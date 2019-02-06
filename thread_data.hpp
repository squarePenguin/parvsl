#ifndef THREAD_DATA_HPP
#define THREAD_DATA_HPP

#include "common.hpp"

#include <cassert>

#include <algorithm>
#include <atomic>
#include <condition_variable>
#include <mutex>
#include <functional>
#include <vector>
#include <thread>
#include <unordered_map>

// #include "stacktrace.h"

extern LispObject print(LispObject);

namespace par {
// Segments are a way of splitting the memory into further chunks
// such that every thread is only writing to a chunk at a time.

class Thread_data {
public:
    // CR VB: Should this be defined in terms of LispObject?
    static constexpr int SEGMENT_SIZE = 0x20000; // 128K 65536; // 64KB per segment
    // VB: use these as the segment we can write on.
    uintptr_t segment_fringe = -1;
    uintptr_t segment_limit = 0;
    int id;
    LispObject *C_stackbase;
    LispObject *C_stackhead = nullptr;

    LispObject *work1;
    LispObject *work2;

    std::vector<LispObject> *fluid_locals;
    
    /**
     * Whether the thread is in a safe state for GC.
     * TODO VB: right now we say always true 
     * */
    bool safe_memory = true;
};

class Rw_lock {
private:
  std::atomic_int l;
public:
    Rw_lock() : l(0) {}

  void acquire_write() {
    int zero = 0;
    do {
      if (l.load() == 0 && l.compare_exchange_weak(zero, -1, std::memory_order_acquire, std::memory_order_relaxed)) {
        break;
      }
    } while (true);
  }

  void release_write() {
    l.store(0, std::memory_order_release);
  }

  void acquire_read() {
    do {
      int old_val = l.load(std::memory_order_relaxed);
      if (old_val >= 0 && l.compare_exchange_weak(old_val, old_val + 1, std::memory_order_acquire, std::memory_order_relaxed)) {
        break;
      }
    } while (true);
  }

  void release_read() {
    l.fetch_add(-1, std::memory_order_release);
  }
};

// joins the thread on destruction
class Thread_RAII {
private:
    std::thread t;
public:
    Thread_RAII(std::thread&& t) : t(std::move(t)) {}
    ~Thread_RAII() {
        if (t.joinable()) {
            t.join();
        }
    }

    // Allow moving
    Thread_RAII(Thread_RAII&&) = default;
    Thread_RAII& operator=(Thread_RAII&&) = default;

    std::thread& get() { return t; }
};

thread_local Thread_data thread_data;

std::unordered_map<int, Thread_data&> thread_table;
thread_local int thread_index = -1;

static std::atomic_int num_symbols(0);

thread_local std::vector<LispObject> fluid_locals;
std::vector<LispObject> fluid_globals; // the global values

std::atomic_int num_threads(0);
std::atomic_int paused_threads(0);
std::condition_variable gc_waitall;
std::condition_variable gc_cv;
std::atomic_bool gc_on(false);

class Gc_guard {
    std::mutex m;
    std::unique_lock<std::mutex> lock;
public:

    Gc_guard() : m(), lock(m) {
        // assert(false);
        // print_stacktrace();
        std::cerr << "Gc_guard" << std::endl;
        int stack_var = 0;
        thread_data.C_stackhead = (LispObject *)((intptr_t)&stack_var & -sizeof(LispObject));

        paused_threads += 1;
        gc_waitall.notify_one();
    }

    ~Gc_guard() {
        std::cerr << "Waiting gc guard" << std::endl;
        gc_cv.wait(lock, []() { 
            std::cerr << "gc_on: " << gc_on << std::endl;
            return !gc_on; });
        std::cerr << "~Gc_guard" << std::endl;
        paused_threads -= 1;
        
        thread_data.C_stackhead = nullptr;
    }
};

class Gc_lock {
private:
    std::mutex m;
    std::unique_lock<std::mutex> lock;
public:
    Gc_lock() : m(), lock(m) {
        assert(gc_on);
        std::cerr << "waiting gc lock" << std::endl;

        int stack_var = 0;
        thread_data.C_stackhead = (LispObject *)((intptr_t)&stack_var & -sizeof(LispObject));

        paused_threads += 1;
        gc_waitall.wait(lock, []() { 
            std::cerr << "paused: " << paused_threads << std::endl;
            std::cerr << "total: " << num_threads << std::endl;
            return paused_threads == num_threads; });
        std::cerr << "Gc_lock" << std::endl;
    }

    ~Gc_lock() {
        std::cerr << "~Gc_lock" << std::endl;
        paused_threads -= 1;
        gc_on = false;
        thread_data.C_stackhead = nullptr;
        gc_cv.notify_all();
    }
};

/**
 * Force all threads to go in GC mode
 * Reset the limits of each thread's segment
 * */
void reset_segments() {
    for (auto kv: thread_table) {
        kv.second.segment_limit = 0;
    }
}

// For now, just make sure all threads are joined at some point
std::unordered_map<int, Thread_RAII> active_threads;
int tid = 0;

void init_thread_data(LispObject *C_stackbase) {
    thread_data.C_stackbase = C_stackbase;
    thread_data.id = tid;
    thread_data.fluid_locals = &fluid_locals;
    thread_data.work1 = &work1;
    thread_data.work2 = &work2;
    thread_table.emplace(tid, par::thread_data);
    num_threads += 1;
}

class Thread_manager {
public:
    Thread_manager() {
        int stack_var = 0;
        init_thread_data((LispObject *)((intptr_t)&stack_var & -sizeof(LispObject)));
        // num_threads += 1;
    }

    ~Thread_manager() {
        std::cerr << "~Thread_manager" << std::endl;
        num_threads -= 1;
    }
};

std::mutex thread_mutex;
int start_thread(std::function<void(void)> f) {
    std::lock_guard<std::mutex> lock(thread_mutex);

    tid += 1;
    auto twork = [f]() {
        Thread_manager tm;
        f();
    };

    active_threads.emplace(tid, std::thread(twork));
    return tid;
}

void join_thread(int tid) {
    // auto& t = active_threads[tid];
    // t.join();
    active_threads.erase(tid);
}

// we are keeping mutexes in a map, just like thread
// unfortunately there are no finalisers
// maybe have function to clean up mutex?
std::unordered_map<int, std::mutex> mutexes;
int mutex_id = 0;

std::mutex mutex_mutex;
int mutex() {
    std::lock_guard<std::mutex> lock(mutex_mutex);
    mutex_id += 1;

    mutexes[mutex_id]; // easiest way to construct mutex in place
    return mutex_id;
}

void mutex_lock(int mid) {
    mutexes[mid].lock();
}

void mutex_unlock(int mid) {
    mutexes[mid].unlock();
}

std::unordered_map<int, std::condition_variable> condvars;
int condvar_id = 0;

std::mutex condvar_mutex;
int condvar() {
    std::lock_guard<std::mutex> lock(condvar_mutex);
    condvar_id += 1;

    condvars[condvar_id]; // easiest way to construct condvar in place
    return condvar_id;
}

// mutex must be locked when calling this function
void condvar_wait(int cvid, int mid) {
    auto& m = mutexes[mid];
    std::unique_lock<std::mutex> lock(m, std::adopt_lock);
    auto& condvar = condvars[cvid];

    condvar.wait(lock);
}

void condvar_notify_one(int cvid) {
    condvars[cvid].notify_one();
}

void condvar_notify_all(int cvid) {
    condvars[cvid].notify_all();
}

static std::mutex alloc_symbol_mutex;

/**
 * This just returns a shared id to index the symbol
 * */
inline
int allocate_symbol() {
    std::unique_lock<std::mutex> lock(alloc_symbol_mutex);
    int loc = num_symbols;
    num_symbols += 1;
    fluid_globals.resize(num_symbols, undefined);
    return loc;
}

/**
* [local_symbol] gets the thread local symbol.
* may return undefined
*/
LispObject& local_symbol(int loc) {
    if (num_symbols > (int)fluid_locals.size()) {
        fluid_locals.resize(num_symbols, undefined);
    }

    if (loc >= (int)fluid_locals.size()) {
        std::cerr << "location invalid " << loc << " " << num_symbols << std::endl;
        throw std::logic_error("bad thread_local index");
    }

    return fluid_locals[loc];
}

/**
* [symval] returns the real current value of the symbol on the current thread.
* It handles, globals, fluid globals, fluid locals and locals.
* should be used to get the true symbol.
*/
LispObject& symval(LispObject s) {
    assert(isSYMBOL(s));
    if (is_global(s)) {
        return qvalue(s);
    }

    int loc = qfixnum(qvalue(s));
    LispObject& res = local_symbol(loc);
    // Here I assume undefined is a sort of "reserved value", meaning it can only exist
    // when the object is not shallow_bound. This helps me distinguish between fluids that
    // are actually global and those that have been bound.
    if (is_fluid(s) && res == undefined) {
        return fluid_globals[loc];
    }
    // THis is either local or locally bound fluid
    return res;
}

/**
* [Shallow_bind] is a RAII class to rebind a variable.
* It will throw an exception when trying to rebind globals
*/
class Shallow_bind {
private:
    int loc;
    LispObject save;
public:
    Shallow_bind(LispObject x, LispObject tval) {
        if (is_global(x)) {
            error1("shallow bind global", qpname(x));
        }

        loc = qfixnum(qvalue(x));
        LispObject& sv = local_symbol(loc);
        save = sv;
        sv = tval;
    }

    ~Shallow_bind() {
        local_symbol(loc) = save;
    }
};

} // namespace par


#endif // THREAD_DATA_HPP
