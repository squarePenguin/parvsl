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
#include <set>
#include <thread>
#include <unordered_map>

// #include "stacktrace.h"

extern LispObject print(LispObject);

namespace par {
// Segments are a way of splitting the memory into further chunks
// such that every thread is only writing to a chunk at a time.

#ifdef DEBUG_GLOBALS

bool debug_safe = true;

std::set<std::string> debug_globals;

void add_debug_global(LispObject s) {
    if (not debug_safe) return;
    LispObject name = qpname(s);
    assert(isSTRING(name));
    size_t len = veclength(qheader(name));
    assert(len < 100);
    std::string ns{qstring(name), len};
    debug_globals.insert(ns);
}

#endif // DEBUG_GLOBALS

static std::atomic_int num_symbols(0);

thread_local std::vector<LispObject> fluid_locals;
std::vector<LispObject> fluid_globals; // the global values


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

    /**
     * [local_symbol]
     * Works just like above, except you can specify the thread id to access.
     * */
    LispObject& local_symbol(int loc) {
        auto& locals = *fluid_locals;

        if (num_symbols > (int)locals.size()) {
            locals.resize(num_symbols, undefined);
        }

#ifdef DEBUG
        if (loc >= (int)locals.size()) {
            std::cerr << "location invalid " << loc << " " << num_symbols << std::endl;
            throw std::logic_error("bad thread_local index");
        }
#endif // DEBUG

        return locals[loc];
    }
};

class Rw_lock {
private:
    std::atomic_int l;
public:
    Rw_lock() : l(0) {}

    void acquire_write() {
        int zero = 0;
        do {
            if (l.load() == 0
              && l.compare_exchange_weak(zero,
                                         -1,
                                         std::memory_order_acquire,
                                         std::memory_order_relaxed))
            {
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
            if (old_val >= 0
                && l.compare_exchange_weak(old_val,
                                           old_val + 1,
                                           std::memory_order_acquire,
                                           std::memory_order_relaxed))
            {
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

// these are values on threads waiting to be joined.
// TODO: add to Garbage collector
std::unordered_map<int, LispObject> thread_returns;
thread_local int thread_index = -1;

std::atomic_int num_threads(0);
std::atomic_int paused_threads(0);
std::condition_variable gc_waitall;
std::condition_variable gc_cv;
std::atomic_bool gc_on(false);

namespace {
std::mutex gc_guard_mutex;
std::mutex gc_lock_mutex;
}

class Gc_guard {
public:
    Gc_guard() {
        int stack_var = 0;
        thread_data.C_stackhead = (LispObject *)((intptr_t)&stack_var & -sizeof(LispObject));

        paused_threads += 1;
        gc_waitall.notify_one();
    }

    ~Gc_guard() {
        std::unique_lock<std::mutex> lock(gc_guard_mutex);
        gc_cv.wait(lock, []() { return !gc_on; });
        paused_threads -= 1;

        thread_data.C_stackhead = nullptr;
    }
};

class Gc_lock {
private:
    std::unique_lock<std::mutex> lock;

public:
    Gc_lock() : lock(gc_lock_mutex) {
        assert(gc_on);
        // std::cerr << "waiting gc lock " << thread_data.id <<  std::endl;

        int stack_var = 0;
        thread_data.C_stackhead = (LispObject *)((intptr_t)&stack_var & -sizeof(LispObject));

        paused_threads += 1;
        gc_waitall.wait(lock, []() {
            // std::cerr << "paused: " << paused_threads << std::endl;
            // std::cerr << "total: " << num_threads << std::endl;
            return paused_threads == num_threads; });
        // std::cerr << "Gc_lock " << thread_data.id <<  std::endl;
    }

    ~Gc_lock() {
        // std::cerr << "~Gc_lock " << thread_data.id << std::endl;
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

void init_thread_data(int id, LispObject *C_stackbase) {
    thread_data.C_stackbase = C_stackbase;
    thread_data.id = id;
    thread_data.fluid_locals = &fluid_locals;
    thread_data.work1 = &work1;
    thread_data.work2 = &work2;
    thread_table.emplace(tid, par::thread_data);
    num_threads += 1;
}

class Thread_manager {
public:
    Thread_manager(int id) {
        int stack_var = 0;
        init_thread_data(id, (LispObject *)((intptr_t)&stack_var & -sizeof(LispObject)));
    }

    ~Thread_manager() {
        num_threads -= 1;
        thread_table.erase(thread_data.id);
    }
};

std::mutex thread_mutex;
int start_thread(std::function<LispObject(void)> f) {
    std::lock_guard<std::mutex> lock(thread_mutex);

    tid += 1;
    int id = tid;

    auto twork = [f, id]() {
        Thread_manager tm{id};

        // std::cerr << "stackbase " << thread_data.C_stackbase << std::endl;
        LispObject result = f();
        thread_returns[thread_data.id] = result;
    };

    active_threads.emplace(tid, std::thread(twork));
    return tid;
}

LispObject join_thread(int tid) {
    std::lock_guard<std::mutex> lock(thread_mutex);
    {
        // this will block if the thread is still running
        Gc_guard guard;
        active_threads.erase(tid);
    }

    LispObject value = thread_returns[tid];
    thread_returns.erase(tid);
    return value;
}

LispObject yield_thread() {
    std::this_thread::yield();
    return nil;
}

// we are keeping mutexes in a map, just like thread
// unfortunately there are no finalisers maybe have function to clean up mutex?
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
    par::Gc_guard guard;
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

/**
 * mutex must be locked when calling this function
 * undefined behaviour otherwise
**/
void condvar_wait(int cvid, int mid) {
    auto& m = mutexes[mid];
    std::unique_lock<std::mutex> lock(m, std::adopt_lock);
    auto& condvar = condvars[cvid];

    par::Gc_guard guard;
    condvar.wait(lock);
}

/**
 * mutex must be locked when calling this function
 * undefined behaviour otherwise
 * [ms] specifies timeout duration in miliseconds
 * returns true when signaled
**/
LispObject condvar_wait_for(int cvid, int mid, int ms) {
    auto& m = mutexes[mid];
    std::unique_lock<std::mutex> lock(m, std::adopt_lock);
    auto& condvar = condvars[cvid];

    par::Gc_guard guard;
    if (condvar.wait_for(lock, std::chrono::milliseconds(ms)) == std::cv_status::timeout) {
        return nil;
    }

    return lisptrue;
}

void condvar_notify_one(int cvid) {
    condvars[cvid].notify_one();
}

void condvar_notify_all(int cvid) {
    condvars[cvid].notify_all();
}

static std::mutex alloc_symbol_mutex;

/**
 * This just returns a shared id to index the symbol.
 * Used internally to identify the same symbol name on multiple
 * threads. This location will be an index into the actual storage array,
 * whether it's global or thread_local, and will be stored inside
 * qvalue(x).
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
* [local_symbol] gets the thread local symbol. may return undefined.
* Note, thread_local symbols are only lazily resised. Accessing a local
* symbol directly is dangerous. You need to use this function to ensure the
* local symbol is at least allocated.
*/
LispObject& local_symbol(int loc) {
    if (num_symbols > (int)fluid_locals.size()) {
        fluid_locals.resize(num_symbols, undefined);
    }

#ifdef DEBUG
    if (loc >= (int)fluid_locals.size()) {
        // THis is basically an assert but I am printing a bit more information.
        std::cerr << "location invalid " << loc << " " << num_symbols << std::endl;
        throw std::logic_error("bad thread_local index");
    }
#endif // DEBUG

    return fluid_locals[loc];
}

bool is_fluid_bound(LispObject s) {
    if (not is_fluid(s)) return false;

    int loc = qfixnum(qvalue(s));
    LispObject local = local_symbol(loc);

    return local == undefined;
}

/**
* [symval] returns the real current value of the symbol on the current thread.
* It handles, globals, fluid globals, fluid locals and locals.
* should be used to get the true symbol, instead of qvalue(s).
*/
LispObject& symval(LispObject s) {
    // assert(isSYMBOL(s));
    if (is_global(s)) {
        return qvalue(s);
    }

    int loc = qfixnum(qvalue(s));
    LispObject& res = local_symbol(loc);
    // Here I assume undefined is a sort of "reserved value", meaning it can only exist
    // when the object is not shallow_bound. This helps me distinguish between fluids that
    // are actually global and those that have been bound.
    // When the local value is undefined, I refer to the global value.
    if (is_fluid(s) && res == undefined) {
        return fluid_globals[loc];
    }
    // THis is either local or locally bound fluid
    return res;
}

/**
* [Shallow_bind] is a RAII class to rebind a variable.
* It will throw an exception when trying to rebind globals
* When it goes out of scope, it restores the original value of the symbol.
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

    Shallow_bind(Shallow_bind&&) noexcept = default;

    ~Shallow_bind() {
        local_symbol(loc) = save;
    }
};

} // namespace par


#endif // THREAD_DATA_HPP
