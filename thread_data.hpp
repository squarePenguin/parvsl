#ifndef THREAD_DATA_HPP
#define THREAD_DATA_HPP

#include "common.hpp"

#include <cassert>

#include <algorithm>
#include <atomic>
#include <mutex>
#include <functional>
#include <vector>
#include <thread>
#include <unordered_map>

extern LispObject print(LispObject);

namespace par {
// Segments are a way of splitting the memory into further chunks
// such that every thread is only writing to a chunk at a time.

class Thread_data {
public:
    // CR VB: Should this be defined in terms of LispObject?
    static constexpr int SEGMENT_SIZE = 65536; // 64KB per segment
    // VB: use these as the segment we can write on.
    uintptr_t segment_fringe = -1;
    uintptr_t segment_limit = 0;
    int id;
    LispObject *C_stackbase;
    LispObject *C_stackhead = nullptr;
    
    /**
     * Whether the thread is in a safe state for GC.
     * TODO VB: right now we say always true 
     * */
    bool safe_memory = true;
};

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
static std::atomic_int tid(0);

std::mutex thread_mutex;
int start_thread(std::function<void(void)> f) {
    std::lock_guard<std::mutex> lock(thread_mutex);

    tid += 1;
    auto twork = [f]() {
        int stack_var = 0;
        thread_data.C_stackbase = (LispObject *)((intptr_t)&stack_var & -sizeof(LispObject));
        thread_data.id = tid;

        thread_table.emplace(tid, thread_data);

        f();
    };

    active_threads.emplace(tid, std::thread(twork));
    return tid;
}

static std::atomic_int num_symbols(0);

thread_local static std::vector<LispObject> fluid_locals;
static std::vector<LispObject> fluid_globals; // the global values

static std::mutex alloc_symbol_mutex;

/**
 * This just returns a shared id to index the symbol
 * */
inline
int allocate_symbol() {
    std::unique_lock<std::mutex> lock(alloc_symbol_mutex);
    int loc = num_symbols;
    num_symbols += 1;
    fluid_globals.push_back(undefined);
    return loc;
}

inline bool is_global(LispObject x) {
    return ((qflags(x) & flagGLOBAL) != 0);
}

inline bool is_fluid(LispObject x) {
    return ((qflags(x) & flagFLUID) != 0);
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

    // std::cerr << "loc=" << loc << std::endl;

    // Here I assume undefined is a sort of "reserved value", meaning it can only exist
    // when the object is not shallow_bound. This helps me distinguish between fluids that
    // are actually global and those that have been bound.
    if (is_fluid(s) && res == undefined) {
        // std::cerr << "fluid global" << std::endl;
        return fluid_globals[loc];
    }
    // THis is either local or locally bound fluid
    // std::cerr << "local" << std::endl;
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
        assert(!is_global(x));

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
