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

thread_local static std::vector<LispObject> local_symbols;
// thread_local static 

// inline 
// void set_symbol(int loc, LispObject val) {
//     if (num_symbols > local_symbols.size()) {
//         local_symbols.resize(num_symbols, undefined);
//     }
//     assert(loc < local_symbols.size());
//     local_symbols[loc] = val;
// }

// inline
// LispObject get_symbol(int loc) {
//     if (num_symbols > local_symbols.size()) {
//         local_symbols.resize(num_symbols, undefined);
//     }
//     assert(loc < local_symbols.size());
//     return local_symbols[loc];
// }

static std::mutex alloc_symbol_mutex;

/**
 * This just returns a shared id to index the symbol
 * */
inline
int allocate_symbol() {
    std::unique_lock<std::mutex> lock(alloc_symbol_mutex);
    int loc = num_symbols;
    num_symbols += 1;
    return loc;
}

LispObject& get_symbol(int loc) {
    if (num_symbols > (int)local_symbols.size()) {
        local_symbols.resize(num_symbols, undefined);
    }
    // std::cerr << loc << ' ' << num_symbols << ' ' << local_symbols.size() << std::endl;
    // assert(loc < (int)local_symbols.size());
    if (loc >= (int)local_symbols.size()) {
        std::cerr << "location invalid " << loc << " " << num_symbols << std::endl;
        throw std::logic_error("bad thread_local index");
    }
    return local_symbols[loc];
}

LispObject& symval(LispObject s) {
    assert(isSYMBOL(s));
    if ((qflags(s) & flagGLOBAL) != 0) {
        return qvalue(s);
    } else {
        return get_symbol(qfixnum(qvalue(s)));
    }
}

inline
void set_value(LispObject x, LispObject val) {
    if ((qflags(x) & flagGLOBAL) != 0) {
        qvalue(x) = val;
    } else {
        int loc = qfixnum(qvalue(x));
        get_symbol(loc) = val;
    }
}

inline
LispObject& get_value(LispObject x) {
    if ((qflags(x) & flagGLOBAL) != 0) {
        return qvalue(x);
    } else {
        int loc = qfixnum(qvalue(x));
        return get_symbol(loc);
    }
}

class Shallow_bind {
private:
    LispObject loc;
    LispObject save;
public:
    Shallow_bind(LispObject x, LispObject tval) : loc(x) {
        save = tval;
        std::swap(symval(loc), save);
    }

    ~Shallow_bind() {
        symval(loc) = save;
    }
};

} // namespace par


#endif // THREAD_DATA_HPP
