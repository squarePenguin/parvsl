#include "thread_data.hpp"

#include <algorithm>
#include <vector>


namespace par {

// At the beginning a thread has not segment. This is indicated by
// `segment_allocations[thread_index] == -1`
// The first time that thread calls `check_space`, it will be
// allocated a segment, indicated by `segment_fringe` and `segment_limit`
// Whenever the GC kicks in, it will reset all these segments allocations,
// so threads have to ask for new segments.

std::vector<bool> segment_allocations;
thread_local int thread_index = -1;

inline
void check_thread_index() {
    // VB: for now assume it is called within a mutex
    if (thread_index == -1) {
        thread_index = segment_allocations.size();
        segment_allocations.push_back(false);
    }
}

inline
void clear_segment_stati() {
    std::fill(segment_allocations.begin(), segment_allocations.end(), false);
}

inline
void set_segment_status(bool status) {
    check_thread_index();
    segment_allocations[thread_index] = status;
}

inline
bool get_segment_status() {
    check_thread_index();
    return segment_allocations[thread_index];
}

} // namespace par