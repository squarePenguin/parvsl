#ifndef RW_LOCK_HPP
#define RW_LOCK_HPP

#include <atomic>

// TODO: test performance vs mutex lock

class Rw_lock {
public:
    class Reader_lock {
    private:
        Rw_lock *l;

        Reader_lock(Rw_lock *_l) : l(_l) {}
    public:
        Reader_lock() = delete;
        ~Reader_lock() = default;
        friend class Rw_lock;

        void lock() {
            l->acquire_read();
        }

        bool try_lock() {
            return l->try_acquire_read();
        }

        void unlock() {
            l->release_read();
        }
    };

    class Writer_lock {
    private:
        Rw_lock *l;

        Writer_lock(Rw_lock *_l) : l(_l) {}
    public:
        Writer_lock() = delete;
        ~Writer_lock() = default;

        friend class Rw_lock;

        void lock() {
            l->acquire_write();
        }

        bool try_lock() {
            return l->try_acquire_write();
        }

        void unlock() {
            l->release_write();
        }
    };

private:
    std::atomic_int l;
    Reader_lock reader;
    Writer_lock writer;
public:
    Rw_lock() : l(0), reader(this), writer(this) {}

    Reader_lock& reader_lock() {
        return reader;
    }

    Writer_lock& writer_lock() {
        return writer;
    }

    void acquire_write() {
        int zero = 0;
        do {
            if (l.load(std::memory_order_relaxed) == 0
                && l.compare_exchange_weak(zero, -1, std::memory_order_acq_rel, std::memory_order_relaxed))
            {
                break;
            }
        } while (true);
    }


    bool try_acquire_write() {
        int zero = 0;
        if (l.load(std::memory_order_relaxed) == 0
            && l.compare_exchange_strong(zero, -1, std::memory_order_acq_rel, std::memory_order_relaxed))
        {
            return true;
        }
        return false;
    }

    void release_write() {
        l.store(0, std::memory_order_release);
    }

    void acquire_read() {
        do {
            int old_val = l.load(std::memory_order_acquire);
            if (old_val >= 0
                && l.compare_exchange_weak(old_val, old_val + 1, std::memory_order_acq_rel, std::memory_order_relaxed))
            {
                break;
            }
        } while (true);
    }

    bool try_acquire_read() {
        int old_val = l.load(std::memory_order_acquire);
        if (old_val >= 0
            && l.compare_exchange_strong(old_val, old_val + 1, std::memory_order_acq_rel, std::memory_order_relaxed))
        {
            return true;
        }
        return false;
    }

    void release_read() {
        l.fetch_add(-1, std::memory_order_acq_rel);
    }
};

#endif // RW_LOCK_HPP