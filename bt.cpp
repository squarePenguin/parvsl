// Rather crummy manually added backtrace scheme for C++.

// The idea here is that source changes are made such that a function
// with say 2 arguments x and y is given an n extra first line
//     TWO_ARGS(x,y);
// This is in fact a macro that expands into the declaration of an RAII
// object that stores the argument values such that use of a function
// backtrace() can display all active functions along with their arguments.


#include <cstdlib>
#include <iostream>

class backtrace_frame
{   public:
        backtrace_frame *caller = NULL;
        virtual void print()
        {}
};

static backtrace_frame *backtrace_list = NULL;

void backtrace()
{   std::cout << std::endl << "BACKTRACE" << std::endl;
    for (backtrace_frame *b=backtrace_list; b!=NULL; b=b->caller)
        b->print();
    std::cout << "end of backtrace" << std::endl;
}

class zero_args: public backtrace_frame
{
    private:
        const char *name;
        const char *file;
        unsigned int line;
    public:
        zero_args(const char *fname, const char *f, unsigned int l)
        {   caller = backtrace_list;
            backtrace_list = this;
            name = fname;
            file = f;
            line = l;
        }
        ~zero_args()
        {   backtrace_list = caller;
        }
        virtual void print()
        {   std::cout << "[" << file << ":" << line << "] "
                      << name << " (0 args)" << std::endl;
        }
};

#define ZERO_ARGS \
    zero_args _bt(__func__, __FILE__, __LINE__)

template <typename T1>
class one_args: public backtrace_frame
{
    private:
        const char *name;
        const char *file;
        unsigned int line;
        T1 a1val;
    public:
        one_args(const char *fname, const char *f, unsigned int l, T1 a1)
        {   caller = backtrace_list;
            backtrace_list = this;
            name = fname;
            file = f;
            line = l;
            a1val = a1;
        }
        ~one_args()
        {   backtrace_list = caller;
        }
        virtual void print()
        {   std::cout << "[" << file << ":" << line << "] "
                      << name << " (1 arg)" << std::endl;
            std::cout << "    Arg1: " << a1val << std::endl;
        }
};

#define ONE_ARGS(a) \
    one_args<decltype(a)> _bt(__func__, __FILE__, __LINE__, a)

template <typename T1, typename T2>
class two_args: public backtrace_frame
{
    private:
        const char *name;
        const char *file;
        unsigned int line;
        T1 a1val;
        T2 a2val;
    public:
        two_args(const char *fname, const char *f, unsigned int l,
                 T1 a1, T2 a2)
        {   caller = backtrace_list;
            backtrace_list = this;
            name = fname;
            file = f;
            line = l;
            a1val = a1;
            a2val = a2;
        }
        ~two_args()
        {   backtrace_list = caller;
        }
        virtual void print()
        {   std::cout << "[" << file << ":" << line << "] "
                      << name << " (1 arg)" << std::endl;
            std::cout << "    Arg1: " << a1val << std::endl;
            std::cout << "    Arg2: " << a2val << std::endl;
        }
};

#define TWO_ARGS(a) \
    one_args<decltype(a)> _bt(__func__, __FILE__, __LINE__, a, b)

static int a = 0;

void testfn()
{   ZERO_ARGS;
    if (a++ > 4)
    {   backtrace();
        return;
    }
    testfn();
    testfn();
}

int nest(int n)
{   ONE_ARGS(n);
    if (n == 0)
    {   backtrace();
        return 1;
    }
    else return n*nest(n-1);
}

int main(int argc, char *argv[])
{   testfn();
    std::cout << "Result = " << nest(5) << std::endl;
    return 0;
}
