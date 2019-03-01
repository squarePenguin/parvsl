// fake_thread_local.cpp                        Copyright (C) 2019 Codemist

// $Id: fake_thread_local.cpp 4902 2019-02-17 10:11:08Z arthurcnorman $


/**************************************************************************
 * Copyright (C) 2019, Codemist.                         A C Norman       *
 *                                                                        *
 * Redistribution and use in source and binary forms, with or without     *
 * modification, are permitted provided that the following conditions are *
 * met:                                                                   *
 *                                                                        *
 *     * Redistributions of source code must retain the relevant          *
 *       copyright notice, this list of conditions and the following      *
 *       disclaimer.                                                      *
 *     * Redistributions in binary form must reproduce the above          *
 *       copyright notice, this list of conditions and the following      *
 *       disclaimer in the documentation and/or other materials provided  *
 *       with the distribution.                                           *
 *                                                                        *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    *
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      *
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS      *
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE         *
 * COPYRIGHT OWNERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,   *
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,   *
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS  *
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND *
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR  *
 * TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF     *
 * THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH   *
 * DAMAGE.                                                                *
 *************************************************************************/

// If you decorate some data defintions as "thread_local" on Linux or
// OSX you can use then from threaded code and each thread will see its own
// version of the data. The time overhead is rather modest. If you do the
// same on Windows (at least in Q1 2019, using either cygwin or mingw32)
// each function that will access thread_local data does a system call to
// find where its thread-specific data is. If the function then does a large
// amount of work (perhaps using the thread_local values) this is not too
// bad, but a small function can have a really severe overhead. A case close
// to worst would be
//      thread_local int val;
//      void my_function() { val++; }
// in circumstances where my_function() can not be inlined and so the
// identity of the thread_local variable had to be established on every call.
// The cost overheads are large enough that they can hurt overall performance
// at a whole-program level.
// The code here is intended to circumvent this. It does so at the cost of
// restricting stack-size under Windows to some fixed amount (by default
// one megabtye), by reducing protection against stack overflow and by
// insisting that all values that are to be thread_local are collected
// to be fields within a single class. This last restriction applies when the
// code is used on Linux or OSX, but the stack delicacies are not present
// in those cases.

// The code here has various bits of conditional compilation not just to
// discriminate between Windows and other systems but to let it take
// advantage of C++17 features when they are available. It can also be built
// so that the C++ "thread_local" qualifier is used on Windows or so that the
// special Windows-targetted scheme is activated elsewhere (it needs x86_64
// to work, since there is some assembly code!). That may be interesting
// for testing performance consequences.

// When this is compiled one (but not both!) of USE_NATIVE_THREAD_LOCAL
// or USE_FAKE_THREAD_LOCAL can be defined to control how this works. If
// you are building on Windows the FAKE option is the default. On all other
// systems the NATIVE one is the default.

#ifdef USE_NATIVE_THREAD_LOCAL
#ifdef USE_FAKE_THREAD_LOCAL
// It is an error to have both options selected.
#error You have selected both NATIVE and FAKE options
#endif
#else // USE_NATIVE_THREAD_LOCAL
#ifndef USE_FAKE_THREAD_LOCAL
// If neither option was explicit I use a platform-specific default.
#ifdef __WIN32__
#define USE_FAKE_THREAD_LOCAL 1
#else
#define USE_NATIVE_THREAD_LOCAL 1 
#endif // __WIN32
#endif // USE_FAKE_THREAD_LOCALS
#endif // USE_NATIVE_THREAD_LOCALS



#include <cstdint>
#include <cstdlib>
#include <iostream>
#include <ctime>
#ifdef __WIN32__
#include <windows.h>
#endif

// I will call a function of the form "void *f(void *a)" on a different
// stack. I am viewing "void *" as a generic type for an argument or a
// result so that any interesting values can be passed across ising it.

typedef void *function_to_call(void *);

// The following function takes an argument and function to call on it,
// and then a 1 Megabyte region of memory to be used as the stack during
// the call. This memory block *MUST* be aligned at a 1 Mbyte boundary.
// The main intent of this is that while running using the new stack
// a low segment of it can be used as thread-local storage. This is
// achieved by defining a structure to contain thread-local values, as in
//
//   typedef struct __thread_locals
//   {
//   public:
//       int a;
//       double d;
//       char c[10];
//   } thread_locals;
//
// and then writing
//   my_tl()->a     my_tl()->d     my_tl()->c
// to access them.
//

#ifdef TEST

// If TEST is defined I will have some test and sample code. I MUST have
// a class called thread_locals defined when I build this code. The one
// here has minimal size and alignment so that I can show that my code
// patches up for that and still aligns the stack properly.

class thread_locals
{
public:
    int i;
};

#endif // TEST

#ifdef USE_NATIVE_THREAD_LOCAL

// This case is really easy. By using an inline function I expect that
// my_tl()->a will compile into code that is the same as that for just
// my_thread_locals.a and any overhead will be just in the system support
// for thread_local access, which on many systems is rather low.

thread_local thread_locals my_thread_locals;

inline thread_locals *my_tl()
{   return &my_thread_locals;
}

inline void *call_for_thread(function_to_call *f, void *arg)
{   return f(arg);
}

#endif


#ifdef USE_FAKE_THREAD_LOCAL

#ifdef __cpp_inline_variables
#define INLINE_VAR inline
#else
#define INLINE_VAR static
#endif

#ifndef STACK_BITS
INLINE_VAR constexpr int STACK_BITS = 20;
#endif

// The idea here will be that my stack is smaller than (1<<STACK_BITS) and
// lives in a block of memory that was allocated aligned that way (eg
// typically a 1Mbyte stack allocated at an address that is a multiple of
// 1Mbyte). If I take the address of an 8-byte aligned local variable and
// OR in a bunch of low bits I can get the address of the top 8-byte
// item in the stack segment. In this code that word will be used to
// hold a previous stack pointer. Below this top word is an instance of
// a thread_locals object, and I need to mess around somewhat here to allow
// for its alignment, but a good compiler should elide pretty well all of this
// into just one or two instructions.

inline thread_locals *my_tl()
{   double d;    // will be aligned on 8-byte boundary
    uintptr_t i = (intptr_t)&d | ((1<<STACK_BITS) - 8);
    size_t s = sizeof(thread_locals);
    size_t a = alignof(thread_locals);
// Force it to be at least 16-byte aligned.
    a = (a + 15) & (-(size_t)16);
    size_t w = (8 + s + a-1) & (-a);
    return (thread_locals *)(i + 8 - w);
}

// The key trick here is the following function, which is to be called
// as about the first thing that any thread does. It moved machine registers
// around so that the call to the target function f is executed in the
// stack segment that it is given.

void __attribute__ ((noinline)) *otherstack(
    function_to_call *f,
    void *arg,
    char *new_stack)
{   uintptr_t mask = (1<<STACK_BITS) - 8;
    void *old_stackloc = (void *)&new_stack[mask];
    size_t s = sizeof(thread_locals);
    size_t a = alignof(thread_locals);
    size_t w = (8 + s + a-1) & (-a);
// The new stack must be aligned on a 16-byte boundary, so I have left space
// here for 8 bytes of old stack pointer, then an (aligned) instance of
// thread_locals and then whatever gap is required to bump alignment up to
// 16 bytes.
    void *new_stacktop = (void *)
        (((uintptr_t)old_stackloc + 8 - w) & (-(size_t)16));
    asm volatile
    (   "movq %%rsp, (%0)\n\t"   // Save old stack pointer at base of block.
        "movq %1, %%rsp"         // Set stack pointer near top of new block.
        :
        : "r" (old_stackloc), "r" (new_stacktop)
        :
    );
    void *ret = (*f)(arg);
    asm volatile
    (   "movq %%rsp, %%rcx\n\t"  // Find top of the stack block
        "orq %0, %%rcx\n\t"      // I will expect rsp to be 8-byte aligned.
        "movq (%%rcx), %%rsp"    // Recover previous stack pointer.
        :
        : "rmi" (mask)
        :
    );
    return ret;
}

// The intent is that call_for_thread is called only once in each thread.
// it allocates new space and deletes it when it is done. If memory can not
// be allocated it just returns NULL.

inline void *call_for_thread(function_to_call *f, void *arg)
{   size_t constexpr alignment = (size_t)(1<<STACK_BITS);
#ifdef __cpp_aligned_new
    class alignas(alignment) stack_block {};
    void *p = (void *)new stack_block;
    if (p == NULL) return NULL;
#else // __cpp_aligned_new
    size_t space = 2*alignment;
    void *p0 = (void *)new char[space];
    if (p0 == NULL) return NULL;
#ifdef ONE_DAY
// C++11 provides std::align but versions of g++ up to 5.x do not, even
// though in general their support for C++11 is robust...
    void *p = p0;
    if (std::align(alignment, alignment, p, space) == NULL)
    {   delete (char *)p0;
        return NULL;
    }
#else // ONE _DAY
// ... so for now I have my own code that arranges alignment. Hmmm in this
// case it is more compact than the use of std::align!
    void *p = (void *)(((uintptr_t)p0 + alignment - 1) & (-alignment));
#endif // ONE_DAY
#endif // __cpp_aligned_new
#ifdef __WIN32__
// To help get stack overflow detected I will set the low page of the
// stack block to be inaccessible.
    DWORD old_protection;
    VirtualProtect((LPVOID)p, 1, PAGE_NOACCESS, &old_protection);
#endif // __WIN32__
    void *r = otherstack(f, arg, (char *)p);
#ifdef __WIN32__
// Restore access to low page of the segment
    VirtualProtect((LPVOID)p, 1, old_protection, &old_protection);
#endif // __WIN32__
#ifdef __cpp_aligned_new
    delete (stack_block *)p;
#else // __cpp_aligned_new
    delete (char *)p0;
#endif // __cpp_aligned_new
    return r;
}

// Note somewhat horribly well that on Windows the main thread can not
// use this until it has gone through call_for_thread() to transition itself
// into a new stack. Well that means that to use all of this that main()
// should do that almost as soon as it can, and then each created thread
// must also do it.

// This WASTES the space that had been allocated by default for the main
// program and for each thread, and certainly unless you have C++17 and
// support for over-aligned "new" (and possibly even then!) it may waste a
// megabyte (or however much STACK_BITS indicates) per thread arranging
// to keep things aligned as needed. But on a 64-bit machine that is not
// really liable to feel like a problem if you are using say up to a dozen
// threads. This is perhaps not really suitable for cases where you may use
// hundreds of threads.


#endif // FAKE_THREAD_LOCAL

#ifdef TEST
//
// Here is a test or demonstration program. It does not create any
// threads, but it shoould illustrate the stack being moved.
//

#include <iostream>
#include <cstdint>

void incvar() __attribute__ ((noinline));
void incvar()
{   my_tl()->i++;
}


void *testfunction(void *arg)
{   std::cout << "Argument received as " << std::hex
              << (intptr_t)arg << std::endl;
    std::cout << "stack in testfunction() " << std::hex
              << ((intptr_t)&arg) << std::endl;
    std::cout << "My thread locals at " << (void *)my_tl() << std::endl;
    std::cout << "Address of thread_local i = "
              << (void *)&(my_tl()->i) << std::endl;
// I now do a time-test
    my_tl()->i = 3;
    for (size_t i=0; i<0x40000000; i++) incvar();
    std::cout << "TL value at end = " << my_tl()->i << std::endl;
    return (void *)0x6789a;
}

int main(int argc, char *argv[])
{   clock_t c0 = std::clock();
    std::cout << "stack in main() starts off at " << std::hex
              << ((intptr_t)&argc) << std::endl;
    void *result = call_for_thread(testfunction, (void *)0x12345);
    std::cout << "Result = " << std::hex << ((intptr_t)result) << std::endl;
    clock_t c1 = std::clock();
    std::cout << "Time = " << ((c1-c0)/(double)CLOCKS_PER_SEC) << std::endl;
    return 0;
}

#endif // TEST

// end of fake_thread_local.cpp
