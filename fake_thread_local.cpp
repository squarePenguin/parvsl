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

// This is intended to be something of a HACK to get thread-local access on
// Windows with only modest overhead.


#include <cstdint>
#include <cstdlib>
#include <iostream>

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

class thread_locals
{
public:
    int i;
};

#endif // TEST

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
    uintptr_t i = (intptr_t)&d | (1<<STACK_BITS - 8);
    size_t s = sizeof(thread_locals);
    size_t a = alignof(thread_locals);
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
{   uintptr_t mask = 1<<STACK_BITS - 8;
    void *old_stackloc = (void *)&new_stack[mask];
    size_t s = sizeof(thread_locals);
    size_t a = alignof(thread_locals);
    size_t w = (8 + s + a-1) & (-a);
// The new stack must be aligned on a 16-byte boundary, so I have left space
// here for 8 bytes of old stack pointer, then an (aligned) instance of
// thread_locals and then whatever gap is required to bump alignment up to
// 16 bytes.
    void *new_stacktop = (void *)
        (((uintptr_t)old_stackloc + 8 - w) & -(size_t)16);
std::cout << "current sp = " << &mask << std::endl;
std::cout << "new sp = " << new_stacktop << std::endl;
std::cout << "difference = " << ((intptr_t)&mask - (intptr_t)new_stacktop) << std::endl;
    asm
    (   "movq %%rsp, (%0)\n\t"   // Save old stack pointer at base of block.
        "movq %1, %%rsp"         // Set stack pointer near top of new block.
        :
        : "r" (old_stackloc), "r" (new_stacktop)
        :
    );
int www;
std::cout << "current sp = " << &www << std::endl;
std::cout << "new sp = " << new_stacktop << std::endl;
std::cout << "difference = " << ((intptr_t)&www - (intptr_t)new_stacktop) << std::endl;
    void *ret = (*f)(arg);
    asm
    (   "movq %%rsp, %%rcx\n\t"  // Find top of the stack block
        "orq %0, %%rcx\n\t"      // I will expect rsp to be 8-byte aligned.
        "movq (%%rcx), %%rsp"    // Recover previous stack pointer.
        :
        : "rmi" (mask)
        :
    );
int www1;
std::cout << "current sp = " << &www1 << std::endl;
std::cout << "new sp = " << new_stacktop << std::endl;
std::cout << "difference = " << ((intptr_t)&www1 - (intptr_t)new_stacktop) << std::endl;
    return ret;
}

inline void *call_for_thread(function_to_call *f, void *arg)
{
    size_t constexpr alignment = (size_t)(1<<STACK_BITS);
#ifdef __cpp_aligned_new
    class alignas(alignment) stack_block {};
    void *p = (void *)new stack_block;
#else // __cpp_aligned_new
    size_t space = 2*alignment;
    void *p = (void *)new char[space];
    if (p == NULL) return NULL;
#ifdef ONE_DAY
// C++11 provides std::align but versions of g++ up to 5.x do not, even
// though in general their support for C++11 is robust. So I will provide
// my own cruder version!
    if (std::align(alignment, alignment, p, space) == NULL)
    {   delete (char *)p;
        return NULL;
    }
#else // ONE _DAY
    p = (void *)(((uintptr_t)p + alignment - 1) & (-alignment));
#endif // ONE_DAY
#endif // __cpp_aligned_new
    return otherstack(f, arg, (char *)p);
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

#include <iostream>
#include <cstdint>

void *testfunction(void *arg)
{   std::cout << "Argument received as " << std::hex
              << (intptr_t)arg << std::endl;
    std::cout << "Using other stack " << std::hex
              << ((intptr_t)&arg) << std::endl;
    std::cout << "Address of thread_local i = " << &(my_tl()->i) << std::endl;
    return (void *)0x6789a;
}

int main(int argc, char *argv[])
{   std::cout << "My stack starts off at " << std::hex
              << ((intptr_t)&argc) << std::endl;
    void *result = call_for_thread(testfunction, (void *)0x12345);
    std::cout << "Result = " << std::hex << ((intptr_t)result) << std::endl;
    return 0;
}

#endif // TEST

// end of fake_thread_local.cpp
