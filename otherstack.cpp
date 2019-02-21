// otherstack.cpp                               Copyright (C) 2019 Codemist

// $Id: otherstack.cpp 4902 2019-02-17 10:11:08Z arthurcnorman $


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
//   typedef struct __my_thread_locals
//   {   void *__reserved_for_old_stack_pointer__;
//       int a;
//       double d;
//       char c[10];
//   } my_thread_locals;
// and then writing
//   {   int localvar;
//       my_thread_locals *p =
//           (my_thread_locals *)((uintptr_t)&localvar & ~0xfffff);
//       ... p.a ... p.d ... p.c[2] ...
//   }


void __attribute__ ((noinline)) *otherstack(
    function_to_call *f,
    void *arg,
    char *new_stack)
{   void *stacktop = (void *)&new_stack[0xffff0];
    asm
    (   "movq %%rsp, (%0)\n\t"   // Save old stack pointer at base of block.
        "movq %1, %%rsp"         // Set stack pointer near top of new block.
        :
        : "r" (new_stack), "r" (stacktop)
        :
    );
    void *ret = (*f)(arg);
    asm
    (   "movq %%rsp, %%rcx\n\t"  // Find base of the block by masking rsp down.
        "andq $0xfffffffffff00000, %%rcx\n\t"
        "movq (%%rcx), %%rsp"    // Recover previous stack pointer.
        :
        :
        :
    );
    return ret;
}

#ifdef TEST

#include <iostream>
#include <cstdint>

void *testfunction(void *arg)
{   std::cout << "Argument received as " << std::dec
              << (intptr_t)arg << std::endl;
    std::cout << "Using other stack " << std::hex
              << ((intptr_t)&arg) << std::endl;
// Now the previous sp should be something I can find.
    uint64_t *base = (uint64_t *)((intptr_t)(&arg) & ~0xfffff);
    std::cout << "base = " << std::hex << (intptr_t)base << std::endl;
    std::cout.flush();
    std::cout << "base[0] = " << std::hex << base[0] << std::endl;
    std::cout.flush();
    return (void *)67890;
}

int main(int argc, char *argv[])
{   std::cout << "My stack starts off at " << std::hex
              << ((intptr_t)&argc) << std::endl;
    char *newstack = new char[0x200000];
    std::cout << "Raw new memory at " << std::hex
              << ((intptr_t)newstack) << std::endl;
    char *aligned = (char *)((intptr_t)(newstack+0xfffff) & ~0xfffff);
    std::cout << "Aligned memory at " << std::hex
              << ((intptr_t)aligned) << std::endl;
    void *result = otherstack(testfunction, (void *)12345, aligned);
    std::cout << "Result = " << std::dec << ((intptr_t)result) << std::endl;
    return 0;
}

#endif // TEST

// end of otherstack.cpp
