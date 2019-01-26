/* bench_arithlib.cpp
 *
 * This is based on bench_gmp*.cpp with the following credits:
 * Author           : Alexander J. Yee
 * Date Created     : 08/09/2015
 * Last Modified    : 08/09/2015
 * 
 *  Benchmark arithlib's multiplication for sequential latency.
 * 
 *  A latency benchmark is how long it takes to perform a single multiply using
 *  all the computing resources of the entire system.
 * 
 *  This is most relevant to applications that aren't parallelized and must
 *  invoke library calls sequentially.
 * 
 *  Compile with:   g++ bench_arithlib.cpp -std=c++11 -O2
 * 
 */

// Adjusted by A C norman, January 2019 to use arithlib rather then gmp.

#include <stdint.h>
#include <string.h>
#include <time.h>
#include <sys/time.h>
#include <memory>
#include <iostream>

#include "arithlib.hpp"

using std::cout;
using std::endl;

typedef uint64_t wtype;

double get_wall_time(){
    struct timeval time;
    if (gettimeofday(&time,NULL)){
        //  Handle error
        return 0;
    }
    return (double)time.tv_sec + (double)time.tv_usec * .000001;
}

bool bench_multiply(size_t memory_limit, size_t L){
    //  Compute memory requirement
    size_t ML = 10*L * sizeof(wtype);   //  Heuristic Estimate
    size_t bytes = 4*L * sizeof(wtype) + ML;
    if (bytes > memory_limit)
        return false;

    //  Allocate Operands
    auto A_uptr = std::unique_ptr<wtype>(new wtype[L]);
    auto B_uptr = std::unique_ptr<wtype>(new wtype[L]);
    auto C_uptr = std::unique_ptr<wtype>(new wtype[2*L]);
    wtype* A = A_uptr.get();
    wtype* B = B_uptr.get();
    wtype* C = C_uptr.get();

    //  Generate Random Data
    for (size_t c = 0; c < L; c++){
        A[c] = c * c * c + 1718498867;
        B[c] = c * c + 1404340939;
    }
    memset(C, 0, 2*L * sizeof(wtype));

    cout << "L =\t" << L << "\t| ";
    cout.flush();
    uint64_t iterations = 0;
    double start = get_wall_time();
    do{
//      mpn_mul(C, A, L, B, L);
        size_t lena = L, lenb = L, lenc;
        arithlib::bigmultiply(A, lena, B, lenb, C, lenc);
        arithlib::bigmultiply(A, lena, B, lenb, C, lenc);
        arithlib::bigmultiply(A, lena, B, lenb, C, lenc);
        arithlib::bigmultiply(A, lena, B, lenb, C, lenc);
        arithlib::bigmultiply(A, lena, B, lenb, C, lenc);
        arithlib::bigmultiply(A, lena, B, lenb, C, lenc);
        arithlib::bigmultiply(A, lena, B, lenb, C, lenc);
        arithlib::bigmultiply(A, lena, B, lenb, C, lenc);
        arithlib::bigmultiply(A, lena, B, lenb, C, lenc);
        arithlib::bigmultiply(A, lena, B, lenb, C, lenc);
        iterations++;
    }while (get_wall_time() - start < 4.0);

    cout << (get_wall_time() - start) / (10*iterations)
         << "\tsecs / multiply" << endl;
    return true;
}

int main(){
    if (sizeof(wtype) != sizeof(uint64_t))
    {   printf("Discrepency in object sizes!\n");
        return  1;
    }
//  size_t bytes = (size_t)30 << 30;    //  30 GiB
    size_t bytes = (size_t)1 << 20;    //  1 GiB (ACN less interested in truly huge cases)

    cout << endl;
    cout << "Benchmarking: L x L -> 2L  (64-bit word multiply)" << endl;
    cout << endl;

    size_t L = 2;
    while (bench_multiply(bytes, L)){
        L += (L+7)/8;
    }
    return 0;
}
