/* bench_both.cpp
 * 
 * Author           : Alexander J. Yee
 * Date Created     : 08/09/2015
 * Last Modified    : 08/09/2015
 * 
 *  Benchmark GMP's multiplication for sequential latency.
 * 
 *  A latency benchmark is how long it takes to perform a single multiply using
 *  all the computing resources of the entire system.
 * 
 *  This is most relevant to applications that aren't parallelized and must
 *  invoke library calls sequentially.
 * 
 *  Compile with:   g++ bench_gmp_multiply_latency.cpp -std=c++11 -lgmp -O2
 * 
 */

// Adjusted ct A C norman, January 2019 because with the version of gmp I
// tried then the typedef for wtype caused gcc to moan abour argument types
// for the gmp calls.

#include <stdint.h>
#include <string.h>
#include <time.h>
#include <sys/time.h>
#include <memory>
#include <iostream>

#include "gmp.h"
#include "arithlib.hpp"


using std::cout;
using std::endl;

//typedef uint64_t wtype;
typedef mp_limb_t wtype;

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

    uint64_t iterations = 0;
    double start = get_wall_time();
    do{
        for (int k=0; k<1000; k++)
            mpn_mul(C, A, L, B, L);
        iterations++;
    }while (get_wall_time() - start < 4.0);
    double time_gmp = (get_wall_time() - start) / (1000*iterations);

    uint64_t iterationsA = 0;
    double startA = get_wall_time();
    do{
        size_t lena = L, lenb = L, lenc;
        for (int k=0; k<1000; k++)
            arithlib::bigmultiply((uint64_t *)A, lena, (uint64_t *)B, lenb, (uint64_t *)C, lenc);
        iterationsA++;
    }while (get_wall_time() - startA < 4.0);
    double time_arithlib = (get_wall_time() - startA) / (1000*iterationsA);

    cout << "L" << std::setw(5) << L
         << std::fixed << std::setprecision(3)
         << std::setw(9) << (1.0e6*time_arithlib)
         << std::setw(9) << (1.0e6*time_gmp)
         << " usec/op:   ratio: "
         << std::fixed << std::setw(8)
         << (time_arithlib/time_gmp) << endl;
    return true;
}

int main(){
    if (sizeof(wtype) != sizeof(uint64_t))
    {   printf("Discrepency in object sizes!\n");
        return  1;
    }
    size_t bytes = 200000;

    cout << endl;
    cout << "Using gmp version " << gmp_version << endl;
    cout << "Benchmarking: L x L -> 2L  (64-bit word multiply)" << endl;
    cout << endl;
    cout << "  words arithlib    gmp" << endl;
    size_t L = 2;
    while (bench_multiply(bytes, L)){
        L += (L+4)/5;
    }
    return 0;
}

// end of bench_both.cpp
