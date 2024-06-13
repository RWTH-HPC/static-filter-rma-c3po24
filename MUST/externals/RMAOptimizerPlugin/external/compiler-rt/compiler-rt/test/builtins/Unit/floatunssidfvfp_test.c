// RUN: %clang_builtins %s %librt -o %t && %run %t
// REQUIRES: librt_has_floatunssidfvfp

#include "int_lib.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>


extern COMPILER_RT_ABI double __floatunssidfvfp(unsigned int a);

#if defined(__arm__) && defined(__ARM_FP) && (__ARM_FP & 0x8)
int test__floatunssidfvfp(unsigned int a)
{
    double actual = __floatunssidfvfp(a);
    double expected = a;
    if (actual != expected)
        printf("error in test__floatunssidfvfp(%u) = %f, expected %f\n",
               a, actual, expected);
    return actual != expected;
}
#endif

int main()
{
#if defined(__arm__) && defined(__ARM_FP) && (__ARM_FP & 0x8)
    if (test__floatunssidfvfp(0))
        return 1;
    if (test__floatunssidfvfp(1))
        return 1;
    if (test__floatunssidfvfp(0x7FFFFFFF))
        return 1;
    if (test__floatunssidfvfp(0x80000000))
        return 1;
    if (test__floatunssidfvfp(0xFFFFFFFF))
        return 1;
#else
    printf("skipped\n");
#endif
    return 0;
}
