#ifndef _CONFIG_H_
#define _CONFIG_H_

#include <inttypes.h>

#define VERSION "v3.5\n"

#define GCRED    1              /* do some reductions during GC */
#define FASTTAGS 1              /* compute tag by pointer subtraction */
#define UNIONPTR 1              /* use compact (2 pointer) layout */
#define INTTABLE 1              /* use fixed table of small INT nodes */
#define SANITY   1              /* do some sanity checks */
#define STACKOVL 1              /* check for stack overflow */
#define GETRAW   1              /* implement raw character get */

/* Keep permanent nodes for LOW_INT <= i < HIGH_INT */
#define LOW_INT (-10)
#define HIGH_INT 128

#define HEAP_CELLS 50000000
#define STACK_SIZE 100000

/****** FIXME These should maybe go somewhere else, ask Joel ******/
typedef intptr_t value_t;       /* Make value the same size as pointers, since they are in a union */
#define PRIvalue PRIdPTR
typedef uintptr_t uvalue_t;     /* Make unsigned value the same size as pointers, since they are in a union */
#define PRIuvalue PRIuPTR
typedef uintptr_t heapoffs_t;   /* Heap offsets */
#define PRIheap PRIuPTR
typedef uintptr_t tag_t;        /* Room for tag, low order bit indicates AP/not-AP */
typedef intptr_t stackptr_t;    /* Index into stack */
/* These types can be changed for 32 bit platforms. */
typedef uint64_t counter_t;     /* Statistics counter, can be smaller since overflow doesn't matter */
#define PRIcounter PRIu64
typedef uint64_t bits_t;        /* One word of bits */
/******************************************************************/

#define BITS_PER_WORD (sizeof(bits_t) * 8)

#endif // _CONFIG_H_