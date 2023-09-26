#include <string.h>

#include "gc.h"
#include "err.h"
#include "combinators.h"

extern heapoffs_t heap_start;
extern heapoffs_t free_map_nwords;
extern heapoffs_t next_scan_index;
extern bits_t *free_map;

extern counter_t num_marked;

#if GCRED
extern int red_a;
extern int red_i;
extern int red_int;
#endif

extern NODEPTR intTable[];

/* Set FREE bit to 0 */
inline void mark_used(NODEPTR n)
{
  heapoffs_t i = LABEL(n);
  if (i < heap_start)
    return;
#if SANITY
  if (i >= free_map_nwords * BITS_PER_WORD) ERR("mark_used");
#endif
  free_map[i / BITS_PER_WORD] &= ~(1ULL << (i % BITS_PER_WORD));
}

/* Test if FREE bit is 0 */
inline int is_marked_used(NODEPTR n)
{
  heapoffs_t i = LABEL(n);
  if (i < heap_start)
    return 1;
#if SANITY
  if (i >= free_map_nwords * BITS_PER_WORD) ERR("is_marked_used");;
#endif
  return (free_map[i / BITS_PER_WORD] & (1ULL << (i % BITS_PER_WORD))) == 0;
}

inline void mark_all_free(void)
{
  memset(free_map, ~0, free_map_nwords * sizeof(bits_t));
  next_scan_index = heap_start;
}

void
mark(NODEPTR *np)
{
  NODEPTR n;
#if GCRED
  value_t i;
#endif

  //  mark_depth++;
  //  if (mark_depth % 10000 == 0)
  //    printf("mark depth %"PRIcounter"\n", mark_depth);
  top:
  n = *np;
  if (GETTAG(n) == T_IND) {
#if SANITY
    int loop = 0;
    /* Skip indirections, and redirect start pointer */
    while (GETTAG(n) == T_IND) {
      //      printf("*"); fflush(stdout);
      n = INDIR(n);
      if (loop++ > 10000000) {
        printf("%p %p %p\n", n, INDIR(n), INDIR(INDIR(n)));
        ERR("IND loop");
      }
    }
    //    if (loop)
    //      printf("\n");
#else  /* SANITY */
    while (GETTAG(n) == T_IND) {
      n = INDIR(n);
    }
#endif  /* SANITY */
    *np = n;
  }
  if (is_marked_used(n)) {
    //    mark_depth--;
    return;
  }
  num_marked++;
  mark_used(n);
#if GCRED
  /* This is really only fruitful just after parsing.  It can be removed. */
  if (GETTAG(n) == T_AP && GETTAG(FUN(n)) == T_AP && GETTAG(FUN(FUN(n))) == T_A) {
    /* Do the A x y --> y reduction */
    NODEPTR y = ARG(n);
    SETTAG(n, T_IND);
    INDIR(n) = y;
    red_a++;
    goto top;
  }
#if 0
  /* This never seems to happen */
  if (GETTAG(n) == T_AP && GETTAG(FUN(n)) == T_AP && GETTAG(FUN(FUN(n))) == T_K) {
    /* Do the K x y --> x reduction */
    NODEPTR x = ARG(FUN(n));
    SETTAG(n, T_IND);
    INDIR(n) = x;
    red_k++;
    goto top;
  }
#endif
  if (GETTAG(n) == T_AP && GETTAG(FUN(n)) == T_I) {
    /* Do the I x --> x reduction */
    NODEPTR x = ARG(n);
    SETTAG(n, T_IND);
    INDIR(n) = x;
    red_i++;
    goto top;
  }
#if INTTABLE
  if (GETTAG(n) == T_INT && LOW_INT <= (i = GETVALUE(n)) && i < HIGH_INT) {
    SETTAG(n, T_IND);
    INDIR(n) = intTable[i - LOW_INT];
    red_int++;
    goto top;
  }
#endif  /* INTTABLE */
#endif  /* GCRED */
  if (GETTAG(n) == T_AP) {
#if 1
    mark(&FUN(n));
    //mark(&ARG(n));
    np = &ARG(n);
    goto top;                   /* Avoid tail recursion */
#else
    mark(&ARG(n));
    np = &FUN(n);
    goto top;                   /* Avoid tail recursion */
#endif
  }
}