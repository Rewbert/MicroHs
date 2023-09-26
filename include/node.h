#ifndef _NODE_H_
#define _NODE_H_

#include <stdio.h>

#include "config.h"

#if NAIVE

/* Naive node representation with minimal unions */
typedef struct node {
  enum node_tag tag;
  union {
    value_t value;
    double doublevalue;
    FILE *file;
    const char *string;
    struct {
      struct node *fun;
      struct node *arg;
    } s;
  } u;
} node;
typedef struct node* NODEPTR;
extern node *cells; // all cells
#define NIL 0
#define HEAPREF(i) &cells[(i)]
#define MARK(p) (p)->mark
#define GETTAG(p) (p)->tag
#define SETTAG(p, t) do { (p)->tag = (t); } while(0)
#define GETVALUE(p) (p)->u.value
#define GETDOUBLEVALUE(p) (p)->u.doublevalue
#define SETVALUE(p,v) (p)->u.value = v
#define SETDOUBLEVALUE(p,v) (p)->u.doublevalue = v
#define FUN(p) (p)->u.s.fun
#define ARG(p) (p)->u.s.arg
#define NEXT(p) FUN(p)
#define INDIR(p) FUN(p)
#define HANDLE(p) (p)->u.file
#define NODE_SIZE sizeof(node)
#define ALLOC_HEAP(n) do { cells = malloc(n * sizeof(node)); if (!cells) memerr(); memset(cells, 0x55, n * sizeof(node)); } while(0)
#define LABEL(n) ((heapoffs_t)((n) - cells))

#elif UNIONPTR

typedef struct node {
  union {
    struct node *uufun;
    tag_t uutag;             /* LSB=1 indicates that this is a tag, LSB=0 that this is a T_AP node */
  } ufun;
  union {
    struct node *uuarg;
    value_t uuvalue;
    double uudoublevalue;
    FILE *uufile;
    const char *uustring;
  } uarg;
} node;
typedef struct node* NODEPTR;
extern node *cells; // all cells
#define NIL 0
#define HEAPREF(i) &cells[(i)]
#define GETTAG(p) ((p)->ufun.uutag & 1 ? (int)((p)->ufun.uutag >> 1) : T_AP)
#define SETTAG(p,t) do { if (t != T_AP) (p)->ufun.uutag = ((t) << 1) + 1; } while(0)
#define GETVALUE(p) (p)->uarg.uuvalue
#define GETDOUBLEVALUE(p) (p)->uarg.uudoublevalue
#define SETVALUE(p,v) (p)->uarg.uuvalue = v
#define SETDOUBLEVALUE(p,v) (p)->uarg.uudoublevalue = v
#define FUN(p) (p)->ufun.uufun
#define ARG(p) (p)->uarg.uuarg
#define STR(p) (p)->uarg.uustring
#define INDIR(p) ARG(p)
#define HANDLE(p) (p)->uarg.uufile
#define NODE_SIZE sizeof(node)
#define ALLOC_HEAP(n) do { cells = malloc(n * sizeof(node)); memset(cells, 0x55, n * sizeof(node)); } while(0)
#define LABEL(n) ((heapoffs_t)((n) - cells))

#else

#error "pick a node type"

#endif

#endif // _NODE_H_