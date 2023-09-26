#ifndef _GC_H_
#define _GC_H_

#include "node.h"

/* Set FREE bit to 0 */
extern void mark_used(NODEPTR n);

/* Test if FREE bit is 0 */
extern int is_marked_used(NODEPTR n);

extern void mark_all_free(void);

void mark(NODEPTR *np);

#endif // _GC_H_