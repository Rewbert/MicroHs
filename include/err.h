#ifndef _ERR_H_
#define _ERR_H_

#include <stdlib.h> // FIXME only need these because ERR and memerr is in this header
#include <stdio.h>

// this is platform specific, and should eventually move to a platform specific file
#define ERR(s) do { fprintf(stderr, "ERR: %s\n", s); exit(1); } while(0)

void memerr(void);

#endif // _ERR_H_