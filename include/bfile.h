#ifndef _BFILE_H_
#define _BFILE_H_

#include <stdint.h>
#include <stdio.h>
#include <stddef.h>

typedef struct bfile BFILE;

BFILE * alloc_buffer(size_t size);
size_t read_into_buffer(BFILE *f, size_t num, FILE *src);
int getb(BFILE *p);
void ungetb(int c, BFILE *p);

#endif // _BFILE_H_