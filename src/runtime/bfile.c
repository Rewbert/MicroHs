
#include "config.h"
#include <stdlib.h>
#include "bfile.h"
#include "err.h"

struct bfile {
  size_t   b_size;
  size_t   b_pos;
  uint8_t  b_buffer[1];
};

BFILE *
alloc_buffer(size_t size)
{
  BFILE *p;
  p = malloc(sizeof(BFILE) + size);
  if (!p) memerr();
  p->b_size = size;
  p->b_pos = 0;
  return p;
}

size_t read_into_buffer(BFILE *f, size_t num, FILE *src) {
  return fread(f->b_buffer, 1, num, src);
}

int
getb(BFILE *p)
{
  if (p->b_pos >= p->b_size)
    return -1;
  return p->b_buffer[p->b_pos++];
}

void
ungetb(int c, BFILE *p)
{
  if (p->b_pos == 0)
    ERR("ungetb");
  p->b_buffer[--p->b_pos] = (uint8_t)c;
}