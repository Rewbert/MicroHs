
#include "err.h"

void
memerr(void)
{
  fprintf(stderr, "Out of memory\n");
  exit(1);
}