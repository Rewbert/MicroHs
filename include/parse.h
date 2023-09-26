#ifndef _PARSE_H_
#define _PARSE_H_

#include "bfile.h"

/* If the next input character is c, then consume it, else leave it alone. */
int gobble(BFILE *f, int c);

/* Get a non-terminating character.  ' ' and ')' terminate a token. */
int getNT(BFILE *f);

value_t parse_int(BFILE *f);

double parse_double(BFILE *f);

#endif // _PARSE_H_