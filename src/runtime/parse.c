#include <stdlib.h>

#include "config.h"
#include "parse.h"

/* If the next input character is c, then consume it, else leave it alone. */
int
gobble(BFILE *f, int c)
{
  int d = getb(f);
  if (c == d) {
    return 1;
  } else {
    ungetb(d, f);
    return 0;
  }
}

/* Get a non-terminating character.  ' ' and ')' terminate a token. */
int
getNT(BFILE *f)
{
  int c;
  
  c = getb(f);
  if (c == ' ' || c == ')') {
    ungetb(c, f);
    return 0;
  } else {
    return c;
  }
}

value_t
parse_int(BFILE *f)
{
  value_t i = 0;
  int c = getb(f);
  for(;;) {
    i = i * 10 + c - '0';
    c = getb(f);
    if (c < '0' || c > '9') {
      ungetb(c, f);
      break;
    }
  }
  return i;
}

double
parse_double(BFILE *f)
{
  // apparently longest float, when rendered, takes up 24 characters. We add one more for a potential
  // minus sign, and another one for the final null terminator.
  // https://stackoverflow.com/questions/1701055/what-is-the-maximum-length-in-chars-needed-to-represent-any-double-value
  char buf[26];
  for(int j = 0; (buf[j] = getNT(f)); j++)
    ;

  return strtod(buf, NULL);;
}