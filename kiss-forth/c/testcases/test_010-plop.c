#include "forth.h"

DEFUN(main,
  IMM(10),
  IMM(42),
  IMM(20),
  IMM(30),
  IMM(0), IMM(3), DO(plop),
  IMM(2), DO(ndrop),
)

