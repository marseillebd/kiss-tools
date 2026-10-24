#include "forth.h"

DEFUN(callee,
  IMM(0),
)

DEFUN(main,
  TICK(callee), DO(jump),
  IMM(10),
)

