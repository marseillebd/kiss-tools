#include "forth.h"

DEFUN(callee,
  IMM(10),
)

DEFUN(main,
  TICK(callee), DO(call),
  IMM(0),
)
