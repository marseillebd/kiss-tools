#include "forth.h"

DEFUN(callee,
  IMM(1), DO(rZ_ndrop),
)

DEFUN(main,
  IMM(0),
  DO(callee),
  IMM(10),
)

