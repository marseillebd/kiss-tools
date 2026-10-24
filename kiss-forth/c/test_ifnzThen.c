#include "forth.h"

DEFUN(main,
  IMM(42),
  IMM(0),
  IMM(1),
  DO(ifnz),
)
