#include "forth.h"

DEFUN(main,
  IMM(42),
  IMM(10),
  IMM(0),
  IMM(20),
  IMM(30),
  IMM(4), DO(rZ_save),
  IMM(3), DO(rZ_pick),
  IMM(4), DO(rZ_ndrop),
)

