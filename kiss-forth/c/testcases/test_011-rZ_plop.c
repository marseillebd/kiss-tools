#include "forth.h"

DEFUN(main,
  IMM(42),
  IMM(10),
  IMM(20),
  IMM(30),
  IMM(3), DO(rZ_save),
  IMM(0), IMM(2), DO(rZ_plop),
  IMM(1), DO(rZ_ndrop),
  IMM(2), DO(rZ_load),
)

