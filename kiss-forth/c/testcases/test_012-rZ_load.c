#include "forth.h"

DEFUN(main,
  IMM(10),
  IMM(20),
  IMM(30),
  IMM(0),
  IMM(3), DO(rZ_save),
  IMM(1), DO(rZ_load),
  IMM(2), DO(rZ_ndrop),
)

