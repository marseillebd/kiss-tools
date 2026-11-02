#include "forth.h"

DEFUN(main,
  IMM(30),
  IMM(0),
  IMM(10),
  DO(ZGr),
  DO(ZGr),
  IMM(1), DO(rZ_load),
  IMM(1), DO(rZ_ndrop),
)

