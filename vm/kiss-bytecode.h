// The frame pointer points to the top of the call stack.
// The call stack holds call frames.
// Each frame consists of a contiguous memory from top to bottom:
// - saved frame pointer,
// - local registers numbered from 1 upwards,
// - the link register.
// Just after a function is called (JAL(C)), the link register holds the address just after the call instruction.
// Once a function call has been executed, the only valid instruction is ENTR.
// Thus, the caller sets up the link register,
// and the callee sets up the rest of the call frame: local registers and frame pointer.

typedef enum opcode {

  ////// 00-31 reserved as illegal //////
  // They are non-printing characters, so there's not much reason to use them.
  // While 1-31 may be assigned uses if needed, 00 will forever be illegal.
  OP_ILL = 0,

  ////// CTL 32-47 //////

  // no operation
  OP_NOP = 32,

  // 33 undefined

  // J <imm32>: Jump to immediate absolute address.
  // JC: Computed jump. Absolute address is popped off stack.
  OP_J = 34, OP_JC = 35,
  // J(N)Z: pop the stack and jump if the result is (non-)zero.
  OP_JZ = 36, OP_JNZ = 37,
  // JAL <imm32>: Jump and link to immediate absolute address. Aka "call" or "jump to subroutine".
  // JALC: Jump and link to absolute address popped off stack. Aka "call" or "jump to subroutine".
  OP_JAL = 38, OP_JALC = 39,
  // JALR: pop the current call frame and jump and link to <imm32>. Ie, a tail call.
  // JALRC: ass JALR, but target address is instead popped off the data stack.
  OP_JALR = 40, OP_JALRC = 41,

  // TODO ldlr/stlr: load/store link register? stlr would allow alternate continuations/returns

  // 42-43 undefined

  // ENTR <imm8>: allocate <imm8> registers on the call stack and then push the previous frame pointer on the call stack.
  // Effectively, this one instruction handles most of the function prologue (except perhaps moving arguments from the data stack into registers where necessary).
  OP_ENTR = 44,

  // 45 undefined

  // RET: Pop the current call frame and transfer control to the address stored in the link register.
  OP_RET = 46,

  // 47 undefined

  ////// MOV 48-63 //////

  // Manipulate the stack directly.
  // PUSH pushed an immediate value onto the stack.
  // POP pops the stack and discards the value.
  // DUP (duplicate) peeks a value from the stack and pushed it.
  // SWP (swap) pops two values from the stack and pushes them in reverse order.
  OP_PUSH = 48, OP_POP = 49, OP_DUP = 50, OP_SWP = 51,

  // LD (load), ST (store), and TEE transfer values between the stack and other memory regions
  // Loads copy the value in target memory and push it onto the stack.
  // Stores pop a value and write it to the target memory.
  // Stores copy a value (peek) and write it to the target memory (like DUP+ST).
  // The R- suffix targets registers aka local variables.
  OP_LDR = 52, OP_STR = 53, OP_TEER = 54,
  // 55 undefined
  // Without a mnemonic suffix, main memory is the target.
  OP_LD = 56, OP_ST = 57, OP_TEE = 58,
  // 59 undefined
  // The B- suffix indicates loading a byte from memory,
  // zero-extending or truncating where necessary.
  // For sign-extension, follow up with a SEXT operation.
  OP_LDB = 60, OP_STB = 61, OP_TEEB = 62,
  // 63 undefined

  ////// ALU 64-87 and 96-119 //////

  // Arithmetic 64-71 and 96-103 //
  // SEXT: low 8 bits remain same, higher bits are set to the 8th bit (thus, signed byte -> signed word)
  OP_NEG = 64,                   OP_SEXT = 96,
  // 65 undefined                // 97 undefined
  OP_ADD = 66, OP_SUB = 67,      OP_ADDC = 98, OP_SUBB = 99,
  OP_UMUL = 68, OP_UDIV = 69,    OP_UMULW = 100, OP_UDIVW = 101,
  OP_IMUL = 70, OP_IDIV = 71,    OP_IMULW = 102, OP_IDIVW = 103,

  // Logical 72-79 and 104-111 //

  // NOT: if pop is zero, push one, else push zero
  // CMP: push -1/0/1 if pop is lt/eq/gt zero
  OP_NOT = 72,                  OP_CMP = 104,
  OP_MIN = 73,                  OP_MAX = 105,
  // (N)Z: push 1 if pop is (non)zero, else push 1
  OP_Z = 74,                    OP_NZ = 106,
  OP_EQ = 75,                   OP_NEQ = 107,
  OP_ULT = 76, OP_ULTE = 77,    OP_UGTE = 108, OP_UGT = 109,
  OP_ILT = 78, OP_ILTE = 79,    OP_IGTE = 110, OP_IGT = 111,

  // Bitwise 80-87 //

  // Inversion/one's complement/bitwise negation
  OP_INV = 80,
  // 81 undefined
  OP_OR = 82, OP_AND = 83,
  OP_XOR = 84,
  // 85-87 undefined

  // Shifts 112-119 //
  OP_SHL = 112,
  OP_ROT = 113, // shift left, low bits filled from high bits
  // {U,I}SHR: shift right, high buts filled with {zeros, high bit}
  OP_USHR = 114, OP_ISHR = 115,
  // 116-117 undefined
  // WSHR: pop A, B. push B shifted right with high bits filled from low bits of A
  OP_WSHR = 118,
  // 119 undefined

  ////// IO 88-95 //////

  // open (a file/device identified by a length-n bytestring; returns a device handle)
  // close (disconnects from the device identified by the handle)
  // stat (gets data about a device handle: could be tape/block/read- or write-once/new, mode indicator, buffering state, errors possibly)
  //   read, write, noseek/slowseek/fastseek
  // gets/puts (and perhaps get/put when you know you're going char by char)
  // ??? peek (to get next byte but not advance position)
  // tell/seek (always in number of bytes)
  // eof (say if we're at end of file)
  // ??? mktmp (only for a temp file, as there may not be folders on the host system)

  ////// VM 120-127 //////

  // TODO argc, argv
  // TODO stdin, stdout, stderr
  OP_HLT = 127

  ////// no native floating point //////

  ////// no conversion instructions ////

  // There are no conversion instructions, because the VM is fuindamentally untyped.
  // An assembly language could ofc implement relevant pseudo-instructions to help with type analysis.

  ////// 128-255 reserved for future extensions //////

  ////// 256+ are never available for use, even on 9+ bit machines //////

} opcode;
