// This file is copied and modified from The LLVM Compiler Infrastructure, which
// is distributed under the Apache License v2.0 with LLVM Exceptions (see
// LICENSE.TXT for details). This file is licensed under the same license.

// Temple 16-bit C return-value convention.
def RetCC_Temple : CallingConv<[CCIfType<[i16], CCAssignToReg<[R0, R1]>>]>;

// Temple 16-bit C Calling convention.
def CC_Temple : CallingConv<[
  // Promote i8 args to i16
  CCIfType<[i8], CCPromoteToType<i16>>,

  // All arguments get passed in integer registers if there is space.
  CCIfType<[i16], CCAssignToReg<[R0, R1, R2, R3, R4, R5, R6, R7]>>,

  // Could be assigned to the stack in 8-byte aligned units, but unsupported
  CCAssignToStack<2, 2> // The slot's size, the stack alignment
]>;

def CSR : CalleeSavedRegs<(add(sequence "R%u", 2, 3))>;