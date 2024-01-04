//===----------------------------------------------------------------------===//
// Register file, calling conventions, instruction descriptions.
//===----------------------------------------------------------------------===//
include "llvm/Target/Target.td"

include "TempleSchedule.td"
include "TempleRegisterInfo.td"
include "TempleCallingConv.td"
include "TemplePseudoInstr.td"

//===----------------------------------------------------------------------===//
// Temple processors supported.
//===----------------------------------------------------------------------===//

def : ProcessorModel<"generic", NoSchedModel, []>;

//===----------------------------------------------------------------------===//
// Define the Temple target.
//===----------------------------------------------------------------------===//

def TempleInstrInfo : InstrInfo { let guessInstructionProperties = 0; }

def TempleAsmParser : AsmParser {
  // Use alternative names of registers when emitting.
  let ShouldEmitMatchRegisterAltName = 1;
}

def TempleAsmWriter : AsmWriter { int PassSubtarget = 1; }

def Temple : Target {
  let InstructionSet = TempleInstrInfo;
  let AssemblyParsers = [TempleAsmParser];
  let AssemblyWriters = [TempleAsmWriter];
  let AllowRegisterRenaming = 1;
}