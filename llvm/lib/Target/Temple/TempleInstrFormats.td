//===-- TempleInstrFormats.td - Temple Instruction Formats -----*- tablegen
//-*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//  Describe Temple instructions format
//
//  CPU INSTRUCTION FORMATS
//
//  opcode  - operation code.
//  ra      - 1st reg.
//  rb      - 3rd reg only used in J type.
//  cond    - 2nd data only used in J type.
//  cx      - immediate only used in I type.
//
//===----------------------------------------------------------------------===//

// Format specifies the encoding used by the instruction.  This is part of the
// ad-hoc solution used to emit machine instruction encodings by our machine
// code emitter.
class Format<bits<4> val> {
  bits<4> Value = val;
}

def Pseudo : Format<0>;
def FrmF : Format<1>;
def FrmJ : Format<2>;
def FrmI : Format<3>;

// Generic Temple Format
class TempleInst<dag outs, dag ins, string asmstr, InstrItinClass itin,
                 Format f, list<dag> pattern = []> : Instruction {
  // Inst and Size: for tablegen(... -gen-emitter) and
  // tablegen(... -gen-disassembler) in CMakeLists.txt

  Format Form = f;

  let Namespace = "Temple";

  let OutOperandList = outs;
  let InOperandList = ins;

  let AsmString = asmstr;
  let Pattern = pattern;
  let Itinerary = itin;

  //
  // Attributes specific to Temple instructions...
  //
  bits<4> FormBits = Form.Value;

  // TSFlags layout should be kept in sync with TempleInstrInfo.h.
  let TSFlags{3 - 0} = FormBits;

  let DecoderNamespace = "Temple";

  field bits<32> SoftFail = 0;
}

//===----------------------------------------------------------------------===//
// Format F instruction class in Temple : <|opcode|ra|>
//===----------------------------------------------------------------------===//

class FF<bits<3> op, dag outs, dag ins, string asmstr, InstrItinClass itin,
         list<dag> pattern = []>
    : TempleInst<outs, ins, asmstr, itin, FrmF, pattern> {
  field bits<8> Inst;
  bits<5> ra;

  let Size = 1;
  let hasSideEffects = 1;

  // Top 3 bits are the 'opcode' field
  let Inst{7 - 5} = op;
  let Inst{4 - 0} = ra;
}

//===----------------------------------------------------------------------===//
// Format F no operand instruction class in Temple : <|opcode|>
//===----------------------------------------------------------------------===//

class FFnoOP<bits<3> op, dag outs, dag ins, string asmstr, InstrItinClass itin,
             list<dag> pattern = []>
    : TempleInst<outs, ins, asmstr, itin, FrmF, pattern> {
  field bits<8> Inst;

  let Size = 1;
  let hasSideEffects = 1;

  // Top 3 bits are the 'opcode' field
  let Inst{7 - 5} = op;
  let Inst{4 - 0} = 0;
}

//@class FJ {
//===----------------------------------------------------------------------===//
// Format J instruction class in Temple : <|opcode|rb|cond|ra|>
//===----------------------------------------------------------------------===//

class FJ<bits<3> op, dag outs, dag ins, string asmstr, InstrItinClass itin,
         list<dag> pattern = []>
    : TempleInst<outs, ins, asmstr, itin, FrmJ, pattern> {
  field bits<16> Inst;
  bits<5> ra;
  bits<3> cond;
  bits<5> rb;

  let Size = 2;

  let Inst{15 - 13} = op;
  let Inst{12 - 8} = rb;
  let Inst{7 - 5} = cond;
  let Inst{4 - 0} = ra;
}
//@class FJ }

//===----------------------------------------------------------------------===//
// Format I instruction class in Temple : <|opcode|immediate|>
//===----------------------------------------------------------------------===//

class FI<bits<3> op, dag outs, dag ins, string asmstr, InstrItinClass itin,
         list<dag> pattern = []>
    : TempleInst<outs, ins, asmstr, itin, FrmI, pattern> {
  field bits<24> Inst;
  bits<16> imm16;

  let Size = 3;

  let Inst{23 - 21} = op;
  let Inst{20 - 16} = 0;
  let Inst{15 - 0} = imm16;
}

// Pseudo instructions
class Pseudo<dag outs, dag ins, list<dag> pattern = []>
    : TempleInst<outs, ins, "", IIPseudo, Pseudo, pattern> {
  let isPseudo = 1;
  let isCodeGenOnly = 1;
}