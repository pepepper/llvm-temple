//===-- TempleRegisterInfo.td - Temple Register defs -----------*- tablegen
//-*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//@Registers
//===----------------------------------------------------------------------===//
// The register string, such as "9" or "r9" will show on "llvm-objdump -d"
//@ All registers definition
let Namespace = "Temple" in {
  class TempleReg<bits<5> Enc, string n> : Register<n> {
    // For tablegen(... -gen-emitter)  in CMakeLists.txt
    let HWEncoding{4 - 0} = Enc;
  } class TempleConstReg<bits<5> Enc, string n> : Register<n> {
    // For tablegen(... -gen-emitter)  in CMakeLists.txt
    let HWEncoding{4 - 0} = Enc;
    let isConstant = true;
  } class TempleAuxReg<bits<5> Enc, string n> : Register<n> {
    // For tablegen(... -gen-emitter)  in CMakeLists.txt
    let HWEncoding{4 - 0} = Enc;
  }
  //@ General Purpose Registers
  def R0 : TempleReg<0, "r0">,
           DwarfRegNum<[0]>;
  def R1 : TempleReg<1, "r1">, DwarfRegNum<[1]>;
  def R2 : TempleReg<2, "r2">, DwarfRegNum<[2]>;
  def R3 : TempleReg<3, "r3">, DwarfRegNum<[3]>;
  def R4 : TempleReg<4, "r4">, DwarfRegNum<[4]>;
  def R5 : TempleReg<5, "r5">, DwarfRegNum<[5]>;
  def R6 : TempleReg<6, "r6">, DwarfRegNum<[6]>;
  def R7 : TempleReg<7, "r7">, DwarfRegNum<[7]>;
  def R8 : TempleReg<8, "r8">, DwarfRegNum<[8]>;
  def R9 : TempleReg<9, "r9">, DwarfRegNum<[9]>;
  def R10 : TempleReg<10, "r10">, DwarfRegNum<[10]>;
  def R11 : TempleReg<11, "r11">, DwarfRegNum<[11]>;
  def R12 : TempleReg<12, "r12">, DwarfRegNum<[12]>;
  def R13 : TempleReg<13, "r13">, DwarfRegNum<[13]>;
  def R14 : TempleReg<14, "r14">, DwarfRegNum<[14]>;
  def R15 : TempleReg<15, "r15">, DwarfRegNum<[15]>;
  def R16 : TempleReg<16, "r16">, DwarfRegNum<[16]>;
  def R17 : TempleReg<17, "r17">, DwarfRegNum<[17]>;
  def R18 : TempleReg<18, "r18">, DwarfRegNum<[18]>;
  def R19 : TempleReg<19, "r19">, DwarfRegNum<[19]>;
  def T0 : TempleReg<20, "t0">, DwarfRegNum<[20]>;
  def T1 : TempleReg<21, "t1">, DwarfRegNum<[21]>;
  def T2 : TempleReg<22, "t2">, DwarfRegNum<[22]>;
  def IW : TempleReg<23, "iw">, DwarfRegNum<[23]>;
  def SP : TempleReg<24, "sp">, DwarfRegNum<[24]>;
  def RA : TempleReg<25, "ra">, DwarfRegNum<[25]>;
  def SPC : TempleReg<26, "spc">, DwarfRegNum<[26]>;
  def IA : TempleReg<27, "ia">, DwarfRegNum<[27]>;
  def ZERO : TempleConstReg<28, "zero">, DwarfRegNum<[28]>;
  def ONE : TempleConstReg<29, "one">, DwarfRegNum<[29]>;
  def ALLONE : TempleConstReg<30, "allone">, DwarfRegNum<[30]>;
  def FLAG : TempleReg<31, "flag">, DwarfRegNum<[31]>;

  def ACC : TempleAuxReg<0, "acc">, DwarfRegNum<[32]>;
  def PC : TempleAuxReg<1, "pc">, DwarfRegNum<[33]>;
}

//===----------------------------------------------------------------------===//
//@Register Classes
//===----------------------------------------------------------------------===//

class TempleRegisterClass<string namespace, list<ValueType> regTypes,
                          int alignment, dag regList,
                          RegAltNameIndex idx = NoRegAltName>
    : RegisterClass<namespace, regTypes, alignment, regList, idx> {
  let CopyCost = 5;
}

// def CPURegs
//     : TempleRegisterClass<"Temple", [i16], 16,
//                     (add
//                          // General
//                          R0,
//                      R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13,
//                      R14, R15, R16, R17, R18, R19,
//                      // Reserved
//                      T0, T1, T2, IW, SP, RA, SPC, IA, ZERO, ONE, ALLONE,
//                      FLAG)>;

//@Status Registers class
def SR : TempleRegisterClass<"Temple", [i16], 16, (add FLAG)>;

//@ Program counter Registers class
def PCRegs : RegisterClass<"Temple", [i16], 16, (add PC, SPC)>;

def AccReg : RegisterClass<"Temple", [i16], 16, (add ACC)>;
def GPR : TempleRegisterClass<"Temple", [i16], 16,
                              (add(sequence "R%u", 0, 19), SP, RA, ZERO, ONE,
                               ALLONE)>;
def PseudoOut
    : TempleRegisterClass<"Temple", [i16], 16, (add(sequence "T%u", 0, 2))>;