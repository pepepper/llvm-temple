// This file is copied and modified from The LLVM Compiler Infrastructure, which
// is distributed under the Apache License v2.0 with LLVM Exceptions (see
// LICENSE.TXT for details). This file is licensed under the same license.

#include "MCTargetDesc/TempleMCTargetDesc.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCDecoderOps.h"
#include "llvm/MC/MCDisassembler/MCDisassembler.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/Endian.h"
#include "llvm/Support/FormatVariadic.h"

using namespace llvm;

#define DEBUG_TYPE "Temple-disassembler"

typedef MCDisassembler::DecodeStatus DecodeStatus;

namespace {
class TempleDisassembler : public MCDisassembler {

public:
  TempleDisassembler(const MCSubtargetInfo &STI, MCContext &Ctx)
      : MCDisassembler(STI, Ctx) {}

  DecodeStatus getInstruction(MCInst &Instr, uint64_t &Size,
                              ArrayRef<uint8_t> Bytes, uint64_t Address,
                              raw_ostream &CStream) const override;
};
} // end anonymous namespace

static MCDisassembler *createTempleDisassembler(const Target &T,
                                                const MCSubtargetInfo &STI,
                                                MCContext &Ctx) {
  return new TempleDisassembler(STI, Ctx);
}

extern "C" void LLVMInitializeTempleDisassembler() {
  TargetRegistry::RegisterMCDisassembler(getTheTempleTarget(),
                                         createTempleDisassembler);
}

static const unsigned GPRDecoderTable[] = {
    Temple::R0,    Temple::R1,  Temple::R2,  Temple::R3,   Temple::R4,
    Temple::R5,    Temple::R6,  Temple::R7,  Temple::R8,   Temple::R9,
    Temple::R10,   Temple::R11, Temple::R12, Temple::R13,  Temple::R14,
    Temple::R15,   Temple::R16, Temple::R17, Temple::R18,  Temple::R19,
    Temple::T0,    Temple::T1,  Temple::T2,  Temple::IW,   Temple::SP,
    Temple::RA,    Temple::SPC, Temple::IA,  Temple::ZERO, Temple::ONE,
    Temple::ALLONE};


static DecodeStatus DecodeGPRRegisterClass(MCInst &Inst, uint64_t RegNo,
                                              uint64_t Address,
                                              const void *Decoder) {
  if (RegNo > sizeof(GPRDecoderTable))
    return MCDisassembler::Fail;

  unsigned Reg = GPRDecoderTable[RegNo];
  Inst.addOperand(MCOperand::createReg(Reg));
  return MCDisassembler::Success;
}

static DecodeStatus decodeSimm16(MCInst &Inst, uint64_t Imm, int64_t Address,
                                 const void *Decoder) {
  assert(isUInt<16>(Imm) && "Invalid immediate");
  // Sign-extend the number in the bottom N bits of Imm
  Inst.addOperand(MCOperand::createImm(SignExtend64<16>(Imm)));
  return MCDisassembler::Success;
}

static DecodeStatus decodeCond(MCInst &Inst, uint64_t Imm, int64_t Address,
                               const void *Decoder) {
  assert(isUInt<3>(Imm) && "Invalid immediate");
  // Sign-extend the number in the bottom N bits of Imm
  Inst.addOperand(MCOperand::createImm(Imm));
  return MCDisassembler::Success;
}

#include "TempleGenDisassemblerTables.inc"

DecodeStatus TempleDisassembler::getInstruction(MCInst &MI, uint64_t &Size,
                                                ArrayRef<uint8_t> Bytes,
                                                uint64_t Address,
                                                raw_ostream &CS) const {

  uint32_t Inst;
  DecodeStatus Result;

  uint8_t opcode = Bytes[0] >> 5;

  switch (opcode) {
  case 0b101: // I type(24bit)
    if (Bytes.size() < 3) {
      Size = 0;
      return MCDisassembler::Fail;
    }
    Inst = (Bytes[0] << 16) | (Bytes[1] << 8) | Bytes[2];
    LLVM_DEBUG(dbgs() << "Trying Temple 24bit table :\n");
    Result =
        decodeInstruction(DecoderTableTemple24, MI, Inst, Address, this, STI);
    Size = 3;
    break;
  case 0b110: // J type(16bit)
    if (Bytes.size() < 2) {
      Size = 0;
      return MCDisassembler::Fail;
    }
    Inst = support::endian::read16be(Bytes.data());
    LLVM_DEBUG(dbgs() << "Trying Temple 16bit table :\n");
    Result =
        decodeInstruction(DecoderTableTemple16, MI, Inst, Address, this, STI);
    Size = 2;
    break;
  default: // F type(8bit)
    LLVM_DEBUG(dbgs() << "Trying Temple 8bit table :\n");
    Result = decodeInstruction(DecoderTableTemple8, MI, Bytes[0], Address, this,
                               STI);
    Size = 1;
    break;
  }
  return Result;
}