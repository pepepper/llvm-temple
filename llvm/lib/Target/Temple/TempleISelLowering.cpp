// This file is copied and modified from The LLVM Compiler Infrastructure, which
// is distributed under the Apache License v2.0 with LLVM Exceptions (see
// LICENSE.TXT for details). This file is licensed under the same license.

#include "TempleISelLowering.h"
#include "MCTargetDesc/TempleBaseInfo.h"
#include "Temple.h"
#include "TempleInstrInfo.h"
#include "TempleRegisterInfo.h"
#include "TempleSubtarget.h"
#include "TempleTargetMachine.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/SelectionDAGISel.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/CodeGen/ValueTypes.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/DiagnosticPrinter.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

#define DEBUG_TYPE "temple-lower"

TempleTargetLowering::TempleTargetLowering(const TargetMachine &TM,
                                           const TempleSubtarget &STI)
    : TargetLowering(TM), Subtarget(STI) {

  // Set up the register classes.
  addRegisterClass(MVT::i16, &Temple::GPRRegClass);

  // Compute derived properties from the register classes.
  computeRegisterProperties(STI.getRegisterInfo());

  setStackPointerRegisterToSaveRestore(Temple::SP);

  for (auto N : {ISD::EXTLOAD, ISD::SEXTLOAD, ISD::ZEXTLOAD}){
    setLoadExtAction(N, MVT::i16, MVT::i1, Promote);
  }

  // TODO: add all necessary setOperationAction calls.
  setOperationAction(ISD::GlobalAddress, MVT::i16, Custom);
  setOperationAction(ISD::SELECT_CC, MVT::i16, Expand);
  setOperationAction(ISD::BRCOND, MVT::Other, Expand);
  setOperationAction(ISD::BR_CC, MVT::i16, Custom);
  //   setOperationAction(ISD::BR, MVT::Other, Custom);
  setOperationAction(ISD::BR_JT, MVT::Other, Expand);
  setOperationAction(ISD::SETCC, MVT::i16, Custom);
  for (auto VT : {MVT::i1, MVT::i8})
    setOperationAction(ISD::SIGN_EXTEND_INREG, VT, Expand);

  setOperationAction(ISD::MUL, MVT::i16, Expand);
  setOperationAction(ISD::SMUL_LOHI, MVT::i16, Expand);
  setOperationAction(ISD::UMUL_LOHI, MVT::i16, Expand);
  setOperationAction(ISD::MULHS, MVT::i16, Expand);
  setOperationAction(ISD::MULHU, MVT::i16, Expand);

  setOperationAction(ISD::SREM, MVT::i16, Expand);
  setOperationAction(ISD::SDIVREM, MVT::i16, Expand);
  setOperationAction(ISD::SDIV, MVT::i16, Expand);
  setOperationAction(ISD::UREM, MVT::i16, Expand);
  setOperationAction(ISD::UDIVREM, MVT::i16, Expand);
  setOperationAction(ISD::UDIV, MVT::i16, Expand);

  setOperationAction(ISD::DYNAMIC_STACKALLOC, MVT::i16, Expand);
  setOperationAction(ISD::STACKSAVE, MVT::Other, Expand);
  setOperationAction(ISD::STACKRESTORE, MVT::Other, Expand);

  setBooleanContents(ZeroOrOneBooleanContent);

  //   // Function alignments (log2).
  //   setMinFunctionAlignment(Align(0));
  //   setPrefFunctionAlignment(Align(0));

  // Effectively disable jump table generation.
  setMinimumJumpTableEntries(SHRT_MAX);
}

SDValue TempleTargetLowering::LowerOperation(SDValue Op,
                                             SelectionDAG &DAG) const {
  switch (Op.getOpcode()) {
  default:
    report_fatal_error("unimplemented operand");

  case ISD::BR_CC:
    return LowerBR_CC(Op, DAG);
  case ISD::SETCC:
    return LowerSETCC(Op, DAG);

    // case ISD::BR:
    //   return LowerBR(Op, DAG);

    // case ISD::SELECT_CC:
    //   return LowerSELECT(Op, DAG);

  case ISD::GlobalAddress:
    return LowerGlobalAddress(Op, DAG);

  case ISD::FRAMEADDR:
    return LowerFRAMEADDR(Op, DAG);

  case ISD::RETURNADDR:
    return LowerRETURNADDR(Op, DAG);
  }
}

// SDValue TempleTargetLowering::LowerBR(SDValue Op, SelectionDAG &DAG) const {
//   SDLoc dl(Op);
//   EVT VT = Op.getValueType();
//   SDValue Chain = Op.getOperand(0);
//   SDValue Dest = Op.getOperand(1);
// //   MachineFunction &MF = DAG.getMachineFunction();
// //   Register Reg =
// MF.getRegInfo().createVirtualRegister(&Temple::GPRRegClass);
// //   Chain = DAG.getCopyToReg(Chain, dl, Reg, Dest);
//   return DAG.getNode(ISD::BRIND, dl, VT, Chain, Dest);
// }

// SDValue TempleTargetLowering::LowerSELECT(SDValue Op, SelectionDAG &DAG)
// const {
//   SDLoc dl(Op);
//   EVT VT = Op.getValueType();
//   SDValue LHS = Op.getOperand(0);
//   SDValue RHS = Op.getOperand(1);
//   SDValue TrueV = Op.getOperand(2);
//   SDValue FalseV = Op.getOperand(3);
//   ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(4))->get();

//   return DAG.getNode(TempleISD::SELECT_CC, dl, VT, TrueV, FalseV, TempleCC,
//                      Flag);
// }

SDValue TempleTargetLowering::LowerBR_CC(SDValue Op, SelectionDAG &DAG) const {

  SDLoc dl(Op);
  EVT VT = Op.getValueType();

  SDValue LHS = Op.getOperand(2);
  SDValue RHS = Op.getOperand(3);
  ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(1))->get();
  switch (CC) {
  case ISD::SETEQ:
    return SDValue(DAG.getMachineNode(Temple::BEQ, dl, VT, Op.getOperand(4), LHS, RHS),0);
  case ISD::SETNE:{
    SDValue a=SDValue(DAG.getMachineNode(Temple::BNE, dl, VT, Op.getOperand(4), LHS, RHS),0);
a.dump(&DAG);
    return a;
  }
  case ISD::SETGT:
    std::swap(LHS, RHS);
  case ISD::SETLT:
    return SDValue(DAG.getMachineNode(Temple::BLT, dl, VT, Op.getOperand(4), LHS, RHS),0);
  case ISD::SETLE:
    std::swap(LHS, RHS);
  case ISD::SETGE:
    return SDValue(DAG.getMachineNode(Temple::BGE, dl, VT, Op.getOperand(4), LHS, RHS),0);
  case ISD::SETULT:
    return SDValue(DAG.getMachineNode(Temple::BLTU, dl, VT, Op.getOperand(4), LHS, RHS),0);
  case ISD::SETUGE:
    return SDValue(DAG.getMachineNode(Temple::BGEU, dl, VT, Op.getOperand(4), LHS, RHS),0);
  default:
    dbgs()<<"LowerBR_CC:lowering failed?\n";
    break;
  }
}

SDValue TempleTargetLowering::LowerSETCC(SDValue Op, SelectionDAG &DAG) const {

  SDLoc dl(Op);
  EVT VT = Op.getValueType();

  SDValue LHS = Op.getOperand(0);
  SDValue RHS = Op.getOperand(1);
  ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(2))->get();
  switch (CC) {
  case ISD::SETEQ:
    return SDValue(
        DAG.getMachineNode(Temple::SETEQr, dl, VT, LHS, RHS), 0);
  case ISD::SETNE:
    return SDValue(
        DAG.getMachineNode(Temple::SETNEr, dl, VT, LHS, RHS), 0);

  case ISD::SETGT:
    std::swap(LHS, RHS);
  case ISD::SETLT:
    return SDValue(
        DAG.getMachineNode(Temple::SETLTr, dl, VT, LHS, RHS), 0);

  case ISD::SETLE:
    std::swap(LHS, RHS);
  case ISD::SETGE:
    return SDValue(
        DAG.getMachineNode(Temple::SETGEr, dl, VT, LHS, RHS), 0);

  case ISD::SETUGT:
    std::swap(LHS, RHS);
  case ISD::SETULT:
    return SDValue(
        DAG.getMachineNode(Temple::SETULTr, dl, VT, LHS, RHS),
        0);

  case ISD::SETULE:
    std::swap(LHS, RHS);
  case ISD::SETUGE:
    return SDValue(
        DAG.getMachineNode(Temple::SETUGEr, dl, VT, LHS, RHS),
        0);
  default:
    dbgs() << "LowerSETCC:lowering failed?\n";
    break;
  }
}

SDValue TempleTargetLowering::LowerGlobalAddress(SDValue Op,
                                                 SelectionDAG &DAG) const {
  SDLoc DL(Op);
  EVT Ty = Op.getValueType();
  GlobalAddressSDNode *N = cast<GlobalAddressSDNode>(Op);
  const GlobalValue *GV = N->getGlobal();
  int64_t Offset = N->getOffset();

  if (isPositionIndependent())
    report_fatal_error("Unable to LowerGlobalAddress");

  SDValue GA = DAG.getTargetGlobalAddress(GV, DL, Ty, Offset);
  SDValue MN = SDValue(DAG.getMachineNode(Temple::MOV, DL, Ty, GA), 0);
  return MN;
}

SDValue TempleTargetLowering::LowerFRAMEADDR(SDValue Op,
                                             SelectionDAG &DAG) const {
  const TempleRegisterInfo &RI = *Subtarget.getRegisterInfo();
  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  MFI.setFrameAddressIsTaken(true);
  unsigned FrameReg = RI.getFrameRegister(MF);

  EVT VT = Op.getValueType();
  SDLoc DL(Op);
  SDValue FrameAddr = DAG.getCopyFromReg(DAG.getEntryNode(), DL, FrameReg, VT);
  unsigned Depth = cast<ConstantSDNode>(Op.getOperand(0))->getZExtValue();
  while (Depth--) {
    int Offset = -2;
    SDValue Ptr = DAG.getNode(ISD::ADD, DL, VT, FrameAddr,
                              DAG.getIntPtrConstant(Offset, DL));
    FrameAddr =
        DAG.getLoad(VT, DL, DAG.getEntryNode(), Ptr, MachinePointerInfo());
  }
  return FrameAddr;
}

SDValue TempleTargetLowering::LowerRETURNADDR(SDValue Op,
                                              SelectionDAG &DAG) const {
  const TempleRegisterInfo &RI = *Subtarget.getRegisterInfo();
  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  MFI.setReturnAddressIsTaken(true);

  if (verifyReturnAddressArgumentIsConstant(Op, DAG))
    return SDValue();

  EVT VT = Op.getValueType();
  SDLoc DL(Op);
  unsigned Depth = cast<ConstantSDNode>(Op.getOperand(0))->getZExtValue();
  if (Depth) {
    int Off = -2;
    SDValue FrameAddr = LowerFRAMEADDR(Op, DAG);
    SDValue Offset = DAG.getConstant(Off, DL, VT);
    return DAG.getLoad(VT, DL, DAG.getEntryNode(),
                       DAG.getNode(ISD::ADD, DL, VT, FrameAddr, Offset),
                       MachinePointerInfo());
  }

  // Return the value of the return address register, marking it an implicit
  // live-in.
  unsigned Reg = MF.addLiveIn(RI.getRARegister(), &Temple::GPRRegClass);
  return DAG.getCopyFromReg(DAG.getEntryNode(), DL, Reg, MVT::i16);
}

// static unsigned getBranchOpcodeForIntCondCode(Temple::CondCode CC) {
//   switch (CC) {
//   default:
//     llvm_unreachable("Unsupported CondCode");
//   case Temple::COND_Z:
//     return Temple::JLZero;
//   case Temple::COND_C:
//     return Temple::JLCarry;
//   case Temple::COND_M:
//     return Temple::JLNeg;
//   }
// }

// MachineBasicBlock *
// TempleTargetLowering::EmitInstrWithCustomInserter(MachineInstr &MI,
//                                                   MachineBasicBlock *BB)
//                                                   const {
//   const TargetInstrInfo &TII =
//   *BB->getParent()->getSubtarget().getInstrInfo(); DebugLoc DL =
//   MI.getDebugLoc();

//   // assert(MI.getOpcode() == Temple::Select_GPR_Using_CC_GPR &&
//   //        "Unexpected instr type to insert");

//   // To "insert" a SELECT instruction, we actually have to insert the
//   triangle
//   // control-flow pattern.  The incoming instruction knows the destination
//   vreg
//   // to set, the condition code register to branch on, the true/false values
//   to
//   // select between, and the condcode to use to select the appropriate
//   branch.
//   //
//   // We produce the following control flow:
//   //     HeadMBB
//   //     |  \
//   //     |  IfFalseMBB
//   //     | /
//   //    TailMBB
//   const BasicBlock *LLVM_BB = BB->getBasicBlock();
//   MachineFunction::iterator I = ++BB->getIterator();

//   MachineBasicBlock *HeadMBB = BB;
//   MachineFunction *F = BB->getParent();
//   MachineBasicBlock *TailMBB = F->CreateMachineBasicBlock(LLVM_BB);
//   MachineBasicBlock *IfFalseMBB = F->CreateMachineBasicBlock(LLVM_BB);

//   F->insert(I, IfFalseMBB);
//   F->insert(I, TailMBB);
//   // Move all remaining instructions to TailMBB.
//   TailMBB->splice(TailMBB->begin(), HeadMBB,
//                   std::next(MachineBasicBlock::iterator(MI)),
//                   HeadMBB->end());
//   // Update machine-CFG edges by transferring all successors of the current
//   // block to the new block which will contain the Phi node for the select.
//   TailMBB->transferSuccessorsAndUpdatePHIs(HeadMBB);
//   // Set the successors for HeadMBB.
//   HeadMBB->addSuccessor(IfFalseMBB);
//   HeadMBB->addSuccessor(TailMBB);

//   // Insert appropriate branch.
//   unsigned LHS = MI.getOperand(1).getReg();
//   unsigned RHS = MI.getOperand(2).getReg();
//   auto CC = static_cast<Temple::CondCode>(MI.getOperand(3).getImm());
//   unsigned Opcode = getBranchOpcodeForIntCondCode(CC);

//   BuildMI(HeadMBB, DL, TII.get(Opcode)).addMBB(TailMBB);

//   // IfFalseMBB just falls through to TailMBB.
//   IfFalseMBB->addSuccessor(TailMBB);

//   // %Result = phi [ %TrueValue, HeadMBB ], [ %FalseValue, IfFalseMBB ]
//   BuildMI(*TailMBB, TailMBB->begin(), DL, TII.get(Temple::PHI),
//           MI.getOperand(0).getReg())
//       .addReg(MI.getOperand(4).getReg())
//       .addMBB(HeadMBB)
//       .addReg(MI.getOperand(5).getReg())
//       .addMBB(IfFalseMBB);

//   MI.eraseFromParent(); // The pseudo instruction is gone now.
//   return TailMBB;
// }

// Calling Convention Implementation.
#include "TempleGenCallingConv.inc"

// Transform physical registers into virtual registers.
SDValue TempleTargetLowering::LowerFormalArguments(
    SDValue Chain, CallingConv::ID CallConv, bool IsVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &DL,
    SelectionDAG &DAG, SmallVectorImpl<SDValue> &InVals) const {

  switch (CallConv) {
  default:
    report_fatal_error("Unsupported calling convention");
  case CallingConv::C:
  case CallingConv::Fast:
    break;
  }

  MachineFunction &MF = DAG.getMachineFunction();
  MachineRegisterInfo &RegInfo = MF.getRegInfo();

  if (IsVarArg)
    report_fatal_error("VarArg not supported");

  // Assign locations to all of the incoming arguments.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, IsVarArg, MF, ArgLocs, *DAG.getContext());
  CCInfo.AnalyzeFormalArguments(Ins, CC_Temple);

  for (auto &VA : ArgLocs) {
    SDValue ArgVal;
    if (VA.isRegLoc()) {
      // Arguments passed in registers.
      EVT RegVT = VA.getLocVT();
      if (RegVT != MVT::i16) {
        LLVM_DEBUG(dbgs() << "LowerFormalArguments Unhandled argument type: "
                          << RegVT.getEVTString() << "\n");
        report_fatal_error("unhandled argument type");
      }
      const unsigned VReg = RegInfo.createVirtualRegister(&Temple::GPRRegClass);
      RegInfo.addLiveIn(VA.getLocReg(), VReg);
      ArgVal = DAG.getCopyFromReg(Chain, DL, VReg, RegVT);
    } else {
      assert(VA.isMemLoc());
      assert(VA.getValVT() == MVT::i16);
      assert(VA.getLocVT() == MVT::i16);

      // Create the frame index object for this incoming parameter.
      int FI =
          MF.getFrameInfo().CreateFixedObject(2, VA.getLocMemOffset(), true);

      // Create the SelectionDAG nodes corresponding to a load from this
      // parameter.
      SDValue FIN = DAG.getFrameIndex(FI, MVT::i16);
      ArgVal = DAG.getLoad(
          VA.getLocVT(), DL, Chain, FIN,
          MachinePointerInfo::getFixedStack(DAG.getMachineFunction(), FI));
    }

    InVals.push_back(ArgVal);
  }
  return Chain;
}

// Lower a call to a callseq_start + CALL + callseq_end chain, and add input
// and output parameter nodes.
SDValue
TempleTargetLowering::LowerCall(CallLoweringInfo &CLI,
                                SmallVectorImpl<SDValue> &InVals) const {
  SelectionDAG &DAG = CLI.DAG;
  SDLoc &DL = CLI.DL;
  SmallVectorImpl<ISD::OutputArg> &Outs = CLI.Outs;
  SmallVectorImpl<SDValue> &OutVals = CLI.OutVals;
  SmallVectorImpl<ISD::InputArg> &Ins = CLI.Ins;
  SDValue Chain = CLI.Chain;
  SDValue Callee = CLI.Callee;
  CLI.IsTailCall = false;
  CallingConv::ID CallConv = CLI.CallConv;
  bool IsVarArg = CLI.IsVarArg;
  EVT PtrVT = getPointerTy(DAG.getDataLayout());

  if (IsVarArg) {
    report_fatal_error("LowerCall with varargs not implemented");
  }

  MachineFunction &MF = DAG.getMachineFunction();

  // Analyze the operands of the call, assigning locations to each operand.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState ArgCCInfo(CallConv, IsVarArg, MF, ArgLocs, *DAG.getContext());
  ArgCCInfo.AnalyzeCallOperands(Outs, CC_Temple);

  // Get a count of how many bytes are to be pushed on the stack.
  unsigned NumBytes = ArgCCInfo.getNextStackOffset();

  // Create local copies for byval args
  SmallVector<SDValue, 8> ByValArgs;
  for (unsigned i = 0, e = Outs.size(); i != e; ++i) {
    ISD::ArgFlagsTy Flags = Outs[i].Flags;
    if (!Flags.isByVal())
      continue;

    SDValue Arg = OutVals[i];
    unsigned Size = Flags.getByValSize();
    Align Align = Flags.getNonZeroByValAlign();

    int FI = MF.getFrameInfo().CreateStackObject(Size, Align,
                                                 /*isSillSlot=*/false);
    SDValue FIPtr = DAG.getFrameIndex(FI, MVT::i16);
    SDValue SizeNode = DAG.getConstant(Size, DL, MVT::i16);

    Chain = DAG.getMemcpy(Chain, DL, FIPtr, Arg, SizeNode, Align,
                          /*IsVolatile=*/false,
                          /*AlwaysInline=*/false, CLI.IsTailCall,
                          MachinePointerInfo(), MachinePointerInfo());
    ByValArgs.push_back(FIPtr);
  }

  Chain = DAG.getCALLSEQ_START(Chain, NumBytes, 0, CLI.DL);

  // Copy argument values to their designated locations.
  SmallVector<std::pair<unsigned, SDValue>, 8> RegsToPass;
  SmallVector<SDValue, 8> MemOpChains;
  SDValue StackPtr;
  for (unsigned I = 0, J = 0, E = ArgLocs.size(); I != E; ++I) {
    CCValAssign &VA = ArgLocs[I];
    SDValue ArgValue = OutVals[I];
    ISD::ArgFlagsTy Flags = Outs[I].Flags;

    // Promote the value if needed.
    // For now, only handle fully promoted arguments.
    switch (VA.getLocInfo()) {
    case CCValAssign::Full:
      break;
    default:
      llvm_unreachable("Unknown loc info!");
    }

    if (Flags.isByVal())
      ArgValue = ByValArgs[J++];

    if (VA.isRegLoc()) {
      // Queue up the argument copies and emit them at the end.
      RegsToPass.push_back(std::make_pair(VA.getLocReg(), ArgValue));
    } else {
      assert(VA.isMemLoc() && "Argument not register or memory");

      if (!StackPtr.getNode())
        StackPtr = DAG.getCopyFromReg(Chain, DL, Temple::SP, MVT::i16);

      SDValue Address =
          DAG.getNode(ISD::ADD, DL, MVT::i16, StackPtr,
                      DAG.getIntPtrConstant(VA.getLocMemOffset(), DL));

      MemOpChains.push_back(
          DAG.getStore(Chain, DL, ArgValue, Address, MachinePointerInfo()));
    }
  }

  // Transform all store nodes into one single node because all store nodes are
  // independent of each other.
  if (!MemOpChains.empty())
    Chain = DAG.getNode(ISD::TokenFactor, DL, MVT::Other, MemOpChains);

  SDValue Glue;

  // Build a sequence of copy-to-reg nodes, chained and glued together.
  for (auto &Reg : RegsToPass) {
    Chain = DAG.getCopyToReg(Chain, DL, Reg.first, Reg.second, Glue);
    Glue = Chain.getValue(1);
  }

  if (GlobalAddressSDNode *S = dyn_cast<GlobalAddressSDNode>(Callee))
    Callee = DAG.getTargetGlobalAddress(S->getGlobal(), DL, PtrVT, 0, 0);
  else if (ExternalSymbolSDNode *S = dyn_cast<ExternalSymbolSDNode>(Callee))
    Callee = DAG.getTargetExternalSymbol(S->getSymbol(), PtrVT, 0);

  // The first call operand is the chain and the second is the target address.
  SmallVector<SDValue, 8> Ops;
  Ops.push_back(Chain);
  Ops.push_back(Callee);

  // Add argument registers to the end of the list so that they are
  // known live into the call.
  for (auto &Reg : RegsToPass)
    Ops.push_back(DAG.getRegister(Reg.first, Reg.second.getValueType()));

  // Add a register mask operand representing the call-preserved registers.
  const TargetRegisterInfo *TRI = Subtarget.getRegisterInfo();
  const uint32_t *Mask = TRI->getCallPreservedMask(MF, CallConv);
  assert(Mask && "Missing call preserved mask for calling convention");
  Ops.push_back(DAG.getRegisterMask(Mask));

  // Glue the call to the argument copies, if any.
  if (Glue.getNode())
    Ops.push_back(Glue);

  // Emit the call.
  SDVTList NodeTys = DAG.getVTList(MVT::Other, MVT::Glue);
  Chain = DAG.getNode(TempleISD::CALL, DL, NodeTys, Ops);
  Glue = Chain.getValue(1);

  // Mark the end of the call, which is glued to the call itself.
  Chain = DAG.getCALLSEQ_END(Chain, DAG.getConstant(NumBytes, DL, PtrVT, true),
                             DAG.getConstant(0, DL, PtrVT, true), Glue, DL);
  Glue = Chain.getValue(1);

  // Assign locations to each value returned by this call.
  SmallVector<CCValAssign, 16> RVLocs;
  CCState RetCCInfo(CallConv, IsVarArg, MF, RVLocs, *DAG.getContext());
  RetCCInfo.AnalyzeCallResult(Ins, RetCC_Temple);

  // Copy all of the result registers out of their specified physreg.
  for (auto &VA : RVLocs) {
    // Copy the value out, gluing the copy to the end of the call sequence.
    SDValue RetValue =
        DAG.getCopyFromReg(Chain, DL, VA.getLocReg(), VA.getLocVT(), Glue);
    Chain = RetValue.getValue(1);
    Glue = RetValue.getValue(2);

    InVals.push_back(Chain.getValue(0));
  }

  return Chain;
}

SDValue
TempleTargetLowering::LowerReturn(SDValue Chain, CallingConv::ID CallConv,
                                  bool IsVarArg,
                                  const SmallVectorImpl<ISD::OutputArg> &Outs,
                                  const SmallVectorImpl<SDValue> &OutVals,
                                  const SDLoc &DL, SelectionDAG &DAG) const {
  if (IsVarArg) {
    report_fatal_error("VarArg not supported");
  }

  // Stores the assignment of the return value to a location.
  SmallVector<CCValAssign, 16> RVLocs;

  // Info about the registers and stack slot.
  CCState CCInfo(CallConv, IsVarArg, DAG.getMachineFunction(), RVLocs,
                 *DAG.getContext());

  CCInfo.AnalyzeReturn(Outs, RetCC_Temple);

  SDValue Flag;
  SmallVector<SDValue, 4> RetOps(1, Chain);

  // Copy the result values into the output registers.
  for (unsigned i = 0, e = RVLocs.size(); i < e; ++i) {
    CCValAssign &VA = RVLocs[i];
    assert(VA.isRegLoc() && "Can only return in registers!");

    Chain = DAG.getCopyToReg(Chain, DL, VA.getLocReg(), OutVals[i], Flag);

    // Guarantee that all emitted copies are stuck together.
    Flag = Chain.getValue(1);
    RetOps.push_back(DAG.getRegister(VA.getLocReg(), VA.getLocVT()));
  }

  RetOps[0] = Chain; // Update chain.

  // Add the flag if we have it.
  if (Flag.getNode()) {
    RetOps.push_back(Flag);
  }

  return DAG.getNode(TempleISD::RET, DL, MVT::Other, RetOps);
}

const char *TempleTargetLowering::getTargetNodeName(unsigned Opcode) const {
  switch ((TempleISD::NodeType)Opcode) {
  case TempleISD::FIRST_NUMBER:
    break;
  case TempleISD::CALL:
    return "TempleISD::CALL";
  case TempleISD::RET:
    return "TempleISD::RET";
  }
  return nullptr;
}

std::pair<unsigned, const TargetRegisterClass *>
TempleTargetLowering::getRegForInlineAsmConstraint(
    const TargetRegisterInfo *TRI, StringRef Constraint, MVT VT) const {
  // First, see if this is a constraint that directly corresponds to a
  // Temple register class.
  if (Constraint.size() == 1) {
    switch (Constraint[0]) {
    case 'r':
      return std::make_pair(0U, &Temple::GPRRegClass);
    default:
      break;
    }
  }

  return TargetLowering::getRegForInlineAsmConstraint(TRI, Constraint, VT);
}
