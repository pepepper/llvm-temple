add_llvm_component_group(Temple)

set(LLVM_TARGET_DEFINITIONS Temple.td)

tablegen(LLVM TempleGenAsmMatcher.inc -gen-asm-matcher)
tablegen(LLVM TempleGenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM TempleGenCompressInstEmitter.inc -gen-compress-inst-emitter)
tablegen(LLVM TempleGenCallingConv.inc -gen-callingconv)
tablegen(LLVM TempleGenDAGISel.inc -gen-dag-isel)
tablegen(LLVM TempleGenDisassemblerTables.inc -gen-disassembler)
tablegen(LLVM TempleGenInstrInfo.inc -gen-instr-info)
tablegen(LLVM TempleGenMCCodeEmitter.inc -gen-emitter)
tablegen(LLVM TempleGenMCPseudoLowering.inc -gen-pseudo-lowering)
tablegen(LLVM TempleGenRegisterInfo.inc -gen-register-info)
tablegen(LLVM TempleGenSubtargetInfo.inc -gen-subtarget)

add_public_tablegen_target(TempleCommonTableGen)

add_llvm_target(
 TempleCodeGen
  TempleAsmPrinter.cpp
  TempleFrameLowering.cpp
  TempleISelDAGToDAG.cpp
  TempleISelLowering.cpp
  TempleInstrInfo.cpp
  TempleMCInstLower.cpp
  TempleRegisterInfo.cpp
  TempleSubtarget.cpp
  TempleTargetMachine.cpp

 LINK_COMPONENTS
 Analysis
 AsmPrinter
 CodeGen
 Core
 MC
 TempleDesc
 TempleInfo
 SelectionDAG
 Support
 Target
 GlobalISel

 ADD_TO_COMPONENT
 Temple
)

add_subdirectory(AsmParser)
add_subdirectory(Disassembler)
add_subdirectory(MCTargetDesc)
add_subdirectory(TargetInfo)
