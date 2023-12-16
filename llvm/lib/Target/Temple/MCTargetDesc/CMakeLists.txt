add_llvm_component_library(LLVMTempleDesc
  TempleMCTargetDesc.cpp
  TempleMCAsmInfo.cpp
  TempleAsmBackend.cpp
  TempleELFObjectWriter.cpp
  TempleMCCodeEmitter.cpp
  TempleMCExpr.cpp
  TempleInstPrinter.cpp

  LINK_COMPONENTS
  MC
  TempleInfo
  Support

  ADD_TO_COMPONENT
  Temple
  )
