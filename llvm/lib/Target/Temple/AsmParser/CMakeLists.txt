add_llvm_component_library(LLVMTempleAsmParser
  TempleAsmParser.cpp

  LINK_COMPONENTS
  MC
  MCParser
  TempleDesc
  TempleInfo
  Support

  ADD_TO_COMPONENT
  Temple
  )