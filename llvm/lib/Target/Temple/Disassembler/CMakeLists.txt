add_llvm_component_library(LLVMTempleDisassembler
  TempleDisassembler.cpp

  LINK_COMPONENTS
  MCDisassembler
  TempleInfo
  Support

  ADD_TO_COMPONENT
  Temple
  )