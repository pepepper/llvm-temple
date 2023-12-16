add_llvm_component_library(LLVMTempleInfo
  TempleTargetInfo.cpp

  LINK_COMPONENTS
  MC
  Support

  ADD_TO_COMPONENT
  Temple
  )