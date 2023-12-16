//===-- TempleSchedule.td - Temple Scheduling Definitions ------*- tablegen
//-*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Functional units across Temple chips sets.
//===----------------------------------------------------------------------===//
def ALU : FuncUnit;

//===----------------------------------------------------------------------===//
// Instruction Itinerary classes used for Temple
//===----------------------------------------------------------------------===//
def IIAlu : InstrItinClass;
def IILoad : InstrItinClass;
def IIStore : InstrItinClass;
def IIBranch : InstrItinClass;
def IIPseudo : InstrItinClass;

//===----------------------------------------------------------------------===//
// Temple Generic instruction itineraries.
//===----------------------------------------------------------------------===//
//@ http://llvm.org/docs/doxygen/html/structllvm_1_1InstrStage.html
def TempleGenericItineraries : ProcessorItineraries<[ALU], [], [
  //@2
  InstrItinData<IIAlu, [InstrStage<1, [ALU]>]>,
  InstrItinData<IILoad, [InstrStage<2, [ALU]>]>,
  InstrItinData<IIStore, [InstrStage<2, [ALU]>]>,
  InstrItinData<IIBranch, [InstrStage<3, [ALU]>]>
]>;