TODO: 
- rename and do obvious refactoring. 
- Use Cmp X and Y rather than Sym Pos. Beware that Cmp X and Cmp Y are ints but Sym.Pos.X and Sym.Pos.Y are floats.
- Remove Sym Id and use Cmp Id instead


# Types

## Symbol
- **Pos** field is redundant. Could use Component X,Y (Symbol top left corner)
- **InWidth0**: int option, and **InWidth1**: int option have confusing names. Only used in MergeWire and SplitWIre
- **Id** field is redundant. (Same as Component Id)
- add **APortOffsetsMap** (store XYpos of each port)

  
## Model
- **Ports** should change string to portID (protect type using DU) 
- **InputPortsConnected** and **OutputPortsConnected** are not used!
- add **OutputPortLocation** Map<OutputPortId,XYPos> and **InputPortLocation** Map<InputPortId,XYPos>


# GET BOUNDING BOXES FUNCS

TODO: remove getBoundingBoxofSymbol and only use getOneBoundingBox

**getBoundingBoxofSymbol** 
- renamed to getSymbolBoundingBox
- should use component X and Y

**getBoundingBoxes** 
- renamed to getModelBoundingBoxes

**getOneBoundingBox**
- renamed to getCmpBoundingBox


# GETTING PORTS AND THEIR LOCATIONS INTERFACE FUNCTIONS

TODO: use Component (wrapped by symbol) input and output ports!

**getSymbolPos**
- function not used anywhere. Removed

**getInputPortsPositionMap**
- slow, change when implementing rotation

**getOutputPortsPositionMap**
- slow, change when implementing rotation

**getPortLocations**
- renamed to getCmpsPortLocations
  
**getInputPortLocation**
- ok

**getOutputPortLocation**
- ok

**getOnePortLocation**
- function not used anywhere. Removed

**getOnePortLocationNew**
- renamed to getPortLocation

**getTwoPortLocations**
- renamed to getPortLocations


#  LABEL AND COPY SYMBOLS

**getCopiedSymbols**
- renamed to getCopiedSymbolsIds

**getCompList**
- change later if you have time. Only used in genCmpIndex to return the number associated 
with each component. Create new datastracture (Map) in model that contains type of each component and how many
times it is displayed in the current model

**filterString**
- removed. Equivalent to String.ToUpper

**regex**
- renamed to getCmpIndex

**getIndex** 
- renamed to genCmpIndex

**labelGenNumber**
- removed. same functionality as generateLabel

**generateLabel**
- renamed to genCmpLabel

**pasteSymbols**
- ok. TODO: Discuss this funtion with team

**getEquivalentCopiedPorts**
- ok. TODO: Discuss this funtion with team

**changeNumberOfBitsf**
- renamed to updateCmpBits
  
**changeLsbf**
- renamed to updateCmpLSBBits

**changeConstantf**
- renamed to updateConstant


# UPDATE FUNCTION
**update**
- ok

# INTERFACE TO ISSIE
- ok
  


# 2nd Refactoring

TODO:
- port locations functions. Use Model.Ports
- Use Cmp X and Y rather than Sym Pos. Beware that Cmp X and Cmp Y are ints but Sym.Pos.X and Sym.Pos.Y are floats.
- make sym pos sym center
- Remove Sym Id and use Cmp Id instead
- add STransform (Rotation) and PortOrientation.
- add PortOffsets map and helper functions to rotate ports

# GETTING PORTS AND THEIR LOCATIONS INTERFACE FUNCTIONS

**getInputPortsPositionMap**
- made subfunction of getCmpsPortLocations

**getOutputPortsPositionMap**
- made subfunction of getCmpsPortLocations

**getInputPortLocation**
- improved efficiency and speed. Rather than generating getInputPortsPositionMap it nows looks up the port from Model.Ports

**getOutputPortLocation**
- improved efficiency and speed. Rather than generating getOutputPortsPositionMap it nows looks up the port from Model.Ports

**getPortLocation**
- removed. Substituted with getInputPortLocation and getOutputPortLocation
- 
**getPortLocations**
- removed. Substituted with getInputPortLocation and getOutputPortLocation

#  LABEL AND COPY SYMBOLS

**pasteSymbols**
- refactored heavily and commented to facilitate understanding. Used new type Center to make the funciton simpler

**getEquivalentCopiedPorts**
- renamed to getPastedPortsIdsFromCopiedPortsIds and commented to facilitate understanding.

**getCompList**
- removed
  
introduced new Type **SymbolCount**

**genCompIndex**
- made subfunction of genCompLabel. Restructured to work with SymbolCount

**addSymToSymbolsCount** and **removeSymFromSymbolsCount**
- created new funcitons to deal with SymbolCount functionality

**addSymbol**
- renamed to addSymToModel