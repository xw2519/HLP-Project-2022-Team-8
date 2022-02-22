TODO: 
- rename and do obvious refactoring. 
- Use Cmp X and Y rather than Sym Pos
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
- ok

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
- renamed to updateCmpNumOfBits
  
**changeLsbf**
- renamed to updateLSB

**changeConstantf**
- renamed to updateConstant


# UPDATE FUNCTION
**update**
- ok

# INTERFACE TO ISSIE
- ok