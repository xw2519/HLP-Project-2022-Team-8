# Types

## Symbol
- **Pos** field is redundant. Could use Component X,Y (Symbol top left corner)
- **InWidth0**: int option, and **InWidth1**: int option have confusing names. Only used in MergeWire and SplitWIre
- **Id** field is redundant. (Same as Component Id)

  
## Model
- **Ports** should change string to portID (protect type using DU) 
- **InputPortsConnected** and **OutputPortsConnected** are not used!


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