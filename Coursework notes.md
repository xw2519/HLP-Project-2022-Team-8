# Changes

## Variable renaming:
- `Id` -> `ComponentId`
- `comp` -> `Comp`
- `title` -> `symbolTitle`
- `bustitle` -> `busTitle`
- `makeComp` parameters: `comptype` -> `compType`, `id` -> `compId`, `label` -> `compLabel`
- `oldColor` -> `outlineColor` 
  
### `initialiseComponent`
- `n` -> `numOfInputPorts`
- `nout` -> `numOfOutputPorts` 
- `h`
- `w`

### `getPortPosEdgeGap`
- `ct` -> `compType`
- 

## Functions renaming:
- `customToLength` -> `cutToLength`
- `generatePortList` -> `initPorts`
- `makeComp` -> `initialiseComponent` 
- `busTitle` -> `insertBusTitle`
- `title` -> `insertSymbolTitle`
- `prefix` -> `insertCompLabel` 
- `gateDecoderType` -> `insertCompTitle`
- `portDecName` -> `insertPortTitle`
- `createNewSymbol` -> `makeSymbol`
- `portText` -> `addPortName`
- `compSymbol` -> `drawSymbol`
- `getPortPosModel` -> `getModelPortPos`
- `portCircles` -> `drawPortCircles`

## Variabled modified
- Removed `pos` from `Symbol` 

  `Component` type within `Symbol` already has information about the `X` and `Y` coordinates

## Styling
- Removed unnecessary parameter brackets
- Ensured consistent space between member declarations
- Handled long parameter styling

## Refactoring

### `drawSymbol`
- Moved the section of code that generates points (`points`) to a function inside `DrawHelpers`



### Rotation functionality
- Changed function parameters to pass in symbol 
  - `drawPortText`
  - `drawPorts`