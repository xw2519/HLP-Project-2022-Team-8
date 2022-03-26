# Group 8 - Analysis Report

This document provides a description and analysis for notable algorithms used in our improvements of the ISSIE DrawBlock.

<br/>

## Autorouting

### Declaration and arguments

The auto-routing functionality is implemented inside the `makeInitialSegmentsList` that is declared as follows:

```fsharp
let makeInitialSegmentsList connId (startPort: XYPos) (endPort: XYPos) (startSymbolRotation: int) (endSymbolRotation: int) (startSymbolFlip: bool) (endSymbolFlip: bool) (startPortOnAltSide: bool) (endPortOnAltSide: bool) : Segment list
```

It takes multiple arguments of various types:
 - `connId` (ConnectionId): The Id of the wire that is being autorouted.
 - `startPort` (XYPos): The coordinates of the port at the start of the wire.
 - `endPort` (XYPos): The coordinates of the port at the end of the wire.
 - `startSymbolRotation` (integer): A value in the range [0; 360) that describes the **clockwise** rotation of the symbol at the start of the wire.
 - `endSymbolRotation` (integer): Similarly as `startSymbolRotation`, but for the symbol at the end of the wire.
 - `startSymbolFlip` (boolean): A boolean describing if the symbol at the start of the wire is flipped or not.
 - `endSymbolFlip` (boolean): Similarly as `startSymbolFlip`, but for the symbol at the end of the wire.
 - `startPortOnAltSide` (boolean): A boolean describing if the starting port is on the alternative side of the symbol at the start of the wire.
 - `endPortOnAltSide` (boolean): Similarly as `startPortOnAltSide`, but for the end port of the wire.

 All those arguments are then used to compute the characteristics and requirements of the wire's segments to be generated.

 ### Computing the port orientations

 The length of the sticks at each end of the wire is first computed using a similar way as the original Issie implementation to generate simple wires.

 Then, the rotation of the start and end ports of the wire is computed. This is based on the rotation of the Symbol that each port is on, as well as wether the Symbol is flipped, or if the ports are on the 'alternative side' of the Symbol (for a more detailed explanation of alternatives sides, please refer to [this documentation](./appendix/alt_side.md)):

 ```fsharp
let startPortRotation = 
        match startPortOnAltSide, startSymbolFlip with
        // If the startPort is on the alternative side then it means that 
        // it is on the side to the right of where it would've been normally (so an additional rotation of -90)
        // We use a wildcard here because if a port is at the top of a symbol that points to the left (for example),
        // it is not sensitive to flips
        | true, _       -> makeInRangeRotation (startSymbolRotation - 90)
        // Normal orientation
        | false, false  -> startSymbolRotation
        // If the symbol is flipped, the ports are pointing backwards
        | false, true   -> makeInRangeRotation (startSymbolRotation + 180)
```

The implementation for the start port is given above and works as follows: it is a match statement on a 2-tuple of booleans. The first on is if the start port is on the alternative side or not, and the second one is if the start symbol is flipped of not.
If a symbol is on the alternative side, it will not be affected by flips as flips only occur along the axis of orientation of the component, so we can use a wild card for the second boolean and compute its rotation from the rotation of the symbol: `startSymbolRotation - 90` because it is on the next counter-clockwise side.
If on the other hand, the port is on its regular side, then the second boolean tells us if the rotation is shifted by `180` to point backwards in case of a flipped symbol. *It is important to note here that the rotation of a Symbol and whether or not it is flipped are two parameters that are handle independantly.*

The computation for `endPortRotation` is very similar, except an offset of `180` is always applied to the rotation of the end Symbol as input ports are always pointing opposite to the orientation of the Symbol.

Both computation rely on a helper function called `makeInRangeRotation` that takes the modulo of the input value by 360 and shifts it to always be in the positive range (the F# modulo operator returns a value of the same sign as the input value, which we don't want).


### Normalizing the requirements for the wire to be generated

Given the rotation of the ports, the overall rotation of the wire is set to be the same as the rotation of the start port:
```fsharp 
    // Overall rotation of the wire
    let wireRotation = startPortRotation
```

This then used in a match statement to normalize the distances over both the X and Y axis to be covered by the wire:

```fsharp
    // Get the real/actual difference between the ports over the X and Y axis
    let differenceInX, differenceInY = (endPort.X - startPort.X), (endPort.Y - startPort.Y) 

    // Get the NORMALIZED differences between the X and Y coordinates of the ports
    // i.e. assuming the Input port points to the right, towards the positive X
    let diffX, diffY =
        match wireRotation with
        | 0     -> differenceInX, differenceInY
        | 90    -> differenceInY, - differenceInX
        | 180   -> - differenceInX, - differenceInY
        | 270   -> - differenceInY, differenceInX
        // edge case: should not happen
        | _     -> differenceInY, differenceInX
```

This computation essentially shifts the coordinate system so that the start port of the wire is always rotated towards the positive X.  

The end port's rotation can then also be normalized to fit into this new coordinate system:

```fsharp
    // Get the NORMALIZED rotation of the output port
    // i.e. assuming the Input port points to the right, towards the positive X
    let normalizedEndPortRotation = makeInRangeRotation (endPortRotation - wireRotation)
```

### Generating the list of segments lengths

After normalization, all possible normalized wire configuration can be broken down into 12 simple cases that are matched on a 3-tuple of parameters:

```fsharp
    /// Generate a list of segment lengths for the wire to go from the normalized startPort to the normalized endPort
    let generatelengthList (normalizedEndPortRotation: int) (s: float) (diffX: float) (diffY: float) : float list = 
        match (normalizedEndPortRotation, (diffX >= 0.0), (diffY >= 0.0)) with
        // Same orientation
        | 0, true, _        -> [s; 0.0; diffX; diffY; 0.0; 0.0; -s]
        | 0, false, _       -> [s; 0.0; 0.0; diffY; diffX; 0.0; -s]
        // Opposite orientation
        | 180, true, _      -> [s; 0.0; (diffX - 2.0 * s)/2.0; diffY; (diffX - 2.0 * s)/2.0; 0.0; s]
        | 180, false, _     -> [s; diffY/2.0; (diffX - 2.0 * s); diffY/2.0; 0.0; 0.0; s]
        // Perpendicular orientation: if startPort points to the right, endPort points down
        | 90, true, true    -> [s; 0.0; (diffX - s)/2.0; (diffY + s); (diffX - s)/2.0; 0.0; 0.0; -s]
        | 90, true, false   -> [s; 0.0; (diffX - s); (diffY + s); 0.0; 0.0; 0.0; -s]
        | 90, false, true   -> [s; 0.0; 0.0; (diffY + s); (diffX - s); 0.0; 0.0; -s]
        | 90, false, false  -> [s; 0.0; 0.0; (diffY+s)/2.0; (diffX-s); (diffY+s)/2.0; 0.0; -s]
        // Perpendicular orientation: if startPort points to the right, endPort points up
        | 270, true, true   -> [s; 0.0; (diffX - s); (diffY - s); 0.0; 0.0; 0.0; s]
        | 270, true, false  -> [s; 0.0; (diffX - s)/2.0; (diffY - s); (diffX - s)/2.0; 0.0; 0.0; s]
        | 270, false, true  -> [s; 0.0; 0.0; (diffY - s)/2.0; (diffX - s); (diffY - s)/2.0; 0.0; s]
        | 270, false, false -> [s; 0.0; 0.0; (diffY - s); (diffX - s); 0.0; 0.0; s]
        // Edge case that should never happen
        | _                 -> [s; 0.0; 0.0; 0.0; 0.0; 0.0; s]


    // Generate the list of segments length
    let lengthList = generatelengthList normalizedEndPortRotation s diffX diffY
```

This function seperates all possible combinations/arrangements of wires into several categories based on the normalized rotation of the end port, and whether the normalized distances to cover are negative or positive over both the X and Y axis. It then returns a simple length list that describes a basic wire shape to fulfill the requirements of the wire (i.e. join the two ports together).

### Converting the list of segments lengths back into a regular Segment list

The generated list of lengths of segments is then mapped into a list of `RotationInvariantSeg` before being converted back into a regular `Segment list` as required by the function signature:

```fsharp
    lengthList
    // Map the generated segment lengths to a list of RISegs
    |> List.mapi buildRiSegListFromLengths
    // Convert those RISegs back into a regular Segment list
    |> convertRISegsToSegments connId startPort wireRotation
```

Those two convertions rely respectively on the `buildRiSegFromLength` function, that maps a length into a `RotationInvariantSeg`, and the `convertRISegsToSegments` general helper function, that is explained in [this document](./appendix/helper_convertRISegsToSegments.md).

<br/>

## Segment stickiness

abc

```fsharp
code_block
```

<br/>

## Placing of ports 

abc

```fsharp
code_block
```

<br/>