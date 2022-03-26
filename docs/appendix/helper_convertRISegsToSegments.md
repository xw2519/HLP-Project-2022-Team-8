 ## Helper function: `convertRISegsToSegments`

The helper function converts a list of `RotationInvariantSeg` to a list of `Segment` (i.e. absolute segments that rely on XYPos coordinates)

### Declaration and arguments

It is declared as follows:

```fsharp
/// Converts a list of RI segments to regular segments
let convertRISegsToSegments (hostId: ConnectionId) (startRotation: XYPos) (startDir: int) (riSegs: RotationInvariantSeg list) : Segment list =
```

And takes in as arguments:
 - `hostId` (ConnectionId): The ID of the Wire that will host the generated segment list.
 - `startPos` (XYPos): The starting coordinate of the Segment list to be generated.
 - `startRotation` (int): The starting rotation of the `RotationInvariantSeg` list. 
 - `riSegs` (RotationInvariantSeg list): The input list

### Initial state

The function relies on a folder operation to convert the RISegs into regular segments. This is needed because being given only a list of RISegs with a given lengths, the function needs to keep track of some state: where the last Segment ended. We therefore intialize the initial state as follows:

```fsharp
    let firstSeg:Segment =
        {
            Id= SegmentId(JSHelpers.uuid())
            Index = -1
            Start = startPos
            Vector = {X=0.0;Y=0.0}
            HostId = hostId
            JumpCoordinateList = []
            Autorouted = true
            Draggable = false
        }
```

This `firstSeg` is constructed from the starting position of the wire, so that this information can be passed to the folder function. It will then be discarded once the computation of the `Segment` list is completed.

### Folder function

The folder function is declared as follows:

```fsharp
    // Folder used to convert a RI segment into a regular Segment
    let convertToSeg (oldState: Segment) (element: RotationInvariantSeg) : Segment
```

At each fold, it takes in the previous `Segment` computed and the new element to be converted, and returns the latter converted into a `Segment`.

Before assembling the new `Segment` it first needs to compute its parameters, some of which rely on the rotation of the wire. We will go through them in order:

1. The index of the new `Segment` is simply the index of the previous `Segment` incremented by 1:

```fsharp
        // Current index of the segment
        let index = oldState.Index + 1
```

2. The start of the new `Segment` is the end of the previous one, i.e. the `Vector` of the previous one added onto its `Start`:

```fsharp
        // Compute the new start of the segment based on the old start + old vector
        let newStart = addPositions oldState.Start oldState.Vector
```

3. Based on the starting rotation of the wire, we can determine if the `Vector` of the next `Segment` is going to be along the X or Y axis. A wire that starts horizontally will have all segments with even indices going along the X axis, and all those with odd indeces going along the Y axis. This is the opposite for a wire that starts vertically.

```fsharp
        // Define the new vector based on the wire start orientation and the current index
        let newBaseVector = match startRotation with
                            | 0 | 180 when ((index % 2) = 0)    -> {X=element.Length;Y=0.0}
                            | 0 | 180                           -> {X=0.0;Y=element.Length}
                            | 90 | 270 when ((index % 2) = 0)   -> {X=0.0;Y=element.Length}
                            | 90 | 270                          -> {X=element.Length;Y=0.0}
                            | _ -> {X = 1 ; Y = 1}
```

4. Then, the sign of the value of this `baseVector` needs to be refined, once again based on the starting rotation of the wire. This last adjustment step for the `Vector` basically shifts the RISegs back into the real coordinate system:

```fsharp
        // Adjust the values of the vectors based on the wire start orientation and current index:
        // Invert the vector when at the right index
        let newOrientedVector = match startRotation with
                                | 0     -> newBaseVector
                                | 90    -> {newBaseVector with X = - newBaseVector.X}
                                | 180   -> {X = - newBaseVector.X ; Y = - newBaseVector.Y }
                                | 270   -> {newBaseVector with Y = - newBaseVector.Y }
                                | _     -> newBaseVector
```

5. Whether or not the new `Segment` is draggable simple depends on if it is on either end of the Wire (i.e. if it is a "stick"):

```fsharp
        // Define if the new segment is draggable or not
        let newDraggable = if(oldState.Index + 1 = 0) || (oldState.Index + 1 = riSegs.Length - 1)
                           then false
                           else true 
```

6. Finally, with all those parameters computed for the new `Segment`, it can finally be assembled and returned by the folder:

```fsharp
        //Assemble the new Segment
        {
            Id = element.Id
            Index = oldState.Index + 1
            Start = newStart
            Vector = newOrientedVector
            HostId = hostId
            JumpCoordinateList = element.JumpCoordinateList
            Autorouted = true
            Draggable = newDraggable
        }
```


### Application of the folder function

The folder function is called as follows using `List.scan` on the initial state and the input list of RISegs:

```fsharp
    // Apply the folder to the list, and keep track of the changes
    let (segmentList: Segment list) = (firstSeg, riSegs) ||> List.scan convertToSeg
```

This is because the folder only computes the next Segment of the list, based on the previous one. `List.scan` therefore allows us to keep a history of all the states computed, i.e. all the `Segments` computed.

Because `List.scan` also keeps the intial state in its output, we need to remove it from the head of the list as it was temporary and only created by us for the computation of the other `Segments`:

```fsharp
    // Return all but the head of the list
    match segmentList with
    | hd::tl -> tl
    | _ -> []
```
