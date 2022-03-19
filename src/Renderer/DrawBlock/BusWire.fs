(*
This module implements wires between symbol ports. Wires can be autorouted, or manually routed by dragging segments.
Moving symbols causes the corresponding wires to move.
Wires are read and written from Issie as lists of wire vertices, whatever teh internal representation is.
*)


module BusWire

open CommonTypes
open Fable.React
open Fable.React.Props
open Elmish
open DrawHelpers

//Static Vars
let minSegLen = 5.

/// Threshold to determine if a segment is aligned with a stick, i.e. on the same "level" as a stick.
/// (The bigger, the more forgiving it is.)
/// Used to enforce a safe distance between a segment and a port.
let onStickAxisThreshold : float = 2.0

/// Threshold to determine if a segment is aligned with another segment, i.e. on the same "level" as the other segment.
/// (The bigger, the more forgiving it is.)
/// Used to determine if two opposite segments are close enough and should cancel each other.
let onRedundantSegmentAxisThreshold : float = 5.0

/// Threshold to determine if a segment is aligned with another segment, i.e. on the same "level" as the other segment.
/// (The bigger, the more forgiving it is.)
/// Used to snap/stick two segments that are on the same level together.
let stickynessThreshold : float = 5.0


//------------------------------------------------------------------------//
//------------------------------BusWire Types-----------------------------//
//------------------------------------------------------------------------//

///Potential orientations for a segment
type Orientation =  Horizontal | Vertical | Point

///
type SnapPosition = High | Mid | Low

type InOut = Input | Output

///Include Labels for different segemnt positions
type RenderSegmentType = First | Second | Third | Middle | Antepenultimate | Penultimate | Last

///Three display modes that are changed with ctrlM in issie
type Modes = OldFashionedCircuit | ModernCircuit | Radiussed

///DU to Define potential wire directions
type WireDirection = LeftToRight | RightToLeft | RightToLeftHorizontal

///Standard ASeg Type with use of vector for direction adn magnitude
type Segment = 
    {
        Id : SegmentId
        Index: int
        Start: XYPos
        Vector: XYPos
        HostId: ConnectionId
        /// List of x-coordinate values of segment jumps. Only used on horizontal segments.
        JumpCoordinateList: list<float>
        Autorouted : bool
        Draggable : bool
    }

///RISeg implementation with only floats
type RotationInvariantSeg = 
    {
        Id : SegmentId
        Length: float
        HostId: ConnectionId
        /// List of x-coordinate values of segment jumps. Only used on horizontal segments.
        JumpCoordinateList: float list
        Draggable : bool
        Autorouted : bool
    }


///Standard AWire containing a list of ASegs
type Wire =
    {
        Id: ConnectionId 
        InputPort: InputPortId
        OutputPort: OutputPortId
        Color: HighLightColor
        Width: int
        Segments: list<Segment>
    }

    with static member stickLength = 16.0
    
///Rotation Invariant Wire containing a list of RISegs
type RotationInvariantWire =
     {
         Id: ConnectionId 
         InputPort: InputPortId
         OutputPort: OutputPortId
         Start: XYPos
         StartDir: int
         Color: HighLightColor
         Width: int
         Segments: list<RotationInvariantSeg>
     }
    with static member stickLength = 16.0
    


///
type Model =
    {
        Symbol: Symbol.Model
        WX: Map<ConnectionId, Wire>
        FromVerticalToHorizontalSegmentIntersections: Map<SegmentId, list<ConnectionId*SegmentId>>
        FromHorizontalToVerticalSegmentIntersections: Map<SegmentId, list<ConnectionId*SegmentId>>
        CopiedWX: Map<ConnectionId, Wire> 
        SelectedSegment: SegmentId
        LastMousePos: XYPos
        ErrorWires: list<ConnectionId>
        Notifications: Option<string>
        Mode: Modes
        SplitWireList: XYPos List
    }

//----------------------------Message Type-----------------------------------//

///
type Msg =
    | Symbol of Symbol.Msg
    | AddWire of (InputPortId * OutputPortId)
    | BusWidths
    | CopyWires of list<ConnectionId>
    | DeleteWires of list<ConnectionId>
    | SelectWires of list<ConnectionId>
    | UpdateWires of list<ComponentId> * XYPos
    | DragWire of ConnectionId * MouseT
    | ColorWires of list<ConnectionId> * HighLightColor
    | ErrorWires of list<ConnectionId>
    | ResetJumps of list<ConnectionId>
    | MakeJumps of list<ConnectionId>
    | ResetModel // For Issie Integration
    | LoadConnections of list<Connection>
    | ChangeMode // For Issie Integration
    | ReRouteSymbol of list<ComponentId>

/// Adds two XYPos together
let addPositions (a: XYPos) (b: XYPos) : XYPos =
    {X = a.X + b.X ; Y = a.Y + b.Y}

/// Returns the XYPos of the end of a Segment
let getEndPoint (segment: Segment) : XYPos =
    {X=(segment.Start.X + segment.Vector.X) ; Y=(segment.Start.Y + segment.Vector.Y)}

/// Gets the orientation of a Segment
let getOrientation (segment : Segment) : Orientation =
    if ((abs segment.Vector.Y) < 0.001) && ((abs segment.Vector.X) < 0.001) then Point 
    elif ((abs segment.Vector.Y) < 0.001) then Horizontal
    else Vertical

/// Returns the length of a Segment (can be negative).
/// For positive-only lengths, use 'getAbsLength'
let getLength (seg: Segment) : float =
    match getOrientation seg with
    | Horizontal -> seg.Vector.X
    | Vertical   -> seg.Vector.Y
    | Point      -> 0.0

/// Returns the absolute length of a Segment (always positive).
/// To allow for negative lengths, use 'getLength'
let getAbsLength (seg: Segment) : float =
    match getOrientation seg with
    | Horizontal -> abs seg.Vector.X
    | Vertical   -> abs seg.Vector.Y
    | Point      -> 0.0

/// Converts a list of RI segments to regular segments
let convertRISegsToSegments (hostId: ConnectionId) (startPos: XYPos) (startDir: int) (riSegs: RotationInvariantSeg list) : Segment list =
    
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

    // Folder used to convert a RI segment into a regular Segment
    let convertToSeg (oldState: Segment) (element: RotationInvariantSeg) : Segment  =
        
        // Current index of the segment
        let index = oldState.Index + 1

        // Compute the new start of the segment based on the old start + old vector
        let newStart = addPositions oldState.Start oldState.Vector

        // Define the new vector based on the wire start orientation and the current index
        let newBaseVector = match startDir with
                            | 0 | 180 when ((index % 2) = 0)    -> {X=element.Length;Y=0.0}
                            | 0 | 180                           -> {X=0.0;Y=element.Length}
                            | 90 | 270 when ((index % 2) = 0)   -> {X=0.0;Y=element.Length}
                            | 90 | 270                          -> {X=element.Length;Y=0.0}
                            | _ -> {X = 1 ; Y = 1} 

        // Adjust the values of the vectors based on the wire start orientation and current index:
        // Invert the vector when at the right index
        let newOrientedVector = match startDir with         // works
                                | 0     -> newBaseVector
                                | 90    -> {newBaseVector with X = - newBaseVector.X}
                                | 180   -> {X = - newBaseVector.X ; Y = - newBaseVector.Y }
                                | 270   -> {newBaseVector with Y = - newBaseVector.Y }
                                | _     -> newBaseVector
        
        // Define if the new segment is draggable or not
        let newDraggable = if(oldState.Index + 1 = 0) || (oldState.Index + 1 = riSegs.Length - 1)
                           then false
                           else true 
        
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
        
    // Apply the folder to the list, and keep track of the changes
    let (segmentList: Segment list) = (firstSeg, riSegs) ||> List.scan convertToSeg
    // Return all but the head of the list
    match segmentList with
    | hd::tl -> tl
    | _ -> []


/// Converts a RotationInvariant Wire into a regular Wire
let convertToWire (riWire: RotationInvariantWire) : Wire =
    // Convert the RiSeg list to a regular Segment list
    let (segmentList: Segment list) = convertRISegsToSegments riWire.Id riWire.Start riWire.StartDir riWire.Segments
    // Assemble the new Wire
    {
        Id= riWire.Id
        InputPort = riWire.InputPort
        OutputPort= riWire.OutputPort
        Color= riWire.Color
        Width= riWire.Width
        Segments= segmentList
    }



//-------------------------Debugging functions---------------------------------//


let ppSId (sId:SegmentId) =
    sId
    |> (fun (SegmentId x) -> x)
    |> Seq.toList
    |> (fun chars -> chars[0..2])
    |> List.map string
    |> String.concat ""

let ppS (seg:Segment) =
    sprintf $"|{seg.Index}:{ppSId seg.Id}|"

let ppWId (wId:ConnectionId) =
        wId
        |> (fun (ConnectionId x) -> x)
        |> Seq.toList
        |> (fun chars -> chars[0..2])
        |> List.map string
        |> String.concat ""

let ppMaps (model:Model) =
    let mhv = model.FromHorizontalToVerticalSegmentIntersections
    let mvh = model.FromVerticalToHorizontalSegmentIntersections
    let m1 =
        mhv
        |> Map.toList
        |> List.map (fun (sid,lst) ->
            List.map (snd >> ppSId) lst
            |> (fun segs -> sprintf $"""<{ppSId sid}->[{String.concat ";" segs}]>"""))
            |> String.concat ";\n"
    let m2 =
        mvh
        |> Map.toList
        |> List.map (fun (sid,lst) ->
            List.map (snd >> ppSId) lst
            |> (fun segs -> sprintf $"""<{ppSId sid}->[{String.concat ";" segs}]>"""))
            |> String.concat ";\n"
    let jumps =
        model.WX
        |> Map.toList
        |> List.map (fun (wId,w) ->
            sprintf $"Wire: {w.Segments |> List.collect (fun seg -> seg.JumpCoordinateList |> List.map (fun (f) -> f))}")
            
    printfn $"\n------------------\nMapHV:\n {m1} \n MapVH\n{m2} \nJumps:\n {jumps}\n------------------\n"



let ppSeg seg (model: Model) = 
        let cid,sid = seg
        let wire = model.WX[cid]
        let sg = List.find (fun (s:Segment) -> s.Id = sid ) wire.Segments
        let pxy (xy: XYPos) = sprintf $"{(xy.X,xy.Y)}"
        sprintf $"""[{ppSId sg.Id}: {pxy sg.Start}->{pxy (getEndPoint sg)}]-{match (getOrientation sg) with | Vertical -> "V" | _ -> "H"}-{sg.Index}"""

let pp segs (model: Model)= 
    segs
    |> List.map  ( fun seg ->
        let cid,sid = seg
        let wire = model.WX[cid]
        match List.tryFind (fun (s:Segment) -> s.Id = sid ) wire.Segments with
        | Some  sg ->
            let pxy (xy: XYPos) = sprintf $"{(xy.X,xy.Y)}"
            sprintf $"""[{pxy sg.Start}->{pxy (getEndPoint sg)}]-{match (getOrientation sg) with | Vertical -> "V" | _ -> "H"}-{sg.Index}"""
        | None -> "XX")
    |> String.concat ";"



//-------------------------------------Implementation code----------------------------------//



let makeInitialSegmentsList connId (startPort: XYPos) (endPort: XYPos) (startSymbolRotation: int) (endSymbolRotation: int) (startSymbolFlip: bool) (endSymbolFlip: bool) (endPortOnAltSide: bool): Segment list =

    // adjust length of the first and last segments - the sticks - so that when two ports are aligned and close you still get left-to-right routing.
    let s = 
        let d = List.max [ abs (startPort.X - endPort.X) ; abs (startPort.Y - endPort.Y) ; Wire.stickLength / 4.0 ]
        if (endPort.X - startPort.X > 0.0) then
            min d (Wire.stickLength / 2.0)
        else
            Wire.stickLength / 2.0
    
    /// Given an int, gets its modulo by 360, returning the positive remainder, so that it is in the range [0;360)
    let makeInRangeRotation rotation = 
        // modulo returns the remainder, but it is of the same sign as the first operand,
        // so we need to care to adjust it, so that it is positive, as required for a Rotation
        match (rotation % 360) with
        | x when (x < 0) -> x + 360
        | x              -> x

    // Rotation of the ports
    let startPortRotation = 
        match startSymbolFlip with
        | false -> startSymbolRotation
        // If the symbol is flipped, the ports are pointing backwards
        | true  -> makeInRangeRotation (startSymbolRotation + 180)

    let endPortRotation =
        match endSymbolFlip, endPortOnAltSide with
        // If the endPort is on the alternative side (used for example in MUX2) then it means that 
        // it is on the side to the right of where it would've been normally (so an additional rotation of -90)
        // We use a wildcard here because if a port is at the bottom of a symbol that points to the left (for example),
        // it is not sensitive to flips
        | _, true      -> makeInRangeRotation (endSymbolRotation - 180 - 90)
        // Without being flipped, the input ports point in the opposite direction as the symbol they are on
        | false, _ -> makeInRangeRotation (endSymbolRotation - 180)
        // If the symbol is flipped, the ports are pointing backwards,
        // but because we know that the input ports are already pointing opposite to the symbol they are on,
        // it counteracts/negates itself (endSymbolRotation - 180 + 180)
        | true, _  -> endSymbolRotation

    // Overall rotation of the wire
    let wireRotation = startPortRotation

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
        | _     -> differenceInY, differenceInX

    // Get the NORMALIZED rotation of the output port
    // i.e. assuming the Input port points to the right, towards the positive X
    let normalizedEndPortRotation = 
        match ((endPortRotation - wireRotation) % 360) with
        | x when (x < 0) -> x + 360
        | x              -> x

    /// Clarke's message:
    /// This function, if turned instead into a value, causes a FABLE compiler problem that crashes wepback
    /// However, it should probably in any case be a subfunction with paras diffX, diffY, s, taken out of this function
    /// so if done the way you would normally do it, it will also not crash.
    /// It should be reported to FABLE as a compiler bug - but it is not an HLP student's job to do that
    /// PS this match statement is very bad code. You should match on a tuple and not have the when clauses.
    /// Matching on a tuple: normalizedEndPortRotation, diffX >= 0, diffY >= 0, you will I'm sure not get the error
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

    let lengthList = generatelengthList normalizedEndPortRotation s diffX diffY

    let lastIndex = (lengthList.Length - 1)

    let buildRiSegListFromLengths (index:int) (length:float) : RotationInvariantSeg = {
        Id = SegmentId(JSHelpers.uuid())
        Length= length
        HostId  = connId;
        JumpCoordinateList = [];
        Draggable = 
            if (index = 0 || index = lastIndex) then false else true
        Autorouted = true
    }

    let RISegs = lengthList |> List.mapi buildRiSegListFromLengths

    RISegs |> convertRISegsToSegments connId startPort wireRotation


 
// ----------------------------------------------------------------------------------------------------------------------------------



/// Convert a (possibly legacy) issie Connection stored as a list of vertices to Wire
let issieVerticesToSegments (connId) (verticesList: list<float*float>) =
    let WireVertices =
        verticesList
        |> List.map (fun (x,y) -> {X=x;Y=y})
    makeInitialSegmentsList connId WireVertices[0] WireVertices[WireVertices.Length - 1] 0 0 false false false
 
    
//----------------------interface to Issie-----------------------//
/// This function is given a ConnectionId and it
/// converts the corresponding BusWire.Wire type to a
/// Connection type, offering an interface
/// between our implementation and Issie.
let extractConnection (wModel : Model) (cId : ConnectionId) : Connection =
    let conn = wModel.WX[cId]
    let ConnectionId strId, InputPortId strInputPort, OutputPortId strOutputPort = conn.Id, conn.InputPort, conn.OutputPort
    {
        Id = strId
        Source = { Symbol.getPort wModel.Symbol strOutputPort with PortNumber = None } // None for connections 
        Target = { Symbol.getPort wModel.Symbol strInputPort with PortNumber = None } // None for connections 
        Vertices = ( [conn.Segments[0].Start.X,conn.Segments[0].Start.Y] @ List.mapi (fun i seg -> ((getEndPoint seg).X,(getEndPoint seg).Y)) conn.Segments)
    }

/// This function is given a list of ConnectionId and it
/// converts the corresponding BusWire.Wire(s) to a
/// list of Connectio, offering an interface
/// between our implementation and Issie.
let extractConnections (wModel : Model) : list<Connection> = 
    wModel.WX
    |> Map.toList
    |> List.map (fun (key, _) -> extractConnection wModel key)

///Returns the abs of an XYPos object
let getAbsXY (pos : XYPos) = 
    {X = abs pos.X; Y = abs pos.Y}

///This function determines if a point is in a range in order to determine if a line is overlapping another
let isPointwithinRange point lowRange highRange : bool =
    (lowRange<=point)&&(point<=highRange)
///This function determines if either the low or high range of a line is in a range of another line to determine segment intersections
let isEitherwithinRange pointA pointB lowRange2 highRange2 : bool = 
    (isPointwithinRange pointA lowRange2 highRange2)||(isPointwithinRange pointB lowRange2 highRange2)
///This function returns a direction based on a start and end point
let getDirectionfromPoints (startPos :XYPos) (endPos :XYPos) : Orientation = 
    if ((endPos.X = startPos.X)&&(endPos.Y <> startPos.Y)) then Vertical
    elif ((endPos.Y = startPos.Y)&&(endPos.X <> startPos.X)) then Horizontal
    elif ((endPos.Y = startPos.Y)&&(endPos.X = startPos.X)) then Point
    else failwithf "Unexpected area of code"

  
/// Given two sets of two points: (p1, q1) and (p2, q2)
/// that define two segments, the function returns true
/// if these two segments intersect and false otherwise.
let segmentIntersectsSegment ((seg1Start, seg1End) : (XYPos * XYPos)) ((seg2Start, seg2End) : (XYPos * XYPos)) : bool =
    
    //get direction of both segs
    let Seg1Direction = getDirectionfromPoints seg1Start seg1End
    let Seg2Direction = getDirectionfromPoints seg2Start seg2End

    //store all minimums and maximums of points in variables for easy use later
    let MinSeg1X = min seg1End.X seg1Start.X
    let MinSeg1Y = min seg1End.Y seg1Start.Y
    let MaxSeg1X = max seg1End.X seg1Start.X
    let MaxSeg1Y = max seg1End.Y seg1Start.Y

    let MinSeg2X = min seg2End.X seg2Start.X
    let MinSeg2Y = min seg2End.Y seg2Start.Y
    let MaxSeg2X = max seg2End.X seg2Start.X
    let MaxSeg2Y = max seg2End.Y seg2Start.Y

    //match all possible intersection cases seg1 can be Horizontal, vertical or a point and so can seg2, then check ranges and see if they intersect
    match Seg1Direction with 
                            | Horizontal -> match Seg2Direction with 
                                                                   | Horizontal -> if ((isEitherwithinRange MinSeg1X MaxSeg1X MinSeg2X MaxSeg2X)&&(seg1Start.Y=seg2Start.Y)) then true else false
                                                                   | Vertical -> if ((isEitherwithinRange MinSeg2X MaxSeg2X MinSeg1X MaxSeg1X)&&(isEitherwithinRange MinSeg1Y MaxSeg1Y MinSeg2Y MaxSeg2Y)) then true else false
                                                                   | Point -> if((isPointwithinRange MaxSeg2X MinSeg1X MaxSeg1X)&&(MaxSeg2Y=MaxSeg1Y)) then true else false
                            | Vertical -> match Seg2Direction with 
                                                                   | Horizontal -> if((isEitherwithinRange MinSeg1X MaxSeg1X MinSeg2X MaxSeg2X)&&(isEitherwithinRange MinSeg2Y MaxSeg2Y MinSeg1Y MaxSeg1Y)) then true else false
                                                                   | Vertical ->  if ((isEitherwithinRange MinSeg1Y MaxSeg1Y MinSeg2Y MaxSeg2Y)&&(seg1Start.X=seg2Start.X)) then true else false
                                                                   | Point -> if ((isPointwithinRange MaxSeg2Y MinSeg1Y MaxSeg1Y)&&(MaxSeg2X=MaxSeg1X)) then true else false
                            | Point -> match Seg2Direction with 
                                                                   | Horizontal -> if((isPointwithinRange MaxSeg1X MinSeg2X MaxSeg2X)&&(MaxSeg2Y=MaxSeg1Y)) then true else false
                                                                   | Vertical -> if((isPointwithinRange MaxSeg1Y MinSeg2Y MaxSeg2Y)&&(MaxSeg2X=MaxSeg1X)) then true else false
                                                                   | Point ->  if((MaxSeg2Y=MaxSeg1Y)&&(MaxSeg2X=MaxSeg1X)) then true else false

///Returns a segment with positive Start and End coordinates
let makeSegPos (seg : Segment) =
    {seg with
        Start = getAbsXY seg.Start }


/// This function renders the given
/// wire in Radiussed wire mode,
/// using the colour and width properties given.
let renderRadiusedWire (segmentList: Segment List) (colour : string) (width : string) (numofSegments:int) : ReactElement List = 
    //render segement inside of render wire in order to have access to next and previous segments
    let renderSegment (segment : Segment) (colour : string) (width : string) (index:int) (numofSegments:int) : ReactElement =
        //match all the possible positions of the segments
        let SegPosition = match index with  | 0 -> First
                                            | 1 -> Second
                                            | 2 -> Third
                                            | 4 -> Antepenultimate
                                            | 5  -> Penultimate
                                            | 6 -> Last
                                            | _ -> Middle
        //guard for next segment if current segemnt is last                                      
        let nextSegment = if(index <> (segmentList.Length-1))
                          then segmentList[index+1]
                          else segmentList[index]
        //guard for one after next segment if current segemnt is one before last
        let nextSegment' = if(index <> (segmentList.Length-1)&&index <> (segmentList.Length-2))
                           then segmentList[index+2]
                           else nextSegment
        //guard for previous segment if current segment is first 
        let prevSegment = if(index <> 0)
                          then segmentList[index-1]
                          else segmentList[index]
        //guard for one before previous segment if current segment is one after first 
        let prevSegment' = if((index <> 0)&&(index <> 1))
                           then segmentList[index-2]
                           else prevSegment

        let wOpt = EEExtensions.String.tryParseWith System.Int32.TryParse width
        let renderWidth = 
            match wOpt with
            | Some 1 -> 1.5
            | Some n when n < int "8" -> 2.5
            | _ -> 3.5

        let lineParameters = { defaultLine with Stroke = colour; StrokeWidth = string renderWidth }

        let radiusSize = 12.0
        //get seg directions of current, previous and next segment
        let SegDirection = getOrientation segment
        let NextSegDirection = getOrientation nextSegment
        let PrevSegDirection = getOrientation prevSegment

        //if the next or previous segments are points, skip them and calculate the radius off of the next segment with length
        let nextSegmentSkipPoint = if (NextSegDirection=Point) then nextSegment' else nextSegment
        let prevSegmentSkipPoint = if (PrevSegDirection=Point) then prevSegment' else prevSegment

        //Calculate the next and previous radius for curved wires based upon the current, previous, next segments and constant radius 
        let SegStartX, SegStartY, SegEndX, SegEndY = segment.Start.X, segment.Start.Y, (getEndPoint segment).X, (getEndPoint segment).Y
        let prevSegCaluRadius = (min (min radiusSize ((abs ((SegEndX-SegStartX)+(SegEndY-SegStartY)))/2.0) ) ((abs (prevSegmentSkipPoint.Vector.X + prevSegmentSkipPoint.Vector.Y))/2.0))
        let nextSegCaluRadius = min (min radiusSize ((abs ((SegEndX-SegStartX)+(SegEndY-SegStartY)))/2.0) ) ((abs (nextSegmentSkipPoint.Vector.X + nextSegmentSkipPoint.Vector.Y))/2.0)
        let pathParameters = { defaultPath with Stroke = colour; StrokeWidth = string renderWidth }

        // render a horizontal segment
        if SegDirection = Horizontal then

            let wireSegmentReactElementList =
                //check if segments are left to right and down to up as it changes corner rendering 
                let isSegmentLefttoRight = if( SegEndX - SegStartX > 0) then true else false
                let isNextSegDowntoUp = if( (getEndPoint nextSegmentSkipPoint).Y - nextSegmentSkipPoint.Start.Y < 0) then true else false
                //if next seg horizontal fix graphical path that renders on end

                if (NextSegDirection=Horizontal&&SegPosition=Penultimate) then [makeLine (SegStartX+prevSegCaluRadius) SegStartY (SegEndX+prevSegCaluRadius) SegEndY lineParameters]
                //if next seg horizontal fix graphical path that renders on start

                elif (NextSegDirection=Horizontal&&SegPosition=First) then [makeLine (SegStartX) SegStartY (SegEndX+nextSegCaluRadius) SegEndY lineParameters]
                //specific rendering of paths for the first segment fixes eg both paths and arcs rendering

                elif (SegPosition=First&&NextSegDirection=Point&&isSegmentLefttoRight&&((getOrientation segmentList[2])=Vertical)) then                                         
                    if (isNextSegDowntoUp = true) then 

                        let startingPoint,endingPoint = {X = SegStartX; Y = SegEndY},{X = SegEndX; Y = SegEndY - nextSegCaluRadius}
                        let startingControlPoint,endingControlPoint = {X = SegEndX ; Y = SegEndY },{X = SegEndX ; Y = SegEndY - nextSegCaluRadius/2.0 }
                        [makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]

                    else 
                        let startingPoint,endingPoint = {X = SegStartX; Y = SegEndY},{X = SegEndX; Y = SegEndY + nextSegCaluRadius}
                        let startingControlPoint,endingControlPoint = {X = SegEndX ; Y = SegEndY },{X = SegEndX ; Y = SegEndY + nextSegCaluRadius/2.0 }
                        [makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]

                elif(SegPosition=First&&NextSegDirection=Point&&isSegmentLefttoRight&&((getOrientation segmentList[2])=Horizontal)) then [makeLine (SegStartX) SegStartY (SegEndX+5.0) SegEndY lineParameters]
                //specific rendering of paths for the last segment fixes eg both paths and arcs rendering

                elif (SegPosition=Last&&NextSegDirection=Point&&isSegmentLefttoRight&&((getOrientation segmentList[4])=Vertical)) then

                    if (isNextSegDowntoUp = true) then 
                        let startingPoint,endingPoint = {X = SegStartX ; Y = SegEndY},{X = SegEndX; Y = SegEndY - nextSegCaluRadius}
                        let startingControlPoint,endingControlPoint = {X = SegEndX ; Y = SegEndY },{X = SegEndX ; Y = SegEndY - nextSegCaluRadius/2.0 }
                        [makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]
                    else 
                    
                        let startingPoint,endingPoint = {X = SegStartX ; Y = SegEndY},{X = SegEndX; Y = SegEndY + nextSegCaluRadius}
                        let startingControlPoint,endingControlPoint = {X = SegEndX ; Y = SegEndY },{X = SegEndX ; Y = SegEndY + nextSegCaluRadius/2.0 }
                        [makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]

                //if previous segment is a point and antepenultimate segment is horizontal then only render a wire with no arc
                elif (SegPosition=Last&&PrevSegDirection=Point&&isSegmentLefttoRight&&((getOrientation segmentList[4])=Horizontal)) then [makeLine (SegStartX-5.0) SegStartY SegEndX SegEndY lineParameters]
                
                //render antepenultimate segment with just a line if next segment is a point
                elif (SegPosition=Antepenultimate&&NextSegDirection=Point&&isSegmentLefttoRight&&(SegDirection=Horizontal)) then [makeLine (SegStartX+prevSegCaluRadius) SegStartY SegEndX SegEndY lineParameters]
                //render segment with a curve after it depending upon a calculated radius and which corner it should render
                elif ((isSegmentLefttoRight = true)) then
                    let LineStart = if(SegPosition= First) then SegStartX else (SegStartX+prevSegCaluRadius)
                    if (isNextSegDowntoUp = true) then 
                        let startingPoint,endingPoint = {X = SegEndX - nextSegCaluRadius ; Y = SegEndY},{X = SegEndX; Y = SegEndY - nextSegCaluRadius}
                        let startingControlPoint,endingControlPoint = {X = SegEndX ; Y = SegEndY },{X = SegEndX ; Y = SegEndY - nextSegCaluRadius/2.0 }
                        [makeLine (LineStart) SegStartY (SegEndX-nextSegCaluRadius) SegEndY lineParameters;  makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]

                    else 
                        let startingPoint,endingPoint = {X = SegEndX - nextSegCaluRadius ; Y = SegEndY},{X = SegEndX; Y = SegEndY + nextSegCaluRadius}
                        let startingControlPoint,endingControlPoint = {X = SegEndX ; Y = SegEndY },{X = SegEndX ; Y = SegEndY + nextSegCaluRadius/2.0 }
                        [makeLine (LineStart) SegStartY (SegEndX-nextSegCaluRadius) SegEndY lineParameters;  makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]

                //render segment with a curve after it depending upon a calculated radius and which corner it should render
                elif((isSegmentLefttoRight = false)) then 
                    let LineStart = if(SegPosition= First) then SegStartX else (SegStartX-prevSegCaluRadius)
                    if (isNextSegDowntoUp = true) then 
                        let startingPoint,endingPoint = {X = SegEndX + nextSegCaluRadius ; Y = SegEndY},{X = SegEndX ; Y = SegEndY - nextSegCaluRadius}
                        let startingControlPoint,endingControlPoint = {X = SegEndX ; Y = SegEndY },{X = SegEndX ; Y = SegEndY - nextSegCaluRadius/2.0 }
                        [makeLine (LineStart) SegStartY (SegEndX+nextSegCaluRadius) SegEndY lineParameters;  makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]

                    else 
                        let startingPoint,endingPoint = {X = SegEndX + nextSegCaluRadius ; Y = SegEndY},{X = SegEndX; Y = SegEndY + nextSegCaluRadius}   
                        let startingControlPoint,endingControlPoint = {X = SegEndX ; Y = SegEndY },{X = SegEndX ; Y = SegEndY + nextSegCaluRadius/2.0 }                 
                        [makeLine (LineStart) SegStartY (SegEndX+nextSegCaluRadius) SegEndY lineParameters;  makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]


                else
                [makeLine (SegStartX) SegStartY SegEndX SegEndY lineParameters]

            g [] wireSegmentReactElementList
        
        else
            //rendering vertical segments

            //check if segments are left to right and down to up as it changes corner rendering 
            let isseg1DowntoUp = if( SegEndY - SegStartY < 0) then true
                                 else false
            let isLeftToRight = if( ((getEndPoint nextSegment).X - nextSegment.Start.X) >= 0) then true else false
            
            let segmentElements = 
                 //render vertical segment with a curve after it depending upon a calculated radius and which corner it should render   
                 if ((isseg1DowntoUp = true)) then
                    if (isLeftToRight = true) then 
                        let startingPoint,endingPoint = {X = SegEndX ; Y = SegEndY + nextSegCaluRadius},{X = SegEndX + nextSegCaluRadius; Y = SegEndY}
                        let startingControlPoint,endingControlPoint = {X = SegEndX ; Y = SegEndY },{X = SegEndX + nextSegCaluRadius/2.0; Y = SegEndY  }
                        [makeLine SegStartX (SegStartY-prevSegCaluRadius) SegEndX (SegEndY+nextSegCaluRadius) lineParameters;  makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]

                    else 
                        let startingPoint,endingPoint = {X = SegEndX ; Y = SegEndY + nextSegCaluRadius},{X = SegEndX - nextSegCaluRadius; Y = SegEndY}
                        let startingControlPoint,endingControlPoint = {X = SegEndX ; Y = SegEndY },{X = SegEndX - nextSegCaluRadius/2.0; Y = SegEndY  }
                        [makeLine SegStartX (SegStartY-prevSegCaluRadius) SegEndX (SegEndY+nextSegCaluRadius) lineParameters;  makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]

                 //render vertical segment with a curve after it depending upon a calculated radius and which corner it should render 
                 elif((isseg1DowntoUp = false)) then 
                        if (isLeftToRight = true) then 
                            let startingPoint,endingPoint = {X = SegEndX ; Y = SegEndY - nextSegCaluRadius},{X = SegEndX + nextSegCaluRadius; Y = SegEndY}
                            let startingControlPoint,endingControlPoint = {X = SegEndX ; Y = SegEndY },{X = SegEndX + nextSegCaluRadius/2.0; Y = SegEndY  }
                            [makeLine SegStartX (SegStartY+prevSegCaluRadius) SegEndX (SegEndY-nextSegCaluRadius) lineParameters;  makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]

                        else 
                            let startingPoint,endingPoint = {X = SegEndX ; Y = SegEndY - nextSegCaluRadius},{X = SegEndX - nextSegCaluRadius; Y = SegEndY}
                            let startingControlPoint,endingControlPoint = {X = SegEndX ; Y = SegEndY },{X = SegEndX - nextSegCaluRadius/2.0; Y = SegEndY  }
                            [makeLine SegStartX (SegStartY+prevSegCaluRadius) SegEndX (SegEndY-nextSegCaluRadius) lineParameters;  makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]


                 else[makeLine SegStartX SegStartY SegEndX SegEndY lineParameters]
                
            g [] segmentElements
    //call a map on the rendersegemnt function to generate the required segement list from the renderradiussedwire function
    segmentList |> List.mapi ( fun i (segment : Segment) -> renderSegment segment colour width i (segmentList.Length:int);)

/// This function renders the given
/// wire in either old fashioned or modern wire mode,
/// using the colour, displaytype and width properties given.
let renderOldFashionedorModernWire (segmentList: Segment List) (colour : string) (width : string) (numofSegments:int) (displayMode:Modes) (circleList:XYPos List) : ReactElement List = 
    //render segement inside of render wire in order to have access to next and previous segments
    let renderSegment (segment : Segment) (colour : string) (width : string) : ReactElement = 
        let wOpt = EEExtensions.String.tryParseWith System.Int32.TryParse width
        let renderWidth = 
            match wOpt with
            | Some 1 -> 1.5
            | Some n when n < int "8" -> 2.5
            | _ -> 3.5
        let lineParameters = { defaultLine with Stroke = colour; StrokeWidth = string renderWidth }
        //calculate the current rendered segment direction
        let SegmentDirection = getOrientation segment
        //render a horizontal segment which may include jumps depending on the display mode
        if SegmentDirection = Horizontal then
            let pathParameters = { defaultPath with Stroke = colour; StrokeWidth = string renderWidth }
            //render a single wire line without any jumps
            let renderWireSubSegment (startPos : XYPos) (endPos : XYPos) : list<ReactElement> =
                let SegStartX, SegStartY, SegEndX, SegEndY = startPos.X, startPos.Y, endPos.X, endPos.Y
                [makeLine SegStartX SegStartY SegEndX SegEndY lineParameters]
            //arbritary jump parameters
            let segmentJumpHorizontalSize = 9.0
            let segmentJumpVerticalSize = 6.0
            //create a path which renders the given jump
            let renderSingleSegmentJump (intersectionCoordinate : XYPos) : list<ReactElement> =
                let x, y = intersectionCoordinate.X, intersectionCoordinate.Y
                //set the parameters for the jump
                let startingPoint,endingPoint = {X = x - segmentJumpHorizontalSize/2.0; Y = y},{X = x + segmentJumpHorizontalSize/2.0; Y = y}
                let startingControlPoint,endingControlPoint = {X = x - segmentJumpHorizontalSize/2.0; Y = y - segmentJumpVerticalSize},{X = x + segmentJumpHorizontalSize/2.0; Y = y - segmentJumpVerticalSize}
                //create reactelement of the jump
                [makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]
            //render multiple jumps recursively by rendering the single jump and the affiliated wires    
            let rec renderMultipleSegmentJumps (segmentJumpCoordinateList : list<float>) (segmentJumpYCoordinate : float) : list<ReactElement> =
                
                match segmentJumpCoordinateList with

                | [] -> []

                | [singleElement] ->
                    renderSingleSegmentJump {X = singleElement; Y = segmentJumpYCoordinate}

                | firstElement :: secondElement :: tailList ->

                    if (segment.Start.X > (getEndPoint segment).X) then
                        renderSingleSegmentJump {X = firstElement; Y = segmentJumpYCoordinate}
                        @
                        renderWireSubSegment {X = firstElement - segmentJumpHorizontalSize/2.0; Y = segmentJumpYCoordinate} {X = secondElement + segmentJumpHorizontalSize/2.0; Y = segmentJumpYCoordinate}
                        @
                        renderMultipleSegmentJumps (secondElement :: tailList) (segmentJumpYCoordinate)
                    
                    else
                        renderSingleSegmentJump {X = firstElement; Y = segmentJumpYCoordinate}
                        @
                        renderWireSubSegment {X = firstElement + segmentJumpHorizontalSize/2.0; Y = segmentJumpYCoordinate} {X = secondElement - segmentJumpHorizontalSize/2.0; Y = segmentJumpYCoordinate}
                        @
                        renderMultipleSegmentJumps (secondElement :: tailList) (segmentJumpYCoordinate)
                
            //render the entire segment, breaking up the segement into rendering the pre jump subsegment, then rendering the jumps and their connecting sgements, then the final subsegment
            let completeWireSegmentRenderFunction (seg : Segment) : list<ReactElement> =
                //if old fashioned then populate the jumpcoordinate list otherwise if modern then do not populate the jump list
                let jumpCoordinateList =
                    match displayMode with 
                        | OldFashionedCircuit ->    if (segment.Start.X > (getEndPoint segment).X) then
                                                        seg.JumpCoordinateList
                                                        |> List.sortDescending
                            
                                                    else
                                                        seg.JumpCoordinateList
                                                        |> List.sort
                        | ModernCircuit -> []

                        | Radiussed -> failwithf "unexpected area of code"                                                                        
                //if no jumps then render a single wire, else render the segemnts and the jumps in between
                match jumpCoordinateList with
                    | [] -> renderWireSubSegment seg.Start (getEndPoint segment)

                    | lst ->
                        let y = seg.Start.Y
                        let firstSegmentJumpCoordinate = lst[0]
                        let lastSegmentJumpCoordinate = lst[(List.length lst) - 1]

                        if (segment.Start.X > (getEndPoint segment).X) then
                            renderWireSubSegment seg.Start {X = firstSegmentJumpCoordinate + segmentJumpHorizontalSize/2.0; Y = y}
                            @
                            renderMultipleSegmentJumps lst y
                            @
                            renderWireSubSegment {X = lastSegmentJumpCoordinate - segmentJumpHorizontalSize/2.0; Y = y} (getEndPoint segment)

                        else
                            renderWireSubSegment seg.Start {X = firstSegmentJumpCoordinate - segmentJumpHorizontalSize/2.0; Y = y}
                            @
                            renderMultipleSegmentJumps lst y
                            @
                            renderWireSubSegment {X = lastSegmentJumpCoordinate + segmentJumpHorizontalSize/2.0; Y = y} (getEndPoint segment)
            

            let wireSegmentReactElementList = segment |> completeWireSegmentRenderFunction
            //return the reactelement list
            g [] wireSegmentReactElementList
        
        else
        //if vertical then dont render jumps, only render wire
            let SegStartX, SegStartY, SegEndX, SegEndY = segment.Start.X, segment.Start.Y, (getEndPoint segment).X, (getEndPoint segment).Y
            let segmentElements = [makeLine SegStartX SegStartY SegEndX SegEndY lineParameters]
        
            g [] segmentElements

    //call a map on the rendersegemnt function to generate the required segement list from the renderradiussedwire function
    let segmentElements = segmentList |> List.mapi ( fun i (segment : Segment) -> renderSegment segment colour width;)
    //if modern then populate the circles list otherwise if old fashioned then do not populate the circle list
    let renderCirclesList = match displayMode with 
                                | OldFashionedCircuit -> []
                                | ModernCircuit -> circleList
                                | Radiussed -> failwithf "unexpected area of code"

    //circle parameters for modern display mode
    let ModerncircleParameters = { defaultCircle with R = 6.0; Stroke = colour; Fill = colour }
    //if there are any circles then render all the circles for the modern circuit mode
    let circleElements = renderCirclesList |> List.mapi ( fun i (circlepos : XYPos) -> makeCircle (abs circlepos.X) (abs circlepos.Y) ModerncircleParameters;)
    //return a main concatenated list of circle react elements and wire react elements for modern mode
    segmentElements @ circleElements

///Main wrapper function for rendering a whole wire, it will call the relevant rendering function and input the associated parameters for each display type
let RenderWire (segmentList: Segment List) (colour : string) (width : string) (numofSegments:int) (displayMode:Modes) (circleList:XYPos List) : ReactElement List = 
    match displayMode with 
        |OldFashionedCircuit -> renderOldFashionedorModernWire segmentList colour width numofSegments OldFashionedCircuit circleList : ReactElement List
        |ModernCircuit -> renderOldFashionedorModernWire segmentList colour width numofSegments ModernCircuit circleList : ReactElement List
        |Radiussed -> renderRadiusedWire segmentList colour width numofSegments : ReactElement List

///
type WireRenderProperties =
    {
        key: string
        Segments: list<Segment>
        ColorP: HighLightColor
        DisplayType: Modes
        SplitWireList : XYPos List
        StrokeWidthP: int
        OutputPortLocation: XYPos
    }
///Render a Wire and attach a buswidth label, using given inputted properties fetched from the model
let RenderWireAttachBusWidth = 
    FunctionComponent.Of(
        fun ((renderProperties: WireRenderProperties)) ->
            //call the render wire function with the model's displaytype 
            let renderWireSegmentList : list<ReactElement> = RenderWire renderProperties.Segments (renderProperties.ColorP.Text()) (string renderProperties.StrokeWidthP) (renderProperties.Segments.Length:int) renderProperties.DisplayType renderProperties.SplitWireList
            //bus width properties    
            let renderWireWidthText : ReactElement =
                let textParameters =
                    {
                        TextAnchor = "left";
                        FontSize = "12px";
                        FontWeight = "Bold";
                        FontFamily = "Verdana, Arial, Helvetica, sans-serif";
                        Fill = renderProperties.ColorP.Text();
                        UserSelect = UserSelectOptions.None;
                        DominantBaseline = "middle";
                    }
                //bus width string
                let textString = if renderProperties.StrokeWidthP = 1 then "" else string renderProperties.StrokeWidthP //Only print width > 1
                makeText (renderProperties.OutputPortLocation.X+1.0) (renderProperties.OutputPortLocation.Y-7.0) (textString) textParameters
            //return the rendered wire and bus width react elements
            g [] ([ renderWireWidthText ] @ renderWireSegmentList)
        
    , "Wire"
    , equalsButFunctions
    )
///Render every Wire and symbol in the model that has been defined, using properties from the model
let RenderModel (model : Model) (dispatch : Dispatch<Msg>) =
    let start = TimeHelpers.getTimeMs()
    //generate the wire array from the model
    let wiresToRender =
        model.WX
        |> Map.toArray
        |> Array.map snd
    TimeHelpers.instrumentTime "WirePropsSort" start
    let rStart = TimeHelpers.getTimeMs()
    //call the RenderWireAttachBusWidth function on every wire in the wire array
    let RenderedWires =
        wiresToRender
        |> Array.map
            (
                fun wire ->
                    let stringOutId =
                        match wire.OutputPort with
                        | OutputPortId stringId -> stringId
                        
                    let outputPortLocation = Symbol.getOutputPortLocation model.Symbol wire.OutputPort
                    //define the properties for the rendering including display type
                    let renderProperties =
                        {
                            key = match wire.Id with | ConnectionId s -> s
                            Segments = List.map makeSegPos wire.Segments
                            DisplayType = model.Mode
                            SplitWireList = model.SplitWireList
                            ColorP = wire.Color
                            StrokeWidthP = wire.Width
                            OutputPortLocation = outputPortLocation
                        }
                    RenderWireAttachBusWidth renderProperties)
    TimeHelpers.instrumentInterval "WirePrepareProps" rStart ()
    let symbols = Symbol.view model.Symbol (Symbol >> dispatch)
    //return the rendered wires and rendered symbols
    g [] [(g [] RenderedWires); symbols]
    |> TimeHelpers.instrumentInterval "WireView" start



// ----------------------------------------------------------------------------------------------------------------------------------



/// This function is given two couples of
/// points that define two line segments and it returns:
/// - Some (x, y) if the two segments intersect;
/// - None if the do not.
let tryCoordinatesIfIntersection ((startSeg1, endSeg1) : (XYPos * XYPos)) ((startSeg2, endSeg2) : (XYPos * XYPos)) : Option<XYPos> =

    // Check if the two intersect
    if (segmentIntersectsSegment (startSeg1, endSeg1) (startSeg2, endSeg2)) then                                        
        // Extract their coordinates
        let x1, y1, x2, y2 = startSeg1.X, startSeg1.Y, endSeg1.X, endSeg1.Y
        let x3, y3, x4, y4 = startSeg2.X, startSeg2.Y, endSeg2.X, endSeg2.Y
        // How far from the start on seg1 the intersection happens (between 0 and 1)
        // (diffXseg2 * diffYstarts - diffYseg2 * diffXstarts ) / (diffYseg2 * diffXseg1 - diffXseg2 * diffYseg1)
        let uA = ((x4-x3)*(y1-y3) - (y4-y3)*(x1-x3)) / ((y4-y3)*(x2-x1) - (x4-x3)*(y2-y1))
        
        // Compute the coordinates of the intersection, from the coordinates of the first segment
        let intersectionX = x1 + (uA * (x2-x1))
        let intersectionY = y1 + (uA * (y2-y1))
        Some {X = intersectionX; Y = intersectionY}
    
    else None


/// This funtion is given a bounding box and it returns the coordinates
/// of the top-left and the bottom-right corners of this bounding box.
let getTopLeftAndBottomRightCorner (box : BoundingBox) : XYPos * XYPos = 
    let {BoundingBox.X = x; BoundingBox.Y = y} = box
    let {BoundingBox.H = h; BoundingBox.W = w} = box
    // Compute the coordinates of the corners of the bounding box
    let corners = [(x, y); (x, y+h); (x+w, y); (x+w, y+h)]
    // Handle negative widths and heights
    let topLeft = List.min corners
    let bottomRight = List.max corners
    // Assemble both coordinates in an XYPos for output
    {X = fst(topLeft) ; Y = snd(topLeft)} , {X = fst(bottomRight) ; Y = snd(bottomRight)}


/// This function is given a Segment and a BoundingBox
/// and it returns:
/// - (false, None) if the segment does not intersect the bounding box
/// - (true, None) if the segment is fully included inside the bounding box 
/// - (true, Some coordinate)  if the segment intersects the bounding box
let isSegmentIntersectingBoundingBox (seg : Segment) (bb : BoundingBox) : bool =
    // Get the absolute coordinates of the segment
    let segEnd = getEndPoint seg
    // Get the topLeft and bottomRight corners of the bounding box, and decompose/match their XYPos into distinct values
    //let ({X = x; Y = y} : XYPos), ({X = a; Y = b} : XYPos) = getTopLeftAndBottomRightCorner bb
    let ({X = xTL; Y = yTL} : XYPos), ({X = xBR; Y = yBR} : XYPos) = getTopLeftAndBottomRightCorner bb
    // Decompose the coordinates of the segment's endpoints
    let x1, y1, x2, y2 = seg.Start.X, seg.Start.Y, segEnd.X, segEnd.Y

    // Check to see if all coordinates of the segment are within the limits of the bounding box
    let segPointInBox =
        (( (x1 > xTL) && (x1 < xBR) ) && ( (y1 > yTL) && (y1 < yBR) ))
        ||
        (( (x2 > xTL) && (x2 < xBR) ) && ( (y2 > yTL) && (y2 < yBR) ))
    
    // Get, if they exist, the intersections of the segment with the 4 sides of the bounding box
    let left = tryCoordinatesIfIntersection (seg.Start, (getEndPoint seg)) ({X=xTL; Y=yTL}, {X=xTL; Y=yBR})
    let right = tryCoordinatesIfIntersection (seg.Start, (getEndPoint seg)) ({X=xBR; Y=yTL}, {X=xBR; Y=yBR})
    let top = tryCoordinatesIfIntersection (seg.Start, (getEndPoint seg)) ({X=xTL; Y=yTL}, {X=xBR; Y=yTL})
    let bottom = tryCoordinatesIfIntersection (seg.Start, (getEndPoint seg)) ({X=xTL; Y=yBR}, {X=xBR; Y=yBR})
    
    // Put the intersections in a list
    let (intersectionList : list<XYPos>) = 
        [top; bottom; left; right]
        |> List.choose id   // Filter out the None elements

    // If no intersections, 
    if intersectionList.Length = 0 then
        // check if the segment is fully in the bounding box
        if segPointInBox then
            true
        else
            false
    else
        true


/// This function is given a point and a segment
/// and it returns the distance between them.
let distanceFromPointToSegment (point : XYPos) (segment : Segment) : float = 
    let x0, y0 = point.X, point.Y
    let segmentEnd = getEndPoint segment
    let x1, y1, x2, y2 = segment.Start.X, segment.Start.Y, segmentEnd.X, segmentEnd.Y
    // If the segment if only horizontal or vertical
    if (x1 = x2) then abs (x1 - x0)
    elif (y1 = y2) then abs (y1 - y0)
    // otherwise, compute the distance
    else
        let numer = abs (  (x2-x1)*(y1-y0) - (x1-x0)*(y2-y1)  )
        let denom = sqrt (  (x2-x1)*(x2-x1) + (y2-y1)*(y2-y1)  )
        numer/denom


/// Given the current state of the BusWire model,
/// a ConnectionId (id of a wire) and an BoundingBox,
/// this function returns a list of Segments of the
/// wire (corresponding to the given id) that intersect the bounding box.
let getIntersectingSegments (model:Model) (wireId:ConnectionId) (selectBox:BoundingBox) : list<Segment> =     
    model.WX[wireId].Segments
    // Filter the wire's segments according to if they intersect the bounding box
    |> List.filter (fun seg -> isSegmentIntersectingBoundingBox seg selectBox)


//Finds the closest segment in a wire to a point using euclidean distance
let getClosestSegment (model : Model) (wireId : ConnectionId) (pos : XYPos) : Segment =
    model.WX[wireId].Segments
    // Get the segment of the list with the minimum distance from the point
    |> List.minBy (
        fun seg -> 
            distanceFromPointToSegment pos seg)


/// Return the segment in the wire that was clicked on by the user
/// Function called when a wire has been clicked, so no need to be an option
let getClickedSegment (model:Model) (wireId: ConnectionId) (pos: XYPos) : SegmentId =
    // Get the area around the mouse click
    let boundingBox = {X = pos.X - 5.0; Y = pos.Y - 5.0; H = 10.0; W = 10.0}
    // Get the segments in range of mouse click
    let intersectingSegments = getIntersectingSegments model wireId boundingBox

    //getIntersectingSegments may not return anything at low resolutions as the mouse was not on any segment, but in range of the wire bbox
    //In this case just return the segment closest to mouse position
    //Should it just do this anyway? No as it breaks some functionality (some segments can't be clicked)
    if List.isEmpty intersectingSegments 
    then (getClosestSegment model wireId pos).Id
    else (List.head intersectingSegments).Id


/// Reverse segment order, and Start, End coordinates, so list can be processed from input to output
/// this function is self-inverse
let revSegments (segs:Segment list) =
    List.rev segs
    |> List.map (fun seg -> {seg with Start = (getEndPoint seg) ; Vector = {X = - seg.Vector.X ; Y = - seg.Vector.Y}})

       
let getSafeDistanceForMoveFromStart (index: int) (segments: Segment list) (distance:float) =
    /// Check if 2 floats are notEqual according to some threshold
    let areFloatsClose (x: float) (y: float) : bool =
        (abs (x - y)) < onStickAxisThreshold

    // Get the orientation of the stick and of the segment being moved
    let stickOrientation = getOrientation segments[0]
    let segOrientation = getOrientation segments[index]

    // Boolean to see if the segment being moved is perpendicular to the stick
    let isPerpendicularToStick = stickOrientation <> segOrientation

    // The shrink factor is 1 for all segments perpendicular to the stick
    // that are after the first perpendicular segment after the stick (which has index 2)
    let shrink = match isPerpendicularToStick with 
                 | true when (index > 2) -> 1.0
                 | _                     -> 0.5
    
    // Start of seg has same X or Y as start of stick (depending on orientation of stick)
    let hasStartOnStickAxis =               
        match stickOrientation with
        | Horizontal -> areFloatsClose segments[0].Start.Y segments[index].Start.Y
        | Vertical   -> areFloatsClose segments[0].Start.X segments[index].Start.X
        | _          -> false

    // Distance is increasing/positive and stick points towards negative numbers,
    // or opposite (get sign of X or Y coord of vector, according to stick orientation)
    let isGoingTowardsStick =
        match stickOrientation with
        | Horizontal when (segments[0].Vector.X >= 0.0) -> (distance < 0.0)
        | Horizontal                                    -> (distance >= 0.0)
        | Vertical   when (segments[0].Vector.Y >= 0.0) -> (distance < 0.0)
        | Vertical                                      -> (distance >= 0.0)
        | _                                             -> false

    // Distance, along the right axis, between the end of shrinked stick, and start of seg being moved
    let safeDistanceToShrinkedStick =      
        match stickOrientation with
        | Horizontal -> (segments[0].Start.X + (float (sign segments[0].Vector.X) * Wire.stickLength * shrink)) - segments[index].Start.X
        | Vertical   -> (segments[0].Start.Y + (float (sign segments[0].Vector.Y) * Wire.stickLength * shrink)) - segments[index].Start.Y
        | _          -> 0.0

    // Compute the max distance that the segment can be moved by, while abiding by stick constraints
    let restreignedDistance = 
        match (distance >= 0) with
        // If Distance is positive, take the MIN between Distance and 'distance between the seg start and end of stick' (assuming same sign)
        | true  -> min distance safeDistanceToShrinkedStick
        // If Distance is negative, take the MAX between Distance and 'distance between the seg start and end of stick' (assuming same sign)
        | false -> max distance safeDistanceToShrinkedStick

    // Then, if they are perpendicular, and the start of the segment being moved is on the same axis of the stick, restrict their movement
    match (isPerpendicularToStick, hasStartOnStickAxis, isGoingTowardsStick) with
    | (true, true, true) -> restreignedDistance
    | _                  -> distance


let getSafeDistanceForMove (index: int) (segments: Segment list) (distance:float) =
    // Get the safe distance from the start of the wire
    let safeDistanceFromStart = getSafeDistanceForMoveFromStart index segments distance
    
    // Reverse the wire and get the safe distance to the start of the reversed wire, i.e. the end of the initial wire
    let (revIndex: int) = (segments.Length - 1) - index
    let (revSegments: Segment list) = revSegments segments
    let safeDistanceFromStartAndEnd = getSafeDistanceForMoveFromStart revIndex revSegments safeDistanceFromStart
    
    // Return that final safe distance
    safeDistanceFromStartAndEnd


/// Adjust wire (input type is Segment list) around the moved segment's index,
/// so that two adjacent parallel segments that are in opposite directions
/// get eliminated. 
/// Note: we remove redundant segments directly as they are created,
/// instead of filtering the whole wire afterwards.
let removeRedundantSegments (index: int) (segs: Segment list) = 
    /// Checks if the length of a segment is less than a threshold
    let isSmall (seg: Segment) : bool =
        (getAbsLength seg) <= onRedundantSegmentAxisThreshold

    /// Returns the negated Vector of the segment
    let negVector (seg: Segment) : XYPos = 
        {X = - seg.Vector.X ; Y = - seg.Vector.Y}

    // Check if a segment is a Stick, and if it is of a minimum length
    let preservesStick (seg: Segment) : bool = 
        match ((seg.Index = 0) || (seg.Index = segs.Length-1)) with
        | true  -> ((getAbsLength seg) >= Wire.stickLength)
        | false -> true

    /// Takes two segments, and if they are Horizontal and in opposite direction, "adjust" them
    let adjust (seg1: Segment) (segMiddle: Segment) (seg2: Segment) =
        // Get their directions
        let seg1Length = match (getOrientation seg1) with
                            | Horizontal -> seg1.Vector.X
                            | Vertical   -> seg1.Vector.Y
                            | _          -> 0.0
        let seg2Length = match (getOrientation seg2) with
                            | Horizontal -> seg2.Vector.X
                            | Vertical   -> seg2.Vector.Y
                            | _          -> 0.0
        // If they are horizontal and of opposite direction/length, with a small/Point/invisible segment in between them
        if ((getOrientation seg1) = (getOrientation seg2))
           && ((sign seg1Length) <> (sign seg2Length))
           && (isSmall segMiddle)
           && (preservesStick seg1)
           && (preservesStick seg2)
        then
            // If the first segment is longer than the second one
            if abs seg1Length > abs seg2Length then
                // replace the end of segment 1 with the end of segment 2 (add both vectors), 
                // and the start of segment 2 with its end (and making it of length 0).
                // also translate the middle segment by the vector of segment 2
                ({seg1 with Vector = addPositions seg1.Vector seg2.Vector}, 
                 {segMiddle with Start = addPositions segMiddle.Start seg2.Vector},
                 {seg2 with Start = getEndPoint seg2 ; Vector = {X=0.0 ; Y=0.0}})
            else
                // do the opposite
                ({seg1 with Vector = {X=0.0 ; Y=0.0}},  // note: the start of seg1 is moved by the moveSegment function
                 {segMiddle with Start = addPositions segMiddle.Start (negVector seg1)},    //translate middle segment
                 {seg2 with Start = addPositions seg2.Start (negVector seg1) ; Vector = addPositions seg1.Vector seg2.Vector})  //move start of seg2 without moving the end
        else
            // Otherwise, do nothing
            (seg1, segMiddle, seg2)
    
    // Check which sides of the wire, around the moved segment, have enough segments to perform redundancy removal
    match ((index >= 3), (index < segs.Length-3)) with
    | true, true   -> 
        // Adjust the segments that have possible redundancy on both sides
        let adjSegM3, adjSegM2, adjSegM1 = adjust segs[index-3] segs[index-2] segs[index-1]
        let adjSegP1, adjSegP2, adjSegP3 = adjust segs[index+1] segs[index+2] segs[index+3]
        // Assemble the adjusted segment list:
        // firstSegs @ adjustedSegs @ lastSegs
        segs[0..(index - 4)]
        @ [adjSegM3 ; adjSegM2 ; adjSegM1 ; segs[index] ; adjSegP1 ; adjSegP2 ; adjSegP3]
        @ segs[(index + 4)..(segs.Length-1)]
    | true, false  -> 
        // Adjust the segments that have possible redundancy on the "left"/"smaller indexes" side
        let adjSegM3, adjSegM2, adjSegM1 = adjust segs[index-3] segs[index-2] segs[index-1]
        // Assemble the adjusted segment list
        segs[0..(index - 4)]
        @ [adjSegM3 ; adjSegM2 ; adjSegM1]
        @ segs[index..(segs.Length-1)]
    | false, true  ->
        // Adjust the segments that have possible redundancy on the "right"/"greater indexes" side
        let adjSegP1, adjSegP2, adjSegP3 = adjust segs[index+1] segs[index+2] segs[index+3]
        // Assemble the adjusted segment list
        segs[0..index]
        @ [adjSegP1 ; adjSegP2 ; adjSegP3]
        @ segs[(index + 4)..(segs.Length-1)]
    | false, false ->
        // If not enough segments around the moved segment to perform redundancy removal, don't do anything
        segs


let alignToCloseParallelSegments (index: int) (currentWireOutputPort : OutputPortId) (allModelWires: Wire list) (segs: Segment list) = 
    // Get the segment that has just been moved
    let segMoved = segs[index]

    /// Returns the coordinate along the normal axis of a segment
    let getNormalCoord (seg: Segment) : float =
        match getOrientation seg with
        | Horizontal -> seg.Start.Y
        | Vertical   -> seg.Start.X
        | _          -> 0.0

    /// Check if two segments are on the same level
    let areSegsOnSameLevel (seg1: Segment) (seg2: Segment) : bool = 
        if ((abs ((getNormalCoord seg2) - (getNormalCoord seg1))) <= stickynessThreshold)
        then true
        else false

    /// Check if the end of seg1 is close to the start of seg2
    let areXYPosClose (pos1: XYPos) (pos2: XYPos) : bool = 
        if (((abs (pos1.X - pos2.X)) <= stickynessThreshold)
            && ((abs (pos1.Y - pos2.Y)) <= stickynessThreshold))
        then true
        else false

    /// Takes two segments, and if they are Horizontal and in opposite direction, "adjust" them
    let alignTo (segB: Segment) ((prevSegA: Segment), (segA: Segment), (nextSegA: Segment)) =
        // Find the difference in level to compensate for
        let diff = (getNormalCoord segB) - (getNormalCoord segA)
        // Shift and extend the segments to align segA with segB
        match getOrientation segA with
        | Horizontal ->
            ({prevSegA with Vector = {X = prevSegA.Vector.X; Y = prevSegA.Vector.Y + diff}},
             {segA with Start = {X = segA.Start.X; Y = segA.Start.Y + diff}},
             {nextSegA with 
                 Start = {X = nextSegA.Start.X; Y = nextSegA.Start.Y + diff} ; 
                 Vector = {X = nextSegA.Vector.X; Y = nextSegA.Vector.Y - diff}
             })
        | Vertical   ->
            ({prevSegA with Vector = {X = prevSegA.Vector.X + diff; Y = prevSegA.Vector.Y}},
            {segA with Start = {X = segA.Start.X + diff; Y = segA.Start.Y}},
            {nextSegA with 
                Start = {X = nextSegA.Start.X + diff; Y = nextSegA.Start.Y} ; 
                Vector = {X = nextSegA.Vector.X - diff; Y = nextSegA.Vector.Y}
            })
        | _          ->
            (prevSegA, segA, nextSegA)            
    

    let (segmentsFromSameOutputPort: Segment list) = 
        allModelWires
        // Filter all the wires coming out of the same output port
        |> List.filter (fun (wire: Wire) -> (wire.OutputPort = currentWireOutputPort))
        // Extract all their segments into a single Segment list
        |> List.collect (fun (wire: Wire) -> wire.Segments)

    let (alignmentMatchSegs: Segment list) = 
        segmentsFromSameOutputPort
        |> List.filter (fun seg -> ((getOrientation seg) = (getOrientation segMoved)))
        // Filter the segments that are on the same "level" as the segment moved
        |> List.filter (fun seg -> areSegsOnSameLevel segMoved seg)
        // Filter the segments that have their end (or start) close to the start (or end) of the segment moved
        |> List.filter (fun seg -> (areXYPosClose (getEndPoint seg) segMoved.Start)
                                    || (areXYPosClose seg.Start (getEndPoint segMoved))
                                    || (areXYPosClose seg.Start segMoved.Start)
                                    || (areXYPosClose (getEndPoint seg) (getEndPoint segMoved)) )
        // Filter out the current segment moved from this list
        |> List.filter (fun seg -> (seg.Id <> segMoved.Id))

    let isValidIndex = ((index >= 1) && (index < (segs.Length - 1)))

    match isValidIndex, alignmentMatchSegs with
    | true, hd::tl ->
        // Adjust the segments that have possible redundancy on both sides
        let alignedPrevSeg, alignedSeg, alignedNextSeg = alignTo hd (segs[index-1], segMoved, segs[index+1])
        // Assemble the adjusted segment list:
        // firstSegs @ adjustedSegs @ lastSegs
        segs[0..(index - 2)] @ [alignedPrevSeg; alignedSeg; alignedNextSeg] @ segs[(index + 2)..(segs.Length-1)]
    | _, _ -> segs
    

/// MANUAL ROUTING: 
/// This function allows a wire segment to be moved a given amount in a direction perpedicular to
/// its orientation (Horizontal or Vertical). Used to manually adjust routing by mouse drag.
/// The moved segment is tagged by negating one of its coordinates so that it cannot be auto-routed
/// after the move, thus keeping the moved position.
let moveSegment (seg:Segment) (distance:float) (model:Model) = 
    // Get the wire that the segment is in
    let (wire : Wire) = model.WX[seg.HostId]

    // Retrieve the position of the segment in the segment list of the wire
    let index = seg.Index
    // Check if the segment has a valid index, and is neither the first, nor the last segment
    if index <= 0 || index >= wire.Segments.Length - 1 then
        failwithf $"Buswire segment index {index} out of range in moveSegment in wire length {wire.Segments.Length}"

    // Get the previous and next segments
    let prevSeg = wire.Segments[index-1]
    let nextSeg = wire.Segments[index+1]

    // If the segment has the same direction than any of it's neighbours, don't do anything
    if ((getOrientation seg) = (getOrientation prevSeg)) || ((getOrientation seg) = (getOrientation nextSeg)) then
        wire
    // Else, move the segment, and update the neighbours
    else
        //runTestFable()
        distance      
        |> getSafeDistanceForMove index wire.Segments
        |> (fun (distance': float) ->
            // Define the updated coordinates of the segments based on the orientation of the segment being moved
            let newPrevVector, newSegStart, newNextStart, newNextVector = 
                match (getOrientation seg) with
                // If it's vertical, update the X coordinates
                | Vertical -> 
                    //extract old XYPos, and change the X
                    {prevSeg.Vector with X = (prevSeg.Vector.X + distance')},
                    {seg.Start with X = (seg.Start.X + distance')}, 
                    {nextSeg.Start with X = (seg.Start.X + seg.Vector.X + distance')},
                    {nextSeg.Vector with X = (nextSeg.Vector.X - distance')}
                //If it's horizontal, update the Y coordinates
                | Horizontal -> 
                    //extract old XYPos, and change the Y
                    {prevSeg.Vector with Y = (prevSeg.Vector.Y + distance')},  
                    {seg.Start with Y = (seg.Start.Y + distance')},        
                    {nextSeg.Start with Y = (seg.Start.Y + seg.Vector.Y + distance')},
                    {nextSeg.Vector with Y = (nextSeg.Vector.Y - distance')}
                | _ -> 
                    // Wild card: don't change anything
                    prevSeg.Vector,
                    seg.Start,
                    nextSeg.Start,
                    nextSeg.Vector
            
            // Compile the new segments with updated XYPos Starts and Ends
            let newPrevSeg = {prevSeg with Vector = newPrevVector}
            let newSeg     = {seg with Start = newSegStart ; Autorouted = false}
            let newNextSeg = {nextSeg with Start = newNextStart ; Vector = newNextVector}

            // Extract the other wires from the model for stickiness/alignement purposes
            let (allModelWires : Wire list) = 
                Map.toList model.WX
                |> List.map (fun element -> snd(element))
        
            // Rebuild the list of segments of the wire with the updated segments at the right indexes
            let newSegments =
                wire.Segments[.. index-2] @ [newPrevSeg; newSeg; newNextSeg] @ wire.Segments[index+2 ..]
                // Remove all redundant segments that may have been created by the move
                |> removeRedundantSegments index
                // Align the moved segment with any parallel segments in the wire that are close to it
                |> alignToCloseParallelSegments index wire.OutputPort allModelWires

            // Update the list of segments in the wire object, and return it
            {wire with Segments = newSegments})


/// Initialisatiton with no wires
let init () =
    let symbols,_ = Symbol.init()
    {   
        WX = Map.empty;
        FromVerticalToHorizontalSegmentIntersections = Map.empty;
        FromHorizontalToVerticalSegmentIntersections = Map.empty;
        Symbol = symbols; 
        CopiedWX = Map.empty; 
        SelectedSegment = SegmentId(""); 
        LastMousePos = {X = 0.0; Y = 0.0};
        ErrorWires = []
        Notifications = None
        Mode = OldFashionedCircuit
        SplitWireList = []
    } , Cmd.none


///Returns the wires connected to a list of components given by componentIds
let getConnectedWires (wModel : Model) (compIds : list<ComponentId>) =
    let inputPorts, outputPorts = Symbol.getCmpsPortLocations wModel.Symbol compIds
    wModel.WX
    |> Map.toList
    |> List.map snd
    |> List.filter (fun wire -> Map.containsKey wire.InputPort inputPorts || Map.containsKey wire.OutputPort outputPorts)
    |> List.map (fun wire -> wire.Id)
    |> List.distinct


///Returns a tuple of IDs of: wires connected to inputs ONLY, wires connected to outputs ONLY, wires connected to both inputs and outputs
let getWiresConnectedToPorts (wModel : Model) (compIds : list<ComponentId>) =
        // Get the maps of input and output ports from specific symbols in the model
        let inputPorts, outputPorts = Symbol.getCmpsPortLocations wModel.Symbol compIds
        // Get the list of all wires in the model
        let lst = 
            wModel.WX
            |> Map.toList
            |> List.map snd

        // Get the list of wires that are connected to Inputs
        let inputWires =
            lst
            // Filter the Wires according to if the map of inputPorts in the model contains the InputPort of the current wire
            |> List.filter (fun wire -> Map.containsKey wire.InputPort inputPorts && (not (Map.containsKey wire.OutputPort outputPorts)))
            // Get its ID
            |> List.map (fun wire -> wire.Id)
            // Remove duplicates
            |> List.distinct

        // Get the list of wires that are connected to Outputs
        let outputWires =
            lst
            |> List.filter (fun wire -> Map.containsKey wire.OutputPort outputPorts && (not (Map.containsKey wire.InputPort inputPorts)))
            |> List.map (fun wire -> wire.Id)
            |> List.distinct

        // Get the list of wires that are Fully Connected (both connected to inputs and outputs)
        let fullyConnected =
            lst
            |> List.filter (fun wire -> Map.containsKey wire.InputPort inputPorts && Map.containsKey wire.OutputPort outputPorts)
            |> List.map (fun wire -> wire.Id)
            |> List.distinct

        // Return the three list in a tuple
        {| InputWires = inputWires ; OutputWires = outputWires ; FullyConnectedWires = fullyConnected |}


/// FULL AUTOROUTING: 
/// Returns a new fully autorouted wire given a model and wire
let autorouteWire (model : Model) (wire : Wire) : Wire =
    // Get the port positions
    let outputPortPos = Symbol.getOutputPortLocation model.Symbol wire.OutputPort
    let inputPortPos = Symbol.getInputPortLocation model.Symbol wire.InputPort
    // Get the characteristics of the input and output symbols
    let outputSymbol = Symbol.getSymbolFromOutPortId model.Symbol wire.OutputPort
    let outputSymbolRotation = int (outputSymbol.Rotation)
    let outputSymbolFlip = outputSymbol.SymbolCharacteristics.flip
    let inputSymbol = Symbol.getSymbolFromInPortId model.Symbol wire.InputPort
    let inputSymbolRotation = int (inputSymbol.Rotation)
    let inputSymbolFlip = inputSymbol.SymbolCharacteristics.flip
    let inputPortOnAltSide = Symbol.isPortOnAlternativeSide model.Symbol wire.InputPort

    // Re-generate default Wire shape going from the InputPort to the OutputPort
    {wire with Segments = makeInitialSegmentsList wire.Id outputPortPos inputPortPos outputSymbolRotation inputSymbolRotation outputSymbolFlip inputSymbolFlip inputPortOnAltSide}



/// Move the End of the Nth segment according to the suplied 'mover' function
let moveEnd (mover: XYPos -> XYPos) (n:int) =
    List.mapi (fun i (seg:Segment) -> if i = n then {seg with Vector = (mover (getEndPoint seg)) - seg.Start} else seg)

/// Move the Start of the Nth segment according to the suplied 'mover' function
let moveStart (mover: XYPos -> XYPos) (n:int) =
    List.mapi (fun i (seg:Segment) -> 
            let prevEnd = getEndPoint seg
            if i = n then {seg with Start = mover seg.Start ; Vector = (prevEnd - mover seg.Start)} else seg
        )

/// Move both the Start and the End of the Nth segment according to the suplied 'mover' function (applied on both)
let moveAll (mover: XYPos -> XYPos) (n : int) =
    List.mapi (fun i (seg:Segment) -> if i = n then {seg with Start = mover seg.Start} else seg)    //No need to move the vector

/// Given 2 functions and a point, apply the first one on the X coordinate of the point, and the second on the Y coordinate
let transformXY tX tY (pos: XYPos) =
    {pos with X = tX pos.X; Y = tY pos.Y}

/// Given 2 functions, applies them respectively to the X and Y coordinates of the endpoints of the segment
let transformSeg tX tY (seg: Segment) =
    let trans = transformXY tX tY
    let transStart = trans seg.Start
    let transVector = (trans (getEndPoint seg)) - transStart
    {seg with Start = trans seg.Start ; Vector = transVector }

/// Returns a tuple containing the sign of the difference of the X coordinates, and the sign of the difference of the Y coordinates
let topology (pos1: XYPos) (pos2:XYPos) =
    sign (abs pos1.X - abs pos2.X), sign (abs pos1.Y - abs pos2.Y)


/// Returns None if full autoroute is required or Some segments with initial part of the segment list autorouted
/// up till the first dragged (manually routed) segment.
/// This always done in the order of the segment list, it needs to be reversed before calling the function
let tryPartialAutoRoute (segs: Segment list) (newPortPos: XYPos) =
    // Get the previous portPos
    let portPos = segs[0].Start

    // Get where the first movable segment of the wire started previously
    let wirePos = getEndPoint segs[0]
    // Keep the same "clearance" (the stick) from the port than previously, 
    // and compute where the first movable segment of the wire should now start
    let newWirePos = addPositions newPortPos segs[0].Vector
    //{newPortPos with X = newPortPos.X + (abs wirePos.X - portPos.X) } //old way, only considering horizontal sticks

    // Get by how much the port has moved
    let (diff: XYPos) = {X=newPortPos.X-portPos.X ; Y= newPortPos.Y - portPos.Y}
    
    /// Returns the index of the first index that is manually routed
    let tryGetIndexOfFirstManuallyRoutedSegment =
        segs
        // Append to the new list all segments until one of them is not autorouted
        |> List.takeWhile (fun seg -> seg.Autorouted)
        // Get the index of the next segment to come, as Length = lastIndexOfTheList + 1, so the first manually routed segment
        |> List.length                                  
        // If all segments are autorouted, then the length of the list of autorouted segments will be equal to the length of the initial list of segments.
        // In that case, fail the partial routing to just fully autoroute the segment list.
        |> (fun n -> if n >= (List.length segs) then None else Some (n))

    /// Check if we are still in the same quadrant as the other end of the wire
    let checkTopologyChangeOption index =
        // Get the end coordinates of the manually routed segment
        let manualSegmentEndpoint = getEndPoint segs[index]
        // Compute the topology between the OLD position of the port with the end of the manually fixed wire
        let oldTop = topology (if index = 1 then portPos else wirePos) manualSegmentEndpoint
        // Compute the topology between the NEW position of the port with the end of the manually fixed wire
        let newTop = topology (if index = 1 then newPortPos else newWirePos) manualSegmentEndpoint
        // Not necessary: Check if we are still in the same quadrant as the other end of the wire
        // Check if we are still in the same quadrant as the end of the manually routed segment
        if oldTop = newTop then
            Some index
        else
            None

    /// Scale all the segments between the end of seg[0] and the end of the first manually routed segment.
    let scaleAutoroutedSegments manualSegIndex =
        // Get the first segment that is manually routed (we want its End to be fixed)
        let manualSeg = segs[manualSegIndex]
        // Get its End's coordinates
        let fixedPt = getEndPoint manualSeg
        // Get the start of the wire that we are moving (according to if we can afford to keep a constant length for the stick)
        let startPos = if manualSegIndex = 1 then portPos else wirePos
        let newStartPos = if manualSegIndex = 1 then newPortPos else newWirePos
        
        // Scale the segments by the amount needed
        let scale x fixedX newStartX prevStartX =
            // If the coordinate along the specific axis is the same
            if newStartX = fixedX 
            // Don't do any scaling
            then x
            // Otherwise compute a scaling factor proportional to the distance moved
            else 
                ((x-fixedX)*(newStartX-fixedX)/(prevStartX-fixedX) + fixedX) //* float (sign x)
                //((x-fixedX)*(newStartX-fixedX)/(prevStartX-fixedX) + fixedX) //* float (sign x)
        
        // Curried functions for scaling the X and Y "lengths" of segments
        let scaleX x = scale x fixedPt.X newStartPos.X startPos.X
        let scaleY y = scale y fixedPt.Y newStartPos.Y startPos.Y

        // Extract the n segments to be scaled
        match List.splitAt (manualSegIndex+1) segs, manualSegIndex with
        // If the last autorouted segment's index is 1, scale the stick and the first segment
        | ((scaledSegs), otherSegs), 1 ->
            Some ((List.map (transformSeg scaleX scaleY) scaledSegs) @ otherSegs)
        // Otherwise, if we can extract the head, which is the segment connected to the moving port (i.e. the stick), we just translate it, and scale the other segs
        | ((firstSeg :: scaledSegs), otherSegs), _ ->
            Some ((moveAll (addPositions diff) 0 [firstSeg]) @ (List.map (transformSeg scaleX scaleY) scaledSegs) @ otherSegs)
        // If we can't, then return None to fully autoRoute everything (edge case)
        | _ -> None


    tryGetIndexOfFirstManuallyRoutedSegment     // Get index: None if all segments are autorouted
    |> Option.bind checkTopologyChangeOption    // Check: None if we change quadrants (between moving endpoint and first manually fixed end)
    |> Option.bind scaleAutoroutedSegments


///Moves a wire by a specified amount by adding a XYPos to each start and end point of each segment
let moveWire (wire : Wire) (diff : XYPos) = 
    // Translates a segment by a vector 'diff' 
    let translateSegment (seg:Segment) = 
        {seg with
            //Just update the start, as we don't need to update the vector
            Start = addPositions seg.Start diff
        }
    // Translate all the segments of the wire
    let newSegs = List.map translateSegment wire.Segments
    // Return the new wire
    {wire with Segments = newSegs}

/// PARTIAL AUTOROUTING: 
/// Re-routes a single wire in the model when its ports move.
/// Tries to preserve manual routing when this makes sense, otherwise re-routes with autoroute.
/// Partial routing from input end is done by reversing segments and and swapping Start/End
/// inOut = true => reroute input (target) side of wire.
let updateWire (model : Model) (wire : Wire) (inInputPort : InOut) =
    let newPort = 
        // Get the coordinates of the port that moved, according to if it was the input or output
        match inInputPort with
        | Input -> Symbol.getInputPortLocation model.Symbol wire.InputPort
        | Output -> Symbol.getOutputPortLocation model.Symbol wire.OutputPort
    if inInputPort=Input then
        // reverse, partialAutoRoute and reverse again
        tryPartialAutoRoute (revSegments wire.Segments) newPort
        |> Option.map revSegments
    else 
        // no need to reverse, partialAutoRoute directly
        tryPartialAutoRoute wire.Segments newPort
    // Replace the old segments in the wire by the autorouted ones (if partialAutoRoute worked)
    |> Option.map (fun segs -> {wire with Segments = segs})
    // Otherwise, if it didn't work, re-autoRoute fully the wire
    |> Option.defaultValue (autorouteWire model wire)



// ----------------------------------------------------------------------------------------------------------------------------------



/// Updates the JumpCoordinateList component of every segment in the model
/// To do so, it creates a grid of all possible wire combinations and check for intersection
/// If it locates one, it will update the list
/// While dragged, the jumps of the wire will be reset to 0 and recomputed when released
let makeAllJumps (wiresWithNoJumps: ConnectionId list) (model: Model) =
    // Arrays are faster to check than lists
    let wiresWithNoJumpsA = List.toArray wiresWithNoJumps

    /// Gather all segments of the Model in an Array.
    let segs =
        model.WX
        |> Map.toArray
        |> Array.map (fun (_, w) -> List.toArray w.Segments)
        |> Array.concat

   
    /// Split the array of all segments into two arrays: one for all Horizontals and one for all Verticals.
    let splithorizontalVertical allSegs=
        let isSegInJumpList (seg:Segment) =
            not (Array.contains seg.HostId wiresWithNoJumpsA)
            
        let horizontal (seg:Segment) =
            match getOrientation seg with
            | Horizontal -> true
            | _ -> false 
            
        let vertical (seg:Segment) =
            match getOrientation seg with
            | Vertical -> true
            | _ -> false 

        let horizontalArray = allSegs                
                              |> Array.filter isSegInJumpList
                              |> Array.filter horizontal

        let verticalArray = allSegs                
                            |> Array.filter isSegInJumpList
                            |> Array.filter vertical

        (horizontalArray, verticalArray)


    /// Create a Gird associating all elements of the two arrays.
    /// It will result in an array of all possible combination of Horizontal and Vertical segments.
    let makeHoriVertiGrid (horizontalVerticalSegs: Segment array * Segment array) =
        let horizontalArray = fst horizontalVerticalSegs
        let verticalArray = snd horizontalVerticalSegs

        let makePair a b =
            (a,b)

        let makeColumn (lst: Segment array) (x:Segment) =
            Array.map (makePair x) lst
        Array.map (makeColumn verticalArray) horizontalArray
            |> Array.concat                                 
    


    /// Check if the elements in each pair intersect, while respecting the condition (</> 5)
    /// If they do, append to the list of Jumps a tuple of the HorizontalId and the distance
    /// to the start the intersection occurs.
    let registerAllJumps (hvGrid:(Segment*Segment) array)=
        let checkCrossing ((segHrz:Segment), (segVrt:Segment)) =
            let vrtX =  segVrt.Start.X
            let hrzY =  segHrz.Start.Y 

            let maxMin x y =
                (max x y , min x y)

            let (xhi, xlo) = maxMin ( segHrz.Start.X) ( (getEndPoint segHrz).X)
            let (yhi, ylo) = maxMin ( segVrt.Start.Y) ( (getEndPoint segVrt).Y)
           
            if vrtX < xhi - 5.0 && vrtX > xlo + 5.0 && hrzY < yhi - 5.0 && hrzY > ylo + 5.0 then
                [|(segHrz.Id,vrtX)|]
            else [||]

        Array.map checkCrossing hvGrid
            |> Array.collect id 


    /// Take in an array of jumps and iterate through every Wires and then every segments in 
    /// the model. If the segment id is contained in the Jump key array, then we update its 
    /// JumpCoordinateList component otherwise we reset the component as it no longer/doesn't
    /// have any segments intersecting itself
    /// Moreover, we check that the Value array of Jump contains distinct elements in order to
    /// render it correctly on the Convas.
    let changeJumps (model: Model) (jumps: (SegmentId * float ) array) =   
        let WX = model.WX
        let jumpHorizontalId = Array.toList (Array.map fst jumps)

        let changeSegment (segs: Segment list) =
            List.map (fun (x:Segment) -> if not (List.contains x.Id jumpHorizontalId) then { x with JumpCoordinateList = []}     // Reinitialise it every time we change the concerned wire
                                         else 
                                            let contains (lst:(SegmentId * float)) =
                                                x.Id = (fst lst)
                                            let jumpIdList = Array.filter contains jumps
                                            let jumpList = Array.toList (Array.map snd jumpIdList)
                                                            |> List.distinct
                                            { x with JumpCoordinateList = jumpList} ) segs

        let iterateWire connectionId =
            let updateSegs = changeSegment (WX[connectionId].Segments)
            {WX[connectionId] with Segments = updateSegs}

        let keyList = List.map fst (Map.toList WX)
        List.zip keyList (List.map iterateWire keyList)
        |> Map.ofList

    let newWX = segs
                |> splithorizontalVertical
                |> makeHoriVertiGrid
                |> registerAllJumps
                |> changeJumps model

    { model with WX = newWX }


/// Computes the Coordinates of the Splitting points for wires.
/// When two wires have the same OutputPort id but different InputPort id, in Modern Mode, we have to
/// compute the coordinate representing the separation of the two wires. We first create a grid of all 
/// wires in the model. We then check if they satisfy the Portid situation. If so, we iterate through
/// all the segment pairs and find the first pair where they don't have the same Vector and outputs it. 
let computeWireSplitCoord (model:Model) =

    let WX = model.WX

    /// Create a Grid of all possible combination of Wires in the Model.
    let makeWireGrid (wireList:(ConnectionId*Wire) list) =
        let makePair a b =
            (a,b)

        let makeColumn (lst: (ConnectionId*Wire) list) (x:ConnectionId*Wire)=
            List.map (makePair x) lst
        List.map (makeColumn wireList) wireList
            |> List.concat  
 
    /// Finds the cicle coordinates by iterating through all the segments and finding the
    /// the separation. The return value will depend on the structure:
    /// if the Vectors are of opposite direction then we give out the origin point
    /// if the are pointing in the same direction, we render the end point of the shortest vector.
    let findCircle (segs1: Segment list) (segs2: Segment list) =
        let pairList = Seq.zip (List.toSeq segs1) (List.toSeq segs2)
        let circleSeg = Seq.tryFind (fun ((x:Segment),(y:Segment)) ->  (x.Start.X = y.Start.X) && (x.Start.Y = y.Start.Y) 
                                                                        && not (((getEndPoint x).X = (getEndPoint y).X) 
                                                                        && ((getEndPoint x).Y = (getEndPoint y).Y)))
                                                                        pairList
        let isSameDir (x:float) (y:float) =
            match (x < 0 && y<0) || (x>0 && y>0) with
            | true -> true
            | _ -> false

        match circleSeg with
        | Some (a,b) -> if isSameDir (a.Vector.X + a.Vector.Y) (b.Vector.X + b.Vector.Y)
                        then if (( sqrt (((getEndPoint a).X - a.Start.X)**2 + ((getEndPoint a).Y - a.Start.Y)**2)) 
                                <= sqrt (((getEndPoint b).X - b.Start.X)**2 + ((getEndPoint b).Y - b.Start.Y)**2))
                             then Some (getEndPoint a)
                             else Some (getEndPoint b)
                        else Some (a.Start)
                                
        | None -> None

    /// Takes a pair of wires and check out if they have the same OutputPort id but different InputPort id
    /// The second test is to avoid the case where we have twice the same wire and thus no splitting point
    let getPorts ((wireSeq1:(ConnectionId * Wire)),(wireSeq2:ConnectionId * Wire)) =
        
        let wire1 = snd wireSeq1
        let wire2 = snd wireSeq2
        
        let stringInId1, stringOutId1 =
            match wire1.InputPort, wire1.OutputPort with
            | InputPortId stringId1, OutputPortId stringId2 -> stringId1, stringId2
        let stringInId2, stringOutId2 =
            match wire2.InputPort, wire2.OutputPort with
            | InputPortId stringId1, OutputPortId stringId2 -> stringId1, stringId2

        match (stringOutId1 = stringOutId2) && (stringInId1 <> stringInId2) with
        | false -> None
        | true -> findCircle (wire1.Segments: Segment list) (wire2.Segments: Segment list)


    /// Remove all none from the option list & keep all distinct values.
    let XYPos = makeWireGrid (Map.toList WX) 
               |> List.map getPorts
               |> List.choose id 
               |> List.distinct
    
    { model with SplitWireList= XYPos }


// Update at said interval the jumps arrays

/// For model.Mode = OldFashionedCircuit, it updates the wire model by removing from the stored lists of intersections
/// all those generated by wireList wires. It updates if the list is empty and Resets if the list is not.
/// Intersections are stored in maps on the model and on the horizontal segments containing the jumps.
/// 
/// For Mode = Radiussed, the model will not be changed in this part of the code as it requires rendering modifications.async
/// 
/// For Mode = ModernCircuit, instead of returning the coordinates of Jumps, we want to make available the list of Wire split
/// coordinates. This list of XYPos will be used and converted to Render components of type circle marking the point a Wire splits.
let updateOrResetWireSegmentJumps (wireList: ConnectionId list) (wModel: Model) : Model =
    let startT = TimeHelpers.getTimeMs()                // StartT is not an interval
    let model = match wModel.Mode with
                | OldFashionedCircuit -> makeAllJumps wireList wModel
                | Radiussed -> wModel
                | ModernCircuit -> computeWireSplitCoord wModel   
    TimeHelpers.instrumentTime "UpdateJumps" startT     // print interval
    model


/// Re-routes the wires in the model based on a list of components that have been altered.
/// If the wire input and output ports are both in the list of moved components, does not re-route wire but instead translates it.
/// Keeps manual wires manual (up to a point).
/// Otherwise it will auto-route wires connected to components that have moved.
let updateWires (model : Model) (compIdList : ComponentId list) (diff : XYPos) =

    // Returns an anonymous record of: 
    // wires connected to inputs ONLY, wires connected to outputs ONLY, wires connected to both inputs and outputs
    let wiresConnectedToPorts = getWiresConnectedToPorts model compIdList
    let inputWires = wiresConnectedToPorts.InputWires
    let outputWires = wiresConnectedToPorts.OutputWires
    let InOutConnected = wiresConnectedToPorts.FullyConnectedWires

    let newWires = 
        model.WX
        |> Map.toList
        |> List.map (fun (connectionId, wire) ->
            match (List.contains connectionId InOutConnected), (List.contains connectionId inputWires), 
                  (List.contains connectionId outputWires) with
            | true, _ , _ -> (connectionId, moveWire wire diff)                       // Translate wires that are connected to moving components on both sides
            | false, true, _ -> (connectionId, updateWire model wire Input)           // Only route wires connected to ports that moved for efficiency
            | false, false, true -> (connectionId, updateWire model wire Output)
            | _ ,_ ,_ -> (connectionId, wire)
            )
        |> Map.ofList
        
    {model with WX = newWires}

/// Defines a list of functions that update the model and communicates with others with the use of Cmd messages
let update (msg : Msg) (model : Model) : Model*Cmd<Msg> =
    
    match msg with
    | Symbol sMsg ->
        let sm,sCmd = Symbol.update sMsg model.Symbol
        {model with Symbol=sm}, Cmd.map Symbol sCmd


    | UpdateWires (componentIdList, diff) -> 
        (updateWires model componentIdList diff, Cmd.none)

    | AddWire ( (inputId, outputId) : (InputPortId * OutputPortId) ) ->
        // Get the port positions
        let outputPortPos = Symbol.getOutputPortLocation model.Symbol outputId
        let inputPortPos = Symbol.getInputPortLocation model.Symbol inputId
        // Get the characteristics of the input and output symbols
        let outputSymbol = Symbol.getSymbolFromOutPortId model.Symbol outputId
        let outputSymbolRotation = int (outputSymbol.Rotation)
        let outputSymbolFlip = outputSymbol.SymbolCharacteristics.flip
        let inputSymbol = Symbol.getSymbolFromInPortId model.Symbol inputId
        let inputSymbolRotation = int (inputSymbol.Rotation)
        let inputSymbolFlip = inputSymbol.SymbolCharacteristics.flip
        let inputPortOnAltSide = Symbol.isPortOnAlternativeSide model.Symbol inputId

        let wireId = ConnectionId(JSHelpers.uuid())
        let segmentList = (makeInitialSegmentsList wireId outputPortPos inputPortPos outputSymbolRotation inputSymbolRotation outputSymbolFlip inputSymbolFlip inputPortOnAltSide)
        
        let newWire = 
            {
                Id = wireId
                InputPort = inputId
                OutputPort = outputId
                Color = HighLightColor.DarkSlateGrey
                Width = 1
                Segments = segmentList
            }
            
        let wireAddedMap = Map.add newWire.Id newWire model.WX
        let newModel = updateOrResetWireSegmentJumps [] {model with WX = wireAddedMap}

        newModel, Cmd.ofMsg BusWidths

    | BusWidths ->

        let processConWidths (connWidths: ConnectionsWidth) =
            let addWireWidthFolder (wireMap: Map<ConnectionId, Wire>) _ (wire:Wire)  =
                let width =
                    match connWidths[wire.Id] with
                    | Some a -> a
                    | None -> wire.Width
                let newColor =                                      
                    match wire.Color with
                    | Purple | Brown -> Purple
                    | _ -> DarkSlateGrey
                wireMap.Add ( wire.Id, { wire with                  
                                            Width = width ; 
                                            Color = newColor} )
                                                                                                                
            let addSymbolWidthFolder (mapSymbolId: Map<ComponentId,Symbol.Symbol>) (_: ConnectionId) (wire: Wire) =
                    let inPort = model.Symbol.Ports[match wire.InputPort with InputPortId ip -> ip]
                    let symId = ComponentId inPort.HostId
                    let symbol = mapSymbolId[symId]

                    match symbol.Component.Type with
                    | SplitWire _ ->        // Splitwire needs an argument int
                        match inPort.PortNumber with 
                        | Some 0 -> {symbol with InWidth0 = Some wire.Width}
                        | x -> failwithf $"What? wire found with input port {x} other than 0 connecting to SplitWire"
                        |> (fun sym -> Map.add symId sym mapSymbolId)
                    | ExtractWire _ ->        
                        match inPort.PortNumber with 
                        | Some 0 -> {symbol with InWidth0 = Some wire.Width}
                        | x -> failwithf $"What? wire found with input port {x} other than 0 connecting to ExtractWire"
                        |> (fun sym -> Map.add symId sym mapSymbolId)
                    | MergeWires ->
                        match inPort.PortNumber with
                        | Some 0 -> 
                            Map.add symId {symbol with InWidth0 = Some wire.Width} mapSymbolId
                        | Some 1 -> 
                            Map.add symId {symbol with InWidth1 = Some wire.Width} mapSymbolId
                        | x -> failwithf $"What? wire found with input port {x} other than 0 or 1 connecting to MergeWires"
                    | _ -> mapSymbolId


            let newWX = (Map.empty, model.WX) ||> Map.fold addWireWidthFolder
            let symbolsWithWidths =
                (model.Symbol.Symbols, newWX) ||> Map.fold addSymbolWidthFolder

            { model with 
                WX = newWX
                Notifications = None
                ErrorWires=[]
                Symbol = {model.Symbol with Symbols = symbolsWithWidths}}, Cmd.none        


        let canvasState = (Symbol.extractComponents model.Symbol, extractConnections model )
        
        
        match BusWidthInferer.inferConnectionsWidth canvasState with
        | Ok connWidths ->
            processConWidths connWidths
        | Error e ->
                { model with 
                    Notifications = Some e.Msg }, Cmd.ofMsg (ErrorWires e.ConnectionsAffected)
    

    | CopyWires (connIdList : list<ConnectionId>) ->
        let copiedWires = Map.filter (fun connId _ -> List.contains connId connIdList) model.WX
        { model with CopiedWX = copiedWires }, Cmd.none

    | ErrorWires (connectionIds : list<ConnectionId>) -> 
        let newWX =
            model.WX
            |> Map.map
                (fun id wire -> 
                    if List.contains id connectionIds then
                        {wire with Color = HighLightColor.Red}              // If he got detected as error this turn around
                    else if List.contains id model.ErrorWires then         
                        {wire with Color = HighLightColor.DarkSlateGrey}    // If he was in the error list but is not anymore switch back to default color
                    else 
                        wire                                                // Else all the wires that are not errors and that you don't need to modify their color                                                                 
                ) 
        
        { model with 
            WX = newWX
            ErrorWires = connectionIds}, Cmd.none

    //selects all wires in connectionIds, and also deselects all other wires
    | SelectWires (connectionIds : list<ConnectionId>) -> 
        let newWX =
            model.WX
            |> Map.map
                (fun id wire ->
                    match List.contains id model.ErrorWires, List.contains id connectionIds with
                    | true, true -> {wire with Color = HighLightColor.Brown} 
                    | true, false -> {wire with Color = HighLightColor.Red}
                    | false, true -> {wire with Color = HighLightColor.Purple}
                    | false, false -> {wire with Color = HighLightColor.DarkSlateGrey}
                )
        
        {model with WX = newWX}, Cmd.none

    // Create a new model without the wires with Id component contained in connectionIds (Reset it without given wires)
    | DeleteWires (connectionIds : list<ConnectionId>) -> 
        let resetWireModel = updateOrResetWireSegmentJumps (connectionIds) (model)    // Call the function to reset the model by filtering out certain wires
        let newWX =                                                                   // and removing the jumps they possibly had with other wires.
             resetWireModel.WX
             |> Map.filter (fun id _ -> not (List.contains id connectionIds))         
        {resetWireModel with WX = newWX}, Cmd.ofMsg BusWidths   

    | DragWire (connectionId : ConnectionId, mouse: MouseT) ->
        let newWX = match mouse.Op with                                               
                    | Down ->
                        let segId = getClickedSegment model connectionId mouse.Pos
                        {model with SelectedSegment = segId }
                    | Drag ->
                        let segId = model.SelectedSegment
                        let (seg:Segment) = match List.tryFind (fun (x:Segment) -> x.Id = segId) model.WX[connectionId].Segments with  
                                            | Some x -> x
                                            | None -> failwithf "segment Id not found in segment list"

                        if seg.Draggable then
                            let distanceToMove = 
                                match getOrientation seg with
                                | Horizontal -> mouse.Pos.Y -  seg.Start.Y
                                | Vertical -> mouse.Pos.X -  seg.Start.X
                                | Point -> 0.0

                            let newWire = moveSegment seg distanceToMove model
                            let newWX = Map.add seg.HostId newWire model.WX
            
                            {model with WX = newWX}
                        else
                            model
                    
                    | _ -> model

        newWX, Cmd.none

    // Just Changes the colour of the wires, Sheet calls pasteWires before this
    | ColorWires ((connIds: list<ConnectionId>), (color: HighLightColor)) -> 
        let newWiresMap =
            List.fold (fun oldWiresMap cId -> 
                            let oldWireOpt = Map.tryFind cId model.WX
                            match oldWireOpt with
                            | Some oldWire ->
                                Map.add cId { oldWire with Color = color } oldWiresMap
                            | None -> 
                                oldWiresMap      
                        ) model.WX connIds
        
        { model with WX = newWiresMap }, Cmd.none
    
    | ResetJumps connIds ->        
        let newModel =
            model
            |> updateOrResetWireSegmentJumps connIds    // Reset Wire Segment
        
        newModel, Cmd.none
    
    | MakeJumps _ ->
        let newModel =
            model
            |> updateOrResetWireSegmentJumps [] // Update Wire Segment
            
        newModel, Cmd.none
    
    | ResetModel -> { model with 
                        WX = Map.empty
                        ErrorWires = []
                        Notifications = None }, Cmd.none
    
    | LoadConnections (connections:Connection list) -> // we assume components (and hence ports) are loaded before connections
        let posMatchesVertex (pos:XYPos) (vertex: float*float) =
            let epsilon = 0.00001
            abs ( pos.X -  (fst vertex)) < epsilon && abs ( pos.Y -  (snd vertex)) < epsilon
        let newWX =
            connections 
            |> List.map ( fun connection ->
                            let inputId = InputPortId connection.Target.Id
                            let outputId = OutputPortId connection.Source.Id
                            let connId = ConnectionId connection.Id
                            let segments = issieVerticesToSegments connId connection.Vertices
                            let makeWirePosMatchSymbol (inOut:InOut) (wire:Wire) =
                                match inOut with
                                | Input -> posMatchesVertex 
                                            (Symbol.getInputPortLocation model.Symbol inputId)
                                            (List.head connection.Vertices)
                                | Output ->
                                          posMatchesVertex 
                                            (Symbol.getOutputPortLocation model.Symbol outputId) 
                                            (List.last connection.Vertices)
                                |> (fun b -> 
                                    if b then 
                                        wire 
                                    else
                                        let getS (connId:string) = 
                                            Map.tryFind connId model.Symbol.Ports
                                            |> Option.map (fun port -> port.HostId)
                                            |> Option.bind (fun symId -> Map.tryFind (ComponentId symId) model.Symbol.Symbols)
                                            |> Option.map (fun sym -> sym.Component.Label)
                                        updateWire model wire inOut)  
                                
                            connId,

                            { Id = ConnectionId connection.Id
                              InputPort = inputId
                              OutputPort = outputId
                              Color = HighLightColor.DarkSlateGrey
                              Width = 1
                              Segments = segments }

                            |> makeWirePosMatchSymbol Output
                            |> makeWirePosMatchSymbol Input
                        )
            |> Map.ofList
        
        let connIds =
            connections
            |> List.map (fun connection -> ConnectionId connection.Id)
            
        { model with WX = newWX }, Cmd.ofMsg (MakeJumps connIds)

    // This will allow you to use Ctrl+M to change modes. When pressing the keys it will trigger the match statement
    // and jump to the next mode. As well, it will call updateOrResetWireSegmentJumps (beign passed an empty list: Reset)
    // and reinitialise all the data transmitted to the rendering part of the code: setting both JumpCoordinateList and
    // SplitWireList to empty list.
    // 
    // This is set-up in the Sheet.fs and Renderer.fs files.
    | ChangeMode ->
        let wModel = model
        let prvmode = model.Mode
        let mode = match prvmode with
                    | OldFashionedCircuit -> Radiussed
                    | Radiussed -> ModernCircuit
                    | ModernCircuit -> OldFashionedCircuit
        let newWX = { wModel with Mode = mode }
        let resetWireModel = updateOrResetWireSegmentJumps [] (newWX)    // Reset Wire Segment
        
        resetWireModel, Cmd.none

    | ReRouteSymbol componentIdList ->
        // Returns an anonymous record of: 
        // wires connected to inputs ONLY, wires connected to outputs ONLY, wires connected to both inputs and outputs
        let wiresConnectedToPorts = getWiresConnectedToPorts model componentIdList
        let inputWires = wiresConnectedToPorts.InputWires
        let outputWires = wiresConnectedToPorts.OutputWires
        let InOutConnected = wiresConnectedToPorts.FullyConnectedWires
        
        // If a Wire is connected in some way to the components that have been rotated or flipped, re-autoroute them fully
        let newWires = 
            model.WX
            |> Map.toList
            |> List.map (fun (connectionId, wire) ->
                match (List.contains connectionId InOutConnected) || (List.contains connectionId inputWires)
                      || (List.contains connectionId outputWires) with
                | true -> (connectionId, autorouteWire model wire)
                | false -> (connectionId, wire)
                )
            |> Map.ofList
        
        // Return the model with the updated wires
        let newReRoutedWiresModel = {model with WX = newWires}
        newReRoutedWiresModel, Cmd.none

//---------------Other interface functions--------------------//

/// returns true when for wires inside the bounding box or hovered by mouse else false
let wireIntersectsBoundingBox (w : Wire) (boundBox : BoundingBox) =
    let boolList = List.map (fun seg -> isSegmentIntersectingBoundingBox seg boundBox) w.Segments
    List.contains true boolList


/// 
let getIntersectingWires (wModel : Model) (selectBox : BoundingBox) : list<ConnectionId> = 
    wModel.WX
    |> Map.map (fun id wire -> wireIntersectsBoundingBox wire selectBox)
    |> Map.filter (fun id boolVal -> boolVal)
    |> Map.toList
    |> List.map (fun (id, bool) -> id)

///searches if the position of the cursor is on a wire in a model
///Where n is 5 pixels adjusted for top level zoom
let getWireIfClicked (wModel : Model) (pos : XYPos) (n : float) : ConnectionId Option =
    let boundingBox = {X = pos.X - n
                       Y = pos.Y - n
                       H = n*2.
                       W = n*2.}
    let intersectingWires = getIntersectingWires (wModel : Model) boundingBox
    List.tryHead intersectingWires

///
let pasteWires (wModel : Model) (newCompIds : list<ComponentId>) : (Model * list<ConnectionId>) =
    let oldCompIds = Symbol.getCopiedSymbolsIds wModel.Symbol
    
    let pastedWires =
        let createNewWire (oldWire : Wire) : list<Wire> =
            let newId = ConnectionId(JSHelpers.uuid())
    
            match Symbol.getPastedPortsIdsFromCopiedPortsIds wModel.Symbol oldCompIds newCompIds (oldWire.InputPort, oldWire.OutputPort) with
            | Some (newInputPort, newOutputPort) ->
                // Get the port positions
                let outputPortPos = Symbol.getOutputPortLocation wModel.Symbol (OutputPortId newOutputPort)
                let inputPortPos = Symbol.getInputPortLocation wModel.Symbol (InputPortId newInputPort)
                // Get the characteristics of the input and output symbols
                let outputSymbol = Symbol.getSymbolFromOutPortId wModel.Symbol (OutputPortId newOutputPort)
                let outputSymbolRotation = int (outputSymbol.Rotation)
                let outputSymbolFlip = outputSymbol.SymbolCharacteristics.flip
                let inputSymbol = Symbol.getSymbolFromInPortId wModel.Symbol (InputPortId newInputPort)
                let inputSymbolRotation = int (inputSymbol.Rotation)
                let inputSymbolFlip = inputSymbol.SymbolCharacteristics.flip
                let inputPortOnAltSide = Symbol.isPortOnAlternativeSide wModel.Symbol (InputPortId newInputPort)

                let segmentList = (makeInitialSegmentsList newId outputPortPos inputPortPos outputSymbolRotation inputSymbolRotation outputSymbolFlip inputSymbolFlip inputPortOnAltSide)
                [
                    {
                        oldWire with
                            Id = newId;
                            InputPort = InputPortId newInputPort;
                            OutputPort = OutputPortId newOutputPort;
                            Segments = segmentList;
                    }
                ]
            | None -> []
        
        wModel.CopiedWX
        |> Map.toList
        |> List.map snd
        |> List.collect createNewWire
        |> List.map (fun wire -> wire.Id, wire)
        |> Map.ofList
    
    let newWireMap = Map.fold ( fun acc newKey newVal -> Map.add newKey newVal acc ) pastedWires wModel.WX
    let pastedConnIds =
        pastedWires
        |> Map.toList
        |> List.map fst
        
    { wModel with WX = newWireMap }, pastedConnIds