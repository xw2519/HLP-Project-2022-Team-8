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
    | RotatedSymbol of list<ComponentId>

/// Adds two XYPos together
let addPositions (a: XYPos) (b: XYPos) : XYPos =
    {X = a.X + b.X ; Y = a.Y + b.Y}

/// Returns the XYPos of the end of a Segment
let getEndPoint (segment: Segment) =
    {X=(segment.Start.X + segment.Vector.X);Y=(segment.Start.Y + segment.Vector.Y)}

/// Gets the orientation of a Segment
let getOrientation (segment : Segment) =
    if (segment.Vector.Y = 0.0) && (segment.Vector.X = 0.0) then Point 
    elif (segment.Vector.Y = 0.0) then Horizontal
    else Vertical

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
    let (segmentList: Segment list) = convertRISegsToSegments riWire.Id riWire.Start riWire.StartDir riWire.Segments

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



let makeInitialSegmentsList connId (startPort: XYPos) (endPort: XYPos) (startSymbolRotation: int) (endSymbolRotation: int) : Segment list =

    // adjust length of the first and last segments - the sticks - so that when two ports are aligned and close you still get left-to-right routing.
    let s = 
        let d = List.max [ abs (startPort.X - endPort.X) ; abs (startPort.Y - endPort.Y) ; Wire.stickLength / 4.0 ]
        if (endPort.X - startPort.X > 0.0) then
            min d (Wire.stickLength / 2.0)
        else
            Wire.stickLength / 2.0

    // Rotation of the symbols - Constants for DEMO
    //let startSymbolRotation = 0
    //let endSymbolRotation = 180      //CHANGE HERE MANUALLY

    // Rotation of the ports
    let startPortRotation = startSymbolRotation

    let endPortRotation =
        // modulo returns the remainder, but it is of the same sign as the first operand
        match ((endSymbolRotation - 180) % 360) with
        | x when (x < 0) -> x + 360
        | x -> x

    // Overall rotation of the wire
    let wireRotation = startPortRotation


    let differenceInX, differenceInY = (endPort.X - startPort.X), (endPort.Y - startPort.Y) 

    // Get the NORMALIZED differences between the X and Y coordinates of the ports
    let diffX, diffY =
        match wireRotation with
        | 0 -> differenceInX, differenceInY
        | 90 -> differenceInY, - differenceInX
        | 180 -> - differenceInX, - differenceInY
        | 270 -> - differenceInY, differenceInX
        | _ -> differenceInY, differenceInX

    let normalizedEndPortRotation = 
        match ((endPortRotation - wireRotation) % 360) with
        | x when (x < 0) -> x + 360
        | x -> x

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
    makeInitialSegmentsList connId WireVertices[0] WireVertices[WireVertices.Length - 1] 0 0
 
    
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

       
// EXTENSION: adapt that to horizontal as well
/// Called for segments 1, 2, 3, 4, 5 - if they are vertical and move horizontally.
/// The function returns distance reduced, if it needs to be, to prevent wires from moving into components.
// Note: approx equality test is safer tehn exact equality - but probably not needed.
let getSafeDistanceForMove (seg: Segment) (seg0:Segment) (segLast:Segment) (distance:float) =
    /// Check if 2 floats are notEqual according to some threshold
    let areFloatsNotEquals (x: float) (y: float) : bool =
        abs (abs x - abs y) > 0.0001

    let segEnd = getEndPoint seg
    let segLastEnd = getEndPoint segLast

    // Define a smaller shrink factor if the segment is not in the middle of the wire
    let shrink = match seg.Index with 
                 | 1 | 2 | 4 | 5 -> 0.5 
                 | 3 | _ -> 1.0 
                     
    // Then, according to the index
    match seg.Index with
    | 1 | 2 ->   
        //max distance you can do = X coordinate that you can't pass - where you are   (moving towards negative/smaller numbers)
        let minDistance = (seg0.Start.X + Wire.stickLength * shrink) - segEnd.X
        max minDistance distance
    | 4 | 5 ->
        let maxDistance = segLastEnd.X -  Wire.stickLength * shrink - seg.Start.X
        min maxDistance distance
    | 3 when distance < 0.0 && (areFloatsNotEquals seg0.Start.Y seg.Start.Y) ->    // If Seg3 is not merged with Seg0, can do anything
        distance
    | 3 when distance > 0.0 && (areFloatsNotEquals segLast.Start.Y segEnd.Y) ->      // If Seg3 is not merged with last Seg, can do anything
        distance
    | 3 ->
        let minDistance = seg0.Start.X + Wire.stickLength * shrink - seg.Start.X
        let maxDistance = segLastEnd.X -  Wire.stickLength * shrink - seg.Start.X
        distance
        |> max minDistance
        |> min maxDistance
    | _ -> 
        distance

/// Adjust wire (input type is Segment list) so that two adjacent horizontal segments that are in opposite directions
/// get eliminated
let removeRedundantSegments (segs: Segment list) =
    /// Set the X comp of the Start of the segment to 'x', keeping the sign
    let setStartX x (seg:Segment) = {seg with Start = {X = x ; Y = seg.Start.Y}}

    /// Set the X comp of the End of the segment to 'x', keeping the sign
    let setEndX x (seg:Segment) = {seg with Vector = {X = (x - seg.Start.X) ; Y = seg.Vector.Y}}

    /// Takes two segments, and if they are Horizontal and in opposite direction, "adjust" them
    let adjust seg1 seg2 =
        // Get their direction
        let xDirection1, xDirection2 = seg1.Vector.X, seg2.Vector.X
        // If they are horizontal and of opposite direction
        if (getOrientation seg1) = Horizontal &&
           (getOrientation seg2) = Horizontal && 
           sign xDirection1 <> sign xDirection2
        then
            // If the first segment is longer than the second one
            if abs xDirection1 > abs xDirection2 then
                // replace the end of segment 1 with the end of segment 2, and the start of segment 2 with its end (making it of length 0)
                [setEndX (getEndPoint seg2).X seg1; setStartX (getEndPoint seg2).X seg2]
            else
                // do the opposite
                [setEndX seg1.Start.X seg1; setStartX (getEndPoint seg1).X seg2]
        else
            // Otherwise, do nothing
            [seg1;seg2]
    
    // Adjust the first two, and last two, segments of a Wire's segments list
    adjust segs[0] segs[1] @  segs[2..(segs.Length - 3)] @ adjust segs[segs.Length - 2] segs[segs.Length - 1]
    

/// MANUAL ROUTING: 
/// This function allows a wire segment to be moved a given amount in a direction perpedicular to
/// its orientation (Horizontal or Vertical). Used to manually adjust routing by mouse drag.
/// The moved segment is tagged by negating one of its coordinates so that it cannot be auto-routed
/// after the move, thus keeping the moved position.
let moveSegment (seg:Segment) (distance:float) (model:Model) = 
    // Get the wire that the segment is in
    let wire = model.WX[seg.HostId]
    // Retrieve the position of the segment in the segment list of the wire
    let index = seg.Index
    // Check if the segment has a valid index, and is neither the first, nor the last segment
    if index <= 0 || index >= wire.Segments.Length - 1 then
        failwithf $"Buswire segment index {index} out of range in moveSegment in wire length {wire.Segments.Length}"
    // Get the previous and next segments
    let prevSeg = wire.Segments[index-1]
    let nextSeg = wire.Segments[index+1]
    // If the segment has the same direction than any of it's neighbours, don't do anything
    if (getOrientation seg) = (getOrientation prevSeg) || (getOrientation seg) = (getOrientation nextSeg) then
        wire
    // Else, move the segment, and update the neighbours
    else
        //runTestFable()
        distance      
        |> getSafeDistanceForMove seg wire.Segments[0] wire.Segments[wire.Segments.Length - 1]
        |> (fun distance' ->
            // Define the updated coordinates of the segments based on the orientation of the segment being moved
            let newPrevVector, newSegStart, newSegVector, newNextStart, newNextVector = 
                match (getOrientation seg) with
                // If it's vertical, update the X coordinates
                | Vertical -> 
                    //extract old XYPos, and change the X
                    {prevSeg.Vector with X = (prevSeg.Vector.X + distance')},
                    {seg.Start with X = (seg.Start.X + distance')}, 
                    {seg.Vector with X = (seg.Vector.X)},
                    {nextSeg.Start with X = (getEndPoint seg).X},
                    {nextSeg.Vector with X = (nextSeg.Vector.X - distance')}
                //If it's horizontal, update the Y coordinates
                | Horizontal -> 
                    //extract old XYPos, and change the Y
                    {prevSeg.Vector with Y = (prevSeg.Vector.Y + distance')},  
                    {seg.Start with Y = (seg.Start.Y + distance')}, 
                    {seg.Vector with Y = (seg.Vector.Y)},       
                    {nextSeg.Start with Y = ((getEndPoint seg).Y + distance')},
                    {nextSeg.Vector with Y = (nextSeg.Vector.Y - distance')}
                | _ -> 
                    // Wild card: don't change anything
                    prevSeg.Vector,
                    seg.Start,
                    seg.Vector,
                    nextSeg.Start,
                    nextSeg.Vector
            
            // Compile the new segments with updated XYPos Starts and Ends
            let newPrevSeg = {prevSeg with Vector = newPrevVector}
            let newSeg = {seg with Start = newSegStart ; Vector = newSegVector ; Autorouted = false}
            let newNextSeg = {nextSeg with Start = newNextStart ; Vector = newNextVector}
        
            // Rebuild the list of segments of the wire with the updated segments at the right indexes
            let newSegments =
                wire.Segments[.. index-2] @ [newPrevSeg; newSeg; newNextSeg] @ wire.Segments[index+2 ..]
                |> removeRedundantSegments

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
    // Get the rotation of the input and output symbols
    let outputSymbol = Symbol.getSymbolFromOutPortId model.Symbol wire.OutputPort
    let outputSymbolRotation = int (outputSymbol.Rotation)
    let inputSymbol = Symbol.getSymbolFromInPortId model.Symbol wire.InputPort
    let inputSymbolRotation = int (inputSymbol.Rotation)
    // Re-generate default Wire shape going from the InputPort to the OutputPort
    {wire with Segments = makeInitialSegmentsList wire.Id outputPortPos inputPortPos outputSymbolRotation inputSymbolRotation}

//
//  ====================================================================================================================
//
//                                        WIRE SEGMENTS FOR ROUTING
//
//
// Segments, going from Start (output port) to End (input port) coords, are summarised as:
// H => Horizontal (incr X)
// V => Vertical (incr Y)
// 0 => zero length segment (never used)
//
// segment qualifiers:
// F => min length (next to output or input, cannot be shortened)
//
// "Simple" case where output.X < input.X and 3 segment autoroute is possible
//  S0.FH  S1.0V  S2.H  S3.V  S4.H  S5.0V S6.FH
//
// "Complex" case where output.X > input.X and wire ends back for 5 segment autoroute
//  S0.FH  S1.V  S2.H  S3.V  S4.H  S5.0V S6.FH (not sure if H and V are correct here)
//
// To determine adjustment on End change we just reverse the segment and apply the Start change algorithm
// Adjustment => reverse list of segments, swap Start and End, and alter the sign of all coordinates
//
// ======================================================================================================================

(*/// Adds two XYPos together, (X1+X2, Y1+Y2) similarly to adding two vectors
let inline addPositions (pos1: XYPos) (pos:XYPos) =
    {X = pos1.X + pos.X; Y = pos1.Y + pos.Y}
*)

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

/// Reverse segment order, and Start, End coordinates, so list can be processed from input to output
/// this function is self-inverse
let revSegments (segs:Segment list) =
    List.rev segs
    |> List.map (fun seg -> {seg with Start = (getEndPoint seg) ; Vector = {X = - seg.Vector.X ; Y = - seg.Vector.Y}})


/// Returns None if full autoroute is required or Some segments with initial part of the segment list autorouted
/// up till the first dragged (manually routed) segment.
/// This always done in the order of the segment list, it needs to be reversed before calling the function
let tryPartialAutoRoute (segs: Segment list) (newPortPos: XYPos) =
    // Get where the first movable segment of the wire starts
    let wirePos = getEndPoint segs[0]
    // Get the previous portPos
    let portPos = segs[0].Start
    // Keep the same clearance from the port than previously, and move the first segment with the port
    let newWirePos = {newPortPos with X = newPortPos.X + (abs wirePos.X - portPos.X) }
    // Get by how much the port has moved
    let (diff:XYPos) = {X=newPortPos.X-portPos.X; Y= newPortPos.Y - portPos.Y}
    
    /// Returns the index of the first index that is manually routed
    let tryGetIndexOfFirstManuallyRoutedSegment =
        segs
        |> List.takeWhile (fun seg -> seg.Autorouted)   // Checks to see if a Segment is autorouted
        |> List.length
        |> (fun n -> if n > 5 then None else Some (n))

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

    /// Scale all the segments between the end of seg[0] and the end of the last manually routed segment.
    let scaleAutoroutedSegments segIndex =
        // Get the first segment that is manually routed (its End is fixed)
        let seg = segs[segIndex]
        // Get its fixed End's coordinates
        let fixedPt = getEndPoint seg
        // Scale the segments by the amount needed
        let scale x fx nx wx =
            if nx = fx then x else ((abs x - fx)*(nx-fx)/(abs wx - fx) + fx) * float (sign x)
        // Get the start of the wire that we are moving
        let startPos = if segIndex = 1 then portPos else wirePos
        let newStartPos = if segIndex = 1 then newPortPos else newWirePos
        // Curried functions for scaling the X and Y "lengths" of segments
        let scaleX x = scale x fixedPt.X newStartPos.X startPos.X
        let scaleY y = scale y fixedPt.Y newStartPos.Y startPos.Y
        // Extract the n+1 segments to be scaled
        match List.splitAt (segIndex+1) segs, segIndex with
        // If the first manually routed segment's index is 1
        | ((scaledSegs), otherSegs), 1 ->
            Some ((List.map (transformSeg scaleX scaleY) scaledSegs) @ otherSegs)
        // Otherwise, if we can extract the head, which is the segment connected to the moving port, we just translate it, and scale the other segs
        | ((firstSeg :: scaledSegs), otherSegs), _ ->
            Some ((moveAll (addPositions diff) 0 [firstSeg] @ List.map (transformSeg scaleX scaleY) scaledSegs) @ otherSegs)
        // If we can't, then return None to fully autoRoute everything
        | _ -> None


    tryGetIndexOfFirstManuallyRoutedSegment      //Get index: None if all segments are autorouted
    |> Option.bind checkTopologyChangeOption    //Check: None if we change quadrants (between moving endpoint and first manually fixed end)
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
        // Get the rotation of the input and output symbols
        let outputSymbol = Symbol.getSymbolFromOutPortId model.Symbol outputId
        let outputSymbolRotation = int (outputSymbol.Rotation)
        let inputSymbol = Symbol.getSymbolFromInPortId model.Symbol inputId
        let inputSymbolRotation = int (inputSymbol.Rotation)

        let wireId = ConnectionId(JSHelpers.uuid())
        let segmentList = makeInitialSegmentsList wireId outputPortPos inputPortPos outputSymbolRotation inputSymbolRotation
        
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

    | RotatedSymbol componentIdList ->

        // Returns an anonymous record of: 
        // wires connected to inputs ONLY, wires connected to outputs ONLY, wires connected to both inputs and outputs
        let wiresConnectedToPorts = getWiresConnectedToPorts model componentIdList
        let inputWires = wiresConnectedToPorts.InputWires
        let outputWires = wiresConnectedToPorts.OutputWires
        let InOutConnected = wiresConnectedToPorts.FullyConnectedWires
        
        // If a Wire is connected in some way to the components that have been rotated, re-autoroute them fully
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
        let newRotatedWiresModel = {model with WX = newWires}
        newRotatedWiresModel, Cmd.none

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
                // Get the rotation of the input and output symbols
                let outputSymbol = Symbol.getSymbolFromOutPortId wModel.Symbol (OutputPortId newOutputPort)
                let outputSymbolRotation = int (outputSymbol.Rotation)
                let inputSymbol = Symbol.getSymbolFromInPortId wModel.Symbol (InputPortId newInputPort)
                let inputSymbolRotation = int (inputSymbol.Rotation)

                let segmentList = makeInitialSegmentsList newId outputPortPos inputPortPos outputSymbolRotation inputSymbolRotation
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