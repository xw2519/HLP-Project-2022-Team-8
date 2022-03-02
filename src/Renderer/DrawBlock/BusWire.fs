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

/// 1 of 2 Orientations of a Wire (wires can only be horizontal or vertical in ISSIE)
type Orientation =  Horizontal | Vertical | Point

///
type SnapPosition = High | Mid | Low

type InOut = Input | Output

type RenderSegmentType = First | Second | Third | Middle | Antepenultimate | Penultimate | Last

type RenderJumpSegmentType = Before | InBetween | After | NoJump

type Modes = OldFashionedCircuit | ModernCircuit | Radiussed

/// Absolute-based segment, but with relative vectors denoting length and direction
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

/// Rotation-invariant representation of a segment: we know that all consecutive segments are perpendicular to each other
/// So no need to store individual orientations
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


/// Absolute Wire type using the redefined Segment type
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

/// Rotation-invariant Wire type using RISegments with an initial Start position and Start direction
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


/// Type that contains all elements regarding the current BusWire model
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

/// Message type to send updates accross the different component of Issie
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

//-------------------------Helper Functions---------------------------------//

/// Returns the XYPos of the end of a Segment
let getEndPoint (segment: Segment) =
    {X=(segment.Start.X + segment.Vector.X);Y=(segment.Start.Y + segment.Vector.Y)}

/// Gets the orientation of a Segment
let getOrientation (segment : Segment) =
    if (segment.Vector.Y = 0) && (segment.Vector.X = 0) then Point 
    elif (segment.Vector.Y = 0) then Horizontal
    else Vertical

/// Converts a list of RI segments to regular segments
let convertRISegsToSegments (hostId: ConnectionId)(startPos: XYPos) (startDir: int) (riSegs: RotationInvariantSeg list) : Segment list =
    
    let firstVector:XYPos = if (startDir = 90) || (startDir = 270) then {X=1;Y=0}
                            else {X=0;Y=1}
    
    let firstSeg:Segment = {Id= SegmentId(JSHelpers.uuid())
                            Index = -1
                            Start = startPos
                            Vector = firstVector
                            HostId = hostId
                            JumpCoordinateList = []
                            Autorouted = true
                            Draggable = false
                            }

    // Folder used to convert a RI segment into a regular Segment
    let convertToSeg (oldState:Segment) (element:RotationInvariantSeg):Segment  =
        // Current index of the segment
        let index = oldState.Index + 1
        // Compute the new start of the segment based on the old start + old vector
        let newStart = if (index = 0) then oldState.Start
                       else oldState.Start + oldState.Vector
        // Define the new vector based on orientation of the previous one
        let newBaseVector = if (oldState.Vector.X > 0)
                            then {X=0;Y=element.Length}
                            else {X=element.Length;Y=0}
        // Adjust the values of the vectors based on the current index and wire start orientation
        let newOrientedVector = match startDir with
                                | 0 -> newBaseVector
                                | 90 when ((index % 2) = 1) -> {newBaseVector with X = - newBaseVector.X ; Y = - newBaseVector.Y }
                                | 180 -> {newBaseVector with X = - newBaseVector.X ; Y = - newBaseVector.Y }
                                | 270 when ((index % 2) = 0) -> {newBaseVector with X = - newBaseVector.X ; Y = - newBaseVector.Y }
                                | _ -> newBaseVector
        
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
    let (segmentList:Segment list) = ((firstSeg, riSegs) ||> List.scan convertToSeg)
    // Return all but the head of the list
    match segmentList with
    | hd::tl -> tl
    | _ -> []

/// Converts a RotationInvariant Wire into a regular Wire
let convertToWire (riWire:RotationInvariantWire) : Wire =
    let (segmentList:Segment list) = convertRISegsToSegments riWire.Id riWire.Start riWire.StartDir riWire.Segments

    {
        Id= riWire.Id
        InputPort = riWire.InputPort
        OutputPort= riWire.OutputPort
        Color= riWire.Color
        Width= riWire.Width
        Segments= segmentList
    }

/// Converts a regular Wire into a RotationInvariant Wire
let convertToRIwire (wire:Wire) : RotationInvariantWire =

    let convertToSeg (element:Segment):RotationInvariantSeg  =
        let newLenth = if element.Vector.X = 0 
                       then element.Vector.Y
                       else element.Vector.X

        {
            Id = element.Id
            Length= newLenth
            HostId= wire.Id
            /// List of x-coordinate values of segment jumps. Only used on horizontal segments.
            JumpCoordinateList= element.JumpCoordinateList
            Draggable = element.Draggable
            Autorouted = true
        }
        

    let (segmentList:RotationInvariantSeg list) = wire.Segments |> List.map convertToSeg

    let newSegs = match segmentList with
                    | hd::tl -> tl
                    | _ -> []

    let vector = wire.Segments[0].Vector
    let StartDirection = if (vector.X > 0 && vector.Y = 0) then 0
                         elif (vector.X = 0 && vector.Y > 0) then 90
                         elif (vector.X < 0 && vector.Y = 0) then 180
                         else 270

    {
         Id = wire.Id
         InputPort = wire.InputPort
         OutputPort = wire.OutputPort
         Start = wire.Segments[0].Start
         StartDir = StartDirection
         Color = wire.Color
         Width = wire.Width
         Segments = newSegs
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
            sprintf $"Wire: {w.Segments |> List.collect (fun seg -> seg.JumpCoordinateList |> List.map (fun (f) -> ppSId))}")
            
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

//-------------------------------Implementation code----------------------------//


// Turns a list of vertices into a list of Segments
let xyVerticesToSegments connId (isLeftToRight: bool) (xyVerticesList: XYPos list) =
    // Put two endpoints together to form a segment
    List.pairwise xyVerticesList
    |> List.mapi (
        fun i ({X=startX; Y=startY},{X=endX; Y=endY}) ->    
            {
                Id = SegmentId(JSHelpers.uuid())
                Index = i
                Start = {X=startX;Y=startY};
                Vector = {X=(endX-startX);Y=(endY-startY)};
                HostId  = connId;
                JumpCoordinateList = [];
                Draggable =
                    match i with
                    | 1 | 5 -> isLeftToRight
                    | 0  | 6  -> false
                    | _ -> true
                Autorouted= true
            })

/// Given the coordinates of two port locations that correspond
/// to the endpoints of a wire, this function returns a list of
/// wire vertices
let makeInitialSegmentsList connId (portCoords : XYPos * XYPos)  =
    // Get the coordinates of the start port of the wire
    let xs, ys = snd(portCoords).X, snd(portCoords).Y
    // Get the coordinates of the end port of the wire
    let Xt, Yt = fst(portCoords).X, fst(portCoords).Y

    // adjust length of segments 0 and 6 - the sticks - so that when two ports are aligned and close you still get left-to-right routing.
    let adjStick = 
        let d = List.max [ abs (xs - Xt) ; abs (ys - Yt) ; Wire.stickLength / 4.0 ]
        if (Xt - xs > 0.0) then
            min d (Wire.stickLength / 2.0)
        else
            Wire.stickLength / 2.0

    // the simple case of a wire travelling from output to input in a left-to-right (positive X) direction
    let leftToRight = 
        [
            {X = xs; Y = ys};
            {X = xs+adjStick; Y = ys};
            {X = xs+adjStick; Y = ys};
            {X = (xs+Xt)/2.0; Y = ys};
            {X = (xs+Xt)/2.0; Y = Yt};
            {X = Xt-adjStick; Y = Yt}
            {X = Xt-adjStick; Y = Yt}
            {X = Xt; Y = Yt}
        ]
    // the case of a wire travelling from output to input in a right-to-left (negative X) direction. Thus must bend back on itself.
    let rightToLeft =
        [
            {X = xs; Y = ys}
            {X = xs+Wire.stickLength; Y = ys}
            {X = xs+Wire.stickLength; Y = ys}
            {X = xs+Wire.stickLength; Y = (ys+Yt)/2.0}
            {X = Xt-Wire.stickLength; Y = (ys+Yt)/2.0}
            {X = Xt-Wire.stickLength; Y = Yt}
            {X = Xt-Wire.stickLength; Y = Yt}
            {X = Xt; Y = Yt}
        ]

    // the special case of a wire travelling right-to-left where the two ends are vertically almost identical. 
    // In this case we ad an offset to the main horizontal segment so it is more visible and can be easily re-routed manually.
    let rightToLeftHorizontal =
        [
            {X = xs; Y = ys}
            {X = xs+Wire.stickLength; Y = ys}
            {X = xs+Wire.stickLength; Y = ys}
            {X = xs+Wire.stickLength; Y = ys + Wire.stickLength}
            {X = Xt-Wire.stickLength; Y = ys + Wire.stickLength}
            {X = Xt-Wire.stickLength; Y = Yt}
            {X = Xt-Wire.stickLength; Y = Yt}
            {X = Xt; Y = Yt}
        ]
    let vertlist, isLeftToRight =

        if Xt - xs >= adjStick * 2.0 then 
            leftToRight, true
        elif abs (ys - Yt) < 4.0 then 
            rightToLeftHorizontal, false
        else 
            rightToLeft, false 
                
    xyVerticesToSegments connId isLeftToRight vertlist


let inferDirectionfromVertices (xyVerticesList: XYPos list) =
    if xyVerticesList.Length <> 8 then 
        failwithf $"Can't perform connection type inference except with 8 vertices: here given {xyVerticesList.Length} vertices"
    let getDir (vs:XYPos) (ve:XYPos) =
        match sign ((abs vs.X - abs ve.X)*(abs vs.X - abs ve.X) - (abs vs.Y - abs ve.Y)*(abs vs.Y - abs ve.Y)) with
        | 1 -> Some Horizontal
        | -1 -> Some Vertical
        | _ -> None
    let midS, midE = xyVerticesList[3], xyVerticesList[4]
    let first,last = xyVerticesList[1], xyVerticesList[5]
    let xDelta = abs last.X - abs first.X
    match getDir midS midE, abs xDelta > 20.0, xDelta > 0.0 with
    | Some Horizontal, _, _ when midE.X < midS.X -> Some Horizontal
    | Some Vertical, _, _ -> Some Vertical 
    | _, true, true -> Some Vertical
    | _, true, false -> Some Horizontal
    | _, false, _ -> None

/// Convert a (possibly legacy) issie Connection stored as a list of vertices to Wire
let issieVerticesToSegments (connId) (verticesList: list<float*float>) =
    let xyVerticesList =
        verticesList
        |> List.map (fun (x,y) -> {X=x;Y=y})
     

    // segments lists must must be length 7, in case legacy vertex list does not conform check this
    // if there are problems reroute
        //vertex lists are one element longer than segment lists
    
    if xyVerticesList.Length <> 8 then  
        makeInitialSegmentsList connId (xyVerticesList[0], xyVerticesList[xyVerticesList.Length - 1])    
    else 
        match inferDirectionfromVertices xyVerticesList with
        | Some Vertical -> 
            printfn "Converting vertical"
            xyVerticesToSegments connId true xyVerticesList
        | Some Horizontal -> 
            printfn "Converting horizontal"
            xyVerticesToSegments connId false xyVerticesList
        | _ ->
            // can't work out what vertices are, so default to auto-routing
            printfn "Converting unknown"
            makeInitialSegmentsList connId (xyVerticesList[0], xyVerticesList[xyVerticesList.Length - 1])    
            
//END INTERFACE CONVERSIONS

//START MISC SEG FUNCTIONS
    
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
    } // We don't use vertices

/// This function is given a list of ConnectionId and it
/// converts the corresponding BusWire.Wire(s) to a
/// list of Connectio, offering an interface
/// between our implementation and Issie.
let extractConnections (wModel : Model) : list<Connection> = 
    wModel.WX
    |> Map.toList
    |> List.map (fun (key, _) -> extractConnection wModel key)


/// Given three points p, q, r, the function returns true if 
/// point q lies on line segment 'pr'. Otherwise it returns false.
let onSegment (p : XYPos) (q : XYPos) (r : XYPos) : bool = 
    (
        (q.X <= max (p.X) (r.X)) &&
        (q.X >= min (p.X) (r.X)) &&
        (q.Y <= max (p.Y) (r.Y)) &&
        (q.Y >= min (p.Y) (r.Y))
    )
  
/// Given three points p, q, r, the function returns:
/// - 0 if p, q and r are colinear;
/// - 1 if the path that you must follow when you start at p, you visit q and you end at r, is a CLOCKWISE path;
/// - 2 if the path that you must follow when you start at p, you visit q and you end at r, is a COUNTERCLOCKWISE path.
let orientation (p : XYPos) (q : XYPos) (r : XYPos) : int =
    let result = (q.Y - p.Y) * (r.X - q.X) - (q.X - p.X) * (r.Y - q.Y)
  
    if (result = 0.0) then 0 // colinear
    elif (result > 0.0) then 1 // clockwise
    else 2 //counterclockwise

///Returns the abs of an XYPos object
let getAbsXY (pos : XYPos) = 
    {X = abs pos.X; Y = abs pos.Y}
  
/// Given two sets of two points: (p1, q1) and (p2, q2)
/// that define two segments, the function returns true
/// if these two segments intersect and false otherwise.
let segmentIntersectsSegment ((p1, q1) : (XYPos * XYPos)) ((p2, q2) : (XYPos * XYPos)) : bool =
    // this is a terrible implementation
    // determining intersection should be done by finding intersection point and comparing with coords
    // since segments are always horizontal or vertical that is pretty easy.
    // in addition the way that coordinates can be positive or negative but are absed when used is appalling
    // the manual or auto route info per segment should be a separate field in Segmnet, not encoded in the sign of the coordinates
    // that is needed when writing out or reading from Issie, but the write/read process can easily translate to a sane internal data structure in the draw blokc model
    // let p1x,q1x,p2x,q2x = abs p1.X,abs q1.X,abs p2.X,abs q2.X
    // let p1y,q1y,p2y,q2y = abs p1.Y,abs q1.Y,abs p2.Y,abs q2.Y

    // let s = ((max q2x p2x) - (min q2x p2x),(max q2y p2y) - (min q2y p2y))
    // let r = ((max (abs q1.X) (abs p1.X)) - (min (abs q1.X) (abs p1.X)),(max (abs q1.Y) (abs p1.Y)) - (min (abs q1.Y) (abs p1.Y)))

    // let dbp = ((max (abs p1.X) (abs p2.X)) - (min (abs p1.X) (abs p2.X)),(max (abs p1.Y) (abs p2.Y)) - (min (abs p1.Y) (abs p2.Y)))
    
    // let rcrosss = fst(r)*snd(s) - snd(r)*fst(s)
    // let qminuspcrossr = fst(dbp)*snd(r) - snd(dbp)*fst(r)
    // //Four Cases

    // let seg1isHoriz = if ((abs q1.X = abs p1.X)&&(abs q1.Y <> abs p1.Y)) then 0
    //                   elif ((abs q1.Y = abs p1.Y)&&(abs q1.X <> abs p1.X)) then 1
    //                   //elif ((abs q1.Y = abs p1.Y)&&(abs q1.X = abs p1.X)) then true
    //                   else 2
    //                     //failwithf "neither Horizontal or Vertical1"

    // let seg2isHoriz = if ((abs q2.X = abs p2.X)&&(abs q2.Y <> abs p2.Y)) then 0 
    //                   elif ((abs q2.Y = abs p2.Y)&&(abs q2.X <> abs p2.X)) then 1
    //                   //elif ((abs q1.Y = abs p1.Y)&&(abs q1.X = abs p1.X)) then true
    //                   else 2
    //                     //failwithf "neither Horizontal or Vertical2"
    // //if ((min (abs p1.X) (abs q1.X) <p2.X< max (abs p1.X) (abs q1.X)) && (min (abs p1.Y) (abs q1.Y) <p2.Y< max (abs p1.Y) (abs q1.Y))) then true else false

    // let seg1length = sqrt ((abs q1.X - abs p1.X)**2 + (abs q1.Y - abs q1.Y)**2)
    // let seg2length = sqrt ((abs q2.X - abs p2.X)**2 + (abs q2.Y - abs q2.Y)**2)

    

    // //Colinear so true
    // if(seg1isHoriz=2) then if ((((min p2x q2x) <p1x) && (p1x< (max p2x  q2x))) && ((min p2y q2y <p1y) && (p1y< max p2y q2y))) then true else false

    // elif(seg2isHoriz=2) then if ((((min p1x q1x) <p2x) && (p2x< (max p1x  q1x))) && ((min (abs p1.Y) (abs q1.Y) <p2y) && (p2y< max (abs p1.Y) (abs q1.Y)))) then true else false

    // elif (rcrosss = 0 && qminuspcrossr = 0) then match seg1isHoriz with 
    //                                             | 1 -> if (p1.Y = p2.Y)&&((((max (abs p2.X) (abs q2.X)) >(max p1.X q1.X))&&((max p1.X q1.X)>(min p2.X q2.X)))||(((max p2.X q2.X) >(min p1.X q1.X))&&((min p1.X q1.X)> (min p2.X q2.X)))) then true else false
    //                                             | 0 ->  if (p1.X = p2.X)&&((((max (abs p2.Y) (abs q2.Y)) >(max p1.Y q1.Y))&&((max p1.Y q1.Y)>(min p2.Y q2.Y)))||(((max p2.Y q2.Y) >(min p1.Y q1.Y))&&((min p1.Y q1.Y)> (min p2.Y q2.Y)))) then true else false

    // //parallel non intersecting
    // elif (rcrosss = 0 && qminuspcrossr <> 0) then false
    // //non parallel intersecting
    // elif (rcrosss = 0) then match seg1isHoriz with 
    //                         | 1 -> match seg2isHoriz with 
    //                                   |1 -> failwithf "Unexpected area of Code"
    //                                   |0 -> let YRange = seg2length 
    //                                         let Ystart = min (abs p2.Y) (abs q2.Y)
    //                                         let XRange = seg1length 
    //                                         let Xstart = min (abs p1.X) (abs q1.X)
    //                                         if (((Ystart<p1.Y)&&(p1.Y<Ystart+YRange))&&((Xstart<p2.X)&&(p2.X<Xstart+XRange))) then true else false

    //                         |0 -> match seg2isHoriz with 
    //                                   |1 -> let YRange = seg1length 
    //                                         let Ystart = min (abs p1.Y) (abs q1.Y)
    //                                         let XRange = seg2length 
    //                                         let Xstart = min (abs p2.X) (abs q2.X)
    //                                         if (((Ystart<p2.Y)&&(p2.Y<Ystart+YRange))&&((Xstart<p1.X)&&(p1.X<Xstart+XRange))) then true else false
    //                                   |0 ->  failwithf "Unexpected area of Code"

    // //non parallel non intersecting
    // else false

    //true
    

    let p1,q1,p2,q2= getAbsXY p1, getAbsXY q1, getAbsXY p2, getAbsXY q2
    // Find the four orientations needed for general and 
    // special cases 
    let o1 = orientation (p1) (q1) (p2)
    let o2 = orientation (p1) (q1) (q2)
    let o3 = orientation (p2) (q2) (p1)
    let o4 = orientation (p2) (q2) (q1)
  
    // General case 
    if (o1 <> o2 && o3 <> o4)
        then true

    // Special Cases 
    // p1, q1 and p2 are colinear and p2 lies on segment p1q1 
    elif (o1 = 0 && onSegment (p1) (p2) (q1))
        then true
  
    // p1, q1 and q2 are colinear and q2 lies on segment p1q1 
    elif (o2 = 0 && onSegment (p1) (q2) (q1))
        then true
  
    // p2, q2 and p1 are colinear and p1 lies on segment p2q2 
    elif (o3 = 0 && onSegment (p2) (p1) (q2))
        then true
  
     // p2, q2 and q1 are colinear and q1 lies on segment p2q2 
    elif (o4 = 0 && onSegment (p2) (q1) (q2))
        then true
    else false



///Returns a segment with positive Start and End coordinates
let makeSegPos (seg : Segment) =
    {seg with
        Start = getAbsXY seg.Start }

/// Given the coordinates of two port locations that correspond
/// to the endpoints of a wire, this function returns a list of
/// Segment(s).
// let makeInitialSegmentsList (hostId : ConnectionId) (portCoords : XYPos * XYPos) : list<Segment> =
//     PortCoordstoSegList hostId portCoords

//END MISC SEG FUNCTIONS

//START RENDER AND VIEW

/// This function renders the given
/// segment (i.e. creates a ReactElement
/// using the data stored inside it),
/// using the colour and width properties given.
let renderRadiusedWire (segmentList: Segment List) (colour : string) (width : string) (numofSegments:int) : ReactElement List = 
    
    let renderSegment (segment : Segment) (colour : string) (width : string) (index:int) (numofSegments:int) : ReactElement =

        let rendertype = if ( index = 0) then First
                         elif (index = 1) then Second
                         elif (index = 2) then Third
                         elif (index = numofSegments - 3) then Antepenultimate
                         elif (index = numofSegments - 2) then Penultimate
                         elif (index = numofSegments - 1) then Last
                         else Middle
        
        let nextSegment = if(index <> (segmentList.Length-1))
                          then segmentList[index+1]
                          else segmentList[index]

        let prevSegment = if(index <> 0)
                          then segmentList[index-1]
                          else segmentList[index]

        let wOpt = EEExtensions.String.tryParseWith System.Int32.TryParse width
        let renderWidth = 
            match wOpt with
            | Some 1 -> 1.5
            | Some n when n < int "8" -> 2.5
            | _ -> 3.5
        let halfWidth = (renderWidth/2.0) - (0.75)
        let lineParameters = { defaultLine with Stroke = colour; StrokeWidth = string renderWidth }
        let circleParameters = { defaultCircle with R = halfWidth; Stroke = colour; Fill = colour }
        let radiusSize = 12.0
        let SegDirection = getOrientation segment
        let NextSegDirection = getOrientation nextSegment
        let PrevSegDirection = getOrientation prevSegment
        if SegDirection = Horizontal then
            let pathParameters = { defaultPath with Stroke = colour; StrokeWidth = string renderWidth }

            let renderWireSubSegment (vertex1 : XYPos) (vertex2 : XYPos) (renderJumpSeg:RenderJumpSegmentType): list<ReactElement> =
                let Xa, Ya, Xb, Yb = vertex1.X, vertex1.Y, vertex2.X, vertex2.Y
                let pathParameters = { defaultPath with Stroke = colour; StrokeWidth = string renderWidth }
                let segmentJumpHorizontalSize = 30.0

                let isseg1LeftToRight = if( Xb - Xa > 0) then true
                                        else false
                
                if ((rendertype=Penultimate)&&(NextSegDirection=Horizontal)&&(SegDirection=Horizontal)&&(abs(Xb-Xa) < 20)&&(abs((getEndPoint nextSegment).X-nextSegment.Start.X) < 20)) then 
                                                                                                                [makeLine  Xb Ya Xb Yb lineParameters]

                
                elif ((rendertype=Last)&&(PrevSegDirection=Horizontal)&&(abs(Xb-Xa) < 20)) then 
                                                        [makeLine  Xb Ya Xb Yb lineParameters]

                elif ((abs((getEndPoint nextSegment).Y-nextSegment.Start.Y) < 20.0)&&(rendertype<>Last)&&(rendertype<>First)&&(abs((getEndPoint prevSegment).Y-prevSegment.Start.Y) > radiusSize)) then 
                                                        [makeLine (Xa+radiusSize) Ya Xb Yb lineParameters]

                elif ((abs((getEndPoint nextSegment).Y-nextSegment.Start.Y) < 20.0)&&(rendertype<>Last)&&(rendertype<>First)&&(abs((getEndPoint prevSegment).Y-prevSegment.Start.Y) < radiusSize)) then 
                                                        [makeLine  Xa Ya Xb Yb lineParameters]

                elif ((rendertype=Second)&&(PrevSegDirection=Horizontal)&&(abs(Xb-Xa) < 8)) then 
                                                                                                                let isDowntoUp = if( (getEndPoint nextSegment).Y - nextSegment.Start.Y < 0) then true
                                                                                                                                 else false
                                                                                                                if (isDowntoUp = true) then 
                                                                                                                                    let startingPoint = {X = Xb - radiusSize ; Y = Yb}
                                                                                                                                    let startingControlPoint = {X = Xb ; Y = Yb }
                                                                                                                                    let endingControlPoint = {X = Xb ; Y = Yb - segmentJumpHorizontalSize/2.0 }
                                                                                                                                    let endingPoint = {X = Xb; Y = Yb - segmentJumpHorizontalSize/2.}
                                                                                                                                    [makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]
                                                                                                                else 
                                                                                                                let startingPoint = {X = Xb - radiusSize ; Y = Yb}
                                                                                                                let startingControlPoint = {X = Xb ; Y = Yb }
                                                                                                                let endingControlPoint = {X = Xb ; Y = Yb + segmentJumpHorizontalSize/2.0 }
                                                                                                                let endingPoint = {X = Xb; Y = Yb + segmentJumpHorizontalSize/2.}
                                                                                                                [makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]

                
                elif ((rendertype=First)&&(NextSegDirection=Horizontal)&&(abs((getEndPoint nextSegment).X-nextSegment.Start.X) < 8)) then 
                                                        [makeLine  Xa Ya (Xa+5.0) Yb lineParameters]

                

                
                elif ((abs((getEndPoint prevSegment).Y-prevSegment.Start.Y) < 20.0)&&(rendertype<>Last)&&(rendertype<>First)) then 
                                                                                                                let isDowntoUp = if( (getEndPoint nextSegment).Y - nextSegment.Start.Y < 0) then true
                                                                                                                                 else false
                                                                                                                if (isDowntoUp = true) then 
                                                                                                                                    let startingPoint = {X = Xb - radiusSize ; Y = Yb}
                                                                                                                                    let startingControlPoint = {X = Xb ; Y = Yb }
                                                                                                                                    let endingControlPoint = {X = Xb ; Y = Yb - segmentJumpHorizontalSize/2.0 }
                                                                                                                                    let endingPoint = {X = Xb; Y = Yb - segmentJumpHorizontalSize/2.}
                                                                                                                                    [makeLine Xa Ya (Xb - radiusSize) Yb lineParameters;  makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]
                                                                                                                else 
                                                                                                                let startingPoint = {X = Xb - radiusSize ; Y = Yb}
                                                                                                                let startingControlPoint = {X = Xb ; Y = Yb }
                                                                                                                let endingControlPoint = {X = Xb ; Y = Yb + segmentJumpHorizontalSize/2.0 }
                                                                                                                let endingPoint = {X = Xb; Y = Yb + segmentJumpHorizontalSize/2.}
                                                                                                                [makeLine Xa Ya (Xb - radiusSize) Yb lineParameters;  makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]


                elif ((rendertype = First)&&(abs((getEndPoint nextSegment).Y-nextSegment.Start.Y) < radiusSize)) then
                                                                                                            [makeLine Xa Ya Xb Yb lineParameters]

                elif ((rendertype = First)&&(abs((getEndPoint nextSegment).Y-nextSegment.Start.Y) > radiusSize)) then
                                                                                                            let startingPoint = {X = Xb - radiusSize ; Y = Yb}
                                                                                                            let startingControlPoint = {X = Xb ; Y = Yb }
                                                                                                            let endingControlPoint = {X = Xb ; Y = Yb - segmentJumpHorizontalSize/2.0 }
                                                                                                            let endingPoint = {X = Xb; Y = Yb - segmentJumpHorizontalSize/2.}
                                                                                                                                    
                                                                                                            [makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]

                
                elif ((rendertype = Third)&&(abs((getEndPoint prevSegment).Y-prevSegment.Start.Y) > radiusSize)) then                                                            
                                                                                                            let isDowntoUp = if( (getEndPoint nextSegment).Y - nextSegment.Start.Y < 0) then true
                                                                                                                                else false
                                                                                                            if (isDowntoUp = true) then 
                                                                                                                                    let startingPoint = {X = Xb - radiusSize ; Y = Yb}
                                                                                                                                    let startingControlPoint = {X = Xb ; Y = Yb }
                                                                                                                                    let endingControlPoint = {X = Xb ; Y = Yb - segmentJumpHorizontalSize/2.0 }
                                                                                                                                    let endingPoint = {X = Xb; Y = Yb - segmentJumpHorizontalSize/2.}
                                                                                                                                    [makeLine (Xa + radiusSize) Ya (Xb - radiusSize) Yb lineParameters;  makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]
                                                                                                            else 
                                                                                                            let startingPoint = {X = Xb - radiusSize ; Y = Yb}
                                                                                                            let startingControlPoint = {X = Xb ; Y = Yb }
                                                                                                            let endingControlPoint = {X = Xb ; Y = Yb + segmentJumpHorizontalSize/2.0 }
                                                                                                            let endingPoint = {X = Xb; Y = Yb + segmentJumpHorizontalSize/2.}
                                                                                                            [makeLine (Xa + radiusSize) Ya (Xb - radiusSize) Yb lineParameters;  makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]

                elif ((rendertype = Third)&&(abs((getEndPoint prevSegment).Y-prevSegment.Start.Y) < radiusSize)) then                                                            
                                                                                                            let isDowntoUp = if( (getEndPoint nextSegment).Y - nextSegment.Start.Y < 0) then true
                                                                                                                                else false
                                                                                                            if (isDowntoUp = true) then 
                                                                                                                                    let startingPoint = {X = Xb - radiusSize ; Y = Yb}
                                                                                                                                    let startingControlPoint = {X = Xb ; Y = Yb }
                                                                                                                                    let endingControlPoint = {X = Xb ; Y = Yb - segmentJumpHorizontalSize/2.0 }
                                                                                                                                    let endingPoint = {X = Xb; Y = Yb - segmentJumpHorizontalSize/2.}
                                                                                                                                    [makeLine Xa Ya (Xb - radiusSize) Yb lineParameters;  makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]
                                                                                                            else 
                                                                                                            let startingPoint = {X = Xb - radiusSize ; Y = Yb}
                                                                                                            let startingControlPoint = {X = Xb ; Y = Yb }
                                                                                                            let endingControlPoint = {X = Xb ; Y = Yb + segmentJumpHorizontalSize/2.0 }
                                                                                                            let endingPoint = {X = Xb; Y = Yb + segmentJumpHorizontalSize/2.}
                                                                                                            [makeLine Xa Ya (Xb - radiusSize) Yb lineParameters;  makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]

                
                elif ((rendertype = Antepenultimate)&&(abs((getEndPoint nextSegment).Y-nextSegment.Start.Y) > radiusSize)) then                                                            
                                                                                                            let isDowntoUp = if( (getEndPoint nextSegment).Y - nextSegment.Start.Y < 0) then true
                                                                                                                                else false
                                                                                                            if (isDowntoUp = true) then 
                                                                                                                                    let startingPoint = {X = Xb - radiusSize ; Y = Yb}
                                                                                                                                    let startingControlPoint = {X = Xb ; Y = Yb }
                                                                                                                                    let endingControlPoint = {X = Xb ; Y = Yb - segmentJumpHorizontalSize/2.0 }
                                                                                                                                    let endingPoint = {X = Xb; Y = Yb - segmentJumpHorizontalSize/2.}
                                                                                                                                    [makeLine (Xa + radiusSize) Ya (Xb - radiusSize) Yb lineParameters;  makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]
                                                                                                            else 
                                                                                                            let startingPoint = {X = Xb - radiusSize ; Y = Yb}
                                                                                                            let startingControlPoint = {X = Xb ; Y = Yb }
                                                                                                            let endingControlPoint = {X = Xb ; Y = Yb + segmentJumpHorizontalSize/2.0 }
                                                                                                            let endingPoint = {X = Xb; Y = Yb + segmentJumpHorizontalSize/2.}
                                                                                                            [makeLine (Xa + radiusSize) Ya (Xb - radiusSize) Yb lineParameters;  makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]

                elif ((rendertype = Antepenultimate)&&(abs((getEndPoint nextSegment).Y-nextSegment.Start.Y) < radiusSize)) then                                                            
                                                                                                            let isDowntoUp = if( (getEndPoint nextSegment).Y - nextSegment.Start.Y < 0) then true
                                                                                                                                else false
                                                                                                            if (isDowntoUp = true) then 
                                                                                                                                    let startingPoint = {X = Xb - radiusSize ; Y = Yb}
                                                                                                                                    let startingControlPoint = {X = Xb ; Y = Yb }
                                                                                                                                    let endingControlPoint = {X = Xb ; Y = Yb - segmentJumpHorizontalSize/2.0 }
                                                                                                                                    let endingPoint = {X = Xb; Y = Yb - segmentJumpHorizontalSize/2.}
                                                                                                                                    [makeLine (Xa+radiusSize) Ya Xb Yb lineParameters;]
                                                                                                            else 
                                                                                                            let startingPoint = {X = Xb - radiusSize ; Y = Yb}
                                                                                                            let startingControlPoint = {X = Xb ; Y = Yb }
                                                                                                            let endingControlPoint = {X = Xb ; Y = Yb + segmentJumpHorizontalSize/2.0 }
                                                                                                            let endingPoint = {X = Xb; Y = Yb + segmentJumpHorizontalSize/2.}
                                                                                                            [makeLine (Xa+radiusSize) Ya Xb Yb lineParameters;]



                elif ((rendertype = Last)&&(abs((getEndPoint prevSegment).Y-prevSegment.Start.Y) > radiusSize)) then                                                            
                                                                                                            //[makeLine (Xa+radiusSize) Ya Xb Yb lineParameters;]
                                                                                                            [makeLine (Xa+radiusSize) Ya Xb Yb lineParameters;]
                
                elif ((rendertype = Last)&&(abs((getEndPoint prevSegment).Y-prevSegment.Start.Y) < radiusSize)) then                                                            
                                                                                                            [makeLine Xa Ya Xb Yb lineParameters;]





                elif ((NextSegDirection = Vertical) && (rendertype = Middle) && (isseg1LeftToRight = true)) then
                                                                                                                let isDowntoUp = if( (getEndPoint nextSegment).Y - nextSegment.Start.Y < 0) then true
                                                                                                                                 else false
                                                                                                                if (isDowntoUp = true) then 
                                                                                                                                    let startingPoint = {X = Xb - radiusSize ; Y = Yb}
                                                                                                                                    let startingControlPoint = {X = Xb ; Y = Yb }
                                                                                                                                    let endingControlPoint = {X = Xb ; Y = Yb - segmentJumpHorizontalSize/2.0 }
                                                                                                                                    let endingPoint = {X = Xb; Y = Yb - segmentJumpHorizontalSize/2.}
                                                                                                                                    [makeLine (Xa + radiusSize) Ya (Xb - radiusSize) Yb lineParameters;  makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]
                                                                                                                else 
                                                                                                                let startingPoint = {X = Xb - radiusSize ; Y = Yb}
                                                                                                                let startingControlPoint = {X = Xb ; Y = Yb }
                                                                                                                let endingControlPoint = {X = Xb ; Y = Yb + segmentJumpHorizontalSize/2.0 }
                                                                                                                let endingPoint = {X = Xb; Y = Yb + segmentJumpHorizontalSize/2.}
                                                                                                                [makeLine (Xa + radiusSize) Ya (Xb - radiusSize) Yb lineParameters;  makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]

                elif((NextSegDirection = Vertical) && (rendertype = Middle) && (isseg1LeftToRight = false)) then 
                                                                                                            let isDowntoUp = if( (getEndPoint nextSegment).Y - nextSegment.Start.Y < 0) then true
                                                                                                                                else false
                                                                                                            if (isDowntoUp = true) then 
                                                                                                                                    let startingPoint = {X = Xb + radiusSize ; Y = Yb}
                                                                                                                                    let startingControlPoint = {X = Xb ; Y = Yb }
                                                                                                                                    let endingControlPoint = {X = Xb ; Y = Yb - segmentJumpHorizontalSize/2.0 }
                                                                                                                                    let endingPoint = {X = Xb ; Y = Yb - segmentJumpHorizontalSize/2.}
                                                                                                                                    [makeLine (Xa - radiusSize) Ya (Xb + radiusSize) Yb lineParameters;  makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]
                                                                                                            else 
                                                                                                            let startingPoint = {X = Xb + radiusSize ; Y = Yb}
                                                                                                            let startingControlPoint = {X = Xb ; Y = Yb }
                                                                                                            let endingControlPoint = {X = Xb ; Y = Yb + segmentJumpHorizontalSize/2.0 }
                                                                                                            let endingPoint = {X = Xb; Y = Yb + segmentJumpHorizontalSize/2.}                      
                                                                                                            [makeLine (Xa - radiusSize) Ya (Xb + radiusSize) Yb lineParameters;  makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]

              
                else
                [makeLine Xa Ya Xb Yb lineParameters]
                // ::
                // makeCircle Xa Ya circleParameters
                // ::
                // [
                //     makeCircle Xb Yb circleParameters
                // ]
            
            let segmentJumpHorizontalSize = 9.0
            let segmentJumpVerticalSize = 6.0
            
            let renderSingleSegmentJump (intersectionCoordinate : XYPos) : list<ReactElement> =
                let x, y = intersectionCoordinate.X, intersectionCoordinate.Y

                let startingPoint = {X = x - segmentJumpHorizontalSize/2.0; Y = y}
                let startingControlPoint = {X = x - segmentJumpHorizontalSize/2.0; Y = y - segmentJumpVerticalSize}
                let endingControlPoint = {X = x + segmentJumpHorizontalSize/2.0; Y = y - segmentJumpVerticalSize}
                let endingPoint = {X = x + segmentJumpHorizontalSize/2.0; Y = y}

                makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters
                ::
                makeCircle startingPoint.X startingPoint.Y circleParameters
                ::
                [
                    makeCircle endingPoint.X endingPoint.Y circleParameters
                ]
            
            let rec renderMultipleSegmentJumps (segmentJumpCoordinateList : list<float>) (segmentJumpYCoordinate : float) : list<ReactElement> =
                
                match segmentJumpCoordinateList with

                | [] -> []


                | [singleElement] ->
                    renderSingleSegmentJump {X = singleElement; Y = segmentJumpYCoordinate}


                | firstElement :: secondElement :: tailList ->

                    if (segment.Start.X > (getEndPoint segment).X) then
                        renderSingleSegmentJump {X = firstElement; Y = segmentJumpYCoordinate}
                        @
                        renderWireSubSegment {X = firstElement - segmentJumpHorizontalSize/2.0; Y = segmentJumpYCoordinate} {X = secondElement + segmentJumpHorizontalSize/2.0; Y = segmentJumpYCoordinate} InBetween
                        @
                        renderMultipleSegmentJumps (secondElement :: tailList) (segmentJumpYCoordinate)
                    
                    else
                        renderSingleSegmentJump {X = firstElement; Y = segmentJumpYCoordinate}
                        @
                        renderWireSubSegment {X = firstElement + segmentJumpHorizontalSize/2.0; Y = segmentJumpYCoordinate} {X = secondElement - segmentJumpHorizontalSize/2.0; Y = segmentJumpYCoordinate} InBetween
                        @
                        renderMultipleSegmentJumps (secondElement :: tailList) (segmentJumpYCoordinate)
                

            let completeWireSegmentRenderFunction (seg : Segment) : list<ReactElement> =
                
                let jumpCoordinateList = []
                
                match jumpCoordinateList with
                    | [] -> renderWireSubSegment seg.Start (getEndPoint seg) NoJump

                    | lst ->
                        let y = seg.Start.Y // SHOULD be equal to seg.End.Y since ONLY horizontal segments have jumps
                        let firstSegmentJumpCoordinate = lst[0]
                        let lastSegmentJumpCoordinate = lst[(List.length lst) - 1]

                        if (segment.Start.X > (getEndPoint segment).X) then
                            renderWireSubSegment seg.Start {X = firstSegmentJumpCoordinate + segmentJumpHorizontalSize/2.0; Y = y} Before
                            @
                            renderMultipleSegmentJumps lst y
                            @
                            renderWireSubSegment {X = lastSegmentJumpCoordinate - segmentJumpHorizontalSize/2.0; Y = y} (getEndPoint seg) After

                        else
                            renderWireSubSegment seg.Start {X = firstSegmentJumpCoordinate - segmentJumpHorizontalSize/2.0; Y = y} Before
                            @
                            renderMultipleSegmentJumps lst y
                            @
                            renderWireSubSegment {X = lastSegmentJumpCoordinate + segmentJumpHorizontalSize/2.0; Y = y} (getEndPoint seg) After
            

            let wireSegmentReactElementList = segment
                                            |> completeWireSegmentRenderFunction

            g [] wireSegmentReactElementList
        
        else

            

            let Xa, Ya, Xb, Yb = segment.Start.X, segment.Start.Y, (getEndPoint segment).X, (getEndPoint segment).Y
            let isseg1DowntoUp = if( Yb - Ya < 0) then true
                                 else false

            let pathParameters = { defaultPath with Stroke = colour; StrokeWidth = string renderWidth }
            let segmentJumpHorizontalSize = 30.0
            let radiusSize = 12.0
            

            let segmentElements = 
                //[makeLine Xa (Ya - radiusSize) Xb (Yb + radiusSize) lineParameters; makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]
                // if((Yb - Ya < 25.0)&&(Yb - Ya > -25.0)) then 
                //                       printfn "%A" (snd(segmentTuple))
                //                       [makeLine Xa Ya Xb Yb lineParameters]
                if ((abs(Yb-Ya) < 20.0)) then 
                                            [makeLine Xa Ya Xb Yb lineParameters]

            
                elif (NextSegDirection = Vertical) then 
                                                        //printfn "%A" (snd(segmentTuple))
                                                        [makeLine Xa Ya Xb Yb lineParameters]
                elif ((rendertype = Second)&&(abs(Yb-Ya) < radiusSize)) then
                                                [makeLine Xa Ya Xb Yb lineParameters]
                                                

                elif ((rendertype = Penultimate)&&(abs(Yb-Ya) < radiusSize)) then
                                                [makeLine Xa Ya Xb Yb lineParameters]

                elif ((rendertype = Penultimate)&&(abs(Yb-Ya) > radiusSize)) then
                                                                                    let startingPoint = {X = Xb ; Y = Yb + radiusSize}
                                                                                    let startingControlPoint = {X = Xb ; Y = Yb }
                                                                                    let endingControlPoint = {X = Xb + segmentJumpHorizontalSize/2.0; Y = Yb  }
                                                                                    let endingPoint = {X = Xb + segmentJumpHorizontalSize/2.0; Y = Yb}
                                                                                    [makeLine Xa (Ya - radiusSize) Xb (Yb + radiusSize) lineParameters;  makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]


                elif ((NextSegDirection = Horizontal) && (isseg1DowntoUp = true)) then
                                                                                                        let isLeftToRight = if( ((getEndPoint nextSegment).X - nextSegment.Start.X) >= 0) then true
                                                                                                                            else false
                                                                                                        if (isLeftToRight = true) then 
                                                                                                                                let startingPoint = {X = Xb ; Y = Yb + radiusSize}
                                                                                                                                let startingControlPoint = {X = Xb ; Y = Yb }
                                                                                                                                let endingControlPoint = {X = Xb + segmentJumpHorizontalSize/2.0; Y = Yb  }
                                                                                                                                let endingPoint = {X = Xb + segmentJumpHorizontalSize/2.0; Y = Yb}
                                                                                                                                [makeLine Xa (Ya - radiusSize) Xb (Yb + radiusSize) lineParameters;  makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]
                                                                                                        else 
                                                                                                        let startingPoint = {X = Xb ; Y = Yb + radiusSize}
                                                                                                        let startingControlPoint = {X = Xb ; Y = Yb }
                                                                                                        let endingControlPoint = {X = Xb - segmentJumpHorizontalSize/2.0; Y = Yb  }
                                                                                                        let endingPoint = {X = Xb - segmentJumpHorizontalSize/2.0; Y = Yb}
                                                                                                        [makeLine Xa (Ya - radiusSize) Xb (Yb + radiusSize) lineParameters;  makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]

                elif((NextSegDirection = Horizontal) && (isseg1DowntoUp = false)) then 
                                                                                                        let isLeftToRight = if( (getEndPoint nextSegment).X - nextSegment.Start.X >= 0) then true
                                                                                                                            else false
                                                                                                        if (isLeftToRight = true) then 
                                                                                                                                let startingPoint = {X = Xb ; Y = Yb - radiusSize}
                                                                                                                                let startingControlPoint = {X = Xb ; Y = Yb }
                                                                                                                                let endingControlPoint = {X = Xb + segmentJumpHorizontalSize/2.0; Y = Yb  }
                                                                                                                                let endingPoint = {X = Xb + segmentJumpHorizontalSize/2.0; Y = Yb}
                                                                                                                                [makeLine Xa (Ya + radiusSize) Xb (Yb - radiusSize) lineParameters;  makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]
                                                                                                        else 
                                                                                                        let startingPoint = {X = Xb ; Y = Yb - radiusSize}
                                                                                                        let startingControlPoint = {X = Xb ; Y = Yb }
                                                                                                        let endingControlPoint = {X = Xb - segmentJumpHorizontalSize/2.0; Y = Yb  }
                                                                                                        let endingPoint = {X = Xb - segmentJumpHorizontalSize/2.0; Y = Yb}
                                                                                                        [makeLine Xa (Ya + radiusSize) Xb (Yb - radiusSize) lineParameters;  makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters]

                else[makeLine Xa (Ya - radiusSize) Xb (Yb + radiusSize) lineParameters]
                
            g [] segmentElements

    segmentList |> List.mapi ( fun i (segment : Segment) -> renderSegment segment colour width i (segmentList.Length:int);)


let renderOldFashionedorModernWire (segmentList: Segment List) (colour : string) (width : string) (numofSegments:int) (displayMode:Modes) (circleList:XYPos List) : ReactElement List = 


    let renderSegment (segment : Segment) (colour : string) (width : string) : ReactElement = 
        let wOpt = EEExtensions.String.tryParseWith System.Int32.TryParse width
        let renderWidth = 
            match wOpt with
            | Some 1 -> 1.5
            | Some n when n < int "8" -> 2.5
            | _ -> 3.5
        let halfWidth = (renderWidth/2.0) - (0.75)
        let lineParameters = { defaultLine with Stroke = colour; StrokeWidth = string renderWidth }
        let circleParameters = { defaultCircle with R = halfWidth; Stroke = colour; Fill = colour }
        let SegmentDirection = getOrientation segment
        if SegmentDirection = Horizontal then
            let pathParameters = { defaultPath with Stroke = colour; StrokeWidth = string renderWidth }

            let renderWireSubSegment (vertex1 : XYPos) (vertex2 : XYPos) : list<ReactElement> =
                let Xa, Ya, Xb, Yb = vertex1.X, vertex1.Y, vertex2.X, vertex2.Y
                makeLine Xa Ya Xb Yb lineParameters
                ::
                makeCircle Xa Ya circleParameters
                ::
                [
                    makeCircle Xb Yb circleParameters
                ]
            
            let segmentJumpHorizontalSize = 9.0
            let segmentJumpVerticalSize = 6.0
            
            let renderSingleSegmentJump (intersectionCoordinate : XYPos) : list<ReactElement> =
                let x, y = intersectionCoordinate.X, intersectionCoordinate.Y

                let startingPoint = {X = x - segmentJumpHorizontalSize/2.0; Y = y}
                let startingControlPoint = {X = x - segmentJumpHorizontalSize/2.0; Y = y - segmentJumpVerticalSize}
                let endingControlPoint = {X = x + segmentJumpHorizontalSize/2.0; Y = y - segmentJumpVerticalSize}
                let endingPoint = {X = x + segmentJumpHorizontalSize/2.0; Y = y}

                makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters
                ::
                makeCircle startingPoint.X startingPoint.Y circleParameters
                ::
                [
                    makeCircle endingPoint.X endingPoint.Y circleParameters
                ]
            
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
                

            let completeWireSegmentRenderFunction (seg : Segment) : list<ReactElement> =
                
                let jumpCoordinateList =
                    match displayMode with 
                                                | OldFashionedCircuit ->  if (segment.Start.X > (getEndPoint segment).X) then
                                                                            seg.JumpCoordinateList
                                                                            |> List.sortDescending
                                                
                                                                          else
                                                                            seg.JumpCoordinateList
                                                                            |> List.sort
                                                | ModernCircuit -> []

                                                | Radiussed -> failwithf "unexpected area of code"
                                                

                                            
                
                match jumpCoordinateList with
                    | [] -> renderWireSubSegment seg.Start (getEndPoint segment)

                    | lst ->
                        let y = seg.Start.Y // SHOULD be equal to seg.End.Y since ONLY horizontal segments have jumps
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
            

            let wireSegmentReactElementList = segment
                                            |> completeWireSegmentRenderFunction

            g [] wireSegmentReactElementList
        
        else
            let Xa, Ya, Xb, Yb = segment.Start.X, segment.Start.Y, (getEndPoint segment).X, (getEndPoint segment).Y
            let segmentElements = 
                makeLine Xa Ya Xb Yb lineParameters
                ::
                makeCircle Xa Ya circleParameters
                ::
                [
                    makeCircle Xb Yb circleParameters
                ]
            g [] segmentElements


    let segmentElements = segmentList |> List.mapi ( fun i (segment : Segment) -> renderSegment segment colour width;)

    let renderCirclesList = match displayMode with 
                                    | OldFashionedCircuit -> []
                                    | ModernCircuit -> circleList
                                    | Radiussed -> failwithf "unexpected area of code"


    let ModerncircleParameters = { defaultCircle with R = 6.0; Stroke = colour; Fill = colour }

    let circleElements = renderCirclesList |> List.mapi ( fun i (circlepos : XYPos) -> makeCircle (abs circlepos.X) (abs circlepos.Y) ModerncircleParameters;)

    segmentElements @ circleElements


let RenderWire (segmentList: Segment List) (colour : string) (width : string) (numofSegments:int) (displayMode:Modes) (circleList:XYPos List) : ReactElement List = 
    match displayMode with 
                        |OldFashionedCircuit -> renderOldFashionedorModernWire segmentList colour width numofSegments OldFashionedCircuit circleList : ReactElement List
                        |ModernCircuit -> renderOldFashionedorModernWire segmentList colour width numofSegments ModernCircuit circleList : ReactElement List
                        |Radiussed -> renderRadiusedWire segmentList colour width numofSegments : ReactElement List

///
type WireRenderProps =
    {
        key: string
        Segments: list<Segment>
        ColorP: HighLightColor
        DisplayType: Modes
        SplitWireList : XYPos List
        StrokeWidthP: int
        OutputPortLocation: XYPos
    }


// ------------------------------redundant wire memoisation code------------------------------
// this code is not used because React (via Function.Of) does this caching anyway - better tha it can be
// done here
let mutable cache:Map<string,WireRenderProps*ReactElement> = Map.empty

/// not used
let memoOf (f: WireRenderProps -> ReactElement, _, _) =
    (fun props ->
        match Map.tryFind props.key cache with
        | None -> 
            let re = f props
            cache <- Map.add props.key (props,re) cache 
            re
        | Some (props',re) ->  
            if props' = props then re else
                let re = f props
                cache <- Map.add props.key (props,re) cache
                re)
//-------------------------------------------------------------------------------------------

let singleWireView = 
    FunctionComponent.Of(
        fun ((props: WireRenderProps)) ->
            printfn "%A" props.Segments
            let SegmentList = props.Segments
            let renderWireSegmentList : list<ReactElement> = RenderWire SegmentList (props.ColorP.Text()) (string props.StrokeWidthP) (props.Segments.Length:int) props.DisplayType props.SplitWireList
                
            
            let renderWireWidthText : ReactElement =
                let textParameters =
                    {
                        TextAnchor = "left";
                        FontSize = "12px";
                        FontWeight = "Bold";
                        FontFamily = "Verdana, Arial, Helvetica, sans-serif";
                        Fill = props.ColorP.Text();
                        UserSelect = UserSelectOptions.None;
                        DominantBaseline = "middle";
                    }
                let textString = if props.StrokeWidthP = 1 then "" else string props.StrokeWidthP //Only print width > 1
                makeText (props.OutputPortLocation.X+1.0) (props.OutputPortLocation.Y-7.0) (textString) textParameters
            g [] ([ renderWireWidthText ] @ renderWireSegmentList)
        
    , "Wire"
    , equalsButFunctions
    )

///
let MapToSortedList (map: Map<ConnectionId,Wire>) : Wire list = 
    let listSelected = 
        Map.filter (fun id (wire:Wire) -> wire.Color = HighLightColor.Purple) map
        |> Map.toList
        |> List.map snd
    let listErrorSelected =
        Map.filter (fun id (wire:Wire) -> wire.Color = HighLightColor.Brown) map
        |> Map.toList
        |> List.map snd
    let listErrorUnselected =
        Map.filter (fun id (wire:Wire) -> wire.Color = HighLightColor.Red) map
        |> Map.toList
        |> List.map snd
    let listUnSelected = 
        Map.filter (fun id (wire:Wire) -> wire.Color = HighLightColor.DarkSlateGrey) map
        |> Map.toList
        |> List.map snd
    let listCopied = 
        Map.filter (fun id (wire:Wire) -> wire.Color = HighLightColor.Thistle) map
        |> Map.toList
        |> List.map snd
    let listWaves = 
        Map.filter (fun id (wire:Wire) -> wire.Color = HighLightColor.Blue) map
        |> Map.toList
        |> List.map snd

    listUnSelected @ listErrorUnselected @ listErrorSelected @ listSelected @ listWaves @ listCopied
   
let view (model : Model) (dispatch : Dispatch<Msg>) =
    let start = TimeHelpers.getTimeMs()
    let wires1 =
        model.WX
        |> Map.toArray
        |> Array.map snd
    TimeHelpers.instrumentTime "WirePropsSort" start
    let rStart = TimeHelpers.getTimeMs()
    let wires =
        wires1
        |> Array.map
            (
                fun wire ->
                    let stringOutId =
                        match wire.OutputPort with
                        | OutputPortId stringId -> stringId
                        
                    let outputPortLocation = Symbol.getOnePortLocationNew model.Symbol stringOutId PortType.Output
                    let props =
                        {
                            key = match wire.Id with | ConnectionId s -> s
                            Segments = List.map makeSegPos wire.Segments
                            DisplayType = model.Mode
                            SplitWireList = model.SplitWireList
                            ColorP = wire.Color
                            StrokeWidthP = wire.Width
                            OutputPortLocation = outputPortLocation
                        }
                    singleWireView props)
    TimeHelpers.instrumentInterval "WirePrepareProps" rStart ()
    let symbols = Symbol.view model.Symbol (Symbol >> dispatch)
 
    g [] [(g [] wires); symbols]
    |> TimeHelpers.instrumentInterval "WireView" start

//END RENDER AND VIEW




//------------------------------------------------------------------------------------//
//----------------------TLP19 CODE SECTION STARTS-------------------------------------//
//------------------------------------------------------------------------------------//



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
let removeRedundantSegments  (segs: Segment list) =                                 // TODO: rewrite and fix this function
/// Set the absolute value of X (keeping the previously assigned sign)
(*let setAbsX x (pos: XYPos) =
    let x = if pos.X < 0.0 then - abs x else abs x
    {pos with X = x}
*)
    /// Get difference in X along a segment
    let xDelta seg = 
        let segEnd = getEndPoint seg
        segEnd.X - seg.Start.X //abs segEnd.X - abs seg.Start.X

    /// Set the X comp of the Start of the segment to 'x', keeping the sign
    let setStartX x (seg:Segment) = {seg with Start = {X = x ; Y = seg.Start.Y}}

    /// Set the X comp of the End of the segment to 'x', keeping the sign
    let setEndX x (seg:Segment) = 
        //let newX = x - seg.Start.X
        //{seg with Vector = setAbsX newX seg.Vector}
        let segEnd = getEndPoint seg
        let newSegEnd = {segEnd with X=x}
        {seg with Vector = {X = (newSegEnd.X - seg.Start.X) ; Y = (newSegEnd.Y - seg.Start.Y)}}

    /// Takes two segments, and if they are Horizontal and in opposite direction, "adjust" them
    let adjust seg1 seg2 =
        // Get their direction
        let xd1, xd2 = xDelta seg1, xDelta seg2
        // If they are horizontal and of opposite direction
        if (getOrientation seg1) = Horizontal && 
           (getOrientation seg2) = Horizontal && 
           sign xd1 <> sign xd2 
        then
            // If the first segment is longer than the second one
            if abs xd1 > abs xd2 then
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
    

// MANUAL ROUTING: ENTRY POINT TO THIS CODE SECTION
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
    let inputPorts, outputPorts = Symbol.getPortLocations wModel.Symbol compIds
    wModel.WX
    |> Map.toList
    |> List.map snd
    |> List.filter (fun wire -> Map.containsKey wire.InputPort inputPorts || Map.containsKey wire.OutputPort outputPorts)
    |> List.map (fun wire -> wire.Id)
    |> List.distinct


///Returns a tuple of IDs of: wires connected to inputs ONLY, wires connected to outputs ONLY, wires connected to both inputs and outputs
let getWiresConnectedToPorts (wModel : Model) (compIds : list<ComponentId>) =
        // Get the maps of input and output ports from specific symbols in the model
        let inputPorts, outputPorts = Symbol.getPortLocations wModel.Symbol compIds
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


// AUTOMATIC FULL AUTOROUTE
/// Returns a new fully autorouted wire given a model and wire
let autorouteWire (model : Model) (wire : Wire) : Wire =
    let posTuple = Symbol.getTwoPortLocations (model.Symbol) (wire.InputPort) (wire.OutputPort)
    // Re-generate default Wire shape going from the InputPort to the OutputPort
    {wire with Segments = makeInitialSegmentsList wire.Id posTuple}

/// Reverse segment order, and Start, End coordinates, so list can be processed from input to output
/// this function is self-inverse
let revSegments (segs:Segment list) =
    List.rev segs
    |> List.map (fun seg -> {seg with Start = (getEndPoint seg) ; Vector = {X = - seg.Vector.X ; Y = - seg.Vector.Y}})

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

/// Adds two XYPos together, (X1+X2, Y1+Y2) similarly to adding two vectors
let inline addPositions (pos1: XYPos) (pos:XYPos) =
    {X = pos1.X + pos.X; Y = pos1.Y + pos.Y}

/// Move the End of the Nth segment according to the suplied 'mover' function
let inline moveEnd (mover: XYPos -> XYPos) (n:int) =
    List.mapi (fun i (seg:Segment) -> if i = n then {seg with Vector = (mover (getEndPoint seg)) - seg.Start} else seg)

/// Move the Start of the Nth segment according to the suplied 'mover' function
let inline moveStart (mover: XYPos -> XYPos) (n:int) =
    List.mapi (fun i (seg:Segment) -> 
            let prevEnd = getEndPoint seg
            if i = n then {seg with Start = mover seg.Start ; Vector = (prevEnd - mover seg.Start)} else seg
        )

/// Move both the Start and the End of the Nth segment according to the suplied 'mover' function (applied on both)
let inline moveAll (mover: XYPos -> XYPos) (n : int) =
    List.mapi (fun i (seg:Segment) -> if i = n then {seg with Start = mover seg.Start} else seg)    //No need to move the vector

/// Given 2 functions and a point, apply the first one on the X coordinate of the point, and the second on the Y coordinate
let  transformXY tX tY (pos: XYPos) =
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





//----------------------------------------------------------------------------------//
//----------------------TLP19 CODE SECTION ENDS-------------------------------------//
//----------------------------------------------------------------------------------//





/// Updates the JumpCoordinateList component of every segment in the model
/// To do so, it creates a grid of all possible wire combinations and check for intersection
/// If it locates one, it will update the list
/// While dragged, the jumps of the wire will be reset to 0 and recomputed when released
let makeAllJumps (wiresWithNoJumps: ConnectionId list) (model: Model) =
    // Arrays are faster to check than lists
    let wiresWithNoJumpsA = List.toArray wiresWithNoJumps

    // Gather all segments of the Model in an Array
    let segs =
        model.WX
        |> Map.toArray
        |> Array.map (fun (_, w) -> List.toArray w.Segments)
        |> Array.concat

   
    // Split the array of all segments into two arrays: one for all Horizontals and one for all Verticals
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


    // Create a Gird associating all elements of the two arrays 
    // It will result in an array of all possible combination of Horizontal and Vertical segments
    let makeHoriVertiGrid (horizontalVerticalSegs: Segment array * Segment array) =
        let horizontalArray = fst horizontalVerticalSegs
        let verticalArray = snd horizontalVerticalSegs

        let makePair a b =
            (a,b)

        let makeColumn (lst: Segment array) (x:Segment) =
            Array.map (makePair x) lst
        Array.map (makeColumn verticalArray) horizontalArray
            |> Array.concat                                 
    


    // Check if the elements in each pair intersect, while respecting the condition (</> 5)
    // If they do, append to the list of Jumps a tuple of the HorizontalId and the distance
    // to the start the intersection occurs
    let registerAllJumps (hvGrid:(Segment*Segment) array)=
        let checkCrossing ((segHrz:Segment), (segVrt:Segment)) =
            let vrtX = abs segVrt.Start.X
            let hrzY = abs segHrz.Start.Y 

            let maxMin x y =
                (max x y , min x y)

            let (xhi, xlo) = maxMin (abs segHrz.Start.X) (abs (getEndPoint segHrz).X)
            let (yhi, ylo) = maxMin (abs segVrt.Start.Y) (abs (getEndPoint segVrt).Y)
           
            if vrtX < xhi - 5.0 && vrtX > xlo + 5.0 && hrzY < yhi - 5.0 && hrzY > ylo + 5.0 then
                [|(segHrz.Id,vrtX)|]
            else [||]

        Array.map checkCrossing hvGrid
            |> Array.collect id 


    // Take in an array of jumps and iterate through every Wires and then every segments in 
    // the model. If the segment id is contained in the Jump key array, then we update its 
    // JumpCoordinateList component otherwise we reset the component as it no longer/doesn't
    // have any segments intersecting itself
    // Moreover, we check that the Value array of Jump contains distinct elements in order to
    // render it correctly on the Convas.
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
            let updateSeg = changeSegment (WX[connectionId].Segments)
            {WX[connectionId] with Segments = updateSeg}

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

    // Create a Grid of all possible combination of Wires in the Model
    let makeWireGrid (wireList:(ConnectionId*Wire) list) =
        let makePair a b =
            (a,b)

        let makeColumn (lst: (ConnectionId*Wire) list) (x:ConnectionId*Wire)=
            List.map (makePair x) lst
        List.map (makeColumn wireList) wireList
            |> List.concat  
 
    // Finds the cicle coordinates by iterating through all the segments and finding the
    // the separation. The return value will depend on the structure:
    // if the Vectors are of opposite direction then we give out the origin point
    // if the are pointing in the same direction, we render the end point of the shortest vector
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

    // Takes a pair of wires and check out if they have the same OutputPort id but different InputPort id
    // The second test is to avoid the case where we have twice the same wire and thus no splitting point
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


    // remove all none from the option list & keep all distinct some values
    let XYPos = makeWireGrid (Map.toList WX) 
               |> List.map getPorts
               |> List.choose id 
               |> List.distinct
    
    { model with SplitWireList= XYPos }


// Update at said interval the jumps arrays

/// This function updates the wire model by removing from the stored lists of intersections
/// all those generated by wireList wires. It updates if the list is empty and Resets if the list is not.
/// Intersections are stored in maps on the model and on the horizontal segments containing the jumps.

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
/// Otherwise it will auto-route wires connected to components that have moved
let updateWires (model : Model) (compIdList : ComponentId list) (diff : XYPos) =

    ///Returns a tuple of: wires connected to inputs ONLY, wires connected to outputs ONLY, wires connected to both inputs and outputs
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
            | true, _ , _ -> (connectionId, moveWire wire diff)                      //Translate wires that are connected to moving components on both sides
            | false, true, _ -> (connectionId, updateWire model wire Input)           //Only route wires connected to ports that moved for efficiency
            | false, false, true -> (connectionId, updateWire model wire Output)
            | _ ,_ ,_ -> (connectionId, wire)
            )
        |> Map.ofList
        
    {model with WX = newWires}


let update (msg : Msg) (model : Model) : Model*Cmd<Msg> =
    
    match msg with
    | Symbol sMsg ->
        let sm,sCmd = Symbol.update sMsg model.Symbol
        {model with Symbol=sm}, Cmd.map Symbol sCmd


    | UpdateWires (componentIdList, diff) -> 
        (updateWires model componentIdList diff, Cmd.none)

    | AddWire ( (inputId, outputId) : (InputPortId * OutputPortId) ) ->
        let portOnePos, portTwoPos = Symbol.getTwoPortLocations model.Symbol inputId outputId
        let wireId = ConnectionId(JSHelpers.uuid())
        let segmentList = makeInitialSegmentsList wireId (portOnePos, portTwoPos)
        
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
                let newColor =                                      // Done: if wire.Color = Purple || wire.Color = Brown then Purple else DarkSlateGrey       
                    match wire.Color with
                    | Purple | Brown -> Purple
                    | _ -> DarkSlateGrey
                wireMap.Add ( wire.Id, { wire with                  // formatting
                                            Width = width ; 
                                            Color = newColor} )
                                                                                                                // change name m to map or else
            let addSymbolWidthFolder (mapSymbolId: Map<ComponentId,Symbol.Symbol>) (_: ConnectionId) (wire: Wire) =       // _ as no need for the key in map.fold
                    let inPort = model.Symbol.Ports[match wire.InputPort with InputPortId ip -> ip]
                    let symId = ComponentId inPort.HostId
                    let symbol = mapSymbolId[symId]

                    match symbol.Compo.Type with
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
            // TODO: rename type Model.Symbol in Model.SymbolModel
        


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
                (fun id wire ->                     // convert to match doesn't really makes sense more complexe
                    match List.contains id connectionIds, List.contains id model.ErrorWires with
                    | true , _ -> {wire with Color = HighLightColor.Red}                // If he got detected as error this turn around
                    | false, true -> {wire with Color = HighLightColor.DarkSlateGrey}   // If he was in the error list but is not anymore switch back to default color
                    | _ , _ -> wire                                                     // Else all the wires that are not errors and that you don't need to modify their color                        
                ) 
        
        { model with 
            WX = newWX
            ErrorWires = connectionIds}, Cmd.none

    | SelectWires (connectionIds : list<ConnectionId>) -> //selects all wires in connectionIds, and also deselects all other wires
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

// Create a new model without the wires contained in connectionIds
    | DeleteWires (connectionIds : list<ConnectionId>) -> 
        let resetWireModel = updateOrResetWireSegmentJumps (connectionIds) (model)    // Reset Wire Segment
        let newWX =
             resetWireModel.WX
             |> Map.filter (fun id _ -> not (List.contains id connectionIds))     // TODO: change the wire not used to a _
        {resetWireModel with WX = newWX}, Cmd.ofMsg BusWidths

    | DragWire (connectionId : ConnectionId, mouse: MouseT) ->
        match mouse.Op with                                             // TODO: Change to return cmd.none at the end only once | use newModel = match ... | newModel, cmd.none
        | Down ->
            let segId = getClickedSegment model connectionId mouse.Pos
            {model with SelectedSegment = segId }, Cmd.none
        | Drag ->
            let segId = model.SelectedSegment
            let rec getSeg (segList: list<Segment>) =           // TODO: change from rec to a library function call like list.filter or Map.tryfind
                match segList with
                | h::t -> if h.Id = segId then h else getSeg t
                | _ -> failwithf "segment Id not found in segment list"
        (*    match result with
            | Some x -> printfn "Found an element: %d" x
            | None -> printfn "Failed to find a matching element."  *)
            let seg = getSeg model.WX[connectionId].Segments

            if seg.Draggable then
                let distanceToMove = 
                    match getOrientation seg with
                    | Horizontal -> mouse.Pos.Y - abs seg.Start.Y
                    | Vertical -> mouse.Pos.X - abs seg.Start.X
                    | Point -> 0.0

                let newWire = moveSegment seg distanceToMove model
                let newWX = Map.add seg.HostId newWire model.WX
 
                {model with WX = newWX}, Cmd.none
            else
                model, Cmd.none
        
        | _ -> model, Cmd.none


    | ColorWires ((connIds: list<ConnectionId>), (color: HighLightColor)) -> // Just Changes the colour of the wires, Sheet calls pasteWires before this
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
    
    | LoadConnections connections -> // we assume components (and hence ports) are loaded before connections
        let posMatchesVertex (pos:XYPos) (vertex: float*float) =
            let epsilon = 0.00001
            abs (abs pos.X - abs (fst vertex)) < epsilon && abs (abs pos.Y - abs (snd vertex)) < epsilon
        let newWX =
            connections 
            |> List.map ( fun connection ->
                            let inputId = InputPortId connection.Target.Id
                            let outputId = OutputPortId connection.Source.Id
                            let connId = ConnectionId connection.Id
                            let segments = issieVerticesToSegments connId connection.Vertices
                            let makeWirePosMatchSymbol inOut (wire:Wire) =
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
                                            |> Option.map (fun sym -> sym.Compo.Label)
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
    
    | ChangeMode ->
        let prvmode = model.Mode
        let mode = match prvmode with
                    | OldFashionedCircuit -> Radiussed
                    | Radiussed -> ModernCircuit
                    | ModernCircuit -> OldFashionedCircuit
        let wModel = model
        let newWX = { wModel with Mode = mode }
        printfn $"{mode}"
        let resetWireModel = updateOrResetWireSegmentJumps [] (newWX)    // Reset Wire Segment
        
        resetWireModel, Cmd.none

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
    let oldCompIds = Symbol.getCopiedSymbols wModel.Symbol
    
    let pastedWires =
        let createNewWire (oldWire : Wire) : list<Wire> =
            let newId = ConnectionId(JSHelpers.uuid())
    
            match Symbol.getEquivalentCopiedPorts wModel.Symbol oldCompIds newCompIds (oldWire.InputPort, oldWire.OutputPort) with
            | Some (newInputPort, newOutputPort) ->

                let portOnePos, portTwoPos = Symbol.getTwoPortLocations wModel.Symbol (InputPortId newInputPort) (OutputPortId newOutputPort)
                let segmentList = makeInitialSegmentsList newId (portOnePos, portTwoPos)
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

/// not used in the code: was used in sheets though
let getPortIdsOfWires (model: Model) (connIds: ConnectionId list) : (InputPortId list * OutputPortId list) =
    (([], []), connIds)
    ||> List.fold (fun (inputPorts, outputPorts) connId ->
            (model.WX[connId].InputPort :: inputPorts, model.WX[connId].OutputPort :: outputPorts))
