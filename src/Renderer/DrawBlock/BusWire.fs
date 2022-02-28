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
type Orientation =  Horizontal | Vertical

/// ?
type SnapPosition = High | Mid | Low

/// Default type for Segments, using Coordinates for both start- and end-points of segments
type Segment = 
    {
        Id : SegmentId
        Index: int
        Start: XYPos
        End: XYPos
        Dir: Orientation
        HostId: ConnectionId
        /// List of x-coordinate values of segment jumps. Only used on horizontal segments.
        JumpCoordinateList: list<float * SegmentId>
        Draggable : bool
    }
    
/// Absolute-based segment, with relative length and direction, less redundancy, equivalent to initial one
//type ASeg = 
//    {
//        Id : SegmentId
//        Index: int              // keep for quick lookup?
//        Start: XYPos            // Absolute start of segment
//        Dir: Orientation        // 1 of 2 orientations in X and Y       // -> in vector
//        Length: float           // Length of the segment                // -> in vector
//        HostId: ConnectionId
//        Autorouted: bool        // bool to see if the segment is manually routed or autorouted
//        /// List of x-coordinate values of segment jumps. Only used on horizontal segments.
//        JumpCoordinateList: list<float * SegmentId>
//        Draggable : bool
//    }
    
/// Rotation-invariant representation of a segment: we know that all consecutive segments are perpendicular to each other
/// So no need to store individual orientations
//type RISeg = 
//    {
//        Id : SegmentId
//        Index: int              // keep for quick lookup?
//        //Dir: Orientation        // -> Not needed as we know that all segments are perpendicular to each other
//        Length: float           // Length of the segment
//        HostId: ConnectionId
//        Autorouted: bool        // bool to see if the segment is manually routed or autorouted
//        /// List of x-coordinate values of segment jumps. Only used on horizontal segments.
//        JumpCoordinateList: list<float * SegmentId>
//        Draggable : bool
//    }


/// Default type to represent Wires in ISSIE
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

/// Absolute Wire type using ASegs, less redundancy
//type AWire =
//    {
//        Id: ConnectionId 
//        InputPort: InputPortId
//        OutputPort: OutputPortId
//        Color: HighLightColor
//        Width: int
//        Segments: array<ASeg>    // List of absolute segments (use array for faster lookup of nested elements, maybe list for interface h::tl)
//    }
//    with static member stickLength = 16.0

/// Rotation-invariant Wire type using RISegs and initial StartPos
//type RIWire =
//    {
//        Id: ConnectionId 
//        InputPort: InputPortId
//        OutputPort: OutputPortId
//        Color: HighLightColor
//        Width: int
//        Start: XYPos                // Start of the wire
//        Segments: array<RISeg>      // RI Segments one after the other

        //XMirroring: bool            //should be included?
        //YMirroring: bool            //should be included?
        //AngleRotation: Rotation     //should be included?
//    }
//    with static member stickLength = 16.0

/// Type that contains all elements regarding the BusWires model
type Model =
    {
        Symbol: Symbol.Model            //TODO: Rename to SymbolModel
        WX: Map<ConnectionId, Wire>     // Map of all the wires and their Id
        FromVerticalToHorizontalSegmentIntersections: Map<SegmentId, list<ConnectionId*SegmentId>>
        FromHorizontalToVerticalSegmentIntersections: Map<SegmentId, list<ConnectionId*SegmentId>>
        CopiedWX: Map<ConnectionId, Wire> 
        SelectedSegment: SegmentId
        LastMousePos: XYPos
        ErrorWires: list<ConnectionId>
        Notifications: Option<string>
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
    | LoadConnections of list<Connection> // For Issie Integration

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
            sprintf $"Wire: {w.Segments |> List.collect (fun seg -> seg.JumpCoordinateList |> List.map (fun (f, sid) -> ppSId sid))}")
            
    printfn $"\n------------------\nMapHV:\n {m1} \n MapVH\n{m2} \nJumps:\n {jumps}\n------------------\n"



let ppSeg seg (model: Model) = 
        let cid,sid = seg
        let wire = model.WX[cid]
        let sg = List.find (fun (s:Segment) -> s.Id = sid ) wire.Segments
        let pxy (xy: XYPos) = sprintf $"{(xy.X,xy.Y)}"
        sprintf $"""[{ppSId sg.Id}: {pxy sg.Start}->{pxy sg.End}]-{match sg.Dir with | Vertical -> "V" | _ -> "H"}-{sg.Index}"""

let pp segs (model: Model)= 
    segs
    |> List.map  ( fun seg ->
        let cid,sid = seg
        let wire = model.WX[cid]
        match List.tryFind (fun (s:Segment) -> s.Id = sid ) wire.Segments with
        | Some  sg ->
            let pxy (xy: XYPos) = sprintf $"{(xy.X,xy.Y)}"
            sprintf $"""[{pxy sg.Start}->{pxy sg.End}]-{match sg.Dir with | Vertical -> "V" | _ -> "H"}-{sg.Index}"""
        | None -> "XX")
    |> String.concat ";"

//-------------------------------Implementation code----------------------------//

/// Wire to Connection
let segmentsToVertices (segList:Segment list) = 
    let firstCoord = (segList[0].Start.X, segList[0].Start.Y)
    let verticesExceptFirst = List.mapi (fun i seg -> (seg.End.X,seg.End.Y)) segList
    [firstCoord] @ verticesExceptFirst


// -------------------------------------------- tlp19 start ---------------------------------------------------

/// Given the coordinates of two port locations that correspond
/// to the endpoints of a wire, this function returns a list of
/// wire vertices
let makeInitialWireVerticesList (portCoords : XYPos * XYPos)  = 
    // Coordinates of the starting port the wire
    let xs, ys = snd(portCoords).X, snd(portCoords).Y
    // Coordinates of the ending port the wire
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

    if Xt - xs >= adjStick * 2.0 then       // put return in anonymous record
        leftToRight, true
    elif abs (ys - Yt) < 4.0 then 
        rightToLeftHorizontal, false
    else 
        rightToLeft, false 


// -------------------------------------------- tlp19 end ---------------------------------------------------


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


// -------------------------------------------- tlp19 start ---------------------------------------------------


/// this turns a list of vertices into a list of segments
let xyVerticesToSegments connId (isLeftToRight: bool) (xyVerticesList: XYPos list) =
    // Define the shape of the Wire/Segment List
    let dirs = 
        match isLeftToRight with
        | true -> 
            // for 5 adjustable segments left-to-right
            [Horizontal;Vertical;Horizontal;Vertical;Horizontal;Vertical;Horizontal]
        | false ->
            // for 3 adjustale segments right-to-left
            [Horizontal;Horizontal;Vertical;Horizontal;Vertical;Horizontal;Horizontal]

    // Pair-up the verticies to form segments
    List.pairwise xyVerticesList
    |> List.mapi (
        fun i ({X=startX; Y=startY},{X=endX; Y=endY}) ->    
            {
                Id = SegmentId(JSHelpers.uuid())
                Index = i
                Start = {X=startX;Y=startY};
                End = {X=endX;Y=endY};
                Dir = dirs[i]
                HostId  = connId;
                JumpCoordinateList = [];
                Draggable =
                    match i with
                    | 1 | 5 ->  isLeftToRight
                    | 0  | 6  -> false
                    | _ -> true
            })


// -------------------------------------------- tlp19 end ---------------------------------------------------


/// Convert a (possibly legacy) issie Connection stored as a list of vertices to Wire
let issieVerticesToSegments 
        (connId) 
        (verticesList: list<float*float>) =
    let xyVerticesList =
        verticesList
        |> List.map (fun (x,y) -> {X=x;Y=y})

    let makeSegmentsFromVertices (xyList: XYPos list) =
        makeInitialWireVerticesList (xyList[0], xyList[xyList.Length - 1])
        |> (fun (vl, isLeftToRight) -> xyVerticesToSegments connId isLeftToRight vl)
        

    // segments lists must must be length 7, in case legacy vertex list does not conform check this
    // if there are problems reroute
        //vertex lists are one element longer than segment lists
    if xyVerticesList.Length <> 8 then  
        makeSegmentsFromVertices xyVerticesList
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
            makeSegmentsFromVertices xyVerticesList
            

    
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
        Vertices = segmentsToVertices conn.Segments
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
        Start = getAbsXY seg.Start
        End = getAbsXY seg.End }

/// Given two coordinates, this function returns the euclidean
/// distance between them.
let distanceBetweenTwoPoints (pos1 : XYPos) (pos2 : XYPos) : float =
    sqrt ( (pos1.X - pos2.X)*(pos1.X - pos2.X) + (pos1.Y - pos2.Y)*(pos1.Y - pos2.Y) )



// -------------------------------------------- tlp19 start ---------------------------------------------------

/// Given the coordinates of two port locations that correspond
/// to the endpoints of a wire, this function returns a list of
/// Segment(s).
let makeInitialSegmentsList (hostId : ConnectionId) (portCoords : XYPos * XYPos) : list<Segment> =
    let xyPairs, isLeftToRight = makeInitialWireVerticesList portCoords
    xyPairs
    |> xyVerticesToSegments hostId isLeftToRight

// -------------------------------------------- tlp19 end ---------------------------------------------------

/// This function renders the given
/// segment (i.e. creates a ReactElement
/// using the data stored inside it),
/// using the colour and width properties given.
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

    if segment.Dir = Horizontal then
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

                if (segment.Start.X > segment.End.X) then
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
                if (segment.Start.X > segment.End.X) then
                    seg.JumpCoordinateList
                    |> List.map fst
                    |> List.sortDescending
                    
                else
                    seg.JumpCoordinateList
                    |> List.map fst
                    |> List.sort
            
            match jumpCoordinateList with
                | [] -> renderWireSubSegment seg.Start seg.End

                | lst ->
                     let y = seg.Start.Y // SHOULD be equal to seg.End.Y since ONLY horizontal segments have jumps
                     let firstSegmentJumpCoordinate = lst[0]
                     let lastSegmentJumpCoordinate = lst[(List.length lst) - 1]

                     if (segment.Start.X > segment.End.X) then
                         renderWireSubSegment seg.Start {X = firstSegmentJumpCoordinate + segmentJumpHorizontalSize/2.0; Y = y}
                         @
                         renderMultipleSegmentJumps lst y
                         @
                         renderWireSubSegment {X = lastSegmentJumpCoordinate - segmentJumpHorizontalSize/2.0; Y = y} seg.End

                     else
                         renderWireSubSegment seg.Start {X = firstSegmentJumpCoordinate - segmentJumpHorizontalSize/2.0; Y = y}
                         @
                         renderMultipleSegmentJumps lst y
                         @
                         renderWireSubSegment {X = lastSegmentJumpCoordinate + segmentJumpHorizontalSize/2.0; Y = y} seg.End
        

        let wireSegmentReactElementList = segment
                                          |> completeWireSegmentRenderFunction

        g [] wireSegmentReactElementList
    
    else
        let Xa, Ya, Xb, Yb = segment.Start.X, segment.Start.Y, segment.End.X, segment.End.Y
        let segmentElements = 
            makeLine Xa Ya Xb Yb lineParameters
            ::
            makeCircle Xa Ya circleParameters
            ::
            [
                makeCircle Xb Yb circleParameters
            ]
        g [] segmentElements

///
type WireRenderProps =
    {
        key: string
        Segments: list<Segment>
        ColorP: HighLightColor
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
        fun (props: WireRenderProps) ->
            let renderWireSegmentList : list<ReactElement> =
                props.Segments
                |> List.map
                    (
                        fun (segment : Segment) -> renderSegment segment (props.ColorP.Text()) (string props.StrokeWidthP)
                            //call a bunch of render helper functions to render the segment (*** DO NOT FORGET SEGMENT JUMPS ***)
                    )
            
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
let MapToSortedList map : Wire list = 
    let listSelected = 
        Map.filter (fun id wire -> wire.Color = HighLightColor.Purple) map
        |> Map.toList
        |> List.map snd
    let listErrorSelected =
        Map.filter (fun id wire -> wire.Color = HighLightColor.Brown) map
        |> Map.toList
        |> List.map snd
    let listErrorUnselected =
        Map.filter (fun id wire -> wire.Color = HighLightColor.Red) map
        |> Map.toList
        |> List.map snd
    let listUnSelected = 
        Map.filter (fun id wire -> wire.Color = HighLightColor.DarkSlateGrey) map
        |> Map.toList
        |> List.map snd
    let listCopied = 
        Map.filter (fun id wire -> wire.Color = HighLightColor.Thistle) map
        |> Map.toList
        |> List.map snd
    let listWaves = 
        Map.filter (fun id wire -> wire.Color = HighLightColor.Blue) map
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
                            ColorP = wire.Color
                            StrokeWidthP = wire.Width
                            OutputPortLocation = outputPortLocation
                        }
                    singleWireView props)
    TimeHelpers.instrumentInterval "WirePrepareProps" rStart ()
    let symbols = Symbol.view model.Symbol (Symbol >> dispatch)
 
    g [] [(g [] wires); symbols]
    |> TimeHelpers.instrumentInterval "WireView" start


//
//---------------------------------------------------------------------------------//
//--------------------tlp19 CODE SECTION STARTS------------------------------------//
//---------------------------------------------------------------------------------//
//

/// This function is given two couples of
/// points that define two line segments and it returns:
/// - Some (x, y) if the two segments intersect;
/// - None if the do not.
let tryCoordinatesIfIntersection ((p1, q1) : (XYPos * XYPos)) ((p2, q2) : (XYPos * XYPos)) : Option<XYPos> =
    let startSeg1 = p1
    let endSeg1 = q1
    let startSeg2 = p2
    let endSeg2 = q2    
    
    // Check if the two intersect
    if (segmentIntersectsSegment (startSeg1, endSeg1) (p2, q2)) then                                        
        // Extract their coordinates
        let x1, y1, x2, y2 = abs startSeg1.X, abs startSeg1.Y, abs endSeg1.X, abs endSeg1.Y
        let x3, y3, x4, y4 = abs startSeg2.X, abs startSeg2.Y, abs endSeg2.X, abs endSeg2.Y
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
let isSegmentIntersectingBoundingBox (segIn : Segment) (bb : BoundingBox) : bool =
    // Get the absolute coordinates of the segment
    let seg = makeSegPos segIn
    // Get the topLeft and bottomRight corners of the bounding box, and decompose/match their XYPos into distinct values
    let ({X = x; Y = y} : XYPos), ({X = a; Y = b} : XYPos) = getTopLeftAndBottomRightCorner bb
    let ({X = xTL; Y = yTL} : XYPos), ({X = xBR; Y = yBR} : XYPos) = getTopLeftAndBottomRightCorner bb
    // Decompose the coordinates of the segment's endpoints
    let x1, y1, x2, y2 = seg.Start.X, seg.Start.Y, seg.End.X, seg.End.Y

    // Check to see if all coordinates of the segment are within the limits of the bounding box
    let segPointInBox =
        (( (x1 > xTL) && (x1 < xBR) ) && ( (y1 > yTL) && (y1 < yBR) ))
        ||
        (( (x2 > xTL) && (x2 < xBR) ) && ( (y2 > yTL) && (y2 < yBR) ))
    
    // Get, if they exist, the intersections of the segment with the 4 sides of the bounding box
    let left = tryCoordinatesIfIntersection (seg.Start, seg.End) ({X=xTL; Y=yTL}, {X=xTL; Y=yBR})
    let right = tryCoordinatesIfIntersection (seg.Start, seg.End) ({X=xBR; Y=yTL}, {X=xBR; Y=yBR})
    let top = tryCoordinatesIfIntersection (seg.Start, seg.End) ({X=xTL; Y=yTL}, {X=xBR; Y=yTL})
    let bottom = tryCoordinatesIfIntersection (seg.Start, seg.End) ({X=xTL; Y=yBR}, {X=xBR; Y=yBR})
    
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
    let x0, y0 = point.X, abs point.Y
    let x1, y1, x2, y2 = abs segment.Start.X, abs segment.Start.Y, abs segment.End.X, abs segment.End.Y
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
let getSafeDistanceForMove (seg: Segment) (seg0:Segment) (seg6:Segment) (distance:float) =
    /// Check if 2 floats are equal according to some threshold
    let areFloatsEquals (x: float) (y: float) : bool =
        abs (abs x - abs y) > 0.0001

    // Define a smaller shrink factor if the segment is not in the middle of the wire
    let shrink = match seg.Index with 
                 | 1 | 2 | 4 | 5 -> 0.5 
                 | 3 | _ -> 1.0      
    // Then, according to the index
    match seg.Index with
    | 1 | 2 ->   
        //max distance you can do = X coordinate that you can't pass - where you are   (moving towards negative/smaller numbers)
        let minDistance = (seg0.Start.X + Wire.stickLength * shrink) - abs seg.End.X
        max minDistance distance
    | 4 | 5 ->
        let maxDistance = seg6.End.X -  Wire.stickLength * shrink - abs seg.Start.X
        min maxDistance distance
    | 3 when distance < 0.0 && (areFloatsEquals seg0.Start.Y seg.Start.Y) ->    // If Seg3 is not merged with Seg0, can do anything
        distance
    | 3 when distance > 0.0 && (areFloatsEquals seg6.Start.Y seg.End.Y) ->      // If Seg3 is not merged with Seg6, can do anything
        distance
    | 3 ->
        let minDistance = abs seg0.Start.X + Wire.stickLength * shrink - abs seg.Start.X
        let maxDistance = abs seg6.End.X -  Wire.stickLength * shrink - abs seg.Start.X
        distance
        |> max minDistance
        |> min maxDistance
    | _ -> 
        distance

        
/// Adjust wire (input type is Segment list) so that two adjacent horizontal segments that are in opposite directions
/// get eliminated
let removeRedundantSegments  (segs: Segment list) =
    /// Set the absolute value of X (keeping the previously assigned sign)
    let setAbsX x (pos: XYPos) =
        let x = if pos.X < 0.0 then - abs x else abs x
        {pos with X = x}
    /// Get difference in X along a segment
    let xDelta seg = abs seg.End.X - abs seg.Start.X
    /// Set the Start of the segment to 'x', keeping the sign
    let setStartX x (seg:Segment) = {seg with Start = setAbsX x seg.Start}
    /// Set the End of the segment to 'x', keeping the sign
    let setEndX x (seg:Segment) = {seg with End = setAbsX x seg.End}
    /// Takes two segments, and if they are Horizontal and in opposite direction, "adjust" them
    let adjust seg1 seg2 =
        // Get their direction
        let xd1, xd2 = xDelta seg1, xDelta seg2
        // If they are horizontal and of opposite direction
        if seg1.Dir = Horizontal && 
           seg2.Dir = Horizontal && 
           sign xd1 <> sign xd2 
        then
            // If the first segment is longer than the second one
            if abs xd1 > abs xd2 then
                // replace the end of segment 1 with the end of segment 2, and the start of segment 2 with its end (making it of length 0)
                [setEndX seg2.End.X seg1; setStartX seg2.End.X seg2]
            else
                // do the opposite
                [setEndX seg1.Start.X seg1; setStartX seg1.End.X seg2]
        else
            // Otherwise, do nothing
            [seg1;seg2]
    
    // Adjust the first two, and last two, segments of a Wire's segments list
    adjust segs[0] segs[1] @  segs[2..4] @ adjust segs[5] segs[6]
    
    
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
    // Check if the segment has a valid index, and is not Segment[0] or Segment[6]
    if index <= 0 || index >= wire.Segments.Length - 1 then
        failwithf $"Buswire segment index {index} out of range in moveSegment in wire length {wire.Segments.Length}"
    // Get the previous and next segments
    let prevSeg = wire.Segments[index-1]
    let nextSeg = wire.Segments[index+1]
    // If the segment has the same direction than any of it's neighbours, don't do anything
    if seg.Dir = prevSeg.Dir || seg.Dir = nextSeg.Dir then
        wire
    // Else, move the segment, and update the neighbours
    else
        //runTestFable()
        distance      
        |> getSafeDistanceForMove seg wire.Segments[0] wire.Segments[6]   
        |> (fun distance' ->                                                            //TODO: Define a seperate function
            // Define the updated coordinates of the segments based on the orientation of the segment being moved
            let newPrevEnd, newSegStart, newSegEnd, newNextStart = 
                match seg.Dir with
                // If it's vertical, update the X coordinates
                | Vertical -> 
                    //extract old XYPos, and change the X
                    {prevSeg.End with X = - (abs seg.Start.X + distance')}, 
                    {seg.Start with X = - (abs seg.Start.X + distance')}, 
                    {seg.End with X = - (abs seg.End.X + distance')}, 
                    {nextSeg.Start with X = - (abs seg.End.X + distance')}
                //If it's horizontal, update the Y coordinates
                | Horizontal -> 
                    //extract old XYPos, and change the Y
                    {prevSeg.End with Y = - (abs seg.Start.Y + distance')}, 
                    {seg.Start with Y = - (abs seg.Start.Y + distance')}, 
                    {seg.End with Y = - (abs seg.End.Y + distance')}, 
                    {nextSeg.Start with Y = - (abs seg.End.Y + distance')}
            
            // Compile the new segments with updated XYPos Starts and Ends
            let newPrevSeg = {prevSeg with End = newPrevEnd}
            let newSeg = {seg with Start = newSegStart;End = newSegEnd}
            let newNextSeg = {nextSeg with Start = newNextStart}
        
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
    } , Cmd.none


// NOT USED IN THIS FILE, but breaks functionnality if removed
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
        (inputWires, outputWires, fullyConnected)
        //{| InputWires = inputWires ; OutputWires = outputWires ; FullyConnectedWires = fullyConnected |}


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
    |> List.map (fun seg -> {seg with Start = seg.End; End = seg.Start})

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
// For simplicity, due to the encoding of manual changes into coordinates by negating them (yuk!)
// we do not alter coordinate sign. Instead we invert all numeric comparisons.
// There are no constants used in the algorithm (if there were, they would need to be negated)
//
// ======================================================================================================================

/// Adds two XYPos together, (X1+X2, Y1+Y2) similarly to adding two vectors
let inline addPositions (pos1: XYPos) (pos:XYPos) =
    {X = pos1.X + pos.X; Y = pos1.Y + pos.Y}

/// Move the End of the Nth segment according to the suplied 'mover' function
let inline moveEnd (mover: XYPos -> XYPos) (n:int) =
    List.mapi (fun i (seg:Segment) -> if i = n then {seg with End = mover seg.End} else seg)

/// Move the Start of the Nth segment according to the suplied 'mover' function
let inline moveStart (mover: XYPos -> XYPos) (n:int) =
    List.mapi (fun i (seg:Segment) -> if i = n then {seg with Start = mover seg.Start} else seg)

/// Move both the Start and the End of the Nth segment according to the suplied 'mover' function (applied on both)
let inline moveAll (mover: XYPos -> XYPos) (n : int) =
    List.mapi (fun i (seg:Segment) -> if i = n then {seg with Start = mover seg.Start; End = mover seg.End} else seg)

/// Given 2 functions and a point, apply the first one on the X coordinate of the point, and the second on the Y coordinate
let  transformXY tX tY (pos: XYPos) =
    {pos with X = tX pos.X; Y = tY pos.Y}

/// Given 2 functions, applies them respectively to the X and Y coordinates of the endpoints of the segment
let transformSeg tX tY (seg: Segment) =
    let trans = transformXY tX tY
    {seg with Start = trans seg.Start; End = trans seg.End }

/// Returns a tuple containing the sign of the difference of the X coordinates, and the sign of the difference of the Y coordinates
let topology (pos1: XYPos) (pos2:XYPos) =
    sign (abs pos1.X - abs pos2.X), sign (abs pos1.Y - abs pos2.Y)


/// Returns None if full autoroute is required or Some segments with initial part of the segment list autorouted
/// up till the first dragged (manually routed) segment.
/// This always done in the order of the segment list, it needs to be reversed before calling the function
let tryPartialAutoRoute (segs: Segment list) (newPortPos: XYPos) =
    // Get where the first movable segment of the wire starts
    let wirePos = segs[0].End
    // Get the previous portPos
    let portPos = segs[0].Start
    // Keep the same clearance from the port than previously
    let newWirePos = {newPortPos with X = newPortPos.X + (abs wirePos.X - portPos.X) }
    // Get by how much the port has moved
    let (diff:XYPos) = {X=newPortPos.X-portPos.X; Y= newPortPos.Y - portPos.Y}
    
    /// Returns the index of the first index that is manually routed
    let tryGetIndexOfFirstManuallyRoutedSegment =
        // Checks to see if a Segment is manually routed
        let isNegative (pos:XYPos) = pos.X < 0.0 || pos.Y < 0.0
        let isAutoSeg seg =                                                     //TODO: UPDATE FOR NEW TYPES
            not (isNegative seg.Start || isNegative seg.End)
        // Get the index of the last segment that was autorouted
        segs
        |> List.takeWhile isAutoSeg
        |> List.length
        |> (fun n -> if n > 5 then None else Some (n + 1))  //Return the first manually routed segment's index

    /// Scale all the segments between the end of seg[0] and the end of the last manually routed segment.
    let scaleAutoroutedSegments segIndex =
        // Get the first segment that is manually routed (its End is fixed)
        let seg = segs[segIndex]
        // Get its fixed End's coordinates
        let fixedPt = getAbsXY seg.End
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

    /// Check if we are still in the same quadrant as the other end of the wire
    let checkTopologyChangeOption index =
        let finalPt = segs[6].Start
        let oldTop x = topology (if index = 1 then portPos else wirePos) x
        let newTop x = topology (if index = 1 then newPortPos else newWirePos) x
        // Check if we are still in the same quadrant as the other end of the wire
        if oldTop finalPt <> newTop finalPt then
            // always abandon manual routing if we are not
            None 
        else
            let manualSegmentEndpoint = segs[index].End
            let oldT = oldTop manualSegmentEndpoint
            let newT = newTop manualSegmentEndpoint
            // Check if we are still in the same quadrant as the end of the manually routed segment
            if oldT = newT then
                Some index
            else
                None

    tryGetIndexOfFirstManuallyRoutedSegment      //Get index: None if all segments are autorouted
    |> Option.bind checkTopologyChangeOption    //Checks: None if we change quadrants (either between two ends of wire, or between moving endpoint and first manually fixed end)
    |> Option.bind scaleAutoroutedSegments


///Returns the new positions keeping manual coordinates negative, and auto coordinates positive
let addToPosAndKeepRoutingMode (pos : XYPos) (diff : XYPos) : XYPos =
    let newPos = Symbol.posAdd (getAbsXY pos) diff                  
    // If coordinates where manually routed before, keep it that way
    if pos.X < 0. || pos.Y < 0. then {X = - newPos.X; Y = - newPos.Y}
    else newPos

///Moves a wire by a specified amount by adding a XYPos to each start and end point of each segment
let moveWire (wire : Wire) (diff : XYPos) = 
    // Translates a segment by a vector 'diff' 
    let translateSegment seg = 
        {seg with
            Start = addToPosAndKeepRoutingMode seg.Start diff
            End = addToPosAndKeepRoutingMode seg.End diff
        }
    // Translate all the segments of the wire
    let newSegs = List.map translateSegment wire.Segments
    // Return the new wire
    {wire with Segments = newSegs}

/// Re-routes a single wire in the model when its ports move.
/// Tries to preserve manual routing when this makes sense, otherwise re-routes with autoroute.
/// Partial routing from input end is done by reversing segments and and swapping Start/End
/// inOut = true => reroute input (target) side of wire.
let updateWire (model : Model) (wire : Wire) (inInputPort : bool) =
    let newPort = 
        // Get the coordinates of the port that moved, according to if it was the input or output
        match inInputPort with
        | true -> Symbol.getInputPortLocation model.Symbol wire.InputPort
        | false -> Symbol.getOutputPortLocation model.Symbol wire.OutputPort
    if inInputPort then
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


//
//-------------------------------------------------------------------------------//
//--------------------tlp19 CODE SECTION ENDS------------------------------------//
//-------------------------------------------------------------------------------//
//


let makeAllJumps (wiresWithNoJumps: ConnectionId list) (model: Model) =
    let mutable newWX = model.WX
    // Arrays are faster to check than lists
    let wiresWithNoJumpsA = List.toArray wiresWithNoJumps
    let changeJumps wid index jumps =
        let jumps = List.sortDescending jumps
        let changeSegment segs =
            List.mapi (fun i x -> if i <> index then x else { x with JumpCoordinateList = jumps }) segs

        newWX <- Map.add wid { newWX[wid] with Segments = changeSegment newWX[wid].Segments } newWX

    let segs =
        model.WX
        |> Map.toArray
        |> Array.mapi (fun i (wid, w) -> List.toArray w.Segments)

    for w1 in 0 .. segs.Length - 1 do
        for h in segs[w1] do
            if h.Dir = Horizontal then
                // work out what jumps this segment should have
                let mutable jumps: (float * SegmentId) list = []
                
                if not (Array.contains h.HostId wiresWithNoJumpsA) then
                    for w2 in 0 .. segs.Length - 1 do
                        // everything inside the inner loop should be very highly optimised
                        // it is executed n^2 time where n is the number of segments (maybe 5000)
                        // the abs here are because segment coordinates my be negated to indicate manual routing
                        for v in segs[w2] do
                            if not (Array.contains v.HostId wiresWithNoJumpsA) then
                                match v.Dir with
                                | Vertical ->
                                    let x, x1, x2 = abs v.Start.X, abs h.Start.X, abs h.End.X
                                    let y, y1, y2 = abs h.Start.Y, abs v.Start.Y, abs v.End.Y
                                    let xhi, xlo = max x1 x2, min x1 x2
                                    let yhi, ylo = max y1 y2, min y1 y2
                                    //printfn $"{[xlo;x;xhi]}, {[ylo;y;yhi]}"
                                    if x < xhi - 5.0 && x > xlo + 5.0 && y < yhi - 5.0 && y > ylo + 5.0 then
                                        //printfn "found a jump!"
                                        jumps <- (x, v.Id) :: jumps
                                | _ -> ()
                    // compare jumps with what segment now has, and change newWX if need be
                // note that if no change is needed we do not update WX
                // simple cases are done without sort for speed, proably not necessary!
                // The jump list is sorted in model to enable easier rendering of segments
                match jumps, h.JumpCoordinateList with
                | [], [] -> ()
                | [ a ], [ b ] when a <> b -> changeJumps h.HostId h.Index jumps
                | [], _ -> changeJumps h.HostId h.Index jumps
                | _, [] -> // in this case we need to sort the jump list
                    changeJumps h.HostId h.Index (List.sort jumps)
                | newJumps, oldJ ->
                    let newJ = List.sort newJumps
                    // oldJ is already sorted (we only ever write newJ back to model)
                    if newJ <> oldJ then changeJumps h.HostId h.Index newJumps else ()

    { model with WX = newWX }


let updateWireSegmentJumps (wireList: list<ConnectionId>) (wModel: Model) : Model =
    let startT = TimeHelpers.getTimeMs()
    let model = makeAllJumps [] wModel
    TimeHelpers.instrumentTime "UpdateJumps" startT
    model



/// This function updates the wire model by removing from the stored lists of intersections
/// all those generated by wireList wires.
/// intersetcions are stored in maps on the model and on the horizontal segments containing the jumps
let resetWireSegmentJumps (wireList : list<ConnectionId>) (wModel : Model) : Model =
    makeAllJumps wireList wModel



   
        



/// Re-routes the wires in the model based on a list of components that have been altered.
/// If the wire input and output ports are both in the list of moved components, does not re-route wire but instead translates it.
/// Keeps manual wires manual (up to a point).
/// Otherwise it will auto-route wires connected to components that have moved
let updateWires (model : Model) (compIdList : ComponentId list) (diff : XYPos) =

    let (inputWires, outputWires, fullyConnected) = getWiresConnectedToPorts model compIdList

    let newWires = 
        model.WX
        |> Map.toList
        |> List.map (fun (cId, wire) -> 
            if List.contains cId fullyConnected //Translate wires that are connected to moving components on both sides
            then (cId, moveWire wire diff)
            elif List.contains cId inputWires //Only route wires connected to ports that moved for efficiency
            then (cId, updateWire model wire true)
            elif List.contains cId outputWires
            then (cId, updateWire model wire false)
            else (cId, wire))
        |> Map.ofList
        
    {model with WX = newWires}

///
let update (msg : Msg) (model : Model) : Model*Cmd<Msg> =
    
    match msg with
    | Symbol sMsg ->
        let sm,sCmd = Symbol.update sMsg model.Symbol
        {model with Symbol=sm}, Cmd.map Symbol sCmd


    | UpdateWires (componentIdList, diff) -> 
        updateWires model componentIdList diff, Cmd.none

    | AddWire ( (inputId, outputId) : (InputPortId * OutputPortId) ) ->
        let portOnePos, portTwoPos = Symbol.getTwoPortLocations model.Symbol inputId outputId
        let wireWidthFromSymbol = WireWidth.Configured 1
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
        let newModel = updateWireSegmentJumps [wireId] {model with WX = wireAddedMap}

        newModel, Cmd.ofMsg BusWidths

    | BusWidths ->

        let processConWidths (connWidths: ConnectionsWidth) =
            let addWireWidthFolder (wireMap: Map<ConnectionId, Wire>) _ wire  =
                let width =
                    match connWidths[wire.Id] with
                    | Some a -> a
                    | None -> wire.Width
                let newColor = if wire.Color = Purple || wire.Color = Brown then Purple else DarkSlateGrey
                wireMap.Add ( wire.Id, { wire with Width = width ; Color = newColor} )

            let addSymbolWidthFolder (m: Map<ComponentId,Symbol.Symbol>) (_: ConnectionId) (wire: Wire) =
                    let inPort = model.Symbol.Ports[match wire.InputPort with InputPortId ip -> ip]
                    let symId = ComponentId inPort.HostId
                    let symbol = m[symId]

                    match symbol.Compo.Type with
                    | SplitWire n ->
                        match inPort.PortNumber with 
                        | Some 0 -> {symbol with InWidth0 = Some wire.Width}
                        | x -> failwithf $"What? wire found with input port {x} other than 0 connecting to SplitWire"
                        |> (fun sym -> Map.add symId sym m)
                    | MergeWires ->
                        match inPort.PortNumber with 
                        | Some 0 -> 
                            Map.add symId  {symbol with InWidth0 = Some wire.Width} m
                        | Some 1 -> 
                            Map.add symId {symbol with InWidth1 = Some wire.Width} m
                        | x -> failwithf $"What? wire found with input port {x} other than 0 or 1 connecting to MergeWires"
                    | _ -> m

            let newWX = ((Map.empty, model.WX) ||> Map.fold addWireWidthFolder)

            let symbolsWithWidths =
                (model.Symbol.Symbols, newWX) ||> Map.fold addSymbolWidthFolder

            { model with 
                WX = newWX; Notifications = None ; 
                ErrorWires=[]; 
                Symbol = {model.Symbol with Symbols = symbolsWithWidths}}, Cmd.none    
        


        let canvasState = (Symbol.extractComponents model.Symbol, extractConnections model )
        
        
        match BusWidthInferer.inferConnectionsWidth canvasState with
        | Ok connWidths ->
            processConWidths connWidths
        | Error e ->
                { model with 
                    Notifications = Some e.Msg }, Cmd.ofMsg (ErrorWires e.ConnectionsAffected)
    
    | CopyWires (connIds : list<ConnectionId>) ->
        let copiedWires = Map.filter (fun connId _ -> List.contains connId connIds) model.WX
        { model with CopiedWX = copiedWires }, Cmd.none

    | ErrorWires (connectionIds : list<ConnectionId>) -> 
        let newWX =
            model.WX
            |> Map.map
                (fun id wire -> 
                    if List.contains id connectionIds then
                        {wire with Color = HighLightColor.Red}
                    else if List.contains id model.ErrorWires then 
                        {wire with Color = HighLightColor.DarkSlateGrey}
                    else wire
                ) 
        
        {model with WX = newWX ; ErrorWires = connectionIds}, Cmd.none

    | SelectWires (connectionIds : list<ConnectionId>) -> //selects all wires in connectionIds, and also deselects all other wires
        let newWX =
            model.WX
            |> Map.map
                (fun id wire -> 
                    if List.contains id model.ErrorWires then
                        if List.contains id connectionIds then 
                            {wire with Color = HighLightColor.Brown} 
                        else 
                            {wire with Color = HighLightColor.Red}
                    else if List.contains id connectionIds then
                        {wire with Color = HighLightColor.Purple} 
                    else
                        {wire with Color = HighLightColor.DarkSlateGrey} 
                ) 
        
        {model with WX = newWX}, Cmd.none

    | DeleteWires (connectionIds : list<ConnectionId>) -> 
        let newModel = resetWireSegmentJumps (connectionIds) (model)
        let newWX =
             newModel.WX
             |> Map.filter (fun id wire -> not (List.contains id connectionIds))
        {newModel with WX = newWX}, Cmd.ofMsg BusWidths

    | DragWire (connId : ConnectionId, mMsg: MouseT) ->
        match mMsg.Op with
        | Down ->
            let segId = getClickedSegment model connId mMsg.Pos
            {model with SelectedSegment = segId }, Cmd.none
        | Drag ->
            let segId = model.SelectedSegment
            let rec getSeg (segList: list<Segment>) = 
                match segList with
                | h::t -> if h.Id = segId then h else getSeg t
                | _ -> failwithf "segment Id not found in segment list"
            let seg = getSeg model.WX[connId].Segments
            if seg.Draggable then
                let distanceToMove = 
                    match seg.Dir with
                    | Horizontal -> mMsg.Pos.Y - abs seg.Start.Y
                    | Vertical -> mMsg.Pos.X - abs seg.Start.X

                let newWire = moveSegment seg distanceToMove model
                let newWX = Map.add seg.HostId newWire model.WX
 
                {model with WX = newWX}, Cmd.none
            else
                model, Cmd.none
        
        | _ -> model, Cmd.none


    | ColorWires (connIds, color) -> // Just Changes the colour of the wires, Sheet calls pasteWires before this
        let newWires =
            (List.fold (fun prevWires cId -> 
                let oldWireOpt = Map.tryFind cId model.WX
                match oldWireOpt with
                | None -> 
                    printfn "BusWire error: expected wire in ColorWires does not exist"
                    prevWires
                | Some oldWire ->
                    Map.add cId { oldWire with Color = color } prevWires) model.WX connIds)
        { model with WX = newWires }, Cmd.none
    
    | ResetJumps connIds ->
        printfn $"resetting jumps on {connIds.Length} wires"
        
        let newModel =
            model
            |> resetWireSegmentJumps connIds
        
        newModel, Cmd.none
    
    | MakeJumps connIds ->
        printfn $"making jumps on {connIds.Length} wires"

        let newModel =
            model
            |> updateWireSegmentJumps connIds
            
        newModel, Cmd.none
    
    | ResetModel -> { model with WX = Map.empty; ErrorWires = []; Notifications = None }, Cmd.none
    
    | LoadConnections conns -> // we assume components (and hence ports) are loaded before connections
        let posMatchesVertex (pos:XYPos) (vertex: float*float) =
            let epsilon = 0.00001
            abs (abs pos.X - abs (fst vertex)) < epsilon &&
            abs (abs pos.Y - abs (snd vertex)) < epsilon
            |> (fun b -> if not b then printf $"Bad wire endpoint match on {pos} {vertex}"; b else b)
        let newWX =
            conns 
            |> List.map ( fun conn ->
                            let inputId = InputPortId conn.Target.Id
                            let outputId = OutputPortId conn.Source.Id
                            let connId = ConnectionId conn.Id
                            let segments = issieVerticesToSegments connId conn.Vertices
                            let makeWirePosMatchSymbol inOut (wire:Wire) =
                                match inOut with
                                | true -> posMatchesVertex 
                                            (Symbol.getInputPortLocation model.Symbol inputId)
                                            (List.head conn.Vertices)
                                | false ->
                                          posMatchesVertex 
                                            (Symbol.getOutputPortLocation model.Symbol outputId) 
                                            (List.last conn.Vertices)
                                |> (fun b -> 
                                    if b then 
                                        wire 
                                    else
                                        let getS (connId:string) = 
                                            Map.tryFind connId model.Symbol.Ports
                                            |> Option.map (fun port -> port.HostId)
                                            |> Option.bind (fun symId -> Map.tryFind (ComponentId symId) model.Symbol.Symbols)
                                            |> Option.map (fun sym -> sym.Compo.Label)
                                        printfn $"Updating loaded wire from {getS conn.Source.Id}->{getS conn.Target.Id} of wire "
                                        updateWire model wire inOut)
                                
                                
                            connId,
                            { Id = ConnectionId conn.Id
                              InputPort = inputId
                              OutputPort = outputId
                              Color = HighLightColor.DarkSlateGrey
                              Width = 1
                              Segments = segments}
                            |> makeWirePosMatchSymbol false
                            |> makeWirePosMatchSymbol true
                        )
            |> Map.ofList
        
        let connIds =
            conns
            |> List.map (fun conn -> ConnectionId conn.Id)
            
        { model with WX = newWX }, Cmd.ofMsg (MakeJumps connIds)

//---------------Other interface functions--------------------//

///
let wireIntersectsBoundingBox (w : Wire) (bb : BoundingBox) =
    let boolList = List.map (fun seg -> isSegmentIntersectingBoundingBox seg bb) w.Segments
    List.contains true boolList

///
let getIntersectingWires (wModel : Model) (selectBox : BoundingBox) : list<ConnectionId> = 
    wModel.WX
    |> Map.map (fun id wire -> wireIntersectsBoundingBox wire selectBox)
    |> Map.filter (fun id boolVal -> boolVal)
    |> Map.toList
    |> List.map (fun (id,bool) -> id)

///searches if the position of the cursor is on a wire in a model
///Where n is 5 pixels adjusted for top level zoom
let getWireIfClicked (wModel : Model) (pos : XYPos) (n : float) : ConnectionId Option =
    let boundingBox = {BoundingBox.X = pos.X - n; Y = pos.Y - n; H = n*2.; W = n*2.}
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

///
let getPortIdsOfWires (model: Model) (connIds: ConnectionId list) : (InputPortId list * OutputPortId list) =
    (([], []), connIds)
    ||> List.fold (fun (inputPorts, outputPorts) connId ->
            (model.WX[connId].InputPort :: inputPorts, model.WX[connId].OutputPort :: outputPorts))
