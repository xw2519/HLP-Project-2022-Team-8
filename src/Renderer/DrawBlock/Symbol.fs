﻿// This module draws schematics component symbols. 
// Each symbol is associated with a unique Issie component.

module Symbol
open Fable.React
open Fable.React.Props
open Elmish
open DrawHelpers
open CommonTypes
open System.Text.RegularExpressions

//--------------------------------- Debugging functions ---------------------------------//

let print x = printfn "%A" x

//--------------------------------- Static Variables ---------------------------------//

let GridSize = 30 

//--------------------------------- Symbol Types ---------------------------------//

type SymbolCharacteristics =
    {
        clocked: bool 
        inverted: bool
    }

type SymbolTitles = 
    { 
        fontSize: string
        fontWeight: string
        posX: float
        posY: float
        text: string
        textAlignment: string
    }

type Symbol =
    {
        Colour: string
        ComponentId : ComponentId       
        Component : Component
        InWidth0: int option
        InWidth1: int option      
        Moving: bool
        Opacity: float
        Pos: XYPos
        Rotation: float
        ShowInputPorts: bool
        ShowOutputPorts: bool        
        SymbolCharacteristics: SymbolCharacteristics
        SymbolPoints: XYPos list
    }

type Model = 
    {   
        CopiedSymbols: Map<ComponentId, Symbol>
        Ports: Map<string, Port> // string since it's for both input and output ports
        Symbols: Map<ComponentId, Symbol>
    }

type Msg =
    | MouseMsg of MouseT
    | AddSymbol of pos:XYPos * compType:ComponentType * lbl: string
    | CopySymbols of ComponentId list
    | DeleteSymbols of sIds:ComponentId list
    | ShowAllInputPorts | ShowAllOutputPorts | DeleteAllPorts 
    | MoveSymbols of compList: ComponentId list * move: XYPos
    | ShowPorts of ComponentId list
    | SelectSymbols of ComponentId list
    | SymbolsHaveError of sIds: ComponentId list
    | ChangeLabel of sId : ComponentId * newLabel : string
    | PasteSymbols of sIds: ComponentId list
    | ColorSymbols of compList : ComponentId list * colour : HighLightColor
    | ErrorSymbols of errorIds: ComponentId list * selectIds: ComponentId list * isDragAndDrop: bool
    | ChangeNumberOfBits of compId:ComponentId * NewBits:int 
    | ChangeLsb of compId: ComponentId * NewBits:int64 
    | ChangeConstant of compId: ComponentId * NewBits:int64 * NewText:string
    | ResetModel
    | LoadComponents of  Component list 
    | WriteMemoryLine of ComponentId * int64 * int64
    | WriteMemoryType of ComponentId * ComponentType
    | RotateSymbols of ComponentId list

//--------------------------------- Helper functions ---------------------------------//
let convertDegtoRad degree = System.Math.PI * degree / 180.0 

let convertRelativeToSymbolCenter (symbol: Symbol) (portPos: XYPos) : XYPos =
    { X = portPos.X-(float(symbol.Component.W)/2.0)
      Y = portPos.Y-(float(symbol.Component.H)/2.0) }

let convertRelativeToTopLeft (symbol: Symbol) (portPos: XYPos) : XYPos =
    { X = portPos.X+(float(symbol.Component.W)/2.0)
      Y = portPos.Y+(float(symbol.Component.H)/2.0) }

let convertSymbolPointsToString (xyPosL: XYPos list) = 
    let splitXYPos accumulator (xyPos: XYPos) = 
        accumulator + string(xyPos.X) + "," + string(xyPos.Y) + " "

    let coordinateString = ("", xyPosL) ||> List.fold splitXYPos

    coordinateString[0 .. (String.length coordinateString) - 2]   

let flipSymbol (symbol: Symbol) = 
    print "Flip Triggered"

let getSymbolPoints (symbol: Symbol) = convertSymbolPointsToString symbol.SymbolPoints

let posAdd (a: XYPos) (b: XYPos) = { X = a.X+b.X; Y = a.Y+b.Y }

let posDiff (a: XYPos) (b: XYPos) = { X = a.X-b.X; Y = a.Y-b.Y }

let posOf x y = { X = x; Y = y}

let rotatePoint degree (xyPos: XYPos) : XYPos =
    { X = xyPos.Y * -System.Math.Sin(convertDegtoRad degree) + xyPos.X * System.Math.Cos(convertDegtoRad degree)
      Y = xyPos.X * System.Math.Sin(convertDegtoRad degree) + xyPos.Y * System.Math.Cos(convertDegtoRad degree) }

let rotateSymbol (symbol: Symbol) = 
    let nextRotation = 
        [   0.0, 90.0;
            90.0, 180.0;
            180.0, 270.0;
            270.0, 0.0  ]
        |> Map.ofList

    let newSymbolPoints = 
        symbol.SymbolPoints
        |> List.map (convertRelativeToSymbolCenter symbol)
        |> List.map (rotatePoint 90.0)
        |> List.map (convertRelativeToTopLeft symbol)

    {symbol with Rotation = nextRotation.[symbol.Rotation]; SymbolPoints = newSymbolPoints}

//--------------------------------- Skeleton Model Type for Symbols and Components ---------------------------------//

let init () = { Symbols = Map.empty; CopiedSymbols = Map.empty; Ports = Map.empty }, Cmd.none

let initComponent (pos: XYPos) (compType: ComponentType) (compId: string) (compLabel: string) : Component =
    let cutToLength (lst: (string * int) list) =
        let labelList = List.map (fst >> String.length) lst
        
        if List.isEmpty labelList then 0 else List.max labelList

    let initPorts numOfPorts compId (portType: PortType) =
        if numOfPorts < 1 then 
            []
        else
            [0..(numOfPorts-1)]
            |> List.collect (fun x ->
                [{ Id = JSHelpers.uuid ()
                   PortNumber = Some x
                   PortType = portType
                   HostId = compId }])

    let roundToNth (precision : int) (x : int) = x + abs((x % precision) - precision)

    let getSymbolShapeSize = 
        match compType with
        | ROM _ | RAM _ | AsyncROM _ -> 
            failwithf "What? Legacy RAM component types should never occur"
        | And | Nand | Or | Nor | Xnor | Xor ->  (2, 1, 2*GridSize, 2*GridSize) 
        | Not -> (1, 1, 2*GridSize, 2*GridSize) 
        | ComponentType.Input (a) -> (0, 1, GridSize, 2*GridSize)                
        | ComponentType.Output (a) -> (1, 0, GridSize, 2*GridSize) 
        | ComponentType.Viewer a -> (1, 0, GridSize, GridSize) 
        | ComponentType.IOLabel  ->(1, 1, GridSize, 2*GridSize) 
        | Decode4 ->(2, 4 , 4*GridSize, 3*GridSize) 
        | Constant1 (a, b,_) | Constant(a, b) -> (0, 1, GridSize, 2*GridSize) 
        | MergeWires -> (2, 1, 2*GridSize, 2*GridSize) 
        | SplitWire (a) ->(1, 2, 2*GridSize, 2*GridSize) 
        | Mux2 -> (3, 1, 3*GridSize, 2*GridSize) 
        | Demux2 ->(2, 2, 3*GridSize, 2*GridSize) 
        | BusSelection (a, b) -> (1, 1, GridSize, 2*GridSize) 
        | BusCompare (a, b) -> (1 , 1, GridSize, 2*GridSize) 
        | DFF -> (1, 1, 3*GridSize, 3*GridSize) 
        | DFFE -> (2, 1, 3*GridSize, 3*GridSize) 
        | Register (a) -> (1, 1, 3*GridSize, 4*GridSize )
        | RegisterE (a) -> (2, 1, 3*GridSize, 4*GridSize) 
        | AsyncROM1 (a) -> (1, 1, 3*GridSize, 5*GridSize) 
        | ROM1 (a) -> (1, 1, 3*GridSize, 5*GridSize) 
        | RAM1 (a) | AsyncRAM1 a -> (3, 1, 3*GridSize, 5*GridSize) 
        | NbitsXor (n) -> (2, 1, 3*GridSize, 5*GridSize) 
        | NbitsAdder (n) -> (3, 2, 3*GridSize, 5*GridSize) 
        | Custom x -> 
            let h = GridSize + GridSize * (List.max [List.length x.InputLabels; List.length x.OutputLabels])
            let maxInLength, maxOutLength = cutToLength x.InputLabels, cutToLength x.OutputLabels
            let maxW = maxInLength + maxOutLength + compLabel.Length
            let scaledW = roundToNth GridSize (maxW * GridSize / 5) //Divide by 5 is just abitrary as otherwise the symbols would be too wide 
            let w = max scaledW (GridSize * 4) //Ensures a minimum width if the labels are very small
            
            (List.length x.InputLabels, List.length x.OutputLabels, h,  w)

        // EXTENSION:
        // | Mux4 -> (5, 1, 5*GridSize, 2*GridSize)   
        // | Mux8 -> (9, 1, 7*GridSize, 2*GridSize) 
        // | Demux4 -> (2, 4, 150, 50) 
        // | Demux8 -> (2, 8, 200, 50) 

    let makeComponent (numOfInputPorts, numOfOutputPorts, height, width) label : Component =  
        { Id = compId 
          Type = compType 
          Label = label 
          InputPorts = initPorts numOfInputPorts compId PortType.Input 
          OutputPorts  = initPorts numOfOutputPorts compId PortType.Output 
          X  = int(pos.X - float(width)/2.0) 
          Y = int(pos.Y - float(height)/2.0) 
          H = height 
          W = width }

    makeComponent getSymbolShapeSize compLabel
    
let initSymbolCharacteristics (comp: Component) = 
    match comp.Type with 
    | Nand | Nor | Xnor | Not -> { clocked = false; inverted = true }
    | DFF | DFFE | Register _ | RegisterE _ | ROM1 _ | RAM1 _ | AsyncRAM1 _ -> { clocked = true; inverted = false }
    | _ -> { clocked = false; inverted = false }

let initSymbolPoints (compType: ComponentType) compHeight compWidth : XYPos list = 
    match compType with
        | BusSelection _ | BusCompare _ -> 
            [ { X = 0; Y = 0 }; { X = 0; Y = compHeight }
              { X = (0.6*compWidth); Y = compHeight }
              { X = (0.8*compWidth); Y = (0.7*compHeight) }
              { X = compWidth; Y = (0.7*compHeight) }
              { X = compWidth; Y = (0.3*compHeight) }
              { X = (0.8*compWidth); Y = (0.3*compHeight) }
              { X = (0.6*compWidth); Y = 0 } ]
        | Constant1 _ -> 
            [ { X = 0; Y = compHeight } 
              { X = 0; Y = 0 }
              { X = compWidth/2.0; Y = compHeight/2.0 }
              { X = compWidth; Y = compHeight/2.0 } ]
        | Demux2 -> 
            [ { X = 0; Y = 0.2*compHeight }
              { X = 0; Y = 0.8*compHeight }
              { X = compWidth; Y = compHeight }
              { X = compWidth; Y = 0 } ]
        | Input _ -> 
            [ { X = 0; Y = 0 }
              { X = 0; Y = compHeight }
              { X = 0.66*compWidth; Y = compHeight }
              { X = compWidth; Y = compHeight/2.0 }
              { X = 0.66*compWidth; Y = 0 } ]
        | IOLabel ->
            [ { X = 0.33*compWidth; Y = 0 }
              { X = 0; Y = compHeight/2.0 }
              { X = 0.33/compWidth; Y = compHeight }
              { X = 0.66*compWidth; Y = compHeight }
              { X = compWidth; Y = compHeight/2.0 }
              { X = 0.66*compWidth; Y = 0 } ]
        | MergeWires -> 
            [ { X = compWidth/2.0; Y = ((1.0/6.0)*compHeight) }
              { X = compWidth/2.0; Y = ((5.0/6.0)*compHeight) }
              { X = 0; Y = ((5.0/6.0)*compHeight) }
              { X = 0; Y = ((1.0/6.0)*compHeight) }
              { X = compWidth; Y = ((1.0/2.0)*compHeight) } ]
        | Mux2 -> 
            [ { X = 0; Y = 0 }; { X = compWidth; Y = 0.2*compHeight }
              { X = compWidth; Y = 0.8*compHeight }
              { X = 0; Y = compHeight } ]
        | Output _ | Viewer _ -> 
            [ { X = 0.33*compWidth; Y = 0 }
              { X = 0; Y = compHeight/2.0 }
              { X = 0.33*compWidth; Y = compHeight }
              { X = compWidth; Y = compHeight }
              { X = compWidth; Y = 0 } ]
        | SplitWire _ -> 
            [ { X = compWidth/2.0; Y = ((1.0/6.0)*compHeight) }
              { X = compWidth/2.0; Y = ((5.0/6.0)*compHeight) }
              { X = 0; Y = ((1.0/2.0)*compHeight) }
              { X = compWidth; Y = ((1.0/6.0)*compHeight) }
              { X = compWidth; Y = ((5.0/6.0)*compHeight) } ]
        | _ -> 
            [ { X = 0; Y = compHeight }
              { X = compWidth; Y = compHeight }
              { X = compWidth; Y = 0 }
              { X = 0; Y = 0 } ]

        // EXTENSION:
        // | Mux4 | Mux8 ->(sprintf "%i,%i %i,%f  %i,%f %i,%i" 0 0 W (float(H)*0.2) W (float(H)*0.8) 0 H )
        // | Demux4 | Demux8 -> (sprintf "%i,%f %i,%f %i,%i %i,%i" 0 (float(H)*0.2) 0 (float(H)*0.8) W H W 0)

let makeSymbol (pos: XYPos) (comptype: ComponentType) (label: string) : Symbol=
    let id = JSHelpers.uuid()
    let comp = initComponent pos comptype id label

    { Pos = {X = pos.X - float(comp.W)/2.0; Y = pos.Y - float(comp.H)/2.0}
      ShowInputPorts = false
      ShowOutputPorts = false
      InWidth0 = None // set by BusWire
      InWidth1 = None
      Colour = "lightgrey"
      ComponentId = ComponentId id
      Component = comp
      Opacity = 1.0
      Moving = false
      Rotation = 0.0
      SymbolPoints = initSymbolPoints comp.Type (float(comp.H)) (float(comp.W))
      SymbolCharacteristics = initSymbolCharacteristics comp }

//--------------------------------- Port Functions ---------------------------------//

let addToPortModel (model: Model) (symbol: Symbol) =
    let addOnePort (currentPorts: Map<string, Port>) (port: Port) =
        Map.add port.Id port currentPorts
    
    let addedInputPorts = 
        (model.Ports, symbol.Component.InputPorts) ||> List.fold addOnePort

    (addedInputPorts, symbol.Component.OutputPorts) ||> List.fold addOnePort
    
let getPortPos (symbol: Symbol) (port: Port) : XYPos = 
    let inline getPortPosEdgeGap (compType: ComponentType) =
        match compType with
        | MergeWires | SplitWire _ -> 0.25
        | _ -> 1.0
    
    let (ports, posX) =
        if port.PortType = (PortType.Input) then
            symbol.Component.InputPorts, 0.0
        else 
            symbol.Component.OutputPorts, float(symbol.Component.W)

    /// Calculate equidistant port spacing
    let index = float(List.findIndex (fun (p: Port) -> p = port) ports)
    let gap = getPortPosEdgeGap symbol.Component.Type 
    let posY = (float(symbol.Component.H)) * ((index + gap)/(float(ports.Length) + 2.0*gap - 1.0))
    
    { X = posX; Y = posY }
    |> convertRelativeToSymbolCenter symbol
    |> rotatePoint symbol.Rotation
    |> convertRelativeToTopLeft symbol

let getModelPortPos (model: Model) (port: Port) =
    getPortPos (Map.find (ComponentId port.HostId) model.Symbols) port

//--------------------------------- Symbol Text Helper Functions ---------------------------------//

let private addText posX posY name txtPos weight size =
    let text = {defaultText with TextAnchor = txtPos; FontWeight = weight; FontSize = size}

    [makeText posX posY name text]

//--------------------------------- Symbol Text Functions ---------------------------------//

let private addPortText (symbol: Symbol) (portList: Port List) (listOfNames: string List) = 
    let addPortName x y name portType=
        let xPos = 
            if portType = PortType.Output then 
                match symbol.Rotation with
                | 90.0 | 270.0 -> x 
                | 180.0 -> x + 8.0
                | _ -> x - 8.0
            else 
                match symbol.Rotation with
                | 90.0 | 270.0 -> x 
                | 180.0 -> x - 10.0
                | _ -> x + 8.0

        let yPos = 
            if portType = PortType.Output then 
                match symbol.Rotation with
                | 90.0 -> y - 20.0
                | 270.0 -> y + 5.0
                | _ -> y - 5.0
            else 
                match symbol.Rotation with
                | 90.0 -> y + 8.0
                | 270.0 -> y - 20.0
                | _ -> y - 5.0
        
        let alignment = 
            match (portType, symbol.Rotation) with
            | (PortType.Output, 0.0) -> "end"
            | (PortType.Output, 180.0) -> "start"
            | (PortType.Input, 0.0) -> "start"
            | (PortType.Input, 180.0) -> "end"
            | _ -> "middle"

        addText xPos yPos name alignment "normal" "10px"

    if listOfNames.Length < 1
        then  []
        else 
            [0..(portList.Length-1)]
            |> List.map2 (fun name x -> 
                            addPortName (getPortPos symbol portList[x]).X (getPortPos symbol portList[x]).Y name portList.Head.PortType) listOfNames 
            |> List.collect id

let addPortTitle (comp: Component) = 
    match comp.Type with
    | AsyncRAM1 _ -> (["Addr"; "Din";"Wen" ],["Dout"])
    | Custom x -> (List.map fst x.InputLabels), (List.map fst x.OutputLabels)
    | Decode4 -> (["Sel";"Data"],["0"; "1";"2"; "3"])
    | Demux2 -> (["IN" ; "SEL"],["0"; "1"])
    | DFF -> (["D"],["Q"])
    | DFFE -> (["D";"EN"],["Q"])
    | Mux2 -> (["0"; "1";"SEL"],["OUT"])
    | NbitsAdder _ -> (["Cin";"A";"B"],["Sum "; "Cout"])
    | NbitsXor _ -> (["P"; "Q"], ["Out"])
    | Register _ -> (["D"],["Q"])
    | RegisterE _ -> (["D"; "EN"],["Q"])
    | ROM1 _ |AsyncROM1 _ -> (["Addr"],["Dout"])
    | RAM1 _ -> (["Addr"; "Din";"Wen" ],["Dout"])
    |_ -> ([],[])

    // EXTENSION: Extra Components made that are not currently in Issie. Can be extended later by using this code as it is .
    // |Mux4 -> (["0"; "1"; "2"; "3" ;"SEL"],["OUT"])
    // |Demux4 -> (["IN"; "SEL"],["0"; "1";"2"; "3";])
    // |Demux8 -> (["IN"; "SEL"],["0"; "1"; "2" ; "3" ; "4" ; "5" ; "6" ; "7"])
    // |Mux8 -> (["0"; "1"; "2" ; "3" ; "4" ; "5" ; "6" ; "7";"SEL"],["OUT"])
    // |_ -> ([],[])

let addSymbolLabel rotation W H label : ReactElement list = 
    let getSymbolLabelCoord : XYPos =
            match rotation with 
            | 90.0 | 270.0 ->
                {X = float(W - H)/2.0 + float(H) + 5.0; Y = float(H/2 - 8)}
            | _ -> 
                {X = float(W/2); Y = -20.0}

    let textAlignment = if (rotation = 90.0) || (rotation = 270.0) then "start" else "middle"

    addText getSymbolLabelCoord.X getSymbolLabelCoord.Y label textAlignment "normal" "16px"

let addSymbolText (comp: Component) inWidth0 inWidth1 : ReactElement list =
    let compWidth = float(comp.W)
    let compHeight = float(comp.H)

    let addBusTitle posX1 posX2 posY msb lsb =
        let text = 
            match msb = lsb, msb >= lsb with
            | _, false -> ""
            | true, _ -> $"({msb})"
            | false, _ -> $"({msb}:{lsb})"
        
        addText (float(posX1 + posX2)/2.0) (posY*float(comp.H) - 25.0) text "middle" "bold" "12px"
    
    let addTitleWithBusWidth title busWidth lsb =  
        match comp.Type with 
        | BusCompare(_, y) -> 
            if busWidth <> 1 then
                "(" + string(busWidth + lsb - 1) + ".." + string(lsb) +  ")" 
            else 
                string(lsb)
        | _ ->  
            if busWidth = 1 then 
                title 
            else 
                title + "(" + string(busWidth - 1) + "..0)"

    let addSymbolTitle (comp: Component) =
        match comp.Type with
        | And | Nand-> "&"
        | Or | Nor-> "≥1"
        | Xor | Xnor -> "=1"
        | Not -> "1"
        | Decode4 -> "Decode"
        | NbitsAdder n -> addTitleWithBusWidth "Adder" n 0
        | Register n | RegisterE n -> addTitleWithBusWidth "Register" n 0
        | AsyncROM1 _ -> "Async-ROM"
        | ROM1 _ -> "Sync-ROM"
        | RAM1 _ -> "Sync-RAM"
        | AsyncRAM1 _ -> "Async-RAM"
        | DFF -> "DFF"
        | DFFE -> "DFFE"
        | NbitsXor x -> addTitleWithBusWidth "N-bits-Xor" x 0
        | Custom x -> x.Name
        | _ -> ""

    match comp.Type with 
    | BusSelection(x,y) ->
        addText ((compWidth/2.0)-8.0) ((compHeight/3.0)-3.0) (addTitleWithBusWidth "" x y) "middle" "bold" "12px"
    | BusCompare(_,y) -> 
        addText ((compWidth/2.0)-8.0) ((compHeight/3.0)-3.0) ("=" + NumberHelpers.hex(int y)) "middle" "bold" "12px"
    | Input(x) -> 
        addText ((compWidth/2.0)-5.0) ((compHeight/3.0)-9.0) (addTitleWithBusWidth "" x 0) "middle" "bold" "12px"
    | Output(x) -> 
        addText (compWidth/2.0) ((compHeight/3.0)-9.0) (addTitleWithBusWidth "" x 0) "middle" "bold" "12px"
    | MergeWires -> 
        let lo, hi = 
            match inWidth0, inWidth1  with 
            | Some n, Some m -> n, m
            | _ -> -1,-1
        let msb = hi + lo - 1
        let midb = lo
        let midt = lo - 1

        addBusTitle 0 (comp.W/2) (1.0/6.0) midt 0 @ 
        addBusTitle 0 (comp.W/2) (5.0/6.0) msb midb @ 
        addBusTitle (comp.W/2) comp.W 0.5 msb 0

    | SplitWire mid ->  
        let msb, mid' = match inWidth0 with | Some n -> n - 1, mid | _ -> -100, -50
        let midb = mid'
        let midt = mid'-1

        addBusTitle (comp.W/2) comp.W (1.0/6.0) midt 0 @ 
        addBusTitle (comp.W/2) comp.W (5.0/6.0) msb midb @ 
        addBusTitle 0 (comp.W/2) 0.5 msb 0
    | _ ->  
        addText (compWidth/2.0) ((compHeight/2.0) - 8.5) (addSymbolTitle comp) "middle" "bold" "14px"

//--------------------------------- Symbol Draw Helpers ---------------------------------//

let addOutlineColor (color:string) =
    match color.ToLower() with
    | "lightgray" | "lightgrey" -> "black"
    | c -> 
        printfn $"color={color}"
        c

let private createPolygon colour opacity points  = 
    [makePolygon points {defaultPolygon with Fill = colour; FillOpacity = opacity}]

let drawBiColorPolygon points colour strokeColor opacity strokeWidth = 
    if strokeColor <> "black" then 
        let polygonFormatting = 
            { defaultPolygon with Fill = colour; Stroke = strokeColor; FillOpacity = opacity; StrokeWidth = strokeWidth}
        
        [makePolygon points polygonFormatting]
    else   
        let polygonFormatting = 
            { defaultPolygon with Fill = colour; FillOpacity = opacity; StrokeWidth = strokeWidth }

        [makePolygon points polygonFormatting]

let drawHorizontalLine posX1 posX2 posY opacity =
    let points = $"%f{posX1},%f{posY} %f{posX2},%f{posY}"
    createPolygon "lightgray" opacity points

let drawHorizontalColorLine posX1 posX2 posY opacity (color: string) = 
    let points = $"%f{posX1},%f{posY} %f{posX2},%f{posY}"
    let olColor = addOutlineColor color
    [makePolygon points {defaultPolygon with Fill = "olcolor"; Stroke=olColor; StrokeWidth = "2.0"; FillOpacity = opacity}]

let private drawPortCircle x y = 
    [makeCircle x y portCircle]

let drawVerticalLine posY1 posY2 posX opacity = 
    let points = $"%f{posX},%f{posY1} %f{posX},%f{posY2}"
    createPolygon "lightgray" opacity points

let drawVerticalColorLine posY1 posY2 posX opacity (color: string) = 
    let points = $"%f{posX},%f{posY1} %f{posX},%f{posY2}"
    let olColor = addOutlineColor color
    [makePolygon points {defaultPolygon with Fill = "olcolor"; Stroke=olColor; StrokeWidth = "2.0"; FillOpacity = opacity}]

//--------------------------------- Symbol Draw Functions ---------------------------------//

let private drawPorts (portList: Port List) (printPorts: bool) (symbol: Symbol) = 
    if (portList.Length > 0) && printPorts then 
        [0..(portList.Length-1)] |> List.collect (fun x -> (drawPortCircle (getPortPos symbol portList[x]).X (getPortPos symbol portList[x]).Y))
    else 
        []

let drawArrow symbol (points: XYPos list) colour outlineColor opacity strokeWidth =
    let originalSymbolPoints = 
        points 
        |> List.map (convertRelativeToSymbolCenter symbol) 
        |> List.map (rotatePoint -symbol.Rotation) 
        |> List.map (convertRelativeToTopLeft symbol)

    let trianglePoints =
        [ { X = (originalSymbolPoints[0].X + originalSymbolPoints[1].X)/2.0; Y = originalSymbolPoints[1].Y+6.0}
          { X = (originalSymbolPoints[0].X + originalSymbolPoints[1].X)/2.0; Y = originalSymbolPoints[1].Y-6.0}
          { X = (originalSymbolPoints[0].X + originalSymbolPoints[1].X)/2.0 + 6.0; Y = originalSymbolPoints[1].Y} ]
        |> List.map (convertRelativeToSymbolCenter symbol)
        |> List.map (rotatePoint symbol.Rotation) 
        |> List.map (convertRelativeToTopLeft symbol)

    drawBiColorPolygon (convertSymbolPointsToString trianglePoints) colour outlineColor opacity strokeWidth

let drawSymbolCharacteristics (symbol: Symbol) colour opacity : ReactElement list =
    let addInvertor posX posY =
        [{ X = posX; Y = posY}; { X = (posX+9.0); Y = posY}; { X = posX; Y = (posY-8.0)}]
        |> List.map (convertRelativeToSymbolCenter symbol)
        |> List.map (rotatePoint symbol.Rotation)
        |> List.map (convertRelativeToTopLeft symbol)
        |> convertSymbolPointsToString
        |> createPolygon colour opacity

    let addClock posX posY =
        let clockPoints = 
            [{ X = posX; Y = posY-1.0}; { X = posX; Y = posY-13.0}; { X = posX+8.0; Y = posY-7.0}]
            |> List.map (convertRelativeToSymbolCenter symbol)
            |> List.map (rotatePoint symbol.Rotation)
            |> List.map (convertRelativeToTopLeft symbol)
        
        // let addClockText = 
        //     match symbol.Rotation with 
        //     | 90.0 ->
        //         addText (clockPoints[2].X) (clockPoints[2].Y) "clk" "middle" "normal" "10px"
        //     | 180.0 ->
        //         addText (clockPoints[2].X - 10.0) (clockPoints[2].Y - 6.0) "clk" "middle" "normal" "10px"
        //     | 270.0 ->
        //         addText (clockPoints[2].X) (clockPoints[2].Y - 13.0) "clk" "middle" "normal" "10px"
        //     | _ -> 
        //         addText (clockPoints[2].X + 2.0) (clockPoints[2].Y - 7.0) "clk" "start" "normal" "10px"

        clockPoints
        |> convertSymbolPointsToString
        |> createPolygon colour opacity
        // |> List.append addClockText
        
    match symbol.SymbolCharacteristics with 
    | { clocked = false; inverted = true  } -> addInvertor (float(symbol.Component.W)) (float(symbol.Component.H) / 2.0)
    | { clocked = true;  inverted = false } -> addClock 0 symbol.Component.H
    | { clocked = true;  inverted = true  } -> addClock 0 symbol.Component.H @ addInvertor symbol.Component.X symbol.Component.Y
    | _ -> []

let drawSymbolShape (symbol: Symbol) opacity colour :  ReactElement list =
    let outlineColor, strokeWidth =
            match symbol.Component.Type with
            | SplitWire _ | MergeWires -> addOutlineColor colour, "2.0"
            | _ -> "black", "1.0"
    
    let drawSymbolLines = 
        if symbol.Rotation = 90.0 || symbol.Rotation = 270.0 then 
            match symbol.Component.Type with 
            | Constant1 (_,_,txt) -> 
                drawVerticalLine symbol.SymbolPoints[2].Y symbol.SymbolPoints[3].Y symbol.SymbolPoints[3].X opacity
            | MergeWires -> 
                drawVerticalColorLine symbol.SymbolPoints[1].Y symbol.SymbolPoints[2].Y symbol.SymbolPoints[2].X opacity colour
                |> List.append (drawVerticalColorLine symbol.SymbolPoints[1].Y symbol.SymbolPoints[3].Y symbol.SymbolPoints[3].X opacity colour)
                |> List.append (drawVerticalColorLine symbol.SymbolPoints[1].Y symbol.SymbolPoints[4].Y symbol.SymbolPoints[4].X opacity colour)
            | SplitWire _ -> 
                drawVerticalColorLine symbol.SymbolPoints[1].Y symbol.SymbolPoints[2].Y symbol.SymbolPoints[2].X opacity colour
                |> List.append (drawVerticalColorLine symbol.SymbolPoints[1].Y symbol.SymbolPoints[3].Y symbol.SymbolPoints[3].X opacity colour)
                |> List.append (drawVerticalColorLine symbol.SymbolPoints[1].Y symbol.SymbolPoints[4].Y symbol.SymbolPoints[4].X opacity colour)
            | _ -> []
        else
            match symbol.Component.Type with 
            | Constant1 (_,_,txt) -> 
                drawHorizontalLine symbol.SymbolPoints[2].X symbol.SymbolPoints[3].X symbol.SymbolPoints[3].Y opacity
            | MergeWires -> 
                drawHorizontalColorLine symbol.SymbolPoints[1].X symbol.SymbolPoints[2].X symbol.SymbolPoints[2].Y opacity colour
                |> List.append (drawHorizontalColorLine (int(symbol.SymbolPoints[1].X)) (int(symbol.SymbolPoints[3].X)) symbol.SymbolPoints[3].Y opacity colour)
                |> List.append (drawHorizontalColorLine (int(symbol.SymbolPoints[1].X)) (int(symbol.SymbolPoints[4].X)) symbol.SymbolPoints[4].Y opacity colour)
            | SplitWire _ -> 
                drawHorizontalColorLine (int(symbol.SymbolPoints[1].X)) (int(symbol.SymbolPoints[2].X)) symbol.SymbolPoints[2].Y opacity colour
                |> List.append (drawHorizontalColorLine (int(symbol.SymbolPoints[1].X)) (int(symbol.SymbolPoints[3].X)) symbol.SymbolPoints[3].Y opacity colour)
                |> List.append (drawHorizontalColorLine (int(symbol.SymbolPoints[1].X)) (int(symbol.SymbolPoints[4].X)) symbol.SymbolPoints[4].Y opacity colour)
            | _ -> []

    let drawDirectionalArrows : ReactElement list =
        match symbol.Component.Type with 
        | MergeWires -> 
            let arrowLines = 
                [ [{ X = symbol.SymbolPoints[0].X; Y = symbol.SymbolPoints[0].Y}; { X = symbol.SymbolPoints[2].X; Y = symbol.SymbolPoints[2].Y }]
                  [{ X = symbol.SymbolPoints[1].X; Y = symbol.SymbolPoints[1].Y}; { X = symbol.SymbolPoints[3].X; Y = symbol.SymbolPoints[3].Y }]
                  [{ X = (symbol.SymbolPoints[0].X + symbol.SymbolPoints[1].X)/2.0; Y = (symbol.SymbolPoints[0].Y + symbol.SymbolPoints[1].Y)/2.0}; { X = symbol.SymbolPoints[4].X; Y = symbol.SymbolPoints[4].Y }]]

            drawArrow symbol arrowLines[0] colour outlineColor opacity strokeWidth
            |> List.append (drawArrow symbol arrowLines[1] colour outlineColor opacity strokeWidth)
            |> List.append (drawArrow symbol arrowLines[2] colour outlineColor opacity strokeWidth)
        | SplitWire _ -> 
            let arrowLines = 
                [ [{ X = symbol.SymbolPoints[0].X; Y = symbol.SymbolPoints[0].Y}; { X = symbol.SymbolPoints[2].X; Y = symbol.SymbolPoints[2].Y}]
                  [{ X = symbol.SymbolPoints[1].X; Y = symbol.SymbolPoints[1].Y}; { X = symbol.SymbolPoints[3].X; Y = symbol.SymbolPoints[3].Y}]
                  [{ X = (symbol.SymbolPoints[0].X + symbol.SymbolPoints[1].X)/2.0; Y = (symbol.SymbolPoints[0].Y + symbol.SymbolPoints[1].Y)/2.0}; { X = symbol.SymbolPoints[4].X; Y = symbol.SymbolPoints[4].Y}]]

            drawArrow symbol arrowLines[0] colour outlineColor opacity strokeWidth
            |> List.append (drawArrow symbol arrowLines[1] colour outlineColor opacity strokeWidth)
            |> List.append (drawArrow symbol arrowLines[2] colour outlineColor opacity strokeWidth)
        | _ -> []
    
    match symbol.Component.Type with
    | Constant1 (_,_,txt) -> 
        drawBiColorPolygon (convertSymbolPointsToString symbol.SymbolPoints[0..2]) colour outlineColor opacity strokeWidth
        |> List.append drawSymbolLines
        |> List.append (addText (float (symbol.Component.W/2)-5.0) (float(symbol.Component.H)-8.0) txt "middle" "normal" "12px")
    | MergeWires -> 
        drawBiColorPolygon (convertSymbolPointsToString symbol.SymbolPoints[0..1]) colour outlineColor opacity strokeWidth
        |> List.append drawSymbolLines
        |> List.append drawDirectionalArrows
    | SplitWire _ -> 
        drawBiColorPolygon (convertSymbolPointsToString symbol.SymbolPoints[0..1]) colour outlineColor opacity strokeWidth
        |> List.append drawSymbolLines
        |> List.append drawDirectionalArrows
    | _ -> drawBiColorPolygon (getSymbolPoints symbol) colour outlineColor opacity strokeWidth

/// --------------------------------------- SYMBOL DRAWING ------------------------------------------------------ ///   

let drawSymbol (symbol: Symbol, colour: string, opacity: float) = 
    addSymbolLabel symbol.Rotation symbol.Component.W symbol.Component.H symbol.Component.Label
    |> List.append (addSymbolText symbol.Component symbol.InWidth0 symbol.InWidth1)
    |> List.append (addPortText symbol symbol.Component.InputPorts (fst(addPortTitle symbol.Component)))
    |> List.append (addPortText symbol symbol.Component.OutputPorts (snd(addPortTitle symbol.Component)))      
    |> List.append (drawPorts symbol.Component.OutputPorts symbol.ShowOutputPorts symbol)
    |> List.append (drawPorts symbol.Component.InputPorts symbol.ShowInputPorts symbol)
    |> List.append (drawSymbolCharacteristics symbol colour opacity)
    |> List.append (drawSymbolShape symbol opacity colour)

//----------------------------View Function for Symbols----------------------------//

type private RenderSymbolProps =
    {
        Symbol : Symbol 
        Dispatch : Dispatch<Msg>
        key: string 
    }

/// View for one symbol. Using FunctionComponent.Of to improve efficiency (not printing all symbols but only those that are changing)
let private renderSymbol =
    FunctionComponent.Of(
        fun (props: RenderSymbolProps) ->
            let symbol = props.Symbol
            let ({X=fX; Y = fY}: XYPos) = {X=float(symbol.Component.X); Y = float(symbol.Component.Y)}
            g ([ Style [ Transform($"translate(%f{fX}px, %f{fY}px)") ] ]) (drawSymbol(props.Symbol, symbol.Colour, symbol.Opacity))
            
        , "Symbol"
        , equalsButFunctions
        )
    
/// View function for symbol layer of SVG
let convertMapsIntoLists map =
    let listMoving = 
        Map.filter (fun _ sym -> not sym.Moving) map
        |> Map.toList
        |> List.map snd
    let listNotMoving =
        Map.filter (fun _ sym -> sym.Moving) map
        |> Map.toList
        |> List.map snd
    listMoving @ listNotMoving

let view (model: Model) (dispatch: Msg -> unit) = 
    let start = TimeHelpers.getTimeMs()
    model.Symbols
    |> convertMapsIntoLists
    |> List.map (fun ({ComponentId = ComponentId id} as symbol) ->
        renderSymbol
            {
                Symbol = symbol
                Dispatch = dispatch
                key = id
            }
    )
    |> ofList
    |> TimeHelpers.instrumentInterval "SymbolView" start

//------------------------GET BOUNDING BOXES FUNCS--------------------------------used by sheet.
// Function that returns the bounding box of a symbol. It is defined by the height and the width as well as the x,y position of the symbol
let getBoundingBoxofSymbol (sym:Symbol): BoundingBox =
    {X = float(sym.Pos.X) ; Y = float(sym.Pos.Y) ; H = float(sym.Component.H) ; W = float(sym.Component.W)}

let getBoundingBoxes (symModel: Model): Map<ComponentId, BoundingBox> =
    Map.map (fun sId (sym:Symbol) -> (getBoundingBoxofSymbol sym)) symModel.Symbols
    
let getOneBoundingBox (symModel: Model) (compid: ComponentId ): BoundingBox =
    let symb = Map.find compid symModel.Symbols
    getBoundingBoxofSymbol symb

//--------------------- GETTING PORTS AND THEIR LOCATIONS INTERFACE FUNCTIONS-------------------------------
// Helpers
let getSymbolPos (symbolModel: Model) compId =
    let symbol = Map.find compId symbolModel.Symbols
    symbol.Pos

/// This is quite slow, because it gets the whole maps.
/// It is used in getInputPortLocation for a single port!!
/// Bad
let getInputPortsPositionMap (model: Model) (symbols: Symbol list)  = 
    symbols
    |> List.collect (fun sym -> List.map (fun p -> sym,p) sym.Component.InputPorts)
    |> List.map (fun (sym,port) -> (InputPortId port.Id, posAdd (getModelPortPos model port) (sym.Pos)))
    |> Map.ofList

/// This is quite slow, because it gets the whole maps.
/// It is used in getOutputPortLocation for a single port!!
/// Bad
let getOutputPortsPositionMap (model: Model) (symbols: Symbol list)  = //These function add the coordinates of the symbol too
    symbols
    |> List.collect (fun sym -> List.map (fun p -> sym,p) sym.Component.OutputPorts)
    |> List.map (fun (sym,port) -> (OutputPortId port.Id , posAdd (getModelPortPos model port) (sym.Pos)))
    |> Map.ofList

///Returns the port object associated with a given portId
let getPort (symModel: Model) (portId: string) =
    symModel.Ports[portId]

///Returns all the port locations of the given components   
let getPortLocations (symbolModel: Model) (sIds: ComponentId list) = 
    let getSymbols = 
        symbolModel.Symbols 
        |> Map.filter (fun sId sym  -> List.contains sId sIds)
        |> Map.toList
        |> List.map snd
        
    let getInputPortMap = getInputPortsPositionMap symbolModel getSymbols
    let getOutputPortMap = getOutputPortsPositionMap symbolModel getSymbols
       
    getInputPortMap , getOutputPortMap

///Returns the location of an input portId  
let getInputPortLocation (model:Model) (portId: InputPortId)  = 
    let allSymbols =
        model.Symbols
        |> Map.toList
        |> List.map snd
        
    getInputPortsPositionMap model allSymbols 
    |> Map.find (portId)
    
//Returns the location of an output portId
let getOutputPortLocation (model:Model) (portId : OutputPortId) =
    let allSymbols =
        model.Symbols
        |> Map.toList
        |> List.map snd
        
    getOutputPortsPositionMap model allSymbols 
    |> Map.find (portId)     

///Returns the location of a given portId
let getOnePortLocation (symModel: Model) (portId : string) (pType: PortType)=
        match pType with
        | PortType.Input ->
            getInputPortLocation symModel (InputPortId portId)
        | PortType.Output ->
            getOutputPortLocation symModel (OutputPortId portId)   
            
/// Returns the location of a given portId, with better efficiency
/// This is still slow, the ports should be looked up from a map of ports
let getOnePortLocationNew (symModel: Model) (portId : string) (pType: PortType) : XYPos=
    symModel.Symbols
    |> Map.pick (fun key sym -> 
        let comp = sym.Component
        if pType = PortType.Input then
            List.tryFind (fun (po:Port) -> po.Id = portId) comp.InputPorts
        else
            List.tryFind (fun (po:Port) -> po.Id = portId) comp.OutputPorts
        |> Option.map (fun port -> posAdd (getModelPortPos symModel port) (sym.Pos)))

/// Returns the locations of a given input portId and output portId
let getTwoPortLocations (symModel: Model) (inPortId: InputPortId ) (outPortId: OutputPortId) =
    match inPortId, outPortId with
    | InputPortId inputId, OutputPortId outputId ->
        (getOnePortLocationNew symModel inputId PortType.Input, getOnePortLocationNew symModel outputId PortType.Output)

/// Interface function to get componentIds of the copied symbols
let getCopiedSymbols (symModel: Model) : (ComponentId list) =
    symModel.CopiedSymbols
    |> Map.toList
    |> List.map fst

/// Function to filter out terminal non-letter characters.
/// Modified to capitalise labels
let filterString (string: string) = 
    string.ToUpper()
    |> Seq.rev
    |> Seq.skipWhile System.Char.IsDigit
    |> Seq.rev
    |> Seq.map System.Char.ToString
    |> String.concat ""
   
///Returns the number of the component label (i.e. the number 1 from IN1 or ADDER16.1)
let regex (str : string) = 
    let index = Regex.Match(str, @"\d+$")
    match index with
    | null -> 0
    | _ -> int index.Value

let getCompList compType listSymbols =
    match compType with 
       | Not | And | Or | Xor | Nand | Nor | Xnor -> 
            listSymbols
            |> List.filter (fun sym ->
                (sym.Component.Type = Not || sym.Component.Type = And 
                || sym.Component.Type = Or || sym.Component.Type = Xor
                || sym.Component.Type = Nand || sym.Component.Type = Nor
                || sym.Component.Type = Xnor)
                )
       | DFF | DFFE -> 
            listSymbols
            |> List.filter (fun sym ->
                (sym.Component.Type = DFF || sym.Component.Type = DFFE))
       //The following components require this pattern matching in order to correctly identify all of the components in the circuit of that type
       //Normally this is because they are defined by a width as well as a type
       | Register _ | RegisterE _ ->
            listSymbols
            |> List.filter (fun sym ->
                match sym.Component.Type with 
                | Register _ | RegisterE _ -> true
                | _ -> false)
       | Constant1 _ ->
            listSymbols
            |> List.filter (fun sym ->
                match sym.Component.Type with 
                | Constant1 _ -> true
                | _ -> false)
       | Input _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Component.Type with 
               | Input _ -> true
               | _ -> false)
       | Output _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Component.Type with 
               | Output _ -> true
               | _ -> false)
       | Viewer _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Component.Type with 
               | Viewer _ -> true
               | _ -> false)
       | BusSelection _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Component.Type with 
               | BusSelection _ -> true
               | _ -> false)
       | BusCompare _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Component.Type with 
               | BusCompare _ -> true
               | _ -> false)
       | NbitsAdder _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Component.Type with 
               | NbitsAdder _ -> true
               | _ -> false)
       | NbitsXor _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Component.Type with 
               | NbitsXor _ -> true
               | _ -> false)
       | AsyncROM1 _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Component.Type with 
               | AsyncROM1 _ -> true
               | _ -> false)
       | ROM1 _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Component.Type with 
               | ROM1 _ -> true
               | _ -> false)
       | RAM1 _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Component.Type with 
               | RAM1 _ -> true
               | _ -> false)
       | AsyncRAM1 _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Component.Type with 
               | AsyncRAM1 _ -> true
               | _ -> false)

       | _ ->
            listSymbols
            |> List.filter (fun sym -> sym.Component.Type = compType)

let getIndex listSymbols compType =
    let symbolList = 
        getCompList compType listSymbols

    match compType with
    | MergeWires | SplitWire _ -> ""
    | _ ->
        if List.isEmpty symbolList then 1 
        else symbolList
            |> List.map (fun sym -> regex sym.Component.Label)
            |> List.max
            |> (+) 1
        |> string

///Generates the number to be put in the title of symbols  
let labelGenNumber (model: Model) (compType: ComponentType) (label : string) = 
    let listSymbols = List.map snd (Map.toList model.Symbols) 
    match compType with
    | IOLabel -> label
    | _ -> filterString label + (getIndex listSymbols compType)

///Generates the label for a component type
let generateLabel (model: Model) (compType: ComponentType)  =
    let getCompLabel (compType: ComponentType) = 
        match compType with
        | Not | And | Or | Xor | Nand | Nor | Xnor -> "G"
        | Mux2 -> "MUX"
        | Demux2 -> "DM"
        | NbitsAdder _ -> "A"
        | NbitsXor _ -> "XOR"
        | DFF | DFFE -> "FF"
        | Register _ | RegisterE _ -> "REG"
        | AsyncROM1 _ -> "AROM"
        | ROM1 _ -> "ROM"
        | RAM1 _ -> "RAM"
        | AsyncRAM1 _ -> "ARAM"
        | Custom c -> c.Name + (if c.Name |> Seq.last |> System.Char.IsDigit then "." else "")
        | Constant1 _ -> "C"
        | BusCompare _ -> "EQ"
        | Decode4 -> "DEC"
        | BusSelection _ -> "SEL"
        | _ -> ""

    labelGenNumber model compType (getCompLabel compType)

/// Interface function to paste symbols. Is a function instead of a message because we want an output
/// Currently drag-and-drop
let pasteSymbols (symModel: Model) (mPos: XYPos) : (Model * ComponentId list) =
    let makeSymbol (basePos: XYPos) ((currSymbolModel, pastedIdsList) : Model * ComponentId List) (oldSymbol: Symbol): Model * ComponentId List =
        let newId = JSHelpers.uuid()
        let posDiff = posDiff oldSymbol.Pos basePos
        let newPos = posAdd posDiff mPos
        
        let pastedSymbol =
            { oldSymbol with
                ComponentId = ComponentId newId
                Component = initComponent newPos oldSymbol.Component.Type newId (labelGenNumber { symModel with Symbols = currSymbolModel.Symbols } oldSymbol.Component.Type oldSymbol.Component.Label) // TODO: Change label later
                Pos = newPos
                ShowInputPorts = false
                ShowOutputPorts = false }
             
        let newSymbolMap = currSymbolModel.Symbols.Add ((ComponentId newId), pastedSymbol) // List needs to be in this order
        let newPorts = addToPortModel currSymbolModel pastedSymbol
        { currSymbolModel with Symbols = newSymbolMap; Ports = newPorts }, pastedIdsList @ [ pastedSymbol.ComponentId ]
        
    let oldSymbolsList =
        symModel.CopiedSymbols
        |> Map.toList
        |> List.map snd

    match List.sortBy (fun sym -> sym.Pos.X) oldSymbolsList with
    | baseSymbol :: _ ->
        let basePos = posAdd baseSymbol.Pos { X = (float baseSymbol.Component.W) / 2.0; Y = (float baseSymbol.Component.H) / 2.0 }
        
        ((symModel, []), oldSymbolsList) ||> List.fold (makeSymbol basePos)
    | [] -> symModel, []

/// Given two componentId list of same length and input / output ports that are in list 1, return the equivalent ports in list 2.
/// ComponentIds at same index in both list 1 and list 2 need to be of the same ComponentType
/// CompIds1 need to be in model.CopiedSymbols
let getEquivalentCopiedPorts (model: Model) (copiedIds) (pastedIds) (InputPortId copiedInputPort, OutputPortId copiedOutputPort) =
    let findEquivalentPorts compId1 compId2 =
        let copiedComponent = model.CopiedSymbols[compId1].Component
        let pastedComponent = model.Symbols[compId2].Component // TODO: These can be different for an output gate for some reason.
        
        let tryFindEquivalentPort (copiedPorts: Port list) (pastedPorts: Port list) targetPort =
            if copiedPorts.Length = 0 || pastedPorts.Length = 0
            then None
            else
                match List.tryFindIndex ( fun (port: Port) -> port.Id = targetPort ) copiedPorts with
                | Some portIndex -> 

                    Some pastedPorts[portIndex].Id // Get the equivalent port in pastedPorts. Assumes ports at the same index are the same (should be the case unless copy pasting went wrong).
                | _ -> None
        
        let pastedInputPortId = tryFindEquivalentPort copiedComponent.InputPorts pastedComponent.InputPorts copiedInputPort
        let pastedOutputPortId = tryFindEquivalentPort copiedComponent.OutputPorts pastedComponent.OutputPorts copiedOutputPort
    
        pastedInputPortId, pastedOutputPortId
        
    let foundPastedPorts =
        List.zip copiedIds pastedIds
        |> List.map (fun (compId1, compId2) -> findEquivalentPorts compId1 compId2)
    
    let foundPastedInputPort = List.collect (function | Some a, _ -> [a] | _ -> []) foundPastedPorts
    let foundPastedOutputPort = List.collect (function | _, Some b -> [b] | _ -> []) foundPastedPorts
    
    match foundPastedInputPort, foundPastedOutputPort with 
    | [pastedInputPort], [pastedOutputPort] -> Some (pastedInputPort, pastedOutputPort) 
    | _ -> None // If either of source or target component of the wire was not copied then we discard the wire
  
/// Given a model return a model with a new Symbol and also the component id
let addSymbol (model: Model) pos compType lbl =
    let newSym = makeSymbol pos compType lbl
    let newPorts = addToPortModel model newSym
    let newSymModel = Map.add newSym.ComponentId newSym model.Symbols
    { model with Symbols = newSymModel; Ports = newPorts }, newSym.ComponentId

// Helper function to change the number of bits expected in a port of each component type
let changeNumberOfBitsf (symModel:Model) (compId:ComponentId) (newBits : int) =
    
    let symbol = Map.find compId symModel.Symbols
    let newcompotype = 
        match symbol.Component.Type with
        | Input _ -> Input newBits
        | Output _ -> Output newBits
        | Viewer _ -> Viewer newBits
        | NbitsAdder _ -> NbitsAdder newBits
        | NbitsXor _ -> NbitsXor newBits
        | Register _ -> Register newBits
        | RegisterE _ -> RegisterE newBits
        | SplitWire _ -> SplitWire newBits
        | BusSelection (_,b) -> BusSelection (newBits,b)
        | BusCompare (_,b) -> BusCompare (newBits,b)
        | Constant1 (_,b,txt) -> Constant1 (newBits,b,txt)
        | c -> c

    let newcompo = {symbol.Component with Type = newcompotype}
    {symbol with Component = newcompo}

// Helper function to change the number of bits expected in the LSB port of BusSelection and BusCompare
let changeLsbf (symModel:Model) (compId:ComponentId) (newLsb:int64) =
    let symbol = Map.find compId symModel.Symbols
    let newcompotype = 
        match symbol.Component.Type with
        | BusSelection (w, _) -> BusSelection (w, int32(newLsb))
        | BusCompare (w, _) -> BusCompare (w, uint32(newLsb)) 
        | Constant1(w, _,txt) -> Constant1 (w, newLsb,txt)
        | _ -> failwithf "this shouldnt happen, incorrect call of message changeLsb"
    let newcompo = {symbol.Component with Type = newcompotype}
    {symbol with Component = newcompo}

let changeConstantf (symModel:Model) (compId:ComponentId) (constantVal:int64) (constantText: string) =
    let symbol = Map.find compId symModel.Symbols
    let newcompotype = 
        match symbol.Component.Type with
        | Constant1 (w, _, _) -> Constant1 (w, constantVal,constantText)
        | _ -> failwithf "this shouldnt happen, incorrect call of message changeLsb"
    let newcompo = {symbol.Component with Type = newcompotype}
    printfn "Changing symbol to: %A" newcompotype
    {symbol with Component = newcompo}
    
/// update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | DeleteSymbols compList ->
        let newSymbols = List.fold (fun prevModel sId -> Map.remove sId prevModel) model.Symbols compList
        { model with Symbols = newSymbols }, Cmd.none //filters out symbol with a specified id

    | AddSymbol (pos,compType, lbl) ->
        let (newModel, _) = addSymbol model pos compType lbl
        newModel, Cmd.none

    | CopySymbols compIds ->
        let copiedSymbols = Map.filter (fun compId _ -> List.contains compId compIds) model.Symbols
        { model with CopiedSymbols = copiedSymbols }, Cmd.none

    | ShowAllInputPorts ->
        { model with Symbols = Map.map (fun _ sym -> {sym with ShowInputPorts = true; ShowOutputPorts = false}) model.Symbols },
        Cmd.none

    | ShowAllOutputPorts ->
        {model with Symbols = Map.map (fun _ sym -> {sym with ShowInputPorts = false; ShowOutputPorts = true}) model.Symbols },
        Cmd.none

    | DeleteAllPorts ->
        { model with Symbols = Map.map (fun _ sym -> {sym with ShowInputPorts = false; ShowOutputPorts = false}) model.Symbols },
        Cmd.none //demo

    | ShowPorts compList -> //show ports of one component (shown in demo for a random component, sheet gives list in group phace)  find showPorts in other interfaces (above)
        let resetSymbols = Map.map (fun _ sym -> {sym with ShowInputPorts = false; ShowOutputPorts = false}) model.Symbols
        let newSymbols =
            (List.fold (fun prevSymbols sId -> Map.add sId {resetSymbols[sId] with ShowInputPorts = true; ShowOutputPorts = true} prevSymbols) resetSymbols compList)
        { model with Symbols = newSymbols }, Cmd.none

    | MoveSymbols (compList, move) -> 
        let resetSymbols = Map.map (fun _ sym -> { sym with Moving = false}) model.Symbols
        let newSymbols =
            (List.fold (fun prevSymbols sId ->
                let (newCompo: Component) = {model.Symbols[sId].Component with X = int (model.Symbols[sId].Pos.X + move.X);Y = int (model.Symbols[sId].Pos.Y + move.Y )}
                Map.add sId {model.Symbols[sId] with Moving = true; Pos = {X = (model.Symbols[sId].Pos.X + move.X);Y = (model.Symbols[sId].Pos.Y + move.Y)} ; Component = newCompo} prevSymbols) resetSymbols compList)
        { model with Symbols = newSymbols }, Cmd.none

    | SymbolsHaveError compList ->
        let resetSymbols = Map.map (fun _ sym -> {sym with Colour = "Lightgray"}) model.Symbols
        let newSymbols =
            (List.fold (fun prevSymbols sId -> Map.add sId {resetSymbols[sId] with Colour = "Red"} prevSymbols) resetSymbols compList)
        { model with Symbols = newSymbols }, Cmd.none

    | SelectSymbols compList -> //select a symbol (shown in demo for a random component, sheet gives list in group phase)
        let resetSymbols = Map.map (fun _ sym ->  { sym with Colour = "Lightgray"; Opacity = 1.0 }) model.Symbols
        let newSymbols =
            (List.fold (fun prevSymbols sId -> Map.add sId {resetSymbols[sId] with Colour = "lightgreen"} prevSymbols) resetSymbols compList)
        { model with Symbols = newSymbols }, Cmd.none  

    | ErrorSymbols (errorCompList,selectCompList,isDragAndDrop) -> 
        let resetSymbols = Map.map (fun _ sym ->  { sym with Colour = "Lightgray"; Opacity = 1.0 }) model.Symbols
        let selectSymbols =
            List.fold (fun prevSymbols sId -> 
                            if not isDragAndDrop then 
                                Map.add sId {resetSymbols[sId] with Colour = "lightgreen"} prevSymbols
                            else 
                                Map.add sId { resetSymbols[sId] with Opacity = 0.2 } prevSymbols
                        ) resetSymbols selectCompList
        let newSymbols = 
            (List.fold (fun prevSymbols sId -> Map.add sId {resetSymbols[sId] with Colour = "Red"} prevSymbols) selectSymbols errorCompList)
        { model with Symbols = newSymbols }, Cmd.none 
        
    | MouseMsg _ -> model, Cmd.none // allow unused mouse messags

    | ChangeLabel (sId, newLabel) ->
        let tempsym = Map.find sId model.Symbols
        let newcompo = {tempsym.Component with Label = newLabel}
        let addsym = {tempsym with Component = newcompo}
        { model with Symbols = Map.add sId addsym model.Symbols }, Cmd.none

    | PasteSymbols compList ->
        let newSymbols =
            (List.fold (fun prevSymbols sId -> Map.add sId { model.Symbols[sId] with Opacity = 0.4 } prevSymbols) model.Symbols compList)
        { model with Symbols = newSymbols }, Cmd.none  
    
    | ColorSymbols (compList, colour) -> 
        let newSymbols = 
            Map.map (fun sId sym -> if List.contains sId compList then {sym with Colour = string colour} else sym) model.Symbols
        { model with Symbols = newSymbols }, Cmd.none 
    
    | ChangeNumberOfBits (compId, newBits) ->
        let newsymbol = changeNumberOfBitsf model compId newBits
        let symbolswithoutone = model.Symbols.Remove compId
        let newSymbolsWithChangedSymbol = symbolswithoutone.Add (compId, newsymbol)
        { model with Symbols = newSymbolsWithChangedSymbol }, Cmd.none
    
    | ChangeLsb (compId, newLsb) -> 
        let newsymbol = changeLsbf model compId newLsb
        let symbolswithoutone = model.Symbols.Remove compId
        let newSymbolsWithChangedSymbol = symbolswithoutone.Add (compId, newsymbol)
        { model with Symbols = newSymbolsWithChangedSymbol }, Cmd.none

    | ChangeConstant (compId, newVal, newText) -> 
        let newsymbol = changeConstantf model compId newVal newText
        let symbolswithoutone = model.Symbols.Remove compId
        let newSymbolsWithChangedSymbol = symbolswithoutone.Add (compId, newsymbol)
        { model with Symbols = newSymbolsWithChangedSymbol }, Cmd.none
    
    | ResetModel -> { model with Symbols = Map.empty; Ports = Map.empty }, Cmd.none
    
    | LoadComponents comps ->
        let compIdsWithSymbols =
            comps
            |> List.map ( fun comp -> (
                                        let xyPos = {X = float comp.X; Y = float comp.Y}
                                        let (h,w) =
                                            if comp.H = -1 && comp.W = -1 then
                                                let comp' = initComponent xyPos comp.Type comp.Id comp.Label
                                                comp'.H,comp'.W
                                            else
                                                comp.H, comp.W
                                        ComponentId comp.Id,
                                        { Pos = xyPos
                                          ShowInputPorts = false   // do not show input ports initially
                                          ShowOutputPorts = false  // do not show output ports initially
                                          Colour = "lightgrey"     // initial color 
                                          ComponentId = ComponentId comp.Id
                                          Component = {comp with H=h ; W = w}
                                          Opacity = 1.0
                                          Moving = false
                                          InWidth0 = None
                                          InWidth1 = None
                                          Rotation = 0.0
                                          SymbolPoints = (initSymbolPoints comp.Type comp.H comp.W)
                                          SymbolCharacteristics = (initSymbolCharacteristics comp)
                                        }
                                        ))
        let symbolList =
            compIdsWithSymbols
            |> List.map snd

        let symbolMap =
            compIdsWithSymbols   
            |> Map.ofList
        
        let folder currModel sym =
            { currModel with Ports = addToPortModel currModel sym }
            
        let newModel = ( model, symbolList ) ||> List.fold folder
        { newModel with Symbols = symbolMap }, Cmd.none
 
    | WriteMemoryLine (compId, addr, value) ->
        let symbol = model.Symbols[compId]
        let comp = symbol.Component
        
        let newCompType =
            match comp.Type with
            | RAM1 mem -> RAM1 { mem with Data = Map.add addr value mem.Data }
            | AsyncRAM1 mem -> AsyncRAM1 { mem with Data = Map.add addr value mem.Data }
            | ROM1 mem -> ROM1 { mem with Data = Map.add addr value mem.Data }
            | AsyncROM1 mem -> AsyncROM1 { mem with Data = Map.add addr value mem.Data }
            | _ -> comp.Type
        
        let newComp = { comp with Type = newCompType }
        
        let newSymbols = Map.add compId { symbol with Component = newComp } model.Symbols
        
        { model with Symbols = newSymbols }, Cmd.none
    | WriteMemoryType (compId, memory) ->
        let symbol = model.Symbols[compId]
        let comp = symbol.Component       
        let newCompType =
            match comp.Type with
            | RAM1 mem | AsyncRAM1 mem -> memory
            | ROM1 mem -> memory
            | AsyncROM1 mem -> memory
            | _ -> 
                printfn $"Warning: improper use of WriteMemoryType on {comp} ignored"
                comp.Type
        
        let newComp = { comp with Type = newCompType }
        
        let newSymbols = Map.add compId { symbol with Component = newComp } model.Symbols
        
        { model with Symbols = newSymbols }, Cmd.none
        
// ----------------------interface to Issie----------------------------- //
let extractComponent (symModel: Model) (sId:ComponentId) : Component = 
    symModel.Symbols[sId].Component

let extractComponents (symModel: Model) : Component list =
    symModel.Symbols
    |> Map.toList
    |> List.map (fun (key, _) -> extractComponent symModel key)

let extractSymbol(symModel: Model) (sId:ComponentId) : Symbol = 
    symModel.Symbols[sId]

let extractSymbols (symModel: Model) : Symbol list =
    symModel.Symbols
    |> Map.toList
    |> List.map (fun (key, _) -> extractSymbol symModel key)

let updateModel(symModel: Model) (symbol: Symbol): Model = 
    let updateSymbolMapping =
        symModel.Symbols
        |> Map.change (symbol.ComponentId) ( fun x ->
                                               match x with
                                               | Some s -> Some symbol
                                               | None -> None
                                           )

    {CopiedSymbols = symModel.CopiedSymbols; Ports = symModel.Ports; Symbols = updateSymbolMapping}
