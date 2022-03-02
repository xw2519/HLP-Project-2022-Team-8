(*
This module draws schematics component symbols. Each symbol is associated with a unique Issie component.
*)

module Symbol
open Fable.React
open Fable.React.Props
open Elmish
open DrawHelpers
open CommonTypes
open System.Text.RegularExpressions

let print x = printfn "%A" x

/// --------- STATIC VARIABLES --------- ///

let GridSize = 30 

/// ---------- SYMBOL TYPES ---------- ///
// TODO: ComponentID removed
// TODO: POS centered

type SymbolCharacteristics =
    {
        clocked: bool 
        inverted: bool
    }

type SymbolTitles = 
    { 
        posX: float
        posY: float
        text: string
        textAlignment: string
        fontWeight: string
        fontSize: string 
    }

type Symbol =
    {
        Center: XYPos
        InWidth0: int option
        InWidth1: int option
        ComponentId : ComponentId       
        Component : Component                 
        Colour: string
        ShowInputPorts: bool
        ShowOutputPorts: bool
        Opacity: float
        Moving: bool
        Rotation: float
        SymbolPoints: XYPos list
        SymbolCharacteristics: SymbolCharacteristics
    }

type Model = {
    Symbols: Map<ComponentId, Symbol>
    CopiedSymbols: Map<ComponentId, Symbol>
    SymbolsCount: Map<ComponentType, int>
    Ports: Map<string, Port> 
    }


//----------------------------Message Type-----------------------------------//

type Msg =
    | MouseMsg of MouseT
    | AddSymbol of pos:XYPos * compType:ComponentType * lbl: string
    | CopySymbols of ComponentId list
    | DeleteSymbols of sIds:ComponentId list
    | ShowAllInputPorts | ShowAllOutputPorts | DeleteAllPorts 
    | MoveSymbols of compList: ComponentId list * move: XYPos
    | ShowPorts of ComponentId list
    | SelectSymbols of ComponentId list// Issie interface
    | SymbolsHaveError of sIds: ComponentId list
    | ChangeLabel of sId : ComponentId * newLabel : string
    | PasteSymbols of sIds: ComponentId list
    | ColorSymbols of compList : ComponentId list * colour : HighLightColor
    | ErrorSymbols of errorIds: ComponentId list * selectIds: ComponentId list * isDragAndDrop: bool
    | ChangeNumberOfBits of compId:ComponentId * NewBits:int 
    | ChangeLsb of compId: ComponentId * NewBits:int64 
    | ChangeConstant of compId: ComponentId * NewBits:int64 * NewText:string
    | ResetModel // For Issie Integration
    | LoadComponents of  Component list // For Issie Integration
    | WriteMemoryLine of ComponentId * int64 * int64 // For Issie Integration 
    | WriteMemoryType of ComponentId * ComponentType
    | RotateSymbols of ComponentId list

//---------------------------------helper types and functions----------------//

let posDiff (a: XYPos) (b: XYPos) = {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd (a: XYPos) (b: XYPos) = {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x; Y=y}

let convertDegtoRad degree = System.Math.PI * degree / 180.0 

// Once Pos is converted to read center positions, can remove 
let convertCoordtoCenter (symbol: Symbol) (portPos: XYPos) : XYPos =
    {X = portPos.X-(float(symbol.Component.W) / 2.0); Y = portPos.Y-(float(symbol.Component.H) / 2.0)}

let convertCenterCoordtoOriginal (symbol: Symbol) (portPos: XYPos) : XYPos =
    {X = portPos.X+(float(symbol.Component.W) / 2.0); Y = portPos.Y+(float(symbol.Component.H) / 2.0)}

let rotatePoint degree (xyPos: XYPos) : XYPos =
    {       
        X = (xyPos.Y * -System.Math.Sin(convertDegtoRad degree) + xyPos.X * System.Math.Cos(convertDegtoRad degree))
        Y = (xyPos.X * System.Math.Sin(convertDegtoRad degree) + xyPos.Y * System.Math.Cos(convertDegtoRad degree))
    }

let convertSymbolPointsListtoString (xyPosL: XYPos list) : string = 
    let splitXYPos accumulator (xyPos: XYPos) = accumulator + string(xyPos.X) + "," + string(xyPos.Y) + " "
    let coordinateString = ("", xyPosL) ||> List.fold splitXYPos

    coordinateString[0..(String.length coordinateString) - 2]   

// Points that specify each symbol 
let getSymbolPoints (symbol: Symbol) = convertSymbolPointsListtoString symbol.SymbolPoints
// ----- helper functions for titles ----- //





// Decodes the component type into component labels
let insertCompLabel (compType: ComponentType) = 
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
    | Custom c ->
        c.Name + (if c.Name |> Seq.last |> System.Char.IsDigit then "." else "")
    | Constant1 _ -> "C"
    | BusCompare _ -> "EQ"
    | Decode4 -> "DEC"
    | BusSelection _ -> "SEL"
    | _ -> ""

//-----------------------------Skeleton Model Type for symbols----------------//

let init () = {Symbols = Map.empty; CopiedSymbols = Map.empty; Ports = Map.empty; SymbolsCount = Map.empty}, Cmd.none



// Input and Output names of the ports depending on their ComponentType
// (input port names, output port names)
let insertPortTitle (comp: Component) = 
    match comp.Type with
    | Decode4 -> (["Sel";"Data"],["0"; "1";"2"; "3"])
    | NbitsAdder _ -> (["Cin";"A";"B"],["Sum "; "Cout"])
    | Register _ -> (["D"],["Q"])
    | RegisterE _ -> (["D"; "EN"],["Q"])
    | ROM1 _ |AsyncROM1 _ -> (["Addr"],["Dout"])
    | RAM1 _ -> (["Addr"; "Din";"Wen" ],["Dout"])
    | AsyncRAM1 _ -> (["Addr"; "Din";"Wen" ],["Dout"])
    | DFF -> (["D"],["Q"])
    | DFFE -> (["D";"EN"],["Q"])
    | Mux2 -> (["0"; "1";"SEL"],["OUT"])
    | Demux2 -> (["IN" ; "SEL"],["0"; "1"])
    | NbitsXor _ -> (["P"; "Q"], ["Out"])
    | Custom x -> (List.map fst x.InputLabels), (List.map fst x.OutputLabels)
    |_ -> ([],[])
    // EXTENSION: Extra Components made that are not currently in Issie. Can be extended later by using this code as it is .
    // |Mux4 -> (["0"; "1"; "2"; "3" ;"SEL"],["OUT"])
    // |Demux4 -> (["IN"; "SEL"],["0"; "1";"2"; "3";])
    // |Demux8 -> (["IN"; "SEL"],["0"; "1"; "2" ; "3" ; "4" ; "5" ; "6" ; "7"])
    // |Mux8 -> (["0"; "1"; "2" ; "3" ; "4" ; "5" ; "6" ; "7";"SEL"],["OUT"])
    // |_ -> ([],[])
   
let initPorts numOfPorts compId (portType: PortType) =
    if numOfPorts < 1 
    then []
    else
        [0..(numOfPorts-1)]
        |> List.collect (fun x ->
            [{
                Id = JSHelpers.uuid ()
                PortNumber = Some x
                PortType = portType
                HostId = compId
            }])

// let initSymbolTitles posX posY (comp: Component) = 

let initSymbolCharacteristics (comp: Component) = 
    match comp.Type with 
    | Nand | Nor | Xnor | Not -> {clocked=false; inverted=true}
    | DFF | DFFE | Register _ | RegisterE _ | ROM1 _ | RAM1 _ | AsyncRAM1 _ -> {clocked=true; inverted=false}
    | _ -> {clocked=false; inverted=false}

let initSymbolPoints (comp: Component) (pos: XYPos) H W : XYPos list = 

    let halfW = W/2
    let halfH = H/2

    match comp.Type with
        | BusSelection _ | BusCompare _ -> [{X=0; Y=0}; {X=0; Y=H}; {X=(0.6*float(W)); Y=H}; {X=(0.8*float(W)); Y=(0.7*float(H))}; {X=W; Y=(0.7*float(H))}; {X=W; Y=(0.3*float(H))}; {X=(0.8*float(W)); Y=(0.3*float(H))}; {X=(0.6*float(W)); Y=0}]
        | Constant1 _ -> [{X=0; Y=comp.H}; {X=halfW; Y=halfH}; {X=0; Y=0}]
        | Demux2 -> [{X=0; Y=(float(H)*0.2)}; {X=0; Y=(float(H)*0.8)}; {X=W; Y=H}; {X=W; Y=0}]
        | Input _ -> [{X=0; Y=0}; {X=0; Y=H}; {X=(float(W)*0.66); Y=H}; {X=W; Y=halfH}; {X=(float(W)*0.66); Y=0}]
        | IOLabel -> [{X=(float(W)*0.33); Y=0}; {X=0; Y=halfH}; {X=(float(W)*0.33); Y=H}; {X=(float(W)*0.66); Y=H}; {X=W; Y=halfH}; {X=(float(W)*0.66); Y=0}]
        | Output _ | Viewer _ -> [{X=(float(W)*(0.2)); Y=0}; {X=0; Y=halfH}; {X=(float(W)*(0.2)); Y=H}; {X=W; Y=H}; {X=W; Y=0}]
        | MergeWires | SplitWire _ -> [{X=halfW; Y=((1.0/6.0)*float(H))}; {X=halfW; Y=((5.0/6.0)*float(H))}]
        | Mux2 -> [{X=0; Y=0}; {X=W; Y=(float(H)*0.2)}; {X=W; Y=(float(H)*0.8)}; {X=0; Y=H}]
        | _ -> [{X=0; Y=H}; {X=W; Y=H}; {X=W; Y=0}; {X=0; Y=0}]
        // EXTENSION: |Mux4|Mux8 ->(sprintf "%i,%i %i,%f  %i,%f %i,%i" 0 0 W (float(H)*0.2) W (float(H)*0.8) 0 H )
        // EXTENSION: | Demux4 |Demux8 -> (sprintf "%i,%f %i,%f %i,%i %i,%i" 0 (float(H)*0.2) 0 (float(H)*0.8) W H W 0)

//-----------------------Skeleton Message type for symbols---------------------//

let cutToLength (lst : (string * int) list) =
    let labelList = List.map (fst >> String.length) lst

    // If a component has no inputs or outputs list max will fail
    if List.isEmpty labelList then 0 
    else List.max labelList

let initialiseComponent (pos: XYPos) (compType: ComponentType) (compId: string) (compLabel: string) : Component =

    // Rounds an integer to any given number. 
    let roundToNth (precision : int) (x : int) = x + abs((x % precision) - precision)
    
    // match statement for each component type. the output is a 4-tuple that is used as an input to make component (see below)
    // 4-tuple of the form ( number of input ports, number of output ports, Height, Width)
    let args = 
        match compType with
        | ROM _ | RAM _ | AsyncROM _ -> 
            failwithf "What? Legacy RAM component types should never occur"
        | And | Nand | Or | Nor | Xnor | Xor ->  (2 , 1, 2*GridSize , 2*GridSize) 
        | Not -> ( 1 , 1, 2*GridSize ,  2*GridSize) 
        | ComponentType.Input (a) -> ( 0 , 1, GridSize ,  2*GridSize)                
        | ComponentType.Output (a) -> (  1 , 0, GridSize ,  2*GridSize) 
        | ComponentType.Viewer a -> (  1 , 0, GridSize ,  GridSize) 
        | ComponentType.IOLabel  ->(  1 , 1, GridSize ,  2*GridSize) 
        | Decode4 ->( 2 , 4 , 4*GridSize  , 3*GridSize) 
        | Constant1 (a, b,_) | Constant(a, b) -> (  0 , 1, GridSize ,  2*GridSize) 
        | MergeWires -> ( 2 , 1, 2*GridSize ,  2*GridSize) 
        | SplitWire (a) ->(  1 , 2 , 2*GridSize ,  2*GridSize) 
        | Mux2 -> ( 3  , 1, 3*GridSize ,  2*GridSize) 
        | Demux2 ->( 2  , 2, 3*GridSize ,  2*GridSize) 
        | BusSelection (a, b) -> (  1 , 1, GridSize,  2*GridSize) 
        | BusCompare (a, b) -> ( 1 , 1, GridSize ,  2*GridSize) 
        | DFF -> (  1 , 1, 3*GridSize  , 3*GridSize) 
        | DFFE -> ( 2  , 1, 3*GridSize  , 3*GridSize) 
        | Register (a) -> ( 1 , 1, 3*GridSize  , 4*GridSize )
        | RegisterE (a) -> ( 2 , 1, 3*GridSize  , 4*GridSize) 
        | AsyncROM1 (a)  -> (  1 , 1, 3*GridSize  , 4*GridSize) 
        | ROM1 (a) -> (   1 , 1, 3*GridSize  , 4*GridSize) 
        | RAM1 (a) | AsyncRAM1 a -> ( 3 , 1, 3*GridSize  , 4*GridSize) 
        | NbitsXor (n) -> (  2 , 1, 3*GridSize  , 4*GridSize) 
        | NbitsAdder (n) -> (  3 , 2, 3*GridSize  , 4*GridSize) 
        | Custom x -> 
            let h = GridSize + GridSize * (List.max [List.length x.InputLabels; List.length x.OutputLabels])
            let maxInLength, maxOutLength = cutToLength x.InputLabels, cutToLength x.OutputLabels
            let maxW = maxInLength + maxOutLength + compLabel.Length
            let scaledW = roundToNth GridSize (maxW * GridSize / 5) //Divide by 5 is just abitrary as otherwise the symbols would be too wide 
            let w = max scaledW (GridSize * 4) //Ensures a minimum width if the labels are very small
            ( List.length x.InputLabels, List.length x.OutputLabels, h ,  w)

        // EXTENSION:    | Mux4 -> ( 5  , 1, 5*GridSize ,  2*GridSize)   
        // EXTENSION:    | Mux8 -> ( 9  , 1, 7*GridSize ,  2*GridSize) 
        // EXTENSION:   | Demux4 -> ( 2  , 4, 150 ,  50) 
        // EXTENSION:    | Demux8 -> ( 2  , 8, 200 ,  50) 

    // function that helps avoid duplicate code by initialising parameters that are the same for all component types and takes as argument the others
    let makeComponent (numOfInputPorts, numOfOutputPorts, height, width) label : Component =  
        {
            Id = compId 
            Type = compType 
            Label = label 
            InputPorts = initPorts numOfInputPorts compId PortType.Input 
            OutputPorts  = initPorts numOfOutputPorts compId PortType.Output 
            X  = int (pos.X - float width / 2.0) 
            Y = int (pos.Y - float height / 2.0) 
            H = height 
            W = width
        }

    makeComponent args compLabel
   
// Function to generate a new symbol
// TODO: Change to component ID
let makeSymbol (pos: XYPos) (comptype: ComponentType) (label: string) =
    let id = JSHelpers.uuid ()
    let comp = initialiseComponent pos comptype id label
    { 
      Center = {X = pos.X ; Y = pos.Y }
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
      SymbolPoints = (initSymbolPoints comp pos comp.H comp.W)
      SymbolCharacteristics = (initSymbolCharacteristics comp)
    }

// Function to add ports to port model     
let addToPortModel (model: Model) (sym: Symbol) =
    let addOnePort (currentPorts: Map<string, Port>) (port: Port) =
        Map.add port.Id port currentPorts
    
    let addedInputPorts = 
        (model.Ports, sym.Component.InputPorts) ||> List.fold addOnePort

    (addedInputPorts, sym.Component.OutputPorts) ||> List.fold addOnePort

//----------------------------------------- GET PORT POSITION---------------------------------------------------
// Function that calculates the positions of the ports 

let getPortPos (symbol: Symbol) (port: Port) : XYPos = 
    /// hack so that bounding box of splitwire, mergewires can be smaller height relative to ports
    let inline getPortPosEdgeGap (compType: ComponentType) =
        match compType with
        | MergeWires | SplitWire _  -> 0.25
        | _ -> 1.0
    
    let (ports, posX) =
        if port.PortType = (PortType.Input) then
            (symbol.Component.InputPorts, 0.0)
        else 
            (symbol.Component.OutputPorts, float( symbol.Component.W ))

    let index = float( List.findIndex (fun (p:Port)  -> p = port) ports )
    let gap = getPortPosEdgeGap symbol.Component.Type 
    let posY = (float(symbol.Component.H))* (( index + gap )/( float( ports.Length ) + 2.0*gap - 1.0))  // the ports are created so that they are equidistant

    // Rotation logic
    {X = posX; Y = posY}
    |> convertCoordtoCenter symbol
    |> rotatePoint symbol.Rotation
    |> convertCenterCoordtoOriginal symbol

let getModelPortPos (model: Model) (port: Port) =
    getPortPos (Map.find (ComponentId port.HostId) model.Symbols) port

//----------------------------------------- DRAWING HELPERS ---------------------------------------------------
// Text adding function with many parameters (such as bold, position and text)
let private insertText posX posY name txtPos weight size =
    let text = {defaultText with TextAnchor = txtPos; FontWeight = weight; FontSize = size}

    [makeText posX posY name text]

// Generate circles
let private drawPortCircle x y = 
    [makeCircle x y portCircle]

// Print the name of each port 
let private insertPortText (portList: Port List) (listOfNames: string List) (symbol: Symbol) = 
    // Define the name of each port 
    let addPortName x y name portType=
        let xPos = if portType = PortType.Output then x - 5. else x + 5.
        let test = if portType = PortType.Output then "end" else "start"
        
        (insertText xPos (y - 7.0) name test "normal" "12px")
    
    if listOfNames.Length < 1
        then  []
        else 
            [0..(portList.Length-1)]
            |> List.map2 (fun name x -> (addPortName (getPortPos symbol portList[x]).X (getPortPos symbol portList[x]).Y name (portList.Head.PortType))) listOfNames 
            |> List.collect id

// Function to draw ports using getPortPos. The ports are equidistant     
let private drawPorts (portList: Port List) (printPorts: bool) (symbol: Symbol) = 
    if (portList.Length)  < 1 
    then []
    else
        if printPorts
        then [0..(portList.Length-1)] |> List.collect (fun x -> (drawPortCircle (getPortPos symbol portList[x]).X (getPortPos symbol portList[x]).Y))
        else []

//------------------------------ HELPER FUNCTIONS FOR DRAWING SYMBOLS-------------------------------------

let private createPolygon colour opacity points  = 
    [makePolygon points {defaultPolygon with Fill = colour; FillOpacity = opacity}]

let drawBiColorPolygon points colour strokeColor opacity strokeWidth = 
    if strokeColor <> "black" then 
        [makePolygon points {defaultPolygon with Fill = colour; Stroke = strokeColor; FillOpacity = opacity; StrokeWidth=strokeWidth}]
    else   
        [makePolygon points {defaultPolygon with Fill = colour; FillOpacity = opacity; StrokeWidth = strokeWidth}]

let addHorizontalLine posX1 posX2 posY opacity = // TODO: Line instead of polygon?
    let points = (sprintf "%i,%f %i,%f" posX1 posY posX2 posY)
    createPolygon "lightgray" opacity points

let addOutlineColor (color:string) =
    match color.ToLower() with
    | "lightgray" | "lightgrey" -> "black"
    | c -> 
        //printfn $"color={color}"
        c

let addHorizontalColorLine posX1 posX2 posY opacity (color: string) = // TODO: Line instead of polygon?
    let points = (sprintf "%i,%f %i,%f" posX1 posY posX2 posY)
    let olColor = addOutlineColor color
    [makePolygon points {defaultPolygon with Fill = "olcolor"; Stroke=olColor; StrokeWidth = "2.0"; FillOpacity = opacity}]

let rotateSymbol (symbol: Symbol) = 
    // For debugging purposes: Get next rotation
    let nextRotation = 
        [   0.0, 90.0;
            90.0, 180.0;
            180.0, 270.0;
            270.0, 0.0  ]
        |> Map.ofList

    // Handle merge and split wires exception
    let newSymbolPoints = 
        symbol.SymbolPoints
        |> List.map (convertCoordtoCenter symbol)
        |> List.map (rotatePoint 90.0)
        |> List.map (convertCenterCoordtoOriginal symbol)

    {symbol with Rotation = nextRotation.[symbol.Rotation]; SymbolPoints = newSymbolPoints}

let drawSymbolCharacteristics (symbol: Symbol) colour opacity : ReactElement list =
    let addInvertor posX posY =
        [{X=posX; Y=posY}; {X=(posX+9.0); Y=posY}; {X=posX; Y=(posY-8.0)}]
        |> List.map (convertCoordtoCenter symbol)
        |> List.map (rotatePoint symbol.Rotation)
        |> List.map (convertCenterCoordtoOriginal symbol)
        |> convertSymbolPointsListtoString
        |> (createPolygon colour opacity)

    let addClock posX posY =
        let clockLabelPos = 
            {X=posX+10.0; Y=posY-13.0}
            |> convertCoordtoCenter symbol
            |> rotatePoint symbol.Rotation
            |> convertCenterCoordtoOriginal symbol
            
        [{X=posX; Y=posY-1.0}; {X=(posX+8.0); Y=posY-7.0}; {X=posX; Y=(posY-13.0)}]
        |> List.map (convertCoordtoCenter symbol)
        |> List.map (rotatePoint symbol.Rotation)
        |> List.map (convertCenterCoordtoOriginal symbol)
        |> convertSymbolPointsListtoString
        |> (createPolygon colour opacity)
        |> List.append (insertText clockLabelPos.X clockLabelPos.Y "clk" "start" "normal" "12px")
        
    match symbol.SymbolCharacteristics with 
    | { clocked = false; inverted = true  } -> addInvertor (float(symbol.Component.W)) (float(symbol.Component.H) / 2.0)
    | { clocked = true;  inverted = false } -> addClock 0 symbol.Component.H
    | { clocked = true;  inverted = true  } -> addClock 0 symbol.Component.H @ addInvertor symbol.Component.X symbol.Component.Y
    | _ -> []

let addSymbolText (comp: Component) : ReactElement list =
    // Insert titles compatible with greater than 1 buswidth
    let insertSymbolTitle title busWidth =  
        if busWidth = 1 then title else title + "(" + string(busWidth-1) + "..0)"
    
    // Insert titles for bus select
    let insertBusTitle busWidth lsb = 
        if busWidth <> 1 then"(" + string(busWidth + lsb - 1) + ".." + string(lsb) +  ")" else string(lsb)

    // Text to be put inside different Symbols depending on their ComponentType
    let getSymbolTitle (comp: Component) =
        match comp.Type with
        | And | Nand-> "&"
        | Or | Nor-> "≥1"
        | Xor | Xnor -> "=1"
        | Not -> "1"
        | Decode4 -> "Decode"
        | NbitsAdder n -> insertSymbolTitle "Adder" n
        | Register n | RegisterE n -> insertSymbolTitle "Register" n
        | AsyncROM1 _ -> "Async-ROM"
        | ROM1 _ -> "Sync-ROM"
        | RAM1 _ -> "Sync-RAM"
        | AsyncRAM1 _ -> "Async-RAM"
        | DFF -> "DFF"
        | DFFE -> "DFFE"
        | NbitsXor (x)->   insertSymbolTitle "N-bits-Xor" x
        | Custom x -> x.Name
        | _ -> ""

    match comp.Type with 
    | Input (x) -> 
        (insertText  (float(comp.W/2)-5.0) ((float(comp.H)/2.7)-3.0) (insertSymbolTitle "" x) "middle" "normal" "12px")
    | Output (x) -> 
        (insertText  (float(comp.W/2)) ((float(comp.H)/2.7)-3.0) (insertSymbolTitle "" x) "middle" "normal" "12px")
    | BusSelection(x,y) ->
        (insertText  (float(comp.W/2)-5.0) ((float(comp.H)/2.7)-2.0) (insertBusTitle x y) "middle" "normal" "12px")
    | BusCompare (_,y) -> 
        (insertText  (float(comp.W/2)-6.0) (float(comp.H)/2.7-1.0) ("=" + NumberHelpers.hex(int y)) "middle" "bold" "10px")
    | _ ->  
        (insertText (float(comp.W/2)) (+5.0) (getSymbolTitle comp) "middle" "bold" "14px") 
        |> List.append (insertText (float(comp.W/2)) (-20.0) comp.Label "middle" "normal" "16px")

let flipSymbol (symbol: Symbol) = 
    print "Flip Triggered"

/// --------------------------------------- SYMBOL DRAWING ------------------------------------------------------ ///   
let drawSymbol 
    (
        symbol: Symbol,
        comp: Component,
        colour: string,
        showInputPorts: bool,
        showOutputPorts: bool,
        opacity: float
    ) = 

    let h = comp.H
    let w = comp.W
    let halfW = comp.W/2
    let halfH = (comp.H)/2

    let mergeSplitLine posX1 posX2 posY msb lsb =
        let text = 
            match msb = lsb, msb >= lsb with
            | _, false -> ""
            | true, _ -> sprintf $"({msb})"
            | false, _ -> sprintf $"({msb}:{lsb})"
        addHorizontalColorLine posX1 posX2 (posY*float(h)) opacity colour @
        insertText (float (posX1 + posX2)/2.0) (posY*float(h)-11.0) text "middle" "bold" "9px"

    // Helper function to add certain characteristics on specific symbols (inverter, enables, clocks)
    let additions = 
        match comp.Type with
        // Write Custom Name
        | Constant1 (_,_,txt) -> (addHorizontalLine halfW w (float(halfH)) opacity @ insertText (float (halfW)-5.0) (float(h)-8.0) txt "middle" "normal" "12px") 

        | MergeWires -> 
            let lo, hi = 
                match symbol.InWidth0, symbol.InWidth1  with 
                | Some n, Some m  -> n, m
                | _ -> -1,-1
            let msb = hi + lo - 1
            let midb = lo
            let midt = lo - 1
            mergeSplitLine 0 halfW (1.0/6.0) midt 0 @ 
            mergeSplitLine 0 halfW (10.0/6.0) msb midb @ 
            mergeSplitLine halfW w 0.5 msb 0
        | SplitWire mid -> 
            let msb, mid' = match symbol.InWidth0 with | Some n -> n - 1, mid | _ -> -100, -50
            let midb = mid'
            let midt = mid'-1
            mergeSplitLine halfW w (1.0/6.0) midt 0 @ 
            mergeSplitLine halfW w (5.0/6.0) msb midb @ 
            mergeSplitLine 0 halfW 0.5 msb 0
        
        | _ -> []

    //print symbol
    //print (getSymbolPoints symbol)

    let outlineColor, strokeWidth =
        match comp.Type with
        | SplitWire _ | MergeWires -> addOutlineColor colour, "2.0"
        | _ -> "black", "1.0"
   
    // Put everything together 
    (drawPorts comp.OutputPorts showOutputPorts symbol)
    |> List.append (drawPorts comp.InputPorts showInputPorts symbol)
    |> List.append (insertPortText comp.InputPorts (fst(insertPortTitle comp)) symbol)
    |> List.append (insertPortText comp.OutputPorts (snd(insertPortTitle comp)) symbol)  
    |> List.append (additions)
    |> List.append (addSymbolText comp)
    |> List.append (drawSymbolCharacteristics symbol colour opacity)
    |> List.append (drawBiColorPolygon (getSymbolPoints symbol) colour outlineColor opacity strokeWidth)

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
            let ({X=fX; Y=fY}: XYPos) = {X=float(symbol.Component.X); Y=float(symbol.Component.Y)}
            g ([ Style [ Transform(sprintf "translate(%fpx, %fpx)" fX fY) ] ]) (drawSymbol(props.Symbol, props.Symbol.Component, symbol.Colour, symbol.ShowInputPorts, symbol.ShowOutputPorts, symbol.Opacity))
            
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




//-----------------------------------------------------------------------------------------------------------------------------//
//---------------------------------------------------LG519 CODE STARTS---------------------------------------------------------//
//-----------------------------------------------------------------------------------------------------------------------------//



//----------------------------------------------GET BOUNDING BOXES FUNCTIONS---------------------------------------------------//

/// Returns the bounding box of a symbol
let getSymBoundingBox (sym:Symbol): BoundingBox =

    let height = float sym.Component.H
    let width = float sym.Component.W

    match sym.Rotation with
    | 0.0 | 180.0 -> 
        let topLeftCorner = {X= float(sym.Component.X); Y = float(sym.Component.Y)}
        {X = topLeftCorner.X ; Y = topLeftCorner.Y ; H = height ; W =  width}
    | 90.0 | 270.0-> 
        let topLeftCorner = {X = sym.Center.X - height/2.0; Y = sym.Center.Y - width/2.0}
        {X = topLeftCorner.X ; Y = topLeftCorner.Y ; H = width ; W = height}
    | _ -> failwithf $"Rotation of {sym.Rotation} degrees is not allowed "

/// Returns the bounding boxes of every symbol in the model
let getModelBoundingBoxes (model: Model): Map<ComponentId, BoundingBox> =
    Map.map (fun symId sym -> (getSymBoundingBox sym)) model.Symbols

/// Returns the bounding box of the symbol associated with compId
let getCmpBoundingBox (model: Model) (compId: ComponentId ): BoundingBox =
    let sym = Map.find compId model.Symbols
    getSymBoundingBox sym


//------------------------------------- GETTING PORTS AND THEIR LOCATIONS INTERFACE FUNCTIONS -------------------------------------//

/// Returns the port associated with portId
let getPort (model: Model) (portId: string) =
    model.Ports[portId]

/// Returns the Input Ports and Output Ports locations associated with componentIds
let getCmpsPortLocations (model: Model) (componentIds: ComponentId list) = 
    let getInputPortsPositionMap (model: Model) (symbols: Symbol list)  = 
        symbols
        |> List.collect (fun sym -> List.map (fun port -> sym,port) sym.Component.InputPorts)
        |> List.map (fun (sym,port) -> (InputPortId port.Id, posAdd (getModelPortPos model port) ({X= sym.Component.X; Y=sym.Component.Y})))
        |> Map.ofList


    let getOutputPortsPositionMap (model: Model) (symbols: Symbol list)  = 
        symbols
        |> List.collect (fun sym -> List.map (fun port -> sym,port) sym.Component.OutputPorts)
        |> List.map (fun (sym,port) -> (OutputPortId port.Id , posAdd (getModelPortPos model port) ({X= sym.Component.X; Y=sym.Component.Y})))
        |> Map.ofList

    let symbols = 
        model.Symbols 
        |> Map.filter (fun sId sym  -> List.contains sId componentIds)
        |> Map.toList
        |> List.map snd
        
    let InputPortsPositions = getInputPortsPositionMap model symbols
    let OutputPortsPositions = getOutputPortsPositionMap model symbols
       
    InputPortsPositions , OutputPortsPositions

/// Returns the location of the input port associated with inPortId  
let getInputPortLocation (model:Model) (inPortId: InputPortId)  =     
    match inPortId with
    | InputPortId(str) -> 
        let inPort = Map.find str model.Ports
        let symbol = Map.find (ComponentId(inPort.HostId)) model.Symbols
        posAdd (getModelPortPos model inPort) {X= symbol.Component.X; Y=symbol.Component.Y}
    
    
/// Returns the location of the output port associated with outPortId
let getOutputPortLocation (model:Model) (outPortId : OutputPortId) =
    match outPortId with
    | OutputPortId(str) -> 
        let outPort = Map.find str model.Ports
        let symbol = Map.find (ComponentId(outPort.HostId)) model.Symbols
        posAdd (getModelPortPos model outPort) {X= symbol.Component.X; Y=symbol.Component.Y}


//----------------------------  LABELS AND COPY SYMBOLS -------------------------------------//

    
///Generates the label for a given ComponentType
let genCmpLabel (model: Model) (cmpType: ComponentType) : string =
    ///Genertates the number of the component label (i.e. the number 1 from IN1 or XOR1)
    let genCmpIndex model cmpType = 
        match Map.tryFind cmpType model.SymbolsCount with
        | Some count -> count+1 |> string
        | None -> 1 |> string

    let label = insertCompLabel cmpType
    match cmpType with
    | IOLabel -> label
    | _ -> label.ToUpper() + (genCmpIndex model cmpType)

///Updates the model's SymbolsCount by increasing the count associated with cmpType
let addSymToSymbolsCount cmpType model  =
    match Map.tryFindKey (fun cType cCount -> cType = cmpType) model.SymbolsCount with
        | Some cmp -> 
            // Increase count of componentType
            Map.change cmp (Option.map (fun n -> n+1)) model.SymbolsCount
        | None -> 
            // Add new componentType to map with count of 1
            Map.add cmpType 1 model.SymbolsCount

/// Updates the model's SymbolsCount by decreasing the count associated with cmpIds
let removeSymsFromSymbolsCount cmpIds model =

    let getCmpIndex (str : string) = 
        let index = Regex.Match(str, @"\d+$")
        match index with
        | null -> 0
        | _ -> int index.Value

    let removeSym model symbolCount cmpId =
        let cmpType = model.Symbols[cmpId].Component.Type
        let cmpIndex = getCmpIndex model.Symbols[cmpId].Component.Label
        Map.change cmpType (Option.map (fun n -> if n = cmpIndex then n-1 else n)) symbolCount

    List.fold (removeSym model) model.SymbolsCount cmpIds

/// Return an updated model containing a new Symbol.
/// The Symbol is centered at symCenter, contains a Component of type cmpType
/// and has a label symLabel
let addSymToModel (model: Model) symCenter cmpType symLabel =
    let sym = makeSymbol symCenter cmpType symLabel
    let ports = addToPortModel model sym
    let updatedSyms = Map.add sym.ComponentId sym model.Symbols
    let updatedCount = addSymToSymbolsCount sym.Component.Type model 
    { model with Symbols = updatedSyms; Ports = ports; SymbolsCount = updatedCount }, sym.ComponentId


/// Returns the componentIds of the Symbols in CopiedSymbols
let getCopiedSymbolsIds (model: Model) : (ComponentId list) =
    model.CopiedSymbols
    |> Map.toList
    |> List.map fst


/// Retrns a tuple (updatedModel, pastedSymbolsIds).
/// updatedModel is a new model where the symbols in model.CopiedSymbols have been added to model.Symbols.
/// pastedSymbolsIds is a list containing pasted symbols Ids
let pasteSymbols (model: Model) (mousePos: XYPos) : (Model * ComponentId list) =
    
    /// folder function used to update the State (Model, list of pasted symbols) with a list of copied symbols
    let genPastedSyms (referencePos: XYPos) ((currModel, pastedIds) : Model * ComponentId List) (copiedSym: Symbol): Model * ComponentId List =
        // generate pastedSymbol
        let id = JSHelpers.uuid()
        let offsetFromReferencePos = posDiff copiedSym.Center referencePos
        let pastedSymCenter = posAdd offsetFromReferencePos mousePos
        let pastedSymLabel = genCmpLabel { model with Symbols = currModel.Symbols } copiedSym.Component.Type
        let pastedCmp = initialiseComponent pastedSymCenter copiedSym.Component.Type id pastedSymLabel

        let pastedSymbol =
            { copiedSym with
                ComponentId = ComponentId id
                Component = pastedCmp 
                Center = pastedSymCenter
                ShowInputPorts = false
                ShowOutputPorts = false }
                
        // update currModel with pastedSymbol  
        let updatedSymbols = currModel.Symbols.Add ((ComponentId id), pastedSymbol) 
        let updatedPorts = addToPortModel currModel pastedSymbol
        let updatedModel = { currModel with Symbols = updatedSymbols; Ports = updatedPorts }

        // update pastedSymbolsIds with pastedSymbol
        let pastedSymbolsIds = pastedIds @ [ pastedSymbol.ComponentId ]
        updatedModel, pastedSymbolsIds

    // Symbols that need to be pasted  
    let copiedSyms =
        model.CopiedSymbols
        |> Map.toList
        |> List.map snd
    
    // Order copiedSyms based on their X coordinate.
    let copiedSymsSorted = List.sortBy (fun sym -> sym.Component.X) copiedSyms

    match copiedSymsSorted with
    | referenceSymbol :: _ ->
        let referencePos = referenceSymbol.Center 
        let updatedModel, pastedSymbolsIds = ((model, []), copiedSyms) ||> List.fold (genPastedSyms referencePos)
        updatedModel, pastedSymbolsIds
    | [] -> model, []

    


/// Returns the pasted ports associated with copiedInputPortId and copiedOutputPortId
let getPastedPortsIdsFromCopiedPortsIds (model: Model) (copiedCmpIds) (pastedCmpIds) (InputPortId copiedInputPortId, OutputPortId copiedOutputPortId) =
    let tryFindPastedPorts copiedCmpId pastedCmpId =

        let copiedComponent = model.CopiedSymbols[copiedCmpId].Component
        let pastedComponent = model.Symbols[pastedCmpId].Component 
        
        /// Returns the pasted port associated with copiedPort
        let tryFindPastedPort (copiedPorts: Port list) (pastedPorts: Port list) copiedPort =
            if copiedPorts.Length = 0 || pastedPorts.Length = 0
            then None
            else
                let index = List.tryFindIndex ( fun (port: Port) -> port.Id = copiedPort ) copiedPorts
                match index with
                | Some portIndex -> 
                    Some pastedPorts[portIndex].Id 
                | _ -> None
        
        let pastedInputPortId = tryFindPastedPort copiedComponent.InputPorts pastedComponent.InputPorts copiedInputPortId
        let pastedOutputPortId = tryFindPastedPort copiedComponent.OutputPorts pastedComponent.OutputPorts copiedOutputPortId
    
        pastedInputPortId, pastedOutputPortId
        
    let foundPastedPorts =
        List.zip copiedCmpIds pastedCmpIds
        |> List.map (fun (copiedCmpId, pastedCmpId) -> tryFindPastedPorts copiedCmpId pastedCmpId)
    
    let foundPastedInputPortId = List.collect (function | Some a, _ -> [a] | _ -> []) foundPastedPorts
    let foundPastedOutputPortId = List.collect (function | _, Some b -> [b] | _ -> []) foundPastedPorts
    
    match foundPastedInputPortId, foundPastedOutputPortId with 
    | [pastedInputPortId], [pastedOutputPortId] -> Some (pastedInputPortId, pastedOutputPortId) 
    | _ -> 
        // If either of source or target component of the wire was not copied then we discard the wire
        None 
  
 

/// Returns a new Symbol who's Component number of bits expected in a port is set according to bits
let updateCmpNumOfBits (model:Model) (cmpId:ComponentId) (bits : int) =   
    let sym = Map.find cmpId model.Symbols
    let updatedCmpType = 
        match sym.Component.Type with
        | Input _ -> Input bits
        | Output _ -> Output bits
        | Viewer _ -> Viewer bits
        | NbitsAdder _ -> NbitsAdder bits
        | NbitsXor _ -> NbitsXor bits
        | Register _ -> Register bits
        | RegisterE _ -> RegisterE bits
        | SplitWire _ -> SplitWire bits
        | BusSelection (_,b) -> BusSelection (bits,b)
        | BusCompare (_,b) -> BusCompare (bits,b)
        | Constant1 (_,b,txt) -> Constant1 (bits,b,txt)
        | c -> c
    let updatedCmp = {sym.Component with Type = updatedCmpType}
    {sym with Component = updatedCmp}

/// Returns a new Symbol who's Component number of bits expected in the LSB a port is set according to lsb
let updateLSBNumOfBits (model:Model) (cmpId:ComponentId) (lsb:int64) =
    let sym = Map.find cmpId model.Symbols
    let updatedCmpType = 
        match sym.Component.Type with
        | BusSelection (w, _) -> BusSelection (w, int32(lsb))
        | BusCompare (w, _) -> BusCompare (w, uint32(lsb)) 
        | Constant1(w, _,txt) -> Constant1 (w, lsb,txt)
        | _ -> failwithf "this shouldnt happen, incorrect call of message changeLsb"
    let updatedCmp = {sym.Component with Type = updatedCmpType}
    {sym with Component = updatedCmp}


let updateConstant (model:Model) (cmpId:ComponentId) (constantVal:int64) (constantText: string) =
    let sym = Map.find cmpId model.Symbols
    let updatedCmpType = 
        match sym.Component.Type with
        | Constant1 (w, _, _) -> Constant1 (w, constantVal,constantText)
        | _ -> failwithf "this shouldnt happen, incorrect call of message changeConstant"
    let updatedCmp = {sym.Component with Type = updatedCmpType}
    {sym with Component = updatedCmp}
    

//---------------------------------- UPDATE FUNCTION -------------------------------//


/// Update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | DeleteSymbols compIds ->
        let newSymbols = List.fold (fun prevModel sId -> Map.remove sId prevModel) model.Symbols compIds
        let newSymbolCount = removeSymsFromSymbolsCount compIds model
        { model with Symbols = newSymbols; SymbolsCount = newSymbolCount }, Cmd.none 

    | AddSymbol (pos,compType, lbl) ->
        let (newModel, _) = addSymToModel model pos compType lbl
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
        Cmd.none 

    | ShowPorts compIds -> 
        let resetSymbols = Map.map (fun _ sym -> {sym with ShowInputPorts = false; ShowOutputPorts = false}) model.Symbols
        let newSymbols =
            (List.fold (fun prevSymbols sId -> Map.add sId {resetSymbols[sId] with ShowInputPorts = true; ShowOutputPorts = true} prevSymbols) resetSymbols compIds)
        { model with Symbols = newSymbols }, Cmd.none

    | MoveSymbols (compIds, move) -> 
        let resetSymbols = Map.map (fun _ sym -> { sym with Moving = false}) model.Symbols
        let moveSym prevSymbols cmpId =
            let newCmp = {model.Symbols[cmpId].Component with X = model.Symbols[cmpId].Component.X + int move.X ;Y = model.Symbols[cmpId].Component.Y +  int move.Y }
            Map.add cmpId {model.Symbols[cmpId] with Moving = true; Center = {X = float (newCmp.X + newCmp.W/2); Y = float (newCmp.Y + newCmp.H/2)} ; Component = newCmp} prevSymbols
        let newSymbols =
            (List.fold moveSym resetSymbols compIds)
        { model with Symbols = newSymbols }, Cmd.none

    | SymbolsHaveError compIds ->
        let resetSymbols = Map.map (fun _ sym -> {sym with Colour = "Lightgray"}) model.Symbols
        let newSymbols =
            (List.fold (fun prevSymbols cmpId -> Map.add cmpId {resetSymbols[cmpId] with Colour = "Red"} prevSymbols) resetSymbols compIds)
        { model with Symbols = newSymbols }, Cmd.none

    | SelectSymbols compIds -> 
        let resetSymbols = Map.map (fun _ sym ->  { sym with Colour = "Lightgray"; Opacity = 1.0 }) model.Symbols
        let newSymbols =
            (List.fold (fun prevSymbols cmpId -> Map.add cmpId {resetSymbols[cmpId] with Colour = "lightgreen"} prevSymbols) resetSymbols compIds)
        { model with Symbols = newSymbols }, Cmd.none  

    | ErrorSymbols (errorCompIds,selectCompIds,isDragAndDrop) -> 
        let resetSymbols = Map.map (fun _ sym ->  { sym with Colour = "Lightgray"; Opacity = 1.0 }) model.Symbols
        let selectSymbols =
            List.fold (fun prevSymbols cmpId -> 
                            if not isDragAndDrop then 
                                Map.add cmpId {resetSymbols[cmpId] with Colour = "lightgreen"} prevSymbols
                            else 
                                Map.add cmpId { resetSymbols[cmpId] with Opacity = 0.2 } prevSymbols
                        ) resetSymbols selectCompIds
        let newSymbols = 
            (List.fold (fun prevSymbols cmpId -> Map.add cmpId {resetSymbols[cmpId] with Colour = "Red"} prevSymbols) selectSymbols errorCompIds)
        { model with Symbols = newSymbols }, Cmd.none 
        
    | MouseMsg _ -> model, Cmd.none 

    | ChangeLabel (cmpId, newLabel) ->
        let sym = Map.find cmpId model.Symbols
        let updatedCmp = {sym.Component with Label = newLabel}
        let updatedSym = {sym with Component = updatedCmp}
        { model with Symbols = Map.add cmpId updatedSym model.Symbols }, Cmd.none

    | PasteSymbols compIds ->
        let newSymbols =
            (List.fold (fun prevSymbols sId -> Map.add sId { model.Symbols[sId] with Opacity = 0.4 } prevSymbols) model.Symbols compIds)
        { model with Symbols = newSymbols }, Cmd.none  
    
    | ColorSymbols (compIds, colour) -> 
        let newSymbols = 
            Map.map (fun sId sym -> if List.contains sId compIds then {sym with Colour = string colour} else sym) model.Symbols
        { model with Symbols = newSymbols }, Cmd.none 
    
    | ChangeNumberOfBits (compId, newBits) ->
        let newSymbol = updateCmpNumOfBits model compId newBits
        let newSymbols = Map.add compId newSymbol model.Symbols
        { model with Symbols = newSymbols }, Cmd.none
    
    | ChangeLsb (compId, newLsb) -> 
        let newSymbol = updateLSBNumOfBits model compId newLsb
        let newSymbols = Map.add compId newSymbol model.Symbols
        { model with Symbols = newSymbols }, Cmd.none

    | ChangeConstant (compId, newVal, newText) -> 
        let newSymbol = updateConstant model compId newVal newText
        let newSymbols = Map.add compId newSymbol model.Symbols
        { model with Symbols = newSymbols }, Cmd.none
    
    | ResetModel -> { model with Symbols = Map.empty; Ports = Map.empty; SymbolsCount = Map.empty }, Cmd.none
    
    | LoadComponents cmpsToLoad ->
        let loadComponent cmp =
            let (h,w) =
                if cmp.H = -1 && cmp.W = -1 then
                    let cmp' = initialiseComponent {X = float cmp.X; Y = float cmp.Y} cmp.Type cmp.Id cmp.Label
                    cmp'.H,cmp'.W
                else
                    cmp.H, cmp.W
            ComponentId cmp.Id,
            { 
            Center = {X = float (cmp.X + h/2); Y = float (cmp.Y+ w/2)}
            ShowInputPorts = false 
            ShowOutputPorts = false 
            Colour = "lightgrey"  
            ComponentId = ComponentId cmp.Id
            Component = {cmp with H=h ; W = w}
            Opacity = 1.0
            Moving = false
            InWidth0 = None
            InWidth1 = None
            Rotation = 0.0
            SymbolPoints = (initSymbolPoints cmp {X = float cmp.X; Y = float cmp.Y} cmp.H cmp.W)
            SymbolCharacteristics = (initSymbolCharacteristics cmp)
            }
        

        let loadedSymbolsMap = 
            List.map loadComponent cmpsToLoad
            |>  Map.ofList

        let loadedSymbols =
            List.map loadComponent cmpsToLoad
            |> List.map snd
        
        let addPortsToModel currModel sym =
            { currModel with Ports = addToPortModel currModel sym }
            
        let loadedPortsModel = ( model, loadedSymbols ) ||> List.fold addPortsToModel
        { loadedPortsModel with Symbols = loadedSymbolsMap }, Cmd.none
 
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
        
    | RotateSymbols compIds ->

        let rotateSelectedCmps selectedComponents cmpId symbol  =
            match List.contains cmpId selectedComponents with
            | true -> rotateSymbol symbol
            | false -> symbol

        let rotatedSymbols = Map.map (rotateSelectedCmps compIds) model.Symbols
        {model with Symbols = rotatedSymbols}, Cmd.none

        


// -------------------------------INTERFACE TO ISSIE --------------------------------- //

let extractComponent (symModel: Model) (sId:ComponentId) : Component = 
    symModel.Symbols[sId].Component

let extractComponents (symModel: Model) : Component list =
    symModel.Symbols
    |> Map.toList
    |> List.map (fun (key, _) -> extractComponent symModel key)



