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


/// --------- STATIC VARIABLES --------- ///

let GridSize = 30 

/// ---------- SYMBOL TYPES ---------- ///
type Symbol =
    {
        Center: XYPos
        InWidth0: int option
        InWidth1: int option
        Id : ComponentId       
        Compo : Component                 
        Colour: string
        ShowInputPorts: bool
        ShowOutputPorts: bool
        Opacity: float
        Moving: bool
    }

type Model = {
    Symbols: Map<ComponentId, Symbol>
    CopiedSymbols: Map<ComponentId, Symbol>
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

//---------------------------------helper types and functions----------------//

let posDiff (a:XYPos) (b:XYPos) =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd (a:XYPos) (b:XYPos) =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}


// ----- helper functions for titles ----- //

///Insert titles compatible with greater than 1 buswidth
let title t (n) =  
        if n = 1 then t else t + "(" + string(n-1) + "..0)"

///Insert titles for bus select
let bustitle wob lsb = 
    if wob <> 1 then"(" + string(wob + lsb - 1) + ".." + string(lsb) +  ")" else string(lsb)

///Decodes the component type into component labels
let prefix compType = 
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

// Text to be put inside different Symbols depending on their ComponentType
let gateDecoderType (comp:Component) =
    match comp.Type with
    | And | Nand-> "&"
    | Or | Nor-> "≥1"
    | Xor | Xnor -> "=1"
    | Not -> "1"
    | Decode4 -> "Decode"
    | NbitsAdder n -> title "Adder" n
    | Register n | RegisterE n-> title "Register" n
    | AsyncROM1 _ -> "Async-ROM"
    | ROM1 _ -> "Sync-ROM"
    | RAM1 _ -> "Sync-RAM"
    | AsyncRAM1 _ -> "Async-RAM"
    | DFF -> "DFF"
    | DFFE -> "DFFE"
    | NbitsXor (x)->   title "N-bits-Xor" x
    | Custom x -> x.Name
    | _ -> ""

// Input and Output names of the ports depending on their ComponentType
let portDecName (comp:Component) = //(input port names, output port names)
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
   // |Mux4 -> (["0"; "1"; "2"; "3" ;"SEL"],["OUT"])
   // |Demux4 -> (["IN"; "SEL"],["0"; "1";"2"; "3";])
   // |Demux8 -> (["IN"; "SEL"],["0"; "1"; "2" ; "3" ; "4" ; "5" ; "6" ; "7"])
   // |Mux8 -> (["0"; "1"; "2" ; "3" ; "4" ; "5" ; "6" ; "7";"SEL"],["OUT"])
   // |_ -> ([],[])
   // EXTENSION: Extra Components made that are not currently in Issie. Can be extended later by using this code as it is .

/// Genererates a list of ports:
let portLists numOfPorts hostID portType =
    if numOfPorts < 1 
    then []
    else
        [0..(numOfPorts-1)]
        |> List.collect (fun x ->
            [{
                Id = JSHelpers.uuid ()
                PortNumber = Some x
                PortType = portType
                HostId = hostID
            }])


//-----------------------Skeleton Message type for symbols---------------------//

///Rounds an integer to any given number. The first parameter is the number to round to, the second parameter is the input number that will be rounded
let roundToN (n : int) (x : int) =
    x + abs((x % n) - n)

let customToLength (lst : (string * int) list) =
    let labelList = List.map (fst >> String.length) lst
    if List.isEmpty labelList then 0 //if a component has no inputs or outputs list max will fail
    else List.max labelList

// helper function to initialise each type of component
let makeComp (symCenter: XYPos) (comptype: ComponentType) (id:string) (label:string) : Component =

    // function that helps avoid dublicate code by initialising parameters that are the same for all component types and takes as argument the others
    let makeComponent (n, nout, h, w) label : Component=  
        {
            Id = id 
            Type = comptype 
            Label = label 
            InputPorts = portLists n id PortType.Input 
            OutputPorts  = portLists nout id PortType.Output 
            X  = int (symCenter.X - float w / 2.0) 
            Y = int (symCenter.Y - float h / 2.0) 
            H = h 
            W = w
        }
    
    // match statement for each component type. the output is a 4-tuple that is used as an input to makecomponent (see below)
    // 4-tuple of the form ( number of input ports, number of output ports, Height, Width)
    let args = 
        match comptype with
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
        // EXTENSION:    | Mux4 -> ( 5  , 1, 5*GridSize ,  2*GridSize)   
        // EXTENSION:    | Mux8 -> ( 9  , 1, 7*GridSize ,  2*GridSize) 
        | Demux2 ->( 2  , 2, 3*GridSize ,  2*GridSize) 
        // EXTENSION:   | Demux4 -> ( 2  , 4, 150 ,  50) 
        // EXTENSION:    | Demux8 -> ( 2  , 8, 200 ,  50) 
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
            let maxInLength, maxOutLength = customToLength x.InputLabels, customToLength x.OutputLabels
            let maxW = maxInLength + maxOutLength + label.Length
            let scaledW = roundToN GridSize (maxW * GridSize / 5) //Divide by 5 is just abitrary as otherwise the symbols would be too wide 
            let w = max scaledW (GridSize * 4) //Ensures a minimum width if the labels are very small
            ( List.length x.InputLabels, List.length x.OutputLabels, h ,  w)
                
    makeComponent args label
   
// Function to generate a new symbol
let createNewSymbol (pos: XYPos) (comptype: ComponentType) (label:string) =
    let id = JSHelpers.uuid ()
    let comp = makeComp pos comptype id label
    { 
      Center = {X = pos.X ; Y = pos.Y }
      ShowInputPorts = false
      ShowOutputPorts = false
      InWidth0 = None // set by BusWire
      InWidth1 = None
      Colour = "lightgrey"
      Id = ComponentId id
      Compo = comp
      Opacity = 1.0
      Moving = false
    }

// Function to add ports to port model     
let addToPortModel (model: Model) (sym: Symbol) =
    let addOnePort (currentPorts: Map<string, Port>) (port: Port) =
        Map.add port.Id port currentPorts
    
    let addedInputPorts = (model.Ports, sym.Compo.InputPorts) ||> List.fold addOnePort
    (addedInputPorts, sym.Compo.OutputPorts) ||> List.fold addOnePort

//-----------------------------------------GET PORT POSITION---------------------------------------------------
// Function that calculates the positions of the ports 

/// hack so that bounding box of splitwire, mergewires can be smaller height relative to ports
let inline getPortPosEdgeGap (ct: ComponentType) =
    match ct with
    | MergeWires | SplitWire _  -> 0.25
    | _ -> 1.0

let getPortPos (comp: Component) (port:Port) = 
    let (ports, posX) =
        if port.PortType = (PortType.Input) then
            (comp.InputPorts, 0.0)
        else 
            (comp.OutputPorts, float( comp.W ))
    let index = float( List.findIndex (fun (p:Port)  -> p = port) ports )
    let gap = getPortPosEdgeGap comp.Type 
    let posY = (float(comp.H))* (( index + gap )/( float( ports.Length ) + 2.0*gap - 1.0))  // the ports are created so that they are equidistant
    {X = posX; Y = posY}
let getPortPosModel (model: Model) (port:Port) =
    getPortPos (Map.find (ComponentId port.HostId) model.Symbols).Compo port


//-----------------------------------------DRAWING HELPERS ---------------------------------------------------
// Text adding function with many parameters (such as bold, position and text)
let private addText posX posY name txtPos weight size=
    let text =
            {defaultText with TextAnchor = txtPos; FontWeight = weight; FontSize = size}
    [makeText posX posY name text]

// Generate circles
let private portCircles x y  = 
    [makeCircle x y portCircle]
// Define the name of each port 
let private portText x y name portType=
    let xPos = 
        if portType = PortType.Output
        then x - 5.
        else x + 5.
    let test = if portType = PortType.Output then "end" else "start"
    (addText xPos (y - 7.0) name test "normal" "12px")

// Print the name of each port 
let private drawPortsText (portList: Port List) (listOfNames: string List) (comp: Component)= 
    if listOfNames.Length < 1
        then  []
        else 
            [0..(portList.Length-1)]
            |> List.map2 (fun name x -> (portText (getPortPos comp portList[x]).X (getPortPos comp portList[x]).Y name (portList.Head.PortType))) listOfNames 
            |> List.collect id

// Function to draw ports using getPortPos. The ports are equidistant     
let private drawPorts (portList: Port List) (printPorts:bool) (comp: Component)= 
    if (portList.Length)  < 1 
    then []
    else
        if printPorts
        then [0..(portList.Length-1)] |> List.collect (fun x -> (portCircles (getPortPos comp portList[x]).X (getPortPos comp portList[x]).Y))
        else []

//------------------------------HELPER FUNCTIONS FOR DRAWING SYMBOLS-------------------------------------
let private createPolygon points colour opacity = 
    [makePolygon points {defaultPolygon with Fill = colour; FillOpacity = opacity}]

let createBiColorPolygon points colour strokeColor opacity strokeWidth= 
    if strokeColor <> "black" then 
        [makePolygon points {defaultPolygon with Fill = colour; Stroke = strokeColor; FillOpacity = opacity; StrokeWidth=strokeWidth}]
    else   
        [makePolygon points {defaultPolygon with Fill = colour; FillOpacity = opacity; StrokeWidth = strokeWidth}]

let addInvertor posX posY colour opacity =
    let points = (sprintf "%i,%i %i,%i %i,%i" posX (posY) (posX+9) (posY) posX (posY-8))
    createPolygon points colour opacity

let addClock posX posY colour opacity =
    let points = (sprintf "%i,%i %i,%i %i,%i" posX (posY-1) (posX+8) (posY-7) posX (posY-13))
    createPolygon points colour opacity
    |> List.append (addText (float(posX+10)) (float(posY-13)) " clk" "start" "normal" "12px")

let addHorizontalLine posX1 posX2 posY opacity = // TODO: Line instead of polygon?
    let points = (sprintf "%i,%f %i,%f" posX1 posY posX2 posY)
    createPolygon points "lightgray" opacity

let outlineColor (color:string) =
    match color.ToLower() with
    | "lightgray" | "lightgrey" -> "black"
    | c -> 
        printfn $"color={color}"
        c

let addHorizontalColorLine posX1 posX2 posY opacity (color:string) = // TODO: Line instead of polygon?
    let points = (sprintf "%i,%f %i,%f" posX1 posY posX2 posY)
    let olColor = outlineColor color
    [makePolygon points {defaultPolygon with Fill = "olcolor"; Stroke=olColor; StrokeWidth = "2.0"; FillOpacity = opacity}]



/// --------------------------------------- SYMBOL DRAWING ------------------------------------------------------ ///   

let compSymbol (symbol:Symbol) (comp:Component) (colour:string) (showInputPorts:bool) (showOutputPorts:bool) (opacity: float)= 
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
        addText (float (posX1 + posX2)/2.0) (posY*float(h)-11.0) text "middle" "bold" "9px"



    let points =            // Points that specify each symbol 
        match comp.Type with
        | Input _ -> (sprintf "%i,%i %i,%i %f,%i %i,%i %f,%i" 0 0 0 h (float(w)*(0.66)) h w halfH (float(w)*(0.66)) 0)
        | Constant1 _ -> (sprintf "%i,%i %i,%i %i,%i" 0 comp.H halfW halfH 0 0)
        | IOLabel -> (sprintf "%f,%i %i,%i %f,%i %f,%i %i,%i %f,%i"  (float(w)*(0.33)) 0 0 halfH (float(w)*(0.33)) h (float(w)*(0.66)) h w halfH (float(w)*(0.66)) 0)
        | Output _ -> (sprintf "%f,%i %i,%i %f,%i %i,%i %i,%i" (float(w)*(0.2)) 0 0 halfH (float(w)*(0.2)) h w h w 0)
        | Viewer _ -> (sprintf "%f,%i %i,%i %f,%i %i,%i %i,%i" (float(w)*(0.2)) 0 0 halfH (float(w)*(0.2)) h w h w 0)
        | MergeWires -> (sprintf "%i,%f %i,%f " halfW ((1.0/6.0)*float(h)) halfW ((5.0/6.0)*float(h)))
        | SplitWire _ ->  (sprintf "%i,%f %i,%f " halfW ((1.0/6.0)*float(h)) halfW ((5.0/6.0)*float(h)))
        | Demux2 -> (sprintf "%i,%f %i,%f %i,%i %i,%i" 0 (float(h)*0.2) 0 (float(h)*0.8) w h w 0)
        | Mux2 -> (sprintf "%i,%i %i,%f  %i,%f %i,%i" 0 0 w (float(h)*0.2) w (float(h)*0.8) 0 h )
        // EXTENSION: |Mux4|Mux8 ->(sprintf "%i,%i %i,%f  %i,%f %i,%i" 0 0 w (float(h)*0.2) w (float(h)*0.8) 0 h )
        // EXTENSION: | Demux4 |Demux8 -> (sprintf "%i,%f %i,%f %i,%i %i,%i" 0 (float(h)*0.2) 0 (float(h)*0.8) w h w 0)
        | BusSelection _ |BusCompare _ -> (sprintf "%i,%i %i,%i %f,%i %f,%f %i,%f %i,%f %f,%f %f,%i ")0 0 0 h (0.6*float(w)) h (0.8*float(w)) (0.7*float(h)) w (0.7*float(h)) w (0.3*float(h)) (0.8*float(w)) (0.3*float(h)) (0.6*float(w)) 0
        | _ -> (sprintf "%i,%i %i,%i %i,%i %i,%i" 0 (comp.H) comp.W (comp.H) comp.W 0 0 0)
    let additions =       // Helper function to add certain characteristics on specific symbols (inverter, enables, clocks)
        match comp.Type with
        | Constant1 (_,_,txt) -> (addHorizontalLine halfW w (float(halfH)) opacity @ addText (float (halfW)-5.0) (float(h)-8.0) txt "middle" "normal" "12px") 
        | Nand | Nor | Xnor |Not -> (addInvertor w halfH colour opacity)
        | MergeWires -> 
            let lo, hi = 
                match symbol.InWidth0, symbol.InWidth1  with 
                | Some n, Some m  -> n, m
                | _ -> -1,-1
            let msb = hi + lo - 1
            let midb = lo
            let midt = lo - 1
            mergeSplitLine 0 halfW (1.0/6.0) midt 0 @ 
            mergeSplitLine 0 halfW (5.0/6.0) msb midb @ 
            mergeSplitLine halfW w 0.5 msb 0
        | SplitWire mid -> 
            let msb, mid' = match symbol.InWidth0 with | Some n -> n - 1, mid | _ -> -100, -50
            let midb = mid'
            let midt = mid'-1
            mergeSplitLine halfW w (1.0/6.0) midt 0 @ 
            mergeSplitLine halfW w (5.0/6.0) msb midb @ 
            mergeSplitLine 0 halfW 0.5 msb 0
        | DFF |DFFE -> (addClock 0 h colour opacity)
        | Register _ |RegisterE _ -> (addClock 0 h colour opacity)
        | ROM1 _ |RAM1 _ | AsyncRAM1 _ -> (addClock 0 h colour opacity)
        | BusSelection(x,y) -> (addText  (float(w/2)-5.0) ((float(h)/2.7)-2.0) (bustitle x y) "middle" "normal" "12px")
        | BusCompare (_,y) -> (addText  (float(w/2)-6.0) (float(h)/2.7-1.0) ("=" + NumberHelpers.hex(int y)) "middle" "bold" "10px")
        | Input (x) -> (addText  (float(w/2)-5.0) ((float(h)/2.7)-3.0) (title "" x) "middle" "normal" "12px")
        | Output (x) -> (addText  (float(w/2)) ((float(h)/2.7)-3.0) (title "" x) "middle" "normal" "12px")
        | Viewer (x) -> (addText  (float(w/2)) ((float(h)/2.7)-1.25) (title "" x) "middle" "normal" "9px")
        | _ -> []

    let olColour, strokeWidth =
        match comp.Type with
        | SplitWire _ | MergeWires -> outlineColor colour, "2.0"
        | _ -> "black", "1.0"
   
    // Put everything together 
    
    (drawPorts comp.OutputPorts showOutputPorts comp)
    |> List.append (drawPorts comp.InputPorts showInputPorts comp)
    |> List.append (drawPortsText comp.InputPorts (fst(portDecName comp)) comp)
    |> List.append (drawPortsText comp.OutputPorts (snd(portDecName comp)) comp)  
    |> List.append (addText (float halfW) (+5.0) (gateDecoderType comp) "middle" "bold" "14px") 
    |> List.append (addText (float halfW) (-20.0) comp.Label "middle" "normal" "16px")
    |> List.append (additions)
    |> List.append (createBiColorPolygon points colour olColour opacity strokeWidth)

let init () = 
    { Symbols = Map.empty; CopiedSymbols = Map.empty; Ports = Map.empty }, Cmd.none

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
        fun (props : RenderSymbolProps) ->
            let symbol = props.Symbol
            let ({X=fX; Y=fY}:XYPos) = {X =symbol.Compo.X; Y = symbol.Compo.Y}
            g ([ Style [ Transform(sprintf "translate(%fpx, %fpx)" fX fY) ] ]) (compSymbol props.Symbol props.Symbol.Compo symbol.Colour symbol.ShowInputPorts symbol.ShowOutputPorts symbol.Opacity)
            
        , "Symbol"
        , equalsButFunctions
        )
    
/// View function for symbol layer of SVG
let MapsIntoLists map =
    let listMoving = 
        Map.filter (fun _ sym -> not sym.Moving) map
        |>Map.toList
        |>List.map snd
    let listNotMoving =
        Map.filter (fun _ sym -> sym.Moving) map
        |>Map.toList
        |>List.map snd
    listMoving @ listNotMoving


let view (model : Model) (dispatch : Msg -> unit) = 
    let start = TimeHelpers.getTimeMs()
    model.Symbols
    |> MapsIntoLists
    |> List.map (fun ({Id = ComponentId id} as symbol) ->
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
let getSymBoundingBox (sym:Symbol): BoundingBox =
    {X = float(sym.Compo.X) ; Y = float(sym.Compo.Y) ; H = float(sym.Compo.H) ; W = float(sym.Compo.W)}

let getModelBoundingBoxes (model: Model): Map<ComponentId, BoundingBox> =
    Map.map (fun symId sym -> (getSymBoundingBox sym)) model.Symbols
    
let getCmpBoundingBox (model: Model) (compid: ComponentId ): BoundingBox =
    let sym = Map.find compid model.Symbols
    getSymBoundingBox sym


//--------------------- GETTING PORTS AND THEIR LOCATIONS INTERFACE FUNCTIONS-------------------------------

///Returns the port object associated with a given portId
let getPort (model: Model) (portId: string) =
    model.Ports[portId]

///Returns all the port locations of the given components   
let getCmpsPortLocations (model: Model) (symIds: ComponentId list) = 
    let getInputPortsPositionMap (model: Model) (symbols: Symbol list)  = 
        symbols
        |> List.collect (fun sym -> List.map (fun p -> sym,p) sym.Compo.InputPorts)
        |> List.map (fun (sym,port) -> (InputPortId port.Id, posAdd (getPortPosModel model port) ({X= sym.Compo.X; Y=sym.Compo.Y})))
        |> Map.ofList


    let getOutputPortsPositionMap (model: Model) (symbols: Symbol list)  = //These function add the coordinates of the symbol too
        symbols
        |> List.collect (fun sym -> List.map (fun p -> sym,p) sym.Compo.OutputPorts)
        |> List.map (fun (sym,port) -> (OutputPortId port.Id , posAdd (getPortPosModel model port) ({X= sym.Compo.X; Y=sym.Compo.Y})))
        |> Map.ofList

    let syms = 
        model.Symbols 
        |> Map.filter (fun sId sym  -> List.contains sId symIds)
        |> Map.toList
        |> List.map snd
        
    let getInputPortMap = getInputPortsPositionMap model syms
    let getOutputPortMap = getOutputPortsPositionMap model syms
       
    getInputPortMap , getOutputPortMap

///Returns the location of an input portId  
let getInputPortLocation (model:Model) (inPortId: InputPortId)  =     
    match inPortId with
    | InputPortId(str) -> 
        let inPort = Map.find str model.Ports
        let sym = Map.find (ComponentId(inPort.HostId)) model.Symbols
        posAdd (getPortPosModel model inPort) {X= sym.Compo.X; Y=sym.Compo.Y}
    
    
//Returns the location of an output portId
let getOutputPortLocation (model:Model) (outPortId : OutputPortId) =
    match outPortId with
    | OutputPortId(str) -> 
        let outPort = Map.find str model.Ports
        let sym = Map.find (ComponentId(outPort.HostId)) model.Symbols
        posAdd (getPortPosModel model outPort) {X= sym.Compo.X; Y=sym.Compo.Y}


//----------------------------  LABELS AND COPY SYMBOLS -------------------------------------//




let getCompList compType listSymbols =
    match compType with 
       | Not | And | Or | Xor | Nand | Nor | Xnor -> 
            listSymbols
            |> List.filter (fun sym ->
                (sym.Compo.Type = Not || sym.Compo.Type = And 
                || sym.Compo.Type = Or || sym.Compo.Type = Xor
                || sym.Compo.Type = Nand || sym.Compo.Type = Nor
                || sym.Compo.Type = Xnor)
                )
       | DFF | DFFE -> 
            listSymbols
            |> List.filter (fun sym ->
                (sym.Compo.Type = DFF || sym.Compo.Type = DFFE))
       //The following components require this pattern matching in order to correctly identify all of the components in the circuit of that type
       //Normally this is because they are defined by a width as well as a type
       | Register _ | RegisterE _ ->
            listSymbols
            |> List.filter (fun sym ->
                match sym.Compo.Type with 
                | Register _ | RegisterE _ -> true
                | _ -> false)
       | Constant1 _ ->
            listSymbols
            |> List.filter (fun sym ->
                match sym.Compo.Type with 
                | Constant1 _ -> true
                | _ -> false)
       | Input _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | Input _ -> true
               | _ -> false)
       | Output _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | Output _ -> true
               | _ -> false)
       | Viewer _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | Viewer _ -> true
               | _ -> false)
       | BusSelection _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | BusSelection _ -> true
               | _ -> false)
       | BusCompare _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | BusCompare _ -> true
               | _ -> false)
       | NbitsAdder _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | NbitsAdder _ -> true
               | _ -> false)
       | NbitsXor _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | NbitsXor _ -> true
               | _ -> false)
       | AsyncROM1 _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | AsyncROM1 _ -> true
               | _ -> false)
       | ROM1 _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | ROM1 _ -> true
               | _ -> false)
       | RAM1 _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | RAM1 _ -> true
               | _ -> false)
       | AsyncRAM1 _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | AsyncRAM1 _ -> true
               | _ -> false)

       | _ ->
            listSymbols
            |> List.filter (fun sym -> sym.Compo.Type = compType)

///Genertates the number of the component label (i.e. the number 1 from IN1 or XOR1)
let genCmpIndex modelSyms compType =
    
    let getCmpIndex (str : string) = 
        let index = Regex.Match(str, @"\d+$")
        match index with
        | null -> 0
        | _ -> int index.Value

    let symbolList = 
        getCompList compType modelSyms

    match compType with
    | MergeWires | SplitWire _ -> ""
    | _ ->
        if List.isEmpty symbolList then 1 
        else symbolList
            |> List.map (fun sym -> getCmpIndex sym.Compo.Label)
            |> List.max
            |> (+) 1
        |> string


///Generates the label for a component type
let genCmpLabel (model: Model) (compType: ComponentType) : string =

    let label = prefix compType
    let modelSyms = List.map snd (Map.toList model.Symbols) 
    match compType with
    | IOLabel -> label
    | _ -> label.ToUpper() + (genCmpIndex modelSyms compType)


/// Interface function to get componentIds of the copied symbols
let getCopiedSymbolsIds (model: Model) : (ComponentId list) =
    model.CopiedSymbols
    |> Map.toList
    |> List.map fst

/// Interface function to paste symbols. Is a function instead of a message because we want an output
let pasteSymbols (model: Model) (mousePos: XYPos) : (Model * ComponentId list) =
    
    /// folder function used to update the State (Model, list of pasted symbols) with a list of copied symbols
    let genPastedSyms (referencePos: XYPos) ((currModel, pastedIds) : Model * ComponentId List) (copiedSym: Symbol): Model * ComponentId List =
        
        // generate pastedSymbol
        let Id' = JSHelpers.uuid()
        let offsetFromReferencePos = posDiff copiedSym.Center referencePos
        let pastedSymCenter = posAdd offsetFromReferencePos mousePos
        let pastedCmp = makeComp pastedSymCenter copiedSym.Compo.Type Id' (genCmpLabel { model with Symbols = currModel.Symbols } copiedSym.Compo.Type )

        let pastedSymbol =
            { copiedSym with
                Id = ComponentId Id'
                Compo = pastedCmp 
                Center = pastedSymCenter
                ShowInputPorts = false
                ShowOutputPorts = false }
                
        // update currModel with pastedSymbol  
        let currSymbols' = currModel.Symbols.Add ((ComponentId Id'), pastedSymbol) // List needs to be in this order
        let currPorts' = addToPortModel currModel pastedSymbol
        { currModel with Symbols = currSymbols'; Ports = currPorts' }, pastedIds @ [ pastedSymbol.Id ]

    // Symbols that need to be pasted  
    let copiedSyms =
        model.CopiedSymbols
        |> Map.toList
        |> List.map snd
    
    // Order copiedSyms based on their X coordinate.
    let copiedSyms' = List.sortBy (fun sym -> sym.Compo.X) copiedSyms

    match copiedSyms' with
    | referenceSymbol :: _ ->
        let referencePos = referenceSymbol.Center 
        ((model, []), copiedSyms') ||> List.fold (genPastedSyms referencePos)
    | [] -> model, []

    
/// Given two componentId list of same length and input / output ports that are in list 1, return the equivalent ports in list 2.
/// ComponentIds at same index in both list 1 and list 2 need to be of the same ComponentType
/// CompIds1 need to be in model.CopiedSymbols
let getEquivalentCopiedPorts (model: Model) (copiedIds) (pastedIds) (InputPortId copiedInputPort, OutputPortId copiedOutputPort) =
    let findEquivalentPorts compId1 compId2 =
        let copiedComponent = model.CopiedSymbols[compId1].Compo
        let pastedComponent = model.Symbols[compId2].Compo // TODO: These can be different for an output gate for some reason.
        
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
    let newSym = createNewSymbol pos compType lbl
    let newPorts = addToPortModel model newSym
    let newSymModel = Map.add newSym.Id newSym model.Symbols
    { model with Symbols = newSymModel; Ports = newPorts }, newSym.Id

// Helper function to change the number of bits expected in a port of each component type
let updateCmpNumOfBits (model:Model) (cmpId:ComponentId) (newBits : int) =
    
    let symbol = Map.find cmpId model.Symbols
    let newcompotype = 
        match symbol.Compo.Type with
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

    let newcompo = {symbol.Compo with Type = newcompotype}
    {symbol with Compo = newcompo}

// Helper function to change the number of bits expected in the LSB port of BusSelection and BusCompare
let updateLSB (symModel:Model) (compId:ComponentId) (newLsb:int64) =
    let symbol = Map.find compId symModel.Symbols
    let newcompotype = 
        match symbol.Compo.Type with
        | BusSelection (w, _) -> BusSelection (w, int32(newLsb))
        | BusCompare (w, _) -> BusCompare (w, uint32(newLsb)) 
        | Constant1(w, _,txt) -> Constant1 (w, newLsb,txt)
        | _ -> failwithf "this shouldnt happen, incorrect call of message changeLsb"
    let newcompo = {symbol.Compo with Type = newcompotype}
    {symbol with Compo = newcompo}

let updateConstant (symModel:Model) (compId:ComponentId) (constantVal:int64) (constantText: string) =
    let symbol = Map.find compId symModel.Symbols
    let newcompotype = 
        match symbol.Compo.Type with
        | Constant1 (w, _, _) -> Constant1 (w, constantVal,constantText)
        | _ -> failwithf "this shouldnt happen, incorrect call of message changeLsb"
    let newcompo = {symbol.Compo with Type = newcompotype}
    printfn "Changing symbol to: %A" newcompotype
    {symbol with Compo = newcompo}
    

//---------------------------------- UPDATE FUNCTION -------------------------------//


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
                let (newCmp: Component) = {model.Symbols[sId].Compo with X = model.Symbols[sId].Compo.X + int move.X ;Y = model.Symbols[sId].Compo.Y +  int move.Y }
                Map.add sId {model.Symbols[sId] with Moving = true; Center = {X = float (newCmp.X + newCmp.W/2); Y = float (newCmp.Y + newCmp.H/2)} ; Compo = newCmp} prevSymbols) resetSymbols compList)
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
        let newcompo = {tempsym.Compo with Label = newLabel}
        let addsym = {tempsym with Compo = newcompo}
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
        let newsymbol = updateCmpNumOfBits model compId newBits
        let symbolswithoutone = model.Symbols.Remove compId
        let newSymbolsWithChangedSymbol = symbolswithoutone.Add (compId, newsymbol)
        { model with Symbols = newSymbolsWithChangedSymbol }, Cmd.none
    
    | ChangeLsb (compId, newLsb) -> 
        let newsymbol = updateLSB model compId newLsb
        let symbolswithoutone = model.Symbols.Remove compId
        let newSymbolsWithChangedSymbol = symbolswithoutone.Add (compId, newsymbol)
        { model with Symbols = newSymbolsWithChangedSymbol }, Cmd.none

    | ChangeConstant (compId, newVal, newText) -> 
        let newsymbol = updateConstant model compId newVal newText
        let symbolswithoutone = model.Symbols.Remove compId
        let newSymbolsWithChangedSymbol = symbolswithoutone.Add (compId, newsymbol)
        { model with Symbols = newSymbolsWithChangedSymbol }, Cmd.none
    
    | ResetModel -> { model with Symbols = Map.empty; Ports = Map.empty }, Cmd.none
    
    | LoadComponents comps ->
        let compIdsWithSymbols =
            comps
            |> List.map ( fun comp -> (
                                        let (h,w) =
                                            if comp.H = -1 && comp.W = -1 then
                                                let comp' = makeComp {X = float comp.X; Y = float comp.Y} comp.Type comp.Id comp.Label
                                                comp'.H,comp'.W
                                            else
                                                comp.H, comp.W
                                        ComponentId comp.Id,
                                        { Center = {X = float (comp.X + h/2); Y = float (comp.Y+ w/2)}
                                          ShowInputPorts = false //do not show input ports initially
                                          ShowOutputPorts = false //do not show output ports initially
                                          Colour = "lightgrey"     // initial color 
                                          Id = ComponentId comp.Id
                                          Compo = {comp with H=h ; W = w}
                                          Opacity = 1.0
                                          Moving = false
                                          InWidth0 = None
                                          InWidth1 = None
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
        let comp = symbol.Compo
        
        let newCompType =
            match comp.Type with
            | RAM1 mem -> RAM1 { mem with Data = Map.add addr value mem.Data }
            | AsyncRAM1 mem -> AsyncRAM1 { mem with Data = Map.add addr value mem.Data }
            | ROM1 mem -> ROM1 { mem with Data = Map.add addr value mem.Data }
            | AsyncROM1 mem -> AsyncROM1 { mem with Data = Map.add addr value mem.Data }
            | _ -> comp.Type
        
        let newComp = { comp with Type = newCompType }
        
        let newSymbols = Map.add compId { symbol with Compo = newComp } model.Symbols
        
        { model with Symbols = newSymbols }, Cmd.none
    | WriteMemoryType (compId, memory) ->
        let symbol = model.Symbols[compId]
        let comp = symbol.Compo       
        let newCompType =
            match comp.Type with
            | RAM1 mem | AsyncRAM1 mem -> memory
            | ROM1 mem -> memory
            | AsyncROM1 mem -> memory
            | _ -> 
                printfn $"Warning: improper use of WriteMemoryType on {comp} ignored"
                comp.Type
        
        let newComp = { comp with Type = newCompType }
        
        let newSymbols = Map.add compId { symbol with Compo = newComp } model.Symbols
        
        { model with Symbols = newSymbols }, Cmd.none
        


// -------------------------------INTERFACE TO ISSIE --------------------------------- //

let extractComponent (symModel: Model) (sId:ComponentId) : Component = 
    symModel.Symbols[sId].Compo

let extractComponents (symModel: Model) : Component list =
    symModel.Symbols
    |> Map.toList
    |> List.map (fun (key, _) -> extractComponent symModel key)
