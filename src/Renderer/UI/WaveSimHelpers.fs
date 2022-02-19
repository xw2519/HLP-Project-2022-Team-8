module WaveSimHelpers

open Fable.React
open Fable.React.Props

open Fulma
open TimeHelpers
open ModelType
open DiagramStyle
open CommonTypes
open FileMenuView
open Extractor
open SimulatorTypes

let maxLastClk = 500u

type WaveGapT = {
    /// length of stable waveform in cycles
    GapLen: int
    /// starting cycle of stable waveform
    GapStart: int
    }

////////////////////////
/// Radix conversion ///
////////////////////////


// TODO: Rationalise by deleting these and using instead the functions in Helpers.

let private charList2String (charLst: char list) = 
    List.map string charLst
    |> List.toSeq
    |> String.concat ""

let dec2bin (n:bigint) (nBits:uint32) =
    [0u..nBits - 1u]
    |> List.rev
    |> List.map (fun bitNum -> if n &&& (1I <<< int bitNum) = 0I then '0' else '1')

let dec2hex (n: bigint) (nBits: uint32): string =
    let seqPad = 
        let times = (4 - int nBits % 4) % 4
        Seq.replicate times '0'

    let paddedBin =
        dec2bin n nBits
        |> Seq.append seqPad
        |> Seq.toList

    let fourBit2HexDig =
        let bit vec n = if (1 <<< n) &&& vec = 0 then '0' else '1'
        let hexDigOf n = (sprintf "%x" n)[0]
        [0..15]
        |> List.map (fun dig -> 
                List.map (bit dig) [3..-1..0], hexDigOf dig)
        |> Map.ofList

    [ 0 .. 4 .. List.length paddedBin - 4 ]
    |> List.map (fun i -> fourBit2HexDig[ paddedBin[i..i + 3] ])
    |> charList2String

let dec2sdec (n: bigint) (nBits: uint32) =
    if (dec2bin n nBits)[0] = '1' 
        then n - bigint (2.0 ** (float nBits)) 
        else n
    |> string

/// convert number to number string of the chosen radix
let n2StringOfRadix (hasRadixPrefix: bool) (n: bigint) (nBits: uint32) (rad: NumberBase) =
    let pref =
        match rad with
        | Bin -> "0b"
        | Hex -> "0x"
        | _ -> ""
    match rad with
    | Dec -> string n
    | Bin -> dec2bin n nBits |> charList2String
    | Hex -> dec2hex n nBits
    | SDec -> dec2sdec n nBits
    |> (fun numberRepStr -> (if hasRadixPrefix then pref else "") + numberRepStr)


/////////////////////////////
// General WaveSim Helpers //
/////////////////////////////

/// Get an option of the reduced canvas state, with geometry eliminated, good for electrical
/// circuit comparisons
let getReducedCanvState model = extractReducedState <| model.Sheet.GetCanvasState ()
    
/// get NetList from WaveSimModel
let wsModel2netList wsModel =
    match wsModel.LastCanvasState with
    | Some canvState -> Helpers.getNetList canvState
    | None -> Map.empty

// Look up netgroup (= waveform) name from WaveData and netgroup
// If Netgroup is not in AllNetGroups return "ERROR"
let waveNameOf (ws:WaveSimModel) (wave: WaveformSpec) =
    Map.tryFindKey (fun _ p -> p = wave) ws.AllWaves
    |> Option.defaultValue "ERROR"

let reactTickBoxRow name nameStyle ticked toggleFun =   
    tr
        [ Class "rowHeight"
          Style [ VerticalAlign "middle" ] ]
        [ td
            [ Class "wAcheckboxCol"
              Class "rowHeight"
              Style [ VerticalAlign "middle" ] ]
              [ input
                  [ Type "checkbox"
                    Class "check"
                    Checked <| ticked
                    Style [ Float FloatOptions.Left ]
                    OnChange toggleFun ] ]
          td [] [ label [Style nameStyle] [ str name] ] ]           

////////////////////////



 
    
    


//-------------------------------------------------------------------------------------------------------//
//---------------------------------More wave Helpers-----------------------------------------------------//
//-------------------------------------------------------------------------------------------------------//

/// get components on other sheets, and RAMs, formatted for setting up "more" waveform selection.
/// The output is used as data for the "more wave select" popup.
let getWaveSetup (ws:WaveSimModel) (model:Model): MoreWaveSetup =
    /// get immediate subsheets of given sheet (sg,name,path)
    let rec subSheets ((sg,name,label,path):SimulationGraph*string*ComponentLabel*ComponentId list) : (SimulationGraph*string*ComponentLabel*ComponentId list) list =
        Map.toList sg
        |> List.collect (fun (cId,comp) -> 
            match comp.Type with
            | Custom custComp -> [Option.get comp.CustomSimulationGraph,  custComp.Name, comp.Label, path @ [cId]] | _ -> [])
    /// get all sheets rooted in (sg,name,path)
    let rec allSheets ((sg,name,label,path): SimulationGraph * string *ComponentLabel* ComponentId list) =
        match subSheets (sg,name,label,path) with
        | [] -> [sg,name,label,path]
        | sheets -> [sg,name,label,path] @ List.collect allSheets sheets
    let mainSheet = ((Option.get ws.InitWaveSimGraph).Graph, model.WaveSimSheet,ComponentLabel "", [])
    let sheets = allSheets mainSheet
    let getSortOf path (comp:SimulationComponent) = 
        match comp.Type, path with 
        | RAM1 _,_ -> Some (4, "RAM")
        | AsyncRAM1 _,_ -> Some(5,"ARAM")
        | AsyncROM1 _,_ | ROM1 _,_ -> Some (6, "ROM")
        | _,[] | _, [_] -> None
        | Input _,_-> Some (1,"Input")
        | Output _,_ -> Some (2, "Output")
        | IOLabel,_  -> Some (3, "Bus Label")
        | _ -> None
    let sheetCol =
        fun (sg, name, ComponentLabel label,path) ->
            Map.toList sg
            |> List.map (fun (cid,comp) -> getSortOf path comp, path @ [cid])
            |> List.sort
            |> List.collect (function | (Some (i,ctyp),path) -> [{Label = label;Sheet=name; Path=path;CSort=ctyp}] | _ -> [])
    sheets
    |> List.collect sheetCol
    |> (fun cols -> cols, (Set.ofList ws.SimParams.MoreWaves))

/// get component from graph subsheet with given name path
let rec getSimComp (sg:SimulationGraph) path =
    match path with
    | [] -> failwithf "What? Path cannot be [] looking up sim component in wave sim"
    | [cid]-> sg[cid]
    | h :: t -> 
        match sg[h].CustomSimulationGraph with
        | Some sg -> getSimComp sg t
        | None -> failwithf "What? A non-terminal part of a path must have a customSimulationgraph"

/// get component from graph subsheet with given name path
let rec getSimCompOpt (sg:SimulationGraph) path =
    match path with
    | [] -> None
    | [cid]-> Map.tryFind cid sg
    | h :: t -> 
        match Map.tryFind h sg with
        | Some {CustomSimulationGraph = Some sg} -> getSimCompOpt sg t
        | Some x -> failwithf "What? Lookup of compnent in simulationgraph failed"
        | None -> None

/// Get the form data for RAM (and other extra) simulation setup
let reactMoreWaves ((sheets,ticks): MoreWaveSetup) (sg:SimulationGraph) (dispatch: Msg -> Unit) =
    let makeTableCell r =   td [Style [VerticalAlign Top]] [r]
    let makeWaveReactList (swL:SheetWave list) =
        swL
        |> List.map (fun sw ->
            let comp = getSimComp sg sw.Path
            let ticked = Set.contains sw.Path ticks
            let toggle _ =
                let ticks = if ticked then Set.remove sw.Path ticks else Set.add sw.Path ticks
                dispatch <| SetPopupWaveSetup(sheets,ticks)
            let name = comp.Label |> function | ComponentLabel lab -> lab
            (sw.Label+":"+name),ticked, comp, toggle)
        |> (fun els -> 
                let isRamOrRom (comp: SimulationComponent) = 
                    match comp.Type with | RAM1 _ | AsyncRAM1 _ | ROM1 _ | AsyncROM1 _ -> true | _ -> false
                let uniques = 
                    List.countBy (fun (name,ticked,comp, toggle) -> name) els
                    |> List.filter (fun (el,i)-> i = 1)
                    |> List.map fst
                    |> Set
                List.filter (fun (name,ticked,comp, toggle) -> isRamOrRom comp && Set.contains name uniques) els)
        |> List.map (fun (name, ticked, comp, toggle) -> reactTickBoxRow name [] ticked toggle)
        
    let makeReactCol (name, sheetWaves) =
        let colBody = makeWaveReactList sheetWaves
        if colBody <> [] then 
            table [Style [Display DisplayOptions.InlineBlock; VerticalAlign Top; PaddingLeft "5px"; PaddingRight "5px"]] 
                    [tbody [Style [Display DisplayOptions.Inline]] <| [tr [] [th [ColSpan 2; Style [TextAlign TextAlignOptions.Center]] [str name]]] @ colBody]
        else div [] []
    
    let cols =
        sheets
        |> List.groupBy (fun {Sheet=name}->name)
        |> List.map makeReactCol
        |> List.map makeTableCell
    if cols = [] then
        str "There are no memories (RAM or ROM), in this design."
    else 
        table [] [tbody [] [tr [] cols]]

let formatMemory (mem:Memory1) =
    let maxFilledGap = 2L
    let sortedLocs = 
        Map.toList mem.Data
        |> List.sort
        |> List.filter (fun (a,d) -> d <> 0L)
    let makeRamLoc (addr: int64) (dat:int64) =
        let disp (v:int64) w = dec2hex (bigint v) (uint32 w)
        sprintf "[0x%s]: 0x%s" (disp addr mem.AddressWidth) (disp dat mem.WordWidth)
    let dispGap endG startG =
        match endG - startG with
        | x when x > maxFilledGap -> 
            [ "[...]: 0x0" ]
        | _ -> 
            [startG..endG - 1L]
            |> List.map (fun a -> makeRamLoc a 0L)

    let rec fillGaps curAddr lst =
        match lst with
        | [] -> dispGap ((1L <<< mem.AddressWidth) - 1L) curAddr
        | (addr, dat) :: lst' -> dispGap addr curAddr @ [ makeRamLoc addr dat ] @ fillGaps (addr+1L) lst'
    fillGaps 0L sortedLocs


/// return sample at current cursor position
let getCursorSampleFromGraph (wSMod: WaveSimModel) =
    let n = int wSMod.SimParams.CursorTime
    wSMod.SimDataCache[n].Graph

/// get Ram contents as array to display. RAM contents is
/// determined on cursor sample from wSMod
let getRamInfoToDisplay wSMod (path: ComponentId list) =
    let cursorStep = wSMod.SimParams.CursorTime
    let sdOpt = Array.tryItem (int cursorStep) wSMod.SimDataCache
    let apLst = path |> List.rev |> function | (h::rest) -> h :: (List.rev rest) | [] -> []
    match sdOpt, apLst with
    | Some sd, (cid :: ap)  ->
        let state = FastRun.extractFastSimulationState sd.FastSim (int cursorStep) (cid,ap)
        let lab = match sd.FastSim.FComps[cid,ap].SimComponent.Label with |  ComponentLabel lab -> lab
        match state with
        | RamState dat  ->
            lab, formatMemory dat
        | _ -> lab, []
    | _ -> "",[]
    




//------------------------------------------------------------------------------------------------------//
//--------------------------------------NetGroup Helpers------------------------------------------------//
//------------------------------------------------------------------------------------------------------//





/// Get NetGroup from targets which represents the group of nLTargets connected by IOLabels.
/// targets:list of inputs connected to a single driving component output (e.g. a connected Net).
/// Return the containing NetGroup, where Nets connected by IOLabels form single Netgroups.
let rec private getNetGroup (netList: NetList) targets = failwithf "this function is no longer implemented"

/// returns a bool representing if the given NLTarget is present in the given NetList
let private isNetListTrgtInNetList (netList: NetList) (nlTrgt: NLTarget) =
    Map.exists (fun _ (nlComp: NetListComponent) -> 
                    Map.exists (fun _ nlTrgtLst -> List.contains nlTrgt nlTrgtLst) nlComp.Outputs) netList

                    (*
/// get array of TrgtLstGroup with the non-existing NLTargets removed
let private getReloadableNetGroups (model: Model) (netList: NetList) =
    match currWaveSimModel model with
    | Some wSModel ->
        dispWaves wSModel
        |> Array.map (fun netGroup -> netGroup.driverNet) 
        |> Array.map (List.filter <| isNetListTrgtInNetList netList)
        |> Array.filter ((<>) [])
        |> Array.map (getNetGroup netList)
    | None -> [||] *)

/// advance SimulationData by 1 clock cycle
let private clkAdvance (sD: SimulationData) =
    let sD =
        if sD.ClockTickNumber = 0 then
            // set up the initial fast simulation
            {sD with FastSim = match FastRun.buildFastSimulation (int maxLastClk) sD.Graph with | Ok fs -> fs | Error e -> failwithf "fast simulation error"}
        else
            sD
    //feedClockTick sD.Graph
    //|> (fun graph ->
    let newClock = sD.ClockTickNumber + 1
    FastRun.runFastSimulation newClock sD.FastSim
    { sD with
              Graph = sD.Graph
              ClockTickNumber = newClock }

/// array of SimData for the given number of cycles
let extractSimData simData nCycles =
    (simData, [| 1u .. nCycles |])
    ||> Array.mapFold (fun s _ -> 
         let s' = clkAdvance s
         (s',s'))
    |> fst

/// get NLSource option from ComponentId and InputPortNumber
let private drivingOutput (netList: NetList) compId inPortN =
    netList[compId].Inputs[inPortN]



/// get array of available NLSource in current canvas state
let availableNetGroups (model: Model) =
    match getSheetWaveSimOpt model with
    | None -> [||]
    | Some waveSim ->
        waveSim.LastCanvasState
        |> Option.defaultValue ([],[])
        |> Helpers.getNetList
        |> makeAllNetGroups



/// get instantaneous value of a port
let private simWireData2Wire wireData =
    wireData
    |> List.mapFold (fun weight bit ->
        match bit with
        | SimulatorTypes.Bit.Zero -> bigint 0
        | SimulatorTypes.Bit.One -> weight
        |> (fun r -> r, weight * (bigint 2))) (bigint 1)
    |> fst
    |> List.sum

/// extract current value of the given array of SourceGroup
let getSimTime (waves: WaveformSpec array) (simData: SimulationData) =
    let fs = simData.FastSim
    let step = simData.ClockTickNumber
    let topLevelComps = simData.FastSim.FComps
    FastRun.runFastSimulation step fs
    waves
    |> Array.map (fun wave ->
        try 
            let driver,opn = wave.Driver
            let wD = FastRun.extractFastSimulationOutput fs step driver opn
            Wire
                { NBits = uint (List.length wD)
                  BitData = simWireData2Wire wD }
        with
        | e -> 
            printfn "Exception: %A" e.StackTrace
            printSimGraph simData.Graph
            failwithf "What? This error in getSimTime should not be possible"

        )
 

/// get all values of waveforms
let getAllWaveSimDataBySample (wsMod: WaveSimModel) =
        let waves = dispWaves wsMod
        wsMod.SimDataCache
        |> Array.map (getSimTime waves)

/// get values of waveforms for one sample
let getWaveSimDataOneSample (wsMod: WaveSimModel) (sample:int) =
    let waves = dispWaves wsMod
    wsMod.SimDataCache[sample]
    |> getSimTime waves




/// extend WaveSimModel.SimData by n cycles
let private appendSimData (model: Model) (wSModel: WaveSimModel) nCycles = 
    match wSModel.SimDataCache with
    | [||] ->
        SimulationView.makeSimData model
        |> Option.map fst
        |> ( Option.map (Result.map (fun sd -> extractSimData sd nCycles))) // TODO simulate this ncycles no init data
    | dat ->
        extractSimData (Array.last dat) nCycles
        |> Array.append dat
        |> Ok
        |> Some

/// get Connection list (1 or 0) from ConnectionId (as a string)
let private connId2Conn (sheet: Sheet.Model) (connId: ConnectionId) : Connection list =
    match sheet.GetCanvasState() with
    | (_, conns) -> 
        List.tryFind (fun (conn: Connection) -> ConnectionId conn.Id = connId) conns
    |> function
       | Some conn -> [ conn ]
       | None -> []

/// get Ids of connections in a trgtLstGroup
let private wave2ConnIds (netGrp: NetGroup) =
    Array.append [|netGrp.driverNet|] netGrp.connectedNets
    |> Array.collect (fun net -> 
        List.toArray net 
        |> Array.map (fun net -> net.TargetConnId))


/// select or deselect the connections of a given waveSpec
let selectWaveConns (model: Model)  (on : bool) (wave: WaveformSpec) (dispatch: Msg -> unit) =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    wave.Conns
    |> Array.toList
    |> model.Sheet.SelectConnections sheetDispatch on

let selectExactConns (model: Model)  (conns: ConnectionId array) (dispatch: Msg -> unit) =
    let allConns =
        snd <| model.Sheet.GetCanvasState()
        |> List.map (fun conn -> ConnectionId conn.Id)
        |> Set.ofList
    let otherConns = allConns - Set.ofArray conns
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    model.Sheet.SelectConnections sheetDispatch true (conns |> Array.toList)
    model.Sheet.SelectConnections sheetDispatch false (Set.toList otherConns)


          

/// returns labels of all custom component instances of sheet in lComp
let findInstancesOf sheet (lComp:LoadedComponent) =
    lComp.CanvasState
    |> fst
    |> List.collect (function | {Type=(Custom {Name=sheet'})} as comp when sheet' = sheet-> [comp.Label] | _ -> [])
  

/// finds out if current sheet is used in some other sheet.
/// works recursively to find root sheet
/// returns current wavesim sheet if there is a cycle, or more than one path to a root
let getRootSheet  (model:Model) =
    match model with
    | {CurrentProj = (Some ({LoadedComponents=lComps} as proj))} ->
        let openSheet = model.WaveSimSheet
        if List.tryFind (fun lc -> lc.Name = openSheet) proj.LoadedComponents = None
            then failwithf "Can't find wavesim sheet in loadedcomponents"
        let getParentData (sheet:string) =
            lComps
            |> List.filter (fun lComp -> lComp.Name <> sheet) // can't be own parent
            |> List.collect (fun lComp -> 
                let parentName = lComp.Name
                match findInstancesOf sheet lComp with
                | [] -> [] //not a parent
                | [_] -> [parentName]
                | _ -> [openSheet]) // if more than one instance can't analyse
        let rec findRoot (traversedSheets:string list) (sheet:string) =
            if List.contains sheet traversedSheets then 
                openSheet
            else
                match getParentData sheet with
                | [root] -> findRoot (sheet :: traversedSheets) root
                | [] -> sheet
                | _ -> openSheet
        Some <| findRoot [] openSheet
    | _ -> None // should never happen

let getCustoms (cid:ComponentId,comp:SimulationComponent) =
    match comp.Type with
    | Custom c -> [cid, c.Name,comp]
    | _ -> []

let simGraphOrFail (comp:SimulationComponent) =
    match comp.CustomSimulationGraph with
    | Some x -> x
    | None -> failwithf "What? a Custom component %A appears to have no simulation graph" comp

/// return a function which can extract the correct SimulationGraph from the current simulation
/// for the given sheet. The parameter is a sample SumulationGraph
let getSimGraph (sheet:string) (graph:SimulationGraph) =
    let makeSelector cids =
        (id, List.rev cids) 
        ||> List.fold (fun selFun cid -> 
                            let func graph = simGraphOrFail (Map.find cid graph)
                            selFun >> func)
       
        
    let rec getGraph (customs:ComponentId list) (thisSheet:string) (graph:SimulationGraph) : (SimulationGraph -> SimulationGraph) array =
        if sheet = thisSheet 
        then 
            [|id|]
        else          
            Map.toArray graph
            |> Array.collect (getCustoms >> List.toArray)
            |> Array.collect (fun (cid,sheet',comp) -> 
                if sheet' = sheet then 
                    [|makeSelector (cid :: customs)|]
                else 
                    getGraph (cid :: customs) sheet' (Option.defaultValue (failwithf "What? %A should have a CustomSimulationGraph" comp) comp.CustomSimulationGraph))
      
    getGraph [] sheet graph
    |> function | [|f|] -> f | x -> failwithf "Wrong number of candidates %A to find %s in Simgraph" x sheet

                    

                

//--------------------------//
//   Naming of waveforms    //
//--------------------------//

/// get common NLSource of list of NLTarget with the same source
let private nlTrgtLst2CommonNLSource (netList: NetList) (nlTrgtLst: NLTarget list) : NLSource option =
    List.tryPick (fun (nlTrgt: NLTarget) -> 
        match Map.tryFind nlTrgt.TargetCompId netList with
        | Some comp -> 
            match Map.tryFind nlTrgt.InputPort comp.Inputs with
            | Some (Some src) -> Some src
            | _ -> None
        | None -> None ) nlTrgtLst

/// get label without (x:x) part at the end
let private labelNoParenthesis (netList: NetList) compId = 
    let lbl = netList[compId].Label
    match Seq.tryFindIndexBack ((=) '(') lbl with
    | Some i -> lbl[0..i - 1]
    | None -> lbl

/// get integer from OutputPortInt
let private outPortInt2int outPortInt =
    match outPortInt with
    | OutputPortNumber pn -> pn

/// get labels of Output and IOLabel components in nlTargetList
let net2outputsAndIOLabels (netList: NetList) (netLst: NLTarget list) =
    let nlTrgt2Lbls st nlTrgt = 
        match Map.tryFind nlTrgt.TargetCompId netList with
        | Some nlComp -> match nlComp.Type with
                         | IOLabel | Output _ -> List.append st [netList[nlTrgt.TargetCompId].Label]
                         | _ -> st
        | None -> st
    List.fold nlTrgt2Lbls [] netLst
    |> List.distinct

/// get labels of Output and IOLabel components in TargetListGroup
let netGroup2outputsAndIOLabels netList (netGrp: NetGroup) =
    Array.append [|netGrp.driverNet|] netGrp.connectedNets
    |> Array.toList
    |> List.collect (net2outputsAndIOLabels netList)
    |> List.distinct


let rec private removeSubSeq startC endC chars =
    ((false,[]), chars)
    ||> Seq.fold (fun (removing,res) ch ->
                    match removing,ch with
                    | true, ch when ch = endC -> false,res
                    | true, _ -> true, res
                    | false, ch when ch = startC -> true,res
                    |false, ch -> false, ch :: res)
    |> snd
    |> List.rev
    |> List.map string
    |> String.concat ""
                 
    
    
    

/// truncate names to remove redundant width specs
let private simplifyName name =
    name
    |> removeSubSeq '(' ')'

/// get WaveLabel corresponding to a NLTarget list
let rec private findName (compIds: ComponentId Set) (sd: SimulationData) (net: NetList) netGrp nlTrgtList =
    let graph = sd.Graph
    let fs = sd.FastSim
    match nlTrgtLst2CommonNLSource net nlTrgtList with
    //nlTrgtLst is not connected to any driving components
    | None -> { OutputsAndIOLabels = []; ComposingLabels = [] }
    | Some nlSource ->
        //TODO check if its ok to comment this?
        if not (Set.contains nlSource.SourceCompId compIds) then
            // printfn "DEBUG: In findname, if not \n nlSource = %A \n compIds = %A" nlSource compIds
            // printfn "What? graph, net, netGrp, nltrgtList should all be consistent, compIds is deprecated"
            // component is no longer in circuit due to changes
            { OutputsAndIOLabels = []; ComposingLabels = [] }
        else   
            let compLbl = labelNoParenthesis net nlSource.SourceCompId
            let outPortInt = outPortInt2int nlSource.OutputPort
            let drivingOutputName inPortN =
                match drivingOutput net nlSource.SourceCompId inPortN with
                | Some nlSource' ->
                    net[nlSource'.SourceCompId].Outputs[nlSource'.OutputPort]
                    |> findName compIds sd net netGrp
                | None ->  { OutputsAndIOLabels = []; ComposingLabels = [] } 
            let srcComp = net[nlSource.SourceCompId]
            match net[nlSource.SourceCompId].Type with
            | ROM _ | RAM _ | AsyncROM _ -> 
                    failwithf "What? Legacy RAM component types should never occur"

            | Not | And | Or | Xor | Nand | Nor | Xnor | Decode4 | Mux2 | BusCompare _ -> 
                [ { LabName = compLbl; BitLimits = 0, 0 } ] 
            | Input w | Output w | Constant1(w, _,_) | Constant(w,_) | Viewer w -> 
                [ { LabName = compLbl; BitLimits = w - 1, 0 } ] 
            | Demux2 -> 
                [ { LabName = compLbl + "." + string outPortInt; BitLimits = 0, 0 } ]
            | NbitsXor w -> 
                [ { LabName = compLbl; BitLimits = w - 1, 0 } ]
            | NbitsAdder w ->
                match outPortInt with
                | 0 -> [ { LabName = compLbl + ".Sum"; BitLimits = w - 1, 0 } ]
                | _ -> [ { LabName = compLbl + ".Cout"; BitLimits = w - 1, 0 } ]
            | DFF | DFFE -> 
                [ { LabName = compLbl + ".Q"; BitLimits = 0, 0 } ]
            | Register w | RegisterE w -> 
                [ { LabName = compLbl + ".Dout"; BitLimits = w-1, 0 } ]
            | RAM1 mem | AsyncRAM1 mem | AsyncROM1 mem | ROM1 mem -> 
                [ { LabName = compLbl + ".Dout"; BitLimits = mem.WordWidth - 1, 0 } ]
            | Custom c -> 
                [ { LabName = compLbl + "." + (fst c.OutputLabels[outPortInt])
                    BitLimits = snd c.OutputLabels[outPortInt] - 1, 0 } ]
            | MergeWires -> 
                List.append (drivingOutputName (InputPortNumber 1)).ComposingLabels 
                            (drivingOutputName (InputPortNumber 0)).ComposingLabels
            | SplitWire w ->
                let mostsigBranch (_, b) =
                    match outPortInt with
                    | 0 -> b >= 16 - w
                    | 1 -> b < 16 - w
                    | _ -> failwith "SplitWire output port number greater than 1"

                let split { LabName = name; BitLimits = msb, lsb } st =
                    List.zip [ lsb .. msb ] [ st + msb - lsb .. -1 .. st ]
                    |> List.filter mostsigBranch
                    |> List.unzip
                    |> function
                    | [], _ -> None 
                    | lst, _ -> Some { LabName = name
                                       BitLimits = List.max lst, List.min lst } 

                let updateState { LabName = _; BitLimits = msb, lsb } st =
                    st + msb - lsb + 1

                (0, (drivingOutputName (InputPortNumber 0)).ComposingLabels)
                ||> List.mapFold (fun st lstEl -> split lstEl st, updateState lstEl st)
                |> fst
                |> List.choose id
            | BusSelection(w, oLSB) ->
                let filtSelec { LabName = name; BitLimits = msb, lsb } st =
                    List.zip [ lsb .. msb ] [ st .. st + msb - lsb ]
                    |> List.filter (fun (_, b) -> oLSB <= b && b <= oLSB + w - 1)
                    |> List.unzip
                    |> function
                    | [], _ -> None
                    | lst, _ -> Some { LabName = name
                                       BitLimits = List.max lst, List.min lst } 

                let updateState { LabName = _; BitLimits = msb, lsb } st =
                    st + msb - lsb + 1 

                ((drivingOutputName (InputPortNumber 0)).ComposingLabels, 0)
                ||> List.mapFoldBack (fun lstEl st -> filtSelec lstEl st, updateState lstEl st)
                |> fst
                |> List.choose id
                |> List.rev
            | IOLabel -> 
                let drivingComp = fs.FIOActive[ComponentLabel srcComp.Label,[]]
                let ioLblWidth = FastRun.extractFastSimulationWidth fs (drivingComp.Id,[]) (OutputPortNumber 0)
                match ioLblWidth with
                | None ->
                    failwithf $"What? Can't find width for IOLabel {net[srcComp.Id].Label}$ "
                | Some width ->
                            
                            [ { LabName = compLbl
                                BitLimits = width - 1, 0 } ]

            |> (fun composingLbls -> { OutputsAndIOLabels = netGroup2outputsAndIOLabels net netGrp
                                       ComposingLabels = composingLbls })


/// get string in the [x:x] format given the bit limits
let private bitLimsString (a, b) =
    match (a, b) with
    | (0, 0) -> ""
    | (msb, lsb) when msb = lsb -> sprintf "[%d]" msb
    | (msb, lsb) -> sprintf "[%d:%d]" msb lsb

/// get the label of a waveform
let netGroup2Label compIds (sd:SimulationData) netList (netGrp: NetGroup) =
    let start = getTimeMs()
    let waveLbl = findName compIds sd netList netGrp netGrp.driverNet
    //printfn "Finding label for %A\n%A\n\n" netGrp.driverComp.Label waveLbl.OutputsAndIOLabels
    let tl =
        match waveLbl.ComposingLabels with
        | [ el ] -> el.LabName + bitLimsString el.BitLimits
        | lst when List.length lst > 0 ->
            let appendName st lblSeg = st + lblSeg.LabName + bitLimsString lblSeg.BitLimits + ", "
            List.fold appendName "{" lst 
            |> (fun lbl -> lbl[0..String.length lbl - 3] + "}")
        |  _ -> ""
    let appendName st name = st + name + ", "
    match waveLbl.OutputsAndIOLabels with
    | [] -> tl
    | hdLbls -> 
        List.fold appendName "" hdLbls
        |> (fun hd -> hd[0..String.length hd - 3] + " : " + tl)
    |> simplifyName
    |> instrumentInterval "netGroup2Label" start




// Required wSModel with correct simulation.
// Sets AllWaveNames and AllPorts from netlist derived from simulation
// filters 
let setWSAllPorts (availablePorts: NetGroup array) (wSModel:WaveSimModel) : WaveSimModel  = failwithf "not implemented"
    
//////////////////
/// SVG shapes ///
//////////////////

let makeLinePoints style (x1, y1) (x2, y2) =
    line
        (List.append style 
             [ X1 x1
               Y1 y1
               X2 x2
               Y2 y2 ]) []

let makeSvg style elements = svg style elements
let makeLine style = line style []
let makeText style t = text style [ str t ]

let backgroundSvg (model: WaveSimModel) =
    let clkLine x = makeLinePoints [ Class "clkLineStyle" ] (x, vPos) (x, vPos + sigHeight + spacing)
    [| 1u .. model.SimParams.LastClkTime + 1u |] 
    |> Array.map ((fun x -> float x * model.SimParams.ClkSvgWidth) >> clkLine)

let button options func label = 
    Button.button (List.append options [ Button.OnClick func ]) [ str label ]



//-----------------//
//   Transitions   //
//-----------------//

/// get m x (n-1) array of integers representing when value change between clock cycles from m x n waveData
/// (1: change, 0 no change)
let transitions waveData =
    let isDiff (ws1, ws2) =
        let folder state e1 e2 =
            match state, e1 = e2 with
            | 0, true -> 0
            | _ -> 1
        match ws1, ws2 with
        | Wire a, Wire b ->
            if a.BitData = b.BitData then 0 else 1
        | StateSample a, StateSample b when Array.length a = Array.length b -> 
            (a, b) ||> Array.fold2 folder 0
        | _ -> 1

    Array.transpose waveData
    |> Array.map (Array.pairwise >> Array.map isDiff)

/// get gaps - periods for which waveform is stable - from transition array
let makeGaps trans =
    Array.append trans [| 1 |]
    |> Array.mapFold (fun tot t -> tot, tot + t) 0
    |> fst
    |> Array.indexed
    |> Array.groupBy snd
    |> Array.map (fun (_, gL) ->
        let times = Array.map fst gL
        {
            GapLen = Array.max times - Array.min times + 1
            GapStart = Array.min times 
        })

/// get value positions of bus labels from the transition gaps (for one Waveform)
let private busLabelPositions (wSModel: WaveSimModel) (wave: Waveform) gaps =
    let clkWidth = wSModel.SimParams.ClkSvgWidth
    let nSpaces gap = 
        (float gap.GapLen * clkWidth / (float maxBusValGap + 1.) + 2.)
    let busLabelXPosition g i =
        float g.GapStart + float i * float g.GapLen / float (nSpaces g)
    gaps
    |> Array.map (fun (gap) ->
        {| WaveValue = wave[gap.GapStart]
           XPosArray = Array.map (busLabelXPosition gap) [| 1 .. int (nSpaces gap) - 1 |] |} )

/// get values position of bus labels
let private busLabels (wSModel: WaveSimModel) waveData =
    (Array.transpose waveData, Array.map makeGaps (transitions waveData)) 
    ||> Array.map2 (busLabelPositions wSModel)

/// get the labels of a waveform for a period in which the value doesn't change
let private busLabelRepeats wsMod (busLabelValAndPos: {| WaveValue: Sample; XPosArray: float [] |}) =
    let addLabel nLabels xInd = makeText (inWaveLabel nLabels xInd wsMod)
    match busLabelValAndPos.WaveValue with
    | Wire w when w.NBits > 1u ->
        Array.map (fun xInd -> 
            addLabel 1 xInd (n2StringOfRadix true w.BitData w.NBits wsMod.WaveViewerRadix)) busLabelValAndPos.XPosArray
    | _ -> [||]

//--------------------//
//   WaveSim Actions  //
//--------------------//

/// get SVG of a single waveform for one clock cycle
let private makeClkSegment (clkW: float) (xInd: int)  =
    let top = spacing
    let bot = top + sigHeight - sigLineThick
    let left = float xInd * clkW
    let right = left + float clkW
    let mid = left + float clkW / 2.

    let makeSigLine =
        makeLinePoints
            [ Class "sigLineStyle"
              Style [ Stroke("blue") ] ]

    let clkPoints = [
        (left, top)
        (mid, top)
        (mid, bot)
        (right, bot)
        (right, top)
    ]

    clkPoints
    |> List.pairwise
    |> List.map (fun (p1,p2) -> makeSigLine p1 p2)
    |> List.toArray




/// get SVG of a single waveform for one clock cycle
let private makeSegment (clkW: float) (xInd: int) (data: Sample) (trans: int * int) =
    let top = spacing
    let bot = top + sigHeight - sigLineThick
    let left = float xInd * clkW
    let right = left + float clkW

    let makeSigLine =
        makeLinePoints
            [ Class "sigLineStyle"
              Style [ Stroke("blue") ] ]

    match data with
    | Wire w when w.NBits = 1u ->
        let y =
            match w.BitData with
            | n when n = bigint 1 -> top
            | _ -> bot
        let sigLine = makeSigLine (left, y) (right, y)
        match snd trans with
        | 1 -> [| makeSigLine (right, bot + sigLineThick / 2.0) (right, top - sigLineThick / 2.0) |]
        | 0 -> [||]
        | _ -> failwith "What? Transition has value other than 0 or 1"
        |> Array.append [| sigLine |]
    | _ ->
        let leftInner = if fst trans = 1 then left + transLen else left
        let rightInner = if snd trans = 1 then right - transLen else right
        let cen = (top + bot) / 2.0

        //make lines
        let topL = makeSigLine (leftInner, top) (rightInner, top)
        let botL = makeSigLine (leftInner, bot) (rightInner, bot)
        let topLeft = makeSigLine (left, cen) (leftInner, top)
        let botLeft = makeSigLine (left, cen) (leftInner, bot)
        let topRight = makeSigLine (right, cen) (rightInner, top)
        let botRight = makeSigLine (right, cen) (rightInner, bot)

        match trans with
        | 1, 1 -> [| topLeft; botLeft; topRight; botRight |]
        | 1, 0 -> [| topLeft; botLeft |]
        | 0, 1 -> [| topRight; botRight |]
        | 0, 0 -> [||]
        | _ -> failwith "What? Transition has value other than 0 or 1"
        |> Array.append [| topL; botL |]




/// SVG of the clock numbers above the waveforms
let clkRulerSvg (model: WaveSimModel) =
    let makeClkRulLbl i =
        match model.SimParams.ClkSvgWidth with
        | clkW when clkW < 0.5 && i % 5 <> 0 -> [||]
        | _ -> [| makeText (cursRectText model i) (string i) |]
    [| 0 .. int model.SimParams.LastClkTime |]
    |> Array.collect makeClkRulLbl
    |> Array.append (backgroundSvg model)
    |> makeSvg (clkRulerStyle model)


/// get SVG of a positive edge trigerred CLK waveform
let makeClkSvg (sampArr: Waveform) (wsMod): ReactElement [] =
    [|0.. Array.length sampArr - 1|]
    |> Array.map (makeClkSegment (wsMod.ClkWidth / 2.))
    |> Array.concat

 

/// get SVG of the array of waveforms in wsMod
let waveSvg wsMod waveData  =
    let valueLabels =
        busLabels wsMod waveData
        |> Array.map (Array.collect (busLabelRepeats wsMod.SimParams))

    let makeWaveSvg (sampArr: Waveform) (transArr: (int * int) []): ReactElement [] =
        (sampArr, transArr)
        ||> Array.mapi2 (makeSegment wsMod.SimParams.ClkSvgWidth)
        |> Array.concat

    let padTrans t =
        match Array.length t with
        | 0 -> [| 1, 1 |]
        | 1 ->
            [| (1, t[0])
               (t[0], 1) |]
        | _ ->
            Array.pairwise t
            |> (fun pairs ->
                Array.concat
                    [ [| 1, fst pairs[0] |]
                      pairs
                      [| snd (Array.last pairs), 1 |] ])

    transitions waveData
    |> Array.map padTrans
    |> Array.map2 makeWaveSvg (Array.transpose waveData)
    |> Array.map2 Array.append valueLabels


/// Calculate and add the waveform SVGs to the current wsModel.
/// TODO: only recalculate as needed on DispWave change
let addSVGToWaveSimModel wSModel =
    let waveData = 
        getAllWaveSimDataBySample wSModel

    let waveTableRow rowClass cellClass svgClass svgChildren =
        tr rowClass [ td cellClass [ makeSvg svgClass svgChildren ] ]
    let bgSvg = backgroundSvg wSModel

    let lastRow = [| waveTableRow [ Class "fullHeight" ] (lwaveCell wSModel) (waveCellSvg wSModel true) bgSvg |]
    let firstRow = [| tr [ Class "rowHeight" ] [ td (waveCell wSModel) [ clkRulerSvg wSModel ] ] |]

    let midRows =
        waveSvg wSModel waveData
        |> Array.zip wSModel.SimParams.DispNames
        |> Array.map (fun (name,wave) ->
                let waveWithBg = Array.append bgSvg wave
                let svg = waveTableRow [ Class "rowHeight" ] (waveCell wSModel) (waveCellSvg wSModel false) waveWithBg
                name,svg)
        |> Map.ofArray
    let svgs = {Top =  firstRow ; Waves =  midRows; Bottom = lastRow }
    {wSModel with DispWaveSVGCache = svgs}


/// add entry with key: current fileName and data: initWS to model.WaveSim
let initFileWS (model:Model) dispatch =
    let netListOpt = getSheetWaveNetList model
    match getCurrFile model,netListOpt with
    | Some fileName, Some _netList ->
        (fileName, initWS [||] Map.empty)
        |> AddWaveSimFile
        |> dispatch
    | _ -> ()



/// get wave name labels from waveforms names
let makeLabels waveNames =
    let makeLbl l = label [ Class "waveLbl" ] [ str l ]
    Array.map makeLbl waveNames



                     

/// adjust parameters before feeding them into simulateAndMakeWaves 
let adjustPars (wsMod: WaveSimModel) (pars: SimParamsT) rightLim =
    let currPars = wsMod.SimParams
    let defRightLim =
        match dispWaves wsMod with
        | [||] -> 600.0
        | _ -> maxWavesColWidthFloat wsMod
    let rightLim = match rightLim with | Some x -> x | None -> defRightLim
    let lastClkTime = 
        rightLim / (currPars.ClkSvgWidth * 40.0)
        |> uint
        |> max currPars.CursorTime
        |> (+) 10u
        |> max pars.LastClkTime
        |> min maxLastClk 
    {pars with LastClkTime = lastClkTime}

/// Update wavesim based on new parameters in par.
/// Update waveSimCache as needed with a new longer simulation to view new parameters.
/// Update  DispWaveSVGCache with new wave SVGs and/or added SVGs as determined by new parameters.
/// Update the currentWaveSimModel entry with new parameters
let simulateAndMakeWaves (model: Model) (wsMod: WaveSimModel) 
                (par: SimParamsT) : WaveSimModel =
    
    let newData =
        par.LastClkTime + 1u - uint (Array.length wsMod.SimDataCache)
        |> appendSimData model wsMod
        |> function | Some (Ok dat) -> dat
                    | None -> failwithf "No simulation data when Some are expected"
                    | Some (Error e) -> failwithf "%A" e
    let par' = {par with DispNames = par.DispNames}
    { wsMod with SimDataCache = newData
                 SimParams = par' }
    |> addSVGToWaveSimModel



//------------------------------------//
//    Interaction with Model.Diagram  //
//------------------------------------//

/// is the given connection in the given NLTarget list
let private isConnInNLTrgtLst (connId: ConnectionId) (nlTrgtLst: NLTarget list) =
    List.exists (fun nlTrgt -> nlTrgt.TargetConnId = connId) nlTrgtLst

/// is the given component connected to the NLTarget list
let private isCompInNLTrgtLst (nlComponent: NetListComponent) (nlTrgtLst: NLTarget list) =
    Map.exists (fun _ compNLTrgtLst -> compNLTrgtLst = nlTrgtLst) nlComponent.Outputs
    || List.exists (fun nlTrgt -> nlTrgt.TargetCompId = nlComponent.Id) nlTrgtLst

/// is the given NLTarget list selected by the given selection
let private isNLTrgtLstSelected (netList: NetList) ((comps, conns): CanvasState) (nlTrgtLst: NLTarget list) =
    List.exists (fun (comp: Component) -> 
        match Map.tryFind (ComponentId comp.Id) netList with
        | Some comp -> isCompInNLTrgtLst comp nlTrgtLst
        | None -> false ) comps
    || List.exists (fun (conn: Connection) -> 
           isConnInNLTrgtLst (ConnectionId conn.Id) nlTrgtLst) conns

/// is the given waveform selected by the given selection
let private isNetGroupSelected (netList: NetList) ((comps, conns): CanvasState) (trgtLstGroup: NetGroup) =
    Array.append [|trgtLstGroup.driverNet|] trgtLstGroup.connectedNets
    |> Array.exists (isNLTrgtLstSelected netList (comps, conns)) 


/// is the given waveform selected by the current diagram selection
let isWaveSelected (model:Model) (wSpec: WaveformSpec) = 
    match wSpec.WType, wSpec.Conns with
    | ViewerWaveform selected,_ -> selected
    | NormalWaveform, [||] -> false // can't select a wave with no connections (maybe this case does not exist)
    | NormalWaveform, conns -> 
        model.Sheet.GetSelectedConnections
        |> List.exists (fun conn -> ConnectionId conn.Id = wSpec.Conns[0]) 
    
//--------------------------------------------------//
//  Functions fed into FileMenuView View function   //
//--------------------------------------------------//



let getAllWaves (waveSim:WaveSimModel) = 
    mapValues waveSim.AllWaves

///Takes a connection and model, and returns the netgroup as a list of connectionIds associated with that connection
let getNetSelection (canvas : CanvasState) (model : Model) =
    
    let netList = 
        model.LastSimulatedCanvasState
        |> Option.map Helpers.getNetList 
        |> Option.defaultValue (Map.empty)
    

    let netGroups = makeAllNetGroups netList

    let selectedConnectionIds (ng:NetGroup) =
        if isNetGroupSelected netList canvas ng then 
            wave2ConnIds ng
        else [||]
            
    Array.collect selectedConnectionIds netGroups
    |> Array.toList

/// In wave simulation highlight nets which are ticked on viewer or editor
/// Nets can be highlighted or unhighlighted by clicking on nets, or tick-boxes
/// TODO: rewrite this code and SetSelWaveHighlighted message avoiding
/// loops and fixes for loops.
let highlightConnectionsFromWaves (model: Model) (dispatch: Msg -> Unit) =
    match currWaveSimModel model with
    | Some wSModel ->
        let waves =
            match wSModel.WSViewState with 
            | WSEditorOpen 
            | WSInitEditorOpen -> 
                mapValues wSModel.AllWaves                
            | WSViewerOpen  ->
                wSModel.SimParams.DispNames
                |> Array.map (fun name -> wSModel.AllWaves[name])
            | WSClosed -> [||]
  
            
        let selectedConnectionIds (wave: WaveformSpec) =
            if isWaveSelected model wave then 
                wave.Conns
            else [||]
                
        let selectedIds =  Array.collect selectedConnectionIds waves
        let wrongColorIds =
            selectedIds
            |> Array.map (fun cid -> Map.tryFind cid model.Sheet.Wire.WX)
            |> Array.filter ((<>) None)
            |> Array.map Option.get
            |> Array.filter (fun wire -> wire.Color <> HighLightColor.Blue)
            |> Array.map (fun wire -> wire.Id)
        // break loop while ensuring that connections are colored if needed
        // nasty fix
        if wrongColorIds <> [||] then
            dispatch <| SetSelWavesHighlighted selectedIds
    | _ -> ()






/// actions triggered whenever the fileMenuView function is executed
let fileMenuViewActions model dispatch =
    match getCurrentWSMod model with
    | None 
    | Some {WSViewState = WSClosed} -> ()
    | _ ->
        highlightConnectionsFromWaves  model dispatch







//-----------------------//
// Auto-scroll functions //
//-----------------------//

/// strings of the values displayed in the right column of the simulator
let cursorValueStrings (wSMod: WaveSimModel) =
    let paras = wSMod.SimParams

    let makeCursVal sample =
        match sample with
        | Wire w when w.NBits > 1u -> [| n2StringOfRadix true  w.BitData w.NBits paras.WaveViewerRadix |]
        | Wire w -> [| string w.BitData |]
        | StateSample s -> s

    match int paras.CursorTime < Array.length wSMod.SimDataCache with
    | true -> Array.map makeCursVal (getWaveSimDataOneSample wSMod (int paras.CursorTime))
    | false -> [||]

/// strings of the values displayed for a RAM
let ramValueStrings (wSMod: WaveSimModel) (ramName: string) =
    let paras = wSMod.SimParams

    let makeRamVal sample =
        match sample with
        | Wire w when w.NBits > 1u -> [| n2StringOfRadix true  w.BitData w.NBits paras.WaveViewerRadix |]
        | Wire w -> [| string w.BitData |]
        | StateSample s -> s

    match int paras.CursorTime < Array.length wSMod.SimDataCache with
    | true -> Array.map makeRamVal (getWaveSimDataOneSample wSMod (int paras.CursorTime))
    | false -> [||]

/// returns true when the cursor rectangle is in the visible section of the scrollable div
let isCursorVisible wSMod divWidth scrollPos =
    let cursLeftPos = cursorLeftPx wSMod.SimParams <| float wSMod.SimParams.CursorTime
    let cursMid = cursLeftPos + (wSMod.SimParams.ClkSvgWidth * 40.0 / 2.0)
    let leftScreenLim = scrollPos
    let rightScreenLim = leftScreenLim + divWidth
    cursLeftPos >= cursMid && cursMid <= rightScreenLim

/// returns horizontal scrolling position required so that the cursor becomes visible
let makeCursorVisiblePos wSMod divWidth = 
    let cursLeftPos = cursorLeftPx wSMod.SimParams <| float wSMod.SimParams.CursorTime
    let cursMid = cursLeftPos + (wSMod.SimParams.ClkSvgWidth * 40.0 / 2.0)
    cursMid - (divWidth / 2.0)

//-------------------------------------//
// Functions to Manage Wave Editor     //
//-------------------------------------//

type SheetInfo = {
    SheetName: ComponentId; 
    Canvas: CanvasState; 
    SheetGraph: SimulationGraph; 
    Instance: string * ComponentLabel
    SheetPath: ComponentId list; 
    InstancePath: (string * ComponentLabel) list
    NetL: NetList
    }

let rec getSubSheets 
        (sheets: ComponentId list) 
        (instances: (string * ComponentLabel) list) 
        (graph: SimulatorTypes.SimulationGraph) 
        (model:Model) : SheetInfo list =
    mapValues graph
    |> Array.toList
    |> List.collect (fun sComp -> 
        let label = sComp.Label                
        match sComp.Type with 
        | Custom cusComp -> 
            let canvas = 
                match model.CurrentProj with
                | Some {LoadedComponents= lst} ->
                    match List.tryFind (fun ldComp -> ldComp.Name = cusComp.Name) lst with
                    | None -> failwithf "Can't find canvas for sheet"
                    | Some ldComp -> ldComp.CanvasState
                | _ -> failwithf "Can't find project"
            match sComp.CustomSimulationGraph with
            | None -> failwithf "Custom compnent without simulation graph? InstancePath=%A, Instance=%A" instances label
            | Some sg ->
                let thisSheet =
                    {
                        SheetPath = List.rev sheets
                        InstancePath = List.rev instances
                        SheetName = sComp.Id
                        Instance = cusComp.Name, sComp.Label
                        SheetGraph = sg
                        Canvas = canvas
                        NetL = Helpers.getNetList canvas
                    }
                let otherSheets = getSubSheets (sComp.Id :: sheets) (thisSheet.Instance ::instances) graph model
                thisSheet :: otherSheets
        | _ -> [])


            

