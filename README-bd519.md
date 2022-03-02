# Example README for individual code submission

## Instructions

* This file should be submitted (changed) on branch `hlp22-indiv-assess-<login>` of either your own repo or your group repo
   * replace `<login>` in filename `README-<login>.md` by your login - in his example `<login> = tomcl`
   * name the branch as above, including your login. This branch is used only for your submission.
* A link to the repo and branch must be on the `indiv` sheet of Tom Clarke's Team [google spreadsheet](https://docs.google.com/spreadsheets/d/1prQ5usnpu36FgtbsMO8j6_mwbdd34haSMOQKN2OkLBA/edit?usp=sharing)
* The repo you use **must have your marker added as collaborator** (github login is on indiv assessment spreadsheet page)
* Delete these instructions from your version of this readme
* Delete my comments from your version of the readme (it is an example, not something you add lines to). 
Keep the headings and **replace the admin section links and notes with correct ones**.
* Link to the sourcefile your code is contained in (drawblock or symbol) with an absolute hyperlink 
to your repo and branch
* Specify which code section and file you are doing as in my ppt (1,2,3), (buswire,symbol)
* Add any changes to my section code allocations. This must be consistent with what has been 
recorded in your team's file in my Team contributions repo](https://github.com/tomcl/hlp22docs/blob/main/README.md)  
main branch ./TeamN.md (N = 1 - 9) file. The team contrib repo is as official record. This file will be 
used marking and should have line numbers for easy access. Expect to be marked down if your marker
cannot easily find everything via links from this README.

## Team Shared Team.md

[Team Contribution Repo](https://github.com/tomcl/hlp22docs/blob/main/Team8.md)

* A file in this repo file is common to the team contains who is responsible for which parts of code
* Fork and clone it
* Any team member can make agreed by team commits. First commit before Wed 23 Fen 16:00. 
* Changes in who does what are recorded by altering list of functions AND 
as extra lines in this file. See sample file in [README](https://github.com/tomcl/hlp22docs/blob/main/README.md)

## Admin and quick access links

*link to your teamN.md file*
[Common repo TeamN file](https://github.com/tomcl/hlp22docs/blob/main/Team8.md)

[Buswire (section 3)](https://github.com/xw2519/HLP-Project-2022-Team-8/blob/d27d4c07909365bae120d256b34ba3fb0002b7ec/src/Renderer/DrawBlock/BusWire.fs#L1906-L2474)

Section 3 on my file is lines : 1906-2474
I am also responsible for lines:
- 87 [adding keyBoardMsg](https://github.com/xw2519/HLP-Project-2022-Team-8/blob/d27d4c07909365bae120d256b34ba3fb0002b7ec/src/Renderer/DrawBlock/Sheet.fs#L87) in Sheet.fs
- 913 : 915 [Defining keyBoardMsg associated with Ctrl+M](https://github.com/xw2519/HLP-Project-2022-Team-8/blob/d27d4c07909365bae120d256b34ba3fb0002b7ec/src/Renderer/DrawBlock/Sheet.fs#L913-L915) in Sheet.fs
- 177 [editMenu dispatch](https://github.com/xw2519/HLP-Project-2022-Team-8/blob/d27d4c07909365bae120d256b34ba3fb0002b7ec/src/Renderer/Renderer.fs#L177) in Renderer.fs

Anything else you need to say about what you are or are not responsible for.

## Code partitioning notes (don't copy this section in your submitted README - delete it)

Insert clear comments in the source files indicating where each person's code starts and stops

```
//---------------------------------------------------------------------------------//
//--------------------TJWC CODE SECTION STARTS-------------------------------------//
//---------------------------------------------------------------------------------//
```

### Separating code into 3 modules (in different files)

Groups can - **if they wish** - modularise their buswire or symbol files into 2 or 3 modules in separate files

1. Symbol1
2. Symbol2
3. Symbol3

With each module referencing the ones before with `open`. 

A top-level module `Symbol` or `BusWire` after these three must contain functions exported anywhere outside symbol so that
references in the rest of the code still work.

```
Symbol.fs

let interface1 = Symbol1.interface1
let interface2 = Symbol1.Interface2
let interface3 = Symbol2.interface3
```

### Submodules within one file

Less dramatic, but still useful for modularity. Groups can - **if they wish** - modularise their buswire or symbol files into 3 submodules inside the existing file

```
Symbol.fs

module symbol1 =
    let symbol1defn = ...

module symbol2
    open symbol1
    let symbol2defn

module symbol3 =
    open symbol1
    open symbol2
    let symbol3 defn = 
```

Again you will need to mend references - because open Symbol in some other part of Issie would need to become open Symbol.Symbol1, open Symbol.Symbol2 etc to pick up exported code in the submodules. you can add exports to Symbol.fs outside the three submodules as
when each moduel is in separate files.

## Note on assessment

The aim of assessment if that the first two sections should be fairly straightforward and demonstrate basic knowledge
of how to write FP code. They can achieve overall marks up to 70% without extension work. Those working on 
significant  extensions should put extra effort into these and not complex analysis section since the 
analysis marks can accept well-written code supporting extensions as evidence of understanding. 
The aim of this is so that you get marks for good code, 
if you can write signiifcant good code without being motivated to write reports.

There are signiifcant differences between different code sections, and also things which 
change dependning on what your base types are and how ambitious your code rewrite it. Ask early
if you are concerned about how this assessment will impact you. There will be ways to reward code 
writing which can be found if need be in individual cases.

## Code Quality

This will be assessed based on the code. You can **but do not have to** highlight here things you are particularly proud of and want markers to look at (up to 3) as short bullet points:

* Refactored the function [`makeAllJumps`](https://github.com/xw2519/HLP-Project-2022-Team_8/blob/d27d4c07909365bae120d256b34ba3fb0002b7ec/src/Renderer/DrawBlock/BusWire.fs#L1906-L2016) to remove mutable variables.
* Implementated the extension [`computeWireSplitCoord`](https://github.com/xw2519/HLP-Project-2022-Team-8/blob/d27d4c07909365bae120d256b34ba3fb0002b7ec/src/Renderer/DrawBlock/BusWire.fs#L2024-L2088) to return the Split-Wire coordinates.
* Merged two functions and implemented the Model Mode selection system [`updateOrResetWireSegmentJumps`](https://github.com/xw2519/HLP-Project-2022-Team-8/blob/d27d4c07909365bae120d256b34ba3fb0002b7ec/src/Renderer/DrawBlock/BusWire.fs#L2102-L2109).


Your code will all be assessed anyway, so this is optional.

## Analysis

[`makeAllJumps`](https://github.com/xw2519/HLP-Project-2022-Team_8/blob/d27d4c07909365bae120d256b34ba3fb0002b7ec/src/Renderer/DrawBlock/BusWire.fs#L1906-L2016)

* This function takes in a model as well as a ConnectionID List and returns a new model. It will iterate throught all the elements of the model, and check if two orthogonal segments intersect. If the function finds such case it will keep modify the JumpCoordinateList of the horizontal segment. This allows us to represent such jumps in OldfashionRepresentation.

* The old version of this function was using "for loops" and mutable data opposite to F# directories, as well as being very messy and hard to understand.

* The new function uses pipeline (Array.map, Array.fold...) by creating a Grid, making an array of all possible pairs of wires in the model and iterating through those to check intersection. It then updates the given model with the modified Map<ConnectionId,Wire>. This is much more readable and agrees with F# guidelines.

* `splithorizontalVertical`: splits the given Segment array and extracts from that two arrays one for Horizontal and the other for Vertical. Also check if it has to do the processing given if the Segment Id is in wiresWithNoJumpsA.
* `makeHoriVertiGrid`: creates an array of Segment pairs. Those pairs are composed of an horizontal and a Vertical Segment.
* `registerAllJumps`: extracts an array of SegmentId and float out of the Grid, when the two segments in the pairs intersect.
* `changeSegment`: iterate through all the segments in the model and checks if their ids are included in the jump array obtained before. If yes then we update its JumpCoordinateList component else we set it to an empty list.

[`updateOrResetWireSegmentJumps`](https://github.com/xw2519/HLP-Project-2022-Team-8/blob/d27d4c07909365bae120d256b34ba3fb0002b7ec/src/Renderer/DrawBlock/BusWire.fs#L2102-L2109)

* This function takes in a model as well as a ConnectionID List and returns a new model. This function allows you to update or Reset the Jump components of a given model.

* The old version of this function was divided into two parts one for update and the other one for reset. This method was quite unpracticle as it was requiring the same arguments and calling the same function, hard setting for the Update one the ConnectionID List to an empty List. 

* The new function combined the two functions by instead assigning the ConnectionID List to an empty list in the case of Update. This allowed for clearer and less redondant code.

[`updateWires`](https://github.com/xw2519/HLP-Project-2022-Team-8/blob/d27d4c07909365bae120d256b34ba3fb0002b7ec/src/Renderer/DrawBlock/BusWire.fs#L2116-L2134)

* This function takes in a model, a ConnectionID List and an XYPos returning a new model. This function updates the model by updating its characteristics when something is changed/moved. It needs to check which type of wire it needs to update, in order to call the right function.

* The old version was composed of an if else statement, which wasn't clear enough.

* The new function now operates a match statement and makes more readable code when dealing with carried if else statements.
* Also renamed the parameter "fullyConnected" into "InOutConnected" which is more understandable.

[`update`](https://github.com/xw2519/HLP-Project-2022-Team-8/blob/d27d4c07909365bae120d256b34ba3fb0002b7ec/src/Renderer/DrawBlock/BusWire.fs#L2137-L2403)

* Defines a list of functions that update the model and communicates with others with the use of Cmd messages (defined as outputs).

* `AddWire`: This function allows the systems to add a wire to the existing model. 
   * Removed the "wireWidthFromSymbol" parameter definition as it was not used anywhere.
   * Modified the calling of the function "updateWireSegmentJumps" to "updateOrResetWireSegmentJumps" by removing the unused ConnectionId list for Update.
* `BusWidths`: Updates the width of wires and Segments linked to there state in the model: SplitWire/MergeWire
   * Transformed the if else statement for "newColor" into a match statement to simplify visualisation and conform to "width"
   * `addSymbolWidthFolder` bad naming for the map component "m" changed to "mapSymbolId".
* `CopyWires`: iterates through every wires in the model and checks if their id is inside the selection for copy.
* `ErrorWires`: Change the colors of wires if it detected an error in the system (not connected or misconnected, etc).
   * Hesitated in changing the if else statement into a match case but decided that it is actually clearer the way it is.
   * Reformatted the model declaration for clarity as it wasn't super clear before.



### Issues in Existing Code

#### Bad Function list

List any problems with the existing code **concisely**  as numbered points one function per point. State why
the problem exists. List only functions with **significant problems**. You can should refer to XML comments 
in your refactored code where this helps. You may note the worst 3 functions even if they have problems that are not
significant.

* if function is poorly documented say why this is necessary (what is not obvious from a good name + 
* parameter names + types).
* if function has bad name say why this is confusing
* if a function is poorly written indicate what rewriting would improve this (briefly). You can 
refer to your code if this helps.

#### Other problems

State **concisely** Issues with existing code, or in refactoring for new types, that do not fit into per function list. 
Again numbered points, at most 3. Choose the most inmportant if you have too much to say. You can should
refer to documentation (not XML docs) in code where this helps.

### Analysis of how/why code works

This section need not contain analysis if the code can be demonstarted working. In 
that case list as bullet points the features you will demonstarte (quickly) in the 5 min
interview.

* If code works fully and can be demonstrated in the 5 minute feedback interview no analysis is needed. 
* If code will be manually tested in interview say what the tests are that supplement your analysis
* Interview code must be the assessed branch (not something else, or using later group code)
* A good way to show code works is to explain how it differs from existing working code and how existing
functionality is preserved.

# Extensions

Extensions are required for mark > 70 and optional for mark below 70. High marks on 
the first two sections cannot boost extensions mark if this is lower than them.

$baseMark = \min (70, code * 20/35 + analysis * 15/35)$

$extendedMark = code * 20/50 + analysis * 15/50  + extensions * 15/50$

$overallMark = \max (baseMark, extendedMark)$

* This section can be missing if you have not done significant extension work.
* Extension code, if well documented (in the code) and clearly written, can be assessed without working 
  (if it is demonstrated it depends on other people's code not yet written). 
* Don't bother writing this section unless you have significant extension work, because the mark here 
  will usually not be counted in that case (see the marking scheme). Marks for extensions will be awarded 
only for work of C level or above.

* delete the above comemnts and add your satement of extensions as below*

1.  List as numbered points the extensions (features) your code will support

     a. Use nested letters for the functions you have written extra, 
     or changed, to allow this, and for concise comments concise comments about why they work.



