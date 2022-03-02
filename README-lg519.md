# Example README for individual code submission

## Instructions


* A link to the repo and branch must be on the `indiv` sheet of Tom Clarke's Team [google spreadsheet](https://docs.google.com/spreadsheets/d/1prQ5usnpu36FgtbsMO8j6_mwbdd34haSMOQKN2OkLBA/edit?usp=sharing)
* The repo you use **must have your marker added as collaborator** (github login is on indiv assessment spreadsheet page)

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

[Team Contribution Repo](https://github.com/xw2519/HLP-Project-2022-Team-8.git)


## Admin and quick access links

*link to your teamN.md file*
[Common repo TeamN file](https://github.com/xw2519/hlp22docs/blob/main/Team8.md)

[Symbol (section 3)](src/Renderer/DrawBlock/Symbol.fs)

Section 3 on my file is lines : 627- end of file

I am also responsible for lines 850-851 (Rotation of symbols pressing key R)




## Code Quality


This will be assessed based on the code. You can **but do not have to** highlight here things you are particularly proud of and want markers to look at (up to 3) as short bullet points:

- added a new Symbol field Center to substitute the redundant Pos field. This new field allows us to implement rotation easily because the center of a symbol is rotation invariant

- added a new Model field SymbolsCount that is used to label symbols. This allows us to get rid of the highly unreadable getCompList match statement facilitates code mantainance. It also allowes us to implement a new extension in the project work: having specific gate names and counts for the Not,And,Or... components which are currently all labeled with G<index> where <index> is the number of components on the screen
  
- Improved efficiency of getInputPortLocation and getOutputPortLocation which now look up the port from Model.Ports. This allows to get rid of other functions (reduces code complexity) and makes the code easier to mantain because all of the info about ports is looked up from Model.Ports.



## Analysis

This combines analysis of **issues in existing code** with **proof or analysis of how/why the new code works**. 
The marking combines both parts. If new code already works and is well written (especially if it is 
demonstrated to have significant value in terms of ability to support extensions (see Extensions below)
less evidence will be needed for a high mark in this section.
The analysis here demonstrates that you understand the problems in the old code. 
If the old code has no or few problems you can say this.

Anything you say here may if necessary be tested in the 5 min feedback interview to check understanding.

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



**getBoundingBoxofSymbol**
- unnecessary long name
- uses symbol Pos field (redundant field) rather than coordinates from Symbol.Component

-------------------------------------------------------------------------------------
**getSymbolPos**
- function not used anywhere. Removed

**getInputPortsPositionMap**
- should be a subfunction of getCmpsPortLocations

**getOutputPortsPositionMap**
- should be a subfunction of getCmpsPortLocations

**getPortLocations**
- bad name. renamed to getCmpsPortLocations to make clear that the input is a list of Component Ids

**getInputPortLocation**
- Rather than generating getInputPortsPositionMap it should look up the port from Model.Ports. This would improve efficiency and code maintainability.

**getOutputPortLocation**
- Rather than generating getOutputPortsPositionMap it should look up the port from Model.Ports. This would improve efficiency and code maintainability.

**getOnePortLocation**
- function not used anywhere. Removed

**getOnePortLocationNew**
- redundant. Substituted with getInputPortLocation and getOutputPortLocation for efficiency and code maintainability.

**getTwoPortLocations**
- redundant. Substituted with getInputPortLocation and getOutputPortLocation for efficiency and code maintainability

---------------------------------------------------------------------------------

**getCopiedSymbols**
- bad name. It returns a list of ComponentIds not Symbols. renamed to getCopiedSymbolsIds

**getCompList**
- removed. Only used in genCmpIndex to return the Index associated 
with each component. Created new datastracture (SymbolsCount) in model that contains type of each component and how many
times it is displayed in the current model to make up for this function. This improves efficiency and code maintainability

**filterString**
- removed. Equivalent to String.ToUpper

**regex**
- bad name. Renamed to getCmpIndex to make purpose of the function obvious. Restructured to work with SymbolCount.

**labelGenNumber**
- removed. same functionality as generateLabel

**generateLabel**
- bad name. Renamed to genCmpLabel to make clear that the input is a field of Component

**pasteSymbols**
- vague documentation and bad variables/subfunction names. Documentation has been rewritten to explain what the function does. New variable names and comments have been introduced within the function body to explain the algorithm

**getEquivalentCopiedPorts**
- bad name and confusing documentation. Concept of equivalent port is unclear unless you go through the function. Renamed to getPastedPortsIdsFromCopiedPortsIds (long name necessary in my opinion) to make function itent clear. Simplified documentation by removing equivalent port concept.

**changeNumberOfBitsf**
- long and unclear name. renamed to updateCmpBits to make clear that the input is a Component Id
  
**changeLsbf**
- Unclear name. renamed to updateCmpLsbBits to make clear that the input is a Component Id


#### Other problems

State **concisely** Issues with existing code, or in refactoring for new types, that do not fit into per function list. 
Again numbered points, at most 3. Choose the most inmportant if you have too much to say. You can should
refer to documentation (not XML docs) in code where this helps.

- Symbol Pos field is redundant. It represents the top-left corner of a symbol, which is also described by a component X and Y field.


### Analysis of how/why code works

All of the code and the extensions are currently working. Some compatibility issues have been found for the labeling of Inputs and Outputs when incorporating the Symbol part 1 changes from hlp22-indiv-assess-xw2519 branch. It can be proved that the code for **getCompList** works fully by checking out 1b7c13adb1b10efe65eaf0fb94566359391cdc04 (the commit before the merge).

# Extensions

1. Locate port position and orientation, given symbol position and orientation.
    a. This extention did not require extra functions because the rotation has been implented w.r.t. the center of the symbol. The (new) Symbol field Center allows us to do this easily

2. UI to rotate selected symbols by pressing R key or by using the Edit Menu
    a. Msg type in Symbol.fs was extended by adding extra type RotateSymbols of ComponentId list. 
    b. Update function in Symbol.fs was modifed by adding an extra entry RotateSymbols. 
    c. Update function in Sheet.fs was modified to forward message to symbol. 
    d. EditMenu in Renderer.fs was modified by adding an extra entry "Rotate" "R"

3. Make bounding box work with rotation
   a. getSymBoundingBox was modified by taking into account the Rotation field of Symbol 