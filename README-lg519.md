# Example README for individual code submission

## Instructions

## Team Shared Team.md

[Team Contribution Repo](https://github.com/xw2519/HLP-Project-2022-Team-8.git)


## Admin and quick access links

*link to your teamN.md file*
[Common repo TeamN file](https://github.com/xw2519/hlp22docs/blob/main/Team8.md)

[Symbol (section 2)](src/Renderer/DrawBlock/Symbol.fs](https://github.com/xw2519/HLP-Project-2022-Team-8/blob/hlp22-indiv-assess-lg519/src/Renderer/DrawBlock/Symbol.fs))

Section 2 on my file is lines : 627- 1104

I am responsible for the Center field of Symbol . Line 41 

I am responsible for the SymbolsCount field of model. Line 59

I am responsible for lines 850-851 in [Sheet](src/Renderer/DrawBlock/Sheet.fs) (Rotation of symbols pressing key R).
For the extension "UI to rotate selected symbols by pressing R key" it should be noted that:
- Xw2519 implemented R keypress to rotate symbols initially as a functionality to test his code.
- I then simplified the logic, removed redundant functions and modified xw2519 code heavily to comply with Elmish MVU guidelines. I have also added functionality to rotate multiple symbols. 


## Code Quality


- added a new Symbol field **Center** to substitute the redundant Pos field. This new field allows us to implement rotation easily because the center of a symbol is rotation invariant

- added a new Model field **SymbolsCount** that is used to label symbols. This allows us to get rid of the highly unreadable getCompList match statement and facilitates code mantainance. It makes it easier to add new types as they will not have to be included manually in getCompList. It also allows us to implement a new extension in the project work: having specific gate names and counts for the Not,And,Or... components which are currently all labeled with G`index` where `index` is the number of components on the screen
  
- Improved efficiency of getInputPortLocation and getOutputPortLocation which now look up the port from Model.Ports. This allows to get rid of other functions (reduces code complexity) and makes the code easier to mantain because all of the info about ports is looked up from Model.Ports.



## Analysis

### Issues in Existing Code

#### Bad Function list

**getBoundingBoxofSymbol**
- unnecessary long name
- uses symbol Pos field (redundant field) rather than coordinates from Symbol.Component

-------------------------------------------------------------------------------------
**getSymbolPos**
- function not used anywhere. Removed

**getInputPortsPositionMap**
- should be a subfunction of getPortLocations as it is only useful there

**getOutputPortsPositionMap**
- should be a subfunction of getPortLocations as it is only useful there

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

- Symbol Pos field is redundant. It represents the top-left corner of a symbol, which is also described by a component X and Y field.


### Analysis of how/why code works

All of the code and the extensions are currently working. Some compatibility issues have been found for the labeling of Inputs and Outputs when incorporating the Symbol part 1 changes from hlp22-indiv-assess-xw2519 branch. It can be proved that the code for **getCompList** works fully by checking out 1b7c13adb1b10efe65eaf0fb94566359391cdc04 (the commit before the merge).

# Extensions

1. Locate port position and orientation, given symbol position and orientation.
    - This extention did not require extra functions because the rotation has been implented w.r.t. the center of the symbol. The (new) Symbol field Center allows us to do this easily

2. UI to rotate selected symbols by pressing R key or by using the Edit Menu
    - Msg type in Symbol.fs was extended by adding extra type RotateSymbols of ComponentId list. 
    - Update function in Symbol.fs was modifed by adding an extra entry RotateSymbols. 
    - Update function in Sheet.fs was modified to forward message to symbol. 
    - EditMenu in Renderer.fs was modified by adding an extra entry "Rotate" "R" (this was done by xw2519 when he was testing his extension)

3. Make bounding box work with rotation
   - getSymBoundingBox was modified by taking into account the Rotation field of Symbol 