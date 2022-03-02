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






* Naming of `myGoodName`, `myWonderfulLongName`
* New function boundaries: `topfun` -> `firstfun`, `topfun` -> `secondFun`
* New types: MyGoodType
* Helper function `myHelper` which is higher order and really helpful
* I cleaned up all the sprintf and printf statements to use interpolated strings

Your code will all be assessed anyway, so this is optional.

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

**getBoundingBoxes**
- bad name. What bounding boxes? Renamed to getModelBoundingBoxes

**getOneBoundingBox**
- bad name. OneBoundingBox is too vague. Renamed to getCmpBoundingBox


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
- improved efficiency and code maintainability. Rather than generating getInputPortsPositionMap it should look up the port from Model.Ports

**getOutputPortLocation**
- improved efficiency and code maintainability. Rather than generating getOutputPortsPositionMap it should look up the port from Model.Ports

**getOnePortLocation**
- function not used anywhere. Removed

**getOnePortLocationNew**
- removed. Substituted with getInputPortLocation and getOutputPortLocation for efficiency and code maintainability.

**getTwoPortLocations**
- removed. Substituted with getInputPortLocation and getOutputPortLocation for efficiency and code maintainability

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

**changeConstantf**
- renamed to updateConstant for consistency.

**update**
- Structure of the function mostly ok. It needed some renaming of variables (for instance CompList->CompIds) to facilitate understandig.

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