# README for individual code submission - tlp19

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

## Admin and quick access links

***-- UPDATE THIS AT THE END: --***

*link to your teamN.md file*
[Common repo TeamN file](https://github.com/tomcl/hlp22docs/blob/main/README.md)

[Buswire (section 2)](src/renderer/drawblock/buswire.fs)

Section 2 on my file is lines : ***XXXX-XXXX***
I am also responsible for lines 100-120 (functions mySplurgeFunc, my2ndSplurgeFunc)

*Anything else you need to say about what you are or are not responsible for.*

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

* Naming of `myGoodName`, `myWonderfulLongName`
* New function boundaries: `topfun` -> `firstfun`, `topfun` -> `secondFun`
* New types: MyGoodType
* Helper function `myHelper` which is higher order and really helpful
* I cleaned up all the sprintf and printf statements to use interpolated strings

Your code will all be assessed anyway, so this is optional.

***-- FILL THIS IN (OPTIONAL) --***

  

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

***-- FILL THIS IN --***

1. The function `segmentIntersectsSegmentCoordinates` has an inconsistent name with the fact that it returns an `Option`, and that it takes 2 tuples of coordinates, instead of two Segment types.

1. The function `segmentIntersectsBoundingBoxCoordinates` returns a tuple of a `bool` and an `Option`, which is a bad output type. It could return a custom type, via an `Anonymous record`, but as the `Option` is not even used throughout the code, it can simply be removed. Inside the function, the calculation that gets the topLeft and bottomRight corners of the box, to then compute the width and height of the Bounding Box is useless as these parameters are already in the `BoundingBox` type. A similar redundant computation used later on in the function can similarly be removed to simply the function.

1. The function `routeGivenWiresBasedOnPortPositions` is not used and can be deleted.

1. The function `checkSegmentAngle` is very poorly written, but can simply be removed as it is not used.

1. The function `getSafeDistanceForMove` is uses the similar pattern to check if two floats are equal multiple times. We can extract it into a helper function. The big match statement can have its cases re-organized to clarify its purpose, and the first match case to check if the wire is Horizontal can also be remove as it not needed.

1. The function `moveSegment` calls an anonymous funtion that is longer that the rest of the function body. It should be extracted into a local function to make the code clearer.

1. The function `filterWiresByCompMoved` has a very obsure name unless we see where it called. To make it more general, it should be renamed to `getWiresConnectedToPorts`. 

1. The function `filterWiresByCompMoved` has a 3-tuple as return type, this could be changed to an Anonymous record for better usability and clarity.

1. The XML comments of the function `filterWiresByCompMoved` say that it should return wires connected to inputs ONLY, output ONLY, and those connected to both. However, in its implementation, the filters for wires connected to inputs only do not exclude wires connected to both, and similarly for the filters regarding wires connected to outputs only. This needs to be added to the filter functions.

1. The XML comment of `partialAutoRoute` is out of date, as the `ReverseFun` parameter is not being used. It should be updated. The name could also be improved to denote the fact that it returns an Option, and the names of local functions can also be improved to be made more clear and explicit.

1. The function `negXYPos` has a very confusing name, and should be renamed to `addToPosAndKeepRoutingMode`.

1. The function `moveWire` has a big part of computation inside of the type instanciation. This should be done outside, in the function body, to make the function cleaner.

1. The function `updateWire` has a very cryptic boolean parameter called `inOut` that suggests that the wire in connected to an output, on the contrary, this boolean is used to denote that a wire is connected to an Input port, and should therefore be renamed to `isInInputPort`


#### Other problems

State **concisely** Issues with existing code, or in refactoring for new types, that do not fit into per function list. 
Again numbered points, at most 3. Choose the most inmportant if you have too much to say. You can should
refer to documentation (not XML docs) in code where this helps.

***-- FILL THIS IN --***

### Analysis of how/why code works

This section need not contain analysis if the code can be demonstarted working. In 
that case list as bullet points the features you will demonstarte (quickly) in the 5 min
interview.

* If code works fully and can be demonstrated in the 5 minute feedback interview no analysis is needed. 
* If code will be manually tested in interview say what the tests are that supplement your analysis
* Interview code must be the assessed branch (not something else, or using later group code)
* A good way to show code works is to explain how it differs from existing working code and how existing
functionality is preserved.

***-- FILL THIS IN --***
*DESCRIBE ALGORITHM*
  

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

***-- FILL THIS IN: --***

1.  List as numbered points the extensions (features) your code will support

     a. Use nested letters for the functions you have written extra, 
     or changed, to allow this, and for concise comments concise comments about why they work.



