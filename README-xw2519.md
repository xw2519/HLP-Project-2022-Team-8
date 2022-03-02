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
recorded in your team's file in my Team contributions repo](https://github.com/tomcl/hlp22docs/blob/main/Team8.md)  
main branch ./TeamN.md (N = 1 - 9) file. The team contrib repo is as official record. This file will be 
used marking and should have line numbers for easy access. Expect to be marked down if your marker
cannot easily find everything via links from this README.

## Team Shared Team.md

[Team Contribution Repo](https://github.com/tomcl/hlp22docs/blob/main/Team8.md)

## Admin and quick access links

[Common repo Team 8 file](https://github.com/tomcl/hlp22docs/blob/main/Team8.md)

[Symbols (Section 1)]([src/renderer/drawblock/buswire.fs](https://github.com/xw2519/HLP-Project-2022-Team-8/blob/hlp22-indiv-assess-xw2519/src/Renderer/DrawBlock/Symbol.fs))

Section 1 on my file is lines : 22 - 737

## Code Quality

This will be assessed based on the code. You can **but do not have to** highlight here things you are particularly proud of and want markers to look at (up to 3) as short bullet points:

1. Reactoring of original function `drawSymbol` into `createSymbol` which divides the function into: Drawing and Adding text
2. Addition of new `Symbol` attribute `SymbolPoints` which stores the coordinates of shapes which are all initialised at the beginning in the function `initSymbolPoints` 

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

1.  List as numbered points the extensions (features) your code will support

     a. Use nested letters for the functions you have written extra, 
     or changed, to allow this, and for concise comments concise comments about why they work.



