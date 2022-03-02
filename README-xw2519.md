# Team 8 - xw2519 - Individual Code Submission

## Admin and quick access links

[Common repo Team 8 file](https://github.com/tomcl/hlp22docs/blob/main/Team8.md)

[Symbols (Section 1)](https://github.com/xw2519/HLP-Project-2022-Team-8/blob/hlp22-indiv-assess-xw2519/src/Renderer/DrawBlock/Symbol.fs)

Section 1 on my file is lines : 22 - 729

--- 

## Code Quality

This will be assessed based on the code. You can **but do not have to** highlight here things you are particularly proud of and want markers to look at (up to 3) as short bullet points:

1. Linking rotation operation to Key `R`. All selected symbols can be rotated by pressing the Key `R`.
2. Addition of new `Symbol` attribute `SymbolPoints` which stores the coordinates of shapes which are all initialised at the beginning in the function `initSymbolPoints`. 
3. Renamed symbol names `MergeWires -> Bus Merge` and `SplitWire -> Bus Split` and addition of directional arrows onto those symbols to help differentiate the symbols when they are rotated. 

--- 

## Analysis

### Issues in Existing Code

#### <u> Bad Functions </u>

- [Symbol.fs: Line 80 - 86](https://github.com/xw2519/HLP-Project-2022-Team-8/blob/0e517e099437c23ac5792b14e940bec50300685d/src/Renderer/DrawBlock/Symbol.fs#L80): Converting points between relative to symbol top-left and symbol center

    Current code implements rotation with points where the origin is the top-left corner of the symbol. Sections with rotation can be simplified if all the points of the symbol are changed to be all relative to the symbol center which is rotation-invariant. 

    Person responsible for the second part of Symbol (lg519) has already modified the `Pos` field of `Symbol` type to contain the coordinate of the center of the symbol. To build on, the `SymbolPoints` field can be changed to draw the symbol based on the center. 

- [Symbol.fs: Line 339 - 495](https://github.com/xw2519/HLP-Project-2022-Team-8/blob/0e517e099437c23ac5792b14e940bec50300685d/src/Renderer/DrawBlock/Symbol.fs#L339): Manually adjusting the coordinate of the symbol text depending on rotation

    This leads to long code that is very specific and inefficient. With the location of the symbol center, symbol text can be generated relative to the center and, with rotations, can change the text to be vertical. 

    This would fix the current problem with symbols like `Bus Select` and `Bus Compare` where the symbol text (in horizontal form) does not fit when the symbol rotates. 

#### <u> Other Problems </u> 

- Rotated symbol orientation are not preserved when the sheets are saved. 
- `Bus Select` and `Bus Compare` symbol text do not rotate and would not fit into symbol shape if rotated 90 degrees and 270 degrees.
- Port connections to rotated symbols do not connect directly onto the displayed port. But simulation functionalities are still preserved.

### Analysis of how/why code works

- Rotation of symbol triggered by pressing Key `R` 
  - Show the symbol shape rotating and port titles rotating along with it 

- Displaying `Bus Merge` and `Bus Split` 
  - Show the directional arrows and how it helps in rotation

- Creating a simple circuit with AND gate in normal orientation and rotated orientation
  - Show that the simulation functionalities are preserved

--- 

## Extensions

List as numbered points the extensions (features) your code will support

1. Symbol rotation:
   
   a. Rotation functionality triggered when pressing Key `R` 

   b. Clock and inverted shapes rotate with symbol 

   c. Symbol title and port titles support rotations





