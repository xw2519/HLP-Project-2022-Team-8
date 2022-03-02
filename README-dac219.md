## Team Shared Team.md

[Team Contribution Repo](https://github.com/xw2519/HLP-Project-2022-Team-8)

## Admin and quick access links

*link to your teamN.md file*
[Common repo TeamN file](https://github.com/tomcl/hlp22docs/blob/main/Team8.md)

[Buswire (section 1)](https://github.com/xw2519/HLP-Project-2022-Team-8/blob/hlp22-indiv-assess-dac219/src/Renderer/DrawBlock/BusWire.fs)

Section 3 on my file is lines : 283-837
I am also responsible for lines 20-90 and 131-155 (functions mySplurgeFunc, my2ndSplurgeFunc)

Anything else you need to say about what you are or are not responsible for.

## Code Quality

This will be assessed based on the code. You can **but do not have to** highlight here things you are particularly proud of and want markers to look at (up to 3) as short bullet points:

1. Reduction of code and new implementation of Segement Intersection function
2. New Type: WireDirection, used extensively to render the three extension display modes
3. Implementation of radius rendering function, with calculated dynamic radiuses

Your code will all be assessed anyway, so this is optional.

## Analysis

### Issues in Existing Code

#### Bad Function list

List any problems with the existing code **concisely**  as numbered points one function per point. State why
the problem exists. List only functions with **significant problems**. You can should refer to XML comments 
in your refactored code where this helps. You may note the worst 3 functions even if they have problems that are not
significant.

1. segmentIntersectSegment
	* Requirement to get absolute values of coordinates is not very ideal
	* Requirement of many other functions to be called to check intersections , not neccessary when only horizontal or vertical segments
	* Rewriting to contain a small set of possibilities then checking the range of each value to see if they intersect far cleaner

2. MapToSortedList
	* Very poor naming of function, not clear at all what it does
	* Lots of repeated code, could be cut down to many factors smaller than it is
	* Not well commented and variable names do not make it clear what it does

3. view
	* A very generic and poorly named function, not very clear what view does
	* A name Such as RenderModel is far clearer to its purpose
	* Many variable names not clear at all such as wires and wires1, not easy to differentiate wiresBeforeRender and renderedWires is much better


#### Other problems

1. Overcomplicated first section
	* Many unneccasary functions were used in the first section, 
	the functions created an intermediatary value before feeding it into another function.
	* This was resolved by reducing the code down into one clearer function

2. Many poor variable names that arent clear
	* A lot of functions contain variables such as Ys or Xt, these are not clear at all and can lead to confusion in reading the code

3. Using absolute values instead of having an is manually routed bool
	* This seemed to be something that should never have been implemented this way, it can easily be replaced by a Bool in the Segment type



### Analysis of how/why code works

1. RenderWireAttachBusWidth now calls a new RenderWire Wrapper that connects to the models display type
	* RenderWire can call the new radius wire Render Function or the Old Fashioned/ Modern One
	* Newly implemented Radius wire function 

2. Rendering of all 3 display types
	* Using Ctrl M on the keyboard shifts the render mode from rendering wires and thir correlated jumps, to rendering radius wires to rendering Circle Intersections and no jumps
	* A new circle list is added in order to render the modern display type

3. Segement intersections works by matching all possible cases for only horizontal or vertical wires instead of trying to check if they are colinear or intersect in the general case

# Extensions

1.  List as numbered points the extensions (features) your code will support
	a. Use nested letters for the functions you have written extra, 
	 or changed, to allow this, and for concise comments concise comments about why they work.

1. Radius wire rendering mode
	a. renderRadiusedWire new function that takes an entire wire and maps a radius segment function
	b. RenderWire Wrapper that calls either renderRadiusedWire or renderOldFashionedorModernWire depending on the Display Type

2. Display of Circles/Modern and Display of Jumps/old Fashioned Rendering Mode 
	a. New type Modes which is stored in the model and defines which Display Type to Render
	b. This is then passed from the model to the Rendering Functions, calling the relevant function with correct parameters to render in a specific Display Mode
	c. Working alongside Buswire Section 3 (Bertil) we managed to pass through a keypress (Ctrl-M) to change the models' Mode to my functions to change the rendering type 

