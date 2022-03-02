# README for individual code submission - tlp19

## Instructions

*** REMOVE THIS AT THE END***

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

***-- UPDATE THIS AT THE END: WITH MAIN SECTION AND TRADED FUNCTIONS IN SECTION 1 --***

[Common repo Team8 file](https://github.com/tomcl/hlp22docs/blob/main/Team8.md)

[Buswire (section 2)](./src/Renderer/DrawBlock/BusWire.fs)

Section 2 on my file is lines : **1310-1860** - [Link](./src/Renderer/DrawBlock/BusWire.fs#L1310)

I am also responsible for lines **325-410** (functions `xyVerticesToSegments`, `makeInitialSegmentsList`) - [Link](./src/Renderer/DrawBlock/BusWire.fs#L325)

I have also worked on implementing the Extension for BusWire section 2, which is to implement AutoRouting for rotated Symbols. This part of my code is in the file Autorouted.fs - [Link](./src/Renderer/DrawBlock/Autorouted.fs) - but is currently not included in the compile script, as adding it in caused the following error when compiling: [Link to EdStem Post](https://edstem.org/us/courses/17809/discussion/1222697). I am hoping this part of my code can also be assessed, as a basis for coding style (even though it does not work).
 

## Code Quality

Highlights:

* New types: Segment using a Vector as parameter, as well as a Start position, to represent both the Length and Direction of the Segment.


## Analysis

### Issues in Existing Code

#### Bad Function list

1. The function `segmentIntersectsSegmentCoordinates` has an inconsistent name with the fact that it returns an `Option`, and that it takes 2 tuples of coordinates, instead of two Segment types.

1. The function `segmentIntersectsBoundingBoxCoordinates` returns a tuple of a `bool` and an `Option`, which is a bad output type. It could return a custom type, via an `Anonymous record`, but as the `Option` is not even used throughout the code, it can simply be removed. Inside the function, the calculation that gets the topLeft and bottomRight corners of the box, to then compute the width and height of the Bounding Box is useless as these parameters are already in the `BoundingBox` type. A similar redundant computation used later on in the function can similarly be removed to simply the function.

1. The function `routeGivenWiresBasedOnPortPositions` is not used and can be deleted.

1. The function `checkSegmentAngle` is very poorly written, but can simply be removed as it is not used.

1. The function `removeRedundantSegments` is overall quite poorly written. It does not handle general cases well.

1. The function `getSafeDistanceForMove` is uses the similar pattern to check if two floats are equal multiple times. We can extract it into a helper function. The big match statement can have its cases re-organized to clarify its purpose, and the first match case to check if the wire is Horizontal can also be remove as it not needed.

1. The function `moveSegment` calls an anonymous funtion that is longer that the rest of the function body. It should be extracted into a local function to make the code clearer.

1. The function `filterWiresByCompMoved` has a very obsure name unless we see where it called. To make it more general, it should be renamed to `getWiresConnectedToPorts`. 

1. The function `filterWiresByCompMoved` has a 3-tuple as return type, this could be changed to an Anonymous record for better usability and clarity.

1. The XML comments of the function `filterWiresByCompMoved` say that it should return wires connected to inputs ONLY, output ONLY, and those connected to both. However, in its implementation, the filters for wires connected to inputs only do not exclude wires connected to both, and similarly for the filters regarding wires connected to outputs only. This needs to be added to the filter functions.

1. The XML comment of `partialAutoRoute` is out of date, as the `ReverseFun` parameter is not being used. It should be updated. The name could also be improved to denote the fact that it returns an Option, and the names of local functions can also be improved to be made more clear and explicit.

1. The function `negXYPos` has a very confusing name, and should be renamed to `addToPosAndKeepRoutingMode`. Moreover, after refactoring the types, this function can be completely removed.

1. The function `moveWire` has a big part of computation inside of the type instanciation. This should be done outside, in the function body, to make the function cleaner.

1. The function `updateWire` has a very cryptic boolean parameter called `inOut` that suggests that the wire in connected to an output, on the contrary, this boolean is used to denote that a wire is connected to an Input port, and should therefore be renamed to `inInputPort`


#### Other problems

1. The use of negated coordinates to denote Manual routing as opposed to Autorouting is a very bad implementation choice. This can easily be replaced by a simple boolean called `Autorouted` is the new Segment type, allowing the to be simpler, clearer and safer.

1. The code only allowed for 7-segment wires. This is very restrictive, although it can represent most of the wire shapes. The code should be extended to allow for wires with different number of segments.

### Analysis of how/why code works

*This section need not contain analysis if the code can be demonstarted working. In *
*that case list as bullet points the features you will demonstarte (quickly) in the 5 min*
*interview.*

* *A good way to show code works is to explain how it differs from existing working code and how existing*
*functionality is preserved.*


1. Function `getClickedSegment` - **Detecting which segment is clicked**
    - The function establishes a bounding bow around the coordinates that the user has clicked at (given that it is on a wire) and checks which of the wire's segments was clicked on.
    - `getIntersectingSegments` is used to get the intersecting segments of the bounding box. It filters all segments of the current wire using the `isSegmentIntersectingBoundingBox` function.
        - This second function first finds the TopLeft and BottomRight corners of the bounding box with `getTopLeftAndBottomRightCorner`, and then tries to get any intersection with the current segment being filtered using `tryCoordinatesIfIntersection`. This function, as the name suggests, returns an Option that is `None` if there is no intersection, or `Some XYPos`reprensenting the coordinates of the intersection.
    - If one or more segments are found, then the segment recognized as being 'clicked' is the first one of them.
    - If no intersecting segments are found, the function `getClosestSegment` is called, and it returns the segment that is the closest to the mouse click, according to their Euclidian distance calculated by the `distanceFromPointToSegment` function.

1. Functions `moveWire` and `moveSegment` - **Moving a wire manually**
    - `moveSegment` gets called once a segment is clicked and is being dragged by the user. It performs the appropriate modifications to the position of the segment being dragged, and to its neighbours that have their Vector component being incremented by the amount moved, and one of them also has their start point being moved as well.
        - The function `getSafeDistanceForMove` is used to restrict the distance that a segment can be moved by, according to if it is going to collide with either one of the Symbols on the ends of the wire.
        - In parallel of the segments being moved, `removeRedundantSegments` is called to adjust any two segments going in opposite direction and canceling each other.
    - `moveWire`, on the other hand, gets called whenever a whole wire is selected by a click-and-drag form the user, and simply translates all segments of the selected wire in the direction and distance of the mouse-drag.
        - The local function `translateSegment` is called, and it simply operates the translation on the start of each segment, as their end is defined relative to their start using their `Segment.Vector` component. This is different from the previous segment type, where both the start and end of a segment had to be translated by the function. Changing the type of Segment to include a `Autorouted` boolean component allows this function to be greatly simplified, as we no longer need to operate on the absolute values of the coordinates of the segment. 

1. Function `updateWire` - **Partial Routing**
    -
    - 
    -

1. Function `autorouteWire` - **AutoRouting**
    - 



# Extensions


1.  Attempt at full autorouting for rotated components

     a. `makeInitialSegmentsListFromRISegs` was written in order to implement full autorouting for rotated components. Provided that it worked, it would have been compatible with the already implemented partial routing after manually moving components. It uses normalization of it's given input Port coordinates to define base cases for several possible combinations of port orientations and positions, and then applies those normalized cases back onto a real RotationInvariant Wire that is rotated to match the initial requirements of the Wire.