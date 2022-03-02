# README for individual code submission - tlp19

## Admin and quick access links

***-- UPDATE THIS AT THE END: WITH MAIN SECTION AND TRADED FUNCTIONS IN SECTION 1 --***

[Common repo Team8 file](https://github.com/tomcl/hlp22docs/blob/main/Team8.md)

[Buswire (section 2)](https://github.com/xw2519/HLP-Project-2022-Team-8/blob/hlp22-indiv-assess-tlp19/src/Renderer/DrawBlock/BusWire.fs)

Section 2 on my file is lines : **1310-1850** - [Link to code section](./src/Renderer/DrawBlock/BusWire.fs#L1310)

I am also responsible for lines **325-410** (functions `xyVerticesToSegments`, `makeInitialSegmentsList`) - [Link to code section](./src/Renderer/DrawBlock/BusWire.fs#L325)

I have also worked on implementing the Extension for BusWire section 2, which is to implement AutoRouting for rotated Symbols. This part of my code is in the file Autorouted.fs - [Link to file](https://github.com/xw2519/HLP-Project-2022-Team-8/blob/hlp22-indiv-assess-tlp19/src/Renderer/DrawBlock/Autorouted.fs) - but is currently not included in the compile script, as adding it in caused the following error when compiling: [Link to EdStem Post](https://edstem.org/us/courses/17809/discussion/1222697). I am hoping this part of my code can also be assessed, as a basis for coding style (even though it does not work).
 

## Code Quality

Highlights:

* New types: The refactored type `Segment` uses a `Vector` as a component, instead of an `End` point. This allows both the Length and Direction of the Segment to be represented in a simple and intuitive data type (`XYPos`). This allows multiple calculations throughout the code to be simplified.


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
        - Moving a wire manually sets its `Autorouted` boolean to false, disabling Autorouting for this segment, fixing it in its position (under certain conditions, see function `updateWire` bellow).
    - `moveWire`, on the other hand, gets called whenever a whole wire is selected by a click-and-drag form the user, and simply translates all segments of the selected wire in the direction and distance of the mouse-drag.
        - The local function `translateSegment` is called, and it simply operates the translation on the start of each segment, as their end is defined relative to their start using their `Segment.Vector` component. This is different from the previous segment type, where both the start and end of a segment had to be translated by the function. Changing the type of Segment to include a `Autorouted` boolean component allows this function to be greatly simplified, as we no longer need to operate on the absolute values of the coordinates of the segment. 

1. Function `updateWire` - **Partial Routing**
    - The function `updateWire` gets called whenever the user clicks on a Symbol and moves it. It receives as parameters the port of the component that moved, as well as if this port is an Input or Output port via the `inInputPort` boolean.
    - This function will first get the new coordinates of the port that moved, either by looking it up in the list of Input Ports of the Symbol model, or in the list of Output Ports, according to the `inInputPort` boolean.
    - Then, if the port was an Input Port, then we know that it was on the end of the wire. We therefore reverse the list of segments to re-route all of them using the `tryPartialAutoRoute` function. This function returns an Option of whether or not the partial routing worked.
        - `tryPartialAutoRoute` performs two main checks on the wire:
            - Using `tryGetIndexOfFirstManuallyRoutedSegment`, the index of the first manually routed segment is returned as an Option. If there are no manually routed in the wire, it returns None and the check fails. To find this index, it performs a `List.takeWhile` on the segments with the condition that their `Autorouted` boolean is set to true.
            - Using `checkTopologyChangeOption`, it checks if the end of the wire being moved goes into another quadrant w.r.t. the fixed end of the manually routed segment.
        -  If both of those checks are met, then it re-routes partially the wire:
            - The function `scaleAutoroutedSegments` takes the index of the first manually routed segment, and performs scaling on the appropriate segments in order for the Wire to remain intact and coherent.
    - If the partial routing failed, either because no segments are currently manually routed or because the topology of the wire changed, then the function `updateWire` defaults back to operating a full autorouting on the wire...

1. Function `autorouteWire` - **AutoRouting**
    - Given a wire, this function finds the two ports that its ends are connected to, and then calls `makeInitialSegmentsList` to build an auto routed list of segments to replace the current one of the wire.
    - The `makeInitialSegmentsList` function first computes the appropriate length of the 'sticks'. Those sticks are the first and last segment of a wire's segment list, making the visual connection between the wire and the ports it is connected to. They are not draggable and have fixed length. It then establishes a basic pattern of X-Y coordinates that the wire should follow in order to join the two ports, each segment being perpendicular to the previous one.
    - `xyVerticesToSegments` is then called in order to first convert those coordinates into pairs of end points of segments, and then convert those pairs of endpoints back into regular Segments with a start position and a vector.


# Extensions

1.  Attempt at full autorouting for rotated components

     a. `makeInitialSegmentsListFromRISegs` was written in order to implement full autorouting for rotated components. Provided that it worked, it would have been compatible with the already implemented partial routing after manually moving components. It uses normalization of it's given input Port coordinates to define base cases for several possible combinations of port orientations and positions, and then applies those normalized cases back onto a real RotationInvariant Wire that is rotated to match the initial requirements of the Wire.