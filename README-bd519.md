# Example README for individual code submission

## Team Shared Team.md

[Team Contribution Repo](https://github.com/tomcl/hlp22docs/blob/main/Team8.md)

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

* `AddWire`: Allows the systems to add a wire to the existing model. 
   * Removed the "wireWidthFromSymbol" parameter definition as it was not used anywhere.
   * Modified the calling of the function `updateWireSegmentJumps` to `updateOrResetWireSegmentJumps` by removing the unused ConnectionId list for Update.
* `BusWidths`: Updates the width of wires and Segments linked to there state in the model: SplitWire/MergeWire
   * Transformed the if else statement for "newColor" into a match statement to simplify visualisation and conform to "width"
   * `addSymbolWidthFolder` bad naming for the map component "m" changed to "mapSymbolId".
* `CopyWires`: Iterates through every wires in the model and checks if their id is inside the selection for copy.
* `ErrorWires`: Changes the colors of wires if it detected an error in the system (not connected or misconnected, etc).
   * Hesitated in changing the if else statement into a match case but decided that it is actually clearer the way it is.
   * Reformatted the model declaration for clarity as it wasn't super clear before.
* `SelectWires`: Updates the model wires by changing their color flagging that they have been selected. Every color will describe a different case, the normal one being purple.
   * Changing from an if else statement to a match case helps a lot xith the clarity of the code, allowing you to easily understand every cases.
* `DeleteWires`: Creates a new model without the wires with Id component contained in connectionIds.
   * Changing the name of the variable "newModel" into "resetWireModel" which is more descriptive in the context.
   * Replacing the function `resetWireSegmentJumps` into `updateOrResetWireSegmentJumps` to adapt to my new function. As it was Reset we keep the ConnectionId list as is.
   * removing the wire component from the lambda and changing it to _, it just takes place and confuses the reader.
* `DragWire`: Describes every stage of dragging a wire by calling the right functions each time, verifying that the segment is draggable.
   * Changed the function in order to gather all the second parts of the return type, Cmb.none at the end as they do not change through out the function.
   * Modified the implementation of segment extraction, instead of using recursion, I used List.tryFind doing the same but neater embedded in a match statement to extract the Some value or else throwing an error.
* `ColorWires`: Changes the color of a wire with a given color.
   * Added the arguments types in order to protect the function parameter.
   * Changed the name of "newWires" to "newWiresMap" as it also contains the ConnectionId.
   * Changed the name of "prevWires" to "oldWiresMap" for the same reason and because it is more understandable.
* `ResetJumps`: As the name indicates it reinitialise the JumpCoordinate list of all segments whose ids are include in the ConnectionId list.
   * Changed the function to call the new `updateOrResetWireSegmentJumps` keeping the ConnectionId list as is for Reset type.
* `MakeJumps`: Updates the JumpCoordinate list of the segments in the model.
   * Changed the parameter connids to _ as not required for update type of function
   * Changed the function to call the new `updateOrResetWireSegmentJumps` removing the ConnectionId list as in Update type.
* `ResetModel`:
   * Reformated the model declaration for clarity.
* `LoadConnections`:
   * Renamed the component variable "conn" into "connections" making more sense as well as declaring its type for safety.
   * Removed the Lambda function which had no sense and did nothing except debugging.
   * Creating a DU for the inOut component of the function `makeWirePosMatchSymbol` and changing from true to input and false to output.
   

# Extensions

1.  Switch to calculate jumps or circles

     a. [`ChangeMode`](https://github.com/xw2519/HLP-Project-2022-Team-8/blob/7da6be6db998d9690aa2e6057a629895ee314a77/src/Renderer/DrawBlock/BusWire.fs#L2395-L2406):
     It allows along the modification made in Sheet.fs and Renderer.fs the definition of a new command Ctrl+M. This command allows you to switch mode from a list of three: OldFashioned Radiussed or Modern, using a match case to jump from one mode to the next upon pressing the command. It will store the selected Mode using a new type definition declared as a DU part of the Model: type Modes. It calls the function `updateOrResetWireSegmentJumps` to reset the parameters after changing modes.
     
     b. [`updateOrResetWireSegmentJumps`](https://github.com/xw2519/HLP-Project-2022-Team-8/blob/d27d4c07909365bae120d256b34ba3fb0002b7ec/src/Renderer/DrawBlock/BusWire.fs#L2102-L2109):
     Modified this function in order to know which function to call depending on the mode we are in. This is done by adding a match statement based on "model.Mode" and iterating through all the possibilities. If the mode is Radiussed we just return the model without modifications as its dealt with in another part.
     
     c. [`computeWireSplitCoord`](https://github.com/xw2519/HLP-Project-2022-Team-8/blob/d27d4c07909365bae120d256b34ba3fb0002b7ec/src/Renderer/DrawBlock/BusWire.fs#L2024-L2088):
     This function retrieves the coordinates of the Splits happening on the wires of the model.
     
     * `makeWireGrid`:
        We first create a grid containing all possible pairs/combination of wires in the model

     * `getPorts`:
     Once we have definied the grid containing all pairs, we are going to iterate throught all the possibilities verifying two conditions:
     do they have the say OutputPort and if they have, do they have different InputPort. The pairs satisfying both conditions go to the next stages filtering out both the ones not having Split wires and the pairs of same wires.

     * `findCircle`:
      Converting the List to sequences and then zipping them allows us to reduce the size of the biggest to the smallest segment list of the two wires, avoiding lenght issues while zipping. This creates a sequence of Segment pairs.
      We then use the function Seq.tryFind to get the first segment pair which has the same Starting but not the same Ending coordinates.
      Finally, in order to get the coordinate of the splitting point we iterate through all the cases, using the directions of the segment's vector. Either giving back the start Position of the segments if opposite directions, else the end position of the shortest vector distance of the given segments.


          
        

    



