﻿(*
  Helper functions for drawing on SVG canvas: mainly used by the draw block.
*)

module DrawHelpers
open Browser.Types
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props


//-------------------------------------------------------------------------//
//------------------------------Types--------------------------------------//
//-------------------------------------------------------------------------//

/// Position on SVG canvas
/// Positions can be added, subtracted, scaled using overloaded +,-, *  operators
/// currently these custom operators are not used in Issie - they should be!
type XYPos =
    {
        X : float
        Y : float
    }

    /// allowed tolerance when comparing positions with floating point errors for equality
    static member epsilon = 0.0000001
    /// Add postions as vectors (overlaoded operator)

    static member ( + ) (left: XYPos, right: XYPos) =
        { X = left.X + right.X; Y = left.Y + right.Y }

    /// Subtract positions as vectors (overloaded operator)
    static member ( - ) (left: XYPos, right: XYPos) =
        { X = left.X - right.X; Y = left.Y - right.Y }

    /// Scale a position by a number (overloaded operator).
    static member ( * ) (pos: XYPos, scaleFactor: float) =
        { X = pos.X*scaleFactor; Y = pos.Y * scaleFactor }

    /// Compare positions as vectors. Comparison is approximate so 
    /// it will work even with floating point errors. New infix operator.
    static member ( =~ ) (left: XYPos, right: XYPos) =
        abs (left.X - right.X) <= XYPos.epsilon && abs (left.Y - right.Y) <= XYPos.epsilon

let euclideanDistance (pos1: XYPos) (pos2:XYPos) = 
    let vec = pos1 - pos2
    sqrt(vec.X**2 + vec.Y**2)

/// example use of comparison operator: note that F# type inference will not work without at least
/// one of the two operator arguments having a known XYPos type.
let private testXYPosComparison a  (b:XYPos) = 
    a =~ b


type BoundingBox = {
    X: float
    Y: float
    W: float
    H: float
}

type PortLocation = {
    X: float
    Y: float
    R: float
}

type MouseOp = 
    /// button up
    | Up
    /// button down
    | Down
    /// Move with button up
    | Move 
    /// Move with button Down
    | Drag

type MouseT = {
    Pos: XYPos
    Movement: XYPos
    Op: MouseOp}

/// Record to help draw SVG circles
type Circle = {
    ///  Radius of the circle
    R: float  
    /// color of outline: default => black color
    Stroke: string
    /// width of outline: default => thin
    StrokeWidth: string
    /// Fill: 0.0 => transparent, 1.0 => opaque
    FillOpacity: float // transparent fill
    /// color of fill: default => black color
    Fill: string
}

/// Record tonhelp draw SVG lines
type Line = {
    /// color of outline: default => black color
    Stroke: string
    /// width of outline: default => thin
    StrokeWidth: string
    /// what type of line: default => solid
    StrokeDashArray: string
}


/// Record to help create SVG paths (for wire segment jumps ONLY)
type Path = {
    Stroke: string
    StrokeWidth: string
    StrokeDashArray: string
    StrokeLinecap: string
    Fill: string
}

/// Record to help create SVG polygons
type Polygon = {
    Stroke: string
    StrokeWidth: string
    FillOpacity: float
    Fill: string
}

/// Record to help create SVG text
type Text = {
    /// left/right/middle: horizontal algnment vs (X,Y)
    TextAnchor: string
    FontSize: string
    FontWeight: string
    FontFamily: string
    Fill: string
    UserSelect: UserSelectOptions
    /// auto/middle/hanging: vertical alignment vs (X,Y)
    DominantBaseline: string
}

let testCanvas = Browser.Dom.document.createElement("canvas") :?> HTMLCanvasElement
let canvasWidthContext = testCanvas.getContext_2d()

let getTextWidthInPixels(txt:string, font:string) =
   canvasWidthContext.font <- font; // e.g. "16px times new roman";
   canvasWidthContext.measureText(txt).width;


/// Default line, change this one to create new lines
let defaultLine = {
    Stroke = "Black"
    StrokeWidth = "1px"
    StrokeDashArray = "None"
}

/// Default path, change this one to create new paths
let defaultPath = {
    Stroke = "Black"
    StrokeWidth = "1px"
    StrokeDashArray = "None"
    StrokeLinecap = "butt"
    Fill = "transparent"
}

/// Default polygon, change this one to create new polygons
let defaultPolygon = {
    Stroke = "Black"
    StrokeWidth = "1px"
    FillOpacity = 1.0
    Fill = "None"
}

/// Default circle, change this one to create new circles
let defaultCircle = {
    R = 5.0
    Stroke = "Black"
    StrokeWidth = "1px"
    FillOpacity = 1.0
    Fill = "None"
}

/// Default text, change this to create new text types
let defaultText = {
    TextAnchor = "Middle"
    FontSize = "10px"
    FontFamily = "Verdana, Arial, Helvetica, sans-serif" // Change font family to something good
    FontWeight = "Normal"
    Fill = "Black"
    UserSelect = UserSelectOptions.None
    DominantBaseline = "Hanging"
}

/// Port circle, used by both Sheet and Symbol to create ports
let portCircle = { defaultCircle with R = 5.0; Stroke = "Black"; StrokeWidth = "1.0px"; Fill = "Grey"}
//--------------------------------------------------------------------------//
//-----------------------------Helpers--------------------------------------//
//--------------------------------------------------------------------------//

/// return a v4 (random) universally unique identifier (UUID)
/// works under .NET and FABLE
#if FABLE_COMPILER
let uuid():string = import "v4" "uuid"
#else
let uuid():string = System.Guid.NewGuid.ToString()
#endif

// ----------------------------- SVG Helpers ----------------------------- //

/// Makes a line ReactElement, wildcard inputs as position can be a number or a string 
let makeLine (x1: 'a) (y1: 'b) (x2: 'c) (y2: 'd) (lineParameters: Line) =
    line [
            X1 x1
            Y1 y1
            X2 x2
            Y2 y2
            SVGAttr.Stroke lineParameters.Stroke
            SVGAttr.StrokeWidth lineParameters.StrokeWidth
            SVGAttr.StrokeDasharray lineParameters.StrokeDashArray
    ] []

/// Makes a path ReactElement, points are to be given as an XYPos record element.
/// Please note that this function is designed to create ONLY "Move to - Bézier Curve"
///paths (this is what the "M" and "C" attributes stand for) and NOT a generalized SVG path element.
let makePath (startingPoint: XYPos) (startingControlPoint: XYPos) (endingControlPoint: XYPos) (endingPoint: XYPos) (pathParameters: Path) =
    let x1, y1, x2, y2 = startingPoint.X, startingPoint.Y, endingPoint.X, endingPoint.Y
    let dx1, dy1, dx2, dy2 = startingControlPoint.X, startingControlPoint.Y, endingControlPoint.X, endingControlPoint.Y
    let dAttrribute = sprintf "M %f %f C %f %f, %f %f, %f %f" x1 y1 dx1 dy1 dx2 dy2 x2 y2
    path [
            D dAttrribute
            SVGAttr.Stroke pathParameters.Stroke
            SVGAttr.StrokeWidth pathParameters.StrokeWidth
            SVGAttr.StrokeDasharray pathParameters.StrokeDashArray
            SVGAttr.StrokeLinecap pathParameters.StrokeLinecap
            SVGAttr.Fill pathParameters.Fill
    ] []
    
/// Makes a polygon ReactElement, points are to be given as a correctly formatted SVGAttr.Points string 
let makePolygon (points: string) (polygonParameters: Polygon) =
    polygon [
            SVGAttr.Points points
            SVGAttr.Stroke polygonParameters.Stroke
            SVGAttr.StrokeWidth polygonParameters.StrokeWidth
            SVGAttr.Fill polygonParameters.Fill
            SVGAttr.FillOpacity polygonParameters.FillOpacity
    ] []
    
/// Makes a circle ReactElement
let makeCircle (centreX: float) (centreY: float) (circleParameters: Circle) =
    circle
      [ 
        Cx centreX
        Cy centreY
        R circleParameters.R
        SVGAttr.Fill circleParameters.Fill
        SVGAttr.FillOpacity circleParameters.FillOpacity
        SVGAttr.Stroke circleParameters.Stroke
        SVGAttr.StrokeWidth circleParameters.StrokeWidth
      ] []
      
/// Makes a text ReactElement
let makeText (posX: float) (posY: float) (displayedText: string) (textParameters: Text) =
    text [
            X posX; 
            Y posY; 
            Style [
                TextAnchor textParameters.TextAnchor
                DominantBaseline textParameters.DominantBaseline
                FontWeight textParameters.FontWeight
                FontSize textParameters.FontSize
                Fill textParameters.Fill
                UserSelect textParameters.UserSelect
                
            ]
        ] [str <| sprintf "%s" (displayedText)]



/// deliver string suitable for HTML color from a HighlightColor type value
let getColorString (col: CommonTypes.HighLightColor) =
    (sprintf "%A" col).ToLower()



//--------------------------------Constants----------------------------------//

/// these determine the size of the draw block canvas relative to the objects on it.
let canvasUnscaledSize = 3500.0




    

