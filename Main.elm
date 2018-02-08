module Main exposing (..)

import Html exposing (Html)
import Html.Attributes as HA
import Random
import List.Extra exposing (unique)
import Svg exposing (..)
import Svg.Events exposing (onClick)
import Svg.Attributes exposing (..)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL


type alias Model = 
  { width : Int
  , height : Int
  , mode : Mode
  , state : GameState
  , bombs : Bombs
  , cells : Cells
  }

type alias Cell = 
  { position : Position
  , danger : Int
  , hidden : Bool
  , marked : Bool
  }

type alias Position = (Int, Int)
type alias Bomb = Position
type alias Bombs = List Position
type alias Cells = List Cell
type Mode = Marking | Normal
type GameState = Playing | GameOver

init : (Model, Cmd Msg)
init =
  (newModel gridWidth gridHeight, Random.generate Generate (randomBombs gridWidth gridHeight))

gridWidth =
  10

gridHeight =
  10

doneNothing : Model -> (Model, Cmd Msg)
doneNothing model = 
  (model, Cmd.none)

randomBombs : Int -> Int -> Random.Generator Bombs
randomBombs x y =
  Random.list 10 <| randomBomb (x - 1) (y - 1)

randomBomb : Int -> Int -> Random.Generator Bomb
randomBomb fst snd =
  Random.pair (randomInt fst) (randomInt snd)

randomInt : Int -> Random.Generator Int
randomInt maximum =
  Random.int 0 maximum 

newModel : Int -> Int -> Model
newModel h w =
  Model h w Normal Playing [] (initCells h w)

initCells : Int -> Int -> Cells
initCells w h =
  let
    width = w - 1
    height = h - 1
    listW = List.range 0 width
    listH = List.range 0 height
    positions = cartesian listW listH
  in
    List.map (\position -> Cell position 0 False False) positions

-- UPDATE

type Msg
  = None
  | Reset
  | ToggleMode
  | Click Cell
  | Generate Bombs


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    None ->
      (model, Cmd.none)
    Reset -> 
      init
    ToggleMode ->
      let
        mode = case model.mode of 
          Marking -> Normal
          Normal -> Marking
      in
        ({model | mode = mode}, Cmd.none)
    Click cell -> 
      case model.state of
        GameOver -> 
          (model, Cmd.none)
        Playing -> 
          case model.mode of
            Marking -> (toggleMarkingAt cell.position model, Cmd.none)
            Normal -> 
              if cell.marked then
                (model, Cmd.none)
              else
                let
                  gameover = List.any (\b -> b == cell.position) model.bombs
                  modelUpdated = floodFill cell.position False True 0 model
                  isWin = List.all (\p -> p.hidden || List.member p.position modelUpdated.bombs) modelUpdated.cells 
                in
                  if gameover then 
                    ({model | state = GameOver} , Cmd.none) 
                  else if isWin then
                    ({modelUpdated | state = GameOver} , Cmd.none)
                  else
                    (modelUpdated, Cmd.none)
    Generate bombs ->
      let
        bombsUniq = (unique bombs)
        cells = applyBombs bombsUniq model.cells
      in
        ({ model | bombs = bombsUniq, cells = cells }, Cmd.none)

applyBombs : Bombs -> Cells -> Cells
applyBombs bombs cells =
  List.map (\cell -> { cell | danger = calcDanger cell.position bombs}) cells


toggleMarkingAt : Position -> Model -> Model
toggleMarkingAt position model =
  let
    cells = List.map 
      (\pos -> 
        if pos.position == position then
          { pos | marked = not pos.marked } 
        else 
          pos )
      model.cells
  in
    { model | cells = cells }


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW

view : Model -> Html Msg
view model =
  let
    mode = case model.mode of 
      Marking -> "Switch to Normal Mode"
      Normal -> "Switch to Marking Mode" 
    grid = renderGrid model.cells
    bombs =   
      case model.state of 
        Playing -> []
        GameOver -> renderBombs model.bombs
    danger = renderDangerMap model.cells
    blocks = renderBlocks model.cells
    board = 
      grid 
      ++ danger
      ++ blocks
      ++ bombs
  in
    Html.div [] 
      [ Html.div [] [ svg [ viewBox "0 0 100 100", width "300px", height "300px" ] board ]
      , Html.div [] [ Html.button [ onClick Reset ] [ text "New Game"] ]
      , Html.div [] [ Html.button [ onClick ToggleMode ] [ text mode] ]
      ]
   
    

boardPositions : Int -> Int -> List Position
boardPositions w h =
  let
    width = w - 1
    height = h - 1
    listW = List.range 0 width
    listH = List.range 0 height
    positions = cartesian listW listH
  in
    positions

cartesian : List a -> List b -> List (a,b)
cartesian xs ys =
  List.concatMap
    ( \x -> List.map ( \y -> (x, y) ) ys )
    xs

calcDanger : Position -> Bombs -> Int
calcDanger (x, y) bombs =
  let
      fn = \a b -> if inRange 1 a (x, y) then b + 1 else b
  in
   List.foldl fn 0 bombs

pixelCell : Position -> (String, String)
pixelCell (xPosition, yPosition) =
  let
    xPos = toString (xPosition * 10)
    yPos = toString (yPosition * 10)      
  in
    (xPos, yPos)

pixelCellText : Position -> (String, String)
pixelCellText (xPosition, yPosition) =
  let
    xPos = toString (xPosition * 10)
    yPos = toString (yPosition * 10 + 10)      
  in
    (xPos, yPos)

inRange : Int -> Position -> Position -> Bool
inRange distance (x1, y1) (x2, y2) =
  let 
    diffX = abs <| x1 - x2
    diffY = abs <| y1 - y2  
  in
    diffX <= distance && diffY <= distance

renderDangerMap : Cells -> List (Svg Msg)
renderDangerMap cells =
    cells
      |> List.filter (\pos -> pos.hidden && pos.danger > 0)
      |> List.map renderDanger

renderDanger : Cell -> (Svg Msg)
renderDanger cell =
  let
    (xPos, yPos) = pixelCellText cell.position
    dangerText = toString cell.danger
  in
    text_ [x xPos, y yPos, width "10", height "10", fontSize "10px", fontFamily "Verdana"] [ text dangerText ]

renderGrid : Cells -> List (Svg Msg)
renderGrid cells =
    List.map renderSquare cells

renderSquare : Cell -> (Svg Msg)
renderSquare cell =
  let
    (xPos, yPos) = pixelCell cell.position  
  in
    rect [x xPos, y yPos, width "10", height "10", fillOpacity "0", stroke "lightgray", strokeWidth "0.5"] []

renderBlocks : Cells -> List (Svg Msg)
renderBlocks cells =
  cells
    |> List.filter (\cell -> cell.hidden == False)
    |> List.map renderBlock

renderBlock : Cell -> (Svg Msg)
renderBlock cell =
  let
    (xPos, yPos) = pixelCell cell.position 
    color = if cell.marked then "orange" else "lightgray"
  in
    rect [x xPos, y yPos, width "10", height "10", fill color, fillOpacity "1", stroke "gray", strokeWidth "0.5", Svg.Events.onClick (Click cell) ] [ ]
          

renderBombs : List Position -> List (Svg Msg)
renderBombs bombs =
  List.map renderBomb bombs

renderBomb : Position -> Svg Msg 
renderBomb position =
  let 
    (xPos, yPos) = pixelCell position      
  in
    rect [x xPos, y yPos, rx "5", ry "5", width "10", height "10", fill "red"] []



-- HELPER

 
floodFill : (Int, Int) -> Bool -> Bool -> Int -> Model -> Model
floodFill (x,y) old new danger model =
  let
    cellM = List.Extra.find (\pos -> pos.position == (x, y)) model.cells
  in
    case cellM of
      Just cell -> 
        if cell.hidden == old && danger == 0 && old /= new then
          let
            newDanger = cell.danger
            model_ = setHiddenAt (x,y) new model
            modelE = floodFill (x+1,y) old new newDanger model_
            modelW = floodFill (x-1,y) old new newDanger modelE
            modelS = floodFill (x,y+1) old new newDanger modelW
            modelN = floodFill (x,y-1) old new newDanger modelS
            modelNE = floodFill (x+1,y-1) old new newDanger modelN
            modelNW = floodFill (x-1,y-1) old new newDanger modelNE
            modelSE = floodFill (x+1,y+1) old new newDanger modelNW
            modelSW = floodFill (x-1,y+1) old new newDanger modelSE
          in
            modelSW
        else 
          model
      Nothing ->
        model

setHiddenAt : Position -> Bool -> Model -> Model
setHiddenAt position hidden model =
  let
    cells = List.map 
      (\pos -> 
        if pos.position == position then
          { pos | hidden = hidden } 
        else 
          pos )
      model.cells
  in
    { model | cells = cells }
