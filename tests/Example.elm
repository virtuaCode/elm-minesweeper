module Example exposing (..)

import Main exposing (calcDanger, Bombs, Coord)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "The Main module"
        [ describe "Main.calcDanger" -- Nest as many descriptions as you like.
            [ test "1 bomb" <|
                \_ ->
                    let
                        coord =
                            (0, 0)
                        bombs = 
                            [(0, 0)]
                    in
                        Expect.equal 1 (Main.calcDanger coord bombs)
            
            , test "2 bombs" <|
                \_ ->
                    let
                        coord =
                            (0, 0)
                        bombs = 
                            [(0, 0), (0, 1)]
                    in
                        Expect.equal 2 (Main.calcDanger coord bombs)
            
            
            , test "8 bombs" <|
                \_ ->
                    let
                        coord =
                            (1, 1)
                        bombs = 
                            [(0, 0), (0, 1), (0, 2), (1, 0), (1, 2), (2, 0), (2, 1), (2, 2)]
                    in
                        Expect.equal 8 (Main.calcDanger coord bombs)
            
            
            ]
        ]