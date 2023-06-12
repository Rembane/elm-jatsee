module Main exposing (..)

import Array exposing (Array)
import Browser
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)
import Random


type Die
    = Hidden
    | Showing Int
    | Locked Int


type alias Model =
    { dice : Array Die }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Array.repeat 5 Hidden), Cmd.none )


main =
    Browser.element
        { init = init
        , update = update
        , view = view >> toUnstyled
        , subscriptions = subscriptions
        }


type Msg
    = PushRollButton
    | Roll (List ( Int, Int ))
    | LockDie Int
    | UnlockDie Int
    | NewFace Int
    | NewGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Randomness makes everything messy.
        PushRollButton ->
            let
                rollDiceArray =
                    Array.indexedMap (\i d -> ( i, d )) model.dice
                        |> Array.filter
                            (\( _, d ) ->
                                case d of
                                    Hidden ->
                                        True

                                    Showing _ ->
                                        True

                                    _ ->
                                        False
                            )
                        |> Array.map (\( i, _ ) -> i)
            in
            ( model, Random.generate Roll (Random.map (List.map2 (\i n -> ( i, n )) (Array.toList rollDiceArray)) (Random.list (Array.length rollDiceArray) (Random.int 1 6))) )

        Roll newValues ->
            ( { model | dice = List.foldr (\( i, v ) -> Array.set i (Showing v)) model.dice newValues }, Cmd.none )

        LockDie idx ->
            Maybe.withDefault
                ( model, Cmd.none )
                (Maybe.map
                    (\d -> ( { model | dice = Array.set idx (lock d) model.dice }, Cmd.none ))
                    (Array.get idx model.dice)
                )

        UnlockDie idx ->
            Maybe.withDefault
                ( model, Cmd.none )
                (Maybe.map
                    (\d -> ( { model | dice = Array.set idx (lock d) model.dice }, Cmd.none ))
                    (Array.get idx model.dice)
                )

        _ ->
            ( model, Cmd.none )


lock : Die -> Die
lock d =
    case d of
        Hidden ->
            Hidden

        Showing i ->
            Locked i

        Locked i ->
            Locked i

unlock : Die -> Die
unlock d =
    case d of
        Hidden ->
            Hidden

        Showing i ->
            Showing i

        Locked i ->
            Showing i

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div [ css [ margin auto, maxWidth (px 720.0), paddingTop (px 25.0), position relative ] ]
        [ h1 [ css [ textAlign center ] ] [ text "Yatzy" ]
        , div [] [ button [ css [ position absolute, top (px 25.0), right (px 0) ] ] [ text "New game!" ] ]
        , div [ css [ textAlign center ] ] (Array.toList (Array.indexedMap viewDie model.dice))
        , div [] [ button [ css [ fontSize (px 36.0), marginTop (px 25.0), width (pct 100) ], onClick PushRollButton ] [ text "Roll!" ] ]
        ]


viewDie : Int -> Die -> Html Msg
viewDie idx d =
    let
        buttonStyle =
            css [ fontSize (px 75.0), marginRight (px 25.0) ]

        lockSign = " 🔒"

        toGlyph value =
            String.fromChar (Char.fromCode (0x267F + value))
    in
    case d of
        Hidden ->
            button [ buttonStyle ] [ text "□" ]

        Showing value ->
            button [ buttonStyle, onClick (LockDie idx) ] [ text (toGlyph value) ]

        Locked value ->
            button [ buttonStyle, onClick (UnlockDie idx) ] [ text (String.append (toGlyph value) lockSign) ]
