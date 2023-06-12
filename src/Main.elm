module Main exposing (..)

import Array exposing (Array)
import Browser
import Css exposing (..)
import Dict exposing (Dict)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (colspan, css, disabled, href, src)
import Html.Styled.Events exposing (onClick)
import Random



-- All rules fetched from https://en.wikipedia.org/wiki/Yahtzee


type DieState
    = Showing
    | Locked


type Die
    = Die DieState Int


getValue : Die -> Int
getValue (Die _ i) =
    i


type alias Scoreboard =
    { aces : Maybe Int
    , twos : Maybe Int
    , threes : Maybe Int
    , fours : Maybe Int
    , fives : Maybe Int
    , sixes : Maybe Int
    , threeofakind : Maybe Int
    , fourofakind : Maybe Int
    , fullhouse : Maybe Int
    , smallstraight : Maybe Int
    , largestraight : Maybe Int
    , yahtzee : Maybe Int
    , chance : Maybe Int
    }


type ScoreType
    = Aces
    | Twos
    | Threes
    | Fours
    | Fives
    | Sixes
    | ThreeOfAKind
    | FourOfAKind
    | FullHouse
    | SmallStraight
    | LargeStraight
    | Yahtzee
    | Chance


calculateTotal : Scoreboard -> Int
calculateTotal s =
    Maybe.withDefault 0 s.aces
        + Maybe.withDefault 0 s.twos
        + Maybe.withDefault 0 s.threes
        + Maybe.withDefault 0 s.fours
        + Maybe.withDefault 0 s.fives
        + Maybe.withDefault 0 s.sixes
        + Maybe.withDefault 0 s.threeofakind
        + Maybe.withDefault 0 s.fourofakind
        + Maybe.withDefault 0 s.fullhouse
        + Maybe.withDefault 0 s.smallstraight
        + Maybe.withDefault 0 s.largestraight
        + Maybe.withDefault 0 s.yahtzee
        + Maybe.withDefault 0 s.chance


saveScore : ScoreType -> Int -> Scoreboard -> Scoreboard
saveScore st v board =
    case st of
        Aces ->
            { board | aces = Just v }

        Twos ->
            { board | twos = Just v }

        Threes ->
            { board | threes = Just v }

        Fours ->
            { board | fours = Just v }

        Fives ->
            { board | fives = Just v }

        Sixes ->
            { board | sixes = Just v }

        ThreeOfAKind ->
            { board | threeofakind = Just v }

        FourOfAKind ->
            { board | fourofakind = Just v }

        FullHouse ->
            { board | fullhouse = Just v }

        SmallStraight ->
            { board | smallstraight = Just v }

        LargeStraight ->
            { board | largestraight = Just v }

        Yahtzee ->
            { board | yahtzee = Just v }

        Chance ->
            { board | chance = Just v }


scoreThis : Array Die -> ScoreType -> Int
scoreThis dice scoreType =
    let
        values =
            Array.map getValue dice
                |> Array.foldr (\v -> Dict.update v (Just << (\i -> 1 + i) << Maybe.withDefault 0))
                    Dict.empty

        mirrorValues =
            List.foldr (\( k, v ) -> Dict.update k (Just << (\vs -> v :: vs) << Maybe.withDefault [ v ])) Dict.empty <| List.map (\( a, b ) -> ( b, a )) <| Dict.toList values

        getAndScore value =
            value * Maybe.withDefault 0 (Dict.get value values)

        maximum =
            List.foldr max 0

        -- Get the largest possible score available when a minimum number of a
        -- certain value is needed.
        getANumberOf value =
            List.range value 6
                |> List.map (\v -> v * maximum (Maybe.withDefault [ 0 ] (Dict.get v mirrorValues)))
                |> maximum

        straights range =
            let
                predicate =
                    List.all identity (List.map2 (\a ( k, v ) -> a == k && v == 1) range (List.sort <| Dict.toList values))
            in
            if predicate then
                30

            else
                0
    in
    case scoreType of
        Aces ->
            getAndScore 1

        Twos ->
            getAndScore 2

        Threes ->
            getAndScore 3

        Fours ->
            getAndScore 4

        Fives ->
            getAndScore 5

        Sixes ->
            getAndScore 6

        ThreeOfAKind ->
            getANumberOf 3

        FourOfAKind ->
            getANumberOf 4

        FullHouse ->
            Maybe.withDefault
                0
                (Maybe.map2
                    (\a b -> 2 * maximum a + 3 * maximum b)
                    (Dict.get 2 mirrorValues)
                    (Dict.get 3 mirrorValues)
                )

        SmallStraight ->
            straights (List.range 1 5)

        LargeStraight ->
            straights (List.range 2 6)

        Yahtzee ->
            getANumberOf 5

        Chance ->
            List.sum (List.map (\( k, v ) -> k * v) (Dict.toList values))


type alias Model =
    { dice : Array Die
    , playing : Bool
    , score : Scoreboard
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { dice = Array.repeat 5 (Die Showing 1)
      , playing = False
      , score =
            { aces = Nothing
            , twos = Nothing
            , threes = Nothing
            , fours = Nothing
            , fives = Nothing
            , sixes = Nothing
            , threeofakind = Nothing
            , fourofakind = Nothing
            , fullhouse = Nothing
            , smallstraight = Nothing
            , largestraight = Nothing
            , yahtzee = Nothing
            , chance = Nothing
            }
      }
    , Cmd.none
    )


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
    | SaveScore ScoreType


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Randomness makes everything messy.
        PushRollButton ->
            let
                rollDiceArray =
                    Array.indexedMap (\i d -> ( i, d )) model.dice
                        |> Array.filter (\( _, Die state _ ) -> state == Showing)
                        |> Array.map (\( i, _ ) -> i)
            in
            ( model, Random.generate Roll (Random.map (List.map2 (\i n -> ( i, n )) (Array.toList rollDiceArray)) (Random.list (Array.length rollDiceArray) (Random.int 1 6))) )

        Roll newValues ->
            ( { model | dice = List.foldr (\( i, v ) -> Array.set i (Die Showing v)) model.dice newValues, playing = True }, Cmd.none )

        LockDie idx ->
            Maybe.withDefault
                ( model, Cmd.none )
                (Maybe.map
                    (\(Die _ i) -> ( { model | dice = Array.set idx (Die Locked i) model.dice }, Cmd.none ))
                    (Array.get idx model.dice)
                )

        UnlockDie idx ->
            Maybe.withDefault
                ( model, Cmd.none )
                (Maybe.map
                    (\(Die _ i) -> ( { model | dice = Array.set idx (Die Showing i) model.dice }, Cmd.none ))
                    (Array.get idx model.dice)
                )

        SaveScore st ->
            ( { model | score = saveScore st (scoreThis model.dice st) model.score }, Cmd.none )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


buttonStyle =
    css [ fontSize (px 75.0), marginRight (px 25.0) ]


view : Model -> Html Msg
view model =
    let
        scorePadding = css [paddingLeft (px 10), paddingRight (px 10)]

        renderScoreLine st label field =
            tr [css [borderBottom3 (px 1) solid (rgb 0 0 0)]]
                [ td [scorePadding]
                    [ button
                        [ onClick (SaveScore st)
                        , Html.Styled.Attributes.disabled
                            (case field model.score of
                                Nothing ->
                                    False

                                Just _ ->
                                    True
                            )
                        ]
                        [ text "Score me!" ]
                    ]
                , td [scorePadding] [ text label ]
                , td [scorePadding, css [textAlign right]]
                    [ text
                        (case field model.score of
                            Nothing ->
                                ""

                            Just i ->
                                String.fromInt i
                        )
                    ]
                ]
    in
    div [ css [ margin auto, maxWidth (px 720.0), paddingTop (px 25.0), position relative ] ]
        [ h1 [ css [ textAlign center ] ] [ text "Yatzy" ]
        , div [] [ button [ css [ position absolute, top (px 25.0), right (px 0) ] ] [ text "New game!" ] ]
        , Html.Styled.table [ css [ borderCollapse collapse, border3 (px 1) solid (rgb 0 0 0), marginBottom (Css.em 3) ] ]
            [ renderScoreLine Aces "Aces" .aces
            , renderScoreLine Twos "Twos" .twos
            , renderScoreLine Threes "Threes" .threes
            , renderScoreLine Fours "Fours" .fours
            , renderScoreLine Fives "Fives" .fives
            , renderScoreLine Sixes "Sixes" .sixes
            , renderScoreLine ThreeOfAKind "Three Of A Kind" .threeofakind
            , renderScoreLine FourOfAKind "Four Of A Kind" .fourofakind
            , renderScoreLine FullHouse "Full House" .fullhouse
            , renderScoreLine SmallStraight "Small Straight" .smallstraight
            , renderScoreLine LargeStraight "Large Straight" .largestraight
            , renderScoreLine Yahtzee "Yahtzee" .yahtzee
            , renderScoreLine Chance "Chance" .chance
            , tr []
                [ td [ colspan 2 ] []
                , td []
                    [ strong [] [ text "Total: " ]
                    , text <| String.fromInt <| calculateTotal model.score
                    ]
                ]
            ]
        , div [ css [ textAlign center ] ]
            (if model.playing then
                Array.toList (Array.indexedMap viewDie model.dice)

             else
                List.repeat 5 (button [ buttonStyle ] [ text "□" ])
            )
        , div [] [ button [ css [ fontSize (px 36.0), marginTop (px 25.0), width (pct 100) ], onClick PushRollButton ] [ text "Roll!" ] ]
        ]


viewDie : Int -> Die -> Html Msg
viewDie idx (Die state dieFace) =
    let
        lockSign =
            " 🔒"

        toGlyph value =
            String.fromChar (Char.fromCode (0x267F + value))
    in
    case state of
        Showing ->
            button [ buttonStyle, onClick (LockDie idx) ] [ text (toGlyph dieFace) ]

        Locked ->
            button [ buttonStyle, onClick (UnlockDie idx) ] [ text (String.append (toGlyph dieFace) lockSign) ]
