module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- MAIN


main : Program Frags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { tree : Tree
    , visiblePaths : List String
    }


type alias Entry =
    { id : Int
    , name : String
    , segments : List String
    , children : List String
    , depth : Int
    }


type alias Tree =
    List Entry


type alias Frags =
    { files : List String
    }


init : Frags -> ( Model, Cmd Msg )
init frags =
    let
        tree =
            initTree frags.files

        visiblePaths =
            tree
                |> List.filter (\entry -> not <| List.isEmpty entry.children)
                |> List.map .name
    in
    ( { tree = tree
      , visiblePaths = visiblePaths
      }
    , Cmd.none
    )


initTree : List String -> Tree
initTree files =
    let
        separatePaths path =
            String.split "/" path

        foldEntries path tree =
            let
                segments =
                    separatePaths path

                getDepth name =
                    name |> (separatePaths >> List.length)

                newEntry =
                    { id = 0
                    , name = path
                    , segments = segments
                    , children = []
                    , depth = getDepth path
                    }

                maybeEntry =
                    tree
                        |> indexWhere
                            (\{ name } ->
                                String.startsWith name path
                                    && getDepth name
                                    == getDepth path
                                    - 1
                            )

                indexedTree =
                    Array.fromList tree
            in
            case maybeEntry of
                Just ( index, knownPath ) ->
                    let
                        newParent =
                            { knownPath
                                | children = path :: knownPath.children
                            }

                        updatedTree =
                            indexedTree
                                |> Array.set index newParent
                                |> Array.toList
                    in
                    newEntry :: updatedTree

                Nothing ->
                    newEntry :: tree
    in
    files
        |> List.sortBy (separatePaths >> List.length)
        |> List.foldl foldEntries []



-- UPDATE


type Msg
    = NoOp
    | ChangeVisibility Entry


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ChangeVisibility entry ->
            let
                isVisible =
                    List.member entry.name model.visiblePaths

                newVisiblePaths =
                    if isVisible then
                        model.visiblePaths
                            |> List.filter (\path -> not (List.member path model.visiblePaths))

                    else
                        model.visiblePaths
            in
            ( { model
                | visiblePaths = newVisiblePaths
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    viewTreeView model


viewTreeView : Model -> Html Msg
viewTreeView model =
    let
        isExpanded entry =
            List.member entry.name model.visiblePaths
    in
    table
        [ class "table is-fullwidth is-hoverable" ]
        [ tbody
            []
            (model.tree
                |> List.map (\entry -> viewCells (isExpanded entry) entry)
            )
        ]


viewCells : Bool -> Entry -> Html Msg
viewCells visible entry =
    let
        fontClass =
            if List.isEmpty entry.children then
                ""

            else if visible then
                "fa-angle-right"

            else
                "fa-angle-down"

        tableCell =
            td
                [ attribute "data-children" <| String.join "," entry.children ]
                [ a
                    [ class "link"
                    , onClick (ChangeVisibility entry)
                    ]
                    [ span
                        [ class "icon" ]
                        [ i [ class <| "fas " ++ fontClass ] []
                        ]
                    , span
                        []
                        [ text entry.name ]
                    ]
                ]
    in
    tr [ hidden <| not visible ] [ tableCell ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UTILITIES


indicesWhere : (a -> Bool) -> List a -> List ( Int, a )
indicesWhere pred list =
    List.indexedMap Tuple.pair list
        |> List.filter (\( _, val ) -> pred val)


indexWhere : (a -> Bool) -> List a -> Maybe ( Int, a )
indexWhere pred list =
    indicesWhere pred list
        |> List.head
