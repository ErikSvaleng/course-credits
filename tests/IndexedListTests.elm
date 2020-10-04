module IndexedListTests exposing (suite)

import Expect
import IndexedList exposing (Index, IndexedList)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Indexed list tests"
        [ test "singleton creates an indexedList of length 1" <|
            \_ ->
                let
                    created =
                        IndexedList.singleton "Bill"
                in
                Expect.equal (IndexedList.length created) 1
        , test "update substitutes the entry at the given index" <|
            \_ ->
                let
                    index =
                        2

                    newValue =
                        "Bill"

                    updatedList =
                        IndexedList.update (\_ -> newValue) index exampleIndexedList
                in
                Expect.equal (IndexedList.get index updatedList) (Just newValue)
        , test "Running update with an invalid index does nothing" <|
            \_ ->
                let
                    invalidIndex =
                        IndexedList.length exampleIndexedList |> (+) 5

                    updatedList =
                        IndexedList.update (\_ -> "FAIL!") invalidIndex exampleIndexedList
                in
                Expect.equal updatedList exampleIndexedList
        , test "when removing an item, indexes are updated" <|
            \_ ->
                let
                    removed =
                        IndexedList.remove 1 exampleIndexedList
                in
                Expect.equalLists [ 0, 1, 2 ] (getIndexes removed)
        , test "Removing an invalid index does nothing" <|
            \_ ->
                let
                    invalidIndex =
                        IndexedList.length exampleIndexedList |> (+) 5

                    removed =
                        IndexedList.remove invalidIndex exampleIndexedList
                in
                Expect.equal (getIndexes exampleIndexedList) (getIndexes removed)
        , test "An added item is given an index of the currently highest index + 1" <|
            \_ ->
                let
                    addedIndexes =
                        IndexedList.add "Bill" exampleIndexedList |> getIndexes
                in
                Expect.equalLists addedIndexes [ 0, 1, 2, 3, 4 ]
        , test "Given an empty indexedlist, when adding a new item, it is given index 0" <|
            \_ ->
                let
                    indexes =
                        IndexedList.singleton "John"
                            |> IndexedList.remove 0
                            |> IndexedList.add "Bill"
                            |> getIndexes
                in
                Expect.equalLists indexes [ 0 ]
        ]


exampleIndexedList : IndexedList String
exampleIndexedList =
    let
        exampleStrings =
            [ "Dean"
            , "James"
            , "Roger"
            ]

        list =
            IndexedList.singleton "John"
    in
    List.foldl IndexedList.add list exampleStrings


getIndexes : IndexedList a -> List Index
getIndexes list =
    IndexedList.map2List (\idx _ -> idx) list
