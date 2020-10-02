module IndexedList exposing (Index, IndexedList, add, filter2List, get, length, map2List, remove, singleton, toList, update)

import Dict exposing (Dict)


type IndexedList a
    = IndexedList (Dict Index a)


type alias Index =
    Int


lowestIndex : Index
lowestIndex =
    0


length : IndexedList a -> Int
length list =
    list |> getDict |> Dict.size


get : Index -> IndexedList a -> Maybe a
get idx list =
    list |> getDict |> Dict.get idx


add : a -> IndexedList a -> IndexedList a
add thing list =
    let
        dict =
            getDict list

        idx =
            nextIndex dict
    in
    Dict.insert idx thing dict |> IndexedList


nextIndex : Dict Index a -> Index
nextIndex dict =
    dict
        |> Dict.keys
        |> List.maximum
        |> Maybe.withDefault lowestIndex
        |> (+) 1


update : (a -> a) -> Index -> IndexedList a -> IndexedList a
update updatefn index list =
    let
        dict =
            getDict list

        updater =
            Maybe.map updatefn
    in
    Dict.update index updater dict |> IndexedList


singleton : a -> IndexedList a
singleton item =
    Dict.singleton 0 item |> IndexedList


toList : IndexedList a -> List ( Index, a )
toList indexedList =
    indexedList |> getDict |> Dict.toList


map2List : (Index -> a -> b) -> IndexedList a -> List b
map2List mapfn list =
    let
        dict =
            getDict list
    in
    Dict.map mapfn dict |> Dict.values


filter2List : (Index -> a -> Bool) -> IndexedList a -> List a
filter2List filterFn list =
    let
        dict =
            getDict list
    in
    Dict.filter filterFn dict |> Dict.values


remove : Index -> IndexedList a -> IndexedList a
remove index list =
    let
        dict =
            getDict list
    in
    Dict.remove index dict |> IndexedList |> updateIndexes


updateIndexes : IndexedList a -> IndexedList a
updateIndexes list =
    let
        dict =
            getDict list

        asList =
            Dict.toList dict
    in
    List.indexedMap updateIndex asList
        |> Dict.fromList
        |> IndexedList


updateIndex : Index -> ( Index, a ) -> ( Index, a )
updateIndex actualIdx ( _, thing ) =
    ( actualIdx, thing )


getDict : IndexedList a -> Dict Index a
getDict list =
    case list of
        IndexedList dict ->
            dict
