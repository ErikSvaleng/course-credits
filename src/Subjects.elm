module Subjects exposing (Subject, SubjectId, Subjects, decode, diff, getById)

import Json.Decode exposing (Decoder, field, int, list, map2, string)
import Set


type alias SubjectId =
    Int


type alias Subject =
    { id : SubjectId
    , name : String
    }


type alias Subjects =
    List Subject


diff : Subjects -> Subjects -> Subjects
diff subjectsToRemove allSubjects =
    let
        removeSet =
            subjectsToRemove |> getIds |> Set.fromList

        allSet =
            allSubjects |> getIds |> Set.fromList

        idsToKeep =
            Set.diff allSet removeSet
    in
    List.filter (\subj -> Set.member subj.id idsToKeep) allSubjects


getIds : Subjects -> List SubjectId
getIds subjects =
    subjects |> List.map .id


decode : Decoder Subjects
decode =
    list decodeOne


decodeOne : Decoder Subject
decodeOne =
    map2 Subject
        (field "id" int)
        (field "name" string)


getById : Subjects -> SubjectId -> Maybe Subject
getById subjects id =
    let
        matches =
            List.filter (\subj -> subj.id == id) subjects
    in
    case matches of
        [] ->
            Nothing

        first :: _ ->
            Just first
