module CourseCredits exposing (Credit, CreditSubject(..), Credits, Subject, SubjectId, Subjects, addCredit, diffSubjects, empty, getSelectedSubjects, getSubjectById, updateHours, updateSubject)

import IndexedList exposing (Index, IndexedList)
import Set


type alias SubjectId =
    Int


type alias Subject =
    { id : SubjectId
    , name : String
    }


type CreditSubject
    = NotSelected
    | Selected Subject


type alias Subjects =
    List Subject


type alias Credit =
    { subject : CreditSubject
    , hours : Float
    }


type alias Credits =
    IndexedList Credit


updateHours : Index -> Float -> Credits -> Credits
updateHours index hours credits =
    IndexedList.update (updateCreditHours hours) index credits


updateCreditHours : Float -> Credit -> Credit
updateCreditHours hours subj =
    { subj | hours = hours }


updateSubject : Index -> Subject -> Credits -> Credits
updateSubject index subject credits =
    IndexedList.update (updateCreditSubject subject) index credits


updateCreditSubject : Subject -> Credit -> Credit
updateCreditSubject subject credit =
    { credit | subject = Selected subject }


addCredit : Credit -> Credits -> Credits
addCredit credit credits =
    IndexedList.add credit credits


empty : Credits
empty =
    Credit NotSelected 0 |> IndexedList.singleton


getSubjectById : Subjects -> SubjectId -> Maybe Subject
getSubjectById subjects id =
    let
        matches =
            List.filter (\subj -> subj.id == id) subjects
    in
    case matches of
        [] ->
            Nothing

        first :: _ ->
            Just first


getSelectedSubjects : Credits -> Subjects
getSelectedSubjects credits =
    credits
        |> IndexedList.toList
        |> List.filterMap getSubject


getSubject : ( Index, Credit ) -> Maybe Subject
getSubject ( _, credit ) =
    case credit.subject of
        Selected subject ->
            Just subject

        NotSelected ->
            Nothing


diffSubjects : Subjects -> Subjects -> Subjects
diffSubjects subjectsToRemove allSubjects =
    let
        removeSet =
            subjectsToRemove |> getSubjectIds |> Set.fromList

        allSet =
            allSubjects |> getSubjectIds |> Set.fromList

        idsToKeep =
            Set.diff allSet removeSet
    in
    List.filter (\subj -> Set.member subj.id idsToKeep) allSubjects


getSubjectIds : Subjects -> List SubjectId
getSubjectIds subjects =
    subjects |> List.map .id
