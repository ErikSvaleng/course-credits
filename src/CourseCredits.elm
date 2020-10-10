module CourseCredits exposing (Credit, CreditSubject(..), Credits, Index, addCredit, anyMissingSubject, empty, getSelectedSubjects, map2List, remove, updateHours, updateSubject)

import Array exposing (Array)
import Array.Extra as Array
import Subjects exposing (Subject, Subjects)


type CreditSubject
    = NotSelected
    | Selected Subject


type alias Credit =
    { subject : CreditSubject
    , hours : Float
    }


type alias Credits =
    Array Credit


type alias Index =
    Int


anyMissingSubject : Credits -> Bool
anyMissingSubject credits =
    credits
        |> Array.filter
            (\credit ->
                case credit.subject of
                    Selected _ ->
                        False

                    NotSelected ->
                        True
            )
        |> Array.length
        |> (<) 0


updateHours : Index -> Float -> Credits -> Credits
updateHours index hours credits =
    Array.update index (updateCreditHours hours) credits


remove : Index -> Credits -> Credits
remove index credits =
    Array.removeAt index credits


updateCreditHours : Float -> Credit -> Credit
updateCreditHours hours subj =
    { subj | hours = hours }


updateSubject : Index -> Subject -> Credits -> Credits
updateSubject index subject credits =
    Array.update index (updateCreditSubject subject) credits


updateCreditSubject : Subject -> Credit -> Credit
updateCreditSubject subject credit =
    { credit | subject = Selected subject }


addCredit : Credit -> Credits -> Credits
addCredit credit credits =
    Array.push credit credits


empty : Credits
empty =
    [ Credit NotSelected 0 ] |> Array.fromList


getSelectedSubjects : Credits -> Subjects
getSelectedSubjects credits =
    credits
        |> Array.toIndexedList
        |> List.filterMap getSubject


getSubject : ( Index, Credit ) -> Maybe Subject
getSubject ( _, credit ) =
    case credit.subject of
        Selected subject ->
            Just subject

        NotSelected ->
            Nothing


map2List : (Index -> Credit -> b) -> Credits -> List b
map2List mapFn credits =
    credits
        |> Array.indexedMap mapFn
        |> Array.toList
