module ViewElements exposing (container, dangerButton, formGroupRow, mainHeading, paragraph, primaryButton)

import Html exposing (Html, button, div, h1, p, text)
import Html.Attributes exposing (class, disabled, type_)
import Html.Events exposing (onClick)


container : List (Html msg) -> Html msg
container content =
    div [ class "container pt-3" ] content


mainHeading : String -> Html msg
mainHeading string =
    h1 [] [ text string ]


paragraph : String -> Html msg
paragraph string =
    p [] [ text string ]


formGroupRow : List (Html msg) -> Html msg
formGroupRow content =
    div [ class "form-group row" ] content


primaryButton : String -> msg -> Bool -> Html msg
primaryButton =
    standardBtn "btn-outline-primary"


dangerButton : String -> msg -> Bool -> Html msg
dangerButton =
    standardBtn "btn-outline-danger"


standardBtn : String -> String -> msg -> Bool -> Html msg
standardBtn buttonType label onClickMsg isDisabled =
    button [ type_ "button", class ("btn " ++ buttonType), onClick onClickMsg, disabled isDisabled ] [ text label ]
