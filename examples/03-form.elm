module Main exposing (Model, Msg(..), init, main, update, view, viewInput, viewValidation)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { name : String
    , age : String
    , password : String
    , passwordAgain : String
    , submitted : Bool
    }


init : Model
init =
    Model "" "" "" "" False



-- UPDATE


type Msg
    = Name String
    | Age String
    | Password String
    | PasswordAgain String
    | Submit


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Age age ->
            { model | age = age }

        Password password ->
            { model | password = password }

        PasswordAgain password ->
            { model | passwordAgain = password }

        Submit ->
            { model | submitted = True }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Name" model.name Name
        , viewInput "text" "Age" model.age Age
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
        , button [ onClick Submit ] [ text "Submit" ]
        , viewValidation model
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
    if not model.submitted then
        div [ style "color" "green" ] [ text "Ok!" ]

    else if String.toInt model.age == Nothing then
        div [ style "color" "red" ] [ text "The age must be a number!" ]

    else if String.length model.password <= 8 then
        div [ style "color" "red" ] [ text "Passwords must have more than 8 characters!" ]

    else if not <| passwordHasAllRequiredCharacterTypes model.password then
        div [ style "color" "red" ] [ text "The password musth have at least one uppercase, lowercase and number!" ]

    else if model.password /= model.passwordAgain then
        div [ style "color" "red" ] [ text "Passwords do not match!" ]

    else
        div [ style "color" "green" ] [ text "Ok!" ]


passwordHasAllRequiredCharacterTypes : String -> Bool
passwordHasAllRequiredCharacterTypes password =
    stringHasAtLeastOneCharacterType password Char.isLower
        && stringHasAtLeastOneCharacterType password Char.isUpper
        && stringHasAtLeastOneCharacterType password Char.isDigit


stringHasAtLeastOneCharacterType : String -> (Char -> Bool) -> Bool
stringHasAtLeastOneCharacterType string validation =
    let
        list =
            String.toList string
    in
    List.any validation list
