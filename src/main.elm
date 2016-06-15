module Main exposing (..)

import Html exposing (Html, div, input, button, text)
import Html.Attributes exposing (class, placeholder)
import Html.App exposing (program)
import Html.Events exposing (onClick, onInput)
import Http
import Platform.Cmd exposing (Cmd)
import Task
import Json.Decode exposing (..)


-- curl -X GET "https://api.spotify.com/v1/search?q=mr+brightside&type=track&market=US" -H "Accept: application/json"
-- Main app entry point


main : Program Never
main =
    program { init = init, update = update, subscriptions = (\_ -> Sub.none), view = view }


type alias SpotifyAlbum =
    { name : String
    , images : List String
    }


type alias SpotifyTrack =
    { album : SpotifyAlbum
    , artists : List String
    , name : String
    }


type alias TrackList =
    List SpotifyTrack



-- RemoteData represents all possible states for data that is remotely fetched.
-- Original credit for pattern here: http://blog.jenkster.com/2016/06/how-elm-slays-a-ui-antipattern.html


type RemoteData e a
    = NotAsked
    | Loading
    | Failure e
    | Success a



-- SpotifyData is RemoteData containing a list of spotify track data


type alias SpotifyData =
    RemoteData Http.Error TrackList



-- All the Msgs our app responds to


type Msg
    = SpotifyData
    | Query String
    | Search



-- | SpotifyData
-- The data model for our app


type alias Model =
    { query : String
    , tracks : SpotifyData
    }


init : ( Model, Cmd Msg )
init =
    ( { query = "", tracks = NotAsked }, Cmd.none )



-- update : Msg -> Model -> ( Model, Cmd Msg )


update msg model =
    case msg of
        Query query ->
            ( { model | query = query }, fetchSpotify query )

        Search ->
            ( { model | tracks = Loading }, Cmd.none )



-- updateSpotify : SpotifyData -> Model -> ( Model, Cmd a )
-- updateSpotify data model =
--     ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ input [ class "input", placeholder "Search track title", onInput Query ] []
        , button [ class "button is-primary", onClick Search ] [ text "Search" ]
        , spotifyView model
        ]


spotifyView : Model -> Html Msg
spotifyView model =
    case model.tracks of
        NotAsked ->
            div [] [ text "Search for tracks!" ]

        Loading ->
            div [] [ text ("Searching for " ++ model.query ++ "...") ]

        Failure error ->
            div [] [ text "There was an error..." ]

        Success tracks ->
            div [] [ text "Lots of cool spotify tracks!" ]


decodeSpotifyJson : Decoder TrackList
decodeSpotifyJson =
    at [ "tracks", "items" ]
        (list
            (object3 SpotifyTrack
                ("album"
                    := (object2 SpotifyAlbum
                            ("name" := string)
                            ("images" := (list ("url" := string)))
                       )
                )
                ("artists" := (list ("name" := string)))
                ("name" := string)
            )
        )


fetchSpotify : String -> Cmd Msg
fetchSpotify query =
    let
        url =
            "https://api.spotify.com/v1/search?q=" ++ query ++ "&type=track&market=US"
    in
        Task.perform Failure Success (Http.get decodeSpotifyJson url)
