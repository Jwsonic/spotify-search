module Main exposing (..)

import Html exposing (Html, div, input, button, text, section, h1, figure, img)
import Html.Attributes exposing (class, placeholder, src)
import Html.App exposing (program)
import Html.Events exposing (onClick, onInput)
import Http
import Platform.Cmd exposing (Cmd)
import Task
import Array
import Json.Decode exposing (..)


-- curl -X GET "https://api.spotify.com/v1/search?q=mr+brightside&type=track&market=US" -H "Accept: application/json"
-- Main app entry point


main : Program Never
main =
    program { init = init, update = update, subscriptions = (\_ -> Sub.none), view = view }



-- Spotify data types


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



-- The data model for our app


type alias Model =
    { query : String
    , tracks : SpotifyData
    }



-- All the Msgs our app responds to


type Msg
    = UpdateQuery String
    | Search
    | SearchResult SpotifyData


init : ( Model, Cmd Msg )
init =
    ( { query = "", tracks = NotAsked }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateQuery query ->
            ( { model | query = query }, Cmd.none )

        Search ->
            ( { model | tracks = Loading }, fetchSpotify model.query )

        SearchResult tracks ->
            ( { model | tracks = tracks }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "container is-fluid" ]
        [ h1 [ class "title" ] [ text "Spotify Search" ]
        , div [ class "columns" ]
            [ input [ class "input column is-4", placeholder "Search track title", onInput UpdateQuery ] []
            , button [ class "button is-primary", onClick Search ] [ text "Search" ]
            ]
        , spotifyView model
        ]



-- spotifyView handles rendering the potential states for our tracks


spotifyView : Model -> Html Msg
spotifyView model =
    case model.tracks of
        NotAsked ->
            -- We haven't searched yet, give a generic search message
            div [] [ text "Search for tracks!" ]

        Loading ->
            -- While the HTTP request is going on, let the user know something is happening
            div [] [ text ("Searching for " ++ model.query ++ "...") ]

        Failure error ->
            -- Ideally we would display a more informative error, but this works for now
            div [] [ text "There was an error..." ]

        Success tracks ->
            -- We've gotten n <= 20 tracks back, we'll display them in a grid
            if List.length tracks == 0 then
                div [] [ text ("No results for \"" ++ model.query ++ "\"!") ]
            else
                let
                    trackViews =
                        Array.fromList <| List.map trackView tracks
                in
                    -- This bit could be condensed with map, but for readibility it's manually laid out
                    div []
                        [ div [ class "columns" ]
                            (Array.toList <| Array.slice 0 5 trackViews)
                        , div [ class "columns" ]
                            (Array.toList <| Array.slice 5 10 trackViews)
                        , div [ class "columns" ]
                            (Array.toList <| Array.slice 10 15 trackViews)
                        , div [ class "columns" ]
                            (Array.toList <| Array.slice 15 20 trackViews)
                        ]



-- trackView handles rendering an individual track


trackView : SpotifyTrack -> Html Msg
trackView track =
    let
        -- We can't be 100% sure what the API call will return, so we need to provide some defaults
        imageUrl =
            case List.head track.album.images of
                Just url ->
                    url

                Nothing ->
                    "http://placehold.it/128x128"

        artist =
            case List.head track.artists of
                Just name ->
                    name

                Nothing ->
                    "Unknown"
    in
        div [ class "column" ]
            [ figure []
                [ img [ class "image is-128x128", src imageUrl ] []
                ]
            , div []
                [ div [ class "title is-5" ] [ text track.name ]
                , div [ class "subtitle is-6" ] [ text artist ]
                ]
            ]



-- decodeSpotifyJson is a JSONDecoder that takes a spotify API call and turns it into an Elm record


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



-- fetchSpotify builds the command to actually do the API call


fetchSpotify : String -> Cmd Msg
fetchSpotify query =
    let
        url =
            "https://api.spotify.com/v1/search?q=" ++ (Http.uriEncode query) ++ "&type=track&market=US"
    in
        Cmd.map SearchResult <| Task.perform Failure Success (Http.get decodeSpotifyJson url)
