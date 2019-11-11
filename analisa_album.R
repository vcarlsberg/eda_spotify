library(spotifyr)
library(tidyverse)
library(data.table)
library(genius)
library(tm)
library(cowplot)
library(dplyr)
library(rlang)
library(tidyselect)

Sys.setenv(SPOTIFY_CLIENT_ID = 'c97953505980495e8fef75f234f6e82f')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '665799370c6044e4bf5eb2a74cb25f7f')

access_token <- get_spotify_access_token(client_id = Sys.getenv('SPOTIFY_CLIENT_ID'), client_secret = Sys.getenv('SPOTIFY_CLIENT_SECRET'))


playlists_kopikustik <- get_playlist_tracks("37i9dQZF1DWTDjtvOmdzrF")

xxx<-playlists_kopikustik[[9]][[30]][[3]]

dim(playlists_kopikustik)[1]

for (artist in xxx){
  xxxaaa<-tryCatch(
    {
      
      get_artist<-get_artist_audio_features(artist)
    },
    error = function(e){
      get_artist<-data.frame()
    }
  )
}




