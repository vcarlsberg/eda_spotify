library(spotifyr)
library(tidyverse)
library(data.table)
library(genius)
library(tm)


access_token <- get_spotify_access_token(client_id = Sys.getenv('SPOTIFY_CLIENT_ID'), client_secret = Sys.getenv('SPOTIFY_CLIENT_SECRET'))


af_DK <- get_artist_audio_features('Didi Kempot')
af_VV <- get_artist_audio_features('Via Vallen')
af_BSB <- get_artist_audio_features('Backstreet Boys')
af_BTS <- get_artist_audio_features('BTS')
af<-rbind(af_DK,af_VV,af_BSB,af_BTS)

ggplot()+
  geom_boxplot(data = af, 
             mapping = aes(x=artist_name,y = tempo,color = as.factor(artist_name)), 
             )

ggplot()+
  geom_boxplot(data = af, 
               mapping = aes(x=artist_name,y = danceability,color = as.factor(artist_name)), 
  )

af_filter=filter(af,artist_name=="Via Vallen")
ggplot()+
  geom_histogram(
    bins=10,
    data = af_filter, 
    mapping = aes(danceability)
  )



lyrics<-genius_lyrics(artist = "Didi Kempot", song = "Pamer Bojo", info = "title")
#spotify:playlist:37i9dQZF1DX7qK8ma5wgG1
#playlists <- get_user_playlists('Sad Songs')
playlists <- get_playlist_tracks("37i9dQZF1DWX27wTtU5ZMz")
uri<-as.data.frame(playlists$track.id) 
#spotify:playlist:37i9dQZF1DX9U5XaCM7ssr
#spotify:playlist:7vBShQfXqgKHshNTD9JnWQ
#spotify:playlist:37i9dQZF1DWX27wTtU5ZMz menikahimu

track<-get_track_audio_features(uri[1:dim(uri)[1],])
playlists<-cbind(playlists,track)
summary(track)

ggplot()+
  geom_histogram(
    bins=10,
    data = track, 
    mapping = aes(tempo)
  )


library(mlbench)
library(caret)
control<-trainControl(method="repeatedcv",number=10, repeats=3)
