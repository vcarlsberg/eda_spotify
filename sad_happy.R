library(spotifyr)
library(tidyverse)
library(data.table)
library(genius)
library(tm)
library(cowplot)

Sys.setenv(SPOTIFY_CLIENT_ID = 'c97953505980495e8fef75f234f6e82f')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '665799370c6044e4bf5eb2a74cb25f7f')

access_token <- get_spotify_access_token(client_id = Sys.getenv('SPOTIFY_CLIENT_ID'), client_secret = Sys.getenv('SPOTIFY_CLIENT_SECRET'))

playlists_happy <- get_playlist_tracks("7vBShQfXqgKHshNTD9JnWQ")
playlists_sad <- get_playlist_tracks("37i9dQZF1DX7qK8ma5wgG1")

uri_happy<-as.data.frame(playlists_happy$track.id) 
uri_sad<-as.data.frame(playlists_sad$track.id) 

track_happy<-get_track_audio_features(uri_happy[1:dim(uri_happy)[1],])
track_sad<-get_track_audio_features(uri_sad[1:dim(uri_sad)[1],])

label_happy<-as.data.frame(rep("happy",times=dim(uri_happy)[1]))
names(label_happy)[1]<-"genre"

label_sad<-as.data.frame(rep("sad",times=dim(uri_sad)[1]))
names(label_sad)[1]<-"genre"

t_happy<-cbind(playlists_happy,track_happy,label_happy)
t_sad<-cbind(playlists_sad,track_sad,label_sad)

playlists<-rbind(t_happy,t_sad)

plot_loudness<-ggplot(data=playlists,aes(x=genre,y=loudness,fill=genre))+
  geom_boxplot()+
  theme(legend.position = "none")

plot_danceability<-ggplot(data=playlists,aes(x=genre,y=danceability,fill=genre))+
  geom_boxplot()+
  theme(legend.position = "none")

plot_energy<-ggplot(data=playlists,aes(x=genre,y=energy,fill=genre))+
  geom_boxplot()+
  theme(legend.position = "none")

plot_liveness<-ggplot(data=playlists,aes(x=genre,y=liveness,fill=genre))+
  geom_boxplot()+
  theme(legend.position = "none")

plot_valence<-ggplot(data=playlists,aes(x=genre,y=valence,fill=genre))+
  geom_boxplot()+
  theme(legend.position = "none")

plot_tempo<-ggplot(data=playlists,aes(x=genre,y=tempo,fill=genre))+
  geom_boxplot()+
  theme(legend.position = "none")

plot_instrumentalness<-ggplot(data=playlists,aes(x=genre,y=instrumentalness,fill=genre))+
  geom_boxplot()+
  theme(legend.position = "none")

plot_acousticness<-ggplot(data=playlists,aes(x=genre,y=acousticness,fill=genre))+
  geom_boxplot()+
  theme(legend.position = "none")

plot_speechiness<-ggplot(data=playlists,aes(x=genre,y=speechiness,fill=genre))+
  geom_boxplot()+
  theme(legend.position = "none")

plot_grid(plot_loudness,plot_danceability,
          plot_energy,plot_liveness,
          plot_tempo,
          plot_valence,plot_instrumentalness,plot_acousticness,plot_speechiness,
          labels = "AUTO")
