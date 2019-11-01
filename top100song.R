#characteristic of top 50 song
library(ggplot2)
library(dplyr)

library(spotifyr)
library(tidyverse)
library(data.table)
library(genius)
library(tm)
library(cowplot)
library(corrplot)
library(Hmisc)
library(corrplot)

Sys.setenv(SPOTIFY_CLIENT_ID = 'c97953505980495e8fef75f234f6e82f')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '665799370c6044e4bf5eb2a74cb25f7f')

access_token <- get_spotify_access_token(client_id = Sys.getenv('SPOTIFY_CLIENT_ID'), client_secret = Sys.getenv('SPOTIFY_CLIENT_SECRET'))

#spotify:playlist:37i9dQZEVXbObFQZ3JLcXt --> top 50 indonesia
#spotify:playlist:37i9dQZEVXbMDoHDwVN2tF --> global top 50
#spotify:playlist:37i9dQZEVXbKpV6RVDTWcZ --> indonesia viral 50
#spotify:playlist:37i9dQZEVXbLiRSasKsNU9 --> global viral 50

playlist<-get_playlist_tracks("37i9dQZEVXbLiRSasKsNU9")
uri<-as.data.frame(playlist$track.id)
track<-get_track_audio_features(uri[1:dim(uri)[1],])
data_model<-cbind(playlist,track)

linearMod <- lm(track.popularity ~ danceability+energy+key+
                  loudness+speechiness+acousticness+instrumentalness+
                  liveness+valence+tempo+track.duration_ms+0, data=data_model)
summary(linearMod)

cor.test(data_model$track.popularity,data_model$acousticness,method="pearson")

track_corr=cbind(track[,1:11],data_model$track.duration_ms)
names(track_corr)[12]<-"duration"

res2 <- rcorr(as.matrix(track_corr))
res2

res<-cor(as.matrix(track_corr))
par(ask = TRUE)
res

corrplot(res, method="circle", order = "alphabet")

plot_loudness<-ggplot(data=track,aes(y=loudness))+
  geom_boxplot()+
  theme(legend.position = "none")

plot_danceability<-ggplot(data=track,aes(y=danceability,fill=danceability))+
  geom_boxplot()+
  theme(legend.position = "none")

