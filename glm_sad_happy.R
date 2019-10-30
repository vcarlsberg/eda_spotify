library(readxl)
library(factoextra)
library(FactoMineR)
library(ggplot2)
library(dplyr)

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

label_happy<-as.data.frame(rep(1,times=dim(uri_happy)[1]))
names(label_happy)[1]<-"genre"

label_sad<-as.data.frame(rep(0,times=dim(uri_sad)[1]))
names(label_sad)[1]<-"genre"

t_happy<-cbind(track_happy,label_happy)
t_sad<-cbind(track_sad,label_sad)

playlists<-rbind(t_happy,t_sad)

data_model<-playlists[,c(1:11,17,19)]

#principle_component<-prcomp(data_model[,1:12], scale = TRUE)
#eig.val<-get_eigenvalue(principle_component)
#fviz_eig(principle_component)

res.pca <- PCA(data_model[,1:12],  graph = TRUE)
get_eig(res.pca)

final.pca<-PCA(data_model[,1:12],ncp=7)

coord<-as.data.frame(final.pca$ind$coord)
coord<-cbind(coord,data_model[,13])
names(coord)[8]<-"genre"

#coord<-as.data.frame(as.matrix(coord))

general_lin<-glm(genre ~ Dim.1 + Dim.2 + Dim.3+Dim.4+Dim.5+Dim.6+Dim.7, data = coord)
predict(general_lin, type="response")

summary(general_lin)

#TESTING
#spotify:album:7FR9c3LHGJEdMdoGOzPRJ0
album_test<-get_album_tracks("7FR9c3LHGJEdMdoGOzPRJ0")
uri_test<-as.data.frame(album_test$id) 
track_test<-get_track_audio_features(uri_test[1:dim(uri_test)[1],])
data_model_test<-track_test[,c(1:11,17)]
final.pca_test<-PCA(data_model_test[,1:12],ncp=7)
coord_test<-as.data.frame(final.pca_test$ind$coord)
#coord_test<-cbind(coord_test,data_model[,13])
#predict(general_lin,final.pca_test)
predict_test<-predict(general_lin,newdata=coord_test, ) 
predict_test<-as.data.frame(round(predict_test)) 

album_test_result<-cbind(album_test,predict_test)
names(album_test_result)[15]<-"sad(0)/happy(1)"
View(album_test_result[,c(4,5,9,15)])

#spotify:album:7FR9c3LHGJEdMdoGOzPRJ0
#test_song<-get_track_audio_features("3u9PKhNvPHDRMRQDTIX7zJ")
#test_song<-test_song[,c(1:11,17)]
#test_song<-as.data.frame(test_song)
#test_song.pca<-PCA(test_song[,1:12],ncp=7)
#spotify:track:3u9PKhNvPHDRMRQDTIX7zJ

