library(spotifyr)
library(tidyverse)
library(data.table)
library(genius)
library(tm)
library(cowplot)

Sys.setenv(SPOTIFY_CLIENT_ID = 'c97953505980495e8fef75f234f6e82f')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '665799370c6044e4bf5eb2a74cb25f7f')

access_token <- get_spotify_access_token(client_id = Sys.getenv('SPOTIFY_CLIENT_ID'), client_secret = Sys.getenv('SPOTIFY_CLIENT_SECRET'))


#af_DK <- get_artist_audio_features('Didi Kempot')
#af_VV <- get_artist_audio_features('Via Vallen')
#af_BSB <- get_artist_audio_features('Backstreet Boys')
#af_BTS <- get_artist_audio_features('BTS')
#af_NK <- get_artist_audio_features('Nella Kharisma')
#af<-rbind(af_DK,af_VV,af_BSB,af_BTS,af_NK)

#ggplot()+
#  geom_boxplot(data = af, 
#             mapping = aes(x=artist_name,y = liveness,color = as.factor(artist_name)), 
#             )

# ggplot()+
#   geom_boxplot(data = af, 
#                mapping = aes(x=artist_name,y = danceability,color = as.factor(artist_name)), 
#   )
# 
# af_filter=filter(af,artist_name=="Via Vallen")
# ggplot()+
#   geom_histogram(
#     bins=10,
#     data = af_filter, 
#     mapping = aes(danceability)
#   )
# 
# 
# 
# lyrics<-genius_lyrics(artist = "Didi Kempot", song = "Pamer Bojo", info = "title")
#spotify:playlist:37i9dQZF1DX7qK8ma5wgG1
#spotify:playlist:37i9dQZF1DXaKIA8E7WcJj 60 an
#playlists <- get_user_playlists('Sad Songs')
playlists_60 <- get_playlist_tracks("37i9dQZF1DXaKIA8E7WcJj")
playlists_70 <- get_playlist_tracks("37i9dQZF1DWTJ7xPn4vNaz")
playlists_80 <- get_playlist_tracks("37i9dQZF1DX4UtSsGT1Sbe")
playlists_90 <- get_playlist_tracks("37i9dQZF1DXbTxeAdrVG2l")

uri_60<-as.data.frame(playlists_60$track.id) 
uri_70<-as.data.frame(playlists_70$track.id) 
uri_80<-as.data.frame(playlists_80$track.id) 
uri_90<-as.data.frame(playlists_90$track.id) 

track_60<-get_track_audio_features(uri_60[1:dim(uri_60)[1],])
track_70<-get_track_audio_features(uri_70[1:dim(uri_70)[1],])
track_80<-get_track_audio_features(uri_80[1:dim(uri_80)[1],])
track_90<-get_track_audio_features(uri_90[1:dim(uri_90)[1],])

label_60<-as.data.frame(rep("60an",times=dim(uri_60)[1]))
names(label_60)[1]<-"genre"

label_70<-as.data.frame(rep("70an",times=dim(uri_70)[1]))
names(label_70)[1]<-"genre"

label_80<-as.data.frame(rep("80an",times=dim(uri_80)[1]))
names(label_80)[1]<-"genre"

label_90<-as.data.frame(rep("90an",times=dim(uri_90)[1]))
names(label_90)[1]<-"genre"

t_60<-cbind(playlists_60,track_60,label_60)
t_70<-cbind(playlists_70,track_70,label_70)
t_80<-cbind(playlists_80,track_80,label_80)
t_90<-cbind(playlists_90,track_90,label_90)

playlists<-rbind(t_60,t_70,t_80,t_90)
summary(track)

ggplot()+
  geom_histogram(
    bins=10,
    data = track, 
    mapping = aes(tempo)
  )

#ggplot(data=playlists,aes(x=tempo))+
#  geom_histogram(bins=10,data=playlists,mapping=aes(tempo))

#ggplot2.histogram(data=playlists, xName='danceability',
#                  groupName='genre', legendPosition="top",
#                  alpha=0.5 )

#ggplot(data=playlists,aes(x=energy,fill=genre))+
#  geom_histogram(binwidth=2)

ggplot(data=playlists)+
  geom_histogram(bins=5,mapping=aes(x=tempo,fill=genre))

plot_loudness<-ggplot(data=playlists,aes(x=genre,y=loudness,fill=genre))+
  geom_boxplot()+
  theme(legend.position = "none")+
  stat_summary(fun.y="mean", geom="point", size=2,
               position=position_dodge(width=0.75), color="red")

plot_danceability<-ggplot(data=playlists,aes(x=genre,y=danceability,fill=genre))+
  geom_boxplot()+
  theme(legend.position = "none")+
  stat_summary(fun.y="mean", geom="point", size=2,
               position=position_dodge(width=0.75), color="red")

plot_energy<-ggplot(data=playlists,aes(x=genre,y=energy,fill=genre))+
  geom_boxplot()+
  theme(legend.position = "none")+
  stat_summary(fun.y="mean", geom="point", size=2,
               position=position_dodge(width=0.75), color="red")

plot_liveness<-ggplot(data=playlists,aes(x=genre,y=liveness,fill=genre))+
  geom_boxplot()+
  theme(legend.position = "none")+
  stat_summary(fun.y="mean", geom="point", size=2,
               position=position_dodge(width=0.75), color="red")

plot_valence<-ggplot(data=playlists,aes(x=genre,y=valence,fill=genre))+
  geom_boxplot()+
  theme(legend.position = "none")+
  stat_summary(fun.y="mean", geom="point", size=2,
               position=position_dodge(width=0.75), color="red")
plot_valence
aggregate(playlists, by=list(playlists$genre), FUN=mean)

plot_tempo<-ggplot(data=playlists,aes(x=genre,y=tempo,fill=genre))+
  geom_boxplot()+
  theme(legend.position = "none")+
  stat_summary(fun.y="mean", geom="point", size=2,
               position=position_dodge(width=0.75), color="red")

plot_instrumentalness<-ggplot(data=playlists,aes(x=genre,y=instrumentalness,fill=genre))+
  geom_boxplot()+
  theme(legend.position = "none")+
  stat_summary(fun.y="mean", geom="point", size=2,
               position=position_dodge(width=0.75), color="red")
  

plot_grid(plot_loudness,
          plot_energy,
          plot_tempo,
          plot_valence,plot_instrumentalness,plot_liveness,
          labels = "AUTO")

#ggplot(playlists, aes(x=genre), danceability)) +
#  geom_boxplot()
df <- apply(playlists,2,as.character)
write.csv(df,"spotify.csv")

library(mlbench)
library(caret)
control<-trainControl(method="repeatedcv",number=10, repeats=3)
