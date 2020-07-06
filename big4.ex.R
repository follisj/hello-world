library(rvest)
library(jpeg)
library(grid)
library(ggimage)
library(png)
library(ggalt)
library(tidyverse)
library(ggrepel)
library(spotifyr)
library(plotly)

#### create big4 data

s.url <-"https://en.wikipedia.org/wiki/Metallica_discography"

metallica <- s.url %>%
  read_html()%>%
  html_table(fill=T)
metallica <- metallica[[2]][3:nrow(metallica[[2]])-1,1:3]
names(metallica) <- c("Title","Details","Chart")
metallica <- metallica %>% separate(Details,c("A","Month","Day","Year")) %>% 
  select(-A) %>% unite("Date",c("Day","Year"),sep=", ") %>% 
  unite("Date",c("Month","Date"),sep=" ") %>% 
  mutate(Date=as.Date(Date,format="%B%d,%Y"))
metallica$Chart <- gsub("-",NA,metallica$Chart) %>% as.numeric()
metallica$Artist <- "Metallica"

s.url <- "https://en.wikipedia.org/wiki/Megadeth_discography"
megadeth <- s.url %>%
  read_html()%>%
  html_table(fill=T)
megadeth <- megadeth[[2]][3:nrow(megadeth[[2]])-1,1:3]
names(megadeth) <- c("Title","Details","Chart")
megadeth <- megadeth %>% separate(Details,c("A","Month","Day","Year")) %>% 
  select(-A) %>% unite("Date",c("Day","Year"),sep=", ") %>% 
  unite("Date",c("Month","Date"),sep=" ") %>% 
  mutate(Date=as.Date(Date,format="%B%d,%Y"))
megadeth$Chart <- gsub("-",NA,megadeth$Chart) %>% as.numeric()
megadeth$Artist <- "Megadeth"

s.url <- "https://en.wikipedia.org/wiki/Anthrax_discography"
anthrax <- s.url %>%
  read_html() %>%
  html_table(fill=T)
anthrax <- anthrax[[2]][3:nrow(anthrax[[2]])-1,1:3]
names(anthrax) <- c("Title","Details","Chart")
anthrax <- anthrax %>% separate(Details,c("A","Month","Day","Year")) %>% 
  select(-A) %>% unite("Date",c("Day","Year"),sep=", ") %>% 
  unite("Date",c("Month","Date"),sep=" ") %>% 
  mutate(Date=as.Date(Date,format="%B%d,%Y"))
anthrax$Chart <- gsub("-",NA,anthrax$Chart) %>% as.numeric()
anthrax$Artist <- "Anthrax"

s.url <- "https://en.wikipedia.org/wiki/Slayer_discography"
slayer <- s.url %>%
  read_html() %>%
  html_table(fill=T)
slayer <- slayer[[2]][3:nrow(slayer[[2]])-1,1:3]
names(slayer) <- c("Title","Details","Chart")
slayer <- slayer %>% separate(Details,c("A","Month","Day","Year")) %>% 
  select(-A) %>% unite("Date",c("Day","Year"),sep=", ") %>% 
  unite("Date",c("Month","Date"),sep=" ") %>% 
  mutate(Date=as.Date(Date,format="%B%d,%Y"))
slayer$Chart <- gsub("-",NA,slayer$Chart) %>% as.numeric()
slayer$Artist <- "Slayer"

big4 <- rbind(metallica,megadeth,anthrax,slayer)
big4$Title <- str_to_title(big4$Title)

#https://www.logolynx.com/topic/slayer
slayer.pic=readJPEG("slayer.jpg")
anthrax.pic<-readJPEG("anthrax.jpg")
mega.pic <- readJPEG("megadeth.jpg")
metallica.pic <- readJPEG("metallica.jpg")


ggplot(big4,aes(Date,log(Chart),col=Artist,label=Title))+
  geom_line()+
  geom_point()+
  geom_label_repel()+
  scale_y_reverse()+
  theme_bw()+
  ggtitle("The Big 4")

ggplot(big4,aes(Date,log(Chart),col=Artist,label=Title))+
  geom_line()+
  geom_point()+
  geom_label_repel()+
  scale_y_reverse()+
  theme_bw()+
  ggtitle("The Big 4")+
  coord_polar()

ggplot(big4,aes(Artist,Date,col=Artist,label=Title))+
  geom_point()+
  geom_label_repel(size=6.2,fill="grey",fontface="bold")+
  #scale_color_manual(values=c("yellow2","blue","black","red"))+
  scale_color_manual(values=c("yellow","blue","black","red"))+
  expand_limits(y=as.Date(c("1983-1-1","2022-1-1")))+
  labs(title="The Big 4",
          subtitle="Studio Albums")+
  theme_bw()+
  theme(panel.background= element_rect(fill="black"),
        plot.background = element_rect(fill="grey"),
        legend.position="none",
        plot.title=element_text(hjust=.5,size=24),
        plot.subtitle=element_text(hjust=.5,size=18),
        axis.text.x=element_blank(),
        axis.text.y=element_text(face="bold"),
        axis.title=element_blank())+
  annotation_custom(rasterGrob(slayer.pic),
                    xmin=3.5,xmax=4.5,ymin=as.Date("2018-1-1"),ymax=as.Date("2024-1-1"))+
  annotation_custom(rasterGrob(metallica.pic),
                    xmin=2.5,xmax=3.5,ymin=as.Date("2018-1-1"),ymax=as.Date("2024-1-1"))+
  annotation_custom(rasterGrob(mega.pic),
                    xmin=1.6,xmax=2.4,ymin=as.Date("2018-1-1"),ymax=as.Date("2024-1-1"))+
  annotation_custom(rasterGrob(anthrax.pic),
                    xmin=0.5,xmax=1.5,ymin=as.Date("2018-1-1"),ymax=as.Date("2024-1-1"))

#ggsave("big4.plot.2b.jpg",big4.plot.2,dpi=320,scale=2)

ggplot(big4,aes(Artist,Date,col=Artist,label=Title))+
  geom_point()+
  geom_label_repel(size=5,fill="grey",fontface="bold")+
  scale_color_manual(values=c("yellow2","blue","black","red"))+
  expand_limits(y=as.Date(c("1983-1-1","2022-1-1")))+
  ggtitle("The Big 4",
          subtitle="")+
  theme_bw()+
  theme(panel.background= element_rect(fill="black"),
        plot.background = element_rect(fill="grey"),
        legend.position="none",
        plot.title=element_text(hjust=.5,size=24),
        axis.text.y=element_blank(),
        axis.title=element_blank())+
  coord_flip()


Sys.setenv(SPOTIFY_CLIENT_ID = '71d7a1e476ca4955bfba84027fe38452')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '8ce4153118d9451c98074e8fba685595')

access_token <- get_spotify_access_token()

anthrax_feat <- get_artist_audio_features('anthrax')
metallica_feat <- get_artist_audio_features('Metallica')
slayer_feat <- get_artist_audio_features('slayer')
megadeth_feat <- get_artist_audio_features('megadeth')

big4_feat <- rbind(metallica_feat,megadeth_feat,anthrax_feat,slayer_feat)
big4_feat$album_name <- str_to_title(big4_feat$album_name)
big4_feat$album_name2 <- gsub('[[:punct:]]','',big4_feat$album_name)
big4_feat$album_name2 <- gsub('...','',big4_feat$album_name2)
big4_feat$album_name2 <- tolower(gsub('[[:space:]]','',big4_feat$album_name2))

albums <- data.frame(album_name=big4$Title)
albums$album_name <- gsub('[[:punct:]]','',albums$album_name)
albums$album_name <- tolower(gsub('[[:space:]]','',albums$album_name))


big4_feat.red <- inner_join(albums,big4_feat)
big4_feat.red %>% group_by(artist_name,album_name) %>% summarize(n=n()) %>% print.data.frame()
big4_feat.red %>% filter(artist_name=="Slayer") %>% group_by(album_name) %>% summarize(n=n()) %>%
  print.data.frame()

big4.all <- big4_feat.red %>% 
  group_by(artist_name,album_name) %>% 
  summarize(dance=mean(danceability,na.rm=T),
            energy=mean(energy,na.rm=T),
            loud=mean(loudness,na.rm=T)/10,
            speech=mean(speechiness,na.rm=T),
            acoustic=mean(acousticness,na.rm=T),
            instrument=mean(instrumentalness,na.rm=T),
            live=mean(liveness,na.rm=T),
            valence=mean(valence,na.rm=T),
            tempo=mean(tempo,na.rm=T)/100,
            n=n()) %>% 
  full_join(big4,by=c("album_name"="Title")) %>% 
  print.data.frame()

big4.all.long <- big4.all %>%
  select(-n,-Chart,-Artist) %>% 
  gather(key="feature",value="rating",-c(album_name,artist_name,Date))

big4.all.long %>% filter(artist_name=="Anthrax",
                         feature=="loud" | 
                           feature=="valence" | 
                           feature=="energy" |
                           feature=="tempo") %>% 
  ggplot(aes(Date,rating,fill=feature))+
  #geom_label(aes(Date,rating,label=feature))+
  geom_label(aes(Date,rating,label=round(rating,2)))+
  expand_limits(y=c(-2.5,2))+
  #geom_point()+
  geom_text(aes(Date,y=-2,label=album_name))+
  theme_bw()+
  coord_flip()



b41 <- ggplot(big4.all,aes(Artist,Date,col=Artist,label=album_name,
                           text=paste("Album: ",album_name,
                                      "<br>loudness: ",round(mean,1),
                                      "<br>Songs: ",n)))+
  geom_point(col="black")+
  geom_text(size=5,fill="grey",fontface="bold")+
  #scale_color_manual(values=c("yellow2","blue","black","red"))+
  scale_color_manual(values=c("yellow","blue","white","red"))+
  expand_limits(y=as.Date(c("1983-1-1","2022-1-1")))+
  labs(title="The Big 4",
       subtitle="Studio Albums")+
  theme_bw()+
  theme(panel.background= element_rect(fill="black"),
        plot.background = element_rect(fill="grey"),
        legend.position="none",
        plot.title=element_text(hjust=.5,size=24),
        plot.subtitle=element_text(hjust=.5,size=18),
        axis.text.x=element_blank(),
        axis.text.y=element_text(face="bold"),
        axis.title=element_blank())+
  annotation_custom(rasterGrob(slayer.pic),
                    xmin=3.5,xmax=4.5,ymin=as.Date("2018-1-1"),ymax=as.Date("2024-1-1"))+
  annotation_custom(rasterGrob(metallica.pic),
                    xmin=2.5,xmax=3.5,ymin=as.Date("2018-1-1"),ymax=as.Date("2024-1-1"))+
  annotation_custom(rasterGrob(mega.pic),
                    xmin=1.6,xmax=2.4,ymin=as.Date("2018-1-1"),ymax=as.Date("2024-1-1"))+
  annotation_custom(rasterGrob(anthrax.pic),
                    xmin=0.5,xmax=1.5,ymin=as.Date("2018-1-1"),ymax=as.Date("2024-1-1"))

ggplotly(b41,tooltip="text")



#genius
# etBzXsMNqqMBiO4-Y1KagIZLySo1wLYf-z0usnomyQMi6owejkBjPNIBlpIBI9Hn #acess token
library(geniusr)
library(assertr)
GENIUS_API_TOKEN=genius_token("etBzXsMNqqMBiO4-Y1KagIZLySo1wLYf-z0usnomyQMi6owejkBjPNIBlpIBI9Hn")

xxx <- genius_lyrics("SLayer","South of Heaven")
for(i in 1:6) {xxx.lyr <- paste(xxx.lyr[1],xxx$lyric[i])}






