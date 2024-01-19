library(cowplot)
library(ggplot2)
library(lubridate)
library(tidyverse) 
library(readxl) 
library(gridExtra)
 
ballots <- read_excel("~/ballots.xltx", sheet = "H2H")  
ballots$end_date<-mdy(ballots$end_date) 
b2t <- ballots %>%
  dplyr::arrange(desc(candidate_name)) %>% 
  dplyr::group_by(candidate_name) %>% 
  dplyr::mutate(pct_03da = zoo::rollmean(Two_Way, k = 3, fill = NA),
                pct_05da = zoo::rollmean(Two_Way, k = 5, fill = NA),
                pct_07da = zoo::rollmean(Two_Way, k = 7, fill = NA),
                pct_15da = zoo::rollmean(Two_Way, k = 15, fill = NA),
                pct_21da = zoo::rollmean(Two_Way, k = 21, fill = NA)) %>% 
  dplyr::ungroup() 
h_h<-
ggplot(b2t, mapping = aes(x=end_date, y=pct_05da,color=candidate_name))+
  geom_point(alpha=1/5,size=5) +
  geom_smooth(method = 'loess', formula = 'y~x',span=1.5, se=FALSE, lwd=2) +
  scale_colour_manual(values = c('red','blue'))+
   
  labs(title = 'Biden vs Trump National Polling',x=' ',caption = "source:538.com\njamesstats.github.io/blog",
       y='%Percentage%', colour='Key')+
  theme_bw()+
  theme(legend.position = 'none') +
theme(plot.title = element_text(face = "bold",size = 17,hjust = 0)) +
  theme(plot.title = element_text(face = "bold",hjust = 0),
        plot.caption = element_text(face = 'italic'),axis.text = element_text(face = 'bold'))
  ggsave('~/R/blog/r-images/biden_trump_h2h.png',dpi = 500) 

blk<-
ggplot(ballots, mapping = aes(x=end_date, y=Two_Way_BLK,color=candidate_name))+
  geom_point(alpha=1/10,size=5) +
  geom_smooth(method = 'loess', formula = 'y~x',span=1, se=FALSE, lwd=2) +
  scale_colour_manual(values = c('red','blue'))+
  labs(title = 'Biden vs Trump:Black Voters',x='',caption = "source:538.com\njamesstats.github.io/blog",
       y='%percentage%', colour='Key')+
  theme_bw()+
  theme(legend.position = 'none')  +
  theme(plot.title = element_text(face = "bold",size = 17,hjust = 1))+
  theme(plot.title = element_text(face = "bold",hjust = 1),
        plot.caption = element_text(face = 'italic'),axis.text = element_text(face = 'bold'))+
  ggsave('~/R/blog/r-images/biden_trump_blk.png',dpi = 500) 
  

hsp<-
ggplot(ballots, mapping = aes(x=end_date, y=Two_Way_HISP,color=candidate_name))+
  geom_point(alpha=1/10,size=5) +
  geom_smooth(method = 'loess', formula = 'y~x',span=1, se=FALSE, lwd=2) +
  scale_colour_manual(values = c('red','blue'))+
  labs(title = 'Biden vs Trump:Hispanic Voters',x=' ',caption = "source:538.com\njamesstats.github.io/blog",
       y='%Percentage%', colour='Key')+
  theme_bw()+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(face = "bold",hjust = 1),
        plot.caption = element_text(face = 'italic'),axis.text = element_text(face = 'bold'))+
  ggsave('~/R/blog/r-images/biden_trump_hsp.png',dpi = 500) 

u35<-
ggplot(ballots, mapping = aes(x=end_date, y=Two_Way_U35,color=candidate_name))+
  geom_point(alpha=1/10,size=5) +
  geom_smooth(method = 'loess', formula = 'y~x',span=1, se=FALSE, lwd=2) +
  scale_colour_manual(values = c('red','blue'))+
  labs(title = 'Biden vs Trump:Under35 Voters',x=' ',caption = "sources:538.com(jamesstats.github.io/blog)",
       y='%percentage%', colour='Key')+
  theme_bw()+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(face = "bold",hjust = 1),
        plot.caption = element_text(face = 'italic'),axis.text = element_text(face = 'bold'))+
  ggsave('~/R/blog/r-images/biden_trump_u35.png',dpi = 500) 

plist<-list(h_h,blk,hsp,u35) 
grid.arrange(grobs = plist, ncol = 2) ## display plot
