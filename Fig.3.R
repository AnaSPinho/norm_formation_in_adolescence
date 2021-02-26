# read data 
directionEffect<-read.csv("directionEffect.DF.csv")
w2<-read.csv("w2.csv")

#upload package
library(tidyverse)
library(patchwork)

#theme setup
cleanup = theme(plot.title = element_text(size = 14, face = "bold"),
                legend.key = element_rect(fill = "white"),
                legend.text=element_text(size=14),
                legend.title=element_text(size=14),
                text = element_text(size = 14),
                axis.title.x=element_text(size=14, face="bold"),
                axis.title.y=element_text(size=14, face="bold"),
                panel.background = element_rect(fill = "white", colour = "black"),
                axis.text.x = element_text(size=14),
                axis.text.y = element_text(size=14),
                strip.text = element_text(size = 13)
)

#plot 3a
Fig3<-ggplot(directionEffect, 
             aes(x=factor(direction, 
                          level=c("Extreme\ndisapproval", 
                                  "Moderate\ndisapproval", 
                                  "Moderate\napproval", 
                                  "Extreme\napproval"),
                          labels=c("Extreme\ndisapproval",
                                   "Moderate\ndisapproval", 
                                   "Moderate\napproval", 
                                   "Extreme\napproval")), 
                 y=fit, 
                 fill=direction)) +
  geom_bar(stat="identity", 
           color="black",
           position=position_dodge()) +
  geom_errorbar(aes(ymin=fit-se, 
                    ymax=fit+se),
                    width=0.25,
                    position=position_dodge(width=0.9)) +
  scale_fill_manual(values=c("Extreme\ndisapproval" = "#6A51A3",
                             "Moderate\ndisapproval"= "#9E9AC8",
                             "Moderate\napproval" = "#9E9AC8",
                             "Extreme\napproval"= "#6A51A3"))+
  ylab("Norm adjustment (mean ±s.e.m)")+
  xlab("Direction")+
  geom_text(aes(label=sprintf("%0.2f", round(fit, digits = 2)),
            y=fit+0.07, group=direction),
            position = position_dodge(width=0.9),
            size=5,
            colour= "black",
            fontface = "bold") +
  theme(legend.position = "none")+
  cleanup
Fig3<-Fig3+annotate("label", x= 1.5, y = 0.1, label = "Risk-taking", size=5) 
Fig3<-Fig3+annotate("label", x= 3.5, y = 0.1, label = "Prosocial", size=5) 
Fig3

### 3b - P1
P1<-w2%>%
  select(participantNr, R1, Dbeh_rec)%>%
  mutate(Dbeh_rec=forcats::fct_relevel(Dbeh_rec,
                                       "Risk",
                                       "Prosocial"))%>%
  group_by(Dbeh_rec)%>%
  ggplot(aes(x=R1, 
             color=Dbeh_rec, 
             fill=Dbeh_rec)) +
  geom_bar(alpha=0.9)+
  scale_x_continuous(breaks=seq(1, 11, 1))+
  xlab("Personal norm scores\n[1=very bad;11=very good]")+
  ylab("Frequency")+
  scale_color_manual(values = c("firebrick4","deepskyblue4"), 
                     name="Personal norm domain")+
  scale_fill_manual(values = c("firebrick4","deepskyblue4"), 
                    name="Personal norm domain")+
  theme(legend.position="none")+
  facet_wrap(~Dbeh_rec)+
  cleanup
P1


### 3b - P2
P2<-w2%>%
  select(participantNr, R2, Dbeh_rec)%>%
  mutate(Dbeh_rec=forcats::fct_relevel(Dbeh_rec,
                                       "Risk",
                                       "Prosocial"))%>%
  group_by(Dbeh_rec)%>%
  ggplot(aes(x=R2, 
             color=Dbeh_rec, 
             fill=Dbeh_rec)) +
  geom_bar(alpha=0.9)+
  scale_x_continuous(breaks=seq(1, 11, 1))+
  scale_y_continuous(limit=c(0,150))+
  xlab("Personal norm scores\n[1=very bad;11=very good]")+
  ylab("Frequency")+
  scale_color_manual(values = c("firebrick4","deepskyblue4"), 
                     name="Personal norm domain")+
  scale_fill_manual(values = c("firebrick4","deepskyblue4"), 
                    name="Personal norm domain")+
  theme(legend.position="none")+
  facet_wrap(~Dbeh_rec)+
  cleanup
P2 

####--------------------- CREATE PANEL 3 --------------------- ###
# Impact of peer norms depends on their direction and the domain of behaviour

P1<-P1+theme(axis.title.x=element_blank())
P1

panel<-  Fig3 | (P1/P2)
panel<- panel +
  plot_annotation(tag_levels = 'a')
panel<-panel+plot_layout(ncol = 2, 
                         widths=c(4.3,4.5), 
                         heights = c(3,3))
panel



