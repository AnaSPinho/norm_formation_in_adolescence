# read data 
w1<-read.csv("w1_mean_score.csv")
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

# Fig. S1
Fig.S1<-select(w1, participantNr, all_risk_scores, prosocial_scores) %>% 
  pivot_longer(!participantNr,
               names_to = "personal_norm", 
               values_to = "value") %>% 
  mutate(personal_norm=case_when(
    personal_norm == "all_risk_scores" ~ "Risk-taking",
    personal_norm == "prosocial_scores" ~ "Prosocial",
  )) %>% 
  mutate(personal_norm=factor(personal_norm, 
                              levels=c("Risk-taking", "Prosocial")), 
         personal_norm) %>% 
  group_by(personal_norm) %>%
  ggplot(aes(x=round(value,0), 
             fill=personal_norm)) +
  geom_bar(alpha=0.9)+
  scale_x_continuous(breaks=seq(1, 11, 1))+
  xlab("Personal norm scores\n[1=very bad; 11=very good]")+
  ylab("Frequency")+
  scale_fill_manual(values = c("firebrick4","deepskyblue4"), 
                    name="Domain")+
  theme(legend.position="right")+
  cleanup

Fig.S1

# Fig. S2
p_risk<- w1%>% 
  select(all_risk_scores, gID)%>%
  mutate(gID=as.factor(gID)) %>% 
  group_by(gID) %>% 
  na.omit()%>%
  ggplot(aes(x=as.factor(gID), y=all_risk_scores)) + 
  geom_boxplot(outlier.shape = NA,
               notch=F,
               alpha=0.9,
               fill="firebrick4")+
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 4,
    size = 1,
    stroke = 1,
    color = "white",
    fill = "white"
  )+
  geom_jitter(position = position_jitter(0.2), alpha = 0.3)+
  ylab("Personal norm\n[1=very bad;11=very good]")+
  xlab("High school grade")+
  scale_y_continuous(
    limits = c(1, 11), 
    breaks = 1:11, 
    expand = c(0,0)) +
  cleanup
p_risk<-p_risk+ annotate("label", x = 3, y = 9, label = "Risk-taking") 

p_prosocial<- w1%>% 
  select(prosocial_scores, gID)%>%
  mutate(gID=as.factor(gID)) %>% 
  group_by(gID) %>% 
  na.omit()%>%
  ggplot(aes(x=as.factor(gID), y=prosocial_scores)) + 
  geom_boxplot(outlier.shape = NA,
               notch=F,
               alpha=0.9,
               fill="deepskyblue4")+
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 4,
    size = 1,
    stroke = 1,
    color = "white",
    fill = "white"
  )+
  geom_jitter(position = position_jitter(0.2), alpha = 0.3)+
  ylab("Personal norms")+
  xlab("High school grade")+
  scale_y_continuous(
    limits = c(1, 11), 
    breaks = 1:11, 
    expand = c(0,0)) +
  cleanup
p_prosocial<-p_prosocial + annotate("label", x = 3, y = 3, label = "Prosocial") 

p_prosocial<-p_prosocial+ theme(axis.title.y=element_blank())


Fig.S2<-  p_risk | p_prosocial
Fig.S2


# Fig. S3
Fig.S3 <-w2%>%
  select(S, SR_gen, age)%>%
  filter(S>=0 & S<=1)%>%
  mutate(SR_gen=factor(SR_gen, 
                       levels=c("Popular peer", 
                                "Majority")), 
         SR_gen) %>% 
  group_by(SR_gen, age) %>%
  na.omit()%>%
  ggplot(aes(x=as.factor(age), 
             y=S, 
             fill=SR_gen)) + 
  geom_boxplot(outlier.shape = NA,
               notch=F)+
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 4,
    size = 1,
    stroke = 1,
    color = "red",
    fill = "red"
  )+
  geom_jitter(position = position_jitter(0.2), 
              alpha = 0.3)+
  scale_fill_manual(values = c("Majority" = "forestgreen", "Popular peer" = "deepskyblue3"))+
  ylab("Norm adjustment")+
  xlab("Age")+
  facet_wrap(~SR_gen)+
  theme(legend.position='none')+
  cleanup

Fig.S3 


# Fig. S5
Fig.S5<-w2 %>%
        mutate(SR_gen=factor(SR_gen, 
                             levels=c("Popular peer", "Majority")), 
               SR_gen) %>% 
        ggplot(aes(S, 
                   fill = SR_gen)) +
  geom_rect(aes(xmin = -Inf, 
                ymin = 0, 
                xmax = 0, 
                ymax = Inf), 
            fill="cornsilk2")+
  geom_rect(aes(xmin = 1,
                ymin = 0, 
                xmax = Inf, 
                ymax = Inf), 
            fill="cornsilk2")+
  geom_histogram(position="dodge", 
                 color="black")+
  xlab("Norm adjustment in individual rounds")+
  ylab("Frequency")+
  scale_x_continuous(limits = c(-1, 2))+
  scale_y_continuous(limits = c(0, 245))+
  scale_fill_manual(values = c("Popular peer" = "deepskyblue3", "Majority" = "forestgreen"), 
                    name="Source")+
  theme(legend.position = "right",
        plot.title = element_text(size = 12, face = "bold"),
        legend.key = element_rect(fill = "white"),
        legend.text=element_text(size=10),
        legend.title=element_text(size=11),
        text = element_text(size = 12),
        axis.title.x=element_text(size=12, face="bold"),
        axis.title.y=element_text(size=12, face="bold"),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = 'black', fill = NA))

#arrows & brace
Fig.S5<-Fig.S5+annotate("segment", 
                        x = 0,
                        y = 140, 
                        xend = 0, 
                        yend = 110,
                        arrow = arrow(type = "closed", length = unit(0.3, "cm")))
Fig.S5<-Fig.S5+annotate("text", 
                        x = 0,
                        y=180, 
                        label = "stay",
                        color = "black", 
                        angle = 90, 
                        hjust = 1.5, 
                        size = 4, 
                        fontface = "bold") 
Fig.S5<-Fig.S5+annotate("segment", 
                        x = 1, 
                        y = 195, 
                        xend = 1, 
                        yend = 165,
                        arrow = arrow(type = "closed", length = unit(0.3, "cm")))
Fig.S5<-Fig.S5+annotate("text", 
                        x = 1, 
                        y=240, 
                        label = "copy", 
                        color = "black", 
                        angle = 90, 
                        hjust = 1.5, 
                        size = 4, 
                        fontface = "bold") 
Fig.S5<-Fig.S5+geom_segment(aes(x = 0.10, 
                                y =90 , 
                                xend = 0.90,
                                yend = 90))
Fig.S5<-Fig.S5+geom_segment(aes(x = 0.10, 
                                y = 90 , 
                                xend = 0.10, 
                                yend = 70),  
                                arrow = arrow(type = "closed", length = unit(0.3, "cm")))
Fig.S5<-Fig.S5+geom_segment(aes(x = 0.90, 
                              y = 90 , 
                              xend = 0.90, 
                              yend = 70),  
                              arrow = arrow(type = "closed", length = unit(0.3, "cm")))
Fig.S5<-Fig.S5+annotate("text", 
                        x = 1.175, 
                        y=100, 
                        label = "compromise", 
                        color = "black", 
                        angle = 360, 
                        hjust = 1.5, 
                        size = 4, 
                        fontface = "bold") 
Fig.S5

