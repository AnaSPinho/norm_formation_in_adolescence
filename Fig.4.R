# read data 
w2<-read.csv("w2.csv")

#upload package
library(tidyverse)

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

# Fig. 4 Participants’ norm updating strategies
fig.4<-w2%>%
  select(S, SR_gen)%>%
  filter(S>=0 & S<=1)%>%
  mutate(Strategy=case_when(
    S== 0~'stay',
    S== 1~ 'copy',
    S > 0 | S <1 ~ 'compromise'))%>%
  group_by(SR_gen, Strategy)%>%
  summarise(n=n())%>%
  mutate(Strategy_freq=n/sum(n))%>%
  ggplot(aes(x=forcats::fct_reorder(SR_gen, Strategy_freq, .desc =FALSE), 
             y=Strategy_freq, 
             fill=factor(Strategy, levels=c("copy", "compromise", "stay"))))+ 
  geom_bar(position="stack", 
           stat="identity",  
           width=0.5, 
           alpha=0.85) +
  scale_fill_manual(values = c("copy"="#01665E", "compromise"="#35978F", "stay"="#80CDC1", "other"="#C7EAE5"), 
                    name="Strategy")+
  ylab("Relative frequency")+
  xlab("Source")+
  labs(fill="Strategy")+
  geom_text(aes(label=scales::percent(Strategy_freq, accuracy=1L), 
                y=Strategy_freq), 
            position=position_fill(vjust=0.5),
            size=6) +
  cleanup
fig.4
