# read data 
w2<-read.csv("w2.csv")

#upload packages
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

# Fig. 1 Peer norms impact adolescents’ personal norms
fig.2<-w2%>%
  select(S, SR_gen)%>%
  filter(S>=0 & S<=1)%>%
  group_by(SR_gen) %>%
  summarise(
    meanSR = mean(S),
    SD = sd(S, na.rm = TRUE),
    N = sum(!is.na(S)),
    upper_limit = meanSR + SD/sqrt(N),
    lower_limit = meanSR - SD/sqrt(N),
    .groups="drop"
  )%>%
  ggplot(aes(x=forcats::fct_reorder(SR_gen, meanSR, .desc =FALSE), 
             y=meanSR, 
             fill=SR_gen)) + 
  geom_bar(stat="identity", 
           color="black", 
           position=position_dodge(width = 0.2),
           width=0.5) +
  geom_errorbar(aes(ymax=upper_limit, ymin=lower_limit), 
                width=0.25,
                position=position_dodge()) +
  scale_fill_manual(values = c("Popular peer" = "deepskyblue3", "Majority" = "forestgreen"), 
                    name="Source")+
  ylab("Norm adjustment (mean ±s.e.m)")+
  xlab("Source")+
  theme(legend.position='none',
        axis.text.x = element_text( 
          size=12))+
  geom_text(aes(label=round(meanSR, 2)), 
            position=position_dodge(width=0.9), 
            vjust=5.5, 
            size=5) +
  cleanup

fig.2



