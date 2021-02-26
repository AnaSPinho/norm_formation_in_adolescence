# read data 
w1<-read.csv("w1_mean_score.csv")
w2<-read.csv("w2.csv")

# load required packages
library(tidyverse)
library(lme4)
library(lmerTest)
library(effects)
library(car)
library(stargazer)

####### Adolescents’ personal norms prior to peer influence ########

# we will look at individuals mean rating across all risk-taking items and prosocial items
grpMSD<-select(w1, participantNr, all_risk_scores, prosocial_scores) %>% 
  pivot_longer(!participantNr,
               names_to = "personal_norm", 
               values_to = "value") %>% 
  mutate(personal_norm=case_when(
    personal_norm == "all_risk_scores" ~ "Risk-taking",
    personal_norm == "prosocial_scores" ~ "Prosocial"
  )) %>% 
  group_by(personal_norm) %>% 
  summarise(grp.mean=round(mean(value), 2), grp.sd=round(sd(value),2))
grpMSD 

# compute individuals' initial personal norms mean differences across high school grades
# risk-taking
w1<-w1 %>% 
  mutate(gID=as.factor(gID))

r_anova<-aov(all_risk_scores ~ gID, data = w1)
summary(r_anova)

# prosocial
pro_anova<- aov(prosocial_scores ~ gID, data = w1)
summary(pro_anova)



####### Main analysis: adolescents' norm adjustments ########

# model 1: fit linear mixed-effect model to norm adjustments to test whether adjustments are influenced by social source and whether this effect is dependent on age
dfmodels<-w2%>%
  select(participantNr, S, SR, age_z, direction)%>%
  filter(S>=0 & S<=1) %>% # filter out cases away and beyond normative information (Molleman et al 2019, 2020, reference in the paper)
  mutate(participantNr=as.factor(participantNr),
         SR=as.factor(SR),
         direction=as.factor(direction))

mod1<-lmer(S~SR*age_z+(1|participantNr), data=dfmodels, control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))

# model 2: fit linear mixed-effect model to norm adjustments to test whether adjustments are influenced by driection and domain of peer norm
mod2<-lmer(S~SR*age_z+direction+(1|participantNr), data=dfmodels, control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))

#  in order to be compatible with stargazer the output from the models needs to be get a new class
class(mod1) <- "lmerMod"
class(mod2) <- "lmerMod"

# Table 1 (output HTML)
stargazer(mod1, mod2, 
          type="html",
          dep.var.labels = "Norm adjustments",
          column.labels = c("model 1", "model 2"),
          model.numbers=FALSE,
          order = c(7, 1, 2, 6, 3, 5, 4), 
          covariate.labels = c("(Intercept)",
                    "Source (1 when Majority, 0 when Popular peer)",
                    "Age (continuous)", 
                    "Source X Age",
                    "Direction:Extreme disapproval risk", 
                    "Direction:Moderate disapproval risk",
                    "Direction:Moderate approval prosocial"),
          ci=TRUE,
          ci.level=0.95, 
          title="", 
          align=TRUE,
          # star.cutoffs=c(0.05,0.01e,0.001), 
          single.row=TRUE,
          keep.stat = "n",
          digits = 2,
          no.space = FALSE, 
          omit.table.layout = "n",
          report=('vcsp'),
          add.lines=list(c("Number of participants", "89", "89")),
          out=("Table 1.html"))



### create data-frame for Fig.3 ### 

directionEffect <- effects::Effect('direction',
                                   mod2,
                                   se=TRUE)

directionEffect.DF<-as.data.frame(directionEffect)

#re-label the variable for the plot
directionEffect.DF$direction<- factor(directionEffect.DF$direction,
                                      level=c("ext_risk", "mod_risk", "mod_pro", "ext_pro"),
                                      labels=c("Extreme\ndisapproval",
                                               "Moderate\ndisapproval",
                                               "Moderate\napproval",
                                               "Extreme\napproval"))
# write.csv(directionEffect.DF, "directionEffect.DF.csv")



####### Exploratory analyses: adolescents' norm updating strategies ####### 

#  model stay: fit logistic generalized mixed model to participants’ decisions to stay with initial personal norm
w2_stay<-w2%>%
  select(participantNr,S_adjust, SR, age_z, direction) %>% 
  filter(S_adjust=="stay" | S_adjust=="compromise") %>% 
  mutate(stay=case_when(
    S_adjust== 'stay'~'1',
    S_adjust== 'compromise'~'0'
  )) %>% 
  mutate(stay=as.factor(stay),
         SR=as.factor(SR),
         direction=as.factor(direction))

# model 1 - stay
# source interacting with age
log.mod1.age <- glmer(stay ~ SR*age_z+(1 | participantNr), 
                      data = w2_stay, 
                      family = binomial(link = "logit"),
                      control=glmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))

# direction and domain
log.mod1.direction<- glmer(stay ~ SR*age_z+direction+(1 | participantNr), data = w2_stay, family = binomial(link = "logit"), 
                           control=glmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))


#  model copy: fit logistic generalized mixed model to participants’ decisions to copy normative information
w2_copy<-w2%>%
  select(participantNr,S_adjust, SR, age_z, direction) %>% 
  filter(S_adjust=="conformity" | S_adjust=="compromise") %>% 
  mutate(copy=case_when(
    S_adjust== 'conformity'~'1',
    S_adjust== 'compromise'~'0'
  )) %>%
  mutate(copy=as.factor(copy),
         SR=as.factor(SR),
         direction=as.factor(direction))

# model 2 - copy
#source interacting with age
log.mod2.age <- glmer(copy ~ SR*age_z + (1 | participantNr), data = w2_copy, family = binomial(link = "logit"))

#valence and domain
log.mod2.direction<- glmer(copy ~ SR*age_z+direction+(1 | participantNr), data = w2_copy, family = binomial(link = "logit"))

# Table S2 (output HTLM)
stargazer(log.mod1.direction, log.mod2.direction, 
          type="html",
          column.labels = c("model 1", "model 2"),
          model.numbers=FALSE,
          order = c(7, 1, 2, 6, 3, 5, 4), 
          covariate.labels = c("(Intercept)",
                               "Source (1 when Majority, 0 when Popular peer)",
                               "Age (continuous)", 
                               "Source X Age",
                               "Direction:Extreme disapproval risk", 
                               "Direction:Moderate disapproval risk",
                               "Direction:Moderate approval prosocial"),
          ci=TRUE,
          ci.level=0.95, 
          title="", 
          align=TRUE,
          single.row=TRUE,
          keep.stat = "n",
          digits = 2,
          no.space = FALSE, 
          omit.table.layout = "n",
          report=('vcsp'),
          add.lines=list(c("Number of participants", "89", "89")),
          out=("Table S2.html"))

