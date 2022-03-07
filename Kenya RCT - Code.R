## Libraries

library(Gmisc)
library(dplyr) 
library(tidyr) 
library(haven)
library(Hmisc)
library(stargazer)
library(sjmisc)
library(modelr)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
library(survival)
library(vtable)
library(gtsummary)
library(ggpubr)
library(readxl)
library(sjPlot)
library("jtools")

## Setting data

Martinez_Glandel <- read_excel("C:/Users/denze/OneDrive/LMU Econ MSc/WS 2021 2022/Experimental/Project/Analysis/Martinez Glandel.xlsx")
View(Martinez_Glandel)   


## Get three Treatments: Control, Treatment and Not part of Experiment (already practicing agroforestry)

Data_Kenya <- Martinez_Glandel %>%
  mutate(Treatment = ifelse(AlreadyFodder == 1, 
                           3, Treatment))

## Setting 2 levels for beliefs about social norms
Data_Kenya$BeliefYN <- ifelse(Data_Kenya$BeliefFodder > 2,0,1)

## Drop observations where Treatment = 0 (Control)

Data_Kenya_0 <-subset(Data_Kenya,Treatment!="0")

## Drop observations where Treatment = 3 (already fodder)

Data_Kenya_3 <-subset(Data_Kenya,Treatment!="3")


# I. Descriptive Statistics 

## Multiple t-tests for Age, Education, Land Size and Workers (continuous).

lapply(Data_Kenya_3[,c("Age", "Education", "LandSize", "Workers")], function(x) t.test(x ~ Data_Kenya_3$Treatment, var.equal = TRUE))

lapply(Data_Kenya_0[,c("Age", "Education", "LandSize", "Workers")], function(x) t.test(x ~ Data_Kenya_0$Treatment, var.equal = TRUE))

## Chi-square test for Gender, Marital Status and Legal Status (categorical) 
## Dropping Treatment = 3 

chisq.test(Data_Kenya_3$Treatment, Data_Kenya_3$Gender, correct=FALSE)
chisq.test(Data_Kenya_3$Treatment, Data_Kenya_3$Marriage, correct=FALSE)
chisq.test(Data_Kenya_3$Treatment, Data_Kenya_3$Legal, correct=FALSE)

## Dropping Control 

chisq.test(Data_Kenya_0$Treatment, Data_Kenya_0$Gender, correct=FALSE)
chisq.test(Data_Kenya_0$Treatment, Data_Kenya_0$Marriage, correct=FALSE)
chisq.test(Data_Kenya_0$Treatment, Data_Kenya_0$Legal, correct=FALSE)


## Summary Statistics - Tables

## Get summary statistics for Treatment and Control

library(smd)

Table1 <- Data_Kenya_3 %>% dplyr::select(-BeliefFodder) %>% 
  dplyr::select(-AlreadyFodder) %>%
  dplyr::select(-PostFodder) %>%
tbl_summary(by = Treatment) %>% 
add_difference() %>%
  modify_header(label ~ "**Characteristic**") %>%
  modify_spanning_header(c("stat_1") ~ "**Control Group**") %>%
  modify_spanning_header(c("stat_2") ~ "**Treatment Group**") %>%
  modify_caption("**Table 1. Comparisons of Demographic and Land Characteristics between
Treatment and Control Group.**") %>%
  bold_labels() 

library(Rcpp)
library(officer)
library(flextable)

## Get summary statistics for Treatment and Already Agroforestry (Treatment = 3)

Table1.1 <- Data_Kenya_0 %>% dplyr::select(-BeliefFodder) %>% 
  dplyr::select(-AlreadyFodder) %>%
  dplyr::select(-PostFodder) %>%
  tbl_summary(by = Treatment) %>% 
  add_difference() %>%
  modify_header(label ~ "**Characteristic**") %>%
  modify_spanning_header(c("stat_1") ~ "**Treatment Group**") %>%
  modify_spanning_header(c("stat_2") ~ "**Omitted Group**") %>%
  modify_caption("**Table 1.1. Comparisons of Demographic and Land Characteristics between
Treatment and Omitted Group.**") %>%
  bold_labels() 

## Summary Statistics - Figures

library(ggplot2)

## Land Characteristics

## 1. Land Size (Box Plot)

boxplot(LandSize ~ Treatment, data = Data_Kenya,
        horizontal = F,
        col = c("#FF4500", "#FFA500","#FFD700"),
        ylim = c(400, 85000),
        names=c("Control","Treatment","Omitted"),
        ylab ="Land Size (sqm)",
        xlab = "Group") 

## 1. Land Size (Bar Plot)

Names <- Data_Kenya %>%
  mutate(Treatment = recode(Treatment, "1" = "Treatment", "0" = "Control", "3" = "Omitted"))

ggplot(Names, aes(x = LandSize)) +
  geom_histogram(fill = "white", colour = "black", bins = 10, binwidth = 3000) +
    facet_grid(Treatment~.) + labs(x = "Land Size (sqm)", y ="Frequency") +  theme(strip.background = element_rect(fill = "#FFA500"))  

## 2. Workers (Box Plot)

boxplot(Workers ~ Treatment, data = Data_Kenya,
        col = c("#FF4500", "#FFA500","#FFD700"),
        horizontal = F,
        ylim = c(0, 64),
        names=c("Control","Treatment","Omitted"),
        ylab ="Number of Workers",
        xlab = "Group")

## 2. Workers (Bar Plot)

ggplot(Names, aes(x = Workers)) +
  geom_histogram(fill = "white", colour = "black", bins = 10, binwidth = 5) +
  facet_grid(Treatment ~ .) + labs(x = "Number of Workers", y ="Frequency") +  theme(strip.background = element_rect(fill = "#FFA500"))  

## Demographic Characteristics

## 1. Age (Box Plot)

boxplot(Age ~ Treatment, data = Data_Kenya,
        horizontal = F,
        col = c("#FF4500", "#FFA500","#FFD700"),
        ylim = c(0, 90),
        names=c("Control","Treatment","Omitted"),
        ylab ="Age",
        xlab = "Group")

## 1. Age (Bar Plot)

ggplot(Names, aes(x = Age)) +
  geom_histogram(fill = "white", colour = "black", bins = 10, binwidth = 5) +
  facet_grid(Treatment ~ .) + labs(x = "Age", y ="Frequency") +  theme(strip.background = element_rect(fill = "#FFA500"))  

## 2. Gender (Bar Plot) --> Maybe not necessary 

ggplot(Names, aes(x = Gender)) +
  geom_histogram(fill = "white", colour = "black", bins = 3, binwidth = 0.2) +
  facet_grid(Treatment ~ .) + labs(x = "Gender", y ="Frequency") +  theme(strip.background = element_rect(fill = "#FFA500"))  

## 3. Education (Box Plot)

boxplot(Education ~ Treatment, data = Data_Kenya,
        horizontal = F,
        col = c("#FF4500", "#FFA500","#FFD700"),
        ylim = c(0, 16),
        names=c("Control","Treatment","Omitted"),
        ylab ="Years of Education",
        xlab = "Group")

## 3. Education (Bar Plot)

ggplot(Names, aes(x = Education)) +
  geom_histogram(fill = "white", colour = "black", bins = 8, binwidth = 1) +
  facet_grid(Treatment ~ .) +  labs(x = "Years of Education", y ="Frequency") +  theme(strip.background = element_rect(fill = "#FFA500"))

## Multiple comparisons 

## 1. Gender & Land Size (Bar Plot)

gender.mean_land <- t(tapply(Data_Kenya$LandSize,
                        list(Data_Kenya$Treatment, Data_Kenya$Gender), mean))

par(mar = c(5, 5, 3.5, 5))
barplot(gender.mean_land, col=c("#FF4500", "#FFA500","#FFD700"), beside=TRUE, names.arg=c("Control","Treatment","Omitted"),
        xlab = "Group", ylab = "Mean Land Size (sqm)", legend=c("Male","Female","Diverse"),
        args.legend=list(title="Gender", x = "topright", bty="n", xpd = TRUE, inset=c(-0.45,0)))

## 2. Gender & Legal (Table) --> Maybe not necessary, doesn't say much

Gender_Legal<- Data_Kenya %>%
  group_by(Treatment,Legal,Gender) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

## 3. Treatment & Belief Fodder (Table)

Belief <- Data_Kenya %>%
  group_by(Treatment, BeliefFodder) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

Names_Belief <- Belief %>%
  mutate(Treatment = recode(Treatment, "1" = "Treatment", "0" = "Control", "3" = "Omitted"))

ggplot(Names_Belief, aes(fill=factor(BeliefFodder), y=freq, x=Treatment)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values = c("#FF8C00", "#FFA500","#FFD700","#B8860B"), labels = c("1", "2", "3","4")) +
  labs(x = "Group", y = "Proportion", fill = "Belief Fodder") + facet_grid(.~Treatment, scales="free") +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +   
  scale_y_continuous(expand = expansion(mult = c(0, .1)))


## 4. Education & Belief Fodder (Table) --> Maybe not necessary. Can't imply that it has to do with education

Education_Belief <- Data_Kenya %>%
  group_by(Education,BeliefFodder) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

## 5. Education & Already Fodder (Table)

Education_Already <- Data_Kenya %>%
  group_by(Education, AlreadyFodder) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

## 6. Education & Already Fodder (Box Plot)
Names_Educ_Belief <- Data_Kenya %>%
  mutate(AlreadyFodder = recode(AlreadyFodder, "1" = "Already Fodder", "0" = "No Fodder"))

ggplot(Names_Educ_Belief, aes(factor(AlreadyFodder), Education, fill = AlreadyFodder)) + geom_boxplot() +
  geom_jitter(width = 0.009, cex=0.02) + stat_summary(fun=mean, geom="point", shape=18,
                                                      size=3, color="red") + 
  labs(x = "Already Fodder", y ="Years of Education") + scale_fill_manual( values=c("#FFA500", "#FFD700")) +
  theme(legend.position="none")

# II. Analysis
# Setting necesary dfs

# No missing values
Data_Kenya_3F <- subset(Data_Kenya_3, PostFodder!="N/A")
# No missing values + no outliers
Data_Kenya_3FnOL <- subset(Data_Kenya_3F, Workers < 26)

# No missing values; belief/no belief
Data_Kenya_3F_0 <- subset(Data_Kenya_3F, BeliefYN == 0)
Data_Kenya_3F_1 <- subset(Data_Kenya_3F, BeliefYN == 1)

# No missing values + no outliers; belief/no belief
Data_Kenya_3FnOL_0 <- subset(Data_Kenya_3FnOL, BeliefYN == 0)
Data_Kenya_3FnOL_1 <- subset(Data_Kenya_3FnOL, BeliefYN == 1)

# No missing values + only controls
Data_Kenya_3F_C <- subset(Data_Kenya_3F, Treatment == 0)

# No missing values + only controls + no outliers
Data_Kenya_3FnOL_C <- subset(Data_Kenya_3FnOL, Treatment == 0)

# No missing values + only treated
Data_Kenya_3F_T <- subset(Data_Kenya_3F, Treatment == 1)

# No missing values + only treated + no outliers
Data_Kenya_3FnOL_T <- subset(Data_Kenya_3FnOL, Treatment == 1)


# II.1 Set of mean comparation, without controls

## 1. T test. H1: control < treated - No outliers
t.test(Data_Kenya_3FnOL$PostFodder ~ Data_Kenya_3FnOL$Treatment, 
       alternative = "less", var.equal = TRUE)

## 2. T test. H1: control < treated - Full db (robustness)
t.test(Data_Kenya_3F$PostFodder ~ Data_Kenya_3F$Treatment, 
       alternative = "less", var.equal = TRUE)

# 3. T test. H1: control < treated - No ourliers, no belief
t.test(Data_Kenya_3FnOL_0$PostFodder ~ Data_Kenya_3FnOL_0$Treatment, 
       alternative = "less", var.equal = TRUE)

# 4. T test. H1: control < treated - Full db, no belief (robustness)
t.test(Data_Kenya_3F_0$PostFodder ~ Data_Kenya_3F_0$Treatment, 
       alternative = "less", var.equal = TRUE)

# 5. T test. H1: control < treated - No ourliers, belief
t.test(Data_Kenya_3FnOL_1$PostFodder ~ Data_Kenya_3FnOL_1$Treatment, 
       alternative = "less", var.equal = TRUE)

# 6. T test. H1: control < treated - Full db, belief (robustness)
t.test(Data_Kenya_3F_1$PostFodder ~ Data_Kenya_3F_1$Treatment, 
       alternative = "less", var.equal = TRUE)

# 7. T test. H1: No belief < Belief - No ourliers, controls
t.test(Data_Kenya_3FnOL_C$PostFodder ~ Data_Kenya_3FnOL_C$BeliefYN, 
       alternative = "less", var.equal = TRUE)

# 8. T test. H1: No belief < Belief - Full db, controls (robustness)
t.test(Data_Kenya_3F_C$PostFodder ~ Data_Kenya_3F_C$BeliefYN, 
       alternative = "less", var.equal = TRUE)

# 9. T test. H1: No belief < Belief - No ourliers, treated
t.test(Data_Kenya_3FnOL_T$PostFodder ~ Data_Kenya_3FnOL_T$BeliefYN, 
       alternative = "less", var.equal = TRUE)

# 10. T test. H1: No belief < Belief - Full db, treated (robustness)
t.test(Data_Kenya_3F_T$PostFodder ~ Data_Kenya_3F_T$BeliefYN, 
       alternative = "less", var.equal = TRUE)

# II.2 Set of regressions, with controls

common_controls <-	~ Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal

# 1. Probit - no outliers
Probit1 <- glm(formula = PostFodder ~ Treatment 
              + Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal, 
              family = binomial(link = "probit"), data = Data_Kenya_3FnOL)
tab_model(Probit1)

# 2. Probit - full db
Probit2 <- glm(formula = PostFodder ~ Treatment 
               + Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal, 
               family = binomial(link = "probit"), data = Data_Kenya_3F)
tab_model(Probit2)

# 3. Probit - no belief, no outliers
Probit3 <- glm(formula = PostFodder ~ Treatment 
               + Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal, 
               family = binomial(link = "probit"), data = Data_Kenya_3FnOL_0)
tab_model(Probit3)

# 4. Probit - no belief, full db
Probit4 <- glm(formula = PostFodder ~ Treatment 
               + Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal, 
               family = binomial(link = "probit"), data = Data_Kenya_3F_0)
tab_model(Probit4)

# 5. Probit - belief, no outliers
Probit5 <- glm(formula = PostFodder ~ Treatment 
               + Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal, 
               family = binomial(link = "probit"), data = Data_Kenya_3FnOL_1)
tab_model(Probit5)

# 6. Probit - belief, full db
Probit6 <- glm(formula = PostFodder ~ Treatment 
               + Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal, 
               family = binomial(link = "probit"), data = Data_Kenya_3F_1)
tab_model(Probit6)

# 7. Probit - Belief vs No belief, only controls, no outliers
Probit7 <- glm(formula = PostFodder ~ BeliefYN 
               + Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal, 
               family = binomial(link = "probit"), data = Data_Kenya_3FnOL_C)
tab_model(Probit7)

# 8. Probit - Belief vs No belief, only controls, full db
Probit8 <- glm(formula = PostFodder ~ BeliefYN 
               + Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal, 
               family = binomial(link = "probit"), data = Data_Kenya_3F_C)
tab_model(Probit8)

# 9. Probit - Belief vs No belief, only treated, no outliers
Probit9 <- glm(formula = PostFodder ~ BeliefYN 
               + Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal, 
               family = binomial(link = "probit"), data = Data_Kenya_3FnOL_T)
tab_model(Probit9)

# 10. Probit - Belief vs No belief, only treated, full db
Probit10 <- glm(formula = PostFodder ~ BeliefYN 
               + Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal, 
               family = binomial(link = "probit"), data = Data_Kenya_3F_T)
tab_model(Probit10)

# 11. Logit - no outliers
Logit1 <- glm(formula = PostFodder ~ Treatment 
               + Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal, 
               family = binomial(link = "logit"), data = Data_Kenya_3FnOL)
tab_model(Logit1)

# 12. Logit - full db
Logit2 <- glm(formula = PostFodder ~ Treatment 
               + Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal, 
               family = binomial(link = "logit"), data = Data_Kenya_3F)
tab_model(Logit2)

# 13. Logit - no belief, no outliers
Logit3 <- glm(formula = PostFodder ~ Treatment 
               + Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal, 
               family = binomial(link = "logit"), data = Data_Kenya_3FnOL_0)
tab_model(Logit3)

# 14. Logit - no belief, full db
Logit4 <- glm(formula = PostFodder ~ Treatment 
               + Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal, 
               family = binomial(link = "logit"), data = Data_Kenya_3F_0)
tab_model(Logit4)

# 15. Logit - belief, no outliers
Logit5 <- glm(formula = PostFodder ~ Treatment 
               + Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal, 
               family = binomial(link = "logit"), data = Data_Kenya_3FnOL_1)
tab_model(Logit5)

# 16. Logit - belief, full db
Logit6 <- glm(formula = PostFodder ~ Treatment 
               + Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal, 
               family = binomial(link = "logit"), data = Data_Kenya_3F_1)
tab_model(Logit6)

# 17. Logit - Belief vs No belief, only controls, no outliers
Logit7 <- glm(formula = PostFodder ~ BeliefYN 
               + Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal, 
               family = binomial(link = "logit"), data = Data_Kenya_3FnOL_C)
tab_model(Logit7)

# 18. Logit - Belief vs No belief, only controls, full db
Logit8 <- glm(formula = PostFodder ~ BeliefYN 
               + Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal, 
               family = binomial(link = "logit"), data = Data_Kenya_3F_C)
tab_model(Logit8)

# 19. Logit - Belief vs No belief, treated, no outliers
Logit9 <- glm(formula = PostFodder ~ BeliefYN 
               + Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal, 
               family = binomial(link = "logit"), data = Data_Kenya_3FnOL_T)
tab_model(Logit9)

# 20. Logit - Belief vs No belief, treated, full db
Logit10 <- glm(formula = PostFodder ~ BeliefYN 
               + Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal, 
               family = binomial(link = "logit"), data = Data_Kenya_3F_T)
tab_model(Logit10)


# 31. mco - no outliers
mco1 <- glm(formula = PostFodder ~ Treatment 
              + Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal, 
              family = "gaussian", data = Data_Kenya_3FnOL)
tab_model(mco1)

# 12. mco - full db
mco2 <- glm(formula = PostFodder ~ Treatment 
              + Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal, 
              family = "gaussian", data = Data_Kenya_3F)
tab_model(mco2)

# 13. mco - no belief, no outliers
mco3 <- glm(formula = PostFodder ~ Treatment 
              + Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal, 
              family = "gaussian", data = Data_Kenya_3FnOL_0)
tab_model(mco3)

# 14. mco - no belief, full db
mco4 <- glm(formula = PostFodder ~ Treatment 
              + Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal, 
              family = "gaussian", data = Data_Kenya_3F_0)
tab_model(mco4)

# 15. mco - belief, no outliers
mco5 <- glm(formula = PostFodder ~ Treatment 
              + Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal, 
              family = "gaussian", data = Data_Kenya_3FnOL_1)
tab_model(mco5)

# 16. mco - belief, full db
mco6 <- glm(formula = PostFodder ~ Treatment 
              + Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal, 
              family = "gaussian", data = Data_Kenya_3F_1)
tab_model(mco6)

# 17. mco - Belief vs No belief, only controls, no outliers
mco7 <- glm(formula = PostFodder ~ BeliefYN 
              + Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal, 
              family = "gaussian", data = Data_Kenya_3FnOL_C)
tab_model(mco7)

# 18. mco - Belief vs No belief, only controls, full db
mco8 <- glm(formula = PostFodder ~ BeliefYN 
              + Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal, 
              family = "gaussian", data = Data_Kenya_3F_C)
tab_model(mco8)

# 19. mco - Belief vs No belief, treated, no outliers
mco9 <- glm(formula = PostFodder ~ BeliefYN 
              + Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal, 
              family = "gaussian", data = Data_Kenya_3FnOL_T)
tab_model(mco9)

# 20. mco - Belief vs No belief, treated, full db
mco10 <- glm(formula = PostFodder ~ BeliefYN 
               + Age +	Gender +	Education +	Marriage +	Workers	+ LandSize	+ Legal, 
               family = "gaussian", data = Data_Kenya_3F_T)
tab_model(mco10)

export_summs(Probit1, Probit2, Probit3, Probit4, Probit5, Probit6)
export_summs(Logit1, Logit2, Logit3, Logit4, Logit5, Logit6)
export_summs(mco1, mco2, mco3, mco4, mco5, mco6)

