# Descriptive Statistics 

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

Martinez_Glandel <- read_excel("~/Documents/Master Material/Experimental Econ/Martinez Glandel.xlsx")
View(Martinez_Glandel)   

## Get three Treatments: Control, Treatment and Not part of Experiment (already practicing agroforestry)

Data_Kenya <- Martinez_Glandel %>%
  mutate(Treatment = ifelse(AlreadyFodder == 1, 
                           3, Treatment))

## Drop observations where Treatment = 0 (Control)

Data_Kenya_0 <-subset(Data_Kenya,Treatment!="0")

## Drop observations where Treatment = 3 (already fodder)

Data_Kenya_3 <-subset(Data_Kenya,Treatment!="3")

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
