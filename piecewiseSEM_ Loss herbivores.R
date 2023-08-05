###########################################################################################################
#The model with and without herbivores, all variables, except precipitation, were transformed using the log10 + 1 transformation to ensure normality. The R script includes lineal model mixed were the random parameters for the plots and the consideration of temporal autocorrelation by year.  The corresponding codes used in the analysis are as follows: q0_nat_p1: Native plant richness, q0_ex_p1: Exotic plant richness, q01_ex_s1: Exotic seed richness of the next year, q01_ex_s1: Native seed richness of the next year, Abun_micro1: Abundance annual of O. degus, Cover_phli1: Cover annual of  P. hispidula, pp_mean: Mean annual precipitation.

library(tidyverse)
library(ggplot2)
library(gamm4)
library(lme4)
sem_gamm
res<-sem_gamm
names(res)
df <- res %>%
  mutate(Abun_micro1=log(Abun_micro +1),
         Cover_phli1=log (Cover_phli +1),
         q0_nat_p1 = log(q0_nat_p + 1),
         q0_ex_p1 = log(q0_ex_p + 1),
         q0_nat_s1 = log(q0_nat_s + 1),
         q0_ex_s1 = log(q0_ex_s + 1),
         q1_nat_p1 = log(q1_nat_p + 1),
         q1_ex_p1 = log(q1_ex_p + 1),
         q1_nat_s1 = log(q0_nat_s + 1),
         q1_ex_s1 = log(q1_ex_s + 1),
         q01_nat_s1 = log(q01_nat_s + 1),
         q11_nat_s1 = log(q11_nat_s + 1),
         q01_ex_s1 = log(q01_ex_s + 1),
         q11_ex_s1 = log(q11_ex_s + 1)) %>%
  select(. ,Trat,Year,subplot, pp_mean,
         q0_nat_s1, q0_ex_s1, q0_ex_p1, q0_nat_p1,
         q1_nat_p1, q1_ex_p1, q1_nat_s1, q1_ex_s1,
         q0_nat_s, q0_ex_s, q0_ex_p, q0_nat_p,
         q1_nat_p, q1_ex_p, q1_nat_s, q1_ex_s,
         q01_nat_s1,q11_nat_s1,q01_ex_s1,q11_ex_s1,
         q01_nat_s,q11_nat_s,q01_ex_s,q11_ex_s,
         Abun_micro1, Cover_phli1)
df

names(df)
### data normality without lac in seed
dfH<-subset(df,df$Trat=="H")
dfH

dfWH<-subset(df,df$Trat=="WH")
dfWH



#####  data normality with lac in seed 
df1<-df[1:80,]
df1


df2H<-subset(df1,df1$Trat=="H")
df2H

df2WH<-subset(df1,df1$Trat=="WH")
df2WH


####### Model:MODELO 0_ whitout herbivores, lac seed and normality 
library(piecewiseSEM)
library(ggplot2)
modelo_q0_nat_p <- lme(
  q0_nat_p1 ~ pp_mean+ Abun_micro1,
  random = ~1 | subplot,
  correlation = corAR1(form = ~ Year | subplot),
  data = df2WH)
modelo_q0_nat_s<- lme(
  q01_nat_s1~ pp_mean+ Abun_micro1+Cover_phli1,
  random = ~1 | subplot,
  correlation = corAR1(form = ~ Year | subplot),
  data = df2WH)
modelo_q0_nat_s<- lme(
  q01_nat_s1~ pp_mean+ Abun_micro1+Cover_phli1+  q01_ex_s1 + q0_nat_p1 ,
  random = ~1 | subplot,
  correlation = corAR1(form = ~ Year | subplot),
  data = df2WH)
modelo_Cover_phli1 <- lme(
  Cover_phli1 ~ pp_mean+ Abun_micro1,
  random = ~1 | subplot,
  correlation = corAR1(form = ~ Year | subplot),
  data = df2WH)

modelo0_lme_H <- psem(
  modelo_q0_nat_p,
  modelo_q0_ex_p,
  modelo_q0_nat_s,
  modelo_Cover_phli1
)

summary(modelo0_lme_WH)
WH<-plot(modelo0_lme_WH)
WH

####### Model1 : MODELO 0_ with herbivores, lac seed and fitand normality in all variables
library(piecewiseSEM)
library(ggplot2)

modelo_q0_nat_p <- lme(
  q0_nat_p1 ~ pp_mean + Abun_micro1+Cover_phli1,
  random = ~1 | subplot,
  correlation = corAR1(form = ~ Year | subplot),
  data = df2H)
modelo_q0_ex_p <- lme(
  q0_ex_p1 ~ pp_mean + Abun_micro1+Cover_phli1+ q01_ex_s1 + q0_nat_p1,
  random = ~1 | subplot,
  correlation = corAR1(form = ~ Year | subplot),
  data = df2H)
modelo_q0_nat_s <- lme(
  q01_nat_s1 ~ pp_mean + Abun_micro1+Cover_phli1+ q01_ex_s1 + q0_nat_p1+q0_ex_p1,
  random = ~1 | subplot,
  correlation = corAR1(form = ~ Year | subplot),
  data = df2H)

modelo_Cover_phli1 <- lme(
  Cover_phli1 ~ pp_mean+ Abun_micro1,
  random = ~1 | subplot,
  correlation = corAR1(form = ~ Year | subplot),
  data = df2H)

modelo0_lme_H <- psem(
  modelo_q0_nat_p,
  modelo_q0_ex_p,
  modelo_q0_nat_s,
  modelo_Cover_phli1
)

summary(modelo0_lme_H)
H<-plot(modelo0_lme_H, layout = "circle")
H

