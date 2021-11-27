
rm(list = ls())

install.packages("huxtable")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("fixest")
install.packages("modelsummary")
install.packages("huxtable")


library(dplyr)
library(tidyverse)
library(fixest)
library(modelsummary)
library(huxtable)

data <- read_csv("https://osf.io/4ay9x/download")

#tried different occupations but at the end went with 0010- Chief Executives because had enough observations to make meaningful comparisons 
#  Using the occupational code 0010 - Chief executives
df <- data %>% filter(occ2012 == 0010 & uhours>=20 & earnwke>0 & age >= 17 & age<=64)
df <- df %>% filter(grade92 <= 46 & grade92 >= 41)

#to check the distribution according to grade level
ggplot(data=df,aes(x=grade92,y=earnwke))+geom_point() 
# Creating the wage/hour (wagehour) variable for comparison across different observations
df <- mutate(df, wagehour = earnwke/uhours)
head(df)
df <- mutate(df, female=as.numeric(sex==2))
summary(df)


# creating the log of wage per hour variable
df <- df %>% mutate(logw = log ( wagehour ))
datasummary(as.factor(female)*wagehour ~ Mean + SD + Min + Max + P25 + P75 + N , data = df)
reg0 <- feols(wagehour ~ female, data = df , vcov="hetero")
reg0
# The unconditional wage gap between men and women: men on average tend to earn around $4 more than women in the 
# Chief Executive jobs. This is shown by both the data summary (The difference in the mean values of male and female wage per hour) and the unconditional regression of wage per hour on female binary variable.



# plotting the distribution of wage per hour and log of wage per hour to see if we should run regression with the log of wage/hour or the absolute value of wage/hour.
ggplot(data = df, aes(x=wagehour))+
  geom_density()+
  xlab("log (wage/hour)")
ggplot(data = df, aes(x=logw))+
  geom_density()+
  xlab("log (wage/hour)")
# Since the distribution of wage per hour is closer to a normal distribution compared to log of wage per hour it makes more sense to use the absolute values rather than the log.

## Assigning degree names to grade92 variable based on the information provided on page 25 of the **[cpsx](https://osf.io/uqe8z/)** document provided.
- "ed_AD_V contains the individuals with education levels of associate degrees (Vocational/occupational)"
- "ed_AD_AP contains the individuals with education levels of associate degrees (Academic program)"
- "ed_BD contains the individuals with education levels of a Bachelor's degree (e.g.BA,AB,BS)"
- "ed_MD contains the individuals with education levels of a Masters degree (e.g.MA,MS,MEng,Med,MSW,MBA)"
- "ed_Profess contains the individuals with education of a professional degree (e.g.MD,DDS,DVM,LLB,JD)"
- "ed_PhD contains the individuals with education levels of a Doctorate degree (e.g.PhD,EdD)"



# Assigning values to grade92 based on the above information 
df <- df %>% mutate(ed_AD_V=as.numeric(grade92==41),
                    ed_AD_AP=as.numeric(grade92==42),
                    ed_BD=as.numeric(grade92==43),
                    ed_MD=as.numeric(grade92==44),
                    ed_Profess = as.numeric(grade92==45),
                    ed_PhD = as.numeric(grade92==46))

summary(df)



# Running a non-parametric regression of wagehour on education levels
ggplot(data = df, aes(x = grade92, y = wagehour )) +
  geom_smooth( color = "blue", method = "loess", formula = " y ~ x") +
  geom_point()+
  labs(title = "Non-parametric regression - wage per hour ~ education")+
  scale_x_continuous( expand=c(0.1, 0.1),  breaks=seq(41, 46,   by=1), 
                      labels = c("ed_AD_V","ed_AD_AP","ed_BD","ed_MD","ed_Profess","ed_PhD"))









