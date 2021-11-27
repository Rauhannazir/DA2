
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





