
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
