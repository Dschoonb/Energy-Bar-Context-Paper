library(tidyverse)
library(RColorBrewer)
library(readr)
library(dplyr)
library(ggsignif)

#Appropriateness
App <- read_excel("C:/Users/dlsch/OneDrive - University of California, Davis/Academics/Sensory/Projects/Energy Bar/Nonconscious Effects of Context on Energy Bar Perception/Data Analysis/Preliminary Data/Prelim Data.xlsx", sheet = "App")

App$Context <- as.factor(App$Context)

str(App)
gg <- ggplot(App) + 
  geom_boxplot(aes(x=Context,y=Appropriateness))

gg

##Liking
Like <- read_excel("C:/Users/dlsch/OneDrive - University of California, Davis/Academics/Sensory/Projects/Energy Bar/Nonconscious Effects of Context on Energy Bar Perception/Data Analysis/Preliminary Data/Prelim Data.xlsx", sheet = "Like")

Like$Context <- as.factor(Like$Context)

str(Like)
