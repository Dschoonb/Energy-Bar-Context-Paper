library(tidyverse)
library(RColorBrewer)
library(readr)
library(dplyr)
library(ggsignif)

#Appropriateness
All <- read_excel("C:/Users/dlsch/OneDrive - University of California, Davis/Academics/Sensory/Projects/Energy Bar/Nonconscious Effects of Context on Energy Bar Perception/Data Analysis/Preliminary Data/Prelim Data.xlsx", sheet = "All")

All$Context <- as.factor(All$Context)
str(All)

#Context-App
gg.App.con <- ggplot(All) + 
  geom_boxplot(aes(x=Product,y=Appropriateness)) +
  facet_grid(cols = vars(Context))

gg.App.con

#Freq-App
gg.App.freq <- ggplot(All) + 
  geom_boxplot(aes(x=`User Status`,y=Appropriateness)) +
  facet_grid(cols = vars(Context))

gg.App.freq

#Time-App
gg.App.time <- ggplot(All) + 
  geom_boxplot(aes(x=Time,y=Appropriateness)) +
  facet_grid(cols = vars(Context))

gg.App.time

#Context-Liking
gg.Like.con <- ggplot(All) + 
  geom_boxplot(aes(x=Product,y=Liking)) +
  facet_grid(cols = vars(Context))

gg.Like.con

#Freq-Like
gg.Like.freq <- ggplot(All) + 
  geom_boxplot(aes(x=`User Status`,y=Liking)) +
  facet_grid(cols = vars(Context))

gg.Like.freq

#Time-Like
gg.Like <- ggplot(All) + 
  geom_boxplot(aes(x=Time,y=Liking)) +
  facet_grid(cols = vars(Context))

gg.Like.time
