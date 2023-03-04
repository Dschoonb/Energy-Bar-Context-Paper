library(tidyverse)
library(RColorBrewer)
library(readr)
library(dplyr)
library(ggsignif)
library(plyr)

#Appropriateness
All <- read_excel("C:/Users/dlsch/OneDrive - University of California, Davis/Academics/Sensory/Projects/Energy Bar/Nonconscious Effects of Context on Energy Bar Perception/Data Analysis/Preliminary Data/Prelim Data.xlsx", sheet = "All")

All$Context <- as.factor(All$Context)
str(All)

Users_freq <-  filter(All, "Frequent" == `User Status`)
Users_infreq <-  filter(All, "Infrequent" == `User Status`)

#generating data sets w/ means and standard deviations for bar plot error bars
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

App.All.sd<- data_summary(All, varname="Appropriateness",
                    groupnames=c("Context", "Product"))
Like.All.sd <- data_summary(All, varname="Liking",
                            groupnames=c("Context", "Product"))

App.freq.sd <- data_summary(Users_freq, varname="Appropriateness",
                            groupnames=c("Context", "Product"))
Like.freq.sd <- data_summary(Users_freq, varname="Liking",
                            groupnames=c("Context", "Product"))

App.infreq.sd <- data_summary(Users_infreq, varname="Appropriateness",
                            groupnames=c("Context", "Product"))
Like.infreq.sd <- data_summary(Users_infreq, varname="Liking",
                            groupnames=c("Context", "Product"))

##Full Data Set
#Approp.
gg.App.Full <- ggplot(data = App.sd.data, aes(x=Product,y=Appropriateness)) +
  geom_col() +
  facet_grid(cols = vars(Context)) +
  geom_errorbar(aes(ymax =Appropriateness+sd, ymin=Appropriateness-sd), width=.2,
                position=position_dodge(.9)) 
gg.App.Full

#Liking
gg.Like.Full <- ggplot(data = Like.All.sd, aes(x=Product,y=Liking)) +
  geom_col() +
  facet_grid(cols = vars(Context)) +
  geom_errorbar(aes(ymax =Liking+sd, ymin=Liking-sd), width=.2,
                position=position_dodge(.9)) 
gg.Like.Full

##Freq DS
#Approp.
gg.App.freq <- ggplot(data = App.freq.sd, aes(x=Product,y=Appropriateness)) +
    geom_col() +
    facet_grid(cols = vars(Context)) +
  geom_errorbar(aes(ymax =Appropriateness+sd, ymin=Appropriateness-sd), width=.2,
                position=position_dodge(.9)) 
gg.App.freq

#Liking
gg.Like.freq <- ggplot(data = Like.freq.sd, aes(x=Product,y=Liking)) +
  geom_col() +
  facet_grid(cols = vars(Context)) +
  geom_errorbar(aes(ymax =Liking+sd, ymin=Liking-sd), width=.2,
                position=position_dodge(.9)) 
gg.Like.freq

##Infreq DS
#Approp.
gg.App.infreq <- ggplot(data = App.infreq.sd, aes(x=Product,y=Appropriateness)) +
  geom_col() +
  facet_grid(cols = vars(Context)) +
  geom_errorbar(aes(ymax =Appropriateness+sd, ymin=Appropriateness-sd), width=.2,
                position=position_dodge(.9)) 
gg.App.infreq

#Liking
gg.like.infreq <- ggplot(data = Like.infreq.sd, aes(x=Product,y=Liking)) +
  geom_col() +
  facet_grid(cols = vars(Context)) +
  geom_errorbar(aes(ymax =Liking+sd, ymin=Liking-sd), width=.2,
                position=position_dodge(.9)) 
gg.like.infreq
