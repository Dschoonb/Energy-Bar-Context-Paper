library(car)
library(readxl)
library(agricolae)
library(lme4)

##Appropriate Scores ANOVA
All <- read_excel("C:/Users/dlsch/OneDrive - University of California, Davis/Academics/Sensory/Projects/Energy Bar/Nonconscious Effects of Context on Energy Bar Perception/Data Analysis/Preliminary Data/Prelim Data.xlsx", sheet = "All")

#creating factors
All[,c(1:6)]<-lapply(All[,c(1:6)],factor)
str(All)

#Changing data from tibble to data frame for ANOVA
All.df <- as.data.frame(All)

#ANOVA-Appropriateness
App.model <- lm(data = All.df, Appropriateness~ (Context + `User Status` + Product + `User Status`*Time + Context*`User Status`*Time))
options(contrasts = c("contr.sum", "contr.poly"))
App.aov <-  Anova(App.model,type = "III")
print(App.aov)

HSD.test(App.model, "Context", console = TRUE)

#ANOVA-Liking
Like.model <- lm(data = All.df, Liking~ (Context + `User Status` + Time*Product))
options(contrasts = c("contr.sum", "contr.poly"))
Like.aov <-  Anova(Like.model,type = "III")
print(Like.aov)

HSD.test(Like.model, "Context", console = TRUE)
