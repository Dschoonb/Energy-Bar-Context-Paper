library(car)
library(readxl)
library(agricolae)

##Appropriate Scores ANOVA
App <- read_excel("C:/Users/dlsch/OneDrive - University of California, Davis/Academics/Sensory/Projects/Energy Bar/Nonconscious Effects of Context on Energy Bar Perception/Data Analysis/Preliminary Data/Prelim Data.xlsx", sheet = "App")

#creating factors
App[,c(1:5)]<-lapply(App[,c(1:5)],factor)
str(App)

#Changing data from tibble to data frame for ANOVA
App.df <- as.data.frame(App)

#ANOVA
#Time is unbalanced
#Non significant interactions were removed and the test was ran again
App.model <- lm(data = App.df, Appropriateness~(Product+Context+User))
options(contrasts = c("contr.sum", "contr.poly"))
App.aov <-  Anova(App.model,type = "III")
print(App.aov)

#Approp. Post-Hoc Test

##Liking Scores ANOVA
Like <- read_excel("C:/Users/dlsch/OneDrive - University of California, Davis/Academics/Sensory/Projects/Energy Bar/Nonconscious Effects of Context on Energy Bar Perception/Data Analysis/Preliminary Data/Prelim Data.xlsx", sheet = "Like")

#creating factors
Like[,c(1:5)]<-lapply(Like[,c(1:5)],factor)
str(Like)

#Changing data from tibble to data frame for ANOVA
Like.df <- as.data.frame(Like)

#ANOVA
#Time is unbalanced
#Non significant interactions were removed and the test was ran again
Like.model <- lm(data = Like.df, Liking~(Product+Time)^2)
options(contrasts = c("contr.sum", "contr.poly"))
Like.aov <-  Anova(Like.model,type = "III")
print(Like.aov)

#Like Approp. Post-Hoc Test

