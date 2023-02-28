library(car)
library(readxl)

##Appropriate Scores ANOVA
App <- read_excel("C:/Users/dlsch/OneDrive - University of California, Davis/Academics/Sensory/Projects/Energy Bar/Nonconscious Effects of Context on Energy Bar Perception/Data Analysis/Preliminary Data/Prelim Data.xlsx", sheet = "App")

#creating factors
App[,c(1:5)]<-lapply(App[,c(1:5)],factor)
str(App)

#Changing data from tibble to data frame for ANOVA
App.df <- as.data.frame(App)

#ANOVA
App.model <- lm(data = App.df, Appropriateness~(Product+Context+Time+User)^3)
options(contrasts = c("contr.sum", "contr.poly"))
App.aov <-  Anova(App.model,type = "III")
print(App.aov)

#Tukey HSD
ap.mode <- aov(Appropriateness~Context, data = App)
TukeyHSD(ap.mode, conf.level=.95)



##Liking Scores ANOVA
Like <- read_excel("C:/Users/dlsch/OneDrive - University of California, Davis/Academics/Sensory/Projects/Energy Bar/Nonconscious Effects of Context on Energy Bar Perception/Data Analysis/Preliminary Data/Prelim Data.xlsx", sheet = "Like")

#creating factors
Like[,c(1:5)]<-lapply(Like[,c(1:5)],factor)
str(Like)

#Changing data from tibble to data frame for ANOVA
Like.df <- as.data.frame(Like)

#ANOVA
#Note Context was significant in Type 2 SS but not Type 3
Like.model <- lm(data = Like.df, Liking~(Product+Context+Time+User)^3)
options(contrasts = c("contr.sum", "contr.poly"))
Like.aov <-  Anova(Like.model,type = "III")
print(Like.aov)

#Tukey HSD
lk.mode <- aov(Liking~Context, data = Like)
TukeyHSD(lk.mode, conf.level=.95)
