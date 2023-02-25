library(car)
library(readxl)

## Appropriate ANOVA

App <- read_excel("C:/Users/dlsch/OneDrive - University of California, Davis/Academics/Sensory/Projects/Energy Bar/Nonconscious Effects of Context on Energy Bar Perception/Data Analysis/Preliminary Data/Prelim Data.xlsx", sheet = "App")

#creating factors
App[,c(1:5)]<-lapply(App[,c(1:5)],factor)
str(App)

#ANOVA
options(contrasts = c("contr.sum", "contr.poly"))
App.aov <-  aov(Appropriateness ~ (Product+Context+User)^2, App)
summary(App.aov)

write.csv(App.aov, "Energy_Bar_DA_3way_ANOVA_2way_Int.csv")


## Hedonic ANOVA

Like <- read_excel("C:/Users/dlsch/OneDrive - University of California, Davis/Academics/Sensory/Projects/Energy Bar/Nonconscious Effects of Context on Energy Bar Perception/Data Analysis/Preliminary Data/Prelim Data.xlsx", sheet = "Like")

#creating factors
Like[,c(1:5)]<-lapply(Like[,c(1:5)],factor)
str(Like)

#ANOVA
options(contrasts = c("contr.sum", "contr.poly"))
Like.aov <-  aov(Liking ~ (Product+Context+User)^2, Like)
summary(Like.aov)

write.csv(Like.aov, "Energy_Bar_DA_3way_ANOVA_2way_Int.csv")

