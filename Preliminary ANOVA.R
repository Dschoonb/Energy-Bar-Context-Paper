library(car)
library(readxl)
library(agricolae)
library(lme4)
library(broom)
library(dplyr)

##Appropriate Scores ANOVA
All <- read_excel("Prelim Data.xlsx")

#creating factors
All[,c(1:6)]<-lapply(All[,c(1:6)],factor)
str(All)

#Split the orginal data set by user status
Users_freq <-  filter(All, "Frequent" == `User Status`)
Users_infreq <-  filter(All, "Infrequent" == `User Status`)

#Split the orginal data set by Context
Context_Office_Break <-  filter(All, "Office Break" == Context)
Context_Pre_Workout <-  filter(All, "Pre-Workout" == Context)
Context_Outdoor_Break <-  filter(All, "Outdoor Break" == Context)


###Contextwise ANOVAs
##Appropriateness
#Office Break
App.officebreak.model <- lm(data = Context_Office_Break, Appropriateness~ (Panelist + Product + Order))
options(contrasts = c("contr.sum", "contr.poly"))
App.officebreak.aov <-  Anova(App.officebreak.model,type = "III")
print(App.officebreak.aov)
HSD.test(App.officebreak.model, "Product", console = TRUE)

#Outdoor Break
App.outdoorbreak.model <- lm(data = Context_Outdoor_Break, Appropriateness~ (Panelist + Product + Order))
options(contrasts = c("contr.sum", "contr.poly"))
App.outdoorbreak.aov <-  Anova(App.outdoorbreak.model,type = "III")
print(App.outdoorbreak.aov)
HSD.test(App.outdoorbreak.model, "Product", console = TRUE)

#Pre-Workout
App.preworkout.model <- lm(data = Context_Pre_Workout, Appropriateness~ (Panelist + Product))
options(contrasts = c("contr.sum", "contr.poly"))
App.preworkout.aov <-  Anova(App.preworkout.model,type = "III")
print(App.preworkout.aov)
HSD.test(App.officebreak.model, "Product", console = TRUE)

##Liking
#Office Break
Like.officebreak.model <- lm(data = Context_Office_Break, Liking~ (Panelist + Product + Order))
options(contrasts = c("contr.sum", "contr.poly"))
Like.officebreak.aov <-  Anova(Like.officebreak.model,type = "III")
print(Like.officebreak.aov)
HSD.test(Like.officebreak.model, "Product", console = TRUE)

#Outdoor Break
Like.outdoorbreak.model <- lm(data = Context_Outdoor_Break, Liking~ (Panelist + Product + Order))
options(contrasts = c("contr.sum", "contr.poly"))
Like.outdoorbreak.aov <-  Anova(Like.outdoorbreak.model,type = "III")
print(Like.outdoorbreak.aov)
HSD.test(Like.outdoorbreak.model, "Product", console = TRUE)

#Pre-Workout
Like.preworkout.model <- lm(data = Context_Pre_Workout, Liking~ (Panelist + Product + Order))
options(contrasts = c("contr.sum", "contr.poly"))
Like.preworkout.aov <-  Anova(Like.preworkout.model,type = "III")
print(Like.preworkout.aov)
HSD.test(Like.preworkout.model, "Product", console = TRUE)


##Appropriateness ANOVAs
#Full Data Set Model
App.model <- lm(data = All, Appropriateness~ (Context + `User Status` + Product + Order + Order*Product))
options(contrasts = c("contr.sum", "contr.poly"))
App.aov <-  Anova(App.model,type = "III")
print(App.aov)
HSD.test(App.model, "Context", console = TRUE)

#Freq User Model
App.model.freq <- lm(data = Users_freq, Appropriateness~ (Context))
options(contrasts = c("contr.sum", "contr.poly"))
App.aov.freq <-  Anova(App.model.freq,type = "III")
print(App.aov.freq)
HSD.test(App.model.freq, "Product", console = TRUE)

#Infreq User Model
App.model.infreq <- lm(data = Users_infreq, Appropriateness~ (Product + Order))
options(contrasts = c("contr.sum", "contr.poly"))
App.aov.infreq <-  Anova(App.model.infreq,type = "III")
print(App.aov.infreq)
HSD.test(App.model.infreq, "Product", console = TRUE)


##Liking ANOVAs
#Full Data Set Model
Like.model <- lm(data = All, Liking~ (Context + Product + Order + Product*Order))
options(contrasts = c("contr.sum", "contr.poly"))
Like.aov <-  Anova(Like.model,type = "III")
print(Like.aov)
HSD.test(Like.model, "Context", console = TRUE)

#Freq User Model
Like.model.freq <- lm(data = Users_freq, Liking~ (Product*Order))
options(contrasts = c("contr.sum", "contr.poly"))
Like.aov.freq <-  Anova(Like.model.freq,type = "III")
print(Like.aov.freq)
HSD.test(Like.model.freq, "Product", console = TRUE)

#Infreq User Model
Like.model.infreq <- lm(data = Users_infreq, Liking~ (Product*Order))
options(contrasts = c("contr.sum", "contr.poly"))
Like.aov.infreq <-  Anova(Like.model.infreq,type = "III")
print(Like.aov.infreq)
HSD.test(Like.model.infreq, "Product", console = TRUE)

