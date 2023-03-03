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



##Appropriateness
#Full Data Set Model - Panelist Effect
App.model <- lm(data = All, Appropriateness~ (Context + `User Status` + Product + Order + Order*Product))
options(contrasts = c("contr.sum", "contr.poly"))
App.aov <-  Anova(App.model,type = "III")
print(App.aov)

HSD.test(App.model, "Context", console = TRUE)

#Full Data Set Model + Panelist Effect - Context
App.model.panel <- lm(data = All, Appropriateness~ (Panelist + `User Status` + Product + Order + Order*Product + Context*Product))
options(contrasts = c("contr.sum", "contr.poly"))
App.aov.panel <-  Anova(App.model.panel,type = "III")
print(App.aov.panel)

HSD.test(App.model.panel, "Context", console = TRUE)

#Freq User Model - Panelist Effect
App.model.freq <- lm(data = Users_freq, Appropriateness~ (Context + Product))
options(contrasts = c("contr.sum", "contr.poly"))
App.aov.freq <-  Anova(App.model.freq,type = "III")
print(App.aov.freq)

HSD.test(App.model.freq, "Context", console = TRUE)

#Freq User Model + Panelist Effect - Context 
App.model.freq.panel <- lm(data = Users_freq, Appropriateness~ (Panelist + Product + Order))
options(contrasts = c("contr.sum", "contr.poly"))
App.aov.freq.panel <-  Anova(App.model.freq.panel,type = "III")
print(App.aov.freq.panel)

HSD.test(App.model.freq.panel, "Product", console = TRUE)

#Infreq User Model - Panelist Effect
App.model.infreq <- lm(data = Users_infreq, Appropriateness~ (Context + Product + Order))
options(contrasts = c("contr.sum", "contr.poly"))
App.aov.infreq <-  Anova(App.model.infreq,type = "III")
print(App.aov.infreq)

HSD.test(App.model.infreq, "Product", console = TRUE)

#Infreq User Model + Panelist Effect - Context 
App.model.infreq.panel <- lm(data = Users_infreq, Appropriateness~ (Panelist + Product + Order))
options(contrasts = c("contr.sum", "contr.poly"))
App.aov.infreq.panel <-  Anova(App.model.infreq.panel,type = "III")
print(App.aov.infreq.panel)

HSD.test(App.model.infreq.panel, "Product", console = TRUE)



##Liking
#Full Data Set Model - Panelist Effect
Like.model <- lm(data = All, Liking~ (Context + `User Status`+ Product + Order + Product*Order))
options(contrasts = c("contr.sum", "contr.poly"))
Like.aov <-  Anova(Like.model,type = "III")
print(Like.aov)

HSD.test(Like.model, "Context", console = TRUE)

#Full Data Set Model + Panelist Effect - Context - User Status
App.model.panel <- lm(data = All, Appropriateness~ (Panelist + Product + Order))
options(contrasts = c("contr.sum", "contr.poly"))
App.aov.panel <-  Anova(App.model.panel,type = "III")
print(App.aov.panel)

HSD.test(App.model, "Product", console = TRUE)

#Freq User Model - Panelist Effect
Like.model.freq <- lm(data = Users_freq, Liking~ (Context + Product*Order))
options(contrasts = c("contr.sum", "contr.poly"))
Like.aov.freq <-  Anova(Like.model.freq,type = "III")
print(Like.aov.freq)

HSD.test(Like.model.freq, "Context", console = TRUE)

#Freq User Model + Panelist Effect - Context 
Like.model.freq.panel <- lm(data = Users_freq, Liking~ (Panelist + Product + Order))
options(contrasts = c("contr.sum", "contr.poly"))
Like.aov.freq.panel <-  Anova(Like.model.freq.panel,type = "III")
print(Like.aov.freq.panel)

HSD.test(Like.model.freq.panel, "Product", console = TRUE)

#Infreq User Model - Panelist Effect
Like.model.infreq <- lm(data = Users_infreq, Liking~ (Product*Order))
options(contrasts = c("contr.sum", "contr.poly"))
Like.aov.infreq <-  Anova(Like.model.infreq,type = "III")
print(Like.aov.infreq)

HSD.test(Like.model.infreq, "Context", console = TRUE)

#Infreq User Model + Panelist Effect - Context 
Like.model.infreq.panel <- lm(data = Users_infreq, Liking~ (Panelist + Product + Order))
options(contrasts = c("contr.sum", "contr.poly"))
Like.aov.infreq.panel <-  Anova(Like.model.infreq.panel,type = "III")
print(Like.aov.infreq.panel)

HSD.test(Like.model.infreq.panel, "Product", console = TRUE)






