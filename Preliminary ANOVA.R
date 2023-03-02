library(car)
library(readxl)
library(agricolae)
library(lme4)
library(broom)

##Appropriate Scores ANOVA
All <- read_excel("Prelim Data.xlsx")


#creating factors
All[,c(1:6)]<-lapply(All[,c(1:6)],factor)
str(All)


Users_freq <- filter(All, "Frequent" == `User Status`)
Users_infreq <- filter(All, "Infrequent" == `User Status`)

1

#ANOVA-Appropriateness


App.model <- lm(data = All, Appropriateness~ (Context + `User Status` + Product + `User Status`*Time + Context*`User Status`*Time))
options(contrasts = c("contr.sum", "contr.poly"))
App.aov <-  Anova(App.model,type = "III")
print(App.aov)

HSD.test(App.model, "Context", console = TRUE)


App.model2 <- lm(data = All, Appropriateness~ (Context + `User Status` + Product + Order + Order*Product)) #+ Context*`User Status` + Context*Product + Context*Product*`User Status`))
options(contrasts = c("contr.sum", "contr.poly"))
App.aov2 <-  Anova(App.model2,type = "III")
print(App.aov2)

HSD.test(App.model2, "Context", console = TRUE)

#ANOVA-Appropriateness --- NOT NESTED YET
App.model <- lm(data = All, Appropriateness~ (Context + Product + Order + Order*Product + Context*Product))
options(contrasts = c("contr.sum", "contr.poly"))
App.aov <-  Anova(App.model,type = "III")
print(App.aov)

App.model.freq <- lm(data = Users_freq, Appropriateness~ (Context + Product + Order + Order*Product + Context*Product))
options(contrasts = c("contr.sum", "contr.poly"))
App.aov.freq <-  Anova(App.model.freq,type = "III")
print(App.aov.freq)

App.model.infreq <- lm(data = Users_infreq, Appropriateness~ (Context + Product + Order + Order*Product + Context*Product))
options(contrasts = c("contr.sum", "contr.poly"))
App.aov.infreq <-  Anova(App.model.infreq,type = "III")
print(App.aov.infreq)


#ANOVA-Liking
Like.model <- lm(data = All, Liking~ (Context + `User Status` + Time*Product))
options(contrasts = c("contr.sum", "contr.poly"))
Like.aov <-  Anova(Like.model,type = "III")
print(Like.aov)

HSD.test(Like.model, "Context", console = TRUE)



Like.model <- lm(data = All, Liking~ (Context + Product + Order + Order*Product + Context*Product))
options(contrasts = c("contr.sum", "contr.poly"))
Like.aov <-  Anova(Like.model,type = "III")
print(Like.aov)

Like.model.freq <- lm(data = Users_freq, Liking~ (Context + Product + Order + Order*Product + Context*Product))
options(contrasts = c("contr.sum", "contr.poly"))
Like.aov.freq <-  Anova(Like.model.freq,type = "III")
print(App.aov.freq)