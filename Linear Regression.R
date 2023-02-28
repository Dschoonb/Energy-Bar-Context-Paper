All <- read_excel("C:/Users/dlsch/OneDrive - University of California, Davis/Academics/Sensory/Projects/Energy Bar/Nonconscious Effects of Context on Energy Bar Perception/Data Analysis/Preliminary Data/Prelim Data.xlsx", sheet = "All")

lm = lm(Liking~Appropriateness, data = All)
summary(lm)

lm = lm(Appropriateness~Liking, data = All)
summary(lm)

plot(lm$residuals, pch = 16, col = "red")
