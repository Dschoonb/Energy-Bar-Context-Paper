All <- read_excel("Prelim Data.xlsx")

## Appropriateness uncentered
#Create means
App = as.matrix(All[, -c(1:6,8)])
app.means = aggregate(App ~ Panelist, data = All, FUN = mean)
row.names(app.means) = app.means$Panelist
app.means$Panelist = NULL
#app.meansscaled <- scale(app.means)

###Similarity Matrix "euclidean"
app.disteuc <- dist(app.means, method = "euclidean")

##Ward Euclidean
app.wardeu <- hclust(app.disteuc, method="ward.D")
plot(da.wardeu)
groups.wardeu <- cutree(da.wardeu, k=2)
rect.hclust(da.wardeu, k=2, border="blue")

groups = data.frame(groups.wardeu)
groups$Product = row.names(groups)
groups

da.wclus= merge(SblancConsumer_, groups, by = "Product")
head(da.wclus)

da.wclus$groups.wardeu =as.factor(da.wclus$groups.wardeu)
da.a=as.matrix(da.wclus [,-c(1:2,ncol(da.wclus))])
head(da.a)

da.aov1=aov(da.a~groups.wardeu, data=da.wclus)
summary(da.aov1)

##Liking centered
