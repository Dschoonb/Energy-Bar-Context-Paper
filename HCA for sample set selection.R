library(readxl)

DA <- read_excel("C:/Users/dlsch/OneDrive - University of California, Davis/Academics/Sensory/Projects/Energy Bar/Determinants of Energy Bar Expectation 2022/Data Analysis/QDA/EBar_DA_DuplicatesRemoved.xlsx",sheet = "DA")

da.a = as.matrix(DA[, -c(1:7)])
da.means = aggregate(da.a ~ Product, data = DA, FUN = mean)
row.names(da.means) = da.means$Product
da.means$Product = NULL
da.meansscaled <- scale(da.means)

###Similarity Matrix "euclidean"
da.disteuc <- dist(da.meansscaled, method = "euclidean")

##Ward Euclidean
da.wardeu <- hclust(da.disteuc, method="ward.D")
plot(da.wardeu)
groups.wardeu <- cutree(da.wardeu, k=3)
rect.hclust(da.wardeu, k=3, border="blue")

groups <-  data.frame(groups.wardeu)
groups$Product <-  row.names(groups)
groups

da.wclus <- merge(DA, groups, by = "Product")
head(da.wclus)

da.wclus$groups.wardeu =as.factor(da.wclus$groups.wardeu)
da.a <- as.matrix(da.wclus [,-c(1:7,ncol(da.wclus))])
head(da.a)

da.aov <- aov(da.a~groups.wardeu, data=da.wclus)
summary(da.aov)
