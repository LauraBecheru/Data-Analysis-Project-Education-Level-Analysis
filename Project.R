#READ DATA
data<-read.csv("Project.csv", header = TRUE, sep=";", row.names=1)
names(data)<-c("Enrolment","Expenditure","IncomeByEdu","Health",
               "Fertility","GNI","GDP","Unemployment")
#DISPLAY DATA
View(data)
str(data)

#PAIRS SCATTER PLOT
pairs(data)

#STANDARDIZATION
data_stand <- as.data.frame(scale(data))
View(data_stand)


#CORELATION MATRIX
cor_matrix<-cor(data_stand)
View(cor_matrix)

#DET PC
pca<-prcomp(data,center = TRUE,scale. = TRUE)
pca
summary(pca)

#PLOTS
screeplot(pca, type="lines")
biplot(pca)

pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)


pca.data <- data.frame(Sample=rownames(pca$x), X=pca$x[,1], Y=pca$x[,2])
pca.data



ggplot(d=pca.data, aes(x=X, y=Y, label=Sample)) + geom_text() + xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) + theme_bw() +  ggtitle("PCA Graph")