#Author :Anirudh Prabhu
#email : prabha2@rpi.edu

#Natural Kind clustering.

Gregory <- read.csv("~/Downloads/Gregory.csv")
Gregory[Gregory == 0] <- 0.000001
Gregory[, 3:14] <- log(Gregory[3:14], 2)

Greg_scale<-scale(Gregory[,-c(1:2)])

#Clustering

#Commence PAM Clustering !!!

# Calculating Gower Distance
library(cluster)
library(compiler)
#f<-function(Gregory)
#{
  gower_dist <- daisy(Gregory[,-c(1:2)],
                      metric = "gower",
                      type = list())
  
#  gower_dist <- daisy(Greg_scale,
#                      metric = "gower",
#                      type = list())
  #return(gower_dist)
#}

#fc <- cmpfun(f)

#system.time(try <- fc(Gregory))



gower_mat <- as.matrix(gower_dist)
#View(gower_mat)

summary(Gregory)
#Check for most similar minerals
Gregory[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

#Check for most dissimilar minerals
Gregory[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]


# Calculate Silhoutte width for the fit (selecting the optimum number of clusters)
sil_width <- c(NA)

for(i in 2:11){
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}

plot(sil_width,xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(sil_width)


# The actual PAM Clustering
pam_fit <- pam(gower_dist, diss = TRUE, k = 5)
pam_fit$clusinfo 

Gregory$Cluster <- pam_fit$clustering  
Gregory$Cluster <- as.factor(Gregory$Cluster)

counts <- table(Gregory$Deposit.style,Gregory$Cluster)
barplot(counts, main="Natural Kind Clustering",
        xlab="Number of Gears",
        , col=c("darkblue", "red", "black" , "pink", "green", "brown", "white"),  beside=T)
barplot(counts, main="Natural Kind Clustering",
        xlab="Number of Gears",
        , col=c("darkblue", "red", "black" , "pink", "green", "brown", "white"),  beside=T,legend = rownames(counts))
#legend("topright",legend = rownames(counts),cex = 0.7,fill = 1:6, ncol = 2)

clusplot(pam_fit)

#_________________________

library(randomForest)
rm <- randomForest(Deposit.style~.,data = Gregory[,-1])
rm$importance
varImpPlot(rm)

#__________________________
#MClust
#Model based clustering

library(mclust)
X<-Gregory[,-c(1:2)]
#X<-Greg_scale
BIC <- mclustBIC(X)
plot(BIC)
summary(BIC)

mod1 <- Mclust(X, x = BIC)
summary(mod1, parameters = TRUE)
plot(mod1, what = "classification")
cc<-table(Gregory$Deposit.style, mod1$classification)
table(Gregory$Deposit.style, mod1$classification)
barplot(cc, main="Natural Kind Clustering",
        xlab="Number of Gears",
        , col=c("darkblue", "red", "black" , "pink", "green", "brown", "white"),  beside=T)
#plot(table(mod1$classification,Gregory$Deposit.style))
plot(mod1, what = "uncertainty")

ICL <- mclustICL(X)
summary(ICL)
plot(ICL)

LRT <- mclustBootstrapLRT(X, modelName = "VEV")
