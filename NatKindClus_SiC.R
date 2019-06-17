#Author :Anirudh Prabhu
#email : prabha2@rpi.edu

#Natural Kind clustering.

Gregory <- read.csv("~/Downloads/Gregory.csv")
library(readxl)
New_Pyrite <- read_excel("~/Downloads/Appendix 1 data used in Random Forest analysis.xlsx")
#Pyrite_WithTexture <- read_excel("Downloads/Sed py thesis data.xlsx",sheet = 15)
#Pyrite_WithTexture_Sub <- Pyrite_WithTexture[,-c(16,18,2:7,9:14)]
#Pyrite_WithTexture_Sub$`Pyrite type` <- as.factor(Pyrite_WithTexture_Sub$`Pyrite type`)
Gregory <- New_Pyrite


Gregory[Gregory == 0] <- 0.001

Gregory[, 3:14] <- log(Gregory[3:14], 2)
#Gregory$`Deposit style` <- as.factor(Gregory$`Deposit style`)

#Gregory[Gregory$`Deposit style` == ""]

#Greg_scale<-scale(Gregory[,-c(1:2)])

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
pam_fit <- pam(gower_dist, diss = TRUE, k = 3)
pam_fit$clusinfo 

Gregory$Cluster <- pam_fit$clustering  
Gregory$Cluster <- as.factor(Gregory$Cluster)

counts <- table(Gregory$Deposit.style,Gregory$Cluster)
barplot(counts, main="Natural Kind Clustering",
        xlab="Number of Gears",
        , col=c("darkblue", "red", "black" , "pink", "green", "brown", "white"),  beside=T)
barplot(counts, main="Natural Kind Clustering",
        xlab="Number of Gears",
        , col=c("darkblue", "red", , "pink", "green", "brown", "white"),  beside=T,legend = rownames(counts))
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

#Gregory <- as.data.frame(Gregory)
#Gregory[is.na(Gregory)] <- -999

library(mclust)
X<-Gregory[,-c(1:2,8)]
#X<-Greg_scale
set.seed(2633)
BIC <- mclustBIC(X)
plot(BIC)
summary(BIC)

mod1 <- Mclust(X, x = BIC)
summary(mod1, parameters = TRUE)
plot(mod1, what = "classification")
cc<-table(Gregory$`Deposit style`, mod1$classification)
table(Gregory$`Deposit style`, mod1$classification)
#plot(table(mod1$classification,Gregory$Deposit.style))
plot(mod1, what = "uncertainty")

cc_df <- data.frame(cc) # this will change deposit sytle to factor

cc_df
library(ggplot2)
ggplot(cc_df) +
  geom_bar(aes(x = Var2, y = Freq, fill = Var1), stat = 'identity') +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Cluster",
       y = "Count") +
  guides(fill=guide_legend(title="Deposit type")) +
  theme_bw() +
  theme(aspect.ratio = 0.9,
        axis.text = element_text(size=8),
        axis.title = element_text(size=10),
        plot.margin = ggplot2::margin(l=0.5, t=0.5, b=0.3, r=0.5, unit = 'cm')
  )


ggplot(cc_df) +
  geom_bar(aes(x = Var2, y = Freq, fill = Var1), stat = 'identity', position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Cluster",
       y = "Count") +
  scale_y_continuous(labels = scales::percent_format()) +
  guides(fill=guide_legend(title="Deposit type")) +
  theme_bw() +
  theme(aspect.ratio = 0.9,
        axis.text = element_text(size=8),
        axis.title = element_text(size=10),
        plot.margin = ggplot2::margin(l=0.5, t=0.5, b=0.3, r=0.5, unit = 'cm')
  )

ggplot(cc_df) +
  geom_bar(aes(x = Var2, y = Freq, fill = Var1), stat = 'identity', position=position_dodge()) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Cluster",
       y = "Count") +
  guides(fill=guide_legend(title="Deposit type")) +
  theme_bw() +
  theme(aspect.ratio = 0.9,
        axis.text = element_text(size=8),
        axis.title = element_text(size=10),
        plot.margin = ggplot2::margin(l=0.5, t=0.5, b=0.3, r=0.5, unit = 'cm')
  )



ICL <- mclustICL(X)
summary(ICL)
plot(ICL)

LRT <- mclustBootstrapLRT(X, modelName = "VEV")

mod1$classification

#______________________________________

#Classification

Gregory$Cluster <- as.factor(mod1$classification)

Ross_PyriteClean <- read.csv("Downloads/pyrite_clean_V2.csv")
Ross_PyriteClean_sub <- Ross_PyriteClean[,c(3:7,9,11,12,14,15,19)]
summary(Ross_PyriteClean_sub)

#Ross_PyriteClean_sub[,1:11] <- as.data.frame(sapply( Ross_PyriteClean_sub[,1:11], as.numeric ))
#Ross_PyriteClean_sub$Ni60<-as.numeric(Ross_PyriteClean_sub$Ni60)
#Ross_PyriteClean_sub$Zn66<-as.numeric(Ross_PyriteClean_sub$Zn66)
#Ross_PyriteClean_sub$Au197<-as.numeric(Ross_PyriteClean_sub$Au197)
#Ross_PyriteClean_sub$Tl205<-as.numeric(Ross_PyriteClean_sub$Tl205)

Ross_PyriteClean_sub[is.na(Ross_PyriteClean_sub)] <- 0.001
Ross_PyriteClean_sub[,1:11] <- log(Ross_PyriteClean_sub[,1:11], 2)


library(randomForest)
rm2 <- randomForest(Cluster~.,data = Gregory[,-c(1,2,8)])
rm2$importance
varImpPlot(rm2)
rm2
#Ross_PyriteClean_sub$Cluster <- Gregory$Cluster[1:1228]

#Ross_PyriteClean_sub$Cluster <- factor(Ross_PyriteClean_sub$Cluster, levels = levels(Gregory$Cluster))

P<-predict(rm2,Ross_PyriteClean_sub)
levels(Ross_PyriteClean_sub$Cluster)

#Ross_Pyrite_ReallyClean<-Ross_PyriteClean_sub[rowSums(is.na(Ross_PyriteClean_sub)) != ncol(Ross_PyriteClean_sub), ]
Ross_PyriteClean_sub$Cluster <- P

plot(P)
