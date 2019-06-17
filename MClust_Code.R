#Author :Anirudh Prabhu
#email : prabha2@rpi.edu

#Code for Model based clustering
#Mixed Models

#Read File
DataFile <- read.csv("~/Downloads/Gregory.csv")
library(readxl)
DataFile <- read_excel("~/Downloads/Appendix 1 data used in Random Forest analysis.xlsx")

#Perform any cleaning required

#__________________________
#MClust
#Model based clustering

library(mclust)
#Temp object for clustering 

#Excluding the unneeded columns
X<-DataFile[,-c(1:2,8)]

#Setting seed for reproducibility
set.seed(2633)
# Choose the optimum mixed models and number of clusters
BIC <- mclustBIC(X)
plot(BIC)
summary(BIC)

#Actually run the model based clustering
mod1 <- Mclust(X, x = BIC)
summary(mod1, parameters = TRUE)
plot(mod1, what = "classification")

#View confusion matrix if comparing with existing categorical data (optional)
cc<-table(Gregory$`Deposit style`, mod1$classification)
table(Gregory$`Deposit style`, mod1$classification)
#plot(table(mod1$classification,Gregory$Deposit.style))
plot(mod1, what = "uncertainty")

cc_df <- data.frame(cc) # this will change deposit sytle to factor

#Plot Bar Graphs
library(ggplot2)

#Stacked
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

#Stacked and Normalized
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

#Side by side Bar Graph
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


#Use ICL to compare models (Can be used instead of BIC)
ICL <- mclustICL(X)
summary(ICL)
plot(ICL)

#LRT <- mclustBootstrapLRT(X, modelName = "VEV")

mod1$classification
