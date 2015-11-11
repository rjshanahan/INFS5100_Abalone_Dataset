#Richard Shanahan  
#https://github.com/rjshanahan  
#17 August 2015

###### INFS 5100 Predictive Analytics: Assignment 2 Abalone Dataset

# load required packages
library(Hmisc)
library(psych)
library(ggplot2)
library(reshape2)
library(dplyr)
library(devtools)
library(e1071)

# source custom code for plots from GitHub Gist: https://gist.github.com/rjshanahan
source_gist("e47c35277a36dca7189a")       #boxplot
source_gist("7eed7f043c987f884748")       #facet wrap boxplot
source_gist("40f46687d48030d40704")       #cluster plot


###### 1. read in file and inspect data ###### 

abalone <- read.csv(#'abalone.data',
                    '/Users/rjshanahan/Documents/DATA SCIENCE/7. Predictive Analytics_INFS5100/04. Assignments/Assignment 02/abalone+hdr.csv',
                    header=T,
                    sep=",",
                    quote='"',
                    colClasses=c(
                      'factor',    # sex
                      'numeric',   # Length
                      'numeric',   # Diameter
                      'numeric',   # Height
                      'numeric',   # Whole_weight
                      'numeric',   # Shucked_weight
                      'numeric',   # Viscera_weight
                      'numeric',   # Shell_weight
                      'numeric'    # Rings
                    ),
                    strip.white=T,
                    stringsAsFactors=F,
                    fill=T)

#inspect
str(abalone)
describe(abalone)

#check for duplicate records based
nrow(unique(abalone))

#check if there are any missing values
colSums(is.na(abalone)) 

#identify records with a minimum value of 0
abalone <- filter(abalone, Height != 0)
#     Sex Length Diameter Height Whole_weight Shucked_weight Viscera_weight Shell_weight Rings
# 1   I  0.430     0.34      0        0.428         0.2065         0.0860       0.1150     8
# 2   I  0.315     0.23      0        0.134         0.0575         0.0285       0.3505     6

# assign id field for visualisations
abalone$id <- 1:nrow(abalone)

# outliers - run twice - two outliers in Height
filter(abalone, Height == max(Height))
#     Sex Length Diameter Height Whole_weight Shucked_weight Viscera_weight Shell_weight Rings
# 1   F  0.455    0.355   1.13        0.594          0.332          0.116       0.1335     8
abalone <- filter(abalone, Height != max(Height))
filter(abalone, Height == max(Height))
#     Sex Length Diameter Height Whole_weight Shucked_weight Viscera_weight Shell_weight Rings
# 1   M  0.705    0.565  0.515         2.21         1.1075         0.4865        0.512    10
abalone <- filter(abalone, Height != max(Height))

# recode RINGS to reduce the number of classes

# study distribution
ggplot(data = abalone, 
       aes(x=Rings, fill=Sex)) + 
  geom_histogram(binwidth=1) +
  ggtitle("Abalone: Histogram of Rings by Sex - binwidth = 1 ring ('~0.4 years')")

mR <- median(abalone$Rings)
madR <- mad(abalone$Rings)
iqrR <- IQR(abalone$Rings)

# recode
#abalone$ring_group <- ifelse(abalone$Rings > mR,
#                          1,
#                          0)

# recode
abalone$ring_group <- ifelse(abalone$Rings >= mR + madR,
                             "Old",
                             (ifelse(abalone$Rings <= mR - madR,
                                     "Young",
                                     "Adult")))

table(abalone$ring_group)

ggplot(data = abalone, 
       aes(x=Rings, fill=factor(ring_group))) + 
  geom_histogram(binwidth=1) +
  ggtitle("Abalone: Histogram of Rings by 'Ring Group' - binwidth = 1 ring ('~0.4 years')")


#write to file
write.csv(select(abalone, -id, -Rings), file = "abalone_3class.csv", row.names = FALSE)


#test for normality for continuous attributes
apply(abalone[,2:9], 2, shapiro.test)

#qqplots
par(mfrow=c(2,4))
qqnorm(abalone[,2], xlab=colnames(abalone)[2])
qqnorm(abalone[,3], xlab=colnames(abalone)[3])
qqnorm(abalone[,4], xlab=colnames(abalone)[4])
qqnorm(abalone[,5], xlab=colnames(abalone)[5])
qqnorm(abalone[,6], xlab=colnames(abalone)[6])
qqnorm(abalone[,7], xlab=colnames(abalone)[7])
qqnorm(abalone[,8], xlab=colnames(abalone)[8])
qqnorm(abalone[,9], xlab=colnames(abalone)[9])
par(mfrow=c(1,1))

#ANOVA
#Recode Sex to continuous
#M = 1, #F = 2, #I = 3
abalone$sex_num <- ifelse(abalone$Sex == "M",
                          1,
                          (ifelse(abalone$Sex == "F",
                                  2,
                                  3)))

abalone_rings <- aov(Rings ~ Length + Height + Diameter + Whole_weight + Shucked_weight +
                       Viscera_weight + Shell_weight + sex_num, 
                     data=abalone)


summary(abalone_rings)
abalone_rings$coefficients
#print(model.tables(abalone_sex,"means"),digits=3) 


###### 2. Boxplots #######
#Grouped Boxplots
# reshape dataset for boxplot representation - un-standardised
abalone.m <- melt(select(abalone, -Sex, -ring_group),
                  id.var="id")

#un-standardised boxplot - no outliers
source_GitHubGist_boxplot(abalone.m,
                          "Abalone Boxplots for Continuous Attributes - Significant Outliers Removed",
                          "variable name",
                          "un-standardised value")

#standardise
abalone.scale <- scale(abalone[,2:10])
abalone.scale <- as.data.frame(abalone.scale[1:nrow(abalone), 1:9])

# reshape dataset for boxplot representation - standardised
abalone.scale.m <- melt(abalone.scale,
                        id.var="id")

#standardised boxplot - no outliers
source_GitHubGist_boxplot(abalone.scale.m,
                          "Abalone Boxplots for Continuous Attributes - Outliers Removed",
                          "variable name",
                          "standardised value")



#facte wrap boxplots by each variable - extra granularity
source_GitHubGist_boxplot_faces(select(abalone, var=Sex, val=Length, faces=ring_group), 
                                var,
                                val,
                                faces,
                                "Abalone - Age by Length and Sex", 
                                "Sex", 
                                "Length")
source_GitHubGist_boxplot_faces(select(abalone, var=Sex, val=Diameter, faces=ring_group), 
                                var,
                                val,
                                faces,
                                "Abalone - Age by Diameter and Sex", 
                                "Sex", 
                                "Diameter")
source_GitHubGist_boxplot_faces(select(abalone, var=Sex, val=Height, faces=ring_group), 
                                var,
                                val,
                                faces,
                                "Abalone - Age by Height and Sex", 
                                "Sex", 
                                "Height")
source_GitHubGist_boxplot_faces(select(abalone, var=Sex, val=Whole_weight, faces=ring_group), 
                                var,
                                val,
                                faces,
                                "Abalone - Age by Whole_weight and Sex", 
                                "Sex", 
                                "Whole_weight")
source_GitHubGist_boxplot_faces(select(abalone, var=Sex, val=Shucked_weight, faces=ring_group), 
                                var,
                                val,
                                faces,
                                "Abalone - Age by Shucked_weight and Sex", 
                                "Sex", 
                                "Shucked_weight")
source_GitHubGist_boxplot_faces(select(abalone, var=Sex, val=Viscera_weight, faces=ring_group), 
                                var,
                                val,
                                faces,
                                "Abalone - Age by Viscera_weight and Sex", 
                                "Sex", 
                                "Viscera_weight")
source_GitHubGist_boxplot_faces(select(abalone, var=Sex, val=Shell_weight, faces=ring_group), 
                                var,
                                val,
                                faces,
                                "Abalone - Age by Shell_weight and Sex", 
                                "Sex", 
                                "Shell_weight")


######## 3. Histograms and barplots ######## 
# order dataframe for bar chart purposes
#abalone <- abalone[order(abalone$Height, abalone$Sex),]
abalone <- abalone[order(abalone$Rings, abalone$Sex),]

ggplot(data = abalone, 
       aes(x=Rings, fill=ring_group)) + 
  geom_histogram(stat="bin", binwidth=1) +
  ggtitle("Abalone: Histogram of Rings by Ring Group' and Sex") 

# melt for barchart on same grid
abalone.m2 <- melt(select(abalone, -id, -sex_num, -Rings))

#barcharts by variable then Ring Group
ggplot(abalone.m2, 
       aes(ring_group,
           fill=ring_group)) + 
  geom_histogram(stat='bin') + 
  facet_wrap(~variable) +
  ggtitle("Abalone: Barcharts for Continuous Attributes by Age") 


###### 4. Correlations ###### 

#continuous attributes
abalone.cor <- round(cor(abalone[2:9], 
                         use = "complete.obs",
                         y=NULL,
                         method = "pearson"), 2)

# scatterplot
pairs(#ring_group ~ Length + Diameter + Height + Whole_weight + Shucked_weight + Viscera_weight + Shell_weight ,
  abalone[2:9],
  main="Scatterplot of Abalone Continuous Attributes",
  pch = 6,
  cex = 1.5,
  col="indianred")


###### 5. PCA ######
abalone.pca <- prcomp(abalone[,2:9])

#PCA details
summary(abalone.pca)
abalone.pca$rotation
abalone.pca$center
abalone.pca$sdev

#check sum of variance
sum((abalone.pca$sdev)^2)

#assess eigenvalues
round((abalone.pca$sdev)^2, 2)

#the first 2 eigenvalues account for ~95% of variance

#generate screeplot to show how many components to retain
par(mfrow=c(1,1))
lty.o <- par("lty") 
par(lty = 2) 
screeplot(abalone.pca, type="lines",pch=19,col='red3')

#assess variable loadings against the first four factors
round(abalone.pca$rotation[,1:2], 3)


####### 6. k-means clustering #######

#abalone.scale <- scale(abalone[,2:9])

abalone.cluster <- kmeans(abalone.scale[,1:8], 4)

str(abalone.cluster)
abalone.cluster$centers
abalone.cluster$size

#clusplot
cluster_plot(abalone.cluster, abalone.scale, "Abalone Cluster Coordinate Plot") 


###### 7. Attributes Removed ######

abalone2 <- select(abalone, Sex, Length, Diameter, Height, Shell_weight, id, ring_group)

#standardise
abalone2.scale <- scale(abalone2[,2:6])
abalone2.scale <- as.data.frame(abalone2.scale[1:nrow(abalone2), 1:5])


# reshape dataset for boxplot representation - un-standardised
abalone2.m <- melt(abalone2.scale,
                  id.var="id")

#un-standardised boxplot - no outliers
source_GitHubGist_boxplot(abalone2.m,
                          "Abalone Boxplots for SELECTED Continuous Attributes - Significant Outliers Removed",
                          "variable name",
                          "standardised value")

#correlation continuous attributes
abalone2.cor <- round(cor(abalone2[2:5], 
                         use = "complete.obs",
                         y=NULL,
                         method = "pearson"), 2)

#histograms with binwidth = 3 - used to assess splitting rules in SAS EM
ggplot(data = abalone2, 
       aes(x=Shell_weight,
           fill=ring_group)) + 
  geom_histogram(stat="bin", binwidth=(range(abalone2$Shell_weight)[2] - range(abalone2$Shell_weight)[1])/3) +
  ggtitle("Abalone: Shell_weight by Ring Group") 

abalone2$Shell.bin <- ifelse(abalone2$Shell_weight <= 0.3345,
                             "bin1",
                             ifelse(abalone2$Shell_weight >= 0.3345*2,
                                    "bin3",
                                    "bin2"))
  
table(abalone2$ring_group, abalone2$Shell.bin)
##
ggplot(data = abalone2, 
       aes(x=Sex,
           fill=ring_group)) + 
  geom_histogram(stat="bin", binwidth=3) +
  ggtitle("Abalone: Sex by Ring Group") 

table(abalone2$ring_group, abalone2$Sex)
##

ggplot(data = abalone2, 
       aes(x=Length,
           fill=ring_group)) + 
  geom_histogram(stat="bin", binwidth=(range(abalone2$Length)[2] - range(abalone2$Length)[1])/3) +
  ggtitle("Abalone: Length by Ring Group") 

abalone2$Length.bin <- ifelse(abalone2$Length <= 0.2466667,
                             "bin1",
                             ifelse(abalone2$Length >= 0.2466667*2,
                                    "bin3",
                                    "bin2"))

table(abalone2$ring_group, abalone2$Length.bin)
##

ggplot(data = abalone2, 
       aes(x=Diameter,
           fill=ring_group)) + 
  geom_histogram(stat="bin", binwidth=(range(abalone2$Diameter)[2] - range(abalone2$Diameter)[1])/3) +
  ggtitle("Abalone: Diameter by Ring Group") 

abalone2$Dia.bin <- ifelse(abalone2$Diameter <= 0.19833,
                              "bin1",
                              ifelse(abalone2$Diameter >= 0.19833*2,
                                     "bin3",
                                     "bin2"))

table(abalone2$ring_group, abalone2$Dia.bin)
##

ggplot(data = abalone2, 
       aes(x=Height,
           fill=ring_group)) + 
  geom_histogram(stat="bin", binwidth=(range(abalone2$Height)[2] - range(abalone2$Height)[1])/3) +
  ggtitle("Abalone: Height by Ring Group")

abalone2$Height.bin <- ifelse(abalone2$Height <= 0.08,
                           "bin1",
                           ifelse(abalone2$Height >= 0.08*2,
                                  "bin3",
                                  "bin2"))

table(abalone2$ring_group, abalone2$Height.bin)
##


#function for entropy calculations
entrop <- function(classy) {
  -(classy*log2(classy))
}

C1 <- 153/181
C2 <- 28/181

C4 <- 140/468
C5 <- 311/468
C6 <- 17/468

C7 <- 19/2269
C8 <- 1596/2269
C9 <- 654/2269

entrop(C1) + entrop(C2) 
entrop(C4) + entrop(C5) + entrop(C6)
entrop(C7) + entrop(C8) + entrop(C9)


entrop(C1) + entrop(C2) + entrop(C3)
entrop(C4) + entrop(C5) + entrop(C6)

#output file for SAS EM import
write.csv(select(abalone2, -id), file = "abalone_reworked.csv", row.names = FALSE)

#add re-scaled continuous attributes
abalone2 <- mutate(abalone2, length.S = abalone2$Length*200,
         diameter.S = abalone2$Diameter*200,
         height.S = abalone2$Height*200,
         shell_weight.S = abalone2$Shell_weight*200)

###### 8. MODELLING ######
##### 8.1 Split dataframe into TRAIN and TEST and CALIBRATE#####

#NOTE: comment this code once run initially - if rerun it will recreate slightly different sized and 
#randomised samples - ie, differing results for predictive models

#assign random value to each ob based on uniform distribution - for reproducibility this is commented
abalone$sample <- runif(nrow(abalone))

abalone_train <- filter(abalone, sample > 0.3)
abalone_test <- filter(abalone, sample <= 0.3)

dim(abalone_train)
dim(abalone_test)


############ 7.2 DECISION TREE with 'rpart' package ###############
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#mode
fit <- rpart(Rings ~ Sex + Length + Diameter + Height + Shell_weight,
             data=abalone_train)

# model execution
Prediction <- predict(fit, abalone_train)                     

# generate predictions for TEST dataset
abalone_test$pred <- predict(fit, abalone_test) 

#split metrics
print(fit)
summary(fit)

#other metrics
fit$variable.importance
fit$splits
fit$cptable
fit$frame$var
it$frame$n

#plot as dendrogram
par(xpd = TRUE)
plot(fit, compress = TRUE)
text(fit, use.n = TRUE)

fancyRpartPlot(fit)

#plot variable importance
plot(fit$variable.importance)


# output comparison of prediction
abalone_test %>% 
  select(id, Rings, pred) %>% 
  mutate(diff = Rings-pred)




############ Assignment 02 File Rework ############ 

#output file with all attributes and outliers removed
abalone3 <- select(abalone, Sex, Length, Diameter, Height, Shell_weight,
                   Whole_weight, Shucked_weight, Viscera_weight, ring_group)

#write to file
write.csv(abalone3, file = "abalone_reworked_02.csv", row.names = FALSE)


#import score tuple dataset for analysis
abalone_score <- read.csv(#'abalone.data',
  '/Users/rjshanahan/Documents/DATA SCIENCE/7. Predictive Analytics_INFS5100/04. Assignments/Assignment 02/abalone_test.csv',
  header=T,
  sep=",",
  quote='"',
  colClasses=c(
    'factor',    # sex
    'numeric',   # Length
    'numeric',   # Diameter
    'numeric',   # Height
    'numeric',   # Whole_weight
    'numeric',   # Shucked_weight
    'numeric',   # Viscera_weight
    'numeric',   # Shell_weight
    'numeric'    # Rings
  ),
  strip.white=T,
  stringsAsFactors=F,
  fill=T)


#inspect
str(abalone_score)
describe(abalone_score)

#check for duplicate records based
nrow(unique(abalone_score))

#check if there are any missing values
colSums(is.na(abalone_score)) 


# recode RINGS to reduce the number of classes

# study distribution
ggplot(data = abalone_score, 
       aes(x=Rings, fill=Sex)) + 
  geom_histogram(binwidth=1) +
  ggtitle("Abalone: Histogram of Rings by Sex - binwidth = 1 ring ('~0.4 years')")

mR <- median(abalone$Rings)
madR <- mad(abalone_score$Rings)
iqrR <- IQR(abalone_score$Rings)

# recode
abalone_score$ring_group <- ifelse(abalone_score$Rings > mR,
                             1,
                             0)

table(abalone_score$ring_group)

ggplot(data = abalone_score, 
       aes(x=Rings, fill=factor(ring_group))) + 
  geom_histogram(binwidth=1) +
  ggtitle("Abalone: Histogram of Rings by 'Ring Group' - binwidth = 1 ring ('~0.4 years')")


#Grouped Boxplots
# assign id field for visualisations
abalone_score$id <- 1:nrow(abalone_score)

# reshape dataset for boxplot representation - un-standardised
abalone_score.m <- melt(select(abalone_score, -Sex, -ring_group),
                  id.var="id")

#un-standardised boxplot
source_GitHubGist_boxplot(abalone_score.m,
                          "Abalone Boxplots for Continuous Attributes - Significant Outliers Removed",
                          "variable name",
                          "un-standardised value")

#standardise
abalone_score.scale <- scale(abalone_score[,2:11])
abalone_score.scale <- as.data.frame(abalone_score.scale[1:nrow(abalone_score),])

# reshape dataset for boxplot representation - standardised
abalone_score.scale.m <- melt(abalone_score.scale,
                        id.var="id")

#standardised boxplot
source_GitHubGist_boxplot(abalone_score.scale.m,
                          "Abalone Boxplots for Continuous Attributes - Outliers Removed",
                          "variable name",
                          "standardised value")

#write to file
write.csv(abalone_score[,1:8], file = "abalone_score.csv", row.names = FALSE)





############ 6.6 NAIVE BAYES with 'e1071' package ###############


#import TEST dataset used in SAS EM
abalone_train_SAS <- read.csv(#'abalone.data',
  '/Users/rjshanahan/Documents/DATA SCIENCE/7. Predictive Analytics_INFS5100/04. Assignments/Assignment 02/abalone_train.txt',
  header=T,
  sep="\t",
  quote='"',
  colClasses=c(
    'factor',    # sex
    'numeric',   # Length
    'numeric',   # Diameter
    'numeric',   # Height
    'numeric',   # Whole_weight
    'numeric',   # Shucked_weight
    'numeric',   # Viscera_weight
    'numeric',   # Shell_weight
    'factor'    # Rings
  ),
  strip.white=T,
  stringsAsFactors=F,
  fill=T)

#inspect
str(abalone_train_SAS)
describe(abalone_train_SAS)


#import TEST dataset used in SAS EM
abalone_test_SAS <- read.csv(#'abalone.data',
  '/Users/rjshanahan/Documents/DATA SCIENCE/7. Predictive Analytics_INFS5100/04. Assignments/Assignment 02/abalone_test.txt',
  header=T,
  sep="\t",
  quote='"',
  colClasses=c(
    'factor',    # sex
    'numeric',   # Length
    'numeric',   # Diameter
    'numeric',   # Height
    'numeric',   # Whole_weight
    'numeric',   # Shucked_weight
    'numeric',   # Viscera_weight
    'numeric',   # Shell_weight
    'factor'    # Rings
  ),
  strip.white=T,
  stringsAsFactors=F,
  fill=T)

#inspect
str(abalone_test_SAS)
describe(abalone_test_SAS)


#import SCORE dataset used in SAS EM
abalone_score_SAS <- read.csv(#'abalone.data',
  '/Users/rjshanahan/Documents/DATA SCIENCE/7. Predictive Analytics_INFS5100/04. Assignments/Assignment 02/abalone_test_tuple.csv',
  header=T,
  sep=",",
  quote='"',
  colClasses=c(
    'factor',    # sex
    'numeric',   # Length
    'numeric',   # Diameter
    'numeric',   # Height
    'numeric',   # Whole_weight
    'numeric',   # Shucked_weight
    'numeric',   # Viscera_weight
    'numeric',   # Shell_weight
    'numeric'    # Rings
  ),
  strip.white=T,
  stringsAsFactors=F,
  fill=T)

#inspect
str(abalone_score_SAS)
describe(abalone_score_SAS)



#Naive Bayes Model and configuration
fit <- naiveBayes(ring_group ~ Sex + Length  + Diameter + Height + Whole_weight + Shucked_weight + Viscera_weight + Shell_weight,
                  data=abalone_train_SAS,
                  laplace = 3)

# model execution
# generate predictions for TEST dataset
abalone_test_SAS$pred <- predict(fit, abalone_test_SAS)
table(abalone_test_SAS$pred)

#misclassification rate

abalone_train_SAS$pred <- predict(fit, abalone_train_SAS)

misclass_train <- sum(ifelse(abalone_train_SAS$ring_group == abalone_train_SAS$pred, 1, 0))/nrow(abalone_train_SAS)

misclass_train


misclass_test <- sum(ifelse(abalone_test_SAS$ring_group == abalone_test_SAS$pred, 1, 0))/nrow(abalone_test_SAS)

misclass_test

#predict the SCORE test tuple
abalone_score_SAS$pred <- predict(fit, abalone_score_SAS)
table(abalone_score_SAS$pred)

# recode for comparison
abalone_score_SAS$ring_group <- ifelse(abalone_score_SAS$Rings >= mR + madR,
                             "Old",
                             (ifelse(abalone_score_SAS$Rings <= mR - madR,
                                     "Young",
                                     "Adult")))


ggplot(data = abalone_score_SAS, 
       aes(x=Rings, fill=factor(ring_group))) + 
  geom_histogram(binwidth=1) +
  ggtitle("Abalone SCORE: Histogram of Rings by 'Ring Group' - binwidth = 1 ring ('~0.4 yrs')")





#misclassification rate
misclass_score <- sum(ifelse(abalone_score_SAS$ring_group == abalone_score_SAS$pred, 1, 0))/nrow(abalone_score_SAS)

misclass_score

#check assumption of indepdence between attributes - chi square test
apply(abalone,
      2,
      function(x) {chisq.test(table(abalone$ring_group, x))})



# calculate AUC
library(AUC)

#metrics for TEST
sensit_test <- sensitivity(abalone_test_SAS$pred, abalone_test_SAS$ring_group)
specif_test <- specificity(abalone_test_SAS$pred, abalone_test_SAS$ring_group)
accur_test <- accuracy(abalone_test_SAS$pred, abalone_test_SAS$ring_group)
roc_test <- roc(abalone_test_SAS$pred, abalone_test_SAS$ring_group)

plot(roc_test, min=0, max=1)

auc_test <- auc(sensit_test, 0, 1)

#metrics for TRAIN
sensit_train <- sensitivity(abalone_train_SAS$pred, abalone_train_SAS$ring_group)
specif_train <- specificity(abalone_train_SAS$pred, abalone_train_SAS$ring_group)
accur_train <- accuracy(abalone_train_SAS$pred, abalone_train_SAS$ring_group)
roc_train <- roc(abalone_train_SAS$pred, abalone_train_SAS$ring_group)

plot(roc_train, min=0, max=1)

auc_train <- auc(sensit_train, 0, 1)




#continuous attributes correlation
abalone_score.cor <- round(cor(abalone_score_SAS[2:9], 
                         use = "complete.obs",
                         y=NULL,
                         method = "pearson"), 2)



#visualise SCORE distributions
abalone_score.scale <- scale(abalone_score_SAS[,2:9])
abalone_score.scale <- as.data.frame(abalone_score.scale[1:nrow(abalone_score_SAS), 1:8])

# reshape dataset for boxplot representation - standardised
abalone_score.scale$id <- 1:nrow(abalone_score_SAS)

abalone_score.scale.m <- melt(abalone_score.scale,
                        id.var="id")

#standardised boxplot - no outliers
source_GitHubGist_boxplot(abalone_score.scale.m,
                          "Abalone SCORE Boxplots for Continuous Attributes",
                          "variable name",
                          "standardised value")
