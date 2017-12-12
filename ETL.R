library(plyr)
library(dplyr)

#Script to load in and clean up data sets for machine learning problem set 1 
setwd("~/Desktop/Machine Learning Problem Set 1")

#Start with glass because it looks easy
glass <- read.csv("./Glass/glass.data", header = F)
colnames(glass) <- c('ID','RI','Na','Mg','Al','Si','K','Ca','Ba','Fe','Window')
#create 2-class dependent variable - window glass is 1, else 0
glass$Window<- ifelse(glass$Window > 4,0,1)
# 1 hot code all variables - less than mean 0, greater than mean 1 - not the greatest approach, but a good *quick* approach. Works for both categorical and numerical.
glass[,2:10] <- as.data.frame(sapply(glass[,2:10],  function(x) ifelse( x > mean(x),1,0)))
glass$ID <- NULL

#Next lets look at the breast cancer data set
breast_cancer <- read.csv("./Breast Cancer/breast-cancer-wisconsin.data", header = F)
colnames(breast_cancer) <- c('ID','Clump Thickness','Uniformity of Cell Size','Uniformity of Cell Shape','Marginal Adhesion','Single Epithelial Cell Size','Bare Nuclei','Bland Chromatin','Normal Nucleoli','Mitoses','Malignant')
#create discreet dependant - malignant 1, else 0
breast_cancer$Malignant <- ifelse(breast_cancer$Malignant == 4,1,0)
#remove columns with missing data
breast_cancer$`Bare Nuclei`<-ifelse(breast_cancer$`Bare Nuclei` == '?',NA,breast_cancer$`Bare Nuclei`)
breast_cancer <- breast_cancer[complete.cases(breast_cancer),]
# 1 hot code all variables - less than mean 0, greater than mean 1 - not the greatest approach, but a good *quick* approach. Works for both categorical and numerical.
breast_cancer[,2:10] <- as.data.frame(sapply(breast_cancer[,2:10],  function(x) ifelse( x > mean(x),1,0)))
breast_cancer$ID <- NULL

#Next up -iris data set 
iris <- read.csv("./Iris/iris.data", header = F, stringsAsFactors = T)
colnames(iris) <- c('sepal length','sepal width','petal length','petal width','Setosa')
#create 2 class dependant - setosa is 1, else 0
iris$Setosa <- ifelse(iris$Setosa == 'Iris-setosa',1,0)
# 1 hot code all variables - less than mean 0, greater than mean 1 - not the greatest approach, but a good *quick* approach. Works for both categorical and numerical.
iris[,1:4] <- as.data.frame(sapply(iris[,1:4],  function(x) ifelse( x > mean(x),1,0)))

#Now lets do soybean
soybean <- read.csv('./Soybean/soybean-small.data', header = F, stringsAsFactors = T)
#yikes...lots of variables. All numeric though, so lets not sweat nameing them (we cant anyways ;) )
#one thing we can do - make a discreet dependant - lets call it D4 - 1 for D4 0 for all else. Then, delete the previous column.
soybean$D4 <- ifelse(soybean$V36 == 'D4',1,0)
soybean$V36 <- NULL
# 1 hot code all variables - less than mean 0, greater than mean 1 - not the greatest approach, but a good *quick* approach. Works for both categorical and numerical.
soybean[,1:35] <- as.data.frame(sapply(soybean[,1:35],  function(x) ifelse( x > mean(x),1,0)))


#finally lets etl vote
vote <- read.csv('./Vote/house-votes-84.data', header = F, stringsAsFactors = F)
#I guess we saved the best for last...much manipulation needed
colnames(vote) <- c('Democrat','handicapped-infants','water-project-cost-sharing','adoption-of-the-budget-resolution','physician-fee-freeze','el-salvador-aid','religious-groups-in-schools','anti-satellite-test-ban','aid-to-nicaraguan-contras','mx-missile','immigration','synfuels-corporation-cutback','education-spending','superfund-right-to-sue','crime','duty-free-exports','export-administration-act-south-africa')
#change Democrats to y/n format
vote$Democrat <- ifelse(vote$Democrat == 'democrat','y','n')
#now let's deal with all of our question marks...lets randomly assign them y's or n's
yes_no <- c('y','n')
set.seed(100)
vote <- as.data.frame(sapply(vote,  function(x) ifelse( x == '?',sample(yes_no,1),x)))
#annnd last but not least lets convert all y's to 1's and n's to 0's
vote <- as.data.frame(sapply(vote,  function(x) ifelse( x == 'y',1,0)))
vote$Class_Democrat <- vote$Democrat
vote$Democrat <- NULL





