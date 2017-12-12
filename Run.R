#Script to run through implementations and produce results

######MAKE RANDOM TRAIN/TEST SETS###########

#breast_cancer
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 2/3 of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(breast_cancer), size = floor(.666667*nrow(breast_cancer)), replace = F)
breast_cancer_train <- breast_cancer[sample, ]
breast_cancer_test  <- breast_cancer[-sample, ]

#glass
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 2/3 of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(glass), size = floor(.666667*nrow(glass)), replace = F)
glass_train <- glass[sample, ]
glass_test  <- glass[-sample, ]

#iris
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 2/3 of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(iris), size = floor(.666667*nrow(iris)), replace = F)
iris_train <- iris[sample, ]
iris_test  <- iris[-sample, ]

#soybean
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 2/3 of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(soybean), size = floor(.666667*nrow(soybean)), replace = F)
soybean_train <- soybean[sample, ]
soybean_test  <- soybean[-sample, ]

#vote
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 2/3 of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(vote), size = floor(.666667*nrow(vote)), replace = F)
vote_train <- vote[sample, ]
vote_test  <- vote[-sample, ]

######RUN MODELS FOR DIFFERENT DATA SETS###########

###breast_cancer###
breast_cancer_train_winnow2 <- winnow2.train(breast_cancer_train[,1:(ncol(breast_cancer_train)-1)],breast_cancer_train[,ncol(breast_cancer_train)],(ncol(breast_cancer_train)-1)/2,2,1)
breast_cancer_train_naiveBayes <- naiveBayes.train(breast_cancer_train[,1:(ncol(breast_cancer_train)-1)],breast_cancer_train[,ncol(breast_cancer_train)])
breast_cancer_predictions <- as.data.frame(breast_cancer_test[,ncol(breast_cancer_test)])
colnames(breast_cancer_predictions) <- c("Actual")
breast_cancer_predictions$Winnow2<-winnow2.predict(breast_cancer_train_winnow2,breast_cancer_test[,1:(ncol(breast_cancer_test)-1)],(ncol(breast_cancer_test)-1)/2)
breast_cancer_predictions$NaiveBayes <- naiveBayes.predict(breast_cancer_test[,1:(ncol(breast_cancer_test)-1)],breast_cancer_train_naiveBayes)

###iris###
iris_train_winnow2 <- winnow2.train(iris_train[,1:(ncol(iris_train)-1)],iris_train[,ncol(iris_train)],(ncol(iris_train)-1)/2,2,1)
iris_train_naiveBayes <- naiveBayes.train(iris_train[,1:(ncol(iris_train)-1)],iris_train[,ncol(iris_train)])
iris_predictions <- as.data.frame(iris_test[,ncol(iris_test)])
colnames(iris_predictions) <- c("Actual")
iris_predictions$Winnow2<-winnow2.predict(iris_train_winnow2,iris_test[,1:(ncol(iris_test)-1)],(ncol(iris_test)-1)/2)
iris_predictions$NaiveBayes <- naiveBayes.predict(iris_test[,1:(ncol(iris_test)-1)],iris_train_naiveBayes)

###glass###
glass_train_winnow2 <- winnow2.train(glass_train[,1:(ncol(glass_train)-1)],glass_train[,ncol(glass_train)],(ncol(glass_train)-1)/2,2,1)
glass_train_naiveBayes <- naiveBayes.train(glass_train[,1:(ncol(glass_train)-1)],glass_train[,ncol(glass_train)])
glass_predictions <- as.data.frame(glass_test[,ncol(glass_test)])
colnames(glass_predictions) <- c("Actual")
glass_predictions$Winnow2<-winnow2.predict(glass_train_winnow2,glass_test[,1:(ncol(glass_test)-1)],(ncol(glass_test)-1)/2)
glass_predictions$NaiveBayes <- naiveBayes.predict(glass_test[,1:(ncol(glass_test)-1)],glass_train_naiveBayes)

###soybean###
soybean_train_winnow2 <- winnow2.train(soybean_train[,1:(ncol(soybean_train)-1)],soybean_train[,ncol(soybean_train)],(ncol(soybean_train)-1)/2,2,1)
soybean_train_naiveBayes <- naiveBayes.train(soybean_train[,1:(ncol(soybean_train)-1)],soybean_train[,ncol(soybean_train)])
soybean_predictions <- as.data.frame(soybean_test[,ncol(soybean_test)])
colnames(soybean_predictions) <- c("Actual")
soybean_predictions$Winnow2<-winnow2.predict(soybean_train_winnow2,soybean_test[,1:(ncol(soybean_test)-1)],(ncol(soybean_test)-1)/2)
soybean_predictions$NaiveBayes <- naiveBayes.predict(soybean_test[,1:(ncol(soybean_test)-1)],soybean_train_naiveBayes)

###vote###
vote_train_winnow2 <- winnow2.train(vote_train[,1:(ncol(vote_train)-1)],vote_train[,ncol(vote_train)],(ncol(vote_train)-1)/2,2,1)
vote_train_naiveBayes <- naiveBayes.train(vote_train[,1:(ncol(vote_train)-1)],vote_train[,ncol(vote_train)])
vote_predictions <- as.data.frame(vote_test[,ncol(vote_test)])
colnames(vote_predictions) <- c("Actual")
vote_predictions$Winnow2<-winnow2.predict(vote_train_winnow2,vote_test[,1:(ncol(vote_test)-1)],(ncol(vote_test)-1)/2)
vote_predictions$NaiveBayes <- naiveBayes.predict(vote_test[,1:(ncol(vote_test)-1)],vote_train_naiveBayes)


### NOT USING THESE LIBRARIES FOR IMPLEMENTATION - JUST FOR LOOKING AT HOW WE DID ###

library(ROCR)
library(caret)
library(e1071)

#breast_cancer output
breast_cancer_train_winnow2
breast_cancer_train_naiveBayes
breast_cancer_predictions
confusionMatrix(breast_cancer_predictions$Winnow2,breast_cancer_predictions$Actual, positive = '1')
confusionMatrix(breast_cancer_predictions$NaiveBayes,breast_cancer_predictions$Actual, positive = '1')

#glass output
glass_train_winnow2
glass_train_naiveBayes
glass_predictions
confusionMatrix(glass_predictions$Winnow2,glass_predictions$Actual, positive = '1')
confusionMatrix(glass_predictions$NaiveBayes,glass_predictions$Actual, positive = '1')

#iris output
iris_train_winnow2
iris_train_naiveBayes
iris_predictions
confusionMatrix(iris_predictions$Winnow2,iris_predictions$Actual, positive = '1')
confusionMatrix(iris_predictions$NaiveBayes,iris_predictions$Actual, positive = '1')

#soybean output
soybean_train_winnow2
soybean_train_naiveBayes
soybean_predictions
confusionMatrix(soybean_predictions$Winnow2,soybean_predictions$Actual, positive = '1')
confusionMatrix(soybean_predictions$NaiveBayes,soybean_predictions$Actual, positive = '1')

#vote output
vote_train_winnow2
vote_train_naiveBayes
vote_predictions
confusionMatrix(vote_predictions$Winnow2,vote_predictions$Actual, positive = '1')
confusionMatrix(vote_predictions$NaiveBayes,vote_predictions$Actual, positive = '1')

