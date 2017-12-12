#implementation space for Winnow2 and Naive Bayes

#####WINNOW 2 #####

#used to train with winnow2
#so...inputs are pretty self explanatory, but this works as follows: independent are the 1's a 0's of all variables other than the 
# predicted variable, depenendent variable is the predicted class
#theta is the threshold 
# alpha is the divisor
# init is the number we should initialize the vector to
# returns the weighted vector
winnow2.train <- function(independent_variables, dependent_variable, theta, alpha, init){
  cols <- ncol(independent_variables)
  #create vector of initial weights, with proper number of columns
  weights <- rep(init, cols)
  for(i in 1:nrow(independent_variables)) {
    row <- independent_variables[i,]
    product <- row * weights
    sum_product <- sum(product)
    predict <- ifelse(sum_product > theta,1,0)
    # incorrect prediction (do nothing if correct)
    if (predict != dependent_variable[i]) {
      if (dependent_variable[i] == 1) { #promote
        for (j in 1:cols) {
          weights[j] <- ifelse(row[j] == 1,weights[j]*alpha,weights[j])
        } 
      } else { #demote
        for (j in 1:cols) {
          weights[j] <- ifelse(row[j] == 1,weights[j]/alpha,weights[j])
        }
      }
    } 
  }
  return(weights)
}

#used to predict with winnow2
#again pretty self explanatory. Input weighted vector used for prediction, corresponding ind. variables, theta value
#output predictions vector
winnow2.predict <- function(weights,independent_variables, theta) {
  rows <- nrow(independent_variables)
  #create vector of initial predcitions, set to 0
  predictions <- rep(0, rows)
  for(i in 1:nrow(independent_variables)) {
    row <- independent_variables[i,]
    product <- row * weights
    sum_product <- sum(product)
    predict <- ifelse(sum_product > theta,1,0)
    predictions[i] <- predict
  }
  return(predictions)
}

##### NAIVE BAYES #####
#Note - designed to take only boolean values, per the problem specs.
#used to train with naive bayes
#input is vector of independendent and dependent variables 
#output is 4 rowed probability matrix - row 1 is P(C = 0, F = 0), row 1 is P(C = 0, F = 1), row 3 is P(C = 1, F = 0), row 4 row 1 is P(C = 1, F = 1)
naiveBayes.train <- function(independent_variables,dependent_variable) {
  cols <- ncol(independent_variables) + 1
  #initialize output matrix
  output <- data.frame(matrix(ncol = cols, nrow = 4))
  #overall classes
  overall <- mean(dependent_variable)
  output[1,1] <- 1-overall
  output[2,1] <- 1-overall
  output[3,1] <- overall
  output[4,1] <- overall
  #sub-probabilities
  class_0 <- independent_variables[dependent_variable == 0,]
  class_1 <- independent_variables[dependent_variable == 1,]
  # m estimate section - let's use laplace smoothing (m = 1), and uniform priors (p=.5)
  class_0[nrow(class_0) + 1,] = .5
  class_1[nrow(class_1) + 1,] = .5
  #take averages
  class_0_means <- colMeans(class_0)
  class_1_means <- colMeans(class_1)
  #assign proper values in matrix
  output[1,2:cols] <- 1 - class_0_means 
  output[2,2:cols] <- class_0_means 
  output[3,2:cols] <- 1 - class_1_means
  output[4,2:cols] <- class_1_means
  #return matrix
  return(output)
}

#Used to predict classes using naive bayes - input - independent variables, pre-determined probability matrix 
#output - predictions
naiveBayes.predict <- function(independent_variables, prob_mat) {
  rows <- nrow(independent_variables)
  cols <- ncol(independent_variables)
  predictions <- rep(0, rows)
  class_0 <- rep(0, rows + 1)
  #loop through all rows to make predictions
  for(i in 1:rows) {
    row <- independent_variables[i,]
    #initialize multiplicitve matrix, use overall class probabilities as first column
    class_0 <- rep(0, cols + 1)
    class_0[1] <- prob_mat[1,1]
    class_1 <- rep(0, cols + 1)
    class_1[1] <- prob_mat[3,1]
    #assign affirmative and negative case probabilities for each columns
    for (j in 1:cols) {
      class_0[j+1] <- ifelse(row[j] == 1, prob_mat[2,j+1], prob_mat[1,j+1])
      class_1[j+1] <- ifelse(row[j] == 1, prob_mat[4,j+1], prob_mat[3,j+1])
    }
    #take product and compare results
    class_0_product <- prod(class_0)
    class_1_product <- prod(class_1)
    predictions[i] <- ifelse( class_1_product > class_0_product,1,0)
  }
  return(predictions)
}

