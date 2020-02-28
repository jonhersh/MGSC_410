# install.packages("doParallel")


## doParallel package
library(doParallel)
library('microbenchmark')
numCores <- detectCores()
numCores


# register your cores to doParallel
# note on windows machines can only set = 1 :(
registerDoParallel(cores = numCores)

# regular for loop
for(i in 1:3){
  print(sqrt(i))
}

# paralellized for loop!
foreach(i=1:3) %dopar% sqrt(i)


# a real example with bootstrapping
getPrimeNumbers <- function(n) {  
  n <- as.integer(n)
  primes <- rep(TRUE, n)
  primes[1] <- FALSE
  last.prime <- 2L
  for(i in last.prime:floor(sqrt(n)))
  {
    primes[seq.int(2L*last.prime, n, last.prime)] <- FALSE
    last.prime <- last.prime + min(which(primes[(last.prime+1):n]))
  }
  which(primes)
}

# use the prime number 
getPrimeNumbers(100)

index <- 10:10000

library(tictoc)
tic()
results <- c() 
for(i in index){
  results[[i]] <- getPrimeNumbers(i)
}
toc()

# or if you prefer the microbenchmark package
# devtools::install_github("joshuaulrich/microbenchmark")
microbenchmark::microbenchmark(
  for(i in index){ results[[i]] <- getPrimeNumbers(i) }
)


# let's try now with doParallel
library(doParallel)
numCores <- detectCores()
registerDoParallel(cores = numCores)
tic()
results <- foreach(i = 10:10000) %dopar% getPrimeNumbers(i)
toc()

# what if we want to store the results from each foreach loop?

results <- foreach(1:100, .combine = data.frame) %dopar% {
  # do something
}

# generate 1000 random draws of length 1000

tic()
results <- foreach(i=1:1000, .combine = data.frame) %dopar% {
  data.frame(rands = rnorm(1000))
}
toc()

head(results)

# caret package
# install.packages('caret')
library(caret)

# let's get some real data
library(useful)
library(caret)
install.packages('Ecdat')
data(Griliches, package = "Ecdat")
wages <- Griliches
wageFormula <- lw80 ~ age80 + school80 + expr80 + iq + rns80 + mrt80 +
  smsa80 + tenure80 + med + kww


# example trainControl function
ctrl1 <- trainControl(method = "repeatedcv", repeats = 5,
                      allowParallel = TRUE)

# look up model for rf
modelLookup('rf')
modelLookup('xgbTree')

# grid of mtry values to try
rfGrid <- expand.grid(mtry = seq(1, 10, 1))
rfGrid

#example train function
rfTrain <- train(wageFormula, 
                 data = wages, 
                 method = "rf", 
                 trControl = ctrl1,
                 tuneGrid = rfGrid, 
                 nthread = 4)

# confusion matrix
confusionMatrix(data = test_set$pred, 
                reference = test_set$obs)


# lift curves
trellis.par.set(caretTheme())
lift_obj <- lift(Class ~ FDA + 
                   LDA + C5.0, 
                 data = lift_results)
plot(lift_obj, values = 60, 
     auto.key = list(columns = 3,
                     lines = TRUE,
                     points = FALSE))

# calibration curves
trellis.par.set(caretTheme())
cal_obj <- calibration(Class ~ FDA + LDA + C5.0,
                       data = lift_results,
                       cuts = 13)
plot(cal_obj, type = "l", auto.key = list(columns = 3,
                                          lines = TRUE,
                                          points = FALSE))


# example problem
data(Cracker, package = "Ecdat")
brand <- Cracker

modelLookup("xgbTree")

crackerFormula <- paste("choice ~ ", paste(names(brand)[-c(1, 14)],
                                           collapse = " + "))

ctrl <- trainControl(method = "repeatedcv", repeats = 2, number = 5,
                     summaryFunction = multiClassSummary, 
                     classProbs = TRUE, allowParallel = FALSE)

crackerGrid <- expand.grid(nrounds = 100, 
                           max_depth = c(2, 4, 8, 10),
                           eta = c(0.01, 0.1, 0.2), 
                           gamma = 0.5, colsample_bytree = 1, 
                           min_child_weight = 1,
                           subsample = 0.7)

boostCracker <- train(choice ~ disp.sunshine + disp.kleebler + disp.nabisco +
                        disp.private + feat.sunshine + feat.kleebler + feat.nabisco +
                        feat.private + price.sunshine + price.kleebler + price.nabisco +
                        price.private, 
                      data = brand, 
                      method = "xgbTree", 
                      metric = "Accuracy",
                      trControl = ctrl,
                      tuneGrid = crackerGrid, 
                      nthread = 4)

plot(boostCracker)

