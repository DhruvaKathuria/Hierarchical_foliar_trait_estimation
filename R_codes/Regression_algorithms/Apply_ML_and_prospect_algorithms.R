
RMSE_function <- function(o,m){ind11 <- intersect(which(!is.na(m)), which(!is.na(o)))

sqrt(mean((m[ind11] - o[ind11])^2))}

cor_function <- function(o,m){ind11 <- intersect(which(!is.na(m)), which(!is.na(o)))

cor(m[ind11], o[ind11])}

# this is the main function that helps to apply the chosen regression algorithm "algorithm1" to the data
apply_regression_algorithm <- function(algorithm1, spectra_data_train, covariate_train1, spectra_data_test, covariate_test1)
{
  Y = covariate_train1
  X = spectra_data_train
  
  data_matrix_1 <- cbind(Y, X)
  data_matrix_1 = data.frame(data_matrix_1)
  
  data_matrix_1 = na.omit(data_matrix_1)
  
  
  if(algorithm1 == "PLSR")
  {
    library(pls)
    set.seed(123)
    ll_1 = plsr(Y ~., data = data_matrix_1, validation = "CV", scale = T, segments = 5)
    #summary(ll_1)
    RMSE_values_cv = RMSEP(ll_1)
    RMSE_values = RMSE_values_cv$val[1, , ]
    index1_min = which.min(RMSE_values[1:20])
    # validationplot(ll_1, val.type = "MSEP")
    pred1 <- predict(ll_1, spectra_data_test, ncomp = index1_min)
    pred1 = pred1[,1,1]
    
  }
  
  if(algorithm1 == "PCR")
  {
    library(pls)
    set.seed(123)
    ll_1 = pcr(Y ~., data = data_matrix_1, validation = "CV", scale = T)
    #summary(ll_1)
    RMSE_values_cv = RMSEP(ll_1)
    RMSE_values = RMSE_values_cv$val[1, , ]
    index1_min = which.min(RMSE_values[1:20])
    # validationplot(ll_1, val.type = "MSEP")
    pred1 <- predict(ll_1, spectra_data_test, ncomp = index1_min)
    pred1 = pred1[,1,1]
    
  }
  
  if(algorithm1 == "Random_Forest")
  {
    library(caret)
    library(randomForest)
    set.seed(123)
    
    ncol1 = ncol(data_matrix_1)
    grid_rf <- expand.grid(.mtry = c(ncol1/2,ncol1/3,ncol1/4))
    
    ntree = 100
    tuneResult <- train(Y ~., data = data_matrix_1, method = "rf",tuneGrid = grid_rf,ntree = ntree,
                        trControl = trainControl(method = "cv", number = 5,
                                                 #index = folds,                    #for pre-setting your own folds 
                                                 savePredictions = "final"))
    spectra_data_test = data.matrix(spectra_data_test)
    colnames(spectra_data_test) = colnames(data_matrix_1[, -1])
    pred1 = predict(tuneResult, newdata = spectra_data_test)
  }
  
  if(algorithm1 == "ridge") 
  {
    library(glmnet)
    set.seed(123)
    
    cv.out <- cv.glmnet(as.matrix(data_matrix_1[, -1]), as.numeric(data_matrix_1[, 1]), alpha = 0)
    bestlam <- cv.out$lambda.min
    fit <- cv.out$glmnet.fit
    pred1 <- predict(fit, s = bestlam, newx = as.matrix(spectra_data_test))
  }
  
  if(algorithm1 == "lasso") 
  {
    library(glmnet)
    set.seed(123)
    
    cv.out <- cv.glmnet(as.matrix(data_matrix_1[, -1]), as.numeric(data_matrix_1[, 1]), alpha = 1)
    bestlam <- cv.out$lambda.min
    fit <- cv.out$glmnet.fit
    pred1 <- predict(fit, s = bestlam, newx = as.matrix(spectra_data_test))
  }
  
  
  if(algorithm1 == "elastic_net") 
  {
    library(glmnet)
    set.seed(123)
    
    cv.out <- cv.glmnet(as.matrix(data_matrix_1[, -1]), as.numeric(data_matrix_1[, 1]), alpha = 0.5)
    bestlam <- cv.out$lambda.min
    fit <- cv.out$glmnet.fit
    pred1 <- predict(fit, s = bestlam, newx = as.matrix(spectra_data_test))
  }
  
  if(algorithm1 == "PROSPECT")
  {
    
  }
  
  if(algorithm1 == "ANN")
  {
    
  }
  
  if(algorithm1 == "cluster_plus_PLSR")
  {
    
  }
  
  if(algorithm1 == "land_class_plus_PLSR")
  {
    
  }
  
  if(algorithm1 == "Bayesian_linear_horseshoe")
  {
    library(bayesreg)
    rv.G <- bayesreg(Y~., data_matrix_1, model = "t", prior = "hs", n.samples = 1000, t.dof = 1)
    test1 = data.frame(spectra_data_test)
    colnames(test1) = colnames(data_matrix_1[,-1])
    pred1 = predict(rv.G, test1)
  }
  rmse1 = signif(RMSE_function(pred1, covariate_test1), digits =  2)
  cor1 = signif(cor_function(pred1, covariate_test1), digits = 2)
  if(algorithm1 == "Bayesian_linear_horseshoe")
  {
  out1 = list(predictions = pred1, obs = covariate_test1, beta = rv.G$beta, beta0 = rv.G$beta0, var_rank = rv.G$var.ranks, sigma2 = rv.G$sigma2, Accuracy_stats =  c(rmse1, cor1))
  }else{out1 = list(predictions = pred1, obs = covariate_test1, Accuracy_stats =  c(rmse1, cor1))}
  out1
}

# this just differs in the number of samples that we take for the Bayesian implementation and the thinning that we intend to use
apply_regression_algorithm2 <- function(algorithm1, spectra_data_train, covariate_train1, spectra_data_test, covariate_test1)
{
  Y = covariate_train1
  X = spectra_data_train
  
  data_matrix_1 <- cbind(Y, X)
  data_matrix_1 = data.frame(data_matrix_1)
  
  colnames(data_matrix_1)[-1] = colnames(spectra_data_test)
  
  data_matrix_1 = na.omit(data_matrix_1)
  
  
  if(algorithm1 == "PLSR")
  {
    library(pls)
    set.seed(123)
    ll_1 = plsr(Y ~., data = data_matrix_1, validation = "CV", scale = T, segments = 5)
    #summary(ll_1)
    RMSE_values_cv = RMSEP(ll_1)
    RMSE_values = RMSE_values_cv$val[1, , ]
    index1_min = which.min(RMSE_values[1:20])
    # validationplot(ll_1, val.type = "MSEP")
    pred1 <- predict(ll_1, spectra_data_test, ncomp = index1_min)
    pred1 = pred1[,1,1]
    
  }
  
  if(algorithm1 == "PCR")
  {
    library(pls)
    set.seed(123)
    ll_1 = pcr(Y ~., data = data_matrix_1, validation = "CV", scale = T)
    #summary(ll_1)
    RMSE_values_cv = RMSEP(ll_1)
    RMSE_values = RMSE_values_cv$val[1, , ]
    index1_min = which.min(RMSE_values[1:20])
    # validationplot(ll_1, val.type = "MSEP")
    pred1 <- predict(ll_1, spectra_data_test, ncomp = index1_min)
    pred1 = pred1[,1,1]
    
  }
  
  if(algorithm1 == "Random_Forest")
  {
    library(caret)
    library(randomForest)
    set.seed(123)
    
    ncol1 = ncol(data_matrix_1)
    grid_rf <- expand.grid(.mtry = c(ncol1/2,ncol1/3,ncol1/4))
    
    ntree = 500
    tuneResult <- train(Y ~., data = data_matrix_1, method = "rf",tuneGrid = grid_rf,ntree = ntree,
                        trControl = trainControl(method = "cv", number = 5,
                                                 #index = folds,                    #for pre-setting your own folds 
                                                 savePredictions = "final"))
    spectra_data_test = data.matrix(spectra_data_test)
    colnames(spectra_data_test) = colnames(data_matrix_1[, -1])
    pred1 = predict(tuneResult, newdata = spectra_data_test)
  }
  
  if(algorithm1 == "ridge") 
  {
    library(glmnet)
    set.seed(123)
    
    cv.out <- cv.glmnet(as.matrix(data_matrix_1[, -1]), as.numeric(data_matrix_1[, 1]), alpha = 0)
    bestlam <- cv.out$lambda.min
    fit <- cv.out$glmnet.fit
    pred1 <- predict(fit, s = bestlam, newx = as.matrix(spectra_data_test))
  }
  
  if(algorithm1 == "lasso") 
  {
    library(glmnet)
    set.seed(123)
    
    cv.out <- cv.glmnet(as.matrix(data_matrix_1[, -1]), as.numeric(data_matrix_1[, 1]), alpha = 1)
    bestlam <- cv.out$lambda.min
    fit <- cv.out$glmnet.fit
    pred1 <- predict(fit, s = bestlam, newx = as.matrix(spectra_data_test))
  }
  
  
  if(algorithm1 == "elastic_net") 
  {
    library(glmnet)
    set.seed(123)
    
    cv.out <- cv.glmnet(as.matrix(data_matrix_1[, -1]), as.numeric(data_matrix_1[, 1]), alpha = 0.5)
    bestlam <- cv.out$lambda.min
    fit <- cv.out$glmnet.fit
    pred1 <- predict(fit, s = bestlam, newx = as.matrix(spectra_data_test))
  }
  
  if(algorithm1 == "PROSPECT")
  {
    
  }
  
  if(algorithm1 == "ANN")
  {
    
  }
  
  if(algorithm1 == "cluster_plus_PLSR")
  {
    
  }
  
  if(algorithm1 == "land_class_plus_PLSR")
  {
    
  }
  
  if(algorithm1 == "Bayesian_linear_horseshoe")
  {
    library(bayesreg)
    rv.G <- bayesreg(Y~., data_matrix_1, model = "t", prior = "hs", n.samples = 500, t.dof = 1)
    test1 = data.frame(spectra_data_test)
    colnames(test1) = colnames(data_matrix_1[,-1])
    pred1 = predict(rv.G, test1)
  }
  rmse1 = signif(RMSE_function(pred1, covariate_test1), digits =  2)
  cor1 = signif(cor_function(pred1, covariate_test1), digits = 2)
  if(algorithm1 == "Bayesian_linear_horseshoe")
  {
    out1 = list(predictions = pred1, obs = covariate_test1, beta = rv.G$beta, beta0 = rv.G$beta0, var_rank = rv.G$var.ranks, sigma2 = rv.G$sigma2, Accuracy_stats =  c(rmse1, cor1))
  }else{out1 = list(predictions = pred1, obs = covariate_test1, Accuracy_stats =  c(rmse1, cor1))}
  out1
}
