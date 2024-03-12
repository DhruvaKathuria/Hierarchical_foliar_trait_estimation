PLSR_function = function(data_train, data_test)
{
  data_train = data_train |>
    select(trait, num_range("", 400:2400))
  library(pls)
  set.seed(123)
  ll_1 <- list()
  for(i in 1:100)
  {
    ll_1[[i]] <- plsr(trait ~ .,
                      data = data_train,
                      ncomp = i,
                      scale = T,
                      center = T)
  }
  
  ll_1 = plsr(
    trait ~ .,
    data = data_train,
    validation = "CV",
    scale = T,
    center = T,
    segments = 5
  )
  # ll_1 = plsr(
  #   trait ~ .,
  #   data = data_train,
  #   validation = "LOO",
  #   scale = T,
  #   center = T
  # )
  #summary(ll_1)
  RMSE_values_cv = RMSEP(ll_1)
  RMSE_values = RMSE_values_cv$val[1, ,]
  index1_min = which.min(RMSE_values[1:100])
  # validationplot(ll_1, val.type = "MSEP")
  
  data_test1 = data_test |>
    select(trait, num_range("", 400:2400))
  
  pred1 <- predict(ll_1, data_test1, ncomp = index1_min)
  pred1 = pred1[, 1, 1]
  
  data_test_out = data_test |>
    mutate(Prediction_PLSR = pred1,
           .after = trait)
}

# 
y_train <- yarn$density
x_train <- yarn$NIR

y_test <- yarn$density[11:28]
x_test <- yarn$NIR[11:28, ]

ncomp1 = 7 # number of components for PLSR

plsmod <- plsr(y_train ~ x_train, 
               scale = T, 
               center = T)

#plsr_algorithm_prediction <- predict(plsmod, newdata = x_test, ncomp = ncomp1)

scores_train <- scores(plsmod)
scores_test <- predict(plsmod, 
                          newdata = x_test,
                          type = "scores",
                          ncomps = ncomp1)

# get the prediction from the lm version
## training lm
input_lm_matrix <- data.frame(cbind(y_train, scores_train[,1:ncomp1]))
lm_version<- lm(y_train ~., data = input_lm_matrix)
#predicting on test set
predict_out <- predict(lm_version, data.frame(scores_test[, 1:ncomp1]))


plsmod <- plsr(y_train ~ x_train, 
               scale = T, 
               center = T)

scores_train <- scores(plsmod)
scores_test <- predict(plsmod, 
                       newdata = x_test,
                       type = "scores")

for (ncomp1 in 1:ncol(scores_train)){
  
  input_lm_matrix <- data.frame(cbind(y_train, scores_train[,1:ncomp1]))
  assign(paste0("m.", ncomp1),
         lm(y_train ~., data = input_lm_matrix))
}
objects(pattern="m\\.[1-8]")

cv_loo <- cv(models(m.1, m.2, m.3, m.4, m.5,
                        m.6, m.7, m.8, m.9, m.10,
                        m.11, m.12, m.13, m.14, m.15,
                        m.16, m.17, m.18, m.19, m.20, 
                        m.21, m.22, m.23, m.24, m.25, m.26),
                 data=input_lm_matrix, 
                 k = "loo",
                 method  = "hatvalues",
                 seed=2120)

cv.mse.loo <- sapply(cv_loo, function(x) x[["CV crit"]])
plot(cv.mse.loo, type = "b")




Hitters = na.omit(Hitters)
set.seed(1)

train = Hitters %>%
  sample_frac(0.7)

test = Hitters %>%
  setdiff(train)


plsmod <- plsr(Salary ~ ., 
               data = train,
               scale = T, 
               center = T)

scores_train <- scores(plsmod)
scores_test <- predict(plsmod, 
                       newdata = x_test,
                       type = "scores")

data_train <- cbind(train$Salary, scores_train)
colnames(data_train)[1] = "salary"
data_train <- data_train |> 
  clean_names() |> 
  data.frame()


#input_lm_matrix <- data.frame(cbind(train$Salary, 1))

assign("m.0", 
       lm(salary ~ 1., data = data_train))
for (ncomp1 in 1:ncol(scores_train)){
  
  #input_lm_matrix <- data.frame(cbind(train$Salary, scores_train[,1:ncomp1]))
  #colnames(input_lm_matrix)[1] = "Salary"
  assign(paste0("m.", ncomp1),
         lm(as.formula(paste("salary ~ 1 + ", 
                                      paste(colnames(data_train)[2: (ncomp1  + 1) ], 
                                            collapse = "+"), 
                                      sep = "")), 
            data = data_train))
}
objects(pattern="m\\.[1-8]")

cv_loo <- cv(models(m.0, m.1, m.2, m.3, m.4, m.5,
                    m.6, m.7, m.8, m.9, m.10,
                    m.11, m.12, m.13, m.14, m.15,
                    m.16, m.17, m.18 
                    #m.19, m.20, 
                   # m.21, m.22, m.23, m.24, m.25, m.26
                   ),
             data= data_train, 
             k = "loo",
             method  = "hatvalues"
             )

cv.mse.loo <- sapply(cv_loo, function(x) x[["CV crit"]])
plot(cv.mse.loo, type = "b")


set.seed(1)

train = Hitters %>%
  sample_frac(0.5)

test = Hitters %>%
  setdiff(train)


pls_fit <- plsr(Salary ~ ., 
               data = train,
               scale = T, 
               center = T)
validationplot(pls_fit, val.type = "MSEP")
RMSE_values_cv = MSEP(pls_fit)
RMSE_values = RMSE_values_cv$val[1, ,]
RMSE_values

#x_test <- test[, -1]
scores_test <- predict(plsmod, 
                       newdata = test,
                       type = "scores")
scores_test <- scores_test |> clean_names() |> data.frame()
lm_predict <- predict(m.3, scores_test)
plot(lm_predict, test$Salary)
abline(0, 1)
