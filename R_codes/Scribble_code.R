normal_group = paste(paste0("s(" , "PC", 1:i, ")") , collapse = "+")
fla_fixed  = paste("trait ~ 1 + ", normal_group, sep = "")

fla_mixed = str_glue("{fla_fixed} + (1|{group_variable})")

dat <- gamSim(6,n=200,scale=.2,dist="poisson")
b2 <- gamm(y~s(x0)+s(x1)+s(x2),family=poisson,
           data=dat,random=list(fac=~1))
plot(b2$gam,pages=1)
library(gamm4)
set.seed(0)
dat <- gamSim(1,n=400,scale=2) ## simulate 4 term additive truth
## Now add 20 level random effect `fac'...
dat$fac <- fac <- as.factor(sample(1:20,400,replace=TRUE))
dat$y <- dat$y + model.matrix(~fac-1)%*%rnorm(20)*.5
br <- gamm4(y~s(x0)+x1+s(x2),data=dat,random=~(1|fac))
plot(br$gam,pages=1)

mean_2000 <- function(x) {
  tmp <- mean(x, na.rm = T)
  tmp[abs(tmp) > 50] <- 50
  tmp
}

x11 <- pp_check(model_mixed_Bayesian, type = "stat", stat = "mean_2000", prefix = "ppd", binwidth = 0.5)
x11
x11 + coord_cartesian(xlim = c(-10, 10)) 


n <- 10000
df <- n - 1
samples <- rt(n, df)
hist(samples, breaks = 'Scott', freq = FALSE)

i = 19
normal_group = paste(paste0("s(" , "PC", 1:i, ")") , collapse = "+")
fla_mixed  = str_glue("trait ~  {normal_group})")
model_full_hierarchical <- gamm4(as.formula(fla_mixed), 
                    data = data_train_for_hierarchical_analysis,
                    random = ~(1|leaf_classification))
prediction1 <- predict(model_full$gam, data_test_for_hierarchical_analysis)
plot(prediction1, data_test_for_hierarchical_analysis$trait)
abline(0, 1)
RMSE_function(prediction1, data_test_for_hierarchical_analysis$trait)
cor_function(prediction1, data_test_for_hierarchical_analysis$trait)
