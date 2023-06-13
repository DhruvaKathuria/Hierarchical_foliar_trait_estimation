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

library(gamm4)
i = 9
normal_group_spline = paste(paste0("s(" , "PC", 1:i, ")") , collapse = "+")
normal_group = paste(paste0("PC", 1:i) , collapse = "+")
fla_mixed  = str_glue("trait ~  {normal_group}")
model_full_hierarchical <- gamm4(as.formula(fla_mixed), 
                    data = data_train_for_hierarchical_analysis,
                    random = ~ as.formula(str_glue("1|{group_variable}")) (1|leaf_classification))
prediction1_hierarchical <- predict(model_full_hierarchical$gam, data_test_for_hierarchical_analysis)
plot(prediction1_hierarchical, data_test_for_hierarchical_analysis$trait)
abline(0, 1)
RMSE_function(prediction1_hierarchical, data_test_for_hierarchical_analysis$trait)
cor_function(prediction1_hierarchical, data_test_for_hierarchical_analysis$trait)

fla_fixed <- str_glue("trait ~ 1 +  {normal_group}")
model_full <- gamm(as.formula(fla_fixed), 
                                 data = data_train_for_hierarchical_analysis
                                 )
prediction1 <- predict(model_full$gam, data_test_for_hierarchical_analysis)
plot(prediction1, data_test_for_hierarchical_analysis$trait)
abline(0, 1)
RMSE_function(prediction1, data_test_for_hierarchical_analysis$trait)
cor_function(prediction1, data_test_for_hierarchical_analysis$trait)


## Plotting values
 values1 <- c(0.5435762, 0.5398892, 0.5339266, 0.5141535, 0.5330012, 0.5313302, 
              0.5541145, 0.5602397, 0.5663288,
              0.5518800, 0.5437018, 0.5630382, 0.5572503, 0.5629692, 0.5613320, 
              0.5802892, 0.5768880)
 
 values2 <- c(0.6828608, 0.6675234, 0.5496032, 0.5342256, 0.5149531, 0.5017149, 
              0.4916859, 0.4860673, 0.4879369, 0.4924012,
              0.4970502, 0.4987683, 0.5061146, 0.5265341, 0.5243133, 
              0.5119932, 0.5129495, 0.5265289, 0.5448053, 0.5426448, 
              0.5371584, 0.5218941, 0.5158060, 0.5095058, 0.5094469, 0.5233043, 
              0.5346987, 0.5382479, 0.5489948, 0.5480377,
             0.5364568, 0.5361632, 0.5354820, 0.5303765, 0.5327357, 0.5501436, 
              0.5504777, 0.5603669, 0.5514061, 0.5435762, 0.5398892, 0.5339266,
             0.5141535, 0.5330012, 0.5313302, 0.5541145, 0.5602397, 0.5663288)
 
 
brms_list <- list()
for(i in 28:50)
{
  normal_group = paste(paste0("PC", 1:i) , collapse = "+")
  normal_group_spline = paste(paste0("s(" , "PC", 1:i, ")") , collapse = "+")
  fla_fixed = str_glue("trait ~ 1 + {normal_group}")
  fla_mixed = str_glue("trait ~ 1+  {normal_group} + (1|{group_variable})")
  
  fla_fixed_spline <- str_glue("trait ~ 1 + {normal_group_spline}")
  
  model_mixed_Bayesian =
     data_train_for_hierarchical_analysis %>%
     brm(
       #formula = as.formula(fla_mixed),
       formula = as.formula(fla_fixed_spline),
       data = .,
       family = gaussian(),
       prior = c(
         set_prior("normal(0, 0.5)", class = "b"),
         #set_prior("horseshoe()", class = "b")
         set_prior("normal(0, 0.5)", class = "Intercept")
         #set_prior("student_t(3, 0, 1)", class = "sds")
       ),
       #prior = set_prior(horseshoe(df = 3, par_ratio = 0.1)),
       
       warmup = 10000,
       iter = 20000,
       chains = 3,
       cores = 3,
       control = list(adapt_delta = 0.99, max_treedepth = 20)
       #sample_prior = "only"
     ) # bayesian hierarchical modeling
  brms_list[[i]] <- model_mixed_Bayesian
 
}

p1 <- predict(brms_list[[1]], data_test_for_hierarchical_analysis)
plot(p1[, 1], data_test_for_hierarchical_analysis$trait)
abline(0, 1)
RMSE_function(p1[,1], data_test_for_hierarchical_analysis$trait)
cor_function(p1[,1], data_test_for_hierarchical_analysis$trait)

p1_av <- pp_average(brms_list[[1]], 
                    brms_list[[2]], 
                    brms_list[[3]], 
                    brms_list[[4]], 
                    brms_list[[5]],
                    brms_list[[6]],
                    brms_list[[7]],
                    brms_list[[8]],
                    brms_list[[9]],
                    brms_list[[10]],
                    brms_list[[11]],
                    brms_list[[12]],
                    brms_list[[13]],
                    brms_list[[14]],
                    brms_list[[15]],
                    brms_list[[16]],
                    brms_list[[17]],
                    brms_list[[18]],
                    brms_list[[19]],
                    brms_list[[20]],
                    brms_list[[21]],
                    brms_list[[22]],
                     brms_list[[23]],
                     brms_list[[24]],
                     brms_list[[25]],
                     brms_list[[26]],
                     brms_list[[27]],
                    brms_list[[28]],
                    brms_list[[29]],
                    brms_list[[30]],
                     brms_list[[31]],
                     brms_list[[32]],
                     brms_list[[33]],
                    # brms_list[[34]],
                    # brms_list[[35]],
                    # brms_list[[36]],
                    # brms_list[[37]],
                    # brms_list[[38]],
                    # brms_list[[39]],
                    # brms_list[[40]],
                    # brms_list[[41]],
                    # brms_list[[42]],
                    # brms_list[[43]],
                    # brms_list[[44]],
                    # brms_list[[45]],
                    # brms_list[[46]],
                    # brms_list[[47]],
                    # brms_list[[48]],
                    # brms_list[[49]],
                    # brms_list[[50]],
                    
                    newdata = data_test_for_hierarchical_analysis,
                    weights = "bma")


plot(p1_av[, 1], data_test_for_hierarchical_analysis$trait)
abline(0, 1)
RMSE_function(p1_av[, 1], data_test_for_hierarchical_analysis$trait)
cor_function(p1_av[, 1], data_test_for_hierarchical_analysis$trait)

loo_list <- list()
for(jj in 61:84)
{
  brms_list[[jj]] <-  add_criterion(brms_list[[jj]], c("loo", "waic"))
}

loo_list <- loo_compare(brms_list[[1]], 
                        brms_list[[2]], 
                        brms_list[[3]], 
                        brms_list[[4]], 
                        brms_list[[5]], 
                        brms_list[[6]],
                        brms_list[[7]], 
                        brms_list[[8]], 
                        brms_list[[9]], 
                        brms_list[[10]], 
                        brms_list[[11]], 
                        brms_list[[12]],
                        brms_list[[13]], 
                        brms_list[[14]], 
                        brms_list[[15]], 
                        brms_list[[16]], 
                        brms_list[[17]], 
                        brms_list[[18]], 
                        brms_list[[19]], 
                        brms_list[[20]], 
                        brms_list[[21]],
                        brms_list[[22]], 
                        brms_list[[23]], 
                        brms_list[[24]], 
                        brms_list[[25]], 
                        brms_list[[26]],
                        brms_list[[27]], 
                        brms_list[[28]], 
                        brms_list[[29]], 
                        brms_list[[30]], 
                        brms_list[[31]], 
                        brms_list[[32]],
                        brms_list[[33]], 
                        brms_list[[34]], 
                        brms_list[[35]], 
                        brms_list[[36]], 
                        brms_list[[37]], 
                        brms_list[[38]],
                        brms_list[[39]], 
                        brms_list[[40]],
                        brms_list[[41]],
                        brms_list[[42]], 
                        brms_list[[43]], 
                        brms_list[[44]], 
                        brms_list[[45]], 
                        brms_list[[46]],
                        brms_list[[47]], 
                        brms_list[[48]], 
                        brms_list[[49]], 
                        brms_list[[40]], 
                        brms_list[[51]], 
                        brms_list[[52]],
                        brms_list[[53]], 
                        brms_list[[54]], 
                        brms_list[[55]], 
                        brms_list[[56]], 
                        brms_list[[57]], 
                        brms_list[[58]],
                        brms_list[[59]], 
                        brms_list[[60]],
                        brms_list[[61]],
                        brms_list[[62]], 
                        brms_list[[63]], 
                        brms_list[[64]], 
                        brms_list[[65]], 
                        brms_list[[66]],
                        brms_list[[67]], 
                        brms_list[[68]], 
                        brms_list[[69]], 
                        brms_list[[70]], 
                        brms_list[[71]], 
                        brms_list[[72]],
                        brms_list[[73]], 
                        brms_list[[74]], 
                        brms_list[[75]], 
                        brms_list[[76]], 
                        brms_list[[77]], 
                        brms_list[[78]],
                        brms_list[[79]], 
                        brms_list[[80]],
                        brms_list[[81]],
                        brms_list[[82]], 
                        brms_list[[83]], 
                        brms_list[[84]]
                        )

pp_out <- pp_average(brms_list, newdata = data_test_for_hierarchical_analysis)
myfit <- gam(y ~ s(x, bs = 'cr', k = 20),
             knots = list(x = ))



# Gamma distribution ------------------------------------------------------

brms_gamm <- brm(trait~ 1 + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, 
    data= data_train_for_hierarchical_analysis , 
    family=Gamma(link="log"),
    prior=c(prior(normal(0,2),class="Intercept"),
            prior(normal(0,2),class="b"),
            prior(gamma(0.01,0.01),class="shape")),
    chains=2,iter=1000, cores=4)

i = 50
normal_group = paste(paste0("PC", 1:i) , collapse = "+")
fla_fixed  = paste("trait ~ 1 + ", normal_group, sep = "")

#options(brms.backend = "cmdstanr")
brms_gamm <- brm(as.formula(fla_fixed), 
                 data= data_train_for_hierarchical_analysis , 
                 family=Gamma(link="log"),
                 prior=c(prior(normal(0,0.2),class="Intercept"),
                         prior(normal(0, 0.5),class="b"),
                         prior(gamma(1,1),class="shape")),
                 chains=2, warmup = 10000, iter = 20000, cores=4,
                 save_pars = save_pars(all = TRUE)
                 )

brms_normal <- brm(as.formula(fla_fixed), 
                 data= data_train_for_hierarchical_analysis , 
                 family= gaussian(),
                 prior=c(prior(normal(20,5),class="Intercept"),
                         prior(normal(0, 1),class="b")
                         ),
                 chains=2, warmup = 10000, iter = 20000, cores=4,
                 save_pars = save_pars(all = TRUE)
)
pp_check(brms_gamm, ndraws = 1000) 
pp_check(brms_normal, ndraws = 1000)


p1_pred <- predict(brms_gamm, data_test_for_hierarchical_analysis)
plot(p1_pred[, 1], data_test_for_hierarchical_analysis$trait)
abline(0, 1)

RMSE_function(p1_pred[, 1], data_test_for_hierarchical_analysis$trait)
cor_function(p1_pred[, 1], data_test_for_hierarchical_analysis$trait)


p1_pred_n <- predict(brms_normal, data_test_for_hierarchical_analysis)
plot(p1_pred_n[, 1], data_test_for_hierarchical_analysis$trait)
abline(0, 1)

y = data_train_for_hierarchical_analysis$trait
yrep = posterior_predict(brms_gamm,  ndraws = 20000)
y_rep_normal = posterior_predict(brms_normal, ndraws = 20000)
library(bayesplot)
ppc_stat(y, yrep, stat = "median")
ppc_stat(y, y_rep_normal, stat = "median")

loo1 <- loo(brms_gamm, save_psis = TRUE, cores = 4)
psis1 <- loo1$psis_object
lw <- weights(psis1)

ppc_loo_pit_overlay(y, yrep, lw = lw)

keep_obs <- 1:50
ppc_loo_intervals(y, yrep, psis_object = psis1, subset = keep_obs)


loo1_n <- loo(brms_normal, save_psis = TRUE, cores = 4)
psis1_n <- loo1_n$psis_object
lw_n <- weights(psis1_n)

ppc_loo_pit_overlay(y, yrep, lw = lw_n)

loo_c <- loo_compare(loo1, loo1_n)


k1 <- psis1$log_weights
