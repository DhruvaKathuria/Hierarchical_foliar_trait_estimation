

# Uncomment below if you want to plot the horseshoe vs gaussian prior simulations

library(extraDistr)
library(ggplot2)
m_eff <- 0.05
nu <- 4
s = 2
n_sim <- 5000000

set.seed(100)
# simulating from regularized horseshoe
beta_j <- density_j <-  vector()
for(i in 1: n_sim)
{
  sigma = 1
  tau_0_sq <- m_eff *sigma
  tau_sq <- rhcauchy(n = 1, sigma = tau_0_sq)

  lambda_j <- rhcauchy(n = 1, sigma = 1)

  alpha = nu/2; beta <- (nu *s^2)/2
  c_sq <- rinvgamma(n = 1, alpha = alpha, beta = beta)

  lambda_j_tilde_sq <- (c_sq * lambda_j^2)/(c_sq + tau_sq *lambda_j^2)
  beta_j[i] <- rnorm(1, mean = 0, sd = sqrt(tau_sq * lambda_j_tilde_sq))
  density_j[i] <- dnorm(beta_j[i], mean = 0, sd = sqrt(tau_sq * lambda_j_tilde_sq))
}
hist(beta_j, 1e4, freq = FALSE, col = "red")
# simulating from normal distribution

sigma = 1
beta_j_normal <- rnorm(n_sim, mean = 0, sd = sigma)
density_j_normal <- dnorm(beta_j_normal, mean = 0, sd = sigma)
hist(beta_j_normal, 1e4, freq = FALSE, col = "red")

##make plot using ggplot

beta_df <- data.frame(value =  c(beta_j, beta_j_normal),
                      prior = rep(c("horseshoe", "gaussian"), each = n_sim))
# 
# density_df <- data.frame(value =  c(density_j, density_j_normal),
#                       prior = rep(c("horseshoe", "gaussian"), each = n_sim))
# density_df <- density_df |> bind_cols(beta_df[, -2])
# colnames(density_df) <- c("density", "prior", "beta")
# 
# 
# histogram_plot <- ggplot(beta_df, aes(x = value, color = prior)) +
#   geom_histogram(binwidth = 0.1) +
# #geom_freqpoly(linewidth = 0.5) +
#   facet_wrap(~prior) +
#   coord_cartesian (xlim = c(-5, 5))
# 
# density_plot <- ggplot(density_df, aes(x = beta, y = density,  color = prior)) +
#   #geom_histogram() +
#   geom_smooth(method = "loess", se = F, span = 0.005) +
#   #facet_wrap(~prior) +
#   coord_cartesian (ylim = c(0, 5), xlim = c(-5, 5))
# 
# #density_plot/histogram_plot
# density_plot

hist_plot <- ggplot(beta_df, aes(x = value, fill = prior)) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 0.003,        # 👈 very small bins (adjust if needed)
    alpha = 0.4,
    position = "identity"
  ) +
  coord_cartesian(xlim = c(-5, 5), ylim = c(0, 3)) +
  labs(
    x = "beta",
    y = "Density"
    #title = "Regularized Horseshoe vs Gaussian Prior"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  )

hist_plot

ggsave(filename = "paper_draft/figures/horseshoe_vs_gaussian.png",
       hist_plot,
       height = 6,
       width = 7,
       units = "in")
