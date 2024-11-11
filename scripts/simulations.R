library(foreach)
library(parallel)

# Set simulation parameters
n <- 500  # sample size
nsim <- 1000  # number of simulations
alpha_level <- 0.05  # significance level

# Function to run a single simulation
run_simulation <- function(alpha1, seed) {
  set.seed(seed)
  
  # Set other parameters
  pz <- 0.2
  alpha0 <- 0
  beta0 <- -3
  beta1 <- 0
  beta2 <- 2
  
  # Generate data
  z <- rbinom(n, size = 1, prob = pz)
  px <- exp(alpha0 + alpha1 * z) / (1 + exp(alpha0 + alpha1 * z))
  x <- rbinom(n, size = 1, prob = px)
  py <- exp(beta0 + beta1 * x + beta2 * z) / (1 + exp(beta0 + beta1 * x + beta2 * z))
  y <- rbinom(n, size = 1, prob = py)
  
  # Fit models
  unadj_mod <- glm(y ~ x, family = "binomial")
  adj_mod <- glm(y ~ x + z, family = "binomial")
  
  # Check if null hypothesis is rejected
  unadj_reject <- summary(unadj_mod)$coef["x", "Pr(>|z|)"] < alpha_level
  adj_reject <- summary(adj_mod)$coef["x", "Pr(>|z|)"] < alpha_level
  
  return(c(unadj_reject, adj_reject))
}

# Function to run simulations for a given alpha1
run_simulations_for_alpha1 <- function(alpha1) {
  results <- mclapply(1:nsim, function(i) run_simulation(alpha1, i), mc.cores = detectCores())
  results_matrix <- do.call(rbind, results)
  type_i_error_rates <- colMeans(results_matrix)
  return(type_i_error_rates)
}

# Run simulations for different alpha1 values
alpha1_values <- c(0, 1, 2)
simulation_results <- lapply(alpha1_values, run_simulations_for_alpha1)

# Create results table
results_table <- data.frame(
  alpha1 = alpha1_values,
  Unadjusted = sapply(simulation_results, `[`, 1),
  Adjusted = sapply(simulation_results, `[`, 2)
)

# Print results table
print(results_table)

# Create a formatted table for the report
formatted_table <- knitr::kable(results_table, 
                                format = "markdown",
                                col.names = c("α₁", "Unadjusted", "Adjusted"),
                                caption = "Type I Error Rates",
                                digits = 4)

# Print the formatted table
print(formatted_table)