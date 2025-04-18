library(survival)
library(dplyr)

data_sim <- function(n = 500, 
                     t_max = 5, 
                     nonlinear = TRUE, 
                     high_noise = FALSE, 
                     n_extra_vars = 0, 
                     collinear = FALSE, 
                     beta = c(0.03, 0.5, 0.7, 1.2),
                     censoring_range = c(3, 5),
                     seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  generate_time_points <- function(t_max) {
    sort(round(runif(sample(4:8, 1), min = 0.1, max = t_max), 1))
  }
  
  time_list <- lapply(1:n, function(i) data.frame(ID = i, time = generate_time_points(t_max)))
  time_df <- do.call(rbind, time_list)
  
  baseline <- data.frame(
    ID = 1:n,
    age = rnorm(n, 60, 10),
    x1 = rnorm(n),
    sex = factor(sample(c("male", "female"), n, replace = TRUE)),
    group = factor(sample(c("control", "treatment"), n, replace = TRUE))
  )
}

