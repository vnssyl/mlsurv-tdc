library(survival)
library(dplyr)

data_sim <- function(n = 500, 
                     t_max = 5, 
                     nonlinear = TRUE, 
                     high_noise = FALSE, 
                     n_extra_vars = 0, 
                     collinear = FALSE, 
                     interactions = FALSE,
                     beta = c(0.03, 0.2, 0.7),
                     baseline_scale = 0.01,
                     censoring_range = c(3, 5),
                     seed = NULL) {
  
  # Function to simulate one dataset 
  # Arguments:
  #   n               - Number of individuals to simulate
  #   t_max           - Maximum follow-up time
  #   nonlinear       - Whether to include non-linear terms in the hazard model
  #   high_noise      - Whether to increase noise in time-dependent covariates
  #   n_extra_vars    - Number of extra baseline covariates to simulate (noise only)
  #   collinear       - Whether to make x_td2 highly correlated with x_td1
  #   interactions    - Whether to include interaction terms in the linear predictor
  #   beta            - Coefficients for the covariates in the linear predictor (length 4)
  #   baseline_scale  - Multiplier to control the overall magnitude of the hazard
  #   censoring_range - Range for uniform administrative censoring times
  #   seed            - Optional random seed for reproducibility
  
  
  if (!is.null(seed)) set.seed(seed)
  
  generate_time_points <- function(t_max) {
    # ensures generated times are strictly incr with positive gaps
    repeat {
      times <- sort(round(runif(sample(4:8, 1), min = 0.1, max = t_max), 1))
      if (length(unique(times)) > 1 && all(diff(times) > 0)) break
    }
    return(times)
  }
  
  # create long dataset w time points
  time_list <- lapply(1:n, function(i) data.frame(ID = i, time = generate_time_points(t_max)))
  time_df <- do.call(rbind, time_list)
  
  # generate baseline covariates
  baseline <- data.frame(
    ID = 1:n,
    age = rnorm(n, 60, 10),
    x1 = rnorm(n),
    sex = factor(sample(c("male", "female"), n, replace = TRUE)),
    group = factor(sample(c("control", "treatment"), n, replace = TRUE))
  )
  
  # optional noise params (not incl in linpred)
  if (n_extra_vars > 0) {
    for (i in 1:n_extra_vars) {
      baseline[[paste0("z", i)]] <- rnorm(n)
    }
  }
  
  # merge baseline covariates, generate time dependent covariates
  long_data <- time_df %>%
    left_join(baseline, by = "ID") %>%
    group_by(ID) %>%
    mutate(
      id_noise = rnorm(1, 0, 0.5), # patients pecific noise
      x_td1 = sin(time + id_noise) + if (high_noise) rnorm(n(), 0, 1) else 0
    ) %>%

    ungroup()
  
  # create linear predictor and hazard fcn
  long_data <- long_data %>%
    mutate(
      sex_numeric = ifelse(sex == "male", 1, 0),
      group_numeric = ifelse(group == "treatment", 1, 0),
      interaction_term = if (interactions) x1 * x_td1 else 0, # optional interaction
      linpred = if (nonlinear) {
        beta[1]*age + beta[2]*x1^2 + beta[3]*sin(x_td1) + interaction_term
      } else {
        beta[1]*age + beta[2]*x1 + beta[3]*x_td1 + interaction_term
      },
      hazard = baseline_scale * exp(linpred)
    )
  
  # simulates time to event based on cumhaz
  get_event_time <- function(time_seq, haz_seq) {
    u <- runif(1)
    delta_t <- c(diff(time_seq), 0)
    cumhaz <- cumsum(haz_seq * delta_t)
    idx <- which(cumhaz > -log(u))[1] # first time cumhaz exceeds threshold
    if (is.na(idx)) return(max(time_seq)) # no event before end of fu
    return(time_seq[idx])
  }
  
  # simulate event and censoring times
  event_times <- long_data %>%
    group_by(ID) %>%
    summarise(event_time = get_event_time(time, hazard)) %>%
    ungroup() %>%
    mutate(
      censor_time = runif(n, censoring_range[1], censoring_range[2]),
      final_time = pmin(event_time, censor_time),
      event = as.numeric(event_time <= censor_time)
    )
  
  # finalize counting process 
  long_data <- long_data %>%
    left_join(event_times, by = "ID") %>%
    filter(time <= final_time & time > 0 ) %>%
    group_by(ID) %>%
    mutate(
      start = lag(time, default = 0),
      stop = time,
      .keep = "all",
      event_flag = as.numeric(stop == final_time & event == 1)) %>%
    ungroup()
  
  return(long_data)
}

data_example <- data_sim(seed = 42, interactions = TRUE)