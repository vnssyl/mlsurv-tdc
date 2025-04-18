library(survival)
library(dplyr)

data_sim <- function(n = 500, 
                     t_max = 5, 
                     nonlinear = TRUE, 
                     high_noise = FALSE, 
                     n_extra_vars = 0, 
                     collinear = FALSE, 
                     beta = c(0.03, 0.5, 0.7, 1.2),
                     baseline_scale = 0.01,
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
  
  long_data <- time_df %>%
    left_join(baseline, by = "ID") %>%
    group_by(ID) %>%
    mutate(
      id_noise = rnorm(1, 0, 0.5),
      x_td1 = sin(time + id_noise) + rnorm(n(), 0, ifelse(high_noise, 1, 0.2))
    ) %>%
    mutate(
      x_td2 = if (collinear) {
        0.9 * x_td1 + rnorm(n(), 0, 0.1)
      } else {
        time * 0.1 + rnorm(n(), 0, ifelse(high_noise, 0.6, 0.3))
      }
    ) %>%
    ungroup()
  
  long_data <- long_data %>%
    mutate(
      sex_numeric = ifelse(sex == "male", 1, 0),
      group_numeric = ifelse(group == "treatment", 1, 0),
      interaction_term = if (interactions) x1 * x_td1 else 0,
      linpred = if (nonlinear) {
        beta[1]*age + beta[2]*x1^2 + beta[3]*sin(x_td1) + beta[4]*x_td2 + interaction_term
      } else {
        beta[1]*age + beta[2]*x1 + beta[3]*x_td1 + beta[4]*x_td2 + interaction_term
      },
      hazard = baseline_scale * exp(linpred)
    )
  
  get_event_time <- function(time_seq, haz_seq) {
    u <- runif(1)
    delta_t <- c(diff(time_seq), 0)
    cumhaz <- cumsum(haz_seq * delta_t)
    idx <- which(cumhaz > -log(u))[1]
    if (is.na(idx)) return(max(time_seq))
    return(time_seq[idx])
  }
  
  event_times <- long_data %>%
    group_by(ID) %>%
    summarise(event_time = get_event_time(time, hazard)) %>%
    ungroup() %>%
    mutate(
      censor_time = runif(n, censoring_range[1], censoring_range[2]),
      final_time = pmin(event_time, censor_time),
      event = as.numeric(event_time <= censor_time)
    )
  
  long_data <- long_data %>%
    left_join(event_times, by = "ID") %>%
    filter(time <= final_time) %>%
    group_by(ID) %>%
    mutate(
      start = lag(time, default = 0),
      stop = time,
      event_flag = as.numeric(stop == final_time & event == 1)
    ) %>%
    ungroup()
  
  return(long_data)
}

