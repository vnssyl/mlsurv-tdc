library(survival)
library(dplyr)

cox_fit <- function(sim_data, use_squared = FALSE, use_interactions = FALSE) {
  
  # building model formula
  terms <- c("age")
  
  if (use_squared) {
    sim_data <- sim_data %>% mutate(x1_sq = x1^2)
    terms <- c(terms, "x1_sq")
  } else {
    terms <- c(terms, "x1")
  }
  
  terms <- c(terms, "x_td1")
  
  if (use_interactions) {
    sim_data <- sim_data %>% mutate(interaction = x1 * x_td1)
    terms <- c(terms, "interaction")
  }
  
  formula <- as.formula(paste("Surv(start, stop, event_flag) ~", paste(terms, collapse = " + ")))
  
  # fit model
  cox_model <- coxph(formula, data = sim_data, x = TRUE)
  
  # predict
  sim_data$lp <- predict(cox_model, newdata = sim_data, type = "lp")
  
  # c-index
  eval_data <- sim_data %>%
    group_by(ID) %>%
    slice_tail(n = 1) %>%
    ungroup()
  
  c_index <- concordance(Surv(final_time, event) ~ lp, data = eval_data)$concordance
  
  return(list(
    model = cox_model,
    c_index = c_index,
    eval_data = eval_data
  ))
}