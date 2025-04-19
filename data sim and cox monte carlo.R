# create grid
grid <- expand.grid(
  nonlinear = c(TRUE, FALSE),
  interactions = c(TRUE, FALSE),
  high_noise = c(TRUE, FALSE),
  n_extra_vars = c(0, 10),
  replicate = 1:30  
)

# init results df
results <- data.frame(
  replicate = integer(),
  nonlinear = logical(),
  interactions = logical(),
  high_noise = logical(),
  n_extra_vars = integer(),
  c_index = numeric(),
  stringsAsFactors = FALSE
)

# loop through each scenario
for (i in 1:nrow(grid)) {
  params <- grid[i, ]
  
  # step 1: data simulation
  sim_data <- tryCatch({
    data_sim(
      seed = params$replicate,
      nonlinear = params$nonlinear,
      interactions = params$interactions,
      high_noise = params$high_noise,
      n_extra_vars = params$n_extra_vars,
      beta = c(0.05, 0.7, 1.5, 0.8)
    )
  }, error = function(e) {
    message(paste("Simulation error (replicate", params$replicate, "):", e$message))
    return(NULL)
  })
  
  if (!is.null(sim_data)) {
    
    # step 2: fit cox
    fit <- tryCatch({
      cox_fit(
        sim_data,
        use_squared = params$nonlinear,
        use_interactions = params$interactions
      )
    }, error = function(e) {
      message(paste("Model fitting error (replicate", params$replicate, "):", e$message))
      return(NULL)
    })
    
    if (!is.null(fit)) {
      
      # step 3: store results
      results <- rbind(
        results,
        data.frame(
          replicate = params$replicate,
          nonlinear = params$nonlinear,
          interactions = params$interactions,
          high_noise = params$high_noise,
          n_extra_vars = params$n_extra_vars,
          c_index = fit$c_index
        )
      )
    }
    
    # step 4: download simulated data to use for fitting rsf and xgbst
    base_name <- paste0(
      "rep", params$replicate,
      "_nonlin", params$nonlinear,
      "_int", params$interactions,
      "_noise", params$high_noise,
      "_extra", params$n_extra_vars
    )
    write.csv(sim_data, file = file.path("sim_datasets", paste0(base_name, ".csv")), row.names = FALSE)
  }
  
  if (i %% 10 == 0) message("Completed ", i, " of ", nrow(grid), " scenarios...")
}

# results summary

results_summary <- results %>%
  group_by(nonlinear, interactions, high_noise, n_extra_vars) %>%
  summarise(
    mean_cindex = mean(c_index),
    sd_cindex = sd(c_index),
    mce_cindex = sd_cindex / sqrt(n()),  # Monte Carlo Error
    .groups = "drop"
  )
