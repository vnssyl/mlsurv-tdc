library(dplyr)
library(readr)
library(purrr)

summarize_dataset <- function(df) {
  df %>%
    summarise(
      n_patients = n_distinct(ID),
      n_observations = n(),
      event_rate = round(mean(event), 3),
      censor_rate = round(1 - mean(event), 3),
      median_time = round(median(time), 2),
      max_time = round(max(time), 2)
    )
}

summarize_all_files <- function(folder_path) {
  files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  summary_df <- map_dfr(files, function(file) {
    df <- read_csv(file, show_col_types = FALSE)
    summary <- summarize_dataset(df)
    summary$filename <- basename(file)
    summary
  })
  
  return(summary_df)
}

folder_path <- "sim_datasets"

# summary of each dataset
summary_results <- summarize_all_files(folder_path)

print(summary_results)
write_csv(summary_results, "data_summary.csv")

# overall summary statistics
overall_summary <- summary_results %>%
  summarise(
    mean_n_patients = mean(n_patients),
    mean_n_obs = mean(n_observations),
    mean_event_rate = mean(event_rate),
    sd_event_rate = sd(event_rate),
    mean_censor_rate = mean(censor_rate),
    mean_median_time = mean(median_time),
    mean_max_time = mean(max_time)
  )

print(overall_summary)
write_csv(overall_summary, "overall_data_summary.csv")

