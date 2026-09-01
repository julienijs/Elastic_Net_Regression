# Get directory and files
datasets_directory <- "./ElasticNetResults/"
files <- list.files(
  datasets_directory,
  all.files = FALSE,
  full.names = FALSE
)

# Store results
dispersion_results <- data.frame()

for (file in files) {
  
  # Read file
  dat <- read.csv(file.path(datasets_directory, file))
  
  # Extract coefficients
  coeffs <- dat$coefficient
  
  # Remove missing values if present
  coeffs <- coeffs[!is.na(coeffs)]
  
  # Compute dispersion measures
  res <- data.frame(
    file = file,
    n_authors = length(coeffs),
    mean_coef = mean(coeffs),
    sd_coef = sd(coeffs),
    min_coef = min(coeffs),
    max_coef = max(coeffs),
    coef_range = max(coeffs) - min(coeffs),
    nonzero_coeffs = sum(coeffs != 0),
    prop_nonzero = mean(coeffs != 0)
  )
  
  dispersion_results <- rbind(dispersion_results, res)
}

# Order by dispersion if desired
dispersion_results <- dispersion_results[
  order(dispersion_results$sd_coef, decreasing = TRUE),
]

print(dispersion_results)

# Optional: save
write.csv(
  dispersion_results,
  "coefficient_dispersion_summary.csv",
  row.names = FALSE
)


library(ggplot2)

# Rank by SD (already sorted decreasingly)
dispersion_results$rank <- seq_len(nrow(dispersion_results))

ggplot(
  dispersion_results,
  aes(x = rank, y = sd_coef)
) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Rank (highest SD = 1)",
    y = "Standard deviation of coefficients",
    title = "Ranked coefficient dispersion across datasets"
  ) +
  theme_minimal()


ggplot(
  dispersion_results,
  aes(x = rank, y = sd_coef, label = file)
) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_text(
    data = dispersion_results[c(1:5, (nrow(dispersion_results)-4):nrow(dispersion_results)), ],
    vjust = -0.5,
    size = 3
  ) +
  theme_minimal()


ggplot(
  dispersion_results,
  aes(rank, sd_coef)
) +
  geom_point() +
  geom_line() +
  labs(
    x = "Rank",
    y = "Standard deviation"
  ) +
  theme_minimal()


dispersion_results$rank_pct <-
  (dispersion_results$rank - 1) /
  (nrow(dispersion_results) - 1)

ggplot(
  dispersion_results,
  aes(rank_pct, sd_coef)
) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Rank percentile",
    y = "Standard deviation"
  ) +
  theme_minimal()


ggplot(
  dispersion_results,
  aes(rank, sd_coef)
) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  theme_minimal()


# Rank datasets
dispersion_results$rank <- seq_len(nrow(dispersion_results))

# Fit regression
fit <- lm(sd_coef ~ rank, data = dispersion_results)

# Predicted values and residuals
dispersion_results$predicted_sd <- predict(fit)
dispersion_results$residual <- residuals(fit)

# Datasets below the regression line
below_line <- subset(dispersion_results, residual < 0)

# View them
below_line[order(below_line$residual), ]


below_line <- below_line[order(below_line$residual), ]

head(below_line[, c("file", "sd_coef", "predicted_sd", "residual")], 20)


library(ggplot2)

dispersion_results$position <- ifelse(
  dispersion_results$residual < 0,
  "Below line",
  "Above line"
)

ggplot(
  dispersion_results,
  aes(rank, sd_coef, color = position)
) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()

resid_sd <- sd(dispersion_results$residual)

outliers_below <- subset(
  dispersion_results,
  residual < -resid_sd
)

outliers_below[, c("file", "sd_coef", "predicted_sd", "residual")]

outliers_below <- subset(
  dispersion_results,
  residual < -2 * resid_sd
)
