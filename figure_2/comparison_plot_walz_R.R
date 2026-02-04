library(data.table)
library(ggplot2)
library(patchwork)
library(ggthemes)

raw_data_dir <- file.path("data")

walz_model_results <- read.csv("walz_model_results.csv")
r_eilers_peeters_dir <- file.path("output_eilers_peeters")
r_platt_dir <- file.path("output_platt")

result <- data.table()

cat("loading eilers peeters CSV files \n")
files <- list.files(
  path = raw_data_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

for (file in files) {
  file_name <- basename(file)
  r_eilers_peeters_model_result <- read.csv(file.path(r_eilers_peeters_dir, paste(file_name, "_model_result.csv", sep = "")))
  r_platt_model_result <- read.csv(file.path(r_platt_dir, paste(file_name, "_model_result.csv", sep = "")))

  walz_model_result <- walz_model_results[walz_model_results$file_name == file_name, ]

  row <- list(
    "file_name" = file_name,
    # eilers peeters
    "eilers_peeters_etr_r" = as.numeric(r_eilers_peeters_model_result$etrmax_with_photoinhibition),
    "eilers_peeters_etr_walz" = as.numeric(walz_model_result$etrmax_2_ep),
    "eilers_peeters_ik_r" = as.numeric(r_eilers_peeters_model_result$ik_with_photoinhibition),
    "eilers_peeters_ik_walz" = as.numeric(walz_model_result$ik_2_ep),
    "eilers_peeters_alpha_r" = as.numeric(r_eilers_peeters_model_result$alpha),
    "eilers_peeters_alpha_walz" = as.numeric(walz_model_result$alpha_2_ep),
    # platt
    "platt_etr_r" = as.numeric(r_platt_model_result$etrmax_with_photoinhibition),
    "platt_etr_walz" = as.numeric(walz_model_result$etrmax_2_platt),
    "platt_ik_r" = as.numeric(r_platt_model_result$ik_with_photoinhibition),
    "platt_ik_walz" = as.numeric(walz_model_result$ik_2_platt),
    "platt_alpha_r" = as.numeric(r_platt_model_result$alpha),
    "platt_alpha_walz" = as.numeric(walz_model_result$alpha_2_platt)
  )

  result <- rbind(result, row)
}

make_regression_plot <- function(df, shape, nudge_x = 0, nudge_y = 0) {
  df$id <- row.names(df)

  # outlier calculation
  df[, rel_diff := abs((value_R - value_WALZ) / value_R) * 100]
  df[, outlier := rel_diff > 3]

  # regression fit
  lm_clean <- lm(value_R ~ value_WALZ, data = df[outlier == FALSE])
  coef_fit <- coefficients(lm_clean)
  r2_fit <- summary(lm_clean)$r.squared

  # label function and r2
  label_text <- paste0(
    "y = ", round(coef_fit[2], 5), "x + ", round(coef_fit[1], 5),
    "\nR² = ", formatC(r2_fit, format = "f", digits = 6)
  )

  # axis scaling
  max <- max(c(df$value_WALZ, df$value_R), na.rm = TRUE)
  min <- min(c(df$value_WALZ, df$value_R), na.rm = TRUE)

  if (nudge_x != 0) {
    nudge_x <- max / nudge_x
  }

  if (nudge_y != 0) {
    nudge_y <- max / nudge_y
  }

  # plot generation
  ggplot(df, aes(x = value_WALZ, y = value_R)) +
    theme_base() +
    theme(
      axis.text = element_text(size = 15),
      text = element_text(size = 20),
      legend.text=element_text(size = 20),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA)
    ) +
    geom_smooth(data = df[outlier == FALSE], aes(x = value_WALZ, y = value_R), method = "lm", se = FALSE, color = "black") +
    geom_point(data = df[outlier == FALSE], aes(color = outlier), shape = shape, size = 4) +
    geom_point(data = df[outlier == TRUE], aes(color = outlier), shape = shape, size = 4) +
    geom_text(
      data = df[df$outlier == TRUE, ],
      aes(label = id),
      color = "red",
      fontface = "bold",
      nudge_x = nudge_x,
      nudge_y = nudge_y,
      size = 6
    ) +
    annotate(
      "text",
      x = max,
      y = min,
      label = label_text,
      hjust = 1,
      vjust = 0,
      size = 6
    ) +
    coord_cartesian(
      xlim = c(min, max),
      ylim = c(min, max)
    )
}

# platt
legend_platt <- scale_color_manual(
  values = c("black", "red"),
  labels = c("Inlier (Platt)", "Outlier (Platt)")
)

# platt etrmax
plot_etrmax_platt <- make_regression_plot(
  data.table(
    value_R = result$platt_etr_r,
    value_WALZ = result$platt_etr_walz
  ),
  shape = 16,
  nudge_x = 16
) + legend_platt + labs(
  title = "A",
  y = bquote(ETR[max] ~ "(" * mu * "mol electrons" ~ m^-2 ~ s^-1 * ") (R)"),
  x = bquote(ETR[max] ~ "(" * mu * "mol electrons" ~ m^-2 ~ s^-1 * ") (W)")
)

# platt ik
plot_ik_platt <- make_regression_plot(
  data.table(
    value_R = result$platt_ik_r,
    value_WALZ = result$platt_ik_walz
  ),
  shape = 16,
  nudge_x = 18
) + legend_platt + labs(
  title = "B",
  y = bquote(I[k] ~ "(" * mu * "mol photons" ~ m^-2 ~ s^-1 * ") (R)"),
  x = bquote(I[k] ~ "(" * mu * "mol photons" ~ m^-2 ~ s^-1 * ") (W)")
)

# platt alpha
plot_alpha_platt <- make_regression_plot(
  data.table(
    value_R = result$platt_alpha_r,
    value_WALZ = result$platt_alpha_walz
  ),
  shape = 16,
  nudge_y = 20
) + legend_platt + labs(
  title = "C",
  y = bquote(alpha ~ "(electrons / photons) (R)"),
  x = bquote(alpha ~ "(electrons / photons) (W)")
)

# eilers peeters
legend_eilers_peeters <- scale_color_manual(
  values = c("black", "red"),
  labels = c("Inlier (Eilers and Peeters)", "Outlier (Eilers and Peeters)")
)
# eilers peeters etrmax
plot_etrmax_eilers_peeters <- make_regression_plot(
  data.table(
    value_R = result$eilers_peeters_etr_r,
    value_WALZ = result$eilers_peeters_etr_walz
  ),
  shape = 15,
  nudge_x = 16
) + legend_eilers_peeters + labs(
  title = "D",
  y = bquote(ETR[max] ~ "(" * mu * "mol electrons" ~ m^-2 ~ s^-1 * ") (R)"),
  x = bquote(ETR[max] ~ "(" * mu * "mol electrons" ~ m^-2 ~ s^-1 * ") (W)")
)

# eilers peeters ik
plot_ik_eilers_peeters <- make_regression_plot(
  data.table(
    value_R = result$eilers_peeters_ik_r,
    value_WALZ = result$eilers_peeters_ik_walz
  ),
  shape = 15,
  nudge_y = 20
) + legend_eilers_peeters + labs(
  title = "E",
  y = bquote(I[k] ~ "(" * mu * "mol photons" ~ m^-2 ~ s^-1 * ") (R)"),
  x = bquote(I[k] ~ "(" * mu * "mol photons" ~ m^-2 ~ s^-1 * ") (W)")
)

# eilers peeters alpha
plot_alpha_eilers_peeters <- make_regression_plot(
  data.table(
    value_R = result$eilers_peeters_alpha_r,
    value_WALZ = result$eilers_peeters_alpha_walz
  ),
  shape = 15,
  nudge_x = 20
) + legend_eilers_peeters + labs(
  title = "F",
  y = bquote(alpha ~ "(electrons / photons) (R)"),
  x = bquote(alpha ~ "(electrons / photons) (W)")
)

# combo plot
platt_plots <- plot_etrmax_platt | plot_ik_platt | plot_alpha_platt
eilers_peeters_plots <- plot_etrmax_eilers_peeters | plot_ik_eilers_peeters | plot_alpha_eilers_peeters

combined_plot <- platt_plots / eilers_peeters_plots +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

print(combined_plot)
ggsave("figures_2.png", plot = combined_plot, width = 16, height = 12, dpi = 600)
