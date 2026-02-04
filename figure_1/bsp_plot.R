library("pam")
library("ggplot2")

csv_path <- file.path(getwd(), "20260201_08_tomato.csv")

cat("loading CSV file \n")
data <- read_dual_pam_data(csv_path)

cat("generating platt model \n")
model_result <-
  platt_modified(platt_generate_regression_ETR_II(data))

cat("generating plot control \n")
plot <- plot_control(
  data = data,
  model_result = model_result,
  title = "20260201_08_tomato.csv",
  color = "blue"
)

print(plot)

cat("saving plot control \n")
ggsave(
  "figure_1.jpg",
  plot = plot,
  width = 8,
  height = 8,
  dpi = 600
)
