library("pam")


data_dir <- file.path(getwd(), "data")
output_path_pdf <- file.path(getwd(), "output_eilers_peeters", "eilers_peeters_plot_control.pdf")
output_dir <- file.path(getwd(), "output_eilers_peeters")

cat("loading CSV files \n")
csv_files <-
  list.files(path = data_dir,
             pattern = "\\.csv$",
             full.names = TRUE)
data_list <- list()
for (file in csv_files) {
  data_list <- append(data_list, list(list(
    file_name = basename(file),
    data = read_dual_pam_data(file)
  )))
}

cat("generating eilers_peeters model \n")
results_list <- list()
for (data_entry in data_list) {
  model_result <-
    eilers_peeters_modified(eilers_peeters_generate_regression_ETR_II(data_entry$data))
  results_list <- append(results_list, list(
    list(
      file_name = data_entry$file_name,
      data = data_entry$data,
      model_result = model_result
    )))
  cat("Processed file:", data_entry$file_name, "\n")
}

cat("generating control plot PDF \n")
pdf(output_path_pdf, onefile = TRUE)
for (result_entry in results_list) {
  title <- result_entry$file_name
  data <- result_entry$data
  model_result <- result_entry$model_result
  plot <- plot_control(
    data = data,
    model_result = model_result,
    title = title,
    color = "green"
  )
  print(plot)
  cat("Processed file:", title, "\n")
}
dev.off()

cat("exporting results to CSVs files \n")
for (result_entry in results_list) {
  file_name <- result_entry$file_name
  data <- result_entry$data
  model_result <- result_entry$model_result
  write_model_result_csv(output_dir,
                         file_name,
                         data,
                        model_result)
  cat("Processed file:", file_name, "\n")
}
