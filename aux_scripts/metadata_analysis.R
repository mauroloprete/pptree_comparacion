metadata_modelsv3 <- jsonlite::read_json(
  here::here(
    "metadata",
    "metadata_modelsv3.json"
  )
)


metadata_df <- do.call(rbind, lapply(
  X = metadata_modelsv3,
  FUN = function(x) data.frame(
    custom_name = x$custom_name[[1]],
    equal_rows = x$equal_rows[[1]],
    equal_vars = x$equal_vars[[1]],
    q_rows = x$q_rows[[1]],
    q_vars = x$q_vars[[1]],
    rows_with_na = x$rows_with_na[[1]],
    stringsAsFactors = FALSE
  )
))

metadata_df$dataset_code <- rownames(metadata_df)

dataset_grid <- readxl::read_excel(path = here::here("input/paper_consistency/classification_datasets.xlsx"))

metadata_df <- data.table::as.data.table(metadata_df)

metadata_df_full <- merge(metadata_df, dataset_grid, by.x = "dataset_code", by.y = "Data")






