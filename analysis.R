library(rjson)
library(purrr)

resultados <- readRDS(here::here(
    "output",
    "result_abril24.rds"
))

metadata_model <- fromJSON(
    file = here::here("metadata", "metadata_models.json")
)

metadata_config <- fromJSON(
    file = here::here("metadata", "metadata_config.json")
)

names(resultados) <- names(metadata_model)

source(here::here("R", "summary_models.R"))


models_with_pda <- sapply(
  names(metadata_model),
  FUN = function(x) {
    !is.null(metadata_model[[x]]$modelos$ppforest_new$lambda)
  }
)

models_with_pda <- names(metadata_model)[models_with_pda]




summary_result <- summary_models(resultados)


library(ggplot2)
library(ggtext)

summary_result[
    ,
    `:=`(
        error_test = round(as.numeric(error_test), 3),
        error_train = round(as.numeric(error_train), 3),
        dataset = fifelse(
          dataset %in% models_with_pda,
          paste(dataset, "with PDA"),
          dataset
        )
    )
]

p <- ggplot(
    summary_result,
    aes(x = model, xend = model, y = error_train, yend = error_test)
) +
    geom_point(aes(y = error_train), color = "blue", size = 4) +
    geom_point(aes(y = error_test), color = "red", size = 4) +
    geom_segment(aes(xend = model, y = error_test, yend = error_train), color = "purple", linewidth = 1) +
    geom_text(aes(label = "train"), vjust = -2, color = "blue", size = 3) +
    geom_text(aes(x = model, y = error_test, label = "test"), vjust = -2, color = "red", size = 3) +
    theme_minimal() +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    facet_wrap(~dataset, ncol = 4) +
    labs(
        x = "Modelo",
        y = "Error en train y test"
    ) +
    theme(legend.text = element_markdown(size = 12))

filter = summary_result[dataset != "NCI60",]
filter = summary_result[model != "rf"]

p <- ggplot(
  filter,
  aes(x = model, xend = model, y = error_train, yend = error_test)
) +
  geom_point(aes(y = error_train), color = "blue", size = 4) +
  geom_point(aes(y = error_test), color = "red", size = 4) +
  geom_segment(aes(xend = model, y = error_test, yend = error_train), color = "purple", linewidth = 1) +
  #geom_text(aes(label = "train"), vjust = -2, color = "blue", size = 3) +
  #geom_text(aes(x = model, y = error_test, label = "test"), vjust = -2, color = "red", size = 3) +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~dataset, ncol = 4) +
  labs(
    x = "Modelo",
    y = "Error en train y test"
  ) +
  theme(legend.text = element_markdown(size = 12))
p



models_to_disp = c("cart","ppforest_new","rf","ppforest")
models_to_disp = unique(summary_result$model)

resumen = dcast(
  summary_result[
    model %in% models_to_disp,
    list(
      dataset,
      model,
      error_test,
      error_train
    )
  ],
  dataset ~ model,
  value.var = c("error_test","error_train")
)

remove_prefix <- function(name) {
  sub(".*_", "", name)
}


library(gt)

names(resumen) = c("dataset",toupper(names(resumen)[names(resumen) != "dataset"]))
cols_numeric <- setdiff(names(resumen), "dataset")
resumen[sort(dataset), (cols_numeric) := lapply(.SD, function(x) {
  round(as.numeric(x),4)
}), .SDcols = cols_numeric] |> 
gt() %>%
  fmt_number(
    columns = starts_with("error"),
    decimals = 4
  ) |> 
  tab_spanner(
    label = "Training", 
    columns = starts_with("error_train")
  ) |> 
  tab_spanner(
    label = "Test",
    columns = starts_with("error_test")
  ) |> 
  cols_label_with(
    fn = remove_prefix
  ) |> 
  cols_move_to_end(
    columns = starts_with("error_test")
  )