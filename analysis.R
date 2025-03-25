library(purrr)

library(rjson)
resultados <- readRDS(here::here(
    "output",
    "result_jul2.rds"
))

names_df = names(resultados[[1]][[1]])

resultados <- vroom::vroom(
  list.files(pattern = "result_[0-9]+.csv"),
  col_names = FALSE
)

names(resultados) = names_df

metadata_model <- rjson::fromJSON(
    file = here::here("metadata", "metadata_modelsv2.json")
)

metadata_config <- rjson::fromJSON(
    file = here::here("metadata", "metadata_config.json")
)

# names(resultados) <- names(metadata_model)[-length(names(metadata_model))]


source(here::here("R", "summary_models.R"))


models_with_pda <- sapply(
  names(metadata_model),
  FUN = function(x) {
    !is.null(metadata_model[[x]]$modelos$ppforest_new$lambda)
  }
)

models_with_pda <- names(metadata_model)[models_with_pda]




summary_result <- summary_models_df(resultados)


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
        ),
        family_model = fifelse(
          stringr::str_detect(model, "pptree"),
          "PPtree",
          "Classic"
        ),
        extension_pptree = fifelse(
          stringr::str_detect(
            model, 
            "mod"
          ) & stringr::str_detect(model, "pptree"),
          "Extension",
          "Classical"
        ),
        id_model = 1:.N
    )
]

best_pptree = summary_result[
  family_model == "PPtree",
  .SD[which.min(error_test)],
  by = dataset
]

best_pptree = best_pptree[
  ,
  .(
    id_model_best_of_pptree = id_model,
    family_model_best_of_pptree = family_model,
    extension_pptree_best_of_pptree = extension_pptree,
    error_test_best_of_pptree = error_test,
    error_train_best_of_pptree = error_train,
    dataset = dataset
  )
]


best_model = summary_result[
  ,
  .SD[which.min(error_test)],
  by = dataset
]

best_model = best_model[
  ,
  .(
    id_model_best = id_model,
    family_model_best_model = family_model,
    extension_pptree_best_model = extension_pptree,
    error_test_best = error_test,
    error_train_best = error_train,
    dataset = dataset
  )
]

summary_result_best = merge(
  summary_result,
  best_model,
  by = "dataset",
  all.x = TRUE
)

summary_result_final = merge(
  summary_result_best,
  best_pptree,
  by = "dataset",
  all.x = TRUE
)


summary_result_final[
  ,
  `:=`(
    extension_works = extension_pptree_best_of_pptree == "Extension",
    best_model_pptree = family_model_best_model != "Classic",
    best_model_pptree_extension = family_model_best_model != "Classic" & extension_pptree_best_model == "Extension"
  )
]



best_pptree = summary_result_final[
  best_model_pptree == TRUE,
]

saveRDS(
  best_pptree,
  here::here("app","best_pptree.rds")
)

extension_works = summary_result_final[
  extension_works == TRUE,
]

saveRDS(
  extension_works,
  here::here("app","extension_works.rds")
)

best_pptree_extension = summary_result_final[
  best_model_pptree_extension == TRUE,
]

saveRDS(
  best_pptree_extension,
  here::here(
    "app",
    "best_pptree_extension.rds"
  )
)

p <- ggplot(
    best_pptree,
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