library(rjson)
library(purrr)

resultados <- readRDS(here::here(
    "output",
    "result.rds"
))

metadata_model <- fromJSON(
    file = here::here("metadata", "metadata_models.json")
)

metadata_config <- fromJSON(
    file = here::here("metadata", "metadata_config.json")
)

names(resultados) <- names(metadata_model)

source(here::here("R", "summary_models.R"))



summary_result <- summary_models(resultados)


library(ggplot2)
library(ggtext)

summary_result[
    ,
    `:=`(
        error_test = round(as.numeric(error_test), 3),
        error_train = round(as.numeric(error_train), 3)
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
