full_dataset <- readRDS(here::here(
    "input",
    "full_dataset.rds"
))

clases_columnas <- sapply(full_dataset[, names(full_dataset) != "Type"], class)

resultados_pca <- prcomp(full_dataset[, names(full_dataset) != "Type"])

mnist <- data.frame(
   resultados_pca$x[, 1:10]
)

mnist$Type <- as.factor(full_dataset$Type)



library(rjson)
library(PPforest)
library(PPtreeViz)
library(randomForest)
library(e1071)
library(rpart)
library(PPtreeExt)
library(here)

install_packages <- FALSE

if (install_packages) {
    cat("Instalando paquetes")
    source("R/config_packages.R")
}

metadata_model <- fromJSON(
    file = here(
        "metadata",
        "metadata_models.json"
    )
)

metadata_config <- fromJSON(
    file = here::here(
        "metadata",
        "metadata_config.json"
    )
)

source(here::here("R", "funs_comparacion.R"))


datasets <- names(metadata_model)
models <- names(metadata_config)
models <- models[models != "hhcartr"]

datasets <- datasets[datasets != "mnist"]


# Ya quedo vieja (Sin interfaz)
tree = PPTree::train(
    as.matrix(mnist[, -1]),
    mnist$Type
)

# Nueva versión 

system.time(
    {
        tree = PPTree::PPTree(
            formula = Type ~ .,
            data = mnist[s,]
        )
    }
)

system.time(
    {
        tree_viz = PPtreeViz::PPTreeclass(
            formula = Type ~ .,
            data = crab[s,],
            PPmethod = "LDA"
        )
    }
)
set.seed(1)

s = sample(1:nrow(mnist), length(mnist$Type) * 2 / 3, replace = FALSE)


mnist$Type = as.factor(mnist$Type)

system.time({
    forest <- PPTree::PPForest(
        formula = Type ~ .,
        data = mnist[s,],
        size = 1000
    )
})

system.time({
    tree <- PPTree::PPTree(
        formula = Type ~ .,
        data = mnist[s, ]
    )
})

system.time(
    {
        rforest = ranger::ranger(
            formula = Type ~ .,
            data = mnist[s,],
            num.trees = 1000
        )
    }
)


test = mnist[-s,]

predictions = predict(forest, test)
tr <- mnist[s, ]
te <- mnist[-s, ]

rf.err(rforest, tr, te)

tab.tr = table(mnist[s, "Type"], predict(forest, tr))
tab.te = table(mnist[-s, "Type"], predict(forest, te))

tab.tr <- table(tr[, "Type"], predict(tree, tr))
tab.te <- table(te[, "Type"], predict(tree, te))

tab.tr_viz <- table(mnist[s, 1], predict(tree_viz, tr))
tab.te_viz <- table(mnist[-s, 1], predict(tree_viz, te))



data.frame(
    err.tr = (dim(tr)[1] - sum(diag(tab.tr))) / dim(tr)[1],
    err.te = (dim(te)[1] - sum(diag(tab.te))) / dim(te)[1]
)

pptr.err(tree_viz, tr, te)



result <- purrr::map(
    .x = datasets,
    .f = function(x) {
        purrr::map(
            .x = models,
            .f = function(y) {
                set.seed(17)
                message(crayon::bgBlue(
                    glue::glue("Evaluando modelo {y} en dataset {x}")
                ))


                evaluate_models(
                    model = y,
                    dataset = x,
                    metadata_config = metadata_config,
                    metadata_model = metadata_model,
                    n_rep = 1,
                    n_size = 2 / 3
                )
            }
        )
    }
)

saveRDS(
    result,
    file = here::here(
        "output",
        "result_minst.rds"
    )
)

## Verificar memoria, limpiar ambiente para poder correrlo. Darle mas memoria a R