cran_packages <- c("Formula", "MASS", "Matrix", "PPforest", "PPtreeViz", "R6", "RColorBrewer", "Rcpp", "RcppArmadillo", "class", "cli", "codetools", "colorspace", "cpp11", "crayon", "data.table", "doParallel", "dplyr", "e1071", "fansi", "farver", "foreach", "generics", "ggplot2", "glue", "gridExtra", "gtable", "here", "inum", "isoband", "iterators", "jsonlite", "labeling", "lattice", "libcoin", "lifecycle", "magrittr", "mgcv", "munsell", "mvtnorm", "nlme", "partykit", "pillar", "pkgconfig", "plyr", "proxy", "purrr", "randomForest", "renv", "rjson", "rlang", "rpart", "rprojroot", "scales", "stringi", "stringr", "survival", "tibble", "tidyr", "tidyselect", "utf8", "vctrs", "viridisLite", "withr","remotes","pacman")

install.packages(cran_packages)

if (!require(PPtreeExt)) {
    github_packages <- "natydasilva/PPtreeExt"

    remotes::install_github(github_packages)
}

