
rm(list = ls())
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

# library("dplyr")
# library("rlang")
# library("stringr")
# library("tidyr")
# library("ggplot2")
library("tidyverse")
library("rlang")
library("teplot")
library("teproj")
library("tetidy")

paths_functions <-
  list.files(
    path = "R",
    pattern = "func",
    full.names = TRUE,
    recursive = FALSE
  )
sapply(paths_functions, source, .GlobalEnv)
invisible(sapply(paths_functions, source, .GlobalEnv))

params <- get_params()
save.image(file = params$path_functions_image)
# params <- get_params()
# writeLines(yaml::as.yaml(params ), "_params.yml")
