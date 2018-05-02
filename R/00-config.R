
rm(list = ls())
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

library("dplyr")
library("rlang")
library("stringr")
library("tidyr")
library("ggplot2")
library("teplot")

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

# params <- get_params()
# writeLines(yaml::as.yaml(params ), "_params.yml")

purrr::flatten(params)
params$complvls_info_list %>% tibble::enframe()
sort_named_list(params)
