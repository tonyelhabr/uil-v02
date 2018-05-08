
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

get_config <- function(sort = TRUE) {
  ret <-
    list(
      export_data = TRUE,
      dir_data = "data",
      export_viz = FALSE,
      dir_viz = "figs",
      dir_scrape = "data-raw",
      ext_scrape = "csv",
      scrape = FALSE,
      setup = TRUE,
      years = seq(2004L, 2017L),
      confs = paste0(seq(1L, 6L), "A"),
      complvls_info =
        setNames(
          c("District", "Region", "State"),
          c("district", "regional", "state")
        ),
      comps_info_list =
        setNames(
          c(
            "Calculator Applications",
            "Computer Science",
            "Mathematics",
            "Number Sense",
            "Science"
          ),
          c("cal", "csc", "mth", "num", "sci")
        ),
      years_scrape = seq(2004L, 2017L),
      confs_scrape = paste0(seq(1L, 6L), "A"),
      complvls_scrape = c("district", "regional", "state"),
      comps_scrape = c("cal", "csc", "mth", "num", "sci"),
      path_dl_uil = file.path("data-raw", "temp.html"),
      url_base_uil = "https://www.hpscience.net/results/",
      url_suffix_uil = ".php",
      xpath_uil = "/html/body/table",
      url_base_tea = "https://tea.texas.gov/",
      url_tea = "https://tea.texas.gov/acctres/sat_act_index.html",
      xpath_tea = "//tr //td //a",
      path_schools_nces_raw = file.path("data-raw", "EDGE_GEOCODE_PUBLICSCH_1516.xlsx"),
      path_schools_tea_raw = file.path("data-raw", "schools-tea-scrape.csv"),
      path_schools_uil_scrape = file.path("data-raw", "schools-uil-scrape.csv"),
      path_persons_uil_scrape = file.path("data-raw", "persons-uil-scrape.csv"),
      path_schools_uil = file.path("data", "schools-uil.csv"),
      path_persons_uil = file.path("data", "persons-uil.csv"),
      path_schools_nces = file.path("data", "schools-nces.csv"),
      path_schools_nces_join = file.path("data", "schools-nces-join.csv"),
      path_schools_tea = file.path("data", "schools-tea.csv"),
      path_functions_image = file.path("data", "functions.RData"),
      default_city = "unknown",
      default_complvl_num = 0,
      n_rnk_html = 20,
      rgx_school_filt = "CLEMENS",
      rgx_name_filt = "El.*[Hh]ab.*Ant",
      rgx_name_last_filt = "El.*[Hh]ab"
    )
  if (sort) {
    ret <- teproj::sort_named_list(ret)
  }
  ret
}

# rt <- robotstxt::robotstxt("www.hpscience.net")
# rt$bots
# rt$permissions
# rt$check()
# rt$crawldelay %>% tbl_df()

config <- get_config()
writeLines(yaml::as.yaml(config), file.path("R", "config.yml"))
rm(config)
#
# get_yml <- function(path = "_config.yml") {
#   yaml::read_yaml(path)
# }
# yml <- get_yml()

# get_config()
paths_functions <-
  list.files(
    path = "R",
    pattern = "func",
    full.names = TRUE,
    recursive = FALSE
  )
sapply(paths_functions, source, .GlobalEnv)
invisible(sapply(paths_functions, source, .GlobalEnv))

config <- get_config()
save.image(file = config$path_functions_image)
# config <- get_params()
# writeLines(yaml::as.yaml(config ), "_params.yml")
