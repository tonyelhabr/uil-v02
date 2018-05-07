

# config ----
# TODO: Convert `_info` data.frames to lists, then convert them to data.frames later
# in the cleaning functions. This makes `const` more reproducible since it
# can be converted to yaml in a more natural fashion.
get_const <- function(sort = TRUE) {
  ret <-
    list(
      export_data = TRUE,
      dir_data = "data",
      export_viz = FALSE,
      dir_viz = "figs",
      dir_scrape = "data-raw",
      ext_scrape = "csv",
      scrape = FALSE,
      clean = TRUE,
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
      path_functions_image = file.path("data", "functions.RData"),
      path_schools_geo_raw = file.path("data-raw", "EDGE_GEOCODE_PUBLICSCH_1516.xlsx"),
      # path_schools_sat_raw = file.path("data-raw", "sat_campus_data_class_2015.csv"),
      # path_schools_act_raw = file.path("data-raw", "act_campus_data_class_2015.csv"),
      path_schools_tea_raw = file.path("data-raw", "schools_tea-scrape.csv"),
      path_schools_scrape = file.path("data-raw", "schools-scrape.csv"),
      path_persons_scrape = file.path("data-raw", "persons-scrape.csv"),
      path_schools = file.path("data", "schools.csv"),
      path_persons = file.path("data", "persons.csv"),
      path_schools_geo = file.path("data", "schools-geo.csv"),
      path_schools_geo_join = file.path("data", "schools-geo-join.csv"),
      path_schools_tea = file.path("data", "schools-tea.csv"),
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

get_params <- function(const = get_const(), sort = TRUE) {
  ret <- const
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

const <- get_const()
writeLines(yaml::as.yaml(const), "_const.yml")
rm(const)
#
# get_yml <- function(path = "_const.yml") {
#   yaml::read_yaml(path)
# }
# yml <- get_yml()
