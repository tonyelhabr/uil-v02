
# TODO: Convert `_info` data.frames to lists, then convert them to data.frames later
# in the cleaning functions. This makes `const` more reproducible since it
# can be converted to yaml in a more natural fashion.
get_const <- function(sort = TRUE) {
  ret <-
    list(
      export_data = TRUE,
      dir_data = "data",
      export_viz = TRUE,
      dir_viz = "figs",
      scrape = TRUE,
      create_backup = TRUE,
      timestamp_dl = Sys.time(),
      timestamp_dl_chr = paste0("-", format(Sys.time(), "%Y-%m-%d_%H-%M-%S")),
      dir_dl = "data",
      file_dl = "temp",
      ext_dl = "html",
      dir_scrape = "data",
      file_scrape_suffix = "-scrape",
      ext_scrape = "csv",
      years = seq(2004L, 2017L),
      confs = paste0(seq(1L, 6L), "A"),
      complvls = c("district", "regional", "state"),
      # complvls_info_list = setNames(c("district", "regional", "state"), c("District", "Region", "State")),
      complvls_info =
        tibble::tribble(
          ~ complvl,
          ~ complvl_name,
          "district",
          "District",
          "regional",
          "Region",
          "state",
          "State"
        ),
      comps = c("cal", "csc", "mth", "num", "sci"),
      comps_info =
        tibble::tribble(
          ~ comp,
          ~ comp_name,
          "cal",
          "Calculator Applications",
          "csc",
          "Computer Science",
          "mth",
          "Mathematics",
          "num",
          "Number Sense",
          "sci",
          "Science"
        ),
      url_base = "https://www.hpscience.net/results/",
      url_suffix = ".php",
      xpath = "/html/body/table",
      clean = TRUE,
      dir_clean = "data",
      file_clean_suffix = "-clean",
      ext_clean = "csv",
      default_city = "unknown",
      default_complvl_num = 0,
      path_schools_geo_raw = file.path("data-raw", "EDGE_GEOCODE_PUBLICSCH_1516.xlsx"),
      path_analysis_image = file.path("data", "analysis.RData")
    )
  if(sort) {
    ret <- sort_named_list(ret)
  }
  ret
}


get_params <- function(const = get_const(), sort = TRUE) {
  ret <-
    c(
      const,
      list(
        path_dl = get_path_lazily(const$dir_dl, const$file_dl, const$ext_dl),
        path_schools_scrape =
          get_path_lazily(
            const$dir_scrape,
            "schools",
            const$file_scrape_suffix,
            const$ext_scrape
          ),
        path_persons_scrape =
          get_path_lazily(
            const$dir_scrape,
            "persons",
            const$file_scrape_suffix,
            const$ext_scrape
          ),
        path_schools_scrape_ts =
          get_path_lazily(
            const$dir_scrape,
            "schools",
            const$file_scrape_suffix,
            const$timestamp_dl_chr,
            const$ext_scrape
          ),
        path_persons_scrape_ts =
          get_path_lazily(
            const$dir_scrape,
            "persons",
            const$file_scrape_suffix,
            const$timestamp_dl_chr,
            const$ext_scrape
          ),
        # years_scrape = seq(2010, 2016),
        # confs_scrape = c("4A", "5A", "6A"),
        # complvls_scrape = complvls,
        # comps_scrape = c("cal", "mth", "num"),
        years_scrape = const$years,
        confs_scrape = const$confs,
        complvls_scrape = const$complvls,
        comps_scrape = const$comps
      ),
      path_schools_clean =
        get_path_lazily(
          const$dir_clean,
          "schools",
          const$file_clean_suffix,
          const$ext_clean
        ),
      path_persons_clean =
        get_path_lazily(
          const$dir_clean,
          "persons",
          const$file_clean_suffix,
          const$ext_clean
        ),
      path_schools_geo_clean = get_path_lazily(const$dir_clean, "schools-geo-clean", const$ext_clean),
      path_schools_geo_join = get_path_lazily(const$dir_clean, "schools-geo-join", const$ext_clean)
    )
  if(sort) {
    ret <- sort_named_list(ret)
  }
  ret
}

# rt <- robotstxt::robotstxt("www.hpscience.net")
# rt$bots
# rt$permissions
# rt$check()
# rt$crawldelay %>% tbl_df()
