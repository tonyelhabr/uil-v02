
stopifnot(config$scrape)

# uil ----
clean_raw_cols_uil <-
  function(data = NULL,
           complvl = NULL) {
    ret <-
      data %>%
      as_tibble() %>%
      janitor::clean_names()

    if (complvl == "district") {
      ret <-
        ret %>% rename(school_city_complvlnum = school_city_district)
    } else if (complvl == "regional") {
      ret <-
        ret %>% rename(school_city_complvlnum = school_city_region)
    } else if (complvl == "state") {
      ret <- ret %>% rename(school_city_complvlnum = school_city)
    } else {
      stop("Invalid `complvl`", call. = FALSE)
    }
    ret
  }

add_cols_uil <-
  function(data = NULL) {
    data %>%
      mutate(
        year = year,
        conf = conf,
        complvl = complvl,
        comp = comp,
        url = url
      )
  }


# loop ----
i <- 1
j <- 1
k <- 1
l <- 1
while (i <= length(config$years_scrape)) {
  year <- config$years_scrape[i]
  # conf <- 2012
  j <- 1
  k <- 1
  l <- 1
  while (j <= length(config$confs_scrape)) {
    conf <- config$confs_scrape[j]
    # conf <- "4A"
    k <- 1
    l <- 1

    while (k <= length(config$complvls_scrape)) {
      complvl <- config$complvls_scrape[k]
      # complvl <- "state"
      l <- 1
      while (l <= length(config$comps_scrape)) {
        comp <- config$comps_scrape[l]
        # comp <- "mth"
        url <-
          paste0(config$url_base_uil,
                year,
                "/",
                conf,
                "/",
                complvl,
                "/",
                comp,
                config$url_suffix_uil)

        # Test district page.
        # url <- "https://www.hpscience.net/results/2005/1A/district/cal.php"
        # Test district page with weird school names.
        # url <- "https://www.hpscience.net/results/2005/1A/district/sci.php"
        # Test state.
        # complvl <- "state"
        # url <- "https://www.hpscience.net/results/2005/1A/state/sci.php"

        download.file(url, destfile = config$path_dl_uil)

        results_list <-
          config$path_dl_uil %>%
          xml2::read_html() %>%
          # rvest::html_nodes("table")
          rvest::html_nodes(xpath = config$xpath_uil) %>%
          rvest::html_table(header = TRUE)

        schools_raw <- results_list[[1]]
        persons_raw <- results_list[[2]]

        schools_uil <-
          schools_raw %>%
          clean_raw_cols_uil(complvl = complvl) %>%
          add_cols_uil()

        persons_uil <-
          persons_raw %>%
          clean_raw_cols_uil(complvl = complvl) %>%
          add_cols_uil()

        if (i == 1) {
          schools_scrape <- schools_uil
          persons_scrape <- persons_uil
        } else {
          schools_scrape <- bind_rows(schools_scrape, schools_uil)
          persons_scrape <- bind_rows(persons_scrape, persons_uil)
        }
        l <- l + 1
      }
      k <- k + 1
    }
    j <- j + 1
  }
  i <- i + 1
}
unlink(config$path_dl_uil)

teproj::export_path(
  schools_scrape,
  config$path_schools_uil_scrape,
  export = config$export_data
)
teproj::export_path(
  persons_scrape,
  config$path_persons_uil_scrape,
  export = config$export_data
)

