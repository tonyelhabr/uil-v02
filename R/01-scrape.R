

clean_raw_data <-
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

add_cols <-
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
while (i <= length(params$years_scrape)) {
  year <- params$years_scrape[i]
  # conf <- 2012
  j <- 1
  k <- 1
  l <- 1
  while (j <= length(params$confs_scrape)) {
    conf <- params$confs_scrape[j]
    # conf <- "4A"
    k <- 1
    l <- 1

    while (k <= length(params$complvls_scrape)) {
      complvl <- params$complvls_scrape[k]
      # complvl <- "state"
      l <- 1
      while (l <= length(params$comps_scrape)) {
        comp <- params$comps_scrape[l]
        # comp <- "mth"
        url <-
          paste0(params$url_base,
                year,
                "/",
                conf,
                "/",
                complvl,
                "/",
                comp,
                params$url_suffix)

        # Test district page.
        # url <- "https://www.hpscience.net/results/2005/1A/district/cal.php"
        # Test district page with weird school names.
        # url <- "https://www.hpscience.net/results/2005/1A/district/sci.php"
        # Test state.
        # complvl <- "state"
        # url <- "https://www.hpscience.net/results/2005/1A/state/sci.php"

        download.file(url, destfile = params$path_dl)

        results_list <-
          params$path_dl %>%
          xml2::read_html() %>%
          # rvest::html_nodes("table")
          rvest::html_nodes(xpath = params$xpath) %>%
          rvest::html_table(header = TRUE)

        schools_raw <- results_list[[1]]
        persons_raw <- results_list[[2]]

        schools <-
          schools_raw %>%
          clean_raw_data(complvl = complvl) %>%
          add_cols()

        persons <-
          persons_raw %>%
          clean_raw_data(complvl = complvl) %>%
          add_cols()

        if (i == 1) {
          schools_all <- schools
          persons_all <- persons
        } else {
          schools_all <- bind_rows(schools_all, schools)
          persons_all <- bind_rows(persons_all, persons)
        }
        l <- l + 1
      }
      k <- k + 1
    }
    j <- j + 1
  }
  i <- i + 1
}
unlink(params$path_dl)


if (params$export) {
  readr::write_csv(schools_all, params$path_schools_scrape)
  readr::write_csv(persons_all, params$path_persons_scrape)

  if (params$create_backup) {
    file.copy(params$path_schools_scrape, params$path_schools_scrape_ts)
    file.copy(params$path_persons_scrape, params$path_persons_scrape_ts)
  }
}
