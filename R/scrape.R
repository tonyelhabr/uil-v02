

stopifnot(params$scrape)

# library("xml2")
# library("rvest")
# tea ----
scrape_tea <-
  function(params = params, url = params$url_tea, xpath = params$xpath_tea) {
    url %>%
      xml2::read_html() %>%
      rvest::html_nodes(xpath = xpath) %>%
      rvest::html_attr("href") # %>%
      # str_subset("Campus_Data")
  }

create_url_tea <-
  function(params = NULL, url_suffix = NULL, url_base = params$url_base_tea) {
  paste0(url_base, url_suffix)
}
create_path_dl_tea <-
  function(params = NULL, url_suffix = NULL, dir = params$dir_scrape, ext = params$ext_scrape_tea) {
    url_suffix %>%
      str_replace("(\\/acctres\\/)(.*)(\\/)", "\\2") %>%
      paste0(".", ext) %>%
      file.path(dir, .)
  }

import_path_tea <-
  function(path = NULL, test = NULL) {
    ret <-
      path %>%
      # teproj::import_path_cleanly() %>%
      teproj::import_path() %>%
      rename_all(funs(tolower)) %>%
      filter(group == "All Students")

    cols_base <- c("campname", "cntyname", "regnname")
    if(test == "SAT") {
      cols_extra <- c("math", "reading", "writing", "total")
    } else if (test == "ACT") {
      cols_extra <- c("math", "reading", "english", "science", "compos")
    }
    cols <- intersect(names(ret), c(cols_base, cols_extra))
    ret <-
      ret %>%
      select(one_of(cols))
    if(test == "ACT" & ("compos" %in% names(ret))) {
      ret <-
        ret %>%
        rename(total = compos)
    }
    ret
  }

urls_tea <-
  params %>%
  scrape_tea() %>%
  str_subset("^\\/acctres\\/[:alpha:]{3}_[Cc]ampus_[Dd]ata")
urls_tea
urls_tea_dl <-
  urls_tea %>%
  walk(
    ~download.file(
      url = create_url_tea(params = params, url_suffix = .x),
      destfile = create_path_dl_tea(params = params, url_suffix = .x),
      mode = "wb"
    )
  )

schools_tea_raw <-
  urls_tea %>%
  # .[1:13] %>%
  # .[1] %>%
  # .[14:15] %>%
  create_path_dl_tea(params = params, .) %>%
  tibble(path = .) %>%
  mutate(test = toupper(str_extract(path, "([Ss][Aa][Tt])|([Aa][Cc][Tt])")),
         year = str_extract(path, "[:digit:]+")) %>%
  mutate(contents = map2(path, test, ~import_path_tea(.x, .y))) %>%
  unnest() %>%
  select(-path) %>%
  select(-total, everything(), total) %>%
  rename(school = campname, county = cntyname, city = regnname) %>%
  mutate_if(is.character, funs(str_replace_all(., "=|\"", "")))

# Debugging...
schools_tea_raw
schools_tea_raw %>%
  count(test, year, is.na(school), sort = TRUE)

teproj::export_path(
  schools_tea_raw,
  params$path_schools_tea_raw,
  export = params$export_data
)

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
          paste0(params$url_base_uil,
                year,
                "/",
                conf,
                "/",
                complvl,
                "/",
                comp,
                params$url_suffix_uil)

        # Test district page.
        # url <- "https://www.hpscience.net/results/2005/1A/district/cal.php"
        # Test district page with weird school names.
        # url <- "https://www.hpscience.net/results/2005/1A/district/sci.php"
        # Test state.
        # complvl <- "state"
        # url <- "https://www.hpscience.net/results/2005/1A/state/sci.php"

        download.file(url, destfile = params$path_dl_uil)

        results_list <-
          params$path_dl_uil %>%
          xml2::read_html() %>%
          # rvest::html_nodes("table")
          rvest::html_nodes(xpath = params$xpath_uil) %>%
          rvest::html_table(header = TRUE)

        schools_raw <- results_list[[1]]
        persons_raw <- results_list[[2]]

        schools <-
          schools_raw %>%
          clean_raw_cols_uil(complvl = complvl) %>%
          add_cols_uil()

        persons <-
          persons_raw %>%
          clean_raw_cols_uil(complvl = complvl) %>%
          add_cols_uil()

        if (i == 1) {
          schools_scrape <- schools
          persons_scrape <- persons
        } else {
          schools_scrape <- bind_rows(schools_scrape, schools)
          persons_scrape <- bind_rows(persons_scrape, persons)
        }
        l <- l + 1
      }
      k <- k + 1
    }
    j <- j + 1
  }
  i <- i + 1
}
unlink(params$path_dl_uil)

teproj::export_path(
  schools_scrape,
  params$path_schools_scrape,
  export = params$export_data
)
teproj::export_path(
  persons_scrape,
  params$path_persons_scrape,
  export = params$export_data
)

