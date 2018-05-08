
stopifnot(config$scrape)

# tea ----
scrape_tea <-
  function(config = config, url = config$url_tea, xpath = config$xpath_tea) {
    url %>%
      xml2::read_html() %>%
      rvest::html_nodes(xpath = xpath) %>%
      rvest::html_attr("href") # %>%
    # str_subset("Campus_Data")
  }

create_url_tea <-
  function(config = NULL, url_suffix = NULL, url_base = config$url_base_tea) {
    paste0(url_base, url_suffix)
  }
create_path_dl_tea <-
  function(config = NULL, url_suffix = NULL, dir = config$dir_scrape, ext = config$ext_scrape_tea) {
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
  config %>%
  scrape_tea() %>%
  str_subset("^\\/acctres\\/[:alpha:]{3}_[Cc]ampus_[Dd]ata")
urls_tea
urls_tea_dl <-
  urls_tea %>%
  walk(
    ~download.file(
      url = create_url_tea(config = config, url_suffix = .x),
      destfile = create_path_dl_tea(config = config, url_suffix = .x),
      mode = "wb"
    )
  )

schools_tea_raw <-
  urls_tea %>%
  # .[1:13] %>%
  # .[1] %>%
  # .[14:15] %>%
  create_path_dl_tea(config = config, .) %>%
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
  config$path_schools_uil_tea_raw,
  export = config$export_data
)
