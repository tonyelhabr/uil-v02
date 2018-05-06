
stopifnot(params$clean)

schools_scrape <-
  params$path_schools_scrape %>%
  teproj::import_path_cleanly()
persons_scrape <-
  params$path_persons_scrape %>%
  teproj::import_path_cleanly()

schools_clean <-
  schools_scrape %>%
  clean_scrape_data(params = params) %>%
  add_calc_cols_by_at()

persons_clean <-
  persons_scrape %>%
  clean_scrape_data(params = params) %>%
  filter(!is.na(name)) %>%
  add_calc_cols_by_at() %>%
  add_name_cols_at() %>%
  filter(!is.na(name)) %>%
  select(name, name_first, name_last, everything())

schools_clean %>%
  teproj::export_path(
    path = params$path_schools_clean,
    export = params$export_data
  )

persons_clean %>%
  teproj::export_path(
    path = params$path_persons_clean,
    export = params$export_data
  )

schools_geo_raw <-
  params$path_schools_geo_raw %>%
  teproj::import_path_cleanly()

schools_geo_clean <-
  schools_geo_raw %>%
  filter(lstate == "TX") %>%
  select(
    school = name,
    state = lstate,
    county = nmcnty15,
    city = lcity,
    lat = lat1516,
    lon = lon1516
  ) %>%
  mutate_at(vars(county), funs(gsub(" County", "", .))) %>%
  mutate_at(vars(state, county), funs(tolower)) %>%
  mutate_at(vars(school), funs(stringr::str_replace_all(., "[Hh]\\s*[Ss]$", ""))) %>%
  mutate_at(vars(school), funs(stringr::str_trim(.))) %>%
  arrange(school) %>%
  distinct()
schools_geo_clean

schools_geo_clean %>%
  teporj::export_path(
    path = params$path_schools_geo_clean
    export = params$export_data
  )

schools_geo_join <-
  schools_clean %>%
  arrange(school, city) %>%
  distinct(school, city) %>%
  fuzzyjoin::stringdist_inner_join(
    schools_geo_clean %>%
      rename_at(vars(school, city), funs(paste0(., "_geo"))),
    by = c("school" = "school_geo"),
    # distance_col = "dist",
    max_dist = 1
  )
schools_geo_join

# Debugging...
schools_geo_join %>%
  count(!is.na(school))
schools_geo_join %>%
  count(school, city, school_geo, city_geo, sort = TRUE)
schools_geo_join %>%
  count(school, county, city, sort = TRUE)
schools_geo_join %>%
  count(school, county, lat, lon, sort = TRUE)
schools_geo_join %>%
  count(school, city, county, lat, lon, sort = TRUE)

# Break it down by complvl.
schools_geo_join <-
  schools_geo_join %>%
  do_get_schools_geo_bycomplvl(schools_clean)

schools_geo_join %>%
  teporj::export_path(
    path = params$path_schools_geo_join
    export = params$export_data
  )

