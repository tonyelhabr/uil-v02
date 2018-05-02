
stopifnot(params$clean)

schools_scrape <- params$path_schools_scrape %>% import_cleanly()
persons_scrape <- params$path_persons_scrape %>% import_cleanly()
# schools_scrape %>% filter(is.na(name))
schools_clean <-
  schools_scrape %>%
  clean_scrape_data(params = params)
persons_clean <-
  persons_scrape %>%
  clean_scrape_data(params = params) %>%
  filter(!is.na(name))

if (params$export_data) {
  readr::write_csv(schools_clean, params$path_schools_clean)
  readr::write_csv(persons_clean, params$path_persons_clean)
}

schools_geo_raw <-
  params$path_schools_geo_raw %>%
  import_cleanly()

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

if(params$export_data) {
  readr::write_csv(schools_geo_clean, params$path_schools_geo_clean)
}

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

if(params$export_data) {
  readr::write_csv(schools_geo_join, params$path_schools_geo_join)
}

