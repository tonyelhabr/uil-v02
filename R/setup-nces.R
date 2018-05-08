
schools_uil <-
  config$path_schools_uil %>%
  teproj::import_path_cleanly()

schools_distinct <-
  schools_uil %>%
  distinct(school, city) %>%
  arrange(school, city)

# schools_nces ----
schools_nces_raw <-
  config$path_schools_nces_raw %>%
  teproj::import_path_cleanly()

# NOTE: This might be the same thing as region.
schools_nces_raw %>% filter(lstate == "TX") %>% count(sldu15)

schools_nces <-
  schools_nces_raw %>%
  filter(lstate == "TX") %>%
  select(
    school = name,
    county = nmcnty15,
    city = lcity,
    lat = lat1516,
    lon = lon1516
  ) %>%
  clean_county_col_at() %>%
  clean_city_col_at() %>%
  clean_school_col_at() %>%
  arrange(school) %>%
  distinct()
schools_nces

schools_nces %>%
  teproj::export_path(
    path = config$path_schools_nces,
    export = config$export_data
  )

# schools_nces_join ----
schools_nces_join <-
  schools_distinct %>%
  join_fuzzily(schools_nces, suffix_x = "", suffix_y = "_geo")
schools_nces_join

# Debugging...
schools_nces_join %>%
  count(!is.na(school))
schools_nces_join %>%
  count(school, city, school_geo, city_geo, sort = TRUE)
schools_nces_join %>%
  count(school, county, city, sort = TRUE)
schools_nces_join %>%
  count(school, county, lat, lon, sort = TRUE)
schools_nces_join %>%
  count(school, city, county, lat, lon, sort = TRUE)

get_schools_nces_bycomplvl <-
  function(x = NULL,
           y = NULL,
           complvl = c("District", "Region", "State")) {
    # x <- schools_nces_join
    # y <- schools_clean
    .complvl <- match.arg(complvl)
    x %>%
      distinct(school, county, lat, lon) %>%
      arrange(school) %>%
      group_by(school) %>%
      summarise_at(vars(county, lat, lon), funs(last)) %>%
      ungroup() %>%
      inner_join(
        y %>%
          filter(complvl == .complvl) %>%
          arrange(school, desc(year)) %>%
          group_by(school) %>%
          summarise_at(vars(complvl_num, conf), funs(last)) %>%
          ungroup() %>%
          distinct(school, complvl_num, conf),
        by = c("school")
      ) %>%
      mutate(complvl = .complvl) %>%
      arrange(school)
  }

do_get_schools_nces_bycomplvl <-
  function(x = NULL, y = NULL) {
    bind_rows(
      x %>%
        get_schools_nces_bycomplvl(y, complvl = "District"),
      x %>%
        get_schools_nces_bycomplvl(y, complvl = "Region"),
      x %>%
        get_schools_nces_bycomplvl(y, complvl = "State")
    )
  }

schools_nces_join <-
  schools_nces_join %>%
  do_get_schools_nces_bycomplvl(schools_uil)

schools_nces_join %>%
  teproj::export_path(
    path = config$path_schools_nces_join,
    export = config$export_data
  )

