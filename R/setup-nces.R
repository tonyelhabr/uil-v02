
# NOTE: These function were originally used across multiple files, although now
# they are only used here, so it's not so important to have them defined as functions.
clean_county_col_at <-
  function(data = NULL, col = "county") {
    col <- sym(col)
    data %>%
      mutate_at(vars(!!col), funs(gsub(" County", "", .))) %>%
      mutate_at(vars(!!col), funs(tolower))
  }

clean_city_col_at <-
  function(data = NULL, col = "city") {
    col <- sym(col)
    data %>%
      mutate_at(vars(!!col), funs(toupper))
  }

clean_school_col_at <-
  function(data = NULL, col = "school") {
    col <- sym(col)
    data %>%
      mutate_at(vars(!!col), funs(toupper)) %>%
      mutate_at(vars(!!col), funs(stringr::str_replace_all(., "[Hh]\\s*[Ss]$", ""))) %>%
      mutate_at(vars(!!col), funs(stringr::str_trim(.)))
  }

join_fuzzily <-
  function(x = NULL,
           y = NULL,
           how = "inner",
           max_dist = 1,
           cols_join = "school",
           copy = FALSE,
           suffix_x = "_x",
           suffix_y = "_y") {

    f <- sprintf("fuzzyjoin::stringdist_%s_join", how)

    ret <-
      teproj::do_call_with(f, list(x = x, y = y, by = cols_join, max_dist = max_dist))

    ret <-
      ret %>%
      rename_at(vars(ends_with(".x")), funs(gsub("\\.x", suffix_x, .))) %>%
      rename_at(vars(ends_with(".y")), funs(gsub("\\.y", suffix_y, .)))

    if(copy) {
      cols_join_y <- quo_name(paste0(cols_join, suffix_y))
      ret <-
        ret %>%
        left_join(y %>% mutate(!!!sym(cols_join_y) := !!!sym(cols_join)), by = cols_join)
    }
    ret
  }

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

