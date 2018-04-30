
schools_scrape <- readr::read_csv(params$path_schools_scrape)
persons_scrape <- readr::read_csv(params$path_persons_scrape)

clean_scrape_data <-
  function(data = NULL,
           params = NULL,
           default_city = params$default_city,
           default_complvl_num = params$default_complvl_num,
           complvls_info = params$complvls_info,
           comps_info = params$comps_info) {

    data_proc_0 <-
      data %>%
      select(-url) %>%
      mutate(school_city_complvlnum_raw = school_city_complvlnum)

    # Remove commas in the middle of a school name.
    rgx_patt_detect <- ",\\s(Science)"
    rgx_patt <- rgx_patt_detect
    rgx_repl <- " Science"
    data_proc_1a <-
      data_proc_0 %>%
      mutate(
        school_city_complvlnum = ifelse(
          str_detect(school_city_complvlnum, rgx_patt_detect),
          str_replace(
            school_city_complvlnum,
            rgx_patt,
            rgx_repl
          ),
          school_city_complvlnum
        )
      )

    # setdiff(data_proc_1a, data_proc_0)
    # setdiff(data_proc_0, data_proc_1a)

    # Fix cases where 'HS' is not followed by a comma or city, such as 'Academy H S (32)'.
    # This is only a problem with the persons data.
    rgx_patt_detect <- "[S]\\s\\(.*\\)$"
    rgx_patt <- "[S]\\s\\("
    rgx_repl <- paste0("S, ", default_city, " \\(")
    data_proc_1b <-
      data_proc_1a %>%
      mutate(
        school_city_complvlnum = ifelse(
          str_detect(school_city_complvlnum, rgx_patt_detect) == TRUE,
          str_replace(
            school_city_complvlnum,
            rgx_patt,
            rgx_repl
          ),
          school_city_complvlnum
        )
      )

    # Remove 'H S' from high school name.
    # Note that some schools have 'Hs' instead. which isn't captured by this rgx.
    # This is done on purpose, because the alternative rgx conflicts with school names where some
    # combination of the letters 'h' and 's' are found consecutively.
    # A fix is made later.
    rgx_patt <- "\\s[H]\\s*[Ss]" # "\\s[H]\\s[Ss]"

    rgx_repl <- ""
    data_proc_1c <-
      data_proc_1b %>%
      mutate(school_city_complvlnum = str_replace(school_city_complvlnum, rgx_patt, rgx_repl))

    # Separate school_city_complvlnum.
    data_proc_2a <-
      data_proc_1c %>%
      separate(school_city_complvlnum,
               c("school", "city_complvlnum"),
               sep = ", ") %>%
      select(-school_city_complvlnum_raw)

    # Fix cases where no city is listed, so city_complvl looks something like '(32)'.
    data_proc_2b <-
      data_proc_2a %>%
      mutate(city_complvlnum = ifelse(
        str_detect(city_complvlnum, "^\\(") == TRUE,
        str_replace(city_complvlnum, "^\\(", paste0(default_city, " \\(")),
        city_complvlnum
      ))

    # Add dummy city value for 'state' complvl.
    default_complvl_num_char <- as.character(default_complvl_num)
    data_proc_3 <-
      data_proc_2b %>%
      mutate(city_complvlnum =
               ifelse(
                 complvl == "state",
                 paste0(city_complvlnum, paste0(
                   " (", default_complvl_num_char, ")"
                 )),
                 city_complvlnum
               ))

    # data_proc_3 %>%
    # # # persons_scrape %>%
    # # schools_scrape %>%
    #   mutate(rn = row_number()) %>%
    #   filter(rn %in% c(1087, 1088, 1089, 1090, 1091, 1092, 1093, 10947))

    data_proc_4 <-
      data_proc_3 %>%
      separate(city_complvlnum, c("city", "complvlnum"), sep = "\\s\\(") %>%
      mutate(complvlnum = str_replace(complvlnum, "\\)", "")) %>%
      mutate(complvlnum = as.integer(complvlnum)) %>%
      rename(complvl_num = complvlnum) %>%
      # mutate(complvl = ifelse(complvl == "regional", "region", complvl)) %>%
      # mutate(complvl = str_to_title(complvl))
      inner_join(complvls_info, by = c("complvl")) %>%
      rename(complvl_shortname = complvl) %>%
      rename(complvl = complvl_name)

    ret <-
      data_proc_4 %>%
      inner_join(comps_info, by = c("comp")) %>%
      rename(comp_shortname = comp) %>%
      rename(comp = comp_name)

    # These are difficult cases to account for...
    ret %>%
      mutate_at(vars(conf, complvl, complvl_shortname, comp, comp_shortname), funs(factor)) %>%
      filter(!is.na(complvl_num))
  }

schools_clean <- clean_scrape_data(schools_scrape, params = params)
persons_clean <- clean_scrape_data(persons_scrape, params = params)

if (params$clean) {
  readr::write_csv(schools_clean, params$path_schools_clean)
  readr::write_csv(persons_clean, params$path_persons_clean)
}

schools_geo_raw <-
  params$path_schools_geo_raw %>%
  readxl::read_excel() %>%
  tibble::as_tibble() %>%
  janitor::clean_names()

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
  mutate_at(vars(school), funs(stringr::str_replace_scrape(., "[Hh]\\s*[Ss]$", ""))) %>%
  mutate_at(vars(school), funs(stringr::str_trim(.))) %>%
  arrange(school) %>%
  distinct()
schools_geo_clean

if(params$clean) {
  readr::write_csv(schools_geo_clean, params$path_schools_geo_clean)
}

schools_clean <-
  schools_clean %>%
  # params$path_schools_clean %>%
  # import_cleanly() %>%
  mutate_at(vars(school, city), funs(toupper)) %>%
  arrange(school, city)

schools_geo_join <-
  schools_clean %>%
  arrange_distinctly(school, city) %>%
  fuzzyjoin::stringdist_inner_join(
    schools_geo_clean %>%
      rename_at(vars(school, city), funs(paste0(., "_geo"))),
    by = c("school" = "school_geo"),
    # distance_col = "dist",
    max_dist = 1
  )
schools_geo_join

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

get_schools_geo_bycomplvl <-
  function(data1 = NULL,
           data2 = NULL,
           complvl = c("District", "Region", "State")) {
    # data1 <- schools_geo_join
    # data2 <- schools_clean
    .complvl <- match.arg(complvl)
    data1 %>%
      distinct(school, county, lat, lon) %>%
      arrange(school) %>%
      group_by(school) %>%
      summarise_at(vars(county, lat, lon), funs(last)) %>%
      ungroup() %>%
      inner_join(
        data2 %>%
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

do_get_schools_geo_bycomplvl <-
  function(data1 = NULL, data2 = NULL) {

    bind_rows(
      schools_geo_join %>%
        get_schools_geo_bycomplvl(schools_clean, complvl = "District"),
      schools_geo_join %>%
        get_schools_geo_bycomplvl(schools_clean, complvl = "Region"),
      schools_geo_join %>%
        get_schools_geo_bycomplvl(schools_clean, complvl = "State")
    )
  }

schools_geo_bycomplvl <-
  schools_geo_join %>%
  do_get_schools_geo_bycomplvl(schools_clean)
schools_geo_bycomplvl

