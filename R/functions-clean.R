
# NOTE: This really should be separated into a bunch of smaller functions.
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

    rgx_split <- "(^.*)(\\,)(.*$)"
    # Separate school_city_complvlnum.
    data_proc_2a <-
      data_proc_1c %>%
      # mutate(school = str_replace(school_city_complvlnum, rgx_split, "\\1"),
      #        city_complvlnum = str_replace(school_city_complvlnum, rgx_split, "\\3")) %>%
      # select(-school_city_complvlnum) %>%
      separate(school_city_complvlnum,
               into = c("school", "city_complvlnum"),
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

    rgx_split <- "(^.*)(\\s\\()(.*$)"
    data_proc_4 <-
      data_proc_3 %>%
      # mutate(city = str_replace(city_complvlnum, rgx_split, "\\1"),
      #        complvlnum = str_replace(city_complvlnum, rgx_split, "\\3")) %>%
      # select(-city_complvlnum) %>%
      separate(city_complvlnum, c("city", "complvlnum"), sep = "\\s\\(") %>%
      mutate(complvlnum = str_replace(complvlnum, "\\)", "")) %>%
      mutate(complvlnum = as.integer(complvlnum)) %>%
      rename(complvl_num = complvlnum) %>%
      # mutate(complvl = ifelse(complvl == "regional", "region", complvl)) %>%
      # mutate(complvl = str_to_title(complvl))
      inner_join(complvls_info, by = c("complvl")) %>%
      rename(complvl_shortname = complvl) %>%
      rename(complvl = complvl_name)

    data_proc_5 <-
      data_proc_4 %>%
      inner_join(comps_info, by = c("comp")) %>%
      rename(comp_shortname = comp) %>%
      rename(comp = comp_name)

    ret <-
      data_proc_5 %>%
      mutate_at(vars(conf), funs(as.integer(str_replace(., "A", "")))) %>%
      mutate_at(vars(complvl, complvl_shortname, comp, comp_shortname), funs(factor)) %>%
      mutate_at(vars(school, city), funs(toupper)) %>%
      filter(!is.na(complvl_num))
    ret
  }


add_name_cols_at <-
  function(data = NULL, col = "name") {
    data %>%
      # filter(!is.na(name))
      # separate(!!sym(col), c("name_last", "name_first"), sep = ", ", remove = FALSE)
      mutate(
        name_last = str_extract(!!sym(col), "^.*(?=\\,)"),
        name_first = str_extract(!!sym(col), "(?<=\\,\\s).*$")
      )

  }

add_advanced_col <-
  function(data = NULL) {
    data %>%
      group_by(year, school, city, conf, comp_shortname, comp) %>%
      mutate(advanced = ifelse(complvl == "State",
                               as.logical(NA),
                               ifelse(!is.na(lead(
                                 complvl, 1L
                               )), 1L, 0L))) %>%
      ungroup()
  }

add_calc_cols_by_at <-
  function(data = NULL,
           cols_grp = c("year",
                        "conf",
                        "complvl",
                        "complvl_num",
                        "comp_shortname",
                        "comp")) {
    data %>%
      add_advanced_col() %>%
      group_by(!!!syms(cols_grp)) %>%
      mutate(n_bycomp = n(),
             rnk = row_number(desc(score)),
             prnk = percent_rank(score)) %>%
      mutate(n_defeat = n_bycomp - rnk) %>%
      mutate(w = ifelse(rnk == 1, T, F)) %>%
      select(-rnk) %>%
      ungroup()
  }

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
