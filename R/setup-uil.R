

detect_clean_txt_at <-
  function(data = NULL,
           col = NULL,
           rgx_detect = NULL,
           rgx_patt = rgx_detect,
           rgx_repl = NULL) {
    col <- sym(col)
    data %>%
      mutate(
        !!col := ifelse(
          str_detect(!!col, rgx_detect),
          str_replace(!!col, rgx_patt, rgx_repl),
          !!col
        )
      )
  }

remove_hs_txt_at <-
  function(data = NULL,
           col = "school_city_complvlnum",
           rgx_patt = "\\s[H]\\s*[Ss]",
           rgx_repl = "") {
    col <- sym(col)
    data %>%
      mutate(!!col := str_replace(!!col, rgx_patt, rgx_repl))
  }


separate_cols_at <-
  function(data = NULL,
           col = "school_city_complvlnum",
           into = NULL,
           sep = ", ",
           remove = TRUE,
           ...,
           rgx_split = NULL) {
    stopifnot(is.character(col), length(col) == 1)
    stopifnot(is.character(into), length(into) == 2)
    col <- sym(col)
    ret <-
      try({
        data %>%
          separate(!!col, into, sep = sep, remove = remove, convert = TRUE)
      }, silent = TRUE)

    if("try-error" %in% class(ret)) {
      col_1 <- sym(into[1])
      col_2 <- sym(into[2])

      ret <-
        data %>%
        mutate(!!col_1 := str_replace(!!col, rgx_split, "\\1")) %>%
        mutate(!!col_2 := str_replace(!!col, rgx_split, "\\3"))

      if(remove) {
        ret <-
          ret %>%
          select(-!!col)
      }
    }
    ret

  }

impute_dummy_city_at <-
  function(data = NULL,
           col = "city_complvlnum",
           default = NULL) {
    col <- sym(col)
    data %>%
      mutate(!!col :=
               ifelse(
                 complvl == "state",
                 paste0(!!col, paste0(
                   " (", as.character(default), ")"
                 )),
                 !!col
               ))
  }


clean_complvl_cols <-
  function(data = NULL,
           data_supp = NULL) {
    ret <-
      data %>%
      separate_cols_at(
        col = "city_complvlnum",
        into = c("city", "complvlnum"),
        sep = "\\s\\(",
        rgx_split = "(^.*)(\\s\\()(.*$)"
      ) %>%
      mutate_at(vars(complvlnum), funs(as.integer(str_replace(., "\\)", "")))) %>%
      rename(complvl_num = complvlnum)


    ret %>%
      rename(temp = complvl) %>%
      inner_join(
        data_supp %>%
          select(temp = name, complvl = value),
        by = c("temp")
      ) %>%
      select(-temp)
  }

clean_comp_cols <-
  function(data = NULL, data_supp = NULL) {

    data %>%
      rename(temp = comp) %>%
      inner_join(
        data_supp %>%
          select(temp = name, comp = value),
        by = c("temp")
      ) %>%
      select(-temp)
  }


finalize_cols <-
  function(data = NULL) {
    ret <-
      data %>%
      mutate_at(vars(conf), funs(as.integer(str_replace(., "A", "")))) %>%
      mutate_at(vars(school, city), funs(toupper)) %>%
      filter(!is.na(complvl_num))
    if("name" %in% names(ret)) {
      ret <-
        ret %>%
        filter(!is.na(name))
    }
    ret %>%
      distinct()
  }

clean_scrape_data <-
  function(data = NULL,
           config = NULL,
           default_city = config$default_city,
           default_complvl_num = config$default_complvl_num,
           complvls_info = tibble::enframe(config$complvls_info),
           comps_info = tibble::enframe(config$comps_info)) {

    ret <-
      data %>%
      select(-url)

    # NOTE: This removes commas in the middle of a school name.
    ret <-
      ret %>%
      detect_clean_txt_at(
        col = "school_city_complvlnum",
        rgx_detect = ",\\s(Science)",
        rgx_patt = ",\\s(Science)",
        rgx_repl = " Science"

      )

    # NOTE: This fixes cases where 'HS' is not followed by a comma or city, such as 'Academy H S (32)'.
    # This is only a problem with the persons_uil data.
    ret <-
      ret %>%
      detect_clean_txt_at(
        col = "school_city_complvlnum",
        rgx_detect = "[S]\\s\\(.*\\)$",
        rgx_patt = "[S]\\s\\(",
        rgx_repl = paste0("S, ", default_city, " \\(")

      )

    # NOTE: This emoves 'H S' from the school name.
    # Some schools_uil have 'Hs' instead. which isn't captured by this rgx.
    # This is done on purpose, because the alternative rgx conflicts with school names where some
    # combination of the letters 'h' and 's' are found consecutively.
    # A fix is made later.
    ret <-
      ret %>%
      remove_hs_txt_at()

    ret <-
      ret %>%
      separate_cols_at(
        col = "school_city_complvlnum",
        into =  c("school", "city_complvlnum"),
        sep = ", ",
        rgx_split = "(^.*)(\\,)(.*$)"
      )


    # NOTE: This fixes cases where no city is listed, so city_complvl looks something like '(32)'.
    ret <-
      ret %>%
      detect_clean_txt_at(
        col = "city_complvlnum",
        rgx_detect = "^\\(",
        rgx_patt = "^\\(",
        rgx_repl = paste0(default_city, " \\(")
      )

    # NOTE; This adds a dummy city value for 'state' complvl.
    ret <-
      ret %>%
      impute_dummy_city_at(
        col = "city_complvlnum",
        default = default_complvl_num
      )

    ret <-
      ret %>%
      clean_complvl_cols(complvls_info)

    ret <-
      ret %>%
      clean_comp_cols(comps_info)

    ret <-
      ret %>%
      finalize_cols()
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

factor_complvls <-
  function(x = NULL) {
    factor(x, levels = c("District", "Region", "State"))
  }

add_advanced_col <-
  function(data = NULL,
           entity = c("schools", "persons"),
           cols_grp_base = c("year", "school", "city", "conf", "comp")){

    entity <- match.arg(entity)
    if(entity == "schools") {
      cols_grp <- cols_grp_base
    } else if (entity == "persons") {
      cols_grp <- c(cols_grp_base, "name")
    }

    data %>%
      mutate_at(vars(complvl), funs(factor_complvls)) %>%
      group_by(!!!syms(cols_grp_base)) %>%
      mutate(
        advanced =
          ifelse(!is.na(lead(complvl, 1L)), 1L, 0L)) %>%
          # ifelse(complvl == "State",
          #        as.logical(NA),
          #        ifelse(!is.na(lead(
          #          complvl, 1L
          #        )), 1L, 0L))) %>%
      ungroup()
  }

add_n_state_col <-
  function(data = NULL) {
    data %>%
      mutate(n_state = ifelse(complvl == "Region" &
                                advanced == 1L, 1L, 0L))
  }

add_calc_cols_by_at <-
  function(data = NULL,
           entity = c("schools", "persons"),
           cols_grp = c("year",
                        "conf",
                        "complvl",
                        "complvl_num",
                        "comp")) {
    entity <- match.arg(entity)

    data %>%
      add_advanced_col(entity) %>%
      add_n_state_col() %>%
      group_by(!!!syms(cols_grp)) %>%
      mutate(n_bycomp = n(),
             rnk = row_number(desc(score)),
             prnk = percent_rank(score)) %>%
      mutate(n_defeat = n_bycomp - rnk) %>%
      mutate(w = ifelse(rnk == 1, TRUE, FALSE)) %>%
      select(-rnk) %>%
      ungroup()
  }

# schools_uil ----
schools_scrape <-
  config$path_schools_uil_scrape %>%
  teproj::import_path_cleanly()

schools_uil <-
  schools_scrape %>%
  clean_scrape_data(config = config) %>%
  add_calc_cols_by_at(entity = "schools")

schools_uil %>%
  teproj::export_path(
    path = config$path_schools_uil,
    export = config$export_data
  )

# NOTE: Some additional fixes should be made to these schools_uil.
schools_distinct <-
  schools_uil %>%
  distinct(school, city) %>%
  arrange(school, city)

schools_distinct_join <-
  schools_distinct %>%
  join_fuzzily(schools_distinct)

schools_distinct_join %>%
  unite(school_xy, school_x, school_y, sep = ", ", remove = FALSE) %>%
  group_by(school_xy) %>%
  mutate(n = rank(school_xy, ties.method = "max")) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  select(n, school_x, school_y, city_x, city_y) %>%
  filter(n > 1) %>%
  distinct(school_x, school_y)

# persons_uil ----
persons_scrape <-
  config$path_persons_uil_scrape %>%
  teproj::import_path_cleanly()


persons_uil <-
  persons_scrape %>%
  clean_scrape_data(config = config) %>%
  add_calc_cols_by_at(entity = "persons") %>%
  add_name_cols_at() %>%
  select(name, name_first, name_last, everything())

persons_uil %>%
  filter(str_detect(name, config$rgx_name_filt))

persons_uil %>%
  teproj::export_path(
    path = config$path_persons_uil,
    export = config$export_data
  )

