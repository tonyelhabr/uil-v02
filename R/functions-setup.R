
# NOTE: This really should be separated into a bunch of smaller functions.

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
    ret
  }

clean_scrape_data <-
  function(data = NULL,
           params = NULL,
           default_city = params$default_city,
           default_complvl_num = params$default_complvl_num,
           complvls_info = tibble::enframe(params$complvls_info),
           comps_info = tibble::enframe(params$comps_info)) {

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
    # This is only a problem with the persons data.
    ret <-
      ret %>%
      detect_clean_txt_at(
        col = "school_city_complvlnum",
        rgx_detect = "[S]\\s\\(.*\\)$",
        rgx_patt = "[S]\\s\\(",
        rgx_repl = paste0("S, ", default_city, " \\(")

      )

    # NOTE: This emoves 'H S' from the school name.
    # Some schools have 'Hs' instead. which isn't captured by this rgx.
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

add_advanced_col <-
  function(data = NULL) {
    data %>%
      group_by(year, school, city, conf, comp, comp) %>%
      mutate(advanced = ifelse(complvl == "State",
                               as.logical(NA),
                               ifelse(!is.na(lead(
                                 complvl, 1L
                               )), 1L, 0L))) %>%
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
           cols_grp = c("year",
                        "conf",
                        "complvl",
                        "complvl_num",
                        "comp")) {

    data %>%
      add_advanced_col() %>%
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
# https://github.com/tidyverse/tidyr/blob/master/R/gather.R
compute_join_stats <-
  function(data = NULL,
           x = NULL,
           y = NULL) {
    stopifnot(is.data.frame(data))
    stopifnot(is.null(x), !is.null(y))

    x_chr <- rlang::quo_name(rlang::enexpr(x))
    y_chr <- rlang::quo_name(rlang::enexpr(y))

    idx <- match(c(x_chr, y_chr), names(data))
    ret <- dplyr::select(data, idx)
    ret <- dplyr::rename(ret, in_x = !!names(.[1]), in_y = !!names(.[2]))
    ret <-
      dplyr::summarise(
        ret,
        n_x = sum(!is.na(in_x)),
        n_y = sum(!is.na(in_y)),
        n_joined = sum(!is.na(in_x) & !is.na(in_y)),
        n_x_unjoined = sum(is.na(in_x) & !is.na(in_y)),
        n_y_unjoined = sum(is.na(in_y) & !is.na(in_x))
      )
    ret <-
      dplyr::mutate(
        ret,
        x_in_y_pct = 100 * n_joined / n_x,
        y_in_x_pct = 100 * n_joined / n_y
      )
    ret <- dplyr::bind_cols(tibble::tibble(x = x_chr, y = y_chr), ret)
    ret
}


get_schools_geo_bycomplvl <-
  function(x = NULL,
           y = NULL,
           complvl = c("District", "Region", "State")) {
    # x <- schools_geo_join
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

do_get_schools_geo_bycomplvl <-
  function(x = NULL, y = NULL) {

    bind_rows(
      schools_geo_join %>%
        get_schools_geo_bycomplvl(schools_clean, complvl = "District"),
      schools_geo_join %>%
        get_schools_geo_bycomplvl(schools_clean, complvl = "Region"),
      schools_geo_join %>%
        get_schools_geo_bycomplvl(schools_clean, complvl = "State")
    )
  }
