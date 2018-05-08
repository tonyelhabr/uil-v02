
# NOTE: This really should be separated into a bunch of smaller functions.

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

