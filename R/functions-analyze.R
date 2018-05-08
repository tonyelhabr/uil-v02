
scale_color_set1 <- function() {
  scale_color_brewer(palette = "Set1")
}

scale_fill_set1 <- function() {
  scale_fill_brewer(palette = "Set1")
}

validate_nm <-
  function(data = NULL, nm = NULL) {
    stopifnot(length(intersect(names(data), nm)) == 1)
  }

identify_rgx_from_params <-
  function(config = NULL, col_rgx = NULL) {
    nm <- sprintf("rgx_%s_filt", col_rgx)
    # validate_nm(data, nm)
    ret <- config[[nm]]
    ret
  }
# identify_rgx_from_params(config, col_rgx = "name")
# identify_rgx_from_params(config, col_rgx = "name_last")
filter_rnk_or_rgx_at <-
  function(data = NULL,
           config = NULL,
           col_rnk = "rnk",
           n_rnk = config$n_rnk_html,
           col_rgx = "name_last",
           rgx = identify_rgx_from_params(config, col_rgx)) {
    # validate_nm(data, col_rnk)
    # validate_nm(data, col_rgx)
    data %>%
      filter(!!sym(col_rnk) <= n_rnk |
               str_detect(!!sym(col_rgx), rgx))
  }

create_kable_filt_at <-
  function(data, format = "html", ...) {
    data %>%
      filter_rnk_or_rgx_at(...) %>%
      teproj::create_kable(n_show = Inf, format = format)

  }


prettify_nums_for_kable <-
  function(data = NULL) {
    data %>%
      mutate_if(is.numeric, funs(round(., 2))) # %>%
    # mutate_if(is.numeric, funs(scales::comma))
  }

add_stats_cols_by_at <-
  function(data = NULL,
           cols_grp = NULL,
           col_rnk = "prnk_sum",
           rank_all = FALSE,
           ...) {
    ret <-
      data %>%
      group_by(!!!syms(cols_grp)) %>%
      summarise(
        n = n(),
        prnk_sum = sum(prnk),
        prnk_mean = mean(prnk),
        n_defeat_sum = sum(n_defeat),
        n_defeat_mean = mean(n_defeat),
        n_advanced_sum = sum(advanced),
        n_state_sum = sum(n_state)
      ) %>%
      ungroup()
    if(!rank_all) {
      ret <-
        ret %>%
        tetidy::rank_arrange_at(col_rnk, ...)
    } else {
      cols_rnk <-
        setdiff(names(ret), names(data))
      ret <-
        ret %>%
        mutate_at(vars(!!!syms(cols_rnk)), funs(rnk = row_number(desc(.))))
    }
    ret
  }

add_persons_stats_cols_by_at <-
  function(data = NULL,
           cols_grp = c("name", "school", "city"),
           col_rnk = "prnk_sum",
           ...) {
    add_stats_cols_by_at(data = data,
                         cols_grp = cols_grp,
                         col_rnk = col_rnk,
                         ...)
  }

add_schools_stats_cols_by_at <-
  function(data = NULL,
           cols_grp = c("school", "city"),
           col_rnk = "prnk_sum",
           ...) {
    add_stats_cols_by_at(data = data,
                         cols_grp = cols_grp,
                         col_rnk = col_rnk,
                         ...)
  }

