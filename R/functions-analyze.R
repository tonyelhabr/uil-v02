
# validate_nm <-
#   function(data = NULL, nm = NULL) {
#     stopifnot(length(intersect(names(data), nm)) == 1)
#   }

identify_rgx_from_config <-
  function(config = NULL, col_rgx = NULL) {
    nm <- sprintf("rgx_%s_filt", col_rgx)
    # validate_nm(data, nm)
    ret <- config[[nm]]
    ret
  }
# identify_rgx_from_config(config, col_rgx = "name")
# identify_rgx_from_config(config, col_rgx = "name_last")
filter_rnk_or_rgx_at <-
  function(data = NULL,
           config = NULL,
           col_rnk = "rnk",
           n_rnk = config$n_rnk_html,
           col_rgx = "name_last",
           rgx = identify_rgx_from_config(config, col_rgx)) {
    # validate_nm(data, col_rnk)
    # validate_nm(data, col_rgx)

    if((col_rnk %in% names(data)) & (col_rgx %in% names(data))) {
      ret <-
        data %>%
        filter(!!sym(col_rnk) <= n_rnk |
                 str_detect(!!sym(col_rgx), rgx))
    } else if (col_rnk %in% names(data)) {
      ret <-
        data %>%
        filter(!!sym(col_rnk) <= n_rnk)
    } else if (col_rgx %in% names(data)) {
      ret <-
        data %>%
        filter(row_number() <= n_rnk | str_detect(!!sym(col_rgx), rgx))
    } else {
      ret <- data
    }
    ret
  }

create_kable_filt_at <-
  function(data = NULL, format = "html", ...) {
    n_footnote <- nrow(data)
    data %>%
      filter_rnk_or_rgx_at(...) %>%
      teproj::create_kable(n_show = Inf, show_footnote = TRUE, n_footnote = n_footnote, format = format)

  }

add_stats_cols_by_at <-
  function(data = NULL,
           cols_grp = NULL,
           rank = TRUE,
           col_rnk = "prnk_sum",
           na.rm = TRUE,
           rank_all = FALSE,
           ...) {
    ret <-
      data %>%
      group_by(!!!syms(cols_grp)) %>%
      summarise(
        n = n(),
        prnk_sum = sum(prnk, na.rm = na.rm),
        prnk_mean = mean(prnk, na.rm = na.rm),
        n_defeat_sum = sum(n_defeat, na.rm = na.rm),
        n_defeat_mean = mean(n_defeat, na.rm = na.rm),
        n_advanced_sum = sum(advanced, na.rm = na.rm),
        n_state_sum = sum(n_state, na.rm = na.rm)
      ) %>%
      ungroup()
    if(!rank_all) {
      ret <-
        ret %>%
        tetidy::rank_arrange_at(col_rnk, ...)
    } else if (rank) {
      cols_rnk <-
        setdiff(names(ret), names(data))
      ret <-
        ret %>%
        mutate_at(vars(!!!syms(cols_rnk)), funs(rnk = row_number(desc(.))))
    }
    ret
  }


