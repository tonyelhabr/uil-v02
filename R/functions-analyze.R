
scale_color_set1 <- function() {
  scale_color_brewer(palette = "Set1")
}

scale_fill_set1 <- function() {
  scale_fill_brewer(palette = "Set1")
}

rank_arrange_at <-
  function(data = NULL,
           col = "n",
           col_out = "rnk") {
    col <- rlang::sym(col)
    col_out <- rlang::sym(col_out)
    data %>%
      mutate(!!col_out := row_number(desc(!!col))) %>%
      arrange(!!col_out) %>%
      select(!!col_out, everything())
  }


rank_arrange <-
  function(data, col, ...) {
    # stopifnot(!missing(col), !missing(col_out))
    rank_arrange_at(data = data,
                    col = quo_text(enquo(col)),
                    ...)
  }

count_arrange_desc <-
  function(data = NULL, col) {
    col <- enquo(col)
    data %>%
      count(!!col) %>%
      arrange(desc(!!col))
  }

group_arrange_desc <-
  function(data = NULL, col) {
    col <- enquo(col)
    data %>%
      group_by(!!col) %>%
      arrange(desc(!!col)) %>%
      ungroup()
  }

# rescale01 <- function(x) {
#   (x - min(x)) / (max(x) - min(x))
# }

validate_nm <-
  function(data = NULL, nm = NULL) {
    stopifnot(length(intersect(names(data), nm)) == 1)
  }

identify_rgx_from_params <-
  function(params = NULL, col_rgx = NULL) {
    nm <- sprintf("rgx_%s_filt", col_rgx)
    # validate_nm(data, nm)
    ret <- params[[nm]]
    ret
  }

# identify_rgx_from_params(params, col_rgx = "name_last")
filter_rnk_or_rgx_at <-
  function(data = NULL,
           params = NULL,
           col_rnk = "rnk",
           n_rnk = params$n_rnk_html,
           col_rgx = "name_last",
           rgx = identify_rgx_from_params(params, col_rgx)) {
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

add_n_state_col <-
  function(data = NULL) {
    data %>%
      mutate(n_state = ifelse(complvl == "Region" &
                                advanced == 1L, 1L, 0L))
  }

add_stats_cols_by_at <-
  function(data = NULL,
           cols_grp = c("school"),
           col_rnk = "prnk_sum") {
    data %>%
      add_n_state_col() %>%
      group_by(!!!syms(cols_grp)) %>%
      summarise(
        n = n(),
        prnk_sum = sum(prnk),
        prnk_mean = mean(prnk),
        n_defeat = sum(n_defeat),
        n_advanced = sum(advanced),
        n_state = sum(n_state)
      ) %>%
      ungroup() %>%
      rank_arrange_at(col_rnk)
  }

add_persons_stats_cols_by_at <-
  function(data = NULL,
           cols_grp = c("name", "school"),
           ...) {
    add_stats_cols_by_at(data = data,
                         cols_grp = cols_grp,
                         ...)
  }

add_schools_stats_cols_by_at <-
  function(data = NULL,
           cols_grp = c("school"),
           ...) {
    add_stats_cols_by_at(data = data,
                         cols_grp = cols_grp,
                         ...)
  }

