

visualize_map_base <- function() {
  teplot::create_map_state(state = "texas", show_county = FALSE) +
    teplot::theme_te() +
    teplot::theme_te_map()
}


visualize_map_bycomplvl <-
  function(data = NULL) {
    visualize_map_base() +
      geom_polygon(
        data =
          data %>%
          count(county, complvl_num, complvl, sort = TRUE) %>%
          # filter(complvl == "Region") %>%
          mutate_at(vars(complvl_num), funs(factor)) %>%
          inner_join(
            teplot::get_map_data_county_tx(),
            by = c("county" = "subregion")
          ),
        aes(group = county, fill = complvl_num),
      ) +
      viridis::scale_fill_viridis(
        # guide = guide_legend(title = complvls, nrow = 4, byrow = TRUE)
        discrete = T,
        option = "D"
      ) +
      theme(
        legend.position = "bottom"
      )
  }


labs_n_byx <- function() {
    labs(x = NULL, y = NULL)
}

theme_n_byx <- function() {
  theme(legend.position = "none")
}

add_calc_cols <-
  function(data = NULL) {
    data %>%
      group_by(year, school, city, conf, comp_shortname, comp) %>%
      mutate(advanced = ifelse(
        complvl == "State",
        as.logical(NA),
        ifelse(!is.na(lead(complvl, 1)), TRUE, FALSE)
      )) %>%
      ungroup() %>%
      group_by(year, conf, complvl, complvl_num, comp_shortname, comp) %>%
      mutate(
        n_bycomp = n(),
        rank = row_number(desc(score)),
        prank = percent_rank(score)
      ) %>%
      mutate(n_defeat = n_bycomp - rank) %>%
      mutate(win = ifelse(rank == 1, TRUE, FALSE)) %>%
      ungroup()
  }


rank_and_arrange_at <-
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

rank_and_arrange <-
  function(data, col, col_out) {
    # stopifnot(!missing(col), !missing(col_out))
    rank_and_arrange_at(
      data = data,
      col = quo_text(enquo(col)),
      col_out = quo_text(enquo(col_out))
    )
  }

summarise_n_bygrps <-
  function(data = NULL,
           cols_grp1 = NULL,
           cols_grp2 = NULL,
           cols_grp3 = NULL) {

    ret <-
      data %>%
      group_by(!!!syms(cols_grp1)) %>%
      summarise(n = n()) %>%
      ungroup()

    if(!is.null(cols_grp2)) {
      ret <-
        ret %>%
        group_by(!!!syms(cols_grp2)) %>%
        summarise(n = mean(n)) %>%
        ungroup()
    }

    if(!is.null(cols_grp3)) {
      ret <-
        ret %>%
        group_by(!!!syms(cols_grp3)) %>%
        summarise(n = mean(n)) %>%
        ungroup()
    }
    ret %>%
      rank_and_arrange_at()

  }

summarise_n_bycomplvl <-
  function(data = NULL, complvls = NULL) {
    data %>%
      filter(complvl %in% complvls) %>%
      distinct(year, complvl_num, school) %>%
      summarise_n_bygrps(
        cols_grp1 = c("year", "complvl_num"),
        cols_grp2 = c("complvl_num")
      ) %>%
      # group_by(year, complvl_num) %>%
      # distinct(year, complvl_num, school) %>%
      # ungroup() %>%
      # summarise(n = n()) %>%
      # ungroup() %>%
      # group_by(complvl_num) %>%
      # summarise(n = mean(n)) %>%
      # ungroup() %>%
      # rank_and_arrange_at() %>%
      mutate(complvl = complvls)
  }

summarise_n_bycomp <-
  function(data = NULL) {
    data %>%
      summarise_n_bygrps(
        cols_grp1 = c("year", "comp"),
        cols_grp2 = c("comp")
      )
    # group_by(year, comp) %>%
    # summarise(n = n()) %>%
    # ungroup() %>%
    # group_by(comp) %>%
    # summarise(n = mean(n)) %>%
    # ungroup() %>%
    # rank_and_arrange_at()
  }

summarise_n_bycompcomplvl <-
  function(data = NULL) {
    data %>%
      summarise_n_bygrps(
        cols_grp1 = c("year", "complvl", "comp"),
        cols_grp2 = c("complvl", "comp")
      )
      # group_by(year, complvl, comp) %>%
      # summarise(n = n()) %>%
      # ungroup() %>%
      # group_by(complvl, comp) %>%
      # summarise(n = mean(n)) %>%
      # ungroup() %>%
      # rank_and_arrange_at()
  }

summarise_n_bycompcomplvlconf <-
  function(data = NULL) {
    data %>%
      summarise_n_bygrps(
        cols_grp1 = c("year", "complvl", "comp", "conf"),
        cols_grp2 = c("complvl", "comp", "conf")
      ) %>%
      group_by(complvl, comp) %>%
      rank_and_arrange_at() %>%
      ungroup() %>%
      arrange(comp, complvl, rnk)
  }

summarise_stats_at <-
  function(data = NULL,
           col = NULL,
           ...,
           tidy = FALSE) {
    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(col), length(col) == 1, is.character(col))
    stopifnot(length(intersect(names(data), col)) == 1)
    is_grouped <- ifelse(is.null(dplyr::groups(data)), FALSE, TRUE)
    if (is_grouped) {
      cols_grp_chr <- as.character(dplyr::groups(data))
      cols_grp <- rlang::syms(cols_grp_chr)
      data <- dplyr::group_by(data, !!!cols_grp)
    }
    col <- rlang::sym(col)
    n <- NULL

    data <- mutate(data, n = sum(!is.na(!!col)))

    ret <-
      dplyr::summarise_at(
      data,
      dplyr::vars(!!col),
      dplyr::funs(
        n = dplyr::first(n),
        mean = mean(., ...),
        median = stats::median(., ...),
        sd = stats::sd(., ...),
        min = min(., ...),
        max = max(., ...),
        z_n1 = mean(., ...) - stats::sd(., ...),
        z_p1 = mean(., ...) + stats::sd(., ...),
        q25 = stats::quantile(., 0.25, ...),
        q75 = stats::quantile(., 0.75, ...),
        q05 = stats::quantile(., 0.05, ...),
        q95 = stats::quantile(., 0.95, ...)
      )
    )

    ret <- dplyr::ungroup(ret)

    if (tidy) {
      stat <- NULL
      value <- NULL
      # ret <- tidyr::gather(ret, stat, value)
      if (!missing(cols_grp)) {

        ret <-
          suppressWarnings(tidyr::gather(ret, stat, value, -c(cols_grp_chr)))
      } else {
        ret <- suppressWarnings(tidyr::gather(ret, stat, value))
      }
      ret <- tibble::as_tibble(ret)
    }
    ret

  }

summarise_stats <-
  function(data = NULL, col, ..., tidy = FALSE) {
    # stopifnot(!missing(col), !is.character(col))
    # browser()
    summarise_stats_at(
      data = data,
      col = quo_text(enquo(col)),
      ...,
      tidy = tidy
    )
  }

