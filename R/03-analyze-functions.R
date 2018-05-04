

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
      theme(legend.position = "bottom")
  }


labs_xy_null <- function() {
    labs(x = NULL, y = NULL)
}

scale_color_set1 <- function() {
  scale_color_brewer(palette = "Set1")
}

scale_fill_set1 <- function() {
  scale_fill_brewer(palette = "Set1")
}

scale_y_pretty_comma <- function() {
  scale_y_continuous(breaks = scales::pretty_breaks(), labels = scales::comma_format())
}

scale_y_pretty_percent <- function() {
  scale_y_continuous(breaks = scales::pretty_breaks(), labels = scales::percent_format())
}

add_calc_cols <-
  function(data = NULL) {
    data %>%
      group_by(year, school, city, conf, comp_shortname, comp) %>%
      mutate(advanced = ifelse(
        complvl == "State",
        as.logical(NA),
        ifelse(!is.na(lead(complvl, 1)), T, F)
      )) %>%
      ungroup() %>%
      group_by(year, conf, complvl, complvl_num, comp_shortname, comp) %>%
      mutate(
        n_bycomp = n(),
        rnk = row_number(desc(score)),
        prnk = percent_rank(score)
      ) %>%
      mutate(n_defeat = n_bycomp - rnk) %>%
      mutate(w = ifelse(rnk == 1, T, F)) %>%
      select(-rnk) %>%
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



visualize_n_bycomp_common_at <-
  function(data = NULL, x = "comp", col_grp = NULL) {
    data %>%
      # inner_join(comp_icons, by = "comp") %>%
      ggplot(aes_string(x = x, y = "n")) +
      geom_point(aes(color = comp), size = 5) +
      # geom_icon(aes(image = icon, color = icon), size = 0.1) +
      geom_hline(
        data =
          data %>%
          group_by(!!sym(col_grp)) %>%
          mutate(n_mean = mean(n)),
        aes(yintercept = n_mean),
        color = "black",
        linetype = "dashed",
        size = 1
      ) +
      scale_color_set1() +
      guides(color = guide_legend(override.aes = list(size = 5)))+
      scale_x_discrete(labels = scales::wrap_format(10)) +
      scale_y_pretty_comma() +
      labs_xy_null() +
      teplot::theme_te_facet()
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
      # browser()
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

summarise_stats_by_at <-
  function(data = NULL, col = NULL, cols_grp = NULL) {
    stopifnot(is.character(col))
    stopifnot(is.character(cols_grp))
    stopifnot(length(intersect(names(data), cols_grp)) == length(cols_grp))
    cols_grp <- syms(cols_grp)
    data %>%
      group_by(!!!cols_grp) %>%
      summarise_stats_at(col) %>%
      ungroup() %>%
      arrange(!!!cols_grp)
  }

summarise_stats_score_by_at <-
  function(data = NULL, col = "score", cols_grp = NULL) {
    summarise_stats_by_at(
      data = data,
      col = col,
      cols_grp = cols_grp
    )
  }


visualize_persons_stats_bycompx_at <-
  function(data = NULL, x = NULL) {

    # Using facet_wrap() so strip.position can be specified.
    data %>%
      ggplot(aes_string(x = x, y = "mean")) +
      geom_pointrange(aes(ymin = z_n1, ymax = z_p1, color = comp)) +
      scale_color_set1() +
      guides(color = FALSE) +
      facet_wrap(
        ~ comp,
        scales = "free",
        nrow = n_comps,
        labeller = label_wrap_gen(width = 12),
        strip.position = "right"
      ) +
      coord_flip() +
      labs_xy_null() +
      teplot::theme_te_facet() +
      theme(legend.position = "none")
  }

# rescale01 <- function(x) {
#   (x - min(x)) / (max(x) - min(x))
# }

correlate_rnks_by_at <-
  function(data = NULL,
           col = NULL,
           cols_grp = c("name", "school", "conf")) {
    stopifnot(is.character(col), length(col) == 1, length(intersect(names(data), col)) == 1)
    stopifnot(is.character(cols_grp),
              length(intersect(names(data), cols_grp)) == length(cols_grp))
    data %>%
      group_by(!!!syms(cols_grp)) %>%
      summarise_at(vars(!!sym(col)), funs(mean, sum)) %>%
      ungroup() %>%
      mutate(rnk_sum = row_number(desc(sum)),
             rnk_mean = row_number(desc(mean))) %>%
      select_at(vars(starts_with("rnk_"))) %>%
      corrr::correlate()
  }

get_siblings_at <-
  function(data = NULL,
           min = 2,
           max = Inf,
           cols_grp = c(
             "name_last",
             "school",
             "city",
             "year",
             "conf",
             "complvl",
             "comp",
             "complvl_num")) {

    cols_grp_chr <- cols_grp
    cols_grp <- syms(cols_grp)
    data_proc <-
      data %>%
      group_by(!!!cols_grp) %>%
      mutate(rnk_max = rank(name_last, ties.method = "max")) %>%
      ungroup()

    data_filt <-
      data_proc %>%
      filter(rnk_max >= min, rnk_max <= max)

    ret <-
      data_filt %>%
      left_join(
        data_filt,
        by = cols_grp_chr,
        suffix = c("", "_sibling")
      ) %>%
      filter(name_first != name_first_sibling) %>%
      mutate(name_first_pair = paste0(name_first, " & ", name_first_sibling)) %>%
      ungroup() %>%
      select(name, name_first, name_last, name_first_sibling, everything()) %>%
      # distinct(year, school, city, complvl, complvl_num, conf, comp, name_last, .keep_all = TRUE) %>%
      arrange(name_last, name_first, school) %>%
      select(name_first_pair, everything())

    cols_drop <-
      setdiff(c(names(data), "name_first_pair", "name_first_sibling"), names(ret))

    ret <-
      ret %>%
      select(-one_of(cols_drop))
    ret
  }

get_siblings_pair_at <-
  function(data = NULL, min = 2, max = 2) {
    get_siblings_at(
      data = data,
      min = min,
      max = max
    )
  }

# filter_rnk_or_rgx <-
#   function(data = NULL, col_rnk, n_rnk = NULL, col_rgx = "name_last", rgx = NULL) {
#     data %>%
#       filter(!!sym(col_rnk) <= n_rnk | str_detect(!!sym(col_rgx), rgx))
#   }


