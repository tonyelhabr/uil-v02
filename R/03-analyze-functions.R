
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

# rank_and_arrange <-
#   function(data, col, col_out) {
#     # stopifnot(!missing(col), !missing(col_out))
#     col <- rlang::quo_text(rlang::enquo(col))
#     col_out <- rlang::quo_text(rlang::enquo(col_out))
#     rank_and_arrange_at(
#       data = data,
#       col = col,
#       col_out = col_out
#     )
#   }

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
        cols_grp2 = c("complvl", "comp", "conf"),
        cols_grp3 = c("complvl", "conf")
      )
    # group_by(year, complvl, comp) %>%
    # summarise(n = n()) %>%
    # ungroup() %>%
    # group_by(complvl, comp) %>%
    # summarise(n = mean(n)) %>%
    # ungroup() %>%
    # rank_and_arrange_at()
  }

