
# config ----
# TODO: Convert `_info` data.frames to lists, then convert them to data.frames later
# in the cleaning functions. This makes `const` more reproducible since it
# can be converted to yaml in a more natural fashion.
get_const <- function(sort = TRUE) {
  ret <-
    list(
      export_data = TRUE,
      dir_data = "data",
      export_viz = FALSE,
      dir_viz = "figs",
      scrape = FALSE,
      create_backup = TRUE,
      timestamp_dl = Sys.time(),
      timestamp_dl_chr = paste0("-", format(Sys.time(), "%Y-%m-%d_%H-%M-%S")),
      dir_dl = "data",
      file_dl = "temp",
      ext_dl = "html",
      dir_scrape = "data",
      file_scrape_suffix = "-scrape",
      ext_scrape = "csv",
      years = seq(2004L, 2017L),
      confs = paste0(seq(1L, 6L), "A"),
      complvls = c("district", "regional", "state"),
      # complvls_info_list = setNames(c("district", "regional", "state"), c("District", "Region", "State")),
      complvls_info =
        tibble::tribble(
          ~ complvl,
          ~ complvl_name,
          "district",
          "District",
          "regional",
          "Region",
          "state",
          "State"
        ),
      comps = c("cal", "csc", "mth", "num", "sci"),
      comps_info =
        tibble::tribble(
          ~ comp,
          ~ comp_name,
          "cal",
          "Calculator Applications",
          "csc",
          "Computer Science",
          "mth",
          "Mathematics",
          "num",
          "Number Sense",
          "sci",
          "Science"
        ),
      url_base = "https://www.hpscience.net/results/",
      url_suffix = ".php",
      xpath = "/html/body/table",
      clean = TRUE,
      dir_clean = "data",
      file_clean_suffix = "-clean",
      ext_clean = "csv",
      default_city = "unknown",
      default_complvl_num = 0,
      n_rnk_html = 20,
      path_schools_geo_raw = file.path("data-raw", "EDGE_GEOCODE_PUBLICSCH_1516.xlsx"),
      rgx_school_filt = "CLEMENS",
      rgx_name_filt = "El.*[Hh]ab.*Ant",
      rgx_name_last_filt = "El.*[Hh]ab",
      path_analysis_image = file.path("data", "analysis.RData")
    )
  if(sort) {
    ret <- teproj::sort_named_list(ret)
  }
  ret
}


get_params <- function(const = get_const(), sort = TRUE) {
  ret <-
    c(
      const,
      list(
        path_dl = teproj::get_path_lazily(const$dir_dl, const$file_dl, const$ext_dl),
        path_schools_scrape =
          teproj::get_path_lazily(
            const$dir_scrape,
            "schools",
            const$file_scrape_suffix,
            const$ext_scrape
          ),
        path_persons_scrape =
          teproj::get_path_lazily(
            const$dir_scrape,
            "persons",
            const$file_scrape_suffix,
            const$ext_scrape
          ),
        path_schools_scrape_ts =
          teproj::get_path_lazily(
            const$dir_scrape,
            "schools",
            const$file_scrape_suffix,
            const$timestamp_dl_chr,
            const$ext_scrape
          ),
        path_persons_scrape_ts =
          teproj::get_path_lazily(
            const$dir_scrape,
            "persons",
            const$file_scrape_suffix,
            const$timestamp_dl_chr,
            const$ext_scrape
          ),
        # years_scrape = seq(2010, 2016),
        # confs_scrape = c("4A", "5A", "6A"),
        # complvls_scrape = complvls,
        # comps_scrape = c("cal", "mth", "num"),
        years_scrape = const$years,
        confs_scrape = const$confs,
        complvls_scrape = const$complvls,
        comps_scrape = const$comps
      ),
      path_schools_clean =
        teproj::get_path_lazily(
          const$dir_clean,
          "schools",
          const$file_clean_suffix,
          const$ext_clean
        ),
      path_persons_clean =
        teproj::get_path_lazily(
          const$dir_clean,
          "persons",
          const$file_clean_suffix,
          const$ext_clean
        ),
      path_schools_geo_clean = teproj::get_path_lazily(const$dir_clean, "schools-geo-clean", const$ext_clean),
      path_schools_geo_join = teproj::get_path_lazily(const$dir_clean, "schools-geo-join", const$ext_clean)
    )
  if(sort) {
    ret <- teproj::sort_named_list(ret)
  }
  ret
}

# rt <- robotstxt::robotstxt("www.hpscience.net")
# rt$bots
# rt$permissions
# rt$check()
# rt$crawldelay %>% tbl_df()

# const <- get_const()
# writeLines(yaml::as.yaml(const ), "_const.yml")
#
# get_yml <- function(path = "_const.yml") {
#   yaml::read_yaml(path)
# }
# yml <- get_yml()


# clean ----
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

# analyze ----
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

get_siblings_by_at <-
  function(data = NULL,
           min = 2,
           max = Inf,
           cols_grp = c("name_last",
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
      left_join(data_filt,
                by = cols_grp_chr,
                suffix = c("", "_sibling")) %>%
      filter(name_first != name_first_sibling) %>%
      mutate(name_first_pair = paste0(name_first, " & ", name_first_sibling)) %>%
      ungroup() %>%
      select(name,
             name_first,
             name_last,
             name_first_sibling,
             everything()) %>%
      # distinct(year, school, city, complvl, complvl_num, conf, comp, name_last, .keep_all = TRUE) %>%
      arrange(name_last, name_first, school) %>%
      select(name_first_pair, everything())

    cols_drop <-
      setdiff(c(names(data), "name_first_pair", "name_first_sibling"),
              names(ret))

    ret <-
      ret %>%
      select(-one_of(cols_drop))
    ret
  }

get_siblings_pair_by_at <-
  function(data = NULL,
           min = 2,
           max = 2,
           ...) {
    get_siblings_by_at(data = data,
                       min = min,
                       max = max,
                       ...)
  }

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

add_tier3_lab_cols_at <-
  function(data = NULL,
           col = "rnk",
           col_out_chr = "rnk_tier_lab",
           tier_base = 3,
           n_tiers = 3,
           factor = TRUE,
           add_col_numeric = TRUE,
           col_out_num = "rnk_tier") {
    tier_max <- tier_base * 10 ^ (n_tiers - 1L - 1L)
    if (tier_max > nrow(data)) {
      warning("Too many tiers specified given rows in `data`.", call. = FALSE)
      return(data)
    }
    tiers <-
      tier_base * (10 ^ seq(1L - 1L, n_tiers - 1L - 1L, by = 1L))
    tier_lab_prefixes <-
      c(rep("Top ", n_tiers - 1L), "Outside top ")
    tier_labs <- paste0(tier_lab_prefixes, c(tiers, rev(tiers)[1]))

    col <- sym(col)
    col_out_chr <- sym(col_out_chr)
    ret <-
      data %>%
      mutate(
        !!col_out_chr :=
          case_when(
            !!col <= tiers[1] ~ tier_labs[1],
            !!col <= tiers[2] ~ tier_labs[2],
            TRUE ~ tier_labs[3]
          )
      )

    # NOTE: Factor first, then unfactor later if necessary.
    ret <-
      ret %>%
      mutate_at(vars(!!col_out_chr), funs(factor(., levels = rev(tier_labs))))

    if (add_col_numeric) {
      col_out_num <- sym(col_out_num)
      ret <-
        ret %>%
        mutate(!!col_out_num := as.integer(!!col_out_chr))
    }

    if (!factor) {
      ret <-
        ret %>%
        mutate_at(vars(!!col_out), funs(as.character(.)))
    }
    ret
  }

visualize_map_tier3_at <-
  function(data = NULL,
           col = "rnk",
           tier_base = 3,
           n_tier = 3,
           add_labels = TRUE,
           col_label = "school") {
    data_map <-
      # schools_stats %>%
      # inner_join(schools_geo_join %>% distinct(school, county, lat, lon), by = c("school")) %>%
      data %>%
      rank_arrange_at(col) %>%
      add_tier3_lab_cols_at(tier_base = tier_base, n_tier = n_tier)

    rnk_tier_labs <- levels(data_map$rnk_tier_lab)

    data_tier1 <-
      data_map %>%
      filter(rnk_tier_lab == rev(rnk_tier_labs)[1])
    data_tier2 <-
      data_map %>%
      filter(rnk_tier_lab == rev(rnk_tier_labs)[2])
    data_tier3 <-
      data_map %>%
      filter(rnk_tier_lab == rev(rnk_tier_labs)[3])
    ret <-
      teplot::create_map_base_tx() +
      # geom_point(
      #   data = data_map,
      #   aes(
      #     x = lon,
      #     y = lat,
      #     color = rnk_tier_lab,
      #     size = rnk_tier,
      #     alpha = rnk_tier,
      #     group = ""
      #   )
      # ) +
    geom_point(
      data = data_tier3,
      aes(
        x = lon,
        y = lat,
        color = rnk_tier_lab,
        group = ""
      ),
      size = 1,
      alpha = 0.5
    ) +
      geom_point(
        data = data_tier2,
        aes(
          x = lon,
          y = lat,
          color = rnk_tier_lab,
          group = ""
        ),
        size = 3,
        alpha = 0.75
      ) +
      geom_point(
        data = data_tier1,
        aes(
          x = lon,
          y = lat,
          color = rnk_tier_lab,
          group = ""
        ),
        size = 5,
        alpha = 1
      ) +
      scale_alpha_continuous(range = c(0.5, 1)) +
      # scale_radius(range = c(1, 6)) +
      # scale_size_area(max_size = 6) +
      scale_color_manual(values = c("grey", "blue", "cyan")) +
      theme(legend.position = "bottom")

    if (add_labels) {
      ret <-
        ret +
        ggrepel::geom_label_repel(
          data = data_tier1,
          aes(
            x = lon,
            y = lat,
            label = school,
            group = ""
          )
        )
      # ggrepel::geom_label_repel(
      #   data = data_tier1,
      #   aes_string(
      #     x = "lon",
      #     y = "lat",
      #     label = col_label,
      #     group = ""
      #   )
      # )

    }

    ret
  }
