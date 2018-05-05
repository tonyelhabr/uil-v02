
source(file.path("R", "00-config.R"))
schools <-
  params$path_schools_clean %>%
  teproj::import_path_cleanly()

persons <-
  params$path_persons_clean %>%
  teproj::import_path_cleanly()

# lax version is new ----
siblings_lax <-
  persons %>%
  get_siblings_by_at(cols_grp = c("name_last", "school", "city", "year"))

siblings_lax_n <-
  siblings_lax %>%
  group_by(school, name_last, name_first_pair) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  distinct(school, name_last, n, .keep_all = TRUE) %>%
  rank_arrange_at("n")
siblings_lax_n

html_siblings_lax_n <-
  siblings_lax_n %>%
  create_kable_filt_at(
    params = params,
    col_rgx = "name_last",
  )

siblings <-
  persons %>%
  get_siblings_by_at()
siblings

siblings_n <-
  siblings %>%
  group_by(name_last, name_first_pair) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  distinct(name_last, n, .keep_all = TRUE) %>%
  rank_arrange_at("n")
siblings_n

html_siblings_n <-
  siblings_n %>%
  create_kable_filt_at(
    params = params,
    col_rgx = "name_last",
  )

siblings_prnk <-
  siblings %>%
  select(matches("^name|^prnk")) %>%
  gather(prak_sum_type, value, matches("^prnk")) %>%
  group_by(name_last, name_first_pair) %>%
  summarise(sum = sum(value)) %>%
  ungroup() %>%
  distinct(name_last, sum, .keep_all = TRUE) %>%
  rank_arrange_at("sum")
siblings_prnk

html_siblings_prnk <-
  siblings_prnk %>%
  create_kable_filt_at(
    params = params,
    col_rgx = "name_last",
  )

# old version: comp_rank_byschool_byperson ----
persons_stats <-
  persons %>%
  add_persons_stats_cols_by_at()

# persons_stats_filt <-
#   persons_stats %>%
#   filter(str_detect(school, params$rgx_school_filt))
# persons_stats %>% filter_rnk_or_rgx_at(params = params, col_rnk = "prnk_sum", col_rgx = "school")
persons_stats %>% count(n_advanced) %>% arrange(desc(n_advanced))
persons_stats %>% count(n_state) %>% arrange(desc(n_state))

html_persons_stats_filt <-
  persons_stats %>%
  select(-n_advanced, -n_state) %>%
  create_kable_filt_at(
    params = params,
    col_rgx = "name",
  )

# old version: comp_rank_byschool ----
schools_stats <-
  schools %>%
  add_schools_stats_cols_by_at(cols_grp = c("school"))

# schools_stats_filt <-
#   schools_stats %>%
#   filter(str_detect(school, params$rgx_school_filt))
schools_stats %>% count(n_state) %>% arrange(desc(n_state))

html_schools_stats_filt <-
  schools_stats %>%
  mutate_if(is.numeric, funs(round(., 2))) %>%
  create_kable_filt_at(
    params = params,
    col_rgx = "school",
  )

# new ----


schools_stats %>% count_arrange_desc(n_advanced)
schools_stats %>% group_arrange_desc(n_advanced)
schools_stats %>% group_arrange_desc(n_defeat)
schools_stats %>% group_arrange_desc(n_state)


viz_map_tier3 <-
  schools_stats %>%
  inner_join(
    schools_geo_join %>%
      distinct(school, county, lat, lon),
    by = c("school")
  ) %>%
  visualize_map_tier3_at(
    col = "prnk_sum",
    add_labels = TRUE,
    col_label = "school"
  ) +
  theme(
    panel.background = element_blank()
  ) +
  labs(
    title = "Most Dominant Schools",
    caption =
      str_wrap(
        paste0(
          "Dominance is evaluated according to total percent rank."
        ), 80
      )
  )
viz_map_tier3

# old version: comp_rank_byschool_byyear ----
schools_stats_byyear <-
  schools %>%
  add_schools_stats_cols_by_at(cols_grp = c("school", "year"))

# schools_stats_byyear_filt <-
#   schools_stats_byyear %>%
#   filter(str_detect(school, params$rgx_school_filt))

html_schools_stats_byyear_filt <-
  schools_stats_byyear %>%
  mutate_if(is.numeric, funs(round(., 2))) %>%
  create_kable_filt_at(
    params = params,
    col_rgx = "school",
  )

# extra ----
schools_filt <-
  schools %>%
  filter(str_detect(school, params$rgx_school_filt))
schools_filt

persons_filt <-
  persons %>%
  filter(str_detect(name, params$rgx_name_filt))
persons_filt

persons_filt_2 <-
  persons %>%
  filter(str_detect(name, params$rgx_name_last_filt))
persons_filt_2

# export ----
vars_all <- ls()
vars_keep <-
  vars_all %>%
  str_subset("^viz_|^html_|(^params$)|(^persons$)|(^schools$)")
vars_drop <-
  setdiff(vars_all, vars_keep)
# rm(list = vars_drop)
# save.image(file = params$path_analysis_image)
save(list = vars_keep)
