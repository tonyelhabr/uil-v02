
source(file.path("R", "00-config.R"))
schools <-
  params$path_schools_clean %>%
  teproj::import_path_cleanly()

persons <-
  params$path_persons_clean %>%
  teproj::import_path_cleanly()

# lax version is new ----


# old version: comp_rank_byschool_byperson ----

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
