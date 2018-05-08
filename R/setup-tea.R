
schools_uil <-
  config$path_schools_uil %>%
  teproj::import_path_cleanly()

schools_distinct <-
  schools_uil %>%
  distinct(school, city) %>%
  arrange(school, city)

# schools_tea ----
schools_tea_raw <-
  config$path_schools_tea_raw %>%
  teproj::import_path_cleanly()

# NOTE: Not sure if district and region actually correspond to the fields
# from the scraped data.
# schools_tea_raw %>% count(district)
# schools_tea_raw %>% count(region)

schools_tea <-
  schools_tea_raw %>%
  clean_county_col_at() %>%
  clean_city_col_at() %>%
  clean_school_col_at() %>%
  arrange(test, year, school)
schools_tea

schools_tea %>%
  teproj::export_path(
    path = config$path_schools_tea,
    export = config$export_data
  )

schools_tea %>%
  # filter(test == "SAT") %>%
  select(test, year, school, total) %>%
  distinct(test, year, school, .keep_all = TRUE) %>%
  # group_by(test, school) %>%
  filter(str_detect(school, "CLEMENS|STEELE II")) %>%
  spread(year, total)

schools_tea_cors_byyear <-
  schools_tea %>%
  # filter(test == "SAT") %>%
  distinct(test, year, school, .keep_all = TRUE) %>%
  filter(!is.na(total)) %>%
  unite(test_school, test, school) %>%
  widyr::pairwise_cor(
    # feature = school,
    feature = test_school,
    item = year,
    value = total
  )

schools_tea_cors_byyear %>%
  ggplot(aes(x = item1, y = item2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = round(correlation, 2))) +
  # scale_fill_gradient2(low = "red", high = "cyan", mid = "white", midpoint = 0) +
  teplot::theme_te()

# schools_tea_join_0 <-
#   schools_distinct %>%
#   join_fuzzily(schools_tea, how = "left", suffix_x = "", suffix_y = "_sat")
# schools_tea_join_0

schools_tea_join <-
  schools_tea %>%
  join_fuzzily(
    schools_distinct,
    how = "left",
    cols_join = "school",
    suffix_x = "_sat",
    suffix_y = ""
  )
schools_tea_join

# Alternatively...
schools_tea %>%
  join_fuzzily(
    schools_distinct,
    how = "full",
    max_dist = 0,
    cols_join = c("school"),
    suffix_x = "_sat",
    suffix_y = ""
  ) %>%
  tetidy::summarise_join_stats(school, school_sat)

schools_tea %>%
  unite(school_city, school, city, remove = FALSE) %>%
  join_fuzzily(
    schools_distinct %>%
      unite(school_city, school, city, remove = FALSE),
    how = "full",
    cols_join = c("school_city"),
    suffix_x = "_sat",
    suffix_y = ""
  ) %>%
  tetidy::summarise_join_stats(school_city, school_city_sat)

schools_stats_2015 <-
  schools_uil %>%
  filter(year == 2015) %>%
  add_schools_stats_cols_by_at(rank_all = TRUE) %>%
  select(matches("school|city|_rnk$"))

school_sat_join_prnks <-
  schools_tea %>%
  select(school, city, math, total) %>%
  mutate_at(vars(math, total), funs(rnk = row_number(desc(.)))) %>%
  select(matches("school|city|_rnk$")) %>%
  join_fuzzily(
    schools_uil %>%
      filter(year == 2015) %>%
      add_schools_stats_cols_by_at(rank_all = TRUE) %>%
      select(matches("school|city|_rnk$")),
    how = "inner", cols_join = c("school"), suffix_x = "_sat", suffix_y = ""
  ) %>%
  mutate_if(is.numeric, funs(prnk = percent_rank(.))) %>%
  rename_at(vars(ends_with("_rnk_prnk")), funs(str_replace(., "_rnk_prnk", "_prnk"))) %>%
  select(matches("school|city|_prnk$"))
school_sat_join_prnks %>%
  select_if(is.numeric) %>%
  corrr::correlate()

schools_tea_join %>%
  select(school, city, math, total) %>%
  inner_join(schools_stats_2015, by = c("school", "city")) %>%
  # filter(!is.na(school)) %>%
  select_if(is.numeric) %>%
  corrr::correlate()
