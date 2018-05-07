
# source(file.path("R", "00-config.R"))
schools <-
  params$path_schools_clean %>%
  teproj::import_path_cleanly()

persons <-
  params$path_persons_clean %>%
  teproj::import_path_cleanly()

schools_stats %>% tetidy::count_arrange_desc(n_advanced)

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
