
# source(file.path("R", "config.R"))
schools_uil <-
  config$path_schools_uil %>%
  teproj::import_path_cleanly()

persons_uil <-
  config$path_persons_uil %>%
  teproj::import_path_cleanly()

schools_stats %>% tetidy::count_arrange_desc(n_advanced)

# extra ----
schools_filt <-
  schools_uil %>%
  filter(str_detect(school, config$rgx_school_filt))
schools_filt

persons_filt <-
  persons_uil %>%
  filter(str_detect(name, config$rgx_name_filt))
persons_filt

persons_filt_2 <-
  persons_uil %>%
  filter(str_detect(name, config$rgx_name_last_filt))
persons_filt_2
