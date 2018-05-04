
source(file.path("R", "00-config.R"))

schools_geo_join <-
  params$path_schools_geo_join %>%
  import_cleanly()

lab_title_map <-
  "Locations of Schools, by Competition Level"

lab_caption_map <-
  str_wrap(
    paste0(
      "Note that some counties may be incorrectly categorized ",
      "due to imperfect data extraction and cleaning. \n\n",
      "Data Source:\n",
      "The geo-spatial data was downloaded from ",
      "https://nces.ed.gov/opengis/rest/services/K12_School_Locations",
      "/EDGE_GEOCODE_PUBLICSCH_1516/MapServer ",
      "and combined with the school information (including districts and regions) ",
      "accompanying the UIL scores ",
      "scraped from https://www.hpscience.net/ via \"fuzzy-joining\"."
      ),
    60)

viz_map_byregion <-
  schools_geo_join %>%
  filter(complvl == "Region") %>%
  visualize_map_bycomplvl() +
  guides(fill = guide_legend(title = "Region", nrow = 1, byrow = FALSE)) +
  theme(plot.caption = element_text(hjust = 0)) +
  labs(title = lab_title_map, caption = lab_caption_map)
viz_map_byregion

viz_map_bydistrict <-
  schools_geo_join %>%
  filter(complvl == "District") %>%
  visualize_map_bycomplvl() +
  guides(fill = guide_legend(title = "District", nrow = 4, byrow = TRUE))
viz_map_bydistrict

viz_map_bycomplvl_grid <-
  gridExtra::arrangeGrob(
    viz_map_byregion,
    viz_map_bydistrict,
    # ncol = 1,
    nrow = 1
    # top =
    #   grid::textGrob(
    #     lab_title_map,
    #     just = 1,
    #     gp = grid::gpar(fontsize = 18, fontfamily = "Arial Narrow", fontface = "bold")
    #   ),
    # bottom =
    #   grid::textGrob(lab_caption_map,
    #     just = 0,
    #     gp = grid::gpar(fontsize = 12, fontfamily = "Arial Narrow", fontface = "plain")
    #   )
  )
gridExtra::grid.arrange(viz_map_bycomplvl_grid)
# closeAllConnections()

if(params$export_viz) {
  ggsave(
    file = file.path(params$dir_viz, "viz_map_bycomplvl_grid.png"),
    # device = "png",
    height = 8,
    # height = 10,
    width = 10,
    # width = 8,
    viz_map_bycomplvl_grid
  )
  # closeAllConnections()
}

schools <-
  params$path_schools_clean %>%
  import_cleanly() %>%
  add_calc_cols()

persons <-
  params$path_persons_clean %>%
  import_cleanly() %>%
  add_calc_cols() %>%
  # separate(name, c("name_last", "name_first"), sep = ", ", remove = FALSE)
  mutate(name_last = str_extract(name, "^.*(?=\\,)"),
         name_first = str_extract(name, "(?<=\\,\\s).*$")) %>%
  filter(!is.na(name)) %>%
  select(name, name_first, name_last, everything())
# persons %>% filter(is.na(name_last))


summ_n_bycomplvl <-
  bind_rows(
    schools %>%
      summarise_n_bycomplvl("District"),
    schools %>%
      summarise_n_bycomplvl("Region"),
    schools %>%
      mutate(complvl = "Conference") %>%
      select(-complvl_num) %>%
      rename(complvl_num = conf) %>%
      summarise_n_bycomplvl("Conference")
  )

viz_n_bycomplvl <-
  summ_n_bycomplvl %>%
  ggplot(aes(x = complvl_num, y = n)) +
  geom_point(aes(size = desc(rnk)), color = "black") +
  geom_hline(
    data = summ_n_bycomplvl %>%
      group_by(complvl) %>%
      summarise(n_filtan = mean(n)),
    aes(yintercept = n_filtan),
    color = "black",
    linetype = "dashed",
    size = 1
  ) +
  scale_y_pretty_comma() +
  facet_wrap (~ complvl, scales = "free") +
  labs(
    title = "Count of Schools by Different UIL Groups",
    caption =
      str_wrap(
        paste0(
          "Note that these groupings are not subsets of the same \"parent\" group.",
          "(Rather, they are different groupings of all schools.",
          "District and Region are references to competition levels,",
          " while conference is a classification based on school size.)"
        ),
        125
      )
  ) +
  labs_xy_null() +
  teplot::theme_te_facet() +
  theme(legend.position = "none")
viz_n_bycomplvl

teproj::export_ext_png(
  viz_n_bycomplvl,
  export = params$export_viz,
  dir = params$dir_viz,
  units = "in",
  height = 5,
  width = 8
)

summ_n_bycomp <-
  bind_rows(
    schools %>%
      summarise_n_bycomp() %>%
      mutate(entity = "Schools"),
    persons %>%
      summarise_n_bycomp() %>%
      mutate(entity = "Competitors")
  )

# comp_icons <-
#   tibble::tibble(
#     comp = params$comps_info$comp_name,
#     icon = c("calculator", "laptop", "stats-bars", "wand", "flask")
#   )

viz_n_bycomp <-
  summ_n_bycomp %>%
  visualize_n_bycomp_common_at(col_grp = "entity") +
  facet_wrap(~ entity, scales = "free") +
  theme(axis.text.x = element_blank()) +
  labs(
    title = "Count of Entities for Each Competition Type",
    subtitle = NULL
  )
viz_n_bycomp

teproj::export_ext_png(
  viz_n_bycomp,
  export = params$export_viz,
  dir = params$dir_viz,
  units = "in",
  height = 8,
  width = 8
)

summ_n_bycompcomplvl <-
  bind_rows(
    schools %>%
      summarise_n_bycompcomplvl() %>%
      mutate(entity = "Schools"),
    persons %>%
      summarise_n_bycompcomplvl() %>%
      mutate(entity = "Competitors")
  ) %>%
  mutate(complvl_entity = paste(complvl, entity, sep = ", "))

viz_n_bycompcomplvl <-
  summ_n_bycompcomplvl %>%
  visualize_n_bycomp_common_at(col_grp = "complvl_entity") +
  facet_wrap( ~ complvl_entity, scales = "free", ncol = 2) +
  theme(axis.text.x = element_blank()) +
  labs(
    title = "Count of Entities for Each Competition Type and Level",
    subtitle = NULL
  )
viz_n_bycompcomplvl

teproj::export_ext_png(
  viz_n_bycompcomplvl,
  export = params$export_viz,
  dir = params$dir_viz,
  units = "in",
  height = 8,
  width = 8
)

summ_n_bycompcomplvlconf <-
  bind_rows(
    schools %>%
      summarise_n_bycompcomplvlconf() %>%
      mutate(entity = "Schools"),
    persons %>%
      summarise_n_bycompcomplvlconf() %>%
      mutate(entity = "Competitors")
  ) %>%
  mutate(complvl_comp = paste(complvl, comp, sep = ", "))
summ_n_bycompcomplvlconf

viz_n_bycompcomplvlconf <-
  summ_n_bycompcomplvlconf %>%
  filter(entity == "Competitors") %>%
  mutate_at(vars(conf), funs(factor)) %>%
  visualize_n_bycomp_common_at(x = "conf", col_grp = "complvl_comp") +
  facet_grid(complvl ~ comp, scales = "free", labeller = label_wrap_gen(width = 12)) +
  labs(
    title = "Count of Competition for Each Competition Type,\nLevel, and Conference",
    subtitle = NULL,
    x = "Conference"
  ) +
  theme(legend.position = "none")
viz_n_bycompcomplvlconf

teproj::export_ext_png(
  viz_n_bycompcomplvlconf,
  export = params$export_viz,
  dir = params$dir_viz,
  units = "in",
  height = 8,
  width = 8
)

persons_stats_bycomp <-
  persons %>%
  summarise_stats_score_by_at(cols_grp = "comp")
persons_stats_bycomp

n_comps <- length(params$comps)
q05_min <- min(persons_stats_bycomp$q05)
q95_max <- max(persons_stats_bycomp$q95)

lab_title_stats_bycomp_prefix <-
  "Distribution of Scores for Each Competition Type"
lab_caption_stats_bycomp <-
  "Dashed line: mean. Solid lines: z-score = -1 and 1."

viz_persons_stats_bycomp <-
  persons %>%
  inner_join(persons_stats_bycomp, by = "comp") %>%
  ggplot(aes(x = score)) +
  geom_histogram(aes(fill = comp), bins = 30) +
  geom_density() +
  scale_fill_set1() +
  geom_vline(
    aes(xintercept = mean),
    color = "black",
    linetype = "solid",
    size = 1
  ) +
  geom_vline(aes(xintercept = z_n1), color = "black", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = z_p1), color = "black", linetype = "dashed", size = 1) +
  guides(fill = FALSE) +
  scale_x_continuous(limits = c(q05_min, q95_max)) +
  # scale_y_continuous(breaks = scales::pretty_breaks(), labels = scales::comma) +
  scale_y_pretty_comma() +
  # scale_y_continuous(breaks = function(x) round(x, 2)) +
  facet_wrap(
    ~ comp,
    scales = "free",
    nrow = n_comps,
    labeller = label_wrap_gen(width = 12),
    strip.position = "right"
  ) +
  labs_xy_null() +
  labs(title = lab_title_stats_bycomp_prefix,
       caption = lab_caption_stats_bycomp) +
  teplot::theme_te_facet() +
  theme(legend.position = "none")
viz_persons_stats_bycomp

teproj::export_ext_png(
  viz_persons_stats_bycomp,
  export = params$export_viz,
  dir = params$dir_viz,
  units = "in",
  height = 10,
  width = 8
)

persons_stats_bycompyear <-
  persons %>%
  summarise_stats_score_by_at(cols_grp = c("comp", "year"))

persons_stats_bycompcomplvl <-
  persons %>%
  summarise_stats_score_by_at(cols_grp = c("comp", "complvl"))

persons_stats_bycompconf <-
  persons %>%
  summarise_stats_score_by_at(cols_grp = c("comp", "conf"))

year_min <- min(persons$year)
year_max <- max(persons$year)
year_labs <- seq.int(year_min, year_max, by = 4L)

lab_caption_stats_bycompx <-
  gsub("Dashed line", "Point", lab_caption_stats_bycomp)

viz_persons_stats_bycompyear <-
  persons_stats_bycompyear %>%
  visualize_persons_stats_bycompx_at(x = "year") +
  scale_x_continuous(breaks = year_labs) +
  # scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(title = lab_title_stats_bycomp_prefix,
       subtitle = "By Year",
       caption = lab_caption_stats_bycompx)
viz_persons_stats_bycompyear

viz_persons_stats_bycompcomplvl <-
  persons_stats_bycompcomplvl %>%
  visualize_persons_stats_bycompx_at(x = "complvl") +
  labs(title = lab_title_stats_bycomp_prefix,
       subtitle = "By Competition Level",
       caption = lab_caption_stats_bycompx)
viz_persons_stats_bycompcomplvl

viz_persons_stats_bycompconf <-
  persons_stats_bycompconf %>%
  visualize_persons_stats_bycompx_at(x = "conf") +
  labs(title = lab_title_stats_bycomp_prefix,
       subtitle = "By Conference",
       caption = lab_caption_stats_bycompx)

teproj::export_ext_png(
  viz_persons_stats_bycompconf,
  export = params$export_viz,
  dir = params$dir_viz,
  units = "in",
  height = 6,
  width = 8
)

viz_persons_stats_bycomp_grid <-
  gridExtra::arrangeGrob(
    viz_persons_stats_bycompyear + theme(strip.text = element_blank()),
    viz_persons_stats_bycompcomplvl + theme(strip.text = element_blank()) + labs(title = "", caption = ""),
    viz_persons_stats_bycompconf + labs(title = "", caption = ""),
    ncol = 3
  )
gridExtra::grid.arrange(viz_persons_stats_bycomp_grid)

if(params$export_viz) {
  ggsave(
    file = file.path(params$dir_viz, "viz_persons_stats_bycomp_grid.png"),
    # device = "png",
    # height = 8,
    height = 10,
    # width = 10,
    width = 8,
    viz_persons_stats_bycomp_grid
  )
  closeAllConnections()
}

persons_stats_bycompcomplvl_diffs <-
  persons_stats_bycompcomplvl %>%
  group_by(comp) %>%
  # mutate_at(vars(mean, median, sd), funs(diff_pct = diff(c(NA, .)) / .))
  mutate_at(
    vars(mean, sd),
    funs(
    diff1_pct = (diff(c(NA, .), lag = 1) / lag(., 1)),
    diff2_pct = (diff(c(NA, NA, .), lag = 2) / lag(., 2))
  )) %>%
  mutate_at(
    vars(complvl),
    funs(
      complvl_diff1 = paste0(lag(.), " to ", .),
      complvl_diff2 = paste0(lag(., 2), " to ", .)
    )
  )

persons_stats_bycompcomplvl_diffs_tidy <-
  persons_stats_bycompcomplvl_diffs %>%
  filter(!is.na(mean_diff1_pct) | !is.na(mean_diff2_pct)) %>%
  gather(complvl_diff, complvl_diff_lab, complvl_diff1, complvl_diff2) %>%
  filter(!is.na(complvl_diff_lab)) %>%
  gather(metric, value, mean_diff1_pct, mean_diff2_pct) %>%
  filter(!is.na(value)) %>%
  filter(!(complvl_diff == "complvl_diff1" & metric == "mean_diff2_pct")) %>%
  filter(!(complvl_diff == "complvl_diff2" & metric == "mean_diff1_pct")) %>%
  ungroup() %>%
  arrange(comp)

viz_persons_stats_bycompcomplvl_diffs <-
  persons_stats_bycompcomplvl_diffs_tidy  %>%
  mutate_at(
    vars(complvl_diff_lab),
    funs(factor),
    levels = c("District to Region", "Region to State", "District to State")
  ) %>%
  ggplot(aes(x = complvl_diff_lab, y = value)) +
  geom_col(aes(fill = comp, alpha = complvl_diff_lab)) +
  scale_fill_set1() +
  scale_alpha_discrete(range = c(0.4, 1)) +
  guides(fill = FALSE, size = FALSE) +
  scale_y_pretty_percent() +
  facet_grid( ~ comp, scales = "free", labeller = label_wrap_gen(width = 12)) +
  teplot::theme_te_facet_dx() +
  theme(axis.text.x = element_blank()) +
  labs_xy_null() +
  labs(title = "% Change in Score Distributions for Each Competition Type",
       subtitle = "By Competition Level")
viz_persons_stats_bycompcomplvl_diffs

teproj::export_ext_png(
  viz_persons_stats_bycompcomplvl_diffs,
  export = params$export_viz,
  dir = params$dir_viz,
  units = "in",
  height = 6,
  width = 10
)

persons_stats_bycompyear_diffs <-
  persons_stats_bycompyear %>%
  mutate_at(vars(mean, median, sd), funs(diff_pct = diff(c(NA, .)) / .))

# fmla_fit <- formula(mean_diff_pct ~ mean + comp)
fmla_fit <- formula(mean_diff_pct ~ mean + year)
persons_stats_bycompyear_terms <-
  persons_stats_bycompyear_diffs %>%
  # group_by(year) %>%
  group_by(comp) %>%
  nest() %>%
  mutate(fit = purrr::map(data, ~lm(fmla_fit, data = .x))) %>%
  mutate(terms = purrr::map(fit, ~broom::tidy(.x))) %>%
  unnest(terms, .drop = TRUE) %>%
  arrange(comp, p.value)
persons_stats_bycompyear_terms

html_persons_stats_bycompyear_terms <-
  persons_stats_bycompyear_terms %>%
  # filter(term == "year") %>%
  select(comp, term, p.value) %>%
  mutate_if(is.numeric, funs(round(., 4))) %>%
  spread(term, p.value) %>%
  create_kable(n = Inf)
html_persons_stats_bycompyear_terms

viz_persons_stats_bycompyear_diffs <-
  persons_stats_bycompyear_diffs %>%
  ggplot(aes(x = year, y = mean_diff_pct)) +
  geom_line(aes(color = comp), size = 2) +
  scale_fill_set1() +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", size = 1) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_pretty_percent() +
  guides(color = FALSE) +
  # geom_smooth(se = FALSE, color = "black", size = 1) +
  facet_wrap( ~ comp,
              scales = "fixed",
              ncol = 1,
              labeller = label_wrap_gen(width = 12),
              strip.position = "right") +
  teplot::theme_te_facet() +
  theme(legend.position = "none") +
  labs_xy_null() +
  labs(title = lab_title_stats_bycomp_prefix,
       subtitle = "By Year",
       caption = "Dashed line: Comptition-specific average year-to-year difference percentage.")
viz_persons_stats_bycompyear_diffs

teproj::export_ext_png(
  viz_persons_stats_bycompyear_diffs,
  export = params$export_viz,
  dir = params$dir_viz,
  units = "in",
  height = 8,
  width = 8
)

persons_n <-
  persons %>%
  group_by(name, school, conf) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  rank_and_arrange_at(col = "n")
persons_n

n_top <- 20
html_persons_n_top <-
  persons_n %>%
  filter(rnk <= n_top | str_detect(name, params$rgx_name_filt)) %>%
  create_kable(n = Inf)

# n_top_conf <- 10000
n_top_conf <- nrow(persons_n)
persons_n_top_byconf <-
  persons_n %>%
  slice(1:n_top_conf) %>%
  group_by(conf) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  rank_and_arrange_at(col = "n")
persons_n_top_byconf

html_persons_n_top_byconf <-
  persons_n_top_byconf %>%
  mutate_at(vars(n), funs(scales::comma)) %>%
  create_kable()

viz_persons_n_top_byconf <-
  persons_n %>%
  mutate(
    rnk_tier_lab =
      case_when(
        rnk <= 1000L ~ "Top 1k",
        rnk <= 10000L ~ "Top 10k",
        rnk <= 100000L ~ "Top 100k",
        TRUE ~ "Outside top 100k"
      )
  ) %>%
  mutate_at(vars(rnk_tier_lab), funs(forcats::fct_rev(factor(.)))) %>%
  ggplot(aes(x = conf, y = n, alpha = rnk_tier_lab)) +
  geom_col() +
  scale_y_pretty_comma() +
  teplot::theme_te_dx() +
  labs_xy_null() +
  labs(title = "Count of Unique Competitors for Each Conference",
       caption = "\"Tier\" of individuals ranked by count of competitions is emphasized by shading.")
viz_persons_n_top_byconf

teproj::export_ext_png(
  viz_persons_n_top_byconf,
  export = params$export_viz,
  dir = params$dir_viz,
  units = "in",
  height = 6,
  width = 8
)

persons %>%
  correlate_rnks_by_at("prnk")
persons %>%
  correlate_rnks_by_at("n_defeat")

pct_top_ingroup_diff_prnk <- 0.05
n_ingroup_min <- 3
persons_stats_byschool <-
  persons %>%
  # filter(school == params$rgx_school_filt, year <= 2012, year >= 2011) %>%
  group_by(comp, complvl, conf, year, school) %>%
  mutate(n_ingroup = n()) %>%
  filter(n_ingroup >= n_ingroup_min) %>%
  mutate(ingroup_rnk = row_number(desc(score))) %>%
  # filter(ingroup_rnk %in% c(1, 2)) %>%
  # arrange(desc(ingroup_rnk)) %>%
  # mutate(ingroup_diff = score - lag(score)) %>%
  mutate(ingroup_score_other = (sum(score) - score)  / (n_ingroup - 1)) %>%
  mutate(ingroup_diff = score - ingroup_score_other) %>%
  # filter(ingroup_rnk == 1) %>%
  # select(-ingroup_rnk) %>%
  ungroup() %>%
  # mutate(group_score = score - ingroup_diff) %>%
  mutate(among_group_rnk = row_number(desc(ingroup_diff))) %>%
  arrange(among_group_rnk)
persons_stats_byschool

set.seed(42)
viz_comp_stats_byperson_byschool <-
  persons_stats_byschool %>%
  mutate(ingroup_diff_prnk = percent_rank(ingroup_diff)) %>%
  mutate(ingroup_diff_toppct = if_else(ingroup_diff_prnk >= (1 - pct_top_ingroup_diff_prnk), T, F)) %>%
  sample_frac(size = 0.05) %>%
  # ggplot(aes(x = score, y = group_score)) +
  ggplot(aes(x = score, y = ingroup_score_other)) +
  geom_point(aes(color = ingroup_diff_toppct), alpha = 0.1) +
  geom_point(
    data =
      persons_stats_byschool %>%
      filter(str_detect(name, params$rgx_name_filt)),
    aes(x = score, y = ingroup_score_other),
    color = "blue",
    size = 5,
    shape = 18
  ) +
  scale_color_manual(values = c("black", "red")) +
  geom_abline(
    aes(intercept = 0, slope = 1),
    color = "black",
    linetype = "solid",
    size = 2
  ) +
  geom_abline(
    aes(
      intercept = -quantile(ingroup_diff, 1 - pct_top_ingroup_diff_prnk, na.rm = T),
      slope = 1
    ),
    color = "red",
    size = 1,
    linetype = "dashed"
  ) +
  labs(
    x = "Individual Score",
    y = "Average Score of Teammates",
    title = "Distribution of Individual and Teammate Scores",
    caption =
      str_wrap(
        paste0(
          "Blue points highlight my personal scores. ",
          sprintf("Red emphasizes top %.0f%% of individuals who \"carried\" ", 100 * pct_top_ingroup_diff_prnk),
          "their teammates, according to differences in percent rank ",
          "of difference in individual score and average of teammates scores. ",
          sprintf("Only unique school-competitions observations having at least %.0f team members are considered.", n_ingroup_min)
        ), 100
      )
  ) +
  teplot::theme_te_facet() +
  theme(legend.position = "none")
viz_comp_stats_byperson_byschool

teproj::export_ext_png(
  viz_comp_stats_byperson_byschool,
  export = params$export_viz,
  dir = params$dir_viz,
  units = "in",
  height = 8,
  width = 8
)

siblings_lax <-
  persons %>%
  get_siblings_at(cols_grp = c("name_last", "school", "city", "year"))

siblings_lax_n <-
  siblings_lax %>%
  group_by(school, name_last, name_first_pair) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  distinct(school, name_last, n, .keep_all = TRUE) %>%
  rank_and_arrange_at("n")
siblings_lax_n

html_siblings_lax_n <-
  siblings_lax_n %>%
  filter(rnk <= (n_top) | str_detect(name_last, params$rgx_name_last_filt)) %>%
  create_kable(n = Inf)

siblings <-
  persons %>%
  get_siblings_at()
siblings

siblings_n <-
  siblings %>%
  group_by(name_last, name_first_pair) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  distinct(name_last, n, .keep_all = TRUE) %>%
  rank_and_arrange_at("n")
siblings_n

html_siblings_n <-
  siblings_n %>%
  filter(rnk <= (n_top) | str_detect(name_last, params$rgx_name_last_filt)) %>%
  create_kable(n = Inf)

siblings_prnk <-
  siblings %>%
  select(matches("^name|^prnk")) %>%
  gather(prak_sum_type, value, matches("^prnk")) %>%
  group_by(name_last, name_first_pair) %>%
  summarise(sum = sum(value)) %>%
  ungroup() %>%
  distinct(name_last, sum, .keep_all = TRUE) %>%
  rank_and_arrange_at("sum")
siblings_prnk

html_siblings_prnk <-
  siblings_prnk %>%
  filter(rnk <= (n_top) | str_detect(name_last, params$rgx_name_last_filt)) %>%
  create_kable(n = Inf)

persons_byschool <-
  persons %>%
  group_by(school, name) %>%
  summarise(
    cnt = n(),
    prnk_sum = sum(prnk),
    prnk_mean = mean(prnk),
    n_defeat = sum(n_defeat)
  ) %>%
  ungroup() %>%
  rank_and_arrange_at("prnk_sum")

persons_byschool_filt <-
  persons_byschool %>%
  filter(str_detect(school, params$rgx_school_filt)) %>%
  rank_and_arrange_at("prnk_sum")
persons_byschool_filt

html_persons_byschool_filt <-
  persons_byschool_filt%>%
  filter(rnk <= (n_top) | str_detect(name, params$rgx_name_filt)) %>%
  mutate_if(is.numeric, funs(round(., 2))) %>%
  create_kable(n = Inf)

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

save.image(file = params$path_analysis_image)
