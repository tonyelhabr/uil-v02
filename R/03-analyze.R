
# Where are the schools? ----
schools_geo_join <-
  params$path_schools_geo_join %>%
  import_cleanly()

viz_map_bydistrict <-
  schools_geo_join %>%
  filter(complvl == "District") %>%
  visualize_map_bycomplvl() +
  guides(fill = guide_legend(title = "District", nrow = 4, byrow = TRUE))
viz_map_bydistrict
viz_map_byregion <-
  schools_geo_join %>%
  filter(complvl == "Region") %>%
  visualize_map_bycomplvl() +
  guides(fill = guide_legend(title = "Region", nrow = 1, byrow = FALSE))
viz_map_byregion

arrange_maps_bycomplvl <-
  function() {
    gridExtra::arrangeGrob(
      viz_map_byregion,
      viz_map_bydistrict,
      # ncol = 2,
      nrow = 1,
      top =
        grid::textGrob(
          "Locations of Schools, by Competition Level",
          just = 1,
          gp = grid::gpar(fontsize = 18, fontfamily = "Arial Narrow", fontface = "bold")
        ),
      bottom =
        grid::textGrob(
          paste0(
            "Note that some counties may be incorrectly categorized\n",
            "due to imperfect data extraction and cleaning.",
            "Data Source:\n",
            "The geo-spatial data was downloaded from ",
            "https://nces.ed.gov/opengis/rest/services/K12_School_Locations/EDGE_GEOCODE_PUBLICSCH_1516/MapServer\n",
            "and combined with the school information (including districts and regions) accompanying the UIL scores\n",
            "scraped from https://www.hpscience.net/ via \"fuzzy-joining\"."
          just = 1,
          gp = grid::gpar(fontsize = 12, fontfamily = "Arial Narrow", fontface = "plain")
        )
    )
  }

viz_map_bycomplvl <-
  arrange_maps_bycomplvl()
gridExtra::grid.arrange(viz_map_bycomplvl)

ggsave(
  file = file.path(params$dir_viz, "viz_map_bycomplvl.png"),
  device = "png",
  height = 6,
  width = 10,
  arrange_maps_bycomplvl()
)

# What differentiates the schools?
schools <-
  params$path_schools_clean %>%
  import_cleanly() %>%
  add_calc_cols()

schools_elhabr <-
  schools %>%
  filter(school == "CLEMENS")
schools_elhabr

persons <-
  params$path_persons_clean %>%
  import_cleanly() %>%
  add_calc_cols() %>%
  # separate(name, c("name_last", "name_first"), sep = ", ", remove = FALSE)
  mutate(name_first = str_extract(name, "^.*(?=\\,)"), name_last = str_extract(name, "(?<=\\,\\s).*$")) %>%
  select(name, name_first, name_last, everything())
persons %>% filter(is.na(name_last))
persons_elhabr <-
  persons %>%
  filter(name_last == "Elhabr")
persons_elhabr

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
      summarise(n_mean = mean(n)),
    aes(yintercept = n_mean),
    color = "black",
    linetype = "dashed",
    size = 1
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(), labels = scales::comma) +
  facet_wrap (~ complvl, scales = "free") +
  labs(
    title = "Count of Schools by Different UIL Groups",
    subtitle =
      paste0(
        "Note that these groupings are not subsets of the same \"parent\" group.",
        "(Rather, they are different groupings of all schools.\n",
        "District and Region are references to competition levels,",
        " while conference is a classification based on school size.)\n",

      )
  ) +
  labs_n_byx() +
  teplot::theme_te_facet() +
  theme_n_byx()
viz_n_bycomplvl

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

visualize_n_bycomp_common <-
  function(data = NULL, col_x = "comp", col_facet = NULL) {
    data %>%
      # inner_join(comp_icons, by = "comp") %>%
      ggplot(aes_string(x = col_x, y = "n")) +
      geom_point(aes(color = comp), size = 5) +
      # geom_icon(aes(image = icon, color = icon), size = 0.1) +
      geom_hline(
        data =
          data %>%
          group_by(!!sym(col_facet)) %>%
          mutate(n_mean = mean(n)),
        aes(yintercept = n_mean),
        color = "black",
        linetype = "dashed",
        size = 1
      ) +
      scale_color_brewer(palette = "Set1") +
      guides(color = guide_legend(override.aes = list(size = 5)))+
      scale_x_discrete(labels = scales::wrap_format(10)) +
      scale_y_continuous(breaks = scales::pretty_breaks(), labels = scales::comma) +
      labs_n_byx() +
      teplot::theme_te_facet()
  }

viz_n_bycomp <-
  summ_n_bycomp %>%
  visualize_n_bycomp_common(col_facet = "entity") +
  facet_wrap(~ entity, scales = "free") +
  theme(axis.text.x = element_blank()) +
  labs(
    title = "Count of Entities for Each Competition Type",
    subtitle = NULL
  )
viz_n_bycomp

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
  visualize_n_bycomp_common(col_facet = "complvl_entity") +
  facet_wrap( ~ complvl_entity, scales = "free", ncol = 2) +
  theme(axis.text.x = element_blank()) +
  labs(
    title = "Count of Entities for Each Competition Type and Level",
    subtitle = NULL
  )
viz_n_bycompcomplvl

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
  visualize_n_bycomp_common(col_x = "conf", col_facet = "complvl_comp") +
  # facet_wrap( ~ complvl_comp, scales = "free_y", labeller = label_wrap_gen(width = 20), ncol = 5, dir = "h") +
  facet_grid(complvl ~ comp, scales = "free") +
  labs(
    title = "Count of Competition for Each Competition Type, Level, and Conference",
    subtitle = NULL,
    x = "Conference"
  )
viz_n_bycompcomplvlconf

 ### From which conferences do the individuals who competed the most come? ----

persons %>%
  group_by(comp) %>%
  # summarise_stats_at("score", tidy = TRUE) %>%
  summarise_stats(score, tidy = TRUE) %>%
  ungroup()

comp_stats_byperson <-
  persons %>%
  group_by(name, school, conf) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  rank_and_arrange_at()
comp_stats_byperson

comp_stats_byperson %>%
  create_kable(n = 20, format = "markdown")

num_top <- 1000
comps_stats_byperson_top <-
  comp_stats_byperson %>%
  slice(1:num_top)

comps_stats_byperson_top_byconf <-
  comps_stats_byperson_top %>%
  group_by(conf) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  rank_and_arrange_at()
comps_stats_byperson_top_byconf

comps_stats_byperson_top_byconf %>%
  mutate(n = formattable::color_tile("white", "orange")(n)) %>%
  # mutate(n = formattable::proportion_bar("orange")(n)) %>%
  create_kable(format = "html")

save.image(file = params$path_analysis_image)

