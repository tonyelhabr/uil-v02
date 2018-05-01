
source("R/00-config.R")

schools_wcalcs <-
  params$path_schools_clean %>%
  import_cleanly() %>%
  add_calc_cols()

schools_elhabr <-
  schools_wcalcs %>%
  filter(school == "CLEMENS")
schools_elhabr

persons_wcalcs <-
  params$path_persons_clean %>%
  import_cleanly() %>%
  add_calc_cols() %>%
  separate(name,
           c("name_last", "name_first"),
           sep = ", ",
           remove = FALSE)

persons_elhabr <-
  persons_wcalcs %>%
  filter(name_last == "Elhabr")
persons_elhabr

summ_n_bycomplvl <-
  bind_rows(
    schools_wcalcs %>%
      summarise_n_bycomplvl("District"),
    schools_wcalcs %>%
      summarise_n_bycomplvl("Region"),
    schools_wcalcs %>%
      mutate_at(vars(conf), funs(as.integer(str_replace(., "A", "")))) %>%
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
        "Rather, they are different groupings of all schools.\n",
        "District and Region are references to competition levels,",
        " while conference is a classification based on school size."
      )
  ) +
  labs_n_byx() +
  teplot::theme_te_facet() +
  theme_n_byx()
viz_n_bycomplvl

summ_n_bycomp <-
  bind_rows(
    schools_wcalcs %>%
      summarise_n_bycomp() %>%
      mutate(entity = "Schools"),
    persons_wcalcs %>%
      summarise_n_bycomp() %>%
      mutate(entity = "Competitors")
  )

# comp_icons <-
#   tibble::tibble(
#     comp = params$comps_info$comp_name,
#     icon = c("calculator", "laptop", "stats-bars", "wand", "flask")
#   )

visualize_n_bycomp_common <-
  function(data = NULL, col_facet = NULL) {
    data %>%
      # inner_join(comp_icons, by = "comp") %>%
      ggplot(aes(x = comp, y = n)) +
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
      teplot::theme_te_facet() +
      # theme_n_byx() +
      theme(axis.text.x = element_blank(), legend.position = "bottom")
  }

viz_n_bycomp <-
  summ_n_bycomp %>%
  visualize_n_bycomp_common(col_facet = "entity") +
  facet_wrap(~ entity, scales = "free") +
  labs(
    title = "Count of Entities for Each Competition Type",
    subtitle = NULL
  )
viz_n_bycomp

summ_n_bycompcomplvl <-
  bind_rows(
    schools_wcalcs %>%
      summarise_n_bycompcomplvl() %>%
      mutate(entity = "Schools"),
    persons_wcalcs %>%
      summarise_n_bycompcomplvl() %>%
      mutate(entity = "Competitors")
  ) %>%
  mutate(complvl_entity = paste(complvl, entity, sep = ", "))

viz_n_bycompcomplvl <-
  summ_n_bycompcomplvl %>%
  visualize_n_bycomp_common(col_facet = "complvl_entity") +
  facet_wrap( ~ complvl_entity, scales = "free", ncol = 2) +
  labs(
    title = "Count of Entities for Each Competition Type and Level",
    subtitle = NULL
  )
viz_n_bycompcomplvl

summ_n_bycompcomplvlconf <-
  bind_rows(
    schools_wcalcs %>%
      summarise_n_bycompcomplvlconf() %>%
      mutate(entity = "Schools"),
    persons_wcalcs %>%
      summarise_n_bycompcomplvlconf() %>%
      mutate(entity = "Competitors")
  ) %>%
  mutate(complvl_conf_entity = paste(complvl, conf, entity, sep = ", "))
summ_n_bycompcomplvlconf
