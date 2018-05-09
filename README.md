
Introduction
============

See my blog posts (to be linked here) for the full write-ups, or, check out the `.html` file(s) in the `output/` directory in this repo, which was used as the basis for the blog post. The `figs/` directory also contains some of the visualizations in the post.

The documents can be recreated with the following commands:

``` r
rmarkdown::render("R/01-intro.Rmd", output_dir = "output", intermediates_dir = "output")
rmarkdown::render("R/02-analyze-comps.Rmd", output_dir = "output", intermediates_dir = "output")
rmarkdown::render("R/03-analyze-persons.Rmd", output_dir = "output", intermediates_dir = "output")
rmarkdown::render("R/04-analyze-schools.Rmd", output_dir = "output", intermediates_dir = "output")
rmarkdown::render("R/05-analyze-misc.Rmd", output_dir = "output", intermediates_dir = "output")
```

Highlights
==========

Here are a couple of the coolest visualizations, in my opinion.

![](figs/viz_map_bycomplvl_grid.png)

![](figs/viz_persons_stats_bycomp_grid.png)

![](figs/viz_persons_stats_bycompcomplvl_diffs.png)

![](figs/viz_persons_stats_bycompyear_diffs.png)

![](figs/viz_comp_stats_byperson_byschool.png)

![](figs/viz_map_schools_stats_tier3.png)
