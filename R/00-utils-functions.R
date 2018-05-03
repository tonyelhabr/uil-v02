
sort_named_list <- function(lst = NULL) {
  lst[order(names(lst))]
}

# Modified from https://stackoverflow.com/questions/10022436/do-call-in-combination-with.
do_call_with <- function(what, args, ...) {
  if (is.character(what)) {
    fn <- strsplit(what, "::")[[1]]
    what <- if (length(fn) == 1) {
      get(fn[[1]], envir = parent.frame(), mode = "function")
    } else {
      get(fn[[2]], envir = asNamespace(fn[[1]]), mode = "function")
    }
  }

  do.call(what, as.list(args), ...)
}


import_cleanly <-
  function(path = NULL, ...) {
    # path %>%
    #   rio::import() %>%
    #   tibble::as_tibble() %>%
    #   janitor::clean_names()

    ext <- tools::file_ext(path)
    ret <- try({
      fun_readr <- paste0("readr::read_", ext)
      do_call_with(fun_readr, list(file = path))
    }, silent = TRUE)

    if(inherits(ret, "try-error")) {
      ret <- rio::import(path, ...)
      on.exit(return(ret))
      ret <- try(tibble::as_tibble(ret), silent = TRUE)
    }
    ret <- ret %>% janitor::clean_names()

    ret
  }

create_kable <-
  function(data = NULL, num_show = 10, format = "html") {

    num_rows <- nrow(data)
    show_fn <- ifelse(num_rows > num_show, TRUE, FALSE)
    if(show_fn) {
      data <- data %>% dplyr::slice(1:num_show)
    }

    ret <-
      data %>%
      knitr::kable(format = format, escape = FALSE)

    if(format == "html") {
      ret <-
        ret %>%
        kableExtra::kable_styling(full_width = FALSE, position = "center")
    }

    if(show_fn) {
      ret <-
        ret %>%
        kableExtra::add_footnote(c(sprintf("# of total rows: %.0f", num_rows)), notation = "number")
    }
    ret
  }

create_kable_md <-
  function(format = "markdown", ...) {
    create_kable(format = format, ...)
  }

create_kable_html <-
  function(format = "html", ...) {
    create_kable(format = format, ...)
  }

arrange_distinctly <- function(data, ...) {
  cols <- rlang::enquos(...)
  data %>%
    arrange(!!!cols) %>%
    distinct(!!!cols)
}

get_path_lazily <-
  function(dir = NULL, ..., ext = NULL) {
    dots <- list(...)
    if (is.null(ext)) {
      ext <- rev(unlist(dots))[1]
      dots <- dots[-c(length(dots))]
    }
    file <- paste(unlist(dots), collapse = "", sep = "")
    file.path(dir, paste0(file, ".", ext))
  }

# get_path_lazily_1(const$dir_scrape, paste0("schools", const$file_scrape_suffix), const$ext_scrape)
# get_path_lazily_2(const$dir_scrape, const$ext_scrape, "schools", const$file_scrape_suffix)
# get_path_lazily_3(const$dir_scrape, "schools", const$file_scrape_suffix, const$ext_scrape)

# const <- get_const()
# writeLines(yaml::as.yaml(const ), "_const.yml")
#
# get_yml <- function(path = "_const.yml") {
#   yaml::read_yaml(path)
# }
# yml <- get_yml()



