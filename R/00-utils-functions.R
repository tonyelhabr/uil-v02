
import_cleanly <-
  function(path = NULL, ...) {
    # path %>%
    #   rio::import() %>%
    #   tibble::as_tibble() %>%
    #   janitor::clean_names()

    ext <- tools::file_ext(path)
    ret <- try({
      fun_readr <- paste0("read_", ext)
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

arrange_distinctly <- function(data, ...) {
  cols <- rlang::enquos(...)
  data %>%
    arrange(!!!cols) %>%
    distinct(!!!cols)
}




# get_path_1 <-
#   function(dir = NULL, file = NULL, ext = NULL) {
#     file.path(dir, paste0(file, ".", ext))
#   }
#
# get_path_2 <-
#   function(dir = NULL, ext = NULL, ...) {
#     dots <- list(...)
#     file <- paste(unlist(dots), collapse = "", sep = "")
#     file.path(dir, paste0(file, ".", ext))
#   }

#  get_path_3
get_path <-
  function(dir = NULL, ..., ext = NULL) {
    dots <- list(...)
    if (is.null(ext)) {
      ext <- rev(unlist(dots))[1]
      dots <- dots[-c(length(dots))]
    }
    file <- paste(unlist(dots), collapse = "", sep = "")
    file.path(dir, paste0(file, ".", ext))
  }

# get_path_1(const$dir_scrape, paste0("schools", const$file_scrape_suffix), const$ext_scrape)
# get_path_2(const$dir_scrape, const$ext_scrape, "schools", const$file_scrape_suffix)
# get_path_3(const$dir_scrape, "schools", const$file_scrape_suffix, const$ext_scrape)

# const <- get_const()
# writeLines(yaml::as.yaml(const ), "_const.yml")
#
# get_yml <- function(path = "_const.yml") {
#   yaml::read_yaml(path)
# }
# yml <- get_yml()



