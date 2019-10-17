# Tạo package mới
usethis::create_package("market.basket.analysis")

# Bỏ qua file tmp.R khi build
usethis::use_build_ignore("R/hello.R")
usethis::use_build_ignore("R/tmp.R")
usethis::use_build_ignore("R/dev_history.R")
usethis::use_build_ignore("R/prepare_data.R")
usethis::use_build_ignore("R/prepare_data_current.R")
usethis::use_build_ignore("R/prepare_data_past.R")

#
?attachment::att_to_description()

# add data to system data
usethis::use_data(product_holding, internal = TRUE, overwrite = TRUE)

product %>% select(-dayid, -segmentid, -contact_date, -age, - no_years_as_client) %>% slice(1:1000) -> product_holding
save(product_holding, file = "data/product_holding.rdata")
system.file("data/product_holding.rdata", package = "market.basket.analysis")

# Add import packages
lapply(c('dplyr','ggplot2', 'readr', 'tidyr', 'magrittr', 'tibble',
         'arules', 'hrbrthemes', 'scales', 'extrafont', 'purrr', 'gridExtra',
         'DataExplorer', 'arulesViz', 'knitr'),
       function(pkg) usethis::use_package(pkg,
                                          type = 'import', min_version = TRUE))


# Add suggest packages
lapply(c( 'lubridate', 'rio'),
       function(pkg) usethis::use_package(pkg,
                                          type = 'suggest', min_version = TRUE))

# create rd file
devtools::document()
roxygen2::roxygenise()


# Kiểm tra package
devtools::check()

# Thêm data
dataset_path <- system.file("example-data/my-dataset.csv", package = "bank.basket.analysis")
clients <- read_csv(dataset_path)
output <- filter_by_dpt(clients, dpt = 11)
testthat::expect_is(output, "data.frame")
testthat::expect_equal(nrow(output), 10)

#
usethis::use_r("R/")

usethis::use_test("tmp.R")
usethis::use_test("dev_history.R")
usethis::use_test("R/ultility_functions.R")
usethis::use_test("R/prepare_data_current.R")
usethis::use_test("R/prepare_data_past.R")

# https://github.com/STAT545-UBC/Discussion/issues/451
## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

# using for Roboto font
loadfonts("pdf", quiet = TRUE)
loadfonts("postscript", quiet = TRUE)
if (.Platform$OS.type == "windows") {
  loadfonts("win", quiet = TRUE)
}

# vignettes
devtools::clean_vignettes()

usethis::use_vignette("mba_past")
usethis::use_vignette("mba_current")
#
devtools::build_vignettes()
