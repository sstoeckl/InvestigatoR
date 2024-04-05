## code to prepare `DATASET` dataset goes here
set.seed(8976) # remeber to set your seed!
my_data <-
  tibble::tibble(
    x = runif(100),
    y = runif(100)
  )
usethis::use_data(my_data, overwrite = TRUE)
