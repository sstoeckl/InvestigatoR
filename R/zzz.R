.onLoad <- function(libname, pkgname) {
  library(conflicted)

  # Resolve conflicts between keras3 and tensorflow
  conflict_prefer("set_random_seed", "keras3")
  conflict_prefer("shape", "keras3")
  conflict_prefer("%<-%", "keras3")
  conflict_prefer("train", "caret")
  conflict_prefer("filter", "dplyr")
}
