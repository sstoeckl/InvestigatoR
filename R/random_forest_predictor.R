#'
#' #' Title
#' #'
#' #' @param y xy
#' #' @param features xy
#' #' @param data xy
#' #' @param ntree xy
#' #'
#' #' @return xy
#' #'
#' #' @importFrom randomForest randomForest
#' #'
#' #'
#' #' @export
#' #'
#' #'
#' #' @examples xy
#' #'
#' random_foreest_predictor <- function(y, features, data, ntree ) {
#'   formula <- paste(y , paste(features, collapse = " + ")) # Defines the model
#'   formula <- as.formula(formula)                                   # Forcing formula object
#'   fit_RF <- randomForest(formula_C,         # New formula!
#'                            data = training_sample,    # Data source: training sample
#'                            sampsize = 20000,          # Size of (random) sample for each tree --> dependant on data/feature size
#'                            replace = FALSE,           # Is the sampling done with replacement?
#'                            nodesize = 250,            # Minimum size of terminal cluster --> dependant on data/feature size
#'                            ntree = 40,                # Number of random trees --> dependant on data/feature size
#'                            mtry = 30                  # Number of predictive variables for each tree --> dependant on data/feature size
#'   )
#' predictions <-  predict(fit_RF, testing_sample[1:5,])       # Prediction over the first 5 test instances
#'
#' return(predictions)
#' }
