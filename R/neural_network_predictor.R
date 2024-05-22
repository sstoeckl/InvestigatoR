#' #' Title
#' #'
#' #' @param train_features xy _ Andi changes smth
#' #' @param train_labels xy
#' #' @param test_features xy
#' #' @param test_labels xy
#' #' @param epochs xy
#' #'
#' #' @return xy
#' #' @export
#' #'
#' #' @examples xy
#' predict_stock <- function(train_features, train_labels, test_features, test_labels, epochs = 50, batches = 32) {
#'   # Normalize the data
#'   mean <- apply(train_features, 2, mean)
#'   std <- apply(train_features, 2, sd)
#'   train_features <- scale(train_features, center = mean, scale = std)
#'   test_features <- scale(test_features, center = mean, scale = std)
#'
#'   # Define the model architecture  !!!!!!!!activation functions chosen by user or predefine !!!!!!!
#'   model <- keras_model_sequential() %>%
#'     layer_dense(units = ncol(train_features)-1, activation = 'relu', input_shape = ncol(train_features)) %>%
#'     layer_dense(units = (ncol(train_features)-1)/4, activation = 'relu') %>%
#'     layer_dense(units = 1, activation = 'sigmoid') # Assuming binary classification (0 = not invest, 1 = invest)
#'
#'   # Compile the model
#'   model %>% compile(
#'     loss = 'binary_crossentropy',
#'     optimizer = optimizer_rmsprop(),
#'     metrics = c('accuracy')
#'   )
#'
#'   # Fit the model
#'   history <- model %>% fit(
#'     train_features,
#'     train_labels,
#'     epochs = epochs,
#'     batch_size = batches,           #ask sebastian
#'     validation_split = 0.2
#'   )
#'
#'   # Evaluate the model
#'   model %>% evaluate(test_features, test_labels, verbose = 0)
#'
#'   # Make predictions
#'   predictions <- model %>% predict_classes(test_features)
#'
#'   return(list(model = model, history = history, predictions = predictions))
#' }
