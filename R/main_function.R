#' @export

easyCTT <- function(data,
                    item_cols,
                    forced_estimator = FALSE,
                    model_fit_alpha = 0.05,
                    group = "random_group") {
  # Stop execution if there are less than 3 items
  n_items <- length(item_cols)

  if (n_items < 4) {
    stop("Less than four items specified. No analysis possible.")
  }

  # First test if the items are jointly normal distributed
  # Choose the estimator accordingly
  estimator <- decision_tree_mvn(data = data,
                                 item_cols = item_cols,
                                 forced_estimator = forced_estimator)

  # Generate the model codes
  model_codes <- make_model_codes(data = data,
                                  item_cols = item_cols,
                                  multi_group = FALSE)

  # Fit the models
  fitted_models <- lapply(model_codes,
                          FUN = cfa,
                          data = data,
                          meanstructure = TRUE,
                          estimator = estimator) # Use estimator according to the mvn test.

  # Extract the parameters into tables
  par_tables <- lapply(fitted_models,
                       FUN = extract_parameters)

  par_tables
}
