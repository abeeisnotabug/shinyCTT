decision_tree_mvn <- function(data,
                              item_cols,
                              forced_estimator = FALSE) {
  mvn_test <- MVN::mvn(data[, item_cols])
  mvn_test_statistics <- as.numeric(levels(mvn_test$multivariateNormality$Statistic))
  names(mvn_test_statistics) <- c("Mardia's skewness coefficient",
                                  "Mardia's kurtosis coefficient")
  mvn_test_pvalues <- as.numeric(levels(mvn_test$multivariateNormality$`p value`))

  if (isFALSE(forced_estimator)) {
    if (mvn_test$multivariateNormality$Result[3] == "NO") {
      message("The hypothesis that the items follow a multivariate normal distribution has to be discarded on a significance level of 0.05.")
      message(test_result_output(mvn_test_statistics[1], mvn_test_pvalues[1]))
      message(test_result_output(mvn_test_statistics[2], mvn_test_pvalues[2]))
      message("Continuing with the MLR estimator.")
      return("MLR")
    } else {
      message("The hypothesis that the items follow a multivariate normal distribution can be maintained on a significance level of 0.05.")
      message(test_result_output(mvn_test_statistics[1], mvn_test_pvalues[1]))
      message(test_result_output(mvn_test_statistics[2], mvn_test_pvalues[2]))
      message("Continuing with the ML estimator.")
      return("ML")
    }
  } else {
    message(paste("Forced estimator", forced_estimator, "specified."))
    return(forced_estimator)
  }
}

decision_tree_model_fit <- function(data,
                                    item_cols,
                                    group,
                                    estimator,
                                    model_fit_alpha) {
  if (group == "random_group") {
    data <- cbind(data, random_group = sample(c(0,1), size = nrow(data), replace = TRUE))
  }

  # Generate the model codes
  model_codes <- make_model_codes(data = data,
                                  item_cols = item_cols,
                                  multi_group = FALSE)
  model_codes_mg <- make_model_codes(data = data,
                                     item_cols = item_cols,
                                     multi_group = TRUE)

  for (model in names(model_codes)) {
    fitted_model <- cfa(model = model_codes[[model]],
                        data = data,
                        meanstructure = TRUE,
                        estimator = estimator)

    scaled_addon <- switch(estimator,
                           "MLR" = ".scaled",
                           "ML" = "")

    fit_stats <- lavInspect(fitted_model, what = "fit")

    model_chisq <- fit_stats[paste0("chisq", scaled_addon)]
    model_chisq_pvalue <- fit_stats[paste0("pvalue", scaled_addon)]
    model_chisq_df <- fit_stats[paste0("df", scaled_addon)]
    model_rmsea <- fit_stats[paste0("rmsea", scaled_addon)]
    model_rmsea_pvalue <- fit_stats[paste0("rmsea.pvalue", scaled_addon)]

    if (model_chisq_pvalue < model_fit_alpha) {
      message(sprintf("The hypotheses that the %s model fits the data has to be discarded on a significance level of %.3f:",
                      model,
                      model_fit_alpha))
      message(test_result_output(model_chisq, model_chisq_pvalue, model_chisq_df))
      if (model_rmsea_pvalue < model_fit_alpha) {
        message(sprintf("Moreover, the hypothesis that the RMSEA is smaller or equal to 0.05 also has to be discarded on a significance level of %.3f:",
                        model_fit_alpha))
        message(test_result_output(model_rmsea, model_rmsea_pvalue))

        # If the model checked is the first model (most unrestrictive), stop further analysis
        if (which(names(model_codes) == model) == 1) {
          message("")
          stop(sprintf("The %s model is the most unrestrictive model testable with the given data. Since the model fit test is significant and the RMSEA is also significantly smaller than 0.05, the hypothesis that the modle fits the data should be discarded. Since it is the most unrestrictive model, this means that further analysis (items, number of observations, etc.) is needed and this app cannot continue.",
                       model))
        }

        # If not, then stop execution regularly, but return the most restrictive model that still fits the data:
        message(sprintf("Therefore, the %s model does not longer fit the data.
The most restrictive model that still fits the data is the %s model.",
                        model,
                        names(model_codes)[which(names(model_codes) == model) - 1]))
        break
      } else {
        message(sprintf("However, the hypothesis that the RMSEA is smaller or equal to 0.05 can be maintained on a significance level of %.3f:",
                        model_fit_alpha))
        message(test_result_output(model_rmsea, model_rmsea_pvalue))
        message("Continuing with the next, more restrictive model.")
        message("")
      }
    } else {
      message(sprintf("The hypothesis that the %s model fits the data can be maintained on a significance level of %.3f:",
                      model,
                      model_fit_alpha))
      message(test_result_output(model_chisq, model_chisq_pvalue, model_chisq_df))

      # Now that the model seems to fit, explore if the subgroup test is also non-significant.
      fitted_model_mg <- cfa(model = model_codes_mg[[model]],
                             data = data,
                             meanstructure = TRUE,
                             group = group,
                             group.equal = c("loadings", "intercepts"),
                             estimator = estimator)

      fit_stats_mg <- lavInspect(fitted_model_mg, what = "fit")

      model_chisq_mg <- fit_stats[paste0("chisq", scaled_addon)]
      model_chisq_pvalue_mg <- fit_stats[paste0("pvalue", scaled_addon)]
      model_chisq_df_mg <- fit_stats[paste0("df", scaled_addon)]
      model_rmsea_mg <- fit_stats[paste0("rmsea", scaled_addon)]
      model_rmsea_pvalue_mg <- fit_stats[paste0("rmsea.pvalue", scaled_addon)]

      message("Continuing with the next, more restrictive model.
")
    }
  }
}
