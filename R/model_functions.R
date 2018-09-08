#' @export
make_model_codes <- function(input_data, item_cols, group = FALSE) {
  item_names <- colnames(input_data[, item_cols])
  n_items <- length(item_cols)

  models <- c("tk", "ete", "te", "etp", "tp")

  one_disc <- rep("lambda_1", n_items)
  mult_disc <- paste("lambda", 1:n_items, sep = "_")

  disc_par <- list("tk" = mult_disc,
                   "ete" = one_disc,
                   "te" = one_disc,
                   "etp" = one_disc,
                   "tp" = one_disc)

  one_eas <- rep("alpha_1", n_items)
  mult_eas <- paste("alpha", 1:n_items, sep = "_")

  eas_par <- list("tk" = mult_eas,
                  "ete" = mult_eas,
                  "te" = one_eas,
                  "etp" = mult_eas,
                  "tp" = one_eas)

  one_err_var <- rep("sigma_epsilon_1", n_items)
  mult_err_var <- paste("sigma_epsilon", 1:n_items, sep = "_")

  err_var <- list("tk" = mult_err_var,
                  "ete" = mult_err_var,
                  "te" = mult_err_var,
                  "etp" = one_err_var,
                  "tp" = one_err_var)

  model_codes <- lapply(models, function(model) {
    eta_dep <- paste("eta =~",
                     paste(sprintf("%s * %s",
                                   disc_par[[model]],
                                   item_names),
                           collapse = " + "))
    alphas <- paste(sprintf("%s ~ %s * 1",
                            item_names,
                            eas_par[[model]]),
                    collapse = "\n")
    if (!isFALSE(group)) {
      n_groups <- length(unique(input_data[, group]))

      err_vars <- paste(sprintf("%s ~~ c(%s) * %s",
                                item_names,
                                apply(
                                  sapply(1:n_groups,
                                         function(group) sprintf("%s_g%i", err_var[[model]], group)),
                                  1,
                                  paste,
                                  collapse = ", "
                                ),
                                item_names),
                        collapse = "\n")
      eta_var <- sprintf("eta ~~ c(%s) * eta",
                         paste(paste0("sigma_eta_g", 1:n_groups), collapse = ", "))

      paste(eta_dep, err_vars, alphas, eta_var, sep = "\n")
    } else {
      err_vars <- paste(sprintf("%s ~~ %s * %s",
                                item_names,
                                err_var[[model]],
                                item_names),
                        collapse = "\n")
      eta_var <- "eta ~~ sigma_eta * eta"

      # Item reliabilities:
      item_rels_names <- paste("rel", 1:n_items, sep = "_")
      def_names <- gsub("lambda_1", "1", disc_par[[model]])
      item_rels <- paste(sprintf("rel_%i := 1 / (1 + %s / (%s^2 * sigma_eta))",
                                 1:n_items,
                                 err_var[[model]],
                                 def_names),
                         collapse = "\n")
      # Sum reliability:
      disc_par_sum_squared <-  paste0("(",
                                      paste(def_names,
                                            collapse = " + "),
                                      ")^2")
      err_var_sum_div_by_sigma <- paste0("(",
                                          paste(err_var[[model]],
                                                collapse = " + "),
                                          ") / sigma_eta")
      sum_rel <- sprintf("sumrel := %s / (%s + %s)",
                         disc_par_sum_squared,
                         disc_par_sum_squared,
                         err_var_sum_div_by_sigma)

      paste(eta_dep, err_vars, alphas, eta_var, item_rels, sum_rel, sep = "\n")
    }
  })

  names(model_codes) <- models

  model_codes
}

#' @export
corr_ind_test <- function(input_data, estimator, latex = TRUE) {
  dummyModel <- paste(sprintf("%s ~ 1", colnames(input_data), collapse = "\n"))

  fittedCorrIndModel <- cfa(model = dummyModel,
                            data = input_data,
                            estimator = estimator)

  scaled_addon <- switch(estimator,
                         "MLR" = ".scaled",
                         "ML" = "")

  to_extract <- paste0(c("baseline.chisq", "baseline.df", "baseline.pvalue"), scaled_addon)

  out_vec <- lavInspect(fittedCorrIndModel, what = "fit")[to_extract]
  if (latex) names(out_vec) <- c("\\chi^2", "df", "p")

  out_vec
}

#' @export
extract_fit_parameters <- function(fitted_model, what) {
  estimator <- fitted_model@options$estimator

  raw_params <- lavInspect(fitted_model, what = "fit")

  par_names <- switch(what,
                      "model_fit" = c("chisq", "df", "pvalue"),
                      "corr_ind" = c("baseline.chisq", "baseline.df", "baseline.pvalue"),
                      "rmsea" = c("rmsea", "rmsea.pvalue"))

  scaled_addon <- switch(estimator,
                         "MLR" = ".scaled",
                         "ML" = "")

  to_extract <- paste0(par_names, scaled_addon)

  out_vec <- c(raw_params[to_extract])

  names(out_vec) <- switch(what,
                           "model_fit" = c("\\chi^2", "df", "p"),
                           "corr_ind" = c("\\chi^2", "df", "p"),
                           "rmsea" = c("\\text{RMSEA}", "p"))

 out_vec
}

#' @export
extract_parameters <- function(fitted_model, alpha = 0.05) {
  par_est_df <- parameterEstimates(fitted_model,
                                   zstat = F,
                                   pvalue = F,
                                   rsquare = T)[, -c(1, 2, 3)]

  ## CIs for reliabilities: ----------------------------------------------------
  rels <- par_est_df$est[grep("rel", par_est_df$label)]
  rels_se <- par_est_df$se[grep("rel", par_est_df$label)]

  rels_logit <- log(rels / (1 - rels))
  rels_logit_se <- rels_se / (rels * (1 - rels))

  rels_ci_l <- 1 / (1 + exp(-rels_logit + qnorm(1 - alpha / 2) * rels_logit_se))
  rels_ci_u <- 1 / (1 + exp(-rels_logit - qnorm(1 - alpha / 2) * rels_logit_se))

  par_est_df$ci.lower[grep("rel", par_est_df$label)] <- rels_ci_l
  par_est_df$ci.upper[grep("rel", par_est_df$label)] <- rels_ci_u
  # ----------------------------------------------------------------------------

  par_est_df <- par_est_df[grep("epsilon|alpha|lambda|eta|rel", par_est_df$label), ]
  par_est_df$CI <- sprintf("[%.3f; %.3f]", par_est_df$ci.lower, par_est_df$ci.upper)
  par_est_df <- par_est_df[, -c(4, 5)]

  # Prepare the names for HTML
  par_est_df$label <- gsub("sigma_epsilon_(\\d+)",
                          "&sigma;&#x302;<sub>&epsilon;<sub>\\1</sub></sub>",
                          par_est_df$label)
  par_est_df$label <- gsub("lambda_(\\d+)",
                          "&lambda;<sub>\\1</sub>",
                          par_est_df$label)
  par_est_df$label <- gsub("alpha_(\\d+)",
                          "&alpha;<sub>\\1</sub>",
                          par_est_df$label)
  par_est_df$label[grep("rel_", par_est_df$label)] <- sprintf("Rel(%s)", lavNames(fitted_model))
  par_est_df$label[grep("sumrel", par_est_df$label)] <- "Rel(S)"
  par_est_df$label[grep("sigma_eta", par_est_df$label)] <- "&sigma;&#x302;<sub>&eta;</sub>"

  # Bind all! -------------------------------------------------------------------------------
  rbind(cbind(Item = lavNames(fitted_model),
              par_est_df[grep("lambda", par_est_df$label), ],
              par_est_df[grep("alpha", par_est_df$label), ],
              par_est_df[grep("epsilon", par_est_df$label), ],
              par_est_df[grep("Rel", par_est_df$label)[1:length(lavNames(fitted_model))], ]),
        c(Item = NA,
          label = NA,
          est = NA,
          se = NA,
          CI = NA,
          label = NA,
          est = NA,
          se = NA,
          CI = NA,
          par_est_df[grep("eta", par_est_df$label), ],
          par_est_df[grep("Rel\\(S\\)", par_est_df$label), ])
        )
}

#' @export
succ_model_test <- function(fitted_models) {

}
