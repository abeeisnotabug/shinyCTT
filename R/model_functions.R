#' @export
make_model_codes <- function(input_data, multi_group = FALSE) {
  item_names <- colnames(input_data)
  n_items <- length(item_names)

  models <- c("tau-kongeneric",
              "essentially tau-equivalent",
              "tau-equivalent",
              "essentially tau-parallel",
              "tau-parallel")

  models <- models[c(n_items > 3, n_items > 2, TRUE, TRUE, TRUE)]

  one_disc <- rep("lambda_1", n_items)
  mult_disc <- paste("lambda", 1:n_items, sep = "_")

  disc_par <- list("tau-kongeneric" = mult_disc,
                   "essentially tau-equivalent" = one_disc,
                   "tau-equivalent" = one_disc,
                   "essentially tau-parallel" = one_disc,
                   "tau-parallel" = one_disc)

  one_eas <- rep("alpha", n_items)
  mult_eas <- paste("alpha", 1:n_items, sep = "_")

  eas_par <- list("tau-kongeneric" = mult_eas,
                  "essentially tau-equivalent" = mult_eas,
                  "tau-equivalent" = one_eas,
                  "essentially tau-parallel" = mult_eas,
                  "tau-parallel" = one_eas)

  one_err_var <- rep("varepsilon", n_items)
  mult_err_var <- paste("varepsilon", 1:n_items, sep = "_")

  err_var <- list("tau-kongeneric" = mult_err_var,
                  "essentially tau-equivalent" = mult_err_var,
                  "tau-equivalent" = mult_err_var,
                  "essentially tau-parallel" = one_err_var,
                  "tau-parallel" = one_err_var)

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
    if (multi_group) {
      err_vars <- paste(sprintf("%s ~~ c(%s_g1, %s_g2) * %s",
                                item_names,
                                err_var[[model]],
                                err_var[[model]],
                                item_names),
                        collapse = "\n")
      eta_var <- "eta ~~ c(sigma_g1, sigma_g2) * eta"

      paste(eta_dep, err_vars, alphas, eta_var, sep = "\n")
    } else {
      err_vars <- paste(sprintf("%s ~~ %s * %s",
                                item_names,
                                err_var[[model]],
                                item_names),
                        collapse = "\n")
      eta_var <- "eta ~~ sigma * eta"

      # Item reliabilities:
      item_rels_names <- paste("rel", 1:n_items, sep = "_")
      def_names <- gsub("lambda_1", "1", disc_par[[model]])
      item_rels <- paste(sprintf("rel_%i := 1 / (1 + %s / (%s^2 * sigma))",
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
                                          ") / sigma")
      sum_rel <- sprintf("rel_sum := %s / (%s + %s)",
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
extract_fit_params <- function(fitted_model_fit_params, estimator, what) {
  par_names <- switch(what,
                      "model_fit" = c("chisq", "df", "pvalue"),
                      "corr_ind" = c("baseline.chisq", "baseline.df", "baseline.pvalue"),
                      "rmsea" = c("rmsea", "rmsea.pvalue"))

  scaled_addon <- switch(estimator,
                         "MLR" = ".scaled",
                         "ML" = "")

  to_extract <- paste0(par_names, scaled_addon)

  out_vec <- c(fitted_model_fit_params[to_extract])

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
                                       rsquare = T)

  ## CIs for reliabilities:
  rels <- par_est_df$est[grep("rel", par_est_df$label)]
  rels_se <- par_est_df$se[grep("rel", par_est_df$label)]

  rels_logit <- log(rels / (1 - rels))
  rels_logit_se <- rels_se / (rels * (1 - rels))

  rels_ci_l <- 1 / (1 + exp(-rels_logit + qnorm(1 - alpha / 2) * rels_logit_se))
  rels_ci_u <- 1 / (1 + exp(-rels_logit - qnorm(1 - alpha / 2) * rels_logit_se))

  par_est_df$ci.lower[grep("rel", par_est_df$label)] <- rels_ci_l
  par_est_df$ci.upper[grep("rel", par_est_df$label)] <- rels_ci_u

  par_est_df$label[grep("epsilon|alpha|lambda|sigma", par_est_df$label)] <-
    sprintf("\\%s", par_est_df$label[grep("epsilon|alpha|lambda|sigma", par_est_df$label)])

  par_est_df[grep("epsilon|alpha|lambda|sigma|rel", par_est_df$label), -c(1, 2, 3)]
}
