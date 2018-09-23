#' @export
make_model_codes <- function(input_data, item_cols, group = FALSE) {
  item_names <- colnames(input_data[, item_cols])
  n_items <- length(item_cols)

  models <- c("tko", "ete", "teq", "etp", "tpa")

  one_disc <- rep("lambda_1", n_items)
  mult_disc <- paste("lambda", 1:n_items, sep = "_")

  disc_par <- list(mult_disc,
                   one_disc,
                   one_disc,
                   one_disc,
                   one_disc)

  one_eas <- rep("alpha_1", n_items)
  mult_eas <- paste("alpha", 1:n_items, sep = "_")

  eas_par <- list(mult_eas,
                  mult_eas,
                  one_eas,
                  mult_eas,
                  one_eas)

  one_err_var <- rep("sigma_epsilon_1", n_items)
  mult_err_var <- paste("sigma_epsilon", 1:n_items, sep = "_")

  err_var <- list(mult_err_var,
                  mult_err_var,
                  mult_err_var,
                  one_err_var,
                  one_err_var)

  names(disc_par) <- names(eas_par) <- names(err_var) <- models

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
extractFitParameters <- function(fittedModel) {
  scaledAddon <- switch(length(fittedModel@test), "", ".scaled")
  rawParams <- lavInspect(fittedModel, what = "fit")

  paramsDfLeft <- as.data.frame(t(
    rawParams[c(
      paste0(c("df", "chisq", "pvalue"), scaledAddon),
      paste0(c("rmsea", "rmsea.pvalue"), scaledAddon)
    )]
  ))

  paramsDfRight <- as.data.frame(t(
    rawParams[c(
      paste0("cfi", scaledAddon),
      "srmr",
      "aic",
      "bic"
    )]
  ))

  paramsDf <- cbind(paramsDfLeft,
                    rmsea.ci = sprintf("[%.3f, %.3f]",
                                       rawParams[paste0("rmsea.ci.lower", scaledAddon)],
                                       rawParams[paste0("rmsea.ci.upper", scaledAddon)]),
                    paramsDfRight,
                    stringsAsFactors = FALSE)

  names(paramsDf) <- gsub(".scaled", "", names(paramsDf))

  paramsDf
}

#' @export
extractParameters <- function(fittedModel, alpha = 0.05) {
  par_est_df <- parameterEstimates(fittedModel,
                                   zstat = F,
                                   pvalue = F,
                                   rsquare = T)[, -c(1, 2, 3)]

  ## CIs for reliabilities: ----------------------------------------------------
  rels <- par_est_df$est[grep("rel_", par_est_df$label)]
  rels_se <- par_est_df$se[grep("rel_", par_est_df$label)]

  rels_logit <- log(rels / (1 - rels))
  rels_logit_se <- rels_se / (rels * (1 - rels))

  rels_ci_l <- 1 / (1 + exp(-rels_logit + qnorm(1 - alpha / 2) * rels_logit_se))
  rels_ci_u <- 1 / (1 + exp(-rels_logit - qnorm(1 - alpha / 2) * rels_logit_se))

  par_est_df$ci.lower[grep("rel_", par_est_df$label)] <- rels_ci_l
  par_est_df$ci.upper[grep("rel_", par_est_df$label)] <- rels_ci_u
  # ----------------------------------------------------------------------------

  par_est_df <- par_est_df[grep("epsilon|alpha|lambda|eta|rel", par_est_df$label), ]
  par_est_df$CI <- sprintf("[%.3f, %.3f]", par_est_df$ci.lower, par_est_df$ci.upper)
  par_est_df <- par_est_df[, -c(4, 5)]

  # Prepare the names for HTML
  par_est_df$label <- gsub("sigma_epsilon_(\\d+)",
                          "&sigma;&#x302;&sup2;<sub>&epsilon;<sub>\\1</sub></sub>",
                          par_est_df$label)
  par_est_df$label <- gsub("lambda_(\\d+)",
                          "&lambda;&#x302;<sub>\\1</sub>",
                          par_est_df$label)
  par_est_df$label <- gsub("alpha_(\\d+)",
                          "&alpha;&#x302;<sub>\\1</sub>",
                          par_est_df$label)
  par_est_df$label[grep("rel_", par_est_df$label)] <- sprintf("R&#x302;<sub>%s</sub>", lavNames(fittedModel))
  par_est_df$label[grep("sumrel", par_est_df$label)] <- "R&#x302;<sub>&Sigma;</sub>"
  par_est_df$label[grep("sigma_eta", par_est_df$label)] <- "&sigma;&#x302;&sup2;<sub>&eta;</sub>"

  # Bind all! -------------------------------------------------------------------------------
  rbind(cbind(Item = lavNames(fittedModel),
              par_est_df[grep("lambda", par_est_df$label), ],
              par_est_df[grep("alpha", par_est_df$label), ],
              par_est_df[grep("epsilon", par_est_df$label), ],
              par_est_df[grep("R&#x302;", par_est_df$label)[1:length(lavNames(fittedModel))], ]),
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
          par_est_df[grep("R&#x302;<sub>&Sigma;", par_est_df$label), ])
        )
}
