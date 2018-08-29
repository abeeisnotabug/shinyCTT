#' @export
test_result_output <- function(statistic,  parameter = FALSE, pvalue, estimator) {
  if (isFALSE(parameter)) {
    parameter_string <- ""
  } else {
    names(parameter) <- gsub(".scaled", "", names(parameter))
    parameter_string <- sprintf(" %s = %i,\\",
                                names(parameter),
                                round(parameter, 3))
  }

  names(statistic) <- gsub(".scaled", "", names(statistic))

  sprintf("$$\\text{%s-}%s = %.3f,\\ %s p %s %.3f$$",
          estimator,
          names(statistic),
          round(statistic, 3),
          parameter_string,
          if (pvalue < 0.001) {"<"} else {"="},
          if (pvalue < 0.001) {0.001} else {round(pvalue, 3)})
}

#' @export
create_corr_table_with_cis <- function(input_data, alpha = 0.05) {
  cor_table <- cor(input_data)
  n_items <- dim(cor_table)[1]

  CIs <- corrplot::cor.mtest(input_data, conf.level = (1 - alpha))[c(2,3)]

  out_table <- matrix("", nrow = n_items, ncol = n_items)

  for (i in 1:n_items) {
    for (j in 1:i) {
      out_table[i, j] <- sprintf("%.3f [%.3f; %.3f]", cor_table[i, j], CIs$lowCI[i, j], CIs$uppCI[i, j])
    }
  }

  colnames(out_table) <- rownames(out_table) <- colnames(input_data)

  out_table
}
