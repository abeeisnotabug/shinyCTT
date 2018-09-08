#' @export
makeKable <- function(table,
                      digits = 3,
                      full_width = FALSE,
                      position = "center",
                      bootstrap_options = "striped",
                      col.names = NA,
                      ...) {
  kableExtra::kable_styling(
    kableExtra::kable(table,
                      digits = digits,
                      escape = FALSE,
                      col.names = col.names),
    full_width = full_width,
    position = position,
    bootstrap_options = bootstrap_options,
    ...)
}

#' @export
test_result_output <- function(test_vector, estimator) {
  if (length(test_vector) == 2) {
    parameter_string <- ""
  } else {
    parameter_string <- sprintf(" %s = %i,\\",
                                names(test_vector)[2],
                                test_vector[2])
  }

  sprintf("$$\\text{%s-}%s = %.3f,\\ %s p %s %.3f$$",
          estimator,
          names(test_vector)[1],
          round(test_vector[1], 3),
          parameter_string,
          if (test_vector[3] < 0.001) {"<"} else {"="},
          if (test_vector[3] < 0.001) {0.001} else {round(test_vector[3], 3)})
}

#' @export
create_corr_table_with_cis <- function(input_data, alpha = 0.05) {
  cor_table <- cor(input_data)
  n_items <- dim(cor_table)[1]

  CIs <- corrplot::cor.mtest(input_data, conf.level = (1 - alpha))[c(2,3)]

  out_table <- matrix("", nrow = n_items, ncol = n_items)

  for (i in 1:n_items) {
    for (j in 1:i) {
      out_table[i, j] <- sprintf("%.3f<p>[%.3f; %.3f]", cor_table[i, j], CIs$lowCI[i, j], CIs$uppCI[i, j])
    }
  }

  colnames(out_table) <- rownames(out_table) <- colnames(input_data)

  out_table
}
