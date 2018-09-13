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
  names(test_vector) <- gsub(".scaled", "", names(test_vector))

  if (length(test_vector) == 2) {
    parameter_string <- ""
  } else {
    parameter_string <- sprintf(" %s = %i,\\",
                                names(test_vector)[2],
                                test_vector[2])
  }

  sprintf("$$\\text{%s-}%s = %.3f,\\ %s p %s %.3f$$",
          estimator,
          gsub("chisq", "\\\\chi^2", names(test_vector)[1]),
          round(test_vector[1], 3),
          parameter_string,
          if (test_vector[3] < 0.001) {"<"} else {"="},
          if (test_vector[3] < 0.001) {0.001} else {round(test_vector[3], 3)})
}
