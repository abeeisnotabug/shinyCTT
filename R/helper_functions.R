#' @export

test_result_output <- function(statistic,  parameter = FALSE, pvalue) {
  if (isFALSE(parameter)) {
    parameter_string <- ""
  } else {
    names(parameter) <- gsub(".scaled", "", names(parameter))
    parameter_string <- sprintf(" %s = %i,",
                                names(parameter),
                                round(parameter, 3))
  }

  names(statistic) <- gsub(".scaled", "", names(statistic))

  sprintf("%s = %.3f,%s p %s %.3f",
          names(statistic),
          round(statistic, 3),
          parameter_string,
          if (pvalue < 0.001) {"<"} else {"="},
          if (pvalue < 0.001) {0.001} else {round(pvalue, 3)})
}
