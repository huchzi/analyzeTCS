#' @export
adjust_for_age <-
  function(sensitivity, real_age, adjusted_age) {
    return(
      10^(log10(sensitivity) - 0.01 * (adjusted_age - real_age))
    )
  }

#' @export
label_age <- function(tab)
{
  return(
    function(x) {
      sapply(x, function(y) as.character(tab[patid == y, .(mean(age_exam))]))
    }
  )
}
