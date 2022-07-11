#' @export
pick_eye <- function(t, var, lab)
{
  if (!inherits(t, "data.table")) error("This function only works for data.tables!")

  varcols <- names(t)[grepl(paste0("^", var, "_"), names(t))]
  t[eye == "OD", newvar := .SD, .SDcols = varcols[1]]
  t[eye == "OS", newvar := .SD, .SDcols = varcols[2]]
  label(t$newvar) <- lab
  setnames(t, "newvar", var)

  t
}
