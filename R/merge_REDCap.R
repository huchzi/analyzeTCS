merge_redcap <- function(tCSF, redcap) {
  
  stopifnot(is.data.table(tCSF))
  stopifnot(nrow(tCSF) > 0)
  
  redcap[end_date == "", end_date := begin_date]
  
  exams <- split(redcap, 1:nrow(redcap))
  exams <- 
    rbindlist(
      lapply(exams, 
             function(x) data.table(patid = as.integer(x$patid),
                                    redcap_repeat_instance = as.integer(x$redcap_repeat_instance),
                                    date_of_exam = seq(as.Date(x$begin_date), 
                                                       as.Date(x$end_date),
                                                       by = "days")
             )
      )
    )
  
  data <- merge(redcap, exams, 
                by = c("patid", "redcap_repeat_instance"), 
                all.y = TRUE)
  
  result <- merge(tCSF, redcap, by = c("patid", "date_of_exam"))
  
  result
}
