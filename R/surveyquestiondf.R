# @ surveyquestiondf.r
#
# This program creates a data frame from the survey questions and answers
surveyquestiondf <- function(survey) {
  df <- data.frame()
  sd <- surveydetails(survey, question_details = TRUE)
  questions <- do.call('c', lapply(details$pages, function(i) i[['questions']]))
  for (i in questions) {
    id <- i$id
    family <- i$family
    subtype <- i$subtype
    heading <- i$heading
    if (!is.null(i$answer$rows)) {
      for (j in i$answer$rows) {
        row_text <- j$text
        newrow <-
          data.frame(
            id,
            family,
            subtype,
            heading,
            row_text,
            stringsAsFactors = FALSE,
            check.rows = FALSE
          )
        df <- rbind(df, newrow)
      }
    } else {
      row_text <- ''
      newrow <-
        data.frame(
          id,
          family,
          subtype,
          heading,
          row_text,
          stringsAsFactors = FALSE,
          check.rows = FALSE
        )
      df <- rbind(df, newrow)
    }
  }
  return(df)
}