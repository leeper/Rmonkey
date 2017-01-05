#' as.data.frame.surveyresponses
#' 
#' Extracts data from the survey responses data set and formats it as a data frame for analysis
#' 
#' @param survey A sm_survey object, as retrieved by \code{surveylist()}.
#' @return A data frame with survey responses
#' @export as.data.frame.surveyresponses


as.data.frame.surveyresponses <- function(survey) {
  df <- data.frame()
  sr <- getresponses(survey, bulk = TRUE)
  survey_id <- survey$id
  
  # Iterate through responses
  for (h in sr) {
    response_id <- h$id
    recipient_id <- h$recipient_id
    collector_id <- h$collector_id
    questions <-
      do.call('c', lapply(h$pages, function(x)
        x[['questions']]))
    for (i in questions) {
      question_id <- i$id
      j <- 0
      # use a repeat loop to account for cases where there are no answer rows
      repeat {
        j <- j + 1     # increment counter first for array indexing
        if (is.null(i$answers[[j]]$row_id)) {
          subquestion_id <- NA
        } else {
          subquestion_id <- i$answers[[j]]$row_id
        }
        if (is.null(i$answers[[j]]$choice_id)) {
          answerchoice_id <- NA
        } else {
          answerchoice_id <- i$answers[[j]]$choice_id
        }
        newrow <-
          data.frame(
            response_id,
            survey_id,
            recipient_id,
            collector_id,
            question_id,
            subquestion_id,
            answerchoice_id,
            stringsAsFactors = FALSE,
            check.rows = FALSE
          )
        df <- rbind(df, newrow)
        if (j >= length(i$answers)) {
          break
        }
      }
    }
  }
  return(df)
}
  
  # Future work
  #
  
  # do.call(rbind, lapply(i$answers, function(x) data.frame(answerchoice_id = x$choice_id, subquestion_id = x$row_id, stringsAsFactors = FALSE)))