#' as.data.frame.surveyresponses
#' 
#' Extracts data from the survey responses data set and formats it as a data frame for analysis
#' 
#' @param survey A sm_survey object, as retrieved by \code{surveylist()}.
#' @return A data frame with survey responses
#' @export as.data.frame.surveyresponses


as.data.frame.surveyresponses <- function(survey) {
  df <- data.frame()
  sr <- getresponses(survey, bulk = TRUE, all_page = TRUE)
  sq <- surveyquestiondf(survey)
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
  
  # join responses to question data
  df <- dplyr::left_join (df, sq)
  
  # Combine the two question headers to make a single one
  df$question_text_full <-
    ifelse (
      df$question_type == 'multiple_choice',
      paste(df$question_text, "-", df$answerchoice_text),
      ifelse(
        !is.na(df$subquestion_text),
        paste(df$question_text, "-", df$subquestion_text),
        paste(df$question_text)
      )
    )
  
  # Select only the columns for the final dataframe
  df <- select(df, response_id, survey_id, collector_id, recipient_id, question_text_full, answerchoice_text)
  
  # Spread from column to tablular form
  df_table <- spread(df, question_text_full, answerchoice_text)
  
  return(df_table)
}
  
  # Future work
  #
  
  # do.call(rbind, lapply(i$answers, function(x) data.frame(answerchoice_id = x$choice_id, subquestion_id = x$row_id, stringsAsFactors = FALSE)))