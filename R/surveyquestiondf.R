# @ surveyquestiondf.r
#
# This program creates a data frame from the survey questions and answers
surveyquestiondf <- function(survey) {
  df <- data.frame()
  sd <- surveydetails(survey, question_details = TRUE)
  questions <- do.call('c', lapply(details$pages, function(i) i[['questions']]))

  # answerrows <- do.call('c', lapply(questions, function(i) i[['answers']][['rows']]))
  # answerchoices <- answerchoices <- do.call('c', lapply(questions, function(i) i[['answers']][['choices']]))
  
  # q_df <- do.call(rbind, lapply(questions, function(x) data.frame(question_id = x$id, question_type = x$family, question_subtype = x$subtype, question_text = x$heading, stringsAsFactors = FALSE)))
  # ac_df <- do.call(rbind, lapply(answerchoices, function(x) data.frame(answerchoice_text = x$text, answerchoice_id = x$id, stringsAsFactors = FALSE)))
  # ar_df <- do.call(rbind, lapply(answerrows, function(x) data.frame(subquestion_text = x$text, subquestion_id = x$id, stringsAsFactors = FALSE)))

  # these work but don't preserve the question ID in the ar and ac frames preventing joining
  
  # experiment to extract row data and then apply question ids as names
  # q_id <- do.call('c', lapply(questions, function(i) i[['id']]))
  # ar <- lapply(questions, function(i) i[['answers']][['rows']])
  # write the question_id into the answer row list prior to unpacking
  # for (i in 1:length( ar) ) {if (!is.null(ar[[i]])) {ar[[i]]$rows$question_id <- questions[[i]]$id}}
  # setNames(ar, q_id)
  
  for (i in questions) {
    id <- i$id
    family <- i$family
    subtype <- i$subtype
    heading <- i$heading
    j <- 0
    # use a repeat loop to account for cases where there are no answer rows 
    repeat {        
      j <- j + 1     # increment the index first for array indexing
      if (is.null(i$answers$rows)) {
        row_id <- NA
        row_text <- NA
      } else {
        row_id <- i$answers$rows[[j]]$id
        row_text <- i$answers$rows[[j]]$text
      }
      newrow <-
        data.frame(
          id,
          row_id,
          family,
          subtype,
          heading,
          row_text,
          stringsAsFactors = FALSE,
          check.rows = FALSE
        )
      df <- rbind(df, newrow)
      if (j >= length(i$answers$rows)) {break}
    }
  }
  return(df)
}
