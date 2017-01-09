#' surveyquestions
#' 
#' Creates a data frame from the survey questions and answers
#' 
#' @param survey A sm_survey object, as retrieved by \code{surveylist()}.
#' @return A data frame with one row per question/subquestion/answer choice
#' @export surveyquestions
#' 
surveyquestions <- function(survey) {
  df <- data.frame()
  sd <- surveydetails(survey, question_details = TRUE)
  survey_id <- sd$id
  questions <-
    do.call('c', lapply(sd$pages, function(i)
      i[['questions']]))
  for (i in questions) {
    question_id <- i$id
    question_type <- i$family
    question_subtype <- i$subtype
    question_text <- gsub("<.*?>", "",unlist(i$heading))
    
    j <- 0
    # use a repeat loop to account for cases where there are no answer rows
    repeat {
      j <- j + 1     # increment counter first for array indexing
      if (is.null(i$answers$rows)) {
        subquestion_id <- NA
        subquestion_text <- NA
      } else {
        subquestion_id <- i$answers$rows[[j]]$id
        subquestion_text <- i$answers$rows[[j]]$text
      }
      k <- 0
      repeat {
        k <- k + 1    # increment counter first for array indexing
        if (is.null(i$answers$choices)) {
          answerchoice_id <- NA
          answerchoice_text <- NA
          answerchoice_weight <- NA
        } else {
          answerchoice_id <- i$answers$choices[[k]]$id
          answerchoice_text <- i$answers$choices[[k]]$text
          if (!is.null(i$answers$choices[[k]]$weight)) {
            answerchoice_weight <-
              i$answers$choices[[k]]$weight
          } else {
            answerchoice_weight <- NA
          }
        }
        newrow <-
          data.frame(
            survey_id,
            question_id,
            subquestion_id,
            answerchoice_id,
            question_type,
            question_subtype,
            question_text,
            subquestion_text,
            answerchoice_text,
            answerchoice_weight,
            stringsAsFactors = FALSE,
            check.rows = FALSE
          )
        
        # append a second new row for other options on select questions
        if(!is.null(i$answers$other) & k == 1) {
          answerchoice_id <- i$answers$other$id
          answerchoice_text <- i$answers$other$text
          answerchoice_weight <- NA
          newrow2 <-
            data.frame(
              survey_id,
              question_id,
              subquestion_id,
              answerchoice_id,
              question_type,
              question_subtype,
              question_text,
              subquestion_text,
              answerchoice_text,
              answerchoice_weight,
              stringsAsFactors = FALSE,
              check.rows = FALSE
            )
          newrow <- rbind(newrow, newrow2)
        }
        
        # add new row(s) to dataframe
        df <- rbind(df, newrow)
        if (k >= length(i$answers$choices)) {
          break
        }
      }
      if (j >= length(i$answers$rows)) {
        break
      }
    }
    
  }
  return(df)
}

# Future work
#
# This code works but is inelegant since it uses loops (for and repeat) vs. using vectorized approaches like lapply
# or data table approaches like those in dplyr.  To use lapply, I have to solve how to nest the functions
# so I can both manage cases where there are no rows for some answers (e.g., single choice answers) and the
# case where there are multiple rows per answer which require applying the choices to each row.
# To use dplyr, I need to figure out how to build tables that I can join for quesitons, answer rows, and
# answer choices.  The trick here, is figuring out how to include the question id as a key in the
# answer row and answer choice tables.  Some work to this end is below:

# One can use the lapply in the inner call here to extract the answers$rows elements but they lack the
# question_id.  The index of the lapply array is the question number but once the do.call is applied
# that structure is lost.
# answerrows <- do.call('c', lapply(questions, function(i) i[['answers']][['rows']]))
# answerchoices <- answerchoices <- do.call('c', lapply(questions, function(i) i[['answers']][['choices']]))

# These functions make data frames from the resulting data.  If the question ids were included they could be joined
# with dplyr to offer a more elegant solution
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
