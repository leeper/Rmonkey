print.sm_survey <- function(x, ...){
  if(!is.null(x$title)) {
    if(is.list(x$title))
      cat('Survey Title:', x$title$text, '\n')
    else
      cat('Survey Title:', x$title, '\n')
  }
  if(!is.null(x$survey_id))
    cat('ID:', x$survey_id, '\n')
  if(!is.null(x$language_id))
    cat('Language:', x$language_id, '\n')
  if(!is.null(x$question_count))
    cat('No. of Questions:', x$question_count, '\n')
  if(!is.null(x$num_responses))
    cat('Respondents:', x$num_responses, '\n')
  if(!is.null(x$preview_url))
    cat('Preview URL:', x$preview_url, '\n')
  if(!is.null(x$analysis_url))
    cat('Analysis URL:', x$analysis_url, '\n')
  if(!is.null(x$date_created))
    cat('Date Created: ', x$date_created, '\n')
  if(!is.null(x$date_modified))
    cat('Date Modified:', x$date_modified, '\n')
  if(!is.null(x$pages))
    cat('Survey Pages:', length(x$pages), '\n')
  invisible(x)    
}
