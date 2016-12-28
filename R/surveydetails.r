surveydetails <- function(
    survey,
    question_details = FALSE,
    oauth_token = getOption('sm_oauth_token'),
    ...
){
    if(inherits(survey, 'sm_survey'))
        survey <- survey$id
    if(question_details) {
      u <- paste('https://api.surveymonkey.net/v3/surveys/',survey,'/details?',sep='')  
    }
    else 
      u <- paste('https://api.surveymonkey.net/v3/surveys/',survey,'?',sep='')  
    if(!is.null(oauth_token)) {
      token <- paste('bearer', oauth_token)
    }
    else
        stop("Must specify 'oauth_token'")
    h <- add_headers(Authorization=token,
                     'Content-Type'='application/json')
    out <- GET(u, config = h, ...)
    stop_for_status(out)
    content <- parsed_content(out)
    # if(content$status != 0) {
    #     warning("An error occurred: ",content$errmsg)
    #     return(content)
    # } else
    structure(content, class = "sm_survey")
}

surveyquestions <- function(survey){
    d <- surveydetails(survey, oauth_token = getOption('sm_oauth_token'), question_details = TRUE)
    questions <- unlist(unlist(lapply(d$pages, `[`, "questions"), recursive = FALSE), recursive = FALSE)
    n <- unname(unlist(lapply(questions, `[`, "id")))
    w <- unname(unlist(lapply(questions, `[`, "headings")))
    w <- gsub("<.*?>", "", w)
    structure(w, names = n, class = c("character", "sm_surveyquestions"))
}

surveypreview <- function(survey) {
    d <- surveydetails(survey, oauth_token = getOption('sm_oauth_token'))
    browseURL(d$preview)
}

print.sm_survey <- function(x, ...){
  if(!is.null(x$title)) 
    cat('Survey Title:', x$title, '\n')
  if(!is.null(x$nickname))
    cat('Survey Nickname:', x$nickname, '\n')
  if(!is.null(x$id))
    cat('ID:', x$id, '\n')
  if(!is.null(x$language))
    cat('Language:', x$language, '\n')
  if(!is.null(x$question_count))
    cat('No. of Questions:', x$question_count, '\n')
  if(!is.null(x$response_count))
    cat('Respondents:', x$response_count, '\n')
  if(!is.null(x$preview))
    cat('Preview URL:', x$preview, '\n')
  if(!is.null(x$analyze_url))
    cat('Analysis URL:', x$analyze_url, '\n')
  if(!is.null(x$date_created))
    cat('Date Created: ', x$date_created, '\n')
  if(!is.null(x$date_modified))
    cat('Date Modified:', x$date_modified, '\n')
  if(!is.null(x$pages))
    cat('Survey Pages:', length(x$pages), '\n')
  invisible(x)    
}
