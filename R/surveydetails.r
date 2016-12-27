surveydetails <- function(
    survey,
    oauth_token = getOption('sm_oauth_token'),
    question_details = FALSE,
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

surveyquestions <- function(
    survey,
    details,
    oauth_token = getOption('sm_oauth_token'),
    ...
){
    if (!missing(survey)) {
        d <- surveydetails(survey, oauth_token = oauth_token, ...)
    } else {
        d <- details
    }
    questions <- unlist(unlist(lapply(d$pages, `[`, "questions"), recursive = FALSE), recursive = FALSE)
    n <- unname(unlist(lapply(questions, `[`, "question_id")))
    w <- unname(unlist(lapply(questions, `[`, "heading")))
    structure(w, names = n, class = c("character", "sm_surveyquestions"))
}

surveypreview <- function(details) {
    browseURL(details$preview_url)
}
