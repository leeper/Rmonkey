getresponses <- function(
    survey,
    collector = NULL,
    bulk = FALSE,
    oauth_token = getOption('sm_oauth_token'),
    ...
){
    if (inherits(survey, 'sm_survey')) {
        survey$id <- survey$id
    } else {
        stop("'survey' is not of class sm_survey")
    }
    if (!is.null(collector)) {
        if (bulk) {
          u <- paste('https://api.surveymonkey.net/v3/collectors/',collector$id,'/responses/bulk?', sep='')   
        } else {
          u <- paste('https://api.surveymonkey.net/v3/collectors/',collector$id,'/responses?', sep='')  
        }
    } else {
        if (bulk) {
          u <- paste('https://api.surveymonkey.net/v3/surveys/',survey$id,'/responses/bulk?', sep='')  
        } else {
          u <- paste('https://api.surveymonkey.net/v3/surveys/',survey$id,'/responses?', sep='')  
        }
    }
    if (!is.null(oauth_token)) {
        token <- paste('bearer', oauth_token)
    } else {
        stop("Must specify 'oauth_token'")
    }
    h <- add_headers(Authorization=token,
                     'Content-Type'='application/json')
    out <- GET(u, config = h, ...)
    stop_for_status(out)
    content <- parsed_content(out)
    # if (content$status != 0) {
    #     warning("An error occurred: ",content$errmsg)
    #     return(content)
    # } else {
    if (!is.null(content$data)) {
        lapply(content$data, `class<-`, "sm_response")
        # content$data <- lapply(content$data, `attr<-`, 'survey_id', survey)
    }
    return(structure(content, class = 'sm_response_list'))
}

print.sm_response <- function(x, ...){
    if (!is.null(x$id)) {
        cat('Respondent ID:',x$id,'\n')
    }
    invisible(x)
}


getallresponses <- function(
    survey,
    collector = NULL,
    oauth_token = getOption('sm_oauth_token'),
    wait = 0,
    ...
) {
    r <- respondentlist(survey, api_key = api_key, oauth_token = oauth_token, ...)
    Sys.sleep(wait)
    respondents <- unname(sapply(r, `[`, "respondent_id"))
    Sys.sleep(wait)
    n <- ceiling(length(respondents)/100)
    w <- split(1:length(respondents), rep(1:n, each = 100)[1:length(respondents)])
    out <- list()
    for (i in seq_len(n)) {
        out <- c(out, getresponses(unlist(respondents[w[[i]]]), survey = survey, 
                                   api_key = api_key, oauth_token = oauth_token, ...))
        Sys.sleep(wait)
    }
    class(out) <- 'sm_response_list'
    d <- surveydetails(survey, api_key = api_key, oauth_token = oauth_token, ...)
    as.data.frame(out, details = d)
}
