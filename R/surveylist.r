#' surveylist
#'
#' Obtains a list of surveys for a SurveyMonkey account.
#'
#' This function calls the SurveyMonkey API using the current oauth token and returns
#' a list of surveys based on the parameters entered.
#'
#' @param page Integer numebr to select which page of resources to return. By default is 1.
#' @param per_page Integer number to set the number of surveys to return per page.  By default, is 50 surveys per page.
#' @param sort_by String used to sort returned survey list: ‘title’, 'date_modified’, or 'num_responses’. By default, date_modified.
#' @param sort_order String used to set the sort order for returned surveys: 'ASC’ or 'DESC’. By default, DESC.
#' @param start_modified_at Date string used to select surveys last modified after this date. By default is NULL.
#' @param end_modified_at Date string used to select surveys modified before this date.  By default is NULL.
#' @param title String used to select survey by survey title.  By default is NULL.
#' @param include Comma separated strings used to filter survey list: 'shared_with’, 'shared_by’, or 'owned’ (useful for teams) or to specify additional fields to return per survey: 'response_count’, 'date_created’, 'date_modified’, 'language’, 'question_count’, 'analyze_url’, 'preview’.  By default is NULL.
#' @param oauth_token The SurveyMonkey App oauth_token stored in the environment.
#' @return sm_surveylist

surveylist <- function(
    page = NULL,
    per_page = NULL,
    sort_by = NULL,
    sort_order = NULL,
    start_modified_at = NULL,
    end_modified_at = NULL,
    title = NULL,
    include = NULL,
    oauth_token = getOption('sm_oauth_token'),
    ...
){
    if(!is.null(oauth_token)){
        u <- 'https://api.surveymonkey.net/v3/surveys?'
        token <- paste('bearer', oauth_token)
    }
    else
        stop("Must specify 'oauth_token'")
    if(inherits(start_modified_at, "POSIXct") | inherits(start_modified_at, "Date"))
      start_modified_at <- format(start_modified_at, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    if(inherits(end_modified_at, "POSIXct") | inherits(end_modified_at, "Date"))
      end_modified_at <- format(end_modified_at, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    b <- list(    page = page,
                  per_page = per_page,
                  sort_by = sort_by,
                  sort_order = sort_order,
                  start_modified_at = start_modified_at,
                  end_modified_at = end_modified_at,
                  title = title,
                  include = include)
    nulls <- sapply(b, is.null)
    if(all(nulls))
        b <- NULL
    else
        b <- b[!nulls]
    h <- add_headers(Authorization=token,
                     'Content-Type'='application/json')
    out <- GET(u, config = h, ..., query = b)
    stop_for_status(out)
    content <- content(out, as = 'parsed')
    sl <- content$data
    lapply(sl, `class<-`, 'sm_survey')
}