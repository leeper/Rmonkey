# getresponses.r
#
# This function returns details on SurveyMonkey responses
#
# get a set of bulk responses (this will get 50 responses with the following structure:
# $per_page        : int  = total number of responses per page
# $total           : int  = number of survey responses
# $data            : list = list with data for each survey response
# $data[[x]]       : list = individual survey response
#   $total_time   : int  = time spent on the survey
#   $href         : chr  = api url for survey response
#   $custom_variables  : list = custom variables for respondents
#   $ip_address : chr  = IP address for respondent
#   $id : chr = id of survey response
#   $logic_path : list
#   $date_modified : chr = date survey response last modified
#   $response_status : chr = status of response {completed, partial, etc...}
#   $custom_value : chr = ?
#   $analyze_url : chr = web browsable url to view responses
#   $pages : list = list with data for questions and answers on each survey page
#     $id : chr = id
#     $ questions : list
#       $ id : chr = id
#       $ answers : list
#         $ choice_id : chr = id of answer choice
#   $page_path : list = ?
#   $recipient_id : chr = id of survey recipient
#   $collector_id : chr = id of survey collector
#   $date_created : chr = date the survey response was started
#   $survey_id : chr = id of the survey
#   $collection_mode : chr = ?
#   $edit_url : chr = web browsable url to modify responses
#   $metadata : list = list with additional information about respondent
#     $contact : list 
#     $contact$first_name : list
#     $contact$first_name$type : chr = type for first_name$value variable
#     $contact$first_name$value : chr = respondent first name
#     $contact$last_name : list
#     $contact$last_name$type : chr = type for last_name$value variable
#     $contact$lasy_name$value : chr = respondent last name
#     $contact$email : list
#     $contact$email$type : chr = type for email variable
#     $contact$email$value : chr = respondent email address
# $page      : int  = page of responses
# $links     : list = urls for the previous ($last), current ($self) and next ($next) response pages
# )



getresponses <- function(
    survey,
    collector = NULL,
    bulk = FALSE,
    page = NULL,
    per_page = NULL,
    start_created_at = NULL,
    end_created_at = NULL,
    start_modified_at = NULL,
    end_modified_at = NULL,
    sort_order = 'ASC',
    sort_by = 'date_modified',
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
  if (inherits(start_created_at, "POSIXct") | inherits(start_created_at, "Date")) {
    start_created_at <- format(start_created_at, "%Y-%m-%d")
  }
  if (inherits(end_created_at, "POSIXct") | inherits(end_created_at, "Date")) {
    end_created_at <- format(end_created_at, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  }
  if (inherits(start_modified_at, "POSIXct") | inherits(start_modified_at, "Date")) {
    start_modified_at <- format(start_modified_at, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  }
  if (inherits(end_modified_at, "POSIXct") | inherits(end_modified_at, "Date")) {
    end_modified_at <- format(end_modified_at, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  }
  # need to add error checking for status
  b <- list(page = page, 
            per_page = per_page,
            start_created_at = start_created_at, 
            end_created_at = end_created_at,
            start_modified_at = start_modified_at,
            end_modified_at = end_modified_at,
            sort_order = sort_order, 
            sort_by = sort_by)
  nulls <- sapply(b, is.null)
  if (all(nulls)) {
    b <- '{}'
  } else {
    b <- toJSON(b[!nulls], auto_unbox = TRUE)
  }  
  h <- add_headers(Authorization=token,
                     'Content-Type'='application/json')
    out <- GET(u, config = h, ..., body = b)
    stop_for_status(out)
    content <- content(out, as = 'parsed')
    if (!is.null(content$data)) {
        lapply(content$data, `class<-`, "sm_response")
    }
    structure(content, class = 'sm_response_list')
}

print.sm_response <- function(x, ...){
    if (!is.null(x$id)) {
        cat('Respondent ID:',x$id,'\n')
    }
    invisible(x)
}


# getallresponses <- function(
#     survey,
#     collector = NULL,
#     oauth_token = getOption('sm_oauth_token'),
#     wait = 0,
#     ...
# ) {
#     r <- respondentlist(survey, api_key = api_key, oauth_token = oauth_token, ...)
#     Sys.sleep(wait)
#     respondents <- unname(sapply(r, `[`, "respondent_id"))
#     Sys.sleep(wait)
#     n <- ceiling(length(respondents)/100)
#     w <- split(1:length(respondents), rep(1:n, each = 100)[1:length(respondents)])
#     out <- list()
#     for (i in seq_len(n)) {
#         out <- c(out, getresponses(unlist(respondents[w[[i]]]), survey = survey, 
#                                    api_key = api_key, oauth_token = oauth_token, ...))
#         Sys.sleep(wait)
#     }
#     class(out) <- 'sm_response_list'
#     d <- surveydetails(survey, api_key = api_key, oauth_token = oauth_token, ...)
#     as.data.frame(out, details = d)
# }
