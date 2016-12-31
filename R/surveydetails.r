# surveydetails
#
# This function returns details about a SurveyMonkey survey in the list structure based on the
# JSON response format
#
# $response_count : int = total number of responses
# $pages_count : int = number of pages in the survey
# $buttons_text : list of chr = display text for buttons
# $custom_variables : named list = ?
# $nickname : chr = short name for survey
# $id : chr = SurveyMonkey id for the survey
# $question_count : int = total number of questions on the survey
# $category : chr = broad category for survey 
# $preview : chr = web browsable url to preview the survey
# $is_owner : logical = is user owner of the survey(?)
# $language : chr = langauge the survey is written in
# $date_modified : chr = date/time when survey was last modified
# $title : chr = survey title
# $analyze_url : chr = web browsable url to analyze responses
# $pages : list
#   $ href : chr = api accessible weblink for data on this page
#   $ description : chr = text displayed at top of page
#   $ questions : list = length is the number of questions on the page
#     $ display_options : list
#       $ show_display_number : logical 
#     $ sorting : 
#     $ family : chr = general style of question
#     $ subtype : chr = detailed style of question
#     $ required :
#       $ text: chr = text to display if not answered(?)
#       $ amount : chr = 
#       $ type : chr = ?
#     $ answers : list
#       $ choices : list
#         $ visible : logical 
#         $ text : chr = answerchoice text
#         $ position : int = answerchoice position
#         $ id : chr = answerchoice id
#     $ visible : logical 
#     $ href : api accessible weblink for this question
#     $ headings : list
#     $ heading : chr = HTML text displayed with question
#     $ position : int = display position for question on the page
#     $ validation : 
#     $ id : chr = id for the question
#     $ forced_ranking : logical 
#   $ title : chr
#   $ position : int
#   $ id : id for page
#   $ question_count : int = number of questions on this page
# $summary_url : chr = web browsable url for survey summary
# $href : chr = api accessible web link for survey data
# $date_created : chr = date survey originally created
# $collect_url : chr = web browsable url to collect responses
# $edit_url : chr = web browsable url to edit the survey

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
    content <- content(out, as = 'parsed')
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
