#' userdetails
#'
#' Obtains information about SurveyMonkey user.
#'
#' This function calls the SurveyMonkey API using the current oauth token and returns
#' information about the SurveyMonkey user and account associated with the token.
#'
#' @param oauth_token The SurveyMonkey App oauth_token stored in the environment.
#' @return userdetails

userdetails <- function(oauth_token = getOption('sm_oauth_token'), ...) {
  u <- 'https://api.surveymonkey.net/v3/users/me'
  if (!is.null(oauth_token))
    token <- paste('bearer', oauth_token)
  else
    stop("Must specify 'oauth_token'.  Try smlogin() first to get a token.")
  out <- GET(u, config = add_headers(Authorization = token,
                                  'Content-Type' = 'application/json'))
  stop_for_status(out)
  content <- content(out, as='parsed')
  structure(content, class = "sm_userdetails")
  return(content)
}