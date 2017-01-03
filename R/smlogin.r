#' smlogin
#'
#' Obtains a long lasting oauth token for API access.
#'
#' This function takes as input a SurveyMonkey API account client ID and
#' secret code and conducts the oauth2 authentication process to return
#' an oauth token.  The client_id and secret values can be obtained from the settings
#' section of the appropriate SurveyMonkey apps page at https://developer.surveymonkey.com/apps/
#'

#' @param client_id Your SurveyMonkey App client_id. By default, retrieved from \code{getOption('sm_client_id')}.
#' @param secret Your API secret key. By default, retrieved from \code{getOption('sm_secret')}.
#' @param redirect_uri Default value is \url{http://localhost:1410}. No other value is allowed. This must be the redirect URL registered for your application in your Survey Monkey developer account.
#' @param response_type Default value is \code{code}. No other values are allowed.
#' @return oauth_token
#' @export smlogin

smlogin <- function(client_id = getOption('sm_client_id'),
                    secret = getOption('sm_secret'),
                    redirect_uri = 'http://localhost:1410',
                    response_type = 'code') {
  if (is.null(client_id))
    stop("Must supply developer username as 'client_id'")
  if (is.null(secret))
    stop("Must supply developer secret key as 'secret'")
  a <- list(response_type = response_type,
            redirect_uri = redirect_uri,
            client_id = client_id)
  a <- paste(names(a),
             curl_escape(a),
             sep = '=',
             collapse = '&')
  e <- structure(list(authorize = 'https://api.surveymonkey.net/oauth/authorize',
                 access = 'https://api.surveymonkey.net/oauth/token'),
                 class = 'oauth_endpoint')
  e$authorize <- paste(e$authorize, a, sep = '?')
  smapp <- oauth_app('surveymonkey', client_id, secret)
  token <- oauth2.0_token(e, smapp, use_oob = FALSE, cache = FALSE)
  if ('error' %in% names(token$credentials)) {
    warning('OAuth error ', token$credentials$error, ': ', token$credentials$error_description, sep = '')
  } else
    options('sm_oauth_token' = token$credentials$access_token)
  invisible(token)
}
