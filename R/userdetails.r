userdetails <- function(
    oauth_token = getOption('sm_oauth_token'),
    ...
){
    u <- 'https://api.surveymonkey.net/v3/users/me'
    if(!is.null(oauth_token))
        token <- paste('bearer', oauth_token)
    else
        stop("Must specify 'oauth_token'.  Try smlogin() first to get a token.")
    out <- GET(u, config = add_headers(Authorization=token))
    stop_for_status(out)
    content <- parsed_content(out)
    # if(content$status != 0)
    #     warning("An error occurred: ",content$errmsg)
    structure(content$data$user_details, class="sm_userdetails")
}
