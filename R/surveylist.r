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
    lapply(content$data, `class<-`, 'sm_survey')
    
}