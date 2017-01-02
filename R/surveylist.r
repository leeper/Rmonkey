surveylist <- function(
    page = NULL,
    page_size = NULL,
    start_date = NULL,
    end_date = NULL,
    title = NULL,
    recipient_email = NULL,
    order_asc = NULL,
    fields = NULL,
    oauth_token = getOption('sm_oauth_token'),
    ...
){
    if(!is.null(oauth_token)){
        u <- 'https://api.surveymonkey.net/v3/surveys?'
        token <- paste('bearer', oauth_token)
    }
    else
        stop("Must specify 'oauth_token'")
    if(inherits(start_date, "POSIXct") | inherits(start_date, "Date"))
        start_date <- format(start_date, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    if(inherits(end_date, "POSIXct") | inherits(end_date, "Date"))
        end_date <- format(end_date, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    b <- list(page = page, page_size = page_size,
              start_date = start_date, end_date = end_date,
              title = title, recipient_email = recipient_email,
              order_asc = order_asc, fields = as.list(fields))
    nulls <- sapply(b, is.null)
    if(all(nulls))
        b <- '{}'
    else
        b <- toJSON(b[!nulls], auto_unbox = TRUE)
    h <- add_headers(Authorization=token,
                     'Content-Type'='application/json')
    out <- GET(u, config = h, ..., query = b)
    stop_for_status(out)
    content <- content(out, as = 'parsed')
    lapply(content$data, `class<-`, 'sm_survey')
    
}