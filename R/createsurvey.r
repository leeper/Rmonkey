#function written by Tom Leeper
#proposed changes by Kevin Little, Ph.D. 24 August 2015
createsurvey <- function(
    template = NULL,
    survey = NULL,
    title,
    collector_name = NULL,
    type = 'email', # only 'email' is allowed
    recipients = NULL,
    #recipients needs to be a list of list(s) of character strings.  E.g. recipients=list(list('first@example.com),list('second@example.com))
    # or recipients=list(list(email='first@example.com,first_name='first',last_name='last',custom_id='123'))
    email_replyto = NULL,
    email_subject = NULL,
    email_body = NULL,
    #email_body is required containing [SurveyLink], [OptOutLink] and [FooterLink] (unless you have account privileges
    #to set global options in SurveyMonkey to remove presense of the SurveyMonkey Footer).  Note [OptOutLink] is the 
    #current (as of 24 Aug 2015) link replacing [RemoveLink] per http://help.surveymonkey.com/articles/en_US/kb/SurveyLink-and-RemoveLink
    api_key = getOption('sm_api_key'),
    oauth_token = getOption('sm_oauth_token')
){
    if(!is.null(api_key)) {
        u <- paste('https://api.surveymonkey.net/v2/batch/create_flow?',
                    'api_key=', api_key, sep='')
    } else
        stop("Must specify 'api_key'")
    if(!is.null(oauth_token))
        token <- paste('bearer', oauth_token)
    else
        stop("Must specify 'oauth_token'")
    # handle recipients list
    if(length(recipients)>10000){
        respondents <- head(recipients, 10000)
        warning("Maximum number of recipients exceeded. Only first 10000 used.")
    }
    recipients <- lapply(recipients, function(x){
        if(is.null(names(x))) {
            if(length(x)==1)
                return(c(email=x[1]))
            else if(length(x)==2)
                return(c(email=x[1], first_name=x[2]))
            else if(length(x)==3)
                return(c(email=x[1], first_name=x[2], last_name=x[3]))
            else if(length(x)==4)
                return(c(email=x[1], first_name=x[2],
                         last_name=x[3], custom_id=x[4]))
        } else
            return(x)
    })
    if(!is.null(template) & !is.null(survey)) {
        stop("Only 'template' xor 'survey' is allowed.")
    } else if(!is.null(template)){
        b <- list(survey=list(template_id = as.character(template), survey_title = title),
                  collector = if(!is.null(collector_name))
                  { list(type = type,
                         name = collector_name,
                         recipients = recipients)
                  } else { list(type = type,
                                recipients = recipients) },
                  email_message = if(is.null(email_body)) {
                  list(subject = email_subject, reply_email = email_replyto)
                  } else { list(reply_email = email_replyto,
                                            subject = email_subject,
                                            body_text = email_body #parameter corrected)})
    } else if(!is.null(survey)){
        if(inherits(survey, 'sm_survey'))
            survey <- survey$survey_id
        b <- list(survey=list(from_survey_id = as.character(survey), survey_title = title),
                  collector = if(!is.null(collector_name))
                  { list(type = type,
                         name = collector_name,
                         recipients = recipients)
                  } else { list(type = type,
                                recipients = recipients) },
                  email_message = if(is.null(email_body)) {
                  list(subject = email_subject, reply_email = email_replyto)
                  } else { list(reply_email = email_replyto,
                                            subject = email_subject,
                                            body_text = email_body #parameter corrected)})
    }
    b <- toJSON(b, auto_unbox = TRUE)
    h <- add_headers(Authorization=token, 'Content-Type'='application/json')
    out <- POST(u, config = h, body = b)
    stop_for_status(out)
    content <- content(out, as='parsed')
    if(content$status==3){
        warning("An error occurred: ",content$errmsg)
        return(content)
    } else {
        content <- content$data
        class(content$collector) <- 'sm_collector'
        class(content$survey) <- 'sm_survey'
        return(content)
    }
}
