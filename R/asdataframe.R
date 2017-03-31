as.data.frame.sm_response <- function(x, row.names, optional, details = NULL, stringsAsFactors = FALSE, ...){
    if (is.null(details) && !is.null(attr(x, 'survey_id'))) {
        details <- surveydetails(survey = attr(x, 'survey_id'))
    } else if (!is.null(details)) {
        if (inherits(details, 'sm_survey')) {
            details <- details
        } else if (is.character(details)) {
            details <- surveydetails(survey = details[1])
        } else {
            stop("'details' is not character or an 'sm_survey' object")
        }
    } else {
        stop("'details' is missing and cannot be determined automatically")
    }
   
  survey<-x
  # These first functions unpack the surveydetails() with questions
  details <- surveydetails(survey, question_details = TRUE)
  
  
    # extract all questions from the `question` element in all pages
  # note: this assumes that all data records are identical and so the first can be used as a model
    questions <- do.call('c', lapply(details$pages, function(i) i[['questions']]))

    # `type` contains info about each question type
    qtypes <- sapply(questions, function(i) {
        fam <- i$family
        if (fam == "matrix") {
            setNames(paste0(fam, "_", i$subtype), i$id)
        } else {
            setNames(fam, i$id)
        }
    })
    
    # set variable names
    varnames <- sapply(questions, function(i) {
        # `heading` is the display text
        setNames(i$heading, i$id)
    })
    
    # alternate method which reuses an existing function and cleans HTML tags
    varnames2 <- surveyquestions(survey)
    
    # extract all answers from the `answers` elements of each subelement of `question`
        # `answer_id` is what is recorded in `sm_response`
        # `text` is the display seen by respondents
        # `answers` is empty for "open_ended" type questions
    answerchoices <- sapply(questions, function(i) {
                        out <- list()
                        for (k in seq_along(i$answers$choices)) {
                            # if (i$family == "matrix") {
                            #     if (i$subtype == "rating") {
                            #         if (i$answers[[k]]$type == "other") {
                            #             out[[k]] <- setNames(i$answers[[k]]$text, i$answers[[k]]$id)
                            #         } else {
                            #     # exclude "col" values from matrix questions
                            #     if (i$answers[[k]]$type == "row") {
                            #         out[[k]] <- setNames(i$answers[[k]]$text, i$answers[[k]]$id)
                            #     }
                            # }
                            #         out[[k]] <- setNames(i$answers[[k]]$text, i$answers[[k]]$id)
                            #     } else if (i$type$subtype == "menu") {
                            #         if (i$answers[[k]]$type == "col") {
                            #             tmp_txt <- unlist(lapply(i$answers[[k]]$items, `[`, "text"))
                            #             tmp_ans <- unlist(lapply(i$answers[[k]]$items, `[`, "id"))
                            #             out[[k]] <- setNames(tmp_txt, tmp_ans)
                            #             rm(tmp_txt)
                            #             rm(tmp_ans)
                            #         }
                            #     }
                            # } else {
                                out[[k]] <- setNames(i$answers$choices[[k]]$text, i$answers$choices[[k]]$id)
                            # }
                        }
                        return(unlist(out))
                     })
    answerchoices <- unlist(do.call(c, answerchoices))
    
    # extract question_ids
    question_ids <- unlist(sapply(questions, `[`, 'id'))
    
    # # count number of answers per question
    # nanswers <- integer()
    # for (i in seq_along(x$questions)) {
    #     nanswers[i] <- length(x$questions[[i]]$answers)
    # }
    # rm(i)
    
    # count the number of answer choices per question
    nanswers <- sapply(questions, function(x) {length(x$answers$choices)})
    
    # create vector of answer names by repeating question names `nanswers` times each
    answer_names <- rep(question_ids, nanswers)
    
    # recode responses by looking up `question_id` in details and recoding answers
    responses <- character()
    for (i in seq_along(x$questions)) {
        if (qtypes[question_ids[i]] %in% c('single_choice','multiple_choice')) {
            tmp <- unname(answerchoices[unlist(x$questions[[i]]$answers)])
        } else if (qtypes[question_ids[i]] %in% c('matrix_rating')) {
            # this extracts `col` values as answer options
            tmp <- unname(answerchoices[unlist(lapply(x$questions[[i]]$answers, `[[`, "col"))])
        } else if (qtypes[question_ids[i]] %in% c('matrix_menu')) {
            tmp <- unname(answerchoices[unlist(lapply(x$questions[[i]]$answers, `[[`, "col_choice"))])
        } else if (qtypes[question_ids[i]] %in% c('open_ended')) {
            tmp <- unname(unlist(lapply(x$questions[[i]]$answers, `[`, "text")))
        } else {
            tmp <- unname(unlist(x$questions[[i]]$answers))
        }
        responses <- c(responses, tmp)
    }
    rm(tmp)
    responses <- setNames(responses, answer_names)
    
    unique_names <- unique(names(responses))
    for (i in seq_along(unique_names)) {
        cnt <- names(responses)[names(responses) %in% unique_names[i]]
        if (length(cnt) > 1) {
            names(responses)[names(responses) %in% unique_names[i]] <- 
                paste0(unique_names[i], ".", seq_along(cnt))
        }
    }
    out <- setNames(as.data.frame(matrix(unlist(responses), nrow=1), 
                                  stringsAsFactors = stringsAsFactors), names(responses))
    
    # rename columns to something
    for (i in seq_along(out)) {
        attr(out[,i], 'question') <- unname(varnames[answer_names[i]])
    }
    return(out)
}

as.data.frame.sm_response_list <- function(x, row.names, optional, details = NULL, stringsAsFactors = FALSE, ...){
    if (is.null(details) && !is.null(attr(x[[1]], 'survey_id'))) {
        details <- surveydetails(survey = attr(x[[1]], 'survey_id'))
    } else if (!is.null(details)) {
        if (inherits(details, 'sm_survey')) {
            details <- details
        } else if (is.character(details)) {
            details <- surveydetails(survey = details[1])
        } else {
            stop("'details' is not character or an 'sm_survey' object")
        }
    } else {
        stop("'details' is missing and cannot be determined automatically")
    }
    tmp <- lapply(x, as.data.frame, details = details, stringsAsFactors = stringsAsFactors, ...)
    out <- rbind.fill(tmp)
    return(out)
}

