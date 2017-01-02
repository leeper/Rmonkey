# RMonkey library demo
#
# Sean Fahey
# 2016-12-28
#
# This program shows how the RMonkey library can be used to access SurveyMonkey data
# via API V3.
#

# load needed libraries
library(curl)
library(httr)
library(jsonlite)
library(dplyr)

# Load the latest Rmonkey library from github
if(!require("devtools")) {
  install.packages("devtools")
  library("devtools")
}
install_github("seanofahey/Rmonkey")
library("Rmonkey")

# Create a SurveyMonkey App to enable the API
# 1) go to https://developer.surveymonkey.com/apps/ to create an app
# 2) set the OAuth redirect URL as http://localhost:1410
# 3) set the scope permissions (I used all the view ones but no create ones)
# 4) note the following values from the App screen: clientID, Secret


# Enter your app API info into R
options(sm_client_id = 'YourClientID')
options(sm_secret = 'YourAPISecret')

# Get a long lasting oauth token.  This function completes the OAuth handshake
# and saves a long lasting token on the computer.  It needs to be done only once
smlogin()

### USER FUNCTIONS

# Lookup userdetails to test API
userdetails()

### SURVEY FUNCTIONS

# Get and display a list of surveys
sl <- surveylist()
# print the sm_survey object using the print.sm_survey function
sl
# show the structure of the sm_survey object
str(sl[[1]])

# Return a specific list of surveys
sl <- surveylist(per_page = 100, sort_by = 'num_responses', sort_order = 'desc')
sl

# Return surveys that have been modified since a certain date
sl <- surveylist(start_modified_at = '2016-12-25')
sl


# Get and display survey deatils without the details of the survey questions
s1.d <- surveydetails(sl[[1]], question_details = FALSE)
# (This uses the same print.sm_survey function but has more data to display)
s1.d
# show the expanded details for the survey 
str(s1.d)


# Get and display more details for the first survey on the list
s1.dq <- surveydetails(sl[[1]])
# show the survey summary
s1.dq
# show the expanded details for the survey with all the question data
str(s1.dq)


# Show just the questions for a survey
sl1.q <- surveyquestions(sl[[1]])
sl1.q

# Open browser to a web preview of the survey
surveypreview(sl[[1]])

# Get a dataframe with details on each question in the survey
s1_df <- surveyquestiondf(sl[[1]])
str(s1_df)

### SURVEY RESPONSE FUNCTIONS

# Show the list of response ids to a survey
s1.r <- getresponses(sl[[1]])
s1.r

# Show the expanded list of responses including answers to all questions
s1.rd <- getresponses(sl[[1]], bulk = TRUE)

# Generate a data frame with response data
s1.r_df <- as.data.frame.surveyresponses(sl[[1]])
str(s1.r_df)

# Join response data with question data to decode responses 
s1.r_decode <- left_join (s1.r_df, s1_df)