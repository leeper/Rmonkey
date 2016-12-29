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
options(sm_client_id = 'YourMasheryDeveloperUsername')
options(sm_secret = 'YourAPISecret')

# Get a long lasting oauth token.  This function completes the OAuth handshake
# and saves a long lasting token on the computer.  It needs to be done only once
smlogin()

# Lookup userdetails to test API
userdetails()

# Return a list of surveys
sl <- surveylist()

# Display the list of surveys
# (This shows each survey using the print.sm_survey function which overrides the standard
# print function)
sl

# Get and display more details for the first survey on the list
# (This uses the same print.sm_survey function but has more data to display)
sd1 <- surveydetails(sl[[1]])
sd1

# Get and display survey deatils including the details of the survey questions
sd1.q <- surveydetails(sl[[1]], question_details = TRUE)
str(sd1.q)

# Show just the questions for a survey
sl1.q <- surveyquestions(sl[[1]])
sl1.q

# Open browser to a web preview of the survey
surveypreview(sl[[1]])

# Show the list of response ids to a survey
sl1.r <- getresponses(sl[[1]])
sl1.r$data

# Show the expanded list of responses including answers to all questions
sl1.rd <- getresponses(sl[[1]], bulk = TRUE)


