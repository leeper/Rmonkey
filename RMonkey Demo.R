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

# Show a list of surveys
surveylist()

