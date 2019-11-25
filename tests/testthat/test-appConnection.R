context("basic app connection")

library(RSelenium)
library(testthat)

# speicify the port based on the command run for docker `sudo docker run -d -p 4445:4444 selenium/standalone-firefox`
#remDr <- remoteDriver(
#  remoteServerAddr = "localhost",
#  port = 4445L,
#  browserName = "firefox")

#open the connect. 
#remDr$open()

#remDr$getStatus()

#appURL <- "http://127.0.0.1:4445"

#test_that("can connect to app", {  
#  remDr$navigate(appURL)
#  appTitle <- remDr$getTitle()[[1]]
#  expect_equal(appTitle, "Tinsel")  
#})


#remDr$close()



















