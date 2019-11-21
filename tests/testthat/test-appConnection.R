context("basic")

library(RSelenium)
library(testthat)

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444,
  browserName = "chrome"
  
)

remDr$open(silent = TRUE)
appURL <- "http://127.0.0.1:6756"

test_that("can connect to app", {  
  remDr$navigate(appURL)
  appTitle <- remDr$getTitle()[[1]]
  expect_equal(appTitle, "Tinsel")  
})

remDr$close()