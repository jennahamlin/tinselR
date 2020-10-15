context("testing distance matrix")

set.seed(12345)
testing_data <- matrix(c(1, "-", 0.2, 0.5, 0.1, 0, "-", 0.3, 0.9, 0.8, 0.2, 0.2, "-", 0.5, "-", 0.7), nrow = 4, dimnames = list(c("A","B","C","D"), c("A","B","C","D")))
testing_data <- as.data.frame(testing_data)
testing_data <- droplevels(testing_data)
testing_data <- data.frame(lapply(testing_data, as.character), stringsAsFactors=FALSE)
testing_data <- tibble::rownames_to_column(testing_data, var=".")  
testing_data <- replaceHwithZeros(testing_data)

test_that("confirm genetic distance swithces - or 0", {
  expect_silent(replaceHwithZeros(testing_data))
})

test_that('no missing values', {
  expect_identical(testing_data, na.omit(testing_data))
})

test_that('data types correct', {
  expect_is(testing_data,'data.frame')
  expect_is(testing_data$A, 'character')
})

