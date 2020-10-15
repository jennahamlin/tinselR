context("testing distance matrix")

set.seed(12345)
#honestly this seems a bit bananas to me that I need to do lines 5-14de to get a matrix that matches
testing_genetic <- matrix(c(1, "-", 0.2, 0.5, 0.1, 0, "-", 0.3, 0.9, 0.8, 0.2, 0.2, "-", 0.5, "-", 0.7), nrow = 4, dimnames = list(c("A","B","C","D"), c("A","B","C","D")))
testing_genetic <- as.data.frame(testing_genetic)
testing_genetic <- droplevels(testing_genetic)
testing_genetic <- data.frame(lapply(testing_genetic, as.character), stringsAsFactors=FALSE)
testing_genetic <- tibble::rownames_to_column(testing_genetic, var=".")  
#rename this column to make a matrix with both 
testing_genetic[1,1] <- "A"
testing_genetic[2,1] <- "B"
testing_genetic[3,1] <- "C"
testing_genetic[4,1] <- "D"
testing_genetic <- replaceHwithZeros(testing_genetic)

#print(testing_data, row.names = F) confirms that there are no row names

test_that('no missing values', {
  expect_identical(testing_genetic, na.omit(testing_genetic))
})

test_that('data types correct', {
  expect_is(testing_genetic,'data.frame')
  expect_is(testing_genetic$A, 'character')
})

test_that("confirm genetic distance swithces - or 0", {
  expect_silent(replaceHwithZeros(testing_genetic))
})

test_that("make distance matrix into a 3 column file", {
  expect_silent(geneObjectOut(testing_genetic))
})

context("connecting genetic distance with tree data")

#make random tree with 5 tips to match the 5 labes in genetic distancing up above
testing_tree <- ape::rtree(5)
testing_tree <- tibble::as_tibble(testing_tree)

test_that('data types correct before combining tree', {
  expect_is(testing_tree,'data.frame')
})

test_that("confirm tree and genetic distance can be combined", {
  expect_silent(combineGandT(testing_tree, testing_genetic))
})

test_that('data types correct after combing tree', {
  expect_is( combineGandT(testing_tree, testing_genetic),'treedata')
})

 context("testing meta data")
# 
# 
# 
# 
# mFileConversion <- function(impMeta, metSep){
#   mFile <- fileCheck(fileUp = impMeta, fileType = metSep, fileSep = metSep)
#   
#   meta2 <-mFile %>%
#     tibble::column_to_rownames(var = "Display.labels")%>% #convert the column Display labels to the row name
#     dplyr::select(-Tip.labels) #do not include the column of 'ugly' tip labels
# }
# 
