context("reading in data")

#list files in the folder in which you are working 

files<-list.files(".", pattern='\\.dnd$')

#does file with .dnd exist -this is what I need to do

test_that("Does a file with the extension .dnd (newick file) exist",{
  expect_that(assertthat::assert_that(assertthat::see_if(assertthat::not_empty(files))), is_a("logical")) })

