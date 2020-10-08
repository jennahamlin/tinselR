# Building a Prod-Ready, Robust Shiny Application.
# 
# Each step is optional. 
# 

# 2. All along your project

## 2.1 Add modules
## 
golem::add_module(name="about")

#upload data - tree, genetic, meta
golem::add_module(name="tipCheck")
golem::add_module(name ="htmlMessages")
golem::add_module(name="uploadData")
golem::add_module(name="exampleData")

#tree parameters and display
golem::add_module(name = "paramsTree")
golem::add_module(name = "DisplayTree")
golem::add_module(name = "exampleDisplay")

#annotation module
golem::add_module(name = "cladeAnnotator")
golem::add_module(name = "pushButtons")

#donwload image
golem::add_module(name="downloadImage") 

#relaunch the application
golem::add_module(name="relaunchApp")


## 2.2 Add dependencies
usethis::use_package("config")
usethis::use_package("utils")
usethis::use_package("markdown") # for the documentation page
usethis::use_package( "dplyr" ) # for manipulating input files 
usethis::use_package("assertthat")
usethis::use_package("phytools") # for the find MRCA function
usethis::use_package("magrittr") # for %>% essentially a pipe
usethis::use_package("ggtree") # for tree viz
usethis::use_package("readr") # for reading in files
usethis::use_package("tidyr")
usethis::use_package("phylotools")
usethis::use_package("tibble")
usethis::use_package("shinythemes")
usethis::use_package("colorspace")
usethis::use_package("phangorn")
usethis::use_package("glue")
usethis::use_package("gtools")
usethis::use_package("ape")
usethis::use_package("ggplot2")
usethis::use_package("treeio")
usethis::use_package("stats")
usethis::use_package("zip")
usethis::use_package("shinyjs")
usethis::use_package("here")
usethis::use_package("DescTools")
usethis::use_pipe() 



## 2.3 Add tests

usethis::use_test( "appConnection" )

## 2.4 Add a browser button

golem::browser_button()

## 2.5 Add external files

golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "bootstrap.css" )

golem::use_external_css_file(url = "where", name = "filename")


# 3. Documentation

## 3.1 Vignette
usethis::use_vignette("Tinsel")
devtools::build_vignettes()

## 3.2 Code coverage
## You'll need GitHub there
usethis::use_github()

usethis::use_travis()

usethis::use_appveyor()

# You're now set! 
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
