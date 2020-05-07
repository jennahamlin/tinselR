# Building a Prod-Ready, Robust Shiny Application.
# 
# Each step is optional. 
# 

# 2. All along your project

## 2.1 Add modules
## 
#trees
golem::add_module(name="uploadTree")
golem::add_module(name = "paramsTree")
golem::add_module(name = "DisplayTree")
golem::add_module(name = "cladeAnnotator")

#meta and genetic data
golem::add_module(name = "dataInput") # Name of the module
golem::add_module(name="displayTable")

#donwload image
golem::add_module(name="downloadImage")


golem::add_module(name="combineTandG")


## 2.2 Add dependencies

usethis::use_package("utils")
usethis::use_package("markdown") # for the documentation page
usethis::use_package( "dplyr" ) # for manipulating input files 
usethis::use_package("assertthat")
usethis::use_package("phytools") # for the find MRCA function
usethis::use_package("magrittr") # for %>% essentially a pipe
usethis::use_package("ggtree") # for tree viz
usethis::use_package("readr") # for reading in files
usethis::use_package("shinyjs") 

## 2.3 Add tests

usethis::use_test( "appConnection" )

## 2.4 Add a browser button

golem::browser_button()

## 2.5 Add external files

golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

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
