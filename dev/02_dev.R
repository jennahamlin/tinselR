# Building a Prod-Ready, Robust Shiny Application.
#
# Each step is optional.
#

# 2. All along your project

## 2.1 Add modules
##
golem::add_module(name = "about")
golem::add_module(name = "aboutExample")

#upload data - tree, genetic, meta
golem::add_module(name = "tipCheck")
golem::add_module(name = "uploadData")
golem::add_module(name = "exampleData")

#upload data - tree, genetic, meta functions
golem::add_fct("read_data", module = "uploadData")
golem::add_fct("file_type", module = "uploadData")
golem::add_fct("file_check", module = "uploadData")
golem::add_fct("replace_column_header", module = "uploadData")
golem::add_fct("gene_object_out", module = "uploadData")


#upload data - tree, genetic, meta functions
golem::add_fct("sanity", module = "tipCheck")
golem::add_fct("m_file_conversion", module = "tipCheck")
golem::add_fct("not_columns", module = "tipCheck")

#tree parameters and display
golem::add_module(name = "pushButtons")
golem::add_module(name = "paramsTree")
golem::add_module(name = "displayTree")
golem::add_module(name = "exampleDisplay")

#tree parameters and display functions
golem::add_fct("combine_g_and_t", module = "displayTree")

#annotation module
golem::add_module(name = "cladeAnnotator")
golem::add_module(name = "addMatrix")

#annotation module functions
golem::add_fct("create_tip_list", module = "cladeAnnotator")
golem::add_fct("add_annotations", module = "cladeAnnotator")
golem::add_fct("snp_anno", module = "cladeAnnotator")
golem::add_fct("make_layer", module = "cladeAnnotator")
golem::add_fct("add_map", module = "cladeAnnotator")

#download image
golem::add_module(name = "downloadImage")

#relaunch the application
golem::add_module(name = "relaunchApp")

## 2.2 Add dependencies
usethis::use_package("ape")
usethis::use_package("config")
usethis::use_package("dplyr") #for manipulating input files
usethis::use_package("ggplot2")
usethis::use_package("ggtree") #for tree viz
usethis::use_package("golem")
usethis::use_package("magrittr") #for %>% essentially a pipe
usethis::use_package("phytools") #for the find MRCA function
usethis::use_package("phylotools")
usethis::use_package("shinythemes")
usethis::use_package("readr") # for reading in files
usethis::use_package("shiny")
usethis::use_package("shinyjs")
usethis::use_package("tibble")
usethis::use_package("tidyr")
usethis::use_package("tidytree")
usethis::use_package("treeio")

usethis::use_pipe()

usethis::use_package("assertthat", type = "Suggests")
usethis::use_package("attempt", type = "Suggests")
usethis::use_package("colorspace", type = "Suggests")
usethis::use_package("DT", type = "Suggests")
usethis::use_package("glue", type = "Suggests")
usethis::use_package("gtools", type = "Suggests")
usethis::use_package("here", type = "Suggests")
usethis::use_package("htmltools", type = "Suggests")
usethis::use_package("knitr", type = "Suggests")
usethis::use_package("markdown", type = "Suggests") #for the documentation page
usethis::use_package("pkgload", type = "Suggests")
usethis::use_package("processx", type = "Suggests")
usethis::use_package("purrr", type = "Suggests")
usethis::use_package("rmarkdown", type = "Suggests")
usethis::use_package("testthat", type = "Suggests", min_version = "2.1.0")
usethis::use_package("utils", type = "Suggests")
usethis::use_package("xfun", type = "Suggests")

## 2.3 Add tests

usethis::use_test("appConnection")

## 2.4 Add a browser button

golem::browser_button()

## 2.5 Add external files

golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("bootstrap.css")

golem::use_external_css_file(url = "where", name = "filename")


# 3. Documentation

## 3.1 Vignette
usethis::use_vignette("tinselR")
devtools::build_vignettes()

## 3.2 Code coverage
## You'll need GitHub there
usethis::use_github()

usethis::use_travis()

usethis::use_appveyor()

# You're now set!
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
