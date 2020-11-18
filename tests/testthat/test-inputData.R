##############################################################################
context("testing distance matrix")

set.seed(12345)
# honestly this seems a bit bananas to me that I need to do lines
# 7-20 to get a matrix that matches
testing_genetic <- matrix(c(1, "-", 0.2, 0.5, 0.1, 0, "-", 0.3, 0.9, 0.8, 0.2,
                            0.2, "-", 0.5, "-", 0.7), nrow = 4,
                          dimnames = list(c("A", "B", "C", "D"),
                                          c("A", "B", "C", "D")))
testing_genetic <- as.data.frame(testing_genetic)
testing_genetic <- droplevels(testing_genetic)
testing_genetic <- data.frame(lapply(testing_genetic, as.character),
                              stringsAsFactors = FALSE)
testing_genetic <- tibble::rownames_to_column(testing_genetic, var = ".")

# rename this column to make
testing_genetic[1, 1] <- "A"
testing_genetic[2, 1] <- "B"
testing_genetic[3, 1] <- "C"
testing_genetic[4, 1] <- "D"

testing_genetic <- replace_column_header(testing_genetic)

test_that("no missing values", {
  expect_identical(testing_genetic, na.omit(testing_genetic))
})

test_that("data types correct", {
  expect_type(testing_genetic, "list")
  expect_type(testing_genetic$A, "character")
})

test_that("confirm genetic distance switches column 1 and row 1 header", {
  expect_silent(replace_column_header(testing_genetic))
})

test_that("make distance matrix into a 3 column file", {
  expect_silent(gene_object_out(testing_genetic))
})

data("gene3")
testing_gene3 <- gene3
testing_gene3 <- replace_column_header(testing_gene3)
testing_gene3 <- gene_object_out(testing_gene3)

test_that("Determine the number of columns before meta data conversion", {
  expect_equal(ncol(testing_gene3), 3)
})

##############################################################################
context("connecting genetic distance with tree data")

data(tree1)
data(gene1)

testing_tree <- tree1
testing_tree <- tidytree::as_tibble(testing_tree)

testing_gene <- replace_column_header(gene1)

test_that("data types correct before combining tree", {
  expect_type(testing_tree, "list")
})

test_that("confirm tree and genetic distance can be combined", {
  expect_silent(dplyr::full_join(testing_tree, testing_gene, by = "label"))
})

test_that("data types correct after combing tree", {
  expect_type(combine_g_and_t(testing_tree, testing_genetic), "list")
})

###############################################################################
context("testing meta data")

# building example meta data file
testing_meta <- data.frame("Tip.labels" = c("Label_1_Ugly", "Label_2_Ugly",
                                            "Label_3_Ugly", "Label_4_Ugly"),
                           "Display,labels" = c("Label_1", "Label_2",
                                                "Label_3", "Label_4"),
                           "Source" = c("Tree", "Tree", "Stream", "Flower"))


test_that("Converts meta data correctly for matrix visualization", {
  expect_equal(ncol(m_file_conversion(testing_meta)), 1)
})

###############################################################################
context("tip check functions")

data(tree3)
data(gene3)
data(meta3)

t_3 <- tree3
g_3 <- gene3
m_3 <- meta3

test_that("Check to see if tip check gives an error message; this should not", {
  expect_silent(sanity(m_file = m_3, g_file = g_3, t_file = t_3))
})

data(gene1)
g_1 <- gene1
test_that("Message to check number of columns", {
  expect_length(ncol(g_1), 1)
  })

##############################################################################
context("connecting genetic distance with meta data")

data(meta2)
data(gene2)

m_2 <- meta2
g_2 <- gene2

g_2 <- g_2 %>% dplyr::rename(center = 1)

test_that("Length of genetic data 2 for number of unique isolates", {
  expect_length(g_2, 20)
})


colnames(g_2)[2:ncol(g_2)] <-
  m_2$Display.labels[which(m_2$Tip.labels %in%
                                        colnames(g_2)
                                      [2:ncol(g_2)])]

g_2$center <-
  m_2$Display.labels[which(m_2$Tip.labels
                                      %in% g_2$center)]

test_that("Confirm type of the combined data", {
  expect_type(g_2, "list")
})

##############################################################################
context("confirm median and range calcuated correctly")

data(gene1)
data(tree1)

testing_gene <- gene_object_out(replace_column_header(gene1))

tips_in <- c("PNUSAS000901.cleaned", "PNUSAS000876.cleaned",
             "2015K-1169-M3235-15-050.cleaned",
             "2015K-0966-M3235-15-044.cleaned")

snps <- snp_anno(gene_file = testing_gene, tips = tips_in)

min_snps <- min(snps)
max_snps <- max(snps)
median_snps <- median(snps)

test_that("Confirm median is calculated correctly", {
  expect_equal(median_snps, 4.5)
})

test_that("Confirm max of range is calculated correctly", {
  expect_equal(max_snps, 7)
})

test_that("Confirm length of min snps", {
  expect_length(min_snps, 1)
})

test_that("Confirm type for min snps", {
  expect_type(min_snps, "double")
})
