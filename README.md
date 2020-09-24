### What is Tinsel?

Tinsel at its' most basic level is a graphical viewer of newick-formatted phylogenetic trees and as an application for producing publication-ready figures. The **power** of Tinsel comes with combining a genetic distance matrix for annotating a tree for epidemiological outbreak analyses. A genetic distance matrix contains snp differences for all pairwise comparisons for the tips on the tree.  


### Requires - 
 - tibble_2.1.3; problems arise if you use tibble_3.0.3 `devtools::install_version("tibble", version = "2.1.3")`
 - stringi_1.4.3; problems arise if you use stringi_1.5.3 `devtools::install_version("stringi", version = "1.4.3")`
 - [ggtree](https://bioconductor.org/packages/release/bioc/html/ggtree.html) 
 - [treeio](http://bioconductor.org/packages/release/bioc/html/treeio.html); which should install if you install ggtree first

### Quick Start 

**1). Install devtools package** 

`install.packages("devtools", dep=T)`

**2). Launcning the Tinsel shiny application**

Run this code in your R console -     

```
devtools::install_github("jennhamlin/Tinsel")
Library(Tinsel)
run_app()
```  
**3). Load your data or use the example data**  

*Please click on the 'Data Upload' pane, where you will be able to upload your files or access example data on the 'Example Data' pane.* 

* **Phylogenetic Tree** - required; a [newick](https://en.wikipedia.org/wiki/Newick_format) generated tree 
* **Genetic Distance data** - optional for use with the annotation function; a tsv/txt/csv file
* **Metadata** - optional for easy correction of tip labels; a tsv/txt/csv file 

<u>Once the phylogenetic tree is uploaded you can -</u>
* Alter additional visualization parameters in the sidebar panel on the left. 

<p>
    <img src="man/figures/treeWSomeViz.png" />
</p>



<u>Once the genetic distance file is uploaded you can -</u>
* add annotation to the visual representation of the tree.

If you have any problems, please file an *issue* [here](https://github.com/jennahamlin/Tinsel/issues).

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/jennahamlin/Tinsel.svg?branch=master)](https://travis-ci.com/jennahamlin/Tinsel)
<!-- badges: end -->
<hr>


**Known issues as September 23, 2020**

- If users add one annotation and then remove that annotation then unable to download the image and this error is produced ' [[: subscript out of bounds'

- If tip labels are selected and do not overlap but overlap via mrca (node) then overlap in annotations occurs

- If user uploads tree and genetic distance matrix and then annotates the tree, error pops up if user then tries to upload a meta data file. 

**Tinsel has been tested with**
* R version 3.6.0 


