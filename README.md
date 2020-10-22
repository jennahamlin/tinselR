### What is Tinsel?

Tinsel at its' most basic level is a graphical viewer of newick-formatted 
phylogenetic trees and as an application for producing publication-ready 
figures. The **power** of Tinsel comes with combining a genetic distance matrix 
for annotating a tree for epidemiological outbreak analyses. A genetic distance
matrix contains snp differences for all pairwise comparisons for the tips on 
the tree. One can also include a 
[heatmap](https://yulab-smu.top/treedata-book/chapter7.html) when that data is
provided in the meta data file.

### Issues, problems, suggestions, thoughts

If you have any the above, please submit an *issue* on github located 
[here](https://github.com/jennahamlin/Tinsel/issues).

### Requires - 
 - [ggtree](https://bioconductor.org/packages/release/bioc/html/ggtree.html); 
 see the quick start for how to install ggtree. 
 - [treeio](http://bioconductor.org/packages/release/bioc/html/treeio.html); 
 which will install with ggtree.


<!-- badges: start -->
[![Travis build status](https://travis-ci.com/jennahamlin/Tinsel.svg?branch=master)](https://travis-ci.com/jennahamlin/Tinsel)
<!-- badges: end -->
<hr>

### Quick Start 

**1). Install devtools package** 

`install.packages("devtools", dep=T)`

**2). Install ggtree and treeio**
```
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("ggtree")
```

**3). Install and launch the Tinsel shiny application**

Run this code in your R console -     

```
devtools::install_github("jennahamlin/Tinsel")
Library(Tinsel)
run_app()
```

**4). Load your data or use the example data**  

*Please click on the 'Data Upload' pane, where you will be able to upload your
files or access example data on the 'Example Data' pane.* 

* **Phylogenetic Tree** - required; a [newick](https://en.wikipedia.org/wiki/Newick_format) generated tree 
* **Genetic Distance data** - optional for use with the annotation function; a tsv/txt/csv file - see below for image of genetic distance matrix, requires column headers
* **Metadata** - optional for easy correction of tip labels; a tsv/txt/csv file - requires column headers of Display.labels and Tip.labels. See image below for a csv file example 

<p>

<h4> genetic Distance screenshot </h4>
    <img src="man/figures/geneticDistanceScreenshot.png" />
</p>



<p>

<h4> meta Data screenshot </h4>
    <img src="man/figures/metaDataScreenshot.png" />
</p>

#### Once the phylogenetic tree is uploaded you can -
* Alter additional visualization parameters in the sidebar panel on the left. See below for tree with aligned tips.  

<p>
    <img src="man/figures/treeWSomeViz.png" />
</p>

#### Once the genetic distance file is uploaded you can -
* add annotation to the visual representation of the tree. See below for a tree with annotated clades including the range of SNPs. 

<p>
    <img src="man/figures/treeWSomeAnn.png" />
</p>

#### Tinsel Flow Control diagram 

<p>
    <img src="man/figures/tinselFlowControl.png" />
</p>

**Known issues as September 23, 2020**

- If user uploads tree and genetic distance matrix and then annotates the tree, error pops up if user then tries to upload a meta data file. 


**Tinsel has been tested with**
* R version 3.6.0 


