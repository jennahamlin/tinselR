### Getting started with Tinsel

Tinsel at its' most basic level is a graphical viewer of newick phylogenetic trees and as a program for producing publication-ready figures.  
The **power** of Tinsel comes with combining a genetic distance matrix for annotating a tree for epidemological outbreak analyses.
A genetic distance matrix contains the number of snp differences for all pairwise comparisons. 

##### Please click on the 'Data Upload' pane, where you will be able to upload your files. 

* **Phylogenetic Tree** - required; a [newick](https://en.wikipedia.org/wiki/Newick_format) generated tree 
* **Genetic Distance data** - optional for use with the annotation function; a tsv/txt/csv file
* **Metadata** - optional for easy correction of tip labels; a tsv/txt/csv file 

<u>Once the phylogenetic tree is uploaded you can -</u>
* Alter additional visualization parameters in the sidebar panel on the left. 

<u>Once the genetic distance file is uploaded you can -</u>
* add annotation to the visual representation of the tree.

If you have any problems, please file an *issue* [here](https://github.com/jennahamlin/Tinsel/issues).

<hr>

**Known issues as June 8, 2020**

- Unable to remove the very first placed annotation. As is, a user can place annotations and then step them back one by one. However, when only two annotations are visible both will be removed. Then when the user 'visulizaes the tree' again and attempts to add annotations, the first placed annotation remains. 

- Need to develop a check for overlap annotations and when they are the same locaiton, move the newest out by X distance

- Unable to download file from CDC server

