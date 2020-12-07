
---
title: "tinselR"
authors:
- affiliation: '1'
  name: Jennafer A. P. Hamlin
  orcid: 0000-0001-7037-134X
- affiliation: '2'
  name: Teofil Nakov
  orcid: XXX
- affiliation: '3'
  name: Amanda Williams-Newkirk
  orcid: XXX
date: "20 November 2020"
bibliography: paper.bib
tags:
- R
- RShiny
- phylognetics
- epidemiology
- outbreak analysis
affiliations:
- index: '1'
  name: Respiratory Diseases Laboratory Branch, Centers for Disease Control and Prevention, Atlanta, GA, USA
- index: '2'
  name: University of Arkansas
- index: '3'
  name: Enteric Diseases Laboratory Branch, Centers for Disease Control and Prevention, Atlanta, GA, USA
bibliography: paper.bib
---

# Summary
Across the United States, public health laboratories perform whole-genome
sequencing for many pathogens, a milestone for protecting public health by
subtyping organisms at a higher resolution than was previously possible
[@armstrong2019pathogen]. These high resolution subtypes can be used to
determine the relationships between organisms, which can be visualized with
phylogenetic trees. Once the genetic relationships of pathogens are known,
empirically derived thresholds can be used to identify possible outbreaks, and
additional epidemiological data can be added to the visualization. In
combination, these data can be used to inform the design of investigations to
confirm the occurrance of an outbreak and identify potential transmission
routes. If appropriate, interventions such as the recall of contaminated
products and public announcements may be issued. The creation and markup of
phylogenetic trees is an essential component for this public health workflow.
Our goal was to develop as open-source graphical user interface for
phylogenetictree visualization and annotation usable by persons without
specialized bioinformatics or data visualization skills. Given that the R
programming language contains some of the gold standard packages for
phylogenetic analyses and visualization (e.g. ape[@paradis2004ape], and ggtree
[@yu2017ggtree]), we used the Rshiny framework[@chang2017shiny] to develop
**tinselR** (pronounced tinsel-er) to provide GUI access to the tools in ape,
ggtree, and other key packages. TinselR's minimum input requirement is a Newick
formatted phylogenetic tree. Once loaded, user-selected inputs change the
appearance of the displayed tree. For example, a user can quickly transform tip
label formating. By adding a genetic distance matrix and/or metadata file, the
user can include annotations on the image, relabel tips, or add a heatmap to the
phylogenetic tree. These modified tree images are downloadable in various
formats (pdf, png, or tiff) for presentations, publications, or other
communications with collaborators. Below we detail how to install the
application and describe the example data that is pre-loaded so that a new user
can familiarize themselves with the application.

# Installation

To install TinselR from GitHub, users will need to install the

R package devtools [@wickham2016devtools] from the Comprehensive R Archive
Network or other repositiory. The R packages ggtree [@yu2017ggtree] and treeio
[@wang2020treeio] are also required and can be installed from Bioconductor using
BiocManager [@morgan2019biocmanager]. Once these dependencies are installed,
tinselR can be installed via the install_github command from devtools. Explicit
installation commands are below, and the final command (run_app()) will launch
the application. Note that install_github will also install other missing R
dependencies. 

**1). Install devtools package**

Run the below code in your R console -    

`install.packages("devtools", dep=T)`

**2). Install ggtree and treeio**

```
if (!requireNamespace("BiocManager", quietly = TRUE))
install.packages("BiocManager")
BiocManager::install("ggtree")
```

**3). Install and launch the tinselR shiny application**

```
devtools::install_github("jennahamlin/tinselR")
library(tinselR)
run_app()
```

# Acceptable Inputs

TinselR will accept Newick tree files from any program, e.g. RAxML
[@stamatakis2014raxml], as input. The genetic distance matrix file must contain
a square matrix of single nucleotide polymorphism (SNP) differences between tips
on the tree. The metadata file is a table of additional information to be
displayed on the tree. The header of the first column must be Tip.labels and it
must contain the labels for all tree tips. The tip labels contained in the
Newick tree, distance matrix, and metadata files must match prior to upload or
tinselR will report an error. If the user wishes to relabel the tips on the
final tree image, alternative labels can be provided in the metadata file using
the column header Display.labels. If desired, users may include additional
columns in the metadata file such as collection site and display the information
in a heatmap next to the tree. Headers for these additional columns in the
metadata file are flexible because they are not automatically recoginized and
used by tinselR. CSV, TSV, and TXT formats are accepted for the genetic distance
and metadata files. File types can be set independently for each input.


# About Pre-loaded Example Data

When tinselR is launched, new users can explore the application using one ofthe
pre-loaded datasets located in the 'Example Data' tab (Figure 1). We
providethree datasets (i.e. Newick formatted tree, genetic distance matrix,
andmetadata file). These data are either *Eschericia coli* (from NCBI
Bioproject:PRJNA218110) or *Salmonella enterica* (from NCBI Bioproject:
PRJNA230403) with the number of isolates ranging from 14 - 19. After clicking on
the'Example Data' tab, users can select one of the datasets(e.g. example data 1,
example data 2, and example data 3 (Figure 2) from the drop down menu.We
highlight the capabilities of tinselR (Figure 3) using example data 1 below. 


# Figures

<p>
<img src = "image1.PNG" />
<h4> Figure 1: tinselR landing page with the example data tab indicated in the
blue box. </h4>
</p>

<p>
<img src = "image2.PNG" />
<h4> Figure 2: Example data tab with action buttons and the location of the
drop-down menu for the example data. Here example data set 1 is selected. </h4>

</p>

<p>
<img src = "image3.PNG" />
<h4> Figure 3: Example dataset 1 displayed with annotations and a heatmap
indicating collection source. </h4>
</p>

# Development for local-user experience 

Although it is possible to host ShinyR applications on a server, to date TinselR
has only been tested by single users running the application locally. We
recommend testing to ensure tinselR performs as expected under multiuser
conditions before providing access from a server for production purposes.


# Acknowledgements

This publication was supported by Cooperative Agreement Number U60OE000103,
funded by Centers for Disease Control and Prevention through the Association of
Public Health Laboratories. Its contents are solely the responsibility of the
authors and do not necessarily represent the official views of Centers for
Disease Control and Prevention or the Association of Public Health Laboratories.


# References
