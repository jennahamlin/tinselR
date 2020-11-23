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

Across the United States, public health laboratories at the state-level now
perform whole-genome sequencing for foodborne pathogens, a milestone for
protecting our food sources by identifying bacterial contamination at a higher
resolution than previously. To understand organismal relationships in response
to outbreak investigations, building and interpreting phylogenetic trees is an
essential component for this public health workflow. Placing new outbreak
isolates within the broader context of within species diversity allows for
identification across temporal or spatial scales regarding similarities or
differences for those newly sequenced isolates. Thus, our goal was to develop an
open-source graphical user interface for phylogenetic tree visualization and
annotation primarily for external (state public health laboratories) partners
but also internal users. Given that many state public health laboratories have
limited bioinformatics support, one of our main focal points is that an
application be both free and easily installable. To accomplish this, we used
the Rshiny framework to develop **tinselR** (pronounced tinsel-er) as a R
package. Beyond the above requirements listed, the R programming language
also contains some of the gold standard packages for phylogenetic analyses and
visualization. tinselR's minimum input requirements are a Newick formatted
phylogenetic tree, in which once loaded, user-selected inputs change parameters
of the visualized tree. For example, a user can quickly transform tip labels
to either bold or bold and italic font format. With a genetic distance matrix or
the metadata file, or both, the user can include annotations on the image,
automatically adjust tip labels, or add a heatmap to the phylogenetic tree.
These modified tree images are downloadable in various formats for presentations
or publications. These images help public health officials and stakeholders
communicate regarding isolates in response to outbreaks. Below, we detail how to
install the application and describe the example data that is pre-loaded so that
a new user can familiarize themselves with the application.

# Installation

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

Any program that generates a phylogenetic tree in the Newick format can be used.
For example, *RAxML* [@stamatakis2014raxml] outputs in Newick format and thus
are acceptable inputs of a user uploaded phylogenetic tree. Currently, our
application only supports a genetic distance matrix of SNP  differences between 
tips on the tree. Your distance matrix can be in either csv, tsv, or txt
formats. The metadata file should be in one of these formats as well (csv, tsv,
or txt). However, the metadata file requires specific column headers
(Tip.labels, and Display.labels), but the third column, if you are interested in
plotting a heatmapcan be titled whatever is informative for you. 

# About Pre-loaded Example Data

When tinselR is launched, the user can test out the application by using one of
the pre-loaded datasets located in the 'Example Data' tab (Figure 1). We provide
three datasets (i.e. Newick formatted tree, genetic distance matrix, and
metadata file) already combined. These data are either *Eschericia coli* 
(NCBI Bioproject: PRJNA218110) or *Salmonella enterica* (NCBI Bioproject:
PRJNA230403) with the number of isolates ranging from 14 - 19 depending on the 
dataset.After clicking on the 'Example Data' tab, the user selects from one drop
down menu for one of the combined datasets (e.g. example data 1, example data 2,
and example data 3 (Figure 2). Using example data set 1, we highlight the
capabilities of tinselR (Figure 3). 

# Figures

<p>
<img src = "image1.PNG" />
<h4> Figure 1: tinselR landing page with the example data tab indicated in the blue box. </h4>
</p>

<p>
<img src = "image2.PNG" />
<h4> Figure 2: Example data tab with action buttons and the location of the drop-down menu for the example data. Here example data set 1 is selected. </h4>
</p>

<p>
<img src = "image3.PNG" />
<h4> Figure 3: Display of example dataset 1 with annotations and a heatmap. </h4>
</p>

# Development for local-user experience 

One note regarding tinselR is to keep in mind that the application has only been tested by single-user instances. If one hosts the application on a personal server, then testing if multiple users can access the application should be done before release.

# Acknowledgements

This publication was supported by Cooperative Agreement Number U60OE000103, funded by Centers for Disease Control and Prevention through the Association of Public Health Laboratories. Its contents are solely the responsibility of the authors and do not necessarily represent the official views of Centers for Disease Control and Prevention or the Association of Public Health Laboratories.

# References
