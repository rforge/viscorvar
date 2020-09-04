---
output: pdf_document
---
visCorVar
========

The R package visCorVar enables to visualize results from data integration with the function block.spslda from the mixOmics package. The data integration is performed for different types of omics data (transcriptomics, metabolomics, transcriptomics) in order to select high correlated variables of different blocks and the predict the class membership of a new sample. These highly correlated variables can be visualized with correlation circles and networks.

How to install visCorVar?
------------------------

For Ubuntu distribution, the package libgl1-mesa-glx has to be installed :
`sudo apt-get install libgl1-mesa-glx`.

### Within R

To install the visCorVar package from gitlab, open a R session and :
- Install the package mixOmics 
```r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("mixOmics")
```
- Install the package igraph : `install.packages("igraph")`.
- Install the package remotes : `install.packages("remotes")`
- Install the package visCorVar : 
```r
library(remotes)
remotes::install_gitlab("bilille/viscorvar")
```

### Using conda


How to use visCorVar?
--------------------

# Data integration

Before using the package visCorVar, a data integration has to be performed with the function block.splsda (mixOmics package). The data integration enables to retrieve variables of a block (i.e. selected variables) which are highly correlated with the variables of other blocks and the response variables.

```r 
res_data_integration = block.splsda(X = list_X,
                                    Y = factor_Y,
                                    ncomp = ncomp,
                                    keepX = keepX,
                                    design = design,
                                    scheme = scheme,
                                    mode = mode,
                                    max.iter = max_iter)
```

# Preprocessing

The package visCorVar enables to visualize highly correlated variables thanks to :
- correlation circles
- networks

Before plotting correlation circles, the blocks whose correlation circles can be overlaid have to be determined. Correlations required to plot the correlation circles and to create the network are computed.

```r
load(system.file("extdata", "var_interest.rda", package="visCorVar"))
load(system.file("extdata", "block_Y.rda", package="visCorVar"))
comp = 1:2
cutoff_comp = 0.8
res_matCorAddVar = matCorAddVar(res_block_splsda = res_data_integration,
                                block_Y = block_Y,
                                cutoff_comp = cutoff_comp,
                                var_interest = var_interest,
                                comp = comp)
```

# Correlation circles

If it is possible to overlay the correlation circles, the function circleCor performs the overlaying of blocks and a zoom
in a rectangle of correlation circle. Only the selected block variables whose correlation with comp[1] component or comp[2]
component is greater than cutoff in absolute value are plotted in the correlation circle. The function circleCor returns a subset of relevant correlated variables contained in the rectangle.

```r
library(RColorBrewer)
list_cor_comp_selected_var_resp_var = res_matCorAddVar$list_cor_comp_selected_var_resp_var
list_vec_index_block_select = res_matCorAddVar$list_vec_index_block_select
mat_cor_comp1 = res_matCorAddVar$mat_cor_comp1
mat_cor_comp2 = res_matCorAddVar$mat_cor_comp2
names_blocks = c("X1", "X3")
names_response_variables = c("A", "B")
comp = 1:2
vec_col = colorRampPalette(brewer.pal(9, "Spectral"))(dim(mat_cor_comp1)[1] + 1)

names_block_variables =   circleCor(list_dataframe_cor_comp_var_global =   list_cor_comp_selected_var_resp_var,    
                                list_vec_index_block_select = list_vec_index_block_select,
                                mat_cor_comp1 = mat_cor_comp1,
                                mat_cor_comp2 = mat_cor_comp2,
                                names_blocks = names_blocks,
                                names_response_variables = names_response_variables,
                                comp = comp,
                                cutoff = 0.85,
                                min.X = -1,
                                max.X = 1,
                                min.Y = -1,
                                max.Y = 1,
                                vec_col = vec_col,
                                rad.in = 0.5, 
                                cex = 0.7,
                                cex_legend = 0.8,
                                pos = c(1.2, 0),
                                pch = 20)
```

The description of the input parameters is provided in the help of the function circleCor.

# Network

A network can be created with the function networkVar and can be exported in the graphml format for visualization with
Cytoscape. Before creating the network with the function networkVar, similarity 
matrices (the similarity is an approximation of the correlation) have to be computed with the function computeMatSimilarity.

```r  
res_compute_mat_similarity = computeMatSimilarity(res_matCorAddVar = res_matCorAddVar)
```

The description of the input parameters is provided in the help of the function computeMatSimilarity.

The nodes of this network are the response variables, the variables of interest (optional) and the selected block variables. The similarity between two variables is associated with a link between this two variables. This network gives a insight of correlated variables.

```r
names_resp_var2 = c("A")
res_networkVar = networkVar(res_compute_mat_similarity = res_compute_mat_similarity,
                            names_block_variables = names_block_variables,
                            names_response_variables = names_resp_var2,
                            cutoff = 0)
```

The description of the input parameters is provided in the help of the function networkVar.