---
title: "ggcyto: Next generation visualization software for flow cytometry. Supplementary Information."
author: "Phu T. Van, Mike Jiang, Greg Finak & Raphael Gottardo"
date: "March 28, 2018"
output: 
  html_document: 
    keep_md: yes
---



# Overview

This Supplementary Information document for the paper "ggcyto: Next generation visualization software for flow cytometry" shows how to use the BioConductor package `ggcyto` to visualize flow cytometry data and reproduces the plots from the original publication, together with code. 

# ggcyto supported data structures

`ggcyto` supports the core flow cytometry data structures in R/Bioconductor: `flowFrame` and `flowSet` (defined in the `flowCore` package) for ungated data, and `GatingSet` and `GatingHierarchy` (defined in the `flowWorkspace` package) for gated data. 

This document use the "graft vs. host disease" (`GvHD`) data set from the `flowCore` package  [on BioConductor](https://doi.org/doi:10.18129/B9.bioc.flowCore)), and part of the FlowCAP Lyoplate data that is in the `flowWorkspaceData` package, also [on BioConductor](https://doi.org/doi:10.18129/B9.bioc.flowWorkspaceData)).


# Reproducing figures from the original manuscript.

The original manuscript shows three sets of plots created with `ggcyto`. We load the required data and use the code verbatim from the published figure to generate the associated plots.


```r
data(GvHD)
dataDir <- system.file("extdata",package="flowWorkspaceData")
gs <- load_gs(list.files(dataDir, pattern = "gs_bcell_auto",full = TRUE))
# Below we associate a transformation with the Lyoplate data GatingSet, which did not have one.
gs@transformation <- transformerList(colnames(gs)[-(1:2)], flowJo_biexp_trans())

# Choose a subset of samples.
fs <- GvHD[subset(pData(GvHD), Patient %in%5:7 & Visit %in% c(5:6))[["name"]]]
 
# Give meaningful visit and subject identifiers to the samples.
pd <- pData(fs)
pd$Visit <- paste("visit",pd$Visit)
pd$Patient <- paste("patient",pd$Patient)
pData(fs) <- pd
```

## Figure 1 plots.

### Autoplot API


```r
autoplot(gs[1], c("CD3","CD19"), bins = 64) +
  geom_overlay("IgD+CD27+",size=0.25)
```

![](ggcyto_paper_figures_files/figure-html/figure1_1-1.png)<!-- -->

### ggcyto API.


```r
ggcyto(gs[1:2], aes(x="IgD",y="CD27"),subset= c("CD19andCD20")) + 
  geom_hex(bins=128) +
  geom_gate(c("IgD+CD27+","IgD+CD27-","IgD-CD27+","IgD-CD27-")) + 
  geom_stats(type = "count")
```

![](ggcyto_paper_figures_files/figure-html/figure1_2-1.png)<!-- -->

### transforming data.


```r
#Use the first GvHD sample
fr <- GvHD[[1]]
p = autoplot(fr, "FL1-H") + theme_cowplot(font_size = 18)
print(p)
```

![](ggcyto_paper_figures_files/figure-html/figure1_3-1.png)<!-- -->

```r
#flowCore logicle scale
p + scale_x_logicle() 
```

![](ggcyto_paper_figures_files/figure-html/figure1_3-2.png)<!-- -->

```r
# flowJo fasinh
p + scale_x_flowJo_fasinh() 
```

![](ggcyto_paper_figures_files/figure-html/figure1_3-3.png)<!-- -->

# Additional ggcyto features.

The ggcyto package is built on `ggplot2` and uses the "grammar of graphics". Below we demonstrate how its new features can be used to easily generate high quality plots of cytometry data.

## Context-aware plots.

Following common `ggplot2` usage, `ggcyto` defines an `autoplot()` method. 
Calling `autoplot()` on a supported data structure automatically results in a default
plot that is sensible for the data being displayed. 

For example, supplying a `flowSet` and a channel name result in a 1D density plot using `geom_density`. If more than one sample are passed in, a faceted plot of all samples is created. The `facet_grid` and `facet_wrap` geoms can be used on variables in the `pData` slot of the `flowFrame` or `flowSet` object.
Below we generate a faceted 1D density grid of the forward scatter channel from the first four samples of the GvHD data.


```r
autoplot(fs[1:4], "FSC-H") + facet_grid(Patient~Visit)
```

![](ggcyto_paper_figures_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

### Changing `geom`s.

The plot can be easily modified to show a histogram by appending the appropriate `geom`.


```r
autoplot(fs[1:4], "FSC-H") + facet_grid(Patient~Visit) + geom_histogram()
```

![](ggcyto_paper_figures_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

### Using gated data.

Calling `autoplot` on a `GatingSet` or `GatingHierarchy` and supplying a **population name** results in a 1D or 2D density (depending on how the population is defined) using `geom_hex` or `geom_density`. Any gates in the GatingSet defined in those two dimensions are automatically overlaid on the plot:


```r
autoplot(gs[[1]], c("CD3", "CD19"), bins = 64)
```

![](ggcyto_paper_figures_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

### Visualizing back-gating.

Different populations can be overlaid on the same plot using `geom_overlay`, a ggcyto-specific `geom`. This is analogous to viewing back-gated cell populations. Below we highlight the IgD+CD127+ cells on the CD19 and CD3 projection of a sample from the Lyoplate B cell panel.


```r
autoplot(gs[1], c("CD3","CD19"), bins = 64) +
  geom_overlay("IgD+CD27+",size=0.5) 
```

```
## Warning: Removed 298 rows containing non-finite values (stat_binhex).
```

```
## Warning: Removed 1 rows containing missing values (geom_hex).
```

```
## Warning: Removed 35 rows containing missing values (geom_path).
```

```
## Warning: Removed 39 rows containing missing values (geom_path).
```

![](ggcyto_paper_figures_files/figure-html/backgating-1.png)<!-- -->

### Changing the scales.

The `x` and `y` axis scales are transformed above but show the raw data values. These can be changed to show the transformed data values on a linear scale using the `axis_inverse_trans` argument to `autoplot`.


```r
autoplot(gs[1], c("CD3","CD19"), bins = 64, axis_inverse_trans = FALSE) +
  geom_overlay("IgD+CD27+",size=0.5) 
```

![](ggcyto_paper_figures_files/figure-html/linear_scales-1.png)<!-- -->

### Using the `ggcyto` interface. 

The same plots created with `autoplot` can be generated using `ggcyto()`. Below, we show how to create theprevious plot using the `ggcyto` API.


```r
ggcyto(gs[1], aes(x="CD3",y="CD19"),subset= "Live") + 
  geom_hex(bins=64) +
  geom_gate(c("CD19","CD3")) + 
  geom_stats(type = "percent") + 
  axis_x_inverse_trans() +
  axis_y_inverse_trans()
```

![](ggcyto_paper_figures_files/figure-html/ggcyto-1.png)<!-- -->

## A use case with the Lyoplate data.

We load the lyoplate data for the T cell panel. the data are available on [ImmuneSpace](http://www.immunespace.org).


```r
# load the Lyoplate gatingSet
gs2 <- rbind2(load_gslist("/Volumes/gottardo//mike_working/lyoplate_out/gated_data/auto/gslist-tcell/"))

gs2@transformation = transformerList(colnames(gs2)[-(1:2)], flowJo_biexp_trans())
```

The Lyoplate dataset for the T cell panel contains three biological samples. The samples were distributed across seven centers, an each center ran three technical replicates of each sample, as indicated in the metadata. 


name                        Center     Sample   Replicate 
--------------------------  ---------  -------  ----------
12828_1_Tcell_A01.fcs       NHLBI      12828    1         
12828_2_Tcell_A02.fcs       NHLBI      12828    2         
12828_3_Tcell_A03.fcs       NHLBI      12828    3         
1349_1_Tcell_A04.fcs        NHLBI      1349     1         
1349_2_Tcell_A05.fcs        NHLBI      1349     2         
1349_3_Tcell_A06.fcs        NHLBI      1349     3         
1369_1_Tcell_A07.fcs        NHLBI      1369     1         
1369_2_Tcell_A08.fcs        NHLBI      1369     2         
1369_3_Tcell_A09.fcs        NHLBI      1369     3         
12828_1_A1_A01.fcs          Yale       12828    1         
12828_2_A2_A02.fcs          Yale       12828    2         
12828_3_A3_A03.fcs          Yale       12828    3         
1349_1_A4_A04.fcs           Yale       1349     1         
1349_2_A5_A05.fcs           Yale       1349     2         
1349_3_A6_A06.fcs           Yale       1349     3         
1369_1_A7_A07.fcs           Yale       1369     1         
1369_2_A8_A08.fcs           Yale       1369     2         
1369_3_A9_A09.fcs           Yale       1369     3         
TCELL 22013_12828_001.fcs   UCLA       12828    1         
TCELL 22013_12828_002.fcs   UCLA       12828    2         
TCELL 22013_12828_003.fcs   UCLA       12828    3         
TCELL 22013_1349_001.fcs    UCLA       1349     1         
TCELL 22013_1349_002.fcs    UCLA       1349     2         
TCELL 22013_1349_003.fcs    UCLA       1349     3         
TCELL 22013_1369_001.fcs    UCLA       1369     1         
TCELL 22013_1369_002.fcs    UCLA       1369     2         
TCELL 22013_1369_003.fcs    UCLA       1369     3         
T_CELL_12828_001_P1.fcs     CIMR       12828    2         
T_CELL_12828_002_P1.fcs     CIMR       12828    3         
T_CELL_12828_P1.fcs         CIMR       12828    1         
T_CELL_1349_001_P1.fcs      CIMR       1349     2         
T_CELL_1349_002_P1.fcs      CIMR       1349     3         
T_CELL_1349_P1.fcs          CIMR       1349     1         
T_CELL_1369_001_P1.fcs      CIMR       1369     2         
T_CELL_1369_002_P1.fcs      CIMR       1369     3         
T_CELL_1369_P1.fcs          CIMR       1369     1         
lot 12828_A1_A01.fcs        Miami      12828    1         
lot 12828_A2_A02.fcs        Miami      12828    2         
lot 12828_A3_A03.fcs        Miami      12828    3         
lot 1349_A4_A04.fcs         Miami      1349     1         
lot 1349_A5_A05.fcs         Miami      1349     2         
lot 1349_A6_A06.fcs         Miami      1349     3         
lot 1369_A7_A07.fcs         Miami      1369     1         
lot 1369_A8_A08.fcs         Miami      1369     2         
lot 1369_A9_A09.fcs         Miami      1369     3         
12828_1_T CELL.fcs          Baylor     12828    1         
12828_2_T CELL.fcs          Baylor     12828    2         
12828_3_T CELL.fcs          Baylor     12828    3         
1349_1_T CELL.fcs           Baylor     1349     1         
1349_2_T CELL.fcs           Baylor     1349     2         
1349_3_T CELL.fcs           Baylor     1349     3         
1369_1_T CELL.fcs           Baylor     1369     1         
1369_2_T CELL.fcs           Baylor     1369     2         
1369_3_T CELL.fcs           Baylor     1369     3         
1228-1_A1_A01.fcs           Stanford   12828    1         
1228-2_A2_A02.fcs           Stanford   12828    2         
1228-3_A3_A03.fcs           Stanford   12828    3         
1349-1_A4_A04.fcs           Stanford   1349     1         
1349-2_A5_A05.fcs           Stanford   1349     2         
1349-3_A6_A06.fcs           Stanford   1349     3         
1369-1_A7_A07.fcs           Stanford   1369     1         
1369-2_A8_A08.fcs           Stanford   1369     2         
1369-3_A9_A09.fcs           Stanford   1369     3         

The data was gated to identify CD4 and CD8 memory T cells:

![](ggcyto_paper_figures_files/figure-html/lyoplate_hier-1.png)<!-- -->

We can visualize variation across replicates and centers for sample #1349.
The plot is built up using the `ggcyto()` API. Note we use `axis_x_inverse_trans` and `axis_y_inverse_trans` to transform the axes tick marks to a non-linear scale and display the raw data values. 

The plot shows clearly that center-to-center variability is greater than variability across technical replicates.


```r
ggcyto(subset(gs2, Sample=="1349")
        ,aes(x = CCR7,y=CD45RA)
        ,subset="CD4") +
        geom_hex(bins = 64) +
        geom_gate("CD4/CCR7+CD45RA-") +
        geom_stats() +
        facet_grid(Replicate ~ Center) +
        labs(title="sample 1349 CD4 central memory T cells, by center and replicate") +
        ggcyto_par_set(limits = "instrument") + 
        axis_x_inverse_trans() + 
        axis_y_inverse_trans() 
```

![](ggcyto_paper_figures_files/figure-html/lyoplate_facet-1.png)<!-- -->
