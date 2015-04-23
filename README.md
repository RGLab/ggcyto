---
title: "Visualize cytometry data with ggplot2"
author: "Mike Jiang"
date: "04/13/2015"
output: html_document
---



## `ggCyto` makes `ggplot2` to be able to work with `Cytometry` data, namely `flowSet/ncdfFlowSet` or `flowFrame` S4 objects.



```r
library(ggCyto)
data(GvHD)
fs <- GvHD[subset(pData(GvHD), Patient %in%5:7 & Visit %in% c(5:6))[["name"]]]
```

## 1d histogram/densityplot

```r
# histogram for raw flow data
autoplot(fs, aes(x = `FL1-H`))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
# add transformation
autoplot(fs, aes(x = `FL1-H`)) + scale_x_log10()
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-2.png) 

```r
# disable marginal events filtering
autoplot(fs, aes(x = `FL1-H`), margin = F) + scale_x_log10()
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-3.png) 

```r
# density
autoplot(fs, aes(x = `FL1-H`), plotType = "density") + scale_x_log10()
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-4.png) 

```r
# customize border colors 
ggplot(fs, aes(x = `FL1-H`)) + facet_wrap(~name) + geom_histogram(colour = "white") + scale_x_log10()
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-5.png) 

```r
# change the bin width
ggplot(fs, aes(x = `FL1-H`)) + facet_wrap(~name, scale = "free") + geom_histogram(colour = "white", binwidth = 1/10) + scale_x_log10()
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-6.png) 

## 2d scatter/dot plot

```r
# default geom_hex plot
autoplot(fs, aes(x = `FSC-H`, y =  `SSC-H`)) 
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
# add contour
autoplot(fs, aes(x = `FSC-H`, y =  `SSC-H`)) + geom_density2d(colour = "black")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-2.png) 

```r
# change the faceting
autoplot(fs, aes(x = `FSC-H`, y =  `SSC-H`)) + facet_grid(Patient~Visit) 
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-3.png) 

