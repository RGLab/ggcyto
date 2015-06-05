# ggcyto : Visualize `Cytometry` data with `ggplot`
Mike Jiang  
06/05/2015  




```r
library(ggcyto)
dataDir <- system.file("extdata",package="flowWorkspaceData")
gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
```

### Overloaded `fortify` S3 method makes `Cytometry` data to be fully compatible with `ggplot`. 


```r
#GatingSet
class(gs)
```

```
## [1] "GatingSet"
## attr(,"package")
## [1] "flowWorkspace"
```

```r
#show gating tree
getNodes(gs, path = "auto")[1:6]
```

```
## [1] "root"        "not debris"  "singlets"    "CD3+"        "CD4"        
## [6] "CD4/38- DR+"
```

```r
#tell gs which sub pupolation to fortify
attr(gs, "subset") <- "CD8"
fortify(gs)[1:3, c(2:3,6:9), with = FALSE]
```

```
##                       name     FSC-A    SSC-A  <B710-A> <R660-A> <R780-A>
## 1: CytoTrol_CytoTrol_1.fcs 127717.88 76954.91 1287.7864 1928.069 3771.027
## 2: CytoTrol_CytoTrol_1.fcs  84330.34 66052.55 1456.5524 1515.816 3703.416
## 3: CytoTrol_CytoTrol_1.fcs 113783.64 62380.92  702.0849 1983.690 3516.511
```

```r
#flowSet
fs <- getData(gs, "CD3+")
fortify(fs)[1:3, c(2:3,6:9), with = FALSE]
```

```
##                       name    FSC-A    SSC-A <B710-A> <R660-A> <R780-A>
## 1: CytoTrol_CytoTrol_1.fcs 140733.0 91113.96 3103.893 3302.285 2073.787
## 2: CytoTrol_CytoTrol_1.fcs 127717.9 76954.91 1287.786 1928.069 3771.027
## 3: CytoTrol_CytoTrol_1.fcs 134347.0 70116.48 3126.778 1829.362 1607.414
```

### `ggplot` + `flowSet`

```r
# 1d
p <- ggplot(fs, aes(x = `<B710-A>`)) + facet_wrap(~name) 
#histogram plot
p + geom_histogram(colour = "white")
```

![](README_files/figure-html/unnamed-chunk-4-1.png) 

```r
#density plot
p + geom_density(fill = "black")
```

![](README_files/figure-html/unnamed-chunk-4-2.png) 

```r
# 2d hexbin
ggplot(fs, aes(x = `<B710-A>`, y = `<R780-A>`)) + facet_wrap(~name) + geom_hex(bins = 64)
```

![](README_files/figure-html/unnamed-chunk-4-3.png) 

More examples of using `ggplot` directly on `flowSet`:

* [ggplot + flowSet1d](vignettes/ggplot.flowSet.1d.md)
* [ggplot + flowSet2d](vignettes/ggplot.flowSet.2d.md)
* [ggplot + flowSet + gate](vignettes/ggplot.flowSet.gate.md)
* [ggplot + flowSet + overlay](vignettes/ggplot.flowSet.overlay.md)

## Using **ggcyto** convenient wrapper

### plot `flowSet` with `ggcyto`

```r
# support fuzzy-matching of aes to the data
# with flowJo-type of default color fills
# facet on `name` by default
ggcyto(fs,aes(x = CD4, y = CD8)) + geom_hex(bins = 64) + xlim(0, 3600)
```

![](README_files/figure-html/unnamed-chunk-5-1.png) 

More examples :

* [ggcyto + flowSet](vignettes/ggcyto.flowSet.md)


### plot `GatingSet` with `ggcyto`

```r
ggcyto(gs,aes(x = CCR7, y = CD45RA), subset = "CD4") + geom_hex(bins = 64) + geom_gate("CD4/CCR7+ 45RA+") + geom_stats(fill = "yellow", size = 4)
```

![](README_files/figure-html/unnamed-chunk-6-1.png) 

More examples :

* [ggcyto + GatingSet](vignettes/ggcyto.GatingSet.md)

