# Visualize GatingSet with ggcyto






By specifying the dimensions through `aes` and selecting the cell population through `subset`, `ggcyto` can easily visualize the gated data stored in `GatingSet`.

```r
p <- ggcyto(gs, aes(x = CD4, y = CD8), subset = "CD3+") 
# 2d plot 
p <- p + geom_hex(bins = 64)
p
```

![](ggcyto.GatingSet_files/figure-html/unnamed-chunk-3-1.png) 

We can use the instrument range to automatically filter out these outlier cell events

```r
p + ggcyto_par_set(limits = "instrument")
```

![](ggcyto.GatingSet_files/figure-html/unnamed-chunk-4-1.png) 

Or by setting limits manually

```r
myPars <- ggcyto_par_set(limits = list(x = c(0,3.5e3), y = c(-10, 4.1e3)))
p <- p + myPars# or xlim(0,3.5e3) + ylim(-10, 4e3) 
p
```

![](ggcyto.GatingSet_files/figure-html/unnamed-chunk-5-1.png) 

To check what kind of visualization parameters can be changed through `ggcyto_par_set`, simply print the default settings

```r
ggcyto_par_default()
```

```
## $limits
## [1] "data"
## 
## $facet
## facet_wrap(name) 
## 
## $hex_fill
## <ScaleContinuous>
##  Range:  
##  Limits:    0 --    1
## 
## $lab
## $labels
## [1] "both"
## 
## attr(,"class")
## [1] "labs_cyto"
## 
## attr(,"class")
## [1] "ggcyto_par"
```

To plot a gate, simply pass the gate name to the `geom_gate` layer

```r
p + geom_gate("CD4")
```

![](ggcyto.GatingSet_files/figure-html/unnamed-chunk-7-1.png) 


More than one gate can be added as long as they share the same parent and dimensions

```r
p <- p + geom_gate(c("CD4","CD8")) # short for geom_gate("CD8") + geom_gate("CD4")
p
```

![](ggcyto.GatingSet_files/figure-html/unnamed-chunk-8-1.png) 

By default, stats for all gate layers are added through empty `geom_stats` layer. 

```r
p + geom_stats() + labs_cyto("marker")
```

![](ggcyto.GatingSet_files/figure-html/unnamed-chunk-9-1.png) 

Note that we choose to only display marker on axis through `labs_cyto` layer here.

To add stats just for one specific gate, we can pass the gate name to `geom_gate`

```r
p + geom_stats("CD4")
```

![](ggcyto.GatingSet_files/figure-html/unnamed-chunk-10-1.png) 

stats type, background color and position are all adjustable.

```r
p + geom_stats("CD4", type = "count", size = 6,  color = "white", fill = "black", adjust = 0.3)
```

![](ggcyto.GatingSet_files/figure-html/unnamed-chunk-11-1.png) 

When 'subset' is not specified, it is at abstract status thus can not be visualized 

```r
p <- ggcyto(gs, aes(x = CD4, y = CD8)) + geom_hex() + myPars
p
```

```
## Error in fortify_fs.GatingSet(model, ...): 'subset' must be instantiated by the actual node name!
## Make sure either 'subset' is specified or the 'geom_gate' layer is added.
```

unless it is instantiated by the gate layer, i.e. lookup the gating tree for the parent node based on the given gates in `geom_gate`

```r
p + geom_gate(c("CD4", "CD8"))
```

![](ggcyto.GatingSet_files/figure-html/unnamed-chunk-13-1.png) 


Alternatively, we can choose to plot all children of one specified parent and projections

```r
p <- ggcyto(gs, aes(x = 38, y = DR), subset = "CD4") + geom_hex(bins = 64) + geom_gate() + geom_stats()
p
```

![](ggcyto.GatingSet_files/figure-html/unnamed-chunk-14-1.png) 


Or we can add gate layer to any arbitary node instead of its parent node

```r
ggcyto(gs, subset = "root", aes(x = CD4, y = CD8)) + geom_hex(bins = 64) + geom_gate("CD4") + myPars
```

![](ggcyto.GatingSet_files/figure-html/unnamed-chunk-15-1.png) 


Sometime it is helpful to display the axis label in raw scale by inverse transforming the axis without affecting the data

```r
p + axis_x_inverse_trans() + axis_y_inverse_trans()
```

![](ggcyto.GatingSet_files/figure-html/unnamed-chunk-16-1.png) 

```r
#add filter (consistent with `margin` behavior in flowViz)
# ggcyto(gs, aes(x = CD4, y = CD8), subset = "3+", filter = marginalFilter)  + geom_hex(bins = 32, na.rm = T)
```

