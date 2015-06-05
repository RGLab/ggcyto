# 2d scatter plot of cytometry data with ggplot2
Mike Jiang  
04/13/2015  



`ggcyto` makes `ggplot2` to be able to work with `Cytometry` data, namely `flowSet/ncdfFlowSet` or `flowFrame` S4 objects.



```r
library(ggcyto)
data(GvHD)
fs <- GvHD[subset(pData(GvHD), Patient %in%5:7 & Visit %in% c(5:6))[["name"]]]
fr <- fs[[1]]
```


```r
# 2d hex
p <- ggplot(fr, aes(x = `FSC-H`, y =  `SSC-H`))
p + stat_binhex()
```

![](ggplot.flowSet.2d_files/figure-html/unnamed-chunk-3-1.png) 

```r
# change the smooth color 
myColor <- rev(RColorBrewer::brewer.pal(11, "Spectral"))
myColor_scale_fill <- scale_fill_gradientn(colours = myColor)
p + myColor_scale_fill + stat_binhex()
```

![](ggplot.flowSet.2d_files/figure-html/unnamed-chunk-3-2.png) 

```r
#change the bin
p + myColor_scale_fill + stat_binhex(bin = 64)
```

![](ggplot.flowSet.2d_files/figure-html/unnamed-chunk-3-3.png) 

```r
# sqr trans binned color
myColor_scale_fill_sqrt <- scale_fill_gradientn(colours = myColor, trans = "sqrt")
p1 <- p + myColor_scale_fill_sqrt + stat_binhex(bin = 64)
p1
```

![](ggplot.flowSet.2d_files/figure-html/unnamed-chunk-3-4.png) 

```r
# add boundary limits
p1 <- p1 + ylim(c(10,9e2)) + xlim(c(10,9e2))   
p1
```

![](ggplot.flowSet.2d_files/figure-html/unnamed-chunk-3-5.png) 

```r
# add contour
p <- p + myColor_scale_fill
p + stat_binhex(bin = 64) + geom_density2d(colour = "black")
```

![](ggplot.flowSet.2d_files/figure-html/unnamed-chunk-3-6.png) 

```r
# non-binned dot plot
df <- fortify(fr)
cols <- densCols(df[,`FSC-H`], df[,`SSC-H`], colramp = flowViz::flowViz.par.get("argcolramp"))
p1 <- ggplot(df, aes(x = `FSC-H`, y =  `SSC-H`))
p1 + geom_point(color = cols) 
```

![](ggplot.flowSet.2d_files/figure-html/unnamed-chunk-3-7.png) 

```r
# multiple samples
p1 <- ggplot(mapping = aes(x = `FSC-H`, y =  `SSC-H`)) + myColor_scale_fill + facet_grid(Patient~Visit)
p1 + stat_binhex(data = fs, bin = 64)
```

![](ggplot.flowSet.2d_files/figure-html/unnamed-chunk-3-8.png) 

```r
# color scale is different when plotting single sample
p1 + stat_binhex(data = fs[6], bin = 64)
```

![](ggplot.flowSet.2d_files/figure-html/unnamed-chunk-3-9.png) 

```r
# display density instead
p1 + stat_binhex(data = fs[6], bin = 64, aes(fill = ..density..))
```

![](ggplot.flowSet.2d_files/figure-html/unnamed-chunk-3-10.png) 

```r
# the color scale difference is reduced
p1 + stat_binhex(data = fs, bin = 64, aes(fill = ..density..))
```

![](ggplot.flowSet.2d_files/figure-html/unnamed-chunk-3-11.png) 

