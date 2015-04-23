# ggCyto


The package makes `ggplot` to be aware of `Cytometry` data structures by overloading `foritfy` and `autoplot` S3 methods for `flowSet/ncdfFlowSet` and `flowFrame` class.


```r
library(ggCyto)
data(GvHD)
fs <- GvHD[subset(pData(GvHD), Patient %in%5:7 & Visit %in% c(5:6))[["name"]]]
```

# 1d histogram/densityplot

```r
# histogram for raw flow data
autoplot(fs, aes(x = `FL1-H`))
```

```r
# add transformation
autoplot(fs, aes(x = `FL1-H`)) + scale_x_log10()
```

```r
# disable marginal events filtering
autoplot(fs, aes(x = `FL1-H`), margin = F) + scale_x_log10()
```

```r
# density
autoplot(fs, aes(x = `FL1-H`), plotType = "density") + scale_x_log10()
```

```r
# customize border colors 
ggplot(fs, aes(x = `FL1-H`)) + facet_wrap(~name) + geom_histogram(colour = "white") + scale_x_log10()
```

```r
# change the bin width
ggplot(fs, aes(x = `FL1-H`)) + facet_wrap(~name, scale = "free") + geom_histogram(colour = "white", binwidth = 1/10) + scale_x_log10()
```

# 2d scatter/dot plot

```r
# default geom_hex plot
autoplot(fs, aes(x = `FSC-H`, y =  `SSC-H`)) 
```

```r
# add contour
autoplot(fs, aes(x = `FSC-H`, y =  `SSC-H`)) + geom_density2d(colour = "black")
```

```r
# change the faceting
autoplot(fs, aes(x = `FSC-H`, y =  `SSC-H`)) + facet_grid(Patient~Visit) 
```

