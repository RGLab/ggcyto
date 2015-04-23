# ggCyto

### This package overloads `foritfy` S3 methods so that `ggplot` is fully aware of `Cytometry` data structures (e.g `flowSet`).


```r
library(ggCyto)
data(GvHD)
# select samples from 2 patients * 3 visits
fs <- GvHD[subset(pData(GvHD), Patient %in%5:7 & Visit %in% c(5:6))[["name"]]]
```

### `histogram` is default for one-dimensional `autoplot`
```r
# histogram for raw flow data
autoplot(fs, aes(x = `FL1-H`))

# add transformation
autoplot(fs, aes(x = `FL1-H`)) + scale_x_log10()

# disable marginal events filtering
autoplot(fs, aes(x = `FL1-H`), margin = F) + scale_x_log10()
```

### Display 1d `density` by specifying `plotType` argument
```r
autoplot(fs, aes(x = `FL1-H`), plotType = "density") + scale_x_log10()
```

### Use `ggplot` directly for more customizations
```r
# customize border colors 
ggplot(fs, aes(x = `FL1-H`)) + facet_wrap(~name) + geom_histogram(colour = "white") + scale_x_log10()

# change the bin width
ggplot(fs, aes(x = `FL1-H`)) + facet_wrap(~name, scale = "free") + geom_histogram(colour = "white", binwidth = 1/10) + scale_x_log10()
```

### Two-dimensional scatter/dot plot

```r
# default geom_hex plot
autoplot(fs, aes(x = `FSC-H`, y =  `SSC-H`)) 

# add contour
autoplot(fs, aes(x = `FSC-H`, y =  `SSC-H`)) + geom_density2d(colour = "black")
```

Change the faceting (default is faceting by 'sampleName')
```r
autoplot(fs, aes(x = `FSC-H`, y =  `SSC-H`)) + facet_grid(Patient~Visit) 
```

