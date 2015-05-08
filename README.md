# ggcyto : Visualize `Cytometry` data with `ggplot`

```r
library(ggcyto)
data(GvHD)
# select samples from 2 patients * 3 visits
fs <- GvHD[subset(pData(GvHD), Patient %in%5:7 & Visit %in% c(5:6))[["name"]]]
```

### Overloaded `fortify` S3 method makes `ggplot` to be fully aware of `Cytometry` data
```r
df <- fortify(fs)
```

### One-dimensional `autoplot`
```r
# default is `geom_histogram`
autoplot(fs, aes(x = `FL1-H`))

# add transformation
autoplot(fs, aes(x = `FL1-H`)) + scale_x_log10()

# disable marginal events filtering
autoplot(fs, aes(x = `FL1-H`), margin = F) + scale_x_log10()
```

### Switch to `densityplot`
```r
autoplot(fs, aes(x = `FL1-H`), plotType = "density") + scale_x_log10()
```

### For more customizations: do `ggplot` directly
```r
# customize border colors 
ggplot(fs, aes(x = `FL1-H`)) + facet_wrap(~name) + geom_histogram(colour = "white") + scale_x_log10()

# change the bin width
ggplot(fs, aes(x = `FL1-H`)) + facet_wrap(~name, scale = "free") + geom_histogram(colour = "white", binwidth = 1/10) + scale_x_log10()
```

### Two-dimensional plot

```r
# default geom_hex plot
autoplot(fs, aes(x = `FSC-H`, y =  `SSC-H`)) 

# add contour
autoplot(fs, aes(x = `FSC-H`, y =  `SSC-H`)) + geom_density2d(colour = "black")
```

### Change faceting (default is `facet_wrap(~name)`)
```r
autoplot(fs, aes(x = `FSC-H`, y =  `SSC-H`)) + facet_grid(Patient~Visit) 
```

