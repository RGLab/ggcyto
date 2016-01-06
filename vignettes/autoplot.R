## ---- echo=FALSE---------------------------------------------------------
library(knitr)
opts_chunk$set(message = FALSE, warning = FALSE)

## ------------------------------------------------------------------------
library(ggcyto)
dataDir <- system.file("extdata",package="flowWorkspaceData")
gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
data(GvHD)
fs <- GvHD[subset(pData(GvHD), Patient %in%5 & Visit %in% c(5:6))[["name"]]]

## ------------------------------------------------------------------------
## 1d densityplot
autoplot(fs, x = 'FSC-H')

## 2d hex
autoplot(fs, x = 'FSC-H', y = 'SSC-H', bins = 64)

