context("ggcyto-flowSet")


set.seed(1)#due to subsampling
fr <- GvHD[[1]]

test_that("multiRangeGate", {
  
  gate = multiRangeGate(ranges = list(min=c(100, 350), max=c(250, 400)))
  
  ## ------------------------------------------------------------------------
  p <- autoplot(fr, "Time") + geom_gate(gate)
p =as.ggplot(autoplot(fr, "Time"))
 p$data
 dt = fortify(gate)
 dt[["Time"]] = 0
 dt
 p+ geom_rect(data=dt,xmin=dt[,xmin],xmax=dt[,xmax], ymin=0, ymax=Inf, alpha=0.5,color="red")
  
  
  ## ------------------------------------------------------------------------
  suppressWarnings(expect_doppelganger("ggcyto-fs-2d-hex-rectgate-stats", p + geom_gate(rect.gates) + geom_stats(size = 3)))  
  
  ## ------------------------------------------------------------------------
  den.gates.x <- fsApply(fs, openCyto::gate_mindensity, channel = "FSC-H", gate_range = c(100, 300), adjust = 1)
  suppressWarnings(expect_doppelganger("ggcyto-fs-2d-hex-1dgate", p + geom_gate(den.gates.x) + geom_stats()))  
  
  ## ------------------------------------------------------------------------
  den.gates.y <- fsApply(fs, openCyto::gate_mindensity, channel = "SSC-H", gate_range = c(100, 500), adjust = 1, positive = FALSE)
  
  suppressWarnings(expect_doppelganger("ggcyto-fs-2d-hex-1dgate-static-stats", p + geom_gate(den.gates.y) + geom_stats(value = lapply(rect.gates, function(g)0.1))))  
  
  ## ------------------------------------------------------------------------
  suppressWarnings(expect_doppelganger("ggcyto-fs-1d-den-stats", ggcyto(fs, aes(x = `FSC-H`)) + geom_density(fill = "black", aes(y = ..scaled..)) + geom_gate(den.gates.x)  + geom_stats(type = "count")))  
  
  ## ------------------------------------------------------------------------
  suppressWarnings(expect_doppelganger("ggcyto-fs-2d-multi-gates", p + geom_gate(lg) + geom_gate(rect.gates) + geom_stats(size = 3)))  
  
  
  
  

})