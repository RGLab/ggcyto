context("ggcyto-flowSet")


set.seed(1)#due to subsampling
fr <- GvHD[[1]]

test_that("multiRangeGate", {
  
  gate = multiRangeGate(ranges = list(min=c(100, 350), max=c(250, 400)))
  
  ## ------------------------------------------------------------------------
  suppressWarnings(expect_doppelganger("ggcyto-fs-1d-multiRangeGate", autoplot(fr, "Time") + geom_gate(gate)))  
  suppressWarnings(expect_doppelganger("ggcyto-fs-2d-multiRangeGate", autoplot(fr, "Time",  "FSC-H") + geom_gate(gate)))  
  suppressWarnings(expect_doppelganger("ggcyto-fs-2d-multiRangeGate-flip", autoplot(fr, "FSC-H", "Time") + geom_gate(gate)))  
  

})