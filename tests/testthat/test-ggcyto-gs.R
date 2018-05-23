context("ggcyto-GatingSet")
set.seed(1)#due to subsampling
gs1 <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
test_that("gs", {


  p <- ggcyto(gs1, aes(x = CD4, y = CD8), subset = "CD3+")
  # 2d plot
  p <- p + geom_hex(bins = 64)
  expect_doppelganger("ggcyto-gs-2d", p)

  expect_doppelganger("ggcyto-gs-instrument-range", p + ggcyto_par_set(limits = "instrument"))

  myPars <- ggcyto_par_set(limits = list(x = c(0,3.5e3), y = c(-10, 4.1e3)))
  p <- p + myPars# or xlim(0,3.5e3) + ylim(-10, 4e3)
  expect_doppelganger("ggcyto-gs-custom-range", p)

  expect_is(ggcyto_par_default(), "ggcyto_par")

  expect_doppelganger("ggcyto-gs-1-gate", p + geom_gate("CD4"))


  p <- p + geom_gate(c("CD4","CD8")) # short for geom_gate("CD8") + geom_gate("CD4")
  expect_doppelganger("ggcyto-gs-2-gate", p)

  expect_doppelganger("ggcyto-gs-stats-all", p + geom_stats() + labs_cyto("marker"))

  expect_doppelganger("ggcyto-gs-stats", p + geom_stats("CD4"))


  expect_doppelganger("ggcyto-gs-stats-custom", p + geom_stats("CD4", type = "count", size = 6,  color = "white", fill = "black", adjust = 0.3))


  p <- ggcyto(gs1, aes(x = CD4, y = CD8)) + geom_hex() + myPars
  expect_error(plot(p), "'subset' must be instantiated")

  ## ------------------------------------------------------------------------
  p <- p + geom_gate(c("CD4", "CD8"))


  expect_doppelganger("ggcyto-gs-overlay-2d", p + geom_overlay("CD8/CCR7- 45RA+", col = "black", size = 0.1, alpha = 0.4))

  p <- ggcyto(gs1, aes(x = CD4), subset = "CD3+") + geom_density(aes(y = ..count..))
  expect_doppelganger("ggcyto-gs-overlay-1d", p + geom_overlay("CD8/CCR7- 45RA+", aes(y = ..count..), fill = "red"))

  ## ------------------------------------------------------------------------
  p <- ggcyto(gs1, aes(x = 38, y = DR), subset = "CD4") + geom_hex(bins = 64) + geom_gate() + geom_stats()
  expect_doppelganger("ggcyto-gs-all-children", p)

  expect_doppelganger("ggcyto-gs-root", ggcyto(gs1, subset = "root", aes(x = CD4, y = CD8)) + geom_hex(bins = 64) + geom_gate("CD4") + myPars)


  expect_doppelganger("ggcyto-gs-axis_x_inverse_trans", p + axis_x_inverse_trans() + axis_y_inverse_trans())


})