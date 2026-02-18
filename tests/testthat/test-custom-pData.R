context("fortify")
test_that("fortify -- ggcyto-fs-custom-order", {
  # Create a subset of the data
  fs <- GvHD[subset(pData(GvHD), Patient %in% 5:7 & Visit %in% c(5:6))[["name"]]]
  
  # Create custom pData with Patient as factor in custom order
  custom_pd <- pData(fs)
  custom_pd$Patient <- factor(custom_pd$Patient, levels = c("7", "6", "5"))
  custom_pd$Visit <- factor(custom_pd$Visit, levels = c("6", "5"))
  
  # Original plot order
  p1 <- ggcyto(fs, aes(x = `FSC-H`)) +
    geom_histogram(bins = 30) +
    facet_wrap(~Patient + Visit)
  
  # Manual plot order
  p2 <- ggcyto(fs, aes(x = `FSC-H`), pData = custom_pd) +
    geom_histogram(bins = 30) +
    facet_wrap(~Patient + Visit)
  
  suppressWarnings(
    expect_doppelganger(
      "ggcyto-fs-default-order", 
      p1
    )
  )
  suppressWarnings(
    expect_doppelganger(
      "ggcyto-fs-custom-order", 
      p1
    )
  )

})

test_that("fortify-- pData-rownames", {
  fs <- GvHD[1:3]
  
  # Create pData with wrong rownames
  wrong_pd <- data.frame(
    name = c("a", "b", "c"),
    Patient = c("1", "2", "3"),
    row.names = c("wrong1", "wrong2", "wrong3")
  )
  
  # Should error because rownames don't match
  expect_error(
    ggcyto(fs, aes(x = `FSC-H`), pData = wrong_pd),
    "rownames must match"
  )
})

test_that("fortify-- pData-columns", {
  fs <- GvHD[1:3]
  
  # Create pData with only some columns
  partial_pd <- data.frame(
    name = sampleNames(fs),
    row.names = sampleNames(fs)
  )
  
  # Should warn about missing columns
  expect_warning(
    ggcyto(fs, aes(x = `FSC-H`), pData = partial_pd),
    "missing in supplied pData"
  )
})

test_that("fortify-- pData-sample-order", {
  fs <- GvHD[1:3]
  
  # Create pData with samples in different order
  custom_pd <- pData(fs)
  # Reverse the order of rows
  custom_pd <- custom_pd[rev(rownames(custom_pd)), , drop = FALSE]
  custom_pd$Test <- factor(rownames(custom_pd), levels = rownames(custom_pd))
  
  # Should reorder to match flowSet
  p <- ggcyto(fs, aes(x = `FSC-H`), pData = custom_pd)
  
  # The pData in the flowSet should match the original order
  expect_equal(rownames(pData(p$data)), sampleNames(fs))
})