test_that("scriptPopAPIM returns syntax and simulateData runs", {
  skip_if_not_installed("lavaan")
  dvn <- build_dvn(2L, n_items_y = 2L, distinguish_1 = "A", distinguish_2 = "B")
  pv <- list(
    x_loadings_p1 = c(0.7, 0.65),
    x_residuals_p1 = c(0.5, 0.55),
    x_coresids = c(0, 0),
    y_loadings_p1 = c(0.72, 0.6),
    y_residuals_p1 = c(0.48, 0.6),
    y_coresids = c(0, 0),
    x_lv_cov = 0.2,
    actor = 0.3,
    partner = 0.15,
    y_lv_var = c(0.4, 0.4),
    y_lv_cov = 0.05
  )
  m <- scriptPopAPIM(dvn, "X", "Y", pv)
  expect_type(m, "character")
  expect_gt(nchar(m), 50L)
  dat <- lavaan::simulateData(m, sample.nobs = 30L, seed = 2L)
  expect_equal(nrow(dat), 30L)
  expect_equal(ncol(dat), 8L)
})

test_that("build_dvn with n_items_y adds y varnames", {
  d <- build_dvn(2L, n_items_y = 3L, y_stem = "y")
  expect_length(d$p1yvarnames, 3L)
  expect_equal(d$yindper, 3L)
  expect_equal(d$indnum, 2L * 2L + 2L * 3L)
})
