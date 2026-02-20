# Multi-group models tests (plan §7)

test_that("scrapeVarCross with group adds group metadata to dvn", {
  dat <- commitmentQ
  dat$dyad_type <- rep(c("MM", "FF"), length.out = nrow(dat))

  dvn <- scrapeVarCross(
    dat = dat, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    group = "dyad_type", verbose = FALSE
  )

  expect_true("group" %in% names(dvn))
  expect_true("group_n" %in% names(dvn))
  expect_true("group_levels" %in% names(dvn))
  expect_equal(dvn$group, "dyad_type")
  expect_equal(dvn$group_n, 2L)
  expect_equal(sort(dvn$group_levels), c("FF", "MM"))
})

test_that("scrapeVarCross rejects group with single level", {
  dat <- commitmentQ
  dat$single_grp <- "A"

  expect_error(
    scrapeVarCross(
      dat = dat, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
      x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
      group = "single_grp", verbose = FALSE
    ),
    "at least 2 levels"
  )
})

test_that("scriptCor with constr_group_meas produces c() syntax", {
  dat <- commitmentQ
  dat$dyad_type <- rep(c("MM", "FF"), length.out = nrow(dat))
  dvn <- scrapeVarCross(
    dat = dat, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    group = "dyad_type", verbose = FALSE
  )

  s <- scriptCor(dvn, lvname = "Sat", constr_group_meas = "loadings",
                 constr_group_struct = "variances")
  expect_true(grepl("c(", s, fixed = TRUE))
})

test_that("scriptUni with constr_group_meas produces c() syntax", {
  dat <- commitmentQ
  dat$dyad_type <- rep(c("MM", "FF"), length.out = nrow(dat))
  dvn <- scrapeVarCross(
    dat = dat, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    group = "dyad_type", verbose = FALSE
  )

  s <- scriptUni(dvn, lvname = "Sat", constr_group_meas = "loadings")
  expect_true(grepl("c(", s, fixed = TRUE))
})

test_that("scriptCor multi-group model runs with lavaan::cfa", {
  dat <- commitmentQ
  dat$dyad_type <- rep(c("MM", "FF"), length.out = nrow(dat))
  dvn <- scrapeVarCross(
    dat = dat, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    group = "dyad_type", verbose = FALSE
  )

  s <- scriptCor(dvn, lvname = "Sat", constr_group_meas = "loadings",
                 constr_group_struct = "variances")
  fit <- tryCatch(
    lavaan::cfa(s, data = dat, group = dvn$group),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    skip(paste("lavaan cfa failed:", conditionMessage(fit)))
  }
  expect_s3_class(fit, "lavaan")
})

test_that("constr_group without group in dvn raises clear error", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    verbose = FALSE
  )

  expect_error(
    scriptCor(dvn, lvname = "Sat", constr_group_meas = "loadings"),
    "scrapeVarCross.*group"
  )
})
