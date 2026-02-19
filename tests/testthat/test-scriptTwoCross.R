test_that("scriptTwoCross requires dvn with both X and Y", {
  dvn_x_only <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2", verbose = FALSE
  )
  expect_error(
    scriptTwoCross(dvn_x_only, lvxname = "Sat", lvyname = "Com"),
    "both X and Y"
  )
})

test_that("scriptTwoCross produces valid syntax for cor/uni", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    verbose = FALSE
  )
  script <- scriptTwoCross(dvn, lvxname = "Sat", lvyname = "Com",
    x_model = "cor", y_model = "uni")
  expect_type(script, "character")
  expect_gt(nchar(script), 500)
  expect_true(grepl("Sat1", script))
  expect_true(grepl("ComDy", script))
  expect_true(grepl("Latent Regressions", script))
})

test_that("scriptTwoCross produces valid syntax for all 16 model combinations", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    verbose = FALSE
  )
  models <- c("uni", "cor", "hier", "bifactor")
  for (xm in models) {
    for (ym in models) {
      script <- scriptTwoCross(dvn, lvxname = "Sat", lvyname = "Com",
        x_model = xm, y_model = ym)
      expect_type(script, "character")
      expect_true(nchar(script) > 100, label = paste("x_model =", xm, ", y_model =", ym))
    }
  }
})

test_that("scriptTwoCross constr_dy_xy_struct zero yields zero regression paths", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    verbose = FALSE
  )
  script <- scriptTwoCross(dvn, lvxname = "Sat", lvyname = "Com",
    x_model = "cor", y_model = "uni", constr_dy_xy_struct = "zero")
  expect_true(grepl("0\\*Sat1", script))
  expect_true(grepl("0\\*Sat2", script))
})

test_that("scriptTwoCross rejects invalid x_model and y_model", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    verbose = FALSE
  )
  expect_error(scriptTwoCross(dvn, x_model = "invalid", y_model = "cor"),
    "x_model must be")
  expect_error(scriptTwoCross(dvn, x_model = "cor", y_model = "invalid"),
    "y_model must be")
})

test_that("scriptTwoCross produces same df for cor/uni when x_scaleset and y_scaleset vary", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    verbose = FALSE
  )

  script_ff_ff <- scriptTwoCross(dvn, lvxname = "Sat", lvyname = "Com",
    x_model = "cor", y_model = "uni", x_scaleset = "FF", y_scaleset = "FF",
    constr_dy_x_meas = "none", constr_dy_x_struct = "none",
    constr_dy_y_meas = "none", constr_dy_y_struct = "none")
  mod_ff_ff <- lavaan::cfa(script_ff_ff, data = commitmentQ, parallel = "no")
  df_ff_ff <- as.double(lavaan::fitmeasures(mod_ff_ff, "df"))

  script_mv_mv <- scriptTwoCross(dvn, lvxname = "Sat", lvyname = "Com",
    x_model = "cor", y_model = "uni", x_scaleset = "MV", y_scaleset = "MV",
    constr_dy_x_meas = "none", constr_dy_x_struct = "none",
    constr_dy_y_meas = "none", constr_dy_y_struct = "none")
  mod_mv_mv <- lavaan::cfa(script_mv_mv, data = commitmentQ, parallel = "no")
  df_mv_mv <- as.double(lavaan::fitmeasures(mod_mv_mv, "df"))

  script_ff_mv <- scriptTwoCross(dvn, lvxname = "Sat", lvyname = "Com",
    x_model = "cor", y_model = "uni", x_scaleset = "FF", y_scaleset = "MV",
    constr_dy_x_meas = "none", constr_dy_x_struct = "none",
    constr_dy_y_meas = "none", constr_dy_y_struct = "none")
  mod_ff_mv <- lavaan::cfa(script_ff_mv, data = commitmentQ, parallel = "no")
  df_ff_mv <- as.double(lavaan::fitmeasures(mod_ff_mv, "df"))

  script_mv_ff <- scriptTwoCross(dvn, lvxname = "Sat", lvyname = "Com",
    x_model = "cor", y_model = "uni", x_scaleset = "MV", y_scaleset = "FF",
    constr_dy_x_meas = "none", constr_dy_x_struct = "none",
    constr_dy_y_meas = "none", constr_dy_y_struct = "none")
  mod_mv_ff <- lavaan::cfa(script_mv_ff, data = commitmentQ, parallel = "no")
  df_mv_ff <- as.double(lavaan::fitmeasures(mod_mv_ff, "df"))

  expect_equal(df_ff_ff, df_mv_mv)
  expect_equal(df_ff_ff, df_ff_mv)
  expect_equal(df_ff_ff, df_mv_ff)
})

test_that("scriptTwoCross produces same chisq for cor/uni when x_scaleset and y_scaleset vary", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    verbose = FALSE
  )

  script_ff_ff <- scriptTwoCross(dvn, lvxname = "Sat", lvyname = "Com",
    x_model = "cor", y_model = "uni", x_scaleset = "FF", y_scaleset = "FF",
    constr_dy_x_meas = "none", constr_dy_x_struct = "none",
    constr_dy_y_meas = "none", constr_dy_y_struct = "none")
  mod_ff_ff <- lavaan::cfa(script_ff_ff, data = commitmentQ, parallel = "no")
  chisq_ff_ff <- as.double(lavaan::fitmeasures(mod_ff_ff, "chisq"))

  script_mv_mv <- scriptTwoCross(dvn, lvxname = "Sat", lvyname = "Com",
    x_model = "cor", y_model = "uni", x_scaleset = "MV", y_scaleset = "MV",
    constr_dy_x_meas = "none", constr_dy_x_struct = "none",
    constr_dy_y_meas = "none", constr_dy_y_struct = "none")
  mod_mv_mv <- lavaan::cfa(script_mv_mv, data = commitmentQ, parallel = "no")
  chisq_mv_mv <- as.double(lavaan::fitmeasures(mod_mv_mv, "chisq"))

  script_ff_mv <- scriptTwoCross(dvn, lvxname = "Sat", lvyname = "Com",
    x_model = "cor", y_model = "uni", x_scaleset = "FF", y_scaleset = "MV",
    constr_dy_x_meas = "none", constr_dy_x_struct = "none",
    constr_dy_y_meas = "none", constr_dy_y_struct = "none")
  mod_ff_mv <- lavaan::cfa(script_ff_mv, data = commitmentQ, parallel = "no")
  chisq_ff_mv <- as.double(lavaan::fitmeasures(mod_ff_mv, "chisq"))

  script_mv_ff <- scriptTwoCross(dvn, lvxname = "Sat", lvyname = "Com",
    x_model = "cor", y_model = "uni", x_scaleset = "MV", y_scaleset = "FF",
    constr_dy_x_meas = "none", constr_dy_x_struct = "none",
    constr_dy_y_meas = "none", constr_dy_y_struct = "none")
  mod_mv_ff <- lavaan::cfa(script_mv_ff, data = commitmentQ, parallel = "no")
  chisq_mv_ff <- as.double(lavaan::fitmeasures(mod_mv_ff, "chisq"))

  expect_equal(chisq_ff_ff, chisq_mv_mv)
  expect_equal(chisq_ff_ff, chisq_ff_mv)
  expect_equal(chisq_ff_ff, chisq_mv_ff)
})

# ---- scriptTwoCross(cor/cor) vs scriptAPIM equivalence ----
# When x_model and y_model are both "cor" and constraint args match, both should
# produce the same model (same df and chisq).
test_that("scriptTwoCross cor/cor produces same df as scriptAPIM when constraint args match", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    verbose = FALSE
  )
  script_tc <- scriptTwoCross(dvn, lvxname = "Sat", lvyname = "Com",
    x_model = "cor", y_model = "cor",
    x_scaleset = "FF", y_scaleset = "FF",
    constr_dy_x_meas = "none", constr_dy_x_struct = "none",
    constr_dy_y_meas = "none", constr_dy_y_struct = "none",
    constr_dy_xy_struct = "free", includeMeanStruct = FALSE
  )
  script_apim <- scriptAPIM(dvn, lvxname = "Sat", lvyname = "Com", scaleset = "FF",
    constr_dy_x_meas = c("none"), constr_dy_y_meas = c("none"),
    constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
    constr_dy_xy_struct = c("none"), includeMeanStruct = FALSE
  )
  mod_tc <- lavaan::cfa(script_tc, data = commitmentQ, parallel = "no")
  mod_apim <- lavaan::cfa(script_apim, data = commitmentQ, parallel = "no")
  expect_equal(
    as.double(lavaan::fitmeasures(mod_tc, "df")),
    as.double(lavaan::fitmeasures(mod_apim, "df"))
  )
})

test_that("scriptTwoCross cor/cor produces same chisq as scriptAPIM when constraint args match", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    verbose = FALSE
  )
  script_tc <- scriptTwoCross(dvn, lvxname = "Sat", lvyname = "Com",
    x_model = "cor", y_model = "cor",
    x_scaleset = "FF", y_scaleset = "FF",
    constr_dy_x_meas = "none", constr_dy_x_struct = "none",
    constr_dy_y_meas = "none", constr_dy_y_struct = "none",
    constr_dy_xy_struct = "free", includeMeanStruct = FALSE
  )
  script_apim <- scriptAPIM(dvn, lvxname = "Sat", lvyname = "Com", scaleset = "FF",
    constr_dy_x_meas = c("none"), constr_dy_y_meas = c("none"),
    constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
    constr_dy_xy_struct = c("none"), includeMeanStruct = FALSE
  )
  mod_tc <- lavaan::cfa(script_tc, data = commitmentQ, parallel = "no")
  mod_apim <- lavaan::cfa(script_apim, data = commitmentQ, parallel = "no")
  expect_equal(
    as.double(lavaan::fitmeasures(mod_tc, "chisq")),
    as.double(lavaan::fitmeasures(mod_apim, "chisq"))
  )
})
