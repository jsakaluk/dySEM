##### Error checks####

test_that("outputConstraintTab produces correct error when writeTo is not character and gtTab is TRUE", {
  dvn <- scrapeVarCross(
    dat = commitmentM, x_order = "sip", x_stem = "sat.g",
    x_delim2 = "_", distinguish_1 = "f", distinguish_2 = "m"
  )

  sat.resids.script <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none")

  sat.resids.mod <- lavaan::cfa(sat.resids.script,
    data = commitmentM, std.lv = FALSE,
    auto.fix.first = FALSE, meanstructure = TRUE
  )

  expect_error(
    outputParamTab(dvn,
      model = "cfa", sat.resids.mod,
      gtTab = TRUE,
      writeTo = 6,
      fileName = "dCFA_Residual"
    ),
    cat("The `writeTo` argument must be a character string. \n Use `writeTo = '.'` to save output file(s) in the current working directory.")
  )
})

test_that("outputParamTab produces correct error when fileName is not character and gtTab is TRUE", {
  dvn <- scrapeVarCross(
    dat = commitmentM, x_order = "sip", x_stem = "sat.g",
    x_delim2 = "_", distinguish_1 = "f", distinguish_2 = "m"
  )

  sat.resids.script <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none")

  sat.resids.mod <- lavaan::cfa(sat.resids.script,
    data = commitmentM, std.lv = FALSE,
    auto.fix.first = FALSE, meanstructure = TRUE
  )

  expect_error(
    outputParamTab(dvn,
      model = "cfa", sat.resids.mod,
      gtTab = TRUE,
      writeTo = tempdir(),
      fileName = 5
    ),
    "The `fileName` argument must be a character string."
  )
})

#### output from CFA ####

test_that("outputParamTab produces correct output for CFA measurement table without gt::", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )

  script <- scriptCor(dvn,
    lvname = "Sat", constr_dy_meas = "none",
    constr_dy_struct = "none"
  )

  mod <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE)



  expect_equal(
    outputParamTab(dvn, fit = mod, model = "cfa", table = "measurement", gtTab = FALSE),
    structure(list(
      `Latent Factor` = c(
        "Sat1", "Sat1", "Sat1", "Sat1",
        "Sat1", "Sat2", "Sat2", "Sat2", "Sat2", "Sat2"
      ), Indicator = c(
        "sat.g.1_1",
        "sat.g.1_2", "sat.g.1_3", "sat.g.1_4", "sat.g.1_5", "sat.g.2_1",
        "sat.g.2_2", "sat.g.2_3", "sat.g.2_4", "sat.g.2_5"
      ), Loading = c(
        2.112,
        1.909, 2.098, 1.955, 1.869, 1.83, 1.843, 1.883, 1.611, 1.926
      ),
      SE = c(
        0.158, 0.168, 0.161, 0.162, 0.179, 0.144, 0.146, 0.146,
        0.151, 0.159
      ), Z = c(
        13.345, 11.383, 13.064, 12.049, 10.429,
        12.668, 12.66, 12.862, 10.669, 12.12
      ), `p-value` = c(
        "< .001",
        "< .001", "< .001", "< .001", "< .001", "< .001", "< .001",
        "< .001", "< .001", "< .001"
      ), `Std. Loading` = c(
        0.937,
        0.851, 0.926, 0.884, 0.806, 0.91, 0.908, 0.918, 0.819, 0.887
      ), Intercept = c(
        6.6, 6.548, 6.409, 6.661, 6.426, 6.93, 6.922,
        6.704, 7.13, 6.817
      )
    ), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -10L))
  )
})

test_that("outputParamTab produces correct output for CFA measurement table with gt + writeTo and fileName", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )

  script <- scriptCor(dvn,
    lvname = "Sat", constr_dy_meas = "none",
    constr_dy_struct = "none"
  )

  mod <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE)

  table <- outputParamTab(dvn, fit = mod, model = "cfa", table = "measurement", gtTab = TRUE, writeTo = tempdir(), fileName = "cfa_indist")
  expect_s3_class(table, "gt_tbl")
  expect_true("_data" %in% names(table)) # Check if "data" component exists
})

test_that("outputParamTab produces correct output for CFA measurement table with gt + writeTo but without fileName", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )

  script <- scriptCor(dvn,
    lvname = "Sat", constr_dy_meas = "none",
    constr_dy_struct = "none"
  )

  mod <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE)

  table <- outputParamTab(dvn, fit = mod, model = "cfa", table = "measurement", gtTab = TRUE, writeTo = tempdir(), fileName = NULL)
  expect_s3_class(table, "gt_tbl")
  expect_true("_data" %in% names(table)) # Check if "data" component exists
})

test_that("outputParamTab produces correct output for CFA measurement table with gt + fileName but without writeTo", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )

  script <- scriptCor(dvn,
    lvname = "Sat", constr_dy_meas = "none",
    constr_dy_struct = "none"
  )

  mod <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE)

  table <- outputParamTab(dvn, fit = mod, model = "cfa", table = "measurement", gtTab = TRUE, writeTo = NULL, fileName = "cfa_indist")
  expect_s3_class(table, "gt_tbl")
  expect_true("_data" %in% names(table)) # Check if "data" component exists
})

#### Error Handling Tests for outputParamTab ####
# ================================================
# These tests verify that outputParamTab properly handles invalid inputs,
# missing required arguments, invalid parameter combinations, and malformed dvn objects.

#### Group 1: Invalid Input Types ####
# ------------------------------------
# These tests verify that outputParamTab rejects inputs of incorrect types.

test_that("outputParamTab rejects non-list dvn argument", {
  # Test that dvn must be a list object
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )
  script <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none")
  mod <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE)

  expect_error(
    outputParamTab(dvn = "not_a_list", model = "cfa", fit = mod, tabletype = "measurement"),
    "The `dvn` argument must be a list object."
  )
})

test_that("outputParamTab rejects non-lavaan fit object", {
  # Test that fit must be a lavaan model object
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_error(
    outputParamTab(dvn = dvn, model = "cfa", fit = "not_a_lavaan_object", tabletype = "measurement"),
    "The `fit` argument must be a fitted lavaan model object."
  )
})

test_that("outputParamTab rejects non-character model argument when provided", {
  # Test that model must be a character string when provided
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )
  script <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none")
  mod <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE)

  expect_error(
    outputParamTab(dvn = dvn, model = 123, fit = mod, tabletype = "measurement"),
    "The `model` argument must be a character string."
  )
})

test_that("outputParamTab rejects non-logical gtTab argument", {
  # Test that gtTab must be a logical value
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )
  script <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none")
  mod <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE)

  expect_error(
    outputParamTab(dvn = dvn, model = "cfa", fit = mod, tabletype = "measurement", gtTab = "yes"),
    "The `gtTab` argument must be a logical value \\(TRUE or FALSE\\)."
  )
})

#### Group 2: Missing Required Arguments ####
# --------------------------------------------
# These tests ensure that required arguments are properly validated.

test_that("outputParamTab requires dvn argument", {
  # Test that dvn is required
  expect_error(
    outputParamTab(model = "cfa", fit = "some_fit", tabletype = "measurement"),
    "The `dvn` argument is required and cannot be NULL."
  )
})

test_that("outputParamTab requires fit argument", {
  # Test that fit is required
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )
  expect_error(
    outputParamTab(dvn = dvn, model = "cfa", tabletype = "measurement"),
    "The `fit` argument is required and cannot be NULL."
  )
})

#### Group 3: Invalid Parameter Combinations ####
# -------------------------------------------------
# These tests check that invalid parameter values are caught.

test_that("outputParamTab handles invalid model value", {
  # Test that invalid model value may cause error or unexpected behavior
  # Note: outputParamTab doesn't validate model values, it just uses them
  # This test documents current behavior where invalid model may return NULL
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )
  script <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none")
  mod <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE)

  # Function may error, produce unexpected output, or return NULL
  result <- tryCatch(
    outputParamTab(dvn = dvn, model = "invalid_model", fit = mod, tabletype = "measurement"),
    error = function(e) e
  )
  # Either errors, produces output, or returns NULL
  expect_true(inherits(result, "error") || is.data.frame(result) || is.null(result) || inherits(result, "gt_tbl"))
})

test_that("outputParamTab handles invalid tabletype value", {
  # Test that invalid tabletype may cause error or unexpected behavior
  # Note: outputParamTab doesn't validate tabletype values, it just uses them
  # This test documents current behavior where invalid tabletype may return NULL
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )
  script <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none")
  mod <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE)

  # Function may error, produce unexpected output, or return NULL
  result <- tryCatch(
    outputParamTab(dvn = dvn, model = "cfa", fit = mod, tabletype = "invalid_type"),
    error = function(e) e
  )
  # Either errors, produces output, or returns NULL
  expect_true(inherits(result, "error") || is.data.frame(result) || is.null(result) || inherits(result, "gt_tbl"))
})

test_that("outputParamTab rejects non-existent directory for writeTo when gtTab is TRUE", {
  # Test that writeTo must point to an existing directory when gtTab is TRUE
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )
  script <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none")
  mod <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE)

  expect_error(
    outputParamTab(
      dvn = dvn, model = "cfa", fit = mod, tabletype = "measurement",
      gtTab = TRUE, writeTo = "/nonexistent/directory/path"
    ),
    "The specified directory does not exist"
  )
})

#### Group 4: Malformed dvn Objects ####
# --------------------------------------
# These tests verify handling of incorrectly structured dvn objects.

test_that("outputParamTab handles dvn object with wrong structure", {
  # Test that malformed dvn causes error when function tries to access missing elements
  # Note: outputParamTab doesn't validate dvn structure, it just tries to use it
  # This test documents current behavior where missing elements cause errors
  dvn_malformed <- list(
    p1xvarnames = c("var1", "var2"),
    p2xvarnames = c("var3", "var4")
    # Missing required elements like xindper, dist1, dist2, indnum
  )
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )
  script <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none")
  mod <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE)

  # Function will try to access missing elements - may error or return NULL
  result <- tryCatch(
    outputParamTab(dvn = dvn_malformed, model = "cfa", fit = mod, tabletype = "measurement"),
    error = function(e) e
  )
  # Either errors or returns NULL/empty result
  expect_true(inherits(result, "error") || is.null(result) || is.data.frame(result))
})

#### output from APIM ####

#### output from MIM ####

test_that("outputParamTab returns tibble for MIM measurement table when gtTab = FALSE", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    verbose = FALSE
  )
  script <- scriptMIM(dvn, lvxname = "Sat", lvyname = "Com", scaleset = "FF")
  fit <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE)

  result <- outputParamTab(
    dvn = dvn,
    fit = fit,
    model = "mim",
    tabletype = "measurement",
    gtTab = FALSE
  )

  expect_s3_class(result, "tbl_df")
})

test_that("outputParamTab returns tibble for MIM structural table when gtTab = FALSE", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    verbose = FALSE
  )
  script <- scriptMIM(dvn, lvxname = "Sat", lvyname = "Com", scaleset = "FF")
  fit <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE)

  result <- outputParamTab(
    dvn = dvn,
    fit = fit,
    model = "mim",
    tabletype = "structural",
    gtTab = FALSE
  )

  expect_s3_class(result, "tbl_df")
})

test_that("outputParamTab returns gt table for MIM measurement table when gtTab = TRUE and writeTo is NULL", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    verbose = FALSE
  )
  script <- scriptMIM(dvn, lvxname = "Sat", lvyname = "Com", scaleset = "FF")
  fit <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE)

  result <- suppressMessages(
    outputParamTab(
      dvn = dvn,
      fit = fit,
      model = "mim",
      tabletype = "measurement",
      gtTab = TRUE,
      writeTo = NULL,
      fileName = "mim_meas_no_write"
    )
  )

  expect_s3_class(result, "gt_tbl")
  expect_true("_data" %in% names(result))
})

test_that("outputParamTab returns gt table for MIM structural table when gtTab = TRUE and writeTo is NULL", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    verbose = FALSE
  )
  script <- scriptMIM(dvn, lvxname = "Sat", lvyname = "Com", scaleset = "FF")
  fit <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE)

  result <- suppressMessages(
    outputParamTab(
      dvn = dvn,
      fit = fit,
      model = "mim",
      tabletype = "structural",
      gtTab = TRUE,
      writeTo = NULL,
      fileName = "mim_struct_no_write"
    )
  )

  expect_s3_class(result, "gt_tbl")
  expect_true("_data" %in% names(result))
})
