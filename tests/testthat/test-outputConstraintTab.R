# Error checks

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
    outputConstraintTab(sat.resids.mod,
      filterSig = FALSE,
      gtTab = TRUE,
      writeTo = 5,
      fileName = "dCFA_Residual"
    ),
    cat("The `writeTo` argument must be a character string. \n Use `writeTo = '.'` to save output file(s) in the current working directory.")
  )
})

test_that("outputConstraintTab produces correct error when fileName is not character and gtTab is TRUE", {
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
    outputConstraintTab(sat.resids.mod,
      filterSig = FALSE,
      gtTab = TRUE,
      writeTo = tempdir(),
      fileName = 5
    ),
    "The `fileName` argument must be a character string."
  )
})

#### Error Handling Tests for outputConstraintTab ####
# =====================================================
# These tests verify that outputConstraintTab properly handles invalid inputs,
# missing required arguments, invalid parameter combinations, and provides
# clear, actionable error messages.

#### Group 1: Invalid Input Types ####
# ------------------------------------
# These tests verify that outputConstraintTab rejects inputs of incorrect types.

test_that("outputConstraintTab rejects non-lavaan constrainFit object", {
  # Test that constrainFit must be a lavaan model object
  expect_error(
    outputConstraintTab(constrainFit = "not_a_lavaan_object"),
    "The `constrainFit` argument must be a fitted lavaan model object."
  )
})

test_that("outputConstraintTab rejects non-logical filterSig argument", {
  # Test that filterSig must be a logical value
  dvn <- scrapeVarCross(
    dat = commitmentM, x_order = "sip", x_stem = "sat.g",
    x_delim2 = "_", distinguish_1 = "f", distinguish_2 = "m"
  )
  sat.resids.script <- scriptCor(dvn,
    lvname = "Sat", constr_dy_meas = c("loadings", "intercepts", "residuals"),
    constr_dy_struct = "none"
  )
  sat.resids.mod <- lavaan::cfa(sat.resids.script,
    data = commitmentM, std.lv = FALSE,
    auto.fix.first = FALSE, meanstructure = TRUE
  )

  expect_error(
    outputConstraintTab(constrainFit = sat.resids.mod, filterSig = "yes"),
    "The `filterSig` argument must be a logical value \\(TRUE or FALSE\\)."
  )
})

test_that("outputConstraintTab rejects non-logical gtTab argument", {
  # Test that gtTab must be a logical value
  dvn <- scrapeVarCross(
    dat = commitmentM, x_order = "sip", x_stem = "sat.g",
    x_delim2 = "_", distinguish_1 = "f", distinguish_2 = "m"
  )
  sat.resids.script <- scriptCor(dvn,
    lvname = "Sat", constr_dy_meas = c("loadings", "intercepts", "residuals"),
    constr_dy_struct = "none"
  )
  sat.resids.mod <- lavaan::cfa(sat.resids.script,
    data = commitmentM, std.lv = FALSE,
    auto.fix.first = FALSE, meanstructure = TRUE
  )

  expect_error(
    outputConstraintTab(constrainFit = sat.resids.mod, gtTab = "yes"),
    "The `gtTab` argument must be a logical value \\(TRUE or FALSE\\)."
  )
})

#### Group 2: Missing Required Arguments ####
# --------------------------------------------
# These tests ensure that required arguments are properly validated.

test_that("outputConstraintTab requires constrainFit argument", {
  # Test that constrainFit is required
  expect_error(
    outputConstraintTab(),
    "The `constrainFit` argument is required and cannot be NULL."
  )
})

#### Group 3: Invalid Parameter Combinations ####
# -------------------------------------------------
# These tests check that invalid parameter values are caught.

test_that("outputConstraintTab rejects non-existent directory for writeTo when gtTab is TRUE", {
  # Test that writeTo must point to an existing directory when gtTab is TRUE
  dvn <- scrapeVarCross(
    dat = commitmentM, x_order = "sip", x_stem = "sat.g",
    x_delim2 = "_", distinguish_1 = "f", distinguish_2 = "m"
  )
  sat.resids.script <- scriptCor(dvn,
    lvname = "Sat", constr_dy_meas = c("loadings", "intercepts", "residuals"),
    constr_dy_struct = "none"
  )
  sat.resids.mod <- lavaan::cfa(sat.resids.script,
    data = commitmentM, std.lv = FALSE,
    auto.fix.first = FALSE, meanstructure = TRUE
  )

  expect_error(
    outputConstraintTab(
      constrainFit = sat.resids.mod, gtTab = TRUE,
      writeTo = "/nonexistent/directory/path"
    ),
    "The specified directory does not exist"
  )
})

#### Group 4: Malformed dvn Objects ####
# --------------------------------------
# Note: outputConstraintTab does not take a dvn object as input,
# so this category is not applicable. The function works directly with
# a fitted lavaan model object.
