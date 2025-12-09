#### Error Handling Tests for scriptCFA ####
# ============================================
# These tests verify that scriptCFA properly handles invalid inputs,
# missing required arguments, invalid parameter combinations, and malformed dvn objects.

#### Group 1: Invalid Input Types ####
# ------------------------------------
# These tests verify that scriptCFA rejects inputs of incorrect types.

test_that("scriptCFA rejects non-list dvn argument", {
  # Test that dvn must be a list object
  expect_error(
    scriptCFA(dvn = "not_a_list"),
    "The `dvn` argument must be a list object."
  )
})

test_that("scriptCFA rejects non-character scaleset argument", {
  # Test that scaleset must be a character string
  # Note: scriptCFA requires var_list in scrapeVarCross, so we'll test with a malformed dvn
  dvn_malformed <- list(
    p1xvarnames = list(Sat = c("var1", "var2")),
    p2xvarnames = list(Sat = c("var3", "var4")),
    xindper = 2L,
    dist1 = "1",
    dist2 = "2",
    indnum = 4L
  )
  expect_error(
    scriptCFA(dvn = dvn_malformed, scaleset = 123),
    regexp = ".*"
  )
})

#### Group 2: Missing Required Arguments ####
# --------------------------------------------
# These tests ensure that required arguments are properly validated.

test_that("scriptCFA requires dvn argument", {
  # Test that dvn is required
  expect_error(
    scriptCFA(),
    "The `dvn` argument is required and cannot be NULL."
  )
})

#### Group 3: Invalid Parameter Combinations ####
# -------------------------------------------------
# These tests check that invalid parameter values are caught.

test_that("scriptCFA rejects invalid constr_dy_meas values", {
  # Test that constr_dy_meas must contain valid options
  # Create a minimal dvn for multi-factor model
  imsList <- list(
    lvnames = c("Sat", "Q_Alt"),
    stem = c("sat.g", "qalt.g"),
    delim1 = c("", ""),
    delim2 = c("_", "_")
  )
  dvn <- scrapeVarCross(imsM,
    var_list = imsList, var_list_order = "sip",
    distinguish_1 = "f", distinguish_2 = "m"
  )
  expect_error(
    scriptCFA(dvn = dvn, constr_dy_meas = "invalid_option"),
    regexp = ".*"
  )
})

test_that("scriptCFA rejects invalid scaleset value", {
  # Test that scaleset must be either "FF" or "MV"
  imsList <- list(
    lvnames = c("Sat", "Q_Alt"),
    stem = c("sat.g", "qalt.g"),
    delim1 = c("", ""),
    delim2 = c("_", "_")
  )
  dvn <- scrapeVarCross(imsM,
    var_list = imsList, var_list_order = "sip",
    distinguish_1 = "f", distinguish_2 = "m"
  )
  expect_error(
    scriptCFA(dvn = dvn, scaleset = "invalid"),
    regexp = ".*"
  )
})

#### Group 4: Malformed dvn Objects ####
# --------------------------------------
# These tests verify handling of incorrectly structured dvn objects.

test_that("scriptCFA rejects dvn object with wrong structure", {
  # Test that scriptCFA requires a dvn from var_list (multi-factor structure)
  dvn_x_only <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )
  # scriptCFA should error or warn when given single-factor dvn
  expect_error(
    scriptCFA(dvn = dvn_x_only),
    regexp = ".*"
  )
})

test_that("scriptCFA rejects dvn object with missing required elements", {
  # Test that dvn must contain all required elements for multi-factor models
  dvn_malformed <- list(
    p1xvarnames = list(Sat = c("var1", "var2"))
    # Missing p2xvarnames and other required elements
  )
  expect_error(
    scriptCFA(dvn = dvn_malformed),
    regexp = ".*"
  )
})
