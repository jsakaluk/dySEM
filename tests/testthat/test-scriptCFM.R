#### Error Handling and Edge Case Tests for scriptCFM ####
# =========================================================
# These tests verify that scriptCFM properly handles invalid inputs,
# deprecated arguments, and covers previously untested code paths.

#### Group 1: Deprecated Argument Tests ####
# ------------------------------------------
# These tests verify that the deprecated 'model' argument triggers appropriate errors.

test_that("scriptCFM stops with deprecation error when model argument is provided", {
  # Create a valid dvn object with both X and Y
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  # Test that providing the deprecated 'model' argument triggers deprecation error
  expect_error(
    scriptCFM(dvn, lvxname = "Sat", lvyname = "Com", model = "some_model"),
    regexp = 'The argument "scriptCFM\\(model\\)"'
  )
})

#### Group 2: Invalid dvn Object Tests ####
# ------------------------------------------
# These tests verify that scriptCFM properly handles malformed dvn objects.

test_that("scriptCFM stops when dvn object does not contain both X and Y", {
  # Create a dvn object with only X (length 6, not 9)
  dvn_x_only <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  # Test that scriptCFM stops when dvn doesn't have both X and Y
  expect_error(
    scriptCFM(dvn_x_only, lvxname = "Sat", lvyname = "Com"),
    "You must supply a dvn object containing information for both X and Y"
  )
})

#### Group 3: MV Scaleset Tests for Loadings ####
# ------------------------------------------------
# These tests cover the MV scaleset branches for loadings that were previously untested.

test_that("scriptCFM handles MV scaleset with loadings constraint for X", {
  # Create a valid dvn object with both X and Y
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  # Test MV scaleset with loadings constraint for X
  # This should trigger lines 107-109 (MV with loadings constraint)
  script <- scriptCFM(
    dvn,
    lvxname = "Sat", lvyname = "Com",
    scaleset = "MV",
    constr_dy_x_meas = "loadings",
    constr_dy_x_struct = "none",
    constr_dy_y_meas = "none",
    constr_dy_y_struct = "none"
  )

  # Verify script is generated (not an error)
  expect_type(script, "character")
  expect_true(nchar(script) > 0)
})

test_that("scriptCFM handles MV scaleset without loadings constraint for X", {
  # Create a valid dvn object with both X and Y
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  # Test MV scaleset without loadings constraint for X
  # This should trigger lines 116-118 (MV without loadings constraint)
  script <- scriptCFM(
    dvn,
    lvxname = "Sat", lvyname = "Com",
    scaleset = "MV",
    constr_dy_x_meas = "none",
    constr_dy_x_struct = "none",
    constr_dy_y_meas = "none",
    constr_dy_y_struct = "none"
  )

  # Verify script is generated (not an error)
  expect_type(script, "character")
  expect_true(nchar(script) > 0)
})

test_that("scriptCFM handles MV scaleset with loadings constraint for Y", {
  # Create a valid dvn object with both X and Y
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  # Test MV scaleset with loadings constraint for Y
  script <- scriptCFM(
    dvn,
    lvxname = "Sat", lvyname = "Com",
    scaleset = "MV",
    constr_dy_x_meas = "none",
    constr_dy_x_struct = "none",
    constr_dy_y_meas = "loadings",
    constr_dy_y_struct = "none"
  )

  # Verify script is generated (not an error)
  expect_type(script, "character")
  expect_true(nchar(script) > 0)
})

test_that("scriptCFM handles MV scaleset without loadings constraint for Y", {
  # Create a valid dvn object with both X and Y
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  # Test MV scaleset without loadings constraint for Y
  script <- scriptCFM(
    dvn,
    lvxname = "Sat", lvyname = "Com",
    scaleset = "MV",
    constr_dy_x_meas = "none",
    constr_dy_x_struct = "none",
    constr_dy_y_meas = "none",
    constr_dy_y_struct = "none"
  )

  # Verify script is generated (not an error)
  expect_type(script, "character")
  expect_true(nchar(script) > 0)
})

#### Group 4: MV Scaleset Tests for Variances ####
# ------------------------------------------------
# These tests cover the MV scaleset branches for variances that were previously untested.

test_that("scriptCFM handles MV scaleset with variances constraint for X", {
  # Create a valid dvn object with both X and Y
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  # Test MV scaleset with variances constraint for X
  # This should trigger lines 225-228 (MV with variances constraint)
  script <- scriptCFM(
    dvn,
    lvxname = "Sat", lvyname = "Com",
    scaleset = "MV",
    constr_dy_x_meas = "none",
    constr_dy_x_struct = "variances",
    constr_dy_y_meas = "none",
    constr_dy_y_struct = "none"
  )

  # Verify script is generated (not an error)
  expect_type(script, "character")
  expect_true(nchar(script) > 0)
})

test_that("scriptCFM handles MV scaleset without variances constraint for X (no loadings)", {
  # Create a valid dvn object with both X and Y
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  # Test MV scaleset without variances constraint and without loadings constraint for X
  # This should trigger lines 239-242 (MV without variances constraint, no loadings)
  script <- scriptCFM(
    dvn,
    lvxname = "Sat", lvyname = "Com",
    scaleset = "MV",
    constr_dy_x_meas = "none",
    constr_dy_x_struct = "none",
    constr_dy_y_meas = "none",
    constr_dy_y_struct = "none"
  )

  # Verify script is generated (not an error)
  expect_type(script, "character")
  expect_true(nchar(script) > 0)
})

test_that("scriptCFM handles MV scaleset with variances constraint for Y", {
  # Create a valid dvn object with both X and Y
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  # Test MV scaleset with variances constraint for Y
  script <- scriptCFM(
    dvn,
    lvxname = "Sat", lvyname = "Com",
    scaleset = "MV",
    constr_dy_x_meas = "none",
    constr_dy_x_struct = "none",
    constr_dy_y_meas = "none",
    constr_dy_y_struct = "variances"
  )

  # Verify script is generated (not an error)
  expect_type(script, "character")
  expect_true(nchar(script) > 0)
})

test_that("scriptCFM handles MV scaleset without variances constraint for Y (no loadings)", {
  # Create a valid dvn object with both X and Y
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  # Test MV scaleset without variances constraint and without loadings constraint for Y
  script <- scriptCFM(
    dvn,
    lvxname = "Sat", lvyname = "Com",
    scaleset = "MV",
    constr_dy_x_meas = "none",
    constr_dy_x_struct = "none",
    constr_dy_y_meas = "none",
    constr_dy_y_struct = "none"
  )

  # Verify script is generated (not an error)
  expect_type(script, "character")
  expect_true(nchar(script) > 0)
})

#### Group 5: Complex Constraint Combinations ####
# -------------------------------------------------
# These tests cover the special case where variances are not constrained but loadings are (FF scaleset)

test_that("scriptCFM handles FF scaleset with loadings but no variances constraint for X", {
  # Create a valid dvn object with both X and Y
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  # Test FF scaleset with loadings constraint but no variances constraint for X
  # This should trigger lines 230-233 (special case: no variances but loadings with FF)
  script <- scriptCFM(
    dvn,
    lvxname = "Sat", lvyname = "Com",
    scaleset = "FF",
    constr_dy_x_meas = "loadings",
    constr_dy_x_struct = "none",
    constr_dy_y_meas = "none",
    constr_dy_y_struct = "none"
  )

  # Verify script is generated (not an error)
  expect_type(script, "character")
  expect_true(nchar(script) > 0)
})

test_that("scriptCFM handles FF scaleset with loadings but no variances constraint for Y", {
  # Create a valid dvn object with both X and Y
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  # Test FF scaleset with loadings constraint but no variances constraint for Y
  # This should trigger lines 256-259 (special case: no variances but loadings with FF)
  script <- scriptCFM(
    dvn,
    lvxname = "Sat", lvyname = "Com",
    scaleset = "FF",
    constr_dy_x_meas = "none",
    constr_dy_x_struct = "none",
    constr_dy_y_meas = "loadings",
    constr_dy_y_struct = "none"
  )

  # Verify script is generated (not an error)
  expect_type(script, "character")
  expect_true(nchar(script) > 0)
})

#### Group 6: Comprehensive MV Scaleset Tests ####
# -------------------------------------------------
# These tests ensure all MV branches are covered with various constraint combinations

test_that("scriptCFM handles MV scaleset with all measurement constraints for X and Y", {
  # Create a valid dvn object with both X and Y
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  # Test MV scaleset with all measurement constraints
  script <- scriptCFM(
    dvn,
    lvxname = "Sat", lvyname = "Com",
    scaleset = "MV",
    constr_dy_x_meas = c("loadings", "intercepts", "residuals"),
    constr_dy_x_struct = c("variances", "means"),
    constr_dy_y_meas = c("loadings", "intercepts", "residuals"),
    constr_dy_y_struct = c("variances", "means")
  )

  # Verify script is generated (not an error)
  expect_type(script, "character")
  expect_true(nchar(script) > 0)
})

test_that("scriptCFM handles MV scaleset with no constraints", {
  # Create a valid dvn object with both X and Y
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  # Test MV scaleset with no constraints (configural model)
  script <- scriptCFM(
    dvn,
    lvxname = "Sat", lvyname = "Com",
    scaleset = "MV",
    constr_dy_x_meas = "none",
    constr_dy_x_struct = "none",
    constr_dy_y_meas = "none",
    constr_dy_y_struct = "none"
  )

  # Verify script is generated (not an error)
  expect_type(script, "character")
  expect_true(nchar(script) > 0)
})
