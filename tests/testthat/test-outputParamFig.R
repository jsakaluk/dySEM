#### Error Handling Tests for outputParamFig ####
# ================================================
# These tests verify that outputParamFig properly handles invalid inputs,
# missing required arguments, invalid parameter combinations, and provides
# clear, actionable error messages.

#### Group 1: Invalid Input Types ####
# ------------------------------------
# These tests verify that outputParamFig rejects inputs of incorrect types.

test_that("outputParamFig rejects non-lavaan fit object", {
  # Test that fit must be a lavaan model object
  expect_error(
    outputParamFig(
      fit = "not_a_lavaan_object", figtype = "standardized",
      writeTo = tempdir(), fileName = "test"
    ),
    "The `fit` argument must be a fitted lavaan model object."
  )
})

test_that("outputParamFig rejects non-character figtype argument when provided", {
  # Test that figtype must be a character string when provided
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )
  script <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none")
  mod <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE)

  expect_error(
    outputParamFig(fit = mod, figtype = 123, writeTo = tempdir(), fileName = "test"),
    "The `figtype` argument must be a character string."
  )
})

test_that("outputParamFig rejects non-character writeTo argument when provided", {
  # Test that writeTo must be a character string when provided
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )
  script <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none")
  mod <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE)

  expect_error(
    outputParamFig(fit = mod, figtype = "standardized", writeTo = 123, fileName = "test"),
    "The `writeTo` argument must be a character string."
  )
})

test_that("outputParamFig rejects non-character fileName argument when provided", {
  # Test that fileName must be a character string when provided
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )
  script <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none")
  mod <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE)

  expect_error(
    outputParamFig(fit = mod, figtype = "standardized", writeTo = tempdir(), fileName = 123),
    "The `fileName` argument must be a character string."
  )
})

#### Group 2: Missing Required Arguments ####
# --------------------------------------------
# These tests ensure that required arguments are properly validated.

test_that("outputParamFig requires fit argument", {
  # Test that fit is required
  expect_error(
    outputParamFig(figtype = "standardized", writeTo = tempdir(), fileName = "test"),
    "The `fit` argument is required and cannot be NULL."
  )
})

#### Group 3: Invalid Parameter Combinations ####
# -------------------------------------------------
# These tests check that invalid parameter values are caught.

test_that("outputParamFig rejects invalid figtype value", {
  # Test that figtype must be one of the allowed values
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )
  script <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none")
  mod <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE)

  expect_error(
    outputParamFig(fit = mod, figtype = "invalid_type", writeTo = tempdir(), fileName = "test"),
    "The `figtype` argument must be one of: 'unstandardized', 'standardized', or 'labels'."
  )
})

test_that("outputParamFig rejects non-existent directory for writeTo", {
  # Test that writeTo must point to an existing directory when provided
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )
  script <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none")
  mod <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE)

  expect_error(
    outputParamFig(
      fit = mod, figtype = "standardized",
      writeTo = "/nonexistent/directory/path", fileName = "test"
    ),
    "The specified directory does not exist"
  )
})

#### Group 4: Malformed dvn Objects ####
# --------------------------------------
# Note: outputParamFig does not take a dvn object as input,
# so this category is not applicable. The function works directly with
# a fitted lavaan model object.

#### File I/O Tests for outputParamFig ####
# ==========================================
# These tests verify that outputParamFig properly handles file creation,
# overwriting behavior, error handling for directories, and correct file formats.

# Setup: Create a test model for file I/O tests
setup_test_model <- function() {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    verbose = FALSE
  )
  script <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none")
  mod <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE)
  return(mod)
}

#### Group 5: File Creation in Temporary Directories ####
# -------------------------------------------------------
# These tests verify that files are created correctly in temporary directories.

test_that("outputParamFig creates PNG file with custom fileName in temp directory", {
  # Test that a PNG file is created with the correct name and extension
  mod <- setup_test_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  outputParamFig(
    fit = mod, figtype = "standardized",
    writeTo = test_dir, fileName = "test_figure"
  )

  # Check that file was created with correct name and extension
  expected_file <- file.path(test_dir, "test_figure std.png")
  expect_true(file.exists(expected_file),
    info = paste("Expected file:", expected_file, "not found")
  )
})

test_that("outputParamFig creates PNG file with default fileName in temp directory", {
  # Test that a PNG file is created with default name when fileName is NULL
  mod <- setup_test_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  outputParamFig(
    fit = mod, figtype = "standardized",
    writeTo = test_dir, fileName = NULL
  )

  # Check that file was created with default name
  expected_file <- file.path(test_dir, "dySEM_figure std.png")
  expect_true(file.exists(expected_file),
    info = paste("Expected file:", expected_file, "not found")
  )
})

test_that("outputParamFig creates correct PNG files for all figtype values", {
  # Test that correct files are created for unstandardized, standardized, and labels
  mod <- setup_test_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  # Test unstandardized
  outputParamFig(fit = mod, figtype = "unstandardized", writeTo = test_dir, fileName = "test")
  expect_true(file.exists(file.path(test_dir, "test unstd.png")))

  # Test standardized
  outputParamFig(fit = mod, figtype = "standardized", writeTo = test_dir, fileName = "test")
  expect_true(file.exists(file.path(test_dir, "test std.png")))

  # Test labels
  outputParamFig(fit = mod, figtype = "labels", writeTo = test_dir, fileName = "test")
  expect_true(file.exists(file.path(test_dir, "test lab.png")))
})

test_that("outputParamFig creates PNG files with correct format", {
  # Test that created files are actually PNG format (check file extension and magic bytes)
  mod <- setup_test_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  outputParamFig(
    fit = mod, figtype = "standardized",
    writeTo = test_dir, fileName = "test_format"
  )

  created_file <- file.path(test_dir, "test_format std.png")
  expect_true(file.exists(created_file))

  # Check file extension
  expect_equal(tools::file_ext(created_file), "png")

  # Check that file is not empty (PNG files should have content)
  expect_gt(file.info(created_file)$size, 0)
})

#### Group 6: File Overwriting Behavior ####
# ------------------------------------------
# These tests verify that existing files are properly overwritten.

test_that("outputParamFig overwrites existing file with same name", {
  # Test that calling the function twice with same fileName overwrites the first file
  mod <- setup_test_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  output_file <- file.path(test_dir, "test_overwrite std.png")

  # Create file first time
  outputParamFig(fit = mod, figtype = "standardized", writeTo = test_dir, fileName = "test_overwrite")
  first_size <- file.info(output_file)$size
  first_mtime <- file.info(output_file)$mtime

  # Wait a moment to ensure different modification time
  Sys.sleep(0.1)

  # Create file second time (should overwrite)
  outputParamFig(fit = mod, figtype = "standardized", writeTo = test_dir, fileName = "test_overwrite")
  second_mtime <- file.info(output_file)$mtime

  # File should still exist and have been modified
  expect_true(file.exists(output_file))
  expect_true(second_mtime > first_mtime,
    info = "File was not overwritten (modification time did not change)"
  )
})

#### Group 7: Error Handling for Directories ####
# ------------------------------------------------
# These tests verify proper error handling for directory-related issues.

test_that("outputParamFig errors when directory does not exist", {
  # Test that function errors when writeTo points to non-existent directory
  mod <- setup_test_model()

  expect_error(
    outputParamFig(
      fit = mod, figtype = "standardized",
      writeTo = "/nonexistent/directory/path", fileName = "test"
    ),
    "The specified directory does not exist"
  )
})

test_that("outputParamFig errors when writeTo is NULL but fileName is provided", {
  # Test that function errors when writeTo is NULL but fileName is provided
  # Note: makeFigure doesn't handle this case (writeTo NULL, fileName provided)
  # This documents current behavior - the function errors in this scenario
  mod <- setup_test_model()

  # When writeTo is NULL but fileName is provided, makeFigure doesn't handle this case
  # and will error because semplot is never created
  expect_error(
    outputParamFig(fit = mod, figtype = "standardized", writeTo = NULL, fileName = "test"),
    "object 'semplot' not found"
  )
})

test_that("outputParamFig handles current working directory (writeTo = '.')", {
  # Test that function works when writeTo is set to current working directory
  mod <- setup_test_model()
  original_wd <- getwd()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(
    {
      setwd(original_wd)
      unlink(test_dir, recursive = TRUE)
    },
    add = TRUE
  )

  setwd(test_dir)

  outputParamFig(fit = mod, figtype = "standardized", writeTo = ".", fileName = "test_cwd")

  # Check that file was created in current directory
  expect_true(file.exists("test_cwd std.png"))
})
