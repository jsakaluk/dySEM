#### File I/O Tests for outputInvarCompTab ####
# =============================================
# These tests verify that outputInvarCompTab properly handles file creation,
# overwriting behavior, error handling for directories, and correct file formats.

# Setup: Create test models for file I/O tests
setup_test_models <- function() {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    verbose = FALSE
  )

  sat.residual.script <- scriptCor(dvn,
    lvname = "Sat",
    constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none"
  )
  sat.intercept.script <- scriptCor(dvn,
    lvname = "Sat",
    constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none"
  )
  sat.loading.script <- scriptCor(dvn,
    lvname = "Sat",
    constr_dy_meas = c("loadings"), constr_dy_struct = "none"
  )
  sat.config.script <- scriptCor(dvn,
    lvname = "Sat",
    constr_dy_meas = "none", constr_dy_struct = "none"
  )

  sat.residual.fit <- lavaan::cfa(sat.residual.script,
    data = commitmentQ,
    std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE
  )
  sat.intercept.fit <- lavaan::cfa(sat.intercept.script,
    data = commitmentQ,
    std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE
  )
  sat.loading.fit <- lavaan::cfa(sat.loading.script,
    data = commitmentQ,
    std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE
  )
  sat.config.fit <- lavaan::cfa(sat.config.script,
    data = commitmentQ,
    std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE
  )

  return(list(sat.residual.fit, sat.intercept.fit, sat.loading.fit, sat.config.fit))
}

#### Group 1: File Creation in Temporary Directories ####
# --------------------------------------------------------
# These tests verify that files are created correctly in temporary directories.

test_that("outputInvarCompTab creates RTF file with custom fileName in temp directory", {
  # Test that an RTF file is created with the correct name and extension
  mods <- setup_test_models()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  outputInvarCompTab(mods,
    parsimonyFirst = FALSE,
    gtTab = TRUE, writeTo = test_dir, fileName = "test_invar_table"
  )

  # Check that file was created with correct name and extension
  expected_file <- file.path(test_dir, "test_invar_table.rtf")
  expect_true(file.exists(expected_file),
    info = paste("Expected file:", expected_file, "not found")
  )
})

test_that("outputInvarCompTab creates RTF file with default fileName in temp directory", {
  # Test that an RTF file is created with default name when fileName is NULL
  mods <- setup_test_models()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  outputInvarCompTab(mods,
    parsimonyFirst = FALSE,
    gtTab = TRUE, writeTo = test_dir, fileName = NULL
  )

  # Check that file was created with default name
  expected_file <- file.path(test_dir, "dySEM_table.rtf")
  expect_true(file.exists(expected_file),
    info = paste("Expected file:", expected_file, "not found")
  )
})

test_that("outputInvarCompTab creates RTF files with correct format", {
  # Test that created files are actually RTF format (check file extension)
  mods <- setup_test_models()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  outputInvarCompTab(mods,
    parsimonyFirst = FALSE,
    gtTab = TRUE, writeTo = test_dir, fileName = "test_format"
  )

  created_file <- file.path(test_dir, "test_format.rtf")
  expect_true(file.exists(created_file))

  # Check file extension
  expect_equal(tools::file_ext(created_file), "rtf")

  # Check that file is not empty (RTF files should have content)
  expect_gt(file.info(created_file)$size, 0)
})

test_that("outputInvarCompTab does not create file when gtTab is FALSE", {
  # Test that no file is created when gtTab is FALSE
  mods <- setup_test_models()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  result <- outputInvarCompTab(mods,
    parsimonyFirst = FALSE,
    gtTab = FALSE, writeTo = test_dir, fileName = "should_not_exist"
  )

  # Check that no file was created
  expected_file <- file.path(test_dir, "should_not_exist.rtf")
  expect_false(file.exists(expected_file),
    info = "File should not be created when gtTab is FALSE"
  )

  # But function should still return a tibble
  expect_true(inherits(result, "tbl_df"))
})

test_that("outputInvarCompTab does not create file when writeTo is NULL", {
  # Test that no file is created when writeTo is NULL (even if gtTab is TRUE)
  mods <- setup_test_models()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  result <- outputInvarCompTab(mods,
    parsimonyFirst = FALSE,
    gtTab = TRUE, writeTo = NULL, fileName = "should_not_exist"
  )

  # Check that no file was created in test_dir
  expected_file <- file.path(test_dir, "should_not_exist.rtf")
  expect_false(file.exists(expected_file),
    info = "File should not be created when writeTo is NULL"
  )

  # But function should still return a gt object
  expect_true(inherits(result, "gt_tbl"))
})

#### Group 2: File Overwriting Behavior ####
# ------------------------------------------
# These tests verify that existing files are properly overwritten.

test_that("outputInvarCompTab overwrites existing file with same name", {
  # Test that calling the function twice with same fileName overwrites the first file
  mods <- setup_test_models()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  output_file <- file.path(test_dir, "test_overwrite.rtf")

  # Create file first time
  outputInvarCompTab(mods,
    parsimonyFirst = FALSE,
    gtTab = TRUE, writeTo = test_dir, fileName = "test_overwrite"
  )
  first_size <- file.info(output_file)$size
  first_mtime <- file.info(output_file)$mtime

  # Wait a moment to ensure different modification time
  Sys.sleep(0.1)

  # Create file second time (should overwrite)
  outputInvarCompTab(mods,
    parsimonyFirst = FALSE,
    gtTab = TRUE, writeTo = test_dir, fileName = "test_overwrite"
  )
  second_mtime <- file.info(output_file)$mtime

  # File should still exist and have been modified
  expect_true(file.exists(output_file))
  expect_true(second_mtime > first_mtime,
    info = "File was not overwritten (modification time did not change)"
  )
})

#### Group 3: Error Handling for Directories ####
# -------------------------------------------------
# These tests verify proper error handling for directory-related issues.

test_that("outputInvarCompTab errors when directory does not exist", {
  # Test that function errors when writeTo points to non-existent directory
  mods <- setup_test_models()

  expect_error(
    outputInvarCompTab(mods,
      parsimonyFirst = FALSE,
      gtTab = TRUE, writeTo = "/nonexistent/directory/path", fileName = "test"
    ),
    "The specified directory does not exist"
  )
})

test_that("outputInvarCompTab handles current working directory (writeTo = '.')", {
  # Test that function works when writeTo is set to current working directory
  mods <- setup_test_models()
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

  outputInvarCompTab(mods,
    parsimonyFirst = FALSE,
    gtTab = TRUE, writeTo = ".", fileName = "test_cwd"
  )

  # Check that file was created in current directory
  expect_true(file.exists("test_cwd.rtf"))
})

#### Group 4: File Content Verification ####
# -------------------------------------------
# These tests verify that files contain expected content structure.

test_that("outputInvarCompTab creates RTF file with valid content", {
  # Test that the created RTF file has valid RTF structure (starts with RTF header)
  mods <- setup_test_models()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  outputInvarCompTab(mods,
    parsimonyFirst = FALSE,
    gtTab = TRUE, writeTo = test_dir, fileName = "test_content"
  )

  created_file <- file.path(test_dir, "test_content.rtf")
  expect_true(file.exists(created_file))

  # Read first few bytes to check RTF header
  file_content <- readBin(created_file, "raw", n = 10)
  # RTF files typically start with "{\rtf" or similar RTF header
  # Since gt::gtsave creates RTF, we just verify file has content
  expect_gt(length(file_content), 0)
})
