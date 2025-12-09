#### File I/O Tests for outputParamTab ####
# ==========================================
# These tests verify that outputParamTab properly handles file creation,
# overwriting behavior, error handling for directories, and correct file formats.

# Setup: Create test models for different model types
setup_cfa_model <- function() {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    verbose = FALSE
  )
  script <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none")
  mod <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE)
  return(list(dvn = dvn, fit = mod))
}

setup_bidyc_model <- function() {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    verbose = FALSE
  )
  script <- scriptBifac(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none")
  mod <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE)
  return(list(dvn = dvn, fit = mod))
}

setup_apim_model <- function() {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    verbose = FALSE
  )
  script <- scriptAPIM(dvn, lvxname = "Sat", lvyname = "Com", est_k = FALSE)
  mod <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE)
  return(list(dvn = dvn, fit = mod))
}

setup_mim_model <- function() {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    verbose = FALSE
  )
  script <- scriptMIM(dvn, lvxname = "Sat", lvyname = "Com", scaleset = "FF")
  mod <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE)
  return(list(dvn = dvn, fit = mod))
}

setup_cfm_model <- function() {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    verbose = FALSE
  )
  script <- scriptCFM(dvn, lvxname = "Sat", lvyname = "Com", scaleset = "FF")
  mod <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE)
  return(list(dvn = dvn, fit = mod))
}

setup_bidys_model <- function() {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    verbose = FALSE
  )
  script <- scriptBiDy(dvn, type = "SEM", lvxname = "Sat", lvyname = "Com")
  mod <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE)
  return(list(dvn = dvn, fit = mod))
}

#### Group 1: File Creation in Temporary Directories ####
# --------------------------------------------------------
# These tests verify that files are created correctly in temporary directories.

#### CFA Model Tests ####
test_that("outputParamTab creates RTF file for CFA measurement table with custom fileName", {
  models <- setup_cfa_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    outputParamTab(
      dvn = models$dvn, fit = models$fit, model = "cfa",
      tabletype = "measurement", gtTab = TRUE, writeTo = test_dir, fileName = "test_cfa_meas"
    )
  )

  expected_file <- file.path(test_dir, "test_cfa_meas.rtf")
  expect_true(file.exists(expected_file),
    info = paste("Expected file:", expected_file, "not found")
  )
  expect_equal(tools::file_ext(expected_file), "rtf")
})

test_that("outputParamTab creates RTF file for CFA measurement table with default fileName", {
  models <- setup_cfa_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    outputParamTab(
      dvn = models$dvn, fit = models$fit, model = "cfa",
      tabletype = "measurement", gtTab = TRUE, writeTo = test_dir, fileName = NULL
    )
  )

  expected_file <- file.path(test_dir, "dySEM_table.rtf")
  expect_true(file.exists(expected_file),
    info = paste("Expected file:", expected_file, "not found")
  )
})

test_that("outputParamTab creates RTF file for CFA correlation table", {
  models <- setup_cfa_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    outputParamTab(
      dvn = models$dvn, fit = models$fit, model = "cfa",
      tabletype = "correlation", gtTab = TRUE, writeTo = test_dir, fileName = "test_cfa_corr"
    )
  )

  expected_file <- file.path(test_dir, "test_cfa_corr.rtf")
  expect_true(file.exists(expected_file),
    info = paste("Expected file:", expected_file, "not found")
  )
})

#### BiDyC Model Tests ####
test_that("outputParamTab creates RTF file for BiDyC measurement table", {
  models <- setup_bidyc_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    outputParamTab(
      dvn = models$dvn, fit = models$fit, model = "bidyc",
      gtTab = TRUE, writeTo = test_dir, fileName = "test_bidyc"
    )
  )

  expected_file <- file.path(test_dir, "test_bidyc.rtf")
  expect_true(file.exists(expected_file),
    info = paste("Expected file:", expected_file, "not found")
  )
})

#### APIM Model Tests ####
test_that("outputParamTab creates RTF file for APIM measurement table", {
  models <- setup_apim_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    outputParamTab(
      dvn = models$dvn, fit = models$fit, model = "apim",
      tabletype = "measurement", gtTab = TRUE, writeTo = test_dir, fileName = "test_apim_meas"
    )
  )

  expected_file <- file.path(test_dir, "test_apim_meas.rtf")
  expect_true(file.exists(expected_file),
    info = paste("Expected file:", expected_file, "not found")
  )
})

test_that("outputParamTab creates RTF file for APIM structural table", {
  models <- setup_apim_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    outputParamTab(
      dvn = models$dvn, fit = models$fit, model = "apim",
      tabletype = "structural", gtTab = TRUE, writeTo = test_dir, fileName = "test_apim_struct"
    )
  )

  expected_file <- file.path(test_dir, "test_apim_struct.rtf")
  expect_true(file.exists(expected_file),
    info = paste("Expected file:", expected_file, "not found")
  )
})

test_that("outputParamTab creates two RTF files for APIM both tabletype", {
  models <- setup_apim_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    outputParamTab(
      dvn = models$dvn, fit = models$fit, model = "apim",
      tabletype = "both", gtTab = TRUE, writeTo = test_dir, fileName = "test_apim_both"
    )
  )

  meas_file <- file.path(test_dir, "test_apim_both_measurement.rtf")
  struct_file <- file.path(test_dir, "test_apim_both_structural.rtf")
  expect_true(file.exists(meas_file),
    info = paste("Expected measurement file:", meas_file, "not found")
  )
  expect_true(file.exists(struct_file),
    info = paste("Expected structural file:", struct_file, "not found")
  )
})

test_that("outputParamTab creates two RTF files with default names for APIM both tabletype", {
  models <- setup_apim_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    outputParamTab(
      dvn = models$dvn, fit = models$fit, model = "apim",
      tabletype = "both", gtTab = TRUE, writeTo = test_dir, fileName = NULL
    )
  )

  meas_file <- file.path(test_dir, "dySEM_table_measurement.rtf")
  struct_file <- file.path(test_dir, "dySEM_table_structural.rtf")
  expect_true(file.exists(meas_file),
    info = paste("Expected measurement file:", meas_file, "not found")
  )
  expect_true(file.exists(struct_file),
    info = paste("Expected structural file:", struct_file, "not found")
  )
})

#### MIM Model Tests ####
test_that("outputParamTab creates RTF files for MIM both tabletype", {
  models <- setup_mim_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    outputParamTab(
      dvn = models$dvn, fit = models$fit, model = "mim",
      tabletype = "both", gtTab = TRUE, writeTo = test_dir, fileName = "test_mim"
    )
  )

  meas_file <- file.path(test_dir, "test_mim_measurement.rtf")
  struct_file <- file.path(test_dir, "test_mim_structural.rtf")
  expect_true(file.exists(meas_file))
  expect_true(file.exists(struct_file))
})

#### CFM Model Tests ####
test_that("outputParamTab creates RTF files for CFM both tabletype", {
  models <- setup_cfm_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    outputParamTab(
      dvn = models$dvn, fit = models$fit, model = "cfm",
      tabletype = "both", gtTab = TRUE, writeTo = test_dir, fileName = "test_cfm"
    )
  )

  meas_file <- file.path(test_dir, "test_cfm_measurement.rtf")
  struct_file <- file.path(test_dir, "test_cfm_structural.rtf")
  expect_true(file.exists(meas_file))
  expect_true(file.exists(struct_file))
})

#### BiDyS Model Tests ####
test_that("outputParamTab creates RTF files for BiDyS both tabletype", {
  models <- setup_bidys_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    outputParamTab(
      dvn = models$dvn, fit = models$fit, model = "bidys",
      tabletype = "both", gtTab = TRUE, writeTo = test_dir, fileName = "test_bidys"
    )
  )

  meas_file <- file.path(test_dir, "test_bidys_measurement.rtf")
  struct_file <- file.path(test_dir, "test_bidys_structural.rtf")
  expect_true(file.exists(meas_file))
  expect_true(file.exists(struct_file))
})

#### Group 2: File Overwriting Behavior ####
# ------------------------------------------
# These tests verify that existing files are properly overwritten.

test_that("outputParamTab overwrites existing file with same name", {
  models <- setup_cfa_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  output_file <- file.path(test_dir, "test_overwrite.rtf")

  suppressMessages(
    outputParamTab(
      dvn = models$dvn, fit = models$fit, model = "cfa",
      tabletype = "measurement", gtTab = TRUE, writeTo = test_dir, fileName = "test_overwrite"
    )
  )
  first_mtime <- file.info(output_file)$mtime

  Sys.sleep(0.1)

  suppressMessages(
    outputParamTab(
      dvn = models$dvn, fit = models$fit, model = "cfa",
      tabletype = "measurement", gtTab = TRUE, writeTo = test_dir, fileName = "test_overwrite"
    )
  )
  second_mtime <- file.info(output_file)$mtime

  expect_true(file.exists(output_file))
  expect_true(second_mtime > first_mtime,
    info = "File was not overwritten (modification time did not change)"
  )
})

test_that("outputParamTab overwrites both files when tabletype is both", {
  models <- setup_apim_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  meas_file <- file.path(test_dir, "test_overwrite_both_measurement.rtf")
  struct_file <- file.path(test_dir, "test_overwrite_both_structural.rtf")

  suppressMessages(
    outputParamTab(
      dvn = models$dvn, fit = models$fit, model = "apim",
      tabletype = "both", gtTab = TRUE, writeTo = test_dir, fileName = "test_overwrite_both"
    )
  )
  first_meas_mtime <- file.info(meas_file)$mtime
  first_struct_mtime <- file.info(struct_file)$mtime

  Sys.sleep(0.1)

  suppressMessages(
    outputParamTab(
      dvn = models$dvn, fit = models$fit, model = "apim",
      tabletype = "both", gtTab = TRUE, writeTo = test_dir, fileName = "test_overwrite_both"
    )
  )
  second_meas_mtime <- file.info(meas_file)$mtime
  second_struct_mtime <- file.info(struct_file)$mtime

  expect_true(second_meas_mtime > first_meas_mtime,
    info = "Measurement file was not overwritten"
  )
  expect_true(second_struct_mtime > first_struct_mtime,
    info = "Structural file was not overwritten"
  )
})

#### Group 3: Error Handling for Directories ####
# -------------------------------------------------
# These tests verify proper error handling for directory-related issues.

test_that("outputParamTab errors when directory does not exist", {
  models <- setup_cfa_model()

  expect_error(
    suppressMessages(
      outputParamTab(
        dvn = models$dvn, fit = models$fit, model = "cfa",
        tabletype = "measurement", gtTab = TRUE,
        writeTo = "/nonexistent/directory/path", fileName = "test"
      )
    ),
    "The specified directory does not exist"
  )
})

test_that("outputParamTab handles current working directory (writeTo = '.')", {
  models <- setup_cfa_model()
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

  suppressMessages(
    outputParamTab(
      dvn = models$dvn, fit = models$fit, model = "cfa",
      tabletype = "measurement", gtTab = TRUE, writeTo = ".", fileName = "test_cwd"
    )
  )

  expect_true(file.exists("test_cwd.rtf"))
})

#### Group 4: Conditional File Creation ####
# -------------------------------------------
# These tests verify that files are only created under the right conditions.

test_that("outputParamTab does not create file when gtTab is FALSE", {
  models <- setup_cfa_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  result <- outputParamTab(
    dvn = models$dvn, fit = models$fit, model = "cfa",
    tabletype = "measurement", gtTab = FALSE, writeTo = test_dir, fileName = "should_not_exist"
  )

  expected_file <- file.path(test_dir, "should_not_exist.rtf")
  expect_false(file.exists(expected_file),
    info = "File should not be created when gtTab is FALSE"
  )
  expect_true(inherits(result, "tbl_df"))
})

test_that("outputParamTab does not create file when writeTo is NULL", {
  models <- setup_cfa_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  result <- suppressMessages(
    outputParamTab(
      dvn = models$dvn, fit = models$fit, model = "cfa",
      tabletype = "measurement", gtTab = TRUE, writeTo = NULL, fileName = "should_not_exist"
    )
  )

  expected_file <- file.path(test_dir, "should_not_exist.rtf")
  expect_false(file.exists(expected_file),
    info = "File should not be created when writeTo is NULL"
  )
  expect_true(inherits(result, "gt_tbl"))
})

#### Group 5: File Format Verification ####
# -----------------------------------------
# These tests verify that files have correct format and content.

test_that("outputParamTab creates RTF files with valid content", {
  models <- setup_cfa_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    outputParamTab(
      dvn = models$dvn, fit = models$fit, model = "cfa",
      tabletype = "measurement", gtTab = TRUE, writeTo = test_dir, fileName = "test_format"
    )
  )

  created_file <- file.path(test_dir, "test_format.rtf")
  expect_true(file.exists(created_file))
  expect_equal(tools::file_ext(created_file), "rtf")
  expect_gt(file.info(created_file)$size, 0)
})

test_that("outputParamTab creates files with correct extensions for all model types", {
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  # Test CFA
  cfa_models <- setup_cfa_model()
  suppressMessages(
    outputParamTab(
      dvn = cfa_models$dvn, fit = cfa_models$fit, model = "cfa",
      tabletype = "measurement", gtTab = TRUE, writeTo = test_dir, fileName = "cfa_test"
    )
  )
  expect_equal(tools::file_ext(file.path(test_dir, "cfa_test.rtf")), "rtf")

  # Test BiDyC
  bidyc_models <- setup_bidyc_model()
  suppressMessages(
    outputParamTab(
      dvn = bidyc_models$dvn, fit = bidyc_models$fit, model = "bidyc",
      gtTab = TRUE, writeTo = test_dir, fileName = "bidyc_test"
    )
  )
  expect_equal(tools::file_ext(file.path(test_dir, "bidyc_test.rtf")), "rtf")

  # Test APIM
  apim_models <- setup_apim_model()
  suppressMessages(
    outputParamTab(
      dvn = apim_models$dvn, fit = apim_models$fit, model = "apim",
      tabletype = "both", gtTab = TRUE, writeTo = test_dir, fileName = "apim_test"
    )
  )
  expect_equal(tools::file_ext(file.path(test_dir, "apim_test_measurement.rtf")), "rtf")
  expect_equal(tools::file_ext(file.path(test_dir, "apim_test_structural.rtf")), "rtf")
})
