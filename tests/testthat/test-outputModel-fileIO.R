#### File I/O Tests for outputModel (Deprecated Function) ####
# ============================================================
# These tests verify that outputModel properly handles file creation,
# overwriting behavior, error handling for directories, and correct file formats.
# Note: outputModel is deprecated but these tests document its current behavior.

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

#### Group 1: Deprecation Warning Tests ####
# ------------------------------------------
# These tests verify that deprecation warnings are properly issued.

test_that("outputModel issues deprecation warning", {
  # Test that a deprecation warning is issued when function is called
  models <- setup_cfa_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  expect_warning(
    suppressMessages(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfa",
        table = TRUE, figure = FALSE, writeTo = test_dir, fileName = "test"
      )
    ),
    "deprecated"
  )
})

test_that("outputModel deprecation warning includes correct message", {
  # Test that deprecation warning suggests using outputParamTab and outputParamFig
  models <- setup_cfa_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  expect_warning(
    suppressMessages(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfa",
        table = TRUE, figure = FALSE, writeTo = test_dir, fileName = "test"
      )
    ),
    "outputParamTab|outputParamFig"
  )
})

#### Group 2: File Creation - Table Only (table=TRUE, figure=FALSE) ####
# -----------------------------------------------------------------------
# These tests verify that RTF table files are created correctly when only tables are requested.

#### CFA Model Tests ####
test_that("outputModel creates RTF file for CFA measurement table with custom fileName", {
  models <- setup_cfa_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfa",
        table = TRUE, figure = FALSE, writeTo = test_dir, fileName = "test_cfa_table"
      )
    )
  )

  expected_file <- file.path(test_dir, "test_cfa_table.rtf")
  expect_true(file.exists(expected_file),
    info = paste("Expected file:", expected_file, "not found")
  )
  expect_equal(tools::file_ext(expected_file), "rtf")
})

test_that("outputModel creates RTF file for CFA measurement table with default fileName", {
  models <- setup_cfa_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfa",
        table = TRUE, figure = FALSE, writeTo = test_dir, fileName = NULL
      )
    )
  )

  expected_file <- file.path(test_dir, "dySEM_table.rtf")
  expect_true(file.exists(expected_file),
    info = paste("Expected file:", expected_file, "not found")
  )
})

#### BiDyC Model Tests ####
test_that("outputModel creates RTF file for BiDyC measurement table", {
  models <- setup_bidyc_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "bidyc",
        table = TRUE, figure = FALSE, writeTo = test_dir, fileName = "test_bidyc"
      )
    )
  )

  expected_file <- file.path(test_dir, "test_bidyc.rtf")
  expect_true(file.exists(expected_file),
    info = paste("Expected file:", expected_file, "not found")
  )
})

#### APIM Model Tests ####
test_that("outputModel creates RTF file for APIM measurement table", {
  models <- setup_apim_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "apim",
        table = TRUE, figure = FALSE, tabletype = "measurement",
        writeTo = test_dir, fileName = "test_apim_meas"
      )
    )
  )

  expected_file <- file.path(test_dir, "test_apim_meas.rtf")
  expect_true(file.exists(expected_file),
    info = paste("Expected file:", expected_file, "not found")
  )
})

test_that("outputModel creates RTF file for APIM structural table", {
  models <- setup_apim_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "apim",
        table = TRUE, figure = FALSE, tabletype = "structural",
        writeTo = test_dir, fileName = "test_apim_struct"
      )
    )
  )

  expected_file <- file.path(test_dir, "test_apim_struct.rtf")
  expect_true(file.exists(expected_file),
    info = paste("Expected file:", expected_file, "not found")
  )
})

test_that("outputModel creates two RTF files for APIM both tabletype with custom fileName", {
  models <- setup_apim_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "apim",
        table = TRUE, figure = FALSE, tabletype = "both",
        writeTo = test_dir, fileName = "test_apim_both"
      )
    )
  )

  meas_file <- file.path(test_dir, "test_apim_both_Measurement.rtf")
  struct_file <- file.path(test_dir, "test_apim_both_Structural.rtf")
  expect_true(file.exists(meas_file),
    info = paste("Expected measurement file:", meas_file, "not found")
  )
  expect_true(file.exists(struct_file),
    info = paste("Expected structural file:", struct_file, "not found")
  )
  # Note: outputModel uses capitalized "_Measurement" and "_Structural" (not lowercase)
})

test_that("outputModel creates two RTF files for APIM both tabletype with default fileName", {
  models <- setup_apim_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "apim",
        table = TRUE, figure = FALSE, tabletype = "both",
        writeTo = test_dir, fileName = NULL
      )
    )
  )

  meas_file <- file.path(test_dir, "dySEM_table_Measurement.rtf")
  struct_file <- file.path(test_dir, "dySEM_table_Structural.rtf")
  expect_true(file.exists(meas_file),
    info = paste("Expected measurement file:", meas_file, "not found")
  )
  expect_true(file.exists(struct_file),
    info = paste("Expected structural file:", struct_file, "not found")
  )
})

#### MIM Model Tests ####
test_that("outputModel creates two RTF files for MIM both tabletype", {
  models <- setup_mim_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "mim",
        table = TRUE, figure = FALSE, tabletype = "both",
        writeTo = test_dir, fileName = "test_mim"
      )
    )
  )

  meas_file <- file.path(test_dir, "test_mim_Measurement.rtf")
  struct_file <- file.path(test_dir, "test_mim_Structural.rtf")
  expect_true(file.exists(meas_file))
  expect_true(file.exists(struct_file))
})

#### CFM Model Tests ####
test_that("outputModel creates two RTF files for CFM both tabletype", {
  models <- setup_cfm_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfm",
        table = TRUE, figure = FALSE, tabletype = "both",
        writeTo = test_dir, fileName = "test_cfm"
      )
    )
  )

  meas_file <- file.path(test_dir, "test_cfm_Measurement.rtf")
  struct_file <- file.path(test_dir, "test_cfm_Structural.rtf")
  expect_true(file.exists(meas_file))
  expect_true(file.exists(struct_file))
})

#### BiDyS Model Tests ####
test_that("outputModel creates two RTF files for BiDyS both tabletype", {
  models <- setup_bidys_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "bidys",
        table = TRUE, figure = FALSE, tabletype = "both",
        writeTo = test_dir, fileName = "test_bidys"
      )
    )
  )

  meas_file <- file.path(test_dir, "test_bidys_Measurement.rtf")
  struct_file <- file.path(test_dir, "test_bidys_Structural.rtf")
  expect_true(file.exists(meas_file))
  expect_true(file.exists(struct_file))
})

#### Group 3: File Creation - Figure Only (table=FALSE, figure=TRUE) ####
# ------------------------------------------------------------------------
# These tests verify that PNG figure files are created correctly when only figures are requested.

test_that("outputModel creates PNG file for unstandardized figure with custom fileName", {
  models <- setup_cfa_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfa",
        table = FALSE, figure = TRUE, figtype = "unstandardized",
        writeTo = test_dir, fileName = "test_fig"
      )
    )
  )

  expected_file <- file.path(test_dir, "test_fig unstd.png")
  expect_true(file.exists(expected_file),
    info = paste("Expected file:", expected_file, "not found")
  )
  expect_equal(tools::file_ext(expected_file), "png")
})

test_that("outputModel creates PNG file for standardized figure with custom fileName", {
  models <- setup_cfa_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfa",
        table = FALSE, figure = TRUE, figtype = "standardized",
        writeTo = test_dir, fileName = "test_fig"
      )
    )
  )

  expected_file <- file.path(test_dir, "test_fig std.png")
  expect_true(file.exists(expected_file),
    info = paste("Expected file:", expected_file, "not found")
  )
})

test_that("outputModel creates PNG file for labels figure with custom fileName", {
  models <- setup_cfa_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfa",
        table = FALSE, figure = TRUE, figtype = "labels",
        writeTo = test_dir, fileName = "test_fig"
      )
    )
  )

  expected_file <- file.path(test_dir, "test_fig lab.png")
  expect_true(file.exists(expected_file),
    info = paste("Expected file:", expected_file, "not found")
  )
})

test_that("outputModel creates PNG file for standardized figure with default fileName", {
  models <- setup_cfa_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfa",
        table = FALSE, figure = TRUE, figtype = "standardized",
        writeTo = test_dir, fileName = NULL
      )
    )
  )

  expected_file <- file.path(test_dir, "dySEM_figure std.png")
  expect_true(file.exists(expected_file),
    info = paste("Expected file:", expected_file, "not found")
  )
})

test_that("outputModel creates correct PNG files for all figtype values", {
  models <- setup_cfa_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  # Test unstandardized
  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfa",
        table = FALSE, figure = TRUE, figtype = "unstandardized",
        writeTo = test_dir, fileName = "test_all"
      )
    )
  )
  expect_true(file.exists(file.path(test_dir, "test_all unstd.png")))

  # Test standardized
  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfa",
        table = FALSE, figure = TRUE, figtype = "standardized",
        writeTo = test_dir, fileName = "test_all"
      )
    )
  )
  expect_true(file.exists(file.path(test_dir, "test_all std.png")))

  # Test labels
  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfa",
        table = FALSE, figure = TRUE, figtype = "labels",
        writeTo = test_dir, fileName = "test_all"
      )
    )
  )
  expect_true(file.exists(file.path(test_dir, "test_all lab.png")))
})

#### Group 4: File Creation - Both Table and Figure (table=TRUE, figure=TRUE) ####
# --------------------------------------------------------------------------------
# These tests verify that both RTF table files and PNG figure files are created correctly.

test_that("outputModel creates both RTF and PNG files for CFA model", {
  models <- setup_cfa_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfa",
        table = TRUE, figure = TRUE, figtype = "standardized",
        writeTo = test_dir, fileName = "test_both"
      )
    )
  )

  rtf_file <- file.path(test_dir, "test_both.rtf")
  png_file <- file.path(test_dir, "test_both std.png")
  expect_true(file.exists(rtf_file),
    info = paste("Expected RTF file:", rtf_file, "not found")
  )
  expect_true(file.exists(png_file),
    info = paste("Expected PNG file:", png_file, "not found")
  )
})

test_that("outputModel creates both RTF and PNG files for BiDyC model", {
  models <- setup_bidyc_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "bidyc",
        table = TRUE, figure = TRUE, figtype = "standardized",
        writeTo = test_dir, fileName = "test_bidyc_both"
      )
    )
  )

  rtf_file <- file.path(test_dir, "test_bidyc_both.rtf")
  png_file <- file.path(test_dir, "test_bidyc_both std.png")
  expect_true(file.exists(rtf_file))
  expect_true(file.exists(png_file))
})

test_that("outputModel creates RTF table and PNG figure for APIM measurement tabletype", {
  models <- setup_apim_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "apim",
        table = TRUE, figure = TRUE, tabletype = "measurement",
        figtype = "standardized", writeTo = test_dir, fileName = "test_apim_meas_both"
      )
    )
  )

  rtf_file <- file.path(test_dir, "test_apim_meas_both.rtf")
  png_file <- file.path(test_dir, "test_apim_meas_both std.png")
  expect_true(file.exists(rtf_file))
  expect_true(file.exists(png_file))
})

test_that("outputModel creates RTF table and PNG figure for APIM structural tabletype", {
  models <- setup_apim_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "apim",
        table = TRUE, figure = TRUE, tabletype = "structural",
        figtype = "standardized", writeTo = test_dir, fileName = "test_apim_struct_both"
      )
    )
  )

  rtf_file <- file.path(test_dir, "test_apim_struct_both.rtf")
  png_file <- file.path(test_dir, "test_apim_struct_both std.png")
  expect_true(file.exists(rtf_file))
  expect_true(file.exists(png_file))
})

test_that("outputModel creates two RTF files and one PNG file for APIM both tabletype", {
  models <- setup_apim_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "apim",
        table = TRUE, figure = TRUE, tabletype = "both",
        figtype = "standardized", writeTo = test_dir, fileName = "test_apim_all"
      )
    )
  )

  meas_file <- file.path(test_dir, "test_apim_all_Measurement.rtf")
  struct_file <- file.path(test_dir, "test_apim_all_Structural.rtf")
  png_file <- file.path(test_dir, "test_apim_all std.png")
  expect_true(file.exists(meas_file))
  expect_true(file.exists(struct_file))
  expect_true(file.exists(png_file))
})

test_that("outputModel creates two RTF files and one PNG file for MIM both tabletype", {
  models <- setup_mim_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "mim",
        table = TRUE, figure = TRUE, tabletype = "both",
        figtype = "standardized", writeTo = test_dir, fileName = "test_mim_all"
      )
    )
  )

  meas_file <- file.path(test_dir, "test_mim_all_Measurement.rtf")
  struct_file <- file.path(test_dir, "test_mim_all_Structural.rtf")
  png_file <- file.path(test_dir, "test_mim_all std.png")
  expect_true(file.exists(meas_file))
  expect_true(file.exists(struct_file))
  expect_true(file.exists(png_file))
})

test_that("outputModel creates two RTF files and one PNG file for CFM both tabletype", {
  models <- setup_cfm_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfm",
        table = TRUE, figure = TRUE, tabletype = "both",
        figtype = "standardized", writeTo = test_dir, fileName = "test_cfm_all"
      )
    )
  )

  meas_file <- file.path(test_dir, "test_cfm_all_Measurement.rtf")
  struct_file <- file.path(test_dir, "test_cfm_all_Structural.rtf")
  png_file <- file.path(test_dir, "test_cfm_all std.png")
  expect_true(file.exists(meas_file))
  expect_true(file.exists(struct_file))
  expect_true(file.exists(png_file))
})

test_that("outputModel creates two RTF files and one PNG file for BiDyS both tabletype", {
  models <- setup_bidys_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "bidys",
        table = TRUE, figure = TRUE, tabletype = "both",
        figtype = "standardized", writeTo = test_dir, fileName = "test_bidys_all"
      )
    )
  )

  meas_file <- file.path(test_dir, "test_bidys_all_Measurement.rtf")
  struct_file <- file.path(test_dir, "test_bidys_all_Structural.rtf")
  png_file <- file.path(test_dir, "test_bidys_all std.png")
  expect_true(file.exists(meas_file))
  expect_true(file.exists(struct_file))
  expect_true(file.exists(png_file))
})

#### Group 5: File Overwriting Behavior ####
# ------------------------------------------
# These tests verify that existing files are properly overwritten.

test_that("outputModel overwrites existing RTF file with same name", {
  models <- setup_cfa_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  output_file <- file.path(test_dir, "test_overwrite.rtf")

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfa",
        table = TRUE, figure = FALSE, writeTo = test_dir, fileName = "test_overwrite"
      )
    )
  )
  first_mtime <- file.info(output_file)$mtime

  Sys.sleep(0.1)

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfa",
        table = TRUE, figure = FALSE, writeTo = test_dir, fileName = "test_overwrite"
      )
    )
  )
  second_mtime <- file.info(output_file)$mtime

  expect_true(file.exists(output_file))
  expect_true(second_mtime > first_mtime,
    info = "RTF file was not overwritten (modification time did not change)"
  )
})

test_that("outputModel overwrites existing PNG file with same name", {
  models <- setup_cfa_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  output_file <- file.path(test_dir, "test_overwrite std.png")

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfa",
        table = FALSE, figure = TRUE, figtype = "standardized",
        writeTo = test_dir, fileName = "test_overwrite"
      )
    )
  )
  first_mtime <- file.info(output_file)$mtime

  Sys.sleep(0.1)

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfa",
        table = FALSE, figure = TRUE, figtype = "standardized",
        writeTo = test_dir, fileName = "test_overwrite"
      )
    )
  )
  second_mtime <- file.info(output_file)$mtime

  expect_true(file.exists(output_file))
  expect_true(second_mtime > first_mtime,
    info = "PNG file was not overwritten (modification time did not change)"
  )
})

test_that("outputModel overwrites both RTF and PNG files when table=TRUE and figure=TRUE", {
  models <- setup_cfa_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  rtf_file <- file.path(test_dir, "test_overwrite_both.rtf")
  png_file <- file.path(test_dir, "test_overwrite_both std.png")

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfa",
        table = TRUE, figure = TRUE, figtype = "standardized",
        writeTo = test_dir, fileName = "test_overwrite_both"
      )
    )
  )
  first_rtf_mtime <- file.info(rtf_file)$mtime
  first_png_mtime <- file.info(png_file)$mtime

  Sys.sleep(0.1)

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfa",
        table = TRUE, figure = TRUE, figtype = "standardized",
        writeTo = test_dir, fileName = "test_overwrite_both"
      )
    )
  )
  second_rtf_mtime <- file.info(rtf_file)$mtime
  second_png_mtime <- file.info(png_file)$mtime

  expect_true(second_rtf_mtime > first_rtf_mtime,
    info = "RTF file was not overwritten"
  )
  expect_true(second_png_mtime > first_png_mtime,
    info = "PNG file was not overwritten"
  )
})

test_that("outputModel overwrites multiple files when tabletype is both", {
  models <- setup_apim_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  meas_file <- file.path(test_dir, "test_overwrite_multi_Measurement.rtf")
  struct_file <- file.path(test_dir, "test_overwrite_multi_Structural.rtf")

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "apim",
        table = TRUE, figure = FALSE, tabletype = "both",
        writeTo = test_dir, fileName = "test_overwrite_multi"
      )
    )
  )
  first_meas_mtime <- file.info(meas_file)$mtime
  first_struct_mtime <- file.info(struct_file)$mtime

  Sys.sleep(0.1)

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "apim",
        table = TRUE, figure = FALSE, tabletype = "both",
        writeTo = test_dir, fileName = "test_overwrite_multi"
      )
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

#### Group 6: Error Handling for Directories ####
# -------------------------------------------------
# These tests verify proper error handling for directory-related issues.

test_that("outputModel errors when writeTo is NULL", {
  # Test that writeTo is required (cannot be NULL)
  models <- setup_cfa_model()

  expect_error(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfa",
        table = TRUE, figure = FALSE, writeTo = NULL, fileName = "test"
      )
    ),
    "Must specify a directory"
  )
})

test_that("outputModel errors when directory does not exist", {
  models <- setup_cfa_model()

  expect_error(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfa",
        table = TRUE, figure = FALSE,
        writeTo = "/nonexistent/directory/path", fileName = "test"
      )
    ),
    "The specified directory does not exist"
  )
})

test_that("outputModel errors when writeTo is not a character string", {
  models <- setup_cfa_model()

  expect_error(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfa",
        table = TRUE, figure = FALSE, writeTo = 123, fileName = "test"
      )
    ),
    "The `writeTo` argument must be a character string"
  )
})

test_that("outputModel errors when fileName is not a character string", {
  models <- setup_cfa_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  expect_error(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfa",
        table = TRUE, figure = FALSE, writeTo = test_dir, fileName = 123
      )
    ),
    "The `fileName` argument must be a character string"
  )
})

test_that("outputModel handles current working directory (writeTo = '.')", {
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
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfa",
        table = TRUE, figure = FALSE, writeTo = ".", fileName = "test_cwd"
      )
    )
  )

  expect_true(file.exists("test_cwd.rtf"))
})

#### Group 7: Edge Cases and Combinations ####
# ---------------------------------------------
# These tests verify edge cases and various parameter combinations.

test_that("outputModel handles table=FALSE, figure=FALSE (no files created)", {
  # Test edge case where both table and figure are FALSE
  models <- setup_cfa_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfa",
        table = FALSE, figure = FALSE, writeTo = test_dir, fileName = "should_not_exist"
      )
    )
  )

  rtf_file <- file.path(test_dir, "should_not_exist.rtf")
  png_file <- file.path(test_dir, "should_not_exist std.png")
  expect_false(file.exists(rtf_file),
    info = "RTF file should not be created when table=FALSE"
  )
  expect_false(file.exists(png_file),
    info = "PNG file should not be created when figure=FALSE"
  )
})

test_that("outputModel creates files for all model types", {
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  # Test CFA
  cfa_models <- setup_cfa_model()
  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = cfa_models$dvn, fit = cfa_models$fit, model = "cfa",
        table = TRUE, figure = FALSE, writeTo = test_dir, fileName = "test_cfa"
      )
    )
  )
  expect_true(file.exists(file.path(test_dir, "test_cfa.rtf")))

  # Test BiDyC
  bidyc_models <- setup_bidyc_model()
  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = bidyc_models$dvn, fit = bidyc_models$fit, model = "bidyc",
        table = TRUE, figure = FALSE, writeTo = test_dir, fileName = "test_bidyc"
      )
    )
  )
  expect_true(file.exists(file.path(test_dir, "test_bidyc.rtf")))

  # Test APIM
  apim_models <- setup_apim_model()
  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = apim_models$dvn, fit = apim_models$fit, model = "apim",
        table = TRUE, figure = FALSE, tabletype = "measurement",
        writeTo = test_dir, fileName = "test_apim"
      )
    )
  )
  expect_true(file.exists(file.path(test_dir, "test_apim.rtf")))

  # Test MIM
  mim_models <- setup_mim_model()
  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = mim_models$dvn, fit = mim_models$fit, model = "mim",
        table = TRUE, figure = FALSE, tabletype = "measurement",
        writeTo = test_dir, fileName = "test_mim"
      )
    )
  )
  expect_true(file.exists(file.path(test_dir, "test_mim.rtf")))

  # Test CFM
  cfm_models <- setup_cfm_model()
  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = cfm_models$dvn, fit = cfm_models$fit, model = "cfm",
        table = TRUE, figure = FALSE, tabletype = "measurement",
        writeTo = test_dir, fileName = "test_cfm"
      )
    )
  )
  expect_true(file.exists(file.path(test_dir, "test_cfm.rtf")))

  # Test BiDyS
  bidys_models <- setup_bidys_model()
  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = bidys_models$dvn, fit = bidys_models$fit, model = "bidys",
        table = TRUE, figure = FALSE, tabletype = "measurement",
        writeTo = test_dir, fileName = "test_bidys"
      )
    )
  )
  expect_true(file.exists(file.path(test_dir, "test_bidys.rtf")))
})

#### Group 8: File Format Verification ####
# ------------------------------------------
# These tests verify that files have correct format and content.

test_that("outputModel creates RTF files with valid content", {
  models <- setup_cfa_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfa",
        table = TRUE, figure = FALSE, writeTo = test_dir, fileName = "test_format_rtf"
      )
    )
  )

  created_file <- file.path(test_dir, "test_format_rtf.rtf")
  expect_true(file.exists(created_file))
  expect_equal(tools::file_ext(created_file), "rtf")
  expect_gt(file.info(created_file)$size, 0)
})

test_that("outputModel creates PNG files with valid content", {
  models <- setup_cfa_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfa",
        table = FALSE, figure = TRUE, figtype = "standardized",
        writeTo = test_dir, fileName = "test_format_png"
      )
    )
  )

  created_file <- file.path(test_dir, "test_format_png std.png")
  expect_true(file.exists(created_file))
  expect_equal(tools::file_ext(created_file), "png")
  expect_gt(file.info(created_file)$size, 0)
})

test_that("outputModel creates files with correct extensions for all combinations", {
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  models <- setup_cfa_model()

  # Test RTF file
  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfa",
        table = TRUE, figure = FALSE, writeTo = test_dir, fileName = "test_ext"
      )
    )
  )
  expect_equal(tools::file_ext(file.path(test_dir, "test_ext.rtf")), "rtf")

  # Test PNG file
  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfa",
        table = FALSE, figure = TRUE, figtype = "standardized",
        writeTo = test_dir, fileName = "test_ext"
      )
    )
  )
  expect_equal(tools::file_ext(file.path(test_dir, "test_ext std.png")), "png")

  # Test both files
  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "cfa",
        table = TRUE, figure = TRUE, figtype = "standardized",
        writeTo = test_dir, fileName = "test_ext_both"
      )
    )
  )
  expect_equal(tools::file_ext(file.path(test_dir, "test_ext_both.rtf")), "rtf")
  expect_equal(tools::file_ext(file.path(test_dir, "test_ext_both std.png")), "png")
})

test_that("outputModel uses capitalized suffixes for both tabletype", {
  models <- setup_apim_model()
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  suppressMessages(
    suppressWarnings(
      outputModel(
        dvn = models$dvn, fit = models$fit, model = "apim",
        table = TRUE, figure = FALSE, tabletype = "both",
        writeTo = test_dir, fileName = "test_caps"
      )
    )
  )

  # Verify capitalized suffixes (not lowercase like newer functions)
  meas_file <- file.path(test_dir, "test_caps_Measurement.rtf")
  struct_file <- file.path(test_dir, "test_caps_Structural.rtf")
  expect_true(file.exists(meas_file),
    info = "Measurement file should use capitalized '_Measurement' suffix"
  )
  expect_true(file.exists(struct_file),
    info = "Structural file should use capitalized '_Structural' suffix"
  )
  # Verify file names match expected capitalized pattern
  # Note: On case-insensitive filesystems (e.g., macOS), both capitalized and lowercase
  # paths may resolve to the same file, so we verify the actual file names in the directory
  created_files <- list.files(test_dir)
  expect_true("test_caps_Measurement.rtf" %in% created_files,
    info = "File should be named with capitalized '_Measurement' suffix"
  )
  expect_true("test_caps_Structural.rtf" %in% created_files,
    info = "File should be named with capitalized '_Structural' suffix"
  )
  # Verify lowercase versions are NOT in the directory listing
  expect_false("test_caps_measurement.rtf" %in% created_files,
    info = "Lowercase version should not exist as separate file"
  )
  expect_false("test_caps_structural.rtf" %in% created_files,
    info = "Lowercase version should not exist as separate file"
  )
})
