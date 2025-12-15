#### Tests for scriptBiDy ####

test_that("scriptBiDy errors when deprecated model argument is provided", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    verbose = FALSE
  )

  expect_error(
    scriptBiDy(
      dvn = dvn,
      type = "SEM",
      lvxname = "Sat",
      lvyname = "Com",
      model = "deprecated"
    ),
    "The argument \"scriptBiDy\\(model\\)\"",
    fixed = FALSE
  )
})

test_that("scriptBiDy errors when deprecated equate argument is provided", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    verbose = FALSE
  )

  expect_error(
    scriptBiDy(
      dvn = dvn,
      type = "SEM",
      lvxname = "Sat",
      lvyname = "Com",
      equate = "deprecated"
    ),
    "The argument \"scriptBiDy\\(equate\\)\"",
    fixed = FALSE
  )
})

test_that("scriptBiDy errors when dvn does not have expected length", {
  dvn_bad <- list(p1xvarnames = "x1", p2xvarnames = "x2") # length != 6

  expect_error(
    scriptBiDy(
      dvn = dvn_bad,
      type = "SEM",
      lvxname = "Sat",
      lvyname = "Com"
    ),
    "You must supply a dvn object containing information for both X and Y",
    fixed = FALSE
  )
})

test_that("scriptBiDy errors on invalid constr_dy_x_meas input", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    verbose = FALSE
  )

  expect_error(
    scriptBiDy(
      dvn = dvn,
      type = "SEM",
      lvxname = "Sat",
      lvyname = "Com",
      constr_dy_x_meas = "not_a_valid_option"
    ),
    "constr_dy_meas must be a character vector",
    fixed = FALSE
  )
})

test_that("scriptBiDy errors on invalid constr_dy_x_struct input", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    verbose = FALSE
  )

  expect_error(
    scriptBiDy(
      dvn = dvn,
      type = "SEM",
      lvxname = "Sat",
      lvyname = "Com",
      constr_dy_x_struct = "not_a_valid_option"
    ),
    "constr_dy_x_struct must be a character vector",
    fixed = FALSE
  )
})

test_that("scriptBiDy returns a character script for default constraints", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    verbose = FALSE
  )

  script <- scriptBiDy(
    dvn = dvn,
    type = "SEM",
    lvxname = "Sat",
    lvyname = "Com"
  )

  expect_type(script, "character")
  expect_match(script, "#Measurement Model", fixed = TRUE)
  expect_match(script, "#Structural Model", fixed = TRUE)
})

test_that("scriptBiDy (CFA) respects constr_dy_x_struct = 'variances'", {
  dvn <- scrapeVarCross(
    dat = DRES,
    x_order = "sip", x_stem = "sexsat",
    x_delim2 = ".", distinguish_1 = "1", distinguish_2 = "2",
    verbose = FALSE
  )

  script <- scriptBiDy(
    dvn = dvn,
    type = "CFA",
    lvxname = "SexSat",
    lvyname = NULL,
    constr_dy_x_meas = "loadings",
    constr_dy_x_struct = "variances"
  )

  expect_type(script, "character")
  # Variance constraints should introduce psix labels
  expect_match(script, "psix", fixed = TRUE)
})

test_that("scriptBiDy (CFA) respects constr_dy_x_struct = 'means'", {
  dvn <- scrapeVarCross(
    dat = DRES,
    x_order = "sip", x_stem = "sexsat",
    x_delim2 = ".", distinguish_1 = "1", distinguish_2 = "2",
    verbose = FALSE
  )

  script <- scriptBiDy(
    dvn = dvn,
    type = "CFA",
    lvxname = "SexSat",
    lvyname = NULL,
    constr_dy_x_meas = "intercepts",
    constr_dy_x_struct = "means"
  )

  expect_type(script, "character")
  # Mean constraints should introduce alphax labels
  expect_match(script, "alphax", fixed = TRUE)
})

test_that("scriptBiDy respects loading constraint options for X", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    verbose = FALSE
  )

  # loadings
  script_loadings <- scriptBiDy(
    dvn = dvn,
    type = "SEM",
    lvxname = "Sat",
    lvyname = "Com",
    constr_dy_x_meas = "loadings"
  )
  expect_match(script_loadings, "lx1", fixed = TRUE)

  # loading_source
  script_source <- scriptBiDy(
    dvn = dvn,
    type = "SEM",
    lvxname = "Sat",
    lvyname = "Com",
    constr_dy_x_meas = "loading_source"
  )
  expect_match(script_source, "lx1", fixed = TRUE)

  # loading_mutual
  script_mutual <- scriptBiDy(
    dvn = dvn,
    type = "SEM",
    lvxname = "Sat",
    lvyname = "Com",
    constr_dy_x_meas = "loading_mutual"
  )
  expect_match(script_mutual, "SatDy=~", fixed = TRUE)
})

test_that("scriptBiDy writes script to file when writeTo and fileName are valid", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    verbose = FALSE
  )

  tmp_dir <- tempfile("scriptBiDy_dir_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_name <- "test_scriptBiDy"

  script <- scriptBiDy(
    dvn = dvn,
    type = "SEM",
    lvxname = "Sat",
    lvyname = "Com",
    writeTo = tmp_dir,
    fileName = file_name
  )

  expect_type(script, "character")
  expected_path <- file.path(tmp_dir, paste0(file_name, ".txt"))
  expect_true(file.exists(expected_path))
})

test_that("scriptBiDy errors on invalid writeTo or fileName when writing to disk", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    verbose = FALSE
  )

  # Non-character writeTo
  expect_error(
    scriptBiDy(
      dvn = dvn,
      type = "SEM",
      lvxname = "Sat",
      lvyname = "Com",
      writeTo = 123,
      fileName = "bad"
    ),
    "The `writeout` argument must be a character string",
    fixed = FALSE
  )

  # Non-existent directory
  expect_error(
    scriptBiDy(
      dvn = dvn,
      type = "SEM",
      lvxname = "Sat",
      lvyname = "Com",
      writeTo = "/nonexistent/directory/path",
      fileName = "bad"
    ),
    "The specified directory does not exist",
    fixed = FALSE
  )

  # Non-character fileName
  tmp_dir <- tempfile("scriptBiDy_bad_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    scriptBiDy(
      dvn = dvn,
      type = "SEM",
      lvxname = "Sat",
      lvyname = "Com",
      writeTo = tmp_dir,
      fileName = 123
    ),
    "The `fileName` argument must be a character string.",
    fixed = TRUE
  )
})
