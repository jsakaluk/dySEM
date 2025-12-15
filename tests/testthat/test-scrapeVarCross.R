#### scrapeVarCross spi order####
test_that("scrapeVarCross produces correct output for LV X for spi order", {
  expect_equal(
    scrapeVarCross(
      dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
      x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
    ),
    list(
      p1xvarnames = c(
        "sat.g.1_1", "sat.g.1_2", "sat.g.1_3", "sat.g.1_4",
        "sat.g.1_5"
      ), p2xvarnames = c(
        "sat.g.2_1", "sat.g.2_2", "sat.g.2_3",
        "sat.g.2_4", "sat.g.2_5"
      ), xindper = 5L, dist1 = "1", dist2 = "2",
      indnum = 10L
    )
  )
})

test_that("scrapeVarCross produces correct output for LV X and LV Y for spi order", {
  expect_equal(
    scrapeVarCross(
      dat = commitmentQ,
      x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
      y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
      distinguish_1 = "1", distinguish_2 = "2"
    ),
    list(
      p1xvarnames = c(
        "sat.g.1_1", "sat.g.1_2", "sat.g.1_3", "sat.g.1_4",
        "sat.g.1_5"
      ), p2xvarnames = c(
        "sat.g.2_1", "sat.g.2_2", "sat.g.2_3",
        "sat.g.2_4", "sat.g.2_5"
      ), xindper = 5L, dist1 = "1", dist2 = "2",
      p1yvarnames = c(
        "com.1_1", "com.1_2", "com.1_3", "com.1_4",
        "com.1_5"
      ), p2yvarnames = c(
        "com.2_1", "com.2_2", "com.2_3",
        "com.2_4", "com.2_5"
      ), yindper = 5L, indnum = 20L
    )
  )
})

#### scrapeVarCross sip order####

test_that("scrapeVarCross produces correct output for LV X for sip order", {
  expect_equal(
    scrapeVarCross(
      dat = DRES,
      x_order = "sip", x_stem = "PRQC", x_delim1 = "_", x_delim2 = ".",
      distinguish_1 = "1", distinguish_2 = "2"
    ),
    list(
      p1xvarnames = c(
        "PRQC_1.1", "PRQC_2.1", "PRQC_3.1", "PRQC_4.1",
        "PRQC_5.1", "PRQC_6.1", "PRQC_7.1", "PRQC_8.1", "PRQC_9.1"
      ),
      p2xvarnames = c(
        "PRQC_1.2", "PRQC_2.2", "PRQC_3.2", "PRQC_4.2",
        "PRQC_5.2", "PRQC_6.2", "PRQC_7.2", "PRQC_8.2", "PRQC_9.2"
      ), xindper = 9L, dist1 = "1", dist2 = "2", indnum = 18L
    )
  )
})

test_that("scrapeVarCross produces correct output for LV X and LV Y for sip order", {
  expect_equal(
    scrapeVarCross(
      dat = DRES,
      x_order = "sip", x_stem = "PRQC", x_delim1 = "_", x_delim2 = ".",
      y_order = "sip", y_stem = "sexsat", y_delim2 = ".",
      distinguish_1 = "1", distinguish_2 = "2"
    ),
    list(
      p1xvarnames = c(
        "PRQC_1.1", "PRQC_2.1", "PRQC_3.1", "PRQC_4.1",
        "PRQC_5.1", "PRQC_6.1", "PRQC_7.1", "PRQC_8.1", "PRQC_9.1"
      ),
      p2xvarnames = c(
        "PRQC_1.2", "PRQC_2.2", "PRQC_3.2", "PRQC_4.2",
        "PRQC_5.2", "PRQC_6.2", "PRQC_7.2", "PRQC_8.2", "PRQC_9.2"
      ), xindper = 9L, dist1 = "1", dist2 = "2", p1yvarnames = c(
        "sexsat1.1",
        "sexsat2.1", "sexsat3.1", "sexsat4.1", "sexsat5.1"
      ), p2yvarnames = c(
        "sexsat1.2",
        "sexsat2.2", "sexsat3.2", "sexsat4.2", "sexsat5.2"
      ), yindper = 5L,
      indnum = 28L
    )
  )
})


# TODO: make unit tests for dfs with unequal items per partner to test error

#### Error Handling Tests for scrapeVarCross ####
# ================================================
# These tests verify that scrapeVarCross properly handles invalid inputs,
# missing required arguments, invalid parameter combinations, and provides
# clear, actionable error messages.

#### Group 1: Invalid Input Types ####
# ------------------------------------
# These tests verify that scrapeVarCross handles inputs of incorrect types.
# Note: scrapeVarCross does not currently validate input types strictly,
# but these tests document current behavior.

test_that("scrapeVarCross rejects non-data.frame dat argument", {
  # Test that dat must be a data.frame object
  expect_error(
    scrapeVarCross(dat = "not_a_dataframe", x_stem = "test"),
    "The `dat` argument must be a data.frame object."
  )
})

test_that("scrapeVarCross rejects non-character x_stem argument", {
  # Test that x_stem must be a character string
  expect_error(
    scrapeVarCross(dat = commitmentQ, x_stem = 123),
    "The `x_stem` argument must be a character string."
  )
})

test_that("scrapeVarCross rejects non-character x_order argument", {
  # Test that x_order must be a character string when provided
  expect_error(
    scrapeVarCross(dat = commitmentQ, x_stem = "sat.g", x_order = 123),
    "The `x_order` argument must be a character string."
  )
})

test_that("scrapeVarCross rejects non-character y_order argument when provided", {
  # Test that y_order must be a character string when provided
  expect_error(
    scrapeVarCross(dat = commitmentQ, x_stem = "sat.g", y_order = 123, y_stem = "com"),
    "The `y_order` argument must be a character string."
  )
})

test_that("scrapeVarCross rejects non-character distinguish_1 argument", {
  # Test that distinguish_1 must be a character string
  expect_error(
    scrapeVarCross(dat = commitmentQ, x_stem = "sat.g", distinguish_1 = 123),
    "The `distinguish_1` argument must be a character string."
  )
})

test_that("scrapeVarCross rejects non-character distinguish_2 argument", {
  # Test that distinguish_2 must be a character string
  expect_error(
    scrapeVarCross(dat = commitmentQ, x_stem = "sat.g", distinguish_2 = 123),
    "The `distinguish_2` argument must be a character string."
  )
})

test_that("scrapeVarCross rejects non-logical verbose argument", {
  # Test that verbose must be a logical value
  expect_error(
    scrapeVarCross(dat = commitmentQ, x_stem = "sat.g", verbose = "yes"),
    "The `verbose` argument must be a logical value \\(TRUE or FALSE\\)."
  )
})

#### Group 2: Missing Required Arguments ####
# --------------------------------------------
# These tests ensure that required arguments are properly validated.

test_that("scrapeVarCross requires x_stem argument", {
  # Test that x_stem is required (no default value)
  # Note: R's default error message for missing arguments, but our validation also catches NULL
  expect_error(
    scrapeVarCross(dat = commitmentQ),
    "The `x_stem` argument is required and cannot be NULL."
  )
})

#### Group 3: Invalid Parameter Combinations ####
# -------------------------------------------------
# These tests check that invalid parameter values are caught.

test_that("scrapeVarCross rejects invalid x_order value", {
  # Test that x_order must be one of "spi", "sip", or "psi"
  expect_error(
    scrapeVarCross(
      dat = commitmentQ, x_stem = "sat.g", x_order = "invalid",
      x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
    ),
    "The `x_order` argument must be one of: 'spi', 'sip', or 'psi'."
  )
})

test_that("scrapeVarCross rejects invalid y_order value when provided", {
  # Test that y_order must be one of "spi", "sip", or "psi" when provided
  expect_error(
    scrapeVarCross(
      dat = commitmentQ, x_stem = "sat.g", y_order = "invalid", y_stem = "com"
    ),
    "The `y_order` argument must be one of: 'spi', 'sip', or 'psi'."
  )
})

test_that("scrapeVarCross rejects empty x_stem", {
  # Test that x_stem cannot be an empty string
  expect_error(
    scrapeVarCross(dat = commitmentQ, x_stem = ""),
    "The `x_stem` argument cannot be an empty string."
  )
})

test_that("scrapeVarCross validates x_stem even when var_list is provided", {
  # When var_list is supplied, x_stem is optional but must still be valid if provided
  dat_dummy <- data.frame(
    sat_1.1 = 1:5,
    sat_1.2 = 1:5
  )

  var_list <- list(
    lvnames = c("Sat"),
    stem = c("sat"),
    delim1 = c("_"),
    delim2 = c(".")
  )

  expect_error(
    scrapeVarCross(
      dat = dat_dummy,
      x_stem = 123,
      var_list = var_list,
      var_list_order = "sip",
      distinguish_1 = "1",
      distinguish_2 = "2"
    ),
    "The `x_stem` argument must be a character string."
  )

  expect_error(
    scrapeVarCross(
      dat = dat_dummy,
      x_stem = "",
      var_list = var_list,
      var_list_order = "sip",
      distinguish_1 = "1",
      distinguish_2 = "2"
    ),
    "The `x_stem` argument cannot be an empty string."
  )
})

test_that("scrapeVarCross handles data with unequal items per partner", {
  # Create a test dataset with unequal items
  # This tests the existing error handling for mismatched partner variable counts
  # For spi order: stem.partner_item, so "var" with spi finds var.1_1, var.2_1, var.1_2
  # Need to create data where partners have different numbers of matching variables
  unequal_data <- data.frame(
    var.1_1 = 1:10,
    var.2_1 = 1:10,
    var.1_2 = 1:10,
    var.2_2 = 1:10,
    var.1_3 = 1:10
    # Partner 1 has 3 items, partner 2 has 2 items
  )
  # This should trigger the existing error about unequal items
  expect_error(
    scrapeVarCross(
      dat = unequal_data, x_order = "spi", x_stem = "var",
      x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
      verbose = FALSE
    ),
    "cannot detect a similar number of"
  )
})

#### Group 4: Malformed dvn Objects ####
# --------------------------------------
# Note: scrapeVarCross creates dvn objects, so these tests focus on
# scenarios that would result in malformed output or errors during creation.
# These tests verify that the function handles edge cases gracefully.

test_that("scrapeVarCross handles data with no matching variables gracefully", {
  # Test that function handles case where no variables match the pattern
  # Currently returns empty dvn object with 0 indicators
  result <- scrapeVarCross(
    dat = commitmentQ, x_stem = "nonexistent_variable_pattern_xyz",
    x_order = "spi", x_delim1 = ".", x_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )
  expect_equal(result$indnum, 0)
  expect_equal(length(result$p1xvarnames), 0)
  expect_equal(length(result$p2xvarnames), 0)
})

#### Group 5: Multi-LV var_list behaviour ####
# --------------------------------------------

test_that("scrapeVarCross correctly handles multi-LV var_list with sip order", {
  dat_multi <- data.frame(
    sat_1.1 = 1:5,
    sat_2.1 = 1:5,
    sat_1.2 = 1:5,
    sat_2.2 = 1:5,
    com_1.1 = 1:5,
    com_2.1 = 1:5,
    com_1.2 = 1:5,
    com_2.2 = 1:5
  )

  var_list <- list(
    lvnames = c("Sat", "Comm"),
    stem = c("sat", "com"),
    delim1 = c("_", "_"),
    delim2 = c(".", ".")
  )

  dvn <- scrapeVarCross(
    dat = dat_multi,
    x_stem = "sat",
    var_list = var_list,
    var_list_order = "sip",
    distinguish_1 = "1",
    distinguish_2 = "2",
    verbose = FALSE
  )

  expect_equal(names(dvn$p1xvarnames), c("Sat", "Comm"))
  expect_equal(names(dvn$p2xvarnames), c("Sat", "Comm"))
  expect_equal(dvn$p1xvarnames$Sat, c("sat_1.1", "sat_2.1"))
  expect_equal(dvn$p2xvarnames$Sat, c("sat_1.2", "sat_2.2"))
  expect_equal(dvn$p1xvarnames$Comm, c("com_1.1", "com_2.1"))
  expect_equal(dvn$p2xvarnames$Comm, c("com_1.2", "com_2.2"))
  expect_equal(dvn$xindper, 4L)
  expect_equal(dvn$indnum, 8L)
})

test_that("scrapeVarCross respects min_num and max_num in var_list", {
  dat_multi <- data.frame(
    sat_1.1 = 1:5,
    sat_2.1 = 1:5,
    sat_3.1 = 1:5,
    sat_1.2 = 1:5,
    sat_2.2 = 1:5,
    sat_3.2 = 1:5,
    com_1.1 = 1:5,
    com_2.1 = 1:5,
    com_3.1 = 1:5,
    com_1.2 = 1:5,
    com_2.2 = 1:5,
    com_3.2 = 1:5
  )

  var_list <- list(
    lvnames = c("Sat", "Comm"),
    stem = c("sat", "com"),
    delim1 = c("_", "_"),
    delim2 = c(".", "."),
    min_num = c(1L, 2L),
    max_num = c(2L, 3L)
  )

  dvn <- scrapeVarCross(
    dat = dat_multi,
    x_stem = "sat",
    var_list = var_list,
    var_list_order = "sip",
    distinguish_1 = "1",
    distinguish_2 = "2",
    verbose = FALSE
  )

  # Sat keeps items 1-2; Comm keeps items 2-3
  expect_equal(dvn$p1xvarnames$Sat, c("sat_1.1", "sat_2.1"))
  expect_equal(dvn$p2xvarnames$Sat, c("sat_1.2", "sat_2.2"))
  expect_equal(dvn$p1xvarnames$Comm, c("com_2.1", "com_3.1"))
  expect_equal(dvn$p2xvarnames$Comm, c("com_2.2", "com_3.2"))
  expect_equal(dvn$xindper, 4L)
  expect_equal(dvn$indnum, 8L)
})

test_that("scrapeVarCross errors when var_list leads to unequal items per partner", {
  dat_multi <- data.frame(
    sat_1.1 = 1:5,
    sat_2.1 = 1:5,
    sat_1.2 = 1:5,
    # sat_2.2 missing -> unequal counts
    com_1.1 = 1:5,
    com_2.1 = 1:5,
    com_1.2 = 1:5,
    com_2.2 = 1:5
  )

  var_list <- list(
    lvnames = c("Sat", "Comm"),
    stem = c("sat", "com"),
    delim1 = c("_", "_"),
    delim2 = c(".", ".")
  )

  expect_error(
    scrapeVarCross(
      dat = dat_multi,
      x_stem = "sat",
      var_list = var_list,
      var_list_order = "sip",
      distinguish_1 = "1",
      distinguish_2 = "2",
      verbose = FALSE
    ),
    "scrapeVarCross\\(\\) cannot detect a similar number of"
  )
})

test_that("scrapeVarCross correctly handles multi-LV var_list with spi order", {
  dat_multi_spi <- data.frame(
    sat.1_1 = 1:5,
    sat.1_2 = 1:5,
    sat.2_1 = 1:5,
    sat.2_2 = 1:5,
    com.1_1 = 1:5,
    com.1_2 = 1:5,
    com.2_1 = 1:5,
    com.2_2 = 1:5
  )

  var_list_spi <- list(
    lvnames = c("Sat", "Comm"),
    stem = c("sat", "com"),
    delim1 = c(".", "."),
    delim2 = c("_", "_")
  )

  dvn_spi <- scrapeVarCross(
    dat = dat_multi_spi,
    x_stem = "sat",
    var_list = var_list_spi,
    var_list_order = "spi",
    distinguish_1 = "1",
    distinguish_2 = "2",
    verbose = FALSE
  )

  expect_equal(names(dvn_spi$p1xvarnames), c("Sat", "Comm"))
  expect_equal(names(dvn_spi$p2xvarnames), c("Sat", "Comm"))
  expect_equal(dvn_spi$p1xvarnames$Sat, c("sat.1_1", "sat.1_2"))
  expect_equal(dvn_spi$p2xvarnames$Sat, c("sat.2_1", "sat.2_2"))
  expect_equal(dvn_spi$p1xvarnames$Comm, c("com.1_1", "com.1_2"))
  expect_equal(dvn_spi$p2xvarnames$Comm, c("com.2_1", "com.2_2"))
  expect_equal(dvn_spi$xindper, 4L)
  expect_equal(dvn_spi$indnum, 8L)
})

test_that("scrapeVarCross respects min_num and max_num in var_list for spi order", {
  dat_multi_spi <- data.frame(
    sat.1_1 = 1:5,
    sat.1_2 = 1:5,
    sat.1_3 = 1:5,
    sat.2_1 = 1:5,
    sat.2_2 = 1:5,
    sat.2_3 = 1:5,
    com.1_1 = 1:5,
    com.1_2 = 1:5,
    com.1_3 = 1:5,
    com.2_1 = 1:5,
    com.2_2 = 1:5,
    com.2_3 = 1:5
  )

  var_list_spi <- list(
    lvnames = c("Sat", "Comm"),
    stem = c("sat", "com"),
    delim1 = c(".", "."),
    delim2 = c("_", "_"),
    min_num = c(1L, 2L),
    max_num = c(2L, 3L)
  )

  dvn_spi <- scrapeVarCross(
    dat = dat_multi_spi,
    x_stem = "sat",
    var_list = var_list_spi,
    var_list_order = "spi",
    distinguish_1 = "1",
    distinguish_2 = "2",
    verbose = FALSE
  )

  # Sat keeps items 1-2; Comm keeps items 2-3
  expect_equal(dvn_spi$p1xvarnames$Sat, c("sat.1_1", "sat.1_2"))
  expect_equal(dvn_spi$p2xvarnames$Sat, c("sat.2_1", "sat.2_2"))
  expect_equal(dvn_spi$p1xvarnames$Comm, c("com.1_2", "com.1_3"))
  expect_equal(dvn_spi$p2xvarnames$Comm, c("com.2_2", "com.2_3"))
  expect_equal(dvn_spi$xindper, 4L)
  expect_equal(dvn_spi$indnum, 8L)
})

test_that("scrapeVarCross correctly handles multi-LV var_list with psi order", {
  dat_multi_psi <- data.frame(
    `1_sat_1` = 1:5,
    `1_sat_2` = 1:5,
    `2_sat_1` = 1:5,
    `2_sat_2` = 1:5,
    `1_com_1` = 1:5,
    `1_com_2` = 1:5,
    `2_com_1` = 1:5,
    `2_com_2` = 1:5,
    check.names = FALSE
  )

  var_list_psi <- list(
    lvnames = c("Sat", "Comm"),
    stem = c("sat", "com"),
    delim1 = c("_", "_"),
    delim2 = c("_", "_")
  )

  dvn_psi <- scrapeVarCross(
    dat = dat_multi_psi,
    x_stem = "sat",
    var_list = var_list_psi,
    var_list_order = "psi",
    distinguish_1 = "1",
    distinguish_2 = "2",
    verbose = FALSE
  )

  expect_equal(names(dvn_psi$p1xvarnames), c("Sat", "Comm"))
  expect_equal(names(dvn_psi$p2xvarnames), c("Sat", "Comm"))
  expect_equal(dvn_psi$p1xvarnames$Sat, c("1_sat_1", "1_sat_2"))
  expect_equal(dvn_psi$p2xvarnames$Sat, c("2_sat_1", "2_sat_2"))
  expect_equal(dvn_psi$p1xvarnames$Comm, c("1_com_1", "1_com_2"))
  expect_equal(dvn_psi$p2xvarnames$Comm, c("2_com_1", "2_com_2"))
  expect_equal(dvn_psi$xindper, 4L)
  expect_equal(dvn_psi$indnum, 8L)
})

#### Group 6: Verbose argument behaviour ####
# ------------------------------------------

test_that("scrapeVarCross returns a dvn object when verbose = TRUE (single LV)", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2 = "_",
    distinguish_1 = "1",
    distinguish_2 = "2",
    verbose = TRUE
  )
  expect_true(is.list(dvn))
  expect_equal(dvn$indnum, 10L)
})

test_that("scrapeVarCross returns a dvn object when verbose = TRUE (multi-LV var_list)", {
  dat_multi <- data.frame(
    sat_1.1 = 1:5,
    sat_2.1 = 1:5,
    sat_1.2 = 1:5,
    sat_2.2 = 1:5,
    com_1.1 = 1:5,
    com_2.1 = 1:5,
    com_1.2 = 1:5,
    com_2.2 = 1:5
  )

  var_list <- list(
    lvnames = c("Sat", "Comm"),
    stem = c("sat", "com"),
    delim1 = c("_", "_"),
    delim2 = c(".", ".")
  )

  dvn <- scrapeVarCross(
    dat = dat_multi,
    x_stem = "sat",
    var_list = var_list,
    var_list_order = "sip",
    distinguish_1 = "1",
    distinguish_2 = "2",
    verbose = TRUE
  )

  expect_true(is.list(dvn))
  expect_equal(names(dvn$p1xvarnames), c("Sat", "Comm"))
})
