#### loads ####
##### partner = 1 #####

test_that("loads produces correct output for lvar = X, partner = 1, type = free", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    loads(dvn, lvar = "X", lvname = "Sat", partner = "1", type = "free"),
    "Sat1=~NA*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5"
  )
})

test_that("loads produces correct output for lvar = Y, partner = 1, type = free", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    loads(dvn, lvar = "Y", lvname = "Com", partner = "1", type = "free"),
    "Com1=~NA*com.1_1+com.1_2+com.1_3+com.1_4+com.1_5"
  )
})

test_that("loads produces correct output for lvar = X, partner = 1, type = fixed", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    loads(dvn, lvar = "X", lvname = "Sat", partner = "1", type = "fixed"),
    "Sat1=~1*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5"
  )
})

test_that("loads produces correct output for lvar = Y, partner = 1, type = fixed", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    loads(dvn, lvar = "Y", lvname = "Com", partner = "1", type = "fixed"),
    "Com1=~1*com.1_1+com.1_2+com.1_3+com.1_4+com.1_5"
  )
})

test_that("loads produces correct output for lvar = X, partner = 1, type = equated", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    loads(dvn, lvar = "X", lvname = "Sat", partner = "1", type = "equated"),
    "Sat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5"
  )
})

test_that("loads produces correct output for lvar = Y, partner = 1, type = equated", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    loads(dvn, lvar = "Y", lvname = "Com", partner = "1", type = "equated"),
    "Com1=~NA*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5"
  )
})

test_that("loads produces correct output for lvar = X, partner = 1, type = equated_mv", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    loads(dvn, lvar = "X", lvname = "Sat", partner = "1", type = "equated_mv"),
    "Sat1=~1*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5"
  )
})

test_that("loads produces correct output for lvar = Y, partner = 1, type = equated_mv", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    loads(dvn, lvar = "Y", lvname = "Com", partner = "1", type = "equated_mv"),
    "Com1=~1*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5"
  )
})

test_that("loads produces correct output for lvar = X, partner = 1, type = equated_source", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    loads(dvn, lvar = "X", lvname = "Sat", partner = "1", type = "equated_source"),
    "Sat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5"
  )
})

test_that("loads produces correct output for lvar = Y, partner = 1, type = equated_source", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    loads(dvn, lvar = "Y", lvname = "Com", partner = "1", type = "equated_source"),
    "Com1=~NA*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5"
  )
})

##### partner = 2 #####
test_that("loads produces correct output for lvar = X, partner = 2, type = free", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    loads(dvn, lvar = "X", lvname = "Sat", partner = "2", type = "free"),
    "Sat2=~NA*sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5"
  )
})

test_that("loads produces correct output for lvar = Y, partner = 2, type = free", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    loads(dvn, lvar = "Y", lvname = "Com", partner = "2", type = "free"),
    "Com2=~NA*com.2_1+com.2_2+com.2_3+com.2_4+com.2_5"
  )
})

test_that("loads produces correct output for lvar = X, partner = 2, type = fixed", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    loads(dvn, lvar = "X", lvname = "Sat", partner = "2", type = "fixed"),
    "Sat2=~1*sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5"
  )
})

test_that("loads produces correct output for lvar = Y, partner = 2, type = fixed", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    loads(dvn, lvar = "Y", lvname = "Com", partner = "2", type = "fixed"),
    "Com2=~1*com.2_1+com.2_2+com.2_3+com.2_4+com.2_5"
  )
})

test_that("loads produces correct output for lvar = X, partner = 2, type = equated", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    loads(dvn, lvar = "X", lvname = "Sat", partner = "2", type = "equated"),
    "Sat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5"
  )
})

test_that("loads produces correct output for lvar = Y, partner = 2, type = equated", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    loads(dvn, lvar = "Y", lvname = "Com", partner = "2", type = "equated"),
    "Com2=~NA*com.2_1+ly1*com.2_1+ly2*com.2_2+ly3*com.2_3+ly4*com.2_4+ly5*com.2_5"
  )
})

test_that("loads produces correct output for lvar = X, partner = 2, type = equated_mv", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    loads(dvn, lvar = "X", lvname = "Sat", partner = "2", type = "equated_mv"),
    "Sat2=~1*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5"
  )
})

test_that("loads produces correct output for lvar = Y, partner = 2, type = equated_mv", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    loads(dvn, lvar = "Y", lvname = "Com", partner = "2", type = "equated_mv"),
    "Com2=~1*com.2_1+ly1*com.2_1+ly2*com.2_2+ly3*com.2_3+ly4*com.2_4+ly5*com.2_5"
  )
})

test_that("loads produces correct output for lvar = X, partner = 2, type = equated_source", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    loads(dvn, lvar = "X", lvname = "Sat", partner = "2", type = "equated_source"),
    "Sat2=~NA*sat.g.2_1+lx6*sat.g.2_1+lx7*sat.g.2_2+lx8*sat.g.2_3+lx9*sat.g.2_4+lx10*sat.g.2_5"
  )
})

test_that("loads produces correct output for lvar = Y, partner = 2, type = equated_source", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    loads(dvn, lvar = "Y", lvname = "Com", partner = "2", type = "equated_source"),
    "Com2=~NA*com.2_1+ly6*com.2_1+ly7*com.2_2+ly8*com.2_3+ly9*com.2_4+ly10*com.2_5"
  )
})

##### partner = g #####

test_that("loads produces correct output for lvar = X, partner = g, type = free", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    loads(dvn, lvar = "X", lvname = "Sat", partner = "g", type = "free"),
    "SatDy=~NA*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5+sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5"
  )
})

test_that("loads produces correct output for lvar = Y, partner = g, type = free", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    loads(dvn, lvar = "Y", lvname = "Com", partner = "g", type = "free"),
    "ComDy=~NA*com.1_1+com.1_2+com.1_3+com.1_4+com.1_5+com.2_1+com.2_2+com.2_3+com.2_4+com.2_5"
  )
})


test_that("loads produces correct output for lvar = X, partner = g, type = equated_source", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    loads(dvn, lvar = "X", lvname = "Sat", partner = "g", type = "equated_source"),
    "SatDy=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5+lx6*sat.g.2_1+lx7*sat.g.2_2+lx8*sat.g.2_3+lx9*sat.g.2_4+lx10*sat.g.2_5"
  )
})

test_that("loads produces correct output for lvar = Y, partner = g, type = equated_source", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    loads(dvn, lvar = "Y", lvname = "Com", partner = "g", type = "equated_source"),
    "ComDy=~NA*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5+ly6*com.2_1+ly7*com.2_2+ly8*com.2_3+ly9*com.2_4+ly10*com.2_5"
  )
})

##### multi-factor loadings ####

# Full exemplar dvn list
dvn <- list(
  p1xvarnames = list(
    Sat = c("sat_1.1", "sat_2.1", "sat_3.1"),
    Comm = c("com_1.1", "com_2.1", "com_3.1"),
    Intim = c("int_1.1", "int_2.1", "int_3.1"),
    Trust = c("trust_1.1", "trust_2.1", "trust_3.1"),
    Pass = c("pass_1.1", "pass_2.1", "pass_3.1"),
    Love = c("love_1.1", "love_2.1", "love_3.1")
  ),
  p2xvarnames = list(
    Sat = c("sat_1.2", "sat_2.2", "sat_3.2"),
    Comm = c("com_1.2", "com_2.2", "com_3.2"),
    Intim = c("int_1.2", "int_2.2", "int_3.2"),
    Trust = c("trust_1.2", "trust_2.2", "trust_3.2"),
    Pass = c("pass_1.2", "pass_2.2", "pass_3.2"),
    Love = c("love_1.2", "love_2.2", "love_3.2")
  ),
  xindper = 18,
  dist1 = "1",
  dist2 = "2",
  indnum = 36
)

test_that("Correct output for partner 1, type = 'free'", {
  result <- multifac_loads(dvn, partner = "1", type = "free")
  expect_equal(result[1], "Sat1 =~ NA*sat_1.1 + lx1*sat_1.1 + lx2*sat_2.1 + lx3*sat_3.1")
  expect_equal(result[2], "Comm1 =~ NA*com_1.1 + lx4*com_1.1 + lx5*com_2.1 + lx6*com_3.1")
  expect_equal(length(result), 6) # There are 6 latent variables
})

test_that("Correct output for partner 2, type = 'fixed'", {
  result <- multifac_loads(dvn, partner = "2", type = "fixed")
  expect_equal(result[1], "Sat2 =~ 1*sat_1.2 + lx19*sat_1.2 + lx20*sat_2.2 + lx21*sat_3.2")
  expect_equal(result[2], "Comm2 =~ 1*com_1.2 + lx22*com_1.2 + lx23*com_2.2 + lx24*com_3.2")
  expect_equal(length(result), 6) # There are 6 latent variables
})

test_that("Correct output for partner 2, type = 'equated'", {
  result <- multifac_loads(dvn, partner = "2", type = "equated")
  expect_equal(result[1], "Sat2 =~ NA*sat_1.2 + lx1*sat_1.2 + lx2*sat_2.2 + lx3*sat_3.2")
  expect_equal(result[2], "Comm2 =~ NA*com_1.2 + lx4*com_1.2 + lx5*com_2.2 + lx6*com_3.2")
  expect_equal(length(result), 6) # There are 6 latent variables
})

test_that("Error on invalid partner", {
  expect_error(multifac_loads(dvn, partner = "3"), "Invalid partner argument. Use '1' or '2'.")
})

test_that("Handles invalid type gracefully", {
  expect_error(multifac_loads(dvn, partner = "1", type = "unknown"), "Invalid type argument. Use 'free', 'fixed', or 'equated'.")
})

test_that("multifac_loads rejects unsupported type equated_mv", {
  expect_error(
    multifac_loads(dvn, partner = "1", type = "equated_mv"),
    "Invalid type argument. Use 'free', 'fixed', or 'equated'."
  )
})

test_that("Handles empty variable names", {
  dvn_empty <- list(p1xvarnames = list(), p2xvarnames = list(), xindper = 18, indnum = 36)
  result <- multifac_loads(dvn_empty, partner = "1", type = "free")
  expect_equal(result, character(0)) # Should return an empty character vector
})

test_that("Correct label indexing for partner 1 and partner 2", {
  result1 <- multifac_loads(dvn, partner = "1", type = "free")
  result2 <- multifac_loads(dvn, partner = "2", type = "free")

  # Extract labels using a regex pattern to ensure proper handling
  extract_labels <- function(result) {
    unlist(lapply(strsplit(result, "\\+"), function(x) {
      matches <- regmatches(x, regexpr("lx\\d+", x))
      if (length(matches) > 0) matches else NA
    }))
  }

  labels1 <- extract_labels(result1)
  labels2 <- extract_labels(result2)

  # Verify no NAs in label extraction
  expect_true(!any(is.na(labels1)))
  expect_true(!any(is.na(labels2)))

  # Convert labels to numeric
  labels1_numeric <- as.numeric(gsub("lx", "", labels1))
  labels2_numeric <- as.numeric(gsub("lx", "", labels2))

  # Verify label ranges do not overlap
  expect_true(max(labels1_numeric) < min(labels2_numeric))
})

#### intercepts ####
##### partner = 1 #####

test_that("intercepts produces correct output for lvar = X, partner = 1, type = free", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    intercepts(dvn, lvar = "X", partner = "1", type = "free"),
    "sat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1"
  )
})

test_that("intercepts produces correct output for lvar = Y, partner = 1, type = free", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    intercepts(dvn, lvar = "Y", partner = "1", type = "free"),
    "com.1_1 ~ 1\ncom.1_2 ~ 1\ncom.1_3 ~ 1\ncom.1_4 ~ 1\ncom.1_5 ~ 1"
  )
})

test_that("intercepts produces correct output for lvar = X, partner = 1, type = fixed", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    intercepts(dvn, lvar = "X", partner = "1", type = "fixed"),
    "sat.g.1_1 ~ 0*1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1"
  )
})

test_that("intercepts produces correct output for lvar = Y, partner = 1, type = fixed", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    intercepts(dvn, lvar = "Y", partner = "1", type = "fixed"),
    "com.1_1 ~ 0*1\ncom.1_2 ~ 1\ncom.1_3 ~ 1\ncom.1_4 ~ 1\ncom.1_5 ~ 1"
  )
})

test_that("intercepts produces correct output for lvar = X, partner = 1, type = equated", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    intercepts(dvn, lvar = "X", partner = "1", type = "equated"),
    "sat.g.1_1 ~ tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1"
  )
})

test_that("intercepts produces correct output for lvar = Y, partner = 1, type = equated", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    intercepts(dvn, lvar = "Y", partner = "1", type = "equated"),
    "com.1_1 ~ ty1*1\ncom.1_2 ~ ty2*1\ncom.1_3 ~ ty3*1\ncom.1_4 ~ ty4*1\ncom.1_5 ~ ty5*1"
  )
})

test_that("intercepts produces correct output for lvar = X, partner = 1, type = equated_mv", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    intercepts(dvn, lvar = "X", partner = "1", type = "equated_mv"),
    "sat.g.1_1 ~ 0*1 + tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1"
  )
})

test_that("intercepts produces correct output for lvar = Y, partner = 1, type = equated_mv", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    intercepts(dvn, lvar = "Y", partner = "1", type = "equated_mv"),
    "com.1_1 ~ 0*1 + ty1*1\ncom.1_2 ~ ty2*1\ncom.1_3 ~ ty3*1\ncom.1_4 ~ ty4*1\ncom.1_5 ~ ty5*1"
  )
})

##### partner = 2 #####
test_that("intercepts produces correct output for lvar = X, partner = 2, type = free", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    intercepts(dvn, lvar = "X", partner = "2", type = "free"),
    "sat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1"
  )
})

test_that("intercepts produces correct output for lvar = Y, partner = 2, type = free", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    intercepts(dvn, lvar = "Y", partner = "2", type = "free"),
    "com.2_1 ~ 1\ncom.2_2 ~ 1\ncom.2_3 ~ 1\ncom.2_4 ~ 1\ncom.2_5 ~ 1"
  )
})

test_that("intercepts produces correct output for lvar = X, partner = 2, type = fixed", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    intercepts(dvn, lvar = "X", partner = "2", type = "fixed"),
    "sat.g.2_1 ~ 0*1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1"
  )
})

test_that("intercepts produces correct output for lvar = Y, partner = 2, type = fixed", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    intercepts(dvn, lvar = "Y", partner = "2", type = "fixed"),
    "com.2_1 ~ 0*1\ncom.2_2 ~ 1\ncom.2_3 ~ 1\ncom.2_4 ~ 1\ncom.2_5 ~ 1"
  )
})

test_that("intercepts produces correct output for lvar = X, partner = 2, type = equated", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    intercepts(dvn, lvar = "X", partner = "2", type = "equated"),
    "sat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1"
  )
})

test_that("intercepts produces correct output for lvar = Y, partner = 1, type = equated", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    intercepts(dvn, lvar = "Y", partner = "2", type = "equated"),
    "com.2_1 ~ ty1*1\ncom.2_2 ~ ty2*1\ncom.2_3 ~ ty3*1\ncom.2_4 ~ ty4*1\ncom.2_5 ~ ty5*1"
  )
})

test_that("intercepts produces correct output for lvar = X, partner = 1, type = equated_mv", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    intercepts(dvn, lvar = "X", partner = "2", type = "equated_mv"),
    "sat.g.2_1 ~ 0*1 + tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1"
  )
})

test_that("intercepts produces correct output for lvar = Y, partner = 2, type = equated_mv", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    intercepts(dvn, lvar = "Y", partner = "2", type = "equated_mv"),
    "com.2_1 ~ 0*1 + ty1*1\ncom.2_2 ~ ty2*1\ncom.2_3 ~ ty3*1\ncom.2_4 ~ ty4*1\ncom.2_5 ~ ty5*1"
  )
})

##### multi-factor intercepts####

# Full exemplar dvn list
dvn <- list(
  p1xvarnames = list(
    Sat = c("sat_1.1", "sat_2.1", "sat_3.1"),
    Comm = c("com_1.1", "com_2.1", "com_3.1"),
    Intim = c("int_1.1", "int_2.1", "int_3.1"),
    Trust = c("trust_1.1", "trust_2.1", "trust_3.1"),
    Pass = c("pass_1.1", "pass_2.1", "pass_3.1"),
    Love = c("love_1.1", "love_2.1", "love_3.1")
  ),
  p2xvarnames = list(
    Sat = c("sat_1.2", "sat_2.2", "sat_3.2"),
    Comm = c("com_1.2", "com_2.2", "com_3.2"),
    Intim = c("int_1.2", "int_2.2", "int_3.2"),
    Trust = c("trust_1.2", "trust_2.2", "trust_3.2"),
    Pass = c("pass_1.2", "pass_2.2", "pass_3.2"),
    Love = c("love_1.2", "love_2.2", "love_3.2")
  ),
  xindper = 18,
  dist1 = "1",
  dist2 = "2",
  indnum = 36
)

test_that("Correct output for partner 1, type = 'free'", {
  result <- multifac_intercepts(dvn, partner = "1", type = "free")
  expect_equal(result[1], "sat_1.1 ~ t1*1")
  expect_equal(result[2], "sat_2.1 ~ t2*1")
  expect_equal(result[3], "sat_3.1 ~ t3*1")
  expect_equal(result[length(result)], "love_3.1 ~ t18*1") # Check last label
})

test_that("Correct output for partner 2, type = 'free'", {
  result <- multifac_intercepts(dvn, partner = "2", type = "free")
  expect_equal(result[1], "sat_1.2 ~ t19*1")
  expect_equal(result[2], "sat_2.2 ~ t20*1")
  expect_equal(result[3], "sat_3.2 ~ t21*1")
  expect_equal(result[length(result)], "love_3.2 ~ t36*1") # Check last label
})

test_that("Correct output for partner 1, type = 'fixed'", {
  result <- multifac_intercepts(dvn, partner = "1", type = "fixed")
  expect_equal(result[1], "sat_1.1 ~ 0*1")
  expect_equal(result[2], "sat_1.1 ~ t1*1")
  expect_equal(result[3], "sat_2.1 ~ t2*1")
  expect_equal(result[4], "sat_3.1 ~ t3*1")
})

test_that("Correct output for partner 2, type = 'fixed'", {
  result <- multifac_intercepts(dvn, partner = "2", type = "fixed")
  expect_equal(result[1], "sat_1.2 ~ 0*1")
  expect_equal(result[2], "sat_1.2 ~ t19*1")
  expect_equal(result[3], "sat_2.2 ~ t20*1")
  expect_equal(result[4], "sat_3.2 ~ t21*1")
})

test_that("Correct output for partner 2, type = 'equated'", {
  result <- multifac_intercepts(dvn, partner = "2", type = "equated")
  expect_equal(result[1], "sat_1.2 ~ t1*1")
  expect_equal(result[2], "sat_2.2 ~ t2*1")
  expect_equal(result[3], "sat_3.2 ~ t3*1")
  expect_equal(result[length(result)], "love_3.2 ~ t18*1") # Check last label
})

test_that("Handles empty variable names", {
  dvn_empty <- list(p1xvarnames = list(), p2xvarnames = list(), xindper = 18, indnum = 36)
  result <- multifac_intercepts(dvn_empty, partner = "1", type = "free")
  expect_equal(result, character(0)) # Should return an empty character vector
})

test_that("Error on invalid partner", {
  expect_error(multifac_intercepts(dvn, partner = "3", type = "free"), "Invalid partner argument. Use '1' or '2'.")
})

test_that("Error on invalid type", {
  expect_error(multifac_intercepts(dvn, partner = "1", type = "unknown"), "Invalid type argument. Use 'free', 'fixed', or 'equated'.")
})

#### resids ####
#### multi-factor residuals####
# Full exemplar dvn list
dvn <- list(
  p1xvarnames = list(
    Sat = c("sat_1.1", "sat_2.1", "sat_3.1"),
    Comm = c("com_1.1", "com_2.1", "com_3.1"),
    Intim = c("int_1.1", "int_2.1", "int_3.1"),
    Trust = c("trust_1.1", "trust_2.1", "trust_3.1"),
    Pass = c("pass_1.1", "pass_2.1", "pass_3.1"),
    Love = c("love_1.1", "love_2.1", "love_3.1")
  ),
  p2xvarnames = list(
    Sat = c("sat_1.2", "sat_2.2", "sat_3.2"),
    Comm = c("com_1.2", "com_2.2", "com_3.2"),
    Intim = c("int_1.2", "int_2.2", "int_3.2"),
    Trust = c("trust_1.2", "trust_2.2", "trust_3.2"),
    Pass = c("pass_1.2", "pass_2.2", "pass_3.2"),
    Love = c("love_1.2", "love_2.2", "love_3.2")
  ),
  xindper = 18,
  dist1 = "1",
  dist2 = "2",
  indnum = 36
)

test_that("Correct output for partner 1, type = 'free'", {
  result <- multifac_resids(dvn, partner = "1", type = "free")
  expect_equal(result[1], "sat_1.1 ~~ th1*sat_1.1")
  expect_equal(result[2], "sat_2.1 ~~ th2*sat_2.1")
  expect_equal(result[3], "sat_3.1 ~~ th3*sat_3.1")
  expect_equal(result[length(result)], "love_3.1 ~~ th18*love_3.1") # Check last label
})

test_that("Correct output for partner 2, type = 'free'", {
  result <- multifac_resids(dvn, partner = "2", type = "free")
  expect_equal(result[1], "sat_1.2 ~~ th19*sat_1.2")
  expect_equal(result[2], "sat_2.2 ~~ th20*sat_2.2")
  expect_equal(result[3], "sat_3.2 ~~ th21*sat_3.2")
  expect_equal(result[length(result)], "love_3.2 ~~ th36*love_3.2") # Check last label
})

test_that("Correct output for partner 2, type = 'equated'", {
  result <- multifac_resids(dvn, partner = "2", type = "equated")
  expect_equal(result[1], "sat_1.2 ~~ th1*sat_1.2")
  expect_equal(result[2], "sat_2.2 ~~ th2*sat_2.2")
  expect_equal(result[3], "sat_3.2 ~~ th3*sat_3.2")
  expect_equal(result[length(result)], "love_3.2 ~~ th18*love_3.2") # Check last label
})

test_that("Handles empty variable names", {
  dvn_empty <- list(p1xvarnames = list(), p2xvarnames = list(), xindper = 18, indnum = 36)
  result <- multifac_resids(dvn_empty, partner = "1", type = "free")
  expect_equal(result, character(0)) # Should return an empty character vector
})

test_that("Error on invalid partner", {
  expect_error(multifac_resids(dvn, partner = "3", type = "free"), "Invalid partner argument. Use '1' or '2'.")
})

test_that("Error on invalid type", {
  expect_error(multifac_resids(dvn, partner = "1", type = "unknown"), "Invalid type argument. Use 'free' or 'equated'.")
})

#### multi factor correlated residuals ####
# devtools::load_all()
# Full exemplar dvn list
dvn <- list(
  p1xvarnames = list(
    Sat = c("sat_1.1", "sat_2.1", "sat_3.1"),
    Comm = c("com_1.1", "com_2.1", "com_3.1"),
    Intim = c("int_1.1", "int_2.1", "int_3.1"),
    Trust = c("trust_1.1", "trust_2.1", "trust_3.1"),
    Pass = c("pass_1.1", "pass_2.1", "pass_3.1"),
    Love = c("love_1.1", "love_2.1", "love_3.1")
  ),
  p2xvarnames = list(
    Sat = c("sat_1.2", "sat_2.2", "sat_3.2"),
    Comm = c("com_1.2", "com_2.2", "com_3.2"),
    Intim = c("int_1.2", "int_2.2", "int_3.2"),
    Trust = c("trust_1.2", "trust_2.2", "trust_3.2"),
    Pass = c("pass_1.2", "pass_2.2", "pass_3.2"),
    Love = c("love_1.2", "love_2.2", "love_3.2")
  ),
  xindper = 18,
  dist1 = "1",
  dist2 = "2",
  indnum = 36
)

test_that("Correct output for type = 'free'", {
  result <- multifac_coresids(dvn, type = "free")
  expect_equal(result[1], "sat_1.1 ~~ sat_1.2")
  expect_equal(result[2], "sat_2.1 ~~ sat_2.2")
  expect_equal(result[3], "sat_3.1 ~~ sat_3.2")
  expect_equal(result[length(result)], "love_3.1 ~~ love_3.2") # Check last pair
})

test_that("Correct output for type = 'zero'", {
  result <- multifac_coresids(dvn, type = "zero")
  expect_equal(result[1], "sat_1.1 ~~ 0*sat_1.2")
  expect_equal(result[2], "sat_2.1 ~~ 0*sat_2.2")
  expect_equal(result[3], "sat_3.1 ~~ 0*sat_3.2")
  expect_equal(result[length(result)], "love_3.1 ~~ 0*love_3.2") # Check last pair
})

test_that("Handles empty variable names", {
  dvn_empty <- list(p1xvarnames = list(), p2xvarnames = list(), xindper = 18, indnum = 36)
  result <- multifac_coresids(dvn_empty, type = "free")
  expect_equal(result, character(0)) # Should return an empty character vector
})

test_that("Error on invalid type", {
  expect_error(multifac_coresids(dvn, type = "unknown"), "Invalid type argument. Use 'free' or 'zero'.")
})

test_that("Error on mismatched indicators", {
  dvn_mismatch <- list(
    p1xvarnames = list(Sat = c("sat_1.1", "sat_2.1")),
    p2xvarnames = list(Sat = c("sat_1.2")),
    xindper = 18,
    indnum = 36
  )
  expect_error(multifac_coresids(dvn_mismatch, type = "free"), "Mismatch in the number of indicators for latent variable: Sat")
})

##### partner = 1 #####
test_that("resids produces correct output for lvar = X, partner = 1, type = free", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    resids(dvn, lvar = "X", partner = "1", type = "free"),
    "sat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5"
  )
})

test_that("resids produces correct output for lvar = Y, partner = 1, type = free", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    resids(dvn, lvar = "Y", partner = "1", type = "free"),
    "com.1_1 ~~ com.1_1\ncom.1_2 ~~ com.1_2\ncom.1_3 ~~ com.1_3\ncom.1_4 ~~ com.1_4\ncom.1_5 ~~ com.1_5"
  )
})

test_that("resids produces correct output for lvar = X, partner = 1, type = equated", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    resids(dvn, lvar = "X", partner = "1", type = "equated"),
    "sat.g.1_1 ~~ thx1*sat.g.1_1\nsat.g.1_2 ~~ thx2*sat.g.1_2\nsat.g.1_3 ~~ thx3*sat.g.1_3\nsat.g.1_4 ~~ thx4*sat.g.1_4\nsat.g.1_5 ~~ thx5*sat.g.1_5"
  )
})

test_that("resids produces correct output for lvar = Y, partner = 1, type = equated", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    resids(dvn, lvar = "Y", partner = "1", type = "equated"),
    "com.1_1 ~~ thy1*com.1_1\ncom.1_2 ~~ thy2*com.1_2\ncom.1_3 ~~ thy3*com.1_3\ncom.1_4 ~~ thy4*com.1_4\ncom.1_5 ~~ thy5*com.1_5"
  )
})

##### partner = 2 #####
test_that("resids produces correct output for lvar = X, partner = 2, type = free", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    resids(dvn, lvar = "X", partner = "2", type = "free"),
    "sat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5"
  )
})

test_that("resids produces correct output for lvar = Y, partner = 2, type = free", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    resids(dvn, lvar = "Y", partner = "2", type = "free"),
    "com.2_1 ~~ com.2_1\ncom.2_2 ~~ com.2_2\ncom.2_3 ~~ com.2_3\ncom.2_4 ~~ com.2_4\ncom.2_5 ~~ com.2_5"
  )
})

test_that("resids produces correct output for lvar = X, partner = 2, type = equated", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    resids(dvn, lvar = "X", partner = "2", type = "equated"),
    "sat.g.2_1 ~~ thx1*sat.g.2_1\nsat.g.2_2 ~~ thx2*sat.g.2_2\nsat.g.2_3 ~~ thx3*sat.g.2_3\nsat.g.2_4 ~~ thx4*sat.g.2_4\nsat.g.2_5 ~~ thx5*sat.g.2_5"
  )
})

test_that("resids produces correct output for lvar = Y, partner = 2, type = equated", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    resids(dvn, lvar = "Y", partner = "2", type = "equated"),
    "com.2_1 ~~ thy1*com.2_1\ncom.2_2 ~~ thy2*com.2_2\ncom.2_3 ~~ thy3*com.2_3\ncom.2_4 ~~ thy4*com.2_4\ncom.2_5 ~~ thy5*com.2_5"
  )
})

#### lvars ####
##### partner = 1 #####
test_that("lvars produces correct output for lvar = X, partner = 1, type = free", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    lvars(dvn, lvar = "X", lvname = "Sat", partner = "1", type = "free"),
    "Sat1 ~~ NA*Sat1"
  )
})

test_that("lvars produces correct output for lvar = Y, partner = 1, type = free", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    lvars(dvn, lvar = "Y", lvname = "Com", partner = "1", type = "free"),
    "Com1 ~~ NA*Com1"
  )
})

test_that("lvars produces correct output for lvar = X, partner = 1, type = fixed", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    lvars(dvn, lvar = "X", lvname = "Sat", partner = "1", type = "fixed"),
    "Sat1 ~~ 1*Sat1"
  )
})

test_that("lvars produces correct output for lvar = Y, partner = 1, type = fixed", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    lvars(dvn, lvar = "Y", lvname = "Com", partner = "1", type = "fixed"),
    "Com1 ~~ 1*Com1"
  )
})

test_that("lvars produces correct output for lvar = X, partner = 1, type = equated", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    lvars(dvn, lvar = "X", lvname = "Sat", partner = "1", type = "equated"),
    "Sat1 ~~ psix*Sat1"
  )
})

test_that("lvars produces correct output for lvar = Y, partner = 1, type = equated", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    lvars(dvn, lvar = "Y", lvname = "Com", partner = "1", type = "equated"),
    "Com1 ~~ psiy*Com1"
  )
})

##### partner = 2 #####
test_that("lvars produces correct output for lvar = X, partner = 2, type = free", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    lvars(dvn, lvar = "X", lvname = "Sat", partner = "2", type = "free"),
    "Sat2 ~~ NA*Sat2"
  )
})

test_that("lvars produces correct output for lvar = Y, partner = 2, type = free", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    lvars(dvn, lvar = "Y", lvname = "Com", partner = "2", type = "free"),
    "Com2 ~~ NA*Com2"
  )
})

test_that("lvars produces correct output for lvar = X, partner = 2, type = fixed", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    lvars(dvn, lvar = "X", lvname = "Sat", partner = "2", type = "fixed"),
    "Sat2 ~~ 1*Sat2"
  )
})

test_that("lvars produces correct output for lvar = Y, partner = 2, type = fixed", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    lvars(dvn, lvar = "Y", lvname = "Com", partner = "2", type = "fixed"),
    "Com2 ~~ 1*Com2"
  )
})

test_that("lvars produces correct output for lvar = X, partner = 2, type = equated", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    lvars(dvn, lvar = "X", lvname = "Sat", partner = "2", type = "equated"),
    "Sat2 ~~ psix*Sat2"
  )
})

test_that("lvars produces correct output for lvar = Y, partner = 2, type = equated", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    lvars(dvn, lvar = "Y", lvname = "Com", partner = "2", type = "equated"),
    "Com2 ~~ psiy*Com2"
  )
})

##### partner = g #####
test_that("lvars produces correct output for lvar = X, partner = g, type = free", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    lvars(dvn, lvar = "X", lvname = "Sat", partner = "g", type = "free"),
    "SatDy ~~ NA*SatDy"
  )
})

test_that("lvars produces correct output for lvar = X, partner = g, type = fixed", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    lvars(dvn, lvar = "X", lvname = "Sat", partner = "g", type = "fixed"),
    "SatDy ~~ 1*SatDy"
  )
})

##### multi-factor lvars ####

# Full exemplar dvn list
dvn <- list(
  p1xvarnames = list(
    Sat = c("sat_1.1", "sat_2.1", "sat_3.1"),
    Comm = c("com_1.1", "com_2.1", "com_3.1"),
    Intim = c("int_1.1", "int_2.1", "int_3.1"),
    Trust = c("trust_1.1", "trust_2.1", "trust_3.1"),
    Pass = c("pass_1.1", "pass_2.1", "pass_3.1"),
    Love = c("love_1.1", "love_2.1", "love_3.1")
  ),
  p2xvarnames = list(
    Sat = c("sat_1.2", "sat_2.2", "sat_3.2"),
    Comm = c("com_1.2", "com_2.2", "com_3.2"),
    Intim = c("int_1.2", "int_2.2", "int_3.2"),
    Trust = c("trust_1.2", "trust_2.2", "trust_3.2"),
    Pass = c("pass_1.2", "pass_2.2", "pass_3.2"),
    Love = c("love_1.2", "love_2.2", "love_3.2")
  ),
  xindper = 18,
  dist1 = "1",
  dist2 = "2",
  indnum = 36
)

test_that("Correct output for partner 1, type = 'free'", {
  result <- multifac_lvars(dvn, partner = "1", type = "free")
  expect_equal(result[1], "Sat1 ~~ psv1*Sat1 + NA*Sat1")
  expect_equal(result[2], "Comm1 ~~ psv2*Comm1 + NA*Comm1")
  expect_equal(result[3], "Intim1 ~~ psv3*Intim1 + NA*Intim1")
  expect_equal(result[length(result)], "Love1 ~~ psv6*Love1 + NA*Love1") # Check last latent variable
})

test_that("Correct output for partner 2, type = 'free'", {
  result <- multifac_lvars(dvn, partner = "2", type = "free")
  expect_equal(result[1], "Sat2 ~~ psv7*Sat2 + NA*Sat2")
  expect_equal(result[2], "Comm2 ~~ psv8*Comm2 + NA*Comm2")
  expect_equal(result[3], "Intim2 ~~ psv9*Intim2 + NA*Intim2")
  expect_equal(result[length(result)], "Love2 ~~ psv12*Love2 + NA*Love2") # Check last latent variable
})

test_that("Correct output for partner 1, type = 'constrain'", {
  result <- multifac_lvars(dvn, partner = "1", type = "constrain")
  expect_equal(result[1], "Sat1 ~~ psv1*Sat1 + 1*Sat1")
  expect_equal(result[2], "Comm1 ~~ psv2*Comm1 + 1*Comm1")
  expect_equal(result[3], "Intim1 ~~ psv3*Intim1 + 1*Intim1")
  expect_equal(result[length(result)], "Love1 ~~ psv6*Love1 + 1*Love1") # Check last latent variable
})

test_that("Correct output for partner 2, type = 'constrain'", {
  result <- multifac_lvars(dvn, partner = "2", type = "constrain")
  expect_equal(result[1], "Sat2 ~~ psv7*Sat2 + 1*Sat2")
  expect_equal(result[2], "Comm2 ~~ psv8*Comm2 + 1*Comm2")
  expect_equal(result[3], "Intim2 ~~ psv9*Intim2 + 1*Intim2")
  expect_equal(result[length(result)], "Love2 ~~ psv12*Love2 + 1*Love2") # Check last latent variable
})

test_that("Correct output for partner 2, type = 'equate'", {
  result <- multifac_lvars(dvn, partner = "2", type = "equate")
  expect_equal(result[1], "Sat2 ~~ psv1*Sat2 + NA*Sat2")
  expect_equal(result[2], "Comm2 ~~ psv2*Comm2 + NA*Comm2")
  expect_equal(result[3], "Intim2 ~~ psv3*Intim2 + NA*Intim2")
  expect_equal(result[length(result)], "Love2 ~~ psv6*Love2 + NA*Love2") # Check last latent variable
})

test_that("Handles empty variable names", {
  dvn_empty <- list(p1xvarnames = list(), p2xvarnames = list(), xindper = 18, indnum = 36)
  result <- multifac_lvars(dvn_empty, partner = "1", type = "free")
  expect_equal(result, character(0)) # Should return an empty character vector
})

test_that("Error on invalid partner", {
  expect_error(multifac_lvars(dvn, partner = "3", type = "free"), "Invalid partner argument. Use '1' or '2'.")
})

test_that("Error on invalid type", {
  expect_error(multifac_lvars(dvn, partner = "1", type = "unknown"), "Invalid type argument. Use 'free', 'constrain', or 'equate'.")
})

##### multi-factor lcovars####
# devtools::load_all()

# Full exemplar dvn list
dvn <- list(
  p1xvarnames = list(
    Sat = c("sat_1.1", "sat_2.1", "sat_3.1"),
    Comm = c("com_1.1", "com_2.1", "com_3.1"),
    Intim = c("int_1.1", "int_2.1", "int_3.1"),
    Trust = c("trust_1.1", "trust_2.1", "trust_3.1"),
    Pass = c("pass_1.1", "pass_2.1", "pass_3.1"),
    Love = c("love_1.1", "love_2.1", "love_3.1")
  ),
  p2xvarnames = list(
    Sat = c("sat_1.2", "sat_2.2", "sat_3.2"),
    Comm = c("com_1.2", "com_2.2", "com_3.2"),
    Intim = c("int_1.2", "int_2.2", "int_3.2"),
    Trust = c("trust_1.2", "trust_2.2", "trust_3.2"),
    Pass = c("pass_1.2", "pass_2.2", "pass_3.2"),
    Love = c("love_1.2", "love_2.2", "love_3.2")
  ),
  xindper = 18,
  dist1 = "1",
  dist2 = "2",
  indnum = 36
)

test_that("Correct output for type = 'free'", {
  result <- multifac_lcovars(dvn, type = "free")
  expect_equal(result[1], "Sat1 ~~ psi12*Comm1")
  expect_equal(result[2], "Sat1 ~~ psi13*Intim1")
  expect_equal(result[length(result)], "Pass2 ~~ psi1112*Love2") # Check last pair
})

test_that("Correct output for type = 'zero'", {
  result <- multifac_lcovars(dvn, type = "zero")
  expect_equal(result[1], "Sat1 ~~ psi12*Comm1 + 0*Comm1")
  expect_equal(result[2], "Sat1 ~~ psi13*Intim1 + 0*Intim1")
  expect_equal(result[length(result)], "Pass2 ~~ psi1112*Love2 + 0*Love2") # Check last pair
})

test_that("Handles empty variable names", {
  dvn_empty <- list(p1xvarnames = list(), p2xvarnames = list(), xindper = 18, indnum = 36)
  result <- multifac_lcovars(dvn_empty, type = "free")
  expect_equal(result, character(0)) # Should return an empty character vector
})

test_that("Error on invalid type", {
  expect_error(multifac_lcovars(dvn, type = "unknown"), "Invalid type argument. Use 'free' or 'zero'.")
})
#### lmeans ####
#### multi-factor lmeans ####
# devtools::load_all()
# Full exemplar dvn list
dvn <- list(
  p1xvarnames = list(
    Sat = c("sat_1.1", "sat_2.1", "sat_3.1"),
    Comm = c("com_1.1", "com_2.1", "com_3.1"),
    Intim = c("int_1.1", "int_2.1", "int_3.1"),
    Trust = c("trust_1.1", "trust_2.1", "trust_3.1"),
    Pass = c("pass_1.1", "pass_2.1", "pass_3.1"),
    Love = c("love_1.1", "love_2.1", "love_3.1")
  ),
  p2xvarnames = list(
    Sat = c("sat_1.2", "sat_2.2", "sat_3.2"),
    Comm = c("com_1.2", "com_2.2", "com_3.2"),
    Intim = c("int_1.2", "int_2.2", "int_3.2"),
    Trust = c("trust_1.2", "trust_2.2", "trust_3.2"),
    Pass = c("pass_1.2", "pass_2.2", "pass_3.2"),
    Love = c("love_1.2", "love_2.2", "love_3.2")
  ),
  xindper = 18,
  dist1 = "1",
  dist2 = "2",
  indnum = 36
)

test_that("Correct output for partner 1, type = 'free'", {
  result <- multifac_lmeans(dvn, partner = "1", type = "free")
  expect_equal(result[1], "Sat1 ~ a1*1")
  expect_equal(result[2], "Comm1 ~ a2*1")
  expect_equal(result[3], "Intim1 ~ a3*1")
  expect_equal(result[length(result)], "Love1 ~ a6*1") # Check last latent variable
})

test_that("Correct output for partner 2, type = 'free'", {
  result <- multifac_lmeans(dvn, partner = "2", type = "free")
  expect_equal(result[1], "Sat2 ~ a7*1")
  expect_equal(result[2], "Comm2 ~ a8*1")
  expect_equal(result[3], "Intim2 ~ a9*1")
  expect_equal(result[length(result)], "Love2 ~ a12*1") # Check last latent variable
})

test_that("Correct output for partner 1, type = 'fixed'", {
  result <- multifac_lmeans(dvn, partner = "1", type = "fixed")
  expect_equal(result[1], "Sat1 ~ a1*1 + 0*1")
  expect_equal(result[2], "Comm1 ~ a2*1 + 0*1")
  expect_equal(result[3], "Intim1 ~ a3*1 + 0*1")
  expect_equal(result[length(result)], "Love1 ~ a6*1 + 0*1") # Check last latent variable
})

test_that("Correct output for partner 2, type = 'fixed'", {
  result <- multifac_lmeans(dvn, partner = "2", type = "fixed")
  expect_equal(result[1], "Sat2 ~ a7*1 + 0*1")
  expect_equal(result[2], "Comm2 ~ a8*1 + 0*1")
  expect_equal(result[3], "Intim2 ~ a9*1 + 0*1")
  expect_equal(result[length(result)], "Love2 ~ a12*1 + 0*1") # Check last latent variable
})

test_that("Correct output for partner 2, type = 'equated'", {
  result <- multifac_lmeans(dvn, partner = "2", type = "equated")
  expect_equal(result[1], "Sat2 ~ a1*1")
  expect_equal(result[2], "Comm2 ~ a2*1")
  expect_equal(result[3], "Intim2 ~ a3*1")
  expect_equal(result[length(result)], "Love2 ~ a6*1") # Check last latent variable
})

test_that("Handles empty variable names", {
  dvn_empty <- list(p1xvarnames = list(), p2xvarnames = list(), xindper = 18, indnum = 36)
  result <- multifac_lmeans(dvn_empty, partner = "1", type = "free")
  expect_equal(result, character(0)) # Should return an empty character vector
})

test_that("Error on invalid partner", {
  expect_error(multifac_lmeans(dvn, partner = "3", type = "free"), "Invalid partner argument. Use '1' or '2'.")
})

test_that("Error on invalid type", {
  expect_error(multifac_lmeans(dvn, partner = "1", type = "unknown"), "Invalid type argument. Use 'free', 'fixed', or 'equated'.")
})
##### partner = 1 #####
test_that("lmeans produces correct output for lvar = X, partner = 1, type = free", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    lmeans(dvn, lvar = "X", lvname = "Sat", partner = "1", type = "free"),
    "Sat1 ~ NA*1"
  )
})

test_that("lmeans produces correct output for lvar = Y, partner = 1, type = free", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    lmeans(dvn, lvar = "Y", lvname = "Com", partner = "1", type = "free"),
    "Com1 ~ NA*1"
  )
})

test_that("lmeans produces correct output for lvar = X, partner = 1, type = fixed", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    lmeans(dvn, lvar = "X", lvname = "Sat", partner = "1", type = "fixed"),
    "Sat1 ~ 0*1"
  )
})

test_that("lmeans produces correct output for lvar = Y, partner = 1, type = fixed", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    lmeans(dvn, lvar = "Y", lvname = "Com", partner = "1", type = "fixed"),
    "Com1 ~ 0*1"
  )
})

test_that("lmeans produces correct output for lvar = X, partner = 1, type = equated", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    lmeans(dvn, lvar = "X", lvname = "Sat", partner = "1", type = "equated"),
    "Sat1 ~ alphax*1"
  )
})

test_that("lmeans produces correct output for lvar = Y, partner = 1, type = equated", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    lmeans(dvn, lvar = "Y", lvname = "Com", partner = "1", type = "equated"),
    "Com1 ~ alphay*1"
  )
})

##### partner = 2 #####
test_that("lmeans produces correct output for lvar = X, partner = 2, type = free", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    lmeans(dvn, lvar = "X", lvname = "Sat", partner = "2", type = "free"),
    "Sat2 ~ NA*1"
  )
})

test_that("lmeans produces correct output for lvar = Y, partner = 2, type = free", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    lmeans(dvn, lvar = "Y", lvname = "Com", partner = "2", type = "free"),
    "Com2 ~ NA*1"
  )
})

test_that("lmeans produces correct output for lvar = X, partner = 2, type = fixed", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    lmeans(dvn, lvar = "X", lvname = "Sat", partner = "2", type = "fixed"),
    "Sat2 ~ 0*1"
  )
})

test_that("lmeans produces correct output for lvar = Y, partner = 2, type = fixed", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    lmeans(dvn, lvar = "Y", lvname = "Com", partner = "2", type = "fixed"),
    "Com2 ~ 0*1"
  )
})

test_that("lmeans produces correct output for lvar = X, partner = 2, type = equated", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    lmeans(dvn, lvar = "X", lvname = "Sat", partner = "2", type = "equated"),
    "Sat2 ~ alphax*1"
  )
})

test_that("lmeans produces correct output for lvar = Y, partner = 2, type = equated", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    lmeans(dvn, lvar = "Y", lvname = "Com", partner = "2", type = "equated"),
    "Com2 ~ alphay*1"
  )
})

##### partner = g #####
test_that("lmeans produces correct output for lvar = X, partner = g, type = free", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    lmeans(dvn, lvar = "X", lvname = "Sat", partner = "g", type = "free"),
    "SatDy ~ NA*1"
  )
})

test_that("lmeans produces correct output for lvar = X, partner = g, type = fixed", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  expect_equal(
    lmeans(dvn, lvar = "X", lvname = "Sat", partner = "g", type = "fixed"),
    "SatDy ~ 0*1"
  )
})
