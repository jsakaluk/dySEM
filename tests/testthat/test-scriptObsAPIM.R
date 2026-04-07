test_that("scriptObsAPIM default matches indistinguishable paths and equated variances", {
  exp <- paste(
    c(
      "# Actor and partner effects",
      "Com_A ~ a*Sat_A + p*Sat_B",
      "Com_B ~ a*Sat_B + p*Sat_A",
      "",
      "# Exogenous variances",
      "Sat_A ~~ vx*Sat_A",
      "Sat_B ~~ vx*Sat_B",
      "",
      "# Dyadic X covariance",
      "Sat_A ~~ Sat_B",
      "",
      "# Endogenous residual variances",
      "Com_A ~~ vy*Com_A",
      "Com_B ~~ vy*Com_B",
      "",
      "# Residual covariance between Y",
      "Com_A ~~ Com_B"
    ),
    collapse = "\n"
  )
  expect_equal(
    scriptObsAPIM("Sat_A", "Com_A", "Sat_B", "Com_B"),
    exp
  )
})

test_that("scriptObsAPIM fully distinguishable matches legacy equate = none (no variance constraints)", {
  exp <- paste(
    c(
      "# Actor and partner effects",
      "Com_A ~ a1*Sat_A + p1*Sat_B",
      "Com_B ~ a2*Sat_B + p2*Sat_A",
      "",
      "# Dyadic X covariance",
      "Sat_A ~~ Sat_B",
      "",
      "# Residual covariance between Y",
      "Com_A ~~ Com_B"
    ),
    collapse = "\n"
  )
  expect_equal(
    scriptObsAPIM(
      "Sat_A", "Com_A", "Sat_B", "Com_B",
      constr_dy_xy_struct = "none",
      constr_dy_x_struct = "none",
      constr_dy_y_struct = "none"
    ),
    exp
  )
})

test_that("scriptObsAPIM est_k with distinguishable paths", {
  exp <- paste(
    c(
      "# Actor and partner effects",
      "Com_A ~ a1*Sat_A + p1*Sat_B",
      "Com_B ~ a2*Sat_B + p2*Sat_A",
      "",
      "# Dyadic X covariance",
      "Sat_A ~~ Sat_B",
      "",
      "# Residual covariance between Y",
      "Com_A ~~ Com_B",
      "",
      "# k parameter(s)",
      "k1 := p1/a1",
      "k2 := p2/a2"
    ),
    collapse = "\n"
  )
  expect_equal(
    scriptObsAPIM(
      "Sat_A", "Com_A", "Sat_B", "Com_B",
      constr_dy_xy_struct = "none",
      constr_dy_x_struct = "none",
      constr_dy_y_struct = "none",
      est_k = TRUE
    ),
    exp
  )
})

test_that("scriptObsAPIM est_k with default indistinguishable paths", {
  exp <- paste(
    c(
      "# Actor and partner effects",
      "Com_A ~ a*Sat_A + p*Sat_B",
      "Com_B ~ a*Sat_B + p*Sat_A",
      "",
      "# Exogenous variances",
      "Sat_A ~~ vx*Sat_A",
      "Sat_B ~~ vx*Sat_B",
      "",
      "# Dyadic X covariance",
      "Sat_A ~~ Sat_B",
      "",
      "# Endogenous residual variances",
      "Com_A ~~ vy*Com_A",
      "Com_B ~~ vy*Com_B",
      "",
      "# Residual covariance between Y",
      "Com_A ~~ Com_B",
      "",
      "# k parameter(s)",
      "k := p/a"
    ),
    collapse = "\n"
  )
  expect_equal(
    scriptObsAPIM("Sat_A", "Com_A", "Sat_B", "Com_B", est_k = TRUE),
    exp
  )
})

test_that("scriptObsAPIM est_k with actors equated only", {
  exp <- paste(
    c(
      "# Actor and partner effects",
      "Com_A ~ a*Sat_A + p1*Sat_B",
      "Com_B ~ a*Sat_B + p2*Sat_A",
      "",
      "# Exogenous variances",
      "Sat_A ~~ vx*Sat_A",
      "Sat_B ~~ vx*Sat_B",
      "",
      "# Dyadic X covariance",
      "Sat_A ~~ Sat_B",
      "",
      "# Endogenous residual variances",
      "Com_A ~~ vy*Com_A",
      "Com_B ~~ vy*Com_B",
      "",
      "# Residual covariance between Y",
      "Com_A ~~ Com_B",
      "",
      "# k parameter(s)",
      "k1 := p1/a",
      "k2 := p2/a"
    ),
    collapse = "\n"
  )
  expect_equal(
    scriptObsAPIM(
      "Sat_A", "Com_A", "Sat_B", "Com_B",
      constr_dy_xy_struct = "actors",
      est_k = TRUE
    ),
    exp
  )
})

test_that("scriptObsAPIM est_k with partners equated only", {
  exp <- paste(
    c(
      "# Actor and partner effects",
      "Com_A ~ a1*Sat_A + p*Sat_B",
      "Com_B ~ a2*Sat_B + p*Sat_A",
      "",
      "# Exogenous variances",
      "Sat_A ~~ vx*Sat_A",
      "Sat_B ~~ vx*Sat_B",
      "",
      "# Dyadic X covariance",
      "Sat_A ~~ Sat_B",
      "",
      "# Endogenous residual variances",
      "Com_A ~~ vy*Com_A",
      "Com_B ~~ vy*Com_B",
      "",
      "# Residual covariance between Y",
      "Com_A ~~ Com_B",
      "",
      "# k parameter(s)",
      "k1 := p/a1",
      "k2 := p/a2"
    ),
    collapse = "\n"
  )
  expect_equal(
    scriptObsAPIM(
      "Sat_A", "Com_A", "Sat_B", "Com_B",
      constr_dy_xy_struct = "partners",
      est_k = TRUE
    ),
    exp
  )
})

test_that("scriptObsAPIM orthogonal X fixes dyadic X covariance", {
  out <- scriptObsAPIM(
    "Sat_A", "Com_A", "Sat_B", "Com_B",
    constr_dy_x_struct = c("orthogonal"),
    constr_dy_y_struct = "none",
    constr_dy_xy_struct = c("actors", "partners")
  )
  expect_match(out, "Sat_A ~~ 0\\*Sat_B", fixed = FALSE)
  expect_false(grepl("Sat_A ~~ Sat_B\n", out, fixed = TRUE))
})

test_that("scriptObsAPIM rejects est_k with actors_zero", {
  expect_error(
    scriptObsAPIM(
      "Sat_A", "Com_A", "Sat_B", "Com_B",
      constr_dy_xy_struct = "actors_zero",
      constr_dy_x_struct = "none",
      constr_dy_y_struct = "none",
      est_k = TRUE
    ),
    "cannot estimate k"
  )
})

test_that("scriptObsAPIM includeMeanStruct adds equated intercepts when means constrained", {
  out <- scriptObsAPIM(
    "Sat_A", "Com_A", "Sat_B", "Com_B",
    constr_dy_xy_struct = "none",
    constr_dy_x_struct = c("means"),
    constr_dy_y_struct = c("means"),
    includeMeanStruct = TRUE
  )
  expect_match(out, "Com_A ~ iy\\*1 \\+ a1\\*Sat_A", fixed = FALSE)
  expect_match(out, "Sat_A ~ mx\\*1", fixed = FALSE)
})
