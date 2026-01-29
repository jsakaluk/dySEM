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


#### Script Correctness Tests for scriptCFA ####
# ============================================
# These tests verify that scriptCFA generates correct lavaan syntax for dyadic CFA
# invariance models (configural, loading, intercept, residual), under both scale-setting
# approaches (fixed-factor vs marker-variable).

#### Group 1: Configural Invariance Script Correctness ####
# ------------------------------------
# These tests verify that scriptCFA generates correct dyadic configural invariance scripts.

#FF output
test_that("scriptCFA produces correct output for constr_dy_meas = none, constr_dy_struct = none, and scaleset= FF", {

  pnrq_list <- list(
    lvnames = c(
      "pos", "neg"
    ),
    stem = c(
      "sat.pnrq", "dsat.pnrq"
    ),
    delim1 = c(
      "", ""
    ),
    delim2 = c(
      "_", "_"
    )
  )

  dvn <- scrapeVarCross(
    pnrqM,
    var_list = pnrq_list,
    var_list_order = "sip",
    distinguish_1 = "w",
    distinguish_2 = "m"
  )

  expect_equal(
    scriptCFA(
      dvn,
      scaleset = "FF",
      constr_dy_meas = "none",
      constr_dy_struct = "none"
    ),
    "#Measurement Model\n\n#Loadings\npos1 =~ NA*sat.pnrq1_w + lx1*sat.pnrq1_w + lx2*sat.pnrq2_w + lx3*sat.pnrq3_w + lx4*sat.pnrq4_w\nneg1 =~ NA*dsat.pnrq1_w + lx5*dsat.pnrq1_w + lx6*dsat.pnrq2_w + lx7*dsat.pnrq3_w + lx8*dsat.pnrq4_w\npos2 =~ NA*sat.pnrq1_m + lx9*sat.pnrq1_m + lx10*sat.pnrq2_m + lx11*sat.pnrq3_m + lx12*sat.pnrq4_m\nneg2 =~ NA*dsat.pnrq1_m + lx13*dsat.pnrq1_m + lx14*dsat.pnrq2_m + lx15*dsat.pnrq3_m + lx16*dsat.pnrq4_m\n\n#Intercepts\nsat.pnrq1_w ~ t1*1\nsat.pnrq2_w ~ t2*1\nsat.pnrq3_w ~ t3*1\nsat.pnrq4_w ~ t4*1\ndsat.pnrq1_w ~ t5*1\ndsat.pnrq2_w ~ t6*1\ndsat.pnrq3_w ~ t7*1\ndsat.pnrq4_w ~ t8*1\n\nsat.pnrq1_m ~ t9*1\nsat.pnrq2_m ~ t10*1\nsat.pnrq3_m ~ t11*1\nsat.pnrq4_m ~ t12*1\ndsat.pnrq1_m ~ t13*1\ndsat.pnrq2_m ~ t14*1\ndsat.pnrq3_m ~ t15*1\ndsat.pnrq4_m ~ t16*1\n\n#Residual Variances\nsat.pnrq1_w ~~ th1*sat.pnrq1_w\nsat.pnrq2_w ~~ th2*sat.pnrq2_w\nsat.pnrq3_w ~~ th3*sat.pnrq3_w\nsat.pnrq4_w ~~ th4*sat.pnrq4_w\ndsat.pnrq1_w ~~ th5*dsat.pnrq1_w\ndsat.pnrq2_w ~~ th6*dsat.pnrq2_w\ndsat.pnrq3_w ~~ th7*dsat.pnrq3_w\ndsat.pnrq4_w ~~ th8*dsat.pnrq4_w\n\nsat.pnrq1_m ~~ th9*sat.pnrq1_m\nsat.pnrq2_m ~~ th10*sat.pnrq2_m\nsat.pnrq3_m ~~ th11*sat.pnrq3_m\nsat.pnrq4_m ~~ th12*sat.pnrq4_m\ndsat.pnrq1_m ~~ th13*dsat.pnrq1_m\ndsat.pnrq2_m ~~ th14*dsat.pnrq2_m\ndsat.pnrq3_m ~~ th15*dsat.pnrq3_m\ndsat.pnrq4_m ~~ th16*dsat.pnrq4_m\n\n#Residual Covariances\nsat.pnrq1_w ~~ sat.pnrq1_m\nsat.pnrq2_w ~~ sat.pnrq2_m\nsat.pnrq3_w ~~ sat.pnrq3_m\nsat.pnrq4_w ~~ sat.pnrq4_m\ndsat.pnrq1_w ~~ dsat.pnrq1_m\ndsat.pnrq2_w ~~ dsat.pnrq2_m\ndsat.pnrq3_w ~~ dsat.pnrq3_m\ndsat.pnrq4_w ~~ dsat.pnrq4_m\n\n#Structural Model\n\n#Latent (Co)Variances\npos1 ~~ psv1*pos1 + 1*pos1\nneg1 ~~ psv2*neg1 + 1*neg1\npos2 ~~ psv3*pos2 + 1*pos2\nneg2 ~~ psv4*neg2 + 1*neg2\npos1 ~~ psi12*neg1\npos1 ~~ psi13*pos2\npos1 ~~ psi14*neg2\nneg1 ~~ psi23*pos2\nneg1 ~~ psi24*neg2\npos2 ~~ psi34*neg2\n\n#Latent Means\npos1 ~ a1*1 + 0*1\nneg1 ~ a2*1 + 0*1\npos2 ~ a3*1 + 0*1\nneg2 ~ a4*1 + 0*1"
  )
})

#MV output
test_that("scriptCFA produces correct output for constr_dy_meas = none, constr_dy_struct = none and scaleset= MV", {

  pnrq_list <- list(
    lvnames = c(
      "pos", "neg"
    ),
    stem = c(
      "sat.pnrq", "dsat.pnrq"
    ),
    delim1 = c(
      "", ""
    ),
    delim2 = c(
      "_", "_"
    )
  )

  dvn <- scrapeVarCross(
    pnrqM,
    var_list = pnrq_list,
    var_list_order = "sip",
    distinguish_1 = "w",
    distinguish_2 = "m"
  )

  expect_equal(
    scriptCFA(
      dvn,
      scaleset = "MV",
      constr_dy_meas = "none",
      constr_dy_struct = "none"
    ),
    "#Measurement Model\n\n#Loadings\npos1 =~ 1*sat.pnrq1_w + lx1*sat.pnrq1_w + lx2*sat.pnrq2_w + lx3*sat.pnrq3_w + lx4*sat.pnrq4_w\nneg1 =~ 1*dsat.pnrq1_w + lx5*dsat.pnrq1_w + lx6*dsat.pnrq2_w + lx7*dsat.pnrq3_w + lx8*dsat.pnrq4_w\npos2 =~ 1*sat.pnrq1_m + lx9*sat.pnrq1_m + lx10*sat.pnrq2_m + lx11*sat.pnrq3_m + lx12*sat.pnrq4_m\nneg2 =~ 1*dsat.pnrq1_m + lx13*dsat.pnrq1_m + lx14*dsat.pnrq2_m + lx15*dsat.pnrq3_m + lx16*dsat.pnrq4_m\n\n#Intercepts\nsat.pnrq1_w ~ 0*1\nsat.pnrq1_w ~ t1*1\nsat.pnrq2_w ~ t2*1\nsat.pnrq3_w ~ t3*1\nsat.pnrq4_w ~ t4*1\ndsat.pnrq1_w ~ 0*1\ndsat.pnrq1_w ~ t5*1\ndsat.pnrq2_w ~ t6*1\ndsat.pnrq3_w ~ t7*1\ndsat.pnrq4_w ~ t8*1\n\nsat.pnrq1_m ~ 0*1\nsat.pnrq1_m ~ t9*1\nsat.pnrq2_m ~ t10*1\nsat.pnrq3_m ~ t11*1\nsat.pnrq4_m ~ t12*1\ndsat.pnrq1_m ~ 0*1\ndsat.pnrq1_m ~ t13*1\ndsat.pnrq2_m ~ t14*1\ndsat.pnrq3_m ~ t15*1\ndsat.pnrq4_m ~ t16*1\n\n#Residual Variances\nsat.pnrq1_w ~~ th1*sat.pnrq1_w\nsat.pnrq2_w ~~ th2*sat.pnrq2_w\nsat.pnrq3_w ~~ th3*sat.pnrq3_w\nsat.pnrq4_w ~~ th4*sat.pnrq4_w\ndsat.pnrq1_w ~~ th5*dsat.pnrq1_w\ndsat.pnrq2_w ~~ th6*dsat.pnrq2_w\ndsat.pnrq3_w ~~ th7*dsat.pnrq3_w\ndsat.pnrq4_w ~~ th8*dsat.pnrq4_w\n\nsat.pnrq1_m ~~ th9*sat.pnrq1_m\nsat.pnrq2_m ~~ th10*sat.pnrq2_m\nsat.pnrq3_m ~~ th11*sat.pnrq3_m\nsat.pnrq4_m ~~ th12*sat.pnrq4_m\ndsat.pnrq1_m ~~ th13*dsat.pnrq1_m\ndsat.pnrq2_m ~~ th14*dsat.pnrq2_m\ndsat.pnrq3_m ~~ th15*dsat.pnrq3_m\ndsat.pnrq4_m ~~ th16*dsat.pnrq4_m\n\n#Residual Covariances\nsat.pnrq1_w ~~ sat.pnrq1_m\nsat.pnrq2_w ~~ sat.pnrq2_m\nsat.pnrq3_w ~~ sat.pnrq3_m\nsat.pnrq4_w ~~ sat.pnrq4_m\ndsat.pnrq1_w ~~ dsat.pnrq1_m\ndsat.pnrq2_w ~~ dsat.pnrq2_m\ndsat.pnrq3_w ~~ dsat.pnrq3_m\ndsat.pnrq4_w ~~ dsat.pnrq4_m\n\n#Structural Model\n\n#Latent (Co)Variances\npos1 ~~ psv1*pos1 + NA*pos1\nneg1 ~~ psv2*neg1 + NA*neg1\npos2 ~~ psv3*pos2 + NA*pos2\nneg2 ~~ psv4*neg2 + NA*neg2\npos1 ~~ psi12*neg1\npos1 ~~ psi13*pos2\npos1 ~~ psi14*neg2\nneg1 ~~ psi23*pos2\nneg1 ~~ psi24*neg2\npos2 ~~ psi34*neg2\n\n#Latent Means\npos1 ~ a1*1\nneg1 ~ a2*1\npos2 ~ a3*1\nneg2 ~ a4*1"
  )
})

#Number of parameter estimates (FF)
test_that("scriptCFA produces correct number of parameter estimates for constr_dy_meas = none, constr_dy_struct = none and scaleset= FF", {

  pnrq_list <- list(
    lvnames = c(
      "pos", "neg"
    ),
    stem = c(
      "sat.pnrq", "dsat.pnrq"
    ),
    delim1 = c(
      "", ""
    ),
    delim2 = c(
      "_", "_"
    )
  )

  dvn <- scrapeVarCross(
    pnrqM,
    var_list = pnrq_list,
    var_list_order = "sip",
    distinguish_1 = "w",
    distinguish_2 = "m"
  )

  # knowns for model w/ intercepts/latent means:
  # var_num <- 16
  # knowns <- var_num*(var_num+1)/2+var_num

  # Script model w/ `constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "FF"`
  script <- scriptCFA(dvn, constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "FF")

  # Fit model
  mod <- lavaan::cfa(script, data = pnrqM)

  # manually calculated free parameters (see LINK)
  # my_param <- 62

  lav_param <- as.double(lavaan::fitmeasures(mod, "npar"))

  expect_equal(
    lav_param,
    62
  )
})

#Number of df (FF)
test_that("scriptCFA produces correct df for constr_dy_meas = none, constr_dy_struct = none and scaleset= FF", {

  pnrq_list <- list(
    lvnames = c(
      "pos", "neg"
    ),
    stem = c(
      "sat.pnrq", "dsat.pnrq"
    ),
    delim1 = c(
      "", ""
    ),
    delim2 = c(
      "_", "_"
    )
  )

  dvn <- scrapeVarCross(
    pnrqM,
    var_list = pnrq_list,
    var_list_order = "sip",
    distinguish_1 = "w",
    distinguish_2 = "m"
  )

  # knowns for model w/ intercepts/latent means:
  # var_num <- 16
  # knowns <- var_num*(var_num+1)/2+var_num

  # Script model w/ `constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "FF"`
  script <- scriptCFA(dvn, constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "FF")

  # Fit model
  mod <- lavaan::cfa(script, data = pnrqM)

  # manually calculated free parameters (see LINK)
  # my_param <- 62

  # calculated df (should be 90)
  # my_df <- knowns-my_param

  lav_df <- as.double(lavaan::fitmeasures(mod, "df"))

  expect_equal(
    lav_df,
    90
  )
})

#FF df = MV df
test_that("scriptCFA produces same df for constr_dy_meas = none, constr_dy_struct = none when scaleset= FF, and scaleset = MV", {

  pnrq_list <- list(
    lvnames = c(
      "pos", "neg"
    ),
    stem = c(
      "sat.pnrq", "dsat.pnrq"
    ),
    delim1 = c(
      "", ""
    ),
    delim2 = c(
      "_", "_"
    )
  )

  dvn <- scrapeVarCross(
    pnrqM,
    var_list = pnrq_list,
    var_list_order = "sip",
    distinguish_1 = "w",
    distinguish_2 = "m"
  )

  # Script model w/ `constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "FF"`
  script_ff <- scriptCFA(dvn, constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "FF")

  # Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = pnrqM)

  # get its df
  df_ff <- as.double(lavaan::fitmeasures(mod_ff, "df"))

  # Script model w/ `constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "MV"`
  script_mv <- scriptCFA(dvn, constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "MV")

  # Fit model w MV
  mod_mv <- lavaan::cfa(script_mv, data = pnrqM)

  # get its df
  df_mv <- as.double(lavaan::fitmeasures(mod_mv, "df"))

  # scale setting should have no impact on df
  expect_equal(
    df_ff,
    df_mv
  )
})

#FF chisq = MV chisq
test_that("scriptCFA produces same chisq for constr_dy_meas = none, constr_dy_struct = none when scaleset= FF, and scaleset = MV", {

  pnrq_list <- list(
    lvnames = c(
      "pos", "neg"
    ),
    stem = c(
      "sat.pnrq", "dsat.pnrq"
    ),
    delim1 = c(
      "", ""
    ),
    delim2 = c(
      "_", "_"
    )
  )

  dvn <- scrapeVarCross(
    pnrqM,
    var_list = pnrq_list,
    var_list_order = "sip",
    distinguish_1 = "w",
    distinguish_2 = "m"
  )

  # Script model w/ `constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "FF"`
  script_ff <- scriptCFA(dvn, constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "FF")

  # Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = pnrqM)

  # get its df
  chisq_ff <- as.double(lavaan::fitmeasures(mod_ff, "chisq"))

  # Script model w/ `constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "MV"`
  script_mv <- scriptCFA(dvn, constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "MV")

  # Fit model w MV
  mod_mv <- lavaan::cfa(script_mv, data = pnrqM)

  # get its chisq
  chisq_mv <- as.double(lavaan::fitmeasures(mod_mv, "chisq"))

  # scale setting should have no impact on chisq
  expect_equal(
    chisq_ff,
    chisq_mv
  )
})

#### Group 2: Loading Invariance Script Correctness ####
# --------------------------------------------
# These tests verify that scriptCFA generates correct dyadic loading invariance scripts.

#FF output
test_that("scriptCFA produces correct output for constr_dy_meas = loadings, constr_dy_struct = none, and scaleset= FF", {

  pnrq_list <- list(
    lvnames = c(
      "pos", "neg"
    ),
    stem = c(
      "sat.pnrq", "dsat.pnrq"
    ),
    delim1 = c(
      "", ""
    ),
    delim2 = c(
      "_", "_"
    )
  )

  dvn <- scrapeVarCross(
    pnrqM,
    var_list = pnrq_list,
    var_list_order = "sip",
    distinguish_1 = "w",
    distinguish_2 = "m"
  )

  expect_equal(
    scriptCFA(
      dvn,
      scaleset = "FF",
      constr_dy_meas = "loadings",
      constr_dy_struct = "none"
    ),
    "#Measurement Model\n\n#Loadings\npos1 =~ NA*sat.pnrq1_w + lx1*sat.pnrq1_w + lx2*sat.pnrq2_w + lx3*sat.pnrq3_w + lx4*sat.pnrq4_w\nneg1 =~ NA*dsat.pnrq1_w + lx5*dsat.pnrq1_w + lx6*dsat.pnrq2_w + lx7*dsat.pnrq3_w + lx8*dsat.pnrq4_w\npos2 =~ NA*sat.pnrq1_m + lx1*sat.pnrq1_m + lx2*sat.pnrq2_m + lx3*sat.pnrq3_m + lx4*sat.pnrq4_m\nneg2 =~ NA*dsat.pnrq1_m + lx5*dsat.pnrq1_m + lx6*dsat.pnrq2_m + lx7*dsat.pnrq3_m + lx8*dsat.pnrq4_m\n\n#Intercepts\nsat.pnrq1_w ~ t1*1\nsat.pnrq2_w ~ t2*1\nsat.pnrq3_w ~ t3*1\nsat.pnrq4_w ~ t4*1\ndsat.pnrq1_w ~ t5*1\ndsat.pnrq2_w ~ t6*1\ndsat.pnrq3_w ~ t7*1\ndsat.pnrq4_w ~ t8*1\n\nsat.pnrq1_m ~ t9*1\nsat.pnrq2_m ~ t10*1\nsat.pnrq3_m ~ t11*1\nsat.pnrq4_m ~ t12*1\ndsat.pnrq1_m ~ t13*1\ndsat.pnrq2_m ~ t14*1\ndsat.pnrq3_m ~ t15*1\ndsat.pnrq4_m ~ t16*1\n\n#Residual Variances\nsat.pnrq1_w ~~ th1*sat.pnrq1_w\nsat.pnrq2_w ~~ th2*sat.pnrq2_w\nsat.pnrq3_w ~~ th3*sat.pnrq3_w\nsat.pnrq4_w ~~ th4*sat.pnrq4_w\ndsat.pnrq1_w ~~ th5*dsat.pnrq1_w\ndsat.pnrq2_w ~~ th6*dsat.pnrq2_w\ndsat.pnrq3_w ~~ th7*dsat.pnrq3_w\ndsat.pnrq4_w ~~ th8*dsat.pnrq4_w\n\nsat.pnrq1_m ~~ th9*sat.pnrq1_m\nsat.pnrq2_m ~~ th10*sat.pnrq2_m\nsat.pnrq3_m ~~ th11*sat.pnrq3_m\nsat.pnrq4_m ~~ th12*sat.pnrq4_m\ndsat.pnrq1_m ~~ th13*dsat.pnrq1_m\ndsat.pnrq2_m ~~ th14*dsat.pnrq2_m\ndsat.pnrq3_m ~~ th15*dsat.pnrq3_m\ndsat.pnrq4_m ~~ th16*dsat.pnrq4_m\n\n#Residual Covariances\nsat.pnrq1_w ~~ sat.pnrq1_m\nsat.pnrq2_w ~~ sat.pnrq2_m\nsat.pnrq3_w ~~ sat.pnrq3_m\nsat.pnrq4_w ~~ sat.pnrq4_m\ndsat.pnrq1_w ~~ dsat.pnrq1_m\ndsat.pnrq2_w ~~ dsat.pnrq2_m\ndsat.pnrq3_w ~~ dsat.pnrq3_m\ndsat.pnrq4_w ~~ dsat.pnrq4_m\n\n#Structural Model\n\n#Latent (Co)Variances\npos1 ~~ psv1*pos1 + 1*pos1\nneg1 ~~ psv2*neg1 + 1*neg1\npos2 ~~ psv3*pos2 + NA*pos2\nneg2 ~~ psv4*neg2 + NA*neg2\npos1 ~~ psi12*neg1\npos1 ~~ psi13*pos2\npos1 ~~ psi14*neg2\nneg1 ~~ psi23*pos2\nneg1 ~~ psi24*neg2\npos2 ~~ psi34*neg2\n\n#Latent Means\npos1 ~ a1*1 + 0*1\nneg1 ~ a2*1 + 0*1\npos2 ~ a3*1 + 0*1\nneg2 ~ a4*1 + 0*1"
  )
})

#MV output
test_that("scriptCFA produces correct output for constr_dy_meas = loadings, constr_dy_struct = none and scaleset= MV", {

  pnrq_list <- list(
    lvnames = c(
      "pos", "neg"
    ),
    stem = c(
      "sat.pnrq", "dsat.pnrq"
    ),
    delim1 = c(
      "", ""
    ),
    delim2 = c(
      "_", "_"
    )
  )

  dvn <- scrapeVarCross(
    pnrqM,
    var_list = pnrq_list,
    var_list_order = "sip",
    distinguish_1 = "w",
    distinguish_2 = "m"
  )

  expect_equal(
    scriptCFA(
      dvn,
      scaleset = "MV",
      constr_dy_meas = "loadings",
      constr_dy_struct = "none"
    ),
    "#Measurement Model\n\n#Loadings\npos1 =~ 1*sat.pnrq1_w + lx1*sat.pnrq1_w + lx2*sat.pnrq2_w + lx3*sat.pnrq3_w + lx4*sat.pnrq4_w\nneg1 =~ 1*dsat.pnrq1_w + lx5*dsat.pnrq1_w + lx6*dsat.pnrq2_w + lx7*dsat.pnrq3_w + lx8*dsat.pnrq4_w\npos2 =~ NA*sat.pnrq1_m + lx1*sat.pnrq1_m + lx2*sat.pnrq2_m + lx3*sat.pnrq3_m + lx4*sat.pnrq4_m\nneg2 =~ NA*dsat.pnrq1_m + lx5*dsat.pnrq1_m + lx6*dsat.pnrq2_m + lx7*dsat.pnrq3_m + lx8*dsat.pnrq4_m\n\n#Intercepts\nsat.pnrq1_w ~ 0*1\nsat.pnrq1_w ~ t1*1\nsat.pnrq2_w ~ t2*1\nsat.pnrq3_w ~ t3*1\nsat.pnrq4_w ~ t4*1\ndsat.pnrq1_w ~ 0*1\ndsat.pnrq1_w ~ t5*1\ndsat.pnrq2_w ~ t6*1\ndsat.pnrq3_w ~ t7*1\ndsat.pnrq4_w ~ t8*1\n\nsat.pnrq1_m ~ 0*1\nsat.pnrq1_m ~ t9*1\nsat.pnrq2_m ~ t10*1\nsat.pnrq3_m ~ t11*1\nsat.pnrq4_m ~ t12*1\ndsat.pnrq1_m ~ 0*1\ndsat.pnrq1_m ~ t13*1\ndsat.pnrq2_m ~ t14*1\ndsat.pnrq3_m ~ t15*1\ndsat.pnrq4_m ~ t16*1\n\n#Residual Variances\nsat.pnrq1_w ~~ th1*sat.pnrq1_w\nsat.pnrq2_w ~~ th2*sat.pnrq2_w\nsat.pnrq3_w ~~ th3*sat.pnrq3_w\nsat.pnrq4_w ~~ th4*sat.pnrq4_w\ndsat.pnrq1_w ~~ th5*dsat.pnrq1_w\ndsat.pnrq2_w ~~ th6*dsat.pnrq2_w\ndsat.pnrq3_w ~~ th7*dsat.pnrq3_w\ndsat.pnrq4_w ~~ th8*dsat.pnrq4_w\n\nsat.pnrq1_m ~~ th9*sat.pnrq1_m\nsat.pnrq2_m ~~ th10*sat.pnrq2_m\nsat.pnrq3_m ~~ th11*sat.pnrq3_m\nsat.pnrq4_m ~~ th12*sat.pnrq4_m\ndsat.pnrq1_m ~~ th13*dsat.pnrq1_m\ndsat.pnrq2_m ~~ th14*dsat.pnrq2_m\ndsat.pnrq3_m ~~ th15*dsat.pnrq3_m\ndsat.pnrq4_m ~~ th16*dsat.pnrq4_m\n\n#Residual Covariances\nsat.pnrq1_w ~~ sat.pnrq1_m\nsat.pnrq2_w ~~ sat.pnrq2_m\nsat.pnrq3_w ~~ sat.pnrq3_m\nsat.pnrq4_w ~~ sat.pnrq4_m\ndsat.pnrq1_w ~~ dsat.pnrq1_m\ndsat.pnrq2_w ~~ dsat.pnrq2_m\ndsat.pnrq3_w ~~ dsat.pnrq3_m\ndsat.pnrq4_w ~~ dsat.pnrq4_m\n\n#Structural Model\n\n#Latent (Co)Variances\npos1 ~~ psv1*pos1 + NA*pos1\nneg1 ~~ psv2*neg1 + NA*neg1\npos2 ~~ psv3*pos2 + NA*pos2\nneg2 ~~ psv4*neg2 + NA*neg2\npos1 ~~ psi12*neg1\npos1 ~~ psi13*pos2\npos1 ~~ psi14*neg2\nneg1 ~~ psi23*pos2\nneg1 ~~ psi24*neg2\npos2 ~~ psi34*neg2\n\n#Latent Means\npos1 ~ a1*1\nneg1 ~ a2*1\npos2 ~ a3*1\nneg2 ~ a4*1"
  )
})

#Number of parameter estimates (FF)
test_that("scriptCFA produces correct number of parameter estimates for constr_dy_meas = loadings, constr_dy_struct = none and scaleset= FF", {

  pnrq_list <- list(
    lvnames = c(
      "pos", "neg"
    ),
    stem = c(
      "sat.pnrq", "dsat.pnrq"
    ),
    delim1 = c(
      "", ""
    ),
    delim2 = c(
      "_", "_"
    )
  )

  dvn <- scrapeVarCross(
    pnrqM,
    var_list = pnrq_list,
    var_list_order = "sip",
    distinguish_1 = "w",
    distinguish_2 = "m"
  )

  # knowns for model w/ intercepts/latent means:
  # var_num <- 16
  # knowns <- var_num*(var_num+1)/2+var_num

  # Script model w/ `constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "FF"`
  script <- scriptCFA(dvn, constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "FF")

  # Fit model
  mod <- lavaan::cfa(script, data = pnrqM)

  # manually calculated free parameters (see LINK)
  # my_param <- 56

  lav_param <- as.double(lavaan::fitmeasures(mod, "npar"))

  expect_equal(
    lav_param,
    56
  )
})

#Number of df (FF)
test_that("scriptCFA produces correct df for constr_dy_meas = loadings, constr_dy_struct = none and scaleset= FF", {

  pnrq_list <- list(
    lvnames = c(
      "pos", "neg"
    ),
    stem = c(
      "sat.pnrq", "dsat.pnrq"
    ),
    delim1 = c(
      "", ""
    ),
    delim2 = c(
      "_", "_"
    )
  )

  dvn <- scrapeVarCross(
    pnrqM,
    var_list = pnrq_list,
    var_list_order = "sip",
    distinguish_1 = "w",
    distinguish_2 = "m"
  )

  # knowns for model w/ intercepts/latent means:
  # var_num <- 16
  # knowns <- var_num*(var_num+1)/2+var_num

  # Script model w/ `constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "FF"`
  script <- scriptCFA(dvn, constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "FF")

  # Fit model
  mod <- lavaan::cfa(script, data = pnrqM)

  # manually calculated free parameters (see LINK)
  # my_param <- 56

  # calculated df (should be 96)
  # my_df <- knowns-my_param

  lav_df <- as.double(lavaan::fitmeasures(mod, "df"))

  expect_equal(
    lav_df,
    96
  )
})

#FF df = MV df
test_that("scriptCFA produces same df for constr_dy_meas = loadings, constr_dy_struct = none when scaleset= FF, and scaleset = MV", {

  pnrq_list <- list(
    lvnames = c(
      "pos", "neg"
    ),
    stem = c(
      "sat.pnrq", "dsat.pnrq"
    ),
    delim1 = c(
      "", ""
    ),
    delim2 = c(
      "_", "_"
    )
  )

  dvn <- scrapeVarCross(
    pnrqM,
    var_list = pnrq_list,
    var_list_order = "sip",
    distinguish_1 = "w",
    distinguish_2 = "m"
  )

  # Script model w/ `constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "FF"`
  script_ff <- scriptCFA(dvn, constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "FF")

  # Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = pnrqM)

  # get its df
  df_ff <- as.double(lavaan::fitmeasures(mod_ff, "df"))

  # Script model w/ `constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "MV"`
  script_mv <- scriptCFA(dvn, constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "MV")

  # Fit model w MV
  mod_mv <- lavaan::cfa(script_mv, data = pnrqM)

  # get its df
  df_mv <- as.double(lavaan::fitmeasures(mod_mv, "df"))

  # scale setting should have no impact on df
  expect_equal(
    df_ff,
    df_mv
  )
})

#FF chisq = MV chisq
test_that("scriptCFA produces same chisq for constr_dy_meas = loadings, constr_dy_struct = none when scaleset= FF, and scaleset = MV", {

  pnrq_list <- list(
    lvnames = c(
      "pos", "neg"
    ),
    stem = c(
      "sat.pnrq", "dsat.pnrq"
    ),
    delim1 = c(
      "", ""
    ),
    delim2 = c(
      "_", "_"
    )
  )

  dvn <- scrapeVarCross(
    pnrqM,
    var_list = pnrq_list,
    var_list_order = "sip",
    distinguish_1 = "w",
    distinguish_2 = "m"
  )

  # Script model w/ `constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "FF"`
  script_ff <- scriptCFA(dvn, constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "FF")

  # Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = pnrqM)

  # get its df
  chisq_ff <- as.double(lavaan::fitmeasures(mod_ff, "chisq"))

  # Script model w/ `constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "MV"`
  script_mv <- scriptCFA(dvn, constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "MV")

  # Fit model w MV
  mod_mv <- lavaan::cfa(script_mv, data = pnrqM)

  # get its chisq
  chisq_mv <- as.double(lavaan::fitmeasures(mod_mv, "chisq"))

  # scale setting should have no impact on chisq
  expect_equal(
    chisq_ff,
    chisq_mv
  )
})

#### Group 3: Intercept Invariance Script Correctness ####
# --------------------------------------------
# These tests verify that scriptCFA generates correct dyadic intercept invariance scripts.

#FF output
test_that("scriptCFA produces correct output for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none, and scaleset= FF", {

  pnrq_list <- list(
    lvnames = c(
      "pos", "neg"
    ),
    stem = c(
      "sat.pnrq", "dsat.pnrq"
    ),
    delim1 = c(
      "", ""
    ),
    delim2 = c(
      "_", "_"
    )
  )

  dvn <- scrapeVarCross(
    pnrqM,
    var_list = pnrq_list,
    var_list_order = "sip",
    distinguish_1 = "w",
    distinguish_2 = "m"
  )

  expect_equal(
    scriptCFA(
      dvn,
      scaleset = "FF",
      constr_dy_meas = c("loadings", "intercepts"),
      constr_dy_struct = "none"
    ),
    "#Measurement Model\n\n#Loadings\npos1 =~ NA*sat.pnrq1_w + lx1*sat.pnrq1_w + lx2*sat.pnrq2_w + lx3*sat.pnrq3_w + lx4*sat.pnrq4_w\nneg1 =~ NA*dsat.pnrq1_w + lx5*dsat.pnrq1_w + lx6*dsat.pnrq2_w + lx7*dsat.pnrq3_w + lx8*dsat.pnrq4_w\npos2 =~ NA*sat.pnrq1_m + lx1*sat.pnrq1_m + lx2*sat.pnrq2_m + lx3*sat.pnrq3_m + lx4*sat.pnrq4_m\nneg2 =~ NA*dsat.pnrq1_m + lx5*dsat.pnrq1_m + lx6*dsat.pnrq2_m + lx7*dsat.pnrq3_m + lx8*dsat.pnrq4_m\n\n#Intercepts\nsat.pnrq1_w ~ t1*1\nsat.pnrq2_w ~ t2*1\nsat.pnrq3_w ~ t3*1\nsat.pnrq4_w ~ t4*1\ndsat.pnrq1_w ~ t5*1\ndsat.pnrq2_w ~ t6*1\ndsat.pnrq3_w ~ t7*1\ndsat.pnrq4_w ~ t8*1\n\nsat.pnrq1_m ~ t1*1\nsat.pnrq2_m ~ t2*1\nsat.pnrq3_m ~ t3*1\nsat.pnrq4_m ~ t4*1\ndsat.pnrq1_m ~ t5*1\ndsat.pnrq2_m ~ t6*1\ndsat.pnrq3_m ~ t7*1\ndsat.pnrq4_m ~ t8*1\n\n#Residual Variances\nsat.pnrq1_w ~~ th1*sat.pnrq1_w\nsat.pnrq2_w ~~ th2*sat.pnrq2_w\nsat.pnrq3_w ~~ th3*sat.pnrq3_w\nsat.pnrq4_w ~~ th4*sat.pnrq4_w\ndsat.pnrq1_w ~~ th5*dsat.pnrq1_w\ndsat.pnrq2_w ~~ th6*dsat.pnrq2_w\ndsat.pnrq3_w ~~ th7*dsat.pnrq3_w\ndsat.pnrq4_w ~~ th8*dsat.pnrq4_w\n\nsat.pnrq1_m ~~ th9*sat.pnrq1_m\nsat.pnrq2_m ~~ th10*sat.pnrq2_m\nsat.pnrq3_m ~~ th11*sat.pnrq3_m\nsat.pnrq4_m ~~ th12*sat.pnrq4_m\ndsat.pnrq1_m ~~ th13*dsat.pnrq1_m\ndsat.pnrq2_m ~~ th14*dsat.pnrq2_m\ndsat.pnrq3_m ~~ th15*dsat.pnrq3_m\ndsat.pnrq4_m ~~ th16*dsat.pnrq4_m\n\n#Residual Covariances\nsat.pnrq1_w ~~ sat.pnrq1_m\nsat.pnrq2_w ~~ sat.pnrq2_m\nsat.pnrq3_w ~~ sat.pnrq3_m\nsat.pnrq4_w ~~ sat.pnrq4_m\ndsat.pnrq1_w ~~ dsat.pnrq1_m\ndsat.pnrq2_w ~~ dsat.pnrq2_m\ndsat.pnrq3_w ~~ dsat.pnrq3_m\ndsat.pnrq4_w ~~ dsat.pnrq4_m\n\n#Structural Model\n\n#Latent (Co)Variances\npos1 ~~ psv1*pos1 + 1*pos1\nneg1 ~~ psv2*neg1 + 1*neg1\npos2 ~~ psv3*pos2 + NA*pos2\nneg2 ~~ psv4*neg2 + NA*neg2\npos1 ~~ psi12*neg1\npos1 ~~ psi13*pos2\npos1 ~~ psi14*neg2\nneg1 ~~ psi23*pos2\nneg1 ~~ psi24*neg2\npos2 ~~ psi34*neg2\n\n#Latent Means\npos1 ~ a1*1 + 0*1\nneg1 ~ a2*1 + 0*1\npos2 ~ a3*1\nneg2 ~ a4*1"
  )
})

#MV output
test_that("scriptCFA produces correct output for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none and scaleset= MV", {

  pnrq_list <- list(
    lvnames = c(
      "pos", "neg"
    ),
    stem = c(
      "sat.pnrq", "dsat.pnrq"
    ),
    delim1 = c(
      "", ""
    ),
    delim2 = c(
      "_", "_"
    )
  )

  dvn <- scrapeVarCross(
    pnrqM,
    var_list = pnrq_list,
    var_list_order = "sip",
    distinguish_1 = "w",
    distinguish_2 = "m"
  )

  expect_equal(
    scriptCFA(
      dvn,
      scaleset = "MV",
      constr_dy_meas = c("loadings", "intercepts"),
      constr_dy_struct = "none"
    ),
    "#Measurement Model\n\n#Loadings\npos1 =~ 1*sat.pnrq1_w + lx1*sat.pnrq1_w + lx2*sat.pnrq2_w + lx3*sat.pnrq3_w + lx4*sat.pnrq4_w\nneg1 =~ 1*dsat.pnrq1_w + lx5*dsat.pnrq1_w + lx6*dsat.pnrq2_w + lx7*dsat.pnrq3_w + lx8*dsat.pnrq4_w\npos2 =~ NA*sat.pnrq1_m + lx1*sat.pnrq1_m + lx2*sat.pnrq2_m + lx3*sat.pnrq3_m + lx4*sat.pnrq4_m\nneg2 =~ NA*dsat.pnrq1_m + lx5*dsat.pnrq1_m + lx6*dsat.pnrq2_m + lx7*dsat.pnrq3_m + lx8*dsat.pnrq4_m\n\n#Intercepts\nsat.pnrq1_w ~ 0*1\nsat.pnrq1_w ~ t1*1\nsat.pnrq2_w ~ t2*1\nsat.pnrq3_w ~ t3*1\nsat.pnrq4_w ~ t4*1\ndsat.pnrq1_w ~ 0*1\ndsat.pnrq1_w ~ t5*1\ndsat.pnrq2_w ~ t6*1\ndsat.pnrq3_w ~ t7*1\ndsat.pnrq4_w ~ t8*1\n\nsat.pnrq1_m ~ t1*1\nsat.pnrq2_m ~ t2*1\nsat.pnrq3_m ~ t3*1\nsat.pnrq4_m ~ t4*1\ndsat.pnrq1_m ~ t5*1\ndsat.pnrq2_m ~ t6*1\ndsat.pnrq3_m ~ t7*1\ndsat.pnrq4_m ~ t8*1\n\n#Residual Variances\nsat.pnrq1_w ~~ th1*sat.pnrq1_w\nsat.pnrq2_w ~~ th2*sat.pnrq2_w\nsat.pnrq3_w ~~ th3*sat.pnrq3_w\nsat.pnrq4_w ~~ th4*sat.pnrq4_w\ndsat.pnrq1_w ~~ th5*dsat.pnrq1_w\ndsat.pnrq2_w ~~ th6*dsat.pnrq2_w\ndsat.pnrq3_w ~~ th7*dsat.pnrq3_w\ndsat.pnrq4_w ~~ th8*dsat.pnrq4_w\n\nsat.pnrq1_m ~~ th9*sat.pnrq1_m\nsat.pnrq2_m ~~ th10*sat.pnrq2_m\nsat.pnrq3_m ~~ th11*sat.pnrq3_m\nsat.pnrq4_m ~~ th12*sat.pnrq4_m\ndsat.pnrq1_m ~~ th13*dsat.pnrq1_m\ndsat.pnrq2_m ~~ th14*dsat.pnrq2_m\ndsat.pnrq3_m ~~ th15*dsat.pnrq3_m\ndsat.pnrq4_m ~~ th16*dsat.pnrq4_m\n\n#Residual Covariances\nsat.pnrq1_w ~~ sat.pnrq1_m\nsat.pnrq2_w ~~ sat.pnrq2_m\nsat.pnrq3_w ~~ sat.pnrq3_m\nsat.pnrq4_w ~~ sat.pnrq4_m\ndsat.pnrq1_w ~~ dsat.pnrq1_m\ndsat.pnrq2_w ~~ dsat.pnrq2_m\ndsat.pnrq3_w ~~ dsat.pnrq3_m\ndsat.pnrq4_w ~~ dsat.pnrq4_m\n\n#Structural Model\n\n#Latent (Co)Variances\npos1 ~~ psv1*pos1 + NA*pos1\nneg1 ~~ psv2*neg1 + NA*neg1\npos2 ~~ psv3*pos2 + NA*pos2\nneg2 ~~ psv4*neg2 + NA*neg2\npos1 ~~ psi12*neg1\npos1 ~~ psi13*pos2\npos1 ~~ psi14*neg2\nneg1 ~~ psi23*pos2\nneg1 ~~ psi24*neg2\npos2 ~~ psi34*neg2\n\n#Latent Means\npos1 ~ a1*1\nneg1 ~ a2*1\npos2 ~ a3*1\nneg2 ~ a4*1"
  )
})

#Number of parameter estimates (FF)
test_that("scriptCFA produces correct number of parameter estimates for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none and scaleset= FF", {

  pnrq_list <- list(
    lvnames = c(
      "pos", "neg"
    ),
    stem = c(
      "sat.pnrq", "dsat.pnrq"
    ),
    delim1 = c(
      "", ""
    ),
    delim2 = c(
      "_", "_"
    )
  )

  dvn <- scrapeVarCross(
    pnrqM,
    var_list = pnrq_list,
    var_list_order = "sip",
    distinguish_1 = "w",
    distinguish_2 = "m"
  )

  # knowns for model w/ intercepts/latent means:
  # var_num <- 16
  # knowns <- var_num*(var_num+1)/2+var_num

  # Script model w/ `constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "FF"`
  script <- scriptCFA(dvn, constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "FF")

  # Fit model
  mod <- lavaan::cfa(script, data = pnrqM)

  # manually calculated free parameters (see LINK)
  # my_param <- 50

  lav_param <- as.double(lavaan::fitmeasures(mod, "npar"))

  expect_equal(
    lav_param,
    50
  )
})

#Number of df (FF)
test_that("scriptCFA produces correct df for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none and scaleset= FF", {

  pnrq_list <- list(
    lvnames = c(
      "pos", "neg"
    ),
    stem = c(
      "sat.pnrq", "dsat.pnrq"
    ),
    delim1 = c(
      "", ""
    ),
    delim2 = c(
      "_", "_"
    )
  )

  dvn <- scrapeVarCross(
    pnrqM,
    var_list = pnrq_list,
    var_list_order = "sip",
    distinguish_1 = "w",
    distinguish_2 = "m"
  )

  # knowns for model w/ intercepts/latent means:
  # var_num <- 16
  # knowns <- var_num*(var_num+1)/2+var_num

  # Script model w/ `constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "FF"`
  script <- scriptCFA(dvn, constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "FF")

  # Fit model
  mod <- lavaan::cfa(script, data = pnrqM)

  # manually calculated free parameters (see LINK)
  # my_param <- 50

  # calculated df (should be 102)
  # my_df <- knowns-my_param

  lav_df <- as.double(lavaan::fitmeasures(mod, "df"))

  expect_equal(
    lav_df,
    102
  )
})

#FF df = MV df
test_that("scriptCFA produces same df for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none when scaleset= FF, and scaleset = MV", {

  pnrq_list <- list(
    lvnames = c(
      "pos", "neg"
    ),
    stem = c(
      "sat.pnrq", "dsat.pnrq"
    ),
    delim1 = c(
      "", ""
    ),
    delim2 = c(
      "_", "_"
    )
  )

  dvn <- scrapeVarCross(
    pnrqM,
    var_list = pnrq_list,
    var_list_order = "sip",
    distinguish_1 = "w",
    distinguish_2 = "m"
  )

  # Script model w/ `constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "FF"`
  script_ff <- scriptCFA(dvn, constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "FF")

  # Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = pnrqM)

  # get its df
  df_ff <- as.double(lavaan::fitmeasures(mod_ff, "df"))

  # Script model w/ `constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "MV"`
  script_mv <- scriptCFA(dvn, constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "MV")

  # Fit model w MV
  mod_mv <- lavaan::cfa(script_mv, data = pnrqM)

  # get its df
  df_mv <- as.double(lavaan::fitmeasures(mod_mv, "df"))

  # scale setting should have no impact on df
  expect_equal(
    df_ff,
    df_mv
  )
})

#FF chisq = MV chisq
test_that("scriptCFA produces same chisq for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none when scaleset= FF, and scaleset = MV", {

  pnrq_list <- list(
    lvnames = c(
      "pos", "neg"
    ),
    stem = c(
      "sat.pnrq", "dsat.pnrq"
    ),
    delim1 = c(
      "", ""
    ),
    delim2 = c(
      "_", "_"
    )
  )

  dvn <- scrapeVarCross(
    pnrqM,
    var_list = pnrq_list,
    var_list_order = "sip",
    distinguish_1 = "w",
    distinguish_2 = "m"
  )

  # Script model w/ `constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "FF"`
  script_ff <- scriptCFA(dvn, constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "FF")

  # Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = pnrqM)

  # get its df
  chisq_ff <- as.double(lavaan::fitmeasures(mod_ff, "chisq"))

  # Script model w/ `constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "MV"`
  script_mv <- scriptCFA(dvn, constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "MV")

  # Fit model w MV
  mod_mv <- lavaan::cfa(script_mv, data = pnrqM)

  # get its chisq
  chisq_mv <- as.double(lavaan::fitmeasures(mod_mv, "chisq"))

  # scale setting should have no impact on chisq
  expect_equal(
    chisq_ff,
    chisq_mv
  )
})

#### Group 4: Residual Invariance Script Correctness ####
# --------------------------------------------
# These tests verify that scriptCFA generates correct dyadic residual invariance scripts.

#FF output
test_that("scriptCFA produces correct output for constr_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = none, and scaleset= FF", {

  pnrq_list <- list(
    lvnames = c(
      "pos", "neg"
    ),
    stem = c(
      "sat.pnrq", "dsat.pnrq"
    ),
    delim1 = c(
      "", ""
    ),
    delim2 = c(
      "_", "_"
    )
  )

  dvn <- scrapeVarCross(
    pnrqM,
    var_list = pnrq_list,
    var_list_order = "sip",
    distinguish_1 = "w",
    distinguish_2 = "m"
  )

  expect_equal(
    scriptCFA(
      dvn,
      scaleset = "FF",
      constr_dy_meas = c("loadings", "intercepts", "residuals"),
      constr_dy_struct = "none"
    ),
    "#Measurement Model\n\n#Loadings\npos1 =~ NA*sat.pnrq1_w + lx1*sat.pnrq1_w + lx2*sat.pnrq2_w + lx3*sat.pnrq3_w + lx4*sat.pnrq4_w\nneg1 =~ NA*dsat.pnrq1_w + lx5*dsat.pnrq1_w + lx6*dsat.pnrq2_w + lx7*dsat.pnrq3_w + lx8*dsat.pnrq4_w\npos2 =~ NA*sat.pnrq1_m + lx1*sat.pnrq1_m + lx2*sat.pnrq2_m + lx3*sat.pnrq3_m + lx4*sat.pnrq4_m\nneg2 =~ NA*dsat.pnrq1_m + lx5*dsat.pnrq1_m + lx6*dsat.pnrq2_m + lx7*dsat.pnrq3_m + lx8*dsat.pnrq4_m\n\n#Intercepts\nsat.pnrq1_w ~ t1*1\nsat.pnrq2_w ~ t2*1\nsat.pnrq3_w ~ t3*1\nsat.pnrq4_w ~ t4*1\ndsat.pnrq1_w ~ t5*1\ndsat.pnrq2_w ~ t6*1\ndsat.pnrq3_w ~ t7*1\ndsat.pnrq4_w ~ t8*1\n\nsat.pnrq1_m ~ t1*1\nsat.pnrq2_m ~ t2*1\nsat.pnrq3_m ~ t3*1\nsat.pnrq4_m ~ t4*1\ndsat.pnrq1_m ~ t5*1\ndsat.pnrq2_m ~ t6*1\ndsat.pnrq3_m ~ t7*1\ndsat.pnrq4_m ~ t8*1\n\n#Residual Variances\nsat.pnrq1_w ~~ th1*sat.pnrq1_w\nsat.pnrq2_w ~~ th2*sat.pnrq2_w\nsat.pnrq3_w ~~ th3*sat.pnrq3_w\nsat.pnrq4_w ~~ th4*sat.pnrq4_w\ndsat.pnrq1_w ~~ th5*dsat.pnrq1_w\ndsat.pnrq2_w ~~ th6*dsat.pnrq2_w\ndsat.pnrq3_w ~~ th7*dsat.pnrq3_w\ndsat.pnrq4_w ~~ th8*dsat.pnrq4_w\n\nsat.pnrq1_m ~~ th1*sat.pnrq1_m\nsat.pnrq2_m ~~ th2*sat.pnrq2_m\nsat.pnrq3_m ~~ th3*sat.pnrq3_m\nsat.pnrq4_m ~~ th4*sat.pnrq4_m\ndsat.pnrq1_m ~~ th5*dsat.pnrq1_m\ndsat.pnrq2_m ~~ th6*dsat.pnrq2_m\ndsat.pnrq3_m ~~ th7*dsat.pnrq3_m\ndsat.pnrq4_m ~~ th8*dsat.pnrq4_m\n\n#Residual Covariances\nsat.pnrq1_w ~~ sat.pnrq1_m\nsat.pnrq2_w ~~ sat.pnrq2_m\nsat.pnrq3_w ~~ sat.pnrq3_m\nsat.pnrq4_w ~~ sat.pnrq4_m\ndsat.pnrq1_w ~~ dsat.pnrq1_m\ndsat.pnrq2_w ~~ dsat.pnrq2_m\ndsat.pnrq3_w ~~ dsat.pnrq3_m\ndsat.pnrq4_w ~~ dsat.pnrq4_m\n\n#Structural Model\n\n#Latent (Co)Variances\npos1 ~~ psv1*pos1 + 1*pos1\nneg1 ~~ psv2*neg1 + 1*neg1\npos2 ~~ psv3*pos2 + NA*pos2\nneg2 ~~ psv4*neg2 + NA*neg2\npos1 ~~ psi12*neg1\npos1 ~~ psi13*pos2\npos1 ~~ psi14*neg2\nneg1 ~~ psi23*pos2\nneg1 ~~ psi24*neg2\npos2 ~~ psi34*neg2\n\n#Latent Means\npos1 ~ a1*1 + 0*1\nneg1 ~ a2*1 + 0*1\npos2 ~ a3*1\nneg2 ~ a4*1"
  )
})

#MV output
test_that("scriptCFA produces correct output for constr_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = none and scaleset= MV", {

  pnrq_list <- list(
    lvnames = c(
      "pos", "neg"
    ),
    stem = c(
      "sat.pnrq", "dsat.pnrq"
    ),
    delim1 = c(
      "", ""
    ),
    delim2 = c(
      "_", "_"
    )
  )

  dvn <- scrapeVarCross(
    pnrqM,
    var_list = pnrq_list,
    var_list_order = "sip",
    distinguish_1 = "w",
    distinguish_2 = "m"
  )

  expect_equal(
    scriptCFA(
      dvn,
      scaleset = "MV",
      constr_dy_meas = c("loadings", "intercepts", "residuals"),
      constr_dy_struct = "none"
    ),
    "#Measurement Model\n\n#Loadings\npos1 =~ 1*sat.pnrq1_w + lx1*sat.pnrq1_w + lx2*sat.pnrq2_w + lx3*sat.pnrq3_w + lx4*sat.pnrq4_w\nneg1 =~ 1*dsat.pnrq1_w + lx5*dsat.pnrq1_w + lx6*dsat.pnrq2_w + lx7*dsat.pnrq3_w + lx8*dsat.pnrq4_w\npos2 =~ NA*sat.pnrq1_m + lx1*sat.pnrq1_m + lx2*sat.pnrq2_m + lx3*sat.pnrq3_m + lx4*sat.pnrq4_m\nneg2 =~ NA*dsat.pnrq1_m + lx5*dsat.pnrq1_m + lx6*dsat.pnrq2_m + lx7*dsat.pnrq3_m + lx8*dsat.pnrq4_m\n\n#Intercepts\nsat.pnrq1_w ~ 0*1\nsat.pnrq1_w ~ t1*1\nsat.pnrq2_w ~ t2*1\nsat.pnrq3_w ~ t3*1\nsat.pnrq4_w ~ t4*1\ndsat.pnrq1_w ~ 0*1\ndsat.pnrq1_w ~ t5*1\ndsat.pnrq2_w ~ t6*1\ndsat.pnrq3_w ~ t7*1\ndsat.pnrq4_w ~ t8*1\n\nsat.pnrq1_m ~ t1*1\nsat.pnrq2_m ~ t2*1\nsat.pnrq3_m ~ t3*1\nsat.pnrq4_m ~ t4*1\ndsat.pnrq1_m ~ t5*1\ndsat.pnrq2_m ~ t6*1\ndsat.pnrq3_m ~ t7*1\ndsat.pnrq4_m ~ t8*1\n\n#Residual Variances\nsat.pnrq1_w ~~ th1*sat.pnrq1_w\nsat.pnrq2_w ~~ th2*sat.pnrq2_w\nsat.pnrq3_w ~~ th3*sat.pnrq3_w\nsat.pnrq4_w ~~ th4*sat.pnrq4_w\ndsat.pnrq1_w ~~ th5*dsat.pnrq1_w\ndsat.pnrq2_w ~~ th6*dsat.pnrq2_w\ndsat.pnrq3_w ~~ th7*dsat.pnrq3_w\ndsat.pnrq4_w ~~ th8*dsat.pnrq4_w\n\nsat.pnrq1_m ~~ th1*sat.pnrq1_m\nsat.pnrq2_m ~~ th2*sat.pnrq2_m\nsat.pnrq3_m ~~ th3*sat.pnrq3_m\nsat.pnrq4_m ~~ th4*sat.pnrq4_m\ndsat.pnrq1_m ~~ th5*dsat.pnrq1_m\ndsat.pnrq2_m ~~ th6*dsat.pnrq2_m\ndsat.pnrq3_m ~~ th7*dsat.pnrq3_m\ndsat.pnrq4_m ~~ th8*dsat.pnrq4_m\n\n#Residual Covariances\nsat.pnrq1_w ~~ sat.pnrq1_m\nsat.pnrq2_w ~~ sat.pnrq2_m\nsat.pnrq3_w ~~ sat.pnrq3_m\nsat.pnrq4_w ~~ sat.pnrq4_m\ndsat.pnrq1_w ~~ dsat.pnrq1_m\ndsat.pnrq2_w ~~ dsat.pnrq2_m\ndsat.pnrq3_w ~~ dsat.pnrq3_m\ndsat.pnrq4_w ~~ dsat.pnrq4_m\n\n#Structural Model\n\n#Latent (Co)Variances\npos1 ~~ psv1*pos1 + NA*pos1\nneg1 ~~ psv2*neg1 + NA*neg1\npos2 ~~ psv3*pos2 + NA*pos2\nneg2 ~~ psv4*neg2 + NA*neg2\npos1 ~~ psi12*neg1\npos1 ~~ psi13*pos2\npos1 ~~ psi14*neg2\nneg1 ~~ psi23*pos2\nneg1 ~~ psi24*neg2\npos2 ~~ psi34*neg2\n\n#Latent Means\npos1 ~ a1*1\nneg1 ~ a2*1\npos2 ~ a3*1\nneg2 ~ a4*1"
  )
})

#Number of parameter estimates (FF)
test_that("scriptCFA produces correct number of parameter estimates for constr_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = none and scaleset= FF", {

  pnrq_list <- list(
    lvnames = c(
      "pos", "neg"
    ),
    stem = c(
      "sat.pnrq", "dsat.pnrq"
    ),
    delim1 = c(
      "", ""
    ),
    delim2 = c(
      "_", "_"
    )
  )

  dvn <- scrapeVarCross(
    pnrqM,
    var_list = pnrq_list,
    var_list_order = "sip",
    distinguish_1 = "w",
    distinguish_2 = "m"
  )

  # knowns for model w/ intercepts/latent means:
  # var_num <- 16
  # knowns <- var_num*(var_num+1)/2+var_num

  # Script model w/ `constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none", scaleset = "FF"`
  script <- scriptCFA(dvn, constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none", scaleset = "FF")

  # Fit model
  mod <- lavaan::cfa(script, data = pnrqM)

  # manually calculated free parameters (see LINK)
  # my_param <- 42

  lav_param <- as.double(lavaan::fitmeasures(mod, "npar"))

  expect_equal(
    lav_param,
    42
  )
})

#Number of df (FF)
test_that("scriptCFA produces correct df for constr_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = none and scaleset= FF", {

  pnrq_list <- list(
    lvnames = c(
      "pos", "neg"
    ),
    stem = c(
      "sat.pnrq", "dsat.pnrq"
    ),
    delim1 = c(
      "", ""
    ),
    delim2 = c(
      "_", "_"
    )
  )

  dvn <- scrapeVarCross(
    pnrqM,
    var_list = pnrq_list,
    var_list_order = "sip",
    distinguish_1 = "w",
    distinguish_2 = "m"
  )

  # knowns for model w/ intercepts/latent means:
  # var_num <- 16
  # knowns <- var_num*(var_num+1)/2+var_num

  # Script model w/ `constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none", scaleset = "FF"`
  script <- scriptCFA(dvn, constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none", scaleset = "FF")

  # Fit model
  mod <- lavaan::cfa(script, data = pnrqM)

  # manually calculated free parameters (see LINK)
  # my_param <- 42

  # calculated df (should be 110)
  # my_df <- knowns-my_param

  lav_df <- as.double(lavaan::fitmeasures(mod, "df"))

  expect_equal(
    lav_df,
    110
  )
})

#FF df = MV df
test_that("scriptCFA produces same df for constr_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = none when scaleset= FF, and scaleset = MV", {

  pnrq_list <- list(
    lvnames = c(
      "pos", "neg"
    ),
    stem = c(
      "sat.pnrq", "dsat.pnrq"
    ),
    delim1 = c(
      "", ""
    ),
    delim2 = c(
      "_", "_"
    )
  )

  dvn <- scrapeVarCross(
    pnrqM,
    var_list = pnrq_list,
    var_list_order = "sip",
    distinguish_1 = "w",
    distinguish_2 = "m"
  )

  # Script model w/ `constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none", scaleset = "FF"`
  script_ff <- scriptCFA(dvn, constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none", scaleset = "FF")

  # Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = pnrqM)

  # get its df
  df_ff <- as.double(lavaan::fitmeasures(mod_ff, "df"))

  # Script model w/ `constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none", scaleset = "MV"`
  script_mv <- scriptCFA(dvn, constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none", scaleset = "MV")

  # Fit model w MV
  mod_mv <- lavaan::cfa(script_mv, data = pnrqM)

  # get its df
  df_mv <- as.double(lavaan::fitmeasures(mod_mv, "df"))

  # scale setting should have no impact on df
  expect_equal(
    df_ff,
    df_mv
  )
})

#FF chisq = MV chisq
test_that("scriptCFA produces same chisq for constr_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = none when scaleset= FF, and scaleset = MV", {

  pnrq_list <- list(
    lvnames = c(
      "pos", "neg"
    ),
    stem = c(
      "sat.pnrq", "dsat.pnrq"
    ),
    delim1 = c(
      "", ""
    ),
    delim2 = c(
      "_", "_"
    )
  )

  dvn <- scrapeVarCross(
    pnrqM,
    var_list = pnrq_list,
    var_list_order = "sip",
    distinguish_1 = "w",
    distinguish_2 = "m"
  )

  # Script model w/ `constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none", scaleset = "FF"`
  script_ff <- scriptCFA(dvn, constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none", scaleset = "FF")

  # Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = pnrqM)

  # get its df
  chisq_ff <- as.double(lavaan::fitmeasures(mod_ff, "chisq"))

  # Script model w/ `constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none", scaleset = "MV"`
  script_mv <- scriptCFA(dvn, constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none", scaleset = "MV")

  # Fit model w MV
  mod_mv <- lavaan::cfa(script_mv, data = pnrqM)

  # get its chisq
  chisq_mv <- as.double(lavaan::fitmeasures(mod_mv, "chisq"))

  # scale setting should have no impact on chisq
  expect_equal(
    chisq_ff,
    chisq_mv
  )
})


