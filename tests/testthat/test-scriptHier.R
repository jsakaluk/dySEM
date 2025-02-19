# Configural -------------------------------------------------------------------
# ---- FF ----
test_that(
  "scriptHier produces correct output for constr_dy_meas = none, constr_dy_struct = none, and scaleset = FF", {

  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2="_",
    distinguish_1="1",
    distinguish_2="2"
  )

  expect_equal(

    scriptHier(
      dvn,
      scaleset = "FF",
      lvname = "Sat",
      constr_dy_meas = "none",
      constr_dy_struct = "none"
    ),

    "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5\nSat2=~NA*sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5\n\nSat =~ NA*Sat1 + cfx*Sat1 + cfx*Sat2\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat ~~ 1*Sat\nSat1 ~~ 1*Sat1\nSat2 ~~ 1*Sat2\n\n#Latent Means\nSat ~ 0*1\nSat1 ~ 0*1\nSat2 ~ 0*1"
  )

}
)
test_that(
  "scriptHier produces correct number of parameter estimates for consrt_dy_meas = none, constr_dy_struct = none, and scaleset = FF", {

  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2="_",
    distinguish_1="1",
    distinguish_2="2"
  )

  #knowns for model w/ estimated mean structure:
  #var_num <- 10
  #knowns <- (var_num*(var_num+1)/2)+var_num

  #script configural invariance model w/ scaleset = FF
  script <- scriptHier(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = "none",
    constr_dy_struct = "none"
  )

  #fit model
  mod <- lavaan::cfa(script, data = commitmentQ)

  #manually calculated free parameters (see LINK)
  #my_param <- 36

  lav_param <- as.double(
    lavaan::fitmeasures(
      mod,
      "npar")
  )

  expect_equal(
    lav_param,
    36
  )

}
)
test_that(
  "scriptHier produces correct df for constr_dy_meas = none, constr_dy_struct = none, and scaleset = FF", {

  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2="_",
    distinguish_1="1",
    distinguish_2="2"
  )

  #knowns for model w/ estimated mean structure:
  #var_num <- 10
  #knowns <- (var_num*(var_num+1)/2)+var_num

  #script configural invariance model w/ scaleset = FF
  script <- scriptHier(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = "none",
    constr_dy_struct = "none"
  )

  #fit model
  mod <- lavaan::cfa(
    script,
    data = commitmentQ
  )

  #manually calculated free parameters (see LINK)
  #my_param <- 36

  #calculated df (should be 29)
  #my_df <- knowns-my_param

  lav_df <- as.double(
    lavaan::fitmeasures(
      mod,
      "df")
  )

  expect_equal(
    lav_df,
    29
  )

}
)

# ---- MV ----
test_that(
  "scriptHier produces correct output for constr_dy_meas = none, constr_dy_struct = none, and scaleset = MV", {

  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2="_",
    distinguish_1="1",
    distinguish_2="2"
  )

  expect_equal(

    scriptHier(
      dvn,
      scaleset = "MV",
      lvname = "Sat",
      constr_dy_meas = "none",
      constr_dy_struct = "none"
    ),

    "#Measurement Model\n\n#Loadings\nSat1=~1*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5\nSat2=~1*sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5\n\nSat =~ 1*Sat1 + 1*Sat2\n\n#Intercepts\nsat.g.1_1 ~ 0*1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 0*1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat ~~ NA*Sat\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\n\n#Latent Means\nSat ~ 0*1\nSat1 ~ NA*1\nSat2 ~ NA*1"
  )

}
)

# ---- FF vs. MV ----
test_that(#failure
  "scriptHier produces same df for constr_dy_meas = none, constr_dy_struct = none when scaleset= FF, and scaleset = MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  #Script configural invariance model and scaleset = FF
  script_ff <- scriptHier(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "FF")

  #Fit model w FF
  mod_ff <- lavaan::cfa(
    script_ff,
    data = commitmentQ
    )

  #get its df
  df_ff <- as.double(lavaan::fitmeasures(mod_ff, "df"))

  #Script configural invariance model and scaleset = MV
  script_mv <- scriptHier(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "MV")

  #Fit model w MV
  mod_mv <- lavaan::cfa(
    script_mv,
    data = commitmentQ
    )

  #get its df
  df_mv <- as.double(lavaan::fitmeasures(mod_mv, "df"))

  #scale setting should have no impact on df
  expect_equal(df_ff,
               df_mv
  )
}
)
test_that(#pass (but fails in `equal_identical()`)
  "scriptHier produces same chisq for constr_dy_meas = none, constr_dy_struct = none when scaleset= FF, and scaleset = MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  #Script configural invariance model and scaleset = FF
  script_ff <- scriptHier(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "FF")

  #Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  #get its df
  chisq_ff <- as.double(lavaan::fitmeasures(mod_ff, "chisq"))

  #Script configural invariance model and scaleset = MV
  script_mv <- scriptHier(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "MV")

  #Fit model w MV
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  #get its df
  chisq_mv <- as.double(lavaan::fitmeasures(mod_mv, "chisq"))

  #scale setting should have no impact on chisq
  expect_equal(chisq_ff,
               chisq_mv
  )
}
)

# Loading  ---------------------------------------------------------------------
# ---- FF ----
test_that(
  "scriptHier produces correct output for constr_dy_meas = loadings, constr_dy_struct = none, and scaleset = FF", {

  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2="_",
    distinguish_1="1",
    distinguish_2="2"
  )

  expect_equal(

    scriptHier(
      dvn,
      scaleset = "FF",
      lvname = "Sat",
      constr_dy_meas = "loadings",
      constr_dy_struct = "none"
    ),

    "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\nSat =~ NA*Sat1 + cfx*Sat1 + cfx*Sat2\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat ~~ 1*Sat\nSat1 ~~ 1*Sat1\nSat2 ~~ NA*Sat2\n\n#Latent Means\nSat ~ 0*1\nSat1 ~ 0*1\nSat2 ~ 0*1"
  )

}
)
test_that(
  "scriptHier produces correct number of parameter estimates for consrt_dy_meas = loadings, constr_dy_struct = none, and scaleset = FF", {

  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2="_",
    distinguish_1="1",
    distinguish_2="2"
  )

  #knowns for model w/ estimated mean structure:
  #var_num <- 10
  #knowns <- (var_num*(var_num+1)/2)+var_num

  #script loading invariance model w/ scaleset = FF
  script <- scriptHier(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = "loadings",
    constr_dy_struct = "none"
  )

  #fit model
  mod <- lavaan::cfa(script, data = commitmentQ)

  #manually calculated free parameters (see LINK)
  #my_param <- 32

  lav_param <- as.double(
    lavaan::fitmeasures(
      mod,
      "npar")
  )

  expect_equal(
    lav_param,
    32
  )

}
)
test_that(
  "scriptHier produces correct df for constr_dy_meas = loadings, constr_dy_struct = none, and scaleset = FF", {

  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2="_",
    distinguish_1="1",
    distinguish_2="2"
  )

  #knowns for model w/ estimated mean structure:
  #var_num <- 10
  #knowns <- (var_num*(var_num+1)/2)+var_num

  #script loading invariance model w/ scaleset = FF
  script <- scriptHier(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = "loadings",
    constr_dy_struct = "none"
  )

  #fit model
  mod <- lavaan::cfa(
    script,
    data = commitmentQ,
    missing = "fiml" #helps the model converge (lavaan::fitmeasures requires convergence)
  )

  #manually calculated free parameters (see LINK)
  #my_param <- 32

  #calculated df (should be 33)
  #my_df <- knowns-my_param

  lav_df <- as.double(
    lavaan::fitmeasures(
      mod,
      "df")
  )

  expect_equal(
    lav_df,
    33
  )

}
)

# ---- MV ----
test_that(
  "scriptHier produces correct output for constr_dy_meas = loadings, constr_dy_struct = none, and scaleset = MV", {

  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2="_",
    distinguish_1="1",
    distinguish_2="2"
  )

  expect_equal(

    scriptHier(
      dvn,
      scaleset = "MV",
      lvname = "Sat",
      constr_dy_meas = "loadings",
      constr_dy_struct = "none"
    ),

    "#Measurement Model\n\n#Loadings\nSat1=~1*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\nSat =~ 1*Sat1 + 1*Sat2\n\n#Intercepts\nsat.g.1_1 ~ 0*1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 0*1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat ~~ NA*Sat\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\n\n#Latent Means\nSat ~ 0*1\nSat1 ~ NA*1\nSat2 ~ NA*1"
  )

}
)

# ---- FF vs. MV ----
test_that(#failure
  "scriptHier produces same df for constr_dy_meas = loadings, constr_dy_struct = none when scaleset= FF, and scaleset = MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  #Script loading invariance model and scaleset = FF
  script_ff <- scriptHier(dvn, lvname = "Sat", constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "FF")

  #Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  #get its df
  df_ff <- as.double(lavaan::fitmeasures(mod_ff, "df"))

  #Script loading invariance model and scaleset = MV
  script_mv <- scriptHier(dvn, lvname = "Sat", constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "MV")

  #Fit model w MV
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  #get its df
  df_mv <- as.double(lavaan::fitmeasures(mod_mv, "df"))

  #scale setting should have no impact on df
  expect_equal(df_ff,
               df_mv
  )
}
)
test_that(#pass (but fails in `equal_identical()`)
  "scriptHier produces same chisq for constr_dy_meas = loadings, constr_dy_struct = none when scaleset = FF, and scaleset = MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  #Script loading invariance model and scaleset = FF
  script_ff <- scriptHier(dvn, lvname = "Sat", constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "FF")

  #Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  #get its df
  chisq_ff <- as.double(lavaan::fitmeasures(mod_ff, "chisq"))

  #Script loading invariance model and scaleset = MV
  script_mv <- scriptHier(dvn, lvname = "Sat", constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "MV")

  #Fit model w MV
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  #get its df
  chisq_mv <- as.double(lavaan::fitmeasures(mod_mv, "chisq"))

  #scale setting should have no impact on chisq
  expect_equal(chisq_ff,
               chisq_mv
  )
}
)

# Intercept --------------------------------------------------------------------
# ---- FF ----
test_that(
  "scriptHier produces correct output for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none, and scaleset = FF", {

  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2="_",
    distinguish_1="1",
    distinguish_2="2"
  )

  expect_equal(

    scriptHier(
      dvn,
      scaleset = "FF",
      lvname = "Sat",
      constr_dy_meas = c("loadings", "intercepts"),
      constr_dy_struct = "none"
    ),

    "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\nSat =~ NA*Sat1 + cfx*Sat1 + cfx*Sat2\n\n#Intercepts\nsat.g.1_1 ~ tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat ~~ 1*Sat\nSat1 ~~ 1*Sat1\nSat2 ~~ NA*Sat2\n\n#Latent Means\nSat ~ 0*1\nSat1 ~ 0*1\nSat2 ~ NA*1"
  )

}
)
test_that(
  "scriptHier produces correct number of parameter estimates for consrt_dy_meas = c(loadings, intercepts), constr_dy_struct = none, and scaleset = FF", {

  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2="_",
    distinguish_1="1",
    distinguish_2="2"
  )

  #knowns for model w/ estimated mean structure:
  #var_num <- 10
  #knowns <- (var_num*(var_num+1)/2)+var_num

  #script intercept invariance model w/ scaleset = FF
  script <- scriptHier(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = c("loadings", "intercepts"),
    constr_dy_struct = "none"
  )

  #fit model
  mod <- lavaan::cfa(script, data = commitmentQ)

  #manually calculated free parameters (see LINK)
  #my_param <- 28

  lav_param <- as.double(
    lavaan::fitmeasures(
      mod,
      "npar")
  )

  expect_equal(
    lav_param,
    28
  )

}
)
test_that(
  "scriptHier produces correct df for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none, and scaleset = FF", {

  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2="_",
    distinguish_1="1",
    distinguish_2="2"
  )

  #knowns for model w/ estimated mean structure:
  #var_num <- 10
  #knowns <- (var_num*(var_num+1)/2)+var_num

  #script intercept invariance model w/ scaleset = FF
  script <- scriptHier(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = c("loadings", "intercepts"),
    constr_dy_struct = "none"
  )

  #fit model
  mod <- lavaan::cfa(
    script,
    data = commitmentQ,
    missing = "fiml" #helps the model converge (lavaan::fitmeasures requires convergence)
  )

  #manually calculated free parameters (see LINK)
  #my_param <- 28

  #calculated df (should be 37)
  #my_df <- knowns-my_param

  lav_df <- as.double(
    lavaan::fitmeasures(
      mod,
      "df")
  )

  expect_equal(
    lav_df,
    37
  )

}
)

# ---- MV ----
test_that(
  "scriptHier produces correct output for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none, and scaleset = MV", {

  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2="_",
    distinguish_1="1",
    distinguish_2="2"
  )

  expect_equal(

    scriptHier(
      dvn,
      scaleset = "MV",
      lvname = "Sat",
      constr_dy_meas = c("loadings", "intercepts"),
      constr_dy_struct = "none"
    ),

    "#Measurement Model\n\n#Loadings\nSat1=~1*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\nSat =~ 1*Sat1 + 1*Sat2\n\n#Intercepts\nsat.g.1_1 ~ 0*1 + tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat ~~ NA*Sat\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\n\n#Latent Means\nSat ~ 0*1\nSat1 ~ NA*1\nSat2 ~ NA*1"
  )

}
)

# ---- FF vs. MV ----
test_that(#failure
  "scriptHier produces same df for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none when scaleset= FF, and scaleset = MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  #Script intercept invariance model and scaleset = FF
  script_ff <- scriptHier(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "FF")

  #Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  #get its df
  df_ff <- as.double(lavaan::fitmeasures(mod_ff, "df"))

  #Script intercept invariance model and scaleset = MV
  script_mv <- scriptHier(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "MV")

  #Fit model w MV
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  #get its df
  df_mv <- as.double(lavaan::fitmeasures(mod_mv, "df"))

  #scale setting should have no impact on df
  expect_equal(df_ff,
               df_mv
  )
}
)
test_that(#pass (but fails in `equal_identical()`)
  "scriptHier produces same chisq for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none when scaleset = FF, and scaleset = MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  #Script intercept invariance model and scaleset = FF
  script_ff <- scriptHier(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "FF")

  #Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  #get its chisq
  chisq_ff <- as.double(lavaan::fitmeasures(mod_ff, "chisq"))

  #Script intercept invariance model and scaleset = MV
  script_mv <- scriptHier(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "MV")

  #Fit model w MV
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  #get its chisq
  chisq_mv <- as.double(lavaan::fitmeasures(mod_mv, "chisq"))

  #scale setting should have no impact on chisq
  expect_equal(chisq_ff,
               chisq_mv
  )
}
)

# Residual ---------------------------------------------------------------------
# ---- FF ----
test_that(
  "scriptHier produces correct output for constr_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = none, and scaleset = FF", {

  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2="_",
    distinguish_1="1",
    distinguish_2="2"
  )

  expect_equal(

    scriptHier(
      dvn,
      scaleset = "FF",
      lvname = "Sat",
      constr_dy_meas = c("loadings", "intercepts", "residuals"),
      constr_dy_struct = "none"
    ),

    "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\nSat =~ NA*Sat1 + cfx*Sat1 + cfx*Sat2\n\n#Intercepts\nsat.g.1_1 ~ tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ thx1*sat.g.1_1\nsat.g.1_2 ~~ thx2*sat.g.1_2\nsat.g.1_3 ~~ thx3*sat.g.1_3\nsat.g.1_4 ~~ thx4*sat.g.1_4\nsat.g.1_5 ~~ thx5*sat.g.1_5\n\nsat.g.2_1 ~~ thx1*sat.g.2_1\nsat.g.2_2 ~~ thx2*sat.g.2_2\nsat.g.2_3 ~~ thx3*sat.g.2_3\nsat.g.2_4 ~~ thx4*sat.g.2_4\nsat.g.2_5 ~~ thx5*sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat ~~ 1*Sat\nSat1 ~~ 1*Sat1\nSat2 ~~ NA*Sat2\n\n#Latent Means\nSat ~ 0*1\nSat1 ~ 0*1\nSat2 ~ NA*1"
  )

}
)
test_that(
  "scriptHier produces correct number of parameter estimates for consrt_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = none, and scaleset = FF", {

  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2="_",
    distinguish_1="1",
    distinguish_2="2"
  )

  #knowns for model w/ estimated mean structure:
  #var_num <- 10
  #knowns <- (var_num*(var_num+1)/2)+var_num

  #script residual invariance model w/ scaleset = FF
  script <- scriptHier(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = c("loadings", "intercepts", "residuals"),
    constr_dy_struct = "none"
  )

  #fit model
  mod <- lavaan::cfa(script, data = commitmentQ
  )

  #manually calculated free parameters (see LINK)
  #my_param <- 23

  lav_param <- as.double(
    lavaan::fitmeasures(
      mod,
      "npar")
  )

  expect_equal(
    lav_param,
    23
  )

}
)
test_that(
  "scriptHier produces correct df for constr_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = none, and scaleset = FF", {

  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2="_",
    distinguish_1="1",
    distinguish_2="2"
  )

  #knowns for model w/ estimated mean structure:
  #var_num <- 10
  #knowns <- (var_num*(var_num+1)/2)+var_num

  #script residual invariance model w/ scaleset = FF
  script <- scriptHier(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = c("loadings", "intercepts", "residuals"),
    constr_dy_struct = "none"
  )

  #fit model
  mod <- lavaan::cfa(
    script,
    data = commitmentQ
  )

  #manually calculated free parameters (see LINK)
  #my_param <- 23

  #calculated df (should be 42)
  #my_df <- knowns-my_param

  lav_df <- as.double(
    lavaan::fitmeasures(
      mod,
      "df")
  )

  expect_equal(
    lav_df,
    42
  )

}
)

# ---- MV ----
test_that(
  "scriptHier produces correct output for constr_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = none, and scaleset = MV", {

  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2="_",
    distinguish_1="1",
    distinguish_2="2"
  )

  expect_equal(

    scriptHier(
      dvn,
      scaleset = "MV",
      lvname = "Sat",
      constr_dy_meas = c("loadings", "intercepts", "residuals"),
      constr_dy_struct = "none"
    ),

    "#Measurement Model\n\n#Loadings\nSat1=~1*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\nSat =~ 1*Sat1 + 1*Sat2\n\n#Intercepts\nsat.g.1_1 ~ 0*1 + tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ thx1*sat.g.1_1\nsat.g.1_2 ~~ thx2*sat.g.1_2\nsat.g.1_3 ~~ thx3*sat.g.1_3\nsat.g.1_4 ~~ thx4*sat.g.1_4\nsat.g.1_5 ~~ thx5*sat.g.1_5\n\nsat.g.2_1 ~~ thx1*sat.g.2_1\nsat.g.2_2 ~~ thx2*sat.g.2_2\nsat.g.2_3 ~~ thx3*sat.g.2_3\nsat.g.2_4 ~~ thx4*sat.g.2_4\nsat.g.2_5 ~~ thx5*sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat ~~ NA*Sat\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\n\n#Latent Means\nSat ~ 0*1\nSat1 ~ NA*1\nSat2 ~ NA*1"
  )

}
)

# ---- FF vs. MV ----

test_that(#failure
  "scriptHier produces same df for constr_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = none when scaleset= FF, and scaleset = MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  #Script residual invariance model and scaleset = FF
  script_ff <- scriptHier(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none", scaleset = "FF")

  #Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  #get its df
  df_ff <- as.double(lavaan::fitmeasures(mod_ff, "df"))

  #Script residual invariance model and scaleset = MV
  script_mv <- scriptHier(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none", scaleset = "MV")

  #Fit model w MV
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  #get its df
  df_mv <- as.double(lavaan::fitmeasures(mod_mv, "df"))

  #scale setting should have no impact on df
  expect_equal(df_ff,
               df_mv
  )
}
)

test_that(#pass (but fails in `equal_identical()`)
  "scriptHier produces same chisq for constr_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = none when scaleset = FF, and scaleset = MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  #Script residual invariance model and scaleset = FF
  script_ff <- scriptHier(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none", scaleset = "FF")

  #Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  #get its chisq
  chisq_ff <- as.double(lavaan::fitmeasures(mod_ff, "chisq"))

  #Script #Script residual invariance model and scaleset = MV
  script_mv <- scriptHier(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none", scaleset = "MV")

  #Fit model w MV
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  #get its chisq
  chisq_mv <- as.double(lavaan::fitmeasures(mod_mv, "chisq"))

  #scale setting should have no impact on chisq
  expect_equal(chisq_ff,
               chisq_mv
  )
}
)

# Latent Variances -------------------------------------------------------------
# ---- FF ----
test_that(
  "scriptHier produces correct output for constr_dy_meas = loadings, constr_dy_struct = variances and scaleset = FF", {

  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2="_",
    distinguish_1="1",
    distinguish_2="2"
  )

  expect_equal(

    scriptHier(
      dvn,
      scaleset = "FF",
      lvname = "Sat",
      constr_dy_meas = "loadings",
      constr_dy_struct = "variances"
    ),

    "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\nSat =~ NA*Sat1 + cfx*Sat1 + cfx*Sat2\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat ~~ 1*Sat\nSat1 ~~ 1*Sat1\nSat2 ~~ 1*Sat2\n\n#Latent Means\nSat ~ 0*1\nSat1 ~ 0*1\nSat2 ~ 0*1"
  )

}
)
test_that(
  "scriptHier produces correct number of parameter estimates for constr_dy_meas = loadings, constr_dy_struct = variances and scaleset = FF", {

  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2="_",
    distinguish_1="1",
    distinguish_2="2"
  )

  #knowns for model w/ estimated mean structure:
  #var_num <- 10
  #knowns <- (var_num*(var_num+1)/2)+var_num

  #script lvars equality model w/ scaleset = FF
  script <- scriptHier(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = "loadings",
    constr_dy_struct = "variances"
  )

  #fit model
  mod <- lavaan::cfa(script, data = commitmentQ,
                     missing = "fiml"
  )

  #manually calculated free parameters (see LINK)
  #my_param <- 31

  lav_param <- as.double(
    lavaan::fitmeasures(
      mod,
      "npar")
  )

  expect_equal(
    lav_param,
    31
  )

}
)
test_that(
  "scriptHier produces correct df for constr_dy_meas = loadings, constr_dy_struct = variances and scaleset = FF", {

  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2="_",
    distinguish_1="1",
    distinguish_2="2"
  )

  #knowns for model w/ estimated mean structure:
  #var_num <- 10
  #knowns <- (var_num*(var_num+1)/2)+var_num

  #script lvars equality model w/ scaleset = FF
  script <- scriptHier(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = "loadings",
    constr_dy_struct = "variances"
  )

  #fit model
  mod <- lavaan::cfa(
    script,
    data = commitmentQ
  )

  #manually calculated free parameters (see LINK)
  #my_param <- 31

  #calculated df (should be 34)
  #my_df <- knowns-my_param

  lav_df <- as.double(
    lavaan::fitmeasures(
      mod,
      "df")
  )

  expect_equal(
    lav_df,
    34
  )

}
)

# ---- MV ----
test_that(
  "scriptHier produces correct output for constr_dy_meas = loadings, constr_dy_struct = variances and scaleset = MV", {

  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2="_",
    distinguish_1="1",
    distinguish_2="2"
  )

  expect_equal(

    scriptHier(
      dvn,
      scaleset = "MV",
      lvname = "Sat",
      constr_dy_meas = "loadings",
      constr_dy_struct = "variances"
    ),

    "#Measurement Model\n\n#Loadings\nSat1=~1*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\nSat =~ 1*Sat1 + 1*Sat2\n\n#Intercepts\nsat.g.1_1 ~ 0*1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 0*1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat ~~ NA*Sat\nSat1 ~~ psix*Sat1\nSat2 ~~ psix*Sat2\n\n#Latent Means\nSat ~ 0*1\nSat1 ~ NA*1\nSat2 ~ NA*1"
  )

}
)

# ---- FF vs. MV ----
test_that(
  "scriptHier produces same df for constr_dy_meas = loadings, constr_dy_struct = variances when scaleset= FF, and scaleset = MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  #Script lvars equality model and scaleset = FF
  script_ff <- scriptHier(dvn, lvname = "Sat", constr_dy_meas = "loadings", constr_dy_struct = "variances", scaleset = "FF")

  #Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  #get its df
  df_ff <- as.double(lavaan::fitmeasures(mod_ff, "df"))

  #Script lvars equality model and scaleset = MV
  script_mv <- scriptHier(dvn, lvname = "Sat", constr_dy_meas = "loadings", constr_dy_struct = "variances", scaleset = "MV")

  #Fit model w MV
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  #get its df
  df_mv <- as.double(lavaan::fitmeasures(mod_mv, "df"))

  #scale setting should have no impact on df
  expect_equal(df_ff,
               df_mv
  )
}
)
test_that(
  "scriptHier produces same chisq for constr_dy_meas = loadings, constr_dy_struct = variances when scaleset= FF, and scaleset = MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  #Script lvars equality model and scaleset = FF
  script_ff <- scriptHier(dvn, lvname = "Sat", constr_dy_meas = "loadings", constr_dy_struct = "variances", scaleset = "FF")

  #Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  #get its chisq
  chisq_ff <- as.double(lavaan::fitmeasures(mod_ff, "chisq"))

  #Script lvars equality model and scaleset = MV
  script_mv <- scriptHier(dvn, lvname = "Sat", constr_dy_meas = "loadings", constr_dy_struct = "variances", scaleset = "MV")

  #Fit model w MV
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  #get its chisq
  chisq_mv <- as.double(lavaan::fitmeasures(mod_mv, "chisq"))

  #scale setting should have no impact on chisq
  expect_equal(chisq_ff,
               chisq_mv
  )
}
)

# Latent Means -----------------------------------------------------------------
# ---- FF ----
test_that(
  "scriptHier produces correct output for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = means and scaleset = FF", {

  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2="_",
    distinguish_1="1",
    distinguish_2="2"
  )

  expect_equal(

    scriptHier(
      dvn,
      scaleset = "FF",
      lvname = "Sat",
      constr_dy_meas = c("loadings", "intercepts"),
      constr_dy_struct = "means"
    ),

    "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\nSat =~ NA*Sat1 + cfx*Sat1 + cfx*Sat2\n\n#Intercepts\nsat.g.1_1 ~ tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat ~~ 1*Sat\nSat1 ~~ 1*Sat1\nSat2 ~~ NA*Sat2\n\n#Latent Means\nSat ~ 0*1\nSat1 ~ 0*1 + alphax*1\nSat2 ~ alphax*1"
  )

}
)
test_that(#failure
  "scriptHier produces correct number of parameter estimates for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = means and scaleset = FF", {

  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2="_",
    distinguish_1="1",
    distinguish_2="2"
  )

  #knowns for model w/ estimated mean structure:
  #var_num <- 10
  #knowns <- (var_num*(var_num+1)/2)+var_num

  #script lmeans equivality model w/ scaleset = FF
  script <- scriptHier(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = c("loadings", "intercepts"),
    constr_dy_struct = "means"
  )

  #fit model
  mod <- lavaan::cfa(script, data = commitmentQ,
                     missing = "fiml"
  )

  #manually calculated free parameters (see LINK)
  #my_param <- 27

  lav_param <- as.double(
    lavaan::fitmeasures(
      mod,
      "npar")
  )

  expect_equal(
    lav_param,
    27
  )

}
)
test_that(#failure
  "scriptHier produces correct df for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = means and scaleset = FF", {

  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2="_",
    distinguish_1="1",
    distinguish_2="2"
  )

  #knowns for model w/ estimated mean structure:
  #var_num <- 10
  #knowns <- (var_num*(var_num+1)/2)+var_num

  #script lmeans equality model w/ scaleset = FF
  script <- scriptHier(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = c("loadings", "intercepts"),
    constr_dy_struct = "means"
  )

  #fit model
  mod <- lavaan::cfa(
    script,
    data = commitmentQ
  )

  #manually calculated free parameters (see LINK)
  #my_param <- 27

  #calculated df (should be 38)
  #my_df <- knowns-my_param

  lav_df <- as.double(
    lavaan::fitmeasures(
      mod,
      "df")
  )

  expect_equal(
    lav_df,
    38
  )

}
)

# ---- MV ----
test_that(
  "scriptHier produces correct output for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = means and scaleset = MV", {

  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2="_",
    distinguish_1="1",
    distinguish_2="2"
  )

  expect_equal(

    scriptHier(
      dvn,
      scaleset = "MV",
      lvname = "Sat",
      constr_dy_meas = c("loadings", "intercepts"),
      constr_dy_struct = "means"
    ),

    "#Measurement Model\n\n#Loadings\nSat1=~1*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\nSat =~ 1*Sat1 + 1*Sat2\n\n#Intercepts\nsat.g.1_1 ~ 0*1 + tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat ~~ NA*Sat\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\n\n#Latent Means\nSat ~ 0*1\nSat1 ~ alphax*1\nSat2 ~ alphax*1"
  )

}
)

# ---- FF vs. MV ----
test_that(#failure
  "scriptHier produces same df for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = means when scaleset= FF, and scaleset = MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  #Script lmeans equality model and scaleset = FF
  script_ff <- scriptHier(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "means", scaleset = "FF")

  #Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  #get its df
  df_ff <- as.double(lavaan::fitmeasures(mod_ff, "df"))

  #Script lmeans equality model and scaleset = MV
  script_mv <- scriptHier(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "means", scaleset = "MV")

  #Fit model w MV
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  #get its df
  df_mv <- as.double(lavaan::fitmeasures(mod_mv, "df"))

  #scale setting should have no impact on df
  expect_equal(df_ff,
               df_mv
  )
}
)
test_that(#pass (but fails in `equal_identical()`)
  "scriptHier produces same chisq for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = means when scaleset= FF, and scaleset = MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  #Script lmeans equality model and scaleset = FF
  script_ff <- scriptHier(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "means", scaleset = "FF")

  #Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  #get its chisq
  chisq_ff <- as.double(lavaan::fitmeasures(mod_ff, "chisq"))

  #Script lmeans equality model and scaleset = MV
  script_mv <- scriptHier(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "means", scaleset = "MV")

  #Fit model w MV
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  #get its chisq
  chisq_mv <- as.double(lavaan::fitmeasures(mod_mv, "chisq"))

  #scale setting should have no impact on df
  expect_equal(chisq_ff,
               chisq_mv
  )
}
)
