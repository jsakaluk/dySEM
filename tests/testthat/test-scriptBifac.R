# Configural -------------------------------------------------------------------
# ---- FF ----
test_that(
  "scriptBifac produces correct output for constr_dy_meas = none, constr_dy_struct = none, and scaleset = FF", {

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

    scriptBifac(
      dvn,
      scaleset = "FF",
      lvname = "Sat",
      constr_dy_meas = "none",
      constr_dy_struct = "none"
    ),

    "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5\nSat2=~NA*sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5\nSatDy=~NA*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5+sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances (Orthogonal Structure)\nSat1 ~~ 1*Sat1\nSat2 ~~ 1*Sat2\nSatDy ~~ 1*SatDy\n\nSatDy ~~ 0*Sat1\nSatDy ~~ 0*Sat2\nSat1 ~~ 0*Sat2\n\n#Latent Means\nSat1 ~ 0*1\nSat2 ~ 0*1\nSatDy ~ 0*1"
  )

}
)
test_that(
  "scriptBifac produces correct number of parameter estimates for consrt_dy_meas = none, constr_dy_struct = none, and scaleset = FF", {

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
  script <- scriptBifac(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = "none",
    constr_dy_struct = "none"
  )

  #fit model
  mod <- lavaan::cfa(script, data = commitmentQ,
                     missing = "fiml" #lavaan::fitmeasures requires convergence
  )

  #manually calculated free parameters (see LINK)
  #my_param <- 45

  lav_param <- as.double(
    lavaan::fitmeasures(
      mod,
      "npar")
  )

  expect_equal(
    lav_param,
    45
  )

}
)
test_that(
  "scriptBifac produces correct df for constr_dy_meas = none, constr_dy_struct = none, and scaleset = FF", {

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
  script <- scriptBifac(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = "none",
    constr_dy_struct = "none"
  )

  #fit model
  mod <- lavaan::cfa(
    script,
    data = commitmentQ,
    missing = "fiml" #for convergence
  )

  #manually calculated free parameters (see LINK)
  #my_param <- 45

  #calculated df (should be 20)
  #my_df <- knowns-my_param

  lav_df <- as.double(
    lavaan::fitmeasures(
      mod,
      "df")
  )

  expect_equal(
    lav_df,
    20
  )

}
)

# ---- MV ----
test_that(
  "scriptBifac produces correct output for constr_dy_meas = none, constr_dy_struct = none, and scaleset = MV", {

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

    scriptBifac(
      dvn,
      scaleset = "MV",
      lvname = "Sat",
      constr_dy_meas = "none",
      constr_dy_struct = "none"
    ),

    "#Measurement Model\n\n#Loadings\nSat1=~1*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5\nSat2=~1*sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5\nSatDy=~1*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5+sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 0*1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances (Orthogonal Structure)\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nSatDy ~~ NA*SatDy\n\nSatDy ~~ 0*Sat1\nSatDy ~~ 0*Sat2\nSat1 ~~ 0*Sat2\n\n#Latent Means\nSat1 ~ NA*1\nSat2 ~ NA*1\nSatDy ~ 0*1"
  )

}
)

# ---- FF vs. MV ----
test_that(#failure
  "scriptBifac produces same df for constr_dy_meas = none, constr_dy_struct = none when scaleset= FF, and scaleset = MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "com", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  #Script configural invariance model and scaleset = FF
  script_ff <- scriptBifac(dvn, lvname = "Com", constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "FF")

  #Fit model w FF
  mod_ff <- lavaan::cfa(
    script_ff,
    data = commitmentQ,
    missing = "fiml"
    )

  #get its df
  df_ff <- as.double(lavaan::fitmeasures(mod_ff, "df"))

  #Script configural invariance model and scaleset = MV
  script_mv <- scriptBifac(dvn, lvname = "Com", constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "MV")

  #Fit model w MV
  mod_mv <- lavaan::cfa(
    script_mv,
    data = commitmentQ,
    missing = "fiml"
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
  "scriptBifac produces same chisq for constr_dy_meas = none, constr_dy_struct = none when scaleset= FF, and scaleset = MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "com", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  #Script configural invariance model and scaleset = FF
  script_ff <- scriptBifac(dvn, lvname = "Com", constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "FF")

  #Fit model w FF
  mod_ff <- lavaan::cfa(
    script_ff,
    data = commitmentQ,
    missing = "fiml"
  )

  #get its df
  chisq_ff <- as.double(lavaan::fitmeasures(mod_ff, "chisq"))

  #Script configural invariance model and scaleset = MV
  script_mv <- scriptBifac(dvn, lvname = "Com", constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "MV")

  #Fit model w MV
  mod_mv <- lavaan::cfa(
    script_mv,
    data = commitmentQ,
    missing = "fiml"
  )

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
  "scriptBifac produces correct output for constr_dy_meas = loadings, constr_dy_struct = none, and scaleset = FF", {

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

    scriptBifac(
      dvn,
      scaleset = "FF",
      lvname = "Sat",
      constr_dy_meas = "loadings",
      constr_dy_struct = "none"
    ),

    "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\nSatDy=~NA*sat.g.1_1+lxg1*sat.g.1_1+lxg2*sat.g.1_2+lxg3*sat.g.1_3+lxg4*sat.g.1_4+lxg5*sat.g.1_5+lxg1*sat.g.2_1+lxg2*sat.g.2_2+lxg3*sat.g.2_3+lxg4*sat.g.2_4+lxg5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances (Orthogonal Structure)\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nSatDy ~~ 1*SatDy\n\nSatDy ~~ 0*Sat1\nSatDy ~~ 0*Sat2\nSat1 ~~ 0*Sat2\n\n#Latent Means\nSat1 ~ 0*1\nSat2 ~ 0*1\nSatDy ~ 0*1"
  )

}
)
test_that(
  "scriptBifac produces correct number of parameter estimates for consrt_dy_meas = loadings, constr_dy_struct = none, and scaleset = FF", {

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
  script <- scriptBifac(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = "loadings",
    constr_dy_struct = "none"
  )

  #fit model
  mod <- lavaan::cfa(script, data = commitmentQ,
                     missing = "fiml" #for convergence
  )

  #manually calculated free parameters (see LINK)
  #my_param <- 37

  lav_param <- as.double(
    lavaan::fitmeasures(
      mod,
      "npar")
  )

  expect_equal(
    lav_param,
    37
  )

}
)
test_that(
  "scriptBifac produces correct df for constr_dy_meas = loadings, constr_dy_struct = none, and scaleset = FF", {

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
  script <- scriptBifac(
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
    missing = "fiml" #for convergence
  )

  #manually calculated free parameters (see LINK)
  #my_param <- 37

  #calculated df (should be 28)
  #my_df <- knowns-my_param

  lav_df <- as.double(
    lavaan::fitmeasures(
      mod,
      "df")
  )

  expect_equal(
    lav_df,
    28
  )

}
)

# ---- MV ----
test_that(
  "scriptBifac produces correct output for constr_dy_meas = loadings, constr_dy_struct = none, and scaleset = MV", {

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

    scriptBifac(
      dvn,
      scaleset = "MV",
      lvname = "Sat",
      constr_dy_meas = "loadings",
      constr_dy_struct = "none"
    ),

    "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\nSatDy=~1*sat.g.1_1+lxg1*sat.g.1_1+lxg2*sat.g.1_2+lxg3*sat.g.1_3+lxg4*sat.g.1_4+lxg5*sat.g.1_5+lxg1*sat.g.2_1+lxg2*sat.g.2_2+lxg3*sat.g.2_3+lxg4*sat.g.2_4+lxg5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 0*1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances (Orthogonal Structure)\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nSatDy ~~ NA*SatDy\n\nSatDy ~~ 0*Sat1\nSatDy ~~ 0*Sat2\nSat1 ~~ 0*Sat2\n\n#Latent Means\nSat1 ~ NA*1\nSat2 ~ NA*1\nSatDy ~ 0*1"
  )

}
)

# ---- FF vs. MV ----
test_that(
  "scriptBifac produces same df for constr_dy_meas = loadings, constr_dy_struct = none when scaleset= FF, and scaleset = MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "com", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  #Script loading invariance model and scaleset = FF
  script_ff <- scriptBifac(dvn, lvname = "Com", constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "FF")

  #Fit model w FF
  mod_ff <- lavaan::cfa(
    script_ff,
    data = commitmentQ,
    missing = "fiml"
  )

  #get its df
  df_ff <- as.double(lavaan::fitmeasures(mod_ff, "df"))

  #Script loading invariance and scaleset = MV
  script_mv <- scriptBifac(dvn, lvname = "Com", constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "MV")

  #Fit model w MV
  mod_mv <- lavaan::cfa(
    script_mv,
    data = commitmentQ,
    missing = "fiml"
  )

  #get its df
  df_mv <- as.double(lavaan::fitmeasures(mod_mv, "df"))

  #scale setting should have no impact on df
  expect_equal(df_ff,
               df_mv
  )
}
)
test_that(
  "scriptBifac produces same chisq for constr_dy_meas = loadings, constr_dy_struct = none when scaleset= FF, and scaleset = MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "com", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  #Script model w/ constr_dy_meas = "loadings" and scaleset = FF
  script_ff <- scriptBifac(dvn, lvname = "Com", constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "FF")

  #Fit model w FF
  mod_ff <- lavaan::cfa(
    script_ff,
    data = commitmentQ,
    missing = "fiml"
  )

  #get its chisq
  chisq_ff <- as.double(lavaan::fitmeasures(mod_ff, "chisq"))

  #Script model w/ constr_dy_meas = "loadings" and scaleset = MV
  script_mv <- scriptBifac(dvn, lvname = "Com", constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "MV")

  #Fit model w MV
  mod_mv <- lavaan::cfa(
    script_mv,
    data = commitmentQ,
    missing = "fiml"
  )

  #get its chisq
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
  "scriptBifac produces correct output for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none, and scaleset = FF", {

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

    scriptBifac(
      dvn,
      scaleset = "FF",
      lvname = "Sat",
      constr_dy_meas = c("loadings", "intercepts"),
      constr_dy_struct = "none"
    ),

    "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\nSatDy=~NA*sat.g.1_1+lxg1*sat.g.1_1+lxg2*sat.g.1_2+lxg3*sat.g.1_3+lxg4*sat.g.1_4+lxg5*sat.g.1_5+lxg1*sat.g.2_1+lxg2*sat.g.2_2+lxg3*sat.g.2_3+lxg4*sat.g.2_4+lxg5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances (Orthogonal Structure)\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nSatDy ~~ 1*SatDy\n\nSatDy ~~ 0*Sat1\nSatDy ~~ 0*Sat2\nSat1 ~~ 0*Sat2\n\n#Latent Means\nSat1 ~ NA*1\nSat2 ~ NA*1\nSatDy ~ 0*1"
  )

}
)
test_that(
  "scriptBifac produces correct number of parameter estimates for consrt_dy_meas = c(loadings, intercepts), constr_dy_struct = none, and scaleset = FF", {

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
  script <- scriptBifac(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = c("loadings", "intercepts"),
    constr_dy_struct = "none"
  )

  #fit model
  mod <- lavaan::cfa(script, data = commitmentQ,
                     missing = "fiml" #for convergence
  )

  #manually calculated free parameters (see LINK)
  #my_param <- 34

  lav_param <- as.double(
    lavaan::fitmeasures(
      mod,
      "npar")
  )

  expect_equal(
    lav_param,
    34
  )

}
)
test_that(
  "scriptBifac produces correct df for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none, and scaleset = FF", {

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
  script <- scriptBifac(
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
    missing = "fiml" #for convergence
  )

  #manually calculated free parameters (see LINK)
  #my_param <- 37

  #calculated df (should be 31)
  #my_df <- knowns-my_param

  lav_df <- as.double(
    lavaan::fitmeasures(
      mod,
      "df")
  )

  expect_equal(
    lav_df,
    31
  )

}
)

# ---- MV ----
test_that(
  "scriptBifac produces correct output for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none, and scaleset = MV", {

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

    scriptBifac(
      dvn,
      scaleset = "MV",
      lvname = "Sat",
      constr_dy_meas = c("loadings", "intercepts"),
      constr_dy_struct = "none"
    ),

    "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\nSatDy=~1*sat.g.1_1+lxg1*sat.g.1_1+lxg2*sat.g.1_2+lxg3*sat.g.1_3+lxg4*sat.g.1_4+lxg5*sat.g.1_5+lxg1*sat.g.2_1+lxg2*sat.g.2_2+lxg3*sat.g.2_3+lxg4*sat.g.2_4+lxg5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1 + tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances (Orthogonal Structure)\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nSatDy ~~ NA*SatDy\n\nSatDy ~~ 0*Sat1\nSatDy ~~ 0*Sat2\nSat1 ~~ 0*Sat2\n\n#Latent Means\nSat1 ~ NA*1\nSat2 ~ NA*1\nSatDy ~ NA*1"
  )

}
)

# ---- FF vs. MV ----
test_that(#failure
  "scriptBifac produces same df for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none when scaleset= FF, and scaleset = MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "com", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  #Script model w/ constr_dy_meas = c("loadings", "intercepts") and scaleset = FF
  script_ff <- scriptBifac(dvn, lvname = "Com", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "FF")

  #Fit model w FF
  mod_ff <- lavaan::cfa(
    script_ff,
    data = commitmentQ,
    missing = "fiml"
  )

  #get its df
  df_ff <- as.double(lavaan::fitmeasures(mod_ff, "df"))

  #Script model w/ constr_dy_meas = c("loadings", "intercepts") and scaleset = MV
  script_mv <- scriptBifac(dvn, lvname = "Com", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "MV")

  #Fit model w MV
  mod_mv <- lavaan::cfa(
    script_mv,
    data = commitmentQ,
    missing = "fiml"
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
  "scriptBifac produces same chisq for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none when scaleset= FF, and scaleset = MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "com", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  #Script intercept invariance model and scaleset = FF
  script_ff <- scriptBifac(dvn, lvname = "Com", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "FF")

  #Fit model w FF
  mod_ff <- lavaan::cfa(
    script_ff,
    data = commitmentQ,
    missing = "fiml"
  )

  #get its chisq
  chisq_ff <- as.double(lavaan::fitmeasures(mod_ff, "chisq"))

  #Script intercept invariance model and scaleset = MV
  script_mv <- scriptBifac(dvn, lvname = "Com", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "MV")

  #Fit model w MV
  mod_mv <- lavaan::cfa(
    script_mv,
    data = commitmentQ,
    missing = "fiml"
  )

  #get its chisq
  chisq_mv <- as.double(lavaan::fitmeasures(mod_mv, "chisq"))

  #scale setting should have no impact on chisq
  expect_equal(chisq_ff,
               chisq_mv
  )
}
)

# Residual ----------------------------------------------------------------
# ---- FF ----
test_that(
  "scriptBifac produces correct output for constr_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = none, and scaleset = FF", {

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

    scriptBifac(
      dvn,
      scaleset = "FF",
      lvname = "Sat",
      constr_dy_meas = c("loadings", "intercepts", "residuals"),
      constr_dy_struct = "none"
    ),

    "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\nSatDy=~NA*sat.g.1_1+lxg1*sat.g.1_1+lxg2*sat.g.1_2+lxg3*sat.g.1_3+lxg4*sat.g.1_4+lxg5*sat.g.1_5+lxg1*sat.g.2_1+lxg2*sat.g.2_2+lxg3*sat.g.2_3+lxg4*sat.g.2_4+lxg5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ thx1*sat.g.1_1\nsat.g.1_2 ~~ thx2*sat.g.1_2\nsat.g.1_3 ~~ thx3*sat.g.1_3\nsat.g.1_4 ~~ thx4*sat.g.1_4\nsat.g.1_5 ~~ thx5*sat.g.1_5\n\nsat.g.2_1 ~~ thx1*sat.g.2_1\nsat.g.2_2 ~~ thx2*sat.g.2_2\nsat.g.2_3 ~~ thx3*sat.g.2_3\nsat.g.2_4 ~~ thx4*sat.g.2_4\nsat.g.2_5 ~~ thx5*sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances (Orthogonal Structure)\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nSatDy ~~ 1*SatDy\n\nSatDy ~~ 0*Sat1\nSatDy ~~ 0*Sat2\nSat1 ~~ 0*Sat2\n\n#Latent Means\nSat1 ~ NA*1\nSat2 ~ NA*1\nSatDy ~ 0*1"
  )

}
)
test_that(
  "scriptBifac produces correct number of parameter estimates for consrt_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = none, and scaleset = FF", {

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
  script <- scriptBifac(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = c("loadings", "intercepts", "residuals"),
    constr_dy_struct = "none"
  )

  #fit model
  mod <- lavaan::cfa(script, data = commitmentQ,
                     missing = "fiml" #for convergence
  )

  #manually calculated free parameters (see LINK)
  #my_param <- 29

  lav_param <- as.double(
    lavaan::fitmeasures(
      mod,
      "npar")
  )

  expect_equal(
    lav_param,
    29
  )

}
)
test_that(
  "scriptBifac produces correct df for constr_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = none, and scaleset = FF", {

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
  script <- scriptBifac(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = c("loadings", "intercepts", "residuals"),
    constr_dy_struct = "none"
  )

  #fit model
  mod <- lavaan::cfa(
    script,
    data = commitmentQ,
    missing = "fiml" #for convergence
  )

  #manually calculated free parameters (see LINK)
  #my_param <- 29

  #calculated df (should be 36)
  #my_df <- knowns-my_param

  lav_df <- as.double(
    lavaan::fitmeasures(
      mod,
      "df")
  )

  expect_equal(
    lav_df,
    36
  )

}
)

# ---- MV ----
test_that(
  "scriptBifac produces correct output for constr_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = none, and scaleset = MV", {

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

    scriptBifac(
      dvn,
      scaleset = "MV",
      lvname = "Sat",
      constr_dy_meas = c("loadings", "intercepts", "residuals"),
      constr_dy_struct = "none"
    ),

    "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\nSatDy=~1*sat.g.1_1+lxg1*sat.g.1_1+lxg2*sat.g.1_2+lxg3*sat.g.1_3+lxg4*sat.g.1_4+lxg5*sat.g.1_5+lxg1*sat.g.2_1+lxg2*sat.g.2_2+lxg3*sat.g.2_3+lxg4*sat.g.2_4+lxg5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1 + tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ thx1*sat.g.1_1\nsat.g.1_2 ~~ thx2*sat.g.1_2\nsat.g.1_3 ~~ thx3*sat.g.1_3\nsat.g.1_4 ~~ thx4*sat.g.1_4\nsat.g.1_5 ~~ thx5*sat.g.1_5\n\nsat.g.2_1 ~~ thx1*sat.g.2_1\nsat.g.2_2 ~~ thx2*sat.g.2_2\nsat.g.2_3 ~~ thx3*sat.g.2_3\nsat.g.2_4 ~~ thx4*sat.g.2_4\nsat.g.2_5 ~~ thx5*sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances (Orthogonal Structure)\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nSatDy ~~ NA*SatDy\n\nSatDy ~~ 0*Sat1\nSatDy ~~ 0*Sat2\nSat1 ~~ 0*Sat2\n\n#Latent Means\nSat1 ~ NA*1\nSat2 ~ NA*1\nSatDy ~ NA*1"
  )

}
)

# ---- FF vs. MV ----
test_that(#failure
  "scriptBifac produces same df for constr_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = none when scaleset= FF, and scaleset = MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "com", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  #Script residual invariance model and scaleset = FF
  script_ff <- scriptBifac(dvn, lvname = "Com", constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none", scaleset = "FF")

  #Fit model w FF
  mod_ff <- lavaan::cfa(
    script_ff,
    data = commitmentQ,
    missing = "fiml"
  )

  #get its df
  df_ff <- as.double(lavaan::fitmeasures(mod_ff, "df"))

  #Script residual invariance and scaleset = MV
  script_mv <- scriptBifac(dvn, lvname = "Com", constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none", scaleset = "MV")

  #Fit model w MV
  mod_mv <- lavaan::cfa(
    script_mv,
    data = commitmentQ,
    missing = "fiml"
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
  "scriptBifac produces same chisq for constr_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = none when scaleset= FF, and scaleset = MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "com", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  #Script residual invariance model and scaleset = FF
  script_ff <- scriptBifac(dvn, lvname = "Com", constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none", scaleset = "FF")

  #Fit model w FF
  mod_ff <- lavaan::cfa(
    script_ff,
    data = commitmentQ,
    missing = "fiml"
  )

  #get its chisq
  chisq_ff <- as.double(lavaan::fitmeasures(mod_ff, "chisq"))

  #Script residual invariance model and scaleset = MV
  script_mv <- scriptBifac(dvn, lvname = "Com", constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none", scaleset = "MV")

  #Fit model w MV
  mod_mv <- lavaan::cfa(
    script_mv,
    data = commitmentQ,
    missing = "fiml"
  )

  #get its chisq
  chisq_mv <- as.double(lavaan::fitmeasures(mod_mv, "chisq"))

  #scale setting should have no impact on chisq
  expect_equal(chisq_ff,
               chisq_mv
  )
}
)

# Loadings: Source --------------------------------------------------------
# ---- FF ----
test_that(
  "scriptBifac produces correct output for constr_dy_meas = loadings_source, constr_dy_struct = none, and scaleset = FF", {

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

    scriptBifac(
      dvn,
      scaleset = "FF",
      lvname = "Sat",
      constr_dy_meas = "loadings_source",
      constr_dy_struct = "none"
    ),

    "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx6*sat.g.2_1+lx7*sat.g.2_2+lx8*sat.g.2_3+lx9*sat.g.2_4+lx10*sat.g.2_5\nSatDy=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5+lx6*sat.g.2_1+lx7*sat.g.2_2+lx8*sat.g.2_3+lx9*sat.g.2_4+lx10*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances (Orthogonal Structure)\nSat1 ~~ 1*Sat1\nSat2 ~~ 1*Sat2\nSatDy ~~ 1*SatDy\n\nSatDy ~~ 0*Sat1\nSatDy ~~ 0*Sat2\nSat1 ~~ 0*Sat2\n\n#Latent Means\nSat1 ~ 0*1\nSat2 ~ 0*1\nSatDy ~ 0*1"
  )

}
)
test_that(
  "scriptBifac produces correct number of parameter estimates for consrt_dy_meas = loadings_source, constr_dy_struct = none, and scaleset = FF", {

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

  #script source model w/ scaleset = FF
  script <- scriptBifac(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = "loadings_source",
    constr_dy_struct = "none"
  )

  #fit model
  mod <- lavaan::cfa(script, data = commitmentQ,
                     missing = "fiml" #for convergence
  )

  #manually calculated free parameters (see LINK)
  #my_param <- 35

  lav_param <- as.double(
    lavaan::fitmeasures(
      mod,
      "npar")
  )

  expect_equal(
    lav_param,
    35
  )

}
)
test_that(
  "scriptBifac produces correct df for constr_dy_meas = loadings_source, constr_dy_struct = none, and scaleset = FF", {

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

  #script source model w/ scaleset = FF
  script <- scriptBifac(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = "loadings_source",
    constr_dy_struct = "none"
  )

  #fit model
  mod <- lavaan::cfa(
    script,
    data = commitmentQ,
    missing = "fiml" #for convergence
  )

  #manually calculated free parameters (see LINK)
  #my_param <- 35

  #calculated df (should be 30)
  #my_df <- knowns-my_param

  lav_df <- as.double(
    lavaan::fitmeasures(
      mod,
      "df")
  )

  expect_equal(
    lav_df,
    30
  )

}
)

# ---- MV ----

# ---- FF vs. MV ----

# Loadings: Mutuality --------------------------------------------------------
# ---- FF ----

# ---- MV ----

# ---- FF vs. MV ----

# Latent Variances --------------------------------------------------------
# ---- FF ----
test_that(
  "scriptBifac produces correct output for constr_dy_meas = loadings, constr_dy_struct = variances and scaleset = FF", {

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

    scriptBifac(
      dvn,
      scaleset = "FF",
      lvname = "Sat",
      constr_dy_meas = "loadings",
      constr_dy_struct = "variances"
    ),

    "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\nSatDy=~NA*sat.g.1_1+lxg1*sat.g.1_1+lxg2*sat.g.1_2+lxg3*sat.g.1_3+lxg4*sat.g.1_4+lxg5*sat.g.1_5+lxg1*sat.g.2_1+lxg2*sat.g.2_2+lxg3*sat.g.2_3+lxg4*sat.g.2_4+lxg5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances (Orthogonal Structure)\nSat1 ~~ psix*Sat1\nSat2 ~~ psix*Sat2\nSatDy ~~ NA*SatDy + psix*SatDy\n\nSatDy ~~ 0*Sat1\nSatDy ~~ 0*Sat2\nSat1 ~~ 0*Sat2\n\n#Latent Means\nSat1 ~ 0*1\nSat2 ~ 0*1\nSatDy ~ 0*1"
  )

}
)
test_that(
  "scriptBifac produces correct number of parameter estimates for constr_dy_meas = loadings, constr_dy_struct = variances and scaleset = FF", {

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

  #script latent variance equality model w/ scaleset = FF
  script <- scriptBifac(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = "loadings",
    constr_dy_struct = "variances"
  )

  #fit model
  mod <- lavaan::cfa(script, data = commitmentQ,
                     missing = "fiml" #for convergence
  )

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
  "scriptBifac produces correct df for constr_dy_meas = loadings, constr_dy_struct = variances and scaleset = FF", {

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

  #script latent variance equality model w/ scaleset = FF
  script <- scriptBifac(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = "loadings",
    constr_dy_struct = "variances"
  )

  #fit model
  mod <- lavaan::cfa(
    script,
    data = commitmentQ,
    missing = "fiml" #for convergence
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
  "scriptBifac produces correct output for constr_dy_meas = loadings, constr_dy_struct = variances, and scaleset = MV", {

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

    scriptBifac(
      dvn,
      scaleset = "MV",
      lvname = "Sat",
      constr_dy_meas = "loadings",
      constr_dy_struct = "variances"
    ),

    "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\nSatDy=~NA*sat.g.1_1+lxg1*sat.g.1_1+lxg2*sat.g.1_2+lxg3*sat.g.1_3+lxg4*sat.g.1_4+lxg5*sat.g.1_5+lxg1*sat.g.2_1+lxg2*sat.g.2_2+lxg3*sat.g.2_3+lxg4*sat.g.2_4+lxg5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 0*1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances (Orthogonal Structure)\nSat1 ~~ psix*Sat1\nSat2 ~~ psix*Sat2\nSatDy ~~ psix*SatDy\n\nSatDy ~~ 0*Sat1\nSatDy ~~ 0*Sat2\nSat1 ~~ 0*Sat2\n\n#Latent Means\nSat1 ~ NA*1\nSat2 ~ NA*1\nSatDy ~ 0*1"
  )

}
)

# ---- FF vs. MV ----
test_that(#failure
  "scriptBifac produces same df for constr_dy_meas = loadings, constr_dy_struct = variances when scaleset= FF, and scaleset = MV", {

    dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                          x_delim2="_", distinguish_1="1", distinguish_2="2")


    #Script lvars equality model and scaleset = FF
    script_ff <- scriptBifac(dvn, lvname = "Sat", constr_dy_meas = "loadings", constr_dy_struct = "variances", scaleset = "FF")

    #Fit model w FF
    mod_ff <- suppressWarnings(lavaan::cfa(script_ff, data = commitmentQ))

    #get its df
    df_ff <- as.double(lavaan::fitmeasures(mod_ff, "df"))

    #Script lvars equality model and scaleset = MV
    script_mv <- scriptBifac(dvn, lvname = "Sat", constr_dy_meas = "loadings", constr_dy_struct = "variances", scaleset = "MV")

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
test_that(#failure
  "scriptBifac produces same chisq for constr_dy_meas = loadings, constr_dy_struct = variances when scaleset= FF, and scaleset = MV", {

    dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                          x_delim2="_", distinguish_1="1", distinguish_2="2")


    #Script lvars equality model and scaleset = FF
    script_ff <- scriptBifac(dvn, lvname = "Sat", constr_dy_meas = "loadings", constr_dy_struct = "variances", scaleset = "FF")

    #Fit model w FF
    mod_ff <- suppressWarnings(lavaan::cfa(script_ff, data = commitmentQ))

    #get its chisq
    chisq_ff <- as.double(lavaan::fitmeasures(mod_ff, "chisq"))

    #Script lvars equality model and scaleset = MV
    script_mv <- scriptBifac(dvn, lvname = "Sat", constr_dy_meas = "loadings", constr_dy_struct = "variances", scaleset = "MV")

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

# Latent Means --------------------------------------------------------
# ---- FF ----
test_that(
  "scriptBifac produces correct output for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = means and scaleset = FF", {

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

    scriptBifac(
      dvn,
      scaleset = "FF",
      lvname = "Sat",
      constr_dy_meas = c("loadings", "intercepts"),
      constr_dy_struct = "means"
    ),

    "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\nSatDy=~NA*sat.g.1_1+lxg1*sat.g.1_1+lxg2*sat.g.1_2+lxg3*sat.g.1_3+lxg4*sat.g.1_4+lxg5*sat.g.1_5+lxg1*sat.g.2_1+lxg2*sat.g.2_2+lxg3*sat.g.2_3+lxg4*sat.g.2_4+lxg5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances (Orthogonal Structure)\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nSatDy ~~ 1*SatDy\n\nSatDy ~~ 0*Sat1\nSatDy ~~ 0*Sat2\nSat1 ~~ 0*Sat2\n\n#Latent Means\nSat1 ~ alphax*1\nSat2 ~ alphax*1\nSatDy ~ 0*1 + alphax*1"
  )

}
)


test_that(#failure
  "scriptBifac produces correct number of parameter estimates for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = means and scaleset = FF", {

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

  #script latent means equality model w/ scaleset = FF
  script <- scriptBifac(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = c("loadings", "intercepts"),
    constr_dy_struct = "means"
  )

  #fit model
  mod <- lavaan::cfa(script, data = commitmentQ,
                     missing = "fiml" #for convergence
  )

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


test_that(#failure
  "scriptBifac produces correct df for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = means and scaleset = FF", {

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

  #script latent means equality model w/ scaleset = FF
  script <- scriptBifac(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = c("loadings", "intercepts"),
    constr_dy_struct = "means"
  )

  #fit model
  mod <- lavaan::cfa(
    script,
    data = commitmentQ,
    missing = "fiml" #for convergence
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
test_that("scriptBifac produces correct output for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = means, and scaleset = MV", {

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

    scriptBifac(
      dvn,
      scaleset = "MV",
      lvname = "Sat",
      constr_dy_meas = c("loadings", "intercepts"),
      constr_dy_struct = "means"
    ),

    "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\nSatDy=~1*sat.g.1_1+lxg1*sat.g.1_1+lxg2*sat.g.1_2+lxg3*sat.g.1_3+lxg4*sat.g.1_4+lxg5*sat.g.1_5+lxg1*sat.g.2_1+lxg2*sat.g.2_2+lxg3*sat.g.2_3+lxg4*sat.g.2_4+lxg5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1 + tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances (Orthogonal Structure)\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nSatDy ~~ NA*SatDy\n\nSatDy ~~ 0*Sat1\nSatDy ~~ 0*Sat2\nSat1 ~~ 0*Sat2\n\n#Latent Means\nSat1 ~ alphax*1\nSat2 ~ alphax*1\nSatDy ~ alphax*1"
  )

}
)

# ---- FF vs. MV ----
test_that(#failure
  "scriptBifac produces same df for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = means when scaleset= FF, and scaleset = MV", {

    dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                          x_delim2="_", distinguish_1="1", distinguish_2="2")


    #Script lmeans equality model and scaleset = FF
    script_ff <- scriptBifac(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "means", scaleset = "FF")

    #Fit model w FF
    mod_ff <- suppressWarnings(lavaan::cfa(script_ff, data = commitmentQ))

    #get its df
    df_ff <- as.double(lavaan::fitmeasures(mod_ff, "df"))

    #Script lmeans equality model and scaleset = MV
    script_mv <- scriptBifac(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "means", scaleset = "MV")

    #Fit model w MV
    mod_mv <- suppressWarnings(lavaan::cfa(script_mv, data = commitmentQ))

    #get its df
    df_mv <- as.double(lavaan::fitmeasures(mod_mv, "df"))

    #scale setting should have no impact on df
    expect_equal(df_ff,
                 df_mv
    )
  }
)
test_that(#pass (but fails in `equal_identical()`)
  "scriptBifac produces same chisq for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = means when scaleset= FF, and scaleset = MV", {

    dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                          x_delim2="_", distinguish_1="1", distinguish_2="2")


    #Script lmeans equality model and scaleset = FF
    script_ff <- scriptBifac(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "means", scaleset = "FF")

    #Fit model w FF
    mod_ff <- suppressWarnings(lavaan::cfa(script_ff, data = commitmentQ))

    #get its chisq
    chisq_ff <- as.double(lavaan::fitmeasures(mod_ff, "chisq"))

    #Script lmeans equality model and scaleset = MV
    script_mv <- scriptBifac(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "means", scaleset = "MV")

    #Fit model w MV
    mod_mv <- suppressWarnings(lavaan::cfa(script_mv, data = commitmentQ))

    #get its chisq
    chisq_mv <- as.double(lavaan::fitmeasures(mod_mv, "chisq"))

    #scale setting should have no impact on chisq
    expect_equal(chisq_ff,
                 chisq_mv
    )
  }
)
