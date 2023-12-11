#### configural ####
test_that("scriptCFA produces correct output for constr_dy_meas = none, constr_dy_struct = none, and scaleset= FF", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                         x_delim2="_", distinguish_1="1", distinguish_2="2")


  expect_equal(scriptCFA(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "FF"),
               "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5\nSat2=~NA*sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ 1*Sat1\nSat2 ~~ 1*Sat2\nSat1 ~~ Sat2\n\n#Latent Means\nSat1 ~ 0*1\nSat2 ~ 0*1"
  )
})

test_that("scriptCFA produces correct output for constr_dy_meas = none, constr_dy_struct = none and scaleset= MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  expect_equal(scriptCFA(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "MV"),
               "#Measurement Model\n\n#Loadings\nSat1=~1*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5\nSat2=~1*sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 0*1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\n#Latent Means\nSat1 ~ NA*1\nSat2 ~ NA*1"
               )
})

test_that("scriptCFA produces correct number of parameter estimates for constr_dy_meas = none, constr_dy_struct = none and scaleset= FF", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")

  #knowns for model w/ intercepts/latent means:
  #var_num <- 10
  #knowns <- var_num*(var_num+1)/2+var_num

  #Script model w/ model = configural and scaleset = FF
  script <- scriptCFA(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "FF")

  #Fit model
  mod <- lavaan::cfa(script, data = commitmentQ)

  #manually calculated free parameters (see LINK)
  #my_param <- 36

  lav_param <- as.double(lavaan::fitmeasures(mod, "npar"))

  expect_equal(lav_param,
               36
  )
})

test_that("scriptCFA produces correct df for constr_dy_meas = none, constr_dy_struct = none and scaleset= FF", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")

  #knowns for model w/ intercepts/latent means:
  #var_num <- 10
  #knowns <- var_num*(var_num+1)/2+var_num

  #Script model w/ model = configural and scaleset = FF
  script <- scriptCFA(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "FF")

  #Fit model
  mod <- lavaan::cfa(script, data = commitmentQ)

  #manually calculated free parameters (see LINK)
  #my_param <- 36

  #calculated df (should be 29)
  #my_df <- knowns-my_param

  lav_df <- as.double(lavaan::fitmeasures(mod, "df"))

  expect_equal(lav_df,
               29
  )
})

test_that("scriptCFA produces same df for constr_dy_meas = none, constr_dy_struct = none when scaleset= FF, and scaleset = MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  #Script model w/ model = configural and scaleset = FF
  script_ff <- scriptCFA(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "FF")

  #Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  #get its df
  df_ff <- as.double(lavaan::fitmeasures(mod_ff, "df"))

  #Script model w/ model = configural and scaleset = FF
  script_mv <- scriptCFA(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "MV")

  #Fit model w FF
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  #get its df
  df_mv <- as.double(lavaan::fitmeasures(mod_mv, "df"))

  #scale setting should have no impact on df
  expect_equal(df_ff,
               df_mv
  )
})

test_that("scriptCFA produces same chisq for constr_dy_meas = none, constr_dy_struct = none when scaleset= FF, and scaleset = MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  #Script model w/ model = configural and scaleset = FF
  script_ff <- scriptCFA(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "FF")

  #Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  #get its df
  chisq_ff <- as.double(lavaan::fitmeasures(mod_ff, "chisq"))

  #Script model w/ model = configural and scaleset = FF
  script_mv <- scriptCFA(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "MV")

  #Fit model w FF
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  #get its df
  chisq_mv <- as.double(lavaan::fitmeasures(mod_mv, "chisq"))

  #scale setting should have no impact on df
  expect_equal(chisq_ff,
               chisq_mv
  )
})

#### loading ####
test_that("scriptCFA produces correct output for constr_dy_meas = loadings, constr_dy_struct = none and scaleset= FF", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  expect_equal(scriptCFA(dvn, lvname = "Sat", constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "FF"),
               "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ 1*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\n#Latent Means\nSat1 ~ 0*1\nSat2 ~ 0*1")
})

test_that("scriptCFA produces correct output for constr_dy_meas = loadings, constr_dy_struct = none and scaleset= MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  expect_equal(scriptCFA(dvn, lvname = "Sat", constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "MV"),
               "#Measurement Model\n\n#Loadings\nSat1=~1*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 0*1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\n#Latent Means\nSat1 ~ NA*1\nSat2 ~ NA*1"
               )
})

test_that("scriptCFA produces correct number of parameter estimates for constr_dy_meas = loadings, constr_dy_struct = none and scaleset= FF", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")

  #knowns for model w/ intercepts/latent means:
  #var_num <- 10
  #knowns <- var_num*(var_num+1)/2+var_num

  #Script model w/ model = configural and scaleset = FF
  script <- scriptCFA(dvn, lvname = "Sat", constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "FF")

  #Fit model
  mod <- lavaan::cfa(script, data = commitmentQ)

  #manually calculated free parameters (see LINK)
  #my_param <- 32

  lav_param <- as.double(lavaan::fitmeasures(mod, "npar"))

  expect_equal(lav_param,
               32
  )
})

test_that("scriptCFA produces correct df for constr_dy_meas = loadings, constr_dy_struct = none and scaleset= FF", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")

  #knowns for model w/ intercepts/latent means:
  #var_num <- 10
  #knowns <- var_num*(var_num+1)/2+var_num

  #Script model w/ model = configural and scaleset = FF
  script <- scriptCFA(dvn, lvname = "Sat", constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "FF")

  #Fit model
  mod <- lavaan::cfa(script, data = commitmentQ)

  #manually calculated free parameters (see LINK)
  #my_param <- 32

  #calculated df (should be 33)
  #my_df <- knowns-my_param

  lav_df <- as.double(lavaan::fitmeasures(mod, "df"))

  expect_equal(lav_df,
               33
  )
})

test_that("scriptCFA produces same df for constr_dy_meas = loadings, constr_dy_struct = none when scaleset= FF, and scaleset = MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  #Script model w/ model = configural and scaleset = FF
  script_ff <- scriptCFA(dvn, lvname = "Sat", constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "FF")

  #Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  #get its df
  df_ff <- as.double(lavaan::fitmeasures(mod_ff, "df"))

  #Script model w/ model = configural and scaleset = FF
  script_mv <- scriptCFA(dvn, lvname = "Sat", constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "MV")

  #Fit model w FF
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  #get its df
  df_mv <- as.double(lavaan::fitmeasures(mod_mv, "df"))

  #scale setting should have no impact on df
  expect_equal(df_ff,
               df_mv
  )
})

test_that("scriptCFA produces same chisq for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none when scaleset= FF, and scaleset = MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  #Script model w/ model = configural and scaleset = FF
  script_ff <- scriptCFA(dvn, lvname = "Sat", constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "FF")

  #Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  #get its df
  chisq_ff <- as.double(lavaan::fitmeasures(mod_ff, "chisq"))

  #Script model w/ model = configural and scaleset = FF
  script_mv <- scriptCFA(dvn, lvname = "Sat", constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "MV")

  #Fit model w FF
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  #get its df
  chisq_mv <- as.double(lavaan::fitmeasures(mod_mv, "chisq"))

  #scale setting should have no impact on df
  expect_equal(chisq_ff,
               chisq_mv
  )
})
#### intercept ####
test_that("scriptCFA produces correct output for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none and scaleset= FF", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  expect_equal(scriptCFA(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "FF"),
               "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ 1*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\n#Latent Means\nSat1 ~ 0*1\nSat2 ~ NA*1"
               )
})

test_that("scriptCFA produces correct output for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none and scaleset= MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  expect_equal(scriptCFA(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "MV"),
               "#Measurement Model\n\n#Loadings\nSat1=~1*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1 + tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\n#Latent Means\nSat1 ~ NA*1\nSat2 ~ NA*1"
               )
})

test_that("scriptCFA produces correct number of parameter estimates for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none and scaleset= FF", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")

  #knowns for model w/ intercepts/latent means:
  #var_num <- 10
  #knowns <- var_num*(var_num+1)/2+var_num

  #Script model w/ model = configural and scaleset = FF
  script <- scriptCFA(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "FF")

  #Fit model
  mod <- lavaan::cfa(script, data = commitmentQ)

  #manually calculated free parameters (see LINK)
  #my_param <- 28

  lav_param <- as.double(lavaan::fitmeasures(mod, "npar"))

  expect_equal(lav_param,
               28
  )
})

test_that("scriptCFA produces correct df for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none and scaleset= FF", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")

  #knowns for model w/ intercepts/latent means:
  #var_num <- 10
  #knowns <- var_num*(var_num+1)/2+var_num

  #Script model w/ model = configural and scaleset = FF
  script <- scriptCFA(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "FF")

  #Fit model
  mod <- lavaan::cfa(script, data = commitmentQ)

  #manually calculated free parameters (see LINK)
  #my_param <- 28

  #calculated df (should be 37)
  #my_df <- knowns-my_param

  lav_df <- as.double(lavaan::fitmeasures(mod, "df"))

  expect_equal(lav_df,
               37
  )
})

test_that("scriptCFA produces same df for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none when scaleset= FF, and scaleset = MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  #Script model w/ model = configural and scaleset = FF
  script_ff <- scriptCFA(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "FF")

  #Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  #get its df
  df_ff <- as.double(lavaan::fitmeasures(mod_ff, "df"))

  #Script model w/ model = configural and scaleset = FF
  script_mv <- scriptCFA(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "MV")

  #Fit model w FF
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  #get its df
  df_mv <- as.double(lavaan::fitmeasures(mod_mv, "df"))

  #scale setting should have no impact on df
  expect_equal(df_ff,
               df_mv
  )
})

test_that("scriptCFA produces same chisq for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none when scaleset= FF, and scaleset = MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  #Script model w/ model = configural and scaleset = FF
  script_ff <- scriptCFA(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "FF")

  #Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  #get its df
  chisq_ff <- as.double(lavaan::fitmeasures(mod_ff, "chisq"))

  #Script model w/ model = configural and scaleset = FF
  script_mv <- scriptCFA(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "MV")

  #Fit model w FF
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  #get its df
  chisq_mv <- as.double(lavaan::fitmeasures(mod_mv, "chisq"))

  #scale setting should have no impact on df
  expect_equal(chisq_ff,
               chisq_mv
  )
})
#### residual ####
test_that("scriptCFA produces correct output for constr_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = none and scaleset= FF", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  expect_equal(scriptCFA(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none", scaleset = "FF"),
               "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ thx1*sat.g.1_1\nsat.g.1_2 ~~ thx2*sat.g.1_2\nsat.g.1_3 ~~ thx3*sat.g.1_3\nsat.g.1_4 ~~ thx4*sat.g.1_4\nsat.g.1_5 ~~ thx5*sat.g.1_5\n\nsat.g.2_1 ~~ thx1*sat.g.2_1\nsat.g.2_2 ~~ thx2*sat.g.2_2\nsat.g.2_3 ~~ thx3*sat.g.2_3\nsat.g.2_4 ~~ thx4*sat.g.2_4\nsat.g.2_5 ~~ thx5*sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ 1*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\n#Latent Means\nSat1 ~ 0*1\nSat2 ~ NA*1"
               )
})

test_that("scriptCFA produces correct output for constr_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = none and scaleset= MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  expect_equal(scriptCFA(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none", scaleset = "MV"),
               "#Measurement Model\n\n#Loadings\nSat1=~1*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1 + tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ thx1*sat.g.1_1\nsat.g.1_2 ~~ thx2*sat.g.1_2\nsat.g.1_3 ~~ thx3*sat.g.1_3\nsat.g.1_4 ~~ thx4*sat.g.1_4\nsat.g.1_5 ~~ thx5*sat.g.1_5\n\nsat.g.2_1 ~~ thx1*sat.g.2_1\nsat.g.2_2 ~~ thx2*sat.g.2_2\nsat.g.2_3 ~~ thx3*sat.g.2_3\nsat.g.2_4 ~~ thx4*sat.g.2_4\nsat.g.2_5 ~~ thx5*sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\n#Latent Means\nSat1 ~ NA*1\nSat2 ~ NA*1"
               )
})


#### lvariance ####
test_that("scriptCFA produces correct output for constr_dy_meas = c(loadings), constr_dy_struct = variances and scaleset= FF", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  expect_equal(scriptCFA(dvn, lvname = "Sat",
                         constr_dy_meas = c("loadings"),
                         constr_dy_struct = c("variances"), scaleset = "FF"),
               "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ 1*Sat1 + psix*Sat1\nSat2 ~~ psix*Sat2\nSat1 ~~ Sat2\n\n#Latent Means\nSat1 ~ 0*1\nSat2 ~ 0*1"
               )
})

test_that("scriptCFA produces correct output for constr_dy_meas = c(loadings), constr_dy_struct = variances and scaleset= MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  expect_equal(scriptCFA(dvn, lvname = "Sat",
                         constr_dy_meas = c("loadings"),
                         constr_dy_struct = c("variances"), scaleset = "MV"),
               "#Measurement Model\n\n#Loadings\nSat1=~1*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 0*1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ psix*Sat1\nSat2 ~~ psix*Sat2\nSat1 ~~ Sat2\n\n#Latent Means\nSat1 ~ NA*1\nSat2 ~ NA*1"
               )
})

#### lmean ####
test_that("scriptCFA produces correct output for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = means and scaleset= FF", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  expect_equal(scriptCFA(dvn, lvname = "Sat",
                         constr_dy_meas = c("loadings", "intercepts"),
                         constr_dy_struct = c("means"), scaleset = "FF"),
               "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ 1*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\n#Latent Means\nSat1 ~ 0*1 + alphax*1\nSat2 ~ alphax*1"
               )
})

test_that("scriptCFA produces correct output for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = means and scaleset= MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  expect_equal(scriptCFA(dvn, lvname = "Sat",
                         constr_dy_meas = c("loadings", "intercepts"),
                         constr_dy_struct = c("means"), scaleset = "MV"),
               "#Measurement Model\n\n#Loadings\nSat1=~1*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1 + tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\n#Latent Means\nSat1 ~ alphax*1\nSat2 ~ alphax*1"
               )
})

#### indist ####
test_that("scriptCFA produces correct output for constr_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = c(variances, means) and scaleset= FF", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  expect_equal(scriptCFA(dvn, lvname = "Sat",
                         constr_dy_meas = c("loadings", "intercepts", "residuals"),
                         constr_dy_struct = c("variances","means"), scaleset = "FF"),
               "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ thx1*sat.g.1_1\nsat.g.1_2 ~~ thx2*sat.g.1_2\nsat.g.1_3 ~~ thx3*sat.g.1_3\nsat.g.1_4 ~~ thx4*sat.g.1_4\nsat.g.1_5 ~~ thx5*sat.g.1_5\n\nsat.g.2_1 ~~ thx1*sat.g.2_1\nsat.g.2_2 ~~ thx2*sat.g.2_2\nsat.g.2_3 ~~ thx3*sat.g.2_3\nsat.g.2_4 ~~ thx4*sat.g.2_4\nsat.g.2_5 ~~ thx5*sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ 1*Sat1 + psix*Sat1\nSat2 ~~ psix*Sat2\nSat1 ~~ Sat2\n\n#Latent Means\nSat1 ~ 0*1 + alphax*1\nSat2 ~ alphax*1"
               )
})

test_that("scriptCFA produces correct output for constr_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = c(variances, means) and scaleset= MV", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")


  expect_equal(scriptCFA(dvn, lvname = "Sat",
                         constr_dy_meas = c("loadings", "intercepts", "residuals"),
                         constr_dy_struct = c("variances", "means"), scaleset = "MV"),
               "#Measurement Model\n\n#Loadings\nSat1=~1*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1 + tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ thx1*sat.g.1_1\nsat.g.1_2 ~~ thx2*sat.g.1_2\nsat.g.1_3 ~~ thx3*sat.g.1_3\nsat.g.1_4 ~~ thx4*sat.g.1_4\nsat.g.1_5 ~~ thx5*sat.g.1_5\n\nsat.g.2_1 ~~ thx1*sat.g.2_1\nsat.g.2_2 ~~ thx2*sat.g.2_2\nsat.g.2_3 ~~ thx3*sat.g.2_3\nsat.g.2_4 ~~ thx4*sat.g.2_4\nsat.g.2_5 ~~ thx5*sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ psix*Sat1\nSat2 ~~ psix*Sat2\nSat1 ~~ Sat2\n\n#Latent Means\nSat1 ~ alphax*1\nSat2 ~ alphax*1"
               )
})



#TODO: tests for writescript = TRUE
