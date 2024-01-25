#### loads ####
##### partner = 1 #####

test_that("loads produces correct output for lvar = X, partner = 1, type = free", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(loads(dvn, lvar = "X", lvname = "Sat", partner="1", type = "free"),
               "Sat1=~NA*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5"
  )
})

test_that("loads produces correct output for lvar = Y, partner = 1, type = free", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(loads(dvn, lvar = "Y", lvname = "Com", partner="1", type = "free"),
               "Com1=~NA*com.1_1+com.1_2+com.1_3+com.1_4+com.1_5"
  )
})

test_that("loads produces correct output for lvar = X, partner = 1, type = fixed", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(loads(dvn, lvar = "X", lvname = "Sat", partner="1", type = "fixed"),
               "Sat1=~1*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5"
  )
})

test_that("loads produces correct output for lvar = Y, partner = 1, type = fixed", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(loads(dvn, lvar = "Y", lvname = "Com", partner="1", type = "fixed"),
               "Com1=~1*com.1_1+com.1_2+com.1_3+com.1_4+com.1_5"
  )
})

test_that("loads produces correct output for lvar = X, partner = 1, type = equated", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(loads(dvn, lvar = "X", lvname = "Sat", partner="1", type = "equated"),
               "Sat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5"
  )
})

test_that("loads produces correct output for lvar = Y, partner = 1, type = equated", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(loads(dvn, lvar = "Y", lvname = "Com", partner="1", type = "equated"),
               "Com1=~NA*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5"
  )
})

test_that("loads produces correct output for lvar = X, partner = 1, type = equated_mv", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(loads(dvn, lvar = "X", lvname = "Sat", partner="1", type = "equated_mv"),
               "Sat1=~1*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5"
  )
})

test_that("loads produces correct output for lvar = Y, partner = 1, type = equated_mv", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(loads(dvn, lvar = "Y", lvname = "Com", partner="1", type = "equated_mv"),
               "Com1=~1*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5"
  )
})

test_that("loads produces correct output for lvar = X, partner = 1, type = equated_source", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(loads(dvn, lvar = "X", lvname = "Sat", partner="1", type = "equated_source"),
               "Sat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5"
  )
})

test_that("loads produces correct output for lvar = Y, partner = 1, type = equated_source", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(loads(dvn, lvar = "Y", lvname = "Com", partner="1", type = "equated_source"),
               "Com1=~NA*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5"
  )
})

##### partner = 2 #####
test_that("loads produces correct output for lvar = X, partner = 2, type = free", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(loads(dvn, lvar = "X", lvname = "Sat", partner="2", type = "free"),
               "Sat2=~NA*sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5"
  )
})

test_that("loads produces correct output for lvar = Y, partner = 2, type = free", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(loads(dvn, lvar = "Y", lvname = "Com", partner="2", type = "free"),
               "Com2=~NA*com.2_1+com.2_2+com.2_3+com.2_4+com.2_5"
  )
})

test_that("loads produces correct output for lvar = X, partner = 2, type = fixed", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(loads(dvn, lvar = "X", lvname = "Sat", partner="2", type = "fixed"),
               "Sat2=~1*sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5"
  )
})

test_that("loads produces correct output for lvar = Y, partner = 2, type = fixed", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(loads(dvn, lvar = "Y", lvname = "Com", partner="2", type = "fixed"),
               "Com2=~1*com.2_1+com.2_2+com.2_3+com.2_4+com.2_5"
  )
})

test_that("loads produces correct output for lvar = X, partner = 2, type = equated", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(loads(dvn, lvar = "X", lvname = "Sat", partner="2", type = "equated"),
               "Sat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5"
  )
})

test_that("loads produces correct output for lvar = Y, partner = 2, type = equated", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(loads(dvn, lvar = "Y", lvname = "Com", partner="2", type = "equated"),
               "Com2=~NA*com.2_1+ly1*com.2_1+ly2*com.2_2+ly3*com.2_3+ly4*com.2_4+ly5*com.2_5"
  )
})

test_that("loads produces correct output for lvar = X, partner = 2, type = equated_mv", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(loads(dvn, lvar = "X", lvname = "Sat", partner="2", type = "equated_mv"),
               "Sat2=~1*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5"
  )
})

test_that("loads produces correct output for lvar = Y, partner = 2, type = equated_mv", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(loads(dvn, lvar = "Y", lvname = "Com", partner="2", type = "equated_mv"),
               "Com2=~1*com.2_1+ly1*com.2_1+ly2*com.2_2+ly3*com.2_3+ly4*com.2_4+ly5*com.2_5"
  )
})

test_that("loads produces correct output for lvar = X, partner = 2, type = equated_source", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(loads(dvn, lvar = "X", lvname = "Sat", partner="2", type = "equated_source"),
               "Sat2=~NA*sat.g.2_1+lx6*sat.g.2_1+lx7*sat.g.2_2+lx8*sat.g.2_3+lx9*sat.g.2_4+lx10*sat.g.2_5"
  )
})

test_that("loads produces correct output for lvar = Y, partner = 2, type = equated_source", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(loads(dvn, lvar = "Y", lvname = "Com", partner="2", type = "equated_source"),
               "Com2=~NA*com.2_1+ly6*com.2_1+ly7*com.2_2+ly8*com.2_3+ly9*com.2_4+ly10*com.2_5"
  )
})

##### partner = g #####

test_that("loads produces correct output for lvar = X, partner = g, type = free", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(loads(dvn, lvar = "X", lvname = "Sat", partner="g", type = "free"),
               "SatDy=~NA*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5+sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5"
  )
})

test_that("loads produces correct output for lvar = Y, partner = g, type = free", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(loads(dvn, lvar = "Y", lvname = "Com", partner="g", type = "free"),
               "ComDy=~NA*com.1_1+com.1_2+com.1_3+com.1_4+com.1_5+com.2_1+com.2_2+com.2_3+com.2_4+com.2_5"
  )
})


test_that("loads produces correct output for lvar = X, partner = g, type = equated_source", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(loads(dvn, lvar = "X", lvname = "Sat", partner="g", type = "equated_source"),
               "SatDy=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5+lx6*sat.g.2_1+lx7*sat.g.2_2+lx8*sat.g.2_3+lx9*sat.g.2_4+lx10*sat.g.2_5"
  )
})

test_that("loads produces correct output for lvar = Y, partner = g, type = equated_source", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(loads(dvn, lvar = "Y", lvname = "Com", partner="g", type = "equated_source"),
               "ComDy=~NA*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5+ly6*com.2_1+ly7*com.2_2+ly8*com.2_3+ly9*com.2_4+ly10*com.2_5"
  )
})

#### intercepts ####
##### partner = 1 #####

test_that("intercepts produces correct output for lvar = X, partner = 1, type = free", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(intercepts(dvn, lvar = "X", partner="1", type = "free"),
               "sat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1"
  )
})

test_that("intercepts produces correct output for lvar = Y, partner = 1, type = free", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(intercepts(dvn, lvar = "Y", partner="1", type = "free"),
               "com.1_1 ~ 1\ncom.1_2 ~ 1\ncom.1_3 ~ 1\ncom.1_4 ~ 1\ncom.1_5 ~ 1"
  )
})

test_that("intercepts produces correct output for lvar = X, partner = 1, type = fixed", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(intercepts(dvn, lvar = "X", partner="1", type = "fixed"),
               "sat.g.1_1 ~ 0*1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1"
  )
})

test_that("intercepts produces correct output for lvar = Y, partner = 1, type = fixed", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(intercepts(dvn, lvar = "Y", partner="1", type = "fixed"),
               "com.1_1 ~ 0*1\ncom.1_2 ~ 1\ncom.1_3 ~ 1\ncom.1_4 ~ 1\ncom.1_5 ~ 1"
  )
})

test_that("intercepts produces correct output for lvar = X, partner = 1, type = equated", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(intercepts(dvn, lvar = "X", partner="1", type = "equated"),
               "sat.g.1_1 ~ tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1"
  )
})

test_that("intercepts produces correct output for lvar = Y, partner = 1, type = equated", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(intercepts(dvn, lvar = "Y", partner="1", type = "equated"),
               "com.1_1 ~ ty1*1\ncom.1_2 ~ ty2*1\ncom.1_3 ~ ty3*1\ncom.1_4 ~ ty4*1\ncom.1_5 ~ ty5*1"
  )
})

test_that("intercepts produces correct output for lvar = X, partner = 1, type = equated_mv", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(intercepts(dvn, lvar = "X", partner="1", type = "equated_mv"),
               "sat.g.1_1 ~ 0*1 + tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1"
  )
})

test_that("intercepts produces correct output for lvar = Y, partner = 1, type = equated_mv", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(intercepts(dvn, lvar = "Y", partner="1", type = "equated_mv"),
               "com.1_1 ~ 0*1 + ty1*1\ncom.1_2 ~ ty2*1\ncom.1_3 ~ ty3*1\ncom.1_4 ~ ty4*1\ncom.1_5 ~ ty5*1"
  )
})

##### partner = 2 #####
test_that("intercepts produces correct output for lvar = X, partner = 2, type = free", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(intercepts(dvn, lvar = "X", partner="2", type = "free"),
               "sat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1"
  )
})

test_that("intercepts produces correct output for lvar = Y, partner = 2, type = free", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(intercepts(dvn, lvar = "Y", partner="2", type = "free"),
               "com.2_1 ~ 1\ncom.2_2 ~ 1\ncom.2_3 ~ 1\ncom.2_4 ~ 1\ncom.2_5 ~ 1"
  )
})

test_that("intercepts produces correct output for lvar = X, partner = 2, type = fixed", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(intercepts(dvn, lvar = "X", partner="2", type = "fixed"),
               "sat.g.2_1 ~ 0*1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1"
  )
})

test_that("intercepts produces correct output for lvar = Y, partner = 2, type = fixed", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(intercepts(dvn, lvar = "Y", partner="2", type = "fixed"),
               "com.2_1 ~ 0*1\ncom.2_2 ~ 1\ncom.2_3 ~ 1\ncom.2_4 ~ 1\ncom.2_5 ~ 1"
  )
})

test_that("intercepts produces correct output for lvar = X, partner = 2, type = equated", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(intercepts(dvn, lvar = "X", partner="2", type = "equated"),
               "sat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1"
  )
})

test_that("intercepts produces correct output for lvar = Y, partner = 1, type = equated", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(intercepts(dvn, lvar = "Y", partner="2", type = "equated"),
               "com.2_1 ~ ty1*1\ncom.2_2 ~ ty2*1\ncom.2_3 ~ ty3*1\ncom.2_4 ~ ty4*1\ncom.2_5 ~ ty5*1"
  )
})

test_that("intercepts produces correct output for lvar = X, partner = 1, type = equated_mv", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(intercepts(dvn, lvar = "X", partner="2", type = "equated_mv"),
               "sat.g.2_1 ~ 0*1 + tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1"
  )
})

test_that("intercepts produces correct output for lvar = Y, partner = 2, type = equated_mv", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(intercepts(dvn, lvar = "Y", partner="2", type = "equated_mv"),
               "com.2_1 ~ 0*1 + ty1*1\ncom.2_2 ~ ty2*1\ncom.2_3 ~ ty3*1\ncom.2_4 ~ ty4*1\ncom.2_5 ~ ty5*1"
  )
})

#### resids ####
##### partner = 1 #####
test_that("resids produces correct output for lvar = X, partner = 1, type = free", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(resids(dvn, lvar = "X", partner="1", type = "free"),
               "sat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5"
  )
})

test_that("resids produces correct output for lvar = Y, partner = 1, type = free", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(resids(dvn, lvar = "Y", partner="1", type = "free"),
               "com.1_1 ~~ com.1_1\ncom.1_2 ~~ com.1_2\ncom.1_3 ~~ com.1_3\ncom.1_4 ~~ com.1_4\ncom.1_5 ~~ com.1_5"
  )
})

test_that("resids produces correct output for lvar = X, partner = 1, type = equated", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(resids(dvn, lvar = "X", partner="1", type = "equated"),
               "sat.g.1_1 ~~ thx1*sat.g.1_1\nsat.g.1_2 ~~ thx2*sat.g.1_2\nsat.g.1_3 ~~ thx3*sat.g.1_3\nsat.g.1_4 ~~ thx4*sat.g.1_4\nsat.g.1_5 ~~ thx5*sat.g.1_5"
  )
})

test_that("resids produces correct output for lvar = Y, partner = 1, type = equated", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(resids(dvn, lvar = "Y", partner="1", type = "equated"),
               "com.1_1 ~~ thy1*com.1_1\ncom.1_2 ~~ thy2*com.1_2\ncom.1_3 ~~ thy3*com.1_3\ncom.1_4 ~~ thy4*com.1_4\ncom.1_5 ~~ thy5*com.1_5"
  )
})

##### partner = 2 #####
test_that("resids produces correct output for lvar = X, partner = 2, type = free", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(resids(dvn, lvar = "X", partner="2", type = "free"),
               "sat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5"
  )
})

test_that("resids produces correct output for lvar = Y, partner = 2, type = free", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(resids(dvn, lvar = "Y", partner="2", type = "free"),
               "com.2_1 ~~ com.2_1\ncom.2_2 ~~ com.2_2\ncom.2_3 ~~ com.2_3\ncom.2_4 ~~ com.2_4\ncom.2_5 ~~ com.2_5"
  )
})

test_that("resids produces correct output for lvar = X, partner = 2, type = equated", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(resids(dvn, lvar = "X", partner="2", type = "equated"),
               "sat.g.2_1 ~~ thx1*sat.g.2_1\nsat.g.2_2 ~~ thx2*sat.g.2_2\nsat.g.2_3 ~~ thx3*sat.g.2_3\nsat.g.2_4 ~~ thx4*sat.g.2_4\nsat.g.2_5 ~~ thx5*sat.g.2_5"
  )
})

test_that("resids produces correct output for lvar = Y, partner = 2, type = equated", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(resids(dvn, lvar = "Y", partner="2", type = "equated"),
               "com.2_1 ~~ thy1*com.2_1\ncom.2_2 ~~ thy2*com.2_2\ncom.2_3 ~~ thy3*com.2_3\ncom.2_4 ~~ thy4*com.2_4\ncom.2_5 ~~ thy5*com.2_5"
  )
})

#### lvars ####
##### partner = 1 #####
test_that("lvars produces correct output for lvar = X, partner = 1, type = free", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lvars(dvn, lvar = "X", lvname = "Sat", partner="1", type = "free"),
               "Sat1 ~~ NA*Sat1"
  )
})

test_that("lvars produces correct output for lvar = Y, partner = 1, type = free", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lvars(dvn, lvar = "Y", lvname = "Com", partner="1", type = "free"),
               "Com1 ~~ NA*Com1"
  )
})

test_that("lvars produces correct output for lvar = X, partner = 1, type = fixed", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lvars(dvn, lvar = "X", lvname = "Sat", partner="1", type = "fixed"),
               "Sat1 ~~ 1*Sat1"
  )
})

test_that("lvars produces correct output for lvar = Y, partner = 1, type = fixed", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lvars(dvn, lvar = "Y", lvname = "Com", partner="1", type = "fixed"),
               "Com1 ~~ 1*Com1"
  )
})

test_that("lvars produces correct output for lvar = X, partner = 1, type = equated", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lvars(dvn, lvar = "X", lvname = "Sat", partner="1", type = "equated"),
               "Sat1 ~~ psix*Sat1"
  )
})

test_that("lvars produces correct output for lvar = Y, partner = 1, type = equated", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lvars(dvn, lvar = "Y", lvname = "Com", partner="1", type = "equated"),
               "Com1 ~~ psiy*Com1"
  )
})

##### partner = 2 #####
test_that("lvars produces correct output for lvar = X, partner = 2, type = free", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lvars(dvn, lvar = "X", lvname = "Sat", partner="2", type = "free"),
               "Sat2 ~~ NA*Sat2"
  )
})

test_that("lvars produces correct output for lvar = Y, partner = 2, type = free", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lvars(dvn, lvar = "Y", lvname = "Com", partner="2", type = "free"),
               "Com2 ~~ NA*Com2"
  )
})

test_that("lvars produces correct output for lvar = X, partner = 2, type = fixed", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lvars(dvn, lvar = "X", lvname = "Sat", partner="2", type = "fixed"),
               "Sat2 ~~ 1*Sat2"
  )
})

test_that("lvars produces correct output for lvar = Y, partner = 2, type = fixed", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lvars(dvn, lvar = "Y", lvname = "Com", partner="2", type = "fixed"),
               "Com2 ~~ 1*Com2"
  )
})

test_that("lvars produces correct output for lvar = X, partner = 2, type = equated", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lvars(dvn, lvar = "X", lvname = "Sat", partner="2", type = "equated"),
               "Sat2 ~~ psix*Sat2"
  )
})

test_that("lvars produces correct output for lvar = Y, partner = 2, type = equated", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lvars(dvn, lvar = "Y", lvname = "Com", partner="2", type = "equated"),
               "Com2 ~~ psiy*Com2"
  )
})

##### partner = g #####
test_that("lvars produces correct output for lvar = X, partner = g, type = free", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lvars(dvn, lvar = "X", lvname = "Sat", partner="g", type = "free"),
               "SatDy ~~ NA*SatDy"
  )
})

test_that("lvars produces correct output for lvar = X, partner = g, type = fixed", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lvars(dvn, lvar = "X", lvname = "Sat", partner="g", type = "fixed"),
               "SatDy ~~ 1*SatDy"
  )
})

#### lmeans ####
##### partner = 1 #####
test_that("lmeans produces correct output for lvar = X, partner = 1, type = free", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lmeans(dvn, lvar = "X", lvname = "Sat", partner="1", type = "free"),
               "Sat1 ~ NA*1"
  )
})

test_that("lmeans produces correct output for lvar = Y, partner = 1, type = free", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lmeans(dvn, lvar = "Y", lvname = "Com", partner="1", type = "free"),
               "Com1 ~ NA*1"
  )
})

test_that("lmeans produces correct output for lvar = X, partner = 1, type = fixed", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lmeans(dvn, lvar = "X", lvname = "Sat", partner="1", type = "fixed"),
               "Sat1 ~ 0*1"
  )
})

test_that("lmeans produces correct output for lvar = Y, partner = 1, type = fixed", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lmeans(dvn, lvar = "Y", lvname = "Com", partner="1", type = "fixed"),
               "Com1 ~ 0*1"
  )
})

test_that("lmeans produces correct output for lvar = X, partner = 1, type = equated", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lmeans(dvn, lvar = "X", lvname = "Sat", partner="1", type = "equated"),
               "Sat1 ~ alphax*1"
  )
})

test_that("lmeans produces correct output for lvar = Y, partner = 1, type = equated", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lmeans(dvn, lvar = "Y", lvname = "Com", partner="1", type = "equated"),
               "Com1 ~ alphay*1"
  )
})

##### partner = 2 #####
test_that("lmeans produces correct output for lvar = X, partner = 2, type = free", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lmeans(dvn, lvar = "X", lvname = "Sat", partner="2", type = "free"),
               "Sat2 ~ NA*1"
  )
})

test_that("lmeans produces correct output for lvar = Y, partner = 2, type = free", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lmeans(dvn, lvar = "Y", lvname = "Com", partner="2", type = "free"),
               "Com2 ~ NA*1"
  )
})

test_that("lmeans produces correct output for lvar = X, partner = 2, type = fixed", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lmeans(dvn, lvar = "X", lvname = "Sat", partner="2", type = "fixed"),
               "Sat2 ~ 0*1"
  )
})

test_that("lmeans produces correct output for lvar = Y, partner = 2, type = fixed", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lmeans(dvn, lvar = "Y", lvname = "Com", partner="2", type = "fixed"),
               "Com2 ~ 0*1"
  )
})

test_that("lmeans produces correct output for lvar = X, partner = 2, type = equated", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lmeans(dvn, lvar = "X", lvname = "Sat", partner="2", type = "equated"),
               "Sat2 ~ alphax*1"
  )
})

test_that("lmeans produces correct output for lvar = Y, partner = 2, type = equated", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lmeans(dvn, lvar = "Y", lvname = "Com", partner="2", type = "equated"),
               "Com2 ~ alphay*1"
  )
})

##### partner = g #####
test_that("lmeans produces correct output for lvar = X, partner = g, type = free", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lmeans(dvn, lvar = "X", lvname = "Sat", partner="g", type = "free"),
               "SatDy ~ NA*1"
  )
})

test_that("lmeans produces correct output for lvar = X, partner = g, type = fixed", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lmeans(dvn, lvar = "X", lvname = "Sat", partner="g", type = "fixed"),
               "SatDy ~ 0*1"
  )
})
