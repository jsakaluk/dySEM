#### apim/mim actor  slopes ####

test_that("lregs produces correct actor scripts for param = act, type = equated", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lregs(dvn,
                     param = "act",
                     type = "equated",
                     lvxname = "Sat", lvyname = "Com"),
               "Com1 ~ a*Sat1\nCom2 ~ a*Sat2"
  )
})

test_that("lregs produces correct actor scripts for param = act, type = zero", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lregs(dvn,
                     param = "act",
                     type = "zero",
                     lvxname = "Sat", lvyname = "Com"),
               "Com1 ~ 0*Sat1\nCom2 ~ 0*Sat2"
  )
})


#### apim partner  slopes ####

test_that("lregs produces correct apim-partner scripts for param = apim_part, type = equated", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lregs(dvn,
                     param = "apim_part",
                     type = "equated",
                     lvxname = "Sat", lvyname = "Com"),
               "Com1 ~ p*Sat2\nCom2 ~ p*Sat1"
  )
})

test_that("lregs produces correct apim-partner scripts for param = apim_part, type = zero", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lregs(dvn,
                     param = "apim_part",
                     type = "zero",
                     lvxname = "Sat", lvyname = "Com"),
               "Com1 ~ 0*Sat2\nCom2 ~ 0*Sat1"
  )
})


#### mim partner  slopes ####

test_that("lregs produces correct mim-partner scripts for param = mim_part, type = equated", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  expect_equal(lregs(dvn,
                     param = "mim_part",
                     type = "equated",
                     lvxname = "Sat", lvyname = "Com"),
               "Com1 ~ p*Com2\nCom2 ~ p*Com1"
  )
})

