#### sipExtractor####
test_that("sipExtractor produces correct output", {

  expect_equal(sipExtractor(dat = DRES, stem = "PRQC", delim1 = "_",
                            item_num = "\\d+", delim2 = ".", distinguish = "1"),
               c("PRQC_1.1", "PRQC_2.1", "PRQC_3.1", "PRQC_4.1", "PRQC_5.1", "PRQC_6.1", "PRQC_7.1", "PRQC_8.1", "PRQC_9.1")
  )
})


#### spiExtractor####
test_that("spiExtractor produces correct output", {

  expect_equal(spiExtractor(dat = commitmentQ, stem = "sat.g", delim1 = ".",
                            item_num = "\\d+", delim2 = "_", distinguish = "1"),
               c("sat.g.1_1", "sat.g.1_2", "sat.g.1_3", "sat.g.1_4", "sat.g.1_5")
  )
})



# TODO: longitudinal helper tests (once built in data set available)
