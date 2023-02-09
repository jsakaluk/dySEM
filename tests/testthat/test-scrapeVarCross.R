#### scrapeVarCross spi order####
test_that("scrapeVarCross produces correct output for LV X for spi order", {

  expect_equal(scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                              x_delim2="_", distinguish_1="1", distinguish_2="2"),
               list(p1xvarnames = c("sat.g.1_1", "sat.g.1_2", "sat.g.1_3", "sat.g.1_4",
                                    "sat.g.1_5"), p2xvarnames = c("sat.g.2_1", "sat.g.2_2", "sat.g.2_3",
                                                                  "sat.g.2_4", "sat.g.2_5"), xindper = 5L, dist1 = "1", dist2 = "2",
                    indnum = 10L)
  )
})

test_that("scrapeVarCross produces correct output for LV X and LV Y for spi order", {

  expect_equal(scrapeVarCross(dat = commitmentQ,
                              x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                              y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                              distinguish_1="1", distinguish_2="2"),
               list(p1xvarnames = c("sat.g.1_1", "sat.g.1_2", "sat.g.1_3", "sat.g.1_4",
                                    "sat.g.1_5"), p2xvarnames = c("sat.g.2_1", "sat.g.2_2", "sat.g.2_3",
                                                                  "sat.g.2_4", "sat.g.2_5"), xindper = 5L, dist1 = "1", dist2 = "2",
                    p1yvarnames = c("com.1_1", "com.1_2", "com.1_3", "com.1_4",
                                    "com.1_5"), p2yvarnames = c("com.2_1", "com.2_2", "com.2_3",
                                                                "com.2_4", "com.2_5"), yindper = 5L, indnum = 20L)
  )
})

#### scrapeVarCross sip order####

test_that("scrapeVarCross produces correct output for LV X for sip order", {

  expect_equal(scrapeVarCross(dat = DRES,
                              x_order = "sip", x_stem = "PRQC", x_delim1 = "_", x_delim2=".",
                              distinguish_1="1", distinguish_2="2"),
               list(p1xvarnames = c("PRQC_1.1", "PRQC_2.1", "PRQC_3.1", "PRQC_4.1",
                                    "PRQC_5.1", "PRQC_6.1", "PRQC_7.1", "PRQC_8.1", "PRQC_9.1"),
                    p2xvarnames = c("PRQC_1.2", "PRQC_2.2", "PRQC_3.2", "PRQC_4.2",
                                    "PRQC_5.2", "PRQC_6.2", "PRQC_7.2", "PRQC_8.2", "PRQC_9.2"
                    ), xindper = 9L, dist1 = "1", dist2 = "2", indnum = 18L)
  )
})

test_that("scrapeVarCross produces correct output for LV X and LV Y for sip order", {

  expect_equal(scrapeVarCross(dat = DRES,
                              x_order = "sip", x_stem = "PRQC", x_delim1 = "_", x_delim2=".",
                              y_order="sip", y_stem="sexsat", y_delim2=".",
                              distinguish_1="1", distinguish_2="2"),
               list(p1xvarnames = c("PRQC_1.1", "PRQC_2.1", "PRQC_3.1", "PRQC_4.1",
                                    "PRQC_5.1", "PRQC_6.1", "PRQC_7.1", "PRQC_8.1", "PRQC_9.1"),
                    p2xvarnames = c("PRQC_1.2", "PRQC_2.2", "PRQC_3.2", "PRQC_4.2",
                                    "PRQC_5.2", "PRQC_6.2", "PRQC_7.2", "PRQC_8.2", "PRQC_9.2"
                    ), xindper = 9L, dist1 = "1", dist2 = "2", p1yvarnames = c("sexsat1.1",
                                                                               "sexsat2.1", "sexsat3.1", "sexsat4.1", "sexsat5.1"), p2yvarnames = c("sexsat1.2",
                                                                                                                                                    "sexsat2.2", "sexsat3.2", "sexsat4.2", "sexsat5.2"), yindper = 5L,
                    indnum = 28L)
  )
})


#TODO: make unit tests for dfs with unequal items per partner to test error
