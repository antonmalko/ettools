context("report_NAs/report_extremes correct results")

test_dat <- data.frame(subj = rep(c(1,2), each = 12),
                       item = rep(c(10,20), times = 2, each = 6),
                       region = rep(c(5,6), times = 4, each = 3),
                       reg.name = rep(c("critical", "spillover"), times = 4, each = 3),
                       fixationtype = rep(c("fp","rp","tt"), times = 4),
                       value = c(NA, 2000, 500, # each row is a region, each column - a measure
                                 NA, NA, 10000, # rows 1,2,5,6 are for item 10;
                                 300, 200, 500, # rows 3,4,7,8 are for item 20;
                                 400, 400, 400, # rows 1-4 are for subj 1;
                                 NA, 50, 50,    # rows 5-8 are for subj 2
                                 NA, 10000, NA,
                                 50, 2000, 50,
                                 500, 500, 500))

test_that("NA counts by subject and item are correct",{

  by.subj.expected <- data.frame(subj = rep(c(1,2), each = 6),
                                cell = rep(c("critical_fp","critical_rp", "critical_tt",
                                             "spillover_fp", "spillover_rp", "spillover_tt"),
                                           times = 2),
                                count = c(1, 0, 0,
                                          1, 1, 0,
                                          1, 0, 0,
                                          1, 0, 1))

  by.subj <- report_NAs_count(dat = test_dat,
                              by = subj,
                              rois = c("critical","spillover"),
                              mois = c("fp", "rp", "tt"))

  expect_equal(by.subj$counts, by.subj.expected)


  by.item.expected <- data.frame(item = rep(c(10,20), each = 6),
                                 cell = rep(c("critical_fp","critical_rp", "critical_tt",
                                              "spillover_fp", "spillover_rp", "spillover_tt"),
                                            times = 2),
                                 count = c(2, 0, 0,
                                           2, 1, 1,
                                           0, 0, 0,
                                           0, 0, 0))

  by.item <- report_NAs_count(dat = test_dat,
                              by = item,
                              rois = c("critical","spillover"),
                              mois = c("fp", "rp", "tt"))

  expect_equal(by.item$counts, by.item.expected)

})

test_that("Extreme values counts by subject and item are correct", {

  # Assume that high extreme values are > 1000 ms, and low < 150ms

  # --------- by subjects
  by.subj.expected <- data.frame(subj = rep(c(1,2), each = 6, times = 2),
                                 cell = factor(rep(c("critical_fp","critical_rp", "critical_tt",
                                              "spillover_fp", "spillover_rp", "spillover_tt"),
                                            times = 4),
                                            levels = c("critical_fp","critical_rp", "critical_tt",
                                                       "spillover_fp", "spillover_rp", "spillover_tt")),
                                 count = c(0, 1, 0, # first four rows for above.max
                                           0, 0, 1,
                                           0, 1, 0,
                                           0, 1, 0,
                                           0, 0, 0, # second four rows for below.min
                                           0, 0, 0,
                                           1, 1, 2,
                                           0, 0, 0),
                                 direction = rep(c("above.max", "below.min"), each = 12),
                                 stringsAsFactors = FALSE)



  by.subj <- report_extremes_count(dat = test_dat,
                              by = subj,
                              rois = c("critical","spillover"),
                              mois = c("fp", "rp", "tt"),
                              max.cutoff = 1000, min.cutoff = 150)

  expect_equal(by.subj$counts, by.subj.expected)

  # --------- by items

  by.item.expected <- data.frame(item = rep(c(10,20), each = 6, times = 2),
                                 cell = factor(rep(c("critical_fp","critical_rp", "critical_tt",
                                                     "spillover_fp", "spillover_rp", "spillover_tt"),
                                                   times = 4),
                                               levels = c("critical_fp","critical_rp", "critical_tt",
                                                          "spillover_fp", "spillover_rp", "spillover_tt")),
                                 count = c(0, 1, 0,
                                           0, 1, 1,
                                           0, 1, 0,
                                           0, 0, 0,
                                           0, 1, 1,
                                           0, 0, 0,
                                           1, 0, 1,
                                           0, 0, 0),
                                 direction = rep(c("above.max", "below.min"), each = 12),
                                 stringsAsFactors = FALSE)



  by.item <- report_extremes_count(dat = test_dat,
                                   by = item,
                                   rois = c("critical","spillover"),
                                   mois = c("fp", "rp", "tt"),
                                   max.cutoff = 1000, min.cutoff = 150)

  expect_equal(by.item$counts, by.item.expected)

})

test_that("report_NAs/report_extremes throw a warning if no relevant cells found",{

  test_dat_noNAs <- test_dat
  test_dat_noNAs[is.na(test_dat_noNAs$value), "value"] <- 0

  expect_warning(report_NAs_count(dat = test_dat_noNAs,
                                  by = item,
                                  rois = c("critical","spillover"),
                                  mois = c("fp", "rp", "tt")),
                 "There are no NAs in column `value`")

  expect_warning(report_extremes_count(dat = test_dat,
                                       by = item,
                                       rois = c("critical","spillover"),
                                       mois = c("fp", "rp", "tt"),
                                       max.cutoff = 15000, min.cutoff = 150),
                 "There are no observations higher than 15000")

  expect_warning(report_extremes_count(dat = test_dat,
                                       by = item,
                                       rois = c("critical","spillover"),
                                       mois = c("fp", "rp", "tt"),
                                       max.cutoff = 1000, min.cutoff = 0),
                 "There are no observations lower than 0")

})

