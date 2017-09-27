context("Name_regions")

test_that("inflate_region_names gives expected results", {


  expect_equal(c(Reg_1 = 1, Reg_2 = 2, critical = 3, spillover = 4, Reg_5 = 5),
               inflate_region_names(reg.names = c(critical = 3, spillover = 4),
                                    regions = 1:5)
               )

  expect_equal(c(R_1 = 1, R_2 = 2 ,critical = 3),
               inflate_region_names(reg.names = c(critical = 3),
                                    regions = 1:3,
                                    missing.prefix = "R_"))

  expect_equal(c(Reg_a = "a", Reg_b = "b", Reg_c = "c"),
               inflate_region_names(reg.names = c(),
                                    regions = c("a","b","c")))

})

test_that("name_regions_in_subset gives expected results",{
  test_dat <- data.frame(type = c("type1", "type1", "type1",
                                  "type2", "type2", "type2"),
                         region = c(1,2,2,1,2,2),
                         stringsAsFactors = FALSE)

  expected <- data.frame(type = c("type1", "type1", "type1",
                                  "type2", "type2", "type2"),
                         region = c(1,2,2,1,2,2),
                         region.names = c("critical", "spillover", "spillover",
                                       "critical","spillover", "spillover"),
                         stringsAsFactors = FALSE)

  expect_equal(name_regions_in_subset(test_dat, region.col = region,
                                      reg.names = c(critical = 1, spillover = 2),
                                      reg.names.col = "region.names"),
               expected)

})
