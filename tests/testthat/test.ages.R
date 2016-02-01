age_range14 <- test_that("Estimated ages for 2014 are valid ages", {
  expect_true(min(ages_2(2014)) > 22)
  expect_true(max(ages_2(2014)) < 95)
}
)

age_range13 <- test_that("Estimated ages for 2013 are valid ages", {
  expect_true(min(ages_1(2013)) > 22)
  expect_true(max(ages_1(2013)) < 95)
}
)

age_range12 <- test_that("Estimated ages for 2012 are valid ages", {
  expect_true(min(ages_1(2012)) > 22)
  expect_true(max(ages_1(2012)) < 95)
}
)

age_range11 <- test_that("Estimated ages for 2011 are valid ages", {
  expect_true(min(ages_1(2011)) > 22)
  expect_true(max(ages_1(2011)) < 95)
}
)

age_range10 <- test_that("Estimated ages for 2010 are valid ages", {
  expect_true(min(ages_1(2010)) > 22)
  expect_true(max(ages_1(2010)) < 95)
}
)

age_range09 <- test_that("Estimated ages for 2009 are valid ages", {
  expect_true(min(ages_1(2009)) > 22)
  expect_true(max(ages_1(2009)) < 95)
}
)

age_range08 <- test_that("Estimated ages for 2008 are valid ages", {
  expect_true(min(ages_1(2008)) > 22)
  expect_true(max(ages_1(2008)) < 95)
}
)

age_range07 <- test_that("Estimated ages for 2007 are valid ages", {
  expect_true(min(ages_1(2007)) > 22)
  expect_true(max(ages_1(2007)) < 95)
}
)

age_range06 <- test_that("Estimated ages for 2006 are valid ages", {
  expect_true(min(ages_1(2006)) > 22)
  expect_true(max(ages_1(2006)) < 95)
}
)

age_range05 <- test_that("Estimated ages for 2005 are valid ages", {
  expect_true(min(ages_1(2005)) > 22)
  expect_true(max(ages_1(2005)) < 95)
}
)

age_range04 <- test_that("Estimated ages for 2004 are valid ages", {
  expect_true(min(ages_1(2004)) > 22)
  expect_true(max(ages_1(2004)) < 95)
}
)
