test_that("Data loading functions exist", {
  expect_true(exists("load_data_safe"))
  expect_true(exists("recode_estrategias"))
  expect_true(exists("process_wm_data"))
})

test_that("Statistical functions exist", {
  expect_true(exists("run_anova_test"))
  expect_true(exists("format_anova_table"))
})