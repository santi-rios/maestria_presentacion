source("../../R/functions.R")

# tests/testthat/test-anova-functions.R



test_that("run_anova_test works with valid data", {
  # Create sample data with unique IDs
  test_data <- data.frame(
    id = factor(1:16),  # Unique ID for each observation
    Tratamiento = factor(rep(c("Salina", "Fluoxetina"), each = 8)),
    Estres = factor(rep(c("Control", "CUMS"), times = 8)),
    response = rnorm(16, mean = 10, sd = 2)
  )
  
  # Test between-subjects ANOVA
  result <- run_anova_test(
    df = test_data,
    dv = "response",
    id_col = "id",
    between_cols = c("Tratamiento", "Estres")
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("p" %in% colnames(result) || "p.value" %in% colnames(result))
})

test_that("format_anova_table produces proper output", {
  # Create mock ANOVA results that match rstatix output
  mock_anova <- data.frame(
    Effect = "Tratamiento",
    DFn = 1,
    DFd = 6,
    F = 2.5,
    p = 0.045,
    ges = 0.3,
    stringsAsFactors = FALSE
  )
  
  # Test that function doesn't error
  expect_no_error({
    result <- format_anova_table(mock_anova, "Test ANOVA")
  })
  
  # Test with actual function call
  result <- format_anova_table(mock_anova, "Test ANOVA")
  expect_s3_class(result, "knitr_kable")
})

test_that("format_anova_table handles invalid input", {
  # Test error handling
  expect_error(
    format_anova_table("not a dataframe"), 
    "anova_results must be a data frame"
  )
})