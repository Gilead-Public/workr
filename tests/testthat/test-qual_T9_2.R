## Test Code
testthat::test_that("Qual: Given phases listed out of dependency order, RunProject runs them in the order given and stops when a phase's inputs are unavailable (#63)", {
  expect_error(
    suppressMessages(RunProject(
      GetYamlPathQualWorkflows(),
      lData = lData,
      strPhases = c("2_metrics", "1_mappings")
    )),
    "Mapped_AE"
  )
})
