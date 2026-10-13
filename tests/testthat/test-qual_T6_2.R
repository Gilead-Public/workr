## Test Setup
metric_workflows <- suppressMessages(MakeWorkflowList(
  "kri0001",
  strPath = GetYamlPathMetrics()
))

## Test Code
testthat::test_that("Qual: Given workflows whose required inputs are not yet available, RunWorkflows stops by default and, when asked to continue on error, records the failure and runs the remaining workflows (#67)", {
  # metrics placed ahead of the mappings they depend on
  out_of_order <- c(metric_workflows["kri0001"], mappings_wf)

  expect_error(
    suppressMessages(RunWorkflows(out_of_order, lData)),
    "Mapped_AE"
  )

  test <- suppressMessages(
    RunWorkflows(out_of_order, lData, bContinueOnError = TRUE)
  )
  expect_identical(
    test$status$workflow,
    c("Analysis_kri0001", paste0("Mapped_", names(mappings_wf)))
  )
  expect_identical(test$status$status, c("error", "success", "success"))
  expect_identical(test$failures$workflow, "Analysis_kri0001")
  expect_identical(
    names(test$results),
    paste0("Mapped_", names(mappings_wf))
  )
})
