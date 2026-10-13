## Test Setup
kri_workflows <- c(
  suppressMessages(MakeWorkflowList(
    strNames = c("kri0001", "cou0001"),
    strPath = GetYamlPathMetrics()
  )),
  suppressMessages(MakeWorkflowList(
    "kri0001_custom",
    strPath = GetYamlPathCustomMetrics()
  ))
)

# Run each workflow up to (but not including) the summary step
partial_mapped_workflows <- purrr::map(kri_workflows, function(workflow) {
  workflow$steps <- utils::head(workflow$steps, -1)
  qual_run_workflow(workflow, mapped_data)
})

## Test Code
testthat::test_that("Qual: Given an accrual threshold in workflow metadata, the summary step receives it, so flag values for sites with low enrollment follow the metadata (#96, #68)", {
  # define custom min denominator
  test_nMinDenominator <- c(500, 2000, 5000)

  # test for identical output between the workflow, with the accrual
  # threshold set in its metadata, and a direct call to the summary step
  yaml_test <- purrr::map(test_nMinDenominator, function(test) {
    purrr::imap(partial_mapped_workflows, function(partial, id) {
      workflow <- kri_workflows[[id]]
      workflow$meta$AccrualThreshold <- test
      workflow$steps <- utils::tail(workflow$steps, 1)
      suppressMessages(RunWorkflow(workflow, partial))
    })
  })

  function_test <- purrr::map(test_nMinDenominator, function(test) {
    purrr::map(
      partial_mapped_workflows,
      ~ qual_summarize(.x$Analysis_Flagged, nAccrualThreshold = test)
    )
  })

  expect_identical(yaml_test, function_test)

  # the metadata value matters: different thresholds give different summaries
  expect_false(identical(yaml_test[[1]], yaml_test[[3]]))
})
