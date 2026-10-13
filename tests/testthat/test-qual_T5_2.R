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
testthat::test_that("Qual: Given appropriate raw participant-level data, flag values are correctly assigned as NA for sites with low enrollment (#96, #68)", {
  # define custom min denominator
  test_nMinDenominator <- c(500, 2000, 5000)

  # number of NA flags equals number of groups below the accrual threshold
  test_output <- purrr::map(test_nMinDenominator, function(test) {
    purrr::map(kri_workflows, function(workflow) {
      workflow$meta$AccrualThreshold <- test
      result <- qual_run_workflow(workflow, mapped_data)
      sum(is.na(result$Analysis_Summary$Flag))
    })
  })

  hardcode_output <- purrr::map(test_nMinDenominator, function(test) {
    purrr::map(partial_mapped_workflows, function(df) {
      sum(df$Analysis_Flagged$Denominator < test)
    })
  })

  expect_equal(test_output, hardcode_output)
  expect_gt(sum(unlist(hardcode_output)), 0)

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
})
