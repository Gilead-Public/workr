## Test Setup
workflow_from_input <- suppressMessages(MakeWorkflowList(
  strPath = GetYamlPathMetricsFromInput()
))[["ae_site_from_input"]]
workflow_full <- suppressMessages(MakeWorkflowList(
  strNames = I("ae_site"),
  strPath = GetYamlPathMetrics()
))[["ae_site"]]

# Pre-processed subject-level input: the full workflow's intermediate output
subject_events <- qual_run_workflow(workflow_full, mapped_data)$Subject_Events

## Test Code
testthat::test_that("Qual: Given pre-processed input data, a workflow that starts mid-pipeline gives the same results as the full workflow (#96, #68)", {
  test <- qual_run_workflow(
    workflow_from_input,
    list(Subject_Events = subject_events)
  )

  expect_true(all(qual_step_outputs(workflow_from_input) %in% names(test)))

  # input data is passed through unchanged
  expect_identical(test$Subject_Events, subject_events)

  # the result matches the full workflow run from mapped data
  expect_identical(
    test$Group_Summary,
    suppressMessages(RunWorkflow(workflow_full, mapped_data))
  )
})
