## Test Setup
ae_workflow <- suppressMessages(MakeWorkflowList(
  "kri0001b_custom",
  strPath = GetYamlPathCustomMetrics()
))[["kri0001b_custom"]]

outputs <- qual_step_outputs(ae_workflow)

# Pre-processed subject-level input, analogous to gsm.core::analyticsInput
analytics_input <- qual_input_rate(
  dfSubjects = mapped_data$Mapped_SUBJ,
  dfNumerator = mapped_data$Mapped_AE
)

## Test Code
testthat::test_that("Qual: Given pre-processed input data, a properly specified Workflow for a KRI creates summarized and flagged data (#96, #68)", {
  test <- qual_run_workflow(
    ae_workflow,
    list(Analysis_Input = analytics_input)
  )

  expect_true(all(outputs %in% names(test)))
  expect_true(is.vector(test$vThreshold))
  expect_true(all(purrr::map_lgl(
    test[grep("^Analysis_", outputs, value = TRUE)],
    is.data.frame
  )))
  expect_equal(nrow(test$Analysis_Flagged), nrow(test$Analysis_Summary))
  expect_identical(
    sort(test$Analysis_Flagged$GroupID),
    sort(test$Analysis_Summary$GroupID)
  )
  expect_setequal(
    test$Analysis_Summary$GroupID,
    unique(analytics_input$GroupID)
  )

  # input data is passed through unchanged
  expect_identical(test$Analysis_Input, analytics_input)

  # starting from pre-processed input gives the same result as running the
  # equivalent full workflow from mapped data
  full_workflow <- suppressMessages(MakeWorkflowList(
    "kri0001_custom",
    strPath = GetYamlPathCustomMetrics()
  ))[["kri0001_custom"]]
  test_full <- qual_run_workflow(full_workflow, mapped_data)
  expect_identical(test$Analysis_Summary, test_full$Analysis_Summary)
})
