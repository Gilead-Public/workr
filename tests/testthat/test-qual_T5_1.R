# Test Setup -------------------------------------------------------
ae_workflow_custom <- suppressMessages(MakeWorkflowList(
  "kri0001_custom",
  strPath = GetYamlPathCustomMetrics()
))[["kri0001_custom"]]
ae_workflow_default <- suppressMessages(MakeWorkflowList(
  strNames = "kri0001",
  strPath = GetYamlPathMetrics()
))[["kri0001"]]

outputs <- qual_step_outputs(ae_workflow_custom)

## Test Code -------------------------------------------------------
testthat::test_that("Qual: Given two workflows that differ only in their threshold metadata, each workflow's steps receive that workflow's own metadata when run together (#96, #68)", {
  # the two workflows have the same steps and differ only in metadata
  expect_identical(ae_workflow_default$steps, ae_workflow_custom$steps)
  expect_false(identical(
    ae_workflow_default$meta$Threshold,
    ae_workflow_custom$meta$Threshold
  ))

  test <- suppressMessages(RunWorkflows(
    list(ae_workflow_default, ae_workflow_custom),
    mapped_data,
    bReturnResult = FALSE
  ))
  test_default <- test$Analysis_kri0001$lData
  test_custom <- test$Analysis_kri0001_custom$lData

  expect_true(all(outputs %in% names(test_default)))
  expect_true(all(outputs %in% names(test_custom)))

  # steps that don't use the thresholds give identical outputs
  for (output in c("Analysis_Input", "Analysis_Transformed", "Analysis_Analyzed")) {
    expect_identical(test_default[[output]], test_custom[[output]])
  }

  # each workflow parsed its own thresholds and passed them, and its own
  # accrual threshold, to its flagging and summary steps
  purrr::walk2(
    list(test_default, test_custom),
    list(ae_workflow_default, ae_workflow_custom),
    function(result, workflow) {
      expect_identical(
        result$vThreshold,
        as.numeric(strsplit(workflow$meta$Threshold, ",")[[1]])
      )
      expect_identical(
        result$Analysis_Flagged,
        qualification_flag_normalapprox(
          result$Analysis_Analyzed,
          threshold = result$vThreshold
        )
      )
      expect_identical(
        result$Analysis_Summary,
        qual_summarize(
          result$Analysis_Flagged,
          nAccrualThreshold = workflow$meta$AccrualThreshold
        )
      )
    }
  )
})
