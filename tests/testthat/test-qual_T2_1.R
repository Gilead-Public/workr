## Test Setup
workflow <- suppressMessages(MakeWorkflowList(
  strNames = I("ae_site"),
  strPath = GetYamlPathMetrics()
))[["ae_site"]]

outputs <- qual_step_outputs(workflow)

## Test Code
testthat::test_that("Qual: Given raw data, mapping and analysis workflows chain their steps to produce group-level results that match an independent derivation (#96, #68)", {
  test <- qual_run_workflow(workflow, mapped_data)
  expected <- qual_double_program_counts(lData, strGroupCol = "invid")

  # every step's output is kept, and the data frames are data frames
  expect_true(all(outputs %in% names(test)))
  expect_true(all(purrr::map_lgl(test[outputs], is.data.frame)))

  # results match counts recomputed with dplyr from raw data, independent of
  # the mapping SQL and the workflow steps
  summary <- test$Group_Summary
  expect_identical(summary$GroupID, expected$GroupID)
  expect_equal(summary$Numerator, expected$Numerator)
  expect_equal(summary$Denominator, expected$Denominator)
  expect_equal(summary$Rate, expected$Rate)

  # the final step's output is the workflow's result
  expect_identical(
    suppressMessages(RunWorkflow(workflow, mapped_data)),
    test$Group_Summary
  )
})
