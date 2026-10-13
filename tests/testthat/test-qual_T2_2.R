testthat::test_that("Qual: Given raw participant-level data with missingness, a properly specified Workflow for a KRI creates summarized and flagged data (#68)", {
  kri_workflow <- suppressMessages(MakeWorkflowList(
    strNames = "kri0001",
    strPath = GetYamlPathMetrics()
  ))[["kri0001"]]
  outputs <- qual_step_outputs(kri_workflow)

  mapped_data_missing_values <- suppressMessages(
    RunWorkflows(mappings_wf, lData_missing_values)
  )

  expect_warning(
    test <- qual_run_workflow(kri_workflow, mapped_data_missing_values),
    "subject\\(s\\) with missing values removed"
  )
  expected <- qual_double_program_counts(
    lData_missing_values,
    strGroupCol = "invid"
  )

  # test output structure
  expect_true(all(outputs %in% names(test)))
  expect_true(is.vector(test$vThreshold))
  expect_true(all(purrr::map_lgl(
    test[grep("^Analysis_", outputs, value = TRUE)],
    is.data.frame
  )))
  expect_equal(nrow(test$Analysis_Flagged), nrow(expected))
  expect_equal(nrow(test$Analysis_Summary), nrow(expected))
  expect_identical(
    sort(test$Analysis_Flagged$GroupID),
    sort(test$Analysis_Summary$GroupID)
  )
  expect_false(anyNA(test$Analysis_Summary$GroupID))

  # test output content: subjects with missing data are excluded from counts
  transformed <- dplyr::arrange(test$Analysis_Transformed, GroupID)
  expect_identical(transformed$GroupID, expected$GroupID)
  expect_equal(transformed$Numerator, expected$Numerator)
  expect_equal(transformed$Denominator, expected$Denominator)
})
