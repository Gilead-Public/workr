## Test Setup
kri_workflow <- suppressMessages(MakeWorkflowList(
  "kri0001_custom",
  strPath = GetYamlPathCustomMetrics()
))[["kri0001_custom"]]
outputs <- qual_step_outputs(kri_workflow)

## Test Code
testthat::test_that("Qual: Given appropriate metadata (i.e. vThresholds), the thresholds are parsed from metadata and passed to the flagging step (#96, #68)", {
  test <- qual_run_workflow(kri_workflow, mapped_data)
  expect_true(all(outputs %in% names(test)))
  expect_true(is.vector(test[["vThreshold"]]))
  expect_true(all(purrr::map_lgl(
    test[grep("^Analysis_", outputs, value = TRUE)],
    is.data.frame
  )))

  # the threshold vector comes from the workflow metadata
  expect_identical(
    test$vThreshold,
    as.numeric(strsplit(kri_workflow$meta$Threshold, ",")[[1]])
  )

  # the flagging step received those thresholds
  expect_identical(
    test$Analysis_Flagged,
    qualification_flag_normalapprox(
      test$Analysis_Analyzed,
      threshold = test$vThreshold
    )
  )

  # changing the metadata changes what the flagging step receives
  kri_workflow$meta$Threshold <- "-3,-2,2,3"
  test_changed <- qual_run_workflow(kri_workflow, mapped_data)
  expect_identical(test_changed$vThreshold, c(-3, -2, 2, 3))
  expect_identical(
    test_changed$Analysis_Flagged,
    qualification_flag_normalapprox(
      test_changed$Analysis_Analyzed,
      threshold = c(-3, -2, 2, 3)
    )
  )
})
