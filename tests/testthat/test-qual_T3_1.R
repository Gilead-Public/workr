## Test Setup
workflow <- suppressMessages(MakeWorkflowList(
  strNames = I("ae_site"),
  strPath = GetYamlPathMetrics()
))[["ae_site"]]

## Test Code
testthat::test_that("Qual: Given workflow metadata referenced by step parameters, the metadata values reach the steps, and changing the metadata changes the results (#96, #68)", {
  test <- qual_run_workflow(workflow, mapped_data)

  # GroupCol: invid in the metadata sets the grouping column
  expect_setequal(
    test$Group_Summary$GroupID,
    unique(mapped_data$Mapped_SUBJ$invid)
  )

  # MinExposure in the metadata sets the cutoff
  expect_identical(
    test$Group_Summary$LowExposure,
    test$Group_Summary$Denominator < workflow$meta$MinExposure
  )
  expect_true(any(test$Group_Summary$LowExposure))
  expect_false(all(test$Group_Summary$LowExposure))

  # changing the metadata changes what the steps receive
  workflow$meta$GroupCol <- "country"
  workflow$meta$MinExposure <- 5000
  test_changed <- qual_run_workflow(workflow, mapped_data)
  expected <- qual_double_program_counts(lData, strGroupCol = "country")

  expect_identical(test_changed$Group_Summary$GroupID, expected$GroupID)
  expect_equal(test_changed$Group_Summary$Denominator, expected$Denominator)
  expect_identical(
    test_changed$Group_Summary$LowExposure,
    test_changed$Group_Summary$Denominator < 5000
  )
})
