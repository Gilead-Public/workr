## Test Setup
workflows <- suppressMessages(MakeWorkflowList(
  strNames = I(c("ae_site", "ae_country")),
  strPath = GetYamlPathMetrics()
))

## Test Code
testthat::test_that("Qual: Given two workflows with the same steps but different metadata, each workflow's steps receive that workflow's own metadata when run together (#96, #68)", {
  # the two workflows differ only in their metadata
  expect_identical(workflows$ae_site$steps, workflows$ae_country$steps)
  expect_identical(workflows$ae_site$spec, workflows$ae_country$spec)

  test <- suppressMessages(RunWorkflows(
    workflows,
    mapped_data,
    bReturnResult = FALSE
  ))
  site <- test$Analysis_ae_site$lData
  country <- test$Analysis_ae_country$lData

  # steps that don't use the metadata give identical outputs
  expect_identical(site$Event_Counts, country$Event_Counts)
  expect_identical(site$Subject_Events, country$Subject_Events)

  # steps that do use it follow each workflow's own metadata
  purrr::walk2(
    list(site, country),
    workflows[c("ae_site", "ae_country")],
    function(result, workflow) {
      expected <- qual_double_program_counts(
        lData,
        strGroupCol = workflow$meta$GroupCol
      )
      expect_identical(result$Group_Summary$GroupID, expected$GroupID)
      expect_identical(
        result$Group_Summary$LowExposure,
        result$Group_Summary$Denominator < workflow$meta$MinExposure
      )
    }
  )
})
