## Test Setup
workflow <- suppressMessages(MakeWorkflowList(
  strNames = I("ae_site"),
  strPath = GetYamlPathMetrics()
))[["ae_site"]]

## Test Code
testthat::test_that("Qual: Given input data that does not satisfy a workflow's data specification, RunWorkflow stops before running any step (#96, #68)", {
  # count step calls so we can confirm nothing ran
  step_calls <- 0
  traced_workflow <- workflow
  traced_workflow$steps[[1]]$name <- "qual_counting_step"
  assign(
    "qual_counting_step",
    function(...) {
      step_calls <<- step_calls + 1
      workr::RunQuery(...)
    },
    envir = .GlobalEnv
  )
  on.exit(rm("qual_counting_step", envir = .GlobalEnv), add = TRUE)

  # a spec-declared domain is missing
  expect_error(
    suppressMessages(RunWorkflow(traced_workflow, mapped_data["Mapped_SUBJ"])),
    "Spec-declared input 'Mapped_AE' not found in lData"
  )
  expect_identical(step_calls, 0)

  # a spec declaring required columns is enforced
  traced_workflow$spec$Mapped_SUBJ <- list(
    required_cols = c("subjid", "invid", "timeonstudy")
  )
  bad_data <- mapped_data
  bad_data$Mapped_SUBJ$timeonstudy <- NULL
  expect_error(
    suppressMessages(RunWorkflow(traced_workflow, bad_data)),
    "Missing required columns in 'Mapped_SUBJ': timeonstudy"
  )
  expect_identical(step_calls, 0)

  # the same workflow runs once its specification is satisfied
  result <- suppressMessages(RunWorkflow(traced_workflow, mapped_data))
  expect_identical(step_calls, 1)
  expect_s3_class(result, "data.frame")
})
