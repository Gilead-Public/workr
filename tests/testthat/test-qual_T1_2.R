## Test Code
testthat::test_that("Qual: Given workflows marked inactive in their metadata, MakeWorkflowList excludes them by default and includes them on request (#53)", {
  wf_default <- suppressMessages(MakeWorkflowList(strPath = GetYamlPathMetrics()))
  wf_all <- suppressMessages(MakeWorkflowList(
    strPath = GetYamlPathMetrics(),
    bActiveOnly = FALSE
  ))

  expect_false("kri0099" %in% names(wf_default))
  expect_true("kri0099" %in% names(wf_all))
  expect_setequal(setdiff(names(wf_all), names(wf_default)), "kri0099")
  expect_true(all(purrr::map_lgl(wf_default, ~ !isFALSE(.x$meta$Active))))
})

testthat::test_that("Qual: Given metadata filters, MakeWorkflowList and FilterWorkflows keep only workflows whose metadata match, case-insensitively (#72)", {
  wf <- suppressMessages(MakeWorkflowList(strPath = GetYamlPathQualWorkflows()))

  # filter at load time
  wf_country <- suppressMessages(MakeWorkflowList(
    strPath = GetYamlPathQualWorkflows(),
    GroupLevel = "country"
  ))
  expect_setequal(
    names(wf_country),
    names(purrr::keep(wf, ~ identical(.x$meta$GroupLevel, "Country")))
  )

  # filter after load, AND logic across fields
  wf_site_sae <- suppressMessages(FilterWorkflows(
    wf,
    GroupLevel = "SITE",
    Abbreviation = "sae"
  ))
  expect_setequal(names(wf_site_sae), c("kri0002", "kri0002_custom"))

  # workflows lacking the field are dropped
  wf_mapped <- suppressMessages(FilterWorkflows(wf, Type = "Mapped"))
  expect_setequal(names(wf_mapped), c("SUBJ", "AE"))
  expect_length(
    suppressMessages(FilterWorkflows(wf_mapped, GroupLevel = "Site")),
    0
  )
})
