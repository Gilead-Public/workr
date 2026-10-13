## Test Setup
yaml_files <- list.files(
  GetYamlPathQualWorkflows(),
  pattern = "\\.yaml$",
  recursive = TRUE,
  full.names = TRUE
)
raw_yaml <- purrr::map(yaml_files, yaml::read_yaml)
names(raw_yaml) <- purrr::map_chr(raw_yaml, c("meta", "ID"))
active_ids <- names(purrr::discard(raw_yaml, ~ isFALSE(.x$meta$Active)))

## Test Code
testthat::test_that("Qual: Given a directory of workflow YAML specifications, MakeWorkflowList loads every active workflow keyed by its ID with meta, spec and steps intact (#96, #68)", {
  wf <- suppressMessages(MakeWorkflowList(strPath = GetYamlPathQualWorkflows()))

  # one entry per active YAML, named by meta$ID
  expect_setequal(names(wf), active_ids)
  expect_identical(names(wf), unname(purrr::map_chr(wf, c("meta", "ID"))))

  # workflow content matches the YAML on disk
  purrr::iwalk(wf, function(workflow, id) {
    expect_identical(workflow$steps, raw_yaml[[id]]$steps)
    expect_identical(workflow$spec, raw_yaml[[id]]$spec)
    expect_identical(
      workflow$meta[names(raw_yaml[[id]]$meta)],
      raw_yaml[[id]]$meta
    )
    expect_true(file.exists(workflow$path))
  })

  # workflows are ordered by Priority, so mappings come before metrics
  priorities <- purrr::map_dbl(wf, c("meta", "Priority"))
  expect_false(is.unsorted(priorities))
  expect_setequal(names(wf)[priorities == 1], names(mappings_wf))
})

testthat::test_that("Qual: Given workflow names, MakeWorkflowList loads only the requested workflows, matching partially by default and exactly on request (#96, #68)", {
  # partial matching: "ae_site" also matches "sae_site"
  wf <- suppressMessages(MakeWorkflowList(
    strNames = "ae_site",
    strPath = GetYamlPathMetrics()
  ))
  expect_setequal(names(wf), c("ae_site", "sae_site"))

  wf_exact <- suppressMessages(MakeWorkflowList(
    strNames = I("ae_site"),
    strPath = GetYamlPathMetrics()
  ))
  expect_identical(names(wf_exact), "ae_site")
})
