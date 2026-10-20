## Qualification data and workflow helpers
##
## The structure follows the gsm.core qualification data helper (raw data,
## mapped data and workflow paths set up once for all qualification tests).
## workr cannot depend on gsm.core, so the raw data is simulated here, and the
## workflows are vendored under `qual_workflows/`.

# ---- Raw source data ---------------------------------------------------------
# Sites vary widely in size so that groups fall on both sides of the
# workflows' minimum-exposure cutoffs.
qual_simulate_raw_data <- function(seed = 123) {
  withr::with_seed(seed, {
    sites <- data.frame(
      invid = sprintf("%04d", 1:20),
      country = rep(c("US", "CA", "GB", "DE", "JP"), each = 4),
      n_subjects = c(60, 45, 40, 35, 30, 30, 25, 25, 20, 20, 15, 15, 12, 10, 8, 6, 4, 3, 2, 1)
    )

    Raw_SUBJ <- data.frame(
      studyid = "QUAL-001",
      invid = rep(sites$invid, sites$n_subjects),
      country = rep(sites$country, sites$n_subjects),
      enrollyn = "Y",
      stringsAsFactors = FALSE
    )
    Raw_SUBJ$subjid <- sprintf("S%04d", seq_len(nrow(Raw_SUBJ)))
    Raw_SUBJ$enrollyn[sample(nrow(Raw_SUBJ), 15)] <- "N"
    Raw_SUBJ$timeonstudy <- sample(30:400, nrow(Raw_SUBJ), replace = TRUE)

    n_ae <- stats::rpois(nrow(Raw_SUBJ), 0.02 * Raw_SUBJ$timeonstudy)
    Raw_AE <- data.frame(
      studyid = "QUAL-001",
      subjid = rep(Raw_SUBJ$subjid, n_ae),
      stringsAsFactors = FALSE
    )
    Raw_AE$aeser <- sample(
      c("Y", "N"),
      nrow(Raw_AE),
      replace = TRUE,
      prob = c(0.15, 0.85)
    )
    Raw_AE$aest_dt <- as.Date("2025-01-01") +
      sample(0:365, nrow(Raw_AE), replace = TRUE)

    list(
      Raw_SUBJ = Raw_SUBJ[, c("studyid", "invid", "country", "subjid", "enrollyn", "timeonstudy")],
      Raw_AE = Raw_AE
    )
  })
}

lData <- qual_simulate_raw_data()

# Data with missing values (15% NA's in every non-identifier column)
lData_missing_values <- withr::with_seed(123, {
  purrr::map(lData, function(df) {
    df %>%
      dplyr::mutate(
        dplyr::across(
          !dplyr::any_of(c("studyid", "subjid")),
          ~ replace(., sample(dplyr::row_number(), size = .15 * dplyr::n()), NA)
        )
      )
  })
})

# ---- Workflow paths ----------------------------------------------------------
GetYamlPathQualWorkflows <- function() {
  testthat::test_path("qual_workflows")
}

GetYamlPathMappings <- function() {
  file.path(GetYamlPathQualWorkflows(), "1_mappings")
}

GetYamlPathMetrics <- function() {
  file.path(GetYamlPathQualWorkflows(), "2_metrics")
}

GetYamlPathMetricsFromInput <- function() {
  file.path(GetYamlPathQualWorkflows(), "2_metrics_from_input")
}

# ---- Running workflows -------------------------------------------------------
# Run a workflow through workr and return its full lData (inputs, intermediate
# outputs and final output).
qual_run_workflow <- function(lWorkflow, lData, ...) {
  suppressMessages(
    RunWorkflow(
      lWorkflow,
      lData,
      bReturnResult = FALSE,
      bKeepInputData = TRUE,
      ...
    )
  )$lData
}

qual_step_outputs <- function(lWorkflow) {
  purrr::map_chr(lWorkflow$steps, "output")
}

mappings_wf <- suppressMessages(MakeWorkflowList(strPath = GetYamlPathMappings()))
mapped_data <- suppressMessages(RunWorkflows(mappings_wf, lData))

# ---- Double programming ------------------------------------------------------
# Independent dplyr derivation of group-level counts straight from raw data,
# without the mapping SQL or any workflow step function.
qual_double_program_counts <- function(
  lRaw,
  strGroupCol = "invid",
  bSeriousOnly = FALSE
) {
  subjects <- lRaw$Raw_SUBJ %>%
    dplyr::filter(enrollyn == "Y") %>%
    dplyr::filter(
      !is.na(.data[[strGroupCol]]),
      !is.na(timeonstudy)
    )

  events <- lRaw$Raw_AE
  if (bSeriousOnly) {
    events <- dplyr::filter(events, aeser == "Y")
  }
  event_counts <- events %>%
    dplyr::group_by(subjid) %>%
    dplyr::summarise(n_events = dplyr::n(), .groups = "drop")

  subjects %>%
    dplyr::left_join(event_counts, by = "subjid") %>%
    dplyr::mutate(n_events = dplyr::coalesce(n_events, 0L)) %>%
    dplyr::group_by(GroupID = .data[[strGroupCol]]) %>%
    dplyr::summarise(
      Numerator = sum(n_events),
      Denominator = sum(timeonstudy),
      .groups = "drop"
    ) %>%
    dplyr::mutate(Rate = Numerator / Denominator) %>%
    dplyr::arrange(GroupID)
}
