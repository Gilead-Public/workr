## Qualification step functions
##
## Ported from the gsm.core qualification helpers (tests/testthat/
## helper-qualification.R, prior to the workr extraction). In gsm.core these
## were independent reference implementations of the KRI analytics. workr has
## no analytics of its own, so here they act as the step functions that the
## qualification workflows in `qual_workflows/` call by name. That gives the
## qualification tests a realistic multi-step KRI pipeline to drive through
## the workr runtime without taking a dependency on gsm.core (which itself
## imports workr).
##
## `RunStep()` resolves bare function names from the global environment, so
## every step function is assigned there at the bottom of this file.

# Threshold parsing -----------------------------------------------------------
qual_parse_threshold <- function(strThreshold) {
  as.numeric(trimws(strsplit(strThreshold, ",", fixed = TRUE)[[1]]))
}

# Input -----------------------------------------------------------------------
# Subject-level input: one row per subject with an event count (Numerator) and
# an exposure (Denominator). Subjects with missing identifiers or exposure are
# dropped with a warning, matching gsm.core's Input_Rate() behaviour.
qual_input_rate <- function(
  dfSubjects,
  dfNumerator,
  strSubjectCol = "subjid",
  strGroupCol = "invid",
  strGroupLevel = "Site",
  strDenominatorCol = "timeonstudy"
) {
  subjects <- data.frame(
    SubjectID = dfSubjects[[strSubjectCol]],
    GroupID = dfSubjects[[strGroupCol]],
    Denominator = dfSubjects[[strDenominatorCol]]
  )

  incomplete <- !stats::complete.cases(subjects)
  if (any(incomplete)) {
    warning(
      sum(incomplete),
      " subject(s) with missing values removed.",
      call. = FALSE
    )
  }
  subjects <- subjects[!incomplete, , drop = FALSE]

  counts <- dfNumerator %>%
    dplyr::count(SubjectID = .data[[strSubjectCol]], name = "Numerator")

  subjects %>%
    dplyr::left_join(counts, by = "SubjectID") %>%
    dplyr::mutate(
      Numerator = dplyr::coalesce(Numerator, 0L),
      GroupLevel = strGroupLevel
    ) %>%
    dplyr::select(GroupLevel, GroupID, SubjectID, Numerator, Denominator) %>%
    dplyr::arrange(SubjectID)
}

# Transform -------------------------------------------------------------------
qualification_transform_counts <- function(
  dfInput,
  countCol = "Count",
  exposureCol = "Exposure",
  GroupID = "SiteID"
) {
  if (is.na(exposureCol)) {
    dfTransformed <- dfInput %>%
      dplyr::filter(!is.na(.data[[countCol]])) %>%
      dplyr::group_by(GroupID = .data[[GroupID]]) %>%
      dplyr::summarise(
        TotalCount = sum(.data[[countCol]]),
        Metric = TotalCount
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(GroupID, TotalCount, Metric) %>%
      unique()
  } else {
    dfTransformed <- dfInput %>%
      dplyr::filter(!is.na(.data[[countCol]])) %>%
      dplyr::group_by(GroupID = .data[[GroupID]]) %>%
      dplyr::summarise(
        Numerator = sum(.data[[countCol]]),
        Denominator = sum(.data[[exposureCol]])
      ) %>%
      dplyr::mutate(Metric = .data$Numerator / .data$Denominator) %>%
      dplyr::ungroup() %>%
      dplyr::select(GroupID, Numerator, Denominator, Metric) %>%
      unique()
  }

  return(dfTransformed)
}

# Poisson ---------------------------------------------------------------------
qualification_analyze_poisson <- function(dfTransformed) {
  dfTransformed$LogExposure <- log(dfTransformed$Denominator)

  model <- stats::glm(
    Numerator ~ stats::offset(LogExposure),
    family = stats::poisson(link = "log"),
    data = dfTransformed
  )

  outputDF <- dfTransformed %>%
    dplyr::mutate(
      Score = unname(stats::residuals(model)),
      PredictedCount = unname(model$fitted.values)
    ) %>%
    dplyr::arrange(Score) %>%
    dplyr::select(GroupID, Numerator, Denominator, Metric, Score, PredictedCount)

  return(outputDF)
}

qualification_flag_poisson <- function(
  dfAnalyzed,
  threshold = c(-7, -5, 5, 7)
) {
  dfAnalyzed %>%
    dplyr::mutate(
      Flag = dplyr::case_when(
        Score < threshold[1] ~ -2,
        threshold[1] <= Score & Score < threshold[2] ~ -1,
        threshold[4] >= Score & Score > threshold[3] ~ 1,
        Score > threshold[4] ~ 2,
        is.na(Score) | is.nan(Score) ~ NA_real_,
        TRUE ~ 0
      ),
    ) %>%
    dplyr::arrange(match(Flag, c(2, -2, 1, -1, 0)))
}

# Normal ----------------------------------------------------------------------
qualification_analyze_normalapprox <- function(dfTransformed, strType) {
  if (strType == "binary") {
    scored <- dfTransformed %>%
      dplyr::mutate(
        OverallMetric = sum(Numerator) / sum(Denominator),
        z_0 = ifelse(
          OverallMetric == 0 | OverallMetric == 1,
          0,
          ((Metric - OverallMetric) /
            sqrt(OverallMetric * (1 - OverallMetric) / Denominator))
        ),
        Factor = mean(z_0^2),
        Score = ifelse(
          OverallMetric == 0 | OverallMetric == 1 | Factor == 0,
          0,
          ((Metric - OverallMetric) /
            sqrt(Factor * OverallMetric * (1 - OverallMetric) / Denominator))
        )
      )
  } else if (strType == "rate") {
    scored <- dfTransformed %>%
      dplyr::mutate(
        OverallMetric = sum(Numerator) / sum(Denominator),
        z_0 = ifelse(
          OverallMetric == 0,
          0,
          ((Metric - OverallMetric) /
            sqrt(OverallMetric / Denominator))
        ),
        Factor = mean(z_0^2),
        Score = ifelse(
          OverallMetric == 0 | Factor == 0,
          0,
          ((Metric - OverallMetric) /
            sqrt(Factor * OverallMetric / Denominator))
        )
      )
  }

  outputDF <- scored %>%
    dplyr::select(
      "GroupID",
      "Numerator",
      "Denominator",
      "Metric",
      "OverallMetric",
      "Factor",
      "Score"
    ) %>%
    dplyr::arrange(Score)

  return(outputDF)
}

qualification_flag_normalapprox <- function(
  dfAnalyzed,
  threshold = c(-3, -2, 2, 3)
) {
  dfAnalyzed %>%
    dplyr::mutate(
      Flag = dplyr::case_when(
        Score < threshold[1] ~ -2,
        threshold[1] <= Score & Score < threshold[2] ~ -1,
        threshold[2] <= Score & Score < threshold[3] ~ 0,
        threshold[3] <= Score & Score < threshold[4] ~ 1,
        threshold[4] <= Score ~ 2
      ),
    ) %>%
    dplyr::arrange(match(Flag, c(2, -2, 1, -1, 0)))
}

# Summarize -------------------------------------------------------------------
# Groups whose accrual metric falls below `nAccrualThreshold` are not flagged.
qual_summarize <- function(
  dfFlagged,
  nAccrualThreshold = 0,
  strAccrualMetric = "Denominator"
) {
  dfFlagged %>%
    dplyr::mutate(
      Flag = dplyr::if_else(
        .data[[strAccrualMetric]] < nAccrualThreshold,
        NA_real_,
        Flag
      )
    ) %>%
    dplyr::select(GroupID, Numerator, Denominator, Metric, Score, Flag) %>%
    dplyr::arrange(match(Flag, c(2, -2, 1, -1, 0)), GroupID)
}

# Register step functions so RunStep() can resolve them by name ---------------
for (fn_name in c(
  "qual_parse_threshold",
  "qual_input_rate",
  "qualification_transform_counts",
  "qualification_analyze_poisson",
  "qualification_flag_poisson",
  "qualification_analyze_normalapprox",
  "qualification_flag_normalapprox",
  "qual_summarize"
)) {
  assign(fn_name, get(fn_name), envir = .GlobalEnv)
}
rm(fn_name)
