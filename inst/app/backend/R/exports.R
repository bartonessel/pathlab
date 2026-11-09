# Exports: XLSX workbook (full precision) and self-contained HTML report.

PATHLAB_VERSION <- "1.2.10"
# Release code names, one per major version: 1.0.0 up to 2.0.0 is "Gauss"; 0.x has none (as in frontend/src/lib/version.ts).
PATHLAB_CODENAMES <- c(`1` = "Gauss")
PATHLAB_CODENAME <- unname(PATHLAB_CODENAMES[sub("[.].*$", "", PATHLAB_VERSION)])
#' Version with its code name for report headers and footers ("1.0.0 Gauss"); citations keep the plain version.
PATHLAB_RELEASE <- if (is.na(PATHLAB_CODENAME)) PATHLAB_VERSION else paste(PATHLAB_VERSION, PATHLAB_CODENAME)
#' The citation of PathLab SEM itself (developed by Harry Barton Essel), listed in every reference list of the app.
#' `fmt`: "text" marks the title with _underscores_ (shown in italics in the results), "html" uses <i>.
pathlab_citation <- function(fmt = c("text", "html")) {
  fmt <- match.arg(fmt); yr <- format(Sys.Date(), "%Y")
  title <- if (fmt == "html") "<i>PathLab SEM</i>" else "_PathLab SEM_"
  sprintf("Essel, H. B. (%s). %s (Version %s) [Computer software].", yr, title, PATHLAB_VERSION)
}
pathlab_in_text <- function() paste0("Essel, ", format(Sys.Date(), "%Y"))

MODE_LABELS <- c(reflective = "Reflective (common factor, PLSc)",
                 composite_a = "Composite, Mode A (correlation weights)",
                 composite_b = "Composite, Mode B (regression weights)")
# Safe lookup: interaction terms (moderation) and unknown modes must not break the report or exports.
mode_label <- function(mode) {
  mode <- as.character(mode %||% "composite_a")[1]
  if (identical(mode, "interaction")) return("Interaction term (moderation)")
  if (mode %in% names(MODE_LABELS)) unname(MODE_LABELS[[mode]]) else mode
}

rows_to_df <- function(rows, keys, labels = keys) {
  if (length(rows) == 0) {
    df <- as.data.frame(setNames(replicate(length(keys), character(0), simplify = FALSE), labels), check.names = FALSE)
    return(df)
  }
  cols <- lapply(keys, function(k) {
    vals <- lapply(rows, function(r) {
      v <- r[[k]]
      if (is.null(v)) NA else if (is.list(v)) paste(unlist(v), collapse = " -> ") else v
    })
    if (all(vapply(vals, function(v) is.na(v) || is.numeric(v), logical(1)))) as.numeric(unlist(vals)) else vapply(vals, function(v) if (is.na(v)) NA_character_ else as.character(v), "")
  })
  df <- as.data.frame(setNames(cols, labels), check.names = FALSE, stringsAsFactors = FALSE)
  df
}

table_to_df <- function(tab) {
  keys <- vapply(tab$columns, function(c) c$key, "")
  labels <- vapply(tab$columns, function(c) if (nzchar(c$label %||% "")) c$label else c$key, "")
  rows_to_df(tab$rows, keys, labels)
}

BOOT_KEYS <- c("original", "bootMean", "bootSD", "t", "lower", "upper", "p")
boot_labels <- function(conf) {
  lo <- format(round((1 - conf) / 2 * 100, 2)); hi <- format(round((1 - (1 - conf) / 2) * 100, 2))
  c("Original estimate", "Bootstrap mean", "Bootstrap SD", "t statistic", paste0(lo, "% CI"), paste0(hi, "% CI"), "p value")
}

through_str <- function(r) if (is.null(r$through)) "" else paste(unlist(r$through), collapse = " -> ")

result_sheets <- function(res, project_name) {
  st <- res$settings
  info <- data.frame(
    Item = c("Project", "Analysis completed (UTC)", "PathLab SEM version", "R version", "seminr version",
             "Dataset file", "Dataset SHA-256", "Rows in dataset", "Rows used", "Rows removed (listwise)",
             "Cells treated as missing via code", "Cells imputed (mean replacement)",
             "Missing-data treatment", "Missing-value code", "Algorithm", "Inner weighting scheme", "Maximum iterations",
             "Stop criterion (10^-x)", "Iterations used",
             "Bootstrap", "Bootstrap samples requested", "Bootstrap samples successful", "Bootstrap seed",
             "Bootstrap cores", "Confidence level", "PLSpredict"),
    Value = c(project_name, res$completedAt, PATHLAB_VERSION, res$environment$rVersion, res$environment$seminrVersion,
              res$data$filename, res$data$checksum, res$data$nInput, res$data$nUsed, res$data$rowsRemoved,
              res$data$cellsRecoded, res$data$cellsImputed,
              st$missing$treatment, st$missing$code %||% "none", if (identical(st$algorithm, "plsc")) "Consistent PLS-SEM (PLSc)" else "PLS-SEM", st$innerWeights, st$maxIt, st$stopCriterion,
              res$estimation$iterations,
              if (is.null(res$bootstrap)) "not run" else "run",
              res$bootstrap$requested %||% "", res$bootstrap$successful %||% "", res$bootstrap$seed %||% "",
              res$bootstrap$cores %||% "", res$bootstrap$confidenceLevel %||% "",
              if (is.null(res$predict)) "not run" else sprintf("%s, %s folds, %s reps, seed %s", res$predict$technique, res$predict$noFolds, res$predict$reps, res$predict$seed %||% "none")),
    stringsAsFactors = FALSE)
  info$Value <- as.character(info$Value)
  sheets <- list(
    "Info" = info,
    "Outer model" = rows_to_df(res$measurement$outer, c("construct", "indicator", "mode", "loading", "weight", "vif"),
                               c("Construct", "Indicator", "Measurement", "Loading", "Weight", "VIF")),
    "Reliability" = rows_to_df(res$measurement$reliability, c("construct", "mode", "alpha", "rhoA", "rhoC", "AVE"),
                               c("Construct", "Measurement", "Cronbach's alpha", "rho_A", "rho_C", "AVE")),
    "HTMT" = table_to_df(res$measurement$htmt),
    "Fornell-Larcker" = table_to_df(res$measurement$fornellLarcker),
    "Cross-loadings" = table_to_df(res$measurement$crossLoadings),
    "Congruence Index" = if (!is.null(res$measurement$congruence)) table_to_df(res$measurement$congruence) else data.frame(),
    "Model fit" = fit_df(res$measurement$fit),
    "Paths" = rows_to_df(res$structural$paths, c("from", "to", "coefficient", "fSquare", "vif"),
                         c("From", "To", "Path coefficient", "f^2", "VIF")),
    "R squared" = rows_to_df(res$structural$rSquared, c("construct", "rSquared", "adjRSquared"), c("Construct", "R^2", "Adjusted R^2")),
    "Total effects" = rows_to_df(res$structural$totalEffects, c("from", "to", "effect"), c("From", "To", "Total effect")),
    "Total indirect effects" = rows_to_df(res$structural$totalIndirectEffects, c("from", "to", "effect"), c("From", "To", "Total indirect effect")),
    "AIC BIC" = rows_to_df(res$structural$itCriteria, c("construct", "AIC", "BIC"), c("Construct", "AIC", "BIC")),
    "Descriptives" = rows_to_df(res$descriptives$items, c("indicator", "missing", "mean", "median", "min", "max", "sd", "kurtosis", "skewness"),
                                c("Indicator", "Missing", "Mean", "Median", "Min", "Max", "SD", "Excess kurtosis", "Skewness"))
  )
  if (!is.null(res$bootstrap)) {
    b <- res$bootstrap
    bl <- boot_labels(b$confidenceLevel)
    bs <- function(rows, from_lab = "From", to_lab = "To") rows_to_df(rows, c("from", "to", BOOT_KEYS), c(from_lab, to_lab, bl))
    sheets[["Boot paths"]] <- bs(b$paths)
    sheets[["Boot loadings"]] <- bs(b$loadings, "Indicator", "Construct")
    sheets[["Boot weights"]] <- bs(b$weights, "Indicator", "Construct")
    sheets[["Boot HTMT"]] <- bs(b$htmt, "Construct A", "Construct B")
    sheets[["Boot total effects"]] <- bs(b$totalEffects)
    sheets[["Boot total indirect"]] <- bs(b$totalIndirectEffects)
    if (length(b$specificIndirectEffects)) {
      si <- b$specificIndirectEffects
      for (i in seq_along(si)) si[[i]]$throughStr <- through_str(si[[i]])
      sheets[["Specific indirect"]] <- rows_to_df(si, c("from", "throughStr", "to", BOOT_KEYS), c("From", "Through", "To", bl))
    }
  }
  if (!is.null(res$predict)) {
    sheets[["PLSpredict"]] <- rows_to_df(res$predict$items, c("indicator", "plsRMSE", "lmRMSE", "diffRMSE", "plsMAE", "lmMAE", "diffMAE"),
                                         c("Indicator", "PLS RMSE", "LM RMSE", "RMSE (PLS - LM)", "PLS MAE", "LM MAE", "MAE (PLS - LM)"))
    if (length(res$predict$q2)) sheets[["Q2predict"]] <- rows_to_df(res$predict$q2, c("indicator", "q2pls", "q2lm"), c("Indicator", "Q2predict (PLS-SEM)", "Q2predict (LM)"))
    if (length(res$predict$cvpat)) sheets[["CVPAT"]] <- rows_to_df(res$predict$cvpat, c("construct", "benchmark", "lossPls", "lossBench", "diff", "t", "df", "p"),
                                                                  c("Construct", "Benchmark", "PLS-SEM loss", "Benchmark loss", "Difference", "t", "df", "p (one-sided)"))
  }
  # Further PLS-SEM assessment (each present only when chosen).
  ft <- res$measurement$fitTest
  if (isTRUE(ft$available)) sheets[["Fit test"]] <- rbind(data.frame(Model = "Saturated", rows_to_df(ft$saturated, c("measure", "value", "hi95", "hi99", "p"), c("Measure", "Value", "HI95", "HI99", "p")), check.names = FALSE),
                                                        data.frame(Model = "Estimated", rows_to_df(ft$estimated, c("measure", "value", "hi95", "hi99", "p"), c("Measure", "Value", "HI95", "HI99", "p")), check.names = FALSE))
  if (length(res$measurement$cta$constructs)) sheets[["CTA-PLS"]] <- do.call(rbind, lapply(res$measurement$cta$constructs, function(k)
    data.frame(Construct = k$construct, rows_to_df(k$rows, c("tetrad", "value", "lower", "upper", "zero"), c("Tetrad", "Value", "Lower", "Upper", "Contains 0")), Verdict = k$verdict, check.names = FALSE)))
  if (length(res$structural$endogeneity$rows)) sheets[["Endogeneity"]] <- rows_to_df(res$structural$endogeneity$rows, c("outcome", "predictor", "normalityP", "copula", "se", "p", "note"),
                                                                                  c("Endogenous construct", "Predictor", "Normality p (CvM)", "Copula coefficient", "Bootstrap SE", "p", "Note"))
  if (length(res$moderation$slopes)) sheets[["Simple slopes"]] <- rows_to_df(res$moderation$slopes, c("interaction", "outcome", "level", "slope", "lower", "upper"), c("Interaction", "Outcome", "Moderator level (SD)", "Slope", "Lower", "Upper"))
  if (length(res$moderation$moderatedMediation)) sheets[["Moderated mediation"]] <- rows_to_df(res$moderation$moderatedMediation, c("chain", "stage", "moderator", "index", "lower", "upper", "p"),
                                                                                             c("Indirect effect", "Stage", "Moderator", "Index", "Lower", "Upper", "p"))
  if (length(res$mga$pairs)) sheets[["MGA pairs"]] <- rows_to_df(res$mga$pairs, c("group1", "group2", "n1", "n2", "from", "to", "b1", "b2", "diff", "p", "pParam", "pWelch"),
                                                                c("Group A", "Group B", "n A", "n B", "From", "To", "Beta A", "Beta B", "Difference", "PLS-MGA p", "Parametric p", "Welch p"))
  if (!is.null(res$ipma)) {
    sheets[["IPMA constructs"]] <- rows_to_df(res$ipma$constructs, c("construct", "importance", "performance"),
                                              c(paste("Predictor of", res$ipma$target), "Importance (unstandardised total effect)", "Performance (0-100)"))
    if (!is.null(res$ipma$cipma)) sheets[["cIPMA"]] <- rows_to_df(res$ipma$cipma$constructs, c("construct", "importance", "performance", "effectSize", "p", "necessary", "required", "casesBelow", "pctBelow"),
                                                                 c(paste("Predictor of", res$ipma$target), "Importance", "Performance (0-100)", "NCA effect size d", "p value (permutation)", "Necessary",
                                                                   sprintf("Required level for %s = %s", res$ipma$target, res$ipma$cipma$outcomeLevel), "Cases below", "Cases below (%)"))
    sheets[["IPMA indicators"]] <- rows_to_df(res$ipma$indicators, c("construct", "indicator", "rescaledWeight", "importance", "performance", "min", "max"),
                                              c("Construct", "Indicator", "Rescaled weight", "Importance", "Performance (0-100)", "Scale min", "Scale max"))
  }
  if (!is.null(res$nca)) {
    sheets[["NCA"]] <- rows_to_df(res$nca$results, c("condition", "ceiling", "effectSize", "p", "pAccuracy", "cAccuracy", "fit", "ceilingZone", "scope"),
                                  c(paste("Condition for", res$nca$target), "Ceiling", "Effect size d", "p value (permutation)", "p accuracy", "Ceiling accuracy (%)", "Fit (%)", "Ceiling zone", "Scope"))
    for (b in res$nca$bottlenecks) {
      cols <- unlist(b$columns)
      df <- as.data.frame(setNames(lapply(seq_along(cols), function(j) vapply(b$rows, function(r) r[[j]] %||% NA_character_, "")), cols), check.names = FALSE)
      sheets[[paste("NCA bottleneck", toupper(gsub("_", "-", b$ceiling)))]] <- df
    }
  }
  if (!is.null(res$mga)) {
    g <- res$mga; l1 <- mga_label(g, 1); l2 <- mga_label(g, 2)
    sheets[["MGA"]] <- rows_to_df(g$paths, c("from", "to", "estimate", "group1", "group2", "diff", "group1BootMean", "group2BootMean", "p"),
                                  c("From", "To", "Pooled (both groups)", paste0(l1, " (n=", g$n1, ")"), paste0(l2, " (n=", g$n2, ")"),
                                    paste0("Difference (", l1, " - ", l2, ")"), paste(l1, "bootstrap mean"), paste(l2, "bootstrap mean"),
                                    paste0("PLS-MGA p (one-sided, H1: ", l1, " > ", l2, ")")))
    if (length(g$micom$rows)) sheets[["MICOM"]] <- rows_to_df(g$micom$rows, c("construct", "c", "c5", "pC", "compositional", "meanDiff", "meanLo", "meanHi", "meanEqual", "logVarRatio", "varLo", "varHi", "varEqual", "invariance"),
      c("Construct", "c", "c 5% quantile", "p (c)", "Compositional invariance", "Mean difference", "Mean 2.5%", "Mean 97.5%", "Equal means", "Log variance ratio", "Var 2.5%", "Var 97.5%", "Equal variances", "Measurement invariance"))
  }
  diag <- c(paste("Warning:", unlist(res$diagnostics$warnings)), paste("Message:", unlist(res$diagnostics$messages)))
  diag <- diag[!diag %in% c("Warning:", "Message:")]
  if (!is.null(res$cmb)) {
    cm <- res$cmb
    if (!is.null(cm$fullCollinearity)) sheets[["CMB full VIF"]] <- rows_to_df(cm$fullCollinearity$constructs, c("construct", "vif"), c("Construct", "Full collinearity VIF"))
    h <- cm$harman
    sheets[["CMB Harman"]] <- data.frame(Measure = c("Indicators", "First eigenvalue", "First component share", "Components with eigenvalue > 1", "Single-factor (ML) share"),
                                         Value = c(h$indicators, h$firstEigenvalue %||% NA, h$firstComponentShare %||% NA, h$componentsAbove1, h$singleFactorShare %||% NA))
    if (!is.null(cm$marker)) sheets[["CMB marker"]] <- rows_to_df(cm$marker$pairs, c("a", "b", "original", "pOriginal", "adjusted", "pAdjusted"),
                                                                  c("Construct A", "Construct B", "r", "p (r)", "Marker-adjusted r", "p (adjusted)"))
  }
  sheets[["Diagnostics"]] <- data.frame(Entry = if (length(diag)) diag else "No warnings.", stringsAsFactors = FALSE)
  sheets
}

write_results_xlsx <- function(res, project_name, path) {
  sheets <- switch(result_kind(res), efa = efa_sheets(res, project_name), gsca = gsca_sheets(res, project_name), regression = reg_sheets(res, project_name), fimix = fimix_sheets(res, project_name), lpa = lpa_sheets(res, project_name), qca = qca_sheets(res, project_name), ml = ml_sheets(res, project_name), stats = stats_sheets(res, project_name), invariance = inv_sheets(res, project_name), cbsem = cb_sheets(res, project_name), result_sheets(res, project_name))
  writexl::write_xlsx(sheets, path)
  path
}

# ---- HTML report -----------------------------------------------------------

sanitize_svg <- function(svg) {
  if (is.null(svg) || !nzchar(svg)) return(NULL)
  if (nchar(svg) > 3e6) return(NULL)
  svg <- sub("^\\s*(<\\?xml[^>]*>)?\\s*", "", svg)
  if (!grepl("^<svg[\\s>]", svg, perl = TRUE)) return(NULL)
  svg <- gsub("(?is)<script.*?</script\\s*>", "", svg, perl = TRUE)
  svg <- gsub("(?is)<script[^>]*/>", "", svg, perl = TRUE)
  svg <- gsub("(?is)<foreignObject.*?</foreignObject\\s*>", "", svg, perl = TRUE)
  svg <- gsub("(?i)\\son[a-z]+\\s*=\\s*(\"[^\"]*\"|'[^']*')", "", svg, perl = TRUE)
  svg <- gsub("(?i)(href\\s*=\\s*[\"'])\\s*javascript:[^\"']*", "\\1#", svg, perl = TRUE)
  svg
}

fmt <- function(x, digits = 3) {
  if (is.null(x) || length(x) == 0) return("&ndash;")
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x)) return("&ndash;")
  formatC(x, format = "f", digits = digits)
}
# Threshold flags: a value that does not meet its commonly used cutoff is shown in red in the report.
FLAG <- list(loading = 0.708, reliability = 0.70, ave = 0.50, htmt = 0.85, vif = 5, fullVif = 3.3, srmr = 0.08, nfi = 0.90,
             f2 = 0.02, harman = 0.50)
flag <- function(text, bad) if (isTRUE(bad)) paste0("<span class=\"flag\">", text, "</span>") else text
lt <- function(x, t) { v <- num_of(x); !is.na(v) && v < t }
gt <- function(x, t) { v <- num_of(x); !is.na(v) && v > t }
fmt_p <- function(p) {
  if (is.null(p)) return("&ndash;")
  p <- as.numeric(p)
  if (is.na(p)) return("&ndash;")
  if (p < 0.001) "&lt; 0.001" else formatC(p, format = "f", digits = 3)
}

html_table <- function(headers, rows, caption = NULL, num_cols = integer(0)) {
  th <- paste0("<th scope=\"col\"", ifelse(seq_along(headers) %in% num_cols, " class=\"num\"", ""), ">", headers, "</th>", collapse = "")
  body <- paste(vapply(rows, function(r) {
    paste0("<tr>", paste0("<td", ifelse(seq_along(r) %in% num_cols, " class=\"num\"", ""), ">", r, "</td>", collapse = ""), "</tr>")
  }, ""), collapse = "\n")
  paste0("<div class=\"tbl\"><table>", if (!is.null(caption)) paste0("<caption>", caption, "</caption>") else "",
         "<thead><tr>", th, "</tr></thead><tbody>", body, "</tbody></table></div>")
}

# `bad(row_label, column_key, value, row)` marks cells that miss their threshold.
matrix_html <- function(tab, caption, bad = NULL) {
  headers <- vapply(tab$columns, function(c) html_escape(if (nzchar(c$label %||% "")) c$label else c$key), "")
  keys <- vapply(tab$columns, function(c) c$key, "")
  rows <- lapply(tab$rows, function(r) vapply(seq_along(keys), function(j) {
    v <- r[[keys[j]]]
    if (j == 1) html_escape(v %||% "") else flag(fmt(v), !is.null(bad) && isTRUE(bad(r[[keys[1]]], keys[j], v, r)))
  }, ""))
  html_table(headers, rows, caption, num_cols = 2:length(headers))
}

decision_for <- function(row, rule) {
  if (is.null(row) || is.null(row$p)) return("Not available")
  sig_p <- row$p < rule$alpha
  sig_ci <- !is.null(row$lower) && !is.null(row$upper) && (row$lower > 0 || row$upper < 0)
  ok <- switch(rule$criterion, p = sig_p, ci = sig_ci, both = sig_p && sig_ci, sig_p)
  exp_sign <- rule$expected %||% "any"
  if (ok && exp_sign == "positive" && row$original <= 0) ok <- FALSE
  if (ok && exp_sign == "negative" && row$original >= 0) ok <- FALSE
  if (ok) "Supported" else "Not supported"
}

# Shared style of all HTML reports (PLS-SEM, EFA, regression and combined reports).
REPORT_STYLE <- paste0("<style>",
    "body{font-family:'Source Sans 3','Segoe UI',system-ui,sans-serif;color:#1d2733;max-width:1000px;margin:0 auto;padding:32px 24px 64px;line-height:1.5;background:#fff}",
    "h1{font-size:1.7rem;margin:0 0 4px;color:#14325c}h2{font-size:1.25rem;margin:2.2em 0 .6em;padding-bottom:4px;border-bottom:2px solid #d5dce6;color:#14325c}",
    "h3{font-size:1.02rem;margin:1.6em 0 .4em}.sub{color:#556270;margin:0}p.note{color:#4a5563;font-size:.92rem}",
    ".tbl{overflow-x:auto;margin:.6em 0 1.2em}table{border-collapse:collapse;font-size:.88rem;min-width:50%}",
    "caption{text-align:left;font-weight:600;padding:4px 0;color:#2a3a4d}th,td{padding:5px 10px;border-bottom:1px solid #e3e8ee;text-align:left;vertical-align:top}",
    "th{background:#f2f5f9;font-weight:600}td.num,th.num{text-align:right;font-variant-numeric:tabular-nums}",
    ".flag{color:#b42318;font-weight:600}sup.sym{display:inline-block;width:0;line-height:0;font-size:.8em;white-space:nowrap}",
    ".kv{display:grid;grid-template-columns:max-content 1fr;gap:4px 16px;font-size:.92rem}.kv dt{color:#556270}.kv dd{margin:0;word-break:break-all}",
    ".warn{background:#fff7e6;border-left:4px solid #c27c0e;padding:8px 12px;margin:6px 0;font-size:.92rem}",
    ".err{background:#fdecec;border-left:4px solid #b42318;padding:8px 12px;margin:6px 0}",
    ".info{background:#eef4fb;border-left:4px solid #2f5f98;padding:8px 12px;margin:6px 0;font-size:.92rem}",
    ".diagram{border:1px solid #d5dce6;border-radius:6px;padding:8px;overflow:auto;background:#fff}.diagram svg{max-width:100%;height:auto;display:block;margin:0 auto}",
    "figure.rfig{margin:1.2em 0}figure.rfig figcaption{font-size:.9rem;color:#3b4654;margin-top:6px}.fig-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(300px,1fr));gap:10px}",
    "@media print{figure.rfig{break-inside:avoid}}",
    "pre{background:#f6f8fa;border:1px solid #e3e8ee;padding:12px;overflow:auto;font-size:.8rem;line-height:1.4}",
    "footer{margin-top:3em;font-size:.82rem;color:#6b7785;border-top:1px solid #e3e8ee;padding-top:8px}",
    "@media print{body{padding:0}h2{break-after:avoid}.tbl,table{break-inside:avoid}}",
      "</style>")

build_report_html <- function(res, project_name, ds, opts = list()) {
  kind <- result_kind(res)
  if (kind == "efa") return(report_doc(project_name, paste0("Exploratory factor analysis report &middot; PathLab SEM ", PATHLAB_RELEASE, " &middot; completed ", html_escape(res$completedAt %||% ""), " (UTC)"), efa_body_html(res)))
  if (kind == "cbsem") {
    svg_cb <- sanitize_svg(opts$svg)
    return(report_doc(project_name, paste0("CB-SEM report &middot; PathLab SEM ", PATHLAB_RELEASE, " &middot; completed ", html_escape(res$completedAt %||% ""), " (UTC)"),
                      paste0(if (!is.null(svg_cb)) paste0("<h2>Path diagram</h2><div class=\"diagram\">", svg_cb, "</div>") else "", cb_body_html(res))))
  }
  if (kind == "gsca") {
    svg_g <- sanitize_svg(opts$svg)
    return(report_doc(project_name, paste0("GSCA report &middot; PathLab SEM ", PATHLAB_RELEASE, " &middot; completed ", html_escape(res$completedAt %||% ""), " (UTC)"),
                      paste0(if (!is.null(svg_g)) paste0("<h2>Path diagram</h2><div class=\"diagram\">", svg_g, "</div>") else "", gsca_body_html(res))))
  }
  if (kind == "invariance") return(report_doc(project_name, paste0("Measurement invariance report &middot; PathLab SEM ", PATHLAB_RELEASE, " &middot; completed ", html_escape(res$completedAt %||% ""), " (UTC)"), inv_body_html(res)))
  if (kind == "stats") return(report_doc(project_name, paste0(html_escape(res$label), " &middot; PathLab SEM ", PATHLAB_RELEASE, " &middot; completed ", html_escape(res$completedAt %||% ""), " (UTC)"), stats_body_html(res)))
  if (kind == "ml") return(report_doc(project_name, paste0(if (identical(res$mode, "semann")) "SEM-ANN report" else "Machine learning report", " &middot; PathLab SEM ", PATHLAB_RELEASE, " &middot; completed ", html_escape(res$completedAt %||% ""), " (UTC)"), ml_body_html(res)))
  if (kind == "qca") return(report_doc(project_name, paste0("QCA report &middot; PathLab SEM ", PATHLAB_RELEASE, " &middot; completed ", html_escape(res$completedAt %||% ""), " (UTC)"), qca_body_html(res, opts$qcaFlow)))
  if (kind == "lpa") return(report_doc(project_name, paste0("Latent profile analysis report &middot; PathLab SEM ", PATHLAB_RELEASE, " &middot; completed ", html_escape(res$completedAt %||% ""), " (UTC)"), lpa_body_html(res)))
  if (kind == "fimix") return(report_doc(project_name, paste0("FIMIX-PLS report &middot; PathLab SEM ", PATHLAB_RELEASE, " &middot; completed ", html_escape(res$completedAt %||% ""), " (UTC)"), fimix_body_html(res)))
  if (kind == "regression") return(report_doc(project_name, paste0("Regression report &middot; PathLab SEM ", PATHLAB_RELEASE, " &middot; completed ", html_escape(res$completedAt %||% ""), " (UTC)"), reg_body_html(res)))
  st <- res$settings
  model <- res$model
  svg <- sanitize_svg(opts$svg)
  # Figures drawn by the app for this result (path diagrams with estimates and bootstrap results, NCA scatter plots,
  # IPMA and cIPMA maps); numbered in the order they appear.
  figs <- opts$figures %||% list()
  fig_n <- 0L
  figure <- function(svg_txt, caption, cls = "diagram") {
    s <- sanitize_svg(svg_txt)
    if (is.null(s)) return(invisible(NULL))
    fig_n <<- fig_n + 1L
    a("<figure class=\"rfig\"><div class=\"", cls, "\" role=\"img\" aria-label=\"", esc(gsub("<[^>]+>", "", caption)), "\">", s,
      "</div><figcaption><strong>Figure ", fig_n, ".</strong> ", caption, "</figcaption></figure>")
  }
  rule <- opts$decisionRule
  b <- res$bootstrap
  conf <- b$confidenceLevel %||% st$bootstrap$confidenceLevel
  ci_lab <- if (!is.null(b)) boot_labels(conf)[5:6] else c("Lower CI", "Upper CI")
  esc <- html_escape
  S <- character(0)
  a <- function(...) S <<- c(S, paste0(...))

  a("<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">")
  a("<title>PathLab SEM report - ", esc(project_name), "</title>")
  a(REPORT_STYLE, "</head><body>")
  a("<header><h1>", esc(project_name), "</h1><p class=\"sub\">PLS-SEM analysis report &middot; PathLab SEM ", PATHLAB_RELEASE,
    " &middot; analysis completed ", esc(res$completedAt), " (UTC) &middot; report generated ", esc(now_iso()), "</p></header>")

  a("<div class=\"info\"><strong>Red values</strong> do not meet a commonly used threshold: outer loading &lt; 0.708 (reflective/Mode A); Cronbach's alpha, rho_A or rho_C &lt; 0.70; AVE &lt; 0.50; HTMT &gt; 0.85; Fornell&ndash;Larcker correlation above the square root of the AVE; VIF &gt; 5; f&sup2; &lt; 0.02; p &ge; &alpha; or a confidence interval containing zero; SRMR &gt; 0.08; NFI &lt; 0.90; full collinearity VIF &gt; 3.3 and Harman share &gt; 50% (common method bias); PLSpredict error above the linear benchmark; NCA d &lt; 0.1 or p &ge; 0.05. Thresholds are guidelines, not decisions.</div>")

  # 1 Settings ---------------------------------------------------------------
  a("<h2>1. Analysis settings and software</h2><dl class=\"kv\">")
  kv <- function(k, v) a("<dt>", k, "</dt><dd>", v, "</dd>")
  kv("Statistical engine", paste0("SEMinR ", esc(res$environment$seminrVersion), " on ", esc(res$environment$rVersion)))
  kv("Dataset file", esc(res$data$filename))
  kv("Dataset SHA-256", paste0("<code>", esc(res$data$checksum), "</code>"))
  kv("Rows used / in file", paste0(res$data$nUsed, " / ", res$data$nInput))
  miss_txt <- switch(st$missing$treatment,
                     none = "None needed (no missing values in the model indicators)",
                     mean_replacement = sprintf("Mean replacement by SEMinR (%d cell(s) imputed)", res$data$cellsImputed),
                     listwise = sprintf("Listwise deletion (%d row(s) removed)", res$data$rowsRemoved))
  kv("Missing-data treatment", esc(miss_txt))
  if (!is.null(st$missing$code)) kv("Missing-value code", sprintf("%s treated as missing (%d cell(s))", esc(format(st$missing$code)), res$data$cellsRecoded))
  kv("Algorithm", if (identical(st$algorithm, "plsc")) "Consistent PLS-SEM (PLSc; Dijkstra &amp; Henseler, 2015): Mode A constructs estimated as common factors" else "PLS-SEM (constructs as drawn)")
  kv("Inner weighting scheme", esc(if (st$innerWeights == "path_factorial") "Factorial (path_factorial)" else "Path weighting (path_weighting)"))
  kv("Maximum iterations / stop criterion", paste0(st$maxIt, " / 10<sup>-", st$stopCriterion, "</sup>"))
  kv("Iterations until convergence", res$estimation$iterations)
  if (!is.null(b)) {
    kv("Bootstrap samples", sprintf("%d requested, %d successful", b$requested, b$successful))
    kv("Bootstrap seed / cores", paste0(b$seed, " / ", b$cores))
    kv("Confidence intervals", sprintf("%s%% percentile intervals; p values are two-sided bootstrap proportions", format(conf * 100)))
  } else kv("Bootstrap", "Not run (no inference statistics are reported)")
  if (!is.null(res$predict)) kv("PLSpredict", sprintf("%s, %d folds, %d repetition(s), seed %s", esc(res$predict$technique), res$predict$noFolds, res$predict$reps, res$predict$seed %||% "none"))
  a("</dl>")
  a("<p class=\"note\">SEMinR standardises indicators internally as part of the PLS algorithm. The uploaded data were not otherwise transformed.</p>")

  # 2 Data ---------------------------------------------------------------------
  a("<h2>2. Data summary and warnings</h2>")
  if (!is.null(ds)) {
    a("<p>", esc(ds$meta$filename), ": ", ds$meta$nRows, " rows &times; ", ds$meta$nCols, " columns.</p>")
    for (i in ds$issues) {
      cls <- switch(i$severity, error = "err", warning = "warn", "info")
      cols_txt <- if (length(i$columns)) paste0(" <em>(", esc(paste(unlist(i$columns), collapse = ", ")), ")</em>") else ""
      a("<div class=\"", cls, "\"><strong>", esc(tools::toTitleCase(i$severity)), ":</strong> ", esc(i$message), cols_txt, "</div>")
    }
  } else a("<p class=\"note\">The dataset is no longer loaded in this session; data validation messages are not available.</p>")
  scr <- screening_report_html(figs$screening)
  if (!is.null(scr)) a(scr)
  if (length(res$diagnostics$warnings)) {
    a("<h3>Estimation warnings</h3>")
    for (w in res$diagnostics$warnings) a("<div class=\"warn\">", esc(w), "</div>")
  }

  # 3 Model ------------------------------------------------------------------
  a("<h2>3. Model specification</h2>")
  if (!is.null(svg)) figure(svg, "Path model: constructs, indicators and hypothesised paths.")
  a(html_table(c("Construct", "Measurement", "Indicators"),
               lapply(model$constructs, function(c) c(esc(c$name), esc(mode_label(c$mode)), esc(paste(unlist(c$indicators), collapse = ", ")))),
               "Measurement model"))
  byid <- setNames(model$constructs, vapply(model$constructs, function(c) c$id, ""))
  a(html_table(c("#", "From", "To"), lapply(seq_along(model$paths), function(i) {
    p <- model$paths[[i]]; c(i, esc(byid[[p$from]]$name), esc(byid[[p$to]]$name))
  }), "Structural model (paths)"))
  if (length(model$moderations)) {
    method_lab <- c(two_stage = "Two-stage", product_indicator = "Product indicator", orthogonal = "Orthogonal")
    a(html_table(c("Interaction term", "Moderated path", "Moderator", "Method"), lapply(model$moderations, function(m) {
      c(esc(paste0(byid[[m$iv]]$name, "*", byid[[m$moderator]]$name)), paste0(esc(byid[[m$iv]]$name), " &rarr; ", esc(byid[[m$dv]]$name)),
        esc(byid[[m$moderator]]$name), esc(method_lab[[m$method %||% "two_stage"]] %||% m$method))
    }), "Moderating effects (SEMinR interaction_term)"))
  }

  # 4 Measurement --------------------------------------------------------------
  a("<h2>4. Measurement model</h2>")
  a("<p>Outer <strong>loadings</strong> are the correlations between each indicator and its construct; for reflective and Mode A constructs they indicate how well an indicator represents the construct. Outer <strong>weights</strong> show each indicator's relative contribution to the construct score and are the primary statistic for Mode B (formative) constructs. <strong>VIF</strong> values summarise collinearity among the indicators of a construct; this matters mainly for Mode B constructs.</p>")
  boot_l <- if (!is.null(b)) setNames(b$loadings, vapply(b$loadings, function(r) paste(r$from, r$to, sep = "\r"), "")) else list()
  boot_w <- if (!is.null(b)) setNames(b$weights, vapply(b$weights, function(r) paste(r$from, r$to, sep = "\r"), "")) else list()
  hdr <- c("Construct", "Indicator", "Loading", "Weight", "VIF")
  if (!is.null(b)) hdr <- c(hdr, "Loading p", "Weight p")
  a(html_table(hdr, lapply(res$measurement$outer, function(r) {
    refl_a <- !identical(r$mode, "composite_b")
    row <- c(esc(r$construct), esc(r$indicator), flag(fmt(r$loading), refl_a && lt(r$loading, FLAG$loading)), fmt(r$weight), flag(fmt(r$vif), gt(r$vif, FLAG$vif)))
    if (!is.null(b)) {
      pl <- boot_l[[paste(r$indicator, r$construct, sep = "\r")]]$p
      row <- c(row, flag(fmt_p(pl), refl_a && !lt(pl, b$alpha) && !is.na(num_of(pl))), fmt_p(boot_w[[paste(r$indicator, r$construct, sep = "\r")]]$p))
    }
    row
  }), "Outer loadings, weights and indicator VIF", num_cols = 3:length(hdr)))
  a("<p><strong>Reliability and convergent validity.</strong> Cronbach's alpha, rho_A and rho_C (composite reliability) describe internal consistency; the average variance extracted (AVE) describes how much indicator variance a construct captures. Commonly cited guidelines are reliability of at least 0.70 and AVE of at least 0.50 (Hair et al.). These criteria apply to reflective and Mode A constructs; for Mode B constructs SEMinR sets rho_A to 1 by definition and the other values should not be interpreted as reliability.</p>")
  a(html_table(c("Construct", "Measurement", "Alpha", "rho_A", "rho_C", "AVE"),
               lapply(res$measurement$reliability, function(r) {
                 chk <- !(r$mode %||% "") %in% c("composite_b", "interaction")
                 c(esc(r$construct), esc(mode_label(r$mode)), flag(fmt(r$alpha), chk && lt(r$alpha, FLAG$reliability)), flag(fmt(r$rhoA), chk && lt(r$rhoA, FLAG$reliability)),
                   flag(fmt(r$rhoC), chk && lt(r$rhoC, FLAG$reliability)), flag(fmt(r$AVE), chk && lt(r$AVE, FLAG$ave)))
               }),
               "Construct reliability and validity", num_cols = 3:6))
  a("<p><strong>Discriminant validity.</strong> The heterotrait-monotrait ratio (HTMT) compares correlations between constructs with correlations within constructs; values below 0.85 (or 0.90 for conceptually similar constructs) are commonly taken to support discriminant validity (Henseler et al., 2015). The Fornell-Larcker table shows the square root of the AVE on the diagonal and construct correlations below it.</p>")
  a(matrix_html(res$measurement$htmt, "HTMT", bad = function(rn, ck, v, r) gt(v, FLAG$htmt)))
  if (!is.null(b) && length(b$htmt)) {
    a(html_table(c("Construct A", "Construct B", "HTMT", ci_lab[1], ci_lab[2]),
                 lapply(b$htmt, function(r) c(esc(r$from), esc(r$to), flag(fmt(r$original), gt(r$original, FLAG$htmt)), fmt(r$lower), flag(fmt(r$upper), gt(r$upper, FLAG$htmt)))),
                 "Bootstrapped HTMT confidence intervals", num_cols = 3:5))
  }
  fl <- res$measurement$fornellLarcker
  fl_keys <- vapply(fl$columns, function(c) c$key, "")
  fl_diag <- setNames(vapply(fl$rows, function(r) { k <- r[[fl_keys[1]]]; v <- if (!is.null(k) && k %in% names(r)) num_of(r[[k]]) else NA_real_; v }, 0),
                      vapply(fl$rows, function(r) as.character(r[[fl_keys[1]]] %||% ""), ""))
  # Off-diagonal correlations larger than the square root of the AVE of either construct.
  a(matrix_html(fl, "Fornell-Larcker criterion", bad = function(rn, ck, v, r) {
    rn <- as.character(rn %||% ""); x <- num_of(v)
    if (is.na(x) || identical(rn, ck)) return(FALSE)
    d <- c(fl_diag[rn], fl_diag[ck]); d <- d[!is.na(d)]
    length(d) > 0 && abs(x) > min(d)
  }))
  if (!is.null(res$measurement$congruence)) {
    a("<p><strong>Congruence Index.</strong> Tucker's congruence coefficient between the cross-loading patterns of two constructs (Tucker, 1951). A value of 1 means the two constructs relate to all indicators in the same proportions; values of 0.95 or more suggest the constructs are not empirically distinct, 0.85&ndash;0.94 indicates fair similarity (Lorenzo-Seva &amp; ten Berge, 2006).</p>")
    a(matrix_html(res$measurement$congruence, "Congruence Index (Tucker's congruence coefficient)", bad = function(rn, ck, v, r) {
      x <- num_of(v); !is.na(x) && !identical(as.character(rn %||% ""), ck) && x >= 0.95
    }))
  }
  fit <- res$measurement$fit
  if (isTRUE(fit$available)) {
    a("<p><strong>Model fit.</strong> SRMR, d_ULS and d_G compare the observed indicator correlations with those implied by the model; the chi-square and NFI are based on the maximum-likelihood discrepancy. The <em>saturated model</em> leaves all construct correlations free (it assesses the measurement model); the <em>estimated model</em> uses the correlations implied by the structural model. An SRMR below 0.08 is a commonly cited guideline (Henseler et al., 2014); PLS-SEM fit measures are best treated as supplementary diagnostics.</p>")
    a(html_table(c("Measure", "Saturated model", "Estimated model"), fit_rows(fit), "Model fit", num_cols = 2:3))
  } else if (!is.null(fit)) {
    a("<p class=\"note\">Model fit: ", esc(fit$reason %||% "not available."), "</p>")
  }

  if (!is.null(res$cmb)) a(cmb_html(res$cmb))

  # 5 Structural ---------------------------------------------------------------
  a("<h2>5. Structural model</h2>")
  a("<p><strong>Path coefficients</strong> are standardised estimates of the direct relationships between constructs. <strong>f&sup2;</strong> is the change in R&sup2; when a predictor is omitted (Cohen's guidelines: 0.02 small, 0.15 medium, 0.35 large). <strong>VIF</strong> values here describe collinearity among the predictors of each endogenous construct.",
    if (!is.null(b)) " Bootstrap <strong>t statistics</strong>, <strong>p values</strong> and <strong>confidence intervals</strong> describe the sampling uncertainty of each estimate." else " Bootstrapping was not run, so no significance tests are reported.", "</p>")
  figure(figs$estimates, "PLS-SEM estimates: path coefficients on the structural paths, outer loadings or weights on the measurement links, and R&sup2; of the endogenous constructs.")
  boot_p <- if (!is.null(b)) setNames(b$paths, vapply(b$paths, function(r) paste(r$from, r$to, sep = "\r"), "")) else list()
  use_rule <- !is.null(rule) && isTRUE(rule$enabled) && !is.null(b)
  if (use_rule) {
    crit_txt <- switch(rule$criterion, p = sprintf("p &lt; %s", rule$alpha), ci = sprintf("the %s confidence interval excludes zero", esc(paste(ci_lab, collapse = "-"))),
                       both = sprintf("p &lt; %s and the confidence interval excludes zero", rule$alpha))
    a("<div class=\"info\"><strong>Decision rule (set by the analyst):</strong> a hypothesis is labelled &lsquo;Supported&rsquo; when ", crit_txt,
      ", and, where an expected sign is given, the estimate has that sign. Otherwise it is labelled &lsquo;Not supported&rsquo;. This label summarises the stated rule only; it is not a substitute for substantive interpretation.</div>")
  } else if (!is.null(rule) && isTRUE(rule$enabled) && is.null(b)) {
    a("<div class=\"warn\">A hypothesis decision rule was requested, but bootstrapping was not run, so no decisions are shown.</div>")
  }
  hyp <- rule$hypotheses %||% list()
  hyp_by <- setNames(hyp, vapply(hyp, function(h) h$pathId %||% "", ""))
  hdr <- c("Path", "Coefficient", "f&sup2;", "VIF")
  if (!is.null(b)) hdr <- c(hdr, "t", "p", ci_lab[1], ci_lab[2])
  if (use_rule) hdr <- c("Hypothesis", hdr, "Expected sign", "Decision")
  a(html_table(hdr, lapply(res$structural$paths, function(r) {
    br <- boot_p[[paste(r$from, r$to, sep = "\r")]]
    row <- c(paste0(esc(r$from), " &rarr; ", esc(r$to), if (isTRUE(r$implicit)) " <span class=\"note\">(moderator's direct effect, added automatically)</span>" else ""),
             fmt(r$coefficient), flag(fmt(r$fSquare), lt(r$fSquare, FLAG$f2)), flag(fmt(r$vif), gt(r$vif, FLAG$vif)))
    if (!is.null(b)) {
      ns <- !is.na(num_of(br$p)) && !lt(br$p, b$alpha)
      zero_in <- !is.na(num_of(br$lower)) && !is.na(num_of(br$upper)) && num_of(br$lower) <= 0 && num_of(br$upper) >= 0
      row <- c(row, fmt(br$t), flag(fmt_p(br$p), ns), flag(fmt(br$lower), zero_in), flag(fmt(br$upper), zero_in))
    }
    if (use_rule) {
      h <- hyp_by[[r$id %||% ""]] %||% list(label = "", expected = "any")
      rr <- rule; rr$expected <- h$expected %||% "any"
      row <- c(esc(h$label %||% ""), row, esc(h$expected %||% "any"), decision_for(br, rr))
    }
    row
  }), "Path coefficients", num_cols = if (use_rule) 3:(length(hdr) - 2) else 2:length(hdr)))
  a("<p><strong>R&sup2;</strong> is the share of variance in an endogenous construct explained by its predictors; adjusted R&sup2; corrects for the number of predictors.</p>")
  a(html_table(c("Construct", "R&sup2;", "Adjusted R&sup2;"), lapply(res$structural$rSquared, function(r) c(esc(r$construct), fmt(r$rSquared), fmt(r$adjRSquared))), "Coefficient of determination", num_cols = 2:3))
  if (length(res$structural$totalIndirectEffects)) {
    a(html_table(c("From", "To", "Total indirect effect"), lapply(res$structural$totalIndirectEffects, function(r) c(esc(r$from), esc(r$to), fmt(r$effect))), "Total indirect effects", num_cols = 3))
  }
  a(html_table(c("From", "To", "Total effect"), lapply(res$structural$totalEffects, function(r) c(esc(r$from), esc(r$to), fmt(r$effect))), "Total effects", num_cols = 3))
  a("<p class=\"note\">AIC and BIC (model selection criteria for each endogenous construct's regression; lower is better when comparing alternative models for the same data):</p>")
  a(html_table(c("Construct", "AIC", "BIC"), lapply(res$structural$itCriteria, function(r) c(esc(r$construct), fmt(r$AIC), fmt(r$BIC))), NULL, num_cols = 2:3))

  # 6 Bootstrap ----------------------------------------------------------------
  if (!is.null(b)) {
    a("<h2>6. Bootstrapping</h2>")
    a("<p>", b$requested, " bootstrap samples were requested and ", b$successful, " succeeded (seed ", b$seed, "). ",
      "Confidence intervals are percentile intervals; t statistics are the original estimate divided by the bootstrap standard deviation; p values are two-sided proportions of bootstrap estimates on either side of zero, as computed by SEMinR.</p>")
    if (b$failed > 0) a("<div class=\"warn\">", b$failed, " bootstrap sample(s) failed and were excluded by SEMinR.</div>")
    figure(figs$bootstrap, paste0("Bootstrapping results (", b$successful, " samples): significance of the path coefficients and of the outer loadings or weights; non-significant paths are dashed."))
    if (length(b$specificIndirectEffects)) {
      a(html_table(c("Indirect path", "Estimate", "Boot SD", "t", "p", ci_lab[1], ci_lab[2]),
                   lapply(b$specificIndirectEffects, function(r) c(esc(paste(c(r$from, unlist(r$through), r$to), collapse = " \u2192 ")), fmt(r$original), fmt(r$bootSD), fmt(r$t), flag(fmt_p(r$p), !is.na(num_of(r$p)) && !lt(r$p, b$alpha)), fmt(r$lower), fmt(r$upper))),
                   "Specific indirect effects", num_cols = 2:7))
    }
    if (length(b$totalIndirectEffects)) {
      a(html_table(c("From", "To", "Estimate", "t", "p", ci_lab[1], ci_lab[2]),
                   lapply(b$totalIndirectEffects, function(r) c(esc(r$from), esc(r$to), fmt(r$original), fmt(r$t), flag(fmt_p(r$p), !is.na(num_of(r$p)) && !lt(r$p, b$alpha)), fmt(r$lower), fmt(r$upper))),
                   "Bootstrapped total indirect effects", num_cols = 3:7))
    }
  }

  # 7 Predict ----------------------------------------------------------------
  if (!is.null(res$predict)) {
    a("<h2>7. Out-of-sample prediction (PLSpredict)</h2>")
    a("<p>Root mean squared error (RMSE) and mean absolute error (MAE) of cross-validated predictions for indicators of endogenous constructs, for the PLS-SEM model and for a linear regression benchmark (LM). Negative differences mean the PLS-SEM model predicted that indicator with less error than the benchmark.</p>")
    if (identical(res$predict$method, "two_stage_folds")) a("<p>", predict_hoc_note(res$predict), "</p>")
    a(html_table(c("Indicator", "PLS RMSE", "LM RMSE", "RMSE (PLS &minus; LM)", "PLS MAE", "LM MAE", "MAE (PLS &minus; LM)"),
                 lapply(res$predict$items, function(r) c(esc(r$indicator), fmt(r$plsRMSE), fmt(r$lmRMSE), flag(fmt(r$diffRMSE), gt(r$diffRMSE, 0)), fmt(r$plsMAE), fmt(r$lmMAE), flag(fmt(r$diffMAE), gt(r$diffMAE, 0)))),
                 NULL, num_cols = 2:7))
    if (length(res$predict$q2)) {
      a("<p>Q&sup2;<sub>predict</sub> (Shmueli et al., 2019) compares the PLS-SEM prediction errors with those of the training-sample mean; values above 0 indicate predictive relevance (values in red are 0 or below).</p>")
      a(html_table(c("Indicator", "Q&sup2;predict (PLS-SEM)", "Q&sup2;predict (LM)"), lapply(res$predict$q2, function(r) c(esc(r$indicator), flag(fmt(r$q2pls), !is.null(r$q2pls) && r$q2pls <= 0), fmt(r$q2lm))), NULL, num_cols = 2:3))
    }
    if (length(res$predict$cvpat)) {
      a("<p>The cross-validated predictive ability test (CVPAT; Liengaard et al., 2021; Sharma et al., 2023) compares the average squared prediction error per case of PLS-SEM with the indicator average (IA) and linear model (LM) benchmarks from the same folds. A significant one-sided t test means PLS-SEM predicts better than the benchmark.</p>")
      a(html_table(c("Construct", "Benchmark", "PLS-SEM loss", "Benchmark loss", "Difference", "t", "p (one-sided)"),
                   lapply(res$predict$cvpat, function(r) c(esc(r$construct), esc(r$benchmark), fmt(r$lossPls), fmt(r$lossBench), fmt(r$diff), fmt(r$t), fmt(r$p))), "CVPAT", num_cols = 3:7))
    }
  }

  # Further assessment ----------------------------------------------------------------
  if (isTRUE(res$measurement$fitTest$available)) {
    ft <- res$measurement$fitTest
    a("<h2>Bootstrap test of model fit</h2><p>Dijkstra &amp; Henseler (2015): the data were transformed to fit the model exactly and bootstrapped (", ft$nboot,
      " samples). A value below HI95 means the discrepancy is not significant at 5% (the model fits).</p>")
    a(html_table(c("Model", "Measure", "Value", "HI95", "HI99"), c(lapply(ft$saturated, function(r) c("Saturated", esc(r$measure), fmt(r$value), fmt(r$hi95), fmt(r$hi99))),
                                                                 lapply(ft$estimated, function(r) c("Estimated", esc(r$measure), flag(fmt(r$value), !is.null(r$value) && !is.null(r$hi95) && r$value > r$hi95), fmt(r$hi95), fmt(r$hi99)))), NULL, num_cols = 3:5))
  }
  if (length(res$structural$endogeneity$rows)) {
    a("<h2>Endogeneity (Gaussian copula)</h2><p>Park &amp; Gupta (2012); Hult et al. (2018). A significant copula term indicates endogeneity of that predictor.</p>")
    a(html_table(c("Construct", "Predictor", "Normality p", "Copula", "p"), lapply(res$structural$endogeneity$rows, function(r) c(esc(r$outcome), esc(r$predictor), fmt(r$normalityP), fmt(r$copula), flag(fmt(r$p), !is.null(r$p) && r$p < 0.05))), NULL, num_cols = 3:5))
  }
  if (length(res$moderation$slopes)) {
    a("<h2>Simple slopes</h2><p>Effect of the predictor at the moderator's mean and &plusmn;1 SD (standardised scores).</p>")
    a(html_table(c("Interaction", "Outcome", "Moderator level", "Slope", "Lower", "Upper"), lapply(res$moderation$slopes, function(r) c(esc(r$interaction), esc(r$outcome), if (r$level == 0) "Mean" else sprintf("%+d SD", r$level), fmt(r$slope), fmt(r$lower), fmt(r$upper))), NULL, num_cols = 4:6))
    if (length(res$moderation$moderatedMediation)) a(html_table(c("Indirect effect", "Stage", "Moderator", "Index of moderated mediation", "Lower", "Upper"),
      lapply(res$moderation$moderatedMediation, function(r) c(esc(r$chain), esc(r$stage), esc(r$moderator), fmt(r$index), fmt(r$lower), fmt(r$upper))), "Index of moderated mediation (Hayes, 2015)", num_cols = 4:6))
  }
  if (length(res$measurement$cta$constructs)) {
    a("<h2>Confirmatory tetrad analysis (CTA-PLS)</h2><p>Gudergan et al. (2008): Bonferroni-adjusted bootstrap intervals of the vanishing tetrads.</p>")
    a(html_table(c("Construct", "Indicators", "Tetrads", "Verdict"), lapply(res$measurement$cta$constructs, function(k) c(esc(k$construct), k$indicators, k$tetrads, esc(k$verdict))), NULL, num_cols = 2:3))
  }

  # Advanced analyses -----------------------------------------------------------------
  if (!is.null(res$nca)) {
    nc <- res$nca
    a("<h2>Necessary Condition Analysis (NCA)</h2>")
    a("<p>NCA asks whether a certain level of a condition is <em>necessary</em> for a level of the outcome (", esc(nc$target),
      "). It was run with the NCA R package (version ", esc(nc$ncaVersion), ") on the SEMinR construct scores. The effect size <em>d</em> is the share of the scope ",
      "without observations above the ceiling line; 0.1&ndash;0.3 is commonly called a medium and 0.3&ndash;0.5 a large effect (Dul, 2016). ",
      if (nc$testRep > 0) sprintf("p values come from an approximate permutation test with %d resamples (seed %s).", nc$testRep, nc$seed %||% "none") else "No permutation test was run.",
      " A necessary condition is usually claimed only when d is meaningfully large <em>and</em> the p value is small.</p>")
    a(html_table(c("Condition", "Ceiling", "Effect size d", "p", "p accuracy", "Ceiling accuracy %"),
                 lapply(nc$results, function(r) c(esc(r$condition), toupper(gsub("_", "-", r$ceiling)), flag(fmt(r$effectSize), lt(r$effectSize, 0.1)), flag(fmt_p(r$p), !is.na(num_of(r$p)) && !lt(r$p, 0.05)), fmt(r$pAccuracy), fmt(r$cAccuracy, 1))),
                 "NCA effect sizes", num_cols = 3:6))
    np <- Filter(function(f) !identical(f$level, "indicator"), figs$nca %||% list())
    ni <- Filter(function(f) identical(f$level, "indicator"), figs$nca %||% list())
    nca_grid <- function(fl, caption) {
      ok <- Filter(Negate(is.null), lapply(fl, function(f) sanitize_svg(f$svg)))
      if (!length(ok)) return(invisible(NULL))
      fig_n <<- fig_n + 1L
      a("<figure class=\"rfig\"><div class=\"fig-grid\">", paste0("<div class=\"diagram\">", unlist(ok), "</div>", collapse = ""),
        "</div><figcaption><strong>Figure ", fig_n, ".</strong> ", caption, "</figcaption></figure>")
    }
    nca_grid(np, paste0("NCA scatter plots of ", esc(nc$target), " against each condition, with the CE-FDH and CR-FDH ceiling lines; open circles are the peers that define the ceiling, the shaded area is the empty ceiling zone."))
    nca_grid(ni, paste0("NCA scatter plots of the single indicators of ", esc(paste(unlist(nc$itemConstructs), collapse = ", ")), " against ", esc(nc$target), " (Richter et al., 2020)."))
    for (b in nc$bottlenecks) {
      cols <- unlist(b$columns)
      a(html_table(esc(cols), lapply(b$rows, function(r) vapply(r, function(v) esc(v %||% ""), "")),
                   sprintf("Bottleneck table (%s; values are %% of range; NN = not necessary)", toupper(gsub("_", "-", b$ceiling))), num_cols = seq_along(cols)))
    }
  }
  if (!is.null(res$ipma)) {
    ip <- res$ipma
    a("<h2>Importance-performance map analysis (IPMA)</h2>")
    a("<p>Target construct: <strong>", esc(ip$target), "</strong> (performance ", fmt(ip$targetPerformance, 1), " on a 0&ndash;100 scale). ",
      "<strong>Importance</strong> is the unstandardised total effect of each construct on the target; <strong>performance</strong> is the mean rescaled construct score. ",
      "Indicators were rescaled to 0&ndash;100 using ", if (identical(ip$scale, "theoretical")) sprintf("the theoretical scale %s&ndash;%s", format(ip$scaleMin), format(ip$scaleMax)) else "each indicator's observed minimum and maximum",
      ". Computed from the SEMinR estimates following Ringle &amp; Sarstedt (2016); SEMinR has no IPMA function.</p>")
    if (length(ip$negativeWeights)) a("<div class=\"warn\">Indicators with negative rescaled weights (IPMA assumes positive weights): ", esc(paste(unlist(ip$negativeWeights), collapse = ", ")), "</div>")
    a(html_table(c("Construct", "Importance", "Performance"), lapply(ip$constructs, function(r) c(esc(r$construct), fmt(r$importance), fmt(r$performance, 2))),
                 "Construct-level IPMA", num_cols = 2:3))
    figure(figs$ipma, paste0("Importance-performance map for ", esc(ip$target), ": importance (total effect) against performance (0&ndash;100); dashed lines mark the means."))
    cp <- ip$cipma
    if (!is.null(cp)) {
      a("<h3>Combined importance-performance map analysis (cIPMA)</h3>")
      a("<p>Following Hauff, Richter, Sarstedt &amp; Ringle (2024), the rescaled (0&ndash;100) construct scores of the IPMA were analysed with NCA (", esc(toupper(gsub("_", "-", cp$ceiling))),
        " ceiling, ", cp$testRep, " permutations). A predecessor is a <em>necessary condition</em> when d &gt; 0 and p &lt; .05. For an outcome level of ", fmt(cp$outcomeLevel, 0),
        " on the 0&ndash;100 scale of ", esc(cp$target), ", the bottleneck table gives the level each condition requires; cases below it cannot reach the outcome level. ",
        sprintf("%.1f%% of the cases (%d of %d) fall below the required level of at least one necessary condition.", cp$pctBelowAny, as.integer(cp$casesBelowAny), as.integer(cp$n)), "</p>")
      if (isTRUE(cp$outcomeAboveScope)) a("<div class=\"warn\">The outcome level lies above the highest observed outcome score, so the bottleneck levels are not defined; choose a lower level.</div>")
      figure(figs$cipma, paste0("Combined importance-performance map (cIPMA) for ", esc(cp$target), ": filled bubbles are necessary conditions, sized by the NCA effect size d; labels give the share of cases below the required level (Hauff et al., 2024)."))
      a(html_table(c("Construct", "Importance", "Performance", "NCA d", "p", "Necessary", "Required level", "Cases below (%)"),
                   lapply(cp$constructs, function(r) c(esc(r$construct), fmt(r$importance), fmt(r$performance, 2), fmt(r$effectSize), fmt(r$p), if (isTRUE(r$necessary)) "Yes" else "No",
                                                       if (is.null(r$required)) "NN" else fmt(r$required, 2), sprintf("%.1f", r$pctBelow))),
                   sprintf("cIPMA for %s (outcome level %s)", esc(cp$target), fmt(cp$outcomeLevel, 0)), num_cols = 2:8))
    }
  }
  if (!is.null(res$mga)) {
    g <- res$mga
    a("<h2>Multigroup analysis (PLS-MGA)</h2>")
    a("<p>Grouping variable <strong>", esc(g$groupVariable), "</strong>: group 1 = ", esc(paste(unlist(g$group1), collapse = ", ")), " (n = ", g$n1,
      "), group 2 = ", esc(paste(unlist(g$group2), collapse = ", ")), " (n = ", g$n2, "). ", if (g$nExcluded > 0) sprintf("%d row(s) belonged to neither group and were excluded. ", g$nExcluded) else "",
      "Each group was bootstrapped with ", g$nbootUsed, " samples (seed ", g$seed %||% "none", ") by SEMinR's <code>estimate_pls_mga()</code>. ",
      "The PLS-MGA p value (Henseler et al., 2009) is <em>one-sided</em>: it is the probability that the group 1 coefficient is not larger than the group 2 coefficient. ",
      "Values near 0 suggest group 1 &gt; group 2; values near 1 suggest group 2 &gt; group 1 (for a 5% two-sided test, look for p &lt; 0.025 or p &gt; 0.975). ",
      if (length(g$micom$rows)) "Measurement invariance was assessed first with MICOM (below)." else "Measurement invariance (MICOM) was not tested.", "</p>")
    a(html_table(c("Path", esc(mga_label(g, 1)), esc(mga_label(g, 2)), "Difference", "PLS-MGA p"),
                 lapply(g$paths, function(r) c(paste0(esc(r$from), " &rarr; ", esc(r$to)), fmt(r$group1), fmt(r$group2), fmt(r$diff), fmt(r$p))),
                 "Group-specific path coefficients", num_cols = 2:5))
    if (length(g$micom$rows)) {
      a("<p>MICOM (Henseler, Ringle &amp; Sarstedt, 2016), ", g$micom$nPermUsed, " permutations (seed ", g$micom$seed %||% "none", "). Step 1 (configural invariance) holds by design. ",
        "Step 2: compositional invariance holds when the correlation c of the scores from the two groups' weights is not below its 5% permutation quantile. ",
        "Step 3: equal means and variances hold when the difference lies in the 95% permutation interval. Partial invariance (step 2) allows comparing path coefficients; full invariance also allows pooling the groups.</p>")
      a(html_table(c("Construct", "c", "5% quantile", "Mean difference", "95% interval", "Log variance ratio", "95% interval", "Invariance"),
                   lapply(g$micom$rows, function(r) c(esc(r$construct), flag(fmt(r$c), !isTRUE(r$compositional)), fmt(r$c5), flag(fmt(r$meanDiff), !isTRUE(r$meanEqual)), sprintf("[%s, %s]", fmt(r$meanLo), fmt(r$meanHi)),
                                                      flag(fmt(r$logVarRatio), !isTRUE(r$varEqual)), sprintf("[%s, %s]", fmt(r$varLo), fmt(r$varHi)), esc(r$invariance))),
                   "MICOM", num_cols = 2:7))
    }
  }

  # 8 Reproducibility ----------------------------------------------------------
  a("<h2>Reproducibility</h2>")
  a("<p>The R script below recreates this model and analysis with SEMinR. Steps not reproduced by the script: the canvas layout of the diagram, PathLab's data-validation messages, and the rounding used in this report (values are shown to three decimals; the XLSX/CSV exports keep full precision).</p>")
  a("<pre><code>", esc(res$rCode %||% ""), "</code></pre>")
  if (!is.null(scr) && nzchar(figs$screening$rCode %||% "")) {
    a("<p>Data screening (run before the model; remove the flagged rows you chose before fitting):</p>")
    a("<pre><code>", esc(figs$screening$rCode), "</code></pre>")
  }

  if (isTRUE(opts$includeData) && !is.null(ds)) {
    items <- unique(unlist(lapply(model$constructs, function(c) unlist(c$indicators))))
    items <- intersect(items, names(ds$data))
    d <- ds$data[, items, drop = FALSE]
    shown <- utils::head(d, 5000)
    a("<h2>Appendix: analysis data (included at the analyst's request)</h2>")
    if (nrow(d) > nrow(shown)) a("<p class=\"note\">Only the first 5,000 of ", nrow(d), " rows are shown.</p>")
    a(html_table(esc(names(shown)), lapply(seq_len(nrow(shown)), function(i) vapply(shown[i, , drop = TRUE], function(v) if (is.na(v)) "" else esc(format(v)), "")), NULL, num_cols = seq_along(items)))
  }
  a("<footer>Generated by PathLab SEM ", PATHLAB_RELEASE, ". All statistics were computed by the SEMinR R package (Ray, Danks &amp; Calero Valdez). ",
    "Guideline thresholds mentioned in this report are commonly cited rules of thumb, not strict cut-offs.</footer>")
  a("</body></html>")
  paste(S, collapse = "\n")
}

FIT_MEASURES <- c(SRMR = "SRMR", d_ULS = "d_ULS", d_G = "d_G", chiSquare = "Chi-square", NFI = "NFI")
fit_rows <- function(fit) lapply(names(FIT_MEASURES), function(k) {
  bad <- function(v) (k == "SRMR" && gt(v, FLAG$srmr)) || (k == "NFI" && lt(v, FLAG$nfi))
  c(FIT_MEASURES[[k]], flag(fmt(fit$saturated[[k]]), bad(fit$saturated[[k]])), flag(fmt(fit$estimated[[k]]), bad(fit$estimated[[k]])))
})
fit_df <- function(fit) {
  if (!isTRUE(fit$available)) return(data.frame(Note = fit$reason %||% "Model fit not available.", stringsAsFactors = FALSE))
  num <- function(v) if (is.null(v)) NA_real_ else as.numeric(v)
  data.frame(Measure = unname(FIT_MEASURES),
             "Saturated model" = vapply(names(FIT_MEASURES), function(k) num(fit$saturated[[k]]), 0),
             "Estimated model" = vapply(names(FIT_MEASURES), function(k) num(fit$estimated[[k]]), 0),
             check.names = FALSE, stringsAsFactors = FALSE)
}

cmb_html <- function(cm) {
  esc <- html_escape
  out <- c("<h3>Common method bias</h3>",
           "<p>Three established checks. None proves the absence of common method variance; they are best reported together.</p>")
  if (!is.null(cm$fullCollinearity)) {
    out <- c(out, "<p><strong>Full collinearity VIFs</strong> (Kock, 2015): each construct regressed on all other constructs. Values above 3.3 are commonly taken to indicate possible common method bias.</p>",
             html_table(c("Construct", "Full collinearity VIF"), lapply(cm$fullCollinearity$constructs, function(r) c(esc(r$construct), flag(fmt(r$vif), gt(r$vif, FLAG$fullVif)))), "Full collinearity VIFs", num_cols = 2))
  }
  h <- cm$harman
  pct <- function(v) if (is.null(v)) "&ndash;" else sprintf("%.1f%%", 100 * v)
  out <- c(out, sprintf("<p><strong>Harman's single-factor test</strong> (Podsakoff et al., 2003): the first unrotated principal component explains %s of the variance of the %d indicators (one-factor ML solution: %s); %d component(s) have an eigenvalue above 1. A single factor explaining more than 50%% is the usual warning sign; the test is insensitive and should not be relied on alone.</p>",
                        flag(pct(h$firstComponentShare), gt(h$firstComponentShare, FLAG$harman)), h$indicators, pct(h$singleFactorShare), h$componentsAbove1))
  mk <- cm$marker
  if (!is.null(mk)) {
    out <- c(out, sprintf("<p><strong>Marker variable technique</strong> (Lindell &amp; Whitney, 2001): marker <code>%s</code> (n = %d). r<sub>M</sub> = %s (smallest positive correlation of the marker with a construct%s). Adjusted correlations r<sub>A</sub> = (r &minus; r<sub>M</sub>) / (1 &minus; r<sub>M</sub>).</p>",
                          esc(mk$variable), mk$n, fmt(mk$rM), if (is.null(mk$rMConstruct)) "; none was positive, so no adjustment" else paste0(": ", esc(mk$rMConstruct))),
             html_table(c("Construct A", "Construct B", "r", "p", "Adjusted r", "p (adjusted)"),
                        lapply(mk$pairs, function(r) c(esc(r$a), esc(r$b), fmt(r$original), fmt_p(r$pOriginal), fmt(r$adjusted),
                                              flag(fmt_p(r$pAdjusted), lt(r$pOriginal, 0.05) && !lt(r$pAdjusted, 0.05)))),
                        "Marker-adjusted construct correlations", num_cols = 3:6))
  }
  paste(out, collapse = "\n")
}
