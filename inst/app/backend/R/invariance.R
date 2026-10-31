# Measurement invariance of the CB-SEM measurement model across groups (lavaan multiple-group CFA).
# Nested models: configural (same structure), metric (equal loadings), scalar (equal loadings and intercepts, or
# thresholds for ordinal indicators) and optionally strict (also equal residual variances). Each step is compared with
# the previous one by lavaan::lavTestLRT (scaled difference test for robust estimators) and by the change in CFI,
# RMSEA and SRMR (Chen, 2007; Cheung & Rensvold, 2002). Every number comes from lavaan.

normalize_invariance <- function(raw) {
  raw <- raw %||% list()
  g <- as.character(raw[["groupVariable"]] %||% "")[1]
  if (!nzchar(g)) api_error(400, "inv_group", "Choose the grouping variable.")
  list(groupVariable = g, groups = unique(as_chr_vec(raw[["groups"]])), strict = isTRUE(raw[["strict"]]))
}

run_invariance <- function(df, model, cb, inv, missing_code = NULL) {
  issues <- validate_cbsem(model, df)
  if (any(vapply(issues, function(i) i$severity == "error", TRUE))) api_error(422, "invalid_model", "Fix the CB-SEM model first.", details = issues)
  g <- inv$groupVariable
  if (!g %in% names(df)) api_error(422, "inv_group", sprintf("The grouping variable '%s' is not in the dataset.", g))
  t0 <- Sys.time()
  mx <- cb_syntax(model, structural = FALSE)
  inds <- names(mx$varNames)
  if (g %in% inds) api_error(422, "inv_group", "The grouping variable cannot also be an indicator.")
  d <- df[, inds, drop = FALSE]
  if (!is.null(missing_code)) for (cn in inds) { hit <- !is.na(d[[cn]]) & d[[cn]] == missing_code; d[[cn]][hit] <- NA }
  names(d) <- unname(mx$varNames[inds])
  grp <- as.character(df[[g]])
  keep <- !is.na(grp) & nzchar(grp)
  if (length(inv$groups)) keep <- keep & grp %in% inv$groups
  d <- d[keep, , drop = FALSE]; grp <- grp[keep]
  if (cb$missing == "listwise") { cc <- stats::complete.cases(d); d <- d[cc, , drop = FALSE]; grp <- grp[cc] }
  sizes <- table(grp)
  if (length(sizes) < 2) api_error(422, "inv_groups", "At least two groups with data are needed.")
  if (any(sizes < 30)) api_error(422, "inv_small", sprintf("Every group needs at least 30 cases (smallest: %s with %d).", names(sizes)[which.min(sizes)], min(sizes)),
                                 "Combine small groups or choose another grouping variable.")
  d$.group <- grp
  steps <- list(configural = character(0), metric = "loadings",
                scalar = if (cb$ordinal) c("loadings", "thresholds") else c("loadings", "intercepts"))
  if (inv$strict) steps$strict <- c(steps$scalar, "residuals")
  warn <- character(0)
  fits <- lapply(names(steps), function(s) {
    args <- list(model = mx$syntax, data = d, group = ".group", estimator = cb$estimator, missing = if (cb$missing == "fiml") "fiml" else "listwise")
    if (length(steps[[s]])) args$group.equal <- steps[[s]]
    if (cb$ordinal) { args$ordered <- TRUE; args$parameterization <- "theta" }
    withCallingHandlers(
      tryCatch(do.call(lavaan::cfa, args), error = function(e) api_error(422, "inv_failed", sprintf("The %s model could not be estimated: %s", s, sanitize_message(conditionMessage(e))))),
      warning = function(w) { warn <<- c(warn, sprintf("%s model: %s", s, sanitize_message(conditionMessage(w)))); invokeRestart("muffleWarning") })
  })
  names(fits) <- names(steps)
  robust <- cb$estimator != "ML"
  fm <- function(f) {
    k <- if (robust) c(chisq = "chisq.scaled", df = "df.scaled", cfi = "cfi.robust", tli = "tli.robust", rmsea = "rmsea.robust", srmr = "srmr")
         else c(chisq = "chisq", df = "df", cfi = "cfi", tli = "tli", rmsea = "rmsea", srmr = "srmr")
    v <- suppressWarnings(lavaan::fitMeasures(f, unname(k)))
    setNames(as.list(as.numeric(v)), names(k))
  }
  rows <- list()
  for (i in seq_along(fits)) {
    s <- names(fits)[i]
    f <- fm(fits[[i]])
    row <- c(list(model = s, constraints = as.list(steps[[s]])), f)
    if (i > 1) {
      prev <- fm(fits[[i - 1]])
      lrt <- tryCatch(suppressWarnings(lavaan::lavTestLRT(fits[[i - 1]], fits[[i]])), error = function(e) NULL)
      if (!is.null(lrt)) {
        row$deltaChisq <- unname(lrt[2, "Chisq diff"]); row$deltaDf <- unname(lrt[2, "Df diff"]); row$p <- unname(lrt[2, "Pr(>Chisq)"])
      }
      row$deltaCfi <- f$cfi - prev$cfi; row$deltaRmsea <- f$rmsea - prev$rmsea; row$deltaSrmr <- f$srmr - prev$srmr
      # Chen (2007): a drop in CFI of 0.010 or more together with an increase in RMSEA of 0.015 or more indicates non-invariance.
      row$supported <- !(row$deltaCfi <= -0.010 && row$deltaRmsea >= 0.015)
      row$cfiCriterion <- row$deltaCfi > -0.010
    }
    rows[[i]] <- row
  }
  highest <- "configural"
  for (r in rows[-1]) { if (isTRUE(r$supported)) highest <- r$model else break }
  list(
    options = inv,
    groupVariable = g,
    groups = lapply(names(sizes), function(k) list(group = k, n = as.integer(sizes[[k]]))),
    estimator = cb$estimator,
    estimatorLabel = unname(CB_ESTIMATORS[cb$estimator]),
    scaled = robust,
    models = rows,
    highestSupported = highest,
    warnings = as.list(unique(warn)),
    rCode = invariance_code(mx, cb, inds, g, inv, steps, missing_code),
    environment = list(lavaan = as.character(utils::packageVersion("lavaan"))),
    durationSeconds = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
}

invariance_code <- function(mx, cb, inds, g, inv, steps, missing_code) {
  q <- function(x) paste0("\"", gsub("\"", "\\\\\"", x), "\"")
  args <- paste0("group = \"group\", estimator = \"", cb$estimator, "\", missing = \"", if (cb$missing == "fiml") "fiml" else "listwise", "\"",
                 if (cb$ordinal) ", ordered = TRUE, parameterization = \"theta\"" else "")
  lines <- c("# Measurement invariance generated by PathLab SEM (lavaan)", "library(lavaan)",
             "# data <- read.csv(\"your_data.csv\")",
             paste0("d <- data.frame(\n", paste(sprintf("  %s = data[[%s]]", unname(mx$varNames), vapply(inds, q, "")), collapse = ",\n"), ",\n  group = as.character(data[[", q(g), "]])\n)"),
             if (!is.null(missing_code)) paste0("d[d == ", missing_code, "] <- NA") else NULL,
             "d <- d[!is.na(d$group) & d$group != \"\", ]",
             if (length(inv$groups)) paste0("d <- d[d$group %in% c(", paste(vapply(inv$groups, q, ""), collapse = ", "), "), ]") else NULL,
             if (cb$missing == "listwise") "d <- d[complete.cases(d), ]" else NULL,
             paste0("model <- '\n", mx$syntax, "\n'"))
  for (s in names(steps)) lines <- c(lines, paste0(s, " <- cfa(model, data = d, ", args,
                                                   if (length(steps[[s]])) paste0(", group.equal = c(", paste(vapply(steps[[s]], q, ""), collapse = ", "), ")") else "", ")"))
  lines <- c(lines, paste0("lavTestLRT(", paste(names(steps), collapse = ", "), ")"),
             paste0("sapply(list(", paste(sprintf("%s = %s", names(steps), names(steps)), collapse = ", "), "), fitMeasures, c(",
                    if (cb$estimator != "ML") "\"cfi.robust\", \"rmsea.robust\"" else "\"cfi\", \"rmsea\"", ", \"srmr\"))"))
  paste(lines, collapse = "\n")
}

# ---- report, workbook, APA table ------------------------------------------------------------------

inv_body_html <- function(res) {
  esc <- html_escape
  S <- character(0); a <- function(...) S <<- c(S, paste0(...))
  a("<h2>Measurement invariance</h2>")
  a("<p>Multiple-group CFA across <strong>", esc(res$groupVariable), "</strong> (", esc(paste(vapply(res$groups, function(x) sprintf("%s: n = %d", x$group, x$n), ""), collapse = "; ")),
    "), ", esc(res$estimatorLabel), ". Each model is compared with the previous one. Invariance is supported up to the <strong>", esc(res$highestSupported), "</strong> level ",
    "(Chen, 2007: &Delta;CFI &le; &minus;0.010 together with &Delta;RMSEA &ge; 0.015 indicates non-invariance).</p>")
  for (w in ul(res$warnings)) a("<div class=\"warn\">", esc(w), "</div>")
  rows <- lapply(res$models, function(m) c(esc(m$model), fmt(m$chisq, 2), fmt(m$df, 0), fmt(m$cfi), fmt(m$rmsea), fmt(m$srmr),
                                           if (is.null(m$deltaChisq)) "&ndash;" else paste0(fmt(m$deltaChisq, 2), " (", fmt(m$deltaDf, 0), ")"), fmt_p(m$p),
                                           if (is.null(m$deltaCfi)) "&ndash;" else flag(fmt(m$deltaCfi), m$deltaCfi <= -0.010),
                                           if (is.null(m$deltaRmsea)) "&ndash;" else flag(fmt(m$deltaRmsea), m$deltaRmsea >= 0.015)))
  a(html_table(c("Model", "&chi;&sup2;", "df", "CFI", "RMSEA", "SRMR", "&Delta;&chi;&sup2; (&Delta;df)", "p", "&Delta;CFI", "&Delta;RMSEA"), rows,
               if (isTRUE(res$scaled)) "Invariance tests (scaled &chi;&sup2; and robust indices)" else "Invariance tests", num_cols = 2:10))
  a("<h3>R code</h3><pre><code>", esc(res$rCode %||% ""), "</code></pre>")
  paste(S, collapse = "\n")
}

inv_sheets <- function(res, project_name) {
  m <- do.call(rbind, lapply(res$models, function(x) data.frame(Model = x$model, ChiSquare = num_of(x$chisq), df = num_of(x$df), CFI = num_of(x$cfi), TLI = num_of(x$tli),
    RMSEA = num_of(x$rmsea), SRMR = num_of(x$srmr), DeltaChiSquare = num_of(x$deltaChisq), DeltaDf = num_of(x$deltaDf), p = num_of(x$p),
    DeltaCFI = num_of(x$deltaCfi), DeltaRMSEA = num_of(x$deltaRmsea), DeltaSRMR = num_of(x$deltaSrmr), stringsAsFactors = FALSE)))
  list(Info = data.frame(Item = c("Project", "Completed (UTC)", "PathLab SEM version", "lavaan", "Grouping variable", "Estimator", "Highest level supported"),
                         Value = c(project_name, res$completedAt, PATHLAB_VERSION, res$environment$lavaan, res$groupVariable, res$estimator, res$highestSupported), stringsAsFactors = FALSE),
       Groups = data.frame(Group = vapply(res$groups, `[[`, "", "group"), n = vapply(res$groups, function(x) x$n, 0L)),
       Models = m)
}

inv_pub_spec <- function(res, opts) {
  d <- opts$decimals; lz <- opts$leadingZero
  rows <- lapply(res$models, function(m) c(tools::toTitleCase(m$model), pt_num(m$chisq, 2), as.character(round(num_of(m$df))), pt_num(m$cfi, d, TRUE, lz), pt_num(m$rmsea, d, TRUE, lz),
                                           pt_num(m$srmr, d, TRUE, lz), if (is.null(m$deltaChisq)) "—" else sprintf("%s (%d)", pt_num(m$deltaChisq, 2), as.integer(round(m$deltaDf))),
                                           if (is.null(m$p)) "—" else pt_p(m$p, lz), if (is.null(m$deltaCfi)) "—" else pt_num(m$deltaCfi, d), if (is.null(m$deltaRmsea)) "—" else pt_num(m$deltaRmsea, d)))
  t <- pt_table("invariance", sprintf("Measurement Invariance Across %s", res$groupVariable),
                c("Model", "χ²", "df", "CFI", "RMSEA", "SRMR", "Δχ² (Δdf)", "p", "ΔCFI", "ΔRMSEA"), rows,
                notes = list(sprintf("%s. Groups: %s. Each model is compared with the previous one%s. Non-invariance: ΔCFI ≤ −.010 with ΔRMSEA ≥ .015 (Chen, 2007).",
                                     res$estimatorLabel, paste(vapply(res$groups, function(x) sprintf("%s (n = %d)", x$group, x$n), ""), collapse = ", "),
                                     if (isTRUE(res$scaled)) " using scaled χ² differences and robust fit indices" else "")))
  t$number <- 1
  list(t)
}
