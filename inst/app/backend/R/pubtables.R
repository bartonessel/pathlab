# Publication-ready tables (APA 7 style) rendered with Quarto to Word (.docx, A4) and HTML.
#
# pub_tables_spec() turns a saved analysis result into a list of table specifications (plain
# character cells, so every number is formatted exactly once, from the same values the app shows).
# render_pub_tables() writes the specification, a Quarto document and an A4 reference document into a
# working folder and runs `quarto render`; the Quarto document draws each table with flextable.

PUB_TABLE_IDS <- c("descriptives", "measurement", "formative", "htmt", "fornell", "crossloadings", "structural",
                   "r2", "mediation", "moderation", "fit", "cmb", "marker", "predict", "nca", "ipma", "mga")

normalize_pub_options <- function(raw) {
  raw <- raw %||% list()
  fmt <- as.character(raw$format %||% "docx")
  if (!fmt %in% c("docx", "html")) api_error(400, "invalid_option", "Format must be docx or html.")
  dec <- suppressWarnings(as.integer(raw$decimals %||% 3L)); if (is.na(dec) || !dec %in% 2:3) dec <- 3L
  pstyle <- as.character(raw$pStyle %||% "stars"); if (!pstyle %in% c("stars", "exact")) pstyle <- "stars"
  lz <- as.character(raw$leadingZero %||% "apa"); if (!lz %in% c("apa", "always")) lz <- "apa"
  font <- as.character(raw$font %||% "Times New Roman"); if (!font %in% c("Times New Roman", "Arial", "Calibri")) font <- "Times New Roman"
  tables <- as_chr_vec(raw$tables); tables <- if (length(tables)) intersect(tables, PUB_TABLE_IDS) else PUB_TABLE_IDS
  list(format = fmt, decimals = dec, pStyle = pstyle, leadingZero = lz, font = font, tables = tables,
       orientation = if (identical(as.character(raw$orientation %||% ""), "landscape")) "landscape" else "portrait",
       pageBreaks = !isFALSE(raw$pageBreaks), hypotheses = raw$hypotheses %||% list(), decisionRule = raw$decisionRule)
}

# ---- APA number formatting ----------------------------------------------------------------------
pt_num <- function(x, d, bounded = FALSE, lz = "apa") {
  v <- suppressWarnings(as.numeric(x %||% NA))
  if (!length(v) || is.na(v)) return("\u2013")
  s <- formatC(abs(v), format = "f", digits = d)
  if (bounded && identical(lz, "apa") && abs(v) < 1) s <- sub("^0", "", s)   # APA: no leading zero when |value| cannot exceed 1
  if (v < 0 && as.numeric(s) != 0) s <- paste0("\u2212", s)
  s
}
pt_p <- function(p, lz = "apa") {
  v <- suppressWarnings(as.numeric(p %||% NA))
  if (!length(v) || is.na(v)) return("\u2013")
  if (v < 0.001) return(if (identical(lz, "apa")) "< .001" else "< 0.001")
  pt_num(v, 3, bounded = TRUE, lz = lz)
}
pt_stars <- function(p) {
  v <- suppressWarnings(as.numeric(p %||% NA))
  if (!length(v) || is.na(v)) return("")
  if (v < 0.001) "***" else if (v < 0.01) "**" else if (v < 0.05) "*" else ""
}
pt_ci <- function(lo, hi, d) paste0("[", pt_num(lo, d), ", ", pt_num(hi, d), "]")

# One table: header labels, rows of character cells, alignment ("l"/"c"), notes, and layout hints.
pt_table <- function(id, title, header, rows, notes = list(), merge = integer(0), bold = list(), align = NULL, label_cols = 1L, span = integer(0)) {
  if (!length(rows)) return(NULL)
  if (is.null(align)) align <- c(rep("l", label_cols), rep("c", length(header) - label_cols))
  list(id = id, title = title, header = header, rows = rows, notes = notes, merge = merge, bold = bold, align = align, span = span)
}

# ---- Specification ----------------------------------------------------------------------------------
pub_tables_spec <- function(res, opts) {
  if (result_kind(res) == "efa") return(efa_pub_spec(res, opts))
  if (result_kind(res) == "regression") return(reg_pub_spec(res, opts))
  if (result_kind(res) == "fimix") return(fimix_pub_spec(res, opts))
  if (result_kind(res) == "lpa") return(lpa_pub_spec(res, opts))
  if (result_kind(res) == "qca") return(qca_pub_spec(res, opts))
  if (result_kind(res) == "ml") return(ml_pub_spec(res, opts))
  if (result_kind(res) == "stats") return(stats_pub_spec(res, opts))
  if (result_kind(res) == "invariance") return(inv_pub_spec(res, opts))
  if (result_kind(res) == "cbsem") return(cb_pub_spec(res, opts))
  d <- opts$decimals; lz <- opts$leadingZero; stars <- identical(opts$pStyle, "stars")
  b <- res$bootstrap
  ci_lab <- if (!is.null(b)) sprintf("%d%% CI", round(100 * (b$confidenceLevel %||% 0.95))) else "CI"
  n <- res$data$nUsed
  boot_key <- function(rows) if (is.null(rows)) list() else setNames(rows, vapply(rows, function(r) paste(r$from, r$to, sep = "\r"), ""))
  bp <- boot_key(b$paths); bl <- boot_key(b$loadings); bw <- boot_key(b$weights); bh <- boot_key(b$htmt)
  outer <- res$measurement$outer
  rel <- setNames(res$measurement$reliability, vapply(res$measurement$reliability, function(r) r$construct, ""))
  modes <- vapply(res$model$constructs %||% list(), function(c) c(c$name, c$mode), c("", ""))
  mode_of <- if (length(modes)) setNames(modes[2, ], modes[1, ]) else character(0)
  cons <- unique(vapply(outer, function(o) o$construct, ""))
  general <- function(...) paste0(...)
  sample_note <- sprintf("N = %s.%s", n,
                         if (!is.null(b)) sprintf(" Bootstrapping with %d subsamples (seed %s); percentile confidence intervals.", b$successful, b$seed) else "")
  prob_note <- "*p < .05. **p < .01. ***p < .001."
  if (!identical(lz, "apa")) prob_note <- "*p < 0.05. **p < 0.01. ***p < 0.001."
  tabs <- list()
  add <- function(t) if (!is.null(t) && t$id %in% opts$tables) tabs[[length(tabs) + 1]] <<- t

  # 1 Descriptives
  ind_con <- setNames(vapply(outer, function(o) o$construct, ""), vapply(outer, function(o) o$indicator, ""))
  desc <- Filter(function(r) r$indicator %in% names(ind_con), res$descriptives$items %||% list())
  any_miss <- any(vapply(desc, function(r) isTRUE(num_of(r$missing) > 0), TRUE))
  add(pt_table("descriptives", "Descriptive Statistics of the Indicators",
    c("Construct", "Indicator", "M", "SD", "Min", "Max", "Skewness", "Kurtosis", if (any_miss) "Missing"),
    lapply(desc, function(r) c(ind_con[[r$indicator]], r$indicator, pt_num(r$mean, 2), pt_num(r$sd, 2), pt_num(r$min, 0), pt_num(r$max, 0),
                               pt_num(r$skewness, d), pt_num(r$kurtosis, d), if (any_miss) as.character(r$missing %||% 0))),
    notes = list(paste0(sample_note, " Kurtosis as reported by SEMinR (normal distribution = 3).")), merge = 1L, label_cols = 2L))

  # 2 Measurement model (reflective and Mode A composites)
  refl_cons <- cons[!mode_of[cons] %in% "composite_b"]
  mrows <- list()
  for (k in refl_cons) {
    items <- Filter(function(o) o$construct == k, outer); r <- rel[[k]]
    for (i in seq_along(items)) {
      o <- items[[i]]; pl <- bl[[paste(o$indicator, k, sep = "\r")]]$p
      first <- i == 1
      mrows[[length(mrows) + 1]] <- c(k, o$indicator, paste0(pt_num(o$loading, d, TRUE, lz), if (stars) pt_stars(pl) else ""), pt_num(o$vif, d),
        if (first) pt_num(r$alpha, d, TRUE, lz) else "", if (first) pt_num(r$rhoA, d, TRUE, lz) else "",
        if (first) pt_num(r$rhoC, d, TRUE, lz) else "", if (first) pt_num(r$AVE, d, TRUE, lz) else "")
    }
  }
  has_plsc <- any(mode_of[refl_cons] %in% "reflective")
  add(pt_table("measurement", "Measurement Model: Indicator Reliability, Internal Consistency and Convergent Validity",
    c("Construct", "Indicator", "Loading", "VIF", "\u03b1", "\u03c1A", "\u03c1C", "AVE"), mrows,
    notes = list(paste0(sample_note, " \u03b1 = Cronbach\u2019s alpha; \u03c1A and \u03c1C = composite reliability; AVE = average variance extracted; VIF = variance inflation factor.",
                        if (has_plsc) " Reflective constructs were estimated with consistent PLS (PLSc)." else "",
                        " Recommended thresholds: loadings \u2265 0.708, \u03b1 and \u03c1 \u2265 0.70, AVE \u2265 0.50, VIF < 5 (Hair et al., 2019)."),
                 if (stars && !is.null(b)) prob_note),
    merge = 1L, label_cols = 2L))

  # 3 Formative (Mode B)
  form_cons <- cons[mode_of[cons] %in% "composite_b"]
  frows <- list()
  for (k in form_cons) for (o in Filter(function(o) o$construct == k, outer)) {
    w <- bw[[paste(o$indicator, k, sep = "\r")]]
    frows[[length(frows) + 1]] <- c(k, o$indicator, paste0(pt_num(o$weight, d), if (stars && !is.null(w)) pt_stars(w$p) else ""),
      if (!is.null(b)) c(pt_num(w$t, 2), if (!stars) pt_p(w$p, lz), pt_ci(w$lower, w$upper, d)), pt_num(o$loading, d, TRUE, lz), pt_num(o$vif, d))
  }
  add(pt_table("formative", "Formative Measurement Model: Indicator Weights and Collinearity",
    c("Construct", "Indicator", "Weight", if (!is.null(b)) c("t", if (!stars) "p", ci_lab), "Loading", "VIF"), frows,
    notes = list(paste0(sample_note, " Mode B (formative) composites. VIF < 5 indicates no critical collinearity; non-significant weights with loadings \u2265 0.50 are commonly retained (Hair et al., 2019)."),
                 if (stars && !is.null(b)) prob_note),
    merge = 1L, label_cols = 2L))

  # 4 HTMT (numbered correlation-matrix layout)
  # Construct numbering for matrix tables follows the matrices' own order (interaction terms shown as "A × B").
  mat_keys <- function(tab) if (is.null(tab)) character(0) else vapply(tab$columns, function(c) c$key, "")[-1]
  # Interaction terms (moderation) are not measured constructs: they are left out of the discriminant-validity tables.
  measured <- function(k) k[k %in% cons]
  all_cons <- unique(c(measured(mat_keys(res$measurement$fornellLarcker)), measured(mat_keys(res$measurement$htmt)), cons))
  int_note <- if (length(setdiff(c(mat_keys(res$measurement$fornellLarcker), mat_keys(res$measurement$htmt)), cons))) " Interaction terms are omitted." else ""
  pretty <- function(k) gsub("*", " × ", k, fixed = TRUE)
  num_of_con <- function(k) as.character(match(k, all_cons))
  num_label <- function(k) paste0(match(k, all_cons), ". ", pretty(k))
  htmt <- res$measurement$htmt
  if (!is.null(htmt) && length(htmt$rows)) {
    keys <- measured(vapply(htmt$columns, function(c) c$key, "")[-1])
    hrows <- lapply(Filter(function(r) r[[htmt$columns[[1]]$key]] %in% cons, htmt$rows), function(r) {
      rn <- r[[htmt$columns[[1]]$key]]
      c(num_label(rn), vapply(keys, function(k) {
        v <- r[[k]]; if (is.null(v) || identical(k, rn)) return("")
        up <- bh[[paste(rn, k, sep = "\r")]]$upper %||% bh[[paste(k, rn, sep = "\r")]]$upper
        paste0(pt_num(v, d), if (!is.null(up)) paste0(" [", pt_num(up, d), "]") else "")
      }, ""))
    })
    used <- which(vapply(seq_along(keys), function(j) any(vapply(hrows, function(r) nzchar(r[j + 1]), TRUE)), TRUE))
    add(pt_table("htmt", "Discriminant Validity: Heterotrait\u2013Monotrait Ratio of Correlations (HTMT)",
      c("Construct", num_of_con(keys[used])), lapply(hrows, function(r) r[c(1, used + 1)]),
      notes = list(paste0("HTMT values below .85 (or .90 for conceptually similar constructs) support discriminant validity (Henseler et al., 2015).",
                          if (length(bh)) paste0(" Values in brackets are the upper limits of the bootstrap ", ci_lab, ".") else "", int_note))))
  }

  # 5 Fornell–Larcker
  fl <- res$measurement$fornellLarcker
  if (!is.null(fl) && length(fl$rows)) {
    keys <- measured(vapply(fl$columns, function(c) c$key, "")[-1])
    bold <- list()
    fl_rows <- Filter(function(r) r[[fl$columns[[1]]$key]] %in% cons, fl$rows)
    frl <- lapply(seq_along(fl_rows), function(i) {
      r <- fl_rows[[i]]; rn <- r[[fl$columns[[1]]$key]]
      c(num_label(rn), vapply(seq_along(keys), function(j) {
        k <- keys[j]; v <- r[[k]]; if (is.null(v)) return("")
        if (identical(k, rn)) bold[[length(bold) + 1]] <<- c(i, j + 1)
        pt_num(v, d, TRUE, lz)
      }, ""))
    })
    add(pt_table("fornell", "Discriminant Validity: Fornell\u2013Larcker Criterion",
      c("Construct", num_of_con(keys)), frl, bold = bold,
      notes = list("Bold diagonal values are the square roots of the AVE; off-diagonal values are construct correlations. Each diagonal value should exceed the correlations in its row and column (Fornell & Larcker, 1981).", int_note)))
  }

  # 6 Cross-loadings
  cl <- res$measurement$crossLoadings
  if (!is.null(cl) && length(cl$rows)) {
    keys <- measured(vapply(cl$columns, function(c) c$key, "")[-1])
    bold <- list()
    cl_rows <- Filter(function(r) r[[cl$columns[[1]]$key]] %in% names(ind_con), cl$rows)
    crows <- lapply(seq_along(cl_rows), function(i) {
      r <- cl_rows[[i]]; ind <- r[[cl$columns[[1]]$key]]
      c(ind, vapply(seq_along(keys), function(j) {
        if (identical(ind_con[ind][[1]], keys[j])) bold[[length(bold) + 1]] <<- c(i, j + 1)
        pt_num(r[[keys[j]]], d, TRUE, lz)
      }, ""))
    })
    add(pt_table("crossloadings", "Cross-Loadings of the Indicators",
      c("Indicator", num_of_con(keys)), crows, bold = bold,
      notes = list(paste0("Bold values are loadings on the assigned construct; each should exceed the indicator\u2019s loadings on all other constructs. Construct numbers: ",
                          paste0(match(keys, all_cons), " = ", pretty(keys), collapse = "; "), ".", int_note))))
  }

  # 7 Structural model / hypotheses
  hyp <- opts$hypotheses
  hyp_by <- if (length(hyp)) setNames(hyp, vapply(hyp, function(h) as.character(h$pathId %||% ""), "")) else list()
  rule <- opts$decisionRule
  use_rule <- !is.null(rule) && isTRUE(rule$enabled) && !is.null(b)
  struct <- Filter(function(p) !isTRUE(p$moderation), res$structural$paths)
  srows <- lapply(struct, function(p) {
    br <- bp[[paste(p$from, p$to, sep = "\r")]]
    h <- hyp_by[[p$id %||% ""]]
    dec <- if (use_rule) { rr <- rule; rr$expected <- h$expected %||% "any"; decision_for(br, rr) } else NULL
    c(if (length(hyp)) (h$label %||% "") else NULL,
      paste0(p$from, " \u2192 ", p$to, if (isTRUE(p$implicit)) "\u1d43" else ""),
      paste0(pt_num(p$coefficient, d), if (stars && !is.null(br)) pt_stars(br$p) else ""),
      if (!is.null(b)) c(pt_num(br$t, 2), if (!stars) pt_p(br$p, lz), pt_ci(br$lower, br$upper, d)),
      pt_num(p$fSquare, d), pt_num(p$vif, d), if (use_rule) dec)
  })
  any_implicit <- any(vapply(struct, function(p) isTRUE(p$implicit), TRUE))
  add(pt_table("structural", if (length(hyp)) "Structural Model: Path Coefficients and Hypothesis Testing" else "Structural Model: Path Coefficients",
    c(if (length(hyp)) "Hypothesis", "Path", "\u03b2", if (!is.null(b)) c("t", if (!stars) "p", ci_lab), "f\u00b2", "VIF", if (use_rule) "Decision"), srows,
    notes = list(paste0(sample_note, " \u03b2 = standardized path coefficient; f\u00b2 = effect size (.02 small, .15 medium, .35 large; Cohen, 1988); VIF = inner variance inflation factor; \u2013 = not reported by SEMinR (e.g., VIF with a single predictor).",
                        if (use_rule) sprintf(" Decision rule: supported when p < %s%s.", format(rule$alpha), if (identical(rule$criterion, "ci")) " (confidence interval excluding zero)" else if (identical(rule$criterion, "both")) " and the confidence interval excludes zero" else "") else ""),
                 if (any_implicit) "\u1d43 Direct effect of a moderator on the outcome, included with the moderating effect.",
                 if (stars && !is.null(b)) prob_note),
    label_cols = if (length(hyp)) 2L else 1L))

  # 8 Explanatory power
  add(pt_table("r2", "Explanatory Power of the Structural Model",
    c("Endogenous construct", "R\u00b2", "Adjusted R\u00b2"),
    lapply(Filter(function(r) !is.null(r$rSquared), res$structural$rSquared), function(r) c(r$construct, pt_num(r$rSquared, d, TRUE, lz), pt_num(r$adjRSquared, d, TRUE, lz))),
    notes = list("R\u00b2 values of .75, .50 and .25 are commonly described as substantial, moderate and weak (Hair et al., 2019).")))

  # 9 Mediation
  if (!is.null(b) && length(b$specificIndirectEffects)) {
    add(pt_table("mediation", "Mediation Analysis: Specific Indirect Effects",
      c("Indirect path", "\u03b2", "SD", "t", if (!stars) "p", ci_lab),
      lapply(b$specificIndirectEffects, function(r) c(paste(c(r$from, unlist(r$through), r$to), collapse = " \u2192 "),
        paste0(pt_num(r$original, d), if (stars) pt_stars(r$p) else ""), pt_num(r$bootSD, d), pt_num(r$t, 2), if (!stars) pt_p(r$p, lz), pt_ci(r$lower, r$upper, d))),
      notes = list(paste0(sample_note, " SD = bootstrap standard deviation. An indirect effect is significant when its confidence interval excludes zero (Zhao et al., 2010)."),
                   if (stars) prob_note)))
  }

  # 10 Moderation
  mods <- Filter(function(p) isTRUE(p$moderation), res$structural$paths)
  if (length(mods)) {
    add(pt_table("moderation", "Moderation Analysis: Interaction Effects",
      c("Interaction term", "Outcome", "\u03b2", if (!is.null(b)) c("t", if (!stars) "p", ci_lab), "f\u00b2"),
      lapply(mods, function(p) {
        br <- bp[[paste(p$from, p$to, sep = "\r")]]
        c(gsub("*", " \u00d7 ", p$from, fixed = TRUE), p$to, paste0(pt_num(p$coefficient, d), if (stars && !is.null(br)) pt_stars(br$p) else ""),
          if (!is.null(b)) c(pt_num(br$t, 2), if (!stars) pt_p(br$p, lz), pt_ci(br$lower, br$upper, d)), pt_num(p$fSquare, d))
      }),
      notes = list(paste0(sample_note, " Interaction terms were estimated with SEMinR\u2019s interaction_term(); f\u00b2 values of .005, .01 and .025 are described as small, medium and large for interaction effects (Kenny, 2018)."),
                   if (stars && !is.null(b)) prob_note), label_cols = 2L))
  }

  # 11 Model fit
  fit <- res$measurement$fit
  if (isTRUE(fit$available)) {
    lab <- c(SRMR = "SRMR", d_ULS = "d_ULS", d_G = "d_G", chiSquare = "\u03c7\u00b2", NFI = "NFI")
    add(pt_table("fit", "Model Fit",
      c("Fit index", "Saturated model", "Estimated model"),
      lapply(names(lab), function(k) c(lab[[k]], pt_num(fit$saturated[[k]], d), pt_num(fit$estimated[[k]], d))),
      notes = list(paste0("SRMR = standardized root mean square residual; d_ULS = squared Euclidean distance; d_G = geodesic distance; NFI = normed fit index. SRMR < .08 indicates good fit (Henseler et al., 2014).",
                          if (length(fit$interactionTerms)) " Interaction terms are included as composite constructs." else ""))))
  }

  # 12 Common method bias
  cm <- res$cmb
  if (!is.null(cm) && !is.null(cm$fullCollinearity)) {
    h <- cm$harman
    add(pt_table("cmb", "Common Method Bias: Full Collinearity Assessment",
      c("Construct", "Full collinearity VIF"),
      lapply(cm$fullCollinearity$constructs, function(r) c(r$construct, pt_num(r$vif, d))),
      notes = list(sprintf("Full collinearity VIFs \u2264 3.3 indicate that the model is free of common method bias (Kock, 2015). Harman\u2019s single-factor test: the first unrotated component explains %.1f%% of the variance of the %d indicators (threshold 50%%; Podsakoff et al., 2003).",
                           100 * (num_of(h$firstComponentShare) %||% NA), as.integer(h$indicators %||% 0)))))
  }
  if (!is.null(cm) && !is.null(cm$marker)) {
    mk <- cm$marker
    add(pt_table("marker", "Common Method Bias: Marker Variable Technique",
      c("Construct pair", "r", if (!stars) "p", "Adjusted r", if (!stars) "p (adjusted)"),
      lapply(mk$pairs, function(r) c(paste0(r$a, " \u2013 ", r$b),
        paste0(pt_num(r$original, d, TRUE, lz), if (stars) pt_stars(r$pOriginal) else ""), if (!stars) pt_p(r$pOriginal, lz),
        paste0(pt_num(r$adjusted, d, TRUE, lz), if (stars) pt_stars(r$pAdjusted) else ""), if (!stars) pt_p(r$pAdjusted, lz))),
      notes = list(sprintf("Marker variable: %s (n = %s). The smallest positive marker\u2013construct correlation (%s) was partialled out: adjusted r = (r \u2212 %s) / (1 \u2212 %s) (Lindell & Whitney, 2001).",
                           mk$variable, mk$n, pt_num(mk$rM, d, TRUE, lz), pt_num(mk$rM, d, TRUE, lz), pt_num(mk$rM, d, TRUE, lz)), if (stars) prob_note)))
  }

  # 13 PLSpredict
  if (!is.null(res$predict)) {
    add(pt_table("predict", "Out-of-Sample Predictive Power (PLSpredict)",
      c("Indicator", "PLS RMSE", "LM RMSE", "PLS MAE", "LM MAE"),
      lapply(res$predict$items, function(r) c(r$indicator, pt_num(r$plsRMSE, d), pt_num(r$lmRMSE, d), pt_num(r$plsMAE, d), pt_num(r$lmMAE, d))),
      notes = list(sprintf("%s-fold cross-validation, %s repetition(s). Lower PLS than LM (linear regression benchmark) errors for most indicators indicate predictive power (Shmueli et al., 2019).%s",
                           res$predict$noFolds, res$predict$reps,
                           if (identical(res$predict$method, "two_stage_folds")) paste0(" ", predict_hoc_note(res$predict)) else ""))))
  }

  # 14 NCA
  if (!is.null(res$nca)) {
    add(pt_table("nca", "Necessary Condition Analysis",
      c("Condition", "Ceiling", "Effect size d", "p"),
      lapply(res$nca$results, function(r) c(r$condition, toupper(gsub("_", "-", r$ceiling)), pt_num(r$effectSize, d), pt_p(r$p, lz))),
      notes = list("Effect sizes 0 \u2264 d < .1 small, .1 \u2264 d < .3 medium, .3 \u2264 d < .5 large, d \u2265 .5 very large (Dul, 2016). p values from the NCA permutation test."), label_cols = 2L))
  }

  # 15 IPMA
  if (!is.null(res$ipma)) {
    add(pt_table("ipma", sprintf("Importance\u2013Performance Map Analysis (Target: %s)", res$ipma$target),
      c("Construct", "Importance", "Performance"),
      lapply(res$ipma$constructs, function(r) c(r$construct, pt_num(r$importance, d), pt_num(r$performance, 2))),
      notes = list("Importance = unstandardized total effect on the target construct; performance = rescaled construct score (0\u2013100; Ringle & Sarstedt, 2016).")))
    cp <- res$ipma$cipma
    if (!is.null(cp)) add(pt_table("cipma", sprintf("Combined Importance\u2013Performance Map Analysis (Target: %s)", cp$target),
      c("Construct", "Importance", "Performance", "d", "p", "Necessary", "Required level", "Cases below (%)"),
      lapply(cp$constructs, function(r) c(r$construct, pt_num(r$importance, d), pt_num(r$performance, 2), pt_num(r$effectSize, d), pt_p(r$p, lz), if (isTRUE(r$necessary)) "Yes" else "No",
                                          if (is.null(r$required)) "NN" else pt_num(r$required, 2), sprintf("%.1f", r$pctBelow))),
      notes = list(sprintf("NCA (%s ceiling) on the rescaled construct scores (0\u2013100); necessary: d > 0 and p < .05 (%d permutations). Required level: minimum level of the condition for %s = %s; cases below cannot reach that outcome level (Hauff et al., 2024). %.1f%% of the cases are below the required level of at least one necessary condition.",
                           toupper(gsub("_", "-", cp$ceiling)), as.integer(cp$testRep), cp$target, format(cp$outcomeLevel), cp$pctBelowAny))))
  }

  # 16 MGA
  if (!is.null(res$mga)) {
    g <- res$mga
    add(pt_table("mga", "Multigroup Analysis (PLS-MGA)",
      c("Path", sprintf("\u03b2 (%s, n = %s)", mga_label(g, 1), g$n1), sprintf("\u03b2 (%s, n = %s)", mga_label(g, 2), g$n2), "Difference", "p (PLS-MGA)"),
      lapply(g$paths, function(r) c(paste0(r$from, " \u2192 ", r$to), pt_num(r$group1, d), pt_num(r$group2, d), pt_num(r$diff, d), pt_p(r$p, lz))),
      notes = list(sprintf("Group 1 = %s; Group 2 = %s (grouping variable: %s). PLS-MGA p values are one-sided as returned by SEMinR (Henseler et al., 2009).",
                           paste(unlist(g$group1), collapse = ", "), paste(unlist(g$group2), collapse = ", "), g$groupVariable))))
  }

  for (i in seq_along(tabs)) tabs[[i]]$number <- i
  tabs
}

# ---- Rendering ---------------------------------------------------------------------------------------
quarto_bin <- function() {
  cand <- c(Sys.getenv("PATHLAB_QUARTO"), Sys.which("quarto"),
            "C:/Program Files/Quarto/bin/quarto.exe",
            file.path(Sys.getenv("LOCALAPPDATA"), "Programs", "Quarto", "bin", "quarto.exe"),
            "C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe",
            "C:/Program Files/Posit/RStudio/resources/app/bin/quarto/bin/quarto.exe",
            "/usr/local/bin/quarto", "/opt/quarto/bin/quarto", "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto")
  cand <- cand[nzchar(cand) & file.exists(cand)]
  if (length(cand)) normalizePath(cand[1], winslash = "/") else NULL
}

pub_tables_available <- function() {
  missing <- c("flextable", "knitr", "rmarkdown", "officer")[!vapply(c("flextable", "knitr", "rmarkdown", "officer"), requireNamespace, TRUE, quietly = TRUE)]
  list(available = !is.null(quarto_bin()) && !length(missing), quarto = !is.null(quarto_bin()), missingPackages = arr(missing))
}

# A4 portrait reference document (2.54 cm margins) with the chosen font for Quarto's Word output.
make_reference_docx <- function(quarto, path, font, orientation = "portrait") {
  base <- tempfile(fileext = ".docx")
  processx::run(quarto, c("pandoc", "-o", base, "--print-default-data-file", "reference.docx"), error_on_status = TRUE)
  doc <- officer::read_docx(base)
  doc <- officer::body_set_default_section(doc, officer::prop_section(
    page_size = if (identical(orientation, "landscape")) officer::page_size(orient = "landscape", width = 11.69, height = 8.27)
                else officer::page_size(orient = "portrait", width = 8.27, height = 11.69),
    page_margins = officer::page_mar(top = 1, bottom = 1, left = 1, right = 1, header = 0.5, footer = 0.5, gutter = 0)))
  for (st in list(c("Normal", "Normal", 12), c("BodyText", "Body Text", 12), c("FirstParagraph", "First Paragraph", 12),
                  c("Title", "Title", 16), c("Subtitle", "Subtitle", 12), c("Date", "Date", 11))) {
    try(doc <- officer::docx_set_paragraph_style(doc, style_id = st[1], style_name = st[2],
          fp_p = officer::fp_par(text.align = if (st[1] %in% c("Title", "Subtitle", "Date")) "center" else "left", padding.bottom = 6),
          fp_t = officer::fp_text_lite(font.family = font, font.size = as.numeric(st[3]), bold = st[1] == "Title", color = "black")), silent = TRUE)
  }
  print(doc, target = path)
  invisible(path)
}

# Rendered by Quarto (knitr engine): draws each table specification with flextable in APA layout.
pt_flextable <- function(t, opts, html = FALSE) {
  df <- as.data.frame(do.call(rbind, lapply(t$rows, function(r) as.character(r))), stringsAsFactors = FALSE)
  names(df) <- paste0("c", seq_along(t$header))
  ft <- flextable::flextable(df)
  ft <- flextable::set_header_labels(ft, values = setNames(as.list(t$header), names(df)))
  # APA statistical symbols in italics; rho with a subscript.
  for (j in seq_along(t$header)) {
    h <- t$header[j]
    if (is.na(h)) next
    if (h %in% c("M", "SD", "t", "p", "r", "n")) ft <- flextable::compose(ft, j = j, part = "header", value = flextable::as_paragraph(flextable::as_i(h)))
    else if (h %in% c("\u03c1A", "\u03c1C")) ft <- flextable::compose(ft, j = j, part = "header", value = flextable::as_paragraph("\u03c1", flextable::as_sub(substr(h, 2, 2))))
    else if (h == "f\u00b2") ft <- flextable::compose(ft, j = j, part = "header", value = flextable::as_paragraph(flextable::as_i("f"), flextable::as_sup("2")))
    else if (h == "Adjusted r") ft <- flextable::compose(ft, j = j, part = "header", value = flextable::as_paragraph("Adjusted ", flextable::as_i("r")))
    else if (h == "p (adjusted)") ft <- flextable::compose(ft, j = j, part = "header", value = flextable::as_paragraph(flextable::as_i("p"), " (adjusted)"))
  }
  if (length(t$merge)) ft <- flextable::merge_v(ft, j = t$merge, part = "body")
  for (j in seq_along(t$align)) ft <- flextable::align(ft, j = j, align = if (t$align[j] == "l") "left" else "center", part = "all")
  ft <- flextable::valign(ft, valign = "top", part = "body")
  for (bc in t$bold) ft <- flextable::bold(ft, i = bc[1], j = bc[2], part = "body")
  # Column widths from the column heads and body only (before the full-width title and note rows), at the final
  # font size. Tables wider than the A4 text width keep their font: numeric columns stay compact and the label
  # columns share the remaining width, so long names wrap inside their cells instead of shrinking the text.
  ncols <- length(t$header)
  body_size <- if (ncols >= 9) 9 else 10
  ft <- flextable::font(ft, fontname = opts$font, part = "all")
  ft <- flextable::fontsize(ft, size = body_size, part = "all")
  ft <- flextable::padding(ft, padding.top = 2, padding.bottom = 2, padding.left = 4, padding.right = 4, part = "all")
  # Rows that span the whole table (`span`, e.g. model-fit lines) do not set the column widths.
  w <- if (length(t$span)) {
    dw <- df; dw[t$span, ] <- ""
    fw <- flextable::font(flextable::flextable(dw), fontname = opts$font, part = "all")
    fw <- flextable::fontsize(fw, size = body_size, part = "all")
    fw <- flextable::set_header_labels(fw, values = setNames(as.list(t$header), names(dw)))
    fw <- flextable::padding(fw, padding.top = 2, padding.bottom = 2, padding.left = 4, padding.right = 4, part = "all")
    flextable::dim_pretty(fw)$widths
  } else flextable::dim_pretty(ft)$widths
  for (i in t$span) ft <- flextable::merge_at(ft, i = i, j = seq_along(t$header), part = "body")
  if (length(t$span)) ft <- flextable::align(ft, i = t$span, align = "left", part = "body")
  # A4 minus 1-inch margins: 6.27 in (portrait) or 9.69 in (landscape).
  text_width <- if (identical(opts$orientation, "landscape")) 9.69 else 6.27
  wide <- sum(w) > text_width
  if (wide) {
    # Short columns (numbers, hypothesis labels) keep their natural width; long label columns share the rest.
    short <- which(w <= 1.1 | t$align != "l"); long <- setdiff(seq_along(w), short)
    w[short] <- pmin(w[short], 1.1)
    if (length(long)) w[long] <- max(text_width - sum(w[short]), 1.4 * length(long)) * w[long] / sum(w[long])
  }
  ft <- flextable::width(ft, j = seq_along(w), width = w)
  # Title above the table (APA: bold "Table n", italic title on the next line), notes below.
  ft <- flextable::add_header_lines(ft, values = t$title)
  ft <- flextable::add_header_lines(ft, values = paste("Table", t$number))
  ft <- flextable::bold(ft, i = 1, part = "header")
  ft <- flextable::italic(ft, i = 2, part = "header")
  ft <- flextable::align(ft, i = 1:2, align = "left", part = "header")
  # Borders last: no lines around the title; rules under the title, under the column heads and under the body.
  rule <- officer::fp_border(color = "black", width = 1)
  ft <- flextable::border_remove(ft)
  ft <- flextable::hline(ft, i = 2, part = "header", border = rule)
  ft <- flextable::hline_bottom(ft, part = "header", border = rule)
  ft <- flextable::hline_bottom(ft, part = "body", border = rule)
  notes <- Filter(function(x) !is.null(x) && nzchar(x), t$notes)
  for (k in seq_along(notes)) {
    para <- if (k == 1) flextable::as_paragraph(flextable::as_i("Note. "), notes[[k]]) else flextable::as_paragraph(notes[[k]])
    ft <- flextable::add_footer_lines(ft, values = "")
    ft <- flextable::compose(ft, i = k, j = 1, part = "footer", value = para)
  }
  ft <- flextable::font(ft, fontname = opts$font, part = "all")
  ft <- flextable::fontsize(ft, i = 1:2, size = 11, part = "header")
  if (length(notes)) {
    ft <- flextable::fontsize(ft, size = 9, part = "footer")
    ft <- flextable::align(ft, align = "left", part = "footer")
  }
  ft <- flextable::padding(ft, padding.top = 2, padding.bottom = 2, padding.left = 4, padding.right = 4, part = "all")
  ft <- flextable::padding(ft, i = 1:2, padding.left = 0, part = "header")
  ft <- flextable::line_spacing(ft, space = 1, part = "all")
  # Fixed widths keep the layout identical in Word; tables with six or more columns span the text width.
  if (!wide && ncols >= 6) ft <- flextable::width(ft, j = seq_along(w), width = w * text_width / sum(w))
  ft <- flextable::set_table_properties(ft, layout = "fixed", align = "center")
  if (!html) ft <- flextable::paginate(ft, init = TRUE, hdr_ftr = TRUE)
  ft
}

render_pub_tables <- function(res, opts, work_dir, project_name = "PathLab SEM project") {
  quarto <- quarto_bin()
  if (is.null(quarto)) stop("Quarto was not found.")
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  spec <- pub_tables_spec(res, opts)
  if (!length(spec)) stop("None of the selected tables can be produced from this result.")
  saveRDS(list(tables = spec, opts = opts), file.path(work_dir, "tables.rds"))
  # The Quarto document only needs the table-drawing function; write it next to the document.
  dump("pt_flextable", file = file.path(work_dir, "pubtables.R"), envir = environment(pt_flextable))
  html <- identical(opts$format, "html")
  yaml <- c("---",
            sprintf("title: %s", deparse("Results Tables")),
            sprintf("subtitle: %s", deparse(paste0(project_name, " \u2014 ", switch(result_kind(res), efa = paste0("Exploratory factor analysis (psych ", res$psychVersion, ")"), fimix = paste0("FIMIX-PLS (SEMinR ", res$packages$seminr, ", flexmix ", res$packages$flexmix, ")"), qca = paste0(res$setType, " (QCA ", res$packages$QCA, ")"), stats = paste0(res$label, if (is.null(res$packages$jmv)) paste0(" (", names(res$packages)[1], " ", unlist(res$packages)[1], ")") else if (!is.null(res$packages$BayesFactor)) paste0(" (BayesFactor ", res$packages$BayesFactor, ")") else paste0(" (jmv ", res$packages$jmv, ")")), ml = if (identical(res$mode, "semann")) paste0("SEM-ANN (nnet ", res$packages$nnet, ")") else paste0("Machine learning (caret ", res$packages$caret, ")"), lpa = paste0("Latent profile analysis (tidyLPA ", res$packages$tidyLPA, ", mclust ", res$packages$mclust, ")"), invariance = paste0("Measurement invariance (lavaan ", res$environment$lavaan, ")"), cbsem = paste0("CB-SEM (lavaan ", res$environment$lavaan, ")"),
              regression = paste0(res$familyLabel, " regression (R ", R.version$major, ".", R.version$minor, ")"), paste0("PLS-SEM (SEMinR ", res$environment$seminrVersion, ")"))))),
            sprintf("date: %s", deparse(format(Sys.Date(), "%Y-%m-%d"))),
            "date-format: \"D MMMM YYYY\"",
            "format:")
  if (html) {
    writeLines(c(
      "body { font-family: '", opts$font, "', serif; max-width: ", if (identical(opts$orientation, "landscape")) "297mm" else "210mm", "; margin: 0 auto; padding: 20mm 18mm; background: #fff; color: #000; }",
      "h1.title { font-size: 16pt; text-align: center; } p.subtitle, .quarto-title-meta { text-align: center; }",
      ".pt-table { margin: 0 0 28px; page-break-inside: avoid; } table { margin: 0 auto; }",
      "@media print { @page { size: A4 ", opts$orientation, "; margin: 25.4mm; } .pt-table { page-break-after: always; } }"), file.path(work_dir, "pubtables.css"), sep = "")
    yaml <- c(yaml, "  html:", "    embed-resources: true", "    theme: none", "    css: pubtables.css", "    toc: false", "    title-block-banner: false")
  } else {
    make_reference_docx(quarto, file.path(work_dir, "reference.docx"), opts$font, opts$orientation)
    yaml <- c(yaml, "  docx:", "    reference-doc: reference.docx", "    toc: false")
  }
  yaml <- c(yaml, "execute:", "  echo: false", "  warning: false", "  message: false", "---", "")
  body <- c(
    "```{r}",
    "#| results: asis",
    "suppressPackageStartupMessages({ library(flextable); library(officer) })",
    "`%||%` <- function(a, b) if (is.null(a)) b else a",
    "source('pubtables.R', local = TRUE)",
    "spec <- readRDS('tables.rds')",
    sprintf("is_html <- %s", if (html) "TRUE" else "FALSE"),
    "for (i in seq_along(spec$tables)) {",
    "  t <- spec$tables[[i]]",
    "  if (is_html) cat('\\n\\n::: {.pt-table}\\n\\n')",
    "  cat(flextable::flextable_to_rmd(pt_flextable(t, spec$opts, html = is_html), print = FALSE))",
    "  if (is_html) cat('\\n\\n:::\\n\\n')",
    "  if (!is_html && i < length(spec$tables)) {",
    "    if (isTRUE(spec$opts$pageBreaks)) cat('\\n\\n```{=openxml}\\n<w:p><w:r><w:br w:type=\"page\"/></w:r></w:p>\\n```\\n\\n') else cat('\\n\\n&nbsp;\\n\\n')",
    "  }",
    "}",
    "```")
  qmd <- file.path(work_dir, "tables.qmd")
  writeLines(c(yaml, body), qmd, useBytes = TRUE)
  out_name <- paste0("tables.", if (html) "html" else "docx")
  # Quarto runs the R chunk with this R installation and its package libraries.
  r <- processx::run(quarto, c("render", qmd, "--to", if (html) "html" else "docx", "--output", out_name),
                     wd = work_dir, error_on_status = FALSE, timeout = 300,
                     env = c("current", QUARTO_R = R.home("bin"), R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)))
  out <- file.path(work_dir, out_name)
  if (r$status != 0 || !file.exists(out)) {
    lines <- trimws(strsplit(paste(r$stderr, r$stdout), "\n")[[1]])
    stop(paste("Quarto could not render the tables:", paste(tail(lines[nzchar(lines)], 6), collapse = " | ")))
  }
  writeLines(sanitize_message(paste(r$stderr, collapse = "\n")), file.path(work_dir, "quarto.log"))
  list(file = out, tables = length(spec))
}

# ---- Background rendering (Quarto takes 10-25 s; the API stays responsive) ---------------------------
start_pub_tables <- function(proj, result_json, opts, project_name) {
  avail <- pub_tables_available()
  if (!isTRUE(avail$available)) {
    api_error(501, "quarto_unavailable",
              if (!avail$quarto) "Quarto is not installed on the analysis server, so publication tables cannot be rendered." else
                sprintf("R package(s) needed for publication tables are missing: %s.", paste(unlist(avail$missingPackages), collapse = ", ")),
              "Install Quarto (https://quarto.org) or set PATHLAB_QUARTO to its executable, and run Rscript backend/install_packages.R.")
  }
  id <- new_id("t")
  dir <- file.path(proj$dir, "pubtables", id)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(result_json, file.path(dir, "result.json"), useBytes = TRUE)
  proc <- callr::r_bg(function(backend_dir, dir, opts, project_name) {
    BACKEND_DIR <<- backend_dir
    for (f in c("util.R", "exports.R", "pubtables.R", "reports_extra.R", "fimix.R", "invariance.R", "model_spec.R", "analysis.R", "cbsem.R", "gsca.R", "lpa.R", "qca.R", "ml.R", "stats.R", "panel.R", "stats_more.R", "gpower.R", "soper.R")) source(file.path(backend_dir, "R", f), local = FALSE)
    res <- jsonlite::fromJSON(file.path(dir, "result.json"), simplifyVector = FALSE)
    out <- tryCatch(c(list(status = "completed"), render_pub_tables(res, opts, file.path(dir, "out"), project_name)),
                    error = function(e) list(status = "failed", message = sanitize_message(conditionMessage(e))))
    jsonlite::write_json(out, file.path(dir, "status.json"), auto_unbox = TRUE)
  }, args = list(backend_dir = BACKEND_DIR, dir = dir, opts = opts, project_name = project_name),
     stdout = NULL, stderr = file.path(dir, "worker.log"), supervise = TRUE)
  proj$pubjobs[[id]] <- list(id = id, dir = dir, process = proc, format = opts$format, startedAt = now_iso())
  list(id = id, status = "running", format = opts$format)
}

pub_tables_status <- function(proj, id) {
  job <- proj$pubjobs[[id]]
  if (is.null(job)) api_error(404, "not_found", "These publication tables were not found. Generate them again.")
  sp <- file.path(job$dir, "status.json")
  if (!file.exists(sp)) {
    if (!job$process$is_alive()) return(list(id = id, status = "failed", message = "The table renderer stopped unexpectedly."))
    return(list(id = id, status = "running", format = job$format))
  }
  s <- jsonlite::fromJSON(sp, simplifyVector = TRUE)
  list(id = id, status = s$status, format = job$format, tables = s$tables, message = s$message)
}
