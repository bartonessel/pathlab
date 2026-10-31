# Qualitative comparative analysis (QCA package): calibration, necessity, truth table and Boolean minimisation for
# crisp-set, multi-value and fuzzy-set QCA. Every number comes from QCA: calibrate(), pof(), truthTable() and
# minimize() (complex, parsimonious and intermediate solutions). The data are never changed: the project
# missing-value code is treated as missing for this analysis only, rows with missing values are left out, and the
# calibrated sets exist only inside the analysis (they can be saved as derived variables on request). The optional
# 0.001 shift of fuzzy memberships of exactly 0.5 (Fiss, 2011) is applied to the calibrated conditions and reported.

QCA_METHODS <- c(fuzzy = "Fuzzy (direct method)", crisp = "Crisp / multi-value thresholds", none = "Already calibrated (0-1)")

normalize_qca <- function(raw) {
  raw <- raw %||% list()
  num <- function(v, default, lo, hi, label) {
    x <- suppressWarnings(as.numeric(v %||% default))[1]
    if (is.na(x) || x < lo || x > hi) api_error(400, "invalid_qca", sprintf("%s must be between %s and %s.", label, lo, hi))
    x
  }
  conds <- unique(as_chr_vec(raw$conditions))
  outcome <- as.character(raw$outcome %||% "")[1]
  cal_raw <- raw$calibration %||% list()
  cal <- lapply(setNames(c(outcome, conds), c(outcome, conds)), function(v) {
    c0 <- cal_raw[[v]] %||% list()
    method <- as.character(c0$method %||% "fuzzy")[1]
    if (!method %in% names(QCA_METHODS)) method <- "fuzzy"
    anchors <- as.character(c0$anchors %||% "percentile")[1]
    if (!anchors %in% c("percentile", "value")) anchors <- "percentile"
    th <- suppressWarnings(as.numeric(unlist(c0$thresholds %||% list())))
    if (!length(th) || anyNA(th)) th <- if (method == "fuzzy") (if (anchors == "percentile") c(0.05, 0.5, 0.95) else numeric(0)) else if (anchors == "percentile") 0.5 else numeric(0)
    list(method = method, anchors = anchors, thresholds = th)
  })
  dir_raw <- raw$expectations %||% list()
  expect <- lapply(setNames(conds, conds), function(v) { e <- as.character(dir_raw[[v]] %||% "present")[1]; if (e %in% c("present", "absent", "either")) e else "present" })
  ncut <- raw$nCut
  overrides <- Filter(function(x) !is.null(x$row) && as.character(x$out %||% "") %in% c("0", "1"), raw$rowOverrides %||% list())
  list(
    outcome = outcome,
    negateOutcome = isTRUE(raw$negateOutcome),
    conditions = conds,
    calibration = cal,
    avoidHalf = !isFALSE(raw$avoidHalf),
    inclCut = num(raw$inclCut, 0.8, 0.5, 1, "The consistency threshold"),
    priCut = num(raw$priCut, 0.7, 0, 1, "The PRI threshold"),
    nCut = if (is.null(ncut) || identical(ncut, "")) NULL else as.integer(num(ncut, 1, 1, 1000, "The frequency threshold")),
    necessityCut = num(raw$necessityCut, 0.9, 0.5, 1, "The necessity threshold"),
    # Tobit regression (VGAM) of the outcome membership as a robustness check; on unless switched off.
    tobit = !isFALSE(raw$tobit),
    # Sensitivity analysis (alternative thresholds and calibration anchors) as a robustness check; on unless switched off.
    sensitivity = !isFALSE(raw$sensitivity),
    # Predictive validity (Woodside, 2014; Pappas & Woodside, 2021): the solution of one random half of the cases is
    # tested on the other half (and the other way round); on unless switched off.
    predictive = !isFALSE(raw$predictive),
    # Solution whose configurations are tested: the parsimonious one (Sukhov et al., 2023) or the intermediate one.
    predictiveSolution = { k <- as.character(raw$predictiveSolution %||% "parsimonious")[1]; if (k %in% c("parsimonious", "intermediate", "complex")) k else "parsimonious" },
    predictiveSeed = { ps <- suppressWarnings(as.integer(raw$predictiveSeed %||% 2026L))[1]; if (is.na(ps) || ps < 1) 2026L else ps },
    expectations = expect,
    # Outcome values coded by hand for truth-table rows: list of {outcome ("Y" or "~Y"), row, out}.
    rowOverrides = lapply(overrides, function(x) list(outcome = as.character(x$outcome %||% outcome)[1], row = as.integer(x$row)[1], out = as.character(x$out)[1]))
  )
}

qca_issue <- function(severity, code, message, hint = NULL) list(severity = severity, code = code, message = message, hint = hint)

validate_qca <- function(df, o) {
  out <- list()
  add <- function(...) out[[length(out) + 1]] <<- qca_issue(...)
  if (!nzchar(o$outcome)) add("error", "qca_outcome", "Choose the outcome.")
  if (!length(o$conditions)) add("error", "qca_conditions", "Add at least one condition.")
  if (length(o$conditions) > 12) add("error", "qca_conditions", "Use at most 12 conditions (the truth table has 2^k rows).")
  if (nzchar(o$outcome) && o$outcome %in% o$conditions) add("error", "qca_outcome_condition", "The outcome cannot also be a condition.")
  vars <- c(if (nzchar(o$outcome)) o$outcome, o$conditions)
  miss <- setdiff(vars, names(df))
  if (length(miss)) add("error", "qca_missing_column", sprintf("Not in the dataset: %s.", paste(miss, collapse = ", ")))
  for (v in intersect(vars, names(df))) {
    x <- df[[v]]
    if (!is.numeric(x)) { add("error", "qca_not_numeric", sprintf("'%s' is not numeric.", v)); next }
    c0 <- o$calibration[[v]]
    th <- c0$thresholds
    if (c0$method == "fuzzy") {
      if (length(th) != 3) add("error", "qca_anchors", sprintf("'%s': the direct method needs three anchors (full non-membership, crossover, full membership).", v))
      else if (!(all(diff(th) > 0) || all(diff(th) < 0))) add("error", "qca_anchors", sprintf("'%s': the three anchors must be strictly increasing (or strictly decreasing for a reversed set).", v))
      if (c0$anchors == "percentile" && length(th) && any(th <= 0 | th >= 1)) add("error", "qca_anchors", sprintf("'%s': percentiles must be between 0 and 1 (for example 0.05, 0.50, 0.95).", v))
    }
    if (c0$method == "crisp") {
      if (!length(th) || length(th) > 2) add("error", "qca_thresholds", sprintf("'%s': give one threshold (crisp set) or two (multi-value, three categories).", v))
      else if (length(th) == 2 && th[2] <= th[1]) add("error", "qca_thresholds", sprintf("'%s': the second threshold must be larger than the first.", v))
      if (identical(v, o$outcome) && length(th) > 1) add("error", "qca_thresholds", "The outcome must be a crisp (one threshold) or fuzzy set, not multi-value.")
      if (c0$anchors == "percentile" && length(th) && any(th <= 0 | th >= 1)) add("error", "qca_thresholds", sprintf("'%s': percentiles must be between 0 and 1.", v))
    }
    if (c0$method == "none") {
      r <- range(x, na.rm = TRUE)
      if (r[1] < 0 || r[2] > 1) add("error", "qca_not_calibrated", sprintf("'%s' has values outside 0-1, so it is not calibrated yet. Choose the direct method or thresholds.", v))
    }
  }
  out
}

#' Syntactic, upper-case names for QCA (QCA reads names in Boolean expressions); mapped back for display.
outcome_label <- function(o, neg) if (neg) paste0("~", o$outcome) else o$outcome

qca_safe_names <- function(v) {
  s <- toupper(gsub("[^A-Za-z0-9]+", "_", v))
  s <- gsub("^_+|_+$", "", s)
  s[!nzchar(s)] <- "X"
  s <- ifelse(grepl("^[0-9]", s), paste0("X", s), s)
  make.unique(s, sep = "_")
}

#' Replace safe names in a QCA expression by the original variable names.
qca_display <- function(expr, map) {
  if (is.null(expr) || !length(expr)) return(expr)
  keys <- names(map)[order(-nchar(names(map)))]
  vapply(expr, function(e) {
    for (k in keys) e <- gsub(paste0("(?<![A-Za-z0-9_])", k, "(?![A-Za-z0-9_])"), map[[k]], e, perl = TRUE)
    e
  }, "", USE.NAMES = FALSE)
}

qca_calibrate_one <- function(x, c0) {
  probs <- NULL
  th <- c0$thresholds
  if (c0$method != "none" && c0$anchors == "percentile") { probs <- th; th <- unname(stats::quantile(x, th, names = FALSE)) }
  if (c0$method == "fuzzy" && (anyDuplicated(signif(th, 12)) || !(all(diff(th) > 0) || all(diff(th) < 0))))
    stop(analysis_failure("calibration", "Two calibration anchors coincide.", "With percentile anchors this happens when many cases share a value; set the anchors as values instead."))
  set <- switch(c0$method,
                fuzzy = QCA::calibrate(x, type = "fuzzy", method = "direct", thresholds = th),
                crisp = QCA::calibrate(x, type = "crisp", thresholds = th),
                none = x)
  list(set = as.numeric(set), values = th, percentiles = probs)
}

#' Literals of a conjunction ("A*~B*C[1]") as a named vector condition -> sign/category.
qca_literals <- function(term) {
  parts <- strsplit(term, "*", fixed = TRUE)[[1]]
  setNames(vapply(parts, function(p) if (startsWith(p, "~")) "absent" else if (grepl("\\[", p)) sub("^.*\\[(.*)\\]$", "\\1", p) else "present", ""),
           sub("\\[.*$", "", sub("^~", "", parts)))
}

#' One minimisation result (complex, parsimonious, or one intermediate solution) as lists of models and terms.
# Model ambiguity: at most this many models of one solution are kept (the total is reported).
QCA_MAX_MODELS <- 20L
# A parsimonious solution with more models than this makes the intermediate solution explode (one per combination of
# complex and parsimonious models), so the intermediate solution is not computed (Baumgartner & Thiem, 2017).
QCA_MAX_AMBIGUITY <- 10L
# Time limit of one Boolean minimisation (seconds); a model that needs longer is reported instead of blocking the server.
QCA_MIN_SECONDS <- 30

qca_with_limit <- function(expr, seconds = QCA_MIN_SECONDS) {
  setTimeLimit(elapsed = seconds, transient = TRUE)
  on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
  tryCatch(expr, error = function(e) {
    if (grepl("time limit", conditionMessage(e), ignore.case = TRUE))
      simpleError(sprintf("The Boolean minimisation took longer than %d seconds and was stopped. Use fewer conditions, or raise the frequency or consistency threshold.", as.integer(seconds)))
    else e
  })
}

qca_models <- function(sol, map, keep_memberships = TRUE, yneg = NULL) {
  models <- sol$solution
  lapply(seq_len(min(length(models), QCA_MAX_MODELS)), function(m) {
    ic <- if (length(models) > 1 && !is.null(sol$IC$individual)) sol$IC$individual[[m]] else sol$IC
    pims <- ic$pims
    if (is.null(pims) && !is.null(sol$pims)) pims <- sol$pims[, intersect(models[[m]], colnames(sol$pims)), drop = FALSE]
    tab <- ic$incl.cov
    terms <- models[[m]]
    # Consistency of a configuration with the absence of the outcome: a configuration consistent with both Y and ~Y
    # (simultaneous subset relation) should not be read as sufficient (Schneider & Wagemann, 2012).
    negIncl <- function(x) { x <- as.numeric(x); if (is.null(yneg) || !length(x) || sum(x) <= 0) NULL else sum(pmin(x, yneg)) / sum(x) }
    rows <- lapply(seq_along(terms), function(i) {
      r <- if (!is.null(tab) && terms[i] %in% rownames(tab)) tab[terms[i], , drop = FALSE] else tab[i, , drop = FALSE]
      list(term = qca_display(terms[i], map), raw = terms[i], inclS = unname(r$inclS), PRI = unname(r$PRI), covS = unname(r$covS),
           # One term: its unique coverage equals its raw coverage (fsQCA reports it; QCA leaves it empty).
           covU = if (length(terms) == 1) unname(r$covS) else if (is.na(r$covU)) NULL else unname(r$covU), cases = if (!is.null(r$cases)) as.character(r$cases) else NULL,
           inclSNeg = if (!is.null(pims) && terms[i] %in% colnames(pims)) negIncl(pims[[terms[i]]]) else NULL)
    })
    s <- ic$sol.incl.cov
    out <- list(model = m, expression = paste(qca_display(terms, map), collapse = " + "), terms = rows,
         inclS = unname(s$inclS), PRI = unname(s$PRI), covS = unname(s$covS),
         inclSNeg = if (!is.null(pims) && ncol(pims)) negIncl(do.call(pmax, unname(as.list(pims)))) else NULL)
    # Membership of each case in each term and in the solution (fuzzy union = maximum), for XY plots.
    if (keep_memberships && !is.null(pims) && ncol(pims)) out$memberships <- list(terms = setNames(lapply(seq_len(ncol(pims)), function(j) as.numeric(pims[[j]])), qca_display(colnames(pims), map)),
                                                                                  solution = as.numeric(do.call(pmax, unname(as.list(pims)))))
    out
  })
}

#' Configuration chart (Ragin & Fiss, 2008): conditions of each intermediate configuration, core when they also
#' appear in a parsimonious term contained in the configuration, peripheral otherwise.
qca_chart <- function(inter, pars, conds_safe, map) {
  if (is.null(inter) || !length(inter)) return(NULL)
  pterms <- if (!is.null(pars) && length(pars)) unique(unlist(lapply(pars[[1]]$terms, `[[`, "raw"))) else character(0)
  plits <- lapply(pterms, qca_literals)
  lapply(inter[[1]]$terms, function(t) {
    lit <- qca_literals(t$raw)
    contained <- Filter(function(p) all(names(p) %in% names(lit)) && all(p == lit[names(p)]), plits)
    core <- unique(unlist(lapply(contained, names)))
    cells <- lapply(setNames(conds_safe, unname(map[conds_safe])), function(cn) {
      if (!cn %in% names(lit)) return("none")
      v <- lit[[cn]]
      kind <- if (cn %in% core) "core" else "peripheral"
      if (v == "absent") paste0(kind, "_absent") else if (v == "present") paste0(kind, "_present") else paste0(kind, "_", v)
    })
    list(term = t$term, cells = cells, inclS = t$inclS, PRI = t$PRI, covS = t$covS, covU = t$covU, inclSNeg = t$inclSNeg)
  })
}

qca_sufficiency <- function(cal_s, out_s, conds_s, o, n_cut, map, mv, neg = FALSE, only = NULL) {
  outcome <- if (neg) paste0("~", out_s) else out_s
  tt <- tryCatch(suppressWarnings(QCA::truthTable(cal_s, outcome = outcome, conditions = conds_s, incl.cut = o$inclCut, n.cut = n_cut,
                                                  pri.cut = o$priCut, show.cases = TRUE, sort.by = c("OUT", "incl"))),
                 error = function(e) stop(analysis_failure("truth_table", paste("The truth table could not be built:", sanitize_message(conditionMessage(e))))))
  # Outcome values coded by hand (fsQCA's "Delete and code" step allows editing the outcome column).
  ov <- Filter(function(x) identical(x$outcome, outcome_label(o, neg)), o$rowOverrides)
  overridden <- integer(0)
  for (x in ov) {
    k <- as.character(x$row)
    if (k %in% rownames(tt$tt) && tt$tt[k, "n"] >= n_cut) { tt$tt[k, "OUT"] <- x$out; overridden <- c(overridden, x$row) }
  }
  t0 <- tt$tt
  rows <- lapply(seq_len(nrow(t0)), function(i) {
    c(list(row = as.integer(rownames(t0)[i]), values = lapply(setNames(conds_s, unname(map[conds_s])), function(cn) as.numeric(t0[i, cn])),
           out = as.character(t0$OUT[i]), n = as.integer(t0$n[i]), incl = suppressWarnings(as.numeric(t0$incl[i])), PRI = suppressWarnings(as.numeric(t0$PRI[i]))),
      if (!is.null(t0$cases)) list(cases = as.character(t0$cases[i])))
  })
  n_rows_possible <- prod(tt$noflevels)
  covered <- sum(t0$n[t0$n >= n_cut])
  mini <- function(...) qca_with_limit(suppressWarnings(QCA::minimize(tt, details = TRUE, ...)))
  positive <- any(t0$OUT == "1")
  inc1 <- suppressWarnings(as.numeric(t0$incl[t0$OUT == "1"]))
  res <- list(outcome = outcome_label(o, neg), negated = neg, rows = rows, overridden = as.list(overridden),
              consistencyCutoffUsed = if (length(inc1)) min(inc1) else NULL,
              nRowsPossible = n_rows_possible, nRowsObserved = nrow(t0), nRemainders = n_rows_possible - nrow(t0),
              casesInRows = covered, nCut = n_cut)
  if (!positive) {
    res$message <- "No truth-table row meets the consistency, PRI and frequency thresholds, so there is no sufficient configuration."
    return(res)
  }
  # `only`: the solution types to compute (the sensitivity analysis needs one; the report needs all three).
  want <- function(k) is.null(only) || k %in% only
  skipped <- simpleError("not computed")
  cs <- if (want("complex")) mini() else skipped
  ps <- if (want("parsimonious") || want("intermediate")) mini(include = "?") else skipped
  dir <- unlist(lapply(conds_s, function(cn) {
    e <- o$expectations[[map[[cn]]]] %||% "present"
    if (cn %in% mv || e == "either") NULL else if (e == "absent") paste0("~", cn) else cn
  }))
  ok <- function(x) !inherits(x, "error")
  nps <- if (ok(ps)) length(ps$solution) else 0L
  ncs <- if (ok(cs)) length(cs$solution) else 0L
  res$modelsTotal <- list(complex = ncs, parsimonious = nps)
  is <- if (!want("intermediate")) skipped
        else if (nps > QCA_MAX_AMBIGUITY) simpleError(sprintf("The parsimonious solution has %d equally good models (severe model ambiguity; Baumgartner & Thiem, 2017), so the intermediate solution was not computed. Use fewer conditions, or raise the frequency or consistency threshold.", nps))
        else if (length(dir)) mini(include = "?", dir.exp = dir) else ps
  if (nps > QCA_MAX_MODELS || ncs > QCA_MAX_MODELS)
    res$ambiguity <- sprintf("Model ambiguity: %s; the first %d models of each solution are shown.",
                             paste(c(if (ncs > 1) sprintf("%d complex models", ncs), if (nps > 1) sprintf("%d parsimonious models", nps)), collapse = " and "), QCA_MAX_MODELS)
  keep <- nrow(cal_s) <= 5000
  yneg <- if (neg) cal_s[[out_s]] else 1 - cal_s[[out_s]]          # membership in the absence of the analysed outcome
  res$complex <- if (ok(cs)) qca_models(cs, map, keep, yneg) else NULL
  res$parsimonious <- if (ok(ps)) qca_models(ps, map, keep, yneg) else NULL
  if (ok(is)) {
    if (!is.null(is$i.sol)) {
      res$intermediate <- lapply(names(is$i.sol), function(k) list(label = k, models = qca_models(is$i.sol[[k]], map, keep, yneg),
                                                                    easyCounterfactuals = if (is.null(is$i.sol[[k]]$EC)) 0L else nrow(is$i.sol[[k]]$EC)))
    } else res$intermediate <- list(list(label = "C1P1", models = qca_models(is, map, keep, yneg), easyCounterfactuals = 0L))
  }
  inter <- if (length(res$intermediate)) res$intermediate[[1]]$models else res$complex
  res$chart <- qca_chart(inter, res$parsimonious, conds_s, map)
  errs <- Filter(function(e) !ok(e) && !identical(e, skipped), list(cs, ps, is))
  if (!length(res$complex) && !length(res$parsimonious) && !length(res$intermediate) && length(errs)) {
    res$message <- if (all(t0$OUT[t0$n >= n_cut] == "1"))
      "Every observed configuration meets the consistency threshold, so the outcome occurs whatever the conditions (all conditions are minimised away). Raise the consistency threshold, or check the calibration of the outcome (it may be skewed)."
      else paste("No solution could be computed:", sanitize_message(conditionMessage(errs[[1]])))
  }
  w <- c(if (length(errs)) vapply(errs, function(e) sanitize_message(conditionMessage(e)), ""), res$ambiguity)
  if (length(w)) res$warnings <- as.list(unique(w))
  res
}

qca_necessity <- function(cal_s, out_s, conds_s, o, map, mv, neg = FALSE) {
  y <- cal_s[[out_s]]
  if (neg) y <- 1 - y
  sets <- list()
  for (cn in conds_s) {
    x <- cal_s[[cn]]
    if (cn %in% mv) for (k in sort(unique(x))) sets[[sprintf("%s[%s]", cn, k)]] <- as.numeric(x == k)
    else { sets[[cn]] <- x; sets[[paste0("~", cn)]] <- 1 - x }
  }
  setms <- as.data.frame(sets, check.names = FALSE)
  p <- QCA::pof(setms, y, relation = "necessity")
  ic <- p$incl.cov
  su <- QCA::pof(setms, y, relation = "sufficiency")$incl.cov
  lapply(seq_len(nrow(ic)), function(i) list(condition = qca_display(rownames(ic)[i], map), inclN = unname(ic$inclN[i]), RoN = unname(ic$RoN[i]),
                                            covN = unname(ic$covN[i]), necessary = isTRUE(ic$inclN[i] >= o$necessityCut),
                                            inclS = unname(su$inclS[i]), PRI = unname(su$PRI[i]), covS = unname(su$covS[i])))
}

run_qca <- function(df, o, missing_code = NULL) {
  issues <- validate_qca(df, o)
  if (any(vapply(issues, function(i) i$severity == "error", TRUE))) api_error(422, "invalid_qca", "The QCA cannot run until the listed problems are fixed.", details = issues)
  if (!requireNamespace("QCA", quietly = TRUE)) api_error(501, "qca_unavailable", "The QCA R package is not installed on the analysis server.", "Run Rscript backend/install_packages.R.")
  t0 <- Sys.time()
  warn <- character(0)
  vars <- c(o$outcome, o$conditions)
  d <- df[, vars, drop = FALSE]
  recoded <- 0L
  if (!is.null(missing_code)) for (cn in vars) { hit <- !is.na(d[[cn]]) & d[[cn]] == missing_code; recoded <- recoded + sum(hit); d[[cn]][hit] <- NA }
  keep <- stats::complete.cases(d)
  rows <- which(keep)
  d <- d[keep, , drop = FALSE]
  n <- nrow(d)
  if (n < 10) api_error(422, "qca_too_few_rows", sprintf("Only %d complete rows remain.", n))
  safe <- setNames(qca_safe_names(vars), vars)
  map <- setNames(vars, safe)
  out_s <- safe[[o$outcome]]
  conds_s <- unname(safe[o$conditions])

  # Calibration
  calibration <- list()
  cal_s <- data.frame(row.names = as.character(rows))
  half <- 0L
  for (v in vars) {
    c0 <- o$calibration[[v]]
    cb <- tryCatch(qca_calibrate_one(d[[v]], c0), analysis_failure = function(e) api_error(422, "qca_calibration", sprintf("'%s': %s", v, conditionMessage(e)), e$hint))
    set <- cb$set
    exactly_half <- sum(abs(set - 0.5) < 1e-12)
    if (o$avoidHalf && c0$method != "crisp" && v != o$outcome && exactly_half > 0) { set[abs(set - 0.5) < 1e-12] <- 0.501; half <- half + exactly_half }
    cal_s[[safe[[v]]]] <- set
    calibration[[length(calibration) + 1]] <- list(variable = v, safeName = safe[[v]], role = if (v == o$outcome) "outcome" else "condition", method = c0$method,
      methodLabel = unname(QCA_METHODS[c0$method]), anchors = c0$anchors, percentiles = if (is.null(cb$percentiles)) NULL else as.list(cb$percentiles),
      thresholds = as.list(cb$values), multiValue = c0$method == "crisp" && length(cb$values) > 1, exactlyHalf = exactly_half,
      meanMembership = mean(set), above = sum(set > 0.5), rawMin = min(d[[v]]), rawMax = max(d[[v]]))
  }
  mv <- unname(safe[o$conditions][vapply(o$conditions, function(v) o$calibration[[v]]$method == "crisp" && length(o$calibration[[v]]$thresholds) > 1, TRUE)])
  if (half > 0) warn <- c(warn, sprintf("%d fuzzy condition membership(s) of exactly 0.5 were set to 0.501 so that these cases are not dropped from the truth table (Fiss, 2011).", half))
  if (!o$avoidHalf && any(vapply(calibration, function(c) c$role == "condition" && c$exactlyHalf > 0, TRUE)))
    warn <- c(warn, "Some fuzzy condition memberships are exactly 0.5; QCA cannot place these cases in a truth-table row.")
  n_cut <- o$nCut %||% (if (n >= 150) 2L else 1L)

  necessity <- list(qca_necessity(cal_s, out_s, conds_s, o, map, mv))
  sufficiency <- list(qca_sufficiency(cal_s, out_s, conds_s, o, n_cut, map, mv))
  if (o$negateOutcome) {
    necessity[[2]] <- qca_necessity(cal_s, out_s, conds_s, o, map, mv, neg = TRUE)
    sufficiency[[2]] <- qca_sufficiency(cal_s, out_s, conds_s, o, n_cut, map, mv, neg = TRUE)
  }
  for (s in sufficiency) if (!is.null(s$message)) warn <- c(warn, sprintf("%s: %s", s$outcome, s$message))
  tobit <- if (isTRUE(o$tobit)) qca_tobit(cal_s, out_s, conds_s, map, mv, sufficiency[[1]], o) else NULL
  slow <- as.numeric(difftime(Sys.time(), t0, units = "secs")) > 20
  sensitivity <- if (!isTRUE(o$sensitivity)) NULL
                 else if (slow) list(tests = list(), robust = FALSE, message = "The sensitivity analysis was skipped because this model takes long to minimise; run it with fewer conditions.")
                 else qca_sensitivity(d, vars, safe, cal_s, out_s, conds_s, o, n_cut, map, mv, sufficiency[[1]])
  slow <- as.numeric(difftime(Sys.time(), t0, units = "secs")) > 25
  predictive <- if (!isTRUE(o$predictive)) NULL
                else if (slow) list(directions = list(), holds = FALSE, message = "The predictive validity test was skipped because this model takes long to minimise; run it with fewer conditions.")
                else qca_predictive(cal_s, out_s, conds_s, o, map, mv, negs = c(FALSE, if (o$negateOutcome) TRUE))
  share <- sufficiency[[1]]$casesInRows / n
  if (share < 0.8) warn <- c(warn, sprintf("Only %.0f%% of the cases are in truth-table rows that meet the frequency threshold (%d); at least 75-80%% is usually recommended.", 100 * share, n_cut))
  setsTab <- cal_s; names(setsTab) <- unname(map[names(cal_s)])
  sets_out <- if (n <= 5000) list(rows = as.list(rows), values = lapply(setsTab, as.numeric)) else NULL
  out <- list(
    kind = "qca",
    options = utils::modifyList(o, list(conditions = as.list(o$conditions))),
    setType = if (length(mv)) "mvQCA" else if (all(vapply(o$calibration, function(c) c$method == "crisp", TRUE))) "csQCA" else "fsQCA",
    n = list(input = nrow(df), used = n, rowsRemoved = nrow(df) - n, cellsRecoded = recoded),
    nCut = n_cut,
    calibration = calibration,
    necessity = lapply(seq_along(necessity), function(i) list(outcome = if (i == 1) o$outcome else paste0("~", o$outcome), rows = necessity[[i]])),
    sufficiency = sufficiency,
    sets = sets_out,
    tobit = tobit,
    sensitivity = sensitivity,
    predictive = predictive,
    warnings = as.list(unique(warn)),
    rCode = paste(c(qca_code(o, safe, n_cut, missing_code, mv), if (!is.null(tobit) && length(tobit$models)) qca_tobit_code(o, safe, mv),
                    if (!is.null(predictive) && length(predictive$directions)) qca_predictive_code(o, safe, mv)), collapse = "
"),
    packages = c(list(QCA = as.character(utils::packageVersion("QCA"))), if (!is.null(tobit$package)) list(VGAM = tobit$package)),
    durationSeconds = as.numeric(difftime(Sys.time(), t0, units = "secs")),
    .derived = list(nInput = nrow(df), rows = rows, tables = list(sets = setsTab))
  )
  out$checklist <- qca_checklist(out)
  out
}

qca_code <- function(o, safe, n_cut, missing_code, mv) {
  q <- function(x) paste0("\"", gsub("\"", "\\\\\"", x), "\"")
  vars <- names(safe)
  num <- function(x) paste(vapply(x, function(v) format(v, digits = 15), ""), collapse = ", ")
  lines <- c("# Qualitative comparative analysis generated by PathLab SEM (QCA package)", "library(QCA)",
             "# data <- read.csv(\"your_data.csv\")   # the same dataset as in the app",
             paste0("d <- data[, c(", paste(vapply(vars, q, ""), collapse = ", "), ")]"))
  if (!is.null(missing_code)) lines <- c(lines, paste0("d[d == ", missing_code, "] <- NA   # project missing-value code"))
  lines <- c(lines, "d <- d[complete.cases(d), ]",
             paste0("cal <- data.frame(row.names = rownames(d))   # calibrated sets (names as used by QCA: ", paste(sprintf("%s = %s", unname(safe), vars), collapse = "; "), ")"))
  for (v in vars) {
    c0 <- o$calibration[[v]]
    th <- if (c0$anchors == "percentile" && c0$method != "none") paste0("quantile(d[[", q(v), "]], c(", num(c0$thresholds), "), names = FALSE)") else paste0("c(", num(c0$thresholds), ")")
    lines <- c(lines, switch(c0$method,
      fuzzy = paste0("cal$", safe[[v]], " <- calibrate(d[[", q(v), "]], type = \"fuzzy\", method = \"direct\", thresholds = ", th, ")"),
      crisp = paste0("cal$", safe[[v]], " <- calibrate(d[[", q(v), "]], type = \"crisp\", thresholds = ", th, ")"),
      none = paste0("cal$", safe[[v]], " <- d[[", q(v), "]]   # already calibrated")))
  }
  conds <- unname(safe[o$conditions]); out <- safe[[o$outcome]]
  fz <- conds[vapply(o$conditions, function(v) o$calibration[[v]]$method != "crisp", TRUE)]
  if (o$avoidHalf && length(fz)) lines <- c(lines, paste0("for (v in c(", paste(vapply(fz, q, ""), collapse = ", "), ")) cal[[v]][cal[[v]] == 0.5] <- 0.501   # avoid memberships of exactly 0.5 (Fiss, 2011)"))
  dir <- unlist(lapply(o$conditions, function(v) { e <- o$expectations[[v]]; s <- safe[[v]]; if (s %in% mv || e == "either") NULL else if (e == "absent") paste0("~", s) else s }))
  outs <- c(out, if (o$negateOutcome) paste0("~", out))
  for (y in outs) {
    lines <- c(lines, "", sprintf("# Outcome %s", y),
               paste0("sets <- cbind(cal[, c(", paste(vapply(setdiff(conds, mv), q, ""), collapse = ", "), "), drop = FALSE], 1 - cal[, c(", paste(vapply(setdiff(conds, mv), q, ""), collapse = ", "), "), drop = FALSE])"),
               paste0("names(sets) <- c(", paste(vapply(c(setdiff(conds, mv), paste0("~", setdiff(conds, mv))), q, ""), collapse = ", "), ")"),
               sprintf("pof(sets, %scal$%s, relation = \"necessity\")   # necessity of each condition and its negation", if (startsWith(y, "~")) "1 - " else "", out),
               sprintf("tt <- truthTable(cal, outcome = %s, conditions = c(%s), incl.cut = %s, pri.cut = %s, n.cut = %d, show.cases = TRUE, sort.by = c(\"OUT\", \"incl\"))",
                       q(y), paste(vapply(conds, q, ""), collapse = ", "), o$inclCut, o$priCut, n_cut),
               unlist(lapply(Filter(function(x) identical(x$outcome, if (startsWith(y, "~")) paste0("~", o$outcome) else o$outcome), o$rowOverrides),
                             function(x) sprintf("tt$tt[\"%d\", \"OUT\"] <- \"%s\"   # outcome coded by hand for this row", x$row, x$out))),
               "tt",
               "minimize(tt, details = TRUE)                     # complex (conservative) solution",
               "minimize(tt, include = \"?\", details = TRUE)       # parsimonious solution",
               if (length(dir)) sprintf("minimize(tt, include = \"?\", dir.exp = c(%s), details = TRUE)   # intermediate solution", paste(vapply(dir, q, ""), collapse = ", ")))
  }
  paste(lines, collapse = "\n")
}

# ---- Report, workbook and APA tables (from the stored result) ---------------------------------------------

QCA_CHART_SYMBOL <- c(core_present = "●", peripheral_present = "•", core_absent = "⊗", peripheral_absent = "⊘", none = "")
QCA_SOLUTIONS <- c(complex = "Complex solution", parsimonious = "Parsimonious solution", intermediate = "Intermediate solution")

#' Solution type to report: intermediate, else parsimonious, else complex (NA when there is none).
qca_first_kind <- function(sl) { k <- intersect(c("intermediate", "parsimonious", "complex"), names(sl) %||% character(0)); if (length(k)) k[1] else NA_character_ }

#' Chart symbol of a cell; multi-value cells ("core_2") show the category, marked as core (bold) or peripheral.
qca_chart_sym <- function(cell) {
  cell <- cell %||% "none"
  if (cell %in% names(QCA_CHART_SYMBOL)) return(unname(QCA_CHART_SYMBOL[[cell]]))
  sub("^(core|peripheral)_(.*)$", "\\2 (\\1)", cell)
}

qca_anchor_text <- function(c, i, html = TRUE) {
  th <- num_of(unlist(c$thresholds))
  pc <- num_of(unlist(c$percentiles))
  one <- function(k) {
    v <- formatC(th[k], format = "f", digits = 2)
    if (length(pc) >= k && !is.na(pc[k])) paste0(v, " (", round(100 * pc[k]), "th pct.)") else v
  }
  if (c$method == "fuzzy") return(c(one(3), one(2), one(1)))                # fsQCA order: full membership, crossover, full non-membership
  if (c$method == "crisp") return(c(paste(vapply(seq_along(th), one, ""), collapse = "; "), "", ""))
  c("already calibrated", "", "")
}

qca_solution_list <- function(s) {
  out <- list()
  if (length(s$complex)) out$complex <- s$complex
  if (length(s$parsimonious)) out$parsimonious <- s$parsimonious
  if (length(s$intermediate)) out$intermediate <- s$intermediate[[1]]$models
  out
}

qca_body_html <- function(res, flow = NULL) {
  esc <- html_escape
  o <- res$options
  conds <- ul(o$conditions)
  S <- character(0); a <- function(...) S <<- c(S, paste0(...))
  a("<h2>Qualitative comparative analysis (", esc(res$setType), ")</h2>")
  a("<p>Model: <strong>", esc(o$outcome), " = f(", esc(paste(conds, collapse = ", ")), ")</strong>; n = ", res$n$used,
    if (res$n$rowsRemoved > 0) paste0(" (", res$n$rowsRemoved, " row(s) with missing values left out)") else "",
    ". Calibration, necessity, truth table and Boolean minimisation with the QCA package ", esc(res$packages$QCA), ".</p>")
  for (w in ul(res$warnings)) a("<div class=\"warn\">", esc(w), "</div>")
  a(qca_flowchart_html(res, flow))
  if (length(res$checklist)) a(qca_writeup(res))
  nu <- res$n$used
  a(html_table(c("Set", "Role", "Calibration", "Full membership", "Crossover", "Full non-membership", "Mean membership", "Cases &gt; 0.5"),
               lapply(res$calibration, function(c) c(esc(c$variable), c$role, esc(c$methodLabel), esc(qca_anchor_text(c)), fmt(c$meanMembership),
                                                     flag(sprintf("%d (%.0f%%)", as.integer(c$above), 100 * c$above / nu), c$above / nu < 0.2 || c$above / nu > 0.8))),
               "Calibration (anchors as values; percentiles of the observed variable in parentheses; red: skewed set, fewer than 20% or more than 80% of the cases above 0.5)", num_cols = 7:8))
  for (nb in res$necessity) {
    a(html_table(c("Condition", "Consistency", "Coverage", "Relevance (RoN)", "Sufficiency consistency", "Sufficiency coverage"),
                 lapply(nb$rows, function(x) c(esc(x$condition), if (isTRUE(x$necessary)) paste0("<strong>", fmt(x$inclN), "</strong>") else fmt(x$inclN),
                                               fmt(x$covN), flag(fmt(x$RoN), lt(x$RoN, 0.5)), fmt(x$inclS), fmt(x$covS))),
                 sprintf("Analysis of necessary conditions, outcome %s (bold: consistency &ge; %s)", esc(nb$outcome), fmt(o$necessityCut, 2)), num_cols = 2:6))
  }
  for (s in res$sufficiency) {
    a("<h3>Truth table analysis: outcome ", esc(s$outcome), "</h3>")
    rows <- Filter(function(r) r$n >= s$nCut, s$rows)
    a(html_table(c(esc(conds), "Number", "Outcome", "Raw consistency", "PRI consistency", "Cases (data rows)"),
                 lapply(rows, function(r) c(vapply(conds, function(cn) format(r$values[[cn]]), ""), as.character(r$n),
                                            paste0(r$out, if (r$row %in% ul(s$overridden)) " (coded by hand)" else ""), fmt(r$incl), fmt(r$PRI), esc(r$cases %||% ""))),
                 sprintf("Truth table rows with at least %d case(s) (%d of %d possible configurations observed; %d logical remainders)", s$nCut, s$nRowsObserved, as.integer(s$nRowsPossible), as.integer(s$nRemainders)),
                 num_cols = seq_len(length(conds) + 4)))
    lowpri <- Filter(function(r) r$n >= s$nCut && isTRUE(r$incl >= o$inclCut) && isTRUE(r$PRI < o$priCut), s$rows)
    if (length(lowpri)) a("<p class=\"note\">Row(s) ", paste(vapply(lowpri, function(r) as.character(r$row), ""), collapse = ", "),
                          " meet the raw consistency threshold but not the PRI threshold, so they were coded 0 (possible simultaneous subset relations).</p>")
    if (!is.null(s$message)) { a("<div class=\"warn\">", esc(s$message), "</div>"); next }
    for (k in names(qca_solution_list(s))) {
      models <- qca_solution_list(s)[[k]]
      a("<h4>", QCA_SOLUTIONS[[k]], "</h4>")
      a("<pre><code>", esc(paste0("Model: ", s$outcome, " = f(", paste(conds, collapse = ", "), ")\n",
                                  "frequency cutoff: ", s$nCut, "\nconsistency cutoff: ", formatC(num_of(s$consistencyCutoffUsed), format = "f", digits = 6),
                                  if (k == "intermediate") paste0("\nAssumptions: ", paste(vapply(conds, function(cn) { e <- o$expectations[[cn]] %||% "present"; if (e == "either") paste(cn, "(present or absent)") else if (e == "absent") paste0("~", cn, " (absent)") else paste(cn, "(present)") }, ""), collapse = ", ")) else "")),
        "</code></pre>")
      for (m in models) {
        trs <- lapply(m$terms, function(t) c(esc(t$term), fmt(t$covS), fmt(t$covU), fmt(t$inclS), fmt(t$PRI), flag(fmt(t$inclSNeg), isTRUE(t$inclSNeg >= o$inclCut))))
        trs[[length(trs) + 1]] <- c("<strong>Solution</strong>", paste0("<strong>", fmt(m$covS), "</strong>"), "", paste0("<strong>", fmt(m$inclS), "</strong>"), fmt(m$PRI), fmt(m$inclSNeg))
        a(html_table(c("Configuration", "Raw coverage", "Unique coverage", "Consistency", "PRI", sprintf("Consistency for ~%s", esc(sub("^~", "", s$outcome)))), trs,
                     sprintf("%s%s: %s", QCA_SOLUTIONS[[k]], if (length(models) > 1) sprintf(", model M%d", m$model) else "", esc(m$expression)), num_cols = 2:6))
      }
    }
    if (length(s$chart)) {
      hdr <- c("Condition", sprintf("Configuration %d", seq_along(s$chart)))
      crow <- lapply(conds, function(cn) c(esc(cn), vapply(s$chart, function(ch) esc(qca_chart_sym(ch$cells[[cn]])), "")))
      m1 <- (qca_solution_list(s)$intermediate %||% qca_solution_list(s)$complex)[[1]]
      crow <- c(crow, list(c("Consistency", vapply(s$chart, function(ch) fmt(ch$inclS), "")), c("PRI", vapply(s$chart, function(ch) fmt(ch$PRI), "")),
                           c("Raw coverage", vapply(s$chart, function(ch) fmt(ch$covS), "")), c("Unique coverage", vapply(s$chart, function(ch) fmt(ch$covU), "")),
                           c("Overall solution consistency", fmt(m1$inclS), rep("", length(s$chart) - 1)), c("Overall solution coverage", fmt(m1$covS), rep("", length(s$chart) - 1))))
      a(html_table(hdr, crow, "Configurations for the outcome (Ragin &amp; Fiss, 2008)", num_cols = 2:length(hdr)))
      a("<p class=\"note\">&#9679; core condition present, &#8226; peripheral condition present, &#8855; core condition absent, &#8856; peripheral condition absent; blank: does not matter. Core conditions are part of the parsimonious solution; peripheral conditions appear only in the intermediate solution.</p>")
    }
  }
  a(qca_sensitivity_html(res$sensitivity))
  a(qca_predictive_html(res$predictive))
  a(qca_tobit_html(res$tobit))
  if (length(res$checklist)) a(qca_checklist_html(res$checklist))
  a(qca_references_html(res))
  a("<h3>R code</h3><pre><code>", esc(res$rCode %||% ""), "</code></pre>")
  paste(S, collapse = "\n")
}

qca_sheets <- function(res, project_name) {
  o <- res$options
  conds <- ul(o$conditions)
  cal <- do.call(rbind, lapply(res$calibration, function(c) {
    th <- num_of(unlist(c$thresholds)); pc <- num_of(unlist(c$percentiles))
    data.frame(Set = c$variable, Role = c$role, Method = c$methodLabel, Anchors = c$anchors,
               Threshold1 = th[1], Threshold2 = if (length(th) > 1) th[2] else NA, Threshold3 = if (length(th) > 2) th[3] else NA,
               Percentile1 = if (length(pc)) pc[1] else NA, Percentile2 = if (length(pc) > 1) pc[2] else NA, Percentile3 = if (length(pc) > 2) pc[3] else NA,
               MeanMembership = num_of(c$meanMembership), CasesAbove05 = c$above, ExactlyHalf = c$exactlyHalf, stringsAsFactors = FALSE)
  }))
  nec <- do.call(rbind, lapply(res$necessity, function(nb) do.call(rbind, lapply(nb$rows, function(x)
    data.frame(Outcome = nb$outcome, Condition = x$condition, ConsistencyN = num_of(x$inclN), CoverageN = num_of(x$covN), RoN = num_of(x$RoN),
               ConsistencyS = num_of(x$inclS), PRI = num_of(x$PRI), CoverageS = num_of(x$covS), stringsAsFactors = FALSE)))))
  tt <- do.call(rbind, lapply(res$sufficiency, function(s) do.call(rbind, lapply(s$rows, function(r) {
    d <- data.frame(Outcome = s$outcome, Row = r$row, stringsAsFactors = FALSE)
    for (cn in conds) d[[cn]] <- num_of(r$values[[cn]])
    d$Number <- r$n; d$OUT <- r$out; d$CodedByHand <- r$row %in% ul(s$overridden); d$RawConsistency <- num_of(r$incl); d$PRI <- num_of(r$PRI); d$Cases <- r$cases %||% ""
    d
  }))))
  sol <- do.call(rbind, lapply(res$sufficiency, function(s) {
    sl <- qca_solution_list(s)
    if (!length(sl)) return(NULL)
    do.call(rbind, lapply(names(sl), function(k) do.call(rbind, lapply(sl[[k]], function(m) rbind(
      do.call(rbind, lapply(m$terms, function(t) data.frame(Outcome = s$outcome, Solution = QCA_SOLUTIONS[[k]], Model = m$model, Term = t$term, RawCoverage = num_of(t$covS),
                                                          UniqueCoverage = num_of(t$covU), Consistency = num_of(t$inclS), PRI = num_of(t$PRI), stringsAsFactors = FALSE))),
      data.frame(Outcome = s$outcome, Solution = QCA_SOLUTIONS[[k]], Model = m$model, Term = paste("Solution:", m$expression), RawCoverage = num_of(m$covS),
                 UniqueCoverage = NA, Consistency = num_of(m$inclS), PRI = num_of(m$PRI), stringsAsFactors = FALSE))))))
  }))
  list(Info = data.frame(Item = c("Project", "Completed (UTC)", "PathLab SEM version", "QCA", "Set type", "Outcome", "Conditions", "Rows used", "Rows in dataset",
                                  "Consistency threshold", "PRI threshold", "Frequency threshold", "0.5 memberships shifted"),
                         Value = c(project_name, res$completedAt, PATHLAB_VERSION, res$packages$QCA, res$setType, o$outcome, paste(conds, collapse = ", "), res$n$used, res$n$input,
                                   o$inclCut, o$priCut, res$nCut, isTRUE(o$avoidHalf)), stringsAsFactors = FALSE),
       Calibration = cal, Necessity = nec, TruthTable = tt, Solutions = if (is.null(sol)) data.frame(Note = "No sufficient configuration") else sol,
       Tobit = qca_tobit_sheet(res$tobit),
       Sensitivity = if (is.null(res$sensitivity) || !length(res$sensitivity$tests)) data.frame(Note = "Sensitivity analysis was switched off") else
         do.call(rbind, lapply(res$sensitivity$tests, function(t) data.frame(Test = t$test, Change = t$change, Solution = t$solution, Consistency = num_of(t$inclS),
                                                                               Coverage = num_of(t$covS), SameSolution = isTRUE(t$same), OriginalConfigurationsKept = t$kept, stringsAsFactors = FALSE))),
       PredictiveValidity = qca_predictive_sheet(res$predictive),
       Checklist = if (!length(res$checklist)) data.frame(Note = "Not available") else
         do.call(rbind, lapply(res$checklist, function(x) data.frame(Status = x$status, Item = x$item, Detail = x$detail, Source = x$source, stringsAsFactors = FALSE))))
}

qca_pub_spec <- function(res, opts) {
  d <- opts$decimals; lz <- opts$leadingZero
  o <- res$options
  conds <- ul(o$conditions)
  bnum <- function(v) pt_num(v, d, TRUE, lz)
  tabs <- list()
  tabs[[1]] <- pt_table("qca_calibration", "Calibration Anchors of the Sets", c("Set", "Full membership", "Crossover", "Full non-membership"),
                        lapply(res$calibration, function(c) c(paste0(c$variable, if (c$role == "outcome") " (outcome)" else ""), qca_anchor_text(c, html = FALSE))),
                        notes = list(sprintf("N = %d. Direct method of calibration (Ragin, 2008) with the QCA package %s; percentiles of the observed variable in parentheses.%s",
                                             as.integer(res$n$used), res$packages$QCA,
                                             if (isTRUE(o$avoidHalf)) " Condition memberships of exactly 0.5 were set to 0.501 (Fiss, 2011)." else "")))
  nec_rows <- list(); span <- integer(0)
  for (nb in res$necessity) {
    if (length(res$necessity) > 1) { nec_rows[[length(nec_rows) + 1]] <- c(sprintf("Outcome: %s", nb$outcome), "", "", ""); span <- c(span, length(nec_rows)) }
    for (x in nb$rows) nec_rows[[length(nec_rows) + 1]] <- c(x$condition, bnum(x$inclN), bnum(x$covN), bnum(x$RoN))
  }
  tabs[[2]] <- pt_table("qca_necessity", "Analysis of Necessary Conditions", c("Condition", "Consistency", "Coverage", "RoN"), nec_rows, span = span,
                        notes = list(sprintf("~ denotes the absence (negation) of a condition. Conditions with a consistency of %s or higher are usually considered necessary; RoN = relevance of necessity.", bnum(o$necessityCut))))
  for (s in res$sufficiency) {
    if (!length(s$chart)) next
    ch <- s$chart
    rows <- c(lapply(conds, function(cn) c(cn, vapply(ch, function(x) qca_chart_sym(x$cells[[cn]]), ""))),
              list(c("Consistency", vapply(ch, function(x) bnum(x$inclS), "")), c("PRI", vapply(ch, function(x) bnum(x$PRI), "")), c("Raw coverage", vapply(ch, function(x) bnum(x$covS), "")),
                   c("Unique coverage", vapply(ch, function(x) bnum(x$covU), ""))))
    sl <- qca_solution_list(s)
    m1 <- (sl$intermediate %||% sl$complex)[[1]]
    tabs[[length(tabs) + 1]] <- pt_table(paste0("qca_chart", if (isTRUE(s$negated)) "_neg" else ""), sprintf("Configurations Sufficient for %s", if (isTRUE(s$negated)) paste("the Absence of", o$outcome) else o$outcome),
      c("Condition", sprintf("%d", seq_along(ch))), rows,
      notes = list(sprintf("● = core condition present; • = peripheral condition present; ⊗ = core condition absent; ⊘ = peripheral condition absent; blank = does not matter. Overall solution consistency = %s; overall solution coverage = %s. Consistency cut-off %s (lowest consistency of a row coded 1), PRI cut-off %s, frequency cut-off %d.",
                           bnum(m1$inclS), bnum(m1$covS), bnum(s$consistencyCutoffUsed), bnum(o$priCut), as.integer(s$nCut))))
  }
  sv <- res$sensitivity
  if (!is.null(sv) && length(sv$tests))
    tabs[[length(tabs) + 1]] <- pt_table("qca_sensitivity", "Sensitivity of the Solution to the Analytic Choices", c("Test", "Change", "Solution", "Consistency", "Coverage", "Same solution"),
      lapply(sv$tests, function(t) c(t$test, t$change, t$solution, bnum(t$inclS), bnum(t$covS), if (isTRUE(t$same)) "Yes" else sprintf("No (%d kept)", t$kept))),
      notes = list(sprintf("Original %s solution: %s. Each test changes one analytic choice and repeats the truth table analysis (Oana & Schneider, 2024).", sv$baseKind %||% "", sv$base %||% "none")))
  pv <- res$predictive
  if (!is.null(pv) && length(pv$directions)) for (dd in Filter(function(x) length(x$terms) > 0, pv$directions))
    tabs[[length(tabs) + 1]] <- pt_table(paste0("qca_predictive_", if (startsWith(dd$outcome, "~")) "neg_" else "", dd$model), sprintf("Predictive Validity for %s: Model From Subsample %s Tested on Subsample %s", dd$outcome, dd$model, dd$test),
      c("Configuration", sprintf("Consistency (%s)", dd$model), sprintf("Coverage (%s)", dd$model), sprintf("Consistency (%s)", dd$test), sprintf("Coverage (%s)", dd$test)),
      c(lapply(dd$terms, function(t) c(t$term, bnum(t$modelInclS), bnum(t$modelCovS), bnum(t$testInclS), bnum(t$testCovS))),
        list(c("Overall solution", bnum(dd$modelInclS), bnum(dd$modelCovS), bnum(dd$testInclS), bnum(dd$testCovS)))),
      notes = list(sprintf("Random split of the cases (seed %d): subsample A n = %d, subsample B n = %d. The solution was derived on subsample %s and its configurations were evaluated on subsample %s (Woodside, 2014; Pappas & Woodside, 2021).",
                           as.integer(pv$seed), as.integer(pv$nA), as.integer(pv$nB), dd$model, dd$test)))
  tb <- res$tobit
  if (!is.null(tb) && length(tb$models)) for (m in tb$models) {
    rows <- lapply(m$coefs, function(k) c(if (!is.null(k$expression)) sprintf("%s (%s)", k$term, k$expression) else k$term,
                                          paste0(pt_num(k$estimate, d, FALSE, lz), qca_stars(k$p)), pt_num(k$se, d, FALSE, lz), pt_num(k$z, d, FALSE, lz), pt_num(k$p, d, TRUE, lz)))
    lrp <- if (isTRUE(m$lrP < 0.001)) "< .001" else paste("=", pt_num(m$lrP, 3, TRUE, lz))
    tabs[[length(tabs) + 1]] <- pt_table(paste0("qca_tobit_", m$id), sprintf("Tobit Regression of %s on the %s", o$outcome, if (m$id == "pathways") "Configurations" else "Calibrated Conditions"),
      c("Variables", "Coefficient", "Std. Error", "z-Value", "Probability"), rows,
      notes = list(sprintf("*p < .05, **p < .01, ***p < .001. N = %d. Tobit model censored at 0 and 1 (VGAM %s); LR χ²(%d) = %s, p %s; σ = %s.%s",
                           as.integer(m$n), tb$package, as.integer(m$df), pt_num(m$lrChi2, 2, FALSE, lz), lrp, pt_num(m$sigma, d, FALSE, lz),
                           if (m$id == "pathways") sprintf(" Solutions are the configurations of the %s solution.", tb$solution) else "")))
  }
  tabs <- Filter(Negate(is.null), tabs)
  for (i in seq_along(tabs)) tabs[[i]]$number <- i
  tabs
}

# ---- Flow chart of the QCA process (report figure) ---------------------------------------------
# Default colours of the flow chart; the Report tab lets the user change each one.
QCA_FLOW_COLORS <- c(terminal = "#5b7cfa", decision = "#ffd166", step = "#f8766d", robust = "#f9a65a", group = "#fdf1e4")

qca_flow_colors <- function(x) {
  out <- QCA_FLOW_COLORS
  if (is.list(x) || is.character(x)) for (k in names(out)) {
    v <- x[[k]]
    if (is.character(v) && length(v) == 1 && grepl("^#[0-9a-fA-F]{6}$", v)) out[[k]] <- tolower(v)
  }
  out
}

# Dark or white text, whichever reads better on the fill.
qca_flow_ink <- function(hex) {
  rgb <- grDevices::col2rgb(hex)[, 1] / 255
  lin <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  if (sum(c(0.2126, 0.7152, 0.0722) * lin) > 0.36) "#1f2733" else "#ffffff"
}

#' SVG flow chart of the QCA process, labelled with the settings of this run.
#' @param flow list(colors = list(terminal, decision, step, robust, group), robustness = TRUE/FALSE)
qca_flowchart_svg <- function(res, flow = NULL) {
  esc <- html_escape
  col <- qca_flow_colors(flow$colors)
  # Robustness checks: the Tobit regression when it was run, plus the user's own checks when switched on.
  tob <- !is.null(res$tobit) && length(res$tobit$models) > 0
  sens <- !is.null(res$sensitivity) && length(res$sensitivity$tests) > 0
  rboxes <- c(if (sens) "Sensitivity analysis", if (tob) "Tobit regression", if (isTRUE(flow$robustness)) c(if (!sens) "Sensitivity analysis", "Predictive analysis", "Post hoc analysis"))
  robust <- length(rboxes) > 0
  o <- res$options
  s <- if (length(res$sufficiency)) res$sufficiency[[1]] else NULL
  methods <- unique(vapply(res$calibration, function(c) as.character(c$methodLabel %||% ""), ""))
  sols <- if (is.null(s)) list() else qca_solution_list(s)
  sol_k <- qca_first_kind(sols)
  n_terms <- if (is.na(sol_k)) 0L else length(sols[[sol_k]][[1]]$terms)
  W <- if (robust) 700 else 520; H <- max(384, 140 + 34 + length(rboxes) * 54 + 10)
  bw <- 190; bh <- 46
  S <- character(0); a <- function(...) S <<- c(S, paste0(...))
  label <- function(cx, cy, main, sub, ink) {
    if (nzchar(sub)) {
      a(sprintf('<text x="%g" y="%g" text-anchor="middle" font-size="12.5" font-weight="700" fill="%s">%s</text>', cx, cy - 3, ink, esc(main)))
      a(sprintf('<text x="%g" y="%g" text-anchor="middle" font-size="10.5" fill="%s">%s</text>', cx, cy + 12, ink, esc(sub)))
    } else a(sprintf('<text x="%g" y="%g" text-anchor="middle" font-size="12.5" font-weight="700" fill="%s">%s</text>', cx, cy + 4, ink, esc(main)))
  }
  box <- function(cx, cy, main, sub = "", kind = "step", w = bw) {
    r <- if (kind == "terminal") 12 else 3
    a(sprintf('<rect x="%g" y="%g" width="%g" height="%g" rx="%g" fill="%s" stroke="#00000022"/>', cx - w / 2, cy - bh / 2, w, bh, r, col[[kind]]))
    label(cx, cy, main, sub, qca_flow_ink(col[[kind]]))
  }
  diamond <- function(cx, cy, main, sub = "") {
    dx <- 110; dy <- 46
    a(sprintf('<polygon points="%g,%g %g,%g %g,%g %g,%g" fill="%s" stroke="#00000022"/>', cx, cy - dy, cx + dx, cy, cx, cy + dy, cx - dx, cy, col[["decision"]]))
    label(cx, cy, main, sub, qca_flow_ink(col[["decision"]]))
  }
  arrow <- function(pts) a(sprintf('<polyline points="%s" fill="none" stroke="#2b3440" stroke-width="1.4" marker-end="url(#qf-arrow)"/>',
                                   paste(vapply(pts, function(p) paste(p, collapse = ","), ""), collapse = " ")))
  a(sprintf('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %d %d" width="%d" height="%d" font-family="Segoe UI, Arial, sans-serif" role="img" aria-label="Flow chart of the %s process" style="max-width:100%%;height:auto">', W, H, W, H, esc(res$setType)))
  a('<defs><marker id="qf-arrow" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse"><path d="M0,0 L10,5 L0,10 z" fill="#2b3440"/></marker></defs>')
  a(sprintf('<rect width="%d" height="%d" fill="#ffffff"/>', W, H))
  L <- 125; R <- 400
  # Left column: sample to truth table analysis.
  box(L, 34, "Sample", sprintf("n = %s", res$n$used), "terminal")
  diamond(L, 112, "Choose thresholds", sprintf("consistency %s · PRI %s", fmt(o$inclCut, 2), fmt(o$priCut, 2)))
  box(L, 196, "Data calibration", paste(methods, collapse = ", "))
  box(L, 266, "Necessary condition test", sprintf("consistency ≥ %s", fmt(o$necessityCut, 2)))
  box(L, 336, "Truth table analysis", if (is.null(s)) "" else sprintf("%d of %d rows observed · n ≥ %s", s$nRowsObserved, as.integer(s$nRowsPossible), s$nCut))
  arrow(list(c(L, 57), c(L, 64))); arrow(list(c(L, 158), c(L, 172))); arrow(list(c(L, 219), c(L, 241))); arrow(list(c(L, 289), c(L, 311)))
  # Middle column: sorting, solution, findings, interpretation.
  diamond(R, 336, "Truth table sorting", "consistency, PRI, n")
  arrow(list(c(L + bw / 2, 336), c(R - 110, 336)))
  box(R, 196, "Configuration solution", if (is.na(sol_k)) "no sufficient configuration" else sprintf("%s: %d pathway%s", tolower(sub(" solution", "", QCA_SOLUTIONS[[sol_k]])), n_terms, if (n_terms == 1) "" else "s"))
  arrow(list(c(R, 290), c(R, 220)))
  box(R, 112, paste(res$setType, "findings"), "")
  box(R, 34, "Findings interpretation", "", "terminal")
  a(sprintf('<line x1="%g" y1="%g" x2="%g" y2="%g" stroke="#2b3440" stroke-width="1.4"/>', R, 89, R, 57))
  if (robust) {
    gx <- 515; gw <- 170; gy <- 140; gh <- 34 + length(rboxes) * 54
    a(sprintf('<rect x="%g" y="%g" width="%g" height="%g" rx="12" fill="%s" stroke="%s" stroke-dasharray="5 4"/>', gx, gy, gw, gh, col[["group"]], col[["robust"]]))
    a(sprintf('<text x="%g" y="%g" text-anchor="middle" font-size="10.5" fill="#5a4630">Robustness checks</text>', gx + gw / 2, gy + 16))
    bwr <- 152
    for (i in seq_along(rboxes)) box(gx + gw / 2, gy + 50 + (i - 1) * 54, rboxes[i], if (rboxes[i] == "Tobit regression") "VGAM, censored at 0 and 1" else if (rboxes[i] == "Sensitivity analysis" && sens) sprintf("%d alternative settings", length(res$sensitivity$tests)) else "", "robust", w = bwr)
    arrow(list(c(R + bw / 2, 196), c(gx, 196)))
    arrow(list(c(gx + gw / 2, gy), c(gx + gw / 2, 112), c(R + bw / 2, 112)))
  } else arrow(list(c(R, 173), c(R, 135)))
  a("</svg>")
  paste(S, collapse = "")
}

qca_flowchart_html <- function(res, flow = NULL) {
  paste0("<h3>Flow chart of the ", html_escape(res$setType), " process</h3><div class=\"diagram\">", qca_flowchart_svg(res, flow), "</div>",
         "<p class=\"note\"><strong>Figure.</strong> Flow chart of the ", html_escape(res$setType), " process applied in this analysis",
         if ((!is.null(res$tobit) && length(res$tobit$models)) || (!is.null(res$sensitivity) && length(res$sensitivity$tests))) "; the robustness checks run in PathLab are reported below" else "",
         if (isTRUE(flow$robustness)) "; the other robustness checks are reported separately" else "", ".</p>")
}

# ---- Tobit regression as a robustness check (VGAM) ------------------------------------------------------
# The outcome membership is bounded by 0 and 1, so it is modelled as censored at both limits:
# VGAM::vglm(y ~ x, tobit(Lower = 0, Upper = 1)). Model 1 uses the calibrated conditions, model 2 the membership in
# each pathway (term) of the solution. Positive, significant coefficients agree with the set-theoretic result.

qca_tobit_fit <- function(y, X, id, label) {
  d <- data.frame(y = y, X, check.names = FALSE)
  safe <- paste0("x", seq_len(ncol(X)))
  names(d) <- c("y", safe)
  f <- stats::as.formula(paste("y ~", paste(safe, collapse = " + ")))
  fit <- suppressWarnings(VGAM::vglm(f, VGAM::tobit(Lower = 0, Upper = 1), data = d))
  fit0 <- suppressWarnings(VGAM::vglm(y ~ 1, VGAM::tobit(Lower = 0, Upper = 1), data = d))
  cf <- methods::slot(VGAM::summary(fit), "coef3")
  lab <- setNames(colnames(X), safe)
  rows <- lapply(seq_len(nrow(cf)), function(i) {
    rn <- rownames(cf)[i]
    if (rn == "(Intercept):2") return(NULL)                        # log(sigma), reported as sigma below
    est <- unname(cf[i, 1]); se <- unname(cf[i, 2])
    list(term = if (rn == "(Intercept):1") "(Intercept)" else unname(lab[[rn]]), estimate = est, se = se, z = unname(cf[i, 3]), p = unname(cf[i, 4]),
         ciLow = est - stats::qnorm(0.975) * se, ciHigh = est + stats::qnorm(0.975) * se)
  })
  ll <- as.numeric(stats::logLik(fit)); ll0 <- as.numeric(stats::logLik(fit0))
  lr <- 2 * (ll - ll0); df <- ncol(X)
  list(id = id, label = label, n = length(y), censoredLow = sum(y <= 0), censoredHigh = sum(y >= 1),
       sigma = exp(unname(cf["(Intercept):2", 1])), logLik = ll, logLik0 = ll0, lrChi2 = lr, df = df,
       lrP = stats::pchisq(lr, df, lower.tail = FALSE), aic = as.numeric(VGAM::AIC(fit)), coefs = Filter(Negate(is.null), rows))
}

qca_tobit <- function(cal_s, out_s, conds_s, map, mv, suff, o) {
  if (o$calibration[[o$outcome]]$method == "crisp")
    return(list(message = "Tobit regression needs a fuzzy outcome (membership between 0 and 1); the outcome is a crisp set."))
  if (!requireNamespace("VGAM", quietly = TRUE))
    return(list(message = "The VGAM R package is not installed on the analysis server (run Rscript backend/install_packages.R)."))
  y <- cal_s[[out_s]]
  # Model 1: calibrated conditions (multi-value conditions as category indicators).
  X1 <- list()
  for (cn in conds_s) {
    x <- cal_s[[cn]]
    if (cn %in% mv) { lv <- sort(unique(x)); for (k in lv[-1]) X1[[sprintf("%s[%s]", map[[cn]], k)]] <- as.numeric(x == k) }
    else X1[[map[[cn]]]] <- x
  }
  X1 <- as.data.frame(X1, check.names = FALSE)
  models <- list()
  m1 <- tryCatch(qca_tobit_fit(y, X1, "conditions", "Model 1: calibrated conditions"), error = function(e) e)
  if (!inherits(m1, "error")) models[[length(models) + 1]] <- m1
  # Model 2: membership in each pathway of the solution (intermediate, else parsimonious, else complex).
  sol <- if (length(suff$intermediate)) suff$intermediate[[1]]$models[[1]] else if (length(suff$parsimonious)) suff$parsimonious[[1]] else if (length(suff$complex)) suff$complex[[1]] else NULL
  solKind <- if (length(suff$intermediate)) "intermediate" else if (length(suff$parsimonious)) "parsimonious" else if (length(suff$complex)) "complex" else NULL
  m2 <- NULL
  if (!is.null(sol$memberships$terms) && length(sol$memberships$terms)) {
    X2 <- as.data.frame(lapply(sol$memberships$terms, as.numeric), check.names = FALSE)
    exprs <- names(sol$memberships$terms)
    names(X2) <- sprintf("Solution %d", seq_along(exprs))          # as in published fsQCA robustness tables
    m2 <- tryCatch(qca_tobit_fit(y, X2, "pathways", sprintf("Model 2: pathways of the %s solution", solKind)), error = function(e) e)
    if (!inherits(m2, "error")) {
      m2$coefs <- lapply(m2$coefs, function(k) { i <- match(k$term, names(X2)); if (!is.na(i)) k$expression <- exprs[i]; k })
      models[[length(models) + 1]] <- m2
    }
  }
  errs <- Filter(function(x) inherits(x, "error"), list(m1, m2))
  list(package = as.character(utils::packageVersion("VGAM")), outcome = o$outcome, solution = solKind, models = models,
       message = if (!length(models)) paste("Tobit regression could not be estimated:", sanitize_message(conditionMessage(errs[[1]]))) else NULL,
       warnings = if (length(errs) && length(models)) as.list(vapply(errs, function(e) sanitize_message(conditionMessage(e)), "")) else NULL)
}

qca_tobit_code <- function(o, safe, mv) {
  conds <- unname(safe[o$conditions]); out <- safe[[o$outcome]]
  c(
    "", "# Robustness check: Tobit regression of the outcome membership, censored at 0 and 1 (VGAM)",
    "library(VGAM)",
    sprintf("summary(vglm(%s ~ %s, tobit(Lower = 0, Upper = 1), data = cal))   # model 1: calibrated conditions", out, paste(conds, collapse = " + ")),
    "sol <- minimize(tt, include = \"?\", details = TRUE)   # or the intermediate solution used in the app",
    sprintf("pw <- data.frame(%s = cal$%s, sol$pims)", out, out),
    sprintf("summary(vglm(%s ~ ., tobit(Lower = 0, Upper = 1), data = pw))   # model 2: membership in each pathway", out)
  )
}

qca_tobit_html <- function(t) {
  if (is.null(t)) return("")
  esc <- html_escape
  S <- c("<h3>Robustness check: Tobit regression</h3>")
  if (!is.null(t$message)) return(paste0(S, "<div class=\"warn\">", esc(t$message), "</div>"))
  S <- c(S, sprintf("<p>The membership in %s is bounded by 0 and 1, so it was modelled with a Tobit regression censored at both limits (VGAM %s, <code>vglm(..., tobit(Lower = 0, Upper = 1))</code>). Positive, significant coefficients agree with the QCA result; QCA and regression answer different questions, so a non-significant coefficient does not contradict a sufficient configuration.</p>",
                    esc(t$outcome), esc(t$package)))
  for (m in t$models) {
    rows <- lapply(m$coefs, function(k) c(paste0(esc(k$term), if (!is.null(k$expression)) paste0(" <span class=\"note\">(", esc(k$expression), ")</span>") else ""),
                                          paste0(fmt(k$estimate), qca_stars(k$p)), fmt(k$se), fmt(k$z), fmt(k$p), paste0("[", fmt(k$ciLow), ", ", fmt(k$ciHigh), "]")))
    S <- c(S, html_table(c("Variables", "Coefficient", "Std. Error", "z-Value", "Probability", "95% CI"), rows,
                         sprintf("%s (n = %d; censored at 0: %d, at 1: %d; &sigma; = %s; LR &chi;&sup2;(%d) = %s, p %s; AIC = %s)", esc(m$label), m$n, m$censoredLow, m$censoredHigh,
                                 fmt(m$sigma), m$df, fmt(m$lrChi2, 2), if (isTRUE(m$lrP < 0.001)) "&lt; 0.001" else paste("=", formatC(m$lrP, format = "f", digits = 3)), fmt(m$aic, 1)), num_cols = 2:6))
  }
  S <- c(S, "<p class=\"note\"><em>Note.</em> *p &lt; .05, **p &lt; .01, ***p &lt; .001. Maximum-likelihood estimates; the z-value is the coefficient divided by its standard error; &sigma; = standard deviation of the latent outcome.</p>")
  for (w in ul(t$warnings)) S <- c(S, paste0("<div class=\"warn\">", esc(w), "</div>"))
  paste(S, collapse = "\n")
}

qca_stars <- function(p) { p <- suppressWarnings(as.numeric(p)); if (!length(p) || is.na(p)) "" else if (p < 0.001) "***" else if (p < 0.01) "**" else if (p < 0.05) "*" else "" }

qca_tobit_sheet <- function(t) {
  if (is.null(t)) return(data.frame(Note = "Tobit regression was switched off"))
  if (!length(t$models)) return(data.frame(Note = t$message %||% "Not estimated"))
  do.call(rbind, lapply(t$models, function(m) do.call(rbind, lapply(m$coefs, function(k)
    data.frame(Model = m$label, Predictor = k$term, Expression = k$expression %||% "", B = k$estimate, SE = k$se, z = k$z, p = k$p, CI_low = k$ciLow, CI_high = k$ciHigh,
               n = m$n, Sigma = m$sigma, LR_chi2 = m$lrChi2, LR_df = m$df, LR_p = m$lrP, AIC = m$aic, stringsAsFactors = FALSE)))))
}

# ---- Robustness: sensitivity of the solution to the thresholds and the calibration ----------------------
# Each test re-runs the truth table analysis with one change (Oana & Schneider, 2024; Greckhamer et al., 2018) and
# compares the solution (intermediate, else parsimonious, else complex; first model) with the original one.

qca_sol_summary <- function(s) {
  sl <- if (is.null(s)) list() else qca_solution_list(s)
  k <- qca_first_kind(sl)
  if (is.na(k)) return(list(kind = NULL, expression = NULL, terms = character(0), inclS = NULL, covS = NULL))
  m <- sl[[k]][[1]]
  list(kind = k, expression = m$expression, terms = vapply(m$terms, `[[`, "", "term"), inclS = m$inclS, covS = m$covS)
}

qca_sensitivity <- function(d, vars, safe, cal_s, out_s, conds_s, o, n_cut, map, mv, base) {
  b <- qca_sol_summary(base)
  tests <- list()
  only <- b$kind %||% "parsimonious"
  run <- function(o2, nc = n_cut, cal = cal_s) tryCatch(suppressWarnings(qca_sufficiency(cal, out_s, conds_s, o2, nc, map, mv, only = only)), error = function(e) NULL)
  add <- function(test, change, s) {
    x <- qca_sol_summary(s)
    tests[[length(tests) + 1]] <<- list(test = test, change = change, solution = x$expression %||% "no sufficient configuration", kind = x$kind,
      inclS = x$inclS, covS = x$covS, nTerms = length(x$terms), kept = sum(b$terms %in% x$terms), same = length(b$terms) > 0 && setequal(b$terms, x$terms))
  }
  f2 <- function(x) formatC(x, format = "f", digits = 2)
  up <- min(1, o$inclCut + 0.05)
  if (up > o$inclCut) add("Raw consistency threshold raised", paste(f2(o$inclCut), "→", f2(up)), run(utils::modifyList(o, list(inclCut = up))))
  dn <- o$inclCut - 0.05
  if (dn >= 0.75) add("Raw consistency threshold lowered", paste(f2(o$inclCut), "→", f2(dn)), run(utils::modifyList(o, list(inclCut = dn))))
  pu <- min(1, o$priCut + 0.05)
  if (pu > o$priCut) add("PRI threshold raised", paste(f2(o$priCut), "→", f2(pu)), run(utils::modifyList(o, list(priCut = pu))))
  add("Frequency threshold raised", paste(n_cut, "→", n_cut + 1), run(o, nc = n_cut + 1))
  # Calibration: fuzzy sets with percentile anchors get anchors 0.05 closer to the crossover (e.g. 95/50/5 -> 90/50/10).
  pv <- Filter(function(v) { c0 <- o$calibration[[v]]; c0$method == "fuzzy" && c0$anchors == "percentile" && (c0$thresholds[2] - c0$thresholds[1]) > 0.1 }, vars)
  if (length(pv)) {
    cal2 <- cal_s
    ok <- TRUE
    for (v in pv) {
      c0 <- o$calibration[[v]]; c0$thresholds <- c(c0$thresholds[1] + 0.05, c0$thresholds[2], c0$thresholds[3] - 0.05)
      cb <- tryCatch(qca_calibrate_one(d[[v]], c0), error = function(e) NULL)
      if (is.null(cb)) { ok <- FALSE; break }
      set <- cb$set
      if (o$avoidHalf && v != o$outcome) set[abs(set - 0.5) < 1e-12] <- 0.501
      cal2[[safe[[v]]]] <- set
    }
    th <- o$calibration[[pv[1]]]$thresholds
    if (ok) add("Calibration anchors moved inwards", sprintf("percentiles %s/%s/%s → %s/%s/%s", round(100 * th[3]), round(100 * th[2]), round(100 * th[1]),
                                                            round(100 * (th[3] - 0.05)), round(100 * th[2]), round(100 * (th[1] + 0.05))), run(o, cal = cal2))
  }
  list(base = b$expression, baseKind = b$kind, tests = tests,
       robust = length(tests) > 0 && all(vapply(tests, function(t) isTRUE(t$same) || (t$kept > 0 && t$kept == min(t$nTerms, length(b$terms))), TRUE)))
}

qca_sensitivity_html <- function(sv) {
  if (!is.null(sv$message)) return(paste0("<h3>Robustness check: sensitivity analysis</h3><div class=\"warn\">", html_escape(sv$message), "</div>"))
  if (is.null(sv) || !length(sv$tests)) return("")
  rows <- lapply(sv$tests, function(t) c(html_escape(t$test), html_escape(t$change), html_escape(t$solution), fmt(t$inclS), fmt(t$covS),
                                         if (isTRUE(t$same)) "Same" else sprintf("%d of %d original configurations kept", t$kept, length(strsplit(sv$base %||% "", " + ", fixed = TRUE)[[1]]))))
  paste0("<h3>Robustness check: sensitivity analysis</h3>",
         "<p>Each row repeats the truth table analysis with one change to the thresholds or the calibration and compares the ", html_escape(sv$baseKind %||% ""), " solution with the original one (",
         html_escape(sv$base %||% "none"), "). Solutions that stay the same, or keep the original configurations, indicate a robust result (Greckhamer et al., 2018; Oana &amp; Schneider, 2024).</p>",
         html_table(c("Test", "Change", "Solution", "Consistency", "Coverage", "Compared with the original"), rows, "Sensitivity of the solution to the analytic choices", num_cols = 4:5))
}

# ---- Reporting checklist, write-up and references -------------------------------------------------------

qca_checklist <- function(res) {
  o <- res$options
  s <- res$sufficiency[[1]]
  sl <- qca_solution_list(s)
  k <- qca_first_kind(sl)
  m <- if (is.na(k)) NULL else sl[[k]][[1]]
  items <- list()
  add <- function(item, status, detail, source) items[[length(items) + 1]] <<- list(item = item, status = status, detail = detail, source = source)
  pct <- Filter(function(c) c$anchors == "percentile" && c$method != "none", res$calibration)
  add("Calibration anchors reported and justified", if (length(pct)) "info" else "ok",
      if (length(pct)) sprintf("Anchors are reported. Percentile anchors were used for %s: justify them (e.g. the scale has no external standard) and consider theory-based values where they exist.", paste(vapply(pct, `[[`, "", "variable"), collapse = ", "))
      else "Value anchors are reported; state the theoretical or substantive basis of each.", "Ragin (2008); Pappas & Woodside (2021)")
  skew <- Filter(function(c) { sh <- c$above / res$n$used; sh < 0.2 || sh > 0.8 }, res$calibration)
  add("Skewed set memberships", if (length(skew)) "warn" else "ok",
      if (length(skew)) paste0("More than 80% or fewer than 20% of the cases are more in than out of: ", paste(vapply(skew, function(c) sprintf("%s (%.0f%%)", c$variable, 100 * c$above / res$n$used), ""), collapse = ", "),
                               ". Skewed sets can produce trivial necessity and inflated sufficiency; consider other anchors.")
      else "Every set has between 20% and 80% of the cases above the crossover.", "Greckhamer et al. (2018); Oana et al. (2021)")
  nb <- res$necessity[[1]]$rows
  nec <- Filter(function(x) isTRUE(x$necessary), nb)
  trivial <- Filter(function(x) isTRUE(x$RoN < 0.5), nec)
  add("Necessity analysed before sufficiency", if (length(trivial)) "warn" else "ok",
      paste0(if (length(nec)) sprintf("Necessary (consistency ≥ %.2f): %s.", o$necessityCut, paste(vapply(nec, `[[`, "", "condition"), collapse = ", ")) else sprintf("No condition reaches a necessity consistency of %.2f.", o$necessityCut),
             if (length(trivial)) sprintf(" Low relevance (RoN < 0.5), possibly trivial: %s.", paste(vapply(trivial, `[[`, "", "condition"), collapse = ", ")) else ""),
      "Schneider & Wagemann (2010, 2012)")
  share <- s$casesInRows / res$n$used
  add("Frequency threshold and share of cases retained", if (share >= 0.75) "ok" else "warn",
      sprintf("Frequency threshold %d; %.0f%% of the cases are in rows that meet it (at least 75%%–80%% is recommended).", s$nCut, 100 * share), "Greckhamer et al. (2018)")
  add("Raw consistency threshold", if (o$inclCut >= 0.8) "ok" else if (o$inclCut >= 0.75) "info" else "warn",
      sprintf("%.2f (%s).", o$inclCut, if (o$inclCut >= 0.8) "meets the recommended 0.80" else if (o$inclCut >= 0.75) "the accepted minimum is 0.75; 0.80 is recommended" else "below the accepted minimum of 0.75"), "Ragin (2008); Greckhamer et al. (2018)")
  lowpri <- Filter(function(r) r$out == "1" && r$n >= s$nCut && isTRUE(r$PRI < 0.5), s$rows)
  add("PRI consistency threshold", if (length(lowpri) || o$priCut < 0.5) "warn" else if (o$priCut >= 0.7) "ok" else "info",
      paste0(sprintf("%.2f", o$priCut), if (o$priCut >= 0.7) " (meets the usual 0.70–0.75)." else if (o$priCut >= 0.5) " (0.70–0.75 is usual; values below 0.50 point to simultaneous subset relations)." else " (below 0.50: rows may be subsets of both the outcome and its absence).",
             if (length(lowpri)) sprintf(" Row(s) %s are coded 1 with a PRI below 0.50.", paste(vapply(lowpri, function(r) as.character(r$row), ""), collapse = ", ")) else ""),
      "Greckhamer et al. (2018)")
  add("Limited diversity and logical remainders", "info",
      sprintf("%d of %d possible configurations (%.0f%%) are logical remainders; report how they were treated (complex: none; intermediate: easy counterfactuals; parsimonious: all).",
              as.integer(s$nRemainders), as.integer(s$nRowsPossible), 100 * s$nRemainders / s$nRowsPossible), "Schneider & Wagemann (2012)")
  ex <- vapply(ul(o$conditions), function(cn) { e <- o$expectations[[cn]] %||% "present"; if (e == "either") paste(cn, "present or absent") else if (e == "absent") paste0("~", cn) else cn }, "")
  add("Counterfactual (directional) expectations", if (length(s$intermediate)) "ok" else "info",
      paste0("Expectations: ", paste(ex, collapse = ", "), if (length(s$intermediate)) "; reported with the intermediate solution." else "; no intermediate solution."), "Ragin (2008); Fiss (2011)")
  if (!is.null(m)) {
    add("Solution consistency and coverage", if (isTRUE(m$inclS >= 0.75)) "ok" else "warn",
        sprintf("%s solution: consistency %.3f, coverage %.3f%s.", tools::toTitleCase(k), m$inclS, m$covS, if (isTRUE(m$inclS < 0.75)) " (consistency below 0.75)" else ""), "Ragin (2008)")
    ssr <- Filter(function(t) isTRUE(t$inclSNeg >= o$inclCut), m$terms)
    add("Simultaneous subset relations", if (length(ssr)) "warn" else "ok",
        if (length(ssr)) sprintf("Configuration(s) %s are also consistent with the absence of %s; they should not be interpreted as sufficient.", paste(vapply(ssr, `[[`, "", "term"), collapse = ", "), o$outcome)
        else sprintf("No configuration is also a consistent subset of ~%s (consistency for ~%s below %.2f).", o$outcome, o$outcome, o$inclCut), "Schneider & Wagemann (2012); Greckhamer et al. (2018)")
    nmod <- max(length(sl[[k]]), as.integer(s$modelsTotal[[k]] %||% 0L), na.rm = TRUE)
    add("Model ambiguity", if (nmod > 1) "warn" else "ok", if (nmod > 1) sprintf("%d equally good models; report all of them.", nmod) else "One model.", "Baumgartner & Thiem (2017)")
    add("Core and peripheral conditions", if (length(s$chart)) "ok" else "info",
        if (length(s$chart)) "Configuration chart with core (also in the parsimonious solution) and peripheral conditions." else "No intermediate solution, so no chart.", "Fiss (2011)")
  } else add("Sufficient configurations", "warn", s$message %||% "No sufficient configuration.", "Ragin (2008)")
  add("Absence of the outcome analysed separately", if (isTRUE(o$negateOutcome)) "ok" else "info",
      if (isTRUE(o$negateOutcome)) "~Y was analysed (causal asymmetry)." else "Recommended: analyse ~Y as well, because the configurations for its absence are not the mirror image (causal asymmetry).", "Schneider & Wagemann (2010)")
  if (length(s$overridden)) add("Truth-table rows coded by hand", "info", sprintf("Row(s) %s; report and justify the coding.", paste(ul(s$overridden), collapse = ", ")), "Schneider & Wagemann (2010)")
  sv <- res$sensitivity; tb <- res$tobit
  if (!is.null(sv) && !length(sv$tests)) sv <- NULL
  pv <- res$predictive
  if (!is.null(pv) && length(pv$directions)) add("Predictive validity", if (isTRUE(pv$holds)) "ok" else "warn",
      if (isTRUE(pv$holds)) sprintf("The solution of each random half predicts the other half (holdout solution consistency \u2265 %.2f).", o$inclCut)
      else sprintf("The solution of at least one random half does not reach a consistency of %.2f in the other half; report this as a limitation.", o$inclCut),
      "Woodside (2014); Pappas & Woodside (2021)")
  else if (!is.null(pv$message)) add("Predictive validity", "info", pv$message, "Pappas & Woodside (2021)")
  add("Robustness tests", if (!is.null(sv) || (!is.null(tb) && length(tb$models))) (if (!is.null(sv) && !isTRUE(sv$robust)) "info" else "ok") else "warn",
      paste(c(if (!is.null(sv)) sprintf("Sensitivity analysis: %d of %d changes give the same solution.", sum(vapply(sv$tests, function(t) isTRUE(t$same), TRUE)), length(sv$tests)),
              if (!is.null(tb) && length(tb$models)) "Tobit regression of the outcome on the conditions and the configurations.",
              if (is.null(sv) && (is.null(tb) || !length(tb$models))) "No robustness test was run; switch on the sensitivity analysis or the Tobit regression."), collapse = " "),
      "Greckhamer et al. (2018); Oana & Schneider (2024)")
  items
}

qca_checklist_html <- function(items) {
  if (!length(items)) return("")
  sym <- c(ok = "<span style=\"color:#166534\">&#10003;</span>", warn = "<span style=\"color:#b45309\">&#9888;</span>", info = "<span style=\"color:#1d4f86\">&#8505;</span>")
  rows <- lapply(items, function(x) c(sym[[x$status]], html_escape(x$item), html_escape(x$detail), html_escape(x$source)))
  paste0("<h3>Reporting checklist</h3><p>Checked against published standards for QCA (Schneider &amp; Wagemann, 2010; Greckhamer et al., 2018; Pappas &amp; Woodside, 2021). ",
         "&#10003; meets the standard, &#9888; needs attention or justification, &#8505; to report.</p>",
         html_table(c("", "Item", "This analysis", "Source"), rows, "Reporting checklist"))
}

qca_writeup <- function(res) {
  o <- res$options
  s <- res$sufficiency[[1]]
  conds <- ul(o$conditions)
  cal <- res$calibration
  fz <- Filter(function(c) c$method == "fuzzy", cal)
  pct <- if (length(fz) && all(vapply(fz, function(c) c$anchors == "percentile", TRUE))) num_of(unlist(fz[[1]]$percentiles)) else NULL
  same_pct <- !is.null(pct) && all(vapply(fz, function(c) isTRUE(all.equal(num_of(unlist(c$percentiles)), pct)), TRUE))
  calib <- if (length(fz) == length(cal) && same_pct)
    sprintf("All sets were calibrated with the direct method (Ragin, 2008), using the %s, %s and %s percentiles of each variable as the thresholds for full membership, the crossover point and full non-membership (Pappas & Woodside, 2021).",
            ordinal(round(100 * pct[3])), ordinal(round(100 * pct[2])), ordinal(round(100 * pct[1])))
    else "The sets were calibrated as reported in the calibration table (direct method for fuzzy sets; Ragin, 2008)."
  half <- if (isTRUE(o$avoidHalf) && any(vapply(cal, function(c) c$role == "condition" && c$exactlyHalf > 0, TRUE))) " Condition memberships of exactly 0.5 were set to 0.501 so that the cases are not dropped (Fiss, 2011)." else ""
  nec <- Filter(function(x) isTRUE(x$necessary), res$necessity[[1]]$rows)
  necs <- if (length(nec)) sprintf("The analysis of necessary conditions identified %s as necessary (consistency ≥ %.2f).", paste(vapply(nec, `[[`, "", "condition"), collapse = ", "), o$necessityCut)
          else sprintf("The analysis of necessary conditions showed that no single condition is necessary for %s (all consistencies below %.2f; Schneider & Wagemann, 2012).", o$outcome, o$necessityCut)
  tt <- sprintf("The truth table (%d of %d possible configurations observed) was minimised with a frequency threshold of %d case%s, which retains %.0f%% of the cases, a raw consistency threshold of %.2f and a PRI consistency threshold of %.2f (Greckhamer et al., 2018).",
                as.integer(s$nRowsObserved), as.integer(s$nRowsPossible), as.integer(s$nCut), if (s$nCut == 1) "" else "s", 100 * s$casesInRows / res$n$used, o$inclCut, o$priCut)
  sl <- qca_solution_list(s)
  k <- qca_first_kind(sl)
  sol <- if (is.na(k)) sprintf("No configuration was consistently sufficient for %s.", o$outcome) else {
    m <- sl[[k]][[1]]
    sprintf("The %s solution%s comprises %d configuration%s (%s) with an overall solution consistency of %.3f and coverage of %.3f.%s",
            k, if (k == "intermediate") " (based on the directional expectations reported)" else "", length(m$terms), if (length(m$terms) == 1) "" else "s", m$expression, m$inclS, m$covS,
            paste0(if (isTRUE(m$inclS < 0.75)) " The solution consistency is below the commonly accepted minimum of 0.75 (Ragin, 2008), so the solution should be interpreted with caution." else "",
                   if (length(s$chart)) " Core conditions are those that also appear in the parsimonious solution; peripheral conditions appear only in the intermediate solution (Fiss, 2011)." else ""))
  }
  rob <- character(0)
  sv <- res$sensitivity
  if (!is.null(sv) && length(sv$tests)) rob <- c(rob, sprintf("In a sensitivity analysis, %d of %d alternative thresholds and calibrations produced the same solution%s.", sum(vapply(sv$tests, function(t) isTRUE(t$same), TRUE)), length(sv$tests),
                                                              if (isTRUE(sv$robust)) ", and the others kept the original configurations" else ""))
  pv <- res$predictive
  if (!is.null(pv) && length(pv$directions))
    rob <- c(rob, sprintf("To test predictive validity, the cases were split at random into two subsamples (n = %d and n = %d); the %s solution derived on each subsample reached a consistency of %s and a coverage of %s on the other (Woodside, 2014; Pappas & Woodside, 2021)%s.",
                          as.integer(pv$nA), as.integer(pv$nB), pv$solution %||% "parsimonious", paste(vapply(Filter(function(dd) dd$outcome == o$outcome, pv$directions), function(dd) sprintf("%.3f", dd$testInclS %||% NA), ""), collapse = " and "),
                          paste(vapply(Filter(function(dd) dd$outcome == o$outcome, pv$directions), function(dd) sprintf("%.3f", dd$testCovS %||% NA), ""), collapse = " and "),
                          if (isTRUE(pv$holds)) ", supporting the predictive validity of the model" else ", so predictive validity is only partly supported"))
  tb <- res$tobit
  if (!is.null(tb) && length(tb$models)) {
    m2 <- Filter(function(m) m$id == "pathways", tb$models)
    if (length(m2)) {
      sig <- Filter(function(x) x$term != "(Intercept)" && isTRUE(x$p < 0.05) && x$estimate > 0, m2[[1]]$coefs)
      rob <- c(rob, sprintf("A Tobit regression of %s on the membership in each configuration (Tobin, 1958; VGAM, Yee, 2015) showed %s.", o$outcome,
                            if (length(sig)) sprintf("positive and significant coefficients for %s", paste(vapply(sig, `[[`, "", "term"), collapse = ", ")) else "no significant configuration coefficient"))
    }
  }
  paste0("<h3>Suggested write-up</h3><p class=\"note\">A draft for the method and results sections, built from this analysis; edit it to fit your study.</p><blockquote>",
         html_escape(paste(c(sprintf("We used %s with the QCA package (Dusa, 2019) in R, run in PathLab SEM (%s), to identify the configurations of %s that are sufficient for %s (n = %d).",
                                     if (res$setType == "fsQCA") "fuzzy-set qualitative comparative analysis (fsQCA)" else if (res$setType == "csQCA") "crisp-set qualitative comparative analysis (csQCA)" else "multi-value qualitative comparative analysis (mvQCA)",
                                     pathlab_in_text(), paste(conds, collapse = ", "), o$outcome, as.integer(res$n$used)),
                             paste0(calib, half), necs, tt, sol, rob), collapse = " ")), "</blockquote>")
}

ordinal <- function(n) paste0(n, if (n %% 100 %in% 11:13) "th" else c("th", "st", "nd", "rd", rep("th", 6))[n %% 10 + 1])

qca_references_html <- function(res) {
  refs <- c(pathlab_citation("html"),
            "Baumgartner, M., &amp; Thiem, A. (2017). Model ambiguities in configurational comparative research. <i>Sociological Methods &amp; Research, 46</i>(4), 954–987.",
            sprintf("Dusa, A. (2019). <i>QCA with R: A comprehensive resource</i>. Springer. (QCA package version %s.)", html_escape(res$packages$QCA %||% "")),
            "Fiss, P. C. (2011). Building better causal theories: A fuzzy set approach to typologies in organization research. <i>Academy of Management Journal, 54</i>(2), 393–420.",
            "Greckhamer, T., Furnari, S., Fiss, P. C., &amp; Aguilera, R. V. (2018). Studying configurations with qualitative comparative analysis: Best practices in strategy and organization research. <i>Strategic Organization, 16</i>(4), 482–495.",
            "Oana, I.-E., &amp; Schneider, C. Q. (2024). A robustness test protocol for applied QCA: Theory and R software application. <i>Sociological Methods &amp; Research, 53</i>(1), 57–88.",
            "Oana, I.-E., Schneider, C. Q., &amp; Thomann, E. (2021). <i>Qualitative comparative analysis using R: A beginner’s guide</i>. Cambridge University Press.",
            "Pappas, I. O., &amp; Woodside, A. G. (2021). Fuzzy-set qualitative comparative analysis (fsQCA): Guidelines for research practice in information systems and marketing. <i>International Journal of Information Management, 58</i>, 102310.",
            "Ragin, C. C. (2008). <i>Redesigning social inquiry: Fuzzy sets and beyond</i>. University of Chicago Press.",
            "Schneider, C. Q., &amp; Wagemann, C. (2010). Standards of good practice in qualitative comparative analysis (QCA) and fuzzy-sets. <i>Comparative Sociology, 9</i>(3), 397–418.",
            "Schneider, C. Q., &amp; Wagemann, C. (2012). <i>Set-theoretic methods for the social sciences: A guide to qualitative comparative analysis</i>. Cambridge University Press.",
            if (!is.null(res$tobit) && length(res$tobit$models)) c("Tobin, J. (1958). Estimation of relationships for limited dependent variables. <i>Econometrica, 26</i>(1), 24–36.",
                                                                    sprintf("Yee, T. W. (2015). <i>Vector generalized linear and additive models: With an implementation in R</i>. Springer. (VGAM package version %s.)", html_escape(res$tobit$package %||% ""))))
  paste0("<h3>References</h3><ul class=\"refs\">", paste0("<li>", refs, "</li>", collapse = ""), "</ul>")
}

# ---- Choosing conditions (more than 7) ----------------------------------------------------------------------

QCA_SELECT_SKEW <- c(0.2, 0.8)       # share of cases in the set outside this range: a skewed set (little diversity)
QCA_SELECT_OVERLAP <- 0.6
QCA_SELECT_RELIABLE <- 0.75          # consistency at which a condition counts as a consistent subset of the outcome            # |r| between two condition sets at or above this: they measure much the same thing

#' Item family of a variable name: the name without its trailing number (PE1, PE2 -> PE). Items of one family are
#' usually the indicators of one construct.
qca_family <- function(v) { f <- sub("[_.[:space:]-]*[0-9]+$", "", v); if (nzchar(f)) f else v }

#' Suggest the k conditions (default 7) that are most useful for the outcome, with a reason for every condition. A
#' data-driven pre-screen to support (never replace) theory: each condition is calibrated as in the analysis, then
#' scored by its single-condition sufficiency relation with the outcome (consistency x coverage, in the expected
#' direction: the condition, or its absence when a negative effect is expected; the stronger of the two when "either").
#' Skewed sets are penalised (Schneider & Wagemann, 2012; Greckhamer et al., 2018). A condition is passed over when it
#' is another item of a construct already chosen (items of one construct belong in one construct score) or overlaps
#' strongly with a chosen set (|r| >= 0.6), so that the chosen conditions are distinct.
qca_select_conditions <- function(df, o, k = 7L, missing_code = NULL) {
  if (!nzchar(o$outcome)) api_error(422, "qca_outcome", "Choose the outcome first.")
  vars <- c(o$outcome, o$conditions)
  miss <- setdiff(vars, names(df))
  if (length(miss)) api_error(422, "qca_missing_column", sprintf("Not in the dataset: %s.", paste(miss, collapse = ", ")))
  d <- df[, vars, drop = FALSE]
  if (!is.null(missing_code)) for (cn in vars) { hit <- !is.na(d[[cn]]) & d[[cn]] == missing_code; d[[cn]][hit] <- NA }
  d <- d[stats::complete.cases(d), , drop = FALSE]
  if (nrow(d) < 10) api_error(422, "qca_too_few_rows", sprintf("Only %d complete rows remain.", nrow(d)))
  cal <- function(v) tryCatch(qca_calibrate_one(d[[v]], o$calibration[[v]])$set, error = function(e) NULL)
  y <- cal(o$outcome)
  if (is.null(y)) api_error(422, "qca_calibration", sprintf("The outcome '%s' could not be calibrated.", o$outcome))
  if (o$negateOutcome) y <- 1 - y
  ylab <- outcome_label(o, o$negateOutcome)
  incl <- function(x) { s <- sum(x); if (s > 0) sum(pmin(x, y)) / s else 0 }
  cov <- function(x) sum(pmin(x, y)) / sum(y)
  rows <- lapply(o$conditions, function(v) {
    x <- cal(v)
    if (is.null(x) || stats::sd(x) == 0) return(list(variable = v, usable = FALSE))
    e <- o$expectations[[v]] %||% "present"
    pres <- c(incl = incl(x), cov = cov(x)); abs_ <- c(incl = incl(1 - x), cov = cov(1 - x))
    use_abs <- e == "absent" || (e == "either" && abs_[["incl"]] * abs_[["cov"]] > pres[["incl"]] * pres[["cov"]])
    m <- if (use_abs) abs_ else pres
    share <- mean(x > 0.5)
    skewed <- share < QCA_SELECT_SKEW[1] || share > QCA_SELECT_SKEW[2]
    list(variable = v, usable = TRUE, set = x, family = qca_family(v), negated = use_abs, expectation = e, consistency = m[["incl"]], coverage = m[["cov"]],
         inSet = share, skewed = skewed, score = m[["incl"]] * m[["cov"]] * (if (skewed) 0.5 else 1))
  })
  ok <- Filter(function(r) isTRUE(r$usable), rows)
  ok <- ok[order(-vapply(ok, function(r) r$score, 0))]
  by_name <- setNames(ok, vapply(ok, function(r) r$variable, ""))
  fam_size <- table(vapply(ok, function(r) r$family, ""))
  chosen <- character(0); passed <- list()
  why_not <- function(r) {
    if (!length(chosen)) return(NULL)
    same <- chosen[vapply(chosen, function(c) by_name[[c]]$family == r$family && fam_size[[r$family]] > 1, TRUE)]
    if (length(same)) return(list(kind = "family", with = same[1], r = suppressWarnings(stats::cor(r$set, by_name[[same[1]]]$set))))
    rr <- vapply(chosen, function(c) suppressWarnings(stats::cor(r$set, by_name[[c]]$set)), 0)
    i <- which.max(abs(rr))
    if (abs(rr[i]) >= QCA_SELECT_OVERLAP) list(kind = "overlap", with = chosen[i], r = unname(rr[i])) else NULL
  }
  for (r in ok) {
    if (length(chosen) >= k) break
    w <- why_not(r)
    if (!is.null(w)) { passed[[r$variable]] <- w; next }
    chosen <- c(chosen, r$variable)
  }
  # Fewer distinct conditions than k: suggest those (never pad with near-duplicates), and say so.
  note <- if (length(chosen) < k) sprintf("Only %d distinct condition(s) remain once the items of each construct are counted once and strongly overlapping sets are left out, so %d %s suggested%s.",
    length(chosen), length(chosen), if (length(chosen) == 1) "is" else "are", if (length(chosen) >= 3) sprintf(" (within the recommended 3 to %d)", k) else "; consider adding conditions from theory") else NULL
  f2 <- function(x) formatC(x, format = "f", digits = 2)
  pct <- function(x) sprintf("%d%%", round(100 * x))
  rank_of <- setNames(seq_along(ok), names(by_name))
  stats_of <- function(r) sprintf("consistency %s, coverage %s", f2(r$consistency), f2(r$coverage))
  set_of <- function(r) if (r$negated) paste0("~", r$variable) else r$variable
  out <- lapply(rows, function(r) {
    v <- r$variable
    if (!isTRUE(r$usable)) return(list(variable = v, selected = FALSE, rank = NULL, reason = "Cannot be used: it could not be calibrated, or every case has the same membership."))
    sel <- v %in% chosen
    w <- passed[[v]]
    # A short headline and one plain sentence. "Consistent subset" is said only at the usual 0.75 benchmark, so a
    # moderate link is never described as a reliable subset relation.
    reliable <- r$consistency >= QCA_SELECT_RELIABLE
    hr <- if (sel) {
      head <- if (reliable && r$coverage >= 0.5) "Strong link." else if (reliable) "Clear link." else "Moderate link."
      body <- paste0(if (reliable) paste0(set_of(r), " is a consistent subset of ", ylab, ".")
                     else sprintf("Below the %s consistency needed for a reliable subset, but among the strongest available.", f2(QCA_SELECT_RELIABLE)),
                     if (r$negated && r$expectation != "absent") paste0(" Its absence links better than its presence, so ~", v, " is used.") else "",
                     if (r$skewed) paste0(" Skewed set (", pct(r$inSet), " of cases in it), kept because few stronger conditions remain.") else " Balanced set.")
      c(head, body)
    } else if (!is.null(w) && w$kind == "family") {
      c(paste0("Same construct as ", w$with, "."), "Its items measure the same thing: combine them into one construct score (Bulk compute) rather than using them separately.")
    } else if (!is.null(w)) {
      c(paste0("Overlaps with ", w$with, " (r = ", f2(w$r), ")."), "The two sets largely measure the same thing, so keeping both adds no new information.")
    } else if (r$skewed) {
      c("Skewed set.", paste0(pct(r$inSet), " of cases are in it, so it adds little diversity to the truth table."))
    } else {
      c(if (identical(rank_of[[v]], length(ok))) paste0("Weakest link of the ", length(ok), ".") else "Weaker link.",
        paste0("Left out to keep the ", length(chosen), " strongest", if (!reliable) "; its link is moderate too" else "", "."))
    }
    reason <- paste(hr[1], hr[2])
    list(variable = v, selected = sel, rank = unname(rank_of[[v]]), negated = r$negated, consistency = r$consistency, coverage = r$coverage,
         inSet = r$inSet, skewed = r$skewed, passedFor = w$kind, overlapWith = w$with, overlapR = w$r, score = r$score, headline = hr[1], detail = hr[2], reason = reason)
  })
  ord <- order(!vapply(out, function(r) isTRUE(r$selected), TRUE), vapply(out, function(r) r$rank %||% 1e6, 0))
  moderate <- length(chosen) > 0 && all(vapply(chosen, function(c) by_name[[c]]$consistency < QCA_SELECT_RELIABLE, TRUE))
  list(k = k, n = nrow(d), outcome = ylab, chosen = as.list(chosen), note = note, allModerate = moderate, conditions = out[ord],
       method = paste0("Each condition is calibrated as in the analysis and scored by its single-condition sufficiency relation with the outcome ",
                       "(consistency x coverage, in the expected direction). Skewed sets (fewer than 20% or more than 80% of cases in the set) are penalised. ",
                       "A condition is passed over when another item of the same construct, or a set that overlaps strongly with it (|r| >= 0.60), was already chosen. ",
                       "This is a data-driven pre-screen: theory and prior research should decide in the end (Schneider & Wagemann, 2012; Greckhamer et al., 2018)."))
}

# ---- Robustness: predictive validity on a holdout subsample ---------------------------------------------------
# Woodside (2014) and Pappas & Woodside (2021): split the cases at random into two halves, derive the solution on one
# half (same calibration, thresholds and expectations), and compute the consistency and coverage of its configurations
# and of the overall solution on the other half; then swap the halves. A model has predictive validity when its
# configurations stay consistent (≥ the consistency threshold) on the cases it was not derived from.

qca_term_membership <- function(raw, dat) {
  lit <- qca_literals(raw)
  m <- rep(1, nrow(dat))
  for (cn in names(lit)) {
    x <- dat[[cn]]; v <- lit[[cn]]
    m <- pmin(m, if (v == "present") x else if (v == "absent") 1 - x else as.numeric(x == as.numeric(v)))
  }
  m
}

qca_fit <- function(x, y) list(inclS = if (sum(x) > 0) sum(pmin(x, y)) / sum(x) else NULL, covS = if (sum(y) > 0) sum(pmin(x, y)) / sum(y) else NULL)

qca_predictive <- function(cal_s, out_s, conds_s, o, map, mv, negs = FALSE) {
  n <- nrow(cal_s)
  if (n < 40) return(list(directions = list(), holds = FALSE, message = sprintf("The predictive validity test needs at least 40 cases (two subsamples of 20); this analysis has %d.", n)))
  had <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had) old <- get(".Random.seed", envir = .GlobalEnv)
  set.seed(o$predictiveSeed)
  a <- sort(sample.int(n, floor(n / 2)))
  if (had) assign(".Random.seed", old, envir = .GlobalEnv) else rm(".Random.seed", envir = .GlobalEnv)
  halves <- list(A = cal_s[a, , drop = FALSE], B = cal_s[-a, , drop = FALSE])
  o2 <- o; o2$rowOverrides <- list()
  want <- o$predictiveSolution %||% "parsimonious"
  one <- function(mod, test, neg) {
    train <- halves[[mod]]; hold <- halves[[test]]
    lab <- outcome_label(o, neg)
    nc <- o$nCut %||% (if (nrow(train) >= 150) 2L else 1L)
    s <- tryCatch(suppressWarnings(qca_sufficiency(train, out_s, conds_s, o2, nc, map, mv, neg = neg, only = want)), error = function(e) NULL)
    sl <- if (is.null(s)) list() else qca_solution_list(s)
    k <- if (want %in% names(sl)) want else qca_first_kind(sl)
    if (is.na(k)) return(list(outcome = lab, model = mod, test = test, kind = NULL, expression = NULL, terms = list(),
                              message = sprintf("%s: no sufficient configuration in subsample %s, so there is nothing to test on subsample %s.", lab, mod, test)))
    m <- sl[[k]][[1]]
    y <- if (neg) 1 - hold[[out_s]] else hold[[out_s]]
    mem <- lapply(m$terms, function(t) qca_term_membership(t$raw, hold))
    terms <- lapply(seq_along(m$terms), function(i) { f <- qca_fit(mem[[i]], y)
      list(term = m$terms[[i]]$term, modelInclS = m$terms[[i]]$inclS, modelCovS = m$terms[[i]]$covS, testInclS = f$inclS, testCovS = f$covS) })
    sf <- qca_fit(do.call(pmax, unname(mem)), y)
    list(outcome = lab, model = mod, test = test, kind = k, expression = m$expression, terms = terms, nCut = nc,
         modelInclS = m$inclS, modelCovS = m$covS, testInclS = sf$inclS, testCovS = sf$covS)
  }
  dirs <- unlist(lapply(negs, function(neg) list(one("A", "B", neg), one("B", "A", neg))), recursive = FALSE)
  main <- Filter(function(d) d$outcome == o$outcome, dirs)
  ok <- Filter(function(d) !is.null(d$testInclS), main)
  list(seed = o$predictiveSeed, nA = length(a), nB = n - length(a), solution = want, directions = dirs, threshold = o$inclCut,
       holds = length(ok) == 2 && all(vapply(ok, function(d) d$testInclS >= o$inclCut, TRUE)))
}

qca_predictive_html <- function(pv) {
  if (is.null(pv)) return("")
  if (!is.null(pv$message)) return(paste0("<h3>Robustness check: predictive validity</h3><div class=\"warn\">", html_escape(pv$message), "</div>"))
  out <- paste0("<h3>Robustness check: predictive validity</h3><p>The cases were split at random (seed ", pv$seed, ") into subsample A (n = ", pv$nA, ") and subsample B (n = ", pv$nB,
                "). The solution derived on one subsample was evaluated on the other: configurations that keep a consistency of at least ", fmt(pv$threshold),
                " on the holdout cases indicate predictive validity (Woodside, 2014; Pappas &amp; Woodside, 2021).</p>")
  for (d in pv$directions) {
    if (!is.null(d$message)) { out <- paste0(out, "<div class=\"warn\">", html_escape(d$message), "</div>"); next }
    rows <- c(lapply(d$terms, function(t) c(html_escape(t$term), fmt(t$modelInclS), fmt(t$modelCovS), fmt(t$testInclS), fmt(t$testCovS))),
              list(c("<b>Overall solution</b>", fmt(d$modelInclS), fmt(d$modelCovS), fmt(d$testInclS), fmt(d$testCovS))))
    out <- paste0(out, html_table(c("Configuration", paste("Consistency", d$model), paste("Coverage", d$model), paste("Consistency", d$test), paste("Coverage", d$test)), rows,
                                  sprintf("%s: model from subsample %s (%s solution) tested on subsample %s", d$outcome, d$model, d$kind, d$test), num_cols = 2:5))
  }
  out
}

qca_predictive_sheet <- function(pv) {
  if (is.null(pv) || !length(pv$directions)) return(data.frame(Note = if (is.null(pv)) "Predictive validity test was switched off" else pv$message %||% "Not available"))
  do.call(rbind, lapply(pv$directions, function(d) {
    if (!length(d$terms)) return(data.frame(Outcome = d$outcome, Model = d$model, Test = d$test, Configuration = d$message %||% "", ConsistencyModel = NA, CoverageModel = NA, ConsistencyTest = NA, CoverageTest = NA, stringsAsFactors = FALSE))
    rbind(do.call(rbind, lapply(d$terms, function(t) data.frame(Outcome = d$outcome, Model = d$model, Test = d$test, Configuration = t$term, ConsistencyModel = num_of(t$modelInclS), CoverageModel = num_of(t$modelCovS),
                                                                 ConsistencyTest = num_of(t$testInclS), CoverageTest = num_of(t$testCovS), stringsAsFactors = FALSE))),
          data.frame(Outcome = d$outcome, Model = d$model, Test = d$test, Configuration = "Overall solution", ConsistencyModel = num_of(d$modelInclS), CoverageModel = num_of(d$modelCovS),
                     ConsistencyTest = num_of(d$testInclS), CoverageTest = num_of(d$testCovS), stringsAsFactors = FALSE))
  }))
}

qca_predictive_code <- function(o, safe, mv) {
  conds <- unname(safe[o$conditions]); out <- safe[[o$outcome]]
  q <- function(x) paste0("\"", x, "\"")
  dir <- unlist(lapply(o$conditions, function(v) { e <- o$expectations[[v]]; s <- safe[[v]]; if (s %in% mv || e == "either") NULL else if (e == "absent") paste0("~", s) else s }))
  paste(c("", "# Predictive validity (Woodside, 2014; Pappas & Woodside, 2021): derive the solution on one random half, test it on the other",
          sprintf("set.seed(%d); a <- sort(sample.int(nrow(cal), floor(nrow(cal) / 2)))", as.integer(o$predictiveSeed)),
          "for (h in list(list(model = a, test = -a), list(model = -a, test = a))) {",
          sprintf("  tt_h <- truthTable(cal[h$model, ], outcome = %s, conditions = c(%s), incl.cut = %s, pri.cut = %s, n.cut = %s)", q(out), paste(vapply(conds, q, ""), collapse = ", "),
                  o$inclCut, o$priCut, if (is.null(o$nCut)) "if (nrow(cal[h$model, ]) >= 150) 2 else 1" else o$nCut),
          sprintf("  sol <- tryCatch(minimize(tt_h, include = \"?\"%s), error = function(e) NULL)", if (length(dir)) sprintf(", dir.exp = c(%s)", paste(vapply(dir, q, ""), collapse = ", ")) else ""),
          "  if (is.null(sol)) { message(\"No sufficient configuration in this half\"); next }",
          if (identical(o$predictiveSolution, "intermediate")) "  sol_h <- if (!is.null(sol$i.sol)) sol$i.sol[[1]] else sol" else "  sol_h <- sol   # parsimonious solution",
          sprintf("  print(pof(paste(sol_h$solution[[1]], collapse = \" + \"), outcome = %s, data = cal[h$test, ], relation = \"sufficiency\"))   # consistency and coverage on the holdout half", q(out)),
          "}"), collapse = "\n")
}
