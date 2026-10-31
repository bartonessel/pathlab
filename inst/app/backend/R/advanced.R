# Advanced analyses: IPMA, NCA and PLS-MGA.
#
# - PLS-MGA: SEMinR's estimate_pls_mga() (Henseler et al., 2009). Reported as returned.
# - NCA: the NCA package (Dul, 2016; reference implementation) applied to the SEMinR
#   construct scores, as recommended for combined PLS-SEM + NCA (Richter et al., 2020).
# - IPMA: not provided by SEMinR. Computed from SEMinR estimates following Ringle &
#   Sarstedt (2016): indicators rescaled to 0-100, outer weights unstandardised by the
#   rescaled indicator SD and normed to sum to 1, performance = mean rescaled construct
#   score, importance = unstandardised total effect on the target. Validated against
#   cSEM::doIPMA() in backend/tests (importance and performance agree to ~1e-7).

NCA_CEILINGS <- c("ce_fdh", "cr_fdh")

# ---- settings ------------------------------------------------------------------

normalize_advanced <- function(raw) {
  raw <- raw %||% list()
  ip <- raw$ipma %||% list()
  sc_min <- nullish(ip$scaleMin); sc_max <- nullish(ip$scaleMax)
  if (!is.null(sc_min)) sc_min <- suppressWarnings(as.numeric(sc_min))
  if (!is.null(sc_max)) sc_max <- suppressWarnings(as.numeric(sc_max))
  if (xor(is.null(sc_min), is.null(sc_max)) || (!is.null(sc_min) && (is.na(sc_min) || is.na(sc_max) || sc_min >= sc_max))) {
    api_error(400, "invalid_setting", "IPMA scale bounds need both a minimum and a larger maximum (or neither, to use observed values).")
  }
  nc <- raw$nca %||% list()
  ceilings <- as_chr_vec(nc$ceilings %||% list("ce_fdh", "cr_fdh"))
  if (!length(ceilings) || !all(ceilings %in% NCA_CEILINGS)) api_error(400, "invalid_setting", "NCA ceilings must be ce_fdh and/or cr_fdh.")
  ng <- raw$mga %||% list()
  cm <- raw$cmb %||% list()
  list(
    cmb = list(markerVariable = as.character(nullish(cm$markerVariable) %||% "")),
    ipma = list(enabled = isTRUE(ip$enabled), target = as.character(nullish(ip$target) %||% ""),
                scaleMin = sc_min, scaleMax = sc_max,
                # cIPMA (Hauff et al., 2024): NCA on the rescaled (0-100) construct scores of the IPMA, and the share of
                # cases below the level each necessary condition requires for the chosen outcome level.
                cipma = isTRUE(ip$cipma),
                cipmaCeiling = { k <- as.character(nullish(ip$cipmaCeiling) %||% "ce_fdh")[1]; if (k %in% NCA_CEILINGS) k else "ce_fdh" },
                cipmaLevel = { v <- suppressWarnings(as.numeric(nullish(ip$cipmaLevel) %||% 80))[1]; if (is.na(v) || v <= 0 || v > 100) api_error(400, "invalid_setting", "The cIPMA outcome level must be between 0 and 100."); v },
                cipmaTestRep = int_in(ip$cipmaTestRep, 1000L, 0, 20000, "cIPMA permutation test repetitions")),
    nca = list(enabled = isTRUE(nc$enabled), target = as.character(nullish(nc$target) %||% ""),
               conditions = as_chr_vec(nc$conditions), ceilings = ceilings,
               testRep = int_in(nc$testRep, 1000L, 0, 20000, "NCA permutation test repetitions"),
               steps = int_in(nc$steps, 10L, 2, 20, "NCA bottleneck steps"),
               # Single indicators as conditions (Richter et al., 2020): of formative constructs (the default), of all, or none.
               itemLevel = { k <- as.character(nullish(nc$itemLevel) %||% "formative")[1]; if (k %in% c("none", "formative", "all")) k else "formative" },
               seed = { s <- nullish(nc$seed); if (is.null(s)) NULL else int_in(s, NULL, 1, 2147483647, "NCA seed") }),
    mga = list(enabled = isTRUE(ng$enabled), groupVariable = as.character(nullish(ng$groupVariable) %||% ""),
               group1 = as_chr_vec(ng$group1), group2 = as_chr_vec(ng$group2),
               nboot = int_in(ng$nboot, 1000L, 50, 20000, "MGA bootstrap samples"),
               seed = { s <- nullish(ng$seed); if (is.null(s)) NULL else int_in(s, NULL, 1, 2147483647 - 100000, "MGA seed") })
  )
}

# NCA conditions: the explicit list, or by default the target's direct antecedents.
nca_conditions <- function(model, nca) {
  byid <- construct_by_id(model)
  ids <- if (length(nca$conditions)) nca$conditions else
    unique(vapply(Filter(function(p) p$to == nca$target, model$paths), `[[`, "", "from"))
  ids[ids %in% names(byid) & ids != nca$target]
}

group_counts <- function(df, rows, settings) {
  g <- as.character(df[[settings$mga$groupVariable]])[rows]
  list(n1 = sum(!is.na(g) & g %in% settings$mga$group1), n2 = sum(!is.na(g) & g %in% settings$mga$group2),
       nMissing = sum(is.na(g)), nOther = sum(!is.na(g) & !(g %in% c(settings$mga$group1, settings$mga$group2))))
}

column_levels <- function(x, max_levels = 30) {
  v <- as.character(x[!is.na(x)])
  if (!length(v)) return(list())
  tab <- table(v)
  if (length(tab) > max_levels) return(NULL)
  lapply(names(tab), function(k) list(value = k, n = as.integer(tab[[k]])))
}

validate_advanced <- function(model, settings, ds, add) {
  byid <- construct_by_id(model)
  mk <- settings$cmb$markerVariable %||% ""
  if (nzchar(mk) && !is.null(ds)) {
    if (!mk %in% names(ds$data)) add("error", "cmb_marker", sprintf("The CMB marker variable '%s' is not in the dataset.", mk), "settings", "cmb")
    else if (mk %in% model_indicators(model)) add("error", "cmb_marker", "The CMB marker variable is used as an indicator in the model.", "settings", "cmb",
                                                  "Choose a variable that is theoretically unrelated to the constructs and not part of the model.")
    else if (!is.numeric(ds$data[[mk]])) add("error", "cmb_marker", "The CMB marker variable must be numeric.", "settings", "cmb")
  }
  endo <- unique(vapply(model$paths, `[[`, "", "to"))
  ip <- settings$ipma
  if (isTRUE(ip$enabled)) {
    if (!ip$target %in% endo) add("error", "ipma_target", "Choose an endogenous construct as the IPMA target.", "settings", "ipma")
    if (length(model$moderations %||% list())) add("error", "ipma_moderation", "IPMA is not available for models with moderating effects (interaction terms).", "settings", "ipma",
                                                     "Remove the moderators or turn IPMA off.")
    refl <- vapply(Filter(function(c) c$mode == "reflective", model$constructs), `[[`, "", "name")
    if (length(refl)) add("error", "ipma_plsc", sprintf("IPMA is available for composite (Mode A/B) models only; these constructs use PLSc: %s.", paste(refl, collapse = ", ")),
                          "settings", "ipma", "Change them to Composite – Mode A, or turn IPMA off.")
    if (!is.null(ds) && !is.null(ip$scaleMin)) {
      items <- intersect(model_indicators(model), names(ds$data))
      vals <- unlist(ds$data[, items, drop = FALSE])
      vals <- vals[!is.na(vals)]
      if (!is.null(settings$missing$code)) vals <- vals[vals != settings$missing$code]
      if (length(vals) && (min(vals) < ip$scaleMin || max(vals) > ip$scaleMax)) {
        add("error", "ipma_bounds", sprintf("Some indicator values fall outside the IPMA scale %s–%s.", format(ip$scaleMin), format(ip$scaleMax)), "settings", "ipma",
            "Correct the scale bounds, or leave them empty to use each indicator's observed minimum and maximum.")
      }
    }
  }
  nc <- settings$nca
  if (isTRUE(nc$enabled)) {
    if (!nc$target %in% names(byid)) add("error", "nca_target", "Choose an outcome construct for NCA.", "settings", "nca")
    else if (!length(nca_conditions(model, nc))) add("error", "nca_conditions", "NCA needs at least one condition construct (by default, the outcome's direct predictors).", "settings", "nca")
    else if (nc$testRep > 0) {
      n_tests <- length(nca_conditions(model, nc)) * length(nc$ceilings)
      if (n_tests * nc$testRep >= 200000) add("warning", "nca_slow", sprintf("The NCA permutation test will run %d × %d resamples, which can take a minute or more.", n_tests, nc$testRep), "settings", "nca")
    }
  }
  mg <- settings$mga
  if (isTRUE(mg$enabled)) {
    if (is.null(ds)) return(invisible())
    if (!nzchar(mg$groupVariable) || !mg$groupVariable %in% names(ds$data)) {
      add("error", "mga_variable", "Choose a grouping variable for the multigroup analysis.", "settings", "mga")
    } else if (mg$groupVariable %in% model_indicators(model)) {
      add("error", "mga_indicator", "The grouping variable is used as an indicator in the model.", "settings", "mga")
    } else if (!length(mg$group1) || !length(mg$group2)) {
      add("error", "mga_groups", "Assign at least one value of the grouping variable to each group.", "settings", "mga")
    } else if (length(intersect(mg$group1, mg$group2))) {
      add("error", "mga_overlap", "A value cannot belong to both groups.", "settings", "mga")
    } else {
      items <- model_indicators(model)
      rows <- seq_len(nrow(ds$data))
      if (settings$missing$treatment == "listwise" && all(items %in% names(ds$data))) {
        d <- ds$data[, items, drop = FALSE]
        if (!is.null(settings$missing$code)) for (v in items) d[[v]][!is.na(d[[v]]) & d[[v]] == settings$missing$code] <- NA
        rows <- which(stats::complete.cases(d))
      }
      gc <- group_counts(ds$data, rows, settings)
      if (min(gc$n1, gc$n2) < 10) add("error", "mga_small", sprintf("Group sizes are %d and %d; each group needs at least 10 rows.", gc$n1, gc$n2), "settings", "mga")
      else if (min(gc$n1, gc$n2) < 30) add("warning", "mga_small", sprintf("Group sizes are %d and %d. Group-specific estimates and PLS-MGA can be unstable in small groups.", gc$n1, gc$n2), "settings", "mga")
      if (gc$nMissing + gc$nOther > 0) add("info", "mga_excluded", sprintf("%d row(s) are not in either group (%d with a missing group value) and are excluded from the MGA model.", gc$nMissing + gc$nOther, gc$nMissing), "settings", "mga")
    }
  }
  invisible()
}

# ---- IPMA ------------------------------------------------------------------------

run_ipma <- function(m, model, ip) {
  byid <- construct_by_id(model)
  target <- byid[[ip$target]]$name
  items <- model_indicators(model)
  X <- as.matrix(m$data[, items, drop = FALSE])
  lo <- if (is.null(ip$scaleMin)) apply(X, 2, min) else rep(ip$scaleMin, length(items))
  hi <- if (is.null(ip$scaleMax)) apply(X, 2, max) else rep(ip$scaleMax, length(items))
  if (any(hi <= lo)) stop(analysis_failure("ipma", "IPMA could not rescale an indicator whose minimum equals its maximum."))
  Xr <- sweep(sweep(X, 2, lo, "-"), 2, hi - lo, "/") * 100
  hocs <- Filter(is_hoc, model$constructs)
  low <- setdiff(colnames(m$outer_weights), vapply(hocs, `[[`, "", "name"))
  W <- m$outer_weights[items, low, drop = FALSE]
  Wu <- W / apply(Xr, 2, stats::sd)
  Wr <- sweep(Wu, 2, colSums(Wu), "/")
  LV <- Xr %*% Wr
  # Higher-order constructs (two-stage approach): the rescaled (0-100) dimension scores are their indicators, with the
  # second-stage weights rescaled in the same way (Ringle & Sarstedt, 2016; Sarstedt et al., 2019).
  dimW <- list()
  for (h in hocs) {
    dims <- vapply(h$dimensions, function(d) byid[[d]]$name, "")
    wu <- m$outer_weights[dims, h$name] / apply(LV[, dims, drop = FALSE], 2, stats::sd)
    dimW[[h$name]] <- wu / sum(wu)
    LV <- cbind(LV, LV[, dims, drop = FALSE] %*% dimW[[h$name]])
    colnames(LV)[ncol(LV)] <- h$name
  }
  lv_sd <- apply(LV, 2, stats::sd)
  te <- seminr:::total_effects(m$path_coef)
  importance <- te[, target] * lv_sd[target] / lv_sd[rownames(te)]
  performance <- colMeans(LV)
  preds <- names(importance)[abs(importance) > 0]
  cons <- lapply(preds, function(cn) list(construct = cn, importance = unname(importance[cn]), performance = unname(performance[cn])))
  # A dimension affects the target only through its higher-order construct: importance = rescaled weight x its importance.
  dim_imp <- list()
  for (h in names(dimW)) if (h %in% preds) for (d in names(dimW[[h]])) dim_imp[[d]] <- unname(dimW[[h]][d] * importance[h])
  ind <- list()
  for (c in model$constructs) {
    if (is_hoc(c)) {
      for (d in names(dimW[[c$name]])) {
        w <- unname(dimW[[c$name]][d])
        ind[[length(ind) + 1]] <- list(construct = c$name, indicator = d, dimension = TRUE, rescaledWeight = w,
                                       importance = dim_imp[[d]], performance = unname(performance[d]), min = 0, max = 100)
      }
      next
    }
    ci <- if (c$name %in% preds) unname(importance[c$name]) else dim_imp[[c$name]]
    for (it in c$indicators) {
      w <- Wr[it, c$name]
      ind[[length(ind) + 1]] <- list(construct = c$name, indicator = it, rescaledWeight = w,
                                     importance = if (!is.null(ci)) w * ci else NULL,
                                     performance = mean(Xr[, it]), min = unname(lo[match(it, items)]), max = unname(hi[match(it, items)]))
    }
  }
  neg <- unname(c(unlist(lapply(Filter(Negate(is_hoc), model$constructs), function(c) c$indicators[Wr[c$indicators, c$name] < 0])),
                  unlist(lapply(dimW, function(w) names(w)[w < 0]))))
  list(target = target, targetPerformance = unname(performance[target]),
       scale = if (is.null(ip$scaleMin)) "observed" else "theoretical", scaleMin = ip$scaleMin, scaleMax = ip$scaleMax,
       constructs = cons, indicators = ind, negativeWeights = arr(neg),
       cipma = if (isTRUE(ip$cipma) && length(preds)) run_cipma(LV, target, preds, importance, performance, ip) else NULL)
}

# ---- cIPMA ---------------------------------------------------------------------------
# Combined importance-performance map analysis (Hauff, Richter, Sarstedt & Ringle, 2024): the IPMA's rescaled (0-100)
# construct scores are analysed with NCA (effect size d, permutation p), so that the map shows which predecessors are
# also necessary conditions. The bottleneck table in actual values gives the level of each condition required for the
# chosen outcome level, and the cases below that level are counted: they cannot reach the outcome level, whatever
# their other scores (single-condition bottleneck cases).

run_cipma <- function(LV, target, preds, importance, performance, ip) {
  ce <- ip$cipmaCeiling %||% "ce_fdh"
  level <- ip$cipmaLevel %||% 80
  dat <- as.data.frame(LV[, c(preds, target), drop = FALSE], check.names = FALSE)
  out <- NULL
  utils::capture.output(out <- NCA::nca_analysis(dat, x = preds, y = target, ceilings = ce, test.rep = 0,
                                                 bottleneck.x = "actual", bottleneck.y = "actual", steps = c(0, level)))   # a vector: outcome values, not a number of steps
  test <- if ((ip$cipmaTestRep %||% 0) > 0) nca_fast_test(dat, preds, target, ce, ip$cipmaTestRep) else NULL
  n <- nrow(dat)
  ymax <- max(dat[[target]])
  rows <- lapply(preds, function(cn) {
    pr <- out$summaries[[cn]]$params
    d <- num_or_null(pr["Effect size", ce])
    p <- num_or_null(test[[cn]][[ce]][["p"]])
    req <- attr(out$bottlenecks[[ce]][[cn]], "mpx.actual")
    req <- if (is.null(req) || length(req) < 2 || is.na(req[2]) || !is.finite(req[2])) NULL else unname(req[2])   # row 2: the outcome level
    below <- if (is.null(req)) 0L else sum(dat[[cn]] < req)
    list(construct = cn, importance = unname(importance[cn]), performance = unname(performance[cn]), effectSize = d, p = p,
         necessary = isTRUE(!is.null(d) && d > 0 && (is.null(p) || p < 0.05)), required = req, casesBelow = below, pctBelow = 100 * below / n)
  })
  nec <- Filter(function(r) r$necessary && !is.null(r$required), rows)
  any_below <- if (length(nec)) sum(Reduce(`|`, lapply(nec, function(r) dat[[r$construct]] < r$required))) else 0L
  list(target = target, ceiling = ce, outcomeLevel = level, outcomeAboveScope = level > ymax, testRep = ip$cipmaTestRep %||% 0, n = n,
       constructs = rows, casesBelowAny = any_below, pctBelowAny = 100 * any_below / n,
       ncaVersion = as.character(utils::packageVersion("NCA")))
}

# ---- NCA ---------------------------------------------------------------------------

# Fast NCA permutation test: the same permutations, peers and ceiling formulas as NCA:::p_test, but only the effect
# size is computed for each permutation (NCA also builds plots, bottlenecks and inefficiencies every time).

# NCA:::p_peers for an upper-left ceiling, on the few points that can be peers.
nca_fast_peers <- function(x, y) {
  o <- order(x, y); xs <- x[o]; ys <- y[o]
  run <- cummax(c(-Inf, ys[-length(ys)]))
  keep <- c(TRUE, ys[-1] >= run[-1] - (abs(run[-1]) + abs(ys[-1])) * 1e-5)   # superset of possible peers
  xs <- xs[keep]; ys <- ys[keep]
  eq <- function(a, b) abs(a - b) <= min(abs(a), abs(b)) / 1e6               # NCA:::p_is_equal
  px <- numeric(length(xs)); py <- numeric(length(xs)); k <- 1L
  px[1] <- xs[1]; py[1] <- ys[1]
  if (length(xs) > 1) for (i in 2:length(xs)) {
    xc <- xs[i]; yc <- ys[i]
    xe <- eq(px[k], xc); ye <- eq(py[k], yc)
    if (yc > py[k] || (xe && ye)) {
      while (xe && !ye) {
        k <- k - 1L
        if (k == 0L) break
        xe <- eq(px[k], xc); ye <- eq(py[k], yc)
      }
      k <- k + 1L; px[k] <- xc; py[k] <- yc
    }
  }
  cbind(X = px[seq_len(k)], Y = py[seq_len(k)])
}

nca_fast_effect <- function(ceiling, ld, peers) {
  if (ceiling == "ce_fdh") return(NCA:::p_ce_ceiling(ld, peers, "fdh") / ld$scope.area)
  if (nrow(unique(peers)) <= 1) return(0)
  b <- stats::.lm.fit(cbind(1, peers[, 1]), peers[, 2])$coefficients
  NCA:::p_ceiling(ld, b[2], b[1]) / ld$scope.area
}

# p values and p accuracy for every condition and ceiling, as NCA::nca_analysis(test.rep = rep) reports them.
nca_fast_test <- function(data, x, y, ceilings, rep, p_confidence = 0.95) {
  cleaned <- NCA:::p_validate_clean(data, x, y)
  scope <- NCA:::p_scope(x, NULL)
  h <- nrow(cleaned$x)
  samples <- lapply(seq_len(rep), function(i) { set.seed(i); sample(1:h, h, replace = FALSE) })
  if (rep <= 720) {
    while (length(unique(samples)) < rep) { set.seed(length(samples)); samples[[length(samples) + 1]] <- sample(1:h, h, replace = FALSE) }
    samples <- unique(samples)
  }
  out <- list()
  for (id in seq_along(x)) {
    ld <- NCA:::p_create_loop_data(cleaned$x, cleaned$y, scope, rep(FALSE, length(x)), FALSE, id, 0.95)
    xv <- as.numeric(ld$x); y0 <- as.numeric(ld$y)
    obs_peers <- nca_fast_peers(xv, y0)
    for (ce in ceilings) {
      obs <- nca_fast_effect(ce, ld, obs_peers)
      sims <- vapply(samples, function(s) nca_fast_effect(ce, ld, nca_fast_peers(xv, y0[s])), 0)
      sims <- sims[!is.na(sims)]
      p <- max((sum(sims >= obs) + 1) / (length(sims) + 1), 1 / rep)
      acc <- stats::qnorm(p_confidence + 0.5 * (1 - p_confidence)) * sqrt(p * (1 - p) / rep)
      if (h <= 6 && rep == factorial(h)) acc <- 0
      out[[x[id]]][[ce]] <- c(p = p, acc = acc)
    }
  }
  out
}

# NCA of the conditions in `dat` (columns `conds`) for `target`: effect sizes, permutation p values, bottlenecks, plots.
nca_block <- function(dat, conds, target, nc, level = "construct", construct_of = NULL) {
  out <- NULL
  # The permutation test runs separately (nca_fast_test): NCA's own test gives the same p values but takes minutes.
  utils::capture.output(out <- NCA::nca_analysis(dat, x = conds, y = target, ceilings = nc$ceilings, test.rep = 0, steps = nc$steps))
  test <- if (nc$testRep > 0) nca_fast_test(dat, conds, target, nc$ceilings, nc$testRep) else NULL
  rows <- list()
  for (cn in conds) {
    s <- out$summaries[[cn]]
    pr <- s$params; gl <- s$global
    for (ce in nc$ceilings) {
      g <- function(k) if (k %in% rownames(pr)) num_or_null(pr[k, ce]) else NULL
      rows[[length(rows) + 1]] <- c(list(condition = cn, ceiling = ce, ceilingZone = g("Ceiling zone"), effectSize = g("Effect size"),
                                         cAccuracy = g("c-accuracy"), fit = g("Fit"),
                                         p = num_or_null(test[[cn]][[ce]][["p"]]), pAccuracy = num_or_null(test[[cn]][[ce]][["acc"]]),
                                         scope = num_or_null(gl["Scope", 1]), level = level),
                                    if (!is.null(construct_of)) list(construct = construct_of[[cn]]))
    }
  }
  bottlenecks <- lapply(nc$ceilings, function(ce) {
    b <- as.data.frame(out$bottlenecks[[ce]], check.names = FALSE, stringsAsFactors = FALSE)
    cols <- names(b)
    list(ceiling = ce, level = level, columns = arr(cols),
         rows = lapply(seq_len(nrow(b)), function(i) lapply(cols, function(k) { v <- b[i, k]; if (is.na(v)) NULL else as.character(v) })))
  })
  plots <- lapply(conds, function(cn) {
    pts <- dat[, c(cn, target)]
    peers <- out$peers[["ce_fdh"]][[cn]]
    c(list(condition = cn, level = level, x = arr(unname(pts[[cn]])), y = arr(unname(pts[[target]])),
           ceFdhPeers = if (is.null(peers)) list() else lapply(seq_len(nrow(peers)), function(i) arr(unname(as.numeric(peers[i, 1:2])))),
           crFdh = if ("cr_fdh" %in% nc$ceilings) { p <- out$summaries[[cn]]$params; list(slope = num_or_null(p["Slope", "cr_fdh"]), intercept = num_or_null(p["Intercept", "cr_fdh"])) } else NULL),
      if (!is.null(construct_of)) list(construct = construct_of[[cn]]))
  })
  list(rows = rows, bottlenecks = bottlenecks, plots = plots)
}

run_nca <- function(m, model, nc, progress) {
  byid <- construct_by_id(model)
  target <- byid[[nc$target]]$name
  cids <- nca_conditions(model, nc)
  conds <- unname(vapply(cids, function(i) byid[[i]]$name, ""))  # unnamed -> JSON arrays
  scores <- as.data.frame(m$construct_scores)
  if (!is.null(nc$seed)) set.seed(nc$seed)
  main <- nca_block(scores, conds, target, nc)
  # Formative (or mixed) constructs: the indicators themselves as conditions, because a formative construct score
  # can hide which indicator is the bottleneck (Richter et al., 2020; Sukhov et al., 2023). The outcome stays the
  # construct score; NCA effect sizes do not depend on the scale of the condition.
  lvl <- nc$itemLevel %||% "none"
  icons <- Filter(function(c) lvl == "all" || (lvl == "formative" && identical(c$mode, "composite_b")), lapply(cids, function(i) byid[[i]]))
  items <- NULL
  if (length(icons)) {
    construct_of <- list()
    for (c in icons) for (it in c$indicators) if (it %in% colnames(m$data)) construct_of[[it]] <- c$name
    inames <- names(construct_of)
    if (length(inames)) {
      dat <- data.frame(m$data[, inames, drop = FALSE], check.names = FALSE)
      dat[[target]] <- scores[[target]]
      items <- nca_block(dat, inames, target, nc, level = "indicator", construct_of = construct_of)
    }
  }
  list(target = target, conditions = arr(conds), ceilings = arr(nc$ceilings), testRep = nc$testRep, seed = nc$seed, steps = nc$steps, itemLevel = lvl,
       itemConstructs = arr(vapply(icons, `[[`, "", "name")),
       results = c(main$rows, items$rows), bottlenecks = c(main$bottlenecks, items$bottlenecks), plots = c(main$plots, items$plots),
       ncaVersion = as.character(utils::packageVersion("NCA")))
}

# ---- MGA -----------------------------------------------------------------------------

run_mga <- function(df, prep, model, settings, cores) {
  mg <- settings$mga
  g <- as.character(df[[mg$groupVariable]])[prep$rows]
  keep <- !is.na(g) & g %in% c(mg$group1, mg$group2)
  d <- prep$data[keep, , drop = FALSE]
  cond <- g[keep] %in% mg$group1
  miss_fun <- if (settings$missing$treatment == "mean_replacement") seminr::mean_replacement else stats::na.omit
  iw <- if (settings$innerWeights == "path_factorial") seminr::path_factorial else seminr::path_weighting
  m <- capture_conditions(seminr::estimate_pls(d, build_measurement_model(model), build_structural_model(model),
                                               inner_weights = iw, missing = miss_fun, missing_value = NA,
                                               maxIt = settings$maxIt, stopCriterion = settings$stopCriterion))$value
  mga_fun <- if (nrow(m$smMatrix) == 1) pls_mga_one_path else seminr::estimate_pls_mga
  res <- tryCatch(capture_conditions(mga_fun(m, condition = cond, nboot = mg$nboot, cores = cores, seed = mg$seed)),
                  error = function(e) stop(analysis_failure("mga", "The multigroup analysis failed, so no MGA results are reported.",
                                                            "Group-specific bootstrapping can fail in small groups or with PLSc constructs. Check the group sizes.",
                                                            sanitize_message(conditionMessage(e)))))
  b <- as.data.frame(unclass(res$value))
  used <- suppressWarnings(as.integer(sub(".*Using (\\d+) bootstrapped.*", "\\1", grep("Using \\d+ bootstrapped", res$messages, value = TRUE))))
  list(groupVariable = mg$groupVariable, group1 = arr(mg$group1), group2 = arr(mg$group2),
       n1 = sum(cond), n2 = sum(!cond), nExcluded = sum(!keep), nboot = mg$nboot,
       nbootUsed = if (length(used)) used[1] else mg$nboot, seed = mg$seed, cores = cores,
       paths = lapply(seq_len(nrow(b)), function(i) list(
         from = b$source[i], to = b$target[i], estimate = num_or_null(b$estimate[i]),
         group1 = num_or_null(b$group1_beta[i]), group2 = num_or_null(b$group2_beta[i]), diff = num_or_null(b$diff[i]),
         group1BootMean = num_or_null(b$group1_beta_mean[i]), group2BootMean = num_or_null(b$group2_beta_mean[i]),
         p = num_or_null(b$pls_mga_p[i]))))
}

#' Name of MGA group i for table headings: its values ("Female"; "Urban, Peri-urban"), or "Group i" if it has none.
mga_label <- function(g, i) {
  v <- unlist(g[[paste0("group", i)]])
  if (length(v)) paste(v, collapse = ", ") else paste("Group", i)
}

#' SEMinR's estimate_pls_mga() for a model with a single path: the same group estimates, bootstraps and PLS-MGA
#' probability (Henseler et al., 2009). SEMinR's own function fails there because its one-column bootstrap matrix
#' is dropped to a vector.
pls_mga_one_path <- function(pls_model, condition, nboot = 2000, ...) {
  g1 <- seminr:::rerun(pls_model, data = pls_model$rawdata[condition, ])
  g2 <- seminr:::rerun(pls_model, data = pls_model$rawdata[!condition, ])
  b1 <- seminr::bootstrap_model(seminr_model = g1, nboot = nboot, ...)
  b2 <- seminr::bootstrap_model(seminr_model = g2, nboot = nboot, ...)
  s <- pls_model$smMatrix[1, "source"]; t <- pls_model$smMatrix[1, "target"]
  d1 <- b1$boot_paths[s, t, seq_len(b1$boots)]; d2 <- b2$boot_paths[s, t, seq_len(b2$boots)]
  J <- min(length(d1), length(d2))
  if (J < nboot) message(paste("NOTE: Using", J, "bootstrapped results of each group after removing inadmissible runs"))
  d1 <- d1[seq_len(J)]; d2 <- d2[seq_len(J)]
  m1 <- mean(d1); m2 <- mean(d2)
  all <- expand.grid(d1, d2)
  data.frame(source = s, target = t, estimate = pls_model$path_coef[s, t], group1_beta = g1$path_coef[s, t], group2_beta = g2$path_coef[s, t],
             diff = g1$path_coef[s, t] - g2$path_coef[s, t], group1_beta_mean = m1, group2_beta_mean = m2,
             pls_mga_p = 1 - sum((2 * m1 - all[, 1] - 2 * m2 + all[, 2]) > 0) / J^2)
}

# ---- PLSpredict for higher-order models (fold-wise two-stage) -------------------------------------
# SEMinR's predict_pls() has no method for higher-order constructs. This runs its k-fold procedure (Shmueli et al.,
# 2019) with the two-stage approach repeated inside every fold: the full two-stage model is estimated on the training
# cases only; the held-out cases' dimension scores are computed with the training stage-1 weights (so the held-out
# cases never help build their own predictors); SEMinR's one_stage_predict() then predicts them from the training
# stage-2 model. The LM benchmark regresses each indicator on the manifest items of the antecedents (a higher-order
# construct is represented by its dimensions' items). Without higher-order constructs this procedure gives exactly
# predict_pls()'s results (tested). The main estimation is not affected: this runs on copies, after it.

#' Dimension scores of new cases from a fitted stage-1 model, with nothing re-estimated: the items standardised with
#' the stage-1 (training) means and SDs, times its outer weights, which is how SEMinR scores the training cases.
stage1_scores <- function(first, newdata, dims) {
  it <- first$mmVariables
  z <- sweep(sweep(as.matrix(newdata[, it, drop = FALSE]), 2, first$meanData[it]), 2, first$sdData[it], "/")
  z %*% first$outer_weights[it, dims, drop = FALSE]
}

#' Method sentence for reports when PLSpredict ran on a higher-order model.
predict_hoc_note <- function(p) {
  paste0("The model contains a higher-order construct, for which SEMinR offers no PLSpredict; the two-stage approach was therefore ",
         "repeated within every fold, so that the dimension scores of the held-out cases were computed from the training cases' ",
         "stage-1 weights, and the linear benchmark used the dimensions' indicators.",
         if (length(p$notPredicted)) paste0(" Dimension scores of endogenous higher-order constructs (", paste(unlist(p$notPredicted), collapse = ", "), ") were not predicted.") else "")
}

#' The stage-2 model of a fitted two-stage model as an ordinary SEMinR model (the dimension scores are plain
#' indicators of the higher-order construct), so SEMinR's prediction functions, which re-estimate from the saved
#' specification, can use it. Same data, modes and settings, so the same estimates (tested).
stage2_plain <- function(fit) {
  mmx <- fit$mmMatrix
  specs <- lapply(fit$constructs, function(cn) {
    r <- mmx[mmx[, "construct"] == cn, , drop = FALSE]
    it <- unname(r[, "measurement"]); t <- r[1, "type"]
    if (t == "C") seminr::reflective(cn, it) else seminr::composite(cn, it, weights = if (t %in% c("B", "HOCB")) seminr::mode_B else seminr::mode_A)
  })
  sm <- do.call(seminr::relationships, lapply(seq_len(nrow(fit$smMatrix)), function(i) seminr::paths(from = fit$smMatrix[i, "source"], to = fit$smMatrix[i, "target"])))
  s <- fit$settings
  suppressMessages(seminr::estimate_pls(fit$data[, fit$mmVariables, drop = FALSE], do.call(seminr::constructs, specs), sm, inner_weights = s$inner_weights,
                                        missing = s$missing, missing_value = s$missing_value, maxIt = s$maxIt, stopCriterion = s$stopCriterion))
}

#' @param m the estimated two-stage model; `technique` "predict_DA" or "predict_EA". Returns the out-of-sample
#'   RMSE / MAE of the PLS and LM predictions (items in columns) and the items that were not predicted.
predict_pls_two_stage <- function(m, technique = "predict_DA", noFolds = 10, reps = 1) {
  ns <- asNamespace("seminr")
  tech <- if (technique == "predict_EA") seminr::predict_EA else seminr::predict_DA
  first <- m$first_stage_model
  dims <- if (is.null(first)) character(0) else setdiff(first$constructs, m$constructs)
  items <- if (is.null(first)) m$mmVariables else first$mmVariables   # manifest indicators (all items, stage 1)
  data <- m$data[, items, drop = FALSE]
  n <- nrow(data)
  sm <- m$smMatrix
  endo <- unique(sm[, "target"])
  hoc_names <- setdiff(m$constructs, if (is.null(first)) m$constructs else first$constructs)
  items_of <- function(cn) {   # manifest items of a construct (a higher-order construct: its dimensions' items)
    if (cn %in% hoc_names) unlist(lapply(m$mmMatrix[m$mmMatrix[, "construct"] == cn, "measurement"], function(d) first$mmMatrix[first$mmMatrix[, "construct"] == d, "measurement"]))
    else m$mmMatrix[m$mmMatrix[, "construct"] == cn, "measurement"]
  }
  # Indicators of an endogenous higher-order construct are dimension scores, not observed items: not predicted.
  skipped <- intersect(endo, hoc_names)
  targets <- setdiff(endo, skipped)
  target_items <- unlist(lapply(targets, items_of))
  if (!length(target_items)) stop("PLSpredict needs an endogenous construct measured by its own indicators; here only higher-order constructs are endogenous.")
  exo <- setdiff(unique(sm[, "source"]), endo)
  ord <- sample(n, n)
  od <- data[ord, , drop = FALSE]
  folds <- cut(seq_len(n), breaks = noFolds, labels = FALSE)
  pls <- matrix(NA_real_, n, length(target_items), dimnames = list(rownames(od), target_items))
  lm_ <- pls
  for (k in seq_len(noFolds)) {
    te <- which(folds == k); tr <- which(folds != k)
    train <- suppressMessages(seminr:::rerun(m, data = od[tr, , drop = FALSE]))
    test <- od[te, , drop = FALSE]
    if (length(dims)) {
      test <- cbind(test, stage1_scores(train$first_stage_model, test, dims))
      train <- stage2_plain(train)
    }
    pr <- ns$one_stage_predict(train, test, tech)
    pls[te, ] <- pr$predicted_items[, target_items, drop = FALSE]
    for (cn in targets) {
      ante <- if (technique == "predict_EA") exo else sm[sm[, "target"] == cn, "source"]
      x <- unique(unlist(lapply(ante, items_of)))
      for (it in items_of(cn)) {
        fit <- stats::lm(stats::reformulate(sprintf("`%s`", x), response = sprintf("`%s`", it)), data = od[tr, c(it, x), drop = FALSE])
        lm_[te, it] <- stats::predict(fit, newdata = od[te, x, drop = FALSE])
      }
    }
  }
  # Every repetition uses the same case order and folds (as in predict_pls()), so repeating does not change the result.
  res_pls <- as.matrix(od[, target_items, drop = FALSE]) - pls
  res_lm <- as.matrix(od[, target_items, drop = FALSE]) - lm_
  metric <- function(r) rbind(RMSE = sqrt(colMeans(r^2)), MAE = colMeans(abs(r)))
  list(PLS_out_of_sample = metric(res_pls), LM_out_of_sample = metric(res_lm),
       notPredicted = unname(unlist(lapply(skipped, items_of))), skippedConstructs = skipped)
}

# ---- R code -----------------------------------------------------------------------------

advanced_code <- function(model, settings, a) {
  byid <- construct_by_id(model)
  ip <- settings$ipma; nc <- settings$nca; mg <- settings$mga
  if (isTRUE(ip$enabled)) {
    a("")
    a("# IPMA (Ringle & Sarstedt, 2016), computed from the SEMinR estimates ------------")
    a("ipma_target <- ", r_str(byid[[ip$target]]$name))
    a("X <- as.matrix(pls_model$data[, indicators])            # data used for estimation")
    if (is.null(ip$scaleMin)) {
      a("scale_min <- apply(X, 2, min); scale_max <- apply(X, 2, max)   # observed bounds")
    } else {
      a("scale_min <- rep(", ip$scaleMin, ", ncol(X)); scale_max <- rep(", ip$scaleMax, ", ncol(X))   # theoretical bounds")
    }
    a("X_rescaled <- sweep(sweep(X, 2, scale_min, \"-\"), 2, scale_max - scale_min, \"/\") * 100")
    hocs <- Filter(is_hoc, model$constructs)
    a("W <- pls_model$outer_weights[indicators, ", if (length(hocs)) paste0("setdiff(colnames(pls_model$outer_weights), ", r_vec(vapply(hocs, `[[`, "", "name")), ")"), ", drop = FALSE]")
    a("W_unstd <- W / apply(X_rescaled, 2, sd)")
    a("W_rescaled <- sweep(W_unstd, 2, colSums(W_unstd), \"/\")")
    a("lv_rescaled <- X_rescaled %*% W_rescaled")
    for (h in hocs) {
      a("# Higher-order construct ", h$name, ": its rescaled dimension scores are its indicators (second-stage weights)")
      a("dims <- ", r_vec(vapply(h$dimensions, function(d) byid[[d]]$name, "")))
      a("w <- pls_model$outer_weights[dims, ", r_str(h$name), "] / apply(lv_rescaled[, dims], 2, sd)")
      a("lv_rescaled <- cbind(lv_rescaled, drop(lv_rescaled[, dims] %*% (w / sum(w))))")
      a("colnames(lv_rescaled)[ncol(lv_rescaled)] <- ", r_str(h$name))
    }
    a("total <- pls_summary$total_effects")
    a("ipma_performance <- colMeans(lv_rescaled)[rownames(total)]")
    a("ipma_importance <- total[, ipma_target] * sd(lv_rescaled[, ipma_target]) / apply(lv_rescaled[, rownames(total)], 2, sd)")
    a("ipma <- data.frame(importance = ipma_importance, performance = ipma_performance)")
    a("ipma[ipma$importance != 0, ]")
    if (isTRUE(ip$cipma)) {
      a("")
      a("# cIPMA (Hauff et al., 2024): NCA on the rescaled (0-100) construct scores, bottlenecks in actual values")
      a("library(NCA)")
      a("cipma_preds <- rownames(ipma)[ipma$importance != 0 & rownames(ipma) != ipma_target]")
      a("cipma_data <- as.data.frame(lv_rescaled[, c(cipma_preds, ipma_target)])")
      a("cipma_nca <- nca_analysis(cipma_data, x = cipma_preds, y = ipma_target, ceilings = ", r_str(ip$cipmaCeiling), ", test.rep = ", ip$cipmaTestRep, ",")
      a("                          bottleneck.x = \"actual\", bottleneck.y = \"actual\", steps = c(0, ", ip$cipmaLevel, "))   # outcome values: 0 and the outcome level")
      a("cipma_nca$summaries")
      a("required <- sapply(cipma_preds, function(x) attr(cipma_nca$bottlenecks[[", r_str(ip$cipmaCeiling), "]][[x]], \"mpx.actual\")[2])   # -Inf = not necessary")
      a("cases_below <- sapply(cipma_preds, function(x) mean(cipma_data[[x]] < required[x]) * 100)   # % of cases below the required level")
      a("cbind(ipma[cipma_preds, ], required, cases_below)")
    }
  }
  if (isTRUE(nc$enabled)) {
    conds <- vapply(nca_conditions(model, nc), function(i) byid[[i]]$name, "")
    a("")
    a("# Necessary Condition Analysis on the SEMinR construct scores (NCA package) -----")
    a("library(NCA)")
    a("scores <- as.data.frame(pls_model$construct_scores)")
    if (!is.null(nc$seed)) a("set.seed(", nc$seed, ")  # permutation test")
    a("nca_model <- nca_analysis(scores, x = ", r_vec(conds), ", y = ", r_str(byid[[nc$target]]$name), ",")
    a("                          ceilings = ", r_vec(nc$ceilings), ", test.rep = ", nc$testRep, ", steps = ", nc$steps, ")")
    a("nca_model$summaries     # effect sizes, accuracy, p values")
    a("nca_model$bottlenecks   # bottleneck tables")
    byidc <- construct_by_id(model)
    icons <- Filter(function(c) nc$itemLevel == "all" || (nc$itemLevel == "formative" && identical(c$mode, "composite_b")), lapply(nca_conditions(model, nc), function(i) byidc[[i]]))
    its <- unlist(lapply(icons, `[[`, "indicators"))
    if (length(its)) {
      a("# Single indicators of ", if (nc$itemLevel == "all") "the conditions" else "the formative conditions", " as conditions (Richter et al., 2020)")
      a("nca_items_data <- data.frame(pls_model$data[, ", r_vec(its), ", drop = FALSE], scores[, ", r_str(byid[[nc$target]]$name), ", drop = FALSE], check.names = FALSE)")
      a("nca_items <- nca_analysis(nca_items_data, x = ", r_vec(its), ", y = ", r_str(byid[[nc$target]]$name), ", ceilings = ", r_vec(nc$ceilings), ", test.rep = ", nc$testRep, ", steps = ", nc$steps, ")")
      a("nca_items$summaries")
    }
  }
  if (isTRUE(mg$enabled)) {
    a("")
    a("# Multigroup analysis (PLS-MGA, SEMinR) ------------------------------------------")
    a("group1_values <- ", r_vec(mg$group1))
    a("group2_values <- ", r_vec(mg$group2))
    a("in_groups <- !is.na(group) & group %in% c(group1_values, group2_values)")
    a("mga_base <- estimate_pls(data[in_groups, , drop = FALSE], measurement_model, structural_model,")
    a("                         inner_weights = ", settings$innerWeights, ", missing = ",
      if (settings$missing$treatment == "mean_replacement") "mean_replacement" else "na.omit",
      ", missing_value = NA, maxIt = ", settings$maxIt, ", stopCriterion = ", settings$stopCriterion, ")")
    one <- length(model$paths) == 1 && !length(model$moderations %||% list())
    if (one) {
      a("# SEMinR's estimate_pls_mga() fails for a model with a single path; the same computation for one path:")
      a("pls_mga_one_path <- ", paste(deparse(pls_mga_one_path), collapse = "\n"))
    }
    a("mga <- ", if (one) "pls_mga_one_path" else "estimate_pls_mga", "(mga_base, condition = group[in_groups] %in% group1_values,")
    a("                        nboot = ", mg$nboot, ", cores = ", settings$bootstrap$cores, ", seed = ", mg$seed %||% "NULL", ")")
    a("mga   # pls_mga_p: Henseler's one-sided PLS-MGA probability (see PathLab report)")
  }
}

# ---- Model fit ----------------------------------------------------------------------------------
# SRMR, d_ULS, d_G, chi-square and NFI for the saturated and the estimated model, computed from
# SEMinR's estimates with the published definitions (Henseler et al., 2014; Dijkstra & Henseler, 2015).
#   S      observed indicator correlation matrix of the data SEMinR analysed.
#   Sigma  model-implied indicator correlations: Lambda' P Lambda between blocks; within a composite
#          block the observed correlations; within a common-factor (PLSc) block lambda lambda'.
#   P      construct correlations, disattenuated with rho_A for common factors only (PLSc).
#          Saturated model: all construct correlations free. Estimated model: correlations implied by
#          the structural model (paths by OLS on P, as in PLSc).
# Validated against cSEM 0.7.1 (fit(), calculateSRMR/DL/DG/ChiSquare/NFI), including moderated models
# (compared with the equivalent linear model that treats SEMinR's interaction indicators as observed). d_G uses the natural
# logarithm as defined by Dijkstra & Henseler (2015); cSEM uses log10, so its d_G is smaller by (ln 10)^2.
model_fit <- function(m, model, reliability) {
  # Constructs, indicators and paths are read from the fitted SEMinR model, so interaction terms are
  # included as composite constructs measured by the indicators SEMinR built for them (two-stage: the
  # product of the first-stage scores; product-indicator / orthogonal: the product indicators).
  refl <- vapply(Filter(function(c) c$mode == "reflective", model$constructs), `[[`, "", "name")
  cons <- colnames(m$construct_scores)
  mmx <- m$mmMatrix
  items <- unique(mmx[mmx[, "construct"] %in% cons, "measurement"])
  interactions <- setdiff(cons, vapply(model$constructs, `[[`, "", "name"))
  X <- as.matrix(m$data[, items, drop = FALSE])
  # Composite loadings are the indicator-score correlations (S_jj w_j); SEMinR reports the same values for
  # ordinary composites but not for interaction indicators, so they are computed here. Common factors keep
  # SEMinR's consistent (PLSc) loadings.
  L <- m$outer_loadings[items, cons, drop = FALSE]
  for (k in setdiff(cons, refl)) {
    ii <- mmx[mmx[, "construct"] == k, "measurement"]
    L[ii, k] <- stats::cor(X[, ii, drop = FALSE], m$construct_scores[, k])
  }
  S <- stats::cor(X)
  n <- nrow(X)
  P <- stats::cor(m$construct_scores[, cons, drop = FALSE])
  rho <- setNames(rep(1, length(cons)), cons)
  if (length(refl)) rho[refl] <- reliability[refl, "rhoA"]
  P <- P / sqrt(outer(rho, rho)); diag(P) <- 1

  implied <- function(Pc) {
    Sig <- L %*% Pc %*% t(L)
    for (k in cons) {
      ii <- which(L[, k] != 0)
      Sig[ii, ii] <- if (k %in% refl) L[ii, k] %o% L[ii, k] else S[ii, ii]
    }
    diag(Sig) <- 1
    Sig
  }
  # Structural model: OLS on P for each endogenous construct.
  pc <- matrix(0, length(cons), length(cons), dimnames = list(cons, cons))
  smx <- m$smMatrix
  for (r in seq_len(nrow(smx))) pc[smx[r, "source"], smx[r, "target"]] <- 1
  endo <- cons[colSums(pc) > 0]; exo <- setdiff(cons, endo)
  for (j in endo) { a <- cons[pc[, j] != 0]; pc[a, j] <- solve(P[a, a, drop = FALSE], P[a, j]) }
  seminr_differs <- max(abs(pc - m$path_coef[cons, cons])) > 1e-6
  if (length(endo)) {
    B <- t(pc)[endo, endo, drop = FALSE]; G <- t(pc)[endo, exo, drop = FALSE]; Phi <- P[exo, exo, drop = FALSE]
    iB <- solve(diag(length(endo)) - B)
    zeta <- diag(1 - rowSums(t(pc)[endo, , drop = FALSE] * P[endo, , drop = FALSE]), length(endo))
    Cee <- iB %*% (G %*% Phi %*% t(G) + zeta) %*% t(iB); diag(Cee) <- 1
    Cxe <- Phi %*% t(G) %*% t(iB)
    Pest <- rbind(cbind(Phi, Cxe), cbind(t(Cxe), Cee))[cons, cons]
  } else Pest <- P

  logdetS <- tryCatch(determinant(S, logarithm = TRUE), error = function(e) NULL)
  pd <- !is.null(logdetS) && logdetS$sign > 0
  F_null <- if (pd) -as.numeric(logdetS$modulus) else NA_real_
  measures <- function(Sig) {
    R <- S - Sig
    out <- list(SRMR = sqrt(mean(R[lower.tri(R, diag = TRUE)]^2)), d_ULS = 0.5 * sum(R^2), d_G = NA_real_, chiSquare = NA_real_, NFI = NA_real_)
    if (pd) {
      SinvSig <- tryCatch(solve(S, Sig), error = function(e) NULL)
      SSinv <- tryCatch(S %*% solve(Sig), error = function(e) NULL)
      if (!is.null(SinvSig) && !is.null(SSinv)) {
        ev <- Re(eigen(SinvSig, only.values = TRUE)$values)
        if (all(ev > 0)) out$d_G <- 0.5 * sum(log(ev)^2)
        dS <- determinant(SSinv, logarithm = TRUE)
        if (dS$sign > 0) {
          f_ml <- sum(diag(SSinv)) - as.numeric(dS$modulus) - ncol(S)
          out$chiSquare <- (n - 1) * f_ml
          out$NFI <- (F_null - f_ml) / F_null
        }
      }
    }
    lapply(out, num_or_null)
  }
  composites_corrected <- if (length(refl)) setdiff(vapply(Filter(function(c) c$mode == "composite_a", model$constructs), `[[`, "", "name"), refl) else character(0)
  list(available = TRUE, n = n, indicators = length(items), interactionTerms = arr(interactions),
       modelType = if (!length(refl)) "composite" else if (length(refl) == length(cons) - length(interactions)) "factor" else "mixed",
       saturated = measures(implied(P)), estimated = measures(implied(Pest)),
       seminrPathsDiffer = seminr_differs && length(composites_corrected) > 0,
       affectedComposites = arr(if (seminr_differs) composites_corrected else character(0)))
}

# ---- Common method bias -------------------------------------------------------------------------
# Three established checks, computed from SEMinR's construct scores and the analysed indicator data:
#  1. Full collinearity VIFs (Kock, 2015; Kock & Lynn, 2012): each construct regressed on all other
#     constructs; VIF_j = 1 / (1 - R2_j) = diag(solve(cor(scores))). Values above 3.3 suggest CMB.
#  2. Harman's single-factor test (Podsakoff et al., 2003): share of the total indicator variance captured
#     by the first unrotated principal component (and by a one-factor ML factor analysis). Above 50% is the
#     usual warning sign; the test is known to be insensitive and should not be used alone.
#  3. Marker variable technique (Lindell & Whitney, 2001): with a theoretically unrelated marker, r_M is the
#     smallest positive correlation between the marker and the constructs; construct correlations are
#     adjusted as r_A = (r_U - r_M) / (1 - r_M), tested with t = r_A / sqrt((1 - r_A^2) / (n - 3)).
run_cmb <- function(m, model, df, prep, cmb) {
  # Constructs with scores in the (second-stage) model; dimensions of higher-order constructs are represented by them.
  cons <- intersect(vapply(model$constructs, `[[`, "", "name"), colnames(m$construct_scores))
  scores <- m$construct_scores[, cons, drop = FALSE]
  n <- nrow(scores)
  R_c <- stats::cor(scores)
  vif <- NULL
  if (length(cons) >= 2) {
    inv <- tryCatch(solve(R_c), error = function(e) NULL)
    vif <- list(threshold = 3.3,
                constructs = lapply(cons, function(k) list(construct = k, vif = if (is.null(inv)) NULL else num_or_null(inv[k, k]))))
  }

  items <- model_indicators(model)
  X <- as.matrix(m$data[, items, drop = FALSE])
  R_i <- stats::cor(X)
  ev <- eigen(R_i, symmetric = TRUE, only.values = TRUE)$values
  fa1 <- tryCatch(suppressWarnings(stats::factanal(covmat = R_i, factors = 1, n.obs = n)), error = function(e) NULL)
  harman <- list(indicators = length(items), firstComponentShare = num_or_null(ev[1] / length(items)),
                 firstEigenvalue = num_or_null(ev[1]), componentsAbove1 = sum(ev > 1),
                 singleFactorShare = if (is.null(fa1)) NULL else num_or_null(sum(fa1$loadings[, 1]^2) / length(items)),
                 threshold = 0.5)

  marker <- NULL
  mk <- cmb$markerVariable %||% ""
  if (nzchar(mk) && mk %in% names(df) && is.numeric(df[[mk]])) {
    x <- as.numeric(df[[mk]])[prep$rows]
    ok <- !is.na(x)
    rm_all <- vapply(cons, function(k) suppressWarnings(stats::cor(x[ok], scores[ok, k])), 0)
    pos <- rm_all[is.finite(rm_all) & rm_all > 0]
    r_m <- if (length(pos)) min(pos) else 0
    src <- if (length(pos)) names(pos)[which.min(pos)] else NULL
    n_m <- sum(ok)
    pairs <- list()
    for (i in seq_along(cons)) for (j in seq_along(cons)) if (j > i) {
      ru <- R_c[i, j]; ra <- (ru - r_m) / (1 - r_m)
      p_of <- function(r, df_) { t <- r / sqrt((1 - r^2) / df_); 2 * stats::pt(-abs(t), df_) }
      pairs[[length(pairs) + 1]] <- list(a = cons[i], b = cons[j], original = num_or_null(ru), adjusted = num_or_null(ra),
                                         pOriginal = num_or_null(p_of(ru, n_m - 2)), pAdjusted = num_or_null(p_of(ra, n_m - 3)))
    }
    marker <- list(variable = mk, n = n_m, nMissing = sum(!ok), rM = num_or_null(r_m), rMConstruct = src,
                   markerCorrelations = lapply(cons, function(k) list(construct = k, r = num_or_null(rm_all[[k]]))),
                   pairs = pairs)
  }
  list(fullCollinearity = vif, harman = harman, marker = marker)
}

cmb_code <- function(model, settings, a) {
  # As in run_cmb: dimensions of higher-order constructs have no scores in the second-stage model.
  dims <- unlist(lapply(model$constructs, `[[`, "dimensions"))
  cons <- vapply(Filter(function(c) !c$id %in% dims, model$constructs), `[[`, "", "name")
  a("")
  a("# Common method bias ---------------------------------------------------------")
  a("scores <- pls_model$construct_scores[, ", r_vec(cons), ", drop = FALSE]")
  if (length(cons) >= 2) {
    a("# Full collinearity VIFs (Kock, 2015): values above 3.3 suggest common method bias.")
    a("full_vif <- diag(solve(cor(scores)))")
  }
  a("# Harman's single-factor test: share of indicator variance in the first unrotated component.")
  a("harman_share <- eigen(cor(pls_model$data[, indicators]), only.values = TRUE)$values[1] / length(indicators)")
  mk <- settings$cmb$markerVariable %||% ""
  if (nzchar(mk)) {
    a("# Marker variable technique (Lindell & Whitney, 2001). 'marker' must be aligned with the analysed rows;")
    a("# it is read here from the original file and subset like the indicators.")
    a("marker <- as.numeric(plain_marker[[", r_str(mk), "]])")
    a("ok <- !is.na(marker)")
    a("r_marker <- apply(scores[ok, , drop = FALSE], 2, function(s) cor(marker[ok], s))")
    a("r_M <- if (any(r_marker > 0)) min(r_marker[r_marker > 0]) else 0")
    a("adjusted <- (cor(scores) - r_M) / (1 - r_M); diag(adjusted) <- 1")
  }
}
