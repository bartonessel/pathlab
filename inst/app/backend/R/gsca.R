# Generalized structured component analysis (GSCA; Hwang & Takane, 2004, 2014) estimated with cSEM
# (cSEM::csem(.approach_weights = "GSCA")). Composite constructs (Mode A/B in the diagram) are components; common
# factors (reflective) use GSCAm (Hwang, Takane & Jung, 2017; .disattenuate = TRUE).
# Moderation uses the two-stage approach: stage 1 estimates the main-effects model, the interaction term is the
# product of the standardised component scores of predictor and moderator (a single-indicator component), and stage 2
# estimates the model with it. cSEM's own resampling cannot repeat both stages, so bootstrapping is done here: every
# bootstrap sample repeats the whole estimation (both stages), with construct-level sign correction.
# GSCA fit (FIT, AFIT, FIT_M, FIT_S) follows the GSCA definitions; GFI and SRMR come from cSEM. The data are never
# changed: names are made syntactic only in the model copy, the project missing-value code is treated as missing and
# rows with missing indicator values are left out (listwise).

#' A clear message when cSEM is not installed (an optional package, since GSCA is one of several estimators).
gsca_need <- function() need_packages("cSEM", "GSCA analysis", "gsca_unavailable")

normalize_gsca_settings <- function(raw) {
  raw <- raw %||% list()
  b <- raw[["bootstrap"]] %||% list()
  ci <- suppressWarnings(as.numeric(raw[["ciLevel"]] %||% 0.95))
  if (is.na(ci) || ci <= 0.5 || ci >= 1) ci <- 0.95
  list(
    bootstrap = list(enabled = !isFALSE(b$enabled), nboot = int_in(b$nboot, 500L, 100L, 5000L, "The number of bootstrap samples"),
                     seed = int_in(b$seed, 2026L, 1L, .Machine$integer.max, "The bootstrap seed")),
    ciLevel = ci
  )
}

#' Model problems: the PLS-SEM model checks (constructs, indicators, paths, moderations), plus what GSCA here does not do.
#' `ds`: the dataset (data, profile, roles); a plain data frame is profiled here.
validate_gsca <- function(model, ds) {
  if (is.data.frame(ds)) ds <- list(data = ds, profile = profile_dataset(ds))
  issues <- Filter(function(i) !identical(i$code, "no_paths"), validate_model(model, normalize_settings(list(missing = list(treatment = "listwise"))), ds)$issues)
  issues <- lapply(issues, function(i) { i$message <- sub("A PLS path model", "A GSCA model", i$message, fixed = TRUE); i })
  add <- function(sev, code, msg, hint = NULL) issues[[length(issues) + 1]] <<- model_issue(sev, code, msg, hint = hint)
  if (has_hoc(model)) add("error", "gsca_hoc", "Higher-order constructs are not supported in GSCA analyses yet.", "Use a PLS-SEM analysis for higher-order constructs.")
  if (!length(model$paths)) add("warning", "gsca_no_paths", "The model has no structural paths; only the measurement model is estimated.")
  issues
}

#' Syntactic names for cSEM and the maps back to display names.
gsca_names <- function(model) {
  cons <- model$constructs
  cn <- setNames(safe_names(vapply(cons, `[[`, "", "name"), "C_"), vapply(cons, `[[`, "", "id"))
  inds <- model_indicators(model)
  vn <- setNames(safe_names(inds, "x_"), inds)
  clash <- cn %in% vn
  if (any(clash)) cn[clash] <- paste0(cn[clash], "_c")
  mods <- model$moderations %||% list()
  int <- setNames(vapply(seq_along(mods), function(k) paste0("INT", k, "_", cn[[mods[[k]]$iv]], "_x_", cn[[mods[[k]]$moderator]]), ""),
                  vapply(mods, `[[`, "", "id"))
  byid <- construct_by_id(model)
  int_label <- setNames(vapply(mods, function(m) paste0(byid[[m$iv]]$name, " × ", byid[[m$moderator]]$name), ""), unname(int))
  list(cn = cn, vn = vn, int = int,
       back = c(setNames(vapply(cons, `[[`, "", "name"), unname(cn)), setNames(names(vn), unname(vn)), int_label))
}

#' cSEM syntax. `with_int`: add the interaction components (stage 2) as single-indicator composites.
gsca_syntax <- function(model, nmap, with_int = FALSE) {
  lines <- character(0)
  for (c in model$constructs) {
    op <- if (identical(c$mode, "reflective")) "=~" else "<~"
    lines <- c(lines, sprintf("%s %s %s", nmap$cn[[c$id]], op, paste(nmap$vn[unlist(c$indicators)], collapse = " + ")))
  }
  mods <- model$moderations %||% list()
  # The single indicator needs its own name (cSEM reads "X <~ X" as a higher-order construct).
  if (with_int) for (m in mods) lines <- c(lines, sprintf("%s <~ %s_ind", nmap$int[[m$id]], nmap$int[[m$id]]))
  for (to in unique(vapply(model$paths, `[[`, "", "to"))) {
    rhs <- vapply(Filter(function(p) p$to == to, model$paths), function(p) nmap$cn[[p$from]], "")
    if (with_int) rhs <- c(rhs, vapply(Filter(function(m) m$dv == to, mods), function(m) nmap$int[[m$id]], ""))
    lines <- c(lines, sprintf("%s ~ %s", nmap$cn[[to]], paste(unique(rhs), collapse = " + ")))
  }
  paste(lines, collapse = "\n")
}

#' One GSCA estimation (both stages when the model has moderations) on data `D` (syntactic indicator names).
gsca_estimate <- function(D, model, nmap) {
  common <- any(vapply(model$constructs, function(c) identical(c$mode, "reflective"), TRUE))
  fit1 <- function(dat, syn) suppressWarnings(suppressMessages(cSEM::csem(dat, syn, .approach_weights = "GSCA", .disattenuate = common)))
  mods <- model$moderations %||% list()
  if (!length(mods)) return(list(fit = fit1(D, gsca_syntax(model, nmap)), data = D))
  s1 <- fit1(D, gsca_syntax(model, nmap))
  S <- scale(s1$Estimates$Construct_scores)
  for (m in mods) D[[paste0(nmap$int[[m$id]], "_ind")]] <- as.numeric(S[, nmap$cn[[m$iv]]] * S[, nmap$cn[[m$moderator]]])
  list(fit = fit1(D, gsca_syntax(model, nmap, with_int = TRUE)), data = D, stage1 = s1)
}

#' Estimates of one fit as named vectors (for bootstrapping): paths "from\rto", weights/loadings "construct\rindicator", R².
gsca_vectors <- function(fit, model, nmap) {
  P <- fit$Estimates$Path_estimates
  W <- fit$Estimates$Weight_estimates
  L <- fit$Estimates$Loading_estimates
  path_keys <- c(lapply(model$paths, function(p) c(nmap$cn[[p$from]], nmap$cn[[p$to]])),
                 lapply(model$moderations %||% list(), function(m) c(nmap$int[[m$id]], nmap$cn[[m$dv]])))
  paths <- vapply(path_keys, function(k) P[k[2], k[1]], 0)
  names(paths) <- vapply(path_keys, function(k) paste(k, collapse = "\r"), "")
  ml <- function(M) {
    out <- c()
    for (c in model$constructs) for (v in unlist(c$indicators)) out[paste(nmap$cn[[c$id]], nmap$vn[[v]], sep = "\r")] <- M[nmap$cn[[c$id]], nmap$vn[[v]]]
    out
  }
  r2 <- fit$Estimates$R2
  list(paths = paths, weights = ml(W), loadings = ml(L), r2 = r2)
}

#' GSCA fit indices. Each indicator and each component is a dependent variable of the GSCA model (indicators of
#' their component, endogenous components of their predictors): FIT is the share of their total variance the model
#' explains, FIT_M the share for the indicators, FIT_S for the components (exogenous components contribute 0), and
#' AFIT = 1 - (1 - FIT)(d0/d1) with d0 = N(J + P), d1 = d0 - G (G = free parameters: weights, loadings, paths).
gsca_fit_indices <- function(fit, n, n_paths) {
  Z <- scale(as.matrix(fit$Information$Data))
  G <- scale(fit$Estimates$Construct_scores)
  L <- fit$Estimates$Loading_estimates
  ind_r2 <- c()
  for (p in rownames(L)) for (j in colnames(L)[L[p, ] != 0]) {
    e <- Z[, j] - L[p, j] * G[, p]
    ind_r2[j] <- 1 - sum(e^2) / sum(Z[, j]^2)
  }
  comp_r2 <- setNames(rep(0, ncol(G)), colnames(G))
  r2 <- fit$Estimates$R2
  comp_r2[names(r2)] <- r2
  J <- length(ind_r2); P <- length(comp_r2)
  FIT <- (sum(ind_r2) + sum(comp_r2)) / (J + P)
  d0 <- n * (J + P); npar <- 2 * J + n_paths
  list(FIT = FIT, AFIT = 1 - (1 - FIT) * d0 / (d0 - npar), FITM = sum(ind_r2) / J, FITS = sum(comp_r2) / P,
       GFI = tryCatch(as.numeric(cSEM::calculateGFI(fit)), error = function(e) NULL),
       SRMR = tryCatch(as.numeric(cSEM::calculateSRMR(fit)), error = function(e) NULL),
       indicatorsR2 = ind_r2)
}

run_gsca <- function(df, model, gs, missing_code = NULL, progress = function(...) NULL) {
  gsca_need()
  issues <- validate_gsca(model, df)
  if (any(vapply(issues, function(i) i$severity == "error", TRUE)))
    stop(analysis_failure("validation", issues[[which(vapply(issues, function(i) i$severity == "error", TRUE))[1]]]$message,
                          "Fix the listed model problems and run again."))
  t0 <- Sys.time()
  warn <- vapply(Filter(function(i) i$severity == "warning", issues), `[[`, "", "message")
  nmap <- gsca_names(model)
  nm <- function(x) unname(ifelse(x %in% names(nmap$back), nmap$back[x], x))
  inds <- names(nmap$vn)
  d <- df[, inds, drop = FALSE]
  recoded <- 0L
  if (!is.null(missing_code)) for (v in inds) { hit <- !is.na(d[[v]]) & d[[v]] == missing_code; recoded <- recoded + sum(hit); d[[v]][hit] <- NA }
  n_input <- nrow(d)
  rows <- which(stats::complete.cases(d))
  D <- d[rows, , drop = FALSE]
  names(D) <- unname(nmap$vn[inds])
  n <- nrow(D)
  if (n < 30) stop(analysis_failure("data", sprintf("Only %d complete rows.", n), "GSCA needs more complete cases."))
  const <- names(D)[vapply(D, function(x) stats::sd(x) == 0, TRUE)]
  if (length(const)) stop(analysis_failure("data", sprintf("Constant indicator(s): %s.", paste(nm(const), collapse = ", ")), "Remove indicators without variation."))

  progress("estimate", "Estimating the GSCA model", 0.1)
  est <- tryCatch(gsca_estimate(D, model, nmap), error = function(e)
    stop(analysis_failure("estimation", paste("GSCA estimation failed:", sanitize_message(conditionMessage(e))), "Check the model and the data.")))
  fit <- est$fit
  ver <- tryCatch(cSEM::verify(fit), error = function(e) NULL)
  admissible <- is.null(ver) || all(!ver)
  if (!admissible) warn <- c(warn, "cSEM reports an inadmissible solution (for example a loading above 1 or a construct correlation matrix that is not positive definite). Interpret with caution.")
  v0 <- gsca_vectors(fit, model, nmap)
  mods <- model$moderations %||% list()
  # Conditional effects of the predictor at moderator values of -1 SD, the mean and +1 SD (standardised scale).
  cond_of <- function(paths) {
    out <- c()
    for (m in mods) {
      biv <- paths[[paste(nmap$cn[[m$iv]], nmap$cn[[m$dv]], sep = "\r")]]
      bint <- paths[[paste(nmap$int[[m$id]], nmap$cn[[m$dv]], sep = "\r")]]
      for (lv in c(-1, 0, 1)) out[paste(m$id, lv, sep = "\r")] <- biv + bint * lv
    }
    out
  }
  c0 <- cond_of(v0$paths)

  # Bootstrap: repeat the whole estimation (both stages) on resamples; construct-level sign correction.
  boot <- NULL
  if (isTRUE(gs$bootstrap$enabled)) {
    B <- gs$bootstrap$nboot
    set.seed(gs$bootstrap$seed)
    sign_ref <- vapply(model$constructs, function(c) sign(sum(v0$loadings[startsWith(names(v0$loadings), paste0(nmap$cn[[c$id]], "\r"))])) %||% 1, 0)
    names(sign_ref) <- unname(nmap$cn[vapply(model$constructs, `[[`, "", "id")])
    draws <- list(paths = list(), weights = list(), loadings = list(), r2 = list(), cond = list())
    ok <- 0L
    for (b in seq_len(B)) {
      if (b %% max(1L, B %/% 20L) == 0) progress("bootstrap", sprintf("Bootstrapping (%d of %d samples)", b, B), 0.15 + 0.75 * b / B)
      idx <- sample.int(n, n, replace = TRUE)
      vb <- tryCatch({
        eb <- gsca_estimate(D[idx, , drop = FALSE], model, nmap)
        gsca_vectors(eb$fit, model, nmap)
      }, error = function(e) NULL)
      if (is.null(vb)) next
      # Flip a construct whose loadings changed sign, and the paths that involve it.
      flip <- setNames(rep(1, length(sign_ref)), names(sign_ref))
      for (k in names(sign_ref)) {
        s <- sign(sum(vb$loadings[startsWith(names(vb$loadings), paste0(k, "\r"))]))
        if (s != 0 && s != sign_ref[[k]]) flip[[k]] <- -1
      }
      fl <- function(v) { pre <- sub("\r.*$", "", names(v)); v * ifelse(pre %in% names(flip), flip[pre], 1) }
      vb$weights <- fl(vb$weights); vb$loadings <- fl(vb$loadings)
      fr <- sub("\r.*$", "", names(vb$paths)); to <- sub("^.*\r", "", names(vb$paths))
      vb$paths <- vb$paths * ifelse(fr %in% names(flip), flip[fr], 1) * ifelse(to %in% names(flip), flip[to], 1)
      ok <- ok + 1L
      draws$paths[[ok]] <- vb$paths; draws$weights[[ok]] <- vb$weights; draws$loadings[[ok]] <- vb$loadings
      draws$r2[[ok]] <- vb$r2[names(v0$r2)]; draws$cond[[ok]] <- cond_of(vb$paths)
    }
    if (ok < 10) warn <- c(warn, "Fewer than 10 bootstrap samples could be estimated; no inference statistics are reported.")
    else {
      a <- (1 - gs$ciLevel) / 2
      inf <- function(orig, lst) {
        M <- do.call(rbind, lst)
        lapply(names(orig), function(k) {
          x <- M[, k]; se <- stats::sd(x); t <- if (se > 0) orig[[k]] / se else NA
          list(key = k, original = unname(orig[[k]]), mean = mean(x), se = se, t = t, p = if (is.finite(t)) 2 * stats::pt(-abs(t), df = ok - 1) else NA,
               lower = unname(stats::quantile(x, a)), upper = unname(stats::quantile(x, 1 - a)))
        })
      }
      boot <- list(requested = B, successful = ok, seed = gs$bootstrap$seed, ciLevel = gs$ciLevel,
                   paths = inf(v0$paths, draws$paths), weights = inf(v0$weights, draws$weights), loadings = inf(v0$loadings, draws$loadings),
                   r2 = if (length(v0$r2)) inf(v0$r2, draws$r2) else list(), cond = if (length(c0)) inf(c0, draws$cond) else list())
    }
  }
  progress("results", "Collecting the results", 0.92)

  bmap <- function(lst) if (is.null(lst)) list() else setNames(lst, vapply(lst, `[[`, "", "key"))
  bp <- bmap(boot$paths); bw <- bmap(boot$weights); bl <- bmap(boot$loadings); br <- bmap(boot$r2); bc <- bmap(boot$cond)
  strip <- function(x) if (is.null(x)) NULL else x[c("mean", "se", "t", "p", "lower", "upper")]
  # HTMT and f² from cSEM. Reliability and AVE from the loadings with the usual definitions (as for PLS-SEM), so they
  # exist for components too: AVE = mean λ², ρ_C = (Σλ)² / ((Σλ)² + Σ(1 − λ²)), Cronbach's α of the standardised indicators.
  qa <- tryCatch(suppressWarnings(cSEM::assess(fit, .quality_criterion = c("htmt", "f2"))), error = function(e) list())
  cons_l <- unname(nmap$cn[vapply(model$constructs, `[[`, "", "id")])
  g <- function(v, k) if (!is.null(v) && k %in% names(v) && is.finite(v[[k]])) unname(v[[k]]) else NULL
  reliability <- lapply(model$constructs, function(c) {
    lam <- vapply(unlist(c$indicators), function(v) v0$loadings[[paste(nmap$cn[[c$id]], nmap$vn[[v]], sep = "\r")]], 0)
    k <- length(lam)
    R <- stats::cor(D[, unname(nmap$vn[unlist(c$indicators)]), drop = FALSE])
    alpha <- if (k > 1) k / (k - 1) * (1 - k / sum(R)) else NULL
    list(construct = c$name, mode = c$mode, alpha = alpha, rhoC = sum(lam)^2 / (sum(lam)^2 + sum(1 - lam^2)), ave = mean(lam^2))
  })
  f2 <- qa$F2; vif <- fit$Estimates$VIF
  path_rows <- c(
    lapply(model$paths, function(p) {
      k <- paste(nmap$cn[[p$from]], nmap$cn[[p$to]], sep = "\r")
      c(list(from = nm(nmap$cn[[p$from]]), to = nm(nmap$cn[[p$to]]), coefficient = unname(v0$paths[[k]]), implicit = isTRUE(p$implicit),
             fSquare = if (!is.null(f2)) num_or_null(f2[nmap$cn[[p$to]], nmap$cn[[p$from]]]) else NULL,
             vif = num_or_null(tryCatch(vif[[nmap$cn[[p$to]]]][[nmap$cn[[p$from]]]], error = function(e) NA))),
        list(boot = strip(bp[[k]])))
    }),
    lapply(mods, function(m) {
      k <- paste(nmap$int[[m$id]], nmap$cn[[m$dv]], sep = "\r")
      list(from = nm(nmap$int[[m$id]]), to = nm(nmap$cn[[m$dv]]), coefficient = unname(v0$paths[[k]]), interaction = TRUE,
           fSquare = if (!is.null(f2)) num_or_null(tryCatch(f2[nmap$cn[[m$dv]], nmap$int[[m$id]]], error = function(e) NA)) else NULL,
           boot = strip(bp[[k]]))
    }))
  outer_rows <- unlist(lapply(model$constructs, function(c) lapply(unlist(c$indicators), function(v) {
    k <- paste(nmap$cn[[c$id]], nmap$vn[[v]], sep = "\r")
    list(construct = c$name, indicator = v, mode = c$mode, weight = unname(v0$weights[[k]]), loading = unname(v0$loadings[[k]]),
         weightBoot = strip(bw[[k]]), loadingBoot = strip(bl[[k]]))
  })), recursive = FALSE)
  r2_rows <- lapply(names(v0$r2), function(k) list(construct = nm(k), rSquared = unname(v0$r2[[k]]), boot = strip(br[[k]])))

  # Discriminant validity: HTMT, Fornell-Larcker, cross-loadings and the Congruence Index (on the model's constructs).
  scores <- fit$Estimates$Construct_scores[, cons_l, drop = FALSE]
  Z <- as.matrix(D[, unname(nmap$vn[inds]), drop = FALSE])
  XL <- stats::cor(Z, scores); dimnames(XL) <- list(nm(rownames(XL)), nm(colnames(XL)))
  cv <- fit$Estimates$Construct_VCV[cons_l, cons_l, drop = FALSE]
  fl <- cv; fl[upper.tri(fl)] <- NA
  ave <- vapply(reliability, function(r) r$ave %||% NA_real_, 0)
  diag(fl) <- sqrt(ave)
  dimnames(fl) <- list(nm(rownames(fl)), nm(colnames(fl)))
  # HTMT (Henseler, Ringle & Sarstedt, 2015) from the indicator correlations, for every pair of constructs with two or
  # more indicators (cSEM reports it only for common factors); absolute correlations, as cSEM's default.
  htmt <- tryCatch({
    Ra <- abs(stats::cor(Z))
    blocks <- lapply(model$constructs, function(c) unname(nmap$vn[unlist(c$indicators)]))
    mono <- vapply(blocks, function(b) if (length(b) < 2) NA_real_ else mean(Ra[b, b][upper.tri(Ra[b, b])]), 0)
    h <- matrix(NA_real_, length(blocks), length(blocks), dimnames = list(nm(cons_l), nm(cons_l)))
    for (i in seq_along(blocks)) for (j in seq_len(i - 1)) h[i, j] <- mean(Ra[blocks[[i]], blocks[[j]]]) / sqrt(mono[i] * mono[j])
    if (all(is.na(h))) NULL else h
  }, error = function(e) NULL)

  fitx <- gsca_fit_indices(fit, n, length(path_rows))
  # Moderation: interaction effect, simple slopes and conditional effects (standardised components).
  moderation <- lapply(mods, function(m) {
    key <- function(a, b) paste(a, b, sep = "\r")
    biv <- unname(v0$paths[[key(nmap$cn[[m$iv]], nmap$cn[[m$dv]])]])
    bmo <- unname(v0$paths[[key(nmap$cn[[m$moderator]], nmap$cn[[m$dv]])]] %||% 0)
    bint <- unname(v0$paths[[key(nmap$int[[m$id]], nmap$cn[[m$dv]])]])
    list(id = m$id, predictor = nm(nmap$cn[[m$iv]]), moderator = nm(nmap$cn[[m$moderator]]), outcome = nm(nmap$cn[[m$dv]]),
         interactionTerm = nm(nmap$int[[m$id]]), predictorEffect = biv, moderatorEffect = bmo, interaction = bint,
         interactionBoot = strip(bp[[key(nmap$int[[m$id]], nmap$cn[[m$dv]])]]),
         conditional = lapply(c(-1, 0, 1), function(lv) {
           k <- paste(m$id, lv, sep = "\r")
           list(level = c("-1 SD", "Mean", "+1 SD")[lv + 2], moderatorValue = lv, effect = biv + bint * lv, boot = strip(bc[[k]]))
         }),
         # Simple slopes: outcome = b_m * w + (b_x + b_xw * w) * x for x from -2 to 2 SD at w = -1, 0, +1 SD.
         slopes = lapply(c(-1, 0, 1), function(w) list(level = c("-1 SD", "Mean", "+1 SD")[w + 2],
                                                       x = as.list(c(-2, -1, 0, 1, 2)), y = as.list(bmo * w + (biv + bint * w) * c(-2, -1, 0, 1, 2)))))
  })
  if (length(mods)) warn <- c(warn, "Moderation uses the two-stage approach: the interaction term is the product of the standardised stage-1 component scores of predictor and moderator.")

  list(
    status = "completed",
    kind = "gsca",
    settings = gs,
    approach = if (any(vapply(model$constructs, function(c) identical(c$mode, "reflective"), TRUE)))
      "GSCA with GSCAm for common factors (cSEM, .disattenuate = TRUE)" else "GSCA (components; cSEM)",
    n = list(input = n_input, used = n, rowsRemoved = n_input - n, cellsRecoded = recoded),
    admissible = admissible,
    fit = fitx[c("FIT", "AFIT", "FITM", "FITS", "GFI", "SRMR")],
    outer = outer_rows,
    paths = path_rows,
    rSquared = r2_rows,
    reliability = reliability,
    htmt = if (is.null(htmt)) NULL else matrix_to_table(htmt, "construct", "Construct"),
    fornellLarcker = matrix_to_table(fl, "construct", "Construct"),
    crossLoadings = matrix_to_table(XL, "indicator", "Indicator"),
    congruence = matrix_to_table(congruence_index(XL), "construct", "Construct"),
    moderation = moderation,
    bootstrap = if (is.null(boot)) NULL else boot[c("requested", "successful", "seed", "ciLevel")],
    warnings = as.list(unique(warn)),
    rCode = gsca_code(model, nmap, gs, inds, missing_code),
    environment = list(rVersion = R.version.string, cSEM = as.character(utils::packageVersion("cSEM"))),
    completedAt = now_iso(),
    durationSeconds = as.numeric(difftime(Sys.time(), t0, units = "secs")),
    .derived = list(nInput = n_input, rows = rows, tables = list(scores = { s <- as.data.frame(scores); names(s) <- nm(colnames(scores)); s }))
  )
}

gsca_code <- function(model, nmap, gs, inds, missing_code) {
  q <- function(x) paste0("\"", gsub("\"", "\\\\\"", x), "\"")
  common <- any(vapply(model$constructs, function(c) identical(c$mode, "reflective"), TRUE))
  mods <- model$moderations %||% list()
  L <- c("# GSCA generated by PathLab SEM (cSEM)", "library(cSEM)",
         "# data <- read.csv(\"your_data.csv\")   # the same dataset as in the app",
         paste0("d <- data.frame(\n", paste(sprintf("  %s = data[[%s]]", unname(nmap$vn), vapply(inds, q, "")), collapse = ",\n"), "\n)"))
  if (!is.null(missing_code)) L <- c(L, paste0("d[d == ", missing_code, "] <- NA   # project missing-value code"))
  L <- c(L, "d <- d[complete.cases(d), ]   # listwise deletion")
  est <- function(obj, syn) paste0(obj, " <- csem(d, ", syn, ", .approach_weights = \"GSCA\", .disattenuate = ", if (common) "TRUE" else "FALSE", ")")
  if (!length(mods)) {
    L <- c(L, paste0("model <- '\n", gsca_syntax(model, nmap), "\n'"), est("fit", "model"))
  } else {
    L <- c(L, "# Stage 1: main-effects model; stage 2 adds the interaction terms (products of standardised stage-1 scores)",
           paste0("model1 <- '\n", gsca_syntax(model, nmap), "\n'"), est("fit1", "model1"),
           "S <- scale(fit1$Estimates$Construct_scores)")
    for (m in mods) L <- c(L, sprintf("d$%s_ind <- S[, \"%s\"] * S[, \"%s\"]", nmap$int[[m$id]], nmap$cn[[m$iv]], nmap$cn[[m$moderator]]))
    L <- c(L, paste0("model2 <- '\n", gsca_syntax(model, nmap, with_int = TRUE), "\n'"), est("fit", "model2"))
  }
  L <- c(L, "summarize(fit)", "assess(fit)   # AVE, reliability, HTMT, f2, VIF, GFI, SRMR", "verify(fit)")
  if (isTRUE(gs$bootstrap$enabled)) L <- c(L, if (length(mods))
    sprintf("# Bootstrap (%d samples, seed %d): PathLab repeats both stages on every resample (cSEM's resampling cannot).", gs$bootstrap$nboot, gs$bootstrap$seed)
    else c(sprintf("boot <- resamplecSEMResults(fit, .R = %d, .seed = %d)", gs$bootstrap$nboot, gs$bootstrap$seed),
           sprintf("infer(boot, .alpha = %s)   # PathLab: SE = SD of the draws, t-based p, percentile CI, construct-level sign correction", format(1 - gs$ciLevel))))
  paste(L, collapse = "\n")
}

# ---- Report and workbook ---------------------------------------------------------------------------

gsca_body_html <- function(res) {
  esc <- html_escape
  S <- character(0); a <- function(...) S <<- c(S, paste0(...))
  b <- res$bootstrap
  a("<h2>Generalized structured component analysis (GSCA)</h2>")
  a("<p>", esc(res$approach), "; n = ", res$n$used, if (res$n$rowsRemoved > 0) sprintf(" (%d of %d rows removed, listwise)", res$n$rowsRemoved, res$n$input) else "",
    ". cSEM ", esc(res$environment$cSEM), ".", if (!is.null(b)) sprintf(" Bootstrapping: %d of %d samples, seed %s, %s%% percentile confidence intervals.", b$successful, b$requested, b$seed, round(100 * b$ciLevel)) else " No bootstrapping.", "</p>")
  for (w in ul(res$warnings)) a("<div class=\"warn\">", esc(w), "</div>")
  f <- res$fit
  a(html_table(c("Fit index", "Value"), list(c("FIT", fmt(f$FIT)), c("AFIT", fmt(f$AFIT)), c("FIT<sub>M</sub> (measurement)", fmt(f$FITM)), c("FIT<sub>S</sub> (structural)", fmt(f$FITS)),
    c("GFI", flag(fmt(f$GFI), lt(f$GFI, 0.93))), c("SRMR", flag(fmt(f$SRMR), gt(f$SRMR, 0.08)))), "Model fit", num_cols = 2))
  a("<p class=\"note\">FIT is the share of the total variance of all indicators and components explained by the model (AFIT adjusts it for model complexity); GFI and SRMR compare the observed and model-implied correlations (Cho, Hwang, Sarstedt &amp; Ringle, 2020, suggest GFI &ge; 0.93 and SRMR &le; 0.08).</p>")
  a(html_table(c("Construct", "Indicator", "Weight", "Loading", if (!is.null(b)) c("Weight p", "Loading CI") else NULL), lapply(res$outer, function(o)
    c(esc(o$construct), esc(o$indicator), fmt(o$weight), fmt(o$loading), if (!is.null(b)) c(fmt_p(o$weightBoot$p), paste0("[", fmt(o$loadingBoot$lower), ", ", fmt(o$loadingBoot$upper), "]")) else NULL)),
    "Measurement model", num_cols = 3:(if (!is.null(b)) 6 else 4)))
  a(html_table(c("Construct", "Cronbach's alpha", "rho_C", "AVE"), lapply(res$reliability, function(r) c(esc(r$construct), fmt(r$alpha), fmt(r$rhoC), flag(fmt(r$ave), lt(r$ave, 0.5)))),
               "Reliability and convergent validity", num_cols = 2:4))
  if (!is.null(res$htmt)) a(matrix_html(res$htmt, "HTMT", bad = function(rn, ck, v, r) gt(v, 0.85)))
  a(matrix_html(res$congruence, "Congruence Index (Tucker's congruence coefficient)", bad = function(rn, ck, v, r) { x <- num_of(v); !is.na(x) && !identical(as.character(rn %||% ""), ck) && x >= 0.95 }))
  a("<p class=\"note\">Congruence Index: Tucker's congruence coefficient between the cross-loading patterns of two constructs; 0.95 or more suggests the constructs are not empirically distinct (Lorenzo-Seva &amp; ten Berge, 2006).</p>")
  a(html_table(c("Path", "Coefficient", if (!is.null(b)) c("SE", "t", "p", "CI") else NULL, "f&sup2;"), lapply(res$paths, function(p)
    c(paste0(esc(p$from), " &rarr; ", esc(p$to)), fmt(p$coefficient), if (!is.null(b)) c(fmt(p$boot$se), fmt(p$boot$t, 2), flag(fmt_p(p$boot$p), gt(p$boot$p, 0.05 - 1e-12)), paste0("[", fmt(p$boot$lower), ", ", fmt(p$boot$upper), "]")) else NULL, fmt(p$fSquare))),
    "Structural model", num_cols = 2:(if (!is.null(b)) 7 else 3)))
  if (length(res$rSquared)) a(html_table(c("Endogenous construct", "R&sup2;"), lapply(res$rSquared, function(x) c(esc(x$construct), fmt(x$rSquared))), "Explained variance", num_cols = 2))
  for (m in res$moderation) {
    a("<h3>Moderation: ", esc(m$moderator), " on ", esc(m$predictor), " &rarr; ", esc(m$outcome), "</h3>")
    a(html_table(c("Moderator level", "Conditional effect", if (!is.null(b)) c("SE", "p", "CI") else NULL), lapply(m$conditional, function(x)
      c(esc(x$level), fmt(x$effect), if (!is.null(b)) c(fmt(x$boot$se), fmt_p(x$boot$p), paste0("[", fmt(x$boot$lower), ", ", fmt(x$boot$upper), "]")) else NULL)),
      sprintf("Conditional effects (interaction %s = %s)", esc(m$interactionTerm), fmt(m$interaction)), num_cols = 2:(if (!is.null(b)) 5 else 2)))
  }
  a("<h3>R code (cSEM)</h3><pre><code>", esc(res$rCode %||% ""), "</code></pre>")
  paste(S, collapse = "\n")
}

gsca_sheets <- function(res, project_name) {
  bt <- function(x, k) num_of(x[[k]])
  out <- list(
    Info = data.frame(Item = c("Project", "Completed (UTC)", "PathLab SEM version", "cSEM", "Approach", "Rows used", "Rows in dataset"),
                      Value = c(project_name, res$completedAt, PATHLAB_VERSION, res$environment$cSEM, res$approach, res$n$used, res$n$input), stringsAsFactors = FALSE),
    Fit = data.frame(Index = c("FIT", "AFIT", "FIT_M", "FIT_S", "GFI", "SRMR"), Value = vapply(c("FIT", "AFIT", "FITM", "FITS", "GFI", "SRMR"), function(k) num_of(res$fit[[k]]), 0)),
    Outer = do.call(rbind, lapply(res$outer, function(o) data.frame(Construct = o$construct, Indicator = o$indicator, Weight = num_of(o$weight), Loading = num_of(o$loading),
                                                                    WeightSE = bt(o$weightBoot, "se"), WeightP = bt(o$weightBoot, "p"), LoadingSE = bt(o$loadingBoot, "se"), LoadingP = bt(o$loadingBoot, "p"), stringsAsFactors = FALSE))),
    Paths = do.call(rbind, lapply(res$paths, function(p) data.frame(From = p$from, To = p$to, Coefficient = num_of(p$coefficient), SE = bt(p$boot, "se"), t = bt(p$boot, "t"),
                                                                    p = bt(p$boot, "p"), CILower = bt(p$boot, "lower"), CIUpper = bt(p$boot, "upper"), f2 = num_of(p$fSquare), stringsAsFactors = FALSE))),
    Reliability = data.frame(Construct = vapply(res$reliability, `[[`, "", "construct"), Alpha = vapply(res$reliability, function(r) num_of(r$alpha), 0),
                             RhoC = vapply(res$reliability, function(r) num_of(r$rhoC), 0), AVE = vapply(res$reliability, function(r) num_of(r$ave), 0)),
    "Congruence Index" = table_to_df(res$congruence),
    "Cross-loadings" = table_to_df(res$crossLoadings),
    "Fornell-Larcker" = table_to_df(res$fornellLarcker)
  )
  if (!is.null(res$htmt)) out$HTMT <- table_to_df(res$htmt)
  if (length(res$rSquared)) out$RSquared <- data.frame(Construct = vapply(res$rSquared, `[[`, "", "construct"), R2 = vapply(res$rSquared, function(x) num_of(x$rSquared), 0))
  if (length(res$moderation)) out$Moderation <- do.call(rbind, lapply(res$moderation, function(m) do.call(rbind, lapply(m$conditional, function(x)
    data.frame(Predictor = m$predictor, Moderator = m$moderator, Outcome = m$outcome, Level = x$level, Effect = num_of(x$effect), SE = bt(x$boot, "se"), p = bt(x$boot, "p"),
               CILower = bt(x$boot, "lower"), CIUpper = bt(x$boot, "upper"), stringsAsFactors = FALSE)))))
  out
}
