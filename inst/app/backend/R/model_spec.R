# Structured model specification: normalisation, validation, translation to SEMinR,
# and generation of reproducible R code.
#
# JSON model format (see docs/API.md):
#   { "constructs": [ { "id": "c1", "name": "Image", "mode": "composite_a",
#                       "indicators": ["IMAG1", "IMAG2"] } ],
#     "paths": [ { "id": "p1", "from": "c1", "to": "c2" } ] }
#
# Measurement modes map onto SEMinR (verified against seminr 2.5.0 help pages):
#   reflective  -> reflective(name, items)            (common factor, estimated with PLSc)
#   composite_a -> composite(name, items, weights = mode_A)   (correlation weights)
#   composite_b -> composite(name, items, weights = mode_B)   (regression weights)

MODES <- c("reflective", "composite_a", "composite_b")
INNER_WEIGHTS <- c("path_weighting", "path_factorial")
MISSING_TREATMENTS <- c("none", "mean_replacement", "listwise")
PREDICT_TECHNIQUES <- c("predict_DA", "predict_EA")

# Cores for bootstrapping: all but one by default (as SmartPLS uses the machine's cores), at most PATHLAB_MAX_CORES when set.
max_cores <- function() {
  detected <- tryCatch(parallel::detectCores(), error = function(e) 1L)
  if (is.na(detected)) detected <- 1L
  env <- suppressWarnings(as.integer(Sys.getenv("PATHLAB_MAX_CORES", "")))
  max(1L, if (!is.na(env)) min(env, detected) else detected - 1L)
}

normalize_model <- function(raw) {
  if (is.null(raw) || !is.list(raw)) api_error(400, "invalid_model", "The model specification is missing or malformed.")
  cons <- lapply(raw$constructs %||% list(), function(c) {
    list(id = as.character(c$id %||% ""), name = trimws(as.character(c$name %||% "")),
         mode = as.character(c$mode %||% "composite_a"),
         indicators = as_chr_vec(c$indicators),
         dimensions = as_chr_vec(c$dimensions))   # higher-order construct: ids of its lower-order constructs
  })
  pths <- lapply(raw$paths %||% list(), function(p) {
    list(id = as.character(p$id %||% ""), from = as.character(p$from %||% ""), to = as.character(p$to %||% ""))
  })
  mods <- lapply(raw$moderations %||% list(), function(m) {
    list(id = as.character(m$id %||% ""), iv = as.character(m$iv %||% ""), moderator = as.character(m$moderator %||% ""),
         dv = as.character(m$dv %||% ""), method = as.character(m$method %||% "two_stage"))
  })
  with_moderator_main_effects(list(constructs = cons, paths = pths, moderations = mods))
}

# A moderating effect is estimated together with the moderator's direct effect on the outcome
# (as SmartPLS does). When the user has not drawn that path, add it here, flagged `implicit`, so
# validation, estimation, bootstrapping, the exported R code and the report all include it.
# Without it SEMinR 2.5.0 cannot estimate or summarise the model (two_stage() and summary() fail).
with_moderator_main_effects <- function(model) {
  pairs <- vapply(model$paths, function(p) paste(p$from, p$to), "")
  for (m in model$moderations %||% list()) {
    if (!nzchar(m$moderator) || !nzchar(m$dv) || m$moderator == m$dv) next
    key <- paste(m$moderator, m$dv)
    if (key %in% pairs) next
    model$paths[[length(model$paths) + 1]] <- list(id = paste0("auto_", m$id), from = m$moderator, to = m$dv, implicit = TRUE)
    pairs <- c(pairs, key)
  }
  model
}

INTERACTION_METHODS <- c("two_stage", "product_indicator", "orthogonal")

# SEMinR names an interaction construct "<iv>*<moderator>".
interaction_name <- function(model, m) {
  byid <- construct_by_id(model)
  paste0(byid[[m$iv]]$name, "*", byid[[m$moderator]]$name)
}

# JSON null, "" or {} (some clients encode NULL as an empty object) -> NULL.
nullish <- function(x) if (is.null(x) || (is.list(x) && length(x) == 0) || identical(x, "")) NULL else x

int_in <- function(x, default, lo, hi, label) {
  x <- nullish(x)
  if (is.null(x)) return(default)
  v <- suppressWarnings(as.numeric(x))
  if (length(v) != 1 || is.na(v) || v != round(v) || v < lo || v > hi) {
    api_error(400, "invalid_setting", sprintf("%s must be a whole number between %s and %s.", label, lo, hi))
  }
  as.integer(v)
}

normalize_settings <- function(raw) {
  raw <- raw %||% list()
  iw <- as.character(raw$innerWeights %||% "path_weighting")
  if (!iw %in% INNER_WEIGHTS) api_error(400, "invalid_setting", "Unsupported inner weighting scheme.")
  mraw <- raw$missing %||% list()
  treat <- as.character(mraw$treatment %||% "none")
  if (!treat %in% MISSING_TREATMENTS) api_error(400, "invalid_setting", "Unsupported missing-data treatment.")
  code <- nullish(mraw$code)
  if (!is.null(code)) {
    code <- suppressWarnings(as.numeric(code))
    if (length(code) != 1 || is.na(code)) api_error(400, "invalid_setting", "The missing-value code must be a single number.")
  }
  braw <- raw$bootstrap %||% list()
  b_enabled <- isTRUE(braw$enabled)
  seed <- nullish(braw$seed)
  seed <- if (is.null(seed) || identical(seed, "")) NULL else int_in(seed, NULL, 1, 2147483647 - 100000, "Bootstrap seed")
  conf <- suppressWarnings(as.numeric(braw$confidenceLevel %||% 0.95))
  if (is.na(conf) || conf <= 0.5 || conf >= 1) api_error(400, "invalid_setting", "Confidence level must be between 0.5 and 1 (for example 0.95).")
  praw <- raw$predict %||% list()
  p_enabled <- isTRUE(praw$enabled)
  tech <- as.character(praw$technique %||% "predict_DA")
  if (!tech %in% PREDICT_TECHNIQUES) api_error(400, "invalid_setting", "Unsupported PLSpredict technique.")
  pseed <- nullish(praw$seed)
  pseed <- if (is.null(pseed) || identical(pseed, "")) NULL else int_in(pseed, NULL, 1, 2147483647, "Prediction seed")
  adv <- normalize_advanced(raw)
  algo <- as.character(raw$algorithm %||% "pls")[1]
  if (!algo %in% c("pls", "plsc")) api_error(400, "invalid_setting", "Unsupported algorithm (use 'pls' or 'plsc').")
  list(
    # "plsc": consistent PLS-SEM (Dijkstra & Henseler, 2015) for every Mode A construct, as SmartPLS's algorithm choice.
    algorithm = algo,
    cmb = adv$cmb,
    ipma = adv$ipma,
    nca = adv$nca,
    mga = adv$mga,
    innerWeights = iw,
    maxIt = int_in(raw$maxIt, 300L, 1, 100000, "Maximum iterations"),
    stopCriterion = int_in(raw$stopCriterion, 7L, 1, 15, "Stop criterion"),
    missing = list(treatment = treat, code = code),
    bootstrap = list(
      enabled = b_enabled,
      nboot = int_in(braw$nboot, 1000L, 20, 50000, "Number of bootstrap samples"),
      seed = seed,
      # 0 (or missing) = automatic: every available core.
      cores = if (is.null(nullish(braw$cores)) || identical(suppressWarnings(as.integer(braw$cores)), 0L)) max_cores() else int_in(braw$cores, 1L, 1, max_cores(), "Number of cores"),
      confidenceLevel = conf,
      specificIndirect = !isFALSE(braw$specificIndirect)
    ),
    predict = list(
      enabled = p_enabled,
      technique = tech,
      noFolds = int_in(praw$noFolds, 10L, 2, 50, "Number of folds"),
      reps = int_in(praw$reps, 1L, 1, 50, "Number of repetitions"),
      seed = pseed
    )
  )
}

#' Consistent PLS-SEM as an algorithm choice (as in SmartPLS): every first-order Mode A construct is estimated as a
#' common factor with the PLSc correction for this run, whatever its drawn mode. Mode B constructs stay composites, and
#' higher-order constructs keep their mode. The drawn model is not changed.
apply_algorithm <- function(model, settings) {
  if (!identical(settings$algorithm, "plsc")) return(model)
  model$constructs <- lapply(model$constructs, function(c) {
    if (identical(c$mode, "composite_a") && !is_hoc(c)) c$mode <- "reflective"
    c
  })
  model
}

# ---- validation ------------------------------------------------------------

model_issue <- function(severity, code, message, target_type = NULL, target_id = NULL, hint = NULL) {
  out <- list(severity = severity, code = code, message = message)
  if (!is.null(target_type)) out$target <- list(type = target_type, id = target_id)
  if (!is.null(hint)) out$hint <- hint
  out
}

find_cycle <- function(nodes, edges_from, edges_to) {
  adj <- split(edges_to, factor(edges_from, levels = nodes))
  state <- setNames(rep(0L, length(nodes)), nodes)  # 0 unvisited, 1 in stack, 2 done
  stack <- character(0)
  found <- NULL
  visit <- function(n) {
    if (!is.null(found)) return()
    state[n] <<- 1L
    stack <<- c(stack, n)
    for (m in adj[[n]] %||% character(0)) {
      if (state[m] == 1L) { found <<- c(stack[match(m, stack):length(stack)], m); return() }
      if (state[m] == 0L) visit(m)
      if (!is.null(found)) return()
    }
    stack <<- stack[-length(stack)]
    state[n] <<- 2L
  }
  for (n in nodes) if (state[n] == 0L) visit(n)
  found
}

#' The parts of the model that are joined to each other (connected components, ignoring the direction of the
#' paths): constructs are linked by a structural path, by a moderation (predictor, moderator and outcome) or as a
#' higher-order construct and its dimensions. Largest part first; each part is a vector of construct ids.
model_parts <- function(model) {
  ids <- vapply(model$constructs, `[[`, "", "id")
  if (!length(ids)) return(list())
  parent <- stats::setNames(ids, ids)
  find <- function(x) { while (parent[[x]] != x) x <- parent[[x]]; x }
  join <- function(a, b) if (a %in% ids && b %in% ids) { ra <- find(a); rb <- find(b); if (ra != rb) parent[[ra]] <<- rb }
  for (p in model$paths) join(p$from, p$to)
  for (m in model$moderations %||% list()) { join(m$iv, m$dv); join(m$moderator, m$dv) }
  for (c in model$constructs) for (d in unlist(c$dimensions)) join(c$id, d)
  root <- vapply(ids, find, "")
  parts <- unname(split(ids, factor(root, levels = unique(root))))
  parts[order(-lengths(parts))]
}

# ds: list(data, profile, roles) where roles is an optional named character vector
validate_model <- function(model, settings, ds = NULL) {
  issues <- list()
  add <- function(...) issues[[length(issues) + 1]] <<- model_issue(...)
  cons <- model$constructs
  pths <- model$paths
  ids <- vapply(cons, `[[`, "", "id")
  nms <- vapply(cons, `[[`, "", "name")

  if (length(cons) < 2) add("error", "too_few_constructs", "A PLS path model needs at least two constructs.", hint = "Add constructs on the canvas.")
  if (length(pths) < 1 && length(cons) >= 2) add("error", "no_paths", "The structural model has no paths.", hint = "Draw at least one arrow between two constructs.")
  if (any(duplicated(ids)) || any(!nzchar(ids))) add("error", "bad_ids", "Construct identifiers are missing or duplicated. Reload the project.")

  col_names <- if (!is.null(ds)) names(ds$data) else character(0)
  prof <- if (!is.null(ds)) setNames(ds$profile, vapply(ds$profile, `[[`, "", "name")) else list()
  roles <- if (!is.null(ds)) ds$roles else NULL

  for (c in cons) {
    if (!nzchar(c$name)) { add("error", "empty_name", "A construct has no name.", "construct", c$id); next }
    if (sum(nms == c$name) > 1) add("error", "duplicate_construct", sprintf("Construct name '%s' is used more than once.", c$name), "construct", c$id, "Give every construct a unique name.")
    if (grepl("\\*|->", c$name)) add("error", "reserved_characters", sprintf("Construct '%s' contains '*' or '->', which SEMinR reserves for interaction terms and output labels.", c$name), "construct", c$id)
    if (nchar(c$name) > 60) add("error", "long_name", sprintf("Construct name '%s' is longer than 60 characters.", substr(c$name, 1, 20)), "construct", c$id)
    if (c$name %in% col_names) add("error", "name_clash", sprintf("Construct '%s' has the same name as a dataset column. SEMinR requires construct and indicator names to differ.", c$name), "construct", c$id, "Rename the construct.")
    if (!c$mode %in% MODES) add("error", "bad_mode", sprintf("Construct '%s' has an unsupported measurement type.", c$name), "construct", c$id)
    if (is_hoc(c)) {
      validate_hoc(c, model, add)
      next
    }
    if (length(c$indicators) == 0) {
      add("error", "no_indicators", sprintf("Construct '%s' has no indicators.", c$name), "construct", c$id, "Drag one or more variables from the Indicators list onto the construct. If other constructs measure it, make it a higher-order construct: right-click each dimension and choose Add as dimension, or add them in its properties.")
    }
    if (any(duplicated(c$indicators))) add("error", "duplicate_indicator", sprintf("Construct '%s' lists the same indicator twice.", c$name), "construct", c$id)
    if (c$mode == "reflective" && length(c$indicators) == 1) {
      add("warning", "single_item_reflective",
          sprintf("Construct '%s' is reflective with a single indicator. A common factor is not identified from one item; consider specifying it as a composite (Mode A), which SEMinR treats as a single-item construct.", c$name),
          "construct", c$id)
    }
    if (c$mode == "composite_b" && length(c$indicators) == 1) {
      add("info", "single_item_formative", sprintf("Construct '%s' is Mode B with one indicator; this is equivalent to a single-item construct.", c$name), "construct", c$id)
    }
    if (!is.null(ds)) for (it in c$indicators) {
      if (!it %in% col_names) { add("error", "unknown_indicator", sprintf("Indicator '%s' (construct '%s') is not a column in the active dataset.", it, c$name), "construct", c$id, "Remove it or upload the dataset that contains it."); next }
      p <- prof[[it]]
      if (isTRUE(p$isDuplicateName)) add("error", "duplicate_column", sprintf("Indicator '%s' matches more than one dataset column with the same name.", it), "construct", c$id)
      else if (!p$type %in% c("integer", "numeric")) add("error", "non_numeric_indicator", sprintf("Indicator '%s' is not numeric (type: %s).", it, p$type), "construct", c$id, "Only numeric columns can be indicators.")
      else if (isTRUE(p$isConstant)) add("error", "constant_indicator", sprintf("Indicator '%s' has zero variance and cannot be standardised.", it), "construct", c$id, "Remove this indicator.")
      if (grepl("->|\\*", it)) add("error", "reserved_indicator", sprintf("Indicator '%s' contains '->' or '*'.", it), "construct", c$id)
      if (!is.null(roles) && identical(unname(roles[it]), "excluded")) add("error", "excluded_indicator", sprintf("Indicator '%s' is marked as excluded from analysis in the Data tab.", it), "construct", c$id, "Mark it as an analysis variable or remove it from the construct.")
    }
  }
  all_items <- unlist(lapply(cons, `[[`, "indicators"))
  shared <- unique(all_items[duplicated(all_items)])
  if (length(shared)) add("error", "shared_indicator", sprintf("Indicator(s) assigned to more than one construct: %s. Each indicator may belong to one construct only.", paste(shared, collapse = ", ")))

  pairs <- character(0)
  for (p in pths) {
    if (!p$from %in% ids || !p$to %in% ids) { add("error", "dangling_path", "A path refers to a construct that no longer exists.", "path", p$id); next }
    if (p$from == p$to) add("error", "self_loop", "A construct cannot have a path to itself.", "path", p$id)
    key <- paste(p$from, p$to)
    if (key %in% pairs) add("error", "duplicate_path", sprintf("The path %s -> %s is drawn more than once.", nms[match(p$from, ids)], nms[match(p$to, ids)]), "path", p$id)
    pairs <- c(pairs, key)
  }
  valid_p <- Filter(function(p) p$from %in% ids && p$to %in% ids && p$from != p$to, pths)
  if (length(valid_p)) {
    cyc <- find_cycle(ids, vapply(valid_p, `[[`, "", "from"), vapply(valid_p, `[[`, "", "to"))
    if (!is.null(cyc)) add("error", "cycle",
                           sprintf("The structural model contains a feedback loop (%s). PLS-SEM in SEMinR estimates recursive models only.", paste(nms[match(cyc, ids)], collapse = " -> ")),
                           hint = "Remove or reverse one of the paths in the loop.")
    used <- unique(c(vapply(valid_p, `[[`, "", "from"), vapply(valid_p, `[[`, "", "to"),
                     unlist(lapply(model$moderations, function(m) c(m$iv, m$moderator, m$dv))),
                     unlist(lapply(cons, `[[`, "dimensions"))))   # dimensions are used through their higher-order construct
    for (c in cons) if (!c$id %in% used) add("error", "isolated_construct", sprintf("Construct '%s' is not connected to any path.", c$name), "construct", c$id, "Connect it with an arrow or delete it.")
    # One model, as SmartPLS requires: the paths (with moderations and higher-order links) must join every construct.
    parts <- model_parts(model)
    parts <- Filter(function(p) any(p %in% used), parts)   # isolated constructs are reported above
    if (length(parts) > 1) {
      lab <- vapply(parts, function(p) paste(nms[match(p, ids)], collapse = ", "), "")
      add("error", "disconnected_model",
          sprintf("The model is not connected: it falls into %d separate parts (%s). All constructs must belong to one model.", length(parts), paste(sprintf("[%s]", lab), collapse = " and ")),
          hint = "Draw a path that links the parts, or delete the part you do not need (or estimate it as a separate analysis).")
      for (p in parts[-1]) for (cid in p) add("error", "disconnected_construct", sprintf("Construct '%s' is in a part of the model that is not connected to the rest.", nms[match(cid, ids)]),
                                              "construct", cid, "Draw a path that links its part to the rest of the model.")
    }
  }

  # Moderation (interaction terms)
  mod_keys <- character(0)
  for (m in model$moderations %||% list()) {
    if (!all(c(m$iv, m$moderator, m$dv) %in% ids)) { add("error", "dangling_moderation", "A moderating effect refers to a construct that no longer exists.", "moderation", m$id); next }
    nm <- function(x) nms[match(x, ids)]
    if (!m$method %in% INTERACTION_METHODS) add("error", "bad_interaction_method", "Unsupported interaction-term method.", "moderation", m$id)
    if (m$moderator %in% c(m$iv, m$dv)) add("error", "moderator_self", "The moderator must differ from the predictor and the outcome.", "moderation", m$id)
    if (!paste(m$iv, m$dv) %in% pairs) add("error", "moderation_no_path", sprintf("The moderated path %s -> %s does not exist.", nm(m$iv), nm(m$dv)), "moderation", m$id)
    # The moderator's direct effect on the outcome is added by normalize_model() when it is not drawn.
    key <- paste(m$iv, m$moderator, m$dv)
    if (key %in% mod_keys) add("error", "duplicate_moderation", sprintf("%s moderates %s -> %s more than once.", nm(m$moderator), nm(m$iv), nm(m$dv)), "moderation", m$id)
    mod_keys <- c(mod_keys, key)
    iname <- paste0(nm(m$iv), "*", nm(m$moderator))
    if (iname %in% nms || iname %in% col_names) add("error", "interaction_name_clash", sprintf("The interaction name '%s' clashes with an existing construct or column.", iname), "moderation", m$id)
  }

  # Data-dependent checks
  if (!is.null(ds) && length(all_items) && all(all_items %in% col_names)) {
    imp <- missing_impact(ds$data, unique(all_items), settings$missing$code)
    if (imp$cellsMissing > 0 && settings$missing$treatment == "none") {
      add("error", "missing_requires_choice",
          sprintf("The selected indicators contain %d missing value(s) in %d row(s). Choose a missing-data treatment in Analysis settings before running.", imp$cellsMissing, imp$rowsWithMissing),
          "settings", "missing")
    }
    n_used <- if (settings$missing$treatment == "listwise") imp$rowsRemainingListwise else imp$nRows
    if (n_used < 3) add("error", "too_few_rows", sprintf("Only %d complete row(s) would remain for estimation.", n_used), "settings", "missing")
    # 10-times rule of thumb (Hair et al.): informational only.
    arrows_in <- vapply(cons, function(c) {
      n_struct <- sum(vapply(valid_p, function(p) p$to == c$id, logical(1))) +
        sum(vapply(model$moderations %||% list(), function(m) m$dv == c$id, logical(1)))
      n_form <- if (c$mode == "composite_b") length(c$indicators) else 0
      n_struct + n_form
    }, numeric(1))
    if (length(arrows_in) && max(arrows_in) * 10 > n_used) {
      add("warning", "ten_times_rule",
          sprintf("The largest number of arrows pointing at one construct is %d, and %d rows will be used. This is below the common '10-times' rule of thumb (a rough guideline, not a formal power analysis).", max(arrows_in), n_used))
    }
    if (isTRUE(settings$predict$enabled) && settings$predict$noFolds > n_used) {
      add("error", "too_many_folds", "PLSpredict needs fewer folds than rows.", "settings", "predict")
    }
  }
  if (isTRUE(settings$bootstrap$enabled) && settings$bootstrap$nboot < 1000) {
    add("info", "few_bootstrap_samples", sprintf("%d bootstrap samples are useful for a quick check; final reports commonly use 5,000 or more.", settings$bootstrap$nboot), "settings", "bootstrap")
  }
  if (has_hoc(model)) {
    if (isTRUE(settings$predict$enabled)) {
      # PLSpredict predicts the observed indicators of endogenous constructs; a higher-order construct's are dimension scores.
      byid_p <- construct_by_id(model)
      endo_ids <- unique(vapply(model$paths, `[[`, "", "to"))
      if (length(endo_ids) && all(vapply(endo_ids, function(i) is_hoc(byid_p[[i]]), TRUE)))
        add("error", "hoc_predict", "PLSpredict predicts the indicators of endogenous constructs, but every endogenous construct here is a higher-order construct, whose indicators are dimension scores rather than observed items.", "settings", "predict", "Turn PLSpredict off.")
      else add("info", "hoc_predict_folds", "PLSpredict with a higher-order construct repeats the two-stage approach inside every fold, so held-out cases never help build their own dimension scores. SEMinR has no method for these models; this is PathLab's extension of its procedure. The model estimates are not affected.", "settings", "predict")
    }
  }
  if (isTRUE(settings$predict$enabled)) {
    endo <- unique(vapply(valid_p, `[[`, "", "to"))
    if (!length(endo)) add("error", "predict_no_endogenous", "PLSpredict needs at least one endogenous construct.", "settings", "predict")
  }
  if (length(valid_p) && all(nzchar(ids))) validate_advanced(list(constructs = cons, paths = valid_p, moderations = model$moderations %||% list()), settings, ds, add)
  list(
    valid = !any(vapply(issues, function(i) i$severity == "error", logical(1))),
    issues = issues
  )
}

# ---- translation to SEMinR -------------------------------------------------

construct_by_id <- function(model) setNames(model$constructs, vapply(model$constructs, `[[`, "", "id"))

# ---- higher-order constructs (SEMinR two-stage approach) -----------------------------------
is_hoc <- function(c) length(c$dimensions %||% character(0)) > 0
has_hoc <- function(model) any(vapply(model$constructs, is_hoc, TRUE))

#' Rules for a higher-order construct: two or more dimensions, each an ordinary construct with indicators that
#' belongs to this higher-order construct only and has no structural paths or moderations of its own.
validate_hoc <- function(c, model, add) {
  byid <- construct_by_id(model)
  if (length(c$indicators)) add("error", "hoc_indicators", sprintf("Higher-order construct '%s' cannot also have indicators; its dimensions are its indicators.", c$name), "construct", c$id)
  if (length(c$dimensions) < 2) add("error", "hoc_dimensions", sprintf("Higher-order construct '%s' needs at least two dimensions (lower-order constructs).", c$name), "construct", c$id)
  if (c$mode == "reflective") add("error", "hoc_mode", sprintf("Higher-order construct '%s' must be a Mode A or Mode B composite (SEMinR's two-stage approach).", c$name), "construct", c$id,
                                  "Choose Composite - Mode A (reflective-type) or Mode B (formative-type) for the higher-order construct.")
  for (d in c$dimensions) {
    dc <- byid[[d]]
    if (is.null(dc)) { add("error", "hoc_missing_dimension", sprintf("A dimension of '%s' no longer exists.", c$name), "construct", c$id); next }
    if (is_hoc(dc)) add("error", "hoc_nested", sprintf("'%s' is itself a higher-order construct; only second-order models are supported.", dc$name), "construct", c$id)
    if (!length(dc$indicators)) add("error", "hoc_dimension_items", sprintf("Dimension '%s' of '%s' has no indicators.", dc$name, c$name), "construct", dc$id)
    parents <- Filter(function(o) d %in% (o$dimensions %||% character(0)), model$constructs)
    if (length(parents) > 1) add("error", "hoc_shared_dimension", sprintf("'%s' is a dimension of more than one higher-order construct.", dc$name), "construct", dc$id)
    if (any(vapply(model$paths, function(p) p$from == d || p$to == d, TRUE)))
      add("error", "hoc_dimension_path", sprintf("Dimension '%s' has a structural path. In the two-stage approach, paths go to and from the higher-order construct '%s', not its dimensions.", dc$name, c$name),
          "construct", dc$id, "Move the path to the higher-order construct.")
    if (any(vapply(model$moderations %||% list(), function(m) d %in% c(m$iv, m$moderator, m$dv), TRUE)))
      add("error", "hoc_dimension_moderation", sprintf("Dimension '%s' is part of a moderating effect; use the higher-order construct instead.", dc$name), "construct", dc$id)
  }
  if (any(vapply(model$moderations %||% list(), function(m) c$id %in% c(m$iv, m$moderator, m$dv), TRUE)))
    add("error", "hoc_moderation", sprintf("Moderating effects involving the higher-order construct '%s' are not supported yet.", c$name), "construct", c$id)
}

#' Blank the cells between a higher-order construct and its own dimensions (HTMT, Fornell-Larcker): they are not
#' a discriminant-validity comparison, since the dimensions form the higher-order construct.
blank_hoc_pairs <- function(mat, model) {
  if (is.null(mat) || !has_hoc(model)) return(mat)
  byid <- construct_by_id(model)
  m <- unclass(mat)
  for (c in Filter(is_hoc, model$constructs)) for (d in c$dimensions) {
    dn <- byid[[d]]$name
    if (c$name %in% rownames(m) && dn %in% colnames(m)) m[c$name, dn] <- NA
    if (dn %in% rownames(m) && c$name %in% colnames(m)) m[dn, c$name] <- NA
  }
  m
}

build_measurement_model <- function(model) {
  byid0 <- construct_by_id(model)
  ordered <- c(Filter(Negate(is_hoc), model$constructs), Filter(is_hoc, model$constructs))
  specs <- lapply(ordered, function(c) {
    if (is_hoc(c)) {
      dims <- vapply(c$dimensions, function(d) byid0[[d]]$name, "")
      return(seminr::higher_composite(c$name, dims, method = seminr::two_stage, weights = if (c$mode == "composite_b") seminr::mode_B else seminr::mode_A))
    }
    switch(c$mode,
           reflective = seminr::reflective(c$name, c$indicators),
           composite_a = seminr::composite(c$name, c$indicators, weights = seminr::mode_A),
           composite_b = seminr::composite(c$name, c$indicators, weights = seminr::mode_B))
  })
  byid <- construct_by_id(model)
  # interaction_term() returns a closure over lazily evaluated arguments, so each term needs its own
  # environment with forced values; in a for loop every term would end up using the last moderation.
  ints <- lapply(model$moderations %||% list(), function(m) {
    iv <- byid[[m$iv]]$name
    moderator <- byid[[m$moderator]]$name
    method <- get(m$method, envir = asNamespace("seminr"))
    force(iv); force(moderator); force(method)
    seminr::interaction_term(iv = iv, moderator = moderator, method = method)
  })
  do.call(seminr::constructs, c(specs, ints))
}

# Paths grouped by source construct, in the order sources first appear.
grouped_paths <- function(model) {
  byid <- construct_by_id(model)
  from <- vapply(model$paths, function(p) byid[[p$from]]$name, "")
  to <- vapply(model$paths, function(p) byid[[p$to]]$name, "")
  for (m in model$moderations %||% list()) { from <- c(from, interaction_name(model, m)); to <- c(to, byid[[m$dv]]$name) }
  srcs <- unique(from)
  lapply(srcs, function(s) list(from = s, to = to[from == s]))
}

build_structural_model <- function(model) {
  do.call(seminr::relationships, lapply(grouped_paths(model), function(g) seminr::paths(from = g$from, to = g$to)))
}

model_indicators <- function(model) unique(unlist(lapply(model$constructs, `[[`, "indicators")))

endogenous_names <- function(model) {
  byid <- construct_by_id(model)
  unique(vapply(model$paths, function(p) byid[[p$to]]$name, ""))
}

# Enumerate every directed chain from -> m1 -> ... -> to with at least one mediator.
indirect_chains <- function(model, limit = 200) {
  byid <- construct_by_id(model)
  from <- vapply(model$paths, function(p) byid[[p$from]]$name, "")
  to <- vapply(model$paths, function(p) byid[[p$to]]$name, "")
  out <- list()
  walk <- function(chain) {
    if (length(out) >= limit) return()
    last <- chain[length(chain)]
    for (nx in to[from == last]) {
      nc <- c(chain, nx)
      if (length(nc) >= 3) out[[length(out) + 1]] <<- nc
      walk(nc)
    }
  }
  for (s in unique(from)) walk(s)
  out
}

# ---- R code generation -----------------------------------------------------

generate_r_code <- function(model, settings, dataset_meta, env = NULL, generated_at = now_iso()) {
  env <- env %||% list(rVersion = R.version.string, seminrVersion = seminr_version())
  L <- character(0)
  a <- function(...) L <<- c(L, paste0(...))
  a("# ---------------------------------------------------------------------------")
  a("# PathLab SEM - reproducible SEMinR analysis script")
  a("# Generated: ", generated_at)
  a("# Generated with: ", env$rVersion, ", seminr ", env$seminrVersion)
  if (!is.null(dataset_meta)) {
    a("# Dataset: ", dataset_meta$filename, " (", dataset_meta$nRows, " rows x ", dataset_meta$nCols, " columns)")
    a("# Dataset SHA-256: ", dataset_meta$checksum)
  }
  a("#")
  a("# This script recreates the model and analysis options selected in PathLab SEM.")
  a("# Steps that are NOT reproduced by this script:")
  a("#   - the diagram layout (node positions) used in the PathLab canvas;")
  a("#   - PathLab's own data-validation messages (re-check your data separately);")
  a("#   - formatting/rounding of the HTML report (SEMinR returns full precision).")
  if (identical(settings$algorithm, "plsc")) a("# Algorithm: consistent PLS-SEM (PLSc); Mode A constructs are specified with reflective().")
  a("# ---------------------------------------------------------------------------")
  a("")
  a("library(seminr)")
  a("")
  a("# 1. Data ------------------------------------------------------------------")
  a("# Point this at your copy of the dataset. Verify the checksum with:")
  a("#   digest::digest(file = data_file, algo = \"sha256\")")
  fname <- if (!is.null(dataset_meta)) dataset_meta$filename else "data.csv"
  a("data_file <- ", r_str(fname))
  reader <- dataset_meta$reader %||% list(kind = "csv", delimiter = ",")
  plain <- c(
    "# Same conversion as PathLab: value labels ignored (numeric codes kept); categorical columns",
    "# with numeric levels become those numbers, others become text.",
    "plain <- function(d) {",
    "  d <- as.data.frame(d, check.names = FALSE, stringsAsFactors = FALSE)",
    "  d[] <- lapply(d, function(x) {",
    "    if (inherits(x, \"haven_labelled\")) x <- haven::zap_labels(x)",
    "    if (is.factor(x)) { lv <- levels(x); x <- if (all(grepl(\"^\\\\s*-?[0-9]+(\\\\.[0-9]+)?\\\\s*$\", lv))) as.numeric(lv)[x] else as.character(x) }",
    "    if (!inherits(x, c(\"Date\", \"POSIXt\"))) x <- as.vector(x)",
    "    x",
    "  })",
    "  d",
    "}")
  if (identical(reader$kind, "xlsx")) {
    a("data <- as.data.frame(readxl::read_excel(data_file, sheet = ", r_str(reader$sheet), ", .name_repair = \"minimal\"))")
  } else if (identical(reader$kind, "ods")) {
    a("data <- as.data.frame(readODS::read_ods(data_file, sheet = ", r_str(reader$sheet), ", na = c(\"\", \"NA\"), .name_repair = \"minimal\"))")
  } else if (identical(reader$kind, "arff")) {
    a("# WEKA ARFF: nominal attributes are kept as categories (text), as in PathLab.")
    a("data <- foreign::read.arff(data_file)")
    a("data[] <- lapply(data, function(x) if (is.factor(x)) as.character(x) else x)")
  } else if (reader$kind %in% c("spss", "stata", "sas", "jamovi", "rdata")) {
    for (l in plain) a(l)
    call <- switch(reader$kind,
      spss = paste0("haven::", reader$fn, "(data_file, user_na = FALSE)"),
      stata = "haven::read_dta(data_file)",
      sas = paste0("haven::", reader$fn, "(data_file)"),
      jamovi = "jmvReadWrite::read_omv(data_file)",
      rdata = if (identical(reader$fn, "readRDS")) "readRDS(data_file)" else paste0("local({ e <- new.env(); load(data_file, envir = e); e[[", r_str(reader$sheet), "]] })"))
    if (identical(reader$kind, "spss")) a("# SPSS user-defined missing values are read as missing (user_na = FALSE), as in PathLab.")
    a("data <- plain(", call, ")")
  } else {
    a("data <- read.csv(data_file, sep = ", r_str(reader$delimiter %||% ","), ", check.names = FALSE,")
    a("                 na.strings = c(\"\", \"NA\"), stringsAsFactors = FALSE,")
    a("                 strip.white = TRUE, fileEncoding = \"UTF-8-BOM\")")
  }
  items <- model_indicators(model)
  a("")
  a("# Keep only the indicators used in the model (other columns are not analysed).")
  a("indicators <- ", r_vec(items))
  mga_on <- isTRUE(settings$mga$enabled)
  if (mga_on) a("group <- as.character(data[[", r_str(settings$mga$groupVariable), "]])   # MGA grouping variable")
  mk_on <- nzchar(settings$cmb$markerVariable %||% "")
  if (mk_on) a("plain_marker <- data[, ", r_str(settings$cmb$markerVariable), ", drop = FALSE]   # CMB marker variable")
  a("data <- data[, indicators, drop = FALSE]")
  if (!is.null(settings$missing$code)) {
    a("")
    a("# Selected in PathLab: treat the value ", format(settings$missing$code), " as missing.")
    a("for (v in indicators) data[[v]][!is.na(data[[v]]) & data[[v]] == ", format(settings$missing$code, digits = 15), "] <- NA")
  }
  if (settings$missing$treatment == "listwise") {
    a("")
    a("# Selected in PathLab: listwise deletion (remove rows with any missing indicator).")
    if (mga_on || mk_on) {
      a("keep <- complete.cases(data)")
      a("data <- data[keep, , drop = FALSE]")
      if (mga_on) a("group <- group[keep]")
      if (mk_on) a("plain_marker <- plain_marker[keep, , drop = FALSE]")
    } else {
      a("data <- data[complete.cases(data), , drop = FALSE]")
    }
  }
  a("")
  a("# 2. Measurement model -----------------------------------------------------")
  a("measurement_model <- constructs(")
  byid_h <- construct_by_id(model)
  cl <- vapply(c(Filter(Negate(is_hoc), model$constructs), Filter(is_hoc, model$constructs)), function(c) {
    if (is_hoc(c)) return(paste0("  higher_composite(", r_str(c$name), ", ", r_vec(vapply(c$dimensions, function(d) byid_h[[d]]$name, "")),
                                 ", method = two_stage, weights = ", if (c$mode == "composite_b") "mode_B" else "mode_A", ")"))
    switch(c$mode,
           reflective = paste0("  reflective(", r_str(c$name), ", ", r_vec(c$indicators), ")"),
           composite_a = paste0("  composite(", r_str(c$name), ", ", r_vec(c$indicators), ", weights = mode_A)"),
           composite_b = paste0("  composite(", r_str(c$name), ", ", r_vec(c$indicators), ", weights = mode_B)"))
  }, "")
  byid_c <- construct_by_id(model)
  il <- vapply(model$moderations %||% list(), function(m) {
    paste0("  interaction_term(iv = ", r_str(byid_c[[m$iv]]$name), ", moderator = ", r_str(byid_c[[m$moderator]]$name), ", method = ", m$method, ")")
  }, "")
  a(paste(c(cl, il), collapse = ",\n"))
  a(")")
  a("")
  a("# 3. Structural model ------------------------------------------------------")
  auto <- Filter(function(p) isTRUE(p$implicit), model$paths)
  if (length(auto)) {
    byid_a <- construct_by_id(model)
    a("# Includes the moderator's direct effect, added automatically for each moderating effect:")
    for (p in auto) a("#   ", byid_a[[p$from]]$name, " -> ", byid_a[[p$to]]$name)
  }
  a("structural_model <- relationships(")
  pl <- vapply(grouped_paths(model), function(g) paste0("  paths(from = ", r_str(g$from), ", to = ", r_vec(g$to), ")"), "")
  a(paste(pl, collapse = ",\n"))
  a(")")
  a("")
  a("# 4. Estimation ------------------------------------------------------------")
  miss_fun <- if (settings$missing$treatment == "mean_replacement") "mean_replacement" else "na.omit"
  if (settings$missing$treatment == "mean_replacement") a("# Selected in PathLab: SEMinR mean replacement of missing values.")
  a("pls_model <- estimate_pls(")
  a("  data = data,")
  a("  measurement_model = measurement_model,")
  a("  structural_model = structural_model,")
  a("  inner_weights = ", settings$innerWeights, ",")
  a("  missing = ", miss_fun, ",")
  a("  missing_value = NA,")
  a("  maxIt = ", settings$maxIt, ",")
  a("  stopCriterion = ", settings$stopCriterion)
  a(")")
  a("pls_summary <- summary(pls_model)")
  a("")
  a("pls_summary$paths                      # path coefficients, R^2 and adjusted R^2")
  a("pls_summary$loadings                   # outer loadings")
  a("pls_summary$weights                    # outer weights")
  a("pls_summary$reliability                # alpha, rhoA, rhoC, AVE")
  a("pls_summary$validity$htmt              # HTMT")
  a("pls_summary$validity$fl_criteria       # Fornell-Larcker criterion")
  a("pls_summary$validity$cross_loadings    # cross-loadings")
  a("pls_summary$validity$vif_items         # indicator VIF")
  a("pls_summary$vif_antecedents            # structural (antecedent) VIF")
  a("pls_summary$fSquare                    # f^2 effect sizes")
  a("pls_summary$it_criteria                # AIC / BIC")
  a("pls_summary$total_effects")
  a("pls_summary$total_indirect_effects")
  if (isTRUE(settings$bootstrap$enabled)) {
    alpha <- 1 - settings$bootstrap$confidenceLevel
    a("")
    a("# 5. Bootstrapping ---------------------------------------------------------")
    a("# SEMinR seeds resample i with (seed + i), so results do not depend on 'cores'.")
    a("boot_model <- bootstrap_model(")
    a("  seminr_model = pls_model,")
    a("  nboot = ", settings$bootstrap$nboot, ",")
    a("  cores = ", settings$bootstrap$cores, ",")
    a("  seed = ", settings$bootstrap$seed %||% "NULL")
    a(")")
    a("boot_summary <- summary(boot_model, alpha = ", format(alpha, digits = 15), ")")
    a("boot_summary$nboot                              # successful bootstrap samples")
    a("boot_summary$bootstrapped_paths")
    a("boot_summary$bootstrapped_loadings")
    a("boot_summary$bootstrapped_weights")
    a("boot_summary$bootstrapped_HTMT")
    a("boot_summary$bootstrapped_total_paths")
    a("boot_summary$bootstrapped_total_indirect_paths")
    chains <- indirect_chains(model)
    if (isTRUE(settings$bootstrap$specificIndirect) && length(chains)) {
      a("")
      a("# Specific indirect effects")
      for (ch in chains) {
        a("specific_effect_significance(boot_model, from = ", r_str(ch[1]),
          ", through = ", r_vec(ch[-c(1, length(ch))]), ", to = ", r_str(ch[length(ch)]),
          ", alpha = ", format(alpha, digits = 15), ")")
      }
    }
  }
  if (isTRUE(settings$predict$enabled)) {
    a("")
    a("# 6. PLSpredict ------------------------------------------------------------")
    if (!is.null(settings$predict$seed)) a("set.seed(", settings$predict$seed, ")  # fold assignment is random")
    if (has_hoc(model)) {
      a("# SEMinR's predict_pls() has no method for higher-order constructs. PathLab repeats the two-stage approach inside")
      a("# every fold (the held-out cases' dimension scores come from the training cases' stage-1 weights):")
      for (f in c("stage1_scores", "stage2_plain", "predict_pls_two_stage")) a(f, " <- ", paste(deparse(get(f), width.cutoff = 120L), collapse = "\n"))
      a("predict_summary <- predict_pls_two_stage(pls_model, technique = ", r_str(settings$predict$technique), ", noFolds = ", settings$predict$noFolds, ", reps = ", settings$predict$reps, ")")
      a("predict_summary$PLS_out_of_sample   # RMSE and MAE of PLS-SEM predictions")
      a("predict_summary$LM_out_of_sample    # RMSE and MAE of the linear-model benchmark")
    } else {
      a("pls_predict <- predict_pls(")
      a("  model = pls_model,")
      a("  technique = ", settings$predict$technique, ",")
      a("  noFolds = ", settings$predict$noFolds, ",")
      a("  reps = ", settings$predict$reps)
      a(")")
      a("predict_summary <- summary(pls_predict)")
      a("predict_summary$PLS_out_of_sample   # RMSE and MAE of PLS-SEM predictions")
      a("predict_summary$LM_out_of_sample    # RMSE and MAE of the linear-model benchmark")
    }
  }
  advanced_code(model, settings, a)
  cmb_code(model, settings, a)
  paste(c(L, ""), collapse = "\n")
}
