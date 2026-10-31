# Machine learning (caret): several learners trained on the same resampling folds, in the style of WEKA's Explorer
# (test options, classifier output) and Orange's Test & Score, Confusion Matrix, ROC Analysis and Rank widgets.
# Models come from established packages through caret::train (stats lm/glm, glmnet, ranger, nnet, rpart, gbm, knn);
# metrics from caret (postResample, confusionMatrix), pROC (AUC) and yardstick (MCC); feature importance from
# caret::varImp; learner comparisons from caret::resamples / diff. The data are never changed: the project
# missing-value code is treated as missing for this analysis, rows with missing values are left out, and centring /
# scaling for distance- and gradient-based learners is done inside each training fold (no leakage).

# More WEKA learners, described as data (weka_catalog.json, the same file as the app's lib/wekaCatalog.json): rules
# (ZeroR, OneR, JRip, PART, DecisionTable), trees (DecisionStump, REPTree, RandomTree, RandomForest, M5P, LMT),
# functions (Logistic, SimpleLogistic), BayesNet and the meta learners AdaBoostM1 and Bagging. Their options are
# checked and turned into WEKA's option list generically (weka_cat_normalize, weka_cat_tokens).
WEKA_CAT <- local({
  # Next to this file: the folder source() or sys.source() is reading it from, otherwise backend_dir/R.
  f <- NULL
  for (i in rev(seq_len(sys.nframe()))) for (v in c("ofile", "file")) { of <- get0(v, envir = sys.frame(i), inherits = FALSE)
    if (is.null(f) && is.character(of) && length(of) == 1 && grepl("ml\\.R$", of)) f <- file.path(dirname(of), "weka_catalog.json") }
  if (is.null(f) || !file.exists(f)) f <- file.path(get("backend_dir"), "R", "weka_catalog.json")
  jsonlite::fromJSON(f, simplifyVector = FALSE)$learners
})

ML_LEARNERS <- c(lm = "Linear / logistic regression", glmnet = "Elastic net", ranger = "Random forest", nnet = "Neural network",
                 rpart = "Decision tree", gbm = "Gradient boosting", knn = "k-nearest neighbours",
                 ibk = "IBk", kstar = "KStar", lwl = "LWL", j48 = "J48", mlp = "MultilayerPerceptron", voted = "VotedPerceptron", smo = "SMO",
                 svm = "Support vector machine", xgb = "XGBoost", nb = "Naive Bayes", lda = "Linear discriminant analysis",
                 vapply(WEKA_CAT, function(x) x$name, ""))
# caret methods of the learners whose key differs from the method name (XGBoost has its own model list, below).
ML_CARET_METHOD <- c(svm = "svmRadial", nb = "naive_bayes", lda = "lda")
# Learners for class targets only, and for numeric targets only.
ML_CLASS_ONLY <- c("j48", "nb", "lda", "voted", "smo", names(Filter(function(x) x$target == "class", WEKA_CAT)))
ML_NUM_ONLY <- names(Filter(function(x) x$target == "numeric", WEKA_CAT))
# WEKA learners, run by WEKA itself through RWeka (needs 64-bit Java): the lazy classifiers (weka.classifiers.lazy),
# J48 (C4.5, weka.classifiers.trees.J48, classes only) and three functions (weka.classifiers.functions):
# MultilayerPerceptron (classes and numbers), VotedPerceptron (two classes) and SMO (support vector machine, classes);
# and the learners of the catalogue.
ML_WEKA <- c("ibk", "kstar", "lwl", "j48", "mlp", "voted", "smo", names(WEKA_CAT))
WEKA_LAZY_CLASS <- c(ibk = "weka/classifiers/lazy/IBk", kstar = "weka/classifiers/lazy/KStar", lwl = "weka/classifiers/lazy/LWL", j48 = "weka/classifiers/trees/J48",
                     mlp = "weka/classifiers/functions/MultilayerPerceptron", voted = "weka/classifiers/functions/VotedPerceptron", smo = "weka/classifiers/functions/SMO",
                     vapply(WEKA_CAT, function(x) gsub(".", "/", x$javaClass, fixed = TRUE), ""))
WEKA_JAVA_NAME <- c(ibk = "lazy.IBk", kstar = "lazy.KStar", lwl = "lazy.LWL", j48 = "trees.J48", mlp = "functions.MultilayerPerceptron", voted = "functions.VotedPerceptron", smo = "functions.SMO",
                    vapply(WEKA_CAT, function(x) sub("^weka\\.classifiers\\.", "", x$javaClass), ""))

#' A number as Java prints a double in WEKA's options (1.0, 0.25, 1.0E-8).
java_num <- function(v) {
  if (v == 0) return("0.0")
  if (abs(v) >= 1e-3 && abs(v) < 1e7) return(if (v == round(v)) sprintf("%.1f", v) else format(v, digits = 15))
  e <- floor(log10(abs(v))); m <- signif(v / 10^e, 15)
  sprintf("%sE%d", if (m == round(m)) sprintf("%.1f", m) else format(m, digits = 15), as.integer(e))
}

#' Whether the conditions of a catalogue entry hold ({key, is | not | in} on the option values, or {task}).
weka_cat_cond <- function(conds, v, task) all(vapply(conds %||% list(), function(c) {
  if (!is.null(c$task)) return(identical(c$task, task))
  x <- v[[c$key]]
  if (!is.null(c$is)) identical(x, c$is) else if (!is.null(c$not)) !identical(x, c$not) else if (!is.null(c$`in`)) x %in% unlist(c$`in`) else TRUE
}, TRUE))

#' The choices of an option that apply to the task.
weka_cat_choices <- function(o, task) Filter(function(ch) is.null(ch$task) || identical(ch$task, task), o$choices)

#' A learner's options from the analysis settings: each one of the right type and within its range or choices,
#' otherwise WEKA's default.
weka_cat_normalize <- function(L, raw) {
  raw <- raw %||% list()
  out <- list()
  for (o in WEKA_CAT[[L]]$options) {
    v <- raw[[o$key]]
    out[[o$key]] <- switch(o$type,
      bool = if (is.logical(v) && length(v) == 1 && !is.na(v)) v else o$default,
      choice = if (is.character(v) && length(v) == 1 && v %in% vapply(o$choices, function(ch) ch$value, "")) v else o$default,
      { x <- suppressWarnings(as.numeric(v))[1]
        if (length(x) && !is.na(x) && x >= o$min && x <= o$max) (if (o$type == "int") as.integer(round(x)) else x) else o$default })
  }
  out
}

#' WEKA's option list of a catalogue learner, in the order WEKA prints it: the tokens passed to the classifier's
#' setOptions. A choice that does not apply to the task (e.g. J48 as Bagging's base learner for a numeric target)
#' falls back to the default.
weka_cat_tokens <- function(L, v, task) {
  spec <- WEKA_CAT[[L]]
  opts <- stats::setNames(spec$options, vapply(spec$options, function(o) o$key, ""))
  v <- utils::modifyList(weka_cat_normalize(L, NULL), v %||% list())
  secs <- list(character(0)); s <- 1L
  add <- function(tok) { if (!length(tok)) return(); tok <- as.character(tok); parts <- split(tok, cumsum(tok == "--")); for (k in seq_along(parts)) {
    p <- parts[[k]]; if (k > 1) { s <<- s + 1L; if (length(secs) < s) secs[[s]] <<- character(0); p <- p[-1] }
    secs[[s]] <<- c(secs[[s]], p) } }
  for (it in spec$cmd) {
    if (isTRUE(it$sep)) { s <- s + 1L; if (length(secs) < s) secs[[s]] <- character(0); next }
    if (!weka_cat_cond(it[["if"]], v, task)) next
    if (!is.null(it$lit)) { add(unlist(it$lit)); next }
    if (!is.null(it$tpl)) {
      arg <- it$tpl[[2]]
      for (k in names(v)) arg <- gsub(paste0("{", k, "}"), as.character(v[[k]]), arg, fixed = TRUE)
      add(c(it$tpl[[1]], arg)); next
    }
    o <- opts[[it$opt]]; x <- v[[it$opt]]
    if (isTRUE(o$changedOnly) && identical(x, o$default)) next
    tok <- switch(o$type,
      bool = if (identical(x, o$when)) sub(" \\(off\\)$", "", o$flag) else character(0),
      choice = { ch <- weka_cat_choices(o, task); hit <- Filter(function(c) identical(c$value, x), ch)
                 if (!length(hit)) hit <- Filter(function(c) identical(c$value, o$default), ch)
                 if (length(hit)) unlist(hit[[1]]$args) else character(0) },
      int = c(o$token %||% o$flag, as.character(as.integer(x))),
      num = c(o$token %||% o$flag, java_num(x)))
    add(tok)
  }
  out <- secs[[1]]
  for (k in seq_along(secs)[-1]) out <- c(out, "--", secs[[k]])
  out
}
WEKA_KERNELS <- c(linear = 0L, epanechnikov = 1L, tricube = 2L, inverse = 3L, gaussian = 4L, constant = 5L)
WEKA_DISTANCE <- c(euclidean = "EuclideanDistance", manhattan = "ManhattanDistance", chebyshev = "ChebyshevDistance")
WEKA_KSTAR_MODE <- c(average = "a", delete = "d", maxdiff = "m", normal = "n")
WEKA_SMO_FILTER <- c(normalize = 0L, standardize = 1L, none = 2L)
# MultilayerPerceptron hidden layers: comma-separated sizes or WEKA's wildcards a, i, o, t (0 = no hidden layer).
WEKA_MLP_HIDDEN_RE <- "^([0-9]+|[aiot])(,([0-9]+|[aiot]))*$"
#' SMO's kernel as WEKA writes it (-K): polynomial (exponent, lower-order terms) or radial basis (gamma).
weka_smo_kernel <- function(s) if (identical(s$kernel, "rbf"))
  sprintf("weka.classifiers.functions.supportVector.RBFKernel -C 250007 -G %s", format(s$gamma)) else
  sprintf("weka.classifiers.functions.supportVector.PolyKernel -E %s -C 250007%s", format(s$exponent), if (isTRUE(s$lowerOrder)) " -L" else "")

# Hyperparameters of the learners outside WEKA that the user may set instead of tuning (as the app's catalogue,
# frontend/src/lib/learnerOptions.ts): type, range or choices. "extra" ones are passed to the fit, not tuned.
ML_TUNE <- list(
  glmnet = list(alpha = list("num", 0, 1), lambda = list("num", 0, 1e6)),
  ranger = list(mtry = list("int", 1, 1e5), splitrule = list("choice", c("gini", "extratrees", "hellinger", "variance", "maxstat")),
                min.node.size = list("int", 1, 1e5), num.trees = list("int", 1, 1e4, extra = TRUE)),
  nnet = list(size = list("int", 1, 100), decay = list("num", 0, 10)),
  rpart = list(cp = list("num", 0, 1)),
  gbm = list(n.trees = list("int", 1, 1e4), interaction.depth = list("int", 1, 20), shrinkage = list("num", 1e-4, 1), n.minobsinnode = list("int", 1, 1e4)),
  knn = list(k = list("int", 1, 1000)),
  svm = list(C = list("num", 1e-4, 1e4), sigma = list("num", 1e-6, 1e3)),
  xgb = list(nrounds = list("int", 1, 1e4), max_depth = list("int", 1, 20), eta = list("num", 1e-3, 1)),
  nb = list(laplace = list("num", 0, 100), usekernel = list("bool"), adjust = list("num", 0.01, 10))
)
#' Defaults of the settable hyperparameters (as the app's catalogue); p = number of features.
ml_tune_default <- function(L, key, task, p) switch(paste(L, key),
  "glmnet alpha" = 0.5, "glmnet lambda" = 0.01, "ranger mtry" = max(1L, floor(sqrt(max(1, p)))),
  "ranger splitrule" = if (task == "classification") "gini" else "variance", "ranger min.node.size" = if (task == "classification") 1L else 5L,
  "ranger num.trees" = 500L, "nnet size" = 5L, "nnet decay" = 0.1, "rpart cp" = 0.01, "gbm n.trees" = 150L, "gbm interaction.depth" = 3L,
  "gbm shrinkage" = 0.1, "gbm n.minobsinnode" = 10L, "knn k" = 5L, "svm C" = 1, "svm sigma" = 0.1, "xgb nrounds" = 100L,
  "xgb max_depth" = 3L, "xgb eta" = 0.1, "nb laplace" = 0, "nb usekernel" = FALSE, "nb adjust" = 1, NULL)

#' Per learner: mode "auto" (caret's grid) or "fixed" with the values that pass the checks (missing ones take the
#' defaults when the model is fitted). Unknown learners and parameters are dropped.
normalize_tune <- function(raw) {
  out <- list()
  for (L in intersect(names(raw %||% list()), names(ML_TUNE))) {
    t <- raw[[L]]; if (!is.list(t)) next
    vals <- list()
    for (k in intersect(names(t$values %||% list()), names(ML_TUNE[[L]]))) {
      d <- ML_TUNE[[L]][[k]]; v <- t$values[[k]]
      ok <- switch(d[[1]],
        bool = if (is.logical(v) && length(v) == 1 && !is.na(v)) v else NULL,
        choice = if (is.character(v) && length(v) == 1 && v %in% d[[2]]) v else NULL,
        { x <- suppressWarnings(as.numeric(v))[1]
          if (length(x) && !is.na(x) && x >= d[[2]] && x <= d[[3]]) (if (d[[1]] == "int") as.integer(round(x)) else x) else NULL })
      if (!is.null(ok)) vals[[k]] <- ok
    }
    out[[L]] <- list(mode = if (identical(t$mode, "fixed")) "fixed" else "auto", values = vals)
  }
  out
}

#' The values a learner is fitted with in "fixed" mode: the user's, completed with the defaults.
ml_tune_values <- function(L, o, task, p) {
  t <- o$tune[[L]]
  stats::setNames(lapply(names(ML_TUNE[[L]]), function(k) t$values[[k]] %||% ml_tune_default(L, k, task, p)), names(ML_TUNE[[L]]))
}
WEKA_LWL_BASE <- c(stump = "weka.classifiers.trees.DecisionStump", linear = "weka.classifiers.functions.LinearRegression", bayes = "weka.classifiers.bayes.NaiveBayes")

# XGBoost through its own interface (xgboost 3.x), as a caret model list so it is tuned and scored on the same folds:
# boosting rounds and tree depth are tuned (learning rate 0.1; 80% row and column subsampling, xgboost's usual setup).
XGB_SRC <- '
xgb_caret <- function() list(
  label = "XGBoost", library = "xgboost", type = c("Regression", "Classification"),
  parameters = data.frame(parameter = c("nrounds", "max_depth", "eta"), class = rep("numeric", 3), label = c("Boosting rounds", "Max. tree depth", "Learning rate")),
  grid = function(x, y, len = NULL, search = "grid") expand.grid(nrounds = c(100, 200, 400)[seq_len(min(len, 3))], max_depth = c(3, 6)[seq_len(min(len, 2))], eta = 0.1),
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    X <- as.matrix(x); storage.mode(X) <- "double"
    if (is.factor(y)) {
      k <- nlevels(y); lab <- as.integer(y) - 1L
      obj <- if (k == 2) list(objective = "binary:logistic") else list(objective = "multi:softprob", num_class = k)
    } else { lab <- y; obj <- list(objective = "reg:squarederror") }
    params <- c(obj, list(max_depth = param$max_depth, eta = param$eta, subsample = 0.8, colsample_bytree = 0.8, nthread = 1))
    b <- xgboost::xgb.train(params = params, data = xgboost::xgb.DMatrix(X, label = lab), nrounds = param$nrounds, verbose = 0)
    list(booster = b, lev = if (is.factor(y)) levels(y) else NULL, features = colnames(X), param = param)
  },
  predict = function(modelFit, newdata, submodels = NULL) {
    X <- as.matrix(newdata)[, modelFit$features, drop = FALSE]; storage.mode(X) <- "double"
    p <- stats::predict(modelFit$booster, X)
    if (is.null(modelFit$lev)) return(as.numeric(p))
    if (length(modelFit$lev) == 2) return(ifelse(p >= 0.5, modelFit$lev[2], modelFit$lev[1]))
    modelFit$lev[max.col(matrix(p, nrow = nrow(X)), ties.method = "first")]
  },
  prob = function(modelFit, newdata, submodels = NULL) {
    X <- as.matrix(newdata)[, modelFit$features, drop = FALSE]; storage.mode(X) <- "double"
    p <- stats::predict(modelFit$booster, X)
    out <- if (length(modelFit$lev) == 2) data.frame(1 - p, p) else as.data.frame(matrix(p, nrow = nrow(X)))
    names(out) <- modelFit$lev; out
  },
  varImp = function(object, ...) {
    imp <- xgboost::xgb.importance(model = object$booster)
    v <- stats::setNames(rep(0, length(object$features)), object$features); v[imp$Feature] <- imp$Gain
    data.frame(Overall = 100 * v / max(v, 1e-12), row.names = object$features)
  },
  sort = function(x) x[order(x$nrounds, x$max_depth), ])
'
eval(parse(text = XGB_SRC))

#' Whether WEKA can run (RWeka installed and a 64-bit Java started). Not cached when it fails, so installing Java and
#' restarting the server is enough.
.weka_ok <- new.env(parent = emptyenv())
weka_status <- function() {
  if (isTRUE(.weka_ok$ok)) return(list(available = TRUE, version = .weka_ok$version, message = "WEKA is ready."))
  if (!requireNamespace("RWeka", quietly = TRUE))
    return(list(available = FALSE, message = "The R package RWeka is not installed.", hint = "Install it with install.packages(\"RWeka\"); it needs a 64-bit Java."))
  res <- tryCatch({ RWeka::WOW("weka/classifiers/lazy/IBk"); TRUE }, error = function(e) e)
  if (!isTRUE(res)) return(list(available = FALSE, message = "WEKA could not start: a 64-bit Java is needed.",
                                hint = "Install a 64-bit Java (JDK 11 or newer), then restart the analysis server.", detail = sanitize_message(conditionMessage(res))))
  .weka_ok$ok <- TRUE
  .weka_ok$version <- tryCatch(as.character(utils::packageVersion("RWeka")), error = function(e) "")
  list(available = TRUE, version = .weka_ok$version, message = "WEKA is ready.")
}

#' WEKA options of a learner from the analysis settings, as a named list for RWeka::Weka_control (name -> -name
#' flag). Only options that apply are passed, in the combinations WEKA accepts: J48 refuses a confidence factor with
#' reduced-error pruning, and pruning options with an unpruned tree.
weka_control_args <- function(kind, w, task) {
  switch(kind,
    ibk = { b <- w$ibk; c(list(K = as.integer(b$k)), if (b$windowSize > 0) list(W = as.integer(b$windowSize)),
      if (isTRUE(b$crossValidate)) list(X = TRUE), if (isTRUE(b$crossValidate) && isTRUE(b$meanSquared) && task == "regression") list(E = TRUE),
      if (identical(b$weighting, "inverse")) list(I = TRUE), if (identical(b$weighting, "similarity")) list(F = TRUE),
      if (!identical(b$distance %||% "euclidean", "euclidean")) list(A = sprintf("weka.core.neighboursearch.LinearNNSearch -A \"weka.core.%s -R first-last\"", WEKA_DISTANCE[[b$distance]]))) },
    kstar = { k <- w$kstar; c(list(B = as.integer(k$blend)), if (isTRUE(k$entropic) && task == "classification") list(E = TRUE),
      if (!identical(k$missingMode %||% "average", "average")) list(M = WEKA_KSTAR_MODE[[k$missingMode]])) },
    lwl = list(K = as.integer(w$lwl$k), U = WEKA_KERNELS[[w$lwl$kernel]],
      W = WEKA_LWL_BASE[[if (task == "regression" && w$lwl$base == "bayes") "stump" else w$lwl$base]]),
    j48 = { j <- w$j48
      c(if (identical(j$collapseTree, FALSE)) list(O = TRUE),
        if (isTRUE(j$unpruned)) list(U = TRUE) else c(if (identical(j$subtreeRaising, FALSE)) list(S = TRUE),
          if (isTRUE(j$reducedErrorPruning)) list(R = TRUE, N = as.integer(j$numFolds %||% 3L), Q = as.integer(j$seed %||% 1L)) else list(C = j$confidence)),
        if (isTRUE(j$binarySplits)) list(B = TRUE), list(M = as.integer(j$minLeaf)),
        if (isTRUE(j$useLaplace)) list(A = TRUE), if (identical(j$useMDLcorrection, FALSE)) list(J = TRUE),
        if (isTRUE(j$doNotMakeSplitPointActualValue)) list(doNotMakeSplitPointActualValue = TRUE)) },
    mlp = { m <- w$mlp
      c(list(L = m$learningRate, M = m$momentum, N = as.integer(m$trainingTime), V = as.integer(m$validationSetSize), S = as.integer(m$seed),
             E = as.integer(m$validationThreshold), H = m$hiddenLayers),
        if (identical(m$nominalToBinaryFilter, FALSE)) list(B = TRUE), if (identical(m$normalizeNumericClass, FALSE) && task == "regression") list(C = TRUE),
        if (identical(m$normalizeAttributes, FALSE)) list(I = TRUE), if (identical(m$reset, FALSE)) list(R = TRUE), if (isTRUE(m$decay)) list(D = TRUE)) },
    voted = { v <- w$voted; list(I = as.integer(v$numIterations), E = v$exponent, S = as.integer(v$seed), M = as.integer(v$maxK)) },
    smo = { s <- w$smo
      c(list(C = s$c, L = s$toleranceParameter, P = s$epsilon, N = WEKA_SMO_FILTER[[s$filterType]]),
        if (isTRUE(s$buildCalibrationModels)) list(M = TRUE, V = as.integer(s$numFolds)), list(W = as.integer(s$randomSeed), K = weka_smo_kernel(s))) })
}
#' The options passed to WEKA: Weka_control for the hand-written learners, WEKA's option list (tokens) for those of
#' the catalogue (RWeka passes a character vector to setOptions as it is).
weka_control <- function(kind, w, task) if (kind %in% names(WEKA_CAT)) weka_cat_tokens(kind, w[[kind]], task) else do.call(RWeka::Weka_control, weka_control_args(kind, w, task))

#' The same options as R code (Weka_control(C = 0.25, M = 2L), or c("-M", "2") for a catalogue learner), for the
#' exported script.
weka_control_text <- function(kind, w, task) {
  if (kind %in% names(WEKA_CAT)) {
    tok <- weka_cat_tokens(kind, w[[kind]], task)
    return(if (length(tok)) sprintf("c(%s)", paste0("\"", gsub("\"", "\\\\\"", tok), "\"", collapse = ", ")) else "character(0)")
  }
  a <- weka_control_args(kind, w, task)
  v <- vapply(a, function(x) if (is.logical(x)) "TRUE" else if (is.character(x)) paste0("\"", gsub("\"", "\\\\\"", x), "\"") else if (is.integer(x)) paste0(x, "L") else format(x), "")
  sprintf("Weka_control(%s)", paste(names(a), v, sep = " = ", collapse = ", "))
}

#' caret model list for a WEKA learner, so it is tuned and scored on the same folds as the other learners. It is
#' given the attributes as WEKA reads them (nominal ones as categories, not dummy columns) and missing values are
#' passed on (na.pass): WEKA handles them itself, e.g. J48 splits such a case over the branches by weight. The
#' classes are given in WEKA's order (class_order: as the file declares them), not caret's positive-class-first
#' order: the perceptrons' starting weights and which class is "+1" depend on it.
#' labels: caret's class names -> the real labels. WEKA is given the real labels: it seeds some of its random choices
#' (e.g. JRip's pruning folds) from the text of a case, so a renamed label would change the model.
weka_lazy_caret <- function(kind, w, task, class_order = NULL, labels = NULL) {
  cls <- WEKA_LAZY_CLASS[[kind]]
  to_caret <- function(v) { v <- as.character(v); if (is.null(labels)) v else names(labels)[match(v, unname(labels))] }
  list(label = unname(ML_LEARNERS[[kind]]), library = "RWeka", type = c("Regression", "Classification"),
    parameters = data.frame(parameter = "parameter", class = "character", label = "parameter"),
    grid = function(x, y, len = NULL, search = "grid") data.frame(parameter = "none"),
    fit = function(x, y, wts, param, lev, last, classProbs, ...) {
      W <- RWeka::make_Weka_classifier(cls)
      if (is.factor(y) && !is.null(class_order) && setequal(class_order, levels(y))) y <- factor(as.character(y), levels = class_order)
      if (is.factor(y) && !is.null(labels) && all(levels(y) %in% names(labels))) y <- factor(unname(labels[as.character(y)]), levels = unname(labels[levels(y)]))
      d <- as.data.frame(x); d$.outcome <- y
      W(.outcome ~ ., data = d, na.action = stats::na.pass, control = weka_control(kind, w, task))
    },
    predict = function(modelFit, newdata, submodels = NULL) {
      p <- stats::predict(modelFit, newdata = as.data.frame(newdata))
      if (is.factor(p)) to_caret(p) else as.numeric(p)
    },
    prob = function(modelFit, newdata, submodels = NULL) {
      p <- as.data.frame(stats::predict(modelFit, newdata = as.data.frame(newdata), type = "probability"))
      names(p) <- to_caret(names(p)); p
    },
    sort = function(x) x)
}
ML_SCALE <- c("glmnet", "nnet", "knn", "svm")
# Feature scaling, as WEKA's Normalize (to 0-1) and Standardize (mean 0, SD 1) filters, but learned on each training
# fold and applied to its held-out fold (caret preProcess), so the test data never inform the scaling.
# "auto": standardize for the scale-sensitive learners only (elastic net, neural network, kNN).
ML_SCALING <- c(auto = "Automatic (standardize elastic net, neural network and kNN)", none = "None",
                range = "Normalize to 0-1 (min-max)", standardize = "Standardize (mean 0, SD 1)")
# Class balancing for an imbalanced class target, done on each training fold only (caret's sampling), so the held-out
# cases keep their real class mix: undersampling (as WEKA's SpreadSubsample), oversampling (as WEKA's Resample biased
# to a uniform class distribution) or SMOTE (Chawla et al., 2002; as WEKA's SMOTE filter).
ML_BALANCE <- c(none = "None", down = "Undersample the larger classes", up = "Oversample the smaller classes", smote = "SMOTE")

#' SMOTE: synthetic cases for every smaller class until it matches the largest. Each one lies at a random point
#' between a case of the class and one of its k = 5 nearest neighbours of the same class; the distance uses the
#' numeric features scaled by their range, plus 1 for each categorical feature whose labels differ. A categorical
#' feature takes the label most common among the case and its neighbours. Cases with missing numeric values are
#' kept as they are but not used as seeds.
ml_smote <- function(x, y, k = 5L) {
  x <- as.data.frame(x); y <- factor(y)
  num <- vapply(x, is.numeric, TRUE)
  tab <- table(y); big <- max(tab)
  rng <- vapply(x[num], function(v) { r <- diff(range(v, na.rm = TRUE)); if (is.finite(r) && r > 0) r else 1 }, 0)
  complete <- if (any(num)) stats::complete.cases(x[num]) else rep(TRUE, nrow(x))
  newx <- list(); newy <- character(0)
  for (cl in names(tab)[tab < big]) {
    idx <- which(y == cl & complete)
    need <- big - tab[[cl]]
    if (length(idx) < 2 || need <= 0) next
    m <- sweep(as.matrix(x[idx, num, drop = FALSE]), 2, rng, "/")
    # Categorical features as 0/1 columns scaled by 1/sqrt(2): two different labels are at distance 1.
    for (j in names(x)[!num]) { v <- addNA(factor(x[idx, j]), ifany = TRUE)
      m <- cbind(m, stats::model.matrix(~ v - 1, data.frame(v = v)) / sqrt(2)) }
    dm <- as.matrix(stats::dist(m)); diag(dm) <- Inf
    kk <- min(k, length(idx) - 1L)
    nn <- t(apply(dm, 1, function(r) order(r)[seq_len(kk)]))
    if (kk == 1) nn <- matrix(nn, ncol = 1)
    seeds <- sample(seq_along(idx), need, replace = TRUE)
    picks <- nn[cbind(seeds, sample.int(kk, need, replace = TRUE))]
    gap <- stats::runif(need)
    s <- x[idx[seeds], , drop = FALSE]
    for (j in names(x)[num]) s[[j]] <- x[idx[seeds], j] + gap * (x[idx[picks], j] - x[idx[seeds], j])
    for (j in names(x)[!num]) s[[j]] <- vapply(seq_len(need), function(q) {
      v <- as.character(x[idx[c(seeds[q], nn[seeds[q], ])], j]); v <- v[!is.na(v)]
      if (length(v)) names(which.max(table(v))) else NA_character_ }, "")
    for (j in names(x)[!num]) if (is.factor(x[[j]])) s[[j]] <- factor(s[[j]], levels = levels(x[[j]]))
    newx[[cl]] <- s; newy <- c(newy, rep(cl, need))
  }
  if (!length(newx)) return(list(x = x, y = y))
  list(x = rbind(x, do.call(rbind, unname(newx))), y = factor(c(as.character(y), newy), levels = levels(y)))
}
ml_sampling <- function(balance) switch(balance, down = "down", up = "up",
  smote = list(name = "SMOTE", func = function(x, y) ml_smote(x, y), first = TRUE), NULL)

ml_preprocess <- function(L, scaling) switch(scaling %||% "auto",
  none = NULL, range = "range", standardize = c("center", "scale"),
  if (L %in% ML_SCALE) c("center", "scale") else NULL)

# WEKA's LinearRegression (weka.classifiers.functions.LinearRegression): least squares with a small ridge on the
# centred attributes (the intercept is not penalised), optional elimination of collinear attributes (standardised
# coefficient above 1.5, removed one at a time) and attribute selection by WEKA's Akaike criterion:
#   akaike = SSE(model) / SSE(full) * (n - p_full) + 2 * p_model   (p counts the intercept)
# "M5": repeatedly drop the attribute with the smallest standardised coefficient while the criterion improves;
# "greedy": backward elimination, dropping the attribute whose removal improves the criterion most, until none does.
WEKA_LR_SRC <- '
weka_lr_fit <- function(X, y, selection = "M5", eliminate = TRUE, ridge = 1e-8) {
  X <- as.matrix(X); storage.mode(X) <- "double"; n <- nrow(X); p <- ncol(X)
  mx <- colMeans(X); sx <- apply(X, 2, stats::sd); my <- mean(y); sy <- stats::sd(y)
  fit_sel <- function(sel) {
    b <- numeric(p)
    if (any(sel)) {
      Z <- sweep(X[, sel, drop = FALSE], 2, mx[sel])
      A <- crossprod(Z); diag(A) <- diag(A) + ridge
      rhs <- crossprod(Z, y - my)
      b[sel] <- tryCatch(solve(A, rhs), error = function(e) MASS::ginv(A) %*% rhs)
    }
    list(b = b, a = my - sum(b * mx))
  }
  sse <- function(f) sum((y - f$a - drop(X %*% f$b))^2)
  sc_of <- function(f) abs(f$b * sx / sy)
  sel <- is.finite(sx) & sx > 0
  f <- fit_sel(sel)
  if (eliminate) repeat {
    sc <- sc_of(f); sc[!sel] <- 0
    j <- which.max(sc)
    if (length(j) && sc[j] > 1.5) { sel[j] <- FALSE; f <- fit_sel(sel) } else break
  }
  removed_collinear <- colnames(X)[!sel & is.finite(sx) & sx > 0]
  full <- sse(f); k_full <- sum(sel) + 1
  akaike <- (n - k_full) + 2 * k_full
  crit <- function(g, s2) if (full > 0) sse(g) / full * (n - k_full) + 2 * (sum(s2) + 1) else Inf
  if (selection == "M5") repeat {
    if (!any(sel)) break
    sc <- sc_of(f); sc[!sel] <- Inf
    j <- which.min(sc); trial <- sel; trial[j] <- FALSE
    g <- fit_sel(trial); a2 <- crit(g, trial)
    if (a2 < akaike) { sel <- trial; f <- g; akaike <- a2 } else break
  }
  if (selection == "greedy") repeat {
    best <- NULL
    for (j in which(sel)) {
      trial <- sel; trial[j] <- FALSE
      g <- fit_sel(trial); a2 <- crit(g, trial)
      if (a2 < akaike) { akaike <- a2; best <- list(sel = trial, f = g) }
    }
    if (is.null(best)) break
    sel <- best$sel; f <- best$f
  }
  structure(list(coef = stats::setNames(f$b, colnames(X)), intercept = f$a, selected = colnames(X)[sel], sx = sx, sy = sy,
                 removedCollinear = removed_collinear, selection = selection, ridge = ridge), class = "weka_lr")
}
weka_lr_predict <- function(object, newdata) {
  X <- as.matrix(newdata)[, names(object$coef), drop = FALSE]
  drop(object$intercept + X %*% object$coef)
}
# caret model list, so the fit is tuned and scored on the same folds as the other learners
weka_lr_caret <- function(selection = "M5", eliminate = TRUE, ridge = 1e-8) list(
  label = "Linear regression (WEKA)", library = NULL, type = "Regression",
  parameters = data.frame(parameter = "parameter", class = "character", label = "parameter"),
  grid = function(x, y, len = NULL, search = "grid") data.frame(parameter = "none"),
  fit = function(x, y, wts, param, lev, last, classProbs, ...) weka_lr_fit(x, y, selection, eliminate, ridge),
  predict = function(modelFit, newdata, submodels = NULL) weka_lr_predict(modelFit, newdata),
  prob = NULL, sort = function(x) x,
  varImp = function(object, ...) data.frame(Overall = unname(abs(object$coef * object$sx / object$sy)), row.names = names(object$coef)))
'
eval(parse(text = WEKA_LR_SRC))
ML_LM_SELECTION <- c(none = "No attribute selection", M5 = "M5 method", greedy = "Greedy method")
ml_weka_lm_on <- function(o) !is.null(o$lm) && (o$lm$selection != "none" || isTRUE(o$lm$eliminate) || o$lm$ridge > 0)

normalize_ml <- function(raw) {
  raw <- raw %||% list()
  pick <- function(v, allowed, default) { v <- as.character(v %||% default)[1]; if (v %in% allowed) v else default }
  s <- raw$sampling %||% list()
  learners <- intersect(as_chr_vec(raw$learners %||% list("lm", "glmnet", "ranger", "nnet")), names(ML_LEARNERS))
  if (!length(learners)) api_error(400, "invalid_ml", "Choose at least one learner.")
  share <- suppressWarnings(as.numeric(s$testShare %||% 0.2))
  if (is.na(share) || share < 0.1 || share > 0.5) api_error(400, "invalid_ml", "The test share must be between 0.1 and 0.5.")
  list(
    target = as.character(raw$target %||% "")[1],
    features = unique(as_chr_vec(raw$features)),
    task = pick(raw$task, c("auto", "regression", "classification"), "auto"),
    positive = if (is.null(raw$positive) || identical(raw$positive, "")) NULL else as.character(raw$positive)[1],
    learners = learners,
    sampling = list(method = pick(s$method, c("cv", "repeatedcv", "split", "train", "supplied"), "cv"),
                    folds = int_in(s$folds, 10L, 2L, 20L, "The number of folds"),
                    repeats = int_in(s$repeats, 3L, 1L, 10L, "The number of repeats"),
                    testShare = share),
    tuneLength = int_in(raw$tuneLength, 3L, 1L, 10L, "The tuning length"),
    scaling = pick(raw$scaling, names(ML_SCALING), "auto"),
    weka = local({
      w <- raw$weka %||% list(); ib <- w$ibk %||% list(); ks <- w$kstar %||% list(); lw <- w$lwl %||% list()
      num <- function(v, d, lo, hi) { v <- suppressWarnings(as.numeric(v %||% d)); if (is.na(v) || v < lo || v > hi) d else v }
      int <- function(v, d, lo, hi) as.integer(round(num(v, d, lo, hi)))
      flag <- function(v, d) if (is.null(v)) d else isTRUE(v)   # options whose WEKA default is TRUE
      list(ibk = list(k = int(ib$k, 1, 1, 1000), crossValidate = isTRUE(ib$crossValidate),
                      weighting = pick(ib$weighting, c("none", "inverse", "similarity"), "none"), distance = pick(ib$distance, names(WEKA_DISTANCE), "euclidean"),
                      meanSquared = isTRUE(ib$meanSquared), windowSize = int(ib$windowSize, 0, 0, 1e7)),
           kstar = list(blend = int(ks$blend, 20, 0, 100), entropic = isTRUE(ks$entropic), missingMode = pick(ks$missingMode, names(WEKA_KSTAR_MODE), "average")),
           lwl = list(k = int(lw$k, -1, -1, 1e6), kernel = pick(lw$kernel, names(WEKA_KERNELS), "linear"),
                      base = pick(lw$base, names(WEKA_LWL_BASE), "stump")),
           j48 = local({ j <- w$j48 %||% list()
             list(confidence = num(j$confidence, 0.25, 0.001, 0.5), minLeaf = int(j$minLeaf, 2, 1, 1e5),
                  unpruned = isTRUE(j$unpruned), binarySplits = isTRUE(j$binarySplits),
                  reducedErrorPruning = isTRUE(j$reducedErrorPruning), numFolds = int(j$numFolds, 3, 2, 100), seed = int(j$seed, 1, 0, 999999999),
                  subtreeRaising = flag(j$subtreeRaising, TRUE), collapseTree = flag(j$collapseTree, TRUE), useLaplace = isTRUE(j$useLaplace),
                  useMDLcorrection = flag(j$useMDLcorrection, TRUE), doNotMakeSplitPointActualValue = isTRUE(j$doNotMakeSplitPointActualValue)) }),
           mlp = local({ m <- w$mlp %||% list(); h <- gsub("\\s+", "", as.character(m$hiddenLayers %||% "a")[1])
             list(hiddenLayers = if (grepl(WEKA_MLP_HIDDEN_RE, h)) h else "a", learningRate = num(m$learningRate, 0.3, 0, 1), momentum = num(m$momentum, 0.2, 0, 1),
                  trainingTime = int(m$trainingTime, 500, 1, 1e6), validationSetSize = int(m$validationSetSize, 0, 0, 99),
                  validationThreshold = int(m$validationThreshold, 20, 1, 1e4), seed = int(m$seed, 0, 0, 999999999), decay = isTRUE(m$decay),
                  nominalToBinaryFilter = flag(m$nominalToBinaryFilter, TRUE), normalizeAttributes = flag(m$normalizeAttributes, TRUE),
                  normalizeNumericClass = flag(m$normalizeNumericClass, TRUE), reset = flag(m$reset, TRUE)) }),
           voted = local({ v <- w$voted %||% list()
             list(numIterations = int(v$numIterations, 1, 1, 1e5), exponent = num(v$exponent, 1, 0.01, 10), seed = int(v$seed, 1, 0, 999999999),
                  maxK = int(v$maxK, 10000, 1, 1e7)) }),
           smo = local({ s <- w$smo %||% list()
             list(c = num(s$c, 1, 1e-4, 1e4), filterType = pick(s$filterType, names(WEKA_SMO_FILTER), "normalize"), kernel = pick(s$kernel, c("poly", "rbf"), "poly"),
                  exponent = num(s$exponent, 1, 0.01, 10), lowerOrder = isTRUE(s$lowerOrder), gamma = num(s$gamma, 0.01, 1e-6, 1e3),
                  toleranceParameter = num(s$toleranceParameter, 0.001, 1e-8, 1), epsilon = num(s$epsilon, 1e-12, 1e-16, 1e-3),
                  buildCalibrationModels = isTRUE(s$buildCalibrationModels), numFolds = int(s$numFolds, -1, -1, 100), randomSeed = int(s$randomSeed, 1, 0, 999999999)) })) |>
        c(stats::setNames(lapply(names(WEKA_CAT), function(L) weka_cat_normalize(L, w[[L]])), names(WEKA_CAT)))
    }),
    balance = pick(raw$balance, names(ML_BALANCE), "none"),
    tune = normalize_tune(raw$tune),
    lm = local({ l <- raw$lm %||% list(); rg <- suppressWarnings(as.numeric(l$ridge %||% 0)); if (is.na(rg) || rg < 0 || rg > 1000) rg <- 0
      list(selection = pick(l$selection, names(ML_LM_SELECTION), "none"), eliminate = isTRUE(l$eliminate), ridge = rg) }),
    seed = int_in(raw$seed, 2026L, 1L, .Machine$integer.max, "The seed")
  )
}

ml_issue <- function(severity, code, message, hint = NULL) list(severity = severity, code = code, message = message, hint = hint)

#' A short list for messages: the first few items, then "and N more".
brief_list <- function(x, k = 6) if (length(x) <= k) paste(x, collapse = ", ") else sprintf("%s and %d more", paste(x[seq_len(k)], collapse = ", "), length(x) - k)

validate_ml <- function(df, o) {
  out <- list()
  add <- function(...) out[[length(out) + 1]] <<- ml_issue(...)
  if (!nzchar(o$target)) add("error", "ml_target", "Choose the target (outcome) variable.")
  if (!length(o$features)) add("error", "ml_features", "Add at least one feature.")
  if (any(o$learners %in% ML_WEKA)) {
    ws <- weka_status()
    if (!isTRUE(ws$available)) add("error", "ml_weka", sprintf("%s: %s", paste(unname(ML_LEARNERS[intersect(ML_WEKA, o$learners)]), collapse = ", "), ws$message), ws$hint)
  }
  co <- intersect(ML_CLASS_ONLY, o$learners)
  if (length(co) && nzchar(o$target) && o$target %in% names(df) && ml_task(df[[o$target]], o$task) == "regression")
    add("error", "ml_class_only", sprintf("%s %s for class targets only.", paste(unname(ML_LEARNERS[co]), collapse = ", "), if (length(co) == 1) "is" else "are"),
        "Untick them, or choose a categorical target (or set Task to Classification).")
  no <- intersect(ML_NUM_ONLY, o$learners)
  if (length(no) && nzchar(o$target) && o$target %in% names(df) && ml_task(df[[o$target]], o$task) == "classification")
    add("error", "ml_num_only", sprintf("%s %s for numeric targets only.", paste(unname(ML_LEARNERS[no]), collapse = ", "), if (length(no) == 1) "is" else "are"),
        "Untick them, or choose a numeric target (or set Task to Regression).")
  if ("voted" %in% o$learners && nzchar(o$target) && o$target %in% names(df) && ml_task(df[[o$target]], o$task) == "classification" &&
      length(unique(stats::na.omit(as.character(df[[o$target]])))) > 2)
    add("error", "ml_two_classes", "VotedPerceptron needs a target with two classes.", "Untick it, or choose a target with two classes.")
  if (nzchar(o$target) && o$target %in% o$features) add("error", "ml_target_feature", "The target cannot also be a feature.")
  miss <- setdiff(c(if (nzchar(o$target)) o$target, o$features), names(df))
  if (length(miss)) add("error", "ml_missing_column", sprintf("Not in the current dataset: %s.", brief_list(miss)),
                        "These variables come from another dataset. Remove them from the target and features, or load the dataset they belong to.")
  for (v in intersect(o$features, names(df))) {
    x <- df[[v]]
    if (!is.numeric(x) && length(unique(stats::na.omit(x))) > 20) add("error", "ml_feature_levels", sprintf("'%s' is text with more than 20 distinct values; it cannot be used as a categorical feature.", v))
    if (length(unique(stats::na.omit(x))) < 2) add("error", "ml_constant", sprintf("'%s' has no variance.", v))
  }
  if (nzchar(o$target) && o$target %in% names(df)) {
    task <- ml_task(df[[o$target]], o$task)
    y <- stats::na.omit(df[[o$target]])
    if (task == "regression" && !is.numeric(y)) add("error", "ml_task", "Regression needs a numeric target.")
    if (task == "classification") {
      tab <- table(y)
      if (length(tab) < 2) add("error", "ml_classes", "The target needs at least two classes.")
      if (length(tab) > 20) add("error", "ml_classes", sprintf("The target '%s' has %d different values, too many to treat as classes.", o$target, length(tab)),
                                if (is.numeric(y)) "It is numeric: set Task to Regression (or Automatic)." else "Choose another target, or group its values into fewer classes.")
      else if (any(tab < 5)) add("warning", "ml_small_class", sprintf("Some classes have fewer than 5 cases: %s.", brief_list(names(tab)[tab < 5])))
    }
  }
  out
}

#' Classification when asked, or (automatic) when the target is text or has at most 5 distinct values.
ml_task <- function(y, task) {
  if (task != "auto") return(task)
  if (!is.numeric(y) || length(unique(stats::na.omit(y))) <= 5) "classification" else "regression"
}

#' Readable feature names for caret's dummy columns (e.g. genderMale -> "gender = Male").
ml_display <- function(nm, map, factors) {
  nm <- gsub("`", "", nm, fixed = TRUE)
  vapply(nm, function(n) {
    if (n %in% names(map)) return(map[[n]])
    for (f in names(factors)) if (startsWith(n, f)) { lev <- substring(n, nchar(f) + 1); if (lev %in% factors[[f]]) return(paste0(map[[f]], " = ", lev)) }
    n
  }, "", USE.NAMES = FALSE)
}

#' Pooled metrics of a set of predictions (as Orange's Test & Score and WEKA compute them).
ml_metrics <- function(obs, pred, prob, task, lev, positive, prior = NULL) {
  if (task == "regression") {
    pr <- caret::postResample(pred, obs)
    # WEKA's relative errors: errors relative to always predicting the training mean (the prior), in %.
    rel <- if (!is.null(prior)) list(RAE = 100 * sum(abs(obs - pred)) / sum(abs(obs - prior)),
                                     RRSE = 100 * sqrt(sum((obs - pred)^2) / sum((obs - prior)^2))) else list()
    return(c(list(RMSE = unname(pr["RMSE"]), MSE = unname(pr["RMSE"])^2, MAE = unname(pr["MAE"]), R2 = unname(pr["Rsquared"]),
                  correlation = suppressWarnings(stats::cor(pred, obs))), rel))
  }
  cm <- caret::confusionMatrix(factor(pred, levels = lev), factor(obs, levels = lev), positive = if (length(lev) == 2) positive else NULL)
  by <- cm$byClass
  out <- list(accuracy = unname(cm$overall["Accuracy"]), kappa = unname(cm$overall["Kappa"]),
              mcc = tryCatch(yardstick::mcc_vec(factor(obs, levels = lev), factor(pred, levels = lev)), error = function(e) NA_real_))
  if (length(lev) == 2) {
    out$precision <- unname(by["Precision"]); out$recall <- unname(by["Recall"]); out$f1 <- unname(by["F1"])
    out$specificity <- unname(by["Specificity"])
    if (!is.null(prob)) out$auc <- tryCatch(as.numeric(pROC::auc(pROC::roc(obs, prob[[positive]], levels = c(setdiff(lev, positive), positive), direction = "<", quiet = TRUE))), error = function(e) NA_real_)
  } else {
    m <- function(col) mean(by[, col], na.rm = TRUE)
    out$precision <- m("Precision"); out$recall <- m("Recall"); out$f1 <- m("F1")
    if (!is.null(prob)) out$auc <- tryCatch(as.numeric(pROC::multiclass.roc(obs, as.data.frame(prob)[, lev], quiet = TRUE)$auc), error = function(e) NA_real_)
  }
  out
}

#' WEKA's PRC area of one class (weka.classifiers.evaluation.ThresholdCurve.getPRCArea): one curve point per distinct
#' predicted probability (predicted positive when the probability is at least that value), plus the point (0, 0);
#' precision of each point times the gain in recall over the next point, summed. NA when the area is 0.
weka_prc_area <- function(score, pos) {
  ok <- !is.na(score) & !is.na(pos); s <- score[ok]; y <- pos[ok]
  totP <- sum(y); if (!totP || !length(s)) return(NA_real_)
  u <- sort(unique(s)); g <- match(s, u)
  tp <- rev(cumsum(rev(tabulate(g[y], length(u))))); fp <- rev(cumsum(rev(tabulate(g[!y], length(u)))))
  P <- c(ifelse(tp + fp == 0, 0, tp / (tp + fp)), 0); R <- c(tp / totP, 0)
  area <- sum(P[-length(P)] * (R[-length(R)] - R[-1]))
  if (area == 0) NA_real_ else area
}

#' Area under the ROC curve of one class against the rest (Mann-Whitney, ties counted half: WEKA's ROC area).
weka_roc_area <- function(score, pos) {
  ok <- !is.na(score) & !is.na(pos); s <- score[ok]; y <- pos[ok]
  nP <- sum(y); nN <- sum(!y); if (!nP || !nN) return(NA_real_)
  (sum(rank(s)[y]) - nP * (nP + 1) / 2) / (nP * nN)
}

#' WEKA's "Detailed Accuracy By Class": for each class against the rest, TP rate, FP rate, precision, recall,
#' F-measure, MCC, ROC area and PRC area, and their average weighted by the number of cases of each class. Rates
#' that divide by zero are missing (WEKA prints "?"). obs / pred: factors (levels `lev`); prob: data frame with a
#' column per level; labels: level -> class label; order: the levels in the order to report them.
ml_by_class <- function(obs, pred, prob, lev, labels, order = lev) {
  dv <- function(a, b) if (b == 0) NA_real_ else a / b
  rows <- lapply(order, function(l) {
    a <- obs == l; p <- pred == l
    tp <- sum(a & p); fn <- sum(a & !p); fp <- sum(!a & p); tn <- sum(!a & !p)
    prec <- dv(tp, tp + fp); rec <- dv(tp, tp + fn)
    f <- if (is.na(prec) || is.na(rec)) NA_real_ else dv(2 * prec * rec, prec + rec)
    den <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
    sc <- if (!is.null(prob) && l %in% names(prob)) prob[[l]] else NULL
    list(class = unname(labels[[l]]), n = tp + fn, tpRate = rec, fpRate = dv(fp, fp + tn), precision = prec, recall = rec, fMeasure = f,
         mcc = if (den == 0) NA_real_ else (tp * tn - fp * fn) / den,
         rocArea = if (is.null(sc)) NA_real_ else weka_roc_area(sc, a), prcArea = if (is.null(sc)) NA_real_ else weka_prc_area(sc, a))
  })
  w <- vapply(rows, `[[`, 0, "n")
  keys <- c("tpRate", "fpRate", "precision", "recall", "fMeasure", "mcc", "rocArea", "prcArea")
  weighted <- stats::setNames(lapply(keys, function(k) { v <- vapply(rows, function(r) num_of(r[[k]]), 0)
    use <- w > 0; if (!any(use) || anyNA(v[use])) NA_real_ else sum(v[use] * w[use]) / sum(w[use]) }), keys)
  list(classes = rows, weighted = weighted)
}

#' Univariate feature scores (Orange's Rank widget): linear R² / ANOVA for a numeric target, AUC / chi-square / ANOVA for classes.
ml_rank <- function(d, y, task, feats, map, positive) {
  lapply(feats, function(f) {
    x <- d[[f]]
    if (task == "regression") {
      if (is.numeric(x)) { ct <- stats::cor.test(x, y); list(feature = map[[f]], test = "Pearson r", statistic = unname(ct$estimate), score = unname(ct$estimate)^2, scoreLabel = "R²", p = ct$p.value) }
      else { a <- stats::anova(stats::lm(y ~ x)); list(feature = map[[f]], test = "ANOVA F", statistic = a$`F value`[1], score = a$`Sum Sq`[1] / sum(a$`Sum Sq`), scoreLabel = "η²", p = a$`Pr(>F)`[1]) }
    } else {
      if (is.numeric(x)) {
        a <- stats::anova(stats::lm(x ~ y))
        auc <- if (nlevels(y) == 2) tryCatch(as.numeric(pROC::auc(pROC::roc(y, x, quiet = TRUE))), error = function(e) NA_real_) else NA_real_
        list(feature = map[[f]], test = "ANOVA F", statistic = a$`F value`[1], score = if (is.na(auc)) a$`Sum Sq`[1] / sum(a$`Sum Sq`) else max(auc, 1 - auc),
             scoreLabel = if (is.na(auc)) "η²" else "AUC", p = a$`Pr(>F)`[1])
      } else {
        ct <- suppressWarnings(stats::chisq.test(table(x, y)))
        list(feature = map[[f]], test = "χ²", statistic = unname(ct$statistic), score = sqrt(unname(ct$statistic) / (length(y) * (min(dim(table(x, y))) - 1))), scoreLabel = "Cramér's V", p = ct$p.value)
      }
    }
  })
}

#' WEKA's attribute evaluators (Select attributes, with the Ranker; "use full training set"): each feature's merit
#' by information gain, gain ratio, symmetrical uncertainty, ReliefF and correlation (the first three need a class
#' target; WEKA discretises numeric features for them), and the subset CFS chooses with best-first search. x: the
#' features as WEKA reads them (display names); y: the target. NULL when WEKA is not available or fails.
ML_ATTR_EVAL <- list(
  list(key = "infoGain", label = "Info gain", cls = "InfoGainAttributeEval", class = TRUE),
  list(key = "gainRatio", label = "Gain ratio", cls = "GainRatioAttributeEval", class = TRUE),
  list(key = "symUncert", label = "Symmetrical uncertainty", cls = "SymmetricalUncertAttributeEval", class = TRUE),
  list(key = "reliefF", label = "ReliefF", cls = "ReliefFAttributeEval", class = FALSE),
  list(key = "correlation", label = "Correlation", cls = "CorrelationAttributeEval", class = FALSE))
ml_attr_eval <- function(x, y, task) {
  if (!isTRUE(weka_status()$available)) return(NULL)
  tryCatch({
    feats <- names(x)
    dd <- x; names(dd) <- make.names(feats, unique = TRUE); dd$.class <- y
    n <- nrow(dd)
    evs <- Filter(function(e) task == "classification" || !e$class, ML_ATTR_EVAL)
    scores <- lapply(evs, function(e) {
      E <- RWeka::make_Weka_attribute_evaluator(paste0("weka/attributeSelection/", e$cls))
      # ReliefF compares every case with its neighbours: on large data a sample of 2000 reference cases.
      ctl <- if (e$cls == "ReliefFAttributeEval" && n > 2000) c("-M", "2000") else character(0)
      v <- E(.class ~ ., data = dd, na.action = stats::na.pass, control = ctl)
      unname(v[make.names(feats, unique = TRUE)])
    })
    inst <- RWeka:::read_data_into_Weka(dd, ncol(dd))
    as <- rJava::.jnew("weka/attributeSelection/AttributeSelection")
    rJava::.jcall(as, "V", "setEvaluator", rJava::.jcast(rJava::.jnew("weka/attributeSelection/CfsSubsetEval"), "weka/attributeSelection/ASEvaluation"))
    rJava::.jcall(as, "V", "setSearch", rJava::.jcast(rJava::.jnew("weka/attributeSelection/BestFirst"), "weka/attributeSelection/ASSearch"))
    rJava::.jcall(as, "V", "SelectAttributes", inst)
    sel <- rJava::.jcall(as, "[I", "selectedAttributes") + 1L
    cfs <- feats[sel[sel <= length(feats)]]
    list(evaluators = lapply(evs, function(e) list(key = e$key, label = e$label)),
         rows = lapply(seq_along(feats), function(j) list(feature = feats[j], cfs = feats[j] %in% cfs,
                                                          scores = stats::setNames(lapply(scores, function(s) num_of(s[j])), vapply(evs, `[[`, "", "key")))),
         sampled = n > 2000)
  }, error = function(e) NULL)
}

#' The fitted equation in the original units of the features, as WEKA prints it: caret's preProcess (centring and
#' scaling, or the 0-1 range) is undone, so each slope is b / s and the intercept b0 - sum(b * m / s).
ml_equation <- function(b0, b, pp, nm) {
  b <- b[is.finite(b)]
  m <- stats::setNames(rep(0, length(b)), names(b)); s <- stats::setNames(rep(1, length(b)), names(b))
  if (!is.null(pp$mean)) { k <- intersect(names(b), names(pp$mean)); m[k] <- pp$mean[k] }
  if (!is.null(pp$std)) { k <- intersect(names(b), names(pp$std)); s[k] <- pp$std[k] }
  if (!is.null(pp$ranges)) { k <- intersect(names(b), colnames(pp$ranges)); m[k] <- pp$ranges[1, k]; s[k] <- pp$ranges[2, k] - pp$ranges[1, k] }
  s[!is.finite(s) | s == 0] <- 1
  list(intercept = unname(b0 - sum(b * m / s)), scaled = !is.null(pp),
       terms = lapply(names(b), function(k) list(term = nm(k), estimate = unname(b[[k]] / s[[k]]))))
}

#' Coefficients of the linear learners for the formatted output: estimate, standard error, t (or z) and p for lm /
#' glm; for WEKA's LinearRegression the estimates and which attributes were selected; for the elastic net (numeric
#' target) the non-zero estimates. Regression learners also get the equation in the original units. NULL otherwise.
ml_coefficients <- function(fit, method, map, factors) tryCatch({
  fm <- fit$finalModel
  pp <- fit$preProcess
  nm <- function(x) ifelse(x == "(Intercept)", "(Intercept)", ml_display(x, map, factors))
  if (method %in% c("lm", "glm")) {
    cf <- summary(fm)$coefficients
    est <- stats::coef(fm)
    out <- list(stat = if (method == "glm") "z" else "t", rows = lapply(seq_len(nrow(cf)), function(j)
      list(term = nm(rownames(cf)[j]), estimate = unname(cf[j, 1]), se = unname(cf[j, 2]), stat = unname(cf[j, 3]), p = unname(cf[j, 4]))))
    if (method == "lm") out$equation <- ml_equation(est[["(Intercept)"]], est[names(est) != "(Intercept)"], pp, nm)
    out
  } else if (method == "weka_lr") {
    list(stat = NULL, intercept = fm$intercept, rows = lapply(names(fm$coef), function(k)
      list(term = nm(k), estimate = unname(fm$coef[[k]]), selected = k %in% fm$selected, collinear = k %in% fm$removedCollinear)),
      equation = ml_equation(fm$intercept, unlist(fm$coef)[fm$selected], pp, nm))
  } else if (method == "glmnet" && identical(fit$modelType, "Regression")) {
    cf <- stats::coef(fm, s = fit$bestTune$lambda)
    est <- stats::setNames(as.numeric(cf), rownames(cf))
    eq <- ml_equation(est[["(Intercept)"]], est[names(est) != "(Intercept)" & est != 0], pp, nm)
    keep <- vapply(eq$terms, function(t) t$term, "")
    list(stat = NULL, intercept = eq$intercept, equation = eq,
         rows = c(lapply(eq$terms, function(t) c(t, list(selected = TRUE))),
                  lapply(setdiff(nm(setdiff(names(est), "(Intercept)")), keep), function(t) list(term = t, estimate = 0, selected = FALSE))))
  } else NULL
}, error = function(e) NULL)

ml_model_text <- function(fit, learner, tgt_label = "y") {
  fm <- fit$finalModel
  if (learner %in% names(WEKA_CAT)) return(paste(tryCatch(utils::capture.output(print(fm)), error = function(e) character(0)), collapse = "\n"))
  txt <- tryCatch(switch(learner,
    weka_lr = {
      cf <- fm$coef[fm$selected]
      c("Linear Regression Model", "", paste0(tgt_label, " ="), "",
        if (length(cf)) sprintf("%12s * %s +", formatC(unname(cf), format = "f", digits = 4), names(cf)),
        sprintf("%12s", formatC(fm$intercept, format = "f", digits = 4)), "",
        sprintf("Attribute selection: %s; ridge: %s", unname(ML_LM_SELECTION[fm$selection]), format(fm$ridge)),
        if (length(fm$removedCollinear)) sprintf("Removed as collinear: %s", paste(fm$removedCollinear, collapse = ", ")),
        if (length(setdiff(names(fm$coef), fm$selected))) sprintf("Not selected: %s", paste(setdiff(names(fm$coef), fm$selected), collapse = ", ")))
    },
    lm = utils::capture.output(print(summary(fm)$coefficients)),
    glmnet = { cf <- stats::coef(fm, fit$bestTune$lambda); utils::capture.output(if (is.list(cf)) print(lapply(cf, as.matrix)) else print(as.matrix(cf))) },
    rpart = utils::capture.output(print(fm)),
    ranger = c(sprintf("Trees: %d; mtry: %s; min. node size: %s; split rule: %s", fm$num.trees, fm$mtry, fm$min.node.size, fm$splitrule),
               sprintf("Out-of-bag prediction error: %.6f", fm$prediction.error)),
    nnet = c(sprintf("Network: %s", paste(fm$n, collapse = "-")), sprintf("Weight decay: %s", fit$bestTune$decay), sprintf("Weights: %d", length(fm$wts))),
    gbm = c(sprintf("Trees: %d; interaction depth: %d; shrinkage: %s; min. observations in node: %d", fit$bestTune$n.trees, fit$bestTune$interaction.depth, fit$bestTune$shrinkage, fit$bestTune$n.minobsinnode)),
    knn = sprintf("k = %d (features centred and scaled)", fit$bestTune$k),
    ibk = , kstar = , lwl = , j48 = , mlp = , voted = , smo = utils::capture.output(print(fm)),
    svm = c(sprintf("Radial basis kernel: sigma = %s, cost C = %s", format(fit$bestTune$sigma, digits = 4), format(fit$bestTune$C)),
            sprintf("Support vectors: %d (features centred and scaled)", kernlab::nSV(fm))),
    xgb = c(sprintf("Boosting rounds: %d; max. tree depth: %d; learning rate: %s; row and column subsampling: 0.8", fit$bestTune$nrounds, fit$bestTune$max_depth, fit$bestTune$eta)),
    nb = utils::head(utils::capture.output(print(fm)), 60),
    lda = utils::capture.output(print(fm)),
    character(0)), error = function(e) character(0))
  paste(txt, collapse = "\n")
}

#' Labels of the nominal features for the WEKA learners: those the file declares (ARFF header order, also labels no
#' case has, as WEKA keeps them), then any other value; without a declaration, sorted.
ml_weka_levels <- function(d, feats, declared = NULL) {
  out <- list()
  for (f in feats) if (!is.numeric(d[[f]])) {
    v <- as.character(d[[f]]); seen <- sort(unique(v[!is.na(v)])); lv <- declared[[f]]
    out[[f]] <- if (is.null(lv)) seen else c(lv, setdiff(seen, lv))
  }
  out
}

#' WEKA prints class labels; caret needs syntactic ones (recurrence.events). The model text gets the labels back.
ml_relabel <- function(txt, labels) {
  if (is.null(labels) || !length(txt)) return(txt)
  for (nm in names(labels)[order(-nchar(names(labels)))]) if (nm != labels[[nm]])
    txt <- gsub(paste0(": ", nm, " ("), paste0(": ", labels[[nm]], " ("), txt, fixed = TRUE)
  txt
}

#' The supplied test file made ready for the trained models, as WEKA's "Supplied test set": the same variables and
#' types as the training data; the project missing-value code treated as missing; rows without the target left out,
#' and (unless every learner is a WEKA one) rows with missing features too. A label the training data never had
#' cannot be used: such rows are left out (for the WEKA learners the value becomes missing). Classes the models do
#' not know are left out. Returns the frame with the training names (d), the WEKA learners' frame (dw), the number
#' of rows read and notes.
ml_prepare_test <- function(td, df, o, missing_code, handles_na, weka_any, weka_levels, safe, factors, labels, task) {
  vars <- c(o$target, o$features)
  miss <- setdiff(vars, names(td))
  if (length(miss)) stop(analysis_failure("data", sprintf("The test file has no column %s.", brief_list(sprintf("'%s'", miss))),
                                          "Use a test file with the same variables (column names) as the training data."))
  input <- nrow(td)
  td <- td[, vars, drop = FALSE]
  for (v in vars) if (is.numeric(df[[v]]) != is.numeric(td[[v]]) && !(v == o$target && task == "classification"))
    stop(analysis_failure("data", sprintf("'%s' is %s in the training data but %s in the test file.", v, if (is.numeric(df[[v]])) "numeric" else "text", if (is.numeric(td[[v]])) "numeric" else "text"),
                          "Use a test file whose columns have the same types as the training data."))
  if (!is.null(missing_code)) for (cn in vars) if (is.numeric(td[[cn]])) td[[cn]][!is.na(td[[cn]]) & td[[cn]] == missing_code] <- NA
  notes <- character(0)
  keep <- if (handles_na) !is.na(td[[o$target]]) else stats::complete.cases(td)
  td <- td[keep, , drop = FALSE]
  if (task == "classification") {
    known <- make.names(as.character(td[[o$target]])) %in% names(labels)
    if (any(!known)) notes <- c(notes, sprintf("%d test row(s) with a class the training data do not have (%s) were left out.", sum(!known),
                                               brief_list(unique(as.character(td[[o$target]][!known])))))
    td <- td[known, , drop = FALSE]
  }
  # Labels of the categorical features: those of the training data only.
  unseen <- rep(FALSE, nrow(td))
  for (f in o$features) if (!is.numeric(df[[f]])) {
    sf <- safe[[f]]; v <- as.character(td[[f]])
    unseen <- unseen | (!is.na(v) & !v %in% factors[[sf]])
  }
  if (any(unseen) && !handles_na) {
    notes <- c(notes, sprintf("%d test row(s) with a label the training data do not have were left out.", sum(unseen)))
    td <- td[!unseen, , drop = FALSE]
  } else if (any(unseen)) notes <- c(notes, sprintf("%d test row(s) have a label the training data do not have; WEKA treats it as missing.", sum(unseen)))
  if (!nrow(td)) stop(analysis_failure("data", "No usable rows remain in the test file.", "Check that it has the target and features with values like those of the training data."))
  dw <- if (weka_any) {
    x <- td[, o$features, drop = FALSE]
    for (f in names(weka_levels)) x[[f]] <- factor(as.character(x[[f]]), levels = weka_levels[[f]])
    x
  } else NULL
  d <- td; names(d) <- unname(safe[vars])
  for (f in names(factors)) d[[f]] <- factor(as.character(d[[f]]), levels = factors[[f]])
  tgt <- safe[[o$target]]
  if (task == "classification") d[[tgt]] <- factor(make.names(as.character(d[[tgt]])), levels = names(labels))
  list(d = d, dw = dw, input = input, notes = notes)
}

run_ml <- function(df, o, missing_code = NULL, progress = function(...) NULL, declared = NULL, test = NULL) {
  issues <- validate_ml(df, o)
  if (any(vapply(issues, function(i) i$severity == "error", TRUE)))
    stop(analysis_failure("validation", issues[[which(vapply(issues, function(i) i$severity == "error", TRUE))[1]]]$message, "Fix the listed problems and run again."))
  suppressPackageStartupMessages({ library(caret); if ("gbm" %in% o$learners) library(gbm) })
  t0 <- Sys.time()
  warn <- vapply(Filter(function(i) i$severity == "warning", issues), `[[`, "", "message")
  vars <- c(o$target, o$features)
  d <- df[, vars, drop = FALSE]
  recoded <- 0L
  if (!is.null(missing_code)) for (cn in vars) if (is.numeric(d[[cn]])) { hit <- !is.na(d[[cn]]) & d[[cn]] == missing_code; recoded <- recoded + sum(hit); d[[cn]][hit] <- NA }
  # WEKA's learners handle missing feature values themselves, as in WEKA; the others need complete rows. All learners
  # are scored on the same cases, so rows with missing features are kept only when every learner is a WEKA one.
  weka_any <- any(o$learners %in% ML_WEKA)
  handles_na <- all(o$learners %in% ML_WEKA)
  keep <- if (handles_na) !is.na(d[[o$target]]) else stats::complete.cases(d)
  if (weka_any && !handles_na) {
    lost <- sum(!stats::complete.cases(d) & !is.na(d[[o$target]]))
    if (lost) warn <- c(warn, sprintf("%d row(s) with missing feature values were left out so that every learner is scored on the same cases. %s handle%s missing values as WEKA does when run without the other learners.",
                                      lost, paste(unname(ML_LEARNERS[intersect(ML_WEKA, o$learners)]), collapse = ", "), if (sum(o$learners %in% ML_WEKA) == 1) "s" else ""))
  }
  rows <- which(keep)
  d <- d[keep, , drop = FALSE]
  n <- nrow(d)
  if (n < 30) stop(analysis_failure("data", sprintf("Only %d complete rows remain.", n), "Machine learning needs more cases; use fewer features with missing values."))
  task <- ml_task(df[[o$target]], o$task)
  # The WEKA learners get the features as WEKA reads them: original names, nominal ones as categories with the
  # declared labels (not caret's dummy columns), missing values kept.
  weka_levels <- if (weka_any) ml_weka_levels(d, o$features, declared) else list()
  dw <- if (weka_any) {
    x <- d[, o$features, drop = FALSE]
    for (f in names(weka_levels)) x[[f]] <- factor(as.character(x[[f]]), levels = weka_levels[[f]])
    x
  } else NULL
  safe <- setNames(make.names(vars, unique = TRUE), vars)
  map <- setNames(vars, safe)
  names(d) <- unname(safe[vars])
  tgt <- safe[[o$target]]
  feats <- unname(safe[o$features])
  factors <- list()
  for (f in feats) if (!is.numeric(d[[f]])) { d[[f]] <- factor(d[[f]]); factors[[f]] <- levels(d[[f]]) }
  lev <- NULL; positive <- NULL; labels <- NULL
  if (task == "classification") {
    raw_lev <- sort(unique(as.character(d[[tgt]])))
    positive_raw <- if (!is.null(o$positive) && o$positive %in% raw_lev) o$positive else raw_lev[length(raw_lev)]
    raw_lev <- c(positive_raw, setdiff(raw_lev, positive_raw))   # caret treats the first level as the event
    labels <- setNames(raw_lev, make.names(raw_lev, unique = TRUE))
    d[[tgt]] <- factor(make.names(as.character(d[[tgt]]), unique = FALSE), levels = names(labels))
    lev <- names(labels); positive <- lev[1]
  }
  # Classes in the order WEKA lists them: as the file declares them, otherwise alphabetical.
  class_order <- if (is.null(labels)) NULL else {
    raw <- sort(unname(labels)); dl <- declared[[o$target]]
    names(labels)[match(if (is.null(dl)) raw else c(intersect(dl, raw), setdiff(raw, dl)), labels)]
  }
  # Supplied test set: the models are trained on all the data and scored on the separate file.
  supplied <- identical(o$sampling$method, "supplied")
  tst <- NULL
  if (supplied) {
    if (is.null(test$data)) stop(analysis_failure("data", "No test file was supplied.", "Choose the test file under Test options, or pick another test option."))
    tst <- ml_prepare_test(test$data, df, o, missing_code, handles_na, weka_any, weka_levels, safe, factors, labels, task)
    warn <- c(warn, tst$notes)
  }
  y <- d[[tgt]]
  if (n < 100) warn <- c(warn, sprintf("n = %d is small for machine learning; estimates of predictive performance will be unstable.", n))
  if (identical(o$scaling, "none") && any(o$learners %in% ML_SCALE))
    warn <- c(warn, sprintf("Features were not scaled: %s depend on the units of the features, so features with large values dominate.",
                            paste(unname(ML_LEARNERS[intersect(ML_SCALE, o$learners)]), collapse = ", ")))

  # Resampling shared by all learners (fair comparison, as in WEKA's Experimenter and Orange's Test & Score).
  set.seed(o$seed)
  s <- o$sampling
  test_idx <- integer(0)
  train_d <- d
  train_rows <- seq_len(n)
  if (s$method == "split") {
    tr <- caret::createDataPartition(y, p = 1 - s$testShare, list = FALSE)[, 1]
    test_idx <- setdiff(seq_len(n), tr)
    train_d <- d[tr, , drop = FALSE]
    train_rows <- tr
  }
  ytr <- train_d[[tgt]]
  k <- min(s$folds, nrow(train_d) - 1)
  index <- if (s$method == "repeatedcv") caret::createMultiFolds(ytr, k = k, times = s$repeats) else caret::createFolds(ytr, k = k, returnTrain = TRUE)
  summ <- if (task == "regression") caret::defaultSummary
          else if (length(lev) == 2) function(data, lev = NULL, model = NULL) c(caret::twoClassSummary(data, lev, model), caret::defaultSummary(data, lev, model))
          else caret::defaultSummary
  balance <- if (task == "classification") o$balance %||% "none" else "none"
  ctl <- caret::trainControl(method = if (s$method == "repeatedcv") "repeatedcv" else "cv", number = k, repeats = if (s$method == "repeatedcv") s$repeats else NA,
                             index = index, savePredictions = "final", classProbs = task == "classification", summaryFunction = summ,
                             sampling = ml_sampling(balance))
  metric <- if (task == "regression") "RMSE" else if (length(lev) == 2) "ROC" else "Accuracy"
  form <- stats::as.formula(paste(tgt, "~ ."))

  fits <- list(); learners_out <- list()
  for (i in seq_along(o$learners)) {
    L <- o$learners[i]
    progress("ml", sprintf("Training %s (%d of %d)", ML_LEARNERS[[L]], i, length(o$learners)), 0.05 + 0.85 * (i - 1) / length(o$learners))
    method <- if (L == "lm") (if (task == "regression") "lm" else if (length(lev) == 2) "glm" else "multinom") else if (L %in% names(ML_CARET_METHOD)) ML_CARET_METHOD[[L]] else L
    weka_lm <- L == "lm" && task == "regression" && ml_weka_lm_on(o)
    args <- list(form, data = train_d, method = if (weka_lm) weka_lr_caret(o$lm$selection, o$lm$eliminate, o$lm$ridge) else if (L %in% ML_WEKA) weka_lazy_caret(L, o$weka, task, class_order, labels) else if (L == "xgb") xgb_caret() else method,
                 trControl = ctl, metric = metric, tuneLength = o$tuneLength)
    if (weka_lm) method <- "weka_lr"
    xw <- if (L %in% ML_WEKA) dw[train_rows, , drop = FALSE] else NULL
    if (L %in% ML_WEKA) {
      method <- paste0("weka_", L)
      args <- list(x = xw, y = ytr, method = args$method, trControl = ctl, metric = metric)
    }
    # "Set values": the learner is fitted with exactly the user's hyperparameters (a one-row grid) instead of a search.
    if (!is.null(ML_TUNE[[L]]) && identical(o$tune[[L]]$mode, "fixed")) {
      tv <- ml_tune_values(L, o, task, length(feats))
      if (L == "ranger") {
        p <- ncol(stats::model.matrix(form, train_d)) - 1L   # predictor columns after dummy coding
        if (tv$mtry > p) { warn <- c(warn, sprintf("Random forest: %d features per split is more than the %d predictor columns; %d was used.", tv$mtry, p, p)); tv$mtry <- p }
        rules <- if (task == "classification") c("gini", "extratrees", if (length(lev) == 2) "hellinger") else c("variance", "extratrees", "maxstat")
        if (!tv$splitrule %in% rules) { warn <- c(warn, sprintf("Random forest: the split rule '%s' does not apply here; '%s' was used.", tv$splitrule, rules[1])); tv$splitrule <- rules[1] }
      }
      extra <- names(Filter(function(d) isTRUE(d$extra), ML_TUNE[[L]]))
      args$tuneGrid <- as.data.frame(tv[setdiff(names(tv), extra)], stringsAsFactors = FALSE)
      args$tuneLength <- NULL
      for (k in extra) args[[k]] <- tv[[k]]
    }
    pp <- ml_preprocess(L, o$scaling)
    # Scaling applies to numeric features only; WEKA learners with categories alone get none.
    if (length(pp) && !(L %in% ML_WEKA && !any(vapply(xw, is.numeric, TRUE)))) args$preProcess <- pp
    if (L == "ranger") args$importance <- "permutation"
    if (L == "nnet") { args$trace <- FALSE; args$MaxNWts <- 100000; if (task == "regression") args$linout <- TRUE }
    if (L == "gbm") args$verbose <- FALSE
    if (method == "multinom") args$trace <- FALSE
    if (method == "glm") args$family <- stats::binomial()
    t1 <- Sys.time()
    set.seed(o$seed)
    fw <- character(0)
    fit <- withCallingHandlers(tryCatch(do.call(caret::train, args), error = function(e) e),
                               warning = function(w) { fw <<- c(fw, conditionMessage(w)); invokeRestart("muffleWarning") })
    if (inherits(fit, "error")) {
      learners_out[[length(learners_out) + 1]] <- list(learner = L, label = ML_LEARNERS[[L]], ok = FALSE, error = sanitize_message(conditionMessage(fit)))
      warn <- c(warn, sprintf("%s could not be trained: %s", ML_LEARNERS[[L]], sanitize_message(conditionMessage(fit))))
      next
    }
    fits[[L]] <- fit
    # Evaluation predictions: pooled out-of-fold (cv), first repeat (repeated cv), the test set (split) or the training set.
    if (s$method %in% c("cv", "repeatedcv")) {
      p <- fit$pred
      if (s$method == "repeatedcv") p <- p[grepl("Rep0?1$", p$Resample), , drop = FALSE]
      p <- p[order(p$rowIndex), , drop = FALSE]
      eval_rows <- p$rowIndex; obs <- p$obs; pred <- p$pred
      prob <- if (task == "classification") p[, lev, drop = FALSE] else NULL
      prior <- if (task == "regression") vapply(as.character(p$Resample), function(f) mean(ytr[index[[f]]]), 0) else NULL
    } else {
      ev <- if (s$method == "split") d[test_idx, , drop = FALSE] else if (supplied) tst$d else train_d
      eval_rows <- if (s$method == "split") test_idx else if (supplied) seq_len(nrow(tst$d)) else seq_len(nrow(train_d))
      newx <- if (L %in% ML_WEKA) (if (supplied) tst$dw else dw[eval_rows, , drop = FALSE]) else ev
      obs <- ev[[tgt]]; pred <- stats::predict(fit, newx, na.action = stats::na.pass)
      prob <- if (task == "classification") stats::predict(fit, newx, type = "prob", na.action = stats::na.pass) else NULL
      prior <- if (task == "regression") rep(mean(ytr), length(obs)) else NULL
    }
    mets <- ml_metrics(obs, pred, prob, task, lev, positive, prior)
    fold <- fit$resample
    foldm <- lapply(setdiff(names(fold), "Resample"), function(m) list(metric = m, mean = mean(fold[[m]], na.rm = TRUE), sd = stats::sd(fold[[m]], na.rm = TRUE)))
    vi <- tryCatch({ v <- caret::varImp(fit)$importance; v <- v[, if ("Overall" %in% names(v)) "Overall" else 1, drop = TRUE] ; nm <- rownames(caret::varImp(fit)$importance)
                     o2 <- order(-v); lapply(o2, function(j) list(feature = ml_display(nm[j], map, factors), importance = unname(v[j]))) }, error = function(e) list())
    out <- list(learner = L, label = ML_LEARNERS[[L]], method = method, ok = TRUE, seconds = as.numeric(difftime(Sys.time(), t1, units = "secs")),
                bestTune = lapply(as.list(fit$bestTune), function(v) if (is.factor(v)) as.character(v) else v),
                tuning = lapply(seq_len(nrow(fit$results)), function(j) lapply(as.list(fit$results[j, , drop = FALSE]), function(v) if (is.factor(v)) as.character(v) else v)),
                metrics = mets, foldMetrics = foldm, importance = vi, model = ml_relabel(ml_model_text(fit, if (method == "weka_lr") "weka_lr" else L, tgt_label = o$target), labels),
                coefficients = ml_coefficients(fit, method, map, factors),
                warnings = as.list(unique(utils::head(sanitize_message(fw), 3))))
    if (task == "classification") {
      tab <- table(factor(obs, levels = lev), factor(pred, levels = lev))
      out$confusion <- list(levels = unname(labels[lev]), counts = lapply(seq_along(lev), function(a) as.list(as.integer(tab[a, ]))))
      out$byClass <- ml_by_class(factor(obs, levels = lev), factor(pred, levels = lev), if (is.null(prob)) NULL else as.data.frame(prob), lev, labels, class_order)
      if (length(lev) == 2) {
        rc <- tryCatch(pROC::roc(obs, prob[[positive]], levels = rev(lev), direction = "<", quiet = TRUE), error = function(e) NULL)
        if (!is.null(rc)) {
          sel <- unique(round(seq(1, length(rc$specificities), length.out = min(200, length(rc$specificities)))))
          out$roc <- list(fpr = as.list(1 - rc$specificities[sel]), tpr = as.list(rc$sensitivities[sel]))
        }
      }
    } else {
      sel <- if (length(obs) > 1500) round(seq(1, length(obs), length.out = 1500)) else seq_along(obs)
      out$fitted <- list(observed = as.list(as.numeric(obs[sel])), predicted = as.list(as.numeric(pred[sel])))
    }
    # Predictions of a supplied test file belong to that file, not to rows of the dataset.
    out$.pred <- list(rows = if (supplied) integer(0) else eval_rows, pred = if (supplied) pred[0] else pred,
                      prob = if (!is.null(prob) && !supplied) prob[[positive]] else NULL)
    learners_out[[length(learners_out) + 1]] <- out
  }
  ok <- Filter(function(x) isTRUE(x$ok), learners_out)
  if (!length(ok)) stop(analysis_failure("estimation", "None of the learners could be trained.", "Check the target and features."))
  progress("results", "Comparing the learners", 0.93)

  # Pairwise comparison of learners on the shared resamples (caret::resamples / diff: paired t tests, Bonferroni).
  comparison <- NULL
  if (length(fits) >= 2 && s$method != "train") {
    cmp <- tryCatch({
      rs <- caret::resamples(fits)
      st <- diff(rs, metric = metric, adjustment = "bonferroni")$statistics[[metric]]
      pairs <- names(st)
      list(metric = metric, pairs = lapply(pairs, function(pn) { ab <- strsplit(pn, ".diff.", fixed = TRUE)[[1]]; t <- st[[pn]]
        list(a = ML_LEARNERS[[ab[1]]], b = ML_LEARNERS[[ab[2]]], difference = unname(t$estimate), p = min(1, t$p.value * length(pairs)), t = unname(t$statistic)) }))
    }, error = function(e) NULL)
    comparison <- cmp
  }
  primary <- if (task == "regression") "RMSE" else if (length(lev) == 2) "auc" else "accuracy"
  vals <- vapply(ok, function(x) num_of(x$metrics[[primary]]), 0)
  best <- ok[[if (primary == "RMSE") which.min(vals) else which.max(vals)]]$learner

  # Out-of-fold / test predictions of every learner for derived variables (by dataset row).
  tabs <- list()
  for (x in if (supplied) list() else ok) {   # a test file's predictions are not rows of the dataset
    pr <- x$.pred
    if (task == "regression") tabs$predictions[[x$learner]] <- { v <- rep(NA_real_, n); v[pr$rows] <- as.numeric(pr$pred); v }
    else {
      cls <- rep(NA_integer_, n); cls[pr$rows] <- match(as.character(pr$pred), lev)
      tabs$predictions[[x$learner]] <- cls
      if (!is.null(pr$prob)) tabs$predictions[[paste0(x$learner, "_prob")]] <- { v <- rep(NA_real_, n); v[pr$rows] <- pr$prob; v }
    }
  }
  learners_out <- lapply(learners_out, function(x) { x$.pred <- NULL; x })
  progress("results", "Evaluating the attributes", 0.96)
  ax <- if (!is.null(dw)) dw else stats::setNames(d[, feats, drop = FALSE], unname(map[feats]))
  list(
    status = "completed", kind = "ml", completedAt = now_iso(),
    options = utils::modifyList(o, list(features = as.list(o$features), learners = as.list(o$learners), balance = balance)),
    task = task,
    classes = if (!is.null(labels)) list(levels = as.list(unname(labels)), positive = unname(labels[positive]),
                                         counts = lapply(names(labels), function(l) list(class = labels[[l]], n = sum(y == l)))) else NULL,
    n = c(list(input = nrow(df), used = n, rowsRemoved = nrow(df) - n, cellsRecoded = recoded, train = nrow(train_d), test = if (supplied) nrow(tst$d) else length(test_idx)),
          if (supplied) list(testFile = test$filename %||% "", testInput = tst$input, testRowsRemoved = tst$input - nrow(tst$d))),
    folds = k, metric = metric, primary = primary, best = best,
    learners = learners_out,
    comparison = comparison,
    rank = ml_rank(d, y, task, feats, map, positive),
    attributeEval = ml_attr_eval(ax, y, task),
    warnings = as.list(unique(warn)),
    rCode = ml_code(o, task, positive_label = if (!is.null(labels)) unname(labels[positive]) else NULL, missing_code, weka_levels, test_name = if (supplied) test$filename else NULL),
    packages = list(caret = as.character(utils::packageVersion("caret"))),
    durationSeconds = as.numeric(difftime(Sys.time(), t0, units = "secs")),
    .derived = list(nInput = nrow(df), rows = rows, tables = list(predictions = as.data.frame(tabs$predictions %||% list(), check.names = FALSE)))
  )
}

ml_code <- function(o, task, positive_label, missing_code, weka_levels = list(), test_name = NULL) {
  q <- function(x) paste0("\"", gsub("\"", "\\\\\"", x), "\"")
  vars <- c(o$target, o$features)
  s <- o$sampling
  weka_any <- any(o$learners %in% ML_WEKA)
  handles_na <- all(o$learners %in% ML_WEKA)
  lines <- c("# Machine learning generated by PathLab SEM (caret)", "library(caret)", if ("gbm" %in% o$learners) "library(gbm)",
             "# data <- read.csv(\"your_data.csv\")   # the same dataset as in the app",
             paste0("d <- data[, c(", paste(vapply(vars, q, ""), collapse = ", "), ")]"))
  if (!is.null(missing_code)) lines <- c(lines, paste0("d[d == ", missing_code, "] <- NA   # project missing-value code"))
  lines <- c(lines, if (handles_na) sprintf("d <- d[!is.na(d[[%s]]), ]   # WEKA's learners handle missing feature values themselves", q(o$target)) else "d <- d[complete.cases(d), ]")
  if (weka_any) lines <- c(lines, "# The WEKA learners get the features as WEKA reads them: nominal attributes as categories (with the labels",
                           "# the file declares, in its order), not dummy columns; missing values are kept",
                           paste0("xw <- d[, c(", paste(vapply(o$features, q, ""), collapse = ", "), "), drop = FALSE]"),
                           vapply(names(weka_levels), function(f) sprintf("xw[[%s]] <- factor(xw[[%s]], levels = c(%s))", q(f), q(f), paste(vapply(weka_levels[[f]], q, ""), collapse = ", ")), ""))
  lines <- c(lines, "names(d) <- make.names(names(d), unique = TRUE)", sprintf("y <- %s", q(make.names(o$target))))
  if (task == "classification") lines <- c(lines, sprintf("d[[y]] <- factor(make.names(d[[y]]))\nd[[y]] <- relevel(d[[y]], ref = %s)   # positive class first", q(make.names(positive_label))))
  lines <- c(lines, sprintf("set.seed(%d)", o$seed))
  if (s$method == "split") lines <- c(lines, sprintf("tr <- createDataPartition(d[[y]], p = %s, list = FALSE)[, 1]", 1 - s$testShare),
                                      if (weka_any) "xw_test <- xw[-tr, , drop = FALSE]; xw <- xw[tr, , drop = FALSE]", "test <- d[-tr, ]; d <- d[tr, ]")
  if (s$method == "supplied") {
    lines <- c(lines, sprintf("# test <- read.csv(%s)   # the supplied test set: the same columns as the data", q(test_name %||% "your_test_file.csv")),
               paste0("test <- test[, c(", paste(vapply(vars, q, ""), collapse = ", "), ")]"),
               if (!is.null(missing_code)) paste0("test[test == ", missing_code, "] <- NA"),
               if (handles_na) sprintf("test <- test[!is.na(test[[%s]]), ]", q(o$target)) else "test <- test[complete.cases(test), ]")
    if (weka_any) lines <- c(lines, paste0("xw_test <- test[, c(", paste(vapply(o$features, q, ""), collapse = ", "), "), drop = FALSE]"),
                             vapply(names(weka_levels), function(f) sprintf("xw_test[[%s]] <- factor(xw_test[[%s]], levels = levels(xw[[%s]]))", q(f), q(f), q(f)), ""))
    lines <- c(lines, "names(test) <- make.names(names(test), unique = TRUE)",
               if (task == "classification") "test[[y]] <- factor(make.names(test[[y]]), levels = levels(d[[y]]))   # the classes of the training data")
  }
  lines <- c(lines, if (s$method == "repeatedcv") sprintf("index <- createMultiFolds(d[[y]], k = %d, times = %d)", s$folds, s$repeats) else sprintf("index <- createFolds(d[[y]], k = %d, returnTrain = TRUE)", s$folds),
             if (task == "classification" && identical(o$balance, "smote"))
               c("# SMOTE on each training fold (synthetic cases of the smaller classes)", paste0("ml_smote <- ", paste(deparse(ml_smote), collapse = "\n"))),
             sprintf("ctl <- trainControl(method = %s, index = index, savePredictions = \"final\"%s%s)", q(if (s$method == "repeatedcv") "repeatedcv" else "cv"),
                     if (task == "classification") ", classProbs = TRUE" else "",
                     if (task != "classification" || is.null(o$balance) || o$balance == "none") "" else if (o$balance == "smote")
                       ", sampling = list(name = \"SMOTE\", func = ml_smote, first = TRUE)" else sprintf(", sampling = %s", q(o$balance))),
             "fits <- list()")
  # "Set values": a one-row grid with the user's hyperparameters (and the fit arguments that are not tuned).
  tuning <- function(L) {
    if (is.null(ML_TUNE[[L]]) || !identical(o$tune[[L]]$mode, "fixed")) return(sprintf("tuneLength = %d", o$tuneLength))
    tv <- ml_tune_values(L, o, task, length(o$features))
    lit <- function(x) if (is.logical(x)) (if (x) "TRUE" else "FALSE") else if (is.character(x)) q(x) else if (is.integer(x)) paste0(x, "L") else format(x)
    ex <- names(Filter(function(d) isTRUE(d$extra), ML_TUNE[[L]]))
    g <- setdiff(names(tv), ex)
    paste0("tuneGrid = data.frame(", paste(sprintf("%s = %s", g, vapply(tv[g], lit, "")), collapse = ", "), ")",
           if (length(ex)) paste0(", ", paste(sprintf("%s = %s", ex, vapply(tv[ex], lit, "")), collapse = ", ")) else "", "   # values set by you")
  }
  for (L in o$learners) {
    method <- if (L == "lm") (if (task == "regression") "lm" else "glm") else if (L %in% names(ML_CARET_METHOD)) ML_CARET_METHOD[[L]] else L
    if (L == "xgb") {
      lines <- c(lines, "# XGBoost through its own interface (xgboost 3.x) as a caret model", trimws(XGB_SRC), sprintf("set.seed(%d)", o$seed),
                 sprintf("fits$xgb <- train(as.formula(paste(y, \"~ .\")), data = d, method = xgb_caret(), trControl = ctl, %s)", tuning("xgb")))
      next
    }
    if (L %in% ML_WEKA) {
      if (!any(grepl("library(RWeka)", lines, fixed = TRUE))) lines <- c(lines, "library(RWeka)   # WEKA's lazy classifiers (needs 64-bit Java)")
      ctl_txt <- weka_control_text(L, o$weka, task)
      lines <- c(lines, sprintf("# %s: weka.classifiers.%s, scored on the same folds", ML_LEARNERS[[L]], WEKA_JAVA_NAME[[L]]),
                 sprintf("W_%s <- make_Weka_classifier(%s)", L, q(WEKA_LAZY_CLASS[[L]])),
                 sprintf("fits$%s <- train(x = xw, y = d[[y]], trControl = ctl, method = list(", L),
                 "  library = \"RWeka\", type = c(\"Regression\", \"Classification\"), sort = function(x) x,",
                 "  parameters = data.frame(parameter = \"parameter\", class = \"character\", label = \"parameter\"),",
                 "  grid = function(x, y, len = NULL, search = \"grid\") data.frame(parameter = \"none\"),",
                 sprintf("  fit = function(x, y, ...) { d2 <- as.data.frame(x); d2$.outcome <- y; W_%s(.outcome ~ ., data = d2, na.action = na.pass, control = %s) },", L, ctl_txt),
                 "  predict = function(modelFit, newdata, ...) predict(modelFit, as.data.frame(newdata)),",
                 "  prob = function(modelFit, newdata, ...) as.data.frame(predict(modelFit, as.data.frame(newdata), type = \"probability\"))))")
      next
    }
    if (L == "lm" && task == "regression" && ml_weka_lm_on(o)) {
      lines <- c(lines, "# WEKA's LinearRegression as a caret model (attribute selection, collinear attributes, ridge)", trimws(WEKA_LR_SRC),
                 sprintf("set.seed(%d)", o$seed),
                 sprintf("fits$lm <- train(as.formula(paste(y, \"~ .\")), data = d, method = weka_lr_caret(%s, %s, %s), trControl = ctl)",
                         q(o$lm$selection), if (isTRUE(o$lm$eliminate)) "TRUE" else "FALSE", format(o$lm$ridge)))
      next
    }
    pp <- ml_preprocess(L, o$scaling)
    extra <- c(if (length(pp)) paste0("preProcess = ", if (length(pp) == 1) q(pp) else paste0("c(", paste(vapply(pp, q, ""), collapse = ", "), ")")), if (L == "ranger") "importance = \"permutation\"",
               if (L == "nnet") paste0("trace = FALSE", if (task == "regression") ", linout = TRUE"), if (L == "gbm") "verbose = FALSE", if (method == "glm") "family = binomial()")
    lines <- c(lines, sprintf("set.seed(%d)", o$seed),
               sprintf("fits$%s <- train(as.formula(paste(y, \"~ .\")), data = d, method = %s, trControl = ctl, %s%s)", L, q(method), sub("   # values set by you$", "", tuning(L)),
                       if (length(extra)) paste0(", ", paste(extra, collapse = ", ")) else ""),
               if (identical(o$tune[[L]]$mode, "fixed")) sprintf("# %s: hyperparameters set by you, not tuned", ML_LEARNERS[[L]]))
  }
  lines <- c(lines, "lapply(fits, getTrainPerf)                 # mean performance over the folds",
             "summary(diff(resamples(fits)))             # pairwise comparison of learners",
             "lapply(fits, varImp)                       # feature importance")
  if (s$method %in% c("split", "supplied")) lines <- c(lines, if (weka_any)
    "lapply(fits, function(f) postResample(predict(f, if (is.null(f$terms)) xw_test else test, na.action = na.pass), test[[y]]))   # test-set performance"
    else "lapply(fits, function(f) postResample(predict(f, test), test[[y]]))   # test-set performance")
  paste(lines, collapse = "\n")
}

# ---- SEM-ANN (two-stage PLS-SEM -> artificial neural network) -----------------------------------------------
# Inputs (usually the significant predictors of an endogenous construct in PLS-SEM, as construct scores) and the
# output are min-max normalised to [0, 1]; a multilayer perceptron with one sigmoid hidden layer (nnet) is trained on
# each of k folds (k networks: train on k - 1 folds, test on the held-out fold); RMSE on training and test data per
# network; relative importance per network from Garson's algorithm (NeuralNetTools::garson), averaged and normalised
# to the most important input (= 100%). Compared with the PLS-SEM path coefficients when they are supplied.

normalize_semann <- function(raw) {
  raw <- raw %||% list()
  paths <- raw$plsPaths %||% list()
  dec <- suppressWarnings(as.numeric(raw$decay %||% 0.01))
  if (is.na(dec) || dec < 0 || dec > 1) api_error(400, "invalid_ml", "Weight decay must be between 0 and 1.")
  list(target = as.character(raw$target %||% "")[1], features = unique(as_chr_vec(raw$features)),
       hidden = int_in(raw$hidden, 0L, 0L, 50L, "The number of hidden units"),      # 0 = automatic
       decay = dec,
       networks = int_in(raw$networks, 10L, 2L, 20L, "The number of networks"),
       seed = int_in(raw$seed, 2026L, 1L, .Machine$integer.max, "The seed"),
       plsPaths = lapply(paths, function(p) list(from = as.character(p$from %||% ""), coefficient = suppressWarnings(as.numeric(p$coefficient %||% NA)),
                                                 p = suppressWarnings(as.numeric(p$p %||% NA)))))
}

run_semann <- function(df, o, missing_code = NULL, progress = function(...) NULL) {
  if (!nzchar(o$target) || !length(o$features)) stop(analysis_failure("validation", "Choose the output construct and at least one input.", NULL))
  miss <- setdiff(c(o$target, o$features), names(df))
  if (length(miss)) stop(analysis_failure("validation", sprintf("Not in the dataset: %s.", paste(miss, collapse = ", ")), "Save the construct scores of the PLS-SEM model first."))
  nonnum <- Filter(function(v) !is.numeric(df[[v]]), c(o$target, o$features))
  if (length(nonnum)) stop(analysis_failure("validation", sprintf("SEM-ANN needs numeric variables; not numeric: %s.", paste(nonnum, collapse = ", ")), NULL))
  t0 <- Sys.time()
  vars <- c(o$target, o$features)
  d <- df[, vars, drop = FALSE]
  if (!is.null(missing_code)) for (cn in vars) d[[cn]][!is.na(d[[cn]]) & d[[cn]] == missing_code] <- NA
  keep <- stats::complete.cases(d); rows <- which(keep); d <- d[keep, , drop = FALSE]
  n <- nrow(d)
  if (n < 30) stop(analysis_failure("data", sprintf("Only %d complete rows remain.", n), NULL))
  safe <- setNames(make.names(vars, unique = TRUE), vars); map <- setNames(vars, safe)
  names(d) <- unname(safe[vars])
  dn <- as.data.frame(lapply(d, function(x) { r <- range(x); if (diff(r) == 0) x * 0 else (x - r[1]) / diff(r) }))
  tgt <- safe[[o$target]]; ins <- unname(safe[o$features])
  hidden <- if (o$hidden > 0) o$hidden else max(1L, as.integer(round(2 / 3 * length(ins) + 1)))
  set.seed(o$seed)
  folds <- caret::createFolds(dn[[tgt]], k = o$networks, returnTrain = FALSE)
  form <- stats::as.formula(paste(tgt, "~", paste(ins, collapse = " + ")))
  nets <- list(); imp <- list()
  for (k in seq_along(folds)) {
    progress("ann", sprintf("Training network %d of %d", k, length(folds)), 0.05 + 0.85 * (k - 1) / length(folds))
    te <- folds[[k]]; tr <- setdiff(seq_len(n), te)
    set.seed(o$seed + k)
    # do.call puts the formula itself into the call, which NeuralNetTools::garson re-evaluates.
    nn <- do.call(nnet::nnet, list(formula = form, data = dn[tr, , drop = FALSE], size = hidden, decay = o$decay, linout = FALSE, maxit = 1000, trace = FALSE))
    pr_tr <- as.numeric(stats::predict(nn, dn[tr, , drop = FALSE])); pr_te <- as.numeric(stats::predict(nn, dn[te, , drop = FALSE]))
    g <- NeuralNetTools::garson(nn, bar_plot = FALSE)
    ri <- setNames(g$rel_imp, rownames(g))
    nets[[k]] <- list(network = k, nTrain = length(tr), nTest = length(te),
                      rmseTrain = unname(caret::RMSE(pr_tr, dn[[tgt]][tr])), rmseTest = unname(caret::RMSE(pr_te, dn[[tgt]][te])),
                      sseTrain = sum((pr_tr - dn[[tgt]][tr])^2), sseTest = sum((pr_te - dn[[tgt]][te])^2),
                      importance = lapply(ins, function(i) list(input = map[[i]], relative = unname(ri[[i]]))))
    imp[[k]] <- ri[ins]
  }
  M <- do.call(rbind, imp)
  mean_imp <- colMeans(M)
  norm <- 100 * mean_imp / max(mean_imp)
  ord <- order(-norm)
  pls <- setNames(o$plsPaths, vapply(o$plsPaths, `[[`, "", "from"))
  pls_rank <- if (length(pls)) { cf <- vapply(o$features, function(f) if (is.null(pls[[f]])) NA_real_ else abs(num_of(pls[[f]]$coefficient)), 0); stats::setNames(rank(-cf, ties.method = "min", na.last = "keep"), o$features) } else NULL
  sens <- lapply(seq_along(ins), function(j) {
    f <- map[[ins[j]]]
    list(input = f, meanImportance = unname(mean_imp[j]), sdImportance = stats::sd(M[, j]), normalized = unname(norm[j]), annRank = match(j, ord),
         plsCoefficient = if (!is.null(pls[[f]])) pls[[f]]$coefficient else NULL, plsP = if (!is.null(pls[[f]])) pls[[f]]$p else NULL,
         plsRank = if (!is.null(pls_rank) && !is.na(pls_rank[[f]])) unname(pls_rank[[f]]) else NULL)
  })
  rt <- vapply(nets, `[[`, 0, "rmseTrain"); rte <- vapply(nets, `[[`, 0, "rmseTest")
  list(
    status = "completed", kind = "ml", mode = "semann", completedAt = now_iso(),
    options = utils::modifyList(o, list(features = as.list(o$features))),
    n = list(input = nrow(df), used = n, rowsRemoved = nrow(df) - n),
    architecture = list(inputs = length(ins), hidden = hidden, outputs = 1L, activation = "sigmoid (hidden and output)", decay = o$decay,
                        label = sprintf("%d-%d-1", length(ins), hidden)),
    networks = nets,
    rmse = list(trainMean = mean(rt), trainSd = stats::sd(rt), testMean = mean(rte), testSd = stats::sd(rte)),
    sensitivity = sens[ord],
    warnings = as.list(if (n < 100) sprintf("n = %d is small for training neural networks.", n) else character(0)),
    rCode = semann_code(o, hidden),
    packages = list(nnet = as.character(utils::packageVersion("nnet")), NeuralNetTools = as.character(utils::packageVersion("NeuralNetTools"))),
    durationSeconds = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
}

semann_code <- function(o, hidden) {
  q <- function(x) paste0("\"", gsub("\"", "\\\\\"", x), "\"")
  vars <- c(o$target, o$features)
  paste(c("# SEM-ANN generated by PathLab SEM (nnet + NeuralNetTools)", "library(nnet); library(NeuralNetTools); library(caret)",
          paste0("d <- data[, c(", paste(vapply(vars, q, ""), collapse = ", "), ")]   # construct scores saved from the PLS-SEM model"),
          "d <- d[complete.cases(d), ]", "names(d) <- make.names(names(d), unique = TRUE)",
          "dn <- as.data.frame(lapply(d, function(x) (x - min(x)) / (max(x) - min(x))))   # min-max normalisation",
          sprintf("set.seed(%d)", o$seed), sprintf("folds <- createFolds(dn[[1]], k = %d)", o$networks),
          "res <- lapply(seq_along(folds), function(k) {",
          "  te <- folds[[k]]; tr <- setdiff(seq_len(nrow(dn)), te)",
          sprintf("  set.seed(%d + k)", o$seed),
          sprintf("  nn <- nnet(%s ~ %s, data = dn[tr, ], size = %d, decay = %s, linout = FALSE, maxit = 1000, trace = FALSE)", make.names(o$target), paste(make.names(o$features), collapse = " + "), hidden, o$decay),
          "  g <- garson(nn, bar_plot = FALSE)",
          "  c(rmse_train = RMSE(as.numeric(predict(nn, dn[tr, ])), dn[tr, 1]), rmse_test = RMSE(as.numeric(predict(nn, dn[te, ])), dn[te, 1]),",
          "    setNames(g$rel_imp, rownames(g)))",
          "})",
          "res <- do.call(rbind, res); res                         # RMSE and relative importance per network",
          "imp <- colMeans(res[, -(1:2)]); 100 * imp / max(imp)     # normalised importance (%)"), collapse = "\n")
}

# ---- Report, workbook and APA tables (from the stored result) ---------------------------------------------

ML_METRIC_LABELS <- list(regression = c(RMSE = "RMSE", MSE = "MSE", MAE = "MAE", R2 = "R²", correlation = "r", RAE = "RAE (%)", RRSE = "RRSE (%)"),
                         classification = c(auc = "AUC", accuracy = "CA", f1 = "F1", precision = "Precision", recall = "Recall", mcc = "MCC", kappa = "Kappa"))

ml_test_mode <- function(res) {
  s <- res$options$sampling
  switch(s$method,
         cv = sprintf("%d-fold cross-validation (stratified)", as.integer(res$folds)),
         repeatedcv = sprintf("%d-fold cross-validation repeated %d times (first repeat pooled)", as.integer(res$folds), as.integer(s$repeats)),
         split = sprintf("percentage split: %d%% training (tuned by %d-fold cross-validation), %d%% test", round(100 * (1 - s$testShare)), as.integer(res$folds), round(100 * s$testShare)),
         train = "evaluated on the training data (optimistic)",
         supplied = sprintf("supplied test set: trained on all %d cases (tuned by %d-fold cross-validation), scored on the %d cases of %s",
                            as.integer(res$n$train %||% res$n$used), as.integer(res$folds), as.integer(res$n$test %||% 0), res$n$testFile %||% "the test file"))
}

ml_ok <- function(res) Filter(function(x) isTRUE(x$ok), res$learners)

ml_body_html <- function(res) {
  esc <- html_escape
  S <- character(0); a <- function(...) S <<- c(S, paste0(...))
  o <- res$options
  if (identical(res$mode, "semann")) {
    a("<h2>SEM-ANN (artificial neural network)</h2>")
    a("<p>Two-stage PLS-SEM &ndash; ANN analysis: a multilayer perceptron (", esc(res$architecture$label), "; ", esc(res$architecture$activation), ", weight decay ",
      res$architecture$decay, ") predicting <strong>", esc(o$target), "</strong> from ", esc(paste(ul(o$features), collapse = ", ")), ", trained on ", length(res$networks),
      " networks (each tested on a held-out fold; n = ", res$n$used, "; inputs and output min&ndash;max normalised). nnet ", esc(res$packages$nnet), ", NeuralNetTools ", esc(res$packages$NeuralNetTools), ".</p>")
    for (w in ul(res$warnings)) a("<div class=\"warn\">", esc(w), "</div>")
    rows <- lapply(res$networks, function(x) c(paste("ANN", x$network), as.character(x$nTrain), as.character(x$nTest), fmt(x$rmseTrain, 4), fmt(x$rmseTest, 4)))
    rows <- c(rows, list(c("<strong>Mean</strong>", "", "", fmt(res$rmse$trainMean, 4), fmt(res$rmse$testMean, 4)), c("SD", "", "", fmt(res$rmse$trainSd, 4), fmt(res$rmse$testSd, 4))))
    a(html_table(c("Network", "Training n", "Test n", "RMSE (training)", "RMSE (test)"), rows, "RMSE of the neural networks (normalised scale)", num_cols = 2:5))
    a(html_table(c("Input", "Mean relative importance", "Normalised importance (%)", "ANN rank", "PLS path coefficient", "PLS rank"),
                 lapply(res$sensitivity, function(x) c(esc(x$input), fmt(x$meanImportance), fmt(x$normalized, 1), as.character(x$annRank), fmt(x$plsCoefficient), if (is.null(x$plsRank)) "&ndash;" else as.character(x$plsRank))),
                 "Sensitivity analysis (Garson's algorithm, averaged over the networks)", num_cols = 2:6))
    a("<p class=\"note\">Normalised importance = mean relative importance divided by the largest mean relative importance. The ANN captures non-linear and non-compensatory relationships; compare its ranking with the PLS-SEM ranking.</p>")
    a("<h3>R code</h3><pre><code>", esc(res$rCode %||% ""), "</code></pre>")
    return(paste(S, collapse = "\n"))
  }
  task <- res$task
  ml <- ML_METRIC_LABELS[[task]]
  a("<h2>Machine learning (", if (task == "regression") "regression" else "classification", ")</h2>")
  a("<pre><code>", esc(paste0("=== Run information ===\n\nTarget:      ", o$target, if (!is.null(res$classes)) paste0(" (positive class: ", res$classes$positive, ")") else "",
                              "\nFeatures:    ", paste(ul(o$features), collapse = ", "), "\nInstances:   ", res$n$used,
                              if (res$n$rowsRemoved > 0) paste0(" (", res$n$rowsRemoved, " rows with missing values left out)") else "",
                              "\nLearners:    ", paste(vapply(res$learners, `[[`, "", "label"), collapse = ", "), "\nTest mode:   ", ml_test_mode(res),
                              "\nScaling:     ", unname(ML_SCALING[o$scaling %||% "auto"]), if (!identical(o$scaling, "none")) " (learned within each training fold)" else "",
                              "\nTuning:      grid of ", o$tuneLength, " value(s) per parameter, selected by ", res$metric, "\nSoftware:    caret ", res$packages$caret)), "</code></pre>")
  for (w in ul(res$warnings)) a("<div class=\"warn\">", esc(w), "</div>")
  ok <- ml_ok(res)
  rows <- lapply(ok, function(x) c(paste0(esc(x$label), if (identical(x$learner, res$best)) " <strong>(best)</strong>" else ""), vapply(names(ml), function(k) fmt(x$metrics[[k]]), "")))
  a(html_table(c("Learner", unname(ml)), rows, sprintf("Test and score (%s; pooled predictions)", ml_test_mode(res)), num_cols = seq_len(length(ml)) + 1))
  if (!is.null(res$comparison)) {
    a(html_table(c("Learner A", "Learner B", sprintf("Difference in %s (A &minus; B)", res$comparison$metric), "t", "p (Bonferroni)"),
                 lapply(res$comparison$pairs, function(p) c(esc(p$a), esc(p$b), fmt(p$difference, 4), fmt(p$t, 2), fmt_p(p$p))),
                 "Pairwise comparison of the learners over the shared folds (caret::resamples)", num_cols = 3:5))
    a("<p class=\"note\">Paired t tests on the per-fold results; folds share training cases, so these tests are approximate (Nadeau &amp; Bengio, 2003).</p>")
  }
  for (x in ok) {
    a("<h3>", esc(x$label), "</h3>")
    if (length(x$bestTune)) a("<p>Selected hyperparameters: ", esc(paste(sprintf("%s = %s", names(x$bestTune), vapply(x$bestTune, function(v) format(v, digits = 4), "")), collapse = "; ")), ".</p>")
    if (nzchar(x$model %||% "")) a("<pre><code>", esc(x$model), "</code></pre>")
    if (!is.null(x$confusion)) {
      lv <- ul(x$confusion$levels)
      a(html_table(c("Actual \\ Predicted", esc(lv)), lapply(seq_along(lv), function(i) c(esc(lv[i]), vapply(ul(x$confusion$counts[[i]]), as.character, ""))), "Confusion matrix", num_cols = seq_along(lv) + 1))
    }
    if (length(x$importance)) {
      top <- utils::head(x$importance, 15)
      a(html_table(c("Feature", "Importance (0&ndash;100)"), lapply(top, function(i) c(esc(i$feature), fmt(i$importance, 1))), "Feature importance (caret::varImp, scaled)", num_cols = 2))
    }
  }
  a(html_table(c("Feature", "Test", "Statistic", "Score", "p"), lapply(res$rank, function(r) c(esc(r$feature), esc(r$test), fmt(r$statistic, 3), paste0(fmt(r$score, 3), " (", esc(r$scoreLabel), ")"), fmt_p(r$p))),
               "Univariate feature scores (rank)", num_cols = 3:5))
  a("<h3>R code</h3><pre><code>", esc(res$rCode %||% ""), "</code></pre>")
  paste(S, collapse = "\n")
}

ml_sheets <- function(res, project_name) {
  o <- res$options
  info <- data.frame(Item = c("Project", "Completed (UTC)", "PathLab SEM version", "Target", "Features", "Rows used"),
                     Value = c(project_name, res$completedAt, PATHLAB_VERSION, o$target, paste(ul(o$features), collapse = ", "), res$n$used), stringsAsFactors = FALSE)
  if (identical(res$mode, "semann")) {
    return(list(Info = rbind(info, data.frame(Item = c("Architecture", "Weight decay"), Value = c(res$architecture$label, res$architecture$decay))),
                Networks = do.call(rbind, lapply(res$networks, function(x) data.frame(Network = x$network, TrainN = x$nTrain, TestN = x$nTest, RMSETrain = x$rmseTrain, RMSETest = x$rmseTest, SSETrain = x$sseTrain, SSETest = x$sseTest))),
                Importance = do.call(rbind, lapply(res$networks, function(x) do.call(rbind, lapply(x$importance, function(i) data.frame(Network = x$network, Input = i$input, RelativeImportance = i$relative, stringsAsFactors = FALSE))))),
                Sensitivity = do.call(rbind, lapply(res$sensitivity, function(x) data.frame(Input = x$input, MeanImportance = x$meanImportance, SD = num_of(x$sdImportance), NormalizedPct = x$normalized,
                                                                                       ANNRank = x$annRank, PLSCoefficient = num_of(x$plsCoefficient), PLSRank = num_of(x$plsRank), stringsAsFactors = FALSE)))))
  }
  ok <- ml_ok(res)
  ml <- ML_METRIC_LABELS[[res$task]]
  scores <- do.call(rbind, lapply(ok, function(x) { d <- data.frame(Learner = x$label, Best = identical(x$learner, res$best), stringsAsFactors = FALSE); for (k in names(ml)) d[[ml[[k]]]] <- num_of(x$metrics[[k]]); d }))
  folds <- do.call(rbind, lapply(ok, function(x) do.call(rbind, lapply(x$foldMetrics, function(m) data.frame(Learner = x$label, Metric = m$metric, Mean = num_of(m$mean), SD = num_of(m$sd), stringsAsFactors = FALSE)))))
  imp <- do.call(rbind, lapply(ok, function(x) if (length(x$importance)) do.call(rbind, lapply(x$importance, function(i) data.frame(Learner = x$label, Feature = i$feature, Importance = num_of(i$importance), stringsAsFactors = FALSE)))))
  tune <- do.call(rbind, lapply(ok, function(x) data.frame(Learner = x$label, Hyperparameters = paste(sprintf("%s = %s", names(x$bestTune), vapply(x$bestTune, function(v) format(v, digits = 6), "")), collapse = "; "), stringsAsFactors = FALSE)))
  out <- list(Info = rbind(info, data.frame(Item = c("Task", "Test mode", "caret"), Value = c(res$task, ml_test_mode(res), res$packages$caret))),
              Scores = scores, FoldMetrics = folds, Importance = if (is.null(imp)) data.frame(Note = "Not available") else imp, Hyperparameters = tune,
              Rank = do.call(rbind, lapply(res$rank, function(r) data.frame(Feature = r$feature, Test = r$test, Statistic = num_of(r$statistic), Score = num_of(r$score), ScoreType = r$scoreLabel, p = num_of(r$p), stringsAsFactors = FALSE))))
  if (!is.null(res$comparison)) out$Comparison <- do.call(rbind, lapply(res$comparison$pairs, function(p) data.frame(A = p$a, B = p$b, Difference = p$difference, t = p$t, pBonferroni = p$p, stringsAsFactors = FALSE)))
  out
}

ml_pub_spec <- function(res, opts) {
  d <- opts$decimals; lz <- opts$leadingZero
  o <- res$options
  tabs <- list()
  if (identical(res$mode, "semann")) {
    rows <- lapply(res$networks, function(x) c(sprintf("ANN%d", x$network), as.character(x$nTrain), as.character(x$nTest), pt_num(x$rmseTrain, 4, TRUE, lz), pt_num(x$rmseTest, 4, TRUE, lz)))
    rows <- c(rows, list(c("Mean", "", "", pt_num(res$rmse$trainMean, 4, TRUE, lz), pt_num(res$rmse$testMean, 4, TRUE, lz)), c("SD", "", "", pt_num(res$rmse$trainSd, 4, TRUE, lz), pt_num(res$rmse$testSd, 4, TRUE, lz))))
    tabs[[1]] <- pt_table("ann_rmse", "Root Mean Square Error of the Artificial Neural Networks", c("Network", "Training n", "Test n", "RMSE (training)", "RMSE (test)"), rows,
                          notes = list(sprintf("N = %d. Multilayer perceptron %s (sigmoid activation, weight decay %s) predicting %s; each network is tested on a held-out fold. Inputs and output were min–max normalised.",
                                               as.integer(res$n$used), res$architecture$label, res$architecture$decay, o$target)))
    tabs[[2]] <- pt_table("ann_sensitivity", "Sensitivity Analysis: Normalised Importance of the Inputs", c("Input", "Mean relative importance", "Normalised importance (%)", "ANN rank", "PLS-SEM β", "PLS-SEM rank"),
                          lapply(res$sensitivity, function(x) c(x$input, pt_num(x$meanImportance, d, TRUE, lz), formatC(x$normalized, format = "f", digits = 1), as.character(x$annRank),
                                                                 if (is.null(x$plsCoefficient)) "–" else pt_num(x$plsCoefficient, d), if (is.null(x$plsRank)) "–" else as.character(x$plsRank))),
                          notes = list("Relative importance from Garson's algorithm, averaged over the networks; normalised importance = relative importance / largest relative importance × 100."))
  } else {
    ml <- ML_METRIC_LABELS[[res$task]]
    ok <- ml_ok(res)
    bnd <- c(R2 = TRUE, correlation = TRUE, auc = TRUE, accuracy = TRUE, f1 = TRUE, precision = TRUE, recall = TRUE, mcc = TRUE, kappa = TRUE)
    rows <- lapply(ok, function(x) c(x$label, vapply(names(ml), function(k) pt_num(x$metrics[[k]], d, isTRUE(bnd[k]), lz), "")))
    tabs[[1]] <- pt_table("ml_scores", "Predictive Performance of the Learners", c("Learner", unname(ml)), rows,
                          notes = list(sprintf("N = %d. %s. %s Hyperparameters were tuned by cross-validation (caret %s).", as.integer(res$n$used), tools::toTitleCase(ml_test_mode(res)),
                                               if (res$task == "regression") "RMSE = root mean square error; MAE = mean absolute error; r = correlation of predicted and observed values." else
                                                 sprintf("CA = classification accuracy; MCC = Matthews correlation coefficient; positive class: %s.", res$classes$positive), res$packages$caret)))
    feats <- unique(unlist(lapply(ok, function(x) vapply(x$importance, `[[`, "", "feature"))))
    if (length(feats)) {
      irows <- lapply(feats, function(f) c(f, vapply(ok, function(x) { v <- Filter(function(i) i$feature == f, x$importance); if (length(v)) formatC(num_of(v[[1]]$importance), format = "f", digits = 1) else "–" }, "")))
      tabs[[2]] <- pt_table("ml_importance", "Feature Importance by Learner", c("Feature", vapply(ok, `[[`, "", "label")), irows,
                            notes = list("Model-specific importance from caret::varImp, scaled so that the most important feature of each learner has 100."))
    }
  }
  tabs <- Filter(Negate(is.null), tabs)
  for (i in seq_along(tabs)) tabs[[i]]$number <- i
  tabs
}
