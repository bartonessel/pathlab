# PathLab SEM API (R Plumber). Interactive OpenAPI docs: http://localhost:8000/__docs__/
# Full reference: docs/API.md
#
# Authentication: creating a project returns a random token; every project route
# requires it in the `X-PathLab-Token` header. Data are scoped to that project.

#* @apiTitle PathLab SEM API
#* @apiDescription Browser-facing API for PLS-SEM analyses. All estimation runs in R with the SEMinR package.
#* @apiVersion 0.1.0

token_of <- function(req) req$HTTP_X_PATHLAB_TOKEN

project_of <- function(req, pid) get_project(pid, token_of(req))

json_raw <- function(res, txt, status = 200) {
  res$status <- status
  res$setHeader("Content-Type", "application/json; charset=utf-8")
  res$body <- txt
  res
}

download <- function(res, content, filename, type) {
  res$setHeader("Content-Type", type)
  res$setHeader("Content-Disposition", sprintf("attachment; filename=\"%s\"", gsub("[^A-Za-z0-9._-]", "_", filename)))
  res$body <- content
  res
}

roles_of <- function(body) {
  r <- body$roles
  if (is.null(r) || !length(r)) return(NULL)
  vapply(r, function(v) as.character(v), "")
}

model_request <- function(req, proj, require_dataset = TRUE) {
  body <- parse_json_body(req)
  did <- as.character(body$datasetId %||% "")
  ds <- if (!require_dataset && !nzchar(did)) NULL else get_dataset(proj, did)
  settings <- normalize_settings(body$settings)
  model <- apply_algorithm(normalize_model(body$model), settings)
  model_json <- jsonlite::toJSON(body$model, auto_unbox = TRUE, null = "null", digits = NA)
  list(body = body, ds = ds, model = model, settings = settings, model_json = as.character(model_json),
       dsv = if (is.null(ds)) NULL else list(data = ds$data, profile = ds$profile, roles = roles_of(body)))
}

#* Service health and versions
#* @tag system
#* @get /api/health
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function() {
  list(status = "ok", service = "pathlab-sem", version = PATHLAB_VERSION,
       rVersion = R.version.string, seminrVersion = seminr_version(), time = now_iso())
}

#* Whether WEKA (through RWeka and a 64-bit Java) can run the lazy learners IBk, KStar and LWL.
#* @tag system
#* @get /api/ml/weka
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function() weka_status()

#* Analysis options supported by this server (verified against the installed SEMinR)
#* @tag system
#* @get /api/capabilities
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function() {
  list(
    seminrVersion = seminr_version(),
    rVersion = R.version.string,
    measurementModes = list(
      list(id = "composite_a", label = "Composite – Mode A", seminr = "composite(..., weights = mode_A)",
           description = "Correlation weights. The standard PLS-SEM treatment of constructs that PLS software often calls 'reflective'."),
      list(id = "composite_b", label = "Composite – Mode B", seminr = "composite(..., weights = mode_B)",
           description = "Regression weights. Used for formatively specified constructs whose indicators form the construct."),
      list(id = "reflective", label = "Reflective – common factor (PLSc)", seminr = "reflective(...)",
           description = "Common-factor model estimated with consistent PLS (PLSc), which corrects loadings and paths for measurement error.")
    ),
    innerWeights = list(
      list(id = "path_weighting", label = "Path weighting", seminr = "path_weighting", default = TRUE),
      list(id = "path_factorial", label = "Factorial", seminr = "path_factorial", default = FALSE)
    ),
    missingTreatments = list(
      list(id = "none", label = "No treatment", description = "Only allowed when the model indicators have no missing values."),
      list(id = "mean_replacement", label = "Mean replacement", seminr = "missing = mean_replacement",
           description = "SEMinR replaces each missing value with the indicator mean."),
      list(id = "listwise", label = "Listwise deletion", seminr = "missing = na.omit",
           description = "Rows with any missing value in a model indicator are removed before estimation.")
    ),
    bootstrap = list(minSamples = 20, maxSamples = 50000, defaultSamples = 1000, maxCores = max_cores(),
                     ciMethod = "percentile", seedSupported = TRUE, confidenceLevelSupported = TRUE),
    predict = list(techniques = list(
      list(id = "predict_DA", label = "Direct antecedents (DA)"),
      list(id = "predict_EA", label = "Earliest antecedents (EA)")
    ), minFolds = 2, maxFolds = 50),
    advanced = list(
      mga = list(available = TRUE, source = "seminr::estimate_pls_mga"),
      nca = list(available = requireNamespace("NCA", quietly = TRUE), ceilings = arr(NCA_CEILINGS),
                 version = if (requireNamespace("NCA", quietly = TRUE)) as.character(utils::packageVersion("NCA")) else NULL),
      ipma = list(available = TRUE, source = "Ringle & Sarstedt (2016), computed from SEMinR estimates")
    ),
    # The Statistics references always list jmv (its installed version), also before any analysis has run.
    stats = list(references = stats_references(character(0))),
    # The references shown under each model's results: the app, R, the package that computes it, then the methods
    # literature for that model. They do not depend on a result, so they are listed before the first run too.
    modelReferences = lapply(stats::setNames(nm = c("plssem", "cbsem", "gsca", "lpa", "qca", "ml")), model_references),
    pubTables = pub_tables_available(),
    limits = list(maxUploadMB = as.numeric(Sys.getenv("PATHLAB_MAX_UPLOAD_MB", "25")),
                  sessionTtlHours = session_ttl_seconds() / 3600)
  )
}

#* Download the bundled sample dataset (CSV)
#* @tag sample
#* @get /api/sample/dataset
function(res) {
  path <- file.path(SAMPLE_DIR, "pathlab_sample_technology_adoption.csv")
  download(res, readBin(path, "raw", file.info(path)$size), basename(path), "text/csv")
}

#* Sample project file (model + settings, no data)
#* @tag sample
#* @get /api/sample/project
function(res) {
  json_raw(res, paste(readLines(file.path(SAMPLE_DIR, "sample_project.plj"), warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
}

#* Create a project session. Returns the project id and an access token.
#* @tag projects
#* @post /api/projects
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, res) {
  body <- parse_json_body(req)
  cleanup_expired_projects()
  p <- create_project(body$name)
  res$status <- 201
  list(id = p$id, token = p$token, name = p$name, createdAt = p$createdAt,
       sessionTtlHours = session_ttl_seconds() / 3600)
}

#* Project status: datasets, jobs and results held for this session
#* @tag projects
#* @get /api/projects/<pid>
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid) {
  p <- project_of(req, pid)
  list(id = p$id, name = p$name, createdAt = p$createdAt,
       datasets = unname(lapply(p$datasets, function(d) list(id = d$id, meta = d$meta))),
       results = arr(names(p$results)))
}

#* Rename a project. Body: {"name": "..."}
#* @tag projects
#* @patch /api/projects/<pid>
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid) {
  p <- project_of(req, pid)
  body <- parse_json_body(req)
  name <- trimws(as.character(body$name %||% ""))
  if (!nzchar(name)) api_error(400, "invalid_name", "The project name cannot be empty.")
  p$name <- substr(name, 1, 120)
  list(id = p$id, name = p$name)
}

#* Delete a project and all of its uploaded data and results
#* @tag projects
#* @delete /api/projects/<pid>
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid) {
  p <- project_of(req, pid)
  delete_project(p)
  list(deleted = TRUE)
}

#* Upload a dataset (CSV/TXT/TSV/DAT, XLSX/XLS, ODS, SAV/ZSAV/POR, DTA, SAS7BDAT/XPT, OMV, RDS/RDATA; multipart field `file`; optional field `sheet`)
#* @tag datasets
#* @post /api/projects/<pid>/datasets
#* @parser multi
#* @parser text
#* @parser octet
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, res, pid) {
  p <- project_of(req, pid)
  parts <- req$body
  f <- parts$file
  if (is.null(f) || is.null(f$value) || is.null(f$filename)) {
    api_error(400, "no_file", "No file was received.", "Choose a data file to upload.")
  }
  max_mb <- as.numeric(Sys.getenv("PATHLAB_MAX_UPLOAD_MB", "25"))
  raw <- f$value
  if (is.character(raw)) raw <- charToRaw(paste(raw, collapse = "\n"))
  if (length(raw) > max_mb * 1024^2) api_error(413, "file_too_large", sprintf("The file is larger than %s MB.", max_mb))
  sheet <- parts$sheet$value %||% parts$sheet
  if (is.raw(sheet)) sheet <- rawToChar(sheet)
  if (!is.null(sheet) && !is.character(sheet)) sheet <- NULL
  ds <- store_dataset(p, raw, f$filename, sheet)
  res$status <- 201
  dataset_payload(ds)
}

#* Load the bundled sample dataset into this project
#* @tag datasets
#* @post /api/projects/<pid>/datasets/sample
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, res, pid) {
  p <- project_of(req, pid)
  path <- file.path(SAMPLE_DIR, "pathlab_sample_technology_adoption.csv")
  ds <- store_dataset(p, readBin(path, "raw", file.info(path)$size), basename(path))
  res$status <- 201
  dataset_payload(ds)
}

#* Dataset profile, validation issues and preview
#* @tag datasets
#* @get /api/projects/<pid>/datasets/<did>
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid, did) {
  p <- project_of(req, pid)
  dataset_payload(get_dataset(p, did))
}

#* A page of dataset rows for the read-only data view (all rows, loaded as the user scrolls)
#* @tag datasets
#* @get /api/projects/<pid>/datasets/<did>/rows
#* @param offset:int Number of rows to skip (default 0)
#* @param limit:int Rows to return (default 500, at most 2000)
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid, did, offset = 0, limit = 500) {
  p <- project_of(req, pid)
  ds <- get_dataset(p, did)
  off <- suppressWarnings(as.integer(offset)); lim <- suppressWarnings(as.integer(limit))
  if (is.na(off) || off < 0) off <- 0L
  if (is.na(lim) || lim < 1) lim <- 500L
  lim <- min(lim, 2000L)
  list(offset = off, total = nrow(ds$data), rows = dataset_rows_json(ds$data, off, lim))
}

#* The uploaded file exactly as received (used to embed the data in a project file at the user's request)
#* @tag datasets
#* @get /api/projects/<pid>/datasets/<did>/file
#* @serializer octet
function(req, res, pid, did) {
  p <- project_of(req, pid)
  ds <- get_dataset(p, did)
  download(res, readBin(ds$path, "raw", file.info(ds$path)$size), ds$meta$filename, "application/octet-stream")
}

#* Re-read an uploaded workbook using a different sheet. Body: {"sheet": "Sheet2"}
#* @tag datasets
#* @post /api/projects/<pid>/datasets/<did>/sheet
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid, did) {
  p <- project_of(req, pid)
  ds <- get_dataset(p, did)
  body <- parse_json_body(req)
  ds2 <- store_dataset(p, NULL, ds$meta$filename, as.character(body$sheet %||% ""), dataset_id = did)
  dataset_payload(ds2)
}

#* Download the data-validation summary as CSV
#* @tag datasets
#* @get /api/projects/<pid>/datasets/<did>/validation.csv
function(req, res, pid, did) {
  p <- project_of(req, pid)
  ds <- get_dataset(p, did)
  download(res, validation_csv(ds$issues, ds$profile, ds$meta),
           paste0(tools::file_path_sans_ext(ds$meta$filename), "_validation.csv"), "text/csv; charset=utf-8")
}

#* Preview what a missing-data choice would change. Body: {"indicators": [...], "code": -99 | null}
#* @tag datasets
#* @post /api/projects/<pid>/datasets/<did>/missing-impact
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid, did) {
  p <- project_of(req, pid)
  ds <- get_dataset(p, did)
  body <- parse_json_body(req)
  code <- body$code
  if (!is.null(code)) code <- suppressWarnings(as.numeric(code))
  if (!is.null(code) && is.na(code)) api_error(400, "invalid_code", "The missing-value code must be a number.")
  missing_impact(ds$data, as_chr_vec(body$indicators), code)
}

#* Exploratory factor analysis / principal component analysis (psych) on chosen dataset columns.
#* Body: {"options": {variables, method, rotation, cor, nfactorsRule, nfactors, cutoff, missing, parallelIter, seed, split}, "missingCode": number | null}
#* @tag analyses
#* @post /api/projects/<pid>/datasets/<did>/efa
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid, did) {
  p <- project_of(req, pid)
  ds <- get_dataset(p, did)
  body <- parse_json_body(req)
  code <- body$missingCode
  if (!is.null(code)) code <- suppressWarnings(as.numeric(code))
  if (!is.null(code) && is.na(code)) api_error(400, "invalid_code", "The missing-value code must be a number.")
  store_result(p, "efa", run_efa(ds$data, normalize_efa(body$options), code), did)
}

#* Regression (linear, logistic, Poisson) with hierarchical blocks on chosen dataset columns.
#* Body: {"spec": {outcome, blocks: [{predictors, interactions: [{predictor, moderator}]}], family, robust, center, ciLevel}, "missingCode": number | null}
#* @tag analyses
#* @post /api/projects/<pid>/datasets/<did>/regression
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid, did) {
  p <- project_of(req, pid)
  ds <- get_dataset(p, did)
  body <- parse_json_body(req)
  code <- body$missingCode
  if (!is.null(code)) code <- suppressWarnings(as.numeric(code))
  if (!is.null(code) && is.na(code)) api_error(400, "invalid_code", "The missing-value code must be a number.")
  store_result(p, "regression", run_regression(ds$data, normalize_regression(body$spec), code), did)
}

#* Qualitative comparative analysis (QCA package): calibration, necessity, truth table, complex / parsimonious / intermediate solutions.
#* Body: {"spec": {outcome, negateOutcome, conditions, calibration: {<variable>: {method, anchors, thresholds}}, avoidHalf, inclCut, priCut, nCut, necessityCut, expectations}, "missingCode"}
#* @tag analyses
#* Runs as a background job (202 + job; poll /jobs/<jid>, then fetch /results/<rid>). A new run cancels the previous one.
#* @post /api/projects/<pid>/datasets/<did>/qca
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, res, pid, did) {
  p <- project_of(req, pid)
  ds <- get_dataset(p, did)
  body <- parse_json_body(req)
  code <- body$missingCode
  if (!is.null(code)) code <- suppressWarnings(as.numeric(code))
  if (!is.null(code) && is.na(code)) api_error(400, "invalid_code", "The missing-value code must be a number.")
  o <- normalize_qca(body$spec)
  issues <- validate_qca(ds$data, o)
  if (any(vapply(issues, function(i) i$severity == "error", TRUE))) api_error(422, "invalid_qca", "The QCA cannot run until the listed problems are fixed.", details = issues)
  if (!requireNamespace("QCA", quietly = TRUE)) api_error(501, "qca_unavailable", "The QCA R package is not installed on the analysis server.", "Run Rscript backend/install_packages.R.")
  cancel_qca_jobs(p)
  job <- start_analysis_job(p, ds, list(), list(bootstrap = list(enabled = FALSE)), "{}", kind = "qca", extra = list(qca = o, missingCode = code), exclusive = FALSE)
  res$status <- 202
  job_payload(job)
}

#* QCA calibration preview: each set's anchors in the data's units, its range, and crisp category counts.
#* Body: {"spec": QCA spec, "missingCode"}
#* @tag analyses
#* @post /api/projects/<pid>/datasets/<did>/qca/anchors
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid, did) {
  p <- project_of(req, pid)
  ds <- get_dataset(p, did)
  body <- parse_json_body(req)
  code <- body$missingCode
  if (!is.null(code)) code <- suppressWarnings(as.numeric(code))
  qca_anchor_preview(ds$data, normalize_qca(body$spec), if (is.null(code) || is.na(code)) NULL else code)
}

#* QCA with more than 7 conditions: suggest the k best (default 7), with a reason for every condition (see qca_select_conditions).
#* Body: {"spec": QCA spec, "k": 7, "missingCode"}
#* @tag analyses
#* @post /api/projects/<pid>/datasets/<did>/qca/select-conditions
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid, did) {
  p <- project_of(req, pid)
  ds <- get_dataset(p, did)
  body <- parse_json_body(req)
  code <- body$missingCode
  if (!is.null(code)) code <- suppressWarnings(as.numeric(code))
  k <- suppressWarnings(as.integer(body$k %||% 7L))
  if (is.na(k) || k < 1 || k > 12) api_error(400, "invalid_k", "k must be between 1 and 12.")
  if (!requireNamespace("QCA", quietly = TRUE)) api_error(501, "qca_unavailable", "The QCA R package is not installed on the analysis server.")
  qca_select_conditions(ds$data, normalize_qca(body$spec), k, if (is.null(code) || is.na(code)) NULL else code)
}

#* Data screening: cases with straight-lining, many missing answers, out-of-range answers, multivariate outliers
#* (Mahalanobis) or influential cases (Cook's distance). Nothing is removed. Body: {"items", "sdCut", "missingCut",
#* "mahaP", "cookRule", "regressions": [{"outcome", "predictors"}], "constructs", "paths", "idColumn", "missingCode"}
#* @tag datasets
#* @post /api/projects/<pid>/datasets/<did>/screen
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid, did) {
  p <- project_of(req, pid)
  ds <- get_dataset(p, did)
  run_screening(ds$data, parse_json_body(req))
}

#* Attribute explorer (WEKA's Preprocess panel): per attribute its type, missing / distinct / unique counts, statistics
#* and histogram bins split by the class attribute. Body: {"class", "bins", "missingCode"}
#* @tag datasets
#* @post /api/projects/<pid>/datasets/<did>/explore
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid, did) {
  p <- project_of(req, pid)
  ds <- dataset_with_derived(get_dataset(p, did))
  run_explore(ds$data, parse_json_body(req), ds$declared)
}

#* Visualize (WEKA's scatter plot matrix): the values of the numeric and nominal attributes, and Pearson correlations.
#* Body: {"missingCode", "maxRows", "seed"}
#* @tag datasets
#* @post /api/projects/<pid>/datasets/<did>/visualize
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid, did) {
  p <- project_of(req, pid)
  ds <- dataset_with_derived(get_dataset(p, did))
  run_visualize_data(ds$data, parse_json_body(req), ds$declared)
}

#* Save the cases flagged in data screening as a 0/1 variable. Body: {"rows": [row, ...], "name"}
#* @tag datasets
#* @post /api/projects/<pid>/datasets/<did>/screen/flag
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid, did) {
  p <- project_of(req, pid)
  b <- parse_json_body(req)
  dataset_payload(screen_flag_variable(p, did, b$rows, b$name))
}

#* Descriptive / inferential statistics with jmv (jamovi's engine). Body: {"proc", "options" (jmv argument names), "measures": {var: nominal/ordinal/continuous}, "missingCode"}
#* @tag analyses
#* @post /api/projects/<pid>/datasets/<did>/stats
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid, did) {
  p <- project_of(req, pid)
  ds <- get_dataset(p, did)
  body <- parse_json_body(req)
  code <- body$missingCode
  if (!is.null(code)) code <- suppressWarnings(as.numeric(code))
  if (!is.null(code) && is.na(code)) api_error(400, "invalid_code", "The missing-value code must be a number.")
  fdf <- stats_prefilter(ds$data, body$filters, body$weights)
  sreq <- normalize_stats(body)
  res <- run_stats(fdf, sreq, code)
  # EFA / PCA: factor or component scores that can be saved as variables (Save scores under the result).
  if (sreq$proc %in% c("efa", "pca")) res$.derived <- tryCatch(stats_factor_scores(fdf, sreq, res, code), error = function(e) NULL)
  # Active row filters are not noted under each table (the Filters panel and the sheet show them); weights are.
  w <- as.character(body$weights %||% "")[1]
  if (!is.na(w) && nzchar(w)) res$warnings <- c(res$warnings, list(sprintf("Weighted by %s (frequency weights).", w)))
  store_result(p, "stats", res, did)
}

#* Export the dataset for jamovi (.omv), JASP / numiqo / SPSS (.sav) or as CSV, with the measurement levels set in the app.
#* Body: {"format": "omv" | "sav" | "csv", "measures": {var: level}, "includeDerived": bool}
#* @tag datasets
#* @post /api/projects/<pid>/datasets/<did>/export
function(req, res, pid, did) {
  p <- project_of(req, pid)
  body <- parse_json_body(req)
  fmt <- as.character(body$format %||% "omv")[1]
  ms <- lapply(body$measures %||% list(), function(m) as.character(m)[1])
  raw <- p$datasets[[did]]
  if (!is.null(raw)) raw$data <- apply_edits(raw$data, raw$edits)
  path <- export_dataset(get_dataset(p, did), raw, fmt, ms, isTRUE(body$includeDerived), file.path(p$dir, "exports"))
  download(res, readBin(path, "raw", file.info(path)$size), basename(path), switch(fmt, omv = "application/octet-stream", sav = "application/x-spss-sav", "text/csv"))
}

#* Start a CB-SEM job (lavaan). Body: {"datasetId", "model" (constructs, paths, covariances), "cbsem": {estimator, missing, ordinal, bootstrap, ciLevel}, "missingCode"}
#* @tag analyses
#* @post /api/projects/<pid>/cbsem
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, res, pid) {
  p <- project_of(req, pid)
  body <- parse_json_body(req)
  ds <- get_dataset(p, as.character(body$datasetId %||% ""))
  model <- normalize_cb_model(body$model)
  cb <- normalize_cb_settings(body$cbsem)
  issues <- validate_cbsem(model, ds$data)
  if (any(vapply(issues, function(i) i$severity == "error", TRUE)))
    api_error(422, "invalid_model", "The CB-SEM model cannot be estimated until the listed problems are fixed.", details = issues)
  code <- body$missingCode
  if (!is.null(code)) code <- suppressWarnings(as.numeric(code))
  if (!is.null(code) && is.na(code)) code <- NULL
  model_json <- as.character(jsonlite::toJSON(body$model, auto_unbox = TRUE, null = "null", digits = NA))
  job <- start_analysis_job(p, ds, model, list(bootstrap = list(enabled = cb$bootstrap$enabled, nboot = cb$bootstrap$nboot, cores = 1L)), model_json,
                            kind = "cbsem", extra = list(cb = cb, missingCode = code))
  res$status <- 202
  job_payload(job)
}

#* Start a GSCA job (cSEM). Body: {"datasetId", "model" (constructs, paths, moderations), "gsca": {bootstrap, ciLevel}, "missingCode"}
#* @tag analyses
#* @post /api/projects/<pid>/gsca
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, res, pid) {
  p <- project_of(req, pid)
  body <- parse_json_body(req)
  ds <- get_dataset(p, as.character(body$datasetId %||% ""))
  model <- normalize_model(body$model)
  gsca_need()
  gs <- normalize_gsca_settings(body$gsca)
  issues <- validate_gsca(model, dataset_with_derived(ds))
  if (any(vapply(issues, function(i) i$severity == "error", TRUE)))
    api_error(422, "invalid_model", "The GSCA model cannot be estimated until the listed problems are fixed.", details = issues)
  code <- body$missingCode
  if (!is.null(code)) code <- suppressWarnings(as.numeric(code))
  if (!is.null(code) && is.na(code)) code <- NULL
  model_json <- as.character(jsonlite::toJSON(body$model, auto_unbox = TRUE, null = "null", digits = NA))
  job <- start_analysis_job(p, dataset_with_derived(ds), model, list(bootstrap = list(enabled = gs$bootstrap$enabled, nboot = gs$bootstrap$nboot, cores = 1L)), model_json,
                            kind = "gsca", extra = list(gsca = gs, missingCode = code))
  res$status <- 202
  job_payload(job)
}

#* Measurement invariance of the CB-SEM measurement model. Body: {"datasetId", "model", "cbsem", "invariance": {groupVariable, groups, strict}, "missingCode"}
#* @tag analyses
#* @post /api/projects/<pid>/cbsem/invariance
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid) {
  p <- project_of(req, pid)
  body <- parse_json_body(req)
  did <- as.character(body$datasetId %||% "")
  ds <- get_dataset(p, did)
  code <- body$missingCode
  if (!is.null(code)) code <- suppressWarnings(as.numeric(code))
  if (!is.null(code) && is.na(code)) code <- NULL
  store_result(p, "invariance", run_invariance(ds$data, normalize_cb_model(body$model), normalize_cb_settings(body$cbsem), normalize_invariance(body$invariance), code), did)
}

#* Save values an analysis produced (scores, profile or segment membership) as derived variables of the dataset.
#* Body: {"resultId", "set", "columns": [{"from", "name", "label"}], "source": {"analysisId", "analysisName", "kind"}}
#* @tag datasets
#* @post /api/projects/<pid>/datasets/<did>/derived
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid, did) {
  p <- project_of(req, pid)
  dataset_payload(add_derived_variables(p, did, parse_json_body(req)))
}

#* Add or replace a computed variable (jamovi's Data > Compute), e.g. MEAN(PE1, PE2, PE3). The formula is checked
#* against a whitelist of functions and the dataset's variables before it is evaluated. Body: {"name", "formula", "description"}
#* @tag datasets
#* @post /api/projects/<pid>/datasets/<did>/computed
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid, did) {
  p <- project_of(req, pid)
  b <- parse_json_body(req)
  dataset_payload(add_computed_variable(p, did, b$name, b$formula, b$description, b$kind %||% "computed"))
}

#* Add or replace several computed variables and filters in one go; one may use another whatever their order.
#* Body: {"variables": [{"name", "formula", "description", "kind"}]}. Returns the dataset plus "failed": [{"name", "message"}].
#* @tag datasets
#* @post /api/projects/<pid>/datasets/<did>/computed/batch
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid, did) {
  p <- project_of(req, pid)
  b <- parse_json_body(req)
  r <- add_computed_variables(p, did, b$variables %||% list())
  c(dataset_payload(r$ds), list(failed = r$failed))
}

#* Functions available in computed-variable formulas (name, group, description)
#* @tag datasets
#* @get /api/compute/functions
#* @serializer unboxedJSON
function() compute_function_list()

#* Added data variables, appended rows and deleted rows (jamovi's Data > Add / Delete). The upload is never changed; the
#* changes are replayed on a copy. Body: {"addColumns": [{"name", "type": "numeric" | "text"}], "addRows": n, "deleteRows": [row, ...]}
#* @tag datasets
#* @put /api/projects/<pid>/datasets/<did>/structure
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid, did) {
  p <- project_of(req, pid)
  dataset_payload(set_dataset_structure(p, did, parse_json_body(req)))
}

#* Replace the set of cell edits (jamovi-style spreadsheet editing). The uploaded file is never changed; edits are applied
#* when the data are read, and computed variables are recomputed. Body: {"edits": [{"row", "column", "value"}]} (value null = missing)
#* @tag datasets
#* @put /api/projects/<pid>/datasets/<did>/edits
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid, did) {
  p <- project_of(req, pid)
  dataset_payload(set_dataset_edits(p, did, parse_json_body(req)$edits))
}

#* Remove a derived variable (uploaded columns cannot be removed)
#* @tag datasets
#* @delete /api/projects/<pid>/datasets/<did>/derived/<name>
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid, did, name) {
  p <- project_of(req, pid)
  dataset_payload(remove_derived_variable(p, did, utils::URLdecode(name)))
}

#* Start a machine-learning job (caret), or a SEM-ANN job when the body has "semann": {target, features, hidden, decay, networks, seed, plsPaths}. Body: {"datasetId", "ml": {target, features, task, positive, learners, sampling: {method, folds, repeats, testShare}, tuneLength, seed}, "missingCode"}
#* @tag analyses
#* @post /api/projects/<pid>/ml
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, res, pid) {
  p <- project_of(req, pid)
  body <- parse_json_body(req)
  ds <- get_dataset(p, as.character(body$datasetId %||% ""))
  code <- body$missingCode
  if (!is.null(code)) code <- suppressWarnings(as.numeric(code))
  if (!is.null(code) && is.na(code)) code <- NULL
  if (!is.null(body$semann)) {
    job <- start_analysis_job(p, ds, list(), list(bootstrap = list(enabled = FALSE)), "{}", kind = "ml", extra = list(semann = normalize_semann(body$semann), missingCode = code))
    res$status <- 202
    return(job_payload(job))
  }
  o <- normalize_ml(body$ml)
  issues <- validate_ml(ds$data, o)
  if (any(vapply(issues, function(i) i$severity == "error", TRUE)))
    api_error(422, "invalid_ml", "The machine-learning analysis cannot run until the listed problems are fixed.", details = issues)
  code <- body$missingCode
  if (!is.null(code)) code <- suppressWarnings(as.numeric(code))
  if (!is.null(code) && is.na(code)) code <- NULL
  # Supplied test set: a file uploaded to this project (it does not replace the dataset), sent to the job with its name.
  test <- NULL
  if (o$sampling$method == "supplied") {
    tid <- as.character(body$testDatasetId %||% "")
    tds <- if (nzchar(tid)) p$datasets[[tid]] else NULL
    if (is.null(tds)) api_error(404, "test_dataset_not_found", "The test file is no longer on the analysis server.", "Choose the test file again under Test options.")
    tds <- dataset_with_derived(tds)
    miss <- setdiff(c(o$target, o$features), names(tds$data))
    if (length(miss)) api_error(422, "invalid_ml", sprintf("The test file has no column %s.", brief_list(sprintf("'%s'", miss))),
                                "Use a test file with the same variables (column names) as the training data.")
    test <- list(data = tds$data, declared = tds$declared, filename = tds$meta$filename)
  }
  job <- start_analysis_job(p, ds, list(), list(bootstrap = list(enabled = FALSE)), "{}", kind = "ml", extra = list(ml = o, missingCode = code, declared = ds$declared, test = test))
  res$status <- 202
  job_payload(job)
}

#* Start a latent profile analysis job (tidyLPA / mclust). Body: {"datasetId", "lpa": {variables, auxiliary, models, kMin, kMax, nboot, standardize, seed}, "missingCode"}
#* @tag analyses
#* @post /api/projects/<pid>/lpa
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, res, pid) {
  p <- project_of(req, pid)
  body <- parse_json_body(req)
  ds <- get_dataset(p, as.character(body$datasetId %||% ""))
  o <- normalize_lpa(body$lpa)
  issues <- validate_lpa(ds$data, o)
  if (any(vapply(issues, function(i) i$severity == "error", TRUE)))
    api_error(422, "invalid_lpa", "The latent profile analysis cannot run until the listed problems are fixed.", details = issues)
  code <- body$missingCode
  if (!is.null(code)) code <- suppressWarnings(as.numeric(code))
  if (!is.null(code) && is.na(code)) code <- NULL
  job <- start_analysis_job(p, ds, list(), list(bootstrap = list(enabled = FALSE)), "{}", kind = "lpa", extra = list(lpa = o, missingCode = code))
  res$status <- 202
  job_payload(job)
}

#* Choose the solution of a stored latent profile analysis to interpret (reports, tables, membership variable). Body: {"model", "classes"}
#* @tag results
#* @post /api/projects/<pid>/results/<rid>/lpa-selection
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid, rid) {
  p <- project_of(req, pid)
  body <- parse_json_body(req)
  set_lpa_selection(p, rid, body$model %||% NA, body$classes %||% NA)
}

#* FIMIX-PLS segmentation of the PLS-SEM model (SEMinR construct scores + flexmix mixture regression).
#* Body: {"datasetId", "model", "settings", "roles", "fimix": {kMax, nrep, iterMax, seed, minPrior}}
#* @tag analyses
#* @post /api/projects/<pid>/fimix
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid) {
  p <- project_of(req, pid)
  mr <- model_request(req, p)
  v <- validate_model(mr$model, mr$settings, mr$dsv)
  if (!v$valid) api_error(422, "invalid_model", "The model cannot be estimated until the listed problems are fixed.", details = v$issues)
  store_result(p, "fimix", run_fimix(mr$ds$data, mr$model, mr$settings, normalize_fimix(mr$body$fimix)), as.character(mr$body$datasetId %||% ""))
}

#* Validate a model specification and return the generated SEMinR code.
#* Body: {"datasetId" (optional: omit for structure-only checks), "model", "settings", "roles"}
#* @tag models
#* @post /api/projects/<pid>/validate
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid) {
  p <- project_of(req, pid)
  mr <- model_request(req, p, require_dataset = FALSE)
  v <- validate_model(mr$model, mr$settings, mr$dsv)
  code <- tryCatch(generate_r_code(mr$model, mr$settings, mr$ds$meta), error = function(e) NULL)
  list(valid = v$valid, issues = v$issues, rCode = code)
}

#* Start an analysis job (estimation, optional bootstrap and PLSpredict)
#* Body: {"datasetId", "model", "settings", "roles"}. Returns 202 with the job.
#* @tag analyses
#* @post /api/projects/<pid>/analyses
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, res, pid) {
  p <- project_of(req, pid)
  mr <- model_request(req, p)
  v <- validate_model(mr$model, mr$settings, mr$dsv)
  if (!v$valid) {
    api_error(422, "invalid_model", "The model cannot be estimated until the listed problems are fixed.",
              "Open the Diagnostics tab or the model checklist to see each problem.", details = v$issues)
  }
  job <- start_analysis_job(p, mr$ds, mr$model, mr$settings, mr$model_json)
  res$status <- 202
  job_payload(job)
}

#* Poll an analysis job for progress and completion
#* @tag analyses
#* @get /api/projects/<pid>/jobs/<jid>
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid, jid) {
  p <- project_of(req, pid)
  job_payload(refresh_job(p, jid))
}

#* Cancel a running analysis job
#* @tag analyses
#* @delete /api/projects/<pid>/jobs/<jid>
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid, jid) {
  p <- project_of(req, pid)
  job_payload(cancel_job(p, jid))
}

#* Full analysis result (JSON, full numeric precision)
#* @tag results
#* @get /api/projects/<pid>/results/<rid>
function(req, res, pid, rid) {
  p <- project_of(req, pid)
  json_raw(res, read_result_json(p, rid))
}

#* Start rendering publication-ready (APA 7) tables with Quarto. Body: {"format": "docx"|"html", "decimals": 2|3,
#* "pStyle": "stars"|"exact", "leadingZero": "apa"|"always", "font": "...", "tables": [...], "pageBreaks": true,
#* "hypotheses": [...], "decisionRule": {...}}
#* @tag results
#* @post /api/projects/<pid>/results/<rid>/tables
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, res, pid, rid) {
  p <- project_of(req, pid)
  rj <- read_result_json(p, rid)
  opts <- normalize_pub_options(parse_json_body(req))
  res$status <- 202
  start_pub_tables(p, rj, opts, p$name)
}

#* Status of a publication-table rendering
#* @tag results
#* @get /api/projects/<pid>/tables/<tid>
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid, tid) {
  pub_tables_status(project_of(req, pid), tid)
}

#* The rendered publication tables (.docx or .html)
#* @tag results
#* @get /api/projects/<pid>/tables/<tid>/file
#* @serializer octet
function(req, res, pid, tid) {
  p <- project_of(req, pid)
  s <- pub_tables_status(p, tid)
  if (!identical(s$status, "completed")) api_error(409, "not_ready", "The tables are not ready yet.")
  job <- p$pubjobs[[tid]]
  path <- file.path(job$dir, "out", paste0("tables.", job$format))
  type <- if (job$format == "html") "text/html; charset=utf-8" else "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  download(res, readBin(path, "raw", file.info(path)$size), paste0(p$name, "_tables.", job$format), type)
}

#* Result tables as an XLSX workbook (full precision)
#* @tag results
#* @get /api/projects/<pid>/results/<rid>/xlsx
function(req, res, pid, rid) {
  p <- project_of(req, pid)
  r <- read_result(p, rid)
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path))
  write_results_xlsx(r, p$name, path)
  download(res, readBin(path, "raw", file.info(path)$size), paste0(p$name, "_results.xlsx"),
           "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
}

#* Reproducible R script for a completed analysis
#* @tag results
#* @get /api/projects/<pid>/results/<rid>/code
function(req, res, pid, rid) {
  p <- project_of(req, pid)
  r <- read_result(p, rid)
  download(res, r$rCode, paste0(p$name, switch(result_kind(r), efa = "_efa.R", regression = "_regression.R", fimix = "_fimix.R", lpa = "_lpa.R", qca = "_qca.R", ml = "_ml.R", stats = "_jmv.R", cbsem = "_cbsem.R", gsca = "_gsca.R", invariance = "_invariance.R", "_seminr.R")), "text/x-r; charset=utf-8")
}

#* Import bibliographic export files (multipart field `file`, repeatable; optional field `database` = wos, scopus,
#* dimensions, pubmed, lens, openalex, cochrane; detected from the file when omitted). Several files are merged and
#* duplicates removed. Field `append` = "true" adds the files to the current collection.
#* @tag bibliometrics
#* @post /api/projects/<pid>/biblio/import
#* @parser multi
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, res, pid) {
  p <- project_of(req, pid)
  parts <- req$body
  field <- function(k) { v <- parts[[k]]$value %||% parts[[k]]; if (is.raw(v)) v <- rawToChar(v); if (is.character(v) && length(v) && nzchar(v[1])) v[1] else NULL }
  files <- Filter(function(x) is.list(x) && !is.null(x$filename), parts)
  if (!length(files)) api_error(400, "no_file", "No file was received.")
  max_mb <- as.numeric(Sys.getenv("PATHLAB_MAX_UPLOAD_MB", "25"))
  read <- lapply(files, function(f) {
    raw <- f$value; if (is.character(raw)) raw <- charToRaw(paste(raw, collapse = "\n"))
    if (length(raw) > max_mb * 1024^2) api_error(413, "file_too_large", sprintf("%s is larger than %s MB.", f$filename, max_mb))
    biblio_read(raw, f$filename, field("database"))
  })
  parts_M <- lapply(read, `[[`, "M")
  info_files <- lapply(seq_along(read), function(i) list(name = files[[i]]$filename, database = unname(BIBLIO_DBS[[read[[i]]$db]]), format = read[[i]]$format, records = nrow(read[[i]]$M)))
  if (identical(field("append"), "true") && !is.null(p$biblio$M)) { parts_M <- c(list(p$biblio$M), parts_M); info_files <- c(p$biblio$files, info_files) }
  res$status <- 201
  biblio_set(p, biblio_merge(parts_M), info_files)
}

#* Load the sample collection (bibliometric approaches in business and management; 898 Web of Science records)
#* @tag bibliometrics
#* @post /api/projects/<pid>/biblio/sample
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid) {
  p <- project_of(req, pid)
  biblio_set(p, biblio_sample(), list(list(name = "Sample: bibliometrics in business and management (bibliometrixData)", database = "Web of Science", format = "plaintext", records = 898)))
}

#* Search OpenAlex or PubMed and load the records. Body: {"source": "openalex" | "pubmed", "query", "limit" (10-5000),
#* "yearFrom", "yearTo", "email" (optional, OpenAlex polite pool), "append": true to add them to the collection}
#* @tag bibliometrics
#* @post /api/projects/<pid>/biblio/search
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, res, pid) {
  p <- project_of(req, pid)
  b <- req$body %||% list()
  yr <- function(x) { v <- suppressWarnings(as.integer(x)); if (length(v) && !is.na(v[1])) v[1] else NULL }
  r <- biblio_search(if (identical(b$source, "pubmed")) "pubmed" else "openalex", b$query, b$limit %||% 500, yr(b$yearFrom), yr(b$yearTo), as.character(b$email %||% "")[1])
  parts_M <- list(r$M); info_files <- list(r$file)
  if (isTRUE(b$append) && !is.null(p$biblio$M)) { parts_M <- c(list(p$biblio$M), parts_M); info_files <- c(p$biblio$files, info_files) }
  res$status <- 201
  biblio_set(p, biblio_merge(parts_M), info_files)
}

#* The loaded collection (summary for the Data view)
#* @tag bibliometrics
#* @get /api/projects/<pid>/biblio
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid) biblio_info(project_of(req, pid))

#* Remove the collection from the session
#* @tag bibliometrics
#* @delete /api/projects/<pid>/biblio
#* @serializer unboxedJSON
function(req, res, pid) { p <- project_of(req, pid); p$biblio <- NULL; res$status <- 204; list() }

#* Thesaurus (VOSviewer format). Body: {"thesaurus": [{label, replaceBy}]}
#* @tag bibliometrics
#* @put /api/projects/<pid>/biblio/thesaurus
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid) {
  p <- project_of(req, pid)
  b <- biblio_get(p)
  b$thesaurus <- normalize_thesaurus(parse_json_body(req)$thesaurus)
  p$biblio <- b
  biblio_info(p)
}

#* A Biblioshiny-style view. Body: {"view": overview|sources|authors|documents|words|thematicMap|thematicEvolution|factorial|historiograph|collaboration, "options", "filters"}
#* @tag bibliometrics
#* @post /api/projects/<pid>/biblio/view
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid) {
  p <- project_of(req, pid)
  b <- parse_json_body(req)
  biblio_view(p, as.character(b$view %||% ""), b$options, b$filters)
}

#* A VOSviewer-style network map. Body: {"options": {type, unit, counting, normalization, minOccurrences, maxItems, resolution, minClusterSize, seed}, "filters"}
#* @tag bibliometrics
#* @post /api/projects/<pid>/biblio/network
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid) {
  p <- project_of(req, pid)
  b <- parse_json_body(req)
  biblio_network(p, b$options, b$filters)
}

#* World map outlines (maps package) for the country maps
#* @tag bibliometrics
#* @get /api/biblio/world
#* @serializer unboxedJSON list(digits = 4)
function() biblio_world()

#* The filtered collection as an Excel file that Biblioshiny can load. Body: {"filters"}
#* @tag bibliometrics
#* @post /api/projects/<pid>/biblio/export
function(req, res, pid) {
  p <- project_of(req, pid)
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path))
  biblio_export_xlsx(p, parse_json_body(req)$filters, path)
  download(res, readBin(path, "raw", file.info(path)$size), paste0(p$name, "_bibliometrix.xlsx"),
           "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
}

#* PDF of the help center's ebook, from the paginated HTML the browser built. Body: {"html", "filename"}
#* @tag help
#* @post /api/projects/<pid>/help-book.pdf
function(req, res, pid) {
  project_of(req, pid)
  b <- parse_json_body(req)
  pdf <- help_book_pdf(as.character(b$html %||% ""))
  download(res, pdf, as.character(b$filename %||% "PathLab_SEM_guide.pdf"), "application/pdf")
}

#* Bibliometric report of the filtered collection. Body: {"filters", "sections": [...], "figures": [{"png", "caption", "section"}], "format": "html" | "docx"}
#* @tag bibliometrics
#* @post /api/projects/<pid>/biblio/report
function(req, res, pid) {
  p <- project_of(req, pid)
  b <- parse_json_body(req)
  html <- biblio_report_html(p, b$filters, b$sections %||% names(BIBLIO_REPORT_SECTIONS), p$name, figures = b$figures %||% list())
  if (identical(b$format, "docx")) {
    path <- stats_report_html_docx(html, tempfile(fileext = ".docx"))
    return(download(res, readBin(path, "raw", file.info(path)$size), paste0(p$name, "_bibliometric_report.docx"),
                    "application/vnd.openxmlformats-officedocument.wordprocessingml.document"))
  }
  res$setHeader("Content-Type", "text/html; charset=utf-8")
  res$body <- html
  res
}

#* Import text documents (multipart field `file`, repeatable): .txt/.md, .pdf, .docx (one document per file) or
#* .csv/.xlsx (one document per row of `textColumn`; optional `idColumn`, `attributes` = comma-separated columns, `sheet`).
#* Without `textColumn` a table returns {needsColumn: {filename, rows, columns}} so the user can choose.
#* @tag text
#* @post /api/projects/<pid>/documents
#* @parser multi
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, res, pid) {
  p <- project_of(req, pid)
  parts <- req$body
  field <- function(k) { v <- parts[[k]]$value %||% parts[[k]]; if (is.raw(v)) v <- rawToChar(v); if (is.character(v) && length(v) && nzchar(v[1])) v[1] else NULL }
  files <- Filter(function(x) is.list(x) && !is.null(x$filename), parts)
  if (!length(files)) api_error(400, "no_file", "No file was received.")
  out <- list(documents = list())
  for (f in files) {
    raw <- f$value; if (is.character(raw)) raw <- charToRaw(paste(raw, collapse = "\n"))
    max_mb <- as.numeric(Sys.getenv(if (media_is(f$filename)) "PATHLAB_MAX_MEDIA_MB" else "PATHLAB_MAX_UPLOAD_MB", if (media_is(f$filename)) "500" else "25"))
    if (length(raw) > max_mb * 1024^2) api_error(413, "file_too_large", sprintf("%s is larger than %s MB.", f$filename, max_mb))
    if (media_is(f$filename)) { out$documents <- c(out$documents, list(text_meta(text_import_media(p, raw, f$filename)))); next }   # audio / video: transcribed later
    if (visual_is(f$filename)) { out$documents <- c(out$documents, list(text_meta(text_import_image(p, raw, f$filename)))); next }   # images: coded as regions
    cols <- if (!is.null(field("textColumns"))) as_chr_vec(jsonlite::fromJSON(field("textColumns"))) else field("textColumn")   # several: a survey, one document per respondent
    r <- text_import_upload(p, raw, f$filename, cols, field("idColumn"), as_chr_vec(strsplit(field("attributes") %||% "", ",")[[1]]), field("sheet"))
    if (!is.null(r$needsColumn)) return(r)
    out$documents <- c(out$documents, r$documents)
  }
  res$status <- 201
  out
}

#* Documents from a text column of the project dataset (one per respondent, linked to the row). Body: {"datasetId", "textColumn", "idColumn", "attributes": []}
#* @tag text
#* @post /api/projects/<pid>/documents/from-dataset
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid) {
  p <- project_of(req, pid)
  b <- parse_json_body(req)
  ds <- get_dataset(p, as.character(b$datasetId %||% ""))
  cols <- if (length(b$textColumns)) as_chr_vec(b$textColumns) else as.character(b$textColumn %||% "")
  docs <- text_from_table(p, ds$data, ds$meta$filename, cols, b$idColumn, as_chr_vec(b$attributes), dataset = list(id = ds$id))
  list(documents = lapply(docs, text_meta))
}

#* A web page as a document (its main text, address and title). Body: {"url"}
#* @tag text
#* @post /api/projects/<pid>/documents/from-url
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, res, pid) {
  r <- text_import_url(project_of(req, pid), parse_json_body(req)$url)
  res$status <- 201
  list(documents = list(r))
}

#* Restore documents embedded in a project file (with their ids). Body: {"documents": [{id, name, source, kind, text, attributes, datasetRow}], "datasetId"}
#* @tag text
#* @post /api/projects/<pid>/documents/restore
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid) {
  p <- project_of(req, pid)
  b <- parse_json_body(req)
  list(documents = text_restore(p, b$documents, b$datasetId))
}

#* Documents of this project (metadata)
#* @tag text
#* @get /api/projects/<pid>/documents
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid) list(documents = text_list(project_of(req, pid)))

#* One document with its text
#* @tag text
#* @get /api/projects/<pid>/documents/<tid>
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid, tid) {
  p <- project_of(req, pid)
  d <- p$documents[[tid]]; if (is.null(d)) api_error(404, "text_doc_missing", "The document is not in this session.")
  c(text_meta(d), list(text = d$text, segments = d$segments %||% list()))
}

#* One page of an image or PDF document as an image (for region coding)
#* @tag text
#* @get /api/projects/<pid>/documents/<tid>/page/<n>
function(req, res, pid, tid, n) {
  pg <- visual_page(project_of(req, pid), tid, n)
  res$setHeader("Content-Type", pg$mime)
  res$body <- readBin(pg$path, "raw", file.info(pg$path)$size)
  res
}

#* The audio or video file of a recording (for the player)
#* @tag text
#* @get /api/projects/<pid>/documents/<tid>/media
function(req, res, pid, tid) {
  p <- project_of(req, pid)
  d <- p$documents[[tid]]
  if (is.null(d) || is.null(d$media$file) || !file.exists(d$media$file)) api_error(404, "media_missing", "The recording is not in this session.", "Import the audio or video file again.")
  res$setHeader("Content-Type", d$media$mime %||% "application/octet-stream")
  res$body <- readBin(d$media$file, "raw", file.info(d$media$file)$size)
  res
}

#* Save a corrected transcript (text and speakers per segment; timestamps kept). Body: {"segments": [{start, end, text, speaker}]}
#* @tag text
#* @put /api/projects/<pid>/documents/<tid>/transcript
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid, tid) transcript_save(project_of(req, pid), tid, parse_json_body(req)$segments %||% list())

#* Start transcribing a recording in the background. Body: {"engine": "local" | "openai", "model", "language", "translate", "consent"}
#* @tag text
#* @post /api/projects/<pid>/documents/<tid>/transcribe
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid, tid) transcribe_start(project_of(req, pid), tid, parse_json_body(req))

#* What can transcribe on this server: whisper.cpp (and its downloaded models), OpenAI
#* @tag text
#* @get /api/projects/<pid>/text/transcription
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid) transcribe_status(project_of(req, pid))

#* Set where whisper.cpp (whisper-cli) is installed. Body: {"path"}; an empty path clears it
#* @tag text
#* @post /api/projects/<pid>/text/whisper-cli
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid) whisper_set_cli(project_of(req, pid), parse_json_body(req)$path)

#* Download a Whisper model for whisper.cpp in the background (once). Body: {"model": "base"}
#* @tag text
#* @post /api/projects/<pid>/text/whisper-model
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid) whisper_model_start(project_of(req, pid), parse_json_body(req)$model)

#* The optional R packages the app can install, and which are already there
#* @tag setup
#* @get /api/packages/optional
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function() pkg_optional_status()

#* Install an optional R package in the background, for a feature that needs one. Body: {"packages": ["bibliometrix"]}
#* Only the packages of PKG_OPTIONAL are installed, only when absent, and nothing already installed is touched.
#* @tag setup
#* @post /api/projects/<pid>/packages/install
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, res, pid) {
  r <- pkg_install_start(project_of(req, pid), parse_json_body(req)$packages)
  if (identical(r$status, "running")) res$status <- 202
  r
}

#* Progress of an install started from the app
#* @tag setup
#* @get /api/projects/<pid>/package-jobs/<jid>
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid, jid) pkg_job_status(project_of(req, pid), jid)

#* Cancel an install started from the app
#* @tag setup
#* @delete /api/projects/<pid>/package-jobs/<jid>
#* @serializer unboxedJSON
function(req, pid, jid) pkg_job_cancel(project_of(req, pid), jid)

#* Progress of a transcription or model download
#* @tag text
#* @get /api/projects/<pid>/media-jobs/<jid>
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid, jid) media_job_status(project_of(req, pid), jid)

#* Cancel a transcription or model download
#* @tag text
#* @delete /api/projects/<pid>/media-jobs/<jid>
#* @serializer unboxedJSON
function(req, pid, jid) media_job_cancel(project_of(req, pid), jid)

#* Rename a document or set its attributes. Body: {"name", "attributes": {...}}
#* @tag text
#* @patch /api/projects/<pid>/documents/<tid>
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid, tid) {
  p <- project_of(req, pid)
  d <- p$documents[[tid]]; if (is.null(d)) api_error(404, "text_doc_missing", "The document is not in this session.")
  b <- parse_json_body(req)
  if (!is.null(b$name) && nzchar(trimws(b$name))) d$name <- substr(trimws(as.character(b$name)), 1, 200)
  if (!is.null(b$attributes)) {
    at <- b$attributes; if (length(at) > 50) api_error(400, "text_attributes", "A document can have at most 50 attributes.")
    d$attributes <- lapply(at, function(v) if (is.null(v) || (is.character(v) && !nzchar(v))) NULL else v[[1]]); d$attributes <- Filter(Negate(is.null), d$attributes)
  }
  p$documents[[tid]] <- d
  text_meta(d)
}

#* Remove a document from the session
#* @tag text
#* @delete /api/projects/<pid>/documents/<tid>
#* @serializer unboxedJSON
function(req, pid, tid) {
  p <- project_of(req, pid)
  if (is.null(p$documents[[tid]])) api_error(404, "text_doc_missing", "The document is not in this session.")
  p$documents[[tid]] <- NULL
  list(deleted = TRUE)
}

#* Preprocessing pipeline (quanteda): tokens and types after each step, top features, per-document counts, R code. Body: {"documentIds": [], "options": {...}}
#* @tag text
#* @post /api/projects/<pid>/text/preprocess
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid) {
  p <- project_of(req, pid)
  b <- parse_json_body(req)
  text_preprocess(p, b$documentIds, b$options)
}

#* Qualitative coding queries. Body: {"kind": overview/retrieve/cooccurrence/attributes/kwic/agreement/autocode/boolean/matrix,
#* "codes", "codings", "documentIds", "codeIds", "level", "attribute", "pattern", "window", "coder1", "coder2"; autocode: "mode",
#* "unit", "categories"; boolean: "op", "codeA", "codeB", "sub", "distance", "scope"}
#* @tag text
#* @post /api/projects/<pid>/text/qda
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid) {
  p <- project_of(req, pid)
  b <- parse_json_body(req)
  q <- normalize_qda(b)
  switch(as.character(b$kind %||% "overview"),
    overview = qda_overview(p, q),
    retrieve = list(segments = qda_retrieve(p, q, as_chr_vec(b$codeIds))),
    cooccurrence = qda_cooccurrence(p, q, if (identical(b$level, "document")) "document" else "overlap"),
    attributes = qda_attributes(p, q, as.character(b$attribute %||% "")),
    kwic = qda_kwic(p, b$documentIds, as.character(b$pattern %||% ""), min(20L, max(1L, as.integer(b$window %||% 6L)))),
    agreement = qda_agreement(p, q, as.character(b$coder1 %||% ""), as.character(b$coder2 %||% "")),
    autocode = qda_autocode(p, b$documentIds, b),
    boolean = qda_boolean(p, q, b),
    matrix = qda_matrix(p, q, b),
    api_error(400, "qda_kind", "Unknown query."))
}

#* Code presence per respondent as variables of the dataset. Body: {"codes", "codings", "codeIds"}
#* @tag text
#* @post /api/projects/<pid>/datasets/<did>/text-codes
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid, did) {
  p <- project_of(req, pid)
  b <- parse_json_body(req)
  r <- qda_to_dataset(p, did, normalize_qda(b), as_chr_vec(b$codeIds))
  list(dataset = dataset_payload(r$dataset), variables = r$variables)
}

#* Export the coding project as REFI-QDA (.qdpx, with the documents) or the codebook (.qdc). Body: {"codes", "codings", "memos", "documentIds", "format"}
#* @tag text
#* @post /api/projects/<pid>/text/refi-export
function(req, res, pid) {
  p <- project_of(req, pid)
  b <- parse_json_body(req)
  fmt <- if (identical(b$format, "qdc")) "qdc" else "qdpx"
  path <- qda_export(p, normalize_qda(b), b$memos %||% list(), b$documentIds, fmt, p$name)
  download(res, readBin(path, "raw", file.info(path)$size), paste0(gsub("[^A-Za-z0-9._-]", "_", p$name), if (fmt == "qdc") "_codebook.qdc" else ".qdpx"), "application/octet-stream")
}

#* AI-assisted coding: suggested codes with verbatim quotes (located in the text), document summaries, or questions
#* answered from the documents. Sends the documents' text to the user's provider; needs consent. Body: {"kind": "suggest" |
#* "summarise" | "ask", "documentIds", "codes", "codings", "focus", "n", "provider", "model", "maxTokens", "consent"}
#* @tag text
#* @post /api/projects/<pid>/text/qda-ai
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid) { b <- parse_json_body(req); qda_ai(project_of(req, pid), normalize_qda(b), b) }

#* Codebook and coded segments as an Excel workbook or a Word report. Body: {"codes", "codings", "memos", "summaries", "format": "xlsx" | "docx"}
#* @tag text
#* @post /api/projects/<pid>/text/qda-report
function(req, res, pid) {
  p <- project_of(req, pid)
  b <- parse_json_body(req)
  fmt <- if (identical(b$format, "docx")) "docx" else "xlsx"
  path <- qda_report(p, normalize_qda(b), b, fmt, p$name)
  download(res, readBin(path, "raw", file.info(path)$size), paste0(gsub("[^A-Za-z0-9._-]", "_", p$name), "_codebook.", fmt),
           if (fmt == "docx") "application/vnd.openxmlformats-officedocument.wordprocessingml.document" else "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
}

#* Import a REFI-QDA project (.qdpx: documents, codebook, coded segments, memos) or codebook (.qdc). Multipart field `file`.
#* @tag text
#* @post /api/projects/<pid>/text/refi-import
#* @parser multi
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid) {
  p <- project_of(req, pid)
  f <- req$body$file
  if (is.null(f$filename)) api_error(400, "no_file", "No file was received.")
  m <- req$body$merge; m <- if (is.list(m)) m$value else m; if (is.raw(m)) m <- rawToChar(m)
  qda_import(p, f$value, f$filename, merge = identical(as.character(m %||% ""), "1"))
}

#* Quantitative text analysis. Body: {"kind": frequency/keyness/collocations/lexdiv/zipf/sentiment/pos/topics/searchk/summary/dictionary, "documentIds", "options" (preprocessing), "params"}
#* @tag text
#* @post /api/projects/<pid>/text/analyze
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid) text_analyze(project_of(req, pid), parse_json_body(req))

#* Download a udpipe language model (once; about 16 MB) for parts of speech. Body: {"language": "en"}
#* @tag text
#* @post /api/text/udpipe-model
#* @serializer unboxedJSON
function(req) tq_download_model(as.character(parse_json_body(req)$language %||% "en"))

#* Layer types, aesthetics, parameters and themes of the ggplot2 builder (the whitelist the server accepts)
#* @tag analyses
#* @get /api/ggplot/registry
#* @serializer unboxedJSON
function() gg_registry()

#* Export a ggplot2 builder plot at print quality. Body: {"spec", "measures", "missingCode", "format": png/pdf/svg/tiff, "dpi"}
#* @tag analyses
#* @post /api/projects/<pid>/datasets/<did>/ggplot/export
function(req, res, pid, did) {
  p <- project_of(req, pid)
  ds <- get_dataset(p, did)
  b <- parse_json_body(req)
  s <- normalize_stats(list(proc = "ggplot", options = list(spec = b$spec), measures = b$measures, levels = b$levels))
  fmt <- as.character(b$format %||% "png")[1]
  dpi <- suppressWarnings(as.integer(b$dpi %||% 300L)); if (is.na(dpi) || dpi < 72 || dpi > 1200) dpi <- 300L
  code <- b$missingCode; if (!is.null(code)) code <- suppressWarnings(as.numeric(code)); if (!is.null(code) && is.na(code)) code <- NULL
  out <- run_ggplot(ds$data, s$options$spec, s$measures, code, dpi = dpi, format = fmt, levels = s$levels)
  download(res, readBin(out$path, "raw", file.info(out$path)$size), paste0("plot.", fmt),
           switch(fmt, png = "image/png", pdf = "application/pdf", svg = "image/svg+xml", tiff = "image/tiff"))
}

#* Journal-ready (APA 7) report of a Statistics results document: Statistical analysis paragraph, Results narrative
#* with numbered tables and figures, references. Body: {"items": [{"resultId", "title", "figures": [{"png" (base64), "caption"}]}],
#* "alpha", "decimals" (2/3), "sections": {method, results, tables, figures, references}, "missingCode", "format": "html"/"docx"}
#* @tag results
#* @post /api/projects/<pid>/stats-report
function(req, res, pid) {
  p <- project_of(req, pid)
  b <- parse_json_body(req)
  raw <- b$items %||% list()
  if (!length(raw)) api_error(400, "no_items", "The results document has no analyses with results yet.")
  if (length(raw) > 60) api_error(400, "too_many", "A report can include at most 60 analyses.")
  items <- lapply(raw, function(it) {
    r <- read_result(p, as.character(it$resultId %||% ""))
    if (!identical(result_kind(r), "stats")) api_error(400, "not_stats", "Only Statistics results can be included in this report.")
    figs <- Filter(function(f) is.character(f$png) && nchar(f$png) < 8e6 && grepl("^[A-Za-z0-9+/=]+$", f$png), it$figures %||% list())
    list(result = r, title = substr(trimws(as.character(it$title %||% r$label)), 1, 160), figures = lapply(figs, function(f) list(png = f$png, caption = substr(as.character(f$caption %||% ""), 1, 400), note = substr(as.character(f$note %||% ""), 1, 600))))
  })
  ds <- p$datasets[[items[[1]]$result$datasetId %||% ""]]
  used <- unique(unlist(lapply(items, function(it) unlist(it$result$options))))
  computed <- Filter(function(v) identical(v$kind, "computed") && v$name %in% used, ds$derived %||% list())
  alpha <- suppressWarnings(as.numeric(b$alpha %||% 0.05)); if (is.na(alpha) || alpha <= 0 || alpha >= 0.5) alpha <- 0.05
  dec <- suppressWarnings(as.integer(b$decimals %||% 2L)); if (is.na(dec) || !dec %in% 2:3) dec <- 2L
  rep <- stats_report_build(items, list(alpha = alpha, decimals = dec, sections = b$sections %||% list(), project = p$name, title = paste(p$name, "- Results"),
                                        missingCode = b$missingCode, nEdits = length(ds$edits %||% list()),
                                        computed = lapply(unname(computed), function(v) list(name = v$name, formula = v$formula))))
  if (identical(b$format, "docx")) {
    if (!requireNamespace("officer", quietly = TRUE) || !requireNamespace("flextable", quietly = TRUE)) api_error(501, "docx_unavailable", "The officer and flextable R packages are needed for Word output.")
    path <- tempfile(fileext = ".docx")
    stats_report_docx(rep, path)
    return(download(res, readBin(path, "raw", file.info(path)$size), paste0(gsub("[^A-Za-z0-9._-]", "_", p$name), "_results.docx"), "application/vnd.openxmlformats-officedocument.wordprocessingml.document"))
  }
  res$setHeader("Content-Type", "text/html; charset=utf-8")
  res$body <- rep$html
  res
}

#* The journal report as edited in the app, converted to Word (.docx) by pandoc. Body: {"html"}
#* @tag results
#* @post /api/projects/<pid>/stats-report/docx-from-html
function(req, res, pid) {
  p <- project_of(req, pid)
  b <- parse_json_body(req)
  path <- stats_report_html_docx(as.character(b$html %||% ""), tempfile(fileext = ".docx"))
  download(res, readBin(path, "raw", file.info(path)$size), paste0(gsub("[^A-Za-z0-9._-]", "_", p$name), "_results.docx"), "application/vnd.openxmlformats-officedocument.wordprocessingml.document")
}

#* One HTML report of several analyses. Body: {"items": [{"resultId", "title", "svg"?, "decisionRule"?}]}
#* @tag results
#* @post /api/projects/<pid>/report/combined
function(req, res, pid) {
  p <- project_of(req, pid)
  body <- parse_json_body(req)
  raw <- body$items %||% list()
  if (!length(raw)) api_error(400, "no_items", "Choose at least one analysis for the combined report.")
  if (length(raw) > 20) api_error(400, "too_many", "A combined report can include at most 20 analyses.")
  items <- lapply(raw, function(it) {
    r <- read_result(p, as.character(it$resultId %||% ""))
    rule <- it$decisionRule
    if (!is.null(rule)) {
      rule$alpha <- suppressWarnings(as.numeric(rule$alpha %||% 0.05))
      if (is.na(rule$alpha) || rule$alpha <= 0 || rule$alpha >= 1) rule$alpha <- 0.05
      rule$criterion <- as.character(rule$criterion %||% "p")
      if (!rule$criterion %in% c("p", "ci", "both")) rule$criterion <- "p"
    }
    list(result = r, title = substr(trimws(as.character(it$title %||% "Analysis")), 1, 120), svg = it$svg, figures = it$figures, decisionRule = rule, qcaFlow = it$qcaFlow)
  })
  html <- combined_report_html(items, p$name, function(r) p$datasets[[r$datasetId %||% ""]])
  res$setHeader("Content-Type", "text/html; charset=utf-8")
  res$body <- html
  res
}

#* HTML report. Body: {"svg": "<svg...>", "decisionRule": {...}, "includeData": false}
#* @tag results
#* @post /api/projects/<pid>/results/<rid>/report
function(req, res, pid, rid) {
  p <- project_of(req, pid)
  r <- read_result(p, rid)
  body <- parse_json_body(req)
  ds <- p$datasets[[r$datasetId %||% ""]]
  rule <- body$decisionRule
  if (!is.null(rule)) {
    rule$alpha <- suppressWarnings(as.numeric(rule$alpha %||% 0.05))
    if (is.na(rule$alpha) || rule$alpha <= 0 || rule$alpha >= 1) api_error(400, "invalid_rule", "The decision-rule significance level must be between 0 and 1.")
    rule$criterion <- as.character(rule$criterion %||% "p")
    if (!rule$criterion %in% c("p", "ci", "both")) api_error(400, "invalid_rule", "Unknown decision criterion.")
  }
  # Projects with several analyses name the analysis in the report title.
  title <- p$name
  an <- body$analysisName
  if (is.character(an) && length(an) == 1 && nzchar(trimws(an))) title <- paste0(p$name, " – ",substr(trimws(an), 1, 120))
  html <- build_report_html(r, title, ds, list(svg = body$svg, figures = body$figures, decisionRule = rule, includeData = isTRUE(body$includeData), qcaFlow = body$qcaFlow))
  res$setHeader("Content-Type", "text/html; charset=utf-8")
  res$body <- html
  res
}

# ---- Generative AI assistant (Phase 7) -------------------------------------------------------------
# Keys are kept in memory for this project session only (or read from environment variables) and never returned.

#* AI providers, whether a key is set (never the key), Azure endpoint and token use in this session.
#* @tag ai
#* @get /api/projects/<pid>/ai/status
#* @serializer unboxedJSON list(null = "null", na = "null")
function(req, pid) ai_status(project_of(req, pid))

#* Set or remove (empty key) the API key of a provider for this session. Body: {provider, key, endpoint?, apiVersion?}
#* @tag ai
#* @put /api/projects/<pid>/ai/key
#* @serializer unboxedJSON list(null = "null", na = "null")
function(req, pid) {
  p <- project_of(req, pid); b <- parse_json_body(req)
  ai_set_key(p, b$provider, b$key, b$endpoint, b$apiVersion)
}

#* Test a provider and model with a tiny request. Body: {provider, model}
#* @tag ai
#* @post /api/projects/<pid>/ai/test
#* @serializer unboxedJSON list(null = "null", na = "null")
function(req, pid) ai_test(project_of(req, pid), parse_json_body(req))

#* The fact sheet that would be sent for the chosen results (shown in the consent dialog). Body: {items}
#* @tag ai
#* @post /api/projects/<pid>/ai/factsheet
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid) ai_fact_sheet(project_of(req, pid), parse_json_body(req)$items %||% list())

#* Grounded writer. Body: {items, section, length, tone, style, notes, provider, model, maxTokens, consent}
#* @tag ai
#* @post /api/projects/<pid>/ai/write
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid) ai_write(project_of(req, pid), parse_json_body(req))

#* Chat about the results. Body: {items, messages: [{role, content}], checks, provider, model, consent}
#* @tag ai
#* @post /api/projects/<pid>/ai/chat
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid) ai_chat(project_of(req, pid), parse_json_body(req))

#* Edit a draft: shorten, expand, formal or journal (with pasted author guidelines). Body: {items, text, action, guidelines, provider, model, consent}
#* @tag ai
#* @post /api/projects/<pid>/ai/edit
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid) ai_edit(project_of(req, pid), parse_json_body(req))

#* Read a picture of a path model into constructs, indicators (matched to the column names) and paths; nothing is drawn.
#* Body: {image (base64), mediaType, columns, provider, model, consent}
#* @tag ai
#* @post /api/projects/<pid>/ai/model-from-image
#* @serializer unboxedJSON list(null = "null", na = "null", digits = NA)
function(req, pid) ai_model_from_image(project_of(req, pid), parse_json_body(req))

#* What was sent to AI providers in this session (most recent first).
#* @tag ai
#* @get /api/projects/<pid>/ai/log
#* @serializer unboxedJSON list(null = "null", na = "null")
function(req, pid) ai_log(project_of(req, pid))
