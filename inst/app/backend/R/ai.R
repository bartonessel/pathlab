# Generative AI assistant (Phase 7): one provider layer for Anthropic Claude, OpenAI, Google Gemini, Azure OpenAI and the
# OpenAI-compatible chat APIs of DeepSeek, Moonshot Kimi and Alibaba Qwen,
# a fact sheet built from the stored results R produced, a curated bibliography, and the grounded writer, chat and edit
# actions. API keys live only in this process's memory (per project session) or in environment variables on the
# server; they are never written to disk, project files or reports, and are never returned by the API.
# Only aggregate results (the same numbers as the HTML reports) are sent; raw respondent data never are.

AI_PROVIDERS <- list(
  anthropic = list(label = "Anthropic Claude", env = "ANTHROPIC_API_KEY", models = c("claude-sonnet-5-5", "claude-opus-5-5", "claude-haiku-5-5"), api = "Messages API"),
  openai = list(label = "OpenAI", env = "OPENAI_API_KEY", models = c("gpt-5", "gpt-5-mini"), api = "Responses API"),
  gemini = list(label = "Google Gemini", env = c("GEMINI_API_KEY", "GOOGLE_API_KEY"), models = c("gemini-2.5-pro", "gemini-2.5-flash"), api = "Gemini API"),
  azure = list(label = "Microsoft Azure OpenAI", env = "AZURE_OPENAI_API_KEY", models = character(0), api = "Azure OpenAI (deployment name as model)"),
  # OpenAI-compatible chat completions. The base address can be changed on the server (for example to the mainland
  # China endpoints, whose keys differ from the international ones). maxOut: the output-token ceiling of the API.
  deepseek = list(label = "DeepSeek", env = "DEEPSEEK_API_KEY", models = c("deepseek-chat", "deepseek-reasoner"), api = "DeepSeek API (OpenAI-compatible)",
                  base = "https://api.deepseek.com", baseEnv = "DEEPSEEK_BASE_URL", images = FALSE, maxOut = c(`deepseek-chat` = 8192)),
  kimi = list(label = "Moonshot Kimi", env = c("MOONSHOT_API_KEY", "KIMI_API_KEY"), models = c("kimi-k2-turbo-preview", "kimi-k2-thinking", "kimi-latest"), api = "Moonshot API (OpenAI-compatible)",
              base = "https://api.moonshot.ai/v1", baseEnv = "MOONSHOT_BASE_URL", images = TRUE),
  qwen = list(label = "Alibaba Qwen", env = c("DASHSCOPE_API_KEY", "QWEN_API_KEY"), models = c("qwen-plus", "qwen-max", "qwen-flash", "qwen-vl-max"), api = "Model Studio / DashScope (OpenAI-compatible)",
              base = "https://dashscope-intl.aliyuncs.com/compatible-mode/v1", baseEnv = "DASHSCOPE_BASE_URL", images = TRUE, maxOut = c(`qwen-max` = 8192, `qwen-vl-max` = 8192))
)
AI_COMPAT <- c("deepseek", "kimi", "qwen")
ai_base_url <- function(provider) {
  p <- AI_PROVIDERS[[provider]]
  u <- sub("/+$", "", trimws(Sys.getenv(p$baseEnv %||% "")))
  if (nzchar(u) && grepl("^https://[A-Za-z0-9.-]+(:[0-9]+)?(/.*)?$", u)) u else p$base
}
AZURE_API_VERSION <- "2024-10-21"
AI_MAX_FACT_CHARS <- 200000

ai_mock_enabled <- function() identical(Sys.getenv("PATHLAB_AI_MOCK"), "1")

ai_state <- function(proj) {
  if (is.null(proj$ai)) proj$ai <- list(keys = list(), azure = list(endpoint = Sys.getenv("AZURE_OPENAI_ENDPOINT"), apiVersion = AZURE_API_VERSION), log = list(), usage = list(input = 0, output = 0, requests = 0))
  proj$ai
}

ai_check_provider <- function(provider) {
  provider <- as.character(provider %||% "")[1]
  ok <- provider %in% names(AI_PROVIDERS) || (identical(provider, "mock") && ai_mock_enabled())
  if (!ok) api_error(400, "ai_provider", "Unknown AI provider.", "Choose Anthropic Claude, OpenAI, Google Gemini, Azure OpenAI, DeepSeek, Moonshot Kimi or Alibaba Qwen.")
  provider
}

ai_key <- function(proj, provider) {
  k <- ai_state(proj)$keys[[provider]]
  if (!is.null(k) && nzchar(k)) return(list(key = k, source = "session"))
  for (e in AI_PROVIDERS[[provider]]$env %||% character(0)) { v <- Sys.getenv(e); if (nzchar(v)) return(list(key = v, source = "environment")) }
  NULL
}

#' Provider status for the settings view (never includes a key).
ai_status <- function(proj) {
  st <- ai_state(proj)
  provs <- lapply(names(AI_PROVIDERS), function(id) {
    p <- AI_PROVIDERS[[id]]; k <- ai_key(proj, id)
    list(id = id, label = p$label, api = p$api, models = as.list(p$models), hasKey = !is.null(k), keySource = if (is.null(k)) NULL else k$source,
         envVar = p$env[1])
  })
  if (ai_mock_enabled()) provs[[length(provs) + 1]] <- list(id = "mock", label = "Test provider (offline)", api = "mock", models = list("mock-1"), hasKey = TRUE, keySource = "environment", envVar = "PATHLAB_AI_MOCK")
  list(providers = provs, azure = list(endpoint = st$azure$endpoint %||% "", apiVersion = st$azure$apiVersion %||% AZURE_API_VERSION),
       usage = st$usage, logEntries = length(st$log))
}

ai_set_key <- function(proj, provider, key, endpoint = NULL, api_version = NULL) {
  provider <- ai_check_provider(provider)
  st <- ai_state(proj)
  key <- trimws(as.character(key %||% ""))[1]
  if (nchar(key) > 500) api_error(400, "ai_key", "The API key is too long.")
  if (grepl("[[:space:]]", key)) api_error(400, "ai_key", "The API key must not contain spaces or line breaks.")
  st$keys[[provider]] <- if (nzchar(key)) key else NULL
  if (identical(provider, "azure")) {
    if (!is.null(endpoint)) {
      ep <- sub("/+$", "", trimws(as.character(endpoint)[1]))
      if (nzchar(ep) && !grepl("^https://[A-Za-z0-9.-]+(:[0-9]+)?(/.*)?$", ep)) api_error(400, "ai_endpoint", "The Azure endpoint must be an https:// address, for example https://my-resource.openai.azure.com.")
      st$azure$endpoint <- ep
    }
    if (!is.null(api_version) && nzchar(api_version)) {
      if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}(-preview)?$", api_version)) api_error(400, "ai_api_version", "The Azure API version looks like 2024-10-21.")
      st$azure$apiVersion <- api_version
    }
  }
  proj$ai <- st
  ai_status(proj)
}

# ---- One request to any provider ---------------------------------------------------------------------

ai_scrub <- function(msg, key) { msg <- sanitize_message(msg, 400); if (!is.null(key) && nzchar(key)) msg <- gsub(key, "<key>", msg, fixed = TRUE); msg }

ai_perform <- function(req, key) {
  req <- httr2::req_timeout(req, 240)
  req <- httr2::req_retry(req, max_tries = 3, is_transient = function(resp) httr2::resp_status(resp) %in% c(408, 429, 500, 502, 503, 504, 529),
                          backoff = function(i) min(20, 2^i))
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  resp <- tryCatch(httr2::req_perform(req), error = function(e)
    api_error(502, "ai_unreachable", paste("The AI provider could not be reached:", ai_scrub(conditionMessage(e), key)), "Check the internet connection, the endpoint address and any proxy."))
  status <- httr2::resp_status(resp)
  body <- tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE), error = function(e) list())
  if (status >= 400) {
    m <- body$error$message %||% (if (is.character(body$error)) body$error else NULL) %||% body$message %||% paste("HTTP", status)
    code <- if (status %in% c(401, 403)) "ai_auth" else if (status == 429) "ai_rate_limit" else if (status == 404) "ai_model" else "ai_provider"
    hint <- switch(code, ai_auth = "Check the API key (and, for Azure, the endpoint) in AI settings.", ai_rate_limit = "The provider's rate or spending limit was reached. Wait a moment or check your plan.",
                   ai_model = "Check the model name (for Azure, the deployment name).", "Try again, or choose another model.")
    api_error(502, code, paste0("The AI provider returned an error (HTTP ", status, "): ", ai_scrub(m, key)), hint)
  }
  body
}

#' messages: list of list(role = "user" | "assistant", content = text). Returns text, token use and the model used.
#' image: optional list(mediaType, data = base64) sent with the last user message (every provider reads images).
ai_complete <- function(proj, provider, model, system, messages, max_tokens = 4000, purpose = "request", image = NULL) {
  provider <- ai_check_provider(provider)
  model <- trimws(as.character(model %||% "")[1])
  if (!nzchar(model)) model <- AI_PROVIDERS[[provider]]$models[1] %||% ""
  if (!nzchar(model) && provider != "mock") api_error(400, "ai_model", "Choose a model (for Azure, the deployment name).")
  if (!grepl("^[A-Za-z0-9._:/-]{1,100}$", model) && provider != "mock") api_error(400, "ai_model", "The model name contains characters that are not allowed.")
  max_tokens <- max(256, min(32000, round(as.numeric(max_tokens %||% 4000))))
  k <- if (provider == "mock") list(key = "", source = "environment") else ai_key(proj, provider)
  if (is.null(k)) api_error(400, "ai_no_key", paste0("No API key is set for ", AI_PROVIDERS[[provider]]$label, "."), "Open AI settings and paste your own API key, or set the environment variable on the server.")
  key <- k$key
  if (!is.null(image) && isFALSE(AI_PROVIDERS[[provider]]$images))
    api_error(400, "ai_no_images", paste0(AI_PROVIDERS[[provider]]$label, "'s API does not read pictures."), "Choose Claude, OpenAI, Gemini, Kimi or Qwen (a vision model such as qwen-vl-max) for this.")
  cap <- AI_PROVIDERS[[provider]]$maxOut
  if (!is.null(cap) && model %in% names(cap)) max_tokens <- min(max_tokens, cap[[model]])
  msgs <- lapply(messages, function(m) list(role = if (identical(m$role, "assistant")) "assistant" else "user", content = as.character(m$content %||% "")))
  last <- length(msgs)
  # The last user message with the image, in each provider's own format (text-only messages stay plain strings).
  with_image <- function(fmt) {
    if (is.null(image)) return(msgs)
    m <- msgs; txt <- m[[last]]$content; url <- paste0("data:", image$mediaType, ";base64,", image$data)
    m[[last]]$content <- switch(fmt,
      anthropic = list(list(type = "image", source = list(type = "base64", media_type = image$mediaType, data = image$data)), list(type = "text", text = txt)),
      openai = list(list(type = "input_image", image_url = url), list(type = "input_text", text = txt)),
      azure = , compat = list(list(type = "image_url", image_url = list(url = url)), list(type = "text", text = txt)))
    m
  }
  t0 <- Sys.time()
  # One chat-completions reply (Azure and the OpenAI-compatible providers) into text, token use and the stop reason.
  chat_out <- function(b) {
    ch <- (b$choices %||% list())[[1]] %||% list()
    list(text = ch$message$content %||% "", input = b$usage$prompt_tokens %||% 0, output = b$usage$completion_tokens %||% 0, model = b$model %||% model,
         stop = if (identical(ch$finish_reason, "length")) "max_tokens" else ch$finish_reason %||% "")
  }
  out <- if (provider %in% AI_COMPAT) {
    chat_out(ai_perform(httr2::req_body_json(httr2::req_auth_bearer_token(httr2::request(paste0(ai_base_url(provider), "/chat/completions")), key),
                                              list(model = model, max_tokens = max_tokens, messages = c(list(list(role = "system", content = system)), with_image("compat")))), key))
  } else switch(provider,
    mock = if (is.null(image)) ai_mock_reply(system, msgs) else ai_mock_image_reply(msgs),
    anthropic = {
      b <- ai_perform(httr2::req_body_json(httr2::req_headers(httr2::request("https://api.anthropic.com/v1/messages"), `x-api-key` = key, `anthropic-version` = "2023-06-01", .redact = "x-api-key"),
                                            list(model = model, max_tokens = max_tokens, system = system, messages = with_image("anthropic"))), key)
      list(text = paste(vapply(Filter(function(x) identical(x$type, "text"), b$content %||% list()), function(x) x$text, ""), collapse = ""),
           input = b$usage$input_tokens %||% 0, output = b$usage$output_tokens %||% 0, model = b$model %||% model, stop = b$stop_reason %||% "")
    },
    openai = {
      b <- ai_perform(httr2::req_body_json(httr2::req_auth_bearer_token(httr2::request("https://api.openai.com/v1/responses"), key),
                                            list(model = model, instructions = system, max_output_tokens = max_tokens,
                                                 input = lapply(with_image("openai"), function(m) list(role = m$role, content = m$content)))), key)
      txt <- unlist(lapply(b$output %||% list(), function(o) if (identical(o$type, "message")) vapply(Filter(function(c) identical(c$type, "output_text"), o$content %||% list()), function(c) c$text, "") else character(0)))
      list(text = paste(txt, collapse = ""), input = b$usage$input_tokens %||% 0, output = b$usage$output_tokens %||% 0, model = b$model %||% model,
           stop = if (identical(b$status, "incomplete")) "max_tokens" else b$status %||% "")
    },
    gemini = {
      url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", utils::URLencode(model, reserved = TRUE), ":generateContent")
      b <- ai_perform(httr2::req_body_json(httr2::req_headers(httr2::request(url), `x-goog-api-key` = key, .redact = "x-goog-api-key"),
                                            list(systemInstruction = list(parts = list(list(text = system))),
                                                 contents = lapply(seq_along(msgs), function(i) { m <- msgs[[i]]
                                                   parts <- list(list(text = m$content))
                                                   if (!is.null(image) && i == last) parts <- c(list(list(inline_data = list(mime_type = image$mediaType, data = image$data))), parts)
                                                   list(role = if (m$role == "assistant") "model" else "user", parts = parts) }),
                                                 generationConfig = list(maxOutputTokens = max_tokens))), key)
      cand <- (b$candidates %||% list())[[1]] %||% list()
      list(text = paste(vapply(cand$content$parts %||% list(), function(p) p$text %||% "", ""), collapse = ""),
           input = b$usageMetadata$promptTokenCount %||% 0, output = b$usageMetadata$candidatesTokenCount %||% 0, model = b$modelVersion %||% model,
           stop = if (identical(cand$finishReason, "MAX_TOKENS")) "max_tokens" else cand$finishReason %||% "")
    },
    azure = {
      az <- ai_state(proj)$azure
      if (!nzchar(az$endpoint %||% "")) api_error(400, "ai_endpoint", "Set the Azure OpenAI endpoint in AI settings (or AZURE_OPENAI_ENDPOINT on the server).")
      url <- paste0(az$endpoint, "/openai/deployments/", utils::URLencode(model, reserved = TRUE), "/chat/completions?api-version=", az$apiVersion %||% AZURE_API_VERSION)
      b <- ai_perform(httr2::req_body_json(httr2::req_headers(httr2::request(url), `api-key` = key, .redact = "api-key"),
                                            list(messages = c(list(list(role = "system", content = system)), with_image("azure")), max_completion_tokens = max_tokens)), key)
      chat_out(b)
    })
  if (!nzchar(trimws(out$text))) api_error(502, "ai_empty", "The AI provider returned no text.", "Try again, raise the output-token limit, or choose another model.")
  st <- ai_state(proj)
  st$usage$input <- st$usage$input + out$input; st$usage$output <- st$usage$output + out$output; st$usage$requests <- st$usage$requests + 1
  sent <- paste0("[system]\n", system, "\n\n", paste(vapply(msgs, function(m) paste0("[", m$role, "]\n", m$content), ""), collapse = "\n\n"))
  if (!is.null(image)) sent <- paste0(sent, sprintf("\n\n[image: %s, %.0f KB]", image$mediaType, nchar(image$data) * 3 / 4 / 1024))
  st$log <- utils::tail(c(st$log, list(list(time = now_iso(), provider = provider, model = out$model, purpose = purpose, sentChars = nchar(sent),
                                             inputTokens = out$input, outputTokens = out$output, seconds = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1),
                                             sent = substr(sent, 1, 300000)))), 50)
  proj$ai <- st
  list(text = out$text, usage = list(input = out$input, output = out$output), model = out$model, provider = provider, truncated = identical(out$stop, "max_tokens"))
}

#' Offline test provider (PATHLAB_AI_MOCK=1): echoes facts so the pipeline can be tested without a key.
ai_mock_reply <- function(system, msgs) {
  last <- msgs[[length(msgs)]]$content
  nums <- regmatches(last, gregexpr("[0-9]+\\.[0-9]{3}", last))[[1]]
  n1 <- if (length(nums)) nums[1] else "0.000"
  list(text = paste0("## Results\n\nAs Table 1 shows, the first reported estimate was ", n1, " (Hair et al., 2022). An unsupported value of 9.876 is included for testing.\n\n",
                     "[Author judgement needed] Explain what this means for theory."),
       input = nchar(paste(system, last)) %/% 4, output = 60, model = "mock-1", stop = "end_turn")
}

#' Offline test provider for pictures: a fixed four-construct model (one indicator is not in the sample data).
ai_mock_image_reply <- function(msgs) {
  txt <- paste0('```json\n{"constructs": [',
    '{"name": "PE", "label": "Performance expectancy", "mode": "reflective", "indicators": ["PE1", "PE2", "PE3"], "x": 0.1, "y": 0.2},',
    '{"name": "EE", "label": "Effort expectancy", "mode": "reflective", "indicators": ["EE 1", "ee2", "EE3"], "x": 0.1, "y": 0.8},',
    '{"name": "BI", "mode": "reflective", "indicators": [], "x": 0.5, "y": 0.5},',
    '{"name": "UB", "mode": "formative", "indicators": ["UB1", "UB2", "UB9"], "x": 0.9, "y": 0.5}],',
    '"paths": [{"from": "PE", "to": "BI"}, {"from": "EE", "to": "BI"}, {"from": "BI", "to": "UB"}, {"from": "BI", "to": "Nowhere"}],',
    '"notes": "Test reading."}\n```')
  list(text = txt, input = 1500, output = 200, model = "mock-1", stop = "end_turn")
}

# ---- Fact sheet ------------------------------------------------------------------------------------

ai_clean <- function(x) trimws(gsub("[[:space:]]+", " ", x))

#' Report HTML to plain text with numbered tables (headings, paragraphs, definition lists, tables in document order).
ai_report_text <- function(html, table_start = 1) {
  doc <- xml2::read_html(html)
  xml2::xml_remove(xml2::xml_find_all(doc, "//style|//script|//svg|//header|//footer|//nav"))
  nodes <- xml2::xml_find_all(doc, "//body//*[self::h1 or self::h2 or self::h3 or self::h4 or self::p or self::li or self::dl or self::table or self::div[contains(@class,'info')]][not(ancestor::table) and not(ancestor::dl) and not(ancestor::div[contains(@class,'info')]) and not(ancestor::li)]")
  lines <- character(0); tables <- list(); n <- table_start
  for (nd in nodes) {
    tag <- xml2::xml_name(nd)
    if (tag %in% c("h1", "h2", "h3", "h4")) lines <- c(lines, "", paste(strrep("#", as.integer(substr(tag, 2, 2)) + 1), ai_clean(xml2::xml_text(nd))))
    else if (tag == "dl") {
      dt <- vapply(xml2::xml_find_all(nd, "./dt"), function(x) ai_clean(xml2::xml_text(x)), ""); dd <- vapply(xml2::xml_find_all(nd, "./dd"), function(x) ai_clean(xml2::xml_text(x)), "")
      m <- min(length(dt), length(dd)); if (m) lines <- c(lines, paste0("- ", dt[seq_len(m)], ": ", dd[seq_len(m)]))
    } else if (tag == "table") {
      cap <- ai_clean(xml2::xml_text(xml2::xml_find_first(nd, "./caption")))
      if (is.na(cap) || !nzchar(cap)) { prev <- xml2::xml_find_first(nd, "./preceding::*[self::h2 or self::h3 or self::h4][1]"); cap <- if (inherits(prev, "xml_missing")) "" else ai_clean(xml2::xml_text(prev)) }
      hdr <- vapply(xml2::xml_find_all(nd, ".//thead//th"), function(x) ai_clean(xml2::xml_text(x)), "")
      trs <- xml2::xml_find_all(nd, ".//tbody/tr|./tr")
      rows <- lapply(trs, function(tr) vapply(xml2::xml_find_all(tr, "./th|./td"), function(x) ai_clean(xml2::xml_text(x)), ""))
      rows <- Filter(length, rows)
      if (!length(hdr) && length(rows)) { hdr <- rows[[1]]; rows <- rows[-1] }
      if (!length(rows)) next
      shown <- utils::head(rows, 60)
      tables[[length(tables) + 1]] <- list(number = n, caption = cap, headers = as.list(hdr), rows = lapply(shown, as.list), omitted = length(rows) - length(shown))
      lines <- c(lines, "", paste0("Table ", n, ". ", cap), paste(hdr, collapse = " | "), vapply(shown, function(r) paste(r, collapse = " | "), ""),
                 if (length(rows) > 60) sprintf("(%d more rows not shown)", length(rows) - 60) else NULL)
      n <- n + 1
    } else {
      t <- ai_clean(xml2::xml_text(nd)); if (nzchar(t)) lines <- c(lines, if (tag == "li") paste0("- ", t) else t)
    }
  }
  list(text = paste(lines, collapse = "\n"), tables = tables, next_table = n)
}

#' items: list(list(resultId, title, decisionRule)). Builds the report of each stored result and converts it to text.
ai_fact_sheet <- function(proj, raw_items, style = "apa7") {
  if (!length(raw_items)) api_error(400, "ai_no_items", "Choose at least one analysis with results.")
  if (length(raw_items) > 12) api_error(400, "ai_too_many", "Choose at most 12 analyses at a time.")
  parts <- character(0); tables <- list(); kinds <- character(0); nt <- 1
  for (i in seq_along(raw_items)) {
    it <- raw_items[[i]]
    r <- read_result(proj, as.character(it$resultId %||% ""))
    rule <- it$decisionRule
    if (!is.null(rule)) {
      rule$alpha <- suppressWarnings(as.numeric(rule$alpha %||% 0.05)); if (is.na(rule$alpha) || rule$alpha <= 0 || rule$alpha >= 1) rule$alpha <- 0.05
      rule$criterion <- as.character(rule$criterion %||% "p"); if (!rule$criterion %in% c("p", "ci", "both")) rule$criterion <- "p"
    }
    title <- substr(trimws(as.character(it$title %||% "Analysis")), 1, 120)
    html <- build_report_html(r, title, proj$datasets[[r$datasetId %||% ""]], list(svg = NULL, decisionRule = rule, includeData = FALSE))
    tx <- ai_report_text(html, nt)
    nt <- tx$next_table
    kind <- result_kind(r); kinds <- c(kinds, kind)
    parts <- c(parts, paste0("=== Analysis ", i, ": ", title, " (", ai_kind_label(kind, r), ") ===\n", tx$text))
    tables <- c(tables, lapply(tx$tables, function(t) c(t, list(analysis = title))))
  }
  text <- paste(parts, collapse = "\n\n")
  if (nchar(text) > AI_MAX_FACT_CHARS) api_error(413, "ai_too_large", "The selected results are too large to send at once.", "Choose fewer analyses, or write the sections one analysis at a time.")
  refs <- ai_references(unique(kinds), style)
  list(text = text, tables = tables, kinds = as.list(unique(kinds)), references = refs, chars = nchar(text))
}

ai_kind_label <- function(kind, r = NULL) switch(kind, plssem = "PLS-SEM", cbsem = "CB-SEM", gsca = "GSCA", efa = "EFA/PCA", regression = "Regression", fimix = "FIMIX-PLS",
                                                 invariance = "Measurement invariance", lpa = "Latent profile analysis", qca = "QCA",
                                                 ml = if (identical(r$mode, "semann")) "SEM-ANN" else "Machine learning", stats = r$label %||% "Statistics", kind)

# ---- Curated references (APA 7). The model may cite only these; the reference list is built from them. ----

#' Harvard (Cite Them Right) from the APA 7 entry. The two carry the same facts in a different order, so the
#' Harvard form is derived rather than written twice and left to drift: initials close up, "and" replaces the
#' ampersand, the year loses its full stop, an article title takes single quotes, the volume leaves the italics,
#' page ranges take "pp.", and an edition becomes "3rd edn.". Place of publication is not in the APA entry and is
#' not invented here; most institutions accept a Harvard reference without it.
harvard_from_apa <- function(apa) {
  m <- regexpr("\\((1[6-9][0-9]{2}|20[0-9]{2})\\)\\. ", apa)
  if (m < 0) return(apa)
  authors <- substr(apa, 1, m - 2)
  year <- sub("^\\((\\d{4})\\).*$", "\\1", substr(apa, m, m + 6))
  rest <- substr(apa, m + attr(m, "match.length"), nchar(apa))
  authors <- gsub(", & ", " and ", authors, fixed = TRUE)
  authors <- gsub(" & ", " and ", authors, fixed = TRUE)
  repeat {                                        # J. F. Hair -> J.F. Hair, however many initials
    squeezed <- gsub("([A-Z]\\.) (?=[A-Z]\\.)", "\\1", authors, perl = TRUE)
    if (identical(squeezed, authors)) break
    authors <- squeezed
  }
  if (startsWith(rest, "<i>")) {                  # a book: title, then edition and publisher
    rest <- sub("</i> \\((\\d+(?:st|nd|rd|th)) ed\\.\\)\\.", "</i>. \\1 edn.", rest)
  } else {                                        # an article: 'title', journal, volume(issue), pages
    cut <- regexpr("[.?!] <i>", rest)             # a title can end in a question mark, not only a full stop
    if (cut > 0) {
      end <- substr(rest, cut, cut)               # keep ? and !, drop the full stop the quotes replace
      title <- paste0(substr(rest, 1, cut - 1), if (end == ".") "" else end)
      tail <- substr(rest, cut + 2, nchar(rest))
      tail <- sub("^<i>(.*), ([0-9]+)</i>", "<i>\\1</i>, \\2", tail)
      tail <- sub("(\\([0-9/]+\\)), ([0-9]+)(&ndash;|–)", "\\1, pp. \\2\\3", tail)
      tail <- sub("^(<i>[^<]*</i>, [0-9]+), ([0-9]+)(&ndash;|–)", "\\1, pp. \\2\\3", tail)
      rest <- paste0("'", title, "', ", tail)
    }
  }
  paste0(authors, " (", year, ") ", rest)
}

#' Vancouver from the APA 7 entry. Authors lose their punctuation (Hair JF), the title loses its italics and its
#' quotation, and the year moves to the end for a book or in front of the volume for an article. ICMJE lists the
#' first six authors and then "et al.". Journal names are kept in full: the NLM abbreviation is not in the entry
#' and guessing one would be worse than printing the whole name, which many journals accept.
vancouver_authors <- function(a) {
  a <- gsub(", & ", ", ", a, fixed = TRUE)
  a <- gsub(" & ", ", ", a, fixed = TRUE)
  parts <- strsplit(a, "(?<=\\.), ", perl = TRUE)[[1]]
  out <- vapply(parts, function(p) {
    p <- trimws(p)
    i <- regexpr(", (?=[A-Z]\\.)", p, perl = TRUE)         # the comma before the initials, which keep their dots
    if (i < 0) return(sub("\\.$", "", p))                  # a corporate author, such as R Core Team
    paste(substr(p, 1, i - 1), gsub("[^A-Za-z]", "", substr(p, i + 2, nchar(p))))
  }, "", USE.NAMES = FALSE)
  if (length(out) > 6) out <- c(out[1:6], "et al")
  paste0(paste(out, collapse = ", "), ".")
}

vancouver_from_apa <- function(apa) {
  m <- regexpr("\\((1[6-9][0-9]{2}|20[0-9]{2})\\)\\. ", apa)
  if (m < 0) return(apa)
  authors <- vancouver_authors(substr(apa, 1, m - 2))
  year <- sub("^\\((\\d{4})\\).*$", "\\1", substr(apa, m, m + 6))
  rest <- substr(apa, m + attr(m, "match.length"), nchar(apa))
  rest <- sub(" https://doi\\.org/(\\S+)$", " doi:\\1", rest)
  if (startsWith(rest, "<i>")) {                           # a book: title, edition, publisher; year
    rest <- sub("</i> \\((\\d+(?:st|nd|rd|th)) ed\\.\\)\\.", ". \\1 ed.", rest)
    rest <- gsub("</?i>", "", rest)
    rest <- sub("( doi:\\S+)$", "", rest)                  # take the doi off before the publisher's full stop
    rest <- sub("\\.\\s*$", "", rest)
    doi <- regmatches(apa, regexpr("https://doi\\.org/\\S+$", apa))
    paste0(authors, " ", rest, "; ", year, ".", if (length(doi)) paste0(" doi:", sub("https://doi\\.org/", "", doi)) else "")
  } else {                                                 # an article: title. Journal. Year;vol(issue):pages.
    cut <- regexpr("[.?!] <i>", rest)
    if (cut < 0) return(paste0(authors, " ", rest))
    title <- substr(rest, 1, cut)
    tail <- substr(rest, cut + 2, nchar(rest))
    jm <- regmatches(tail, regexec("^<i>(.*), ([0-9]+)</i>(\\([0-9/]+\\))?, ([^.]*)\\.(.*)$", tail))[[1]]
    if (length(jm) != 6) return(paste0(authors, " ", title, " ", gsub("</?i>", "", tail)))
    pages <- gsub("&ndash;|–", "-", jm[5])
    paste0(authors, " ", title, " ", jm[2], ". ", year, ";", jm[3], jm[4], ":", pages, ".", jm[6])
  }
}

ai_ref <- function(key, cite, apa, kinds)
  list(key = key, cite = cite, apa = apa, harvard = harvard_from_apa(apa), vancouver = vancouver_from_apa(apa), kinds = kinds)
AI_REFERENCES <- list(
  ai_ref("hair2022", "Hair et al., 2022", "Hair, J. F., Hult, G. T. M., Ringle, C. M., & Sarstedt, M. (2022). <i>A primer on partial least squares structural equation modeling (PLS-SEM)</i> (3rd ed.). Sage.", c("plssem", "fimix")),
  ai_ref("hair2019", "Hair et al., 2019", "Hair, J. F., Risher, J. J., Sarstedt, M., & Ringle, C. M. (2019). When to use and how to report the results of PLS-SEM. <i>European Business Review, 31</i>(1), 2&ndash;24. https://doi.org/10.1108/EBR-11-2018-0203", c("plssem")),
  ai_ref("hair2021", "Hair et al., 2021", "Hair, J. F., Hult, G. T. M., Ringle, C. M., Sarstedt, M., Danks, N. P., & Ray, S. (2021). <i>Partial least squares structural equation modeling (PLS-SEM) using R: A workbook</i>. Springer. https://doi.org/10.1007/978-3-030-80519-7", c("plssem")),
  ai_ref("henseler2015", "Henseler et al., 2015", "Henseler, J., Ringle, C. M., & Sarstedt, M. (2015). A new criterion for assessing discriminant validity in variance-based structural equation modeling. <i>Journal of the Academy of Marketing Science, 43</i>(1), 115&ndash;135. https://doi.org/10.1007/s11747-014-0403-8", c("plssem", "cbsem", "gsca")),
  ai_ref("fornell1981", "Fornell & Larcker, 1981", "Fornell, C., & Larcker, D. F. (1981). Evaluating structural equation models with unobservable variables and measurement error. <i>Journal of Marketing Research, 18</i>(1), 39&ndash;50. https://doi.org/10.2307/3151312", c("plssem", "cbsem", "gsca")),
  ai_ref("dijkstra2015", "Dijkstra & Henseler, 2015", "Dijkstra, T. K., & Henseler, J. (2015). Consistent partial least squares path modeling. <i>MIS Quarterly, 39</i>(2), 297&ndash;316. https://doi.org/10.25300/MISQ/2015/39.2.02", c("plssem")),
  ai_ref("shmueli2019", "Shmueli et al., 2019", "Shmueli, G., Sarstedt, M., Hair, J. F., Cheah, J.-H., Ting, H., Vaithilingam, S., & Ringle, C. M. (2019). Predictive model assessment in PLS-SEM: Guidelines for using PLSpredict. <i>European Journal of Marketing, 53</i>(11), 2322&ndash;2347. https://doi.org/10.1108/EJM-02-2019-0189", c("plssem")),
  ai_ref("kock2015", "Kock, 2015", "Kock, N. (2015). Common method bias in PLS-SEM: A full collinearity assessment approach. <i>International Journal of e-Collaboration, 11</i>(4), 1&ndash;10. https://doi.org/10.4018/ijec.2015100101", c("plssem")),
  ai_ref("dul2016", "Dul, 2016", "Dul, J. (2016). Necessary condition analysis (NCA): Logic and methodology of &ldquo;necessary but not sufficient&rdquo; causality. <i>Organizational Research Methods, 19</i>(1), 10&ndash;52. https://doi.org/10.1177/1094428115584005", c("plssem")),
  ai_ref("henseler2016", "Henseler et al., 2016", "Henseler, J., Ringle, C. M., & Sarstedt, M. (2016). Testing measurement invariance of composites using partial least squares. <i>International Marketing Review, 33</i>(3), 405&ndash;431. https://doi.org/10.1108/IMR-09-2014-0304", c("plssem")),
  ai_ref("lorenzo2006", "Lorenzo-Seva & ten Berge, 2006", "Lorenzo-Seva, U., & ten Berge, J. M. F. (2006). Tucker&rsquo;s congruence coefficient as a meaningful index of factor similarity. <i>Methodology, 2</i>(2), 57&ndash;64. https://doi.org/10.1027/1614-2241.2.2.57", c("plssem", "gsca", "efa")),
  ai_ref("efron1993", "Efron & Tibshirani, 1993", "Efron, B., & Tibshirani, R. J. (1993). <i>An introduction to the bootstrap</i>. Chapman &amp; Hall.", c("plssem", "gsca", "cbsem")),
  ai_ref("hahn2002", "Hahn et al., 2002", "Hahn, C., Johnson, M. D., Herrmann, A., & Huber, F. (2002). Capturing customer heterogeneity using a finite mixture PLS approach. <i>Schmalenbach Business Review, 54</i>(3), 243&ndash;269.", c("fimix")),
  ai_ref("sarstedt2011", "Sarstedt et al., 2011", "Sarstedt, M., Becker, J.-M., Ringle, C. M., & Schwaiger, M. (2011). Uncovering and treating unobserved heterogeneity with FIMIX-PLS: Which model selection criterion provides an appropriate number of segments? <i>Schmalenbach Business Review, 63</i>(1), 34&ndash;62.", c("fimix")),
  ai_ref("rosseel2012", "Rosseel, 2012", "Rosseel, Y. (2012). lavaan: An R package for structural equation modeling. <i>Journal of Statistical Software, 48</i>(2), 1&ndash;36. https://doi.org/10.18637/jss.v048.i02", c("cbsem", "invariance")),
  ai_ref("hu1999", "Hu & Bentler, 1999", "Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance structure analysis: Conventional criteria versus new alternatives. <i>Structural Equation Modeling, 6</i>(1), 1&ndash;55. https://doi.org/10.1080/10705519909540118", c("cbsem", "invariance")),
  ai_ref("kline2016", "Kline, 2016", "Kline, R. B. (2016). <i>Principles and practice of structural equation modeling</i> (4th ed.). Guilford Press.", c("cbsem")),
  ai_ref("mardia1970", "Mardia, 1970", "Mardia, K. V. (1970). Measures of multivariate skewness and kurtosis with applications. <i>Biometrika, 57</i>(3), 519&ndash;530. https://doi.org/10.1093/biomet/57.3.519", c("cbsem")),
  ai_ref("chen2007", "Chen, 2007", "Chen, F. F. (2007). Sensitivity of goodness of fit indexes to lack of measurement invariance. <i>Structural Equation Modeling, 14</i>(3), 464&ndash;504. https://doi.org/10.1080/10705510701301834", c("invariance")),
  ai_ref("putnick2016", "Putnick & Bornstein, 2016", "Putnick, D. L., & Bornstein, M. H. (2016). Measurement invariance conventions and reporting: The state of the art and future directions for psychological research. <i>Developmental Review, 41</i>, 71&ndash;90. https://doi.org/10.1016/j.dr.2016.06.004", c("invariance")),
  ai_ref("hwang2004", "Hwang & Takane, 2004", "Hwang, H., & Takane, Y. (2004). Generalized structured component analysis. <i>Psychometrika, 69</i>(1), 81&ndash;99. https://doi.org/10.1007/BF02295841", c("gsca")),
  ai_ref("hwang2014", "Hwang & Takane, 2014", "Hwang, H., & Takane, Y. (2014). <i>Generalized structured component analysis: A component-based approach to structural equation modeling</i>. CRC Press.", c("gsca")),
  ai_ref("fabrigar1999", "Fabrigar et al., 1999", "Fabrigar, L. R., Wegener, D. T., MacCallum, R. C., & Strahan, E. J. (1999). Evaluating the use of exploratory factor analysis in psychological research. <i>Psychological Methods, 4</i>(3), 272&ndash;299. https://doi.org/10.1037/1082-989X.4.3.272", c("efa")),
  ai_ref("horn1965", "Horn, 1965", "Horn, J. L. (1965). A rationale and test for the number of factors in factor analysis. <i>Psychometrika, 30</i>(2), 179&ndash;185. https://doi.org/10.1007/BF02289447", c("efa")),
  ai_ref("kaiser1974", "Kaiser, 1974", "Kaiser, H. F. (1974). An index of factorial simplicity. <i>Psychometrika, 39</i>(1), 31&ndash;36. https://doi.org/10.1007/BF02291575", c("efa")),
  ai_ref("cohen2003", "Cohen et al., 2003", "Cohen, J., Cohen, P., West, S. G., & Aiken, L. S. (2003). <i>Applied multiple regression/correlation analysis for the behavioral sciences</i> (3rd ed.). Lawrence Erlbaum.", c("regression")),
  ai_ref("hayes2022", "Hayes, 2022", "Hayes, A. F. (2022). <i>Introduction to mediation, moderation, and conditional process analysis: A regression-based approach</i> (3rd ed.). Guilford Press.", c("regression", "plssem")),
  ai_ref("durbin1950", "Durbin & Watson, 1950", "Durbin, J., & Watson, G. S. (1950). Testing for serial correlation in least squares regression: I. <i>Biometrika, 37</i>(3/4), 409&ndash;428. https://doi.org/10.2307/2332391", c("regression")),
  ai_ref("hosmer2013", "Hosmer et al., 2013", "Hosmer, D. W., Lemeshow, S., & Sturdivant, R. X. (2013). <i>Applied logistic regression</i> (3rd ed.). Wiley.", c("regression")),
  ai_ref("delong1988", "DeLong et al., 1988", "DeLong, E. R., DeLong, D. M., & Clarke-Pearson, D. L. (1988). Comparing the areas under two or more correlated receiver operating characteristic curves: A nonparametric approach. <i>Biometrics, 44</i>(3), 837&ndash;845. https://doi.org/10.2307/2531595", c("regression")),
  ai_ref("robin2011", "Robin et al., 2011", "Robin, X., Turck, N., Hainard, A., Tiberti, N., Lisacek, F., Sanchez, J.-C., & M&uuml;ller, M. (2011). pROC: An open-source package for R and S+ to analyze and compare ROC curves. <i>BMC Bioinformatics, 12</i>, 77. https://doi.org/10.1186/1471-2105-12-77", c("regression")),
  ai_ref("nylund2007", "Nylund et al., 2007", "Nylund, K. L., Asparouhov, T., & Muth&eacute;n, B. O. (2007). Deciding on the number of classes in latent class analysis and growth mixture modeling: A Monte Carlo simulation study. <i>Structural Equation Modeling, 14</i>(4), 535&ndash;569. https://doi.org/10.1080/10705510701575396", c("lpa")),
  ai_ref("spurk2020", "Spurk et al., 2020", "Spurk, D., Hirschi, A., Wang, M., Valero, D., & Kauffeld, S. (2020). Latent profile analysis: A review and &ldquo;how to&rdquo; guide of its application within vocational behavior research. <i>Journal of Vocational Behavior, 120</i>, 103445. https://doi.org/10.1016/j.jvb.2020.103445", c("lpa")),
  ai_ref("ragin2008", "Ragin, 2008", "Ragin, C. C. (2008). <i>Redesigning social inquiry: Fuzzy sets and beyond</i>. University of Chicago Press.", c("qca")),
  ai_ref("schneider2012", "Schneider & Wagemann, 2012", "Schneider, C. Q., & Wagemann, C. (2012). <i>Set-theoretic methods for the social sciences: A guide to qualitative comparative analysis</i>. Cambridge University Press.", c("qca")),
  ai_ref("dusa2019", "Dusa, 2019", "Dusa, A. (2019). <i>QCA with R: A comprehensive resource</i>. Springer. https://doi.org/10.1007/978-3-319-75668-4", c("qca")),
  ai_ref("breiman2001", "Breiman, 2001", "Breiman, L. (2001). Random forests. <i>Machine Learning, 45</i>(1), 5&ndash;32. https://doi.org/10.1023/A:1010933404324", c("ml")),
  ai_ref("kuhn2013", "Kuhn & Johnson, 2013", "Kuhn, M., & Johnson, K. (2013). <i>Applied predictive modeling</i>. Springer.", c("ml")),
  ai_ref("cohen1988", "Cohen, 1988", "Cohen, J. (1988). <i>Statistical power analysis for the behavioral sciences</i> (2nd ed.). Lawrence Erlbaum.", c("stats", "regression", "plssem")),
  ai_ref("hausman1978", "Hausman, 1978", "Hausman, J. A. (1978). Specification tests in econometrics. <i>Econometrica, 46</i>(6), 1251&ndash;1271. https://doi.org/10.2307/1913827", c("stats")),
  ai_ref("wooldridge2010", "Wooldridge, 2010", "Wooldridge, J. M. (2010). <i>Econometric analysis of cross section and panel data</i> (2nd ed.). MIT Press.", c("stats")),
  ai_ref("baltagi2021", "Baltagi, 2021", "Baltagi, B. H. (2021). <i>Econometric analysis of panel data</i> (6th ed.). Springer.", c("stats")),
  ai_ref("field2018", "Field, 2018", "Field, A. (2018). <i>Discovering statistics using IBM SPSS Statistics</i> (5th ed.). Sage.", c("stats"))
)

#' References for the analyses used, plus R itself (year of the running R version).
ai_references <- function(kinds, style = "apa7") {
  sel <- Filter(function(r) any(r$kinds %in% kinds), AI_REFERENCES)
  yr <- R.version$year
  rcore <- ai_ref("rcore", paste0("R Core Team, ", yr), paste0("R Core Team. (", yr, "). <i>R: A language and environment for statistical computing</i> (Version ", R.version$major, ".", R.version$minor,
                                                                 ") [Computer software]. R Foundation for Statistical Computing. https://www.R-project.org/"), "all")
  pathlab <- ai_ref("pathlab", pathlab_in_text(), pathlab_citation("html"), "all")
  # `apa` carries the chosen style's text: both APA 7 and Harvard cite by author and year, so everything that reads
  # this list (the citation check, the alphabetical order, the export) works the same either way.
  lapply(c(list(pathlab, rcore), sel), function(r)
    list(key = r$key, cite = r$cite,
         apa = switch(style, harvard = r$harvard, vancouver = r$vancouver, r$apa)))
}

# ---- Prompts ------------------------------------------------------------------------------------------

AI_LENGTH_WORDS <- c(short = 400, standard = 900, extensive = 1800)

#' Vancouver cites by number, so the allowed list is numbered and the model is told to use those numbers.
ai_ref_lines <- function(refs, style = "apa7") {
  cites <- vapply(refs, function(r) r$cite, "")
  if (identical(style, "vancouver")) paste0("- [", seq_along(cites), "] ", cites, collapse = "\n")
  else paste0("- ", cites, collapse = "\n")
}

ai_writer_system <- function(o) {
  sec <- switch(o$section, methods = "the Methods section (data analysis approach and measurement/evaluation criteria)", results = "the Results section",
                "the Methods (data analysis) and Results sections")
  words <- AI_LENGTH_WORDS[[o$length]] %||% 900
  paste0(
    "You are the academic writing assistant of PathLab SEM, a statistics application. Write ", sec, " of a journal manuscript in ", o$styleLabel, " style.\n\n",
    "Strict rules:\n",
    "1. Use ONLY the facts in the FACT SHEET supplied by the user message. Never invent numbers, analyses, samples, software, settings or results. If something a reader would expect is not in the fact sheet, write [missing: what is needed] instead of guessing.\n",
    "2. Copy every number exactly as it appears in the fact sheet, or rounded to fewer decimals. Do not compute new statistics.\n",
    "3. Refer to the tables by the numbers they have in the fact sheet, for example \"As Table 3 shows\". Do not create tables or new table numbers.\n",
    if (identical(o$style, "vancouver"))
      "4. Cite only works from the ALLOWED REFERENCES list, by the number in square brackets shown against each one, for example [1] or [2,5]. Never cite an author and year, never cite anything outside the list, and do not write a reference list.\n"
    else paste0("4. Cite only works from the ALLOWED REFERENCES list, using exactly the author and year shown, in ", o$styleLabel, " in-text form: (Author, Year), (Author & Author, Year), (Author et al., Year), or Author et al. (Year). Never cite anything else and do not write a reference list.\n"),
    "5. Interpretation that needs the author's judgement (why a result occurred, theoretical or practical implications, comparison with prior studies) goes in a separate paragraph that begins with \"[Author judgement needed]\".\n",
    "6. Report hypotheses with the decisions stated in the fact sheet. Use the past tense for results and a ", o$tone, " academic tone.\n",
    "7. Output Markdown only: \"## \" and \"### \" headings and paragraphs. No tables, no code fences, no bullet lists unless a list is clearly better, no preamble or closing remarks.\n",
    "Target length: about ", words, " words", if (o$section == "both") " per section" else "", ".")
}

ai_fact_block <- function(fs, notes = NULL, style = "apa7") paste0(
  if (!is.null(notes) && nzchar(notes)) paste0("STUDY CONTEXT FROM THE AUTHOR (use it for wording only; it is not a source of numbers):\n", notes, "\n\n") else "",
  "FACT SHEET (results produced by R in PathLab SEM):\n", fs$text, "\n\nALLOWED REFERENCES:\n", ai_ref_lines(fs$references, style))

normalize_writer_options <- function(b) {
  pick <- function(x, ok, d) { x <- as.character(x %||% d)[1]; if (x %in% ok) x else d }
  style <- pick(b$style, c("apa7", "harvard", "vancouver"), "apa7")
  list(section = pick(b$section, c("methods", "results", "both"), "results"), length = pick(b$length, names(AI_LENGTH_WORDS), "standard"),
       tone = pick(b$tone, c("formal", "concise", "accessible"), "formal"), style = style,
       styleLabel = switch(style, apa7 = "APA 7th edition", harvard = "Harvard", vancouver = "Vancouver"),
       notes = substr(trimws(as.character(b$notes %||% "")), 1, 4000))
}

ai_require_consent <- function(b) if (!isTRUE(b$consent)) api_error(400, "ai_consent", "Confirm what will be sent to the AI provider first.", "Open the consent dialog and accept it for this session.")

ai_write <- function(proj, b) {
  ai_require_consent(b)
  o <- normalize_writer_options(b)
  fs <- ai_fact_sheet(proj, b$items %||% list(), o$style)
  r <- ai_complete(proj, b$provider, b$model, ai_writer_system(o), list(list(role = "user", content = ai_fact_block(fs, o$notes, o$style))), b$maxTokens %||% 8000, paste("write", o$section))
  c(r, list(factSheet = fs, options = o))
}

ai_chat <- function(proj, b) {
  ai_require_consent(b)
  fs <- ai_fact_sheet(proj, b$items %||% list())
  msgs <- utils::tail(Filter(function(m) is.list(m) && nzchar(as.character(m$content %||% "")), b$messages %||% list()), 20)
  if (!length(msgs) || !identical(msgs[[length(msgs)]]$role, "user")) api_error(400, "ai_chat", "Type a question first.")
  msgs <- lapply(msgs, function(m) list(role = m$role, content = substr(as.character(m$content), 1, 8000)))
  checks <- substr(as.character(b$checks %||% ""), 1, 8000)
  system <- paste0(
    "You are the assistant of PathLab SEM, a statistics application. You explain the user's results, say what to report and how, and suggest fixes for model problems. ",
    "Ground every statement about the user's results in the FACT SHEET below; never invent numbers. If the fact sheet does not contain something, say so. ",
    "General methodological guidance is fine, but cite only works from the ALLOWED REFERENCES list (author and year as given). Be concise and practical; use short Markdown paragraphs or lists.\n\n",
    ai_fact_block(fs), if (nzchar(checks)) paste0("\n\nAPP CHECKS AND SUGGESTIONS:\n", checks) else "")
  ai_complete(proj, b$provider, b$model, system, msgs, b$maxTokens %||% 2000, "chat")
}

AI_EDIT_ACTIONS <- c(shorten = "Shorten the text by about a third, keeping every number, table reference and citation that remains relevant.",
                     expand = "Expand the text with more explanation of the methods and results, using only facts from the fact sheet.",
                     formal = "Rewrite the text in a more formal academic register.",
                     journal = "Rewrite the text to follow the journal's author guidelines given below (structure, headings, reporting conventions, length).")

ai_edit <- function(proj, b) {
  ai_require_consent(b)
  action <- as.character(b$action %||% "")[1]
  if (!action %in% names(AI_EDIT_ACTIONS)) api_error(400, "ai_edit", "Unknown edit action.")
  text <- as.character(b$text %||% "")[1]
  if (!nzchar(trimws(text))) api_error(400, "ai_edit", "There is no text to edit.")
  if (nchar(text) > 60000) api_error(413, "ai_edit", "The text is too long to edit at once.", "Edit one section at a time.")
  guide <- substr(trimws(as.character(b$guidelines %||% "")), 1, 12000)
  if (action == "journal" && !nzchar(guide)) api_error(400, "ai_edit", "Paste the journal's author guidelines first.")
  fs <- ai_fact_sheet(proj, b$items %||% list())
  system <- paste0("You edit a manuscript draft written from statistical results. ", AI_EDIT_ACTIONS[[action]],
                   " Keep the rules: use only facts from the FACT SHEET; do not change numbers; keep table references; cite only the ALLOWED REFERENCES; keep paragraphs that begin with \"[Author judgement needed]\" marked that way; output Markdown only, no preamble.")
  user <- paste0(ai_fact_block(fs), if (nzchar(guide)) paste0("\n\nJOURNAL AUTHOR GUIDELINES:\n", guide) else "", "\n\nDRAFT TO EDIT:\n", text)
  ai_complete(proj, b$provider, b$model, system, list(list(role = "user", content = user)), b$maxTokens %||% 8000, paste("edit", action))
}

ai_test <- function(proj, b) {
  r <- ai_complete(proj, b$provider, b$model, "Reply with the single word OK.", list(list(role = "user", content = "Connection test.")), 256, "connection test")
  list(ok = TRUE, model = r$model, reply = substr(r$text, 1, 80), usage = r$usage)
}

ai_log <- function(proj) rev(ai_state(proj)$log)

# ---- Model from a picture (PLS-SEM) -------------------------------------------------------------------
# A picture of a path model (drawn in Word, PowerPoint, a paper or by hand) is read once by the chosen AI provider into
# constructs, indicators and paths; the indicators are then matched to the dataset's column names here, in R. Only the
# picture and the column names are sent, never data values. Nothing is drawn until the user has checked the result.

AI_IMAGE_TYPES <- c("image/png", "image/jpeg", "image/webp", "image/gif")
AI_IMAGE_MAX_BYTES <- 5 * 1024^2

ai_diagram_system <- function(columns) paste0(
  "You read pictures of structural equation (path) models, as drawn for PLS-SEM studies, and return them as JSON. ",
  "Constructs (latent variables) are usually ovals or circles, sometimes boxes; indicators (items) are small boxes or labels attached to a construct; ",
  "structural paths are arrows between constructs. Use the names exactly as written in the picture (an abbreviation inside the shape is the name; ",
  "a longer full name, if also shown, is the label). List a construct's indicators only when the picture shows them; never invent them. ",
  "A construct is formative when the arrows run from its indicators to the construct; otherwise it is reflective. ",
  "Give each construct's centre as x and y between 0 (left/top) and 1 (right/bottom) of the picture. ",
  "Ignore numbers on the arrows (estimates), error terms, R-squared values and legends.\n\n",
  "Reply with JSON only, no other text, in this form:\n",
  '{"constructs": [{"name": "PE", "label": "Performance expectancy", "mode": "reflective", "indicators": ["PE1", "PE2"], "x": 0.1, "y": 0.3}], ',
  '"paths": [{"from": "PE", "to": "BI"}], "notes": "anything that was unclear, in one short sentence, or an empty string"}\n',
  "If the picture is not a path model, reply {\"constructs\": [], \"paths\": [], \"notes\": \"<what the picture shows>\"}.\n\n",
  "The dataset has these column names (indicators are usually among them): ", paste(utils::head(columns, 600), collapse = ", "))

#' The JSON object in a reply (also inside a ```json block, or with text around it).
ai_extract_json <- function(text) {
  t <- sub("^[^{]*", "", text); t <- sub("[^}]*$", "", t)
  out <- tryCatch(jsonlite::fromJSON(t, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(out) || !is.list(out)) api_error(502, "ai_diagram_reply", "The AI reply could not be read as a model.", "Try again, or use a clearer picture (a screenshot rather than a photo).")
  out
}

#' A column name for an indicator written in the picture: exact, then ignoring case, spaces, dots, dashes and underscores.
ai_match_column <- function(text, columns) {
  if (text %in% columns) return(text)
  key <- function(x) tolower(gsub("[[:space:]._-]+", "", x))
  hit <- columns[key(columns) == key(text)]
  if (length(hit)) hit[1] else NULL
}

#' Columns that look like a construct's items when the picture shows none: the construct name followed by a number
#' (PE1, PE_2, pe 3), not already used by another construct.
ai_prefix_columns <- function(name, columns, used) {
  pat <- paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", name), "[[:space:]._-]*[0-9]+$")
  setdiff(columns[grepl(pat, columns, ignore.case = TRUE)], used)
}

#' Normalises the provider's reading and matches it to the dataset. Returns constructs with their indicators
#' ({text, column, suggestions}), whether indicators were suggested from the data, and the valid paths.
ai_diagram_normalise <- function(raw, columns) {
  num01 <- function(v) { v <- suppressWarnings(as.numeric(v %||% NA))[1]; if (is.na(v)) NA_real_ else max(0, min(1, v)) }
  cons <- list(); seen <- character(0)
  for (c in raw$constructs %||% list()) {
    if (!is.list(c)) next
    name <- trimws(as.character(c$name %||% "")[1])
    if (!nzchar(name) || tolower(name) %in% tolower(seen)) next
    seen <- c(seen, name)
    cons[[length(cons) + 1]] <- list(name = substr(name, 1, 60), label = substr(trimws(as.character(c$label %||% "")[1]), 1, 120),
      mode = if (identical(tolower(as.character(c$mode %||% "")[1]), "formative")) "formative" else "reflective",
      written = unique(trimws(vapply(c$indicators %||% list(), function(i) as.character(i %||% "")[1], ""))), x = num01(c$x), y = num01(c$y))
  }
  used <- character(0)
  # Indicators shown in the picture first (exact matches before loose ones), then prefix suggestions for the rest.
  for (k in seq_along(cons)) {
    w <- Filter(nzchar, cons[[k]]$written)
    cons[[k]]$indicators <- lapply(w, function(t) {
      col <- ai_match_column(t, setdiff(columns, used))
      if (!is.null(col)) used <<- c(used, col)
      list(text = t, column = col)
    })
  }
  for (k in seq_along(cons)) {
    cons[[k]]$indicators <- lapply(cons[[k]]$indicators, function(i) {
      if (is.null(i$column)) i$suggestions <- as.list(utils::head(c(ai_prefix_columns(cons[[k]]$name, columns, used), ai_prefix_columns(sub("[^A-Za-z]+$", "", i$text), columns, used)), 6))
      i
    })
    cons[[k]]$suggested <- FALSE
    if (!length(cons[[k]]$indicators)) {
      sug <- ai_prefix_columns(cons[[k]]$name, columns, used)
      used <- c(used, sug)
      cons[[k]]$indicators <- lapply(sug, function(col) list(text = col, column = col))
      cons[[k]]$suggested <- length(sug) > 0
    }
    cons[[k]]$written <- NULL
  }
  names_ <- vapply(cons, `[[`, "", "name")
  find <- function(n) { i <- match(tolower(trimws(as.character(n %||% "")[1])), tolower(names_)); if (is.na(i)) NULL else names_[i] }
  paths <- list(); skipped <- 0L; keys <- character(0)
  for (p in raw$paths %||% list()) {
    f <- find(p$from); t <- find(p$to)
    if (is.null(f) || is.null(t) || identical(f, t)) { skipped <- skipped + 1L; next }
    key <- paste(f, t, sep = "\r")
    if (key %in% keys) next
    keys <- c(keys, key); paths[[length(paths) + 1]] <- list(from = f, to = t)
  }
  list(constructs = cons, paths = paths, skippedPaths = skipped, notes = substr(trimws(as.character(raw$notes %||% "")[1]), 1, 400))
}

#' Body: {image: base64, mediaType, columns, provider, model, consent}
ai_model_from_image <- function(proj, b) {
  if (!isTRUE(b$consent)) api_error(400, "ai_consent", "Confirm that the picture may be sent to the AI provider first.")
  mt <- as.character(b$mediaType %||% "")[1]
  if (!mt %in% AI_IMAGE_TYPES) api_error(400, "ai_image", "Use a PNG, JPEG, WebP or GIF picture.")
  data <- gsub("[[:space:]]", "", sub("^data:[^,]*,", "", as.character(b$image %||% "")[1]))
  if (!nzchar(data) || !grepl("^[A-Za-z0-9+/]+=*$", data)) api_error(400, "ai_image", "The picture could not be read.")
  if (nchar(data) * 3 / 4 > AI_IMAGE_MAX_BYTES) api_error(413, "ai_image", "The picture is larger than 5 MB.", "Crop it to the model, or take a smaller screenshot.")
  columns <- unique(as.character(unlist(b$columns %||% list())))
  r <- ai_complete(proj, b$provider, b$model, ai_diagram_system(columns),
                   list(list(role = "user", content = "Read the path model in this picture and reply with the JSON.")), b$maxTokens %||% 4000, "model from a picture",
                   image = list(mediaType = mt, data = data))
  out <- ai_diagram_normalise(ai_extract_json(r$text), columns)
  c(out, list(usage = r$usage, model = r$model, provider = r$provider))
}
