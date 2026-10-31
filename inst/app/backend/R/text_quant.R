# Textual analysis, Part C: quantitative text analysis. Every number comes from the R packages named in each
# function (quanteda and its textstats / textplots, sentimentr with the lexicon package, udpipe, topicmodels, stm,
# seededlda, textrank); plots are drawn by R (ggplot2, quanteda.textplots) and returned as PNG with the R code.

tq_png <- function(draw, width = 7, height = 4.5, dpi = 120) {
  f <- tempfile(fileext = ".png")
  ragg::agg_png(f, width = width, height = height, units = "in", res = dpi, background = "white")
  ok <- tryCatch({ r <- draw(); if (inherits(r, "ggplot")) print(r); TRUE }, finally = grDevices::dev.off())
  jsonlite::base64_enc(readBin(f, "raw", file.info(f)$size))
}
tq_theme <- function() ggplot2::theme_minimal(base_size = 11) + ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), plot.title = ggplot2::element_text(face = "bold"))
tq_table <- function(df, cols = names(df)) list(columns = as.list(cols), rows = unname(lapply(seq_len(nrow(df)), function(i) as.list(df[i, cols, drop = FALSE]))))

#' Corpus with document attributes as docvars, plus the pipeline (Part A options).
tq_prep <- function(proj, ids, opts) {
  docs <- text_docs(proj, ids)
  o <- normalize_text_options(opts)
  pp <- text_pipeline(docs, o)
  attrs <- unique(unlist(lapply(docs, function(d) names(d$attributes))))
  dv <- data.frame(doc_name = vapply(docs, `[[`, "", "name"), stringsAsFactors = FALSE)
  for (a in attrs) dv[[a]] <- vapply(docs, function(d) { v <- d$attributes[[a]]; if (is.null(v)) NA_character_ else as.character(v) }, "")
  quanteda::docvars(pp$corpus) <- dv; quanteda::docvars(pp$tokens) <- dv; quanteda::docvars(pp$dfm) <- dv
  c(pp, list(docs = docs, options = o))
}
tq_group <- function(pp, attribute) {
  if (!nzchar(attribute %||% "") || !attribute %in% names(quanteda::docvars(pp$dfm))) api_error(422, "text_attribute", "Choose a document attribute to compare groups.")
  g <- quanteda::docvars(pp$dfm)[[attribute]]; g[is.na(g)] <- "(missing)"; g
}

# ---- Frequencies, TF-IDF, word clouds -----------------------------------------------------------------------------
tq_frequency <- function(pp, p) {
  n <- min(as.integer(p$n %||% 30L), 200L)
  fr <- quanteda.textstats::textstat_frequency(pp$dfm, n = n)
  tf <- quanteda::dfm_tfidf(pp$dfm)
  top_tfidf <- lapply(seq_len(quanteda::ndoc(tf)), function(i) { tv <- quanteda::topfeatures(tf[i, ], 6); list(document = pp$docs[[i]]$name, terms = paste(names(tv), collapse = ", ")) })
  top20 <- utils::head(fr, 20)
  bar <- tq_png(function() ggplot2::ggplot(top20, ggplot2::aes(x = stats::reorder(feature, frequency), y = frequency)) + ggplot2::geom_col(fill = "#0e7490") + ggplot2::coord_flip() +
                  ggplot2::labs(x = NULL, y = "Frequency", title = "Most frequent features") + tq_theme())
  grp <- p$attribute %||% ""
  cloud <- tryCatch(tq_png(function() {
    if (nzchar(grp)) { g <- tq_group(pp, grp); gd <- quanteda::dfm_group(pp$dfm, groups = g); quanteda.textplots::textplot_wordcloud(gd, comparison = quanteda::ndoc(gd) > 1, max_words = 120, color = c("#0e7490", "#c2410c", "#7c3aed", "#4d7c0f", "#b45309", "#be185d")) }
    else quanteda.textplots::textplot_wordcloud(pp$dfm, max_words = 120, color = c("#5b6675", "#0e7490", "#2f6db5", "#c2410c"))
  }, 6.5, 6.5), error = function(e) { warning("No word cloud: the features have too few distinct frequencies (add documents or lower the minimum frequency).", call. = FALSE); NULL })
  list(tables = list(c(list(id = "freq", title = "Feature frequencies"), tq_table(fr, c("feature", "frequency", "rank", "docfreq"))),
                     list(id = "tfidf", title = "Most distinctive terms per document (TF-IDF)", columns = list("document", "terms"), rows = top_tfidf)),
       plots = Filter(Negate(is.null), list(list(title = "Most frequent features", png = bar), if (!is.null(cloud)) list(title = if (nzchar(grp)) paste("Comparison cloud by", grp) else "Word cloud", png = cloud, square = TRUE))),
       code = c("library(quanteda); library(quanteda.textstats); library(quanteda.textplots)", "textstat_frequency(dfmat, n = 30)", "topfeatures(dfm_tfidf(dfmat)[1, ], 6)   # per document",
                if (nzchar(grp)) sprintf("textplot_wordcloud(dfm_group(dfmat, groups = docvars(dfmat, %s)), comparison = TRUE)", deparse(grp)) else "textplot_wordcloud(dfmat, max_words = 120)"))
}

# ---- Keyness --------------------------------------------------------------------------------------------------------
tq_keyness <- function(pp, p) {
  g <- tq_group(pp, p$attribute)
  target <- as.character(p$target %||% "")[1]
  if (!target %in% g) target <- sort(unique(g))[1]
  measure <- if ((p$measure %||% "chi2") %in% c("chi2", "lr", "pmi")) p$measure %||% "chi2" else "chi2"
  gd <- quanteda::dfm_group(pp$dfm, groups = g)
  if (quanteda::ndoc(gd) < 2) api_error(422, "text_keyness", "Keyness needs at least two groups.")
  k <- quanteda.textstats::textstat_keyness(gd, target = target, measure = measure)
  stat <- names(k)[2]
  top <- rbind(utils::head(k, 20), utils::tail(k, 20)); top <- top[!duplicated(top$feature), ]
  plot <- tq_png(function() quanteda.textplots::textplot_keyness(k, n = 15, color = c("#0e7490", "#c2410c"), labelsize = 3.4), 7.5, 5.5)
  list(tables = list(c(list(id = "keyness", title = sprintf("Keyness: %s = %s vs the other groups (%s)", p$attribute, target, measure)), tq_table(as.data.frame(top), c("feature", stat, "p", "n_target", "n_reference")))),
       plots = list(list(title = sprintf("Keyness of %s = %s", p$attribute, target), png = plot)), target = target,
       code = c(sprintf("gd <- dfm_group(dfmat, groups = docvars(dfmat, %s))", deparse(p$attribute)), sprintf("k <- textstat_keyness(gd, target = %s, measure = %s)", deparse(target), deparse(measure)), "textplot_keyness(k)"))
}

# ---- Collocations ---------------------------------------------------------------------------------------------------
tq_collocations <- function(pp, p) {
  size <- min(max(as.integer(p$size %||% 2L), 2L), 4L); minc <- max(as.integer(p$minCount %||% 2L), 1L)
  tk <- quanteda::tokens_tolower(quanteda::tokens(pp$corpus, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE))
  tk <- quanteda::tokens_remove(tk, pp$stopwords, padding = TRUE)
  co <- quanteda.textstats::textstat_collocations(tk, size = size, min_count = minc)
  co <- utils::head(co[order(-co$z), ], 60)
  list(tables = list(c(list(id = "coll", title = sprintf("Collocations of %d words (z = Wald statistic of λ)", size)), tq_table(as.data.frame(co), c("collocation", "count", "lambda", "z")))), plots = list(),
       code = c("toks <- tokens_remove(tokens_tolower(tokens(corp, remove_punct = TRUE)), stopwords(\"en\"), padding = TRUE)", sprintf("textstat_collocations(toks, size = %d, min_count = %d)", size, minc)))
}

# ---- Lexical diversity and readability -----------------------------------------------------------------------------
tq_lexdiv <- function(pp, p) {
  ld <- quanteda.textstats::textstat_lexdiv(pp$tokens, measure = c("TTR", "CTTR", "MATTR", "MSTTR"), MATTR_window = 50L, MSTTR_segment = 50L)
  rd <- quanteda.textstats::textstat_readability(pp$corpus, measure = c("Flesch", "Flesch.Kincaid", "FOG", "Coleman.Liau.grade"))
  df <- data.frame(document = vapply(pp$docs, `[[`, "", "name"), tokens = as.integer(quanteda::ntoken(pp$tokens)), TTR = ld$TTR, CTTR = ld$CTTR, MATTR = ld$MATTR,
                   Flesch = rd$Flesch, Flesch.Kincaid = rd$Flesch.Kincaid, FOG = rd$FOG, stringsAsFactors = FALSE)
  plot <- tq_png(function() ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(document, Flesch), y = Flesch)) + ggplot2::geom_col(fill = "#2f6db5") + ggplot2::coord_flip() +
                   ggplot2::labs(x = NULL, y = "Flesch reading ease (higher = easier)", title = "Readability") + tq_theme(), 7, max(3, 0.28 * nrow(df) + 1.5))
  list(tables = list(c(list(id = "lexdiv", title = "Lexical diversity (after preprocessing) and readability (original text)", notes = list("MATTR: moving-average type-token ratio, 50-token window; Flesch 60–70 is plain English.")), tq_table(df))),
       plots = list(list(title = "Readability (Flesch)", png = plot)),
       code = c("textstat_lexdiv(toks, measure = c(\"TTR\", \"CTTR\", \"MATTR\", \"MSTTR\"), MATTR_window = 50)", "textstat_readability(corp, measure = c(\"Flesch\", \"Flesch.Kincaid\", \"FOG\"))"))
}

# ---- Zipf's law and stop words -------------------------------------------------------------------------------------
tq_zipf <- function(pp, p) {
  raw <- quanteda::tokens_tolower(quanteda::tokens(pp$corpus, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE))
  fr <- quanteda.textstats::textstat_frequency(quanteda::dfm(raw))
  z <- data.frame(rank = fr$rank, frequency = fr$frequency)
  fit <- stats::lm(log10(frequency) ~ log10(rank), data = z)
  slope <- unname(stats::coef(fit)[2]); r2 <- summary(fit)$r.squared
  sw <- stopwords::stopwords(pp$options$language, source = "snowball")
  fr$stop <- fr$feature %in% sw
  share <- sum(fr$frequency[fr$stop]) / sum(fr$frequency)
  plot <- tq_png(function() ggplot2::ggplot(transform(z, stop = fr$stop), ggplot2::aes(log10(rank), log10(frequency))) + ggplot2::geom_point(ggplot2::aes(colour = stop), alpha = .7, size = 1.6) +
                   ggplot2::geom_abline(intercept = stats::coef(fit)[1], slope = slope, linetype = "dashed") + ggplot2::scale_colour_manual(values = c(`FALSE` = "#0e7490", `TRUE` = "#c2410c"), labels = c("Content word", "Stop word"), name = NULL) +
                   ggplot2::labs(x = "log10(rank)", y = "log10(frequency)", title = sprintf("Zipf's law: slope = %.2f (ideal −1), R² = %.2f", slope, r2)) + tq_theme())
  topsw <- utils::head(fr[fr$stop, c("feature", "frequency", "rank")], 20)
  steps <- vapply(pp$steps, `[[`, "", "step"); i_sw <- grep("stop words", steps)
  eff <- if (length(i_sw)) data.frame(measure = c("Tokens", "Types"), before = c(pp$steps[[i_sw - 1]]$tokens, pp$steps[[i_sw - 1]]$types), after = c(pp$steps[[i_sw]]$tokens, pp$steps[[i_sw]]$types)) else NULL
  list(tables = c(list(list(id = "zipf", title = "Zipf's law (log10 frequency on log10 rank)", columns = list("statistic", "value"),
                            rows = list(list(statistic = "Slope", value = slope), list(statistic = "Intercept", value = unname(stats::coef(fit)[1])), list(statistic = "R²", value = r2), list(statistic = "Types", value = nrow(z)),
                                        list(statistic = "Share of tokens that are stop words", value = share))),
                       c(list(id = "stop", title = "Most frequent stop words"), tq_table(topsw))),
                  if (!is.null(eff)) list(c(list(id = "stopeff", title = "Effect of removing stop words"), tq_table(eff)))),
       plots = list(list(title = "Zipf plot", png = plot)),
       code = c("fr <- textstat_frequency(dfm(tokens_tolower(tokens(corp, remove_punct = TRUE))))", "fit <- lm(log10(frequency) ~ log10(rank), data = fr); coef(fit)   # slope near -1 under Zipf's law",
                sprintf("mean(fr$feature %%in%% stopwords(%s))", deparse(pp$options$language))))
}

# ---- Sentiment (sentimentr + lexicon) ------------------------------------------------------------------------------
TQ_LEXICONS <- list(
  jockers_rinker = list(label = "Jockers–Rinker (sentimentr default)", licence = "GPL-3 (lexicon package)", obj = "hash_sentiment_jockers_rinker"),
  huliu = list(label = "Hu & Liu (Bing Liu opinion lexicon)", licence = "Free for research with citation (Hu & Liu, 2004)", obj = "hash_sentiment_huliu"),
  loughran_mcdonald = list(label = "Loughran–McDonald (finance)", licence = "Free for academic research", obj = "hash_sentiment_loughran_mcdonald"),
  nrc = list(label = "NRC (Mohammad & Turney)", licence = "Research and non-commercial use only", obj = "hash_sentiment_nrc"),
  sentiword = list(label = "SentiWordNet", licence = "CC BY-SA 4.0", obj = "hash_sentiment_sentiword"),
  senticnet = list(label = "SenticNet", licence = "Non-commercial use (see sentic.net)", obj = "hash_sentiment_senticnet"))

tq_sentiment <- function(pp, p) {
  lx <- if (!is.null(TQ_LEXICONS[[p$lexicon %||% ""]])) p$lexicon else "jockers_rinker"
  key <- getExportedValue("lexicon", TQ_LEXICONS[[lx]]$obj)
  texts <- vapply(pp$docs, `[[`, "", "text")
  sents <- sentimentr::get_sentences(texts)
  sb <- sentimentr::sentiment_by(sents, polarity_dt = key)
  ss <- sentimentr::sentiment(sents, polarity_dt = key)
  df <- data.frame(document = vapply(pp$docs, `[[`, "", "name"), words = sb$word_count, sentiment = sb$ave_sentiment, sd = sb$sd, stringsAsFactors = FALSE)
  st <- as.data.frame(ss); st$sentence <- unlist(sents)[seq_len(nrow(st))]
  st$document <- df$document[st$element_id]
  st <- st[!is.na(st$sentiment) & nzchar(trimws(st$sentence)), ]
  pos <- utils::head(st[order(-st$sentiment), c("document", "sentence", "sentiment")], 5)
  neg <- utils::head(st[order(st$sentiment), c("document", "sentence", "sentiment")], 5)
  grp <- p$attribute %||% ""
  tables <- list(c(list(id = "sent", title = sprintf("Sentiment by document (%s)", TQ_LEXICONS[[lx]]$label), notes = list("Average sentence polarity with valence shifters (negators, amplifiers, de-amplifiers, adversative conjunctions); 0 = neutral.")), tq_table(df)),
                 c(list(id = "pos", title = "Most positive sentences"), tq_table(pos)), c(list(id = "neg", title = "Most negative sentences"), tq_table(neg)))
  plot <- if (nzchar(grp)) {
    g <- tq_group(pp, grp); df$group <- g
    by <- stats::aggregate(sentiment ~ group, df, function(v) c(mean = mean(v), se = if (length(v) > 1) stats::sd(v) / sqrt(length(v)) else NA, n = length(v)))
    bt <- data.frame(group = by$group, mean = by$sentiment[, "mean"], se = by$sentiment[, "se"], n = by$sentiment[, "n"])
    tables <- c(tables, list(c(list(id = "sentg", title = paste("Mean document sentiment by", grp)), tq_table(bt))))
    tq_png(function() ggplot2::ggplot(bt, ggplot2::aes(group, mean)) + ggplot2::geom_col(fill = "#0e7490") + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se), width = .2) +
             ggplot2::geom_hline(yintercept = 0) + ggplot2::labs(x = grp, y = "Mean sentiment (±1 SE)", title = paste("Sentiment by", grp)) + tq_theme())
  } else tq_png(function() ggplot2::ggplot(st, ggplot2::aes(sentiment)) + ggplot2::geom_histogram(bins = 25, fill = "#0e7490", colour = "white") + ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
                  ggplot2::labs(x = "Sentence sentiment", y = "Sentences", title = "Distribution of sentence sentiment") + tq_theme())
  list(tables = tables, plots = list(list(title = "Sentiment", png = plot)), licence = TQ_LEXICONS[[lx]]$licence, lexicon = TQ_LEXICONS[[lx]]$label,
       lexicons = lapply(names(TQ_LEXICONS), function(k) list(id = k, label = TQ_LEXICONS[[k]]$label, licence = TQ_LEXICONS[[k]]$licence)),
       code = c("library(sentimentr)", "sents <- get_sentences(texts)", sprintf("sentiment_by(sents, polarity_dt = lexicon::%s)", TQ_LEXICONS[[lx]]$obj)))
}

# ---- Parts of speech, noun phrases, candidate entities (udpipe) ------------------------------------------------------
TQ_UDPIPE <- c(en = "english-ewt", fr = "french-gsd", de = "german-gsd", es = "spanish-gsd", pt = "portuguese-bosque", it = "italian-isdt", nl = "dutch-alpino", ru = "russian-syntagrus")
tq_model_dir <- function() { d <- file.path(Sys.getenv("PATHLAB_DATA", file.path(path.expand("~"), ".pathlab")), "udpipe"); dir.create(d, recursive = TRUE, showWarnings = FALSE); d }
tq_model_file <- function(lang) { m <- TQ_UDPIPE[[lang]]; if (is.null(m)) return(NULL); f <- list.files(tq_model_dir(), pattern = paste0("^", m, ".*\\.udpipe$"), full.names = TRUE); if (length(f)) f[1] else NULL }
tq_download_model <- function(lang) {
  m <- TQ_UDPIPE[[lang]]; if (is.null(m)) api_error(400, "text_model", "No udpipe model for this language.")
  r <- tryCatch(udpipe::udpipe_download_model(language = m, model_dir = tq_model_dir()), error = function(e) api_error(502, "text_model_download", paste("The model could not be downloaded:", sanitize_message(conditionMessage(e)))))
  list(file = basename(r$file_model), language = m)
}
tq_pos <- function(pp, p) {
  lang <- pp$options$language
  mf <- tq_model_file(lang)
  if (is.null(mf)) api_error(428, "text_model_needed", sprintf("The %s language model for udpipe is not on the server yet (about 16 MB).", TQ_UDPIPE[[lang]] %||% lang), "Download it once; it is then reused.")
  model <- udpipe::udpipe_load_model(mf)
  texts <- vapply(pp$docs, `[[`, "", "text"); names(texts) <- vapply(pp$docs, `[[`, "", "id")
  if (sum(nchar(texts)) > 2e6) api_error(413, "text_too_long", "Parts of speech are limited to 2 million characters at a time; select fewer documents.")
  an <- as.data.frame(udpipe::udpipe_annotate(model, x = texts, doc_id = names(texts)))
  up <- as.data.frame(table(upos = an$upos), stringsAsFactors = FALSE); up <- up[order(-up$Freq), ]; names(up)[2] <- "tokens"
  top_of <- function(tag) { x <- an[an$upos == tag, ]; t <- sort(table(tolower(x$lemma)), decreasing = TRUE); data.frame(lemma = names(utils::head(t, 15)), frequency = as.integer(utils::head(t, 15))) }
  an$phrase_tag <- udpipe::as_phrasemachine(an$upos, type = "upos")
  np <- udpipe::keywords_phrases(an$phrase_tag, term = tolower(an$token), pattern = "(A|N)*N(P+D*(A|N)*N)*", is_regex = TRUE, detailed = FALSE)
  np <- utils::head(np[np$ngram > 1 & np$freq > 1, c("keyword", "ngram", "freq")], 25)
  # Candidate named entities: runs of proper nouns (PROPN); a GenAI provider can classify them later (decision 6.2).
  pr <- an$upos == "PROPN"; runs <- rle(pr); ends <- cumsum(runs$lengths); starts <- ends - runs$lengths + 1
  ent <- unlist(lapply(which(runs$values), function(i) paste(an$token[starts[i]:ends[i]], collapse = " ")))
  et <- if (length(ent)) { t <- sort(table(ent), decreasing = TRUE); data.frame(entity = names(utils::head(t, 25)), mentions = as.integer(utils::head(t, 25))) } else data.frame(entity = character(0), mentions = integer(0))
  plot <- tq_png(function() ggplot2::ggplot(utils::head(up, 12), ggplot2::aes(stats::reorder(upos, tokens), tokens)) + ggplot2::geom_col(fill = "#7c3aed") + ggplot2::coord_flip() +
                   ggplot2::labs(x = "Universal POS tag", y = "Tokens", title = "Parts of speech") + tq_theme())
  list(tables = list(c(list(id = "upos", title = "Parts of speech (Universal Dependencies)"), tq_table(up)), c(list(id = "nouns", title = "Most frequent nouns (lemmas)"), tq_table(top_of("NOUN"))),
                     c(list(id = "verbs", title = "Most frequent verbs (lemmas)"), tq_table(top_of("VERB"))), c(list(id = "adj", title = "Most frequent adjectives (lemmas)"), tq_table(top_of("ADJ"))),
                     c(list(id = "np", title = "Noun phrases (phrase machine)"), tq_table(np)), c(list(id = "ent", title = "Candidate named entities (proper-noun sequences)"), tq_table(et))),
       plots = list(list(title = "Parts of speech", png = plot)), model = basename(mf),
       code = c("library(udpipe)", sprintf("model <- udpipe_load_model(udpipe_download_model(%s)$file_model)", deparse(TQ_UDPIPE[[lang]])), "an <- as.data.frame(udpipe_annotate(model, x = texts))", "table(an$upos)",
                "keywords_phrases(as_phrasemachine(an$upos, type = \"upos\"), term = an$token, pattern = \"(A|N)*N(P+D*(A|N)*N)*\", is_regex = TRUE)"))
}

# ---- Topic models ---------------------------------------------------------------------------------------------------
tq_topics <- function(pp, p) {
  k <- min(max(as.integer(p$k %||% 5L), 2L), 40L); seed <- as.integer(p[["seed"]] %||% 2026L); method <- p$method %||% "lda"
  eff_rows <- NULL
  dm <- quanteda::dfm_subset(pp$dfm, quanteda::ntoken(pp$dfm) > 0)
  if (quanteda::ndoc(dm) < 3) api_error(422, "text_topics", "Topic models need at least three non-empty documents.")
  names_d <- quanteda::docvars(dm, "doc_name")
  if (method == "seeded") {
    seeds <- p$seeds %||% list()
    if (!length(seeds)) api_error(400, "text_seeds", "Enter seed words for at least one topic (e.g. cost: fee*, price*, expensive).")
    dict <- quanteda::dictionary(lapply(seeds, function(s) as.character(unlist(s))))
    set.seed(seed)
    m <- seededlda::textmodel_seededlda(dm, dict, residual = isTRUE(p$residual), max_iter = 1000L)
    terms <- seededlda::terms(m, 10); theta <- m$theta
  } else if (method == "stm") {
    cov <- p$covariate %||% ""
    st <- quanteda::convert(dm, to = "stm")
    prev <- if (nzchar(cov) && cov %in% names(st$meta)) { st$meta[[cov]] <- factor(st$meta[[cov]]); stats::as.formula(paste("~", sprintf("`%s`", cov))) } else NULL
    m <- stm::stm(st$documents, st$vocab, K = k, prevalence = prev, data = st$meta, seed = seed, verbose = FALSE, max.em.its = 200L)
    lab <- stm::labelTopics(m, n = 10)
    terms <- t(lab$frex); colnames(terms) <- paste("Topic", seq_len(k)); theta <- m$theta
    if (!is.null(prev)) {
      eff <- stm::estimateEffect(stats::as.formula(paste("1:", k, deparse(prev))), m, metadata = st$meta)
      es <- summary(eff)$tables
      eff_rows <- unlist(lapply(seq_along(es), function(i) { tb <- es[[i]]; lapply(rownames(tb)[-1], function(r) list(topic = paste("Topic", i), term = r, estimate = tb[r, 1], se = tb[r, 2], p = tb[r, 4])) }), recursive = FALSE)
    }
  } else {
    set.seed(seed)
    m <- topicmodels::LDA(quanteda::convert(dm, to = "topicmodels"), k = k, method = "Gibbs", control = list(seed = seed, iter = 1000L, burnin = 200L))
    terms <- topicmodels::terms(m, 10); theta <- topicmodels::posterior(m)$topics
  }
  colnames(theta) <- colnames(terms) %||% paste("Topic", seq_len(ncol(theta)))
  tt <- data.frame(rank = seq_len(nrow(terms)), terms, check.names = FALSE)
  share <- colMeans(theta)
  rep_docs <- lapply(seq_len(ncol(theta)), function(j) { o <- order(-theta[, j])[1:min(3, nrow(theta))]; list(topic = colnames(theta)[j], share = share[j], topTerms = paste(utils::head(terms[, j], 6), collapse = ", "),
                                                                                                      documents = paste(sprintf("%s (%.2f)", names_d[o], theta[o, j]), collapse = "; ")) })
  td <- data.frame(document = names_d, round(theta, 4), check.names = FALSE)
  plot <- tq_png(function() {
    long <- utils::stack(as.data.frame(terms[1:min(8, nrow(terms)), , drop = FALSE], stringsAsFactors = FALSE))
    long$rank <- rep(seq_len(min(8, nrow(terms))), ncol(terms))
    ggplot2::ggplot(long, ggplot2::aes(x = ind, y = -rank, label = values)) + ggplot2::geom_text(size = 3.2) + ggplot2::labs(x = NULL, y = NULL, title = "Top terms per topic") +
      tq_theme() + ggplot2::theme(axis.text.y = ggplot2::element_blank(), panel.grid = ggplot2::element_blank(), axis.text.x = ggplot2::element_text(face = "bold"))
  }, max(6, 1.15 * ncol(terms)), 3.6)
  list(tables = c(list(list(id = "topics", title = "Topics: share, top terms and most representative documents", columns = list("topic", "share", "topTerms", "documents"), rows = rep_docs),
                       c(list(id = "terms", title = if (method == "stm") "Top terms per topic (FREX)" else "Top terms per topic"), tq_table(tt)),
                       c(list(id = "theta", title = "Topic proportions per document"), tq_table(td))),
                  if (!is.null(eff_rows)) list(list(id = "effect", title = paste("Effect of", p$covariate, "on topic prevalence (STM)"), columns = list("topic", "term", "estimate", "se", "p"), rows = eff_rows))),
       plots = list(list(title = "Top terms per topic", png = plot)), method = method,
       code = switch(method,
         seeded = c("library(seededlda)", "dict <- dictionary(list(cost = c(\"fee*\", \"price*\"), ...))", sprintf("set.seed(%d); m <- textmodel_seededlda(dfmat, dict)", seed), "terms(m, 10)"),
         stm = c("library(stm)", "st <- convert(dfmat, to = \"stm\")", sprintf("m <- stm(st$documents, st$vocab, K = %d%s, data = st$meta, seed = %d)", k, if (nzchar(p$covariate %||% "")) sprintf(", prevalence = ~ %s", p$covariate) else "", seed), "labelTopics(m)"),
         c("library(topicmodels)", sprintf("m <- LDA(convert(dfmat, to = \"topicmodels\"), k = %d, method = \"Gibbs\", control = list(seed = %d, iter = 1000, burnin = 200))", k, seed), "terms(m, 10); posterior(m)$topics")))
}

#' Choice of the number of topics (stm::searchK): held-out likelihood, semantic coherence, exclusivity, residuals.
tq_searchk <- function(pp, p) {
  ks <- sort(unique(pmin(pmax(as.integer(unlist(p$ks %||% list(3, 5, 7, 10))), 2L), 30L)))
  dm <- quanteda::dfm_subset(pp$dfm, quanteda::ntoken(pp$dfm) > 0)
  if (quanteda::ndoc(dm) < 10) api_error(422, "text_topics", "Choosing the number of topics needs at least ten documents.")
  st <- quanteda::convert(dm, to = "stm")
  sk <- stm::searchK(st$documents, st$vocab, K = ks, data = st$meta, init.type = "Spectral", verbose = FALSE, heldout.seed = as.integer(p[["seed"]] %||% 2026L))
  r <- as.data.frame(lapply(sk$results, unlist))
  df <- data.frame(K = r$K, heldout = r$heldout, coherence = r$semcoh, exclusivity = r$exclus, residual = r$residual)
  plot <- tq_png(function() {
    long <- data.frame(K = rep(df$K, 4), value = c(df$heldout, df$coherence, df$exclusivity, df$residual), measure = rep(c("Held-out likelihood", "Semantic coherence", "Exclusivity", "Residuals"), each = nrow(df)))
    ggplot2::ggplot(long, ggplot2::aes(K, value)) + ggplot2::geom_line(colour = "#0e7490") + ggplot2::geom_point(colour = "#0e7490") + ggplot2::facet_wrap(~measure, scales = "free_y") +
      ggplot2::labs(title = "Diagnostics by number of topics") + tq_theme()
  }, 7.5, 5)
  list(tables = list(c(list(id = "searchk", title = "Number of topics: diagnostics (stm::searchK)", notes = list("Prefer K with high held-out likelihood and coherence, high exclusivity and low residuals.")), tq_table(df))),
       plots = list(list(title = "Diagnostics", png = plot)), code = c(sprintf("searchK(st$documents, st$vocab, K = c(%s), data = st$meta)", paste(ks, collapse = ", "))))
}

# ---- Extractive summaries (textrank) --------------------------------------------------------------------------------
tq_summary <- function(pp, p) {
  n <- min(max(as.integer(p$n %||% 3L), 1L), 10L)
  rows <- list()
  for (d in pp$docs) {
    s <- unlist(sentimentr::get_sentences(d$text)); s <- s[nchar(trimws(s)) > 0]
    if (length(s) <= n) { rows[[length(rows) + 1]] <- list(document = d$name, rank = 1L, sentence = paste(s, collapse = " ")); next }
    sent <- data.frame(textrank_id = seq_along(s), sentence = s, stringsAsFactors = FALSE)
    words <- do.call(rbind, lapply(seq_along(s), function(i) { w <- tolower(unlist(strsplit(s[i], "[^[:alnum:]']+"))); w <- w[nchar(w) > 2 & !w %in% pp$stopwords]; if (length(w)) data.frame(textrank_id = i, word = w) else NULL }))
    if (is.null(words) || !nrow(words)) next
    tr <- textrank::textrank_sentences(data = sent, terminology = words)
    top <- utils::head(tr$sentences[order(-tr$sentences$textrank), ], n)
    top <- top[order(top$textrank_id), ]
    for (i in seq_len(nrow(top))) rows[[length(rows) + 1]] <- list(document = d$name, rank = i, sentence = top$sentence[i])
  }
  list(tables = list(list(id = "summary", title = sprintf("Extractive summary: %d key sentence(s) per document (TextRank)", n), columns = list("document", "rank", "sentence"), rows = rows)), plots = list(),
       code = c("library(textrank)", "tr <- textrank_sentences(data = sentences, terminology = words)   # words: content words per sentence", sprintf("summary(tr, n = %d)", n)))
}

# ---- User dictionaries (content analysis: MAXQDA's MAXDictio, NVivo / ATLAS.ti dictionaries) ---------------------------
#' Counts of the user's categories in each document: quanteda::tokens_lookup on the words of the text (lower-cased,
#' punctuation removed, before stop-word removal and stemming, so every term the user listed can match). Terms may be
#' phrases ("cost of living") and globs (fee*). params$categories: list(list(name, terms)); params$attribute: optional groups.
tq_dictionary <- function(pp, p) {
  cats <- Filter(function(c) nzchar(trimws(as.character(c$name %||% ""))), p$categories %||% list())
  terms <- lapply(cats, function(c) { t <- trimws(tolower(unlist(strsplit(paste(unlist(c$terms), collapse = ","), "[,;\n]")))); unique(t[nzchar(t)]) })
  keep <- lengths(terms) > 0; cats <- cats[keep]; terms <- terms[keep]
  if (!length(cats)) api_error(422, "text_dictionary", "Add at least one category with terms (category: term, term*, …).")
  names(terms) <- make.unique(vapply(cats, function(c) trimws(as.character(c$name)), ""))
  dict <- quanteda::dictionary(terms)
  toks <- quanteda::tokens_tolower(quanteda::tokens(pp$corpus, remove_punct = TRUE, remove_symbols = TRUE))
  quanteda::docvars(toks) <- quanteda::docvars(pp$corpus)
  hits <- quanteda::dfm(quanteda::tokens_lookup(toks, dict, valuetype = "glob", case_insensitive = TRUE), tolower = FALSE)   # keep the category names as written
  m <- as.matrix(hits); words <- quanteda::ntoken(toks)
  cn <- names(terms)
  m <- m[, intersect(cn, colnames(m)), drop = FALSE]; for (c in setdiff(cn, colnames(m))) m <- cbind(m, stats::setNames(matrix(0L, nrow(m), 1), c)); m <- m[, cn, drop = FALSE]
  doc_rows <- lapply(seq_len(nrow(m)), function(i) c(list(document = pp$docs[[i]]$name, words = unname(words[i])),
                                                    stats::setNames(as.list(unname(m[i, ])), cn), stats::setNames(as.list(unname(round(100 * m[i, ] / max(1, words[i]), 2))), paste0(cn, " (%)"))))
  matched <- vapply(cn, function(c) {
    if (!sum(m[, c])) return("")   # no matches: nothing to list
    tk <- quanteda::tokens_keep(toks, dict[c], valuetype = "glob", case_insensitive = TRUE)
    f <- quanteda.textstats::textstat_frequency(quanteda::dfm(tk), n = 8)
    if (!nrow(f)) "" else paste(sprintf("%s (%d)", f$feature, f$frequency), collapse = ", ")
  }, "")
  cat_rows <- lapply(seq_along(cn), function(j) list(category = cn[j], terms = paste(terms[[j]], collapse = ", "), matches = sum(m[, j]), documents = sum(m[, j] > 0),
                                                     `% of words` = round(100 * sum(m[, j]) / max(1, sum(words)), 2), `most frequent matches` = matched[[j]]))
  tables <- list(c(list(id = "dict_categories", title = "Dictionary categories"), list(columns = list("category", "terms", "matches", "documents", "% of words", "most frequent matches"), rows = cat_rows)),
                 c(list(id = "dict_documents", title = "Categories per document (counts and % of words)"), list(columns = as.list(c("document", "words", cn, paste0(cn, " (%)"))), rows = doc_rows)))
  grp <- p$attribute %||% ""
  plot_df <- data.frame(category = cn, pct = vapply(seq_along(cn), function(j) 100 * sum(m[, j]) / max(1, sum(words)), 0))
  if (nzchar(grp)) {
    g <- tq_group(pp, grp)
    gm <- rowsum(m, g); gw <- tapply(words, g, sum)[rownames(gm)]
    tables[[3]] <- list(id = "dict_groups", title = sprintf("Categories by %s (%% of words)", grp), columns = as.list(c(grp, "words", cn)),
                        rows = lapply(seq_len(nrow(gm)), function(i) c(stats::setNames(list(rownames(gm)[i]), grp), list(words = unname(gw[i])), stats::setNames(as.list(unname(round(100 * gm[i, ] / max(1, gw[i]), 2))), cn))))
    plot_df <- do.call(rbind, lapply(seq_len(nrow(gm)), function(i) data.frame(category = cn, group = rownames(gm)[i], pct = 100 * gm[i, ] / max(1, gw[i]))))
  }
  bar <- tq_png(function() {
    g <- if (nzchar(grp)) ggplot2::ggplot(plot_df, ggplot2::aes(x = category, y = pct, fill = group)) + ggplot2::geom_col(position = "dodge") + ggplot2::labs(fill = grp)
         else ggplot2::ggplot(plot_df, ggplot2::aes(x = stats::reorder(category, pct), y = pct)) + ggplot2::geom_col(fill = "#0e7490") + ggplot2::coord_flip()
    g + ggplot2::labs(x = NULL, y = "% of words", title = "Dictionary categories") + tq_theme()
  })
  dict_code <- paste0("dict <- dictionary(list(", paste(sprintf("%s = c(%s)", deparse_names(cn), vapply(terms, function(t) paste(sprintf("\"%s\"", t), collapse = ", "), "")), collapse = ", "), "))")
  list(tables = tables, plots = list(list(title = "Dictionary categories", png = bar)),
       code = c("library(quanteda)", dict_code, "toks <- tokens_tolower(tokens(corp, remove_punct = TRUE, remove_symbols = TRUE))",
                "hits <- dfm(tokens_lookup(toks, dict, valuetype = \"glob\"))   # counts per document and category", "convert(hits, to = \"data.frame\")", "100 * as.matrix(hits) / ntoken(toks)   # % of words"))
}
deparse_names <- function(x) ifelse(make.names(x) == x, x, sprintf("`%s`", x))

text_analyze <- function(proj, b) {
  kind <- as.character(b$kind %||% "")
  f <- switch(kind, frequency = tq_frequency, keyness = tq_keyness, collocations = tq_collocations, lexdiv = tq_lexdiv, zipf = tq_zipf, sentiment = tq_sentiment,
              pos = tq_pos, topics = tq_topics, searchk = tq_searchk, summary = tq_summary, dictionary = tq_dictionary, api_error(400, "text_kind", "Unknown text analysis."))
  pp <- tq_prep(proj, b$documentIds, b$options)
  t0 <- Sys.time()
  warns <- character(0)
  r <- withCallingHandlers(tryCatch(f(pp, b$params %||% list()), 
                                    error = function(e) if (inherits(e, "pathlab_error")) stop(e) else api_error(422, "text_failed", paste("The analysis could not be computed:", sanitize_message(conditionMessage(e))))),
                           warning = function(w) { warns <<- c(warns, sanitize_message(conditionMessage(w))); invokeRestart("muffleWarning") })
  r$kind <- kind; r$nDocs <- length(pp$docs); r$warnings <- as.list(unique(warns)); r$seconds <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  r$rCode <- paste(c("# Preprocessing (see the Preprocessing tab):", text_preprocess_code(pp$options), "", "# Analysis:", r$code), collapse = "\n"); r$code <- NULL
  r
}
