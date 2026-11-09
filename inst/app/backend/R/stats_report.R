# Journal-ready report of a Statistics results document (APA 7): a "Statistical analysis" paragraph for the Method
# section, a Results section with one subsection per analysis (narrative text written from the numbers R computed,
# numbered tables and figures), and a reference list built from citation() of the packages used. Output: HTML
# (manuscript styling) or Word (.docx, officer + flextable). Nothing here computes a statistic: every number is read
# from the stored results; the text only reports and labels them (effect-size labels follow cited conventions).

# ---- APA formatting -------------------------------------------------------------------------------------------
rp_ok <- function(x) !is.null(x) && length(x) == 1 && !is.na(suppressWarnings(as.numeric(x))) && !(is.character(x) && !nzchar(trimws(x)))
rp_n <- function(x, d = 2, bounded = FALSE) if (!rp_ok(x)) "NA" else pt_num(as.numeric(x), d, bounded = bounded)
rp_int <- function(x) if (!rp_ok(x)) "NA" else formatC(round(as.numeric(x)), format = "d", big.mark = ",")
rp_df <- function(x) if (!rp_ok(x)) "NA" else if (abs(as.numeric(x) - round(as.numeric(x))) < 1e-8) rp_int(x) else rp_n(x, 2)
rp_p <- function(p) { if (!rp_ok(p)) return("<i>p</i> = NA"); v <- as.numeric(p); if (v < 0.001) "<i>p</i> &lt; .001" else paste0("<i>p</i> = ", pt_num(v, 3, bounded = TRUE)) }
rp_ci <- function(lo, hi, level, d = 2, bounded = FALSE) if (rp_ok(lo) && rp_ok(hi)) sprintf("%g%% CI [%s, %s]", level, rp_n(lo, d, bounded), rp_n(hi, d, bounded)) else NULL
rp_sig <- function(p, alpha) rp_ok(p) && as.numeric(p) < alpha
rp_list <- function(x) { x <- unlist(x); if (length(x) <= 1) paste(x, collapse = "") else if (length(x) == 2) paste(x, collapse = " and ") else paste0(paste(x[-length(x)], collapse = ", "), ", and ", x[length(x)]) }
rp_bf <- function(b) { if (!rp_ok(b)) return("NA"); b <- as.numeric(b); if (b >= 1000 || b < 0.001) { e <- floor(log10(b)); sprintf("%s &times; 10<sup>%d</sup>", formatC(b / 10^e, format = "f", digits = 2), e) } else formatC(b, format = "f", digits = 2) }
rp_cell <- function(r, k) { v <- r[[k]]; if (is.null(v) || (is.character(v) && (!nzchar(v) || v %in% c("—", "NaN")))) NULL else v }
rp_esc <- function(x) html_escape(as.character(x))

# Effect-size conventions (Cohen, 1988; Lee & Wagenmakers, 2013 for Bayes factors).
rp_mag <- function(v, type) {
  if (!rp_ok(v)) return("")
  a <- abs(as.numeric(v))
  cut <- switch(type, d = c(0.2, 0.5, 0.8), r = c(0.1, 0.3, 0.5), eta = c(0.01, 0.06, 0.14), c(0.2, 0.5, 0.8))
  if (a < cut[1]) "negligible" else if (a < cut[2]) "small" else if (a < cut[3]) "medium" else "large"
}
rp_bf_word <- function(b) {
  if (!rp_ok(b)) return("")
  b <- as.numeric(b); h <- if (b >= 1) "the alternative hypothesis" else "the null hypothesis"; x <- if (b >= 1) b else 1 / b
  w <- if (x < 3) "anecdotal" else if (x < 10) "moderate" else if (x < 30) "strong" else if (x < 100) "very strong" else "extreme"
  paste(w, "evidence for", h)
}

# ---- References -------------------------------------------------------------------------------------------------
STATIC_REFS <- list(
  cohen1988 = list(cite = "Cohen, 1988", ref = "Cohen, J. (1988). <i>Statistical power analysis for the behavioral sciences</i> (2nd ed.). Lawrence Erlbaum Associates."),
  west1995 = list(cite = "West et al., 1995", ref = "West, S. G., Finch, J. F., &amp; Curran, P. J. (1995). Structural equation models with nonnormal variables: Problems and remedies. In R. H. Hoyle (Ed.), <i>Structural equation modeling: Concepts, issues, and applications</i> (pp. 56&ndash;75). Sage."),
  delacre2017 = list(cite = "Delacre et al., 2017", ref = "Delacre, M., Lakens, D., &amp; Leys, C. (2017). Why psychologists should by default use Welch&rsquo;s <i>t</i>-test instead of Student&rsquo;s <i>t</i>-test. <i>International Review of Social Psychology, 30</i>(1), 92&ndash;101. https://doi.org/10.5334/irsp.82"),
  nunnally1994 = list(cite = "Nunnally &amp; Bernstein, 1994", ref = "Nunnally, J. C., &amp; Bernstein, I. H. (1994). <i>Psychometric theory</i> (3rd ed.). McGraw-Hill."),
  wilcox2017 = list(cite = "Wilcox, 2017", ref = "Wilcox, R. R. (2017). <i>Introduction to robust estimation and hypothesis testing</i> (4th ed.). Academic Press."),
  efron1993 = list(cite = "Efron &amp; Tibshirani, 1993", ref = "Efron, B., &amp; Tibshirani, R. J. (1993). <i>An introduction to the bootstrap</i>. Chapman &amp; Hall."),
  lakens2017 = list(cite = "Lakens, 2017", ref = "Lakens, D. (2017). Equivalence tests: A practical primer for <i>t</i> tests, correlations, and meta-analyses. <i>Social Psychological and Personality Science, 8</i>(4), 355&ndash;362. https://doi.org/10.1177/1948550617697177"),
  landis1977 = list(cite = "Landis &amp; Koch, 1977", ref = "Landis, J. R., &amp; Koch, G. G. (1977). The measurement of observer agreement for categorical data. <i>Biometrics, 33</i>(1), 159&ndash;174. https://doi.org/10.2307/2529310"),
  cohen1960 = list(cite = "Cohen, 1960", ref = "Cohen, J. (1960). A coefficient of agreement for nominal scales. <i>Educational and Psychological Measurement, 20</i>(1), 37&ndash;46. https://doi.org/10.1177/001316446002000104"),
  fleiss1971 = list(cite = "Fleiss, 1971", ref = "Fleiss, J. L. (1971). Measuring nominal scale agreement among many raters. <i>Psychological Bulletin, 76</i>(5), 378&ndash;382. https://doi.org/10.1037/h0031619"),
  krippendorff2004 = list(cite = "Krippendorff, 2004", ref = "Krippendorff, K. (2004). <i>Content analysis: An introduction to its methodology</i> (2nd ed.). Sage."),
  koo2016 = list(cite = "Koo &amp; Li, 2016", ref = "Koo, T. K., &amp; Li, M. Y. (2016). A guideline of selecting and reporting intraclass correlation coefficients for reliability research. <i>Journal of Chiropractic Medicine, 15</i>(2), 155&ndash;163. https://doi.org/10.1016/j.jcm.2016.02.012"),
  mcgraw1996 = list(cite = "McGraw &amp; Wong, 1996", ref = "McGraw, K. O., &amp; Wong, S. P. (1996). Forming inferences about some intraclass correlation coefficients. <i>Psychological Methods, 1</i>(1), 30&ndash;46. https://doi.org/10.1037/1082-989X.1.1.30"),
  lee2013 = list(cite = "Lee &amp; Wagenmakers, 2013", ref = "Lee, M. D., &amp; Wagenmakers, E.-J. (2013). <i>Bayesian cognitive modeling: A practical course</i>. Cambridge University Press."),
  hausman1978 = list(cite = "Hausman, 1978", ref = "Hausman, J. A. (1978). Specification tests in econometrics. <i>Econometrica, 46</i>(6), 1251&ndash;1271. https://doi.org/10.2307/1913827"),
  wooldridge2010 = list(cite = "Wooldridge, 2010", ref = "Wooldridge, J. M. (2010). <i>Econometric analysis of cross section and panel data</i> (2nd ed.). MIT Press."),
  baltagi2021 = list(cite = "Baltagi, 2021", ref = "Baltagi, B. H. (2021). <i>Econometric analysis of panel data</i> (6th ed.). Springer."),
  arellano1991 = list(cite = "Arellano & Bond, 1991", ref = "Arellano, M., & Bond, S. (1991). Some tests of specification for panel data: Monte Carlo evidence and an application to employment equations. <i>The Review of Economic Studies, 58</i>(2), 277&ndash;297. https://doi.org/10.2307/2297968"),
  blundell1998 = list(cite = "Blundell & Bond, 1998", ref = "Blundell, R., & Bond, S. (1998). Initial conditions and moment restrictions in dynamic panel data models. <i>Journal of Econometrics, 87</i>(1), 115&ndash;143. https://doi.org/10.1016/S0304-4076(98)00009-8"),
  windmeijer2005 = list(cite = "Windmeijer, 2005", ref = "Windmeijer, F. (2005). A finite sample correction for the variance of linear efficient two-step GMM estimators. <i>Journal of Econometrics, 126</i>(1), 25&ndash;51. https://doi.org/10.1016/j.jeconom.2004.02.005"),
  roodman2009 = list(cite = "Roodman, 2009", ref = "Roodman, D. (2009). A note on the theme of too many instruments. <i>Oxford Bulletin of Economics and Statistics, 71</i>(1), 135&ndash;158. https://doi.org/10.1111/j.1468-0084.2008.00542.x"),
  levin2002 = list(cite = "Levin et al., 2002", ref = "Levin, A., Lin, C.-F., & Chu, C.-S. J. (2002). Unit root tests in panel data: Asymptotic and finite-sample properties. <i>Journal of Econometrics, 108</i>(1), 1&ndash;24. https://doi.org/10.1016/S0304-4076(01)00098-7"),
  im2003 = list(cite = "Im et al., 2003", ref = "Im, K. S., Pesaran, M. H., & Shin, Y. (2003). Testing for unit roots in heterogeneous panels. <i>Journal of Econometrics, 115</i>(1), 53&ndash;74. https://doi.org/10.1016/S0304-4076(03)00092-7"),
  maddala1999 = list(cite = "Maddala & Wu, 1999", ref = "Maddala, G. S., & Wu, S. (1999). A comparative study of unit root tests with panel data and a new simple test. <i>Oxford Bulletin of Economics and Statistics, 61</i>(S1), 631&ndash;652. https://doi.org/10.1111/1468-0084.0610s1631"),
  choi2001 = list(cite = "Choi, 2001", ref = "Choi, I. (2001). Unit root tests for panel data. <i>Journal of International Money and Finance, 20</i>(2), 249&ndash;272. https://doi.org/10.1016/S0261-5606(00)00048-6"),
  hadri2000 = list(cite = "Hadri, 2000", ref = "Hadri, K. (2000). Testing for stationarity in heterogeneous panel data. <i>The Econometrics Journal, 3</i>(2), 148&ndash;161. https://doi.org/10.1111/1368-423X.00043"),
  pathlab = list(cite = pathlab_in_text(), ref = pathlab_citation("html"))
)

#' In-text citation and APA reference of R or an R package, from its citation() entry.
rp_pkg_ref <- function(pkg) {
  b <- tryCatch(pkg_citation(pkg), error = function(e) NULL)
  if (is.null(b)) return(NULL)
  au <- b$author
  fam <- vapply(seq_along(au), function(i) { f <- paste(au[i]$family, collapse = " "); if (nzchar(f)) f else paste(au[i]$given, collapse = " ") }, "")
  ini <- vapply(seq_along(au), function(i) {
    if (!length(au[i]$family)) return("")
    g <- unlist(strsplit(paste(au[i]$given, collapse = " "), "[ -]+")); g <- g[nzchar(g)]
    paste(paste0(substr(g, 1, 1), "."), collapse = " ")
  }, "")
  names_apa <- ifelse(nzchar(ini), paste0(fam, ", ", ini), fam)
  auth <- if (length(names_apa) == 1) names_apa else if (length(names_apa) <= 20) paste0(paste(names_apa[-length(names_apa)], collapse = ", "), ", & ", names_apa[length(names_apa)]) else names_apa[1]
  year <- b$year %||% format(Sys.Date(), "%Y")
  intext <- if (length(fam) == 1) fam else if (length(fam) == 2) paste(fam[1], "&", fam[2]) else paste(fam[1], "et al.")
  title <- gsub("[{}]", "", b$title %||% pkg)
  ver <- if (pkg != "R") tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) "") else ""
  url <- b$url %||% ""
  pub <- b$organization %||% b$publisher %||% ""
  ref <- paste0(rp_esc(auth), if (grepl("\\.$", auth)) "" else ".", " (", year, "). <i>", rp_esc(title), "</i>",
                if (nzchar(ver)) paste0(" (R package version ", ver, ")") else "", " [Computer software].",
                if (nzchar(pub)) paste0(" ", rp_esc(pub), ".") else "", if (nzchar(url)) paste0(" ", rp_esc(strsplit(url, ",\\s*")[[1]][1])) else "")
  list(cite = paste0(rp_esc(intext), ", ", year), ref = ref, version = ver)
}

# ---- Descriptives in long form (both jamovi layouts) -------------------------------------------------------------
desc_long <- function(res) {
  out <- list()
  for (t in res$tables) {
    if (identical(t$id, "descriptives") && !is.null(t$rows[[1]]$statKey)) {
      vars <- setdiff(vapply(t$columns, `[[`, "", "name"), c("stat", "group"))
      for (r in t$rows) for (v in vars) out[[length(out) + 1]] <- list(var = v, group = r$group %||% "", key = r$statKey, label = r$stat, value = rp_cell(r, v))
    } else if (identical(t$id, "descriptivesT")) {
      cols <- t$columns
      for (r in t$rows) for (cc in cols) if (!cc$name %in% c("vars", "var")) {
        if (identical(cc$type, "text")) next
        out[[length(out) + 1]] <- list(var = r$vars %||% r$var, group = "", key = cc$name, label = cc$title, value = rp_cell(r, cc$name))
      }
    }
  }
  out
}
desc_get <- function(long, var, key, group = "") { for (x in long) if (identical(x$var, var) && identical(x$key, key) && identical(x$group, group)) return(x$value); NULL }

# ---- Narratives per procedure ------------------------------------------------------------------------------------
# Each returns list(text = HTML paragraphs, refs = reference keys, method = sentences for the Statistical analysis section).
rp_tab <- function(nums) if (length(nums) == 1) paste("Table", nums) else paste("Tables", nums[1], "to", nums[length(nums)])

narr_descriptives <- function(res, ctx) {
  o <- res$options; long <- desc_long(res)
  vars <- unlist(o$vars); split <- unlist(o$splitBy)
  cont <- Filter(function(v) rp_ok(desc_get(long, v, "mean")), vars)
  txt <- character(0); refs <- character(0)
  lead <- sprintf("Descriptive statistics%s are presented in %s.", if (length(split)) paste0(" by ", rp_esc(rp_list(split))) else "", rp_tab(ctx$tables))
  if (length(cont) && !length(split)) {
    parts <- vapply(cont, function(v) {
      m <- desc_get(long, v, "mean"); s <- desc_get(long, v, "sd"); lo <- desc_get(long, v, "min"); hi <- desc_get(long, v, "max"); n <- desc_get(long, v, "n")
      paste0(rp_esc(v), " (<i>M</i> = ", rp_n(m), if (rp_ok(s)) paste0(", <i>SD</i> = ", rp_n(s)) else "",
             if (rp_ok(lo) && rp_ok(hi)) paste0(", range ", rp_n(lo), "&ndash;", rp_n(hi)) else "", if (rp_ok(n)) paste0(", <i>n</i> = ", rp_int(n)) else "", ")")
    }, "")
    lead <- paste(lead, if (length(parts) <= 6) paste0("Mean scores were: ", rp_list(parts), ".") else {
      ms <- vapply(cont, function(v) as.numeric(desc_get(long, v, "mean")), 0)
      sprintf("Means ranged from %s (%s) to %s (%s).", rp_n(min(ms)), rp_esc(cont[which.min(ms)]), rp_n(max(ms)), rp_esc(cont[which.max(ms)]))
    })
  }
  txt <- c(txt, lead)
  sk <- Filter(function(v) rp_ok(desc_get(long, v, "skew")), cont); ku <- Filter(function(v) rp_ok(desc_get(long, v, "kurt")), cont)
  if (length(sk) && !length(split)) {
    s <- vapply(sk, function(v) as.numeric(desc_get(long, v, "skew")), 0); k <- vapply(ku, function(v) as.numeric(desc_get(long, v, "kurt")), 0)
    bad <- unique(c(sk[abs(s) > 2], ku[abs(k) > 7]))
    txt <- c(txt, paste0("Skewness ranged from ", rp_n(min(s)), " to ", rp_n(max(s)), if (length(k)) paste0(" and kurtosis from ", rp_n(min(k)), " to ", rp_n(max(k))) else "", ". ",
      if (!length(bad)) "All values were within &plusmn;2 (skewness) and &plusmn;7 (kurtosis), indicating no severe departure from normality (West et al., 1995)."
      else paste0("Values beyond &plusmn;2 (skewness) or &plusmn;7 (kurtosis) indicate a substantial departure from normality for ", rp_esc(rp_list(bad)), " (West et al., 1995).")))
    refs <- c(refs, "west1995")
  }
  sw <- Filter(function(v) rp_ok(desc_get(long, v, "sw")), cont)
  if (length(sw) && !length(split)) {
    p <- vapply(sw, function(v) as.numeric(desc_get(long, v, "sw")), 0)
    w <- vapply(sw, function(v) as.numeric(desc_get(long, v, "sww") %||% NA), 0)
    sig <- sw[p < ctx$alpha]
    det <- function(vv) vapply(vv, function(v) paste0(rp_esc(v), " (<i>W</i> = ", rp_n(w[v], 2, TRUE), ", ", rp_p(p[v]), ")"), "")
    det_or <- function(vv) { x <- det(vv); if (length(x) <= 1) x else if (length(x) == 2) paste(x, collapse = " or ") else paste0(paste(x[-length(x)], collapse = ", "), ", or ", x[length(x)]) }
    n_max <- max(vapply(sw, function(v) as.numeric(desc_get(long, v, "n") %||% 0), 0))
    txt <- c(txt, paste0(if (length(sig)) paste0("Shapiro&ndash;Wilk tests indicated a departure from normality for ", rp_list(det(sig)), if (length(sig) < length(sw)) paste0(" but not for ", rp_list(det(setdiff(sw, sig)))) else "", ".")
                         else paste0("Shapiro&ndash;Wilk tests did not indicate a departure from normality for ", det_or(sw), "."),
                         if (length(sig) && n_max >= 200) " Because this test is sensitive to small deviations in large samples, skewness and kurtosis were also considered." else ""))
  }
  nt <- Filter(function(t) identical(t$id, "normality"), res$tables)
  if (length(nt)) {
    bad <- unique(vapply(Filter(function(r) rp_sig(r$swP, ctx$alpha) || rp_sig(r$ksP, ctx$alpha) || rp_sig(r$adP, ctx$alpha), nt[[1]]$rows), function(r) as.character(r$var), ""))
    txt <- c(txt, paste0("Shapiro&ndash;Wilk, Lilliefors-corrected Kolmogorov&ndash;Smirnov and Anderson&ndash;Darling tests ", if (length(bad)) paste0("indicated non-normality for ", rp_esc(rp_list(bad)), " in at least one test") else "did not indicate non-normality", "."))
  }
  list(text = paste(txt, collapse = " "), refs = refs, method = "Variables were described with means, standard deviations and ranges; normality was assessed with skewness, kurtosis and Shapiro&ndash;Wilk tests.")
}

rp_suffixes <- function(row) unique(sub("^.*\\[(.*)\\]$", "\\1", grep("\\[", names(row), value = TRUE)))
rp_es_txt <- function(row, sfx, level) {
  es <- rp_cell(row, paste0("es[", sfx, "]")); type <- rp_cell(row, paste0("esType[", sfx, "]")) %||% "Cohen's d"
  if (!rp_ok(es)) return(NULL)
  sym <- if (grepl("biserial", type)) "<i>r</i><sub>rb</sub>" else "<i>d</i>"
  ci <- rp_ci(rp_cell(row, paste0("ciles[", sfx, "]")), rp_cell(row, paste0("ciues[", sfx, "]")), level, 2, grepl("biserial", type))
  paste0(sym, " = ", rp_n(es, 2, grepl("biserial", type)), if (!is.null(ci)) paste0(", ", ci) else "", ", a ", rp_mag(es, if (grepl("biserial", type)) "r" else "d"), " effect")
}

narr_ttestIS <- function(res, ctx) {
  o <- res$options; tt <- Filter(function(t) t$id == "ttest", res$tables)[[1]]
  desc <- Filter(function(t) t$id == "desc", res$tables); eqv <- Filter(function(t) t$id == "eqv", res$tables); norm <- Filter(function(t) t$id == "norm", res$tables)
  lvl <- o$ciWidth %||% 95; refs <- "cohen1988"; out <- character(0)
  for (row in tt$rows) {
    v <- rp_cell(row, "var[stud]") %||% rp_cell(row, "var[welc]") %||% rp_cell(row, "var[mann]"); sx <- rp_suffixes(row)
    ev <- if (length(eqv)) Filter(function(r) identical(r$name, v), eqv[[1]]$rows) else list()
    welch_needed <- length(ev) && rp_sig(ev[[1]]$p, ctx$alpha)
    main <- if ("welc" %in% sx && (welch_needed || !"stud" %in% sx)) "welc" else if ("stud" %in% sx) "stud" else NULL
    d <- if (length(desc)) Filter(function(r) identical(r$dep, v), desc[[1]]$rows) else list()
    g <- if (length(d) >= 2) d else NULL   # one row per group
    grp <- function(i) if (is.null(g)) NULL else paste0(rp_esc(g[[i]]$group), " (<i>M</i> = ", rp_n(g[[i]]$mean), ", <i>SD</i> = ", rp_n(g[[i]]$sd), ", <i>n</i> = ", rp_int(g[[i]]$num), ")")
    gv <- rp_esc(unlist(o$group)[1])
    if (!is.null(main)) {
      p <- rp_cell(row, paste0("p[", main, "]")); md <- rp_cell(row, paste0("md[", main, "]")); sig <- rp_sig(p, ctx$alpha)
      dirn <- if (!sig) paste0(" did not differ significantly between ", if (!is.null(g)) paste(grp(1), "and", grp(2)) else paste("the two groups of", gv))
              else if (!is.null(g)) paste0(" was significantly ", if (as.numeric(md) > 0) "higher" else "lower", " for ", grp(1), " than for ", grp(2))
              else paste0(" differed significantly between the two groups of ", gv)
      ci <- rp_ci(rp_cell(row, paste0("cil[", main, "]")), rp_cell(row, paste0("ciu[", main, "]")), lvl)
      es <- rp_es_txt(row, main, lvl)
      out <- c(out, paste0("An independent-samples ", if (main == "welc") "Welch&rsquo;s " else "", "<i>t</i>-test showed that ", rp_esc(v), dirn, ", <i>t</i>(", rp_df(rp_cell(row, paste0("df[", main, "]"))), ") = ",
                           rp_n(rp_cell(row, paste0("stat[", main, "]"))), ", ", rp_p(p), if (rp_ok(md)) paste0(", <i>M</i><sub>diff</sub> = ", rp_n(md), if (!is.null(ci)) paste0(", ", ci) else "") else "",
                           if (!is.null(es)) paste0(", ", es) else "", "."))
      if (length(ev)) { out <- c(out, paste0("Levene&rsquo;s test ", if (welch_needed) "indicated unequal variances" else "did not indicate unequal variances", ", <i>F</i>(", rp_df(ev[[1]]$df), ", ", rp_df(ev[[1]]$df2), ") = ", rp_n(ev[[1]]$f), ", ", rp_p(ev[[1]]$p),
                                                if (welch_needed && main == "welc") "; Welch&rsquo;s correction was therefore applied (Delacre et al., 2017)." else ".")); if (welch_needed) refs <- c(refs, "delacre2017") }
      nv <- if (length(norm)) Filter(function(r) identical(r$name, v), norm[[1]]$rows) else list()
      if (length(nv)) out <- c(out, paste0("The Shapiro&ndash;Wilk test ", if (rp_sig(nv[[1]]$p, ctx$alpha)) "indicated a departure from normality" else "did not indicate a departure from normality", " (<i>W</i> = ", rp_n(nv[[1]]$w, 2, TRUE), ", ", rp_p(nv[[1]]$p), ")."))
    }
    if ("mann" %in% sx) {
      pm <- rp_cell(row, "p[mann]"); same <- is.null(main) || rp_sig(pm, ctx$alpha) == rp_sig(rp_cell(row, paste0("p[", main, "]")), ctx$alpha)
      es <- rp_es_txt(row, "mann", lvl)
      out <- c(out, paste0(if (is.null(main)) paste0("A Mann&ndash;Whitney <i>U</i> test for ", rp_esc(v), " showed ") else paste0("A Mann&ndash;Whitney <i>U</i> test ", if (same) "led to the same conclusion" else "led to a different conclusion", ", "),
                           "<i>U</i> = ", rp_int(rp_cell(row, "stat[mann]")), ", ", rp_p(pm), if (!is.null(es)) paste0(", ", es) else "", "."))
    }
  }
  meth <- paste0("Differences between two independent groups were tested with ", rp_list(c(if (isTRUE(o$students)) "Student&rsquo;s", if (isTRUE(o$welchs)) "Welch&rsquo;s", if (isTRUE(o$mann)) "Mann&ndash;Whitney <i>U</i>")), " tests", if (isTRUE(o$eqv)) "; homogeneity of variances was checked with Levene&rsquo;s test" else "", ".")
  list(text = paste(out, collapse = " "), refs = refs, method = meth)
}

narr_ttestPS <- function(res, ctx) {
  o <- res$options; tt <- Filter(function(t) t$id == "ttest", res$tables)[[1]]; desc <- Filter(function(t) t$id == "desc", res$tables)
  lvl <- o$ciWidth %||% 95; out <- character(0)
  dget <- function(v) { if (!length(desc)) return(NULL); r <- Filter(function(r) identical(r$name, v), desc[[1]]$rows); if (length(r)) r[[1]] else NULL }
  for (row in tt$rows) {
    sx <- rp_suffixes(row); main <- if ("stud" %in% sx) "stud" else NULL
    v1 <- rp_cell(row, "var1[stud]") %||% rp_cell(row, "var1[wilc]"); v2 <- rp_cell(row, "var2[stud]") %||% rp_cell(row, "var2[wilc]")
    d1 <- dget(v1); d2 <- dget(v2)
    lab <- function(v, d) paste0(rp_esc(v), if (!is.null(d)) paste0(" (<i>M</i> = ", rp_n(d$m), ", <i>SD</i> = ", rp_n(d$sd), ")") else "")
    if (!is.null(main)) {
      p <- rp_cell(row, "p[stud]"); md <- rp_cell(row, "md[stud]"); es <- rp_es_txt(row, "stud", lvl); ci <- rp_ci(rp_cell(row, "cil[stud]"), rp_cell(row, "ciu[stud]"), lvl)
      out <- c(out, paste0("A paired-samples <i>t</i>-test showed that ", lab(v1, d1), if (rp_sig(p, ctx$alpha)) paste0(" was significantly ", if (as.numeric(md) > 0) "higher" else "lower", " than ") else " did not differ significantly from ", lab(v2, d2),
                           ", <i>t</i>(", rp_df(rp_cell(row, "df[stud]")), ") = ", rp_n(rp_cell(row, "stat[stud]")), ", ", rp_p(p), if (rp_ok(md)) paste0(", <i>M</i><sub>diff</sub> = ", rp_n(md), if (!is.null(ci)) paste0(", ", ci) else "") else "", if (!is.null(es)) paste0(", ", es) else "", "."))
    }
    if ("wilc" %in% sx) { es <- rp_es_txt(row, "wilc", lvl)
      out <- c(out, paste0("A Wilcoxon signed-rank test", if (is.null(main)) paste0(" comparing ", rp_esc(v1), " and ", rp_esc(v2)) else "", " gave <i>W</i> = ", rp_int(rp_cell(row, "stat[wilc]")), ", ", rp_p(rp_cell(row, "p[wilc]")), if (!is.null(es)) paste0(", ", es) else "", ".")) }
  }
  list(text = paste(out, collapse = " "), refs = "cohen1988", method = "Paired measurements were compared with paired-samples <i>t</i>-tests and Wilcoxon signed-rank tests.")
}

narr_ttestOneS <- function(res, ctx) {
  o <- res$options; tt <- Filter(function(t) t$id == "ttest", res$tables)[[1]]; desc <- Filter(function(t) t$id == "descriptives", res$tables)
  tv <- o$testValue %||% 0; out <- character(0); lvl <- o$ciWidth %||% 95
  for (row in tt$rows) {
    v <- rp_cell(row, "var[stud]") %||% rp_cell(row, "var[wilc]"); if (is.null(rp_cell(row, "stat[stud]"))) next
    d <- if (length(desc)) Filter(function(r) identical(r$name, v), desc[[1]]$rows) else list()
    p <- rp_cell(row, "p[stud]"); t <- as.numeric(rp_cell(row, "stat[stud]")); es <- rp_es_txt(row, "stud", lvl)
    out <- c(out, paste0(rp_esc(v), if (length(d)) paste0(" (<i>M</i> = ", rp_n(d[[1]]$mean), ", <i>SD</i> = ", rp_n(d[[1]]$sd), ")") else "",
                         if (rp_sig(p, ctx$alpha)) paste0(" was significantly ", if (t > 0) "higher" else "lower", " than the test value of ") else " did not differ significantly from the test value of ", rp_n(tv),
                         ", <i>t</i>(", rp_df(rp_cell(row, "df[stud]")), ") = ", rp_n(t), ", ", rp_p(p), if (!is.null(es)) paste0(", ", es) else "", "."))
  }
  list(text = paste(out, collapse = " "), refs = "cohen1988", method = "Means were compared with a reference value using one-sample <i>t</i>-tests.")
}

# Significant pairs of a jamovi post hoc matrix table (rows .name[md], columns <level>[md] / <level>[p]).
rp_posthoc_matrix <- function(t, alpha) {
  out <- character(0)
  for (r in t$rows) {
    a <- r[[".name[md]"]]; if (is.null(a)) next
    for (k in grep("\\[p\\]$", names(r), value = TRUE)) {
      if (startsWith(k, ".")) next
      b <- sub("\\[p\\]$", "", k); p <- rp_cell(r, k); md <- rp_cell(r, paste0(b, "[md]"))
      if (rp_sig(p, alpha) && rp_ok(md)) out <- c(out, paste0(rp_esc(if (as.numeric(md) > 0) a else b), " scored higher than ", rp_esc(if (as.numeric(md) > 0) b else a), " (<i>M</i><sub>diff</sub> = ", rp_n(abs(as.numeric(md))), ", ", rp_p(p), ")"))
    }
  }
  out
}

narr_anovaOneW <- function(res, ctx) {
  o <- res$options; at <- Filter(function(t) t$id == "anova", res$tables)[[1]]
  desc <- Filter(function(t) t$id == "desc", res$tables); eqv <- Filter(function(t) t$id == "eqv", res$tables)
  ph <- Filter(function(t) grepl("Post-Hoc", t$title), res$tables); gv <- rp_esc(unlist(o$group)[1]); out <- character(0); refs <- character(0)
  for (row in at$rows) {
    v <- row$dep; sx <- rp_suffixes(row)
    ev <- if (length(eqv)) Filter(function(r) identical(r$dep, v), eqv[[1]]$rows) else list()
    unequal <- length(ev) && rp_sig(ev[[1]]$p, ctx$alpha)
    main <- if ("welch" %in% sx && (unequal || !"fisher" %in% sx)) "welch" else "fisher"
    p <- rp_cell(row, paste0("p[", main, "]"))
    out <- c(out, paste0("A one-way ", if (main == "welch") "Welch&rsquo;s " else "", "ANOVA showed that ", rp_esc(v), if (rp_sig(p, ctx$alpha)) " differed significantly" else " did not differ significantly", " across ", gv,
                         ", <i>F</i>(", rp_df(rp_cell(row, paste0("df1[", main, "]"))), ", ", rp_df(rp_cell(row, paste0("df2[", main, "]"))), ") = ", rp_n(rp_cell(row, paste0("F[", main, "]"))), ", ", rp_p(p), "."))
    if (length(ev)) { out <- c(out, paste0("Levene&rsquo;s test ", if (unequal) "indicated unequal variances" else "did not indicate unequal variances", ", <i>F</i>(", rp_df(ev[[1]]$df1), ", ", rp_df(ev[[1]]$df2), ") = ", rp_n(ev[[1]]$F), ", ", rp_p(ev[[1]]$p), if (unequal && main == "welch") "; Welch&rsquo;s <i>F</i> is therefore reported (Delacre et al., 2017)." else ".")); if (unequal) refs <- c(refs, "delacre2017") }
    d <- if (length(desc)) Filter(function(r) identical(r$dep, v), desc[[1]]$rows) else list()
    if (length(d) >= 2) { ms <- vapply(d, function(r) as.numeric(r$mean), 0)
      out <- c(out, paste0("Group means ranged from ", rp_n(min(ms)), " (", rp_esc(d[[which.min(ms)]]$group), ") to ", rp_n(max(ms)), " (", rp_esc(d[[which.max(ms)]]$group), ").")) }
    pt <- Filter(function(t) grepl(paste0("\\b", gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", v), "$"), t$title), ph)
    if (length(pt)) { s <- rp_posthoc_matrix(pt[[1]], ctx$alpha); meth <- sub(" Post-Hoc.*$", "", pt[[1]]$title)
      out <- c(out, if (length(s)) paste0(meth, " post hoc tests showed that ", rp_list(s), "; no other pairwise differences were significant.") else paste0(meth, " post hoc tests showed no significant pairwise differences.")) }
  }
  list(text = paste(out, collapse = " "), refs = refs, method = "Differences among more than two groups were tested with one-way ANOVA (Welch&rsquo;s <i>F</i> when variances were unequal), followed by post hoc comparisons.")
}

rp_ftable_text <- function(t, dep, alpha, what = "effect") {
  rows <- t$rows; nm <- function(r) as.character(r$name %||% r[["name[none]"]] %||% "")
  getc <- function(r, k) rp_cell(r, k) %||% rp_cell(r, paste0(k, "[none]"))
  res_row <- Filter(function(r) grepl("^Residual", nm(r)), rows)
  df2 <- if (length(res_row)) getc(res_row[[1]], "df") else NULL
  out <- character(0)
  for (r in rows) {
    if (grepl("^Residual", nm(r)) || !rp_ok(getc(r, "F"))) next
    p <- getc(r, "p"); es <- getc(r, "etaSqP") %||% getc(r, "partEta"); es2 <- getc(r, "etaSq"); om <- getc(r, "omegaSq")
    term <- nm(r); inter <- grepl("[:✻*]", term)
    lab <- if (inter) paste("interaction between", rp_list(trimws(strsplit(term, "[:✻*]")[[1]]))) else paste(what, "of", term)
    out <- c(out, paste0("There was ", if (rp_sig(p, alpha)) "a significant " else "no significant ", rp_esc(lab), if (!is.null(dep)) paste0(" on ", rp_esc(dep)) else "",
                         ", <i>F</i>(", rp_df(getc(r, "df")), if (!is.null(df2)) paste0(", ", rp_df(df2)) else "", ") = ", rp_n(getc(r, "F")), ", ", rp_p(p),
                         if (rp_ok(es)) paste0(", &eta;<sup>2</sup><sub><i>p</i></sub> = ", rp_n(es, 2, TRUE), " (", rp_mag(es, "eta"), ")") else if (rp_ok(es2)) paste0(", &eta;<sup>2</sup> = ", rp_n(es2, 2, TRUE), " (", rp_mag(es2, "eta"), ")") else "",
                         if (rp_ok(om)) paste0(", &omega;<sup>2</sup> = ", rp_n(om, 2, TRUE)) else "", "."))
  }
  out
}

narr_ANOVA <- function(res, ctx) {
  o <- res$options; out <- character(0); refs <- "cohen1988"
  main <- Filter(function(t) t$id == "main", res$tables)
  if (length(main)) out <- c(out, rp_ftable_text(main[[1]], o$dep, ctx$alpha, if (res$proc == "ancova") "effect" else "main effect"))
  homo <- Filter(function(t) t$id == "homo", res$tables)
  if (length(homo) && length(homo[[1]]$rows)) { h <- homo[[1]]$rows[[1]]
    out <- c(out, paste0("Levene&rsquo;s test ", if (rp_sig(h$p, ctx$alpha)) "indicated unequal variances" else "did not indicate unequal variances", ", <i>F</i>(", rp_df(h$df1), ", ", rp_df(h$df2), ") = ", rp_n(h$F), ", ", rp_p(h$p), ".")) }
  for (t in Filter(function(t) grepl("^Post Hoc", t$title), res$tables)) {
    pk <- intersect(c("ptukey", "pbonferroni", "pholm", "pscheffe", "pnone"), vapply(t$columns, `[[`, "", "name"))[1]
    if (is.na(pk)) next
    keys <- vapply(t$columns, `[[`, "", "name"); a <- keys[1]; b <- keys[3]
    sig <- Filter(function(r) rp_sig(r[[pk]], ctx$alpha), t$rows)
    lab <- c(ptukey = "Tukey", pbonferroni = "Bonferroni", pholm = "Holm", pscheffe = "Scheff&eacute;", pnone = "Unadjusted")[[pk]]
    s <- vapply(sig, function(r) paste0(rp_esc(r[[a]]), " vs. ", rp_esc(r[[b]]), " (<i>M</i><sub>diff</sub> = ", rp_n(r$md), ", ", rp_p(r[[pk]]), ")"), "")
    out <- c(out, paste0(lab, "-corrected post hoc comparisons for ", rp_esc(sub("^Post Hoc Comparisons - ", "", t$title)), if (length(s)) paste0(" were significant for ", rp_list(s), ".") else " showed no significant differences."))
  }
  for (t in Filter(function(t) t$id %in% c("rmTable", "bsTable"), res$tables)) out <- c(out, rp_ftable_text(t, NULL, ctx$alpha, if (t$id == "rmTable") "within-subjects effect" else "between-subjects effect"))
  list(text = paste(out, collapse = " "), refs = refs, method = if (res$proc == "ancova") "Group differences adjusted for covariates were tested with ANCOVA; partial &eta;<sup>2</sup> was reported as the effect size."
                                                     else if (res$proc == "anovaRM") "Repeated measures were analysed with repeated-measures ANOVA." else "Effects of categorical factors were tested with ANOVA; &eta;<sup>2</sup> or partial &eta;<sup>2</sup> was reported as the effect size.")
}

narr_anovaNP <- function(res, ctx) {
  o <- res$options; out <- character(0)
  t <- Filter(function(t) t$id == "table", res$tables)
  if (res$proc == "anovaRMNP" && length(t)) { r <- t[[1]]$rows[[1]]
    return(list(text = paste0("A Friedman test ", if (rp_sig(r$p, ctx$alpha)) "indicated significant differences" else "did not indicate significant differences", " among ", rp_esc(rp_list(unlist(o$measures))), ", &chi;<sup>2</sup>(", rp_df(r$df), ") = ", rp_n(r$stat), ", ", rp_p(r$p), "."),
                refs = character(0), method = "Repeated measurements were compared with the Friedman test.")) }
  if (length(t)) for (r in t[[1]]$rows) out <- c(out, paste0("A Kruskal&ndash;Wallis test showed that ", rp_esc(r$name), if (rp_sig(r$p, ctx$alpha)) " differed significantly" else " did not differ significantly", " across ", rp_esc(unlist(o$group)[1]),
    ", &chi;<sup>2</sup>(", rp_df(r$df), ") = ", rp_n(r$chiSq), ", ", rp_p(r$p), if (rp_ok(r$es)) paste0(", &epsilon;<sup>2</sup> = ", rp_n(r$es, 2, TRUE), " (", rp_mag(r$es, "eta"), ")") else "", "."))
  for (pt in Filter(function(t) grepl("^Pairwise", t$title), res$tables)) {
    s <- vapply(Filter(function(r) rp_sig(r$p, ctx$alpha), pt$rows), function(r) paste0(rp_esc(r$p1), " vs. ", rp_esc(r$p2), " (<i>W</i> = ", rp_n(r$W), ", ", rp_p(r$p), ")"), "")
    out <- c(out, paste0("Dwass&ndash;Steel&ndash;Critchlow&ndash;Fligner pairwise comparisons for ", rp_esc(sub("^Pairwise comparisons - ", "", pt$title)), if (length(s)) paste0(" were significant for ", rp_list(s), ".") else " showed no significant differences."))
  }
  list(text = paste(out, collapse = " "), refs = "cohen1988", method = "Differences among independent groups were also tested non-parametrically with the Kruskal&ndash;Wallis test.")
}

narr_corr <- function(res, ctx) {
  o <- res$options; t <- Filter(function(t) t$id == "matrix", res$tables)[[1]]; vars <- unlist(o$vars); lvl <- o$ciWidth %||% 95
  type <- if (isTRUE(o$spearman) && !isTRUE(o$pearson %||% TRUE)) "rho" else if (isTRUE(o$kendall) && !isTRUE(o$pearson %||% TRUE)) "tau" else "r"
  sym <- c(r = "<i>r</i>", rho = "<i>r</i><sub>s</sub>", tau = "&tau;")[[type]]
  pairs <- list()
  for (row in t$rows) {
    a <- row[[paste0(".name[", type, "]")]]; if (is.null(a)) next
    for (b in vars) { v <- rp_cell(row, paste0(b, "[", type, "]")); if (!rp_ok(v) || identical(a, b)) next
      pairs[[length(pairs) + 1]] <- list(a = a, b = b, v = as.numeric(v), p = rp_cell(row, paste0(b, "[", type, "p]")), df = rp_cell(row, paste0(b, "[", type, "df]")),
                                         lo = rp_cell(row, paste0(b, "[", type, "cil]")), hi = rp_cell(row, paste0(b, "[", type, "ciu]"))) }
  }
  part <- res$proc == "corrPart"
  one <- function(x) paste0(rp_esc(x$a), " and ", rp_esc(x$b), ", ", sym, if (rp_ok(x$df)) paste0("(", rp_df(x$df), ")") else "", " = ", rp_n(x$v, 2, TRUE), if (!is.null(rp_ci(x$lo, x$hi, lvl, 2, TRUE))) paste0(", ", rp_ci(x$lo, x$hi, lvl, 2, TRUE)) else "", ", ", rp_p(x$p))
  sig <- Filter(function(x) rp_sig(x$p, ctx$alpha), pairs)
  intro <- if (part) paste0("Controlling for ", rp_esc(rp_list(unlist(o$controls))), ", ") else ""
  txt <- if (length(pairs) <= 6) {
    s <- vapply(pairs, function(x) paste0(if (rp_sig(x$p, ctx$alpha)) paste0("a significant ", if (x$v > 0) "positive" else "negative", " (", rp_mag(x$v, "r"), ") ") else "a non-significant ", if (part) "partial " else "", "correlation between ", one(x)), "")
    paste0(intro, if (nzchar(intro)) "there was " else "There was ", rp_list(s), ".")
  } else {
    best <- pairs[[which.max(vapply(pairs, function(x) abs(x$v), 0))]]
    paste0(intro, length(sig), " of the ", length(pairs), " ", if (part) "partial " else "", "correlations were statistically significant (", rp_tab(ctx$tables), "). The strongest was between ", one(best), ".")
  }
  list(text = txt, refs = "cohen1988", method = paste0(if (part) "Partial correlations" else c(r = "Pearson", rho = "Spearman", tau = "Kendall")[[type]], if (part) "" else " correlations", " were computed between continuous variables."))
}

narr_contTables <- function(res, ctx) {
  o <- res$options; out <- character(0); a <- unlist(o$rows)[1]; b <- unlist(o$cols)[1]
  ch <- Filter(function(t) t$id == "chiSq", res$tables); nom <- Filter(function(t) t$id == "nom", res$tables); mc <- Filter(function(t) t$id == "test", res$tables)
  if (length(ch)) { r <- ch[[1]]$rows[[1]]; v <- if (length(nom)) rp_cell(nom[[1]]$rows[[1]], "v[cra]") else NULL
    out <- c(out, paste0("A chi-square test of independence ", if (rp_sig(r[["p[chiSq]"]], ctx$alpha)) "showed a significant association" else "showed no significant association", " between ", rp_esc(a), " and ", rp_esc(b),
                         ", &chi;<sup>2</sup>(", rp_df(r[["df[chiSq]"]]), ", <i>N</i> = ", rp_int(r[["value[N]"]]), ") = ", rp_n(r[["value[chiSq]"]]), ", ", rp_p(r[["p[chiSq]"]]),
                         if (rp_ok(v)) paste0(", Cram&eacute;r&rsquo;s <i>V</i> = ", rp_n(v, 2, TRUE), " (", rp_mag(v, "r"), ")") else "", ".")) }
  if (length(mc)) { r <- mc[[1]]$rows[[1]]; if (rp_ok(r[["value[mcn]"]])) out <- c(out, paste0("McNemar&rsquo;s test ", if (rp_sig(r[["p[mcn]"]], ctx$alpha)) "indicated a significant change" else "indicated no significant change", ", &chi;<sup>2</sup>(", rp_df(r[["df[mcn]"]]), ", <i>N</i> = ", rp_int(r[["value[n]"]]), ") = ", rp_n(r[["value[mcn]"]]), ", ", rp_p(r[["p[mcn]"]]), "."))
    else out <- c(out, paste0("McNemar&rsquo;s test could not be computed because there were no discordant pairs (", rp_tab(ctx$tables), ").")) }
  list(text = paste(out, collapse = " "), refs = "cohen1988", method = "Associations between categorical variables were tested with chi-square tests; Cram&eacute;r&rsquo;s <i>V</i> was reported as the effect size.")
}

narr_props <- function(res, ctx) {
  o <- res$options; out <- character(0)
  if (res$proc == "propTestN") {
    pr <- Filter(function(t) t$id == "props", res$tables); ts <- Filter(function(t) t$id == "tests", res$tables)
    lv <- if (length(pr)) vapply(pr[[1]]$rows, function(r) paste0(rp_esc(r$level), " (", rp_int(r[["count[obs]"]]), ", ", rp_n(as.numeric(r[["prop[obs]"]]) * 100, 1), "%)"), "") else character(0)
    if (length(ts)) { r <- ts[[1]]$rows[[1]]; n <- if (length(pr)) sum(vapply(pr[[1]]$rows, function(r) as.numeric(r[["count[obs]"]]), 0)) else NA
      out <- c(out, paste0("A chi-square goodness-of-fit test showed that the categories of ", rp_esc(o$var), if (rp_sig(r$p, ctx$alpha)) " were not equally frequent" else " did not differ significantly from the expected proportions",
                           ", &chi;<sup>2</sup>(", rp_df(r$df), if (!is.na(n)) paste0(", <i>N</i> = ", rp_int(n)) else "", ") = ", rp_n(r$chi), ", ", rp_p(r$p), if (length(lv)) paste0(": ", rp_list(lv)) else "", ".")) }
    return(list(text = paste(out, collapse = " "), refs = character(0), method = "Category frequencies were compared with expected proportions using chi-square goodness-of-fit tests."))
  }
  t <- Filter(function(t) t$id == "table", res$tables)
  if (length(t)) { tv <- o$testValue %||% 0.5
    for (r in t[[1]]$rows[seq(1, length(t[[1]]$rows), by = 2)]) out <- c(out, paste0("The proportion of ", rp_esc(r$level), " in ", rp_esc(r$var), " (", rp_int(r$count), " of ", rp_int(r$total), ", ", rp_n(r$prop, 2, TRUE), ")",
      if (rp_sig(r$p, ctx$alpha)) " differed significantly" else " did not differ significantly", " from ", rp_n(tv, 2, TRUE), " (binomial test, ", rp_p(r$p), ").")) }
  list(text = paste(out, collapse = " "), refs = character(0), method = "Proportions were tested against a reference value with exact binomial tests.")
}

narr_reliability <- function(res, ctx) {
  s <- Filter(function(t) t$id == "scale", res$tables); if (!length(s)) return(NULL)
  r <- s[[1]]$rows[[1]]; a <- rp_cell(r, "alpha"); w <- rp_cell(r, "omega"); items <- unlist(res$options$vars)
  lab <- function(x) { x <- as.numeric(x); if (x >= .9) "excellent" else if (x >= .8) "good" else if (x >= .7) "acceptable" else if (x >= .6) "questionable" else "poor" }
  v <- a %||% w
  txt <- paste0("The ", length(items), "-item scale (", rp_esc(rp_list(items)), ") showed ", lab(v), " internal consistency",
                if (rp_ok(a)) paste0(", Cronbach&rsquo;s &alpha; = ", rp_n(a, 2, TRUE)) else "", if (rp_ok(w)) paste0(", McDonald&rsquo;s &omega; = ", rp_n(w, 2, TRUE)) else "",
                if (as.numeric(v) >= .7) ", above the recommended threshold of .70 (Nunnally &amp; Bernstein, 1994)." else ", below the recommended threshold of .70 (Nunnally &amp; Bernstein, 1994).")
  it <- Filter(function(t) t$id == "items", res$tables)
  if (length(it)) { better <- Filter(function(x) rp_ok(rp_cell(x, "alpha")) && rp_ok(a) && as.numeric(rp_cell(x, "alpha")) > as.numeric(a), it[[1]]$rows)
    if (length(better)) txt <- paste(txt, paste0("Removing ", rp_esc(rp_list(vapply(better, function(x) as.character(x$name), ""))), " would increase &alpha;.")) }
  list(text = txt, refs = "nunnally1994", method = "Internal consistency was assessed with Cronbach&rsquo;s &alpha; and McDonald&rsquo;s &omega;.")
}

narr_kappa <- function(res, ctx) {
  k <- Filter(function(t) t$id == "kappa", res$tables); if (!length(k)) return(NULL)
  raters <- unlist(res$options$raters); level <- res$options$ciWidth %||% 95
  word <- function(x) tolower(kappa_label(as.numeric(x)))
  out <- character(0); refs <- "landis1977"
  for (r in k[[1]]$rows) {
    if (!rp_ok(rp_cell(r, "kappa"))) next
    name <- if (grepl("^Cohen", r$stat)) { refs <- c(refs, "cohen1960"); "Cohen&rsquo;s &kappa;" } else if (grepl("^Weighted", r$stat)) paste0("weighted &kappa; (", sub("^Weighted kappa \\((.*)\\)$", "\\1", r$stat), ")")
            else if (grepl("^Fleiss", r$stat)) { refs <- c(refs, "fleiss1971"); "Fleiss&rsquo; &kappa;" } else "Light&rsquo;s &kappa;"
    out <- c(out, paste0(if (!length(out)) paste0("Agreement between the ", length(raters), " raters (", rp_esc(rp_list(raters)), ") was ", word(r$kappa), ", ") else paste0("; ", name, " = "),
                         if (!length(out)) paste0(name, " = ") else "", rp_n(r$kappa, 2, TRUE),
                         if (!is.null(rp_ci(rp_cell(r, "lower"), rp_cell(r, "upper"), level))) paste0(", ", rp_ci(r$lower, r$upper, level, 2, TRUE)) else "",
                         if (rp_ok(rp_cell(r, "z"))) paste0(", <i>z</i> = ", rp_n(r$z, 2), ", ", rp_p(r$p)) else ""))
  }
  if (!length(out)) return(NULL)
  txt <- paste0(paste(out, collapse = ""), " (", rp_tab(ctx$tables), ").")
  ka <- Filter(function(t) t$id == "kripp", res$tables)
  if (length(ka) && rp_ok(rp_cell(ka[[1]]$rows[[1]], "alpha"))) { refs <- c(refs, "krippendorff2004")
    a <- as.numeric(ka[[1]]$rows[[1]]$alpha)
    txt <- paste0(txt, " Krippendorff&rsquo;s &alpha; was ", rp_n(a, 2, TRUE), if (a >= .8) ", above the .80 criterion for reliable coding" else if (a >= .667) ", which allows only tentative conclusions" else ", below the .667 minimum for reliable coding", " (Krippendorff, 2004).") }
  list(text = paste0(txt, " Kappa values were interpreted following Landis and Koch (1977)."), refs = unique(refs),
       method = "Inter-rater agreement for the categorical ratings was assessed with kappa statistics.")
}

narr_icc <- function(res, ctx) {
  t <- Filter(function(t) t$id == "icc", res$tables); if (!length(t)) return(NULL)
  rows <- t[[1]]$rows; raters <- unlist(res$options$raters); level <- res$options$ciWidth %||% 95
  model <- sub("^Intraclass Correlation: ", "", t[[1]]$title)
  part <- function(r, what) paste0("the ", what, " ICC was ", rp_n(r$icc, 2, TRUE), ", ", rp_ci(r$lower, r$upper, level, 2, TRUE), ", <i>F</i>(", rp_df(r$df1), ", ", rp_df(r$df2), ") = ", rp_n(r$f, 2), ", ", rp_p(r$p),
                                    ", indicating ", tolower(icc_label(as.numeric(r$icc))), " reliability")
  mp <- strsplit(model, ", ", fixed = TRUE)[[1]]
  txt <- paste0("Using a ", tolower(mp[1]), " model with ", mp[2], " (McGraw &amp; Wong, 1996) for the ", length(raters), " raters (", rp_esc(rp_list(raters)), "), ",
                part(rows[[1]], "single-rater"), "; ", part(rows[[2]], "average-rater"), " (", rp_tab(ctx$tables), "). Reliability was interpreted following Koo and Li (2016).")
  list(text = txt, refs = c("mcgraw1996", "koo2016"), method = "Inter-rater reliability of the numeric ratings was assessed with intraclass correlation coefficients.")
}

narr_bayes <- function(res, ctx) {
  out <- character(0)
  bc <- Filter(function(t) t$id == "bcorr", res$tables)
  if (length(bc)) for (r in bc[[1]]$rows) out <- c(out, paste0("For ", rp_esc(r$v1), " and ", rp_esc(r$v2), " (<i>r</i> = ", rp_n(r$r, 2, TRUE), ", <i>n</i> = ", rp_int(r$n), "), BF<sub>10</sub> = ", rp_bf(r$bf10), ", ", rp_bf_word(r$bf10), "."))
  ba <- Filter(function(t) t$id == "banova", res$tables)
  if (length(ba)) { rows <- Filter(function(r) !identical(r$model, "Null model"), ba[[1]]$rows)
    if (length(rows)) { best <- rows[[which.max(vapply(rows, function(r) as.numeric(r$bf10), 0))]]
      out <- c(out, paste0("Compared with the null model, the model with ", rp_esc(best$model), " had BF<sub>10</sub> = ", rp_bf(best$bf10), ", ", rp_bf_word(best$bf10), ".")) } }
  list(text = paste(out, collapse = " "), refs = "lee2013", method = "Bayes factors were computed with default priors and interpreted following Lee and Wagenmakers (2013).")
}

narr_survival <- function(res, ctx) {
  out <- character(0)
  ks <- Filter(function(t) t$id == "km_summary", res$tables)
  if (length(ks)) { s <- vapply(ks[[1]]$rows, function(r) paste0(rp_esc(r$group), " (", rp_int(r$events), " events of ", rp_int(r$n), ", median = ", if (rp_ok(r$median)) paste0(rp_n(r$median), if (rp_ok(r$lower)) paste0(", ", rp_ci(r$lower, r$upper %||% NA, res$options$ciWidth %||% 95) %||% "") else "") else "not reached", ")"), "")
    out <- c(out, paste0("Kaplan&ndash;Meier estimates of median survival were: ", rp_list(s), ".")) }
  lr <- Filter(function(t) t$id == "logrank", res$tables)
  if (length(lr)) { r <- lr[[1]]$rows[[1]]; out <- c(out, paste0("The log-rank test ", if (rp_sig(r$p, ctx$alpha)) "indicated that survival differed significantly between groups" else "did not indicate a difference in survival between groups", ", &chi;<sup>2</sup>(", rp_df(r$df), ") = ", rp_n(r$chisq), ", ", rp_p(r$p), ".")) }
  cx <- Filter(function(t) t$id == "cox", res$tables)
  if (length(cx)) for (r in cx[[1]]$rows) out <- c(out, paste0(rp_esc(r$term), if (rp_sig(r$p, ctx$alpha)) " was a significant predictor of the hazard" else " was not a significant predictor of the hazard", ", HR = ", rp_n(r$hr), if (rp_ok(r$lower)) paste0(", ", rp_ci(r$lower, r$upper, res$options$ciWidth %||% 95)) else "", ", ", rp_p(r$p), "."))
  list(text = paste(out, collapse = " "), refs = character(0), method = "Time-to-event data were analysed with Kaplan&ndash;Meier estimates, the log-rank test and Cox proportional hazards regression.")
}

narr_panel <- function(res, ctx) {
  o <- res$options
  model <- o[["model"]] %||% "within"; se <- o[["se"]] %||% "classic"; effect <- o[["effect"]] %||% "individual"
  est <- switch(model, pooling = "pooled ordinary least squares", within = "a fixed-effects (within) estimator", random = "a random-effects (GLS) estimator",
                fd = "a first-difference estimator", between = "a between-effects estimator", "a panel estimator")
  eff <- if (model %in% c("within", "random")) switch(effect, time = " with time effects", twoways = " with panel and time (two-way) effects", " with panel effects") else ""
  se_txt <- switch(se, cluster = "cluster-robust standard errors (clustered by panel)", dk = "Driscoll&ndash;Kraay standard errors", pcse = "panel-corrected standard errors", "conventional standard errors")
  get <- function(id) { t <- Filter(function(t) t$id == id, res$tables); if (length(t)) t[[1]] else NULL }
  fit <- get("panel_fit"); item <- function(k) { if (is.null(fit)) return(NULL); r <- Filter(function(x) startsWith(x$item, k), fit$rows); if (length(r)) r[[1]]$value else NULL }
  n_obs <- item("Observations (N)"); n_pan <- item("Panels (n)")
  out <- paste0("A panel regression of ", rp_esc(o[["dep"]]), " on ", rp_esc(rp_list(c(unlist(o[["covs"]]), unlist(o[["factors"]])))), " was estimated with ", est, eff, " and ", se_txt,
                if (!is.null(n_obs)) paste0(" (", rp_int(n_obs), " observations from ", rp_int(n_pan), " panels)") else "", if (length(ctx$tables)) paste0("; ", rp_tab(ctx$tables), " report", if (length(ctx$tables) == 1) "s" else "", " the estimates") else "", ".")
  co <- get("panel_coef")
  zstat <- !is.null(co) && identical(co$columns[[4]]$title, "z")
  if (!is.null(co)) for (r in Filter(function(r) !identical(r$term, "(Intercept)"), co$rows))
    out <- c(out, paste0(rp_esc(r$term), if (rp_sig(r$p, ctx$alpha)) paste0(" had a significant ", if (as.numeric(r$b) > 0) "positive" else "negative", " effect") else " did not have a significant effect",
                         ", <i>b</i> = ", rp_n(r$b, 3), ", <i>SE</i> = ", rp_n(r$se, 3), ", <i>", if (zstat) "z" else "t", "</i> = ", rp_n(r$stat), ", ", rp_p(r$p), "."))
  w <- item("R-squared within"); b <- item("R-squared between"); a <- item("R-squared overall")
  if (!is.null(w)) out <- c(out, paste0("The model explained ", rp_n(w, 2, TRUE), " of the within-panel variance (<i>R</i><sup>2</sup> between = ", rp_n(b, 2, TRUE), ", overall = ", rp_n(a, 2, TRUE), ")."))
  refs <- c("baltagi2021")
  tt <- get("panel_tests")
  if (!is.null(tt)) {
    h <- Filter(function(r) startsWith(r$test, "Hausman"), tt$rows)
    if (length(h)) { h <- h[[1]]; refs <- c(refs, "hausman1978")
      out <- c(out, paste0("The Hausman test ", if (rp_sig(h$p, 0.05)) "rejected the consistency of random effects, supporting the fixed-effects specification" else "did not reject the random-effects specification",
                           ", &chi;<sup>2</sup>(", h$df, ") = ", rp_n(h$stat), ", ", rp_p(h$p), " (Hausman, 1978).")) }
    s <- Filter(function(r) startsWith(r$test, "Wooldridge"), tt$rows)
    if (length(s)) { s <- s[[1]]; refs <- c(refs, "wooldridge2010")
      out <- c(out, paste0("Wooldridge&rsquo;s test ", if (rp_sig(s$p, 0.05)) "indicated" else "did not indicate", " first-order serial correlation, ", rp_p(s$p), " (Wooldridge, 2010).")) }
  }
  list(text = paste(out, collapse = " "), refs = refs,
       method = paste0("Panel data were analysed with ", est, eff, " and ", se_txt, ", following Baltagi (2021)."))
}

narr_plot <- function(res, ctx) {
  o <- res$options
  what <- switch(res$proc,
    plotBar = if (identical(o$type, "means")) paste0("the mean of ", rp_esc(o$var), if (length(o$group)) paste0(" by ", rp_esc(unlist(o$group)[1])) else "") else paste0("the frequencies of ", rp_esc(o$var), if (length(o$group)) paste0(" by ", rp_esc(unlist(o$group)[1])) else ""),
    plotBox = paste0("the distribution of ", rp_esc(rp_list(unlist(o$vars))), if (length(o$group)) paste0(" by ", rp_esc(unlist(o$group)[1])) else ""),
    plotHist = paste0("the distribution of ", rp_esc(o$var)),
    plotScatter = paste0("the relationship between ", rp_esc(o$x), " and ", rp_esc(o$y), if (identical(o$line, "linear")) " with the fitted regression line" else if (identical(o$line, "smooth")) " with a loess smoother" else ""),
    plotRadar = paste0("the ", if (identical(o$stat, "median")) "median" else "mean", " profile of ", rp_esc(rp_list(unlist(o$vars))), if (length(o$group)) paste0(" by ", rp_esc(unlist(o$group)[1])) else "", if (identical(o$scale, "minmax")) " (each variable rescaled to 0&ndash;1)" else if (identical(o$scale, "z")) " (z-scores)" else ""),
    plotLine = paste0("the mean of ", rp_esc(o$dep), " across ", rp_esc(o$x), if (length(o$legend)) paste0(" by ", rp_esc(unlist(o$legend)[1])) else ""), "the data")
  fit <- Filter(function(t) t$id == "plot_fit", res$tables)
  extra <- if (length(fit)) paste(vapply(fit[[1]]$rows, function(r) paste0(if (!identical(r$group, "All")) paste0("For ", rp_esc(r$group), ", the slope was ") else "The slope was ", rp_n(r$slope), " (<i>r</i> = ", rp_n(r$r, 2, TRUE), ", <i>R</i><sup>2</sup> = ", rp_n(r$r2, 2, TRUE), ", ", rp_p(r$p), ")."), ""), collapse = " ") else ""
  list(text = paste0(if (length(ctx$figures)) paste0("Figure ", ctx$figures[1], " shows ") else "The plot shows ", what, ".", if (nzchar(extra)) paste0(" ", extra) else ""), refs = character(0), method = NULL)
}

narr_pgmm <- function(res, ctx) {
  o <- res$options
  get <- function(id) { t <- Filter(function(t) t$id == id, res$tables); if (length(t)) t[[1]] else NULL }
  sys <- identical(o[["estimator"]], "sys"); two <- !identical(o[["steps"]], "onestep"); rob <- !isFALSE(o[["robust"]])
  info <- get("pgmm_info"); item <- function(k) { if (is.null(info)) return(NULL); r <- Filter(function(x) startsWith(x$item, k), info$rows); if (length(r)) r[[1]]$value else NULL }
  est <- if (sys) "the Blundell&ndash;Bond system GMM estimator (Blundell &amp; Bond, 1998)" else "the Arellano&ndash;Bond difference GMM estimator (Arellano &amp; Bond, 1991)"
  refs <- c(if (sys) c("arellano1991", "blundell1998") else "arellano1991", if (two && rob) "windmeijer2005")
  out <- paste0("A dynamic panel model of ", rp_esc(o[["dep"]]), " was estimated with ", est, " in its ", if (two) "two-step" else "one-step", " form",
                if (rob) paste0(", with ", if (two) "Windmeijer-corrected (Windmeijer, 2005)" else "robust", " standard errors") else "",
                if (!is.null(item("Observations"))) paste0(" (", rp_int(item("Observations")), " observations from ", rp_int(item("Panels")), " panels, ", rp_int(item("Instruments")), " instruments)") else "",
                if (length(ctx$tables)) paste0("; ", rp_tab(ctx$tables), " report", if (length(ctx$tables) == 1) "s" else "", " the estimates") else "", ".")
  co <- get("pgmm_coef")
  if (!is.null(co)) for (r in co$rows)
    out <- c(out, paste0(rp_esc(r$term), if (rp_sig(r$p, ctx$alpha)) paste0(" had a significant ", if (as.numeric(r$b) > 0) "positive" else "negative", " effect") else " did not have a significant effect",
                         ", <i>b</i> = ", rp_n(r$b, 3), ", <i>SE</i> = ", rp_n(r$se, 3), ", <i>z</i> = ", rp_n(r$stat), ", ", rp_p(r$p), "."))
  tt <- get("pgmm_tests")
  if (!is.null(tt)) {
    row <- function(k) { r <- Filter(function(x) startsWith(x$test, k), tt$rows); if (length(r)) r[[1]] else NULL }
    a2 <- row("Arellano-Bond AR(2)"); oi <- row(if (two) "Hansen" else "Sargan")
    if (!is.null(a2)) out <- c(out, paste0("The Arellano&ndash;Bond test ", if (rp_sig(a2$p, 0.05)) "detected" else "did not detect", " second-order autocorrelation in the differenced residuals, <i>z</i> = ", rp_n(a2$stat), ", ", rp_p(a2$p), "."))
    if (!is.null(oi)) out <- c(out, paste0("The ", if (two) "Hansen" else "Sargan", " test ", if (rp_sig(oi$p, 0.05)) "rejected" else "did not reject", " the validity of the instruments, &chi;<sup>2</sup>(", oi$df, ") = ", rp_n(oi$stat), ", ", rp_p(oi$p), "."))
    refs <- c(refs, "roodman2009")
  }
  list(text = paste(out, collapse = " "), refs = unique(refs),
       method = paste0("Dynamic panel models were estimated by GMM (", if (sys) "Blundell &amp; Bond, 1998" else "Arellano &amp; Bond, 1991", "), with lagged levels as instruments and the instrument count kept below the number of panels (Roodman, 2009)."))
}

narr_purtest <- function(res, ctx) {
  o <- res$options
  t <- Filter(function(t) t$id == "purtest", res$tables)
  if (!length(t)) return(NULL)
  rows <- Filter(function(r) !is.null(r$p), t[[1]]$rows)
  keys <- c("Levin" = "levin2002", "Im-" = "im2003", "Maddala" = "maddala1999", "Choi" = "choi2001", "Hadri" = "hadri2000")
  refs <- unique(unname(keys[vapply(names(keys), function(k) any(grepl(k, vapply(rows, `[[`, "", "test"), fixed = TRUE)), TRUE)]))
  out <- paste0("Panel unit root tests", if (isTRUE(o[["diff"]])) " of the first differences" else "", " were run for ", rp_esc(rp_list(unlist(o[["vars"]]))),
                if (length(ctx$tables)) paste0(" (", rp_tab(ctx$tables), ")") else "", ".")
  for (v in unique(vapply(rows, `[[`, "", "var"))) {
    rv <- Filter(function(r) identical(r$var, v), rows)
    ur <- Filter(function(r) !startsWith(r$test, "Hadri"), rv)
    if (length(ur)) {
      n_rej <- sum(vapply(ur, function(r) rp_sig(r$p, 0.05), TRUE))
      out <- c(out, paste0("For ", rp_esc(v), ", ", n_rej, " of ", length(ur), " test", if (length(ur) == 1) "" else "s", " rejected the null hypothesis of a unit root",
                           if (n_rej == length(ur)) ", indicating that the series is stationary" else if (n_rej == 0) ", indicating a unit root" else ", giving mixed evidence", "."))
    }
  }
  list(text = paste(out, collapse = " "), refs = refs, method = "Stationarity was examined with panel unit root tests before modelling.")
}

stats_narrative <- function(res, ctx) {
  f <- switch(res$proc, descriptives = narr_descriptives, ttestIS = narr_ttestIS, ttestPS = narr_ttestPS, ttestOneS = narr_ttestOneS, anovaOneW = narr_anovaOneW,
              ANOVA = , ancova = , anovaRM = narr_ANOVA, anovaNP = , anovaRMNP = narr_anovaNP, corrMatrix = , corrPart = narr_corr, contTables = , contTablesPaired = narr_contTables,
              propTest2 = , propTestN = narr_props, reliability = narr_reliability, kappa = narr_kappa, icc = narr_icc, bcorr = , banova = , banovaRM = narr_bayes, km = , cox = narr_survival, panel = narr_panel, pgmm = narr_pgmm, purtest = narr_purtest,
              plotBar = , plotBox = , plotHist = , plotScatter = , plotLine = , plotRadar = , ggplot = narr_plot, NULL)
  out <- if (!is.null(f)) tryCatch(f(res, ctx), error = function(e) NULL) else NULL
  if (is.null(out) || !nzchar(out$text %||% "")) out <- list(text = paste0("The results of the ", rp_esc(tolower(res$label)), " are reported in ", if (length(ctx$tables)) rp_tab(ctx$tables) else if (length(ctx$figures)) paste("Figure", ctx$figures[1]) else "the output", "."), refs = character(0), method = out$method)
  for (fx in list(narr_tost, narr_robust)) {
    eq <- tryCatch(fx(res), error = function(e) NULL)
    if (!is.null(eq)) { out$text <- paste(out$text, eq$text); out$refs <- unique(c(out$refs, eq$refs)); out$method <- paste(c(out$method, eq$method), collapse = " ") }
  }
  eq <- NULL
  if (!is.null(eq)) { out$text <- paste(out$text, eq$text); out$refs <- unique(c(out$refs, eq$refs)); out$method <- paste(c(out$method, eq$method), collapse = " ") }
  out
}

#' Robust-test, bootstrap and Bayesian contingency sentences, added to the write-ups of the analyses that offer them.
narr_robust <- function(res) {
  tb <- function(id) { x <- Filter(function(t) t$id == id, res$tables); if (length(x)) x[[1]] else NULL }
  txt <- character(0); refs <- character(0); tr <- as.numeric(res$options$trim %||% 0.2)
  if (!is.null(t <- tb("robust_ttest"))) { refs <- "wilcox2017"
    txt <- c(txt, paste0("Yuen&rsquo;s test on ", round(100 * tr), "% trimmed means (Wilcox, 2017) ", paste(vapply(t$rows, function(r) paste0("for ", rp_esc(r$var), " gave <i>t</i>(", rp_df(r$df), ") = ", rp_n(r$t, 2), ", ", rp_p(r$p),
                                                       ", &xi; = ", rp_n(r$es, 2, TRUE)), ""), collapse = "; "), ".")) }
  if (!is.null(t <- tb("robust_anova"))) { refs <- "wilcox2017"
    txt <- c(txt, paste0("A robust ANOVA on ", round(100 * tr), "% trimmed means (Wilcox, 2017) ", paste(vapply(t$rows, function(r) paste0("for ", rp_esc(r$var %||% r$term), " gave ",
      if (!is.null(r$df1)) paste0("<i>F</i>(", rp_df(r$df1), ", ", rp_df(r$df2), ") = ", rp_n(r$f, 2)) else paste0("<i>", if (is.null(r$stat)) "F" else "Q", "</i> = ", rp_n(r$stat, 2)), ", ", rp_p(r$p)), ""), collapse = "; "), ".")) }
  for (id in c("boot_mean", "boot_cor")) if (!is.null(t <- tb(id))) { refs <- c(refs, "efron1993")
    txt <- c(txt, paste0("Bias-corrected and accelerated bootstrap intervals (Efron &amp; Tibshirani, 1993) were ", paste(vapply(t$rows, function(r) paste0(rp_esc(r$var), " ", rp_ci(r$bl, r$bu, as.numeric(res$options$ciWidth %||% 95), 2, id == "boot_cor")), ""), collapse = "; "), ".")) }
  if (!is.null(t <- tb("bf_cont"))) { r <- t$rows[[1]]; refs <- c(refs, "lee2013")
    txt <- c(txt, paste0("A Bayesian contingency table test gave BF<sub>10</sub> = ", rp_bf(r$bf10), ", ", rp_bf_word(r$bf10), " (Lee &amp; Wagenmakers, 2013).")) }
  if (!length(txt)) return(NULL)
  list(text = paste(txt, collapse = " "), refs = unique(refs), method = NULL)
}

#' Equivalence test (TOST) sentences, added to the t-test and correlation write-ups.
narr_tost <- function(res) {
  rt <- Filter(function(t) t$id == "tost_result", res$tables); tt <- Filter(function(t) t$id == "tost", res$tables)
  if (!length(rt) || !length(tt)) return(NULL)
  o <- res$options; a <- as.numeric(o$eqAlpha %||% 0.05); level <- 100 * (1 - 2 * a); cor <- identical(res$proc, "corrMatrix")
  lo <- o$eqLow %||% (if (cor) -0.3 else -0.5); hi <- o$eqHigh %||% (if (cor) 0.3 else 0.5)
  bounds <- if (cor) sprintf("<i>r</i> = %s and %s", rp_n(lo, 2, TRUE), rp_n(hi, 2, TRUE)) else if (identical(o$eqBound, "raw")) sprintf("%s and %s (raw units)", rp_n(lo, 2), rp_n(hi, 2)) else sprintf("Cohen&rsquo;s <i>d</i> = %s and %s", rp_n(lo, 2), rp_n(hi, 2))
  stat <- if (cor) "z" else "t"
  parts <- vapply(rt[[1]]$rows, function(r) {
    tr <- Filter(function(x) identical(x$var, r$var), tt[[1]]$rows)
    worst <- tr[[which.max(vapply(tr, function(x) as.numeric(x$p), 0))]]
    tst <- paste0("<i>", stat, "</i>", if (!cor) paste0("(", rp_df(worst$df), ")") else "", " = ", rp_n(worst$stat, 2), ", ", rp_p(worst$p))
    verdict <- switch(r$result, "Equivalent" = "was statistically equivalent to zero", "Equivalent (trivial difference)" = "was statistically equivalent to zero, although different from zero (a trivially small effect)",
                      "Not equivalent (different)" = "was not equivalent to zero and differed from zero", "could not be shown to be equivalent to zero, nor did it differ from zero (inconclusive)")
    paste0("For ", rp_esc(r$var), ", the ", if (cor) "correlation" else "difference", " (", rp_n(r$est, 2, cor), ", ", rp_ci(r$cl, r$cu, level, 2, cor), ") ", verdict, ", ", tst)
  }, "")
  list(text = paste0("Equivalence was tested with two one-sided tests (TOST; Lakens, 2017) against bounds of ", bounds, ", &alpha; = ", rp_n(a, 2, TRUE), ". ", paste(parts, collapse = "; "), "."),
       refs = "lakens2017", method = "Equivalence tests (TOST) examined whether effects were small enough to be considered negligible.")
}

# ---- APA tables ---------------------------------------------------------------------------------------------------
STATS_BOUNDED <- c("r", "rho", "tau", "alpha", "omega", "etaSq", "etaSqP", "omegaSq", "partEta", "prop", "es", "v", "r2", "w", "swW")
rp_table_spec <- function(t, res, d) {
  # Descriptives with variables across columns -> APA layout with one row per variable (and group).
  if (identical(t$id, "descriptives") && !is.null(t$rows[[1]]$statKey)) {
    long <- desc_long(list(tables = list(t)))
    vars <- unique(vapply(long, `[[`, "", "var")); groups <- unique(vapply(long, function(x) x$group, "")); keys <- unique(vapply(long, `[[`, "", "key"))
    base_keys <- unique(vapply(keys, function(k) { for (g in groups) if (nzchar(g) && endsWith(k, g)) return(substr(k, 1, nchar(k) - nchar(g))); k }, ""))
    labels <- vapply(base_keys, function(k) { x <- Filter(function(z) startsWith(z$key, k), long)[[1]]; x$label }, "")
    sym <- c(N = "<i>n</i>", Missing = "Missing", Mean = "<i>M</i>", Median = "<i>Mdn</i>", "Standard deviation" = "<i>SD</i>", Minimum = "Min", Maximum = "Max", Variance = "Variance",
             Range = "Range", Skewness = "Skewness", Kurtosis = "Kurtosis", "Std. error mean" = "<i>SE</i>", "Shapiro-Wilk W" = "<i>W</i>", "Shapiro-Wilk p" = "<i>p</i> (S&ndash;W)")
    head <- c("Variable", if (any(nzchar(groups))) "Group", vapply(labels, function(l) if (!is.null(sym[l]) && !is.na(sym[l])) sym[[l]] else rp_esc(l), ""))
    rows <- list()
    for (v in vars) for (g in groups) rows[[length(rows) + 1]] <- c(rp_esc(v), if (any(nzchar(groups))) rp_esc(g), vapply(base_keys, function(k) {
      val <- desc_get(long, v, paste0(k, g), g); if (!rp_ok(val)) return("&ndash;")
      if (k %in% c("n", "missing")) rp_int(val) else if (k == "sw") sub("<i>p</i> = |<i>p</i> ", "", rp_p(val)) else rp_n(val, d, k %in% c("sww"))
    }, ""))
    return(list(title = "Descriptive Statistics", header = head, rows = rows, notes = character(0)))
  }
  t <- stats_fold(t)   # a row per test, as jamovi shows it
  cols <- t$columns
  head <- vapply(cols, function(cc) {
    tt <- cc$title %||% ""; if (!nzchar(tt)) return("")
    s <- c(p = "<i>p</i>", t = "<i>t</i>", F = "<i>F</i>", df = "<i>df</i>", N = "<i>N</i>", n = "<i>n</i>", Mean = "<i>M</i>", SD = "<i>SD</i>", SE = "<i>SE</i>", r = "<i>r</i>", W = "<i>W</i>", U = "<i>U</i>", z = "<i>z</i>")
    lab <- if (!is.na(s[tt])) s[[tt]] else rp_esc(tt)
    if (nzchar(cc$superTitle %||% "")) paste0(rp_esc(cc$superTitle), ": ", lab) else lab
  }, "")
  rows <- lapply(t$rows, function(r) stats_sym(vapply(cols, function(cc) {
    v <- r[[cc$name]]
    if (is.null(v) || (is.character(v) && !nzchar(v))) return("")
    if (is.character(v)) return(rp_esc(strip_html(v)))
    if (isTRUE(cc$pvalue) || isTRUE(r$.p)) return(pt_p(v))
    base <- sub("\\[.*$", "", cc$name)
    if (identical(cc$type, "integer") || isTRUE(r$.int) || (is.numeric(v) && v == round(v) && base %in% c("n", "N", "num", "count", "missing", "obs", "total", "events", "censored", "df", "df1", "df2"))) return(rp_int(v))
    pt_num(v, d, bounded = base %in% STATS_BOUNDED)
  }, ""), r, cols, html = TRUE))
  for (j in which(vapply(cols, function(cc) isTRUE(cc$combineBelow), TRUE))) { prev <- NULL; for (k in seq_along(rows)) { cur <- rows[[k]][j]; if (identical(cur, prev)) rows[[k]][j] <- "" else prev <- cur } }
  keep <- vapply(seq_along(cols), function(j) any(nzchar(vapply(rows, `[`, "", j))) || j == 1, TRUE)   # drop empty columns
  list(title = tools::toTitleCase(t$title), header = head[keep], rows = lapply(rows, function(r) r[keep]), notes = as.character(ul(t$notes)))
}

rp_table_html <- function(sp, num) {
  paste0("<div class=\"apa-table\"><p class=\"tnum\"><b>Table ", num, "</b></p><p class=\"ttitle\"><i>", rp_esc(sp$title), "</i></p><table><thead><tr>",
         paste0("<th>", sp$header, "</th>", collapse = ""), "</tr></thead><tbody>",
         paste(vapply(sp$rows, function(r) paste0("<tr>", paste0("<td", ifelse(seq_along(r) == 1, "", " class=\"n\""), ">", r, "</td>", collapse = ""), "</tr>"), ""), collapse = ""),
         "</tbody></table>", if (length(sp$notes)) paste0("<p class=\"tnote\"><i>Note.</i> ", rp_esc(paste(sp$notes, collapse = " ")), "</p>") else "", "</div>")
}

# ---- Assemble -----------------------------------------------------------------------------------------------------
REPORT_JOURNAL_STYLE <- "<style>
body{font-family:'Times New Roman',Times,serif;font-size:12pt;line-height:2;color:#000;max-width:8.3in;margin:0 auto;padding:1in;background:#fff}
h1{font-size:12pt;text-align:center;font-weight:bold;margin:0 0 .5em} h2{font-size:12pt;font-weight:bold;margin:1em 0 0} h3{font-size:12pt;font-weight:bold;font-style:italic;margin:1em 0 0}
p{margin:0;text-indent:.5in} p.noindent,.apa-table p,figure p{text-indent:0}
.apa-table{margin:1.2em 0;line-height:1.25} .apa-table table{border-collapse:collapse;width:100%;font-size:11pt;border-top:1px solid #000;border-bottom:1px solid #000}
.apa-table th{border-bottom:1px solid #000;padding:3px 6px;font-weight:normal;text-align:center;vertical-align:bottom} .apa-table td{padding:2px 6px;vertical-align:top} .apa-table td.n{text-align:center} sup.sym{display:inline-block;width:0;line-height:0;font-size:.8em;white-space:nowrap}
.apa-table th:first-child{text-align:left} .tnote{font-size:10.5pt;margin-top:4px} .tnum{margin:0} .ttitle{margin:0 0 4px}
figure{margin:1.2em 0;line-height:1.25} figure img,figure svg{max-width:100%;height:auto;display:block} figure .fnum{font-weight:bold} figure .fcap{font-style:italic;margin-bottom:6px}
.refs p{padding-left:.5in;text-indent:-.5in} .meta{font-size:10pt;color:#555;line-height:1.4;text-indent:0;border:1px solid #ddd;padding:6px 10px;margin-bottom:1.5em}
@media print{body{padding:0}}
</style>"

#' Build the manuscript. `items`: list(list(result, title, figures = list(list(png, caption)))). `ctx`: alpha, decimals,
#' sections (method, results, tables, figures, references), dataset (for computed variables and edits), missingCode.
stats_report_build <- function(items, ctx) {
  alpha <- ctx$alpha %||% 0.05; d <- ctx$decimals %||% 2L; sec <- ctx$sections %||% list()
  on <- function(k) !isFALSE(sec[[k]])
  tab_n <- 0L; fig_n <- 0L; refs <- character(0); method <- character(0); pkgs <- c("R", "jmv")   # jmv: always cited
  blocks <- list(); figs_out <- list()
  for (it in items) {
    res <- it$result
    pkgs <- c(pkgs, names(res$packages), if (uses_nortest(res$proc, res$options)) "nortest")
    specs <- if (on("tables")) Filter(Negate(is.null), lapply(res$tables, function(t) tryCatch(if (length(t$rows)) rp_table_spec(t, res, d), error = function(e) NULL))) else list()
    tnums <- if (length(specs)) seq(tab_n + 1L, tab_n + length(specs)) else integer(0); tab_n <- tab_n + length(specs)
    figs <- if (on("figures")) it$figures %||% list() else list()
    fnums <- if (length(figs)) seq(fig_n + 1L, fig_n + length(figs)) else integer(0); fig_n <- fig_n + length(figs)
    nar <- stats_narrative(res, list(alpha = alpha, tables = tnums, figures = fnums))
    refs <- c(refs, nar$refs); if (!is.null(nar$method)) method <- c(method, nar$method)
    blocks[[length(blocks) + 1]] <- list(title = it$title, text = nar$text, tables = Map(function(s, n) c(s, list(number = n)), specs, tnums),
                                         figures = Map(function(f, n) list(png = f$png, caption = f$caption %||% "", note = f$note %||% "", number = n), figs, fnums))
  }
  pkgs <- unique(setdiff(pkgs, c("graphics", "base", "stats")))
  pk <- Filter(Negate(is.null), setNames(lapply(pkgs, rp_pkg_ref), pkgs))
  soft <- paste0("Analyses were conducted in PathLab SEM (Version ", PATHLAB_VERSION, "; ", pathlab_in_text(), "), which runs R (", pk$R$cite %||% "R Core Team", ")",
                 if (length(setdiff(names(pk), "R"))) paste0(" with the ", rp_list(vapply(setdiff(names(pk), "R"), function(p) paste0(p, " package (Version ", pk[[p]]$version, "; ", pk[[p]]$cite, ")"), "")), "") else "", ".")
  miss <- unique(unlist(lapply(items, function(it) it$result$options$miss)))
  extra <- c(
    if (length(ctx$computed)) paste0("Composite scores were computed as ", rp_list(vapply(ctx$computed, function(cv) paste0(rp_esc(cv$name), " = ", rp_esc(cv$formula)), "")), "."),
    if (rp_ok(ctx$missingCode)) paste0("Values coded ", rp_esc(ctx$missingCode), " were treated as missing."),
    if (length(miss) && identical(miss, "listwise")) "Cases with missing values were excluded listwise." else "Missing values were excluded analysis by analysis.",
    if ((ctx$nEdits %||% 0) > 0) paste0(ctx$nEdits, " data value", if (ctx$nEdits > 1) "s were" else " was", " corrected in the data editor before the analyses; the original file was retained."),
    paste0("Statistical significance was evaluated at &alpha; = ", pt_num(alpha, 2, TRUE), " (two-tailed)."),
    if (any(c("cohen1988") %in% refs)) "Effect sizes were interpreted following Cohen (1988).")
  cite_keys <- unique(refs)
  H <- character(0); a <- function(...) H <<- c(H, paste0(...))
  a("<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\"><title>", rp_esc(ctx$title %||% "Results"), "</title>", REPORT_JOURNAL_STYLE, "</head><body>")
  a("<p class=\"meta\">", rp_esc(ctx$project %||% ""), " &middot; generated by PathLab SEM ", PATHLAB_RELEASE, " on ", format(Sys.time(), "%Y-%m-%d %H:%M"), ". Every number is taken from the stored R results; review the wording before submission.</p>")
  if (on("method")) { a("<h2>Statistical Analysis</h2>"); a("<p>", soft, " ", paste(unique(method), collapse = " "), " ", paste(extra, collapse = " "), "</p>") }
  if (on("results")) {
    a("<h1>Results</h1>")
    for (b in blocks) {
      a("<h2>", rp_esc(b$title), "</h2>"); a("<p>", b$text, "</p>")
      for (t in b$tables) a(rp_table_html(t, t$number))
      for (f in b$figures) if (nzchar(f$png %||% "")) a("<figure><p class=\"fnum\">Figure ", f$number, "</p><p class=\"fcap\">", rp_esc(f$caption), "</p><img alt=\"Figure ", f$number, "\" src=\"data:image/png;base64,", f$png, "\">", if (nzchar(f$note %||% "")) paste0("<p class=\"tnote\"><i>Note.</i> ", rp_esc(f$note), "</p>") else "", "</figure>")
    }
  }
  if (on("references")) {
    entries <- c(vapply(pk, `[[`, "", "ref"), STATIC_REFS$pathlab$ref, vapply(cite_keys, function(k) STATIC_REFS[[k]]$ref %||% "", ""))
    entries <- unique(entries[nzchar(entries)])
    ord <- order(tolower(gsub("<[^>]+>|&[a-z]+;", "", entries)))
    a("<h1>References</h1><div class=\"refs\">", paste0("<p>", entries[ord], "</p>", collapse = ""), "</div>")
  }
  a("</body></html>")
  list(html = paste(H, collapse = "\n"), blocks = blocks, method = if (on("method")) paste(c(soft, unique(method), extra), collapse = " ") else NULL,
       references = if (on("references")) { e <- unique(c(vapply(pk, `[[`, "", "ref"), STATIC_REFS$pathlab$ref, vapply(cite_keys, function(k) STATIC_REFS[[k]]$ref %||% "", ""))); e <- e[nzchar(e)]; e[order(tolower(gsub("<[^>]+>|&[a-z]+;", "", e)))] } else NULL,
       sections = sec)
}

#' Text of an HTML fragment (tags removed, entities decoded) for Word table cells.
rp_plain <- function(x) vapply(x, function(s) paste(vapply(rp_runs(s), function(r) r$value, ""), collapse = ""), "", USE.NAMES = FALSE)

# ---- Word (.docx) -------------------------------------------------------------------------------------------------
#' HTML fragment (i, b, sup, sub and entities) -> officer formatted runs.
rp_runs <- function(s, font = "Times New Roman", size = 12) {
  s <- gsub("&nbsp;", " ", s)
  ent <- c("&lt;" = "<", "&gt;" = ">", "&amp;" = "&", "&ndash;" = "–", "&mdash;" = "—", "&rsquo;" = "’", "&lsquo;" = "‘", "&plusmn;" = "±", "&times;" = "×",
           "&chi;" = "χ", "&eta;" = "η", "&omega;" = "ω", "&alpha;" = "α", "&epsilon;" = "ε", "&tau;" = "τ", "&eacute;" = "é", "&middot;" = "·")
  toks <- regmatches(s, gregexpr("<[^>]+>|[^<]+", s))[[1]]
  st <- list(i = FALSE, b = FALSE, sup = FALSE, sub = FALSE); runs <- list()
  for (tk in toks) {
    if (startsWith(tk, "<")) { tag <- tolower(gsub("[</> ]", "", tk)); on <- !startsWith(tk, "</"); if (tag %in% names(st)) st[[tag]] <- on; next }
    for (e in names(ent)) tk <- gsub(e, ent[[e]], tk, fixed = TRUE)
    runs[[length(runs) + 1]] <- officer::ftext(tk, officer::fp_text(font.family = font, font.size = if (st$sup || st$sub) size * 0.8 else size, italic = st$i, bold = st$b,
                                                                     vertical.align = if (st$sup) "superscript" else if (st$sub) "subscript" else "baseline"))
  }
  runs
}

stats_report_docx <- function(rep, path, font = "Times New Roman") {
  doc <- officer::read_docx()
  doc <- officer::body_set_default_section(doc, officer::prop_section(page_size = officer::page_size(orient = "portrait", width = 8.27, height = 11.69),
                                                                      page_margins = officer::page_mar(top = 1, bottom = 1, left = 1, right = 1, header = 0.5, footer = 0.5, gutter = 0)))
  par <- function(html, align = "left", indent = TRUE, bold = FALSE, italic = FALSE, spacing = 2, hanging = FALSE) {
    rr <- rp_runs(if (bold) paste0("<b>", html, "</b>") else if (italic) paste0("<i>", html, "</i>") else html, font)
    fp <- officer::fp_par(text.align = align, line_spacing = spacing, padding.bottom = 0, padding.top = 0, padding.left = if (hanging) 36 else 0)
    p <- do.call(officer::fpar, c(if (indent && !hanging) list(officer::ftext("\t", officer::fp_text(font.family = font, font.size = 12))) else list(), rr, list(fp_p = fp)))
    doc <<- officer::body_add_fpar(doc, p)
  }
  if (!is.null(rep$method)) { par("Statistical Analysis", indent = FALSE, bold = TRUE); par(rep$method) }
  if (!isFALSE(rep$sections$results)) {
    par("Results", align = "center", indent = FALSE, bold = TRUE)
    for (b in rep$blocks) {
      par(rp_esc(b$title), indent = FALSE, bold = TRUE)
      par(b$text)
      for (t in b$tables) {
        tab <- list(header = rp_plain(t$header), rows = lapply(t$rows, rp_plain),
                    title = t$title, notes = as.list(t$notes), number = t$number, merge = integer(0), bold = list(), span = integer(0),
                    align = c("l", rep("c", length(t$header) - 1)))
        ft <- tryCatch(pt_flextable(tab, list(font = font, orientation = "portrait")), error = function(e) NULL)
        if (!is.null(ft)) { doc <- flextable::body_add_flextable(doc, ft, align = "left"); doc <- officer::body_add_par(doc, "") }
      }
      for (f in b$figures) if (nzchar(f$png %||% "")) {
        par(paste0("Figure ", f$number), indent = FALSE, bold = TRUE, spacing = 1)
        par(rp_esc(f$caption), indent = FALSE, italic = TRUE, spacing = 1)
        img <- tempfile(fileext = ".png"); writeBin(jsonlite::base64_dec(f$png), img)
        dims <- tryCatch({ h <- readBin(img, "raw", 24); c(sum(as.integer(h[17:20]) * 256^(3:0)), sum(as.integer(h[21:24]) * 256^(3:0))) }, error = function(e) c(800, 500))
        wd <- min(6.27, dims[1] / 150); doc <- officer::body_add_img(doc, img, width = wd, height = wd * dims[2] / dims[1])
        if (nzchar(f$note %||% "")) par(paste0("<i>Note.</i> ", rp_esc(f$note)), indent = FALSE, spacing = 1)
        doc <- officer::body_add_par(doc, "")
      }
    }
  }
  if (length(rep$references)) { par("References", align = "center", indent = FALSE, bold = TRUE); for (r in rep$references) par(r, indent = FALSE, hanging = TRUE) }
  print(doc, target = path)
  path
}

# ---- Edited report -> Word (pandoc) ----------------------------------------------------------------------------------
#' The report as edited in the app (HTML) converted to Word by pandoc (from Quarto), sandboxed (no file or network
#' access), with the APA reference document (Times New Roman, A4) and alignment styles; formulas (MathML from the
#' editor) become Word equations; a Lua filter keeps text colour, shading and alignment.
stats_report_html_docx <- function(html, path, font = "Times New Roman") {
  q <- quarto_bin()
  if (is.null(q)) api_error(501, "docx_unavailable", "Word export of the edited report needs Quarto (it includes pandoc).", "Install Quarto from quarto.org, or download the HTML and open it in Word.")
  if (!is.character(html) || !nzchar(html)) api_error(400, "report_html", "The report is empty.")
  if (nchar(html, type = "bytes") > 40e6) api_error(413, "report_html", "The edited report is larger than 40 MB.")
  src <- tempfile(fileext = ".html"); writeLines(enc2utf8(html), src, useBytes = TRUE)
  ref <- tempfile(fileext = ".docx")
  make_reference_docx(q, ref, font)
  doc <- officer::read_docx(ref)
  for (st in list(c("Centered", "center"), c("Right", "right"), c("Justified", "justify")))
    doc <- tryCatch(officer::docx_set_paragraph_style(doc, style_id = st[1], style_name = st[1], base_on = "Body Text", fp_p = officer::fp_par(text.align = st[2]),
                                                      fp_t = officer::fp_text_lite(font.family = font, font.size = 12)), error = function(e) doc)
  print(doc, target = ref)
  lua <- file.path(backend_dir, "pandoc", "pathlab-docx.lua")
  r <- processx::run(q, c("pandoc", src, "-f", "html", "-t", "docx", "--sandbox", "--reference-doc", ref, if (file.exists(lua)) c("--lua-filter", lua), "-o", path), error_on_status = FALSE)
  if (r$status != 0 || !file.exists(path)) api_error(500, "docx_failed", paste("Word conversion failed:", sanitize_message(substr(r$stderr, 1, 300))))
  path
}
