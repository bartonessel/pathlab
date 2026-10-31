# Descriptive and inferential statistics in the style of jamovi / JASP. The engine is jmv, jamovi's own R package, so
# every number equals jamovi's and each analysis carries jamovi's R syntax (Analysis$asSource()). jmv's result tables
# are converted generically (columns, super-titles, p-value formats, notes), so one renderer, report, workbook and APA
# path serves every procedure. Additions not in jmv come from established packages: Kolmogorov-Smirnov (Lilliefors)
# and Anderson-Darling normality tests (nortest), and plot data (graphics::hist, stats::density, grDevices::boxplot.stats,
# stats::qqnorm). The data are never changed: the project missing-value code is treated as missing for the analysis,
# and measurement levels (nominal / ordinal / continuous) only decide how a variable enters it.

# Argument types: vars (several variables), var (one), any (one variable of any type), factors (variables used as factors), bool, num, enum:<values>,
# enums:<values> (several), string, terms (list of variable-name vectors), pairs (list of {i1, i2}),
# latent (list of {label, vars}: the factors of a CFA).
STATS_PROCS <- list(
  descriptives = list(label = "Descriptives", group = "Exploration", fn = "descriptives",
    args = list(vars = "vars", splitBy = "factors", freq = "bool", desc = "enum:columns,rows", n = "bool", missing = "bool", mean = "bool", median = "bool",
                mode = "bool", sum = "bool", sd = "bool", variance = "bool", range = "bool", min = "bool", max = "bool", se = "bool", ci = "bool", ciWidth = "num",
                iqr = "bool", skew = "bool", kurt = "bool", sw = "bool", pcEqGr = "bool", pcNEqGr = "num", pc = "bool", pcValues = "string", extreme = "bool", extremeN = "num"),
    extra = list(normTests = "bool", hist = "bool", dens = "bool", box = "bool", bar = "bool", qq = "bool", violin = "bool", dot = "bool", boxMean = "bool", boxLabelOutliers = "bool")),
  ttestIS = list(label = "Independent Samples T-Test", group = "T-Tests", fn = "ttestIS",
    args = list(vars = "vars", group = "factors", students = "bool", bf = "bool", bfPrior = "num", welchs = "bool", mann = "bool", hypothesis = "enum:different,oneGreater,twoGreater",
                norm = "bool", eqv = "bool", meanDiff = "bool", ci = "bool", ciWidth = "num", effectSize = "bool", ciES = "bool", ciWidthES = "num", desc = "bool", miss = "enum:perAnalysis,listwise"),
    extra = list(meansPlot = "bool", qq = "bool")),
  ttestPS = list(label = "Paired Samples T-Test", group = "T-Tests", fn = "ttestPS",
    args = list(pairs = "pairs", students = "bool", bf = "bool", bfPrior = "num", wilcoxon = "bool", hypothesis = "enum:different,oneGreater,twoGreater", norm = "bool",
                meanDiff = "bool", ci = "bool", ciWidth = "num", effectSize = "bool", ciES = "bool", ciWidthES = "num", desc = "bool", miss = "enum:perAnalysis,listwise"),
    extra = list(meansPlot = "bool", qq = "bool")),
  ttestOneS = list(label = "One Sample T-Test", group = "T-Tests", fn = "ttestOneS",
    args = list(vars = "vars", students = "bool", bf = "bool", bfPrior = "num", wilcoxon = "bool", testValue = "num", hypothesis = "enum:dt,gt,lt", norm = "bool",
                meanDiff = "bool", ci = "bool", ciWidth = "num", effectSize = "bool", ciES = "bool", ciWidthES = "num", desc = "bool", miss = "enum:perAnalysis,listwise"),
    extra = list(meansPlot = "bool", qq = "bool")),
  anovaOneW = list(label = "One-Way ANOVA", group = "ANOVA", fn = "anovaOneW",
    args = list(deps = "vars", group = "factors", welchs = "bool", fishers = "bool", miss = "enum:perAnalysis,listwise", desc = "bool", norm = "bool", eqv = "bool",
                phMethod = "enum:none,gamesHowell,tukey", phMeanDif = "bool", phSig = "bool", phTest = "bool", phFlag = "bool"),
    extra = list(meansPlot = "bool", qq = "bool")),
  ANOVA = list(label = "ANOVA", group = "ANOVA", fn = "ANOVA",
    args = list(dep = "var", factors = "factors", effectSize = "enums:eta,partEta,omega", modelTest = "bool", ss = "enum:1,2,3", homo = "bool", norm = "bool",
                postHoc = "terms", postHocCorr = "enums:none,tukey,scheffe,bonf,holm", postHocES = "enums:d", emMeans = "terms", emmTables = "bool"),
    extra = list(meansPlot = "bool", qq = "bool")),
  ancova = list(label = "ANCOVA", group = "ANOVA", fn = "ancova",
    args = list(dep = "var", factors = "factors", covs = "vars", effectSize = "enums:eta,partEta,omega", modelTest = "bool", ss = "enum:1,2,3", homo = "bool", norm = "bool",
                postHoc = "terms", postHocCorr = "enums:none,tukey,scheffe,bonf,holm", postHocES = "enums:d", emMeans = "terms", emmTables = "bool"),
    extra = list(meansPlot = "bool", qq = "bool")),
  anovaRM = list(label = "Repeated Measures ANOVA", group = "ANOVA", fn = "anovaRM",
    args = list(rmMeasures = "vars", rmLabel = "string", bs = "factors", cov = "vars", effectSize = "enums:eta,partEta,ges,omega", spherTests = "bool",
                spherCorr = "enums:none,GG,HF", leveneTest = "bool", postHoc = "terms", postHocCorr = "enums:none,tukey,scheffe,bonf,holm", groupSumm = "bool"),
    extra = list(meansPlot = "bool")),
  anovaNP = list(label = "Kruskal-Wallis", group = "ANOVA", fn = "anovaNP",
    args = list(deps = "vars", group = "factors", es = "bool", pairs = "bool", pairsDunn = "bool")),
  anovaRMNP = list(label = "Friedman", group = "ANOVA", fn = "anovaRMNP",
    args = list(measures = "vars", pairs = "bool", desc = "bool")),
  corrMatrix = list(label = "Correlation Matrix", group = "Regression", fn = "corrMatrix",
    args = list(vars = "vars", pearson = "bool", spearman = "bool", kendall = "bool", sig = "bool", flag = "bool", n = "bool", ci = "bool", ciWidth = "num",
                hypothesis = "enum:corr,pos,neg")),
  corrPart = list(label = "Partial Correlation", group = "Regression", fn = "corrPart",
    args = list(vars = "vars", controls = "vars", pearson = "bool", spearman = "bool", kendall = "bool", type = "enum:part,semi", sig = "bool", flag = "bool", n = "bool",
                hypothesis = "enum:corr,pos,neg")),
  contTables = list(label = "Contingency Tables", group = "Frequencies", fn = "contTables",
    args = list(rows = "factors", cols = "factors", layers = "factors", chiSq = "bool", chiSqCorr = "bool", zProp = "bool", likeRat = "bool", fisher = "bool", contCoef = "bool",
                phiCra = "bool", logOdds = "bool", odds = "bool", relRisk = "bool", ci = "bool", ciWidth = "num", gamma = "bool", taub = "bool", mh = "bool",
                obs = "bool", exp = "bool", pcRow = "bool", pcCol = "bool", pcTot = "bool")),
  contTablesPaired = list(label = "Paired Samples Contingency Tables (McNemar)", group = "Frequencies", fn = "contTablesPaired",
    args = list(rows = "factors", cols = "factors", chiSq = "bool", chiSqCorr = "bool", exact = "bool", pcRow = "bool", pcCol = "bool")),
  propTest2 = list(label = "Binomial Test", group = "Frequencies", fn = "propTest2",
    args = list(vars = "factors", testValue = "num", hypothesis = "enum:notequal,greater,less", ci = "bool", ciWidth = "num", bf = "bool")),
  propTestN = list(label = "Chi-Square Goodness of Fit", group = "Frequencies", fn = "propTestN",
    args = list(var = "factors", expected = "bool")),
  bcorr = list(label = "Bayesian Correlation", group = "Regression", fn = NULL,
    args = list(vars = "vars", rscale = "enum:medium,wide,ultrawide")),
  banova = list(label = "Bayesian ANOVA", group = "ANOVA", fn = NULL,
    args = list(dep = "var", factors = "factors", rscale = "enum:medium,wide,ultrawide", seed = "num")),
  km = list(label = "Kaplan-Meier Survival", group = "Survival", fn = NULL, engine = "survival",
    args = list(time = "var", event = "any", eventLevel = "string", group = "factors", ciWidth = "num", logrank = "bool", times = "string")),
  cox = list(label = "Cox Regression", group = "Survival", fn = NULL, engine = "survival",
    args = list(time = "var", event = "any", eventLevel = "string", covs = "vars", factors = "factors", ciWidth = "num")),
  panel = list(label = "Panel Data Regression", group = "Regression", fn = NULL, engine = "plm",
    args = list(dep = "var", covs = "vars", factors = "factors", id = "any", time = "any", model = "enum:pooling,within,random,fd,between", effect = "enum:individual,time,twoways",
                se = "enum:classic,cluster,dk,pcse", reMethod = "enum:swar,walhus,amemiya,nerlove", ciWidth = "num", describe = "bool", xtsum = "bool", hausman = "bool", ftest = "bool",
                bplm = "bool", serial = "bool", csd = "bool", hetero = "bool", compare = "bool")),
  pgmm = list(label = "Dynamic Panel GMM", group = "Regression", fn = NULL, engine = "plm",
    args = list(dep = "var", covs = "vars", endog = "vars", id = "any", time = "any", estimator = "enum:d,sys", lagsY = "num", steps = "enum:onestep,twostep",
                effect = "enum:twoways,individual", lagMin = "num", lagMax = "num", collapse = "bool", robust = "bool", ciWidth = "num", showTime = "bool")),
  purtest = list(label = "Panel Unit Root Tests", group = "Time Series", fn = NULL, engine = "plm",
    args = list(vars = "vars", id = "any", time = "any", tests = "enums:levinlin,ips,madwu,Pm,invnormal,logit,hadri", exo = "enum:intercept,trend,none",
                diff = "bool", lagSel = "enum:AIC,SIC,Hall,fixed", lags = "num", pmax = "num")),
  manova = list(label = "MANOVA", group = "ANOVA", fn = "mancova",
    args = list(deps = "vars", factors = "factors", covs = "vars", multivar = "enums:pillai,wilks,hotel,roy", boxM = "bool", shapiro = "bool")),
  logRegMulti = list(label = "Multinomial Logistic Regression", group = "Regression", fn = "logRegMulti",
    args = list(dep = "factors", covs = "vars", factors = "factors", modelTest = "bool", aic = "bool", bic = "bool", pseudoR2 = "enums:r2mf,r2cs,r2n", omni = "bool", ci = "bool", ciWidth = "num", OR = "bool", ciOR = "bool")),
  logRegOrd = list(label = "Ordinal Logistic Regression", group = "Regression", fn = "logRegOrd",
    args = list(dep = "factors", covs = "vars", factors = "factors", modelTest = "bool", aic = "bool", bic = "bool", pseudoR2 = "enums:r2mf,r2cs,r2n", omni = "bool", thres = "bool", ci = "bool", ciWidth = "num", OR = "bool", ciOR = "bool")),
  glmx = list(label = "Generalized Linear Model", group = "Regression", fn = NULL, engine = "MASS",
    args = list(dep = "var", covs = "vars", factors = "factors", family = "enum:gaussian,probit,negbin,gamma,invgauss,quasipoisson", weights = "var", ciWidth = "num", expB = "bool")),
  mixed = list(label = "Mixed Model", group = "Regression", fn = NULL, engine = "lmerTest",
    args = list(dep = "var", covs = "vars", factors = "factors", cluster = "factors", slopes = "vars", family = "enum:gaussian,binomial,poisson", reml = "bool", ciWidth = "num")),
  iv2sls = list(label = "Two-Stage Least Squares", group = "Regression", fn = NULL, engine = "ivreg",
    args = list(dep = "var", exog = "vars", endog = "vars", instruments = "vars", robust = "bool")),
  bayesreg = list(label = "Bayesian Linear Regression", group = "Regression", fn = NULL, engine = "BayesFactor",
    args = list(dep = "var", covs = "vars", rscale = "enum:medium,wide,ultrawide", seed = "num")),
  cancor = list(label = "Canonical Correlation", group = "Regression", fn = NULL, engine = "stats", args = list(set1 = "vars", set2 = "vars")),
  runs = list(label = "Runs Test", group = "Frequencies", fn = NULL, engine = "stats", args = list(vars = "vars", cut = "enum:median,mean")),
  corresp = list(label = "Correspondence Analysis", group = "Frequencies", fn = NULL, engine = "ca", args = list(rows = "factors", cols = "factors", dims = "num")),
  mds = list(label = "Multidimensional Scaling", group = "Factor", fn = NULL, engine = "MASS",
    args = list(vars = "vars", target = "enum:variables,cases", method = "enum:classical,nonmetric", dims = "num", label = "any")),
  lgc = list(label = "Latent Growth Curve", group = "Factor", fn = NULL, engine = "lavaan",
    args = list(measures = "vars", measures2 = "vars", covs = "vars", tvc = "vars", group = "factors", shape = "enum:linear,quadratic,basis,piecewise", times = "string",
                knot = "num", tvcEqual = "bool", estimator = "enum:ML,MLR")),
  lca = list(label = "Latent Class Analysis", group = "Factor", fn = NULL, engine = "poLCA",
    args = list(items = "factors", distal = "vars", covs = "vars", kMin = "num", kMax = "num", k = "num", nrep = "num", seed = "num")),
  lta = list(label = "Latent Transition Analysis", group = "Factor", fn = NULL, engine = "depmixS4",
    args = list(waves = "latent", kMin = "num", kMax = "num", k = "num", nrep = "num", seed = "num")),
  gmm = list(label = "Growth Mixture Model", group = "Factor", fn = NULL, engine = "lcmm",
    args = list(measures = "vars", distal = "vars", covs = "vars", shape = "enum:linear,quadratic", type = "enum:gmm,lcga", random = "enum:intercept,slope", times = "string",
                kMin = "num", kMax = "num", k = "num", nrep = "num", seed = "num")),
  lifetable = list(label = "Life Table", group = "Survival", fn = NULL, engine = "survival",
    args = list(time = "var", event = "any", eventLevel = "string", group = "factors", width = "num")),
  kmeans = list(label = "K-Means / K-Medoids Cluster", group = "Classify", fn = NULL, engine = "cluster",
    args = list(vars = "vars", k = "num", method = "enum:kmeans,pam", standardize = "bool", seed = "num")),
  hclust = list(label = "Hierarchical Cluster", group = "Classify", fn = NULL, engine = "cluster",
    args = list(vars = "vars", method = "enum:ward.D2,average,complete,single", k = "num", standardize = "bool", label = "any")),
  lda = list(label = "Discriminant Analysis", group = "Classify", fn = NULL, engine = "MASS", args = list(group = "factors", vars = "vars", prior = "enum:equal,proportional", cv = "bool")),
  arima = list(label = "Time Series Forecast (ARIMA / ETS)", group = "Time Series", fn = NULL, engine = "forecast",
    args = list(var = "var", order = "any", frequency = "num", model = "enum:auto,ets,manual", p = "num", d = "num", q = "num", horizon = "num", ciWidth = "num")),
  acf = list(label = "Autocorrelations", group = "Time Series", fn = NULL, engine = "stats", args = list(vars = "vars", lags = "num")),
  mva = list(label = "Missing Value Analysis", group = "Missing Data", fn = NULL, engine = "stats", args = list(vars = "vars")),
  mi = list(label = "Multiple Imputation", group = "Missing Data", fn = NULL, engine = "mice",
    args = list(dep = "var", covs = "vars", aux = "vars", m = "num", method = "enum:pmm,norm,cart", maxit = "num", seed = "num")),
  power = list(label = "Power Analysis (G*Power)", group = "Power & Meta", fn = NULL, engine = "stats",
    args = list(gtest = "enum:t_ind,t_paired,t_one,t_reg1,f_anova1,f_anova_special,f_rm_between,f_rm_within,f_rm_inter,f_reg_r2,f_reg_inc,x_gof,z_corr,z_prop2",
                gtype = "enum:apriori,posthoc,sensitivity,criterion,compromise", tails = "enum:1,2", xy = "enum:n_power,power_n,power_es", esList = "string",
                ratio = "num", numdf = "num", m = "num", rho = "num", eps = "num", p = "num", q = "num", rho0 = "num", qratio = "num",
                test = "enum:t2,t1,paired,anova,corr,regression,chisq,prop2", solve = "enum:n,power,es", es = "num", alpha = "num", power = "num", n = "num", k = "num", u = "num", df = "num", alternative = "enum:two.sided,greater")),
  soper_sem = list(label = "A-priori Sample Size: SEM (Soper)", group = "Power & Meta", fn = NULL, engine = "stats",
    args = list(effect = "num", power = "num", latent = "num", observed = "num", alpha = "num")),
  soper_mr = list(label = "A-priori Sample Size: Multiple Regression (Soper)", group = "Power & Meta", fn = NULL, engine = "stats",
    args = list(f2 = "num", power = "num", predictors = "num", alpha = "num")),
  meta = list(label = "Meta-Analysis", group = "Power & Meta", fn = NULL, engine = "metafor",
    args = list(measure = "enum:SMD,COR,generic", study = "any", m1 = "var", sd1 = "var", n1 = "var", m2 = "var", sd2 = "var", n2 = "var", r = "var", n = "var", yi = "var", vi = "var", sei = "var",
                method = "enum:REML,DL,PM,FE", bias = "bool")),
  qc = list(label = "Control Chart", group = "Quality", fn = NULL, engine = "qcc",
    args = list(measure = "var", chart = "enum:xbar,R,S,xbar.one,p,np,c,u", subgroup = "factors", sizes = "var", nsigmas = "num")),
  capability = list(label = "Process Capability", group = "Quality", fn = NULL, engine = "qcc",
    args = list(measure = "var", subgroup = "factors", lsl = "num", usl = "num", target = "num")),
  plotBar = list(label = "Bar Plot", group = "Plots", fn = NULL, engine = "graphics",
    args = list(type = "enum:counts,means", var = "any", group = "factors", legend = "factors", yAxis = "enum:count,percent", errorBars = "enum:se,ci,none", ciWidth = "num",
                title = "string", xTitle = "string", yTitle = "string")),
  plotBox = list(label = "Box Plot", group = "Plots", fn = NULL, engine = "graphics",
    args = list(vars = "vars", group = "factors", box = "bool", violin = "bool", dot = "bool", boxMean = "bool", labelOutliers = "bool", title = "string", xTitle = "string", yTitle = "string")),
  plotHist = list(label = "Histogram", group = "Plots", fn = NULL, engine = "graphics",
    args = list(var = "var", group = "factors", bins = "num", density = "bool", normal = "bool", title = "string", xTitle = "string", yTitle = "string")),
  plotScatter = list(label = "Scatter Plot", group = "Plots", fn = NULL, engine = "graphics",
    args = list(x = "var", y = "var", group = "factors", line = "enum:none,linear,smooth", se = "bool", ciWidth = "num", title = "string", xTitle = "string", yTitle = "string")),
  plotLine = list(label = "Line Plot", group = "Plots", fn = NULL, engine = "graphics",
    args = list(dep = "var", x = "any", legend = "factors", errorBars = "enum:se,ci,none", ciWidth = "num", title = "string", xTitle = "string", yTitle = "string")),
  plotRadar = list(label = "Radar Plot", group = "Plots", fn = NULL, engine = "graphics",
    args = list(vars = "vars", group = "factors", stat = "enum:mean,median", scale = "enum:none,minmax,z", fill = "bool", title = "string")),
  ggplot = list(label = "ggplot2 Builder", group = "Plots", fn = NULL, engine = "ggplot2", args = list()),
  efa = list(label = "Exploratory Factor Analysis", group = "Factor", fn = "efa",
    args = list(vars = "vars", nFactorMethod = "enum:parallel,eigen,fixed", nFactors = "num", minEigen = "num", extraction = "enum:minres,ml,pa",
                rotation = "enum:none,varimax,quartimax,promax,oblimin,simplimax", hideLoadings = "num", sortLoadings = "bool", eigen = "bool", factorCor = "bool",
                factorSummary = "bool", modelFit = "bool", kmo = "bool", bartlett = "bool", screePlot = "bool")),
  pca = list(label = "Principal Component Analysis", group = "Factor", fn = "pca",
    args = list(vars = "vars", nFactorMethod = "enum:parallel,eigen,fixed", nFactors = "num", minEigen = "num",
                rotation = "enum:none,varimax,quartimax,promax,oblimin,simplimax", hideLoadings = "num", sortLoadings = "bool", eigen = "bool", factorCor = "bool",
                factorSummary = "bool", kmo = "bool", bartlett = "bool", screePlot = "bool")),
  cfa = list(label = "Confirmatory Factor Analysis", group = "Factor", fn = "cfa",
    args = list(factors = "latent", resCov = "pairs", miss = "enum:fiml,listwise", constrain = "enum:facVar,facInd", estTest = "bool", ci = "bool", ciWidth = "num",
                stdEst = "bool", factCovEst = "bool", factInterceptEst = "bool", resCovEst = "bool", resInterceptEst = "bool",
                fitMeasures = "enums:cfi,tli,srmr,rmsea,aic,bic", modelTest = "bool", corRes = "bool", hlCorRes = "num", mi = "bool", hlMI = "num"),
    extra = list(pathDiagram = "bool")),   # drawn in the browser from the estimates (jamovi draws it with semPlot)
  reliability = list(label = "Reliability Analysis", group = "Factor", fn = "reliability",
    args = list(vars = "vars", alphaScale = "bool", omegaScale = "bool", meanScale = "bool", sdScale = "bool", alphaItems = "bool", omegaItems = "bool",
                meanItems = "bool", sdItems = "bool", itemRestCor = "bool", revItems = "vars"))
)

stats_type <- function(t) sub(":.*$", "", t)
stats_values <- function(t) strsplit(sub("^[a-z]+:", "", t), ",", fixed = TRUE)[[1]]

#' Validate the options of one procedure against its whitelist (unknown options are dropped).
normalize_stats <- function(raw) {
  raw <- raw %||% list()
  proc <- as.character(raw$proc %||% "")[1]
  if (identical(proc, "ggplot")) {   # ggplot2 builder: its own grammar (R/ggbuilder.R)
    ms <- raw$measures %||% list()
    return(list(proc = proc, options = list(spec = normalize_gg(raw$options$spec)), measures = lapply(ms, function(m) { m <- as.character(m)[1]; if (m %in% c("nominal", "ordinal", "continuous")) m else "continuous" }),
                levels = normalize_stats_levels(raw$levels)))
  }
  def <- STATS_PROCS[[proc]]
  if (is.null(def)) api_error(400, "invalid_stats", "Unknown statistics procedure.")
  o <- raw$options %||% list()
  spec <- c(def$args, def$extra %||% list())
  out <- list()
  for (nm in names(spec)) {
    v <- o[[nm]]
    if (is.null(v)) next
    t <- spec[[nm]]
    out[[nm]] <- switch(stats_type(t),
      vars = , factors = unique(as_chr_vec(v)),
      var = , any = { x <- as.character(v)[1]; if (nzchar(x)) x else NULL },
      bool = isTRUE(v),
      num = { x <- suppressWarnings(as.numeric(v))[1]; if (is.na(x)) api_error(400, "invalid_stats", sprintf("'%s' must be a number.", nm)); x },
      enum = { x <- as.character(v)[1]; if (!x %in% stats_values(t)) api_error(400, "invalid_stats", sprintf("'%s' has an invalid value.", nm)); x },
      enums = { x <- unique(as_chr_vec(v)); bad <- setdiff(x, stats_values(t)); if (length(bad)) api_error(400, "invalid_stats", sprintf("'%s' has an invalid value.", nm)); x },
      string = as.character(v)[1],
      terms = lapply(v, function(x) unique(as_chr_vec(x))),
      pairs = Filter(function(p) nzchar(p$i1) && nzchar(p$i2), lapply(v, function(p) list(i1 = as.character(p$i1 %||% "")[1], i2 = as.character(p$i2 %||% "")[1]))),
      latent = lapply(seq_along(v), function(i) {
        f <- v[[i]]; lab <- trimws(as.character(f$label %||% "")[1])
        list(label = if (is.na(lab) || !nzchar(lab)) paste("Factor", i) else lab, vars = unique(as_chr_vec(f$vars %||% list())))
      }))
  }
  measures <- raw$measures %||% list()
  list(proc = proc, options = out, measures = lapply(measures, function(m) { m <- as.character(m)[1]; if (m %in% c("nominal", "ordinal", "continuous")) m else "continuous" }),
       levels = normalize_stats_levels(raw$levels))
}

#' jamovi's Levels: per variable, its values in the chosen order with their labels ({value, label}); an empty label
#' keeps the value. Labels must be unique within a variable.
normalize_stats_levels <- function(raw) {
  out <- list()
  for (nm in names(raw %||% list())) {
    lv <- Filter(function(l) is.list(l) && length(l$value), raw[[nm]])
    if (!length(lv)) next
    values <- vapply(lv, function(l) as.character(l$value)[1], "")
    labels <- vapply(lv, function(l) { x <- trimws(as.character(l$label %||% "")[1]); if (is.na(x) || !nzchar(x)) "" else x }, "")
    labels[!nzchar(labels)] <- values[!nzchar(labels)]
    if (anyDuplicated(values) || anyDuplicated(labels)) api_error(400, "invalid_stats", sprintf("The levels of '%s' must have different values and labels.", nm))
    out[[nm]] <- list(values = values, labels = labels)
  }
  out
}

#' A factor with the user's level order and labels (jamovi's Levels). Values not in the list follow in their own order;
#' levels that do not occur in the data are dropped (jamovi's default, "Retain unused levels" off).
labelled_factor <- function(x, lv, ordered = FALSE) {
  xs <- as.character(x); xs[is.na(x)] <- NA_character_
  seen <- unique(xs[!is.na(xs)])
  rest <- setdiff(if (is.numeric(x)) as.character(sort(unique(x[!is.na(x)]))) else sort(seen), lv$values)
  droplevels(factor(xs, levels = c(lv$values, rest), labels = c(lv$labels, rest), ordered = ordered))
}

#' Variables an analysis uses, and those that enter as factors.
stats_vars <- function(s) {
  spec <- c(STATS_PROCS[[s$proc]]$args, STATS_PROCS[[s$proc]]$extra %||% list())
  all <- character(0); fac <- character(0); anyv <- character(0)
  for (nm in names(s$options)) {
    t <- stats_type(spec[[nm]])
    v <- s$options[[nm]]
    if (t %in% c("vars", "var")) all <- c(all, v)
    if (t == "any") { all <- c(all, v); anyv <- c(anyv, v) }   # numeric or text (e.g. the event of a survival analysis)
    if (t == "factors") { all <- c(all, v); fac <- c(fac, v) }
    if (t == "pairs") all <- c(all, unlist(lapply(v, function(p) c(p$i1, p$i2))))
    if (t == "latent") all <- c(all, unlist(lapply(v, function(f) f$vars)))
  }
  list(all = unique(all), factors = unique(fac), any = unique(anyv))
}

validate_stats <- function(df, s) {
  out <- list()
  add <- function(code, msg) out[[length(out) + 1]] <<- list(severity = "error", code = code, message = msg)
  v <- stats_vars(s)
  miss <- setdiff(v$all, names(df))
  if (length(miss)) add("stats_missing_column", sprintf("Not in the dataset: %s.", paste(miss, collapse = ", ")))
  num_needed <- setdiff(v$all, c(v$factors, v$any))
  bad <- Filter(function(x) x %in% names(df) && !is.numeric(df[[x]]) && !identical(s$measures[[x]], "nominal") && !identical(s$measures[[x]], "ordinal"), num_needed)
  if (length(bad)) add("stats_not_numeric", sprintf("These variables are not numeric: %s.", paste(bad, collapse = ", ")))
  req <- switch(s$proc,
    descriptives = if (!length(s$options$vars)) "Add at least one variable.",
    ttestIS = if (!length(s$options$vars) || !length(s$options$group)) "Add dependent variables and a grouping variable.",
    ttestPS = if (!length(s$options$pairs)) "Add at least one pair of variables.",
    ttestOneS = if (!length(s$options$vars)) "Add at least one dependent variable.",
    anovaOneW = , anovaNP = if (!length(s$options$deps) || !length(s$options$group)) "Add dependent variables and a grouping variable.",
    ANOVA = , ancova = if (is.null(s$options$dep) || !length(s$options$factors)) "Add the dependent variable and at least one fixed factor.",
    anovaRM = if (length(s$options$rmMeasures) < 2) "Add at least two repeated measures (levels of the within-subjects factor).",
    anovaRMNP = if (length(s$options$measures) < 2) "Add at least two measures.",
    corrMatrix = if (length(s$options$vars) < 2) "Add at least two variables.",
    corrPart = if (length(s$options$vars) < 2 || !length(s$options$controls)) "Add at least two variables and one control variable.",
    contTables = , contTablesPaired = if (!length(s$options$rows) || !length(s$options$cols)) "Add a row and a column variable.",
    propTest2 = if (!length(s$options$vars)) "Add at least one variable.",
    propTestN = if (is.null(s$options$var)) "Add a variable.",
    reliability = if (length(s$options$vars) < 2) "Add at least two items.",
    efa = , pca = if (length(s$options$vars) < 3) "Add at least three variables.",
    cfa = {
      fs <- Filter(function(f) length(f$vars) > 0, s$options$factors %||% list())
      labs <- vapply(fs, function(f) f$label, "")
      if (!length(fs)) "Add at least one factor with its indicators."
      else if (any(vapply(fs, function(f) length(f$vars) < 2, logical(1)))) "Each factor needs at least two indicators."
      else if (length(fs) == 1 && length(fs[[1]]$vars) < 3) "A single factor needs at least three indicators."
      else if (anyDuplicated(tolower(labs))) "Give each factor a different name."
    },
    bcorr = if (length(s$options$vars) < 2) "Add at least two variables.",
    km = if (is.null(s$options$time) || is.null(s$options$event)) "Add the time and the event variable.",
    cox = if (is.null(s$options$time) || is.null(s$options$event) || !length(c(s$options$covs, s$options$factors))) "Add the time, the event and at least one predictor.",
    panel = if (is.null(s$options$dep) || !length(c(s$options$covs, s$options$factors)) || is.null(s$options$id) || is.null(s$options$time)) "Add the dependent variable, at least one predictor, the panel ID and the time variable.",
    pgmm = if (is.null(s$options$dep) || is.null(s$options$id) || is.null(s$options$time)) "Add the dependent variable, the panel ID and the time variable.",
    purtest = if (!length(s$options$vars) || is.null(s$options$id) || is.null(s$options$time)) "Add at least one variable, the panel ID and the time variable.",
    manova = if (length(s$options$deps) < 2 || !length(s$options$factors)) "Add at least two dependent variables and one factor.",
    logRegMulti = , logRegOrd = if (!length(s$options$dep) || !length(c(s$options$covs, s$options$factors))) "Add the dependent variable and at least one predictor.",
    glmx = , bayesreg = if (is.null(s$options$dep) || !length(c(s$options$covs, s$options$factors))) "Add the dependent variable and at least one predictor.",
    mixed = if (is.null(s$options$dep) || !length(s$options$cluster)) "Add the dependent variable and the cluster (grouping) variable.",
    iv2sls = if (is.null(s$options$dep) || !length(s$options$endog) || !length(s$options$instruments)) "Add the dependent variable, an endogenous predictor and an instrument.",
    cancor = if (!length(s$options$set1) || !length(s$options$set2)) "Add the variables of both sets.",
    runs = , acf = if (!length(s$options$vars)) "Add at least one variable.",
    corresp = if (!length(s$options$rows) || !length(s$options$cols)) "Add a row and a column variable.",
    mds = , kmeans = , hclust = if (length(s$options$vars) < 2) "Add at least two variables.",
    mva = if (!length(s$options$vars)) "Add the variables to analyse.",
    lgc = , gmm = if (length(s$options$measures) < 3) "Add at least three repeated measures (waves).",
    lca = if (length(s$options$items) < 2) "Add at least two categorical indicators.",
    lta = if (length(Filter(function(w) length(w$vars) > 0, s$options$waves %||% list())) < 2) "Add at least two waves with the same indicators.",
    lifetable = if (is.null(s$options$time) || is.null(s$options$event)) "Add the time and the event variable.",
    lda = if (!length(s$options$group) || !length(s$options$vars)) "Add the grouping variable and at least one predictor.",
    arima = if (is.null(s$options$var)) "Add the series variable.",
    mi = if (is.null(s$options$dep) || !length(s$options$covs)) "Add the dependent variable and at least one predictor.",
    meta = NULL, power = NULL, soper_sem = NULL, soper_mr = NULL,
    qc = if (is.null(s$options$measure)) "Add the measurement variable.",
    capability = if (is.null(s$options$measure) || (is.null(s$options$lsl) && is.null(s$options$usl))) "Add the measurement variable and a specification limit.",
    banova = if (is.null(s$options$dep) || !length(s$options$factors)) "Add the dependent variable and at least one factor.",
    plotBar = if (is.null(s$options$var)) "Add a variable.",
    plotBox = if (!length(s$options$vars)) "Add at least one variable.",
    plotHist = if (is.null(s$options$var)) "Add a variable.",
    plotScatter = if (is.null(s$options$x) || is.null(s$options$y)) "Add the X and the Y variable.",
    plotLine = if (is.null(s$options$dep) || is.null(s$options$x)) "Add the dependent variable and the X-axis variable.",
    plotRadar = if (length(s$options$vars) < 3) "Add at least three variables.",
    NULL)
  if (!is.null(req)) add("stats_incomplete", req)
  if (s$proc == "ttestIS" && length(s$options$group) && s$options$group %in% names(df)) {
    k <- length(unique(stats::na.omit(df[[s$options$group]])))
    if (k != 2) add("stats_two_groups", sprintf("The grouping variable must have exactly two levels; '%s' has %d. Use a one-way ANOVA for more groups.", s$options$group, k))
  }
  out
}

#' Analysis copy of the data: missing-value code -> NA; factors for nominal variables and for variables used as factors
#' (ordered when ordinal). Nothing is changed in the dataset itself.
stats_data <- function(df, s, missing_code) {
  v <- stats_vars(s)
  d <- df[, v$all, drop = FALSE]
  recoded <- 0L
  for (cn in names(d)) {
    if (!is.null(missing_code) && is.numeric(d[[cn]])) { hit <- !is.na(d[[cn]]) & d[[cn]] == missing_code; recoded <- recoded + sum(hit); d[[cn]][hit] <- NA }
    m <- s$measures[[cn]]
    # Factors: variables in factor slots (grouping, rows, columns ...) and nominal variables. Ordinal variables used as
    # outcomes stay numeric (as in JASP), so t-tests and correlations can use Likert items.
    if (cn %in% v$factors || identical(m, "nominal")) {
      lv <- s$levels[[cn]]
      if (!is.null(lv)) d[[cn]] <- labelled_factor(d[[cn]], lv, ordered = identical(m, "ordinal"))
      else if (!is.factor(d[[cn]])) d[[cn]] <- if (identical(m, "ordinal")) factor(d[[cn]], ordered = TRUE) else factor(d[[cn]])
    }
  }
  w <- attr(df, "pl-weights")
  if (!is.null(w)) { attr(d, "jmv-weights") <- w; attr(d, "jmv-weights-name") <- attr(df, "pl-weights-name") }
  list(data = d, recoded = recoded, factors = v$factors, weighted = !is.null(w), filtered = attr(df, "pl-filtered") %||% 0L)
}

#' Plain values of a jmv cell (NaN / NULL -> NA; HTML-free text).
stats_cell <- function(x) {
  if (is.null(x) || !length(x)) return(NULL)
  if (is.numeric(x)) { if (is.nan(x) || is.na(x)) return(NULL); return(unname(x)) }
  x <- as.character(x)
  if (!length(x) || is.na(x)) NULL else x
}

strip_html <- function(x) gsub("<[^>]+>", "", x)

#' Convert every visible, filled jmv table of a results element into a plain list. Each table keeps the jamovi section it
#' sits in (the title of its group at the top level, e.g. "Assumption Checks"; "" for the analysis' own tables), and
#' attr "plotsAt" counts the tables before jamovi's Plots group.
jmv_tables <- function(el, out = list(), section = NULL) {
  if (inherits(el, "Table")) {
    if (!isTRUE(el$visible) || el$rowCount == 0) return(out)
    cols <- unname(Filter(function(cc) isTRUE(cc$visible), el$columns))   # unnamed: a JSON array
    nms <- vapply(cols, function(cc) cc$name, "")
    rows <- lapply(seq_len(el$rowCount), function(i) {
      cells <- lapply(nms, function(nm) tryCatch(el$getCell(rowNo = i, nm), error = function(e) NULL))
      r <- setNames(lapply(cells, function(cl) stats_cell(cl$value)), nms)
      # Significance stars ("*", "**", "***") jmv adds as cell symbols: kept beside the numbers, which stay numeric.
      sym <- setNames(vapply(cells, function(cl) paste(cl$symbols, collapse = ""), ""), nms)
      if (any(nzchar(sym))) r$.sym <- as.list(sym[nzchar(sym)])
      r
    })
    notes <- unname(vapply(el$notes, function(n) strip_html(n$note), ""))
    fn <- character(0)
    for (i in seq_len(el$rowCount)) for (cc in cols) { cl <- tryCatch(el$getCell(rowNo = i, cc$name), error = function(e) NULL); if (!is.null(cl) && length(cl$footnotes)) fn <- c(fn, strip_html(cl$footnotes)) }
    out[[length(out) + 1]] <- list(
      id = el$name, title = strip_html(el$title),
      columns = lapply(cols, function(cc) list(name = cc$name, title = strip_html(cc$title %||% ""), superTitle = strip_html(cc$superTitle %||% ""),
                                               type = cc$type, pvalue = grepl("pvalue", cc$format %||% ""), combineBelow = isTRUE(cc$combineBelow))),
      rows = rows, notes = as.list(unique(c(notes, fn))), section = section %||% "")
    return(out)
  }
  if (inherits(el, "Group") || inherits(el, "Array")) {
    if (!isTRUE(tryCatch(el$visible, error = function(e) TRUE))) return(out)
    # A Group's children by name: an element may itself be called "items" (reliability), shadowing Group$items.
    kids <- if (inherits(el, "Group")) lapply(names(el), function(n) el[[n]]) else el$items
    for (it in kids) if (inherits(it, "ResultsElement")) {
      if (is.null(section) && identical(it$name, "plots")) attr(out, "plotsAt") <- length(out)
      out <- jmv_tables(it, out, section %||% if (inherits(it, "Table")) "" else strip_html(it$title %||% ""))
    }
  }
  out
}

#' Shapiro-Wilk (stats), Kolmogorov-Smirnov with the Lilliefors correction and Anderson-Darling (nortest) for one sample.
normality_row <- function(x) {
  x <- as.numeric(stats::na.omit(x))
  t <- function(f, min_n) if (length(x) >= min_n && length(unique(x)) > 1) tryCatch(f(x), error = function(e) NULL) else NULL
  sw <- if (length(x) <= 5000) t(stats::shapiro.test, 3) else NULL; ks <- t(nortest::lillie.test, 5); ad <- t(nortest::ad.test, 8)
  list(n = length(x),
       swW = if (!is.null(sw)) unname(sw$statistic), swP = if (!is.null(sw)) sw$p.value,
       ksD = if (!is.null(ks)) unname(ks$statistic), ksP = if (!is.null(ks)) ks$p.value,
       adA = if (!is.null(ad)) unname(ad$statistic), adP = if (!is.null(ad)) ad$p.value)
}
normality_cols <- function() list(
  list(name = "n", title = "N", superTitle = "", type = "integer", pvalue = FALSE, combineBelow = FALSE),
  list(name = "swW", title = "W", superTitle = "Shapiro-Wilk", type = "number", pvalue = FALSE, combineBelow = FALSE),
  list(name = "swP", title = "p", superTitle = "Shapiro-Wilk", type = "number", pvalue = TRUE, combineBelow = FALSE),
  list(name = "ksD", title = "D", superTitle = "Kolmogorov-Smirnov (Lilliefors)", type = "number", pvalue = FALSE, combineBelow = FALSE),
  list(name = "ksP", title = "p", superTitle = "Kolmogorov-Smirnov (Lilliefors)", type = "number", pvalue = TRUE, combineBelow = FALSE),
  list(name = "adA", title = "A", superTitle = "Anderson-Darling", type = "number", pvalue = FALSE, combineBelow = FALSE),
  list(name = "adP", title = "p", superTitle = "Anderson-Darling", type = "number", pvalue = TRUE, combineBelow = FALSE))
NORMALITY_NOTE <- "A low p-value suggests a violation of the assumption of normality. Shapiro-Wilk needs 3-5000 cases, Lilliefors 5 or more and Anderson-Darling 8 or more."

#' The assumption check of the t-tests and ANOVAs with all three tests (jmv reports Shapiro-Wilk only). `sets`: named list
#' of samples, i.e. the variables, the paired differences or the model residuals jmv tests.
stats_normality_sets <- function(sets, what) {
  list(id = "norm", title = "Normality Tests", columns = c(list(list(name = "var", title = "", superTitle = "", type = "text", pvalue = FALSE, combineBelow = FALSE)), normality_cols()),
       rows = lapply(names(sets), function(nm) c(list(var = nm), normality_row(sets[[nm]]))),
       notes = list(paste0("Tested on ", what, ". ", NORMALITY_NOTE)))
}

#' Samples whose normality the assumption check tests, as jmv does: one-sample variables, paired differences, residuals
#' from the group means (independent t-test, one-way ANOVA) or from the linear model (ANOVA, ANCOVA).
assumption_samples <- function(proc, d, o) {
  cc <- function(cols) d[stats::complete.cases(d[cols]), cols, drop = FALSE]
  grp_resid <- function(dep, g) { x <- cc(c(dep, g)); x[[dep]] - stats::ave(x[[dep]], x[[g]]) }
  switch(proc,
    ttestOneS = list(samples = setNames(lapply(o$vars, function(v) d[[v]]), unlist(o$vars)), what = "each variable"),
    ttestPS = list(samples = setNames(lapply(o$pairs, function(p) { x <- cc(c(p$i1, p$i2)); x[[p$i1]] - x[[p$i2]] }), vapply(o$pairs, function(p) paste(p$i1, "-", p$i2), "")), what = "the paired differences"),
    ttestIS = list(samples = setNames(lapply(o$vars, function(v) grp_resid(v, o$group[[1]])), unlist(o$vars)), what = "the residuals from the group means"),
    anovaOneW = list(samples = setNames(lapply(o$deps, function(v) grp_resid(v, o$group[[1]])), unlist(o$deps)), what = "the residuals from the group means"),
    ANOVA = , ancova = {
      q <- function(x) sprintf("`%s`", x)
      fx <- unlist(o$factors); cv <- unlist(o$covs)
      rhs <- c(if (length(fx)) paste(q(fx), collapse = " * "), if (length(cv)) q(cv))
      x <- cc(c(o$dep, fx, cv))
      fit <- stats::lm(stats::as.formula(paste(q(o$dep), "~", paste(rhs, collapse = " + "))), data = x)
      list(samples = setNames(list(stats::residuals(fit)), o$dep), what = "the model residuals")
    },
    NULL)
}

#' Shapiro-Wilk (stats), Kolmogorov-Smirnov with the Lilliefors correction and Anderson-Darling (nortest) for each
#' variable, overall or by the levels of the split variable(s).
stats_normality <- function(d, vars, split) {
  groups <- if (length(split)) split(seq_len(nrow(d)), interaction(d[split], drop = TRUE, sep = " / ")) else list(All = seq_len(nrow(d)))
  rows <- list()
  for (v in vars) for (g in names(groups)) rows[[length(rows) + 1]] <- c(list(var = v), if (length(split)) list(group = g), normality_row(d[[v]][groups[[g]]]))
  cols <- c(list(list(name = "var", title = "", superTitle = "", type = "text", pvalue = FALSE, combineBelow = TRUE)),
            if (length(split)) list(list(name = "group", title = paste(split, collapse = " / "), superTitle = "", type = "text", pvalue = FALSE, combineBelow = FALSE)),
            list(list(name = "n", title = "N", superTitle = "", type = "integer", pvalue = FALSE, combineBelow = FALSE),
                 list(name = "swW", title = "W", superTitle = "Shapiro-Wilk", type = "number", pvalue = FALSE, combineBelow = FALSE),
                 list(name = "swP", title = "p", superTitle = "Shapiro-Wilk", type = "number", pvalue = TRUE, combineBelow = FALSE),
                 list(name = "ksD", title = "D", superTitle = "Kolmogorov-Smirnov (Lilliefors)", type = "number", pvalue = FALSE, combineBelow = FALSE),
                 list(name = "ksP", title = "p", superTitle = "Kolmogorov-Smirnov (Lilliefors)", type = "number", pvalue = TRUE, combineBelow = FALSE),
                 list(name = "adA", title = "A", superTitle = "Anderson-Darling", type = "number", pvalue = FALSE, combineBelow = FALSE),
                 list(name = "adP", title = "p", superTitle = "Anderson-Darling", type = "number", pvalue = TRUE, combineBelow = FALSE)))
  list(id = "normality", title = "Normality Tests", columns = cols, rows = rows,
       notes = list("A low p-value suggests a violation of the assumption of normality. Shapiro-Wilk needs 3-5000 cases, Lilliefors 5 or more and Anderson-Darling 8 or more."))
}

#' Data for the plots of Descriptives (computed with base R; drawn in the browser).
stats_plots <- function(d, vars, split, o, factors) {
  groups <- if (length(split)) split(seq_len(nrow(d)), interaction(d[split], drop = TRUE, sep = " / ")) else list(All = seq_len(nrow(d)))
  plots <- list()
  for (v in vars) {
    # Categorical variables (factors, text, true/false) get bar plots only; histograms, box and Q-Q plots need numbers.
    if (is.factor(d[[v]]) || is.character(d[[v]]) || is.logical(d[[v]])) {
      if (isTRUE(o$bar)) plots[[length(plots) + 1]] <- list(type = "bar", var = v, groups = lapply(names(groups), function(g) {
        tb <- table(d[[v]][groups[[g]]]); list(group = g, levels = as.list(names(tb)), counts = as.list(as.integer(tb))) }))
      next
    }
    # jamovi's Box Plots elements are independent and drawn together in one plot: box, violin, data and mean.
    any_box <- isTRUE(o[["box"]]) || isTRUE(o$violin) || isTRUE(o$dot) || isTRUE(o$boxMean)
    per <- lapply(names(groups), function(g) {
      rows <- groups[[g]][!is.na(d[[v]][groups[[g]]])]
      x <- d[[v]][rows]
      if (length(x) < 2) return(list(group = g, n = length(x)))
      out <- list(group = g, n = length(x))
      if (isTRUE(o$hist)) { h <- graphics::hist(x, plot = FALSE); out$hist <- list(breaks = as.list(h$breaks), counts = as.list(h$counts)) }
      # The density curve only when Density is ticked (jamovi): with Histogram too it is drawn over the bars.
      if (isTRUE(o$dens)) { de <- stats::density(x, n = 64); out$density <- list(x = as.list(de$x), y = as.list(de$y)) }
      if (any_box) {
        b <- grDevices::boxplot.stats(x); oi <- which(x < b$stats[1] | x > b$stats[5])
        out$box <- list(stats = as.list(b$stats), outliers = as.list(utils::head(x[oi], 200)), outlierRows = as.list(utils::head(rows[oi], 200)), mean = mean(x))
        if (isTRUE(o$violin)) { dv <- stats::density(x, n = 64, from = min(x), to = max(x)); out$violin <- list(x = as.list(dv$x), y = as.list(dv$y)) }
        if (isTRUE(o$dot)) out$points <- as.list(x[plot_sample(seq_along(x), 1000)])
      }
      if (isTRUE(o$qq)) out$qq <- qq_points(x)
      out
    })
    # jamovi's Bar plot of a continuous variable: the mean of each group with +/- 1 standard error.
    if (isTRUE(o$bar)) {
      ms <- lapply(names(groups), function(g) { x <- stats::na.omit(d[[v]][groups[[g]]]); n <- length(x)
        list(m = if (n) mean(x) else NA_real_, se = if (n > 1) stats::sd(x) / sqrt(n) else NA_real_, n = n) })
      num <- function(k) lapply(ms, function(z) if (is.na(z[[k]])) NULL else z[[k]])
      plots[[length(plots) + 1]] <- list(type = "means", style = "bar", var = v, xVar = if (length(split)) paste(split, collapse = " / ") else "", legend = "",
        categories = as.list(if (length(split)) names(groups) else v), errorBars = "se",
        series = list(list(name = v, values = num("m"), lower = lapply(ms, function(z) if (is.na(z$se)) NULL else z$m - z$se),
                           upper = lapply(ms, function(z) if (is.na(z$se)) NULL else z$m + z$se), n = lapply(ms, `[[`, "n"))))
    }
    # Nothing ticked to draw (or too few values): no figure at all, not an empty one.
    if (any(vapply(per, function(g) any(c("hist", "density", "box", "qq") %in% names(g)), logical(1))))
      plots[[length(plots) + 1]] <- list(type = "continuous", var = v, groups = per, showBox = isTRUE(o[["box"]]), showMean = isTRUE(o$boxMean),
                                         labelOutliers = isTRUE(o[["box"]]) && isTRUE(o$boxLabelOutliers))
  }
  plots
}

#' Normal Q-Q points of x (at most 500, evenly spaced) and the reference line of stats::qqline.
qq_points <- function(x) {
  q <- stats::qqnorm(x, plot.it = FALSE)
  sel <- if (length(x) > 500) round(seq(1, length(x), length.out = 500)) else seq_along(x)
  ord <- order(q$x)
  yq <- stats::quantile(x, c(0.25, 0.75), names = FALSE); xq <- stats::qnorm(c(0.25, 0.75))
  slope <- diff(yq) / diff(xq)
  list(x = as.list(q$x[ord][sel]), y = as.list(q$y[ord][sel]), intercept = yq[1] - slope * xq[1], slope = slope)
}

# ---- Plots of the t-tests and ANOVAs (jamovi's descriptives / estimated marginal means plots and Q-Q plots) ------------

TEST_PLOT_PROCS <- c(ttestIS = "group", ttestPS = "pairs", ttestOneS = "one", anovaOneW = "group", ANOVA = "emm", ancova = "emm", anovaRM = "rm")

#' Mean and t-based 95% confidence interval of y.
mean_ci <- function(y) {
  y <- y[!is.na(y)]; n <- length(y); m <- if (n) mean(y) else NA_real_
  h <- if (n > 1) stats::qt(0.975, n - 1) * stats::sd(y) / sqrt(n) else NA_real_
  list(n = n, mean = m, lower = m - h, upper = m + h)
}
means_series <- function(name, cells)
  list(name = name, n = lapply(cells, `[[`, "n"), values = lapply(cells, `[[`, "mean"), lower = lapply(cells, `[[`, "lower"), upper = lapply(cells, `[[`, "upper"))
means_plot <- function(style, var, xvar, cats, series, ytitle, legend = "", note = NULL, ref = NULL)
  c(list(type = "means", style = style, var = var, xVar = xvar, legend = legend, categories = as.list(cats), series = series, errorBars = "ci"),
    if (!is.null(ref)) list(ref = ref), if (!is.null(note)) list(note = note), list(title = "", xTitle = xvar, yTitle = ytitle))
#' A normal Q-Q plot of standardized residuals, drawn like the Q-Q plots of Descriptives.
resid_qq <- function(name, r) {
  r <- r[is.finite(r)]
  if (length(r) < 3 || stats::sd(r) == 0) return(NULL)
  z <- (r - mean(r)) / stats::sd(r)
  list(type = "continuous", var = name, groups = list(list(group = "All", n = length(z), qq = qq_points(z))), title = "", xTitle = "Theoretical quantiles", yTitle = "Standardized residuals")
}

#' The plots a t-test or ANOVA asks for: means with 95% confidence intervals (model-based estimated marginal means for
#' ANOVA and ANCOVA, as jamovi draws them) and normal Q-Q plots of the standardized residuals that the normality tests use.
test_plots <- function(proc, d, o) {
  plots <- list()
  add <- function(p, head = p$var) if (!is.null(p)) { p$head <- head; plots[[length(plots) + 1]] <<- p }
  cc <- function(cols) d[stats::complete.cases(d[cols]), cols, drop = FALSE]
  ci_note <- "Means with 95% confidence intervals."
  if (isTRUE(o$meansPlot)) switch(TEST_PLOT_PROCS[[proc]],
    group = {
      g <- o$group[[1]]
      for (v in unlist(if (proc == "ttestIS") o$vars else o$deps)) {
        x <- cc(c(v, g)); f <- droplevels(factor(x[[g]]))
        add(means_plot("points", v, g, levels(f), list(means_series(v, lapply(levels(f), function(l) mean_ci(x[[v]][f == l])))), v, note = ci_note))
      }
    },
    pairs = for (p in o$pairs) {
      x <- cc(c(p$i1, p$i2))
      add(means_plot("points", paste(p$i1, "-", p$i2), "Measure", c(p$i1, p$i2), list(means_series("Mean", list(mean_ci(x[[p$i1]]), mean_ci(x[[p$i2]])))), "Mean",
                     note = "Means with 95% confidence intervals of the cases with both measures."))
    },
    one = {
      vs <- unlist(o$vars); tv <- o$testValue %||% 0
      add(means_plot("points", paste(vs, collapse = ", "), "Variable", vs, list(means_series("Mean", lapply(vs, function(v) mean_ci(d[[v]])))), "Mean",
                     note = sprintf("Means with 95%% confidence intervals; the dotted line is the test value (%s).", format(tv)), ref = tv))
    },
    emm = {
      fx <- unlist(o$factors); cv <- unlist(o$covs)
      x <- cc(c(o$dep, fx, cv)); for (f in fx) x[[f]] <- droplevels(factor(x[[f]]))
      q <- function(z) sprintf("`%s`", z)
      fit <- stats::lm(stats::as.formula(paste(q(o$dep), "~", paste(c(paste(q(fx), collapse = " * "), if (length(cv)) q(cv)), collapse = " + "))), data = x)
      em <- as.data.frame(summary(emmeans::emmeans(fit, fx[seq_len(min(2, length(fx)))], level = 0.95)))
      cats <- levels(x[[fx[1]]])
      cell <- function(rows) lapply(cats, function(cl) {
        r <- rows[as.character(rows[[fx[1]]]) == cl, , drop = FALSE]
        if (!nrow(r) || !is.finite(r$emmean[1])) list(n = 0, mean = NA_real_, lower = NA_real_, upper = NA_real_) else list(n = NA, mean = r$emmean[1], lower = r$lower.CL[1], upper = r$upper.CL[1])
      })
      two <- length(fx) > 1
      series <- if (two) lapply(levels(x[[fx[2]]]), function(l) means_series(l, cell(em[as.character(em[[fx[2]]]) == l, , drop = FALSE]))) else list(means_series(o$dep, cell(em)))
      add(means_plot(if (two) "line" else "points", o$dep, fx[1], cats, series, o$dep, legend = if (two) fx[2] else "",
                     note = paste0("Estimated marginal means with 95% confidence intervals (emmeans)", if (length(cv)) paste0(", at the mean of ", paste(cv, collapse = ", ")) else "",
                                   if (length(fx) > 2) paste0(", averaged over ", paste(fx[-(1:2)], collapse = ", ")) else "", ".")))
    },
    rm = {
      ms <- unlist(o$rmMeasures); bs <- c(unlist(o$bs), NA_character_)[1]
      x <- cc(c(ms, if (!is.na(bs)) bs))
      lab <- if (nzchar(o$rmLabel %||% "")) o$rmLabel else "RM Factor 1"
      series <- if (!is.na(bs)) { f <- droplevels(factor(x[[bs]])); lapply(levels(f), function(l) means_series(l, lapply(ms, function(m) mean_ci(x[[m]][f == l])))) }
                else list(means_series("Mean", lapply(ms, function(m) mean_ci(x[[m]]))))
      add(means_plot("line", lab, lab, ms, series, "Mean", legend = if (!is.na(bs)) bs else "", note = "Means with 95% confidence intervals of the cases with every measure."))
    })
  if (isTRUE(o$qq)) {
    a <- tryCatch(assumption_samples(proc, d, o), error = function(e) NULL)
    if (!is.null(a)) for (nm in names(a$samples)) add(resid_qq(if (proc == "ttestOneS") nm else paste0(nm, " (", sub("^the ", "", a$what), ")"), a$samples[[nm]]), nm)
  }
  # A variable's plots together under its name, as in jamovi.
  hs <- vapply(plots, function(p) p$head, "")
  plots[order(match(hs, unique(hs)))]
}

#' A warning as shown under the results: R's bare convergence notes are dropped (the procedures explain them in words).
stats_warning_text <- function(msg) {
  m <- sanitize_message(msg)
  if (grepl("^(iteration|alternation) limit reached$", m)) character(0) else m
}

NORM_ASSUMPTION_PROCS <- c("ttestIS", "ttestPS", "ttestOneS", "anovaOneW", "ANOVA", "ancova")
#' Whether the result includes the Kolmogorov-Smirnov and Anderson-Darling tests (computed with nortest).
uses_nortest <- function(proc, o) (identical(proc, "descriptives") && (isTRUE(o$normTests) || isTRUE(o$sw))) || (proc %in% NORM_ASSUMPTION_PROCS && isTRUE(o$norm))

#' Run one procedure and add the references of R and the packages that computed it (shown under the results, as in jamovi).
run_stats <- function(df, s, missing_code = NULL) {
  res <- run_stats_core(df, s, missing_code)
  res$references <- stats_references(c(names(res$packages), if (uses_nortest(s$proc, s$options)) "nortest"))
  res
}

run_stats_core <- function(df, s, missing_code = NULL) {
  if (identical(s$proc, "ggplot")) return(run_stats_ggplot(df, s, missing_code))
  issues <- validate_stats(df, s)
  if (length(issues)) api_error(422, "invalid_stats", issues[[1]]$message, details = issues)
  if (!requireNamespace("jmv", quietly = TRUE)) api_error(501, "stats_unavailable", "The jmv R package is not installed on the analysis server.", "Run Rscript backend/install_packages.R.")
  t0 <- Sys.time()
  def <- STATS_PROCS[[s$proc]]
  prep <- stats_data(df, s, missing_code)
  d <- prep$data
  o <- s$options
  args <- o[intersect(names(o), names(def$args))]
  # Translations to jmv's argument forms.
  if (s$proc == "anovaRM") {
    lab <- if (nzchar(o$rmLabel %||% "")) o$rmLabel else "RM Factor 1"
    args$rm <- list(list(label = lab, levels = as.list(o$rmMeasures)))
    args$rmCells <- lapply(o$rmMeasures, function(m) list(measure = m, cell = m))
    args$rmMeasures <- NULL; args$rmLabel <- NULL
  }
  if (s$proc == "ttestPS") args$pairs <- lapply(o$pairs, function(p) list(i1 = p$i1, i2 = p$i2))
  if (s$proc %in% c("efa", "pca") && !is.null(args$nFactors)) args$nFactors <- as.integer(max(1, round(args$nFactors)))
  # Parallel analysis simulates random data: a fixed seed makes the number of factors (and the scree plot) reproducible.
  if (s$proc %in% c("efa", "pca") && identical(o$nFactorMethod %||% "parallel", "parallel")) set.seed(2026)
  if (s$proc == "cfa") {   # jmv's forms: factors as {label, vars}, residual covariances as {i1, i2}
    args$factors <- lapply(Filter(function(f) length(f$vars) > 0, o$factors), function(f) list(label = f$label, vars = as.list(f$vars)))
    args$resCov <- lapply(o$resCov %||% list(), function(p) list(i1 = p$i1, i2 = p$i2))
    if (!is.null(args$fitMeasures)) args$fitMeasures <- as.list(args$fitMeasures)
    # The path diagram needs every estimate: ask jmv for them, and drop afterwards what the user did not ask for.
    if (isTRUE(o$pathDiagram)) { args$stdEst <- TRUE; args$factCovEst <- TRUE; args$resCovEst <- TRUE }
  }
  # Normality is always checked with Shapiro-Wilk, Kolmogorov-Smirnov (Lilliefors) and Anderson-Darling together.
  if (s$proc == "descriptives" && isTRUE(o$sw)) { o$normTests <- TRUE; args$sw <- FALSE }
  for (nm in c("postHoc", "emMeans")) if (!is.null(args[[nm]])) { if (!length(args[[nm]])) args[[nm]] <- NULL }
  for (nm in c("ss")) if (!is.null(args[[nm]])) args[[nm]] <- as.character(args[[nm]])
  if (!is.null(args$rows) && length(args$rows) == 1) args$rows <- args$rows
  for (nm in c("group", "dep", "var", "rows", "cols", "splitBy", "layers")) if (!is.null(args[[nm]]) && length(args[[nm]]) == 0) args[[nm]] <- NULL
  if (s$proc %in% c("ttestIS", "anovaOneW", "anovaNP") && length(args$group)) args$group <- args$group[1]
  if (s$proc %in% c("contTables", "contTablesPaired")) { args$rows <- args$rows[1]; args$cols <- args$cols[1] }
  if (s$proc == "propTestN") args$var <- args$var[1]
  if (s$proc %in% c("logRegMulti", "logRegOrd")) { args$dep <- args$dep[1]; args$blocks <- list(as.list(c(o$covs, o$factors))) }
  warns <- character(0)
  if (!is.null(def$engine) && isTRUE(prep$weighted)) d <- expand_weights(d)   # frequency weights: rows repeated
  if (!is.null(def$engine)) {   # survival / quality control
    if (!requireNamespace(def$engine, quietly = TRUE)) api_error(501, "stats_unavailable", sprintf("The %s R package is not installed on the analysis server.", def$engine))
    out <- withCallingHandlers(tryCatch(if (!is.null(STATS_MORE_RUN[[s$proc]])) STATS_MORE_RUN[[s$proc]](d, o) else switch(s$proc, km = run_km(d, o), cox = run_cox(d, o), panel = run_panel(d, o), pgmm = run_pgmm(d, o), purtest = run_purtest(d, o), qc = run_qc(d, o), capability = run_capability(d, o),
                                               plotBar = run_plot_bar(d, o), plotBox = run_plot_box(d, o), plotHist = run_plot_hist(d, o), plotScatter = run_plot_scatter(d, o), plotLine = run_plot_line(d, o), plotRadar = run_plot_radar(d, o)),
                                        
                                        error = function(e) if (inherits(e, "pathlab_error")) stop(e) else api_error(422, "stats_failed", paste(def$label, "could not be computed:", sanitize_message(conditionMessage(e))))),
                               warning = function(w) { warns <<- c(warns, stats_warning_text(conditionMessage(w))); invokeRestart("muffleWarning") })
    return(list(kind = "stats", proc = s$proc, label = def$label, group = def$group, options = s$options, measures = s$measures,
                n = list(input = nrow(df), complete = out$n, cellsRecoded = prep$recoded), tables = out$tables, plots = out$plots,
                warnings = as.list(unique(warns)), rCode = if (def$engine == "graphics") stats_plot_code(s, missing_code) else stats_e_code(s, missing_code),
                packages = if (def$engine == "graphics") list(R = paste(R.version$major, R.version$minor, sep = ".")) else setNames(list(as.character(utils::packageVersion(def$engine))), def$engine),
                durationSeconds = as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }
  if (is.null(def$fn)) {   # Bayesian procedures (BayesFactor)
    if (!requireNamespace("BayesFactor", quietly = TRUE)) api_error(501, "stats_unavailable", "The BayesFactor R package is not installed on the analysis server.")
    tables <- withCallingHandlers(tryCatch(if (s$proc == "bcorr") run_bcorr(d, o) else run_banova(d, o, as.integer(o$seed %||% 2026)),
                                           error = function(e) api_error(422, "stats_failed", paste(def$label, "could not be computed:", sanitize_message(conditionMessage(e))))),
                                  warning = function(w) { warns <<- c(warns, stats_warning_text(conditionMessage(w))); invokeRestart("muffleWarning") })
    return(list(kind = "stats", proc = s$proc, label = def$label, group = def$group, options = s$options, measures = s$measures,
                n = list(input = nrow(df), complete = sum(stats::complete.cases(d)), cellsRecoded = prep$recoded), tables = tables, plots = list(),
                warnings = as.list(unique(warns)), rCode = stats_bayes_code(s, prep, missing_code),
                packages = list(BayesFactor = as.character(utils::packageVersion("BayesFactor"))),
                durationSeconds = as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }
  res <- withCallingHandlers(
    tryCatch(do.call(getExportedValue("jmv", def$fn), c(list(data = d), args)),
             error = function(e) api_error(422, "stats_failed", paste(def$label, "could not be computed:", sanitize_message(conditionMessage(e))))),
    warning = function(w) { warns <<- c(warns, stats_warning_text(conditionMessage(w))); invokeRestart("muffleWarning") })
  tables <- jmv_tables(res)
  plots_at <- if (s$proc %in% names(TEST_PLOT_PROCS)) attr(tables, "plotsAt")
  if (s$proc == "ttestIS") tables <- lapply(tables, group_desc_long)
  plots <- list()
  if (s$proc == "descriptives") {
    split <- o$splitBy %||% character(0)
    tables <- lapply(tables, desc_columns_fix, vars = o$vars, split_lab = paste(split, collapse = " / "))
    cont <- Filter(function(v) !is.factor(d[[v]]), o$vars)
    if (isTRUE(o$normTests) && length(cont)) tables[[length(tables) + 1]] <- stats_normality(d, cont, split)
    plots <- stats_plots(d, o$vars, split, o, prep$factors)
  }
  if (s$proc %in% c("efa", "pca") && isTRUE(o$screePlot)) {
    sp <- scree_plot(res, tables, s$proc, o)
    if (!is.null(sp)) plots <- list(sp)
  }
  if (s$proc == "cfa" && isTRUE(o$pathDiagram)) {
    plots <- list(cfa_path_plot(tables, standardized = isTRUE(o$stdEst)))
    tables <- cfa_hide_extra(tables, o)
  }
  if (s$proc %in% NORM_ASSUMPTION_PROCS && isTRUE(o$norm)) {
    a <- tryCatch(assumption_samples(s$proc, d, o), error = function(e) NULL)
    if (!is.null(a)) {
      nt <- stats_normality_sets(a$samples, a$what)
      at <- which(vapply(tables, function(t) t$id %in% c("norm", "normality"), logical(1)))
      nt$section <- if (length(at)) tables[[at[1]]]$section %||% "" else "Assumption Checks"
      if (length(at)) tables[[at[1]]] <- nt else tables[[length(tables) + 1]] <- nt
      if (length(at) > 1) tables <- tables[-at[-1]]
    }
  }
  if (s$proc %in% names(TEST_PLOT_PROCS) && (isTRUE(o$meansPlot) || isTRUE(o$qq))) plots <- c(plots, test_plots(s$proc, d, o))
  # Errors jmv reports inside the results (e.g. too few cases) become warnings.
  code <- tryCatch(res$analysis$asSource(), error = function(e) "")
  list(
    kind = "stats", proc = s$proc, label = def$label, group = def$group,
    options = s$options, measures = s$measures,
    n = list(input = nrow(df), complete = sum(stats::complete.cases(d)), cellsRecoded = prep$recoded),
    tables = tables, plots = plots, plotsAt = plots_at,
    warnings = as.list(unique(warns)),
    rCode = stats_code(s, prep, missing_code, code),
    packages = list(jmv = as.character(utils::packageVersion("jmv"))),
    durationSeconds = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
}

#' R code: data preparation as in the app, then jamovi's own syntax for the analysis.
stats_code <- function(s, prep, missing_code, jmv_src) {
  q <- function(x) paste0("\"", gsub("\"", "\\\\\"", x), "\"")
  vars <- names(prep$data)
  lines <- c(sprintf("# %s generated by PathLab SEM with jmv (jamovi's R package); the analysis call is jamovi's syntax", STATS_PROCS[[s$proc]]$label),
             "library(jmv)", "# data <- read.csv(\"your_data.csv\")   # the same dataset as in the app")
  if (!is.null(missing_code)) lines <- c(lines, paste0("data[data == ", missing_code, "] <- NA   # project missing-value code"))
  for (v in vars) if (is.factor(prep$data[[v]])) lines <- c(lines, sprintf("data[[%s]] <- factor(data[[%s]]%s)", q(v), q(v), if (is.ordered(prep$data[[v]])) ", ordered = TRUE" else ""))
  extra <- if (s$proc == "descriptives" && uses_nortest(s$proc, s$options)) c("", "# Normality tests added by PathLab SEM (Lilliefors-corrected Kolmogorov-Smirnov and Anderson-Darling):",
    "library(nortest)", sprintf("for (v in c(%s)) print(list(var = v, sw = shapiro.test(data[[v]]), ks = lillie.test(data[[v]]), ad = ad.test(data[[v]])))",
                                paste(vapply(Filter(function(v) !is.factor(prep$data[[v]]), s$options$vars), q, ""), collapse = ", "))) else character(0)
  # Assumption checks: a helper for all three normality tests, defined before jamovi's call so that the call stays last.
  pre <- if (s$proc != "descriptives" && uses_nortest(s$proc, s$options)) c("", "# Normality of the assumption check with all three tests (PathLab SEM): Shapiro-Wilk, Lilliefors-corrected",
      "# Kolmogorov-Smirnov and Anderson-Darling, applied to the values jmv tests (variables, paired differences or residuals):",
      "library(nortest)", "norm3 <- function(x) list(sw = shapiro.test(x), ks = lillie.test(x), ad = ad.test(x))") else character(0)
  paste(c(lines, pre, "", jmv_src, extra), collapse = "\n")
}

# ---- Report, workbook and APA tables (generic over the converted jmv tables) ------------------------------

#' jamovi folds the tests of a t-test or one-way ANOVA table into rows: jmv sends one wide row per variable
#' (F[welch] F[fisher] ... p[welch] p[fisher]); jamovi shows a row per test under the variable (and the post hoc
#' matrix as a mean difference row and a p-value row per level; the correlation matrix as a row per statistic under
#' each variable). Only for display: the stored rows stay wide, as the narratives read them.
FOLD_SUFFIXES <- c("stud", "welc", "mann", "bf", "wilc", "welch", "fisher", "md", "p",
                   "r", "rdf", "rp", "rciu", "rcil", "rho", "rhodf", "rhop", "tau", "taup", "n")
FOLD_P <- c("p", "rp", "rhop", "taup")
stats_fold <- function(t) {
  nm <- vapply(t$columns, function(cc) cc$name, "")
  br <- grepl("^.+\\[[^]]+\\]$", nm)
  if (!any(br)) return(t)
  sfx <- sub("^.*\\[([^]]+)\\]$", "\\1", nm[br])
  if (!all(sfx %in% FOLD_SUFFIXES) || length(unique(sfx)) < 2) return(t)
  stem <- ifelse(br, sub("\\[[^]]+\\]$", "", nm), nm)
  stems <- unique(c(nm[!br], stem[br]))
  cols <- lapply(stems, function(s) {
    of <- t$columns[stem == s]; cc <- of[[1]]
    ttl <- Filter(nzchar, vapply(of, function(x) x$title %||% "", "")); if (length(ttl)) cc$title <- ttl[[1]]
    cc$name <- s
    # The variable repeats on each test's row: shown once.
    cc$combineBelow <- identical(cc$type, "text") && !s %in% c("test", "name", ".stat", "esType")
    cc
  })
  sym_of <- function(r, s, x) r[[".sym"]][[if (s %in% nm[!br]) s else paste0(s, "[", x, "]")]]
  ints <- unique(sfx[vapply(t$columns[br], function(cc) identical(cc$type, "integer"), TRUE)])   # df and N rows
  rows <- list()
  for (r in t$rows) for (x in unique(sfx)) {
    vals <- lapply(stems, function(s) if (s %in% nm[!br]) r[[s]] else r[[paste0(s, "[", x, "]")]])
    if (all(vapply(vals[stems %in% stem[br]], is.null, TRUE))) next
    sym <- Filter(Negate(is.null), setNames(lapply(stems, function(s) sym_of(r, s, x)), stems))
    rows[[length(rows) + 1]] <- c(setNames(vals, stems), if (x %in% FOLD_P) list(.p = TRUE), if (x %in% ints) list(.int = TRUE), if (length(sym)) list(.sym = sym))
  }
  t$columns <- cols; t$rows <- rows
  t
}

# Columns printed without decimals when the value is whole: counts and degrees of freedom (df, df1, df2, df1[term]).
stats_int_name <- function(nm) nm %in% c("n", "N", "missing", "count", "obs") || grepl("^df[12]?([[].*])?$", nm)

stats_fmt <- function(v, col, html = TRUE) {
  if (is.null(v)) return(if (html) "&ndash;" else "–")
  if (is.character(v)) return(if (html) html_escape(v) else v)
  if (isTRUE(col$pvalue)) return(if (html) fmt_p(v) else pt_p(v, "apa"))
  if (identical(col$type, "integer") || (is.numeric(v) && v == round(v) && abs(v) >= 1 && stats_int_name(col$name))) return(format(round(v)))
  if (html) fmt(v) else pt_num(v, 3)
}

#' jamovi's significance stars after each formatted value of a row. In HTML they hang to the right with no width
#' (sup.sym), so the figures stay aligned, as in jamovi.
stats_sym <- function(txt, r, cols, html = FALSE) {
  s <- vapply(cols, function(cc) r[[".sym"]][[cc$name]] %||% "", "")
  ifelse(nzchar(txt) & nzchar(s), paste0(txt, if (html) paste0("<sup class=\"sym\">", s, "</sup>") else s), txt)
}

stats_header <- function(cols) vapply(cols, function(cc) { t <- cc$title %||% ""; st <- cc$superTitle %||% ""; if (nzchar(st)) paste0(st, ": ", if (nzchar(t)) t else cc$name) else t }, "")

stats_body_html <- function(res) {
  esc <- html_escape
  S <- character(0); a <- function(...) S <<- c(S, paste0(...))
  a("<h2>", esc(res$label), "</h2>")
  a("<p>Computed with ", if (!is.null(res$packages$R)) paste("R", esc(res$packages$R), "(base graphics and stats)") else if (!is.null(res$packages$survival)) paste("survival", esc(res$packages$survival)) else if (!is.null(res$packages$qcc)) paste("qcc", esc(res$packages$qcc)) else if (!is.null(res$packages$BayesFactor)) paste("BayesFactor", esc(res$packages$BayesFactor), "(as in JASP)") else paste0("jmv ", esc(res$packages$jmv), " (jamovi's R package)"),
    if (identical(res$proc, "descriptives") && isTRUE(res$options$normTests)) " and nortest" else "", ".</p>")
  for (w in ul(res$warnings)) a("<div class=\"warn\">", esc(w), "</div>")
  for (pl in res$plots) if (identical(pl$type, "image")) a("<figure><img alt=\"", esc(pl$alt %||% "Plot"), "\" style=\"max-width:100%\" src=\"data:image/png;base64,", pl$png, "\"></figure>")
  for (t in lapply(res$tables, stats_fold)) {
    hdr <- vapply(stats_header(t$columns), esc, "")
    rows <- lapply(t$rows, function(r) stats_sym(vapply(t$columns, function(cc) stats_fmt(r[[cc$name]], if (isTRUE(r$.p)) modifyList(cc, list(pvalue = TRUE)) else if (isTRUE(r$.int)) modifyList(cc, list(type = "integer")) else cc), ""), r, t$columns, html = TRUE))
    a(html_table(hdr, rows, esc(t$title), num_cols = which(vapply(t$columns, function(cc) !identical(cc$type, "text"), TRUE))))
    for (n in ul(t$notes)) a("<p class=\"note\">", esc(n), "</p>")
  }
  if (length(res$references)) { a("<h3>References</h3><ol class=\"refs\">"); for (r in ul(res$references)) a("<li>", esc(gsub("_", "", r)), "</li>"); a("</ol>") }
  a("<h3>R code</h3><pre><code>", esc(res$rCode %||% ""), "</code></pre>")
  paste(S, collapse = "\n")
}

stats_sheets <- function(res, project_name) {
  out <- list(Info = data.frame(Item = c("Project", "Completed (UTC)", "PathLab SEM version", "Analysis", "jmv"),
                                Value = c(project_name, res$completedAt, PATHLAB_VERSION, res$label, paste(names(res$packages)[1], unlist(res$packages)[1])), stringsAsFactors = FALSE))
  for (i in seq_along(res$tables)) {
    t <- stats_fold(res$tables[[i]])
    df <- as.data.frame(setNames(lapply(t$columns, function(cc) vapply(t$rows, function(r) { v <- r[[cc$name]]; if (is.null(v)) NA_character_ else as.character(v) }, "")),
                                 make.unique(stats_header(t$columns))), stringsAsFactors = FALSE, check.names = FALSE)
    for (j in seq_along(t$columns)) if (!identical(t$columns[[j]]$type, "text")) df[[j]] <- suppressWarnings(as.numeric(df[[j]]))
    nm <- substr(gsub("[^A-Za-z0-9 ]", "", t$title), 1, 28)
    out[[make.unique(c(names(out), if (nzchar(nm)) nm else paste("Table", i)))[length(out) + 1]]] <- df
  }
  out
}

stats_pub_spec <- function(res, opts) {
  d <- opts$decimals; lz <- opts$leadingZero
  tabs <- lapply(seq_along(res$tables), function(i) {
    t <- stats_fold(res$tables[[i]])
    rows <- lapply(t$rows, function(r) stats_sym(vapply(t$columns, function(cc) {
      v <- r[[cc$name]]
      if (is.null(v)) "" else if (is.character(v)) strip_html(v) else if (isTRUE(cc$pvalue) || isTRUE(r$.p)) pt_p(v, lz)
      else if (identical(cc$type, "integer") || isTRUE(r$.int) || (v == round(v) && stats_int_name(cc$name))) format(round(v)) else pt_num(v, d)
    }, ""), r, t$columns))
    # Repeated labels shown once (jamovi's combineBelow).
    for (j in which(vapply(t$columns, function(cc) isTRUE(cc$combineBelow), TRUE))) {
      prev <- NULL
      for (k in seq_along(rows)) { cur <- rows[[k]][j]; if (identical(cur, prev)) rows[[k]][j] <- "" else prev <- cur }
    }
    pt_table(paste0("stats_", i), tools::toTitleCase(t$title), stats_header(t$columns), rows, notes = as.list(ul(t$notes)))
  })
  tabs <- Filter(Negate(is.null), tabs)
  for (i in seq_along(tabs)) tabs[[i]]$number <- i
  tabs
}

# ---- Bayesian procedures (BayesFactor, the package JASP uses) ---------------------------------------------

BF_SCALES <- c(medium = "medium", wide = "wide", ultrawide = "ultrawide")

stats_col <- function(name, title, type = "number", superTitle = "", pvalue = FALSE, combineBelow = FALSE)
  list(name = name, title = title, superTitle = superTitle, type = type, pvalue = pvalue, combineBelow = combineBelow)

#' Bayesian Pearson correlations (BayesFactor::correlationBF) for every pair of variables (pairwise complete cases).
run_bcorr <- function(d, o) {
  vars <- o$vars
  sc <- o$rscale %||% "medium"
  rows <- list()
  for (i in seq_along(vars)) for (j in seq_along(vars)) if (j > i) {
    x <- d[[vars[i]]]; y <- d[[vars[j]]]; ok <- stats::complete.cases(x, y)
    bf <- tryCatch(BayesFactor::extractBF(BayesFactor::correlationBF(x[ok], y[ok], rscale = sc))$bf, error = function(e) NA_real_)
    rows[[length(rows) + 1]] <- list(v1 = vars[i], v2 = vars[j], n = sum(ok), r = stats::cor(x[ok], y[ok]), bf10 = bf, bf01 = 1 / bf)
  }
  list(list(id = "bcorr", title = "Bayesian Pearson Correlations", rows = rows,
            columns = list(stats_col("v1", "", "text", combineBelow = TRUE), stats_col("v2", "", "text"), stats_col("n", "n", "integer"), stats_col("r", "Pearson's r"),
                           stats_col("bf10", "BF₁₀"), stats_col("bf01", "BF₀₁")),
            notes = list(sprintf("BF₁₀: evidence for a correlation against no correlation (BayesFactor::correlationBF, stretched beta prior, rscale = %s). Values above 3 (below 1/3) are commonly read as moderate evidence for (against) a correlation.", sc))))
}

#' Bayesian ANOVA (BayesFactor::anovaBF): Bayes factors of the models against the null model.
run_banova <- function(d, o, seed) {
  f <- o$factors
  form <- stats::as.formula(paste(sprintf("`%s`", o$dep), "~", paste(sprintf("`%s`", f), collapse = " * ")))
  dd <- d[stats::complete.cases(d[, c(o$dep, f)]), c(o$dep, f), drop = FALSE]
  set.seed(seed)
  bf <- BayesFactor::anovaBF(form, data = dd, rscaleFixed = o$rscale %||% "medium", progress = FALSE)
  e <- BayesFactor::extractBF(bf)
  rows <- lapply(seq_len(nrow(e)), function(i) list(model = gsub("`", "", rownames(e)[i]), bf10 = e$bf[i], bf01 = 1 / e$bf[i], error = 100 * e$error[i]))
  rows <- rows[order(-vapply(rows, `[[`, 0, "bf10"))]
  list(list(id = "banova", title = "Bayesian ANOVA: Model Comparison", rows = c(list(list(model = "Null model", bf10 = 1, bf01 = 1, error = NULL)), rows),
            columns = list(stats_col("model", "Model", "text"), stats_col("bf10", "BF₁₀"), stats_col("bf01", "BF₀₁"), stats_col("error", "error %")),
            notes = list(sprintf("Bayes factors against the null model (intercept only), from BayesFactor::anovaBF with rscaleFixed = %s; n = %d complete cases. The error %% is the proportional error of the numerical approximation.", o$rscale %||% "medium", nrow(dd)))))
}

stats_bayes_code <- function(s, prep, missing_code) {
  q <- function(x) paste0("\"", gsub("\"", "\\\\\"", x), "\"")
  lines <- c(sprintf("# %s generated by PathLab SEM (BayesFactor, as in JASP)", STATS_PROCS[[s$proc]]$label), "library(BayesFactor)",
             "# data <- read.csv(\"your_data.csv\")   # the same dataset as in the app")
  if (!is.null(missing_code)) lines <- c(lines, paste0("data[data == ", missing_code, "] <- NA   # project missing-value code"))
  o <- s$options
  if (s$proc == "bcorr") {
    lines <- c(lines, sprintf("vars <- c(%s)", paste(vapply(o$vars, q, ""), collapse = ", ")),
               "for (i in 1:(length(vars) - 1)) for (j in (i + 1):length(vars)) {",
               "  ok <- complete.cases(data[[vars[i]]], data[[vars[j]]])",
               sprintf("  print(correlationBF(data[[vars[i]]][ok], data[[vars[j]]][ok], rscale = %s))", q(o$rscale %||% "medium")), "}")
  } else {
    for (v in o$factors) lines <- c(lines, sprintf("data[[%s]] <- factor(data[[%s]])", q(v), q(v)))
    lines <- c(lines, sprintf("d <- data[complete.cases(data[, c(%s)]), ]", paste(vapply(c(o$dep, o$factors), q, ""), collapse = ", ")),
               sprintf("set.seed(%d)", as.integer(o$seed %||% 2026)),
               sprintf("anovaBF(`%s` ~ %s, data = d, rscaleFixed = %s)", o$dep, paste(sprintf("`%s`", o$factors), collapse = " * "), q(o$rscale %||% "medium")))
  }
  paste(lines, collapse = "\n")
}

# ---- Data export for jamovi (.omv), JASP / numiqo / SPSS (.sav) and CSV -----------------------------------

#' Write the dataset (optionally with derived variables) with the measurement levels set in the app: nominal -> factor,
#' ordinal -> ordered factor (jamovi stores integers as "Ordinal Integer"), continuous -> numeric. Returns the file path.
export_dataset <- function(ds_full, ds_raw, format, measures, include_derived, out_dir) {
  d <- if (isTRUE(include_derived)) ds_full$data else ds_raw$data
  for (v in names(d)) {
    m <- measures[[v]]
    if (identical(m, "nominal") && !is.factor(d[[v]])) d[[v]] <- factor(d[[v]])
    if (identical(m, "ordinal") && !is.ordered(d[[v]])) d[[v]] <- factor(d[[v]], ordered = TRUE)
  }
  base <- tools::file_path_sans_ext(ds_raw$meta$filename %||% "data")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(out_dir, paste0(gsub("[^A-Za-z0-9._-]", "_", base), ".", format))
  switch(format,
    omv = { if (!requireNamespace("jmvReadWrite", quietly = TRUE)) api_error(501, "export_unavailable", "The jmvReadWrite package is not installed."); jmvReadWrite::write_omv(d, path) },
    sav = { names(d) <- make.names(names(d), unique = TRUE); haven::write_sav(as.data.frame(lapply(d, function(x) if (is.factor(x)) haven::as_factor(x) else x), check.names = FALSE), path) },
    csv = utils::write.csv(d, path, row.names = FALSE, na = ""),
    api_error(400, "invalid_format", "Choose omv, sav or csv."))
  path
}

# ---- Survival analysis (survival) and quality control (qcc) -----------------------------------------------

#' Event indicator: 1 = event, 0 = censored. Numeric 0/1 (or 1/2, survival's convention) is used as is; otherwise the
#' chosen event value marks events.
surv_status <- function(x, level) {
  if (!is.null(level) && nzchar(level)) return(as.integer(as.character(x) == level))
  if (is.numeric(x) && all(stats::na.omit(x) %in% c(0, 1))) return(as.integer(x))
  if (is.numeric(x) && all(stats::na.omit(x) %in% c(1, 2))) return(as.integer(x == 2))
  if (is.logical(x)) return(as.integer(x))
  # The same codes when the event variable is nominal (categories "0"/"1", "1"/"2" or true/false), as in jamovi.
  v <- toupper(trimws(as.character(x))); u <- unique(stats::na.omit(v))
  if (length(u) && all(u %in% c("0", "1"))) return(ifelse(is.na(v), NA_integer_, as.integer(v == "1")))
  if (length(u) && all(u %in% c("1", "2"))) return(ifelse(is.na(v), NA_integer_, as.integer(v == "2")))
  if (length(u) && all(u %in% c("FALSE", "TRUE"))) return(ifelse(is.na(v), NA_integer_, as.integer(v == "TRUE")))
  api_error(422, "surv_event", "Choose the value of the event variable that marks an event (the other values are censored).")
}

run_km <- function(d, o) {
  tt <- d[[o$time]]
  st <- surv_status(d[[o$event]], o$eventLevel)
  g <- if (length(o$group)) factor(d[[o$group[1]]]) else NULL
  keep <- !is.na(tt) & !is.na(st) & (if (is.null(g)) TRUE else !is.na(g))
  dd <- data.frame(time = tt[keep], status = st[keep])
  if (!is.null(g)) dd$group <- droplevels(g[keep])
  conf <- (o$ciWidth %||% 95) / 100
  fit <- if (is.null(g)) survival::survfit(survival::Surv(time, status) ~ 1, data = dd, conf.int = conf) else survival::survfit(survival::Surv(time, status) ~ group, data = dd, conf.int = conf)
  tab <- summary(fit)$table
  tab <- if (is.null(dim(tab))) t(as.matrix(tab)) else tab
  labs <- if (is.null(g)) "All" else sub("^group=", "", rownames(tab))
  lc <- grep("LCL$", colnames(tab), value = TRUE)[1]; uc <- grep("UCL$", colnames(tab), value = TRUE)[1]
  rows <- lapply(seq_len(nrow(tab)), function(i) list(group = labs[i], n = unname(tab[i, "n.start"] %||% tab[i, "records"]), events = unname(tab[i, "events"]),
                                                     censored = unname(tab[i, "n.start"] %||% tab[i, "records"]) - unname(tab[i, "events"]),
                                                     median = stats_cell(unname(tab[i, "median"])), lower = stats_cell(unname(tab[i, lc])), upper = stats_cell(unname(tab[i, uc]))))
  ci_t <- sprintf("%g%% CI for the median", 100 * conf)
  tables <- list(list(id = "km_summary", title = "Survival Summary (Kaplan-Meier)", rows = rows, notes = list("Median survival time: the time at which the estimated survival probability falls to 0.50 (missing when it never does)."),
                      columns = list(stats_col("group", if (is.null(g)) "" else o$group[1], "text"), stats_col("n", "N", "integer"), stats_col("events", "Events", "integer"), stats_col("censored", "Censored", "integer"),
                                     stats_col("median", "Median"), stats_col("lower", "Lower", superTitle = ci_t), stats_col("upper", "Upper", superTitle = ci_t))))
  if (!is.null(g) && isTRUE(o$logrank %||% TRUE) && nlevels(dd$group) > 1) {
    sd <- survival::survdiff(survival::Surv(time, status) ~ group, data = dd)
    df <- length(sd$n) - 1
    tables[[length(tables) + 1]] <- list(id = "logrank", title = "Log-Rank Test", rows = list(list(test = "Log-rank (Mantel-Cox)", chisq = unname(sd$chisq), df = df, p = stats::pchisq(sd$chisq, df, lower.tail = FALSE))),
                                         columns = list(stats_col("test", "", "text"), stats_col("chisq", "χ²"), stats_col("df", "df", "integer"), stats_col("p", "p", pvalue = TRUE)), notes = list("Tests whether the survival curves of the groups differ (survival::survdiff)."))
  }
  times <- suppressWarnings(as.numeric(strsplit(o$times %||% "", "[,; ]+")[[1]]))
  times <- sort(unique(times[!is.na(times) & times >= 0]))
  if (length(times)) {
    sm <- summary(fit, times = times, extend = TRUE)
    grp <- if (is.null(sm$strata)) rep("All", length(sm$time)) else sub("^group=", "", as.character(sm$strata))
    tables[[length(tables) + 1]] <- list(id = "lifetable", title = "Survival at Selected Times", columns = list(stats_col("group", if (is.null(g)) "" else o$group[1], "text", combineBelow = TRUE),
      stats_col("time", "Time"), stats_col("risk", "At risk", "integer"), stats_col("events", "Events", "integer"), stats_col("surv", "Survival"), stats_col("se", "SE"),
      stats_col("lower", "Lower", superTitle = sprintf("%g%% CI", 100 * conf)), stats_col("upper", "Upper", superTitle = sprintf("%g%% CI", 100 * conf))),
      rows = lapply(seq_along(sm$time), function(i) list(group = grp[i], time = sm$time[i], risk = sm$n.risk[i], events = sm$n.event[i], surv = sm$surv[i], se = sm$std.err[i], lower = sm$lower[i], upper = sm$upper[i])), notes = list())
  }
  strata <- if (is.null(fit$strata)) rep("All", length(fit$time)) else rep(sub("^group=", "", names(fit$strata)), fit$strata)
  plots <- list(list(type = "km", var = o$time, groups = lapply(unique(strata), function(s) {
    i <- which(strata == s)
    list(group = s, time = as.list(c(0, fit$time[i])), surv = as.list(c(1, fit$surv[i])), lower = as.list(c(1, fit$lower[i])), upper = as.list(c(1, fit$upper[i])),
         censored = as.list(fit$time[i][fit$n.censor[i] > 0]))
  })))
  list(tables = tables, plots = plots, n = nrow(dd))
}

run_cox <- function(d, o) {
  tt <- d[[o$time]]; st <- surv_status(d[[o$event]], o$eventLevel)
  preds <- c(o$covs %||% character(0), o$factors %||% character(0))
  dd <- data.frame(time = tt, status = st)
  for (v in preds) dd[[v]] <- if (v %in% (o$factors %||% character(0))) factor(d[[v]]) else d[[v]]
  dd <- dd[stats::complete.cases(dd), , drop = FALSE]
  form <- stats::as.formula(paste("survival::Surv(time, status) ~", paste(sprintf("`%s`", preds), collapse = " + ")))
  fit <- survival::coxph(form, data = dd)
  s <- summary(fit, conf.int = (o$ciWidth %||% 95) / 100)
  co <- s$coefficients; ci <- s$conf.int
  ci_t <- sprintf("%g%% CI for HR", o$ciWidth %||% 95)
  tables <- list(list(id = "cox_coef", title = "Cox Regression: Coefficients", notes = list("HR = hazard ratio, exp(B): above 1, the hazard of the event increases with the predictor."),
    columns = list(stats_col("term", "Predictor", "text"), stats_col("b", "B"), stats_col("se", "SE"), stats_col("hr", "HR"), stats_col("lower", "Lower", superTitle = ci_t),
                   stats_col("upper", "Upper", superTitle = ci_t), stats_col("z", "z"), stats_col("p", "p", pvalue = TRUE)),
    rows = lapply(seq_len(nrow(co)), function(i) list(term = gsub("`", "", rownames(co)[i]), b = co[i, "coef"], se = co[i, "se(coef)"], hr = co[i, "exp(coef)"],
                                                      lower = ci[i, 3], upper = ci[i, 4], z = co[i, "z"], p = co[i, "Pr(>|z|)"]))))
  tests <- list(list(test = "Likelihood ratio", stat = unname(s$logtest["test"]), df = unname(s$logtest["df"]), p = unname(s$logtest["pvalue"])),
                list(test = "Wald", stat = unname(s$waldtest["test"]), df = unname(s$waldtest["df"]), p = unname(s$waldtest["pvalue"])),
                list(test = "Score (log-rank)", stat = unname(s$sctest["test"]), df = unname(s$sctest["df"]), p = unname(s$sctest["pvalue"])))
  tables[[2]] <- list(id = "cox_model", title = "Cox Regression: Model Tests", columns = list(stats_col("test", "Test", "text"), stats_col("stat", "χ²"), stats_col("df", "df", "integer"), stats_col("p", "p", pvalue = TRUE)),
                      rows = tests, notes = list(sprintf("N = %d, events = %d. Concordance (C) = %.3f (SE %.3f).", s$n, s$nevent, s$concordance[1], s$concordance[2])))
  z <- tryCatch(survival::cox.zph(fit)$table, error = function(e) NULL)
  if (!is.null(z)) tables[[3]] <- list(id = "cox_ph", title = "Proportional Hazards Assumption (Schoenfeld Residuals)",
    columns = list(stats_col("term", "", "text"), stats_col("chisq", "χ²"), stats_col("df", "df", "integer"), stats_col("p", "p", pvalue = TRUE)),
    rows = lapply(seq_len(nrow(z)), function(i) list(term = gsub("`", "", rownames(z)[i]), chisq = z[i, "chisq"], df = z[i, "df"], p = z[i, "p"])),
    notes = list("survival::cox.zph: a small p-value suggests that the hazard ratio of that term changes over time."))
  list(tables = tables, plots = list(), n = nrow(dd))
}

QCC_TYPES <- c(xbar = "X-bar chart", R = "R chart", S = "S chart", xbar.one = "Individuals (I) chart", p = "p chart", np = "np chart", c = "c chart", u = "u chart")

run_qc <- function(d, o) {
  type <- o$chart %||% "xbar"
  x <- d[[o$measure]]
  sub <- if (length(o$subgroup)) d[[o$subgroup[1]]] else NULL
  if (type %in% c("xbar", "R", "S")) {
    if (is.null(sub)) api_error(422, "qc_subgroup", "X-bar, R and S charts need a subgroup variable.", "Use the individuals (I) chart for one value per subgroup.")
    ok <- !is.na(x) & !is.na(sub)
    data <- qcc::qcc.groups(x[ok], sub[ok])
  } else if (type %in% c("p", "np", "u")) {
    sz <- d[[o$sizes %||% ""]]
    if (is.null(sz)) api_error(422, "qc_sizes", "p, np and u charts need a variable with the sample sizes.")
    ok <- !is.na(x) & !is.na(sz); data <- x[ok]
  } else { ok <- !is.na(x); data <- x[ok] }
  args <- list(data = data, type = type, plot = FALSE, nsigmas = o$nsigmas %||% 3)
  if (type %in% c("p", "np", "u")) args$sizes <- d[[o$sizes]][ok]
  q <- do.call(qcc::qcc, args)
  lim <- q$limits
  beyond <- q$violations$beyond.limits; runs <- q$violations$violating.runs
  tables <- list(list(id = "qc_limits", title = sprintf("%s: Control Limits", QCC_TYPES[[type]]),
    columns = list(stats_col("chart", "", "text"), stats_col("points", "Points", "integer"), stats_col("center", "Centre"), stats_col("lcl", "LCL"), stats_col("ucl", "UCL"),
                   stats_col("sd", "Std. dev."), stats_col("beyond", "Beyond limits", "integer"), stats_col("runs", "Violating runs", "integer")),
    rows = list(list(chart = QCC_TYPES[[type]], points = length(q$statistics), center = unname(q$center), lcl = unname(min(lim[, 1])), ucl = unname(max(lim[, 2])),
                     sd = unname(q$std.dev), beyond = length(beyond), runs = length(runs))),
    notes = list(sprintf("Limits at %g sigma (qcc). %s", q$nsigmas, if (nrow(lim) > 1) "Limits vary with the sample size; the smallest LCL and largest UCL are shown." else ""))))
  if (length(beyond) || length(runs)) tables[[2]] <- list(id = "qc_flags", title = "Out-of-Control Signals",
    columns = list(stats_col("point", "Point", "text"), stats_col("value", "Statistic"), stats_col("signal", "Signal", "text")),
    rows = c(lapply(beyond, function(i) list(point = names(q$statistics)[i] %||% as.character(i), value = unname(q$statistics[i]), signal = "Beyond the control limits")),
             lapply(runs, function(i) list(point = names(q$statistics)[i] %||% as.character(i), value = unname(q$statistics[i]), signal = "Violating run (7 or more on one side)"))), notes = list())
  plots <- list(list(type = "control", var = o$measure, chart = QCC_TYPES[[type]], labels = as.list(names(q$statistics) %||% as.character(seq_along(q$statistics))),
                     values = as.list(unname(q$statistics)), center = unname(q$center), lcl = as.list(rep_len(lim[, 1], length(q$statistics))), ucl = as.list(rep_len(lim[, 2], length(q$statistics))),
                     beyond = as.list(beyond), runs = as.list(runs)))
  list(tables = tables, plots = plots, n = sum(ok), q = q)
}

run_capability <- function(d, o) {
  x <- d[[o$measure]]
  sub <- if (length(o$subgroup)) d[[o$subgroup[1]]] else NULL
  ok <- !is.na(x) & (if (is.null(sub)) TRUE else !is.na(sub))
  q <- if (is.null(sub)) qcc::qcc(x[ok], type = "xbar.one", plot = FALSE) else qcc::qcc(qcc::qcc.groups(x[ok], sub[ok]), type = "xbar", plot = FALSE)
  if (is.null(o$lsl) && is.null(o$usl)) api_error(422, "qc_spec", "Give at least one specification limit (LSL or USL).")
  spec <- c(o$lsl %||% NA, o$usl %||% NA)
  pc <- qcc::process.capability(q, spec.limits = spec, target = o$target %||% NULL, print = FALSE)
  ind <- pc$indices
  rows <- lapply(seq_len(nrow(ind)), function(i) list(index = rownames(ind)[i], value = stats_cell(ind[i, 1]), lower = stats_cell(ind[i, 2]), upper = stats_cell(ind[i, 3])))
  tables <- list(
    list(id = "cap_indices", title = "Process Capability Indices", columns = list(stats_col("index", "Index", "text"), stats_col("value", "Value"), stats_col("lower", "2.5%", superTitle = "95% CI"), stats_col("upper", "97.5%", superTitle = "95% CI")),
         rows = rows, notes = list(sprintf("n = %d; centre = %.4f; within-subgroup SD = %.4f; LSL = %s, USL = %s, target = %s. Values of 1.33 or more are commonly required (qcc::process.capability).",
                                          pc$nobs, pc$center, pc$std.dev, format(spec[1]), format(spec[2]), format(pc$target)))),
    list(id = "cap_out", title = "Out of Specification", columns = list(stats_col("side", "", "text"), stats_col("exp", "Expected (normal)"), stats_col("obs", "Observed")),
         rows = list(list(side = "Below LSL", exp = stats_cell(pc$exp[1]), obs = stats_cell(pc$obs[1])), list(side = "Above USL", exp = stats_cell(pc$exp[2]), obs = stats_cell(pc$obs[2]))),
         notes = list("Proportions of the output outside the specification limits.")))
  h <- graphics::hist(x[ok], plot = FALSE); de <- stats::density(x[ok], n = 64)
  plots <- list(list(type = "capability", var = o$measure, hist = list(breaks = as.list(h$breaks), counts = as.list(h$counts)), density = list(x = as.list(de$x), y = as.list(de$y)),
                     lsl = o$lsl %||% NULL, usl = o$usl %||% NULL, target = o$target %||% NULL))
  list(tables = tables, plots = plots, n = sum(ok))
}

stats_e_code <- function(s, missing_code) {
  q <- function(x) paste0("\"", gsub("\"", "\\\\\"", x), "\"")
  o <- s$options
  lines <- c(sprintf("# %s generated by PathLab SEM", STATS_PROCS[[s$proc]]$label), "# data <- read.csv(\"your_data.csv\")   # the same dataset as in the app")
  if (!is.null(missing_code)) lines <- c(lines, paste0("data[data == ", missing_code, "] <- NA   # project missing-value code"))
  ev <- function() if (nzchar(o$eventLevel %||% "")) sprintf("as.integer(data[[%s]] == %s)", q(o$event), q(o$eventLevel)) else sprintf("data[[%s]]", q(o$event))
  switch(s$proc,
    km = c(lines, "library(survival)", sprintf("d <- data.frame(time = data[[%s]], status = %s%s)", q(o$time), ev(), if (length(o$group)) sprintf(", group = factor(data[[%s]])", q(o$group[1])) else ""),
           sprintf("fit <- survfit(Surv(time, status) ~ %s, data = d, conf.int = %s)", if (length(o$group)) "group" else "1", (o$ciWidth %||% 95) / 100), "summary(fit)$table",
           if (length(o$group)) "survdiff(Surv(time, status) ~ group, data = d)   # log-rank test", "plot(fit)") |> paste(collapse = "\n"),
    cox = c(lines, "library(survival)", sprintf("d <- data.frame(time = data[[%s]], status = %s)", q(o$time), ev()),
            vapply(o$covs %||% character(0), function(v) sprintf("d[[%s]] <- data[[%s]]", q(v), q(v)), ""), vapply(o$factors %||% character(0), function(v) sprintf("d[[%s]] <- factor(data[[%s]])", q(v), q(v)), ""),
            sprintf("fit <- coxph(Surv(time, status) ~ %s, data = d)", paste(sprintf("`%s`", c(o$covs, o$factors)), collapse = " + ")), "summary(fit)", "cox.zph(fit)   # proportional hazards") |> paste(collapse = "\n"),
    panel = panel_code(o, lines, q),
    pgmm = pgmm_code(o, lines, q),
    purtest = purtest_code(o, lines, q),
    if (s$proc %in% names(STATS_MORE_RUN)) stats_more_code(s, lines, q) else
    qc = c(lines, "library(qcc)", if ((o$chart %||% "xbar") %in% c("xbar", "R", "S")) sprintf("x <- qcc.groups(data[[%s]], data[[%s]])", q(o$measure), q(o$subgroup[1])) else sprintf("x <- data[[%s]]", q(o$measure)),
           sprintf("q <- qcc(x, type = %s%s, nsigmas = %s)", q(o$chart %||% "xbar"), if ((o$chart %||% "") %in% c("p", "np", "u")) sprintf(", sizes = data[[%s]]", q(o$sizes)) else "", o$nsigmas %||% 3),
           "summary(q)") |> paste(collapse = "\n"),
    capability = c(lines, "library(qcc)", if (length(o$subgroup)) sprintf("q <- qcc(qcc.groups(data[[%s]], data[[%s]]), type = \"xbar\", plot = FALSE)", q(o$measure), q(o$subgroup[1])) else sprintf("q <- qcc(na.omit(data[[%s]]), type = \"xbar.one\", plot = FALSE)", q(o$measure)),
                   sprintf("process.capability(q, spec.limits = c(%s, %s)%s)", format(o$lsl %||% NA), format(o$usl %||% NA), if (!is.null(o$target)) sprintf(", target = %s", o$target) else "")) |> paste(collapse = "\n"))
}

# ---- Plots (jamovi's Plots menu: bar, box, histogram, scatter, line) -------------------------------------------
# Every number comes from base R (table, mean / sd / qt, grDevices::boxplot.stats, graphics::hist, stats::density,
# stats::lm / stats::loess with predict); the app only draws them.

plot_split <- function(d, g) if (length(g)) split(seq_len(nrow(d)), droplevels(factor(d[[g[1]]]))) else list(All = seq_len(nrow(d)))
plot_titles <- function(o, x, y) list(title = o$title %||% "", xTitle = if (nzchar(o$xTitle %||% "")) o$xTitle else x, yTitle = if (nzchar(o$yTitle %||% "")) o$yTitle else y)
plot_sample <- function(idx, n = 2000) if (length(idx) > n) idx[round(seq(1, length(idx), length.out = n))] else idx

#' Means with SE or CI error bars of `dep` across the categories of `xvar` (optionally one series per `legend` level).
plot_means <- function(d, dep, xvar, legend, bars, ci_width) {
  x <- if (is.null(xvar)) factor(rep(dep, nrow(d))) else if (is.factor(d[[xvar]])) d[[xvar]] else factor(d[[xvar]])
  lg <- if (length(legend)) factor(d[[legend[1]]]) else NULL
  keep <- !is.na(d[[dep]]) & !is.na(x) & (if (is.null(lg)) TRUE else !is.na(lg))
  x <- droplevels(x[keep]); y <- as.numeric(d[[dep]][keep]); if (!is.null(lg)) lg <- droplevels(lg[keep])
  cats <- levels(x)
  one <- function(sel) lapply(cats, function(cc) {
    v <- y[sel & x == cc]; n <- length(v); m <- if (n) mean(v) else NA_real_
    se <- if (n > 1) stats::sd(v) / sqrt(n) else NA_real_
    h <- switch(bars %||% "se", se = se, ci = if (n > 1) stats::qt(1 - (1 - ci_width / 100) / 2, n - 1) * se else NA_real_, none = NA_real_)
    list(n = n, mean = m, lower = m - h, upper = m + h)
  })
  series <- if (is.null(lg)) list(list(name = dep, cells = one(rep(TRUE, length(y))))) else lapply(levels(lg), function(l) list(name = l, cells = one(lg == l)))
  series <- lapply(series, function(s) list(name = s$name, n = lapply(s$cells, `[[`, "n"), values = lapply(s$cells, `[[`, "mean"),
                                            lower = lapply(s$cells, `[[`, "lower"), upper = lapply(s$cells, `[[`, "upper")))
  rows <- unlist(lapply(series, function(s) lapply(seq_along(cats), function(i) list(series = s$name, cat = cats[i], n = s$n[[i]], mean = s$values[[i]],
                                                                                    lower = if ((bars %||% "se") == "none") NULL else s$lower[[i]], upper = if ((bars %||% "se") == "none") NULL else s$upper[[i]]))), recursive = FALSE)
  list(categories = as.list(cats), series = series, rows = rows, n = sum(keep))
}

means_table <- function(pm, dep, xvar, legend, bars, ci_width) {
  bt <- switch(bars %||% "se", se = "Mean ± 1 SE", ci = sprintf("%g%% CI", ci_width), none = "")
  cols <- c(if (length(legend)) list(stats_col("series", legend[1], "text", combineBelow = TRUE)),
            list(stats_col("cat", xvar %||% "", "text"), stats_col("n", "N", "integer"), stats_col("mean", "Mean")),
            if ((bars %||% "se") != "none") list(stats_col("lower", "Lower", superTitle = bt), stats_col("upper", "Upper", superTitle = bt)))
  list(id = "plot_means", title = sprintf("Plotted values: means of %s", dep), columns = cols, rows = pm$rows,
       notes = list(if ((bars %||% "se") == "ci") "Confidence intervals use the t distribution with n − 1 degrees of freedom." else if ((bars %||% "se") == "se") "Error bars: mean ± one standard error (SD / √n)."))
}

run_plot_bar <- function(d, o) {
  ci_width <- o$ciWidth %||% 95
  if ((o$type %||% "counts") == "means") {
    if (!is.numeric(d[[o$var]])) api_error(422, "stats_not_numeric", sprintf("Means need a numeric variable; '%s' is not numeric. Choose Counts or another variable.", o$var))
    xv <- if (length(o$group)) o$group[1] else NULL
    pm <- plot_means(d, o$var, xv, o$legend, o$errorBars, ci_width)
    return(list(n = pm$n, tables = list(means_table(pm, o$var, xv, o$legend, o$errorBars, ci_width)),
                plots = list(c(list(type = "means", style = "bar", var = o$var, xVar = xv %||% "", legend = o$legend[1] %||% "", categories = pm$categories, series = pm$series,
                                    errorBars = o$errorBars %||% "se"), plot_titles(o, xv %||% "", paste("Mean of", o$var))))))
  }
  x <- if (is.factor(d[[o$var]])) d[[o$var]] else factor(d[[o$var]])
  lg <- if (length(o$group)) factor(d[[o$group[1]]]) else NULL
  keep <- !is.na(x) & (if (is.null(lg)) TRUE else !is.na(lg))
  x <- droplevels(x[keep]); if (!is.null(lg)) lg <- droplevels(lg[keep])
  pct <- identical(o$yAxis, "percent")
  tb <- if (is.null(lg)) table(x) else table(x, lg)
  conv <- function(v, tot) unname(if (pct) 100 * v / tot else as.integer(v))
  series <- if (is.null(lg)) list(list(name = if (pct) "Percent" else "Count", values = as.list(conv(as.vector(tb), sum(tb)))))
            else lapply(colnames(tb), function(l) list(name = l, values = as.list(conv(tb[, l], sum(tb[, l])))))
  rows <- if (is.null(lg)) lapply(seq_along(levels(x)), function(i) list(cat = levels(x)[i], count = as.integer(tb[i]), pct = 100 * as.integer(tb[i]) / sum(tb)))
          else unlist(lapply(colnames(tb), function(l) lapply(rownames(tb), function(r) list(series = l, cat = r, count = as.integer(tb[r, l]), pct = 100 * tb[r, l] / sum(tb[, l])))), recursive = FALSE)
  cols <- c(if (!is.null(lg)) list(stats_col("series", o$group[1], "text", combineBelow = TRUE)), list(stats_col("cat", o$var, "text"), stats_col("count", "Count", "integer"), stats_col("pct", if (is.null(lg)) "% of total" else sprintf("%% within %s", o$group[1]))))
  list(n = sum(keep), tables = list(list(id = "plot_counts", title = sprintf("Plotted values: counts of %s", o$var), columns = cols, rows = rows, notes = list())),
       plots = list(c(list(type = "means", style = "bar", var = o$var, xVar = o$var, legend = o$group[1] %||% "", categories = as.list(levels(x)), series = series, errorBars = "none"),
                      plot_titles(o, o$var, if (pct) "Percent" else "Count"))))
}

run_plot_line <- function(d, o) {
  if (!is.numeric(d[[o$dep]])) api_error(422, "stats_not_numeric", sprintf("'%s' is not numeric.", o$dep))
  ci_width <- o$ciWidth %||% 95
  pm <- plot_means(d, o$dep, o$x, o$legend, o$errorBars, ci_width)
  list(n = pm$n, tables = list(means_table(pm, o$dep, o$x, o$legend, o$errorBars, ci_width)),
       plots = list(c(list(type = "means", style = "line", var = o$dep, xVar = o$x, legend = o$legend[1] %||% "", categories = pm$categories, series = pm$series, errorBars = o$errorBars %||% "se"),
                      plot_titles(o, o$x, paste("Mean of", o$dep)))))
}

#' Box (and violin, jittered points) summaries of one continuous variable per group; row numbers of outliers.
box_groups <- function(d, v, g, o) {
  idx <- plot_split(d, g)
  lapply(names(idx), function(gn) {
    rows <- idx[[gn]][!is.na(d[[v]][idx[[gn]]])]
    x <- as.numeric(d[[v]][rows])
    if (length(x) < 2) return(list(group = gn, n = length(x)))
    b <- grDevices::boxplot.stats(x)
    out_i <- which(x < b$stats[1] | x > b$stats[5])
    res <- list(group = gn, n = length(x), box = list(stats = as.list(b$stats), outliers = as.list(utils::head(x[out_i], 200)),
                                                     outlierRows = as.list(utils::head(rows[out_i], 200)), mean = mean(x)))
    if (isTRUE(o$violin)) { de <- stats::density(x, n = 64, from = min(x), to = max(x)); res$violin <- list(x = as.list(de$x), y = as.list(de$y)) }
    if (isTRUE(o$dot)) res$points <- as.list(x[plot_sample(seq_along(x), 1000)])
    res
  })
}

run_plot_box <- function(d, o) {
  plots <- lapply(o$vars, function(v) {
    if (!is.numeric(d[[v]])) api_error(422, "stats_not_numeric", sprintf("'%s' is not numeric.", v))
    c(list(type = "box", var = v, group = o$group[1] %||% "", groups = box_groups(d, v, o$group, o),
           showMean = isTRUE(o$boxMean %||% TRUE), labelOutliers = isTRUE(o$labelOutliers), violin = isTRUE(o$violin), dot = isTRUE(o$dot), box = isTRUE(o[["box"]] %||% TRUE)),
      plot_titles(o, o$group[1] %||% "", v))
  })
  list(n = sum(stats::complete.cases(d)), tables = list(), plots = plots)
}

run_plot_hist <- function(d, o) {
  v <- o$var
  if (!is.numeric(d[[v]])) api_error(422, "stats_not_numeric", sprintf("'%s' is not numeric.", v))
  idx <- plot_split(d, o$group)
  all <- stats::na.omit(as.numeric(d[[v]]))
  bins <- o$bins
  brk <- if (!is.null(bins) && bins >= 2 && length(all)) seq(min(all), max(all), length.out = as.integer(bins) + 1) else graphics::hist(all, plot = FALSE)$breaks
  per <- lapply(names(idx), function(gn) {
    x <- stats::na.omit(as.numeric(d[[v]][idx[[gn]]]))
    if (length(x) < 2) return(list(group = gn, n = length(x)))
    h <- graphics::hist(x, breaks = brk, plot = FALSE)
    out <- list(group = gn, n = length(x), hist = list(breaks = as.list(h$breaks), counts = as.list(h$counts)))
    if (isTRUE(o$density)) { de <- stats::density(x, n = 64); out$density <- list(x = as.list(de$x), y = as.list(de$y)) }
    if (isTRUE(o$normal)) { xs <- seq(min(brk), max(brk), length.out = 64); out$normal <- list(x = as.list(xs), y = as.list(stats::dnorm(xs, mean(x), stats::sd(x)))) }
    out
  })
  list(n = length(all), tables = list(), plots = list(c(list(type = "continuous", var = v, groups = per), plot_titles(o, v, "Count"))))
}

run_plot_scatter <- function(d, o) {
  for (v in c(o$x, o$y)) if (!is.numeric(d[[v]])) api_error(422, "stats_not_numeric", sprintf("'%s' is not numeric.", v))
  idx <- plot_split(d, o$group)
  line <- o$line %||% "none"
  lvl <- (o$ciWidth %||% 95) / 100
  rows <- list()
  groups <- lapply(names(idx), function(gn) {
    r <- idx[[gn]]
    r <- r[!is.na(d[[o$x]][r]) & !is.na(d[[o$y]][r])]
    x <- as.numeric(d[[o$x]][r]); y <- as.numeric(d[[o$y]][r])
    s <- plot_sample(seq_along(x))
    out <- list(group = gn, n = length(x), x = as.list(unname(x[s])), y = as.list(unname(y[s])))
    if (line != "none" && length(x) >= 3 && stats::sd(x) > 0) {
      grid <- seq(min(x), max(x), length.out = 50)
      dd <- data.frame(x = x, y = y)
      if (line == "linear") {
        fit <- stats::lm(y ~ x, data = dd)
        pr <- stats::predict(fit, newdata = data.frame(x = grid), interval = "confidence", level = lvl)
        out$fit <- list(x = as.list(grid), y = as.list(unname(pr[, "fit"])), lower = as.list(unname(pr[, "lwr"])), upper = as.list(unname(pr[, "upr"])))
        cf <- stats::coef(fit)
        rows[[length(rows) + 1]] <<- list(group = gn, n = length(x), intercept = unname(cf[1]), slope = unname(cf[2]), r = stats::cor(x, y), r2 = summary(fit)$r.squared,
                                          p = summary(fit)$coefficients[2, 4])
      } else if (length(unique(x)) >= 5) {
        fit <- stats::loess(y ~ x, data = dd)
        pr <- stats::predict(fit, newdata = data.frame(x = grid), se = TRUE)
        h <- stats::qt(1 - (1 - lvl) / 2, pr$df) * pr$se.fit
        out$fit <- list(x = as.list(grid), y = as.list(unname(pr$fit)), lower = as.list(unname(pr$fit - h)), upper = as.list(unname(pr$fit + h)))
      }
      if (!isTRUE(o$se %||% TRUE) && !is.null(out$fit)) { out$fit$lower <- NULL; out$fit$upper <- NULL }
    }
    out
  })
  tables <- if (length(rows)) list(list(id = "plot_fit", title = sprintf("Linear fit: %s on %s", o$y, o$x), rows = rows,
    columns = list(stats_col("group", o$group[1] %||% "", "text"), stats_col("n", "N", "integer"), stats_col("intercept", "Intercept"), stats_col("slope", "Slope"),
                   stats_col("r", "r"), stats_col("r2", "R²"), stats_col("p", "p", pvalue = TRUE)),
    notes = list("Ordinary least squares (stats::lm); p tests the slope."))) else list()
  list(n = sum(vapply(groups, function(g) g$n, 0)), tables = tables,
       plots = list(c(list(type = "scatter", xVar = o$x, yVar = o$y, group = o$group[1] %||% "", line = line, groups = groups), plot_titles(o, o$x, o$y))))
}

stats_plot_code <- function(s, missing_code) {
  q <- function(x) paste0("\"", gsub("\"", "\\\\\"", x), "\"")
  o <- s$options
  lines <- c(sprintf("# %s generated by PathLab SEM (base R)", STATS_PROCS[[s$proc]]$label), "# data <- read.csv(\"your_data.csv\")   # the same dataset as in the app")
  if (!is.null(missing_code)) lines <- c(lines, paste0("data[data == ", missing_code, "] <- NA   # project missing-value code"))
  g <- function(k) if (length(o[[k]])) o[[k]][1] else NULL
  means <- function(dep, x, lg) c(sprintf("d <- na.omit(data[, c(%s)])", paste(vapply(c(dep, x, lg), q, ""), collapse = ", ")),
    sprintf("m <- aggregate(d[[%s]], by = list(%s), FUN = function(v) c(mean = mean(v), se = sd(v) / sqrt(length(v)), n = length(v)))", q(dep),
            paste(vapply(c(x, lg), function(v) sprintf("%s = d[[%s]]", make.names(v), q(v)), ""), collapse = ", ")), "m")
  body <- switch(s$proc,
    plotBar = if ((o$type %||% "counts") == "means") c(means(o$var, g("group"), g("legend")), sprintf("barplot(tapply(d[[%s]], list(%s), mean), beside = TRUE, legend.text = %s)", q(o$var),
                     paste(vapply(rev(c(g("legend"), g("group"))), function(v) sprintf("d[[%s]]", q(v)), ""), collapse = ", "), if (!is.null(g("legend"))) "TRUE" else "FALSE"))
              else c(if (is.null(g("group"))) sprintf("tb <- table(data[[%s]])", q(o$var)) else sprintf("tb <- table(data[[%s]], data[[%s]])", q(o$var), q(g("group"))),
                     if (identical(o$yAxis, "percent")) "tb <- 100 * prop.table(tb, margin = if (length(dim(tb)) > 1) 2 else NULL)", "tb", sprintf("barplot(tb, beside = TRUE, legend.text = %s)", if (is.null(g("group"))) "FALSE" else "TRUE")),
    plotBox = unlist(lapply(o$vars, function(v) c(if (is.null(g("group"))) sprintf("b <- boxplot(data[[%s]])", q(v)) else sprintf("b <- boxplot(data[[%s]] ~ data[[%s]])", q(v), q(g("group"))), "b$stats   # whiskers, quartiles, median"))),
    plotHist = c(sprintf("x <- na.omit(data[[%s]])", q(o$var)), if (!is.null(o$bins)) sprintf("h <- hist(x, breaks = seq(min(x), max(x), length.out = %d))", as.integer(o$bins) + 1) else "h <- hist(x)",
                 if (isTRUE(o$density)) "lines(density(x)$x, density(x)$y * length(x) * diff(h$breaks)[1])"),
    plotScatter = c(sprintf("plot(data[[%s]], data[[%s]], xlab = %s, ylab = %s)", q(o$x), q(o$y), q(o$x), q(o$y)),
                    if (identical(o$line, "linear")) c(sprintf("fit <- lm(%s ~ %s, data = data)", sprintf("`%s`", o$y), sprintf("`%s`", o$x)), "abline(fit); summary(fit)")
                    else if (identical(o$line, "smooth")) sprintf("fit <- loess(%s ~ %s, data = data)", sprintf("`%s`", o$y), sprintf("`%s`", o$x))),
    plotRadar = c(sprintf("x <- data[, c(%s)]", paste(vapply(unlist(o$vars), q, ""), collapse = ", ")),
                  if (identical(o$scale, "minmax")) "x[] <- lapply(x, function(v) (v - min(v, na.rm = TRUE)) / diff(range(v, na.rm = TRUE)))" else if (identical(o$scale, "z")) "x[] <- lapply(x, function(v) as.numeric(scale(v)))",
                  if (is.null(g("group"))) sprintf("m <- t(sapply(x, %s, na.rm = TRUE))", if (identical(o$stat, "median")) "median" else "mean")
                  else sprintf("m <- aggregate(x, by = list(group = data[[%s]]), FUN = %s, na.rm = TRUE); rownames(m) <- m$group; m$group <- NULL", q(g("group")), if (identical(o$stat, "median")) "median" else "mean"),
                  "m", "stars(m, locations = c(0, 0), radius = FALSE, key.loc = c(0, 0), lty = seq_len(nrow(m)), draw.segments = FALSE, scale = FALSE)   # radar plot"),
    plotLine = c(means(o$dep, o$x, g("legend")), if (!is.null(g("legend"))) sprintf("interaction.plot(d[[%s]], d[[%s]], d[[%s]])", q(o$x), q(g("legend")), q(o$dep))
                 else sprintf("plot(tapply(d[[%s]], d[[%s]], mean), type = \"b\")", q(o$dep), q(o$x))))
  paste(c(lines, unlist(body)), collapse = "\n")
}

# ---- References (jamovi lists them under the results) ------------------------------------------------------------
STATS_REF_CACHE <- new.env()
#' Text references of R and the packages that computed a result, from citation() (so they always match the versions installed).
#' jmv, the engine of the Statistics analyses, is always cited.
stats_references <- function(pkgs) {
  pkgs <- unique(c("R", "jmv", setdiff(pkgs, c("R", "base", "stats", "graphics", "grDevices", "utils"))))
  out <- character(0)
  for (p in pkgs) {
    if (is.null(STATS_REF_CACHE[[p]])) STATS_REF_CACHE[[p]] <- tryCatch({
      cit <- if (p == "R") utils::citation() else utils::citation(p)
      gsub("\\s+", " ", paste(format(cit[[1]], style = "text"), collapse = " "))
    }, error = function(e) "")
    if (nzchar(STATS_REF_CACHE[[p]])) out <- c(out, STATS_REF_CACHE[[p]])
  }
  as.list(unique(c(pathlab_citation("text"), out)))
}

#' jmv's t-test Group Descriptives arrives wide (group[1] num[1] ... group[2] num[2] ...); jamovi shows it long: one row
#' per group, the variable named once above its groups.
group_desc_long <- function(t) {
  if (!identical(t$id, "desc") || !length(t$rows) || is.null(t$rows[[1]][["group[1]"]])) return(t)
  stems <- c(group = "Group", num = "N", mean = "Mean", med = "Median", sd = "SD", se = "SE")
  have <- vapply(t$columns, function(cc) cc$name, "")
  stems <- stems[paste0(names(stems), "[1]") %in% have]
  type_of <- function(s) { cc <- t$columns[[match(paste0(s, "[1]"), have)]]; cc$type }
  rows <- list()
  for (r in t$rows) for (k in 1:2) rows[[length(rows) + 1]] <- c(list(dep = r$dep), setNames(lapply(names(stems), function(s) r[[sprintf("%s[%d]", s, k)]]), names(stems)))
  t$columns <- c(list(stats_col("dep", "", "text", combineBelow = TRUE)), lapply(names(stems), function(s) stats_col(s, stems[[s]], type_of(s))))
  t$rows <- rows
  t
}

#' jmv's "variables across columns" Descriptives arrives as one row of cells named <var>[<stat><group>]; rebuild jamovi's
#' layout: one row per statistic (and group), one column per variable. Rows keep the statistic key (statKey) and flag
#' p-values (.p) and counts (.int) for formatting.
desc_columns_fix <- function(t, vars, split_lab = "") {
  if (!identical(t$id, "descriptives") || length(t$rows) != 1 || !any(grepl("^stat\\[", names(t$rows[[1]])))) return(t)
  r <- t$rows[[1]]
  sfx <- unique(sub("^stat\\[(.*)\\]$", "\\1", grep("^stat\\[", names(r), value = TRUE)))
  rows <- lapply(sfx, function(s) {
    g <- r[[paste0("var1[", s, "]")]]
    key <- if (!is.null(g) && nzchar(g) && endsWith(s, g)) substr(s, 1, nchar(s) - nchar(g)) else s
    out <- list(stat = r[[paste0("stat[", s, "]")]], statKey = s)
    if (!is.null(g)) out$group <- g
    if (key == "sw" || grepl("^(sw|ks|ad)p?$", key) && key != "sww") out$.p <- TRUE
    if (key %in% c("n", "missing")) out$.int <- TRUE
    for (v in vars) out[[v]] <- r[[paste0(v, "[", s, "]")]]
    out
  })
  has_g <- any(vapply(rows, function(x) !is.null(x$group), TRUE))
  t$columns <- c(list(stats_col("stat", "", "text", combineBelow = TRUE)), if (has_g) list(stats_col("group", split_lab, "text")), lapply(vars, function(v) stats_col(v, v)))
  t$rows <- rows
  t
}

# ---- Radar (spider) plot: profile of means or medians over several variables, one polygon per group -------------
run_plot_radar <- function(d, o) {
  vars <- unlist(o$vars)
  if (length(vars) < 3) api_error(422, "stats_incomplete", "A radar plot needs at least three variables.")
  for (v in vars) if (!is.numeric(d[[v]])) api_error(422, "stats_not_numeric", sprintf("'%s' is not numeric.", v))
  sc <- o$scale %||% "none"
  x <- as.data.frame(lapply(d[vars], as.numeric), check.names = FALSE)
  if (sc == "minmax") x[] <- lapply(x, function(v) { r <- range(v, na.rm = TRUE); if (diff(r) == 0) v * 0 else (v - r[1]) / diff(r) })
  if (sc == "z") x[] <- lapply(x, function(v) as.numeric(scale(v)))
  f <- if (identical(o$stat, "median")) function(v) stats::median(v, na.rm = TRUE) else function(v) mean(v, na.rm = TRUE)
  g <- if (length(o$group)) droplevels(factor(d[[o$group[1]]])) else NULL
  groups <- if (is.null(g)) list(All = rep(TRUE, nrow(x))) else lapply(setNames(levels(g), levels(g)), function(l) !is.na(g) & g == l)
  series <- lapply(names(groups), function(gn) list(name = gn, n = sum(groups[[gn]]), values = as.list(unname(vapply(vars, function(v) f(x[[v]][groups[[gn]]]), 0)))))
  vals <- unlist(lapply(series, function(s) unlist(s$values)))
  lo <- if (sc == "minmax") 0 else min(vals, na.rm = TRUE); hi <- if (sc == "minmax") 1 else max(vals, na.rm = TRUE)
  if (sc == "none" && lo > 0 && all(vapply(d[vars], function(v) min(v, na.rm = TRUE) >= 0, TRUE))) lo <- min(0, min(vapply(d[vars], function(v) min(v, na.rm = TRUE), 0)))
  stat_lab <- if (identical(o$stat, "median")) "Median" else "Mean"
  rows <- unlist(lapply(series, function(s) lapply(seq_along(vars), function(i) list(series = s$name, var = vars[i], value = s$values[[i]]))), recursive = FALSE)
  cols <- c(if (!is.null(g)) list(stats_col("series", o$group[1], "text", combineBelow = TRUE)), list(stats_col("var", "Variable", "text"), stats_col("value", stat_lab)))
  note <- switch(sc, minmax = "Each variable was rescaled to 0–1 (minimum to maximum of the sample) before averaging.", z = "Each variable was standardised (z-scores) before averaging.", "Values are on the variables' own scales.")
  list(n = nrow(d), tables = list(list(id = "plot_radar", title = sprintf("Plotted values: %s of %d variables", tolower(stat_lab), length(vars)), columns = cols, rows = rows, notes = list(note))),
       plots = list(c(list(type = "radar", axes = as.list(vars), series = series, min = lo, max = hi, stat = stat_lab, scale = sc, fill = !isFALSE(o$fill), group = o$group[1] %||% ""),
                      plot_titles(o, "", stat_lab))))
}

# ---- CFA path diagram ------------------------------------------------------------------------------------

#' The CFA path diagram as data (drawn in the browser, like jamovi's semPlot diagram): factors with their variances,
#' indicators with their loadings and residual variances, factor covariances and residual covariances. Fixed
#' parameters (no standard error) are flagged so the diagram can dash them.
cfa_path_plot <- function(tables, standardized) {
  tab <- function(id) { t <- Filter(function(x) identical(x$id, id), tables); if (length(t)) t[[1]]$rows else list() }
  num <- function(x) if (is.null(x) || !is.numeric(x)) NULL else as.numeric(x)
  val <- function(r) if (standardized) num(r$stdEst) %||% num(r$est) else num(r$est)
  fixed <- function(r) is.null(r$se) || !is.numeric(r$se)   # jmv leaves the standard error of a fixed parameter blank
  load <- tab("factorLoadings"); fcov <- tab("factorCov"); rcov <- tab("resCov")
  cur <- ""
  inds <- lapply(load, function(r) { if (!is.null(r$factor) && nzchar(r$factor)) cur <<- r$factor; list(name = r$indicator, factor = cur, loading = val(r), fixed = fixed(r)) })
  facs <- unique(vapply(inds, function(i) i$factor, ""))
  var_of <- function(rows, a, b, k1, k2) { for (r in rows) if (identical(r[[k1]], a) && identical(r[[k2]], b)) return(r); NULL }
  factors <- lapply(facs, function(f) { r <- var_of(fcov, f, f, "factor1", "factor2"); list(name = f, variance = if (!is.null(r)) val(r) %||% num(r$est), varianceFixed = !is.null(r) && fixed(r)) })
  inds <- lapply(inds, function(i) { r <- var_of(rcov, i$name, i$name, "var1", "var2"); i$residual <- if (!is.null(r)) val(r); i })
  fc <- Filter(Negate(is.null), lapply(fcov, function(r) if (!identical(r$factor1, r$factor2)) list(from = r$factor1, to = r$factor2, value = val(r), fixed = fixed(r))))
  rc <- Filter(Negate(is.null), lapply(rcov, function(r) if (!identical(r$var1, r$var2)) list(from = r$var1, to = r$var2, value = val(r), fixed = fixed(r))))
  list(type = "cfaPath", title = "Path Diagram", standardized = standardized, factors = factors, indicators = inds, factorCovs = fc, residualCovs = rc)
}

#' Tables and columns requested only for the path diagram are removed again.
cfa_hide_extra <- function(tables, o) {
  drop <- c(if (!isTRUE(o$factCovEst)) "factorCov", if (!isTRUE(o$resCovEst)) "resCov")
  tables <- Filter(function(t) !t$id %in% drop, tables)
  if (!isTRUE(o$stdEst)) tables <- lapply(tables, function(t) {
    keep <- vapply(t$columns, function(c) !identical(c$name, "stdEst"), logical(1))
    if (all(keep)) return(t)
    t$columns <- t$columns[keep]
    t$rows <- lapply(t$rows, function(r) { r$stdEst <- NULL; r })
    t
  })
  tables
}

# ---- Scree plot (EFA / PCA) --------------------------------------------------------------------------------

#' jamovi's scree plot as data: the eigenvalues jmv computed (initial eigenvalues for PCA, those of the factor
#' solution for EFA) and, with parallel analysis, the 95th percentiles of the eigenvalues of simulated random data.
#' The number of factors retained is read from the loadings table.
scree_plot <- function(res, tables, proc, o) {
  st <- tryCatch(res$eigen$screePlot$state, error = function(e) NULL)
  if (is.null(st) || !nrow(st)) return(NULL)
  sim_lab <- unique(st$type)[1]
  dat <- st[st$type != sim_lab, , drop = FALSE]
  if (!nrow(dat)) dat <- st
  sim <- st[st$type == sim_lab & nrow(dat) < nrow(st), , drop = FALSE]
  load <- Filter(function(t) identical(t$id, "loadings"), tables)
  k <- if (length(load)) sum(grepl("^pc[0-9]+$", vapply(load[[1]]$columns, function(c) c$name, ""))) else NA_integer_
  method <- o$nFactorMethod %||% "parallel"
  list(type = "scree", title = "Scree Plot", unit = if (proc == "pca") "Component" else "Factor",
       data = as.list(as.numeric(dat$eigen)),
       simulated = if (method == "parallel" && nrow(sim)) as.list(as.numeric(sim$eigen)) else NULL,
       cutoff = if (method == "eigen") as.numeric(o$minEigen %||% if (proc == "pca") 1 else 0) else NULL,
       retained = if (is.na(k)) NULL else k, method = method)
}
