# ggplot2 builder: a visual editor for the grammar of graphics. The specification (layers of geoms with their
# aesthetic mappings and parameters, facets, labels, theme, coordinates) is checked against a whitelist, turned into
# ggplot2 R code (values inserted with deparse(), variable names in backticks) and that exact code is evaluated, so
# the plot and the code shown to the user are the same. Nothing typed by the user is ever run as code.

# Parameter types: "num:min:max", "int:min:max", "colour", "bool", "enum:a,b,c", "linetype", "shape".
GG_COMMON_COL <- list(colour = "colour", alpha = "num:0:1")
GG_GEOMS <- list(
  point = list(label = "Points", group = "Basic", fn = "geom_point", aes = c("x", "y", "colour", "fill", "shape", "size", "alpha", "group"), req = c("x", "y"),
               params = c(list(size = "num:0.1:20", shape = "shape", fill = "colour", stroke = "num:0:5"), GG_COMMON_COL), positions = c("identity", "jitter")),
  jitter = list(label = "Jittered points", group = "Basic", fn = "geom_jitter", aes = c("x", "y", "colour", "fill", "shape", "size", "alpha", "group"), req = c("x", "y"),
                params = c(list(width = "num:0:10", height = "num:0:10", size = "num:0.1:20", shape = "shape"), GG_COMMON_COL)),
  line = list(label = "Line", group = "Basic", fn = "geom_line", aes = c("x", "y", "colour", "linetype", "alpha", "group"), req = c("x", "y"),
              params = c(list(linewidth = "num:0.1:10", linetype = "linetype"), GG_COMMON_COL)),
  area = list(label = "Area", group = "Basic", fn = "geom_area", aes = c("x", "y", "fill", "colour", "alpha", "group"), req = c("x", "y"),
              params = c(list(fill = "colour", linewidth = "num:0:10"), GG_COMMON_COL), positions = c("stack", "identity", "fill")),
  bar = list(label = "Bar (counts)", group = "Distribution", fn = "geom_bar", aes = c("x", "fill", "colour", "alpha", "group"), req = "x",
             params = c(list(fill = "colour", width = "num:0.05:1"), GG_COMMON_COL), positions = c("stack", "dodge", "fill")),
  col = list(label = "Bar (values)", group = "Basic", fn = "geom_col", aes = c("x", "y", "fill", "colour", "alpha", "group"), req = c("x", "y"),
             params = c(list(fill = "colour", width = "num:0.05:1"), GG_COMMON_COL), positions = c("stack", "dodge", "fill")),
  histogram = list(label = "Histogram", group = "Distribution", fn = "geom_histogram", aes = c("x", "fill", "colour", "alpha", "group"), req = "x",
                   params = c(list(bins = "int:2:200", binwidth = "num:0:1e9", fill = "colour"), GG_COMMON_COL), positions = c("stack", "identity", "dodge")),
  density = list(label = "Density", group = "Distribution", fn = "geom_density", aes = c("x", "fill", "colour", "linetype", "alpha", "group"), req = "x",
                 params = c(list(adjust = "num:0.1:10", fill = "colour", linewidth = "num:0.1:10"), GG_COMMON_COL), positions = c("identity", "stack")),
  density2d = list(label = "2D density (contours)", group = "Distribution", fn = "geom_density_2d", aes = c("x", "y", "colour", "alpha", "group"), req = c("x", "y"),
                   params = c(list(bins = "int:2:50", linewidth = "num:0.1:5", adjust = "num:0.1:10"), GG_COMMON_COL)),
  boxplot = list(label = "Box plot", group = "Distribution", fn = "geom_boxplot", aes = c("x", "y", "fill", "colour", "alpha", "group"), req = "y",
                 params = c(list(fill = "colour", width = "num:0.05:1", notch = "bool", varwidth = "bool", outlier.shape = "shape"), GG_COMMON_COL), positions = c("dodge2")),
  violin = list(label = "Violin", group = "Distribution", fn = "geom_violin", aes = c("x", "y", "fill", "colour", "alpha", "group"), req = "y",
                params = c(list(fill = "colour", trim = "bool", scale = "enum:area,count,width", width = "num:0.05:1.5"), GG_COMMON_COL), positions = c("dodge")),
  summary = list(label = "Mean with error bars", group = "Statistics", fn = "stat_summary", aes = c("x", "y", "colour", "group"), req = c("x", "y"),
                 params = c(list(fun.data = "enum:mean_se,mean_cl_normal,mean_cl_boot,mean_sdl,median_hilow", geom = "enum:pointrange,errorbar,crossbar,linerange,point,line,col,bar", size = "num:0:5", linewidth = "num:0.1:5", width = "num:0:1"), GG_COMMON_COL),
                 positions = c("identity", "dodge")),
  smooth = list(label = "Smoothed line (lm / loess)", group = "Statistics", fn = "geom_smooth", aes = c("x", "y", "colour", "fill", "linetype", "group"), req = c("x", "y"),
                params = c(list(method = "enum:lm,loess,glm,gam", se = "bool", level = "num:0.5:0.999", linewidth = "num:0.1:10", linetype = "linetype", fill = "colour"), GG_COMMON_COL)),
  errorbar = list(label = "Error bars (from variables)", group = "Statistics", fn = "geom_errorbar", aes = c("x", "ymin", "ymax", "colour", "group"), req = c("x", "ymin", "ymax"),
                  params = c(list(width = "num:0:2", linewidth = "num:0.1:5"), GG_COMMON_COL), positions = c("identity", "dodge")),
  tile = list(label = "Tiles (heat map)", group = "Basic", fn = "geom_tile", aes = c("x", "y", "fill", "alpha"), req = c("x", "y"), params = c(list(colour = "colour", linewidth = "num:0:5"), list(alpha = "num:0:1"))),
  text = list(label = "Text labels", group = "Annotation", fn = "geom_text", aes = c("x", "y", "label", "colour", "size", "alpha"), req = c("x", "y", "label"),
              params = c(list(size = "num:1:20", vjust = "num:-2:3", hjust = "num:-2:3", check_overlap = "bool"), GG_COMMON_COL), positions = c("identity", "jitter")),
  rug = list(label = "Rug", group = "Distribution", fn = "geom_rug", aes = c("x", "y", "colour", "alpha"), req = character(0), params = c(list(sides = "enum:bl,b,l,tr,trbl", length = "num:0.005:0.2"), GG_COMMON_COL)),
  qq = list(label = "Normal Q-Q points", group = "Statistics", fn = "geom_qq", aes = c("sample", "colour", "group"), req = "sample", params = c(list(size = "num:0.1:10"), GG_COMMON_COL)),
  qqline = list(label = "Normal Q-Q line", group = "Statistics", fn = "geom_qq_line", aes = c("sample", "colour", "group"), req = "sample", params = c(list(linewidth = "num:0.1:5", linetype = "linetype"), GG_COMMON_COL)),
  hline = list(label = "Horizontal reference line", group = "Annotation", fn = "geom_hline", aes = character(0), req = character(0), params = c(list(yintercept = "num:-1e12:1e12", linetype = "linetype", linewidth = "num:0.1:10"), GG_COMMON_COL)),
  vline = list(label = "Vertical reference line", group = "Annotation", fn = "geom_vline", aes = character(0), req = character(0), params = c(list(xintercept = "num:-1e12:1e12", linetype = "linetype", linewidth = "num:0.1:10"), GG_COMMON_COL)),
  # Builder step 3.
  step = list(label = "Step line", group = "Basic", fn = "geom_step", aes = c("x", "y", "colour", "linetype", "alpha", "group"), req = c("x", "y"),
              params = c(list(linewidth = "num:0.1:10", linetype = "linetype", direction = "enum:hv,vh,mid"), GG_COMMON_COL)),
  path = list(label = "Path (in data order)", group = "Basic", fn = "geom_path", aes = c("x", "y", "colour", "linetype", "alpha", "group"), req = c("x", "y"),
              params = c(list(linewidth = "num:0.1:10", linetype = "linetype"), GG_COMMON_COL)),
  ribbon = list(label = "Ribbon (band)", group = "Distribution", fn = "geom_ribbon", aes = c("x", "ymin", "ymax", "fill", "colour", "alpha", "group"), req = c("x", "ymin", "ymax"),
                params = c(list(fill = "colour", linewidth = "num:0:10"), GG_COMMON_COL)),
  pointrange = list(label = "Point with range", group = "Summary", fn = "geom_pointrange", aes = c("x", "y", "ymin", "ymax", "colour", "shape", "alpha", "group"), req = c("x", "y", "ymin", "ymax"),
                    params = c(list(size = "num:0.1:20", shape = "shape", linewidth = "num:0.1:10"), GG_COMMON_COL), positions = c("identity", "dodge")),
  crossbar = list(label = "Crossbar", group = "Summary", fn = "geom_crossbar", aes = c("x", "y", "ymin", "ymax", "fill", "colour", "alpha", "group"), req = c("x", "y", "ymin", "ymax"),
                  params = c(list(fill = "colour", width = "num:0:10", linewidth = "num:0.1:10"), GG_COMMON_COL), positions = c("identity", "dodge")),
  hex = list(label = "Hexagonal bins", group = "Distribution", fn = "geom_hex", aes = c("x", "y", "fill", "alpha"), req = c("x", "y"),
             params = c(list(bins = "int:2:200"), GG_COMMON_COL)),
  bin2d = list(label = "Rectangular bins", group = "Distribution", fn = "geom_bin2d", aes = c("x", "y", "fill", "alpha"), req = c("x", "y"),
               params = c(list(bins = "int:2:200"), GG_COMMON_COL)),
  label = list(label = "Boxed labels", group = "Annotation", fn = "geom_label", aes = c("x", "y", "label", "colour", "fill", "size", "alpha", "group"), req = c("x", "y", "label"),
               params = c(list(size = "num:1:20", fill = "colour", vjust = "num:-5:5", hjust = "num:-5:5"), GG_COMMON_COL)),
  abline = list(label = "Sloped reference line", group = "Annotation", fn = "geom_abline", aes = character(0), req = character(0),
                params = c(list(slope = "num:-1e6:1e6", intercept = "num:-1e12:1e12", linetype = "linetype", linewidth = "num:0.1:10"), GG_COMMON_COL))
)
GG_AES <- c("x", "y", "colour", "fill", "shape", "size", "alpha", "linetype", "group", "label", "ymin", "ymax", "sample", "weight")
GG_DISCRETE_AES <- c("shape", "linetype", "group")
GG_THEMES <- c(grey = "theme_grey", bw = "theme_bw", minimal = "theme_minimal", classic = "theme_classic", light = "theme_light", dark = "theme_dark", linedraw = "theme_linedraw", void = "theme_void", apa = "theme_classic")
GG_LINETYPES <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
# Colour scales (builder step 2). "default" leaves ggplot2's own hues alone. The viridis options and Okabe-Ito are
# readable in greyscale and to colour-blind readers, which is what most journals now ask for; ColorBrewer's
# qualitative sets are offered for discrete groups. Each entry is the suffix after scale_<aes>_.
GG_PAL_DISCRETE <- c(default = "", okabe = "manual", viridis = "viridis_d", magma = "viridis_d", plasma = "viridis_d",
                     cividis = "viridis_d", set1 = "brewer", set2 = "brewer", dark2 = "brewer", paired = "brewer", grey = "grey")
GG_PAL_CONTINUOUS <- c(default = "", viridis = "viridis_c", magma = "viridis_c", plasma = "viridis_c", cividis = "viridis_c",
                       blues = "distiller", oranges = "distiller", spectral = "distiller", rdbu = "distiller", grey = "grey")
GG_PAL_OPTION <- c(viridis = "viridis", magma = "magma", plasma = "plasma", cividis = "cividis")   # viridis `option=`
GG_PAL_BREWER <- c(set1 = "Set1", set2 = "Set2", dark2 = "Dark2", paired = "Paired", blues = "Blues", oranges = "Oranges",
                   spectral = "Spectral", rdbu = "RdBu")
GG_OKABE <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
GG_TRANS <- c("identity", "log10", "log2", "sqrt", "reverse")
# Journal presets (builder step 3): the settings a style guide asks for, applied together. They set only the look,
# never the data or the layers, and the user can change anything afterwards. Widths are the common single-column
# and double-column figure sizes in inches.
GG_PRESETS <- list(
  apa = list(label = "APA 7", theme = list(base = "apa", size = 11, family = "sans", legend = "bottom", centreTitle = FALSE, noGrid = TRUE),
             palette = list(discrete = "grey", continuous = "grey"), extra = list(axisLine = TRUE, stripPlain = TRUE, grid = "none"), size = list(width = 6.5, height = 4.5)),
  apacolour = list(label = "APA 7 (colour)", theme = list(base = "apa", size = 11, family = "sans", legend = "bottom", centreTitle = FALSE, noGrid = TRUE),
                   palette = list(discrete = "okabe", continuous = "viridis"), extra = list(axisLine = TRUE, stripPlain = TRUE, grid = "none"), size = list(width = 6.5, height = 4.5)),
  elsevier = list(label = "Elsevier (single column)", theme = list(base = "classic", size = 8, family = "sans", legend = "bottom", centreTitle = FALSE, noGrid = TRUE),
                  palette = list(discrete = "okabe", continuous = "viridis"), extra = list(axisLine = TRUE, stripPlain = TRUE, grid = "none"), size = list(width = 3.54, height = 2.8)),
  springer = list(label = "Springer (double column)", theme = list(base = "bw", size = 9, family = "serif", legend = "right", centreTitle = FALSE, noGrid = FALSE),
                  palette = list(discrete = "set2", continuous = "viridis"), extra = list(border = TRUE, stripPlain = TRUE, grid = "y", minorGrid = FALSE), size = list(width = 7.08, height = 4.5)),
  greyscale = list(label = "Greyscale printing", theme = list(base = "bw", size = 10, family = "sans", legend = "right", centreTitle = FALSE, noGrid = FALSE),
                   palette = list(discrete = "grey", continuous = "grey"), extra = list(border = TRUE, grid = "both", minorGrid = FALSE), size = list(width = 6.5, height = 4.5)),
  slides = list(label = "Slides (large type)", theme = list(base = "minimal", size = 16, family = "sans", legend = "bottom", centreTitle = TRUE, noGrid = FALSE),
                palette = list(discrete = "okabe", continuous = "viridis"), extra = list(grid = "y", minorGrid = FALSE), size = list(width = 10, height = 6))
)

gg_registry <- function() list(
  geoms = lapply(names(GG_GEOMS), function(g) { d <- GG_GEOMS[[g]]; list(id = g, label = d$label, group = d$group, fn = d$fn, aes = as.list(d$aes), req = as.list(d$req),
                                                                   params = lapply(names(d$params), function(p) list(name = p, type = d$params[[p]])), positions = as.list(d$positions %||% character(0))) }),
  aes = as.list(GG_AES), themes = as.list(names(GG_THEMES)), linetypes = as.list(GG_LINETYPES),
  palettes = list(discrete = as.list(names(GG_PAL_DISCRETE)), continuous = as.list(names(GG_PAL_CONTINUOUS))),
  transforms = as.list(GG_TRANS),
  presets = lapply(names(GG_PRESETS), function(k) { p <- GG_PRESETS[[k]]
    list(id = k, label = p$label, theme = p$theme, palette = p$palette, extra = p$extra, size = p$size) }))

gg_colour_ok <- function(x) is.character(x) && length(x) == 1 && (grepl("^#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$", x) || x %in% grDevices::colors())

#' Check one parameter value against its type; returns the cleaned value or NULL (dropped) / error.
gg_param <- function(type, v, what) {
  if (is.null(v) || (is.character(v) && !nzchar(v))) return(NULL)
  kind <- sub(":.*$", "", type)
  bad <- function() api_error(400, "gg_param", sprintf("'%s' has an invalid value.", what))
  switch(kind,
    num = , int = { x <- suppressWarnings(as.numeric(v))[1]; if (is.na(x)) bad(); r <- as.numeric(strsplit(type, ":")[[1]][2:3]); x <- min(max(x, r[1]), r[2]); if (kind == "int") as.integer(round(x)) else x },
    colour = { x <- as.character(v)[1]; if (!gg_colour_ok(x)) bad(); x },
    bool = isTRUE(v),
    enum = { x <- as.character(v)[1]; if (!x %in% strsplit(sub("^enum:", "", type), ",")[[1]]) bad(); x },
    linetype = { x <- as.character(v)[1]; if (!x %in% GG_LINETYPES) bad(); x },
    shape = { x <- suppressWarnings(as.integer(v))[1]; if (is.na(x) || x < 0 || x > 25) bad(); x },
    bad())
}

#' Validate a builder specification (unknown fields are dropped; invalid values are refused).
normalize_gg <- function(raw) {
  raw <- raw %||% list()
  aes_of <- function(a) { a <- a %||% list(); out <- list(); for (k in intersect(names(a), GG_AES)) { v <- as.character(a[[k]] %||% "")[1]; if (nzchar(v)) out[[k]] <- v }; out }
  layers <- lapply(raw$layers %||% list(), function(l) {
    g <- as.character(l$geom %||% "")[1]; def <- GG_GEOMS[[g]]
    if (is.null(def)) api_error(400, "gg_geom", sprintf("Unknown layer type '%s'.", g))
    a <- aes_of(l$aes); a <- a[intersect(names(a), def$aes)]
    p <- list(); for (k in intersect(names(l$params %||% list()), names(def$params))) { v <- gg_param(def$params[[k]], l$params[[k]], k); if (!is.null(v)) p[[k]] <- v }
    pos <- as.character(l$position %||% "")[1]; if (!nzchar(pos) || !pos %in% (def$positions %||% character(0))) pos <- ""
    list(geom = g, aes = a, params = p, position = pos, inherit = !isFALSE(l$inherit))
  })
  if (length(layers) > 12) api_error(400, "gg_layers", "A plot can have at most 12 layers.")
  f <- raw$facet %||% list()
  ftype <- as.character(f$type %||% "none")[1]; if (!ftype %in% c("none", "wrap", "grid")) ftype <- "none"
  facet <- list(type = ftype, wrap = head(as.character(unlist(f$wrap %||% list())), 2), rows = as.character(f$rows %||% "")[1], cols = as.character(f$cols %||% "")[1],
                scales = if ((f$scales %||% "fixed") %in% c("fixed", "free", "free_x", "free_y")) f$scales %||% "fixed" else "fixed",
                ncol = { n <- suppressWarnings(as.integer(f$ncol %||% NA)); if (is.na(n) || n < 1) NA_integer_ else min(n, 10L) })
  lab_keys <- c("title", "subtitle", "caption", "x", "y", "colour", "fill", "shape", "size", "linetype", "alpha")
  labs <- list(); for (k in intersect(names(raw$labels %||% list()), lab_keys)) { v <- as.character(raw$labels[[k]] %||% "")[1]; if (nzchar(v)) labs[[k]] <- substr(v, 1, 300) }
  th <- raw$theme %||% list()
  theme <- list(base = if ((th$base %||% "bw") %in% names(GG_THEMES)) th$base %||% "bw" else "bw",
                size = min(max(suppressWarnings(as.numeric(th$size %||% 12)), 6, na.rm = TRUE), 30),
                family = if ((th$family %||% "sans") %in% c("sans", "serif", "mono")) th$family %||% "sans" else "sans",
                legend = if ((th$legend %||% "right") %in% c("right", "bottom", "top", "left", "none")) th$legend %||% "right" else "right",
                centreTitle = isTRUE(th$centreTitle), noGrid = isTRUE(th$noGrid))
  sz <- raw$size %||% list()
  size <- list(width = min(max(suppressWarnings(as.numeric(sz$width %||% 7)), 2, na.rm = TRUE), 20), height = min(max(suppressWarnings(as.numeric(sz$height %||% 5)), 2, na.rm = TRUE), 20))
  t3 <- raw$three %||% list()
  three <- list(enabled = isTRUE(t3$enabled), z = { z <- as.character(t3$z %||% "")[1]; if (nzchar(z)) z else NULL },
                zLabel = { zl <- as.character(t3$zLabel %||% "")[1]; if (nzchar(zl)) substr(zl, 1, 200) else NULL },
                opacity = min(max(suppressWarnings(as.numeric(t3$opacity %||% 0.75)), 0.1, na.rm = TRUE), 1),
                aspect = if ((t3$aspect %||% "cube") %in% c("cube", "data", "auto")) t3$aspect %||% "cube" else "cube")
  # Direction of the axis labels (tick labels at 0, 45 or 90 degrees) and of the Y-axis title (vertical or horizontal).
  ax <- raw$axes %||% list()
  ang <- function(v) { v <- suppressWarnings(as.integer(v))[1]; if (is.na(v) || !v %in% c(0L, 45L, 90L)) 0L else v }
  axes <- list(x = ang(ax$x), y = ang(ax$y), z = ang(ax$z), yTitle = if (identical(ax$yTitle, "horizontal")) "horizontal" else "vertical")
  # Builder step 2: the colour scale, and limits / transform on the continuous axes. An empty limit is left to
  # ggplot2, so only a pair of finite numbers in order is accepted.
  pl <- raw$palette %||% list()
  pal_pick <- function(v, ok) { v <- as.character(v %||% "default")[1]; if (v %in% names(ok)) v else "default" }
  palette <- list(discrete = pal_pick(pl$discrete, GG_PAL_DISCRETE), continuous = pal_pick(pl$continuous, GG_PAL_CONTINUOUS),
                  direction = if (identical(suppressWarnings(as.integer(pl$direction %||% 1L))[1], -1L) || identical(pl$direction, -1) || identical(pl$direction, "-1") || identical(pl$direction, TRUE)) -1L else 1L)
  lim <- function(v) {
    n <- suppressWarnings(as.numeric(unlist(v %||% list())))
    if (length(n) != 2 || any(!is.finite(n)) || n[1] >= n[2]) NULL else n
  }
  one_scale <- function(s) list(limits = lim(s$limits), trans = if ((s$trans %||% "identity") %in% GG_TRANS) s$trans %||% "identity" else "identity")
  scales <- list(x = one_scale((raw$scales %||% list())$x %||% list()), y = one_scale((raw$scales %||% list())$y %||% list()))
  # Coordinates: zoom keeps the data and moves the window, unlike scale limits which drop what falls outside.
  co <- raw$coord %||% list()
  ratio <- suppressWarnings(as.numeric(co$ratio %||% NA))[1]
  coord <- list(flip = isTRUE(co$flip), zoomX = lim(co$zoomX), zoomY = lim(co$zoomY),
                ratio = if (is.na(ratio) || ratio <= 0 || ratio > 100) NULL else ratio,
                polar = isTRUE(co$polar))
  te <- raw$themeExtra %||% list()
  theme_extra <- list(grid = if ((te$grid %||% "both") %in% c("both", "x", "y", "none")) te$grid %||% "both" else "both",
                      minorGrid = !isFALSE(te$minorGrid), border = isTRUE(te$border), axisLine = isTRUE(te$axisLine),
                      stripPlain = isTRUE(te$stripPlain),
                      margin = { m <- suppressWarnings(as.numeric(te$margin %||% NA))[1]; if (is.na(m) || m < 0 || m > 40) NULL else m })
  anns <- lapply(head(raw$annotations %||% list(), 12), function(a) {
    ty <- as.character(a$type %||% "")[1]
    if (!ty %in% c("hline", "vline", "text", "rect")) return(NULL)
    num <- function(v) { n <- suppressWarnings(as.numeric(v %||% NA))[1]; if (is.finite(n)) n else NULL }
    col <- if (gg_colour_ok(as.character(a$colour %||% "")[1])) as.character(a$colour)[1] else "#444444"
    lt <- if ((a$linetype %||% "dashed") %in% GG_LINETYPES) a$linetype %||% "dashed" else "dashed"
    lab <- substr(as.character(a$label %||% "")[1], 1, 200)
    switch(ty,
      hline = { y <- num(a$y); if (is.null(y)) NULL else list(type = ty, y = y, colour = col, linetype = lt) },
      vline = { x <- num(a$x); if (is.null(x)) NULL else list(type = ty, x = x, colour = col, linetype = lt) },
      text  = { x <- num(a$x); y <- num(a$y); if (is.null(x) || is.null(y) || !nzchar(lab)) NULL else list(type = ty, x = x, y = y, label = lab, colour = col) },
      rect  = { xs <- lim(a$x); ys <- lim(a$y); if (is.null(xs) || is.null(ys)) NULL else list(type = ty, x = xs, y = ys, colour = col) })
  })
  annotations <- Filter(Negate(is.null), anns)
  list(aes = aes_of(raw$aes), layers = layers, facet = facet, labels = labs, theme = theme, coord = coord,
       palette = palette, scales = scales, themeExtra = theme_extra, annotations = annotations, size = size, three = three, axes = axes)
}

gg_vars <- function(sp) unique(c(if (isTRUE(sp$three$enabled)) sp$three$z, unlist(sp$aes), unlist(lapply(sp$layers, function(l) unlist(l$aes))), sp$facet$wrap, Filter(nzchar, c(sp$facet$rows, sp$facet$cols))))

gg_name <- function(v) { if (grepl("`", v, fixed = TRUE)) api_error(400, "gg_name", "Variable names with backticks cannot be plotted."); if (make.names(v) == v) v else paste0("`", v, "`") }

#' ggplot2 code of a specification. `discrete`: variables to turn into factors (nominal / ordinal measures).
gg_code <- function(sp, discrete = character(0), ordered = character(0), missing_code = NULL, data_line = TRUE) {
  dq <- function(x) if (is.integer(x)) as.character(x) else paste(deparse(x, width.cutoff = 500L), collapse = "")
  # A mapping is a variable name, or (AsIs) an expression written as it is.
  aes_txt <- function(a) if (length(a)) paste0("aes(", paste(sprintf("%s = %s", names(a), vapply(a, function(v) if (inherits(v, "AsIs")) as.character(v) else gg_name(v), "")), collapse = ", "), ")") else ""
  L <- c("library(ggplot2)")
  if (data_line) L <- c(L, "# data <- read.csv(\"your_data.csv\")   # the same dataset as in the app")
  if (!is.null(missing_code)) L <- c(L, sprintf("data[data == %s] <- NA   # project missing-value code", dq(missing_code)))
  for (v in discrete) L <- c(L, sprintf("data[[%s]] <- factor(data[[%s]]%s)", dq(v), dq(v), if (v %in% ordered) ", ordered = TRUE" else ""))
  parts <- sprintf("ggplot(data%s)", if (length(sp$aes)) paste0(", ", aes_txt(sp$aes)) else "")
  for (l in sp$layers) {
    def <- GG_GEOMS[[l$geom]]
    # A layer inherits every plot-wide mapping, and a position its geom does not take is an error in ggplot2 (a
    # histogram, bar or density counts one variable: "stat_bin() must only have an x or y aesthetic" when the plot
    # maps both x and y). Such a layer gets the plot-wide mappings it does take written into its own aes() and
    # inherits none. Reference lines (no aesthetics) never inherit, so they are left alone.
    own <- l$aes; inherit <- isTRUE(l$inherit)
    if (inherit && length(def$aes) && length(setdiff(intersect(names(sp$aes), c("x", "y", "ymin", "ymax", "sample")), c(def$aes, names(l$aes))))) {
      keep <- setdiff(intersect(names(sp$aes), def$aes), names(l$aes))
      own <- c(sp$aes[keep], l$aes); inherit <- FALSE
    }
    # A mapped group decides the groups on its own, so a discrete line type, shape, colour or fill that varies within a
    # group is dropped by a statistics layer ("The following aesthetics were dropped during statistical transformation:
    # linetype"). The layer then groups by the group variable and those variables together.
    eff <- c(if (inherit) sp$aes[setdiff(names(sp$aes), names(own))], own)
    if (!is.null(eff$group)) {
      split <- eff[intersect(names(eff), c("linetype", "shape", "colour", "fill", "alpha", "size"))]
      split <- split[names(split) %in% c("linetype", "shape") | unlist(split) %in% discrete]
      extra <- setdiff(unique(unlist(split)), eff$group)
      if (length(extra)) own$group <- I(sprintf("interaction(%s)", paste(vapply(c(eff$group, extra), gg_name, ""), collapse = ", ")))
    }
    args <- c(if (length(own)) aes_txt(own),
              vapply(names(l$params), function(k) sprintf("%s = %s", if (make.names(k) == k) k else paste0("`", k, "`"), dq(l$params[[k]])), ""),
              if (nzchar(l$position)) sprintf("position = %s", dq(l$position)),
              if (!inherit) "inherit.aes = FALSE",
              if (l$geom == "smooth" && is.null(l$params$method)) "method = \"lm\"",
              if (l$geom == "smooth") if (identical(l$params$method, "gam")) "formula = y ~ s(x, bs = \"cs\")" else "formula = y ~ x",
              if (l$geom == "summary" && is.null(l$params$fun.data)) "fun.data = \"mean_se\"")
    if (l$geom == "smooth" && identical(l$params$method, "glm")) args <- c(args, "method.args = list(family = \"binomial\")")
    parts <- c(parts, sprintf("%s(%s)", def$fn, paste(args, collapse = ", ")))
  }
  f <- sp$facet
  if (f$type == "wrap" && length(f$wrap)) parts <- c(parts, sprintf("facet_wrap(vars(%s)%s%s)", paste(vapply(f$wrap, gg_name, ""), collapse = ", "), if (f$scales != "fixed") sprintf(", scales = %s", dq(f$scales)) else "", if (!is.na(f$ncol)) sprintf(", ncol = %d", f$ncol) else ""))
  if (f$type == "grid" && (nzchar(f$rows) || nzchar(f$cols))) parts <- c(parts, sprintf("facet_grid(%s%s%s)", if (nzchar(f$rows)) sprintf("rows = vars(%s)", gg_name(f$rows)) else "", if (nzchar(f$rows) && nzchar(f$cols)) ", " else "", paste0(if (nzchar(f$cols)) sprintf("cols = vars(%s)", gg_name(f$cols)) else "", if (f$scales != "fixed") sprintf(", scales = %s", dq(f$scales)) else "")))
  # Builder step 2: a colour scale for every mapped colour / fill, discrete or continuous as the variable is.
  for (a in c("colour", "fill")) {
    used <- a %in% names(sp$aes) || any(vapply(sp$layers, function(l) a %in% names(l$aes), TRUE))
    if (!used) next
    v <- c(sp$aes[[a]], unlist(lapply(sp$layers, function(l) l$aes[[a]])))[1]
    cont <- !is.null(v) && !(v %in% discrete)
    pick <- if (cont) sp$palette$continuous %||% "default" else sp$palette$discrete %||% "default"
    fn <- if (cont) GG_PAL_CONTINUOUS[[pick]] %||% "" else GG_PAL_DISCRETE[[pick]] %||% ""
    if (!nzchar(fn)) next
    dir <- if (identical(sp$palette$direction, -1L)) ", direction = -1" else ""
    parts <- c(parts, switch(fn,
      manual = sprintf("scale_%s_manual(values = c(%s))", a, paste(sprintf('"%s"', GG_OKABE), collapse = ", ")),
      viridis_d = sprintf("scale_%s_viridis_d(option = %s%s)", a, dq(GG_PAL_OPTION[[pick]] %||% "viridis"), dir),
      viridis_c = sprintf("scale_%s_viridis_c(option = %s%s)", a, dq(GG_PAL_OPTION[[pick]] %||% "viridis"), dir),
      brewer = sprintf("scale_%s_brewer(palette = %s%s)", a, dq(GG_PAL_BREWER[[pick]] %||% "Set2"), dir),
      distiller = sprintf("scale_%s_distiller(palette = %s%s)", a, dq(GG_PAL_BREWER[[pick]] %||% "Blues"), dir),
      grey = sprintf("scale_%s_grey(start = 0.15, end = 0.85)", a),
      NULL))
  }
  # Limits and a transform on the continuous axes. ggplot2 drops data outside the limits, which is what a reader
  # expects from "limits"; a transform of identity is left out so the code stays the plot's own.
  for (a in c("x", "y")) {
    s <- (sp$scales %||% list())[[a]]
    if (is.null(s) || (is.null(s$limits) && identical(s$trans %||% "identity", "identity"))) next
    args <- c(if (!is.null(s$limits)) sprintf("limits = c(%s, %s)", dq(s$limits[1]), dq(s$limits[2])),
              if (!identical(s$trans, "identity")) sprintf("transform = %s", dq(s$trans)))
    parts <- c(parts, sprintf("scale_%s_continuous(%s)", a, paste(args, collapse = ", ")))
  }
  # Annotations are drawn after the layers so a reference line sits on top of the data.
  for (a in sp$annotations %||% list()) parts <- c(parts, switch(a$type,
    hline = sprintf("geom_hline(yintercept = %s, colour = %s, linetype = %s)", dq(a$y), dq(a$colour), dq(a$linetype)),
    vline = sprintf("geom_vline(xintercept = %s, colour = %s, linetype = %s)", dq(a$x), dq(a$colour), dq(a$linetype)),
    text  = sprintf("annotate(\"text\", x = %s, y = %s, label = %s, colour = %s)", dq(a$x), dq(a$y), dq(a$label), dq(a$colour)),
    rect  = sprintf("annotate(\"rect\", xmin = %s, xmax = %s, ymin = %s, ymax = %s, fill = %s, alpha = 0.15)",
                    dq(a$x[1]), dq(a$x[2]), dq(a$y[1]), dq(a$y[2]), dq(a$colour)),
    NULL))
  # Coordinates. coord_flip and coord_polar each replace the coordinate system, so a zoom or a fixed ratio is
  # written into whichever one is in use rather than added as a second coord_*.
  cd <- sp$coord
  zoom <- c(if (!is.null(cd$zoomX)) sprintf("xlim = c(%s, %s)", dq(cd$zoomX[1]), dq(cd$zoomX[2])),
            if (!is.null(cd$zoomY)) sprintf("ylim = c(%s, %s)", dq(cd$zoomY[1]), dq(cd$zoomY[2])))
  if (isTRUE(cd$polar)) parts <- c(parts, "coord_polar()")
  else if (isTRUE(cd$flip)) parts <- c(parts, sprintf("coord_flip(%s)", paste(zoom, collapse = ", ")))
  else if (!is.null(cd$ratio)) parts <- c(parts, sprintf("coord_fixed(ratio = %s%s)", dq(cd$ratio), if (length(zoom)) paste0(", ", paste(zoom, collapse = ", ")) else ""))
  else if (length(zoom)) parts <- c(parts, sprintf("coord_cartesian(%s)", paste(zoom, collapse = ", ")))
  if (length(sp$labels)) parts <- c(parts, sprintf("labs(%s)", paste(sprintf("%s = %s", names(sp$labels), vapply(sp$labels, dq, "")), collapse = ", ")))
  t <- sp$theme
  parts <- c(parts, sprintf("%s(base_size = %s, base_family = %s)", GG_THEMES[[t$base]], dq(t$size), dq(t$family)))
  ax <- sp$axes %||% list(x = 0L, y = 0L, yTitle = "vertical")
  th <- c(if (t$legend != "right") sprintf("legend.position = %s", dq(t$legend)), if (isTRUE(t$centreTitle)) "plot.title = element_text(hjust = 0.5)",
          switch(as.character(ax$x %||% 0L), "45" = "axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)", "90" = "axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)", NULL),
          switch(as.character(ax$y %||% 0L), "45" = "axis.text.y = element_text(angle = 45, hjust = 1, vjust = 0.5)", "90" = "axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.5)", NULL),
          if (identical(ax$yTitle, "horizontal")) "axis.title.y = element_text(angle = 0, vjust = 0.5)",
          if (isTRUE(t$noGrid) || t$base == "apa") "panel.grid = element_blank()",
          # Builder step 2 theme elements; they are written after the theme so they win, and "No grid lines" above
          # already blanks every grid, so these only speak when it is off.
          if (!isTRUE(t$noGrid) && t$base != "apa") switch(sp$themeExtra$grid %||% "both",
            none = "panel.grid = element_blank()",
            x = "panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()",
            y = "panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()", NULL),
          if (!isTRUE(t$noGrid) && t$base != "apa" && isFALSE(sp$themeExtra$minorGrid %||% TRUE)) "panel.grid.minor = element_blank()",
          if (isTRUE(sp$themeExtra$border)) "panel.border = element_rect(colour = \"black\", fill = NA)",
          if (isTRUE(sp$themeExtra$axisLine)) "axis.line = element_line(colour = \"black\")",
          if (isTRUE(sp$themeExtra$stripPlain)) "strip.background = element_blank()",
          if (!is.null(sp$themeExtra$margin)) sprintf("plot.margin = margin(%s, %s, %s, %s)", dq(sp$themeExtra$margin), dq(sp$themeExtra$margin), dq(sp$themeExtra$margin), dq(sp$themeExtra$margin)),
          if (t$base == "apa") c("axis.line = element_line(colour = \"black\")", "legend.key = element_blank()", "strip.background = element_blank()"))
  if (length(th)) parts <- c(parts, sprintf("theme(%s)", paste(th, collapse = ", ")))
  # Continuous colour / fill: a slim colour bar with the title above it, muted labels and short outside ticks (as the
  # colour bar of the 3D view) instead of ggplot2's wide bar with white ticks.
  cb <- gg_colourbar_aes(sp, discrete)
  if (length(cb) && t$legend != "none") {
    horiz <- t$legend %in% c("top", "bottom")
    g <- sprintf("guide_colourbar(theme = theme(legend.title.position = \"top\", legend.key.width = unit(%s, \"lines\"), legend.key.height = unit(%s, \"lines\"), legend.title = element_text(colour = \"#334155\", size = rel(0.9)), legend.text = element_text(colour = \"#64748B\", size = rel(0.8)), legend.ticks = element_line(colour = \"#94A3B8\", linewidth = 0.4), legend.ticks.length = unit(c(-0.25, 0), \"lines\")))",
                 if (horiz) "8" else "0.55", if (horiz) "0.55" else "7")
    parts <- c(parts, sprintf("guides(%s)", paste(sprintf("%s = %s", cb, g), collapse = ", ")))
  }
  c(L, "", paste0("p <- ", paste(parts, collapse = " +\n  ")), "p")
}

#' Colour and fill aesthetics mapped to a continuous variable anywhere in the plot (they get a colour bar).
gg_colourbar_aes <- function(sp, discrete) {
  maps <- c(list(sp$aes), lapply(sp$layers, function(l) l$aes))
  Filter(function(a) any(vapply(maps, function(m) { v <- m[[a]]; !is.null(v) && !inherits(v, "AsIs") && !(as.character(v) %in% discrete) }, TRUE)), c("colour", "fill"))
}

#' Settings that make a plot misleading without an error (also shown in the builder, ggLayerNotes in ggplot.ts): a fixed
#' setting replaces the mapping of the same aesthetic, whose legend then matches nothing; a fill on points whose shape
#' takes none (only shapes 21-25 have a fill).
gg_layer_notes <- function(sp) {
  nm <- c(colour = "colour", fill = "fill", shape = "shape", size = "size", alpha = "transparency", linetype = "line type")
  out <- character(0)
  for (i in seq_along(sp$layers)) {
    l <- sp$layers[[i]]; def <- GG_GEOMS[[l$geom]]
    eff <- c(l$aes, if (isTRUE(l$inherit)) sp$aes[setdiff(intersect(names(sp$aes), def$aes), names(l$aes))])
    tag <- sprintf("Layer %d \u00b7 %s", i, def$label)
    for (k in intersect(names(l$params), intersect(names(nm), names(eff))))
      out <- c(out, sprintf("%s: a fixed %s is set, so the %s mapping (%s) has no effect. Clear one of them.", tag, nm[[k]], nm[[k]], eff[[k]]))
    if (l$geom %in% c("point", "jitter") && (!is.null(eff$fill) || !is.null(l$params$fill)) && !isTRUE(l$params$shape %in% 21:25))
      out <- c(out, sprintf("%s: this shape has no fill, so Fill has no effect. Choose a shape \u201cwith fill\u201d or clear Fill.", tag))
  }
  out
}

#' ggplot2's own warnings in plain words: the hints it adds (lines starting with "i") are dropped and the common ones are
#' rewritten; each note starts with what it is about ("Plot: ...").
gg_tidy_warnings <- function(w) {
  w <- trimws(sub("\\s*(\u2139|\\bi\\s+This can happen|\\bi\\s+Did you).*$", "", w, perl = TRUE))
  vapply(w, function(x) {
    m <- regmatches(x, regexec("aesthetics were dropped during statistical transformation: (.+?)\\.?$", x, perl = TRUE))[[1]]
    if (length(m)) return(sprintf("Plot: %s cannot vary within a summary layer (such as a density or smoother), so %s not applied there. To draw one summary per category, map a categorical variable or set Group.",
                                  sub("^(.)", "\\U\\1", m[2], perl = TRUE), if (grepl(" and |, ", m[2])) "they are" else "it is"))
    m <- regmatches(x, regexec("^Computation failed in `stat_([a-z0-9_]+)\\(\\)`", x, perl = TRUE))[[1]]
    if (length(m)) {
      what <- c(density2d = "2D density", density_2d = "2D density", density_2d_filled = "2D density", density = "density", bin = "histogram",
                count = "bar", smooth = "smoother", summary = "summary", boxplot = "box plot", ydensity = "violin", qq = "Q-Q", qq_line = "Q-Q line")[m[2]]
      what <- if (is.na(what)) "one" else what
      why <- if (grepl("bandwidth|must contain numbers larger than 0", x)) " It needs numeric X and Y with enough different values (ordinal or categorical variables with few levels do not work)." else ""
      return(sprintf("Plot: the %s layer could not be computed and is left out.%s", what, why))
    }
    m <- regmatches(x, regexec("^Removed (\\d+) rows? containing (missing values|non-finite|missing values or values outside the scale range)", x, perl = TRUE))[[1]]
    if (length(m)) return(sprintf("Plot: %s row%s with missing or out-of-range values %s left out.", m[2], if (m[2] == "1") "" else "s", if (m[2] == "1") "was" else "were"))
    if (grepl("^(Layer \\d+|3D view|Plot)\\b", x)) x else paste0("Plot: ", x)
  }, "", USE.NAMES = FALSE)
}

#' Prepare the data (missing code, factors for discrete mappings) and evaluate the generated code.
run_ggplot <- function(df, sp, measures, missing_code = NULL, dpi = 110, format = "png", levels = list()) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) api_error(501, "gg_unavailable", "The ggplot2 R package is not installed on the analysis server.")
  if (!length(sp$layers)) api_error(422, "gg_layers", "Add at least one layer.")
  vars <- gg_vars(sp)
  miss <- setdiff(vars, names(df)); if (length(miss)) api_error(422, "gg_missing", sprintf("Not in the dataset: %s.", paste(miss, collapse = ", ")))
  for (l in sp$layers) { have <- c(names(l$aes), if (isTRUE(l$inherit)) names(sp$aes)); need <- setdiff(GG_GEOMS[[l$geom]]$req, have)
    if (length(need)) api_error(422, "gg_aes", sprintf("The %s layer needs %s.", tolower(GG_GEOMS[[l$geom]]$label), paste(need, collapse = " and "))) }
  d <- df[, vars, drop = FALSE]
  if (!is.null(missing_code)) for (cn in names(d)) if (is.numeric(d[[cn]])) d[[cn]][!is.na(d[[cn]]) & d[[cn]] == missing_code] <- NA
  disc_aes <- unique(c(unlist(lapply(c(list(sp$aes), lapply(sp$layers, `[[`, "aes")), function(a) unlist(a[intersect(names(a), GG_DISCRETE_AES)]))), sp$facet$wrap, Filter(nzchar, c(sp$facet$rows, sp$facet$cols))))
  nominal <- names(Filter(function(m) m %in% c("nominal", "ordinal"), measures))
  discrete <- intersect(vars, unique(c(disc_aes, nominal, names(d)[vapply(d, function(x) is.character(x) || is.logical(x), TRUE)])))
  ordered <- intersect(discrete, names(Filter(function(m) identical(m, "ordinal"), measures)))
  code <- gg_code(sp, discrete, ordered, missing_code)
  data <- d
  for (v in discrete) data[[v]] <- if (!is.null(levels[[v]])) labelled_factor(data[[v]], levels[[v]], ordered = v %in% ordered) else factor(data[[v]], ordered = v %in% ordered)
  env <- new.env(parent = asNamespace("ggplot2")); env$data <- data
  warns <- gg_layer_notes(sp)
  p <- withCallingHandlers(tryCatch(eval(parse(text = paste(code[-(1:which(code == "")[1])], collapse = "\n")), envir = env),
                                    error = function(e) api_error(422, "gg_failed", paste("The plot could not be built:", sanitize_message(conditionMessage(e))))),
                           warning = function(w) { warns <<- c(warns, sanitize_message(conditionMessage(w))); invokeRestart("muffleWarning") },
                           message = function(m) invokeRestart("muffleMessage"))
  path <- gg_render(p, sp$size$width, sp$size$height, dpi, format, function(w) warns <<- c(warns, w))
  fig3d <- if (isTRUE(sp$three$enabled)) gg3d_figure(data, sp, function(w) warns <<- c(warns, w)) else NULL
  if (!is.null(fig3d)) code <- c(code, gg3d_code(sp))
  list(path = path, code = paste(code, collapse = "\n"), warnings = unique(gg_tidy_warnings(warns)), discrete = discrete, fig3d = fig3d)
}

gg_render <- function(p, width, height, dpi, format, warn) {
  path <- tempfile(fileext = paste0(".", format))
  dev <- switch(format, png = if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png", pdf = grDevices::cairo_pdf, svg = grDevices::svg, tiff = if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_tiff else "tiff",
                api_error(400, "gg_format", "Choose png, pdf, svg or tiff."))
  withCallingHandlers(tryCatch(ggplot2::ggsave(path, p, device = dev, width = width, height = height, units = "in", dpi = dpi, bg = "white"),
                               error = function(e) api_error(422, "gg_failed", paste("The plot could not be drawn:", sanitize_message(conditionMessage(e))))),
                      warning = function(w) { warn(sanitize_message(conditionMessage(w))); invokeRestart("muffleWarning") },
                      message = function(m) invokeRestart("muffleMessage"))
  path
}

#' The builder as a Statistics procedure: the rendered PNG is the plot; the code is the R syntax.
run_stats_ggplot <- function(df, s, missing_code) {
  t0 <- Sys.time()
  sp <- s$options$spec
  out <- run_ggplot(df, sp, s$measures, missing_code, dpi = 110, format = "png", levels = s$levels)
  png <- jsonlite::base64_enc(readBin(out$path, "raw", file.info(out$path)$size))
  list(kind = "stats", proc = "ggplot", label = "ggplot2 Builder", group = "Plots", options = s$options, measures = s$measures,
       n = list(input = nrow(df), complete = nrow(df), cellsRecoded = 0L), tables = list(),
       plots = Filter(Negate(is.null), list(
         list(type = "image", png = png, width = sp$size$width, height = sp$size$height, title = sp$labels$title %||% "",
              alt = paste0("ggplot2 plot", if (length(sp$labels$title)) paste0(": ", sp$labels$title) else "")),
         if (!is.null(out$fig3d)) list(type = "plotly3d", figure = out$fig3d, title = sp$labels$title %||% "", width = sp$size$width, height = sp$size$height))),
       warnings = as.list(out$warnings), rCode = out$code, packages = list(ggplot2 = as.character(utils::packageVersion("ggplot2"))),
       durationSeconds = as.numeric(difftime(Sys.time(), t0, units = "secs")))
}
