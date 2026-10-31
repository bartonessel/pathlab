# 3D version of a ggplot2 builder plot. ggplot2 draws in 2D only, so the layers are translated into a 3D figure
# (plotly.js traces and layout) built here in R: points and lines in 3D, smoothers as fitted surfaces (lm plane, loess,
# GAM), density-type layers as bivariate kernel density surfaces (MASS::kde2d), bars as 3D bars. Every coordinate,
# fitted value and density is computed in R; the browser only renders the figure. Colours follow ggplot2's defaults
# (scales::hue_pal for groups, the default blue gradient for continuous colour) so the 2D and 3D versions match.

GG3D_POINTS <- c("point", "jitter", "text")
GG3D_LINES <- c("line")
GG3D_SURF <- c("smooth")
GG3D_DENS <- c("density2d", "density", "violin", "boxplot", "rug")
GG3D_BARS <- c("bar", "col", "histogram", "tile", "area", "summary", "errorbar")

g3_num <- function(v) if (is.factor(v)) as.numeric(v) else suppressWarnings(as.numeric(v))
g3_axis <- function(v, title) {
  ax <- list(title = list(text = title), backgroundcolor = "#ffffff", gridcolor = "#d9dde3", zerolinecolor = "#aab2bd", showbackground = TRUE)
  if (is.factor(v)) { lv <- levels(droplevels(v)); ax$tickvals <- as.list(seq_along(levels(v))[levels(v) %in% lv]); ax$ticktext <- as.list(levels(v)[levels(v) %in% lv]) }
  ax
}
g3_cuboid <- function(x0, x1, y0, y1, z0, z1, k) {
  # Vertex order of plotly's cube example, so its 12 triangles close the box.
  list(x = c(x0, x0, x1, x1, x0, x0, x1, x1), y = c(y0, y1, y1, y0, y0, y1, y1, y0), z = c(z0, z0, z0, z0, z1, z1, z1, z1),
       i = k + c(7, 0, 0, 0, 4, 4, 6, 6, 4, 0, 3, 2), j = k + c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3), k = k + c(0, 7, 2, 3, 6, 7, 1, 1, 5, 5, 7, 6))
}

#' Groups of a layer: by the discrete colour (or fill) variable, coloured as in ggplot2; else one group.
g3_groups <- function(d, cvar) {
  if (!is.null(cvar) && is.factor(d[[cvar]])) {
    lv <- levels(droplevels(d[[cvar]])); pal <- scales::hue_pal()(length(lv))
    lapply(seq_along(lv), function(i) list(name = lv[i], rows = which(d[[cvar]] == lv[i]), colour = pal[i]))
  } else list(list(name = "", rows = seq_len(nrow(d)), colour = "#595959"))
}

gg3d_figure <- function(d, sp, warn) {
  th <- sp$three
  aes_of <- function(l) if (isTRUE(l$inherit)) utils::modifyList(sp$aes, l$aes) else l$aes
  xv <- sp$aes$x; yv <- sp$aes$y; zv <- th$z
  if (is.null(xv) || is.null(yv)) api_error(422, "gg3d_aes", "The 3D view needs the shared X-axis and Y-axis variables.")
  traces <- list(); zlab <- if (nzchar(zv %||% "")) zv else "Density"; any_z <- FALSE
  op <- th$opacity %||% 0.75
  for (l in sp$layers) {
    a <- aes_of(l); g <- l$geom; p <- l$params
    cvar <- a$colour %||% a$fill
    cont_col <- !is.null(cvar) && !is.factor(d[[cvar]])
    const_col <- p$colour %||% p$fill
    if (g %in% c(GG3D_POINTS, GG3D_LINES)) {
      if (!nzchar(zv %||% "")) { warn(sprintf("%s: choose a Z variable to draw it in 3D.", GG_GEOMS[[g]]$label)); next }
      any_z <- TRUE
      for (gr in g3_groups(d, if (cont_col) NULL else cvar)) {
        r <- gr$rows; r <- r[stats::complete.cases(d[r, c(xv, yv, zv), drop = FALSE])]
        if (!length(r)) next
        if (g %in% GG3D_LINES) r <- r[order(g3_num(d[[xv]][r]))]
        sz <- if (!is.null(a$size) && is.numeric(d[[a$size]])) { s <- d[[a$size]][r]; 3 + 9 * (s - min(s, na.rm = TRUE)) / max(1e-9, diff(range(s, na.rm = TRUE))) } else (p$size %||% 1.5) * 2.4
        col <- if (cont_col) list(color = as.list(g3_num(d[[cvar]][r])), colorscale = list(list(0, "#132B43"), list(1, "#56B1F7")), showscale = TRUE, colorbar = list(title = list(text = sp$labels$colour %||% cvar)))
               else list(color = const_col %||% gr$colour)
        tr <- list(type = "scatter3d", x = as.list(g3_num(d[[xv]][r])), y = as.list(g3_num(d[[yv]][r])), z = as.list(g3_num(d[[zv]][r])),
                   name = if (nzchar(gr$name)) gr$name else GG_GEOMS[[g]]$label, showlegend = nzchar(gr$name), legendgroup = gr$name,
                   opacity = p$alpha %||% (if (g %in% GG3D_LINES) 1 else 0.85))
        if (g %in% GG3D_LINES) { tr$mode <- "lines"; tr$line <- list(color = col$color, width = (p$linewidth %||% 0.8) * 5) }
        else { tr$mode <- if (g == "text") "markers+text" else "markers"; tr$marker <- c(list(size = if (length(sz) > 1) as.list(sz) else sz), col, list(line = list(width = 0)))
               if (g == "text" && !is.null(a$label)) tr$text <- as.list(as.character(d[[a$label]][r])) }
        traces[[length(traces) + 1]] <- tr
      }
    } else if (g %in% GG3D_SURF) {
      if (!nzchar(zv %||% "")) { warn("Smoothed surface: choose a Z variable to fit it in 3D."); next }
      any_z <- TRUE
      method <- p$method %||% "lm"
      for (gr in g3_groups(d, if (cont_col) NULL else cvar)) {
        dd <- data.frame(x = g3_num(d[[xv]][gr$rows]), y = g3_num(d[[yv]][gr$rows]), z = g3_num(d[[zv]][gr$rows])); dd <- dd[stats::complete.cases(dd), ]
        if (nrow(dd) < 5) next
        fit <- tryCatch(switch(method, loess = stats::loess(z ~ x + y, data = dd), gam = mgcv::gam(z ~ s(x, y, k = min(20, nrow(dd) - 2)), data = dd), glm = , lm = stats::lm(z ~ x + y, data = dd)),
                        error = function(e) { warn(paste("Surface not fitted:", conditionMessage(e))); NULL })
        if (is.null(fit)) next
        gx <- seq(min(dd$x), max(dd$x), length.out = 30); gy <- seq(min(dd$y), max(dd$y), length.out = 30)
        nd <- expand.grid(x = gx, y = gy)
        zz <- matrix(as.numeric(stats::predict(fit, newdata = nd)), nrow = length(gx))
        traces[[length(traces) + 1]] <- list(type = "surface", x = as.list(gx), y = as.list(gy), z = lapply(seq_along(gy), function(j) as.list(zz[, j])),
                                             opacity = op, showscale = FALSE, name = paste0(if (nzchar(gr$name)) paste0(gr$name, ": ") else "", c(lm = "linear plane", glm = "linear plane", loess = "loess surface", gam = "GAM surface")[[method]]),
                                             colorscale = list(list(0, const_col %||% gr$colour), list(1, const_col %||% gr$colour)), showlegend = FALSE, hoverinfo = "x+y+z")
      }
    } else if (g %in% GG3D_DENS) {
      if (nzchar(zv %||% "")) { warn(sprintf("%s: shown as a density surface only when no Z variable is chosen.", GG_GEOMS[[g]]$label)); next }
      dd <- data.frame(x = g3_num(d[[xv]]), y = g3_num(d[[yv]])); dd <- dd[stats::complete.cases(dd), ]
      if (nrow(dd) < 5) next
      k <- MASS::kde2d(dd$x, dd$y, n = 40)
      traces[[length(traces) + 1]] <- list(type = "surface", x = as.list(k$x), y = as.list(k$y), z = lapply(seq_along(k$y), function(j) as.list(k$z[, j])), opacity = op,
                                           colorscale = "Viridis", showscale = TRUE, colorbar = list(title = list(text = "Density")), name = "Kernel density", hoverinfo = "x+y+z")
    } else if (g %in% GG3D_BARS) {
      xs <- d[[xv]]; ys <- d[[yv]]
      bin <- function(v) if (is.factor(v)) v else cut(v, breaks = pretty(range(v, na.rm = TRUE), n = 8), include.lowest = TRUE)
      bx <- bin(xs); by <- bin(ys)
      h <- if (nzchar(zv %||% "")) { any_z <- TRUE; tapply(g3_num(d[[zv]]), list(bx, by), mean, na.rm = TRUE) } else table(bx, by)
      h[is.na(h)] <- 0
      cols <- scales::hue_pal()(ncol(h)); X <- c(); Y <- c(); Z <- c(); I <- c(); J <- c(); K <- c(); FC <- c()
      for (i in seq_len(nrow(h))) for (j in seq_len(ncol(h))) if (h[i, j] != 0) {
        cb <- g3_cuboid(i - 0.35, i + 0.35, j - 0.35, j + 0.35, min(0, h[i, j]), max(0, h[i, j]), length(X))
        X <- c(X, cb$x); Y <- c(Y, cb$y); Z <- c(Z, cb$z); I <- c(I, cb$i); J <- c(J, cb$j); K <- c(K, cb$k); FC <- c(FC, rep(const_col %||% cols[j], 12))
      }
      if (length(X)) traces[[length(traces) + 1]] <- list(type = "mesh3d", x = as.list(X), y = as.list(Y), z = as.list(Z), i = as.list(I), j = as.list(J), k = as.list(K),
                                                          facecolor = as.list(FC), flatshading = TRUE, opacity = p$alpha %||% 0.9, name = if (nzchar(zv %||% "")) paste("Mean of", zv) else "Count", hoverinfo = "z")
      d[[xv]] <- factor(as.character(bx), levels = rownames(h)); d[[yv]] <- factor(as.character(by), levels = colnames(h))
      if (!nzchar(zv %||% "")) zlab <- "Count"
    } else warn(sprintf("%s has no 3D equivalent and is not shown in the 3D view.", GG_GEOMS[[g]]$label))
  }
  if (!length(traces)) api_error(422, "gg3d_empty", "None of the layers can be drawn in 3D. Use points, lines, a smoother (with a Z variable), a density or bars.")
  fam <- c(sans = "Arial, Helvetica, sans-serif", serif = "Times New Roman, Times, serif", mono = "Courier New, monospace")[[sp$theme$family]]
  layout <- list(title = list(text = sp$labels$title %||% "", x = if (isTRUE(sp$theme$centreTitle)) 0.5 else 0.02),
                 font = list(family = fam, size = sp$theme$size), paper_bgcolor = "#ffffff", showlegend = sp$theme$legend != "none",
                 legend = list(orientation = if (sp$theme$legend %in% c("bottom", "top")) "h" else "v", title = list(text = sp$labels$colour %||% sp$labels$fill %||% (sp$aes$colour %||% sp$aes$fill %||% ""))),
                 margin = list(l = 0, r = 0, t = if (nzchar(sp$labels$title %||% "")) 50 else 10, b = 0),
                 scene = list(xaxis = g3_axis(d[[xv]], sp$labels$x %||% xv), yaxis = g3_axis(d[[yv]], sp$labels$y %||% yv), zaxis = g3_axis(numeric(0), th$zLabel %||% zlab),
                              aspectmode = th$aspect %||% "cube", camera = list(eye = list(x = 1.6, y = -1.6, z = 1.1))))
  list(data = traces, layout = layout)
}

#' R code that reproduces the 3D figure with the plotly R package (points / surfaces), for the R syntax panel.
gg3d_code <- function(sp) {
  q <- function(x) paste0("\"", gsub("\"", "\\\\\"", x), "\"")
  c("", "# 3D view (plotly); surfaces are fitted in R, e.g. for a linear plane:", "library(plotly)",
    sprintf("plot_ly(data, x = ~%s, y = ~%s, z = ~%s%s, type = \"scatter3d\", mode = \"markers\")", gg_name(sp$aes$x), gg_name(sp$aes$y), gg_name(sp$three$z %||% "z"),
            if (!is.null(sp$aes$colour)) paste0(", color = ~", gg_name(sp$aes$colour)) else ""),
    sprintf("# fit <- lm(%s ~ %s + %s, data = data)   # smoother layer as a plane; loess() / mgcv::gam() for curved surfaces", gg_name(sp$three$z %||% "z"), gg_name(sp$aes$x), gg_name(sp$aes$y)))
}
