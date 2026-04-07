library(shiny)
library(bslib)
library(shinyjs)
library(dySEM)
library(lavaan)
library(semPlot)

# ── Model Metadata ──────────────────────────────────────────────────────────

MODEL_TYPES <- list(
  uni = c(
    "Unidimensional" = "scriptUni",
    "Correlated Factors" = "scriptCor",
    "Hierarchical" = "scriptHier",
    "Bifactor" = "scriptBifac"
  ),
  bi = c(
    "L-APIM (Actor-Partner)" = "scriptAPIM",
    "Common Fate Model" = "scriptCFM",
    "Mutual Influence Model" = "scriptMIM",
    "Two-Construct (Flexible)" = "scriptTwoCross"
  ),
  multi = c(
    "Multiple Correlated Factors (M-CDFM)" = "scriptCFA"
  )
)

MODEL_DESCRIPTIONS <- list(
  scriptUni = "Single shared latent factor for the dyad. All indicators from both partners load onto one factor (Kenny & Ledermann, 2010).",
  scriptCor = "Separate latent factors for each partner, freely correlated. The standard Correlated Dyadic Factor Model (Ledermann & Kenny, 2017).",
  scriptHier = "Second-order latent factor above partner-specific first-order factors. Tests whether a higher-order dyadic process explains partner covariation.",
  scriptBifac = "General (dyadic) factor plus partner-specific factors. Indicators load on both the general and specific factors (Eid et al., 2017).",
  scriptAPIM = "Latent Actor-Partner Interdependence Model. Separate X and Y constructs with actor and partner effects from X to Y (Kenny et al., 2006).",

  scriptCFM = "Common Fate Model. A dyadic-level latent X predicts a dyadic-level latent Y, with partner-specific residual covariances (Ledermann & Kenny, 2012).",
  scriptMIM = "Mutual Influence Model. Partners' latent Y scores are predicted by their own and their partner's latent X (Christensen et al., 2006).",
  scriptTwoCross = "Flexible two-construct model. Choose independent measurement structures (uni/cor/hier/bifac) for X and Y, with optional X-to-Y structural paths.",
  scriptCFA = "Multiple correlated dyadic factors. Each factor has partner-specific latent variables that are freely correlated within and across factors."
)

INVARIANCE_PRESETS <- list(
  "Configural" = list(meas = "none", struct = "none"),
  "Metric (Loading)" = list(meas = "loadings", struct = "none"),
  "Scalar (Intercept)" = list(meas = c("loadings", "intercepts"), struct = "none"),
  "Strict (Residual)" = list(meas = c("loadings", "intercepts", "residuals"), struct = "none"),
  "Indistinguishable" = list(meas = c("loadings", "intercepts", "residuals"), struct = c("variances", "means")),
  "Custom" = NULL
)

XY_STRUCT_OPTIONS <- list(
  scriptAPIM = c(
    "Equate actors & partners" = "actors,partners",
    "Equate actors only" = "actors",
    "Equate partners only" = "partners",
    "Equate all paths" = "all",
    "Fix actors to zero" = "actors_zero",
    "Fix partners to zero" = "partners_zero",
    "Free (no constraints)" = "none"
  ),
  scriptMIM = c(
    "Equate actors & partners" = "actors,partners",
    "Equate actors only" = "actors",
    "Equate partners only" = "partners",
    "Equate all paths" = "all",
    "Fix actors to zero" = "actors_zero",
    "Fix partners to zero" = "partners_zero",
    "Free (no constraints)" = "none"
  ),
  scriptCFM = c(
    "No constraints" = "none",
    "Fix P1 residual cov to zero" = "p1_zero",
    "Fix P2 residual cov to zero" = "p2_zero",
    "Fix both residual covs to zero" = "cov_zero",
    "Fix dyadic effect to zero" = "dyadic_zero"
  ),
  scriptTwoCross = c(
    "Free (all paths estimated)" = "free",
    "Zero (no X-to-Y paths)" = "zero"
  )
)

NAMING_ORDER_LABELS <- c(
  "Stem-Partner-Item (e.g., sat.1_1)" = "spi",
  "Stem-Item-Partner (e.g., sat_1.1)" = "sip",
  "Partner-Stem-Item (e.g., 1.sat_1)" = "psi"
)

# Colorblind-friendly palette (Okabe-Ito derived)
SEM_COLORS <- list(
  latent       = "#7B68EE",
  latent_fill  = "#E8E3FC",
  lat_var      = "#B8A9EE",
  loading      = "#009E73",
  indicator    = "#0072B2",
  ind_fill     = "#D6EAF5",
  resid_var    = "#D55E00",
  resid_cov    = "#E69F00",
  lat_cov      = "#CC79A7",
  intercept    = "#56B4E9",
  constraint   = "#999999",
  section      = "#555555"
)

UNDERSTANDING_CSS <- "
.sem-latent     { color: #7B68EE; font-weight: bold; }
.sem-lat-var    { color: #B8A9EE; font-weight: bold; }
.sem-loading    { color: #009E73; font-style: italic; }
.sem-indicator  { color: #0072B2; }
.sem-resid-var  { color: #D55E00; font-style: italic; }
.sem-resid-cov  { color: #E69F00; font-style: italic; text-decoration: underline dashed #E69F00; }
.sem-lat-cov    { color: #CC79A7; font-weight: bold; text-decoration: underline dashed #CC79A7; }
.sem-intercept  { color: #56B4E9; }
.sem-constraint { color: #999999; font-family: 'Fira Code', monospace; font-size: 0.9em; }
.sem-section    { color: #555555; font-weight: bold; font-size: 1.05em; margin-top: 8px; }
.sem-line       { margin-bottom: 3px; line-height: 1.6; }
.understand-panel {
  font-family: 'Fira Code', monospace;
  font-size: 0.82em;
  padding: 10px;
  overflow-y: auto;
  max-height: 500px;
  white-space: pre-wrap;
  word-break: break-word;
}
.understand-english {
  font-family: 'Source Sans Pro', sans-serif;
  font-size: 0.92em;
  padding: 10px;
  overflow-y: auto;
  max-height: 500px;
  line-height: 1.7;
}
.understand-english .sem-line { margin-bottom: 6px; }
.color-legend {
  display: flex;
  flex-wrap: wrap;
  gap: 12px;
  padding: 8px 12px;
  background: #f8f9fa;
  border-radius: 6px;
  margin-bottom: 12px;
  font-size: 0.82em;
}
.color-legend-item {
  display: flex;
  align-items: center;
  gap: 4px;
}
.color-chip {
  width: 14px;
  height: 14px;
  border-radius: 3px;
  display: inline-block;
}
"

SCROLL_SYNC_JS <- "
$(document).ready(function() {
  var syncing = false;
  function syncScroll(source, target) {
    if (syncing) return;
    syncing = true;
    var pct = source.scrollTop / (source.scrollHeight - source.clientHeight || 1);
    target.scrollTop = pct * (target.scrollHeight - target.clientHeight || 1);
    syncing = false;
  }
  $(document).on('scroll', '#understand_syntax_panel', function() {
    var t = document.getElementById('understand_english_panel');
    if (t) syncScroll(this, t);
  });
  $(document).on('scroll', '#understand_english_panel', function() {
    var t = document.getElementById('understand_syntax_panel');
    if (t) syncScroll(this, t);
  });
});
"


# ── Helper Functions ────────────────────────────────────────────────────────

make_varnames <- function(order, stem, delim1, delim2, n_items, distinguish) {
  items <- seq_len(n_items)
  switch(order,
    sip = paste0(stem, delim1, items, delim2, distinguish),
    spi = paste0(stem, delim1, distinguish, delim2, items),
    psi = paste0(distinguish, delim1, stem, delim2, items)
  )
}

build_dvn_from_inputs <- function(category, order, stem_x, delim1_x, delim2_x,
                                  n_items_x, dist1, dist2, lvxname = "X",
                                  stem_y = NULL, delim1_y = NULL, delim2_y = NULL,
                                  n_items_y = NULL, lvyname = "Y",
                                  mc_info = NULL) {
  if (category == "multi" && !is.null(mc_info)) {
    p1x <- list()
    p2x <- list()
    for (i in seq_along(mc_info$lvnames)) {
      nm <- mc_info$lvnames[i]
      p1x[[nm]] <- make_varnames(order, mc_info$stems[i], mc_info$delim1s[i],
                                 mc_info$delim2s[i], mc_info$n_items[i], dist1)
      p2x[[nm]] <- make_varnames(order, mc_info$stems[i], mc_info$delim1s[i],
                                 mc_info$delim2s[i], mc_info$n_items[i], dist2)
    }
    total_per_partner <- sum(mc_info$n_items)
    return(list(
      p1xvarnames = p1x,
      p2xvarnames = p2x,
      xindper = total_per_partner,
      dist1 = dist1,
      dist2 = dist2,
      indnum = total_per_partner * 2L
    ))
  }

  p1x <- make_varnames(order, stem_x, delim1_x, delim2_x, n_items_x, dist1)
  p2x <- make_varnames(order, stem_x, delim1_x, delim2_x, n_items_x, dist2)

  dvn <- list(
    p1xvarnames = p1x,
    p2xvarnames = p2x,
    xindper = n_items_x,
    dist1 = dist1,
    dist2 = dist2,
    indnum = 2L * n_items_x
  )

  if (category == "bi" && !is.null(stem_y) && !is.null(n_items_y)) {
    d1y <- if (!is.null(delim1_y) && nzchar(delim1_y)) delim1_y else delim1_x
    d2y <- if (!is.null(delim2_y) && nzchar(delim2_y)) delim2_y else delim2_x
    p1y <- make_varnames(order, stem_y, d1y, d2y, n_items_y, dist1)
    p2y <- make_varnames(order, stem_y, d1y, d2y, n_items_y, dist2)
    dvn$p1yvarnames <- p1y
    dvn$p2yvarnames <- p2y
    dvn$yindper <- n_items_y
    dvn$indnum <- 2L * (n_items_x + n_items_y)
  }

  dvn
}

get_all_varnames <- function(dvn) {
  vars <- c()
  if (is.list(dvn$p1xvarnames) && !is.null(names(dvn$p1xvarnames))) {
    vars <- c(vars, unlist(dvn$p1xvarnames), unlist(dvn$p2xvarnames))
  } else {
    vars <- c(vars, dvn$p1xvarnames, dvn$p2xvarnames)
  }
  if (!is.null(dvn$p1yvarnames)) {
    vars <- c(vars, dvn$p1yvarnames, dvn$p2yvarnames)
  }
  vars
}

make_dummy_data <- function(varnames) {
  n <- max(length(varnames) * 10, 50)
  set.seed(42)
  dat <- as.data.frame(
    matrix(stats::rnorm(n * length(varnames)), nrow = n, ncol = length(varnames))
  )
  names(dat) <- varnames
  dat
}

parse_constraint_string <- function(s) {
  if (is.null(s) || s == "none") return("none")
  trimmed <- trimws(unlist(strsplit(s, ",")))
  trimmed[nzchar(trimmed)]
}

h_esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

span_cls <- function(text, cls) {
  paste0('<span class="', cls, '">', h_esc(text), '</span>')
}

# ── Understanding Panel: Color-Coded Syntax ────────────────────────────────

detect_section <- function(line) {
  line <- trimws(line)
  if (!startsWith(line, "#")) return(NULL)
  low <- tolower(line)
  if (grepl("loading", low))           return("loading")
  if (grepl("intercept", low))         return("intercept")
  if (grepl("residual var", low))      return("resid_var")
  if (grepl("residual co", low))       return("resid_cov")
  if (grepl("latent.*(co).*var|\\(co\\)var", low)) return("lat_struct")
  if (grepl("latent var", low))        return("lat_struct")
  if (grepl("latent mean", low))       return("lat_mean")
  if (grepl("measurement", low))       return("heading")
  if (grepl("structural", low))        return("heading")
  "heading"
}

color_syntax_line <- function(line, section, latent_names) {
  line <- trimws(line)
  if (!nzchar(line)) return("")

  is_latent <- function(nm) trimws(nm) %in% latent_names

  if (section == "loading") {
    parts <- strsplit(line, "=~", fixed = TRUE)[[1]]
    if (length(parts) != 2) return(span_cls(line, "sem-loading"))
    lv <- trimws(parts[1])
    rhs <- trimws(parts[2])
    lv_html <- span_cls(lv, "sem-latent")
    op_html <- span_cls(" =~ ", "sem-loading")
    terms <- strsplit(rhs, "\\+")[[1]]
    term_htmls <- vapply(terms, function(t) {
      t <- trimws(t)
      if (grepl("\\*", t)) {
        sp <- strsplit(t, "*", fixed = TRUE)[[1]]
        label <- sp[1]
        varname <- paste(sp[-1], collapse = "*")
        var_cls <- if (is_latent(varname)) "sem-latent" else "sem-indicator"
        paste0(span_cls(paste0(label, "*"), "sem-constraint"),
               span_cls(varname, var_cls))
      } else {
        var_cls <- if (is_latent(t)) "sem-latent" else "sem-indicator"
        span_cls(t, var_cls)
      }
    }, character(1))
    paste0(lv_html, op_html, paste(term_htmls, collapse = paste0(span_cls("+", "sem-loading"))))
  } else if (section == "intercept" || section == "lat_mean") {
    parts <- strsplit(line, "~", fixed = TRUE)[[1]]
    if (length(parts) < 2) return(h_esc(line))
    lhs <- trimws(parts[1])
    rhs <- trimws(paste(parts[-1], collapse = "~"))
    cls_lhs <- if (is_latent(lhs)) "sem-latent" else "sem-indicator"
    lhs_html <- span_cls(lhs, cls_lhs)
    op_html <- span_cls(" ~ ", "sem-intercept")
    rhs_terms <- strsplit(rhs, "\\+")[[1]]
    rhs_htmls <- vapply(rhs_terms, function(t) {
      t <- trimws(t)
      if (grepl("\\*", t)) {
        sp <- strsplit(t, "*", fixed = TRUE)[[1]]
        paste0(span_cls(paste0(sp[1], "*"), "sem-constraint"),
               span_cls(sp[2], "sem-intercept"))
      } else {
        span_cls(t, "sem-intercept")
      }
    }, character(1))
    paste0(lhs_html, op_html, paste(rhs_htmls, collapse = paste0(span_cls("+", "sem-intercept"))))
  } else if (section == "resid_var") {
    parts <- strsplit(line, "~~", fixed = TRUE)[[1]]
    if (length(parts) != 2) return(h_esc(line))
    lhs <- trimws(parts[1])
    rhs <- trimws(parts[2])
    lhs_html <- span_cls(lhs, "sem-indicator")
    op_html <- span_cls(" ~~ ", "sem-resid-var")
    if (grepl("\\*", rhs)) {
      sp <- strsplit(rhs, "*", fixed = TRUE)[[1]]
      rhs_html <- paste0(span_cls(paste0(sp[1], "*"), "sem-constraint"),
                         span_cls(sp[2], "sem-indicator"))
    } else {
      rhs_html <- span_cls(rhs, "sem-indicator")
    }
    paste0(lhs_html, op_html, rhs_html)
  } else if (section == "resid_cov") {
    parts <- strsplit(line, "~~", fixed = TRUE)[[1]]
    if (length(parts) != 2) return(h_esc(line))
    lhs <- trimws(parts[1])
    rhs <- trimws(parts[2])
    paste0(span_cls(lhs, "sem-indicator"),
           span_cls(" ~~ ", "sem-resid-cov"),
           span_cls(rhs, "sem-indicator"))
  } else if (section == "lat_struct") {
    parts <- strsplit(line, "~~", fixed = TRUE)[[1]]
    if (length(parts) != 2) return(h_esc(line))
    lhs <- trimws(parts[1])
    rhs <- trimws(parts[2])
    rhs_terms <- strsplit(rhs, "\\+")[[1]]
    is_self <- any(vapply(rhs_terms, function(t) {
      t <- trimws(t)
      t <- sub("^[^*]*\\*", "", t)
      trimws(t) == lhs
    }, logical(1)))
    if (is_self) {
      lhs_html <- span_cls(lhs, "sem-latent")
      op_html <- span_cls(" ~~ ", "sem-lat-var")
      rhs_htmls <- vapply(rhs_terms, function(t) {
        t <- trimws(t)
        if (grepl("\\*", t)) {
          sp <- strsplit(t, "*", fixed = TRUE)[[1]]
          paste0(span_cls(paste0(sp[1], "*"), "sem-constraint"),
                 span_cls(sp[2], "sem-latent"))
        } else {
          span_cls(t, "sem-latent")
        }
      }, character(1))
      paste0(lhs_html, op_html, paste(rhs_htmls, collapse = span_cls(" + ", "sem-lat-var")))
    } else {
      paste0(span_cls(lhs, "sem-latent"),
             span_cls(" ~~ ", "sem-lat-cov"),
             span_cls(rhs, "sem-latent"))
    }
  } else {
    h_esc(line)
  }
}

extract_latent_names <- function(syntax) {
  lines <- strsplit(syntax, "\n", fixed = TRUE)[[1]]
  lvs <- character(0)
  for (ln in lines) {
    if (grepl("=~", ln, fixed = TRUE)) {
      lhs <- trimws(strsplit(ln, "=~", fixed = TRUE)[[1]][1])
      if (nzchar(lhs)) lvs <- c(lvs, lhs)
    }
  }
  unique(lvs)
}

color_syntax <- function(syntax, lvname, dvn) {
  lines <- strsplit(syntax, "\n", fixed = TRUE)[[1]]
  latent_names <- extract_latent_names(syntax)

  section <- "heading"
  html_lines <- character(length(lines))
  for (i in seq_along(lines)) {
    ln <- lines[i]
    sec <- detect_section(ln)
    if (!is.null(sec)) {
      section <- sec
      html_lines[i] <- paste0('<div class="sem-line sem-section">', h_esc(ln), '</div>')
    } else if (!nzchar(trimws(ln))) {
      html_lines[i] <- "<br/>"
    } else {
      colored <- color_syntax_line(ln, section, latent_names)
      html_lines[i] <- paste0('<div class="sem-line">', colored, '</div>')
    }
  }
  paste(html_lines, collapse = "\n")
}

# ── Understanding Panel: English Descriptions ──────────────────────────────

english_line <- function(line, section, lvname, dist1, dist2, latent_names, ind_to_lv = list()) {
  line <- trimws(line)
  if (!nzchar(line)) return("")

  lv_p1 <- paste0(lvname, dist1)
  lv_p2 <- paste0(lvname, dist2)
  lv_dy <- paste0(lvname, "Dy")
  partner_of <- function(lv) {
    if (lv == lv_p1) return(paste("partner", dist1))
    if (lv == lv_p2) return(paste("partner", dist2))
    if (lv == lv_dy) return("dyad (shared)")
    if (lv == lvname) return("higher-order dyad")
    lv
  }

  if (section == "loading") {
    parts <- strsplit(line, "=~", fixed = TRUE)[[1]]
    if (length(parts) != 2) return("")
    lv <- trimws(parts[1])
    rhs <- trimws(parts[2])
    terms <- strsplit(rhs, "\\+")[[1]]
    indicators <- c()
    labels <- c()
    for (t in terms) {
      t <- trimws(t)
      if (grepl("\\*", t)) {
        sp <- strsplit(t, "*", fixed = TRUE)[[1]]
        lab <- sp[1]
        varname <- sp[2]
        if (lab != "NA" && lab != "1") labels <- c(labels, lab)
        if (!varname %in% indicators) indicators <- c(indicators, varname)
      } else {
        if (!t %in% indicators) indicators <- c(indicators, t)
      }
    }
    rhs_are_latent <- all(indicators %in% latent_names)
    if (rhs_are_latent) {
      inds_html <- paste(vapply(indicators, function(v) span_cls(paste("Latent", lvname, "for", partner_of(v)), "sem-latent"), character(1)),
                         collapse = " and ")
      lv_html <- span_cls(paste("Latent", lvname, "for", partner_of(lv)), "sem-latent")
      verb_html <- span_cls("is reflected by", "sem-loading")
      suffix <- ""
      if (length(labels) > 0) {
        suffix <- paste0(" ", span_cls(paste0("(loadings labeled ", paste(labels, collapse = ", "), ")"), "sem-constraint"))
      }
      paste0(lv_html, " ", verb_html, " ", inds_html, suffix)
    } else {
      inds_html <- paste(vapply(indicators, function(v) span_cls(v, "sem-indicator"), character(1)),
                         collapse = ", ")
      lv_html <- span_cls(paste("Latent", lvname, "for", partner_of(lv)), "sem-latent")
      verb_html <- span_cls("is reflected by", "sem-loading")
      suffix <- ""
      if (length(labels) > 0) {
        suffix <- paste0(" ", span_cls(paste0("(loadings labeled ", paste(labels, collapse = ", "), ")"), "sem-constraint"))
      }
      paste0(lv_html, " ", verb_html, " observed ", inds_html, suffix)
    }

  } else if (section == "intercept") {
    parts <- strsplit(line, "~", fixed = TRUE)[[1]]
    if (length(parts) < 2) return("")
    lhs <- trimws(parts[1])
    rhs <- trimws(paste(parts[-1], collapse = "~"))
    label <- ""
    if (grepl("\\*", rhs)) {
      sp <- strsplit(rhs, "*", fixed = TRUE)[[1]]
      label <- sp[1]
    }
    suffix <- if (nzchar(label) && label != "1") {
      paste0(" ", span_cls(paste0("(labeled ", label, ")"), "sem-constraint"))
    } else ""
    lvs_for_ind <- ind_to_lv[[lhs]]
    if (!is.null(lvs_for_ind) && length(lvs_for_ind) > 0) {
      lv_desc <- paste(vapply(lvs_for_ind, function(lv) {
        span_cls(paste("Latent", lvname, "for", partner_of(lv)), "sem-latent")
      }, character(1)), collapse = " and ")
      is_are <- if (length(lvs_for_ind) > 1) "are" else "is"
    } else {
      lv_desc <- span_cls("its latent variable(s)", "sem-latent")
      is_are <- "is"
    }
    paste0(span_cls(lhs, "sem-indicator"), "'s ",
           span_cls("expected value", "sem-intercept"),
           " when ", lv_desc, " ", is_are, " zero", suffix)

  } else if (section == "resid_var") {
    parts <- strsplit(line, "~~", fixed = TRUE)[[1]]
    if (length(parts) != 2) return("")
    lhs <- trimws(parts[1])
    rhs <- trimws(parts[2])
    label <- ""
    if (grepl("\\*", rhs)) {
      sp <- strsplit(rhs, "*", fixed = TRUE)[[1]]
      label <- sp[1]
    }
    suffix <- if (nzchar(label)) {
      paste0(" ", span_cls(paste0("(labeled ", label, ")"), "sem-constraint"))
    } else ""
    lvs_for_ind <- ind_to_lv[[lhs]]
    if (!is.null(lvs_for_ind) && length(lvs_for_ind) > 0) {
      lv_desc <- paste(vapply(lvs_for_ind, function(lv) {
        span_cls(paste("Latent", lvname, "for", partner_of(lv)), "sem-latent")
      }, character(1)), collapse = " and ")
    } else {
      lv_desc <- span_cls("its latent variable(s)", "sem-latent")
    }
    paste0(span_cls(lhs, "sem-indicator"), "'s ",
           span_cls("residual variance", "sem-resid-var"),
           ", not shared with ", lv_desc, suffix)

  } else if (section == "resid_cov") {
    parts <- strsplit(line, "~~", fixed = TRUE)[[1]]
    if (length(parts) != 2) return("")
    lhs <- trimws(parts[1])
    rhs <- trimws(parts[2])
    paste0("The residual contents of ", span_cls(lhs, "sem-indicator"),
           span_cls(" covaries with ", "sem-resid-cov"),
           "the residual contents of ", span_cls(rhs, "sem-indicator"))

  } else if (section == "lat_struct") {
    parts <- strsplit(line, "~~", fixed = TRUE)[[1]]
    if (length(parts) != 2) return("")
    lhs <- trimws(parts[1])
    rhs <- trimws(parts[2])
    rhs_terms <- strsplit(rhs, "\\+")[[1]]
    rhs_clean <- sub("^[^*]*\\*", "", trimws(rhs_terms[1]))
    if (trimws(rhs_clean) == lhs || any(grepl(paste0("\\*", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", lhs), "$"), rhs_terms))) {
      labels <- c()
      for (t in rhs_terms) {
        t <- trimws(t)
        if (grepl("\\*", t)) {
          sp <- strsplit(t, "*", fixed = TRUE)[[1]]
          if (sp[1] != "1" && sp[1] != "NA") labels <- c(labels, sp[1])
        }
      }
      suffix <- if (length(labels) > 0) {
        paste0(" ", span_cls(paste0("(labeled ", paste(labels, collapse = ", "), ")"), "sem-constraint"))
      } else ""
      paste0(span_cls(paste0("Latent ", lvname, " for ", partner_of(lhs), "'s"), "sem-latent"), " ",
             span_cls("variance", "sem-lat-var"), suffix)
    } else {
      rhs_stripped <- sub("^[^*]*\\*", "", trimws(rhs))
      paste0(span_cls(paste("Latent", lvname, "for", partner_of(lhs)), "sem-latent"), " ",
             span_cls("covaries with", "sem-lat-cov"), " ",
             span_cls(paste("Latent", lvname, "for", partner_of(rhs_stripped)), "sem-latent"))
    }

  } else if (section == "lat_mean") {
    parts <- strsplit(line, "~", fixed = TRUE)[[1]]
    if (length(parts) < 2) return("")
    lhs <- trimws(parts[1])
    labels <- c()
    rhs <- trimws(paste(parts[-1], collapse = "~"))
    for (t in strsplit(rhs, "\\+")[[1]]) {
      t <- trimws(t)
      if (grepl("\\*", t)) {
        sp <- strsplit(t, "*", fixed = TRUE)[[1]]
        if (sp[1] != "0") labels <- c(labels, sp[1])
      }
    }
    suffix <- if (length(labels) > 0) {
      paste0(" ", span_cls(paste0("(labeled ", paste(labels, collapse = ", "), ")"), "sem-constraint"))
    } else ""
    paste0(span_cls(paste0("Latent ", lvname, " for ", partner_of(lhs), "'s"), "sem-latent"), " ",
           span_cls("mean", "sem-intercept"), suffix)

  } else {
    ""
  }
}

build_ind_to_lv <- function(syntax) {
  lines <- strsplit(syntax, "\n", fixed = TRUE)[[1]]
  mapping <- list()
  in_loadings <- FALSE
  for (ln in lines) {
    sec <- detect_section(ln)
    if (!is.null(sec)) { in_loadings <- (sec == "loading"); next }
    if (!in_loadings) next
    ln <- trimws(ln)
    if (!nzchar(ln) || !grepl("=~", ln, fixed = TRUE)) next
    parts <- strsplit(ln, "=~", fixed = TRUE)[[1]]
    lv <- trimws(parts[1])
    rhs <- trimws(parts[2])
    for (t in strsplit(rhs, "\\+")[[1]]) {
      t <- trimws(t)
      varname <- sub("^[^*]*\\*", "", t)
      if (nzchar(varname)) {
        mapping[[varname]] <- unique(c(mapping[[varname]], lv))
      }
    }
  }
  mapping
}

color_english <- function(syntax, dvn, lvname) {
  lines <- strsplit(syntax, "\n", fixed = TRUE)[[1]]
  dist1 <- dvn$dist1
  dist2 <- dvn$dist2
  latent_names <- extract_latent_names(syntax)
  ind_to_lv <- build_ind_to_lv(syntax)

  section <- "heading"
  html_lines <- character(length(lines))
  for (i in seq_along(lines)) {
    ln <- lines[i]
    sec <- detect_section(ln)
    if (!is.null(sec)) {
      section <- sec
      html_lines[i] <- paste0('<div class="sem-line sem-section">', h_esc(ln), '</div>')
    } else if (!nzchar(trimws(ln))) {
      html_lines[i] <- "<br/>"
    } else {
      eng <- english_line(ln, section, lvname, dist1, dist2, latent_names, ind_to_lv)
      if (nzchar(eng)) {
        html_lines[i] <- paste0('<div class="sem-line">', eng, '</div>')
      } else {
        html_lines[i] <- ""
      }
    }
  }
  paste(html_lines, collapse = "\n")
}

# ── Understanding Panel: Color-Coded Diagram ──────────────────────────────

color_diagram <- function(fit, show_intercepts = FALSE) {
  g <- semPlot::semPaths(fit,
    whatLabels = "names", layout = "tree2",
    intercepts = show_intercepts, DoNotPlot = TRUE,
    edge.label.cex = 0.6, curvePivot = FALSE,
    weighted = FALSE, nCharNodes = 0,
    sizeMan = 4, sizeLat = 7,
    mar = c(8, 6, 8, 6))

  node_shapes <- g$graphAttributes$Nodes$shape
  n_nodes <- length(node_shapes)
  node_colors <- rep("white", n_nodes)
  node_borders <- rep("black", n_nodes)
  for (i in seq_len(n_nodes)) {
    sh <- node_shapes[i]
    if (identical(sh, "circle")) {
      node_colors[i] <- SEM_COLORS$latent_fill
      node_borders[i] <- SEM_COLORS$latent
    } else if (identical(sh, "triangle")) {
      node_colors[i] <- "#E8F4FC"
      node_borders[i] <- SEM_COLORS$intercept
    } else {
      node_colors[i] <- SEM_COLORS$ind_fill
      node_borders[i] <- SEM_COLORS$indicator
    }
  }
  g$graphAttributes$Nodes$color <- node_colors
  g$graphAttributes$Nodes$border.color <- node_borders

  n_edges <- length(g$Edgelist$from)
  edge_colors <- rep("#AAAAAA", n_edges)
  edge_lty <- g$graphAttributes$Edges$lty
  if (is.null(edge_lty)) edge_lty <- rep(1, n_edges)

  for (i in seq_len(n_edges)) {
    fi <- g$Edgelist$from[i]
    ti <- g$Edgelist$to[i]
    bidir <- isTRUE(g$Edgelist$bidirectional[i])
    from_lat <- (node_shapes[fi] == "circle")
    to_lat <- (node_shapes[ti] == "circle")

    tri_f <- identical(node_shapes[fi], "triangle")
    tri_t <- identical(node_shapes[ti], "triangle")

    if (!bidir && show_intercepts && (tri_f || tri_t)) {
      edge_colors[i] <- SEM_COLORS$intercept
    } else if (!bidir && from_lat && !to_lat) {
      edge_colors[i] <- SEM_COLORS$loading
    } else if (!bidir && from_lat && to_lat) {
      edge_colors[i] <- SEM_COLORS$loading
    } else if (bidir && fi == ti && from_lat) {
      edge_colors[i] <- SEM_COLORS$lat_var
    } else if (bidir && fi == ti && !from_lat) {
      edge_colors[i] <- SEM_COLORS$resid_var
    } else if (bidir && fi != ti && from_lat && to_lat) {
      edge_colors[i] <- SEM_COLORS$lat_cov
      edge_lty[i] <- 2
    } else if (bidir && fi != ti && !from_lat && !to_lat) {
      edge_colors[i] <- SEM_COLORS$resid_cov
      edge_lty[i] <- 2
    }
  }
  g$graphAttributes$Edges$color <- edge_colors
  g$graphAttributes$Edges$lty <- edge_lty
  g
}

make_color_legend <- function() {
  items <- list(
    list(color = SEM_COLORS$latent,    label = "Latent Variable"),
    list(color = SEM_COLORS$loading,   label = "Loading"),
    list(color = SEM_COLORS$indicator, label = "Indicator"),
    list(color = SEM_COLORS$lat_var,   label = "Latent Variance"),
    list(color = SEM_COLORS$lat_cov,   label = "Latent Covariance"),
    list(color = SEM_COLORS$resid_var, label = "Residual Variance"),
    list(color = SEM_COLORS$resid_cov, label = "Residual Covariance"),
    list(color = SEM_COLORS$intercept, label = "Intercept/Mean"),
    list(color = SEM_COLORS$constraint,label = "Constraint Label")
  )
  chips <- lapply(items, function(it) {
    tags$span(class = "color-legend-item",
      tags$span(class = "color-chip", style = paste0("background:", it$color, ";")),
      it$label)
  })
  tags$div(class = "color-legend", chips)
}


# ── UI ──────────────────────────────────────────────────────────────────────

ui <- page_sidebar(
  title = div(
    style = "display: flex; align-items: center; gap: 12px; flex-wrap: wrap;",
    tags$img(
      src = "dysem-logo.png",
      height = "48",
      width = "auto",
      alt = "dySEM — dyadic structural equation modeling for R",
      style = "flex-shrink: 0; height: 48px; width: auto;"
    ),
    div(
      style = "display: flex; flex-direction: column; gap: 2px; min-width: 0;",
      span("The dySEM App", style = "font-weight: 700;"),
      span("A tool to learn dyadic SEM and use dySEM by Sakaluk and Camanto (2026)",
           style = "font-size: 0.85em; opacity: 0.7;")
    )
  ),
  theme = bs_theme(
    bootswatch = "flatly",
    "sidebar-bg" = "#f8f9fa",
    base_font = font_google("Source Sans Pro"),
    code_font = font_google("Fira Code")
  ),
  useShinyjs(),
  tags$head(
    tags$link(rel = "icon", href = "dysem-logo.png", type = "image/png"),
    tags$style(HTML(UNDERSTANDING_CSS)),
    tags$script(HTML(SCROLL_SYNC_JS)),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('copy-to-clipboard', function(text) {
        navigator.clipboard.writeText(text).then(function() {
          Shiny.setInputValue('clipboard_success', Math.random());
        });
      });
    "))
  ),

  sidebar = sidebar(
    width = 380,
    div(
      class = "dysem-sidebar-brand",
      style = "padding-bottom: 12px; margin-bottom: 10px; border-bottom: 1px solid rgba(0,0,0,0.08);",
      div(
        style = "display: flex; align-items: center; gap: 10px;",
        tags$img(
          src = "dysem-logo.png",
          height = "40",
          width = "auto",
          alt = "",
          `aria-hidden` = "true",
          style = "height: 40px; width: auto; flex-shrink: 0;"
        ),
        tags$p(
          style = "margin: 0; font-size: 0.86em; line-height: 1.4;",
          "Part of the ",
          tags$a(
            href = "https://jsakaluk.github.io/dySEM/",
            target = "_blank",
            rel = "noopener noreferrer",
            "dySEM"
          ),
          " R package (documentation, tutorials, and citation)."
        )
      )
    ),
    accordion(
      id = "sidebar_acc",
      open = "step1",
      multiple = TRUE,

      # ── Step 1: Model Category ──
      accordion_panel(
        "1. Model Category",
        value = "step1",
        radioButtons(
          "model_category", NULL,
          choiceNames = list(
            tags$span("Uni-construct", tags$br(),
                      tags$small("1 measure, 2 partners", style = "opacity:0.6")),
            tags$span("Bi-construct", tags$br(),
                      tags$small("2 measures, 2 partners", style = "opacity:0.6")),
            tags$span("Multi-construct", tags$br(),
                      tags$small("3+ measures, 2 partners", style = "opacity:0.6"))
          ),
          choiceValues = c("uni", "bi", "multi"),
          selected = "uni"
        )
      ),

      # ── Step 2: Model Type ──
      accordion_panel(
        "2. Model Type",
        value = "step2",
        uiOutput("model_type_ui"),
        tags$hr(),
        uiOutput("model_description_ui")
      ),

      # ── Step 3: Model Options ──
      accordion_panel(
        "3. Model Options",
        value = "step3",
        uiOutput("model_options_ui")
      ),

      # ── Step 4: Variable Naming ──
      accordion_panel(
        "4. Variable Naming",
        value = "step4",
        selectInput("naming_order", "Naming Order",
                    choices = NAMING_ORDER_LABELS, selected = "spi"),

        fluidRow(
          column(6, textInput("dist1", "Partner 1 ID", value = "1")),
          column(6, textInput("dist2", "Partner 2 ID", value = "2"))
        ),

        uiOutput("naming_inputs_ui")
      ),

      # ── Step 5: Generate ──
      accordion_panel(
        "5. Generate",
        value = "step5",
        actionButton("generate", "Generate Script & Preview",
                     class = "btn-primary w-100",
                     style = "font-size: 1.1em; padding: 10px;"),
        tags$br(), tags$br(),
        tags$small("Generates lavaan syntax, path diagram, and reproducible R code.",
                   style = "opacity: 0.6;")
      )
    )
  ),

  # ── Main Content Area ──
  navset_card_tab(
    id = "output_tabs",

    nav_panel(
      "Variable Preview",
      icon = icon("table"),
      card_body(
        tags$p("Variable names update as you modify the naming inputs.",
               style = "opacity: 0.6; font-style: italic;"),
        tableOutput("var_preview_table")
      )
    ),

    nav_panel(
      "Path Diagram",
      icon = icon("project-diagram"),
      card_body(
        uiOutput("diagram_placeholder"),
        plotOutput("path_diagram", height = "700px")
      )
    ),

    nav_panel(
      "Lavaan Syntax",
      icon = icon("code"),
      card_body(
        uiOutput("syntax_placeholder"),
        div(
          style = "display: flex; gap: 8px; margin-bottom: 10px;",
          actionButton("copy_syntax", "Copy to Clipboard", icon = icon("clipboard"),
                       class = "btn-outline-primary btn-sm"),
          downloadButton("download_syntax", "Download .txt",
                         class = "btn-outline-secondary btn-sm")
        ),
        verbatimTextOutput("syntax_output")
      )
    ),

    nav_panel(
      "dySEM Workflow",
      icon = icon("terminal"),
      card_body(
        uiOutput("rcode_placeholder"),
        div(
          style = "margin-bottom: 10px;",
          actionButton("copy_rcode", "Copy to Clipboard", icon = icon("clipboard"),
                       class = "btn-outline-primary btn-sm")
        ),
        verbatimTextOutput("rcode_output")
      )
    ),

    nav_panel(
      "Learning",
      icon = icon("lightbulb"),
      card_body(
        uiOutput("understand_placeholder"),
        uiOutput("understand_legend"),
        accordion(
          id = "understand_acc",
          open = c("u_diagram", "u_syntax", "u_english"),
          multiple = TRUE,
          accordion_panel(
            "Path Diagram",
            value = "u_diagram",
            icon = icon("project-diagram"),
            plotOutput("understand_diagram", height = "700px")
          ),
          accordion_panel(
            "Lavaan Syntax",
            value = "u_syntax",
            icon = icon("code"),
            div(id = "understand_syntax_panel", class = "understand-panel",
                htmlOutput("understand_syntax"))
          ),
          accordion_panel(
            "English Description",
            value = "u_english",
            icon = icon("book-open"),
            div(id = "understand_english_panel", class = "understand-english",
                htmlOutput("understand_english"))
          )
        )
      )
    )
  )
)


# ── Server ──────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  generated <- reactiveValues(
    syntax = NULL,
    rcode = NULL,
    dvn = NULL,
    fit_fn = NULL
  )

  # ── Step 2: Model type selector (updates on category change) ──
  output$model_type_ui <- renderUI({
    choices <- MODEL_TYPES[[input$model_category]]
    selectInput("model_type", "Select Model", choices = choices)
  })

  output$model_description_ui <- renderUI({
    req(input$model_type)
    desc <- MODEL_DESCRIPTIONS[[input$model_type]]
    if (!is.null(desc)) {
      tags$div(
        style = "background: #eef6fb; border-radius: 6px; padding: 10px; font-size: 0.88em;",
        icon("info-circle", style = "color: #3498db;"),
        desc
      )
    }
  })

  # ── Step 3: Model options (updates on model type change) ──
  output$model_options_ui <- renderUI({
    req(input$model_type)
    model <- input$model_type
    category <- input$model_category

    ui_elements <- list()

    # Scale setting
    if (model == "scriptTwoCross") {
      ui_elements <- c(ui_elements, list(
        fluidRow(
          column(6, radioButtons("x_scaleset", "X Scale Setting",
                                 choices = c("Fixed Factor" = "FF", "Marker Variable" = "MV"),
                                 inline = TRUE)),
          column(6, radioButtons("y_scaleset", "Y Scale Setting",
                                 choices = c("Fixed Factor" = "FF", "Marker Variable" = "MV"),
                                 inline = TRUE))
        )
      ))
    } else {
      ui_elements <- c(ui_elements, list(
        radioButtons("scaleset", "Scale Setting",
                     choices = c("Fixed Factor" = "FF", "Marker Variable" = "MV"),
                     inline = TRUE)
      ))
    }

    # TwoCross: measurement model types for X and Y
    if (model == "scriptTwoCross") {
      ui_elements <- c(ui_elements, list(
        tags$hr(),
        tags$strong("Measurement Model Forms"),
        fluidRow(
          column(6, selectInput("x_meas_model", "X Model",
                                choices = c("Correlated" = "cor", "Unidimensional" = "uni",
                                            "Hierarchical" = "hier", "Bifactor" = "bifactor"))),
          column(6, selectInput("y_meas_model", "Y Model",
                                choices = c("Correlated" = "cor", "Unidimensional" = "uni",
                                            "Hierarchical" = "hier", "Bifactor" = "bifactor")))
        )
      ))
    }

    # Invariance presets
    ui_elements <- c(ui_elements, list(tags$hr()))

    if (category == "bi") {
      ui_elements <- c(ui_elements, list(
        tags$strong("Invariance Constraints"),
        fluidRow(
          column(6, selectInput("invariance_x", "X Invariance",
                                choices = names(INVARIANCE_PRESETS), selected = "Indistinguishable")),
          column(6, selectInput("invariance_y", "Y Invariance",
                                choices = names(INVARIANCE_PRESETS), selected = "Indistinguishable"))
        ),
        conditionalPanel(
          "input.invariance_x == 'Custom'",
          tags$small("X Measurement:"),
          checkboxGroupInput("custom_x_meas", NULL,
                             choices = c("Loadings" = "loadings", "Intercepts" = "intercepts",
                                         "Residuals" = "residuals"),
                             inline = TRUE),
          tags$small("X Structural:"),
          checkboxGroupInput("custom_x_struct", NULL,
                             choices = c("Variances" = "variances", "Means" = "means"),
                             inline = TRUE)
        ),
        conditionalPanel(
          "input.invariance_y == 'Custom'",
          tags$small("Y Measurement:"),
          checkboxGroupInput("custom_y_meas", NULL,
                             choices = c("Loadings" = "loadings", "Intercepts" = "intercepts",
                                         "Residuals" = "residuals"),
                             inline = TRUE),
          tags$small("Y Structural:"),
          checkboxGroupInput("custom_y_struct", NULL,
                             choices = c("Variances" = "variances", "Means" = "means"),
                             inline = TRUE)
        )
      ))
    } else {
      ui_elements <- c(ui_elements, list(
        selectInput("invariance_preset", "Invariance Level",
                    choices = names(INVARIANCE_PRESETS), selected = "Indistinguishable"),
        conditionalPanel(
          "input.invariance_preset == 'Custom'",
          tags$small("Measurement Constraints:"),
          checkboxGroupInput("custom_meas", NULL,
                             choices = c("Loadings" = "loadings", "Intercepts" = "intercepts",
                                         "Residuals" = "residuals"),
                             inline = TRUE),
          tags$small("Structural Constraints:"),
          checkboxGroupInput("custom_struct", NULL,
                             choices = c("Variances" = "variances", "Means" = "means"),
                             inline = TRUE)
        )
      ))
    }

    # Path diagram (semPaths)
    cb_intercepts <- isolate({
      v <- input$diagram_show_intercepts
      if (is.null(v)) TRUE else isTRUE(v)
    })
    ui_elements <- c(ui_elements, list(
      tags$hr(),
      tags$strong("Path diagram display"),
      checkboxInput(
        "diagram_show_intercepts",
        "Show intercepts & latent means on path diagrams",
        value = cb_intercepts
      ),
      tags$small(
        "Uses semPaths(intercepts = TRUE). Only appears when the generated syntax includes mean structure (e.g. measurement intercepts and/or latent means from invariance settings, or mean-structure options for APIM / Two-Construct).",
        style = "opacity: 0.65; display: block;"
      )
    ))

    # XY structural constraints (bi-construct only)
    if (category == "bi" && model %in% names(XY_STRUCT_OPTIONS)) {
      opts <- XY_STRUCT_OPTIONS[[model]]
      if (model == "scriptCFM") {
        ui_elements <- c(ui_elements, list(
          tags$hr(),
          checkboxGroupInput("xy_struct", "X-Y Structural Constraints",
                             choices = opts, selected = "none")
        ))
      } else {
        ui_elements <- c(ui_elements, list(
          tags$hr(),
          selectInput("xy_struct", "X-Y Structural Constraints",
                      choices = opts,
                      selected = opts[1])
        ))
      }
    }

    # APIM-specific
    if (model == "scriptAPIM") {
      ui_elements <- c(ui_elements, list(
        tags$hr(),
        checkboxInput("est_k", "Estimate Kenny's k parameter", value = FALSE),
        checkboxInput("include_mean_struct", "Include mean structure", value = FALSE)
      ))
    }

    # TwoCross mean structure
    if (model == "scriptTwoCross") {
      ui_elements <- c(ui_elements, list(
        tags$hr(),
        checkboxInput("include_mean_struct", "Include mean structure", value = FALSE)
      ))
    }

    do.call(tagList, ui_elements)
  })

  # ── Step 4: Variable naming inputs (updates on category change) ──
  output$naming_inputs_ui <- renderUI({
    category <- input$model_category

    if (category == "uni") {
      tagList(
        tags$strong("Construct"),
        textInput("lvname_x", "Latent Variable Name", value = "Sat"),
        textInput("stem_x", "Stem", value = "sat.g", placeholder = "e.g., sat.g"),
        fluidRow(
          column(6, textInput("delim1_x", "Delimiter 1", value = ".")),
          column(6, textInput("delim2_x", "Delimiter 2", value = "_"))
        ),
        numericInput("n_items_x", "Number of Items", value = 5, min = 1, max = 50)
      )

    } else if (category == "bi") {
      tagList(
        tags$strong("X Construct"),
        textInput("lvname_x", "Latent X Name", value = "Sat"),
        textInput("stem_x", "X Stem", value = "sat.g", placeholder = "e.g., sat.g"),
        fluidRow(
          column(6, textInput("delim1_x", "X Delimiter 1", value = ".")),
          column(6, textInput("delim2_x", "X Delimiter 2", value = "_"))
        ),
        numericInput("n_items_x", "X Number of Items", value = 5, min = 1, max = 50),

        tags$hr(),
        tags$strong("Y Construct"),
        textInput("lvname_y", "Latent Y Name", value = "Com"),
        textInput("stem_y", "Y Stem", value = "com", placeholder = "e.g., com"),
        fluidRow(
          column(6, textInput("delim1_y", "Y Delimiter 1", value = ".")),
          column(6, textInput("delim2_y", "Y Delimiter 2", value = "_"))
        ),
        numericInput("n_items_y", "Y Number of Items", value = 5, min = 1, max = 50)
      )

    } else if (category == "multi") {
      max_constructs <- 6
      construct_uis <- lapply(seq_len(max_constructs), function(i) {
        vis <- if (i <= 3) "" else "display: none;"
        div(
          id = paste0("mc_row_", i),
          style = vis,
          tags$strong(paste0("Construct ", i)),
          fluidRow(
            column(5, textInput(paste0("mc_lvname_", i), "LV Name",
                                value = if (i <= 3) c("Sat", "Invest", "Comm")[i] else "")),
            column(7, textInput(paste0("mc_stem_", i), "Stem",
                                value = if (i <= 3) c("sat.g", "invest.g", "com")[i] else ""))
          ),
          fluidRow(
            column(4, textInput(paste0("mc_delim1_", i), "Delim1",
                                value = if (i <= 3) "." else "")),
            column(4, textInput(paste0("mc_delim2_", i), "Delim2",
                                value = if (i <= 3) "_" else "")),
            column(4, numericInput(paste0("mc_nitems_", i), "Items",
                                   value = if (i <= 3) 5 else 3, min = 1, max = 50))
          ),
          if (i < max_constructs) tags$hr(style = "margin: 5px 0;")
        )
      })

      tagList(
        construct_uis,
        fluidRow(
          column(6, actionButton("add_construct", "+ Add", class = "btn-outline-success btn-sm w-100")),
          column(6, actionButton("remove_construct", "- Remove", class = "btn-outline-danger btn-sm w-100"))
        )
      )
    }
  })

  # Multi-construct: show/hide rows
  n_mc <- reactiveVal(3)

  observeEvent(input$add_construct, {
    new_n <- min(n_mc() + 1, 6)
    n_mc(new_n)
    shinyjs::show(paste0("mc_row_", new_n))
  })

  observeEvent(input$remove_construct, {
    if (n_mc() > 2) {
      shinyjs::hide(paste0("mc_row_", n_mc()))
      n_mc(n_mc() - 1)
    }
  })

  # ── Reactive: Build dvn from current inputs ──
  current_dvn <- reactive({
    category <- input$model_category
    req(input$naming_order, input$dist1, input$dist2)

    if (category == "uni") {
      req(input$stem_x, input$n_items_x)
      build_dvn_from_inputs(
        category = "uni",
        order = input$naming_order,
        stem_x = input$stem_x,
        delim1_x = input$delim1_x %||% "",
        delim2_x = input$delim2_x %||% "",
        n_items_x = input$n_items_x,
        dist1 = input$dist1,
        dist2 = input$dist2
      )
    } else if (category == "bi") {
      req(input$stem_x, input$n_items_x, input$stem_y, input$n_items_y)
      build_dvn_from_inputs(
        category = "bi",
        order = input$naming_order,
        stem_x = input$stem_x,
        delim1_x = input$delim1_x %||% "",
        delim2_x = input$delim2_x %||% "",
        n_items_x = input$n_items_x,
        dist1 = input$dist1,
        dist2 = input$dist2,
        stem_y = input$stem_y,
        delim1_y = input$delim1_y,
        delim2_y = input$delim2_y,
        n_items_y = input$n_items_y
      )
    } else if (category == "multi") {
      n <- n_mc()
      lvnames <- character(n)
      stems <- character(n)
      d1s <- character(n)
      d2s <- character(n)
      nitems <- integer(n)

      for (i in seq_len(n)) {
        lv <- input[[paste0("mc_lvname_", i)]]
        st <- input[[paste0("mc_stem_", i)]]
        if (is.null(lv) || is.null(st) || !nzchar(lv) || !nzchar(st)) return(NULL)
        lvnames[i] <- lv
        stems[i] <- st
        d1s[i] <- input[[paste0("mc_delim1_", i)]] %||% ""
        d2s[i] <- input[[paste0("mc_delim2_", i)]] %||% ""
        nitems[i] <- input[[paste0("mc_nitems_", i)]] %||% 3
      }

      build_dvn_from_inputs(
        category = "multi",
        order = input$naming_order,
        stem_x = stems[1],
        delim1_x = d1s[1],
        delim2_x = d2s[1],
        n_items_x = nitems[1],
        dist1 = input$dist1,
        dist2 = input$dist2,
        mc_info = list(lvnames = lvnames, stems = stems,
                       delim1s = d1s, delim2s = d2s, n_items = nitems)
      )
    }
  })

  # ── Tab 1: Variable Name Preview (live) ──
  output$var_preview_table <- renderTable({
    dvn <- current_dvn()
    req(dvn)

    if (is.list(dvn$p1xvarnames) && !is.null(names(dvn$p1xvarnames))) {
      rows <- list()
      for (nm in names(dvn$p1xvarnames)) {
        for (j in seq_along(dvn$p1xvarnames[[nm]])) {
          rows[[length(rows) + 1]] <- data.frame(
            Construct = nm,
            Item = j,
            Partner_1 = dvn$p1xvarnames[[nm]][j],
            Partner_2 = dvn$p2xvarnames[[nm]][j],
            stringsAsFactors = FALSE
          )
        }
      }
      do.call(rbind, rows)
    } else {
      p1_names <- dvn$p1xvarnames
      p2_names <- dvn$p2xvarnames
      df <- data.frame(
        Construct = "X",
        Item = seq_along(p1_names),
        Partner_1 = p1_names,
        Partner_2 = p2_names,
        stringsAsFactors = FALSE
      )
      if (!is.null(dvn$p1yvarnames)) {
        df_y <- data.frame(
          Construct = "Y",
          Item = seq_along(dvn$p1yvarnames),
          Partner_1 = dvn$p1yvarnames,
          Partner_2 = dvn$p2yvarnames,
          stringsAsFactors = FALSE
        )
        df <- rbind(df, df_y)
      }
      df
    }
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # ── Placeholders for ungenerated tabs ──
  output$diagram_placeholder <- renderUI({
    if (is.null(generated$syntax)) {
      tags$div(
        style = "text-align:center; padding: 60px; opacity: 0.5;",
        icon("project-diagram", style = "font-size: 3em;"),
        tags$p("Click 'Generate Script & Preview' to see the path diagram.")
      )
    }
  })

  output$syntax_placeholder <- renderUI({
    if (is.null(generated$syntax)) {
      tags$div(
        style = "text-align:center; padding: 60px; opacity: 0.5;",
        icon("code", style = "font-size: 3em;"),
        tags$p("Click 'Generate Script & Preview' to see the lavaan syntax.")
      )
    }
  })

  output$rcode_placeholder <- renderUI({
    if (is.null(generated$rcode)) {
      tags$div(
        style = "text-align:center; padding: 60px; opacity: 0.5;",
        icon("terminal", style = "font-size: 3em;"),
        tags$p("Click 'Generate Script & Preview' to see the reproducible R code.")
      )
    }
  })

  # ── Generate Script ──
  observeEvent(input$generate, {
    dvn <- current_dvn()
    req(dvn)

    category <- input$model_category
    model <- input$model_type

    tryCatch({
      # Resolve constraints
      if (category == "bi") {
        preset_x <- input$invariance_x %||% "Indistinguishable"
        preset_y <- input$invariance_y %||% "Indistinguishable"

        if (preset_x == "Custom") {
          x_meas <- if (length(input$custom_x_meas) > 0) input$custom_x_meas else "none"
          x_struct <- if (length(input$custom_x_struct) > 0) input$custom_x_struct else "none"
        } else {
          p <- INVARIANCE_PRESETS[[preset_x]]
          x_meas <- p$meas
          x_struct <- p$struct
        }

        if (preset_y == "Custom") {
          y_meas <- if (length(input$custom_y_meas) > 0) input$custom_y_meas else "none"
          y_struct <- if (length(input$custom_y_struct) > 0) input$custom_y_struct else "none"
        } else {
          p <- INVARIANCE_PRESETS[[preset_y]]
          y_meas <- p$meas
          y_struct <- p$struct
        }

        xy_struct_raw <- input$xy_struct %||% "none"
        xy_struct <- parse_constraint_string(xy_struct_raw)

      } else {
        preset <- input$invariance_preset %||% "Indistinguishable"
        if (preset == "Custom") {
          meas <- if (length(input$custom_meas) > 0) input$custom_meas else "none"
          struct <- if (length(input$custom_struct) > 0) input$custom_struct else "none"
        } else {
          p <- INVARIANCE_PRESETS[[preset]]
          meas <- p$meas
          struct <- p$struct
        }
      }

      lvxname <- input$lvname_x %||% "X"
      lvyname <- input$lvname_y %||% "Y"
      ss <- input$scaleset %||% "FF"

      # Call the appropriate scripter
      syntax <- switch(model,
        scriptUni = scriptUni(dvn, scaleset = ss, lvname = lvxname,
                              constr_dy_meas = meas, constr_dy_struct = struct),
        scriptCor = scriptCor(dvn, scaleset = ss, lvname = lvxname,
                              constr_dy_meas = meas, constr_dy_struct = struct),
        scriptHier = scriptHier(dvn, scaleset = ss, lvname = lvxname,
                                constr_dy_meas = meas, constr_dy_struct = struct),
        scriptBifac = scriptBifac(dvn, scaleset = ss, lvname = lvxname,
                                  constr_dy_meas = meas, constr_dy_struct = struct),
        scriptAPIM = scriptAPIM(dvn, scaleset = ss,
                                lvxname = lvxname, lvyname = lvyname,
                                constr_dy_x_meas = x_meas, constr_dy_x_struct = x_struct,
                                constr_dy_y_meas = y_meas, constr_dy_y_struct = y_struct,
                                constr_dy_xy_struct = xy_struct,
                                est_k = input$est_k %||% FALSE,
                                includeMeanStruct = input$include_mean_struct %||% FALSE),
        scriptCFM = scriptCFM(dvn, scaleset = ss,
                              lvxname = lvxname, lvyname = lvyname,
                              constr_dy_x_meas = x_meas, constr_dy_x_struct = x_struct,
                              constr_dy_y_meas = y_meas, constr_dy_y_struct = y_struct,
                              constr_dy_xy_struct = xy_struct),
        scriptMIM = scriptMIM(dvn, scaleset = ss,
                              lvxname = lvxname, lvyname = lvyname,
                              constr_dy_x_meas = x_meas, constr_dy_x_struct = x_struct,
                              constr_dy_y_meas = y_meas, constr_dy_y_struct = y_struct,
                              constr_dy_xy_struct = xy_struct),
        scriptTwoCross = scriptTwoCross(
                              dvn,
                              x_scaleset = input$x_scaleset %||% "FF",
                              y_scaleset = input$y_scaleset %||% "FF",
                              lvxname = lvxname, lvyname = lvyname,
                              x_model = input$x_meas_model %||% "cor",
                              y_model = input$y_meas_model %||% "cor",
                              constr_dy_x_meas = x_meas, constr_dy_x_struct = x_struct,
                              constr_dy_y_meas = y_meas, constr_dy_y_struct = y_struct,
                              constr_dy_xy_struct = xy_struct_raw,
                              includeMeanStruct = input$include_mean_struct %||% FALSE),
        scriptCFA = scriptCFA(dvn, scaleset = ss,
                              constr_dy_meas = meas, constr_dy_struct = struct),
        stop("Model type not yet supported in the GUI.")
      )

      # Determine the fit function for this model
      fit_fn <- if (model %in% c("scriptAPIM", "scriptCFM", "scriptMIM", "scriptTwoCross")) {
        "sem"
      } else {
        "cfa"
      }

      generated$syntax <- syntax
      generated$dvn <- dvn
      generated$fit_fn <- fit_fn

      # Build reproducible R code
      generated$rcode <- build_rcode(
        category = category, model = model,
        order = input$naming_order,
        dist1 = input$dist1, dist2 = input$dist2,
        stem_x = input$stem_x, delim1_x = input$delim1_x,
        delim2_x = input$delim2_x, n_items_x = input$n_items_x,
        lvxname = lvxname, lvyname = lvyname,
        stem_y = input$stem_y, delim1_y = input$delim1_y,
        delim2_y = input$delim2_y, n_items_y = input$n_items_y,
        scaleset = ss, model_type = model,
        meas = if (category == "bi") x_meas else if (exists("meas")) meas else "none",
        struct = if (category == "bi") x_struct else if (exists("struct")) struct else "none",
        x_meas = if (category == "bi") x_meas else NULL,
        x_struct = if (category == "bi") x_struct else NULL,
        y_meas = if (category == "bi") y_meas else NULL,
        y_struct = if (category == "bi") y_struct else NULL,
        xy_struct = if (category == "bi") xy_struct_raw else NULL,
        mc_info = if (category == "multi") {
          n <- n_mc()
          list(
            lvnames = vapply(seq_len(n), function(i) input[[paste0("mc_lvname_", i)]] %||% "", character(1)),
            stems = vapply(seq_len(n), function(i) input[[paste0("mc_stem_", i)]] %||% "", character(1)),
            delim1s = vapply(seq_len(n), function(i) input[[paste0("mc_delim1_", i)]] %||% "", character(1)),
            delim2s = vapply(seq_len(n), function(i) input[[paste0("mc_delim2_", i)]] %||% "", character(1))
          )
        } else NULL
      )

      showNotification("Script generated successfully!", type = "message", duration = 3)

    }, error = function(e) {
      showNotification(
        paste("Error generating script:", e$message),
        type = "error", duration = 8
      )
    })
  })

  # ── Tab 2: Path Diagram ──
  output$path_diagram <- renderPlot({
    req(generated$syntax, generated$dvn)

    dvn <- generated$dvn
    syntax <- generated$syntax
    varnames <- get_all_varnames(dvn)
    dummy <- make_dummy_data(varnames)

    tryCatch({
      show_int <- if (is.null(input$diagram_show_intercepts)) {
        TRUE
      } else {
        isTRUE(input$diagram_show_intercepts)
      }
      fit_call <- if (generated$fit_fn == "sem") lavaan::sem else lavaan::cfa
      fit <- fit_call(syntax, data = dummy, do.fit = FALSE)
      semPlot::semPaths(fit,
                        whatLabels = "names",
                        layout = "tree2",
                        edge.label.cex = 0.6,
                        curvePivot = FALSE,
                        intercepts = show_int,
                        edge.color = "black",
                        weighted = FALSE,
                        nCharNodes = 0,
                        sizeMan = 4,
                        sizeLat = 7,
                        mar = c(8, 6, 8, 6))
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5,
           paste("Could not render path diagram:\n", e$message),
           cex = 1.1, col = "red")
    })
  })

  # ── Tab 3: Lavaan Syntax ──
  output$syntax_output <- renderText({
    req(generated$syntax)
    generated$syntax
  })

  observeEvent(input$copy_syntax, {
    req(generated$syntax)
    session$sendCustomMessage("copy-to-clipboard", generated$syntax)
    showNotification("Syntax copied to clipboard!", type = "message", duration = 2)
  })

  output$download_syntax <- downloadHandler(
    filename = function() {
      paste0("dySEM_syntax_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
    },
    content = function(file) {
      writeLines(generated$syntax, file)
    }
  )

  # ── Tab 4: Reproducible R Code ──
  output$rcode_output <- renderText({
    req(generated$rcode)
    generated$rcode
  })

  observeEvent(input$copy_rcode, {
    req(generated$rcode)
    session$sendCustomMessage("copy-to-clipboard", generated$rcode)
    showNotification("R code copied to clipboard!", type = "message", duration = 2)
  })

  # ── Tab 5: Understanding Panel ──
  output$understand_placeholder <- renderUI({
    if (is.null(generated$syntax)) {
      tags$div(
        style = "text-align:center; padding: 60px; opacity: 0.5;",
        icon("lightbulb", style = "font-size: 3em;"),
        tags$p("Click 'Generate Script & Preview' to see the color-coded Understanding panel.")
      )
    }
  })

  output$understand_legend <- renderUI({
    req(generated$syntax)
    make_color_legend()
  })

  output$understand_diagram <- renderPlot({
    req(generated$syntax, generated$dvn)
    dvn <- generated$dvn
    syntax <- generated$syntax
    varnames <- get_all_varnames(dvn)
    dummy <- make_dummy_data(varnames)

    tryCatch({
      show_int <- if (is.null(input$diagram_show_intercepts)) {
        TRUE
      } else {
        isTRUE(input$diagram_show_intercepts)
      }
      fit_call <- if (generated$fit_fn == "sem") lavaan::sem else lavaan::cfa
      fit <- fit_call(syntax, data = dummy, do.fit = FALSE)
      g <- color_diagram(fit, show_intercepts = show_int)
      plot(g)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5,
           paste("Could not render diagram:\n", e$message),
           cex = 1.1, col = "red")
    })
  })

  output$understand_syntax <- renderUI({
    req(generated$syntax)
    lvname <- input$lvname_x %||% "X"
    dvn <- generated$dvn
    html <- color_syntax(generated$syntax, lvname, dvn)
    HTML(html)
  })

  output$understand_english <- renderUI({
    req(generated$syntax)
    lvname <- input$lvname_x %||% "X"
    dvn <- generated$dvn
    html <- color_english(generated$syntax, dvn, lvname)
    HTML(html)
  })
}


# ── R Code Generator ───────────────────────────────────────────────────────

build_rcode <- function(category, model, order, dist1, dist2,
                        stem_x, delim1_x, delim2_x, n_items_x,
                        lvxname, lvyname,
                        stem_y, delim1_y, delim2_y, n_items_y,
                        scaleset, model_type,
                        meas, struct,
                        x_meas, x_struct, y_meas, y_struct,
                        xy_struct, mc_info) {

  lines <- c("library(dySEM)", "library(lavaan)", "")

  quote_str <- function(x) {
    if (is.null(x)) return("NULL")
    if (length(x) == 1) return(paste0('"', x, '"'))
    paste0('c(', paste0('"', x, '"', collapse = ", "), ')')
  }

  if (category == "uni") {
    lines <- c(lines,
      "# Step 1: Scrape variable names from your data",
      sprintf('dvn <- scrapeVarCross(dat = my_data, x_order = "%s",', order),
      sprintf('  x_stem = "%s", x_delim1 = "%s", x_delim2 = "%s",',
              stem_x, delim1_x %||% "", delim2_x %||% ""),
      sprintf('  distinguish_1 = "%s", distinguish_2 = "%s")', dist1, dist2),
      "",
      "# Step 2: Generate lavaan syntax",
      sprintf('script <- %s(dvn, scaleset = "%s",', model_type, scaleset),
      sprintf('  lvname = "%s",', lvxname),
      sprintf('  constr_dy_meas = %s,', quote_str(meas)),
      sprintf('  constr_dy_struct = %s)', quote_str(struct)),
      "",
      "# Step 3: Fit the model",
      "fit <- lavaan::cfa(script, data = my_data)",
      "",
      "# Step 4: Examine results",
      "summary(fit, fit.measures = TRUE, standardized = TRUE)"
    )

  } else if (category == "bi") {
    lines <- c(lines,
      "# Step 1: Scrape variable names from your data",
      sprintf('dvn <- scrapeVarCross(dat = my_data, x_order = "%s",', order),
      sprintf('  x_stem = "%s", x_delim1 = "%s", x_delim2 = "%s",',
              stem_x, delim1_x %||% "", delim2_x %||% ""),
      sprintf('  distinguish_1 = "%s", distinguish_2 = "%s",', dist1, dist2),
      sprintf('  y_order = "%s",', order),
      sprintf('  y_stem = "%s", y_delim1 = "%s", y_delim2 = "%s")',
              stem_y %||% "", delim1_y %||% "", delim2_y %||% ""),
      ""
    )

    fit_fn <- if (model_type %in% c("scriptAPIM", "scriptCFM", "scriptMIM", "scriptTwoCross")) "sem" else "cfa"

    if (model_type == "scriptTwoCross") {
      lines <- c(lines,
        "# Step 2: Generate lavaan syntax",
        sprintf('script <- scriptTwoCross(dvn,'),
        sprintf('  lvxname = "%s", lvyname = "%s",', lvxname, lvyname),
        sprintf('  constr_dy_x_meas = %s, constr_dy_x_struct = %s,',
                quote_str(x_meas), quote_str(x_struct)),
        sprintf('  constr_dy_y_meas = %s, constr_dy_y_struct = %s,',
                quote_str(y_meas), quote_str(y_struct)),
        sprintf('  constr_dy_xy_struct = %s)', quote_str(xy_struct))
      )
    } else {
      lines <- c(lines,
        "# Step 2: Generate lavaan syntax",
        sprintf('script <- %s(dvn, scaleset = "%s",', model_type, scaleset),
        sprintf('  lvxname = "%s", lvyname = "%s",', lvxname, lvyname),
        sprintf('  constr_dy_x_meas = %s, constr_dy_x_struct = %s,',
                quote_str(x_meas), quote_str(x_struct)),
        sprintf('  constr_dy_y_meas = %s, constr_dy_y_struct = %s,',
                quote_str(y_meas), quote_str(y_struct)),
        sprintf('  constr_dy_xy_struct = %s)', quote_str(xy_struct))
      )
    }

    lines <- c(lines, "",
      "# Step 3: Fit the model",
      sprintf("fit <- lavaan::%s(script, data = my_data)", fit_fn),
      "",
      "# Step 4: Examine results",
      "summary(fit, fit.measures = TRUE, standardized = TRUE)"
    )

  } else if (category == "multi") {
    lines <- c(lines,
      "# Step 1: Define your variable list",
      "var_list <- list(",
      sprintf('  lvnames = %s,', quote_str(mc_info$lvnames)),
      sprintf('  stem = %s,', quote_str(mc_info$stems)),
      sprintf('  delim1 = %s,', quote_str(mc_info$delim1s)),
      sprintf('  delim2 = %s', quote_str(mc_info$delim2s)),
      ")",
      "",
      "# Step 2: Scrape variable names from your data",
      sprintf('dvn <- scrapeVarCross(dat = my_data, var_list = var_list,'),
      sprintf('  var_list_order = "%s",', order),
      sprintf('  distinguish_1 = "%s", distinguish_2 = "%s")', dist1, dist2),
      "",
      "# Step 3: Generate lavaan syntax",
      sprintf('script <- %s(dvn, scaleset = "%s",', model_type, scaleset),
      sprintf('  constr_dy_meas = %s,', quote_str(meas)),
      sprintf('  constr_dy_struct = %s)', quote_str(struct)),
      "",
      "# Step 4: Fit the model",
      "fit <- lavaan::cfa(script, data = my_data)",
      "",
      "# Step 5: Examine results",
      "summary(fit, fit.measures = TRUE, standardized = TRUE)"
    )
  }

  paste(lines, collapse = "\n")
}


# ── Launch ──────────────────────────────────────────────────────────────────

shinyApp(ui, server)
