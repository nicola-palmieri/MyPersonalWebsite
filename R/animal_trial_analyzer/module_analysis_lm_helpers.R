# ===============================================================
# 🧩 Shared Helpers for LM / LMM Modules
# ===============================================================

make_formula <- function(dep, fixed, covar = NULL, random = NULL, mixed = FALSE) {
  rhs <- c(fixed, covar)
  if (mixed && !is.null(random)) {
    rhs <- c(rhs, paste0("(1|", random, ")"))
  }
  as.formula(paste(dep, "~", paste(rhs, collapse = " + ")))
}

render_diagnostics <- function(model, ns) {
  tagList(
    plotOutput(ns("resid_plot")),
    plotOutput(ns("qq_plot"))
  )
}

download_model_summary <- function(model, file) {
  library(officer)
  library(flextable)
  doc <- read_docx()
  smry <- broom::tidy(model)
  doc <- body_add_flextable(doc, flextable(smry))
  print(doc, target = file)
}
