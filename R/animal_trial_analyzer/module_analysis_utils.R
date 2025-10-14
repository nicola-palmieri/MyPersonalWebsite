# ===============================================================
# 🧪 Animal Trial Analyzer — Shared Analysis Utilities
# ===============================================================

validate_numeric_and_factor <- function(data) {
  num_cols <- names(data)[sapply(data, is.numeric)]
  cat_cols <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
  list(numeric = num_cols, categorical = cat_cols)
}
