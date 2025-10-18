# ===============================================================
# 🧪 Animal Trial Analyzer — Shared Analysis Utilities
# ===============================================================

validate_numeric_and_factor <- function(data) {
  num_cols <- names(data)[sapply(data, is.numeric)]
  cat_cols <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
  list(numeric = num_cols, categorical = cat_cols)
}

format_p_values <- function(p_values, digits = 2, threshold = 0.001) {
  vapply(
    p_values,
    function(p) {
      if (is.na(p)) {
        return(NA_character_)
      }
      if (p < threshold) {
        paste0("<", format(threshold, trim = TRUE))
      } else {
        sprintf(paste0("%.", digits, "f"), round(p, digits))
      }
    },
    character(1)
  )
}

add_significance_markers <- function(formatted_p, raw_p, sig_level = 0.05, symbol = "*") {
  mapply(
    function(fp, rp) {
      if (is.na(rp) || is.na(fp)) {
        return(fp)
      }
      if (rp < sig_level) {
        paste0(fp, symbol)
      } else {
        fp
      }
    },
    formatted_p,
    raw_p,
    USE.NAMES = FALSE
  )
}

prepare_anova_outputs <- function(model_obj, factor_names, sig_level = 0.05, digits = 2) {
  old_contrasts <- options("contrasts")
  on.exit(options(old_contrasts), add = TRUE)
  options(contrasts = c("contr.sum", "contr.poly"))

  anova_obj <- car::Anova(model_obj, type = 3)
  anova_df <- as.data.frame(anova_obj)
  anova_df$Effect <- rownames(anova_df)
  rownames(anova_df) <- NULL
  anova_df <- anova_df[, c("Effect", setdiff(names(anova_df), "Effect"))]

  p_col <- grep("^Pr", names(anova_df), value = TRUE)
  p_col <- if (length(p_col) > 0) p_col[1] else NULL
  raw_p <- if (!is.null(p_col)) anova_df[[p_col]] else rep(NA_real_, nrow(anova_df))

  numeric_cols <- vapply(anova_df, is.numeric, logical(1))
  if (any(numeric_cols)) {
    anova_df[numeric_cols] <- lapply(anova_df[numeric_cols], round, digits = digits)
  }

  anova_significant <- !is.na(raw_p) & raw_p < sig_level
  if (!is.null(p_col)) {
    formatted_p <- format_p_values(raw_p, digits = digits)
    anova_df[[p_col]] <- add_significance_markers(formatted_p, raw_p, sig_level = sig_level)
    names(anova_df)[names(anova_df) == p_col] <- "p.value"
  } else {
    anova_df$p.value <- NA_character_
  }

  factor_names <- unique(factor_names[!is.na(factor_names) & nzchar(factor_names)])
  posthoc_details <- list()
  posthoc_combined <- NULL
  posthoc_significant <- logical()

  for (factor_nm in factor_names) {
    if (!factor_nm %in% names(model_obj$model)) {
      next
    }

    res <- tryCatch({
      emm <- emmeans::emmeans(model_obj, specs = factor_nm)
      contrasts <- emmeans::contrast(emm, method = "pairwise", adjust = "tukey")
      as.data.frame(summary(contrasts))
    }, error = function(e) {
      list(error = e$message)
    })

    if (is.data.frame(res)) {
      res$Factor <- factor_nm
      posthoc_details[[factor_nm]] <- list(table = res, error = NULL)
      posthoc_combined <- rbind(posthoc_combined, res)
    } else {
      posthoc_details[[factor_nm]] <- list(table = NULL, error = res$error)
    }
  }

  if (!is.null(posthoc_combined)) {
    posthoc_combined <- posthoc_combined[, c("Factor", setdiff(names(posthoc_combined), "Factor"))]
    numeric_cols <- vapply(posthoc_combined, is.numeric, logical(1))
    if (any(numeric_cols)) {
      posthoc_combined[numeric_cols] <- lapply(posthoc_combined[numeric_cols], round, digits = digits)
    }

    if ("p.value" %in% names(posthoc_combined)) {
      raw_posthoc_p <- posthoc_combined$p.value
      posthoc_significant <- !is.na(raw_posthoc_p) & raw_posthoc_p < sig_level
      formatted_posthoc_p <- format_p_values(raw_posthoc_p, digits = digits)
      posthoc_combined$p.value <- add_significance_markers(formatted_posthoc_p, raw_posthoc_p, sig_level = sig_level)
    } else {
      posthoc_significant <- rep(FALSE, nrow(posthoc_combined))
    }
  }

  list(
    anova_object = anova_obj,
    anova_table = anova_df,
    anova_significant = anova_significant,
    posthoc_details = posthoc_details,
    posthoc_table = posthoc_combined,
    posthoc_significant = posthoc_significant
  )
}
