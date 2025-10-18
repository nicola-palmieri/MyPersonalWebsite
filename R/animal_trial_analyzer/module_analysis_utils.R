# ===============================================================
# 🧪 Animal Trial Analyzer — Analysis Utility Functions
# ===============================================================

# ---- 1. p-value formatting ----
format_p_value <- function(p_values) {
  vapply(
    p_values,
    function(p) {
      if (is.na(p)) {
        return(NA_character_)
      }
      if (p < 0.001) {
        "<0.001"
      } else {
        sprintf("%.2f", round(p, 2))
      }
    },
    character(1)
  )
}

# ---- 2. significance markers ----
add_significance_marker <- function(formatted_p, raw_p) {
  mapply(
    function(fp, rp) {
      if (is.na(rp)) {
        return(fp)
      }
      if (rp < 0.05) {
        paste0(fp, "*")
      } else {
        fp
      }
    },
    formatted_p,
    raw_p,
    USE.NAMES = FALSE
  )
}

# ---- 3. prepare ANOVA + posthoc results ----
prepare_anova_outputs <- function(model_obj, factor_names) {
  old_contrasts <- options("contrasts")
  on.exit(options(old_contrasts), add = TRUE)
  options(contrasts = c("contr.sum", "contr.poly"))
  
  anova_obj <- car::Anova(model_obj, type = 3)
  anova_df <- as.data.frame(anova_obj)
  anova_df$Effect <- rownames(anova_df)
  rownames(anova_df) <- NULL
  anova_df <- anova_df[, c("Effect", setdiff(names(anova_df), "Effect"))]
  
  # --- format p-values and round numeric columns ---
  p_col <- grep("^Pr", names(anova_df), value = TRUE)
  p_col <- if (length(p_col) > 0) p_col[1] else NULL
  raw_p <- if (!is.null(p_col)) anova_df[[p_col]] else rep(NA_real_, nrow(anova_df))
  
  for (col in names(anova_df)) {
    if (is.numeric(anova_df[[col]])) {
      anova_df[[col]] <- round(anova_df[[col]], 2)
    }
  }
  
  anova_significant <- !is.na(raw_p) & raw_p < 0.05
  if (!is.null(p_col)) {
    formatted_p <- format_p_value(raw_p)
    anova_df[[p_col]] <- add_significance_marker(formatted_p, raw_p)
    names(anova_df)[names(anova_df) == p_col] <- "p.value"
  } else {
    anova_df$p.value <- NA_character_
  }
  
  # --- Post-hoc Tukey for each factor ---
  factor_names <- unique(factor_names[!is.na(factor_names) & nzchar(factor_names)])
  posthoc_details <- list()
  posthoc_combined <- NULL
  posthoc_significant <- numeric(0)
  
  for (factor_nm in factor_names) {
    if (!factor_nm %in% names(model_obj$model)) next
    
    res <- tryCatch({
      emm <- emmeans::emmeans(model_obj, specs = factor_nm)
      contrasts <- emmeans::contrast(emm, method = "pairwise", adjust = "tukey")
      as.data.frame(summary(contrasts))
    }, error = function(e) list(error = e$message))
    
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
    numeric_cols <- names(posthoc_combined)[sapply(posthoc_combined, is.numeric)]
    if (length(numeric_cols) > 0) {
      for (col in numeric_cols) {
        posthoc_combined[[col]] <- round(posthoc_combined[[col]], 2)
      }
    }
    
    if ("p.value" %in% names(posthoc_combined)) {
      raw_posthoc_p <- posthoc_combined$p.value
      posthoc_significant <- !is.na(raw_posthoc_p) & raw_posthoc_p < 0.05
      formatted_posthoc_p <- format_p_value(raw_posthoc_p)
      posthoc_combined$p.value <- add_significance_marker(formatted_posthoc_p, raw_posthoc_p)
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

# ---- 4. Word export helper ----
write_anova_docx <- function(file, results, model_obj, response_name, stratum_label = NULL) {
  doc <- officer::read_docx()
  
  title_text <- paste("ANOVA results for:", response_name)
  doc <- officer::body_add_par(doc, title_text, style = "heading 1")
  
  if (!is.null(stratum_label)) {
    doc <- officer::body_add_par(doc, paste("Stratum:", stratum_label), style = "heading 2")
  }
  
  doc <- officer::body_add_par(doc, paste("Model formula:", format(formula(model_obj))), style = "heading 2")
  doc <- officer::body_add_par(doc, "Type III ANOVA results", style = "heading 2")
  
  ft_anova <- flextable::flextable(results$anova_table)
  if (nrow(results$anova_table) > 0) {
    ft_anova <- flextable::bold(ft_anova, i = which(results$anova_significant), j = "p.value", bold = TRUE)
  }
  ft_anova <- flextable::autofit(ft_anova)
  doc <- flextable::body_add_flextable(doc, ft_anova)
  
  doc <- officer::body_add_par(doc, "Post-hoc Tukey comparisons", style = "heading 2")
  
  if (is.null(results$posthoc_table) || nrow(results$posthoc_table) == 0) {
    doc <- officer::body_add_par(doc, "No post-hoc Tukey comparisons were generated.", style = "Normal")
  } else {
    ft_posthoc <- flextable::flextable(results$posthoc_table)
    if (length(results$posthoc_significant) > 0) {
      ft_posthoc <- flextable::bold(ft_posthoc, i = which(results$posthoc_significant), j = "p.value", bold = TRUE)
    }
    ft_posthoc <- flextable::autofit(ft_posthoc)
    doc <- flextable::body_add_flextable(doc, ft_posthoc)
  }
  
  doc <- officer::body_add_par(doc, "Significant differences are indicated by p < 0.05.", style = "Normal")
  
  print(doc, target = file)
}
