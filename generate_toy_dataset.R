# ===============================================================
# 🧪 Fixed Toy Dataset for Animal Trial Analyzer
# ===============================================================
set.seed(42)

# Define experimental structure ---------------------------------
Treatment_levels <- c("Control", "DrugA", "DrugB")
Diet_levels <- c("LowFat", "HighFat")
Day_levels <- c("Day14", "Day21", "Day28")
Batch_levels <- c("Batch1", "Batch2", "Batch3")
Sex_levels <- c("Male", "Female")

# Balanced design (3×2×3 = 18 combinations)
design <- expand.grid(
  Treatment = Treatment_levels,
  Diet = Diet_levels,
  Day = Day_levels
)

# Repeat design to reach ~120 animals
n_reps <- ceiling(120 / nrow(design))
df_design <- design[rep(1:nrow(design), each = n_reps), ]
df_design <- df_design[1:120, ]  # trim to exact n = 120

# Add other factors ----------------------------------------------
df_design$Animal_ID <- paste0("A", sprintf("%03d", 1:nrow(df_design)))
df_design$Sex <- sample(Sex_levels, nrow(df_design), replace = TRUE)
df_design$Batch <- sample(Batch_levels, nrow(df_design), replace = TRUE)
df_design$Weight_baseline <- round(rnorm(nrow(df_design), mean = 25, sd = 3), 1)

# Simulate responses ---------------------------------------------
n <- nrow(df_design)
with(df_design, {
  Cortisol   <- 10 + ifelse(Treatment == "DrugA", 1.5, ifelse(Treatment == "DrugB", 3, 0)) +
    ifelse(Diet == "HighFat", 1, 0) + rnorm(n, 0, 1)
  
  Glucose    <- 80 + ifelse(Treatment == "DrugA", 3, ifelse(Treatment == "DrugB", 7, 0)) +
    ifelse(Diet == "HighFat", 10, 0) + rnorm(n, 0, 5)
  
  FeedIntake <- 200 + ifelse(Treatment == "DrugA", -10, ifelse(Treatment == "DrugB", -20, 0)) +
    ifelse(Diet == "HighFat", 15, 0) + rnorm(n, 0, 10)
  
  HeartRate  <- 90 + ifelse(Treatment == "DrugA", -2, ifelse(Treatment == "DrugB", -4, 0)) +
    ifelse(Diet == "HighFat", 2, 0) + rnorm(n, 0, 3)
  
  BodyTemp   <- 38.5 + ifelse(Treatment == "DrugA", 0.2, ifelse(Treatment == "DrugB", 0.4, 0)) +
    ifelse(Diet == "HighFat", 0.3, 0) + rnorm(n, 0, 0.2)
  
  df_design$Cortisol   <<- Cortisol
  df_design$Glucose    <<- Glucose
  df_design$FeedIntake <<- FeedIntake
  df_design$HeartRate  <<- HeartRate
  df_design$BodyTemp   <<- BodyTemp
})

# Randomize order ------------------------------------------------
toy_data <- df_design[sample(nrow(df_design)), ]

# Save ------------------------------------------------------------
writexl::write_xlsx(toy_data, "toy_animal_trial_data.xlsx")

cat("✅ Generated: toy_animal_trial_data.xlsx with", nrow(toy_data), "rows\n")
