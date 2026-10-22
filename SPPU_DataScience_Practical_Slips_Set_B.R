# ==============================================================================
# SAVITRIBAI PHULE PUNE UNIVERSITY / DR. D. Y. PATIL COLLEGE
# T.Y.B.Sc. (DATA SCIENCE) SEM-V - DS-302-MJ-T : R PROGRAMMING
# PRACTICAL SLIPS - SET B & DETAILED SOLUTIONS
# ==============================================================================

# Helper function for Mode calculation
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# ==============================================================================
# QUESTION 1
# Write a R program to create two vectors:
# A = 12, 25, 35, 45, 50
# B = 15, 20, 35, 40, 55
# Perform the following operations:
# a) Addition
# b) Multiplication
# c) Find elements where A > B
# d) Find elements where A != B
# ==============================================================================
A <- c(12, 25, 35, 45, 50)
B <- c(15, 20, 35, 40, 55)

cat("Vector A:", A, "
")
cat("Vector B:", B, "
")
cat("a) Addition (A + B):", A + B, "
")
cat("b) Multiplication (A * B):", A * B, "
")
cat("c) Elements where A > B:", A[A > B], "
")
cat("d) Elements where A != B:", A[A != B], "

")


# ==============================================================================
# QUESTION 2
# Write an R program to create a vector 15, 25, 10, 30, 25, 40, 35, 20, 15, 50 and:
# - Reverse the vector.
# - Find maximum and minimum values.
# - Find the frequency of each value.
# - Display unique values.
# ==============================================================================
x <- c(15, 25, 10, 30, 25, 40, 35, 20, 15, 50)

cat("Original Vector:", x, "
")
cat("Reversed Vector:", rev(x), "
")
cat("Maximum Value:", max(x), "
")
cat("Minimum Value:", min(x), "
")
cat("Frequency of each value:
")
print(table(x))
cat("Unique Values:", unique(x), "

")


# ==============================================================================
# QUESTION 3
# Write an R program to create the following data frame:
# RollNo: 1..9, Name: Armaan, Zoya, Riya, Rohan, Khushi, Sid, Piya, Ayan, Soha
# Marks: 72, 88, 91, 65, 79, 63, 55, 81, 77
# Perform:
# - Display students scoring > 75 marks
# - Display first and last 10 rows
# - Find shape and size of the data
# ==============================================================================
students <- data.frame(
  RollNo = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
  Name = c("Armaan", "Zoya", "Riya", "Rohan", "Khushi", "Sid", "Piya", "Ayan", "Soha"),
  Marks = c(72, 88, 91, 65, 79, 63, 55, 81, 77)
)

cat("Students scoring > 75 marks:
")
print(students[students$Marks > 75, ])

cat("
First 10 rows (head):
")
print(head(students, 10))

cat("
Last 10 rows (tail):
")
print(tail(students, 10))

cat("
Shape of Data (Rows x Columns):", dim(students), "
")
cat("Size of Data (Total elements):", length(as.matrix(students)), "

")


# ==============================================================================
# QUESTION 4
# Write an R program for the same Student Data frame to:
# - Find number of rows and columns
# - Display structure of dataset
# - Find column names
# - Obtain summary statistics
# ==============================================================================
cat("Number of Rows:", nrow(students), "
")
cat("Number of Columns:", ncol(students), "
")
cat("
Structure of Dataset:
")
str(students)
cat("
Column Names:", names(students), "
")
cat("
Summary Statistics:
")
print(summary(students))
cat("
")


# ==============================================================================
# QUESTION 5
# Write an R program to create 3 matrices and find:
# (i) A + B + C  (ii) 3A  (iii) B'  (iv) 2B - C  (v) |A|
# ==============================================================================
A_mat <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, byrow = TRUE)
B_mat <- matrix(c(9, 8, 7, 6, 5, 4, 3, 2, 1), nrow = 3, byrow = TRUE)
C_mat <- matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 3), nrow = 3, byrow = TRUE)

cat("(i) A + B + C:
"); print(A_mat + B_mat + C_mat)
cat("(ii) 3A:
"); print(3 * A_mat)
cat("(iii) Transpose of B (B'):
"); print(t(B_mat))
cat("(iv) 2B - C:
"); print(2 * B_mat - C_mat)
cat("(v) Determinant of A |A|:", det(A_mat), "

")


# ==============================================================================
# QUESTION 6
# Create a styled Bar Chart for Online Learning Platforms:
# YouTube (180), Coursera (120), Udemy (90), edX (60)
# ==============================================================================
platforms <- c("YouTube", "Coursera", "Udemy", "edX")
student_counts <- c(180, 120, 90, 60)

barplot(student_counts,
        names.arg = platforms,
        col = c("red", "blue", "orange", "green"),
        border = "black",
        main = "Students Using Online Learning Platforms",
        xlab = "Online Learning Platform",
        ylab = "Number of Students",
        ylim = c(0, 200),
        las = 1)


# ==============================================================================
# QUESTION 7
# Data Center Energy Consumption Pie Chart:
# Servers (60%), Cooling (25%), Lighting (10%), Others (5%)
# ==============================================================================
energy <- c(60, 25, 10, 5)
labels <- c("Servers", "Cooling", "Lighting", "Others")
pct_labels <- paste0(labels, " (", energy, "%)")

pie(energy, labels = pct_labels,
    col = c("steelblue", "orange", "green", "purple"),
    main = "Data Center Energy Consumption")


# ==============================================================================
# QUESTION 8
# Histogram and Boxplot for Daily Step Counts (25 people)
# Steps: 3.2, 4.5, 5.1, 6.0, 7.2, 8.5, 4.8, 6.7, 9.1, 10.5, 5.5, 7.8, 11.2, 12.5,
#        6.2, 8.0, 9.8, 13.0, 14.5, 7.0, 5.9, 10.0, 11.8, 13.5, 8.9
# ==============================================================================
steps <- c(3.2, 4.5, 5.1, 6.0, 7.2, 8.5, 4.8, 6.7, 9.1, 10.5, 
           5.5, 7.8, 11.2, 12.5, 6.2, 8.0, 9.8, 13.0, 14.5, 
           7.0, 5.9, 10.0, 11.8, 13.5, 8.9)

par(mfrow = c(1, 2))
hist(steps, main = "Histogram of Daily Steps", xlab = "Steps (thousands)", col = "skyblue")
boxplot(steps, main = "Boxplot of Daily Steps", ylab = "Steps (thousands)", col = "lightgreen")
par(mfrow = c(1, 1))


# ==============================================================================
# QUESTION 9
# Grouped Frequency Histogram: Weekly Sales Performance
# Sales (10-20, 20-30, 30-40, 40-50, 50-60, 60-70) -> Emp: 5, 9, 12, 7, 4, 3
# ==============================================================================
midpoints <- c(15, 25, 35, 45, 55, 65)
frequencies <- c(5, 9, 12, 7, 4, 3)
sales_data <- rep(midpoints, times = frequencies)

hist(sales_data,
     breaks = seq(10, 70, by = 10),
     main = "Weekly Sales Performance of Employees",
     xlab = "Sales (₹ in thousands)",
     ylab = "Number of Employees",
     col = "lightcoral", border = "black")


# ==============================================================================
# QUESTION 10
# Data Science Test Marks: 45, 52, 60, 65, 13, 70, 72, 75, 80, 90, 55, 74
# Create Histogram, Boxplot, and identify Outliers.
# ==============================================================================
marks <- c(45, 52, 60, 65, 13, 70, 72, 75, 80, 90, 55, 74)

par(mfrow = c(1, 2))
hist(marks, main = "Histogram of Marks", xlab = "Marks", col = "plum")
boxplot(marks, main = "Boxplot of Marks", ylab = "Marks", col = "lightblue")
par(mfrow = c(1, 1))

outliers <- boxplot.stats(marks)$out
cat("Outliers detected:", if(length(outliers) == 0) "None" else outliers, "

")


# ==============================================================================
# QUESTION 11
# Working Hours (10 employees): 6, 8, 7, 9, 10, 8, 17, 6, 9, 8
# Calculate AM, GM, HM, Median, Mode, and state best measure.
# ==============================================================================
work_data <- c(6, 8, 7, 9, 10, 8, 17, 6, 9, 8)

am <- mean(work_data)
gm <- exp(mean(log(work_data)))
hm <- 1 / mean(1 / work_data)
med <- median(work_data)
mod <- get_mode(work_data)

cat("Arithmetic Mean:", am, "
")
cat("Geometric Mean:", gm, "
")
cat("Harmonic Mean:", hm, "
")
cat("Median:", med, "
")
cat("Mode:", mod, "
")
cat("Best Measure: Median (8) or Mode (8) due to the presence of outlier 17.

")


# ==============================================================================
# QUESTION 12
# Mean, Median, and Mode for Grouped Data (Tasks Completed by 20 employees)
# Classes: 10-15, 15-20, 20-25, 25-30, 30-35, 35-40
# Freq: 2, 7, 4, 3, 3, 1
# ==============================================================================
task_mids <- c(12.5, 17.5, 22.5, 27.5, 32.5, 37.5)
task_counts <- c(2, 7, 4, 3, 3, 1)
tasks_data <- rep(task_mids, times = task_counts)

cat("Mean Tasks Completed:", mean(tasks_data), "
")
cat("Median Tasks Completed:", median(tasks_data), "
")
cat("Modal Midpoint:", get_mode(tasks_data), "

")


# ==============================================================================
# QUESTION 13
# Mean, Median, Mode for Statistics test (13 students):
# Marks: 45, 52, 60, 65, 70, 72, 82, 90, 55, 68, 74, 85, 92
# ==============================================================================
stat_marks <- c(45, 52, 60, 65, 70, 72, 82, 90, 55, 68, 74, 85, 92)

cat("Mean Marks:", mean(stat_marks), "
")
cat("Median Marks:", median(stat_marks), "
")
cat("Mode Marks:", get_mode(stat_marks), "

")


# ==============================================================================
# QUESTION 14
# Pocket Money (40 students): 0-10 (5), 10-20 (12), 20-30 (18), 30-40 (5)
# Calculate Mean, Median, P72.
# ==============================================================================
money_mids <- c(5, 15, 25, 35)
money_freqs <- c(5, 12, 18, 5)
pocket_data <- rep(money_mids, times = money_freqs)

cat("Mean Pocket Money: $", mean(pocket_data), "
", sep="")
cat("Median Pocket Money: $", median(pocket_data), "
", sep="")
cat("72nd Percentile (P72): $", quantile(pocket_data, 0.72), "

", sep="")


# ==============================================================================
# QUESTION 15
# Range and CV for Cybersecurity Training Scores (50 employees):
# Range: 50-60 (4), 60-70 (10), 70-80 (22), 80-90 (19), 90-100 (15)
# ==============================================================================
cyber_mids <- c(55, 65, 75, 85, 95)
cyber_freqs <- c(4, 10, 22, 19, 15)
cyber_data <- rep(cyber_mids, times = cyber_freqs)

cyber_range <- 100 - 50
cyber_mean <- mean(cyber_data)
cyber_sd <- sd(cyber_data)
cyber_cv <- (cyber_sd / cyber_mean) * 100

cat("Range of Scores:", cyber_range, "
")
cat("Mean Score:", round(cyber_mean, 2), "
")
cat("Standard Deviation:", round(cyber_sd, 2), "
")
cat("Coefficient of Variation (CV):", round(cyber_cv, 2), "%

")


# ==============================================================================
# QUESTION 16
# Stock Volatility Comparison (Stock A vs Stock B):
# Stock A: 150, 152, 149, 151, 150, 153
# Stock B: 100, 125, 190, 160, 80, 130
# ==============================================================================
stock_A <- c(150, 152, 149, 151, 150, 153)
stock_B <- c(100, 125, 190, 160, 80, 130)

cv_A <- (sd(stock_A) / mean(stock_A)) * 100
cv_B <- (sd(stock_B) / mean(stock_B)) * 100

cat("Stock A -> Mean:", mean(stock_A), "| SD:", sd(stock_A), "| CV:", round(cv_A, 2), "%
")
cat("Stock B -> Mean:", mean(stock_B), "| SD:", sd(stock_B), "| CV:", round(cv_B, 2), "%
")
cat("Advisory: Conservative investor should choose Stock A (lower CV = lower risk).

")


# ==============================================================================
# QUESTION 17
# Pearson Correlation Coefficient: Hours Studied vs Exam Score
# Hours (X): 2, 4, 5, 7, 8, 10, 12, 15 | Score (Y): 45, 52, 58, 70, 72, 85, 88, 95
# ==============================================================================
hours <- c(2, 4, 5, 7, 8, 10, 12, 15)
scores <- c(45, 52, 58, 70, 72, 85, 88, 95)

p_corr <- cor(hours, scores, method = "pearson")
cat("Pearson Correlation Coefficient (r):", round(p_corr, 4), "

")


# ==============================================================================
# QUESTION 18
# Simple Linear Regression: House Size (X) vs Price (Y)
# ==============================================================================
house_size <- c(1200, 1500, 1800, 2100, 2400, 2700, 3000, 3300, 3600, 4000)
price <- c(250, 310, 360, 425, 490, 540, 610, 675, 730, 820)

house_lm <- lm(price ~ house_size)
print(summary(house_lm))


# ==============================================================================
# QUESTION 19
# Spearman's Rank Correlation Coefficient: UI Rank vs UX Rank
# UI: 1, 2, 3, 4, 5, 6, 7 | UX: 2, 1, 4, 3, 7, 5, 6
# ==============================================================================
UI_rank <- c(1, 2, 3, 4, 5, 6, 7)
UX_rank <- c(2, 1, 4, 3, 7, 5, 6)

s_corr <- cor(UI_rank, UX_rank, method = "spearman")
cat("Spearman's Rank Correlation Coefficient:", round(s_corr, 4), "

")


# ==============================================================================
# QUESTION 20
# Probability Calculations using R:
# 1) Binomial P(X <= 3), Bin(10, 0.4)
# 2) Binomial P(X = 4), Bin(8, 0.5)
# 3) Normal P(X <= 5), N(5, 2.1^2)
# 4) Normal P(10 <= X <= 13), N(12, 3^2)
# 5) Normal quantile for P(Z <= c) = 0.025
# ==============================================================================
cat("1. Binomial P(X <= 3) [N=10, p=0.4]:", pbinom(3, 10, 0.4), "
")
cat("2. Binomial P(X = 4) [N=8, p=0.5]:", dbinom(4, 8, 0.5), "
")
cat("3. Normal P(X <= 5) [mean=5, sd=2.1]:", pnorm(5, 5, 2.1), "
")
cat("4. Normal P(10 <= X <= 13) [mean=12, sd=3]:", pnorm(13, 12, 3) - pnorm(10, 12, 3), "
")
cat("5. Critical Value c for P(Z <= c) = 0.025:", qnorm(0.025, 0, 1), "
")

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
