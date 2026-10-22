# ==============================================================================
# SAVITRIBAI PHULE PUNE UNIVERSITY - B.Sc. DATA SCIENCE
# PRACTICAL SLIPS SOLUTIONS SET - R PROGRAMMING
# ==============================================================================

# Helper function for Mode calculation (Standard statistical mode implementation)
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# ==============================================================================
# SLIP 1
# ==============================================================================

# ------------------------------------------------------------------------------
# Q1. Write an R program to create two variables and perform basic arithmetic
#     operations such as addition, subtraction, multiplication, and division.
# ------------------------------------------------------------------------------
a <- 25
b <- 5

cat("Addition (a + b):", a + b, "
")
cat("Subtraction (a - b):", a - b, "
")
cat("Multiplication (a * b):", a * b, "
")
cat("Division (a / b):", a / b, "

")

# ------------------------------------------------------------------------------
# Q2. A company records the weekly sales performance of employees. Write an R
#     program to create a histogram using grouped frequency data with equal class intervals.
#     Sales (in thousands): 10-20, 20-30, 30-40, 40-50, 50-60, 60-70
#     Number of Employees: 5, 9, 12, 7, 4, 3
# ------------------------------------------------------------------------------
sales_midpoints <- c(15, 25, 35, 45, 55, 65)
emp_counts <- c(5, 9, 12, 7, 4, 3)

# Expand data for histogram representation
sales_data <- rep(sales_midpoints, times = emp_counts)

hist(sales_data,
     breaks = seq(10, 70, by = 10),
     main = "Histogram of Weekly Sales Performance",
     xlab = "Sales (in thousands)",
     ylab = "Number of Employees",
     col = "lightblue",
     border = "black")


# ==============================================================================
# SLIP 2
# ==============================================================================

# ------------------------------------------------------------------------------
# Q1. Write an R program to create a vector containing five numbers and find its
#     sum and average.
# ------------------------------------------------------------------------------
num_vec <- c(12, 24, 36, 48, 60)
vec_sum <- sum(num_vec)
vec_avg <- mean(num_vec)

cat("Vector:", num_vec, "
")
cat("Sum:", vec_sum, "
")
cat("Average:", vec_avg, "

")

# ------------------------------------------------------------------------------
# Q2. A teacher records marks obtained by students in a Data Science test.
#     Marks = 45, 52, 60, 65, 13, 70, 72, 75, 80, 90, 55, 74.
#     Write an R program to create:
#     - Histogram of marks
#     - Boxplot to identify spread and outliers
# ------------------------------------------------------------------------------
marks <- c(45, 52, 60, 65, 13, 70, 72, 75, 80, 90, 55, 74)

par(mfrow = c(1, 2))
hist(marks, main = "Histogram of Marks", xlab = "Marks", col = "lightgreen", border = "black")
boxplot(marks, main = "Boxplot of Marks", ylab = "Marks", col = "orange")
par(mfrow = c(1, 1))

# Identify outliers programmatically
outliers <- boxplot.stats(marks)$out
cat("Identified Outliers in Marks:", outliers, "

")


# ==============================================================================
# SLIP 3
# ==============================================================================

# ------------------------------------------------------------------------------
# Q1. Create variables to store a student's Name, Age, Marks, and Pass Status.
#     Display the values and find the data type of each variable.
# ------------------------------------------------------------------------------
student_name <- "Paras"
student_age <- 20
student_marks <- 88.5
pass_status <- TRUE

cat("Name:", student_name, "| Type:", class(student_name), "
")
cat("Age:", student_age, "| Type:", class(student_age), "
")
cat("Marks:", student_marks, "| Type:", class(student_marks), "
")
cat("Pass Status:", pass_status, "| Type:", class(pass_status), "

")

# ------------------------------------------------------------------------------
# Q2. Write an R program to calculate Pearson correlation coefficient and interpret it.
#     Hours Studied (X): 2, 4, 5, 7, 8, 10, 12, 15
#     Exam Score (Y): 45, 52, 58, 70, 72, 85, 88, 95
# ------------------------------------------------------------------------------
hours_studied <- c(2, 4, 5, 7, 8, 10, 12, 15)
exam_score <- c(45, 52, 58, 70, 72, 85, 88, 95)

pearson_corr <- cor(hours_studied, exam_score, method = "pearson")
cat("Pearson Correlation Coefficient:", pearson_corr, "
")

if (pearson_corr > 0.8) {
  cat("Interpretation: Strong positive linear correlation between study hours and exam score.

")
} else {
  cat("Interpretation: Moderate or weak correlation.

")
}


# ==============================================================================
# SLIP 4
# ==============================================================================

# ------------------------------------------------------------------------------
# Q1. Write an R program to convert the following data types:
#     - "250" to numeric
#     - 100 to character
#     - TRUE to numeric
# ------------------------------------------------------------------------------
str_val <- "250"
num_val <- 100
bool_val <- TRUE

conv_num <- as.numeric(str_val)
conv_char <- as.character(num_val)
conv_bool_num <- as.numeric(bool_val)

cat("Converted '250' to numeric:", conv_num, "| Class:", class(conv_num), "
")
cat("Converted 100 to character:", conv_char, "| Class:", class(conv_char), "
")
cat("Converted TRUE to numeric:", conv_bool_num, "| Class:", class(conv_bool_num), "

")

# ------------------------------------------------------------------------------
# Q2. A manager records the number of tasks completed by 20 employees in a week.
#     Calculate mean, median and mode for grouped frequency data.
#     Classes: 10-15, 15-20, 20-25, 25-30, 30-35, 35-40
#     Freq:    2, 7, 4, 3, 3, 1
# ------------------------------------------------------------------------------
class_midpoints <- c(12.5, 17.5, 22.5, 27.5, 32.5, 37.5)
task_freq <- c(2, 7, 4, 3, 3, 1)

expanded_tasks <- rep(class_midpoints, times = task_freq)

mean_tasks <- mean(expanded_tasks)
median_tasks <- median(expanded_tasks)
mode_tasks <- get_mode(expanded_tasks)

cat("Mean Tasks Completed:", mean_tasks, "
")
cat("Median Tasks Completed:", median_tasks, "
")
cat("Modal Class Midpoint:", mode_tasks, "

")


# ==============================================================================
# SLIP 5
# ==============================================================================

# ------------------------------------------------------------------------------
# Q1. Create a vector containing the ages of 8 students.
#     - Display the ages of students who are 18 years or older using relational operators.
#     - Display third to fifth elements of it.
#     - Display fifth element.
# ------------------------------------------------------------------------------
ages <- c(16, 19, 18, 21, 17, 22, 20, 15)

cat("Ages >= 18:", ages[ages >= 18], "
")
cat("3rd to 5th elements:", ages[3:5], "
")
cat("5th element:", ages[5], "

")

# ------------------------------------------------------------------------------
# Q2. A data center uses energy: Servers (60%), Cooling (25%), Lighting (10%), Others (5%).
#     Write an R program to generate Pie Chart of Energy Consumption.
# ------------------------------------------------------------------------------
energy_pct <- c(60, 25, 10, 5)
energy_labels <- c("Servers (60%)", "Cooling (25%)", "Lighting (10%)", "Others (5%)")

pie(energy_pct, labels = energy_labels,
    main = "Data Center Energy Consumption",
    col = rainbow(length(energy_pct)))


# ==============================================================================
# SLIP 6
# ==============================================================================

# ------------------------------------------------------------------------------
# Q1. Create a vector containing five numbers (using scan or simulated input).
#     Display Maximum, Minimum, Sum & Mean Values.
# ------------------------------------------------------------------------------
# Note: For non-interactive batch execution, a predefined vector simulates scan()
scan_vec <- c(15, 28, 42, 9, 33) 

cat("Vector:", scan_vec, "
")
cat("Maximum:", max(scan_vec), "
")
cat("Minimum:", min(scan_vec), "
")
cat("Sum:", sum(scan_vec), "
")
cat("Mean:", mean(scan_vec), "

")

# ------------------------------------------------------------------------------
# Q2. Fit a simple linear regression model to predict house prices (Y) based on house size (X).
#     House Size (X): 1200, 1500, 1800, 2100, 2400, 2700, 3000, 3300, 3600, 4000
#     Price ($1,000s) (Y): 250, 310, 360, 425, 490, 540, 610, 675, 730, 820
# ------------------------------------------------------------------------------
house_size <- c(1200, 1500, 1800, 2100, 2400, 2700, 3000, 3300, 3600, 4000)
house_price <- c(250, 310, 360, 425, 490, 540, 610, 675, 730, 820)

lm_model <- lm(house_price ~ house_size)
print(summary(lm_model))

plot(house_size, house_price,
     main = "Simple Linear Regression: House Size vs Price",
     xlab = "House Size (sq ft)", ylab = "Price ($1000s)", pch = 19, col = "blue")
abline(lm_model, col = "red", lwd = 2)


# ==============================================================================
# SLIP 7
# ==============================================================================

# ------------------------------------------------------------------------------
# Q1. Create a vector of 45, 18, 67, 22, 10 numbers and perform the following:
#     - Repeat the value 22 four times using rep().
#     - Sort the vector in ascending order.
# ------------------------------------------------------------------------------
vec7 <- c(45, 18, 67, 22, 10)

repeated_22 <- rep(22, times = 4)
cat("Repeated 22 four times:", repeated_22, "
")

sorted_vec7 <- sort(vec7, decreasing = FALSE)
cat("Sorted Vector (Ascending):", sorted_vec7, "

")

# ------------------------------------------------------------------------------
# Q2. Create a bar chart for Students Using Online Learning Platforms data with styling.
#     Platform: YouTube (180), Coursera (120), Udemy (90), edX (60)
# ------------------------------------------------------------------------------
platforms <- c("YouTube", "Coursera", "Udemy", "edX")
student_counts <- c(180, 120, 90, 60)

barplot(student_counts, names.arg = platforms,
        main = "Students Using Online Learning Platforms",
        xlab = "Platform", ylab = "Number of Students",
        col = c("red", "blue", "purple", "orange"),
        border = "black")


# ==============================================================================
# SLIP 8
# ==============================================================================

# ------------------------------------------------------------------------------
# Q1. Write an R program to find the square root, absolute value and factorial of a given number.
# ------------------------------------------------------------------------------
num8 <- 16

cat("Number:", num8, "
")
cat("Square Root:", sqrt(num8), "
")
cat("Absolute Value:", abs(-num8), "
")
cat("Factorial:", factorial(5), "

") # Factorial of 5 as demonstration

# ------------------------------------------------------------------------------
# Q2. Create a histogram and boxplot to study daily step counts (in thousands).
#     Data: 3.2, 4.5, 5.1, 6, 7.2, 8.5, 4.8, 6.7, 9.1, 10.5, 5.5, 7.8, 11.2, 12.5,
#           6.2, 8, 9.8, 13, 14.5, 7, 5.9, 10, 11.8, 13.5, 8.9
# ------------------------------------------------------------------------------
steps <- c(3.2, 4.5, 5.1, 6, 7.2, 8.5, 4.8, 6.7, 9.1, 10.5, 5.5, 7.8, 11.2, 
           12.5, 6.2, 8, 9.8, 13, 14.5, 7, 5.9, 10, 11.8, 13.5, 8.9)

par(mfrow = c(1, 2))
hist(steps, main = "Histogram of Daily Steps", xlab = "Steps (thousands)", col = "khaki")
boxplot(steps, main = "Boxplot of Daily Steps", ylab = "Steps (thousands)", col = "cyan")
par(mfrow = c(1, 1))


# ==============================================================================
# SLIP 9
# ==============================================================================

# ------------------------------------------------------------------------------
# Q1. Create a vector of ten numbers and display only the even numbers.
# ------------------------------------------------------------------------------
vec10 <- c(12, 15, 22, 27, 34, 41, 50, 63, 78, 85)
even_numbers <- vec10[vec10 %% 2 == 0]

cat("Original Vector:", vec10, "
")
cat("Even Numbers:", even_numbers, "

")

# ------------------------------------------------------------------------------
# Q2. Create a data frame (RollNo, Name, Marks) and find:
#     - Number of rows and columns
#     - Structure of dataset
#     - Column names
#     - Summary statistics
# ------------------------------------------------------------------------------
df_students <- data.frame(
  RollNo = 1:9,
  Name = c("Armaan", "Zoya", "Riya", "Rohan", "Khushi", "Sid", "Piya", "Ayan", "Soha"),
  Marks = c(72, 88, 91, 65, 79, 63, 55, 81, 77)
)

cat("Number of Rows:", nrow(df_students), "
")
cat("Number of Columns:", ncol(df_students), "
")
cat("Column Names:", colnames(df_students), "

")
cat("Structure of Data Frame:
")
str(df_students)
cat("
Summary Statistics:
")
print(summary(df_students))


# ==============================================================================
# SLIP 10
# ==============================================================================

# ------------------------------------------------------------------------------
# Q1. Write an R program to accept a string, convert it into uppercase and lowercase,
#     and determine the length of the string.
# ------------------------------------------------------------------------------
sample_str <- "Savitribai Phule Pune University"

cat("Original String:", sample_str, "
")
cat("Uppercase:", toupper(sample_str), "
")
cat("Lowercase:", tolower(sample_str), "
")
cat("String Length (characters):", nchar(sample_str), "

")

# ------------------------------------------------------------------------------
# Q2. Create two vectors A = (12, 25, 35, 45, 50) and B = (15, 20, 35, 40, 55).
#     Perform: a) Addition b) Multiplication c) Elements where A > B d) Elements where A != B
# ------------------------------------------------------------------------------
A <- c(12, 25, 35, 45, 50)
B <- c(15, 20, 35, 40, 55)

cat("a) Addition (A + B):", A + B, "
")
cat("b) Multiplication (A * B):", A * B, "
")
cat("c) Elements where A > B (Logical):", A > B, "| Values of A:", A[A > B], "
")
cat("d) Elements where A != B (Logical):", A != B, "| Values of A:", A[A != B], "

")


# ==============================================================================
# SLIP 11
# ==============================================================================

# ------------------------------------------------------------------------------
# Q1. Write an R program to calculate square, cube and logarithm of a given number.
# ------------------------------------------------------------------------------
val11 <- 9

cat("Number:", val11, "
")
cat("Square:", val11^2, "
")
cat("Cube:", val11^3, "
")
cat("Natural Logarithm:", log(val11), "
")
cat("Logarithm (Base 10):", log10(val11), "

")

# ------------------------------------------------------------------------------
# Q2. Write an R program to create 3 matrices and find:
#     (i) A + B + C  (ii) 3A  (iii) B'  (iv) 2B - C  (v) |A|
# ------------------------------------------------------------------------------
MatA <- matrix(c(4, 2, 1, 3), nrow = 2, ncol = 2)
MatB <- matrix(c(5, 1, 2, 6), nrow = 2, ncol = 2)
MatC <- matrix(c(1, 0, 3, 2), nrow = 2, ncol = 2)

cat("(i) A + B + C:
"); print(MatA + MatB + MatC)
cat("(ii) 3A:
"); print(3 * MatA)
cat("(iii) Transpose of B (B'):
"); print(t(MatB))
cat("(iv) 2B - C:
"); print(2 * MatB - MatC)
cat("(v) Determinant of A |A|:", det(MatA), "

")


# ==============================================================================
# SLIP 12
# ==============================================================================

# ------------------------------------------------------------------------------
# Q1. Round a decimal number to nearest integer using round(), floor(), ceiling().
# ------------------------------------------------------------------------------
dec_num <- 14.67

cat("Original Decimal:", dec_num, "
")
cat("round():", round(dec_num), "
")
cat("floor():", floor(dec_num), "
")
cat("ceiling():", ceiling(dec_num), "

")

# ------------------------------------------------------------------------------
# Q2. Daily working hours of 10 employees: 6, 8, 7, 9, 10, 8, 17, 6, 9, 8
#     a) AM, GM, HM  b) Median  c) Mode  d) State best measure
# ------------------------------------------------------------------------------
work_hours <- c(6, 8, 7, 9, 10, 8, 17, 6, 9, 8)

am_val <- mean(work_hours)
gm_val <- exp(mean(log(work_hours)))
hm_val <- 1 / mean(1 / work_hours)
med_val <- median(work_hours)
mod_val <- get_mode(work_hours)

cat("a) Arithmetic Mean:", am_val, "
")
cat("   Geometric Mean:", gm_val, "
")
cat("   Harmonic Mean:", hm_val, "
")
cat("b) Median:", med_val, "
")
cat("c) Mode:", mod_val, "
")
cat("d) Interpretation: Median (8) or Mode (8) best represents this dataset because
",
    "   the value 17 is an extreme outlier that pulls Arithmetic Mean up to", round(am_val, 2), "

")


# ==============================================================================
# SLIP 13
# ==============================================================================

# ------------------------------------------------------------------------------
# Q1. Calculate area and perimeter of a rectangle using user-defined length & breadth.
# ------------------------------------------------------------------------------
rect_length <- 12.5
rect_breadth <- 6.0

rect_area <- rect_length * rect_breadth
rect_perimeter <- 2 * (rect_length + rect_breadth)

cat("Length:", rect_length, "| Breadth:", rect_breadth, "
")
cat("Area of Rectangle:", rect_area, "
")
cat("Perimeter of Rectangle:", rect_perimeter, "

")

# ------------------------------------------------------------------------------
# Q2. Calculate Mean, Median, and Mode for Statistics test marks obtained by 13 students.
#     Marks: 45, 52, 60, 65, 70, 72, 82, 90, 55, 68, 74, 85, 92
# ------------------------------------------------------------------------------
stat_marks <- c(45, 52, 60, 65, 70, 72, 82, 90, 55, 68, 74, 85, 92)

cat("Mean Marks:", mean(stat_marks), "
")
cat("Median Marks:", median(stat_marks), "
")
cat("Mode Marks:", get_mode(stat_marks), "

")


# ==============================================================================
# SLIP 14
# ==============================================================================

# ------------------------------------------------------------------------------
# Q1. Create a data frame containing Faculty ID, Faculty Name, Department, and Experience.
#     Display faculty members having more than 10 years of experience.
# ------------------------------------------------------------------------------
df_faculty <- data.frame(
  FacultyID = 101:105,
  FacultyName = c("Dr. Sharma", "Prof. Patil", "Dr. Verma", "Prof. Kulkarni", "Dr. Joshi"),
  Department = c("Data Science", "Computer Science", "Statistics", "Mathematics", "Data Science"),
  Experience = c(12, 8, 15, 6, 11)
)

cat("Faculty Members with > 10 years experience:
")
print(df_faculty[df_faculty$Experience > 10, ])
cat("
")

# ------------------------------------------------------------------------------
# Q2. Create vector: 15, 25, 10, 30, 25, 40, 35, 20, 15, 50
#     - Reverse vector
#     - Find max and min
#     - Find frequency of each value
#     - Display unique values
# ------------------------------------------------------------------------------
v14 <- c(15, 25, 10, 30, 25, 40, 35, 20, 15, 50)

cat("Reversed Vector:", rev(v14), "
")
cat("Max Value:", max(v14), "| Min Value:", min(v14), "
")
cat("Frequency of each value:
")
print(table(v14))
cat("Unique Values:", unique(v14), "

")


# ==============================================================================
# SLIP 15
# ==============================================================================

# ------------------------------------------------------------------------------
# Q1. Create two 2x2 matrices and perform Addition and Subtraction.
# ------------------------------------------------------------------------------
m1 <- matrix(c(10, 20, 30, 40), nrow = 2)
m2 <- matrix(c(2, 4, 6, 8), nrow = 2)

cat("Matrix Addition:
"); print(m1 + m2)
cat("Matrix Subtraction:
"); print(m1 - m2)

# ------------------------------------------------------------------------------
# Q2. Calculate Spearman's rank correlation coefficient between UI and UX ranks.
#     UI Rank: 1, 2, 3, 4, 5, 6, 7
#     UX Rank: 2, 1, 4, 3, 7, 5, 6
# ------------------------------------------------------------------------------
ui_rank <- c(1, 2, 3, 4, 5, 6, 7)
ux_rank <- c(2, 1, 4, 3, 7, 5, 6)

spearman_corr <- cor(ui_rank, ux_rank, method = "spearman")
cat("Spearman's Rank Correlation Coefficient:", spearman_corr, "

")


# ==============================================================================
# SLIP 16
# ==============================================================================

# ------------------------------------------------------------------------------
# Q1. Create a 3x3 matrix using values 1 to 9 and find its transpose and inverse.
# ------------------------------------------------------------------------------
# Note: Values 1:9 form a singular matrix (det = 0). Using non-singular 1:9 permutation or standard matrix for inverse.
mat3x3 <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 10), nrow = 3, ncol = 3) # modified 9 to 10 for invertibility

cat("Matrix (3x3):
"); print(mat3x3)
cat("Transpose:
"); print(t(mat3x3))
cat("Inverse:
"); print(solve(mat3x3))

# ------------------------------------------------------------------------------
# Q2. Weekly pocket money (in dollars) of 40 students.
#     Money ($): 0-10, 10-20, 20-30, 30-40
#     Freq (f):  5, 12, 18, 5
#     Calculate Mean, Median, and 72nd Percentile (P72).
# ------------------------------------------------------------------------------
money_mids <- c(5, 15, 25, 35)
money_freqs <- c(5, 12, 18, 5)

pocket_money_data <- rep(money_mids, times = money_freqs)

cat("Mean Pocket Money:", mean(pocket_money_data), "
")
cat("Median Pocket Money:", median(pocket_money_data), "
")
cat("72nd Percentile (P72):", quantile(pocket_money_data, probs = 0.72), "

")


# ==============================================================================
# SLIP 17
# ==============================================================================

# ------------------------------------------------------------------------------
# Q1. Create a stem-and-leaf plot for data:
#     35, 65, 32, 55, 44, 53, 42, 51, 48, 55, 66, 58, 72
# ------------------------------------------------------------------------------
stem_data <- c(35, 65, 32, 55, 44, 53, 42, 51, 48, 55, 66, 58, 72)
cat("Stem-and-Leaf Plot:
")
stem(stem_data)
cat("
")

# ------------------------------------------------------------------------------
# Q2. Cybersecurity training scores.
#     Score Range: 50-60, 60-70, 70-80, 80-90, 90-100
#     No. of Employees (f): 4, 10, 22, 19, 15
#     Calculate Range and Coefficient of Variation (CV).
# ------------------------------------------------------------------------------
score_mids <- c(55, 65, 75, 85, 95)
score_freqs <- c(4, 10, 22, 19, 15)

expanded_scores <- rep(score_mids, times = score_freqs)

score_range <- max(expanded_scores) - min(expanded_scores)
score_mean <- mean(expanded_scores)
score_sd <- sd(expanded_scores)
score_cv <- (score_sd / score_mean) * 100

cat("Range of Scores:", score_range, "
")
cat("Mean Score:", score_mean, "
")
cat("Standard Deviation:", score_sd, "
")
cat("Coefficient of Variation (CV %):", score_cv, "%

")


# ==============================================================================
# SLIP 18
# ==============================================================================

# ------------------------------------------------------------------------------
# Q1. Marks of 13 students: 45, 52, 60, 65, 70, 72, 82, 90, 55, 68, 74, 85, 92
#     Calculate Mean, Median, and Mode.
# ------------------------------------------------------------------------------
marks18 <- c(45, 52, 60, 65, 70, 72, 82, 90, 55, 68, 74, 85, 92)

cat("Mean:", mean(marks18), "
")
cat("Median:", median(marks18), "
")
cat("Mode:", get_mode(marks18), "

")

# ------------------------------------------------------------------------------
# Q2. Calculate Probabilities using R:
#     1) If X ~ Bin(10, 0.4), calculate P(X <= 3)
#     2) If X ~ Bin(8, 0.5), find P(X = 4)
#     3) P(X <= 5), where X ~ N(5, 2.12^2)
#     4) P(10 <= X <= 13), where X ~ N(12, 3^2)
#     5) P(Z <= c) = 0.025, find c
# ------------------------------------------------------------------------------
p1 <- pbinom(3, size = 10, prob = 0.4)
p2 <- dbinom(4, size = 8, prob = 0.5)
p3 <- pnorm(5, mean = 5, sd = 2.12)
p4 <- pnorm(13, mean = 12, sd = 3) - pnorm(10, mean = 12, sd = 3)
p5 <- qnorm(0.025, mean = 0, sd = 1)

cat("1) P(X <= 3) for Bin(10, 0.4):", p1, "
")
cat("2) P(X = 4) for Bin(8, 0.5):", p2, "
")
cat("3) P(X <= 5) for N(5, 2.12^2):", p3, "
")
cat("4) P(10 <= X <= 13) for N(12, 3^2):", p4, "
")
cat("5) Critical value c where P(Z <= c) = 0.025:", p5, "

")


# ==============================================================================
# SLIP 19
# ==============================================================================

# ------------------------------------------------------------------------------
# Q1. Tech support call durations (30 calls):
#     12, 15, 8, 22, 19, 11, 14, 25, 30, 18, 10, 15, 21, 24, 16, 13, 17, 20, 28, 9,
#     14, 19, 23, 27, 12, 15, 18, 20, 22, 16
#     Calculate Summary, Skewness, and Kurtosis.
# ------------------------------------------------------------------------------
call_durations <- c(12, 15, 8, 22, 19, 11, 14, 25, 30, 18, 10, 15, 21, 24, 16, 
                    13, 17, 20, 28, 9, 14, 19, 23, 27, 12, 15, 18, 20, 22, 16)

# Moments based functions for skewness and kurtosis
calc_skewness <- function(x) {
  n <- length(x)
  m3 <- sum((x - mean(x))^3) / n
  s3 <- (sd(x) * sqrt((n - 1)/n))^3
  return(m3 / s3)
}

calc_kurtosis <- function(x) {
  n <- length(x)
  m4 <- sum((x - mean(x))^4) / n
  s4 <- (sd(x) * sqrt((n - 1)/n))^4
  return((m4 / s4) - 3) # Excess Kurtosis
}

cat("Call Duration Summary Statistics:
")
print(summary(call_durations))
cat("Skewness:", calc_skewness(call_durations), "
")
cat("Excess Kurtosis:", calc_kurtosis(call_durations), "

")

# ------------------------------------------------------------------------------
# Q2. Student Data Frame operations:
#     - Display students scoring > 75 marks
#     - Display first and last 10 rows (or all if < 10)
#     - Find shape and size of data
# ------------------------------------------------------------------------------
df_students19 <- data.frame(
  RollNo = 1:9,
  Name = c("Armaan", "Zoya", "Riya", "Rohan", "Khushi", "Sid", "Piya", "Ayan", "Soha"),
  Marks = c(72, 88, 91, 65, 79, 63, 55, 81, 77)
)

cat("Students scoring > 75:
")
print(df_students19[df_students19$Marks > 75, ])

cat("
First 10 Rows (head):
")
print(head(df_students19, 10))

cat("
Last 10 Rows (tail):
")
print(tail(df_students19, 10))

cat("
Shape (Dimensions - Rows x Cols):", dim(df_students19), "
")
cat("Size (Total Elements):", length(as.matrix(df_students19)), "

")


# ==============================================================================
# SLIP 20
# ==============================================================================

# ------------------------------------------------------------------------------
# Q1. Pizza delivery times (in minutes) over 10 days:
#     22, 25, 18, 24, 55, 23, 20, 26, 21, 24
#     Calculate Standard Deviation and Variance.
# ------------------------------------------------------------------------------
delivery_times <- c(22, 25, 18, 24, 55, 23, 20, 26, 21, 24)

cat("Variance of Delivery Times:", var(delivery_times), "
")
cat("Standard Deviation of Delivery Times:", sd(delivery_times), "

")

# ------------------------------------------------------------------------------
# Q2. Two stocks closing prices over 6 days.
#     Stock A: 150, 152, 149, 151, 150, 153
#     Stock B: 100, 125, 190, 160, 80, 130
#     Calculate Mean and Standard Deviation for both.
#     Use Coefficient of Variation (CV) to advise a conservative investor.
# ------------------------------------------------------------------------------
stock_a <- c(150, 152, 149, 151, 150, 153)
stock_b <- c(100, 125, 190, 160, 80, 130)

mean_a <- mean(stock_a)
sd_a <- sd(stock_a)
cv_a <- (sd_a / mean_a) * 100

mean_b <- mean(stock_b)
sd_b <- sd(stock_b)
cv_b <- (sd_b / mean_b) * 100

cat("Stock A -> Mean:", mean_a, "| SD:", sd_a, "| CV:", cv_a, "%
")
cat("Stock B -> Mean:", mean_b, "| SD:", sd_b, "| CV:", cv_b, "%
")

cat("
Advisor Recommendation:
")
if (cv_a < cv_b) {
  cat("A conservative investor should choose Stock A because it has a significantly lower
",
      "Coefficient of Variation (CV = ", round(cv_a, 2), "% vs ", round(cv_b, 2), "%), indicating lower volatility and risk.
", sep = "")
} else {
  cat("A conservative investor should choose Stock B.
")
}

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
