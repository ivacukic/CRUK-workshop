### CRUK SESSION ###

# Create a folder for today's session
# Download files from Github

### Read in data ###

# Read in data using a file picker
data <- read.csv(file.choose())

# Inspect the data set
head(data)
names(data)
View(data)

### Variable types ###

# Is the variable categorical or numeric?
is.factor(data$sex)
is.numeric(data$age)
is.numeric(data$mmse)

# Convert sex to a factor
data$sex <- as.factor(data$sex)

### Descriptive summaries ###

# How many cases per category?
table(data$sex)          # 1 == Men, 2 == Women
table(data$disability)  # 0 == absent, 1 == present
table(data$sex, data$disability)

# Summary statistics for continuous variables
summary(data$age)
summary(data$mmse)
summary(data$ces.d)

### Basic manipulation ###

# Rename all variables
names(data) <- c("id","sex","age", "cognition",
                 "depression", "disability")
names(data)

# Rename levels of a factor
data$sex <- factor(data$sex,
                   levels = c(1, 2),
                   labels = c("Male", "Female"))

levels(data$sex)
table(data$sex)

### Subsetting data ###

# Select only data for men
men <- subset(data, data$sex == "Male")

# DIY task:
# Select only data for women
women <- subset(data, data$sex == "Female")

# What is the age range?
summary(data$age)

### Missing values ###

# Change the odd value into NA
summary(data$age)

data$age[data$age == 219] <- NA

summary(data$age)

# How many NAs in cognition and depression variables?
summary(data$cognition)
summary(data$depression)

# What is the range for cognition and depression scores?
summary(data$cognition)
summary(data$depression)

### Histograms ###

# Check distributions for continuous variables
hist(data$age)

hist(data$depression)

### Creating categories ###

# Greater or equal to 16 = clinical depression
data$dep_cat <- NA

data$dep_cat[(data$depression >= 16)] <- 1
data$dep_cat[(data$depression < 16)] <- 0

table(data$dep_cat)

# DIY:
# Make a categorical variable for cognition according to
# clinical cut-off scores:
# 24-30 no impairment
# 18-23 mild impairment
# 0-17 severe impairment
# Add labels

data$cog_cat <- NA

data$cog_cat[(data$cognition <= 17)] <- 3
data$cog_cat[(data$cognition > 17 & data$cognition <= 23)] <- 2
data$cog_cat[(data$cognition > 23)] <- 1

table(data$cog_cat)

data$cog_cat <- factor(data$cog_cat,
                        levels = c(1, 2, 3),
                        labels = c("No impairment",
                                   "Mild impairment",
                                   "Severe impairment"))

table(data$cog_cat)

### Scatterplots ###

# Scatterplot of cognition and depression
plot(data$cognition, data$depression)

# Add labels to axes
plot(data$cognition,
     data$depression,
     xlab = "Cognition",
     ylab = "Depression")

### Exporting data ###

# Export a CSV containing new variables
write.csv(data, file = "new_data.csv")

### Correlation coefficient ###

# Basic correlation test
cor.test(data$cognition, data$depression)

# Create the plot
plot(data$cognition,
     data$depression,
     xlab = "Cognition",
     ylab = "Depression")
