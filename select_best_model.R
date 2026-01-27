# scripts/select_best_model.R

# This script demonstrates how to perform model selection using the survey data.
# It assumes the working directory is the project root.

# Define the data file path
data_file <- "2019 English R Community Survey Responses.txt"

# Check if file exists
if (!file.exists(data_file)) {
  # Try looking one level up if running from scripts directory
  if (file.exists(file.path("..", data_file))) {
    data_file <- file.path("..", data_file)
  } else {
    stop("Data file not found. Please ensure you are in the project root or the file exists.")
  }
}

# Read the data
survey_data <- read.csv(data_file, check.names = FALSE, stringsAsFactors = FALSE)

# Function to clean column names for easier usage
clean_names <- function(names) {
  names <- gsub("[[:punct:]]", "_", names) # Replace punctuation with underscores
  names <- gsub("\\s+", "_", names)        # Replace spaces with underscores
  names <- gsub("_+", "_", names)          # Remove duplicate underscores
  names <- gsub("^_|_$", "", names)        # Remove leading/trailing underscores
  return(names)
}

colnames(survey_data) <- clean_names(colnames(survey_data))

# Select variables for the model
# Target: Likelihood to recommend R (0-10 scale)
# Predictors: Experience level, Enjoyment of R

# Prepare the data frame for modeling
# We convert the Likelihood and Enjoyment to numeric, and Experience to a factor
model_df <- data.frame(
  Recommend = as.numeric(survey_data$How_likely_are_you_to_recommend_R_to_a_colleague_friend_or_family_member),
  Experience = as.factor(survey_data$How_would_you_rate_your_level_of_experience_using_R),
  Enjoyment = as.numeric(survey_data$Please_rate_how_much_you_enjoy_using_R_on_a_scale_of_1_to_5_where_1_is_you_don_t_enjoy_it_at_all_and_5_is_that_you_enjoy_it_a_great_deal)
)

# Remove rows with missing values to ensure the model can fit
model_df <- na.omit(model_df)

# Fit a full linear model with all selected predictors
full_model <- lm(Recommend ~ ., data = model_df)

# Perform stepwise model selection to pick the best model based on AIC
# This function will add/remove variables to find the optimal balance
best_model <- step(full_model, direction = "both")

# Display the summary of the selected model
print(summary(best_model))