# Load necessary libraries
library(readr)
library(dplyr)
library(stringr)

# Load the dataset with the correct delimiter using read_delim
df <- read_delim("C:/Users/rikis/OneDrive - Politecnico di Milano/Desktop/HPC/2.ES_Semestre/2.ES_MULTIVARIATE ANALYSIS/HW_1/euroleague_23_24.csv", delim = ";", col_types = cols(.default = "c"))

# Now check the column names
colnames(df)

# Dropping NO Column
df <- df[ , !(colnames(df) %in% "No")]

#3 Splitting the "Min" Column

# Split the "Min" column by colon and extract hours and minutes
df$Min_split <- str_split(df$Min, ":", simplify = TRUE)

# Convert the time to total minutes
df$Min_total <- as.numeric(df$Min_split[,1]) * 60 + as.numeric(df$Min_split[,2])

# 4. Creating "Min 2"
# Create the "Min 2" column as average minutes per game
df$Min_2 <- as.numeric(df$Min_total) / as.numeric(df$GP)




#5. Converting Variables to Appropriate Types
# Function to check for non-numeric values in each column
check_numeric_conversion <- function(column) {
  as.numeric(column)  # Attempt to convert to numeric
}

# Remove percentage signs (%) and replace commas with dots for decimal points
df[numeric_cols] <- df[numeric_cols] %>% 
  lapply(function(x) gsub("%", "", x)) %>%   # Remove '%' if present
  lapply(function(x) gsub(",", ".", x))      # Replace commas with dots for decimals

# Now convert them to numeric
df[numeric_cols] <- lapply(df[numeric_cols], as.numeric)

# Check for NA values in numeric columns
sapply(df[numeric_cols], function(x) sum(is.na(x)))

# Check the structure of the dataset
str(df)

# Get numeric columns
numeric_columns <- names(df)[sapply(df, is.numeric)]
print(numeric_columns)

# Get categorical columns (including factors)
categorical_columns <- names(df)[sapply(df, is.factor)]
print(categorical_columns)


#============================================================================
#####  PCA
# Install the FactoMineR and factoextra packages if you don't have them
install.packages("FactoMineR")
install.packages("factoextra")

# Step 1: Select the numerical variables, excluding 'PIR' for now
numerical_vars <- df[, sapply(df, is.numeric) & names(df) != "PIR"]

# Scale the numerical variables
scaled_data <- scale(numerical_vars)

# Step 2: Keep the qualitative variables (TEAM, POSITION) and PIR (quantitative supplementary)
qualitative_vars <- df[, c("TEAM", "POSITION")]
pir <- df[, "PIR"]

# Combine the scaled numerical data with the qualitative and PIR variables
combined_data <- cbind(scaled_data, qualitative_vars, pir)


# Perform PCA, treating "TEAM" and "POSITION" as supplementary qualitative variables,
# and "PIR" as a supplementary quantitative variable
pca_result <- PCA(combined_data, 
                  quali.sup = which(names(combined_data) %in% c("TEAM", "POSITION")), 
                  quanti.sup = which(names(combined_data) == "PIR"), 
                  graph = FALSE)


# Plot individuals (e.g., players), with grouping by "TEAM"
plot.PCA(pca_result, choix = "ind", axes = c(1, 2), habillage = "TEAM", addEllipses = TRUE)

# Plot individuals (e.g., players), with grouping by "TEAM"
plot.PCA(pca_result, choix = "ind", axes = c(1, 2), habillage = "POSITION", addEllipses = TRUE)


# Plot variables (correlation circle)
plot.PCA(pca_result, choix = "var", axes = c(1, 2))





# Load the factoextra library
library(factoextra)

# Assuming 'pca_result' is the result of your PCA analysis
# Scree plot to visualize the eigenvalues
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 60))

# Plot the scree plot (eigenvalues) manually
plot(eigenvalues, type = "b", 
     main = "Scree Plot",
     xlab = "Principal Components", 
     ylab = "Eigenvalue",
     col = "red", pch = 19)

# Get the loadings (correlation between variables and principal components)
loadings <- pca_result$var$coord

# View the loadings
print(loadings)

# Sort variables by their contribution to each principal component
top_loadings_dim1 <- loadings[order(abs(loadings[,1]), decreasing = TRUE), 1]
top_loadings_dim2 <- loadings[order(abs(loadings[,2]), decreasing = TRUE), 2]

# View the top loadings for Dim1 and Dim2
print(top_loadings_dim1)
print(top_loadings_dim2)

### d) Plot Correlations Between Variables and Extracted Dimensions

# 1. Generate variable plot for the first two dimensions (Dim.1 and Dim.2)
plot.PCA(pca_result, choix = "var", axes = c(1, 2))

# 2. Generate variable plot for Dimension 1 vs Dimension 3
plot.PCA(pca_result, choix = "var", axes = c(1, 3))

# 3. Generate variable plot for Dimension 2 vs Dimension 3
plot.PCA(pca_result, choix = "var", axes = c(2, 3))


# 4. To iterate through different pairs of dimensions, use a loop (optional)
for (i in 1:3) {
  for (j in (i+1):4) {
    plot.PCA(pca_result, choix = "var", axes = c(i, j))
  }
}

# f) Show individual pilots for the extracted dimensions changing argumennt choix=“ind” in plot.PCA() function. (2p)

# Assuming 'pca_result' is your PCA result object

# 1. Plot individuals for the first two dimensions (Dim.1 and Dim.2)
plot.PCA(pca_result, choix = "ind", axes = c(1, 2))

# 2. Plot individuals for Dimension 1 vs Dimension 3
plot.PCA(pca_result, choix = "ind", axes = c(1, 3))

# 3. Plot individuals for Dimension 2 vs Dimension 3
plot.PCA(pca_result, choix = "ind", axes = c(2, 3))

# You can also customize the appearance (e.g., adding labels, colors):
# For example, you can color individuals by groups (e.g., team) if you have categorical data
plot.PCA(pca_result, choix = "ind", axes = c(1, 2), habillage = "TEAM", addEllipses = TRUE)

# Check if "TEAM" exists in the dataset
"TEAM" %in% colnames(df)

# Ensure TEAM is a factor
df$TEAM <- as.factor(df$TEAM)

#========================================================================
##### MULTIMENSIONAL SCALING (MDS)

# Assuming 'df' is your original data frame

## a)Apply Metric MDS Using Euclidean Distance on Scaled Numerical Variables 

# Select the numerical variables (excluding categorical and supplementary variables)
numerical_vars <- df[, sapply(df, is.numeric)]

# Scale the numerical variables (standardization)
scaled_data <- scale(numerical_vars)

# Compute the Euclidean distance matrix
distance_matrix <- dist(scaled_data, method = "euclidean")

# Apply metric MDS using cmdscale()
mds_result <- cmdscale(distance_matrix, k = 2)  # k = 2 for 2D projection

# Convert the MDS result into a data frame for plotting
mds_df <- as.data.frame(mds_result)
colnames(mds_df) <- c("Dim1", "Dim2")  # Naming the dimensions

# If you have a TEAM or PLAYER variable in the original data, include it for labeling
mds_df$PLAYER <- df$PLAYER  # Assuming PLAYER column exists
mds_df$TEAM <- df$POSITION      # Assuming TEAM column exists

#b)Plot the Data Using the First Two MDS Coordinates with Player Names as Labels
# Plot the MDS result
library(ggplot2)

ggplot(mds_df, aes(x = Dim1, y = Dim2, label = PLAYER, color = POSITION)) +
  geom_point() +
  geom_text(vjust = 1.5, hjust = 0.5) +
  labs(title = "Metric MDS (2D Projection)", x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()





