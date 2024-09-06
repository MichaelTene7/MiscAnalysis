# Load the libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)

# Load  data
data <- read.csv("Data/data_dental_modern_public_10.csv")

traits <- data %>%
  dplyr::select(HYP, FCT_OL, FCT_OT, FCT_CM, FCT_HYP_HOD, FCT_BRA_HOD, FCT_AL, FCT_MES_HOD, FCT_SF)

# Function to perform Fisher's Exact Test
fisher_test_pairwise <- function(trait1, trait2) {
  contingency_table <- table(trait1, trait2)
  print(contingency_table)
  fisher.test(contingency_table)$p.value
}

# Initialize  matrix to store p-values
p_value_matrix <- matrix(1, nrow=ncol(traits), ncol=ncol(traits))
rownames(p_value_matrix) <- colnames(traits)
colnames(p_value_matrix) <- colnames(traits)

# Compute p-values for each pair of traits
for (i in 1:ncol(traits)) {
  for (j in i:ncol(traits)) {
    p_value_matrix[i, j] <- fisher_test_pairwise(traits[, i], traits[, j])
    p_value_matrix[j, i] <- p_value_matrix[i, j]
  }
}

# Convert p-value matrix to a long format for ggplot
p_value_melt <- melt(p_value_matrix)
colnames(p_value_melt) <- c("Trait1", "Trait2", "P_Value")

# Plot heatmap with a pinkish color gradient and differentiate NA values
ggplot(p_value_melt, aes(Trait1, Trait2, fill = -log10(P_Value))) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "deeppink", na.value = "gray") +
  theme_minimal() +
  labs(title = "Pairwise Fisher's Exact Test P-Values", fill = "-log10(P_Value)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.title = element_blank())

