library(dplyr)

# Sample data
df <- data.frame(LeftColumn = c("1", NA, "3", NA, "5"),
                 RightColumn = c("A", "B", "C", "D", "E"))

# Coalesce operation
df$CombinedColumn <- coalesce(df$LeftColumn, df$RightColumn)

# Print the updated dataframe
print(df)