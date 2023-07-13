# Example dataset
data <- data.frame(
  Sensor = c("Sensor1", "Sensor1", "Sensor2", "Sensor3", "Sensor2", "Sensor1", "Sensor3", "Sensor3"),
  Date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-02", "2023-01-03", "2023-01-04", "2023-01-04", "2023-01-05", "2023-01-05"))
)

# Determine the start and end dates of the study period
start_date <- min(data$Date)
end_date <- max(data$Date)

# Create a sequence of dates from start to end
date_sequence <- seq(start_date, end_date, by = "day")

# Create the binary matrix
trigger_matrix <- matrix(0, nrow = length(unique(data$Sensor)), ncol = length(date_sequence),
                         dimnames = list(unique(data$Sensor), date_sequence))

# Set the values in the matrix to 1 for triggered days
for (i in seq_along(data$Sensor)) {
  sensor <- data$Sensor[i]
  date <- data$Date[i]
  rindex<- which(rownames(trigger_matrix) == sensor)
  cindex<- which(date_sequence == date)
  trigger_matrix[rindex, cindex] <- 1
}


