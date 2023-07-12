# Example dataset
dataset <- data.frame(
  Sensor = c("Sensor1", "Sensor1", "Sensor2", "Sensor3", "Sensor2"),
  Date = c("2023-01-01", "2023-01-01", "2023-01-02", "2023-01-03", "2023-01-02")
)

# Define study start and end dates
start_date <- as.Date("2023-01-01")
end_date <- as.Date("2023-04-30")

# Create sequence of dates
dates <- seq(start_date, end_date, by = "day")

# Create empty matrix
sensor_matrix <- matrix(0, nrow = length(unique(dataset$Sensor)), ncol = length(dates),
                        dimnames = list(unique(dataset$Sensor), dates))
rnames<- rownames(dataset)
cnames<- colnames(dataset)

# Update matrix with triggered values
sensor_matrix[dataset$Sensor, dataset$Date] <- 1




sensor_matrix

#((((((((((((((((((((((((((((((((((((((((()))))))))))))))))))))))))))))))))))))))))
# Create an empty matrix with rows for each sensor and columns for each day
sensor_matrix <- matrix(0, nrow = length(unique(dataset$Sensor)), ncol = length(unique(dataset$Date)))

# Get the indices of sensors and dates in the matrix
sensor_indices <- match(dataset$Sensor, unique(dataset$Sensor))
date_indices <- match(dataset$Date, unique(dataset$Date))

# Assign 1 to the corresponding cells in the matrix
sensor_matrix[cbind(sensor_indices, date_indices)] <- 1


