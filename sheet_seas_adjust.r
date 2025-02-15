
# Load the necessary libraries
library(readr)
library(rdbnomics)
library(zoo)
library(x12)
library(seasonal)
library(tsm)
library(dplyr)
library(seastests)

# Load the data
df_qwi_filtered <- read.csv("temp_df.csv")

# Identify numeric columns to adjust (excluding 'date')
series <- names(df_qwi_filtered)[sapply(df_qwi_filtered, is.numeric)]
series <- setdiff(series, "date")

# Loop through each numeric column and apply seasonal adjustment
for (col in series) {
    # The data may have NAs at the beginning
    data_vector <- df_qwi_filtered[[col]]
    dates <- as.yearqtr(as.Date(df_qwi_filtered$date)) |>
      (\(dates) data.frame(year = as.integer(format(dates, "%Y")), quarter = as.integer(format(dates, "%q"))))()

    dates <- dates[!is.na(data_vector), ] # only adjust this one to keep same length of series independent of NAs!

    # Create time series
    time_series <- ts(data_vector[!is.na(data_vector)], start = c(dates[1,1], dates[1,2]), frequency = 4)

    # Apply seasonal adjustment using X-13ARIMA-SEATS
    # Try detecting seasonality and applying seasonal adjustment
    tryCatch({
        if (isSeasonal(time_series)) {
            deseasonalized <- final(seas(time_series))

            if (!is.null(deseasonalized)) {
                df_qwi_filtered[!is.na(data_vector), col] <- as.vector(as.numeric(deseasonalized))
            } else {
                print(paste("Non-seasonal series detected for column:", col))
            }
        } else {
            print(paste("Non-seasonal series detected for column:", col))
        }
    }, error = function(e) {
        warning(paste("Error in isSeasonal or seas for column:", col, ":", e$message))
    })
} # Some errors in these loops are expected due to some series being -1 everywhere

# Now that everything is seasonal, let's move things to python again
write.csv(df_qwi_filtered, "qwi_data_seasadj.csv", row.names = FALSE)