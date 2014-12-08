library(plyr)

# load data
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", 'tmp.zip')
data <- read.csv(unzip('tmp.zip'))

# get a copy without NA's
data_full <- data[complete.cases(data), ]

# total steps per day (group by date)
daily_steps <- ddply(data_full, .(date), summarize, steps = sum(steps))

# historgram of daily steps
hist(
    daily_steps$steps, 
    xlab='total daily steps', 
    main='Total Daily Steps'
  )

# mean and median of daily steps
mean(daily_steps$steps)
median(daily_steps$steps)

# total steps per interval, from all dates (group by interval)
interval_steps <- ddply(data_full, .(interval), summarize, steps = sum(steps))

# timeseries of steps per interval
plot(
    interval_steps$interval, 
    interval_steps$steps, 
    type='l',
    xlab='Day Interval (5-min increments)',
    ylab='Number of Steps',
    main='Average Daily Activity Pattern'
  )

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps
most_steps <- max(interval_steps$steps) # interval with most steps across all days
interval_steps[interval_steps$steps == most_steps, ]

# number of missing values
sum(is.na(data))

# set the missing 'steps' values to be the median for that interval

