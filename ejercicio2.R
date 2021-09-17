set.seed(10)

# Paragraph "a"
data <- rpois(1000000, lambda = 4)
data

# Function that will help sections "b" and "c"
# sample_size: Sample size
# repetitions: Process repetitions
generate_hist <- function(sample_size, repetitions = 1000) {
  counter <- 0
  results <- c()
  tests <- c()
  
  while(counter < repetitions) {
    counter <- counter + 1
    aleatory <- sample(data, sample_size, replace = TRUE)
    result <- mean(aleatory, na.rm = FALSE) * sqrt(20)
    results <- c(results, result)
  }
  
  hist(results, freq = FALSE)
  return(density(results))
}

# Paragraph "b"
generate_hist(20)

# Paragraph "c"
for_100 = generate_hist(100)
plot(for_100)

for_1000 = generate_hist(1000)
plot(for_1000)

for_10000 = generate_hist(10000)
plot(for_10000)

for_100000 = generate_hist(100000)
plot(for_100000)