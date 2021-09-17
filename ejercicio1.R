# Function to determine if a number is prime
# Returns TRUE if it is prime
# Returns FALSE if it is not

primo <- function(x) {
  value <- TRUE
  
  if(x < 2) value <- FALSE
  if(x > 2) {
    end <- FALSE
    
    divider <- 2
    half <- floor(x / 2)
    
    while(!end) {
      if(divider < half) {
        r <- x %% divider
        
        if(r == 0) {
          end <- TRUE
          value <- FALSE
        }
        else divider <- divider + 1
      }
      else end <- TRUE
    }
  }
  
  return(value)
}

# Function that finds the greatest prime factor of a number

maxprimo <- function(x) {
  if(x < 0) x <- x * (-1)
  if(x <= 1) return('This number has no prime factors.')
  
  end <- FALSE
  
  min_divider <- 2
  max_divider <- x
  
  half <- floor(x / 2)
  
  while(!end) {
    if(min_divider > half) {
      end <- TRUE
      break
    }
    
    r <- max_divider %% min_divider
    
    if(r > 0) min_divider <- min_divider + 1
    else {
      max_divider <- max_divider / min_divider
      is_prime <- primo(max_divider)
      
      if(is_prime) end <- TRUE
    }
  }
  
  paste('The largest prime factor is:', max_divider, sep = ' ')
}

maxprimo(7134719293497)

# Value calculated in approximately 8 seconds