is_prime <- function(n) {
  if (n < 2) return(FALSE)
  if (n == 2) return(TRUE)
  if (n %% 2 == 0) return(FALSE)
  for (i in 3:sqrt(n)) {
    if (n %% i == 0) return(FALSE)
  }
  return(TRUE)
}

extract_primes <- function(num) {
  digits <- as.numeric(unlist(strsplit(as.character(num), "")))
  prime_digits <- digits[sapply(digits, is_prime)]
  return(prime_digits)
}

# Example usage:
num <- 123344977
extract_primes(num)

