# Fungsi untuk menghasilkan semua kombinasi yang mungkin
combinations <- function(n, k) {
  if (k == 0) {
    return(list())
  } else if (k == 1) {
    return(lapply(1:n, function(i) {i}))
  } else {
    result <- list()
    for (i in 1:(n - k + 1)) {
      sub_combinations <- combinations(n - i, k - 1)
      for (j in 1:length(sub_combinations)) {
        result <- c(result, list(c(i, sub_combinations[[j]])))
      }
    }
    return(result)
  }
}

# Daftar elemen
elements <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13")

# Generate kombinasi untuk setiap jumlah anggota
for (i in 1:length(elements)) {
  combinations_list <- combinations(length(elements), i)
  
  # Cetak kombinasi
  cat(paste0("**", i, " Anggota:**\n"))
  for (j in 1:length(combinations_list)) {
    combination <- combinations_list[[j]]
    cat(paste0(elements[combination], collapse = ", "), "\n")
  }
  cat("\n")
}