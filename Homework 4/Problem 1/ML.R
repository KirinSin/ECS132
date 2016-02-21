mergeall <- function() {
  data <- read.table("u.data", header = FALSE, sep = "\t", fill = TRUE)
  user <- read.table("u.user", header = FALSE, sep = "|", fill = TRUE)
  item <- read.table("u.item", header = FALSE, sep = "|", fill = TRUE)

  all <- merge(data, user, by = "V1")
  all <- merge(all, item, by = "V1")

  write.table(all, file = "u.all", sep = "|", col.names = FALSE, row.names = FALSE)
}
