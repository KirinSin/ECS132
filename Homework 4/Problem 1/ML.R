mergeall <- function() {
  data <- read.table("u.data", header = FALSE, sep = "\t", fill = TRUE)
  user <- read.table("u.user", header = FALSE, sep = "|", fill = TRUE)
  item <- read.table("u.item", header = FALSE, sep = "|", fill = TRUE)

  all <- merge(data, user, by = "V1")
  all <- merge(all, item, by = "V1")

  write.table(all, file = "u.all", sep = "|", col.names = FALSE, row.names = FALSE)
}

propf <- function() {
  user <- read.table("u.user", header = FALSE, sep = "|", fill = TRUE)
  gender = user["V3"]
  sum(gender == "F") / length(gender[[1]])
}

print(propf())

oldest <- function() {
  user <- read.table("u.user", header = FALSE, sep = "|", fill = TRUE)
  age <- user["V2"]
  max(age)
}

print(oldest())
