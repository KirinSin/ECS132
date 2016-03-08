mergeall <- function() {
  data <- read.table("u.data", header = FALSE, sep = "\t", fill = TRUE)
  user <- read.table("u.user", header = FALSE, sep = "|", fill = TRUE)
  item <- read.table("u.item", header = FALSE, sep = "|", fill = TRUE, quote='')

  all <- merge(data, user, by = 1)
  all <- merge(all, item, by.x=2, by.y=1)

  write.table(all, file="u.all", sep = "|", col.names =c('movieid','userid','rating','timestamp','age','gender','occupation','zipcode','movietitle','releasedate','videoreleasedate','imdburl','unknown','action','adventure','animation','childrens','comedy','crime','documentary','drama','fantasy','film-noir','horror','musical','mystery','romance','sci-fi','thriller','war','western'), row.na    mes = FALSE)
}

propf <- function() {
  user <- read.table("u.user", header = FALSE, sep = "|", fill = TRUE)
  gender = user["V3"]
  sum(gender == "F") / length(gender[[1]])
}

oldest <- function() {
  user <- read.table("u.user", header = FALSE, sep = "|", fill = TRUE)
  age <- user["V2"]
  max(age)
}

mostactive <- function() {
  data <- read.table("u.data", header = FALSE, sep = "\t", fill = TRUE)
  id <- data["V1"]
  as.numeric(names(which.max(table(id))))
}
