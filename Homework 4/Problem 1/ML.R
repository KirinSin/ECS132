mergeall <- function() {
  data <- read.table("u.data", header = FALSE, sep = " ", col.names = ["user_id", "movie_id", "rating", "timestamp"])
  user <- read.table("u.user", header = FALSE, sep = "|", col.names = ["user_id", "age", "gender","occupation", "zip"])
  item <- read.table("u.item", header = FALSE, sep = "|", col.names = ["movie_id", "movie_name", "release_date", "web_page", "genres"])

  all <- merge(data, user, by = "user_id")
  all <- merge(all, item, by = "movie_id")

  write.table(all, sep = "|", col.names = FALSE)
}
