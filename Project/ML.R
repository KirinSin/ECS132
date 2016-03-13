library(ggplot2)

mergeall <- function() {
    data <- read.table("u.data", header = FALSE, sep = "\t", fill = TRUE)
    user <- read.table("u.user", header = FALSE, sep = "|", fill = TRUE)
    item <- read.table("u.item", header = FALSE, sep = "|", fill = TRUE, quote='')

    all <- merge(data, user, by = 1)
    all <- merge(all, item, by.x = 2, by.y = 1)

    write.table(all, file="u.all", sep = "|", col.names =c('movieid','userid','rating','timestamp','age','gender','occupation','zipcode','movietitle','releasedate','videoreleasedate','imdburl','unknown','action','adventure','animation','childrens','comedy','crime','documentary','drama','fantasy','film-noir','horror','musical','mystery','romance','sci-fi','thriller','war','western'), row.names = FALSE, quote = FALSE)
}

mergeperuser <- function() {
    user <- read.table("u.user", header = FALSE, sep = "|", fill = TRUE)
    colnames(user)<-c("uid", "age", "gender", "occupantion", "zipcode")
    gender <- as.integer(user$gender=='M')
    user$gender <- gender
    data <- read.table("u.all", header=TRUE, sep = "|", fill = TRUE, quote='')
    num_ratings <- sapply(c(1:length(user$uid)), function(x) sum(data$userid==x))
    avg_rating <- sapply(c(1:length(user$uid)), function(x) mean(data$rating[data$userid==x]))
    user$num.ratings <- num_ratings
    user$avg.rating <- avg_rating
    write.table(user, file="u.peruser", sep = "|", col.names = TRUE, row.names = FALSE, quote = FALSE)
}

sigtest <- function(a, b) {
  t.test(a,b, "two.sided", 0, FALSE, TRUE, 0.95)
}

getinterval <- function() {
    data <- read.table("u.peruser", header=TRUE, sep = "|", fill = TRUE, quote='')
    idxmen<-which(data$gender==1)
    idxwomen<-which(data$gender==0)
    wbarmen<-mean(data$avg.rating[idxmen])
    wbarwomen<-mean(data$avg.rating[idxwomen])
    s2men<-mean(data$avg.rating[idxmen]^2)-wbarmen^2
    s2women<-mean(data$avg.rating[idxwomen]^2)-wbarwomen^2
    radiusdiff <- 1.96*sqrt(s2men/length(data$gender==1)+s2women/length(data$gender==0))
    radiusmen <- 1.96*sqrt(s2men/length(data$gender==1))
    radiuswomen <- 1.96*sqrt(s2women/length(data$gender==0))
    cat("approx. CI for E(ratings by men) = ", wbarmen -radiusmen, " to ", wbarmen +radiusmen, "\n")
    cat("approx. CI for E(ratings by women) = ", wbarwomen -radiuswomen, " to ", wbarwomen +radiuswomen, "\n")
    cat("approx. CI for E(ratings by men-ratings by women) = ", wbarmen - wbarwomen -radiusdiff, " to ", wbarmen - wbarwomen +radiusdiff, "\n")

    sigtest(data$avg.rating[idxmen], data$avg.rating[idxwomen])
}

ratingsplot <- function() {
    data <- read.table("u.peruser", header=TRUE, sep = "|", fill = TRUE, quote='')
    idxmen<-which(data$gender==1)
    idxwomen<-which(data$gender==0)
    mrating<-data.frame(data$avg.rating[idxmen])
    frating<-data.frame(data$avg.rating[idxwomen])
    colnames(mrating)<-c("Ratings")
    colnames(frating)<-c("Ratings")
    mrating$sex<-'male'
    frating$sex<-'female'
    ratings <- rbind(mrating, frating)
    ggplot(ratings, aes(Ratings, fill=sex)) + geom_histogram(alpha=0.2, aes(y=0.5*..density..), binwidth=0.5, position='identity') + ylab("Density")
    ggsave("ratings.pdf")
}

getinterval2 <- function() {
    user <- read.table("u.peruser", header=TRUE, sep = "|", fill = TRUE, quote='')
    midx <- which(user$gender==1)
    fidx <- which(user$gender==0)
    mbar <- mean(user$num.ratings[midx])
    fbar <- mean(user$num.ratings[fidx])
    ms2<-mean(user$num.ratings[midx]^2)-mbar^2
    fs2<-mean(user$num.ratings[fidx]^2)-fbar^2
    radiusdiff <- 1.96*sqrt(ms2/length(user$gender==1)+fs2/length(user$gender==0))
    cat("approx. CI for E(num of ratings by men- num of ratings by women) = ", mbar - fbar -radiusdiff, " to ", mbar - fbar +radiusdiff, "\n")
    phat <- mean(user$gender==1)
    radius <- 1.96*sqrt(phat*(1-phat)/length(user$gender))
    cat("approx. CI for proportion of users who are men = ", phat - radius, " to ", phat + radius, "\n")
}

predict <- function() { 
    #user <- read.table("u.peruser", header=TRUE, sep = "|", fill = TRUE, quote='')
    data <- read.table("u.all", header=TRUE, sep = "|", fill = TRUE, quote='')
    gender <- as.integer(data$gender =='M')
    data$gender <- gender
    lmout <- lm(data$rating ~ data$age + data$gender)
    summary(lmout)
}
