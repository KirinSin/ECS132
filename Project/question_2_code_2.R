loadtable <- function() {
    data <- read.csv("vocabulary_norms_data.csv", fill = TRUE)

    data$is_male <- as.integer(data$sex=="Male")

    data$is_asian <- as.integer(data$ethnicity=="Asian")
    data$is_black <- as.integer(data$ethnicity=="Black")
    data$is_hispanic <- as.integer(data$ethnicity == "Hispanic")
    data$is_white <- as.integer(data$ethnicity == "White")

    data$education <- sapply(data$mom_ed, function(x) which(c("Primary", "Some Secondary", "Secondary", "Some College", "College", "Some Graduate", "Graduate") == x)[1])
    data$order <- sapply(data$birth_order, function(x) which(c("First", "Second", "Third", "Fourth", "Fifth", "Sixth", "Seventh", "Eighth") == x)[1])

    return(data)
}

x = loadtable()

lmout = lm(x$vocab ~ x$age
                   + x$is_male
                   + x$is_asian + x$is_black + x$is_hispanic + x$is_white
                   + x$education
                   + x$order)

summary(lmout)
