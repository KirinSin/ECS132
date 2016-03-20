# Loading data to the data frame. Treating mother's education, bady's race,
# and birth order as dummy variables.
loadtable_dummy <- function() {
    # Reading CSV file
    data <- read.csv("vocabulary_norms_data.csv", fill = TRUE)

    # Making gender a dummy variable (Male: 1, Female: 0)
    data$is_male <- as.integer(data$sex=="Male")

    # Making Each of the ethnicity a dummy variable
    data$is_asian <- as.integer(data$ethnicity=="Asian")
    data$is_black <- as.integer(data$ethnicity=="Black")
    data$is_hispanic <- as.integer(data$ethnicity == "Hispanic")
    data$is_white <- as.integer(data$ethnicity == "White")

    # Making each of the Mom Education Level a dummy variable
    data$is_primary <- as.integer(data$mom_ed == "Primary")
    data$is_some_secondary <- as.integer(data$mom_ed == "Some Secondary")
    data$is_secondary <- as.integer(data$mom_ed == "Secondary")
    data$is_some_college <- as.integer(data$mom_ed == "Some College")
    data$is_college <- as.integer(data$mom_ed == "College")
    data$is_some_graduate <- as.integer(data$mom_ed == "Some Graduate")
    data$is_graduate <- as.integer(data$mom_ed == "Graduate")

    # Making each of the birth orders a dummy variable
    data$is_first <- as.integer(data$birth_order == "First")
    data$is_second <- as.integer(data$birth_order == "Second")
    data$is_third <- as.integer(data$birth_order == "Third")
    data$is_fourth <- as.integer(data$birth_order == "Fourth")
    data$is_fifth <- as.integer(data$birth_order == "Fifth")
    data$is_sixth <- as.integer(data$birth_order == "Sixth")
    data$is_seventh <- as.integer(data$birth_order == "Seventh")
    data$is_eighth <- as.integer(data$birth_order == "Eighth")

    # Returning data
    return(data)
}

predictplot1 <- function(data) {
    p<-data.frame(data$vocab)
    colnames(p)<-c("vocab")
    p$gender<-data$gender
    p$age<-data$age
    p<-na.omit(p)
    lp<-ggplot(p,
           aes(x=age,
               y=vocab,
               colour = ifelse(gender==1, 'male', 'female'),
               )) + geom_point() + labs(title="Plot of Vocabulary by various ages and genders (F/M)\n", colour="Gender")+ geom_smooth(method=lm) + ylab("Vocabulary") + xlab("Age")
    ggsave("wordbank-age-gender.pdf")
}

quantileplot1 <- function(data) {
    p<-data.frame(data$vocab)
    colnames(p)<-c("vocab")
    p$age<-data$age
    lp<-ggplot(p, aes(x=age, y=vocab)) + geom_point() + stat_quantile(aes(colour = ..quantile..), quantiles = seq(0.1, 0.9, by=0.1), method = "rqss") + ylab("Vocabulary") + xlab("Age")
    ggsave("quantile-vocab-age.pdf")
}

# Loading data to the data frame. Treating mother's education, bady's race as
# level variables and birth order as dummy variables.
loadtable <- function() {
    # Reading CSV file
    data <- read.csv("vocabulary_norms_data.csv", fill = TRUE)

    # Making gender a dummy variable (Male: 1, Female: 0)
    data$is_male <- as.integer(data$sex=="Male")

    # Making Each of the ethnicity a dummy variable
    data$is_asian <- as.integer(data$ethnicity=="Asian")
    data$is_black <- as.integer(data$ethnicity=="Black")
    data$is_hispanic <- as.integer(data$ethnicity == "Hispanic")
    data$is_white <- as.integer(data$ethnicity == "White")

    # Making each of the Mom Education Level (from 1 to 7, which represents from Primary to Graduate)
    data$education <- sapply(data$mom_ed, function(x) which(c("Primary", "Some Secondary", "Secondary", "Some College", "College", "Some Graduate", "Graduate") == x)[1])

    # Making each of the Birth Order Level (from 1 to 8, which represents from First to Eighth)
    data$order <- sapply(data$birth_order, function(x) which(c("First", "Second", "Third", "Fourth", "Fifth", "Sixth", "Seventh", "Eighth") == x)[1])

    # Returning data
    return(data)
}

# Saving data to X. We are using loadtable_dummy instead of loadtable_level.
# However, we used loadtable_level for some of our tests and have included them
# in our project report.
x = loadtable_dummy()

# Finding linear regression model. This is our final regression model, which
# only contains age and gender. We tried different models and have included them
# in our project report.
lmout = lm(x$vocab ~ x$age + x$is_male)

# Printing summary
summary(lmout)
