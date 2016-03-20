loadtable <- function() {
    data <- read.csv("vocabulary_norms_data.csv", fill = TRUE)

    data$is_male <- as.integer(data$sex=="Male")

    data$is_asian <- as.integer(data$ethnicity=="Asian")
    data$is_black <- as.integer(data$ethnicity=="Black")
    data$is_hispanic <- as.integer(data$ethnicity == "Hispanic")
    data$is_white <- as.integer(data$ethnicity == "White")

    data$is_primary <- as.integer(data$mom_ed == "Primary")
    data$is_some_secondary <- as.integer(data$mom_ed == "Some Secondary")
    data$is_secondary <- as.integer(data$mom_ed == "Secondary")
    data$is_some_college <- as.integer(data$mom_ed == "Some College")
    data$is_college <- as.integer(data$mom_ed == "College")
    data$is_some_graduate <- as.integer(data$mom_ed == "Some Graduate")
    data$is_graduate <- as.integer(data$mom_ed == "Graduate")

    data$is_first <- as.integer(data$birth_order == "First")
    data$is_second <- as.integer(data$birth_order == "Second")
    data$is_third <- as.integer(data$birth_order == "Third")
    data$is_fourth <- as.integer(data$birth_order == "Fourth")
    data$is_fifth <- as.integer(data$birth_order == "Fifth")
    data$is_sixth <- as.integer(data$birth_order == "Sixth")
    data$is_seventh <- as.integer(data$birth_order == "Seventh")
    data$is_eighth <- as.integer(data$birth_order == "Eighth")

    return(data)
}

x = loadtable()

lmout = lm(x$vocab ~ x$age
                   + x$is_male
                   + x$is_asian + x$is_black + x$is_hispanic + x$is_white
                   + x$is_primary + x$is_some_secondary + x$is_secondary + x$is_some_college + x$is_college + x$is_some_graduate + x$is_graduate
                   + x$is_first + x$is_second + x$is_third + x$is_fourth + x$is_fifth + x$is_sixth + x$is_seventh + x$is_eighth)

summary(lmout)
