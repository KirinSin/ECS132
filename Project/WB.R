library(ggplot2)

loadtable <- function() {

    # read in data from CSV file
    d<-read.csv("vocabulary_norms_data.csv", fill=TRUE)

    # only extract necessary information for our regression analysis
    dd<-data.frame(d$data_id, d$age, d$birth_order, d$ethnicity, d$sex, d$mom_ed, d$vocab)
    dd[is.na(dd)] <- 0

    # form dummy variables for gender and ethnicity
    dd$d.sex<-as.integer(dd$d.sex=="Male")
    dd$asian<-as.integer(dd$d.ethnicity=="Asian")
    dd$black<-as.integer(dd$d.ethnicity=="Black")
    dd$hispanic<-as.integer(dd$d.ethnicity=="Hispanic")
    dd$d.ethnicity<-as.integer(dd$d.ethnicity=="White")

    # convert categorical variable to numerical variable
    ed_levels<-c("Primary", "Some Secondary", "Secondary", "Some College", "College", "Some Graduate", "Graduate")
    dd$d.mom_ed <- sapply(dd$d.mom_ed, function(x) which(ed_levels==x)[1])
    bo_levels<-c("First", "Second", "Third", "Fourth", "Fifth", "Sixth", "Seventh", "Eighth")
    dd$d.birth_order <- sapply(dd$d.birth_order, function(x) which(bo_levels==x)[1])

    # standardize column names
    colnames(dd)<-c("id", "age", "bo", "white", "gender", "momed", "vocab", "asian", "black", "hispanic")

    # voila, we got a data
    return(dd)
}


x = loadtable()

summary(lm(x$vocab ~ x$age + x$bo + x$gender + x$momed + x$black + x$white + x$asian + x$hispanic))
summary(lm(x$vocab ~ x$age))
