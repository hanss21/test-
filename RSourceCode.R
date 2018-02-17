# Hannah Lee
# Madison Smith
# Justin Chang
# Zale de Jong
# Section AF - Team 2
# Assignment 6

# Loads the httr, dplyr, knitr, ggplot2, and DescTools packages.
library("httr")
library("dplyr")
library(knitr)
library(ggplot2)
library(DescTools)

# Reads in the data set "intro-survey.csv" and stores it in the variable
# survey.data.
survey.data <- read.csv('data/intro-survey.csv', stringsAsFactors=FALSE)

# Renames the first column "programming_exp" to resolve unknown wrong
# column name issue.
colnames(survey.data)[1] <- "programming_exp"


#####################
##### SECTION 1 #####
#####################

# filters the data by whether the respondent was born in WA, outside of WA,
# or did not reply
wa.data <- filter(survey.data, washington_born == "Yes")
no.wa.data <- filter(survey.data, washington_born == "No")
no.reply.data <- filter(survey.data, washington_born == "")

# calculates the total number of students and the total number of students
# in each category
total.students <- summarize(survey.data, count = n())[1, 1]
total.wa.students <- summarize(wa.data, count = n())[1, 1]
total.no.wa.students <- summarize(no.wa.data, count = n())[1, 1]
total.no.reply <- summarize(no.reply.data, count = n())[1, 1]

# calculates the mean, max, and min programming experience scores for
# all students
mean.prog.exp <- summarize(survey.data, mean = mean(programming_exp))[1, 1]
max.prog.exp <- summarize(survey.data, max = max(programming_exp))[1, 1]
min.prog.exp <- summarize(survey.data, min = min(programming_exp))[1, 1]


# calculates the mean programming experience for wa-born and non-wa-born
# respondents
mean.prog.exp.wa <- summarize(wa.data, mean = mean(programming_exp))[1, 1]
mean.prog.exp.no.wa <- 
  summarize(no.wa.data, mean = mean(programming_exp))[1, 1]

# functions recieves a vector as a parameter and returns the mode 
# (most common value) in that vector
getMode <- function(vector.data) {
  uniqv <- unique(vector.data)
  return(uniqv[which.max(tabulate(match(vector.data, uniqv)))])
}

# calculates the mode programming experience for wa-born and non-wa-born 
# respondents
mode.prog.exp <- getMode(survey.data$programming_exp)
mode.prog.exp.wa <- getMode(wa.data$programming_exp)
mode.prog.exp.no.wa <- getMode(no.wa.data$programming_exp)

# calculates the mean web programming experience for all students,
# and wa-born and non-wa-born respondents
mean.web.prog <- summarize(survey.data, mean = mean(web_exp))[1, 1]
mean.web.prog.wa <- summarize(wa.data, mean = mean(web_exp))[1, 1]
mean.web.prog.no.wa <- summarize(no.wa.data, mean = mean(web_exp))[1, 1]

# calculates the mean of all the students, the wa-born students, and the 
# non-wa-born students experience with command line
mean.cd.line <- summarize(survey.data, mean = mean(cli_exp))[1, 1]
mean.cd.line.wa <- summarize(wa.data, mean = mean(cli_exp))[1, 1]
mean.cd.line.no.wa <- summarize(no.wa.data, mean = mean(cli_exp))[1, 1]

# calculates the mean of all the students, the wa-born students, and the 
# non-wa-born students experience with markdown
mean.md <- summarize(survey.data, mean = mean(md_exp))[1, 1]
mean.md.wa <- summarize(wa.data, mean = mean(md_exp))[1, 1]
mean.md.no.wa <- summarize(no.wa.data, mean = mean(md_exp))[1, 1]

# calculates the mean of all the students, the wa-born students, and the 
# non-wa-born students experience with r
mean.r <- summarize(survey.data, mean = mean(r_exp))[1, 1]
mean.r.wa <- summarize(wa.data, mean = mean(r_exp))[1, 1]
mean.r.no.wa <- summarize(no.wa.data, mean = mean(r_exp))[1, 1]

# calculates the most common operating system for wa-born and 
# non-wa-born respondents
mode.os <- getMode(survey.data$os)
mode.os.wa <- getMode(wa.data$os)
mode.os.no.wa <- getMode(no.wa.data$os)


#####################
##### SECTION 2 #####
#####################

# dataframe for the different software experiences 
programming.software <- select(survey.data, cli_exp, vcs_exp, md_exp, r_exp,
                               web_exp)
# dataframe for the how much programming experience for each person 
experience.pro <- select(survey.data, programming_exp)

# Table #1 for experience programming 
# "I've never" dataframe and the total number of students who have had no
# experience programming 
experience.never <- filter(experience.pro, programming_exp == "1") 
total.never <- nrow(experience.never)

# "experimented" dataframe and the total number of students who have 
experience.experimented <- filter(experience.pro, programming_exp == "2") 
total.experimented <- nrow(experience.experimented)

# "some experience" dataframe and the total number of students who have 
experience.some <- filter(experience.pro, programming_exp == "3") 
total.some <- nrow(experience.some) 

# "moderate" dataframe and the total number of students who have 
experience.moderate <- filter(experience.pro, programming_exp == "4") 
total.moderate <- nrow(experience.moderate) 

# "lots of experience" dataframe and the total number of students who have 
experience.lots <- filter(experience.pro, programming_exp == "5") 
total.lots <- nrow(experience.lots) 

# total number of students 
total.num.students <- nrow(experience.pro)

# Combine the numbers 
new.data.frame <- data_frame("no_experience" = total.never,
                             "experimented" = total.experimented,
                             "some_experience" = total.some, 
                             "moderate_experience" = total.moderate,
                             "lots_experience" = total.lots,
                             "total_students" = total.num.students)

#### Table #2 
# command-line experience 
cli.exp.0 <- filter(programming.software, cli_exp == "0") %>%
  nrow()

cli.exp.1 <- filter(programming.software, cli_exp == "1") %>%
  nrow()

cli.exp.2 <- filter(programming.software, cli_exp == "2") %>%
  nrow()

cli.exp.3 <- filter(programming.software, cli_exp == "3") %>%
  nrow()

software.names <- colnames(programming.software) 
name <- software.names[1]

cli.frame <- data_frame(name, "never_used" = cli.exp.0,
                        "used_it_a_few" = cli.exp.1,
                        "intermediate" = cli.exp.2,
                        "expert" = cli.exp.3)

# version control  
vcs.exp.0 <- filter(programming.software, vcs_exp == "0") %>%
  nrow()

vcs.exp.1 <- filter(programming.software, vcs_exp == "1") %>%
  nrow()

vcs.exp.2 <- filter(programming.software, vcs_exp == "2") %>%
  nrow()

vcs.exp.3 <- filter(programming.software, vcs_exp == "3") %>%
  nrow()

name.vsc <- software.names[2]

vcs.data <- data_frame("name" = name.vsc, "never_used" = vcs.exp.0, 
                       "used_it_a_few" = vcs.exp.1,
                       "intermediate" = vcs.exp.2, 
                       "expert" = vcs.exp.3)

vcs.data.frame <- bind_rows(cli.frame, vcs.data)

# Markdown column 
markdown.exp.0 <- filter(programming.software, md_exp == "0") %>%
  nrow()

markdown.exp.1 <- filter(programming.software, md_exp == "1") %>%
  nrow()

markdown.exp.2 <- filter(programming.software, md_exp == "2") %>%
  nrow()

markdown.exp.3 <- filter(programming.software, md_exp == "3") %>%
  nrow()

name.markdown <- software.names[3]

markdown.data <- data_frame("name" = name.markdown,
                            "never_used" = markdown.exp.0, 
                            "used_it_a_few" = markdown.exp.1,
                            "intermediate" = markdown.exp.2, 
                            "expert" = markdown.exp.3)

markdown.data.frame <- bind_rows(vcs.data.frame, markdown.data)

# R Language 
r.exp.0 <- filter(programming.software, r_exp == "0") %>%
  nrow()

r.exp.1 <- filter(programming.software, r_exp == "1") %>%
  nrow()

r.exp.2 <- filter(programming.software, r_exp == "2") %>%
  nrow()

r.exp.3 <- filter(programming.software, r_exp == "3") %>%
  nrow()

name.r <- software.names[4]

r.data <- data_frame("name" = name.r,
                     "never_used" = r.exp.0, 
                     "used_it_a_few" = r.exp.1,
                     "intermediate" = r.exp.2, 
                     "expert" = r.exp.3)

r.data.frame <- bind_rows(markdown.data.frame, r.data)

# Web Language 
web.exp.0 <- filter(programming.software, web_exp == "0") %>%
  nrow()

web.exp.1 <- filter(programming.software, web_exp == "1") %>%
  nrow()

web.exp.2 <- filter(programming.software, web_exp == "2") %>%
  nrow()

web.exp.3 <- filter(programming.software, web_exp == "3") %>%
  nrow()

name.web <- software.names[5]

web.data <- data_frame("name" = name.web, "never_used" = web.exp.0, 
                     "used_it_a_few" = web.exp.1, "intermediate" = web.exp.2,
                     "expert" = web.exp.3)

# this is the final dataframe that has all the numbers and such 
combine.frame <- bind_rows(r.data.frame, web.data)


#####################
##### SECTION 3 #####
#####################

# Gets rid of blanks for column washington_born
no_blanks <- filter(survey.data, washington_born != "")

# Creates a scatterplot relating 3 columns of the data
prgmexp <- ggplot(data = no_blanks) + 
  geom_jitter(mapping = aes(x = washington_born, 
                            y = programming_exp, color = os), 
              width = 0.15, height = 0.15) +
  labs(title = "Survey Responses", x = "Washington Born",
       y = "Programming Experience")

# Creates a bar graph of only 2 columns to further show the relationship
bargrph <- ggplot(data = no_blanks) +
  geom_bar(mapping = aes(x = programming_exp, fill = washington_born)) +
  labs(title = "Survey Responses", x = "Programming Experience",
       fill = "Washington Born")

# The mean programming experience of those born in washington
washingtonmean <- no_blanks %>%
  filter(washington_born == "Yes") %>%
  summarize(mean = mean(programming_exp))

# The mean programming experience of those born outside of washington
othermean <- no_blanks %>%
  filter(washington_born == "No") %>%
  summarize(mean = mean(programming_exp))

#####################
##### SECTION 4 #####
#####################
# This section deals with visualizations of the same data set used in
# previous sections, but comparing different variables than before.

# Creates a new data frame containing certain information from the previous
# dataset.
dataset.zale <- survey.data %>%
  # Selects the columns washington_born, coffee_cups, seahawks_fan, and
  # info_interest.
  select(washington_born, coffee_cups, seahawks_fan, info_interest) %>%
  # Filters the rows to exclude any blank answers.
  filter(washington_born != "", coffee_cups != "", seahawks_fan != "",
         info_interest != "") %>%
  # Filters the rows to exclude the row in which number coffee cups drank
  # is answered as 8,000,000.
  filter(coffee_cups != "8e+05")

# Creates isolated data sets of certain columns based on whether or not
# the respondee was born in Washington State.
coffee.washington.data <- dataset.zale %>%
  filter(washington_born == "Yes") %>%
  select(coffee_cups)
coffee.not.washington.data <- dataset.zale %>%
  filter(washington_born == "No") %>%
  select(coffee_cups)

info.washington.data <- dataset.zale %>%
  filter(washington_born == "Yes") %>%
  select(info_interest)
info.not.washington.data <- dataset.zale %>%
  filter(washington_born == "No") %>%
  select(info_interest)

seahawks.washington.data <- dataset.zale %>%
  filter(washington_born == "Yes") %>%
  select(seahawks_fan)
seahawks.not.washington.data <- dataset.zale %>%
  filter(washington_born == "No") %>%
  select(seahawks_fan)

# Creates the means, medians, and modes of each column in the new data frame,
# when applicable.
coffee.mean <- mean(dataset.zale$coffee_cups)
coffee.mean.washington <- mean(coffee.washington.data$coffee_cups)
coffee.mean.not.washington <- mean(coffee.not.washington.data$coffee_cups)

coffee.median <- median(dataset.zale$coffee_cups)
coffee.median.washington <- median(coffee.washington.data$coffee_cups)
coffee.median.not.washington <- median(coffee.not.washington.data$coffee_cups)

info.mode <- Mode(dataset.zale$info_interest)
info.mode.washington <- Mode(info.washington.data$info_interest)
info.mode.not.washington <- Mode(info.not.washington.data$info_interest)

seahawks.mode <- Mode(dataset.zale$seahawks_fan)
seahawks.mode.washington <- Mode(seahawks.washington.data$seahawks_fan)
seahawks.mode.not.washington <- Mode(seahawks.not.washington.data$seahawks_fan)

# Creates the jitter plot all.three.variables, which is a plot showing the
# count of coffee cups drank by people born/not born in Washington, and
# sorted by whether or not they are Seahawks fans.
all.three.variables <- ggplot(data = dataset.zale) +
                       geom_jitter(mapping = aes(x = washington_born,
                                                 y = coffee_cups,
                                                 color = seahawks_fan),
                                   alpha = 0.5, width = 0.15, height = 0.15,
                                   size = 3) +
                       facet_wrap(~info_interest, nrow = 2) +
                       labs(title = "Average Number Of Daily Coffee Cups
                            Versus Born In Washington And Seahawks Fan",
                            x = "Washington Born",
                            y = "Average Daily Number Of Coffee Cups",
                            color = "Seahawks Fan")

# Creates the jitter plot washington.versus.coffee, which is a plot showing
# the count of coffee cups drank by those born in Washington and those not
# born in Washington.
coffee.versus.washington <- ggplot(data = dataset.zale) +
                            geom_jitter(mapping = aes(x = washington_born,
                                                      y = coffee_cups),
                                        color = "green", alpha = 0.2,
                                        width = 0.15, height = 0.1, size = 3) +
                            labs(title = "Average Daily Cups Of Coffee Versus
                                 Born In Washington", x = "Washington Born",
                                 y = "Average Daily Number Of Coffee Cups")

# Creates the bar plot washington.versus.seahawks, which is a plot showing
# the count of Seahawks fans by those born in Washington and those not born
# in Washington.
seahawks.versus.washington <- ggplot(data = dataset.zale) +
                              geom_bar(mapping = aes(x = washington_born,
                                                     fill = seahawks_fan),
                                       width = 0.5, position = "dodge") +
                              labs(title = "Seahawks Fan Level Versus
                                   Born In Washington", x = "Washington Born",
                                   y = "Degree Of Seahawks Fan",
                                   fill = "Seahawks Fan")

# Creates the bar plot washington.versus.info, which is a plot showing the
# count of those deciding to apply for Informatics at UW by those born in
# Washington and those not born in Washington.
info.versus.washington <- ggplot(data = dataset.zale) +
                          geom_bar(mapping = aes(x = washington_born,
                                                 fill = info_interest),
                                   width = 0.5, position = "dodge") +
                          labs(title = "Considering Applying For Info Versus
                               Born In Washington", x = "Washington Born",
                               y = "Number Of Responses",
                               fill = "Info Interest")

new.data.frame <- data_frame("no_experience" = total.never, "experimented" = total.experimented, "some_experience" = total.some, 
                             "moderate_experience" = total.moderate, "lots_experience" = total.lots, "total_students" = total.num.students)

