# load the data and get the current year
# if loading the data from the 'census.csv' file doesn't work, then
# brute force build the data data.frame in helper.R
data <- read.csv("census.csv")
if (length(data$Income) != 2700) {
    source("helper.R")
}
current_year <- as.integer(strsplit(date(), split = " ")[[1]][5])

# build factor lists to allow converting inputs to factors
ages_list <- unique(data$Age_Group)
education_list <- unique(data$Education_Level)
gender_list <- unique(data$Gender)

# define a function to get from a specific age to an age group
# needed to convert age at different working years to age groups
age_groups <- function(age) {
    rval <- as.factor(ages_list[6])
    if (age < 65) rval <- as.factor(ages_list[5])
    if (age < 55) rval <- as.factor(ages_list[4])
    if (age < 45) rval <- as.factor(ages_list[3])
    if (age < 35) rval <- as.factor(ages_list[2])
    if (age < 25) rval <- as.factor(ages_list[1])
    
    rval
}

# define a function that ensures that the test set of age groups is
# fully covered by the model set
ensure_overlap <- function(m_ages, t_ages) {
    m_int <- as.integer(m_ages)
    t_int <- as.integer(t_ages)
    if (min(t_int) < min(m_int) || max(t_int) > max(m_int)) {
        elements1 <- (t_int < min(m_int))
        elements2 <- (t_int > max(m_int))
        t_int[elements1] <- min(m_int)
        t_int[elements2] <- max(m_int)
        t_ages <- ages_list[t_int]
    }
    t_ages
}

# define the function that takes all the inputs and computes three outputs
# output[[1]] = a vector of annual incomes for each working year
# output[[2]] = a vector of working years
# output[[3]] = a message about the value of data science
do_it_all <-function(age, gen, edu, low, high, dc) {
    # first, build a model for the selected gender and education
    # though we could just build a big model that includes these two factors,
    # it is more interesting to use these parameters to filter the data and
    # then build a specific model
    rows <- (data$Gender == gender_list[as.integer(gen)]) & 
        (data$Education_Level == education_list[as.integer(edu)]) & 
        (!is.na(data$Income))
    years <- data$Year[rows]
    ages <- data$Age_Group[rows]
    m_ages <- ages
    incomes <- data$Income[rows]
    mdata <- data.frame(years, ages, incomes)
    fit <- lm(incomes ~ ., data = mdata)
    
    # second, build the test set for the selected year range and age
    # this would be more straight forward except that the first step can
    # produce a model that doesn't have any data for one or more of the
    # age groups that might then appear in the test set - in order to avoid
    # the resulting error, we adjust the test set (the model isn't terribly
    # accurate to begin with, so in this instance the adjustment doesn't
    # impact the value of the solution significantly)
    if (low < current_year - (age - 18)) {
        low <- current_year - (age - 18)
    }
    if (low >= high) {
        high <- low + 1
    }
    years <- seq(from = low, to = high)
    age_low <- age - (current_year - low)
    age_high <- age - (current_year - high)
    ages_yr <- seq(from = age_low, to = age_high)
    ages <- sapply(ages_yr, age_groups)
    ages <- ensure_overlap(m_ages, ages)
    test <- data.frame(years, ages)
    
    # third, compute earnings based on the model and test set
    earnings <- predict(fit, newdata = test)
    
    # fourth, add a little for those who study data science and craft
    # a message about the value of that study
    extra <- as.integer(dc)
    if (!extra) {
        msg <- "You really should study Data Science... :-("
    } else {
        msg <-""
    }
    if (extra) {
        adds <- years > current_year
        earnings[adds] <- earnings[adds] + 15000
        if (sum(adds) < 10) {
            msg <- "If only you had completed Data Science sooner..."
        } else {
            msg <- "Look at the value of Data Science! :-)"
        }
    }
    
    # finally, output the list of values
    list(earnings, years, msg)
}

shinyServer(
    function(input, output, session) {
        
        # Ensure that the first year of work is not before you turned 18
        # and the last year of work is greater than the first year
        observe({
            min_low <- current_year - (input$my_age - 18)
            if (input$work_low < min_low) {
                updateSliderInput(session, 'work_low', value = min_low)
            }
            val <- input$work_low + 1
            if (input$work_high < val) {
                updateSliderInput(session, 'work_high', value = val)
            }
        })

        # Because the three different outputs are all functions of multiple
        # inputs, the same work gets done three times. Perhaps there is a
        # more efficient solution, but I just put all the work in one
        # function and re-compute each time there is a change in any input
        output$income_graph <- renderPlot({
            # Call the external function that does everything
            ans <- do_it_all(input$my_age, input$my_gender, input$my_education,
                             input$work_low, input$work_high,
                             input$completed_datascience)
            # Use the first (annual income) and second (work years) outputs
            # to create a plot of annual estimated income
            y <- round(ans[[1]] / 1000, 0)
            x <- ans[[2]]
            plot(x, y, main = "Estimated Annual Income", 
                 xlab = "Working Years", ylab = "Income (in $ Thousands)")
        })

        output$total <- renderText({
            # Call the external function that does everything
            ans <- do_it_all(input$my_age, input$my_gender, input$my_education,
                          input$work_low, input$work_high,
                          input$completed_datascience)
            # Sum the first (annual income) output to determine life time
            # income - the hard work is formating with commas...
            final <- round(sum(ans[[1]]),0)
            mil <- floor(final / 1000000)
            tho <- floor((final - mil * 1000000) / 1000)
            hun <- final - mil * 1000000 - tho * 1000
            ans <- sprintf("%d", hun)
            if (tho > 0) {
                ans <- sprintf("%d,%03d", tho, hun)
            }
            if (mil > 0) {
                ans <- sprintf("%d,%03d,%03d", mil, tho, hun)
            }
            sprintf("Lifetime income: $%s", ans)            
        })
        
        output$ds_msg <- renderText({
            # Call the external function that does everything
            ans <- do_it_all(input$my_age, input$my_gender, input$my_education,
                             input$work_low, input$work_high,
                             input$completed_datascience)
            # Just report the third (wise acre remark about data science)
            # output directly
            ans[[3]]
        })
    }
)
