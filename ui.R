shinyUI(pageWithSidebar(
  headerPanel("Lifetime Income Predictor*"),
  sidebarPanel(
    sliderInput('my_age', 'Current Age:', 
                 value = 40, min = 18, max = 60, step = 1),
    radioButtons('my_gender', 'Gender:', c("Male" = 1, "Female" = 2)),
    selectInput('my_education', 'Education Level:',
                c("Less than 9th Grade" = 1,
                  "9th to 12th Grade (no diploma)" = 8,
                  "High School Graduate (includes equivalency)" = 2,
                  "Some College (no degree)" = 9, "Associate Degree" = 3,
                  "Bachelor's Degree" = 4, "Master's Degree" = 5,
                  "Professional Degree" = 6, "Doctorate Degree" = 7)),
    sliderInput('work_low', 'Staring Work:', sep = "",
                value = 2015, min = 1976, max = 2025, step = 1),
    sliderInput('work_high', 'Retiring:', sep = "",
                value = 2025, min = 1981, max = 2030, step = 1),
    radioButtons('completed_datascience',
                 'Completing the Data Science specialization:',
                 c("Yes" = 1, "No" = 0)),
    p("* Not to be confused for a real financial planning tool"),
    width = 5
  ),
  mainPanel(
    plotOutput('income_graph'),
    h4(textOutput('total')),
    h4(textOutput('ds_msg')),
    width = 7
  )
))
