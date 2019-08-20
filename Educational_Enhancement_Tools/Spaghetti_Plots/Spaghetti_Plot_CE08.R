#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    title = 'Spaghetti plot', 
    
    # Add plot output
    plotOutput('plot', height = '600px'), 
    
    # Horizontal line
    HTML("<hr style = 'border-color: #01EBE0; border-width: 1px; margin: 20px 20px; border-style: dotted' >"),
    
    fluidRow(
        column(6, 
               sliderInput('RIvar', 'Subject-specific Intercepts Variance:', 
                           min = 0, max = 1000, value = 500)
    ), 
    column(6, 
           sliderInput('RSvar', 'Subject-specific Slopes Variance:', 
                       min = 0, max = 500, value = 250)))
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    library(nlme)
    library(lattice)
    library(matrixcalc)
    library(Matrix)
    library(MASS)
    library(ggplot2)
    
    # specify number of unique subjects
    N <- 100
    
    # set number of measurements per subject
    n <- 12
    
    # create vector of ids
    id <- rep(1:N, each = n)
    
    # set time limits
    min.time <- 0
    max.time <- 12
    
    # Sample time-points of measurements
    time <- NULL
    
    for (i in 1:N) {
        tmp <- c(0, sort(runif(n - 1, min = min.time, max = max.time)))
        time <- c(time, tmp)
    }
    
    # start data frame
    # simdat <- data.frame("id" = id, "time" = time)
    
    X1 <- model.matrix(~ 1 + time, data = simdat)
    Z1 <- model.matrix(~ 1 + time, data = simdat)
    
    D <- reactive(matrix(c(input$RIvar, rep(rnorm(1), 2), input$RSvar), 
                         ncol = 2, nrow = 2))
    
    D2 <- reactive(as.matrix(nearPD(D())$mat))
    
    b <- reactive(mvrnorm(n = N, mu = rep(0, 2), D2()))
    
    true.betas <- c(0, 3.25)
    
    eta.y <- reactive(as.vector(X1 %*% true.betas + rowSums(Z1 * b()[id, ]))) 
    
    sigma.e <- 1
    
    y <- reactive(rnorm(n * N, mean = eta.y(), sd = sigma.e))

    output$plot <- renderPlot({
        
        ggplot(simdat) + geom_line(aes(x = time, y = y(), group = id)) + 
            ylab('Longitudinal Outcome') + 
            xlab('Time') + 
            ylim(c(-600, 600)) + 
            theme_bw() + 
            theme(panel.grid = element_blank())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
