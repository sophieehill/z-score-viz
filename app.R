#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Z-score visualization"),
    
    tags$div(HTML("<script type='text/x-mathjax-config' >
            MathJax.Hub.Config({
            tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
            });
            </script >
            ")),
    
    verticalLayout(
        
    p("If we have two values $x$ and $y$ drawn from two different distributions $X$ and $Y$, we cannot compare them directly. However, once we take the $z$-score, then we can consider $z_x$ and $z_y$ as values from the same standard normal distribution! That's why taking the $z$-score is also called 'standardizing'."),
     p("Recall that we can compute the $z$-score for a value $x$ from a distribution with mean $\\mu$ and standard deviation $\\sigma$ as follows: $z_x = \\frac{x - \\mu }{ \\sigma }$"),
    p("To understand why, imagine applying this transformation to the whole distribution. When we substract $\\mu$, our distribution shifts to the left or right. When we divide by $\\sigma$, our distribution becomes narrower or wider."),
    p("No matter which values we choose for the original normal distribution, we always end up with the same line in dark blue by the end. This is the 'standard normal distribution', with $\\mu=0$, $\\sigma=1$."),
    p("In theory, we could choose any normal distribution, so why did we decide that $N \\sim (0,1)$ is 'the standard'? Well, one nice feature is that any value from this distribution can immediately be interpreted in terms of standard deviations from the mean. $($Can you see why?$)$ ")
    ),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("mu",
                        "Mean $(\\mu)$",
                        min = -10,
                        max = 10,
                        value = 3),
            sliderInput("sd",
                        "Standard deviation $(\\sigma)$",
                        min = 0,
                        max = 10,
                        value = 5),
            withMathJax(),
            radioButtons("plot", "Show me:",
                         c("Original distribution $X$" = "A",
                           "Mean-shifted distribution $X - \\mu$" = "B",
                           "Mean-shifted and scaled distribution $\\frac{X - \\mu}{\\sigma}$" = "C"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("plot")
        )
    )
)

# create function to create plot based on radio buttons


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # load required packages
    library(ggplot2) # for plots
    library(ggfortify) # for plotting normal PDF
    library(grid) # for annotation outside plot area
    library(gridExtra) # for annotation outside plot area

    # save colors for lines
    col2 <- "#b6dbff"
    col3 <- "#0a68c4"
    
    # select plot function
    # we will create three "different plots" by setting 0,1, or 2 of the lines to be transparent
    # this means the plotting area will stay fixed
    plot.select <- function(a1, a2){
        
    update_geom_defaults("line", list(size = 1.5))
    
    p1 <- ggdistribution(dnorm, seq(-20, 20, 0.1), 
                         mean = input$mu, sd = input$sd) +
        theme_classic(base_size = 18) + theme(plot.margin = unit(c(1,1,2,1), "cm")) +        ylab("Probability") + ggtitle("") + 
        geom_vline(xintercept=0, lty=2, color=col2) +
        geom_vline(xintercept=input$mu, lty=2) +
        annotate("text", x=input$mu, y=-0.08, label="mu", 
                 parse=TRUE, size=5) +
        annotate("segment", x=input$mu, xend=input$mu, 
                 y=-0.06, yend=-0.01, lty=2) +
        coord_cartesian(ylim=c(0,0.4), clip="off")
    
    p2 <- ggdistribution(dnorm, seq(-20, 20, 0.1), p=p1, 
                         mean = 0, sd = input$sd, colour=col2, alpha=a1)
    
    p3 <- ggdistribution(dnorm, seq(-20, 20, 0.1), p=p2, mean = 0, sd = 1, colour=col3, alpha=a2)
    p3
    }
    
    # we will create 3 different plots
    # one is the PDF of the normal distribution created by the user, with mean=mu, sd=sd
    # the second one keeps that PDF, but adds another line showing that distribution with mean 0
    # the third plot keeps both of the previous lines, but is now scaled with sd=1, i.e. a standard normal distribution
    # we use conditionalPanels to select the plot based on the radio button input

    output$plot <- renderPlot({
        a1 <- ifelse(input$plot=="A", 0, 1)
        a2 <- ifelse(input$plot=="C", 1, 0)
        plot.select(a1, a2)
    })
        
    
}

# Run the application 
shinyApp(ui = ui, server = server)
