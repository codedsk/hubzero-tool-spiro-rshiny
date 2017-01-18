library(shiny)

spiro <- function(n1,n2,n3) {
  t <- seq(0,1,length.out=1000)
  z <- exp(1i*2*pi*n1*t) + exp(1i*2*pi*n2*t) + exp(1i*2*pi*n3*t)
  result <- data.frame(x=Re(z),y=Im(z))
  return (result)
}

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Spirograph"),

   # input sliders
   sliderInput(inputId="n1",label="n1",value=13,min=-10,max=20,step=1),
   sliderInput(inputId="n2",label="n2",value=-7,min=-10,max=20,step=1),
   sliderInput(inputId="n3",label="n3",value=-3,min=-10,max=20,step=1),

   # output plot
   plotOutput("spirograph")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$spirograph <- renderPlot({
    result <- spiro(input$n1,input$n2,input$n3)
    plot(result,type="l")
  })
}

# Run the application
#shinyApp(ui = ui, server = server)

