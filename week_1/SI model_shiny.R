# install.packages(c("shiny","deSolve"))  # if needed
library(shiny)
library(deSolve)

# SI model ODEs: S' = -beta*S*I/N, I' = beta*S*I/N
si_model <- function(time, state, params) {
  with(as.list(c(state, params)), {
    lambda <- beta * I / N
    dS <- -lambda * S
    dI <-  lambda * S
    list(c(dS, dI))
  })
}

ui <- fluidPage(
  titlePanel("SI Model (Counts) with Population Size N"),
  sidebarLayout(
    sidebarPanel(
      numericInput("N", "Population size (N):", value = 1e6, min = 1, step = 1),
      sliderInput("beta", "Transmission rate (β per day):", min = 0, max = 2, value = 0.3, step = 0.01),
      
      tags$hr(),
      numericInput("S0", "Initial Susceptible S0:", value = 999000, min = 0, step = 1),
      numericInput("I0", "Initial Infectious I0:", value = 1000,  min = 0, step = 1),
      
      tags$hr(),
      numericInput("days", "Simulation days:", value = 160, min = 1, step = 1),
      actionButton("run", "Run Simulation", class = "btn-primary")
    ),
    mainPanel(
      verbatimTextOutput("stats"),
      plotOutput("siPlot", height = "440px")
    )
  )
)

server <- function(input, output, session) {
  sim <- eventReactive(input$run, {
    validate(
      need(input$S0 + input$I0 == input$N,
           "S0 + I0 must equal N. Please adjust your initial conditions.")
    )
    state  <- c(S = input$S0, I = input$I0)
    params <- c(beta = input$beta, N = input$N)
    times  <- seq(0, input$days, by = 1)
    as.data.frame(ode(y = state, times = times, func = si_model, parms = params))
  })
  
  output$stats <- renderText({
    # Early exponential growth rate when S ~ S0: r0 = beta * S0 / N
    r0 <- input$beta * input$S0 / input$N
    td <- if (r0 > 0) log(2) / r0 else NA
    paste0(
      "Early growth rate (r0) ≈ β·S0/N = ",
      round(input$beta, 4), " × ", input$S0, " / ", input$N,
      " = ", round(r0, 4), " per day",
      if (!is.na(td)) paste0("   |   Early doubling time ≈ ", round(td, 2), " days") else ""
    )
  })
  
  output$siPlot <- renderPlot({
    out <- sim()
    matplot(out$time, cbind(out$S, out$I),
            type = "l", lwd = 2, lty = 1,
            xlab = "Time (days)", ylab = "Count of individuals",
            main = "SI Trajectories (Counts)",
            col = c("blue", "red"))
    legend("right", legend = c("S", "I"),
           col = c("blue", "red"), lwd = 2, lty = 1)
  })
}

shinyApp(ui, server)
