library(shiny)
library(deSolve)

# SIR model with population size N
sir_model <- function(time, state, params) {
  with(as.list(c(state, params)), {
    lambda <- beta * I / N
    dS <- -lambda * S
    dI <-  lambda * S - gamma * I
    dR <-  gamma * I
    
    R0 <- beta / gamma
    Reff <- R0 * S / N
    
    list(c(dS, dI, dR), c(Reff = Reff))
  })
}

ui <- fluidPage(
  titlePanel("SIR Model (Counts) with Population Size N"),
  sidebarLayout(
    sidebarPanel(
      numericInput("N", "Population size (N):", value = 1e6, min = 1, step = 1),
      sliderInput("beta", "Transmission rate (β per day):", min = 0, max = 2, value = 0.3, step = 0.01),
      sliderInput("gamma", "Recovery rate (γ per day):", min = 0, max = 2, value = 0.1, step = 0.01),
      
      tags$hr(),
      numericInput("S0", "Initial Susceptible S0:", value = 999000, min = 0, step = 1),
      numericInput("I0", "Initial Infectious I0:", value = 1000, min = 0, step = 1),
      numericInput("R0_init", "Initial Recovered R0:", value = 0, min = 0, step = 1),
      
      tags$hr(),
      numericInput("days", "Simulation days:", value = 160, min = 1, step = 1),
      actionButton("run", "Run Simulation", class = "btn-primary")
    ),
    mainPanel(
      verbatimTextOutput("stats"),
      plotOutput("sirPlot", height = "420px"),
      plotOutput("reffPlot", height = "280px")
    )
  )
)

server <- function(input, output, session) {
  sim <- eventReactive(input$run, {
    # Validate that totals match N
    validate(
      need(input$S0 + input$I0 + input$R0_init == input$N,
           "S0 + I0 + R0 must equal N. Please adjust your initial conditions.")
    )
    
    state <- c(S = input$S0, I = input$I0, R = input$R0_init)
    params <- c(beta = input$beta, gamma = input$gamma, N = input$N)
    times <- seq(0, input$days, by = 1)
    
    as.data.frame(ode(y = state, times = times, func = sir_model, parms = params))
  })
  
  output$stats <- renderText({
    R0 <- input$beta / input$gamma
    paste0("Basic reproduction number R0 = β/γ = ",
           round(input$beta, 4), " / ", round(input$gamma, 4),
           " = ", round(R0, 3))
  })
  
  output$sirPlot <- renderPlot({
    out <- sim()
    matplot(out$time, cbind(out$S, out$I, out$R),
            type = "l", lwd = 2, lty = 1,
            xlab = "Time (days)", ylab = "Count of individuals",
            main = "SIR Trajectories (Counts)",
            col = c("blue", "red", "green"))
    legend("right", legend = c("S","I","R"),
           col = c("blue", "red", "green"), lwd = 2, lty = 1)
  })
  
  output$reffPlot <- renderPlot({
    out <- sim()
    plot(out$time, out$Reff, type = "l", lwd = 2,
         xlab = "Time (days)", ylab = "Reff(t)",
         main = "Effective Reproduction Number Reff(t)")
    abline(h = 1, lty = 2)
  })
}

shinyApp(ui, server)
