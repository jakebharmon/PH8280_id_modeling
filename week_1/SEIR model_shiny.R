# install.packages(c("shiny","deSolve"))  # if needed
library(shiny)
library(deSolve)

# SEIR with mass-action FOI: lambda = beta * I / N
seir_model <- function(time, state, params) {
  with(as.list(c(state, params)), {
    lambda <- beta * I / N          # force of infection
    dS <- -lambda * S                # S -> E
    dE <-  lambda * S - sigma * E    # E -> I at rate sigma
    dI <-  sigma * E - gamma * I     # I -> R at rate gamma
    dR <-  gamma * I
    
    R0   <- beta / gamma             # for standard SEIR with mass-action
    Reff <- R0 * S / N
    
    list(c(dS, dE, dI, dR), c(Reff = Reff))
  })
}

ui <- fluidPage(
  titlePanel("SEIR Model (Counts) with Population Size N"),
  sidebarLayout(
    sidebarPanel(
      numericInput("N", "Population size (N):", value = 1e6, min = 1, step = 1),
      sliderInput("beta",  "Transmission rate (β per day):", min = 0, max = 2, value = 0.3, step = 0.01),
      sliderInput("sigma", "Progression rate (σ per day):",  min = 0, max = 2, value = 0.2, step = 0.01),  # 1/σ ~ latent period
      sliderInput("gamma", "Recovery rate (γ per day):",     min = 0, max = 2, value = 0.1, step = 0.01),
      
      tags$hr(),
      numericInput("S0", "Initial Susceptible S0:", value = 998000, min = 0, step = 1),
      numericInput("E0", "Initial Exposed E0:",     value = 1000,   min = 0, step = 1),
      numericInput("I0", "Initial Infectious I0:",  value = 1000,   min = 0, step = 1),
      numericInput("R0_init", "Initial Recovered R0:", value = 0,   min = 0, step = 1),
      
      tags$hr(),
      numericInput("days", "Simulation days:", value = 200, min = 1, step = 1),
      actionButton("run", "Run Simulation", class = "btn-primary")
    ),
    mainPanel(
      verbatimTextOutput("stats"),
      plotOutput("seirPlot", height = "440px"),
      plotOutput("reffPlot", height = "280px")
    )
  )
)

server <- function(input, output, session) {
  sim <- eventReactive(input$run, {
    validate(
      need(input$S0 + input$E0 + input$I0 + input$R0_init == input$N,
           "S0 + E0 + I0 + R0 must equal N. Please adjust your initial conditions.")
    )
    
    state  <- c(S = input$S0, E = input$E0, I = input$I0, R = input$R0_init)
    params <- c(beta = input$beta, sigma = input$sigma, gamma = input$gamma, N = input$N)
    times  <- seq(0, input$days, by = 1)
    
    as.data.frame(ode(y = state, times = times, func = seir_model, parms = params))
  })
  
  output$stats <- renderText({
    R0 <- input$beta / input$gamma
    paste0(
      "Basic reproduction number  R0 = β/γ = ",
      round(input$beta, 4), " / ", round(input$gamma, 4),
      " = ", round(R0, 3),
      "   |   Mean latent period = 1/σ = ", round(1/input$sigma, 2),
      " days;  Mean infectious period = 1/γ = ", round(1/input$gamma, 2), " days"
    )
  })
  
  output$seirPlot <- renderPlot({
    out <- sim()
    matplot(out$time, cbind(out$S, out$E, out$I, out$R),
            type = "l", lwd = 2, lty = 1,
            xlab = "Time (days)", ylab = "Count of individuals",
            main = "SEIR Trajectories (Counts)",
            col = c("blue", "orange", "red", "green"))
    legend("right", legend = c("S","E","I","R"),
           col = c("blue", "orange", "red", "green"), lwd = 2, lty = 1)
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
