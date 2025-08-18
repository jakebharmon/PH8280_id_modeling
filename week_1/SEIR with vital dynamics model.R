# install.packages(c("shiny","deSolve"))  # if needed
library(shiny)
library(deSolve)

# SEIR with births & natural deaths: per-capita mu; N constant
seir_vital <- function(time, state, params) {
  with(as.list(c(state, params)), {
    lambda <- beta * I / N
    dS <- mu * N - lambda * S - mu * S
    dE <- lambda * S - sigma * E - mu * E
    dI <- sigma * E - gamma * I - mu * I
    dR <- gamma * I - mu * R
    
    R0   <- (beta * sigma) / ((gamma + mu) * (sigma + mu))
    Reff <- R0 * S / N
    
    # incidence (new infections) = lambda * S
    inc  <- lambda * S
    
    list(c(dS, dE, dI, dR), c(Reff = Reff, Incidence = inc))
  })
}

ui <- fluidPage(
  titlePanel("SEIR with Vital Dynamics (Counts)"),
  sidebarLayout(
    sidebarPanel(
      numericInput("N", "Population size (N):", value = 1e6, min = 1, step = 1),
      sliderInput("beta",  "Transmission rate β (per day):", min = 0, max = 2, value = 0.35, step = 0.01),
      sliderInput("sigma", "Progression rate σ (per day):",  min = 0, max = 2, value = 0.25, step = 0.01),
      sliderInput("gamma", "Recovery rate γ (per day):",     min = 0, max = 2, value = 0.1,  step = 0.01),
      sliderInput("mu",    "Birth = death rate μ (per day):",min = 0, max = 0.1, value = 0.0001, step = 0.0001),
      
      tags$hr(),
      numericInput("S0", "Initial S0:", value = 998000, min = 0, step = 1),
      numericInput("E0", "Initial E0:", value = 1000,   min = 0, step = 1),
      numericInput("I0", "Initial I0:", value = 1000,   min = 0, step = 1),
      numericInput("R0_init", "Initial R0:", value = 0, min = 0, step = 1),
      
      tags$hr(),
      numericInput("days", "Simulation days:", value = 365, min = 1, step = 1),
      actionButton("run", "Run Simulation", class = "btn-primary")
    ),
    mainPanel(
      verbatimTextOutput("stats"),
      plotOutput("seirPlot", height = "420px"),
      plotOutput("incPlot",  height = "260px"),
      plotOutput("reffPlot", height = "260px")
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
    params <- c(beta = input$beta, sigma = input$sigma, gamma = input$gamma,
                mu = input$mu, N = input$N)
    times  <- seq(0, input$days, by = 1)
    as.data.frame(ode(y = state, times = times, func = seir_vital, parms = params))
  })
  
  output$stats <- renderText({
    R0 <- (input$beta * input$sigma) / ((input$gamma + input$mu) * (input$sigma + input$mu))
    paste0(
      "R0 = (β·σ) / ((γ+μ)(σ+μ)) = ",
      round(input$beta, 4), "×", round(input$sigma, 4),
      " / (", round(input$gamma + input$mu, 4), "×", round(input$sigma + input$mu, 4), ") = ",
      round(R0, 3),
      "   |   Mean latent = 1/σ = ", round(1 / input$sigma, 2), " d",
      "   |   Mean infectious = 1/γ = ", round(1 / input$gamma, 2), " d",
      "   |   Life expectancy ≈ 1/μ = ", if (input$mu > 0) round(1 / input$mu, 1) else "∞", " d"
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
  
  output$incPlot <- renderPlot({
    out <- sim()
    plot(out$time, out$Incidence, type = "l", lwd = 2,
         xlab = "Time (days)", ylab = "New infections per day",
         main = "Incidence (λ(t) · S)")
  })
  
  output$reffPlot <- renderPlot({
    out <- sim()
    plot(out$time, out$Reff, type = "l", lwd = 2,
         xlab = "Time (days)", ylab = "Reff(t)",
         main = "Effective Reproduction Number")
    abline(h = 1, lty = 2)
  })
}

shinyApp(ui, server)
