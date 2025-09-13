# install.packages(c("shiny","deSolve"))  # if needed
library(shiny)
library(deSolve)

# SEIRS (waning immunity) without vital dynamics
seirs_model <- function(time, state, params) {
  with(as.list(c(state, params)), {
    lambda <- beta * I / N
    dS <- -lambda * S + omega * R
    dE <-  lambda * S - sigma * E
    dI <-  sigma * E - gamma * I
    dR <-  gamma * I - omega * R
    
    R0   <- beta / gamma
    Reff <- R0 * S / N
    inc  <- lambda * S  # new infections per day
    
    list(c(dS, dE, dI, dR), c(Reff = Reff, Incidence = inc))
  })
}

ui <- fluidPage(
  titlePanel("SEIRS (Counts) – Waning Immunity, No Vital Dynamics - v2"),
  sidebarLayout(
    sidebarPanel(
      numericInput("N", "Population size (N):", value = 1e6, min = 1, step = 1),
      sliderInput("beta",  "Transmission rate β (/day):", min = 0, max = 2, value = 0.3, step = 0.01),
      sliderInput("sigma", "Progression rate σ (/day):",  min = 0, max = 2, value = 0.25, step = 0.01),
      sliderInput("gamma", "Recovery rate γ (/day):",     min = 0, max = 2, value = 0.10, step = 0.01),
      
      # Changed from slider to numeric input for precise control
      numericInput("omega", "Waning rate ω (/day):", 
                   value = 0.00274, min = 0, max = 1, step = 0.00001),
      helpText("For homework: ω = 1/365 days⁻¹ ≈ 0.00274 day⁻¹ or ω = 1/182.5 days⁻¹ ≈ 0.00548 day⁻¹"),
      
      tags$hr(),
      numericInput("S0", "Initial S0:", value = 998000, min = 0, step = 1),
      numericInput("E0", "Initial E0:", value = 1000,   min = 0, step = 1),
      numericInput("I0", "Initial I0:", value = 1000,   min = 0, step = 1),
      numericInput("R0_init", "Initial R0:", value = 0, min = 0, step = 1),
      
      tags$hr(),
      numericInput("days", "Simulation days:", value = 1825, min = 1, step = 1),
      helpText("For homework: simulate for 1,825 days (≈ 5 years)"),
      actionButton("run", "Run Simulation", class = "btn-primary")
    ),
    mainPanel(
      verbatimTextOutput("stats"),
      plotOutput("trajPlot", height = "420px"),
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
                omega = input$omega, N = input$N)
    times  <- seq(0, input$days, by = 1)
    as.data.frame(ode(y = state, times = times, func = seirs_model, parms = params))
  })
  
  output$stats <- renderText({
    R0 <- input$beta / input$gamma
    paste0(
      "R0 = β/γ = ", round(input$beta, 4), " / ", round(input$gamma, 4),
      " = ", round(R0, 3),
      "   |   Mean latent = 1/σ = ", round(1 / input$sigma, 2), " d",
      "   |   Mean infectious = 1/γ = ", round(1 / input$gamma, 2), " d",
      "   |   Mean immunity duration = 1/ω = ",
      if (input$omega > 0) round(1 / input$omega, 2) else "∞", " d"
    )
  })
  
  output$trajPlot <- renderPlot({
    out <- sim()
    matplot(out$time, cbind(out$S, out$E, out$I, out$R),
            type = "l", lwd = 2, lty = 1,
            xlab = "Time (days)", ylab = "Count of individuals",
            main = "SEIRS Trajectories (Counts)",
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