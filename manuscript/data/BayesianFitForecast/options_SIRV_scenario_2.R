#Scenario 2 – Symmetric competition
#Because the model is symmetric, the two strains follow identical epidemic 
#trajectories and reach their peaks simultaneously. This scenario confirms 
#that the code treats both strains consistently, conserves total population, 
#and produces numerically identical outcomes when parameterized symmetrically.

# ----------------------------
# Calibration / forecasting metadata
# ----------------------------
calibrationperiods <- c(40)
forecastinghorizon <- 15

model_name <- "scenario_2_symmetric_competition"
cadfilename1 <- "influenza_df"
caddisease <- "Influenza"
datatype <- c("Influenza A","Influenza B")
series_cases <- c("Influenza A","Influenza B")
datetype <- "Weeks"

# ----------------------------
# State variables
# ----------------------------
vars <- c("S","I1","I2","R1","R2","V")

# ----------------------------
# Parameter vector and ordering
# ----------------------------
# "Lambda",  # params1   birth/input rate
# "beta1",   # params2   transmission rate strain 1
# "beta2",   # params3   transmission rate strain 2
# "mu",      # params4   natural death rate
# "gamma1",  # params5   recovery rate from I1 -> R1
# "gamma2",  # params6   recovery rate from I2 -> R2
# "nu"       # params7   vaccination rate (S -> V)
# "N"        # params8   population
# "i0"       # params9   initial infected
# "rho"      # params10  reporting fraction
# "sigma12"  # params11  Crossimmunity to 2 given 1
# "sigma21"  # params12  Crossimmunity to 1 given 2
params <- c("Lambda","beta1","beta2",
            "mu","gamma1","gamma2",
            "nu","N","i0","rho",
            "sigma12","sigma21")

paramsfix <- c(1,0,0,
               1,0,0,
               0,1,1,0,
               0,0)

# ----------------------------
# Priors and initial conditions 
# Lower / upper bounds for all parameters in two-strain SIRV model
# ----------------------------------------------------------

# 1: Lambda (input rate)
# Fix to 0 for constant population / short epidemic window
params1_prior  <- 0 

# 2–3: beta1, beta2 (transmission rates)
# lognormal priors centered on plausible beta = R0 * gamma (~0.3-0.5)
params2_prior <- "normal(3.5, 0.7)T[0,]"
params2_LB <- 0
params2_UB <- 10
params3_prior <- "normal(3.5, 0.7)T[0,]"
params3_LB <- 0
params3_UB <- 10

# 4: mu (natural death rate)
# Human life expectancy ~70 years 
params4_prior <- 2.7e-4

# 5–6: gamma1, gamma2 (recovery rates)
# Infectious period 3–5 days
params5_prior <- "normal(1.2, 0.2)T[0,]"
params5_LB <- 0
params5_UB <- 5
params6_prior <- "normal(1.2, 0.2)T[0,]"
params6_LB <- 0
params6_UB <- 5

# 7: nu (vaccination rate)
# annual coverage c ≈ 40% or 60%
params7_prior <- 0
#params7_prior <- "normal(0.010, 0.002)T[0,]"
params7_LB <- 0
params7_UB <- 0.05

# 8: N (population)
params8_prior <- 1e5

# 9: i0 (initial infected)
params9_prior <- 10

# 10: rho (reporting fraction)
# mean ~ 0.333;underreporting
params10_prior <- "beta(2, 4)"
params10_LB <- 0
params10_UB <- 1

# 11-12: sigma (cross immunity)
params11_prior <- "normal(0.95, 0.05)T[0,]"  # sigma12: partial immunity from strain 1 → strain 2
params11_LB <- 0
params11_UB <- 1
params12_prior <- "normal(0.95, 0.05)T[0,]"  # sigma21: partial immunity from strain 2 → strain 1
params12_LB <- 0
params12_UB <- 1


# ----------------------------
# Observation / error model priors
# ----------------------------
errstrc <- 1
normalerror1_prior <- "cauchy(0, 2.5)" 
negbinerror1_prior <- "exponential(5)"
negbinerror2_prior <- "exponential(5)"

# ----------------------------
# Time dependent templates
# ----------------------------
time_dependent_templates <- list()

# ----------------------------
# Differential equations (ODE system)
# S (vars1), I1 (vars2), I2 (vars3), R1 (vars4), R2 (vars5), V (vars6)
# ----------------------------
ode_system <- '
  diff_var1 = params1 - (params2 * vars1 * vars2 / params8) - (params3 * vars1 * vars3 / params8) - params7 * vars1 - params4 * vars1
  diff_var2 = (params2 * vars1 * vars2 / params8) + ((1 - params12) * params2 * vars5 * vars2 / params8) - (params4 + params5) * vars2
  diff_var3 = (params3 * vars1 * vars3 / params8) + ((1 - params11) * params3 * vars4 * vars3 / params8) - (params4 + params6) * vars3
  diff_var4 = params5 * vars2 - (1 - params11) * (params3 * vars4 * vars3 / params8) - params4 * vars4
  diff_var5 = params6 * vars3 - (1 - params12) * (params2 * vars5 * vars2 / params8) - params4 * vars5
  diff_var6 = params7 * vars1 - params4 * vars6
  y1 = params10 * vars2 
  y2 = params10 * vars3
  y3 = vars6
'


# ----------------------------
# Composite expressions of interest
# ----------------------------
composite_expressions <- list(
  R0_1 = "beta1 / gamma1",
  R0_2 = "beta2 / gamma2"
)

# ----------------------------
# Data / file / disease metadata
# ----------------------------
fitting_index <- c(2,3)
fitting_diff <- c(1,1)

# ----------------------------
# Initial conditions 
# S (vars1), I1 (vars2), I2 (vars3), R1 (vars4), R2 (vars5), V (vars6)
# ----------------------------
Ic <- c((1e5 - 2000), 1000, 1000, 0, 0, 0)
vars.init <- 1

# ----------------------------
# MCMC / sampling settings
# ----------------------------
niter <- 1000
num_chain <- 4
