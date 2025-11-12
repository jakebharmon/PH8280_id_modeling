# ----------------------------
# Calibration / forecasting metadata
# ----------------------------
calibrationperiods <- c(51)
forecastinghorizon <- 0

model_name <- "scenario_one_half_vaccination"
cadfilename1 <- "influenza_df"
caddisease <- "Influenza"
datatype <- c("Influenza A","Influenza B")
series_cases <- c("Influenza A","Influenza B")
datetype <- "Weeks"

# ----------------------------
# State variables
# ----------------------------
vars <- c("S","I1","I2","R1","R2","V","C1","C2")

# ----------------------------
# Parameter vector and ordering
# ----------------------------
# "beta1",   # params1   transmission rate strain 1
# "beta2",   # params2   transmission rate strain 2
# "gamma1",  # params3   recovery rate from I1 -> R1
# "gamma2",  # params4   recovery rate from I2 -> R2
# "nu",      # params5   vaccination rate (S -> V)
# "N",       # params6   population size
# "rho"      # params7   reporting fraction

params <- c("beta1","beta2",
            "gamma1","gamma2",
            "nu","N","rho")

paramsfix <- c(0,0,
               0,0,
               1,1,0)

# ----------------------------
# Priors and initial conditions 
# Lower / upper bounds for all parameters in two-strain SIRV model
# ----------------------------------------------------------

# 1-2: beta1, beta2 (transmission rates)
params1_prior <- "normal(1.8, 0.2)T[0,]"
params1_LB <- 0
params1_UB <- 3
params2_prior <- "normal(1.5, 0.2)T[0,]"
params2_LB <- 0
params2_UB <- 3

# 3-4: gamma1, gamma2 (recovery rates)
# Infectious period 3–5 days
params3_prior <- "normal(1, 0.1)T[0,]"
params3_LB <- 0
params3_UB <- 2
params4_prior <- "normal(1, 0.1)T[0,]"
params4_LB <- 0
params4_UB <- 2

# 5: nu (vaccination rate)
# annual coverage c ≈ 40% or 60%
# 40 %	0.010
# 50 %	0.013
# 60 %	0.019
params5_prior <- 0.013

# 6: N (population)
params6_prior <- 3e6

# 7: Reporting fraction
params7_prior <- "beta(2,4)"
params7_LB <- 0
params7_UB <- 1

# ----------------------------
# Observation / error model priors
# ----------------------------
errstrc <- 1
negbinerror1_prior <- "exponential(1)"
negbinerror2_prior <- "exponential(1)"

# ----------------------------
# Time dependent templates
# ----------------------------
time_dependent_templates <- list()

# ----------------------------
# Differential equations (ODE system)
# S (vars1), I1 (vars2), I2 (vars3), R1 (vars4), R2 (vars5), V (vars6)
# ----------------------------
ode_system <- '
  diff_var1 = - (params1 * vars1 * vars2 / params6) - (params2 * vars1 * vars3 / params6) - params5 * vars1
  diff_var2 = (params1 * vars1 * vars2 / params6) - params3 * vars2
  diff_var3 = (params2 * vars1 * vars3 / params6) - params4 * vars3
  diff_var4 = params3 * vars2
  diff_var5 = params4 * vars3
  diff_var6 = params5 * vars1
  diff_var7 = params7 * (params1 * vars1 * vars2 / params6)
  diff_var8 = params7 * (params2 * vars1 * vars3 / params6)
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
fitting_index <- c(7,8)
fitting_diff <- c(1,1)

# ----------------------------
# Initial conditions 
# S (vars1), I1 (vars2), I2 (vars3), R1 (vars4), R2 (vars5), V (vars6)
# ----------------------------
Ic <- c(3e6 - 500, 300, 200, 0, 0, 0, 0, 0)
vars.init <- 1

# ----------------------------
# MCMC / sampling settings
# ----------------------------
niter <- 1000
num_chain <- 2