# ----------------------------
# Calibration / forecasting metadata
# ----------------------------
calibrationperiods <- c(17)
forecastinghorizon <- 10

model_name <- "SIQR"
cadfilename1 <- "influenza_df"
caddisease <- "Influenza"
datatype <- c("Infectious","newly infected people")
series_cases <- c("Infectious","newly infected people")
datetype <- "Days"

# ----------------------------
# State variables
# ----------------------------
vars <- c("S","I1","I2","Q1","Q2","R1","R2")

# ----------------------------
# Parameter vector and ordering
# ----------------------------
# "Lambda",  # params1   birth/input rate
# "beta1",   # params2   transmission rate strain 1
# "beta2",   # params3   transmission rate strain 2
# "mu",      # params4   natural death rate
# "gamma1",  # params5   recovery rate from I1 -> R1
# "gamma2",  # params6   recovery rate from I2 -> R2
# "delta1",  # params7   quarantine rate I1 -> Q1
# "delta2",  # params8   quarantine rate I2 -> Q2
# "alpha1",  # params9   recovery rate from Q1 -> R1
# "alpha2",  # params10  recovery rate from Q2 -> R2
# "sigma12", # params11  cross-effect: susceptibility of R1 -> infection by strain2
# "sigma21"  # params12  cross-effect: susceptibility of R2 -> infection by strain1

params <- c("Lambda","beta1","beta2","mu","gamma1","gamma2",
            "delta1","delta2","alpha1","alpha2","sigma12","sigma21",
            "N","i0")

paramsfix <- c(0,0,0,0,0,0,
               0,0,0,0,0,0,
               1,1)

# ----------------------------
# Priors and initial conditions 
# Lower / upper bounds for all 12 parameters in SIQR model
# ----------------------------------------------------------

# 1: Lambda (input rate)
params1_prior  <- "normal(0, 1)T[0,]"
params1_LB <- 0
params1_UB <- NA

# 2–3: beta1, beta2 (transmission rates)
params2_prior  <- "normal(0, 1)T[0,]"
params2_LB <- 0
params2_UB <- NA
params3_prior  <- "normal(0, 1)T[0,]"
params3_LB <- 0
params3_UB <- NA

# 4: mu (natural death rate)
params4_prior  <- "normal(0, 1)T[0,]"
params4_LB <- 0
params4_UB <- 1

# 5–6: gamma1, gamma2 (recovery rates)
params5_prior  <- 1000000     
params5_LB <- 0
params5_UB <- NA
params6_prior  <- "normal(0, 10)T[0,]"
params6_LB <- 0
params6_UB <- NA

# 7–8: delta1, delta2 (quarantine rates)
params7_prior  <- "normal(0, 1)T[0,]"   # delta1 (quarantine rate I1 -> Q1)
params7_LB <- 0
params7_UB <- NA
params8_prior  <- "normal(0, 1)T[0,]"   # delta2 (quarantine rate I2 -> Q2)
params8_LB <- 0
params8_UB <- NA

# 9–10: alpha1, alpha2 (recovery from quarantine)
params9_prior  <- "normal(0, 1)T[0,]"   # alpha1 (recovery rate from Q1 -> R1)
params9_LB <- 0
params9_UB <- NA
params10_prior <- "normal(0, 1)T[0,]"   # alpha2 (recovery rate from Q2 -> R2)
params10_LB <- 0
params10_UB <- NA

# 11–12: sigma12, sigma21 (cross-infection effects)
params11_prior <- "uniform(0, 1)"       # sigma12 (cross-effect R1->infection by strain2)
params11_LB <- 0
params11_UB <- 1
params12_prior <- "uniform(0, 1)"       # sigma21 (cross-effect R2->infection by strain1)
params12_LB <- 0
params12_UB <- 1

# 13-14: initial conditions - N, i0
params13_prior <- 1e6                   # N
params14_prior <- 10                    # i0

time_dependent_templates <- list()

ode_system <- '
  diff_var1 = params1 - (params2 * vars1 * vars2 / N) - (params3 * vars1 * vars3 / N) - params4 * vars1
  diff_var2 = params2 * vars1 * vars2 / N - (params4 + params5 + params7) * vars2
  diff_var3 = params3 * vars1 * vars3 / N - (params4 + params6 + params8) * vars3
  diff_var4 = params7 * vars2 - (params4 + params9) * vars4
  diff_var5 = params8 * vars3 - (params4 + params10) * vars5
  diff_var6 = params5 * vars2 + params9 * vars4 - params3 * params11 * vars6 * vars3 / N - params4 * vars6
  diff_var7 = params6 * vars3 + params10 * vars5 - params2 * params12 * vars7 * vars2 / N - params4 * vars7
'
# ----------------------------
# Composite expressions of interest
# ----------------------------
composite_expressions <- list(
  R0_1 = "beta1 / (gamma1 + delta1)",
  R0_2 = "beta2 / (gamma2 + delta2)",
  CrossReinf = "sigma12 + sigma21"
)

# ----------------------------
# Data / file / disease metadata
# ----------------------------

fitting_index <- c(4,5)
fitting_diff <- c(1,0)
errstrc <- 1
normalerror1_prior <- "cauchy(0, 2.5)" 
negbinerror1_prior <- "exponential(5)"
negbinerror2_prior <- "exponential(5)"

# ----------------------------
# Initial conditions
# ----------------------------
vars.init <- 0

Ic = c("N-i0", 0, "i0", 0, "i0", 0, 0)

# ----------------------------
# MCMC / sampling settings
# ----------------------------
niter <- 1000
num_chain <- 4