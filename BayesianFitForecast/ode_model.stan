functions {




    array[] real ode(real t, array[] real y, array[] real theta, array[] real x_r, array[] int x_i) { 

 
    real beta = theta[1];
    real gamma = theta[2];
    real kappa = theta[3];
    real rho = theta[4];
    real i0 = theta[5];
    real N = x_i[1];
    array[5] real init = {N-i0, 0, i0, 0, i0};

    real S = y[1]+init[1];
    real E = y[2]+init[2];
    real I = y[3]+init[3];
    real R = y[4]+init[4];
    real C = y[5]+init[5];

    real dS_dt = -beta * I * S / N;
    real dE_dt = beta * I * S / N - kappa * E;
    real dI_dt = kappa * E - gamma * I;
    real dR_dt = gamma * I;
    real dC_dt = rho * kappa * E; 
 return{dS_dt,dE_dt,dI_dt,dR_dt,dC_dt};
 }

}
  
data {
    int<lower=1> n_days;
    int<lower=0> nfst_days;real t0;
array[n_days + nfst_days] real ts;
array[n_days] int cases1;
    int N;
}
  
transformed data {
    array[0] real x_r;
    array[1] int x_i = {N};

  }parameters {
    real<lower=0> beta;
    real<lower=0> gamma;
    real<lower=0> kappa;
    real<lower=0, upper=1> rho;
    real<lower=0> i0;
}
transformed parameters {
  array[n_days + nfst_days, 5] real y;
  {
    array[5] real theta;
    theta[1] = beta;
    theta[2] = gamma;
    theta[3] = kappa;
    theta[4] = rho;
    theta[5] = i0;
    y = integrate_ode_rk45(ode, rep_array(0.0, 5), t0, ts, theta, x_r, x_i);
  }
}
model {
  beta ~ normal(0, 1)T[0,];
  gamma ~ normal(0, 1)T[0,];
  kappa ~ normal(0, 1)T[0,];
  rho ~ normal(0, 1)T[0,];
  i0 ~ normal(0, 10)T[0,];

  for (t in 1:n_days) {
    cases1[t] ~ poisson(fmax(1e-6, y[t,5]));
  }
}

generated quantities {
  array[n_days + nfst_days] real pred_cases1;
  for (t in 1:(n_days + nfst_days)) {
    pred_cases1[t] = poisson_rng(fmax(1e-6, y[t,5]));
  }

  real R0 = beta / gamma;
  real recovery_time = 1 / gamma;
}

