functions {




    array[] real ode(real t, array[] real y, array[] real theta, array[] real x_r, array[] int x_i) { 

 
    real Lambda = theta[1];
    real beta1 = theta[2];
    real beta2 = theta[3];
    real mu = theta[4];
    real gamma1 = theta[5];
    real gamma2 = theta[6];
    real nu = theta[7];
    real N = x_i[1];
    real i0 = x_i[2];
    array[6] real init = {10, 0, 0, 0, 0, 0};

    real S = y[1]+init[1];
    real I1 = y[2]+init[2];
    real I2 = y[3]+init[3];
    real R1 = y[4]+init[4];
    real R2 = y[5]+init[5];
    real V = y[6]+init[6];

    real dS_dt = Lambda - (beta1 * S * I1 / N) - (beta2 * S * I2 / N) - nu * S - mu * S;
    real dI1_dt = beta1 * S * I1 / N - (mu + gamma1) * I1;
    real dI2_dt = beta2 * S * I2 / N - (mu + gamma2) * I2;
    real dR1_dt = gamma1 * I1 - mu * R1;
    real dR2_dt = gamma2 * I2 - mu * R2;
    real dV_dt = nu * S - mu * V; 
 return{dS_dt,dI1_dt,dI2_dt,dR1_dt,dR2_dt,dV_dt};
 }

}
  
data {
    int<lower=1> n_days;
    int<lower=0> nfst_days;real t0;
array[n_days + nfst_days] real ts;
array[n_days] int cases1;
array[n_days] int cases2;
    int N;
    int i0;
}
  
transformed data {
    array[0] real x_r;
    array[2] int x_i = {N,i0};

  }parameters {
    real<lower=0> Lambda;
    real<lower=0> beta1;
    real<lower=0> beta2;
    real<lower=0, upper=1> mu;
    real<lower=0> gamma1;
    real<lower=0> gamma2;
    real<lower=0, upper=0.01> nu;
    real<lower=0> phi_inv1;
    real<lower=0> phi_inv2;
}
transformed parameters {
  array[n_days + nfst_days, 6] real y;
  real phi1 = 1.0 / phi_inv1;
  real phi2 = 1.0 / phi_inv2;
  array[7] real theta;
  theta[1] = Lambda;
  theta[2] = beta1;
  theta[3] = beta2;
  theta[4] = mu;
  theta[5] = gamma1;
  theta[6] = gamma2;
  theta[7] = nu;

  y = integrate_ode_rk45(ode, rep_array(0.0, 6), t0, ts, theta, x_r, x_i);
}
model {
  beta1 ~ lognormal(log(0.4), 0.4);
  beta2 ~ lognormal(log(0.38), 0.4);
  mu ~ normal(3.9e-5, 1e-6)T[0,1];
  gamma1 ~ normal(0.25, 0.05)T[0,];
  gamma2 ~ normal(0.25, 0.05)T[0,];
  nu ~ normal(0.0015, 0.0005)T[0,];

  phi_inv1 ~ exponential(5);
  phi_inv2 ~ exponential(5);

  for (t in 1:n_days) {
    cases1[t] ~ neg_binomial_2(fmax(1e-6, beta1 * y[t,1] * y[t,2] / N - (mu + gamma1) * y[t,2]), phi1);
    cases2[t] ~ neg_binomial_2(fmax(1e-6, y[t,3]), phi2);
  }
}

generated quantities {
array[n_days + nfst_days] real pred_cases1;
array[n_days + nfst_days] real pred_cases2;
  for (t in 1:(n_days + nfst_days)) {
    pred_cases1[t] = neg_binomial_2_rng(fmax(1e-6, beta1 * y[t,1] * y[t,2] / N - (mu + gamma1) * y[t,2]), phi1);
    pred_cases2[t] = neg_binomial_2_rng(fmax(1e-6, y[t,3]), phi2);
  }

  real R0_1 = beta1 / gamma1;
  real R0_2 = beta2 / gamma2;
  real VaccinationRate = nu;
}

