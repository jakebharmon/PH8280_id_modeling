functions {




    array[] real ode(real t, array[] real y, array[] real theta, array[] real x_r, array[] int x_i) { 

 
    real beta1 = theta[1];
    real beta2 = theta[2];
    real gamma1 = theta[3];
    real gamma2 = theta[4];
    real nu = theta[5];
    real rho = theta[6];
    real Lambda = x_i[1];
    real N = x_i[2];
    real i0 = x_i[3];
    real mu = x_r[1];
    real S = y[1];
    real I1 = y[2];
    real I2 = y[3];
    real R1 = y[4];
    real R2 = y[5];
    real V = y[6];

    real dS_dt = Lambda - (beta1 * S * I1 / N) - (beta2 * S * I2 / N) - nu * S - mu * S;
    real dI1_dt = (beta1 * S * I1 / N) - (mu + gamma1) * I1;
    real dI2_dt = (beta2 * S * I2 / N) - (mu + gamma2) * I2;
    real dR1_dt = gamma1 * I1 - mu * R1;
    real dR2_dt = gamma2 * I2 - mu * R2;
    real dV_dt = nu * S - mu * V;
    real y1 = rho * I1;
    real y2 = rho * I2;
    real y3 = V; 
 return{dS_dt,dI1_dt,dI2_dt,dR1_dt,dR2_dt,dV_dt};
 }

}
  
data {
    int<lower=1> n_days;
    int<lower=0> nfst_days;array[6] real y0;
real t0;
array[n_days + nfst_days] real ts;
array[n_days] int cases1;
array[n_days] int cases2;
    int Lambda;
    int N;
    int i0;
    real mu;
}
  
transformed data {
    array[1] real x_r = {mu};
    array[3] int x_i = {Lambda,N,i0};

  }parameters {
    real<lower=0, upper=2> beta1;
    real<lower=0, upper=2> beta2;
    real<lower=0, upper=1> gamma1;
    real<lower=0, upper=1> gamma2;
    real<lower=0, upper=0.01> nu;
    real<lower=0, upper=1> rho;
    real<lower=0> phi_inv1;
    real<lower=0> phi_inv2;
}
transformed parameters {
  array[n_days + nfst_days, 6] real y;
  real phi1 = 1.0 / phi_inv1;
  real phi2 = 1.0 / phi_inv2;
  array[6] real theta;
  theta[1] = beta1;
  theta[2] = beta2;
  theta[3] = gamma1;
  theta[4] = gamma2;
  theta[5] = nu;
  theta[6] = rho;

  y = integrate_ode_rk45(ode, y0, t0, ts, theta, x_r, x_i);
}
model {
  beta1 ~ normal(0.4, 0.5)T[0,];
  beta2 ~ normal(0.4, 0.5)T[0,];
  gamma1 ~ normal(0.25, 0.05)T[0,];
  gamma2 ~ normal(0.25, 0.05)T[0,];
  nu ~ normal(0.0019, 0.0006)T[0,];
  rho ~ beta(2, 4);

  phi_inv1 ~ exponential(5);
  phi_inv2 ~ exponential(5);

  for (t in 1:n_days) {
    cases1[t] ~ neg_binomial_2(fmax(1e-6, (beta1 * y[t,1] * y[t,2] / N) - (mu + gamma1) * y[t,2]), phi1);
    cases2[t] ~ neg_binomial_2(fmax(1e-6, (beta2 * y[t,1] * y[t,3] / N) - (mu + gamma2) * y[t,3]), phi2);
  }
}

generated quantities {
array[n_days + nfst_days] real pred_cases1;
array[n_days + nfst_days] real pred_cases2;
  for (t in 1:(n_days + nfst_days)) {
    pred_cases1[t] = neg_binomial_2_rng(fmax(1e-6, (beta1 * y[t,1] * y[t,2] / N) - (mu + gamma1) * y[t,2]), phi1);
    pred_cases2[t] = neg_binomial_2_rng(fmax(1e-6, (beta2 * y[t,1] * y[t,3] / N) - (mu + gamma2) * y[t,3]), phi2);
  }

  real R0_1 = beta1 / gamma1;
  real R0_2 = beta2 / gamma2;
}

