functions {




    array[] real ode(real t, array[] real y, array[] real theta, array[] real x_r, array[] int x_i) { 

 
    real Lambda = theta[1];
    real beta1 = theta[2];
    real beta2 = theta[3];
    real mu = theta[4];
    real gamma1 = theta[5];
    real gamma2 = theta[6];
    real delta1 = theta[7];
    real delta2 = theta[8];
    real alpha1 = theta[9];
    real alpha2 = theta[10];
    real sigma12 = theta[11];
    real sigma21 = theta[12];
    real N = x_i[1];
    real i0 = x_i[2];
    array[7] real init = {N-i0, 0, i0, 0, i0, 0, 0};

    real S = y[1]+init[1];
    real I1 = y[2]+init[2];
    real I2 = y[3]+init[3];
    real Q1 = y[4]+init[4];
    real Q2 = y[5]+init[5];
    real R1 = y[6]+init[6];
    real R2 = y[7]+init[7];

    real dS_dt = Lambda - (beta1 * S * I1 / N) - (beta2 * S * I2 / N) - mu * S;
    real dI1_dt = beta1 * S * I1 / N - (mu + gamma1 + delta1) * I1;
    real dI2_dt = beta2 * S * I2 / N - (mu + gamma2 + delta2) * I2;
    real dQ1_dt = delta1 * I1 - (mu + alpha1) * Q1;
    real dQ2_dt = delta2 * I2 - (mu + alpha2) * Q2;
    real dR1_dt = gamma1 * I1 + alpha1 * Q1 - beta2 * sigma12 * R1 * I2 / N - mu * R1;
    real dR2_dt = gamma2 * I2 + alpha2 * Q2 - beta1 * sigma21 * R2 * I1 / N - mu * R2; 
 return{dS_dt,dI1_dt,dI2_dt,dQ1_dt,dQ2_dt,dR1_dt,dR2_dt};
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
    real<lower=0> delta1;
    real<lower=0> delta2;
    real<lower=0> alpha1;
    real<lower=0> alpha2;
    real<lower=0, upper=1> sigma12;
    real<lower=0, upper=1> sigma21;
    real<lower=0> phi_inv1;
    real<lower=0> phi_inv2;
}
transformed parameters {
  array[n_days + nfst_days, 7] real y;
  real phi1 = 1.0 / phi_inv1;
  real phi2 = 1.0 / phi_inv2;
  array[12] real theta;
  theta[1] = Lambda;
  theta[2] = beta1;
  theta[3] = beta2;
  theta[4] = mu;
  theta[5] = gamma1;
  theta[6] = gamma2;
  theta[7] = delta1;
  theta[8] = delta2;
  theta[9] = alpha1;
  theta[10] = alpha2;
  theta[11] = sigma12;
  theta[12] = sigma21;

  y = integrate_ode_rk45(ode, rep_array(0.0, 7), t0, ts, theta, x_r, x_i);
}
model {
  Lambda ~ normal(0, 1)T[0,];
  beta1 ~ normal(0, 1)T[0,];
  beta2 ~ normal(0, 1)T[0,];
  mu ~ normal(0, 1)T[0,];
  gamma2 ~ normal(0, 10)T[0,];
  delta1 ~ normal(0, 1)T[0,];
  delta2 ~ normal(0, 1)T[0,];
  alpha1 ~ normal(0, 1)T[0,];
  alpha2 ~ normal(0, 1)T[0,];
  sigma12 ~ uniform(0, 1);
  sigma21 ~ uniform(0, 1);

  phi_inv1 ~ exponential(5);
  phi_inv2 ~ exponential(5);

  for (t in 1:n_days) {
    cases1[t] ~ neg_binomial_2(fmax(1e-6, delta1 * y[t,2] - (mu + alpha1) * y[t,4]), phi1);
    cases2[t] ~ neg_binomial_2(fmax(1e-6, y[t,5]), phi2);
  }
}

generated quantities {
array[n_days + nfst_days] real pred_cases1;
array[n_days + nfst_days] real pred_cases2;
  for (t in 1:(n_days + nfst_days)) {
    pred_cases1[t] = neg_binomial_2_rng(fmax(1e-6, delta1 * y[t,2] - (mu + alpha1) * y[t,4]), phi1);
    pred_cases2[t] = neg_binomial_2_rng(fmax(1e-6, y[t,5]), phi2);
  }

  real R0_1 = beta1 / (gamma1 + delta1);
  real R0_2 = beta2 / (gamma2 + delta2);
  real CrossReinf = sigma12 + sigma21;
}

