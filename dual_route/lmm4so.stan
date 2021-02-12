data {
  int<lower=1> N;                            
  real<lower=0> y[N];                    
  real<lower=0> x1[N];                    
  real<lower=0> x2[N];              
  real<lower=0> x3[N];                   
  int<lower=-1,upper=1> x4[N];   
  real<lower=0> x5[N];                
  real<lower=0> x6[N]; 
  int<lower=1> J;                            //number of subjects
  int<lower=1> K;                            //number of items
  int<lower=1, upper=J> subj[N];             //subject id
  int<lower=1, upper=K> item[N];             //item id
}

parameters {
  vector[J] u;                                    //subject intercepts
  vector[K] w;                                    //item intercepts
  real<lower=0> sigma_w;                          //item sd
  real<lower=0> sigma_u;                          //subj sd
  real<lower=0> sigma_e;                          //error sd
  real<lower=0> intercept;                        //intercept - must be positive
  real<lower=0> pp;                               //penalty - must be positive
  real b1;                                        //beta
  real  b2;                                       //beta
  real  b3;                                       //beta
  real b5;                                        //beta
  real  b6;                                       //beta
}

model {
  real mu_route1;
  real mu_route2;
  real mu;                    //priors
  u ~ normal(0, sigma_u);    //subj random effects
  w ~ normal(0, sigma_w);    //item random effects
  pp ~ normal(0.5, 100);   //penalty - high SD due to uncertainty about effect
  
  // priors based on other data
  b1 ~ normal(0.252191, 0.903787);
  b2 ~ normal(0.018582, 0.4455987); 
  b3 ~ normal(0.199111, 0.9162555);
  b5 ~ normal(-0.052183, 0.7654226); 
  b6 ~ normal(-0.056539, 0.8138441);
  
  for (i in 1:N) {  

    if (x4[i] == -1) {                       // simple cases
    
      mu = intercept + b1*x1[i] + b2*x2[i] + b3*x3[i] + b5*x5[i] + u[subj[i]] + w[item[i]];
      
    } else if (x4[i] == 1) {               // complex cases
    
      mu_route1 = intercept + b1*x1[i] + b2*x2[i] + b3*x3[i] + b6*x6[i] + u[subj[i]] + w[item[i]];

      mu_route2 = intercept + b1*x1[i] + b2*x2[i] + b3*x3[i] + b5*x5[i] + pp + u[subj[i]] + w[item[i]];
      
      mu = fmin(mu_route1, mu_route2);
    }
    
    y[i] ~ normal(mu, sigma_e);
    
  }
}

