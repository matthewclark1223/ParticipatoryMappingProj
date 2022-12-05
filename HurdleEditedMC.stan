

data{
    int N; // observations in the dataset
    int Y[N]; //Outcome (times patrolled) reported
    real Decl[N]; // percent of mang area percieved as declining
    real Theft[N]; // stealers per week
    real Area[N]; // Mangrove area
    int Gen[N];
    int REDD[N]; //Redd shehia or not (1,0)
    int S[N]; //Vector of shehia for random intercept
    int Memb[N]; //committee member or not (1,0)
    int Ns;// Number of different shehia
}
transformed data{
 int Z[N]; //Create Binary Variable for Hurdle Stage 1. 
 for(i in 1:N){ //for each observation
  Z[i] = Y[i] > 0 ? 1 : 0; //Z is 1 if Y is greater than 0, 0 if else
 }//End contstruct Z
  
}//End Transformed data
parameters{
  real IntBern; 
  real IntNB;
  real<lower=0> phi; //overdisersion parameter for neg binom distrbution
  
  //Non-centered Random Effects
    real ranIntBern[Ns]; 
    real ranIntNB[Ns]; //ax is the random intercept for each shehia 

    real<lower=0> SigmaIntBern; //variance on the random intercepts 
    //real<lower=0> SigmaIntNB; //variance on the random intercepts 
    
    // Fixed effects for both models
    //Bernouli
    real EffDeclBern;
    real EffTheftBern;
    real EffAreaBern;
    real EffREDDBern;
    real EffMembBern;
    real EffGenBern;
    //NB
    real EffDeclNB;
    real EffTheftNB;
    real EffAreaNB;
    real EffREDDNB;
    real EffMembNB;
    real EffGenNB;
   // real EffIntNB;
}


model{
     vector[N] p;
     vector[N] u;
     phi~exponential(1); //over dispersion parameter for NB distribution
     IntBern ~normal(0,1);
     IntNB ~normal(0,1);
    SigmaIntBern~exponential(2);
     //SigmaIntNB~exponential(1);     //this par is causing trouble. Constraining, but maybe make 1?
     //bernouli effect priors
     EffDeclBern~cauchy(0,1);
     EffTheftBern~cauchy(0,1);
     EffAreaBern~cauchy(0,1);
     EffREDDBern~cauchy(0,1);
     EffMembBern~cauchy(0,1);
     EffGenBern~cauchy(0,1);
     //NB model
     EffDeclNB~cauchy(0,1);
     EffTheftNB~cauchy(0,1);
     EffAreaNB~cauchy(0,1);
     EffREDDNB~cauchy(0,1);
     EffMembNB~cauchy(0,1);
     EffGenNB~cauchy(0,1);
     //EffIntNB~cauchy(0,2.5);
     //non-center random effects
     
     
      for(j in 1:Ns){
  ranIntBern[j]~normal(0,SigmaIntBern); //SigmaIntBern
   ranIntNB[j]~normal(0,0.5); //SigmaIntNB
  }
     

    
     for(i in 1:N){
        p[i] = IntBern+ranIntBern[S[i]] + EffDeclBern*Decl[i]+EffTheftBern*Theft[i]+EffGenBern*Gen[i]+
        EffAreaBern*Area[i]+EffREDDBern*REDD[i]+EffMembBern*Memb[i]; //probability of a 0 or 1
        
        u[i] = IntNB+ranIntNB[S[i]] + 
        EffDeclNB*Decl[i]+EffTheftNB*Theft[i]+
        EffGenNB*Gen[i]+EffMembNB*Memb[i]+
        EffAreaNB*Area[i]+EffREDDNB*REDD[i];
        //EffIntNB*Decl[i]*Theft[i]; // mu value for the NB
     }
    u = exp(u);
    Z ~ bernoulli_logit(p);
    
    for(i in 1:N){
      if(Z[i] ==1){
       target += neg_binomial_lpmf(Y[i] | u[i],phi );   
      }
    }
    
  
    
}//End model block
