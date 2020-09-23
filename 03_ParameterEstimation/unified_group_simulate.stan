
functions{

  //calculates S during wake
  real Sfun(real sw, //sw = S upon waking
             real taw, //taw = time awake
             real tau_r, //tau_r = controls rate of rise in S during wake
             real U0
             ){ 
    
    real r=exp(-taw/tau_r);        
    real S=U0-r*(U0-sw);
    return S;
  }
  
  //calculates S during sleep
  real Spfun(real ss, //ss = S upon falling asleep
             real tas, //tas = time asleep
             real tau_s, //tau_d = controls rate of decay in S during sleep
             real U0,     //upper asymptote
             real tau_la, //rate of change in lower asymptote
             real ls     //lower asymptote at sleep onset
             ){ 
    
    real term1 = ss*exp(-tas/tau_s);
    real term2 = -2*U0*(1-exp(-tas/tau_s));
    real term3 = (((ls + 2*U0)*tau_la)/(tau_la-tau_s)) * (exp(-tas/tau_la)-exp(-tas/tau_s));            
    real Sp=term1+term2+term3;
    return Sp;
  }
  
 //calculates L during wake
  real Lfun(real lw, //lower asymptote upon falling asleep
           real taw,    //time asleep
           real tau_la, //rate of change in lower asymptote
           real U0      //upper asymptote
          ){
    real term1 = lw*exp(-taw/tau_la);
    real term2 = U0*(1-exp(-taw/tau_la));
    real L=term1+term2;
    return L;
  }
  
  
  
  //calculates L during sleep
  real Lpfun(real ls, //lower asymptote upon falling asleep
           real tas,    //time asleep
           real tau_la, //rate of change in lower asymptote
           real U0      //upper asymptote
          ){
    real term1 = ls*exp(-tas/tau_la);
    real term2 = -2*U0*(1-exp(-tas/tau_la));
    real L=term1+term2;
    return L;
  }
  
  
  //calculates C (circadian process)
  real Cfun(real tod, //tod = time of day (in decimal hours)
            real phi,  //phi = phase 
            real tau, //tau = period of C process
            real A //amplitude of process
            ){
    
    real omega = 2*pi()/tau;
    real term1 = 0.97*sin(omega*(tod+phi));
    real term2 = 0.22*sin(2*omega*(tod+phi));
    real term3 = 0.07*sin(3*omega*(tod+phi));
    real term4 = 0.03*sin(4*omega*(tod+phi));
    real term5 = 0.0001*sin(5*omega*(tod+phi));
    real C = A*(term1+term2+term3+term4+term5);
    return C;
  }


}

data {
  int<lower=0> Nsubj;
  int<lower=0> Ntotal;
  int<lower=0> subject[Ntotal];
  int<lower=0> event_number[Ntotal];
  int<lower=0> previous_episode_type[Ntotal];
  real<lower=0> time_since_previous[Ntotal];
  vector<lower=0>[Ntotal] timeofday;
  vector<lower=0>[Ntotal] fatigue;
  int<lower=0> Nvalid;
  int<lower=0> valid[Nvalid];
  real<lower=0> U0;
  real<lower= 0, upper=1> S0_raw;
  real<lower=-2,upper=1> L0_raw;
  real phi;
  real<lower=0> kappa;
  real<lower=0> tau_d; 
  real<lower=0> tau_r; 
  real<lower=0> tau_la;
  real<lower=0> sigma; 

}  
 
parameters {

  //parameters
  real<lower=0,upper=1> dummy;
  
}

transformed parameters {

  //Fixed parameters
  real tau = 24;
  real A = 1;
  
  //Calculate level of processes for each observation
  real s_prev;  //level of homeostatic process at previous event
  real l_prev;  //level of lower asymptote
  vector[Ntotal] S; //level of homeostatic process
  vector[Ntotal] C; //level of 24-hour circadian process
  vector[Ntotal] L; //lower assumptote of homeostatic process


  real L0 = L0_raw*U0;
  real S0 = L0 + (U0-L0)*S0_raw; 


  //Calculate S and C for each event
  for(i in 1:Ntotal){
    //if it is subject's first event, assign S to be S0;
    if(event_number[i] == 1){
      S[i] = S0;
      L[i] = L0;
      s_prev = S[i];
      l_prev = L[i];
    }
    //if it is not subject's first event, update S based on S at previous event
    if(event_number[i] > 1){
      //if most recent episode was sleep (1)
      if(previous_episode_type[i] == 1){
        S[i] = Spfun(s_prev,time_since_previous[i],tau_d,U0,tau_la,l_prev);
        L[i] = Lpfun(l_prev,time_since_previous[i],tau_la,U0);
        s_prev = S[i];
        l_prev = L[i];

      }
      //if most recent episode was wake (2)
      if(previous_episode_type[i] == 2){
        S[i] = Sfun(s_prev,time_since_previous[i],tau_r,U0);
         L[i] = Lfun(l_prev,time_since_previous[i],tau_la,U0);
        s_prev = S[i];
        l_prev = L[i];

      }
    }  
    
    C[i]=Cfun(timeofday[i],phi,tau,A);
    
  }
}

model {
  
//empty
}

//generated a simulated data list

generated quantities{
  real pp[Ntotal];
  
  for(i in 1:Ntotal){
    
    pp[i] = normal_rng(S[i]+kappa*C[i],sigma);   
  }

}
