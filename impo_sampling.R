h=1; r=0.02; t=1; x=0;
a=15;b=-5;sig=0.5; u=r+sig*(sqrt(a^2-(b+1)^2)-sqrt(a^2-b^2));
sum=0;
boti=1i;
sign=1;
price=0;
Fx=0;

  for (m in 1:1000){
    if (m==1000) sign=0.5;
    fi=exp(u*t*m*h*boti-sig*t*(sqrt(a^2-(b+m*h*boti)^2)-sqrt(a^2-b^2)));
    fineg=exp(-h*u*t*m*boti-sig*t*(sqrt(a^2-(b-m*h*boti)^2)-sqrt(a^2-b^2)));
    
  sum=sum+sign*((exp(boti*m*h*x)*fineg-exp(-boti*m*h*x)*fi)/(m*h*boti))
  }
m=1;h=0;
fizero=exp(u * t * m * h * boti - sig * t * (sqrt(a^2 - (b + m * h *boti)^2) - sqrt(a^2 - b^2))) * (u * t * m * boti + sig * t * (0.5 * (2 * (m * boti * (b + m * h * boti)) * (a^2 -(b + m * h * boti)^2)^-0.5)));
Fx=0.5+(0.5/pi)*(sum-(fizero/boti));
price=exp(-r*t)*Fx;
