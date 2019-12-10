#include <Rcpp.h>
#include <iostream>
#include<algorithm> 
#include <R.h>
#include <Rinternals.h>

using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]
double thtC(double y)
{
  if (0 < y && y <= 0.25) {
    return 0.015;
  } else if (0.25 < y && y <= 0.75) {
    return 0.10;
  } else if (0.75 < y && y <= 1.00) {
    return 0.20;
  } else if (1.00 < y && y <= 1.50) {
    return 0.25;
  } else if (1.50 < y && y <= 2.50) {
    return 0.50;
  } else if (2.50 < y && y <= 3.50) {
    return 0.80;
  } else if (y > 3.50) {
    return 1.00;
  } else {
    return 0;
  }
}


// [[Rcpp::export]]
/*Computes the p-value from the empirical distribution of the charting statistic*/
double findPvalue1sC (NumericVector dist, double obsStat)
{
	double prop = 0;
	int n = dist.size();
	
	for (int i = 0; i < n; i++) {
		if (dist[i] >= obsStat) {
			prop += 1;
		}
	}
	return prop/n;
}


// [[Rcpp::export]]
/*Estimates the IC Empirical Distribution of the AEWMA chart using the Bootstrap Method*/
NumericVector empirW (NumericVector x, int simutime, int n)
{
	NumericVector out(simutime);
	NumericVector empW;
	int m = x.size();
	
	for (int j = 0; j < simutime; j++) {
		double z; 
		double omg = 0.1; double detHatSt = 0; double detHatStSt = 0;
		double detTil;
		double W = 0; int b;
  
		for (int i = 0; i < n; i++) {
			b = round(R::runif(0,m));
			z = x[b];
			//z = R::rnorm(0,1);
			detHatSt = omg*z + (1-omg)*detHatSt;
			detHatStSt = detHatSt/(1 - pow((1-omg), i));
			detTil = abs(detHatStSt);
			W = thtC(detTil)*z + (1-thtC(detTil))*W;
		}
		out[j] = W;
	}
	empW = clone(out);
	std::sort(empW.begin(), empW.end());			//sorts in ascending order
	std::reverse(empW.begin(), empW.end());			//sorts in descending order
	return empW;
}


// [[Rcpp::export]]
/*Estimates the ARL of the AEWMA control chart with p-values.*/
NumericVector arlC (double av, int ww, int simutime, double shift)
{
	// Computation of ARL
	NumericVector out(2);
	double arl = 0.0; int j = 0; int count = 0;
	
	
	while (j < simutime) {
		j = j + 1;
		double p = 1; 
		int rl = 0;
		NumericVector x = rnorm(1000);
		NumericVector sortedW = empirW(x, simutime, ww);
  
		double z; double W = 0;
		double omg = 0.1; double detHatSt = 0; double detHatStSt = 0;
		double detTil;
  
		// Phase-I SPC
		while ((p > av) && (rl < ww)) {
			rl = rl + 1;
			z = R::rnorm(0,1);
			detHatSt = omg*z + (1-omg)*detHatSt;
			detHatStSt = detHatSt/(1 - pow((1-omg), rl));
			detTil = abs(detHatStSt);
			W = thtC(detTil)*z + (1-thtC(detTil))*W;
			p = findPvalue1sC(sortedW, W);
		}
		//return p;
  
		if(rl == ww){
			count = count + 1;
		} else {
			continue;		// if steady-state is not reached it skips to the next iteration
		}
  
		// Phase-II SPC
  
		rl = 0;
		while((p > av) && (rl < simutime)) {
			rl = rl + 1;
			z = R::rnorm(0,1) + shift;
			detHatSt = omg*z + (1-omg)*detHatSt;
			detHatStSt = detHatSt/(1 - pow((1-omg), rl));
			detTil = abs(detHatStSt);
			W = thtC(detTil)*z + (1-thtC(detTil))*W;
			p = findPvalue1sC(sortedW, W);
		}
		arl = arl + rl;
	}
	out[0] = arl; out[1] = count;
	return(out);
}


// [[Rcpp::export]]
/*Computes the ARL & ATS for the AEWMA chart with a dynamic sampling scheme.*/
NumericVector arl_atsC (double alpha, int ww, int simutime, double a, double lambda, double b, double shift)
{
	// Computation of ARL and ATS
	NumericVector out(3);
	double arl = 0.0; double ats = 0.0; int j = 0; int count = 0;
	double interval = 0;
	
	while (j < simutime) {
		j = j + 1;
		double p = 1; 
		int rl = 0; double ts = 0.0;
		
		// Estimates the empirical distribution
		NumericVector x = rnorm(1000);
		NumericVector sortedW = empirW(x,simutime, ww);
  
		double z; double W = 0;
		double omg = 0.1; double detHatSt = 0; double detHatStSt = 0;
		double detTil;
  
		// Phase-I SPC
		while ((p > alpha) && (rl < ww)) {
			rl = rl + 1;
			z = R::rnorm(0,1);
			detHatSt = omg*z + (1-omg)*detHatSt;
			detHatStSt = detHatSt/(1 - pow((1-omg), rl));
			detTil = abs(detHatStSt);
			W = thtC(detTil)*z + (1-thtC(detTil))*W;
			p = findPvalue1sC(sortedW, W);
		}
		//return p;
  
		if(rl == ww){
			count = count + 1;
		} else {
			continue;		// if steady-state is not reached it skips to the next iteration
		}
  
		// Phase-II SPC
  
		rl = 0;	interval = 0;
		while((p > alpha) && (rl < simutime)) {
			rl = rl + 1;
			interval = a + b*pow(p, lambda);
			ts = ts + interval;
			z = R::rnorm(0,1);
			z = z + shift;
			detHatSt = omg*z + (1-omg)*detHatSt;
			detHatStSt = detHatSt/(1 - pow((1-omg), rl));
			detTil = abs(detHatStSt);
			W = thtC(detTil)*z + (1-thtC(detTil))*W;
			p = findPvalue1sC(sortedW, W);
		}
		arl = arl + rl;
		ats = ats + ts;
	}
	out[0] = arl; out[1] = ats; out[2] = count;
	return(out);
}


// [[Rcpp::export]]
/*Component for estimating the ARL of the two-sided Adaptive EWMA chart*/
NumericVector arla (double h, double omg, double shift)
{
	int itr =100000;
	NumericVector RL(100000);
	
	for (int i = 0; i < itr; i++ ) {
		
		double detHatSt = 0.0; double W = 0;
		double detHatStSt; double detTil; double z;
		int t = 1;
		while(t >= 1) {
			z = R::rnorm(0,1);
			z = z + shift;
			detHatSt = omg*z + (1-omg)*detHatSt;
			detHatStSt = detHatSt/(1 - pow((1-omg), t));
			detTil = abs(detHatStSt);
			W = thtC(detTil)*z + (1-thtC(detTil))*W;
		
			if(abs(W) > h){
				RL[i] = t; 
				break;
			} else {
				t = t + 1;
			}
		}
	}
	return RL;
}


// [[Rcpp::export]]
/*Component for estimating the ARL of the one-sided Adaptive EWMA chart*/
NumericVector arlb (double h, double omg, double shift)
{
	int itr =100000;
	NumericVector RL(100000);
	
	for (int i = 0; i < itr; i++ ) {
		
		double detHatSt = 0.0; double W = 0;
		double detHatStSt; double detTil; double z;
		int t = 1;
		while(t >= 1) {
			z = R::rnorm(0,1);
			z = z + shift;
			detHatSt = omg*z + (1-omg)*detHatSt;
			detHatStSt = detHatSt/(1 - pow((1-omg), t));
			detTil = abs(detHatStSt);
			W = thtC(detTil)*z + (1-thtC(detTil))*W;
		
			if(W > h){
				RL[i] = t; 
				break;
			} else {
				t = t + 1;
			}
		}
	}
	return RL;
}
