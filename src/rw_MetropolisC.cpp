#include <Rcpp.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
using namespace Rcpp;
using namespace std; 

//' @title Random sampler between 0 and 1 using Rcpp
//' @description Generate n random numbers between 0 and 1 using Rcpp
//' @param n the number of samples
//' @return a random sample of size \code{n}
//' @examples
//' \dontrun{
//' rnC <- runifC(100)
//' plot(rnC,type='l')
//' }
//' @export
// [[Rcpp::export]]
NumericVector runifC(int n){
  NumericVector a(n);
  srand((unsigned)time(NULL)); 
  for(int i = 0; i < n;i++ )
    a[i]=rand()/double(RAND_MAX);
  return a;
} 

#include <Rcpp.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
using namespace Rcpp;
using namespace std;

//' @title A normally distributed sampler using Rcpp
//' @description Generate a normally distributed random sample using Rcpp
//' @param E the mean of a normal distribution
//' @param se the standard error of a normal distribution
//' @return a random sample of size 1
//' @examples
//' \dontrun{
//' rnC <- replicate(100,rnormC(5,1))
//' plot(rnC,type='l')
//' }
//' @export
// [[Rcpp::export]]
double rnormC(double E,double se){
  static double V1,V2,S;
  static int phase=0;
  double X;
  if (phase == 0){
    do {
      double U1 = (double)rand()/RAND_MAX;
      double U2 = (double)rand()/RAND_MAX;
      
      V1 = 2 * U1 - 1;
      V2 = 2 * U2 - 1;
      S = V1 * V1 + V2 * V2;
    } while(S >= 1 || S == 0);
    X = V1 * sqrt(-2 * log(S) / S);
  } else
    X = V2 * sqrt(-2 * log(S) / S);  
  phase = 1 - phase;
  X = se*X+E;
  return X;
} 

#include <Rcpp.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
using namespace Rcpp;
using namespace std;

//' @title A random walk Metropolis sampler using Rcpp
//' @description A random walk Metropolis sampler for generating the standard Laplace distribution using Rcpp
//' @param sigma given standard deviation
//' @param x0 the initial value given
//' @param N the length of the resulting chain
//' @return a list of a chain of size \code{n} and the number of times it is accepted during the process of using this method
//' @examples
//' \dontrun{
//' N <- 2000
//' rwk <- rw_MetropolisC(2,20,N)
//' plot(rwk$x,type='l')
//' accept.rate <- rwk$k/N
//' }
//' @export
// [[Rcpp::export]]
List rw_MetropolisC(double sigma,double x0,int N){
  NumericVector x(N);
  x[0]=x0;
  NumericVector u(N);
  u=runifC(N);
  double y;
  int k = 0;
  for (int i=1;i<N;i++) {
    y = rnormC(x[i-1],sigma);
    if (u[i] <= exp(abs(x[i-1])-abs(y))){
      x[i] = y;
      k = k+1;
    }
    else {
      x[i] = x[i-1];
    }
  }
  List out(2);
  out[0] = x;
  out[1] = k;
  return out;
}

