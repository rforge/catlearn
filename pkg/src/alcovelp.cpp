// ALCOVE model, implemented as a list processor
// Written in C++, using templates from Rcpp.h
// About 6x faster than vectorized-where-possible R code
// Andy Wills
// 2014-11-17
#include <Rcpp.h>
using namespace Rcpp;

// Calculate distances on each dimension from presented
// stimulus to each of hidden nodes (used twice in ALCOVE)
NumericMatrix hmxcalc(NumericMatrix h, NumericVector x) {
  int i, j, nrow = h.nrow(), ncol = h.ncol();
  NumericMatrix out(nrow,ncol);
  for(i=0;i < nrow; i++) {
    for(j=0;j < ncol; j++) {
      out(i,j) = fabs( h(i,j) - x(i));  
    }
  }
  return out;
}

// Calculate activation of hidden nodes
NumericVector ahcalc(NumericMatrix hmx, NumericVector m, NumericVector alpha, double c, double q, double r) {
  int i, j, nrow = hmx.nrow(), ncol = hmx.ncol();
  NumericVector out(ncol);
  for(j=0;j < ncol; j++) {
    out(j) = 0.0;
    for(i=0;i < nrow; i++) {
      if( m[i] == 0 ) out(j) += pow(hmx(i,j),r) * alpha(i);
    }
    out(j) = pow( out(j), 1.0 / r);
    out(j) = exp( -1.0 * c * pow(out(j),q));
  }
  return out;
}

// Calculate activation of output nodes
NumericVector aocalc(NumericMatrix w, NumericVector ah) {
  int i,j, nrow = w.nrow(), ncol = w.ncol();
  NumericVector out(nrow);
  for(j=0;j < nrow; j++) {
    out(j) = 0.0;
    for(i=0;i < ncol; i++) {
      out(j) += ah(i) * w(j,i);
    }
  }
  return out;
}

// Exponential Ratio Rule decision mechanism.
NumericVector expratio(NumericVector act, double phi) {
  int n = act.size();
  NumericVector prob(n);
  int i;
  double denom = 0.0;
  for(i=0; i < n; i++) {
      prob[i] = exp( phi * act[i] );
      denom += prob[i];
  }
  for(i=0; i < n; i++) {
      prob[i] /= denom;
  }
  return prob;
}

// Humble teacher
NumericVector humble(NumericVector t, NumericVector ao) {
  int i, n= t.size();
  NumericVector out(n);
  for(i=0;i < n; i++) {
    out[i] = t[i];
    if(out[i] == 1) out[i] = max(NumericVector::create(1.0,ao[i]));
    if(out[i] == -1) out[i] = min(NumericVector::create(-1.0,ao[i]));
  }
  return out;
}

// Calculate prediction error (at output nodes)
NumericVector prederr(NumericVector t, NumericVector ao) {
  int i, n= t.size();
  NumericVector out(n);
  for(i=0;i < n; i++) {
    out[i] = t[i] - ao[i];
  }
  return out;
}

// Calculate delta for associative links
NumericMatrix deltawcalc(double lw, NumericVector pe, NumericVector ah) {
  int i,j, nout = pe.size(), nhid = ah.size();
  NumericMatrix delt(nout,nhid);
  for(i=0; i < nout; i++) {
    for(j=0; j < nhid; j++) {
      delt(i,j) = lw * pe[i] * ah[j];
    }
  }
  return delt;
}

// Back propagate error to hidden nodes (part of attentional learning)
NumericVector bperr(NumericVector ah, NumericVector pe, NumericMatrix w) {
  int i,j, nout = pe.size(), nhid=ah.size();
  NumericVector bp(nhid);
  for(j=0; j < nhid; j++) {
    bp[j] = 0.0;
    for(i=0; i < nout; i++) {
      bp[j] += pe[i] * w(i,j);
    }
    bp[j] *= ah[j];
  }
  return bp;  
}

// Calculate delta for attentional weights
NumericVector delalphcalc(double la, NumericMatrix hmx, NumericVector m, NumericVector bp, double c) {
  int i,j, nhid = hmx.ncol(), nin = hmx.nrow();
  NumericVector delt(nin);
  for(i=0; i < nin; i++) {
    delt[i] = 0.0;
    if(m[i] == 0)
    {
      for(j=0; j < nhid; j++) {
        delt[i] += bp[j] * c * hmx(i,j);
      }
    }
    delt[i] *= -1.0 * la;
  }
  return delt;
}

// Update associative link strength
NumericMatrix wupdate(NumericMatrix w, NumericMatrix delt) {
  int i, j, nhid = w.ncol(), nout = w.nrow();
  NumericMatrix nw(nout,nhid);
  for(i=0; i < nout; i++) {
    for(j=0; j < nhid; j++) {
      nw(i,j) = w(i,j) + delt(i,j);
    }
  }
  return nw;
}

// Update attentional weights
NumericVector aupdate(NumericVector al, NumericVector delt) {
  int i, n = al.size();
  NumericVector nal(n);
  for(i=0; i < n; i++) {
      nal[i] = al[i] + delt[i];
      if(nal[i] < 0) nal[i] = 0;
  }
  return nal;
}

// Run a single trial of the ALCOVE network
List alcovetrial(NumericVector x, NumericVector tin, NumericVector m, double c, double phi, double la, double lw, 
double r, double q, NumericMatrix h, NumericVector alpha, NumericMatrix w) {
  int nin = x.size(), nhid = w.ncol(), nout = w.nrow();
  NumericMatrix ahmx(nin,nhid), deltaw(nout,nhid), nw(nout,nhid);
  NumericVector ah(nhid), ao(nout), prob(nout), t(nout), pe(nout), bp(nhid), deltaa(nin), na(nin);
  
  ahmx = hmxcalc(h,x); // Stimulus to hidden node distances
  ah = ahcalc(ahmx,m,alpha,c,q,r); // Hidden node activations
  ao = aocalc(w,ah); // Output node activations
  prob = expratio(ao,phi); // Response probability
  t = humble(tin,ao); // Teaching signal (humble teacher)
  pe = prederr(t,ao); // Prediction error
  deltaw = deltawcalc(lw,pe,ah);  // Delta for associative weights
  bp = bperr(ah,pe,w); // Back-prop of error
  deltaa = delalphcalc(la,ahmx,m,bp,c); // Delta for attention weights
  nw = wupdate(w,deltaw); // Update associative weights
  na = aupdate(alpha,deltaa); // Update attenional weights
  
  return Rcpp::List::create(Rcpp::Named("alpha") = na,
                            Rcpp::Named("w") = nw,
                            Rcpp::Named("prob") = prob);
}

// [[Rcpp::export]]
NumericMatrix alcovelp(List st,NumericMatrix tr) {
  // ALCOVE list processor function
  // Takes in a set of parameters and a list of training items
  // Outputs by-trial response probabilities
  
  // This clumsy section copies stuff out of an R List
  // There seems to be no way in RCpp to get direct access to a List at input?
  double c = as<double>(st["c"]);
  double phi = as<double>(st["phi"]);
  double la = as<double>(st["la"]);
  double lw = as<double>(st["lw"]);
  double r = as<double>(st["r"]);
  double q = as<double>(st["q"]);
  NumericMatrix h = as<NumericMatrix>(st["h"]);
  NumericVector initalpha = as<NumericVector>(st["alpha"]);
  NumericMatrix initw = as<NumericMatrix>(st["w"]);
  int colskip = as<int>(st["colskip"]);
  int i, trial, items = tr.nrow(), nin = initalpha.size(), nhid = initw.ncol(), nout = initw.nrow();
  NumericMatrix prob(items,nout);
  NumericVector x(nin);
  NumericVector t(nout);
  NumericVector alpha(nin);
  NumericMatrix w(nout,nhid);
  NumericVector m(nin);
  List alcout;
  alpha = initalpha;
  w = initw;
  // RUN THROUGH THE TRAINING LIST 
  for(trial = 0; trial < items; trial++) {
    if( tr(trial,0) == 1 ) { // Code to reset network to initial state
      alpha = initalpha;
      w = initw;
    }
    // Load input stimulus
    for(i = colskip; i < nin+colskip; i++) {
      x[i-colskip] = tr(trial,i);
    }
    // Load teaching signal
    for(i = colskip+nin; i < colskip+nin+nout; i++) {
      t[i-colskip-nin] = tr(trial,i);
    }
    // Load missing dimension flags
    for(i = colskip+nin+nout; i < colskip+nin+nout+nin; i++) {
      m[i-colskip-nin-nout] = tr(trial,i);
    }
    // Run one trial of ALCOVE
    if( tr(trial,0) == 2 ) 
    { // Code to freeze learning
      alcout = alcovetrial(x,t,m,c,phi,0.0,0.0,r,q,h,alpha,w); 
    }
    else
    {
      alcout = alcovetrial(x,t,m,c,phi,la,lw,r,q,h,alpha,w);
    }
    
    // Retrive variables from returned list
    NumericVector alpharet = as<NumericVector>(alcout["alpha"]);
    NumericMatrix wret = as<NumericMatrix>(alcout["w"]);
    NumericVector probret = as<NumericVector>(alcout["prob"]);
    // Update network state.
    alpha = alpharet;
    w = wret;
    // Store probability for return
    for(i = 0; i < nout; i++) {
      prob(trial,i) = probret[i];
    }
  } // Run next trial
  return prob;
}