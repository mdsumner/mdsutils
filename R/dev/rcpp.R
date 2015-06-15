
suppressMessages(library(Rcpp))
suppressMessages(library(inline))

foo <- '
  SEXP  rl = R_NilValue;        // Use this when there is nothing to be returned.
  char* exceptionMesg = NULL;   // msg var in case of error

  try {
    RcppVector<double> lon1(x1);     // vec parameter
    RcppVector<double> lat1(y1);
    RcppVector<double> lon2(x2);     // vec parameter
    RcppVector<double> lat2(y2);
    RcppVector<double> dist(d);


    int nx = lon1.size(), ny = lat1.size();
    //int nx2 = lon1.size(), ny = lat1.size();
    if (nx != ny) throw std::length_error("Wrong vector size");
    for (int i = 1; i < nx; i++)
           dist(i-1) = lon1(i-1) + lon1(i);


    // already available
    //double M_PI = 3.141592653589793;

    double F, G, L, sinG2, cosG2, sinF2, cosF2, sinL2, cosL2, S, C;
    double w, R, a, f, D, H1, H2;
    double lat1R, lat2R, lon1R, lon2R, DE2RA;


    DE2RA = M_PI/180;
    a = 6378.137;              /* WGS-84 equatorial radius in km */
    f = 1.0/298.257223563;     /* WGS-84 ellipsoid flattening factor */
  //  if (fabs(lat1[0] - lat2[0]) < DOUBLE_EPS) {
  //      if (fabs(lon1[0] - lon2[0]) < DOUBLE_EPS) {
  //          dist[0] = 0.0;
  //          return;
  //   /* Wouter Buytaert bug caught 100211 */
  //      } else if (fabs((fabs(lon1[0]) + fabs(lon2[0])) - 360.0) < DOUBLE_EPS) {
  //          dist[0] = 0.0;
  //          return;
  //      }
  //  }
    lat1R = lat1[0]*DE2RA;
    lat2R = lat2[0]*DE2RA;
    lon1R = lon1[0]*DE2RA;
    lon2R = lon2[0]*DE2RA;

//    F = ( lat1R + lat2R )/2.0;
//    G = ( lat1R - lat2R )/2.0;
//    L = ( lon1R - lon2R )/2.0;

	/*
    printf("%g %g %g %g; %g %g %g\n",  *lon1, *lon2, *lat1, *lat2, F, G, L);
	*/

 //  sinG2 = POWDI( sin( G ), 2 );
 //   cosG2 = POWDI( cos( G ), 2 );
 //   sinF2 = POWDI( sin( F ), 2 );
  //  cosF2 = POWDI( cos( F ), 2 );
 //   sinL2 = POWDI( sin( L ), 2 );
 //   cosL2 = POWDI( cos( L ), 2 );

//    S = sinG2*cosL2 + cosF2*sinL2;
//    C = cosG2*cosL2 + sinF2*sinL2;

//    w = atan( sqrt( S/C ) );
//    R = sqrt( S*C )/w;

//    D = 2*w*a;
//    H1 = ( 3*R - 1 )/( 2*C );
//    H2 = ( 3*R + 2 )/( 2*S );

   //  int N = dist.size();
   // for (j=0; j<N; j++) {
   //    dist[j] = D*( 1 + f*H1*sinF2*cosG2 - f*H2*cosF2*sinG2 );
   // }

    RcppResultSet rs;           // Build result set to be returned as a list to R.
    rs.add("dist", dist);         // vec as named element with name "vec"

    rl = rs.getReturnList();    // Get the list to be returned to R.
  } catch(std::exception& ex) {
    exceptionMesg = copyMessageToR(ex.what());
  } catch(...) {
    exceptionMesg = copyMessageToR("unknown reason");
  }

  if (exceptionMesg != NULL) Rf_error(exceptionMesg);

  return rl;
'

funx <- cfunction(signature(x1="numeric", y1 = "numeric", x2 = "numeric", y2 = "numeric", d = "numeric"), foo, Rcpp=TRUE)
funx(x1 = 1:10, y1 = 10:1, x2 = 1.5:10.5, y2 = 2.3:11.3, d = numeric(9))




dd.inline.rcpp <- function() {
    x <- integer(10000)
    res <- funx(v=x)[[1]]
    tabulate(res)
}

print(mean(replicate(100,system.time(dd.inline.rcpp())["elapsed"]),trim=0.05))

