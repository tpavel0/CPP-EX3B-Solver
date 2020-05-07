#pragma once
#include <stdlib.h>
#include <iostream>
#include <string.h>
#include <complex>

using namespace std;

namespace solver{
	
	class RealVariable{
		public:
			double a, b, c;
			RealVariable(): a(0), b(1), c(0){};
			RealVariable(double a, double b, double c): a(a), b(b), c(c){};
			//             =
			friend RealVariable operator== (const RealVariable& x , const RealVariable& y);
			friend RealVariable operator== (const RealVariable& x , const double num);
			friend RealVariable operator== ( const double num , const RealVariable& x);
			//             +
			friend RealVariable operator+ (const RealVariable& x , const RealVariable& y);
			friend RealVariable operator+ (const RealVariable& x , const double num);
			friend RealVariable operator+ ( const double num , const RealVariable& x);
			//             -
			friend RealVariable operator- (const RealVariable& x , const RealVariable& y);
			friend RealVariable operator- (const RealVariable& x , const double num);
			friend RealVariable operator- ( const double num , const RealVariable& x);
			//             /
			friend RealVariable operator/ (const RealVariable& x , const RealVariable& y);
			friend RealVariable operator/ (const RealVariable& x , const double num);
			friend RealVariable operator/ ( const double num , const RealVariable& x);
			//             *
			friend RealVariable operator* (const double num , const RealVariable& x);
			friend RealVariable operator* (const RealVariable& x , const double num);
			friend RealVariable operator^ (const RealVariable& x , int num);
	};
	double solve (RealVariable x);

	class ComplexVariable{
		public:
			complex<double> a, b, c;

			ComplexVariable(): a(0), b(1), c(0){};
			ComplexVariable(complex<double> a, complex<double> b, complex<double> c): a(a), b(b), c(c){};
			//              =
			friend ComplexVariable operator== (const ComplexVariable &x ,const  ComplexVariable &y);
			friend ComplexVariable operator== (const ComplexVariable &x , std::complex<double> num);
			friend ComplexVariable operator== ( std::complex<double> num ,const ComplexVariable &x);
			friend ComplexVariable operator== (const ComplexVariable &x , double num);
			friend ComplexVariable operator== ( double num ,const ComplexVariable &x);
	        //             +
			friend ComplexVariable operator+ (const ComplexVariable &x , const ComplexVariable &y);
			friend ComplexVariable operator+ (const ComplexVariable &x , std::complex<double> num);
			friend ComplexVariable operator+ ( std::complex<double> num ,const ComplexVariable &x);
			friend ComplexVariable operator+ (const ComplexVariable &x , double num);
			friend ComplexVariable operator+ ( double num ,const ComplexVariable &x);
            //             -
			friend ComplexVariable operator- (const ComplexVariable &x , const ComplexVariable &y);
			friend ComplexVariable operator- (const ComplexVariable &x , std::complex<double> num);
			friend ComplexVariable operator- ( std::complex<double> num ,const ComplexVariable &x);
			friend ComplexVariable operator- (const ComplexVariable &x , double num);
			friend ComplexVariable operator- ( double num ,const ComplexVariable &x);
            //             *
			friend ComplexVariable operator* (const ComplexVariable &x , std::complex<double> num);
			friend ComplexVariable operator* ( std::complex<double> num ,const ComplexVariable &x);
			friend ComplexVariable operator* (const ComplexVariable &x , double num);
			friend ComplexVariable operator* ( double num ,const ComplexVariable &x);
            //             /
			friend ComplexVariable operator/ (const ComplexVariable &x , const ComplexVariable &y);
			friend ComplexVariable operator/ (const ComplexVariable &x , std::complex<double> num);
			friend ComplexVariable operator/ ( std::complex<double> num ,const ComplexVariable &x);
			friend ComplexVariable operator/ (const ComplexVariable &x , double num);
			friend ComplexVariable operator/ ( double num ,const ComplexVariable &x);
            //             sqr
			friend ComplexVariable operator^ (const ComplexVariable &x , int num);
	};
	complex<double> solve (ComplexVariable x);	
};
