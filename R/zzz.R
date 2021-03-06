
.onAttach <- function(libname, pkgname) {
	if (dfddm(rt = 0.001, response = "lower", 
	          a = 5, v = -5, t0 = 1e-4, w = 0.8, sv = 1.5, 
	          log = FALSE, n_terms_small = "Navarro", scale = "large", 
	          err_tol = 1e-6) > 1e-6) {
	  packageStartupMessage("************\n", 
	  "fddm WARNING: Current system shows numerical problems for very small effective\n", 
	  "response times using the Navarro large time method. This issue can be avoided by \n", 
	  "updating the C++ compiler or using 64 bit versions of R. Alternatively, avoding\n", 
	  "the large time method may also be an option.\n", 
	  "************")  
	}
}

