#include <Rcpp.h>
#include "MaximaChain.h"
#include "boost/process.hpp"
#include "boost/filesystem.hpp"


using namespace Rcpp;


namespace bp = ::boost::process;
namespace fs = ::boost::filesystem;

class RMaxima 
{
  public:
    RMaxima()
    {
        std::string maxpath = bp::search_path("maxima").string();
        std::string workDir = fs::current_path().string();

	// Environment env("package:base");
	// Function f = env["system.file"];
	Function f("system.file");
	fs::path p(Rcpp::as<std::string>(f("extdata", "maxima-init.mac", Named("package") = "rmaxima", Named("mustWork") = true)));
        std::string utilsDir = p.parent_path().string();

	Rcout << "Init script directory: " << utilsDir << std::endl;

        myMaxima = new Maxima::MaximaChain(maxpath, workDir, utilsDir);
    }

    ~RMaxima()
    {
	    Rcout << "Destructor" << std::endl; 
	    delete myMaxima;
    }

    std::string execute(std::string command)
    {
            std::string result = myMaxima->executeCommand(command);
            return result;
    }

    std::string loadModule(std::string module)
    {
	    if(module.empty())
	    {
		    Rcpp::stop("Please provide a valid module name!");
	    }
	    else
	    { 
		    std::string result = myMaxima->executeCommand("load(" + module + ");"); 
		    return result;
	    }
    }

  private:
    Maxima::MaximaChain* myMaxima;

};

static void rmaxima_finalizer(RMaxima* ptr)
{
    if (ptr)
    { 
        delete ptr;
    }
}

RCPP_MODULE(Maxima)
{
    class_<RMaxima>("RMaxima")
    .constructor()
    .method("execute", &RMaxima::execute)
    .method("loadModule", &RMaxima::loadModule)
    .finalizer(&rmaxima_finalizer)
    ;
} 

/*** R

# avoid forcing user to create object from class RMaxima
# fixing interface object to reference class named "maxima"

*/
