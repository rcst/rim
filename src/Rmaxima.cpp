#include <Rcpp.h>
#include "MaximaChain.h"
#include "boost/process.hpp"


using namespace Rcpp;


namespace bp = ::boost::process;
namespace fs = ::boost::filesystem;

class RMaxima 
{
  public:
    RMaxima()
    {
        Rcpp::Rcout << "Inside Constructor" << std::endl;
	running = false;

	Function f("system.file");
	Function dirname("dirname");
	Function normalizepath("normalizePath");
	Function SysWhich("Sys.which");
	Function getwd("getwd");

	maxpath = Rcpp::as<std::string>(SysWhich("maxima"));
        // maxpath = bp::search_path("maxima").string();

	workDir = Rcpp::as<std::string>(getwd());
        // workDir = fs::current_path().string();

	utilsDir = Rcpp::as<std::string>(normalizepath(dirname(f("extdata", "maxima-init.mac", Named("package") = "rmaxima", Named("mustWork") = true))));
	// fs::path p(Rcpp::as<std::string>(f("extdata", "maxima-init.mac", Named("package") = "rmaxima", Named("mustWork") = true)));
        // utilsDir = p.parent_path().string();

	startMaxima();

	Rcpp::Rcout << "myMaxima pointer: " << myMaxima << std::endl;
    }

    ~RMaxima()
    {
	    Rcpp::Rcout << "inside destructor" << std::endl;

            stopMaxima();
    }

    std::string execute(std::string command)
    {
	    if(!running) 
	    {
		    Rcpp::Rcout << "Starting maxima ..." << std::endl;
		    startMaxima();
	    }
	    
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
		    std::string result = execute("load(" + module + ");"); 
		    return result;
	    }
    }

    std::string apropos(std::string keystring)
    {
	    std::string result = execute("apropos(\"" + keystring + "\");");
	    return result;
    }

    void startMaxima(bool restart = false)
    {
	    if(running) 
	    { 
		    if(restart) 
		    { 
			    stopMaxima(); 
			    myMaxima = new Maxima::MaximaChain(maxpath, workDir, utilsDir); 
		    }
		    else 
			    Rcout << "Maxima is already running." << std::endl;
	    } else
	    {
		    myMaxima = new Maxima::MaximaChain(maxpath, workDir, utilsDir);
		    running = true;
	    }
    }

    void stopMaxima()
    { 
        Rcout << "inside stopMaxima() "; 
        
        if(running)
        {
                Rcout << "deleting myMaxima at " << myMaxima << std::endl; 

                delete myMaxima; 
                myMaxima = nullptr;
	        running = false;
	} 
        else Rcout << "Maxima is not running." << std::endl;
    }

  private:
    Maxima::MaximaChain* myMaxima;
    std::string maxpath;
    std::string workDir;
    std::string utilsDir;

    bool running;
};

// static void rmaxima_finalizer(RMaxima* ptr)
// {
//     Rcpp::Rcout << "inside finalizer" << std::endl; 
//     Rcpp::Rcout << "RMaxima pointer: " << ptr << std::endl;
// 
//     if(ptr)
//     { 
//         Rcpp::Rcout << "deleting RMaxima pointer" << std::endl;
//         delete ptr;
//     }
// }

RCPP_MODULE(Maxima)
{
    class_<RMaxima>("RMaxima")
    .constructor()
    .method("startMaxima", &RMaxima::startMaxima)
    .method("stopMaxima", &RMaxima::stopMaxima)
    .method("execute", &RMaxima::execute)
    .method("loadModule", &RMaxima::loadModule)
    .method("apropos", &RMaxima::apropos)
    //.finalizer(&rmaxima_finalizer)
    ;
} 

/*** R

# avoid forcing user to create object from class RMaxima
# fixing interface object to reference class named "maxima"

*/
