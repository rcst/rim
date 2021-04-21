#include <Rcpp.h>
#include "MaximaChain.h"
// #include "boost/process.hpp"

using namespace Rcpp;

namespace bp = ::boost::process;
namespace fs = ::boost::filesystem;

class RMaxima 
{
	public:
		RMaxima()
		{
			running = false;

			Function a("normalizePath");
			Function b("dirname");
			Function c("system.file");
			Function SysWhich("Sys.which");
			Function getwd("getwd");

			maxpath = Rcpp::as<std::string>(SysWhich("maxima"));
			// maxpath = bp::search_path("maxima").string();

			workDir = Rcpp::as<std::string>(getwd());
			// workDir = fs::current_path().string();

			utilsDir = Rcpp::as<std::string>(a(b(c("extdata", "maxima-init-lin.mac", Named("package") = "rmaxima", Named("mustWork") = true))));
			// fs::path p(Rcpp::as<std::string>(f("extdata", "maxima-init.mac", Named("package") = "rmaxima", Named("mustWork") = true)));
			// utilsDir = p.parent_path().string();

			startMaxima();
		}

		~RMaxima()
		{
			delete myMaxima;
		}

		Rcpp::CharacterVector execute(std::string command, bool label = false)
		{
			Rcpp::CharacterVector result;
			std::stringstream ss;

			if(!running) 
			{
				startMaxima();
			}

			size_t i = myMaxima->getLastPromptId();

			if(label)
			{
				ss << "(%o" << i << ") " << myMaxima->executeCommand(command);
				result = ss.str();
				ss.str("");
			}
			else  
				result = myMaxima->executeCommand(command);

			ss << "%i" << i;
			result.attr("input.label") = ss.str();
			ss.str("");

			ss << "%o" << i;
			result.attr("output.label") = ss.str();
			ss.str("");

			result.attr("command") = command;

			return result;
		}

		Rcpp::CharacterVector loadModule(std::string module)
		{
			if(module.empty())
			{
				Rcpp::stop("Please provide a valid module name!");
			}
			else
			{ 
				return execute("load(" + module + ");"); 
			}
		}

		Rcpp::CharacterVector apropos(std::string keystring)
		{
			return execute("apropos(\"" + keystring + "\");");
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
			if(running)
			{
				delete myMaxima; 
				myMaxima = nullptr;
				running = false;
			} 
		}

		void setTexEnv(std::string before, std::string after)
		{
			if(!before.empty() && !after.empty()) 
				execute("set_tex_environment_default(\"" + 
						before + 
						"\", \"" + 
						after + 
						"\")$");
		}

		void loadInit(std::string file)
		{
			if(!file.empty())
			{
				loadModule("\"" + utilsDir + "/" + file + "\"");
				execute(std::string("%th(2)"));
			}
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
		.method("setTexEnv", &RMaxima::setTexEnv)
		.method("loadInit", &RMaxima::loadInit)
		//.finalizer(&rmaxima_finalizer)
		;
} 

/*** R

# avoid forcing user to create object from class RMaxima
# fixing interface object to reference class named "maxima"

*/
