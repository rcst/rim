#ifndef MAXIMACHAIN_H_INCLUDED
#define MAXIMACHAIN_H_INCLUDED

#include<cstddef>
#include<string>
#include<deque>

// #include<boost/process.hpp>
#include<regex>
#include<boost/scoped_ptr.hpp>
#include<boost/shared_ptr.hpp>

#include<boost/process/child.hpp>
#include<boost/process/pipe.hpp>
#include<boost/process/io.hpp>

#include<boost/algorithm/string.hpp>

namespace bp = ::boost::process;
namespace fs = ::boost::filesystem;
using std::size_t;

namespace Maxima
{

class MaximaChain
{
    public:

        MaximaChain(const std::string &maximaPath,          	// Full path to the Maxima executable
                    const std::string &workingDir = ".",    	// Working directory for output files
                    const std::string &utilsDir = ".",		// Utils directory that contains display.lisp
		    const std::string &display = "");     

        ~MaximaChain();

	std::string executeCommand(const std::string &command); 

	std::string executeCommandList(const std::string &command); 

	void loadModule(const std::string &module);

        // Command must end with ';' or '$'. In annother case we append ';'
        void sendCommand(std::string command);              

	// bp::process::id_type getId() const;
	bp::pid_t getId() const;

	size_t getLastPromptId() const { return lastPromptId; }
        
        const fs::path &getWorkingDirectory() const;

        class MaximaIOHook
        {
          public:
		  //virtual = runtime polymorphism, i.e. base-class pointer calls deriver-class function
            virtual ~MaximaIOHook();
     
	    // "=0" - this function has to be implemented by derived class, otherwise no objects may be
	    // instantiated by the derived class
            virtual std::string handle(const std::string &first,
                                       const std::string &second) = 0;
        };

        void setMaximaIOHook(const std::string &hookRegex, MaximaIOHook *hook);

 
   private:
	// struct declares a compound data type combining other data types into one. Access element via "."
        struct Reply
        {
		// std::deque (double-ended queue) acts as a container for sequences. 
		// Efficient for removing elements at the front or back. 
		// However modification at other positions is also possible
            typedef std::deque<char> RawReply;

	    // used as type for boost::match_results
            typedef RawReply::iterator It;

	    // std::pair are two objects of same type, i.e. to iterators for deque<char>
            typedef std::pair<It, It> Range;

            RawReply reply;

            Range prompt;

	    // deque of ranges of type deque<char>
            std::deque<Range> outs;

            std::deque<Range> betweens;

	    // once any constructor is defined a default constructor that takes
	    // no arguments has to be defined
	    Reply();

            // Reply(std::istream &in);
            Reply(bp::ipstream &in);

            std::string concatenateParts();

            // True if prompt is valid (%i\d+)
            bool CheckPrompt() const { return validPrompt; } 

            size_t getPromptId() const { return promptId; }

            bool isInterrupted() const;

            bool requireUser(std::match_results<Reply::It> &match) const; 
	    
	    bool checkMaximaError(std::match_results<Reply::It> &match) const;

        private:
            Reply(const Reply &);

            void operator=(const Reply &);

            bool validPrompt;

            size_t promptId;
        };

        typedef boost::shared_ptr<MaximaChain::Reply> ReplyPtr;

        // Returns the number of bytes read
        static size_t readData(std::istream &in, Reply::RawReply &reply);

        // Sends command and get reply in Maxima style
        ReplyPtr crudeExecute(const std::string &command); 

        // Check if the input expression valid
        int checkInput(char nextChar, int checkerState) const;

        ReplyPtr readReply();

	void getPid();

        boost::scoped_ptr<bp::child> process;

	bp::ipstream is; // pipe reading stream
	bp::opstream os; // pipe writing stream

        MaximaIOHook* maximaIOHook;

	std::string maximaIOHookRegexStr;
	std::regex maximaIOHookRegex;

        fs::path workingDirectory;

        fs::path utilsDirectory;

        size_t lastPromptId;

	// size_t pid;
	pid_t pid;
};

} // namespace Maxima


#endif // MAXIMACHAIN_H_INCLUDED
