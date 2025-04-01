#include<Rcpp.h>
#include<string>
#include<vector>

using namespace Rcpp;

std::string ltrim(std::string s) {
  s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](unsigned char ch) {
        return !std::isspace(ch);
        }));

  return s;
}

std::string rtrim(std::string s) {
  s.erase(std::find_if(s.rbegin(), s.rend(), [](unsigned char ch) {
        return !std::isspace(ch);
        }).base(), s.end());

  return s;
}

// [[Rcpp::export]]
std::string trim(std::string s) {
  return rtrim(ltrim(s));
}

const int STATE_END = -1;

enum Input {
  IN_TERMINATOR,
  IN_BACKSLASH,
  IN_QUOTEMARK,
  IN_SLASH,
  IN_ASTERIX,
  IN_COMMENT_ASTERIX,
  IN_COMMENT_START,
  IN_COMMENT,
  IN_COMMENT_END,
  IN_OTHER
};

int checkInput(char c, int state, int comment_count) {

  Input in = IN_OTHER;
  if (c == ';' || c == '$') {
    in = IN_TERMINATOR;
  } else if (c == '\\') {
    in = IN_BACKSLASH;
  } else if (c == '"') {
    in = IN_QUOTEMARK;
  } else if (c == '*') {
    in = IN_ASTERIX;
  } else if (c == '/') {
    in = IN_SLASH;
  }

  switch (state) {
    case IN_TERMINATOR: // IN_TERMINATOR
      switch (in) {
        case IN_BACKSLASH:
          return IN_BACKSLASH;
        case IN_TERMINATOR:
          return STATE_END;
        case IN_QUOTEMARK:
          return IN_QUOTEMARK;
        case IN_SLASH:
          return IN_SLASH;
        default:
          return IN_TERMINATOR;
      }

    case IN_BACKSLASH:
      return IN_TERMINATOR;
    case IN_QUOTEMARK:
      switch (in) {
        case IN_BACKSLASH:
          return IN_OTHER;
        case IN_QUOTEMARK:
          return IN_TERMINATOR;
        default:
          return IN_QUOTEMARK;
      }

    case IN_SLASH:
      switch (in) {
        case IN_ASTERIX:
          return IN_COMMENT_START;
        case IN_TERMINATOR:
          return STATE_END;
        default:
          return IN_TERMINATOR;
      }

    case IN_COMMENT:
      switch (in) {
        case IN_SLASH:
          return IN_SLASH;
        case IN_ASTERIX:
          return IN_COMMENT_ASTERIX;
        default: 
          return IN_COMMENT;
      }

    case IN_COMMENT_START:
      switch (in) {
        case IN_ASTERIX:
          return IN_COMMENT_ASTERIX;
        default:
          return IN_COMMENT;
      }

    case IN_COMMENT_ASTERIX:
      switch (in) {
        case IN_SLASH:
          return IN_COMMENT_END;
        default:
          return IN_COMMENT;
      }

    case IN_COMMENT_END:
      if (comment_count == 0) {
        switch (in) {
          case IN_TERMINATOR:
            return STATE_END;
          case IN_BACKSLASH:
            return IN_BACKSLASH;
          case IN_QUOTEMARK:
            return IN_QUOTEMARK;
          case IN_SLASH:
            return IN_SLASH;
          default:
            return IN_TERMINATOR;
        } 
      } else {
        switch (in) {
          case IN_SLASH:
            return IN_SLASH;
          case IN_ASTERIX:
            return IN_COMMENT_ASTERIX;
          default:
            return IN_COMMENT;
        }
      }

    case IN_OTHER:
      return IN_QUOTEMARK;

    case STATE_END:
      switch (in) {
        case IN_BACKSLASH:
          return IN_BACKSLASH;
        case IN_TERMINATOR:
          return STATE_END;
        case IN_QUOTEMARK:
          return IN_QUOTEMARK;
        case IN_SLASH:
          return IN_SLASH;
        default:
          return IN_TERMINATOR;
      }
  }
  return state;
}

// [[Rcpp::export]]
std::string checkCommand(std::string command) { 
  trim(command);
  int state = 0; 
  int comment_count = 0;
  bool terminated = false;
  std::vector<size_t> csp, cep;

  for (size_t i = 0; i < command.size(); ++i) {
    state = checkInput(command[i], state, comment_count);

    if (state == IN_COMMENT_START) {
      if (comment_count == 0)
        csp.push_back(i-1);

      comment_count++;
    }
    else if (state == IN_COMMENT_END) {
      comment_count--;
      if(comment_count == 0)
        cep.push_back(i);
    }

    if (state == STATE_END && terminated == true) {
      Rcpp::stop("Bad expression: only one ;|$ terminated expression at a time is allowed");
    } 

    if (state == STATE_END) {
      terminated = true;
    }
  }

  if(csp.size() != cep.size()) {
    stop("Bad expression: Found Non-ending comment");
  }

  for (int i = csp.size() - 1;  i >= 0; --i) {
    command.erase(csp.at(i), cep.at(i) - csp.at(i) + 1);
  }

  if (!terminated) {
    command += ";";
  }

  return command;
}

// [[Rcpp::export]]
Rcpp::List dissect_chunk(std::vector<std::string> code) {
  int state = 0; 
  int comment_count = 0;
  bool terminated = false;
  std::vector<size_t> csp, cep;
  List L = List::create();
  IntegerVector temp = IntegerVector::create();

  for (size_t i = 0; i < code.size(); ++i) {
    code[i] = trim(code[i]);
    // code(i) = trim(code[i]);
    for (size_t j = 0; j < code[i].size(); ++j) {
      state = checkInput(code[i][j], state, comment_count);

      if (state == IN_COMMENT_START) {
        if (comment_count == 0)
          csp.push_back(i-1);

        comment_count++;
      }
      else if (state == IN_COMMENT_END) {
        comment_count--;
        if(comment_count == 0)
          cep.push_back(i);
      }

      if(state == STATE_END) {
        terminated = true;
      }
    }
    
    // skip empty lines
    if(code[i].size() > 0) {
      temp.push_back(i+1);
      if(terminated == true) {
        terminated = false;
        L.push_back(temp);
        temp.erase(0, temp.size());
      }
    }

  }

  return L;
}


// [[Rcpp::export]]
Rcpp::List dissect_repl_input(std::string input) {
  int state = 0; 
  int comment_count = 0;
  size_t term_pos = 0;
  std::string in = trim(input);
  List out = List::create();

  for (size_t i = 0; i < in.size(); ++i) {
    state = checkInput(in[i], state, comment_count);

    if (state == IN_COMMENT_START) {
      comment_count++; 
    } else if (state == IN_COMMENT_END) {
      comment_count--;
    }

    // if(state == STATE_END) {
    //   terminated = true;
    // }

    // skip empty lines
    //if(terminated == true) {
    if(state == STATE_END) {
      // copy command to list
      // terminated = false;
      out.push_back(in.substr(term_pos, (i - term_pos) + 1));
      term_pos = i + 1;
      // temp.erase(0, temp.size());
    }
  }
  return out;
}
