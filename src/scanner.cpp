//------------------------------------------------------------------------------
/// @brief SnuPL/1 scanner
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/03/07 Bernhard Egger adapted to SnuPL/0
/// 2014/09/10 Bernhard Egger assignment 1: scans SnuPL/-1
/// 2016/03/13 Bernhard Egger assignment 1: adapted to modified SnuPL/-1 syntax
///
/// @section license_section License
/// Copyright (c) 2012-2017, Computer Systems and Platforms Laboratory, SNU
/// All rights reserved.
///
/// Redistribution and use in source and binary forms,  with or without modifi-
/// cation, are permitted provided that the following conditions are met:
///
/// - Redistributions of source code must retain the above copyright notice,
///   this list of conditions and the following disclaimer.
/// - Redistributions in binary form must reproduce the above copyright notice,
///   this list of conditions and the following disclaimer in the documentation
///   and/or other materials provided with the distribution.
///
/// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
/// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,  BUT NOT LIMITED TO,  THE
/// IMPLIED WARRANTIES OF MERCHANTABILITY  AND FITNESS FOR A PARTICULAR PURPOSE
/// ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDER  OR CONTRIBUTORS BE
/// LIABLE FOR ANY DIRECT,  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSE-
/// QUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF  SUBSTITUTE
/// GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
/// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN  CONTRACT, STRICT
/// LIABILITY, OR TORT  (INCLUDING NEGLIGENCE OR OTHERWISE)  ARISING IN ANY WAY
/// OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
/// DAMAGE.
//------------------------------------------------------------------------------

#include <iostream>
#include <sstream>
#include <cctype>
#include <cstdlib>
#include <cstring>
#include <cassert>
#include <cstdio>

#include "scanner.h"
using namespace std;

//------------------------------------------------------------------------------
// token names
//
#define TOKEN_STRLEN 32

char ETokenName[][TOKEN_STRLEN] = {
    "tAssignment",
    "tBaseType",
    "tBegin",
    "tBoolean",
    "tChar",
    "tEnd",
    "tExpression",
    "tFactOp",
    "tFactor",
    "tFormalParam",
    "tFunctionDecl",
    "tIdent",
    "tIfStatement",
    "tModule",
    "tNumber",
    "tProcedureDecl",
    "tQualident",
    "tRelOp",                         ///< relational operator
    "tReturnStatement",
    "tSimpleExpr",
    "tStatSequence",
    "tStatement",
    "tString",
    "tSubroutineBody",
    "tSubroutineCall",
    "tSubroutineDecl",
    "tTerm",
    "tTermOp",
    "tType",
    "tVarDecl",
    "tVarDeclSequence",
    "tVarDeclaration",
    "tWhileStatement",

    "tEOF",                           ///< end of file
    "tIOError",                       ///< I/O error
    "tUndefined"                    ///< undefined
};


//------------------------------------------------------------------------------
// format strings used for printing tokens
//

char ETokenStr[][TOKEN_STRLEN] = {
    "tAssignment",
    "tBaseType (%s)",
    "tBoolean (%s)",
    "tChar (%s)",
    "tExpression",
    "tFactOp (%s)",
    "tFactor (%s)",
    "tFormalParam",
    "tFunctionDecl",
    "tIdent (%s)",
    "tIfStatement (%s)",
    "tModule",
    "tNumber (%s)",
    "tProcedureDecl",
    "tQualident (%s)",
    "tRelOp (%s)",                    ///< relational operator
    "tReturnStatement",
    "tSimpleExpr",
    "tStatSequence",
    "tStatement",
    "tString (%s)",
    "tSubroutineBody",
    "tSubroutineCall",
    "tSubroutineDecl",
    "tTerm",
    "tTermOp (%s)",
    "tType (%s)",
    "tVarDecl",
    "tVarDeclSequence",
    "tVarDeclaration",
    "tWhileStatement",

    "tEOF",                           ///< end of file
    "tIOError",                       ///< I/O error
    "tUndefined (%s)"                 ///< undefined
};


//------------------------------------------------------------------------------
// reserved keywords
//
pair<const char*, EToken> Keywords[] = {
    {"module", tModule },
    {"begin", tSubroutineBody },
    {"end" , tSubroutineBody },
    {"true", tBoolean },    // done
    {"false", tBoolean },   // done
    {"if", tIfStatement},   // done
    {"then", tIfStatement},
    {"else", tIfStatement},
    {"while", tWhileStatement},
    {"do", tWhileStatement},
    {"return", tReturnStatement},
    {"var", tVarDeclaration},
    {"procedure", tProcedureDecl},
    {"function", tFunctionDecl},
    {"boolean", tBaseType}, // done
    {"integer", tBaseType}, // done
    {"char", tBaseType},    // done
};



//------------------------------------------------------------------------------
// CToken
//
CToken::CToken() {
    _type = tUndefined;
    _value = "";
    _line = _char = 0;
}

CToken::CToken(int line, int charpos, EToken type, const string value) {
    _type = type;
    _value = escape(value);
    _line = line;
    _char = charpos;
}

CToken::CToken(const CToken &token) {
    _type = token.GetType();
    _value = token.GetValue();
    _line = token.GetLineNumber();
    _char = token.GetCharPosition();
}

CToken::CToken(const CToken *token) {
    _type = token->GetType();
    _value = token->GetValue();
    _line = token->GetLineNumber();
    _char = token->GetCharPosition();
}

const string CToken::Name(EToken type) {
    return string(ETokenName[type]);
}

const string CToken::GetName(void) const {
    return string(ETokenName[GetType()]);
}

ostream& CToken::print(ostream &out) const {
    int str_len = _value.length();
    str_len = TOKEN_STRLEN + (str_len < 64 ? str_len : 64);
    char *str = (char*)malloc(str_len);
    snprintf(str, str_len, ETokenStr[GetType()], _value.c_str());
    out << dec << _line << ":" << _char << ": " << str;
    free(str);
    return out;
}

string CToken::escape(const string text) {
    const char *t = text.c_str();
    string s;

    while (*t != '\0') {
        switch (*t) {
            case '\n': s += "\\n";  break;
            case '\t': s += "\\t";  break;
            case '\0': s += "\\0";  break;
            case '\'': s += "\\'";  break;
            case '\"': s += "\\\""; break;
            case '\\': s += "\\\\"; break;
            default :  s += *t;
        }
        t++;
    }

    return s;
}

ostream& operator<<(ostream &out, const CToken &t) {
    return t.print(out);
}

ostream& operator<<(ostream &out, const CToken *t) {
    return t->print(out);
}


//------------------------------------------------------------------------------
// CScanner
//
map<string, EToken> CScanner::keywords;

CScanner::CScanner(istream *in) {
    InitKeywords();
    _in = in;
    _delete_in = false;
    _line = _char = 1;
    _token = NULL;
    _good = in->good();
    NextToken();
}

CScanner::CScanner(string in) {
    InitKeywords();
    _in = new istringstream(in);
    _delete_in = true;
    _line = _char = 1;
    _token = NULL;
    _good = true;
    NextToken();
}

CScanner::~CScanner() {
    if (_token != NULL) delete _token;
    if (_delete_in) delete _in;
}

void CScanner::InitKeywords(void) {
    if (keywords.size() == 0) {
        int size = sizeof(Keywords) / sizeof(Keywords[0]);
        for (int i=0; i<size; i++) {
            keywords[Keywords[i].first] = Keywords[i].second;
        }
    }
}

CToken CScanner::Get() {
    CToken result(_token);

    EToken type = _token->GetType();
    _good = !(type == tIOError);

    NextToken();
    return result;
}

CToken CScanner::Peek() const {
    return CToken(_token);
}

void CScanner::NextToken() {
    if (_token != NULL) delete _token;

    _token = Scan();
}

void CScanner::RecordStreamPosition() {
    _saved_line = _line;
    _saved_char = _char;
}

void CScanner::GetRecordedStreamPosition(int *lineno, int *charpos) {
    *lineno = _saved_line;
    *charpos = _saved_char;
}

CToken* CScanner::NewToken(EToken type, const string token) {
    return new CToken(_saved_line, _saved_char, type, token);
}

CToken* CScanner::Scan() {
    EToken token;
    string tokval;
    char c;

    while (_in->good() && IsWhite(_in->peek())) GetChar();

    RecordStreamPosition();

    if (_in->eof()) return NewToken(tEOF);
    if (!_in->good()) return NewToken(tIOError);

    c = GetChar();
    tokval = c;
    token = tUndefined;

    switch (c) {
        case ':':
            if (_in->peek() == '=') {
                tokval += GetChar();
                token = tAssignment;
            }
            break;

        case '|':
            if (_in->peek() == '|') {
                tokval += GetChar();
                token = tTermOp;
            }
            break;

        case '+':
        case '-':
            token = tTermOp;
            break;

        case '&':
            if (_in->peek() == '&') {
                tokval += GetChar();
                token = tFactOp;
            }
            break;

        case '/':
            if (_in->peek() == '/') {
                    IgnoreRestofLine();
                break;
            }

            token = tFactOp;
            break;

        case '*':
            token = tFactOp;
            break;

        case '<':
            if (_in->peek() == '=') {
                tokval += GetChar();
                token = tRelOp;
                break;
            }
            token = tRelOp;
            break;

        case '>':
            if (_in->peek() == '=') {
                tokval += GetChar();
                token = tRelOp;
                break;
            }

            token = tRelOp;
            break;

        case '=':
        case '#':
            token = tRelOp;
            break;

        case 'b':
            if(Peek(6) == "oolean"){
                tokval += GetChar(6);
                token = tBaseType;
            }
            break;

        case 'c':
            if(Peek(3) == "har"){
                tokval += GetChar(3);
                token = tBaseType;
            }
            break;

        case 'f':
            if(Peek(4) == "alse"){
                tokval += GetChar(4);
                token = tBoolean;
            }
            break;

        case 'i':
            if(Peek(6) == "nteger"){
                tokval += GetChar(6);
                token = tBaseType;
            }
            else if(_in->peek() == 'f'){
                tokval += GetChar();
                token = tIfStatement;
            }
            break;

        case 't':
            if(Peek(3) == "rue"){
                tokval += GetChar(3);
                token = tBoolean;
            }
            break;

        case '\'':
            if(_in -> peek() >= 32 && _in -> peek() <= 126 && _in -> peek() != 92) {
                tokval += GetChar();
                tokval += GetChar();
                token = tChar;
            }
            else if(_in -> peek() == '\\'){

                tokval += GetChar();
                tokval += GetChar();
                tokval += GetChar();
                token = tChar;
                // || _in -> peek() == '\n' || _in -> peek() == '\t' || _in -> peek() == '\"' || _in -> peek() == '\'' || _in -> peek() == '\\' || _in -> peek() == '\0')
            }
            break;

        default:
            if (('0' <= c) && (c <= '9')) {
                token = tNumber;

                do {
                    tokval += c;
                    c = _in->peek();
                } while (('0' <= c) && (c <= '9'));

            } else
                if (('a' <= c) && (c <= 'z')) {
                    token = tString;
                } else {
                    tokval = "invalid character '";
                    tokval += c;
                    tokval += "'";
                }
            break;
    }

    return NewToken(token, tokval);
}

char CScanner::GetChar() {
    char c = _in->get();
    if (c == '\n') { _line++; _char = 1; } else _char++;
    return c;
}

void CScanner::IgnoreRestofLine() {
    char c;
    do {
        c = _in->get();
        if (c == '\n') { _line++; _char = 1; } else _char++;
    } while(c != '\n');

    return;
}

string CScanner::GetChar(int n) {
    string str;
    for (int i=0; i<n; i++) str += GetChar();
    return str;
}

string CScanner::StrCmp(string input) {
    string str;

    for (int i=0; i < (int)input.size(); i++) {
        str += _in->peek();
    }
    if (input.compare(str) == 0) {
        return GetChar(input.size());
    }

    return NULL;
}

string CScanner::Peek(int n) {
    string str;
    for(int i = 0; i < n; i++){
        str += _in -> get();
    }

    for(int i = n - 1; i >= 0; i--){
        _in -> putback(str.at(i));
    }
    return str;
}

bool CScanner::IsKeyword(string input){
    if(keywords.find(input) == keywords.end())
        return false;
    return true;
}

bool CScanner::IsComment(char c) const {
    if (c == '/' && _in->peek() == '/') {
        return true;
    }

    return false;
}

bool CScanner::IsWhite(char c) const {
    return ((c == ' ') || (c == '\n') || (c == '\t'));
}
