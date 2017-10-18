//------------------------------------------------------------------------------
/// @brief SnuPL/0 parser
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/03/07 Bernhard Egger adapted to SnuPL/0
/// 2014/11/04 Bernhard Egger maintain unary '+' signs in the AST
/// 2016/04/01 Bernhard Egger adapted to SnuPL/1 (this is not a joke)
/// 2016/09/28 Bernhard Egger assignment 2: parser for SnuPL/-1
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

#include <limits.h>
#include <cassert>
#include <errno.h>
#include <cstdlib>
#include <vector>
#include <iostream>
#include <exception>

#include "parser.h"
using namespace std;


//------------------------------------------------------------------------------
// CParser
//
CParser::CParser(CScanner *scanner)
{
    _scanner = scanner;
    _module = NULL;
}

CAstNode* CParser::Parse(void)
{
    _abort = false;

    if (_module != NULL) { delete _module; _module = NULL; }

    try {
        if (_scanner != NULL) _module = module();

        if (_module != NULL) {
            CToken t;
            string msg;
            //if (!_module->TypeCheck(&t, &msg)) SetError(t, msg);
        }
    } catch (...) {
        _module = NULL;
    }

    return _module;
}

const CToken* CParser::GetErrorToken(void) const
{
    if (_abort) return &_error_token;
    else return NULL;
}

string CParser::GetErrorMessage(void) const
{
    if (_abort) return _message;
    else return "";
}

void CParser::SetError(CToken t, const string message)
{
    _error_token = t;
    _message = message;
    _abort = true;
    throw message;
}

bool CParser::Consume(EToken type, CToken *token)
{
    if (_abort) return false;

    CToken t = _scanner->Get();

    if (t.GetType() != type) {
        SetError(t, "expected '" + CToken::Name(type) + "', got '" +
                t.GetName() + "'");
    }

    if (token != NULL) *token = t;

    return t.GetType() == type;
}

void CParser::InitSymbolTable(CSymtab *s)
{
    CTypeManager *tm = CTypeManager::Get();

    // TODO: add predefined functions here
    CSymProc *dim = new CSymProc("DIM", tm -> GetInt());
    dim -> AddParam(new CSymParam(0, "array", tm -> GetVoidPtr()));
    dim -> AddParam(new CSymParam(1, "dim", tm -> GetInt()));
    s -> AddSymbol(dim);

    CSymProc *dofs = new CSymProc("DOFS", tm -> GetInt());
    dofs -> AddParam(new CSymParam(0, "array", tm -> GetVoidPtr()));
    s -> AddSymbol(dofs);

    // function ReadInt(): integerread and return an integer value from stdin.
    // –procedure WriteInt(i: integer);print integer value ‘i’ to stdout.
    // –procedure WriteChar(c: char);write a single character to stdout.
    // –procedure WriteStr(string: char[]);write string ‘string’ to stdout. No newline is added.
    // –procedure WriteLn()write a newline sequence to stdout.
    CSymProc *read_int = new CSymProc("ReadInt", tm -> GetInt());
    s -> AddSymbol(read_int);

    CSymProc *write_int = new CSymProc("WriteInt", tm -> GetNull());
    write_int -> AddParam(new CSymParam(0, "i", tm -> GetInt()));
    s -> AddSymbol(write_int);

    CSymProc *write_char = new CSymProc("WriteChar", tm -> GetNull());
    write_char -> AddParam(new CSymParam(0, "c", tm -> GetChar()));
    s -> AddSymbol(write_char);

    CSymProc *write_str = new CSymProc("WriteStr", tm -> GetNull());
    write_str -> AddParam(new CSymParam(0, "string", tm -> GetPointer((tm -> GetArray(-1 , tm -> GetChar())))));
    s -> AddSymbol(write_str);
    
    CSymProc *write_ln = new CSymProc("WriteLn", tm -> GetNull());
    s -> AddSymbol(write_ln);

    // ‘main’ is used to denote the module body in the generated assembly file
    CSymbol *main_keyword = new CSymbol("main", stReserved, tm -> GetNull());
    s -> AddSymbol(main_keyword);
}


void CParser::variable_declaration(void){
    // varDeclaration = [ "var" varDeclSequence ";" ].
    // varDeclSequence = varDecl { ";" varDecl }.
    // varDecl = ident { "," ident } ":" type.
    vector<CToken> variables;
    EToken peek_type = _scanner->Peek().GetType();
    while(peek_type != tBegin && peek_type != tProcedure && peek_type != tFunction){
        // at least one var
        CToken tmp;
        Consume(tIdent, &tmp);
        variables.push_back(tmp);
        cout << tmp << endl;
        peek_type = _scanner->Peek().GetType();
        cout << "peek " << peek_type << endl;
        while(peek_type != tColon){
            Consume(tComma);
            Consume(tIdent, &tmp);
            variables.push_back(tmp);
            peek_type = _scanner->Peek().GetType();
        }
        Consume(tColon);
        // type
        // todo: need to deal with type here
        while(variables.size() != 0){
            cout << variables.front() << endl;
            variables.erase(variables.begin());
        }
    }
}

CAstModule* CParser::module(void)
{
    //
    // module ::= statSequence  ".".
    //
    // module ::= "module" ident ";" varDeclaration { subroutineDecl } "begin" statSequence "end" ident ".".

    CToken module_name;
    Consume(tModule);
    Consume(tIdent, &module_name);
    Consume(tSemicolon);

    CToken dummy;
    CAstModule *m = new CAstModule(dummy, "placeholder");
    InitSymbolTable(m -> GetSymbolTable());

    // variable declaration
    if(_scanner->Peek().GetType() == tVarDecl){
        Consume(tVarDecl);
        variable_declaration();
    }
    // subroutine declaration
    
    Consume(tBegin);
    // CAstStatement *statseq = NULL;
    // statseq = statSequence(m);

    CToken check_module_name;
    Consume(tEnd);
    Consume(tIdent, &check_module_name);
    Consume(tDot);

    // m->SetStatementSequence(statseq);

    return m;
}

CAstStatReturn* CParser::returnStatement(CAstScope *s) {
    CToken t;

    Consume(tReturn, &t);

    CAstExpression *rhs = expression(s);

    return new CAstStatReturn(t, s, rhs);
}

CAstStatement* CParser::statSequence(CAstScope *s) {
    // statSequence ::= [ statement { ";" statement } ].
    // statement ::= assignment | subroutineCall | ifStatement |
    // whileStatement | returnStatement.
    // FIRST(statSequence) = { tIdent, tIf, tWhile, tReturn }
    // FOLLOW(statSequence) = { tElse, tEnd }
    //
    CAstStatement *head = NULL;

    EToken tt = _scanner->Peek().GetType();
    if (!(tt == tElse) && !(tt == tEnd)) {
        CAstStatement *tail = NULL;

        do {
            CToken t;
            EToken tt = _scanner->Peek().GetType();
            CAstStatement *st = NULL;

            switch (tt) {
                // statement ::= assignment
                case tIf:
                case tWhile:
                    st = whileStatement(s);
                case tReturn:
                case tIdent:
                    st = assignment(s);
                    break;
                default:
                    SetError(_scanner->Peek(), "statement expected.");
                    break;
            }

            assert(st != NULL);
            if (head == NULL) head = st;
            else tail->SetNext(st);
            tail = st;

            tt = _scanner->Peek().GetType();
            if (tt == tElse) {
                break;
            } else if (tt == tEnd) {
                break;
            }

            Consume(tSemicolon);
        } while (!_abort);
    }

    return head;
}

CAstStatAssign* CParser::assignment(CAstScope *s)
{
    //
    // assignment ::= number ":=" expression.
    //
    CToken t;

    CAstConstant *lhs = number();
    Consume(tAssign, &t);

    CAstExpression *rhs = expression(s);

    return new CAstStatAssign(t, lhs, rhs);
}

CAstExpression* CParser::expression(CAstScope* s)
{
    //
    // expression ::= simpleexpr [ relOp simpleexpr ].
    //
    CToken t;
    EOperation relop;
    CAstExpression *left = NULL, *right = NULL;

    left = simpleexpr(s);

    if (_scanner->Peek().GetType() == tRelOp) {
        Consume(tRelOp, &t);
        right = simpleexpr(s);

        if (t.GetValue() == "=")       relop = opEqual;
        else if (t.GetValue() == "#")  relop = opNotEqual;
        else SetError(t, "invalid relation.");

        return new CAstBinaryOp(t, relop, left, right);
    } else {
        return left;
    }
}

CAstExpression* CParser::simpleexpr(CAstScope *s)
{
    //
    // simpleexpr ::= term { termOp term }.
    //
    CAstExpression *n = NULL;

    n = term(s);

    while (_scanner->Peek().GetType() == tPlusMinus) {
        CToken t;
        CAstExpression *l = n, *r;

        Consume(tPlusMinus, &t);

        r = term(s);

        n = new CAstBinaryOp(t, t.GetValue() == "+" ? opAdd : opSub, l, r);
    }


    return n;
}

CAstExpression* CParser::term(CAstScope *s)
{
    //
    // term ::= factor { ("*"|"/") factor }.
    //
    CAstExpression *n = NULL;

    n = factor(s);

    EToken tt = _scanner->Peek().GetType();

    while ((tt == tMulDiv)) {
        CToken t;
        CAstExpression *l = n, *r;

        Consume(tMulDiv, &t);

        r = factor(s);

        n = new CAstBinaryOp(t, t.GetValue() == "*" ? opMul : opDiv, l, r);

        tt = _scanner->Peek().GetType();
    }

    return n;
}

CAstExpression* CParser::factor(CAstScope *s)
{
    //
    // factor ::= number | "(" expression ")"
    //
    // FIRST(factor) = { tNumber, tLBrak, tBoolConst, tCharConst, tString, tIdent, tNot }
    //

    CToken t;
    EToken tt = _scanner->Peek().GetType();
    CAstExpression *unary = NULL, *n = NULL;

    switch (tt) {
        // factor ::= number
        case tNumber:
            n = number();
            break;
            // factor ::= "(" expression ")"
        case tLParens:
            Consume(tLParens);
            n = expression(s);
            Consume(tRParens);
            break;
        case tBoolConst:
            break;
        case tCharConst:
            break;
        case tString:
            break;
        case tIdent:
            break;
        case tNot:
            break;

        default:
            cout << "got " << _scanner->Peek() << endl;
            SetError(_scanner->Peek(), "factor expected.");
            break;
    }

    return n;
}

CAstConstant* CParser::number(void)
{
    //
    // number ::= digit { digit }.
    //
    // "digit { digit }" is scanned as one token (tNumber)
    //

    CToken t;

    Consume(tNumber, &t);

    errno = 0;
    long long v = strtoll(t.GetValue().c_str(), NULL, 10);
    if (errno != 0) SetError(t, "invalid number.");

    return new CAstConstant(t, CTypeManager::Get()->GetInt(), v);
}

CAstStatWhile* CParser::whileStatement(CAstScope *s) {
    //
    // whileStatement ::= "while" "(" expression ")" "do" statSequence "end"
    //
    CToken t;
    CAstExpression *cond = NULL;
    CAstStatement *body = NULL;

    Consume(tWhile, &t);
    Consume(tLParens);
    cond = expression(s);
    Consume(tRParens);
    Consume(tDo);
    body = statSequence(s);
    Consume(tEnd);

    return new CAstStatWhile(t, cond, body);
}
