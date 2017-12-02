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

#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <errno.h>
#include <exception>
#include <iostream>
#include <limits.h>
#include <vector>
#include "parser.h"
using namespace std;

//------------------------------------------------------------------------------
// CParser
//
CParser::CParser(CScanner *scanner) {
  _scanner = scanner;
  _module = NULL;
}

CAstNode *CParser::Parse(void) {
  _abort = false;

  if (_module != NULL) {
    delete _module;
    _module = NULL;
  }

  try {
    if (_scanner != NULL)
      _module = module();

    if (_module != NULL) {
      CToken t;
      string msg;
      if (!_module->TypeCheck(&t, &msg))
        SetError(t, msg);
    }
  } catch (...) {
    _module = NULL;
  }

  return _module;
}

const CToken *CParser::GetErrorToken(void) const {
  if (_abort)
    return &_error_token;
  else
    return NULL;
}

string CParser::GetErrorMessage(void) const {
  if (_abort)
    return _message;
  else
    return "";
}

void CParser::SetError(CToken t, const string message) {
  _error_token = t;
  _message = message;
  _abort = true;
  throw message;
}

bool CParser::Consume(EToken type, CToken *token) {
  if (_abort)
    return false;

  CToken t = _scanner->Get();

  if (t.GetType() != type) {
    SetError(t, "expected '" + CToken::Name(type) + "', got '" + t.GetName() +
                    "'");
  }

  if (token != NULL)
    *token = t;

  return t.GetType() == type;
}

void CParser::InitSymbolTable(CSymtab *s) {
  CTypeManager *tm = CTypeManager::Get();

  CSymProc *dim = new CSymProc("DIM", tm->GetInt());
  dim->AddParam(new CSymParam(0, "array", tm->GetVoidPtr()));
  dim->AddParam(new CSymParam(1, "dim", tm->GetInt()));
  s->AddSymbol(dim);

  CSymProc *dofs = new CSymProc("DOFS", tm->GetInt());
  dofs->AddParam(new CSymParam(0, "array", tm->GetVoidPtr()));
  s->AddSymbol(dofs);

  // function ReadInt(): integer;read and return an integer value from stdin.
  // –procedure WriteInt(i: integer);print integer value ‘i’ to stdout.
  // –procedure WriteChar(c: char);write a single character to stdout.
  // –procedure WriteStr(string: char[]);write string ‘string’ to stdout. No
  // newline is added. –procedure WriteLn();write a newline sequence to stdout.
  CSymProc *read_int = new CSymProc("ReadInt", tm->GetInt());
  s->AddSymbol(read_int);

  CSymProc *write_int = new CSymProc("WriteInt", tm->GetNull());
  write_int->AddParam(new CSymParam(0, "i", tm->GetInt()));
  s->AddSymbol(write_int);

  CSymProc *write_char = new CSymProc("WriteChar", tm->GetNull());
  write_char->AddParam(new CSymParam(0, "c", tm->GetChar()));
  s->AddSymbol(write_char);

  CSymProc *write_str = new CSymProc("WriteStr", tm->GetNull());
  write_str->AddParam(new CSymParam(
      0, "string",
      tm->GetPointer((tm->GetArray(CArrayType::OPEN, tm->GetChar())))));
  s->AddSymbol(write_str);

  CSymProc *write_ln = new CSymProc("WriteLn", tm->GetNull());
  s->AddSymbol(write_ln);

  // ‘main’ is used to denote the module body in the generated assembly file
  CSymbol *keyword = new CSymbol("main", stReserved, tm->GetNull());
  s->AddSymbol(keyword);
  // keyword = new CSymbol("begin", stReserved, tm->GetNull());
  // s->AddSymbol(keyword);
  // keyword = new CSymbol("end", stReserved, tm->GetNull());
  // s->AddSymbol(keyword);
  // keyword = new CSymbol("boolean", stReserved, tm->GetNull());
  // s->AddSymbol(keyword);
  // keyword = new CSymbol("char", stReserved, tm->GetNull());
  // s->AddSymbol(keyword);
  // keyword = new CSymbol("integer", stReserved, tm->GetNull());
  // s->AddSymbol(keyword);
  // keyword = new CSymbol("if", stReserved, tm->GetNull());
  // s->AddSymbol(keyword);
  // keyword = new CSymbol("then", stReserved, tm->GetNull());
  // s->AddSymbol(keyword);
  // keyword = new CSymbol("else", stReserved, tm->GetNull());
  // s->AddSymbol(keyword);
  // keyword = new CSymbol("while", stReserved, tm->GetNull());
  // s->AddSymbol(keyword);
  // keyword = new CSymbol("do", stReserved, tm->GetNull());
  // s->AddSymbol(keyword);
  // keyword = new CSymbol("return", stReserved, tm->GetNull());
  // s->AddSymbol(keyword);
  // keyword = new CSymbol("var", stReserved, tm->GetNull());
  // s->AddSymbol(keyword);
  // keyword = new CSymbol("procedure", stReserved, tm->GetNull());
  // s->AddSymbol(keyword);
  // keyword = new CSymbol("function", stReserved, tm->GetNull());
  // s->AddSymbol(keyword);
}

CAstType *CParser::type() {
  // type ::= basetype | type "[" [ number ] "]".
  // basetype ::= "boolean" | "char" | "integer".
  // left recursive, change it to the following
  // type ::= basetype T
  // T ::= "["[number]"]"T | empty

  // check variable type
  EToken peek_type = _scanner->Peek().GetType();
  CToken baseTypeToken;
  if (peek_type == tInteger)
    Consume(tInteger, &baseTypeToken);
  else if (peek_type == tChar)
    Consume(tChar, &baseTypeToken);
  else if (peek_type == tBoolean)
    Consume(tBoolean, &baseTypeToken);
  else
    SetError(_scanner->Peek(), "basetype expected");

  CTypeManager *tm = CTypeManager::Get();
  const CType *returnType = NULL;
  if (baseTypeToken.GetValue() == "char")
    returnType = tm->GetChar();
  else if (baseTypeToken.GetValue() == "boolean")
    returnType = tm->GetBool();
  else if (baseTypeToken.GetValue() == "integer")
    returnType = tm->GetInt();

  vector<int> dimension;
  // if not tLBrak, then base type
  while (_scanner->Peek().GetType() == tLBrak) {
    Consume(tLBrak);
    if (_scanner->Peek().GetType() == tNumber) {
      CAstConstant *num = number();
      if (num->GetValue() <= 0) {
        SetError(num->GetToken(), "array dimension must >= zero or open");
      }
      dimension.push_back(num->GetValue());
    } else {
      dimension.push_back((int)CArrayType::OPEN);
    }
    Consume(tRBrak);
  }
  for (int i = dimension.size() - 1; i >= 0; i--) {
    returnType = tm->GetArray(dimension[i], returnType);
  }
  return new CAstType(baseTypeToken, returnType);
}

// todo: type check
CAstDesignator *CParser::qualident(CAstScope *s) {
  //
  // qualident ::= ident { "[" expression "]" }.
  //
  CToken t;
  Consume(tIdent, &t);
  // default global
  const CSymbol *symbol = s->GetSymbolTable()->FindSymbol(t.GetValue());
  if (symbol == NULL)
    SetError(t, "undefined identifier");
  EToken tt = _scanner->Peek().GetType();
  vector<CAstExpression *> indices;
  if (tt == tLBrak) {
    while (tt == tLBrak) {
      Consume(tLBrak);
      indices.push_back(expression(s));
      Consume(tRBrak);
      tt = _scanner->Peek().GetType();
    }
    CAstArrayDesignator *array = new CAstArrayDesignator(t, symbol);
    for (int i = 0; i < indices.size(); i++) {
      array->AddIndex(indices[i]);
    }
    array->IndicesComplete();
    return array;
  } else {
    return new CAstDesignator(t, symbol);
  }
}

void CParser::varDeclaration(CAstScope *s, bool param){
  // varDeclaration ::= [ "var" varDeclSequence ";" ].
  if(_scanner->Peek().GetType() == tVarDecl){
    Consume(tVarDecl);
    varDeclSequence(s, param);
    // Consume(tSemicolon);
  }
}

void CParser::varDeclSequence(CAstScope *s, bool param) {
  // varDeclSequence ::= varDecl { ";" varDecl }.
  while(true){
    varDecl(s, param);
    if(_scanner->Peek().GetType() == tSemicolon){
      Consume(tSemicolon);
      if(_scanner->Peek().GetType() != tIdent) return;
    }
    else return;
  }
}

void CParser::varDecl(CAstScope *s, bool param){
  // varDecl ::= ident { "," ident } ":" type.
  CSymProc *symproc = NULL;
  CToken tmp;
  vector<CToken> variables;
  Consume(tIdent, &tmp);
  variables.push_back(tmp);
  while (_scanner->Peek().GetType() != tColon) {
    Consume(tComma);
    Consume(tIdent, &tmp);
    variables.push_back(tmp);
  }
  Consume(tColon);

  CAstType *variable_type = type();
  int index = 0;
  while (variables.size() != 0) {
    // check duplicate variable declaration
    if (s->GetSymbolTable()->FindSymbol(variables.front().GetValue(),
                                        sLocal) != NULL)
      SetError(variables.front(), "duplicate variable declaration '" +
                                      variables.front().GetValue() + "'");
    if(param){
      const CPointerType *pointer = NULL;
      CSymParam *symParam = NULL;
      if(variable_type->GetType()->IsArray()){
        pointer = CTypeManager::Get()->GetPointer(variable_type->GetType());
        symParam = new CSymParam(index++, variables.front().GetValue(), pointer);
      }
      else{
        symParam = new CSymParam(index++, variables.front().GetValue(), variable_type->GetType());
      }
      s->GetSymbolTable()->AddSymbol(symParam);
    }
    else{
      s->GetSymbolTable()->AddSymbol(
          s->CreateVar(variables.front().GetValue(), variable_type->GetType()));
    }
    variables.erase(variables.begin());
  }
}

CAstModule *CParser::module(void) {
  //
  // module ::= statSequence  ".".
  //
  // module ::= "module" ident ";" varDeclaration { subroutineDecl } "begin"
  // statSequence "end" ident ".".

  CToken module_name;
  Consume(tModule);
  Consume(tIdent, &module_name);
  Consume(tSemicolon);

  CToken dummy;
  CAstModule *m = new CAstModule(dummy, module_name.GetValue());
  InitSymbolTable(m->GetSymbolTable());

  // variable declaration
  if (_scanner->Peek().GetType() == tVarDecl) {
    varDeclaration(m, false);
  }

  // subroutine declaration
  while (_scanner->Peek().GetType() == tProcedure ||
         _scanner->Peek().GetType() == tFunction)
    subroutineDecl(m);

  Consume(tBegin);
  CAstStatement *statseq = NULL;
  statseq = statSequence(m);
  m->SetStatementSequence(statseq);

  CToken check_module_name;
  Consume(tEnd);
  Consume(tIdent, &check_module_name);
  if (module_name.GetValue() != check_module_name.GetValue())
    SetError(check_module_name, "module identifier mismatch ('" +
                                    module_name.GetValue() + "' != '" +
                                    check_module_name.GetValue() + "')");
  Consume(tDot);

  return m;
}

CAstStatReturn *CParser::returnStatement(CAstScope *s) {
  CToken t;

  Consume(tReturn, &t);

  CAstExpression *rhs = expression(s);

  return new CAstStatReturn(t, s, rhs);
}

CAstStatement *CParser::statSequence(CAstScope *s) {
  // statSequence ::= [ statement { ";" statement } ].
  // statement ::= assignment | subroutineCall | ifStatement |
  // whileStatement | returnStatement.
  // FIRST(statSequence) = { tIdent, tIf, tWhile, tReturn }
  // FOLLOW(statSequence) = { tElse, tEnd }
  //
  CAstStatement *head = NULL;

  EToken ti = _scanner->Peek().GetType();
  if (ti != tSemicolon && ti != tElse && ti != tEnd) {
    CAstStatement *tail = NULL;
    do {
      CToken t;
      EToken tt = _scanner->Peek().GetType();
      CAstStatement *st = NULL;
      switch (tt) {
      // statement ::= assignment
      case tIf:
        st = ifStatement(s);
        break;
      case tWhile:
        st = whileStatement(s);
        break;
      case tReturn:
        st = returnStatement(s);
        break;
      case tIdent:{
        const CSymbol *local = s->GetSymbolTable()->FindSymbol(_scanner->Peek().GetValue(), sLocal);
        const CSymbol *global = s->GetSymbolTable()->FindSymbol(_scanner->Peek().GetValue());
        if (local != NULL && local->GetSymbolType() != stProcedure) {
          st = assignment(s);
        } 
        else if(global != NULL && global->GetSymbolType() != stProcedure){
          st = assignment(s);
        }
        else if (global != NULL && global->GetSymbolType() == stProcedure) {
          st = new CAstStatCall(_scanner->Peek(), subroutineCall(s));
        }  
        else {
          SetError(_scanner->Peek(), "undefined identifier");
        }
        break;
      }
      default:
        SetError(_scanner->Peek(), "statement expected.");
        break;
      }

      assert(st != NULL);
      if (head == NULL)
        head = st;
      else
        tail->SetNext(st);
      tail = st;

      ti = _scanner->Peek().GetType();
      if (ti == tSemicolon) {
        Consume(tSemicolon);
      } else {
        break;
      }
    } while (!_abort);
  }
  return head;
}

CAstStatAssign *CParser::assignment(CAstScope *s) {
  //
  // assignment ::= number ":=" expression.
  //
  CToken t;

  CAstDesignator *lhs = qualident(s);
  Consume(tAssign, &t);

  CAstExpression *rhs = expression(s);
  return new CAstStatAssign(t, lhs, rhs);
}

CAstExpression *CParser::expression(CAstScope *s) {
  //
  // expression ::= simpleexpr [ relOp simpleexpr ].
  //
  CToken t;
  EOperation relop = opEqual;
  CAstExpression *left = NULL, *right = NULL;

  left = simpleexpr(s);

  if (_scanner->Peek().GetType() == tRelOp) {
    Consume(tRelOp, &t);
    right = simpleexpr(s);

    if (t.GetValue() == "=")
      relop = opEqual;
    else if (t.GetValue() == "#")
      relop = opNotEqual;
    else if (t.GetValue() == "<")
      relop = opLessThan;
    else if (t.GetValue() == "<=")
      relop = opLessEqual;
    else if (t.GetValue() == ">")
      relop = opBiggerThan;
    else if (t.GetValue() == ">=")
      relop = opBiggerEqual;
    else
      SetError(t, "invalid relation.");

    return new CAstBinaryOp(t, relop, left, right);
  } else {
    return left;
  }
}

CAstExpression *CParser::simpleexpr(CAstScope *s) {
  //
  // simpleexpr ::= ["+"|"-"] term { termOp term }.
  //
  CAstExpression *n = NULL;
  CToken unaryToken;
  EOperation unaryOperation;
  if (_scanner->Peek().GetType() == tPlusMinus) {
    Consume(tPlusMinus, &unaryToken);
    if (unaryToken.GetValue() == "+")
      unaryOperation = opPos;
    else
      unaryOperation = opNeg;
  }
  n = term(s);
  // todo: need to change the value?
  if (unaryToken.GetValue() != "") {
    n = new CAstUnaryOp(unaryToken, unaryOperation, n);
  }

  while (_scanner->Peek().GetType() == tPlusMinus ||
         _scanner->Peek().GetType() == tOr) {
    CToken t;
    CAstExpression *l = n, *r;

    if (_scanner->Peek().GetType() == tPlusMinus) {
      Consume(tPlusMinus, &t);
      r = term(s);
      n = new CAstBinaryOp(t, t.GetValue() == "+" ? opAdd : opSub, l, r);

    } else if (_scanner->Peek().GetType() == tOr) {
      Consume(tOr, &t);
      r = term(s);
      n = new CAstBinaryOp(t, opOr, l, r);
    }
  }
  return n;
}

CAstExpression *CParser::term(CAstScope *s) {
  //
  // term ::= factor { factorOp factor }.
  // factorOp ::= "*" | "/" | "&&"
  //
  CAstExpression *n = NULL;

  n = factor(s);

  EToken tt = _scanner->Peek().GetType();

  while ((tt == tMulDiv) || tt == tAnd) {
    CToken t;
    CAstExpression *l = n, *r;
    if (tt == tMulDiv)
      Consume(tMulDiv, &t);
    else
      Consume(tAnd, &t);

    r = factor(s);

    if (tt == tMulDiv)
      n = new CAstBinaryOp(t, t.GetValue() == "*" ? opMul : opDiv, l, r);
    else
      n = new CAstBinaryOp(t, opAnd, l, r);

    tt = _scanner->Peek().GetType();
  }

  return n;
}

CAstExpression *CParser::factor(CAstScope *s) {
  //
  // factor ::= qualident | number | boolean | char | string | "(" expression ")" | subroutineCall | "!" factor
  //
  //

  CToken t;
  EToken tt = _scanner->Peek().GetType();
  CAstExpression *n = NULL;

  switch (tt) {
  // factor ::= number
  case tNumber:
    n = number();
    break;
  case tBoolConst:
    n = constbool();
    break;
  case tCharConst:
    n = constchar();
    break;
  case tString:
    n = stringConstant(s);
    break;
  // factor ::= "(" expression ")"
  case tLParens:
    Consume(tLParens);
    n = expression(s);
    Consume(tRParens);
    break;
  // qualident and subroutineCall
  case tIdent:{
    // local variable
    const CSymbol *local = s->GetSymbolTable()->FindSymbol(_scanner->Peek().GetValue(), sLocal);
    const CSymbol *global = s->GetSymbolTable()->FindSymbol(_scanner->Peek().GetValue());
    if (local != NULL && local->GetSymbolType() != stProcedure) {
      n = qualident(s);
    }
    // global variable
    else if (global != NULL && global->GetSymbolType() != stProcedure) {
      n = qualident(s);
    }
    // subroutine call
    else if (global != NULL && global->GetSymbolType() == stProcedure) {
      n = subroutineCall(s);
    } else {
      SetError(_scanner->Peek(), "undefined identifier");
    }
    break;
  }
  case tNot:
    // tNot
    Consume(tNot, &t);
    n = factor(s);
    // todo: need to change the value?
    n = new CAstUnaryOp(t, opNot, n);
    break;
  default:
    cout << "got " << _scanner->Peek() << endl;
    SetError(_scanner->Peek(), "factor expected.");
    break;
  }
  return n;
}

CAstStringConstant *CParser::stringConstant(CAstScope *s) {
  //
  // string ::= '"' { character }'"'.
  //
  // "digit { digit }" is scanned as one token (tNumber)
  //

  CToken t;

  Consume(tString, &t);

  errno = 0;
  string v = t.GetValue().substr(1, t.GetValue().length() - 2);

  return new CAstStringConstant(t, v, s);
}

CAstConstant *CParser::number(void) {
  //
  // number ::= digit { digit }.
  //
  // "digit { digit }" is scanned as one token (tNumber)
  //

  CToken t;

  Consume(tNumber, &t);

  errno = 0;
  long long v = strtoll(t.GetValue().c_str(), NULL, 10);
  if (errno != 0)
    SetError(t, "invalid number.");

  return new CAstConstant(t, CTypeManager::Get()->GetInt(), v);
}

CAstConstant *CParser::constchar(void) {
  // character = printable ASCIIchar | "\n" | "\t" | "\"" | "\'" | "\\"
  // char = "'" character | "\0" "'"
  CToken t;
  Consume(tCharConst, &t);
  const char *arr = t.GetValue().c_str();
  if (t.GetValue().length() == 2) {
    char escape;
    // || _in -> peek() == '\n' || _in -> peek() == '\t'
    // || _in -> peek() == '\''
    // || _in -> peek() == '\\' || _in -> peek() == '\0')
    switch (arr[1]) {
    case 'n':
      escape = '\n';
      break;
    case 't':
      escape = '\t';
      break;
    case '\"':
      escape = '\'';
      break;
    case '\'':
      escape = '\'';
      break;
    case '\\':
      escape = '\\';
      break;
    case '0':
      escape = '\0';
      break;
    }
    return new CAstConstant(t, CTypeManager::Get()->GetChar(), int(escape));
  } else if (t.GetValue().length() == 1) {
    return new CAstConstant(t, CTypeManager::Get()->GetChar(), int(arr[0]));
  } else
    SetError(t, "not allowed char");
}

CAstConstant *CParser::constbool(void) {
  CToken t;
  Consume(tBoolConst, &t);
  errno = 0;
  int v;
  if (t.GetValue() == "false")
    v = 0;
  else
    v = 1;
  return new CAstConstant(t, CTypeManager::Get()->GetBool(), v);
}

CAstStatWhile *CParser::whileStatement(CAstScope *s) {
  //
  // whileStatement ::= "while" "(" expression ")" "do" statSequence "end"
  //
  // FIRST(while) = { tWhile }
  // FOLLOW(while) = { tEnd }
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

CAstStatIf *CParser::ifStatement(CAstScope *s) {
  //
  // ifStatement ::= "if" "(" expression ")" "then" statSequence ["else" statSequence ] "end"
  //
  // FIRST(if) = { tIf }
  // FOLLOW(if) = { tEnd }
  //

  CToken t;
  CAstExpression *cond = NULL;
  CAstStatement *ifbody = NULL;
  CAstStatement *elsebody = NULL;

  Consume(tIf, &t);
  Consume(tLParens);
  cond = expression(s);
  Consume(tRParens);
  Consume(tThen);
  ifbody = statSequence(s);

  EToken tt = _scanner->Peek().GetType();

  if (tt == tElse) {
    Consume(tElse);
    elsebody = statSequence(s);
  }

  Consume(tEnd);
  return new CAstStatIf(t, cond, ifbody, elsebody);
}
CAstProcedure *CParser::subroutineDecl(CAstScope *s){
  // subroutineDecl = (procedureDecl | functionDecl) subroutineBody ident ";".
  CAstProcedure *subroutine = NULL;
  if (_scanner->Peek().GetType() == tProcedure)
    subroutine = procedureDecl(s);
  else
    subroutine = functionDecl(s);
  subroutine = subroutineBody(subroutine);
  CToken ident;
  Consume(tIdent, &ident);
  Consume(tSemicolon);
  if(ident.GetValue() != subroutine->GetSymbol()->GetName()){
    SetError(ident, "subroutine name inconsistent.");
  }
  return subroutine;
}

CAstProcedure *CParser::procedureDecl(CAstScope *s){
  // procedureDecl = "procedure" ident [ formalParam ] ";".
  // formalParam ::= "(" [ varDeclSequence ] ")".
  Consume(tProcedure);
  CToken ident;
  Consume(tIdent, &ident);
  CSymProc *symproc = new CSymProc(ident.GetValue(), CTypeManager::Get()->GetNull());
  CAstProcedure *procedure = new CAstProcedure(ident, ident.GetValue(), s, symproc);
  if(_scanner->Peek().GetType() == tLParens){
    Consume(tLParens);
    if(_scanner->Peek().GetType() == tIdent){
      varDeclSequence(procedure, true);
    }
    Consume(tRParens);
  }
  Consume(tSemicolon);
  vector<CSymbol *> symbols = procedure->GetSymbolTable()->GetSymbols();
  for(int i = 0; i < symbols.size(); i++){
    if(symbols[i]->GetSymbolType() == stParam){
      symproc->AddParam(dynamic_cast<CSymParam *>(symbols[i]));
    }
  }
  s->GetSymbolTable()->AddSymbol(symproc);
  return procedure;
}

CAstProcedure *CParser::functionDecl(CAstScope *s) {
  // functionDecl = "function" ident [ formalParam ] ":" type ";".
  // formalParam ::= "(" [ varDeclSequence ] ")".
  Consume(tFunction);
  CToken ident;
  Consume(tIdent, &ident);
  CSymProc *symproc = new CSymProc(ident.GetValue(), type() -> GetType());
  CAstProcedure *function = new CAstProcedure(ident, ident.GetValue(), s, symproc);
  if(_scanner->Peek().GetType() == tLParens){
    Consume(tLParens);
    if(_scanner->Peek().GetType() == tIdent){
      varDeclSequence(function, true);
    }
    Consume(tRParens);
  }
  Consume(tColon);
  CAstType *returnType = type();
  Consume(tSemicolon);
  vector<CSymbol *> symbols = function->GetSymbolTable()->GetSymbols();
  for(int i = 0; i < symbols.size(); i++){
    if(symbols[i]->GetSymbolType() == stParam){
      symproc->AddParam(dynamic_cast<CSymParam *>(symbols[i]));
    }
  }
  s->GetSymbolTable()->AddSymbol(symproc);
  return function;
}

CAstProcedure *CParser::subroutineBody(CAstProcedure *subroutine){
  // subroutineBody
  // varDeclaration "begin" statSequence "end".
  varDeclaration(subroutine, false);
  Consume(tBegin);
  CAstStatement *statseq = statSequence(subroutine);
  subroutine->SetStatementSequence(statseq);
  Consume(tEnd);
  return subroutine;
}

CAstFunctionCall *CParser::subroutineCall(CAstScope *s) {
  // subroutineCall = ident "(" [ expression {"," expression} ] ")".
  CToken ident;
  Consume(tIdent, &ident);

  const CSymProc *symbol = dynamic_cast<const CSymProc *>(
      s->GetSymbolTable()->FindSymbol(ident.GetValue(), sGlobal));
  if (symbol == NULL) {
    SetError(ident, "undefined identifier.");
    return NULL;
  } else if (symbol->GetSymbolType() != stProcedure) {
    SetError(ident, "not a subroutine call");
    return NULL;
  }
  Consume(tLParens);
  CAstFunctionCall *functionCall = new CAstFunctionCall(ident, symbol);

  if (_scanner->Peek().GetType() != tRParens) {
    CAstExpression *ex = expression(s);
    if(ex->GetType()->IsArray()){
      ex = new CAstSpecialOp(ident, opAddress, ex);
    }
    functionCall->AddArg(ex);
    
    while (_scanner->Peek().GetType() == tComma) {
      Consume(tComma);
      ex = expression(s);
      if(ex->GetType()->IsArray()){
        ex = new CAstSpecialOp(_scanner->Peek(), opAddress, ex);
      }
      functionCall->AddArg(ex);
    }
  }
  Consume(tRParens);
  return functionCall;
}
