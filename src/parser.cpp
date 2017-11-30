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
  keyword = new CSymbol("begin", stReserved, tm->GetNull());
  s->AddSymbol(keyword);
  keyword = new CSymbol("end", stReserved, tm->GetNull());
  s->AddSymbol(keyword);
  keyword = new CSymbol("boolean", stReserved, tm->GetNull());
  s->AddSymbol(keyword);
  keyword = new CSymbol("char", stReserved, tm->GetNull());
  s->AddSymbol(keyword);
  keyword = new CSymbol("integer", stReserved, tm->GetNull());
  s->AddSymbol(keyword);
  keyword = new CSymbol("if", stReserved, tm->GetNull());
  s->AddSymbol(keyword);
  keyword = new CSymbol("then", stReserved, tm->GetNull());
  s->AddSymbol(keyword);
  keyword = new CSymbol("else", stReserved, tm->GetNull());
  s->AddSymbol(keyword);
  keyword = new CSymbol("while", stReserved, tm->GetNull());
  s->AddSymbol(keyword);
  keyword = new CSymbol("do", stReserved, tm->GetNull());
  s->AddSymbol(keyword);
  keyword = new CSymbol("return", stReserved, tm->GetNull());
  s->AddSymbol(keyword);
  keyword = new CSymbol("var", stReserved, tm->GetNull());
  s->AddSymbol(keyword);
  keyword = new CSymbol("procedure", stReserved, tm->GetNull());
  s->AddSymbol(keyword);
  keyword = new CSymbol("function", stReserved, tm->GetNull());
  s->AddSymbol(keyword);
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
    while (tt = tLBrak) {
      Consume(tLBrak);
      indices.push_back(expression(s));
      Consume(tRBrak);
    }
    CAstArrayDesignator *array = new CAstArrayDesignator(t, symbol);
    for (int i = 0; i < indices.size(); i++) {
      array->AddIndex(indices[i]);
    }
    array->IndicesComplete();
  } else {
    return new CAstDesignator(t, symbol);
  }
}

void CParser::variable_declaration(CAstScope *s) {
  // varDeclaration ::= [ "var" varDeclSequence ";" ].
  // varDeclSequence ::= varDecl { ";" varDecl }.
  // varDecl ::= ident { "," ident } ":" type.
  Consume(tVarDecl);
  vector<CToken> variables;
  EToken peek_type = _scanner->Peek().GetType();
  do {
    // at least one var
    CToken tmp;
    Consume(tIdent, &tmp);
    variables.push_back(tmp);
    peek_type = _scanner->Peek().GetType();
    while (peek_type != tColon) {
      Consume(tComma);
      Consume(tIdent, &tmp);
      variables.push_back(tmp);
      peek_type = _scanner->Peek().GetType();
    }
    Consume(tColon);

    CAstType *variable_type = type();
    while (variables.size() != 0) {
      // check duplicate variable declaration
      if (s->GetSymbolTable()->FindSymbol(variables.front().GetValue(),
                                          sLocal) != NULL)
        SetError(variables.front(), "duplicate variable declaration '" +
                                        variables.front().GetValue() + "'");
      s->GetSymbolTable()->AddSymbol(
          s->CreateVar(variables.front().GetValue(), variable_type->GetType()));
      variables.erase(variables.begin());
    }

    Consume(tSemicolon);
    peek_type = _scanner->Peek().GetType();
  } while (peek_type != tBegin && peek_type != tProcedure &&
           peek_type != tFunction);
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
    variable_declaration(m);
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
      case tIdent:
        if (s->GetSymbolTable()->FindSymbol(_scanner->Peek().GetValue(),
                                            sLocal) != NULL &&
            s->GetSymbolTable()
                    ->FindSymbol(_scanner->Peek().GetValue(), sLocal)
                    ->GetSymbolType() != stProcedure) {
          st = assignment(s);
        } else if (s->GetSymbolTable()->FindSymbol(_scanner->Peek().GetValue(),
                                                   sGlobal) != NULL &&
                   s->GetSymbolTable()
                           ->FindSymbol(_scanner->Peek().GetValue(), sGlobal)
                           ->GetSymbolType() == stProcedure) {
          st = new CAstStatCall(_scanner->Peek(), subroutineCall(s));
        } else if (s->GetSymbolTable()->FindSymbol(_scanner->Peek().GetValue(),
                                                   sGlobal) != NULL &&
                   s->GetSymbolTable()
                           ->FindSymbol(_scanner->Peek().GetValue(), sGlobal)
                           ->GetSymbolType() != stProcedure) {
          st = assignment(s);
        } else {
          SetError(_scanner->Peek(), "undefined identifier");
        }
        break;
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
    else if (t.GetValue() == "!=")
      relop = opNotEqual;
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
  if (unaryToken.GetValue() != "") {
    n = new CAstUnaryOp(unaryToken, unaryOperation, n);
  }

  // CAstConstant *constant = dynamic_cast<CAstConstant *>(n);
  // if(constant != NULL && constant -> GetType() -> Match(CTypeManager::Get()
  // -> GetInt()) && unaryToken.GetValue() == "-" ){
  //     constant -> SetValue(-(constant -> GetValue()));
  //     n = new CAstUnaryOp(unaryToken, unaryOperation, constant);
  // }

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
  // term ::= factor { ("*"|"/") factor }.
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
  // factor ::= number | "(" expression ")"
  //
  // FIRST(factor) = { tNumber, tLBrak, tBoolConst, tCharConst, tString, tIdent,
  // tNot }
  //

  CToken t;
  EToken tt = _scanner->Peek().GetType();
  CAstExpression *n = NULL;

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
    n = constbool();
    break;
  case tCharConst:
    n = constchar();
    break;
  case tString:
    n = stringConstant(s);
    break;
  case tIdent:
    // local variable
    if (s->GetSymbolTable()->FindSymbol(_scanner->Peek().GetValue(), sLocal) !=
            NULL &&
        s->GetSymbolTable()
                ->FindSymbol(_scanner->Peek().GetValue(), sLocal)
                ->GetSymbolType() != stProcedure) {
      n = qualident(s);
    }
    // global variable
    else if (s->GetSymbolTable()->FindSymbol(_scanner->Peek().GetValue(),
                                             sGlobal) != NULL &&
             s->GetSymbolTable()
                     ->FindSymbol(_scanner->Peek().GetValue(), sGlobal)
                     ->GetSymbolType() == stGlobal) {
      n = qualident(s);
    }
    // subroutine call
    else if (s->GetSymbolTable()->FindSymbol(_scanner->Peek().GetValue(),
                                             sGlobal) != NULL &&
             s->GetSymbolTable()
                     ->FindSymbol(_scanner->Peek().GetValue(), sGlobal)
                     ->GetSymbolType() == stProcedure) {
      n = subroutineCall(s);
    } else {
      SetError(_scanner->Peek(), "undefined identifier");
    }
    break;
  case tNot:
    // tNot
    Consume(tNot, &t);
    n = factor(s);
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
  // ifStatement ::= "if" "(" expression ")" "then" statSequence ["else"]
  // statSequence "end"
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

CAstProcedure *CParser::subroutineDecl(CAstScope *s) {
  // procedureDecl = "procedure" ident [ formalParam ] ";".
  // functionDecl = "function" ident [ formalParam ] ":" type ";".
  CToken pt;
  if (_scanner->Peek().GetType() == tFunction)
    Consume(tFunction);
  else
    Consume(tProcedure);
  Consume(tIdent, &pt);

  // check duplicate subroutine declaration
  if (s->GetSymbolTable()->FindSymbol(pt.GetValue(), sGlobal) != NULL &&
      s->GetSymbolTable()
              ->FindSymbol(pt.GetValue(), sGlobal)
              ->GetSymbolType() == stProcedure) {

    SetError(pt, "duplicate subroutine declaration '" + pt.GetValue() + "'");
  }

  // formalParam ::= "(" [ varDeclSequence ] ")".
  // add :type here
  // => formalParam ::= "(" [ varDeclSequence ] ")" [":" type]
  vector<CToken> variables;
  vector<int> variable_count;
  vector<CAstType *> types;
  if (_scanner->Peek().GetType() == tLParens) {
    Consume(tLParens);
    if (_scanner->Peek().GetType() != tRParens) {
      EToken peek_type = _scanner->Peek().GetType();
      while (peek_type != tRParens) {
        // at least one var
        int count = 0;
        CToken tmp;
        Consume(tIdent, &tmp);
        variables.push_back(tmp);
        count++;
        peek_type = _scanner->Peek().GetType();
        while (peek_type != tColon) {
          Consume(tComma);
          Consume(tIdent, &tmp);
          variables.push_back(tmp);
          count++;
          peek_type = _scanner->Peek().GetType();
        }
        Consume(tColon);
        variable_count.push_back(count);
        types.push_back(type());

        peek_type = _scanner->Peek().GetType();
        if (peek_type == tSemicolon)
          Consume(tSemicolon);
      }
    }
    Consume(tRParens);
  }

  // return value
  CSymProc *symbol;
  if (_scanner->Peek().GetType() == tColon) {
    Consume(tColon);
    CAstType *return_type = type();
    if (return_type->GetType()->IsArray()) {
      const CArrayType *array_type =
          dynamic_cast<const CArrayType *>(return_type->GetType());
      if (array_type->GetNElem() != -1)
        SetError(pt, "invalid composite type for function.");
    } else if (return_type->GetType()->IsPointer() &&
               dynamic_cast<const CPointerType *>(return_type->GetType())
                   ->GetBaseType()
                   ->IsArray()) {
      const CArrayType *array_type = dynamic_cast<const CArrayType *>(
          dynamic_cast<const CPointerType *>(return_type->GetType())
              ->GetBaseType());
      if (array_type->GetNElem() != -1)
        SetError(pt, "invalid composite type for function.");
    }
    symbol = new CSymProc(pt.GetValue(), return_type->GetType());
  }

  // procedure
  else
    symbol = new CSymProc(pt.GetValue(), CTypeManager::Get()->GetNull());
  CAstProcedure *subroutine = new CAstProcedure(pt, pt.GetValue(), s, symbol);
  int index = 0;

  while (variable_count.size() > 0) {
    // check duplicate variable declaration
    int count = variable_count.front();
    variable_count.erase(variable_count.begin());
    while (count-- > 0) {
      vector<CSymbol *> symbols = subroutine->GetSymbolTable()->GetSymbols();
      vector<string> symbols_string;
      for (vector<CSymbol *>::iterator it = symbols.begin();
           it != symbols.end(); ++it) {
        symbols_string.push_back((*it)->GetName());
      }
      if (find(symbols_string.begin(), symbols_string.end(),
               variables.front().GetValue()) != symbols_string.end()) {
        SetError(variables.front(), "duplicate variable declaration '" +
                                        variables.front().GetValue() + "'");
      }
      subroutine->GetSymbolTable()->AddSymbol(new CSymParam(
          index, variables.front().GetValue(), types.front()->GetType()));
      symbol->AddParam(new CSymParam(index++, variables.front().GetValue(),
                                     types.front()->GetType()));
      variables.erase(variables.begin());
    }
    types.erase(types.begin());
  }
  Consume(tSemicolon);
  s->GetSymbolTable()->AddSymbol(symbol);

  if (_scanner->Peek().GetType() == tVarDecl) {
    variable_declaration(subroutine);
  }

  Consume(tBegin);

  // statSequence
  CAstStatement *statseq = NULL;
  statseq = statSequence(subroutine);
  subroutine->SetStatementSequence(statseq);

  Consume(tEnd);
  CToken check_subroutine_name;
  Consume(tIdent, &check_subroutine_name);
  if (check_subroutine_name.GetValue() != pt.GetValue())
    SetError(check_subroutine_name,
             "procedure/function identifier mismatch ('" + pt.GetValue() +
                 "' != '" + check_subroutine_name.GetValue() + "')");
  Consume(tSemicolon);
  return subroutine;
}

CAstFunctionCall *CParser::subroutineCall(CAstScope *s) {
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
    functionCall->AddArg(expression(s));
    while (_scanner->Peek().GetType() == tComma) {
      Consume(tComma);
      functionCall->AddArg(expression(s));
    }
  }
  Consume(tRParens);
  return functionCall;
}
