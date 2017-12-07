//------------------------------------------------------------------------------
/// @brief SnuPL abstract syntax tree
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/05/22 Bernhard Egger reimplemented TAC generation
/// 2013/11/04 Bernhard Egger added typechecks for unary '+' operators
/// 2016/03/12 Bernhard Egger adapted to SnuPL/1
/// 2014/04/08 Bernhard Egger assignment 2: AST for SnuPL/-1
/// 2017/09/23 Bernhard Egger assignment 2: minor bugfixes
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

#include <cassert>
#include <cstring>
#include <iostream>

#include <typeinfo>

#include "ast.h"
using namespace std;

//------------------------------------------------------------------------------
// CAstNode
//
int CAstNode::_global_id = 0;

CAstNode::CAstNode(CToken token) : _token(token), _addr(NULL) {
  _id = _global_id++;
}

CAstNode::~CAstNode(void) {
  if (_addr != NULL)
    delete _addr;
}

int CAstNode::GetID(void) const { return _id; }

CToken CAstNode::GetToken(void) const { return _token; }

const CType *CAstNode::GetType(void) const {
  return CTypeManager::Get()->GetNull();
}

string CAstNode::dotID(void) const {
  ostringstream out;
  out << "node" << dec << _id;
  return out.str();
}

string CAstNode::dotAttr(void) const { return " [label=\"" + dotID() + "\"]"; }

void CAstNode::toDot(ostream &out, int indent) const {
  string ind(indent, ' ');

  out << ind << dotID() << dotAttr() << ";" << endl;
}

CTacAddr *CAstNode::GetTacAddr(void) const { return _addr; }

ostream &operator<<(ostream &out, const CAstNode &t) { return t.print(out); }

ostream &operator<<(ostream &out, const CAstNode *t) { return t->print(out); }

//------------------------------------------------------------------------------
// CAstScope
//
CAstScope::CAstScope(CToken t, const string name, CAstScope *parent)
    : CAstNode(t), _name(name), _symtab(NULL), _parent(parent), _statseq(NULL),
      _cb(NULL) {
  if (_parent != NULL)
    _parent->AddChild(this);
}

CAstScope::~CAstScope(void) {
  delete _symtab;
  delete _statseq;
  delete _cb;
}

const string CAstScope::GetName(void) const { return _name; }

CAstScope *CAstScope::GetParent(void) const { return _parent; }

size_t CAstScope::GetNumChildren(void) const { return _children.size(); }

CAstScope *CAstScope::GetChild(size_t i) const {
  assert(i < _children.size());
  return _children[i];
}

CSymtab *CAstScope::GetSymbolTable(void) const {
  assert(_symtab != NULL);
  return _symtab;
}

void CAstScope::SetStatementSequence(CAstStatement *statseq) {
  _statseq = statseq;
}

CAstStatement *CAstScope::GetStatementSequence(void) const { return _statseq; }

bool CAstScope::TypeCheck(CToken *t, string *msg) const {
  bool result = true;
  try {
    CAstStatement *s = _statseq;
    while (result && (s != NULL)) {
      result = s->TypeCheck(t, msg);
      s = s->GetNext();
    }
    vector<CAstScope *>::const_iterator it = _children.begin();
    while (result && (it != _children.end())) {
      result = (*it)->TypeCheck(t, msg);
      it++;
    }
  } catch (...) {
    result = false;
  }
  return result;
}

ostream &CAstScope::print(ostream &out, int indent) const {
  string ind(indent, ' ');

  out << ind << "CAstScope: '" << _name << "'" << endl;
  out << ind << "  symbol table:" << endl;
  _symtab->print(out, indent + 4);
  out << ind << "  statement list:" << endl;
  CAstStatement *s = GetStatementSequence();
  if (s != NULL) {
    do {
      s->print(out, indent + 4);
      s = s->GetNext();
    } while (s != NULL);
  } else {
    out << ind << "    empty." << endl;
  }

  out << ind << "  nested scopes:" << endl;
  if (_children.size() > 0) {
    for (size_t i = 0; i < _children.size(); i++) {
      _children[i]->print(out, indent + 4);
    }
  } else {
    out << ind << "    empty." << endl;
  }
  out << ind << endl;

  return out;
}

void CAstScope::toDot(ostream &out, int indent) const {
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  CAstStatement *s = GetStatementSequence();
  if (s != NULL) {
    string prev = dotID();
    do {
      s->toDot(out, indent);
      out << ind << prev << " -> " << s->dotID() << " [style=dotted];" << endl;
      prev = s->dotID();
      s = s->GetNext();
    } while (s != NULL);
  }

  vector<CAstScope *>::const_iterator it = _children.begin();
  while (it != _children.end()) {
    CAstScope *s = *it++;
    s->toDot(out, indent);
    out << ind << dotID() << " -> " << s->dotID() << ";" << endl;
  }
}

CTacAddr *CAstScope::ToTac(CCodeBlock *cb) {
  assert(cb != NULL);
  CAstStatement *s = GetStatementSequence();
  while (s != NULL) {
    CTacLabel *next = cb->CreateLabel();
    s->ToTac(cb, next);
    cb->AddInstr(next);
    s = s->GetNext();
  }
  cb->CleanupControlFlow();
  return NULL;
}

CCodeBlock *CAstScope::GetCodeBlock(void) const { return _cb; }

void CAstScope::SetSymbolTable(CSymtab *st) {
  if (_symtab != NULL)
    delete _symtab;
  _symtab = st;
}

void CAstScope::AddChild(CAstScope *child) { _children.push_back(child); }

//------------------------------------------------------------------------------
// CAstModule
//
CAstModule::CAstModule(CToken t, const string name) : CAstScope(t, name, NULL) {
  SetSymbolTable(new CSymtab());
}

CSymbol *CAstModule::CreateVar(const string ident, const CType *type) {
  return new CSymGlobal(ident, type);
}

string CAstModule::dotAttr(void) const {
  return " [label=\"m " + GetName() + "\",shape=box]";
}

//------------------------------------------------------------------------------
// CAstProcedure
//
CAstProcedure::CAstProcedure(CToken t, const string name, CAstScope *parent,
                             CSymProc *symbol)
    : CAstScope(t, name, parent), _symbol(symbol) {
  assert(GetParent() != NULL);
  SetSymbolTable(new CSymtab(GetParent()->GetSymbolTable()));
  assert(_symbol != NULL);
}

CSymProc *CAstProcedure::GetSymbol(void) const { return _symbol; }

CSymbol *CAstProcedure::CreateVar(const string ident, const CType *type) {
  return new CSymLocal(ident, type);
}

const CType *CAstProcedure::GetType(void) const {
  return GetSymbol()->GetDataType();
}

string CAstProcedure::dotAttr(void) const {
  return " [label=\"p/f " + GetName() + "\",shape=box]";
}

//------------------------------------------------------------------------------
// CAstType
//
CAstType::CAstType(CToken t, const CType *type) : CAstNode(t), _type(type) {
  assert(type != NULL);
}

const CType *CAstType::GetType(void) const { return _type; }

ostream &CAstType::print(ostream &out, int indent) const {
  string ind(indent, ' ');

  out << ind << "CAstType (" << _type << ")" << endl;
  return out;
}

//------------------------------------------------------------------------------
// CAstStatement
//
CAstStatement::CAstStatement(CToken token) : CAstNode(token), _next(NULL) {}

CAstStatement::~CAstStatement(void) { delete _next; }

void CAstStatement::SetNext(CAstStatement *next) { _next = next; }

CAstStatement *CAstStatement::GetNext(void) const { return _next; }

CTacAddr *CAstStatement::ToTac(CCodeBlock *cb, CTacLabel *next) {
  cb->AddInstr(new CTacInstr(opGoto, next));
  return NULL;
}

//------------------------------------------------------------------------------
// CAstStatAssign
//
CAstStatAssign::CAstStatAssign(CToken t, CAstDesignator *lhs,
                               CAstExpression *rhs)
    : CAstStatement(t), _lhs(lhs), _rhs(rhs) {
  assert(lhs != NULL);
  assert(rhs != NULL);
}

CAstDesignator *CAstStatAssign::GetLHS(void) const { return _lhs; }

CAstExpression *CAstStatAssign::GetRHS(void) const { return _rhs; }

bool CAstStatAssign::TypeCheck(CToken *t, string *msg) const {
  ostringstream msg_stream;

  if (!_lhs->TypeCheck(t, msg)) {
    return false;
  }
  if (!_rhs->TypeCheck(t, msg)) {
    return false;
  }
  const CType *lType = _lhs->GetType();

  // Confirm left type is not array.
  if (lType == NULL || !lType->IsScalar()) {
    msg_stream << "Left hand side designator must be of type scalar. Got: ";
    lType != NULL ? msg_stream << lType : msg_stream << "<INVALID>";
    *msg = msg_stream.str();
    *t = _lhs->GetToken();
    return false;
  }

  const CType *rType = _rhs->GetType();
  // Confirm right type is not invalid. Accept array variables - TODO.
  if (rType == NULL || !rType->IsScalar()) {
    msg_stream << "Left hand side designator must be of type scalar. Got: ";
    rType != NULL ? msg_stream << rType : msg_stream << "<INVALID>";
    *msg = msg_stream.str();
    *t = _rhs->GetToken();
    return false;
  }

  if (!lType->Match(rType)) {
    msg_stream << "Right hand side designator must be of type " << lType
               << ". Got: " << rType;
    *msg = msg_stream.str();
    *t = GetToken();
    return false;
  }
  return true;
}

const CType *CAstStatAssign::GetType(void) const { return _lhs->GetType(); }

ostream &CAstStatAssign::print(ostream &out, int indent) const {
  string ind(indent, ' ');

  out << ind << ":="
      << " ";

  const CType *t = GetType();
  if (t != NULL)
    out << t;
  else
    out << "<INVALID>";

  out << endl;

  _lhs->print(out, indent + 2);
  _rhs->print(out, indent + 2);

  return out;
}

string CAstStatAssign::dotAttr(void) const {
  return " [label=\":=\",shape=box]";
}

void CAstStatAssign::toDot(ostream &out, int indent) const {
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _lhs->toDot(out, indent);
  out << ind << dotID() << "->" << _lhs->dotID() << ";" << endl;
  _rhs->toDot(out, indent);
  out << ind << dotID() << "->" << _rhs->dotID() << ";" << endl;
}

CTacAddr *CAstStatAssign::ToTac(CCodeBlock *cb, CTacLabel *next) {
  CTacAddr *lhs = _lhs->ToTac(cb);
  CTacAddr *rhs = _rhs->ToTac(cb);
  cb->AddInstr(new CTacInstr(opAssign, lhs, rhs));
  cb->AddInstr(new CTacInstr(opGoto, next));
  return NULL;
}

//------------------------------------------------------------------------------
// CAstStatCall
//
CAstStatCall::CAstStatCall(CToken t, CAstFunctionCall *call)
    : CAstStatement(t), _call(call) {
  assert(call != NULL);
}

CAstFunctionCall *CAstStatCall::GetCall(void) const { return _call; }

bool CAstStatCall::TypeCheck(CToken *t, string *msg) const {
  return GetCall()->TypeCheck(t, msg);
}

ostream &CAstStatCall::print(ostream &out, int indent) const {
  _call->print(out, indent);

  return out;
}

string CAstStatCall::dotID(void) const { return _call->dotID(); }

string CAstStatCall::dotAttr(void) const { return _call->dotAttr(); }

void CAstStatCall::toDot(ostream &out, int indent) const {
  _call->toDot(out, indent);
}

CTacAddr *CAstStatCall::ToTac(CCodeBlock *cb, CTacLabel *next) {
  _call->ToTac(cb);
  cb->AddInstr(new CTacInstr(opGoto, next));
  return NULL;
}

//------------------------------------------------------------------------------
// CAstStatReturn
//
CAstStatReturn::CAstStatReturn(CToken t, CAstScope *scope, CAstExpression *expr)
    : CAstStatement(t), _scope(scope), _expr(expr) {
  assert(scope != NULL);
}

CAstScope *CAstStatReturn::GetScope(void) const { return _scope; }

CAstExpression *CAstStatReturn::GetExpression(void) const { return _expr; }

bool CAstStatReturn::TypeCheck(CToken *t, string *msg) const {
  const CType *st = GetScope()->GetType();
  CAstExpression *e = GetExpression();
  if (st->Match(CTypeManager::Get()->GetNull())) {
    if (e != NULL) {
      if (t != NULL)
        *t = e->GetToken();
      if (msg != NULL)
        *msg = "superfluous expression after return.";
      return false;
    }
  } else {
    if (e == NULL) {
      if (t != NULL)
        *t = GetToken();
      if (msg != NULL)
        *msg = "expression expected after return.";
      return false;
    }
    if (!e->TypeCheck(t, msg))
      return false;
    if (!st->Match(e->GetType())) {
      if (t != NULL)
        *t = e->GetToken();
      if (msg != NULL)
        *msg = "return type mismatch.";
      return false;
    }
  }
  return true;
}

const CType *CAstStatReturn::GetType(void) const {
  const CType *t = NULL;

  if (GetExpression() != NULL) {
    t = GetExpression()->GetType();
  } else {
    t = CTypeManager::Get()->GetNull();
  }

  return t;
}

ostream &CAstStatReturn::print(ostream &out, int indent) const {
  string ind(indent, ' ');

  out << ind << "return"
      << " ";

  const CType *t = GetType();
  if (t != NULL)
    out << t;
  else
    out << "<INVALID>";

  out << endl;

  if (_expr != NULL)
    _expr->print(out, indent + 2);

  return out;
}

string CAstStatReturn::dotAttr(void) const {
  return " [label=\"return\",shape=box]";
}

void CAstStatReturn::toDot(ostream &out, int indent) const {
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  if (_expr != NULL) {
    _expr->toDot(out, indent);
    out << ind << dotID() << "->" << _expr->dotID() << ";" << endl;
  }
}

CTacAddr *CAstStatReturn::ToTac(CCodeBlock *cb, CTacLabel *next) {
  if (_expr != NULL)
    cb->AddInstr(new CTacInstr(opReturn, NULL, _expr->ToTac(cb)));
  else
    cb->AddInstr(new CTacInstr(opReturn, NULL));
  cb->AddInstr(new CTacInstr(opGoto, next));
  return NULL;
}

//------------------------------------------------------------------------------
// CAstStatIf
//
CAstStatIf::CAstStatIf(CToken t, CAstExpression *cond, CAstStatement *ifBody,
                       CAstStatement *elseBody)
    : CAstStatement(t), _cond(cond), _ifBody(ifBody), _elseBody(elseBody) {
  assert(cond != NULL);
}

CAstExpression *CAstStatIf::GetCondition(void) const { return _cond; }

CAstStatement *CAstStatIf::GetIfBody(void) const { return _ifBody; }

CAstStatement *CAstStatIf::GetElseBody(void) const { return _elseBody; }

bool CAstStatIf::TypeCheck(CToken *t, string *msg) const {
  // Type Check condition
  CAstExpression *cond = GetCondition();
  if (!cond->TypeCheck(t, msg))
    return false;

  if (!cond->GetType()->Match(CTypeManager::Get()->GetBool())) {
    if (t != NULL) {
      *t = cond->GetToken();
    }
    if (msg != NULL) {
      *msg = "Condition is not evaluated as boolean.";
    }
    return false;
  }

  // Recursive type check if statements
  for (CAstStatement *tBody = GetIfBody(); tBody != NULL;
       tBody = tBody->GetNext()) {
    if (!tBody->TypeCheck(t, msg))
      return false;
  }
  for (CAstStatement *eBody = GetElseBody(); eBody != NULL;
       eBody = eBody->GetNext()) {
    if (!eBody->TypeCheck(t, msg))
      return false;
  }
  return true;
}

ostream &CAstStatIf::print(ostream &out, int indent) const {
  string ind(indent, ' ');

  out << ind << "if cond" << endl;
  _cond->print(out, indent + 2);
  out << ind << "if-body" << endl;
  if (_ifBody != NULL) {
    CAstStatement *s = _ifBody;
    do {
      s->print(out, indent + 2);
      s = s->GetNext();
    } while (s != NULL);
  } else {
    out << ind << "  empty." << endl;
  }
  out << ind << "else-body" << endl;
  if (_elseBody != NULL) {
    CAstStatement *s = _elseBody;
    do {
      s->print(out, indent + 2);
      s = s->GetNext();
    } while (s != NULL);
  } else {
    out << ind << "  empty." << endl;
  }

  return out;
}

string CAstStatIf::dotAttr(void) const { return " [label=\"if\",shape=box]"; }

void CAstStatIf::toDot(ostream &out, int indent) const {
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _cond->toDot(out, indent);
  out << ind << dotID() << "->" << _cond->dotID() << ";" << endl;

  if (_ifBody != NULL) {
    CAstStatement *s = _ifBody;
    if (s != NULL) {
      string prev = dotID();
      do {
        s->toDot(out, indent);
        out << ind << prev << " -> " << s->dotID() << " [style=dotted];"
            << endl;
        prev = s->dotID();
        s = s->GetNext();
      } while (s != NULL);
    }
  }

  if (_elseBody != NULL) {
    CAstStatement *s = _elseBody;
    if (s != NULL) {
      string prev = dotID();
      do {
        s->toDot(out, indent);
        out << ind << prev << " -> " << s->dotID() << " [style=dotted];"
            << endl;
        prev = s->dotID();
        s = s->GetNext();
      } while (s != NULL);
    }
  }
}

CTacAddr *CAstStatIf::ToTac(CCodeBlock *cb, CTacLabel *next) {
  CTacLabel *ltrue = cb->CreateLabel("if_true");
  CTacLabel *lfalse = cb->CreateLabel("if_false");
  // boolean expression condition
  _cond->ToTac(cb, ltrue, lfalse);

  cb->AddInstr(ltrue);
  // statSequence
  CAstStatement *ifStatSequence = GetIfBody();
  while (ifStatSequence != NULL) {
    CTacLabel *nextInIf = cb->CreateLabel();
    ifStatSequence->ToTac(cb, nextInIf);
    cb->AddInstr(nextInIf);
    ifStatSequence = ifStatSequence->GetNext();
  }
  cb->AddInstr(new CTacInstr(opGoto, next));

  cb->AddInstr(lfalse);
  // statSequence
  CAstStatement *elseStatSequence = GetElseBody();
  while (elseStatSequence != NULL) {
    CTacLabel *nextInElse = cb->CreateLabel();
    elseStatSequence->ToTac(cb, nextInElse);
    cb->AddInstr(nextInElse);
    elseStatSequence = elseStatSequence->GetNext();
  }
  cb->AddInstr(new CTacInstr(opGoto, next));

  return NULL;
}

//------------------------------------------------------------------------------
// CAstStatWhile
//
CAstStatWhile::CAstStatWhile(CToken t, CAstExpression *cond,
                             CAstStatement *body)
    : CAstStatement(t), _cond(cond), _body(body) {
  assert(cond != NULL);
}

CAstExpression *CAstStatWhile::GetCondition(void) const { return _cond; }

CAstStatement *CAstStatWhile::GetBody(void) const { return _body; }

bool CAstStatWhile::TypeCheck(CToken *t, string *msg) const {
  // Type Check condition
  CAstExpression *cond = GetCondition();
  if (!cond->TypeCheck(t, msg))
    return false;

  if (!cond->GetType()->Match(CTypeManager::Get()->GetBool())) {
    if (t != NULL) {
      *t = cond->GetToken();
    }
    if (msg != NULL) {
      *msg = "Condition is not evaluated as boolean.";
    }
    return false;
  }

  // Recursive type check if statements
  for (CAstStatement *tBody = GetBody(); tBody != NULL;
       tBody = tBody->GetNext()) {
    if (!tBody->TypeCheck(t, msg)) {
      return false;
    }
  }

  return true;
}

ostream &CAstStatWhile::print(ostream &out, int indent) const {
  string ind(indent, ' ');

  out << ind << "while cond" << endl;
  _cond->print(out, indent + 2);
  out << ind << "while-body" << endl;
  if (_body != NULL) {
    CAstStatement *s = _body;
    do {
      s->print(out, indent + 2);
      s = s->GetNext();
    } while (s != NULL);
  } else
    out << ind << "  empty." << endl;

  return out;
}

string CAstStatWhile::dotAttr(void) const {
  return " [label=\"while\",shape=box]";
}

void CAstStatWhile::toDot(ostream &out, int indent) const {
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _cond->toDot(out, indent);
  out << ind << dotID() << "->" << _cond->dotID() << ";" << endl;

  if (_body != NULL) {
    CAstStatement *s = _body;
    if (s != NULL) {
      string prev = dotID();
      do {
        s->toDot(out, indent);
        out << ind << prev << " -> " << s->dotID() << " [style=dotted];"
            << endl;
        prev = s->dotID();
        s = s->GetNext();
      } while (s != NULL);
    }
  }
}
CTacAddr *CAstStatWhile::ToTac(CCodeBlock *cb, CTacLabel *next) {
  CTacLabel *whileLabel = cb -> CreateLabel("while_cond");
  CTacLabel *ltrue = cb -> CreateLabel("while_body");
  cb -> AddInstr(whileLabel);

  // boolean expression
  _cond->ToTac(cb, ltrue, next);
  cb->AddInstr(ltrue);
  // statSequence
  CAstStatement *s = GetBody();
  while (s != NULL) {
    CTacLabel *nextInWhile = cb -> CreateLabel();
    s -> ToTac(cb, nextInWhile);
    cb -> AddInstr(nextInWhile);
    s = s -> GetNext();
  }
  cb -> AddInstr(new CTacInstr(opGoto, whileLabel));
  cb -> AddInstr(new CTacInstr(opGoto, next));
  return NULL;
}

//------------------------------------------------------------------------------
// CAstExpression
//
CAstExpression::CAstExpression(CToken t) : CAstNode(t) {}

void CAstExpression::SetParenthesized(bool parenthesized) {
  _parenthesized = parenthesized;
}

bool CAstExpression::GetParenthesized(void) const { return _parenthesized; }

CTacAddr *CAstExpression::ToTac(CCodeBlock *cb) { return NULL; }

CTacAddr *CAstExpression::ToTac(CCodeBlock *cb, CTacLabel *ltrue,
                                CTacLabel *lfalse) {
  // for boolean expression
  return NULL;
}

//------------------------------------------------------------------------------
// CAstOperation
//
CAstOperation::CAstOperation(CToken t, EOperation oper)
    : CAstExpression(t), _oper(oper) {}

EOperation CAstOperation::GetOperation(void) const { return _oper; }

//------------------------------------------------------------------------------
// CAstBinaryOp
//
CAstBinaryOp::CAstBinaryOp(CToken t, EOperation oper, CAstExpression *l,
                           CAstExpression *r)
    : CAstOperation(t, oper), _left(l), _right(r) {
  // these are the only binary operation we support for now
  assert((oper == opAdd) || (oper == opSub) || (oper == opMul) ||
         (oper == opDiv) || (oper == opAnd) || (oper == opOr) ||
         (oper == opEqual) || (oper == opNotEqual) || (oper == opLessThan) ||
         (oper == opLessEqual) || (oper == opBiggerThan) ||
         (oper == opBiggerEqual));
  assert(l != NULL);
  assert(r != NULL);
}

CAstExpression *CAstBinaryOp::GetLeft(void) const { return _left; }

CAstExpression *CAstBinaryOp::GetRight(void) const { return _right; }

bool CAstBinaryOp::TypeCheck(CToken *t, string *msg) const {
  // Recursive type checking
  if (!_left->TypeCheck(t, msg) || !_right->TypeCheck(t, msg)) {
    return false;
  }

  const CType *leftType = _left->GetType();
  const CType *rightType = _right->GetType();
  switch (GetOperation()) {
  case opAdd:
  case opDiv:
  case opMul:
  case opSub:
    // Integer Operations
    if (!(leftType->Match(CTypeManager::Get()->GetInt()) &&
          rightType->Match(CTypeManager::Get()->GetInt()))) {
      if (t != NULL)
        *t = GetToken();
      if (msg != NULL)
        *msg = "expected integer type in binary op";
      return false;
    } else {
      return true;
    }
  case opAnd:
  case opOr:
    // Boolean Operations
    if (!(leftType->Match(CTypeManager::Get()->GetBool()) &&
          rightType->Match(CTypeManager::Get()->GetBool()))) {
      if (t != NULL)
        *t = GetToken();
      if (msg != NULL)
        *msg = "expected boolean type in binary op";
      return false;
    } else {
      return true;
    }
  case opEqual:
  case opNotEqual:
    // Equality Operations
    // Match necessary
    if (!(leftType->Match(CTypeManager::Get()->GetBool()) ||
          leftType->Match(CTypeManager::Get()->GetChar()) ||
          leftType->Match(CTypeManager::Get()->GetInt()))) {
      if (t != NULL)
        *t = _left->GetToken();
      if (msg != NULL)
        *msg = "expected boolean or character or integer type expression in "
               "left hand side";
      return false;
    } else if (!(rightType->Match(leftType))) {
      if (t != NULL)
        *t = _right->GetToken();
      if (msg != NULL)
        *msg = "expected right and left hand side to type match";
      return false;
    }
    return true;
  case opBiggerEqual:
  case opBiggerThan:
  case opLessEqual:
  case opLessThan:
    // Comparison Operations
    if (!(leftType->Match(CTypeManager::Get()->GetInt()) ||
          leftType->Match(CTypeManager::Get()->GetChar()))) {
      if (t != NULL) {
        *t = GetToken();
      }
      if (msg != NULL) {
        *msg = " expected integer or char in left hand side";
      }
      return false;
    } else if (!leftType->Match(rightType)) {
      if (t != NULL) {
        *t = GetToken();
      }
      if (msg != NULL) {
        *msg = "operand type must match";
      }
      return false;
    } else
      return true;
    // unknown operator
  default:
    if (t != NULL) {
      *t = GetToken();
    }
    if (msg) {
      *msg = "unknown operation";
    }
    return false;
  }
}

const CType *CAstBinaryOp::GetType(void) const {
  EOperation oper = GetOperation();
  if ((oper == opAnd) || (oper == opOr) || (oper == opEqual) ||
      (oper == opNotEqual) || (oper == opLessThan) || (oper == opLessEqual) ||
      (oper == opBiggerThan) || (oper == opBiggerEqual))
    return CTypeManager::Get()->GetBool();
  else
    return CTypeManager::Get()->GetInt();
}

ostream &CAstBinaryOp::print(ostream &out, int indent) const {
  string ind(indent, ' ');

  out << ind << GetOperation() << " ";

  const CType *t = GetType();
  if (t != NULL)
    out << t;
  else
    out << "<INVALID>";

  out << endl;

  _left->print(out, indent + 2);
  _right->print(out, indent + 2);

  return out;
}

string CAstBinaryOp::dotAttr(void) const {
  ostringstream out;
  out << " [label=\"" << GetOperation() << "\",shape=box]";
  return out.str();
}

void CAstBinaryOp::toDot(ostream &out, int indent) const {
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _left->toDot(out, indent);
  out << ind << dotID() << "->" << _left->dotID() << ";" << endl;
  _right->toDot(out, indent);
  out << ind << dotID() << "->" << _right->dotID() << ";" << endl;
}

CTacAddr *CAstBinaryOp::ToTac(CCodeBlock *cb) {
  CTypeManager *tm = CTypeManager::Get();
  if (tm->GetBool()->Match(GetType())) {
    CTacLabel *ltrue = cb->CreateLabel();
    CTacLabel *lfalse = cb->CreateLabel();
    CTacLabel *next = cb->CreateLabel();
    ToTac(cb, ltrue, lfalse);
    
    CTacTemp *target = cb -> CreateTemp(GetType());
		cb->AddInstr(ltrue);
		cb->AddInstr(new CTacInstr(opAssign, target, new CTacConst(1)));
    cb->AddInstr(new CTacInstr(opGoto, next));
		cb->AddInstr(lfalse);
    cb->AddInstr(new CTacInstr(opAssign, target, new CTacConst(0)));
		cb->AddInstr(next);
		return target;
  } else {
    CTacAddr *operand1 = _left -> ToTac(cb);
    CTacAddr *operand2 = _right -> ToTac(cb);
    CTacTemp *target = cb -> CreateTemp(GetType());
    cb -> AddInstr(new CTacInstr(GetOperation(), target, operand1, operand2));
    return target;
  }
}

CTacAddr *CAstBinaryOp::ToTac(CCodeBlock *cb, CTacLabel *ltrue,
                              CTacLabel *lfalse) {
  EOperation operation = GetOperation();
  switch (operation) {
    case opAnd:
    case opOr: {
      CTacLabel *shortCircuit = cb->CreateLabel();
      if (operation == opAnd)
        _left->ToTac(cb, shortCircuit, lfalse);
      else
        _left->ToTac(cb, ltrue, shortCircuit);

      cb->AddInstr(shortCircuit);
      _right->ToTac(cb, ltrue, lfalse);
      break;
    }
    default: {
      CTacAddr *operand1 = _left->ToTac(cb);
      CTacAddr *operand2 = _right->ToTac(cb);
      cb->AddInstr(new CTacInstr(operation, ltrue, operand1, operand2));
      cb->AddInstr(new CTacInstr(opGoto, lfalse));
      break;
    }
  }
  return NULL;
}

//------------------------------------------------------------------------------
// CAstUnaryOp
//
CAstUnaryOp::CAstUnaryOp(CToken t, EOperation oper, CAstExpression *e)
    : CAstOperation(t, oper), _operand(e) {
  assert((oper == opNeg) || (oper == opPos) || (oper == opNot));
  assert(e != NULL);
}

CAstExpression *CAstUnaryOp::GetOperand() const { return _operand; }
bool CAstUnaryOp::TypeCheck(CToken *t, string *msg) const {
  if (!_operand->TypeCheck(t, msg)) {
    return false;
  }

  const CType *type = _operand->GetType();
  if (type == NULL) {
    if (t != NULL) {
      *t = GetToken();
    }
    if (msg != NULL) {
      *msg = "unexpected null operand";
    }
    return false;
  }

  switch (GetOperation()) {
  case opNeg:
  case opPos:
    // - v +
    if (!type->Match(CTypeManager::Get()->GetInt())) {
      if (t != NULL)
        *t = GetToken();
      if (msg != NULL)
        *msg = " expected integer in operand";
      return false;
    } else {
      return true;
    }
  case opNot:
    // !
    if (!type->Match(CTypeManager::Get()->GetBool())) {
      if (t != NULL) {
        *t = GetToken();
      }
      if (msg != NULL) {
        *msg = " expected boolean in operand";
      }
      return false;
    } else {
      return true;
    }
  default:
    if (t != NULL) {
      *t = GetToken();
    }
    if (msg != NULL) {
      *msg = " invalid operation";
    }
    return false;
  }
  return true;
}

const CType *CAstUnaryOp::GetType(void) const {
  if (GetOperation() == opNeg || GetOperation() == opPos)
    return CTypeManager::Get()->GetInt();
  else
    return CTypeManager::Get()->GetBool();
}

ostream &CAstUnaryOp::print(ostream &out, int indent) const {
  string ind(indent, ' ');

  out << ind << GetOperation() << " ";

  const CType *t = GetType();

  if (t != NULL)
    out << t;
  else
    out << "<INVALID>";
  out << endl;

  _operand->print(out, indent + 2);

  return out;
}

string CAstUnaryOp::dotAttr(void) const {
  ostringstream out;
  out << " [label=\"" << GetOperation() << "\",shape=box]";
  return out.str();
}

void CAstUnaryOp::toDot(ostream &out, int indent) const {
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _operand->toDot(out, indent);
  out << ind << dotID() << "->" << _operand->dotID() << ";" << endl;
}

CTacAddr *CAstUnaryOp::ToTac(CCodeBlock *cb) {
  // pos, neg, not
  switch (GetOperation()) {
    case opPos:{
      CTacAddr *operand = _operand -> ToTac(cb);
      return operand;
    }
    case opNeg: {
      CTacTemp *result = cb -> CreateTemp(GetType());
      CTacAddr *operand = _operand -> ToTac(cb);
      cb -> AddInstr(new CTacInstr(GetOperation(), result, operand));
      return result;
    }
    case opNot: {
      CTacLabel *ltrue = cb -> CreateLabel();
      CTacLabel *lfalse = cb -> CreateLabel();
      CTacLabel *next = cb -> CreateLabel();
      ToTac(cb, ltrue, lfalse);

      CTacTemp *result = cb -> CreateTemp(CTypeManager::Get() -> GetBool());
      cb -> AddInstr(ltrue);
      cb -> AddInstr(new CTacInstr(opAssign, result, new CTacConst(1)));
      cb -> AddInstr(new CTacInstr(opGoto, next));
      cb -> AddInstr(lfalse);
      cb -> AddInstr(new CTacInstr(opAssign, result, new CTacConst(0)));
      cb -> AddInstr(next);
      return result;
    }
  }
}

CTacAddr *CAstUnaryOp::ToTac(CCodeBlock *cb, CTacLabel *ltrue,
                             CTacLabel *lfalse) {
  // for opNot
  _operand->ToTac(cb, lfalse, ltrue);
	return NULL;
}

//------------------------------------------------------------------------------
// CAstSpecialOp
//
CAstSpecialOp::CAstSpecialOp(CToken t, EOperation oper, CAstExpression *e,
                             const CType *type)
    : CAstOperation(t, oper), _operand(e), _type(type) {
  assert((oper == opAddress) || (oper == opDeref) || (oper = opCast));
  assert(e != NULL);
  assert(((oper != opCast) && (type == NULL)) ||
         ((oper == opCast) && (type != NULL)));
}

CAstExpression *CAstSpecialOp::GetOperand(void) const { return _operand; }

bool CAstSpecialOp::TypeCheck(CToken *t, string *msg) const {
  if (!_operand->TypeCheck(t, msg)) {
    return false;
  }
  switch (GetOperation()) {
  case opAddress:
    // TypeCheck Array
    if (!_operand->GetType()->IsArray()) {
      if (t != NULL) {
        *t = GetToken();
      }
      if (msg != NULL) {
        *msg = "expect operand type to be array with opAddress";
      }
      return false;
    }
    return true;
  case opDeref:
    // check if the type is pointer type
    if (!_operand->GetType()->IsPointer()) {
      if (t != NULL) {
        *t = GetToken();
      }
      if (msg != NULL) {
        *msg = "expected pointer type in opDeref";
      }
      return false;
    }
    return true;
  case opCast:
    if (t != NULL) {
      *t = GetToken();
    }
    if (msg != NULL) {
      *msg = "invalid op";
    }
    return false;
  default:
    return false;
  }
}

const CType *CAstSpecialOp::GetType(void) const {
  CTypeManager *tm = CTypeManager::Get();
  switch (GetOperation()) {
    case opAddress:
      return tm->GetPointer(_operand->GetType());
    case opDeref:
      const CPointerType* pointer;
      pointer = dynamic_cast<const CPointerType*>(_operand->GetType());
      if (pointer == NULL) return NULL;
      return pointer->GetBaseType();
    case opCast:
      return _type;
    default:
      return NULL;
  }
}

ostream &CAstSpecialOp::print(ostream &out, int indent) const {
  string ind(indent, ' ');

  out << ind << GetOperation() << " ";

  const CType *t = GetType();
  if (t != NULL)
    out << t;
  else
    out << "<INVALID>";
  out << endl;

  _operand->print(out, indent + 2);

  return out;
}

string CAstSpecialOp::dotAttr(void) const {
  ostringstream out;
  out << " [label=\"" << GetOperation() << "\",shape=box]";
  return out.str();
}

void CAstSpecialOp::toDot(ostream &out, int indent) const {
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _operand->toDot(out, indent);
  out << ind << dotID() << "->" << _operand->dotID() << ";" << endl;
}

CTacAddr *CAstSpecialOp::ToTac(CCodeBlock *cb) {
  CTacAddr *result = cb->CreateTemp(GetType());
  CTacAddr *operand = GetOperand()->ToTac(cb);
  // only use opAddress
  cb->AddInstr(new CTacInstr(opAddress, result, operand));
  return result;
}

//------------------------------------------------------------------------------
// CAstFunctionCall
//
CAstFunctionCall::CAstFunctionCall(CToken t, const CSymProc *symbol)
    : CAstExpression(t), _symbol(symbol) {
  assert(symbol != NULL);
}

const CSymProc *CAstFunctionCall::GetSymbol() const { return _symbol; }

void CAstFunctionCall::AddArg(CAstExpression *arg) { _arg.push_back(arg); }

int CAstFunctionCall::GetNArgs() const { return (int)_arg.size(); }

CAstExpression *CAstFunctionCall::GetArg(int index) const {
  assert((index >= 0) && (index < _arg.size()));
  return _arg[index];
}

bool CAstFunctionCall::TypeCheck(CToken *t, string *msg) const {
  const CSymProc *symbol = GetSymbol();
  ostringstream out;
  if (symbol->GetNParams() != GetNArgs()) {
    if (t != NULL) {
      *t = GetToken();
    }
    if (msg != NULL) {
      out << " expected " << GetSymbol()->GetNParams() << " parameters. Got "
          << GetNArgs();
      *msg = out.str();
    }
    return false;
  }
  for (int i = 0; i < GetNArgs(); i++) {
    CAstExpression *arg = GetArg(i);
    if (!arg->TypeCheck(t, msg)) {
      return false;
    }    
    const CType *paramType = symbol->GetParam(i)->GetDataType();
    const CType *argType = arg->GetType();
    if(paramType->IsPointer() && argType->IsPointer()){
      const CArrayType *paramArray = dynamic_cast<const CArrayType *>(dynamic_cast<const CPointerType *>(paramType)->GetBaseType());
      const CArrayType *argArray = dynamic_cast<const CArrayType *>(dynamic_cast<const CPointerType *>(argType)->GetBaseType());
      if(paramArray == NULL || argArray == NULL || !paramArray->Match(argArray)){
        if (t != NULL) {
          *t = arg->GetToken();
        }
        if (msg != NULL) {
          *msg = "array type not match";
        }
        return false;
      }
    }
		if (!paramType->Match(argType) || argType == NULL) {
			if (t != NULL) {
        *t = GetToken();
      }
			if (msg != NULL) {
        *msg = "argument type does not match parameter type";
      }
			return false;
		}
  }
  return true;
}

const CType *CAstFunctionCall::GetType() const {
  return GetSymbol()->GetDataType();
}

ostream &CAstFunctionCall::print(ostream &out, int indent) const {
  string ind(indent, ' ');

  out << ind << "call " << _symbol << " ";
  const CType *t = GetType();
  if (t != NULL)
    out << t;
  else
    out << "<INVALID>";
  out << endl;

  for (size_t i = 0; i < _arg.size(); i++) {
    _arg[i]->print(out, indent + 2);
  }

  return out;
}

string CAstFunctionCall::dotAttr(void) const {
  ostringstream out;
  out << " [label=\"call " << _symbol->GetName() << "\",shape=box]";
  return out.str();
}

void CAstFunctionCall::toDot(ostream &out, int indent) const {
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  for (size_t i = 0; i < _arg.size(); i++) {
    _arg[i]->toDot(out, indent);
    out << ind << dotID() << "->" << _arg[i]->dotID() << ";" << endl;
  }
}

CTacAddr *CAstFunctionCall::ToTac(CCodeBlock *cb) {
  CTypeManager *tm = CTypeManager::Get();
  if(tm -> GetBool() -> Match(GetType())){
    CTacLabel *ltrue = cb -> CreateLabel("function_true");
    CTacLabel *lfalse = cb -> CreateLabel("function_false");
    return ToTac(cb, ltrue, lfalse);
  }
  for (int i = GetNArgs() - 1; i >= 0; i--)
    cb -> AddInstr(
        new CTacInstr(opParam, new CTacConst(i), GetArg(i) -> ToTac(cb)));

  CTacAddr *result = NULL;
  if(!tm->GetNull()->Match(GetSymbol()->GetDataType())){
    result = cb -> CreateTemp(GetType());
  }
  cb -> AddInstr(new CTacInstr(opCall, result, new CTacName(GetSymbol())));
  return result;
}

CTacAddr *CAstFunctionCall::ToTac(CCodeBlock *cb, CTacLabel *ltrue,
                                  CTacLabel *lfalse) {
  CTacLabel *next = cb -> CreateLabel();
  CTacAddr *result = cb -> CreateTemp(CTypeManager::Get() -> GetBool());
  for (int i = GetNArgs() - 1; i >= 0; i--)
    cb -> AddInstr(
        new CTacInstr(opParam, new CTacConst(i), GetArg(i) -> ToTac(cb)));
  cb -> AddInstr(new CTacInstr(opCall, result, new CTacName(GetSymbol())));
  cb -> AddInstr(ltrue);
  cb -> AddInstr(new CTacInstr(opEqual, ltrue, result, new CTacConst(1)));
  cb -> AddInstr(new CTacInstr(opGoto, next));
  cb -> AddInstr(lfalse);
  cb -> AddInstr(new CTacInstr(opEqual, lfalse, result, new CTacConst(0)));
  cb -> AddInstr(next);
  return result;
}

//------------------------------------------------------------------------------
// CAstOperand
//
CAstOperand::CAstOperand(CToken t) : CAstExpression(t) {}

//------------------------------------------------------------------------------
// CAstDesignator
//
CAstDesignator::CAstDesignator(CToken t, const CSymbol *symbol)
    : CAstOperand(t), _symbol(symbol) {
  assert(symbol != NULL);
}

const CSymbol *CAstDesignator::GetSymbol() const { return _symbol; }

bool CAstDesignator::TypeCheck(CToken *t, string *msg) const {
  if (GetType() == NULL || GetType()->IsNull()) {
    if (t != NULL) {
      *t = GetToken();
    }
    if (msg != NULL) {
      *msg = " invalid type";
    }
    return false;
  }
  return true;
}

const CType *CAstDesignator::GetType() const {
  return GetSymbol()->GetDataType();
}

ostream &CAstDesignator::print(ostream &out, int indent) const {
  string ind(indent, ' ');

  out << ind << _symbol << " ";

  const CType *t = GetType();
  if (t != NULL)
    out << t;
  else
    out << "<INVALID>";

  out << endl;

  return out;
}

string CAstDesignator::dotAttr() const {
  ostringstream out;
  out << " [label=\"" << _symbol->GetName();
  out << "\",shape=ellipse]";
  return out.str();
}

void CAstDesignator::toDot(ostream &out, int indent) const {
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);
}

CTacAddr *CAstDesignator::ToTac(CCodeBlock *cb) {
  return new CTacName(GetSymbol());
}

CTacAddr *CAstDesignator::ToTac(CCodeBlock *cb, CTacLabel *ltrue,
                                CTacLabel *lfalse) {
  CTacAddr *target = ToTac(cb);
  cb->AddInstr(new CTacInstr(opEqual, ltrue, target, new CTacConst(1)));
  cb->AddInstr(new CTacInstr(opGoto, lfalse));
  return target;
}

//------------------------------------------------------------------------------
// CAstArrayDesignator
//
CAstArrayDesignator::CAstArrayDesignator(CToken t, const CSymbol *symbol)
    : CAstDesignator(t, symbol), _done(false), _offset(NULL) {}

void CAstArrayDesignator::AddIndex(CAstExpression *idx) {
  assert(!_done);
  _idx.push_back(idx);
}

void CAstArrayDesignator::IndicesComplete() {
  assert(!_done);
  _done = true;
}

int CAstArrayDesignator::GetNIndices() const { return (int)_idx.size(); }

CAstExpression *CAstArrayDesignator::GetIndex(int index) const {
  assert((index >= 0) && (index < _idx.size()));
  return _idx[index];
}

bool CAstArrayDesignator::TypeCheck(CToken *t, string *msg) const {
  assert(_done);
  // get type is array or pointer
  // check dimension
  const CType *symbolType = GetSymbol()->GetDataType();
  const CArrayType *type = NULL;
  if(symbolType->IsPointer()) {
    type = dynamic_cast<const CArrayType *>(dynamic_cast<const CPointerType *>(symbolType)->GetBaseType());
  }
  else{
    type = dynamic_cast<const CArrayType *>(symbolType);
  }
  if(type == NULL || !type->IsArray()){
    if (t != NULL) {
      *t = GetToken();
    }
    if (msg != NULL) {
      *msg = " not an array type";
    }
    return false;
  }
  if(type->GetNDim() != GetNIndices()){
    if (t != NULL) {
      *t = GetToken();
    }
    if (msg != NULL) {
      *msg = " dimension not matched";
    }
    return false;
  }
  for (int i = 0; i < GetNIndices(); i++) {
    CAstExpression *exp = GetIndex(i);
    if (!exp->TypeCheck(t, msg)) {
      return false;
    }
    const CType *type = exp->GetType();
    if (type == NULL || !type->Match(CTypeManager::Get()->GetInt())) {
      if (t != NULL) {
        *t = exp->GetToken();
      }
      if (msg != NULL) {
        *msg = " expected index to be integer";
      }
      return false;
    }
  }
  return true;
}

const CType *CAstArrayDesignator::GetType() const {
  const CType *t = GetSymbol()->GetDataType();
  while (t->IsPointer())
    t = dynamic_cast<const CPointerType *>(t)->GetBaseType();
  if (t->IsArray()){
    return dynamic_cast<const CArrayType *>(t)->GetBaseType();
  }else{
    return NULL;
  }
}

ostream &CAstArrayDesignator::print(ostream &out, int indent) const {
  string ind(indent, ' ');

  out << ind << _symbol << " ";

  const CType *t = GetType();
  if (t != NULL)
    out << t;
  else
    out << "<INVALID>";

  out << endl;

  for (size_t i = 0; i < _idx.size(); i++) {
    _idx[i]->print(out, indent + 2);
  }

  return out;
}

string CAstArrayDesignator::dotAttr() const {
  ostringstream out;
  out << " [label=\"" << _symbol->GetName() << "[]\",shape=ellipse]";
  return out.str();
}

void CAstArrayDesignator::toDot(ostream &out, int indent) const {
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  for (size_t i = 0; i < _idx.size(); i++) {
    _idx[i]->toDot(out, indent);
    out << ind << dotID() << "-> " << _idx[i]->dotID() << ";" << endl;
  }
}

CTacAddr *CAstArrayDesignator::ToTac(CCodeBlock *cb) {
  const CPointerType *pointerType;
	const CArrayType *arrayType;
	CAstExpression *arrayPointer;
  CToken token = GetToken();
  CTypeManager *tm = CTypeManager::Get();

  // wrap array, no need to get baseType of array, use astDesignator
	if (GetSymbol()->GetDataType()->IsPointer()) {
		pointerType = dynamic_cast<const CPointerType *>(GetSymbol()->GetDataType());
    arrayType = dynamic_cast<const CArrayType *>(pointerType->GetBaseType());
    arrayPointer = new CAstDesignator(token, GetSymbol());
	} 
  else {
    arrayType = dynamic_cast<const CArrayType *>(GetSymbol()->GetDataType());
    arrayPointer = new CAstSpecialOp(token, opAddress, new CAstDesignator(token, GetSymbol()));
  }
	int baseTypeSize = arrayType->GetBaseType()->GetSize();

  // function DIM(array: pointer to array; dim: integer): integer;
  // returns the size of the 'dim'-th array dimension of 'array'.
	CAstExpression *offset;
	const CSymProc *dim = dynamic_cast<const CSymProc *>(cb->GetOwner()->GetSymbolTable()->FindSymbol("DIM"));
	for (int i = 0; i < GetNIndices(); i++) {
		if (i == 0) {
			offset = GetIndex(i);
		}
    else {
			offset = new CAstBinaryOp(token, opAdd, offset, GetIndex(i));
		}
		if (i == GetNIndices() - 1) {
			offset = new CAstBinaryOp(token, opMul, offset, new CAstConstant(token, tm->GetInt(), baseTypeSize));
		}
    else {
			CAstFunctionCall *dimCall = new CAstFunctionCall(token, dim);
			dimCall->AddArg(arrayPointer);
			dimCall->AddArg(new CAstConstant(token, tm->GetInt(), i + 2));
			offset = new CAstBinaryOp(token, opMul, offset, dimCall);
		}
  }

  // Function DOFS(array: pointer to array): integer;
  // returns the number of bytes from the starting address of the array to the first data element.
	const CSymProc *dofs = dynamic_cast<const CSymProc *>(cb->GetOwner()->GetSymbolTable()->FindSymbol("DOFS"));
	CAstFunctionCall *dofsCall = new CAstFunctionCall(token, dofs);
  dofsCall->AddArg(arrayPointer);
	CAstBinaryOp *address = new CAstBinaryOp(token, opAdd, offset, dofsCall);
	address = new CAstBinaryOp(token, opAdd, arrayPointer, address);
  
  CTacName *result = dynamic_cast<CTacName *>(address->ToTac(cb));
	return new CTacReference(result->GetSymbol(), GetSymbol());
}

CTacAddr *CAstArrayDesignator::ToTac(CCodeBlock *cb, CTacLabel *ltrue,
                                     CTacLabel *lfalse) {
	CTacAddr *target = ToTac(cb);
	cb->AddInstr(new CTacInstr(opEqual, ltrue, target, new CTacConst(1)));
	cb->AddInstr(new CTacInstr(opGoto, lfalse));
	return target;
}

//------------------------------------------------------------------------------
// CAstConstant
//
CAstConstant::CAstConstant(CToken t, const CType *type, long long value)
    : CAstOperand(t), _type(type), _value(value) {}

void CAstConstant::SetValue(long long value) { _value = value; }

long long CAstConstant::GetValue() const { return _value; }

string CAstConstant::GetValueStr() const {
  ostringstream out;

  if (GetType() == CTypeManager::Get()->GetBool()) {
    out << (_value == 0 ? "false" : "true");
  } else if (GetType() == CTypeManager::Get()->GetChar()) {
    out << _value;
  } else {
    out << dec << _value;
  }

  return out.str();
}

bool CAstConstant::TypeCheck(CToken *t, string *msg) const {
  if (!_type) {
    if (t != NULL) {
      *t = GetToken();
    }
    if (msg != NULL) {
      *msg = "invalid constant type";
    }
    return false;
  } else if (_type->Match(CTypeManager::Get()->GetInt())) {
    long long v = GetValue();

    // if (v < INT32_MIN || v > INT32_MAX) {
    if (v > INT32_MAX) {
      if (t != NULL) {
        *t = GetToken();
      }
      if (msg != NULL) {
        *msg = "invalid value for integer. Overflow condition";
      }
      return false;
    }
  } else if (_type->Match(CTypeManager::Get()->GetBool())) {
    if (_value != 0 && _value != 1) {
      if (t != NULL) {
        *t = GetToken();
      }
      if (msg != NULL) {
        *msg = "invalid value for boolean";
      }
      return false;
    }
  } else if (_type->Match(CTypeManager::Get()->GetChar())) {
    if (_value < 0 || _value > 255) {
      if (t != NULL) {
        *t = GetToken();
      }
      if (msg != NULL) {
        *msg = "invalid value for character constant";
      }
      return false;
    }
  } else {
    if (t != NULL) {
      *t = GetToken();
    }
    if (msg != NULL) {
      *msg = "invalid constant type";
    }
    return false;
  }
  return true;
}

const CType *CAstConstant::GetType(void) const { return _type; }

ostream &CAstConstant::print(ostream &out, int indent) const {
  string ind(indent, ' ');

  out << ind << GetValueStr() << " ";

  const CType *t = GetType();
  if (t != NULL)
    out << t;
  else
    out << "<INVALID>";

  out << endl;

  return out;
}

string CAstConstant::dotAttr(void) const {
  ostringstream out;
  out << " [label=\"" << GetValueStr() << "\",shape=ellipse]";
  return out.str();
}

CTacAddr *CAstConstant::ToTac(CCodeBlock *cb) {
  return  new CTacConst(GetValue());
}

CTacAddr *CAstConstant::ToTac(CCodeBlock *cb, CTacLabel *ltrue,
                              CTacLabel *lfalse) {
  CTacAddr *value = new CTacConst(GetValue());
  cb->AddInstr(new CTacInstr(opEqual, ltrue, value, new CTacConst(1)));
  cb->AddInstr(new CTacInstr(opGoto, lfalse));
  return value;
}

//------------------------------------------------------------------------------
// CAstStringConstant
//
int CAstStringConstant::_idx = 0;

CAstStringConstant::CAstStringConstant(CToken t, const string value,
                                       CAstScope *s)
    : CAstOperand(t) {
  CTypeManager *tm = CTypeManager::Get();
  CSymtab *st = s->GetSymbolTable();

  _type =
      tm->GetArray(strlen(CToken::unescape(value).c_str()) + 1, tm->GetChar());
  _value = new CDataInitString(value);

  // in case of name clashes we simply iterate until we find a
  // name that has not yet been used
  _sym = NULL;
  do {
    ostringstream o;
    o << "_str_" << ++_idx;
    if (st->FindSymbol(o.str(), sGlobal) == NULL) {
      _sym = new CSymGlobal(o.str(), _type);
    }
  } while (_sym == NULL);

  _sym->SetData(_value);
  st->AddSymbol(_sym);
}

const string CAstStringConstant::GetValue(void) const {
  return _value->GetData();
}

const string CAstStringConstant::GetValueStr() const { return GetValue(); }

bool CAstStringConstant::TypeCheck(CToken *t, string *msg) const {
  if (GetType() == NULL || GetType()->IsNull()) {
    if (t != NULL) {
      *t = GetToken();
    }
    if (msg != NULL) {
      *msg = "invalid constant type";
    }
    return false;
  }
  return true;
}

const CType *CAstStringConstant::GetType() const { return _type; }

ostream &CAstStringConstant::print(ostream &out, int indent) const {
  string ind(indent, ' ');

  out << ind << '"' << GetValueStr() << '"' << " ";

  const CType *t = GetType();
  if (t != NULL)
    out << t;
  else
    out << "<INVALID>";

  out << endl;

  return out;
}

string CAstStringConstant::dotAttr(void) const {
  ostringstream out;
  // the string is already escaped, but dot requires double escaping
  out << " [label=\"\\\"" << CToken::escape(GetValueStr())
      << "\\\"\",shape=ellipse]";
  return out.str();
}

CTacAddr *CAstStringConstant::ToTac(CCodeBlock *cb) {
  return new CTacName(_sym);
}

CTacAddr *CAstStringConstant::ToTac(CCodeBlock *cb, CTacLabel *ltrue,
                                    CTacLabel *lfalse) {
  return new CTacName(_sym);
}
