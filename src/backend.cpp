//------------------------------------------------------------------------------
/// @brief SnuPL backend
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/11/28 Bernhard Egger created
/// 2013/06/09 Bernhard Egger adapted to SnuPL/0
/// 2016/04/04 Bernhard Egger adapted to SnuPL/1
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
#include <fstream>
#include <iomanip>
#include <sstream>

#include "backend.h"
using namespace std;

//------------------------------------------------------------------------------
// CBackend
//
CBackend::CBackend(ostream &out) : _out(out) {}

CBackend::~CBackend(void) {}

bool CBackend::Emit(CModule *m) {
    assert(m != NULL);
    _m = m;

    if (!_out.good()) {
        return false;
    }

    bool res = true;

    try {
        EmitHeader();
        EmitCode();
        EmitData();
        EmitFooter();

        res = _out.good();
    } catch (...) {
        res = false;
    }

    return res;
}

void CBackend::EmitHeader(void) {}

void CBackend::EmitCode(void) {}

void CBackend::EmitData(void) {}

void CBackend::EmitFooter(void) {}

//------------------------------------------------------------------------------
// CBackendx86
//
CBackendx86::CBackendx86(ostream &out) : CBackend(out), _curr_scope(NULL) {
    _ind = string(4, ' ');
}

CBackendx86::~CBackendx86(void) {}

void CBackendx86::EmitHeader(void) {
    _out << "##################################################" << endl
         << "# " << _m->GetName() << endl
         << "#" << endl
         << endl;
}

void CBackendx86::EmitCode(void) {
    _out << _ind << "#-----------------------------------------" << endl
         << _ind << "# text section" << endl
         << _ind << "#" << endl
         << _ind << ".text" << endl
         << _ind << ".align 4" << endl
         << endl
         << _ind << "# entry point and pre-defined functions" << endl
         << _ind << ".global main" << endl
         << _ind << ".extern DIM" << endl
         << _ind << ".extern DOFS" << endl
         << _ind << ".extern ReadInt" << endl
         << _ind << ".extern WriteInt" << endl
         << _ind << ".extern WriteStr" << endl
         << _ind << ".extern WriteChar" << endl
         << _ind << ".extern WriteLn" << endl
         << endl;

    // For all subscopes, emit codes for the scope
    CModule *module = _m;
    for (auto subscopes : module->GetSubscopes()) {
        EmitScope(subscopes);
    }

    // then emit actual main function
    EmitScope(module);

    _out << _ind << "# end of text section" << endl
         << _ind << "#-----------------------------------------" << endl
         << endl;
}

void CBackendx86::EmitData(void) {
    _out << _ind << "#-----------------------------------------" << endl
         << _ind << "# global data section" << endl
         << _ind << "#" << endl
         << _ind << ".data" << endl
         << _ind << ".align 4" << endl
         << endl;

    EmitGlobalData(_m);

    _out << _ind << "# end of global data section" << endl
         << _ind << "#-----------------------------------------" << endl
         << endl;
}

void CBackendx86::EmitFooter(void) {
    _out << _ind << ".end" << endl
         << "##################################################" << endl;
}

void CBackendx86::SetScope(CScope *scope) {
    _curr_scope = scope;
}

CScope *CBackendx86::GetScope(void) const {
    return _curr_scope;
}

void CBackendx86::EmitScope(CScope *scope) {
    assert(scope != NULL);

    string label;
    if (scope->GetParent() == NULL) {
        label = "main";
    } else {
        label = scope->GetName();
    }

    // Label
    _out << _ind << "# scope " << scope->GetName() << endl
         << label << ":" << endl;

    SetScope(scope);

    // Local Size
    CSymtab *symbolTable = scope->GetSymbolTable();
    assert(symbolTable != NULL);

    auto size = ComputeStackOffsets(symbolTable, 8, -12);
    // Prologue
    _out << endl << _ind << "# prologue" << endl;
    EmitLocalData(scope);

    // Fuction Body
    _out << endl << _ind << "# function body" << endl;
    EmitCodeBlock(_curr_scope->GetCodeBlock());

    // Epilogue
    _out << endl << Label("exit") << ":" << endl << _ind << "# epilogue" << endl;
    EmitInstruction("addl", Imm(size) + ", %esp", "remove locals");
    EmitInstruction("popl", "%edi");
    EmitInstruction("popl", "%esi");
    EmitInstruction("popl", "%ebx");
    EmitInstruction("popl", "%ebp");
    EmitInstruction("ret");
    _out << endl;
}

void CBackendx86::EmitGlobalData(CScope *scope) {
    assert(scope != NULL);

    // emit the globals for the current scope
    CSymtab *st = scope->GetSymbolTable();
    assert(st != NULL);

    bool header = false;

    vector<CSymbol *> slist = st->GetSymbols();

    _out << dec;

    size_t size = 0;

    for (auto i : slist) {
        const CType *t = i->GetDataType();

        if (i->GetSymbolType() == stGlobal) {
            if (!header) {
                _out << _ind << "# scope: " << scope->GetName() << endl;
                header = true;
            }

            // insert alignment only when necessary
            if ((t->GetAlign() > 1) && (size % t->GetAlign() != 0)) {
                size += t->GetAlign() - size % t->GetAlign();
                _out << setw(4) << " "
                     << ".align " << right << setw(3) << t->GetAlign() << endl;
            }

            _out << left << setw(36) << i->GetName() + ":"
                 << "# " << t << endl;

            if (t->IsArray()) {
                auto a = dynamic_cast<const CArrayType *>(t);
                assert(a != NULL);
                int dim = a->GetNDim();

                _out << setw(4) << " "
                     << ".long " << right << setw(4) << dim << endl;

                for (int d = 0; d < dim; d++) {
                    assert(a != NULL);

                    _out << setw(4) << " "
                         << ".long " << right << setw(4) << a->GetNElem() << endl;

                    a = dynamic_cast<const CArrayType *>(a->GetInnerType());
                }
            }

            const CDataInitializer *di = i->GetData();
            if (di != NULL) {
                auto sdi = dynamic_cast<const CDataInitString *>(di);
                assert(sdi != NULL); // only support string data initializers for now

                _out << left << setw(4) << " "
                     << ".asciz " << '"' << sdi->GetData() << '"' << endl;
            } else {
                _out << left << setw(4) << " "
                     << ".skip " << dec << right << setw(4) << t->GetDataSize() << endl;
            }

            size += t->GetSize();
        }
    }

    _out << endl;

    // emit globals in subscopes (necessary if we support static local
    // variables)
    for (auto sit : scope->GetSubscopes()) {
        EmitGlobalData(sit);
    }
}

void CBackendx86::EmitLocalData(CScope *scope) {
    assert(scope != NULL);

    SetScope(scope);
    auto size = ComputeStackOffsets(_curr_scope->GetSymbolTable(), 8, -12);
    // callee saves and stack pointer setting
    EmitInstruction("pushl", "%ebp");
    EmitInstruction("movl", "%esp, %ebp");
    EmitInstruction("pushl", "%ebx", "save callee saved registers");
    EmitInstruction("pushl", "%esi");
    EmitInstruction("pushl", "%edi");
    EmitInstruction("subl", Imm(size) + ", %esp", "make room for locals");

    // set local variables to 0
    if (0 < size && size <= 16) {
        _out << endl;
        EmitInstruction("xorl", "%eax, %eax", "memset local stack area to 0");
        while (size > 0) {
            size -= 4;
            EmitInstruction("movl", "%eax, " + to_string(size) + "(%esp)");
        }
    } else if (size > 16) {
        _out << endl;
        EmitInstruction("cld", "", "memset local stack area to 0");
        EmitInstruction("xorl", "%eax, %eax");
        EmitInstruction("movl", Imm(size / 4) + ", %ecx");
        EmitInstruction("mov", "%esp, %edi");
        EmitInstruction("rep", "stosl");
    }

    // initialize array locals
    vector<CSymbol *> slist = scope->GetSymbolTable()->GetSymbols();
    auto it = slist.begin();

    while (it != slist.end()) {
        if ((*it)->GetSymbolType() == stLocal && (*it)->GetDataType()->IsArray()) {
            const CArrayType *typ =
                dynamic_cast<const CArrayType *>((*it)->GetDataType());
            int ndim = typ->GetNDim();
            int offset = (*it)->GetOffset();

            EmitInstruction("movl", Imm(ndim) + ", " + to_string(size) + "(%ebp)",
                            "local array '" + (*it)->GetName() +
                            "': " + to_string(ndim) + " dimensions");

            for (int i = 0; typ != NULL;) {
                size += 4;
                i++;
                EmitInstruction(
                    "movl", Imm(typ->GetNElem()) + ", " + to_string(size) + "(%ebp)",
                    "  dimension " + to_string(i) + ": " + to_string(typ->GetNElem()) +
                    " elements");
                typ = dynamic_cast<const CArrayType *>(typ->GetInnerType());
            }
        }
        it++;
    }
}

void CBackendx86::EmitCodeBlock(CCodeBlock *cb) {
    assert(cb != NULL);

    const list<CTacInstr *> &instr = cb->GetInstr();
    list<CTacInstr *>::const_iterator it = instr.begin();

    while (it != instr.end())
        EmitInstruction(*it++);
}

void CBackendx86::EmitInstruction(CTacInstr *i) {
    assert(i != NULL);

    ostringstream cmt;
    string mnm;
    cmt << i;
    ostringstream inst;

    EOperation op = i->GetOperation();

    switch (op) {
    case opAdd:
    case opSub:
    case opMul:
    case opDiv:
    case opAnd:
    case opOr:
        // binary operators
        // dst = src1 op src2

        Load(i->GetSrc(1), "%eax", cmt.str());
        Load(i->GetSrc(2), "%ebx");
        if (op == opDiv) {
            EmitInstruction("cdq");
        }
        if (op == opMul || op == opDiv) {
            inst << "i" << op << "l";
            EmitInstruction(inst.str(), "%ebx");
        } else {
            inst << op << "l";
            EmitInstruction(inst.str(), "%ebx, %eax");
        }
        Store(i->GetDest(), 'a');
        break;
    case opNeg:
    case opPos:
    case opNot:
        Load(i->GetSrc(1), "%eax", cmt.str());
        if (op == opNeg) {
            inst << op << "l";
            EmitInstruction(inst.str(), "%eax");
        } else if (op == opNot) {
            EmitInstruction("xorl", Imm(1) + ", %eax");
        }
        Store(i->GetDest(), 'a');
        break;
    case opAssign: {
        auto src1 = i->GetSrc(1);
        auto name = dynamic_cast<CTacName *>(src1);
        if (name != NULL) {
            auto type = name->GetSymbol()->GetDataType();
            auto dest = static_cast<CTacName *>(i->GetDest());
            if (type->IsPointer() || type->IsArray()) {
                EmitInstruction("cld", "", "array assign");

                if (name->GetSymbol()->GetDataType()->IsArray()) {
                    EmitInstruction("leal", Operand(name) + ", %esi", "ptr of src array");
                } else {
                    Load(name, "%esi", "ptr of src array");
                }
                if (dest->GetSymbol()->GetDataType()->IsArray()) {
                    EmitInstruction("leal", Operand(dest) + ", %edi", "ptr of dst array");
                } else {
                    Load(dest, "%edi", "ptr of dst array");
                }

                auto type = name->GetSymbol()->GetDataType();
                if (type->IsPointer()) {
                    type = static_cast<const CPointerType *>(type)->GetBaseType();
                }

                int csize = type->GetSize();
                EmitInstruction("movl", "$" + to_string(csize) + ", %ecx",
                                "size of array = " + to_string(csize));
                EmitInstruction("rep", "movsb");
                break;
            }
        }
        Load(i->GetSrc(1), "%eax", cmt.str());
        Store(i->GetDest(), 'a');
        break;
    }
    case opAddress: {
        // pointer operations
        // dst = &src1
        EmitInstruction("leal", Operand(i->GetSrc(1)) + ", %eax", cmt.str());
        Store(i->GetDest(), 'a');
        break;
    }
    case opDeref: {
        // dst = *src1
        // opDeref not generated for now
        EmitInstruction("# opDeref", "not implemented", cmt.str());
        break;
    }
    case opGoto: {
        // unconditional branching
        // goto dst
        auto tgt = static_cast<const CTacLabel *>(i->GetDest());
        EmitInstruction("jmp", Label(tgt), cmt.str());
        break;
    }
    case opEqual:
    case opNotEqual:
    case opLessThan:
    case opLessEqual:
    case opBiggerThan:
    case opBiggerEqual: {
        // conditional branching
        // if src1 relOp src2 then goto dst
        Load(i->GetSrc(1), "%eax", cmt.str());
        Load(i->GetSrc(2), "%ebx");
        EmitInstruction("cmpl", "%ebx, %eax");
        inst << "j" << Condition(op);
        auto tgt = dynamic_cast<const CTacLabel *>(i->GetDest());
        EmitInstruction(inst.str(), Label(tgt));
        break;
    }

    // function call-related operations
    case opCall: {
        auto func = static_cast<const CTacName *>(i->GetSrc(1));
        auto proc = static_cast<const CSymProc *>(func->GetSymbol());
        EmitInstruction("call", proc->GetName(), cmt.str());
        if (i->GetDest() != NULL) {
            Store(i->GetDest(), 'a');
        }
        EmitInstruction("addl", Imm(proc->GetNParams() * 4) + ", %esp");
        break;
    }

    case opReturn:
        if (i->GetSrc(1)) {
            Load(i->GetSrc(1), "%eax", cmt.str());
            EmitInstruction("jmp", Label("exit"));
        } else {
            EmitInstruction("jmp", Label("exit"), cmt.str());
        }
        break;
    case opParam:
        Load(i->GetSrc(1), "%eax", cmt.str());
        EmitInstruction("pushl", "%eax");
        break;

    // special
    case opLabel:
        _out << Label(dynamic_cast<CTacLabel *>(i)) << ":" << endl;
        break;

    case opNop:
        EmitInstruction("nop", "", cmt.str());
        break;

    default:
        EmitInstruction("# ???", "not implemented", cmt.str());
    }
}

void CBackendx86::EmitInstruction(string mnemonic, string args,
                                  string comment) {
    _out << left << _ind << setw(7) << mnemonic << " " << setw(23) << args;
    if (comment != "")
        _out << " # " << comment;
    _out << endl;
}

void CBackendx86::Load(CTacAddr *src, string dst, string comment) {
    assert(src != NULL);

    string mnm = "mov";
    string mod = "l";

    // set operator modifier based on the operand size
    switch (OperandSize(src)) {
    case 1:
        mod = "zbl";
        break;
    case 2:
        mod = "zwl";
        break;
    case 4:
        mod = "l";
        break;
    }

    // emit the load instruction
    EmitInstruction(mnm + mod, Operand(src) + ", " + dst, comment);
}

void CBackendx86::Store(CTac *dst, char src_base, string comment) {
    assert(dst != NULL);

    string mnm = "mov";
    string mod = "l";
    string src = "%";

    // compose the source register name based on the operand size
    switch (OperandSize(dst)) {
    case 1:
        mod = "b";
        src += string(1, src_base) + "l";
        break;
    case 2:
        mod = "w";
        src += string(1, src_base) + "x";
        break;
    case 4:
        mod = "l";
        src += "e" + string(1, src_base) + "x";
        break;
    }

    // emit the store instruction
    EmitInstruction(mnm + mod, src + ", " + Operand(dst), comment);
}

// return a string representing op
string CBackendx86::Operand(const CTac *op) {
    string operand;

    auto constant = dynamic_cast<const CTacConst *>(op);
    if (constant != NULL) {
        return Imm(constant->GetValue());
    }

    auto reference = dynamic_cast<const CTacReference *>(op);
    if (reference != NULL) {
        const CSymbol *symbol = reference->GetSymbol();
        EmitInstruction("movl", to_string(symbol->GetOffset()) + "(" +
                        symbol->GetBaseRegister() + "), %edi");
        return "(%edi)";
    }

    auto name = dynamic_cast<const CTacName *>(op);
    if (name != NULL) {
        const CSymbol *symbol = name->GetSymbol();
        switch (symbol->GetSymbolType()) {
        case stGlobal:
        case stProcedure:
            return symbol->GetName();
        case stLocal:
        case stParam:
            return to_string(symbol->GetOffset()) + "(" + symbol->GetBaseRegister() +
                   ")";
        default:
            break;
        }
    }
    return "";

    return operand;
}

string CBackendx86::Imm(int value) const {
    ostringstream o;
    o << "$" << dec << value;
    return o.str();
}

string CBackendx86::Label(const CTacLabel *label) const {
    CScope *cs = GetScope();
    assert(cs != NULL);

    ostringstream o;
    o << "l_" << cs->GetName() << "_" << label->GetLabel();
    return o.str();
    return "l_" + cs->GetName() + "_" + label->GetLabel();
}

string CBackendx86::Label(string label) const {
    CScope *cs = GetScope();
    assert(cs != NULL);

    return "l_" + cs->GetName() + "_" + label;
}

string CBackendx86::Condition(EOperation cond) const {
    switch (cond) {
    case opEqual:
        return "e";
    case opNotEqual:
        return "ne";
    case opLessThan:
        return "l";
    case opLessEqual:
        return "le";
    case opBiggerThan:
        return "g";
    case opBiggerEqual:
        return "ge";
    default:
        assert(false);
        break;
    }
}

// compute the size for operand t of type CTacName
int CBackendx86::OperandSize(CTac *t) const {
    int size = 4;

    const CType *type = NULL;
    if (dynamic_cast<CTacName *>(t) != NULL) {
        if (dynamic_cast<CTacReference *>(t) != NULL) {
            // if it is CTacReference, unwrap the pointer in case the type is pointer
            const CSymbol *deref_symbol =
                dynamic_cast<CTacReference *>(t)->GetDerefSymbol();
            if (deref_symbol->GetDataType()->IsPointer()) {
                auto pointer_type =
                    dynamic_cast<const CPointerType *>(deref_symbol->GetDataType());
                type = dynamic_cast<const CArrayType *>(pointer_type->GetBaseType());
            } else {
                type = dynamic_cast<const CArrayType *>(deref_symbol->GetDataType());
            }
            assert(type != NULL);
            // also if it is array, get the inner type until it is not array
            while (type->IsArray()) {
                type = dynamic_cast<const CArrayType *>(type)->GetInnerType();
            }
        } else {
            // if is is CTacName but not reference, get the symbol's data type
            const CSymbol *symbol = dynamic_cast<CTacName *>(t)->GetSymbol();
            type = symbol->GetDataType();
        }
    }
    CTypeManager *tm = CTypeManager::Get();
    // if it is boolean or character, return 1
    // else, return 4
    if (type != NULL &&
            (type->Match(tm->GetBool()) || type->Match(tm->GetChar()))) {
        size = 1;
    } else {
        size = 4;
    }

    return size;
}

size_t CBackendx86::ComputeStackOffsets(CSymtab *symtab, int param_ofs,
                                        int local_ofs) {
    assert(symtab != NULL);
    vector<CSymbol *> slist = symtab->GetSymbols();

    for (auto sym : slist) {
        int size = sym->GetDataType()->GetSize();
        int align = sym->GetDataType()->GetAlign();
        sym->SetBaseRegister("%ebp");

        if (sym->GetSymbolType() == stLocal) {
            local_ofs -= size;
            if (local_ofs % align != 0) {
                local_ofs -= 4 + local_ofs % align;
            }

            sym->SetOffset(local_ofs);
            _out << _ind << "#" << right << setw(7) << local_ofs << "(%ebp)"
                 << setw(4) << size << setw(3) << "[ " << left << setw(10)
                 << "$" + sym->GetName() << sym->GetDataType() << " %ebp" << local_ofs
                 << " ]" << endl;
        } else if (sym->GetSymbolType() == stParam) {
            auto param = dynamic_cast<CSymParam *>(sym);
            sym->SetOffset(param_ofs + 4 * param->GetIndex());

            _out << _ind << "#" << right << setw(7) << sym->GetOffset() << "(%ebp)"
                 << setw(4) << size << setw(3) << "[ " << left << setw(10)
                 << "%" + sym->GetName() << sym->GetDataType() << " %ebp+"
                 << sym->GetOffset() << " ]" << endl;
        }
    }
    // Globals and Procedures are ignored
    if (0 != local_ofs % 4) {
        local_ofs -= 4 + local_ofs % 4;
    }
    return -12 - local_ofs;
}
