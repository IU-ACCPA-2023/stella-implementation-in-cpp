#pragma once
#ifndef STELLA_VISITTYPECHECK_HEADER
#define STELLA_VISITTYPECHECK_HEADER

#include "Stella/Absyn.H"
#include "Stella/Printer.H"
#include <string>
#include <unordered_map>

namespace Stella
{
  class type_error : public std::runtime_error
  {
  protected:
    Stella::PrintAbsyn printer = Stella::PrintAbsyn();
    std::string message;

  public:
    type_error() : std::runtime_error("Type Error"){};
    const char *what() const noexcept override
    {
      return message.c_str();
    }
  };

  class undefined_variable_error : public type_error
  {
  public:
    undefined_variable_error(Var *var) : type_error()
    {
      message = std::string("undefined variable ") + printer.print(var);
    }
  };

  class type_mismatch_error : public type_error
  {
  public:
    type_mismatch_error(Type *expected_type, Type *actual_type, Expr *expr) : type_error()
    {
      message = std::string("expected type\n") + "  " + printer.print(expected_type) + "\n" + "but got\n" + "  " + printer.print(actual_type) + "\n" + "in expression\n" + "  " + printer.print(expr);
    }
  };

  class type_unexpected_anonymous_function : public type_error
  {
  public:
    type_unexpected_anonymous_function(Type *expected_type, Expr *expr) : type_error()
    {
      message = std::string("expected non-function type\n") + "  " + printer.print(expected_type) + "\n" + "but got an anonymous function\nin expression\n" + "  " + printer.print(expr);
    }
  };

  class expected_function_type_error : public type_error
  {
  public:
    expected_function_type_error(Type *actual_type, Expr *expr) : type_error()
    {
      message = std::string("expected a function type\nbut got\n  ") + printer.print(actual_type) + "\nin expression\n" + "  " + printer.print(expr);
    }
  };

  class VisitTypeCheck : public Visitor
  {
  private:
    Stella::PrintAbsyn printer = Stella::PrintAbsyn();
    std::unordered_map<StellaIdent, Type *> context = {}; // mapping from identifiers (variables and function names) to their types
    Type *expected_type = nullptr;                        // expected type for the current node (nullptr when no expected type is known or makes sense)
    Type *actual_type = nullptr;                          // actual type for the last visited node

    void set_actual_type(Expr *expr, Type *type_);
    std::unordered_map<StellaIdent, Type *> enter_scope(ListParamDecl *paramDecls);
    void exit_scope(std::unordered_map<StellaIdent, Type *> old_context);
    Type *typecheck_subexpr(Expr *expr, Type *type);

  public:
    void visitProgram(Program *p);
    void visitLanguageDecl(LanguageDecl *p);
    void visitExtension(Extension *p);
    void visitDecl(Decl *p);
    void visitLocalDecl(LocalDecl *p);
    void visitAnnotation(Annotation *p);
    void visitParamDecl(ParamDecl *p);
    void visitReturnType(ReturnType *p);
    void visitThrowType(ThrowType *p);
    void visitExpr(Expr *p);
    void visitMatchCase(MatchCase *p);
    void visitOptionalTyping(OptionalTyping *p);
    void visitPatternData(PatternData *p);
    void visitExprData(ExprData *p);
    void visitPattern(Pattern *p);
    void visitLabelledPattern(LabelledPattern *p);
    void visitBinding(Binding *p);
    void visitType(Type *p);
    void visitVariantFieldType(VariantFieldType *p);
    void visitRecordFieldType(RecordFieldType *p);
    void visitTyping(Typing *p);
    void visitAProgram(AProgram *p);
    void visitLanguageCore(LanguageCore *p);
    void visitAnExtension(AnExtension *p);
    void visitDeclFun(DeclFun *p);
    void visitDeclTypeAlias(DeclTypeAlias *p);
    void visitALocalDecl(ALocalDecl *p);
    void visitInlineAnnotation(InlineAnnotation *p);
    void visitAParamDecl(AParamDecl *p);
    void visitNoReturnType(NoReturnType *p);
    void visitSomeReturnType(SomeReturnType *p);
    void visitNoThrowType(NoThrowType *p);
    void visitSomeThrowType(SomeThrowType *p);
    void visitIf(If *p);
    void visitLet(Let *p);
    void visitAMatchCase(AMatchCase *p);
    void visitNoTyping(NoTyping *p);
    void visitSomeTyping(SomeTyping *p);
    void visitNoPatternData(NoPatternData *p);
    void visitSomePatternData(SomePatternData *p);
    void visitNoExprData(NoExprData *p);
    void visitSomeExprData(SomeExprData *p);
    void visitPatternVariant(PatternVariant *p);
    void visitPatternTuple(PatternTuple *p);
    void visitPatternRecord(PatternRecord *p);
    void visitPatternList(PatternList *p);
    void visitPatternCons(PatternCons *p);
    void visitPatternFalse(PatternFalse *p);
    void visitPatternTrue(PatternTrue *p);
    void visitPatternInt(PatternInt *p);
    void visitPatternSucc(PatternSucc *p);
    void visitPatternVar(PatternVar *p);
    void visitALabelledPattern(ALabelledPattern *p);
    void visitABinding(ABinding *p);
    void visitLessThan(LessThan *p);
    void visitLessThanOrEqual(LessThanOrEqual *p);
    void visitGreaterThan(GreaterThan *p);
    void visitGreaterThanOrEqual(GreaterThanOrEqual *p);
    void visitEqual(Equal *p);
    void visitNotEqual(NotEqual *p);
    void visitTypeAsc(TypeAsc *p);
    void visitAbstraction(Abstraction *p);
    void visitTuple(Tuple *p);
    void visitRecord(Record *p);
    void visitVariant(Variant *p);
    void visitMatch(Match *p);
    void visitList(List *p);
    void visitAdd(Add *p);
    void visitLogicOr(LogicOr *p);
    void visitMultiply(Multiply *p);
    void visitLogicAnd(LogicAnd *p);
    void visitApplication(Application *p);
    void visitConsList(ConsList *p);
    void visitHead(Head *p);
    void visitIsEmpty(IsEmpty *p);
    void visitTail(Tail *p);
    void visitSucc(Succ *p);
    void visitLogicNot(LogicNot *p);
    void visitPred(Pred *p);
    void visitIsZero(IsZero *p);
    void visitFix(Fix *p);
    void visitNatRec(NatRec *p);
    void visitFold(Fold *p);
    void visitUnfold(Unfold *p);
    void visitDotRecord(DotRecord *p);
    void visitDotTuple(DotTuple *p);
    void visitConstTrue(ConstTrue *p);
    void visitConstFalse(ConstFalse *p);
    void visitConstInt(ConstInt *p);
    void visitVar(Var *p);
    void visitTypeFun(TypeFun *p);
    void visitTypeRec(TypeRec *p);
    void visitTypeSum(TypeSum *p);
    void visitTypeTuple(TypeTuple *p);
    void visitTypeRecord(TypeRecord *p);
    void visitTypeVariant(TypeVariant *p);
    void visitTypeList(TypeList *p);
    void visitTypeBool(TypeBool *p);
    void visitTypeNat(TypeNat *p);
    void visitTypeUnit(TypeUnit *p);
    void visitTypeVar(TypeVar *p);
    void visitAVariantFieldType(AVariantFieldType *p);
    void visitARecordFieldType(ARecordFieldType *p);
    void visitATyping(ATyping *p);
    void visitListStellaIdent(ListStellaIdent *p);
    void visitListExtensionName(ListExtensionName *p);
    void visitListExtension(ListExtension *p);
    void visitListDecl(ListDecl *p);
    void visitListLocalDecl(ListLocalDecl *p);
    void visitListAnnotation(ListAnnotation *p);
    void visitListParamDecl(ListParamDecl *p);
    void visitListExpr(ListExpr *p);
    void visitListMatchCase(ListMatchCase *p);
    void visitListPattern(ListPattern *p);
    void visitListLabelledPattern(ListLabelledPattern *p);
    void visitListBinding(ListBinding *p);
    void visitListType(ListType *p);
    void visitListVariantFieldType(ListVariantFieldType *p);
    void visitListRecordFieldType(ListRecordFieldType *p);

    void visitInteger(Integer x);
    void visitChar(Char x);
    void visitDouble(Double x);
    void visitString(String x);
    void visitIdent(Ident x);
    void visitStellaIdent(StellaIdent x);
    void visitExtensionName(ExtensionName x);
  };
}

#endif
