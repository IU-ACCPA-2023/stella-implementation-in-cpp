#include "VisitTypeCheck.h"
#include <iostream>

namespace Stella
{
  void VisitTypeCheck::visitProgram(Program *t) {}                   // abstract class
  void VisitTypeCheck::visitLanguageDecl(LanguageDecl *t) {}         // abstract class
  void VisitTypeCheck::visitExtension(Extension *t) {}               // abstract class
  void VisitTypeCheck::visitDecl(Decl *t) {}                         // abstract class
  void VisitTypeCheck::visitLocalDecl(LocalDecl *t) {}               // abstract class
  void VisitTypeCheck::visitAnnotation(Annotation *t) {}             // abstract class
  void VisitTypeCheck::visitParamDecl(ParamDecl *t) {}               // abstract class
  void VisitTypeCheck::visitReturnType(ReturnType *t) {}             // abstract class
  void VisitTypeCheck::visitThrowType(ThrowType *t) {}               // abstract class
  void VisitTypeCheck::visitExpr(Expr *t) {}                         // abstract class
  void VisitTypeCheck::visitMatchCase(MatchCase *t) {}               // abstract class
  void VisitTypeCheck::visitOptionalTyping(OptionalTyping *t) {}     // abstract class
  void VisitTypeCheck::visitPatternData(PatternData *t) {}           // abstract class
  void VisitTypeCheck::visitExprData(ExprData *t) {}                 // abstract class
  void VisitTypeCheck::visitPattern(Pattern *t) {}                   // abstract class
  void VisitTypeCheck::visitLabelledPattern(LabelledPattern *t) {}   // abstract class
  void VisitTypeCheck::visitBinding(Binding *t) {}                   // abstract class
  void VisitTypeCheck::visitType(Type *t) {}                         // abstract class
  void VisitTypeCheck::visitVariantFieldType(VariantFieldType *t) {} // abstract class
  void VisitTypeCheck::visitRecordFieldType(RecordFieldType *t) {}   // abstract class
  void VisitTypeCheck::visitTyping(Typing *t) {}                     // abstract class

  void VisitTypeCheck::set_actual_type(Expr *expr, Type *type_)
  {
    if (expected_type)
    {
      if (std::string(printer.print(type_)) != printer.print(expected_type))
      {
        throw type_mismatch_error(expected_type, type_, expr);
      }
    }
    actual_type = type_;
  }

  std::unordered_map<StellaIdent, Type *> VisitTypeCheck::enter_scope(ListParamDecl *paramDecls)
  {
    auto old_context = std::unordered_map<StellaIdent, Type *>(context);

    for (auto paramDecl : *paramDecls)
    {
      AParamDecl *param = dynamic_cast<AParamDecl *>(paramDecl);
      context.insert({param->stellaident_, param->type_});
    }

    return old_context;
  }

  void VisitTypeCheck::exit_scope(std::unordered_map<StellaIdent, Type *> old_context)
  {
    context = old_context;
  }

  Type *VisitTypeCheck::typecheck_subexpr(Expr *expr, Type *type_)
  {
    std::cout << "Typechecking subexpression " << printer.print(expr) << "\n";
    if (type_)
      std::cout << "  against type " << printer.print(type_) << "\n";
    auto old_expected_type = expected_type;
    expected_type = type_;
    std::cout << "Going in\n";
    expr->accept(this);
    std::cout << "Going out\n";
    expected_type = old_expected_type;
    std::cout << "Done!\n";
    return actual_type;
  }

  void VisitTypeCheck::visitAProgram(AProgram *a_program)
  {
    /* Code For AProgram Goes Here */

    if (a_program->languagedecl_)
      a_program->languagedecl_->accept(this);
    if (a_program->listextension_)
      a_program->listextension_->accept(this);
    if (a_program->listdecl_)
      a_program->listdecl_->accept(this);
  }

  void VisitTypeCheck::visitLanguageCore(LanguageCore *language_core)
  {
    /* Code For LanguageCore Goes Here */
  }

  void VisitTypeCheck::visitAnExtension(AnExtension *an_extension)
  {
    /* Code For AnExtension Goes Here */

    if (an_extension->listextensionname_)
      an_extension->listextensionname_->accept(this);
  }

  void VisitTypeCheck::visitDeclFun(DeclFun *decl_fun)
  {
    /* Code For DeclFun Goes Here */
    StellaIdent name = decl_fun->stellaident_;

    // extract return type
    Type *returnType = dynamic_cast<SomeReturnType *>(decl_fun->returntype_)->type_;
    Expr *returnExpr = decl_fun->expr_;

    if (decl_fun->listannotation_)
      decl_fun->listannotation_->accept(this);
    visitStellaIdent(decl_fun->stellaident_);
    if (decl_fun->listparamdecl_)
      decl_fun->listparamdecl_->accept(this);
    if (decl_fun->returntype_)
      decl_fun->returntype_->accept(this);
    if (decl_fun->throwtype_)
      decl_fun->throwtype_->accept(this);
    if (decl_fun->listdecl_)
      decl_fun->listdecl_->accept(this);

    // save current context before going inside the body
    auto old_context = enter_scope(decl_fun->listparamdecl_);
    // go into the body of the function
    typecheck_subexpr(decl_fun->expr_, returnType);
    // restore context
    exit_scope(old_context);

    if (auto param_type = dynamic_cast<AParamDecl *>((*decl_fun->listparamdecl_)[0])->type_)
    {
      ListType *param_types = new ListType();
      param_types->push_back(dynamic_cast<AParamDecl *>((*decl_fun->listparamdecl_)[0])->type_);
      context.insert({decl_fun->stellaident_, new TypeFun(param_types, returnType->clone())});
    }
  }

  void VisitTypeCheck::visitDeclTypeAlias(DeclTypeAlias *decl_type_alias)
  {
    /* Code For DeclTypeAlias Goes Here */

    visitStellaIdent(decl_type_alias->stellaident_);
    if (decl_type_alias->type_)
      decl_type_alias->type_->accept(this);
  }

  void VisitTypeCheck::visitALocalDecl(ALocalDecl *a_local_decl)
  {
    /* Code For ALocalDecl Goes Here */

    if (a_local_decl->decl_)
      a_local_decl->decl_->accept(this);
  }

  void VisitTypeCheck::visitInlineAnnotation(InlineAnnotation *inline_annotation)
  {
    /* Code For InlineAnnotation Goes Here */
  }

  void VisitTypeCheck::visitAParamDecl(AParamDecl *a_param_decl)
  {
    /* Code For AParamDecl Goes Here */

    visitStellaIdent(a_param_decl->stellaident_);
    if (a_param_decl->type_)
      a_param_decl->type_->accept(this);
  }

  void VisitTypeCheck::visitNoReturnType(NoReturnType *no_return_type)
  {
    /* Code For NoReturnType Goes Here */
  }

  void VisitTypeCheck::visitSomeReturnType(SomeReturnType *some_return_type)
  {
    /* Code For SomeReturnType Goes Here */

    if (some_return_type->type_)
      some_return_type->type_->accept(this);
  }

  void VisitTypeCheck::visitNoThrowType(NoThrowType *no_throw_type)
  {
    /* Code For NoThrowType Goes Here */
  }

  void VisitTypeCheck::visitSomeThrowType(SomeThrowType *some_throw_type)
  {
    /* Code For SomeThrowType Goes Here */

    if (some_throw_type->listtype_)
      some_throw_type->listtype_->accept(this);
  }

  void VisitTypeCheck::visitIf(If *if_)
  {
    /* Code For If Goes Here */

    typecheck_subexpr(if_->expr_1, new TypeBool());
    auto then_type = typecheck_subexpr(if_->expr_2, expected_type);
    typecheck_subexpr(if_->expr_3, then_type);
    set_actual_type(if_, then_type);
  }

  void VisitTypeCheck::visitLet(Let *let)
  {
    /* Code For Let Goes Here */

    visitStellaIdent(let->stellaident_);
    if (let->expr_1)
      let->expr_1->accept(this);
    if (let->expr_2)
      let->expr_2->accept(this);
  }

  void VisitTypeCheck::visitLessThan(LessThan *less_than)
  {
    /* Code For LessThan Goes Here */

    if (less_than->expr_1)
      less_than->expr_1->accept(this);
    if (less_than->expr_2)
      less_than->expr_2->accept(this);
  }

  void VisitTypeCheck::visitLessThanOrEqual(LessThanOrEqual *less_than_or_equal)
  {
    /* Code For LessThanOrEqual Goes Here */

    if (less_than_or_equal->expr_1)
      less_than_or_equal->expr_1->accept(this);
    if (less_than_or_equal->expr_2)
      less_than_or_equal->expr_2->accept(this);
  }

  void VisitTypeCheck::visitGreaterThan(GreaterThan *greater_than)
  {
    /* Code For GreaterThan Goes Here */

    if (greater_than->expr_1)
      greater_than->expr_1->accept(this);
    if (greater_than->expr_2)
      greater_than->expr_2->accept(this);
  }

  void VisitTypeCheck::visitGreaterThanOrEqual(GreaterThanOrEqual *greater_than_or_equal)
  {
    /* Code For GreaterThanOrEqual Goes Here */

    if (greater_than_or_equal->expr_1)
      greater_than_or_equal->expr_1->accept(this);
    if (greater_than_or_equal->expr_2)
      greater_than_or_equal->expr_2->accept(this);
  }

  void VisitTypeCheck::visitEqual(Equal *equal)
  {
    /* Code For Equal Goes Here */

    if (equal->expr_1)
      equal->expr_1->accept(this);
    if (equal->expr_2)
      equal->expr_2->accept(this);
  }

  void VisitTypeCheck::visitNotEqual(NotEqual *not_equal)
  {
    /* Code For NotEqual Goes Here */

    if (not_equal->expr_1)
      not_equal->expr_1->accept(this);
    if (not_equal->expr_2)
      not_equal->expr_2->accept(this);
  }

  void VisitTypeCheck::visitTypeAsc(TypeAsc *type_asc)
  {
    /* Code For TypeAsc Goes Here */

    if (type_asc->expr_)
      type_asc->expr_->accept(this);
    if (type_asc->type_)
      type_asc->type_->accept(this);
  }

  void VisitTypeCheck::visitAbstraction(Abstraction *abstraction)
  {
    /* Code For Abstraction Goes Here */
    if (!expected_type) /* if expected type of the abstraction is not known */
    {
      Type *return_type;
      Type *param_type = dynamic_cast<AParamDecl *>((*abstraction->listparamdecl_)[0])->type_;

      if (abstraction->listparamdecl_)
        abstraction->listparamdecl_->accept(this);

      auto old_context = enter_scope(abstraction->listparamdecl_);
      return_type = typecheck_subexpr(abstraction->expr_, nullptr);
      // abstraction->expr_->accept(this);
      exit_scope(old_context);

      ListType *arg = new ListType;
      arg->push_back(param_type);
      set_actual_type(abstraction, new TypeFun(arg, return_type));
    }
    else if (auto expected_type_fun = dynamic_cast<TypeFun *>(expected_type))
    {
      Type *expected_param_type = (*expected_type_fun->listtype_)[0];
      Type *expected_return_type = expected_type_fun->type_;

      if (abstraction->listparamdecl_)
        abstraction->listparamdecl_->accept(this);

      auto old_context = enter_scope(abstraction->listparamdecl_);
      typecheck_subexpr(new Var(dynamic_cast<AParamDecl *>((*abstraction->listparamdecl_)[0])->stellaident_), expected_param_type);
      typecheck_subexpr(abstraction->expr_, expected_return_type);
      // abstraction->expr_->accept(this);
      exit_scope(old_context);
      set_actual_type(abstraction, expected_type);
    }
    else
    {
      throw type_unexpected_anonymous_function(expected_type, abstraction);
    }
  }

  void VisitTypeCheck::visitTuple(Tuple *tuple)
  {
    /* Code For Tuple Goes Here */

    if (tuple->listexpr_)
      tuple->listexpr_->accept(this);
  }

  void VisitTypeCheck::visitRecord(Record *record)
  {
    /* Code For Record Goes Here */

    if (record->listbinding_)
      record->listbinding_->accept(this);
  }

  void VisitTypeCheck::visitVariant(Variant *variant)
  {
    /* Code For Variant Goes Here */

    visitStellaIdent(variant->stellaident_);
    if (variant->exprdata_)
      variant->exprdata_->accept(this);
  }

  void VisitTypeCheck::visitMatch(Match *match)
  {
    /* Code For Match Goes Here */

    if (match->expr_)
      match->expr_->accept(this);
    if (match->listmatchcase_)
      match->listmatchcase_->accept(this);
  }

  void VisitTypeCheck::visitList(List *list)
  {
    /* Code For List Goes Here */

    if (list->listexpr_)
      list->listexpr_->accept(this);
  }

  void VisitTypeCheck::visitAdd(Add *add)
  {
    /* Code For Add Goes Here */

    if (add->expr_1)
      add->expr_1->accept(this);
    if (add->expr_2)
      add->expr_2->accept(this);
  }

  void VisitTypeCheck::visitLogicOr(LogicOr *logic_or)
  {
    /* Code For LogicOr Goes Here */

    if (logic_or->expr_1)
      logic_or->expr_1->accept(this);
    if (logic_or->expr_2)
      logic_or->expr_2->accept(this);
  }

  void VisitTypeCheck::visitMultiply(Multiply *multiply)
  {
    /* Code For Multiply Goes Here */

    if (multiply->expr_1)
      multiply->expr_1->accept(this);
    if (multiply->expr_2)
      multiply->expr_2->accept(this);
  }

  void VisitTypeCheck::visitLogicAnd(LogicAnd *logic_and)
  {
    /* Code For LogicAnd Goes Here */

    if (logic_and->expr_1)
      logic_and->expr_1->accept(this);
    if (logic_and->expr_2)
      logic_and->expr_2->accept(this);
  }

  void VisitTypeCheck::visitApplication(Application *application)
  {
    /* Code For Application Goes Here */

    std::cout << "Visiting application: " << printer.print(application) << "\n";
    auto type_of_fun = typecheck_subexpr(application->expr_, nullptr);
    std::cout << "Computed type of function: " << printer.print(type_of_fun) << "\n";
    if (auto ft = dynamic_cast<TypeFun *>(type_of_fun))
    {
      auto expected_arg_type = (*ft->listtype_)[0];
      typecheck_subexpr((*application->listexpr_)[0], expected_arg_type);
      set_actual_type(application, ft->type_);
    }
    else
    {
      throw expected_function_type_error(type_of_fun, application->expr_);
    }
  }

  void VisitTypeCheck::visitConsList(ConsList *cons_list)
  {
    /* Code For ConsList Goes Here */

    if (cons_list->expr_1)
      cons_list->expr_1->accept(this);
    if (cons_list->expr_2)
      cons_list->expr_2->accept(this);
  }

  void VisitTypeCheck::visitHead(Head *head)
  {
    /* Code For Head Goes Here */

    if (head->expr_)
      head->expr_->accept(this);
  }

  void VisitTypeCheck::visitIsEmpty(IsEmpty *is_empty)
  {
    /* Code For IsEmpty Goes Here */

    if (is_empty->expr_)
      is_empty->expr_->accept(this);
  }

  void VisitTypeCheck::visitTail(Tail *tail)
  {
    /* Code For Tail Goes Here */

    if (tail->expr_)
      tail->expr_->accept(this);
  }

  void VisitTypeCheck::visitSucc(Succ *succ)
  {
    /* Code For Succ Goes Here */
    typecheck_subexpr(succ->expr_, new TypeNat());
    set_actual_type(succ, new TypeNat());
  }

  void VisitTypeCheck::visitLogicNot(LogicNot *logic_not)
  {
    /* Code For LogicNot Goes Here */

    if (logic_not->expr_)
      logic_not->expr_->accept(this);
  }

  void VisitTypeCheck::visitPred(Pred *pred)
  {
    /* Code For Pred Goes Here */

    if (pred->expr_)
      pred->expr_->accept(this);
  }

  void VisitTypeCheck::visitIsZero(IsZero *is_zero)
  {
    /* Code For IsZero Goes Here */

    if (is_zero->expr_)
      is_zero->expr_->accept(this);
  }

  void VisitTypeCheck::visitFix(Fix *fix)
  {
    /* Code For Fix Goes Here */

    if (fix->expr_)
      fix->expr_->accept(this);
  }

  void VisitTypeCheck::visitNatRec(NatRec *nat_rec)
  {
    /* Code For NatRec Goes Here */

    typecheck_subexpr(nat_rec->expr_1, new TypeNat());
    auto return_type = typecheck_subexpr(nat_rec->expr_2, expected_type);

    ListType *arg1 = new ListType();
    arg1->push_back(new TypeNat());
    ListType *arg2 = new ListType();
    arg2->push_back(return_type);
    typecheck_subexpr(nat_rec->expr_3, new TypeFun(arg1, new TypeFun(arg2, return_type->clone())));
  }

  void VisitTypeCheck::visitFold(Fold *fold)
  {
    /* Code For Fold Goes Here */

    if (fold->type_)
      fold->type_->accept(this);
    if (fold->expr_)
      fold->expr_->accept(this);
  }

  void VisitTypeCheck::visitUnfold(Unfold *unfold)
  {
    /* Code For Unfold Goes Here */

    if (unfold->type_)
      unfold->type_->accept(this);
    if (unfold->expr_)
      unfold->expr_->accept(this);
  }

  void VisitTypeCheck::visitDotRecord(DotRecord *dot_record)
  {
    /* Code For DotRecord Goes Here */

    if (dot_record->expr_)
      dot_record->expr_->accept(this);
    visitStellaIdent(dot_record->stellaident_);
  }

  void VisitTypeCheck::visitDotTuple(DotTuple *dot_tuple)
  {
    /* Code For DotTuple Goes Here */

    if (dot_tuple->expr_)
      dot_tuple->expr_->accept(this);
    visitInteger(dot_tuple->integer_);
  }

  void VisitTypeCheck::visitConstTrue(ConstTrue *const_true)
  {
    /* Code For ConstTrue Goes Here */
    set_actual_type(const_true, new TypeBool());
  }

  void VisitTypeCheck::visitConstFalse(ConstFalse *const_false)
  {
    /* Code For ConstFalse Goes Here */
    set_actual_type(const_false, new TypeBool());
  }

  void VisitTypeCheck::visitConstInt(ConstInt *const_int)
  {
    /* Code For ConstInt Goes Here */
    set_actual_type(const_int, new TypeNat());

    visitInteger(const_int->integer_);
  }

  void VisitTypeCheck::visitVar(Var *var)
  {
    std::cout << "Visiting variable " << printer.print(var) << "\n";
    Type *varType;
    try
    {
      varType = context.at(var->stellaident_);
    }
    catch (std::out_of_range e)
    {
      throw undefined_variable_error(var);
    }
    set_actual_type(var, varType);
    std::cout << "Exiting variable " << printer.print(var) << "\n"
              << "Type of " << printer.print(var) << " is " << printer.print(varType);
    visitStellaIdent(var->stellaident_);
  }

  void VisitTypeCheck::visitAMatchCase(AMatchCase *a_match_case)
  {
    /* Code For AMatchCase Goes Here */

    if (a_match_case->pattern_)
      a_match_case->pattern_->accept(this);
    if (a_match_case->expr_)
      a_match_case->expr_->accept(this);
  }

  void VisitTypeCheck::visitNoTyping(NoTyping *no_typing)
  {
    /* Code For NoTyping Goes Here */
  }

  void VisitTypeCheck::visitSomeTyping(SomeTyping *some_typing)
  {
    /* Code For SomeTyping Goes Here */

    if (some_typing->type_)
      some_typing->type_->accept(this);
  }

  void VisitTypeCheck::visitNoPatternData(NoPatternData *no_pattern_data)
  {
    /* Code For NoPatternData Goes Here */
  }

  void VisitTypeCheck::visitSomePatternData(SomePatternData *some_pattern_data)
  {
    /* Code For SomePatternData Goes Here */

    if (some_pattern_data->pattern_)
      some_pattern_data->pattern_->accept(this);
  }

  void VisitTypeCheck::visitNoExprData(NoExprData *no_expr_data)
  {
    /* Code For NoExprData Goes Here */
  }

  void VisitTypeCheck::visitSomeExprData(SomeExprData *some_expr_data)
  {
    /* Code For SomeExprData Goes Here */

    if (some_expr_data->expr_)
      some_expr_data->expr_->accept(this);
  }

  void VisitTypeCheck::visitPatternVariant(PatternVariant *pattern_variant)
  {
    /* Code For PatternVariant Goes Here */

    visitStellaIdent(pattern_variant->stellaident_);
    if (pattern_variant->patterndata_)
      pattern_variant->patterndata_->accept(this);
  }

  void VisitTypeCheck::visitPatternTuple(PatternTuple *pattern_tuple)
  {
    /* Code For PatternTuple Goes Here */

    if (pattern_tuple->listpattern_)
      pattern_tuple->listpattern_->accept(this);
  }

  void VisitTypeCheck::visitPatternRecord(PatternRecord *pattern_record)
  {
    /* Code For PatternRecord Goes Here */

    if (pattern_record->listlabelledpattern_)
      pattern_record->listlabelledpattern_->accept(this);
  }

  void VisitTypeCheck::visitPatternList(PatternList *pattern_list)
  {
    /* Code For PatternList Goes Here */

    if (pattern_list->listpattern_)
      pattern_list->listpattern_->accept(this);
  }

  void VisitTypeCheck::visitPatternCons(PatternCons *pattern_cons)
  {
    /* Code For PatternCons Goes Here */

    if (pattern_cons->pattern_1)
      pattern_cons->pattern_1->accept(this);
    if (pattern_cons->pattern_2)
      pattern_cons->pattern_2->accept(this);
  }

  void VisitTypeCheck::visitPatternFalse(PatternFalse *pattern_false)
  {
    /* Code For PatternFalse Goes Here */
  }

  void VisitTypeCheck::visitPatternTrue(PatternTrue *pattern_true)
  {
    /* Code For PatternTrue Goes Here */
  }

  void VisitTypeCheck::visitPatternInt(PatternInt *pattern_int)
  {
    /* Code For PatternInt Goes Here */

    visitInteger(pattern_int->integer_);
  }

  void VisitTypeCheck::visitPatternSucc(PatternSucc *pattern_succ)
  {
    /* Code For PatternSucc Goes Here */

    if (pattern_succ->pattern_)
      pattern_succ->pattern_->accept(this);
  }

  void VisitTypeCheck::visitPatternVar(PatternVar *pattern_var)
  {
    /* Code For PatternVar Goes Here */

    visitStellaIdent(pattern_var->stellaident_);
  }

  void VisitTypeCheck::visitALabelledPattern(ALabelledPattern *a_labelled_pattern)
  {
    /* Code For ALabelledPattern Goes Here */

    visitStellaIdent(a_labelled_pattern->stellaident_);
    if (a_labelled_pattern->pattern_)
      a_labelled_pattern->pattern_->accept(this);
  }

  void VisitTypeCheck::visitABinding(ABinding *a_binding)
  {
    /* Code For ABinding Goes Here */

    visitStellaIdent(a_binding->stellaident_);
    if (a_binding->expr_)
      a_binding->expr_->accept(this);
  }

  void VisitTypeCheck::visitTypeFun(TypeFun *type_fun)
  {
    /* Code For TypeFun Goes Here */

    if (type_fun->listtype_)
      type_fun->listtype_->accept(this);
    if (type_fun->type_)
      type_fun->type_->accept(this);
  }

  void VisitTypeCheck::visitTypeRec(TypeRec *type_rec)
  {
    /* Code For TypeRec Goes Here */

    visitStellaIdent(type_rec->stellaident_);
    if (type_rec->type_)
      type_rec->type_->accept(this);
  }

  void VisitTypeCheck::visitTypeSum(TypeSum *type_sum)
  {
    /* Code For TypeSum Goes Here */

    if (type_sum->type_1)
      type_sum->type_1->accept(this);
    if (type_sum->type_2)
      type_sum->type_2->accept(this);
  }

  void VisitTypeCheck::visitTypeTuple(TypeTuple *type_tuple)
  {
    /* Code For TypeTuple Goes Here */

    if (type_tuple->listtype_)
      type_tuple->listtype_->accept(this);
  }

  void VisitTypeCheck::visitTypeRecord(TypeRecord *type_record)
  {
    /* Code For TypeRecord Goes Here */

    if (type_record->listrecordfieldtype_)
      type_record->listrecordfieldtype_->accept(this);
  }

  void VisitTypeCheck::visitTypeVariant(TypeVariant *type_variant)
  {
    /* Code For TypeVariant Goes Here */

    if (type_variant->listvariantfieldtype_)
      type_variant->listvariantfieldtype_->accept(this);
  }

  void VisitTypeCheck::visitTypeList(TypeList *type_list)
  {
    /* Code For TypeList Goes Here */

    if (type_list->type_)
      type_list->type_->accept(this);
  }

  void VisitTypeCheck::visitTypeBool(TypeBool *type_bool)
  {
    /* Code For TypeBool Goes Here */
  }

  void VisitTypeCheck::visitTypeNat(TypeNat *type_nat)
  {
    /* Code For TypeNat Goes Here */
  }

  void VisitTypeCheck::visitTypeUnit(TypeUnit *type_unit)
  {
    /* Code For TypeUnit Goes Here */
  }

  void VisitTypeCheck::visitTypeVar(TypeVar *type_var)
  {
    /* Code For TypeVar Goes Here */

    visitStellaIdent(type_var->stellaident_);
  }

  void VisitTypeCheck::visitAVariantFieldType(AVariantFieldType *a_variant_field_type)
  {
    /* Code For AVariantFieldType Goes Here */

    visitStellaIdent(a_variant_field_type->stellaident_);
    if (a_variant_field_type->optionaltyping_)
      a_variant_field_type->optionaltyping_->accept(this);
  }

  void VisitTypeCheck::visitARecordFieldType(ARecordFieldType *a_record_field_type)
  {
    /* Code For ARecordFieldType Goes Here */

    visitStellaIdent(a_record_field_type->stellaident_);
    if (a_record_field_type->type_)
      a_record_field_type->type_->accept(this);
  }

  void VisitTypeCheck::visitATyping(ATyping *a_typing)
  {
    /* Code For ATyping Goes Here */

    if (a_typing->expr_)
      a_typing->expr_->accept(this);
    if (a_typing->type_)
      a_typing->type_->accept(this);
  }

  void VisitTypeCheck::visitListStellaIdent(ListStellaIdent *list_stella_ident)
  {
    for (ListStellaIdent::iterator i = list_stella_ident->begin(); i != list_stella_ident->end(); ++i)
    {
      visitStellaIdent(*i);
    }
  }

  void VisitTypeCheck::visitListExtensionName(ListExtensionName *list_extension_name)
  {
    for (ListExtensionName::iterator i = list_extension_name->begin(); i != list_extension_name->end(); ++i)
    {
      visitExtensionName(*i);
    }
  }

  void VisitTypeCheck::visitListExtension(ListExtension *list_extension)
  {
    for (ListExtension::iterator i = list_extension->begin(); i != list_extension->end(); ++i)
    {
      (*i)->accept(this);
    }
  }

  void VisitTypeCheck::visitListDecl(ListDecl *list_decl)
  {
    for (ListDecl::iterator i = list_decl->begin(); i != list_decl->end(); ++i)
    {
      (*i)->accept(this);
    }
  }

  void VisitTypeCheck::visitListLocalDecl(ListLocalDecl *list_local_decl)
  {
    for (ListLocalDecl::iterator i = list_local_decl->begin(); i != list_local_decl->end(); ++i)
    {
      (*i)->accept(this);
    }
  }

  void VisitTypeCheck::visitListAnnotation(ListAnnotation *list_annotation)
  {
    for (ListAnnotation::iterator i = list_annotation->begin(); i != list_annotation->end(); ++i)
    {
      (*i)->accept(this);
    }
  }

  void VisitTypeCheck::visitListParamDecl(ListParamDecl *list_param_decl)
  {
    for (ListParamDecl::iterator i = list_param_decl->begin(); i != list_param_decl->end(); ++i)
    {
      (*i)->accept(this);
    }
  }

  void VisitTypeCheck::visitListExpr(ListExpr *list_expr)
  {
    for (ListExpr::iterator i = list_expr->begin(); i != list_expr->end(); ++i)
    {
      (*i)->accept(this);
    }
  }

  void VisitTypeCheck::visitListMatchCase(ListMatchCase *list_match_case)
  {
    for (ListMatchCase::iterator i = list_match_case->begin(); i != list_match_case->end(); ++i)
    {
      (*i)->accept(this);
    }
  }

  void VisitTypeCheck::visitListPattern(ListPattern *list_pattern)
  {
    for (ListPattern::iterator i = list_pattern->begin(); i != list_pattern->end(); ++i)
    {
      (*i)->accept(this);
    }
  }

  void VisitTypeCheck::visitListLabelledPattern(ListLabelledPattern *list_labelled_pattern)
  {
    for (ListLabelledPattern::iterator i = list_labelled_pattern->begin(); i != list_labelled_pattern->end(); ++i)
    {
      (*i)->accept(this);
    }
  }

  void VisitTypeCheck::visitListBinding(ListBinding *list_binding)
  {
    for (ListBinding::iterator i = list_binding->begin(); i != list_binding->end(); ++i)
    {
      (*i)->accept(this);
    }
  }

  void VisitTypeCheck::visitListType(ListType *list_type)
  {
    for (ListType::iterator i = list_type->begin(); i != list_type->end(); ++i)
    {
      (*i)->accept(this);
    }
  }

  void VisitTypeCheck::visitListVariantFieldType(ListVariantFieldType *list_variant_field_type)
  {
    for (ListVariantFieldType::iterator i = list_variant_field_type->begin(); i != list_variant_field_type->end(); ++i)
    {
      (*i)->accept(this);
    }
  }

  void VisitTypeCheck::visitListRecordFieldType(ListRecordFieldType *list_record_field_type)
  {
    for (ListRecordFieldType::iterator i = list_record_field_type->begin(); i != list_record_field_type->end(); ++i)
    {
      (*i)->accept(this);
    }
  }

  void VisitTypeCheck::visitInteger(Integer x)
  {
    /* Code for Integer Goes Here */
  }

  void VisitTypeCheck::visitChar(Char x)
  {
    /* Code for Char Goes Here */
  }

  void VisitTypeCheck::visitDouble(Double x)
  {
    /* Code for Double Goes Here */
  }

  void VisitTypeCheck::visitString(String x)
  {
    /* Code for String Goes Here */
  }

  void VisitTypeCheck::visitIdent(Ident x)
  {
    /* Code for Ident Goes Here */
  }

  void VisitTypeCheck::visitStellaIdent(StellaIdent x)
  {
    /* Code for StellaIdent Goes Here */
  }

  void VisitTypeCheck::visitExtensionName(ExtensionName x)
  {
    /* Code for ExtensionName Goes Here */
  }
}
