#include "Sexp.h"

namespace sexp {
namespace detail {

std::string cursorKindName(CXCursorKind);

std::ostream& operator<<(std::ostream& out, const sexp_proxy<CXCursorKind>& cursor_kind) {
  return out << cursorKindName(cursor_kind.value);
}

/**

   This is a rather large, automatically generated dump of
   CXCursorKind definition, from the source in clang-c/Index.h.

   It is a separate file so that it wouldn't pollute other files.

   NOTE: There are CXCursor_(First|Last)* values; these are not real
   and are commented out.

   FIXME Some of these are undefined on my OSX, I don't know why.

   FIXME Others are equal to each other, despite not being obviously
   the same.
*/

std::string cursorKindName(CXCursorKind cursor_kind) {
  switch (cursor_kind) {
  case CXCursor_UnexposedDecl: return "UnexposedDecl";
  case CXCursor_StructDecl: return "StructDecl";
  case CXCursor_UnionDecl: return "UnionDecl";
  case CXCursor_ClassDecl: return "ClassDecl";
  case CXCursor_EnumDecl: return "EnumDecl";
  case CXCursor_FieldDecl: return "FieldDecl";
  case CXCursor_EnumConstantDecl: return "EnumConstantDecl";
  case CXCursor_FunctionDecl: return "FunctionDecl";
  case CXCursor_VarDecl: return "VarDecl";
  case CXCursor_ParmDecl: return "ParmDecl";
  case CXCursor_ObjCInterfaceDecl: return "ObjCInterfaceDecl";
  case CXCursor_ObjCCategoryDecl: return "ObjCCategoryDecl";
  case CXCursor_ObjCProtocolDecl: return "ObjCProtocolDecl";
  case CXCursor_ObjCPropertyDecl: return "ObjCPropertyDecl";
  case CXCursor_ObjCIvarDecl: return "ObjCIvarDecl";
  case CXCursor_ObjCInstanceMethodDecl: return "ObjCInstanceMethodDecl";
  case CXCursor_ObjCClassMethodDecl: return "ObjCClassMethodDecl";
  case CXCursor_ObjCImplementationDecl: return "ObjCImplementationDecl";
  case CXCursor_ObjCCategoryImplDecl: return "ObjCCategoryImplDecl";
  case CXCursor_TypedefDecl: return "TypedefDecl";
  case CXCursor_CXXMethod: return "CXXMethod";
  case CXCursor_Namespace: return "Namespace";
  case CXCursor_LinkageSpec: return "LinkageSpec";
  case CXCursor_Constructor: return "Constructor";
  case CXCursor_Destructor: return "Destructor";
  case CXCursor_ConversionFunction: return "ConversionFunction";
  case CXCursor_TemplateTypeParameter: return "TemplateTypeParameter";
  case CXCursor_NonTypeTemplateParameter: return "NonTypeTemplateParameter";
  case CXCursor_TemplateTemplateParameter: return "TemplateTemplateParameter";
  case CXCursor_FunctionTemplate: return "FunctionTemplate";
  case CXCursor_ClassTemplate: return "ClassTemplate";
  case CXCursor_ClassTemplatePartialSpecialization: return "ClassTemplatePartialSpecialization";
  case CXCursor_NamespaceAlias: return "NamespaceAlias";
  case CXCursor_UsingDirective: return "UsingDirective";
  case CXCursor_UsingDeclaration: return "UsingDeclaration";
  case CXCursor_TypeAliasDecl: return "TypeAliasDecl";
  case CXCursor_ObjCSynthesizeDecl: return "ObjCSynthesizeDecl";
  case CXCursor_ObjCDynamicDecl: return "ObjCDynamicDecl";
  case CXCursor_CXXAccessSpecifier: return "CXXAccessSpecifier";
    // case CXCursor_FirstDecl: return "FirstDecl";
    // case CXCursor_LastDecl: return "LastDecl";
    // case CXCursor_FirstRef: return "FirstRef";
  case CXCursor_ObjCSuperClassRef: return "ObjCSuperClassRef";
  case CXCursor_ObjCProtocolRef: return "ObjCProtocolRef";
  case CXCursor_ObjCClassRef: return "ObjCClassRef";
  case CXCursor_TypeRef: return "TypeRef";
  case CXCursor_CXXBaseSpecifier: return "CXXBaseSpecifier";
  case CXCursor_TemplateRef: return "TemplateRef";
  case CXCursor_NamespaceRef: return "NamespaceRef";
  case CXCursor_MemberRef: return "MemberRef";
  case CXCursor_LabelRef: return "LabelRef";
  case CXCursor_OverloadedDeclRef: return "OverloadedDeclRef";
  case CXCursor_VariableRef: return "VariableRef";
    // case CXCursor_LastRef: return "LastRef";
    // case CXCursor_FirstInvalid: return "FirstInvalid";
  case CXCursor_InvalidFile: return "InvalidFile";
  case CXCursor_NoDeclFound: return "NoDeclFound";
  case CXCursor_NotImplemented: return "NotImplemented";
  case CXCursor_InvalidCode: return "InvalidCode";
    // case CXCursor_LastInvalid: return "LastInvalid";
    // case CXCursor_FirstExpr: return "FirstExpr";
  case CXCursor_UnexposedExpr: return "UnexposedExpr";
  case CXCursor_DeclRefExpr: return "DeclRefExpr";
  case CXCursor_MemberRefExpr: return "MemberRefExpr";
  case CXCursor_CallExpr: return "CallExpr";
  case CXCursor_ObjCMessageExpr: return "ObjCMessageExpr";
  case CXCursor_BlockExpr: return "BlockExpr";
  case CXCursor_IntegerLiteral: return "IntegerLiteral";
  case CXCursor_FloatingLiteral: return "FloatingLiteral";
  case CXCursor_ImaginaryLiteral: return "ImaginaryLiteral";
  case CXCursor_StringLiteral: return "StringLiteral";
  case CXCursor_CharacterLiteral: return "CharacterLiteral";
  case CXCursor_ParenExpr: return "ParenExpr";
  case CXCursor_UnaryOperator: return "UnaryOperator";
  case CXCursor_ArraySubscriptExpr: return "ArraySubscriptExpr";
  case CXCursor_BinaryOperator: return "BinaryOperator";
  case CXCursor_CompoundAssignOperator: return "CompoundAssignOperator";
  case CXCursor_ConditionalOperator: return "ConditionalOperator";
  case CXCursor_CStyleCastExpr: return "CStyleCastExpr";
  case CXCursor_CompoundLiteralExpr: return "CompoundLiteralExpr";
  case CXCursor_InitListExpr: return "InitListExpr";
  case CXCursor_AddrLabelExpr: return "AddrLabelExpr";
  case CXCursor_StmtExpr: return "StmtExpr";
  case CXCursor_GenericSelectionExpr: return "GenericSelectionExpr";
  case CXCursor_GNUNullExpr: return "GNUNullExpr";
  case CXCursor_CXXStaticCastExpr: return "CXXStaticCastExpr";
  case CXCursor_CXXDynamicCastExpr: return "CXXDynamicCastExpr";
  case CXCursor_CXXReinterpretCastExpr: return "CXXReinterpretCastExpr";
  case CXCursor_CXXConstCastExpr: return "CXXConstCastExpr";
  case CXCursor_CXXFunctionalCastExpr: return "CXXFunctionalCastExpr";
  case CXCursor_CXXTypeidExpr: return "CXXTypeidExpr";
  case CXCursor_CXXBoolLiteralExpr: return "CXXBoolLiteralExpr";
  case CXCursor_CXXNullPtrLiteralExpr: return "CXXNullPtrLiteralExpr";
  case CXCursor_CXXThisExpr: return "CXXThisExpr";
  case CXCursor_CXXThrowExpr: return "CXXThrowExpr";
  case CXCursor_CXXNewExpr: return "CXXNewExpr";
  case CXCursor_CXXDeleteExpr: return "CXXDeleteExpr";
  case CXCursor_UnaryExpr: return "UnaryExpr";
  case CXCursor_ObjCStringLiteral: return "ObjCStringLiteral";
  case CXCursor_ObjCEncodeExpr: return "ObjCEncodeExpr";
  case CXCursor_ObjCSelectorExpr: return "ObjCSelectorExpr";
  case CXCursor_ObjCProtocolExpr: return "ObjCProtocolExpr";
  case CXCursor_ObjCBridgedCastExpr: return "ObjCBridgedCastExpr";
  case CXCursor_PackExpansionExpr: return "PackExpansionExpr";
  case CXCursor_SizeOfPackExpr: return "SizeOfPackExpr";
  case CXCursor_LambdaExpr: return "LambdaExpr";
  case CXCursor_ObjCBoolLiteralExpr: return "ObjCBoolLiteralExpr";
  case CXCursor_ObjCSelfExpr: return "ObjCSelfExpr";
    // case CXCursor_LastExpr: return "LastExpr";
    // case CXCursor_FirstStmt: return "FirstStmt";
  case CXCursor_UnexposedStmt: return "UnexposedStmt";
  case CXCursor_LabelStmt: return "LabelStmt";
  case CXCursor_CompoundStmt: return "CompoundStmt";
  case CXCursor_CaseStmt: return "CaseStmt";
  case CXCursor_DefaultStmt: return "DefaultStmt";
  case CXCursor_IfStmt: return "IfStmt";
  case CXCursor_SwitchStmt: return "SwitchStmt";
  case CXCursor_WhileStmt: return "WhileStmt";
  case CXCursor_DoStmt: return "DoStmt";
  case CXCursor_ForStmt: return "ForStmt";
  case CXCursor_GotoStmt: return "GotoStmt";
  case CXCursor_IndirectGotoStmt: return "IndirectGotoStmt";
  case CXCursor_ContinueStmt: return "ContinueStmt";
  case CXCursor_BreakStmt: return "BreakStmt";
  case CXCursor_ReturnStmt: return "ReturnStmt";
  case CXCursor_GCCAsmStmt: return "GCCAsmStmt";
    // case CXCursor_AsmStmt: return "AsmStmt";
  case CXCursor_ObjCAtTryStmt: return "ObjCAtTryStmt";
  case CXCursor_ObjCAtCatchStmt: return "ObjCAtCatchStmt";
  case CXCursor_ObjCAtFinallyStmt: return "ObjCAtFinallyStmt";
  case CXCursor_ObjCAtThrowStmt: return "ObjCAtThrowStmt";
  case CXCursor_ObjCAtSynchronizedStmt: return "ObjCAtSynchronizedStmt";
  case CXCursor_ObjCAutoreleasePoolStmt: return "ObjCAutoreleasePoolStmt";
  case CXCursor_ObjCForCollectionStmt: return "ObjCForCollectionStmt";
  case CXCursor_CXXCatchStmt: return "CXXCatchStmt";
  case CXCursor_CXXTryStmt: return "CXXTryStmt";
  case CXCursor_CXXForRangeStmt: return "CXXForRangeStmt";
  case CXCursor_SEHTryStmt: return "SEHTryStmt";
  case CXCursor_SEHExceptStmt: return "SEHExceptStmt";
  case CXCursor_SEHFinallyStmt: return "SEHFinallyStmt";
  case CXCursor_MSAsmStmt: return "MSAsmStmt";
  case CXCursor_NullStmt: return "NullStmt";
  case CXCursor_DeclStmt: return "DeclStmt";
  case CXCursor_OMPParallelDirective: return "OMPParallelDirective";
  case CXCursor_OMPSimdDirective: return "OMPSimdDirective";
  case CXCursor_OMPForDirective: return "OMPForDirective";
  case CXCursor_OMPSectionsDirective: return "OMPSectionsDirective";
  case CXCursor_OMPSectionDirective: return "OMPSectionDirective";
  case CXCursor_OMPSingleDirective: return "OMPSingleDirective";
  case CXCursor_OMPParallelForDirective: return "OMPParallelForDirective";
  case CXCursor_OMPParallelSectionsDirective: return "OMPParallelSectionsDirective";
  case CXCursor_OMPTaskDirective: return "OMPTaskDirective";
  case CXCursor_OMPMasterDirective: return "OMPMasterDirective";
  case CXCursor_OMPCriticalDirective: return "OMPCriticalDirective";
  case CXCursor_OMPTaskyieldDirective: return "OMPTaskyieldDirective";
  case CXCursor_OMPBarrierDirective: return "OMPBarrierDirective";
  case CXCursor_OMPTaskwaitDirective: return "OMPTaskwaitDirective";
  case CXCursor_OMPFlushDirective: return "OMPFlushDirective";
  case CXCursor_SEHLeaveStmt: return "SEHLeaveStmt";
  case CXCursor_OMPOrderedDirective: return "OMPOrderedDirective";
  case CXCursor_OMPAtomicDirective: return "OMPAtomicDirective";
  case CXCursor_OMPForSimdDirective: return "OMPForSimdDirective";
  case CXCursor_OMPParallelForSimdDirective: return "OMPParallelForSimdDirective";
  case CXCursor_OMPTargetDirective: return "OMPTargetDirective";
  case CXCursor_OMPTeamsDirective: return "OMPTeamsDirective";
    // case CXCursor_LastStmt: return "LastStmt";
  case CXCursor_TranslationUnit: return "TranslationUnit";
    // case CXCursor_FirstAttr: return "FirstAttr";
  case CXCursor_UnexposedAttr: return "UnexposedAttr";
  case CXCursor_IBActionAttr: return "IBActionAttr";
  case CXCursor_IBOutletAttr: return "IBOutletAttr";
  case CXCursor_IBOutletCollectionAttr: return "IBOutletCollectionAttr";
  case CXCursor_CXXFinalAttr: return "CXXFinalAttr";
  case CXCursor_CXXOverrideAttr: return "CXXOverrideAttr";
  case CXCursor_AnnotateAttr: return "AnnotateAttr";
  case CXCursor_AsmLabelAttr: return "AsmLabelAttr";
  case CXCursor_PackedAttr: return "PackedAttr";
  case CXCursor_PureAttr: return "PureAttr";
  case CXCursor_ConstAttr: return "ConstAttr";
  case CXCursor_NoDuplicateAttr: return "NoDuplicateAttr";
  case CXCursor_CUDAConstantAttr: return "CUDAConstantAttr";
  case CXCursor_CUDADeviceAttr: return "CUDADeviceAttr";
  case CXCursor_CUDAGlobalAttr: return "CUDAGlobalAttr";
  case CXCursor_CUDAHostAttr: return "CUDAHostAttr";
  case CXCursor_CUDASharedAttr: return "CUDASharedAttr";
    // case CXCursor_LastAttr: return "LastAttr";
  case CXCursor_PreprocessingDirective: return "PreprocessingDirective";
  case CXCursor_MacroDefinition: return "MacroDefinition";
  case CXCursor_MacroExpansion: return "MacroExpansion";
    // NOTE duplicate
    // case CXCursor_MacroInstantiation: return "MacroInstantiation";
  case CXCursor_InclusionDirective: return "InclusionDirective";
    // case CXCursor_FirstPreprocessing: return "FirstPreprocessing";
    // case CXCursor_LastPreprocessing: return "LastPreprocessing";
  case CXCursor_ModuleImportDecl: return "ModuleImportDecl";
    // case CXCursor_FirstExtraDecl: return "FirstExtraDecl";
    // case CXCursor_LastExtraDecl: return "LastExtraDecl";
  default: return "UnknownCursorKind";
  }
}

} // namespace detail
} // namespace sexp
