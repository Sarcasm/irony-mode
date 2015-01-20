#include "Sexp.h"

namespace sexp {
namespace detail {

/**
   One big switch statement for pretty-printing CXTypeKind.
 */

std::string typeKindName(CXTypeKind type_kind) {
  switch (type_kind) {
  case CXType_Invalid: return "Invalid";
  case CXType_Unexposed: return "Unexposed";
  case CXType_Void: return "Void";
  case CXType_Bool: return "Bool";
  case CXType_Char_U: return "U";
  case CXType_UChar: return "UChar";
  case CXType_Char16: return "Char16";
  case CXType_Char32: return "Char32";
  case CXType_UShort: return "UShort";
  case CXType_UInt: return "UInt";
  case CXType_ULong: return "ULong";
  case CXType_ULongLong: return "ULongLong";
  case CXType_UInt128: return "UInt128";
  case CXType_Char_S: return "S";
  case CXType_SChar: return "SChar";
  case CXType_WChar: return "WChar";
  case CXType_Short: return "Short";
  case CXType_Int: return "Int";
  case CXType_Long: return "Long";
  case CXType_LongLong: return "LongLong";
  case CXType_Int128: return "Int128";
  case CXType_Float: return "Float";
  case CXType_Double: return "Double";
  case CXType_LongDouble: return "LongDouble";
  case CXType_NullPtr: return "NullPtr";
  case CXType_Overload: return "Overload";
  case CXType_Dependent: return "Dependent";
  case CXType_ObjCId: return "ObjCId";
  case CXType_ObjCClass: return "ObjCClass";
  case CXType_ObjCSel: return "ObjCSel";
  // case CXType_FirstBuiltin: return "FirstBuiltin";
  // case CXType_LastBuiltin  = case CXType_ObjCSel,;
  case CXType_Complex: return "Complex";
  case CXType_Pointer: return "Pointer";
  case CXType_BlockPointer: return "BlockPointer";
  case CXType_LValueReference: return "LValueReference";
  case CXType_RValueReference: return "RValueReference";
  case CXType_Record: return "Record";
  case CXType_Enum: return "Enum";
  case CXType_Typedef: return "Typedef";
  case CXType_ObjCInterface: return "ObjCInterface";
  case CXType_ObjCObjectPointer: return "ObjCObjectPointer";
  case CXType_FunctionNoProto: return "FunctionNoProto";
  case CXType_FunctionProto: return "FunctionProto";
  case CXType_ConstantArray: return "ConstantArray";
  case CXType_Vector: return "Vector";
  case CXType_IncompleteArray: return "IncompleteArray";
  case CXType_VariableArray: return "VariableArray";
  case CXType_DependentSizedArray: return "DependentSizedArray";
  case CXType_MemberPointer: return "MemberPointer";
  default: return "UnknownCXTypeKind";
  }
}

std::ostream& operator<<(std::ostream& out,
                       const sexp_proxy<CXTypeKind>& type_kind)
{
  return out << typeKindName(type_kind.value);
}

} // namespace detail
} // namespace sexp
