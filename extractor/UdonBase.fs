module UdonBase

type [<RequireQualifiedAccess>] UdonTypeKind =
  | UdonTypeName
  | GenericParameter
  | GenericParameterArray
  | GenericParameterList
  | Unknown

type [<RequireQualifiedAccess>] UdonExternKind = Static | Instance | Constructor | Unknown

type UdonExternType = {
  Kind: UdonExternKind
  Arity: int
  ThisType: string option
  GenericParameters: string[]
  Parameters: string[]
  ReturnType: string option
}

type UdonTypeInfo = {
  Name: string
  FullName: string option
  UdonTypeName: string option
  Namespace: string option
  ElementType: string option
  GenericTypeArguments: string[] option
  DeclaringType: string option
  IsSpecial: bool option
}

type UdonExternDefinition = {
  Name: string
  Signature: string
  DotNetFullName: string option
  Type: UdonExternType
}

type UdonInfo = {
  SdkVersion: string
  UdonTypeNameToFullName: Map<string, string>
  FullNameToTypeInfo: Map<string, UdonTypeInfo>
  Externs: Map<string, UdonExternDefinition[]>
}
