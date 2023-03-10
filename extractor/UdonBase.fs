module UdonBase

type [<RequireQualifiedAccess>] UdonTypeKind =
  | UdonTypeName
  | GenericParameter
  | GenericParameterArray
  | GenericParameterList
  | Unknown

type UdonType = {
  Name: string
  Kind: UdonTypeKind
}

module UdonType =
  let parse = function
    | "T" -> { Name = "T"; Kind = UdonTypeKind.GenericParameter }
    | "TArray" -> { Name = "TArray"; Kind = UdonTypeKind.GenericParameterArray }
    | "ListT" -> { Name = "ListT"; Kind = UdonTypeKind.GenericParameterList }
    | s ->
      let kind =
        if isNull s then UdonTypeKind.Unknown
        else UdonTypeKind.UdonTypeName
      { Name = s; Kind = kind }

type [<RequireQualifiedAccess>] UdonExternKind = Static | Instance | Constructor | Unknown

type UdonExternType<'T> = {
  Kind: UdonExternKind
  Arity: int
  GenericParameters: string[]
  Parameters: 'T[]
  ReturnType: 'T option
}

module UdonExternType =
  let map f (et: UdonExternType<_>) =
    {
      Kind = et.Kind; Arity = et.Arity
      GenericParameters = et.GenericParameters
      Parameters = et.Parameters |> Array.map f
      ReturnType = et.ReturnType |> Option.map f
    }

  let safeSplitSignature (s: string) =
    let knownPrefix = [
      "TMProTMP"; "VRCSDKBaseVRC";
    ]
    knownPrefix
    |> List.fold (fun (s: string) prefix -> s.Replace($"{prefix}_", $"{prefix}-")) s
    |> fun (s: string) -> s.Split('_')
    |> Seq.map (fun s -> knownPrefix |> List.fold (fun (s: string) prefix -> s.Replace($"{prefix}-", $"{prefix}_")) s)
    |> Seq.toArray

  let parse (signature: string) arity =
    let (|Args|) = safeSplitSignature
    let baseObj = {
      Kind = UdonExternKind.Unknown; Arity = arity
      GenericParameters = [||]
      Parameters = [||]
      ReturnType = None
    }
    let Constructor (args, ret) =
      { baseObj with Kind = UdonExternKind.Constructor; Parameters = args; ReturnType = Some ret }
    let StaticFunc (args, ret) =
      { baseObj with Kind = UdonExternKind.Static; Parameters = args; ReturnType = ret }
    let InstanceFunc (args, ret) =
      { baseObj with Kind = UdonExternKind.Instance; Parameters = args; ReturnType = ret }
    let StaticGenericFunc (tyargs, args, ret) =
      { baseObj with Kind = UdonExternKind.Static; GenericParameters = tyargs; Parameters = args; ReturnType = ret }
    let InstanceGenericFunc (tyargs, args, ret) =
      { baseObj with Kind = UdonExternKind.Instance; GenericParameters = tyargs; Parameters = args; ReturnType = ret }

    let StaticVoidRetArgFunc = StaticFunc (Array.empty, None)
    let inline StaticVoidRetFunc xs = StaticFunc (xs, None)
    let inline StaticVoidArgFunc x  = StaticFunc (Array.empty, Some x)
    let InstanceVoidRetArgFunc = InstanceFunc (Array.empty, None)
    let inline InstanceVoidRetFunc xs = InstanceFunc (xs, None)
    let inline InstanceVoidArgFunc x  = InstanceFunc (Array.empty, Some x)
    match signature.Split([|"__"|], System.StringSplitOptions.RemoveEmptyEntries) |> Seq.toList with
    | [] -> failwith "impossible"
    | name :: rest ->
      let rest = Array.ofList rest
      match name, rest with
      | "ctor", [| ret |] when arity = 1 -> name, Constructor ([||], ret)
      | "ctor", [| Args args; ret |] when args.Length + 1 = arity -> name, Constructor (args, ret)
      | _, [| "SystemVoid" |] ->
        if arity = 0 then name, StaticVoidRetArgFunc
        else if arity = 1 then name, InstanceVoidRetArgFunc
        else name, baseObj
      | _, [| ("T" | "TArray") as ret |] ->
        if arity = 2 then name, StaticGenericFunc([|"T"|], [||], Some ret)
        else if arity = 3 then name, InstanceGenericFunc([|"T"|], [||], Some ret)
        else name, baseObj
      | _, [| ret |] ->
        if arity = 1 then name, StaticVoidArgFunc ret
        else if arity = 2 then name, InstanceVoidArgFunc ret
        else name, baseObj
      | _, [| Args args; "SystemVoid" |] ->
        if arity = args.Length then name, StaticVoidRetFunc args
        else if arity = args.Length + 1 then name, InstanceVoidRetFunc args
        else name, baseObj
      | _, [| Args args; ("T" | "TArray") as ret |] ->
        if arity = args.Length + 2 then name, StaticGenericFunc ([|"T"|], args, Some ret)
        else if arity = args.Length + 3 then name, InstanceGenericFunc ([|"T"|], args, Some ret)
        else name, baseObj
      | _, [| Args args; ret |] ->
        if arity = args.Length + 1 then name, StaticFunc (args, Some ret)
        else if arity = args.Length + 2 then name, InstanceFunc (args, Some ret)
        else name, baseObj
      | _ -> name, baseObj

type UdonTypeInfo = {
  Name: string option
  FullName: string option
  Namespace: string option
  IsValueType: bool
  IsPrimitive: bool
  IsArray: bool
  ElementType: UdonTypeInfo option
  IsGenericTypeParameter: bool
  IsGenericType: bool
  ContainsGenericParameters: bool
  GenericTypeArguments: UdonTypeInfo[]
  IsNested: bool
  DeclaringType: UdonTypeInfo option
}

type UdonParameterInfo = {
  Type: UdonTypeInfo
  IsOptional: bool
  IsIn: bool
  IsOut: bool
}

type UdonMemberInfo = {
  Name: string
  FullName: string
  DeclaringType: UdonTypeInfo
  IsMethod: bool
  IsProperty: bool
  IsField: bool
  Parameters: UdonParameterInfo[]
  Type: UdonTypeInfo
  GenericArguments: UdonTypeInfo[]
  GenericParameters: UdonTypeInfo[]
}

type UdonExternDefinition = {
  Name: string
  UdonTypeName: string
  Type: UdonExternType<UdonType>
  MemberInfo: UdonMemberInfo option
}

type UdonInfo = {
  SdkVersion: string
  UdonTypeNameToFullName: Map<string, string>
  FullNameToUdonTypeName: Map<string, string>
  FullNameToTypeInfo: Map<string, UdonTypeInfo>
  SignatureToExterns: Map<string, UdonExternDefinition>
}
