module Extract

open System.IO
open FSharpPlus
open Mono.Cecil
open Mono.Cecil.Cil
open Utils.CecilHelpers

open UdonBase

let worldSdkDir =
  Path.Combine(__SOURCE_DIRECTORY__, "..", "project", "Packages", "com.vrchat.worlds")

let udonRuntimeDir =
  Path.Combine(worldSdkDir, "Runtime", "Udon", "External")

let udonEditorDir =
  Path.Combine(worldSdkDir, "Editor", "Udon", "External")

module Asm =
  let ``VRC.Udon.Wrapper.dll`` =
    AssemblyDefinition.ReadAssembly(Path.Combine(udonRuntimeDir, "VRC.Udon.Wrapper.dll"))

  let ``VRC.Udon.VRCWrapperModules.dll`` =
    AssemblyDefinition.ReadAssembly(Path.Combine(udonRuntimeDir, "VRC.Udon.VRCWrapperModules.dll"))

  let ``VRC.Udon.UAssembly.dll`` =
    AssemblyDefinition.ReadAssembly(Path.Combine(udonEditorDir, "VRC.Udon.UAssembly.dll"))

  let ``VRC.Udon.EditorBindings.dll`` =
    AssemblyDefinition.ReadAssembly(Path.Combine(udonEditorDir, "VRC.Udon.EditorBindings.dll"))

  let ``VRC.Udon.VRCTypeResolverModules.dll`` =
    AssemblyDefinition.ReadAssembly(Path.Combine(udonEditorDir, "VRC.Udon.VRCTypeResolverModules.dll"))

let getTypeMapFromTypeDefinition (m: Map<string, TypeReference>) (td: TypeDefinition) =
  let cctor = td.Methods |> Seq.find (fun t -> t.Name = ".cctor")
  let il =
    cctor.Body.Instructions
    |> Seq.skipWhile (fun op ->
      op.OpCode <> OpCodes.Newobj &&
      match op.Operand with
      | CecilType { name = "Dictionary`2"; args = [ CecilType { name = "String" }; CecilType { name = "Type" } ] } -> false
      | _ -> true)
    |> Seq.takeWhile (fun op ->
      op.OpCode <> OpCodes.Stfld &&
      match op.Operand with
      | CecilField { name = "_types" } -> false
      | _ -> true)
    |> Seq.toList
  let rec read acc = function
    | CecilOp OpCodes.Ldstr (:? string as key) :: CecilOp OpCodes.Ldtoken (:? TypeReference as value) :: rest ->
      read (acc |> Map.add key value) rest
    | _ :: rest -> read acc rest
    | [] -> acc
  read m il

let typeResolvers =
  let resolver (asm: AssemblyDefinition) (name: string) =
    asm.MainModule.Types
    |> Seq.tryFind (fun t -> t.Name = name)
    |> Option.toList
  List.concat [
    resolver Asm.``VRC.Udon.VRCTypeResolverModules.dll`` "VRCTypeResolver";
    resolver Asm.``VRC.Udon.EditorBindings.dll`` "UdonTypeResolver";
    resolver Asm.``VRC.Udon.UAssembly.dll`` "SystemTypeResolver";
  ]

let typeDefinitions =
  let getExternClasses (a: AssemblyDefinition) =
    a.MainModule.Types
    |> Seq.filter (fun t -> t.FullName.StartsWith("VRC.Udon.Wrapper.Modules.Extern"))
  Seq.concat
    [ getExternClasses Asm.``VRC.Udon.Wrapper.dll``
      getExternClasses Asm.``VRC.Udon.VRCWrapperModules.dll`` ]
  |> Seq.toArray

let letterOnly =
  String.map (fun c -> if System.Char.IsLetter c then c else '_')
  >> String.replace "_" ""

let getTypeMapFromExternClasses (m: Map<string, TypeReference>) (td: TypeDefinition) =
  td.Methods
  |> Seq.map (fun meth -> meth.Name, meth.Body.Instructions)
  |> Seq.collect (fun (name, insts) ->
    insts |> Seq.choose (function
      | CecilOp OpCodes.Callvirt (CecilMethod ({ name = "GetHeapVariable" | "SetHeapVariable" } as meth)) ->
        Some (name, meth)
      | _ -> None
    ))
  |> Seq.choose (fun (name, meth) ->
    match name |> String.split ["__"] |> Seq.filter ((<>) "") |> Seq.toList, meth.tyargs with
    | _ :: rest, [| ty |] ->
      let internalName =
        rest
        |> Seq.collect UdonExternType.safeSplitSignature
        |> Seq.tryFind (fun s -> letterOnly ty.FullName = letterOnly s)
      match internalName with
      | Some key when m |> Map.containsKey key -> None
      | None -> None
      | Some key -> Some (key, ty)
    | _ -> None)
  |> Seq.distinctBy fst
  |> Seq.fold (fun m (key, ty) -> m |> Map.add key ty) m

let getParameterCounts (td: TypeDefinition) =
  let ctor = td.Methods |> Seq.find (fun t -> t.Name = ".ctor")
  let il =
    ctor.Body.Instructions
    |> Seq.skipWhile (fun op ->
      op.OpCode <> OpCodes.Newobj &&
      match op.Operand with
      | CecilType { name = "Dictionary`2"; args = [ CecilType { name = "String" }; CecilType { name = "Int32" } ] } -> false
      | _ -> true)
    |> Seq.takeWhile (fun op ->
      op.OpCode <> OpCodes.Stfld &&
      match op.Operand with
      | CecilField { name = "_parameterCounts" } -> false
      | _ -> true)
    |> Seq.toList
  let rec read acc = function
    | CecilOp OpCodes.Ldstr (:? string as key) :: ldc_i4 :: rest when ldc_i4.OpCode.Name.StartsWith("ldc.i4") ->
      let value =
        match ldc_i4 with
        | CecilOp OpCodes.Ldc_I4 _
        | CecilOp OpCodes.Ldc_I4_S _ ->
          match ldc_i4.Operand with
          | :? int32 as value -> value
          | :? sbyte as value -> int value
          | x ->
            failwithf "unknown operand %s (%s) for %s"
              (x.ToString()) (x.GetType().FullName) ldc_i4.OpCode.Name
        | CecilOp OpCodes.Ldc_I4_M1 _ -> -1
        | op when op.OpCode.Name.StartsWith("ldc.i4.") ->
          match System.Int32.TryParse(op.OpCode.Name.Substring("ldc.i4.".Length)) with
          | true, value -> value
          | false, _ ->
            failwithf "unknown opcode %s" op.OpCode.Name
        | _ -> failwithf "unknown opcode %s" ldc_i4.OpCode.Name
      read ((td, key, value) :: acc) rest
    | _ :: rest -> read acc rest
    | [] -> List.rev acc
  read [] il

let getNameOfTypeDefinition (td: TypeDefinition) =
  let getName = td.Methods |> Seq.find (fun m -> m.Name = "get_Name")
  getName.Body.Instructions
  |> Seq.pick (function
    | CecilOp OpCodes.Ldstr (:? string as value) -> Some value
    | _ -> None)

let nameOfTypeDefinition =
  typeDefinitions
  |> Seq.map (fun td -> td.FullName, getNameOfTypeDefinition td)
  |> Map.ofSeq

let rec getUdonTypeInfo (t: TypeReference) =
  let tyargs =
    if t.IsGenericInstance then
      (t :?> GenericInstanceType).GenericArguments |> Seq.toArray
    else Array.empty
  {
    Name = Option.ofObj t.Name; FullName = Option.ofObj t.FullName;
    Namespace = Option.ofObj t.Namespace;
    IsValueType = t.IsValueType; IsPrimitive = t.IsPrimitive
    IsArray = t.IsArray;
    ElementType = t.GetElementType() |> Option.ofObj |> Option.filter (fun u -> t.Name <> u.Name) |> Option.map getUdonTypeInfo
    IsGenericTypeParameter = t.IsGenericParameter
    IsGenericType = (t.ContainsGenericParameter && not t.IsGenericParameter) || t.IsGenericInstance
    ContainsGenericParameters = t.ContainsGenericParameter
    GenericTypeArguments = tyargs |> Array.map getUdonTypeInfo
    IsNested = t.IsNested
    DeclaringType = t.DeclaringType |> Option.ofObj |> Option.filter (fun u -> t.Name <> u.Name) |> Option.map getUdonTypeInfo
  }

let tryGetActualMethod (signature: string) (name: string) (m: MethodDefinition) =
  let expectedSignature =
    signature |> String.skip 2
  let expectedName =
    if name = "ctor" then ".ctor"
    else name
  let expectedField =
    if name |> String.startsWith "get_" || name |> String.startsWith "set_" then
      name |> String.skip 4
    else name
  let picker = function
    | CecilOp OpCodes.Call (CecilMember m)
    | CecilOp OpCodes.Callvirt (CecilMember m)
    | CecilOp OpCodes.Newobj (CecilMember m)
      when m.name = expectedSignature || m.name = expectedName -> Some m
    | CecilOp OpCodes.Ldfld (CecilMember m)
    | CecilOp OpCodes.Stfld (CecilMember m)
      when m.name = expectedField -> Some m
    | _ -> None
  m.Body.Instructions |> Seq.tryPick picker

let getUdonParameterInfo (p: ParameterDefinition) =
  {
    Type = getUdonTypeInfo p.ParameterType
    IsOptional = p.IsOptional
    IsIn = p.IsIn; IsOut = p.IsOut
  }

let getUdonMemberInfo (m: CecilMember) =
  {
    Name = m.name; FullName = m.fullname
    DeclaringType = m.declType |> getUdonTypeInfo
    IsMethod = m.kind = CecilMemberKind.Method
    IsProperty = m.kind = CecilMemberKind.Property
    IsField = m.kind = CecilMemberKind.Field
    Parameters = m.prms |> Array.map getUdonParameterInfo
    Type = m.ty |> getUdonTypeInfo
    GenericArguments = m.tyargs |> Array.map getUdonTypeInfo
    GenericParameters = m.typrms |> Array.map getUdonTypeInfo
  }

let getExternDefinition (td: TypeDefinition, signature: string, arity: int) =
  let md = td.Methods |> Seq.find (fun m -> m.Name = signature)
  let name, ty = UdonExternType.parse signature arity
  let mi = tryGetActualMethod signature name md
  signature,
  {
    Name = name; UdonTypeName = nameOfTypeDefinition |> Map.find td.FullName
    Type = ty |> UdonExternType.map UdonType.parse; MemberInfo = mi |> Option.map getUdonMemberInfo
  }

let createUdonInfo sdkVersion =
  printfn "  - Gathering types."
  let typeMap =
    let tmp = typeResolvers |> List.fold getTypeMapFromTypeDefinition Map.empty
    typeDefinitions |> Array.fold getTypeMapFromExternClasses tmp

  let u2f =
    typeMap |> Map.map (fun _ value -> value.FullName)
  let f2u =
    typeMap
    |> Map.toSeq
    |> Seq.map (fun (key, value) -> value.FullName, key)
    |> Map.ofSeq

  printfn "  - Gathering type information."
  let f2t =
    f2u |> Map.map (fun _ value -> typeMap |> Map.find value |> getUdonTypeInfo)

  printfn "  - Gathering externs."
  let s2e =
    typeDefinitions
    |> Seq.collect getParameterCounts
    |> Seq.map getExternDefinition
    |> Map.ofSeq

  {
    SdkVersion = sdkVersion
    UdonTypeNameToFullName = u2f; FullNameToUdonTypeName = f2u
    FullNameToTypeInfo = f2t; SignatureToExterns = s2e
  }
