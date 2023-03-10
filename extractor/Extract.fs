module Extract

open System.IO
open FSharpPlus
open Mono.Cecil
open Mono.Cecil.Cil
open Utils.CecilHelpers

open UdonBase

module UdonExternType =
  let safeSplitSignature (s: string) =
    let knownPrefix = [
      "TMProTMP"; "VRCSDKBaseVRC";
    ]
    knownPrefix
    |> List.fold (fun (s: string) prefix -> s.Replace($"{prefix}_", $"{prefix}-")) s
    |> fun (s: string) -> s.Split('_')
    |> Seq.map (fun s -> knownPrefix |> List.fold (fun (s: string) prefix -> s.Replace($"{prefix}-", $"{prefix}_")) s)
    |> Seq.toArray

  let parse thisType (signature: string) arity =
    let (|Args|) = safeSplitSignature
    let baseObj = {
      Kind = UdonExternKind.Unknown; Arity = arity
      ThisType = None
      GenericParameters = [||]
      Parameters = [||]
      ReturnType = None
    }
    let Constructor (args, ret) =
      { baseObj with Kind = UdonExternKind.Constructor; Parameters = args; ReturnType = Some ret }
    let StaticFunc (args, ret) =
      { baseObj with Kind = UdonExternKind.Static; Parameters = args; ReturnType = ret }
    let InstanceFunc (args, ret) =
      { baseObj with ThisType = Some thisType; Kind = UdonExternKind.Instance; Parameters = args; ReturnType = ret }
    let StaticGenericFunc (tyargs, args, ret) =
      { baseObj with Kind = UdonExternKind.Static; GenericParameters = tyargs; Parameters = args; ReturnType = ret }
    let InstanceGenericFunc (tyargs, args, ret) =
      { baseObj with ThisType = Some thisType; Kind = UdonExternKind.Instance; GenericParameters = tyargs; Parameters = args; ReturnType = ret }

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

let typeMap =
  printfn "  - Gathering types."
  let tmp = typeResolvers |> List.fold getTypeMapFromTypeDefinition Map.empty
  typeDefinitions |> Array.fold getTypeMapFromExternClasses tmp

let getNameOfTypeDefinition (td: TypeDefinition) =
  let getName = td.Methods |> Seq.find (fun m -> m.Name = "get_Name")
  getName.Body.Instructions
  |> Seq.tryPick (function
    | CecilOp OpCodes.Ldstr (:? string as value)
      when typeMap |> Map.containsKey value -> Some value
    | _ -> None)
  |> Option.defaultWith (fun () -> failwithf "module '%s' does not have a corresponding Udon type." td.FullName)

let udonTypeNameOfTypeDefinition =
  typeDefinitions
  |> Seq.map (fun td -> td.FullName, getNameOfTypeDefinition td)
  |> Map.ofSeq

let u2f =
  typeMap |> Map.map (fun _ value -> value.FullName)

let f2u =
  typeMap
  |> Map.toSeq
  |> Seq.map (fun (key, value) -> value.FullName, key)
  |> Seq.distinctBy fst
  |> Map.ofSeq

let rec getUdonTypeInfo (udonTypeName: string option) (t: TypeReference) =
  let tyargs =
    if t.IsGenericInstance then
      (t :?> GenericInstanceType).GenericArguments |> Seq.toArray
    else Array.empty
  let elemType = t.GetElementType() |> Option.ofObj |> Option.filter (fun u -> t.Name <> u.Name)
  let declType = t.DeclaringType |> Option.ofObj |> Option.filter (fun u -> t.Name <> u.Name)

  let self = {
    Name = t.Name; FullName = Some t.FullName; UdonTypeName = udonTypeName
    Namespace =
      if System.String.IsNullOrEmpty t.Namespace then None
      else Some t.Namespace;
    ElementType = elemType |> Option.bind (fun u -> Option.ofObj u.FullName)
    GenericTypeArguments =
      let tyargs = tyargs |> Array.map (fun u -> u.FullName)
      if Array.isEmpty tyargs then None else Some tyargs
    DeclaringType = declType |> Option.bind (fun u -> Option.ofObj u.FullName)
    IsSpecial = None
  }

  seq {
    yield self
    match elemType with
    | Some u -> yield! getUdonTypeInfo (f2u |> Map.tryFind u.FullName) u
    | None -> ()
    match declType with
    | Some u -> yield! getUdonTypeInfo (f2u |> Map.tryFind u.FullName) u
    | None -> ()
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

let getExternDefinition (td: TypeDefinition, signature: string, arity: int) =
  let md = td.Methods |> Seq.find (fun m -> m.Name = signature)
  let udonTypeName = udonTypeNameOfTypeDefinition |> Map.find td.FullName
  let name, ty = UdonExternType.parse udonTypeName signature arity
  let mi = tryGetActualMethod signature name md
  $"{udonTypeName}.{name}", {
    Name = name
    Signature = $"{udonTypeName}.{signature}"
    DotNetFullName = mi |> Option.map (fun m -> m.fullname)
    Type = ty
  }

let createUdonInfo sdkVersion =
  printfn "  - Gathering type information."
  let f2t =
    f2u
    |> Map.values
    |> Seq.collect (fun value -> typeMap |> Map.find value |> getUdonTypeInfo (Some value))
    |> Seq.distinctBy (fun t -> t.FullName)
    |> Seq.choose (fun t -> t.FullName |> Option.map (fun fn -> fn, t))
    |> Map.ofSeq

  printfn "  - Gathering externs."
  let externs =
    typeDefinitions
    |> Seq.collect getParameterCounts
    |> Seq.map getExternDefinition
    |> Seq.groupBy fst
    |> Seq.map (fun (k, v) -> k, v |> Seq.map snd |> Array.ofSeq)
    |> Map.ofSeq

  let mkSpecial name : UdonTypeInfo = {
    Name = name; FullName = None; UdonTypeName = Some name;
    Namespace = None; ElementType = None; GenericTypeArguments = None; DeclaringType = None;
    IsSpecial = Some true
  }

  {
    SdkVersion = sdkVersion

    UdonTypeNameToFullName =
      u2f
      |> Map.add "T" "T"
      |> Map.add "TArray" "TArray"
      |> Map.add "ListT" "ListT"

    FullNameToTypeInfo =
      f2t
      |> Map.add "T"      (mkSpecial "T")
      |> Map.add "TArray" { mkSpecial "TArray" with ElementType = Some "T" }
      |> Map.add "ListT"  { mkSpecial "ListT" with GenericTypeArguments = Some [|"T"|] }

    Externs = externs
  }
