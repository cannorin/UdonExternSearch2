module Utils

open Mono.Cecil
open Mono.Cecil.Cil

module CecilHelpers =
  type CecilType = {
    self: TypeReference
    name: string; fullname: string
    ns: string
    args: TypeReference list
    prms: GenericParameter list
  }

  let (|CecilType|_|) (t: obj) : CecilType option =
    match t with
    | :? TypeReference as t ->
      Some {
        self = t; name = t.Name; fullname = t.FullName; ns = t.Namespace
        args =
          if t.IsGenericInstance then
            (t :?> GenericInstanceType).GenericArguments |> List.ofSeq
          else
            []
        prms = t.GenericParameters |> List.ofSeq
      }
    | _ -> None

  type CecilField = {
    self: FieldReference
    name: string; fullname: string
    declType: TypeReference
    ty: TypeReference
  }

  let (|CecilField|_|) (t: obj) : CecilField option =
    match t with
    | :? FieldReference as t ->
      Some {
        self = t
        name = t.Name; fullname = t.FullName
        declType = t.DeclaringType
        ty = t.FieldType
      }
    | _ -> None

  type CecilMethod = {
    self: MethodReference
    name: string; fullname: string
    declType: TypeReference
    prms: ParameterDefinition[]
    retTy: TypeReference
    typrms: GenericParameter[]
    tyargs: TypeReference[]
  }

  let (|CecilMethod|_|) (t: obj) : CecilMethod option =
    match t with
    | :? MethodReference as t ->
      let tyargs =
        if t.IsGenericInstance then
          (t :?> GenericInstanceMethod).GenericArguments |> Seq.toArray
        else [||]
      Some {
        self = t;
        name = t.Name; fullname = t.FullName
        declType = t.DeclaringType
        prms = t.Parameters |> Seq.toArray
        typrms = t.GenericParameters |> Seq.toArray
        tyargs = tyargs
        retTy = t.ReturnType
      }
    | _ -> None

  type CecilProperty = {
    self: PropertyReference
    name: string; fullname: string
    declType: TypeReference
    prms: ParameterDefinition[]
    ty: TypeReference
  }

  let (|CecilProperty|_|) (t: obj) : CecilProperty option =
    match t with
    | :? PropertyReference as t ->
      Some {
        self = t
        name = t.Name; fullname = t.FullName
        declType = t.DeclaringType
        prms = t.Parameters |> Seq.toArray
        ty = t.PropertyType
      }
    | _ -> None

  type CecilMemberKind =
    | Field = 0
    | Method = 1
    | Property = 2

  type CecilMember = {
    kind: CecilMemberKind
    self: MemberReference
    name: string; fullname: string
    declType: TypeReference
    prms: ParameterDefinition[]
    ty: TypeReference
    typrms: GenericParameter[]
    tyargs: TypeReference[]
  } with
    static member Create(kind, self, name, fullname, declType, ty, ?prms, ?typrms, ?tyargs) =
      {
        kind = kind; self = self; name = name; fullname = fullname
        declType = declType; ty = ty
        prms = prms |> Option.defaultValue Array.empty
        typrms = typrms |> Option.defaultValue Array.empty
        tyargs = tyargs |> Option.defaultValue Array.empty
      }

  let (|CecilMember|_|) (t: obj) : CecilMember option =
    match t with
    | :? PropertyReference as t ->
      CecilMember.Create(
        CecilMemberKind.Property, t, t.Name, t.FullName, t.DeclaringType, t.PropertyType,
        t.Parameters |> Seq.toArray
      ) |> Some
    | :? FieldReference as t ->
      CecilMember.Create(
        CecilMemberKind.Field, t, t.Name, t.FullName, t.DeclaringType, t.FieldType
      ) |> Some
    | :? MethodReference as t ->
      let tyargs =
        if t.IsGenericInstance then
          (t :?> GenericInstanceMethod).GenericArguments |> Seq.toArray
        else [||]
      CecilMember.Create(
        CecilMemberKind.Method, t, t.Name, t.FullName, t.DeclaringType, t.ReturnType,
        t.Parameters |> Seq.toArray,
        t.GenericParameters |> Seq.toArray,
        tyargs
      ) |> Some
    | _ -> None

  let (|CecilOp|_|) (code: OpCode) (i: Instruction) : obj option =
    if i.OpCode <> code then None
    else Some i.Operand
