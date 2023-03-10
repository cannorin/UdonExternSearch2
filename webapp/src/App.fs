module App

open Elmish
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Fable.FontAwesome
open Fulma
open System
open UdonBase

open Thoth.Elmish

type State = Initial | IsTyping | StoppedTyping

type Model = {
  Query : string[]
  Data: UdonInfo option
  Debouncer: Debouncer.State
  InputState: State
  ErrorMessage: string option
}

type Msg =
  | DebouncerSelfMsg of Debouncer.SelfMessage<Msg>
  | ChangeQuery of string
  | EndOfInput
  | LoadData
  | SetData of UdonInfo
  | LoadDataError of string

let init _ =
  { Query = [||]; Data = None; Debouncer = Debouncer.create (); InputState = Initial; ErrorMessage = None },
  Cmd.ofMsg LoadData

open Thoth.Json
let charCoder =
  Extra.empty
  |> Extra.withCustom
    (string >> Encode.string)
    (fun str obj ->
      Decode.string str obj
      |> Result.bind (fun s ->
        if s.Length = 1 then Ok s.[0]
        else Error ("not a char", FailMessage "not a char")))

let decoder : Decoder<UdonInfo> =
  Decode.Auto.generateDecoderCached(extra=charCoder)

let getData () : Cmd<Msg> =
  promise {
    let url = "assets/udonInfo.json.gz"
    let! data = Fetch.fetch url []
    let! blob = data.blob()
    let! buf = blob.arrayBuffer()
    let json = Pako.pako.ungzip(!^buf, {| ``to`` = "string" |})
    let data = Decode.fromString decoder json
    match data with
    | Ok data -> return SetData data
    | Error msg -> return LoadDataError msg
  } |> Cmd.OfPromise.result

let private update msg model =
  match msg with
  | DebouncerSelfMsg m ->
    let dM, dC = Debouncer.update m model.Debouncer
    { model with Debouncer = dM }, dC
  | ChangeQuery newValue ->
    let dM, dC =
      model.Debouncer |> Debouncer.bounce (TimeSpan.FromSeconds 1.0) "user_input" EndOfInput
    { model with Query = newValue.Split([|' '; '.'|], StringSplitOptions.RemoveEmptyEntries)
                 InputState = IsTyping; Debouncer = dM },
    Cmd.batch [ Cmd.map DebouncerSelfMsg dC ]
  | EndOfInput -> { model with InputState = StoppedTyping }, Cmd.none
  | LoadData -> model, getData ()
  | SetData data -> { model with Data = Some data }, Cmd.none
  | LoadDataError msg -> { model with ErrorMessage = Some msg }, Cmd.none

let private viewExtern (info: UdonExternDefinition) =
  let descr =
    match info.Type.Kind, info.Type.GenericParameters.Length > 0 with
    | UdonExternKind.Static, true  -> "Static generic function"
    | UdonExternKind.Static, false -> "Static function"
    | UdonExternKind.Instance, true  -> "Instance generic function"
    | UdonExternKind.Instance, false -> "Generic function"
    | UdonExternKind.Constructor, _ -> "Constructor"
    | UdonExternKind.Unknown, _ -> "Unknown"

  let table =
    if info.Type.Kind = UdonExternKind.Unknown then
      [ for i = 1 to info.Type.Arity do yield "unknown" ]
    else
      [
        match info.Type.ThisType with
        | Some t -> yield sprintf "instance: %s" t
        | None -> ()

        for arg in info.Type.Parameters do
          yield sprintf "arg: %s" arg

        for typrm in info.Type.GenericParameters do
          yield sprintf "type parameter: SystemType object of type %s" typrm

        match info.Type.ReturnType with
        | Some r -> yield sprintf "output: %s" r
        | None -> ()
      ]
  Box.box' [] [
    h5 [] [str (descr + " "); code [] [str info.Signature]]

    Table.table [ Table.IsFullWidth ] [
      thead [] [
        tr [] [
          th [] [str "Stack Position"]
          th [] [str "Usage"]
        ]
      ]
      tbody [] [
        for i, u in table |> Seq.rev |> Seq.indexed |> Seq.rev do
          tr [] [
            th [ ] [ str (string (i+1)) ]
            td [ ] [ str u ]
          ]
      ]
    ]

    h6 [] [str ".NET Equivalent:"]
    match info.DotNetFullName with
    | Some dfn ->
      code [Style [Display DisplayOptions.Block; Width "100%"]] [str dfn]
    | None -> null
  ]

let private containsCaseInsensitive (str: string) (substr: string) =
  str.ToLower().Contains(substr.ToLower())

let private view model dispatch =
  main [] [
    Section.section [] [
      Content.content [] [
        h1 [] [
          str "Udon Extern Search 2 (Udon 関数検索2)"
          a [ Href "https://github.com/cannorin/UdonExternSearch2" ] [
            Icon.icon [Icon.Size IsLarge ] [
              Fa.i [Fa.Brand.Github] []
            ]
          ]
          a [ Href "https://twitter.com/cannorin_vrc" ] [
            Icon.icon [Icon.Size IsLarge ] [
              Fa.i [Fa.Brand.Twitter] []
            ]
          ]
        ]
        p [] [
          match model.Data with
          | Some data ->
            yield
              str (
                sprintf "SDK version: %s" data.SdkVersion
              )
          | None -> ()
        ]
        p [] [str "Udon で使える関数を検索できます．"]
        p [] [str "Here you can search extern functions available in Udon."]
        p [] [str "関数の完全名と，呼び出すにはスタックの何番目に何を入れればいいかを見ることができます．"]
        p [] [str "You can also see the signature (full name) and stack usage per function."]
        Control.div [ Control.HasIconLeft ] [
          Input.text [
            Input.Size IsMedium
            Input.Placeholder (if model.Data.IsSome then "Search" else "Loading")
            Input.Disabled model.Data.IsNone
            Input.OnChange (fun e -> dispatch (ChangeQuery e.Value))
          ]
          Icon.icon [ Icon.Size IsMedium; Icon.IsLeft ] [
            Fa.i [ Fa.Solid.Search ] []
          ]
        ]
      ]
    ]

    if model.InputState = StoppedTyping && Array.isEmpty model.Query |> not then
      Section.section [] [
        Content.content [] [
          match model.Data with
          | Some data ->
            let xs =
              data.Externs
              |> Map.toSeq
              |> Seq.filter (fun (k, _) -> model.Query |> Array.forall (containsCaseInsensitive k))
            Container.container [ ] [
              if Seq.isEmpty xs then
                yield
                  div [ ClassName "block" ] [
                    Notification.notification [ Notification.Color IsWarning ] [
                      str "No match"
                    ]
                  ]
              for k, infos in xs do
                yield
                  div [ ClassName "block" ] [
                    Box.box' [ ] [
                      yield h2 [] [str k]
                      yield h5 [] [str (sprintf "%i overload(s)" infos.Length)]
                      for info in infos do
                        yield viewExtern info
                    ]
                ]
            ]
          | None -> ()
        ]
      ]
  ]


open Elmish.Debug
open Elmish.HMR

Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.run
