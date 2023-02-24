open System
open System.IO
open Thoth.Json.Net
open UdonBase
open Utils

let charCoder =
  Extra.empty
  |> Extra.withCustom
    (string >> Encode.string)
    (fun str obj ->
      Decode.string str obj
      |> Result.bind (fun s ->
        if s.Length = 1 then Ok s.[0]
        else Error (DecoderError("not a char", FailMessage "not a char"))))

type VpmPackage = {
  version: string
}

type VpmManifest = {
  locked: Map<string, VpmPackage>
}

let vrcsdk3Version =
  let vpmManifestPath = Path.Combine(__SOURCE_DIRECTORY__, "..", "project", "Packages", "vpm-manifest.json")
  let vpmManifest =
    Decode.Auto.fromString<VpmManifest>(File.ReadAllText(vpmManifestPath))
  match vpmManifest with
  | Ok m ->
    let package = m.locked |> Map.find "com.vrchat.worlds"
    package.version
  | Error _ -> "unknown"
let udonsdkVersion = vrcsdk3Version

[<EntryPoint>]
let main (argv: string[]) =
  printfn "SDK version: %s" vrcsdk3Version

  let types =
    let xs =
      UdonType.getAllSupported ()
      |> Seq.distinctBy fst
    UdonType.createTyperMap xs

  let externs = GraphNode.createExternMap types

  let info = { Externs = externs; Types = types; VRCSDK3Version = vrcsdk3Version; UDONSDKVersion = udonsdkVersion }

  let encode x =
    Encode.Auto.toString(x, extra=charCoder, skipNullField=true)

  let infoJson = encode info

  let targetDir =
    if argv.Length = 0 then Environment.CurrentDirectory
    else
      let dirName = Path.GetFullPath argv.[0]
      if Directory.Exists dirName then dirName
      else
        try
          Directory.CreateDirectory dirName |> ignore
          dirName
        with
         | _ -> failwithf "error: directory '%s' does not exist" argv.[0]

  let write fileName json =
    let path = Path.Combine(targetDir, fileName)
    File.WriteAllText (path, json)

  write "udon_info.json" infoJson
  0
