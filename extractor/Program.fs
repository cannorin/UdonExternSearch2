open System
open System.IO
open System.IO.Compression
open FSharpPlus
open Thoth.Json.Net

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

let vrcsdk3Version =
  let pkgPath = Path.Combine(__SOURCE_DIRECTORY__, "..", "project", "Packages", "com.vrchat.worlds", "package.json")
  let pkg =
    Decode.Auto.fromString<VpmPackage>(File.ReadAllText(pkgPath))
  match pkg with
  | Ok p -> p.version
  | Error _ -> "unknown"

[<EntryPoint>]
let main (argv: string[]) =
  printfn "* SDK version: %s" vrcsdk3Version

  printfn "* Analyzing SDK."
  let info = Extract.createUdonInfo vrcsdk3Version

  printfn "* Encoding data in JSON."
  let infoJson =
    Encode.Auto.toString(info, extra=charCoder, skipNullField=true)

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
    printfn "* Creating '%s/%s'." targetDir fileName
    let path = Path.Combine(targetDir, fileName)
    File.WriteAllText (path, json)

    printfn "* Creating '%s/%s.gz'." targetDir fileName
    let pathGZ = Path.ChangeExtension(path, Path.GetExtension(path) + ".gz")
    use writeStream = File.OpenWrite(pathGZ)
    use gzStream = new GZipStream(writeStream, CompressionMode.Compress)
    use writer = new StreamWriter(gzStream)
    writer.Write(json)

  write "udonInfo.json" infoJson

  printfn "* Complete."
  0
