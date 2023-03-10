// ts2fable 0.8.0-build.664
module rec Pako
open System
open Fable.Core
open Fable.Core.JS

let [<ImportAll("pako")>] pako: Pako.IExports = jsNative

module Pako =

    type [<AllowNullLiteral>] IExports =
        /// Compress data with deflate algorithm and options.
        abstract deflate: data: U2<Data, string> * ?options: DeflateFunctionOptions -> Uint8Array
        /// The same as deflate, but creates raw data, without wrapper (header and adler32 crc).
        abstract deflateRaw: data: U2<Data, string> * ?options: DeflateFunctionOptions -> Uint8Array
        /// The same as deflate, but create gzip wrapper instead of deflate one.
        abstract gzip: data: U2<Data, string> * ?options: DeflateFunctionOptions -> Uint8Array
        /// Decompress data with inflate/ungzip and options. Autodetect format via wrapper header
        /// by default. That's why we don't provide separate ungzip method.
        abstract inflate: data: Data * options: obj -> string
        abstract inflate: data: Data * ?options: InflateFunctionOptions -> Uint8Array
        /// The same as inflate, but creates raw data, without wrapper (header and adler32 crc).
        abstract inflateRaw: data: Data * options: obj -> string
        abstract inflateRaw: data: Data * ?options: InflateFunctionOptions -> Uint8Array
        /// Just shortcut to inflate, because it autodetects format by header.content. Done for convenience.
        abstract ungzip: data: Data * options: obj -> string
        abstract ungzip: data: Data * ?options: InflateFunctionOptions -> Uint8Array
        abstract Deflate: DeflateStatic
        abstract Inflate: InflateStatic

    type [<RequireQualifiedAccess>] constants =
        | Z_NO_FLUSH = 0
        | Z_PARTIAL_FLUSH = 1
        | Z_SYNC_FLUSH = 2
        | Z_FULL_FLUSH = 3
        | Z_FINISH = 4
        | Z_BLOCK = 5
        | Z_TREES = 6
        | Z_FILTERED = 1
        | Z_HUFFMAN_ONLY = 2
        | Z_RLE = 3
        | Z_FIXED = 4
        | Z_DEFAULT_STRATEGY = 0
        | Z_OK = 0
        | Z_STREAM_END = 1
        | Z_NEED_DICT = 2
        | Z_ERRNO = -1
        | Z_STREAM_ERROR = -2
        | Z_DATA_ERROR = -3
        | Z_BUF_ERROR = -5

    type FlushValues =
        constants

    type StrategyValues =
        constants

    type ReturnCodes =
        constants

    type [<AllowNullLiteral>] DeflateOptions =
        abstract level: DeflateOptionsLevel option with get, set
        abstract windowBits: float option with get, set
        abstract memLevel: float option with get, set
        abstract strategy: StrategyValues option with get, set
        abstract dictionary: obj option with get, set
        abstract raw: bool option with get, set
        abstract chunkSize: float option with get, set
        abstract gzip: bool option with get, set
        abstract header: Header option with get, set

    type [<AllowNullLiteral>] DeflateFunctionOptions =
        abstract level: DeflateOptionsLevel option with get, set
        abstract windowBits: float option with get, set
        abstract memLevel: float option with get, set
        abstract strategy: StrategyValues option with get, set
        abstract dictionary: obj option with get, set
        abstract raw: bool option with get, set

    type [<AllowNullLiteral>] InflateOptions =
        abstract windowBits: float option with get, set
        abstract dictionary: obj option with get, set
        abstract raw: bool option with get, set
        abstract ``to``: InflateOptionsTo option with get, set
        abstract chunkSize: float option with get, set

    type [<AllowNullLiteral>] InflateFunctionOptions =
        abstract windowBits: float option with get, set
        abstract raw: bool option with get, set
        abstract ``to``: InflateOptionsTo option with get, set

    type [<AllowNullLiteral>] Header =
        abstract text: bool option with get, set
        abstract time: float option with get, set
        abstract os: float option with get, set
        abstract extra: float[] option with get, set
        abstract name: string option with get, set
        abstract comment: string option with get, set
        abstract hcrc: bool option with get, set

    type Data =
        U2<Uint8Array, ArrayBuffer>

    type [<AllowNullLiteral>] Deflate =
        abstract err: ReturnCodes with get, set
        abstract msg: string with get, set
        abstract result: Uint8Array with get, set
        abstract onData: chunk: Data -> unit
        abstract onEnd: status: float -> unit
        abstract push: data: U2<Data, string> * ?mode: U2<FlushValues, bool> -> bool

    type [<AllowNullLiteral>] DeflateStatic =
        [<EmitConstructor>] abstract Create: ?options: DeflateOptions -> Deflate

    type [<AllowNullLiteral>] Inflate =
        abstract header: Header option with get, set
        abstract err: ReturnCodes with get, set
        abstract msg: string with get, set
        abstract result: U2<Uint8Array, string> with get, set
        abstract onData: chunk: Data -> unit
        abstract onEnd: status: float -> unit
        abstract push: data: Data * ?mode: U2<FlushValues, bool> -> bool

    type [<AllowNullLiteral>] InflateStatic =
        [<EmitConstructor>] abstract Create: ?options: InflateOptions -> Inflate

    type [<RequireQualifiedAccess>] DeflateOptionsLevel =
        | N0 = 0
        | N1 = 1
        | N2 = 2
        | N3 = 3
        | N4 = 4
        | N5 = 5
        | N6 = 6
        | N7 = 7
        | N8 = 8
        | N9 = 9

    type [<StringEnum>] [<RequireQualifiedAccess>] InflateOptionsTo =
        | String
