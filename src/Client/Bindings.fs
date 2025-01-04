namespace FileSaver

open System
open Fable.Core
open Fable.Core.JS

[<AllowNullLiteral>]
type IFileSaverOptions =
    /// Automatically provide Unicode text encoding hints
    abstract autoBom: bool with get, set

[<AllowNullLiteral>]
type IFileSaver =
    /// <summary>FileSaver.js implements the saveAs() FileSaver interface in browsers that do not natively support it.</summary>
    /// <param name="data">- The actual file data blob or URL.</param>
    /// <param name="filename">- The optional name of the file to be downloaded. If omitted, the name used in the file data will be used. If none is provided "download" will be used.</param>
    /// <param name="options">- Optional FileSaver.js config</param>
    abstract saveAs: data: Browser.Types.Blob * ?filename: string * ?options: IFileSaverOptions -> unit

/// <summary>FileSaver.js implements the saveAs() FileSaver interface in browsers that do not natively support it.</summary>
/// <param name="data">- The actual file data blob or URL.</param>
/// <param name="filename">- The optional name of the file to be downloaded. If omitted, the name used in the file data will be used. If none is provided "download" will be used.</param>
/// <param name="disableAutoBOM">- Optional & defaults to `false`. Set to `true` if you want FileSaver.js to automatically provide Unicode text encoding hints</param>
// abstract saveAs: data: string * ?filename: string * ?disableAutoBOM: bool -> unit


module fileSaver =

    [<ImportAll("file-saver")>]
    let fileSaver: IFileSaver = jsNative
