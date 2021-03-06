﻿open System
open SkiaSharp
open Argu
open ImageProcessing

type Arguments =
    | Radius of int
    | Scale of int
    | Alpha of byte
    | OriginalImage of string
    | Input of string
    | Output of string
    | Measure
    interface IArgParserTemplate with
        member self.Usage =
            match self with
            | Radius _ -> "The radius used for all circles."
            | Scale _ -> "Scale factor for x and y coordinates."
            | Alpha _ -> "The alfa factor to apply to the overlay circles."
            | OriginalImage _ -> "The original image to overlay."
            | Input _ -> "The input JSON file."
            | Output _ -> "The generated PNG image."
            | Measure -> "Measures the time of the single steps."

/// Used to read input from JSON.
type Input = { id: string; x: string; y: string }

let checkFileEnding ext (path: string) =
    if IO.Path.GetExtension(path) <> ext then
        failwithf
            "Invalid file extension. Expected a %s file."
            (ext.[1..].ToUpper())
    else
        path

let checkExists path =
    if not (IO.File.Exists(path)) then
        failwithf "File not found: %s" path
    else
        path

let readJson input =
    IO.File.ReadAllText(input)
    |> Newtonsoft.Json.JsonConvert.DeserializeObject<Input array>

let measure caption f =
    let sw = Diagnostics.Stopwatch.StartNew()
    let result = f ()
    do
        Console.ForegroundColor <- ConsoleColor.Yellow
        printfn "%s: %ims"
            caption
            sw.ElapsedMilliseconds
        Console.ResetColor()
    result

[<EntryPoint>]
let main args =
    let checkStructure =
#if DEBUG
        true
#else
        false
#endif
    let parser =
        ArgumentParser.Create(
            errorHandler = ProcessExiter(),
            checkStructure = checkStructure
        )
    let result = parser.ParseCommandLine(args)

    let radius =
        result.TryGetResult <@ Radius @>
        |> Option.defaultValue 80

    let scale =
        result.TryGetResult <@ Scale @>
        |> Option.defaultValue 8

    let alpha =
        result.TryGetResult <@ Alpha @>
        |> Option.defaultValue 127uy

    let originalImage =
        result.TryGetResult <@ OriginalImage @>
        |> Option.defaultValue "./burgstall.jpg"

    let input =
        result.TryPostProcessResult(
            <@ Input @>,
            checkFileEnding ".json" >> checkExists
        )
        |> Option.defaultValue "./circles.json"

    let output =
        result.TryPostProcessResult(
            <@ Output @>,
            checkFileEnding ".png"
        )
        |> Option.defaultValue "./circles.png"

    let withMeasure =
        result.Contains <@ Measure @>

    let measure caption f =
        if withMeasure then
            measure caption f
        else
            f ()

    measure "TOTAL" <| fun () ->
        /// Array of circles with a random color.
        /// Imports the circles from a JSON file.
        let ccircles =
            measure "Read points from JSON" <| fun () ->
            readJson input
            |> Array.map (fun i ->
                let color = rndColor ()
                let x = int i.x * scale
                let y = int i.y * scale
                (x, y), color)

        use original =
            measure "Read original image" <| fun () ->
            SKBitmap.Decode(originalImage)

        use overlay =
            measure "Draw mask" <| fun () ->
            getColorMap
                (original.Width, original.Height)
                radius
                ccircles

        measure "Recolor pixels" <| fun () ->
            /// Convert array of circles and colors to an array
            /// of points and colors for convenience.
            /// Function used to find the closest point and use its color.
            let getClosestColor point =
                ccircles
                |> Array.minBy (fst >> distance point)
                |> snd

            /// Rewrite pixels of the original bitmap.
            recolorPixels
                getClosestColor
                alpha
                overlay
                original

        measure "Write to file" <| fun () ->
            /// Write the new bitmap to HD.
            use image = SKImage.FromBitmap(original)
            use data = image.Encode(SKEncodedImageFormat.Png, 100)
            let path = IO.Path.GetFullPath(output)
            use stream = IO.File.OpenWrite(path)
            data.SaveTo(stream)
            printfn "File written to: %s" path

    0
