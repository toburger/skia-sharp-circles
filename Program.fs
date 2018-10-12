open System
open SkiaSharp
open Newtonsoft.Json

type Circle = Circle of x: int * y: int * radius: int

type Input = { id: string; x: string; y: string }

let rndColor =
    let rnd = Random()
    let next () = byte (rnd.Next(127, 256))
    fun () -> SKColor(next (), next (), next (), 255uy)

let radius = 80

let inline (|Point|) (p: SKPoint) =
    p.X, p.Y

let distance (mmx: int, mmy: int) (ox: int, oy: int) =
    Math.Sqrt(float ((mmx-ox)*(mmx-ox)+(mmy-oy)*(mmy-oy)))

let pixels (bitmap: SKBitmap) = [|
    for x = 0 to bitmap.Width - 1 do
    for y = 0 to bitmap.Height - 1 do
        let color = bitmap.GetPixel(x, y)
        yield x, y, color
|]

let grayscalePixels (bitmap: SKBitmap) = [|
    for x = 0 to bitmap.Width - 1 do
    for y = 0 to bitmap.Height - 1 do
        let color = bitmap.GetPixel(x, y)
        let grayscale = byte ((int color.Red + int color.Green + int color.Blue) / 3)
        yield grayscale
|]

let blend (a: SKColor) (b: SKColor) =
    let aA, rA, gA, bA = int a.Alpha, int a.Red, int a.Green, int a.Blue
    let aB, rB, gB, bB = int b.Alpha, int b.Red, int b.Green, int b.Blue
    let rOut = (rA * aA / 255) + (rB * aB * (255 - aA) / (255 * 255))
    let gOut = (gA * aA / 255) + (gB * aB * (255 - aA) / (255 * 255))
    let bOut = (bA * aA / 255) + (bB * aB * (255 - aA) / (255 * 255))
    let aOut = aA + (aB * (255 - aA) / 255)
    SKColor(byte rOut, byte gOut, byte bOut, byte aOut)

let recolorPixels
        calcDistance
        (grayscaledPixels: byte [])
        (originalPixels: (int * int * SKColor) []): (int * int * SKColor) [] =
    originalPixels
    |> Array.Parallel.mapi (fun i (x, y, color) ->
        let grayscale = grayscaledPixels.[i]
        if grayscale > 0uy then
            let _, (ncolor: SKColor) = calcDistance (x, y)
            let alpha = byte (int grayscale / 2)
            let ncolor = ncolor.WithAlpha(alpha)
            x, y, blend ncolor color
        else x, y, color)

let getColorMap (width, height) ccircles =
    let info = SKImageInfo(width, height)
    use surface = SKSurface.Create(info)
    let canvas = surface.Canvas
    canvas.Clear(SKColors.Black)
    for Circle (x, y, radius), _ in ccircles do
        let color = SKColors.White
        use paint = new SKPaint(Color = color, FilterQuality = SKFilterQuality.High)
        canvas.DrawCircle(SKPoint(float32 x, float32 y), float32 radius, paint)
    use image = surface.Snapshot()
    use bitmap = SKBitmap.FromImage(image)
    grayscalePixels bitmap

[<EntryPoint>]
let main _ =
    let ccircles =
        IO.File.ReadAllText("./circles.json")
        |> JsonConvert.DeserializeObject<Input array>
        |> Array.map (fun i ->
            let color = rndColor ()
            let x = int i.x * 8
            let y = int i.y * 8
            Circle(x, y, radius), color)

    use bitmap = SKBitmap.Decode("./burgstall.jpg")

    printfn "Get pixels"
    let originalPixels = pixels bitmap

    printfn "Get grayscaled pixels"
    let colorMap =
        getColorMap (bitmap.Width, bitmap.Height) ccircles

    printfn "Get recolored pixels"
    let points =
        ccircles
        |> Array.Parallel.map (fun (Circle (x, y, _), color) ->
            (int x, int y), color)
    let calcDistance point =
        points
        |> Array.minBy (fst >> distance point)
    let recoloredPixels =
        originalPixels
        |> recolorPixels calcDistance colorMap

    printfn "Recolor bitmap"
    recoloredPixels
    |> Array.iter (fun (x, y, color) ->
        bitmap.SetPixel(x, y, color))

    use image = SKImage.FromBitmap(bitmap)

    use data = image.Encode(SKEncodedImageFormat.Png, 100)
    let path = IO.Path.GetFullPath("./circles.png")
    use stream = IO.File.OpenWrite(path)
    data.SaveTo(stream)
    printfn "File written to: %s" path

    0
