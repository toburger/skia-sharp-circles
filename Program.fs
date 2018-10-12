open System
open System.Threading.Tasks
open SkiaSharp
open Newtonsoft.Json

let radius = 80

type Circle = Circle of x: int * y: int

type Input = { id: string; x: string; y: string }

let rndColor =
    let rnd = Random()
    let next () = byte (rnd.Next(127, 256))
    fun () -> SKColor(next (), next (), next (), 255uy)

let distance (mmx: int, mmy: int) (ox: int, oy: int) =
    Math.Sqrt(float ((mmx-ox)*(mmx-ox)+(mmy-oy)*(mmy-oy)))

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
        (overlay: SKBitmap)
        (original: SKBitmap): unit =
    let inline updatePixel x y =
        let color = original.GetPixel(x, y)
        let grayscale = overlay.GetPixel(x, y)
        let grayscale =
           byte ((int grayscale.Red + int grayscale.Green + int grayscale.Blue) / 3)
        if grayscale > 0uy then
            let _, (ncolor: SKColor) = calcDistance (x, y)
            let alpha = grayscale / 2uy
            let ncolor = ncolor.WithAlpha(alpha)
            original.SetPixel(x, y, blend ncolor color)
        else ()
    Parallel.For(0, original.Width, (fun x ->
        for y = 0 to original.Height - 1 do
            updatePixel x y))
    |> ignore

let getColorMap (width, height) ccircles: SKBitmap =
    let info = SKImageInfo(width, height)
    use surface = SKSurface.Create(info)
    let canvas = surface.Canvas
    canvas.Clear(SKColors.Black)
    for Circle (x, y), _ in ccircles do
        let color = SKColors.White
        use paint = new SKPaint(Color = color, FilterQuality = SKFilterQuality.High)
        canvas.DrawCircle(SKPoint(float32 x, float32 y), float32 radius, paint)
    use image = surface.Snapshot()
    SKBitmap.FromImage(image)

[<EntryPoint>]
let main _ =
    let ccircles =
        IO.File.ReadAllText("./circles.json")
        |> JsonConvert.DeserializeObject<Input array>
        |> Array.map (fun i ->
            let color = rndColor ()
            let x = int i.x * 8
            let y = int i.y * 8
            Circle(x, y), color)

    use original = SKBitmap.Decode("./burgstall.jpg")

    printfn "Get overlay pixels"
    use overlay =
        getColorMap (original.Width, original.Height) ccircles

    printfn "Set recolored pixels"
    let points =
        ccircles
        |> Array.map (fun (Circle (x, y), color) ->
            (int x, int y), color)
    let calcDistance point =
        points
        |> Array.minBy (fst >> distance point)
    original
    |> recolorPixels calcDistance overlay

    use image = SKImage.FromBitmap(original)

    use data = image.Encode(SKEncodedImageFormat.Png, 100)
    let path = IO.Path.GetFullPath("./circles.png")
    use stream = IO.File.OpenWrite(path)
    data.SaveTo(stream)
    printfn "File written to: %s" path

    0
