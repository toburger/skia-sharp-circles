open System
open System.Threading.Tasks
open SkiaSharp

/// The radius used for all circles
let radius = 80

/// Scale factor for all circles
let scale = 8

type Circle = Circle of x: int * y: int

/// Used to read input from JSON.
type Input = { id: string; x: string; y: string }

/// Creates a random 'light' color.
let rndColor =
    let rnd = Random()
    let next () = byte (rnd.Next(127, 256))
    fun () -> SKColor(next (), next (), next (), 255uy)

/// Calculate distance between two points.
let distance (mmx: int, mmy: int) (ox: int, oy: int) =
    Math.Sqrt(float ((mmx-ox)*(mmx-ox)+(mmy-oy)*(mmy-oy)))

/// Blend two colors with alpha together.
let blend (a: SKColor) (b: SKColor) =
    let aA, rA, gA, bA = int a.Alpha, int a.Red, int a.Green, int a.Blue
    let aB, rB, gB, bB = int b.Alpha, int b.Red, int b.Green, int b.Blue
    let rOut = (rA * aA / 255) + (rB * aB * (255 - aA) / (255 * 255))
    let gOut = (gA * aA / 255) + (gB * aB * (255 - aA) / (255 * 255))
    let bOut = (bA * aA / 255) + (bB * aB * (255 - aA) / (255 * 255))
    let aOut = aA + (aB * (255 - aA) / 255)
    SKColor(byte rOut, byte gOut, byte bOut, byte aOut)

/// <summary>Recolors a bitmap based on an overlay bitmap.</summary>
/// <remarks>
/// The overlay bitmap is encoded as a gray color map (for antialising),
/// where black is ignored and every grayish/white color gets blended into the
/// original bitmap.
/// </remarks>
/// <param name="getClosesColor">Is used to determine a new color of a pixel.</param>
/// <param name="overlay">The grayscale bitmap which gets blended.</param>
/// <param name="getClosesColor">The original bitmap to modify</param>
let recolorPixels
        getClosestColor
        (overlay: SKBitmap)
        (original: SKBitmap): unit =
    let inline updatePixel x y =
        let color = original.GetPixel(x, y)
        let grayColor = overlay.GetPixel(x, y)
        /// Calculate the grayscale from Color.
        let grayscale =
           byte
            ((int grayColor.Red +
              int grayColor.Green +
              int grayColor.Blue) / 3)
        /// If grayscale not black...
        if grayscale > 0uy then
            /// Find the closest color.
            let (ncolor: SKColor) = getClosestColor (x, y)
            /// Make it half transparent.
            let alpha = grayscale / 2uy
            /// Apply alpha to the closest color.
            let ncolor = ncolor.WithAlpha(alpha)
            original.SetPixel(x, y, blend ncolor color)
        else ()
    /// Executes in parallel.
    Parallel.For(0, original.Width, (fun x ->
        for y = 0 to original.Height - 1 do
            updatePixel x y))
    |> ignore

/// Creates a grayscale color map.
let getColorMap (width, height) ccircles: SKBitmap =
    let info = SKImageInfo(width, height)
    use surface = SKSurface.Create(info)
    let canvas = surface.Canvas
    canvas.Clear(SKColors.Black)
    for Circle (x, y), _ in ccircles do
        let color = SKColors.White
        use paint =
            new SKPaint(
                IsAntialias = true,
                Color = color,
                FilterQuality = SKFilterQuality.High
            )
        canvas.DrawCircle(
            SKPoint(float32 x, float32 y),
            float32 radius,
            paint
        )
    use image = surface.Snapshot()
    SKBitmap.FromImage(image)

[<EntryPoint>]
let main _ =
    /// Array of circles with a random color.
    /// Imports the circles from a JSON file.
    let ccircles =
        IO.File.ReadAllText("./circles.json")
        |> Newtonsoft.Json.JsonConvert.DeserializeObject<Input array>
        |> Array.map (fun i ->
            let color = rndColor ()
            let x = int i.x * scale
            let y = int i.y * scale
            Circle(x, y), color)

    use original = SKBitmap.Decode("./burgstall.jpg")

    printfn "Get overlay pixels"
    use overlay =
        getColorMap
            (original.Width, original.Height)
            ccircles

    printfn "Set recolored pixels"
    /// Convert array of circles and colors to an array
    /// of points and colors for convenience.
    let points =
        ccircles
        |> Array.map (fun (Circle (x, y), color) ->
            (x, y), color)
    /// Function used to find the closest point and use its color.
    let getClosestColor point =
        points
        |> Array.minBy (fst >> distance point)
        |> snd

    /// Rewrite pixels of the original bitmap.
    recolorPixels
        getClosestColor
        overlay
        original

    /// Write the new bitmap to HD.
    use image = SKImage.FromBitmap(original)
    use data = image.Encode(SKEncodedImageFormat.Png, 100)
    let path = IO.Path.GetFullPath("./circles.png")
    use stream = IO.File.OpenWrite(path)
    data.SaveTo(stream)
    printfn "File written to: %s" path

    0
