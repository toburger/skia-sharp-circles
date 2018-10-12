module ImageProcessing

open System
open System.Threading.Tasks
open SkiaSharp

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
        (alpha: byte)
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
            let alpha = byte ((int grayscale * int alpha) / 255)
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
let getColorMap (width, height) radius ccircles: SKBitmap =
    let info = SKImageInfo(width, height)
    use surface = SKSurface.Create(info)
    use canvas = surface.Canvas
    canvas.Clear(SKColors.Black)
    for (x, y), _ in ccircles do
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
