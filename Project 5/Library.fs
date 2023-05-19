//
// F# image processing functions.
//
// The library file contains 5 public functions that perform 5
// unique transformations to an image respectivly. Each function
// takes an encoding of an image as a list of lists of 3-int tuples
// and outputs the same type representing the transformation of
// the original. The first function is grayscale which will
// transform the original image to a black and white image.
// The second is threshold which will change each pixel's RGB value
// to either its maximum or minimum based on a threshold argument.
// The third function is flip_horizontal which will simply flip the
// image across the vertical axis bisecting the image. The fourth
// function is edge detect which will compare each pixel to its
// neighbors, determing if it is an edge pixel in the image. If so
// the pixel will be set to a black pixel, otherwise it will be set
// to a white pixel. The last function is right-rotate-90 which will
// simply rotate the image by 90 degrees clockwise.
//
//   Jordan Fanapour
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace ImageLibrary

module Operations =
  //
  // all functions must be indented
  //

  //
  // Input tuple of rgb pixel
  // outputs a pixel the weighted gray pixel of input
  //
  let private getGrayscale (pixel:int*int*int) =
    let (r,g,b) = pixel
    let gray = int (float (r) * 0.299 + float (g) * 0.587 + float (b) * 0.114)
    (gray, gray, gray)

  //
  // Tail recursion for Grayscale
  //
  let rec private GrayscaleRec (image:(int*int*int) list list) (result) =
    match image with
      | [] -> (List.rev result)
      | hd::tl -> GrayscaleRec (tl) ((List.map getGrayscale hd)::result)

  //
  // Grayscale:
  //
  // Converts the image into grayscale and returns the 
  // resulting image as a list of lists. Pixels in grayscale
  // have the same value for each of the Red, Green and Blue
  // values in the RGB value.  Conversion to grayscale is done
  // by using a WEIGHTED AVERAGE calculation.  A normal average
  // (adding the three values and dividing by 3) is not the best,
  // since the human eye does not perceive the brightness of 
  // red, green and blue the same.  The human eye perceives 
  // green as brighter than red and it perceived red as brighter
  // than blue.  Research has shown that the following weighted
  // values should be used when calculating grayscale.
  //  - the green value should account for 58.7% of the grayscale.
  //  - the red value should account for   29.9% of the grayscale.
  //  - the blue value should account for  11.4% of the grayscale.
  //
  // So if the RGB values were (25, 75, 250), the grayscale amount 
  // would be 80, (25 * 0.299 + 75 * 0.587 + 250 * 0.114 => 80)
  // and then all three RGB values would become 80 or (80, 80, 80).
  // We will use truncation to cast from the floating point result 
  // to the integer grayscale value.
  //
  // Returns: updated image.
  //
  let rec Grayscale (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) = 
    GrayscaleRec image []

  //
  // Compares value to threshold. If value is greater return
  // depth, otherwise return 0
  // 
  let private compareThreshold value threshold depth =
    if value > threshold then
      depth
    else
      0

  //
  // Input tuple of rgb pixel and threshold
  // outputs a pixel that according to threshold
  //
  let private getThreshold (t) (d) (pixel:int*int*int) =
    let (r,g,b) = pixel
    let r1 = compareThreshold r t d
    let g1 = compareThreshold g t d
    let b1 = compareThreshold b t d
    (r1, g1, b1)

  //
  // Tail recursion for Threshold
  //
  let rec private ThresholdRec (image:(int*int*int) list list) (t) (d) (r) =
    match image with
      | [] -> (List.rev r)
      | hd::tl -> ThresholdRec (tl)(t)(d)((List.map(getThreshold t d) (hd))::r)
  
  //
  // Threshold
  //
  // Thresholding increases image separation --- dark values 
  // become darker and light values become lighter. Given a 
  // threshold value in the range 0 < threshold < color depth,
  // each RGB value is compared to see if it's > threshold.
  // If so, that RGB value is replaced by the color depth;
  // if not, that RGB value is replaced with 0. 
  //
  // Example: if threshold is 100 and depth is 255, then given 
  // a pixel (80, 120, 160), the new pixel is (0, 255, 255).
  //
  // Returns: updated image.
  //
  let rec Threshold (width:int) 
                    (height:int)
                    (depth:int)
                    (image:(int*int*int) list list)
                    (threshold:int) = 
    ThresholdRec image threshold depth []

  //
  // Tail recursion for FlipHorizontal
  //
  let rec private FlipHorizontalRec (image:(int*int*int) list list) r =
    match image with
      | [] -> (List.rev r)
      | hd::tl -> FlipHorizontalRec tl ((List.rev hd)::r)
  
  //
  // FlipHorizontal:
  //
  // Flips an image so that what’s on the left is now on 
  // the right, and what’s on the right is now on the left. 
  // That is, the pixel that is on the far left end of the
  // row ends up on the far right of the row, and the pixel
  // on the far right ends up on the far left. This is 
  // repeated as you move inwards toward the row's center.
  //
  // Returns: updated image.
  //
  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 
    FlipHorizontalRec image []

  //
  // Returns the pathagorean distance between two pixels' colors
  //
  let private getColorDifference (p1:int*int*int) (p2:int*int*int) =
    let (a1,b1,c1) = p1
    let (a2,b2,c2) = p2
    let x1 = float a1
    let y1 = float b1
    let z1 = float c1
    let x2 = float a2
    let y2 = float b2
    let z2 = float c2
    sqrt ((x1-x2)**2. + (y1-y2)**2. + (z1-z2)**2.)

  //
  // If the color difference between p1 and p2 
  // or p1 and p3 is greater than the threshold,
  // a black pixel will be the output otherwise
  // a white pixel will be the output
  //
  let private isEdge p1 p2 p3 threshold depth =
    let colorDiff1 = getColorDifference p1 p2
    let colorDiff2 = getColorDifference p1 p3
    let thres = float(threshold)
    if colorDiff1 > thres || colorDiff2 > thres then
      (0,0,0)
    else
      (depth, depth, depth)

  //
  // This function will take the current row being processed
  // (row), the row below it in order to compare pixels below
  // the current row (nextRow), the threshold value (t),
  // the depth of the pixel (d), and a tail recursion result
  // list r. This function will compare every pixel in row
  // to its neighboring pixel to the right and below it and
  // determine if it is an edge pixel using the function above
  // isEdge.
  //
  let rec private processRow row nextRow t d r =
    match row with
      | [] -> (List.rev r)
      | hd::[] -> (List.rev r)
      | hd::tl -> processRow (tl)(List.tail (nextRow))(t)(d) ((isEdge (hd)(List.head tl)(List.head nextRow)(t)(d))::r)

  //
  // Tail recursion for EdgeDetect
  //
  let rec private EdgeDetectRec (image:(int*int*int) list list) t d r =
    match image with
      | [] -> (List.rev r)
      | hd::[] -> (List.rev r)
      | hd::tl -> EdgeDetectRec (tl)(t)(d)
                                ((processRow (hd)(List.head tl)(t)(d)([]))::r)

  //
  // Edge Detection:
  //
  // Edge detection is an algorithm used in computer vision to help
  // distinguish different objects in a picture or to distinguish an
  // object in the foreground of the picture from the background.
  //
  // Edge Detection replaces each pixel in the original image with
  // a black pixel, (0, 0, 0), if the original pixel contains an 
  // "edge" in the original image.  If the original pixel does not
  // contain an edge, the pixel is replaced with a white pixel 
  // (255, 255, 255).
  //
  // An edge occurs when the color of pixel is "signigicantly different"
  // when compared to the color of two of its neighboring pixels. 
  // We only compares each pixel in the image with the 
  // pixel immediately to the right of it and with the pixel
  // immediately below it. If either pixel has a color difference
  // greater than a given threshold, then it is "significantly
  // different" and an edge occurs. Note that the right-most column
  // of pixels and the bottom-most column of pixels can not perform
  // this calculation so the final image contain one less column
  // and one less row than the original image.
  //
  // To calculate the "color difference" between two pixels, we
  // treat the each pixel as a point on a 3-dimensional grid and
  // we calculate the distance between the two points using the
  // 3-dimensional extension to the Pythagorean Theorem.
  // Distance between (x1, y1, z1) and (x2, y2, z2) is
  //  sqrt ( (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2 )
  //
  // The threshold amount will need to be given, which is an 
  // integer 0 < threshold < 255.  If the color distance between
  // the original pixel either of the two neighboring pixels 
  // is greater than the threshold amount, an edge occurs and 
  // a black pixel is put in the resulting image at the location
  // of the original pixel. 
  //
  // Returns: updated image.
  //
  let rec EdgeDetect (width:int)
               (height:int)
               (depth:int)
               (image:(int*int*int) list list)
               (threshold:int) = 
    EdgeDetectRec image threshold depth []

  //
  // Gets the ith index of the list L
  //
  let rec private getIndex L i =
    match i with
      | 0 -> List.head L
      | _ -> getIndex (List.tail L) (i-1)

  //
  // Gets the ith column of image
  //
  let rec private getCol (image:(int*int*int) list list) i result =
    match image with
      | [] -> List.rev result
      | hd::tl -> getCol (tl) (i) ((getIndex (hd) (i))::result)

  //
  // Tail recursion for RotateRight90
  //
  let rec private RR90Rec (image:(int*int*int) list list) w result =
    match w with
      | -1 -> result
      | _ -> RR90Rec (image) (w-1) ((List.rev (getCol image w []))::result)

  //
  // RotateRight90:
  //
  // Rotates the image to the right 90 degrees.
  //
  // Returns: updated image.
  //
  let rec RotateRight90 (width:int)
                        (height:int)
                        (depth:int)
                        (image:(int*int*int) list list) = 
    RR90Rec (image) (width-1) ([])

