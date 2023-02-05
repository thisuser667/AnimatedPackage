unit encodewebp124nodelay;
// Copyright 2010 Google Inc.
//
// This code is licensed under the same terms as WebM:
//  Software License Agreement:  http://www.webmproject.org/license/software/
//  Additional IP Rights Grant:  http://www.webmproject.org/license/additional/
//
//  Original Delphi API by Henri Gourvest <hgourvest@gmail.com>
//  Updated by thisuser667

// -----------------------------------------------------------------------------
{$IFDEF FPC}
{$mode objfpc}
{$H+}
{$ENDIF}


{$ALIGN ON}
{$MINENUMSIZE 4}

interface

{$IFNDEF FPC and IFDEF WINDOWS}
  {$DEFINE NOFPCW}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

//-----------------------------------------------------------------------------
type
// Output colorspaces
  WEBP_CSP_MODE = (
    MODE_RGB = 0, MODE_RGBA = 1,
  MODE_BGR = 2, MODE_BGRA = 3,
  MODE_ARGB = 4, MODE_RGBA_4444 = 5,
  MODE_RGB_565 = 6,
  // RGB-premultiplied transparent modes (alpha value is preserved)
  MODE_rgbATr = 7,
  MODE_bgrATr = 8,
  MODE_ArgbTr = 9,
  MODE_rgbATr_4444 = 10,
  // YUV modes must come after RGB ones.
  MODE_YUV = 11, MODE_YUVA = 12,  // yuv 4:2:0
  MODE_LAST = 13);

  // Coding parameters

  TWebPImageHint = (
    WEBP_HINT_DEFAULT = 0,  // default preset.
    WEBP_HINT_PICTURE,      // digital picture, like portrait, inner shot
    WEBP_HINT_PHOTO,        // outdoor photograph, with natural lighting
    WEBP_HINT_GRAPH,        // Discrete tone image (graph, map-tile etc).
    WEBP_HINT_LAST
    );

  PWebpEncoderConfig = ^TWebpEncoderConfig;
  TWebpEncoderConfig = record
    lossless: Integer;           // Lossless encoding (0=lossy(default), 1=lossless).
    quality: Single;          // between 0 and 100. For lossy, 0 gives the smallest
                            // size and 100 the largest. For lossless, this
                            // parameter is the amount of effort put into the
                            // compression: 0 is the fastest but gives larger
                            // files compared to the slowest, but best, 100.
    method: Integer;             // quality/speed trade-off (0=fast, 6=slower-better)

    image_hint: TWebpImageHint;  // Hint for image type (lossless only for now).

    target_size: Integer;        // if non-zero, set the desired target size in bytes.
                            // Takes precedence over the 'compression' parameter.
    target_PSNR: Single;      // if non-zero, specifies the minimal distortion to
                            // try to achieve. Takes precedence over target_size.
    segments: Integer;           // maximum number of segments to use, in [1..4]
    sns_strength: Integer;       // Spatial Noise Shaping. 0=off, 100=maximum.
    filter_strength: Integer;    // range: [0 = off .. 100 = strongest]
    filter_sharpness: Integer;   // range: [0 = off .. 7 = least sharp]
    filter_type: integer;        // filtering type: 0 = simple, 1 = strong (only used
                            // if filter_strength > 0 or autofilter > 0)
    autofilter: integer;         // Auto adjust filter's strength [0 = off, 1 = on]
    alpha_compression: integer;  // Algorithm for encoding the alpha plane (0 = none,
                            // 1 = compressed with WebP lossless). Default is 1.
    alpha_filtering: integer;    // Predictive filtering method for alpha plane.
                            //  0: none, 1: fast, 2: best. Default if 1.
    alpha_quality: integer;      // Between 0 (smallest size) and 100 (lossless).
                            // Default is 100.
    pass: integer;               // number of entropy-analysis passes (in [1..10]).

    show_compressed: integer;    // if true, export the compressed picture back.
                            // In-loop filtering is not applied.
    preprocessing: integer;      // preprocessing filter:
                            // 0=none, 1=segment-smooth, 2=pseudo-random dithering
    partitions: integer;         // log2(number of token partitions) in [0..3]. Default
                            // is set to 0 for easier progressive decoding.
    partition_limit: integer;    // quality degradation allowed to fit the 512k limit
                            // on prediction modes coding (0: no degradation,
                            // 100: maximum possible degradation).
    emulate_jpeg_size: integer;  // If true, compression parameters will be remapped
                            // to better match the expected output size from
                            // JPEG compression. Generally, the output size will
                            // be similar but the degradation will be lower.
    thread_level: integer;       // If non-zero, try and use multi-threaded encoding.
    low_memory: integer;         // If set, reduce memory usage (but increase CPU use).

    near_lossless: integer;      // Near lossless encoding [0 = max loss .. 100 = off
                            // (default)].
    exact: integer;              // if non-zero, preserve the exact RGB values under
                            // transparent area. Otherwise, discard this invisible
                            // RGB information for better compression. The default
                            // value is 0.

    use_delta_palette: integer;  // reserved for future lossless feature
    use_sharp_yuv: integer;      // if needed, use sharp (and slow) RGB->YUV conversion

    qmin: integer;               // minimum permissible quality factor
    qmax: integer;               // maximum permissible quality factor
  end;
  // Image characteristics hint for the underlying encoder.
// Enumerate some predefined settings for WebpEncoderConfig, depending on the type
// of source picture. These presets are used when calling WebPConfigPreset().
  TWebPPreset = (
    WEBP_PRESET_DEFAULT = 0,  // default preset.
    WEBP_PRESET_PICTURE,      // digital picture, like portrait, inner shot
    WEBP_PRESET_PHOTO,        // outdoor photograph, with natural lighting
    WEBP_PRESET_DRAWING,      // hand or line drawing, with high-contrast details
    WEBP_PRESET_ICON,         // small-sized colorful images
    WEBP_PRESET_TEXT          // text-like
  );
  TWebPEncCSP = (// chroma sampling
    WEBP_YUV420  = 0,        // 4:2:0
    WEBP_CSP_UV_MASK = 3,    // bit-mask to get the UV sampling factors
    WEBP_YUV420A = 4        // alpha channel variant
    //WEBP_CSP_ALPHA_BIT = 4 [NOP= webp_csp_alpha_bit not used in this code]
    );

  TWebPEncodingError = (
    VP8_ENC_OK = 0,
    VP8_ENC_ERROR_OUT_OF_MEMORY,            // memory error allocating objects
    VP8_ENC_ERROR_BITSTREAM_OUT_OF_MEMORY,  // memory error while flushing bits
    VP8_ENC_ERROR_NULL_PARAMETER,           // a pointer parameter is NULL
    VP8_ENC_ERROR_INVALID_CONFIGURATION,    // configuration is invalid
    VP8_ENC_ERROR_BAD_DIMENSION,            // picture has invalid width/height
    VP8_ENC_ERROR_PARTITION0_OVERFLOW,      // partition is bigger than 512k
    VP8_ENC_ERROR_PARTITION_OVERFLOW,       // partition is bigger than 16M
    VP8_ENC_ERROR_BAD_WRITE,                // error while flushing bytes
    VP8_ENC_ERROR_FILE_TOO_BIG,             // file is bigger than 4G
    VP8_ENC_ERROR_USER_ABORT,               // abort request by user
    VP8_ENC_ERROR_LAST                      // list terminator. always last.
    );

  PWebpPicture = ^TWebpPicture;  // needed class convertion to forwarding
  TWebpProgressEvent = function(percent: Integer;
    const picture: PWebpPicture): Integer; cdecl;
// Signature for output function. Should return true if writing was successful.
// data/data_size is the segment of data to write, and 'picture' is for
// reference (and so one can make use of picture->custom_ptr).
  TWebPWriterFunction = function(const data: PByte; data_size: Cardinal;
    const picture: PWebPPicture): Integer; cdecl;
  PWebPAuxStats = ^TWebPAuxStats;
  TWebPAuxStats = record
    coded_size: Integer;         // final size

    PSNR: array[0..4] of Single;          // peak-signal-to-noise ratio for Y/U/V/All/Alpha
    block_count: array[0..2] of Integer;     // number of intra4/intra16/skipped macroblocks
    header_bytes: array[0..1] of Integer;    // approximate number of bytes spent for header
                            // and mode-partition #0
    residual_bytes: array[0..2] of array[0..3] of Integer;  // approximate number of bytes spent for
                               // DC/AC/uv coefficients for each (0..3) segments.
    segment_size: array[0..3] of Integer;    // number of macroblocks in each segments
    segment_quant: array[0..3] of Integer;   // quantizer values for each segments
    segment_level: array[0..3] of Integer;   // filtering strength for each segments [0..63]

    alpha_data_size: Integer;    // size of the transparency data
    layer_data_size: Integer;    // size of the enhancement layer data

    // lossless encoder statistics
    lossless_features: Cardinal;  // bit0:predictor bit1:cross-color transform
                                 // bit2:subtract-green bit3:color indexing
    histogram_bits: Integer;          // number of precision bits of histogram
    transform_bits: Integer;          // precision bits for transform
    cache_bits: Integer;              // number of bits for color cache lookup
    palette_size: Integer;            // number of color in palette, if used
    lossless_size: Integer;           // final lossless size
    lossless_hdr_size: Integer;       // lossless header (transform, huffman etc) size
    lossless_data_size: Integer;      // lossless image data size

    pad: array[0..1] of Cardinal;
  end;
  TWebPPicture = record
    //   INPUT
    //////////////
    // Main flag for encoder selecting between ARGB or YUV input.
    // It is recommended to use ARGB input (*argb, argb_stride) for lossless
    // compression, and YUV input (*y, *u, *v, etc.) for lossy compression
    // since these are the respective native colorspace for these formats.
    use_argb: Integer;

    // YUV input (mostly used for input to lossy compression)
    colorspace: TWebPEncCSP;     // colorspace: should be YUV420 for now (=Y'CbCr).
    width, height: Integer;         // dimensions (less or equal to WEBP_MAX_DIMENSION)
    y, u, v: PByte;        // pointers to luma/chroma planes.
    y_stride, uv_stride: Integer;   // luma/chroma strides.
    a: PByte;                // pointer to the alpha plane
    a_stride: Integer;              // stride of the alpha plane
    pad1: array[0..1] of Cardinal;          // padding for later use

    // ARGB input (mostly used for input to lossless compression)
    argb: PCardinal;            // Pointer to argb (32 bit) plane.
    argb_stride: Integer;           // This is stride in pixels units, not bytes.
    pad2: array[0..2] of Cardinal;          // padding for later use

    //   OUTPUT
    ///////////////
    // Byte-emission hook, to store compressed bytes as they are ready.
    writer: TWebPWriterFunction;  // can be NULL
    custom_ptr: Pointer;           // can be used by the writer.

    // map for extra information (only for lossy compression mode)
    extra_info_type: Integer;    // 1: intra type, 2: segment, 3: quant
                            // 4: intra-16 prediction mode,
                            // 5: chroma prediction mode,
                            // 6: bit cost, 7: distortion
    extra_info: PByte;    // if not NULL, points to an array of size
                            // ((width + 15) / 16) * ((height + 15) / 16) that
                            // will be filled with a macroblock map, depending
                            // on extra_info_type.

    //   STATS AND REPORTS
    ///////////////////////////
    // Pointer to side statistics (updated only if not NULL)
    stats: PWebPAuxStats;

    // Error code for the latest error encountered during encoding
    error_code: TWebPEncodingError;

    // If not NULL, report progress during encoding.
    progress_hook: TWebpProgressEvent;

    user_data: Pointer;        // this field is free to be set to any value and
                            // used during callbacks (like progress-report e.g.).

    pad3: array[0..2] of Cardinal;       // padding for later use

    // Unused for now
    pad4, pad5: PByte;
    pad6: array[0..7] of Cardinal;       // padding for later use

    // PRIVATE FIELDS
    ////////////////////
    memory_: Pointer;          // row chunk of memory for yuva planes
    memory_argb_: Pointer;     // and for argb too.
    pad7: array[0..1] of Pointer;          // padding for later use
  end;

  //TWebPPicture = record end; // main structure for I/O
  // non-essential structure for storing auxilliary statistics
  // Signature for output function. Should return 1 if writing was successful.
  // data/data_size is the segment of data to write, and 'picture' is for
  // reference (and so one can make use of picture->custom_ptr).

  // WebPMemoryWrite: a special WebPWriterFunction that writes to memory using
  // the following WebPMemoryWriter object (to be set as a custom_ptr).
  TWebPMemoryWriter = record
    mem: PByte;       // final buffer (of size 'max_size', larger than 'size').
    size: Cardinal;      // final size
    max_size: Cardinal;  // total capacity
    pad: array[0..0] of Cardinal;    // padding for later use
  end;
  PWebpMemoryWriter = ^TWebpMemoryWriter;

  // The following must be called first before any use.
  // Encoding error conditions.
  // maximum width/height allowed (inclusive), in pixels
  const
    WEBP_MAX_DIMENSION = 16383;

type
(******************************************************************************
  WebP encoder: main interface
 ******************************************************************************)
// Return the encoder's version number, packed in hexadecimal using 8bits for
// each of major/minor/revision. E.g: v2.5.7 is 0x020507.
  TWebPGetEncoderVersion = function: Integer; cdecl;
//-----------------------------------------------------------------------------
// One-stop-shop call! No questions asked:
// Returns the size of the compressed data (pointed to by *output), or 0 if
// an error occurred. The compressed data must be released by the caller
// using the call 'WebPFree(output)'.
// Currently, alpha values are discarded.
  TWebPEncodeRGB = function(const rgb: PByte; width, height, stride: Integer;
    quality_factor: single; var output: PByte): Cardinal; cdecl;
  TWebPEncodeBGR = function(const bgr: PByte; width, height, stride: Integer;
    quality_factor: Single; var output: PByte): Cardinal; cdecl;
  TWebPEncodeRGBA = function(const rgba: PByte; width, height, stride: Integer;
  quality_factor: Single; var output: PByte): Cardinal; cdecl;
  TWebPEncodeBGRA = function(const bgra: PByte; width, height, stride: Integer;
  quality_factor: Single; var output: PByte): Cardinal; cdecl;

// Note these functions, like the lossy versions, use the library's default
// settings. For lossless this means 'exact' is disabled. RGB values in
// transparent areas will be modified to improve compression.
  TWebPEncodeLosslessRGB = function(const bgra: PByte; width, height, stride: Integer;
  var output: PByte): Cardinal; cdecl;
  TWebPEncodeLosslessBGR = function(const bgra: PByte; width, height, stride: Integer;
  var output: PByte): Cardinal; cdecl;
  TWebPEncodeLosslessRGBA = function(const bgra: PByte; width, height, stride: Integer;
  var output: PByte): Cardinal; cdecl;
  TWebPEncodeLosslessBGRA = function(const bgra: PByte; width, height, stride: Integer;
  var output: PByte): Cardinal; cdecl;

// Should always be called, to initialize a fresh WebPConfig structure before
// modification. Returns 0 in case of version mismatch. WebPConfigInit() must
// have succeeded before using the 'config' object.
  TWebPConfigInit = function(const config: PWebpEncoderConfig): Integer;

// This function will initialize the configuration according to a predefined
// set of parameters (referred to by 'preset') and a given quality factor.
// This functioncan be called as a replacement to WebPConfigInit(). Will
// return 0 in case of error.
  TWebPConfigPreset = function(const config: PWebpEncoderConfig; preset: TWebPPreset;
  quality: Single): Integer;

// Returns 1 if all parameters are in valid range and the configuration is OK.
  TWebPValidateConfig = function(const config: PWebpEncoderConfig): Integer; cdecl;

// Should always be called, to initialize the structure. Returns 0 in case of
// version mismatch. WebPPictureInit() must have succeeded before using the
// 'picture' object.
  TWebPPictureInit = function(const picture: PWebPPicture): Integer;
//-----------------------------------------------------------------------------
// WebPPicture utils
// Convenience allocation / deallocation based on picture->width/height:
// Allocate y/u/v buffers as per width/height specification.
// Note! This function will free the previous buffer if needed.
// Returns 0 in case of memory error.
  TWebPPictureAlloc = function(const picture: PWebPPicture): Integer; cdecl;

// Release memory allocated by WebPPictureAlloc() or WebPPictureImport*()
// Note that this function does _not_ free the memory pointed to by 'picture'.
  TWebPPictureFree = procedure(const picture: PWebPPicture); cdecl;

// Copy the pixels of *src into *dst, using WebPPictureAlloc.
// Returns 0 in case of memory allocation error.
  TWebPPictureCopy = function(const src, dst: PWebPPicture): Integer; cdecl;

// self-crops a picture to the rectangle defined by top/left/width/height.
// Returns 0 in case of memory allocation error, or if the rectangle is
// outside of the source picture.
  TWebPPictureCrop = function(const picture: PWebPPicture;
  left, top, width, height: Integer): Integer; cdecl;

// Colorspace conversion function. Previous buffer will be free'd, if any.
// *rgb buffer should have a size of at least height * rgb_stride.
// Returns 0 in case of memory error.
  TWebPPictureImportRGB = function(const picture: PWebPPicture;
  const rgb: PByte; rgb_stride: Integer): Integer; cdecl;

// Same, but for RGBA buffer. Alpha information is ignored.
  TWebPPictureImportRGBA = function(const picture: PWebPPicture;
  const rgba: PByte; rgba_stride: Integer): Integer; cdecl;

// Variant of the above, but taking BGR input:
  TWebPPictureImportBGR = function(const picture: PWebPPicture;
  const bgr: PByte; bgr_stride: Integer): Integer; cdecl;
  TWebPPictureImportBGRA = function(const picture: PWebPPicture;
  const bgra: PByte; bgra_stride: Integer): Integer; cdecl;

//-----------------------------------------------------------------------------
// Main call
// Main encoding call, after config and picture have been initialiazed.
// 'picture' must be less than 16384x16384 in dimension, and the 'config' object
// must be a valid one.
// Returns false in case of error, true otherwise.
  TWebPEncode = function(const config: PWebpEncoderConfig; const picture: PWebPPicture): Integer; cdecl;

  TWebPMemoryWriterInit = procedure(writer: PWebpMemoryWriter); cdecl;

  // The following must be called to deallocate writer->mem memory. The 'writer'
  // object itself is not deallocated.
  TWebPMemoryWriterClear = procedure(writer: PWebpMemoryWriter); cdecl;

  // The custom writer to be used with WebPMemoryWriter as custom_ptr. Upon
  // completion, writer.mem and writer.size will hold the coded data.
  // writer.mem must be freed by calling WebPMemoryWriterClear.
  TWebPMemoryWrite = function(const data: PByte; data_size: Cardinal;
    const picture: PWebpPicture): Integer; cdecl;
  TWebPConfigInitInternal = function(const conf: PWebpEncoderConfig; preset: TWebPPreset;
    quality: single; version: Integer): Integer; cdecl;
  TWebPPictureInitInternal = function(const picture: PWebPPicture; version: Integer): Integer; cdecl;

var
  WebpGetEncoderVersion: TWebpGetEncoderVersion;
  WebpEncodeRGB: TWebpEncodeRGB;
  WebpEncodeBGR: TWebpEncodeBGR;
  WebpEncodeRGBA: TWebpEncodeRGBA;
  WebpEncodeBGRA: TWebpEncodeBGRA;
  WebpEncodeLosslessRGB: TWebpEncodeLosslessRGB;
  WebpEncodeLosslessBGR: TWebpEncodeLosslessBGR;
  WebpEncodeLosslessRGBA: TWebpEncodeLosslessRGBA;
  WebpEncodeLosslessBGRA: TWebpEncodeLosslessBGRA;
  WebpValidateConfig: TWebpValidateConfig;
  WebpPictureAlloc: TWebpPictureAlloc;
  WebpPictureFree: TWebpPictureFree;
  WebpPictureCopy: TWebpPictureCopy;
  WebpPictureCrop: TWebpPictureCrop;
  WebpPictureImportRGB: TWebpPictureImportRGB;
  WebpPictureImportRGBA: TWebpPictureImportRGBA;
  WebpPictureImportBGR: TWebpPictureImportBGR;
  WebpPictureImportBGRA: TWebpPictureImportBGRA;
  WebPEncode: TWebpEncode;
  WebpMemoryWriterInit: TWebpMemoryWriterInit;
  WebPMemoryWriterClear: TWebPMemoryWriterClear;
  WebpMemoryWrite: TWebpMemoryWrite;
  WebpConfigInitInternal: function(const conf: PWebpEncoderConfig; preset: TWebPPreset;
    quality: single; version: Integer): Integer; cdecl;
  WebpPictureInitInternal: TWebpPictureInitInternal;

// Internal, version-checked, entry point

function WebPConfigInit(const config: PWebpEncoderConfig): Integer;
function WebPConfigPreset(const config: PWebpEncoderConfig; preset: TWebPPreset;
  quality: Single): Integer;
function WebPPictureInit(const picture: PWebPPicture): Integer;

implementation
const
  WEBP_ENCODER_ABI_VERSION = $020F;

function WebPConfigInit(const config: PWebpEncoderConfig): Integer;
begin
  Result := WebPConfigInitInternal(config, WEBP_PRESET_DEFAULT, 75.0,  WEBP_ENCODER_ABI_VERSION);
end;

function WebPConfigPreset(const config: PWebpEncoderConfig; preset: TWebPPreset;
  quality: Single): Integer;
begin
  Result := WebPConfigInitInternal(config, preset, quality, WEBP_ENCODER_ABI_VERSION);
end;

function WebPPictureInit(const picture: PWebPPicture): Integer;
begin
  Result := WebPPictureInitInternal(picture, WEBP_ENCODER_ABI_VERSION);
end;

end.
