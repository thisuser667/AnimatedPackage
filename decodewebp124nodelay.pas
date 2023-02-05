unit decodewebp124nodelay;
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

{$IFNDEF FPC}
{$DEFINE WINDOWS}
{$ELSE}
//{$UNDEF WINDOWS} // uncomment this for fpc windows
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

// Enumeration of the status codes
  TVP8StatusCode = (
    VP8_STATUS_OK = 0,
    VP8_STATUS_OUT_OF_MEMORY,
    VP8_STATUS_INVALID_PARAM,
    VP8_STATUS_BITSTREAM_ERROR,
    VP8_STATUS_UNSUPPORTED_FEATURE,
    VP8_STATUS_SUSPENDED,
    VP8_STATUS_USER_ABORT,
    VP8_STATUS_NOT_ENOUGH_DATA
    {$IFNDEF NOFPCW}
    , VP8_BAD_LIBRARY
    {$ENDIF}
  );
  TDecState = (
    STATE_HEADER = 0,
    STATE_PARTS0 = 1,
    STATE_DATA = 2,
    STATE_DONE = 3,
    STATE_ERROR = 4);
  // Decoding output parameters.
  PWebPDecParams = ^TWebPDecParams;
  TWebPDecParams = record
    output: PByte;              // rgb(a) or luma
    u, v: PByte;                // chroma u/v
    top_y, top_u, top_v: PByte; // cache for the fancy upscaler
    stride: Integer;            // rgb(a) stride or luma stride
    u_stride: Integer;          // chroma-u stride
    v_stride: Integer;          // chroma-v stride
    mode: WEBP_CSP_MODE;        // rgb(a) or yuv
    last_y: Integer;            // coordinate of the line that was last output
    output_size: Integer;       // size of 'output' buffer
    output_u_size: Integer;     // size of 'u' buffer
    output_v_size: Integer;     // size of 'v' buffer
    external_buffer: Integer;   // If true, the output buffers are externally owned
  end;
  // Input / Output
  PVP8Io = ^VP8Io;
  VP8Io = record
    // set by VP8GetHeaders()
    width, height: Integer;    // picture dimensions, in pixels
    // set before calling put()
    mb_y: Integer;                  // position of the current rows (in pixels)
    mb_h: Integer;                  // number of rows in the sample
    y, u, v: PByte;                 // rows to copy (in yuv420 format)
    y_stride: Integer;              // row stride for luma
    uv_stride: Integer;             // row stride for chroma
    opaque: Pointer;              // user data
    // called when fresh samples are available. Currently, samples are in
    // YUV420 format, and can be up to width x 24 in size (depending on the
    // in-loop filtering level, e.g.). Should return false in case of error
    // or abort request.
    put: function(const io: PVP8Io): Integer; cdecl;
    // called just before starting to decode the blocks.
    // Should returns 0 in case of error.
    setup: function(io: PVP8Io): Integer; cdecl;
    // called just after block decoding is finished (or when an error occurred).
    teardown: procedure(const io: PVP8Io); cdecl;
    // this is a recommendation for the user-side yuv->rgb converter. This flag
    // is set when calling setup() hook and can be overwritten by it. It then
    // can be taken into consideration during the put() method.
    fancy_upscaling: Integer;
    // Input buffer.
    data_size: Cardinal;
    data: PByte;
    // If true, in-loop filtering will not be performed even if present in the
    // bitstream. Switching off filtering may speed up decoding at the expense
    // of more visible blocking. Note that output will also be non-compliant
    // with the VP8 specifications.
    bypass_filtering: Integer;
  end;
  // Main decoding object. This is an opaque structure.
  PVP8Decoder = ^VP8Decoder;
  VP8Decoder = record end;

//-----------------------------------------------------------------------------
// Features gathered from the bitstream
  TWebPBitstreamFeatures = record
    width: Integer;          // Width in pixels, as read from the bitstream.
    height: Integer;         // Height in pixels, as read from the bitstream.
    has_alpha: Integer;      // True if the bitstream contains an alpha channel.
    has_animation: Integer;  // True if the bitstream is an animation.
    format: Integer;         // 0 = undefined (/mixed), 1 = lossy, 2 = lossless
    pad: array[0..4] of Cardinal;
  end;
  PWebPBitStreamFeatures= ^TWebPBitstreamFeatures;

  TWebpRGBABuffer = record
    rgba: PByte;    // pointer to RGBA samples
    stride: Integer;       // stride in bytes from one scanline to the next.
    size: NativeUint;
  end;

  TWebPYUVABuffer = record
    y, u, v: PByte;     // pointer to luma, chroma U/V, alpha samples
    y_stride: Integer;               // luma stride
    u_stride, v_stride: Integer;     // chroma strides
    a_stride: Integer;               // alpha stride
    y_size: NativeUint;              // luma plane size
    u_size, v_size: NativeUint;      // chroma planes size
    a_size: NativeUint;
  end;

  TGWebPDecodeBuffer = record  // to replace the C++ union
    RGBA: TWebPRGBABuffer;
    YuvSizeMinusRGBaSize: array[1..48 - 12] of Byte;
  end;

  TWebpDecBuffer = record
    colorspace: WEBP_CSP_MODE ;  // Colorspace.
    width, height: Integer;         // Dimensions.
    is_external_memory: Integer;    // If non-zero, 'internal_memory' pointer is not
                             // used. If value is '2' or more, the external
                             // memory is considered 'slow' and multiple
                             // read/write will be avoided.
    RGBA: TGWebPDecodeBuffer;    // replacing:
    {case Variant: Boolean of
      true: (RGBA: TWebPRGBABuffer);
      false: (YUVA: TWebPYUVABuffer);}
    pad: array[0..3] of Cardinal;
    private_memory: PByte;   // Internally allocated memory (only when
                             // is_external_memory is 0). Should not be used
  end;
  PWebpDecBuffer = ^TWebpDecBuffer;

  TWebpDecBufferYUVA = record
    colorspace: WEBP_CSP_MODE ;  // Colorspace.
    width, height: Integer;         // Dimensions.
    is_external_memory: Integer;    // If non-zero, 'internal_memory' pointer is not
                             // used. If value is '2' or more, the external
                             // memory is considered 'slow' and multiple
                             // read/write will be avoided.
    YUVA: TWebPYUVABuffer;
    pad: array[0..3] of Cardinal;
    private_memory: PByte;   // Internally allocated memory (only when
                             // is_external_memory is 0). Should not be used
    {case Variant: Boolean of
      true: (RGBA: TWebPRGBABuffer);
      false: (YUVA: TWebPYUVABuffer);}
  end;
  PWebpDecBufferYUVA = ^TWebpDecBufferYUVA;


  TWebPDecoderOptions = record
    bypass_filtering: Integer;               // if true, skip the in-loop filtering
    no_fancy_upsampling: Integer;            // if true, use faster pointwise upsampler
    use_cropping: Integer;                   // if true, cropping is applied _first_
    crop_left, crop_top: Integer;            // top-left position for cropping.
                                        // Will be snapped to even values.
    crop_width, crop_height: Integer;        // dimension of the cropping area
    use_scaling: Integer;                    // if true, scaling is applied _afterward_
    scaled_width, scaled_height: Integer;    // final resolution
    use_threads: Integer;                    // if true, use multi-threaded decoding
    dithering_strength: Integer;             // dithering strength (0=Off, 100=full)
    flip: Integer;                           // if true, flip output vertically
    alpha_dithering_strength: Integer;       // alpha dithering strength in [0..100]
    pad: array[0..4] of Cardinal;  // padding for later use
  end;

  // Main object storing the configuration for advanced decoding.
  TWebPDecoderConfig = record
    input: TWebPBitstreamFeatures;
    output: TWebPDecBuffer;         // Output buffer (can point to external mem)
    options: TWebPDecoderOptions;   // Decoding options
  end;
  PWebPDecoderConfig = ^TWebPDecoderConfig;
(******************************************************************************
  decode.h
  Main decoding functions for WEBP images.
 ******************************************************************************)
const
  WEBP_DECODER_ABI_VERSION = $0209;
  lib_webp = 'fict';

type
  TWebpFree = procedure(p: pointer); cdecl;

  // Return the decoder's version number, packed in hexadecimal using 8bits for
// each of major/minor/revision. E.g: v2.5.7 is 0x020507.
  //WebpGetDecoder
  TWebPGetDecoderVersion= function: Integer; cdecl;

// Retrieve basic header information: width, height.
// This function will also validate the header and return 0 in
// case of formatting error.
// Pointers *width/*height can be passed NULL if deemed irrelevant.
  TWebpGetInfo = function(const data: PByte; data_size: Cardinal;
  width, height: PInteger): Integer; cdecl;

// Decodes WEBP images pointed to by *data and returns RGB samples, along
// with the dimensions in *width and *height.
// The returned pointer should be deleted calling WebPFree(data).
// Returns NULL in case of error.
  TWebPDecodeRGB = function(const data: PByte; data_size: Cardinal;
  width, height: PInteger): PByte; cdecl;

// Same as WebPDecodeRGB, but returning RGBA data.
  TWebPDecodeRGBA = function(const data: PByte; data_size: Cardinal;
    width, height: PInteger): PByte; cdecl;

// This variant decode to BGR instead of RGB.
  TWebPDecodeBGR = function(const data: PByte; data_size: Cardinal;
    width, height: PInteger): PByte; cdecl;

// This variant decodes to BGRA instead of RGBA.
  TWebPDecodeBGRA = function(const data: PByte; data_size: Cardinal;
    width, height: PInteger): PByte; cdecl;

  TWebPDecodeARGB = function(const data: PByte; data_size: Cardinal;
    width, height: PInteger): PByte; cdecl;

// Decode WEBP images stored in *data in Y'UV format(*). The pointer returned is
// the Y samples buffer. Upon return, *u and *v will point to the U and V
// chroma data. These U and V buffers need NOT be free()'d, unlike the returned
// Y luma one. The dimension of the U and V planes are both (*width + 1) / 2
// and (*height + 1)/ 2.
// Upon return, the Y buffer has a stride returned as '*stride', while U and V
// have a common stride returned as '*uv_stride'.
// Return NULL in case of error.
// (*) Also named Y'CbCr. See: http://en.wikipedia.org/wiki/YCbCr
  TWebPDecodeYUV = function(const data: PByte; data_size: Cardinal; width, height: PInteger;
    var u, v: PByte; stride, uv_stride: PInteger): PByte; cdecl;

// These three functions are variants of the above ones, that decode the image
// directly into a pre-allocated buffer 'output_buffer'. The maximum storage
// available in this buffer is indicated by 'output_buffer_size'. If this
// storage is not sufficient (or an error occurred), NULL is returned.
// Otherwise, output_buffer is returned, for convenience.
// The parameter 'output_stride' specifies the distance (in bytes)
// between scanlines. Hence, output_buffer_size is expected to be at least
// output_stride x picture-height.
  TWebPDecodeRGBInto = function(const data: PByte; data_size: Cardinal;
    output_buffer: PByte; output_buffer_size, output_stride: Integer): PByte; cdecl;
  TWebPDecodeRGBAInto = function(const data: PByte; data_size: Cardinal;
    output_buffer: PByte; output_buffer_size, output_stride: Integer): PByte; cdecl;

// BGR variants
  TWebPDecodeBGRInto = function(const data: PByte; data_size: Cardinal;
    output_buffer: PByte; output_buffer_size, output_stride: Integer): PByte; cdecl;
  TWebPDecodeBGRAInto = function(const data: PByte; data_size: Cardinal;
    output_buffer: PByte; output_buffer_size, output_stride: Integer): PByte; cdecl;

// WebPDecodeYUVInto() is a variant of WebPDecodeYUV() that operates directly
// into pre-allocated luma/chroma plane buffers. This function requires the
// strides to be passed: one for the luma plane and one for each of the
// chroma ones. The size of each plane buffer is passed as 'luma_size',
// 'u_size' and 'v_size' respectively.
// Pointer to the luma plane ('*luma') is returned or NULL if an error occurred
// during decoding (or because some buffers were found to be too small).
  TWebPDecodeYUVInto = function(const data: PByte; data_size: Cardinal;
     luma: PByte; luma_size, luma_stride: Integer;
     u: PByte; u_size, u_stride: Integer;
     v: PByte; v_size, v_stride: Integer): PByte; cdecl;
//-----------------------------------------------------------------------------

// INCREMENTAL DECODING IT'S NOT FULLY DOCUMENTED
//

// Internal, version-checked, entry point
  TWebPGetFeaturesInternal = function(const data: PByte;
    data_size: Cardinal; features: PWebPBitstreamFeatures;
    version: Integer): TVP8StatusCode; cdecl;
  TWebPInitDecoderConfigInternal = function(config:
    PWebPDecoderConfig; version: Integer): Integer;cdecl;
  TWebPInitDecBufferInternal = function(buffer: PWebPDecBuffer;
    version: Integer): Integer; cdecl;
  TWebPFreeDecBuffer = procedure(buffer: PWebPDecBuffer); cdecl;
  TWebPDecode = function(const data: PByte; data_size: NativeUint;
    config: PWebPDecoderConfig): TVP8StatusCode; cdecl;

//-----------------------------------------------------------------------------
// Lower-level API IT'S NOT FULLY DOCUMENTED
//


var
  WebpFree: TWebpFree;
  WebPGetDecoderVersion: TWebPGetDecoderVersion;
  WebpGetInfo: TWebpGetInfo;
  WebPDecodeRGB: TWebPDecodeRGB;
  WebPDecodeRGBA: TWebPDecodeRGBA;
  WebPDecodeBGR: TWebPDecodeBGR;
  WebPDecodeBGRA: TWebPDecodeBGRA;
  WebPDecodeARGB: TWebPDecodeARGB;
  WebPDecodeYUV: TWebPDecodeYUV;
  WebPDecodeRGBInto: TWebPDecodeRGBInto;
  WebPDecodeRGBAInto: TWebPDecodeRGBAInto;
  WebPDecodeBGRInto: TWebPDecodeBGRInto;
  WebPDecodeBGRAInto: TWebPDecodeBGRAInto;
  WebPDecodeYUVInto: TWebPDecodeYUVInto;
  WebPGetFeaturesInternal: TWebPGetFeaturesInternal;
  WebPInitDecoderConfigInternal: TWebPInitDecoderConfigInternal;
  WebPInitDecBufferInternal: TWebPInitDecBufferInternal;
  WebPFreeDecBuffer: TWebPFreeDecBuffer;
  WebPDecode: TWebPDecode;

// Internal, version-checked, entry point

function WebPGetFeatures(data: PByte; data_size: NativeUint;
  features: PWebPBitstreamFeatures): TVp8StatusCode;
function WebPInitDecoderConfig(config: PWebPDecoderConfig): Integer;
function WebPInitDecBuffer(buffer: PWebPDecBuffer): Integer;

implementation

function WebPGetFeatures(data: PByte; data_size: NativeUint;
    features: PWebPBitstreamFeatures): TVp8StatusCode;
begin
  Result:= WebPGetFeaturesInternal(data, data_size, features,
    WEBP_DECODER_ABI_VERSION);
end;

function WebPInitDecoderConfig(config: PWebPDecoderConfig): Integer;
begin
  Result:= WebPInitDecoderConfigInternal(config,
    WEBP_DECODER_ABI_VERSION);
end;

function WebPInitDecBuffer(buffer: PWebPDecBuffer): Integer;
begin
  Result:= WebPInitDecBufferInternal(buffer, WEBP_DECODER_ABI_VERSION);
end;

end.
