unit InitWebp;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

{$IFNDEF FPC}
{$DEFINE WINDOWS}
{$ELSE}
//{$UNDEF WINDOWS} // uncomment this for fpc windows
{$ENDIF}

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  Classes, SysUtils, Dialogs;

const
  LIB_WEBP = 'libwebp.dll';  // only 32bit implemented
                             // change the path name as you wish
                             // it's version 1.2.4. sorry but incremental decoding and
                             // lower-level api are not fully documented for this version.

implementation

uses decodewebp124nodelay, encodewebp124nodelay;

var
  Webphandle: THandle;

function GetWebpFuncAddress(var addr: Pointer; const name : PAnsiChar) : Boolean;
begin
  {$IFDEF WINDOWS}
  addr := GetProcAddress(WebpHandle, name);
  {$ELSE}
  addr:= GetProcedureAddress(WebpHandle, name);
  {$ENDIF}
  Result := (addr <> NIL);
  if not Result then
    ErrorMessage('Error', 'Entry point "' + name + '" not found!', mtError, [mbOk]);
end;

procedure LoadWebp;
begin
  {$IFDEF WINDOWS}
  SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOALIGNMENTFAULTEXCEPT);
  {$ENDIF};

  WebpHandle:= LoadLibrary(LIB_WEBP);

  {$IFDEF LINUX}
  if (WebpHandle < 0) then
  begin
    WebpHandle := 0;
  end;
  {$ENDIF}

  if WebpHandle = 0 then
  begin
    {$IFDEF FPC}
    MessageDlg('Error', 'Library not found ' +
      'Error = ' + IntToStr(GetLastOsError), mtError, [mbOk], '');
    {$ELSE}
    MessageDlg('Library not found ' +
      'Error = ' + IntToStr(GetLastError), mtError, [mbOk], 0);
    {$ENDIF}
    exit;
  end;

  if not GetWebpFuncAddress(@WebpFree, 'WebPFree') then exit;
  if not GetWebpFuncAddress(@WebPGetDecoderVersion, 'WebPGetDecoderVersion') then exit;
  if not GetWebpFuncAddress(@WebpGetInfo, 'WebPGetInfo') then exit;
  if not GetWebpFuncAddress(@WebPDecodeRGB, 'WebPDecodeRGBA') then exit;
  if not GetWebpFuncAddress(@WebPDecodeRGBA, 'WebPDecodeRGBA') then exit;
  if not GetWebpFuncAddress(@WebPDecodeBGR, 'WebPDecodeBGR') then exit;
  if not GetWebpFuncAddress(@WebPDecodeBGRA, 'WebPDecodeBGRA') then exit;
  if not GetWebpFuncAddress(@WebPDecodeARGB, 'WebPDecodeARGB') then exit;
  if not GetWebpFuncAddress(@WebPDecodeYUV, 'WebPDecodeYUV') then exit;
  if not GetWebpFuncAddress(@WebPDecodeRGBInto, 'WebPDecodeRGBInto') then exit;
  if not GetWebpFuncAddress(@WebPDecodeRGBAInto, 'WebPDecodeRGBAInto') then exit;
  if not GetWebpFuncAddress(@WebPDecodeBGRInto, 'WebPDecodeBGRInto') then exit;
  if not GetWebpFuncAddress(@WebPDecodeBGRAInto, 'WebPDecodeBGRAInto') then exit;
  if not GetWebpFuncAddress(@WebPDecodeYUVInto, 'WebPDecodeYUVInto') then exit;
  if not GetWebpFuncAddress(@WebPGetFeaturesInternal, 'WebPGetFeaturesInternal') then exit;
  if not GetWebpFuncAddress(@WebPInitDecoderConfigInternal, 'WebPInitDecoderConfigInternal') then exit;
  if not GetWebpFuncAddress(@WebPInitDecBufferInternal, 'WebPInitDecBufferInternal') then exit;
  if not GetWebpFuncAddress(@WebPFreeDecBuffer, 'WebPFreeDecBuffer') then exit;
  if not GetWebpFuncAddress(@WebPDecode, 'WebPDecode') then exit;
  if not GetWebpFuncAddress(@WebPGetEncoderVersion, 'WebPGetEncoderVersion') then exit;
  if not GetWebpFuncAddress(@WebPEncodeRGB, 'WebPEncodeRGB') then exit;
  if not GetWebpFuncAddress(@WebPEncodeBGR, 'WebPEncodeBGR') then exit;
  if not GetWebpFuncAddress(@WebPEncodeRGBA, 'WebPEncodeRGBA') then exit;
  if not GetWebpFuncAddress(@WebPEncodeBGRA, 'WebPEncodeBGRA') then exit;
  if not GetWebpFuncAddress(@WebPEncodeLosslessRGB, 'WebPEncodeLosslessRGB') then exit;
  if not GetWebpFuncAddress(@WebPEncodeLosslessBGR, 'WebPEncodeLosslessBGR') then exit;
  if not GetWebpFuncAddress(@WebPEncodeLosslessRGBA, 'WebPEncodeLosslessRGBA') then exit;
  if not GetWebpFuncAddress(@WebPEncodeLosslessBGRA, 'WebPEncodeLosslessBGRA') then exit;
  if not GetWebpFuncAddress(@WebPValidateConfig, 'WebPValidateConfig') then exit;
  if not GetWebpFuncAddress(@WebPPictureAlloc, 'WebPPictureAlloc') then exit;
  if not GetWebpFuncAddress(@WebPPictureFree, 'WebPPictureFree') then exit;
  if not GetWebpFuncAddress(@WebPPictureCopy, 'WebPPictureCopy') then exit;
  if not GetWebpFuncAddress(@WebPPictureCrop, 'WebPPictureCrop') then exit;
  if not GetWebpFuncAddress(@WebPPictureImportRGB, 'WebPPictureImportRGB') then exit;
  if not GetWebpFuncAddress(@WebPPictureImportRGBA, 'WebPPictureImportRGBA') then exit;
  if not GetWebpFuncAddress(@WebPPictureImportBGR, 'WebPPictureImportBGR') then exit;
  if not GetWebpFuncAddress(@WebPPictureImportBGRA, 'WebPPictureImportBGRA') then exit;
  if not GetWebpFuncAddress(@WebPEncode, 'WebPEncode') then exit;
  if not GetWebpFuncAddress(@WebPMemoryWriterInit, 'WebPMemoryWriterInit') then exit;
  //if not GetWebpFuncAddress(@WebPMemoryWriterClear, 'WebPMemoryWriterClear') then exit;  NOT FOUND!!
  if not GetWebpFuncAddress(@WebPMemoryWrite, 'WebPMemoryWrite') then exit;
  //@WebpConfigInitInternal:= getprocaddress(WebpHandle, 'WebPConfigInitInternal');
  if not GetWebpFuncAddress(@WebPConfigInitInternal, 'WebPConfigInitInternal') then exit;
  if not GetWebpFuncAddress(@WebPPictureInitInternal, 'WebPPictureInitInternal') then exit;
end;

initialization
  LoadWebp;

finalization
  if WebpHandle <> 0 then
    FreeLibrary(WebpHandle);

end.

