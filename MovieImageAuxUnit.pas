unit MovieImageAuxUnit;

{$IFDEF FPC}
{$mode objfpc}
{$H+}
{$ENDIF}


interface

uses
  {$IFNDEF FPC}
  {$DEFINE WINDOWS}
  Winapi.Windows, System.Classes, System.SysUtils, System.Types,
    System.StrUtils, System.AnsiStrings, Vcl.Graphics, System.Math
    Vcl.Dialogs;
  {$ELSE}
  {$UNDEF WINDOWS}  // see the note in movieimagenographic
  {$IFDEF WINDOWS}
  Windows,
  {$ELSE}
  LclIntf, LclType,
  {$ENDIF}
  Dialogs, Classes, SysUtils, Controls, Forms, Graphics, IntfGraphics,
    GraphType, ExtCtrls, FpImage, FpReadBmp, FpReadJpeg, FpReadPng, FpReadTiff, FpReadXpm,
    Types, StrUtils;
  {$ENDIF}

type
  TBGrQuad = record  // TRGBQuad should be called TBGRQuad!
    rgbRed, rgbGreen, rgbBlue, rgbAlpha: Byte;
  end;
  PBGrQuad = ^TBGRQuad;

  PRGBQuad = ^TRGBQuad;
  TRGBQuadArray = array of TRGBQuad;

  TBGrTriple = record
    rgbRed, rgbGreen, rgbBlue: Byte;
  end;
  PBGrTriple = ^TBGrTriple;

  PPALETTE = ^HPALETTE;

  TBGrQuadArray = array of TBGrQuad;
  PBGRQuadArray = ^TBGrQuadArray;
  TBGrTripleArray = array of TBGrTriple;
  PBGrTripleArray = ^TBGrTripleArray;

  TGraphicType = record
    Offset, Len: Cardinal;
    Signature, Extension: PAnsiChar;
  end;

  TFontData = record
    Height: Integer;
    Weight: Integer;
    Width: Integer;
    Italic: Integer;
  end;
  PFontData = ^TFontData;

  {$IFNDEF FPC}
  TBitsHelper = class helper for TBits
  public
    function ToUInt32: UInt32;
    function ToByte: Byte;
  end;
  {$ENDIF}

function GetImageExtensionBySignature(Stream: TMemoryStream): AnsiString; overload;
function GetImageExtensionBySignature(FileName: String): AnsiString; overload;
function ArrayToAnsiString(const a: array of AnsiChar;
  len: Integer = 0; start: Integer = 0): AnsiString;
function GetBitsValue(sourcevalue, posbyte,
  numbytes: Byte): Byte;
procedure ReadCheck(Stream: TStream; var Buffer; Size: LongInt);
function ArrayToString(const a: array of Char; len: Integer = 0): string;
function ValidFileName(filename: string; charreplace: Char = '='): string;
procedure InvalidOperation(Str: string);
Function ByteToBin(aByte: Byte): String;
function IPath(path: string{$IFDEF WINDOWS}; bfilename: Boolean{$ENDIF}): string;
function ErrorMessage(aCaption, aMsg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons): Word;

procedure GetMonoFonts;

{$IFDEF FPC}
function GetFPImageReaderForFileExtension(const FileExt: string): TFPCustomImageReaderClass;
procedure SetLazBitmapBits(Bitmap: TBitmap; ImageData: Pointer; Bpp: Byte);
procedure GetlazDibits(FBitmap: TBitmap; var ImageData: Pointer; var ImageDataSize: Integer;
   BGR: Boolean = false);
{$ENDIF}

var
  MonoFonts: TStringList;

const
  sInvalidColor		= 'Color not in color table';

implementation

const
  GraphicTypes: array[1..7] of TGraphicType = (
      (Offset: 0; Len: 2;  Signature: 'BM'; Extension: '.bmp'),  // BMP
      (Offset: 6; Len: 4; Signature: 'JFIF'; Extension: '.jpg'),// JPG
      (Offset: 6; Len: 5; Signature: 'Exif'; Extension: '.jpg'),// JPG
      (Offset: 1; Len: 3; Signature: 'PNG'; Extension: '.png'), // PNG
      (Offset: 0; Len: 3; Signature: 'GIF'; Extension: '.gif'), // GIF
      (Offset: 8; Len: 4; Signature: 'WEBP'; Extension: '.webp'), // WEBP
      (Offset: 0; Len: 3; Signature: 'II*'; Extension: '.tiff')  // TIFF
    );

function GetImageExtensionBySignature(Stream: TMemoryStream): AnsiString;
var
  I: Integer;
begin
  Result:= '';
  for I := Low(GraphicTypes) to High(GraphicTypes) do
  with GraphicTypes[I] do
  begin
    if CompareMem({%H-}Pointer({$IFDEF FPC}{%H-}PtrUint
      {$ELSE}Cardinal{$ENDIF}(Stream.Memory) + Offset),
      Signature, Len) then
    begin
      {$IFDEF FPC}
      Result:= StrPas(Extension);
      {$ELSE}
      Result:= System.AnsiStrings.StrPas(Extension);
      {$ENDIF}
      exit;
    end;
  end;
end;

function GetImageExtensionBySignature(FileName: String): AnsiString;
var
  f: TMemoryStream;
begin
  f:= TMemoryStream.Create;
  try
    f.LoadFromFile(FileName);
    Result:= GetImageExtensionBySignature(f);
  finally
    f.Free;
  end;
end;

function ArrayToAnsiString(const a: array of AnsiChar;
  len: Integer = 0; start: Integer = 0): AnsiString;
var
  i: Integer;
begin
  if len = 0 then
  begin
    len:= Length(a);
    for i:= start to High(a) do
      if a[i] = #0 then
      begin
        len:= i;
        break;
      end;
  end;
  if len > 0 then
    SetString(Result, PAnsiChar(@a[start]), len)
  else
    Result := '';
end;

function GetBitsValue(sourcevalue, posbyte,
  numbytes: Byte): Byte;
begin
  Result:= Byte(sourcevalue shl posbyte);
  Result:= Byte(Result shr (8 - numbytes));
end;

function MonoCallback(var f: TEnumLogFontEx; var {%H-}Metric: TNewTextMetricEx;
    {%H-}FontType: Longint; {%H-}Data:LParam): LongInt; stdcall;
var
  I: Integer;
  p: PFontData;
  b: Byte;
  tp: Boolean;
begin
  b:= f.elfLogFont.lfPitchAndFamily;
  tp:= true;
  if FIXED_PITCH and b = 0 then
    tp:= b AND $F0 = FF_MODERN;
  if tp then
  begin
    I:= 0;
    while I < MonoFonts.Count do
    begin
      if CompareText(MonoFonts[I], f.elfLogFont.lfFaceName) = 0 then
        with {%H-}PFontData(IntPtr(MonoFonts.Objects[I]))^ do
          if (Height = f.elfLogFont.lfHeight) and (Width = f.elfLogFont.lfWidth)
            and (Weight = f.elfLogFont.lfWeight) and
            (Italic = f.elfLogFont.lfItalic) then
            break;
      Inc(I);
    end;
    if I = MonoFonts.Count then
    begin
      New(p);
      p^.Height:= f.elfLogFont.lfHeight;
      p^.Width:= f.elfLogFont.lfWidth;
      p^.Weight:= f.elfLogFont.lfWeight;
      p^.Italic:= f.elfLogFont.lfItalic;
      MonoFonts.AddObject(f.elfLogFont.lfFaceName, TObject({%H-}IntPtr(p)));
    end;
  end;
  Result:= 1;
end;

procedure GetMonoFonts;
var
  f: TLOGFONT;
  dc: HDC;
begin
  f.lfCharSet:= ANSI_CHARSET;
  f.lfFaceName[0]:= #0;
  f.lfPitchAndFamily:= 0;
  dc:= GetDC(0);
  try
    EnumFontFamiliesEx(dc, {$IFDEF FPC}@{$ENDIF}f, @MonoCallBack, 0, 0);
  finally
    ReleaseDC(0, dc);
  end;
  ReleaseDC(0, dc);
end;

procedure FreeMonoFonts;
var
  I: Integer;
begin
  for I:= 0 to MonoFonts.Count -1 do
    Dispose({%H-}PFontData(IntPtr(MonoFonts.Objects[I])));
  MonoFonts.Free;
end;

{$IFDEF FPC}
function GetFPImageReaderForFileExtension(const FileExt: string): TFPCustomImageReaderClass;
begin
  if FileExt='.bmp' then
    Result:=TFPReaderBMP;
  if (FileExt='.jpg') or (FileExt='.jpeg') then
    Result:=TFPReaderJPEG;
  if FileExt='.png' then
    Result:=TFPReaderPNG;
  if FileExt='.xpm' then
    Result:=TFPReaderXPM;
  if FileExt='.tiff' then
    Result:=TFPReaderTiff;
end;
{$ENDIF}

procedure ReadCheck(Stream: TStream; var Buffer; Size: LongInt);
var
  ReadSize		: integer;
begin
  ReadSize := Stream.Read(Buffer, Size);
  if ReadSize <> Size then
    raise Exception.Create('Corruption');
end;

function ArrayToString(const a: array of Char; len: Integer = 0): string;
var
  i: Integer;
begin
  if len = 0 then
  begin
    len:= length(a);
    for i:= 0 to High(a) do
      if a[i] = #0 then
      begin
        len:= i;
        break;
      end;
  end;
  if len > 0 then
    SetString(Result, PChar(@a[0]), len)
  else
    Result := '';
end;

{$IFNDEF FPC}
{ TBitsHelper }

function TBitsHelper.ToByte: Byte;
begin
  if Size > 8 then
    raise EOverflow.Create('Size overflow!');
  with Self do
    Result := PByte(FBits)^;
end;

function TBitsHelper.ToUInt32: UInt32;
type
  PUInt32 = ^UInt32;
begin
  if Size > SizeOf(Result) * 8 then
    raise EOverflow.Create('Size overflow!');
  with Self do
    Result := PUInt32(FBits)^;
end;
{$ENDIF}

function ValidFileName(filename: string; charreplace: Char = '='): string;
var
  i, j: Integer;
  s: string;
  sdrive, sfile: string;
  pazs: TStringDynArray;
begin
  s:= '\/:*?"<>|';
  sdrive:= ExtractFileDrive(filename);
  sfile:= StringReplace(filename, sdrive, '', []);
  pazs:= SplitString(sfile, '\');
  sfile:= sdrive;
  for i:= 0 to High(pazs) do
  begin
    for j:= 1 to pazs[i].Length do
      if Pos(pazs[i][j], s) > 0 then
        pazs[i][j]:= charreplace;
    sfile:= sfile + pazs[i];
    if i < High(pazs) then
      sfile:= sfile + '\';
  end;
  sfile:= Trim(sfile);
  while (sfile.Length > 0) and (sfile[sfile.Length] = '.') do
    Delete(sfile, sfile.Length, 1);
  Result:= sfile;
end;

procedure InvalidOperation(Str: string);
var
{$IFDEF FPC}
  p: PString;
{$ELSE}
  p: PResStringRec;
{$ENDIF}
begin
  p:= @Str;
  raise EInvalidGraphicOperation.CreateRes(p);
end;

Function ByteToBin(aByte: Byte): String;
Const
  c10: Array[Boolean] of Char = ('0', '1');
Var
  eLoop1: Byte;
Begin
   {$IFDEF FPC}
   Result:= ''; // fpc should consider SetLength as initialization
   {$ENDIF}
   SetLength(Result, 8);
   For eLoop1 := 0 to 7 do
     Result[eLoop1 + 1] := c10[(aByte and (1 shl eLoop1)) <> 0];
End;

function RPos(const aSubStr, aString : String): Integer; overload;
var
  i: Integer;
  pStr: PChar;
  pSub: PChar;
begin
  pSub := Pointer(aSubStr);
  try

  for i := Length(aString) - Length(aSubStr) + 1 downto 1 do
  begin
    pStr := @(aString[i]);
    if (pStr <> nil) and (pStr^ = pSub^) then
    begin
      if StrLComp(pSub, pstr, Length(aSubStr)) = 0 then
      // if CompareMem(pSub, pStr, Length(aSubStr)) then
      begin
        result := i;
        EXIT;
      end;
    end;
  end;
  except

  end;

  result := 0;
end;


function GetRightFilePath(sfilename: string): string;
{$IFDEF WINDOWS}
var
  i: Integer;
  sdum: PChar;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  GetMem(sdum, sizeof(PChar));
  i:= GetFullPathName(PChar(sfilename), 0, nil, sdum);
  {$IFDEF FPC}
  Result:= '';
  {$ENDIF}
  SetLength(Result, i);
  GetFullPathName(PChar(sfilename), i, PChar(Result), sdum);
  i:= RPos(sdum, Result);
  Result:= Result.Substring(0, i - 1);
  {$ELSE}
  Result:= ExpandFileName(sfilename);
  {$ENDIF}
end;

function IPath(path: string{$IFDEF WINDOWS}; bfilename: Boolean{$ENDIF}): string;
begin
  {$IFDEF WINDOWS}
  if not bfilename then
    Result:= IncludeTrailingPathDelimiter(path)
  else
    Result:= IncludeTrailingPathDelimiter(GetRightFilePath(path)) +
      ExtractFileName(path);
  {$ELSE}
  Result:= path;
  {$ENDIF}
  if Pos('\\?\', Result) = 0 then
    Result:= '\\?\' + Result;
end;

{$IFDEF FPC}
procedure SetLazBitmapBits(Bitmap: TBitmap; ImageData: Pointer; Bpp: Byte);
var
  f: TLazIntfImage;
  raw: TRawImage;
begin
  raw.Init;
  if Bpp = 24 then
    raw.Description.Init_BPP24_B8G8R8_M1_BIO_TTB(Bitmap.Width, Bitmap.Height)
  else
  {$IFDEF WINDOWS}
    raw.Description.Init_BPP32_B8G8R8A8_BIO_TTB(Bitmap.Width, Bitmap.Height);
  {$ELSE}
    raw.Description.Init_BPP32_R8G8B8A8_BIO_TTB(Bitmap.Width, Bitmap.Height);
  {$ENDIF}
  raw.Data:= ImageData;
  raw.DataSize:= (((((Bitmap.Width * Bpp) + 31) and not 31) shr 3)) * Bitmap.Height;
  f:= TLazIntfImage.Create(raw, false);
  try
    Bitmap.LoadFromIntfImage(f);
  finally
    f.Free;
  end;
end;

procedure GetlazDibits(FBitmap: TBitmap; var ImageData: Pointer; var ImageDataSize: Integer;
  BGR: Boolean = false);
var
  f: TLazIntfImage;
  X, Y, w: Integer;
  R, Init: PByte;
begin
  f:= FBitmap.CreateIntfImage;
  X:= f.DataDescription.BytesPerLine;
  ImageDataSize:= f.DataDescription.BytesPerLine * f.DataDescription.Height;
  Getmem(R, ImageDataSize);
  try
    Init:= R;
    w:= f.DataDescription.Width - 1;
    if f.DataDescription.BitsPerPixel = 24 then
    begin
      for Y := 0 to f.DataDescription.Height - 1 do
      begin
        for X := 0 to w do
        begin
          with f.Colors[X, Y] do
          begin
             R^:= Round(Red * MAXBYTE / MAXWORD);
             Inc(R);
             R^:= Round(Green * MAXBYTE / MAXWORD);
             Inc(R);
             R^:= Round(Blue * MAXBYTE / MAXWORD);
             Inc(R);
          end;
        end;
        for X:= f.DataDescription.Width * 3 to f.DataDescription.BytesPerLine - 1 do
          Inc(R);
      end;
    end
    else if BGR then
    begin
      for Y := 0 to f.DataDescription.Height - 1 do
      begin
        for X := 0 to w do
        begin
          with f.Colors[X, Y] do
          begin
             R^:= Round(Blue * MAXBYTE / MAXWORD);
             Inc(R);
             R^:= Round(Green * MAXBYTE / MAXWORD);
             Inc(R);
             R^:= Round(Red * MAXBYTE / MAXWORD);
             Inc(R);
             R^:= Round(Alpha * MAXBYTE / MAXWORD);
             Inc(R);
          end;
        end;
        for X:= f.DataDescription.Width * 4 to f.DataDescription.BytesPerLine - 1 do
          Inc(R);
      end;
    end
    else
    begin
      for Y := 0 to f.DataDescription.Height - 1 do
      begin
        for X := 0 to w do
        begin
          with f.Colors[X, Y] do
          begin
             R^:= Round(Red * MAXBYTE / MAXWORD);
             Inc(R);
             R^:= Round(Green * MAXBYTE / MAXWORD);
             Inc(R);
             R^:= Round(Blue * MAXBYTE / MAXWORD);
             Inc(R);
             R^:= Round(Alpha * MAXBYTE / MAXWORD);
             Inc(R);
          end;
        end;
        for X:= f.DataDescription.Width * 4 to f.DataDescription.BytesPerLine - 1 do
          Inc(R);
      end;
    end;
    R:= Init;
    GetMem(ImageData, ImageDataSize);
    {$IFDEF WINDOWS}
    CopyMemory(ImageData, R, ImageDataSize);
    {$ELSE}
    Move(R^, ImageData^, ImageDataSize);
    {$ENDIF}
  finally
    FreeMem(R);
  end;
end;

{$ENDIF}

function ErrorMessage(aCaption, aMsg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons): Word;
begin
  {$IFDEF FPC}
  Result:= MessageDlg(aCaption, aMsg, DlgType, Buttons, '');
  {$ELSE}
  Result:= MessageDlg(aMsg, DlgType, Buttons, 0);
  {$ENDIF}
end;

initialization
  MonoFonts:= TStringList.Create;

finalization
  FreeMonoFonts;

end.

