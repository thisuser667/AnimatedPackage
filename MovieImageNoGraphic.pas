unit MovieImageNoGraphic;

{$IFDEF FPC}
{$mode objfpc}
{$H+}
{$ENDIF}

// copyright: google, microsoft, embarcadero, sursum corda

interface

uses
  // Define DOPLAINTEXT when you want to process gif plain text extensions.
  // (https://entropymine.wordpress.com/2018/06/23/gif-plain-text-extensions/)
  // That means almost never, but in FPC, if defined, I couldn't achieve
  // transparency.
  //{$DEFINE DOPLAINTEXT}
  {$IFNDEF FPC}
  {$DEFINE WINDOWS}
    WinApi.Windows, Winapi.Messages, Winapi.WinCodec, System.SysUtils,
    System.AnsiStrings, Vcl.Graphics, System.Classes, System.Math,
    Vcl.ExtCtrls, System.Contnrs, Vcl.Forms, Vcl.Dialogs, Vcl.Clipbrd,
    System.Types, Vcl.Controls, System.UiTypes,
  {$ELSE}
  {$UNDEF WINDOWS}  // Lazarus in windows uses windows imaging components, I
                      // implemented this cause I thought it was faster, but
                      // it's perfectly fine and even better undefine windows and
                      // use tlazintfimage or trawimage.
                      // Movieimageauxunit must have the same defs as this one.
  {$IFDEF WINDOWS}
  Windows, LazWicCodec, LazWicImage,
  {$ELSE}
  IntFGraphics, GraphType,
  {$ENDIF}
    Controls, LclType, LclIntf, SysUtils, Types, Graphics, Classes, Math, ExtCtrls,
    Contnrs, Forms, Dialogs, Clipbrd, bithelpers, // https://wiki.freepascal.org/BitHelpers
 {$ENDIF}
  MovieImageAuxUnit, decodewebp124nodelay, encodewebp124nodelay, uMsgDlg, InitWebp;

type
  {$IFDEF FPC}
  {$IFDEF WINDOWS}
  { TAssigntoBitmap }

  TAssigntoBitmap = class(Graphics.TBitmap)
    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
  end;
  {$ENDIF}
  {$ENDIF}

  TMovieImageBase = class;

  TPointArray = array of TPoint;
  PPointArray = ^TPointArray;

  PWordDynArray = ^TWordDynArray;

  TDisposalMethod = (dmNotDisposal, dmBackgroundColor, dmRestorePrev,
    dmUndefined);

  TPlainTextBlock = record   // 0x01 plain text extension identifier
    CellSizeX, CellSizeY: Byte;
    ForeColorIndex, BackColorIndex: Byte; // index to color table
    TextData: array of AnsiChar;
    FontName: array[0..LF_FACESIZE - 1] of Char;
    FontSize: Integer;
  end;
  PPlainTextBlock = ^TPlainTextBlock;

  TImageBase = class;

  {$IFNDEF FPC}
  TWicGraphic = TWicImage;
  TWicGraphicFormat = TWicImageFormat;
  IWicBitmapGraphic = IWicBitmap;
  PWicBitmapGraphic = ^IWicBitmapGraphic;
  {$ENDIF}

  TMovieImageFrame = class
  protected
    function GetBitmapFromDecodedBytes(Width, Height: Integer;{$IFDEF WINDOWS}out WicBitmap:
      IWicBitmapGraphic; {$ENDIF} to32: Boolean = false;
      Bits: Tbytes = nil):
      {$IFDEF WINDOWS}TWicGraphic{$ELSE}TBitmap{$ENDIF};
  public
    MyBase: TMovieImageBase;
    Disposal: TDisposalMethod;
    FrameWidth, FrameHeight, FrameLeft, FrameTop: Word;
    Delay: Cardinal;
    DecodedBits: TBytes;
    EncodedStream: TBytesStream;
    constructor Create(Base: TMovieImageBase); virtual;
    destructor Destroy; override;
  end;

  TGifAppExtension = class
    Identifier: array[0..7] of AnsiChar;
    AuthCode: array[0..2] of AnsiChar;
    AppStream: TBytesStream;
  public
    constructor Create(id: array of AnsiChar);
    destructor Destroy; override;
  end;

  TGifFrame = class(TMovieImageFrame)
  protected
    procedure GetCurrentBits(var FCurrentBits: TBytes;
      colortable: TBGRTripleArray; FWidth, Fheight: Integer;
      to32: Boolean = false);
  public
    Interlaced: Boolean;
    LocalColorTable: TBgrTripleArray;
    FDescriptorPackedField: Byte;
    FControlPackedField: Byte;
    FTransparentFlag: Boolean;
    //TableSorted: Boolean;  it was only useful with less than 256 colors
    UserInput: Boolean;
    MinimumCodeSize: Byte;
    TextBlock: PPlainTextBlock;
    TransparentColorIndex: Byte;
    constructor Create(Base: TMovieImageBase); override;
    destructor Destroy; override;
  end;

  TBytesStreamArray = array of TBytesStream;
  PBytesStreamArray = ^TBytesStreamArray;

  TMovieWebpImage = class;

  TWebpFrame = class(TMovieImageFrame)
  protected
    Lossless: Boolean;
    FrameBitField: Byte; // blending, disposal
    AlphaBitField: Byte; // preprocessing, filtering, compression
    AlphaStream: TBytesStream;
    UnknownChunks: TBytesStreamArray;
    BGR: Boolean; // decoded bytes in BGRA or RGBA format
    function EncodeData(BitCount: Byte; Width, Height: Integer;
      Data: PByte = nil): Boolean;
  public
    constructor Create(Base: TMovieImageBase); override;
    destructor Destroy; override;
  end;

  TMovieFrameList = class(TObjectList)  // container of the frames.
  private
    function GetFrame(Index: Integer): TMovieImageFrame;
    //procedure SetFrame(Index: Integer; Value: TMovieImageFrame);
  public
    MyBase: TMovieImageBase;
    constructor Create(Parent: TMovieImageBase);
    property Frame[Index: Integer]: TMovieImageFrame read GetFrame;
      default;
  end;

  TBeforeDrawingFrameEvent = procedure(Sender: TObject) of
    object;
  TAfterFrameDrawnEvent = procedure(Sender: TObject) of
    object;

  TAnimationSpeed = 1..1000;

  { TImageBase }

  TImageBase = class
  private
    FCanvas: TCanvas;
    FCanvasRect: TRect;
    FFileName: string;
    procedure SetCanvas(Value: TCanvas);
    procedure Draw(ACanvas: TCanvas; const Rect: TRect);
  protected
    FWidth, FHeight: Integer;
    function GetHeight: Integer;
    function GetWidth: Integer;
  public
    procedure Clear;
    procedure CopyToClipboard; virtual; abstract;
    function PasteFromClipboard: Boolean; virtual; abstract;
    procedure DrawFrame(ACanvas: TCanvas; ARect: TRect;
      pfIndex: Integer); virtual; abstract;
    procedure LoadFromFile(const FileName: string); virtual;
    procedure SaveToFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;
    property Canvas: TCanvas read FCanvas write SetCanvas;
    property CanvasRect: TRect read FCanvasRect write FCanvasRect;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property FileName: string read FFileName write FFileName;
  end;

  { TNoMovieImage }

  TNoMovieImage = class(TImageBase)
  public
    {$IFDEF WINDOWS}
    NmImage: TWicGraphic;
    {$ELSE}
    NmImage: TPicture;
    {$ENDIF}
    constructor Create;
    destructor Destroy; override;
    procedure CopyToClipboard; override;
    function PasteFromClipboard: Boolean; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure DrawFrame(ACanvas: TCanvas; ARect: TRect;
      {%H-}pfIndex: Integer); override;
  end;

  { TMovieImageBase }

  TMovieImageBase = class(TImageBase)
  private
    FLoopIndex: Word;  // infinite loop can be overridden
      // with InfiniteLoopAnyWays, despite almost all of the webp
      // files have intinite looping just the same.
    //FAnimateOnGetCanvas: Boolean;  // internal processing.
    FAnimationSpeed: TAnimationSpeed; // 50 = normal speed div 2
    FCurrentBits: TBytes;  // the canvas bits in each iteration
    {$IFNDEF FPC}
    FFileName: string;
    {$ENDIF}
    FNeedCompleteRedraw: Boolean;  // as blending changes
      // as the loop progresses, if one frame is drawn
      // from the user app, all the previous frames have to be
      // rendered.
    FOnBeforeDrawingFrame: TBeforeDrawingFrameEvent;
    FOnAfterFrameDrawn: TAfterFrameDrawnEvent;
    FOnLoop: TNotifyEvent;
    FFrameIndex: Integer;
    FTimer: TTimer;
    FCurrentFrame: Integer; // as said.
    FExternalDrawing: Boolean;  // if true doesn't process draw
    FOnNextFrame: TNotifyEvent;
    FMinimumDelay: Word;
    procedure DrawDecodedBytes(ACanvas: TCanvas; ARect: TRect;
      pfIndex: Integer; BlendedBits: TBytes);
    function GetAnimating: Boolean;
    function GetPaused: Boolean;
    procedure AnimateTimer(Sender: TObject);
  protected
    function DecodeFrame(pfIndex: Integer): Boolean; virtual; abstract;
    {function GetHeight: Integer; override;
    function GetWidth: Integer; override;}
  public
    Frames: TMovieFrameList;
    LoopCount: Word;
    class var SourceAnimated: Byte;
    constructor Create;
    destructor Destroy; override;
    function CanBeAnimated: Boolean; virtual; abstract;
    procedure CopyToClipboard; override;
    function PasteFromClipboard: Boolean; override;
    class function ImageCanBeAnimated(NameOfFile: string;
      silent: Boolean): Boolean; virtual;
    procedure Animate;
    procedure StopAnimation;
    procedure PauseAnimation;
    procedure RestoreAnimation;
    function LoopDurationinMs: Cardinal; // sum of all the delays
    class function ValidGifStream(Stream: TStream): Boolean;
    class function ValidWebpStream(Stream: TStream): Boolean;
    property AnimationSpeed: TAnimationSpeed read FAnimationSpeed
      write FAnimationSpeed default 100;
    property Animating: Boolean read GetAnimating;
    property CurrentFrame: Integer read FCurrentFrame write
      FCurrentFrame;
    property ExternalDrawing: Boolean write FExternalDrawing;
    property FileName: string read FFileName;
    property LoopIndex: Word read FLoopIndex write FLoopIndex;
    property IsPaused: Boolean read GetPaused;
    property FrameIndex: Integer read FFrameIndex;
    property NeedCompleteRedraw: Boolean read FNeedCompleteRedraw
      write FNeedCompleteRedraw;
    property OnBeforeDrawingFrame: TBeforeDrawingFrameEvent read
      FOnBeforeDrawingFrame write FOnBeforeDrawingFrame;
    property OnAfterFrameDrawn: TAfterFrameDrawnEvent read
      FOnAfterFrameDrawn write FOnAfterFrameDrawn;
    property OnLoop: TNotifyEvent read FOnLoop write FOnLoop;
    property OnNextFrame: TNotifyEvent read FONNextFrame write
      FOnNextFrame;
  end;

  { TMovieGifImage }

  TMovieGifImage = class(TMovieImageBase)
  private
    FBackgroundIndex: Byte; // index to global color table
    FPlainTextFontName: string;
    //FColorResolution: Byte; used only for encoding
    // GlobalTableSorted: was only useful with less than 256 colors
    FPixelRatio: Byte; // non-square pixels, apply correction in drawing
    FBackgroundColor: Byte;
    FDescriptorpackedfield: Byte;
    procedure SetPlainTextFontName(Value: string);
  protected
    procedure AddNetscapeExtension(Loops: Word);
    function DecodeFrame(pfIndex: Integer): Boolean; override;
    procedure EncodeFrame(pfIndex: Integer);
    procedure DrawBackground(ARect: TRect; table: TBGRTripleArray);
  public
    SelfGlobalTable: TBGRTripleArray;
    Comments: array of array of AnsiChar;  // only implemented for writing
    AppExtensions: TObjectList;
    constructor Create;
    destructor Destroy; override;
    procedure DrawFrame(ACanvas: TCanvas; ARect: TRect;
      pfIndex: Integer); override;
    function CanBeAnimated: Boolean; override;
    procedure LoadFromStream(Stream: TStream); override;
    // simple make with infinite loop, 100 ms of delay all,
    // dispose none all
    procedure SaveToStream(Stream: TStream); override;
    property BackgroundIndex: Byte read FBackgroundIndex;
    property PixelRatio: Byte read FPixelRatio;
    property BackgroundColor: Byte read FBackgroundColor;
    property PlainTextFontName: string read FPlainTextFontName
      write SetPlainTextFontName;
  end;

  { TMovieWebpImage }

  TWebpEncoding = procedure(Sender: TObject; dataencodedsize: Cardinal;
    finalsize: Cardinal) of Object;

  TMovieWebpImage = class(TMovieImageBase)
  protected
    ExifStream: TBytesstream;  // not implemented
      // chunks without implementation can be just ignored
    HasFramesWithAlpha: Boolean;
    ICCPStream: TBytesStream; // should be implemented in a more professional
      // implementation.
    XMPStream: Tbytesstream; // not implemented
    // All the next overrides, necessary because TGraphic has
    FBackgroundColor: TBgrQuad;  // background color inside the file
    FExtendedFormat: Boolean; // VP8X;  reading or frames.count > 0;
    FOnWebpEncoding: TWebpEncoding;
    function DecodeFrame(pfIndex: Integer): Boolean; override;
    procedure DrawBackground(ARect: TRect);
    procedure SetBackgroundColor(Value: TBgrQuad);
  public
    HeaderBitField: Byte;
    UnknownChunks: TBytesStreamArray;
    constructor Create;
    destructor Destroy; override;
    function CanBeAnimated: Boolean; override;
    procedure DrawFrame(ACanvas: TCanvas; ARect: TRect;
      pfIndex: Integer); override;
    procedure LoadFromStream(Stream: TStream); override;
    function MakeFromGraphicArray(Graphics: array
      of TGraphic; delays: PWordDynArray = nil; loops: Word = 0;
      delayall: Integer = -1; origcoords: PPointArray = nil): Boolean;
    procedure SaveToStream(Stream: TStream); override;
    property BackgroundColor: TBGRQuad read FBackgroundColor write SetBackgroundColor;
    property OnWebpEncoding: TWebpEncoding read FOnWebpEncoding write
     FOnWebpEncoding;
  end;

  TSaveFramesFormat = (sffJpeg, sffPng, sffWebp);

var
  CF_MWEBP: Word = 0;
  CF_MGIF: Word = 1;
  ErrorOnBadFrame: Boolean = true;
  InfiniteLoopAnyWays: Boolean = false;
  AnimateOnLoaded: Boolean = true;
  GlobalTAble: TBGRTripleArray;
  WebpEncodingQuality: Single = 75; // 1 - 100

const
  USER_TIMER_MINIMUM = $0000000A;
  GIFFIXEDMINIMUM = $64; // when all delays in file are <= 1 (10 ms)
  {$IFNDEF FPC}UOI_TIMERPROC_EXCEPTION_SUPPRESSION = 7;{$ENDIF}

function GifToWebp(Gif: TMovieGifImage; Webp: TMovieWebpImage;
  quality: Single = 75): Boolean; overload;

function GifToWebp(GifFileName, WebpFileName: string;
  quality: Single = 75): Boolean; overload;

procedure SaveFramesToFolder(FolderName: string;
  basefilename: string; Image: TMovieImageBase = nil;
  FileSource: string = ''; FileFormat: TSaveFramesFormat = sffJpeg;
  withtransparency: Boolean = true; writedelays: Boolean = false);

implementation

uses GifEncDec;

const
  sWebpImage = 'Webp Image File';
  sGifImage = 'Gif Image File';

var
  config: PWebPEncoderConfig;
  dconfig: PWebPDecoderConfig;

function WebpInitConfig: Integer;
begin
  Result:= 0;
  GetMem(config, sizeof(TWebpEncoderConfig));
  if WebPConfigPreset(config, WEBP_PRESET_PHOTO, 75) = 0 then
  begin
    config:= nil;
    exit;   // version error
  end;
  // ... additional tuning
  config^.sns_strength:= 90;
  config^.filter_sharpness:= 6;
  Result:= WebPValidateConfig(config);  // not mandatory, but useful
  GetMem(dconfig, sizeof(TWebpDecoderConfig));
  if WebPInitDecoderConfig(dconfig) = 0 then
    dconfig:= nil;
end;

procedure FinalizeConfigs;
begin
  FreeMem(config);
  FreeMem(dconfig);
end;

{$IFDEF FPC}
{$IFDEF WINDOWS}
{ TAssigntoBitmap }

procedure TAssigntoBitmap.AssignTo(Dest: TPersistent);
begin
  if Dest is TBitmap then
    TBitmap(Dest).Assign(Self)
  else if Dest is TWicGraphic then
    TWicGraphic(Dest).Assign(Self)
  else
    inherited Assignto(Dest);
end;

procedure TAssigntoBitmap.Assign(Source: TPersistent);
begin
  if Source is TWicGraphic then
    TWicGraphic(Source).AssignToBitmap(Self)
  else
    inherited Assign(Source);
end;

{$ENDIF}
{$ENDIF}

function GifToWebp(Gif: TMovieGifImage; Webp: TMovieWebpImage;
  quality: Single = 75): Boolean;
var
  cr: TColorRef;
  t: TBGRQuad;
  bm: array of TGraphic;
  I: Integer;
  delays: TWordDynArray;
  {$IFDEF WINDOWS}
  wic: TWicGraphic;
  wicbitmap: IWicBitmapGraphic;
  {$ENDIF}
begin
  if Gif = nil then
    raise Exception.Create('Invalid gif file');
  if Webp = nil then
    raise Exception.Create('Invalid webp file');
  try
    if Length(Gif.SelfGlobalTable) > 0 then
      cr:= RGB(Gif.SelfGlobalTable[Gif.BackgroundColor].rgbRed,
        Gif.SelfGlobalTable[Gif.BackgroundColor].rgbGreen,
        Gif.SelfGlobalTable[Gif.BackgroundColor].rgbBlue)
    else
      cr:= 0;
    t.rgbRed:= GetRValue(cr);
    t.rgbGreen:= GetGValue(cr);
    t.rgbBlue:= GetBValue(cr);
    t.rgbAlpha:= 255;
    webp.BackgroundColor:= t;
    webp.LoopCount:= Gif.LoopCount;
    bm:= nil;
    SetLength(bm, Gif.Frames.Count);
    delays:= nil;
    SetLength(delays, Gif.Frames.Count);
    WebpEncodingQuality:= quality;
    for I:= 0 to Gif.Frames.Count -1 do
    with TGifFrame(Gif.Frames[I]) do
    begin
      {$IFDEF WINDOWS}
      wic:= GetBitmapFromDecodedBytes(Gif.Width, Gif.Height,
        wicbitmap, true);
      if wic <> nil then
      begin
        wic.Handle:= wicbitmap;
        {$IFDEF FPC}
        bm[I]:= TAssignToBitmap.Create;
        {$ELSE}
        bm[I]:= TBitmap.Create;
        {$ENDIF}
        bm[I].Assign(wic);
      end;
      {$ELSE}
      bm[I]:= GetBitmapFromDecodedBytes(Gif.Width, Gif.Height, true);
      {$ENDIF}
      if Delay = 0 then
        delays[I]:= GIFFIXEDMINIMUM
      else
        delays[I]:= Max(Delay, USER_TIMER_MINIMUM);
    end;
    Result:= webp.MakeFromGraphicArray(bm, @delays, gif.LoopCount);
  finally
    for I:= 0 to Gif.Frames.Count -1 do
      Bm[I].Free;
    SetLength(Bm, 0);
    Finalize(Bm);
    SetLength(delays,  0);
    Finalize(delays);
  end;
end;

function GifToWebp(GifFileName, WebpFileName: string;
  quality: Single): Boolean;
var
  Gif: TMovieGifImage;
  Webp: TMovieWebpImage;
  FGif: TFileStream;
begin
  if ValidFileName(WebpFileName) <> WebpFileName then
    raise Exception.Create('Incorrect webp filename');
  if not FileExists(GifFileName) then
    raise Exception.Create('Gif file does not exist');
  Gif:= TMovieGifImage.Create;
  FGif:= TFileStream.Create(GifFileName, fmOpenRead or fmShareDenyWrite);
  try
    if Not TMovieImageBase.ValidGifStream(FGif) then
      raise Exception.Create('Gif file not a valid Gif file');
    FGif.Position:= 0;
    Gif.LoadFromStream(FGif);
    Webp:= TMovieWebpImage.Create;
    try
      Result:= GifToWebp(Gif, Webp, quality);
      if Result then
        Webp.SaveToFile(WebpFileName)
    finally
      Webp.Free;
    end;
  finally
    Gif.Free;
     FGif.Free;
  end;
end;

procedure SaveFramesToFolder(FolderName: string;
  basefilename: string; Image: TMovieImageBase;
  FileSource: string; FileFormat: TSaveFramesFormat;
  withtransparency: Boolean; writedelays: Boolean);
const
  sext: array[TSaveFramesFormat] of string = ('.jpg', '.png', '.webp');
var
{$IFDEF WINDOWS}
  wic: TWicGraphic;
  curbitmap, wicbitmap: IWicBitmapGraphic;
{$ELSE}
  png: TPortableNetworkGraphic;
  jp: TJpegImage;
{$ENDIF}
  I, J: Integer;
  delays: TStringList;
  goodframe: Boolean;
  WebpDest: TMovieWebpImage;
  sfilename: string;
  Stream: TFileStream;
  {$IFDEF FPC}
  {$IFDEF WINDOWS}
  NoTransBitmap: TAssignToBitmap;
  {$ELSE}
  NoTransBitmap: TBitmap;
  {$ENDIF}
  {$ELSE}
  NoTransBitmap: TBitmap;
  {$ENDIF}
  graphs: array of TGraphic;
  pnt: TPointArray;
  TransGif: TMovieGifImage;
begin
  FolderName:= IPath(FolderName{$IFDEF WINDOWS}, false{$ENDIF});
  if not DirectoryExists(FolderName) then
  begin
    if ErrorMessage('Confirm', '"' + FolderName + '" does not exist. ' +
      ' Do you want to create it?', mtConfirmation, [mbYes, MbNo]) = IdNo then
      exit
    else if not ForceDirectories(FolderName) then
      raise Exception.Create('"' + FolderName + '" could not be ' +
        'created');
  end;
  FileSource:= IPath(FileSource{$IFDEF WINDOWS}, true{$ENDIF});
  if ((Image = nil) and not FileExists(FileSource)) or
    ((Image <> nil) and FileExists(FileSource)) then
    raise Exception.Create('Both filesource and image cannot  be at '+
      'the same time null or not null');
  if FileExists(FileSource) then
    Stream:= TFileStream.Create(FileSource, fmOpenRead or fmshareDenyWrite)
  else
    Stream:= nil;
  {$IFDEF FPC}
  {$IFDEF WINDOWS}
  NoTransBitmap:= TAssignToBitmap.Create;
  {$ELSE}
  NoTransBitmap:= TBitmap.Create;
  {$ENDIF}
  {$ELSE}
  NoTransBitmap:= TBitmap.Create;
  {$ENDIF}
  try
    if Stream <> nil then
    begin
      if TMovieImageBase.ValidGifStream(Stream) then
      begin
        if withtransparency then
        begin
          Image:= TMovieWEbpImage.Create;
          TransGif:= TMovieGifImage.Create;
          Stream.Position:= 0;
          TransGif.LoadFromStream(Stream);
          if not GifToWebp(TransGif, TMovieWebpImage(Image),
            100) then
            exit;
        end
        else
        begin
          Image:= TMovieGifImage.Create;
          Image.LoadFromStream(Stream);
        end;
      end
      else if TMovieImageBase.ValidWebpStream(Stream) then
      begin
        Image:= TMovieWebpImage.Create;
        Image.LoadFromStream(Stream);
      end
      else
        raise exception.Create('The File source is not a ' +
          'valid movieimagebase file');
    end;
    if Image.Frames.Count = 0 then
      raise Exception.Create('The image does not contain frames');
    delays:= TStringlist.Create;
    try
      basefilename:= ValidFilename(basefilename);
      if not withtransparency then
        NoTransBitmap.SetSize(Image.Width, Image.Height);
      graphs:= nil;
      SetLength(graphs, 1);
      for I:= 0 to Image.Frames.Count -1 do
      begin
        goodframe:= false;
        J:= 0;
        while FileExists(IncludeTrailingPathDelimiter(FolderName) +
          basefilename + FormatFloat('000', J) + sext[FileFormat]) do
          Inc(J);
        sfilename:= IncludeTrailingPathDelimiter(FolderName) +
          basefilename + FormatFloat('000', J) + sext[FileFormat];
        if withtransparency then
        begin
          {$IFDEF WINDOWS}
          wic:= Image.Frames[I].GetBitmapFromDecodedBytes(
            Image.Frames[I].FrameWidth, Image.Frames[I].
            FrameHeight, wicBitmap);
          if wic <> nil then
          try
            curbitmap:= wic.Handle;
            wic.Handle:= wicbitmap;
            {$IFDEF FPC}
            graphs[0]:= TAssignToBitmap.Create;
            {$ELSE}
            graphs[0]:= TBitmap.Create;
            {$ENDIF}
            graphs[0].Assign(wic);
            if curbitmap <> nil then
            begin
              wic.Handle:= curbitmap;
              wicbitmap:= nil;
            end;
          finally
            wic.Free;
          end;
          {$ELSE}
          graphs[0]:= Image.Frames[I].GetBitmapFromDecodedBytes(Image.Frames[I].FrameWidth,
            Image.Frames[I].FrameHeight);
          {$ENDIF}
        end
        else
        begin
          Image.NeedCompleteRedraw:= true;
          Image.DrawFrame(NoTransBitmap.Canvas,
            Rect(0, 0, Image.Width, Image.Height), I);
          graphs[0]:= NoTransBitmap;
        end;
        if graphs[0] <> nil then
        begin
          if FileFormat = sffWebp then
          begin
            WebpDest:= TMovieWebpImage.Create;
            try
              pnt:= nil;
              SetLength(pnt, 1);
              pnt[0]:= Point(0, 0);
              if not WebpDest.MakeFromGraphicArray(graphs, nil, 0,
                0, @pnt) then
                continue;
              goodframe:= true;
              WebpDest.SaveToFile(sfilename);
            finally
              WebpDest.Free;
              SetLength(pnt, 0);
              Finalize(pnt);
            end;
          end
          else
          begin
            {$IFDEF WINDOWS}
            wic:= TWicGraphic.Create;
            try
              wic.Assign(graphs[0]);
              if FileFormat = sffJpeg then
                wic.ImageFormat:= wifJpeg
              else
                wic.ImageFormat:= wifPng;
              wic.SaveToFile(sfilename);
            finally
              wic.Free;
            end;
            {$ELSE}
            if FileFormat = sffPng then
            begin
              png:= TPortableNetworkGraphic.create;
              try
                png.Assign(graphs[0]);
                png.SaveToFile(sfilename);
              finally
                png.Free;
              end
            end
            else
            begin
              jp:= TJpegImage.Create;
              try
                jp.Assign(graphs[0]);
                jp.SaveToFile(sfilename);
              finally
                jp.Free;
              end;
            end;
            {$ENDIF}
            goodframe:= true;
          end;
        end;
        if goodframe and writedelays then
          delays.Add(IntToStr(I + 1) + '= ' +
            IntToStr(Image.Frames[I].Delay));
      end;
      if delays.Count > 0 then
        delays.SaveToFile(IncludeTrailingPathDelimiter(FolderName) +
          basefilename + '.txt', TEncoding.UTF8);
    finally
      delays.Free;
      Image.Free;
      MessageBox(0, 'Done', 'Info', 0);
    end;
  finally
    NoTransBitmap.Free;
  end;
end;

procedure DrawGifBackground(FWidth, FHeight: Integer;
  ARect: TRect; table: TBGrTripleArray;
  FBackgroundIndex: Byte; var DestBits: TBytes);
var
  Row, Col: Integer;
  stride: Integer;
  P: PBgrTriple;
begin
  stride:= ((((FWidth * 24) + 31) and not 31) shr 3);
  SetLength(DestBits, stride * FHeight);
  for Row:= ARect.Top to ARect.Height - 1 do
  begin
    P:= @DestBits[stride * Row + (ARect.Left * 3)];
    Col:= ARect.Width;
    while Col > ARect.Left do
    begin
      P^:= table[FBackgroundIndex];
      Inc(P);
      Dec(Col);
    end;
  end;
end;

{ TIMageBase }

procedure TImageBase.Clear;
begin
  inherited;
  if Self is TNoMovieImage then
  begin
    TNoMovieImage(Self).NmImage.Free;
    {$IFDEF WINDOWS}
    TNoMovieImage(Self).NmImage:= TWicGraphic.Create;
    {$ELSE}
    TNoMovieImage(Self).NmImage:= TPicture.Create;
    {$ENDIF}
  end
  else
  with TMovieImageBase(Self) do
  begin
    Frames.Clear;
    if Self is TMovieGifImage then
      SetLength(TMovieGifImage(self).SelfGlobalTable, 0);
  end;
  FWidth:= 0;
  FHeight:= 0;
end;

procedure TImageBase.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  if Self is TNoMovieImage then
    Acanvas.StretchDraw(Rect, TNoMovieImage(Self).NmImage
    {$IFNDEF WINDOWS}.Graphic{$ENDIF})
  else if Self is TMovieImageBase then
    TMovieImageBase(Self).DrawFrame(ACanvas, Rect, 0);
end;

function TImageBase.GetHeight: Integer;
begin
  if Self is TNoMovieImage then
    Result:= TNoMovieImage(Self).NmImage.Height
  else
    Result:= FHeight;
end;

function TImageBase.GetWidth: Integer;
begin
  if Self is TNoMovieImage then
    Result:= TNoMovieImage(Self).NmImage.Width
  else
    Result:= FWidth;
end;

procedure TImageBase.LoadFromFile(const FileName: string);
var
  F: TfileStream;
begin
  if not FileExists(FileName) then
    raise Exception.Create('File does not exist');
  F:= TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(F);
    FFileName:= FileName;
  finally
    F.Free;
  end;
end;

procedure TImageBase.SaveToFile(const FileName: string);
var
  F: TFileStream;
begin
  F:= TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TImageBase.SetCanvas(Value: TCanvas);
begin
  if (Value <> nil) and ((FCanvas = nil) or (FCanvas.Handle <>
    Value.Handle)) then
  begin
    FCanvas:= Value;
    FCanvasRect:= FCanvas.ClipRect;
  end;
end;

{ TNoMovieImage }

constructor TNoMovieImage.Create;//(NewCanvas: TCanvas);
begin
  {$IFDEF WINDOWS}
  NmImage:= TWicGraphic.Create;
  {$ELSE}
  NmImage:= TPicture.Create;
  {$ENDIF}
end;

destructor TNoMovieImage.Destroy;
begin
  NmImage.Free;
  inherited;
end;

procedure TNoMovieImage.CopyToClipboard;
{$IFDEF FPC}
{$IFDEF WINDOWS}
var
  bm: TAssignToBitmap;
{$ENDIF}
{$ENDIF}
begin
  {$IFDEF FPC}
  {$IFDEF WINDOWS}
  bm:= TAssignTobitmap.Create;
  try
    bm.Assign(NmImage);
    Clipboard.Assign(bm);
  finally
    bm.Free;
  end;
  exit;
  {$ENDIF}
  {$ENDIF}
  Clipboard.Assign(NmImage{$IFDEF FPC}.Bitmap{$ENDIF});
end;

function TNoMovieImage.PasteFromClipboard: Boolean;
{$IFDEF FPC}
var
  Bitmap: TBitmap;
{$ENDIF}
begin
  {$IFDEF FPC}
  Result:= Clipboard.HasFormat(PredefinedClipboardFormat(pcfBitmap));
  {$ELSE}
  Result:= Clipboard.HasFormat(CF_BITMAP) or Clipboard.HasFormat(CF_DIB);
  {$ENDIF}
  if Result then
  begin
    Clear;
    {$IFDEF FPC}
    Bitmap:= TBitmap.Create;
    try
      Bitmap.LoadFromClipboardFormat(PredefinedClipboardFormat(pcfBitmap));
      if Bitmap.Width > 0 then
        NmImage.Assign(Bitmap);
    finally
      Bitmap.Free;
    end;
    {$ELSE}
    NMImage.Assign(Clipboard);
    {$ENDIF}
    Result:= NmImage.Width > 0;
  end;
end;

procedure TNoMovieImage.DrawFrame(ACanvas: TCanvas; ARect: TRect;
  pfIndex: Integer);
begin
  Draw(ACanvas, ARect);
end;

procedure TNoMovieImage.LoadFromStream(Stream: TStream);
begin
  NmImage.LoadFromStream(Stream);
end;

procedure TNoMovieImage.SaveToStream(Stream: TStream);
begin
  NmImage.SaveToStream(Stream);
end;

{ TMovieImageBase }

constructor TMovieImageBase.Create;
begin
  inherited Create;
  SourceAnimated:= 0;
  FAnimationSpeed:= 100;
  SetLength(FCurrentBits, 0);
  FTimer:= nil;
  Frames:= TMovieFrameList.Create(Self);
  FWidth:= 0;
  FHeight:= 0;
  FFileName:= '';
  FNeedCompleteRedraw:= false;
end;

destructor TMovieImageBase.Destroy;
begin
  FreeAndNil(FTimer);
  Frames.Free;
  inherited;
end;

procedure TMovieImageBase.Animate;
{$IFNDEF FPC}
var
  h: THandle;
  b: LONGBOOL;
{$ENDIF}
begin
  if (LoopIndex > 0) or (Frames.Count < 2) or (FTimer <> nil) or
    (Sourceanimated < 2) then
    exit;
  {$IFNDEF FPC}
  h:= GetCurrentProcess;
  if h <> 0 then
  begin
    b:= false;
    if not SetUserObjectInformationW(h,
      UOI_TIMERPROC_EXCEPTION_SUPPRESSION, @b, sizeof(LONGBOOL)) then
      exit;    // in settimer api webpage this is recommended.
               // it can be removed or implemented but
               // without exit on error if desired.
  end;
  {$ENDIF}
  FCurrentFrame:= 0;
  if Ftimer <> nil then
    FTimer.Free;
  FTimer:= TTimer.Create(nil);
  FTimer.Interval:= USER_TIMER_MINIMUM;
  FTimer.Enabled:= true;
  FTimer.OnTimer:= {$IFDEF FPC}@{$ENDIF}AnimateTimer;
  FLoopIndex:= 1;
end;

procedure TMovieImageBase.AnimateTimer(Sender: TObject);
var
  Delay: Integer;
  MsgDlgEx: TMsgDlgEx;
  {$IFNDEF FPC}
  Wnd: HWND;
  stext: string;
  {$ENDIF}
begin
  try
    {$IFNDEF FPC}
    Wnd := GetActiveWindow;
    if IsWindow(Wnd) then
    begin
      SetLength(stext, MAX_PATH);
      SetLength(stext, GetWindowtext(Wnd, PChar(stext), MAX_PATH));
      if stext = 'Confirm' then
        exit;
    end;
    {$ENDIF}
    FTimer.Enabled:= false;
    FExternalDrawing:= true;
    FNeedCompleteRedraw:= FCurrentFrame = 0;
    if IsRectEmpty(FCanvasRect) then
      FCanvasRect:= FCanvas.ClipRect;
    DrawFrame(FCanvas, FCanvasRect, FCurrentFrame);
    FNeedCompleteRedraw:= true;
    Delay:= Max(FMinimumDelay, Min(10000,
      Frames.Frame[FCurrentFrame].Delay));
    if FrameIndex < Frames.Count -1 then
    begin
      if (Self is TMovieGifImage) and
        TGifFrame(Frames.Frame[FCurrentFrame]).UserInput then
      begin
        if Frames.Frame[FCurrentFrame].Delay = 0 then
          ErrorMessage('Warning', 'Press ENTER or ESCAPE to continue',
            mtConfirmation, [mbOk])
        else
        begin
          MsgDlgEx:= TMsgDlgEx.Create(
            'Pulsa Intro o Escape para continuar',
            mtConfirmation, [mbOk], Application.MainForm, mrOk, Delay);
          try
            msgDlgEx.ShowDialog;
          finally
            MsgDlgEx.Free;
          end;
        end;
        FTimer.Interval:= 10;
      end
      else
        FTimer.Interval:= Round(Delay / FAnimationSpeed * 100);
    end;
    FTimer.Enabled:= true;
    if FCurrentFrame < Frames.Count -1 then
    begin
      Inc(FCurrentFrame);
      if Assigned(FOnNextFrame) then
        FOnNextFrame(Sender);
    end
    else if (LoopCount = 0) or (FLoopIndex < LoopCount) or
      InfiniteLoopAnyways then
    begin
      FCurrentFrame:= 0;
      Inc(FLoopIndex);
      FNeedCompleteRedraw:= true;
      if Assigned(FOnLoop) then
        FOnloop(Self);
    end
    else
    begin
      FTimer.Enabled:= false;
      FLoopIndex:= 0;
      FreeAndNil(FTimer);
    end;
  except
    floopindex:= 0;
  end;
end;

procedure TMovieImageBase.CopyToClipboard;
var
{$IFDEF WINDOWS}
  p: Pointer;
  h: THandle;
{$ENDIF}
  m: TMemoryStream;
begin
  {$IFDEF WINDOWS}
  if OpenClipboard(0) then
  begin
    if not EmptyClipboard then
      exit;
    m:= TMemoryStream.Create;
    try
      SaveToStream(m);
      m.Position:= 0;
      h:= GlobalAlloc($42, m.Size);
      if h <> 0 then
      begin
        p:= GlobalLock(h);
        Move(m.Memory^, p^, m.Size);
        GlobalUnlock(h);
        if Self is TMovieGifImage then
          SetClipboardData(CF_MGIF, h)
        else
          SetClipboardData(CF_MWEBP, h);
      end;
    finally
      m.Free;
    end;
    CloseClipboard;
  end;
  {$ELSE}
  Clipboard.Open;
  m:= TMemoryStream.Create;
  try
    Clipboard.Clear;
    SaveToStream(m);
    m.Position:= 0;
    if Self is TMovieGifImage then
      Clipboard.SetFormat(CF_MGIF, m)
    else
      Clipboard.SetFormat(CF_MWEBP, m);
  finally
    m.Free;
    Clipboard.Close;
  end;
  {$ENDIF}
end;

procedure TMovieImageBase.DrawDecodedBytes(ACanvas: TCanvas; ARect: TRect;
  pfIndex: Integer; BlendedBits: TBytes);
{$IFDEF WINDOWS}
var
  wic: TWicGraphic;
  curbitmap, wicbitmap: IWicBitmapGraphic;
{$ELSE}
var
  bm: TBitmap;
{$ENDIF}
begin
  with Frames.Frame[pfIndex] do
  begin
    {$IFDEF WINDOWS}
    wic:= GetBitmapFromDecodedBytes(FWidth, FHeight, wicBitmap,
      false, BlendedBits);
    if wic <> nil then
    try
      curbitmap:= wic.Handle;
      try
        wic.Handle:= wicbitmap;
        wic.ImageFormat:= wifJpeg;
        ACanvas.StretchDraw(ARect, wic);
      finally
        if curbitmap <> nil then
        begin
          wic.Handle:= curbitmap;
          wicbitmap:= nil;
        end;
      end;
    finally
      wic.Free;
    end;
    {$ELSE}
    bm:= GetBitmapFromDecodedBytes(FWidth, Fheight, false, BlendedBits);
    if bm <> nil then
    try
      ACanvas.StretchDraw(ARect, bm);
    finally
      bm.Free;
    end;
    {$ENDIF}
  end;
end;

function TMovieImageBase.GetAnimating: Boolean;
begin
  Result:= LoopIndex > 0;
end;

function TMovieImageBase.GetPaused: Boolean;
begin
  Result:= (fTimer <> nil) and not FTimer.Enabled;
end;

class function TMovieImageBase.ImageCanBeAnimated(
  NameOfFile: string; silent: Boolean): Boolean;
var
  s: string;
  packedfield, sep: Byte;
  colortablesize: Word;
  m: TmemoryStream;
  minlzwsize: Byte;
  extensiontype: Byte;
  blocklength: Byte;
  fillbytes: Integer;
  imagecount: Byte;

  Stream: TFileStream;
  fourcc: array[0..3] of AnsiChar;
  chunksize: Cardinal;
  icount: Integer;


  function BlockRight(blength: Integer = -1): Boolean;
  var
    setendblock: Boolean;
  begin
    setendblock:= blength = -1;
    if setendblock then
      blength:= blocklength;
    Result:= true;
    fillbytes:= m.Position + blength;
    if m.Size - fillbytes <= 0 then  // corruption
      Result:= false;
  end;

  procedure ProcessBlock;
  begin
    repeat
      m.Read(blocklength, 1);
      if blocklength > 0 then
      begin
        if BlockRight then
          m.Seek(blocklength, soCurrent)
        else
          sep:= $00;
      end;
    until (blocklength = 0) or (sep = $00);
  end;

begin
  Result:= false;
  if not FileExists(NameOfFile) then
    if not silent then
      raise Exception.Create('File does not exist')
    else
      exit;
  s:= string(GetImageExtensionBySignature(NameOfFile));
  if s = '.gif' then
  begin
    m:= TmemoryStream.Create;   // I better not reuse the load stream method because
                                // I want this to be a class method.
    try
      m.LoadFromFile(NameOfFile);
      imagecount:= 0;
      if not ValidGifStream(m) then
        exit;
      m.Seek($A, soBeginning);
      m.Read({%H-}packedfield, 1);
      m.Seek(2, soCurrent);
      if GetBitsValue(packedfield, 0, 1) = 1 then
      begin
        colortablesize:= Round(Power(2, GetBitsValue(packedfield, 5, 3) + 1) * 3);
        if not BlockRight(colortablesize) then
          exit;
        m.Seek(colortablesize, soCurrent);
      end;
      repeat
        m.Read(sep, 1);
        if sep = $2C then
        begin
          Inc(imagecount);
          if imagecount = 2 then
            exit;
          m.Seek(8, soCurrent);
          m.Read(packedfield, 1);
          if GetBitsValue(packedfield, 0, 1) = 1 then
          begin
            colortablesize:= Round(Power(2, GetBitsValue(packedfield, 5,3) + 1) * 3);
            if not BlockRight(colortablesize) then
              sep:= $00
            else
              m.Seek(colortablesize, soCurrent);
          end;
          m.Read({%H-}minlzwsize, 1);
          ProcessBlock;
        end
        else if sep = $21 then
        begin
          m.Read({%H-}extensiontype, 1);
          case extensiontype of
            $F9: // control extension
            begin
              m.Read(blocklength, 1);
              if blocklength = 4 then
                m.Seek(5, soCurrent)
              else
                m.Seek(-1, socurrent);
            end;
            $FE: ProcessBlock;
            $01: // plain text extension
            begin
              m.Read(blocklength, 1);
              if blocklength <> 12 then
              begin
                m.Seek(-1, soCurrent);
                continue;
              end;
              m.Seek(blocklength, soCurrent);
              ProcessBlock;
            end;
            $FF: // application extension
            begin
              m.Read(blocklength, 1);
              if blocklength <> 11 then
              begin
                m.Seek(-1, soCurrent);
                continue;
              end;
              if BlockRight then
              begin
                m.Seek(blocklength, soCurrent);
                ProcessBlock;
              end
              else
                sep:= $00;
            end;
          end;
        end;
      until ((sep <> $2C) and (sep <> $21));
    finally
      m.free;
    end;
  end
  else if s= '.webp' then
  begin
    Stream:= TFileStream.Create(NameOfFile, fmOpenRead or
      fmShareDenyWrite);
    try
      Stream.Seek(8, soBeginning);
      Stream.Read({%H-}fourcc, 4);
      icount:= 0;
      if ArrayToAnsiString(fourcc) <> 'WEBP' then
        exit;
      Stream.Read(fourcc, 4);
      if ArrayToAnsiString(fourcc) <> 'VP8X' then
        exit;
      repeat
        Stream.Read({%H-}chunksize, 4);
        if chunksize mod 2 <> 0 then
          Inc(chunksize);
        if (fourcc = 'VP8 ') or (fourcc = 'VP8L') then
        begin
          if chunksize = 0 then
            exit;
          Inc(icount);
          if icount = 2 then
          begin
            Result:= true;
            exit;
          end;
        end;
        if fourcc = 'ANMF' then
          Stream.Seek(16, soCurrent)
        else
          Stream.Seek(chunksize, soCurrent);
        if Stream.Position < Stream.Size - 4 then
          Stream.Read(fourcc, 4);
      until Stream.Position >= Stream.Size - 4;
    finally
      Stream.Free;
    end;
  end;
end;

function TMovieImageBase.LoopDurationinMs: Cardinal;
var
  I: Integer;
begin
  Result:= 0;
  if (Frames.Count < 2) or (SourceAnimated < 2) then
    exit;
  for I:= 0 to Frames.Count -1 do
    Inc(Result, Max(FMinimumDelay, Frames.Frame[I].Delay));
end;

function TMovieImageBase.PasteFromClipboard: Boolean;
var
{$IFDEF WINDOWS}
  h: THandle;
  p: Pointer;
{$ENDIF}
  m: TmemoryStream;
begin
  Result:= false;
  try
    {$IFDEF WINDOWS}
    if not OpenClipboard(0) then
      exit;
    if Self is TMovieWebpImage then
      h:= GetClipboardData(CF_MWEBP)
    else
      h:= GetclipboardData(CF_MGIF);
    if h = 0 then
    begin
      CloseClipboard;
      exit;
    end;
    if (h <> 0) and (h <> INVALID_HANDLE_VALUE) then
    begin
      p:= GlobalLock(h);
      if p = nil then
        exit;
      m:= TMemoryStream.Create;
      try
        try
          m.Size:= GlobalSize(h);
          m.Write(p^, GlobalSize(h));
          GlobalUnlock(h);
          CloseClipboard;
          Clear;
          m.Position:= 0;
          LoadFromStream(m);
          Result:= true;
        except
          Result:= false;
        end;
      finally
        m.Free;
      end;
    end;
    {$ELSE}
    //Clipboard.Open;
    m:= TMemoryStream.Create;
    try
      if (Self is TMovieGifImage) and Clipboard.HasFormat(CF_MGIF) then
        Result:= Clipboard.GetFormat(CF_MGIF, m)
      else if (Self is TMovieWebpImage) and Clipboard.HasFormat(CF_MWEBP) then
        Result:= Clipboard.GetFormat(CF_MWEBP, m);
      if Result and (m.Size > 0) then
      begin
        m.Position:= 0;
        LoadFromstream(m);
      end;
    finally
      //Clipboard.Close;
      m.Free;
    end;
    {$ENDIF}
  except
    if Result then
      Result:= false;
  end;
end;

procedure TMovieImageBase.PauseAnimation;
begin
  if (FTimer <> nil) and Ftimer.Enabled then
    FTimer.Enabled:= false;
end;

procedure TMovieImageBase.RestoreAnimation;
begin
  if (FTimer <> nil) and not Ftimer.Enabled then
    FTimer.Enabled:= true;
end;

procedure TMovieImageBase.StopAnimation;
begin
  if (FTimer = nil) or not FTimer.Enabled then
    exit;
  FloopIndex:= 0;
  FreeAndNil(FTimer);
end;

class function TMovieImageBase.ValidGifStream(Stream: TStream): Boolean;
var
  gif: array[0..2] of Byte;
  iver: Boolean;
  curpos: Int64;
begin
  Result:= false;
  curpos:= Stream.Position;
  try
    Stream.Read({%H-}gif, 3);
    if ((gif[0] <> $47) and (gif[0] <> $50)) or (gif[1] <> $49) or
      (gif[2] <> $46) then
      exit;
    iver:= Boolean(gif[0] = $47);
    Stream.Read(gif, 3);
    if iver then
    begin
      if not ((gif[0] = $38) and (gif[1] = $39) and
        (gif[2] = $61)) and
        not ((gif[0] = $38) and (gif[1] = $37) and
        (gif[2] = $61)) then
        exit;
    end
    else if not ((gif[0] = $39) and (gif[1] = $39) and
      (gif[2] = $61)) then
      exit;
    Result:= true;
  finally
    Stream.Position:= curpos;
  end;
end;

class function TMovieImageBase.ValidWebpStream(Stream: TStream): Boolean;
var
  fourcc: array[0..3] of AnsiChar;
  a: AnsiString;
  filesize: Integer;
  curpos: Int64;
begin
  curpos:= stream.Position;
  try
    Result:= Stream.Size > 20;
    if not Result then
      exit;
    Stream.Read({%H-}fourcc, 4);
    if ArrayToAnsiString(fourcc) <> 'RIFF' then
      exit;
    Stream.Read({%H-}filesize, 4);
    if Stream.Size {%H-}< filesize + 8 then
      exit;
    Stream.Read(fourcc, 4);
    if ArrayToAnsiString(fourcc) <> 'WEBP' then
      exit;
    Stream.Read(fourcc, 4);
    a:= ArrayToAnsiString(fourcc);
    Result:= (a = 'VP8X') or (a = 'VP8 ') or (a = 'VP8L');
  finally
    Stream.Position:= curpos;
  end;
end;

{ TMovieFrameList }

constructor TMovieFrameList.Create(Parent: TMovieImageBase);
begin
  inherited Create;
  Self.MyBase:= Parent;
end;

function TMovieFrameList.GetFrame(Index: Integer): TMovieImageFrame;
begin
  Result:= TMovieImageFrame(Items[Index]);
end;

{ TMovieGifImage}

constructor TMovieGifImage.Create;//(NewCanvas: TCanvas = nil);
begin
  inherited;//(NewCanvas);
  FBackgroundColor:= 0;
  SourceAnimated:= 2;
  FPlainTextFontName:= '';
  AppExtensions:= TObjectList.Create;
end;

destructor TMovieGifImage.Destroy;
begin
  AppExtensions.Free;
  inherited;
end;

procedure TMovieGifImage.AddNetscapeExtension(Loops: Word);
const
  snet: AnsiString = 'NETSCAPE';
var
  Netscape: TGifAppExtension;
  id: array[0..7] of AnsiChar;
  bt: Byte;
  wrd: Word;
begin
  {$IFNDEF FPC}
  System.AnsiStrings.StrCopy(id, PAnsiChar(snet));
  {$ELSE}
  StrCopy(id, PAnsiChar(snet));
  {$ENDIF}
 // Move(snet, id, 8);
  Netscape:= TGifAppExtension.Create(id);
  Netscape.AuthCode:= '2.0';
  bt:= 1;
  Netscape.AppStream.Write(bt, 1);
  wrd:= Loops;
  Netscape.AppStream.Write(wrd, 2);
  AppExtensions.Add(Netscape);
end;

function TMovieGifImage.CanBeAnimated: Boolean;
begin
  Result:= SourceAnimated = 2;
end;

function TMovieGifImage.DecodeFrame(pfIndex: Integer): Boolean;
var
  tr: Byte;
begin
  with TGifFrame(Frames[pfIndex]) do
  begin
    if FTransparentFlag then
      tr:= TransparentColorIndex
    else
      tr:= 0;
    Result:= DecodeGifFrame(EncodedStream, tr,
      Interlaced, FrameWidth, FrameHeight, FWidth,
      FHeight, DecodedBits);
  end;
end;

procedure TMovieGifImage.DrawBackground(ARect: TRect;
  table: TBGRTripleArray);
begin
  DrawGifBackground(FWidth, FHeight, ARect, table,
    FBackgroundColor, FCurrentBits);
end;

function Rect(Left, Top, Right, Bottom: Integer):
  {$IFNDEF FPC}System.Types.TRect{$ELSE}Types.TRect{$ENDIF};
begin
  {$IFNDEF FPC}
  Result:= System.Types.Rect(Left, Top, Right, Bottom);
  {$ELSE}
  Result:= Types.Rect(Left, Top, Right, Bottom);
  {$ENDIF}
end;

procedure TMovieGifImage.DrawFrame(ACanvas: TCanvas; ARect: TRect;
  pfIndex: Integer);
var
  PreviousBits: TBytes;
  I: Integer;
  colortable: TBgrTripleArray;
  bm: TBitmap;
  stext: AnsiString;
  completedx, cellx, celly, cellsizex, cellsizey: Integer;
  scalex, scaley: Single;
  slen: Integer;
  BlendedBits: Tbytes;
  {$IFNDEF FPC}
  transr, transg, transb: Byte;
  {$ENDIF}
begin
  if (pfIndex < 0) or (pfIndex > Frames.Count - 1) then
    exit;
  FFrameIndex:= pfIndex;
  if Assigned(FOnBeforeDrawingFrame) then
    FOnBeforeDrawingFrame(Self);
  with TGifFrame(Frames.Frame[pfIndex]) do
  begin
    if Length(LocalColorTable) > 0 then
      colortable:= LocalColorTable
    else
      colortable:= GlobalTable;
    if (Length(FCurrentBits) = 0) or (Acanvas <> FCanvas) or
      (pfIndex = 0) or FNeedCompleteRedraw then
    begin
      DrawBackground(Rect(0, 0, FWidth, FHeight),
        colortable);
      if ACanvas <> FCanvas then
        FCanvas:= ACanvas;
    end;
    if FNeedCompleteRedraw and (Frames.Count > 1) then
    begin
      FExternalDrawing:= true;
      FNeedCompleteRedraw:= false;
      for I:= 0 to pfIndex - 1 do
        DrawFrame(Acanvas, ARect, I);
      FNeedCompleteRedraw:= true;
    end;
    if (Length(DecodedBits) = 0) and ((TextBlock = nil) or
      (Length(GlobalTable) = 0))  then
      exit;
    try
      if Disposal = dmRestorePrev then
        PreviousBits:= Copy(FCurrentBits, 0, High(Integer))
      else
        SetLength(previousbits, 0);
      if Length(DecodedBits) = 0 then
      begin
        if Length(previousbits) > 0 then
        begin
          BlendedBits:= Copy(PreviousBits, 0, High(Integer));
          DrawDecodedBytes(ACanvas, ARect, pfIndex, BlendedBits);
          SetLength(BlendedBits, 0);
          Finalize(BlendedBits);
        end;
        stext:= ArrayToAnsiString(TextBlock^.TextData);
        bm:= TBitmap.Create;
        try
          with bm do
          begin
            scalex:= ARect.Width / FWidth;
            scaley:= ARect.Height / FHeight;
            completedx:= 0;
            cellx:= Round(FrameLeft * scalex) + ARect.Left;
            celly:= Round(FrameTop * scaley) + Arect.Top;
            cellsizex:= Round(TextBlock^.CellSizeX * scalex);
            cellsizey:= Round(TextBlock^.CellSizey * scaley);
            {$IFNDEF FPC}
            if FTransparentFlag then
            begin
              transb:= GlobalTable[TransparentColorIndex].rgbBlue;
              transg:= GlobalTable[TransparentColorIndex].rgbGreen;
              transr:= GlobalTable[TransparentColorIndex].rgbRed;
            end
            else
            begin
              transb:= 0; transg:= 0; transr:= 0;
            end;
             {$ENDIF}
            ACanvas.Font.Name:= MonoFonts[Random(MonoFonts.Count)];
            ACanvas.Font.Height:= Round(-(TextBlock^.CellSizeY - 1) *
              scaley);
            try
              slen:= Length(stext);
              for I:= 1 to slen do
              begin
                SetSize(ACanvas.TextWidth(string(stext[I])),
                  ACanvas.TextHeight(string(stext[I])));
                with bm.Canvas do
                begin
                  Font.Name:= ACanvas.Font.Name;
                  Font.Height:= Acanvas.Font.Height;
                  with GlobalTable[TextBlock^.ForeColorIndex] do
                    Font.Color:= RGB(rgbRed, rgbGreen, rgbBlue);
                  with GlobalTable[TextBlock^.BackColorIndex] do
                    Brush.Color:= RGB(rgbRed, rgbGreen, rgbBlue);
                  Brush.Style:= bsSolid;
                  {$IFDEF WINDOWS}
                  SetBkMode(Handle, {$IFNDEF FPC}Winapi.{$ENDIF}Windows.TRANSPARENT);
                  {$ELSE}
                  SetBkMode(Handle, LclType.TRANSPARENT);
                  {$ENDIF}
                  FillRect(ClipRect);
                  TextOut(0, 0, string(stext[I]));
                  {$IFNDEF FPC}  // this doesn't work in FPC, at least in Windows.
                                 // It can be done another way, but it's not worth it, in my opinion.
                  if FTransparentFlag and
                    (TextBlock^.BackColorIndex = TransparentColorIndex) then
                  begin
                    bm.Transparent:= true;
                    bm.TransparentColor:= RGB(transr, transg, transb);
                    bm.TransparentMode:= tmFixed;
                  end;
                  {$ENDIF}
                  ACanvas.StretchDraw(Rect(cellx, celly,
                    cellx + cellsizex, celly + cellsizey), bm);
                end;
                Inc(cellx, cellsizeX);
                Inc(completedx, TextBlock^.CellSizeX);
                if completedx >= FrameWidth then
                begin
                  completedx:= 0;
                  Inc(celly, cellsizey);
                  cellx:= Round(FrameLeft * scalex) + Arect.Left;
                end;
              end;
            except
            end;
          end;
        finally
          bm.Free;
        end;
        exit;
      end
      else
      begin
        if (pfIndex < Frames.Count -1) and
          (frames.Frame[pfIndex + 1].Disposal =
          dmRestorePrev) then
          PreviousBits:= Copy(FCurrentBits, 0,
            High(Integer));
        GetCurrentBits(FCurrentBits, colortable, FWidth, FHeight);
        if Length(BlendedBits) <> Length(FCurrentBits) then
          SetLength(BlendedBits, Length(FCurrentBits));
        BlendedBits:= Copy(FCurrentBits, 0, High(Integer));
        DrawDecodedBytes(ACanvas, ARect, pfIndex, BlendedBits);
        SetLength(BlendedBits, 0);
        Finalize(BlendedBits);
      end;
    finally
      if Frames.count > 1 then
        if (Disposal = dmRestorePrev) and
          (Length(PreviousBits) <> 0) then
          FCurrentBits:= Copy(PreviousBits, 0, High(Integer))
        else if Disposal = dmBackgroundColor then
          DrawBackground(Rect(0, 0, FWidth,
            FHeight), colortable);
      if Assigned(FOnAfterFrameDrawn) then
        FOnAfterFrameDrawn(Self);
    end;
  end;
end;

procedure TMovieGifImage.EncodeFrame(pfIndex: Integer);
var
  I, maxcolors: Integer;
begin
  maxcolors:= 0;
  for I:= 0 to Frames.Count -1 do
    with TGifFrame(Frames[I]) do
      if Length(LocalColorTable) > maxcolors then
        maxcolors:= Length(LocalColorTable);

  with TGifFrame(Frames[pfIndex]) do
    EncodeGifFrame(EncodedStream, FWidth, FHeight,
      Interlaced, DecodedBits, SelfGlobalTable,
      maxcolors);
end;

{$POINTERMATH ON}

procedure TMovieGifImage.LoadFromStream(Stream: TStream);
var
  sep: Byte;
  colortablesize: Word;
  w: Word;
  extensiontype: Byte;
  blocklength: Byte;
  bt: Byte;
  fillbytes: Integer;
  frame: TGifFrame;
  AppIdentifier: array[0..7] of AnsiChar;
  AppSubIdentifier: array[0..2] of AnsiChar;
  delaysset: Integer;
  AppExtension: TGifAppExtension;

  function ReadCheck(var Buffer; Size: LongInt): Boolean;
  var
    ReadSize: integer;
  begin
    ReadSize := Stream.Read(Buffer, Size);
    Result:= ReadSize = Size;
  end;

  function BlockRight(blength: Integer = -1): Boolean;
  var
    setendblock: Boolean;
  begin
    setendblock:= blength = -1;
    if setendblock then
      blength:= blocklength;
    Result:= true;
    fillbytes:= Stream.Position + blength;
    if Stream.Size - fillbytes <= 0 then
      Result:= false;
  end;

  function ProcessBlock: Boolean;
  begin
    Result:= false;
    repeat
      if not ReadCheck(blocklength, 1) then
        exit;
      if (blocklength > 0) and (sep <> 0) then
      begin
        Result:= BlockRight;
        if Result then
        begin
          if sep = $2C then
          begin
            frame.EncodedStream.Write(blocklength, 1);
            frame.EncodedStream.CopyFrom(Stream, blocklength);
            //Stream.Seek(blocklength, socurrent);
            continue;
          end
          {$IFNDEF DOPLAINTEXT}
          else if (sep = $21) and (extensiontype = $01) then
          begin
            SetLength(frame.TextBlock^.TextData, blocklength);
            Result:= ReadCheck((@frame.TextBlock^.TextData[0])^,
              blocklength);
            if not Result then
              break;
            continue;
          end
          {$ENDIF}
          else if (sep = $21) and (extensiontype = $FF) then
          begin
            if AppIdentifier = 'NETSCAPE' then
            begin
              Result:= ReadCheck(bt, 1);
              if bt = 1 then
              begin
                Result:= ReadCheck(w, 2);
                if not Result then
                  break;
                begin
                  if w = 0 then
                    LoopCount:= 0
                  else
                    LoopCount:= w + 1;
                end;
              end;
              Stream.Seek(-3, soCurrent);
            end;
            AppExtension.AppStream.CopyFrom(Stream, blocklength);
            continue;
          end;
          Stream.Seek(blocklength, soCurrent)
        end;
      end;
    until Result and ((blocklength = 0) or (sep = $00));
  end;

begin
  SourceAnimated:= 1;
  delaysset:= 0;
  try
    if not ValidGifStream(Stream) then
      exit;
    Stream.Position:= 6;
    sep:= 1;
    if not ReadCheck(w, 2) then
      raise Exception.Create('Bad file');
    FWidth:= w;
    if not ReadCheck(w, 2) then
      raise Exception.Create('Bad file');
    FHeight:= w;
    if not ReadCheck(FDescriptorpackedfield, 1) then
      raise Exception.Create('Bad file');
    if not ReadCheck(bt, 1) then
      raise Exception.Create('Bad file');
    FBackgroundIndex:= bt;
    if not ReadCheck(bt, 1) then
      raise Exception.Create('Bad file');
    FPixelRatio:= bt; // if not 0, = (+ 15) / 64
    if GetBitsValue(FDescriptorpackedfield, 0, 1) = 1 then
    begin
      colortablesize:= Round(Power(2, GetBitsValue(FDescriptorpackedfield,
        5, 3) + 1) * 3);
      SetLength(SelfGlobalTable, colortablesize div 3);
      if not ReadCheck((@SelfGlobalTable[0])^, colortablesize) then
        raise Exception.Create('Bad file');
      GlobalTable:= SelfGlobalTable;
      FBackGroundColor:= RGB(SelfGlobalTable[FBackgroundIndex].rgbRed,
        SelfGlobalTable[FBackgroundIndex].rgbGreen,
        SelfGlobalTable[FBackgroundIndex].rgbBlue);
    end
    else
    begin
      FBackgroundIndex:= 0;
      FBackgroundColor:= 0;
    end;
    frame:= nil;
    AppExtension:= nil;
    FMinimumDelay:= USER_TIMER_MINIMUM;
    LoopCount:= 1;
    repeat
      if not ReadCheck(sep, 1) then
        if Frames.Count = 0 then
          raise Exception.Create('Bad file')
        else
          break;
      if sep = $2C then
      begin
        if frame = nil then
          frame:= TGifFrame.Create(Self);
        if not ReadCheck(w, 2) then
          if Frames.Count = 0 then
            raise Exception.Create('Bad file')
          else
            break;
        frame.FrameLeft:= w;
        if not ReadCheck(w, 2) then
          if Frames.Count = 0 then
            raise Exception.Create('Bad file')
          else
            break;
        frame.FrameTop:= w;
        if not ReadCheck(w, 2) then
          if Frames.Count = 0 then
            raise Exception.Create('Bad file')
          else
            break;
        frame.FrameWidth:= w;
        if not ReadCheck(w, 2) then
          if Frames.Count = 0 then
            raise Exception.Create('Bad file')
          else
            break;
        frame.FrameHeight:= w;
        if not ReadCheck(frame.FDescriptorpackedfield, 1) then
          if Frames.Count = 0 then
            raise Exception.Create('Bad file')
          else
            break;
        frame.Interlaced:= Boolean(GetBitsValue(
          frame.FDescriptorpackedfield, 1, 1));
        if GetBitsValue(frame.FDescriptorpackedfield, 0, 1) = 1 then
        begin
          colortablesize:= Round(Power(2,
            GetBitsValue(frame.FDescriptorpackedfield, 5,3) + 1) * 3);
          if BlockRight(colortablesize) then
          begin
            SetLength(frame.LocalColorTable, colortablesize div 3);
            if not ReadCheck((@frame.LocalColorTable[0])^, colortablesize) then
            begin
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            end
            else if Length(GlobalTable) = 0 then
              GlobalTable:= Copy(frame.LocalColorTable, 0, High(Integer));
          end
          else if Frames.Count = 0 then
            raise Exception.Create('Bad file')
          else
            break;
        end;
        frame.EncodedStream.Clear;
        if Stream.Size < Stream.Position + 1 then
          if Frames.Count = 0 then
            raise Exception.Create('Bad file')
          else
            break;
        if not ReadCheck(frame.MinimumCodeSize, 1) then
          if Frames.Count = 0 then
            raise Exception.Create('Bad file')
          else
            break;
        frame.EncodedStream.Write(frame.MinimumCodeSize, 1);
        if not ProcessBlock then
          if Frames.Count = 0 then
            raise Exception.Create('Bad file')
          else
            break;
        if frame.EncodedStream.Size > 1 then
        begin
          Frames.Add(frame);
          DecodeFrame(Frames.Count - 1);
          frame:= nil;
        end
        else if Frames.Count = 0 then
          raise Exception.Create('Bad file')
        else
          break;
      end
      else if sep = $21 then
      begin
        if not ReadCheck({%H-}extensiontype, 1) then
          if Frames.Count = 0 then
            raise Exception.Create('Bad file')
          else
            break;
        case extensiontype of
          $F9: // graphic control extension
          begin
            frame:= TGifFrame.Create(Self);
            if not ReadCheck(blocklength, 1) then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            if blocklength = 4 then
            begin
              if not ReadCheck(frame.FControlpackedfield, 1) then
                if Frames.Count = 0 then
                  raise Exception.Create('Bad file')
                else
                  break;
              bt:= GetBitsValue(frame.FControlpackedfield, 3, 3);
              if bt = 0 then
                frame.Disposal:= dmUndefined
              else
                frame.Disposal:= TDisposalMethod(bt - 1);
              frame.UserInput:= Boolean(
                GetBitsValue(frame.FControlpackedfield, 6, 1));
              if not ReadCheck(w, 2) then
                if Frames.Count = 0 then
                  raise Exception.Create('Bad file')
                else
                  break;
              frame.FTransparentFlag:= GetBitsValue(frame.FControlPackedField,
                7, 1) = 1;
              frame.Delay:= w * 10;
              if w > 0 then
                Inc(delaysset);
              if not ReadCheck(bt, 1) then
                if Frames.Count = 0 then
                  raise Exception.Create('Bad file')
                else
                  break;
              frame.TransparentColorIndex:= bt;
              Stream.Seek(1, soCurrent);
            end
            else if Frames.Count = 0 then
              raise Exception.Create('Bad file')
            else
              break;
          end;
          $FE:
          begin
            if not ProcessBlock then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
          end;
          $01: // plain text extension
          begin
            if not ReadCheck(blocklength, 1) then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            if blocklength <> 12 then
            begin
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            end;
            {$IFDEF DOPLAINTEXT}
            if frame <> nil then
            begin
              FreeAndNil(frame);
              Stream.Seek(blocklength, soCurrent);
            end;
            {$ELSE}
            if frame = nil then
              frame:= TGifFrame.Create(Self);
            if not ReadCheck(w, 2) then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            frame.FrameLeft:= w;
            if not ReadCheck(w, 2) then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            frame.FrameTop:= w;
            if not ReadCheck(w, 2) then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            frame.FrameWidth:= w;
            if not ReadCheck(w, 2) then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            frame.FrameHeight:= w;
            if not ReadCheck(bt, 1) then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            New(frame.TextBlock);
            frame.TextBlock^.CellSizeX:= bt;
            if not ReadCheck(bt, 1) then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            frame.TextBlock^.CellSizeY:= bt;
            if not ReadCheck(bt, 1) then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            frame.TextBlock^.ForeColorIndex:= bt;
            if not ReadCheck(bt, 1) then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            frame.TextBlock^.BackColorIndex:= bt;
            SetLength(frame.TextBlock^.TextData, 0);
            {$ENDIF}
            if not ProcessBlock then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            {$IFNDEF DOPLAINTEXT}
            if Length(frame.TextBlock^.TextData) > 0 then
            begin
              frame.TextBlock^.FontName[0]:= #0;
              frame.TextBlock^.FontSize:= 0;
              Frames.Add(frame);
              SetLength(frame.DecodedBits, 0);
              frame:= nil;
            end;
            {$ENDIF}
          end;
          $FF: // application extension
          begin
            if not ReadCheck(blocklength, 1) then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            if blocklength <> 11 then
            begin
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            end;
            if not ReadCheck({%H-}AppIdentifier, 8) then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            if not ReadCheck({%H-}AppSubIdentifier, 3) then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            AppExtension:= TGifAppExtension.Create(AppIdentifier);
            Move(AppSubIdentifier, AppExtension.AuthCode, 3);
            if not ProcessBlock then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            AppExtensions.Add(AppExtension);
            AppExtension:= nil;
          end;
        end;
      end;
    until ((sep <> $2C) and (sep <> $21));
  finally
    if frame <> nil then
      FreeAndNil(frame);
    if AppExtension <> nil then
      FreeandNil(AppExtension);
    if delaysset / Frames.count * 100 < 10 then
      FMinimumDelay:= GIFFIXEDMINIMUM;
    SourceAnimated:= Byte(Frames.Count > 1) + 1;
    if (MonoFonts.Count = 0) then // and (Length(FTextblocks) > 0) then
    begin
      GetMonoFonts;
      if (FPlainTextFontName = '') and (Monofonts.Count > 0) then
        FPlaintextFontName:= MonoFonts[Random(MonoFonts.Count)];
    end;
  end;
end;

procedure WriteSubBlock(Stream: TStream; data: TBytesStream); overload;
var
  ILength: Byte;
  IPos: Integer;
  datalen: Integer;
  //bt: TBytes;
begin
  IPos:= 0;
  datalen:= data.Size;
  repeat
    ILength:= Min(255, datalen - Ipos);
    if ILength > 0 then
    begin
      Stream.Write(ILength, 1);
      Stream.Write((@data.Bytes[Ipos])^, ILength);
    end;
    Inc(Ipos, ILength);
  until ILength = 0;
  WriteByte(Stream, 0);
end;

procedure WriteSubBlock(Stream: TStream; data: array of AnsiChar); overload;
var
  b: TBytes;
  m: TBytesStream;
  I: Integer;
begin
  SetLength({%H-}b, Length(data));
  for I:= 0 to High(data) do
    b[I]:= Ord(data[I]);
  m:= TBytesStream.Create(b);
  try
    WriteSubBlock(Stream, m);
  finally
    m.Free;
  end;
end;

procedure WriteExtension(Stream: TStream;
  AppExtension: TGifAppExtension);
begin
  with AppExtension do
  begin
    WriteByte(Stream, $21);
    WriteByte(Stream, $FF);
    WriteByte(Stream, $B);
    Stream.Write(Identifier, 8);
    Stream.Write(AuthCode, 3);
    WriteSubBlock(Stream, AppStream);
  end;
end;

procedure TMovieGifImage.SaveToStream(Stream: TStream);
var
  I: Integer;
  dlay: Word;
  //b: Byte;
begin
  WriteByte(Stream, $47);
  WriteByte(Stream, $49);
  WriteByte(Stream, $46);
  WriteByte(Stream, $38);
  WriteByte(Stream, $39);
  WriteByte(Stream, $61);
  Stream.Write(FWidth, 2);
  Stream.Write(FHeight, 2);
  WriteByte(Stream, FDescriptorpackedfield);
  WriteByte(Stream, FBackgroundIndex);
  WriteByte(Stream, FPixelRatio);
  if Length(SelfGlobalTable) > 0 then
    Stream.Write((@SelfGlobalTable[0])^, Length(SelfGlobalTable) * 3);
  for I:= 0 to AppExtensions.Count - 1 do
  with TGifAppExtension(AppExtensions[I]) do
  begin
    if (CompareText(string(Identifier), 'NETSCAPE') = 0) and
      (CompareText(string(AuthCode), '2.0') = 0) then
    begin
      WriteExtension(Stream, TGifAppExtension(AppExtensions[I]));
      break;
    end;
  end;
  for I:= 0 to Frames.Count -1 do
  with TGifFrame(Frames[I]) do
  begin
    WriteByte(Stream, $21);
    WriteByte(Stream, $F9);
    WriteByte(Stream, $4);
    WriteByte(Stream, FControlPackedField);
    dlay:= Delay div 10;
    Stream.Write(dlay, 2);
    WriteByte(Stream, TransparentColorIndex);
    WriteByte(Stream, 0);
    if TextBlock = nil then
      WriteByte(Stream, $2C)
    else
    begin
      WriteByte(Stream, $21);
      WriteByte(Stream, $1);
      WriteByte(Stream, $C);
    end;
    Stream.Write(FrameLeft, 2);
    Stream.Write(FrameTop, 2);
    Stream.Write(FrameWidth, 2);
    Stream.Write(FrameHeight, 2);
    if TextBlock = nil then
    begin
      WriteByte(Stream, FDescriptorpackedfield);
      if Length(LocalColorTable) > 0 then
        Stream.Write((@LocalColorTable[0])^,
          Length(LocalColorTable) * 3);
      if EncodedStream.Size = 0 then
        EncodeFrame(I);
      Encodedstream.Position:= 0;
      Stream.CopyFrom(EncodedStream, 0);
      WriteByte(Stream, 0);
    end
    else
    with TextBlock^ do
    begin
      WriteByte(Stream, CellSizeX);
      WriteByte(Stream, CellSizeY);
      WriteByte(Stream, ForeColorIndex);
      WriteByte(Stream, BackColorIndex);
      WriteSubBlock(Stream, TextData);
    end;
  end;
  for I:= 0 to High(Comments) do
  begin
    WriteByte(Stream, $21);
    WriteByte(Stream, $FE);
    WriteSubBlock(Stream, Comments[I]);
  end;
  for I:= 0 to AppExtensions.Count - 1 do
  with TGifAppExtension(AppExtensions[I]) do
  begin
    if not ((CompareText(string(Identifier), 'NETSCAPE') = 0) and
      (CompareText(string(AuthCode), '2.0') = 0)) then
      WriteExtension(Stream, TGifAppExtension(AppExtensions[I]));
  end;
  WriteByte(Stream, $3B);
end;

procedure TMovieGifImage.SetPlainTextFontName(Value: string);
var
  smess: string;
begin
  if CompareText(PlaintextFontName, Value) <> 0 then
  begin
    if (Monofonts = nil) or (MonoFonts.Count = 0) or (MonoFonts.IndexOf(Value) = -1) then
    begin
      smess:= 'The font for plain text has to be a monospace font';
      if (Monofonts = nil) or (MonoFonts.Count = 0) then
        smess:= smess + ' and there is no such font installed in your system';
      ErrorMessage('Error', smess, mtConfirmation, [mbOk]);
      exit;
    end;
    PlainTextFontName:= Value;
  end;
end;

constructor TMovieWebpImage.Create;//(NewCanvas: TCanvas);
var
  cl: TColorRef;
begin
  inherited;
  cl:= GetSysColor(COLOR_BACKGROUND);
  FBackgroundColor.rgbAlpha:= Byte(cl);
  FBackgroundColor.rgbRed:= Byte(cl shr 8);
  FBackgroundColor.rgbGreen:= Byte(cl shr 16);
  FBackgroundColor.rgbBlue:= Byte(cl shr 24);
  ExifStream:= TBytesstream.Create;
  ICCPStream:= TBytesStream.Create;
  XMPStream:= Tbytesstream.Create;
  SetLength(UnknownChunks, 0);
end;

destructor TMovieWebpImage.Destroy;
var
  I: Integer;
begin
  ExifStream.Free;
  ICCPStream.Free;
  XMPStream.Free;
  for I:= 0 to High(UnknownChunks) do
    UnknownChunks[I].Free;
  SetLength(UnknownChunks, 0);
  Finalize(UnknownChunks);
  inherited;
end;

function TMovieWebpImage.DecodeFrame(pfIndex: Integer): Boolean;
var
  bufsize: Integer;
  StreamToEncode: TMemoryStream;
  code: TVp8StatusCode;
begin
  Result:= false;
  if dconfig = nil then
    WebpInitConfig;
  with TWebpFrame(Frames.Frame[pfIndex]) do
  begin
    StreamToEncode:= TMemoryStream.Create;
    try
      StreamToEncode.CopyFrom(AlphaStream, 0);
      StreamToEncode.CopyFrom(Encodedstream, 0);
      code:= WEBPGetFeatures(TMemoryStream(StreamToEncode).Memory,
        StreamToEncode.Size, @(dconfig^.input));
      if code = VP8_STATUS_OK then
      begin
        if FrameHeight <> dconfig^.input.height then
          FrameHeight:= dconfig^.input.height;
        if FrameWidth <> dconfig^.input.width then
          FrameWidth:= dconfig^.input.width;
        dconfig^.output.colorspace := decodeWebp124nodelay.WEBP_CSP_MODE.
          MODE_rgbATr; //MODE_bgrATr;
        BGR:= false;
        dconfig^.output.RGBA.RGBA.stride:=
          ((((dconfig^.input.width * 32) + 31) and not 31)
          shr 3);
        bufsize:= dconfig^.output.RGBA.RGBA.stride *
          dconfig^.input.height;
        SetLength(DecodedBits, bufsize);
        dconfig^.output.RGBA.RGBA.rgba:= @DecodedBits[0];
        dconfig^.output.RGBA.RGBA.size := bufsize;
        dconfig^.output.is_external_memory := 1;
        dconfig^.options.no_fancy_upsampling:= 1;
        dconfig^.options.dithering_strength:= 0;
        try
          try
            Result:= WebpDecode(TMemoryStream(StreamToEncode).Memory,
              StreamToEncode.Size, dconfig)
              = VP8_STATUS_OK;
          except
            Result:= false;
          end;
        finally
          WebPFreeDecBuffer(@(dconfig^.output));
        end;
      end;
    finally
      StreamToEncode.Free;
    end;
  end;
end;

function TMovieWebpImage.CanBeAnimated: Boolean;
begin
  Result:= SourceAnimated = 2;
end;

procedure TMovieWebpImage.DrawBackground(ARect: TRect);
var
  Row, Col: Integer;
  stride: Integer;
  P: PBGRQuad;
begin
  stride:= ((((FWidth * 32) + 31) and not 31)
      shr 3);
  SetLength(FCurrentBits, stride * FHeight);
  for Row:= ARect.Top to ARect.Height - 1 do
  begin
    P:= @FCurrentBits[stride * Row + (ARect.Left * 4)];
    Col:= ARect.Width;
    while Col > ARect.Left do
    begin
      P^:= FBackgroundColor;
      Inc(P);
      Dec(Col);
    end;
  end;
end;

procedure TMovieWebpImage.DrawFrame(ACanvas: TCanvas; ARect: TRect;
  pfIndex: Integer);
var
  Row, Col, canvasstride, framestride, minx, maxx: Integer;
  P, P2: PBGRQuad;
  I: Integer;
  BlendedBits: TBytes;
  Blending: Boolean;
begin
  if (pfIndex < 0) or (pfIndex > Frames.Count - 1) then
    exit;
  FFrameIndex:= pfIndex;
  if Assigned(FOnBeforeDrawingFrame) then
    FOnBeforeDrawingFrame(Self);
  if (Length(FCurrentBits) = 0) or (Acanvas <> FCanvas) or
    FNeedCompleteRedraw then
  begin
    if Frames.Count > 1 then
      DrawBackground(Rect(0, 0, FWidth, FHeight));
    if ACanvas <> FCanvas then
      FCanvas:= ACanvas;
  end;
  if FNeedCompleteRedraw and (Frames.Count > 1) then
  begin
    FExternalDrawing:= true;
    FNeedCompleteRedraw:= false;
    for I:= 0 to pfIndex - 1 do
      DrawFrame(Acanvas, ARect, I);
    FNeedCompleteRedraw:= true;
  end;
  with TWebpFrame(Frames.Frame[pfIndex]) do
  begin
    if Length(DecodedBits) = 0 then
      exit;
    Blending:= GetBitsValue(FrameBitField, 6, 1) = 1;
    canvasstride:= ((((FWidth * 32) + 31) and not 31)
        shr 3);
    framestride:= ((((FrameWidth * 32) + 31) and not 31)
        shr 3);
    SetLength({%H-}BlendedBits, 0);
    if Frames.Count > 1 then
    begin
      minx:= Max(0, FrameLeft);
      maxx:= {%H-}Min(FrameLeft + FrameWidth, FWidth);
      for Row:= Max(0, FrameTop) to {%H-}Min(FHeight,
        FrameTop + FrameHeight) -1 do
      begin
        Col := maxx;
        P:= @FCurrentBits[canvasstride * Row + (FrameLeft * 4)];
        P2:= @DecodedBits[framestride * (Row - FrameTop)];
        while Col > minx do
        begin
          if Blending and (p2^.rgbAlpha <> 255) then
          begin
            // this premultiplication formula can be verified
            // in the webp container specification page.
            // probably there are faster methods.
            p^.rgbAlpha := p^.rgbAlpha + p2^.rgbAlpha *
              (1 - p^.rgbAlpha div 255);
            if p^.rgbAlpha = 0 then
            begin
              p^.rgbBlue:= 0;
              p^.rgbGreen:= 0;
              p^.rgbRed:= 0;
            end
            else
            begin
              p^.rgbBlue:= (p^.rgbBlue * p^.rgbAlpha +
                p2^.rgbBlue * p2^.rgbAlpha * Byte((1 - p^.rgbAlpha
                div 255))) div p^.rgbAlpha;
              p^.rgbGreen:= (p^.rgbGreen * p^.rgbAlpha +
                p2^.rgbGreen * p2^.rgbAlpha * Byte((1 - p^.rgbAlpha
                div 255))) div p^.rgbAlpha;
              p^.rgbRed:= (p^.rgbRed * p^.rgbAlpha +
                p2^.rgbRed * p2^.rgbAlpha * Byte((1 - p^.rgbAlpha
                div 255))) div p^.rgbAlpha;
            end;
          end
          else
            Cardinal(p^):= Cardinal(p2^);
          Inc(p);
          Inc(p2);
          Dec(Col);
        end;
      end;
      if Length(BlendedBits) <> Length(FCurrentBits) then
        SetLength(BlendedBits, Length(FCurrentBits));
      BlendedBits:= Copy(FCurrentBits, 0, High(Integer));
    end
    else
      BlendedBits:= Copy(DecodedBits, 0, High(Integer));
    try
      DrawDecodedBytes(ACanvas, ARect, pfIndex, BlendedBits);
      if (Disposal = dmBackgroundColor) and (Frames.Count > 1) then
        DrawBackground(Rect(FrameLeft, FrameTop, FrameLeft +
          FrameWidth, FrameTop + FrameHeight));
      if Assigned(FOnAfterFrameDrawn) then
        FOnAfterFrameDrawn(Self);
    finally
      SetLength(BlendedBits, 0);
      Finalize(BlendedBits);
    end;
  end;
end;

type
  TWriterData = record
    Bytes: TBytesStream;
    WebpImage: TMovieWebpImage;
  end;
  PWriterData = ^TWriterData;

function WebpProgress(percent: Integer; const picture: PWebpPicture):
  Integer; cdecl;
begin
  Result:= 1;
  if Assigned(picture^.custom_ptr) then
    with PWriterData(picture^.custom_ptr)^ do
      if Assigned(WebpImage) and Assigned(Bytes) and
        Assigned(WebpImage.OnWebpEncoding) then
          WebpImage.OnWebpEncoding(WebpImage, Bytes.Size,
            percent * Bytes.Size div 100);
end;

function WebpWriter(const data: PByte; data_size: Cardinal;
    const picture: PWebPPicture): Integer; cdecl;
begin
  Result:= 0;
  if (picture = nil) or (data_size = 0) or
   not Assigned(picture^.custom_ptr) or
   not Assigned(PWriterData(picture^.custom_ptr)^.Bytes) then
    exit;
  try
    with PWriterData(picture^.custom_ptr)^ do
      Result:= Integer(Bytes.Write(data^, Integer(data_size)) =
      Integer(data_size));
  except
    Result:= 0;
  end;
end;

procedure TMovieWebpImage.LoadFromStream(Stream: TStream);
var
  Frame: TWebpFrame;
  fourcc: array[0..3] of AnsiChar;
  chunksize: Cardinal;
  filesize: Cardinal;
  imageheader, unknownfourcc: Boolean;
  b1, b2, b3: Byte;
  HasICCP, HasExifData, HasXMPData: Boolean;
  UChunks: PBytesStreamArray;
begin
  ICCPStream.Clear;
  HasFramesWithAlpha:= false;
  ExifStream.Clear;
  XMPStream.Clear;
  Frames.Clear;
  FWidth:= 0;
  FCurrentFrame:= 0;
  FLoopIndex:= 0;
  Sourceanimated:= 1;
  FExtendedFormat:= false;
  FExternalDrawing:= false;
  FHeight:= 0;
  Frame:= nil;
  HasICCP:= false;
  HasXMPData:= false;
  HasExifData:= false;
  try
    Stream.Read({%H-}fourcc, 4);
    if ArrayToAnsiString(fourcc) <> 'RIFF' then
      exit;
    Stream.Read({%H-}filesize, 4);
    if Stream.Size < Int64(filesize) + 8 then
      exit;
    Stream.Read(fourcc, 4);
    if ArrayToAnsiString(fourcc) <> 'WEBP' then
      exit;
    Stream.Read(fourcc, 4);
    imageheader:= false;
    repeat
      unknownfourcc:= true;
      Stream.Read({%H-}chunksize, 4);
      if chunksize mod 2 <> 0 then
        Inc(chunksize);
      if Stream.Position + chunksize > Stream.Size then  // corruption
        exit;
      if fourcc = 'ICCP' then
      begin
        if HasIccp then
          ICCPStream.CopyFrom(Stream, chunksize)
        else
         Stream.Seek(chunksize, soCurrent);
      end
      else if fourcc = 'EXIF' then
      begin
        if HasExifData then
          ExifStream.CopyFrom(Stream, chunksize)
        else
          Stream.Seek(chunksize, soCurrent);
      end
      else if fourcc = 'XMP' then
      begin
        if HasXMPData then
          XMPStream.CopyFrom(Stream, chunksize)
        else
          Stream.Seek(chunksize, soCurrent);
      end
      else if fourcc = 'VP8X' then
      begin
        if chunksize = 0 then
          exit;
        FExtendedFormat:= true;
        unknownfourcc:= false;
        Stream.Read(HeaderBitField, 1);
        HasICCP:= GetBitsValue(HeaderBitField, 2, 1) = 1;
        HasFramesWithAlpha:= GetBitsValue(HeaderBitField, 3, 1) = 1;
        HasExifData:= GetBitsValue(HeaderBitField, 4, 1) = 1;
        HasXMPData:= GetBitsValue(HeaderBitField, 5, 1) = 1;
        SourceAnimated:= GetBitsValue(HeaderBitField, 6, 1) + 1;
        Stream.Seek(3, soCurrent);
        if Stream.Position + chunksize > Stream.Size then  // corruption
          exit;
        Stream.Read({%H-}b1, 1);
        Stream.Read({%H-}b2, 1);
        Stream.Read({%H-}b3, 1);
        FWidth:= 1 + ((b3 shl 16) or (b2 shl 8) or b1);
        Stream.Read(b1, 1);
        Stream.Read(b2, 1);
        Stream.Read(b3, 1);
        FHeight:= 1 + ((b3 shl 16) or (b2 shl 8) or b1);
      end
      else if (fourcc = 'VP8 ') or (fourcc = 'VP8L') then
      begin
        if chunksize = 0 then
          exit;
        unknownfourcc:= false;
        if Frame = nil then // NO VP8X
        begin
          if not imageheader then
            imageheader:= true;
          Frame:= TWebpFrame.Create(Self);
        end;
        Frame.Lossless:= fourcc = 'VP8L';
        if Frame.Lossless and (Frame.EncodedStream.Size > 0) then
          // alpha y vp8l can't go together
        begin
          if ErrorOnBadFrame then
          begin
            Frames.Clear;
            exit;
          end;
          Frame.Free;
          Frame:= Nil;
        end;
        if imageheader and (Frame <> nil) then
        begin
          Frame.Encodedstream.Write(fourcc, 4);
          Frame.Encodedstream.Write(chunksize, 4);
          Frame.Encodedstream.CopyFrom(Stream, chunksize);
          Frames.Add(Frame);
          if not DecodeFrame(Frames.Count - 1) then
          begin
            if ErrorOnBadFrame then
            begin
              Frames.Clear;
              exit;
            end;
            FreeAndNil(Frame);
          end;
          imageheader:= false;
        end
        else
          Stream.Seek(chunksize, soCurrent);
      end
      else if fourcc = 'ANIM' then
      begin
        Stream.Read(FBackgroundColor, 4);
        unknownfourcc:= false;
        Stream.Read(LoopCount, 2);
        Stream.Seek(chunksize - 6, soCurrent);
      end
      else if fourcc = 'ANMF' then
      begin
        // specification says if not FSourceanimated
        // this shouldn't be present, but it doesn't say
        // ignore it. Besides I implement autouseranimation so
        // I keep using it even if not FSourceAnimated.
        unknownfourcc:= false;
        imageheader:= true;
        Frame:= TWebpFrame.Create(Self);
        Stream.Read(b1, 1);
        Stream.Read(b2, 1);
        Stream.Read(b3, 1);
        Frame.FrameLeft:= ((b3 shl 16) or (b2 shl 8) or b1) * 2;
        Stream.Read(b1, 1);
        Stream.Read(b2, 1);
        Stream.Read(b3, 1);
        Frame.FrameTop:= ((b3 shl 16) or (b2 shl 8) or b1) * 2;
        Stream.Read(b1, 1);
        Stream.Read(b2, 1);
        Stream.Read(b3, 1);
        Frame.FrameWidth:= ((b3 shl 16) or (b2 shl 8) or b1) + 1;
        Stream.Read(b1, 1);
        Stream.Read(b2, 1);
        Stream.Read(b3, 1);
        Frame.FrameHeight:= ((b3 shl 16) or (b2 shl 8) or b1) + 1;
        Stream.Read(b1, 1);
        Stream.Read(b2, 1);
        Stream.Read(b3, 1);
        Frame.Delay:= ((b3 shl 16) or (b2 shl 8) or b1);
        Frame.Encodedstream.Clear;
        SetLength(Frame.DecodedBits, 0);
        Stream.Read(Frame.FrameBitField, 1);
        Frame.Disposal:= TDisposalMethod(GetBitsValue(
          Frame.FrameBitField, 7, 1));
      end
      else if imageheader and (fourcc <> 'ALPH') and
        (fourcc <> 'VP8 ') and (fourcc <> 'VP8X') then
      begin
        imageheader:= false;
        FreeAndNil(Frame);
      end
      else if ((imageheader and (Frame <> nil)) or
        ((Frames.Count = 0) and (Frame = nil))) and
         (fourcc = 'ALPH') then
      begin
        unknownfourcc:= false;
        if Frame = nil then
          Frame:= TWebpFrame.Create(Self);
        if not imageheader then
          imageheader:= true;
        Frame.Alphastream.Write(fourcc, 4);
        Frame.Alphastream.Write(chunksize, 4);
        Stream.Read(frame.AlphaBitField, 1);
        Stream.Seek(-1, soCurrent);
        Frame.AlphaStream.CopyFrom(Stream, chunksize);
        //webinfobyte:= Frame.ImageBits[8];
        if chunksize + Stream.Position > Stream.Size then
          exit;
      end;
      if unknownfourcc then
      begin
        if frame <> nil then
          UChunks:= @frame.UnknownChunks
        else
          UChunks:= @UnknownChunks;
        SetLength(UChunks^, Length(UChunks^)
         + 1);
        UChunks^[High(UChunks^)]:= TBytesStream.Create;
        Stream.Seek(-8, soCurrent);
        UChunks^[High(UChunks^)].CopyFrom(Stream,
          chunksize);
      end;
      if Stream.Position < Stream.Size - 4 then
        Stream.Read(fourcc, 4);
    until Stream.Position >= Stream.Size - 4;
  finally
    if Frames.Count > 0 then
    begin
      if (SourceAnimated = 2) and (Frames.Count < 2) then
        SourceAnimated:= 1;
      if AnimateOnLoaded and (Frames.Count > 1) and
        (SourceAnimated = 2) then
        Animate;
      if FWidth = 0 then
        FWidth:= Frames.Frame[0].FrameWidth;
      if FHeight = 0 then
        FHeight:= Frames.Frame[0].FrameHeight;
      if Frames.Count = 1 then
      begin
        Frames.Frame[0].FrameLeft:= 0;
        Frames.Frame[0].FrameTop:= 0;
      end;
    end;
  end;
end;

function TMovieWebpImage.MakeFromGraphicArray(Graphics: array of TGraphic;
  delays: PWordDynArray; loops: Word; delayall: Integer;
  origcoords: PPointArray): Boolean;
var
  wbf: TWebpFrame;
  {$IFNDEF FPC}
  spackedfield: TBits;
  DeskDc, Dc: HDC;
  info: TBitmapInfo;
  stride: Integer;
  {$ELSE}
  pixelsize: Integer;
  {$ENDIF}
  pixels: PByte;
  bcount: Byte;
  I: Integer;
  w, h: Cardinal;
  alpha: Boolean;
  bm: TBitmap;
begin
  Result:= false;
  if (Length(Graphics) = 0) or ((delays <> nil) and
    (Length(delays^) <> Length(Graphics))) or
    ((origcoords <> nil) and (Length(OrigCoords^) <>
    Length(Graphics))) then
    raise Exception.Create('Wrong parameters in ' +
      'makefromgraphicarray');
  wbf:= nil;
  alpha:= false;
  try
    w:= 0;
    h:= 0;
    Frames.Clear;
    bm:= TBitmap.Create;
    {$IFNDEF FPC}
    DeskDc:= GetDC(0);
    Dc:= CreateCompatibleDc(DeskDc);
    {$ENDIF}
    Self.LoopCount:= loops;
    for I:= 0 to High(Graphics) do
    begin
      bm.Assign(Graphics[I]);
      wbf:= TWebpFrame.Create(Self);
      try
        {$IFNDEF FPC}
        FillChar(info, sizeof(info.bmiHeader), 0);
        info.bmiHeader.biSize:= sizeof(info.bmiHeader);
        if GetDiBits(Dc, bm.Handle, 0, bm.Height, nil, info,
          DIB_RGB_COLORS) = 0 then
          continue;
        bcount:= info.bmiHeader.biBitCount;
        stride:= ((((bm.Width * bcount) + 31) and not 31) shr 3);
        SetLength(wbf.DecodedBits, stride * bm.Height);
        GetMem(pixels, stride * bm.Height);
        wbf.BGR:= true;
        try
          info.bmiHeader.biHeight:= -bm.Height;
          if GetDiBits(Dc, bm.Handle, 0, bm.Height, pixels, info,
            DIB_RGB_COLORS) <> bm.Height then
            continue;
          Copymemory(@(wbf.DecodedBits[0]), pixels, stride * bm.Height);
        finally
          FreeMem(pixels);
        end;
        {$ELSE}
        Pixels:= nil;
        pixelsize:= 0;
        GetLazDiBits(bm, Pixels, pixelsize, true);
        if (Pixelsize = 0) or (Pixels =nil) then
          exit;
        try
          if bm.PixelFormat = pf24Bit then
            bcount:= 24
          else
            bcount:= 32;
          wbf.BGR:= true;
          SetLength(wbf.DecodedBits, pixelsize);
          {$IFDEF WINDOWS}
          CopyMemory(@(wbf.DecodedBits[0]), Pixels, pixelsize);
          {$ELSE}
          Move(Pixels^, (@(wbf.DecodedBits[0]))^, pixelsize);
          {$ENDIF}
        finally
          FreeMem(Pixels);
        end;
        {$ENDIF}
        if wbf.EncodeData(bcount, bm.Width, bm.Height) then
        begin
          wbf.FrameLeft:= 0;
          wbf.FrameTop:= 0;
          wbf.FrameWidth:= bm.Width;
          wbf.FrameHeight:= bm.Height;
          if not alpha then
            alpha:= wbf.AlphaStream.Size > 0;
          if wbf.FrameLeft + wbf.FrameWidth > w then
            w:= wbf.FrameLeft + wbf.FrameWidth;
          if wbf.FrameTop + wbf.FrameHeight > h then
            h:= wbf.FrameTop + wbf.FrameHeight;
          if delays <> nil then
            wbf.Delay:= delays^[I]
          else if delayall > -1 then
            wbf.Delay:= delayall
          else
            wbf.Delay:= 100;
          Frames.Add(wbf);
        end;

      except
        raise Exception.Create('Bad graphic');
      end;
    end;
    Result:= Frames.Count = Length(Graphics);
    if Result then
    begin
      {$IFNDEF FPC}
      spackedfield:= TBits.Create;
      try
        spackedfield.Size:= 8;
        spackedfield[1]:= Length(Graphics) > 1;
        spackedfield[4]:= alpha;
        HeaderBitField:= spackedfield.ToByte;
      finally
        spackedfield.Free;
      end;
      {$ELSE}
      HeaderBitField.Clear;
      HeaderBitField.Bit[1]:= Length(Graphics) > 1;
      HeaderBitField.Bit[4]:= alpha;
      {$ENDIF}
      FExtendedformat:= alpha or (Length(Graphics) > 1);
      FWidth:= w;
      FHeight:= h;
    end
    else if Frames.Count > 0 then
      Frames.Clear;
  finally
    if Assigned(wbf) and not Result then
      wbf.Free;
  end;
end;


procedure TMovieWebpImage.SaveToStream(Stream: TStream);
var
  I, J: integer;
  chunksize, fw: Integer;

  procedure WriteZeros(numbytes: Byte);
  var
    I: Integer;
  begin
    for I:= 1 to numbytes do
      WriteByte(Stream, 0);
  end;

  {$IFNDEF FPC}
  procedure WC(cname: string; csize: Integer = 0;
    wrsize: Boolean = true); // (Write Chunk)
  var
    tb: TBytes;
  begin
    tb:= TEncoding.ASCII.GetBytes(cname);
    Stream.Write(tb, Length(tb));
    if wrsize then
      Stream.Write(csize, 4);
  end;
  {$ELSE}
  procedure WC(cname: array of AnsiChar; csize: Integer = 0;
    wrsize: Boolean = true); // (Write Chunk)
  begin
    Stream.Write(cname, Length(cname));
    if wrsize then
      Stream.Write(csize, 4);
  end;
  {$ENDIF}

begin
  chunksize:= 0;
  WC('RIFF');
  WC('WEBP', 0, false);
  if (Frames.Count > 0) or FExtendedFormat then
  begin
    WC('VP8X', 10);
    Stream.Write(HeaderBitField, 1);
    WriteZeros(3);
    fw:= FWidth - 1;
    Stream.Write(fw, 3);
    fw:= FHeight - 1;
    Stream.Write(fw, 3);
    if ICCPStream.Size > 0 then
      Stream.CopyFrom(ICCPStream, 0);
    if GetBitsValue(HeaderBitField, 6, 1) = 1 then // animation
    begin
      WC('ANIM', 6);
      Stream.Write(FBackgroundColor, 4);
      Stream.Write(LoopCount, 2);
    end;
  end;
  for I:= 0 to Frames.Count -1 do
  with TWebpFrame(Frames[I]) do
  begin
    chunksize:= 0;
    for J:= 0 to High(UnknownChunks) do
      Inc(chunksize, UnknownChunks[J].Size);
    if GetBitsValue(HeaderBitField, 6, 1) = 1 then // animation
    begin
      WC('ANMF', 16 + EncodedStream.Size + AlphaStream.Size +
        chunksize);
      fw:= FrameLeft div 2;
      Stream.Write(fw, 3);
      fw:= FrameTop div 2;
      Stream.Write(fw, 3);
      fw:= FrameWidth - 1;
      Stream.Write(fw, 3);
      fw:= FrameHeight - 1;
      Stream.Write(fw, 3);
      Stream.Write(Delay, 3);
      Stream.Write(FrameBitField, 1);
    end;
    if FExtendedFormat and (AlphaStream.Size > 0) then
      Stream.CopyFrom(AlphaStream, 0);
    Stream.CopyFrom(EncodedStream, 0);
    for J:= 0 to High(UnknownChunks) do
      Stream.CopyFrom(UnknownChunks[J], 0);
  end;
  if FExtendedFormat then
  begin
    if ExifStream.Size > 0 then
      Stream.CopyFrom(ExifStream, 0);
    if XMPStream.Size > 0 then
      Stream.CopyFrom(XmpStream, 0);
    for I:= 0 to High(UnknownChunks) do
      Stream.CopyFrom(UnknownChunks[I], 0);
  end;
  chunksize:= Stream.Size - 8;
  Stream.Seek(4, soBeginning);
  Stream.Write(chunksize, 4);
end;

procedure TMovieWebpImage.SetBackgroundColor(Value: TBGRQuad);
begin
  if Cardinal(Value) <> Cardinal(FBackgroundColor) then
  begin
    FBackgroundColor:= Value;
    DrawBackground(Rect(0, 0, FWidth, FHeight));
  end;
end;

constructor TGifFrame.Create(Base: TMovieImageBase);
begin
  inherited Create(Base);
  TextBlock:= nil;
  SEtLength(LocalColorTable, 0);
end;

destructor TGifFrame.Destroy;
begin
  SetLength(LocalColorTable, 0);
  Finalize(LocalColorTable);
  if TextBlock <> nil then
    Dispose(TextBlock);
  inherited;
end;

procedure TGifFrame.GetCurrentBits(var FCurrentBits: TBytes;
  colortable: TBGRTripleArray; FWidth, Fheight: Integer;
  to32: Boolean);
var
  Row, Col, canvasstride, framestride, minx, maxx: Integer;
  P2: PByte;
  P: PBGrTriple;
  NewBits: TBytes;
  {$IFDEF WINDOWS}
  R: PRGBQuad;
  {$ELSE}
  R: PBGRQuad;
  {$ENDIF}
  convertstride: Integer;
begin
  canvasstride:= ((((FWidth * 24) + 31) and not 31) shr 3);
  minx:= Max(0, FrameLeft);
  maxx:= {%H-}Min(FrameLeft + FrameWidth, FWidth);
  framestride:= FrameWidth;
  if not to32 then
  begin
    for Row:= Max(0, FrameTop) to {%H-}Min(FHeight,
      FrameTop + FrameHeight) -1 do
    begin
      Col := maxx;
      P:= @FCurrentBits[canvasstride * Row + FrameLeft *
        sizeof(TBGRTRiple)];
      P2:= @DecodedBits[framestride * (Row - FrameTop)];
      while Col > minx do
      begin
        if not FTransparentFlag or (P2^ <> TransparentColorIndex) then
           P^:= colortable[P2^];
        Inc(p);
        Inc(p2);
        Dec(Col);
      end;
    end;
  end
  else
  begin
    convertstride:= ((((FWidth * 32) + 31) and not 31) shr 3);
    NewBits:= nil;
    SetLength(NewBits, convertstride * FHeight);
    for Row:= Max(0, FrameTop) to {%H-}Min(FHeight,
      FrameTop + FrameHeight) -1 do
    begin
      Col := maxx;
      P:= @FCurrentBits[canvasstride * Row + FrameLeft *
        sizeof(TBGRTRiple)];
      P2:= @DecodedBits[framestride * (Row - FrameTop)];
      R:= @NewBits[convertstride * Row + FrameLeft *
        sizeof(TRGBQuad)];
      while Col > minx do
      begin
        if not FTransparentFlag or (P2^ <> TransparentColorIndex) then
        begin
          R^.rgbRed:= colortable[P2^].rgbRed;
          R^.rgbGreen:= colortable[P2^].rgbGreen;
          R^.rgbBlue:= colortable[P2^].rgbBlue;
          {$IFDEF WINDOWS}
          R^.rgbReserved:= 255;
          {$ELSE}
          R^.rgbAlpha:= 255;
          {$ENDIF}
        end
        else
        begin
          R^.rgbRed:= P^.rgbRed;
          R^.rgbGreen:= P^.rgbGreen;
          R^.rgbBlue:= P^.rgbBlue;
          {$IFDEF WINDOWS}
          R^.rgbReserved:= 0;
          {$ELSE}
          R^.rgbAlpha:= 0;
          {$ENDIF}
        end;
        Inc(p);
        Inc(p2);
        Inc(R);
        Dec(Col);
      end;
    end;
    SetLength(FcurrentBits, 0);
    SetLength(FCurrentBits, Length(NewBits));
    FCurrentBits:= Copy(NewBits, 0, High(Integer));
  end;
end;

{ TMovieImageFrame }

constructor TMovieImageFrame.Create(Base: TMovieImageBase);
begin
  inherited Create;
  MyBase:= Base;
  EncodedStream:= TBytesStream.Create;
  SetLength(DecodedBits, 0);
end;

destructor TMovieImageFrame.Destroy;
begin
  EncodedStream.Free;
  SetLength(DecodedBits, 0);
  Finalize(DecodedBits);
  inherited;
end;

function TMovieImageFrame.GetBitmapFromDecodedBytes(Width, Height: Integer;{$IFDEF WINDOWS}
  out WicBitmap: IWicBitmapGraphic; {$ENDIF}
  to32: Boolean; Bits: Tbytes): {$IFDEF WINDOWS}
  TWicGraphic{$ELSE}TBitmap{$ENDIF};
var
  canvasstride: Integer;
  drawgfbackground: Boolean;
{$IFDEF WINDOWS}
  pixelFormat: WICPixelFormatGUID;
{$ELSE}
  raw: TRawImage;
{$ENDIF}
  table: PBGRTripleArray;
begin
  if Self is TGifFrame and not to32 then
    canvasstride:= ((((Width * 24) + 31) and not 31)
        shr 3)
  else
    canvasstride:= ((((Width * 32) + 31) and not 31)
        shr 3);
  if Bits = nil then
  begin
    if Self is TGifFrame then
    begin
      drawgfbackground:= false;
      if Length(TGifFrame(Self).LocalColorTable) > 0 then
        table:= @TGifFrame(Self).LocalColorTable
      else if (MyBase <> nil) and (Length(TMovieGifImage(MyBase).
        SelfGlobalTable) > 0) then
      begin
        table:= @TMovieGifImage(MyBase).SelfGlobalTable;
        drawgfbackground:= true;
      end
      else if Length(Globaltable) > 0 then
        table:= @GlobalTable
      else
        exit;
      SetLength(Bits, canvasstride * Height);
      if drawgfbackground then
        DrawGifBackground(Width, Height, Rect(0, 0,
          Width, Height), table^, TMovieGifImage(MyBase).
          BackgroundIndex, Bits);
      TGifFrame(Self).GetCurrentBits(Bits, table^,
        Width, Height, to32);
    end
    else
      Bits:= DecodedBits;
  end;
  {$IFDEF WINDOWS}
  Result:= TWicGraphic.Create;
  if Self is TGifFrame then
  begin
    if not to32 then
      pixelformat:= GUID_WICPixelFormat24bppRGB
    else
    {$IFDEF WINDOWS}
      pixelformat:= GUID_WICPixelFormat32bppBGRA
    {$ELSE}
      pixelformat:= GUID_WICPixelFormat32bppRGBA;
    {$ENDIF}
  end
  else if TWebpFrame(Self).BGR then
    pixelformat:= GUID_WICPixelFormat32bppBGRA
  else
    pixelformat:= GUID_WICPixelFormat32bppRGBA;
  if not Succeeded(Result.ImagingFactory.CreateBitmapFromMemory(
    Width, Height, pixelformat, canvasstride, Length(Bits),
    @Bits[0], WicBitmap)) then
    FreeAndNil(Result);
  {$ELSE}
  Result:= TBitmap.Create;
  try
    raw.Init;
    if Self is TGifFrame and not to32 then
      raw.Description.Init_BPP24_R8G8B8_BIO_TTB(Width, Height)
    else
      {$IFDEF WINDOWS}
      raw.Description.Init_BPP32_B8G8R8A8_BIO_TTB(Width, Height);
      {$ELSE}
      raw.Description.Init_BPP32_R8G8B8A8_BIO_TTB(Width, Height);
      {$ENDIF}
    raw.DataSize:= Length(Bits);
    raw.Data:= @Bits[0];
    Result.LoadFromRawImage(raw, false);
  except
    FreeAndNil(Result);
  end;
  {$ENDIF}
end;

{ TGifAppExtension }

constructor TGifAppExtension.Create(id: array of AnsiChar);
var
  I: integer;
begin
  if Length(id) <> 8 then
    exit;
  inherited Create;
  for I:= 0 to 7 do
    Identifier[I]:= id[I];
  AppStream:= TBytesstream.Create;
end;

destructor TGifAppExtension.Destroy;
begin
  AppStream.Free;
  inherited;
end;

{ TWebpFrame }

constructor TWebpFrame.Create(Base: TMovieImageBase);
begin
  inherited Create(Base);
  AlphaStream:= TBytesStream.Create;
  SetLength(UnknownChunks, 0);
end;

destructor TWebpFrame.Destroy;
var
  I: Integer;
begin
  AlphaStream.Free;
  for I:= 0 to High(UnknownChunks) do
    UnknownChunks[I].Free;
  SetLength(UnknownChunks, 0);
  Finalize(UnknownChunks);
  inherited;
end;

function TWebpFrame.EncodeData(BitCount: Byte; Width, Height: Integer;
  Data: PByte): Boolean;
var
  pic: TWebpPicture;
  stride: Integer;
  tw: TWriterData;
  EncStream: TBytesStream;
  fourcc: array[0..3] of AnsiChar;
  chunksize: Integer;
begin
  Result:= false;
  if WebPPictureInit(@pic) = 0 then
    exit;
  pic.width:= Width;
  if config = nil then
    WebpInitConfig;
  config^.quality:= WebpEncodingQuality;
  pic.height:= Height;
  // allocated picture of dimension width x height
  if WebPPictureAlloc(@pic) = 0 then
    exit;
  if Data = nil then
    Data:= PByte(@DecodedBits[0]);
  stride:= ((((Width * BitCount) + 31) and not 31) shr 3);
  if BGr then
  begin
    if BitCount = 24 then
    begin
      if WebPPictureImportBGR(@pic, Data, stride) = 0 then
        exit;
    end
    else if WebPPictureImportBGRA(@pic, Data, stride) = 0 then
      exit;
  end
  else
  begin
    if BitCount = 24 then
    begin
      if WebPPictureImportRGB(@pic, Data, stride) = 0 then
        exit;
    end
    else if WebPPictureImportRGBA(@pic, Data, stride) = 0 then
      exit;
  end;
  // Set up a byte-output write method. WebPMemoryWriter, for instance.
  //WebPMemoryWriterInit(@wrt);     // initialize 'wrt'

  pic.writer:= {$IFDEF FPC}@{$ENDIF}WebpWriter;
  if Assigned(MyBase) and (MyBase is TMovieWebpImage) and
    Assigned(TMovieWebpImage(MyBase).OnWebpEncoding) then
    pic.progress_hook:= {$IFDEF FPC}@{$ENDIF}WebpProgress;
  EncStream:= TBytesStream.Create;
  try
    tw.Bytes:= EncStream;
    tw.WebpImage:= TMovieWebpImage(MyBase);
    pic.custom_ptr:= @tw;

    // Compress!
    if WebPEncode(config, @pic) = 0 then
      exit;
    EncStream.Position:= $C;
    EncStream.Read({%H-}fourcc, 4);
    AlphaStream.Clear;
    if ArraytoAnsiString(fourcc) <> 'VP8X' then
    begin
      EncStream.Position:= $C;
      Self.EncodedStream.CopyFrom(EncStream, EncStream.Size - $C);
    end
    else
    begin
      EncStream.Position:= $1E;
      repeat
        EncStream.Read(fourcc, 4);
        EncStream.Read({%H-}chunksize, 4);
        if chunksize mod 2 <> 0 then
          Inc(chunksize);
        if fourcc = 'ALPH' then
        begin
          AlphaStream.Write(fourcc, 4);
          AlphaStream.Write(chunksize, 4);
          EncStream.Read(AlphaBitField, 1);
          EncStream.Seek(-1, soCurrent);
          AlphaStream.CopyFrom(EncStream, chunksize);
        end
        else
        begin
          EncodedStream.Write(fourcc, 4);
          Encodedstream.Write(chunksize, 4);
          EncodedStream.CopyFrom(EncStream, chunksize);
        end;
      until Copy(ArrayToAnsiString(fourcc), 1, 3) = 'VP8';
    end;
    Result:= EncodedStream.Size > 0;
  finally
    if not Result then
      EncodedStream.Clear;
    EncStream.Free;
  end;
end;

initialization
  SetLength(GlobalTAble, 0);
  Randomize;
  CF_MWEBP := RegisterClipboardFormat(PChar(sWebpImage));
  CF_MGIF := RegisterClipboardFormat(PChar(sGifImage));
  AnimateOnLoaded:= true;

finalization
  SetLength(GlobalTable, 0);
  Finalize(GlobalTable);
  FinalizeConfigs;

end.
