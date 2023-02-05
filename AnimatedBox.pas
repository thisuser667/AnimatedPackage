unit AnimatedBox;

{$IFDEF FPC}
{$mode objfpc}
{$H+}
{$ENDIF}


interface

uses
{$IFNDEF FPC}
  WinApi.Windows, Winapi.Messages, Winapi.WinCodec, System.SysUtils,
    System.AnsiStrings, Vcl.Graphics, System.Classes, System.Math,
    Vcl.ExtCtrls, System.Contnrs, Vcl.Forms, Vcl.Controls, Vcl.Clipbrd,
    System.Types, Vcl.Dialogs,
  {$ELSE}
  LclType, LclIntF, SysUtils, Graphics, Classes, Math, Dialogs,
  Controls, Forms, Types, Clipbrd,
  {$ENDIF}
  MovieImageNoGraphic, MovieImageAuxUnit;

type
  TOtherImageKind = (ikNone, ikWic, ikGif, ikWebp);

  { TAnimatedNwBox }

  TAnimatedNwBox = class(TGraphicControl)
  private
    FAnimationSpeed: TAnimationSpeed;
    FProportional, FImageAutoSize, FCenter, FStretch: Boolean;
    FImageRect: TRect;
    FResizing: Boolean;
    FImage: TImageBase;
    FNotPaint: Boolean;
    FOnPaint: TNotifyEvent;
    FImageValid: Boolean;
    FUpdatingControl: Boolean;
    FScale: Single;
    FZoomFactor: Single;
    FKind: TOtherImageKind;
    FOnLoop: TNotifyEvent;
    FOnNextFrame: TNotifyEvent;
    function GetAnimating: Boolean;
    function GetAnimationSpeed: TAnimationSpeed;
    function GetCurrentFrameIndex: Integer;
    function GetFileName: string;
    function GetOnLoop: TNotifyEvent;
    function GEtOnNextFrame: TNotifyEvent;
    procedure MyResize(Sender: TObject);
    procedure SetAnimationSpeed(Value: TAnimationSpeed);
    procedure SetCenter(Value: Boolean);
    procedure SetImageRect(Value: TRect);
    procedure SetImageAutoSize(Value: Boolean);
    procedure SetOnLoop(Event: TNotifyEvent);
    procedure SetOnNextFrame(const Value: TNotifyEvent);
    procedure SetProportional(Value: Boolean);
    procedure SetScale(Value: Single);
    procedure SetStretch(Value: Boolean);
    procedure SetZoomFactor(Value: Single);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CanBeAnimated: Boolean;
    procedure DrawFrame(ACanvas: TCanvas; ARect: TRect;
      DrawFrameIndex: Integer);
    function GetImageCount: Integer;
    function GetCanvas: TCanvas;
    function ImageWidth: Integer;
    function ImageHeight: Integer;
    function IsGifOrWebp: Boolean;
    procedure LoadFromFile(Filename: string);
    procedure CopyToClipboard;
    function PasteFromClipboard: Boolean;
    function LoopDuration: Integer;
    procedure StartAnimation;
    procedure StopAnimation;
    function GetFrames: TMovieFrameList;
    procedure ZoomIn;
    procedure ZoomOut;
    procedure UpdateControl;
    property Image: TImageBase read FImage write FImage;
    property Animating: Boolean read GetAnimating;
    property FrameIndex: Integer read GetCurrentFrameIndex;
    property FileName: string read GetFileName;
    property Kind: TOtherImageKind read FKind;
    property Imagerect: TRect read FImageRect write SetImagerect;
    property Scale: Single read FScale write SetScale;
    property Frames: TMovieFrameList read GetFrames;
  published
    property Proportional: Boolean read FProportional write SetProportional;
    property ImageAutoSize: Boolean read FImageAutoSize write
      SetImageAutoSize;
    property Center: Boolean read FCenter write SetCenter;
    property Stretch: Boolean read FStretch write SetStretch;
    property AnimationSpeed: TAnimationSpeed read GetAnimationSpeed
      write SetAnimationSpeed;
    property OnLoop: TNotiFyEvent read GetOnLoop write SetOnLoop;
    property OnNextFrame: TNotifyEvent read GEtOnNextFrame
      write SetOnNextFrame;
    property ZoomFactor: Single read FZoomFactor write
      SetZoomFactor;
    property OnStaticPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property Align;
    property Anchors;
    property AutoSize;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    {$IFNDEF FPC}
    property OnMouseActivate;
    property OnGesture;
    property Touch;
    {$ENDIF}
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;


// the next three declarations are tested only in Windows.
// I haven't found another way to implement High DPI in Lazarus
{$IFDEF WINDOWS}
type
    DEVICE_SCALE_FACTOR = (
    DEVICE_SCALE_FACTOR_INVALID = 0,
    SCALE_100_PERCENT = 100,
    SCALE_120_PERCENT = 120,
    SCALE_125_PERCENT = 125,
    SCALE_140_PERCENT = 140,
    SCALE_150_PERCENT = 150,
    SCALE_160_PERCENT = 160,
    SCALE_175_PERCENT = 175,
    SCALE_180_PERCENT = 180,
    SCALE_200_PERCENT = 200,
    SCALE_225_PERCENT = 225,
    SCALE_250_PERCENT = 250,
    SCALE_300_PERCENT = 300,
    SCALE_350_PERCENT = 350,
    SCALE_400_PERCENT = 400,
    SCALE_450_PERCENT = 450,
    SCALE_500_PERCENT = 500);
  TDeviceScaleFactor = DEVICE_SCALE_FACTOR;
  PDeviceScaleFactor = ^TDeviceScaleFactor;

function GetScaleFactorForMonitor(hMon: HMONITOR; var pScale: TDeviceScaleFactor): HRESULT;
  stdcall; external 'shcore.dll' Name 'GetScaleFactorForMonitor' {$IFNDEF FPC}delayed{$ENDIF};
function GetPPi: Integer;
function GetPixelsFactor: single;
{$ENDIF}

procedure Register;

implementation

{ TAnimatedBox }

{$IFDEF WINDOWS}
function GetPPi: Integer;
var
  sres: HRESULT;
  d: TDeviceScaleFactor;
begin
  Result := 96;
  {$IFDEF FPC}
  d:= SCALE_100_PERCENT;  // just to avoid a hint
  {$ENDIF}
  sres := GetScaleFactorForMonitor(Screen.PrimaryMonitor.Handle, d);
  if sres = S_Ok then
    Result := integer(d);
end;

function GetPixelsFactor: single;
begin
  Result := GetPPi / 100;
end;
{$ENDIF}

procedure Register;
begin
  RegisterComponents('Samples', [TAnimatedNwBox]);
end;

procedure AdjustImage(var w, h: Integer; destw, desth: Integer;
  respectmin: Boolean = false);
var
  rt: Single;
begin
  if (h = 0) or (w = 0) then
    exit;
  rt:= w / h;
  try
    if w >= h then
    begin
      h:= Min(Round(destw * h / w), desth);
      if respectmin and (h * rt > w) then
        h:= Round(w / rt);
      w:= Round(h * rt);
    end
    else
    begin
      w:= Min(Round(desth * w / h), destw);
      if respectmin and (w / rt > h) then
        w:= Round(h * rt);
      h:= Round(w / rt);
    end;
  except
  end;
end;

{$IFDEF FPC}
function RectF(Left, Top, Right, Bottom: single): TRectF; overload;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Bottom := Bottom;
  Result.Right := Right;
end;

function RectF(Rc: TRect): TREctF; overload;
begin
  Result:= RectF(Rc.Left, Rc.Top, Rc.Right, Rc.Bottom);
end;

function PointFRound(pt: Types.TPointF): Types.TPoint;
begin
  Result.X := Round(pt.X);
  Result.Y := Round(pt.Y);
end;
{$ENDIF}

function RectFRound(rc: {$IFNDEF FPC}System.{$ENDIF}Types.
  TRectF): TRect;
begin
  {$IFDEF FPC}
  Result.TopLeft := PointFRound(rc.TopLeft);
  Result.BottomRight := PointFRound(rc.BottomRight);
  {$ELSE}
  Result:= rc.Round;
  {$ENDIF}
end;

function SameMethod(AMethod1, AMethod2: TNotifyEvent): boolean;
begin
  result := (TMethod(AMethod1).Code = TMethod(AMethod2).Code)
    and (TMethod(AMethod1).Data = TMethod(AMethod2).Data);
end;

{ TAnimatedNwBox }

constructor TAnimatedNwBox.Create(AOwner: TComponent);
begin
  inherited;
  OnResize:= {$IFDEF FPC}@{$ENDIF}MyResize;
  FProportional:= false;
  FStretch:= false;
  FCenter:= false;
  FResizing:= false;
  FAnimationSpeed:= 100;
  FNotPaint:= false;
  FImageAutoSize:= false;
  ImageRect:= BoundsRect;
  FImageValid:= false;
  FZoomFactor:= 0.1;
end;

destructor TAnimatedNwBox.Destroy;
begin
  FImageValid:= false;
  FreeAndNil(FImage);
  inherited;
end;

function TAnimatedNwBox.CanBeAnimated: Boolean;
begin
  if IsGifOrWebp then
    Result:= TMovieImageBase(FImage).CanBeAnimated
  else if FileName <> '' then
    Result:= TMovieImageBase.ImageCanBeAnimated(FileName, false)
  else
    Result:= false;
end;

procedure TAnimatedNwBox.DrawFrame(ACanvas: TCanvas; ARect: TRect;
  DrawFrameIndex: Integer);
begin
  if FImageValid then
  begin
    with ACanvas do
    begin
      Brush.Color:= Self.Color;
      Brush.Style:= bsSolid;
      FillRect(Cliprect);
    end;
    if FNotPaint then
      FNotPaint:= false;
    FImage.DrawFrame(ACanvas, ARect, DrawFrameIndex);
    FNotPaint:= IsGifOrWebp and (TMovieImageBase(FImage).Frames.
      Count > 1);
  end;
end;

function TAnimatedNwBox.GetAnimating: Boolean;
begin
  if IsGifOrWebp then
    Result:= TMovieImageBase(FImage).Animating
  else
    Result:= false;
end;

function TAnimatedNwBox.GetAnimationSpeed: TAnimationSpeed;
begin
  if (not FImageValid) or not IsGifOrWebp then
    Result:= FAnimationSpeed
  else
    Result:= TMovieImageBase(FImage).AnimationSpeed;
end;

function TAnimatedNwBox.GetCanvas: TCanvas;
begin
  Result:= Canvas;
end;

function TAnimatedNwBox.GetCurrentFrameIndex: Integer;
begin
  if IsGifOrWebp then
    Result:= TMovieImageBase(FImage).FrameIndex
  else
    Result:= 0;
end;

function TAnimatedNwBox.GetFileName: string;
begin
  if FImageValid then
    Result:= TImageBase(FImage).FileName
  else
    Result:= '';
end;

function TAnimatedNwBox.GetFrames: TMovieFrameList;
begin
  if IsGifOrWebp then
    Result:= TMovieImageBase(FImage).Frames
  else
    Result:= nil;
end;

function TAnimatedNwBox.GetImageCount: Integer;
begin
  if IsGifOrWebp then
    Result:= TMovieImageBase(FImage).Frames.Count
  else
    Result:= 1;
end;

function TAnimatedNwBox.GetOnLoop: TNotifyEvent;
begin
  if IsGifOrWebp then
    Result:= TMovieImageBase(FImage).OnLoop
  else
    Result:= nil;
end;

function TAnimatedNwBox.GEtOnNextFrame: TNotifyEvent;
begin
  if IsGifOrWebp then
    Result:= TMovieImageBase(FImage).OnNextFrame
  else
    Result:= nil;
end;

function TAnimatedNwBox.ImageHeight: Integer;
begin
  Result:= 0;
  if FImageValid then
    if IsGifOrWebp then
      Result:= TMovieImageBase(FImage).Height
    else
      Result:= FImage.Height;
end;

function TAnimatedNwBox.ImageWidth: Integer;
begin
  Result:= 0;
  if FImageValid then
    if IsGifOrWebp then
      Result:= TMovieImageBase(FImage).Width
    else
      Result:= FImage.Width;
end;

function TAnimatedNwBox.IsGifOrWebp: Boolean;
begin
  Result:= (FImage is TMovieGifImage) or (FImage is TMovieWebpImage);
end;

procedure TAnimatedNwBox.LoadFromFile(Filename: string);
var
  sext: string;
begin
  if not FileExists(FileName) then
    raise Exception.Create('The file does not exist');
  sext:= string(GetImageExtensionBySignature(FileName));
  if Animating then
    StopAnimation;
  if FImageValid then
  begin
    FImage.Free;
    FImageValid:= false;
  end;
  try
    if sext = '.gif' then
      FImage:= TMovieGifImage.Create
    else if sext = '.webp' then
      FImage:= TMovieWebpImage.Create
    else
      FImage:= TNoMovieImage.Create;
    if FImage <> nil then
    begin
      try
        FImage.Canvas:= Canvas;
        FImage.LoadFromFile(FileName);
        FScale:= 1;
        FImageValid:= true;
        FNotPaint:= IsGifOrWebp and (TMovieImageBase(FImage).Frames.
          Count > 1);
        FUpdatingControl:= false;
        UpdateControl;
        if CanbeAnimated and AnimateOnloaded then
          StartAnimation;
      except
        FImage:= nil;
      end;
    end;
  except
  end;
end;

procedure TAnimatedNwBox.CopyToClipboard;
begin
  if Image <> nil then
    Image.CopyToClipboard;
end;

function TAnimatedNwBox.PasteFromClipboard: Boolean;
begin
  Result:= false;
  if not Clipboard.HasFormat(CF_MWEBP) and not Clipboard.HasFormat(CF_MGIF) and
    not {$IFDEF FPC}Clipboard.HasFormat(PredefinedClipboardFormat(pcfBitmap))
    {$ELSE}Clipboard.HasFormat(CF_BITMAP) and not Clipboard.HasFormat(
    CF_DIB){$ENDIF} then
  begin
    ErrorMessage('Error', 'The clipboard has not a supported format', mtInformation, [mbOk]);
    exit;
  end;
  if Animating then
    StopAnimation;
  if FImageValid then
  begin
    FImage.Free;
    FImageValid:= false;
  end;
  if Clipboard.HasFormat(CF_MWEBP) then
    Image:= TMovieWebpImage.Create
  else if Clipboard.HasFormat(CF_MGIF) then
    Image:= TMovieGifImage.Create
  else
    Image:= TNoMovieImage.Create;
  Image.FileName:= '';
  Result:= Image.PasteFromClipboard;
  if Result then
  begin
    if Image.Canvas <> Canvas then
      Image.Canvas:= Canvas;
    FScale:= 1;
    FImageValid:= true;
    FNotPaint:= IsGifOrWebp and (TMovieImageBase(FImage).Frames.
      Count > 1);
    FUpdatingControl:= false;
    UpdateControl;
    if CanbeAnimated then
      StartAnimation;
  end;
end;

function TAnimatedNwBox.LoopDuration: Integer;
begin
  if IsGifOrWebp then
    Result:= TMovieImageBase(FImage).LoopDurationinMs
  else
    Result:= 0;
end;

procedure TAnimatedNwBox.MyResize(Sender: TObject);
begin
  if FResizing then
    exit;
  if FImageValid and IsGifOrWebp then
    TMovieImageBase(FImage).NeedCompleteRedraw:= true;
  FUpdatingControl:= false;
  UpdateControl;
end;

procedure TAnimatedNwBox.Paint;
begin
  if (not FImageValid or (not IsGifOrWebp or not CanbeAnimated)) and
    Assigned(FOnPaint) then
    FOnPaint(Self);
  if FImageValid and (not IsGifOrWebp or
    not CanbeAnimated) then
    FImage.DrawFrame(Canvas, ImageRect, 0);
end;

procedure TAnimatedNwBox.SetAnimationSpeed(Value: TAnimationSpeed);
begin
  if Value <> FAnimationSpeed then
  begin
    FAnimationSpeed:= Value;
    if (csDesigning in ComponentState) or
      not CanbeAnimated then
      exit;
    if FImage is TMovieImageBase then
      TMovieImageBase(FImage).AnimationSpeed:= FAnimationSpeed;
  end;
end;

procedure TAnimatedNwBox.SetCenter(Value: Boolean);
begin
  if Value <> FCenter then
  begin
    FCenter:= Value;
    if not FUpdatingControl then
      UpdateControl;
  end;
end;

procedure TAnimatedNwBox.SetImageAutoSize(Value: Boolean);
begin
  if Value <> FImageAutoSize then
  begin
    FImageAutoSize:= Value;
    if not FUpdatingControl then
      UpdateControl;
  end;
end;

procedure TAnimatedNwBox.SetImageRect(Value: TRect);
begin
  if (Value <> FImageRect) or
    ((IsGifOrWebp) and (Value <> TMovieImageBase(FImage).CanvasRect))
    then
  begin
    FImageRect:= Value;
    if IsGifOrWebp then
      TMovieImageBase(Fimage).CanvasRect:= FImageRect
    else
      Refresh;
  end;
end;

procedure TAnimatedNwBox.SetOnLoop(Event: TNotifyEvent);
begin
  if not (IsGifOrWebp) then
    exit;
  with TMovieImageBase(FImage) do
    FOnLoop:= Event;
end;

procedure TAnimatedNwBox.SetOnNextFrame(const Value: TNotifyEvent);
begin
  if not (IsGifOrWebp) then
    exit;
  with TMovieImageBase(FImage) do
    FOnNextFrame:= Value;
end;

procedure TAnimatedNwBox.SetProportional(Value: Boolean);
begin
  if Value <> Fproportional then
  begin
    Fproportional:= Value;
    if not FUpdatingControl then
      UpdateControl;
  end;
end;

procedure TAnimatedNwBox.SetScale(Value: Single);
begin
  if (Value <> FScale) and FImageValid then
  begin
    FScale:= Value;
    ImageRect:= RectFRound({$IFNDEF FPC}System.Types.{$ENDIF}
      RectF(0, 0, FImage.Width * Value,
      FImage.Height * Value));
    //Refresh;
  end;
end;

procedure TAnimatedNwBox.SetStretch(Value: Boolean);
begin
  if Value <> FStretch then
  begin
    FStretch:= Value;
    if not FUpdatingControl then
      UpdateControl;
  end;
end;

procedure TAnimatedNwBox.SetZoomFactor(Value: Single);
begin
  if (Value > 0.05) and (Value <> FZoomFactor) then
    FZoomFactor:= Value;
end;

procedure TAnimatedNwBox.StartAnimation;
begin
  if not IsGifOrWebp or (Frames.Count < 2) or not CanbeAnimated or
    (csDesigning in ComponentState) then
    exit;
  TMovieImageBase(FImage).LoopIndex:= 0;
  TMovieImageBase(FImage).Animate;
end;

procedure TAnimatedNwBox.StopAnimation;
begin
  if not (IsGifOrWebp) then
    exit;
  with TMovieImageBase(FImage) do
  begin
    if not CanbeAnimated or (csDesigning in ComponentState) then
      exit;
    StopAnimation;
  end;
end;

procedure TAnimatedNwBox.UpdateControl;
var
  w, h: Integer;
  rc: TRectF;
  animated: Boolean;
begin
  if FUpdatingControl then
    exit;
  FUpdatingControl:= true;
  FResizing:= true;
  try
    animated:= Animating;
    if animated then
      TMovieImageBase(FImage).PauseAnimation;
    if FImageAutosize then
    begin
      if FImageValid then
      begin
        if (Align <> alNone) and (Align <> alCustom) then
          Align:= alNone;
        Width:= ImageWidth;
        Height:= ImageHeight;
        rc:= RectF(0, 0, Width, Height);
      end;
    end
    else
    begin
      if FImageValid then
      begin
        w:= ImageWidth;
        h:= ImageHeight;
        if FProportional then
          AdjustImage(w, h, Width, Height)
        else if FStretch then
        begin
          w:= Width;
          h:= Height;
        end;
        rc:= RectF(0, 0, w, h);
        if FCenter then
          rc.Offset((Width - w) / 2, (Height - h) / 2);
      end;
    end;
    if FImageValid then
      ImageRect:= RectFRound(rc);
    Refresh;
    if animated then
      TMovieImageBase(FImage).RestoreAnimation;
  finally
    FResizing:= false;
    FUpdatingControl:= false;
  end;
end;

procedure TAnimatedNwBox.ZoomIn;
begin
  SetScale(FScale + FZoomFactor);
end;

procedure TAnimatedNwBox.ZoomOut;
begin
  SetScale(Max(FZoomFactor, FScale - FZoomFactor));
end;

end.
