unit GifEncDec;

// gif decoding and encoding source algorithms:
// http://www.tolderlund.eu/delphi/gifimaged2010b.zip

{$IFDEF FPC}
{$mode delphiunicode}
{$ENDIF}

interface

{$IFNDEF FPC}
uses System.SysUtils, System.Classes,
{$ELSE}
uses Sysutils, Classes,
{$ENDIF}
  MovieImageAuxUnit;


////////////////////////////////////////////////////////////////////////////////
//			LZW Encoder THashTable
////////////////////////////////////////////////////////////////////////////////
const
  GIFCodeBits		= 12;			// Max number of bits per GIF token code
  GIFCodeMax		= (1 SHL GIFCodeBits)-1;// Max GIF token code
  						// 12 bits = 4095
  StackSize		= (2 SHL GIFCodeBits);	// Size of decompression stack
  TableSize		= (1 SHL GIFCodeBits);	// Size of decompression table
  HashKeyBits		= 13;			// Max number of bits per Hash Key

  HashSize		= 8009;			// Size of hash table
  						// Must be prime
                                                // Must be > than HashMaxCode
                                                // Must be < than HashMaxKey

  HashKeyMax		= (1 SHL HashKeyBits)-1;// Max hash key value
  						// 13 bits = 8191

  HashKeyMask		= HashKeyMax;		// $1FFF
  GIFCodeMask		= GIFCodeMax;		// $0FFF

  HashEmpty		= $000FFFFF;		// 20 bits

type
  // A Hash Key is 20 bits wide.
  // - The lower 8 bits are the postfix character (the new pixel).
  // - The upper 12 bits are the prefix code (the GIF token).
  // A KeyInt must be able to represent the integer values -1..(2^20)-1
  KeyInt = longInt;	// 32 bits
  CodeInt = SmallInt;	// 16 bits

  THashArray = array[0..HashSize - 1] of KeyInt;
  PHashArray = ^THashArray;

  THashTable = class
{$ifdef DEBUG_HASHPERFORMANCE}
    CountLookupFound	: longInt;
    CountMissFound	: longInt;
    CountLookupNotFound	: longInt;
    CountMissNotFound	: longInt;
{$endif}
    HashTable: PHashArray;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Insert(Key: KeyInt; Code: CodeInt);
    function Lookup(Key: KeyInt): CodeInt;
  end;

  TGIFStream = class(TStream)
  private
    FStream		: TStream;
    FOnProgress		: TNotifyEvent;
    FBuffer		: array [BYTE] of AnsiChar;
    FBufferCount	: integer;

  protected
    {%H-}constructor Create(Stream: TStream);

    procedure Progress(Sender: TObject); dynamic;
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  public
    function Read(var  {%H-}Buffer;  {%H-}Count: Longint): Longint; override;
    function Write(const  {%H-}Buffer;  {%H-}Count: Longint): Longint; override;
    function Seek( {%H-}Offset: Longint;  {%H-}Origin: Word): Longint; override;
  end;

  TGIFWriter = class(TGIFStream)
  private
    FOutputDirty	: boolean;

  protected
    procedure FlushBuffer;

  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;

    function Write(const Buffer; Count: Longint): Longint; override;
    function WriteByte(Value: BYTE): Longint;
  end;

  TGIFEncoder = class(TObject)
  protected
    MaxColor		: integer;
    BitsPerPixel	: BYTE;		// Bits per pixel of image
    Stream		: TStream;	// Output stream
    Width		,		// Width of image in pixels
    Height		: integer;	// height of image in pixels
    Interlace		: boolean;	// Interlace flag (True = interlaced image)
    Data		: PAnsiChar;	// Pointer to pixel data
    GIFStream		: TGIFWriter;	// Output buffer

    OutputBucket	: longInt;	// Output bit bucket
    OutputBits		: integer;	// Current # of bits in bucket

    ClearFlag		: Boolean;	// True if dictionary has just been cleared
    BitsPerCode		,		// Current # of bits per code
    InitialBitsPerCode	: integer;	// Initial # of bits per code after
  					// dictionary has been cleared
    MaxCode		: CodeInt;	// maximum code, given BitsPerCode
    ClearCode		: CodeInt;	// Special output code to signal "Clear table"
    EOFCode		: CodeInt;	// Special output code to signal EOF
    BaseCode		: CodeInt;	// ...

    Pixel		: PAnsiChar;	// Pointer to current pixel

    cX			,		// Current X counter (Width - X)
    Y			: integer;	// Current Y
    Pass		: integer;	// Interlace pass

    function MaxCodesFromBits(Bits: integer): CodeInt;
    procedure Output(Value: integer); virtual;
    procedure Clear; virtual;
    function BumpPixel: boolean;
    procedure DoCompress; virtual; abstract;
  public
    procedure Compress(AStream: TStream; ABitsPerPixel: integer;
      AWidth, AHeight: integer; AInterlace: boolean; AData: PAnsiChar; AMaxColor: integer);
  end;

  ////////////////////////////////////////////////////////////////////////////////
  //		TLZWEncoder - LZW encoder
  ////////////////////////////////////////////////////////////////////////////////
  const
    TableMaxMaxCode	= (1 SHL GIFCodeBits);	//
    TableMaxFill		= TableMaxMaxCode-1;	// Clear table when it fills to
                // this point.
                // Note: Must be <= GIFCodeMax
  type
    TLZWEncoder = class(TGIFEncoder)
    private
      Prefix		: CodeInt;	// Current run color
      FreeEntry		: CodeInt;	// next unused code in table
      HashTable		: THashTable;
    protected
      procedure Output(Value: integer); override;
      procedure Clear; override;
      procedure DoCompress; override;
    end;

function DecodeGifFrame(EncodedStream: TStream;
  TransparentIndex: Byte; Interlaced: Boolean;
  FrameWidth, FrameHeight, ImageWidth, ImageHeight: Integer;
  var DecodedBits: Tbytes): Boolean;
function Colors2bpp(Colors: integer): integer;
{$IFNDEF FPC}
procedure EncodeGifFrame(EncodedStream: TMemoryStream;
  ImageWidth, ImageHeight: Integer; Interlaced: Boolean;
  DecodedBits: TBytes; Table: TBGrTripleArray = nil;
  maxgifcolors: Integer = 0);
{$ELSE}
procedure EncodeGifFrame(EncodedStream: TMemoryStream;
  ImageWidth, ImageHeight: Integer; Interlaced: Boolean;
  DecodedBits: TBytes; Table: TBGrTripleArray = nil;
  maxgifcolors: Integer = 0);
{$ENDIF}
procedure WriteByte(Stream: TStream; b: BYTE);

implementation

{$IFNDEF FPC}
type
  PtrInt = LongInt;
{$ENDIF}

function DecodeGifFrame(EncodedStream: TStream;
  TransparentIndex: Byte; Interlaced: Boolean;
  FrameWidth, FrameHeight, ImageWidth, ImageHeight: Integer;
  var DecodedBits: Tbytes): Boolean;
var
  table0		: array[0..TableSize-1] of integer;
  table1		: array[0..TableSize-1] of integer;
  firstcode, oldcode	: integer;
  buf			: array[0..257] of BYTE;

  Dest			: PByte;
  v			,
  xpos, ypos, pass	: integer;

  stack			: array[0..StackSize-1] of integer;
  Source			: ^integer;
  BitsPerCode		: integer;		// number of CodeTableBits/code
  InitialBitsPerCode	: BYTE;

  MaxCode		: integer;		// maximum code, given BitsPerCode
  MaxCodeSize		: integer;
  ClearCode		: integer;		// Special code to signal "Clear table"
  EOFCode		: integer;		// Special code to signal EOF
  step			: integer;
  i			: integer;
  StartBit		,			// Index of bit buffer start
  LastBit		,			// Index of last bit in buffer
  LastByte		: integer;		// Index of last byte in buffer
  get_done		,
  return_clear		,
  ZeroBlock		: boolean;
  ClearValue		: BYTE;
{$ifdef DEBUG_DECOMPRESSPERFORMANCE}
  TimeStartDecompress	,
  TimeStopDecompress	: DWORD;
{$endif}

  function nextCode(BitsPerCode: integer): integer;
  const
    masks: array[0..15] of integer =
      ($0000, $0001, $0003, $0007,
       $000f, $001f, $003f, $007f,
       $00ff, $01ff, $03ff, $07ff,
       $0fff, $1fff, $3fff, $7fff);
  var
    StartIndex, EndIndex		: integer;
    ret			: integer;
    EndBit		: integer;
    count		: BYTE;
  begin
    if (return_clear) then
    begin
      return_clear := False;
      Result := ClearCode;
      exit;
    end;

    EndBit := StartBit + BitsPerCode;

    if (EndBit >= LastBit) then
    begin
      if (get_done) then
      begin
        Result := -1;
        exit;
      end;
      buf[0] := buf[LastByte-2];
      buf[1] := buf[LastByte-1];

      if (EncodedStream.Read({%H-}count, 1) <> 1) then
      begin
        Result := -1;
        exit;
      end;
      if (count = 0) then
      begin
        ZeroBlock := True;
        get_done := TRUE;
      end else
      begin
        // Handle premature end of file
        if (EncodedStream.Size - EncodedStream.Position < Count) then
        begin
          // Not enough data left - Just read as much as we can get
          Count := EncodedStream.Size - EncodedStream.Position;
        end;
        if (Count <> 0) then
          ReadCheck(EncodedStream, Buf[2], Count);
      end;

      LastByte := 2 + count;
      StartBit := (StartBit - LastBit) + 16;
      LastBit := LastByte * 8;

      EndBit := StartBit + BitsPerCode;
    end;

    EndIndex := EndBit DIV 8;
    StartIndex := StartBit DIV 8;

    ASSERT(StartIndex <= high(buf), 'StartIndex too large');
    if (StartIndex = EndIndex) then
      ret := buf[StartIndex]
    else
      if (StartIndex + 1 = EndIndex) then
        ret := buf[StartIndex] OR (buf[StartIndex+1] SHL 8)
      else
        ret := buf[StartIndex] OR (buf[StartIndex+1] SHL 8) OR (buf[StartIndex+2] SHL 16);

    ret := (ret SHR (StartBit AND $0007)) AND masks[BitsPerCode];

    Inc(StartBit, BitsPerCode);

    Result := ret;
  end;

  function NextLZW: integer;
  var
    code, incode	: integer;
    i			: integer;
    b			: BYTE;
  begin
    code := nextCode(BitsPerCode);
    while (code >= 0) do
    begin
      if (code = ClearCode) then
      begin
        ASSERT(ClearCode < TableSize, 'ClearCode too large');
        for i := 0 to ClearCode-1 do
        begin
          table0[i] := 0;
          table1[i] := i;
        end;
        for i := ClearCode to TableSize-1 do
        begin
          table0[i] := 0;
          table1[i] := 0;
        end;
        BitsPerCode := InitialBitsPerCode+1;
        MaxCodeSize := 2 * ClearCode;
        MaxCode := ClearCode + 2;
        Source := @stack;
        repeat
          firstcode := nextCode(BitsPerCode);
          oldcode := firstcode;
        until (firstcode <> ClearCode);

        Result := firstcode;
        exit;
      end;
      if (code = EOFCode) then
      begin
        Result := -2;
        if (ZeroBlock) then
          exit;
        // Eat rest of data blocks
        if (EncodedStream.Read({%H-}b, 1) <> 1) then
          exit;
        while (b <> 0) do
        begin
          EncodedStream.Seek(b, soCurrent);
          if (EncodedStream.Read(b, 1) <> 1) then
            exit;
        end;
        exit;
      end;

      incode := code;

      if (code >= MaxCode) then
      begin
        Source^ := firstcode;
        Inc(Source);
        code := oldcode;
      end;

      ASSERT(Code < TableSize, 'Code too large');
      while (code >= ClearCode) do
      begin
        Source^ := table1[code];
        Inc(Source);
        if (code = table0[code]) then
          raise Exception.Create('Decode circular');
          //Error(sDecodeCircular);
        code := table0[code];
        ASSERT(Code < TableSize, 'Code too large');
      end;

      firstcode := table1[code];
      Source^ := firstcode;
      Inc(Source);

      code := MaxCode;
      if (code <= GIFCodeMax) then
      begin
        table0[code] := oldcode;
        table1[code] := firstcode;
        Inc(MaxCode);
        if ((MaxCode >= MaxCodeSize) and
          (MaxCodeSize <= GIFCodeMax)) then
        begin
          MaxCodeSize := MaxCodeSize * 2;
          Inc(BitsPerCode);
        end;
      end;

      oldcode := incode;

      if ({%H-}PtrInt(Source) > {%H-}PtrInt(@stack)) then
      begin
        Dec(Source);
        Result := Source^;
        exit;
      end
    end;
    Result := code;
  end;

  function readLZW: integer;
  begin
    if ({%H-}PtrInt(Source) > {%H-}PtrInt(@stack)) then
    begin
      Dec(Source);
      Result := Source^;
    end else
      Result := NextLZW;
  end;

begin
  Result:= false;
  // Clear image data in case decompress doesn't complete
  EncodedStream.Position:= 0;
  ClearValue := TransparentIndex;
  SetLength(DecodedBits, FrameWidth * FrameHeight);
  FillChar((@DecodedBits[0])^, FrameWidth * FrameHeight, ClearValue);
  (*
  ** Read initial code size in bits from EncodedStream
  *)
  if EncodedStream.Read({%H-}InitialBitsPerCode, 1) <> 1 then
    exit;
// 2006.07.29 ->
  if InitialBitsPerCode > 8 then
    InitialBitsPerCode := 8;
// 2006.07.29 <-
  (*
  **  Initialize the Compression routines
  *)
  BitsPerCode := InitialBitsPerCode + 1;
  ClearCode := 1 SHL InitialBitsPerCode;
  EOFCode := ClearCode + 1;
  MaxCodeSize := 2 * ClearCode;
  MaxCode := ClearCode + 2;

  StartBit := 0;
  LastBit := 0;
  LastByte := 2;

  ZeroBlock := False;
  get_done := False;
  return_clear := TRUE;

  Source := @stack;

  try
    if (Interlaced) then
    begin
      ypos := 0;
      pass := 0;
      step := 8;

      for i := 0 to ImageHeight - 1 do
      begin
        Dest:= @DecodedBits[ImageWidth * ypos];
        for xpos := 0 to ImageWidth-1 do
        begin
          v := readLZW;
          if (v < 0) then
            exit;
          Dest^ := Byte(v);
          Inc(Dest);
        end;
        Inc(ypos, step);
        if (ypos >= ImageHeight) then
          repeat
            if (pass > 0) then
              step := step DIV 2;
            Inc(pass);
            ypos := step DIV 2;
          until (ypos < ImageHeight);
      end;
    end else
    begin
      Dest := @DecodedBits[0];
      for ypos := 0 to (Imageheight * Imagewidth)-1 do
      begin
        v := readLZW;
        if (v < 0) then
          exit;
        Dest^ := Byte(v);
        Inc(Dest);
      end;
    end;
  finally
    if (readLZW >= 0) then
      ;
//      raise GIFException.Create('Too much input data, ignoring extra...');
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			LZW Encoder stuff
//
////////////////////////////////////////////////////////////////////////////////


function HashKey(Key: KeyInt): CodeInt;
begin
  Result := ((Key SHR (GIFCodeBits-8)) XOR Key) MOD HashSize;
end;

function NextHashKey(HKey: CodeInt): CodeInt;
var
  disp		: CodeInt;
begin
  (*
  ** secondary hash (after G. Knott)
  *)
  disp := HashSize - HKey;
  if (HKey = 0) then
    disp := 1;
//  disp := 13;		// disp should be prime relative to HashSize, but
			// it doesn't seem to matter here...
  dec(HKey, disp);
  if (HKey < 0) then
    inc(HKey, HashSize);
  Result := HKey;
end;


constructor THashTable.Create;
begin
  ASSERT(longInt($FFFFFFFF) = -1, 'TGIFImage implementation assumes $FFFFFFFF = -1');

  inherited Create;
  GetMem(HashTable, sizeof(THashArray));
  Clear;
{$ifdef DEBUG_HASHPERFORMANCE}
  CountLookupFound := 0;
  CountMissFound := 0;
  CountLookupNotFound := 0;
  CountMissNotFound := 0;
{$endif}
end;

destructor THashTable.Destroy;
begin
{$ifdef DEBUG_HASHPERFORMANCE}
  ShowMessage(
    Format('Found: %d  HitRate: %.2f',
      [CountLookupFound, (CountLookupFound+1)/(CountMissFound+1)])+#13+
    Format('Not found: %d  HitRate: %.2f',
      [CountLookupNotFound, (CountLookupNotFound+1)/(CountMissNotFound+1)]));
{$endif}
  FreeMem(HashTable);
  inherited Destroy;
end;

// Clear hash table and fill with empty slots (doh!)
procedure THashTable.Clear;
{$ifdef DEBUG_HASHFILLFACTOR}
var
  i			,
  Count			: longInt;
{$endif}
begin
{$ifdef DEBUG_HASHFILLFACTOR}
  Count := 0;
  for i := 0 to HashSize-1 do
    if (HashTable[i] SHR GIFCodeBits <> HashEmpty) then
      inc(Count);
  ShowMessage(format('Size: %d, Filled: %d, Rate %.4f',
    [HashSize, Count, Count/HashSize]));
{$endif}

  FillChar(HashTable^, sizeof(THashArray), $FF);
end;

// Insert new key/value pair into hash table
procedure THashTable.Insert(Key: KeyInt; Code: CodeInt);
var
  HKey			: CodeInt;
begin
  // Create hash key from prefix string
  HKey := HashKey(Key);

  // Scan for empty slot
  // while (HashTable[HKey] SHR GIFCodeBits <> HashEmpty) do { Unoptimized }
  {$IFNDEF FPC}
  while (HashTable[HKey] and (HashEmpty SHL GIFCodeBits) <> (HashEmpty SHL GIFCodeBits)) do { Optimized }
    HKey := NextHashKey(HKey);
  // Fill slot with key/value pair
  HashTable[HKey] := (Key SHL GIFCodeBits) OR (Code AND GIFCodeMask);
  {$ELSE}
  while (HashTable[HKey] and (HashEmpty SHL GIFCodeBits) <> (HashEmpty SHL GIFCodeBits)) do { Optimized }
    HKey := NextHashKey(HKey);
  // Fill slot with key/value pair
  HashTable[HKey] := (Key SHL GIFCodeBits) OR (Code AND GIFCodeMask);
  {$ENDIF}
end;

// Search for key in hash table.
// Returns value if found or -1 if not
function THashTable.Lookup(Key: KeyInt): CodeInt;
var
  HKey			: CodeInt;
  HTKey			: KeyInt;
{$ifdef DEBUG_HASHPERFORMANCE}
  n			: LongInt;
{$endif}
begin
  // Create hash key from prefix string
  HKey := HashKey(Key);

{$ifdef DEBUG_HASHPERFORMANCE}
  n := 0;
{$endif}
  // Scan table for key
  // HTKey := HashTable[HKey] SHR GIFCodeBits; { Unoptimized }
  Key := Key SHL GIFCodeBits; { Optimized }
  HTKey := HashTable[HKey] AND (HashEmpty SHL GIFCodeBits); { Optimized }
  // while (HTKey <> HashEmpty) do { Unoptimized }
  while (HTKey  {%H-}<> HashEmpty SHL GIFCodeBits) do { Optimized }
  begin
    if (Key = HTKey) then
    begin
      // Extract and return value
      Result := HashTable[HKey] AND GIFCodeMask;
{$ifdef DEBUG_HASHPERFORMANCE}
      inc(CountLookupFound);
      inc(CountMissFound, n);
{$endif}
      exit;
    end;
{$ifdef DEBUG_HASHPERFORMANCE}
    inc(n);
{$endif}
    // Try next slot
    HKey := NextHashKey(HKey);
    // HTKey := HashTable[HKey] SHR GIFCodeBits; { Unoptimized }
    HTKey := HashTable[HKey] AND (HashEmpty SHL GIFCodeBits); { Optimized }
  end;
  // Found empty slot - key doesn't exist
  Result := -1;
{$ifdef DEBUG_HASHPERFORMANCE}
  inc(CountLookupNotFound);
  inc(CountMissNotFound, n);
{$endif}
end;

const
  sInvalidStream	= 'Invalid stream operation';


constructor TGIFStream.Create(Stream: TStream);
begin
  inherited Create;
  FStream := Stream;
  FBufferCount := 1; // Reserve first byte of buffer for length
end;

procedure TGIFStream.Progress(Sender: TObject);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender);
end;

function TGIFStream.Write(const Buffer; Count: Longint): Longint;
begin
  {$IFDEF FPC}
  Result:= 0;
  {$ENDIF}
  raise Exception.Create(sInvalidStream);
end;

function TGIFStream.Read(var Buffer; Count: Longint): Longint;
begin
  {$IFDEF FPC}
  Result:= 0;
  {$ENDIF}
  raise Exception.Create(sInvalidStream);
end;

function TGIFStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  {$IFDEF FPC}
  Result:= 0;
  {$ENDIF}
  raise Exception.Create(sInvalidStream);
end;


constructor TGIFWriter.Create(Stream: TStream);
begin
  inherited Create(Stream);
  FBufferCount := 1; // Reserve first byte of buffer for length
  FOutputDirty := False;
end;

destructor TGIFWriter.Destroy;
begin
  inherited Destroy;
  if (FOutputDirty) then
    FlushBuffer;
end;

procedure TGIFWriter.FlushBuffer;
begin
  if (FBufferCount <= 0) then
    exit;

  FBuffer[0] := AnsiChar(FBufferCount-1); // Block size excluding the count
  FStream.WriteBuffer(FBuffer, FBufferCount);
  FBufferCount := 1; // Reserve first byte of buffer for length
  FOutputDirty := False;
end;

function TGIFWriter.Write(const Buffer; Count: Longint): Longint;
var
  n			: integer;
  Src			: PAnsiChar;
begin
  Result := Count;
  FOutputDirty := True;
  Src := @Buffer;
  while (Count > 0) do
  begin
    // Move data to the internal buffer in 255 byte chunks
    while (FBufferCount < sizeof(FBuffer)) and (Count > 0) do
    begin
      n := sizeof(FBuffer) - FBufferCount;
      if (n > Count) then
        n := Count;
      Move(Src^, FBuffer[FBufferCount], n);
      inc(Src, n);
      inc(FBufferCount, n);
      dec(Count, n);
    end;

    // Flush the buffer when it is full
    if (FBufferCount >= sizeof(FBuffer)) then
      FlushBuffer;
  end;
end;

function TGIFWriter.WriteByte(Value: BYTE): Longint;
begin
  Result := Write(Value, 1);
end;


// Calculate the maximum number of codes that a given number of bits can represent
// MaxCodes := (1^bits)-1
function TGIFEncoder.MaxCodesFromBits(Bits: integer): CodeInt;
begin
  Result := (CodeInt(1) SHL Bits) - 1;
end;

// Stuff bits (variable sized codes) into a buffer and output them
// a byte at a time
procedure TGIFEncoder.Output(Value: integer);
const
  BitBucketMask: array[0..16] of longInt =
    ($0000,
     $0001, $0003, $0007, $000F,
     $001F, $003F, $007F, $00FF,
     $01FF, $03FF, $07FF, $0FFF,
     $1FFF, $3FFF, $7FFF, $FFFF);
begin
  if (OutputBits > 0) then
    OutputBucket :=
      (OutputBucket AND BitBucketMask[OutputBits]) OR (longInt(Value) SHL OutputBits)
  else
    OutputBucket := Value;

  inc(OutputBits, BitsPerCode);

  while (OutputBits >= 8) do
  begin
    GIFStream.WriteByte(OutputBucket AND $FF);
    OutputBucket := OutputBucket SHR 8;
    dec(OutputBits, 8);
  end;

  if (Value = EOFCode) then
  begin
    // At EOF, write the rest of the buffer.
    while (OutputBits > 0) do
    begin
      GIFStream.WriteByte(OutputBucket AND $FF);
      OutputBucket := OutputBucket SHR 8;
      dec(OutputBits, 8);
    end;
  end;
end;

procedure TGIFEncoder.Clear;
begin
  // just_cleared = 1;
  ClearFlag := TRUE;
  Output(ClearCode);
end;

// Bump (X,Y) and data pointer to point to the next pixel
function TGIFEncoder.BumpPixel: boolean;
begin
  // Bump the current X position
  dec(cX);

  // If we are at the end of a scan line, set cX back to the beginning
  // If we are interlaced, bump Y to the appropriate spot, otherwise,
  // just increment it.
  if (cX <= 0) then
  begin

    if not(Interlace) then
    begin
      // Done - no more data
      Result := False;
      exit;
    end;

    cX := Width;
    case (Pass) of
      0:
        begin
          inc(Y, 8);
          if (Y >= Height) then
          begin
            inc(Pass);
            Y := 4;
          end;
        end;
      1:
        begin
          inc(Y, 8);
          if (Y >= Height) then
          begin
            inc(Pass);
            Y := 2;
          end;
        end;
      2:
        begin
          inc(Y, 4);
          if (Y >= Height) then
          begin
            inc(Pass);
            Y := 1;
          end;
        end;
      3:
        inc(Y, 2);
    end;

    if (Y >= height) then
    begin
      // Done - No more data
      Result := False;
      exit;
    end;
    Pixel := Data + (Y * Width);
  end;
  Result := True;
end;

procedure WriteByte(Stream: TStream; b: BYTE);
begin
  Stream.Write(b, 1);
end;

procedure TGIFEncoder.Compress(AStream: TStream; ABitsPerPixel: integer;
  AWidth, AHeight: integer; AInterlace: boolean; AData: PAnsiChar; AMaxColor: integer);
const
  EndBlockByte		= $00;			// End of block marker
{$ifdef DEBUG_COMPRESSPERFORMANCE}
var
  TimeStartCompress	,
  TimeStopCompress	: DWORD;
{$endif}
begin
  MaxColor := AMaxColor;
  Stream := AStream;
  BitsPerPixel := ABitsPerPixel;
  Width := AWidth;
  Height := AHeight;
  Interlace := AInterlace;
  Data := AData;

  if (BitsPerPixel <= 1) then
    BitsPerPixel := 2;

  InitialBitsPerCode := BitsPerPixel + 1;
  Stream.Write(BitsPerPixel, 1);

  // out_bits_init = init_bits;
  BitsPerCode := InitialBitsPerCode;
  MaxCode := MaxCodesFromBits(BitsPerCode);

  ClearCode := (1 SHL (InitialBitsPerCode - 1));
  EOFCode := ClearCode + 1;
  BaseCode := EOFCode + 1;

  // Clear bit bucket
  OutputBucket := 0;
  OutputBits  := 0;

  // Reset pixel counter
  if (Interlace) then
    cX := Width
  else
    cX := Width*Height;
  // Reset row counter
  Y := 0;
  Pass := 0;

  GIFStream := TGIFWriter.Create(AStream);
  try
    if (Data <> nil) and (Height > 0) and (Width > 0) then
    begin
{$ifdef DEBUG_COMPRESSPERFORMANCE}
      TimeStartCompress := timeGetTime;
{$endif}

      // Call compress implementation
      DoCompress;

{$ifdef DEBUG_COMPRESSPERFORMANCE}
      TimeStopCompress := timeGetTime;
      ShowMessage(format('Compressed %d pixels in %d mS, Rate %d pixels/mS',
        [Height*Width, TimeStopCompress-TimeStartCompress,
        DWORD(Height*Width) DIV (TimeStopCompress-TimeStartCompress+1)]));
{$endif}
      // Output the final code.
      Output(EOFCode);
    end else
      // Output the final code (and nothing else).
      TGIFEncoder(self).Output(EOFCode);
  finally
    GIFStream.Free;
  end;

  WriteByte(Stream, EndBlockByte);
end;

function Colors2bpp(Colors: integer): integer;
var
  MaxColor		: integer;
begin
  if (Colors = 0) then
    Result := 0
  else
  begin
    Result := 1;
    MaxColor := 2;
    while (Colors > MaxColor) do
    begin
      Inc(Result);
      MaxColor := MaxColor SHL 1;
    end;
  end;
end;

procedure TLZWEncoder.Output(Value: integer);
begin
  inherited Output(Value);

  // If the next entry is going to be too big for the code size,
  // then increase it, if possible.
  if (FreeEntry > MaxCode) or (ClearFlag) then
  begin
    if (ClearFlag) then
    begin
      BitsPerCode := InitialBitsPerCode;
      MaxCode := MaxCodesFromBits(BitsPerCode);
      ClearFlag := False;
    end else
    begin
      inc(BitsPerCode);
      if (BitsPerCode = GIFCodeBits) then
        MaxCode := TableMaxMaxCode
      else
        MaxCode := MaxCodesFromBits(BitsPerCode);
    end;
  end;
end;

procedure TLZWEncoder.Clear;
begin
  inherited Clear;
  HashTable.Clear;
  FreeEntry := ClearCode + 2;
end;


procedure TLZWEncoder.DoCompress;
var
  Color			: AnsiChar;
  NewKey		: KeyInt;
  NewCode		: CodeInt;

begin
  HashTable := THashTable.Create;
  try
    // clear hash table and sync decoder
    Clear;

    Pixel := Data;
    Prefix := CodeInt(Pixel^);
    inc(Pixel);
    if (Prefix >= MaxColor) then
      raise Exception.Create(sInvalidColor);
    while (BumpPixel) do
    begin
      // Fetch the next pixel
      Color := Pixel^;
      inc(Pixel);
      if (ord(Color) >= MaxColor) then
        raise Exception.Create(sInvalidColor);

      // Append Postfix to Prefix and lookup in table...
      NewKey := (KeyInt(Prefix) SHL 8) OR ord(Color);
      NewCode := HashTable.Lookup(NewKey);
      if (NewCode >= 0) then
      begin
        // ...if found, get next pixel
        Prefix := NewCode;
        continue;
      end;

      // ...if not found, output and start over
      Output(Prefix);
      Prefix := CodeInt(Color);

      if (FreeEntry < TableMaxFill) then
      begin
        HashTable.Insert(NewKey, FreeEntry);
        inc(FreeEntry);
      end else
        Clear;
    end;
    Output(Prefix);
  finally
    HashTable.Free;
  end;
end;

{$IFNDEF FPC}
procedure EncodeGifFrame(EncodedStream: TMemoryStream;
  ImageWidth, ImageHeight: Integer; Interlaced: Boolean;
  DecodedBits: TBytes; Table: TBGrTripleArray = nil;
  maxgifcolors: Integer = 0);
{$ELSE}
procedure EncodeGifFrame(EncodedStream: TMemoryStream;
  ImageWidth, ImageHeight: Integer; Interlaced: Boolean;
  DecodedBits: TBytes; Table: TBGRTripleArray = nil;
  maxgifcolors: Integer = 0);
{$ENDIF}
var
  Encoder		: TGIFEncoder;
  BitsPerPixel		: BYTE;
  MaxColors		: integer;

begin
  if Length(Table) > 0 then
  begin
    MaxColors := Length(Table);
    BitsPerPixel := Colors2Bpp(Length(Table))
  end
  else
  begin
    BitsPerPixel := maxgifcolors;
    MaxColors := 1 SHL BitsPerPixel;
  end;

  Encodedstream.Clear;
  Encoder := TLZWEncoder.Create;
  try
    try
      Encoder.Compress(EncodedStream,
        BitsPerPixel, ImageWidth, ImageHeight, Interlaced,
        PAnsiChar((@DecodedBits)^), MaxColors);
    except
      Encodedstream.Clear;
    end;
  finally
    Encoder.Free;
  end;
end;


end.
