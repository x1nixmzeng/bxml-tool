{
 MvA BXML to XML converter
 Written in Delphi 7

 Contact: WRS
 forum.xentax.com

 You are free to modify and redistribute this sourcecode as you please!

 History
   Dec 5th  Released
   Dec 9th  Updated to pull attribute values from pool when type is not 1
            Debugging flag added to dump decompressed data for external examine
            Other minor logical updates
            
}
program BXML; {$APPTYPE CONSOLE} { $DEFINE DBG }

uses
  SysUtils,
  ZLibEx,   // for the decompression
  Dialogs,  // for openfile dialog. huge unit.
  Classes;  // for streams

type
  // 36 bytes
  BXMLHEAD = packed record
    Signature,
    Version,
    StrCount,
    PoolPointer,
    PoolSize,
    AttributeCount,
    NodeCount,
    Unknown,
    ZSize : LongWord; // unsigned 32-bit values
  end;

  // 12 bytes
  BXMLATTRIB = packed record
    ANameIndex,
    AValueIndex  : LongWord; // unsigned 32-bit values

    // Dec 9th: Defined final structure values -
     
    AUsesPool,
    AValueType   : Word; // unsigned 16-bit values
  end;

  // 32 bytes
  BXMLNODE = packed record
    NNameIndex    : LongWord; // unsigned 32-bit value
    NInnerTextIndex : LongInt; // signed 32-bit value
    NUsesPool,
    NValueType,
    NLevelId,
    NChildCount,
    NAttribIndex,
    NAttribCount  : LongWord; // unsigned 32-bit values
  end;

var
  // globals
  BFile: TMemoryStream;
  BHead: BXMLHEAD;

  BStrings : array of string;
  BPool    : array of byte;

  PackedData,
  RawData    : TMemoryStream;

  Decompress  : TZDecompressionStream;

  i : LongWord;

  indent : longword;

  PoolStackPos : longword = 0;

  FOpen : TOpenDialog;

  XFile : TStringList;

  BLine : String;

const
  EOL   = #13#10;
  IDNT  : Byte = 3; // spaces per indent

// Dec 9th: PopFromPool modified to take offset
//          Renamed ReadFromPool and handled seperately

// Returns the size of pool read
function ReadFromPool(ValType, Offset: LongWord) : LongWord;
var
  pos : longint;
  vi:longint;    // int
  vf:single;     // float
  vb:longword;   // bool
  vv3:array[0..2]of single; // vector3
begin

  Result := 0;

  if Offset >= BHead.PoolSize then
  begin
    write('Error: Cannot read anymore data from pool'+EOL);
    exit;
  end;

  pos := RawData.Position;

  RawData.Seek(BHead.PoolPointer + Offset, soBeginning);

  case ValType of
    3: begin
          RawData.Read(vi,4);
          Result := 4;
          BLine:=BLine+inttostr(vi);
       end;
    5: begin
          RawData.Read(vf,4);
          Result := 4;
          BLine:=BLine+format('%f',[vf]);
       end;
    10: begin
          RawData.Read(vv3[0],4);
          RawData.Read(vv3[1],4);
          RawData.Read(vv3[2],4);
          Result := 12;
          BLine:=BLine+format('%f,%f,%f',[vv3[0],vv3[1],vv3[2]]);
       end;
    11: begin
          RawData.Read(vb,4);
          Result := 4;
          if vb = 0 then BLine:=BLine+'false' else BLine:=BLine+'true';
       end;

    // expecting more types - unsigned stuff for sure

  else

    write('Error: Cannot read an unhandled value type from pool'+EOL);

  end;

  // Reset file position
  RawData.Seek(pos, soBeginning);
end;

procedure LookupAttribute(i: LongWord);
var
  pos : longint;
  A : BXMLATTRIB;
begin
  // rarw

  // Just to show how this program reads the attribute table
  // in the same order they was stored.
  {$IFDEF DBG}
    //write('Debug: Attribute index = '+inttostr(i)+EOL);
  {$ENDIF}

  pos := RawData.Position;


  RawData.Seek(BHead.PoolPointer
               + BHead.PoolSize
               +(i * SizeOf(BXMLATTRIB)), soBeginning);
  RawData.Read(A, sizeof(BXMLATTRIB));

  if A.ANameIndex < BHead.StrCount then
  begin

    BLine := BLine + ' ' + BStrings[A.ANameIndex] + '="';

    // Dec 9th: When AUsesPool is FALSE, then AValueType == 1
    // ELSE, check value type and read from pool
    if A.AUsesPool = 1 then
    begin

      // ValueType shouldn't be 1 (string) at this stage
      case a.AValueType of
        3:  BLine:=BLine+'_int:';
        5:  BLine:=BLine+'_float:';
        10: BLine:=BLine+'_vector3:';
        11: BLine:=BLine+'_bool:';
      end;

      ReadFromPool(A.AValueType, A.AValueIndex);
      BLine:=BLine+'"';

    end
    else
      // TODO: Check this index too
      BLine := BLine + bstrings[A.AValueIndex] + '"';

  end
  else
    write('Error: Invalid string index'+EOL);

  RawData.Seek(pos, soBeginning);

end;

procedure PopFromPool(ValType: LongWord);
begin
  // Now just wraps ReadFromPool()

  PoolStackPos := PoolStackPos + ReadFromPool(ValType, PoolStackPos);
end;


{
  Recursive node parsing

  To use, call: EnumLvl(0, 1)

  i = Node tabla index (from RawData)
  c = Number of nodes on the current level
}

procedure EnumLvl(i,c : LongWord);
var
  N : BXMLNODE;
  ii,j: longword;

  procedure doIndent; var i:byte;
  begin
    // formatting!
    for i:=1 to indent do BLine:=BLine+' ';//write(' ');
  end;

begin

  inc(c,i);

  for ii:= i to c-1 do
  begin
    // Seek to the node table
    RawData.Seek(BHead.PoolPointer
                 + BHead.PoolSize
                 +(BHead.AttributeCount * SizeOf(BXMLATTRIB))
                 +(ii * SizeOf(BXMLNODE)), soBeginning);

    // Read node info
    RawData.Read(N, Sizeof(BXMLNODE));

    BLine := '';

    // Format name
    doindent; BLine:=BLine+'<'+bstrings[n.nnameindex];

    // Write out all attributes
    for j:= 1 to n.NAttribCount do
    begin

      // Lookup attribute from index
      LookupAttribute(n.NAttribIndex+(j-1));

    end;

    if n.NUsesPool = 1 then
    begin

      case n.NValueType of
        1:  BLine:=BLine+' _valuetype="string"';
        3:  BLine:=BLine+' _valuetype="int"';
        5:  BLine:=BLine+' _valuetype="float"';
        10: BLine:=BLine+' _valuetype="vector3"';
        11: BLine:=BLine+' _valuetype="bool"';
      else
        write('Error: Unable to convert unhandled value type to attribute name'+EOL);
      end;

    end;

    // Check closing tag method
    if (n.NChildCount=0) and (n.NInnerTextIndex=-1) then
    begin
      // No children or innertext, so there is no need for a closing tag
      XFile.Add(BLine + '/>');

    end
    else
    begin

      // Children or innertext, so we can wrap that in a closing tag
      XFile.Add(BLine + '>');

      // Increase the indentation level
      inc(indent,IDNT);

      // Check for innertext

      if n.NUsesPool = 0 then
      begin
        if (n.NInnerTextIndex >= 0) then
        begin

          // Dec 9th: Remove inner text when there are children

          if N.NChildCount = 0 then
          begin

            BLine := '';
            inc(indent,IDNT);
              doindent; XFile.Add(BLine+bstrings[n.NInnerTextIndex]);
            dec(indent,IDNT);

          end;

        end;
      end
      else
      begin

        BLine := '';
        // take value from stack! awesome huh
        inc(indent,IDNT);
          doindent; PopFromPool(n.NValueType); XFile.Add(BLine);
        dec(indent,IDNT);

      end;

      // Find children
      // Works without this IF, but should be faster
      if n.NChildCount > 0 then
        enumlvl(n.NLevelId, n.NChildCount);

      // Decrease the indentation level
      dec(indent,IDNT);

      // Write out the closing tag
      BLine := '';
      doindent; XFile.Add(BLine+'</'+bstrings[n.nnameindex]+'>');
    end;
  end;
end;

{
  Extract a null-terminating string from the current position
}
procedure GetString(var Str: String; Strm : TMemoryStream);
var
  tmp : char;
  tstr: string;
begin

  Strm.Read(tmp, 1);
  tstr := tmp;

  while tmp <> #0 do
  begin
    Strm.Read(tmp, 1);
    tstr := tstr + tmp;
  end;

  SetLength(Str, Length(tstr)-1);
  Str := Copy(tstr, 0, length(tstr)-1);

end;

begin

  SetLength(BStrings, 0);
  SetLength(BPool, 0);

  write('MvA BXML to XML Converter'{$IFDEF DBG}+' - DEBUGMODE -'{$ENDIF}+EOL+
        '-------------------------'+EOL);

  FOpen := TOpenDialog.Create(nil);

  FOpen.Filter:='MvA BXML (*.bxml)|*.bxml';
  FOpen.Title := 'Select a file to convert..';

  write('Select the file to convert:'+EOL);

  if not FOpen.Execute then
  begin
    FOpen.Destroy;
    write('Error: No BXML file was selected'+EOL);
    readln;
    exit;
  end;

  write(' "'+FOpen.Files[0]+'"'+EOL);

  XFile := TStringList.Create;

  // Load the BXML file
  BFile := TMemoryStream.Create;
  BFile.LoadFromFile(FOpen.Files[0]);

  // Read header info
  BFile.Read(BHead, SizeOf(BXMLHEAD));

  // Check BXML signature
  if BHead.Signature <> $4C4D5842 then
  begin
    BFile.Destroy;
    write('Error: Selected file does not use the BXML format'+EOL);
    readln;
    exit;
  end;

  // Verify the remaining filesize is accurate
  if BFile.Size <> BHead.ZSize + BFile.Position then
  begin
    BFile.Destroy;
    write('Error: Unexpected file size'+EOL);
    readln;
    exit;
  end;

  // Decompress data
  // TODO: Lookup how to do this with a less risky method!
  PackedData := TMemoryStream.Create;
  PackedData.CopyFrom(BFile, BHead.ZSize);

  Decompress := TZDecompressionStream.Create(PackedData);
  RawData := TMemoryStream.Create;

  BFile.Destroy;

  try
    RawData.CopyFrom(Decompress, 0);
  except
    PackedData.Free;
    Decompress.Free;
    RawData.Free;
    write('Error: Unable to decompress data'+EOL);
    readln;
    Exit;
  end;

  RawData.Position := 0;
  PackedData.Free;
  Decompress.Free;

  {$IFDEF DBG}
     RawData.SaveToFile('dgb.bxml');
     write('Debug: Saved decompressed data');
  {$ENDIF}

  // Store the string table in an array
  // TODO: Just store the string lengths in an array?
  //       Bah. Memory.
  SetLength(BStrings, BHead.StrCount);

  for i:=0 to BHead.StrCount-1 do
    GetString(BStrings[i], RawData);

  if RawData.Position <> BHead.PoolPointer then
  begin
    RawData.Free;
    write('Error: Unable to read string table from decompressed data'+EOL);
    readln;
    Exit;
  end;

  if RawData.Size <> BHead.PoolPointer
                      + BHead.PoolSize
                      + (BHead.AttributeCount * SizeOf(BXMLATTRIB))
                      + (BHead.NodeCount * SizeOf(BXMLNODE)) then
  begin
    RawData.Free;
    write('Error: Decompressed data is an unexpected size'+EOL);
    Exit;
  end;

  write('Sucessfully loaded string table!'+EOL);

  // Store the pool in an array
  // TODO: Again, is this really faster access?
  //       Isn't the file already stored in memory as it is?
  SetLength(BPool, BHead.PoolSize);
  RawData.Read(Pointer(BPool)^, BHead.PoolSize);

  indent := 0;
  enumlvl(0,1);   // Begin recursion

  // Free the stream
  RawData.Free;

  write('Sucessfully reconstructed XML file:'+EOL);
  write(' '+inttostr(XFile.Count)+' lines'+EOL);

  XFile.SaveToFile(FOpen.Files[0] + '.xml');

  write('Saved XML as:'+EOL);
  write(' "' +FOpen.Files[0] + '.xml"'+EOL+EOL);

  FOpen.Destroy;
  XFile.Destroy;

  write('Press ENTER to quit...'+EOL);
  readln;

end.
