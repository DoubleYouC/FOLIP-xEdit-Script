{
    Remove HasDistantLOD flag from STAT records.
}
unit HasDistantLOD;

var
    iPluginFile : IInterface;

function Initialize:integer;
{
    This function is called at the beginning.
}
var
    i, j, idx : integer;
    f, g, n, r, p : IInterface;
    slStats : TStringList;
    tlStats : TList;
    editorid, recordId : string;
begin
    slStats := TStringList.Create;
    tlStats := TList.Create;
    for i := 0 to Pred(FileCount) do begin
        f := FileByIndex(i);

        //STAT
        g := GroupBySignature(f, 'STAT');
        for j := 0 to Pred(ElementCount(g)) do begin
            r := WinningOverride(ElementByIndex(g, j));
            recordId := GetFileName(r) + #9 + ShortName(r);
            idx := slStats.IndexOf(recordId);
            if idx > -1 then continue;
            if GetElementEditValues(r, 'Record Header\Record Flags\Has Distant LOD') <> '1' then continue;
            editorid := GetElementEditValues(r, 'EDID');
            if ContainsText(editorid, 'FOLIP_') then continue;
            slStats.Add(recordId);
            tlStats.Add(r);
        end;
    end;
    slStats.Free;

    iPluginFile := AddNewFileName('HasDistantLOD.esp', True);
    AddMasterIfMissing(iPluginFile, 'Fallout4.esm');

    for i := 0 to Pred(tlStats.Count) do begin
        r := ObjectToElement(tlStats[i]);
        p := RefMastersDeterminePlugin(r);
        n := wbCopyElementToFile(r, p, False, True);
        SetElementNativeValues(n, 'Record Header\Record Flags\Has Distant LOD', 0);
    end;
    tlStats.Free;
    Result := 0;
end;

function RefMastersDeterminePlugin(r: IInterface): IInterface;
{
    Sets the output file to either the ESM file or the ESP file based on the required masters for the given reference.
}
begin
    AddRequiredElementMasters(r, iPluginFile, False, True);
    SortMasters(iPluginFile);
    Result := iPluginFile;
end;

end.