{
    Collect LOD Assets for Fallout 4.
}
unit FOLIP;

//Create variables that will need to be used accross multiple functions/procedures.
var
    tlStats, tlMstt, tlFurn, tlActi, tlMswp, tlBasesWithLOD: TList;
    slNifFiles, slMatFiles, slTopLevelModPatternPaths, slStatLODMswp, slMswp: TStringList;
    iFolipMasterFile, iFolipPluginFile, iCurrentPlugin: IInterface;
    i: integer;
    f: string;

const
    sFolipMasterFileName = 'FOLIP Master.esm';
    sFolipPluginFileName = 'FOLIP Patch.esp';
    sFolipFileName = 'FOLIP - New LODs.esp';
    sFO4LODGenFileName = 'FO4LODGen.esp';

function Initialize:integer;
{
    This function is called at the beginning.
}
var
    slContainers: TStringList;
    i: integer;
begin
    CreateTLists;
    TopLevelModPatternPaths;

    //Create FOLIP plugins
    if not CreatePlugins then Exit;

    AddMessage('Collecting assets...');
    //scan archives and loose files
    slContainers := TStringList.Create;
    ResourceContainerList(slContainers);
    FilesInContainers(slContainers);
    slContainers.Free;
    AddMessage('Found ' + IntToStr(slNifFiles.Count) + ' lod models.');
    AddMessage('Found ' + IntToStr(slMatFiles.Count) + ' lod materials.');


    AddMessage('Collecting records...');
    CollectRecords;

    //Assign lod models to STAT records.
    AddMessage('Assigning LOD models to STAT records...');
    AssignLODModelsList;

    //Apply lod material swaps.
    AddMessage('Assigning LOD materials to ' + IntToStr(slStatLODMswp.Count) + ' material swaps.');
    AssignLODMaterialsList;

    //Add fake statics for MSTT, FURN, and ACTI that have lod.

    //Fix bad mod lod edits.

end;

function Finalize: integer;
{
    This function is called at the end.
}
begin
  tlStats.Free;
  tlMstt.Free;
  tlFurn.Free;
  tlActi.Free;
  tlMswp.Free;
  tlBasesWithLOD.Free;

  slStatLODMswp.Free;

  slNifFiles.Free;
  slMatFiles.Free;
  slTopLevelModPatternPaths.Free;
  slMswp.Free;
end;

procedure AssignLODMaterialsList;
{
    Assign lod materials to MSWP record.
}
var
    i, si, msi: integer;
    m, substitutions, sub: IInterface;
    editorID, originalMat, replacementMat: string;
begin
    for i := 0 to Pred(slStatLODMswp.Count) do begin
        msi := slMswp.IndexOf(slStatLODMswp[i]);
        if msi = -1 then AddMessage('error');
        m := ObjectToElement(tlMswp[msi]);
        editorID := GetElementEditValues(m, 'EDID');
        AddMessage(editorID);
        substitutions := ElementByName(m, 'Material Substitutions');
        for si := 0 to Pred(ElementCount(substitutions)) do begin
            sub := ElementByIndex(substitutions, si);
            originalMat := GetElementEditValues(sub, 'BNAM - Original Material');
            replacementMat := GetElementEditValues(sub, 'SNAM - Replacement Material');
            AddMessage(originalMat);
        end;
    end;
end;


procedure AssignLODModelsList;
{
    Assign lod models to STAT records.
}
var
    i, si: integer;
    HasLOD: Boolean;
    m, r, s: IInterface;
    ms: string;
begin
    for i := 0 to Pred(tlStats.Count) do begin
        s := ObjectToElement(tlStats[i]);
        HasLOD := AssignLODModels(s);
        //List relevant material swaps
        if HasLOD then begin
            //check for base material swap
            if ElementExists(s, 'Model\MODS - Material Swap') then begin
                ms := GetElementEditValues(s, 'Model\MODS - Material Swap');
                if slStatLODMswp.IndexOf(ms) = -1 then slStatLODMswp.Add(ms);
            end;

            //check references for material swaps
            m := MasterOrSelf(s);
            for si := 0 to Pred(ReferencedByCount(m)) do begin
                r := ReferencedByIndex(m, si);
                if Signature(r) <> 'REFR' then continue;
                if not IsWinningOverride(r) then continue;
                if GetIsDeleted(r) then continue;
                if not ElementExists(r, 'XMSP - Material Swap') then continue;
                ms := GetElementEditValues(r, 'XMSP - Material Swap');
                if slStatLODMswp.IndexOf(ms) > -1 then continue;
                slStatLODMswp.Add(ms);
            end;
        end;
    end;
end;

function AssignLODModels(s: IInterface): Boolean;
var
    hasChanged: Boolean;
    i, hasDistantLOD: integer;
    n: IInterface;
    colorRemap, lod4, lod8, lod16, lod32, model, olod4, olod8, olod16, olod32: string;
    slTopPaths: TStringList;
begin
    hasChanged := False;

    model := LowerCase(GetElementEditValues(s, 'Model\MODL - Model FileName'));
    if LeftStr(model, 7) <> 'meshes\' then model := 'meshes\' + model;

    slTopPaths := TStringList.Create;
    for i := 0 to Pred(slTopLevelModPatternPaths.Count) do begin
        if ContainsText(model, 'meshes\' + slTopLevelModPatternPaths[i]) then slTopPaths.Add(slTopLevelModPatternPaths[i]);
    end;

    colorRemap := FloatToStr(StrToFloatDef(GetElementEditValues(s, 'Model\MODC - Color Remapping Index'),'9'));
    if colorRemap = '9' then colorRemap := '' else colorRemap := '_' + colorRemap;

    hasDistantLOD := GetElementNativeValues(s,'Record Header\Record Flags\Has Distant LOD');
    olod4 := LowerCase(GetElementNativeValues(s, 'MNAM\LOD #0 (Level 0)\Mesh'));
    olod8 := LowerCase(GetElementNativeValues(s, 'MNAM\LOD #1 (Level 1)\Mesh'));
    olod16 := LowerCase(GetElementNativeValues(s, 'MNAM\LOD #2 (Level 2)\Mesh'));
    olod32 := LowerCase(GetElementNativeValues(s, 'MNAM\LOD #3 (Level 3)\Mesh'));
    lod4 := LODModelForLevel(model, colorRemap, '0', olod4, slTopPaths);
    lod8 := LODModelForLevel(model, colorRemap, '1', olod8, slTopPaths);
    lod16 := LODModelForLevel(model, colorRemap, '2', olod16, slTopPaths);
    lod32 := LODModelForLevel(model, colorRemap, '3', olod32, slTopPaths);
    if lod4 <> olod4 then hasChanged := True
    else if lod8 <> olod8 then hasChanged := True
    else if lod16 <> olod16 then hasChanged := True
    else if lod32 <> olod32 then hasChanged := True;
    if hasDistantLOD = 0 then begin
        if lod4 <> '' then hasDistantLOD := 1
        else if lod8 <> '' then hasDistantLOD := 1
        else if lod16 <> '' then hasDistantLOD := 1
        else if lod32 <> '' then hasDistantLOD := 1;
        if hasDistantLOD = 1 then hasChanged := True;
    end;


    if hasChanged then begin
        AddMessage(ShortName(s) + #9 + model + #9 + lod4 + #9 + lod8 + #9 + lod16 + #9 + lod32);
        iCurrentPlugin := RefMastersDeterminePlugin(s);
        n := wbCopyElementToFile(s, ICurrentPlugin, False, True);
        SetElementNativeValues(n, 'Record Header\Record Flags\Has Distant LOD', hasDistantLOD);
        Add(n, 'MNAM', True);
        SetElementNativeValues(n, 'MNAM\LOD #0 (Level 0)\Mesh', lod4);
        SetElementNativeValues(n, 'MNAM\LOD #1 (Level 1)\Mesh', lod8);
        SetElementNativeValues(n, 'MNAM\LOD #2 (Level 2)\Mesh', lod16);
        SetElementNativeValues(n, 'MNAM\LOD #3 (Level 3)\Mesh', lod32);
    end;
    Result := False;
    if hasDistantLOD = 1 then Result := True;

    slTopPaths.Free;
end;

function LODModelForLevel(model, colorRemap, level, original: string; slTopPaths: TStringList;): string;
{
    Given a model and level, checks to see if an LOD model exists and returns it.
}
var
    searchModel: string;
    i, idx, sl: integer;
    slPossibleLODPaths: TStringList;
begin
    slPossibleLODPaths := TStringList.Create;
    for i := 0 to Pred(slTopPaths.Count) do begin
        // meshes\dlc01\test.nif  to  meshes\dlc01\lod\test.nif
        searchModel := StringReplace(model, 'meshes\' + slTopPaths[i], 'meshes\' + slTopPaths[i] + 'lod\', [rfReplaceAll, rfIgnoreCase]);
        // meshes\dlc01\lod\test.nif  to  meshes\dlc01\lod\test_lod_0.nif
        sl := Length(searchModel);
        searchModel := LeftStr(searchModel, sl - 4) + colorRemap + '_lod';
        slPossibleLODPaths.Add(searchModel + '_' + level + '.nif');
        if level = '0' then slPossibleLODPaths.Add(searchModel + '.nif');
    end;

    for i := 0 to Pred(slPossibleLODPaths.Count) do begin
        if slNifFiles.IndexOf(slPossibleLODPaths[i]) > -1 then begin
            // Removes meshes/ from the file path
            sl := Length(slPossibleLODPaths[i]);
            Result := RightStr(slPossibleLODPaths[i], sl - 7);
            slPossibleLODPaths.Free;
            Exit;
        end;
    end;

    Result := original;
    slPossibleLODPaths.Free;
end;

procedure TopLevelModPatternPaths;
{
    Say you have a major mod that uses a top level pattern identifier, such as DLC01 for Automatron, for all of its assets.
    We add these in this procedure so we know to use that pattern for identification of assets.
}
begin
    slTopLevelModPatternPaths.Add('dlc01\');
    slTopLevelModPatternPaths.Add('dlc02\');
    slTopLevelModPatternPaths.Add('dlc03\');
    slTopLevelModPatternPaths.Add('dlc04\');
    slTopLevelModPatternPaths.Add('dlc05\');
    slTopLevelModPatternPaths.Add('dlc06\');
    slTopLevelModPatternPaths.Add('capitalwasteland\');

    slTopLevelModPatternPaths.Add('');
end;

procedure CreateTLists;
{
    Create TLists.
}
begin
    tlStats := TList.Create;
    tlMstt := TList.Create;
    tlFurn := TList.Create;
    tlActi := TList.Create;
    tlMswp := TList.Create;
    tlBasesWithLOD := TList.Create;

    slMatFiles := TStringList.Create;
    slMatFiles.Duplicates := dupIgnore;
    slNifFiles := TStringList.Create;
    slNifFiles.Duplicates := dupIgnore;
    slTopLevelModPatternPaths := TStringList.Create;
    slStatLODMswp := TStringList.Create;
    slMswp := TStringList.Create;
end;

procedure CollectRecords;
{
    Use this to collect all the record types you need to process over.
    It will iterate over all files in the load order regardless of what is selected in xEdit.

    This should be faster than iterating over all elements selected in the interface,
    since it only will process the specified record groups.
}
var
    i, j, idx: integer;
    recordId: string;
    f, g, r: IInterface;
    slStats, slMstt, slFurn, slActi: TStringList;
begin
    slStats := TStringList.Create;
    slMstt := TStringList.Create;
    slFurn := TStringList.Create;
    slActi := TStringList.Create;

    //Iterate over all files.
    for i := 0 to Pred(FileCount) do begin
        f := FileByIndex(i);

        //STAT
        g := GroupBySignature(f, 'STAT');
        for j := 0 to Pred(ElementCount(g)) do begin
            r := WinningOverride(ElementByIndex(g, j));
            recordId := GetFileName(r) + #9 + ShortName(r);
            idx := slStats.IndexOf(recordId);
            if idx > -1 then continue
            slStats.Add(recordId);
            tlStats.Add(r);
            //AssignLODModels(r);
        end;

        //MSTT
        g := GroupBySignature(f, 'MSTT');
        for j := 0 to Pred(ElementCount(g)) do begin
            r := WinningOverride(ElementByIndex(g, j));
            recordId := GetFileName(r) + #9 + ShortName(r);
            idx := slMstt.IndexOf(recordId);
            if idx > -1 then continue
            slMstt.Add(recordId);
            tlMstt.Add(r);
        end;

        //FURN
        g := GroupBySignature(f, 'FURN');
        for j := 0 to Pred(ElementCount(g)) do begin
            r := WinningOverride(ElementByIndex(g, j));
            recordId := GetFileName(r) + #9 + ShortName(r);
            idx := slFurn.IndexOf(recordId);
            if idx > -1 then continue
            slFurn.Add(recordId);
            tlFurn.Add(r);
        end;

        //ACTI
        g := GroupBySignature(f, 'ACTI');
        for j := 0 to Pred(ElementCount(g)) do begin
            r := WinningOverride(ElementByIndex(g, j));
            recordId := GetFileName(r) + #9 + ShortName(r);
            idx := slActi.IndexOf(recordId);
            if idx > -1 then continue
            slActi.Add(recordId);
            tlActi.Add(r);
        end;

        //MSWP
        g := GroupBySignature(f, 'MSWP');
        for j := 0 to Pred(ElementCount(g)) do begin
            r := WinningOverride(ElementByIndex(g, j));
            recordId := ShortName(r);
            idx := slMswp.IndexOf(recordId);
            if idx > -1 then continue
            slMswp.Add(recordId);
            tlMswp.Add(r);
        end;

    end;

    slStats.Free;
    slMstt.Free;
    slFurn.Free;
    slActi.Free;

    AddMessage('Found ' + IntToStr(tlStats.Count) + ' STAT records.');
    AddMessage('Found ' + IntToStr(tlMstt.Count) + ' MSTT records.');
    AddMessage('Found ' + IntToStr(tlFurn.Count) + ' FURN records.');
    AddMessage('Found ' + IntToStr(tlActi.Count) + ' ACTI records.');
    AddMessage('Found ' + IntToStr(tlMswp.Count) + ' MSWP records.');
end;

procedure FilesInContainers(containers: TStringList);
{
    Retrieves the ba2 packed files.
}
var
    slArchivedFiles: TStringList;
    i: integer;
    f: string;
begin
    slArchivedFiles := TStringList.Create;
    slArchivedFiles.Duplicates := dupIgnore;
    for i := 0 to Pred(containers.Count) do ResourceList(containers[i], slArchivedFiles);
    for i := 0 to Pred(slArchivedFiles.Count) do begin
        f := slArchivedFiles[i];
        //materials or meshes
        {if IsLODResourceModel(f) then slNifFiles.Add(f)
        else if IsInLODDir(f, 'materials') then slMatFiles.Add(f);}
        if IsInLODDir(f, 'materials') then slMatFiles.Add(f)
        else if IsLODResourceModel(f) then slNifFiles.Add(f);
    end;
    slArchivedFiles.Free;
end;

function RefMastersDeterminePlugin(r: IInterface): IInterface;
{
    Sets the output file to either the ESM file or the ESP file based on the required masters for the given reference.
}
begin
    AddRequiredElementMasters(r, iFolipPluginFile, False, True);
  	SortMasters(iFolipPluginFile);
    Result := iFolipPluginFile;
  {try
    AddRequiredElementMasters(r, iFolipMasterFile, False, True);
	SortMasters(iFolipMasterFile);
	Result := iFolipMasterFile;
  except
    on E : Exception do begin
      AddRequiredElementMasters(r, iFolipPluginFile, False, True);
  	  SortMasters(iFolipPluginFile);
  	  Result := iFolipPluginFile;
    end;
  end;}
end;

function CreatePlugins: Boolean;
{
    Checks the plugins to ensure required mods are not missing.
    Checks to make sure an old patch generated from this script is not present.
    Creates the plugin files we need.
}
var
    bFO4LODGen, bFolip: Boolean;
    i: integer;
    f: string;
begin
    bFO4LODGen := 0;
    bFolip := 0;
    for i := 0 to Pred(FileCount) do begin
        f := GetFileName(FileByIndex(i));
        if SameText(f, sFolipMasterFileName) then
            iFolipMasterFile := FileByIndex(i)
        else if SameText(f, sFolipPluginFileName) then
            iFolipPluginFile := FileByIndex(i)
        else if SameText(f, sFO4LODGenFileName) then
            bFO4LODGen := 1
        else if SameText(f, sFolipFileName) then
            bFolip := 1;
    end;
    if not bFO4LODGen then begin
        MessageDlg('Please install FO4LODGen Resources from https://www.nexusmods.com/fallout4/mods/80276 before continuing.', mtError, [mbOk], 0);
        Result := 0;
        Exit;
    end;
    if not bFolip then begin
        MessageDlg('Please install Far Object LOD Improvement Project from https://www.nexusmods.com/fallout4/mods/61884 before continuing.', mtError, [mbOk], 0);
        Result := 0;
        Exit;
    end;
    if Assigned(iFolipMasterFile) then begin
        MessageDlg(sFolipMasterFileName + ' found! Delete the old file before continuing.', mtError, [mbOk], 0);
        Result := 0;
        Exit;
    end;
    if Assigned(iFolipMasterFile) then begin
        MessageDlg(sFolipPluginFileName + ' found! Delete the old file before continuing.', mtError, [mbOk], 0);
        Result := 0;
        Exit;
    end;
    //iFolipMasterFile := AddNewFileName(sFolipMasterFileName, True);
    //AddMasterIfMissing(iFolipMasterFile, 'Fallout4.esm');
    //SetIsESM(iFolipMasterFile, True);
    iFolipPluginFile := AddNewFileName(sFolipPluginFileName, True);
    AddMasterIfMissing(iFolipPluginFile, 'Fallout4.esm');

    Result := 1;
end;

function IsLODResourceModel(f: string): Boolean;
{
    Given the filename, returns true if it follows the naming convention for an LOD resource model:
    _lod.nif
    _lod_0.nif
    _lod_1.nif
    _lod_2.nif
    _lod_3.nif
}
var
    rf: string;
begin
    Result := False;
    rf := RightStr(f, 10); //_lod_#.nif
    if rf = '_lod_0.nif' then Result := True
    else if rf = '_lod_1.nif' then Result := True
    else if rf = '_lod_2.nif' then Result := True
    else if rf = '_lod_3.nif' then Result := True
    else if RightStr(f, 8) = '_lod.nif' then Result := True;
end;

function IsInLODDir(f, m: string): Boolean;
{
    Checks the file path to see if it is in an LOD resource directory location.
    f is the file path.
    m is used to differentiate meshes vs materials top level directory.
}
var
    i: integer;
begin
    Result := False;
    for i := 0 to Pred(slTopLevelModPatternPaths.Count) do begin
        if ContainsText(f, m + '\' + slTopLevelModPatternPaths[i] + 'lod\') then begin
            Result := True;
            Exit;
        end;
    end;
end;

end.