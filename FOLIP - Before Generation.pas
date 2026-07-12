{
    Collect LOD Assets for Fallout 4.

    TODO:
    * Check if object is in a settlement and scrappable. Test to see if LOD Respects Enable State flag works on scrapped items like they do for Enable Markers. It may require persistence, and only would work in Fallout4.esm
    * Increase speed.
    * Attempt to match lod models to models by their base name.
}
unit FOLIP;

// ----------------------------------------------------
//Create variables that will need to be used accross multiple functions/procedures.
// ----------------------------------------------------
var
    tlStats, tlActiFurnMstt, tlMswp, tlEnableParents, tlStolenForms, tlTxst, tlFlst: TList;

    slNifFiles, slUsedLODNifFiles, slMatFiles, slCheckedModels, slMeshCheckMissingMaterials, slMeshCheckNonLODMaterials,
    slMeshCheckNoMaterialSpecified, slMismatchedFullModelToLODMaterials, slTopLevelModPatternPaths, slMessages, slMissingLODMessages,
    slMissingColorRemaps, slFullLODMessages, slPluginFiles, slHasLOD, slFOLIPTexgen_noalpha, slFOLIPTexgen_copy, slFOLIPTexgen_alpha,
    slTexgen_copy, slTexgen_alpha, slTexgen_noalpha, slOutsideUVRange, slContainers, slVerifyLODModels, slMasterableMasters, slPatchMasters,
    slMainMasters, slFakeStatics, slAddedTexGenTextures: TStringList;

    flOverrides, flMultiRefLOD, flParents, flNeverfades, flDecals, flFakeStatics, flRemoveIsFullLOD,
    flOverridesMaster, flMultiRefLODMaster, flParentsMaster, flNeverfadesMaster, flDecalsMaster, flFakeStaticsMaster, flRemoveIsFullLODMaster: IwbMainRecord;

    iFolipMasterFile, iFolipPluginFile, iCurrentPlugin: IwbFile;

    uiScale: integer;

    sFolipPluginFileName, sFolipAfterGenerationPluginFileName, sEnableParentFormidExclusions, sIgnoredWorldspaces, sIgnoredPlugins, pluginFileNameHere: string;

    bFakeStatics, bForceLOD8, bReportMissingLOD, bReportUVs, bReportNonLODMaterials, bSaveUserRules, bUserRulesChanged, bRespectEnableMarkers,
    bIgnoreNoLOD, bLightPlugin, bRemoveVWD, bLimitedHasDistantLODRemoval, bAddVWD, bSkipPrecombined, bRemoveBeforeGeneration, bMakeMissingMaterials,
    bPreviousBeforeGenerationPresent, bDeepScan: Boolean;

    joRules, joMswpMap, joUserRules, joMultiRefLOD, joUserSettings, joModelMatch, joElements, joWinningCells, joRasterizeMaterials: TJsonObject;

    lvRules: TListView;

    btnRuleOk, btnRuleCancel: TButton;

const
    sFolipMasterFileName = 'FOLIP - Master.esm';
    sFolipFileName = 'FOLIP - New LODs.esp';
    sFO4LODGenFileName = 'FO4LODGen.esp';
    sUserSettingsFileName = 'FOLIP\UserSettings.json';
    sRasterizeGrayScaleToPalette = wbScriptsPath + 'FOLIP\RasterizeGrayScaleToPalette.exe';
    sOutputDir = wbScriptsPath + 'FOLIP\output';
    FOLIPTempPath = wbScriptsPath + 'FOLIP\Temp';
    texconv = wbScriptsPath + 'Texconvx64.exe';

// ----------------------------------------------------
// Main functions and procedures go up immediately below.
// ----------------------------------------------------

function Initialize:integer;
{
    This function is called at the beginning.
}
var
    bLoadDefaults: Boolean;
begin
    try
        //TLists
        tlStats := TList.Create;
        tlActiFurnMstt := TList.Create;
        tlMswp := TList.Create;
        tlEnableParents := TList.Create;
        tlStolenForms := TList.Create;
        tlTxst := TList.Create;
        tlFlst := TList.Create;


        //TStringLists
        slPluginFiles := TStringList.Create;
        slVerifyLODModels := TStringList.Create;
        slVerifyLODModels.Sorted := True;
        slVerifyLODModels.Duplicates := dupIgnore;

        slMatFiles := TStringList.Create;
        slMatFiles.Sorted := True;
        slMatFiles.Duplicates := dupIgnore;

        slNifFiles := TStringList.Create;
        slNifFiles.Sorted := True;
        slNifFiles.Duplicates := dupIgnore;

        slUsedLODNifFiles := TStringList.Create;
        slUsedLODNifFiles.Sorted := True;
        slUsedLODNifFiles.Duplicates := dupIgnore;

        slMeshCheckMissingMaterials := TStringList.Create;
        slMeshCheckMissingMaterials.Sorted := True;
        slMeshCheckMissingMaterials.Duplicates := dupIgnore;

        slMeshCheckNonLODMaterials := TStringList.Create;
        slMeshCheckNonLODMaterials.Sorted := True;
        slMeshCheckNonLODMaterials.Duplicates := dupIgnore;

        slMeshCheckNoMaterialSpecified := TStringList.Create;
        slMeshCheckNoMaterialSpecified.Sorted := True;
        slMeshCheckNoMaterialSpecified.Duplicates := dupIgnore;

        slMismatchedFullModelToLODMaterials  := TStringList.Create;
        slMismatchedFullModelToLODMaterials.Sorted := True;
        slMismatchedFullModelToLODMaterials.Duplicates := dupIgnore;

        slPatchMasters := TStringList.Create;
        slPatchMasters.Sorted := True;
        slPatchMasters.Duplicates := dupIgnore;

        slMainMasters := TStringList.Create;
        slMainMasters.Sorted := True;
        slMainMasters.Duplicates := dupIgnore;

        slMasterableMasters := TStringList.Create;

        slCheckedModels := TStringList.Create;
        slHasLOD := TStringList.Create;
        slFakeStatics := TStringList.Create;

        slTopLevelModPatternPaths := TStringList.Create;
        slMessages := TStringList.Create;
        slMessages.Sorted := True;
        slMissingLODMessages := TStringList.Create;
        slMissingLODMessages.Sorted := True;

        slOutsideUVRange := TStringList.Create;
        slOutsideUVRange.Sorted := True;
        slOutsideUVRange.Duplicates := dupIgnore;

        slFullLODMessages := TStringList.Create;
        slFullLODMessages.Sorted := True;

        slMissingColorRemaps := TStringList.Create;
        slMissingColorRemaps.Sorted := True;

        slFOLIPTexgen_noalpha := TStringList.Create;
        if FileExists(wbDataPath + 'DynDOLOD\DynDOLOD_FO4_TexGen_noalpha_folipnewlodsesp.txt') then begin
            slFOLIPTexgen_noalpha.LoadFromFile(wbDataPath + 'DynDOLOD\DynDOLOD_FO4_TexGen_noalpha_folipnewlodsesp.txt');
            AddMessage('Loaded TexGen noalpha rules from ' + wbDataPath + 'DynDOLOD\DynDOLOD_FO4_TexGen_noalpha_folipnewlodsesp.txt');
        end;
        slFOLIPTexgen_copy := TStringList.Create;
        if FileExists(wbDataPath + 'DynDOLOD\DynDOLOD_FO4_TexGen_copy_folipnewlodsesp.txt') then begin
            slFOLIPTexgen_copy.LoadFromFile(wbDataPath + 'DynDOLOD\DynDOLOD_FO4_TexGen_copy_folipnewlodsesp.txt');
            AddMessage('Loaded TexGen copy rules from ' + wbDataPath + 'DynDOLOD\DynDOLOD_FO4_TexGen_copy_folipnewlodsesp.txt');
        end;
        slFOLIPTexgen_alpha := TStringList.Create;
        if FileExists(wbDataPath + 'DynDOLOD\DynDOLOD_FO4_TexGen_alpha_folipnewlodsesp.txt') then begin
            slFOLIPTexgen_alpha.LoadFromFile(wbDataPath + 'DynDOLOD\DynDOLOD_FO4_TexGen_alpha_folipnewlodsesp.txt');
            AddMessage('Loaded TexGen alpha rules from ' + wbDataPath + 'DynDOLOD\DynDOLOD_FO4_TexGen_alpha_folipnewlodsesp.txt');
        end;
        slTexgen_copy := TStringList.Create;
        slTexgen_alpha := TStringList.Create;
        slTexgen_noalpha := TStringList.Create;
        slAddedTexGenTextures := TStringList.Create;

        //TJsonObjects
        joRules := TJsonObject.Create;
        joMswpMap := TJsonObject.Create;
        joMultiRefLOD := TJsonObject.Create;
        joUserSettings := TJsonObject.Create;
        joModelMatch := TJsonObject.Create;
        joElements := TJsonObject.Create;
        joWinningCells := TJsonObject.Create;
        joRasterizeMaterials := TJsonObject.Create;

        bLoadDefaults := True;
        bDeepScan := False;
        bPreviousBeforeGenerationPresent := False;
        if ResourceExists(sUserSettingsFileName) then begin
            AddMessage('Loading user settings from ' + sUserSettingsFileName);
            joUserSettings.LoadFromResource(sUserSettingsFileName);
            try
                bFakeStatics := StrToBool(Fallback(joUserSettings.S['FakeStatics'], 'True'));
                bForceLOD8 := StrToBool(joUserSettings.S['ForceLOD8']);
                bIgnoreNoLOD := StrToBool(joUserSettings.S['IgnoreNoLOD']);
                bMakeMissingMaterials := StrToBool(Fallback(joUserSettings.S['MakeMissingMaterials'], 'True'));
                bReportNonLODMaterials := StrToBool(joUserSettings.S['ReportNonLODMaterials']);
                bReportUVs := StrToBool(joUserSettings.S['ReportUVs']);
                bReportMissingLOD := StrToBool(joUserSettings.S['ReportMissingLOD']);
                bRespectEnableMarkers := StrToBool(Fallback(joUserSettings.S['RespectEnableMarkers'], 'True'));
                sFolipPluginFileName := Fallback(joUserSettings.S['BeforeGenerationPluginName'], 'FOLIP - Patch');

                sFolipAfterGenerationPluginFileName := Fallback(joUserSettings.S['AfterGenerationPluginName'], 'FOLIP - After Generation');
                bLightPlugin := StrToBool(Fallback(joUserSettings.S['LightPlugin'], 'True'));
                bRemoveVWD := StrToBool(Fallback(joUserSettings.S['RemoveVWD'], 'True'));
                bLimitedHasDistantLODRemoval := StrToBool(joUserSettings.S['LimitedHasDistantLODRemoval']);
                bAddVWD := StrToBool(joUserSettings.S['AddVWD']);
                bSkipPrecombined := StrToBool(Fallback(joUserSettings.S['SkipPrecombined'], 'True'));
                bRemoveBeforeGeneration := StrToBool(joUserSettings.S['RemoveBeforeGeneration']);
                bLoadDefaults := False;
            except
                AddMessage('User settings are incomplete. Loading defaults.');
            end;
        end;
        if bLoadDefaults then begin
            //Set default options.
            bFakeStatics := True;
            bForceLOD8 := False;
            bIgnoreNoLOD := False;

            bMakeMissingMaterials := True;

            // This is used to report if a LOD material appears to be using a non-LOD texture.
            bReportNonLODMaterials := False;
            // This is used to report if a LOD mesh appears to have UVs outside of range.
            bReportUVs := False;
            // This is used to inform a LOD author of models that are missing LOD that are of a certain object bounds size or more.
            bReportMissingLOD := False;
            // This is used to ensure enable parented objects use LOD Respects Enable State
            bRespectEnableMarkers := True;

            //Default plugin name.
            sFolipPluginFileName := 'FOLIP - Patch';
            sFolipAfterGenerationPluginFileName := 'FOLIP - After Generation';

            bLightPlugin := True;
            bRemoveVWD := True;
            bSkipPrecombined := True;
            bRemoveBeforeGeneration := False;
        end;

        //Used to tell the Rule Editor whether or not to save changes.
        bSaveUserRules := False;
        bUserRulesChanged := False;

        //Get scaling
        uiScale := Screen.PixelsPerInch * 100 / 96;
        AddMessage('UI scale: ' + IntToStr(uiScale));

        sIgnoredWorldspaces := '';
        sEnableParentFormidExclusions := '';
        sIgnoredPlugins := '';


        FetchRules;
        slTopLevelModPatternPaths.Add('');

        if wbSimpleRecords then begin
            MessageDlg('Simple records must be unchecked in xEdit options', mtInformation, [mbOk], 0);
            Result := 1;
            Exit;
        end;

        //Display the main menu. If the user presses the cancel button, exit.
        if MainMenuForm then BeforeGeneration;
    finally
        tlStats.Free;
        tlActiFurnMstt.Free;
        tlMswp.Free;
        tlEnableParents.Free;
        tlStolenForms.Free;
        tlTxst.Free;
        tlFlst.Free;

        slPluginFiles.Free;
        slNifFiles.Free;
        slUsedLODNifFiles.Free;
        slMatFiles.Free;
        slTopLevelModPatternPaths.Free;
        slMessages.Free;
        slMissingLODMessages.Free;
        slFullLODMessages.Free;
        slMissingColorRemaps.Free;
        slMeshCheckMissingMaterials.Free;
        slMeshCheckNonLODMaterials.Free;
        slMeshCheckNoMaterialSpecified.Free;
        slCheckedModels.Free;
        slMismatchedFullModelToLODMaterials.Free;
        slHasLOD.Free;
        slFakeStatics.Free;
        slFOLIPTexgen_noalpha.Free;
        slFOLIPTexgen_copy.Free;
        slFOLIPTexgen_alpha.Free;
        slTexgen_copy.Free;
        slTexgen_noalpha.Free;
        slTexgen_alpha.Free;
        slOutsideUVRange.Free;
        slVerifyLODModels.Free;
        slMasterableMasters.Free;
        slPatchMasters.Free;
        slMainMasters.Free;
        slAddedTexGenTextures.Free;

        joRules.Free;
        joMswpMap.Free;
        joModelMatch.Free;
        joWinningCells.Free;


        //Save user rules
        if bSaveUserRules and bUserRulesChanged then begin
            AddMessage('Saving ' + IntToStr(joUserRules.Count) + ' user rule(s) to ' + wbDataPath + 'FOLIP\UserRules.json');
            joUserRules.SaveToFile(wbDataPath + 'FOLIP\UserRules.json', False, TEncoding.UTF8, True);
        end;
        joUserRules.Free;

        EnsureDirectoryExists(sOutputDir +'\');
        joElements.Free;

        joRasterizeMaterials.SaveToFile(sOutputDir + '\RasterizeMaterials.json', False, TEncoding.UTF8, True);
        joRasterizeMaterials.Free;

        //Save user settings
        AddMessage('Saving user settings to ' + wbDataPath + sUserSettingsFileName);
        joUserSettings.S['FakeStatics'] := BoolToStr(bFakeStatics);
        joUserSettings.S['ForceLOD8'] := BoolToStr(bForceLOD8);
        joUserSettings.S['IgnoreNoLOD'] := BoolToStr(bIgnoreNoLOD);
        joUserSettings.S['ReportNonLODMaterials'] := BoolToStr(bReportNonLODMaterials);
        joUserSettings.S['ReportUVs'] := BoolToStr(bReportUVs);
        joUserSettings.S['ReportMissingLOD'] := BoolToStr(bReportMissingLOD);
        joUserSettings.S['RespectEnableMarkers'] := BoolToStr(bRespectEnableMarkers);
        joUserSettings.S['BeforeGenerationPluginName'] := sFolipPluginFileName;
        joUserSettings.S['AfterGenerationPluginName'] := sFolipAfterGenerationPluginFileName;
        joUserSettings.S['LightPlugin'] := BoolToStr(bLightPlugin);
        joUserSettings.S['RemoveVWD'] := BoolToStr(bRemoveVWD);
        joUserSettings.S['LimitedHasDistantLODRemoval'] := BoolToStr(bLimitedHasDistantLODRemoval);
        joUserSettings.S['AddVWD'] := BoolToStr(bAddVWD);
        joUserSettings.S['SkipPrecombined'] := BoolToStr(bSkipPrecombined);
        joUserSettings.S['RemoveBeforeGeneration'] := BoolToStr(bRemoveBeforeGeneration);
        joUserSettings.S['MakeMissingMaterials'] := BoolToStr(bMakeMissingMaterials);

        joUserSettings.SaveToFile(wbDataPath + sUserSettingsFileName, False, TEncoding.UTF8, True);
        joUserSettings.Free;
    end;

    Result := 0;
end;

procedure BeforeGeneration;
var
    bSkip: Boolean;
    sFolipPluginFileNameSanitized, cmdline: string;
    fs: TFileStream;
    diamondCityWrld, goodneighborWrld: IwbElement;
begin
    bSkip := False;
    //Create FOLIP plugins
    if not CreatePlugins then Exit;

    SpecificRecordEdits;
    if bSkip then Exit;

    //Clear output directory
    DeleteDirectory(sOutputDir);
    DeleteDirectory(FOLIPTempPath);

    AddMessage('Collecting assets...');
    //Scan archives and loose files.
    slContainers := TStringList.Create;
    ResourceContainerList(slContainers);
    FilesInContainers(slContainers);
    slContainers.Free;
    AddMessage('Found ' + IntToStr(slNifFiles.Count) + ' lod models.');
    AddMessage('Found ' + IntToStr(slMatFiles.Count) + ' lod materials.');

    if bSkip then Exit;

    AddMessage('Collecting records...');
    CollectRecords;

    //Assign lod models to STAT records.
    AddMessage('Assigning LOD models to STAT records...');
    AssignLODModelsList;

    //Add fake statics for MSTT, FURN, and ACTI that have lod.
    if bFakeStatics then ProcessActiFurnMstt;

    CheckUsedLODModels;

    //Add Messages
    ListStringsInStringList(slMessages);
    ListStringsInStringList(slMissingLODMessages);
    ListStringsInStringList(slFullLODMessages);
    ListStringsInStringList(slMissingColorRemaps);
    if bDeepScan then begin
        AddMessage('Verify these model matches are correct');
        ListStringsInStringList(slVerifyLODModels);
    end;

    //Apply lod material swaps.
    AddMessage('Assigning LOD materials to ' + IntToStr(tlMswp.Count) + ' material swaps.');
    AssignLODMaterialsList;
    AddMessage('=======================================================================================');

    if bRespectEnableMarkers then ProcessEnableParents;
    AddMessage('=======================================================================================');

    MultiRefLOD;
    AddMastersForRecords;
    ProcessElements;

    diamondCityWrld := WinningOverride(RecordByFormID(FileByIndex(0), $00000F94, False));
    if not SameText(GetFileName(GetFile(diamondCityWrld)), GetFileName(iFolipPluginFile)) then begin
        diamondCityWrld := wbCopyElementToFile(diamondCityWrld, iFolipPluginFile, False, True);
    end;
    SetElementEditValues(diamondCityWrld, 'NAMA', '0.25');

    goodneighborWrld := WinningOverride(RecordByFormID(FileByIndex(0), $00054BD5, False));
    if not SameText(GetFileName(GetFile(goodneighborWrld)), GetFileName(iFolipPluginFile)) then begin
        goodneighborWrld := wbCopyElementToFile(goodneighborWrld, iFolipPluginFile, False, True);
    end;
    SetElementEditValues(goodneighborWrld, 'NAMA', '0.25');

    DeleteDirectory(FOLIPTempPath);

    EnsureDirectoryExists(sOutputDir +'\');
    joElements.SaveToFile(sOutputDir + '\joElements.json', False, TEncoding.UTF8, True);

    //Save the plugin.
    fs := TFileStream.Create(sOutputDir + '\' + sFolipMasterFileName, fmCreate);
    try
        FileWriteToStream(iFolipMasterfile, fs, 0);
    finally
        fs.Free;
    end;

    fs := TFileStream.Create(sOutputDir + '\' + sFolipPluginFileName + '.esp', fmCreate);
    try
        FileWriteToStream(iFolipPluginFile, fs, 0);
    finally
        fs.Free;
    end;

    //Save Texgen files
    if bMakeMissingMaterials then begin
        if slTexgen_alpha.Count + slTexgen_copy.Count + slTexgen_noalpha.Count > 0 then AddMessage('Saving TexGen files to ' + sOutputDir + '\' + 'DynDOLOD');
        sFolipPluginFileNameSanitized := StripNonAlphanumeric(sFolipPluginFileName) + 'esp';
        EnsureDirectoryExists(sOutputDir + '\' + 'DynDOLOD\');
        if slTexgen_alpha.Count > 0 then slTexgen_alpha.SaveToFile(sOutputDir + '\' + 'DynDOLOD\DynDOLOD_FO4_TexGen_alpha_' + sFolipPluginFileNameSanitized + '.txt');
        if slTexgen_copy.Count > 0 then slTexgen_copy.SaveToFile(sOutputDir + '\' + 'DynDOLOD\DynDOLOD_FO4_TexGen_copy_' + sFolipPluginFileNameSanitized + '.txt');
        if slTexgen_noalpha.Count > 0 then slTexgen_noalpha.SaveToFile(sOutputDir + '\' + 'DynDOLOD\DynDOLOD_FO4_TexGen_noalpha_' + sFolipPluginFileNameSanitized + '.txt');
    end;

    //Zip up output for easy installation
    AddMessage('Zipping up output for easy installation...');
    cmdline := '-Command "Compress-Archive -Path (Get-ChildItem ''' + wbScriptsPath + 'FOLIP\output'').FullName -DestinationPath ''' + wbScriptsPath + 'FOLIP\output\FOLIP Before Generation Output.zip''"';
    AddMessage(cmdline);
    AddMessage('Exit Code: ' + IntToStr(ShellExecuteWait(0, 'open', 'Powershell', cmdline, '', SW_SHOWNORMAL)));

    //Open the output folder in Explorer
    cmdline := '"'+ wbScriptsPath + 'FOLIP\output"';
    ShellExecute(0, 'open', 'explorer', cmdline, '', SW_SHOWNORMAL);

    MessageDlg('Patch generated successfully!' + #13#10#13#10 + 'Install the FOLIP Before Generation Output.zip file in your mod manager.', mtInformation, [mbOk], 0);
end;

// ----------------------------------------------------
// UI functions and procedures go below.
// ----------------------------------------------------

function MainMenuForm: Boolean;
{
    Main menu form.
}
var
    frm: TForm;
    btnRuleEditor, btnStart, btnCancel: TButton;
    edPluginName: TEdit;
    pnl: TPanel;
    picFolip: TPicture;
    fImage: TImage;
    gbOptions: TGroupBox;
    chkFakeStatics, chkForceLOD8, chkIgnoreNoLOD, chkReportNonLODMaterials, chkReportUVs, chkMakeMissingMaterials,
    chkReportMissingLOD, chkRespectEnableMarkers, chkDeepScan: TCheckBox;
begin
    frm := TForm.Create(nil);
    try
        frm.Caption := 'FOLIP xEdit Patcher';
        frm.Width := 640;
        frm.Height := 540;
        frm.Position := poMainFormCenter;
        frm.BorderStyle := bsDialog;
        frm.KeyPreview := True;
        frm.OnClose := frmOptionsFormClose;
        frm.OnKeyDown := FormKeyDown;

        picFolip := TPicture.Create;
        picFolip.LoadFromFile(wbScriptsPath + 'FOLIP\FOLIP.jpg');

        fImage := TImage.Create(frm);
		fImage.Picture := picFolip;
		fImage.Parent := frm;
        fImage.Width := 576;
		fImage.Height := 278;
		fImage.Left := 26;
		fImage.Top := 12;
        fImage.Stretch := True;

        gbOptions := TGroupBox.Create(frm);
        gbOptions.Parent := frm;
        gbOptions.Left := 6;
        gbOptions.Top := fImage.Top + fImage.Height + 24;
        gbOptions.Width := frm.Width - 30;
        gbOptions.Caption := 'FOLIP - Before Generation';
        gbOptions.Height := 164;

        btnRuleEditor := TButton.Create(frm);
        btnRuleEditor.Parent := frm;
        btnRuleEditor.Caption := 'Rule Editor';
        btnRuleEditor.OnClick := RuleEditor;
        btnRuleEditor.Width := 100;
        btnRuleEditor.Left := 16;
        btnRuleEditor.Top := gbOptions.Top + gbOptions.Height + 24;

        edPluginName := TEdit.Create(gbOptions);
        edPluginName.Parent := gbOptions;
        edPluginName.Name := 'edPluginName';
        edPluginName.Left := 104;
        edPluginName.Top := 30;
        edPluginName.Width := 180;
        edPluginName.Hint := 'Sets the output plugin name for the patch.';
        edPluginName.ShowHint := True;
        CreateLabel(gbOptions, 16, edPluginName.Top + 4, 'Output Plugin:');

        chkFakeStatics := TCheckBox.Create(gbOptions);
        chkFakeStatics.Parent := gbOptions;
        chkFakeStatics.Left := edPluginName.Left + edPluginName.Width + 16;
        chkFakeStatics.Top := 32;
        chkFakeStatics.Width := 120;
        chkFakeStatics.Caption := 'Create Fake Statics';
        chkFakeStatics.Hint := 'Allows activator (ACTI), door (DOOR), furniture (FURN),'
            + #13#10 + 'and moveable static (MSTT) references to receive LOD by creating'
            + #13#10 + 'invisible static reference duplicates with LOD attached.';
        chkFakeStatics.ShowHint := True;

        chkForceLOD8 := TCheckBox.Create(gbOptions);
        chkForceLOD8.Parent := gbOptions;
        chkForceLOD8.Left := chkFakeStatics.Left + chkFakeStatics.Width + 32;
        chkForceLOD8.Top := chkFakeStatics.Top;
        chkForceLOD8.Width := 120;
        chkForceLOD8.Caption := 'Force LOD8';
        chkForceLOD8.Hint := 'If an object has a LOD4 model assigned but not a LOD8 model,'
            + #13#10 + 'this will use the LOD4 model in LOD8. Use this for increased'
            + #13#10 + 'LOD distance at the cost of some performance. This is better than'
            + #13#10 + 'increasing the fBlockLevel0Distance.';
        chkForceLOD8.ShowHint := True;



        chkReportNonLODMaterials := TCheckBox.Create(gbOptions);
        chkReportNonLODMaterials.Parent := gbOptions;
        chkReportNonLODMaterials.Left := 16;
        chkReportNonLODMaterials.Top := chkForceLOD8.Top + 30;
        chkReportNonLODMaterials.Width := 120;
        chkReportNonLODMaterials.Caption := 'Report Materials';
        chkReportNonLODMaterials.Hint := 'Adds various warnings for debugging LOD materials.';
        chkReportNonLODMaterials.ShowHint := True;

        chkReportUVs := TCheckBox.Create(gbOptions);
        chkReportUVs.Parent := gbOptions;
        chkReportUVs.Left := chkReportNonLODMaterials.Left + chkReportNonLODMaterials.Width + 16;
        chkReportUVs.Top := chkReportNonLODMaterials.Top;
        chkReportUVs.Width := 120;
        chkReportUVs.Caption := 'Report UVs';
        chkReportUVs.Hint := 'Adds warnings to LOD meshes that appear to be'
            + #13#10 + 'outside the 0 to 1 UV range. This is for information'
            + #13#10 + 'purposes only and has no visual benefit.';
        chkReportUVs.ShowHint := True;

        chkRespectEnableMarkers := TCheckBox.Create(gbOptions);
        chkRespectEnableMarkers.Parent := gbOptions;
        chkRespectEnableMarkers.Left := chkFakeStatics.Left;
        chkRespectEnableMarkers.Top := chkReportUVs.Top;
        chkRespectEnableMarkers.Width := 200;
        chkRespectEnableMarkers.Caption := 'Respect Enable Parents';
        chkRespectEnableMarkers.Hint := 'Adds LOD Respects State Flag to relevant'
            + #13#10 + 'enable parented references and works around'
            + #13#10 + 'engine bugs related to its usage.';
        chkRespectEnableMarkers.ShowHint := True;

        chkIgnoreNoLOD := TCheckBox.Create(gbOptions);
        chkIgnoreNoLOD.Parent := gbOptions;
        chkIgnoreNoLOD.Left := chkFakeStatics.Left + chkFakeStatics.Width + 32;
        chkIgnoreNoLOD.Top := chkRespectEnableMarkers.Top;
        chkIgnoreNoLOD.Width := 150;
        chkIgnoreNoLOD.Caption := 'Ignore NoLOD EditorIDs';
        chkIgnoreNoLOD.Hint := 'If the editor id ends in NoLOD, normal operation,'
            + #13#10 + 'would honor that and not attach LOD to it. Checking this'
            + #13#10 + 'will ignore it and use LOD if available.';
        chkIgnoreNoLOD.ShowHint := True;

        chkMakeMissingMaterials := TCheckBox.Create(gbOptions);
        chkMakeMissingMaterials.Parent := gbOptions;
        chkMakeMissingMaterials.Left := chkReportNonLODMaterials.Left;
        chkMakeMissingMaterials.Top := chkReportUVs.Top + 30;
        chkMakeMissingMaterials.Width := 200;
        chkMakeMissingMaterials.Caption := 'Auto Generate Missing Materials';
        chkMakeMissingMaterials.Hint := 'Missing LOD materials will be'
            + #13#10 + 'automatically generated.';
        chkMakeMissingMaterials.ShowHint := True;

        chkReportMissingLOD := TCheckBox.Create(gbOptions);
        chkReportMissingLOD.Parent := gbOptions;
        chkReportMissingLOD.Left := chkFakeStatics.Left;
        chkReportMissingLOD.Top := chkReportUVs.Top + 30;
        chkReportMissingLOD.Width := 200;
        chkReportMissingLOD.Caption := 'Report Large Objects without LOD';
        chkReportMissingLOD.Hint := 'Adds messages about large objects'
            + #13#10 + 'that do not have LOD. This is for information'
            + #13#10 + 'purposes only and has no visual benefit.';
        chkReportMissingLOD.ShowHint := True;

        chkDeepScan := TCheckBox.Create(gbOptions);
        chkDeepScan.Parent := gbOptions;
        chkDeepScan.Left := chkReportNonLODMaterials.Left;
        chkDeepScan.Top := chkMakeMissingMaterials.Top + 30;
        chkDeepScan.Width := 150;
        chkDeepScan.Caption := 'Deep Scan';
        chkDeepScan.Hint := 'Attempts to find more LOD models'
            + #13#10 + 'based on file name or nif block.';
        chkDeepScan.ShowHint := True;

        btnStart := TButton.Create(frm);
        btnStart.Parent := frm;
        btnStart.Caption := 'Start';
        btnStart.ModalResult := mrOk;
        btnStart.Top := btnRuleEditor.Top;

        btnCancel := TButton.Create(frm);
        btnCancel.Parent := frm;
        btnCancel.Caption := 'Cancel';
        btnCancel.ModalResult := mrCancel;
        btnCancel.Top := btnStart.Top;

        btnStart.Left := gbOptions.Width - btnStart.Width - btnCancel.Width - 16;
        btnCancel.Left := btnStart.Left + btnStart.Width + 8;

        pnl := TPanel.Create(frm);
        pnl.Parent := frm;
        pnl.Left := gbOptions.Left;
        pnl.Top := btnStart.Top - 12;
        pnl.Width := gbOptions.Width;
        pnl.Height := 2;

        frm.ActiveControl := btnStart;
        frm.ScaleBy(uiScale, 100);
        frm.Font.Size := 8;
        frm.Height := btnStart.Top + btnStart.Height + btnStart.Height + 30;

        edPluginName.Text := sFolipPluginFileName;
        chkFakeStatics.Checked := bFakeStatics;
        chkForceLOD8.Checked := bForceLOD8;
        chkIgnoreNoLOD.Checked := bIgnoreNoLOD;
        chkReportNonLODMaterials.Checked := bReportNonLODMaterials;
        chkReportUVs.Checked := bReportUVs;
        chkReportMissingLOD.Checked := bReportMissingLOD;
        chkRespectEnableMarkers.Checked := bRespectEnableMarkers;
        chkMakeMissingMaterials.Checked := bMakeMissingMaterials;
        chkDeepScan.Checked := bDeepScan;

        if frm.ShowModal <> mrOk then begin
            Result := False;
            Exit;
        end
        else Result := True;

        sFolipPluginFileName := edPluginName.Text;
        bFakeStatics := chkFakeStatics.Checked;
        bForceLOD8 := chkForceLOD8.Checked;
        bIgnoreNoLOD := chkIgnoreNoLOD.Checked;
        bReportNonLODMaterials := chkReportNonLODMaterials.Checked;
        bReportUVs := chkReportUVs.Checked;
        bReportMissingLOD := chkReportMissingLOD.Checked;
        bRespectEnableMarkers := chkRespectEnableMarkers.Checked;
        bMakeMissingMaterials := chkMakeMissingMaterials.Checked;
        bDeepScan := chkDeepScan.Checked;

    finally
        frm.Free;
    end;
end;

procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{
    Cancel if Escape key is pressed.
}
begin
    if Key = VK_ESCAPE then TForm(Sender).ModalResult := mrCancel;
end;

procedure frmOptionsFormClose(Sender: TObject; var Action: TCloseAction);
{
    Close form handler.
}
begin
    if TForm(Sender).ModalResult <> mrOk then Exit
    else bSaveUserRules := True;
end;

// ----------------------------------------------------
// Rules menu

function RuleEditor: Boolean;
var
    i: integer;
    mnRules: TPopupMenu;
    MenuItem: TMenuItem;
    lbl: TLabel;
    frm: TForm;
begin
    frm := TForm.Create(nil);
    try
        frm.Caption := 'Rule Editor';
        frm.Width := 1200;
        frm.Height := 600;
        frm.Position := poMainFormCenter;
        frm.BorderStyle := bsSizeable;
        frm.KeyPreview := True;
        frm.OnClose := frmOptionsFormClose;
        frm.OnKeyDown := FormKeyDown;
        frm.OnResize := frmResize;

        lvRules := TListView.Create(frm);
        lvRules.Parent := frm;

        lvRules.Top := 24;
        lvRules.Width := frm.Width - 36;
        lvRules.Left := (frm.Width - lvRules.Width)/2;
        lvRules.Height := frm.Height - 110;
        lvRules.ReadOnly := True;
        lvRules.ViewStyle := vsReport;
        lvRules.RowSelect := True;
        lvRules.DoubleBuffered := True;
        lvRules.Columns.Add.Caption := 'Mesh / EditorID';
        lvRules.Columns[0].Width := 400;
        lvRules.Columns.Add.Caption := 'HasDistantLOD';
        lvRules.Columns[1].Width := 95;
        lvRules.Columns.Add.Caption := 'IsFullLOD';
        lvRules.Columns[2].Width := 95;
        lvRules.Columns.Add.Caption := 'LOD4';
        lvRules.Columns[3].Width := 400;
        lvRules.Columns.Add.Caption := 'LOD8';
        lvRules.Columns[4].Width := 90;
        lvRules.Columns.Add.Caption := 'LOD16';
        lvRules.Columns[5].Width := 60;
        lvRules.Columns.Add.Caption := 'LOD32';
        lvRules.Columns[6].Width := 60;
        lvRules.OwnerData := True;
        lvRules.OnData := lvRulesData;
        lvRules.OnDblClick := lvRulesDblClick;
        lvRules.Items.Count := joRules.Count;
        CreateLabel(frm, 16, lvRules.Top - 20, 'Mesh/EditorID Rule LOD Assignments');

        mnRules := TPopupMenu.Create(frm);
        lvRules.PopupMenu := mnRules;
        MenuItem := TMenuItem.Create(mnRules);
        MenuItem.Caption := 'Add';
        MenuItem.OnClick := RulesMenuAddClick;
        mnRules.Items.Add(MenuItem);
        MenuItem := TMenuItem.Create(mnRules);
        MenuItem.Caption := 'Delete';
        MenuItem.OnClick := RulesMenuDeleteClick;
        mnRules.Items.Add(MenuItem);
        MenuItem := TMenuItem.Create(mnRules);
        MenuItem.Caption := 'Edit';
        MenuItem.OnClick := RulesMenuEditClick;
        mnRules.Items.Add(MenuItem);

        btnRuleOk := TButton.Create(frm);
        btnRuleOk.Parent := frm;
        btnRuleOk.Caption := 'OK';
        btnRuleOk.ModalResult := mrOk;
        btnRuleOk.Top := lvRules.Height + lvRules.Top + 8;

        btnRuleCancel := TButton.Create(frm);
        btnRuleCancel.Parent := frm;
        btnRuleCancel.Caption := 'Cancel';
        btnRuleCancel.ModalResult := mrCancel;
        btnRuleCancel.Top := btnRuleOk.Top;

        btnRuleOk.Left := (frm.Width - btnRuleOk.Width - btnRuleCancel.Width - 8)/2;
        btnRuleCancel.Left := btnRuleOk.Left + btnRuleOk.Width + 8;

        frm.ScaleBy(uiScale, 100);
        frm.Font.Size := 8;

        if frm.ShowModal <> mrOk then begin
            Result := False;
            Exit;
        end
        else Result := True;

    finally
        frm.Free;
    end;
end;

function EditRuleForm(var key, lod4, lod8, lod16, lod32: string; var hasdistantlod, bIsFullLod: Boolean): Boolean;
var
    frmRule: TForm;
    pnl: TPanel;
    btnOk, btnCancel: TButton;
    edKey, edHasDistantLOD, edlod4, edlod8, edlod16, edlod32: TEdit;
    chkHasDistantLOD, chkIsFullLOD: TCheckBox;
begin
  frmRule := TForm.Create(nil);
  try
    frmRule.Caption := 'Mesh/EditorID Rule';
    frmRule.Width := 600;
    frmRule.Height := 300;
    frmRule.Position := poMainFormCenter;
    frmRule.BorderStyle := bsDialog;
    frmRule.KeyPreview := True;
    frmRule.OnKeyDown := FormKeyDown;
    frmRule.OnClose := frmRuleFormClose;

    edKey := TEdit.Create(frmRule);
    edKey.Parent := frmRule;
    edKey.Name := 'edKey';
    edKey.Left := 120;
    edKey.Top := 12;
    edKey.Width := frmRule.Width - 150;
    CreateLabel(frmRule, 16, edKey.Top + 4, 'Mesh or EditorID');

    chkHasDistantLOD := TCheckBox.Create(frmRule);
    chkHasDistantLOD.Parent := frmRule;
    chkHasDistantLOD.Left := 16;
    chkHasDistantLOD.Top := edKey.Top + 32;
    chkHasDistantLOD.Width := 200;
    chkHasDistantLOD.Caption := 'Has Distant LOD';

    chkIsFullLOD := TCheckBox.Create(frmRule);
    chkIsFullLOD.Parent := frmRule;
    chkIsFullLOD.Left := chkHasDistantLOD.Left + chkHasDistantLOD.Width + 16;
    chkIsFullLOD.Top := edKey.Top + 32;
    chkIsFullLOD.Width := 200;
    chkIsFullLOD.Caption := 'Is Full LOD';

    edlod4 := TEdit.Create(frmRule);
    edlod4.Parent := frmRule;
    edlod4.Name := 'edlod4';
    edlod4.Left := 120;
    edlod4.Top := chkHasDistantLOD.Top + 28;
    edlod4.Width := frmRule.Width - 150;
    CreateLabel(frmRule, 16, edlod4.Top + 4, 'LOD4');

    edlod8 := TEdit.Create(frmRule);
    edlod8.Parent := frmRule;
    edlod8.Name := 'edlod8';
    edlod8.Left := 120;
    edlod8.Top := edlod4.Top + 28;
    edlod8.Width := frmRule.Width - 150;
    CreateLabel(frmRule, 16, edlod8.Top + 4, 'LOD8');

    edlod16 := TEdit.Create(frmRule);
    edlod16.Parent := frmRule;
    edlod16.Name := 'edlod16';
    edlod16.Left := 120;
    edlod16.Top := edlod8.Top + 28;
    edlod16.Width := frmRule.Width - 150;
    CreateLabel(frmRule, 16, edlod16.Top + 4, 'LOD16');

    edlod32 := TEdit.Create(frmRule);
    edlod32.Parent := frmRule;
    edlod32.Name := 'edlod32';
    edlod32.Left := 120;
    edlod32.Top := edlod16.Top + 28;
    edlod32.Width := frmRule.Width - 150;
    CreateLabel(frmRule, 16, edlod32.Top + 4, 'LOD32');

    btnOk := TButton.Create(frmRule);
    btnOk.Parent := frmRule;
    btnOk.Caption := 'OK';
    btnOk.ModalResult := mrOk;
    btnOk.Top := edlod32.Top + (2*edlod32.Height);

    btnCancel := TButton.Create(frmRule);
    btnCancel.Parent := frmRule;
    btnCancel.Caption := 'Cancel';
    btnCancel.ModalResult := mrCancel;
    btnCancel.Top := btnOk.Top;

    btnOk.Left := frmRule.Width - btnOk.Width - btnCancel.Width - 32;
    btnCancel.Left := btnOk.Left + btnOk.Width + 8;

    pnl := TPanel.Create(frmRule);
    pnl.Parent := frmRule;
    pnl.Left := 10;
    pnl.Top := btnOk.Top - 12;
    pnl.Width := frmRule.Width - 32;
    pnl.Height := 2;

    frmRule.Height := btnOk.Top + (4*btnOk.Height);
    frmRule.ScaleBy(uiScale, 100);
    frmRule.Font.Size := 8;

    edKey.Text := key;
    chkHasDistantLOD.Checked := hasdistantlod;
    chkIsFullLOD.Checked := bIsFullLod;
    edlod4.Text := lod4;
    edlod8.Text := lod8;
    edlod16.Text := lod16;
    edlod32.Text := lod32;

    if frmRule.ShowModal <> mrOk then Exit;

    key := LowerCase(edKey.Text);
    hasdistantlod := chkHasDistantLOD.Checked;
    bIsFullLod := chkIsFullLOD.Checked;
    lod4 := edlod4.Text;
    lod8 := edlod8.Text;
    lod16 := edlod16.Text;
    lod32 := edlod32.Text;
    Result := True;
  finally
    frmRule.Free;
  end;
end;

procedure lvRulesData(Sender: TObject; Item: TListItem);
{
    Populate lvRules
}
var
    i: integer;
    key: string;
begin
    key := joRules.Names[Item.Index];
    Item.Caption := key;
    Item.SubItems.Add(Fallback(joRules.O[key].S['hasdistantlod'], 'false'));
    Item.SubItems.Add(Fallback(joRules.O[key].S['bisfulllod'], 'false'));
    Item.SubItems.Add(joRules.O[key].S['level0']);
    Item.SubItems.Add(joRules.O[key].S['level1']);
    Item.SubItems.Add(joRules.O[key].S['level2']);
    Item.SubItems.Add(joRules.O[key].S['level3']);
end;

procedure lvRulesDblClick(Sender: TObject);
{
    Double click to edit rule
}
begin
    RulesMenuEditClick(nil);
end;

procedure RulesMenuAddClick(Sender: TObject);
{
    Add rule
}
var
    idx: integer;
    key, lod4, lod8, lod16, lod32: string;
    hasdistantlod, bIsFullLod: Boolean;
begin
    key := '';
    hasdistantlod := True;
    bIsFullLod := False;
    lod4 := '';
    lod8 := '';
    lod16 := '';
    lod32 := '';

    if not EditRuleForm(key, lod4, lod8, lod16, lod32, hasdistantlod, bIsFullLod) then Exit;

    joRules.O[key].S['hasdistantlod'] := BoolToStr(hasdistantlod);
    joRules.O[key].S['bisfulllod'] := BoolToStr(bIsFullLod);
    joRules.O[key].S['level0'] := lod4;
    joRules.O[key].S['level1'] := lod8;
    joRules.O[key].S['level2'] := lod16;
    joRules.O[key].S['level3'] := lod32;

    joUserRules.O[key].Assign(joRules.O[key]);
    bUserRulesChanged := True;

    lvRules.Items.Count := joRules.Count;
    lvRules.Refresh;
end;

procedure RulesMenuEditClick(Sender: TObject);
{
    Edit rule
}
var
    idx: integer;
    key, lod4, lod8, lod16, lod32: string;
    hasdistantlod, bIsFullLod: Boolean;
begin
    if not Assigned(lvRules.Selected) then Exit;
    idx := lvRules.Selected.Index;

    key := joRules.Names[idx];
    hasdistantlod := StrToBool(joRules.O[key].S['hasdistantlod']);
    bIsFullLod := StrToBool(joRules.O[key].S['bisfulllod']);
    lod4 := joRules.O[key].S['level0'];
    lod8 := joRules.O[key].S['level1'];
    lod16 := joRules.O[key].S['level2'];
    lod32 := joRules.O[key].S['level3'];

    if not EditRuleForm(key, lod4, lod8, lod16, lod32, hasdistantlod, bIsFullLod) then Exit;

    joRules.O[key].S['hasdistantlod'] := BoolToStr(hasdistantlod);
    joRules.O[key].S['bisfulllod'] := BoolToStr(bIsFullLod);
    joRules.O[key].S['level0'] := lod4;
    joRules.O[key].S['level1'] := lod8;
    joRules.O[key].S['level2'] := lod16;
    joRules.O[key].S['level3'] := lod32;

    joUserRules.O[key].Assign(joRules.O[key]);
    bUserRulesChanged := True;

    lvRules.Items.Count := joRules.Count;
    lvRules.Refresh;
end;

procedure RulesMenuDeleteClick(Sender: TObject);
{
    Delete rule
}
var
    idx, uidx: integer;
    key: string;
begin
    if not Assigned(lvRules.Selected) then Exit;
    idx := lvRules.Selected.Index;
    key := joRules.Names[idx];
    joRules.Delete(idx);
    uidx := joUserRules.IndexOf(key);
    if uidx > -1 then begin
        joUserRules.Delete(uidx);
        bUserRulesChanged := True;
    end;
    lvRules.Items.Count := joRules.Count;
    lvRules.Refresh;
end;

procedure frmRuleFormClose(Sender: TObject; var Action: TCloseAction);
{
    Close rule edit menu handler.
}
begin
    if TForm(Sender).ModalResult <> mrOk then Exit;
    if TEdit(TForm(Sender).FindComponent('edKey')).Text = '' then begin
        MessageDlg('Mesh/EditorID must not be empty.', mtInformation, [mbOk], 0);
        Action := caNone;
    end;
end;

procedure frmResize(Sender: TObject);
{
    Handle resizing of elements in the rule menu.
}
var
    frm: TForm;
begin
    try
        frm := TForm(Sender);
        lvRules.Width := frm.Width - 36;
        lvRules.Left := (frm.Width - lvRules.Width)/2;
        lvRules.Height := frm.Height - btnRuleOk.Height - btnRuleOk.Height - btnRuleOk.Height - btnRuleOk.Height;

        btnRuleOk.Top := lvRules.Height + lvRules.Top + 8;
        btnRuleCancel.Top := btnRuleOk.Top;
        btnRuleOk.Left := (frm.Width - btnRuleOk.Width - btnRuleCancel.Width - 8)/2;
        btnRuleCancel.Left := btnRuleOk.Left + btnRuleOk.Width + 8;
    except
        frm := TForm(Sender);
    end;
end;

// ----------------------------------------------------
// Record processing Functions and Procedures go below.
// ----------------------------------------------------

procedure SpecificRecordEdits;
{
    Special record modifications to the target plugins
}
var
    flstGroup, flstGroupMaster: IwbGroupRecord;
begin
    //Purge FOLIP - Before Generation.esp
    if HasGroup(iFolipPluginFile, 'STAT') then begin
        RemoveNode(GroupBySignature(iFolipPluginFile, 'STAT'));
    end;
    if HasGroup(iFolipPluginFile, 'MSWP') then begin
        RemoveNode(GroupBySignature(iFolipPluginFile, 'MSWP'));
    end;
    if HasGroup(iFolipPluginFile, 'CELL') then begin
        RemoveNode(GroupBySignature(iFolipPluginFile, 'CELL'));
    end;
    if HasGroup(iFolipPluginFile, 'WRLD') then begin
        RemoveNode(GroupBySignature(iFolipPluginFile, 'WRLD'));
    end;
    if HasGroup(iFolipPluginFile, 'FLST') then begin
        RemoveNode(GroupBySignature(iFolipPluginFile, 'FLST'));
    end;

    //Purge FOLIP - Master.esm
    if HasGroup(iFolipMasterFile, 'STAT') then begin
        RemoveNode(GroupBySignature(iFolipMasterFile, 'STAT'));
    end;
    if HasGroup(iFolipMasterFile, 'MSWP') then begin
        RemoveNode(GroupBySignature(iFolipMasterFile, 'MSWP'));
    end;
    if HasGroup(iFolipMasterFile, 'CELL') then begin
        RemoveNode(GroupBySignature(iFolipMasterFile, 'CELL'));
    end;
    if HasGroup(iFolipMasterFile, 'WRLD') then begin
        RemoveNode(GroupBySignature(iFolipMasterFile, 'WRLD'));
    end;


    flstGroup := Add(iFolipPluginFile, 'FLST', True);
    //Add FOLIP_Overrides formlist
    flOverrides := Add(flstGroup, 'FLST', True);
    SetEditorID(flOverrides, 'FOLIP_Overrides');
    tlFlst.Add(flOverrides);

    flMultiRefLOD := Add(flstGroup, 'FLST', True);
    SetEditorID(flMultiRefLOD, 'FOLIP_MultiRefLOD');
    tlFlst.Add(flMultiRefLOD);

    flParents := Add(flstGroup, 'FLST', True);
    SetEditorID(flParents, 'FOLIP_Parents');
    tlFlst.Add(flParents);

    flNeverfades := Add(flstGroup, 'FLST', True);
    SetEditorID(flNeverfades, 'FOLIP_Neverfades');
    tlFlst.Add(flNeverfades);

    flDecals := Add(flstGroup, 'FLST', True);
    SetEditorID(flDecals, 'FOLIP_Decals');
    tlFlst.Add(flDecals);

    flFakeStatics := Add(flstGroup, 'FLST', True);
    SetEditorID(flFakeStatics, 'FOLIP_FakeStatics');
    tlFlst.Add(flFakeStatics);

    flRemoveIsFullLOD := Add(flstGroup, 'FLST', True);
    SetEditorID(flRemoveIsFullLOD, 'FOLIP_RemoveIsFullLOD');
    tlFlst.Add(flRemoveIsFullLOD);

    //7

    flstGroupMaster := Add(iFolipMasterFile, 'FLST', True);
    //Add FOLIP_Overrides formlist
    flOverridesMaster := Add(flstGroupMaster, 'FLST', True);
    SetEditorID(flOverridesMaster, 'FOLIP_OverridesMaster');
    tlFlst.Add(flOverridesMaster);

    flMultiRefLODMaster := Add(flstGroupMaster, 'FLST', True);
    SetEditorID(flMultiRefLODMaster, 'FOLIP_MultiRefLODMaster');
    tlFlst.Add(flMultiRefLODMaster);

    flParentsMaster := Add(flstGroupMaster, 'FLST', True);
    SetEditorID(flParentsMaster, 'FOLIP_ParentsMaster');
    tlFlst.Add(flParentsMaster);

    flNeverfadesMaster := Add(flstGroupMaster, 'FLST', True);
    SetEditorID(flNeverfadesMaster, 'FOLIP_NeverfadesMaster');
    tlFlst.Add(flNeverfadesMaster);

    flDecalsMaster := Add(flstGroupMaster, 'FLST', True);
    SetEditorID(flDecalsMaster, 'FOLIP_DecalsMaster');
    tlFlst.Add(flDecalsMaster);

    flFakeStaticsMaster := Add(flstGroupMaster, 'FLST', True);
    SetEditorID(flFakeStaticsMaster, 'FOLIP_FakeStaticsMaster');
    tlFlst.Add(flFakeStaticsMaster);

    flRemoveIsFullLODMaster := Add(flstGroupMaster, 'FLST', True);
    SetEditorID(flRemoveIsFullLODMaster, 'FOLIP_RemoveIsFullLODMaster');
    tlFlst.Add(flRemoveIsFullLODMaster);
end;

procedure AddMastersForRecords;
{
    Adds required masters.
}
var
    i: integer;
begin
    for i := 0 to Pred(slMainMasters.Count) do begin
        AddMasterIfMissing(iFolipMasterFile, slMainMasters[i]);
        AddMasterIfMissing(iFolipPluginFile, slMainMasters[i]);
    end;
    for i := 0 to Pred(slPatchMasters.Count) do begin
        AddMasterIfMissing(iFolipPluginFile, slPatchMasters[i]);
    end;
    SortMasters(iFolipMasterFile);
    SortMasters(iFolipPluginFile);
end;

procedure ProcessElements;
{
    Process joElements.
}
var
    f, i, w, x, y, r: integer;
    recordId, wrldEdid, wrldRecordId, cellRecordId, ref, cellX, cellY, fileHere, messageHere: string;
    bFolipMaster, bInterior, bPersistent, bAddedPersistentWorldspaceCell: boolean;
    rWrld, rCell, nCell: IwbMainRecord;
begin
    AddMessage('Processing elements...');
    {
    joElements is a json object that contains all the record edit information that will be applied to the output plugins. The structure of joElements is as follows:

    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].A['AddRefToMyFormlist'].Add(tlFlst.IndexOf(flDecals));
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['AddLinkedReference'] := '00195411:Fallout4.esm|' + MultiRefLODReference;
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['Is Full LOD'] := 0;
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['LOD Respects Enable State'] := 1;
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['NAME'] := '000e4610'; //Enable Marker
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['Opposite Enable Parent'] := 1;
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['RemoveLinkedReference'] := '00195411:Fallout4.esm';
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['Set Is Persistent'] := 1;
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['Visible When Distant'] := 1;
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['XESP Reference'] := parentFormid;
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['XESP'] := 1;

    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].A['AddRefToMyFormlist'].Add(tlFlst.IndexOf(flFakeStatics));
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].O['pos'].S['x'] := GetElementNativeValues(r, 'DATA\Position\X');
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].O['pos'].S['y'] := GetElementNativeValues(r, 'DATA\Position\Y');
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].O['pos'].S['z'] := GetElementNativeValues(r, 'DATA\Position\Z');
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].O['rot'].S['x'] := GetElementNativeValues(r, 'DATA\Rotation\X');
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].O['rot'].S['y'] := GetElementNativeValues(r, 'DATA\Rotation\Y');
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].O['rot'].S['z'] := GetElementNativeValues(r, 'DATA\Rotation\Z');
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].S['fakeStatic'] := fakeStatic;
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].S['Initially Disabled'] := 1;
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].S['Opposite Enable Parent'] := 0;
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].S['Visible When Distant'] := 1;
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].S['XESP Reference'] := parentFormid;
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].S['XESP'] := 1;
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].S['XMSP - Material Swap'] := IntToHex(GetLoadOrderFormID(ms), 8);
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].S['XSCL - Scale'] := GetElementNativeValues(r, 'XSCL - Scale');

    joElements.O['STAT'].O['New'].O[fakeStatic].S['Copy Object Bounds'] := True;
    joElements.O['STAT'].O['New'].O[fakeStatic].S['EDID'] := fakeStaticEditorId;
    joElements.O['STAT'].O['New'].O[fakeStatic].S['File'] := GetFileName(iCurrentPlugin);
    joElements.O['STAT'].O['New'].O[fakeStatic].S['Is Marker'] := 1;
    joElements.O['STAT'].O['New'].O[fakeStatic].S['MODS'] := msFormid;
    joElements.O['STAT'].O['New'].O[recordId].S['Has Distant LOD'] := joLOD.I['hasdistantlod'];
    joElements.O['STAT'].O['New'].O[recordId].S['Level 0'] := joLOD.S['level0'];
    joElements.O['STAT'].O['New'].O[recordId].S['Level 1'] := joLOD.S['level1'];
    joElements.O['STAT'].O['New'].O[recordId].S['Level 2'] := joLOD.S['level2'];
    joElements.O['STAT'].O['New'].O[recordId].S['Level 3'] := joLOD.S['level3'];

    joElements.O['STAT'].O['Overrides'].O[recordId].S['File'] := GetFileName(iCurrentPlugin);
    joElements.O['STAT'].O['Overrides'].O[recordId].S['Has Distant LOD'] := joLOD.I['hasdistantlod'];
    joElements.O['STAT'].O['Overrides'].O[recordId].S['Level 0'] := joLOD.S['level0'];
    joElements.O['STAT'].O['Overrides'].O[recordId].S['Level 1'] := joLOD.S['level1'];
    joElements.O['STAT'].O['Overrides'].O[recordId].S['Level 2'] := joLOD.S['level2'];
    joElements.O['STAT'].O['Overrides'].O[recordId].S['Level 3'] := joLOD.S['level3'];

    joElements.O['MSWP'].O['Overrides'].O[recordId].S['File'] := GetFileName(iCurrentPlugin);
    joElements.O['MSWP'].O['Overrides'].O[recordId].A['AddMaterialSwap'].Add(slLODSubOriginal[n] + '|' + slLODSubReplacement[n]);
    }
    //AddMaterialSwap(mn, slLODSubOriginal[n], slLODSubReplacement[n]);

    //Process material swaps
    for i := 0 to Pred(joElements.O['MSWP'].O['Overrides'].Count) do begin
        recordId := joElements.O['MSWP'].O['Overrides'].Names[i];
        ProcessMaterialSwap(recordId);
    end;

    //Process STAT overrides
    for i := 0 to Pred(joElements.O['STAT'].O['Overrides'].Count) do begin
        recordId := joElements.O['STAT'].O['Overrides'].Names[i];
        ProcessStatOverride(recordId);
    end;

    //Process new STAT records
    for i := 0 to Pred(joElements.O['STAT'].O['New'].Count) do begin
        recordId := joElements.O['STAT'].O['New'].Names[i];
        fileHere := joElements.O['STAT'].O['New'].O[recordId].S['File'];
        ProcessNewStat(recordId, fileHere);
        if SameText(fileHere, sFolipMasterFileName) then ProcessNewStat(recordId, sFolipPluginFileName + '.esp');
    end;

    for f := 0 to Pred(joElements.O['references'].Count) do begin
        pluginFileNameHere := joElements.O['references'].Names[f];
        bFolipMaster := SameText(pluginFileNameHere, sFolipMasterFileName);
        iCurrentPlugin := FileByName(pluginFileNameHere);
        for w := 0 to Pred(joElements.O['references'].O[pluginFileNameHere].Count) do begin
            wrldEdid := joElements.O['references'].O[pluginFileNameHere].Names[w];
            bInterior := SameText(wrldEdid, '');
            bAddedPersistentWorldspaceCell := false;
            for x := 0 to Pred(joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].Count) do begin
                cellX := joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].Names[x];
                for y := 0 to Pred(joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].Count) do begin
                    cellY := joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].Names[y];

                    if not bInterior then begin
                        //Add wrld
                        wrldRecordId := joWinningCells.O[wrldEdid].S['RecordID'];
                        if bFolipMaster then
                            rWrld := GetHighestPossibleOverrideForFile(GetRecordFromFormIdFileId(wrldRecordId), iCurrentPlugin)
                        else rWrld := WinningOverrideIgnoringThisFile(GetRecordFromFormIdFileId(wrldRecordId), sFolipMasterFileName);
                        if Assigned(rWrld) then wbCopyElementToFile(rWrld, iCurrentPlugin, False, True);

                        //Add cell
                        cellRecordId := joWinningCells.O[wrldEdid].O[cellX].O[cellY].S['RecordID'];
                        //It is possible for a world to not have a cell at 0,0, but have persistent references that indicate this cell. Fall back to persistent worldspace cell in this case.
                        if (((cellX = '0') and (cellY = '0')) and SameText(cellRecordID, '')) then cellRecordId := joWinningCells.O[wrldEdid].O['PersistentWorldspaceCell'].S['RecordID'];
                        if bFolipMaster then
                            rCell := GetHighestPossibleOverrideForFile(GetRecordFromFormIdFileId(cellRecordId), iCurrentPlugin)
                        else rCell := WinningOverrideIgnoringThisFile(GetRecordFromFormIdFileId(cellRecordId), sFolipMasterFileName);
                        if IsInIgnoredPlugin(MasterOrSelf((rCell))) then continue;
                        if Assigned(rCell) then nCell := wbCopyElementToFile(rCell, iCurrentPlugin, False, True);
                    end;


                    //Process Overrides
                    for r := Pred(joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].Count) downto 0 do begin
                        bInterior := False;
                        ref := joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].Names[r];

                        bPersistent := (joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[ref].S['Set Is Persistent'] = '1');
                        if (bPersistent and (cellX = '0') and (cellY = '0') and (not bAddedPersistentWorldspaceCell)) then begin
                            cellRecordId := joWinningCells.O[wrldEdid].O['PersistentWorldspaceCell'].S['RecordID'];
                            if bFolipMaster then
                                rCell := GetHighestPossibleOverrideForFile(GetRecordFromFormIdFileId(cellRecordId), iCurrentPlugin)
                            else rCell := WinningOverrideIgnoringThisFile(GetRecordFromFormIdFileId(cellRecordId), sFolipMasterFileName);
                            wbCopyElementToFile(rCell, iCurrentPlugin, False, True);
                            bAddedPersistentWorldspaceCell := True;
                        end;

                        if bInterior then begin
                            cellRecordId := joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[ref].S['cellRecordId'];
                            if bFolipMaster then
                                rCell := GetHighestPossibleOverrideForFile(GetRecordFromFormIdFileId(cellRecordId), iCurrentPlugin)
                            else rCell := WinningOverrideIgnoringThisFile(GetRecordFromFormIdFileId(cellRecordId), sFolipMasterFileName);
                            wbCopyElementToFile(rCell, iCurrentPlugin, False, True);
                        end;

                        if bInterior then messageHere := 'Processing placed reference override: ' + ref + ' in interior ' + cellRecordId
                        else messageHere := 'Processing placed reference override: ' + ref + ' in worldspace ' + wrldEdid + ' cell [' + cellX + ', ' + cellY + ']';

                        AddMessage(messageHere);
                        ProcessOverrideReference(joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[ref], ref, pluginFileNameHere, bPersistent);
                    end;

                    //Process New References
                    for r := Pred(joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].Count) downto 0 do begin
                        ref := joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].Names[r];
                        AddMessage('Adding placed reference: ' + ref + ' in worldspace ' + wrldEdid + ' cell [' + cellX + ', ' + cellY + ']');
                        ProcessNewReference(joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[ref], pluginFileNameHere, nCell);
                    end;
                end;
            end;
        end;
    end;

end;

procedure ProcessMaterialSwap(const recordId: string);
{
    Adds the lod material swaps.
}
var
    e, n: IwbMainRecord;
    i: integer;
    tsMS: TStrings;
    original, replacement: string;
begin
    {joElements.O['MSWP'].O['Overrides'].O[recordId].S['File'] := GetFileName(iCurrentPlugin);
    joElements.O['MSWP'].O['Overrides'].O[recordId].A['AddMaterialSwap'].Add(slLODSubOriginal[n] + '|' + slLODSubReplacement[n]);}
    e := WinningOverride(GetRecordFromFormIdFileId(recordId));
    iCurrentPlugin := FileByName(joElements.O['MSWP'].O['Overrides'].O[recordId].S['File']);
    n := wbCopyElementToFile(e, iCurrentPlugin, False, True);
    for i := 0 to Pred(joElements.O['MSWP'].O['Overrides'].O[recordId].A['AddMaterialSwap'].Count) do begin
        tsMS := SplitString(joElements.O['MSWP'].O['Overrides'].O[recordId].A['AddMaterialSwap'].S[i], '|');
        original := tsMS[0];
        replacement := tsMS[1];
        AddMaterialSwap(n, original, replacement);
    end;
end;

procedure ProcessStatOverride(const recordId: string);
{
    Adds the STAT overrides.
}
var
    e, n: IwbMainRecord;
    i: integer;
begin
    {joElements.O['STAT'].O['Overrides'].O[recordId].S['File'] := GetFileName(iCurrentPlugin);
    joElements.O['STAT'].O['Overrides'].O[recordId].S['Has Distant LOD'] := joLOD.I['hasdistantlod'];
    joElements.O['STAT'].O['Overrides'].O[recordId].S['Level 0'] := joLOD.S['level0'];
    joElements.O['STAT'].O['Overrides'].O[recordId].S['Level 1'] := joLOD.S['level1'];
    joElements.O['STAT'].O['Overrides'].O[recordId].S['Level 2'] := joLOD.S['level2'];
    joElements.O['STAT'].O['Overrides'].O[recordId].S['Level 3'] := joLOD.S['level3'];}
    e := WinningOverride(GetRecordFromFormIdFileId(recordId));
    iCurrentPlugin := FileByName(joElements.O['STAT'].O['Overrides'].O[recordId].S['File']);
    n := wbCopyElementToFile(e, iCurrentPlugin, False, True);
    Add(n, 'MNAM', True);
    SetElementNativeValues(n, 'Record Header\Record Flags\Has Distant LOD', joElements.O['STAT'].O['Overrides'].O[recordId].S['Has Distant LOD']);
    if ElementExists(n, 'MNAM\Level 0') then begin
        SetElementNativeValues(n, 'MNAM\Level 0', joElements.O['STAT'].O['Overrides'].O[recordId].S['Level 0']);
        SetElementNativeValues(n, 'MNAM\Level 1', joElements.O['STAT'].O['Overrides'].O[recordId].S['Level 1']);
        SetElementNativeValues(n, 'MNAM\Level 2', joElements.O['STAT'].O['Overrides'].O[recordId].S['Level 2']);
        SetElementNativeValues(n, 'MNAM\Level 3', joElements.O['STAT'].O['Overrides'].O[recordId].S['Level 3']);
    end else begin
        SetElementNativeValues(n, 'MNAM\LOD #0 (Level 0)\Mesh', joElements.O['STAT'].O['Overrides'].O[recordId].S['Level 0']);
        SetElementNativeValues(n, 'MNAM\LOD #1 (Level 1)\Mesh', joElements.O['STAT'].O['Overrides'].O[recordId].S['Level 1']);
        SetElementNativeValues(n, 'MNAM\LOD #2 (Level 2)\Mesh', joElements.O['STAT'].O['Overrides'].O[recordId].S['Level 2']);
        SetElementNativeValues(n, 'MNAM\LOD #3 (Level 3)\Mesh', joElements.O['STAT'].O['Overrides'].O[recordId].S['Level 3']);
    end;
end;

procedure ProcessNewStat(const recordId, fileHere: string);
{
    Adds the STAT overrides.
}
var
    e, n: IwbMainRecord;
    i: integer;
    statGroup: IwbGroupRecord;
    mods: string;
begin
    {joElements.O['STAT'].O['New'].O[fakeStatic].S['Copy Object Bounds'] := True;
    joElements.O['STAT'].O['New'].O[fakeStatic].S['EDID'] := fakeStaticEditorId;
    joElements.O['STAT'].O['New'].O[fakeStatic].S['Is Marker'] := 1;
    joElements.O['STAT'].O['New'].O[fakeStatic].S['MODS'] := msFormid;
    joElements.O['STAT'].O['New'].O[recordId].S['Has Distant LOD'] := joLOD.I['hasdistantlod'];
    joElements.O['STAT'].O['New'].O[recordId].S['Level 0'] := joLOD.S['level0'];
    joElements.O['STAT'].O['New'].O[recordId].S['Level 1'] := joLOD.S['level1'];
    joElements.O['STAT'].O['New'].O[recordId].S['Level 2'] := joLOD.S['level2'];
    joElements.O['STAT'].O['New'].O[recordId].S['Level 3'] := joLOD.S['level3'];}

    e := WinningOverride(GetRecordFromFormIdFileId(recordId));
    iCurrentPlugin := FileByName(fileHere);
    statGroup := GroupBySignature(iCurrentPlugin, 'STAT');
    if not Assigned(statGroup) then statGroup := Add(iCurrentPlugin, 'STAT', True);
    n := Add(statGroup, 'STAT', True);
    CopyObjectBounds(e, n);
    SetEditorID(n, joElements.O['STAT'].O['New'].O[recordId].S['EDID']);
    SetElementNativeValues(n, 'Record Header\Record Flags\Is Marker', joElements.O['STAT'].O['New'].O[recordId].S['Is Marker']);

    mods := joElements.O['STAT'].O['New'].O[recordId].S['MODS'];
    if mods <> '' then begin
        Add(Add(n, 'Model', True), 'MODS', True);
        SetElementEditValues(n, 'Model\MODS', mods);
    end;
    Add(n, 'MNAM', True);
    SetElementNativeValues(n, 'Record Header\Record Flags\Has Distant LOD', joElements.O['STAT'].O['New'].O[recordId].S['Has Distant LOD']);
    if ElementExists(n, 'MNAM\Level 0') then begin
        SetElementNativeValues(n, 'MNAM\Level 0', joElements.O['STAT'].O['New'].O[recordId].S['Level 0']);
        SetElementNativeValues(n, 'MNAM\Level 1', joElements.O['STAT'].O['New'].O[recordId].S['Level 1']);
        SetElementNativeValues(n, 'MNAM\Level 2', joElements.O['STAT'].O['New'].O[recordId].S['Level 2']);
        SetElementNativeValues(n, 'MNAM\Level 3', joElements.O['STAT'].O['New'].O[recordId].S['Level 3']);
    end else begin
        SetElementNativeValues(n, 'MNAM\LOD #0 (Level 0)\Mesh', joElements.O['STAT'].O['New'].O[recordId].S['Level 0']);
        SetElementNativeValues(n, 'MNAM\LOD #1 (Level 1)\Mesh', joElements.O['STAT'].O['New'].O[recordId].S['Level 1']);
        SetElementNativeValues(n, 'MNAM\LOD #2 (Level 2)\Mesh', joElements.O['STAT'].O['New'].O[recordId].S['Level 2']);
        SetElementNativeValues(n, 'MNAM\LOD #3 (Level 3)\Mesh', joElements.O['STAT'].O['New'].O[recordId].S['Level 3']);
    end;
    joElements.O['STAT'].O['New'].O[recordId].O[fileHere].S['fakeStaticFormId'] := IntToHex(GetLoadOrderFormID(n), 8);
end;

procedure ProcessOverrideReference(placedReferenceOverride: TJsonObject; const ref, fileHere: string; const bPersistent: boolean);
{
    Process a placed reference override.
}
var
    cellRecordId, wrldRecordId, baseFormId, linkedRefRemove, linkedRefAdd, keyword, linkedRef: string;
    bXesp: boolean;
    i, flst: integer;
    rWrld, rCell, rOriginal, rOverride: IwbMainRecord;
    xesp: IwbElement;
    tsLinkedRef: TStrings;
begin
    {
    placedReferenceOverride.S['Set Is Persistent'] := 1;
    placedReferenceOverride.S['Visible When Distant'] := 1;
    placedReferenceOverride.S['Is Full LOD'] := 0;
    placedReferenceOverride.S['LOD Respects Enable State'] := 1;
    placedReferenceOverride.S['NAME'] := '000e4610'; //Enable Marker

    placedReferenceOverride.S['RemoveLinkedReference'] := '00195411';
    placedReferenceOverride.S['AddLinkedReference'] := '00195411|' + MultiRefLODFormidStr;

    placedReferenceOverride.S['XESP'] := 1;
    placedReferenceOverride.S['XESP Reference'] := parentFormid;
    placedReferenceOverride.S['Opposite Enable Parent'] := 1;

    placedReferenceOverride.A['AddRefToMyFormlist'].Add(tlFlst.IndexOf(flDecals));
    }

    rOriginal := WinningOverride(GetRecordFromFormIdFileId(ref));
    if IsInIgnoredPlugin(rOriginal) then Exit;
    rOverride := CopyElementToFileWithVC(rOriginal, iCurrentPlugin);
    SetIsPersistent(rOverride, bPersistent);
    SetIsVisibleWhenDistant(rOverride, (placedReferenceOverride.S['Visible When Distant'] = '1'));
    SetElementNativeValues(rOverride, 'Record Header\Record Flags\Is Full LOD', (placedReferenceOverride.S['Is Full LOD'] = '1'));
    SetElementNativeValues(rOverride, 'Record Header\Record Flags\LOD Respects Enable State', (placedReferenceOverride.S['LOD Respects Enable State'] = '1'));
    baseFormId := placedReferenceOverride.S['NAME'];
    if baseFormId <> '' then begin
        SetElementEditValues(rOverride, 'NAME', baseFormId);
    end;

    linkedRefRemove := placedReferenceOverride.S['RemoveLinkedReference'];
    if linkedRefRemove <> '' then RemoveLinkedReferenceByKeyword(rOverride, linkedRefRemove);
    linkedRefAdd := placedReferenceOverride.S['AddLinkedReference'];
    if linkedRefAdd <> '' then begin
        tsLinkedRef := SplitString(linkedRefAdd, '|');
        keyword := tsLinkedRef[0];
        linkedRef := tsLinkedRef[1];
        AddLinkedReference(rOverride, keyword, linkedRef);
    end;

    bXesp := (placedReferenceOverride.S['XESP'] = '1');
    if bXesp then begin
        xesp := ElementByPath(rOverride, 'XESP');
        if not Assigned(xesp) then begin
            xesp := Add(rOverride, 'XESP', True);
            ElementAssign(xesp, 0, nil, False);
        end;
        SetElementEditValues(xesp, 'Reference', placedReferenceOverride.S['XESP Reference']);
        SetElementNativeValues(xesp, 'Flags\Set Enable State to Opposite of Parent', (placedReferenceOverride.S['Opposite Enable Parent'] = '1'));
    end;

    for i := 0 to Pred(placedReferenceOverride.A['AddRefToMyFormlist'].Count) do begin
        flst := StrToInt(placedReferenceOverride.A['AddRefToMyFormlist'].S[i]);
        if SameText(fileHere, sFolipMasterFileName) then flst := flst + 7;
        AddRefToMyFormlist(rOverride, ObjectToElement(tlFlst[flst]));
    end;
end;

procedure ProcessNewReference(placedReference: TJsonObject; fileHere: string; nCell: IwbMainRecord);
{
    Add a placed reference.
}
var
    cellRecordId, wrldRecordId, fakeStatic, fakeStaticFormId, scale, ms: string;
    bPersistent, bXesp: boolean;
    i, flst: integer;
    rWrld, rCell, rNew: IwbMainRecord;
    xesp: IwbElement;
begin
    {
    placedReference.S['File'] := GetFileName(iCurrentPlugin);
    placedReference.S['Visible When Distant'] := 1;
    placedReference.S['Initially Disabled'] := 1;

    placedReference.S['fakeStatic'] := fakeStatic;

    placedReference.S['XESP'] := 1;
    placedReference.S['XESP Reference'] := parentFormid;
    placedReference.S['Opposite Enable Parent'] := 1;

    placedReference.A['AddRefToMyFormlist'].Add(tlFlst.IndexOf(flDecals));

    placedReference.O['pos'].S['x'] := GetElementNativeValues(r, 'DATA\Position\X');
    placedReference.O['pos'].S['y'] := GetElementNativeValues(r, 'DATA\Position\Y');
    placedReference.O['pos'].S['z'] := GetElementNativeValues(r, 'DATA\Position\Z');
    placedReference.O['rot'].S['x'] := GetElementNativeValues(r, 'DATA\Rotation\X');
    placedReference.O['rot'].S['y'] := GetElementNativeValues(r, 'DATA\Rotation\Y');
    placedReference.O['rot'].S['z'] := GetElementNativeValues(r, 'DATA\Rotation\Z');
    placedReference.S['XSCL - Scale'] := GetElementNativeValues(r, 'XSCL - Scale');

    placedReference.S['XMSP - Material Swap'] := IntToHex(GetLoadOrderFormID(ms), 8);

    }

    rNew := Add(nCell, 'REFR', True);
    SetIsVisibleWhenDistant(rNew, (placedReference.S['Visible When Distant'] = '1'));
    SetIsInitiallyDisabled(rNew, (placedReference.S['Initially Disabled'] = '1'));

    fakeStatic := placedReference.S['fakeStatic'];
    if fakeStatic <> '' then begin
        fakeStaticFormId := joElements.O['STAT'].O['New'].O[fakeStatic].O[fileHere].S['fakeStaticFormId'];
        SetElementEditValues(rNew, 'NAME', fakeStaticFormId);
    end;

    bXesp := (placedReference.S['XESP'] = '1');
    if bXesp then begin
        xesp := ElementByPath(rNew, 'XESP');
        if not Assigned(xesp) then begin
            xesp := Add(rNew, 'XESP', True);
            ElementAssign(xesp, 0, nil, False);
        end;
        SetElementEditValues(xesp, 'Reference', placedReference.S['XESP Reference']);
        SetElementNativeValues(xesp, 'Flags\Set Enable State to Opposite of Parent', (placedReference.S['Opposite Enable Parent'] = '1'));
    end;

    SetElementNativeValues(rNew, 'DATA\Position\X', placedReference.O['pos'].S['x']);
    SetElementNativeValues(rNew, 'DATA\Position\Y', placedReference.O['pos'].S['y']);
    SetElementNativeValues(rNew, 'DATA\Position\Z', placedReference.O['pos'].S['z']);
    SetElementNativeValues(rNew, 'DATA\Rotation\X', placedReference.O['rot'].S['x']);
    SetElementNativeValues(rNew, 'DATA\Rotation\Y', placedReference.O['rot'].S['y']);
    SetElementNativeValues(rNew, 'DATA\Rotation\Z', placedReference.O['rot'].S['z']);
    scale := placedReference.S['XSCL - Scale'];
    if scale <> '' then begin
        Add(rNew, 'XSCL', True);
        SetElementNativeValues(rNew, 'XSCL - Scale', scale);
    end;

    ms := placedReference.S['XMSP - Material Swap'];
    if ms <> '' then begin
        Add(rNew, 'XMSP', True);
        SetElementEditValues(rNew, 'XMSP - Material Swap', ms);
    end;

    for i := 0 to Pred(placedReference.A['AddRefToMyFormlist'].Count) do begin
        flst := StrToInt(placedReference.A['AddRefToMyFormlist'].S[i]);
        if SameText(fileHere, sFolipMasterFileName) then flst := flst + 7;
        AddRefToMyFormlist(rNew, ObjectToElement(tlFlst[flst]));
    end;
end;

procedure ProcessEnableParents;
{
    Apply LOD Respects Enable State flag to XESP - Enable Parent references.
}
var
    i, pi, oi, idx: integer;

    p, m, r, n, base, rCell, rWrld, oppositeEnableParentReplacer, oreplacer, enableParentReplacer, ereplacer, xesp: IwbElement;

    parentFormid, wrldEdid, cellX, cellY, recordId, oreplacerFormid, ereplacerFormid, OverOrNew, pluginFileNameOriginal: string;

    bCanBeRespected, bHasOppositeEnableParent, bHasSuitableReplacer, bHasPersistentReplacer, bIsPersistent,
    bHasOppositeEnableRefs, bHasEnableRefs, bBaseHasLOD, bIsFullLOD: boolean;

    c: TwbGridCell;

    tlOppositeEnableRefs, tlEnableRefs: TList;
begin
    for i := Pred(tlEnableParents.Count) downto 0 do begin
        // This iterates over the actual Parents
        p := ObjectToElement(tlEnableParents[i]);
        bCanBeRespected := False;
        bHasSuitableReplacer := False;
        bHasPersistentReplacer := False;
        bHasOppositeEnableRefs := False;

        parentFormid := IntToHex(GetLoadOrderFormID(p), 8);
        if parentFormid = '00000014' then continue; //skip player
        if Pos(RecordFormIdFileId(p), sEnableParentFormidExclusions) <> 0 then continue; //skip excluded parents

        AddMessage('Respect Enable Parents: Processing ' + Name(p));

        //Parents can only be reliably respected if they are from the first master file (Fallout4.esm) and are not initially disabled (not always a problem but sometimes is).
        if ((LeftStr(IntToHex(GetLoadOrderFormID(p), 8), 2) = '00') and (not GetIsInitiallyDisabled(p))) then bCanBeRespected := True;

        if bCanBeRespected and (GetElementEditValues(p, 'Record Header\Record Flags\LOD Respects Enable State') <> '1') then begin
            //If the parent can be respected but is not respected, set the LOD Respects Enable State flag on the parenet.
            rCell := LinksTo(ElementByIndex(p, 0));
            cellX := GetElementEditValues(rCell, 'XCLC\X');
            cellY := GetElementEditValues(rCell, 'XCLC\Y');
            rWrld := LinksTo(ElementByIndex(rCell, 0));
            wrldEdid := GetElementEditValues(rWrld, 'EDID');

            iCurrentPlugin := CanOverrideDeterminesPlugin(p, iFolipMasterFile);
            iCurrentPlugin := RefMastersDeterminePlugin(p, iCurrentPlugin);
            iCurrentPlugin := RefMastersDeterminePlugin(GetHighestPossibleOverrideForFile(rWrld, iCurrentPlugin), iCurrentPlugin);
            iCurrentPlugin := RefMastersDeterminePlugin(GetHighestPossibleOverrideForFile(rCell, iCurrentPlugin), iCurrentPlugin);

            recordId := RecordFormIdFileId(p);
            pluginFileNameHere := GetFileName(iCurrentPlugin);
            joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['LOD Respects Enable State'] := 1;
            joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['Set Is Persistent'] := 1;

        end;

        {
            Iterate over all references to the parent. Two goals:
            * Find a suitable opposite enable parent replacer
            * Find all LOD references using opposite enable parent flag and store to a TList
        }
        tlOppositeEnableRefs := TList.Create;
        tlEnableRefs := TList.Create;
        for pi := Pred(ReferencedByCount(p)) downto 0 do begin
            r := ReferencedByIndex(p, pi);
            bBaseHasLOD := False;

            if Signature(r) <> 'REFR' then continue;
            if not IsWinningOverride(r) then continue;
            if GetIsDeleted(r) then continue;
            if GetIsCleanDeleted(r) then continue;

            base := WinningOverride(LinksTo(ElementByPath(r, 'NAME')));
            if (slHasLOD.IndexOf(RecordFormIdFileId(base)) <> -1) then bBaseHasLOD := True;

            //Split between refs with LOD and not having LOD
            if not bBaseHasLOD then begin
                // If the base does not have LOD, we will consider it for a suitable opposite enable parent replacer.
                if bHasPersistentReplacer then continue; //Already have a persistent replacer, no need to look for more.
                if Pos(Signature(base), 'STAT,ACTI,TXST') = 0 then continue; //Only consider static, activator, and texture set for opposite enable parent replacer.
                if LeftStr(IntToHex(GetLoadOrderFormID(r), 8), 2) <> '00' then continue; //Only consider references from the first master file (Fallout4.esm) for opposite enable parent replacer.
                if (GetElementNativeValues(r, 'XESP\Flags\Set Enable State to Opposite of Parent') <> 0)
                then bHasOppositeEnableParent := True else bHasOppositeEnableParent := False;
                if not bHasOppositeEnableParent then continue; //Only consider references that are opposite enable state relative to the parent.
                if GetIsInitiallyDisabled(r) then continue; //skip refs that are initially disabled as sometimes this doesn't seem to work.
                bIsPersistent := GetIsPersistent(r);
                if bHasSuitableReplacer and not bIsPersistent then continue; //If we already have a suitable replacer and this one is not persistent, skip it.
                oppositeEnableParentReplacer := r; //if we made it this far, this is a suitable opposite enable parent replacer.
                bHasSuitableReplacer := True;
                if not bIsPersistent then continue;
                bHasPersistentReplacer := True; //if we made it this far, this is a suitable opposite enable parent replacer and it is persistent, so it is a perfect candidate, and we won't look for another.
                continue;
            end;

            if (GetElementNativeValues(r, 'XESP\Flags\Set Enable State to Opposite of Parent') <> 0)
            then bHasOppositeEnableParent := True else bHasOppositeEnableParent := False;

            if not bHasOppositeEnableParent then begin
                tlEnableRefs.Add(r);
                bHasEnableRefs := True;
                continue;
            end;
            tlOppositeEnableRefs.Add(r);
            bHasOppositeEnableRefs := True;

            // Check to see if we need to find a suitable opposite enable parent replacer.
            if bHasPersistentReplacer then continue; //Already have a persistent replacer, no need to look for more.
            if LeftStr(IntToHex(GetLoadOrderFormID(r), 8), 2) <> '00' then continue; //Only consider references from the first master file (Fallout4.esm) for opposite enable parent replacer.
            bIsPersistent := GetIsPersistent(r);
            if bHasSuitableReplacer and not bIsPersistent then continue; //If we already have a suitable replacer and this one is not persistent, skip it.
            if GetIsInitiallyDisabled(r) then continue; //skip refs that are initially disabled as sometimes this doesn't seem to work.
            oppositeEnableParentReplacer := r; //if we made it this far, this is a suitable opposite enable parent replacer.
            bHasSuitableReplacer := True;
            if not bIsPersistent then continue;
            bHasPersistentReplacer := True; //if we made it this far, this is a suitable opposite enable parent replacer and it is persistent, so it is a perfect candidate, and we won't look for another.
        end;

        //OK, we have iterated over all references to the parent. Now we will process the opposite enable parent references and plain enable parent references separately.

        if bHasOppositeEnableRefs then begin
            if not bHasSuitableReplacer then oppositeEnableParentReplacer := GetSuitableReplacement; //If we have not found a suitable opposite enable parent replacer, we will rob one.

            // Create opposite enable parent replacer
            rCell := LinksTo(ElementByIndex(oppositeEnableParentReplacer, 0));
            cellX := GetElementEditValues(rCell, 'XCLC\X');
            cellY := GetElementEditValues(rCell, 'XCLC\Y');

            rWrld := LinksTo(ElementByIndex(rCell, 0));
            wrldEdid := GetElementEditValues(rWrld, 'EDID');

            iCurrentPlugin := CanOverrideDeterminesPlugin(oppositeEnableParentReplacer, iFolipMasterFile);
            iCurrentPlugin := RefMastersDeterminePlugin(p, iCurrentPlugin);
            iCurrentPlugin := RefMastersDeterminePlugin(oppositeEnableParentReplacer, iCurrentPlugin);
            iCurrentPlugin := RefMastersDeterminePlugin(GetHighestPossibleOverrideForFile(rWrld, iCurrentPlugin), iCurrentPlugin);
            iCurrentPlugin := RefMastersDeterminePlugin(GetHighestPossibleOverrideForFile(rCell, iCurrentPlugin), iCurrentPlugin);


            recordId := RecordFormIdFileId(oppositeEnableParentReplacer);
            pluginFileNameHere := GetFileName(iCurrentPlugin);
            joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['cellRecordId'] := RecordFormIdFileId(rCell);
            joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['LOD Respects Enable State'] := 1;
            joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['Set Is Persistent'] := 1;

            joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].A['AddRefToMyFormlist'].Add(tlFlst.IndexOf(flParents));

            if not bHasSuitableReplacer then begin
                // Only for stolen forms
                joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['NAME'] := '000e4610'; //Enable Marker
                joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['XESP'] := 1;
                joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['XESP Reference'] := parentFormid;
                joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['Opposite Enable Parent'] := 1;
                joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].A['AddRefToMyFormlist'].Add(tlFlst.IndexOf(flDecals));
            end;
            oreplacerFormid := IntToHex(GetLoadOrderFormID(oppositeEnableParentReplacer), 8);

            for oi := Pred(tlOppositeEnableRefs.Count) downto 0 do begin
                r := ObjectToElement(tlOppositeEnableRefs[oi]);

                OverOrNew := 'Overrides';
                if (slFakeStatics.IndexOf(RecordFormIdFileId(LinksTo(ElementByPath(r, 'NAME')))) <> -1) then OverOrNew := 'New';

                rCell := LinksTo(ElementByIndex(r, 0));
                if IsInteriorCell(rCell) then continue;
                rWrld := WinningOverride(LinksTo(ElementByIndex(rCell, 0)));
                if IsWorldIgnored(rWrld) then continue;
                if WorldInheritsLOD(rWrld) then continue;


                cellX := GetElementEditValues(rCell, 'XCLC\X');
                cellY := GetElementEditValues(rCell, 'XCLC\Y');
                if (SameText(OverOrNew, 'New') and GetIsPersistent(rCell)) then begin
                    //If New, then it isn't persistent (fake statics), so we need to send it to the correct cell
                    c := wbPositionToGridCell(GetPosition(r));
                    cellX := IntToStr(c.x);
                    cellY := IntToStr(c.y);
                end;
                wrldEdid := GetElementEditValues(rWrld, 'EDID');

                AddMessage(#9 + Name(r));
                recordId := RecordFormIdFileId(r);

                if SameText(OverOrNew, 'New') then begin
                    if joElements.O['references'].O[sFolipMasterFileName].O[wrldEdid].O[cellX].O[cellY].O[OverOrNew].Contains(recordId)
                    then begin
                        iCurrentPlugin := iFolipMasterFile;
                        pluginFileNameOriginal := sFolipMasterFileName;
                    end else begin
                        iCurrentPlugin := iFolipPluginFile;
                        pluginFileNameOriginal := sFolipPluginFileName + '.esp';
                    end;
                end else iCurrentPlugin := CanOverrideDeterminesPlugin(r, iFolipMasterFile);

                iCurrentPlugin := RefMastersDeterminePlugin(p, iCurrentPlugin);
                iCurrentPlugin := RefMastersDeterminePlugin(r, iCurrentPlugin);
                iCurrentPlugin := RefMastersDeterminePlugin(GetHighestPossibleOverrideForFile(rWrld, iCurrentPlugin), iCurrentPlugin);
                iCurrentPlugin := RefMastersDeterminePlugin(GetHighestPossibleOverrideForFile(rCell, iCurrentPlugin), iCurrentPlugin);


                pluginFileNameHere := GetFileName(iCurrentPlugin);
                if ((not SameText(pluginFileNameOriginal, pluginFileNameHere)) and SameText(OverOrNew, 'New')) then begin
                    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O[OverOrNew].O[recordId].Assign(
                        joElements.O['references'].O[pluginFileNameOriginal].O[wrldEdid].O[cellX].O[cellY].O[OverOrNew].O[recordId]);
                    idx := joElements.O['references'].O[pluginFileNameOriginal].O[wrldEdid].O[cellX].O[cellY].O[OverOrNew].IndexOf(key);
                    joElements.O['references'].O[pluginFileNameOriginal].O[wrldEdid].O[cellX].O[cellY].O[OverOrNew].Delete(idx);
                end;
                joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O[OverOrNew].O[recordId].S['XESP'] := 1;
                joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O[OverOrNew].O[recordId].S['XESP Reference'] := oreplacerFormid;
                joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O[OverOrNew].O[recordId].S['Opposite Enable Parent'] := 0;
                joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O[OverOrNew].O[recordId].S['Visible When Distant'] := 1;
                joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O[OverOrNew].O[recordId].S['Is Full LOD'] := 0;
                if GetIsPersistent(r) then joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O[OverOrNew].O[recordId].S['Set Is Persistent'] := 1;
                if SameText(OverOrNew, 'Overrides') then begin
                    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O[OverOrNew].O[recordId].A['AddRefToMyFormlist'].Add(tlFlst.IndexOf(flOverrides));
                end;
                if bIsFullLOD then joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O[OverOrNew].O[recordId].A['AddRefToMyFormlist'].Add(tlFlst.IndexOf(flRemoveIsFullLOD));

            end;
        end;
        if bHasEnableRefs then begin
            if not bCanBeRespected then begin
                enableParentReplacer := GetSuitableReplacement; //We will rob a suitable replacer if the parent cannot be respected.

                recordId := RecordFormIdFileId(enableParentReplacer);
                rCell := LinksTo(ElementByIndex(enableParentReplacer, 0));
                rWrld := LinksTo(ElementByIndex(rCell, 0));
                cellX := GetElementEditValues(rCell, 'XCLC\X');
                cellY := GetElementEditValues(rCell, 'XCLC\Y');
                wrldEdid := GetElementEditValues(rWrld, 'EDID');

                iCurrentPlugin := CanOverrideDeterminesPlugin(enableParentReplacer, iFolipMasterFile);
                iCurrentPlugin := RefMastersDeterminePlugin(p, iCurrentPlugin);
                iCurrentPlugin := RefMastersDeterminePlugin(enableParentReplacer, iCurrentPlugin);
                iCurrentPlugin := RefMastersDeterminePlugin(GetHighestPossibleOverrideForFile(rWrld, iCurrentPlugin), iCurrentPlugin);
                iCurrentPlugin := RefMastersDeterminePlugin(GetHighestPossibleOverrideForFile(rCell, iCurrentPlugin), iCurrentPlugin);


                pluginFileNameHere := GetFileName(iCurrentPlugin);
                joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['cellRecordId'] := RecordFormIdFileId(rCell);
                joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['XESP'] := 1;
                joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['XESP Reference'] := parentFormid;
                joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['NAME'] := '000e4610'; //enable marker
                joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['LOD Respects Enable State'] := 1;
                if GetIsPersistent(enableParentReplacer) then joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['Set Is Persistent'] := 1;

                joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].A['AddRefToMyFormlist'].Add(tlFlst.IndexOf(flParents));
                joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].A['AddRefToMyFormlist'].Add(tlFlst.IndexOf(flDecals));

                ereplacerFormid := IntToHex(GetLoadOrderFormID(enableParentReplacer), 8);
            end;

            for oi := Pred(tlEnableRefs.Count) downto 0 do begin
                r := ObjectToElement(tlEnableRefs[oi]);
                if (GetElementNativeValues(r, 'Record Header\Record Flags\Is Full LOD') <> 0)
                then bIsFullLOD := true else bIsFullLOD := false;

                OverOrNew := 'Overrides';
                if (slFakeStatics.IndexOf(RecordFormIdFileId(LinksTo(ElementByPath(r, 'NAME')))) <> -1) then OverOrNew := 'New';


                if (not bCanBeRespected) or (not GetIsVisibleWhenDistant(r)) or bIsFullLOD then begin
                    AddMessage(#9 + Name(r));
                    rCell := LinksTo(ElementByIndex(r, 0));
                    // Skip if in interior cell
                    if IsInteriorCell(rCell) then continue;
                    rWrld := WinningOverride(LinksTo(ElementByIndex(rCell, 0)));
                    // Check for ignored worldspace
                    if IsWorldIgnored(rWrld) then continue;
                    // Check if WRLD inherits LOD from another worldspace
                    if WorldInheritsLOD(rWrld) then continue;

                    cellX := GetElementEditValues(rCell, 'XCLC\X');
                    cellY := GetElementEditValues(rCell, 'XCLC\Y');

                    if (SameText(OverOrNew, 'New') and GetIsPersistent(rCell)) then begin
                        //If New, then it isn't persistent (fake statics), so we need to send it to the correct cell
                        c := wbPositionToGridCell(GetPosition(r));
                        cellX := IntToStr(c.x);
                        cellY := IntToStr(c.y);
                    end;

                    wrldEdid := GetElementEditValues(rWrld, 'EDID');
                    recordId := RecordFormIdFileId(r);

                    if SameText(OverOrNew, 'New') then begin
                        if joElements.O['references'].O[sFolipMasterFileName].O[wrldEdid].O[cellX].O[cellY].O[OverOrNew].Contains(recordId)
                        then begin
                            iCurrentPlugin := iFolipMasterFile;
                            pluginFileNameOriginal := sFolipMasterFileName;
                        end else begin
                            iCurrentPlugin := iFolipPluginFile;
                            pluginFileNameOriginal := sFolipPluginFileName + '.esp';
                        end;
                    end else iCurrentPlugin := CanOverrideDeterminesPlugin(r, iFolipMasterFile);

                    iCurrentPlugin := RefMastersDeterminePlugin(p, iCurrentPlugin);
                    iCurrentPlugin := RefMastersDeterminePlugin(r, iCurrentPlugin);
                    iCurrentPlugin := RefMastersDeterminePlugin(GetHighestPossibleOverrideForFile(rWrld, iCurrentPlugin), iCurrentPlugin);
                    iCurrentPlugin := RefMastersDeterminePlugin(GetHighestPossibleOverrideForFile(rCell, iCurrentPlugin), iCurrentPlugin);

                    pluginFileNameHere := GetFileName(iCurrentPlugin);
                    if ((not SameText(pluginFileNameOriginal, pluginFileNameHere)) and SameText(OverOrNew, 'New')) then begin
                        joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O[OverOrNew].O[recordId].Assign(
                            joElements.O['references'].O[pluginFileNameOriginal].O[wrldEdid].O[cellX].O[cellY].O[OverOrNew].O[recordId]);
                        idx := joElements.O['references'].O[pluginFileNameOriginal].O[wrldEdid].O[cellX].O[cellY].O[OverOrNew].IndexOf(key);
                        joElements.O['references'].O[pluginFileNameOriginal].O[wrldEdid].O[cellX].O[cellY].O[OverOrNew].Delete(idx);
                    end;
                    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O[OverOrNew].O[recordId].S['XESP'] := 1;
                    if bCanBeRespected then ereplacerFormid := parentFormid;
                    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O[OverOrNew].O[recordId].S['XESP Reference'] := ereplacerFormid;
                    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O[OverOrNew].O[recordId].S['Opposite Enable Parent'] := 0;
                    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O[OverOrNew].O[recordId].S['Visible When Distant'] := 1;
                    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O[OverOrNew].O[recordId].S['Is Full LOD'] := 0;
                    if GetIsPersistent(r) then joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O[OverOrNew].O[recordId].S['Set Is Persistent'] := 1;

                    if SameText(OverOrNew, 'Overrides') then begin
                        joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O[OverOrNew].O[recordId].A['AddRefToMyFormlist'].Add(tlFlst.IndexOf(flOverrides));
                    end;
                    if bIsFullLOD then joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O[OverOrNew].O[recordId].A['AddRefToMyFormlist'].Add(tlFlst.IndexOf(flRemoveIsFullLOD));
                end;
            end;
        end;

        tlOppositeEnableRefs.Free;
        tlEnableRefs.Free;
    end;
end;

procedure MultiRefLOD;
{
    Adds MultiRefLOD keyword to references specified in FOLIP MultiRefLOD.json rules.
}
var
    c, a, i: integer;
    MultiRefLODReference, ref, MultiRefLODFormidStr, editorid, cellX, cellY, wrldEdid, recordId: string;
    r, MultiRefLODElement, n, rCell, rWrld: IwbElement;
    linkedrefs, lref: IInterface;
    bNeedsModified, bHadMultiRefLODCorrect: Boolean;
begin
    for c := 0 to Pred(joMultiRefLOD.Count) do begin
        editorid := joMultiRefLOD.Names[c];
        MultiRefLODReference := joMultiRefLOD.O[editorid].S['MultiRefLOD'];
        MultiRefLODElement := GetRecordFromFormIdFileId(MultiRefLODReference);
        MultiRefLODFormidStr := IntToHex(GetLoadOrderFormID(MultiRefLODElement), 8);
        if not Assigned(MultiRefLODElement) then begin
            AddMessage('MultiRefLOD: Could not find record for ' + MultiRefLODReference);
            Continue;
        end;

        AddMessage('MultiRefLOD: Processing ' + ShortName(MultiRefLODElement));

        for a := 0 to Pred(joMultiRefLOD.O[editorid].A['References to add MultiRefLOD'].Count) do begin
            ref := joMultiRefLOD.O[editorid].A['References to add MultiRefLOD'].S[a];
            r := WinningOverride(GetRecordFromFormIdFileId(ref));
            if not Assigned(r) then begin
                AddMessage('MultiRefLOD: Could not find reference ' + ref + ' for ' + MultiRefLODReference);
                Continue;
            end;

            //Check to see if we need to modify the ref.
            bNeedsModified := False;
            if GetIsDeleted(r) then begin
                AddMessage('MultiRefLOD: Skipping deleted reference ' + Name(r));
                Continue;
            end;
            if GetIsCleanDeleted(r) then begin
                AddMessage('MultiRefLOD: Skipping clean deleted reference ' + Name(r));
                Continue;
            end;

            if ElementExists(r, 'Linked References') then begin
                bHadMultiRefLODCorrect := False;
                linkedrefs := ElementByPath(r, 'Linked References');
                for i := 0 to Pred(ElementCount(linkedrefs)) do begin
                    lref := ElementByIndex(linkedrefs, i);
                    if IntToHex(GetLoadOrderFormID(LinksTo(ElementByPath(lref, 'Keyword/Ref'))), 8) = '00195411' then begin
                        // if the reference already has the MultiRefLOD keyword, but the formid does not match, we need to modify it.
                        if IntToHex(GetLoadOrderFormID(LinksTo(ElementByPath(lref, 'Ref'))), 8) <> MultiRefLODFormidStr then begin
                            bNeedsModified := True;
                            Break; // We can break since we know we need to modify this reference in order to remove this one.
                        end
                        else if (IntToHex(GetLoadOrderFormID(LinksTo(ElementByPath(lref, 'Ref'))), 8) = MultiRefLODFormidStr) then bHadMultiRefLODCorrect := True;
                    end;
                end;
                if not bHadMultiRefLODCorrect then bNeedsModified := True; // If we didn't find the MultiRefLOD, we need to add it.
            end
            else bNeedsModified := True;

            if bNeedsModified then begin
                AddMessage(#9 + Name(r));

                rCell := LinksTo(ElementByIndex(r, 0));
                rWrld := LinksTo(ElementByIndex(rCell, 0));
                cellX := GetElementEditValues(rCell, 'XCLC\X');
                cellY := GetElementEditValues(rCell, 'XCLC\Y');
                wrldEdid := GetElementEditValues(rWrld, 'EDID');
                recordId := RecordFormIdFileId(r);

                iCurrentPlugin := CanOverrideDeterminesPlugin(r, iFolipMasterFile);
                iCurrentPlugin := RefMastersDeterminePlugin(r, iCurrentPlugin);
                iCurrentPlugin := RefMastersDeterminePlugin(MultiRefLODElement, iCurrentPlugin);
                iCurrentPlugin := RefMastersDeterminePlugin(GetHighestPossibleOverrideForFile(rWrld, iCurrentPlugin), iCurrentPlugin);
                iCurrentPlugin := RefMastersDeterminePlugin(GetHighestPossibleOverrideForFile(rCell, iCurrentPlugin), iCurrentPlugin);


                pluginFileNameHere := GetFileName(iCurrentPlugin);
                joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['RemoveLinkedReference'] := '00195411';
                joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['AddLinkedReference'] := '00195411|' + MultiRefLODFormidStr;
                if GetIsPersistent(r) then joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['Set Is Persistent'] := 1;

                joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].A['AddRefToMyFormlist'].Add(tlFlst.IndexOf(flMultiRefLOD));
            end;
        end;
    end;
end;

function AddLinkedReference(e: IInterface; keyword, ref: String): Integer;
{
  Add a linked reference.
}
var
    el, linkedrefs, lref: IInterface;
    i: Integer;
begin
    Result := 0;
    if not ElementExists(e, 'Linked References') then begin
        linkedrefs := Add(e, 'Linked References', True);
        lref := ElementByIndex(linkedrefs, 0);
        SetElementEditValues(lref, 'Keyword/Ref', keyword);
        SetElementEditValues(lref, 'Ref', ref);
    end
    else begin
        linkedrefs := ElementByPath(e, 'Linked References');
        lref := ElementAssign(linkedrefs, HighInteger, nil, False);
        SetElementEditValues(lref, 'Keyword/Ref', keyword);
        SetElementEditValues(lref, 'Ref', ref);
    end;
end;

function RemoveLinkedReferenceByKeyword(e: IwbElement; keyword: String): Integer;
{
  Remove a linked reference by keyword. Returns 1 if a reference was removed, 0 if not.
}
var
    linkedrefs, lref: IInterface;
    i: Integer;
begin
    Result := 0;
    if not ElementExists(e, 'Linked References') then Exit;
    linkedrefs := ElementByPath(e, 'Linked References');
    for i := Pred(ElementCount(linkedrefs)) downto 0 do begin
        lref := ElementByIndex(linkedrefs, i);
        if IntToHex(GetLoadOrderFormID(LinksTo(ElementByPath(lref, 'Keyword/Ref'))), 8) = keyword then begin
            Remove(lref);
            Result := 1; // Indicate that a reference was removed
        end;
    end;
end;

function GetSuitableReplacement: IwbElement;
var
    stolen, r, m, rCell: IwbElement;
    i, j: integer;
begin
    Result := nil;
    for j := 0 to Pred(tlTxst.Count) do begin
        stolen := ObjectToElement(tlTxst[j]);
        for i := Pred(ReferencedByCount(stolen)) downto 0 do begin
            r := ReferencedByIndex(stolen, i);
            if tlStolenForms.IndexOf(r) <> -1 then continue;
            if LeftStr(IntToHex(GetLoadOrderFormID(r), 8), 2) <> '00' then continue;
            if Signature(r) <> 'REFR' then continue;
            if not IsWinningOverride(r) then continue;
            m := MasterOrSelf(r);
            if not Equals(m, r) then continue;
            if not GetIsPersistent(r) then continue;
            rCell := LinksTo(ElementByIndex(r, 0));
            if IsInteriorCell(rCell) then continue;
            if ReferencedByCount(r) <> 0 then continue;
            if ElementExists(r, 'VMAD') then continue;
            if ElementExists(r, 'XMPO') then continue;
            if ElementExists(r, 'XPOD') then continue;
            if ElementExists(r, 'XPTL') then continue;
            if ElementExists(r, 'XORD') then continue;
            if ElementExists(r, 'XMBP') then continue;
            if ElementExists(r, 'XRGD') then continue;
            if ElementExists(r, 'XTEL') then continue;
            if ElementExists(r, 'XTNM') then continue;
            if ElementExists(r, 'XMBR') then continue;
            if ElementExists(r, 'XMCN') then continue;
            if ElementExists(r, 'XMCU') then continue;
            if ElementExists(r, 'XASP') then continue;
            if ElementExists(r, 'XATP') then continue;
            if ElementExists(r, 'XLKT') then continue;
            if ElementExists(r, 'XPDD') then continue;
            if ElementExists(r, 'XPRM') then continue;
            if ElementExists(r, 'XRFG') then continue;
            if ElementExists(r, 'XRDO') then continue;
            if ElementExists(r, 'XBSD') then continue;
            if ElementExists(r, 'XESP') then continue;
            tlStolenForms.Add(r);
            Result := r;
            Exit;
        end;
    end;
end;

procedure AddRefToMyFormlist(const rec: IwbMainRecord; var frmlstHere: IwbMainRecord);
var
    formids, lnam: IwbElement;
    rFormid: string;
begin
    if not Assigned(rec) then begin
        AddMessage('Error: Attempted to add a null reference to a formlist.');
        Exit;
    end;
    if not ElementExists(frmlstHere, 'FormIDs') then begin
        formids := Add(frmlstHere, 'FormIDs', True);
        lnam := ElementByIndex(formids, 0);
    end
    else begin
        formids := ElementByName(frmlstHere, 'FormIDs');
        lnam := ElementAssign(formids, HighInteger, nil, False);
    end;
    rFormid := IntToHex(GetLoadOrderFormID(rec), 8);
    try
        SetEditValue(lnam, rFormid);
    except
        AddMessage('Error: Failed to add ' + #9 + ShortName(rec) + #9 + ' to formlist: ' + #9 + EditorID(frmlstHere));
    end;
end;

function IsFullLOD(s: IwbMainRecord): Boolean;
var
    model, editorid, recordId, wrldEdid, cellX, cellY: string;
    bIsFullLOD, bIsFullLODFlagged, bXESP, bRespect: Boolean;
    i: integer;
    r, rCell, rWrld, parentRef, xesp: IInterface;
begin
    Result := false;
    editorid := LowerCase(GetElementEditValues(s, 'EDID'));
    model := LowerCase(GetElementEditValues(s, 'Model\MODL'));
    if joRules.Contains(editorid) then begin
        bIsFullLOD := StrToBool(joRules.O[editorid].S['bisfulllod']);
    end
    else if joRules.Contains(model) then begin
        bIsFullLOD := StrToBool(joRules.O[model].S['bisfulllod']);
    end;
    if not bIsFullLOD then Exit;
    Result := true;
    for i := Pred(ReferencedByCount(s)) downto 0 do begin
        r := ReferencedByIndex(s, i);
        if Signature(r) <> 'REFR' then continue;
        if not IsWinningOverride(r) then continue;
        if GetIsDeleted(r) then continue;
        if GetIsCleanDeleted(r) then continue;
        rCell := LinksTo(ElementByIndex(r, 0));
        if IsInteriorCell(rCell) then continue;
        rWrld := LinksTo(ElementByIndex(rCell, 0));
        if IsWorldIgnored(rWrld) then continue;

        bXESP := ElementExists(r, 'XESP');
        if bXESP then begin
            xesp := ElementByPath(r, 'XESP');
            parentRef := WinningOverride(LinksTo(ElementByIndex(xesp, 0)));
            if (GetElementNativeValues(parentRef, 'Record Header\Record Flags\LOD Respects Enable State') <> 0) then bRespect := True else bRespect := False;
        end;
        if GetElementEditValues(r, 'Record Header\Record Flags\Is Full LOD') <> '0' then bIsFullLODFlagged := true else bIsFullLODFlagged := false;
        if bIsFullLODFlagged then begin
            if not bXESP then continue;
            if (GetElementNativeValues(r, 'Record Header\Record Flags\LOD Respects Enable State') <> 0) then continue;
            if bRespect then continue;
        end;

        iCurrentPlugin := CanOverrideDeterminesPlugin(r, iFolipMasterFile);
        iCurrentPlugin := RefMastersDeterminePlugin(r, iCurrentPlugin);
        iCurrentPlugin := RefMastersDeterminePlugin(GetHighestPossibleOverrideForFile(rWrld, iCurrentPlugin), iCurrentPlugin);
        iCurrentPlugin := RefMastersDeterminePlugin(GetHighestPossibleOverrideForFile(rCell, iCurrentPlugin), iCurrentPlugin);

        wrldEdid := GetElementEditValues(rWrld, 'EDID');
        cellX := GetElementNativeValues(rCell, 'XCLC\X');
        cellY := GetElementNativeValues(rCell, 'XCLC\Y');
        recordId := RecordFormIdFileId(r);

        pluginFileNameHere := GetFileName(iCurrentPlugin);
        joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['Is Full LOD'] := 1;
        joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['Set Is Persistent'] := 1;

        joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].A['AddRefToMyFormlist'].Add(tlFlst.IndexOf(flNeverfades));

        if bXESP and not bRespect then begin
            joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['LOD Respects Enable State'] := 1;
        end;

        slFullLODMessages.Add('IsFullLOD Reference:' + Name(r));
    end;
end;

procedure ProcessActiFurnMstt;
var
    si, i, cnt: integer;
    r, s, ms, rCell, rWrld: IwbElement;
    HasLOD, bFullLOD: Boolean;
    joLOD: TJsonObject;
    fakeStaticFormID, sMissingLodMessage, fakeStatic: string;
begin
    for i := 0 to Pred(tlActiFurnMstt.Count) do begin
        sMissingLodMessage := '';
        s := ObjectToElement(tlActiFurnMstt[i]);

        bFullLOD := IsFullLOD(s);
        if bFullLOD then continue;

        joLOD := TJsonObject.Create;
        HasLOD := AssignLODModels(s, joLOD, sMissingLodMessage);

        //Process references that have lod.
        if HasLOD then begin
            cnt := 0;

            for si := Pred(ReferencedByCount(s)) downto 0 do begin
                r := ReferencedByIndex(s, si);
                if Signature(r) <> 'REFR' then continue;
                if not IsWinningOverride(r) then continue;
                if GetIsDeleted(r) then continue;
                if GetIsCleanDeleted(r) then continue;
                rCell := LinksTo(ElementByIndex(r, 0));
                if IsInteriorCell(rCell) then continue;
                rWrld := WinningOverride(LinksTo(ElementByIndex(rCell, 0)));
                if IsWorldIgnored(rWrld) then continue;
                if WorldInheritsLOD(rWrld) then continue;

                //This reference should get lod if it passes these checks.
                cnt := cnt + 1;
                //Perform these functions if this is the very first reference. We set up the base object as a Fake Static.
                if cnt = 1 then begin
                    //Create Fake Static
                    fakeStatic := AddFakeStatic(s);

                    //Add LOD models
                    AssignLODToStat(s, joLOD, True, 'New');

                    slHasLOD.Add(fakeStatic);
                    slFakeStatics.Add(fakeStatic);
                end;

                //Copy
                DuplicateRef(r, fakeStatic);

                //Add mat swap if it exists to list to check.
                if not ElementExists(r, 'XMSP - Material Swap') then continue;
                ms := LinksTo(ElementByPath(r, 'XMSP'));
                if tlMswp.IndexOf(ms) = -1 then tlMswp.Add(ms);
            end;
        end
        else if bReportMissingLOD and (sMissingLodMessage <> '') then begin
            if IsObjectUsedInExterior(s) then slMissingLODMessages.Add(sMissingLodMessage);
        end;

        joLOD.Free;
    end;
end;

procedure DuplicateRef(const r: IwbElement; fakeStatic: string);
{
    Duplicates a placed reference, but used the fakeStatic base.
}
var
    n, rCell, rWrld, wCell, nCell, ms, xesp, xespDup, parentRef: IwbElement;
    bHasOppositeParent, bMswp: Boolean;
    c: TwbGridCell;
    parentFormid, cellX, cellY, wrldEdid, recordId: string;
begin
    bMswp := False;
    iCurrentPlugin := FileByName(joElements.O['STAT'].O['New'].O[fakeStatic].S['File']);
    iCurrentPlugin := RefMastersDeterminePlugin(r, iCurrentPlugin);
    rCell := LinksTo(ElementByIndex(r, 0));
    rWrld := LinksTo(ElementByIndex(rCell, 0));
    wrldEdid := GetElementEditValues(rWrld, 'EDID');
    recordId := RecordFormIdFileId(r);

    //Handle persistent worldspace cell because these will never be persistent
    if GetIsPersistent(rCell) then begin
        c := wbPositionToGridCell(GetPosition(r));
        cellX := IntToStr(c.x);
        cellY := IntToStr(c.y);
    end else begin
        cellX := GetElementEditValues(rCell, 'XCLC\X');
        cellY := GetElementEditValues(rCell, 'XCLC\Y');
    end;
    iCurrentPlugin := RefMastersDeterminePlugin(GetHighestPossibleOverrideForFile(rWrld, iCurrentPlugin), iCurrentPlugin);
    iCurrentPlugin := RefMastersDeterminePlugin(GetHighestPossibleOverrideForFile(rCell, iCurrentPlugin), iCurrentPlugin);

    if ElementExists(r, 'XMSP - Material Swap') then begin
        bMswp := true;
        ms := WinningOverride(LinksTo(ElementByPath(r, 'XMSP - Material Swap')));
        iCurrentPlugin := RefMastersDeterminePlugin(ms, iCurrentPlugin);
    end;

    if ElementExists(r, 'XESP - Enable Parent') then begin
        xesp := ElementByPath(r, 'XESP');
        parentRef := WinningOverride(LinksTo(ElementByIndex(xesp, 0)));
        parentFormid := IntToHex(GetLoadOrderFormID(parentRef), 8);
        iCurrentPlugin := RefMastersDeterminePlugin(parentRef, iCurrentPlugin);
        if (GetElementNativeValues(r, 'XESP\Flags\Set Enable State to Opposite of Parent') <> 0) then bHasOppositeParent := True else bHasOppositeParent := False;
        if tlEnableParents.IndexOf(parentRef) = -1 then tlEnableParents.Add(parentRef);
    end else begin
        parentFormid := IntToHex(GetLoadOrderFormID(r), 8);
        RefMastersDeterminePlugin(r, iCurrentPlugin);
        if ContainsText(Name(r), 'repairable') then begin
            parentRef := r;
            if tlEnableParents.IndexOf(parentRef) = -1 then tlEnableParents.Add(parentRef);
        end;
    end;

    pluginFileNameHere := GetFileName(iCurrentPlugin);

    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].S['XESP'] := 1;
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].S['XESP Reference'] := parentFormid;

    if bHasOppositeParent then joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].S['Opposite Enable Parent'] := 1;

    if bMswp then joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].S['XMSP - Material Swap'] := IntToHex(GetLoadOrderFormID(ms), 8);

    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].S['Set Is Persistent'] := 0;


    //Add new ref to cell
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].S['fakeStatic'] := fakeStatic;
    if GetIsInitiallyDisabled(r) then joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].S['Initially Disabled'] := 1;
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].S['Visible When Distant'] := 1;

    //  Set scale
    if ElementExists(r, 'XSCL - Scale') then
        joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].S['XSCL - Scale'] := GetElementNativeValues(r, 'XSCL - Scale');

    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].O['pos'].S['x'] := GetElementNativeValues(r, 'DATA\Position\X');
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].O['pos'].S['y'] := GetElementNativeValues(r, 'DATA\Position\Y');
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].O['pos'].S['z'] := GetElementNativeValues(r, 'DATA\Position\Z');
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].O['rot'].S['x'] := GetElementNativeValues(r, 'DATA\Rotation\X');
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].O['rot'].S['y'] := GetElementNativeValues(r, 'DATA\Rotation\Y');
    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].O['rot'].S['z'] := GetElementNativeValues(r, 'DATA\Rotation\Z');

    joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['New'].O[recordId].A['AddRefToMyFormlist'].Add(tlFlst.IndexOf(flFakeStatics));
end;

procedure CopyObjectBounds(const copyFrom: IwbMainRecord; var copyTo: IwbMainRecord);
{
    Copies the object bounds of the first reference to the second reference.
}
begin
    SetElementNativeValues(copyTo, 'OBND\X1', GetElementNativeValues(copyFrom, 'OBND\X1'));
    SetElementNativeValues(copyTo, 'OBND\X2', GetElementNativeValues(copyFrom, 'OBND\X2'));
    SetElementNativeValues(copyTo, 'OBND\Y1', GetElementNativeValues(copyFrom, 'OBND\Y1'));
    SetElementNativeValues(copyTo, 'OBND\Y2', GetElementNativeValues(copyFrom, 'OBND\Y2'));
    SetElementNativeValues(copyTo, 'OBND\Z1', GetElementNativeValues(copyFrom, 'OBND\Z1'));
    SetElementNativeValues(copyTo, 'OBND\Z2', GetElementNativeValues(copyFrom, 'OBND\Z2'));
end;

function AddFakeStatic(s: IwbElement): string;
{
    Adds a fake static version of the non-static input and returns the recordId of the original object.
}
var
    ms, fakeStatic: IwbElement;
    patchStatGroup: IwbGroupRecord;
    HasMS: Boolean;
    fakeStaticEditorId, recordId, msFormid: string;
begin
    HasMS := ElementExists(s, 'Model\MODS - Material Swap');
    if HasMS then begin
        ms := LinksTo(ElementByPath(s, 'Model\MODS'));
        if tlMswp.IndexOf(ms) = -1 then tlMswp.Add(ms);
        iCurrentPlugin := RefMastersDeterminePlugin(ms, iFolipMasterFile);
    end
    else iCurrentPlugin := iFolipMasterFile;

    //Add Fake STAT
    fakeStaticEditorId := 'FOLIP_' + EditorID(s) + '_FakeStatic';

    recordId := RecordFormIdFileId(s);
    joElements.O['STAT'].O['New'].O[recordId].S['EDID'] := fakeStaticEditorId;
    joElements.O['STAT'].O['New'].O[recordId].S['Copy Object Bounds'] := True;
    joElements.O['STAT'].O['New'].O[recordId].S['Is Marker'] := 1;
    joElements.O['STAT'].O['New'].O[recordId].S['File'] := GetFileName(iCurrentPlugin);

    //Add base material swap
    if HasMS then begin
        msFormid := IntToHex(GetLoadOrderFormID(ms), 8);
        joElements.O['STAT'].O['New'].O[recordId].S['MODS'] := msFormid;
    end;

    Result := recordId;
end;

procedure AssignLODMaterialsList;
{
    Assign lod materials to MSWP record.
}
var
    i, si, tp, sc, cnt, n, oms: integer;
    m, mn, substitutions, sub: IInterface;
    colorRemap, originalMat, originalLODMat, replacementMat, om, rm, recordId: string;
    slLODSubOriginal, slLODSubReplacement, slExistingSubstitutions, slMissingMaterials, slTopPaths, slLODOriginals, slLODReplacements, slDummy, slMismatchedMaterials: TStringList;
    hasLODOriginalMaterial, hasLODReplacementMaterial: Boolean;
begin
    slDummy := TStringList.Create;
    //store missing lod materials here
    slMissingMaterials := TStringList.Create;
    slMismatchedMaterials := TStringList.Create;
    slMismatchedMaterials.Sorted := True;
    slMismatchedMaterials.Duplicates := dupIgnore;
    try
        for i := 0 to Pred(tlMswp.Count) do begin
            m := WinningOverride(ObjectToElement(tlMswp[i]));

            slLODSubOriginal := TStringList.Create;
            slLODSubReplacement := TStringList.Create;

            //make a list of existing substitutions, which we will need to then check to make sure we don't add a duplicate substitution.
            slExistingSubstitutions := TStringList.Create;
            substitutions := ElementByName(m, 'Material Substitutions');
            //foreach substitution in substitutions
            for si := 0 to Pred(ElementCount(substitutions)) do begin
                //the substitution
                sub := ElementByIndex(substitutions, si);
                if not ElementExists(sub, 'BNAM - Original Material') then continue;
                originalMat := LowerCase(GetElementEditValues(sub, 'BNAM - Original Material'));
                if originalMat = '' then continue;
                if LeftStr(originalMat, 10) <> 'materials\' then originalMat := 'materials\' + originalMat;
                slExistingSubstitutions.Add(originalMat);
            end;

            //We need to loop through the substitutions again to actually do the work now that we know what the existing substitutions are.
            //foreach substitution in substitutions
            for si := 0 to Pred(ElementCount(substitutions)) do begin
                //the substitution
                sub := ElementByIndex(substitutions, si);
                if not ElementExists(sub, 'BNAM - Original Material') then continue;
                if not ElementExists(sub, 'SNAM - Replacement Material') then continue;
                originalMat := GetElementEditValues(sub, 'BNAM - Original Material');
                if originalMat = '' then continue;
                originalMat := LowerCase(NormalizeResourceNameFixed(originalMat, resMaterial));

                slTopPaths := TStringList.Create;
                for tp := 0 to Pred(slTopLevelModPatternPaths.Count) do begin
                    if ContainsText(originalMat, 'materials\' + slTopLevelModPatternPaths[tp]) then slTopPaths.Add(slTopLevelModPatternPaths[tp]);
                end;

                hasLODOriginalMaterial := False;
                slLODOriginals := TStringList.Create;
                om := LODMaterial(originalMat, '', slTopPaths, slExistingSubstitutions, slLODOriginals);
                slTopPaths.Free;
                if slLODOriginals.Count > 0 then hasLODOriginalMaterial := True;
                if not hasLODOriginalMaterial then continue;
                //ListStringsInStringList(slLODOriginals);

                replacementMat := GetElementEditValues(sub, 'SNAM - Replacement Material');
                if replacementMat = '' then continue;
                replacementMat := LowerCase(NormalizeResourceNameFixed(replacementMat, resMaterial));
                if (ContainsText(replacementMat, '*') or ContainsText(originalMat, '*')) then begin
                    AddMessage(Name(m) + #9 + 'Warning: This Material Swap Substitution uses an asterisk. This may need manual adjustment for LOD to swap correctly.' + #9 + originalMat + #9 + replacementMat);
                end;

                colorRemap := FloatToStr(StrToFloatDef(GetElementEditValues(sub, 'CNAM - Color Remapping Index'),'9'));
                if ((originalMat = replacementMat) and (colorRemap = '9')) then begin
                    AddMessage(Name(m) + #9 + 'Ignoring this Material Swap Substitution due to the Original Material being the same as the Replacement Material: ' + #9 + originalMat + #9 + replacementMat);
                    continue;
                end;
                if colorRemap = '9' then colorRemap := '' else colorRemap := '_' + colorRemap;


                slTopPaths := TStringList.Create;
                for tp := 0 to Pred(slTopLevelModPatternPaths.Count) do begin
                    if ContainsText(replacementMat, 'materials\' + slTopLevelModPatternPaths[tp]) then slTopPaths.Add(slTopLevelModPatternPaths[tp]);
                end;

                hasLODReplacementMaterial := False;
                slLODReplacements := TStringList.Create;
                rm := LODMaterial(replacementMat, colorRemap, slTopPaths, slDummy, slLODReplacements);
                slTopPaths.Free;
                if slLODReplacements.Count > 0 then hasLODReplacementMaterial := True;
                if not hasLODReplacementMaterial then begin
                    if ResourceExists(replacementMat) then begin
                        if colorRemap = '' then begin
                            if bMakeMissingMaterials then AddMessage(Name(m) + #9 + 'Missing LOD replacement material: ' + rm + #9 + ' from ' + #9 + om);
                            if not CreateLODMaterialReplacement('materials\' + slLODOriginals[0], rm, replacementMat, False) then begin
                                slMissingMaterials.Add(Name(m) + #9 + 'Missing LOD replacement material: ' + rm + #9 + ' from ' + #9 + om);
                                continue;
                            end else begin
                                slLODReplacements.Add(rm);
                                slMatFiles.Add(rm);
                            end;
                        end
                        else begin
                            slMissingMaterials.Add(Name(m) + #9 + 'Missing LOD color remap replacement material: ' + rm + #9 + ' from ' + #9 + om);
                            continue;
                        end;
                    end
                    else begin
                        AddMessage(Name(m) + #9 + 'Ignoring this Material Swap Substitution due to the Replacement Material referencing a material that does not exist: ' + replacementMat);
                        continue;
                    end;
                end;
                //ListStringsInStringList(slLODReplacements);

                for oms := 0 to Pred(slLODOriginals.Count) do begin
                    om := slLODOriginals[oms];
                    if slLODSubOriginal.IndexOf(om) > -1 then continue;
                    if slExistingSubstitutions.IndexOf('materials\' + om) > -1 then continue;
                    rm := slLODReplacements[0];
                    if om = rm then continue;
                    slLODSubOriginal.Add(om);
                    slLODSubReplacement.Add(rm);
                    CompareMaterialSwaps(slMismatchedMaterials, om, rm);
                end;

                slLODReplacements.Free;
                slLODOriginals.Free;
            end;

            //continue if no changes are required.
            cnt := slLODSubOriginal.Count;
            if cnt < 1 then begin
                slLODSubOriginal.Free;
                slLODSubReplacement.Free;
                continue;
            end;

            //Changes required, so add material swap to patch
            iCurrentPlugin := CanOverrideDeterminesPlugin(m, iFolipMasterFile);
            iCurrentPlugin := RefMastersDeterminePlugin(m, iCurrentPlugin);
            recordId := RecordFormIdFileId(m);
            joElements.O['MSWP'].O['Overrides'].O[recordId].S['File'] := GetFileName(iCurrentPlugin);
            for n := 0 to Pred(cnt) do begin
                //AddMessage(ShortName(m) + #9 + slLODSubOriginal[n] + #9 + slLODSubReplacement[n]);
                joElements.O['MSWP'].O['Overrides'].O[recordId].A['AddMaterialSwap'].Add(slLODSubOriginal[n] + '|' + slLODSubReplacement[n]);
                //AddMaterialSwap(mn, slLODSubOriginal[n], slLODSubReplacement[n]);
            end;
            slLODSubOriginal.Free;
            slLODSubReplacement.Free;
        end;

        ListStringsInStringList(slMissingMaterials);
        ListStringsInStringList(slMismatchedMaterials);
    finally
        slMissingMaterials.Free;
        slMismatchedMaterials.Free;
        slDummy.Free;
    end;
end;

function CreateLODMaterialReplacement(om, rm, replacementMat: string; bForceTexGenRedo: Boolean): Boolean;
{
    Creates a LOD material replacement if it does not already exist.
    om is the path to the original lod material.
    rm is the path to the replacement lod material to be created.
    replacementMat is the path to the base material that the lod replacement is based on.
}
var
    ombgsm, rmbgsm, replacementMatbgsm: TwbBGSMFile;
    omDiffuse, omDiffuseNormalized, replacementDiffuse, replacementDiffuseNormalized, replacementNormal, replacementNormalNormalized, replacementSpecular, replacementSpecularNormalized, replacementLodDiffuse,
    lodDiffuse, lodNormal, lodSpecular, omAuto, paletteScale, paletteTexture: string;
    specularMult: float;
    bLodTextureExists, bOmAutoGenerated, bGrayscaleToPalette, bLodUsesGrayscaleToPalette: Boolean;
begin
    Result := False;
    if not bMakeMissingMaterials then Exit;
    if not bForceTexGenRedo then AddMessage('Attempting to create a missing LOD Material Replacement: ' + rm + #9 + 'from' + #9 + om);
    bOmAutoGenerated := False;

    if not ResourceExists(om) then begin
        if FileExists(sOutputDir + '\' + om) then begin
            omAuto := sOutputDir + '\' + om;
            bOmAutoGenerated := True;
        end else begin
            AddMessage(#9 + 'Error: ' + om + ' does not exist.');
            Exit;
        end;
    end;

    ombgsm := TwbBGSMFile.Create;
    replacementMatbgsm := TwbBGSMFile.Create;
    try
        if not bOmAutoGenerated then ombgsm.LoadFromResource(om) else ombgsm.LoadFromFile(omAuto);
        replacementMatbgsm.LoadFromResource(replacementMat);

        //Check if the replacement material is alterring the UV offsets or scales, and if so, exit without creating a replacement.
        if ombgsm.NativeValues['UOffset'] <> replacementMatbgsm.NativeValues['UOffset'] then begin
            AddMessage(#9 + 'Original material ' + om + ' has a different UOffset than the replacement material ' + replacementMat + '. Skipping creation of LOD material replacement.');
            Exit;
        end;
        if ombgsm.NativeValues['VOffset'] <> replacementMatbgsm.NativeValues['VOffset'] then begin
            AddMessage(#9 + 'Original material ' + om + ' has a different VOffset than the replacement material ' + replacementMat + '. Skipping creation of LOD material replacement.');
            Exit;
        end;
        if ombgsm.NativeValues['UScale'] <> replacementMatbgsm.NativeValues['UScale'] then begin
            AddMessage(#9 + 'Original material ' + om + ' has a different UScale than the replacement material ' + replacementMat + '. Skipping creation of LOD material replacement.');
            Exit;
        end;
        if ombgsm.NativeValues['VScale'] <> replacementMatbgsm.NativeValues['VScale'] then begin
            AddMessage(#9 + 'Original material ' + om + ' has a different VScale than the replacement material ' + replacementMat + '. Skipping creation of LOD material replacement.');
            Exit;
        end;

        //Check if the replacement material has the Hide Secret Flag set, and if so, exit without creating a replacement.
        if replacementMatbgsm.EditValues['Hide Secret'] = 'yes' then begin
            AddMessage(#9 + 'Replacement material ' + replacementMat + ' has the Hide Secret Flag set. Skipping creation of LOD material replacement.');
            Exit;
        end;

        //Check if this uses Grayscale To Palette
        bGrayscaleToPalette := (replacementMatbgsm.EditValues['GrayscaleToPaletteColor'] = 'yes');

        //Get lod diffuse texture of the original material
        omDiffuse := ombgsm.EditValues['Textures\Diffuse'];
        omDiffuseNormalized := wbNormalizeResourceName(omDiffuse, resTexture);

        //Get the base replacement material's textures
        replacementDiffuse := replacementMatbgsm.EditValues['Textures\Diffuse'];
        replacementDiffuseNormalized:= wbNormalizeResourceName(replacementDiffuse, resTexture);
        if not ResourceExists(replacementDiffuseNormalized) then begin
            AddMessage(#9 + 'Replacement material ' + replacementMat + ' uses a diffuse texture that does not exist. Skipping creation of LOD material replacement.');
            Exit;
        end;
        if bGrayscaleToPalette then begin
            //Sometimes LOD palette scale will need to be shifted versus the full model's palette scale due to vertex colors.
            //If this is simply a rasterization of grayscale to palette materials, the om will be equal to the rm.
            //If the GrayscaleToPaletteColor flag is on, then it is setup to use that palette at a potentially shifted scale.
            //If both these are true, use the scale from the pre-existing LOD material instead of from the full material.
            //If GrayscaleToPaletteColor is off, then the material was already rasterized.
            //TODO: Make rules to shift these when necessary.
            if (SameText(om, rm) and (ombgsm.EditValues['GrayscaleToPaletteColor'] = 'yes'))
            then paletteScale := FloatToStr(StrToFloatDef(ombgsm.EditValues['GrayscaleToPaletteScale'], '0'))
            else paletteScale := FloatToStr(StrToFloatDef(replacementMatbgsm.EditValues['GrayscaleToPaletteScale'], '0'));
            bLodUsesGrayscaleToPalette := true;
            if joRasterizeMaterials.Contains(rm) then begin
                bLodUsesGrayscaleToPalette := StrToBool(joRasterizeMaterials.O[rm].S['GrayscaleToPaletteColor']);
                paletteScale := Fallback(joRasterizeMaterials.O[rm].S['GrayscaleToPaletteScale'], paletteScale);
            end;
            if bLodUsesGrayscaleToPalette then begin
                paletteTexture := wbNormalizeResourceName(replacementMatbgsm.EditValues['Textures\Grayscale'], resTexture);
                replacementDiffuseNormalized := CreateRasterizedFullDiffuseTexture(replacementDiffuseNormalized, paletteTexture, paletteScale, rm);
            end;
            joRasterizeMaterials.O[rm].S['Full Material'] := replacementMat;
            joRasterizeMaterials.O[rm].S['GrayscaleToPaletteColor'] := true;
            joRasterizeMaterials.O[rm].S['GrayscaleToPaletteScale'] := paletteScale;
            joRasterizeMaterials.O[rm].S['RasterizedDiffusePath'] := replacementDiffuseNormalized;
        end;

        replacementLodDiffuse := ChangeFullToLodDirectory(replacementDiffuseNormalized);

        //In case the diffuse texture doesn't follow proper naming conventions...
        if RightStr(LowerCase(replacementLodDiffuse), 6) <> '_d.dds' then replacementLodDiffuse := TrimLeftChars(replacementLodDiffuse, 4) + '_d.dds';

        replacementNormal := replacementMatbgsm.EditValues['Textures\Normal'];
        replacementNormalNormalized := wbNormalizeResourceName(replacementNormal, resTexture);
        if replacementNormalNormalized = '' then replacementNormalNormalized := 'textures\shared\flatflat_n.dds'
        else if not ResourceExists(replacementNormalNormalized) then replacementNormalNormalized := 'textures\shared\flatflat_n.dds';

        replacementSpecular := replacementMatbgsm.EditValues['Textures\SmoothSpec'];
        replacementSpecularNormalized := wbNormalizeResourceName(replacementSpecular, resTexture);
        if replacementSpecularNormalized = '' then replacementSpecularNormalized := 'textures\shared\black01_d.dds'
        else if not ResourceExists(replacementSpecularNormalized) then replacementSpecularNormalized := 'textures\shared\black01_d.dds';

        specularMult := replacementMatbgsm.NativeValues['SpecularMult'] * replacementMatbgsm.NativeValues['Smoothness'];
        //AddMessage(FloatToStr(specularMult));

        //Check if the replacementDiffuse already has a lod texture being created by TexGen.
        if not bForceTexGenRedo then bLodTextureExists := DoesTexGenAlreadyHaveTexture(replacementLodDiffuse) else bLodTextureExists := False;
        bLodTextureExists := (slAddedTexGenTextures.IndexOf(replacementDiffuseNormalized) <> -1);

        //Attempt to add texgen rules to create the new lod replacement textures.
        if not bLodTextureExists then begin
            if not bForceTexGenRedo then AddMessage(#9 + 'TexGen does not have a texture for ' + replacementLodDiffuse + ', creating TexGen rules.');
            if not AddTexgenRules(omDiffuseNormalized, replacementDiffuseNormalized, replacementNormalNormalized, replacementSpecularNormalized, specularMult) then Exit;
        end
        else AddMessage(#9 + 'TexGen already has a texture for ' + replacementLodDiffuse + ', skipping TexGen rules creation.');

        //If we got this far, we can create the new lod material.

        lodDiffuse := StringReplace(TrimRightChars(replacementLodDiffuse, 9), '\', '/', [rfReplaceAll]);
        lodNormal := TrimLeftChars(lodDiffuse, 6) + '_n.dds';
        lodSpecular := TrimLeftChars(lodDiffuse, 6) + '_s.dds';
        replacementMatbgsm.EditValues['Textures\Diffuse'] := lodDiffuse;
        replacementMatbgsm.EditValues['Textures\Normal'] := lodNormal;
        replacementMatbgsm.EditValues['Textures\SmoothSpec'] := lodSpecular;
        replacementMatbgsm.EditValues['Textures\Grayscale'] := '';
        replacementMatbgsm.EditValues['SpecularEnabled'] := 'yes';
        replacementMatbgsm.NativeValues['AlphaTest'] := ombgsm.NativeValues['AlphaTest'];

        replacementMatbgsm.EditValues['ScreenSpaceReflections'] := 'no';
        replacementMatbgsm.EditValues['WetnessControlScreenSpaceReflections'] := 'no';
        replacementMatbgsm.EditValues['Decal'] := 'no';
        replacementMatbgsm.EditValues['DecalNoFade'] := 'no';
        replacementMatbgsm.EditValues['Refraction'] := 'no';
        replacementMatbgsm.EditValues['EnvironmentMapping'] := 'no';
        replacementMatbgsm.EditValues['GrayscaleToPaletteColor'] := 'no';

        replacementMatbgsm.EditValues['Textures\Envmap'] := '';
        replacementMatbgsm.EditValues['Textures\Glow'] := '';
        replacementMatbgsm.EditValues['Textures\InnerLayer'] := '';
        replacementMatbgsm.EditValues['Textures\Wrinkles'] := '';
        replacementMatbgsm.EditValues['Textures\Displacement'] := '';
        replacementMatbgsm.EditValues['RimLighting'] := 'no';
        replacementMatbgsm.EditValues['SubsurfaceLighting'] := 'no';
        replacementMatbgsm.EditValues['AnisoLighting'] := 'no';
        replacementMatbgsm.EditValues['EmitEnabled'] := 'no';
        replacementMatbgsm.EditValues['ModelSpaceNormals'] := 'no';
        replacementMatbgsm.EditValues['ExternalEmittance'] := 'no';
        replacementMatbgsm.EditValues['ReceiveShadows'] := 'no';
        replacementMatbgsm.EditValues['CastShadows'] := 'no';
        replacementMatbgsm.EditValues['DissolveFade'] := 'no';
        replacementMatbgsm.EditValues['AssumeShadowmask'] := 'no';
        replacementMatbgsm.EditValues['Glowmap'] := 'no';
        replacementMatbgsm.EditValues['EnvironmentMappingWindow'] := 'no';
        replacementMatbgsm.EditValues['EnvironmentMappingEye'] := 'no';
        replacementMatbgsm.EditValues['Hair'] := 'no';
        replacementMatbgsm.EditValues['Tree'] := 'no';
        replacementMatbgsm.EditValues['Facegen'] := 'no';
        replacementMatbgsm.EditValues['SkinTint'] := 'no';
        replacementMatbgsm.EditValues['Tessellate'] := 'no';
        replacementMatbgsm.EditValues['SkewSpecularAlpha'] := 'no';
        EnsureDirectoryExists(sOutputDir + '\' + ExtractFilePath(rm));
        replacementMatbgsm.SaveToFile(sOutputDir + '\' + rm);
        AddMessage(#9 + 'Successfully created LOD Material Replacement: ' + rm + #9 + 'from' + #9 + om);
        Result := True;
        slMatFiles.Add(rm);
    finally
        ombgsm.Free;
        replacementMatbgsm.Free;
    end;
end;

function CreateRasterizedFullDiffuseTexture(replacementDiffuseNormalized, paletteTexture, paletteScale, rm: string): string;
var
    diffuse, diffuseNew, palette, outputTexture, cmdline, paletteName: string;
begin
    Result := '';
    diffuse := ExtractResourceToTempDirectory(replacementDiffuseNormalized);
    palette := ExtractResourceToTempDirectory(paletteTexture);
    paletteName := TrimLeftChars(ExtractFileName(palette), 4);
    AddMessage(replacementDiffuseNormalized + #9 + paletteTexture + #9 + paletteScale);
    //textures\path\to\grayscale_d.dds to textures\RasterizedGrayscales\path\to\grayscale_0.937.dds
    diffuseNew := StringReplace(TrimLeftChars(replacementDiffuseNormalized, 5),'textures\','textures\RasterizedGrayscales\',[rfIgnoreCase]) + paletteName + '_' + paletteScale + '_d.dds';
    diffuseNew := Fallback(joRasterizeMaterials.O[rm].S['RasterizedDiffusePath'], diffuseNew);
    Result := diffuseNew;
    outputTexture := sOutputDir + '\' + TrimLeftChars(diffuseNew, 4) + '.dds';
    if FileExists(outputTexture) then Exit;
    EnsureDirectoryExists(ExtractFilePath(outputTexture));
    cmdline := '"' + texconv + '" "' + diffuse + '" "' + palette + '" ' + paletteScale + ' "' + outputTexture + '" 1024 dxt5';
    AddMessage(cmdline);
    ShellExecute(0, 'open', sRasterizeGrayScaleToPalette, cmdline, '', SW_SHOWNORMAL);
end;

function AddTexgenRules(omDiffuseNormalized, replacementDiffuseNormalized, replacementNormalNormalized, replacementSpecularNormalized: string; specularMult: float): Boolean;
{
    Adds texgen rules to create the new lod replacement textures.
    Returns true if successful, false otherwise.
}
var
    new_line, tempDiffuse: string;
    slNew_lines: TStringList;
    i: integer;
begin
    Result := False;
    slAddedTexGenTextures.Add(replacementDiffuseNormalized);
    slNew_lines := TStringList.Create;
    try
        if ProcessTexgenFile(slFOLIPTexgen_noalpha, omDiffuseNormalized, replacementDiffuseNormalized, slNew_lines) then begin
            TexGenCopy(replacementDiffuseNormalized, replacementNormalNormalized, replacementSpecularNormalized, False, specularMult, tempDiffuse);
            for i := 0 to Pred(slNew_lines.Count) do begin
                new_line := StringReplace(slNew_lines[i], replacementDiffuseNormalized, tempDiffuse, [rfIgnoreCase]);
                slTexgen_noalpha.Add(new_line);
            end;
            Result := True;
        end
        else if ProcessTexgenFile(slFOLIPTexgen_alpha, omDiffuseNormalized, replacementDiffuseNormalized, slNew_lines) then begin
            TexGenCopy(replacementDiffuseNormalized, replacementNormalNormalized, replacementSpecularNormalized, True, specularMult, tempDiffuse);
            for i := 0 to Pred(slNew_lines.Count) do begin
                new_line := StringReplace(slNew_lines[i], replacementDiffuseNormalized, tempDiffuse, [rfIgnoreCase]);
                slTexgen_alpha.Add(new_line);
            end;
            Result := True;
        end;
    finally
        slNew_lines.Free;
    end;
end;

function TexGenCopy(replacementDiffuseNormalized, replacementNormalNormalized, replacementSpecularNormalized: string; alpha: Boolean; specularMult: float; var tempDiffuse: string): Boolean;
{
    Adds texgen copy and adjustment rules to create the new lod replacement textures.
    Returns true if adjustments were needed, false otherwise.
}
var
    lodNormal, lodSpecular, tempNormal, tempSpecular, diffuseLine, normalLine, specularLine, replacementDiffuseNormalizedActual: string;
    specularMultInt: integer;
begin
    Result := False;
    // We always need to copy the diffuse map to the tempDiffuse in case we don't need to adjust the specular.
    tempDiffuse := replacementDiffuseNormalized;

    //In case the diffuse texture doesn't follow proper naming convention of ending in _d.dds
    replacementDiffuseNormalizedActual := replacementDiffuseNormalized;
    if RightStr(LowerCase(replacementDiffuseNormalized), 6) <> '_d.dds' then begin
        replacementDiffuseNormalized := TrimLeftChars(replacementDiffuseNormalized, 4) + '_d.dds';
    end;

    // If the diffuse and normal do not have the same base name, we need to add a copy rule for the normal map.
    lodNormal := TrimLeftChars(replacementDiffuseNormalized, 6) + '_n.dds';
    if TrimLeftChars(replacementDiffuseNormalized, 6) <> TrimLeftChars(replacementNormalNormalized, 6) then begin
        slTexgen_copy.Add(replacementNormalNormalized + ',' + lodNormal);
        AddMessage(#9 + 'Adding TexGen copy rule for normal map: ' + replacementNormalNormalized + ' to ' + lodNormal);
        Result := True;
    end;

    // If the diffuse and specular do not have the same base name, we need to add a copy rule for the specular map.
    lodSpecular := TrimLeftChars(replacementDiffuseNormalized, 6) + '_s.dds';
    if TrimLeftChars(replacementDiffuseNormalized, 6) <> TrimLeftChars(replacementSpecularNormalized, 6) then begin
        slTexgen_copy.Add(replacementSpecularNormalized + ',' + lodSpecular);
        AddMessage(#9 + 'Adding TexGen copy rule for specular map: ' + replacementSpecularNormalized + ' to ' + lodSpecular);
        Result := True;
    end;

    // If the specular multiplier is less than 1, we need to add a rule to adjust the specular map.
    if specularMult < 1 then begin
        Result := True;
        specularMultInt := Round(100 * (specularMult - 1));
        tempDiffuse := 'DynDOLOD-Temp\' + replacementDiffuseNormalized;
        tempNormal := 'DynDOLOD-Temp\' + lodNormal;
        tempSpecular := 'DynDOLOD-Temp\' + lodSpecular;

        diffuseLine := '0' + #9 + '1' + #9 + '1' + #9 + '1' + #9 +
                        replacementDiffuseNormalizedActual +
                        #9 + 'x' + #9 + '7' + #9 + '0' + #9 + '0' + #9 +
                        tempDiffuse + #9 + '0' + #9 + 'x';

        normalLine := '0' + #9 + '1' + #9 + '1' + #9 + '1' + #9 +
                        lodNormal +
                        #9 + 'x' + #9 + '7' + #9 + '0' + #9 + '0' + #9 +
                        tempNormal + #9 + '0' + #9 + 'x';

        specularLine := IntToStr(specularMultInt) + #9 + '-1' + #9 + '1' + #9 + '1' + #9 +
                        lodSpecular +
                        #9 + 'x' + #9 + '7' + #9 + '0' + #9 + '0' + #9 +
                        tempSpecular + #9 + '0' + #9 + 'x';

        if alpha then begin
            slTexgen_alpha.Add(diffuseLine);
            slTexgen_alpha.Add(normalLine);
            slTexgen_alpha.Add(specularLine);
            AddMessage(#9 + 'Adding TexGen alpha rules for diffuse: ' + replacementDiffuseNormalized + ', normal: ' + lodNormal + ', specular: ' + lodSpecular);
        end
        else begin
            slTexgen_noalpha.Add(diffuseLine);
            slTexgen_noalpha.Add(normalLine);
            slTexgen_noalpha.Add(specularLine);
            AddMessage(#9 + 'Adding TexGen noalpha rules for diffuse: ' + replacementDiffuseNormalized + ', normal: ' + lodNormal + ', specular: ' + lodSpecular);
        end;
    end
    else if replacementDiffuseNormalizedActual <> replacementDiffuseNormalized then begin //This is triggered if the diffuse texture didn't follow proper naming standard of _d.dds, and didn't already get handled because of needed specular changes.
        Result := True;
        tempDiffuse := 'DynDOLOD-Temp\' + replacementDiffuseNormalized;
        tempNormal := 'DynDOLOD-Temp\' + lodNormal;
        tempSpecular := 'DynDOLOD-Temp\' + lodSpecular;

        diffuseLine := '0' + #9 + '1' + #9 + '1' + #9 + '1' + #9 +
                        replacementDiffuseNormalizedActual +
                        #9 + 'x' + #9 + '7' + #9 + '0' + #9 + '0' + #9 +
                        tempDiffuse + #9 + '0' + #9 + 'x';

        normalLine := '0' + #9 + '1' + #9 + '1' + #9 + '1' + #9 +
                        lodNormal +
                        #9 + 'x' + #9 + '7' + #9 + '0' + #9 + '0' + #9 +
                        tempNormal + #9 + '0' + #9 + 'x';

        specularLine := '0' + #9 + '1' + #9 + '1' + #9 + '1' + #9 +
                        lodSpecular +
                        #9 + 'x' + #9 + '7' + #9 + '0' + #9 + '0' + #9 +
                        tempSpecular + #9 + '0' + #9 + 'x';

        if alpha then begin
            slTexgen_alpha.Add(diffuseLine);
            slTexgen_alpha.Add(normalLine);
            slTexgen_alpha.Add(specularLine);
            AddMessage(#9 + 'Adding TexGen alpha rules for diffuse: ' + replacementDiffuseNormalized + ', normal: ' + lodNormal + ', specular: ' + lodSpecular);
        end
        else begin
            slTexgen_noalpha.Add(diffuseLine);
            slTexgen_noalpha.Add(normalLine);
            slTexgen_noalpha.Add(specularLine);
            AddMessage(#9 + 'Adding TexGen noalpha rules for diffuse: ' + replacementDiffuseNormalized + ', normal: ' + lodNormal + ', specular: ' + lodSpecular);
        end;
    end;
end;

function ProcessTexgenFile(slTexGen_file: TStringList; omDiffuseNormalized, replacementDiffuseNormalized: string; var slNew_lines: TStringList;): Boolean;
{
    Adds texgen rules to create the new lod replacement textures.
    Returns true if successful, false otherwise.
}
var
    slTextureList, slLine: TStringList;
    i, c, t: integer;
    bTextureMatch: boolean;
    new_line, lodDiffuse: string;
begin
    Result := False;
    slTextureList := TStringList.Create;
    lodDiffuse := ChangeFullToLodDirectory(replacementDiffuseNormalized);
    //In case the diffuse texture doesn't follow proper naming convention of ending in _d.dds
    if RightStr(LowerCase(lodDiffuse), 6) <> '_d.dds' then begin
        lodDiffuse := TrimLeftChars(lodDiffuse, 4) + '_d.dds';
    end;
    try
        slTextureList.Add(omDiffuseNormalized);
        AddMessage(#9 + 'Checking TexGen file for texture: ' + omDiffuseNormalized);

        //Check noalpha
        for i := 0 to Pred(slTexGen_file.Count) do begin
            bTextureMatch := False;
            if slTexGen_file[i] = '' then continue; // Skip empty lines

            for t := 0 to Pred(slTextureList.Count) do begin
                if ContainsText(slTexGen_file[i], slTextureList[t]) then begin

                    bTextureMatch := True;
                    Break; // Exit the inner loop if a match is found
                end;
            end;

            if bTextureMatch then begin
                AddMessage(#9 + 'Match found: ' + slTexGen_file[i]);

                slLine := TStringList.Create;
                try
                    slLine.Delimiter := #9; // Set delimiter to tab character
                    slLine.DelimitedText := slTexGen_file[i];

                    if ContainsText(slLine[0], '//') then begin // skip comment lines
                        continue;
                    end else if slLine[5] = 'x' then begin // skip x lines (temporary texture setup for mipmaps, typically for adjusting specular, which we should do automatically)
                        slTextureList.Add(TrimLeftChars(slLine[9], 5)); // Add the texture to the match list, removing the d.dds suffix
                        continue;
                    end else if slLine[5] = 'r' then begin // skip r lines (rotation lines)
                        AddMessage(#9 + 'Original material requires rotation: ' + slLine[9]);
                        Exit; //If rotation is required, we want to manually handle this, so exit the function.
                        // slTextureList.Add(slLine[9]); // Add the texture to the match list
                        // continue;
                    end;

                    if ContainsText(slLine[9], 'DynDOLOD-Temp') then begin
                        AddMessage(#9#9 + 'Warning: Original LOD texture has a TexGen rule that uses a temporary texture. It is possible that auto-generating the texture based off this may not produce the expected appearance.');
                        slTextureList.Add(slLine[9]); // Add the texture to the match list
                        continue;
                    end;

                    if ContainsText(slLine[4], 'DynDOLOD-Temp') then begin
                        AddMessage(#9#9 + 'Warning: Original LOD texture has a TexGen rule that uses a temporary texture.  It is possible that auto-generating the texture based off this may not produce the expected appearance.');
                        //Don't skip this line, as this is likely the main rule.
                    end;

                    new_line := slLine[0] + #9 + slLine[1] + #9 + slLine[2] + #9 + slLine[3] + #9
                                + replacementDiffuseNormalized
                                + #9 + slLine[5] + #9 + slLine[6] + #9 + slLine[7] + #9 + slLine[8] + #9
                                + lodDiffuse
                                + #9 + slLine[10] + #9 + slLine[11];
                    AddMessage(#9 + 'Adding new TexGen line: ' + new_line);
                    slNew_lines.Add(new_line);
                    Result := True;

                finally
                    slLine.Free;
                end;
            end;
        end;
    finally
        slTextureList.Free;
    end;
end;

function DoesTexGenAlreadyHaveTexture(texture: string): Boolean;
{
    Checks if TexGen already creates the texture.
    Returns true if the texture is found, false otherwise.
}
begin
    Result := False;
    if CheckTexGenFileForTexture(slFOLIPTexgen_noalpha, texture) then Result := True
    else if CheckTexGenFileForTexture(slFOLIPTexgen_alpha, texture) then Result := True;
end;

function CheckTexGenFileForTexture(slTexGen_file: TStringList; texture: string): Boolean;
{
    Checks if the given TexGen file already has a rule for the given texture.
    Returns true if the texture is found, false otherwise.
}
var
    i: integer;
    slLine: TStringList;
begin
    Result := False;
    texture := LowerCase(texture);
    for i := 0 to Pred(slTexGen_file.Count) do begin
        if slTexGen_file[i] = '' then continue; // Skip empty lines
        slLine := TStringList.Create;
        try
            slLine.Delimiter := #9; // Set delimiter to tab character
            slLine.DelimitedText := slTexGen_file[i];

            if ContainsText(slLine[0], '//') then continue; // skip comment lines

            if ((slLine.Count > 9) and SameText(LowerCase(slLine[9]), texture)) then begin
                Result := True;
                Break; // Exit the loop if the texture is found
            end;
        finally
            slLine.Free;
        end;
    end;
end;

procedure AddMaterialSwap(var e: IwbMainRecord; const om, rm: String);
{
    Given a material swap record e, add a new swap with om as the original material and rm as the replacement material.
}
var
    substitutions, ms: IwbElement;
begin
    substitutions := ElementByPath(e, 'Material Substitutions');
    ms := ElementAssign(substitutions, HighInteger, nil, False);
    SetElementNativeValues(ms, 'BNAM - Original Material', om);
    SetElementNativeValues(ms, 'SNAM - Replacement Material', rm);
end;

procedure CompareMaterialSwaps(slMismatchedMaterials: TStringList; om, rm: String);
{
    Compares two material swaps and adds mismatched materials to the provided list.
}
var
    bgsmOm, bgsmRm: TwbBGSMFile;
    bgemOm, bgemRm: TwbBGEMFile;
    omScriptsPath, rmScriptsPath: string;
    bOmAutoGenerated, bRmAutoGenerated: Boolean;
begin
    om := NormalizeResourceNameFixed(om, resMaterial);
    rm := NormalizeResourceNameFixed(rm, resMaterial);
    bOmAutoGenerated := False;
    bRmAutoGenerated := False;

    if not ResourceExists(om) then begin
        if FileExists(sOutputDir + '\' + om) then begin
            omScriptsPath := sOutputDir + '\' + om;
            bOmAutoGenerated := True;
        end else begin
            AddMessage('Error: ' + om + ' does not exist.');
            Exit;
        end;
    end;
    if not ResourceExists(rm) then begin
        if FileExists(sOutputDir + '\' + rm) then begin
            rmScriptsPath := sOutputDir + '\' + rm;
            bRmAutoGenerated := True;
        end else begin
            AddMessage('Error: ' + rm + ' does not exist.');
            Exit;
        end;
    end;
    bgsmOm := TwbBGSMFile.Create;
    bgsmRm := TwbBGSMFile.Create;
    bgemOm := TwbBGEMFile.Create;
    bgemRm := TwbBGEMFile.Create;
    try
        if RightStr(om, 5) = '.bgsm' then begin
            if not bOmAutoGenerated then bgsmOm.LoadFromResource(om) else bgsmOm.LoadFromFile(omScriptsPath);
            if not bRmAutoGenerated then bgsmRm.LoadFromResource(rm) else bgsmRm.LoadFromFile(rmScriptsPath);

            if bgsmOm.NativeValues['TwoSided'] <> bgsmRm.NativeValues['TwoSided'] then begin
                if bgsmOm.EditValues['TwoSided'] = 'yes' then begin
                    if not bMakeMissingMaterials then slMismatchedMaterials.Add('Warning: ' + om + #9 + ' is Two Sided' + #13#10 + #9 + rm + ' should also be Two Sided, but it is not.')
                    else begin
                        bgsmRm.NativeValues['TwoSided'] := bgsmOm.NativeValues['TwoSided'];
                        EnsureDirectoryExists(sOutputDir + '\' + ExtractFilePath(rm));
                        bgsmRm.SaveToFile(sOutputDir + '\' + rm);
                    end;
                end;
            end;
            if bgsmOm.NativeValues['UOffset'] <> bgsmRm.NativeValues['UOffset'] then begin
                if om <> 'materials\lod\setdressing\signage\billboardsmtall02.bgsm' then slMismatchedMaterials.Add('Warning: ' + om + #9 + ' has a different UOffset from ' + #13#10 + #9 + rm);
            end;
            if bgsmOm.NativeValues['VOffset'] <> bgsmRm.NativeValues['VOffset'] then begin
                slMismatchedMaterials.Add('Warning: ' + om + #9 + ' has a different VOffset from ' + #13#10 + #9 + rm);
            end;
            if bgsmOm.NativeValues['UScale'] <> bgsmRm.NativeValues['UScale'] then begin
                slMismatchedMaterials.Add('Warning: ' + om + #9 + ' has a different UScale from ' + #13#10 + #9 + rm);
            end;
            if bgsmOm.NativeValues['VScale'] <> bgsmRm.NativeValues['VScale'] then begin
                slMismatchedMaterials.Add('Warning: ' + om + #9 + ' has a different VScale from ' + #13#10 + #9 + rm);
            end;
        end
        else if RightStr(om, 5) = '.bgem' then begin
            bgemOm.LoadFromResource(om);
            bgemRm.LoadFromResource(rm);
            if bgemOm.NativeValues['TwoSided'] <> bgemRm.NativeValues['TwoSided'] then begin
                if bgsmOm.EditValues['TwoSided'] = 'yes' then slMismatchedMaterials.Add('Warning: ' + om + #9 + ' is Two Sided' + #13#10 + #9 + rm + ' should also be Two Sided, but it is not.');
            end;
            if bgemOm.NativeValues['UOffset'] <> bgemRm.NativeValues['UOffset'] then begin
                slMismatchedMaterials.Add('Warning: ' + om + #9 + ' has a different UOffset from ' + #13#10 + #9 + rm);
            end;
            if bgemOm.NativeValues['VOffset'] <> bgemRm.NativeValues['VOffset'] then begin
                slMismatchedMaterials.Add('Warning: ' + om + #9 + ' has a different VOffset from ' + #13#10 + #9 + rm);
            end;
            if bgemOm.NativeValues['UScale'] <> bgemRm.NativeValues['UScale'] then begin
                slMismatchedMaterials.Add('Warning: ' + om + #9 + ' has a different UScale from ' + #13#10 + #9 + rm);
            end;
            if bgemOm.NativeValues['VScale'] <> bgemRm.NativeValues['VScale'] then begin
                slMismatchedMaterials.Add('Warning: ' + om + #9 + ' has a different VScale from ' + #13#10 + #9 + rm);
            end;
        end;
    finally
        bgsmOm.Free;
        bgsmRm.Free;
        bgemOm.Free;
        bgemRm.Free;
    end;
end;

procedure AssignLODModelsList;
{
    Assign lod models to STAT records.
}
var
    i, cnt: integer;
    HasLOD, bHasEnableParent, bHasSCOLNeedingLOD: Boolean;
    ms, s: IwbElement;
    joLOD: TJsonObject;
    sMissingLodMessage: string;
begin
    for i := 0 to Pred(tlStats.Count) do begin
        bHasEnableParent := False;
        bHasSCOLNeedingLOD := False;
        sMissingLodMessage := '';
        s := ObjectToElement(tlStats[i]);
        joLOD := TJsonObject.Create;
        try
            HasLOD := AssignLODModels(s, joLOD, sMissingLodMessage);

            //Add lod change if we are removing lod from it.
            if ((joLOD.Count > 0) and (joLOD['hasdistantlod'] = 0)) then AssignLODToStat(s, joLOD, False, 'Overrides');

            //List relevant material swaps
            if HasLOD then begin
                slHasLOD.Add(RecordFormIdFileId(s));
                cnt := ProcessReferences(s, bHasEnableParent, bHasSCOLNeedingLOD);

                //check for base material swap
                if (cnt > 0) and (ElementExists(s, 'Model\MODS - Material Swap')) then begin
                    ms := LinksTo(ElementByPath(s, 'Model\MODS'));
                    if tlMswp.IndexOf(ms) = -1 then tlMswp.Add(ms);
                end;
                if ((cnt > 0) and (joLOD.Count > 0)) then begin
                    AssignLODToStat(s, joLOD, false, 'Overrides');
                    // Stripping this idea, since it more than doubles the amount of time required to do this, defeating the purpose of this idea.
                    // if bHasEnableParent and (not bHasSCOLNeedingLOD) then begin
                    //     tlActiFurnMstt.Add(s);
                    //     joLOD.S['level0'] := '';
                    //     joLOD.S['level1'] := '';
                    //     joLOD.S['level2'] := '';
                    //     joLOD.S['level3'] := '';
                    //     joLOD.I['hasdistantlod'] := 0;
                    //     AssignLODToStat(s, joLOD, True);
                    // end
                    // else begin
                    //     slHasLOD.Add(RecordFormIdFileId(s));
                    //     AssignLODToStat(s, joLOD, false);
                    // end;
                end;
            end
            else if bReportMissingLOD and (sMissingLodMessage <> '') then begin
                if IsObjectUsedInExterior(s) then slMissingLODMessages.Add(sMissingLodMessage);
            end;
        finally
            joLOD.Free;
        end;
    end;
end;

function IsObjectUsedInExterior(s: IInterface): boolean;
var
    si, cnt: integer;
    r, rCell, rWrld: IInterface;
begin
    cnt := 0;

    for si := Pred(ReferencedByCount(s)) downto 0 do begin
        r := ReferencedByIndex(s, si);
        if Signature(r) = 'SCOL' then begin
            if not IsObjectUsedInExterior(r) then continue;
            cnt := cnt + 1;
            break;
        end;
        if Signature(r) <> 'REFR' then continue;
        if not IsWinningOverride(r) then continue;
        if GetIsDeleted(r) then continue;
        rCell := LinksTo(ElementByIndex(r, 0));
        if IsInteriorCell(rCell) then continue;
        rWrld := WinningOverride(LinksTo(ElementByIndex(rCell, 0)));
        if IsWorldIgnored(rWrld) then continue;

        // Check if WRLD inherits LOD from another worldspace
        if WorldInheritsLOD(rWrld) then continue;

        //This reference should get lod if it passes these checks.
        cnt := cnt + 1;
        break;
    end;
    if cnt > 0 then Result := True else Result := False;
end;

function ProcessReferences(s: IwbElement; var bHasEnableParent: Boolean; var bHasSCOLNeedingLOD: Boolean): integer;
var
    si, cnt: integer;
    ms, r, rCell, rWrld, xesp, parentRef, n, base: IwbElement;
    parent, wrldEdid, cellX, cellY, recordId: string;
begin
    cnt := 0;
    for si := Pred(ReferencedByCount(s)) downto 0 do begin
        //r is not automatically a REFR.
        r := ReferencedByIndex(s, si);

        //if a SCOL, process the references of it.
        if Signature(r) = 'SCOL' then begin
            if not IsWinningOverride(r) then continue;
            if GetIsDeleted(r) then continue;
            cnt := cnt + ProcessReferences(r, bHasEnableParent, bHasSCOLNeedingLOD);
            continue;
        end;
        // skip anything else that isn't a REFR
        if Signature(r) <> 'REFR' then continue;
        if not IsWinningOverride(r) then continue;
        if GetIsDeleted(r) then continue;
        if GetIsCleanDeleted(r) then continue;

        // Check if cell is in an interior cell
        rCell := WinningOverride(LinksTo(ElementByIndex(r, 0)));
        try
            if IsInteriorCell(rCell) then continue;
        except
            AddMessage('Skipped problem record: '+ GetFileName(rCell) + #9 + Name(rCell));
            continue;
        end;

        // Check if WRLD is in the ignore list
        rWrld := WinningOverride(LinksTo(ElementByIndex(rCell, 0)));
        if IsWorldIgnored(rWrld) then continue;

        // Check if WRLD inherits LOD from another worldspace
        if WorldInheritsLOD(rWrld) then continue;

        wrldEdid := GetElementEditValues(rWrld, 'EDID');
        cellX := GetElementEditValues(rCell, 'XCLC\X');
        cellY := GetElementEditValues(rCell, 'XCLC\Y');
        recordId := RecordFormIdFileId(r);

        // Increment count if you made it this far.
        cnt := cnt + 1;

        // Add any material swap to list of used material swaps to check.
        if ElementExists(r, 'XMSP - Material Swap') then begin
            ms := LinksTo(ElementByPath(r, 'XMSP'));
            if tlMswp.IndexOf(ms) = -1 then tlMswp.Add(ms);
        end;

        // check for enable parent
        if ElementExists(r, 'XESP - Enable Parent') then begin
            parent := GetElementEditValues(r, 'XESP\Reference');
            xesp := ElementByPath(r, 'XESP');
            parentRef := WinningOverride(LinksTo(ElementByIndex(xesp, 0)));
            bHasEnableParent := True;
            if tlEnableParents.IndexOf(parentRef) = -1 then tlEnableParents.Add(parentRef);
        end;

        base := WinningOverride(LinksTo(ElementByPath(r, 'NAME')));

        if ((Signature(base) <> 'SCOL') and (GetElementEditValues(r, 'Record Header\Record Flags\Is Full LOD') <> '0')) then begin
            if ElementExists(r, 'XATR') then begin
                //When a STAT has a XATR, it means that STAT is going to move according to the object it is attached to.
                //Is Full LOD is able to do this. Object LOD is not. We need to prevent the object from having object LOD,
                //and preserve the Is FullLOD Flag.
                //
                //Setting it to be MultiRefLOD of this ref, that always has Object LOD, effectively removes it from Object LOD.
                //[REFR:00187BF3] (Places CapitolDome01 [STAT:00187CB7] in VaultTecOfficeExt02 [CELL:0000E07A] (in Commonwealth "Commonwealth" [WRLD:0000003C] at 3,-3) in Precombined\0000E07A_0D73777F_OC.nif)

                iCurrentPlugin := CanOverrideDeterminesPlugin(r, iFolipMasterFile);
                iCurrentPlugin := RefMastersDeterminePlugin(r, iCurrentPlugin);
                iCurrentPlugin := RefMastersDeterminePlugin(GetHighestPossibleOverrideForFile(rWrld, iCurrentPlugin), iCurrentPlugin);
                iCurrentPlugin := RefMastersDeterminePlugin(GetHighestPossibleOverrideForFile(rCell, iCurrentPlugin), iCurrentPlugin);

                pluginFileNameHere := GetFileName(iCurrentPlugin);
                joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['RemoveLinkedReference'] := '00195411';
                joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['AddLinkedReference'] := '00195411|00187BF3';
                if GetIsPersistent(r) then joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['Set Is Persistent'] := 1;

                joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].A['AddRefToMyFormlist'].Add(tlFlst.IndexOf(flMultiRefLOD));
            end else begin
                //Remove Is Full LOD flag from objects that will have Object LOD added.
                iCurrentPlugin := CanOverrideDeterminesPlugin(r, iFolipMasterFile);
                iCurrentPlugin := RefMastersDeterminePlugin(r, iCurrentPlugin);
                iCurrentPlugin := RefMastersDeterminePlugin(GetHighestPossibleOverrideForFile(rWrld, iCurrentPlugin), iCurrentPlugin);
                iCurrentPlugin := RefMastersDeterminePlugin(GetHighestPossibleOverrideForFile(rCell, iCurrentPlugin), iCurrentPlugin);

                pluginFileNameHere := GetFileName(iCurrentPlugin);
                joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['Is Full LOD'] := 0;
                joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].A['AddRefToMyFormlist'].Add(tlFlst.IndexOf(flRemoveIsFullLOD));
                if GetIsPersistent(r) then joElements.O['references'].O[pluginFileNameHere].O[wrldEdid].O[cellX].O[cellY].O['Overrides'].O[recordId].S['Set Is Persistent'] := 1;
            end;
        end;
    end;
    if (Signature(s) = 'SCOL') and (cnt > 0) then begin
        bHasSCOLNeedingLOD := true;
        if (slHasLOD.IndexOf(RecordFormIdFileId(s)) = -1) then slHasLOD.Add(RecordFormIdFileId(s));
        //check for base material swap on the SCOL record
        if ElementExists(r, 'Model\MODS - Material Swap') then begin
            ms := LinksTo(ElementByPath(r, 'Model\MODS'));
            if tlMswp.IndexOf(ms) = -1 then tlMswp.Add(ms);
        end;
    end;
    Result := cnt;
end;

procedure AssignLODToStat(var s: IwbElement; const joLOD: TJsonObject; const bAddHasDistantLOD: Boolean; const OverOrNew: string);
var
    n: IwbElement;
    recordId: string;
begin
    //AddMessage(ShortName(s) + #9 + joLOD.S['level0'] + #9 + joLOD.S['level1'] + #9 + joLOD.S['level2']);
    if SameText(OverOrNew, 'Overrides') then iCurrentPlugin := CanOverrideDeterminesPlugin(s, iFolipMasterFile);
    iCurrentPlugin := RefMastersDeterminePlugin(s, iCurrentPlugin);
    recordId := RecordFormIdFileId(s);

    joElements.O['STAT'].O[OverOrNew].O[recordId].S['File'] := GetFileName(iCurrentPlugin);
    joElements.O['STAT'].O[OverOrNew].O[recordId].S['Has Distant LOD'] := joLOD.I['hasdistantlod'];
    joElements.O['STAT'].O[OverOrNew].O[recordId].S['Level 0'] := joLOD.S['level0'];
    joElements.O['STAT'].O[OverOrNew].O[recordId].S['Level 1'] := joLOD.S['level1'];
    joElements.O['STAT'].O[OverOrNew].O[recordId].S['Level 2'] := joLOD.S['level2'];
    joElements.O['STAT'].O[OverOrNew].O[recordId].S['Level 3'] := joLOD.S['level3'];

    {
    n := wbCopyElementToFile(s, iCurrentPlugin, False, True);

    if bAddHasDistantLOD then SetElementNativeValues(n, 'Record Header\Record Flags\Has Distant LOD', joLOD.I['hasdistantlod'])
    else SetElementNativeValues(n, 'Record Header\Record Flags\Has Distant LOD', GetElementNativeValues(MasterOrSelf(s), 'Record Header\Record Flags\Has Distant LOD'));
    Add(n, 'MNAM', True);
    if ElementExists(n, 'MNAM\Level 0') then begin
        SetElementNativeValues(n, 'MNAM\Level 0', joLOD.S['level0']);
        SetElementNativeValues(n, 'MNAM\Level 1', joLOD.S['level1']);
        SetElementNativeValues(n, 'MNAM\Level 2', joLOD.S['level2']);
        SetElementNativeValues(n, 'MNAM\Level 3', joLOD.S['level3']);
    end else begin
        SetElementNativeValues(n, 'MNAM\LOD #0 (Level 0)\Mesh', joLOD.S['level0']);
        SetElementNativeValues(n, 'MNAM\LOD #1 (Level 1)\Mesh', joLOD.S['level1']);
        SetElementNativeValues(n, 'MNAM\LOD #2 (Level 2)\Mesh', joLOD.S['level2']);
        SetElementNativeValues(n, 'MNAM\LOD #3 (Level 3)\Mesh', joLOD.S['level3']);
    end;}

    // if Equals(GetFile(s), GetFile(n)) then Exit;
    // if GetElementEditValues(s, 'MNAM\LOD #0 (Level 0)\Mesh') <> GetElementEditValues(n, 'MNAM\LOD #0 (Level 0)\Mesh') then Exit;
    // if GetElementEditValues(s, 'MNAM\LOD #1 (Level 1)\Mesh') <> GetElementEditValues(n, 'MNAM\LOD #1 (Level 1)\Mesh') then Exit;
    // if GetElementEditValues(s, 'MNAM\LOD #2 (Level 2)\Mesh') <> GetElementEditValues(n, 'MNAM\LOD #2 (Level 2)\Mesh') then Exit;
    // if GetElementEditValues(s, 'MNAM\LOD #3 (Level 3)\Mesh') <> GetElementEditValues(n, 'MNAM\LOD #3 (Level 3)\Mesh') then Exit;
    // if GetElementEditValues(s, 'Record Header\Record Flags\Has Distant LOD') <> GetElementEditValues(n, 'Record Header\Record Flags\Has Distant LOD') then Exit;
    // AddMessage('Removing ITM: ' + ShortName(s));
    // Remove(n);
end;

procedure CollectRecords;
{
    Use this to collect all the record types you need to process over.
    It will iterate over all files in the load order regardless of what is selected in xEdit.

    This should be faster than iterating over all elements selected in the interface,
    since it only will process the specified record groups.
}
var
    i, j, idx, blockidx, subblockidx, cellidx: integer;
    recordId, wrldEdid, cellX, cellY: string;
    r, block, subblock, rCell, rWrld: IwbElement;
    f: IwbFile;
    g, wrldgroup: IwbGroupRecord;
begin
    //Iterate over all files.
    for i := 0 to Pred(FileCount) do begin
        f := FileByIndex(i);
        if i = 0 then begin
            g := GroupBySignature(f, 'TXST');
            for j := 0 to Pred(ElementCount(g)) do begin
                r := ElementByIndex(g, j);
                if ReferencedByCount(r) = 0 then continue;
                tlTxst.Add(r);
            end;
        end;

        //STAT
        g := GroupBySignature(f, 'STAT');
        for j := 0 to Pred(ElementCount(g)) do begin
            r := ElementByIndex(g, j);
            if not IsWinningOverride(r) then continue;
            if ReferencedByCount(r) = 0 then continue;
            tlStats.Add(r);
        end;

        //MSTT
        g := GroupBySignature(f, 'MSTT');
        for j := 0 to Pred(ElementCount(g)) do begin
            r := ElementByIndex(g, j);
            if not IsWinningOverride(r) then continue;
            if ReferencedByCount(r) = 0 then continue;
            tlActiFurnMstt.Add(r);
        end;

        //FURN
        g := GroupBySignature(f, 'FURN');
        for j := 0 to Pred(ElementCount(g)) do begin
            r := ElementByIndex(g, j);
            if not IsWinningOverride(r) then continue;
            if ReferencedByCount(r) = 0 then continue;
            tlActiFurnMstt.Add(r);
        end;

        //ACTI
        g := GroupBySignature(f, 'ACTI');
        for j := 0 to Pred(ElementCount(g)) do begin
            r := ElementByIndex(g, j);
            if not IsWinningOverride(r) then continue;
            if ReferencedByCount(r) = 0 then continue;
            tlActiFurnMstt.Add(r);
        end;

        //DOOR
        g := GroupBySignature(f, 'DOOR');
        for j := 0 to Pred(ElementCount(g)) do begin
            r := ElementByIndex(g, j);
            if not IsWinningOverride(r) then continue;
            if ReferencedByCount(r) = 0 then continue;
            tlActiFurnMstt.Add(r);
        end;

        {
        //CONT
        g := GroupBySignature(f, 'CONT');
        for j := 0 to Pred(ElementCount(g)) do begin
            r := ElementByIndex(g, j);
            if not IsWinningOverride(r) then continue;
            tlActiFurnMstt.Add(r);
        end;


        //FLOR
        g := GroupBySignature(f, 'FLOR');
        for j := 0 to Pred(ElementCount(g)) do begin
            r := ElementByIndex(g, j);
            if not IsWinningOverride(r) then continue;
            tlActiFurnMstt.Add(r);
        end;

        //MISC
        g := GroupBySignature(f, 'MISC');
        for j := 0 to Pred(ElementCount(g)) do begin
            r := ElementByIndex(g, j);
            if not IsWinningOverride(r) then continue;
            tlActiFurnMstt.Add(r);
        end;
        }

        g := GroupBySignature(f, 'WRLD');
        for j := 0 to Pred(ElementCount(g)) do begin
            rWrld := ElementByIndex(g, j);
            recordId := RecordFormIdFileId(rWrld);
            if Pos(recordId, sIgnoredWorldspaces) <> 0 then continue;

            wrldEdid := GetElementEditValues(rWrld, 'EDID');
            joWinningCells.O[wrldEdid].S['RecordID'] := recordId;
            wrldgroup := ChildGroup(rWrld);

            for blockidx := 0 to Pred(ElementCount(wrldgroup)) do begin
                block := ElementByIndex(wrldgroup, blockidx);
                if Signature(block) = 'CELL' then begin
                    //Found persistent worldspace cell
                    rCell := block;
                    if not IsWinningOverride(rCell) then continue;
                    joWinningCells.O[wrldEdid].O['PersistentWorldspaceCell'].S['RecordID'] := RecordFormIdFileId(rCell);
                    continue;
                end;
                for subblockidx := 0 to Pred(ElementCount(block)) do begin
                    subblock := ElementByIndex(block, subblockidx);
                    for cellidx := 0 to Pred(ElementCount(subblock)) do begin
                        rCell := ElementByIndex(subblock, cellidx);
                        if (Signature(rCell) <> 'CELL') then continue;
                        if not IsWinningOverride(rCell) then continue;
                        cellX := GetElementNativeValues(rCell, 'XCLC\X');
                        cellY := GetElementNativeValues(rCell, 'XCLC\Y');

                        joWinningCells.O[wrldEdid].O[cellX].O[cellY].S['RecordID'] := RecordFormIdFileId(rCell);
                    end;
                end;
            end;
        end;
    end;
    AddMessage('Found ' + IntToStr(tlStats.Count) + ' STAT records.');
    AddMessage('Found ' + IntToStr(tlActiFurnMstt.Count) + ' ACTI, DOOR, FURN, and MSTT records.');
end;

procedure FilesInContainers(containers: TStringList);
{
    Retrieves the ba2 packed files.
}
var
    slArchivedFiles, slVanilla, slModded: TStringList;
    i, total: integer;
    f, fNoLod, archive, tp, fileNameHere, fileNameStripped: string;
    bRasterized: boolean;
begin
    slArchivedFiles := TStringList.Create;
    slArchivedFiles.Sorted := True;
    slArchivedFiles.Duplicates := dupIgnore;
    slVanilla := TStringList.Create;
    slModded := TStringList.Create;
    try
        for i := 0 to Pred(containers.Count) do begin
            archive := TrimRightChars(containers[i], Length(wbDataPath));
            if ContainsText(archive, ' - Animations.ba2') then continue;
            if ContainsText(archive, ' - Interface.ba2') then continue;
            if ContainsText(archive, ' - MeshesExtra.ba2') then continue;
            if ContainsText(archive, ' - Nvflex.ba2') then continue;
            if ContainsText(archive, ' - Shaders.ba2') then continue;
            if ContainsText(archive, ' - Sounds.ba2') then continue;
            if ContainsText(archive, ' - Startup.ba2') then continue;
            if ContainsText(archive, ' - Textures.ba2') then continue;
            if ContainsText(archive, ' - Textures1.ba2') then continue;
            if ContainsText(archive, ' - Textures2.ba2') then continue;
            if ContainsText(archive, ' - Textures3.ba2') then continue;
            if ContainsText(archive, ' - Textures4.ba2') then continue;
            if ContainsText(archive, ' - Textures5.ba2') then continue;
            if ContainsText(archive, ' - Textures6.ba2') then continue;
            if ContainsText(archive, ' - Textures7.ba2') then continue;
            if ContainsText(archive, ' - Textures8.ba2') then continue;
            if ContainsText(archive, ' - Textures9.ba2') then continue;
            if ContainsText(archive, ' - Voices.ba2') then continue;
            if ContainsText(archive, ' - Voices_cn.ba2') then continue;
            if ContainsText(archive, ' - Voices_de.ba2') then continue;
            if ContainsText(archive, ' - Voices_en.ba2') then continue;
            if ContainsText(archive, ' - Voices_es.ba2') then continue;
            if ContainsText(archive, ' - Voices_esmx.ba2') then continue;
            if ContainsText(archive, ' - Voices_fr.ba2') then continue;
            if ContainsText(archive, ' - Voices_it.ba2') then continue;
            if ContainsText(archive, ' - Voices_ja.ba2') then continue;
            if ContainsText(archive, ' - Voices_pl.ba2') then continue;
            if ContainsText(archive, ' - Voices_ptbr.ba2') then continue;
            if ContainsText(archive, ' - Voices_ru.ba2') then continue;
            if archive <> '' then AddMessage('Loaded archive: ' + archive);

            //Check if the archive is a vanilla archive.
            if ContainsText(archive, 'DLCCoast - Main.ba2') or
            ContainsText(archive, 'DLCNukaWorld - Main.ba2') or
            ContainsText(archive, 'DLCworkshop01 - Main.ba2') or
            ContainsText(archive, 'DLCworkshop02 - Main.ba2') or
            ContainsText(archive, 'DLCworkshop03 - Main.ba2') or
            ContainsText(archive, 'Fallout4 - Materials.ba2') then ResourceList(containers[i], slVanilla)
            else ResourceList(containers[i], slModded);

            //Add all files in the archive to the list of archived files.
            ResourceList(containers[i], slArchivedFiles);
        end;
        for i := 0 to Pred(slVanilla.Count) do begin
            slVanilla[i] := LowerCase(slVanilla[i]);
        end;
        for i := 0 to Pred(slModded.Count) do begin
            slModded[i] := LowerCase(slModded[i]);
        end;

        AddMessage('Please wait while we detect all LOD assets...');

        total := slArchivedFiles.Count;
        for i := 0 to Pred(total) do begin
            f := slArchivedFiles[i];
            bRasterized := false;
            try
                //materials or meshes
                if IsInLODDir(f, 'materials') then begin
                    slMatFiles.Add(LowerCase(f));

                    fNoLod := StringReplace(f, '\lod\', '\', [rfReplaceAll, rfIgnoreCase]);
                    if (slVanilla.IndexOf(LowerCase(fNoLod)) > -1) and (slModded.IndexOf(LowerCase(fNoLod)) > -1) then begin
                        if ((not ContainsText(LowerCase(fNoLod),'materials\architecture\shacks\shacklod01.bgsm')) and bMakeMissingMaterials) then CompareModdedMaterialToVanilla(fNoLod, f);
                        //since we opened the files in CompareModdedMaterialToVanilla we took advantage and checked for rasterizing grayscale to palette then.
                        bRasterized := true;
                    end;
                    if (bMakeMissingMaterials and (not bRasterized)) then CheckIfGrayscaleToPaletteMaterial(fNoLod, f);

                    if not bReportNonLODMaterials then continue;
                    if MatHasNonLodTexture(f, tp) then AddMessage('Warning: ' + f + ' appears to be using a non-LOD texture.' + #13#10 + #9 + tp);
                end
                else if IsInLODDir(f, 'meshes') and IsLODResourceModel(f) then begin
                    slNifFiles.Add(LowerCase(f));
                    if bDeepScan then begin
                        //need to strip mesh to be base file name. strip the lod suffix from it. then map that to the path
                        fileNameHere := ExtractFileName(f);
                        fileNameStripped := StripLODSuffix(fileNameHere);
                        if ContainsText(fileNameHere, '_lod_0.nif') then
                            joModelMatch.O[fileNameStripped].S['lod0'] := TrimRightChars(LowerCase(f), 7)
                        else if ContainsText(fileNameHere, '_lod_1.nif') then
                            joModelMatch.O[fileNameStripped].S['lod1'] := TrimRightChars(LowerCase(f), 7)
                        else if ContainsText(fileNameHere, '_lod_2.nif') then
                            joModelMatch.O[fileNameStripped].S['lod2'] := TrimRightChars(LowerCase(f), 7)
                        else if ContainsText(fileNameHere, '_lod_3.nif') then
                            joModelMatch.O[fileNameStripped].S['lod3'] := TrimRightChars(LowerCase(f), 7)
                    end;
                end;
            except on E: Exception do AddMessage(#9 + 'Error processing file ' + f + #9 + E.Message);
            end;

            if i mod 10000 = 0 then begin
                AddMessage('Processed ' + IntToStr(i) + ' of ' + IntToStr(total) + ' files.');
            end;
        end;
    finally
        slVanilla.Free;
        slModded.Free;
        slArchivedFiles.Free;
    end;
end;

function StripLODSuffix(fileName: string): string;
{
    Strips the LOD suffix from a file name. For example, if the file name is "example_lod_0.nif", it will return "example.nif".
}
begin
    Result := fileName;
    Result := StringReplace(Result, '_lod_0.nif', '.nif', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '_lod_1.nif', '.nif', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '_lod_2.nif', '.nif', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '_lod_3.nif', '.nif', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '_lod.nif', '.nif', [rfReplaceAll, rfIgnoreCase]);
end;


function FetchVanillaContainer(f: string): string;
{
    Fetches the vanilla container for a given file.
}
var
    i: integer;
    slCurrentContainers, slVanillaContainers: TStringList;
    currentContainer: string;
begin
    Result := '';
    slCurrentContainers := TStringList.Create;
    slVanillaContainers := TStringList.Create;
    try
        slVanillaContainers.Add(wbDataPath + 'Fallout4 - Materials.ba2');
        slVanillaContainers.Add(wbDataPath + 'DLCworkshop01 - Main.ba2');
        slVanillaContainers.Add(wbDataPath + 'DLCworkshop02 - Main.ba2');
        slVanillaContainers.Add(wbDataPath + 'DLCworkshop03 - Main.ba2');
        slVanillaContainers.Add(wbDataPath + 'DLCCoast - Main.ba2');
        slVanillaContainers.Add(wbDataPath + 'DLCNukaWorld - Main.ba2');
        ResourceCount(f, slCurrentContainers);
        for i := Pred(slCurrentContainers.Count) downto 0 do begin
            currentContainer := slCurrentContainers[i];
            if slVanillaContainers.IndexOf(currentContainer) > -1 then begin
                Result := currentContainer;
                break;
            end;
        end;
    finally
        slCurrentContainers.Free;
        slVanillaContainers.Free;
    end;
end;

function FetchCurrentContainer(f: string): string;
{
    Fetches the current container for a given file.
}
var
    slCurrentContainers: TStringList;
begin
    slCurrentContainers := TStringList.Create;
    try
        ResourceCount(f, slCurrentContainers);
        Result := slCurrentContainers[Pred(slCurrentContainers.Count)];
    finally
        slCurrentContainers.Free;
    end;
end;

function ExtractResourceToTempDirectory(f: string): string;
{
    Extracts the resource f to the FOLIPTempPath. Returns true if successful.
}
var
    container, folder, filename, outfile: string;
    dds: TBitmap;
begin
    Result := '';
    // dds := TBitmap.Create;
    // try
    try
        folder := ExtractFilePath(f);
        filename := ExtractFileName(f);
        outfile := FOLIPTempPath + '\' + folder + filename;
        Result := outfile;
        if not FileExists(outfile) then begin
            // if SameText(ExtractFileExt(f), '.dds') then begin
            //     wbDDSResourceToBitmap(f, dds);
            //     EnsureDirectoryExists(FOLIPTempPath + '\' + folder);
            //     outfile := TrimLeftChars(outfile, 4) + '.bmp';
            //     dds.SaveToFile(outfile);
            // end else begin
            container := FetchCurrentContainer(f);
            EnsureDirectoryExists(FOLIPTempPath + '\' + folder);
            ResourceCopy(container, f, outfile);
            //end;
        end;
    except on E: Exception do AddMessage(#9 + 'Error accessing resource ' + f + #9 + E.Message);
    end;
    // finally
    //     dds.Free;
    // end;
end;

function CompareModdedMaterialToVanilla(const f, lodMaterial: string): Boolean;
{
    Compares a modded material file to the vanilla material file.
    Returns True if the modded material is different from the vanilla material.
    f is the full material path.
    lodMaterial is the lod material path.
}
var
    i: integer;
    bgsmModded, bgsmVanilla, bgsmLod: TwbBGSMFile;
    vanillaContainer, stringToReplace, whatItShouldBe, paletteScale: string;
    bGrayscaleToPalette, bLodUsesGrayscaleToPalette: boolean;
begin
    Result := False; //Assume no differences found.
    if not ResourceExists(f) then begin
        AddMessage(#9 + 'Warning: ' + f + ' does not exist.');
        Exit;
    end;
    //Fetch vanilla container
    vanillaContainer := FetchVanillaContainer(f);
    if vanillaContainer = '' then begin
        AddMessage(#9 + 'Warning: Could not find vanilla container for ' + f);
        Exit;
    end;
    bgsmModded := TwbBGSMFile.Create;
    bgsmVanilla := TwbBGSMFile.Create;
    bgsmLod := TwbBGSMFile.Create;
    try
        try
            bgsmModded.LoadFromResource(f);
        except on E: Exception do AddMessage(#9 + 'Error loading resource ' + f + #9 + E.Message);
        end;

        try
            bgsmVanilla.LoadFromResource(vanillaContainer, f);
        except on E: Exception do AddMessage(#9 + 'Error loading vanilla resource ' + f + #9 + E.Message);
        end;

        try
            bgsmLod.LoadFromResource(lodMaterial);
        except on E: Exception do AddMessage(#9 + 'Error loading lod resource ' + lodMaterial + #9 + E.Message);
        end;

        if bgsmVanilla.NativeValues['UOffset'] <> bgsmModded.NativeValues['UOffset'] then begin
            AddMessage(#9 + 'Warning: ' + f + ' has a modified UOffset value.');
            Result := True;
        end;
        if bgsmVanilla.NativeValues['VOffset'] <> bgsmModded.NativeValues['VOffset'] then begin
            AddMessage(#9 + 'Warning: ' + f + ' has a modified VOffset value.');
            Result := True;
        end;
        if bgsmVanilla.NativeValues['UScale'] <> bgsmModded.NativeValues['UScale'] then begin
            AddMessage(#9 + 'Warning: ' + f + ' has a modified UScale value.');
            Result := True;
        end;
        if bgsmVanilla.NativeValues['VScale'] <> bgsmModded.NativeValues['VScale'] then begin
            AddMessage(#9 + 'Warning: ' + f + ' has a modified VScale value.');
            Result := True;
        end;
        if bgsmVanilla.NativeValues['AlphaTest'] <> bgsmModded.NativeValues['AlphaTest'] then begin
            AddMessage(#9 + 'Warning: ' + f + ' has a modified AlphaTest value.');
            Result := True;
        end;
        if bgsmVanilla.NativeValues['TwoSided'] <> bgsmModded.NativeValues['TwoSided'] then begin
            AddMessage(#9 + 'Warning: ' + f + ' has a modified TwoSided value.');
            Result := True;
        end;
        if bgsmVanilla.EditValues['Textures\Diffuse'] <> bgsmModded.EditValues['Textures\Diffuse'] then begin
            AddMessage(#9 + 'Warning: ' + f + ' has a modified Diffuse texture.');
            Result := True;
        end;
        if bgsmVanilla.EditValues['Textures\Normal'] <> bgsmModded.EditValues['Textures\Normal'] then begin
            AddMessage(#9 + 'Warning: ' + f + ' has a modified Normal texture.');
            Result := True;
        end;
        if bgsmVanilla.EditValues['Textures\SmoothSpec'] <> bgsmModded.EditValues['Textures\SmoothSpec'] then begin
            AddMessage(#9 + 'Warning: ' + f + ' has a modified Specular texture.');
            Result := True;
        end;
        if bgsmVanilla.EditValues['Textures\Grayscale'] <> bgsmModded.EditValues['Textures\Grayscale'] then begin
            AddMessage(#9 + 'Warning: ' + f + ' has a modified Grayscale palette texture.');
            Result := True;
        end;
        if bgsmVanilla.NativeValues['GrayscaleToPaletteScale'] <> bgsmModded.NativeValues['GrayscaleToPaletteScale'] then begin
            AddMessage(#9 + 'Warning: ' + f + ' has a modified GrayscaleToPaletteScale value.');
            Result := True;
        end;
        if bgsmVanilla.NativeValues['GrayscaleToPaletteColor'] <> bgsmModded.NativeValues['GrayscaleToPaletteColor'] then begin
            AddMessage(#9 + 'Warning: ' + f + ' has a modified GrayscaleToPaletteColor value.');
            Result := True;
        end;
        bLodUsesGrayscaleToPalette := (bgsmLOD.EditValues['GrayscaleToPaletteColor'] = 'yes');
        if joRasterizeMaterials.Contains(LODMaterial) then begin
            bLodUsesGrayscaleToPalette := StrToBool(joRasterizeMaterials.O[LODMaterial].S['GrayscaleToPaletteColor']);
        end;
        bGrayscaleToPalette := (bgsmModded.EditValues['GrayscaleToPaletteColor'] = 'yes') and (bgsmLod.EditValues['GrayscaleToPaletteColor'] = 'yes');
        if (bgsmModded.EditValues['GrayscaleToPaletteColor'] = 'yes') and not bLodUsesGrayscaleToPalette then begin
            joRasterizeMaterials.O[LODMaterial].S['Full Material'] := f;
            joRasterizeMaterials.O[LODMaterial].S['GrayscaleToPaletteColor'] := true;
            joRasterizeMaterials.O[LODMaterial].S['GrayscaleToPaletteScale'] := paletteScale;
        end;
        if bGrayscaleToPalette then Result := True;
        if Result then begin
            CreateLODMaterialReplacement(lodMaterial, lodMaterial, f, True);

            paletteScale := FloatToStr(StrToFloatDef(bgsmVanilla.EditValues['GrayscaleToPaletteScale'], 0));
            stringToReplace := '_' + paletteScale + '.bgsm';
            whatItShouldBe := StringReplace(ChangeFullToLodDirectory(f), '.bgsm', stringToReplace, [rfIgnoreCase]);
            if ((not FileExists(sOutputDir + '\' + whatItShouldBe)) and (not ResourceExists(whatItShouldBe)))
            then CreateLODMaterialReplacement(LODMaterial, whatItShouldBe, f, True);
        end;
    finally
        bgsmModded.free;
        bgsmVanilla.free;
        bgsmLod.free;
    end;
end;

procedure CheckIfGrayscaleToPaletteMaterial(const NonLODMaterial, LODMaterial: string);
{
    Checks to see
}
var
    bgsm, bgsmLOD: TwbBGSMFile;
    bGrayscaleToPalette, bLodUsesGrayscaleToPalette: boolean;
    f, paletteScale, stringToReplace, whatItShouldBe: string;
begin
    f := NonLODMaterial;
    bgsm := TwbBGSMFile.Create;
    bgsmLOD := TwbBGSMFile.Create;
    try
        try
            bgsmLOD.LoadFromResource(LODMaterial);
        except on E: Exception do AddMessage(#9 + 'Error loading resource ' + LODMaterial + #9 + E.Message);
        end;
        bLodUsesGrayscaleToPalette := (bgsmLOD.EditValues['GrayscaleToPaletteColor'] = 'yes');
        paletteScale := FloatToStr(StrToFloatDef(bgsmLOD.EditValues['GrayscaleToPaletteScale'], 0));
        if joRasterizeMaterials.Contains(LODMaterial) then begin
            bLodUsesGrayscaleToPalette := StrToBool(joRasterizeMaterials.O[LODMaterial].S['GrayscaleToPaletteColor']);
            paletteScale := Fallback(joRasterizeMaterials.O[LODMaterial].S['GrayscaleToPaletteScale'], paletteScale);
            f := joRasterizeMaterials.O[LODMaterial].S['Full Material'];
        end;
        if not bLodUsesGrayscaleToPalette then Exit;

        if not ResourceExists(f) then begin
            stringToReplace := '_' + paletteScale + '.bgsm';
            f := StringReplace(NonLODMaterial, stringToReplace, '.bgsm', [rfIgnoreCase]);
            if not ResourceExists(f) then begin
                joRasterizeMaterials.O[LODMaterial].S['Full Material'] := '';
                joRasterizeMaterials.O[LODMaterial].S['GrayscaleToPaletteColor'] := true;
                joRasterizeMaterials.O[LODMaterial].S['GrayscaleToPaletteScale'] := paletteScale;
                Exit;
            end;
        end;
        try
            bgsm.LoadFromResource(f);
        except on E: Exception do AddMessage(#9 + 'Error loading resource ' + f + #9 + E.Message);
        end;
        bGrayscaleToPalette := (bgsm.EditValues['GrayscaleToPaletteColor'] = 'yes');
        paletteScale := FloatToStr(StrToFloatDef(bgsm.EditValues['GrayscaleToPaletteScale'], paletteScale));
    finally
        bgsm.free;
    end;

    stringToReplace := '_' + paletteScale + '.bgsm';
    whatItShouldBe := StringReplace(ChangeFullToLodDirectory(f), '.bgsm', stringToReplace, [rfIgnoreCase]);

    // if bGrayscaleToPalette and not bLodUsesGrayscaleToPalette then begin
    //     joRasterizeMaterials.O[LODMaterial].S['Full Material'] := f;
    //     joRasterizeMaterials.O[LODMaterial].S['GrayscaleToPaletteColor'] := true;
    //     joRasterizeMaterials.O[LODMaterial].S['GrayscaleToPaletteScale'] := paletteScale;
    // end;
    if (bGrayscaleToPalette and bLodUsesGrayscaleToPalette) then begin
        CreateLODMaterialReplacement(LODMaterial, LODMaterial, f, True);
        if ((not FileExists(sOutputDir + '\' + whatItShouldBe)) and (not ResourceExists(whatItShouldBe)))
            then CreateLODMaterialReplacement(LODMaterial, whatItShouldBe, f, True);
    end;
end;

function MatHasNonLodTexture(const f: string; var tp: string): Boolean;
{
    Checks to see if a material file resource contains non-lod textures.
}
var
    bgsm: TwbBGSMFile;
begin
    Result := False;
    bgsm := TwbBGSMFile.Create;
    try
        bgsm.LoadFromResource(f);

        tp := bgsm.EditValues['Textures\Diffuse'];
        if not (ContainsText(tp, 'lod/') or ContainsText(tp, 'lod\')) then begin
            Result := True;
            Exit;
        end;

        tp := bgsm.EditValues['Textures\Normal'];
        if not (ContainsText(tp, 'lod/') or ContainsText(tp, 'lod\')) then begin
            Result := True;
            Exit;
        end;

        tp := bgsm.EditValues['Textures\SmoothSpec'];
        if not (ContainsText(tp, 'lod/') or ContainsText(tp, 'lod\')) then begin
            Result := True;
            Exit;
        end;
    finally
        bgsm.free;
    end;
end;

procedure LoadRules(f: string);
{
    Load LOD Rules and Material Swap Map JSON files
}
var
    sub: TJsonObject;
    c, a: integer;
    j, key: string;
begin
    //LOD Rules
    j := 'FOLIP\' + TrimLeftChars(f, 4) + ' - LODRules.json';
    if ResourceExists(j) then begin
        AddMessage('Loaded LOD Rule File: ' + j);
        sub := TJsonObject.Create;
        try
            sub.LoadFromResource(j);
            for c := 0 to Pred(sub.Count) do begin
                key := sub.Names[c];
                joRules.O[key].Assign(sub.O[key]);
            end;
        finally
            sub.Free;
        end;
    end;

    //LOD Rules
    j := 'FOLIP\' + TrimLeftChars(f, 4) + ' - RasterizeGrayscales.json';
    if ResourceExists(j) then begin
        AddMessage('Loaded Rasterize Grayscales Rule File: ' + j);
        sub := TJsonObject.Create;
        try
            sub.LoadFromResource(j);
            for c := 0 to Pred(sub.Count) do begin
                key := sub.Names[c];
                joRasterizeMaterials.O[key].Assign(sub.O[key]);
            end;
        finally
            sub.Free;
        end;
    end;
    //Material Swap Maps
    j := 'FOLIP\' + TrimLeftChars(f, 4) + ' - MaterialSwapMap.json';
    if ResourceExists(j) then begin
        AddMessage('Loaded LOD Material Swap File: ' + j);
        sub := TJsonObject.Create;
        try
            sub.LoadFromResource(j);
            for c := 0 to Pred(sub.Count) do begin
                key := sub.Names[c];
                for a := 0 to Pred(sub.A[key].Count) do joMswpMap.A[key].Add(sub.A[key].S[a]);
            end;
        finally
            sub.Free;
        end;
    end;
    //MultiRefLOD
    j := 'FOLIP\' + TrimLeftChars(f, 4) + ' - MultiRefLOD.json';
    if ResourceExists(j) then begin
        try
            sub := TJsonObject.Create;
            sub.LoadFromResource(j);
            for c := 0 to Pred(sub.Count) do begin
                key := sub.Names[c];
                joMultiRefLOD.O[key].Assign(sub.O[key]);
            end;
        finally
            sub.Free;
        end;
    end;

    //Ignore Enable Parents
    j := 'FOLIP\' + TrimLeftChars(f, 4) + ' - Ignore Enable Parents.json';
    if ResourceExists(j) then begin
        AddMessage('Loaded Ignore Enable Parents File: ' + j);
        sub := TJsonObject.Create;
        try
            sub.LoadFromResource(j);
            key := 'Ignore Enable Parents';
            for a := 0 to Pred(sub.A[key].Count) do begin
                if sEnableParentFormidExclusions = '' then
                    sEnableParentFormidExclusions := sub.A[key].S[a]
                else
                    sEnableParentFormidExclusions := sEnableParentFormidExclusions + ',' + sub.A[key].S[a];
            end;
        finally
            sub.Free;
            AddMessage('Ignored Enable Parents: ' + sEnableParentFormidExclusions);
        end;
    end;

    //Ignore Worldspaces
    j := 'FOLIP\' + TrimLeftChars(f, 4) + ' - Ignore Worldspaces.json';
    if ResourceExists(j) then begin
        AddMessage('Loaded Ignore Worldspaces File: ' + j);
        sub := TJsonObject.Create;
        try
            sub.LoadFromResource(j);
            key := 'Ignore Worldspaces';
            for a := 0 to Pred(sub.A[key].Count) do begin
                if sIgnoredWorldspaces = '' then
                    sIgnoredWorldspaces := sub.A[key].S[a]
                else
                    sIgnoredWorldspaces := sIgnoredWorldspaces + ',' + sub.A[key].S[a];
            end;
        finally
            sub.Free;
            AddMessage('Ignored Worldspaces: ' + sIgnoredWorldspaces);
        end;
    end;

    //Ignore plugins
    j := 'FOLIP\' + TrimLeftChars(f, 4) + ' - Ignore Plugins.json';
    if ResourceExists(j) then begin
        AddMessage('Loaded Ignore Plugins File: ' + j);
        sub := TJsonObject.Create;
        try
            sub.LoadFromResource(j);
            key := 'Ignore Plugins';
            for a := 0 to Pred(sub.A[key].Count) do begin
                if sIgnoredPlugins = '' then
                    sIgnoredPlugins := sub.A[key].S[a]
                else
                    sIgnoredPlugins := sIgnoredPlugins + ',' + sub.A[key].S[a];
            end;
        finally
            sub.Free;
            AddMessage('Ignored Plugins: ' + sIgnoredPlugins);
        end;
    end;

end;

function AssignLODModels(s: IInterface; joLOD: TJsonObject; var sMissingLodMessage: string): Boolean;
{
    Assigns LOD Models to Stat records.
}
var
    hasChanged, ruleOverride: Boolean;
    i, c, hasDistantLOD, xBnd, yBnd, zBnd: integer;
    n: IInterface;
    colorRemap, lod4, lod8, lod16, lod32, model, omodel, olod4, olod8, olod16, olod32, editorid: string;
    slTopPaths, slMaterialsFromFullModel: TStringList;
begin
    hasChanged := False;
    ruleOverride := False;
    olod4 := '';
    olod8 := '';
    olod16 := '';
    olod32 := '';
    hasDistantLOD := 0;

    omodel := LowerCase(GetElementEditValues(s, 'Model\MODL'));

    if LeftStr(omodel, 7) <> 'meshes\' then model := 'meshes\' + omodel else model := omodel;

    colorRemap := FloatToStr(StrToFloatDef(GetElementEditValues(s, 'Model\MODC'),'9'));
    if colorRemap = '9' then colorRemap := '' else colorRemap := '_' + colorRemap;

    //Only STAT records have these. We use this function on other signatures that don't have these fields.
    if Signature(s) = 'STAT' then begin
        if ElementExists(s, 'MNAM\Level 0') then begin
            olod4 := LowerCase(GetElementNativeValues(s, 'MNAM\Level 0'));
            olod8 := LowerCase(GetElementNativeValues(s, 'MNAM\Level 1'));
            olod16 := LowerCase(GetElementNativeValues(s, 'MNAM\Level 2'));
            olod32 := LowerCase(GetElementNativeValues(s, 'MNAM\Level 3'));
        end else begin
            olod4 := LowerCase(GetElementNativeValues(s, 'MNAM\LOD #0 (Level 0)\Mesh'));
            olod8 := LowerCase(GetElementNativeValues(s, 'MNAM\LOD #1 (Level 1)\Mesh'));
            olod16 := LowerCase(GetElementNativeValues(s, 'MNAM\LOD #2 (Level 2)\Mesh'));
            olod32 := LowerCase(GetElementNativeValues(s, 'MNAM\LOD #3 (Level 3)\Mesh'));
        end;
        if (GetElementNativeValues(s, 'Record Header\Record Flags\Has Distant LOD') <> 0) then hasDistantLOD := 1;
    end;

    editorid := LowerCase(GetElementEditValues(s, 'EDID'));

    if joRules.Contains(editorid) then begin
        lod4 := joRules.O[editorid].S['level0'];
        lod8 := joRules.O[editorid].S['level1'];
        if bForceLOD8 and (lod8 = '') and (lod4 <> '') then lod8 := lod4;
        lod16 := joRules.O[editorid].S['level2'];
        lod32 := joRules.O[editorid].S['level3'];
        if joRules.O[editorid].S['hasdistantlod'] = 'false' then ruleOverride := True;
    end
    else if ((LowerCase(RightStr(editorid, 5)) = 'nolod') and (not bIgnoreNoLOD)) then begin
        slMessages.Add(ShortName(s) + ' - Editor ID ends in "nolod", so it will be skipped.');
        Result := False;
        Exit;
    end
    else if joRules.Contains(omodel) then begin
        lod4 := joRules.O[omodel].S['level0'];
        lod8 := joRules.O[omodel].S['level1'];
        if bForceLOD8 and (lod8 = '') and (lod4 <> '') then lod8 := lod4;
        lod16 := joRules.O[omodel].S['level2'];
        lod32 := joRules.O[omodel].S['level3'];
        if joRules.O[omodel].S['hasdistantlod'] = 'false' then ruleOverride := True;
    end
    else begin
        slTopPaths := TStringList.Create;
        for i := Pred(slTopLevelModPatternPaths.Count) downto 0 do begin
            if ContainsText(model, 'meshes\' + slTopLevelModPatternPaths[i]) then slTopPaths.Add(slTopLevelModPatternPaths[i]);
        end;
        lod4 := LowerCase(LODModelForLevel(s, model, colorRemap, '0', olod4, slTopPaths));
        lod8 := LowerCase(LODModelForLevel(s, model, colorRemap, '1', olod8, slTopPaths));
        if bForceLOD8 and (lod8 = '') and (lod4 <> '') then lod8 := lod4;
        lod16 := LowerCase(LODModelForLevel(s, model, colorRemap, '2', olod16, slTopPaths));
        lod32 := LowerCase(LODModelForLevel(s, model, colorRemap, '3', olod32, slTopPaths));
        slTopPaths.Free;

        //If no lod4 model has been specified, report a list of models that would possibly benefit from having lod.
        if bReportMissingLOD and (lod4 = '') and (LowerCase(RightStr(editorid, 3)) <> 'lod') then begin
            xBnd := Abs(GetElementNativeValues(s, 'OBND\X1')) + GetElementNativeValues(s, 'OBND\X2');
            yBnd := Abs(GetElementNativeValues(s, 'OBND\Y1')) + GetElementNativeValues(s, 'OBND\Y2');
            zBnd := Abs(GetElementNativeValues(s, 'OBND\Z1')) + GetElementNativeValues(s, 'OBND\Z2');
            if (xBnd > 1000) or (yBnd > 1000) or (zBnd > 1000) then begin
                sMissingLodMessage := ShortName(s) + ' with object bounds of ' + IntToStr(xBnd) + 'x' + IntToStr(yBnd) + 'x' + IntToStr(zBnd) + ' has no lod.'
            end;
        end;
    end;

    if lod4 <> olod4 then hasChanged := True
    else if lod8 <> olod8 then hasChanged := True
    else if lod16 <> olod16 then hasChanged := True
    else if lod32 <> olod32 then hasChanged := True;
    if hasDistantLOD = 0 then begin
        if lod4 <> '' then hasDistantLOD := 1
        else if lod8 <> '' then hasDistantLOD := 1
        else if lod16 <> '' then hasDistantLOD := 1
        else if lod32 <> '' then hasDistantLOD := 1;
        // if hasDistantLOD = 1 then hasChanged := True;
    end;

    //ruleOverride means that the record should not have distant LOD.
    if ruleOverride then hasDistantLOD := 0;

    model := LowerCase(wbNormalizeResourceName(model, resMesh));
    if ((hasDistantLOD <> 0) and (slCheckedModels.IndexOf(model) = -1)) then begin
        slCheckedModels.Add(model);

        slMaterialsFromFullModel := TStringList.Create;
        slMaterialsFromFullModel.Sorted := True;
        slMaterialsFromFullModel.Duplicates := dupIgnore;
        try
            AddMessage(ShortName(s) + #9 + model + #9 + lod4 + #9 + lod8 + #9 + lod16 + #9 + lod32);
            if ((model <> '') and bReportNonLODMaterials) then AddMaterialsFromModel(model, slMaterialsFromFullModel);
            ProcessLODModel(model, lod4, colorRemap, slMaterialsFromFullModel);
            ProcessLODModel(model, lod8, colorRemap, slMaterialsFromFullModel);
            ProcessLODModel(model, lod16, colorRemap, slMaterialsFromFullModel);
            ProcessLODModel(model, lod32, colorRemap, slMaterialsFromFullModel);
        finally
            slMaterialsFromFullModel.Free;
        end;
    end;

    if hasChanged then begin
        //AddMessage(ShortName(s) + #9 + model + #9 + lod4 + #9 + lod8 + #9 + lod16 + #9 + lod32);
        joLOD.I['hasdistantlod'] := hasDistantLOD;
        joLOD.S['level0'] := lod4;
        joLOD.S['level1'] := lod8;
        joLOD.S['level2'] := lod16;
        joLOD.S['level3'] := lod32;
    end;
    if hasDistantLOD <> 0 then Result := True else Result := False;
end;

procedure ProcessLODModel(FullModel, LODModel, colorRemap: string; slMaterialsFromFullModel: TStringList);
{
    Processes a LOD model.
}
var
    c, tp: integer;
    slMaterialsFromLODModel, slTopPaths, slPossibleLODPaths, slExistingSubstitutions: TStringList;
    material: string;
begin
    if LODModel = '' then Exit;
    LODModel := wbNormalizeResourceName(LODModel, resMesh);

    slMaterialsFromLODModel := TStringList.Create;
    slMaterialsFromLODModel.Sorted := True;
    slMaterialsFromLODModel.Duplicates := dupIgnore;

    slPossibleLODPaths := TStringList.Create;
    try
        if (slUsedLODNifFiles.IndexOf(LODModel) = -1) then begin
            slUsedLODNifFiles.Add(LODModel);
            if bReportNonLODMaterials or bReportUVs then begin
                if MeshCheck(LODModel, slMaterialsFromLODModel) then slOutsideUVRange.Add('Warning: ' + LODModel + #9 + ' has UVs outside the 0 to 1 range.');
            end;
            if bReportNonLODMaterials then begin
                //Check the full models materials for Possible LOD materials and populate slPossibleLODPaths.
                for c := 0 to Pred(slMaterialsFromFullModel.Count) do begin
                    material := slMaterialsFromFullModel[c];
                    slTopPaths := TStringList.Create;
                    slExistingSubstitutions := TStringList.Create;
                    try
                        for tp := 0 to Pred(slTopLevelModPatternPaths.Count) do begin
                            if ContainsText(material, 'materials\' + slTopLevelModPatternPaths[tp]) then slTopPaths.Add(slTopLevelModPatternPaths[tp]);
                        end;
                        LODMaterial(material, colorRemap, slTopPaths, slExistingSubstitutions, slPossibleLODPaths);
                    finally
                        slTopPaths.Free;
                        slExistingSubstitutions.Free;
                    end;
                end;

                //Check the LOD materials for matches in slPossibleLODPaths and warn if any are not matches.
                for c := 0 to Pred(slMaterialsFromLODModel.Count) do begin
                    material := TrimRightChars(slMaterialsFromLODModel[c], 10);
                    if slPossibleLODPaths.IndexOf(material) = -1 then begin
                        slMismatchedFullModelToLODMaterials.Add('Warning: ' + LODModel + #9 + ' has a LOD material that does not match the known full model materials: ' + #9 + material);
                    end;
                end;
            end;
        end;
    finally
        slMaterialsFromLODModel.Free;
        slPossibleLODPaths.Free;
    end;
end;

procedure AddMaterialsFromModel(model: string; var slMaterialsFromModel: TStringList);
{
    Adds materials from a model to a string list of materials.
}
var
    i: integer;
    nif: TwbNifFile;
    block: TwbNifBlock;
    mat, modelAuto: string;
    bModelAutoGenerated: Boolean;
begin
    if model = '' then Exit;
    model := wbNormalizeResourceName(model, resMesh);
    bModelAutoGenerated := False;

    if not ResourceExists(model) then begin
        if FileExists(sOutputDir + '\' + model) then begin
            modelAuto := sOutputDir + '\' + model;
            bModelAutoGenerated := True;
        end else begin
            AddMessage(#9 + 'Error: ' + model + ' does not exist.');
            Exit;
        end;
    end;

    nif := TwbNifFile.Create;
    try
        if not bModelAutoGenerated then nif.LoadFromResource(model) else nif.LoadFromFile(modelAuto);
        for i := 0 to Pred(nif.BlocksCount) do begin
            block := nif.Blocks[i];
            if block.BlockType = 'BSLightingShaderProperty' then begin
                mat := wbNormalizeResourceName(block.EditValues['Name'], resMaterial);
                if not SameText(ExtractFileExt(mat), '.bgsm') then continue
                else slMaterialsFromModel.Add(LowerCase(mat));
            end;
        end;
    finally
        nif.Free;
    end;
end;

function MeshCheck(f: string; var slMaterialsFromModel: TStringList): Boolean;
{
    Various checks for LOD models.
    Returns True if the model has UV coordinates outside the range of 0.0 to 1.0.
}
var
    tsUV: TStrings;
    j, k, vertexCount, iTimesOutsideRange: integer;
    uv, u, v, mat, modelAuto: string;
    nif: TwbNifFile;
    arr, vertex: TdfElement;
    block, b: TwbNifBlock;
    bWasEverAbleToCheck, bIsTrishape, bModified, bModelAutoGenerated: boolean;
begin
    f := wbNormalizeResourceName(f, resMesh);
    bWasEverAbleToCheck := False;
    bModified := False;
    iTimesOutsideRange := 0;
    nif := TwbNifFile.Create;

    bModelAutoGenerated := False;
    if not ResourceExists(f) then begin
        if FileExists(sOutputDir + '\' + f) then begin
            modelAuto := sOutputDir + '\' + f;
            bModelAutoGenerated := True;
        end else begin
            AddMessage(#9 + 'Error: ' + f + ' does not exist.');
            Exit;
        end;
    end;

    Result := True;
    try
        if not bModelAutoGenerated then nif.LoadFromResource(f) else nif.LoadFromFile(modelAuto);
        // iterate over all nif blocks
        for j := 0 to Pred(nif.BlocksCount) do begin
            block := nif.Blocks[j];
            bIsTrishape := False;
            if block.BlockType = 'BSLightingShaderProperty' then begin
                mat := block.EditValues['Name'];
                if not SameText(ExtractFileExt(mat), '.bgsm') then begin
                    slMeshCheckNoMaterialSpecified.Add('Warning: ' + f + #9 + ' does not specify a material');
                end
                else begin
                    mat := wbNormalizeResourceName(mat, resMaterial);
                    if not (ResourceExists(mat) or FileExists(sOutputDir + '\' + mat)) then slMeshCheckMissingMaterials.Add('Error: ' + f + #9 + ' has a specified material that does not seem to exist: ' + #9 + mat);
                    if not ContainsText(ExtractFilePath(mat), 'lod') then slMeshCheckNonLODMaterials.Add('Warning: ' + f + #9 + ' has a specified material that is not in a LOD directory: ' + #9 + mat);
                    slMaterialsFromModel.Add(LowerCase(mat));
                end;
            end;
            if not bReportUVs then continue;
            if block.IsNiObject('BSTriShape', True) then bIsTrishape := True;
            if block.IsNiObject('BSMeshLODTriShape', True) then bIsTrishape := True;
            if not bIsTrishape then continue;
            vertexCount := block.NativeValues['Num Vertices'];
            if vertexCount < 1 then continue;
            arr := block.Elements['Vertex Data'];
            for k := 0 to Pred(vertexCount) do begin
                vertex := arr[k];
                uv := vertex.EditValues['UV'];
                if Length(uv) < 1 then break;
                bWasEverAbleToCheck := True;
                tsUV := SplitString(uv, ' ');
                u := tsUV[0];
                v := tsUV[1];
                if StrToFloatDef(u, 9) < -0.1 then iTimesOutsideRange := iTimesOutsideRange + 1;
                if StrToFloatDef(u, 9) > 1.1 then iTimesOutsideRange := iTimesOutsideRange + 1;
                if StrToFloatDef(v, 9) < -0.1 then iTimesOutsideRange := iTimesOutsideRange + 1;
                if StrToFloatDef(v, 9) > 1.1 then iTimesOutsideRange := iTimesOutsideRange + 1;
                if iTimesOutsideRange = 0 then Result := False else begin
                    Result := True;
                    Exit;
                end;
            end;
            // if block.IsNiObject('BSMeshLODTriShape', True) then begin
            //     Turn this on if you want to convert LOD meshes with BSMeshLODTriShape to BSTriShape.
            //     nif.ConvertBlock(j, 'BSTriShape');
            //     bModified := True;
            // end;
        end;
    finally
        if bModified then begin
            EnsureDirectoryExists(sOutputDir + '\' + ExtractFilePath(f));
            nif.SaveToFile(sOutputDir + '\' + f);
        end;
        nif.free;
    end;
    if not bWasEverAbleToCheck then Result := False;
end;

procedure CheckUsedLODModels;
{
    Checks all used LOD models for various issues.
    Reports missing LOD nif files, not in LOD directory, or does not follow LOD naming convention.
}
var
    i: integer;
    f: string;
    slMissingLODNifFiles, slNotInLODDirectory, slDoesNotFollowLODNamingConvention: TStringList;
begin
    slMissingLODNifFiles := TStringList.Create;
    slMissingLODNifFiles.Sorted := True;
    slMissingLODNifFiles.Duplicates := dupIgnore;

    slNotInLODDirectory := TStringList.Create;
    slNotInLODDirectory.Sorted := True;
    slNotInLODDirectory.Duplicates := dupIgnore;

    slDoesNotFollowLODNamingConvention := TStringList.Create;
    slDoesNotFollowLODNamingConvention.Sorted := True;
    slDoesNotFollowLODNamingConvention.Duplicates := dupIgnore;
    try
        for i := 0 to Pred(slUsedLODNifFiles.Count) do begin
            f := slUsedLODNifFiles[i];
            if not (ResourceExists(f) or FileExists(sOutputDir + '\' + f)) then begin
                slMissingLODNifFiles.Add('Error: ' + f + #9 + ' does not exist.');
                continue;
            end;
            if not IsInLODDir(f, 'meshes') then slNotInLODDirectory.Add('Warning: ' + f + #9 + ' is not in a LOD directory.');
            if (not IsLODResourceModel(f)) and (not SameText(RightStr(f, 7), 'lod.nif')) then slDoesNotFollowLODNamingConvention.Add('Warning: ' + f + #9 + ' does not follow LOD naming conventions.');
        end;
    finally
        AddMessage(IntToStr(i + 1) + ' of ' +  IntToStr(slUsedLODNifFiles.Count) + ' LOD models were checked.');
        ListStringsInStringList(slMismatchedFullModelToLODMaterials);
        if bReportUVs then begin
            ListStringsInStringList(slMeshCheckMissingMaterials);
            ListStringsInStringList(slMeshCheckNoMaterialSpecified);
            ListStringsInStringList(slMeshCheckNonLODMaterials);
            ListStringsInStringList(slOutsideUVRange);
        end;
        ListStringsInStringList(slMissingLODNifFiles);
        ListStringsInStringList(slNotInLODDirectory);
        ListStringsInStringList(slDoesNotFollowLODNamingConvention);

        slMissingLODNifFiles.Free;
        slNotInLODDirectory.Free;
        slDoesNotFollowLODNamingConvention.Free;
    end;
end;

function LODModelForLevel(e: IwbElement; model, colorRemap, level, original: string; slTopPaths: TStringList;): string;
{
    Given a model and level, checks to see if an LOD model exists and returns it.
}
var
    searchModel, p1, p2, p3, fileNameStripped, splitNameHere: string;
    i, c: integer;
    bColorRemap, bVerifyLodModel, bSkipVerification: Boolean;
    splitNames: TStringDynArray;
    slModelNames, slLodModelNames: TStringList;
begin
    bVerifyLodModel := False;
    bSkipVerification := False;
    fileNameStripped := LowerCase(ExtractFileName(model));
    for i := 0 to Pred(slTopPaths.Count) do begin
        // meshes\dlc01\test.nif  to  meshes\dlc01\lod\test.nif
        searchModel := StringReplace(model, 'meshes\' + slTopPaths[i], 'meshes\' + slTopPaths[i] + 'lod\', [rfReplaceAll, rfIgnoreCase]);
        // meshes\dlc01\lod\test.nif  to  meshes\dlc01\lod\test_lod. Add colorRemap as _0.5 as well.
        searchModel := TrimLeftChars(searchModel, 4);

        bColorRemap := false;
        if Length(colorRemap) > 0 then bColorRemap := true;

        if bColorRemap then begin
            //p3 is for specific color remap level lod like 'model_0.5_lod_1.nif'
            p3 := searchModel + colorRemap + '_lod_' + level + '.nif';
            if slNifFiles.IndexOf(p3) > -1 then begin
                Result := TrimRightChars(p3, 7);
                Exit;
            end;
        end;

        //p1 is for specific level lod like 'model_lod_1.nif'
        p1 := searchModel + '_lod_' + level + '.nif';
        if slNifFiles.IndexOf(p1) > -1 then begin
            if bColorRemap then begin
                if bMakeMissingMaterials then begin
                    if not AttemptCreateColorRemapLODModel(p1, p3, colorRemap)
                    then slMissingColorRemaps.Add('Warning: Possible missing color remap of ' + colorRemap + ' for model: ' + #9 + model + #9 + ShortName(e))
                    else begin
                        Result := TrimRightChars(p3, 7);
                        slNifFiles.Add(p3); // Add the newly created color remapped LOD model to the list of NIF files.
                        AddMessage(#9 + 'Successfully created missing color remap of ' + colorRemap + ' for model: ' + #9 + model + #9 + ShortName(e));
                        Exit;
                    end;
                end
                else slMissingColorRemaps.Add('Warning: Possible missing color remap of ' + colorRemap + ' for model: ' + #9 + model + #9 + ShortName(e));
            end
            else begin
                Result := TrimRightChars(p1, 7);
                Exit;
            end;
        end;

        //p2 is for non-specific level lod like 'model_lod.nif'
        p2 := searchModel + '_lod.nif';
        if ((level = '0') and (slNifFiles.IndexOf(p2) > -1)) then begin
            Result := TrimRightChars(p2, 7);
            joModelMatch.O[fileNameStripped].S['lod' + level] := Result;
            Exit;
        end;
    end;
    //No exact lod model matches were found using the expected paths and naming conventions, so now check joModelMatch to see if there is a match based on the file name.
    //This is less accurate, but may help in cases where the mod author moved the file to alternate path.

    if bDeepScan then begin
        slModelNames := TStringList.Create;
        slLodModelNames := TStringList.Create;
        try
            if (joModelMatch.O[fileNameStripped].S['lod' + level] <> '') then begin
                Result := joModelMatch.O[fileNameStripped].S['lod' + level];
                bVerifyLodModel := True;
                bSkipVerification := True;
                //GetBaseNameFromModel(model, slModelNames);
            end else begin
                if ContainsText(fileNameStripped, '_') then begin
                    splitNames := SplitString(fileNameStripped, '_');
                    for i := 0 to Pred(Length(splitNames)) do begin
                        splitNameHere := splitNames[i];
                        if (joModelMatch.O[splitNameHere].S['lod' + level] <> '') then begin
                            Result := joModelMatch.O[splitNameHere].S['lod' + level];
                            bVerifyLodModel := True;
                            GetBaseNameFromModel(model, slModelNames);
                            break;
                        end;
                    end;
                end;
                if not bVerifyLodModel then begin
                    //open the nif file and change fileNameStripped to be what the base NiNode name is. See if that improves results.
                    fileNameStripped := GetBaseNameFromModel(model, slModelNames);
                    if not ContainsText(fileNameStripped, '.nif') then fileNameStripped := fileNameStripped + '.nif';
                    if (joModelMatch.O[fileNameStripped].S['lod' + level] <> '') then begin
                        Result := joModelMatch.O[fileNameStripped].S['lod' + level];
                        bVerifyLodModel := True;
                    end else begin
                        if ContainsText(fileNameStripped, '_') then begin
                            splitNames := SplitString(fileNameStripped, '_');
                            for i := 0 to Pred(Length(splitNames)) do begin
                                splitNameHere := splitNames[i];
                                if (joModelMatch.O[splitNameHere].S['lod' + level] <> '') then begin
                                    Result := joModelMatch.O[splitNameHere].S['lod' + level];
                                    bVerifyLodModel := True;
                                    break;
                                end;
                            end;
                        end;
                    end;
                end;
            end;

            if bVerifyLodModel then begin
                if not bSkipVerification then begin
                    GetBaseNameFromModel('meshes\' + Result, slLodModelNames);
                    c := 0;
                    for i := 0 to Pred(slLodModelNames.Count) do begin
                        if slModelNames.IndexOf(slLodModelNames[i]) = -1 then continue;
                        Inc(c);
                    end;
                    if c = 0 then begin
                        Result := original;
                        Exit;
                    end;
                end;
                slVerifyLODModels.add(ShortName(e) + #9 + model + #9 + Result);
                joUserRules.O[TrimRightChars(model, 7)].S['hasdistantlod'] := 'true';
                joUserRules.O[TrimRightChars(model, 7)].S['level' + level] := Result;

                bUserRulesChanged := true;
                bSaveUserRules := true;
                Exit;
            end;
        finally
            slModelNames.Free;
            slLodModelNames.Free;
        end;
    end;

    //If you made it this far, no lod models were found using the expected paths, so return the original specified lod model in the record, if any.
    Result := original;
    if ContainsText(original, '.nif') then
        joModelMatch.O[fileNameStripped].S['lod' + level] := original;
end;

function GetBaseNameFromModel(model: string; var slModelNames: TStringList): string;
{
    Gets the base block name from a nif.
}
var
    nifFile: TwbNifFile;
    block: TwbNifBlock;
    i: integer;
begin
    Result := model;
    if not ResourceExists(model) then Exit;
    nifFile := TwbNifFile.Create;
    try
        nifFile.LoadFromResource(model);
        block := nifFile.Blocks[0];
        Result := LowerCase(block.EditValues['Name']);
        for i := 1 to Pred(nifFile.BlocksCount) do begin
            block := nifFile.Blocks[i];
            if not ContainsText(block.BlockType, 'trishape') then continue;
            slModelNames.add(LowerCase(block.EditValues['Name']));
        end;
    finally
        nifFile.Free;
    end;
end;

function AttemptCreateColorRemapLODModel(lodModelNoRemap, lodModelWithRemap, colorRemap: string): Boolean;
{
    Attempts to create a color remapped version of an existing LOD model.
}
var
    lodModelNoRemapNif: TwbNifFile;
    i: integer;
    block: TwbNifBlock;
    mat, matColorRemap, lodModelNoRemapAuto: string;
    bHadColorRemap, bModelAutoGenerated: Boolean;
begin
    Result := False;
    AddMessage('Attempting to create missing color remapped LOD Model: ' + #9 + lodModelWithRemap);
    bHadColorRemap := False;

    if not ResourceExists(lodModelNoRemap) then begin
        if FileExists(sOutputDir + '\' + lodModelNoRemap) then begin
            lodModelNoRemapAuto := sOutputDir + '\' + lodModelNoRemap;
            bModelAutoGenerated := True;
        end else begin
            AddMessage(#9 + 'Error: ' + lodModelNoRemap + ' does not exist. Color remapped LOD model creation aborted.');
            Exit;
        end;
    end;

    lodModelNoRemapNif := TwbNifFile.Create;
    try
        if not bModelAutoGenerated then lodModelNoRemapNif.LoadFromResource(lodModelNoRemap) else lodModelNoRemapNif.LoadFromFile(lodModelNoRemapAuto);
        for i := 0 to Pred(lodModelNoRemapNif.BlocksCount) do begin
            block := lodModelNoRemapNif.Blocks[i];
            if block.BlockType = 'BSLightingShaderProperty' then begin
                mat := wbNormalizeResourceName(block.EditValues['Name'], resMaterial);
                if not SameText(ExtractFileExt(mat), '.bgsm') then continue
                matColorRemap := CreateColorRemapBGSM(mat, colorRemap);
                if not SameText(matColorRemap, mat) then bHadColorRemap := True;
                block.EditValues['Name'] := matColorRemap;
            end;
        end;
        if not bHadColorRemap then begin
            AddMessage(#9 + 'Color remapped LOD model could not be created, as the base lod model does not use grayscale to palette scale for its materials.');
            Exit; //No color remap change was created, so exit without saving.
        end;
        //Save the color remapped version of the LOD model.
        EnsureDirectoryExists(sOutputDir + '\' + ExtractFilePath(lodModelWithRemap));
        lodModelNoRemapNif.SaveToFile(sOutputDir + '\' + lodModelWithRemap);
        Result := True;
    finally
        lodModelNoRemapNif.Free;
    end;
end;

function CreateColorRemapBGSM(material, colorRemap: string): string;
{
    Creates a color remapped version of a BGSM material.
    Color remap is the string to append to the material name, like '_0.5'.
    Returns the new material name.
}
var
    materialBGSM: TwbBGSMFile;
    renamedMaterial, materialAuto: string;
    bMaterialAutoGenerated: Boolean;
begin
    Result := material;

    if not ResourceExists(material) then begin
        if FileExists(sOutputDir + '\' + material) then begin
            materialAuto := sOutputDir + '\' + material;
            bMaterialAutoGenerated := True;
        end else begin
            AddMessage(#9 + 'Error: ' + material + ' does not exist.');
            Exit;
        end;
    end;

    materialBGSM := TwbBGSMFile.Create;
    try
        if not bMaterialAutoGenerated then materialBGSM.LoadFromResource(material) else materialBGSM.LoadFromFile(materialAuto);
        if materialBGSM.EditValues['GrayscaleToPaletteColor'] <> 'yes' then Exit;
        materialBGSM.EditValues['GrayscaleToPaletteScale'] := TrimRightChars(colorRemap, 1);
        renamedMaterial := TrimLeftChars(material, 5) + colorRemap + '.bgsm';
        EnsureDirectoryExists(sOutputDir + '\' + ExtractFilePath(renamedMaterial));
        materialBGSM.SaveToFile(sOutputDir + '\' + renamedMaterial);
        slMatFiles.Add(renamedMaterial); // Add the newly created color remapped material to the list of materials.
        Result := renamedMaterial;
    finally
        materialBGSM.Free;
    end;
end;

function LODMaterial(material, colorRemap: string; slTopPaths, slExistingSubstitutions, slPossibleLODPaths: TStringList): string;
{
    Checks to see if an LOD material exists and returns it.
}
var
    i, a: integer;
    searchMaterial, p1, p2, p3, p4: string;
begin
    // DLC04\Architecture\GalacticZone\MetalPanelTrimCR02.BGSM, _0.93
    Result := ChangeFullToLodDirectory(material);
    for i := 0 to Pred(slTopPaths.Count) do begin
        // materials\dlc01\test.bgsm to materials\dlc01\lod\test.bgsm
        searchMaterial := StringReplace(material, 'materials\' + slTopPaths[i], 'materials\' + slTopPaths[i] + 'lod\', [rfReplaceAll, rfIgnoreCase]);
        p1 := TrimLeftChars(searchMaterial, 5) + colorRemap + '.bgsm';
        //if Result = '' then Result := p1;
        if ((slMatFiles.IndexOf(p1) > -1) and (slExistingSubstitutions.IndexOf(p1) = -1)) then begin
            //sanity check
            //AddMessage(TrimRightChars(p1, 10));
            slPossibleLODPaths.Add(TrimRightChars(p1, 10));
        end;
        p2 := TrimLeftChars(searchMaterial, 5) + 'lod.bgsm';
        if ((slMatFiles.IndexOf(p2) > -1) and (slExistingSubstitutions.IndexOf(p2) = -1)) then slPossibleLODPaths.Add(TrimRightChars(p2, 10));
        p3 := TrimLeftChars(searchMaterial, 5) + '_lod.bgsm';
        if ((slMatFiles.IndexOf(p3) > -1) and (slExistingSubstitutions.IndexOf(p3) = -1)) then slPossibleLODPaths.Add(TrimRightChars(p3, 10));
    end;

    //Material Swap Map Rules
    searchMaterial := TrimRightChars(material,10);
    if joMswpMap.Contains(searchMaterial) then begin
        for a := 0 to Pred(joMswpMap.A[searchMaterial].Count) do begin
            p4 := joMswpMap.A[searchMaterial].S[a];
            if slExistingSubstitutions.IndexOf('materials\' + p4) = -1 then slPossibleLODPaths.Add(p4);
        end;
    end;
end;

function RefMastersDeterminePlugin(e: IwbElement; inputFile: IwbFile): IwbFile;
{
    Determines the plugin to use based on the reference's required masters.
}
var
    slMasters: TStringList;
    i: integer;
begin
    Result := inputFile;
    slMasters := TStringList.Create;
    try
        ReportRequiredMasters(e, slMasters, False, True);

        for i := 0 to Pred(slMasters.Count) do begin
            if SameText(sFolipMasterFileName, slMasters[i]) then continue;
            if (slMasterableMasters.IndexOf(slMasters[i]) = -1) then begin
                Result := iFolipPluginFile;
                break;
            end;
        end;
        for i := 0 to Pred(slMasters.Count) do begin
            if SameText(GetFileName(Result), sFolipMasterFileName) then begin
                if SameText(slMasters[i], sFolipMasterFileName) then continue;
                slMainMasters.Add(slMasters[i]);
            end else begin
                if SameText(slMasters[i], sFolipPluginFileName + '.esp') then continue;
                slPatchMasters.Add(slMasters[i]);
            end;
        end;
    finally
        slMasters.Free;
    end;
end;

procedure FetchRules;
{
    Loads the Rule JSON files.
}
var
    c, i: integer;
    f, j, key: string;
begin
    for i := 0 to Pred(FileCount) do begin
        f := GetFileName(FileByIndex(i));
        LoadRules(f);
    end;
    j := 'FOLIP\UserRules.json';
    if ResourceExists(j) then begin
        AddMessage('Loaded LOD Rule File: ' + j);
        joUserRules := TJsonObject.Create;
        joUserRules.LoadFromResource(j);
        for c := 0 to Pred(joUserRules.Count) do begin
            key := joUserRules.Names[c];
            joRules.O[key].Assign(joUserRules.O[key]);
        end;
    end;
    SortJSONObjectKeys(joRules);
end;

function CreatePlugins: Boolean;
{
    Checks the plugins to ensure required mods are not missing.
    Checks to make sure an old patch generated from this script is not present.
    Creates the plugin files we need.
}
var
    bFO4LODGen, bFolip, bMainFileFound, bMasterFile: Boolean;
    i: integer;
    fileName: string;
    f: IwbFile;
begin
    bMainFileFound := False;
    bFO4LODGen := False;
    bFolip := False;
    for i := 0 to Pred(FileCount) do begin
        f := FileByIndex(i);
        fileName := GetFileName(f);
        if fileName = 'Fallout4.exe' then continue;

        if SameText(fileName, sFolipMasterFileName) then begin
            iFolipMasterFile := FileByIndex(i);
            bMainFileFound := True;
        end else if SameText(fileName, sFolipPluginFileName + '.esp') then begin
            iFolipPluginFile := FileByIndex(i);
            bPreviousBeforeGenerationPresent := True;
        end else if SameText(fileName, sFO4LODGenFileName) then
            bFO4LODGen := True
        else if SameText(fileName, sFolipFileName) then
            bFolip := True;

        bMasterFile := GetIsESM(f);
        if ((not bMainFileFound) and bMasterFile) then slMasterableMasters.Add(fileName);
        slPluginFiles.Add(fileName);

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
    if not Assigned(iFolipMasterFile) then begin
        iFolipMasterFile := AddNewFileName(sFolipMasterFileName, False);
        AddMasterIfMissing(iFolipMasterFile, 'Fallout4.esm');
    end;
    if not Assigned(iFolipPluginFile) then begin
        iFolipPluginFile := AddNewFileName(sFolipPluginFileName + '.esp', False);
        AddMasterIfMissing(iFolipPluginFile, 'Fallout4.esm');
    end;

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
    f := LowerCase(f);
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
    parts: TStringDynArray;
    between: string;
begin
    Result := False;
    if LowerCase(LeftStr(f, Length(m))) <> LowerCase(m) then Exit;
    parts := SplitString(f, '\');
    for i := 1 to 2 do begin
        try
            if LowerCase(parts[i]) = 'lod' then begin
                Result := True;
                if i = 2 then begin
                    between := parts[1] + '\';
                    if slTopLevelModPatternPaths.IndexOf(between) = -1 then begin
                        AddMessage('Added sub path: ' + between);
                        slTopLevelModPatternPaths.Add(between);
                    end;
                end;
                Break;
            end;
        except
            Break;
        end;
    end;
end;

function ChangeFullToLodDirectory(f: string): string;
{
    Given a file path, changes the directory to the LOD directory.
    f is the file path.
}
var
    parts: TStringDynArray;
begin
    if not ContainsText(f, '\') then begin
        Result := 'lod\' + f;
        Exit;
    end;
    parts := SplitString(f, '\');
    if Length(parts) = 0 then begin
        //meshes\ to meshes\lod
        Result := StringReplace(f, parts[0], parts[0] + '\lod', [rfIgnoreCase]);
    end
    else if slTopLevelModPatternPaths.IndexOf(parts[1]) = -1 then begin
        //meshes\path\to\model.nif to meshes\lod\path\to\model.nif
        Result := StringReplace(f, parts[0], parts[0] + '\lod', [rfIgnoreCase]);
    end
    else begin
        //meshes\dlc03\path\to\model.nif to meshes\dlc03\lod\path\to\model.nif
        Result := StringReplace(f, parts[1], parts[1] + '\lod', [rfIgnoreCase]);
    end;
end;

function GetIsCleanDeleted(r: IInterface): Boolean;
{
    Checks to see if a reference has an XESP set to opposite of the PlayerRef
}
begin
    Result := False;
    if not ElementExists(r, 'XESP') then Exit;
    if (GetElementNativeValues(r, 'XESP\Flags\Set Enable State to Opposite of Parent') = 0) then Exit;
    if (GetElementEditValues(r, 'XESP\Reference') <> 'PlayerRef [PLYR:00000014]') then Exit;
    Result := True;
end;

// ----------------------------------------------------
// Generic Functions and Procedures go below.
// ----------------------------------------------------

function BoolToStr(b: boolean): string;
{
    Given a boolean, return a string.
}
begin
    if b then Result := 'true' else Result := 'false';
end;

function StrToBool(str: string): boolean;
{
    Given a string, return a boolean.
}
begin
    if (LowerCase(str) = 'true') or (str = '1') then Result := True else Result := False;
end;

function Fallback(str, fallback: string): string;
{
    Set a fallback value if blank.
}
begin
    if str = '' then Result := fallback else Result := str;
end;

function TrimRightChars(s: string; chars: integer): string;
{
    Returns right string - chars
    TrimRightChars('Example', 2) -> 'ample'
}
begin
    Result := RightStr(s, Length(s) - chars);
end;

function TrimLeftChars(s: string; chars: integer): string;
{
    Returns left string - chars
    TrimLeftChars('Example', 3) -> 'Exam'
}
begin
    Result := LeftStr(s, Length(s) - chars);
end;

function StripNonAlphanumeric(Input: string): string;
var
  i: Integer;
  c: char;
begin
    Result := '';
    i := 1;
    while i <= Length(Input) do begin
        c := Copy(Input,i,1);
        if Pos(c,'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789') <> 0 then Result := Result + c;
        inc(i);
    end;
end;

procedure EnsureDirectoryExists(f: string);
{
    Create directories if they do not exist.
}
begin
    if not DirectoryExists(f) then
        if not ForceDirectories(f) then
            raise Exception.Create('Can not create destination directory ' + f);
end;

procedure ListStringsInStringList(sl: TStringList);
{
    Given a TStringList, add a message for all items in the list.
}
var
    i, count: integer;
begin
    count := sl.Count;
    if count < 1 then Exit;
    AddMessage('=======================================================================================');
    for i := 0 to Pred(count) do AddMessage(sl[i]);
    AddMessage('=======================================================================================');
end;

function CreateLabel(aParent: TControl; x, y: Integer; aCaption: string): TLabel;
{
    Create a label.
}
begin
    Result := TLabel.Create(aParent);
    Result.Parent := aParent;
    Result.Left := x;
    Result.Top := y;
    Result.Caption := aCaption;
end;

procedure SortJSONObjectKeys(JSONObj: TJsonObject);
{
    Sorts JSON keys alphabetically.
}
var
    SortedKeys: TStringList;
    Key: string;
    NewJSONObj: TJsonObject;
    i: integer;
begin
    // Create a sorted list of keys
    SortedKeys := TStringList.Create;
    NewJSONObj := TJsonObject.Create;
    try
        for i := 0 to Pred(JSONObj.Count) do SortedKeys.Add(JSONObj.Names[i]);
        SortedKeys.Sort; // Sort the keys alphabetically

        for i := 0 to Pred(SortedKeys.Count) do begin
            Key := SortedKeys[i];
            NewJSONObj.O[Key].Assign(JSONObj.O[Key]);
        end;

        // Replace the original JSONObj with the sorted one
        JSONObj.Clear;
        JSONObj.Assign(NewJSONObj);
    finally
        SortedKeys.Free;
        NewJSONObj.Free;
    end;
end;

function CopyElementToFileWithVC(e: IwbElement; f: IwbFile): IwbElement;
{
    Copies an element (e) to a file (f), but also copies the version control data from the element being copied.
}
var
    n: IwbElement;
begin
    n := wbCopyElementToFile(e, f, False, True);
    SetFormVCS1(n, GetFormVCS1(e));
    SetFormVCS2(n, GetFormVCS2(e));
    Result := n;
end;

// function RecordFormIdFileId(e: IwbElement): string;
// {
//     Returns the record ID of an element.
// }
// begin
//     Result := TrimRightChars(IntToHex(FixedFormID(MasterOrSelf(e)), 8), 2) + ':' + GetFileName(GetFile(MasterOrSelf(e)));
// end;

function RecordFormIdFileId(e: IwbElement): string;
{
    Returns the record ID of an element.
}
begin
    e := MasterOrSelf(e);
    Result := IntToHex(FormID(e), 8) + ':' + GetFileName(GetFile(e));
end;

function GetRecordFromFormIdFileId(recordId: string): IwbElement;
{
    Returns the record from the given formid:filename.
}
var
    colonPos, recordFormId: integer;
    f: IwbFile;
begin
    colonPos := Pos(':', recordId);
    recordFormId := StrToInt('$' + Copy(recordId, 1, Pred(colonPos)));
    f := FileByName(Copy(recordId, Succ(colonPos), Length(recordId)));
    Result := RecordByFormID(f, recordFormId, False);
end;

function DeleteDirectory(dir: string): boolean;
{
    Deletes a directory and all files and subdirectories in it. Returns true if successful.
}
var
    srFind : TSearchRec;
    f: string;
begin
    Result := True;
    if not DirectoryExists(dir) then Exit;

    if FindFirst(dir + '\*', faAnyFile, srFind) = 0 then begin
        repeat
            if ((srFind.Name <> '.') and (srFind.Name <> '..')) then begin
                f := dir + '\' + srFind.Name;

                if ((srFind.attr and faDirectory) = faDirectory) then begin
                    if (not DeleteDirectory(f)) then begin
                        Result := False;
                        Exit;
                    end;
                end
                else begin
                    if (not DeleteFile(f)) then begin
                        Result := False;
                        Exit;
                    end;
                end;
            end;
        until FindNext(srFind) <> 0;

        FindClose(srFind);
    end;

    if (not RemoveDir(dir)) then begin
        Result := False;
        Exit;
    end;
end;

function NormalizeResourceNameFixed(f: string; resType: TGameResourceType): string;
{
    Normalizes a resource name to the correct format for the given resource type.
}
var
    newf: string;
begin
    newf := StringReplace(f, '*', 'THERE-WAS-AN-ASTERISK-HERE', [rfReplaceAll, rfIgnoreCase]);
    newf := wbNormalizeResourceName(newf, resType);
    newf := StringReplace(newf, 'THERE-WAS-AN-ASTERISK-HERE', '*', [rfReplaceAll, rfIgnoreCase]);
    Result := newf;
end;

function GetHighestPossibleOverrideForFile(r: IwbMainRecord; inputFile: IwbFile): IwbElement;
{
    Gets the highest possible override desired for the given reference and plugin.
}
var
    i, iNumOverrides: integer;
    PluginHereFileName, overrideFileName: string;
    o, masterRecord: IwbMainRecord;
    f: IwbFile;
begin
    PluginHereFileName := GetFileName(inputFile);
    if SameText(PluginHereFileName, sFolipPluginFileName + '.esp') then begin
        Result := WinningOverride(r);
        Exit;
    end;
    masterRecord := MasterOrSelf(r);
    iNumOverrides := OverrideCount(masterRecord);
    if iNumOverrides > 0 then begin
        for i := Pred(iNumOverrides) downto 0 do begin
            o := OverrideByIndex(masterRecord, i);
            f := GetFile(o);
            overrideFileName := GetFileName(f);
            if (slMasterableMasters.IndexOf(overrideFileName) <> -1) then begin
                Result := o;
                Exit;
            end;
        end;
        //AddMessage('Failed to find the best override: Falling back to winning override for' + #9 + RecordFormIdFileId(r) + #9 + PluginHereFileName);
    end;
    Result := WinningOverride(r);
end;

function WinningOverrideIgnoringThisFile(r: IwbMainRecord; f: string): IwbMainRecord;
{
    Given a record and filename, return the winning override ignoring the given filename if present.
}
var
    i: integer;
    m, previousOverride, ovr: IwbMainRecord;
    filename: string;
begin
    m := MasterOrSelf(r);
    if OverrideCount(m) > 0 then begin
        for i := Pred(OverrideCount(m)) downto 0 do begin
            ovr := OverrideByIndex(m, i);
            filename := GetFileName(GetFile(ovr));
            if SameText(filename, f) then continue else break;
        end;
    end else begin
        ovr := r;
    end;
    Result := ovr;
end;

function CanOverrideInMaster(r: IwbMainRecord): Boolean;
{
    Determines if a record can be fully overridden in a master file.
}
var
    fileName: string;
begin
    Result := False;
    fileName := GetFileName(GetFile(WinningOverride(r)));
    if (slMasterableMasters.IndexOf(fileName) <> -1) then Result := True;
end;

function CanOverrideDeterminesPlugin(r: IwbMainRecord; inputFile: IwbFile): IwbFile;
{
    Returns the
}
begin
    Result := inputFile;
    if SameText(GetFileName(inputFile), sFolipPluginFileName + '.esp') then Result := iFolipPluginFile;
    if CanOverrideInMaster(r) then Result := iFolipMasterFile else Result := iFolipPluginFile;
end;

procedure SortByMasterList(const slMasterList: TStringList; var slListToSort: TStringList;);
{
    Sorts the second list in the order of the first list.
}
var
    i: integer;
    temp: TStringList;
    pluginFile: string;
begin
    temp := TStringList.Create;
    try
        for i := 0 to Pred(slMasterList.Count) do begin
            pluginFile := slMasterList[i];
            if (slListToSort.IndexOf(pluginFile) <> -1) then temp.Add(pluginFile);
        end;

        slListToSort.Assign(temp);
    finally
        temp.Free;
    end;
end;

function WorldInheritsLOD(wrld: IwbMainRecord): boolean;
begin
    if ElementExists(wrld, 'Parent Worldspace\PNAM - Flags') then
        Result := (GetElementNativeValues(wrld,'Parent Worldspace\PNAM - Flags\Use LOD Data') <> 0)
    else
        Result := (GetElementNativeValues(wrld,'Parent\PNAM - PNAM\Flags\Use LOD Data') <> 0);
end;

function IsWorldIgnored(wrld: IwbMainRecord): boolean;
begin
    Result := Pos(RecordFormIdFileId(wrld), sIgnoredWorldspaces) <> 0;
end;

function IsInteriorCell(cell: IwbMainRecord): boolean;
begin
    Result := (GetElementNativeValues(cell, 'DATA - Flags\Is Interior Cell') <> 0);
end;

function IsInIgnoredPlugin(r: IwbMainRecord): boolean;
var
    f: string;
begin
    Result := False;
    f := GetFileName(GetFile(r));
    Result := (Pos(f, sIgnoredPlugins) <> 0);
end;

end.