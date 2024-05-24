{
    Collect LOD Assets for Fallout 4.
}
unit FOLIP;

//Create variables that will need to be used accross multiple functions/procedures.
var
    tlStats, tlActiFurnMstt, tlMswp, tlCells: TList;
    slNifFiles, slMatFiles, slTopLevelModPatternPaths, slMessages: TStringList;
    iFolipMasterFile, iFolipPluginFile, iCurrentPlugin: IInterface;
    i: integer;
    f: string;
    bSaveUserRules, bUserRulesChanged: Boolean;
    joRules, joMswpMap, joUserRules: TJsonObject;

    lvRules: TListView;

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
    bSkip: Boolean;
    slContainers: TStringList;
    i: integer;
begin
    bSkip := True;
    bUserRulesChanged := False;
    CreateObjects;
    FetchRules;

    bSaveUserRules := OptionForm;
    if not bSaveUserRules then Exit;

    TopLevelModPatternPaths;

    if bSkip then Exit;

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

    //Add fake statics for MSTT, FURN, and ACTI that have lod.
    ProcessActiFurnMstt;

    //Add Messages
    ListStringsInStringList(slMessages);

    //Apply lod material swaps.
    AddMessage('Assigning LOD materials to ' + IntToStr(tlMswp.Count) + ' material swaps.');
    AssignLODMaterialsList;


    //Fix bad mod lod edits.

    Result := 0;
end;

function Finalize: integer;
{
    This function is called at the end.
}
begin
    tlStats.Free;
    tlActiFurnMstt.Free;
    tlMswp.Free;
    tlCells.Free;

    slNifFiles.Free;
    slMatFiles.Free;
    slTopLevelModPatternPaths.Free;
    slMessages.Free;

    joRules.Free;
    joMswpMap.Free;

    //Save user rules
    if bSaveUserRules and bUserRulesChanged then begin
        AddMessage('Saving ' + IntToStr(joUserRules.Count) + ' user rule(s) to ' + wbDataPath + 'FOLIP\UserRules.json');
        joUserRules.SaveToFile(wbDataPath + 'FOLIP\UserRules.json', False, TEncoding.UTF8, True);
    end;

    joUserRules.Free;

    Result := 0;
end;

procedure frmOptionsFormClose(Sender: TObject; var Action: TCloseAction);
{
    Close form handler.
}
begin
    if TForm(Sender).ModalResult <> mrOk then Exit;
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
    Item.SubItems.Add(joRules.O[key].S['hasdistantlod']);
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
    hasdistantlod: Boolean;
begin
    key := '';
    hasdistantlod := True;
    lod4 := '';
    lod8 := '';
    lod16 := '';
    lod32 := '';

    if not EditRuleForm(key, lod4, lod8, lod16, lod32, hasdistantlod) then Exit;

    joRules.O[key].S['hasdistantlod'] := BoolToStr(hasdistantlod);
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
    hasdistantlod: Boolean;
begin
    if not Assigned(lvRules.Selected) then Exit;
    idx := lvRules.Selected.Index;

    key := joRules.Names[idx];
    hasdistantlod := StrToBool(joRules.O[key].S['hasdistantlod']);
    lod4 := joRules.O[key].S['level0'];
    lod8 := joRules.O[key].S['level1'];
    lod16 := joRules.O[key].S['level2'];
    lod32 := joRules.O[key].S['level3'];

    if not EditRuleForm(key, lod4, lod8, lod16, lod32, hasdistantlod) then Exit;

    joRules.O[key].S['hasdistantlod'] := BoolToStr(hasdistantlod);
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
    idx: integer;
begin
    if not Assigned(lvRules.Selected) then Exit;
    idx := lvRules.Selected.Index;
    joRules.Delete(idx);
    lvRules.Items.Count := joRules.Count;
    lvRules.Refresh;
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

procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{
    Cancel if Escape key is pressed.
}
begin
    if Key = VK_ESCAPE then TForm(Sender).ModalResult := mrCancel;
end;

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
    if str = 'true' then Result := True else Result := False;
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

function EditRuleForm(var key, lod4, lod8, lod16, lod32: string; var hasdistantlod: Boolean): Boolean;
var
    frmRule: TForm;
    pnl: TPanel;
    btnOk, btnCancel: TButton;
    edKey, edHasDistantLOD, edlod4, edlod8, edlod16, edlod32: TEdit;
    chkHasDistantLOD: TCheckBox;
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
    edKey.Width := frmRule.Width - 140;
    CreateLabel(frmRule, 16, edKey.Top + 4, 'Mesh or EditorID');

    chkHasDistantLOD := TCheckBox.Create(frmRule);
    chkHasDistantLOD.Parent := frmRule;
    chkHasDistantLOD.Left := 16;
    chkHasDistantLOD.Top := edKey.Top + 32;
    chkHasDistantLOD.Width := 200;
    chkHasDistantLOD.Caption := 'Has Distant LOD';

    edlod4 := TEdit.Create(frmRule);
    edlod4.Parent := frmRule;
    edlod4.Name := 'edlod4';
    edlod4.Left := 120;
    edlod4.Top := chkHasDistantLOD.Top + 28;
    edlod4.Width := frmRule.Width - 140;
    CreateLabel(frmRule, 16, edlod4.Top + 4, 'LOD4');

    edlod8 := TEdit.Create(frmRule);
    edlod8.Parent := frmRule;
    edlod8.Name := 'edlod8';
    edlod8.Left := 120;
    edlod8.Top := edlod4.Top + 28;
    edlod8.Width := frmRule.Width - 140;
    CreateLabel(frmRule, 16, edlod8.Top + 4, 'LOD8');

    edlod16 := TEdit.Create(frmRule);
    edlod16.Parent := frmRule;
    edlod16.Name := 'edlod16';
    edlod16.Left := 120;
    edlod16.Top := edlod8.Top + 28;
    edlod16.Width := frmRule.Width - 140;
    CreateLabel(frmRule, 16, edlod16.Top + 4, 'LOD16');

    edlod32 := TEdit.Create(frmRule);
    edlod32.Parent := frmRule;
    edlod32.Name := 'edlod32';
    edlod32.Left := 120;
    edlod32.Top := edlod16.Top + 28;
    edlod32.Width := frmRule.Width - 140;
    CreateLabel(frmRule, 16, edlod32.Top + 4, 'LOD32');

    btnOk := TButton.Create(frmRule);
    btnOk.Parent := frmRule;
    btnOk.Caption := 'OK';
    btnOk.ModalResult := mrOk;
    btnOk.Left := frmRule.Width - 176;
    btnOk.Top := frmRule.Height - 62;

    btnCancel := TButton.Create(frmRule);
    btnCancel.Parent := frmRule;
    btnCancel.Caption := 'Cancel';
    btnCancel.ModalResult := mrCancel;
    btnCancel.Left := btnOk.Left + btnOk.Width + 8;
    btnCancel.Top := btnOk.Top;

    pnl := TPanel.Create(frmRule);
    pnl.Parent := frmRule;
    pnl.Left := 8;
    pnl.Top := btnOk.Top - 12;
    pnl.Width := frmRule.Width - 20;
    pnl.Height := 2;

    edKey.Text := key;
    chkHasDistantLOD.Checked := hasdistantlod;
    edlod4.Text := lod4;
    edlod8.Text := lod8;
    edlod16.Text := lod16;
    edlod32.Text := lod32;

    if frmRule.ShowModal <> mrOk then Exit;

    key := edKey.Text;
    hasdistantlod := chkHasDistantLOD.Checked;
    lod4 := edlod4.Text;
    lod8 := edlod8.Text;
    lod16 := edlod16.Text;
    lod32 := edlod32.Text;
    Result := True;
  finally
    frmRule.Free;
  end;
end;

function OptionForm: Boolean;
var
    i: integer;
    mnRules: TPopupMenu;
    MenuItem: TMenuItem;
    lbl: TLabel;
    pnl: TPanel;
    frm: TForm;
    btnOk, btnCancel: TButton;
    gbOptions: TGroupBox;
begin
    frm := TForm.Create(nil);
    try
        frm.Caption := 'FOLIP xEdit Patcher';
        frm.Width := 1200;
        frm.Height := 600;
        frm.Position := poMainFormCenter;
        frm.BorderStyle := bsDialog;
        frm.KeyPreview := True;
        frm.OnClose := frmOptionsFormClose;
        frm.OnKeyDown := FormKeyDown;

        lvRules := TListView.Create(frm);
        lvRules.Parent := frm;
        lvRules.Left := 10;
        lvRules.Top := 24;
        lvRules.Width := frm.Width - 35;
        lvRules.Height := 420;
        lvRules.ReadOnly := True;
        lvRules.ViewStyle := vsReport;
        lvRules.RowSelect := True;
        lvRules.DoubleBuffered := True;
        lvRules.Columns.Add.Caption := 'Mesh / EditorID';
        lvRules.Columns[0].Width := 400;
        lvRules.Columns.Add.Caption := 'HasDistantLOD';
        lvRules.Columns[1].Width := 95;
        lvRules.Columns.Add.Caption := 'LOD4';
        lvRules.Columns[2].Width := 400;
        lvRules.Columns.Add.Caption := 'LOD8';
        lvRules.Columns[3].Width := 90;
        lvRules.Columns.Add.Caption := 'LOD16';
        lvRules.Columns[4].Width := 60;
        lvRules.Columns.Add.Caption := 'LOD32';
        lvRules.Columns[5].Width := 60;
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

        gbOptions := TGroupBox.Create(frm);
        gbOptions.Parent := frm;
        gbOptions.Left := lvRules.Left;
        gbOptions.Top := lvRules.Top + lvRules.Height + 10;
        gbOptions.Width := lvRules.Width;
        gbOptions.Height := 50;
        gbOptions.Caption := 'Options';

        btnOk := TButton.Create(frm);
        btnOk.Parent := frm;
        btnOk.Caption := 'OK';
        btnOk.ModalResult := mrOk;
        btnOk.Left := frm.Width - 190;
        btnOk.Top := frm.Height - 70;

        btnCancel := TButton.Create(frm); btnCancel.Parent := frm;
        btnCancel.Caption := 'Cancel';
        btnCancel.ModalResult := mrCancel;
        btnCancel.Left := btnOk.Left + btnOk.Width + 8;
        btnCancel.Top := btnOk.Top;

        pnl := TPanel.Create(frm);
        pnl.Parent := frm;
        pnl.Left := 8;
        pnl.Top := btnOk.Top - 12;
        pnl.Width := frm.Width - 20;
        pnl.Height := 2;

        if frm.ShowModal <> mrOk then begin
            Result := False;
            Exit;
        end
        else Result := True;

    finally
        frm.Free;
    end;
end;

procedure ProcessActiFurnMstt;
var
    si, i, cnt: integer;
    n, r, s, ms, rCell, fakeStatic, patchStatGroup: IInterface;
    HasLOD, HasMS: Boolean;
    joLOD: TJsonObject;
    fakeStaticEditorId, fakeStaticFormID: string;
begin
    for i := 0 to Pred(tlActiFurnMstt.Count) do begin
        s := ObjectToElement(tlActiFurnMstt[i]);
        joLOD := TJsonObject.Create;
        HasLOD := AssignLODModels(s, joLOD);

        //AssignLODToStat(s, joLOD);

        //Process references that have lod.
        if HasLOD then begin
            cnt := 0;

            for si := 0 to Pred(ReferencedByCount(s)) do begin
                r := ReferencedByIndex(s, si);
                if Signature(r) <> 'REFR' then continue;
                if not IsWinningOverride(r) then continue;
                if GetIsDeleted(r) then continue;
                rCell := WinningOverride(LinksTo(ElementByIndex(r, 0)));
                if GetElementEditValues(rCell, 'DATA - Flags\Is Interior Cell') = 1 then continue;

                //This reference should get lod if it passes these checks.
                cnt := cnt + 1;
                //Perform these functions if this is the very first reference. We set up the base object and mat swap.
                if cnt = 1 then begin
                    //Add a new 'fake' STAT record which we will use to place LODs for these non-STAT records.
                    iCurrentPlugin := RefMastersDeterminePlugin(s);
                    patchStatGroup := GroupBySignature(iCurrentPlugin, 'STAT');
                    HasMS := ElementExists(s, 'Model\MODS - Material Swap');

                    //Add Fake STAT
                    fakeStatic := Add(patchStatGroup, 'STAT', True);
                    fakeStaticEditorId := SetEditorID(fakeStatic, 'FOLIP_' + EditorID(s) + '_FakeStatic');
                    fakeStaticFormID := IntToHex(GetLoadOrderFormID(fakeStatic), 8);

                    //Add base material swap
                    if HasMS then begin
                        ms := LinksTo(ElementByPath(s, 'Model\MODS'));
                        if tlMswp.IndexOf(ms) = -1 then tlMswp.Add(ms);
                        Add(fakeStatic, 'XMSP', True);
                        SetElementNativeValues(fakeStatic, 'XMSP', ms);
                    end;

                    //Add LOD models
                    AssignLODToStat(fakeStatic, joLOD);
                end;

                //Copy
                if tlCells.IndexOf(rCell) < 0 then begin
                    tlCells.Add(rCell);
                    iCurrentPlugin := RefMastersDeterminePlugin(rCell);
                    wbCopyElementToFile(rCell, iCurrentPlugin, False, True);
                end;

                //Copy duplicate of ref to patch and set base record to fakeStatic
                iCurrentPlugin := RefMastersDeterminePlugin(r);
                n := wbCopyElementToFile(r, iCurrentPlugin, True, True);
                SetElementEditValues(n, 'NAME', fakeStaticFormID);

                //Add mat swap if it exists to list to check.
                if not ElementExists(r, 'XMSP - Material Swap') then continue;
                ms := LinksTo(ElementByPath(r, 'XMSP'));
                if tlMswp.IndexOf(ms) = -1 then tlMswp.Add(ms);
            end;
        end;

        joLOD.Free;
    end;
end;

procedure AssignLODMaterialsList;
{
    Assign lod materials to MSWP record.
}
var
    i, si, tp, sc, cnt, n, oms: integer;
    m, mn, substitutions, sub: IInterface;
    colorRemap, originalMat, originalLODMat, replacementMat, om, rm: string;
    slLODSubOriginal, slLODSubReplacement, slExistingSubstitutions, slMissingMaterials, slTopPaths, slLODOriginals, slLODReplacements, slDummy: TStringList;
    hasLODOriginalMaterial, hasLODReplacementMaterial: Boolean;
begin
    slDummy := TStringList.Create;
    //store missing lod materials here
    slMissingMaterials := TStringList.Create;

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
            originalMat := LowerCase(GetElementEditValues(sub, 'BNAM - Original Material'));
            if originalMat = '' then continue;
            if LeftStr(originalMat, 10) <> 'materials\' then originalMat := 'materials\' + originalMat;

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

            replacementMat := LowerCase(GetElementEditValues(sub, 'SNAM - Replacement Material'));
            if replacementMat = '' then continue;
            if LeftStr(replacementMat, 10) <> 'materials\' then replacementMat := 'materials\' + replacementMat;

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
                if ResourceExists(replacementMat) and (colorRemap = '') then slMissingMaterials.Add(Name(m) + #9 + 'Missing LOD replacement material: ' + rm + #9 + ' from ' + #9 + om)
                else AddMessage(Name(m) + #9 + 'Ignoring this Material Swap Substitution due to the Replacement Material referencing a material that does not exist: ' + replacementMat);
                continue;
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
        iCurrentPlugin := RefMastersDeterminePlugin(m);
        mn := wbCopyElementToFile(m, iCurrentPlugin, False, True);
        for n := 0 to Pred(cnt) do begin
            //AddMessage(ShortName(m) + #9 + slLODSubOriginal[n] + #9 + slLODSubReplacement[n]);
            AddMaterialSwap(mn, slLODSubOriginal[n], slLODSubReplacement[n]);
        end;
        slLODSubOriginal.Free;
        slLODSubReplacement.Free;
    end;

    ListStringsInStringList(slMissingMaterials);
    slMissingMaterials.Free;
    slDummy.Free;
end;

procedure AddMaterialSwap(e: IInterface; om, rm: String);
{
    Given a material swap record e, add a new swap with om as the original material and rm as the replacement material.
}
var
    substitutions, ms: IInterface;
begin
    substitutions := ElementByPath(e, 'Material Substitutions');
    ms := ElementAssign(substitutions, HighInteger, nil, False);
    SetElementNativeValues(ms, 'BNAM - Original Material', om);
    SetElementNativeValues(ms, 'SNAM - Replacement Material', rm);
end;

procedure AssignLODModelsList;
{
    Assign lod models to STAT records.
}
var
    i, cnt: integer;
    HasLOD: Boolean;
    ms, s: IInterface;
    joLOD: TJsonObject;
begin
    for i := 0 to Pred(tlStats.Count) do begin
        s := ObjectToElement(tlStats[i]);
        joLOD := TJsonObject.Create;
        HasLOD := AssignLODModels(s, joLOD);

        //Add lod change if the HasDistantLOD flag needs to be unset.
        if ((joLOD.Count > 0) and (joLOD['hasdistantlod'] = 0)) then AssignLODToStat(s, joLOD);

        //List relevant material swaps
        if HasLOD then begin
            cnt := ProcessReferences(s);

            //check for base material swap
            if (cnt > 0) and (ElementExists(s, 'Model\MODS - Material Swap')) then begin
                ms := LinksTo(ElementByPath(s, 'Model\MODS'));
                if tlMswp.IndexOf(ms) = -1 then tlMswp.Add(ms);
            end;

            if ((cnt > 0) and (joLOD.Count > 0)) then AssignLODToStat(s, joLOD);
        end;
        joLOD.Free;
    end;
end;

function ProcessReferences(s: IInterface;): integer;
var
    si, cnt: integer;
    ms, r, rCell: IInterface;
begin
    cnt := 0;
    for si := 0 to Pred(ReferencedByCount(s)) do begin
        r := ReferencedByIndex(s, si);
        if Signature(r) <> 'REFR' then continue;
        if not IsWinningOverride(r) then continue;
        if GetIsDeleted(r) then continue;
        rCell := WinningOverride(LinksTo(ElementByIndex(r, 0)));
        if GetElementEditValues(rCell, 'DATA - Flags\Is Interior Cell') = 1 then continue;
        cnt := cnt + 1;
        if not ElementExists(r, 'XMSP - Material Swap') then continue;
        ms := LinksTo(ElementByPath(r, 'XMSP'));
        if tlMswp.IndexOf(ms) = -1 then tlMswp.Add(ms);
    end;
    Result := cnt;
end;

procedure AssignLODToStat(s: IInterface; joLOD: TJsonObject);
var
    n: IInterface;
begin
    //AddMessage(ShortName(s) + #9 + joLOD.S['level0'] + #9 + joLOD.S['level1'] + #9 + joLOD.S['level2']);
    iCurrentPlugin := RefMastersDeterminePlugin(s);
    n := wbCopyElementToFile(s, ICurrentPlugin, False, True);
    SetElementNativeValues(n, 'Record Header\Record Flags\Has Distant LOD', joLOD.I['hasdistantlod']);
    Add(n, 'MNAM', True);
    SetElementNativeValues(n, 'MNAM\LOD #0 (Level 0)\Mesh', joLOD.S['level0']);
    SetElementNativeValues(n, 'MNAM\LOD #1 (Level 1)\Mesh', joLOD.S['level1']);
    SetElementNativeValues(n, 'MNAM\LOD #2 (Level 2)\Mesh', joLOD.S['level2']);
    SetElementNativeValues(n, 'MNAM\LOD #3 (Level 3)\Mesh', joLOD.S['level3']);
end;

procedure ListStringsInStringList(sl: TStringList);
{
    Given a TStringList, add a message for all items in the list.
}
var
    i: integer;
begin
    AddMessage('=======================================================================================');
    for i := 0 to Pred(sl.Count) do AddMessage(sl[i]);
    AddMessage('=======================================================================================');
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

procedure CreateObjects;
{
    Create objects.
}
begin
    //TLists
    tlStats := TList.Create;
    tlActiFurnMstt := TList.Create;
    tlMswp := TList.Create;
    tlCells := TList.Create;

    //TStringLists
    slMatFiles := TStringList.Create;
    slMatFiles.Duplicates := dupIgnore;
    slNifFiles := TStringList.Create;
    slNifFiles.Duplicates := dupIgnore;
    slTopLevelModPatternPaths := TStringList.Create;
    slMessages := TStringList.Create;

    //TJsonObjects
    joRules := TJsonObject.Create;
    joMswpMap := TJsonObject.Create;
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
    slStats, slActiFurnMstt: TStringList;
begin
    slStats := TStringList.Create;
    slActiFurnMstt := TStringList.Create;

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
            idx := slActiFurnMstt.IndexOf(recordId);
            if idx > -1 then continue
            slActiFurnMstt.Add(recordId);
            tlActiFurnMstt.Add(r);
        end;

        //FURN
        g := GroupBySignature(f, 'FURN');
        for j := 0 to Pred(ElementCount(g)) do begin
            r := WinningOverride(ElementByIndex(g, j));
            recordId := GetFileName(r) + #9 + ShortName(r);
            idx := slActiFurnMstt.IndexOf(recordId);
            if idx > -1 then continue
            slActiFurnMstt.Add(recordId);
            tlActiFurnMstt.Add(r);
        end;

        //ACTI
        g := GroupBySignature(f, 'ACTI');
        for j := 0 to Pred(ElementCount(g)) do begin
            r := WinningOverride(ElementByIndex(g, j));
            recordId := GetFileName(r) + #9 + ShortName(r);
            idx := slActiFurnMstt.IndexOf(recordId);
            if idx > -1 then continue
            slActiFurnMstt.Add(recordId);
            tlActiFurnMstt.Add(r);
        end;

    end;

    slStats.Free;
    slActiFurnMstt.Free;

    AddMessage('Found ' + IntToStr(tlStats.Count) + ' STAT records.');
    AddMessage('Found ' + IntToStr(tlActiFurnMstt.Count) + ' ACTI, FURN, and MSTT records.');
end;

procedure FilesInContainers(containers: TStringList);
{
    Retrieves the ba2 packed files.
}
var
    slArchivedFiles: TStringList;
    i: integer;
    f, archive: string;
begin
    slArchivedFiles := TStringList.Create;
    slArchivedFiles.Duplicates := dupIgnore;
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
        ResourceList(containers[i], slArchivedFiles);
    end;
    for i := 0 to Pred(slArchivedFiles.Count) do begin
        f := slArchivedFiles[i];
        //materials or meshes
        if IsInLODDir(f, 'materials') then slMatFiles.Add(f)
        else if IsLODResourceModel(f) then slNifFiles.Add(f);
    end;
    slArchivedFiles.Free;
end;

procedure LoadRules(f: string);
{
    Load LOD Rules and Material Swap Map JSON files
}
var
    sub: TJsonObject;
    c, a: integer;
    j, key: string;
    bFirstRuleJson, bFirstMswpJson: Boolean;
begin
    //LOD Rules
    j := 'FOLIP\' + TrimLeftChars(f, 4) + ' - LODRules.json';
    if ResourceExists(j) then begin
        AddMessage('Loaded LOD Rule File: ' + j);
        if bFirstRuleJson then begin
            bFirstRuleJson := False;
            joRules.LoadFromResource(j);
        end
        else begin
            sub := TJsonObject.Create;
            sub.LoadFromResource(j);
            for c := 0 to Pred(sub.Count) do begin
                key := sub.Names[c];
                joRules.O[key].Assign(sub.O[key]);
            end;
            sub.Free;
        end;
    end;
    //Material Swap Maps
    j := 'FOLIP\' + TrimLeftChars(f, 4) + ' - MaterialSwapMap.json';
    if ResourceExists(j) then begin
        AddMessage('Loaded LOD Material Swap File: ' + j);
        if bFirstMswpJson then begin
            bFirstMswpJson := False;
            joMswpMap.LoadFromResource(j);
        end
        else begin
            sub := TJsonObject.Create;
            sub.LoadFromResource(j);
            for c := 0 to Pred(sub.Count) do begin
            key := sub.Names[c];
            for a := 0 to Pred(sub.A[key].Count) do joMswpMap.A[key].Add(sub.A[key].S[a]);
            end;
            sub.Free;
        end;
    end;
end;

function AssignLODModels(s: IInterface; joLOD: TJsonObject): Boolean;
{
    Assigns LOD Models to Stat records.
}
var
    hasChanged, ruleOverride: Boolean;
    i, hasDistantLOD: integer;
    n: IInterface;
    colorRemap, lod4, lod8, lod16, lod32, model, omodel, olod4, olod8, olod16, olod32, editorid: string;
    slTopPaths: TStringList;
begin
    hasChanged := False;
    ruleOverride := False;
    olod4 := '';
    olod8 := '';
    olod16 := '';
    olod32 := '';
    hasDistantLOD := 0;

    omodel := LowerCase(GetElementEditValues(s, 'Model\MODL - Model FileName'));

    if LeftStr(omodel, 7) <> 'meshes\' then model := 'meshes\' + omodel else model := omodel;

    colorRemap := FloatToStr(StrToFloatDef(GetElementEditValues(s, 'Model\MODC - Color Remapping Index'),'9'));
    if colorRemap = '9' then colorRemap := '' else colorRemap := '_' + colorRemap;

    //Only STAT records have these. We use this function on other signatures that don't have these fields.
    if Signature(s) = 'STAT' then begin
        olod4 := LowerCase(GetElementNativeValues(s, 'MNAM\LOD #0 (Level 0)\Mesh'));
        olod8 := LowerCase(GetElementNativeValues(s, 'MNAM\LOD #1 (Level 1)\Mesh'));
        olod16 := LowerCase(GetElementNativeValues(s, 'MNAM\LOD #2 (Level 2)\Mesh'));
        olod32 := LowerCase(GetElementNativeValues(s, 'MNAM\LOD #3 (Level 3)\Mesh'));
        hasDistantLOD := GetElementNativeValues(s,'Record Header\Record Flags\Has Distant LOD');
    end;

    editorid := GetElementEditValues(s, 'EDID');

    if LowerCase(RightStr(editorid, 5)) = 'nolod' then begin
        slMessages.Add(ShortName(s) + ' - Editor ID ends in "nolod", so it will be skipped.');
        Result := False;
        Exit;
    end;

    if joRules.Contains(editorid) then begin
        lod4 := joRules.O[editorid].S['level0'];
        lod8 := joRules.O[editorid].S['level1'];
        lod16 := joRules.O[editorid].S['level2'];
        lod32 := joRules.O[editorid].S['level3'];
        if joRules.O[editorid].S['hasdistantlod'] = 'false' then ruleOverride := True;
    end
    else if joRules.Contains(omodel) then begin
        lod4 := joRules.O[omodel].S['level0'];
        lod8 := joRules.O[omodel].S['level1'];
        lod16 := joRules.O[omodel].S['level2'];
        lod32 := joRules.O[omodel].S['level3'];
        if joRules.O[omodel].S['hasdistantlod'] = 'false' then ruleOverride := True;
    end
    else begin
        slTopPaths := TStringList.Create;
        for i := 0 to Pred(slTopLevelModPatternPaths.Count) do begin
            if ContainsText(model, 'meshes\' + slTopLevelModPatternPaths[i]) then slTopPaths.Add(slTopLevelModPatternPaths[i]);
        end;
        lod4 := LODModelForLevel(model, colorRemap, '0', olod4, slTopPaths);
        lod8 := LODModelForLevel(model, colorRemap, '1', olod8, slTopPaths);
        lod16 := LODModelForLevel(model, colorRemap, '2', olod16, slTopPaths);
        lod32 := LODModelForLevel(model, colorRemap, '3', olod32, slTopPaths);
        slTopPaths.Free;
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
        if hasDistantLOD = 1 then hasChanged := True;
    end;

    if ruleOverride then hasDistantLOD := 0;


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

function LODModelForLevel(model, colorRemap, level, original: string; slTopPaths: TStringList;): string;
{
    Given a model and level, checks to see if an LOD model exists and returns it.
}
var
    searchModel, p1, p2: string;
    i: integer;
begin

    for i := 0 to Pred(slTopPaths.Count) do begin
        // meshes\dlc01\test.nif  to  meshes\dlc01\lod\test.nif
        searchModel := StringReplace(model, 'meshes\' + slTopPaths[i], 'meshes\' + slTopPaths[i] + 'lod\', [rfReplaceAll, rfIgnoreCase]);
        // meshes\dlc01\lod\test.nif  to  meshes\dlc01\lod\test_lod. Add colorRemap as _0.500 as well.
        searchModel := TrimLeftChars(searchModel, 4) + colorRemap + '_lod';
        p1 := searchModel + '_' + level + '.nif';
        if slNifFiles.IndexOf(p1) > -1 then begin
            Result := TrimRightChars(p1, 7);
            Exit;
        end;
        p2 := searchModel + '.nif';
        if ((level = '0') and (slNifFiles.IndexOf(p2) > -1)) then begin
            Result := TrimRightChars(p2, 7);
            Exit;
        end;
    end;
    Result := original;
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
    Result := '';
    for i := 0 to Pred(slTopPaths.Count) do begin
        // materials\dlc01\test.bgsm to materials\dlc01\lod\test.bgsm
        searchMaterial := StringReplace(material, 'materials\' + slTopPaths[i], 'materials\' + slTopPaths[i] + 'lod\', [rfReplaceAll, rfIgnoreCase]);
        p1 := TrimLeftChars(searchMaterial, 5) + colorRemap + '.bgsm';
        if Result = '' then Result := p1;
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

function TrimRightChars(s: string; chars: integer): string;
{
    Returns right string - chars
}
begin
    Result := RightStr(s, Length(s) - chars);
end;

function TrimLeftChars(s: string; chars: integer): string;
{
    Returns left string - chars
}
begin
    Result := LeftStr(s, Length(s) - chars);
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

function CreatePlugins: Boolean;
{
    Checks the plugins to ensure required mods are not missing.
    Checks to make sure an old patch generated from this script is not present.
    Creates the plugin files we need.
}
var
    bFO4LODGen, bFolip, bFirstRuleJson, bFirstMswpJson: Boolean;
    i: integer;
    f: string;
begin
    bFO4LODGen := False;
    bFolip := False;
    bFirstRuleJson := True;
    bFirstMswpJson := True;
    for i := 0 to Pred(FileCount) do begin
        f := GetFileName(FileByIndex(i));

        if SameText(f, sFolipMasterFileName) then
            iFolipMasterFile := FileByIndex(i)
        else if SameText(f, sFolipPluginFileName) then
            iFolipPluginFile := FileByIndex(i)
        else if SameText(f, sFO4LODGenFileName) then
            bFO4LODGen := True
        else if SameText(f, sFolipFileName) then
            bFolip := True;
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
    //iFolipMasterFile := AddNewFileName(sFolipMasterFileName, False);
    //AddMasterIfMissing(iFolipMasterFile, 'Fallout4.esm');
    //SetIsESM(iFolipMasterFile, True);
    iFolipPluginFile := AddNewFileName(sFolipPluginFileName, False);
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