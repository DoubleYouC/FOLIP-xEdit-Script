{
    Remove HasDistantLOD flag from STAT records.
}
unit HasDistantLOD;

var
    iPluginFile : IInterface;
    sFolipPluginFileName : string;
    slStats : TStringList;
    tlStats : TList;

function Initialize:integer;
{
    This function is called at the beginning.
}
var
    i, j, idx : integer;
    f, g, n, r, p : IInterface;
    editorid, recordId : string;
begin
    slStats := TStringList.Create;
    tlStats := TList.Create;
    sFolipPluginFileName := 'FOLIP - After Generation';

    if not MainMenuForm then Exit;

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

    iPluginFile := AddNewFileName(sFolipPluginFileName + '.esp', True);
    AddMasterIfMissing(iPluginFile, 'Fallout4.esm');

    for i := 0 to Pred(tlStats.Count) do begin
        r := ObjectToElement(tlStats[i]);
        p := RefMastersDeterminePlugin(r);
        n := wbCopyElementToFile(r, p, False, True);
        SetElementNativeValues(n, 'Record Header\Record Flags\Has Distant LOD', 0);
    end;
    MessageDlg('Patch generated successfully!' + #13#10#13#10 + 'Do not forget to save the plugin.', mtInformation, [mbOk], 0);
    Result := 0;
end;

function Finalize: integer;
{
    This function is called at the end.
}
begin
    slStats.Free;
    tlStats.Free;

    Result := 0;
end;

function MainMenuForm: Boolean;
{
    Main menu form.
}
var
    frm: TForm;
    btnStart, btnCancel: TButton;
    edPluginName: TEdit;
    pnl: TPanel;
    picFolip: TPicture;
    fImage: TImage;
    gbOptions: TGroupBox;
    chkFakeStatics, chkForceLOD8: TCheckBox;
begin
    frm := TForm.Create(nil);
    try
        frm.Caption := 'FOLIP HasDistantLOD Flag Remover';
        frm.Width := 600;
        frm.Height := 480;
        frm.Position := poMainFormCenter;
        frm.BorderStyle := bsDialog;
        frm.KeyPreview := True;
        frm.OnClose := frmOptionsFormClose;
        frm.OnKeyDown := FormKeyDown;

        picFolip := TPicture.Create;
        picFolip.LoadFromFile(ScriptsPath() + 'FOLIP\FOLIP.jpg');

        fImage := TImage.Create(frm);
		fImage.Picture := picFolip;
		fImage.Parent := frm;
        fImage.Width := 576;
		fImage.Height := 278;
		fImage.Left := 8;
		fImage.Top := 12;

        gbOptions := TGroupBox.Create(frm);
        gbOptions.Parent := frm;
        gbOptions.Left := 10;
        gbOptions.Top := fImage.Top + fImage.Height + 24;
        gbOptions.Width := frm.Width - 30;
        gbOptions.Caption := 'FOLIP - After Generation';
        gbOptions.Height := 104;

        edPluginName := TEdit.Create(gbOptions);
        edPluginName.Parent := gbOptions;
        edPluginName.Name := 'edPluginName';
        edPluginName.Left := 104;
        edPluginName.Top := 30;
        edPluginName.Width := 180;
        edPluginName.Hint := 'Sets the output plugin name for the patch.';
        edPluginName.ShowHint := True;
        CreateLabel(gbOptions, 16, edPluginName.Top + 4, 'Output Plugin:');

        btnStart := TButton.Create(gbOptions);
        btnStart.Parent := gbOptions;
        btnStart.Caption := 'Start';
        btnStart.ModalResult := mrOk;
        btnStart.Top := 72;

        btnCancel := TButton.Create(gbOptions);
        btnCancel.Parent := gbOptions;
        btnCancel.Caption := 'Cancel';
        btnCancel.ModalResult := mrCancel;
        btnCancel.Top := btnStart.Top;

        btnStart.Left := gbOptions.Width - btnStart.Width - btnCancel.Width - 16;
        btnCancel.Left := btnStart.Left + btnStart.Width + 8;

        pnl := TPanel.Create(gbOptions);
        pnl.Parent := gbOptions;
        pnl.Left := 8;
        pnl.Top := btnStart.Top - 12;
        pnl.Width := gbOptions.Width - 16;
        pnl.Height := 2;

        edPluginName.Text := sFolipPluginFileName;

        if frm.ShowModal <> mrOk then begin
            Result := False;
            Exit;
        end
        else Result := True;

        sFolipPluginFileName := edPluginName.Text;

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

function RefMastersDeterminePlugin(r: IInterface): IInterface;
{
    Adds masters required by the reference to the plugin and returns the plugin.
}
begin
    AddRequiredElementMasters(r, iPluginFile, False, True);
    SortMasters(iPluginFile);
    Result := iPluginFile;
end;

end.