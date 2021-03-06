unit SettingsWindow;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, Grids, StrUtils, Windows;

type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    ApplyButton: TButton;
    CancelButton: TButton;
    SettingsStrGridBeforeChange: TStringGrid;
    ShowMouseLables: TCheckBox;
    LoadSettings: TButton;
    DefaultButton: TButton;
    ColorBox: TGroupBox;
    ColorDialog: TColorDialog;
    InputHistEdit: TEdit;
    OpenDialog: TOpenDialog;
    OtherGroupBox: TGroupBox;
    KeyBGColor: TEdit;
    BGColor: TEdit;
    BG: TLabel;
    InputHistLabel: TLabel;
    SaveDialog: TSaveDialog;
    SaveSettings: TButton;
    PressedColor: TEdit;
    FontColor: TEdit;
    KeyBG: TLabel;
    KeyPressed: TLabel;
    KeyFont: TLabel;
    SettingsStrGrid: TStringGrid;
    InvokerMode: TCheckBox;

    procedure ApplyButtonClick(Sender: TObject);
    procedure BGColorClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure InputHistEditEditingDone(Sender: TObject);
    procedure InvokerModeClick(Sender: TObject);
    procedure KeyBGColorClick(Sender: TObject);
    procedure DefaultButtonClick(Sender: TObject);
    procedure LoadSettingsClick(Sender: TObject);
    procedure PressedColorClick(Sender: TObject);
    procedure FontColorClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ShowMouseLablesClick(Sender: TObject);

    procedure UpdateSettingsStrGrid();

    procedure ClearSettingsStrGrid();

    procedure ApplyColorsFrom(var strGrid: TStringGrid);
    procedure SaveSettingsClick(Sender: TObject);

    function LoadSettingsFrom(dir: string): boolean;

    procedure setDefault();

  private
    { private declarations }
  public
    { public declarations }
  end;

var
  SettingsForm: TSettingsForm;

const
  DEFAULT_KEY_BG_COLOR = clWhite;
  DEFAULT_PRESSED_COLOR = $00646464;
  DEFAULT_FONT_COLOR = clBlack;
  DEFAULT_BG_COLOR = clLime;
  DEFAULT_HIST_EDIT = 18;
  DEFAULT_MOUSE_LABELS = false;

implementation

uses MainWindow, DetachedHistoryWindow, InvokerSpellsWindow;
{$R *.lfm}

// TAG 1 - KEY BG (TSHAPE)
// TAG 2 - KEY PRESSED (TSHAPE)
// TAG 3 - KEY LABEL (TLABEL)

{ TSettingsForm }

procedure TSettingsForm.UpdateSettingsStrGrid();
begin
  SettingsStrGrid.Cells[1, 0] := IntToStr(KeyBGColor.Color);              // Key BG color
  SettingsStrGrid.Cells[1, 1] := IntToStr(PressedColor.Color);            // Key pressed color
  SettingsStrGrid.Cells[1, 2] := IntToStr(FontColor.Color);               // Key font color
  SettingsStrGrid.Cells[1, 3] := IntToStr(BGColor.Color);                 // BG color
  SettingsStrGrid.Cells[1, 4] := InputHistEdit.Text;                      // History max
  SettingsStrGrid.Cells[1, 5] := BoolToStr(ShowMouseLables.Checked);      // Show mouse labels
end;

procedure TSettingsForm.setDefault();
begin
  KeyBGColor.Color := DEFAULT_KEY_BG_COLOR;
  KeyBGColor.Font.Color := KeyBGColor.Color;

  PressedColor.Color := DEFAULT_PRESSED_COLOR;
  PressedColor.Font.Color := PressedColor.Color;

  FontColor.Color := DEFAULT_FONT_COLOR;
  FontColor.Font.Color := FontColor.Color;

  BGColor.Color := DEFAULT_BG_COLOR;
  BGColor.Font.Color := BGColor.Color;

  InputHistEdit.Text := IntToStr(DEFAULT_HIST_EDIT);

  ShowMouseLables.Checked := DEFAULT_MOUSE_LABELS;

  UpdateSettingsStrGrid;
end;

function TSettingsForm.LoadSettingsFrom(dir: string): boolean;
var
  newKeyBGClr,
  newPressedClr,
  newFontClr,
  newBGClr: LongInt;
  newInputHist: String;
  newShowMouseLabels: Boolean;

  // Exception stuff
  badColNum: longInt;
  param: string;

begin
  ClearSettingsStrGrid();

  try
     SettingsStrGrid.LoadFromFile(Dir);
  except
     on err:exception do
     begin
        ShowMessage('Settings file is corrupted in ' + #10 + Dir + #10 + 'Error message: ' + #10 + err.message + #10
        + 'Loading canceled!');
        result := false;
        exit;
     end;
  end;

  try
     begin
       badColNum := 1;
       param := 'Key BG color';
       newKeyBGClr := StrToInt(SettingsStrGrid.Cells[1, 0]);

       badColNum := 2;
       param := 'Key pressed color';
       newPressedClr := StrToInt(SettingsStrGrid.Cells[1, 1]);

       badColNum := 3;
       param := 'Key font color';
       newFontClr := StrToInt(SettingsStrGrid.Cells[1, 2]);

       badColNum := 4;
       param := 'BG color';
       newBGClr := StrToInt(SettingsStrGrid.Cells[1, 3]);

       badColNum := 5;
       param := 'Input history';
       newInputHist := SettingsStrGrid.Cells[1, 4];

       badColNum := 6;
       param := 'Show mouse labels';
       newShowMouseLabels := StrToBool(SettingsStrGrid.Cells[1, 5]);
     end;
  except
     on Exception : EConvertError do
     begin
       ShowMessage(Exception.Message + #10 + 'In file ' + dir + #10 + 'At column: ' + IntToStr(badColNum) + #10 + 'Parameter: ' + param);
       ShowMessage('Error while loading a preset! Nothing has been changed!');
       result := false;
       exit;
     end;
  end;

  KeyBGColor.Color := newKeyBGClr;
  PressedColor.Color := newPressedClr;
  FontColor.Color := newFontClr;
  BGColor.Color := newBGClr;
  InputHistEdit.Text := newInputHist;
  ShowMouseLables.Checked := newShowMouseLabels;

  result := true;
  //ShowMessage('Preset loaded successfuly!');
end;

procedure TSettingsForm.ClearSettingsStrGrid();
begin
  SettingsStrGrid.Cells[1, 0] := '';   // Key BG color
  SettingsStrGrid.Cells[1, 1] := '';   // Key pressed color
  SettingsStrGrid.Cells[1, 2] := '';   // Key font color
  SettingsStrGrid.Cells[1, 3] := '';   // BG color
  SettingsStrGrid.Cells[1, 4] := '';   // History max
  SettingsStrGrid.Cells[1, 5] := '';   // Show mouse labels
end;

procedure TSettingsForm.ApplyColorsFrom(var strGrid: TStringGrid);
var
  i, j: LongInt;
begin
  KeyBGColor.Color := StrToInt(strGrid.Cells[1, 0]);   // Key BG color
  PressedColor.Color := StrToInt(strGrid.Cells[1, 1]);   // Key pressed color
  FontColor.Color := StrToInt(strGrid.Cells[1, 2]);   // Key font color
  BGColor.Color := StrToInt(strGrid.Cells[1, 3]);   // BG color
  InputHistEdit.Text := strGrid.Cells[1, 4];   // History max
  ShowMouseLables.Checked := StrToBool(strGrid.Cells[1, 5]);   // Show mouse labels

  with PVInput do
  begin
    for i := 0 to KeyboardPanel.ControlCount-1 do
    begin
       if (KeyboardPanel.Controls[i] is TPanel) then
       begin
         for j := 0 to (KeyboardPanel.Controls[i] as TPanel).ControlCount-1 do
         begin
            if ((KeyboardPanel.Controls[i] as TPanel).Controls[j] is TShape) then
            begin
              if (((KeyboardPanel.Controls[i] as TPanel).Controls[j] as TShape).Tag = 1) then // KEY BG
              begin
                ((KeyboardPanel.Controls[i] as TPanel).Controls[j] as TShape).Brush.Color := KeyBGColor.Color;
                ((KeyboardPanel.Controls[i] as TPanel).Controls[j] as TShape).Pen.Color := KeyBGColor.Color;
              end
              else if (((KeyboardPanel.Controls[i] as TPanel).Controls[j] as TShape).Tag = 2) then // KEY PRESSED
              begin
                ((KeyboardPanel.Controls[i] as TPanel).Controls[j] as TShape).Brush.Color := PressedColor.Color;
                ((KeyboardPanel.Controls[i] as TPanel).Controls[j] as TShape).Pen.Color := PressedColor.Color;
              end;
            end
            else if ((KeyboardPanel.Controls[i] as TPanel).Controls[j] is TLabel) then
            begin
              if (((KeyboardPanel.Controls[i] as TPanel).Controls[j] as TLabel).Tag = 3) then // KEY LABEL
              begin
                ((KeyboardPanel.Controls[i] as TPanel).Controls[j] as TLabel).Font.Color := FontColor.Color;
              end
            end;
         end;
       end;
    end;

    for i := 0 to MousePanel.ControlCount-1 do
    begin
       if (MousePanel.Controls[i] is TPanel) then
       begin
         for j := 0 to (MousePanel.Controls[i] as TPanel).ControlCount-1 do
         begin
            if ((MousePanel.Controls[i] as TPanel).Controls[j] is TShape) then
            begin
              if (((MousePanel.Controls[i] as TPanel).Controls[j] as TShape).Tag = 1) then // KEY BG
              begin
                ((MousePanel.Controls[i] as TPanel).Controls[j] as TShape).Brush.Color := KeyBGColor.Color;
                ((MousePanel.Controls[i] as TPanel).Controls[j] as TShape).Pen.Color := KeyBGColor.Color;
              end
              else if (((MousePanel.Controls[i] as TPanel).Controls[j] as TShape).Tag = 2) then // KEY PRESSED
              begin
                ((MousePanel.Controls[i] as TPanel).Controls[j] as TShape).Brush.Color := PressedColor.Color;
                ((MousePanel.Controls[i] as TPanel).Controls[j] as TShape).Pen.Color := PressedColor.Color;
              end;
            end
            else if ((MousePanel.Controls[i] as TPanel).Controls[j] is TLabel) then
            begin
              if (((MousePanel.Controls[i] as TPanel).Controls[j] as TLabel).Tag = 3) then // KEY LABEL
              begin
                ((MousePanel.Controls[i] as TPanel).Controls[j] as TLabel).Font.Color := FontColor.Color;
              end
            end;
         end;
       end;
    end;

    PVInput.Color := BGColor.Color;



    UpdateSettingsStrGrid();
  end;
end;

procedure TSettingsForm.SaveSettingsClick(Sender: TObject);
var
  Dir: string;
begin
  SaveDialog.Execute;

  Dir := SaveDialog.FileName;

  if (Dir = '') then
  begin
    //MessageBox(0, PChar('Empty save file directory!'), 'Error', MB_OK+MB_ICONERROR);
    exit;
  end
  else if (not(AnsiContainsStr(Dir, '.set'))) then
  begin
    ShowMessage('File at "' + Dir + '" is not of extension ".set"!');
    exit;
  end;

  UpdateSettingsStrGrid();

  SettingsStrGrid.SaveToFile(Dir);

  ShowMessage('Preset saved successfuly!');
end;

procedure TSettingsForm.FormCreate(Sender: TObject);
var
  latestSettingsDir: string;
begin
  isInvokerSupportEnabled := false;
  latestSettingsDir := Application.Params[0];

  SetLength(latestSettingsDir, Length(latestSettingsDir) - Length(ExtractFileName(Application.ExeName)));

  latestSettingsDir := latestSettingsDir + 'latestSettings.set';

  if (FileExists(latestSettingsDir)) then  // we already have a file with previous settings, load it
  begin
    //showMessage('Loading previos settings');
    if (LoadSettingsFrom(latestSettingsDir)) then
    begin
      ApplyButtonClick((ApplyButton as TObject));
    end
    else
    begin
      showMessage('Failed to load latest settings! Loading default values!');
      setDefault();
      ApplyButtonClick((ApplyButton as TObject));
    end;
  end
  else
  begin
    // set default values
    //showMessage('Setting default values');
    setDefault();
    ApplyButtonClick((ApplyButton as TObject));
  end;
end;

procedure TSettingsForm.KeyBGColorClick(Sender: TObject);
begin
  ColorDialog.Color:=(Sender as TEdit).Color;
  ColorDialog.Execute;

  (Sender as TEdit).Color := ColorDialog.Color;
  (Sender as TEdit).Font.Color := ColorDialog.Color;

  SettingsStrGrid.Cells[1, 0] := IntToStr((Sender as TEdit).Color);
end;

procedure TSettingsForm.DefaultButtonClick(Sender: TObject);
var
  buttonPressed: longInt;
begin
  buttonPressed := MessageBox             (SettingsForm.Handle,
                                                              'Are you sure you want to load default settings?',
                                                              'Loading default settings',
                                                              (MB_YESNO or MB_ICONQUESTION or MB_DEFBUTTON2 or MB_SYSTEMMODAL)
                                           );

  if (buttonPressed = IDNO) then
  begin
    exit;
  end
  else if (buttonPressed = IDYES) then
  begin
    SetDefault;
  end
  else
  begin
    // ??!?!?
    showMessage('ПИДАРАС ЧЕ ВАЩЕ СЛУЧИЛОСЬ ТО');
    exit;
  end;
end;

procedure TSettingsForm.LoadSettingsClick(Sender: TObject);
var
  Dir: string;

  buttonPressed: longInt;

begin
  OpenDialog.Execute;

  Dir := OpenDialog.FileName;

  if (Dir = '') then
  begin
    //MessageBox(0, PChar('Empty load file directory!'), 'Error', MB_OK+MB_ICONERROR);
    exit;
  end
  else if (not(AnsiContainsStr(Dir, '.set'))) then
  begin
    ShowMessage('File at "' + Dir + '" is not of extension ".set"!');
    exit;
  end;

  buttonPressed := MessageBox             (SettingsForm.Handle,
                                                              'Are you sure you want to load new settings?',
                                                              'Loading new settings',
                                                              (MB_YESNO or MB_ICONQUESTION or MB_DEFBUTTON2 or MB_SYSTEMMODAL)
                                           );

  if (buttonPressed = IDNO) then
  begin
    exit;
  end
  else if (buttonPressed = IDYES) then
  begin
    LoadSettingsFrom(dir);
  end
  else
  begin
    // ??!?!?
    showMessage('ПИДАРАС ЧЕ ВАЩЕ СЛУЧИЛОСЬ ТО');
    exit;
  end;
end;

procedure TSettingsForm.ApplyButtonClick(Sender: TObject);
var
  newHist: longInt;
  okInputHist: boolean;
begin
  ApplyColorsFrom(SettingsStrGrid);

  okInputHist := true;

  try
     newHist := StrToInt(InputHistEdit.Text);
  except
     on Exception : EConvertError do
     begin
       ShowMessage(Exception.Message);
       okInputHist := false;
     end;
  end;

  if (okInputHist) then
  begin
    MAX_HISTORY_SIZE := newHist;
  end
  else
  begin
    InputHistEdit.Text:=IntToStr(MAX_HISTORY_SIZE);
  end;

  if (PVInput.HistoryBox.Items.Count > MAX_HISTORY_SIZE) then
  begin
    while (PVInput.HistoryBox.Items.Count > MAX_HISTORY_SIZE) do
    begin
      PVInput.HistoryBox.Items.Delete(0);
      DetachedHistoryForm.DetachedInputHist.Items.Delete(0);
    end;
  end;

  if (ShowMouseLables.Checked) then
  begin
    PVInput.MMBLabel.Visible:=true;
    PVInput.LMBLabel.Visible:=true;
    PVInput.RMBLabel.Visible:=true;
  end
  else
  begin
    PVInput.MMBLabel.Visible:=false;
    PVInput.LMBLabel.Visible:=false;
    PVInput.RMBLabel.Visible:=false;
  end;

  SettingsForm.Hide;
end;

procedure TSettingsForm.BGColorClick(Sender: TObject);
begin
  ColorDialog.Color:=(Sender as TEdit).Color;
  ColorDialog.Execute;

  (Sender as TEdit).Color := ColorDialog.Color;
  (Sender as TEdit).Font.Color := ColorDialog.Color;

  SettingsStrGrid.Cells[1, 3] := IntToStr((Sender as TEdit).Color);
end;

procedure TSettingsForm.CancelButtonClick(Sender: TObject);
var
  newHist: longInt;
  okInputHist: boolean;
begin
  ApplyColorsFrom(SettingsStrGridBeforeChange);

  SettingsForm.Hide;
end;



procedure TSettingsForm.FormDestroy(Sender: TObject);
var
  latestSettingsDir: string;
begin
  latestSettingsDir := Application.Params[0];

  SetLength(latestSettingsDir, Length(latestSettingsDir) - Length(ExtractFileName(Application.ExeName)));

  latestSettingsDir := latestSettingsDir + 'latestSettings.set';

  if (FileExists(latestSettingsDir)) then
  begin
    SetFileAttributes(PChar(latestSettingsDir), FILE_ATTRIBUTE_NORMAL);
  end;

  SettingsStrGrid.SaveToFile(latestSettingsDir);

  SetFileAttributes(PChar(latestSettingsDir), FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_READONLY);
end;

procedure TSettingsForm.InputHistEditEditingDone(Sender: TObject);
begin
  try
     StrToInt(InputHistEdit.Text);
  except
     on Exception : EConvertError do
     begin
       InputHistEdit.Text:=IntToStr(MAX_HISTORY_SIZE);
       ShowMessage(Exception.Message);
     end;
  end;

  SettingsStrGrid.Cells[1, 4] := InputHistEdit.Text;
end;

procedure TSettingsForm.InvokerModeClick(Sender: TObject);
begin
  isInvokerSupportEnabled := (Sender as TCheckBox).Checked;
  InvokerSpellsForm.Visible := isInvokerSupportEnabled;
  if (isInvokerSupportEnabled) then InvokerSpellsForm.Show
  else InvokerSpellsForm.Hide;
end;

procedure TSettingsForm.PressedColorClick(Sender: TObject);
begin
  ColorDialog.Color:=(Sender as TEdit).Color;
  ColorDialog.Execute;

  (Sender as TEdit).Color := ColorDialog.Color;
  (Sender as TEdit).Font.Color := ColorDialog.Color;

  SettingsStrGrid.Cells[1, 1] := IntToStr((Sender as TEdit).Color);
end;

procedure TSettingsForm.FontColorClick(Sender: TObject);
begin
  ColorDialog.Color:=(Sender as TEdit).Color;
  ColorDialog.Execute;

  (Sender as TEdit).Color := ColorDialog.Color;
  (Sender as TEdit).Font.Color := ColorDialog.Color;

  SettingsStrGrid.Cells[1, 2] := IntToStr((Sender as TEdit).Color);
end;

procedure TSettingsForm.FormShow(Sender: TObject);
var
  i,j: longInt;
begin
  KeyBGColor.Color := PVInput.Key.Brush.Color;
  KeyBGColor.Font.Color := KeyBGColor.Color;

  PressedColor.Color := PVInput.TabPressed.Brush.Color;
  PressedColor.Font.Color := PressedColor.Color;

  FontColor.Color := PVInput.KeyLabel.Font.Color;
  FontColor.Font.Color := FontColor.Color;

  for i := 0 to SettingsStrGridBeforeChange.RowCount-1 do
  begin
    for j := 0 to SettingsStrGridBeforeChange.ColCount-1 do
    begin
      SettingsStrGridBeforeChange.Cells[j, i] := SettingsStrGrid.Cells[j, i];
    end;
  end;
end;

procedure TSettingsForm.ShowMouseLablesClick(Sender: TObject);
begin
  SettingsStrGrid.Cells[1, 5] := BoolToStr((Sender as TCheckBox).Checked);
end;

end.

