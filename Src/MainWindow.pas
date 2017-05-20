unit MainWindow;

// TAG 1 - KEY BG (TSHAPE)
// TAG 2 - KEY PRESSED (TSHAPE)
// TAG 3 - KEY LABEL (TLABEL)

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, LCL,
  Windows, Messages, Variants, StdCtrls, jwawinuser, lcltype, Menus, SettingsWindow;

type

  { TPVInput }

  TPVInput = class(TForm)
    InvokerKeys: TListBox;
    Key1: TShape;
    Key10: TShape;
    Key11: TShape;
    Key12: TShape;
    Key13: TShape;
    Key14: TShape;
    Key15: TShape;
    Key16: TShape;
    Key17: TShape;
    Key18: TShape;
    Key2: TShape;
    Key3: TShape;
    Key4: TShape;
    Key5: TShape;
    Key6: TShape;
    Key7: TShape;
    Key8: TShape;
    Key9: TShape;
    KeyLabel1: TLabel;
    KeyLabel10: TLabel;
    KeyLabel11: TLabel;
    KeyLabel12: TLabel;
    KeyLabel13: TLabel;
    KeyLabel14: TLabel;
    KeyLabel15: TLabel;
    KeyLabel16: TLabel;
    InputHistPopUpMenu: TPopupMenu;
    DetachItem: TMenuItem;
    ClearItem: TMenuItem;
    RMB: TPanel;
    MMB: TPanel;
    LMBLabel: TLabel;
    KeyLabel18: TLabel;
    KeyLabel2: TLabel;
    KeyLabel3: TLabel;
    KeyLabel4: TLabel;
    KeyLabel5: TLabel;
    KeyLabel6: TLabel;
    KeyLabel7: TLabel;
    KeyLabel8: TLabel;
    KeyLabel9: TLabel;
    CapsLockPressed: TShape;
    DPressed: TShape;
    APressed: TShape;
    InvertItem: TMenuItem;
    ExitItem: TMenuItem;
    RMBLabel: TLabel;
    MMBLabel: TLabel;
    MouseRight: TShape;
    MouseMiddle: TShape;
    MouseLeftBG: TShape;
    LMB: TPanel;
    MouseRightBG: TShape;
    MouseMiddleBG: TShape;
    SettingsItem: TMenuItem;
    MouseLeft: TShape;
    HistoryBoxBG: TShape;
    UpdateTimer: TTimer;
    VPressed: TShape;
    Alt: TPanel;
    AltPressed: TShape;
    XPressed: TShape;
    CPressed: TShape;
    ZPressed: TShape;
    WinPressed: TShape;
    SpaceBarPressed: TShape;
    ShiftPressed: TShape;
    CtrlPressed: TShape;
    QPressed: TShape;
    WPressed: TShape;
    EPressed: TShape;
    RPressed: TShape;
    FPressed: TShape;
    SPressed: TShape;
    Tab: TPanel;
    HistoryBox: TListBox;
    HideItem: TMenuItem;
    KeyLabel: TLabel;
    CapsLock: TPanel;
    D: TPanel;
    A: TPanel;
    V: TPanel;
    X: TPanel;
    C: TPanel;
    Z: TPanel;
    Win: TPanel;
    SpaceBar: TPanel;
    Shift: TPanel;
    Ctrl: TPanel;
    Q: TPanel;
    W: TPanel;
    E: TPanel;
    R: TPanel;
    F: TPanel;
    S: TPanel;
    KeyboardMain: TImage;
    KeyboardPanel: TPanel;
    MousePanel: TPanel;
    HistoryPanel: TPanel;
    MainPopUpMenu: TPopupMenu;
    Key: TShape;
    TabPressed: TShape;
    procedure ClearItemClick(Sender: TObject);
    procedure DetachItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HideItemClick(Sender: TObject);
    procedure InvertItemClick(Sender: TObject);
    procedure ExitItemClick(Sender: TObject);
    procedure KeyboardPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure KeyboardPanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure KeyboardPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SettingsItemClick(Sender: TObject);

    procedure InvertColors();

    procedure CheckAllButtons();

    procedure AddHistory(str: string);
    procedure UpdateTimerTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  PVInput: TPVInput;

  keyboardDLL: THandle;

  MouseHook : HHOOK = 0;
  KeyboardHook : HHOOK = 0;
  KeyboardHookLowLevel : HHOOK = 0;

  hookFunc: procedure (Switch: Boolean; HandleProg: HWND) stdcall;

  isHistoryShowing: boolean;
  isInvertedColor: boolean;

  MAX_HISTORY_SIZE: LongInt;

  isHistoryDetached: boolean;

  isInvokerSupportEnabled: boolean;

  isBeingMoved: boolean;
  prevX, prevY: longInt;

const
  HookMsg = WM_USER+$125;

  WH_MOUSE_LL = 14;
  WH_KEYBOARD = 2;

  // Mouse Event Codes
  WM_LBUTTONDOWN = $0201;
  WM_LBUTTONUP = $0202;

  WM_RBUTTONDOWN = $0204;
  WM_RBUTTONUP = $0205;

  WM_MOUSEMOVE = $0200;

  WM_MOUSEWHEEL = $020A; // the mouse wheel is rotated
  WM_MOUSEHWHEEL = $020E; // the mouse's horizontal scroll wheel is tiltedor rotated

  WM_MOUSEWHEELDOWN = $0207;
  WM_MOUSEWHEELUP = $0208;

  ENGLISH_LAYOUT = 67699721;

type
  PKBDLLHOOKSTRUCT = ^KBDLLHOOKSTRUCT ;
  KBDLLHOOKSTRUCT  =
  record
    vkCode,
    scanCode,
    flags,
    time: DWORD;

    dwExtraInfo: ULONG_PTR;
  end;

implementation

uses DetachedHistoryWindow, InvokerSpellsWindow;

{$R *.lfm}

function IsBitSet(const AValueToCheck, ABitIndex: LongInt): Boolean;
begin
  Result := AValueToCheck and (1 shl ABitIndex) <> 0;
end;

procedure TPVInput.CheckAllButtons();
begin
  if (not(IsBitSet(GetKeyState(VK_TAB), 15))) then
  begin
    // Tab's up
    TabPressed.Visible:=false;
  end;
  if (not(IsBitSet(GetKeyState(VK_CAPITAL), 15))) then
  begin
    CapsLockPressed.Visible:=false;
  end;
  if (not(IsBitSet(GetKeyState(VK_LSHIFT), 15))) then
  begin
    ShiftPressed.Visible:=false;
  end;
  if (not(IsBitSet(GetKeyState(VK_LCONTROL), 15))) then
  begin
    CtrlPressed.Visible:=false;
  end;
  if (not(IsBitSet(GetKeyState(VK_LWIN), 15))) then
  begin
    WinPressed.Visible:=false;
  end;
  if (not(IsBitSet(GetKeyState(VK_MENU), 15))) then
  begin
    AltPressed.Visible:=false;
  end;
  if (not(IsBitSet(GetKeyState(VK_Q), 15))) then
  begin
    QPressed.Visible:=false;
  end;
  if (not(IsBitSet(GetKeyState(VK_W), 15))) then
  begin
    WPressed.Visible:=false;
  end;
  if (not(IsBitSet(GetKeyState(VK_E), 15))) then
  begin
    EPressed.Visible:=false;
  end;
  if (not(IsBitSet(GetKeyState(VK_R), 15))) then
  begin
    RPressed.Visible:=false;
  end;
  if (not(IsBitSet(GetKeyState(VK_A), 15))) then
  begin
    APressed.Visible:=false;
  end;
  if (not(IsBitSet(GetKeyState(VK_S), 15))) then
  begin
    SPressed.Visible:=false;
  end;
  if (not(IsBitSet(GetKeyState(VK_D), 15))) then
  begin
    DPressed.Visible:=false;
  end;
  if (not(IsBitSet(GetKeyState(VK_F), 15))) then
  begin
    FPressed.Visible:=false;
  end;
  if (not(IsBitSet(GetKeyState(VK_Z), 15))) then
  begin
    ZPressed.Visible:=false;
  end;
  if (not(IsBitSet(GetKeyState(VK_X), 15))) then
  begin
    XPressed.Visible:=false;
  end;
  if (not(IsBitSet(GetKeyState(VK_C), 15))) then
  begin
    CPressed.Visible:=false;
  end;
  if (not(IsBitSet(GetKeyState(VK_V), 15))) then
  begin
   VPressed.Visible:=false;
  end;
  if (not(IsBitSet(GetKeyState(VK_SPACE), 15))) then
  begin
    SpaceBarPressed.Visible:=false;
  end;
  if (not(IsBitSet(GetKeyState(VK_LBUTTON), 15))) then
  begin
    MouseLeft.Visible:=false;
  end;
  if (not(IsBitSet(GetKeyState(VK_RBUTTON), 15))) then
  begin
    MouseRight.Visible:=false;
  end;
  if (not(IsBitSet(GetKeyState(VK_MBUTTON), 15))) then
  begin
    MouseMiddle.Visible:=false;
  end;
end;

function OccurrencesOfChar(const S: string; const C: char): longInt;
var
  i: longInt;
begin
  result := 0;
  for i := 1 to Length(S) do
    if S[i] = C then
      inc(result);
end;

procedure CheckSpell(var Q, W, E: longInt; str: string);
begin
  Q := OccurrencesOfChar(str, 'Q');
  W := OccurrencesOfChar(str, 'W');
  E := OccurrencesOfChar(str, 'E');
end;

procedure CheckAndSendSpell(str: string);
var
  Q, W, E: longInt;
begin
  Q := 0; W := 0; E := 0;

  CheckSpell(Q, W, E, str);

  if (Q = 3) then
  begin
    InvokerSpellsForm.ShowSpell('Cold Snap');
  end
  else if (Q = 2) and (W = 1) then
  begin
    InvokerSpellsForm.ShowSpell('Ghost Walk');
  end
  else if (Q = 1) and (W = 2) then
  begin
    InvokerSpellsForm.ShowSpell('Tornado');
  end
  else if (W = 3) then
  begin
    InvokerSpellsForm.ShowSpell('EMP');
  end
  else if (E = 3) then
  begin
    InvokerSpellsForm.ShowSpell('Sun Strike');
  end
  else if (E = 2) and (W = 1) then
  begin
    InvokerSpellsForm.ShowSpell('Chaos Meteor');
  end
  else if (W = 2) and (E = 1) then
  begin
    InvokerSpellsForm.ShowSpell('Alacrity');
  end
  else if (Q = 2) and (E = 1) then
  begin
    InvokerSpellsForm.ShowSpell('Ice Wall');
  end
  else if (E = 2) and (Q = 1) then
  begin
    InvokerSpellsForm.ShowSpell('Forge Spirit');
  end
  else if (W = 1) and (E = 1) and (Q = 1) then
  begin
    InvokerSpellsForm.ShowSpell('Deafening Blast');
  end
end;

function LowLevelKeyboardProc(nCode : LongInt; AWParam : WPARAM; ALParam : LPARAM) : LRESULT; stdcall;
var
  p : PKBDLLHOOKSTRUCT;
  keyCodeGot: longInt;
  keyUp, isHolding: boolean;

  i: longInt;
  spellStr: string;

begin
  p := PKBDLLHOOKSTRUCT(ALParam);

  if (AWParam = WM_KEYDOWN) or (AWParam = WM_SYSKEYDOWN) then
  begin
    keyUp := false;
  end
  else
  begin
     keyUp := true;
  end;

  KeyCodeGot := p^.vkCode;

  isHolding := GetKeyState(KeyCodeGot) < 0;

  KeyCodeGot := p^.vkCode;

    if (keyCodeGot = VK_Q) then
    begin
      PVInput.QPressed.Visible := not(keyUp);
      if not(keyUp) and not(isHolding) then PVInput.AddHistory('Q');
    end
    else if (keyCodeGot = VK_W) then
    begin
      PVInput.WPressed.Visible := not(keyUp);
      if not(keyUp) and not(isHolding) then PVInput.AddHistory('W');
    end
    else if (keyCodeGot = VK_E) then
    begin
      PVInput.EPressed.Visible := not(keyUp);
      if not(keyUp) and not(isHolding) then PVInput.AddHistory('E');
    end
    else if (keyCodeGot = VK_R) then
    begin
      PVInput.RPressed.Visible := not(keyUp);

      if not(keyUp) and not(isHolding) then
      begin
        PVInput.AddHistory('R');

        if (isInvokerSupportEnabled) then
        begin
          for i := 0 to PVInput.InvokerKeys.Count-1 do
          begin
             SpellStr := SpellStr + PVInput.InvokerKeys.Items[i];
          end;
        end;

        CheckAndSendSpell(SpellStr);
      end;

    end
    else if (keyCodeGot = VK_A) then
    begin
      PVInput.APressed.Visible := not(keyUp);
      if not(keyUp) and not(isHolding) then PVInput.AddHistory('A');
    end
    else if (keyCodeGot = VK_S) then
    begin
      PVInput.SPressed.Visible := not(keyUp);
      if not(keyUp) and not(isHolding) then PVInput.AddHistory('S');
    end
    else if (keyCodeGot = VK_D) then
    begin
      PVInput.DPressed.Visible := not(keyUp);
      if not(keyUp) and not(isHolding) then PVInput.AddHistory('D');
    end
    else if (keyCodeGot = VK_F) then
    begin
      PVInput.FPressed.Visible := not(keyUp);
      if not(keyUp) and not(isHolding) then PVInput.AddHistory('F');
    end
    else if (keyCodeGot = VK_Z) then
    begin
      PVInput.ZPressed.Visible := not(keyUp);
      if not(keyUp) and not(isHolding) then PVInput.AddHistory('Z');
    end
    else if (keyCodeGot = VK_X) then
    begin
      PVInput.XPressed.Visible := not(keyUp);
      if not(keyUp) and not(isHolding) then PVInput.AddHistory('X');
    end
    else if (keyCodeGot = VK_C) then
    begin
      PVInput.CPressed.Visible := not(keyUp);
      if not(keyUp) and not(isHolding) then PVInput.AddHistory('C');
    end
    else if (keyCodeGot = VK_V) then
    begin
      PVInput.VPressed.Visible := not(keyUp);
      if not(keyUp) and not(isHolding) then PVInput.AddHistory('V');
    end
    else if (keyCodeGot = VK_TAB) then
    begin
      PVInput.TabPressed.Visible := not(keyUp);
      if not(keyUp) and not(isHolding) then PVInput.AddHistory('Tab');
    end
    else if (keyCodeGot = VK_CAPITAL) then
    begin
      // CAPS LOCK HAS TO TOGGLE!!!!
      PVInput.CapsLockPressed.Visible := not(keyUp);
      if not(keyUp) and not(isHolding) then PVInput.AddHistory('Caps');
    end
    else if (keyCodeGot = 160) then
    begin
      PVInput.ShiftPressed.Visible := not(keyUp);
      if not(keyUp) and not(isHolding) then PVInput.AddHistory('Shift');
    end
    else if (keyCodeGot = 162) then
    begin
      PVInput.CtrlPressed.Visible := not(keyUp);
      if not(keyUp) and not(isHolding) then PVInput.AddHistory('Ctrl');
    end
    else if (keyCodeGot = 91) then
    begin
      PVInput.WinPressed.Visible := not(keyUp);
      if not(keyUp) and not(isHolding) then PVInput.AddHistory('Win');
    end
    else if (keyCodeGot = 164) then
    begin
      PVInput.AltPressed.Visible := not(keyUp);
      if not(keyUp) and not(isHolding) then PVInput.AddHistory('Alt');
    end
    else if (keyCodeGot = VK_SPACE) then
    begin
      PVInput.SpaceBarPressed.Visible := not(keyUp);
      if not(keyUp) and not(isHolding) then PVInput.AddHistory('Space');
    end;

  Result := CallNextHookEx(KeyboardHookLowLevel, nCode, AWParam, ALParam);
end;

procedure TPVInput.AddHistory(str: string);
begin
  HistoryBox.Items.Add(str);
  DetachedHistoryForm.DetachedInputHist.Items.Add(str);

  if (str = 'Q') or (str = 'W') or (str = 'E') then
  begin
    InvokerKeys.Items.Add(str);
    if (PVInput.InvokerKeys.Items.Count > 3) then
    begin
       PVInput.InvokerKeys.Items.Delete(0);
    end;
  end;

  if (PVInput.HistoryBox.Items.Count > MAX_HISTORY_SIZE) then
  begin
     PVInput.HistoryBox.Items.Delete(0);
     DetachedHistoryForm.DetachedInputHist.Items.Delete(0);
  end;
end;

procedure TPVInput.UpdateTimerTimer(Sender: TObject);
begin
  CheckAllButtons;

  (Sender as TTimer).Enabled:=false;
  (Sender as TTimer).Enabled:=true;
end;

procedure TPVInput.InvertColors();
var
  i, j: LongInt;
begin

  { SEEK CERTAIN TAG AND INVERT IT'S COLOR IN KEYBOARD PANEL }
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
              ((KeyboardPanel.Controls[i] as TPanel).Controls[j] as TShape).Brush.Color := InvertColor(((KeyboardPanel.Controls[i] as TPanel).Controls[j] as TShape).Brush.Color);
              ((KeyboardPanel.Controls[i] as TPanel).Controls[j] as TShape).Pen.Color := InvertColor(((KeyboardPanel.Controls[i] as TPanel).Controls[j] as TShape).Pen.Color);
            end
            else if (((KeyboardPanel.Controls[i] as TPanel).Controls[j] as TShape).Tag = 2) then // KEY PRESSED
            begin
              ((KeyboardPanel.Controls[i] as TPanel).Controls[j] as TShape).Brush.Color := InvertColor(((KeyboardPanel.Controls[i] as TPanel).Controls[j] as TShape).Brush.Color);
              ((KeyboardPanel.Controls[i] as TPanel).Controls[j] as TShape).Pen.Color := InvertColor(((KeyboardPanel.Controls[i] as TPanel).Controls[j] as TShape).Pen.Color);
            end;
          end
          else if ((KeyboardPanel.Controls[i] as TPanel).Controls[j] is TLabel) then
          begin
            if (((KeyboardPanel.Controls[i] as TPanel).Controls[j] as TLabel).Tag = 3) then // KEY LABEL
            begin
              ((KeyboardPanel.Controls[i] as TPanel).Controls[j] as TLabel).Font.Color := InvertColor(((KeyboardPanel.Controls[i] as TPanel).Controls[j] as TLabel).Font.Color);
            end
          end;
       end;
     end;
  end;
  { SEEK CERTAIN TAG AND INVERT IT'S COLOR IN KEYBOARD PANEL }

  { SEEK CERTAIN TAG AND INVERT IT'S COLOR IN MOUSE PANEL }
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
              ((MousePanel.Controls[i] as TPanel).Controls[j] as TShape).Brush.Color := InvertColor(((MousePanel.Controls[i] as TPanel).Controls[j] as TShape).Brush.Color);
              ((MousePanel.Controls[i] as TPanel).Controls[j] as TShape).Pen.Color := InvertColor(((MousePanel.Controls[i] as TPanel).Controls[j] as TShape).Pen.Color);
            end
            else if (((MousePanel.Controls[i] as TPanel).Controls[j] as TShape).Tag = 2) then // KEY PRESSED
            begin
              ((MousePanel.Controls[i] as TPanel).Controls[j] as TShape).Brush.Color := InvertColor(((MousePanel.Controls[i] as TPanel).Controls[j] as TShape).Brush.Color);
              ((MousePanel.Controls[i] as TPanel).Controls[j] as TShape).Pen.Color := InvertColor(((MousePanel.Controls[i] as TPanel).Controls[j] as TShape).Pen.Color);
            end;
          end
          else if ((MousePanel.Controls[i] as TPanel).Controls[j] is TLabel) then
          begin
            if (((MousePanel.Controls[i] as TPanel).Controls[j] as TLabel).Tag = 3) then // KEY LABEL
            begin
              ((MousePanel.Controls[i] as TPanel).Controls[j] as TLabel).Font.Color := InvertColor(((MousePanel.Controls[i] as TPanel).Controls[j] as TLabel).Font.Color);
            end
          end;
       end;
     end;
  end;
  { SEEK CERTAIN TAG AND INVERT IT'S COLOR IN MOUSE PANEL }

  SettingsForm.KeyBGColor.Color := PVInput.Key.Brush.Color; // KEY BG
  SettingsForm.KeyBGColor.Font.Color := SettingsForm.KeyBGColor.Color;

  SettingsForm.PressedColor.Color := PVInput.TabPressed.Brush.Color; // KEY PRESSED
  SettingsForm.PressedColor.Font.Color := SettingsForm.PressedColor.Color;

  SettingsForm.FontColor.Color := PVInput.KeyLabel.Font.Color; // FONT COLOR
  SettingsForm.FontColor.Font.Color := SettingsForm.FontColor.Color;

  SettingsForm.UpdateSettingsStrGrid();

end;

{ TPVInput }

function LowLevelMouseProc(nCode : LongInt; AWParam : WPARAM; ALParam : LPARAM) : LRESULT; stdcall;
begin
  if (AWParam = WM_LBUTTONDOWN) then
  begin
    PVInput.MouseLeft.Visible:=true;
    PVInput.AddHistory('LMB');
  end
  else if (AWParam = WM_LBUTTONUP) then
  begin
    PVInput.MouseLeft.Visible:=false;
  end
  else if (AWParam = WM_RBUTTONDOWN) then
  begin
    PVInput.MouseRight.Visible:=true;
    PVInput.AddHistory('RMB');
  end
  else if (AWParam = WM_RBUTTONUP) then
  begin
    PVInput.MouseRight.Visible:=false;
  end
  else if (AWParam = WM_MOUSEWHEELDOWN) then
  begin
    PVInput.MouseMiddle.Visible:=true;
    PVInput.AddHistory('MMB');
  end
  else if (AWParam = WM_MOUSEWHEELUP) then
  begin
    PVInput.MouseMiddle.Visible:=false;
  end;

  Result := CallNextHookEx(MouseHook, nCode, AWParam, ALParam);
end;

procedure TPVInput.FormCreate(Sender: TObject);
begin
  MAX_HISTORY_SIZE := 17;

  isHistoryShowing := true;
  isInvertedColor := false;
  isHistoryDetached := false;
  isBeingMoved := false;

  MouseHook := SetWindowsHookEx(WH_MOUSE_LL, @LowLevelMouseProc, HInstance, 0);
  KeyboardHookLowLevel := SetWindowsHookEx(WH_KEYBOARD_LL, @LowLevelKeyboardProc, HInstance, 0);

end;

procedure TPVInput.DetachItemClick(Sender: TObject);
begin
  if not(isHistoryDetached) then  // Detach it
  begin
    DetachedHistoryForm.Show;

    HistoryBox.Visible:=false;

    (Sender as TMenuItem).Caption:='Attach';

    DetachedHistoryForm.Height:=HistoryBox.Height;

    isHistoryDetached := true;

    HideItem.Enabled:=false;

    HistoryBoxBG.Visible := false;
  end
  else  // Attach it back
  begin
    DetachedHistoryForm.Hide;

    HistoryBox.Visible:=true;

    (Sender as TMenuItem).Caption:='Detach';

    isHistoryDetached := false;

    HideItem.Enabled := true;

    HistoryBoxBG.Visible := true;
  end;
end;

procedure TPVInput.ClearItemClick(Sender: TObject);
begin
  HistoryBox.Clear;
  DetachedHistoryForm.DetachedInputHist.Clear;
end;

procedure TPVInput.FormDestroy(Sender: TObject);
begin
  UnhookWindowsHookEx(MouseHook);
  UnhookWindowsHookEx(KeyboardHookLowLevel);
end;

procedure TPVInput.HideItemClick(Sender: TObject);
begin
  if (isHistoryShowing) then
  begin
     HistoryBox.Visible:=false;
     HideItem.Caption:='Show input history';
     isHistoryShowing := false;
     HistoryBoxBG.Visible:=false;
  end
  else
  begin
    HistoryBox.Visible:=true;
    HideItem.Caption:='Hide input history';
    isHistoryShowing := true;
    HistoryBoxBG.Visible:=true;
  end;
end;

procedure TPVInput.InvertItemClick(Sender: TObject);
begin
  if (isInvertedColor) then
  begin
     InvertColors();
     isInvertedColor := false;
  end
  else
  begin
     InvertColors();
     isInvertedColor := true;
  end;
end;

procedure TPVInput.ExitItemClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TPVInput.KeyboardPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = TMouseButton.mbLeft) then
  begin
    isBeingMoved := true;

    prevX := X;
    prevY := Y;
  end;
end;

procedure TPVInput.KeyboardPanelMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  diffX, diffY: longInt;
begin
  if (isBeingMoved) then
  begin
    diffX := X - prevX;
    diffY := Y - prevY;

    PVInput.Left:=PVInput.Left + diffX;
    PVInput.Top:=PVInput.Top + diffY;
  end;
end;

procedure TPVInput.KeyboardPanelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  isBeingMoved := false;
end;

procedure TPVInput.SettingsItemClick(Sender: TObject);
begin
  SettingsForm.Show();
  SettingsForm.Top:=Screen.Height div 2;
  SettingsForm.Left:=Screen.Width div 2;
end;

end.

