unit DetachedHistoryWindow;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TDetachedHistoryForm }

  TDetachedHistoryForm = class(TForm)
    DetachedInputHist: TListBox;
    SizePanelBottom: TPanel;
    procedure FormCreate(Sender: TObject);

    procedure DetachedInputHistMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure DetachedInputHistMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);

    procedure DetachedInputHistMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SizePanelBottomMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SizePanelBottomMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SizePanelBottomMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

  private
    { private declarations }
  public
    { public declarations }
  end;

var
  DetachedHistoryForm: TDetachedHistoryForm;

  mousePrevX, mousePrevY: longInt;

  mouseResizePrevX, mouseResizePrevY: longInt;

  isBeingMoved: boolean;
  isBeingResized: boolean;
implementation

uses MainWindow;

{$R *.lfm}

{ TDetachedHistoryForm }

procedure TDetachedHistoryForm.DetachedInputHistMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = TMouseButton.mbLeft) then
  begin
    isBeingMoved := true;

    mousePrevX := X;
    mousePrevY := Y;
  end;
end;

procedure TDetachedHistoryForm.DetachedInputHistMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  diffX, diffY: longInt;
begin
  if (isBeingMoved) then
  begin
    diffX := X - mousePrevX;
    diffY := Y - mousePrevY;

    DetachedHistoryForm.Left:=DetachedHistoryForm.Left + diffX;
    DetachedHistoryForm.Top:=DetachedHistoryForm.Top + diffY;
  end;
end;

procedure TDetachedHistoryForm.DetachedInputHistMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  isBeingMoved := false;
end;

procedure TDetachedHistoryForm.SizePanelBottomMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = TMouseButton.mbLeft) then
  begin
    isBeingResized := true;

    mouseResizePrevX := X;
    mouseResizePrevY := Y;
  end;
end;

procedure TDetachedHistoryForm.SizePanelBottomMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  diffY: longInt;
begin
  if (isBeingResized) then
  begin
    diffY := Y - mouseResizePrevY;

    DetachedHistoryForm.Height:=DetachedHistoryForm.Height + diffY;
  end;
end;

procedure TDetachedHistoryForm.SizePanelBottomMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  isBeingResized := false;
end;


procedure TDetachedHistoryForm.FormCreate(Sender: TObject);
begin
  isBeingMoved := false;

  DetachedHistoryForm.Constraints.MinHeight:=Screen.Height div 10;
  DetachedHistoryForm.Constraints.MaxHeight:=Screen.Height - 300;
end;

end.

