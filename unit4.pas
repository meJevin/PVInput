unit Unit4;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TInvokerSpellsForm }

  TInvokerSpellsForm = class(TForm)
    PaintBox1: TPaintBox;

    Spell1: TPanel;

    ColdSnap, EMP, Tornado,
    Alacrity, ChaosMeteor,
    DeafeningBlast, ForgeSpirit,
    GhostWalk, IceWall, SunStrike: TBitmap;

    Timer1: TTimer;

    procedure ColdSnapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure ColdSnapMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);

    procedure ColdSnapMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure LoadToBMP(var BMP: TBitmap; path: string);
    procedure PaintBox1Paint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);

    procedure ShowSpell(spellName: string);

  private
    { private declarations }
  public
    { public declarations }
  end;

var
  InvokerSpellsForm: TInvokerSpellsForm;


  mousePrevX, mousePrevY: longInt;

  isBeingMoved: boolean;

  currSpellName: string;

implementation

{$R *.lfm}

{ TInvokerSpellsForm }

procedure TInvokerSpellsForm.ShowSpell(spellName: string);
begin
  currSpellName := spellName;

  Timer1.Enabled := false;
  Timer1.Interval := 2000;
  Timer1.Enabled := true;

  PaintBox1.Invalidate;
  PaintBox1.Visible := true;
end;

procedure TInvokerSpellsForm.LoadToBMP(var BMP: TBitmap; path: string);
var
  Jpg: TJpegImage;
begin
  Jpg := TJpegImage.Create;
  try
    Jpg.LoadFromFile(path);
    BMP.Assign(Jpg);
  except
    on err:exception do
    showMessage(err.message);
  end;

  Jpg.Free;
end;

procedure TInvokerSpellsForm.PaintBox1Paint(Sender: TObject);
begin
  if (currSpellName = 'Cold Snap') then
  begin
    PaintBox1.Canvas.Draw(0,0, ColdSnap);
  end
  else if (currSpellName = 'EMP') then
  begin
    PaintBox1.Canvas.Draw(0,0, EMP);
  end
  else if (currSpellName = 'Alacrity') then
  begin
    PaintBox1.Canvas.Draw(0,0, Alacrity);
  end
  else if (currSpellName = 'Chaos Meteor') then
  begin
    PaintBox1.Canvas.Draw(0,0, ChaosMeteor);
  end
  else if (currSpellName = 'Forge Spirit') then
  begin
    PaintBox1.Canvas.Draw(0,0, ForgeSpirit);
  end
  else if (currSpellName = 'Tornado') then
  begin
    PaintBox1.Canvas.Draw(0,0, Tornado);
  end
  else if (currSpellName = 'Ice Wall') then
  begin
    PaintBox1.Canvas.Draw(0,0, IceWall);
  end
  else if (currSpellName = 'Sun Strike') then
  begin
    PaintBox1.Canvas.Draw(0,0, SunStrike);
  end
  else if (currSpellName = 'Ghost Walk') then
  begin
    PaintBox1.Canvas.Draw(0,0, GhostWalk);
  end
  else if (currSpellName = 'Deafening Blast') then
  begin
    PaintBox1.Canvas.Draw(0,0, DeafeningBlast);
  end;
end;

procedure TInvokerSpellsForm.Timer1Timer(Sender: TObject);
begin
  PaintBox1.Visible:=false;
end;

procedure TInvokerSpellsForm.ColdSnapMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = TMouseButton.mbLeft) then
  begin
    isBeingMoved := true;

    mousePrevX := X;
    mousePrevY := Y;
  end;
end;

procedure TInvokerSpellsForm.ColdSnapMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  diffX, diffY: longInt;
begin
  if (isBeingMoved) then
  begin
    diffX := X - mousePrevX;
    diffY := Y - mousePrevY;

    InvokerSpellsForm.Left:=InvokerSpellsForm.Left + diffX;
    InvokerSpellsForm.Top:=InvokerSpellsForm.Top + diffY;
  end;
end;

procedure TInvokerSpellsForm.ColdSnapMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  isBeingMoved := false;
end;

procedure TInvokerSpellsForm.FormCreate(Sender: TObject);
begin
  InvokerSpellsForm.Width:=68;

  currSpellName := 'Cold Snap';

  EMP := TBitmap.Create;
  LoadToBMP(EMP, 'pics\InvokerSpells\EMP.jpg');
  EMP.Width:=58; EMP.Height:=58;

  Tornado := TBitmap.Create;
  LoadToBMP(Tornado, 'pics\InvokerSpells\Tornado.jpg');
  Tornado.Width:=58; Tornado.Height:=58;

  Alacrity := TBitmap.Create;
  LoadToBMP(Alacrity, 'pics\InvokerSpells\Alacrity.jpg');
  Alacrity.Width:=58; Alacrity.Height:=58;

  ChaosMeteor := TBitmap.Create;
  LoadToBMP(ChaosMeteor, 'pics\InvokerSpells\ChaosMeteor.jpg');
  ChaosMeteor.Width:=58; ChaosMeteor.Height:=58;

  DeafeningBlast := TBitmap.Create;
  LoadToBMP(DeafeningBlast, 'pics\InvokerSpells\DeafeningBlast.jpg');
  DeafeningBlast.Width:=58; DeafeningBlast.Height:=58;

  ForgeSpirit := TBitmap.Create;
  LoadToBMP(ForgeSpirit, 'pics\InvokerSpells\ForgeSpirit.jpg');
  ForgeSpirit.Width:=58; ForgeSpirit.Height:=58;

  GhostWalk := TBitmap.Create;
  LoadToBMP(GhostWalk, 'pics\InvokerSpells\GhostWalk.jpg');
  GhostWalk.Width:=58; GhostWalk.Height:=58;

  IceWall := TBitmap.Create;
  LoadToBMP(IceWall, 'pics\InvokerSpells\IceWall.jpg');
  IceWall.Width:=58; IceWall.Height:=58;

  SunStrike := TBitmap.Create;
  LoadToBMP(SunStrike, 'pics\InvokerSpells\SunStrike.jpg');
  SunStrike.Width:=58; SunStrike.Height:=58;

  ColdSnap := TBitmap.Create;
  LoadToBMP(ColdSnap, 'pics\InvokerSpells\ColdSnap.jpg');
  ColdSnap.Width:=58; ColdSnap.Height:=58;

  Timer1.Interval := 4000; // 30Hz refresh rate
end;

procedure TInvokerSpellsForm.FormDestroy(Sender: TObject);
begin
  ColdSnap.Free;
  EMP.Free;
  Tornado.Free;
  Alacrity.Free;
  ChaosMeteor.Free;
  DeafeningBlast.Free;
  ForgeSpirit.Free;
  GhostWalk.Free;
  IceWall.Free;
  SunStrike.Free;
end;

end.

