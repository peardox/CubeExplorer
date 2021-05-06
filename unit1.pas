unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Grids, ComCtrls, Buttons, Menus, RTTIGrids, CastleControl,
  CastleViewport, CastleSceneCore, CastleScene, CastleProjection,
  CastleRenderOptions, CastleCameras, CastleVectors, CastleDebugTransform,
  CastleControls, CastleImages, CastleGLImages, CastleColors, CastleRectangles,
  CastleNotifications, CastleUIControls, CastleFilesUtils;

type
  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ListView1: TListView;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    Panel2: TPanel;
    RadioGroup1: TRadioGroup;
    TrackBar1: TTrackBar;
    Window: TCastleControlBase;
    Panel1: TPanel;
    Splitter1: TSplitter;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure WindowBeforeRender(Sender: TObject);
    procedure WindowOpen(Sender: TObject);
    procedure WindowResize(Sender: TObject);
    procedure WindowUpdate(Sender: TObject);
  private
    procedure ViewFromRadius(const ARadius: Single; const ADirection: TVector3);
    procedure AddInfo(const AName: String; const AValue: Integer);
    procedure AddInfo(const AName: String; const AValue: Single);
    procedure UpdateInfo(const AName: String; const AValue: Integer);
    procedure UpdateInfo(const AName: String; const AValue: Single);
    function  CreateSpriteTexture(const SourceScene: TCastleScene; const TextureWidth: Cardinal; const TextureHeight: Cardinal): TCastleImage;
  public
    Scene: TCastleScene;
    Viewport: TCastleViewport;
    Debug: TDebugTransformBox;
    infoNotifications: TCastleNotifications;
    gYAngle: Single;
  end;

var
  Form1: TForm1;

const
  InfoFloatFormat: String = '#.######';

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.AddInfo(const AName: String; const AValue: Integer);
var
  vNewItem: TListItem;
begin
  vNewItem := ListView1.Items.Add;
  vNewItem.Caption := AName;
  vNewItem.SubItems.Add(IntToStr(AValue));
end;

procedure TForm1.AddInfo(const AName: String; const AValue: Single);
var
  vNewItem: TListItem;
begin
  vNewItem := ListView1.Items.Add;
  vNewItem.Caption := AName;
  vNewItem.SubItems.Add(FormatFloat(InfoFloatFormat, AValue));
end;

procedure TForm1.UpdateInfo(const AName: String; const AValue: Integer);
var
  idx: Integer;
begin
  for idx := 0 to ListView1.Items.Count -1 do
    begin
      if ListView1.Items[idx].Caption = AName then
        ListView1.Items[idx].SubItems[0] := IntToStr(AValue);
    end;
end;

procedure TForm1.UpdateInfo(const AName: String; const AValue: Single);
var
  idx: Integer;
begin
  for idx := 0 to ListView1.Items.Count -1 do
    begin
      if ListView1.Items[idx].Caption = AName then
        ListView1.Items[idx].SubItems[0] := FormatFloat(InfoFloatFormat, AValue);
    end;
end;

procedure TForm1.WindowOpen(Sender: TObject);
begin
  TrackBar1.Position := 90;

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := True;
  Viewport.AutoCamera := True;

  Viewport.Transparent := True;
  Viewport.NavigationType := ntNone;
  Viewport.AssignDefaultCamera;
  Viewport.Camera.Orthographic.Width := 4;
  Viewport.Camera.Orthographic.Height := 4;
  Viewport.Camera.Orthographic.Origin := Vector2(0.5, 0.5);
  Viewport.Camera.Orthographic.Scale := Viewport.Camera.Orthographic.Height / Viewport.Camera.Orthographic.Width;
  Viewport.Camera.ProjectionType := ptOrthographic;

  Scene := TCastleScene.Create(Application);
  Scene.PrimitiveGeometry := pgBox;

  Debug := TDebugTransformBox.Create(Application);
  Debug.Parent := Scene;
  Debug.BoxColor := Vector4(0,1,0, 1);
  Debug.Exists := True;

  Viewport.Items.Add(Scene);
  Viewport.Items.MainScene := Scene;
  Window.Controls.InsertFront(Viewport);

  infoNotifications := TCastleNotifications.Create(Application);
  infoNotifications.MaxMessages := 1;
  infoNotifications.Anchor(hpLeft, 10);
  infoNotifications.Anchor(vpBottom, 10);
  Window.Controls.InsertFront(infoNotifications);

  AddInfo('Window Width', Window.Width);
  AddInfo('Window Height', Window.Height);
  AddInfo('Projection (Y Axis)', gYAngle);
  AddInfo('Ortho Width', Viewport.Camera.Orthographic.Width);
  AddInfo('Ortho Height', Viewport.Camera.Orthographic.Height);
  AddInfo('Ortho Scale', Viewport.Camera.Orthographic.Scale);
end;

procedure TForm1.WindowResize(Sender: TObject);
begin
  Viewport.Camera.Orthographic.Scale := Viewport.Camera.Orthographic.Height / Viewport.Camera.Orthographic.Width;

  UpdateInfo('Window Width', Window.Width);
  UpdateInfo('Window Height', Window.Height);
  UpdateInfo('Ortho Width', Viewport.Camera.Orthographic.Width);
  UpdateInfo('Ortho Height', Viewport.Camera.Orthographic.Height);
  UpdateInfo('Ortho Scale', Viewport.Camera.Orthographic.Scale);
end;

procedure TForm1.WindowUpdate(Sender: TObject);
begin

end;

procedure TForm1.WindowBeforeRender(Sender: TObject);
begin
  if not(Scene = nil) then
    begin
      ViewFromRadius(4, Vector3(-1, gYAngle, -1));
    end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Sprite: TCastleImage;
  SName: String;
begin
  if not (Scene = nil) then
    begin
      Sprite := CreateSpriteTexture(Scene, 1536, 1536);
      if not(Sprite = nil) then
        begin
          SName := FileNameAutoInc('grab_%4.4d.png');
          SaveImage(Sprite, SName);
          infoNotifications.Show('Saved : ' + SName);
          FreeAndNil(Sprite);
        end;
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TrackBar1.Enabled := False;
  gYAngle := -1;
  RadioGroup1.ItemIndex := 14;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  if not (Scene = nil) then
    begin
      FreeAndNil(Debug);
      FreeAndNil(Scene);
    end;
  Scene := TCastleScene.Create(Application);
  Scene.Load('castle-data:/Medieval_Village_Pack/Buildings/gltf/Inn.glb');
  Scene.Scale := Vector3(2/3.5, 2/3.5, 2/3.5); // Hacky unmeasured test
  Scene.Translation := Vector3(0, -1, 0); // Hacky unmeasured test

  Debug := TDebugTransformBox.Create(Application);
  Debug.Parent := Scene;
  Debug.BoxColor := Vector4(0,1,0, 1);
//  Debug.Exists := True;

  Viewport.Items.Add(Scene);
  Viewport.Items.MainScene := Scene;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  if RadioGroup1.Items[RadioGroup1.ItemIndex] = 'Trackbar' then
    begin
    TrackBar1.Enabled := True;
    gYAngle := -Pi * (TrackBar1.Position / TrackBar1.Max);
    end
  else if RadioGroup1.Items[RadioGroup1.ItemIndex] = 'Military' then
    begin
    gYAngle := -2;
    end
  else
    begin
      TrackBar1.Enabled := False;
      Case RadioGroup1.ItemIndex of
         0: gYAngle := -1;
         1: gYAngle := -2;
         2: gYAngle := -3;
         3: gYAngle := -1/3;
         4: gYAngle := -1/2;
         5: gYAngle := -2/3;
         6: gYAngle := -3/4;
         7: gYAngle := -Pi / 2;
         8: gYAngle := -Pi / 3;
         9: gYAngle := -Pi / 4;
        10: gYAngle := -Pi / 5;
        11: gYAngle := -Pi / 6;
        12: gYAngle := -Sqrt(2);
        13: gYAngle := -((1 + Sqrt(5)) / 2);
      end;
    end;

    UpdateInfo('Projection (Y Axis)', gYAngle);
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  if TrackBar1.Enabled then
    begin
      gYAngle := -Pi * (TrackBar1.Position / TrackBar1.Max);
      UpdateInfo('Projection (Y Axis)', gYAngle);
    end;
end;

procedure TForm1.ViewFromRadius(const ARadius: Single; const ADirection: TVector3);
var
  Spherical: TVector3;
begin
  if not(Scene = nil) then
    begin
      Spherical := -ADirection.Normalize;
      Spherical := Spherical * ARadius;
      Viewport.Camera.Up := Vector3(0, 1, 0);
      Viewport.Camera.Direction := ADirection;
      Viewport.Camera.Position  := Spherical;
    end;
end;

function TForm1.CreateSpriteTexture(const SourceScene: TCastleScene; const TextureWidth: Cardinal; const TextureHeight: Cardinal): TCastleImage;
var
  SourceViewport: TCastleViewport;
  RenderToTexture: TGLRenderToTexture;
  GrabScene: TCastleScene;
  ViewportRect: TRectangle;
begin
  SourceViewport := nil;
  RenderToTexture := nil;

  if not(Scene = nil) and (TextureWidth > 0) and (TextureHeight > 0) then
    begin
      try
        GrabScene := SourceScene.Clone(nil);

        SourceViewport := TCastleViewport.Create(nil);
        SourceViewport.Width := TextureWidth;
        SourceViewport.Height := TextureHeight;
        SourceViewport.BackgroundColor := Vector4(1,1,1,1);

        SourceViewport.Setup2D;
        SourceViewport.Camera.ProjectionType := ptOrthographic;
        SourceViewport.Camera.Orthographic.Origin := Viewport.Camera.Orthographic.Origin;
        SourceViewport.Camera.Up := Viewport.Camera.Up;
        SourceViewport.Camera.Direction := Viewport.Camera.Direction;
        SourceViewport.Camera.Position  := Viewport.Camera.Position;
        SourceViewport.Camera.Orthographic.Scale := Min(
          Viewport.Camera.Orthographic.EffectiveWidth / TextureWidth,
          Viewport.Camera.Orthographic.EffectiveHeight / TextureHeight);

        SourceViewport.Items := ViewPort.Items;

        RenderToTexture := TGLRenderToTexture.Create(TextureWidth, TextureHeight);
        RenderToTexture.Buffer := tbNone;
        RenderToTexture.GLContextOpen;
        RenderToTexture.RenderBegin;

        ViewportRect := Rectangle(0, 0, TextureWidth, TextureHeight);

        Window.Container.RenderControl(SourceViewport, ViewportRect);

        Result := SaveScreen_NoFlush(TRGBImage, ViewportRect, RenderToTexture.ColorBuffer);

        RenderToTexture.RenderEnd;
      finally
        FreeAndNil(RenderToTexture);
        FreeAndNil(GrabScene);
        FreeAndNil(SourceViewport);
      end;
    end;
end;

end.

