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
  { TCastleSceneHelper }

  TCastleSceneHelper = class helper for TCastleScene
  public
     procedure Normalize;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ListView1: TListView;
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    Panel3: TPanel;
    Panel4: TPanel;
    QuaterniusBuildingsMenu: TMenuItem;
    QuaterniusPropsMenu: TMenuItem;
    QuaterniusMenu: TMenuItem;
    YogYogMenu: TMenuItem;
    DebugBoxMenu: TMenuItem;
    Panel2: TPanel;
    RadioGroup1: TRadioGroup;
    TrackBar1: TTrackBar;
    Window: TCastleControlBase;
    Panel1: TPanel;
    Splitter1: TSplitter;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure QuaterniusBuildingsMenuClick(Sender: TObject);
    procedure QuaterniusPropsMenuClick(Sender: TObject);
    procedure YogYogMenuClick(Sender: TObject);
    procedure DebugBoxMenuClick(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure WindowBeforeRender(Sender: TObject);
    procedure WindowOpen(Sender: TObject);
    procedure WindowResize(Sender: TObject);
  private
    procedure ViewFromRadius(const ARadius: Single; const ADirection: TVector3);
    procedure AddInfo(const AName: String; const AValue: Integer);
    procedure AddInfo(const AName: String; const AValue: Single);
    procedure AddInfo(const AName: String; const AValue: String);
    procedure UpdateInfo(const AName: String; const AValue: Integer);
    procedure UpdateInfo(const AName: String; const AValue: Single);
    procedure UpdateInfo(const AName: String; const AValue: String);
    function  CreateSpriteTexture(const SourceScene: TCastleScene; const TextureWidth: Cardinal; const TextureHeight: Cardinal): TCastleImage;
    procedure LoadMenuScene(const AFileName: String);
  public
    Scene: TCastleScene;
    Viewport: TCastleViewport;
    Debug: TDebugTransformBox;
    infoNotifications: TCastleNotifications;
    gYAngle: Single;
    gSceneRot: Integer;
  end;

var
  Form1: TForm1;

const
  MaxSceneRot: Integer = 7;
  InfoFloatFormat: String = '#.######';
  QuaterniusBuildings: Array [0..9] of String = ('Bell_Tower.glb', 'Blacksmith.glb', 'House_1.glb', 'House_2.glb', 'House_3.glb', 'House_4.glb', 'Inn.glb', 'Mill.glb', 'Sawmill.glb', 'Stable.glb');
  QuaterniusProps: Array [0..33] of String = ('Bag.glb', 'Bag_Open.glb', 'Bags.glb', 'Barrel.glb', 'Bell.glb', 'Bench_1.glb', 'Bench_2.glb', 'Bonfire.glb', 'Bonfire_Lit.glb', 'Cart.glb', 'Cauldron.glb', 'Crate.glb', 'Door_Round.glb', 'Door_Straight.glb', 'Fence.glb', 'Gazebo.glb', 'Hay.glb', 'MarketStand_1.glb', 'MarketStand_2.glb', 'Package_1.glb', 'Package_2.glb', 'Path_Square.glb', 'Path_Straight.glb', 'Rock_1.glb', 'Rock_2.glb', 'Rock_3.glb', 'Sawmill_saw.glb', 'Smoke.glb', 'Stairs.glb', 'Well.glb', 'Window_1.glb', 'Window_2.glb', 'Window_3.glb', 'Window_4.glb');

implementation

{$R *.lfm}

{ TCastleSceneHelper }

{
  Normalize - Center the model in a 1x1x1 cube
}
procedure TCastleSceneHelper.Normalize;
begin
  if not(RootNode = nil) then
    begin
    if not BoundingBox.IsEmptyOrZero then
      begin
        if BoundingBox.MaxSize > 0 then
          begin
            Center := Vector3(Min(BoundingBox.Data[0].X, BoundingBox.Data[1].X) + (BoundingBox.SizeX / 2),
                              Min(BoundingBox.Data[0].Y, BoundingBox.Data[1].Y) + (BoundingBox.SizeY / 2),
                              Min(BoundingBox.Data[0].Z, BoundingBox.Data[1].Z) + (BoundingBox.SizeZ / 2));
            Scale := Vector3(1 / BoundingBox.MaxSize,
                             1 / BoundingBox.MaxSize,
                             1 / BoundingBox.MaxSize);
            Translation := -Center;
          end;
      end;
    end;
end;

{ TForm1 }

procedure TForm1.AddInfo(const AName: String; const AValue: Integer);
begin
  AddInfo(AName, IntToStr(AValue));
end;

procedure TForm1.AddInfo(const AName: String; const AValue: Single);
begin
  AddInfo(AName, FormatFloat(InfoFloatFormat, AValue));
end;

procedure TForm1.AddInfo(const AName: String; const AValue: String);
var
  vNewItem: TListItem;
begin
  vNewItem := ListView1.Items.Add;
  vNewItem.Caption := AName;
  vNewItem.SubItems.Add(AValue);
end;

procedure TForm1.UpdateInfo(const AName: String; const AValue: Integer);
begin
  UpdateInfo(AName, IntToStr(AValue));
end;

procedure TForm1.UpdateInfo(const AName: String; const AValue: Single);
begin
  UpdateInfo(AName, FormatFloat(InfoFloatFormat, AValue));
end;

procedure TForm1.UpdateInfo(const AName: String; const AValue: String);
var
  idx: Integer;
begin
  for idx := 0 to ListView1.Items.Count -1 do
    begin
      if ListView1.Items[idx].Caption = AName then
        ListView1.Items[idx].SubItems[0] := AValue;
    end;
end;

procedure TForm1.WindowOpen(Sender: TObject);
begin
  TrackBar1.Position := 90;

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := True;
  Viewport.AutoCamera := False;

  Viewport.Transparent := True;
  Viewport.NavigationType := ntNone;
  Viewport.AssignDefaultCamera;
  Viewport.Camera.Orthographic.Width := 2;
  Viewport.Camera.Orthographic.Height := 2;
  Viewport.Camera.Orthographic.Origin := Vector2(0.5, 0.5);
  Viewport.Camera.Orthographic.Scale := Viewport.Camera.Orthographic.Height / Viewport.Camera.Orthographic.Width;
  Viewport.Camera.ProjectionType := ptOrthographic;

  Scene := TCastleScene.Create(Application);
  Scene.PrimitiveGeometry := pgBox;
  Scene.Normalize;
  Caption := 'CubeExplorer : Default Cube';

  Debug := TDebugTransformBox.Create(Application);
  Debug.Parent := Scene;
  Debug.BoxColor := Vector4(0,1,0, 1);
  Debug.Exists := True;
  DebugBoxMenu.Checked := Debug.Exists;

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

  AddInfo('BBox 0', Scene.BoundingBox.Data[0].ToString);
  AddInfo('BBox 1', Scene.BoundingBox.Data[1].ToString);
  AddInfo('Translation', Scene.Translation.ToString);
  AddInfo('Center', Scene.Center.ToString);
  AddInfo('Scale', Scene.Scale.ToString);
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

procedure TForm1.WindowBeforeRender(Sender: TObject);
begin
  if not(Scene = nil) then
    begin
      ViewFromRadius(2, Vector3(-1, gYAngle, -1));
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

procedure TForm1.Button2Click(Sender: TObject);
begin
  if not(Scene = nil) then
    begin
      Dec(gSceneRot);
      if (gSceneRot < 0) then
        gSceneRot := MaxSceneRot;
      Scene.Rotation := Vector4(0, 1, 0, 2 * Pi * (gSceneRot / (MaxSceneRot + 1)));
    end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if not(Scene = nil) then
    begin
      Inc(gSceneRot);
      if (gSceneRot > MaxSceneRot) then
        gSceneRot := 0;
      Scene.Rotation := Vector4(0, 1, 0, 2 * Pi * (gSceneRot / (MaxSceneRot + 1)));
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  AMenuItem: TMenuItem;
  Idx: Integer;
begin
  for Idx := 0 to Length(QuaterniusBuildings) - 1 do
    begin
      AMenuItem:=TMenuItem.Create(QuaterniusBuildingsMenu);
      AMenuItem.Caption:=QuaterniusBuildings[Idx];
      AMenuItem.Tag := Idx;
      AMenuItem.OnClick := @QuaterniusBuildingsMenuClick;
      QuaterniusBuildingsMenu.Add(AMenuItem);
    end;
  for Idx := 0 to Length(QuaterniusProps) - 1 do
    begin
      AMenuItem:=TMenuItem.Create(QuaterniusPropsMenu);
      AMenuItem.Caption:=QuaterniusProps[Idx];
      AMenuItem.Tag := Idx;
      AMenuItem.OnClick := @QuaterniusPropsMenuClick;
      QuaterniusPropsMenu.Add(AMenuItem);
    end;
  TrackBar1.Enabled := False;
  gYAngle := -1;
  gSceneRot := 0;
  RadioGroup1.ItemIndex := 14;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  Panel4.Height := Button2.Height;
  Button2.Width := Trunc(Panel4.Width / 2);
  Button3.Width := Panel4.Width - Button2.Width;
end;

procedure TForm1.LoadMenuScene(const AFileName: String);
var
  OldDebug: Boolean;
begin
  if not (Scene = nil) then
    begin
      OldDebug := Debug.Exists;
      FreeAndNil(Debug);
      FreeAndNil(Scene);
    end;
  Scene := TCastleScene.Create(Application);
  Scene.Load(AFileName);
  Scene.Normalize;

  UpdateInfo('BBox 0', Scene.BoundingBox.Data[0].ToString);
  UpdateInfo('BBox 1', Scene.BoundingBox.Data[1].ToString);
  UpdateInfo('Translation', Scene.Translation.ToString);
  UpdateInfo('Center', Scene.Center.ToString);
  UpdateInfo('Scale', Scene.Scale.ToString);

  Debug := TDebugTransformBox.Create(Application);
  Debug.Parent := Scene;
  Debug.BoxColor := Vector4(0,1,0, 1);
  Debug.Exists := OldDebug;
  DebugBoxMenu.Checked := Debug.Exists;

  Viewport.Items.Add(Scene);
  Viewport.Items.MainScene := Scene;
end;

procedure TForm1.QuaterniusBuildingsMenuClick(Sender: TObject);
begin
  LoadMenuScene('castle-data:/Medieval_Village_Pack/Buildings/gltf/' + QuaterniusBuildings[Integer(TComponent(Sender).Tag)]);
  Caption := 'CubeExplorer : Buildings : ' + QuaterniusBuildings[Integer(TComponent(Sender).Tag)];
end;

procedure TForm1.QuaterniusPropsMenuClick(Sender: TObject);
begin
  LoadMenuScene('castle-data:/Medieval_Village_Pack/Props/gltf/' + QuaterniusProps[Integer(TComponent(Sender).Tag)]);
  Caption := 'CubeExplorer : Props : ' + QuaterniusProps[Integer(TComponent(Sender).Tag)];
end;

procedure TForm1.YogYogMenuClick(Sender: TObject);
begin
  LoadMenuScene('castle-data:/oblique.glb');
  Caption := 'CubeExplorer : Yogyog Castle';
end;

procedure TForm1.DebugBoxMenuClick(Sender: TObject);
begin
  Debug.Exists := not Debug.Exists;
  DebugBoxMenu.Checked := Debug.Exists;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  if RadioGroup1.Items[RadioGroup1.ItemIndex] = 'Trackbar' then
    begin
    TrackBar1.Enabled := True;
    gYAngle := -Pi * (TrackBar1.Position / TrackBar1.Max);
    end
  else if RadioGroup1.Items[RadioGroup1.ItemIndex] = 'Cavalier' then
    begin
    gYAngle := -2; // Placeholder - alter with shear when @michalis writes it
    end
  else if RadioGroup1.Items[RadioGroup1.ItemIndex] = 'Cabinet' then
    begin
    gYAngle := -2; // Placeholder - alter with shear when @michalis writes it
    end
  else if RadioGroup1.Items[RadioGroup1.ItemIndex] = 'Military' then
    begin
    gYAngle := -2; // Placeholder - alter with shear when @michalis writes it
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

