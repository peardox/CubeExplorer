unit Unit1;

{$mode objfpc}{$H+}
 {$define usestage}
 {$define skipcube}

interface

uses
  Classes, SysUtils, Math, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Grids, ComCtrls, Buttons, Menus, RTTIGrids, CastleControl,
  CastleViewport, CastleSceneCore, CastleScene, CastleProjection,
  CastleRenderOptions, CastleCameras, CastleVectors, CastleDebugTransform,
  CastleControls, CastleImages, CastleGLImages, CastleColors, CastleRectangles,
  CastleNotifications, CastleUIControls, CastleFilesUtils, CastleLCLUtils,
  {$ifdef usestage}
  Staging, CastleTransform,
  {$endif}
  CastleGLUtils, CastleQuaternions, CastleTimeUtils, CastleLog, CastleKeysMouse;

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
    Button4: TButton;
    ComboBox1: TComboBox;
    ListView1: TListView;
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    MedievalFantasyBookMenu: TMenuItem;
    LoadFileMenu: TMenuItem;
    CrockMenu: TMenuItem;
    NavigationMenu: TMenuItem;
    OpenDialog1: TOpenDialog;
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
    procedure Button4Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure LoadFileMenuClick(Sender: TObject);
    procedure MedievalFantasyBookMenuClick(Sender: TObject);
    procedure CrockMenuClick(Sender: TObject);
    procedure NavigationMenuClick(Sender: TObject);
    procedure QuaterniusBuildingsMenuClick(Sender: TObject);
    procedure QuaterniusPropsMenuClick(Sender: TObject);
    procedure WindowMotion(Sender: TObject; const Event: TInputMotion);
    procedure WindowPress(Sender: TObject; const Event: TInputPressRelease);
    procedure WindowUpdate(Sender: TObject);
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
    function  CreateSpriteImage(const SourceScene: TCastleScene; const TextureWidth: Cardinal; const TextureHeight: Cardinal; const isTransparent: Boolean = False): TCastleImage;
    procedure LoadMenuScene(const AFileName: String; const AddToViewport: Boolean = True);
    function  Pos2DTo3D(const AXpos: Single; const AYpos: Single): String;
    procedure StopMotion(const Anim: String; const FrameCount: Integer; const addWatermark: Boolean = True);
    procedure NextDirection;
    procedure UpdateInfoPanel;
  public
    {$ifdef usestage}
    Stage: TCastleScene;
    {$endif}
    Scene: TCastleScene;
    Viewport: TCastleViewport;
    Debug: TDebugTransformBox;
    infoNotifications: TCastleNotifications;
    gYAngle: Single;
    gSceneRot: Integer;
    gUsingShear: Boolean;
    gViewportBackground: TVector4;
    SpriteSize: Integer;
  end;

var
  Form1: TForm1;
  gOrientation: TVector4;

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
            gOrientation := Vector4(0, 0, 0, 0);
            Rotation := gOrientation;
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
var
  ASize: Integer;
begin
  gViewportBackground := Vector4(1,1,1,1);
  TrackBar1.Position := 90;

  Viewport := TCastleViewport.Create(Application);
  Viewport.BackgroundColor := gViewportBackground;
  Viewport.FullSize := True;
  Viewport.AutoCamera := False;
  Viewport.Setup2D;
  Viewport.NavigationType := ntNone;
  Viewport.AssignDefaultCamera;
  Viewport.Camera.Orthographic.Width := 2;
  Viewport.Camera.Orthographic.Height := 2;
  Viewport.Camera.Orthographic.Origin := Vector2(0.5, 0.5);
  Viewport.Camera.Orthographic.Scale := 1; // Viewport.Camera.Orthographic.Height / Viewport.Camera.Orthographic.Width;
  Viewport.Camera.ProjectionType := ptOrthographic;

  {$ifdef usestage}
  if not (Stage = nil) then
    begin
      FreeAndNil(Stage);
    end;
  {$endif}

  {$ifdef skipcube}
  CrockMenuClick(Sender);
  {$else}
  Scene := TCastleScene.Create(Application);
  Scene.Setup2D;
  Scene.RenderOptions.MinificationFilter := minNearest;
  Scene.RenderOptions.MagnificationFilter := magNearest;
  Scene.PrimitiveGeometry := pgBox;
  Scene.Normalize;
  Caption := 'CubeExplorer : Default Cube';
  {$endif}

  Debug := TDebugTransformBox.Create(Application);
  Debug.Parent := Scene;
  Debug.BoxColor := Vector4(0,1,0, 1);
  Debug.Exists := False;
  DebugBoxMenu.Checked := Debug.Exists;

  {$ifndef skipcube}
  Viewport.Items.Add(Scene);
  Viewport.Items.MainScene := Scene;
  {$endif}

  Window.Controls.InsertFront(Viewport);

  infoNotifications := TCastleNotifications.Create(Application);
  infoNotifications.MaxMessages := 2;
  infoNotifications.Color := Vector4(0,0,0,1);
  infoNotifications.OutlineColor := Vector4(1,1,1,1);
  infoNotifications.Anchor(hpLeft, 10);
  infoNotifications.Anchor(vpBottom, 10);
  Window.Controls.InsertFront(infoNotifications);

  SpriteSize := 1024;

  AddInfo('Sprite Size', Integer(SpriteSize));
  AddInfo('Window Width', Window.Width);
  AddInfo('Window Height', Window.Height);
  AddInfo('Projection (Y Axis)', gYAngle);
  AddInfo('Ortho Width', Viewport.Camera.Orthographic.Width);
  AddInfo('Ortho Height', Viewport.Camera.Orthographic.Height);
  AddInfo('Ortho Effective Width', Viewport.Camera.Orthographic.EffectiveWidth);
  AddInfo('Ortho Effective Height', Viewport.Camera.Orthographic.EffectiveHeight);
  AddInfo('Ortho Scale', Viewport.Camera.Orthographic.Scale);

  AddInfo('BBox 0', Scene.BoundingBox.Data[0].ToString);
  AddInfo('BBox 1', Scene.BoundingBox.Data[1].ToString);
  AddInfo('Translation', Scene.Translation.ToString);
  AddInfo('Center', Scene.Center.ToString);
  AddInfo('Rotation', Scene.Rotation.ToString);
  AddInfo('3D Scale', Scene.Scale.ToString);

  AddInfo('Pos A', Pos2DTo3D(0, 0));
  AddInfo('Pos B', Pos2DTo3D(Window.Width, Window.Height));
  AddInfo('Mouse', 'Unknown');

  ComboBox1.Items.Clear;
  ASize := 32;
  while ASize <= GLFeatures.MaxRenderbufferSize do
    begin
      ComboBox1.Items.Add('Sprite Size : ' + IntToStr(ASize) + ' x ' + IntToStr(ASize));
      ASize *= 2;
    end;

  ComboBox1.ItemIndex := 5;

end;

function TForm1.Pos2DTo3D(const AXpos: Single; const AYpos: Single): String;
var
  res: String;
  PlanePosition: TVector3;
begin
  res := 'Unknown';
  if Viewport.PositionToCameraPlane(Vector2(AXpos, AYpos), False, 0, PlanePosition) then
    begin
      res := PlanePosition.ToString;
    end;

  Result := res;
end;

procedure TForm1.UpdateInfoPanel;
begin
  UpdateInfo('Window Width', Window.Width);
  UpdateInfo('Window Height', Window.Height);
  UpdateInfo('Ortho Width', Viewport.Camera.Orthographic.Width);
  UpdateInfo('Ortho Height', Viewport.Camera.Orthographic.Height);
  UpdateInfo('Ortho Effective Width', Viewport.Camera.Orthographic.EffectiveWidth);
  UpdateInfo('Ortho Effective Height', Viewport.Camera.Orthographic.EffectiveHeight);
  UpdateInfo('Ortho Scale', Viewport.Camera.Orthographic.Scale);
  UpdateInfo('BBox 0', Scene.BoundingBox.Data[0].ToString);
  UpdateInfo('BBox 1', Scene.BoundingBox.Data[1].ToString);
  UpdateInfo('Translation', Scene.Translation.ToString);
  UpdateInfo('Center', Scene.Center.ToString);
  UpdateInfo('Rotation', Scene.Rotation.ToString);
  UpdateInfo('3D Scale', Scene.Scale.ToString);
  UpdateInfo('Pos A', Pos2DTo3D(0, 0));
  UpdateInfo('Pos B', Pos2DTo3D(Window.Width, Window.Height));
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
      {$ifdef usestage}
      Sprite := CreateSpriteImage(Stage, SpriteSize, SpriteSize, True);
      {$else}
      Sprite := CreateSpriteImage(Scene, SpriteSize, SpriteSize);
      {$endif}
      if not(Sprite = nil) then
        begin
          SName := FileNameAutoInc('grab_%4.4d.jpg');
          SaveImage(Sprite, SName);
          infoNotifications.Show('Saved : ' + SName);
          FreeAndNil(Sprite);
        end;
    end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Q: TQuaternion;
begin
  if not(Scene = nil) then
    begin
      Dec(gSceneRot);
      if (gSceneRot < 0) then
        gSceneRot := MaxSceneRot;
      Q := QuatFromAxisAngle(gOrientation);
      Q := Q * QuatFromAxisAngle(Vector4(0, 1, 0, 2 * Pi * (gSceneRot / (MaxSceneRot + 1))));
      Scene.Rotation := Q.ToAxisAngle;
    end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  Q: TQuaternion;
begin
  if not(Scene = nil) then
    begin
      Inc(gSceneRot);
      if (gSceneRot > MaxSceneRot) then
        gSceneRot := 0;
      Q := QuatFromAxisAngle(gOrientation);
      Q := Q * QuatFromAxisAngle(Vector4(0, 1, 0, 2 * Pi * (gSceneRot / (MaxSceneRot + 1))));
      Scene.Rotation := Q.ToAxisAngle;
    end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  StopMotion('Walk_In_Place', 8, True); // 0.79
//  StopMotion('WalkBackwards', 8); // 2.00
//  StopMotion('Hit', 8); // 0.96
//  StopMotion('Death', 8); // 1.67
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  SpriteSize := Trunc(Power(2, ComboBox1.ItemIndex + 5));
  UpdateInfo('Sprite Size', SpriteSize);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  AMenuItem: TMenuItem;
  Idx: Integer;
begin
  InitializeLog;
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
  gUsingShear := False; // Placeholder - alter with shear when @michalis writes it
  gSceneRot := 0;
  Scene := nil;
  Debug := nil;
  {$ifdef usestage}
  Stage := nil;
  {$endif}
  RadioGroup1.ItemIndex := 14;
  KeyPreview := True;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  Panel4.Height := Button2.Height;
  Button2.Width := Trunc(Panel4.Width / 2);
  Button3.Width := Panel4.Width - Button2.Width;
//  Panel3.Width := Panel3.Height - Panel4.Height;
end;

procedure TForm1.LoadMenuScene(const AFileName: String; const AddToViewport: Boolean = True);
var
  OldDebug: Boolean;
begin
  {$ifdef usestage}
  if not (Stage = nil) then
    begin
      FreeAndNil(Stage);
    end;
  {$endif}

  if not (Scene = nil) then
    begin
      if not (Debug = nil) then
        begin
          OldDebug := Debug.Exists;
          FreeAndNil(Debug);
        end;
      FreeAndNil(Scene);
    end;
  Scene := TCastleScene.Create(Application);
  Scene.Setup2D;
  Scene.ReceiveShadowVolumes := True;
  Scene.RenderOptions.PhongShading := True;
  Scene.RenderOptions.ShadowSampling := ssSimple;
  Scene.RenderOptions.MinificationFilter := minNearest;
  Scene.RenderOptions.MagnificationFilter := magNearest;
  Scene.Load(AFileName);
  Scene.Normalize;

  UpdateInfo('BBox 0', Scene.BoundingBox.Data[0].ToString);
  UpdateInfo('BBox 1', Scene.BoundingBox.Data[1].ToString);
  UpdateInfo('Translation', Scene.Translation.ToString);
  UpdateInfo('Center', Scene.Center.ToString);
  UpdateInfo('3D Scale', Scene.Scale.ToString);

  Debug := TDebugTransformBox.Create(Application);
  Debug.Parent := Scene;
  Debug.BoxColor := Vector4(0,1,0, 1);
  Debug.Exists := OldDebug;
  DebugBoxMenu.Checked := Debug.Exists;

  if AddToViewport then
    begin
      Viewport.Items.Add(Scene);
      Viewport.Items.MainScene := Scene;
    end;
end;

procedure TForm1.LoadFileMenuClick(Sender: TObject);
begin
  OpenDialog1.Filter := '3D Models|*.gltf;*.glb;*.obj;';
  if OpenDialog1.Execute then
    begin
      LoadMenuScene(OpenDialog1.Filename);
      Caption := 'CubeExplorer : ' + OpenDialog1.Filename;
      {$ifdef usestage}
      Stage := LoadStage(Scene);
      Viewport.Items.UseHeadlight := hlOff;
      Viewport.Items.MainScene := Stage;
      Viewport.Items.Remove(Scene);
      Viewport.Items.Add(Stage);
      {$endif}
    end;
end;

procedure TForm1.YogYogMenuClick(Sender: TObject);
begin
  LoadMenuScene('castle-data:/oblique.glb');
  Caption := 'CubeExplorer : Yogyog Castle';
  {$ifdef usestage}
  Stage := LoadStage(Scene);
  Viewport.Items.UseHeadlight := hlOff;
  Viewport.Items.MainScene := Stage;
  Viewport.Items.Remove(Scene);
  Viewport.Items.Add(Stage);
  {$endif}
end;

procedure TForm1.MedievalFantasyBookMenuClick(Sender: TObject);
begin
//  LoadMenuScene('castle-data:/medieval_fantasy_book/scene.gltf');
  LoadMenuScene('C:\Assets\Self\up.glb');
//  LoadMenuScene('C:\Assets\Sketchfab\alpha_wolf\scene.gltf');
  Caption := 'CubeExplorer : Medieval Fantasy Book';
  {$ifdef usestage}
  Stage := LoadStage(Scene, -0.25);
  Viewport.Items.UseHeadlight := hlOff;
  Viewport.Items.MainScene := Stage;
  Viewport.Items.Remove(Scene);
  Viewport.Items.Add(Stage);
  {$endif}
end;

procedure TForm1.CrockMenuClick(Sender: TObject);
const
  offWhite: Single = 1.0;
begin
//  LoadMenuScene('C:\Assets\Sketchfab\crocodile_with_animation\crockrotate.glb');
//  LoadMenuScene('C:\Assets\3drt\paid\Dragon-boss\DragonBoss.glb');
//  LoadMenuScene('C:\Assets\RPG Characters - Nov 2020\GLTF\GroupShotPeardox.glb', False);
  LoadMenuScene('C:\Assets\Sketchfab\alpha_wolf\scene.gltf');
  {$ifdef usestage}
  Stage := LoadStage(Scene, 0, Vector3(offWhite, offWhite, offWhite));
  Stage.PrepareResources([prSpatial, prRenderSelf, prRenderClones, prScreenEffects],
      True,
      Viewport.PrepareParams);

  Viewport.Items.UseHeadlight := hlOff;
  Viewport.Items.MainScene := Stage;

  Viewport.Items.Add(Stage);
  {$endif}
  Caption := 'CubeExplorer : Crock';
  Viewport.Camera.Orthographic.Scale := 0.52;
//  Scene.TimePlayingSpeed := 0.1;
//  Scene.PlayAnimation('Armature|Howl', True, True);
    Scene.ForceAnimationPose('Armature|Howl', 0, False, True);
    Scene.StopAnimation;
//  Scene.Normalize;
end;

procedure TForm1.NextDirection;
var
  Q: TQuaternion;
begin
  if not(Scene = nil) then
    begin
      Dec(gSceneRot);
      if (gSceneRot < 0) then
        gSceneRot := MaxSceneRot;
      Q := QuatFromAxisAngle(gOrientation);
      Q := Q * QuatFromAxisAngle(Vector4(0, 1, 0, 2 * Pi * (gSceneRot / (MaxSceneRot + 1))));
      Scene.Rotation := Q.ToAxisAngle;
    end;
end;

procedure TForm1.StopMotion(const Anim: String; const FrameCount: Integer; const addWatermark: Boolean = True);
var
  Frame: Integer;
  Angle: Integer;
  Watermark: TCastleImage;
  Sprite: TCastleImage;
  TextureAtlas: TCastleImage;
  ProcTimer: Int64;
const
  TestSize: Integer = 2048;
  AngleCount: Integer = 8;
begin
  ProcTimer := CastleGetTickCount64;
  Scene.StopAnimation;

  TextureAtlas := TRGBAlphaImage.Create(TestSize, TestSize);
  Watermark := LoadImage('castle-data:/NoWatermark.png', [TRGBAlphaImage]) as TRGBAlphaImage;

  for Angle := 0 to AngleCount - 1 do
    begin
      NextDirection;
      for Frame := 0 to FrameCount - 1 do
        begin
          Scene.ForceAnimationPose(Anim, (Frame / FrameCount) * Scene.AnimationDuration(Anim), False, True);
          Scene.StopAnimation;
          if AddWatermark then
            Sprite := CreateSpriteImage(Scene, TestSize, TestSize, True)
          else
            Sprite := CreateSpriteImage(Scene, TestSize, TestSize, False);
          if not(Sprite = nil) then
            begin
              Sprite.Resize(TestSize div 8, TestSize div 8, riLanczos);
              if AddWatermark then
                TextureAtlas.DrawFrom(Watermark, Frame * TestSize div 8, Angle * TestSize div 8, 0, 0, TestSize div 8, TestSize div 8, dmOverwrite);
              TextureAtlas.DrawFrom(Sprite, Frame * TestSize div 8, Angle * TestSize div 8, 0, 0, TestSize div 8, TestSize div 8, dmBlendSmart);
              FreeAndNil(Sprite);
            end;
        end;
    end;
  SaveImage(TextureAtlas, Anim + '.png');
  FreeAndNil(TextureAtlas);
  FreeAndNil(Watermark);

  ProcTimer := CastleGetTickCount64 - ProcTimer;
  infoNotifications.Show('Saved : ' + Anim);
  infoNotifications.Show('Animation time = ' + FormatFloat('####0.000', ProcTimer / 1000) + ' seconds');

end;

procedure TForm1.QuaterniusBuildingsMenuClick(Sender: TObject);
begin
  LoadMenuScene('castle-data:/Medieval_Village_Pack/Buildings/gltf/' + QuaterniusBuildings[Integer(TComponent(Sender).Tag)]);
  Caption := 'CubeExplorer : Buildings : ' + QuaterniusBuildings[Integer(TComponent(Sender).Tag)];
  {$ifdef usestage}
  Stage := LoadStage(Scene);
  Viewport.Items.UseHeadlight := hlOff;
  Viewport.Items.MainScene := Stage;
  Viewport.Items.Remove(Scene);
  Viewport.Items.Add(Stage);
  {$endif}
end;

procedure TForm1.QuaterniusPropsMenuClick(Sender: TObject);
begin
  LoadMenuScene('castle-data:/Medieval_Village_Pack/Props/gltf/' + QuaterniusProps[Integer(TComponent(Sender).Tag)]);
  Caption := 'CubeExplorer : Props : ' + QuaterniusProps[Integer(TComponent(Sender).Tag)];
  {$ifdef usestage}
  Stage := LoadStage(Scene);
  Viewport.Items.UseHeadlight := hlOff;
  Viewport.Items.MainScene := Stage;
  Viewport.Items.Remove(Scene);
  Viewport.Items.Add(Stage);
  {$endif}
end;

procedure TForm1.WindowResize(Sender: TObject);
begin
  {
  Viewport.Camera.Orthographic.Scale := Min(
            Viewport.Camera.Orthographic.EffectiveWidth / Window.Width,
            Viewport.Camera.Orthographic.EffectiveHeight / Window.Height);

  UpdateInfoPanel;
  }
end;

procedure TForm1.WindowMotion(Sender: TObject; const Event: TInputMotion);
begin
  UpdateInfo('Mouse', Pos2DTo3D(Event.Position.X, Event.Position.Y));
end;

procedure TForm1.WindowPress(Sender: TObject; const Event: TInputPressRelease);
begin
//  UpdateInfoPanel;
end;

procedure TForm1.WindowUpdate(Sender: TObject);
begin
  UpdateInfoPanel;
end;

procedure TForm1.NavigationMenuClick(Sender: TObject);
begin
  if Viewport.NavigationType = ntNone then
    begin
      Viewport.NavigationType := ntExamine;
      NavigationMenu.Checked := True;
    end
  else
    begin
      if Viewport.Navigation is TCastleExamineNavigation then
        (Viewport.Navigation as TCastleExamineNavigation).StopRotating;
      Viewport.NavigationType := ntNone;
      NavigationMenu.Checked := False;
    end;

end;

procedure TForm1.DebugBoxMenuClick(Sender: TObject);
begin
  Debug.Exists := not Debug.Exists;
  DebugBoxMenu.Checked := Debug.Exists;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  gUsingShear := False;
  if RadioGroup1.Items[RadioGroup1.ItemIndex] = 'Trackbar' then
    begin
    TrackBar1.Enabled := True;
    gYAngle := -Pi * (TrackBar1.Position / TrackBar1.Max);
    end
  else if RadioGroup1.Items[RadioGroup1.ItemIndex] = 'Cavalier' then
    begin
    gYAngle := -2; // Placeholder - alter with shear when @michalis writes it
    gUsingShear := True;
    end
  else if RadioGroup1.Items[RadioGroup1.ItemIndex] = 'Cabinet' then
    begin
    gYAngle := -2; // Placeholder - alter with shear when @michalis writes it
    gUsingShear := True;
    end
  else if RadioGroup1.Items[RadioGroup1.ItemIndex] = 'Military' then
    begin
    gYAngle := -2; // Placeholder - alter with shear when @michalis writes it
    gUsingShear := True;
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
        11: gYAngle := -Sqrt(2);
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

function TForm1.CreateSpriteImage(const SourceScene: TCastleScene; const TextureWidth: Cardinal; const TextureHeight: Cardinal; const isTransparent: Boolean = False): TCastleImage;
var
  SourceViewport: TCastleViewport;
  GrabScene: TCastleScene;
  ViewportRect: TRectangle;
  Image: TDrawableImage;
begin
  SourceViewport := nil;

  if not(Scene = nil) and (TextureWidth > 0) and (TextureHeight > 0) then
    begin
      try
        try
          Image := TDrawableImage.Create(TRGBAlphaImage.Create(TextureWidth, TextureHeight), true, true);
          Image.RenderToImageBegin;

          GrabScene := SourceScene.Clone(nil);

          SourceViewport := TCastleViewport.Create(nil);
          SourceViewport.Width := TextureWidth;
          SourceViewport.Height := TextureHeight;
          if isTransparent then
            SourceViewport.Transparent := True
          else
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

          WriteLnLog('Scale : ' + FloatToStr(SourceViewport.Camera.Orthographic.Scale));

          SourceViewport.Items := ViewPort.Items;
          ViewportRect := Rectangle(0, 0, TextureWidth, TextureHeight);
          Window.Container.RenderControl(SourceViewport,ViewportRect);

          Image.RenderToImageEnd;

          if not False { Application.OpenGLES } then
          begin
            try
              Result := Image.GetContents(TRGBAlphaImage);
            except
              on E : Exception do
                begin
                  ShowMessage(E.ClassName + LineEnding + E.Message);
                end;
            end;
          end;

        except
          on E : Exception do
            begin
              ShowMessage(E.ClassName + LineEnding + E.Message);
            end;
        end;
      finally
        FreeAndNil(GrabScene);
        FreeAndNil(SourceViewport);
        FreeAndNil(Image);
      end;
    end;
end;

end.

