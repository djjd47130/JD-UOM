; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#define MyAppName "JD Convert"
#define MyAppVersion "1.3"
#define MyAppPublisher "JD Software"
#define MyAppURL "http://jerrydodge.com"
#define MyAppExeName "JDConvert.exe"

[Setup]
; NOTE: The value of AppId uniquely identifies this application. Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{5FD3E4EB-8033-4069-911F-DBF436F9295C}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={autopf}\{#MyAppPublisher}\{#MyAppName}
DefaultGroupName={#MyAppName}
; Uncomment the following line to run in non administrative install mode (install for current user only.)
;PrivilegesRequired=lowest
PrivilegesRequiredOverridesAllowed=dialog
OutputBaseFilename=JDConvert Setup
SetupIconFile=..\JDConvert.ico
Compression=lzma
SolidCompression=yes
WizardStyle=modern
UninstallDisplayIcon={app}\JDConvert.exe,0
AppModifyPath={app}\JDConvert Setup.exe
CloseApplications=force

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Types]
Name: "full"; Description: "Full Installation"
Name: "compact"; Description: "Compact Installation"
Name: "custom"; Description: "Custom Installation"; Flags: iscustom

[Components]
Name: "jdconvert"; Description: "JD Convert Application"; Types: full compact custom; Flags: fixed
Name: "fontawesome"; Description: "Install Font Awesome"; Types: full compact custom; Flags: fixed
Name: "help"; Description: "JD Convert Help File"; Types: full compact custom; Flags: fixed
Name: "systemuoms"; Description: "System UOMs"; Types: full
Name: "systemuoms\distance"; Description: "Distance UOMs"; Types: full compact custom; Flags: disablenouninstallwarning fixed
Name: "systemuoms\area"; Description: "Area UOMs"; Types: full; Flags: disablenouninstallwarning
Name: "systemuoms\temp"; Description: "Temperature UOMs"; Types: full; Flags: disablenouninstallwarning

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: checkedonce

[Files]
; JD Convert Application
Source: "..\Win32\Release\{#MyAppExeName}"; DestDir: "{app}"; Components: jdconvert; Flags: ignoreversion
; System UOMs - BE SURE TO INCLUDE IN InstallDelete
; Also be sure to create reference in [Components]
Source: "UOMs\Distance.ini"; DestDir: "{app}\System"; Components: systemuoms\distance; Flags: ignoreversion
Source: "UOMs\Area.ini"; DestDir: "{app}\System"; Components: systemuoms\area; Flags: ignoreversion
Source: "UOMs\Temperature.ini"; DestDir: "{app}\System"; Components: systemuoms\temp; Flags: ignoreversion
; FontAwesome
Source: "FontAwesome.ttf"; DestDir: "{autofonts}"; FontInstall: "FontAwesome"; Components: fontawesome; Flags: onlyifdoesntexist uninsneveruninstall
; Help File
Source: "..\Help_output\HTML Help\JD Convert Help.chm"; DestDir: "{app}"; Components: help; Flags: ignoreversion  
Source: "{srcexe}"; DestDir: "{app}"; Flags: ignoreversion external
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[InstallDelete]
Type: files; Name: "{app}\JDConvert.exe"
Type: files; Name: "{app}\FontAwesome.ttf"
Type: files; Name: "{app}\JDConvert Help.chm"
Type: files; Name: "{app}\System\Distance.ini"
Type: files; Name: "{app}\System\Area.ini"
Type: files; Name: "{app}\System\Temperature.ini"

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{autodesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon 

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent





