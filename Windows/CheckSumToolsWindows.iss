; ViaThinkSoft Checksum Tools Setup Script for InnoSetup
; by Daniel Marschall, ViaThinkSoft

[Setup]
AppName=ViaThinkSoft Checksum Tools
AppVerName=ViaThinkSoft Checksum Tools 1.2
AppVersion=1.2
AppCopyright=� Copyright 2017 - 2025 ViaThinkSoft
AppPublisher=ViaThinkSoft
AppPublisherURL=https://www.viathinksoft.com/
AppSupportURL=https://www.daniel-marschall.de/
AppUpdatesURL=https://www.viathinksoft.com/
DefaultDirName={autopf}\ViaThinkSoft Checksum Tools
DefaultGroupName=Checksum Tools
VersionInfoCompany=ViaThinkSoft
VersionInfoCopyright=� Copyright 2017 - 2025 ViaThinkSoft
VersionInfoDescription=Checksum Tools Setup
VersionInfoTextVersion=1.2.0.0
VersionInfoVersion=1.2
ChangesAssociations=yes
OutputDir=.
OutputBaseFilename=ChecksumToolsSetup
LicenseFile=..\LICENSE
; Configure Sign Tool in InnoSetup at "Tools => Configure Sign Tools" (adjust the path to your SVN repository location)
; Name    = sign_single   
; Command = "C:\SVN\...\sign_single.bat" $f
SignTool=sign_single
SignedUninstaller=yes
ArchitecturesAllowed=x86compatible x64compatible
ArchitecturesInstallIn64BitMode=x64compatible

[CustomMessages]
Assoc=File associations:

[Languages]
;Name: de; MessagesFile: "compiler:Languages\German.isl"

[LangOptions]
LanguageName=English
LanguageID=$0409

[Components]
Name: "dirchecker";  Description: "Recursive directory checker";  Types: full compact custom
Name: "editor"; Description: "Checksum file editor"; Types: full

[Tasks]
Name: fileassocSFV;  Description: "{cm:AssocFileExtension,'SFV Checksum File','.sfv'}"; GroupDescription: "{cm:Assoc}"; Components: editor
Name: fileassocMD5;  Description: "{cm:AssocFileExtension,'MD5 Checksum File','.md5'}"; GroupDescription: "{cm:Assoc}"; Components: editor

[Files]
Source: "win32\MD5DirCheck.exe";  DestDir: "{app}"; Flags: ignoreversion signonce; Components: dirchecker; Check: not Is64BitInstallMode
Source: "win32\SFVCorrect.exe";   DestDir: "{app}"; Flags: ignoreversion signonce; Components: editor; Check: not Is64BitInstallMode
Source: "win64\MD5DirCheck.exe";  DestDir: "{app}"; Flags: ignoreversion signonce; Components: dirchecker; Check: Is64BitInstallMode
Source: "win64\SFVCorrect.exe";   DestDir: "{app}"; Flags: ignoreversion signonce; Components: editor; Check: Is64BitInstallMode
Source: "ChecksumFileIcon.ico";   DestDir: "{app}"; Flags: ignoreversion; Components: editor

[Dirs]

[Icons]
Name: "{group}\Recursive directory checker";  Filename: "{app}\MD5DirCheck.exe";  Components: dirchecker
Name: "{group}\Checksum file editor"; Filename: "{app}\SFVCorrect.exe"; Components: editor

[Run]
Filename: "{app}\MD5DirCheck.exe";  Description: "Run recursive directory checker";  Flags: nowait postinstall skipifsilent; Components: dirchecker
Filename: "{app}\SFVCorrect.exe"; Description: "Run checksum file editor"; Flags: nowait postinstall skipifsilent unchecked; Components: editor

[Registry]
Root: HKCR; Subkey: ".sfv";                                       ValueName: ""; ValueType: string; ValueData: "VtsChecksumSFV";                   Flags: uninsdeletekey;      Components: editor;  Tasks: fileassocSFV
Root: HKCR; Subkey: ".sfv\ShellNew";                              ValueName: "ItemName"; ValueType: string; ValueData: "SFV checksum file";                                    Components: editor;  Tasks: fileassocSFV
Root: HKCR; Subkey: ".sfv\ShellNew";                              ValueName: "NullFile"; ValueType: string; ValueData: "";                                                     Components: editor;  Tasks: fileassocSFV
Root: HKCR; Subkey: "VtsChecksumSFV";                             ValueName: ""; ValueType: string; ValueData: "SFV checksum File";                Flags: uninsdeletekey;      Components: editor;  Tasks: fileassocSFV
Root: HKCR; Subkey: "VtsChecksumSFV\DefaultIcon";                 ValueName: ""; ValueType: string; ValueData: "{app}\ChecksumFileIcon.ico";                                   Components: editor;  Tasks: fileassocSFV
Root: HKCR; Subkey: "VtsChecksumSFV\shell\open\command";          ValueName: ""; ValueType: string; ValueData: """{app}\SFVCorrect.exe"" ""%1""";                              Components: editor;  Tasks: fileassocSFV

Root: HKCR; Subkey: ".md5";                                       ValueName: ""; ValueType: string; ValueData: "VtsChecksumMD5";                   Flags: uninsdeletekey;      Components: editor;  Tasks: fileassocMD5
Root: HKCR; Subkey: ".md5\ShellNew";                              ValueName: "ItemName"; ValueType: string; ValueData: "MD5 checksum file";                                    Components: editor;  Tasks: fileassocMD5
Root: HKCR; Subkey: ".md5\ShellNew";                              ValueName: "NullFile"; ValueType: string; ValueData: "";                                                     Components: editor;  Tasks: fileassocMD5
Root: HKCR; Subkey: "VtsChecksumMD5";                             ValueName: ""; ValueType: string; ValueData: "MD5 checksum File";                Flags: uninsdeletekey;      Components: editor;  Tasks: fileassocMD5
Root: HKCR; Subkey: "VtsChecksumMD5\DefaultIcon";                 ValueName: ""; ValueType: string; ValueData: "{app}\ChecksumFileIcon.ico";                                   Components: editor;  Tasks: fileassocMD5
Root: HKCR; Subkey: "VtsChecksumMD5\shell\open\command";          ValueName: ""; ValueType: string; ValueData: """{app}\SFVCorrect.exe"" ""%1""";                              Components: editor;  Tasks: fileassocMD5


[Code]
function InitializeSetup(): Boolean;
begin
  if CheckForMutexes('VtsChecksumToolsSetup')=false then
  begin
    Createmutex('VtsChecksumToolsSetup');
    Result := true;
  end
  else
  begin
    Result := False;
  end;
end;
function IsAnyComponentSelected: Boolean;
var
  I: Integer;
begin
  // Source: https://stackoverflow.com/questions/20691583/innosetup-if-no-components-are-selected-go-back-to-components-page
  Result := False;
  for I := 0 to WizardForm.ComponentsList.Items.Count - 1 do
    if WizardForm.ComponentsList.Checked[I] then
    begin
      Result := True;
      Exit;
    end;
end;
function NextButtonClick(PageID: Integer): Boolean;
begin
  Result:= True;
  if PageID = wpSelectComponents then
  begin
    if not IsAnyComponentSelected then
    begin
      MsgBox('No items selected, please select at least one item', mbError, MB_OK);
      Result := False;
    end;
  end;
end;
