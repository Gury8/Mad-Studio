{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2023
  Unit: Common constant and variable library
}
unit common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls, LCLIntf, SynEditMiscClasses;

const
  programName = 'Mad Studio';
//  programVersion = ' v1.2.5 (i386-Win32)';
  programVersion = ' v1.2.5 x86_64-Win64';

type
  TPlayerResolution = (singleResolution, doubleResolution);

  fldType = array[0..3, 0..7, 0..220] of byte;
  fldFontSetType = array[0..7, 0..1023] of byte;
  fldAntic3FontSetType = array[0..7, 0..1280] of byte;
  charType = array[0..7, 0..7] of byte;
  charTypeAntic3 = array[0..7, 0..9] of byte;

  TAnticMode = record
    basicMode : byte;
    scanLines : byte;
    bits : byte;
  end;

  TListings = array[0..7, 0..8] of boolean;

  TAplAnim = (normal, extended, fixed52);

  TAntic4TileType = record
    dimX, dimY : byte;  // Antic mode 4 tile dimension
    selected : byte;    // Selected tile
    filename : string;  // Tile filename
    charValues : array[0..19] of byte;      // ATASCII values
    charInverse : array[0..19] of boolean;  // Inverse character flag
  end;

  TAntic4CharValueType = record
    tileSelected : byte;  // Selected tile
    x, y : byte;          // Tile value coordinates
    value : byte;         // Tile value
  end;

  TTileType = record
    tileIndex : byte;          // Tile index
    coordX, coordY : integer;  // Tile coordinates on Antic mode 4 screen
    x, y : byte;               // Tile character coordinates on Antic mode 4 screen
  end;

  TSetValuesType = record
    caption : string;
    warningText : string;
    valuesSet : boolean;
    editX, editY : byte;
    minEditX, minEditY : byte;
    maxEditX, maxEditY : byte;
    editText : string;
  end;

const
  // Supported graphics resolutions
  grMode40x24x4 = 3;
  grMode80x48x2 = 4;
  grMode80x48x4 = 5;
  grMode160x96x2 = 6;
  grMode160x96x4 = 7;
  grMode160x192x2 = 14;
  grMode160x192x4 = 15;
  grMode320x192x2 = 8;

  // Player sizes
  _PLAYER_SIZE_NORMAL = 0;
  _PLAYER_SIZE_DOUBLE = 1;
  _PLAYER_SIZE_QUADRUPLE = 3;

  // Character set normal/inverse flag
  fontSetNormal = true;
  fontSetInverse = false;

  formMain = 0;
  formSrcEdit = 1;
  formGraph = 2;
  formPmg = 3;
  formFont = 4;
  formAntic2 = 5;
  formAntic6 = 6;
  formDisplayList = 7;
  formAntic4 = 8;
  formAnimator = 9;
  formAntic3 = 10;
  formByteEditor = 11;
  formAntic4TileEditor = 12;

  tabPmgIndex = 0;
  tabDlIndex = 1;
  tabFontsIndex = 2;

  _CHARSET_UPPER = 1;
  _CHARSET_LOWER = 2;
  _CHARSET_UPPER_INV = 3;
  _CHARSET_LOWER_INV = 10;

  // Text mode sizes
  _TEXT_MODE_1_SIZE = 480;
  _TEXT_MODE_2_SIZE = 240;
  _ANTIC_MODE_4_SIZE = 960;
  _ANTIC_MODE_5_SIZE = 480;

  // Display list Antic mode and blank line codes
  _BLANK_LINES_01 = 0;
  _BLANK_LINES_02 = $10;
  _BLANK_LINES_03 = $20;
  _BLANK_LINES_04 = $30;
  _BLANK_LINES_05 = $40;
  _BLANK_LINES_06 = $50;
  _BLANK_LINES_07 = $60;
  _BLANK_LINES_08 = $70;
  _ANTIC_MODE_2 = 2;  // Text mode 0
  _ANTIC_MODE_6 = 6;  // Text mode 1
  _ANTIC_MODE_7 = 7;  // Text mode 2
  _ANTIC_MODE_8 = 8;  // Graphics mode 3 (40 x 24, 4 colors)

  // Display list constants
  _LMS          = $40;  // Load memory scan
  _JVB          = $41;  // Jump and wait for vertical blank

  // Display list commands
  dliBytes : array[0..16] of byte = (
    _BLANK_LINES_01,
    _BLANK_LINES_02,
    _BLANK_LINES_03,
    _BLANK_LINES_04,
    _BLANK_LINES_05,
    _BLANK_LINES_06,
    _BLANK_LINES_07,
    _BLANK_LINES_08,
    _ANTIC_MODE_2,
    _ANTIC_MODE_6,
    _ANTIC_MODE_7,
    _ANTIC_MODE_8,
    _LMS + _ANTIC_MODE_2,
    _LMS + _ANTIC_MODE_6,
    _LMS + _ANTIC_MODE_7,
    _LMS + _ANTIC_MODE_8,
    _JVB);

  // Display list Antic mode codes
  dliAnticModes : array[0..3] of byte = (
    _ANTIC_MODE_2,
    _ANTIC_MODE_6,
    _ANTIC_MODE_7,
    _ANTIC_MODE_8);

  // Display list LMS address + Antic mode
  dliLMSAnticModes : array[0..3] of byte = (
    _LMS + _ANTIC_MODE_2,
    _LMS + _ANTIC_MODE_6,
    _LMS + _ANTIC_MODE_7,
    _LMS + _ANTIC_MODE_8);

  _MAX_ANIM_FRAME_WIDTH = 15;
  _ANIM_MAX_LINES       = 58;
  _ANIM_APL_MAX_LINES   = 48;

  // Supported languages
  _ATARI_BASIC    = 0;
  _TURBO_BASIC_XL = 1;
  _MAD_PASCAL     = 2;
  _ACTION         = 3;
  _FAST_BASIC     = 4;
  _MADS           = 5;
  _MAC65          = 6;
  _CC65           = 7;
  _KICKC          = 8;

  _PM_WIDTH       = 7;   // Player line dimension - 1
  _MAX_PLAYER_POS = 56;  // Max. X position for multi-color players

  pmSize : array[0..3] of string[14] = ('normal size', 'double size', '', 'quadruple size');
  GPRIOR : array[0..3] of byte = (1, 4, 16, 32);

  _CHAR_DIM       = 7;
  _MAX_TILES      = 8;
  _MAX_TILE_CHARS = 20;

  _TILE_FUNCTION : array[0..9] of string[40] = (
    'Tile region select',
    'Tile draw',
    'Flip tile horizontally',
    'Flip tile vertically',
    'Invert bits of tile',
    'Rotate tile',
    'Fill tile character with color',
    'Clear tile character',
    'Clear tile',
    'Fill tile with color');

  _ANTIC_MODE_2_MAX_X = 39;
  _ANTIC_MODE_2_MAX_Y = 23;
  _ANTIC_MODE_4_MAX_X = 39;
  _ANTIC_MODE_4_MAX_Y = 23;
  _ANTIC_MODE_5_MAX_X = 39;
  _ANTIC_MODE_5_MAX_Y = 11;
  _ANTIC_MODE_6_MAX_X = 20;
  _ANTIC_MODE_6_MAX_Y = 23;
  _ANTIC_MODE_7_MAX_X = 20;
  _ANTIC_MODE_7_MAX_Y = 11;

  {$I 'missile_sizes.inc'}

var
  formId : byte;

  grMode : byte;
  grX, grY : integer;
  factX, factY : byte;
  players : array[0..3] of byte;
  playerSize : array[0..3] of byte;
  missileSizes : array[0..3] of byte;
  playerColor : array[0..3] of word;

  isOutputLog : boolean = true;

  strAtariBASICOutput : string;
  strMadPascalOutput : string;
  strMadsOutput : string;
  strCC65Output : string;
  strFastBasicOutput : string;
  strEffectusOutput : string;

  isAtariBASICUserLocation : boolean;
  strAtariBASICUserLocation : string;
  isMadPascalUserLocation : boolean;
  strMadPascalUserLocation : string;
  isMadsUserLocation : boolean;
  strMadsUserLocation : string;
  isMadsUserLocation02 : boolean;
  strMadsUserLocation02 : string;
  isCC65UserLocation : boolean;
  strCC65UserLocation : string;
  isFastBasicUserLocation : boolean;
  strFastBasicUserLocation : string;
  isEffectusUserLocation : boolean;
  strEffectusUserLocation : string;
  isKickCUserLocation : boolean;
  strKickCUserLocation : string;

  coltab : array[0..10] of TColor;
  colorMem : array[0..255] of TColor;
  colorValues : array[0..10] of byte;
  coltabFont : array[0..1] of TColor;

  coltabCopy : array[0..10] of TColor;
  colorValuesCopy : array[0..10] of byte;

  AnticModes : array[2..27] of TAnticMode;
  getDir : string;
  tabRefIndex : byte;
  atasciiPutChar : byte;
  isGrMicPalette : boolean;

  propAtariBASIC : TStringList;
  propFlagAtariBASIC : TStringList;

  propAction : TStringList;
  propFlagAction : TStringList;

  propMadPascal : TStringList;
  propFlagMadPascal : TStringList;

  propMadsMP : TStringList;
  propFlagMadsMP : TStringList;

  propMADS : TStringList;
  propFlagMADS : TStringList;

  propCC65 : TStringList;
  propFlagCC65 : TStringList;

  propFastBasic : TStringList;
  propFlagFastBasic : TStringList;

  propEffectus : TStringList;
  propFlagEffectus : TStringList;

  propKickC : TStringList;
  propFlagKickC : TStringList;

  isShowPmEditorGrid : boolean;
  isPmMixedColor : boolean;

  isMemoToEditor : boolean = true;

  // Editor font settings
  editorFont : TFont;

  // Editor color
  editorBackColor : TColor;
  editorLineHighlightColor : TSynSelectedColor;
  editorSelectedTextBackColor : TSynSelectedColor;

  // Animator editor settings
  isAnimEditorGrid : boolean;
  animFrames : byte;
  animFrameHeight : byte;

  // Module view
  propModules : TStringList;
  propFlagModules : array[0..12] of byte;

  // Selected computer language
  langIndex : byte = 0;

  pmResolution : TPlayerResolution;
//  playerSize : TPlayerSize;

  _PM_MAX_LINES : byte = 40;
//  _ANIM_MAX_LINES : byte = 60;

  exportData : array[0..500000] of byte;
  isDataExport : boolean;
  maxLines : word;
  beType : byte;

  setValues : TSetValuesType;

  isAntic4 : boolean;
  isAntic6 : boolean;

  aplAnim : TAplAnim;

  imgCharList : TList;

  antic_mode_max_x, antic_mode_max_y : byte;

implementation

end.

