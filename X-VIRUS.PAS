USES Dos,Crt;

CONST maxarq=1000;
      dirref='X-VIRUS.DIR';
      programa='X-VIRUS.EXE';
TYPE
  filename=STRING[12];
  diretorio=ARRAY[1..maxarq] of RECORD
    nome:filename;
    tamanho,tempo:LONGINT;
  END;
VAR
  diratual,dirantigo:diretorio;
  referencia:FILE of diretorio;
  f:FILE of BYTE;
  path:PathStr;
  r:CHAR;
  eventos,i,j,nantigo,natual:INTEGER;
  s:STRING[1];

PROCEDURE MaisUm;
BEGIN
  Inc(eventos);
  IF eventos and 15=0 THEN BEGIN WriteLn('Mais...'); r:=ReadKey END;
END;

PROCEDURE TestaContaminacao;
  CONST
    tamanho:STRING[5]='7387';
  VAR
    tlido,tcorreto:LONGINT;
    cod:INTEGER;
  BEGIN
    Assign(f,programa);
    {$i-} Reset(f) {$i+};
    IF IOResult<>0 THEN BEGIN
      WriteLn('* Teste de auto-contamina‡„o n„o realizado (',programa,' n„o encontrado)');
      MaisUm;
      Exit
    END;
    tlido:=FileSize(f);
    Val(tamanho,tcorreto,cod);
    Close(f);
    IF tcorreto<>tlido THEN BEGIN
      WriteLn('* Esta c¢pia do programa apresenta sinais de poss¡vel contamina‡„o!');
      MaisUm;;
      Write('  Toque uma tecla para prosseguir assim mesmo ou ESC para terminar'#13);
      r:=ReadKey;
      IF r=#27 THEN Halt;
      ClrEol;
    END;
  END;

PROCEDURE LePath;
BEGIN
  IF ParamCount<>0 THEN path:=ParamStr(1) ELSE path:='';
END;

PROCEDURE LeReferencia;
BEGIN
  FileMode:=0; {Necess rio para poder dar Reset em arquivos ReadOnly}
  Assign(referencia,path+dirref);
  {$I-} Reset(referencia); {$i+}
  IF IOResult<>0 THEN BEGIN
    dirantigo[1].nome:='';
    WriteLn('* Diret¢rio de referˆncia n„o encontrado (',path,dirref,')');
    MaisUm;
  END
  ELSE BEGIN Read(referencia,dirantigo); Close(referencia) END;
  i:=1;
  WHILE (dirantigo[i].nome<>'') and (i<=maxarq) DO i:=i+1;
  nantigo:=i-1;
END;

PROCEDURE LeDiretorio;
VAR
  srec:SearchRec;
BEGIN
  i:=1;
  FindFirst(path+'*.*',Archive+ReadOnly+Hidden+SysFile,srec);
  WHILE DosError=0 DO WITH diratual[i] DO BEGIN
    nome:=srec.Name;
    tamanho:=srec.Size;
    tempo:=srec.Time;
    FindNext(srec);
    i:=i+1;
  END;
  natual:=i-1;
  IF natual<>maxarq THEN diratual[natual+1].nome:='';
END;

PROCEDURE Compara;
VAR
  achado:BOOLEAN;
  dataantiga,dataatual:DateTime;
  jaexistia:ARRAY[1..maxarq] OF BOOLEAN;

BEGIN
  FOR i:=1 TO natual DO jaexistia[i]:=FALSE;
  FOR i:=1 TO nantigo DO
  WITH dirantigo[i] DO BEGIN
    achado:=FALSE;
    FOR j:=1 TO natual DO
      IF nome=diratual[j].nome THEN BEGIN
        IF achado THEN BEGIN
          WriteLn('*',nome:13,': Duplicado');
          MaisUm;;
        END;
        achado:=TRUE;
        jaexistia[j]:=TRUE;
        IF tempo<>diratual[j].tempo THEN BEGIN
          UnpackTime(tempo,dataantiga);
          UnpackTime(diratual[j].tempo,dataatual);
          Write('*',nome:13,': Data mudou de ');
          WITH dataantiga DO Write(Hour,':',Min,':',Sec,' ',Day,'/',Month,'/',Year,' para ');
          WITH dataatual DO  WriteLn(Hour,':',Min,':',Sec,' ',Day,'/',Month,'/',Year);
          MaisUm;;
        END;
        IF tamanho<>diratual[j].tamanho THEN BEGIN
          WriteLn('*',nome:13,': Tamanho mudou de ',tamanho,' para ',diratual[j].tamanho,' bytes');
          MaisUm;;
        END;
      END;
    IF not achado THEN BEGIN
      WriteLn('*',nome:13,': Desapareceu');
      MaisUm;;
    END;
  END;
  FOR i:=1 TO natual DO IF not jaexistia[i] THEN BEGIN
    WriteLn('*',diratual[i].nome:13,': Apareceu');
    MaisUm;
  END;

END;

PROCEDURE SalvaReferencia;
BEGIN
  IF eventos>0 THEN BEGIN
    WriteLn(#10'Atualizar diret¢rio de referˆncia? (s/n) ');
    REPEAT r:=UpCase(ReadKey) UNTIL r in ['S','N'];
    IF r='S'THEN BEGIN
      Assign(referencia,path+dirref);
      SetFAttr(referencia,Archive);
      ReWrite(referencia);
      Write(referencia,diratual);
      SetFTime(referencia,0);
      Close(referencia);
      SetFAttr(referencia,Archive+ReadOnly+Hidden);
      WriteLn(#10'Diret¢rio de referˆncia (oculto) atualizado');
    END;
  END;
END;

BEGIN
  ClrScr;
  WriteLn('ÚÄÄÄÄÄÄÄÄÄ¿');
  WriteLn('³ X-VIRUS ³');
  WriteLn('ÀÄÄÄÄÄÄÄÄÄÙ');
  WriteLn('ACMQ - COPPE/UFRJ - Vers„o 1.1');
  WriteLn;
  WriteLn('Verifica‡„o de sinais de poss¡vel presen‡a de virus:');
  WriteLn;
  eventos:=0;
  TestaContaminacao;
  LePath;
  LeReferencia;
  LeDiretorio;
  Compara;
  IF eventos=1 THEN s:='' ELSE s:='s';
  IF eventos<>0 THEN WriteLn(#10,eventos,' evento',s,' suspeito',s,' encontrado',s)
  ELSE WriteLn('Nada de anormal encontrado');
  WriteLn(#10'Verificados todos os arquivos ',path,'*.*');
  SalvaReferencia;
END.