PROGRAM GrafXY;
{Por Ant“nio Carlos Moreir„o de Queiroz}
{ I dummy.inc}
{$I typedef.sys}
{$I graphix.sys}
{$I kernel.sys}
{$I windows.sys}
{$I axis.hgh}
{$I polygon.hgh}
TYPE nome=STRING[128];
CONST maximo=10000000000.;
  mcr=5;
VAR tituloatual,titulovelho,telaatual,telavelha,arquivovelho:nome;
  arq:ARRAY [1..mcr] OF nome;
  a:ARRAY [1..mcr] OF PlotArray;
  pontos,m:ARRAY [1..mcr] OF INTEGER;
  x1,x2,y1,y2,x,y,z:REAL;
  arquivo:TEXT;
  ok:BOOLEAN;
  cx,cy,t,i,j,c,curvas,marca,largura:INTEGER;
  imprimir,marcar,opcao:CHAR;
  fim:^INTEGER;
  
PROCEDURE Comentarios;
  VAR x,y,ix,iy:INTEGER;
    c:CHAR;
  
  PROCEDURE Cursor;
  BEGIN
    IF PD(x,y+4) THEN SetColorBlack ELSE SetColorWhite;
    DP(x,y+4);
    SetColorWhite
  END;
  
BEGIN
  x:=42; y:=18;
  REPEAT
    Cursor;
    Read(Kbd,c);
    Cursor;
    CASE c OF
    #27:BEGIN
        Read(Kbd,c);
        CASE c OF
        'H':y:=y-6;
        'P':y:=y+6;
        'K':x:=x-6;
        'M':x:=x+6;
        END;
      END;
    #13:Exit;
    #8:BEGIN
        SetColorBlack;
        FOR ix:=x-6 TO x DO
          FOR iy:=y-1 TO y+4 DO DP(ix,iy);
        x:=x-6;
        SetColorWhite
      END
    ELSE
        DrawAscii(x,y,1,Ord(c));
    END;
  UNTIL FALSE
END; {Comentarios}
  
PROCEDURE GravarTelaVertical; {Grava a tela grafica na vertical}
  VAR i,j:INTEGER;
BEGIN
  WriteLn(arquivo,CHR(27),'A',CHR(8));
  FOR i:=0 TO largura DO
  BEGIN
    Write(arquivo,'           ',CHR(27),'L',CHR(144),CHR(1));
    j:=7920+i;
    REPEAT
      BEGIN
        Write(arquivo,CHR(Mem[$B800:j+8192]),CHR(Mem[$B800:j+8192]),CHR(Mem[$B800:j]),CHR(Mem[$B800:j]));
        j:=j-80
      END
    UNTIL j=i-80;
    WriteLn(arquivo);
  END;
  WriteLn(arquivo,CHR(27),'A',CHR(10))
END; {GravarTelaVertical}

PROCEDURE GravarTelaHorizontal; {Grava a tela grafica na posicao normal}
  VAR i,j,l,p:INTEGER;
    b7,b6,b5,b4,b3,b2,b1,b0,b,k:BYTE;
BEGIN
  WriteLn(arquivo,Chr(27),'A',CHr(8));
  l:=0;
  REPEAT
    BEGIN
      Write(arquivo,Chr(27),'K',Chr(Lo(largura SHL 3)),Chr(largura SHR 5));
      FOR i:=l TO l+largura-1 DO              {Chr(128),Chr(2) e o normal}
      BEGIN
        b7:=Mem[$b800:i];
        b6:=Mem[$b800:i+8192];
        b5:=Mem[$b800:i+80];
        b4:=Mem[$b800:i+8272];
        b3:=Mem[$b800:i+160];
        b2:=Mem[$b800:i+8352];
        b1:=Mem[$b800:i+240];
        b0:=Mem[$b800:i+8432];
        k:=128; j:=7;
        REPEAT
          b:=(b7 AND k) SHR j SHL 7+(b6 AND k) SHR j SHL 6+(b5 AND k) SHR j SHL 5+
          +(b4 AND k) SHR j SHL 4+(b3 AND k) SHR j SHL 3
          +(b2 AND k) SHR j SHL 2+(b1 AND k) SHR j SHL 1+(b0 AND k) SHR j;
          IF l=7920 THEN b:=b AND 224;
          Write(arquivo,Chr(b)); k:=k SHR 1; j:=j-1
        UNTIL k<1;
      END;
      WriteLn(arquivo);
      l:=l+320
    END
  UNTIL l>7920
END; {GravarTelaHorizontal}
  
BEGIN
ClrScr;
WriteLn('Programa para tra‡ar gr ficos');
WriteLn('-----------------------------');
WriteLn(' ACMQ - COPPE/UFRJ - 1985 - Vers„o 1.7 - 03/03/89');
WriteLn;
WriteLn('Crie at‚ ',mcr,' arquivos contendo:');
WriteLn('- Primeira linha: Coment rio.');
WriteLn('- Linhas seguintes: Colunas de n£meros separadas por espa‡os.');
WriteLn('  Quaisquer colunas podem ser escolhidas para os valores x e y.');
WriteLn('As escalas iniciais s„o definidas pela faixa coberta pelos pontos.');
WriteLn('Com o gr fico pronto:');
WriteLn('   "H" imprime ou grava o gr fico na posi‡„o horizontal.');
WriteLn('   "V" imprime ou grava o gr fico na posi‡„o vertical e ampliado (mais lento).');
WriteLn('   "R" permite a mudan‡a de escalas, etc.');
WriteLn('   "O" permite a entrada de novos arquivos de dados.');
WriteLn('   "C" permite a escrita de comentarios. Use as setas para movimentar o cursor');
WriteLn('       (um ponto), o teclado para escrever e BS para corrigir.');
WriteLn('       "Return" desativa, habilitando as outras fun‡”es.');
WriteLn('   "Esc Esc" termina o programa.');
WriteLn('Os arquivos ERROR.MSG e 4X6.FON devem estar presentes para a opera‡„o normal.');
WriteLn('A op‡„o de grava‡„o n„o imediata faz com que as telas sejam impressas em  ar-');
WriteLn('quivos em disco.  Estes arquivos podem ser impressos como arquivos comuns  de');
WriteLn('texto ( type nome>lpt1 ).');
WriteLn;
y1:=maximo; y2:=-y1; x1:=y1; x2:=-y1; t:=2; titulovelho:=''; marcar:='N';
imprimir:='N'; telavelha:='tela.grf'; largura:=79; cx:=1; cy:=2; arquivovelho:='';
FOR i:=1 to mcr DO m[i]:=7;
Mark(fim); {Necessario para evitar acumulo de variaveis na "heap"}
REPEAT
  REPEAT
    Write('Numero de curvas a plotar: ');
      {$I-} ReadLn(curvas); {$I+}
    ok:=(IOResult=0);
    IF NOT (ok AND (curvas IN [1..mcr])) THEN WriteLn('Entre 1 e mcr curvas sao permitidas.')
  UNTIL (curvas IN [1..mcr]) AND ok;
  FOR c:=1 TO curvas DO
  BEGIN
    REPEAT
      WriteLn;
      Write('Arquivo de entrada para a curva ',c,': (',arquivovelho,') '); ReadLn(arq[c]);
      IF arq[c]='' THEN arq[c]:=arquivovelho ELSE arquivovelho:=arq[c];
      Assign(arquivo,arq[c]);
         {$I-} Reset(arquivo); {$I+}
      ok:=(IOResult=0);
      IF NOT ok THEN WriteLn('Arquivo inexistente')
    UNTIL ok;
    REPEAT
      Write('N£mero das colunas onde est„o os valores x e y: (',cx,' ',cy,') ');
        {$I-} ReadLn(cx,cy); {$I+}
    UNTIL (cx>0) AND (cy>0) AND (IOResult=0);
    i:=0;
    ReadLn(arquivo);
    REPEAT
      i:=i+1; j:=0;
      REPEAT
        j:=j+1;
            {$I-} Read(arquivo,z); {$I+}
        IF IOResult<>0 THEN BEGIN
          WriteLn('Erro no arquivo: linha ',i,' coluna ',j);
          Halt
        END;
        IF j=cx THEN x:=z;
        IF j=cy THEN y:=z;
      UNTIL SeekEoln(arquivo);
      ReadLn(arquivo);
      IF (j>=cx) AND (j>=cy) THEN BEGIN
        a[c][i,1]:=x; a[c][i,2]:=y;
        IF y>y2 THEN y2:=y;
        IF y<y1 THEN y1:=y;
        IF x>x2 THEN x2:=x;
        IF x<x1 THEN x1:=x
      END
      ELSE i:=i-1
    UNTIL SeekEof(arquivo) OR (i=640);
    pontos[c]:=i;
    Close(arquivo);
    WriteLn('Pontos lidos no arquivo: ',pontos[c]);
  END;
  Delay(1000);
  REPEAT
    ClrScr;
    GoToXY(1,1);
    Write('Titulo a colocar: (',titulovelho,') '); ReadLn(tituloatual);
    IF tituloatual='' THEN tituloatual:=titulovelho ELSE titulovelho:=tituloatual;
    Write('Marcar os pontos? [s/n] (',marcar,') '); ReadLn(opcao);
    IF NOT (UpCase(opcao) IN ['S','N']) THEN opcao:=marcar;
    IF UpCase(opcao)='S' THEN
    BEGIN
      FOR c:=1 TO curvas DO BEGIN
        Write('Tipo da marca para a curva ',c,' [int.]: (',m[c],'): ');
        {$I-} ReadLn(m[c]); {$I+}
      END;
      Write('Tamanho das marcas [int.] (',t,'): ');
      {$I-} ReadLn(t); {$I+}
      marca:=1; marcar:='S'
    END
    ELSE BEGIN marca:=0; marcar:='n' END;
    REPEAT
      Write('Escala horizontal (',x1:9,' a ',x2:9,'): '); {$I-} ReadLn(x1,x2) {$I+}
    UNTIL IOResult=0;
    REPEAT
      Write('Escala vertical   (',y1:9,' a ',y2:9,'): '); {$I-} ReadLn(y1,y2) {$I+}
    UNTIL IOResult=0;
    Write('A impressao da tela deve ser imediata? [s/n] (',imprimir,') ');
    ReadLn(opcao);
    IF NOT (UpCase(opcao) IN ['S','N']) THEN opcao:=imprimir;
    imprimir:=UpCase(opcao);
    IF imprimir='N' THEN
    BEGIN
      Write('Arquivo onde gravar a tela: (',telavelha,') '); ReadLn(telaatual);
      IF telaatual='' THEN telaatual:=telavelha ELSE telavelha:=telaatual
    END;
    Write('Largura do grafico em bytes (',largura,'): '); ReadLn(largura);
    IF largura>80 THEN largura:=80;
    IF largura<25 THEN largura:=25;
    InitGraphic;
    ClearScreen;
    LineStyleGlb:=0;
    DefineWindow(1,0,0,largura-1,YMaxGlb);
    DefineHeader(1,tituloatual);
    DefineWorld(1,x1,y1,x2,y2);
    SelectWorld(1);
    SelectWindow(1);
    SetBackground(0);
    SetHeaderOn;
    ColorGlb:=255-ColorGlb;
    DrawBorder;
    ColorGlb:=255-ColorGlb;
    DrawAxis(8,-8,0,0,0,0,0,0,FALSE);
    FOR c:=1 TO curvas DO
    BEGIN
      DrawAxis(0,0,0,0,0,0,0,0,FALSE);
      LineStyleGlb:=c-1;
      DrawPolygon(a[c],1,-pontos[c],m[c]*marca,t*marca,0);
    END;
    REPEAT
      Read(Kbd,opcao);
      opcao:=UpCase(opcao);
      CASE opcao OF
      'C':Comentarios;
      'H':BEGIN
          IF imprimir='S' THEN Assign(arquivo,'Lst:')
          ELSE Assign(arquivo,telaatual);
          ReWrite(arquivo);
          GravarTelaHorizontal;
          Close(arquivo);
          Sound(1000); Delay(100); NoSound;
        END;
      'V':BEGIN
          IF imprimir='S' THEN Assign(arquivo,'Lst:')
          ELSE Assign(arquivo,telaatual);
          ReWrite(arquivo);
          GravarTelaVertical;
          Close(arquivo);
          Sound(1000); Delay(100); NoSound;
        END;
      #27:BEGIN
          Read(Kbd,opcao);
          IF opcao=#27 THEN BEGIN LeaveGraphic; HALT END
        END
      END
    UNTIL (opcao='R') OR (opcao='O');
    LeaveGraphic;
    Release(fim) {Limpa a "heap" de variaveis alocadas pelas procedures}
  UNTIL opcao='O'
UNTIL FALSE
END.
