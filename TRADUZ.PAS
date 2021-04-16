PROGRAM Traduz;
{******************************************************************************
* Programa auxiliar para traducao de textos em programas                      *
* Antonio Carlos M. de Queiroz                                                *
* Versao 1.0 de 20/06/91                                                      *
******************************************************************************}
USES Crt;

CONST
  nmmax=500;

TYPE
  texto=STRING[128];

VAR
  entrada,saida:TEXT;
  txt,linha:STRING;
  nome:ARRAY[1..nmmax] OF ^texto;
  ultimo,it,col,inicio,fim,i:INTEGER;
  terminar:BOOLEAN;
  r:CHAR;

FUNCTION Original(VAR teste:STRING; VAR i:INTEGER):BOOLEAN;
VAR
  ok:BOOLEAN;
BEGIN
  i:=0;
  REPEAT
    Inc(i);
    ok:=nome[i]^=teste
  UNTIL (i=it) or ok;
  Original:=not ok;
END;

FUNCTION AchouString(VAR achado:STRING):BOOLEAN;
BEGIN
  REPEAT
    Inc(col)
  UNTIL (linha[col]='''') or (col>=Length(linha));
  IF (linha[col]='''') and (col<=Length(linha)) THEN BEGIN
    inicio:=col;
    REPEAT
      Inc(col);
      WHILE Copy(linha,col,2)='''''' DO Inc(col,2)
    UNTIL (linha[col]='''') or (col>=Length(linha));
    IF (linha[col]='''') and (col<=Length(linha)) THEN BEGIN
      fim:=col;
      achado:=Copy(linha,inicio+1,fim-inicio-1);
      AchouString:=TRUE;
      col:=fim;
    END
    ELSE BEGIN
      WriteLn('Erro: String nao fechada');
      Halt;
    END
  END
  ELSE AchouString:=FALSE;
END;

BEGIN
  WriteLn('Programa TRADUZ: Auxiliar na tradu‡„o de mensagens');
  WriteLn;
  WriteLn('Este programa separa todas as strings de um programa em Pascal');
  WriteLn('e as coloca em um bloco de constantes no inicio do programa.');
  WriteLn('Pode tambem fazer a operacao inversa.');
  WriteLn;
  FOR i:=1 TO nmmax DO New(nome[i]);
  Write('Programa original: '); ReadLn(txt);
  Assign(entrada,txt);
  Reset(entrada);
  Write('Programa a criar: '); ReadLn(txt);
  Assign(saida,txt);
  ReWrite(saida);
  Write('[S]eparar todas as strings ou [R]ecolocar as strings no lugar? ');
  REPEAT r:=UpCase(ReadKey) UNTIL r in ['R','S']; WriteLn(r);
  IF r='S' THEN BEGIN
    it:=0; terminar:=FALSE;
    WriteLn('Identificando strings:');
    WHILE not terminar and not Eof(entrada) DO BEGIN
      ReadLn(entrada,linha);
      col:=0;
      WHILE not terminar and AchouString(txt) DO
        IF (it=0) or Original(txt,i) THEN BEGIN
          Inc(it);
          IF it>nmmax THEN BEGIN
            WriteLn('Numero maximo de strings (',nmmax,') excedido. As demais serao ignoradas');
            terminar:=TRUE
          END;
          IF not terminar THEN BEGIN
            nome[it]^:=txt;
            WriteLn('String ',it:2,': [',txt,']');
          END
      END
    END;
    WriteLn('Achadas ',it,' strings.');
    WriteLn('Escrevendo o bloco de constantes:');
    Reset(entrada);
    WriteLn(saida,'CONST {Strings do programa}');
    FOR i:=1 TO it DO BEGIN
      Str(i,txt);
      WHILE Length(txt)<3 DO txt:='0'+txt;
      WriteLn(saida,'  S_',txt,'=''',nome[i]^,''';')
    END;
    WriteLn(saida);
    WriteLn('Traduzindo o programa:');
    WHILE not Eof(entrada) DO BEGIN
      ReadLn(entrada,linha);
      ultimo:=1; col:=0;
      WHILE AchouString(txt) DO
        IF not Original(txt,i) THEN BEGIN
          Str(i,txt);
          WHILE Length(txt)<3 DO txt:='0'+txt;
          txt:='S_'+txt;
          IF (inicio>1) and (linha[inicio-1] in ['0'..'9']) THEN txt:=','+txt;
          IF (fim<Length(linha)) and (linha[fim+1]='#') THEN txt:=txt+',';
          Write(saida,Copy(linha,ultimo,inicio-ultimo),txt);
          ultimo:=fim+1;
        END;
      WriteLn(saida,Copy(linha,ultimo,255));
    END
  END
  ELSE BEGIN
    WriteLn('Lendo o bloco de constantes:');
    ReadLn(entrada,linha);
    IF Copy(linha,1,5)='CONST' THEN BEGIN
      it:=0; terminar:=FALSE;
      REPEAT
        ReadLn(entrada,linha);
        IF Copy(linha,3,2)='S_' THEN BEGIN
          Inc(it);
          Dec(linha[0],2);
          nome[it]^:=Copy(linha,10,255);
          WriteLn('String ',it:2,': [',nome[it]^,']');
        END
        ELSE terminar:=TRUE
      UNTIL terminar;
      WriteLn('Encontradas ',it,' strings');
      WriteLn('Recolocando as strings no lugar:');
      WHILE not Eof(entrada) DO BEGIN
        ReadLn(entrada,linha);
        terminar:=FALSE; col:=0; ultimo:=1;
        REPEAT
          REPEAT
            Inc(col);
            IF col>Length(linha)-4 THEN terminar:=TRUE;
          UNTIL (linha[col]='S') and (linha[col+1]='_') or terminar;
          IF not terminar THEN BEGIN
            Val(Copy(linha,col+2,3),i,fim);
            IF fim=0 THEN BEGIN
              Write(saida,Copy(linha,ultimo,col-ultimo),'''',nome[i]^,'''');
              Inc(col,4);
              ultimo:=col+1
            END
          END
          ELSE WriteLn(saida,Copy(linha,ultimo,255))
        UNTIL terminar
      END
    END
    ELSE BEGIN
      WriteLn('As constantes devem ser a primeira coisa no programa.');
      Halt
    END;
  END;
  Close(saida);
  Close(entrada);
  WriteLn('Feito.')
END.
