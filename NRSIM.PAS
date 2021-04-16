{
   NRSIM - simulador eletrico incremental.

   autor : Jones Schechtman - COPPE/UFRJ

   data  : 07/87

   versao : 1.1   (25/09/87) inclusao do modelo de uma chave.
   versao : 1.2   (18/12/87) inclusao do modelo de SCR.
                             saida para GRAFXY.
   versao : 1.3   (07/04/88) inclusao do ajuste automatico de passo.
   versao : 1.2b  (28/05/88) operacao em batch facilitada
                             passo fixo
                             mudanca na geracao de arquivos
                             graficos
   versao : 1.2c  (14/10/88) compatibilizacao com o EdFil
   versao : 1.2d  (24/02/89) turbo 5.0
   versao : 1.2e  (05/03/89) arquivo unico de saida
   versao : 1.2f  (04/04/92) para no fim no modo nao "batch". Chave "S"

   graficos por Antonio Carlos M. de Queiroz - COPPE/UFRJ
}

USES Graph3,Crt,Sprites;

CONST
  versao='1.2f de 04/04/92';
  maxlin=30;    { mumero maximo de linhas da matriz }
  maxcol=31;    { maxlin + 1 }
  hdc=1.0E6;             { "h" para gerar a matriz "DC" }
  deltamax=1.0E-8;       { determina o fim do NR }
  valmin=1.0E-8;         { valor minimo para o pivot do gauss }
  maxlista=100; { numero maximo de elemento na lista }

TYPE
  {$IFOPT N+}
  REAL=DOUBLE;
  {$ENDIF}
  matriz=ARRAY[0..maxlin,0..maxcol] OF REAL;
  vetor=ARRAY[0..maxlin] OF REAL;
  
  TipoDoElemento=
  (resistor,capacitor,indutor,diodo,fonteCte,fonteSin,FontePul,chave,SCR);
  elemento=
  RECORD
    no1,no2,ng:INTEGER;         { numeros dos nos }
    ni:INTEGER;                 { indice da variavel "I" (se houver) }
    CASE tipo:TipoDoElemento OF
    resistor:  ( R:REAL );
    capacitor: ( C:REAL );
    indutor:   ( L:REAL );
    fonteCte:  ( VF:REAL );
    fonteSin:  ( V,freq,fase:REAL );
    fontePul:  ( V0,V1,Delay,T,P:REAL );
    diodo:     ( R0,R1,R2,VD:REAL );
    chave:     ( VT,RON,ROFF:REAL );
    SCR:       ( VTS,RONS,ROFFS:REAL; fechado:BOOLEAN );
  END;
  tipo_de_erro=(A_singular,N_demais,el_demais,el_estranho,pt_demais);

VAR
  nome:STRING[20];                       { nome do arquivo com a descricao }
  arquivo:TEXT;                          { arquivo de entrada/saida }
  lista:ARRAY[1..maxlista] OF elemento;  { lista de elementos }
  ult:INTEGER;                           { aponta para o ultimo da lista }
  tempo:REAL;                            { tempo da simulacao }
  maxtempo:REAL;                         { tempo final da simulacao }
  passo:REAL;                            { passo de apresentacao }
  h:REAL;                                { passo interno da simulacao (igual a passo) }
  A:matriz;                              { matriz do sistema N x N+1 }
  N:INTEGER;                             { numero de linhas do sistema }
  NV:INTEGER;                            { numero de tensoes nodais }
  X:vetor;                               { vetor solucao no inst. "n" }
  XNR:vetor;                             { vetor de iteracao do NR }
  XNR1:vetor;                            { vetor de iteracao do NR }

{ Parametros relacionados com graficos }
  
CONST
  vmax:REAL=10;  { tensao/corrente maxima }
  vmin:REAL=-10; { tensao/corrente minima }
  cor=1;         { cor para graficos }
  coreixo=1;     { cor para eixos }
  corcsr=1;      { cor para cursor }
  xmin=138;      { limites da area dos graficos }
  xmax=639;
  ymin=10;
  ymax=199;
  maxsaidas=10;  { numero maximo de saidas }
  maxpontos=501; { numero maximo de pontos a calcular (xmax-xmin+1)}

TYPE
  curva=ARRAY[0..maxpontos] OF REAL;

VAR
  ay,by:REAL;     { y=ay*v+by }
  ax:REAL;        { x=xmin+ax*pontos }
  cursor:INTEGER; { ponto do cursor }
  linha:INTEGER;  { curva apontada pelo cursor }
  dx:INTEGER;     { passo do cursor }
  Nsaidas:INTEGER;                       { numero de saidas }
  saida:ARRAY[1..maxsaidas] OF INTEGER;  { lista das saidas }
  grafico:ARRAY[1..maxsaidas] OF curva;  { curvas calculadas }
  analises:INTEGER;                      { parametros na linha de comando }
  pontos:INTEGER;                        { pontos atuais no grafico }
  salvar:BOOLEAN;                        { salvar ou nao graficos }
  recalcular:BOOLEAN;                    { recalcular apos mudanca de tempos }
  interromper:BOOLEAN;                   { interromper antes do fim }
  consultar:BOOLEAN;                     { consultar o usuario antes de terminar }
  acabou:BOOLEAN;                        { fim do grafico }

LABEL
  Inicio;

PROCEDURE Termina;
BEGIN
  textmode(80);
END;

PROCEDURE ModoGrafico;
BEGIN
  hires;
  colortable(3,2,1,0);
END;
  
PROCEDURE Posiciona;
  VAR
    i:INTEGER;
BEGIN
  GoToXY(1,1);
  FOR i:=1 TO 79 DO Write(' ');
  GoToXY(1,1);
END;

PROCEDURE Erro(x: tipo_de_erro);
BEGIN
  Posiciona;
  CASE x OF
    A_singular:  Write('* Sistema singular');
    N_demais:    Write('* Excessivo numero de tensoes/correntes');
    el_demais:   Write('* Excessivo numero de elementos');
    el_estranho: Write('* Elemento desconhecido na descricao');
  END;
  Halt;
END;
  
PROCEDURE Gauss(VAR A:matriz; VAR X:vetor);
  
{ "A" e' a matriz ampliada do sistema ( N x N+1 ) }
{  A solucao retorna em X }
{  Assume-se que existe solucao e e' unica !!! }
  
  VAR
    pivot:INTEGER;
    lin,col:INTEGER;
    aux:REAL;

BEGIN
  FOR pivot:=1 TO N DO BEGIN
    lin:=pivot;
    WHILE (abs(A[lin,pivot])<valmin) DO
      IF lin<N THEN lin:=lin+1
      ELSE Erro(A_singular);
    IF lin<>pivot THEN
      FOR col:=pivot TO N+1 DO BEGIN
        aux:=A[pivot,col];
        A[pivot,col]:=A[lin,col];
        A[lin,col]:=aux;
      END;
    FOR col:=N+1 DOWNTO pivot DO A[pivot,col]:=A[pivot,col]/A[pivot,pivot];
    FOR lin:=pivot+1 TO N DO
      FOR col:=N+1 DOWNTO pivot DO
        A[lin,col]:=A[lin,col]-A[lin,pivot]*A[pivot,col];
  END;
  FOR lin:=N DOWNTO 1 DO BEGIN
    aux:=0;
    FOR col:=lin+1 TO N DO aux:=aux+A[lin,col]*X[col];
    X[lin]:=A[lin,N+1]-aux;
  END;
END;
  
FUNCTION Delta( VAR A,B:vetor ):REAL;
  
{ calcula uma medida da diferenca entre os dois vetores "A" e "B" }
{ o valor retornado e' a maior diferenca absoluta }
{ o vetor "B" e' movido para "A" }
  
  VAR
    i:INTEGER;
    max,d:REAL;
BEGIN
  max:=0;
  FOR i:=1 TO N DO BEGIN
    d:=abs(B[i]-A[i]);
    IF d>max THEN max:=d;
  END;
  delta:=max;
  A:=B;
END;
  
PROCEDURE GeraFonte(VAR e:elemento);
  VAR
    VV:REAL;
BEGIN
  WITH e DO BEGIN
    A[no1,ni]:=A[no1,ni]+1;
    A[no2,ni]:=A[no2,ni]-1;
    A[ni,no1]:=A[ni,no1]+1;
    A[ni,no2]:=A[ni,no2]-1;
    CASE tipo OF
    fonteSin: VV:=V*sin(2*pi*freq*tempo+fase);
    fonteCte: VV:=VF;
    fontePul: BEGIN
        IF tempo<Delay THEN VV:=V0
        ELSE
          IF frac((tempo-Delay)/P)*P<T THEN VV:=V1 ELSE VV:=V0;
      END;
    END;
    A[ni,N+1]:=A[ni,N+1]+VV;
  END;
END;
  
PROCEDURE GeraResistor(VAR e:elemento);
  VAR
    G:REAL;
BEGIN
  WITH e DO BEGIN
    G:=1/R;
    A[no1,no1]:=A[no1,no1]+G;
    A[no1,no2]:=A[no1,no2]-G;
    A[no2,no1]:=A[no2,no1]-G;
    A[no2,no2]:=A[no2,no2]+G;
  END;
END;

PROCEDURE GeraCapacitor(VAR e:elemento);
  VAR
    G:REAL;
BEGIN
  WITH e DO BEGIN
    G:=C/h;
    A[no1,no1]:=A[no1,no1]+G;
    A[no1,no2]:=A[no1,no2]-G;
    A[no2,no1]:=A[no2,no1]-G;
    A[no2,no2]:=A[no2,no2]+G;
    A[no1,N+1]:=A[no1,N+1]+G*(X[no1]-X[no2]);
    A[no2,N+1]:=A[no2,N+1]-G*(X[no1]-X[no2]);
  END;
END;
  
PROCEDURE GeraIndutor(VAR e:elemento);
  VAR
    Z:REAL;
BEGIN
  WITH e DO BEGIN
    Z:=L/h;
    A[no1,ni]:=A[no1,ni]+1;
    A[no2,ni]:=A[no2,ni]-1;
    A[ni,no1]:=A[ni,no1]+1;
    A[ni,no2]:=A[ni,no2]-1;
    A[ni,ni]:=A[ni,ni]-Z;
    A[ni,N+1]:=A[ni,N+1]-Z*X[ni];
  END;
END;
  
PROCEDURE GeraDiodo(VAR e:elemento);
  VAR
    G:REAL;
    vv:REAL;
    ID:REAL;
BEGIN
  WITH e DO BEGIN
    vv:=XNR[no1]-XNR[no2];
    IF vv<0 THEN G:=1/R0 ELSE
      IF vv>VD THEN G:=1/R2 ELSE G:=1/R1;
    ID:=VD/R1;
    A[no1,no1]:=A[no1,no1]+G;
    A[no1,no2]:=A[no1,no2]-G;
    A[no2,no1]:=A[no2,no1]-G;
    A[no2,no2]:=A[no2,no2]+G;
    IF vv>VD THEN BEGIN
      A[no1,N+1]:=A[no1,N+1]+VD*G-ID;
      A[no2,N+1]:=A[no2,N+1]-VD*G+ID;
    END;
  END;
END;
  
PROCEDURE GeraChave(VAR e:elemento);
  VAR
    G:REAL;
BEGIN
  WITH e DO BEGIN
    IF XNR[ng]>VT THEN G:=1/RON ELSE G:=1/ROFF;
    A[no1,no1]:=A[no1,no1]+G;
    A[no1,no2]:=A[no1,no2]-G;
    A[no2,no1]:=A[no2,no1]-G;
    A[no2,no2]:=A[no2,no2]+G;
  END;
END;
  
PROCEDURE GeraSCR(VAR e:elemento);
  VAR
    G:REAL;
BEGIN
  WITH e DO BEGIN
    IF (NOT fechado) AND (((XNR[ng]-XNR[no2])>VTS) AND (XNR[no1]>XNR[no2]))
      THEN fechado:=TRUE;
    IF fechado AND (XNR[no1]<XNR[no2]) THEN fechado:=FALSE;
    IF fechado THEN G:=1/RONS ELSE G:=1/ROFFS;
    A[no1,no1]:=A[no1,no1]+G;
    A[no1,no2]:=A[no1,no2]-G;
    A[no2,no1]:=A[no2,no1]-G;
    A[no2,no2]:=A[no2,no2]+G;
  END;
END;
  
PROCEDURE GeraMatriz;
  VAR
    i,j:INTEGER;
BEGIN
  FOR i:=0 TO N DO
    FOR j:=0 TO N+1 DO A[i,j]:=0; { limpa matriz }
  FOR i:=1 TO ult DO BEGIN
    CASE lista[i].tipo OF
      fonteCte,
      fonteSin,
    fontePul: GeraFonte(lista[i]);
    resistor: GeraResistor(lista[i]);
    capacitor: GeraCapacitor(lista[i]);
    indutor: GeraIndutor(lista[i]);
    diodo: GeraDiodo(lista[i]);
    chave: GeraChave(lista[i]);
    SCR: GeraSCR(lista[i]);
    END;
  END;
END;
  
PROCEDURE LeDescricao;
  VAR
    c,cc:CHAR;
    i,j:INTEGER;
    ok:BOOLEAN;
BEGIN
  ok:=FALSE;
  IF paramcount>0 THEN BEGIN
    analises:=analises+1;
    nome:=paramstr(analises);
    ok:=TRUE;
    consultar:=FALSE;
  END
  ELSE consultar:=TRUE;
  REPEAT
    IF NOT ok THEN BEGIN
      Write(#10'Nome do arquivo de descricao (.net): ');
      ReadLn(nome);
    END;
    assign(arquivo,nome+'.net');
    {$I-} reset(arquivo); {$I+}
    ok:=ioresult=0;
    IF NOT ok THEN WriteLn(#10'Arquivo ',nome,'.net inexistente');
  UNTIL ok;
  WriteLn(#10'Lendo arquivo ',nome,'.net'#10);
  ReadLn(arquivo,N);
  NV:=N;
  ult:=0;
  Nsaidas:=N;
  FOR i:=1 TO N DO saida[i]:=i;
  passo:=0.01;
  maxtempo:=passo*(maxpontos-1)/2;
  REPEAT
    Read(arquivo,c);
    REPEAT Read(arquivo,cc) UNTIL cc=' ';
    IF (c<>'*') AND (c<>'#') THEN ult:=ult+1;
    IF ult>maxlista THEN Erro(el_demais);
    CASE UpCase(c) OF
    '#': BEGIN
        Read(arquivo,passo,maxtempo,Nsaidas);
        FOR i:=1 TO Nsaidas DO Read(arquivo,saida[i]);
        ReadLn(arquivo);
      END;
    '*': ReadLn(arquivo);
    'R': WITH lista[ult] DO BEGIN
        ReadLn(arquivo,no1,no2,R);
        tipo:=resistor;
        WriteLn('Resistor entre ',no1,' e ',no2,': ',R:10,' Ohms');
      END;
    'C': WITH lista[ult] DO BEGIN
        ReadLn(arquivo,no1,no2,C);
        tipo:=capacitor;
        WriteLn('Capacitor entre ',no1,' e ',no2,': ',C:10,' Farads');
      END;
    'L': WITH lista[ult] DO BEGIN
        ReadLn(arquivo,no1,no2,L);
        N:=N+1;
        ni:=N;
        tipo:=indutor;
        WriteLn('Indutor entre ',no1,' e ',no2,': ',L:10,' Henrys');
      END;
    'D': WITH lista[ult] DO BEGIN
        ReadLn(arquivo,no1,no2,R0,R1,R2,VD);
        tipo:=diodo;
        WriteLn('Diodo entre ',no1,' e ',no2,': ',R0:10,',',R1:10,',',R2:10,',',VD:10);
      END;
    'V': WITH lista[ult] DO BEGIN
        Read(arquivo,no1,no2);
        REPEAT Read(arquivo,cc) UNTIL cc<>' ';
        CASE UpCase(cc) OF
        'P': BEGIN
            ReadLn(arquivo,V0,V1,Delay,T,P);
            N:=N+1;
            ni:=N;
            tipo:=fontePul;
            WriteLn('Fonte de pulso entre ',no1,' e ',no2,': ',
            VF:10,',',V1:10,',',Delay:10,',',T:10,',',P:10);
          END;
        'E': BEGIN
            ReadLn(arquivo,VF);
            N:=N+1;
            ni:=N;
            tipo:=fonteCte;
            WriteLn('Fonte CC entre ',no1,' e ',no2,': ',VF:10,' Volts');
          END;
        'S': BEGIN
            ReadLn(arquivo,V,freq,fase);
            N:=N+1;
            ni:=N;
            tipo:=fonteSin;
            WriteLn('Fonte senoidal entre ',no1,' e ',no2,': ',V:10,',',freq:10,',',fase:10);
          END;
        END;
      END;
    'S': WITH lista[ult] DO BEGIN
        ReadLn(arquivo,no1,no2,ng,RON,ROFF,VT);
        tipo:=chave;
        WriteLn('Chave entre ',no1,', ',no2,' e ',ng,': ',RON:10,',',ROFF:10,',',VT:10)
      END;
    'T': WITH lista[ult] DO BEGIN
        ReadLn(arquivo,no1,no2,ng,RONS,ROFFS,VTS);
        tipo:=SCR;
        fechado:=FALSE;
        WriteLn('Tiristor entre ',no1,', ',no2,' e ',ng,': ',RONS:10,',',ROFFS:10,',',VTS:10);
      END;
    ELSE Erro(el_estranho);
    END;
    IF N>maxlin THEN Erro(N_demais);
  UNTIL seekeof(arquivo);
  close(arquivo);
  delay(2000);
  IF keypressed THEN BEGIN
    c:=ReadKey;
    WriteLn('Toque uma tecla para prosseguir...');
    c:=ReadKey;
  END;
END;


FUNCTION PtY(V:REAL):INTEGER;
  VAR t:REAL;
BEGIN
  t:=ay*V+by;
  IF t>ymax THEN PtY:=ymax
  ELSE
    IF t<ymin THEN PtY:=ymin
    ELSE PtY:=round(t);
END;

PROCEDURE Inicializa;
BEGIN
  ModoGrafico;
  spritemode;
  sprite(1,#$FF#$FE#$FC#$FE#$FF#$FE#$DC#$88);
  square(xmin,ymin,xmax,ymax,cor);
  ax:=(xmax-xmin)/maxtempo*passo;
  IF ax<1.0 THEN BEGIN
    ax:=1.0;
    maxtempo:=(maxpontos-1)*passo;
    Posiciona;
    Write('Numero excessivo de pontos; tempo maximo reduzido para ',maxtempo:10:5);
    Delay(2000);
  END;
  ay:=(ymin-ymax)/(vmax-vmin);
  by:=ymin-ay*vmax;
  draw(xmin,PtY(0),xmax,PtY(0),coreixo);
  cursor:=0;
  linha:=1;
  salvar:=TRUE;
  interromper:=FALSE;
  recalcular:=FALSE;
  dx:=1;
  GoToXY(1,3);
  WriteLn('Tmax: ',maxtempo:10:5);
  WriteLn('Passo:',passo:10:5);
  WriteLn('Vmax: ',vmax:10:5);
  WriteLn('Vmin: ',vmin:10:5);
  WriteLn;
  WriteLn('cursor:      ');
  WriteLn('hor:move seta');
  WriteLn('pg:muda passo');
  WriteLn('vert:desloca ');
  WriteLn('tab:muda crv.'#10);
  WriteLn('+:amplia x2  ');
  WriteLn('-:reduz  /2  '#10);
  WriteLn('T:muda tempos');
  WriteLn('C:muda curvas');
  WriteLn('ret:continua ');
  WriteLn('esc:interrompe');
END;

PROCEDURE Escreve;
  VAR
    i:INTEGER;
BEGIN
  FOR i:=1 TO Nsaidas DO
    grafico[i,pontos]:=X[saida[i]];
END;

PROCEDURE Plota;
  VAR
    i:INTEGER;

  PROCEDURE PlotaUm(pt:INTEGER);
    VAR
      ptx1,ptx2,i:INTEGER;
  BEGIN
    ptx1:=xmin+Round(ax*(pt-1));
    ptx2:=xmin+Round(ax*pt);
    FOR i:=1 TO Nsaidas DO
      draw(ptx1,PtY(grafico[i,pt-1]),ptx2,PtY(grafico[i,pt]),cor)
  END;

  PROCEDURE PlotaTudo;
    VAR i:INTEGER;
  BEGIN
    Inicializa;
    FOR i:=1 TO pontos DO BEGIN
      PlotaUm(i);
      IF keypressed THEN exit;
    END;
  END;

  PROCEDURE Desloca(aumenta:BOOLEAN);
    VAR
      delta:REAL;
  BEGIN
    delta:=(vmax-vmin)/4;
    IF aumenta THEN delta:=-delta;
    vmax:=vmax-delta;
    vmin:=vmin-delta;
    PlotaTudo;
  END;

  PROCEDURE ReEscala(aumenta:BOOLEAN);
    VAR
      fator:REAL;
  BEGIN
    IF aumenta THEN fator:=2.0 ELSE fator:=0.5;
    vmax:=vmax*fator;
    vmin:=vmin*fator;
    PlotaTudo;
  END;

BEGIN
  PlotaUm(pontos);
  acabou:=(pontos=maxpontos) or (tempo>=maxtempo);
  IF keypressed or (consultar and acabou) THEN
  REPEAT
    REPEAT
      CASE UpCase(Readkey) OF
      #13: BEGIN
          PutSprite(1,-10,-10);
          Posiciona;
          Exit;
        END;
      #27: BEGIN
          putsprite(1,-10,-10);
          Posiciona;
          Write('Terminar analise no ponto atual? (s/n)');
          IF UpCase(Readkey)='S' THEN interromper:=TRUE;
          Posiciona;
          Write('Salvar curvas ao fim da analise? (s/n)');
          IF UpCase(Readkey)='N' THEN salvar:=FALSE;
          Posiciona;
          Exit;
        END;
      #9: IF linha<Nsaidas THEN linha:=linha+1 ELSE linha:=1;
      #0: CASE Readkey OF
        'K': IF cursor>=dx THEN cursor:=cursor-dx;
        'M': IF cursor+dx<=pontos THEN cursor:=cursor+dx;
        'H': Desloca(TRUE);
        'P': Desloca(FALSE);
        'I': dx:=10;
        'Q': dx:=1;
        END;
      '-': ReEscala(TRUE);
      '+': ReEscala(FALSE);
      'T': BEGIN
             Posiciona;
             Write('Mudar tempos? (s/n)');
             IF UpCase(ReadKey)='S' THEN  BEGIN
               recalcular:=TRUE;
               Posiciona;
               Write('Novo passo e tempo maximo: ');
               Read(passo, maxtempo);
             END;
             Posiciona;
             Exit;
           END;
      'C': BEGIN
             Posiciona;
             Write('Mudar saidas a plotar (todas serao recalculadas)? (s/n)');
             IF UpCase(ReadKey)='S' THEN BEGIN
               REPEAT
                 Posiciona;
                 Write('Numero de saidas e seus numeros: ');
                 Read(Nsaidas);
                 FOR i:=1 TO Nsaidas DO Read(saida[i]);
               UNTIL Nsaidas<=N;
               recalcular:=TRUE;
             END;
             Posiciona;
             Exit;
           END;
      END;
    UNTIL NOT keypressed;
    PutSprite(1,xmin+Round(ax*cursor),PtY(grafico[linha,cursor]));
    GoToXY(1,1);
    Write('Saida no. ',linha:2,'; Tempo: ',cursor*h:10:5,' s; Valor: ',grafico[linha,cursor]:10:5);
    IF saida[linha]>NV THEN Write(' A   ') ELSE Write(' V   ');
  UNTIL FALSE;
END;

PROCEDURE SalvaCurvas;
  VAR
    i,j:INTEGER;
    s:STRING[2];
BEGIN
  Assign(arquivo,nome+'.grf');
  Posiciona;
  Write('Escrevendo arquivo ',nome,'.grf...');
  ReWrite(arquivo);
  Write(arquivo,nome:10);
  FOR i:=1 TO Nsaidas DO Write(arquivo,Saida[i]:11);
  WriteLn(arquivo);
  FOR j:=0 TO pontos DO BEGIN
    Write(arquivo,j*h:10:5);
    FOR i:=1 TO Nsaidas DO Write(arquivo,' ',grafico[i,j]:10:5);
    WriteLn(arquivo)
  END;
  Close(arquivo)
END;

PROCEDURE LimpaVetor(VAR X:vetor);
  VAR
    i:INTEGER;
BEGIN
  FOR i:=0 TO N DO X[i]:=0;
END;

PROCEDURE Newton_Raphson;

{ XNR e o vetor solucao }

BEGIN
  REPEAT
    GeraMatriz;
    Gauss(A,XNR1);
  UNTIL delta(XNR,XNR1)<deltamax;
END;

BEGIN
analises:=0;
REPEAT
  ModoGrafico;
  WriteLn('NRSIM - versao ',versao);
  WriteLn('Autor:     Jones Schechtman');
  WriteLn('Graficos:  Antonio Carlos M. de Queiroz');
  WriteLn('COPPE/UFRJ - 1988');
  LeDescricao;
 Inicio:
  tempo:=0;
  pontos:=0;
  Inicializa;
  Posiciona;
  Write('Analise DC ...');
  h:=hdc;
  LimpaVetor(X);
  LimpaVetor(XNR);
  LimpaVetor(XNR1);
  Newton_Raphson;
  X:=XNR;
  Escreve;
  Posiciona;
  Write('Analise de Transiente ...');
  h:=passo;
  REPEAT
    tempo:=tempo+h;
    pontos:=pontos+1;
    Newton_Raphson;
    X:=XNR;
    Escreve;
    Plota;
  UNTIL acabou OR interromper OR recalcular;
  IF recalcular THEN GoTo Inicio;
  IF salvar THEN SalvaCurvas;
UNTIL analises=paramcount;
Termina;
WriteLn('Fim da Simulacao');
END.
