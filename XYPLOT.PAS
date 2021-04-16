PROGRAM XYPlot;
{
********************************************************************************
 Por Antonio Carlos Moreirao de Queiroz
 V. 1.0 de 24/01/92 - Multiplos paineis
 V. 1.1 de 01/02/92 - Melhor leitura de tabelas
 V. 1.2 de 29/02/92 - Melhor controle das curvas a plotar
 V. 1.3 de 02/08/92 - Graficos com fundo preto, diretorio e melhor cursor
 V. 1.4 de 10/10/92 - Confirmacao da saida e lembranca do estado
 V. 1.4a de 06/11/92 - Notacao cientifica
 V. 1.4b de 25/11/92 - Arquivo nulo e recusado
 V. 1.4c de 16/01/93 - Le arquivos do SwitCap, melhor escalamento
 V. 1.4d de 20/05/93 - Escrita de bitmap
 V. 1.5  de 05/01/94 - Titulos nas escalas e fontes variaveis
 V. 1.5a de 26/01/94 - Melhores numeros nas escalas
 V. 1.6  de 16/12/94 - Eliminada janela de cursor, melhor cursor
 V. 1.7  de 02/03/96 - Mais pontos nas curvas
 V. 1.7a de 18/05/03 - Muda curva com tab, mais espa‡o para legenda
 V. 1.7b de 13/06/07 - Salva e termina
********************************************************************************
Ideias a implementar:
  Alocar as tabelas na heap, nao os graficos
  Grade em varios formatos
  Lembranca dos comentarios
}

{$IFDEF DOUBLE}
  {$N+,E+}
{$ENDIF}

USES Graph,XView,Dos,Crt,Mickey;

TYPE
  cores=ARRAY[1..6] OF BYTE;
CONST
  versao='1.7b de 13/6/2007';
  ptmax={$IFDEF DPMI} 3000 {$ELSE} 700 {$ENDIF};
  grmax=5;
  selecao:BOOLEAN=FALSE;
  cor:cores=(white,yellow,lightgreen,lightblue,lightred,lightgray);
  corCGA:cores=(3,1,2,3,1,2);
  corpb:cores=(1,1,1,1,1,1);
TYPE
  curva=RECORD
    nome:xv_label_type;
    pontos,col1,col2,estilo:INTEGER;
    vmin,vmax,ah,bh,av,bv,hminimo,vminimo,hmaximo,vmaximo:REAL;
    ptx,pty:ARRAY[1..ptmax] of REAL;
  END;
  xpainel=RECORD
    xlog,ylog,grade:BOOLEAN;
    hmin,hmax:REAL;
    xmin,xmax,ymin,ymax:INTEGER;
    catual,cursorx,cursory:INTEGER;
    mesmoy:BOOLEAN;
    grafico:ARRAY[1..grmax] of ^curva;
    janela,painel:Xv_opaque;
    tamanho,fonte:INTEGER;
    escv,esch:STRING[10];
  END;
  ptr_painel=^xpainel;

VAR
  i,k,xi,xf,yi,yf,placa,modo:INTEGER;
  ok:BOOLEAN;
  painel1:ptr_painel;
  jmenu:Xv_opaque;
  jarquivo,tnome,tcol1,tcol2,bcatual,bdir,tstyle,bincluir,bapagar:Xv_opaque;
  jescalas,thmin,thmax,tvmin,tvmax,bok:Xv_opaque;
  jtitulo,tescv,tesch,ttamanho,tfonte,ttitulo,btitulo:Xv_opaque;
  jmsg,msg:Xv_opaque;
  jdiretorio,cdiretorio,tmask:Xv_opaque;
  jconfirmar,bsalvar,bterminar:Xv_opaque;
  jbitmap,tbitmap,sbitmap,bbitmap:Xv_opaque;
  tela_atual:Xv_opaque;
  arquivo:TEXT;
  c:CHAR;
  saida:FILE of WORD;
  b1,b2,b3,b4:BYTE;
  w1:WORD absolute b1;
  w2:WORD absolute b3;
  dw:LONGINT absolute b1;

LABEL
  Continuar;

FUNCTION RLim(x:REAL):INTEGER;
BEGIN
  IF x>2000 THEN x:=2000
  ELSE IF x<-2000 THEN x:=-2000;
  Rlim:=Round(x)
END;

FUNCTION LogLin(x:REAL;log:BOOLEAN):REAL;
BEGIN
  IF log THEN BEGIN
    x:=Abs(x);
    IF x=0 THEN x:=1e-20;
    LogLin:=Ln(x)
  END
  ELSE LogLin:=x
END;

FUNCTION Si(i:LONGINT):STRING;
VAR
  txt:STRING;
BEGIN
  Str(i,txt);
  Si:=txt
END;

FUNCTION Cpct1(x:REAL):STRING;
VAR
  txt:STRING;
  e,z,d:INTEGER;
BEGIN
  IF (Abs(x)>100) or (Abs(x)<0.01) THEN BEGIN
    Str(x,txt);
    IF txt[1]=' ' THEN txt:=Copy(txt,2,255);
    e:=Pos('E',txt);
    z:=e+2;
    WHILE (z<=Length(txt)) and (txt[z]='0') DO Inc(z);
    d:=6;
    IF z>Length(txt) THEN e:=z;
    WHILE txt[d]='0' DO Dec(d);
    IF txt[d]='.' THEN Dec(d);
    Cpct1:=Copy(txt,1,d)+Copy(txt,e,2)+Copy(txt,z,4);
  END
  ELSE BEGIN
    Str(x:7:3,txt);
    WHILE txt[1]=' ' DO txt:=Copy(txt,2,255);
    WHILE txt[Length(txt)]='0' DO Dec(txt[0]);
    IF txt[Length(txt)]='.' THEN Dec(txt[0]);
    Cpct1:=txt
  END
END;

PROCEDURE MudarCatual;
VAR
  i:INTEGER;
BEGIN
  WITH ptr_painel(tela_atual^.client_data)^ DO BEGIN
    i:=catual;
    REPEAT
      Inc(catual);
      IF catual>grmax THEN catual:=1;
    UNTIL (grafico[catual]^.pontos<>0) or (i=catual)
  END
END;

PROCEDURE PreencherPopUp(VAR graf:curva;catual:INTEGER);
BEGIN
  bcatual^.xv_label:='Curva '+Si(catual);
  WITH graf DO IF pontos>0 THEN BEGIN
    bcatual^.fore_color:=c_white;
    tnome^.panel_value:=nome;
    tcol1^.panel_int:=col1;
    tcol2^.panel_int:=col2;
    tstyle^.panel_int:=estilo
  END
  ELSE bcatual^.fore_color:=c_black;
END;

PROCEDURE Destruir(p:ptr_painel);
VAR
  i:INTEGER;
BEGIN
  FOR i:=1 TO grmax DO Dispose(p^.grafico[i]);
  Dispose(p^.painel);
  Dispose(p^.janela);
  Dispose(p);
  p:=nil
END;

{$F+}

PROCEDURE SalvarBitmap(obj:Xv_opaque);
VAR
  i,j,k,ddx,ddy:INTEGER;
BEGIN
  close_window(jbitmap);
  IF sbitmap^.sel_setting and 1=1 THEN BEGIN
    SetViewPort(0,0,GetMaxX,GetMaxY,ClipOff);
    ddx:=GetMaxX; ddy:=GetMaxY
  END
  ELSE WITH active_w^.next^ DO BEGIN
    ddx:=dx-2;
    ddy:=dy-2
  END;
  Assign(saida,tbitmap^.panel_value);
  ReWrite(saida);
  b1:=Ord('B'); b2:=Ord('M');
  Write(saida,w1); {bfType}
  dw:=ddx+1;
  WHILE dw mod 8<>0 DO Inc(dw); {multiplo de 4 bytes em cada linha}
  dw:=dw*(ddy+1);
  dw:=54+16*4+dw div 2;
  Write(saida,w1,w2); {bfSize}
  w1:=0; w2:=0;
  Write(saida,w1,w2); {bfReserved1, bfReserved2}
  dw:=118; {54+16*4}
  Write(saida,w1,w2); {bfOffBits}
  dw:=40; {40 bytes}
  Write(saida,w1,w2); {biSize}
  dw:=ddx+1;
  Write(saida,w1,w2); {biWidth}
  dw:=ddy+1;
  Write(saida,w1,w2); {biHeigth}
  w1:=1;
  Write(saida,w1); {biPlanes}
  w1:=4;
  Write(saida,w1); {biBitCount}
  dw:=0;
  Write(saida,w1,w2,w1,w2,w1,w2,w1,w2); {...}
  dw:=16;
  Write(saida,w1,w2,w1,w2); {biClrUsed, biClrImportant}
  b4:=0;
  IF sbitmap^.sel_setting and 2=2 THEN j:=255 else j:=0;
  i:=256 div GetMaxColor;
  FOR k:=0 TO 15 DO BEGIN
    IF i*k>255 THEN b1:=255 ELSE b1:=i*k;
    b1:=b1 xor j; b2:=b1; b3:=b2;
    Write(saida,w1,w2);
  END;
  k:=ddx; WHILE k mod 8<>0 DO Inc(k);
  FOR i:=ddy DOWNTO 0 DO BEGIN
    j:=0;
    REPEAT
      b1:=getpixel(j,i) shl 4+getpixel(j+1,i);
      b2:=getpixel(j+2,i) shl 4+getpixel(j+3,i);
      Write(saida,w1);
      Inc(j,4)
    UNTIL j>=k;
  END;
  Close(saida);
  ttysw_output(msg,'Arquivo '+tbitmap^.panel_value+' escrito.'^M^J);
END;

PROCEDURE Redesenhar(tela:Xv_opaque);
VAR
  i,j,k:INTEGER;
  algo:BOOLEAN;
  t1,t2:REAL;

BEGIN
IF tela^.owner<>active_w THEN open_window(tela^.owner);
WITH ptr_painel(tela^.client_data)^ DO BEGIN
  algo:=FALSE;
  SetTextStyle(fonte,horizdir,tamanho);
  IF fonte=0 THEN k:=3 ELSE k:=0;
  WITH tela^ DO BEGIN
    xmin:=TextWidth('-9.999E-99');
    xmax:=dx-5;
    ymin:=3;
    ymax:=dy-TextHeight('8')-5;
    SetFillStyle(SolidFill,back_color);
    Bar(0,0,dx,dy);
  END;
  FOR i:=1 TO grmax DO WITH grafico[i]^ DO IF pontos<>0 THEN BEGIN
    IF (hmin<=0) or (hmax<=0) THEN xlog:=FALSE;
    IF (vmin<=0) or (vmax<=0) THEN ylog:=FALSE;
    IF (hmax<hmin) or (vmax<vmin) THEN grade:=FALSE;
    IF (i=1) or not mesmoy THEN BEGIN
      SetColor(cor[i]);
      SetTextJustify(RightText,TopText);
      OutTextXY(xmin,ymin+(i-1)*TextHeight('8'),Cpct1(vmax));
      SetTextJustify(RightText,BottomText);
      OutTextXY(xmin,ymax-(i-1)*TextHeight('8'),Cpct1(vmin));
    END;
    av:=(ymax-ymin)/(LogLin(vmin,ylog)-LogLin(vmax,ylog));                    {pixel y:=av*pty+bv}
    bv:=-av*LogLin(vmax,ylog);
    ah:=(xmax-xmin)/(LogLin(hmax,xlog)-LogLin(hmin,xlog));    {pixel x:=ah*ptx+bh}
    bh:=-ah*LogLin(hmin,xlog);
    algo:=TRUE;
  END;
  IF algo THEN BEGIN
    SetColor(c_white);
    SetTextJustify(RightText,CenterText);
    OutTextXY(xmin,(ymax+ymin+k) div 2,escv);
    SetTextJustify(CenterText,TopText);
    OutTextXY((xmin+xmax) div 2,ymax+k,esch);
    SetTextJustify(LeftText,TopText);
    OutTextXY(xmin,ymax+k,Cpct1(hmin));
    SetTextJustify(RightText,TopText);
    OutTextXY(xmax,ymax+k,Cpct1(hmax));
    WITH active_w^.gr_out DO SetViewPort(x1+xmin,y1+ymin,x1+xmax,y1+ymax,ClipOn);
    IF grade THEN WITH grafico[catual]^ DO IF pontos<>0 THEN BEGIN
      {grade X}
      SetLineStyle(DottedLn,0,NormWidth);
      SetColor(cor[6]);
      IF xlog and (hmax-hmin>hmin) THEN t1:=hmin
      ELSE t1:=hmax-hmin;
      t1:=Exp(Ln(10)*Round(Ln(t1)/Ln(10)-0.499999));
      t2:=Round(hmin/t1+0.5)*t1;
      WHILE t2<hmax DO
        IF xlog THEN BEGIN
          IF Round(t2/t1)=10 THEN BEGIN
            t1:=10*t1;
            SetColor(cor[1]);
          END;
          i:=Round(ah*Ln(t2)+bh);
          Line(i,0,i,ymax-ymin);
          t2:=t2+t1;
          SetColor(cor[6]);
        END
        ELSE BEGIN
          i:=Round(ah*t2+bh);
          Line(i,0,i,ymax-ymin);
          t2:=t2+t1
        END;
      {grade Y}
      IF ylog and (vmax-vmin>vmin) THEN t1:=vmin
      ELSE t1:=vmax-vmin;
      t1:=Exp(Ln(10)*Round(Ln(t1)/Ln(10)-0.499999));
      t2:=Round(vmin/t1+0.5)*t1;
      WHILE t2<vmax DO
        IF ylog THEN BEGIN
          IF Round(t2/t1)=10 THEN BEGIN
            t1:=10*t1;
            SetColor(cor[1]);
          END;
          i:=Round(av*Ln(t2)+bv);
          Line(0,i,xmax-xmin,i);
          t2:=t2+t1;
          SetColor(cor[6]);
        END
        ELSE BEGIN
          i:=Round(av*t2+bv);
          Line(0,i,xmax-xmin,i);
          t2:=t2+t1
        END;
      SetLineStyle(SolidLn,0,NormWidth);
    END;
    SetColor(cor[1]);
    MoveTo(0,0);
    LineTo(0,ymax-ymin);
    LineTo(xmax-ymin,ymax-ymin);
    FOR i:=1 TO grmax DO WITH grafico[i]^ DO IF pontos<>0 THEN BEGIN
      SetLineStyle(estilo,0,NormWidth);
      MoveTo(Rlim(ah*LogLin(ptx[1],xlog)+bh),Rlim(av*LogLin(pty[1],ylog)+bv));
      SetColor(cor[i]);
      FOR j:=2 TO pontos DO LineTo(Rlim(ah*LogLin(ptx[j],xlog)+bh),Rlim(av*LogLin(pty[j],ylog)+bv));
      bh:=bh+xmin;
      bv:=bv+ymin;
    END;
    cursorx:=-1000; cursory:=-1000;
    selecao:=FALSE;
  END
END
END;

PROCEDURE EventosPainel(tela:Xv_opaque);
VAR
  novox,novoy,i,k:INTEGER;
  hx,hy,d,d1:REAL;
  ch:CHAR;
  buf:STRING;
BEGIN
  WITH ptr_painel(tela^.client_data)^ DO BEGIN
    tela_atual:=active_w;
    IF (ie_code=MS_MIDDLE) or (ie_code=MS_BOTH) or (ie_code=9) THEN MudarCatual;
    IF ie_code=8 THEN BEGIN
      SetFillStyle(SolidFill,tela^.back_color);
      Bar(ie_locx-6,ie_locy-8,ie_locx-1,ie_locy);
      mouse_move(mousex-6,mousey)
    END;
    IF (ie_code>30) and (ie_code<256) THEN BEGIN
      SetTextStyle(smallfont,horizdir,4);
      SetColor(tela^.fore_color);
      OutTextXY(ie_locx,ie_locy-11,Chr(ie_code));
      mouse_move(mousex+6,mousey);
      Exit
    END;
    SetWriteMode(XorPut);
    SetColor(c_white);
    IF (ie_code=MS_LEFT) and (grafico[catual]^.pontos>0) THEN BEGIN
      xi:=ie_locx; yi:=ie_locy;
      xf:=xi; yf:=yi;
      Rectangle(cursorx-3,cursory-3,cursorx+3,cursory+3);
      cursorx:=-1000; cursory:=-1000;
      selecao:=TRUE;
      Exit
    END;
    IF selecao and (ie_code=LOC_DRAG) THEN BEGIN
      Rectangle(xi,yi,xf,yf);
      xf:=ie_locx; yf:=ie_locy;
      Rectangle(xi,yi,xf,yf);
      Exit
    END;
    IF selecao and (ie_shiftcode=0) THEN BEGIN
      selecao:=FALSE;
      Rectangle(xi,yi,xf,yf);
      IF (xi<xf) and (yi<yf) THEN BEGIN
        FOR i:=1 TO grmax DO WITH grafico[i]^ DO IF pontos>0 THEN
          IF (i=catual) or mesmoy THEN BEGIN
            hmin:=(xi-bh)/ah;
            IF xlog THEN hmin:=exp(hmin);
            hmax:=(xf-bh)/ah;
            IF xlog THEN hmax:=exp(hmax);
            vmin:=(yf-bv)/av;
            IF ylog THEN vmin:=exp(vmin);
            vmax:=(yi-bv)/av;
            IF ylog THEN vmax:=exp(vmax);
          END;
        SetWriteMode(NormalPut);
        Redesenhar(painel);
      END;
      Exit
    END;
    IF (ie_code=MS_RIGHT) or (ie_code=MS_MIDDLE) or (ie_code=LOC_DRAG) or (ie_code=MS_BOTH) or (ie_code=9) THEN
    WITH grafico[catual]^ DO BEGIN
      IF pontos=0 THEN Exit;
      novox:=ie_locx-1;
      novoy:=ie_locy-1;
      d:=1e30;
      FOR i:=1 TO pontos DO BEGIN
        d1:=sqr(ah*LogLin(ptx[i],xlog)+bh-novox)+sqr(av*LogLin(pty[i],ylog)+bv-novoy);
        IF d1<d THEN BEGIN d:=d1; k:=i END
      END;
      Rectangle(cursorx-3,cursory-3,cursorx+3,cursory+3);
      cursorx:=Rlim(ah*LogLin(ptx[k],xlog)+bh);
      cursory:=Rlim(av*LogLin(pty[k],ylog)+bv);
      Rectangle(cursorx-3,cursory-3,cursorx+3,cursory+3);
      SetWriteMode(NormalPut);
      buf:='Curva '+Si(catual)+' x:'+Cpct1(ptx[k])+' y:'+Cpct1(pty[k]);
      SetFillStyle(SolidFill,c_black);
      SetTextStyle(SmallFont,HorizDir,4);
      Bar(xmin+4,5,xmin+190,14);
      SetColor(c_white);
      OutTextXY(xmin+5,ymin,buf);
    END
  END
END;

PROCEDURE LerTabela;
VAR
  c1,c2:INTEGER;
  valorx,valory:REAL;
  texto:STRING;
  arquivo:TEXT;

  PROCEDURE Verificar;
  VAR
    p,f,c,i,k:INTEGER;
    valor:REAL;
    leux,leuy:BOOLEAN;
  BEGIN
    p:=1; i:=0; leux:=FALSE; leuy:=FALSE;
    REPEAT
      WHILE (texto[p]=' ') and (p<=Length(texto)) DO Inc(p);
      IF p<=Length(texto) THEN BEGIN
        f:=p;
        WHILE (texto[f]<>' ') and (f<=Length(texto)) DO Inc(f);
        {Correcao para ler arquivos do SwitCap}
        k:=f-4;
        IF (k>=p) and (texto[k]='D') THEN texto[k]:='E';
        Val(Copy(texto,p,f-p),valor,c);
        IF c=0 THEN BEGIN
          Inc(i);
          IF i=c1 THEN BEGIN
            valorx:=valor;
            leux:=TRUE
          END;
          IF i=c2 THEN BEGIN
            valory:=valor;
            leuy:=TRUE
          END;
          p:=f;
        END
      END;
      ok:=leux and leuy;
    UNTIL (p>Length(texto)) or (c<>0) or ok;
  END;

BEGIN
  WITH ptr_painel(tela_atual^.client_data)^ DO BEGIN
    ok:=FALSE;
    IF grafico[catual]^.nome='' THEN Exit;
    Assign(arquivo,grafico[catual]^.nome);
    {$I-} Reset(arquivo); {$I+}
    IF IOResult<>0 THEN Exit;
    WITH grafico[catual]^ DO BEGIN
      c1:=col1; c2:=col2;
      REPEAT
        ReadLn(arquivo,texto);
        Verificar;
      UNTIL ok or Eof(arquivo);
      IF ok THEN BEGIN
        hmaximo:=valorx;
        hminimo:=valorx;
        vmaximo:=valory;
        vminimo:=valory;
        pontos:=1;
        ptx[pontos]:=valorx;
        pty[pontos]:=valory;
        WHILE not Eof(arquivo) and ok and (pontos<ptmax) DO BEGIN
          ReadLn(arquivo,texto);
          Verificar;
          IF ok THEN BEGIN
            Inc(pontos);
            IF valorx>hmaximo THEN hmaximo:=valorx;
            IF valorx<hminimo THEN hminimo:=valorx;
            IF valory>vmaximo THEN vmaximo:=valory;
            IF valory<vminimo THEN vminimo:=valory;
            ptx[pontos]:=valorx;
            pty[pontos]:=valory
          END
        END;
        IF vminimo=vmaximo THEN BEGIN vmaximo:=vminimo+1; vminimo:=vminimo-1 END;
        IF hminimo=hmaximo THEN BEGIN hmaximo:=hminimo+1; hminimo:=hminimo-1 END;
        hmin:=hminimo;
        hmax:=hmaximo;
        vmin:=vminimo;
        vmax:=vmaximo;
        ok:=TRUE;
      END
    END;
    Close(arquivo);
  END
END;

PROCEDURE ApagarCurva(obj:Xv_opaque);
BEGIN
  WITH ptr_painel(tela_atual^.client_data)^ DO BEGIN
    grafico[catual]^.pontos:=0;
    close_window(jarquivo);
    MudarCatual;
    Redesenhar(painel)
  END
END;

PROCEDURE Reescalar;
VAR
  maximo,minimo:REAL;
  i:INTEGER;
BEGIN
WITH ptr_painel(tela_atual^.client_data)^ DO BEGIN
  hmin:=1e30; hmax:=-1e30; maximo:=-1e30; minimo:=1e30;
  FOR i:=1 TO grmax DO WITH grafico[i]^ DO IF pontos<>0 THEN BEGIN
    IF hminimo<hmin THEN hmin:=hminimo;
    IF hmaximo>hmax THEN hmax:=hmaximo;
    vmin:=vminimo;
    vmax:=vmaximo;
    IF vmin<minimo THEN minimo:=vmin;
    IF vmax>maximo THEN maximo:=vmax
  END;
  IF mesmoy THEN FOR i:=1 TO grmax DO WITH grafico[i]^ DO BEGIN
    vmin:=minimo;
    vmax:=maximo
  END;
  Redesenhar(painel);
END
END;

FUNCTION CriarInstancia:ptr_painel; FORWARD;

PROCEDURE TratarMenu(obj:Xv_opaque);
BEGIN
WITH ptr_painel(active_w^.client_data)^ DO BEGIN
  tela_atual:=active_w;
  CASE obj^.sel_menu OF
    1:BEGIN
        close_window(jarquivo);
        PreencherPopUp(grafico[catual]^,catual);
        open_window(jarquivo)
      END;
    2:BEGIN xlog:=not xlog; Redesenhar(painel) END;
    3:BEGIN ylog:=not ylog; Redesenhar(painel) END;
    4:BEGIN grade:=not grade; Redesenhar(painel) END;
    5:BEGIN mesmoy:=TRUE; Reescalar END;
    6:BEGIN mesmoy:=FALSE; Reescalar END;
    7:open_window(jescalas);
    8:BEGIN
        close_window(jtitulo);
        ttitulo^.panel_value:=painel^.owner^.xv_label;
        tfonte^.panel_int:=fonte;
        ttamanho^.panel_int:=tamanho;
        tesch^.panel_value:=esch;
        tescv^.panel_value:=escv;
        open_window(jtitulo)
      END;
     9:Back;
    10:BEGIN
         msg^.yc:=msg^.dy;
         ttysw_output(msg,'XYPlot - Versao '+versao+^M^J);
         ttysw_output(msg,'Antonio Carlos M. de Queiroz - COPPE/EE/UFRJ'^M^J);
         ttysw_output(msg,'e-mail: acmq@coe.ufrj.br'^M^J);
         ttysw_output(msg,'Memoria livre: '+Si(MaxAvail)+' bytes; Max. pontos:'+Si(ptmax)+^M^J);
       END;
    11:BEGIN
         painel1:=CriarInstancia;
         IF painel1<>nil THEN BEGIN
           open_window(painel1^.janela);
           IF not xv_ok THEN BEGIN
             ttysw_output(msg,'Impossivel abrir a janela'^M^J);
             Destruir(painel1);
           END;
         END
         ELSE ttysw_output(msg,'Memoria insuficiente'^M^J)
       END;
    12:BEGIN
         close_window(tela_atual);
         Destruir(ptr_painel(tela_atual^.client_data))
       END;
    13:open_window(jbitmap);
    14:open_window(jconfirmar);
  END
END
END;

PROCEDURE ForcarEscalas(obj:Xv_opaque);
VAR
  i:INTEGER;
BEGIN
  WITH ptr_painel(tela_atual^.client_data)^ DO BEGIN
    FOR i:=1 TO grmax DO
      WITH grafico[i]^ DO IF mesmoy or (i=catual) THEN BEGIN
        vmin:=tvmin^.panel_real;
        vmax:=tvmax^.panel_real;
      END;
    hmin:=thmin^.panel_real;
    hmax:=thmax^.panel_real;
    close_window(jescalas);
    Redesenhar(painel)
  END
END;

PROCEDURE ProximaCurva(obj:Xv_opaque);
BEGIN
  WITH ptr_painel(tela_atual^.client_data)^ DO BEGIN
    IF catual<grmax THEN Inc(catual) ELSE catual:=1;
    PreencherPopUp(grafico[catual]^,catual);
    draw_object(obj^.owner,TRUE);
  END
END;

PROCEDURE IncluirCurva(obj:Xv_opaque);
BEGIN
  WITH ptr_painel(tela_atual^.client_data)^ DO BEGIN
    close_window(jdiretorio);
    close_window(jarquivo);
    WITH grafico[catual]^ DO BEGIN
      nome:=tnome^.panel_value;
      col1:=tcol1^.panel_int;
      col2:=tcol2^.panel_int;
      estilo:=tstyle^.panel_int;
      LerTabela;
      IF mesmoy THEN BEGIN
        vmax:=grafico[1]^.vmax;
        vmin:=grafico[1]^.vmin
      END;
      IF ok THEN Redesenhar(painel)
      ELSE ttysw_output(msg,'Arquivo ou tabela nao encontrada'^M^J);
      IF pontos=ptmax THEN BEGIN
        SetTextStyle(DefaultFont,HorizDir,1);
        ttysw_output(msg,'Numero maximo de pontos ('+Si(ptmax)+') atingido'^M^J);
      END
    END
  END
END;

PROCEDURE MudarTitulo(obj:Xv_opaque);
BEGIN
  WITH ptr_painel(tela_atual^.client_data)^ DO BEGIN
    close_window(jtitulo);
    fonte:=tfonte^.panel_int;
    tamanho:=ttamanho^.panel_int;
    esch:=tesch^.panel_value;
    escv:=tescv^.panel_value;
    xv_set(tela_atual,ttitulo^.panel_value);
    Redesenhar(painel)
  END
END;

VAR
  nomes,total:INTEGER;

PROCEDURE LerDiretorio(obj:Xv_opaque);
VAR
  srec:SearchRec;

BEGIN
  SetTextStyle(SmallFont,HorizDir,4);
  IF obj<>cdiretorio THEN BEGIN
    SetFillStyle(SolidFill,cdiretorio^.back_color);
    Bar(0,0,cdiretorio^.dx,cdiretorio^.dy)
  END;
  nomes:=(cdiretorio^.dx) div 78;
  total:=0;
  FindFirst(tmask^.panel_value,AnyFile,srec);
  WHILE DosError=0 DO BEGIN
    OutTextXY(3+(total mod nomes)*78,(total div nomes)*8,srec.Name);
    FindNext(srec);
    Inc(total)
  END;
  Dec(total)
END;

PROCEDURE EscolherArquivo(obj:Xv_opaque);
VAR
  srec:SearchRec;
  D:DirStr;
  N:NameStr;
  E:ExtStr;
  i,k:INTEGER;

BEGIN
  IF ie_code=MS_LEFT THEN BEGIN
    k:=(ie_locx-3) div 78;
    IF k<nomes THEN BEGIN
      k:=k+((ie_locy-3) div 8)*nomes;
      IF k<=total THEN BEGIN
        i:=0;
        FindFirst(tmask^.panel_value,AnyFile,srec);
        WHILE (DosError=0) and (i<k) DO BEGIN
          FindNext(srec);
          Inc(i)
        END;
        FSplit(tmask^.panel_value,D,N,E);
        tnome^.panel_value:=D+srec.Name;
        xv_set(tnome,tnome^.xv_label)
      END
    END
  END
END;

PROCEDURE AbrirDiretorio(obj:Xv_opaque);
BEGIN
  open_window(jdiretorio)
END;

PROCEDURE Salvar(obj:Xv_opaque);
VAR
  w:Xv_opaque;
BEGIN
  {$I-}
  Assign(arquivo,'xyplot.cfg');
  ReWrite(arquivo);
  w:=active_w^.over;
  WHILE w<>active_w DO BEGIN
    IF (w<>jarquivo)
       and (w<>jescalas)
       and (w<>jtitulo)
       and (w<>jmsg)
       and (w<>jdiretorio)
       and (w<>jconfirmar) THEN
         WITH ptr_painel(w^.client_data)^ DO BEGIN
           WriteLn(arquivo,janela^.xv_label);
           WITH janela^ DO WriteLn(arquivo,x,' ',y,' ',dx,' ',dy);
           WriteLn(arquivo,xlog);
           WriteLn(arquivo,ylog);
           WriteLn(arquivo,grade);
           WriteLn(arquivo,mesmoy);
           k:=0;
           FOR i:=1 TO grmax DO IF grafico[i]^.pontos<>0 THEN Inc(k);
           WriteLn(arquivo,k,' ',fonte,' ',tamanho);
           WriteLn(arquivo,esch);
           WriteLn(arquivo,escv);
           FOR i:=1 TO grmax DO WITH grafico[i]^ DO IF pontos<>0 THEN BEGIN
             WriteLn(arquivo,nome);
             WriteLn(arquivo,col1,' ',col2,' ',estilo);
             WriteLn(arquivo,hmin,' ',hmax,' ',vmin,' ',vmax)
           END
         END;
    w:=w^.over
  END;
  Close(arquivo);
  {$I+}
  IF IOResult=0 THEN BEGIN
    ttysw_output(msg,'Estado salvo no arquivo xyplot.cfg'^M^J);
    Delay(500);
    xv_end:=TRUE
  END
  ELSE ttysw_output(msg,'Erro na escrita do arquivo de configuracao'^M^J);
END;

PROCEDURE MudaFonte(obj:Xv_opaque);
BEGIN
END;

PROCEDURE Terminar(obj:Xv_opaque);
BEGIN
  xv_end:=TRUE
END;

{$F-}

FUNCTION CriarInstancia:ptr_painel;
VAR
  v:ptr_painel;
  i,j:INTEGER;
  ok:BOOLEAN;
BEGIN
  IF MaxAvail<SizeOf(xpainel)+2*SizeOf(xv_widget) THEN BEGIN
    CriarInstancia:=nil;
    Exit;
  END;
  New(v);
  WITH v^ DO BEGIN
    normal_client_data:=v;
    janela:=xv_create(frame);
    WITH janela^ DO BEGIN
      dx:=319; dy:=239;
      x:=(GetMaxX-dx) div 2; y:=(GetMaxY-dy) div 2;
      xv_label:='XY Plot';
      adjust_exit:=FALSE;
      menu_name:=jmenu;
    END;
    painel:=xv_create(canvas);
    WITH painel^ DO BEGIN
      fore_color:=c_white;
      back_color:=c_black;
      notify_handler:=Redesenhar;
      event_handler:=EventosPainel;
    END;
    FOR i:=1 TO grmax DO BEGIN
      IF MaxAvail<SizeOf(curva) THEN BEGIN
        FOR j:=i-1 DOWNTO 1 DO Dispose(grafico[j]);
        Dispose(painel);
        Dispose(janela);
        Dispose(v);
        CriarInstancia:=nil;
        Exit
      END;
      New(grafico[i]);
      grafico[i]^.pontos:=0;
    END;
    catual:=1; xlog:=FALSE; ylog:=FALSE; grade:=TRUE;
    hmin:=1e30; hmax:=-1e30; mesmoy:=FALSE;
    fonte:=tfonte^.panel_int;
    tamanho:=ttamanho^.panel_int;
    esch:=tesch^.panel_value;
    escv:=tescv^.panel_value;
  END;
  CriarInstancia:=v
END;

BEGIN
  DetectGraph(placa,modo);
  IF placa=CGA THEN modo:=CGAC1;
  IF ParamCount=2 THEN BEGIN
    Val(ParamStr(1),placa,i);
    Val(ParamStr(2),modo,i)
  END;
  xv_init(placa,modo);
  IF GetMaxColor=3 THEN cor:=corCGA
  ELSE IF GetMaxColor=1 THEN cor:=corpb;
  normal_length:=13;
  jmenu:=xv_create(menu);
  item_create('Incluir/mudar curvas');
  item_create('Escala X: linear/log');
  item_create('Escala Y: linear/log');
  item_create('Grade: sim/nao');
  item_create('Reescalar com Y iguais');
  item_create('Reescalar com Y diferentes');
  item_create('Reescalar com limites dados');
  item_create('Mudar titulos e fonte');
  item_create('Por a janela no fundo');
  item_create('Informacoes');
  item_create('Outro painel');
  item_create('Fechar painel');
  item_create('Salvar bitmap');
  item_create('Terminar');
  jmenu^.xv_label:='Opcoes';
  jmenu^.notify_handler:=TratarMenu;
  jmenu^.sel_menu:=1;
  jbitmap:=xv_create(frame);
  WITH jbitmap^ DO BEGIN
    dx:=179; dymin:=69; dy:=69;
    xv_label:='Bitmap para Windows'
  END;
  tbitmap:=xv_create(textfield);
  WITH tbitmap^ do BEGIN
    xv_label:='Arquivo';
    panel_value:='xyplot.bmp'
  END;
  sbitmap:=xv_create(setting);
  WITH sbitmap^ DO BEGIN
    xv_label:='opcoes';
    y:=15;
    item_create('tudo');
    item_create('invertido');
  END;
  bbitmap:=xv_create(button);
  WITH bbitmap^ DO BEGIN
    xv_label:='Escrever';
    y:=30;
    notify_handler:=SalvarBitmap
  END;
  jarquivo:=xv_create(frame);
  WITH jarquivo^ DO BEGIN
    xv_label:='Curva';
    dx:=200; dy:=130;
    x:=(GetMaxX-dx) div 2; y:=(GetMaxY-dy) div 2;
  END;
  tnome:=xv_create(textfield);
  WITH tnome^ DO BEGIN
    xv_label:='Arquivo:'; y:=15;
  END;
  tcol1:=xv_create(textfield);
  WITH tcol1^ DO BEGIN
    xv_label:='Coluna X:';
    y:=30; value_length:=3; field_type:=int_field;
    min_value:=1; panel_int:=1
  END;
  tcol2:=xv_create(textfield);
  WITH tcol2^ DO BEGIN
    xv_label:='Coluna Y:';
    y:=45; value_length:=3; field_type:=int_field;
    min_value:=1; panel_int:=2
  END;
  bcatual:=xv_create(button);
  WITH bcatual^ DO BEGIN
    notify_handler:=ProximaCurva;
  END;
  bdir:=xv_create(button);
  WITH bdir^ DO BEGIN
    xv_label:='Diretorio';
    x:=68;
    notify_handler:=AbrirDiretorio;
  END;
  tstyle:=xv_create(textfield);
  WITH tstyle^ DO BEGIN
    xv_label:='Estilo:';
    field_type:=int_field; panel_int:=0; value_length:=3;
    min_value:=0; max_value:=4;
    y:=60;
  END;
  bincluir:=xv_create(button);
  WITH bincluir^ DO BEGIN
    xv_label:='Incluir curva';
    y:=75; notify_handler:=IncluirCurva;
  END;
  bapagar:=xv_create(button);
  WITH bapagar^ DO BEGIN
    xv_label:='Apagar  curva';
    y:=90; notify_handler:=ApagarCurva;
  END;
  jescalas:=xv_create(frame);
  WITH jescalas^ DO BEGIN
    xv_label:='Escalas'; dx:=170;
    x:=(GetMaxX-dx) div 2; y:=(GetMaxY-dy) div 2;
  END;
  thmin:=xv_create(textfield);
  WITH thmin^ DO BEGIN
    xv_label:='Xmin:'; field_type:=real_field; panel_real:=0.2;
  END;
  thmax:=xv_create(textfield);
  WITH thmax^ DO BEGIN
    xv_label:='Xmax:'; field_type:=real_field; panel_real:=5; y:=15;
  END;
  tvmin:=xv_create(textfield);
  WITH tvmin^ DO BEGIN
    xv_label:='Ymin:'; field_type:=real_field; panel_real:=-60; y:=30;
  END;
  tvmax:=xv_create(textfield);
  WITH tvmax^ DO BEGIN
    xv_label:='Ymax:'; field_type:=real_field; panel_real:=10; y:=45;
  END;
  bok:=xv_create(button);
  WITH bok^ DO BEGIN
    xv_label:='ok'; notify_handler:=ForcarEscalas; y:=60;
  END;
  jtitulo:=xv_create(frame);
  WITH jtitulo^ DO BEGIN
    xv_label:='Titulos e fonte';
    dx:=253;
    dy:=116;
    x:=(GetMaxX-dx) div 2; y:=(GetMaxY-dy) div 2;
  END;
  tescv:=xv_create(textfield);
  WITH tescv^ DO BEGIN
    xv_label:='Unidade vertical';
    y:=30;
  END;
  tesch:=xv_create(textfield);
  WITH tesch^ DO BEGIN
    xv_label:='Unidade horizontal';
    y:=15;
    value_length:=11;
  END;
  ttamanho:=xv_create(textfield);
  WITH ttamanho^ DO BEGIN
    xv_label:='Tamanho';
    y:=60;
    value_length:=5;
    field_type:=int_field;
    panel_int:=4;
    min_value:=0;
  END;
  tfonte:=xv_create(textfield);
  WITH tfonte^ DO BEGIN
    xv_label:='Fonte';
    y:=45;
    value_length:=5;
    field_type:=int_field;
    panel_int:=2;
    min_value:=0;
    max_value:=10;
  END;
  ttitulo:=xv_create(textfield);
  WITH ttitulo^ DO BEGIN
    xv_label:='Titulo';
    value_length:=23;
  END;
  btitulo:=xv_create(button);
  WITH btitulo^ DO BEGIN
    xv_label:='ok';
    y:=75;
    notify_handler:=MudarTitulo;
  END;
  jmsg:=xv_create(frame);
  WITH jmsg^ DO BEGIN
    xv_label:='Mensagens'; dx:=319; dy:=63; dymin:=27;
    x:=(GetMaxX-dx) div 2; y:=20;
  END;
  msg:=xv_create(tty);
  jdiretorio:=xv_create(frame);
  WITH jdiretorio^ DO BEGIN
    xv_label:='Diretorio';
    dy:=159;
    dx:=255;
  END;
  tmask:=xv_create(textfield);
  WITH tmask^ DO BEGIN
    xv_label:='Mascara';
    value_length:=24;
    panel_value:='*.*';
    notify_handler:=LerDiretorio;
  END;
  cdiretorio:=xv_create(canvas);
  WITH cdiretorio^ DO BEGIN
    y:=15;
    notify_handler:=LerDiretorio;
    event_handler:=EscolherArquivo
  END;
  jconfirmar:=xv_create(frame);
  WITH jconfirmar^ DO BEGIN
    xv_label:='Terminar?';
    dymin:=55; dxmin:=118;
    dy:=55; dx:=150;
    x:=(GetMaxX-dx) div 2; y:=80
  END;
  bsalvar:=xv_create(button);
  WITH bsalvar^ DO BEGIN
    xv_label:='Salvar e terminar';
    notify_handler:=Salvar;
    owner^.mouse_obj:=bsalvar
  END;
  bterminar:=xv_create(button);
  WITH bterminar^ DO BEGIN
    xv_label:='Terminar'; y:=15;
    notify_handler:=Terminar;
  END;
  tela_atual:=nil;
  Assign(arquivo,'xyplot.cfg');
  {$I-} Reset(arquivo); {$I+}
  IF IOResult=0 THEN BEGIN
    WHILE not SeekEof(arquivo) DO BEGIN
      IF KeyPressed THEN BEGIN
        Close(arquivo);
        ttysw_output(msg,'Leitura interrompida'^M^J);
        GoTo Continuar
      END;
      painel1:=CriarInstancia;
      IF painel1<>nil THEN BEGIN
        WITH painel1^ DO BEGIN
          ReadLn(arquivo,janela^.xv_label);
          WITH janela^ DO ReadLn(arquivo,x,y,dx,dy);
          ReadLn(arquivo,c); xlog:=c='T';
          ReadLn(arquivo,c); ylog:=c='T';
          ReadLn(arquivo,c); grade:=c='T';
          ReadLn(arquivo,c); mesmoy:=c='T';
          Read(arquivo,k);
          IF SeekEoln(arquivo) THEN ReadLn(arquivo)
          ELSE BEGIN {Compativel com versao anterior}
            ReadLn(arquivo,fonte,tamanho);
            ReadLn(arquivo,esch);
            ReadLn(arquivo,escv)
          END;
          open_window(janela);
          IF xv_ok THEN BEGIN
            tela_atual:=janela;
            FOR i:=1 TO k DO WITH grafico[i]^ DO BEGIN
              ReadLn(arquivo,nome);
              ReadLn(arquivo,col1,col2,estilo);
              catual:=i;
              LerTabela;
              ReadLn(arquivo,hmin,hmax,vmin,vmax);
              IF ok THEN draw_object(janela,TRUE)
              {Isto chama Redesenhar como "callback",
               inicializando o contexto grafico corretamente}
            END
          END
          ELSE BEGIN
            Destruir(painel1); Close(arquivo); GoTo Continuar;
          END
        END
      END
      ELSE BEGIN
        ttysw_output(msg,'Memoria insuficiente'^M^J);
        Close(arquivo);
        GoTo Continuar
      END
    END
  END
  ELSE painel1:=CriarInstancia;
  IF painel1<>nil THEN tela_atual:=painel1^.janela;
 Continuar:
  IF tela_atual<>nil THEN xv_main_loop(tela_atual);
  TextMode(80);
END.
