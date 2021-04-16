PROGRAM XTool;
{
*********************************************************************
 Por Antonio Carlos Moreirao de Queiroz.
 Demonstrativo do "toolkit" XView para Turbo Pascal.
 Este programa demonstra o uso de instancias de uma mesma janela.
 O arquivo que o programa desenha e do tipo "arquivo" abaixo.
 Os valores dos bytes representam tons de cinza, variando de 0 a 63.
 V. 1.0 de 02/01/92
 V. 1.1 de 04/04/92 Apenas VGA
 V. 1.2 de 13/04/93 Salva bitmaps de Windows
*********************************************************************
}
USES Mickey,Graph,Crt,XView;

TYPE buffer=ARRAY[0..199,0..255] OF BYTE;

TYPE {Instancia de janela}
  ptr_visor=^visor;
  visor=RECORD
    janela,texto1,canvas1,setting1,button1:Xv_opaque;
  END;

VAR
  menu1:Xv_opaque;            {menu das janelas}
  memoria,terminal:Xv_opaque; {janela de informacoes}
  visor1:ptr_visor;           {alocador de instancias}
  arquivo:FILE of buffer;
  figura:buffer;
  i,placa,modo:INTEGER;
  saida:FILE OF WORD;
  b1,b2,b3,b4:BYTE;
  w1:WORD absolute b1;
  w2:WORD absolute b3;
  dw:LONGINT absolute b1;

FUNCTION CriarInstancia:ptr_visor; FORWARD;

FUNCTION Si(i:LONGINT):STRING; {string representando inteiro}
VAR
  txt:STRING;
BEGIN
  Str(i,txt);
  Si:=txt
END;

{$F+} {todos os "callbacks" usam chamada "far"}

PROCEDURE Destruir(w:Xv_opaque); {"notify_handler" da janela}
                                 {desalocador de instancias}
BEGIN
  WITH w^ DO BEGIN
    WITH ptr_visor(client_data)^ DO BEGIN
      Dispose(janela);
      Dispose(texto1);
      Dispose(canvas1);
      WITH setting1^ DO BEGIN
        FreeMem(item_setting[1],Length(item_setting[1]^)+1);
        FreeMem(item_setting[2],Length(item_setting[2]^)+1)
      END;
      Dispose(setting1);
      Dispose(button1);
    END;
    Dispose(ptr_visor(client_data))
  END
END;

PROCEDURE Desenha(obj:Xv_opaque); {"notify_handler" dos objetos do painel}
                                  {redesenha a imagem}
VAR
  i,j,k:INTEGER;
  kx,ky:REAL;
BEGIN
  WITH ptr_visor(obj^.client_data)^ DO {com a instancia atual...}
    IF texto1^.panel_value<>'' THEN BEGIN;
      Assign(arquivo,texto1^.panel_value);
      {$I-} Reset(arquivo); {$I+}
      IF IOResult=0 THEN BEGIN
        Read(arquivo,figura);
        Close(arquivo);
        IF setting1^.sel_setting=1 THEN
          FOR i:=0 TO 199 DO
            FOR j:=0 TO 255 DO PutPixel(j,i,figura[i,j] div 4)
        ELSE BEGIN
          kx:=255/(canvas1^.dx-2);
          ky:=199/(canvas1^.dy-2);
          FOR i:=0 TO canvas1^.dy-2 DO BEGIN
            k:=Round(ky*i);
            FOR j:=0 TO canvas1^.dx-2 DO PutPixel(j,i,figura[k,Round(kx*j)] div 4)
          END
        END
      END
      ELSE ttysw_output(terminal,'Arquivo inexistente'^M^J);
    END;
  ttysw_output(terminal,'MemAvail='+Si(MemAvail)+' MaxAvail='+Si(MaxAvail)+^M^J);
END;

PROCEDURE Outro(obj:Xv_opaque); {"notify_handler" do botao}
                                {cria outro painel, se possivel}
BEGIN
  visor1:=CriarInstancia;
  open_window(visor1^.janela);
  IF not xv_ok THEN BEGIN {Nao ha memoria para abrir a nova janela}
    ttysw_output(terminal,'Impossivel abrir nova janela'^M^J);
    Destruir(visor1^.janela);
  END
END;

PROCEDURE MenuJanela(obj:Xv_opaque); {"notify_handler" do menu}
VAR
  txt:STRING;
  i,j,k:INTEGER;
BEGIN
  CASE obj^.sel_menu OF
    1:close_window(active_w);
    2:Back;
    3:xv_end:=TRUE;
    4:ttysw_output(terminal,'MemAvail='+Si(MemAvail)+' MaxAvail='+Si(MaxAvail)+^M^J);
    5:WITH ptr_visor(active_w^.client_data)^ DO BEGIN
        txt:=texto1^.panel_value;
        Assign(arquivo,txt);
        {$I-} Reset(arquivo); {$I+}
        IF IOResult=0 THEN BEGIN
          ttysw_output(terminal,'Lendo figura...'^M^J);
          Read(arquivo,figura);
          Close(arquivo);
          k:=Pos('.',txt);
          IF k<>0 THEN txt:=Copy(txt,1,k-1);
          txt:=txt+'.bmp';
          Assign(saida,txt);
          ReWrite(saida);
          b1:=Ord('B'); b2:=Ord('M');
          Write(saida,w1); {bfType}
          dw:=52278; {54+256*4+256*200}
          Write(saida,w1,w2); {bfSize}
          w1:=0; w2:=0;
          Write(saida,w1,w2); {bfReserved1, bfReserved2}
          dw:=1078; {54+256*4}
          Write(saida,w1,w2); {bfOffBits}
          dw:=40;
          Write(saida,w1,w2); {biSize}
          dw:=256;
          Write(saida,w1,w2); {biWidth}
          dw:=200;
          Write(saida,w1,w2); {biHeigth}
          w1:=1;
          Write(saida,w1); {biPlanes}
          w1:=8;
          Write(saida,w1); {biBitCount}
          dw:=0;
          Write(saida,w1,w2,w1,w2,w1,w2,w1,w2); {...}
          dw:=256;
          Write(saida,w1,w2,w1,w2); {biClrUsed, biClrImportant}
          {So as primeiras 64 entradas da palette sao usadas}
          {Mas todas devem existir. Escolhido repetir 4 vezes}
          FOR i:=1 TO 4 DO BEGIN
            k:=0; b4:=0;
            REPEAT
              b1:=4*k; b2:=b1; b3:=b2;
              Write(saida,w1,w2);
              Inc(k)
            UNTIL k>63
          END;
          ttysw_output(terminal,'Escrevendo bitmap...'^M^J);
          FOR i:=199 DOWNTO 0 DO BEGIN
            j:=0;
            REPEAT
              b1:=figura[i,j];
              b2:=figura[i,j+1];
              Write(saida,w1);
              Inc(j,2)
            UNTIL j>255
          END;
          Close(saida);
          ttysw_output(terminal,'Arquivo '+txt+', 256 cores, escrito.'^M^J);
        END
        ELSE ttysw_output(terminal,'Arquivo '+txt+' nao encontrado'^M^J);
      END;
    6:WITH ptr_visor(active_w^.client_data)^ DO BEGIN
        txt:=texto1^.panel_value;
        Assign(arquivo,txt);
        {$I-} Reset(arquivo); {$I+}
        IF IOResult=0 THEN BEGIN
          ttysw_output(terminal,'Lendo figura...'^M^J);
          Read(arquivo,figura);
          Close(arquivo);
          k:=Pos('.',txt);
          IF k<>0 THEN txt:=Copy(txt,1,k-1);
          txt:=txt+'.bmp';
          Assign(saida,txt);
          ReWrite(saida);
          b1:=Ord('B'); b2:=Ord('M');
          Write(saida,w1); {bfType}
          dw:=25718; {54+16*4+256*200/2}
          Write(saida,w1,w2); {bfSize}
          w1:=0; w2:=0;
          Write(saida,w1,w2); {bfReserved1, bfReserved2}
          dw:=118; {54+16*4}
          Write(saida,w1,w2); {bfOffBits}
          dw:=40; {40 bytes}
          Write(saida,w1,w2); {biSize}
          dw:=256;
          Write(saida,w1,w2); {biWidth}
          dw:=200;
          Write(saida,w1,w2); {biHeigth}
          w1:=1;
          Write(saida,w1); {biPlanes}
          w1:=4;
          Write(saida,w1); {biBitCount}
          dw:=0;
          Write(saida,w1,w2,w1,w2,w1,w2,w1,w2); {...}
          dw:=0;
          Write(saida,w1,w2,w1,w2); {biClrUsed, biClrImportant}
          k:=0; b4:=0;
          REPEAT
            b1:=16*k; b2:=b1; b3:=b2;
            Write(saida,w1,w2);
            Inc(k)
          UNTIL k>15;
          ttysw_output(terminal,'Escrevendo bitmap...'^M^J);
          FOR i:=199 DOWNTO 0 DO BEGIN
            j:=0;
            REPEAT
              b1:=16*(figura[i,j] div 4)+figura[i,j+1] div 4;
              b2:=16*(figura[i,j+2] div 4)+figura[i,j+3] div 4;
              Write(saida,w1);
              Inc(j,4)
            UNTIL j>255
          END;
          Close(saida);
          ttysw_output(terminal,'Arquivo '+txt+', 16 cores, escrito.'^M^J);
        END
        ELSE ttysw_output(terminal,'Arquivo '+txt+' nao encontrado'^M^J);
      END
  END
END;

{$F-}

FUNCTION CriarInstancia:ptr_visor; {cria nova instancia do painel}
VAR v:ptr_visor;
BEGIN
  New(v); {Seria bom verificar se ha espaco na heap antes}
          {Como esta pode ocorrer "heap overflow", mas e muito improvavel}
  WITH v^ DO BEGIN
    normal_client_data:=v; {xv_create inicializa client_data com isto}
    janela:=xv_create(frame);  {cria a janela base}
    WITH janela^ DO BEGIN
      xv_label:='Imagens';
      dx:=180;
      dy:=180;
      menu_name:=menu1;
      notify_handler:=Destruir {a janela se desaloca ao ser fechada}
    END;
    {O canvas deve ser o primeiro, pois seu handler sera chamado antes do
     fim do desenho da janela. O primeiro objeto e' o ultimo a ser desenhado.}
    canvas1:=xv_create(canvas); {cria o painel de desenho}
    WITH canvas1^ DO BEGIN
      y:=40;
      notify_handler:=Desenha;
    END;
    texto1:=xv_create(textfield); {cria o campo para o nome do arquivo}
    WITH texto1^ DO BEGIN
      xv_label:='Fig';
      value_length:=17;
      notify_handler:=Desenha;
    END;
    setting1:=xv_create(setting); {cria a escolha normal/escalado}
    WITH setting1^ DO BEGIN
      item_create('Nor');
      item_create('Esc');
      y:=20;
      exclusive:=TRUE;
      sel_setting:=2;
      notify_handler:=Desenha;
    END;
    button1:=xv_create(button); {cria o botao que abre outro painel}
    WITH button1^ DO BEGIN
      xv_label:='Outro';
      y:=20; x:=70;
      notify_handler:=Outro;
    END
  END;
  CriarInstancia:=v;
END;

BEGIN
  type_hatch:=SlashFill;
  DetectGraph(placa,modo); {So funciona direito na VGA}
  IF placa=CGA THEN modo:=CGAC1;
  xv_init(placa,modo);
  FOR i:=0 TO MaxColors DO BEGIN {inicializa 16 tons de cinza}
    SetPalette(i,i);
    SetRGBPalette(i,4*i,4*i,4*i);
  END;
  c_normal:=10;
  c_active:=8;
  menu1:=xv_create(menu); {cria o menu}
  WITH menu1^ DO BEGIN
    item_create('Desligar');
    item_create('No fundo');
    item_create('Fim');
    item_create('Memoria');
    item_create('Salvar bitmap 256 cores');
    item_create('salvar bitmap 16 cores');
    xv_label:='Janela';
    notify_handler:=MenuJanela;
  END;
  memoria:=xv_create(frame); {cria o painel de informacoes}
  WITH memoria^ DO BEGIN
    xv_label:='Informacoes';
    dx:=300; x:=600; y:=600;
  END;
  terminal:=xv_create(tty);  {... e seu terminal}
  visor1:=CriarInstancia; {cria o primeiro painel}
  xv_main_loop(visor1^.janela); {...e o aciona}
  TextMode(80);
END.