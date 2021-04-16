/*
Conversion ASIZ-WatSnap
By Antonio Carlos Moreirao de Queiroz - COPPE/EE/UFRJ (acmq@coe.ufrj.br).
*/

#define versao "1.0b - 15/03/94"

#include <stdio.h>
#include <stdlib.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <math.h>
#include <string.h>

/* Interface objects */

Xv_opaque 
  fprincipal,painel,tcgs,tcgd,tgds,toutput,tswfreq,
  tymax,tymin,txmax,txmin,tpontos,tloglin,
  tentrada,tsaida,tkfile,bconverter;
  
/* Globals */

FILE *entrada,*saida;
int t1,t2,t3,t4;
char c1;
char nome[10];
double Cgs,Cgd,Gds,f1,f2;

/* Functions */

lereol() /* without "void" is the only way that works. C... */
{
  do t1=fscanf(entrada,"%c",&c1); while (c1!='\n' && t1!=EOF);
}

/* Callbacks */

int Converter(item,evento)
Panel_item item;
Event *evento;
{
  printf("Converting...\n",versao);
  Cgs=atof((char*)xv_get(tcgs,PANEL_VALUE));
  Cgd=atof((char*)xv_get(tcgd,PANEL_VALUE));
  Gds=atof((char*)xv_get(tgds,PANEL_VALUE));
  printf("Cgs value for unitary Gm: %.9lE\n",Cgs);
  printf("Cgd value for unitary Gm: %.9lE\n",Cgd);
  printf("Gds value for unitary Gm: %.9lE\n",Gds);
  printf("Reading file %s\n", (char*)xv_get(tentrada,PANEL_VALUE));
  entrada=fopen((char*)xv_get(tentrada,PANEL_VALUE),"r");
  if (!entrada) {
    printf("File %s not found\n",(char*)xv_get(tentrada,PANEL_VALUE));
    return(0);
  }
  saida=fopen((char*)xv_get(tsaida,PANEL_VALUE),"w");
  if (!saida) {
    printf("File %s could not be created\n",(char*)xv_get(tsaida,PANEL_VALUE));
    return(0);
  }
  /* Header */
  fprintf(saida,
    "* Input file for Watsnap, created from ASIZ file %s by SnapNet\n", 
    (char*)xv_get(tentrada,PANEL_VALUE));
  fprintf(saida,"* Cgs value for unitary Gm: %.9lE\n",Cgs);
  fprintf(saida,"* Cgd value for unitary Gm: %.9lE\n",Cgd);
  fprintf(saida,"* Gds value for unitary Gm: %.9lE\n",Gds);
  fprintf(saida,"* Increase if not enough:\n*O maxels 200 maxswi 50\n");
  /* NetList */
  fscanf(entrada,"%d%*c1",&t1); /* c1 because of the ^M */
  fprintf(saida,"* Nodes: %d\n",t1);
  while (fscanf(entrada,"%s",nome)!=EOF) {
    /* printf("%s,",nome); */
    switch ((char)nome[0]) {
      case 'S':
          fscanf(entrada,"%d%d",&t1,&t2);
          fprintf(saida,"S.%s %d %d",nome,t1,t2);
          do {
            t1=fscanf(entrada,"%c",&c1);
            if (t1!=EOF && c1==' ') {
              fscanf(entrada,"%d",&t1);
              fprintf(saida," %d",t1);
            }
          }
          while (c1!='\n' && t1!=EOF);
          fprintf(saida,"\n");
        break;
      case 'R':
          fscanf(entrada,"%d%d%lg",&t1,&t2,&f1);
          fprintf(saida,"R.%s %d %d %.9lE\n",nome,t1,t2,f1);
          lereol();
        break;
      case 'L':
          fscanf(entrada,"%d%d%lg",&t1,&t2,&f1);
          fprintf(saida,"L.%s %d %d %.9lE\n",nome,t1,t2,f1);
          lereol();
        break;
      case 'C':
          fscanf(entrada,"%d%d%lg",&t1,&t2,&f1);
          fprintf(saida,"C.%s %d %d %.9lE\n",nome,t1,t2,f1);
          lereol();
        break;
      case 'V':
          fscanf(entrada,"%d%d%lg",&t1,&t2,&f1);
          fprintf(saida,"VS.%s %d %d %.9lE ST 1\n",nome,t1,t2,f1);
          lereol();
        break;
      case 'I':
          fscanf(entrada,"%d%d%lg",&t1,&t2,&f1);
          fprintf(saida,"JS.%s %d %d %.9lE ST 1\n",nome,t1,t2,f1);
          lereol();
        break;
      case 'O':
          fscanf(entrada,"%d%d%d",&t1,&t2,&t3);
          fprintf(saida,"OP.%s %d %d %d %d\n",nome,t2,t1,t3,0);
          lereol();
        break;
      case 'E':
          fscanf(entrada,"%d%d%d%d%lg",&t1,&t2,&t3,&t4,&f1);
          fprintf(saida,"VV.%s %d %d %d %d %.9lE\n",nome,t3,t4,t1,t2,f1);
          lereol();
        break;
      case 'F':
          fscanf(entrada,"%d%d%d%d%lg",&t1,&t2,&t3,&t4,&f1);
          fprintf(saida,"CC.%s %d %d %d %d %.9lE\n",nome,t3,t4,t1,t2,f1);
          lereol();
        break;
      case 'G':
          fscanf(entrada,"%d%d%d%d%lg",&t1,&t2,&t3,&t4,&f1);
          fprintf(saida,"CV.%s %d %d %d %d %.9lE\n",nome,t3,t4,t1,t2,f1);
          lereol();
        break;
      case 'H':
          fscanf(entrada,"%d%d%d%d%lg",&t1,&t2,&t3,&t4,&f1);
          fprintf(saida,"VC.%s %d %d %d %d %.9lE\n",nome,t3,t4,t1,t2,f1);
          lereol();
        break;
      case 'A':
          fscanf(entrada,"%d%d%d%lg",&t1,&t2,&t3,&f1);
          fprintf(saida,"VV.%s %d %d %d %d %.9lE\n",nome,t2,t1,t3,0,f1);
          lereol();
        break; 
      case 'M':
          fscanf(entrada,"%d%d%d%lg%lg",&t1,&t2,&t3,&f1,&f2);
          fprintf(saida,"VC.%s %d %d %d %d %.9lE\n",nome,t2,t3,t1,t3,f1);
          if (Gds==0.0) {
            if (f2!=0.0) fprintf(saida,"R.%s_ds %d %d %.9G\n",nome,t1,t3,1/f2);
          }
          else 
            fprintf(saida,"R.%s_ds %d %d %.9G\n",nome,t1,t3,1/f1/Gds);
          fprintf(saida,"C.%s_gs %d %d %.9lE\n",nome,t2,t3,f1*Cgs);
          if (Cgd!=0.0)
            fprintf(saida,"C.%s_gd %d %d %.9lE\n",nome,t1,t2,f1*Cgd);
          lereol();
        break;
    }
  }
  /* End */
  fprintf(saida,"#\n");
  fprintf(saida,"shold yes\n");
  fprintf(saida,"output %s\n",(char*)xv_get(toutput,PANEL_VALUE));
  fprintf(saida,"phinterv EQual\n");
  fprintf(saida,"swf %s\n",(char*)xv_get(tswfreq,PANEL_VALUE));
  fprintf(saida,"xaxis %s %s %s %s\nband 1\n",
    (char*)xv_get(txmin,PANEL_VALUE),
    (char*)xv_get(txmax,PANEL_VALUE),
    (char*)xv_get(tloglin,PANEL_VALUE),
    (char*)xv_get(tpontos,PANEL_VALUE));
  fprintf(saida,"vlimits %s %s\nvauto no\n",
    (char*)xv_get(tymin,PANEL_VALUE),
    (char*)xv_get(tymax,PANEL_VALUE));
  fprintf(saida,"kfile %s\n",(char*)xv_get(tkfile,PANEL_VALUE));
  fprintf(saida,"ufile %s\n",(char*)xv_get(tkfile,PANEL_VALUE));
  fprintf(saida,"wrtfile %s\n",(char*)xv_get(tkfile,PANEL_VALUE));
  fprintf(saida,"pldevice xwindows\nytype db\n");
  fprintf(saida,"frequency\nwrite\nplot\n");
  fclose(entrada);
  fclose(saida);
  printf("Converted file saved as %s\n",(char*)xv_get(tsaida,PANEL_VALUE));
}

Panel_setting Change(item,evento)
Panel_item item;
Event *evento;
{
  printf("%s %s\n", (char*)xv_get(item,PANEL_LABEL_STRING),(char*) xv_get(item,PANEL_VALUE));
/* To allow tab to work */
return (PANEL_NEXT);
}

/* Main */

main(argc, argv)
int argc;
char *argv[];
{
  /* Inicialization */
  printf("\nSnapNet - ASIZ to WatsNap netlist converter\nBy Antonio C. M. de Queiroz (acmq@coe.ufrj.br)\nCOPPE/EE/Universidade Federal do Rio de Janeiro\nVersion %s\n\n",versao);
  xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);
  /* Frame and panel */
  fprincipal=(Frame)xv_create(NULL,FRAME,
    XV_LABEL,"SnapNet",
    XV_WIDTH, 400,
    XV_HEIGHT, 400,
  NULL);
  painel=(Panel)xv_create(fprincipal,PANEL,
    XV_X,0,
    XV_Y,0,
    XV_WIDTH,  WIN_EXTEND_TO_EDGE,
    XV_HEIGHT, 30,
    PANEL_LAYOUT,PANEL_VERTICAL,
    NULL);
  /* Files */
  tentrada=(Panel_item)xv_create(painel,PANEL_TEXT,
    PANEL_LABEL_STRING, "Input file:",
    PANEL_VALUE_DISPLAY_LENGTH, 20,
    PANEL_VALUE_STORED_LENGTH, 20, 
    PANEL_NOTIFY_PROC, Change,
    PANEL_VALUE, "xxx.net",  
    NULL);
  tsaida=(Panel_item)xv_create(painel,PANEL_TEXT,
    PANEL_LABEL_STRING, "Watsnap input file:",
    PANEL_VALUE_DISPLAY_LENGTH, 20,
    PANEL_VALUE_STORED_LENGTH, 20, 
    PANEL_VALUE, "xxx.snp",  
    PANEL_NOTIFY_PROC, Change,
    NULL);
  tkfile=(Panel_item)xv_create(painel,PANEL_TEXT,
    PANEL_LABEL_STRING, "Watsnap files prefix:",
    PANEL_VALUE_DISPLAY_LENGTH, 20,
    PANEL_VALUE_STORED_LENGTH, 20, 
    PANEL_VALUE, "xxx",  
    PANEL_NOTIFY_PROC, Change,
    NULL);
  /* Parameters */
  tcgs=(Panel_item)xv_create(painel,PANEL_TEXT,
    PANEL_LABEL_STRING, "Unitary Cgs capacitance:",
    PANEL_VALUE_DISPLAY_LENGTH, 20,
    PANEL_VALUE_STORED_LENGTH, 20, 
    PANEL_NOTIFY_PROC, Change,
    PANEL_VALUE, "0.001",  
    NULL);
  tcgd=(Panel_item)xv_create(painel,PANEL_TEXT,
    PANEL_LABEL_STRING, "Unitary Cgd capacitance:",
    PANEL_VALUE_DISPLAY_LENGTH, 20,
    PANEL_VALUE_STORED_LENGTH, 20, 
    PANEL_NOTIFY_PROC, Change,
    PANEL_VALUE, "0",  
    NULL);
  tgds=(Panel_item)xv_create(painel,PANEL_TEXT,
    PANEL_LABEL_STRING, "Unitary Gds conductance (0=as read) :",
    PANEL_VALUE_DISPLAY_LENGTH, 20,
    PANEL_VALUE_STORED_LENGTH, 20, 
    PANEL_NOTIFY_PROC, Change,
    PANEL_VALUE, "0",  
    NULL);
  toutput=(Panel_item)xv_create(painel,PANEL_TEXT,
    PANEL_LABEL_STRING, "Output(s):",
    PANEL_VALUE_DISPLAY_LENGTH, 20,
    PANEL_VALUE_STORED_LENGTH, 20, 
    PANEL_NOTIFY_PROC, Change,
    PANEL_VALUE, "v(1)",  
    NULL);
  tswfreq=(Panel_item)xv_create(painel,PANEL_TEXT,
    PANEL_LABEL_STRING, "Switching frequency(Hz):",
    PANEL_VALUE_DISPLAY_LENGTH, 20,
    PANEL_VALUE_STORED_LENGTH, 20, 
    PANEL_NOTIFY_PROC, Change,
    PANEL_VALUE, "1",  
    NULL);
  /* Scales */
  txmin=(Panel_item)xv_create(painel,PANEL_TEXT,
    PANEL_LABEL_STRING, "Minimum frequency:",
    PANEL_VALUE_DISPLAY_LENGTH, 20,
    PANEL_VALUE_STORED_LENGTH, 20, 
    PANEL_NOTIFY_PROC, Change,
    PANEL_VALUE, "0.001",  
    NULL);
  txmax=(Panel_item)xv_create(painel,PANEL_TEXT,
    PANEL_LABEL_STRING, "Maximum frequency:",
    PANEL_VALUE_DISPLAY_LENGTH, 20,
    PANEL_VALUE_STORED_LENGTH, 20,
    PANEL_VALUE, "1.5",   
    PANEL_NOTIFY_PROC, Change,   
    NULL);
  tymin=(Panel_item)xv_create(painel,PANEL_TEXT,
    PANEL_LABEL_STRING, "Minimum gain:",
    PANEL_VALUE_DISPLAY_LENGTH, 20,
    PANEL_VALUE_STORED_LENGTH, 20,  
    PANEL_NOTIFY_PROC, Change,   
    PANEL_VALUE, "-80", 
    NULL);
  tymax=(Panel_item)xv_create(painel,PANEL_TEXT,
    PANEL_LABEL_STRING, "Maximum gain:",
    PANEL_VALUE_DISPLAY_LENGTH, 20,
    PANEL_VALUE_STORED_LENGTH, 20, 
    PANEL_VALUE, "10", 
    PANEL_NOTIFY_PROC, Change, 
    NULL);
  tpontos=(Panel_item)xv_create(painel,PANEL_TEXT,
    PANEL_LABEL_STRING, "Frequency points:",
    PANEL_VALUE_DISPLAY_LENGTH, 20,
    PANEL_VALUE_STORED_LENGTH, 20, 
    PANEL_VALUE, "400", 
    PANEL_NOTIFY_PROC, Change,
    NULL);
  tloglin=(Panel_item)xv_create(painel,PANEL_TEXT,
    PANEL_LABEL_STRING,  "Scale: LOG or LIN:",
    PANEL_VALUE_DISPLAY_LENGTH, 20,
    PANEL_VALUE_STORED_LENGTH, 20, 
    PANEL_VALUE, "LIN", 
    PANEL_NOTIFY_PROC, Change,
    NULL);
  bconverter=(Panel_item)xv_create(painel,PANEL_BUTTON,
    PANEL_LABEL_STRING, "Convert",
    PANEL_NOTIFY_PROC, Converter,
    NULL);
  window_fit(painel);
  window_fit(fprincipal);  
  xv_main_loop(fprincipal);
  exit(0);    
}
