%{
	#include<stdio.h>
	#include<stdlib.h>
	#include<string.h>
	int yylex(void);
	int yyerror(const char *s);
	int success = 1;
	int current_data_type;
	int expn_type=-1;
	int temp;
	extern FILE*yyout;
	int forind=0;
        int flag=0;
	struct symbol_table{
    char var_name[30];
    int type;
   }var_list[20];
   
   // you may associate an integer with a datatype (say var_list[i].type=1 may imply that variable var_list[i].var_name is of type int) and store that integer against the variable whenever you deal with a declaration statement
   
	int var_count=-1;//number of entries in the symbol table
 
	extern int lookup_in_table(char var[30]);
  //returns the data type associated with the variable name being passed to, returns -1 if in case the variable is undeclared
	
	
  extern void insert_to_table(char var[30], int type);
  //adds a new variable along with its data type to the table and terminates with a "multiple declaration error message", if in case the variable is already being defined
  
%}

%union{
int data_type;
char var_name[30];
int var_num;
}

%token HEADER MAIN LB RB LCB RCB LSB RSB SC CO COMA VAR VAR_NUM EQ ADD SUBTRACT MULTIPLY DIV WENN AND OR NOT ET NET LT GT LET GET DANN OTHERWISE WENNDANN WHEN THEN ENDWHEN RECUR UPTO FLO OUT CONTINUE LESEN AUS QUOTES FUNC START FINISH STRG RET CALL MKOWN REPLACE

%token<data_type>INT
%token<data_type>STR
%token<data_type>DEC
%token<data_type>TF
%token<data_type>LIST
%token<data_type>ARRAY
%left VAR_NUM
%type<data_type>DATA_TYPE
%type<var_name>VAR
%type<var_num>VAR_NUM

%start prm

%%
prm	: header MAIN_TYPE mainn lb rb lcb start co BODY finish scc rcb {printf("\n parsing successful\n");}
          |header FUNCTIONS MAIN_TYPE mainn lb rb lcb start co BODY finish scc rcb {printf("\n parsing successful\n");}
          |header MACROS FUNCTIONS MAIN_TYPE mainn lb rb lcb start co BODY finish scc rcb {printf("\n parsing successful\n");}
          |header MACROS MAIN_TYPE mainn lb rb lcb start co BODY finish scc rcb {printf("\n parsing successful\n");}
    ;
scc: SC
    ;
BODY	: DECLARATION_STATEMENTS PROGRAM_STATEMENTS
        
         
      ;
      
     
DECLARATION_STATEMENTS : DECLARATION_STATEMENT DECLARATION_STATEMENTS 
						  {printf("\n Declaration section successfully parsed\n");}
			| DECLARATION_STATEMENT
			
			
     ;  


PROGRAM_STATEMENTS : PROGRAM_STATEMENT PROGRAM_STATEMENTS 
						  {printf("\n program statements successfully parsed\n");}
		    
			| PROGRAM_STATEMENT
			|
			
		       
      ;
DECLARATION_STATEMENT: DATA_TYPE co lcbt VAR_LIST rcbt sc 
                      |DATA_TYPE Arr co lcbt VAR_LIST rcbt sc 
                      |UDDT DECLARATION_STATEMENTS
		       |UDDT
		       |MACROS DECLARATION_STATEMENTS
		       |MACROS
                      ;
lcbt: LCB 
     ;
rcbt: RCB
     ;
UDDT : mkown DATA_TYPE VAR_LIST sc 
       |VARCHK VAR_LIST sc 
       |VARCHK EQ{fprintf(yyout,"=");} A_EXPN sc
       
       ;
MACROS: replace VAR_LIST DXPN sc
       |VARCHK EQ{fprintf(yyout,"=");} A_EXPN sc 
       ;
DXPN: VAR_NUM {fprintf(yyout," %d",$1);}  
     ;     
VAR_LIST : VAR coma VAR_LIST {insert_to_table($1,current_data_type);fprintf(yyout,",%s",$1);}
	      | VAR {insert_to_table($1,current_data_type);if(flag==1){fprintf(yyout,"char %s[100]",$1);}else {fprintf(yyout," %s",$1);} }
	      |VAR lsb VAR_NUM rsb lsb VAR_NUM rsb coma VAR_LIST  {insert_to_table($1,current_data_type);fprintf(yyout," ,%s[%d][%d]",$1,$3,$6);}
	      
	      |VAR lsb VAR_NUM rsb lsb VAR_NUM rsb {insert_to_table($1,current_data_type);fprintf(yyout," %s[%d][%d]",$1,$3,$6);}
	      
	      |VAR lsb VAR_NUM rsb coma VAR_LIST  {insert_to_table($1,current_data_type);fprintf(yyout," %s[%d]",$1,$3);}
	      
	      |VAR lsb VAR_NUM rsb {insert_to_table($1,current_data_type);fprintf(yyout," %s[%d]",$1,$3);}
	     
	      
	      
               ;
PROGRAM_STATEMENT : VARCHK eq A_EXPN sc {expn_type=-1;}
                  |CONDITIONAL_STATEMENTS 
                  |LOOPS
                  |IPOP
                  |FUNCTIONS
                  |VARCHK EQ{fprintf(yyout,"=");} call VARCHK lb PARAMETERSC rb sc
                  
                 
                  ;
                 

CONDITIONAL_STATEMENTS : wenn lb CONDITION rb dann lcb ACTION rcb
                        
                        |wenn lb CONDITION rb dann lcb ACTION rcb WENNDY otherwise lcb ACTION rcb
                       
                        
                        
                        
                        ;
WENNDY: wenndann lb CONDITION rb dann lcb ACTION rcb WENNDY
        |
        
       ;
LOOPS: when lb CONDITION rb then lcb ACTION rcb endwhen scc 
       |recur lcb ACTION rcb upto lb CONDITION rb sc
       |flo lb FORCHK eq A_EXPN sccc uptoo{fprintf(yyout,"%s<=",var_list[forind].var_name);} DELIM then ACTION1 rb lcb ACTION rcb 
       
       
       ;
FORCHK: VAR { if((temp=lookup_in_table($1))!=-1)
			{
				if(expn_type==-1)
				{  
				       
					expn_type=temp;
				}else if(expn_type!=temp)
				{
					printf("\ntype mismatch in the expression\n");
					exit(0);
				}
			}else
			{
				printf("\n variable \"%s\" undeclared\n",$1);exit(0);
			}
			for(int i =0; i<=var_count;i++){ 
            if (strcmp(var_list[i].var_name,$1)==0) 
            {forind=i;
}
 }

fprintf(yyout,"%s",var_list[forind].var_name);}
;
DELIM: VAR_NUM {fprintf(yyout,"%d;",$1);}

      ;
sccc : {fprintf(yyout,";");}
;
uptoo: UPTO
       ;
CONDITION : VARCHK LOG_EXP VARCHK    
           | VARCHK LOG_EXP VAR_NUM {fprintf(yyout,"%d",$3);}
           | VAR_NUM {fprintf(yyout,"%d",$1);}
           | VARCHK
          ;


ACTION1: VARCHK eq A_EXPN  {expn_type=-1;	}  
        ;
ACTION : VARCHK eq A_EXPN sc ACTION {expn_type=-1;	}
        | VARCHK eq A_EXPN sc out sc {expn_type=-1;	}
        | out sc
        | VARCHK eq A_EXPN sc continuee sc {expn_type=-1;	}
        | continuee sc
        |VARCHK eq A_EXPN sc {expn_type=-1;	}
        
        ;
 
IPOP : lesen lb INPUTT rb sc
      |aus lb OUTPUTT rb sc
      |aus lb{fprintf(yyout,"\"%%d\",");} OUTPUTTT rb sc
      |aus lb quotes VAR{fprintf(yyout,"%s",$4);}  quotes rb sc 
      
      
      
       ;
OUTPUTTT:   OUTPUTTT OP OUTPUTTT
		| lb OUTPUTTT rb
		|VAR_NUM {fprintf(yyout,"%d",$1);}
	     ; 
		    
OUTPUTT:       OUTPUTT OP OUTPUTT
		| lb OUTPUTT rb 
		| VAR {
			if((temp=lookup_in_table($1))!=-1)
			{
				if(expn_type==-1)
				{
					expn_type=temp;
				}else if(expn_type!=temp)
				{
					printf("\ntype mismatch in the expression\n");
					exit(0);
				}
			}else
			{
				printf("\n variable \"%s\" undeclared\n",$1);exit(0);
			}
		if(temp==0) {fprintf(yyout,"\"%%d\\n\" , %s",$1);} //int
		       else if (temp==1) {fprintf(yyout,"\"%%s\\n\" , %s", $1);} //str
                       else if (temp==3) {fprintf(yyout,"\"%%f\\n\" , %s", $1);} //float
                       else if (temp==4) {fprintf(yyout,"\"%%i\\n\" , %s", $1);} //bool , prints 0 or 1
                       else if (temp==5) {fprintf(yyout,"\"%%s\\n\" , %s", $1);} //list?
                       else if (temp==6) {fprintf(yyout,"\"%%f\\n\" , %s", $1);} //array?
		     }
		 
		 
		 ;
INPUTT:VAR {if((temp=lookup_in_table($1))!=-1)
			{      printf("%d",temp);
				if(expn_type==-1)
				{  
				       
					expn_type=temp;
				}else if(expn_type!=temp)
				{
					printf("\ntype mismatch in the expression\n");
					exit(0);
				}
			}else
			{
				printf("\n variable \"%s\" undeclared\n",$1);exit(0);
			}
			if(temp==0) {fprintf(yyout,"\"%%d\" , &%s",$1);} //int
		       else if (temp==1) {fprintf(yyout,"\"%%s\" , &%s", $1);} //str
                       else if (temp==3) {fprintf(yyout,"\"%%f\" , &%s", $1);} //float
                       else if (temp==4) {fprintf(yyout,"\"%%i\" , &%s", $1);} //bool , prints 0 or 1
                       else if (temp==5) {fprintf(yyout,"\"%%s\" , &%s", $1);} //list?
                       else if (temp==6) {fprintf(yyout,"\"%%f\" , &%s", $1);} //array?
		}
	|VAR coma INPUTT
                       
	;		
FUNCTIONS :DATA_TYPEE VARR lb PARAMETERS rb co DATA_TYPEEE lcb FUNCBODY ret VARCHK sc rcb 

           ;
DATA_TYPEE: {fprintf(yyout, "int "); }
            ;
DATA_TYPEEE: INT 
            |STR
            |DEC
            |LIST
            |TF
            ;
VARR : VAR {insert_to_table($1,current_data_type);fprintf(yyout," %s",$1);}
       ;    
FUNCBODY: PROGRAM_STATEMENT
         |DECLARATION_STATEMENT
         |PROGRAM_STATEMENT DECLARATION_STATEMENT FUNCBODY
         |DECLARATION_STATEMENT PROGRAM_STATEMENT FUNCBODY
         
     ;
PARAMETERS :DATA_TYPE co lcbt VARR rcbt
            |DATA_TYPE co lcbt VARR rcbt comaa PARAMETERS
            ;
comaa: COMA {fprintf(yyout,",");}
       ;            

           ;
PARAMETERSC:A_EXPN 
            |A_EXPN comaa PARAMETERSC
            |VARCHK 
            |VARCHK comaa PARAMETERSC
            ;
VARCHK:VAR {
			if((temp=lookup_in_table($1))!=-1)
			{
				if(expn_type==-1)
				{  
				       
					expn_type=temp;
				}else if(expn_type!=temp)
				{
					printf("\ntype mismatch in the expression\n");
					exit(0);
				}
			}else
			{
				printf("\n variable \"%s\" undeclared\n",$1);exit(0);
			}
		fprintf(yyout,"%s",$1);
		     }
       |VAR coma VARCHK
       ;
        
A_EXPN: A_EXPN OP A_EXPN
		| lb A_EXPN rb 
		| VAR {
			if((temp=lookup_in_table($1))!=-1)
			{
				if(expn_type==-1)
				{
					expn_type=temp;
				}else if(expn_type!=temp)
				{
					printf("\ntype mismatch in the expression\n");
					exit(0);
				}
			}else
			{
				printf("\n variable \"%s\" undeclared\n",$1);exit(0);
			}
		{fprintf(yyout,"%s",$1);}
		     }
		 |VAR_NUM {fprintf(yyout,"%d",$1);}
		  
		   ;
		   
OP : ADD {fprintf(yyout,"+");}
     |SUBTRACT {fprintf(yyout,"-");}
     |MULTIPLY {fprintf(yyout,"*");}
     |DIV {fprintf(yyout,"/");}
     ;	  
MAIN_TYPE : FUNC {fprintf(yyout,"\nvoid ");}
            ;
DATA_TYPE : INT {
			$$=$1;
			current_data_type=$1;
			flag=0;
			fprintf(yyout,"int");
		} 
	| STR  {
			$$=$1;
			flag=1;
		
			current_data_type=$1;
			
		}
	| DEC  {
			$$=$1;
			fprintf(yyout,"float");
			current_data_type=$1;
			
		}
	| TF  {
			$$=$1;
			fprintf(yyout,"bool");
			current_data_type=$1;
			
		}
	| LIST  {
			$$=$1;
			fprintf(yyout,"list");
			current_data_type=$1;
			
		}
	;
	
Arr :  ARRAY {}
       ;
co : CO 
LOG_EXP:and 
       |or
       |not
       |et
       |net
       |gt
       |lt
       |get
       |let
      ;
      
mainn: MAIN {fprintf(yyout,"main");}
lcb: LCB {fprintf(yyout,"\n{");}
rcb: RCB {fprintf(yyout,"}");}
lb : LB {fprintf(yyout,"(");}
rb : RB {fprintf(yyout,")");}
header: HEADER {fprintf(yyout,"#include<stdio.h>\n");}
lsb: LSB {}
rsb: RSB {}
sc: SC {fprintf(yyout,";\n");}
coma: COMA {fprintf(yyout," ");}

wenn : WENN{fprintf(yyout,"\nif");}
and : AND {fprintf(yyout,"&&");}
or : OR {fprintf(yyout,"||");}
not : NOT{fprintf(yyout,"!");}
et : ET{fprintf(yyout,"==");}
net : NET{fprintf(yyout,"!=");}
lt: LT{fprintf(yyout,"<");}
continuee: CONTINUE{fprintf(yyout,"continue");}
lesen: LESEN { fprintf(yyout, "\nscanf"); }
aus: AUS { fprintf(yyout, "printf"); }
quotes: QUOTES { fprintf(yyout, "\""); }
start: START{ fprintf(yyout, " "); }
finish: FINISH { fprintf(yyout,"  "); }
ret: RET { fprintf(yyout, "return\t"); }
call: CALL{ fprintf(yyout, " "); }
mkown: MKOWN { fprintf(yyout, "typedef "); }
replace: REPLACE { fprintf(yyout, "#define"); }
gt: GT { fprintf(yyout, ">"); }
let: LET { fprintf(yyout, "<="); }
get: GET { fprintf(yyout, ">="); }
dann: DANN { fprintf(yyout, " "); }
otherwise: OTHERWISE { fprintf(yyout, "\nelse"); }
wenndann: WENNDANN { fprintf(yyout, "\nelse if"); }
when: WHEN { fprintf(yyout, "\nwhile"); }
then: THEN { fprintf(yyout, " "); }
endwhen: ENDWHEN { fprintf(yyout, " "); }
recur: RECUR { fprintf(yyout, "\ndo"); }
upto: UPTO { fprintf(yyout, "\nwhile"); }
flo: FLO { fprintf(yyout, "for"); }
out: OUT { fprintf(yyout, "break");}
eq: EQ {fprintf(yyout,"=");}
%%

int lookup_in_table(char var[30])//returns the data type associated with 
{
  for(int i = var_count; i>=0; i--){
    if (strcmp(var_list[i].var_name, var) == 0)
      return  var_list[i].type;
      
  }
	return -1;
}

void insert_to_table(char var[30], int type)
{
  temp = lookup_in_table(var);
  if(temp!=-1){
    printf("multiple declaration for variable %s\n\n", var); exit(0);
  }
  var_count++;
  
  strcpy(var_list[var_count].var_name , var);
 
  var_list[var_count].type = type;
}


int main()
{
    yyout=fopen("output.c","w");
    yyparse();
/*    if(success)
    	printf("Parsing Successful\n");*/
    return 0;
}

int yyerror(const char *msg)
{
	extern int yylineno;
	printf("Parsing Failed\nLine Number: %d %s\n",yylineno,msg);
	success = 0;
	return 0;
}


