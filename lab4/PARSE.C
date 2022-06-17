/****************************************************/
/* File: parse.c                                    */
/* The parser implementation for the TINY compiler  */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#include "globals.h"
#include "util.h"
#include "scan.h"
#include "parse.h"

static TokenType token; /* holds current token */

/* function prototypes for recursive calls */
static TreeNode * stmt_sequence(void);
static TreeNode * statement(void);
static TreeNode * if_stmt(void);
static TreeNode * repeat_stmt(void);
static TreeNode * assign_stmt(void);
static TreeNode * read_stmt(void);
static TreeNode * write_stmt(void);
static TreeNode * exp(void);
static TreeNode * simple_exp(void);
static TreeNode * term(void);
static TreeNode * factor(void);
static TreeNode * varlist(void);
static TreeNode * decl_stmt(void);
static TreeNode * declarations(void);
static TreeNode * while_stmt(void);
static TreeNode * program(void);
static TreeNode * for_stmt(void);
static TreeNode * to_stmt(void);
static TreeNode * switch_stmt(void);
static TreeNode * case_stmt(void);


static void syntaxError(char * message)
{ fprintf(listing,"\n>>> ");
  fprintf(listing,"Syntax error at line %d: %s",lineno,message);
  Error = TRUE;
}

static void match(TokenType expected)
{ if (token == expected) token = getToken();
  else {
    syntaxError("unexpected token -> ");
    printToken(token,tokenString);
    fprintf(listing,"      ");
  }
}

TreeNode* program(void)
{
    TreeNode* t = newProgNode();
    t->child[0] = declarations();
    t->child[1] = stmt_sequence();
    //return t;
}

TreeNode * stmt_sequence(void)
{ 
  TreeNode * t = statement();
  TreeNode * p = t;
  while ((token!=ENDFILE) && (token!=END) &&
         (token!=ELSE) && (token!=UNTIL) && (token!=WHILE) && (token!=BREAK))
  { TreeNode * q;
    match(SEMI);
    q = statement();
    if (q!=NULL) {
      if (t==NULL) t = p = q;
      else /* now p cannot be NULL either */
      { 
          if (p->sibling!=NULL)
          {
                p->sibling->sibling = q;
                p = q;
          }
          else{
                p->sibling = q;
                p = q;
          }
      }
    }
  }
  return t;
}



TreeNode * statement(void)
{ TreeNode * t = NULL;
  switch (token) {
    case IF : t = if_stmt(); break;
    case REPEAT : t = repeat_stmt(); break;
    case ID : t = assign_stmt(); break;
    case READ : t = read_stmt(); break;
    case WRITE : t = write_stmt(); break;
    case DO: t = while_stmt();break;
    case FOR: t = for_stmt();break;
    case SWITCH: t = switch_stmt();break;

    default : syntaxError("unexpected token -> ");
              printToken(token,tokenString);
              token = getToken();
              break;
  } /* end case */
  return t;
}

TreeNode* declarations(void)
{
    TreeNode* t = decl_stmt();
    if (!t)
        return NULL;     
    TreeNode* p = t;
    while (token != ENDFILE)
    {
        TreeNode* q;
        match(SEMI);
        q = decl_stmt();
        if (q != NULL)
        {
            if (t == NULL)
                t = p = q;
            else
            {
                p->sibling = q;
                p = q;
            }
        }
        else
            break;
    }
    return t;
   
}

TreeNode * decl_stmt(void)
{
    if (token != INT && token != STRING && token != BOOL)
        return NULL;

    TreeNode* t = newStmtNode(DeclK);

    t->attr.op = token;
    match(token);
    t->child[1] = varlist();

    return t;
}

TreeNode * varlist(void)
{
    TreeNode* t = newExpNode(IdK);
    TreeNode* p = t;
    if (token == ID)
        t->attr.name = copyString(tokenString);
    match(ID);

    if (token == COMMA)
    {
        TreeNode* q;
        match(COMMA);

        p->child[0] = varlist();

    }
   
    return t;
}



TreeNode* while_stmt(void)
{
    TreeNode* t = newStmtNode(WhileK);
    match(DO);
    if (t != NULL)
        t->child[0] = stmt_sequence();
    match(WHILE);
    if (t != NULL)
        t->child[1] = exp();
    return t;
}

TreeNode* for_stmt(void)
{
    TreeNode* t = newStmtNode(ForK);
    match(FOR);
    if (t != NULL) {
        t->child[0] = assign_stmt();
        t->child[1] = to_stmt();
    }
    match(THEN);
    t->child[2] = stmt_sequence();
    match(END);
    return t;
}

TreeNode* to_stmt(void)
{
    TreeNode* t = NULL;
    if (token == TO){
        t = newStmtNode(ToK);
        match(TO);
        if (t != NULL)
            t->child[0] = factor();
    }
    else {
        t = newStmtNode(DownK);
        match(DOWNTO);
        if (t != NULL)
            t->child[0] = factor();
    }
    return t;
}


TreeNode * if_stmt(void)
{ TreeNode * t = newStmtNode(IfK);
    
  match(IF);
  if (t!=NULL) t->child[0] = exp();
  match(THEN);
  if (t!=NULL) t->child[1] = stmt_sequence();
  if (token==ELSE) {
    TreeNode* q = newStmtNode(ElseK);
    match(ELSE);
    if (q!=NULL) q->child[0] = stmt_sequence();
    t->sibling = q;
  }
  match(END);
  return t;
}

TreeNode* switch_stmt(void)
{
    TreeNode* t = newStmtNode(SwitchK);

    match(SWITCH);
    if (t != NULL) t->child[0] = factor();
    t->child[1] = case_stmt();
  
    return t;
}

TreeNode* case_stmt(void)
{
    TreeNode* t = newStmtNode(CaseK);
    TreeNode* p = t;
    match(CASE);
    t->child[0] = factor();
    t->child[1] = stmt_sequence();
    match(BREAK);
    if (token == CASE) 
        p->sibling = case_stmt();
    if (token == DEFAULT) {
        TreeNode* q = newStmtNode(DefaultK);
        match(DEFAULT);
        q->child[0] = stmt_sequence();
        p->sibling = q;
    }
    return t;
}

TreeNode * repeat_stmt(void)
{ TreeNode * t = newStmtNode(RepeatK);
  match(REPEAT);
  if (t!=NULL) t->child[0] = stmt_sequence();
  match(UNTIL);
  if (t!=NULL) t->child[1] = exp();
  return t;
}

TreeNode * assign_stmt(void)
{ TreeNode * t = newStmtNode(AssignK);
  if ((t!=NULL) && (token==ID))
    t->attr.name = copyString(tokenString);
  match(ID);
  match(ASSIGN);
  if (t!=NULL) t->child[0] = exp();
  return t;
}

TreeNode * read_stmt(void)
{ TreeNode * t = newStmtNode(ReadK);
  match(READ);
  if ((t!=NULL) && (token==ID))
    t->attr.name = copyString(tokenString);
  match(ID);
  return t;
}

TreeNode * write_stmt(void)
{ TreeNode * t = newStmtNode(WriteK);
  match(WRITE);
  if (t!=NULL) t->child[0] = exp();
  return t;
}

TreeNode * exp(void)
{ TreeNode * t = simple_exp();
  if ((token==LT)||(token==EQ)||(token==LTE)||(token == GT)||(token == GTE)) {
    TreeNode * p = newExpNode(OpK);
    if (p!=NULL) {
      p->child[0] = t;
      p->attr.op = token;
      t = p;
    }
    match(token);
    if (t!=NULL)
      t->child[1] = simple_exp();
  }
  return t;
}

TreeNode * simple_exp(void)
{ TreeNode * t = term();
  while ((token==PLUS)||(token==MINUS))
  { TreeNode * p = newExpNode(OpK);
    if (p!=NULL) {
      p->child[0] = t;
      p->attr.op = token;
      t = p;
      match(token);
      t->child[1] = term();
    }
  }
  return t;
}

TreeNode * term(void)
{ TreeNode * t = factor();
  while ((token==TIMES)||(token==OVER))
  { TreeNode * p = newExpNode(OpK);
    if (p!=NULL) {
      p->child[0] = t;
      p->attr.op = token;
      t = p;
      match(token);
      p->child[1] = factor();
    }
  }
  return t;
}

TreeNode * factor(void)
{ TreeNode * t = NULL;
  switch (token) {
    case NUM :
      t = newExpNode(ConstK);
      if ((t!=NULL) && (token==NUM))
        t->attr.val = atoi(tokenString);
      match(NUM);
      break;
    case ID :
      t = newExpNode(IdK);
      if ((t!=NULL) && (token==ID))
        t->attr.name = copyString(tokenString);
      match(ID);
      break;
    case STR:
        t = newExpNode(StrK);
        if ((t != NULL) && (token == STR))
            t->attr.name = copyString(tokenString);
        match(STR);
      break; 
    case LPAREN :
      match(LPAREN);
      t = exp();
      match(RPAREN);
      break;
    default:
      syntaxError("unexpected token -> ");
      printToken(token,tokenString);
      token = getToken();
      break;
    }
  return t;
}

/****************************************/
/* the primary function of the parser   */
/****************************************/
/* Function parse returns the newly 
 * constructed syntax tree
 */
TreeNode * parse(void)
{ TreeNode * t;
  token = getToken();
  t = program();
  if (token!=ENDFILE)
    syntaxError("Code ends before file\n");
  return t;
}
