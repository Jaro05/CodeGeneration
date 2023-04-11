/* $Id: gen_code_stubs.c,v 1.7 2023/03/27 16:39:12 leavens Exp $ */
#include "utilities.h"
#include "gen_code.h"


/* HOW TO COMPILE AND RUN THE CODE

    // TO COMPILE
    gcc -o compiler ast.c ast.h code.c code.h compiler_main.c file_location.c file_location.h gen_code.c gen_code.h id_attrs.c id_attrs.h id_use.c id_use.h instruction.c instruction.h label.c label.h lexer.c lexer.h lexer_output.c lexer_output.h lexical_address.c lexical_address.h parser.c parser.h reserved.c reserved.h scope.c scope.h scope_check.c scope_check.h symtab.c symtab.h token.c token.h unparser.c unparser.h utilities.c utilities.h
    gcc compiler ast.c ast.h code.c code.h compiler_main.c file_location.c file_location.h gen_code.c gen_code.h id_attrs.c id_attrs.h id_use.c id_use.h instruction.c instruction.h label.c label.h lexer.c lexer.h lexer_output.c lexer_output.h lexical_address.c lexical_address.h parser.c parser.h reserved.c reserved.h scope.c scope.h scope_check.c scope_check.h symtab.c symtab.h token.c token.h unparser.c unparser.h utilities.c utilities.h

    // TO RUN
    // PART 1
    ./compiler hw4-vmtest1.pl0 > hw4-vmtest1.myvi
                        or
    make hw4-vmtest1.myvi

    // PART 2
    vm/vm hw4-vmtest1.myvi > hw4-vmtest1.myvo 2>&1
                        or
    make hw4-vmtest1.myvo

*/


code_seq procs;
//Also a global data struct for the starting points of the procedures.
static int procBodysSize = 1;

// Initialize the code generator
void gen_code_initialize()
{
    //bail_with_error("gen_code_initialize not implemented yet!");
}

// Generate code for the given AST
code_seq gen_code_program(AST *prog)
{
    // ⟨program⟩ ::= ⟨block⟩ in pl0, so just go on to gen_code_block.
    procs = code_seq_empty();
    code_seq ret = gen_code_block(prog);
    ret = code_seq_concat(procs, ret);
    ret = code_seq_concat(code_jmp(procBodysSize), ret);
    return(ret);
}

// generate code for blk
code_seq gen_code_block(AST *blk)
{
    code_seq ret = code_seq_singleton(code_inc(LINKS_SIZE));
    ret = code_seq_concat(ret, gen_code_constDecls(blk->data.program.cds));
    ret = code_seq_concat(ret, gen_code_varDecls(blk->data.program.vds));
    //Proc decls returns void because they are stored in a global data structure.
    gen_code_procDecls(blk->data.program.pds);
    ret = code_seq_concat(ret, gen_code_stmt(blk->data.program.stmt));
    ret = code_seq_add_to_end(ret, code_hlt());
    return ret;
}

// TODO
// generate code for the declarations in cds
code_seq gen_code_constDecls(AST_list cds)
{
    code_seq ret = code_seq_empty();
    while (!ast_list_is_empty(cds)) {
        ret = code_seq_concat(ret, gen_code_constDecl(ast_list_first(cds)));
        cds = ast_list_rest(cds);
    }
    return ret;
}

// TODO
// generate code for the const declaration cd
code_seq gen_code_constDecl(AST *cd)
{
    code_seq ret = code_seq_singleton(code_lit(cd->data.const_decl.num_val));
    return ret;
}

// generate code for the declarations in vds
code_seq gen_code_varDecls(AST_list vds)
{
    code_seq ret = code_seq_empty();
    while (!ast_list_is_empty(vds)) {
        ret = code_seq_concat(ret, gen_code_varDecl(ast_list_first(vds)));
        vds = ast_list_rest(vds);
    }
    return ret;
}

// generate code for the var declaration vd
code_seq gen_code_varDecl(AST *vd)
{
    return code_seq_singleton(code_inc(1));
}

// generate code for the declarations in pds
void gen_code_procDecls(AST_list pds)
{
    while (!ast_list_is_empty(pds)) {
        gen_code_procDecl(ast_list_first(pds));
        pds = ast_list_rest(pds);
    }
}

// ⟨proc-decl⟩ ::= procedure ⟨ident⟩ ; ⟨block⟩ ;
void gen_code_procDecl(AST *pd)
{
    //Add the procedure for the AST to the array/linked list.
    label_set(pd->data.proc_decl.lab, procBodysSize);
    procs = code_seq_concat(procs, gen_code_procBlock(pd->data.proc_decl.block));
}

// generate code for blk
code_seq gen_code_procBlock(AST *blk)
{
    code_seq ret = code_seq_empty();
    ret = code_seq_concat(ret, gen_code_constDecls(blk->data.program.cds));
    ret = code_seq_concat(ret, gen_code_varDecls(blk->data.program.vds));
    //Proc decls returns void because they are stored in a global data structure.
    gen_code_procDecls(blk->data.program.pds);
    ret = code_seq_concat(ret, gen_code_stmt(blk->data.program.stmt));
    ret = code_seq_add_to_end(ret, code_rtn());

    procBodysSize += code_seq_size(ret);

    return ret;
}

// generate code for the statement
code_seq gen_code_stmt(AST *stmt)
{
    switch (stmt->type_tag) {
        case assign_ast:
	        return gen_code_assignStmt(stmt);
	        break;
        case call_ast:
            return gen_code_callStmt(stmt);
            break;
        case begin_ast:
	        return gen_code_beginStmt(stmt);
	        break;
        case if_ast:
	        return gen_code_ifStmt(stmt);
	        break;
        case while_ast:
            return gen_code_whileStmt(stmt);
            break;
        case read_ast:
	        return gen_code_readStmt(stmt);
	        break;
        case write_ast:
	        return gen_code_writeStmt(stmt);
	        break;
        case skip_ast:
            return gen_code_skipStmt(stmt);
            break;
        default:
	        bail_with_error("Bad AST passed to gen_code_stmt!");
	        // The following should never execute
            return code_seq_empty();
    }
}

// generate code for the statement
code_seq gen_code_assignStmt(AST *stmt)
{
     /* design of code seq:
       [get fp for the variable on top of stack]
       [get value of expression on top of stack]
       STO([offset for the variable])
     */
    unsigned int outLevels = stmt->data.assign_stmt.ident->data.ident.idu->levelsOutward;
    code_seq ret = code_compute_fp(outLevels);
    ret = code_seq_concat(ret, gen_code_expr(stmt->data.assign_stmt.exp));
    unsigned int ofst = stmt->data.assign_stmt.ident->data.ident.idu->attrs->loc_offset;
    ret = code_seq_add_to_end(ret, code_sto(ofst));
    return ret;
}

// TODO
// generate code for the statement
code_seq gen_code_callStmt(AST *stmt)
{
    return(code_cal(stmt->data.call_stmt.ident->data.ident.idu->attrs->lab));
}

// TODO Correct begin stmt
// generate code for the statement
code_seq gen_code_beginStmt(AST *stmt)
{
    /* design of code_seq
        [save old BP on stack, PBP]
	[adjust the BP]
        [allocate variables declared]
	[concatenated code for each stmt]
	[if there are variables, pop them off the stack]
        [RBP]
     */

    // save the static link (surronging scope's BP) on stack
    code_seq ret = code_seq_empty();

    // add code for all the statements
    AST_list stmts = stmt->data.begin_stmt.stmts;
    while (!ast_list_is_empty(stmts)) {
        ret = code_seq_concat(ret, gen_code_stmt(ast_list_first(stmts)));
        stmts = ast_list_rest(stmts);
    }
    //if (num_vds > 0) {
	    // if there are variables, trim the variables
	    //ret = code_seq_add_to_end(ret, code_inc(- num_vds));
    //}

    return ret;
}

// generate code for the statement
code_seq gen_code_ifStmt(AST *stmt)
{
    /* design:
        [code for pushing the condition on top of stack]
	JPC 2
        JMP [around the body]
        [code for the body]
     */

    code_seq cond = gen_code_cond(stmt->data.if_stmt.cond);
    code_seq ret = code_seq_add_to_end(cond, code_jpc(2));
    code_seq thenbody = gen_code_stmt(stmt->data.if_stmt.thenstmt);
    code_seq elsebody = gen_code_stmt(stmt->data.if_stmt.elsestmt);
    ret = code_seq_add_to_end(ret, code_jmp(code_seq_size(thenbody) + 2));
    ret = code_seq_concat(ret, thenbody);
    ret = code_seq_add_to_end(ret, code_jmp(code_seq_size(elsebody) + 1));
    ret = code_seq_concat(ret, elsebody);

    return(ret);

    /* Code given by professor
    code_seq condc = gen_code_expr(stmt->data.if_stmt.exp);
    code_seq bodyc = gen_code_stmt(stmt->data.if_stmt.stmt);
    code_seq ret = code_seq_add_to_end(condc, code_jpc(2));
    ret = code_seq_add_to_end(ret, code_jmp(code_seq_size(bodyc)+1));
    ret = code_seq_concat(ret, bodyc);
    return ret;
    */
}

// TODO fix whilestmt
// generate code for the statement
code_seq gen_code_whileStmt(AST *stmt)
{
    // // Replace the following with your implementation
    // code_seq cond = gen_code_whileStmt(stmt->data.while_stmt.cond);
    // code_seq ret = code_seq_add_to_end(cond, code_jpc(2));
    // code_seq thenbody = gen_code_stmt(stmt->data.if_stmt.thenstmt);
    // code_seq elsebody = gen_code_stmt(stmt->data.if_stmt.elsestmt);
    // ret = code_seq_add_to_end(ret, code_jmp(code_seq_size(thenbody) + 2));
    // ret = code_seq_concat(ret, thenbody);
    // ret = code_seq_add_to_end(ret, code_jmp(code_seq_size(elsebody) + 1));
    // ret = code_seq_concat(ret, elsebody);

    code_seq cond = gen_code_cond(stmt->data.while_stmt.cond);
    code_seq ret = code_seq_add_to_end(cond, code_jpc(2));
    code_seq stmtbody = gen_code_stmt(stmt->data.while_stmt.stmt);
    ret = code_seq_add_to_end(ret, code_jmp(code_seq_size(stmtbody) + 2));
    ret = code_seq_concat(ret, stmtbody);
    ret = code_seq_add_to_end(ret, code_jmp( -(code_seq_size(cond)) ));


    return(ret);
}

// generate code for the statement
code_seq gen_code_readStmt(AST *stmt)
{
    /* design:
       [code to put the fp for the variable on top of stack]
       CHI
       STO [(variable offset)]
     */
    id_use *idu = stmt->data.read_stmt.ident->data.ident.idu;
    code_seq ret = code_compute_fp(idu->levelsOutward);
    ret = code_seq_add_to_end(ret, code_chi());
    ret = code_seq_add_to_end(ret, code_sto(idu->attrs->loc_offset));
    return ret;
}

// generate code for the statement
code_seq gen_code_writeStmt(AST *stmt)
{
    /* design:
       [code to put the exp's value on top of stack
       CHO
     */
    code_seq ret = gen_code_expr(stmt->data.write_stmt.exp);
    return code_seq_add_to_end(ret, code_cho());
}

// generate code for the statement
code_seq gen_code_skipStmt(AST *stmt)
{
    return code_nop();
}

// generate code for the condition
code_seq gen_code_cond(AST *cond)
{
    // if (DEBUG) {
    //     fprintf(stderr, "generating condition: %s\n", cond->type_tag == odd_cond_ast ? "ODD" : "EVEN");
    // }
    if(cond->type_tag == odd_cond_ast){
        return gen_code_odd_cond(cond);
    }else{
        return gen_code_bin_cond(cond);
    }
}

// TODO fix this idk what should happen here
// generate code for the condition
code_seq gen_code_odd_cond(AST *cond)
{
    code_seq ret = gen_code_expr(cond->data.odd_cond.exp);

    return ret;
}

// generate code for the condition
code_seq gen_code_bin_cond(AST *cond)
{
    code_seq ret = gen_code_expr(cond->data.bin_cond.leftexp);
    ret = code_seq_concat(ret, gen_code_expr(cond->data.bin_cond.rightexp));
    switch (cond->data.bin_cond.relop) {
        case eqop:
            return code_seq_add_to_end(ret, code_eql());
            break;
        case neqop:
            return code_seq_add_to_end(ret, code_neq());
            break;
        case ltop:
            return code_seq_add_to_end(ret, code_lss());
            break;
        case leqop:
            return code_seq_add_to_end(ret, code_leq());
            break;
        case gtop:
            return code_seq_add_to_end(ret, code_gtr());
            break;
        case geqop:
            return code_seq_add_to_end(ret, code_geq());
            break;
        default:
	        bail_with_error("gen_code_bin_expr passed AST with bad relop!");
	        // The following should never execute
	        return code_seq_empty();
    }

}

// TODO fix this for pl0.
// generate code for the expresion
code_seq gen_code_expr(AST *exp)
{
    switch (exp->type_tag) {
        case number_ast:
	        return gen_code_number_expr(exp);
	        break;
        case ident_ast:
	        return gen_code_ident_expr(exp);
	        break;
        case bin_expr_ast:
	        return gen_code_bin_expr(exp);
	        break;
        default:
	        bail_with_error("gen_code_expr passed bad AST!");
            // The following should never execute
            return code_seq_empty();
            break;
    }
}

// generate code for the expression (exp)
code_seq gen_code_bin_expr(AST *exp)
{
    /* design:
        [code to push left exp's value on top of stack]
	[code to push right exp's value on top of stack]
	[instruction that implements the operation op]
    */
    code_seq ret = gen_code_expr(exp->data.bin_expr.leftexp);
    ret = code_seq_concat(ret, gen_code_expr(exp->data.bin_expr.rightexp));
    switch (exp->data.bin_expr.arith_op) {
        case addop:
            return code_seq_add_to_end(ret, code_add());
            break;
        case subop:
            return code_seq_add_to_end(ret, code_sub());
            break;
        case divop:
            return code_seq_add_to_end(ret, code_div());
            break;
        case multop:
            return code_seq_add_to_end(ret, code_mul());
            break;
        default:
	        bail_with_error("gen_code_bin_expr passed AST with bad op!");
	        // The following should never execute
	        return code_seq_empty();
    }
}

// generate code for the ident expression (ident)
code_seq gen_code_ident_expr(AST *ident)
{
    /* design:
       [code to load fp for the variable]
       LOD [offset for the variable]
     */
    id_use *idu = ident->data.ident.idu;
    lexical_address *la = lexical_address_create(idu->levelsOutward, idu->attrs->loc_offset);
    return code_load_from_lexical_address(la);
}

// generate code for the number expression (num)
code_seq gen_code_number_expr(AST *num)
{
    // TODO fix compilation error
    return code_seq_singleton(code_lit((num->data.number.value)));
}
