/* $Id: gen_code_stubs.c,v 1.7 2023/03/27 16:39:12 leavens Exp $ */
#include "utilities.h"
#include "gen_code.h"

// Initialize the code generator
void gen_code_initialize()
{
    bail_with_error("gen_code_initialize not implemented yet!");
}

// Generate code for the given AST
code_seq gen_code_program(AST *prog)
{
    code_seq ret = code_seq_singleton(code_inc(BLOCK_LINKS_SIZE));
    ret = code_seq_concat(ret, gen_code_varDecls(prog->data.program.vds));
    ret = code_seq_concat(ret, gen_code_stmt(prog->data.program.stmt));
    ret = code_seq_add_to_end(ret, code_hlt());
    return ret;
}

// generate code for blk
code_seq gen_code_block(AST *blk)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_block not implemented yet!");
    return code_seq_empty();
}

// generate code for the declarations in cds
code_seq gen_code_constDecls(AST_list cds)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_constDecls not implemented yet!");
    return code_seq_empty();
}

// generate code for the const declaration cd
code_seq gen_code_constDecl(AST *cd)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_constDecl not implemented yet!");
    return code_seq_empty();
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
    // Replace the following with your implementation
    bail_with_error("gen_code_procDecls not implemented yet!");
}

// generate code for the procedure declaration pd
void gen_code_procDecl(AST *pd)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_procDecl not implemented yet!");
}

// generate code for the statement
code_seq gen_code_stmt(AST *stmt)
{
    switch (stmt->type_tag) {
        case assign_ast:
	        return gen_code_assignStmt(stmt);
	        break;
        case begin_ast:
	        return gen_code_beginStmt(stmt);
	        break;
        case if_ast:
	        return gen_code_ifStmt(stmt);
	        break;
        case read_ast:
	        return gen_code_readStmt(stmt);
	        break;
        case write_ast:
	        return gen_code_writeStmt(stmt);
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

// generate code for the statement
code_seq gen_code_callStmt(AST *stmt)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_callStmt not implemented yet!");
    return code_seq_empty();
}

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
    code_seq ret = code_seq_singleton(code_pbp());
    // set the BP to SP-1
    ret = code_seq_add_to_end(ret, code_psp());
    ret = code_seq_add_to_end(ret, code_lit(1));
    ret = code_seq_add_to_end(ret, code_sub());
    ret = code_seq_add_to_end(ret, code_rbp());
    // allocate any declared variables
    AST_list vds = stmt->data.begin_stmt.vds;
    int num_vds = ast_list_size(vds);
    ret = code_seq_concat(ret, gen_code_varDecls(vds));
    // add code for all the statements
    AST_list stmts = stmt->data.begin_stmt.stmts;
    while (!ast_list_is_empty(stmts)) {
        ret = code_seq_concat(ret, gen_code_stmt(ast_list_first(stmts)));
        stmts = ast_list_rest(stmts);
    }
    if (num_vds > 0) {
	    // if there are variables, trim the variables
	    ret = code_seq_add_to_end(ret, code_inc(- num_vds));
    }

    // restore the old BP
    ret = code_seq_add_to_end(ret, code_rbp());
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
    code_seq condc = gen_code_expr(stmt->data.if_stmt.exp);
    code_seq bodyc = gen_code_stmt(stmt->data.if_stmt.stmt);
    code_seq ret = code_seq_add_to_end(condc, code_jpc(2));
    ret = code_seq_add_to_end(ret, code_jmp(code_seq_size(bodyc)+1));
    ret = code_seq_concat(ret, bodyc);
    return ret;
}

// generate code for the statement
code_seq gen_code_whileStmt(AST *stmt)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_whileStmt not implemented yet!");
    return code_seq_empty();
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
    // Replace the following with your implementation
    bail_with_error("gen_code_skipStmt not implemented yet!");
    return code_seq_empty();
}

// generate code for the condition
code_seq gen_code_cond(AST *cond)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_cond not implemented yet!");
    return code_seq_empty();
}

// generate code for the condition
code_seq gen_code_odd_cond(AST *cond)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_odd_cond not implemented yet!");
    return code_seq_empty();
}

// generate code for the condition
code_seq gen_code_bin_cond(AST *cond)
{
    // Replace the following with your implementation
    bail_with_error("gen_code_bin_cond not implemented yet!");
    return code_seq_empty();
}

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
        case not_expr_ast:
	        return gen_code_not_expr(exp);
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
    switch (exp->data.bin_expr.op) {
        case eqeqop:
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
        case plusop:
	        return code_seq_add_to_end(ret, code_add());
	        break;
        case minusop:
	        return code_seq_add_to_end(ret, code_sub());
	        break;
        case multop:
	        return code_seq_add_to_end(ret, code_mul());
	        break;
        case divop:
	        return code_seq_add_to_end(ret, code_div());
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
    return code_seq_singleton(code_lit(word2float(num->data.number.value)));
}
