#include "hw7.h"

bst_sf* insert_bst_sf(matrix_sf *mat, bst_sf *root) {
    if (root==NULL){
        root = malloc(sizeof(bst_sf));
        root->mat=mat;
        root->left_child=NULL;
        root->right_child=NULL;
        return root;
    }
    else if (mat->name > root->mat->name)
    {
        root->right_child = insert_bst_sf(mat, root->right_child);
    }
    else
    {
        root->left_child = insert_bst_sf(mat, root->left_child);
    }
    
    return root;
}

matrix_sf* find_bst_sf(char name, bst_sf *root) {
    if (root == NULL) {
        return NULL;
    }
    if (root->mat->name == name) {
        return root->mat;
    }
    if (name < root->mat->name) {
        return find_bst_sf(name, root->left_child);
    } else {
        return find_bst_sf(name, root->right_child);
    }
}

void free_bst_sf(bst_sf *root) {
    if (root == NULL) {
        return;
    }
    free_bst_sf(root->left_child);
    free_bst_sf(root->right_child);
    free(root->mat);
    free(root);
}

matrix_sf* add_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    matrix_sf *result = copy_matrix(mat1->num_rows,mat1->num_cols,mat1->values);
    for(unsigned int i = 0; i < result->num_rows;i++){
    for(unsigned int j = 0; j < result->num_cols;j++){
        result->values[i*result->num_cols+j] += mat2->values[i*mat2->num_cols+j];
    }
    }
    return result;
}

matrix_sf* mult_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
   matrix_sf *result = malloc(sizeof(matrix_sf)+mat1->num_rows*mat2->num_cols*sizeof(int));
    result->name = '?';
    result->num_rows = mat1->num_rows;
    result->num_cols = mat2->num_cols;
    for(unsigned int i = 0; i < mat1->num_rows;i++){

        for(unsigned int x = 0; x < mat2->num_cols;x++){
            int total=0;
        for(unsigned int y = 0; y < mat2->num_rows;y++){
            total += mat1->values[i*mat1->num_cols+y]*mat2->values[y*mat2->num_cols+x];
        }
        result->values[i*result->num_cols+x]=total;
    }
    }
    return result;
    }
   


matrix_sf* transpose_mat_sf(const matrix_sf *mat) {
    matrix_sf *result = malloc(sizeof(matrix_sf) + mat->num_cols * mat->num_rows * sizeof(int));
    result->name = '?';
    result->num_rows = mat->num_cols;
    result->num_cols = mat->num_rows;
    for(unsigned int i = 0; i < mat->num_rows; i++){
        for(unsigned int j = 0; j < mat->num_cols; j++){
            result->values[j * result->num_cols + i] = mat->values[i * mat->num_cols + j];
        }
    }
    return result;
}

matrix_sf* create_matrix_sf(char name, const char *expr) {
    char *p = (char*)expr;
    char *endptr;
    int value[1000];
    int count = 0;

    while (*p != '\0') {
        long temp = strtol(p, &endptr, 10);
        if (endptr == p) {
            p++;
            continue;
        }
        value[count++] = (int)temp;
        p = endptr;
    }

    unsigned int num_rows = value[0];
    unsigned int num_cols = value[1];
    matrix_sf *result = malloc(sizeof(matrix_sf) + num_rows * num_cols * sizeof(int));
    result->name = name;
    result->num_rows = num_rows;
    result->num_cols = num_cols;

    for (unsigned int i = 0; i < num_rows * num_cols; i++) {
        result->values[i] = value[i + 2];
    }
    return result;
}


char* infix2postfix_sf(char *infix) {
    int len = strlen(infix);
    char *postfix = malloc(len + 1);
    char *stack = malloc(len);
    int top = -1;
    int out_idx = 0;

    for (int i = 0; i < len; i++) {
        char c = infix[i];
        if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
            continue;
        }
        if (c >= 'A' && c <= 'Z') {
            postfix[out_idx++] = c;
        }
        else if (c == '\'') {
            postfix[out_idx++] = c;
        }
        else if (c == '(') {
            stack[++top] = c;
        }
        else if (c == ')') {
            while (top >= 0 && stack[top] != '(') {
                postfix[out_idx++] = stack[top--];
            }
            if (top >= 0) top--;
        }
        else if (c == '+' || c == '*') {
            int precedence_c = (c == '*') ? 2 : 1;
            while (top >= 0 && stack[top] != '(') {
                int precedence_top = (stack[top] == '*') ? 2 : 1;
                if (precedence_top >= precedence_c) {
                    postfix[out_idx++] = stack[top--];
                } else {
                    break;
                }
            }
            stack[++top] = c;
        }
    }
    while (top >= 0) {
        postfix[out_idx++] = stack[top--];
    }
    postfix[out_idx] = '\0';
    free(stack);
    return postfix;
}

matrix_sf* evaluate_expr_sf(char name, char *expr, bst_sf *root) {
    char *postfix = infix2postfix_sf(expr);
    int len = strlen(postfix);
    matrix_sf **stack = malloc(len * sizeof(matrix_sf*));
    int top = -1;

    for (int i = 0; i < len; i++) {
        char c = postfix[i];
        if (c >= 'A' && c <= 'Z') {
            matrix_sf *mat = find_bst_sf(c, root);
            stack[++top] = mat;
        }
        else if (c == '\'') {
            matrix_sf *mat = stack[top];
            stack[top] = transpose_mat_sf(mat);
            if (mat->name == '?') {
                free(mat);
            }
        }
        else if (c == '+') {
            matrix_sf *mat2 = stack[top--];
            matrix_sf *mat1 = stack[top--];
            matrix_sf *result = add_mats_sf(mat1, mat2);
            if (mat1->name == '?') {
                free(mat1);
            }
            if (mat2->name == '?') {
                free(mat2);
            }
            stack[++top] = result;
        }
        else if (c == '*') {
            matrix_sf *mat2 = stack[top--];
            matrix_sf *mat1 = stack[top--];
            matrix_sf *result = mult_mats_sf(mat1, mat2);
            if (mat1->name == '?') {
                free(mat1);
            }
            if (mat2->name == '?') {
                free(mat2);
            }
            stack[++top] = result;
        }
    }

    matrix_sf *result = stack[top];
    result->name = name;
    free(postfix);
    free(stack);
    return result;
}

void free_bst_except_sf(bst_sf *root, matrix_sf *keep) {
    if (root == NULL) {
        return;
    }
    free_bst_except_sf(root->left_child, keep);
    free_bst_except_sf(root->right_child, keep);
    if (root->mat != keep) {
        free(root->mat);
    }
    free(root);
}

matrix_sf *execute_script_sf(char *filename) {
    FILE *file = fopen(filename, "r");
    if (file == NULL) {
        return NULL;
    }

    bst_sf *root = NULL;
    matrix_sf *last_matrix = NULL;
    char line[1000];

    while (fgets(line, 1000, file) != NULL) {
        if (strlen(line) <= 1) {
            continue;
        }

        char name;
        char *equals = strchr(line, '=');
        if (equals == NULL) {
            continue;
        }

        name = line[0];
        char *expr = equals + 1;
        while (*expr == ' ') expr++;

        if (*expr >= '0' && *expr <= '9') {
            last_matrix = create_matrix_sf(name, expr);
        } else {
            last_matrix = evaluate_expr_sf(name, expr, root);
        }

        root = insert_bst_sf(last_matrix, root);
    }

    fclose(file);
    free_bst_except_sf(root, last_matrix);
    return last_matrix;
}

// This is a utility function used during testing. Feel free to adapt the code to implement some of
// the assignment. Feel equally free to ignore it.
matrix_sf *copy_matrix(unsigned int num_rows, unsigned int num_cols, int values[]) {
    matrix_sf *m = malloc(sizeof(matrix_sf)+num_rows*num_cols*sizeof(int));
    m->name = '?';
    m->num_rows = num_rows;
    m->num_cols = num_cols;
    memcpy(m->values, values, num_rows*num_cols*sizeof(int));
    return m;
}

// Don't touch this function. It's used by the testing framework.
// It's been left here in case it helps you debug and test your code.
void print_matrix_sf(matrix_sf *mat) {
    assert(mat != NULL);
    assert(mat->num_rows <= 1000);
    assert(mat->num_cols <= 1000);
    printf("%d %d ", mat->num_rows, mat->num_cols);
    for (unsigned int i = 0; i < mat->num_rows*mat->num_cols; i++) {
        printf("%d", mat->values[i]);
        if (i < mat->num_rows*mat->num_cols-1)
            printf(" ");
    }
    printf("\n");
}
