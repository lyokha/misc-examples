/*
 * =============================================================================
 *
 *       Filename:  ngx_http_myutil.module.c
 *
 *    Description:  misc utilities
 *
 *        Version:  1.0
 *        Created:  23.03.2012 14:23:42
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Alexey Radkov (), 
 *        Company:  
 *
 * =============================================================================
 */

#include <ngx_core.h>
#include <ngx_http.h>


typedef struct
{
    ngx_array_t  varalias_data;
    ngx_array_t  first_not_empty_var_data;
}  ngx_http_myutil_loc_conf_t;


typedef struct
{
    ngx_int_t    self;
    ngx_int_t    index;
}  ngx_http_myutil_var_elem_t;


typedef struct
{
    ngx_str_t    key;
    ngx_int_t    index;
}  ngx_http_myutil_var_handle_t;


typedef struct
{
    ngx_array_t  data;
    ngx_int_t    index;
}  ngx_http_myutil_fne_elem_t;


static void *  ngx_http_myutil_create_loc_conf( ngx_conf_t *  cf );
static char *  ngx_http_myutil_merge_loc_conf( ngx_conf_t *  cf, void *  parent,
                                               void *  child );
static char *  ngx_http_myutil_var_alias( ngx_conf_t *  cf,
                                          ngx_command_t *  cmd, void *  conf );
static char *  ngx_http_myutil_first_not_empty( ngx_conf_t *  cf,
                                          ngx_command_t *  cmd, void *  conf );


static ngx_command_t  ngx_http_myutil_commands[] =
{
    {
        ngx_string( "myutil_var_alias" ),
        NGX_HTTP_LOC_CONF | NGX_HTTP_LIF_CONF | NGX_CONF_TAKE2,
        ngx_http_myutil_var_alias,
        NGX_HTTP_LOC_CONF_OFFSET,
        0,
        NULL
    },
    {
        ngx_string( "myutil_first_not_empty" ),
        NGX_HTTP_LOC_CONF | NGX_HTTP_LIF_CONF | NGX_CONF_2MORE,
        ngx_http_myutil_first_not_empty,
        NGX_HTTP_LOC_CONF_OFFSET,
        0,
        NULL
    },

    ngx_null_command
};


static ngx_http_module_t  ngx_http_myutil_module_ctx = {
    NULL,                                  /* preconfiguration */
    NULL,                                  /* postconfiguration */

    NULL,                                  /* create main configuration */
    NULL,                                  /* init main configuration */

    NULL,                                  /* create server configuration */
    NULL,                                  /* merge server configuration */

    ngx_http_myutil_create_loc_conf,       /* create location configuration */
    ngx_http_myutil_merge_loc_conf         /* merge location configuration */
};


ngx_module_t  ngx_http_myutil_module = {
    NGX_MODULE_V1,
    &ngx_http_myutil_module_ctx,           /* module context */
    ngx_http_myutil_commands,              /* module directives */
    NGX_HTTP_MODULE,                       /* module type */
    NULL,                                  /* init master */
    NULL,                                  /* init module */
    NULL,                                  /* init process */
    NULL,                                  /* init thread */
    NULL,                                  /* exit thread */
    NULL,                                  /* exit process */
    NULL,                                  /* exit master */
    NGX_MODULE_V1_PADDING
};


static void *  ngx_http_myutil_create_loc_conf( ngx_conf_t *  cf )
{
    ngx_http_myutil_loc_conf_t *  lcf = NULL;

    lcf = ngx_pcalloc( cf->pool, sizeof( ngx_http_myutil_loc_conf_t ) );

    if ( ! lcf )
        return NULL;

    if ( ngx_array_init( &lcf->varalias_data, cf->pool, 1,
                         sizeof( ngx_http_myutil_var_elem_t ) ) != NGX_OK )
        return NULL;

    if ( ngx_array_init( &lcf->first_not_empty_var_data, cf->pool, 1,
                         sizeof( ngx_http_myutil_fne_elem_t ) ) != NGX_OK )
        return NULL;

    return lcf;
}


static char *  ngx_http_myutil_merge_loc_conf( ngx_conf_t *  cf, void *  parent,
                                               void *  child )
{
    ngx_http_myutil_loc_conf_t *  prev = parent;
    ngx_http_myutil_loc_conf_t *  conf = child;

    ngx_uint_t                    i;

    /* all data in next sections will be imported from parent location in
     * addition to already collected data in child location */

    for ( i = 0; i < prev->varalias_data.nelts; ++i )
    {
        ngx_http_myutil_var_elem_t *  elem = NULL;

        elem = ngx_array_push( &conf->varalias_data );

        if ( ! elem )
            return NGX_CONF_ERROR;

        *elem = ( ( ngx_http_myutil_var_elem_t * )
                                    prev->varalias_data.elts )[ i ];
    }

    for ( i = 0; i < prev->first_not_empty_var_data.nelts; ++i )
    {
        ngx_http_myutil_fne_elem_t *  elem = NULL;

        elem = ngx_array_push( &conf->first_not_empty_var_data );

        if ( ! elem )
            return NGX_CONF_ERROR;

        *elem = ( ( ngx_http_myutil_fne_elem_t * )
                                    prev->first_not_empty_var_data.elts )[ i ];
    }

    return NGX_CONF_OK;
}


static ngx_int_t  ngx_http_myutil_get_var_alias_value( ngx_http_request_t *  r,
                            ngx_http_variable_value_t *  v, uintptr_t  data )
{
    ngx_http_myutil_loc_conf_t *  lcf     = NULL;
    ngx_http_variable_value_t *   var     = NULL;
    ngx_array_t *                 vaarray = NULL;
    ngx_http_myutil_var_elem_t *  vavar   = NULL;
    ngx_int_t *                   index   = NULL;

    ngx_uint_t                    i;

    if ( ! data )
        return NGX_ERROR;

    lcf = ngx_http_get_module_loc_conf( r, ngx_http_myutil_module );

    vaarray = &lcf->varalias_data;
    vavar   = vaarray->elts;
    index   = ( ngx_int_t * )data;

    for ( i = 0; i < vaarray->nelts ; ++i )
    {
        ngx_http_variable_value_t *  found = NULL;

        if ( *index != vavar[ i ].self )
            continue;

        found = ngx_http_get_indexed_variable( r, vavar[ i ].index );

        if ( found && found->len > 0 )
        {
            var = found;
            break;
        }
    }

    if ( ! var )
        return NGX_ERROR;

    v->len          = var->len;
    v->data         = var->data;
    v->valid        = 1;
    v->no_cacheable = 0;
    v->not_found    = 0;

    return NGX_OK;
}


static ngx_int_t  ngx_http_myutil_get_first_not_empty_value(
                        ngx_http_request_t *  r, ngx_http_variable_value_t *  v,
                        uintptr_t  data )
{
    ngx_http_myutil_loc_conf_t *    lcf         = NULL;
    ngx_http_variable_value_t *     var         = NULL;
    ngx_array_t *                   fne_data    = NULL;
    ngx_http_myutil_fne_elem_t *    fne_elts    = NULL;
    ngx_http_myutil_fne_elem_t *    fne_elem    = NULL;
    ngx_array_t *                   exprarray   = NULL;
    ngx_http_myutil_var_handle_t *  expr        = NULL;
    ngx_int_t *                     index       = NULL;
    ngx_uint_t                      found_index = ( ngx_uint_t )NGX_ERROR;

    ngx_uint_t                      i;

    if ( ! data )
        return NGX_ERROR;

    lcf = ngx_http_get_module_loc_conf( r, ngx_http_myutil_module );

    fne_data = &lcf->first_not_empty_var_data;
    fne_elts = fne_data->elts;
    index    = ( ngx_int_t * )data;

    for ( i = 0; i < fne_data->nelts; ++i )
    {
        if ( *index != fne_elts[ i ].index )
            continue;

        found_index = i;
        break;
    }

    if ( found_index == ( ngx_uint_t )NGX_ERROR )
        return NGX_ERROR;

    fne_elem = &fne_elts[ found_index ];

    exprarray = &fne_elem->data;
    expr      = exprarray->elts;

    for ( i = 0; i < exprarray->nelts; ++i )
    {
        ngx_http_variable_value_t *  found = NULL;

        if ( expr[ i ].index == NGX_ERROR )
        {
            v->len          = expr[ i ].key.len;
            v->data         = expr[ i ].key.data;
            v->valid        = 1;
            v->no_cacheable = 0;
            v->not_found    = 0;

            return NGX_OK;
        }

        found = ngx_http_get_indexed_variable( r, expr[ i ].index );

        if ( found && found->len > 0 )
        {
            var = found;
            break;
        }
    }

    if ( ! var )
        return NGX_ERROR;

    v->len          = var->len;
    v->data         = var->data;
    v->valid        = 1;
    v->no_cacheable = 0;
    v->not_found    = 0;

    return NGX_OK;
}


static char *  ngx_http_myutil_var_alias( ngx_conf_t *  cf,
                                          ngx_command_t *  cmd, void *  conf )
{
    ngx_str_t *                   value     = cf->args->elts;
    ngx_http_variable_t *         v         = NULL;
    ngx_http_myutil_loc_conf_t *  lcf       = conf;
    ngx_int_t                     index     = NGX_ERROR;
    ngx_int_t                     v_idx     = NGX_ERROR;
    ngx_http_myutil_var_elem_t *  res       = NULL;
    ngx_uint_t *                  v_idx_ptr = NULL;

    ngx_uint_t                    i;

    for ( i = 1; i < 3; ++i )
    {
        if ( value[ i ].data[ 0 ] != '$' )
        {
            ngx_conf_log_error( NGX_LOG_EMERG, cf, 0,
                                "%V: invalid variable name '%V'",
                                &cmd->name, &value[ i ] );
            return NGX_CONF_ERROR;
        }

        value[ i ].len--;
        value[ i ].data++;
    }

    index = ngx_http_get_variable_index( cf, &value[ 2 ] );

    if ( index == NGX_ERROR )
        return NGX_CONF_ERROR;

    v = ngx_http_add_variable( cf, &value[ 1 ], NGX_HTTP_VAR_CHANGEABLE );

    if ( v == NULL )
        return NGX_CONF_ERROR;

    v_idx = ngx_http_get_variable_index( cf, &value[ 1 ] );

    if ( v_idx == NGX_ERROR )
        return NGX_CONF_ERROR;

    res = ngx_array_push( &lcf->varalias_data );

    if ( ! res )
        return NGX_CONF_ERROR;

    res->self  = v_idx;
    res->index = index;

    v_idx_ptr = ngx_pnalloc( cf->pool, sizeof( ngx_uint_t ) );

    if ( ! v_idx_ptr )
        return NGX_CONF_ERROR;

    *v_idx_ptr     = v_idx;
    v->data        = ( uintptr_t )v_idx_ptr;
    v->get_handler = ngx_http_myutil_get_var_alias_value;

    return NGX_CONF_OK;
}


static char *  ngx_http_myutil_first_not_empty( ngx_conf_t *  cf,
                                            ngx_command_t *  cmd, void *  conf )
{
    ngx_str_t *                   value     = cf->args->elts;
    ngx_http_variable_t *         v         = NULL;
    ngx_http_myutil_loc_conf_t *  lcf       = conf;
    ngx_int_t                     v_idx     = NGX_ERROR;
    ngx_uint_t *                  v_idx_ptr = NULL;
    ngx_http_myutil_fne_elem_t *  resvar    = NULL;

    ngx_uint_t                    i;

    if ( value[ 1 ].data[ 0 ] != '$' )
    {
        ngx_conf_log_error( NGX_LOG_EMERG, cf, 0,
                            "invalid variable name '%V'", &value[ 1 ] );
        return NGX_CONF_ERROR;
    }

    value[ 1 ].len--;
    value[ 1 ].data++;

    /* It must never occur due to command arguments number restriction */
    if ( cf->args->nelts < 3 )
        return NGX_CONF_ERROR;

    resvar = ngx_array_push( &lcf->first_not_empty_var_data );

    if ( ! resvar )
        return NGX_CONF_ERROR;

    if ( ngx_array_init( &resvar->data, cf->pool, cf->args->nelts - 2,
                         sizeof( ngx_http_myutil_var_handle_t ) ) != NGX_OK )
        return NGX_CONF_ERROR;

    for ( i = 2; i < cf->args->nelts; ++i )
    {
        ngx_http_myutil_var_handle_t *  res   = NULL;
        ngx_int_t                       index = NGX_ERROR;

        ngx_uint_t  isvar = value[ i ].data[ 0 ] == '$' ? 1 : 0;

        res = ngx_array_push( &resvar->data );

        if ( ! res )
            return NGX_CONF_ERROR;

        if ( ! isvar )
        {
            res->index = NGX_ERROR;
            res->key   = value[ i ];
            continue;
        }

        value[ i ].len--;
        value[ i ].data++;

        index = ngx_http_get_variable_index( cf, &value[ i ] );

        if ( index == NGX_ERROR )
            return NGX_CONF_ERROR;

        res->index = index;
        res->key   = value[ i ];
    }

    v = ngx_http_add_variable( cf, &value[ 1 ], NGX_HTTP_VAR_CHANGEABLE );

    if ( v == NULL )
        return NGX_CONF_ERROR;

    v_idx = ngx_http_get_variable_index( cf, &value[ 1 ] );

    if ( v_idx == NGX_ERROR )
        return NGX_CONF_ERROR;

    v_idx_ptr = ngx_pnalloc( cf->pool, sizeof( ngx_uint_t ) );

    if ( ! v_idx_ptr )
        return NGX_CONF_ERROR;

    resvar->index  = v_idx;

    *v_idx_ptr     = v_idx;
    v->data        = ( uintptr_t )v_idx_ptr;
    v->get_handler = ngx_http_myutil_get_first_not_empty_value;

    return NGX_CONF_OK;
}

