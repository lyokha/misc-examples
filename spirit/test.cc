/*
 * =============================================================================
 *
 *       Filename:  test.cc
 *
 *    Description:  
 *
 *        Version:  1.0
 *        Created:  16.06.2010 16:20:20
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Alexey Radkov (), 
 *        Company:  
 *
 * =============================================================================
 */

#include <iostream>
#include <sstream>
#include <vector>
#include <string>
#include <cmath>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_function.hpp>
#include <boost/spirit/include/phoenix_fusion.hpp>
#include <boost/variant/recursive_variant.hpp>
#include <boost/format.hpp>


namespace  CexmcAST
{
    using boost::variant;
    using boost::recursive_wrapper;

    enum  OperatorType
    {
        Uninitialized,
        Top,
        UMinus,
        Not,
        Mult,
        Div,
        Plus,
        Minus,
        Less,
        LessEq,
        More,
        MoreEq,
        Eq,
        NotEq,
        And,
        Or
    };


    struct  Operator
    {
        Operator( OperatorType  type = Uninitialized, int  priority = 0,
                  bool  hasRLAssoc = false ) :
            type( type ), priority( priority ), hasRLAssoc( hasRLAssoc )
        {}

        OperatorType  type;

        int           priority;

        bool          hasRLAssoc;
    };


    struct  Variable
    {
        Variable() : index1 ( 0 ), index2( 0 ), addr( ( const int * ) NULL )
        {}

        std::string                             name;

        int                                     index1;

        int                                     index2;

        variant< const int *, const double * >  addr;
    };


    struct  Subtree;

    typedef std::string                    Function;

    typedef variant< int, double >         Constant;

    typedef variant< Variable, Constant >  Leaf;

    typedef recursive_wrapper< Subtree >   Tree;

    typedef variant< Tree, Leaf >          Node;

    typedef variant< Operator, Function >  NodeType;


    struct  Subtree
    {
        Subtree() : type ( Operator( Uninitialized ) )
        {}

        void  Print( int  level = 0 ) const;

        void  PrintLeaf( const Leaf *  leaf, int  level = 0 ) const;

        std::vector< Node >  children;

        NodeType             type;

        static const int     printIndent = 4;
    };


    void Subtree::Print( int  level ) const
    {
        static const std::string  opId[] =
            { "UNINITIALIZED", "TOP", "u -", "!", "*", "/", "+", "-", "<", "<=",
              ">", ">=", "=", "!=", "&", "|" };

        std::stringstream         value;
        const Operator *          op( boost::get< Operator >( &type ) );

        if ( op )
        {
            value << "-op- " << opId[ op->type ];
        }
        else
        {
            const Function *  fun( boost::get< Function >( &type ) );
            value << "-fun- " << *fun;
        }

        std::stringstream  format;
        format << "%|" << level * printIndent << "t|";
        std::cout << boost::format( format.str() ) << value.str() << std::endl;

        for ( std::vector< Node >::const_iterator  k( children.begin() );
              k != children.end(); ++k )
        {
            const Subtree *  subtree( boost::get< Subtree >( &*k ) );

            if ( subtree )
            {
                subtree->Print( level + 1 );
            }
            else
            {
                const Leaf *  leaf( boost::get< Leaf >( &*k ) );
                if ( leaf )
                    PrintLeaf( leaf, level + 1 );
            }
        }
    }


    void Subtree::PrintLeaf( const Leaf *  leaf, int  level ) const
    {
        const Variable *   variable( NULL );
        std::stringstream  value;

        if ( ( variable = boost::get< Variable >( leaf ) ) )
        {
            value << variable->name;
            if ( variable->index1 > 0 )
            {
                value << "[" << variable->index1;
                if ( variable->index2 > 0 )
                    value << "," << variable->index2;
                value << "]";
            }
        }
        else
        {
            const Constant *  constant( boost::get< Constant >( leaf ) );
            const int *       intConstant( boost::get< int >( constant ) );
            const double *    doubleConstant( boost::get< double >(
                                                                constant ) );

            value << ( intConstant ? *intConstant : *doubleConstant );
        }

        std::stringstream  format;
        format << "%|" << level * printIndent << "t|";
        std::cout << boost::format( format.str() ) << value.str() << std::endl;
    }


    class  BasicEval
    {
        protected:
            typedef variant< int, double >  ScalarValueType;

        protected:
            virtual ~BasicEval();

        public:
            bool  operator()( const Subtree &  ast ) const;

        protected:
            ScalarValueType          GetScalarValue( const Node &  node ) const;

            virtual ScalarValueType  GetFunScalarValue( const Subtree &  ast )
                                                                        const;

            virtual ScalarValueType  GetVarScalarValue( const Variable &  var )
                                                                        const;

            ScalarValueType          GetBasicFunScalarValue(
                                        const Subtree &  ast, bool &  result )
                                                                        const;
    };


    BasicEval::~BasicEval()
    {
    }


    bool  BasicEval::operator()( const Subtree &  ast ) const
    {
        ScalarValueType  retval( GetScalarValue( ast ) );
        int *            intRetval( NULL );
        double *         doubleRetval( NULL );

        intRetval = boost::get< int >( &retval );

        if ( ! intRetval )
            doubleRetval = boost::get< double >( &retval );

        if ( doubleRetval )
            std::cout << "Evaluated to double " << *doubleRetval << std::endl;
        else
            std::cout << "Evaluated to int " << *intRetval << std::endl;

        return doubleRetval ? bool( *doubleRetval ) : bool( *intRetval );
    }


    BasicEval::ScalarValueType  BasicEval::GetScalarValue(
                                                    const Node &  node ) const
    {
        const Subtree *  ast( boost::get< Subtree >( &node ) );

        if ( ast )
        {
            const Operator *  op( boost::get< Operator >( &ast->type ) );
            if ( op )
            {
                ScalarValueType           left( 0 );
                ScalarValueType           right( 0 );
                int *                     intLeft( NULL );
                double *                  doubleLeft( NULL );
                int *                     intRight( NULL );
                double *                  doubleRight( NULL );
                bool                      isDoubleRetval( false );

                if ( ast->children.size() > 0 )
                {
                    left = GetScalarValue( ast->children[ 0 ] );
                    intLeft = boost::get< int >( &left );
                    if ( ! intLeft )
                        doubleLeft = boost::get< double >( &left );
                }

                switch ( op->type )
                {
                case And :
                case Or :
                    break;
                default :
                    if ( ast->children.size() > 1 )
                    {
                        right = GetScalarValue( ast->children[ 1 ] );
                        intRight = boost::get< int >( &right );
                        if ( ! intRight )
                            doubleRight = boost::get< double >( &right );
                    }
                    isDoubleRetval = doubleLeft || doubleRight;
                    break;
                }

                switch ( op->type )
                {
                case Uninitialized :
                    return 1;
                case Top :
                    return left;
                case UMinus :
                    if ( doubleLeft )
                        return - *doubleLeft;
                    else
                        return - *intLeft;
                case Not :
                    if ( doubleLeft )
                        return ! *doubleLeft;
                    else
                        return ! *intLeft;
                case Mult :
                    if ( isDoubleRetval )
                        return ( doubleLeft ? *doubleLeft : *intLeft ) *
                               ( doubleRight ? *doubleRight : *intRight );
                    else
                        return *intLeft * *intRight;
                case Div :
                    if ( isDoubleRetval )
                        return ( doubleLeft ? *doubleLeft : *intLeft ) /
                               ( doubleRight ? *doubleRight : *intRight );
                    else
                        return *intLeft / *intRight;
                case Plus :
                    if ( isDoubleRetval )
                        return ( doubleLeft ? *doubleLeft : *intLeft ) +
                               ( doubleRight ? *doubleRight : *intRight );
                    else
                        return *intLeft + *intRight;
                case Minus :
                    if ( isDoubleRetval )
                        return ( doubleLeft ? *doubleLeft : *intLeft ) -
                               ( doubleRight ? *doubleRight : *intRight );
                    else
                        return *intLeft - *intRight;
                case Less :
                    if ( isDoubleRetval )
                        return ( doubleLeft ? *doubleLeft : *intLeft ) <
                               ( doubleRight ? *doubleRight : *intRight );
                    else
                        return *intLeft < *intRight;
                case LessEq :
                    if ( isDoubleRetval )
                        return ( doubleLeft ? *doubleLeft : *intLeft ) <=
                               ( doubleRight ? *doubleRight : *intRight );
                    else
                        return *intLeft <= *intRight;
                case More :
                    if ( isDoubleRetval )
                        return ( doubleLeft ? *doubleLeft : *intLeft ) >
                               ( doubleRight ? *doubleRight : *intRight );
                    else
                        return *intLeft > *intRight;
                case MoreEq :
                    if ( isDoubleRetval )
                        return ( doubleLeft ? *doubleLeft : *intLeft ) >=
                               ( doubleRight ? *doubleRight : *intRight );
                    else
                        return *intLeft >= *intRight;
                case Eq :
                    if ( isDoubleRetval )
                        return ( doubleLeft ? *doubleLeft : *intLeft ) ==
                               ( doubleRight ? *doubleRight : *intRight );
                    else
                        return *intLeft == *intRight;
                case NotEq :
                    if ( isDoubleRetval )
                        return ( doubleLeft ? *doubleLeft : *intLeft ) !=
                               ( doubleRight ? *doubleRight : *intRight );
                    else
                        return *intLeft != *intRight;
                case And :
                    if ( doubleLeft )
                    {
                        if ( ! *doubleLeft )
                            return 0;
                    }
                    else
                    {
                        if ( ! *intLeft )
                            return 0;
                    }
                    right = GetScalarValue( ast->children[ 1 ] );
                    intRight = boost::get< int >( &right );
                    if ( ! intRight )
                        doubleRight = boost::get< double >( &right );
                    if ( doubleRight )
                    {
                        if ( *doubleRight )
                            return 1;
                    }
                    else
                    {
                        if ( *intRight )
                            return 1;
                    }
                    return 0;
                case Or :
                    if ( doubleLeft )
                    {
                        if ( *doubleLeft )
                            return 1;
                    }
                    else
                    {
                        if ( *intLeft )
                            return 1;
                    }
                    right = GetScalarValue( ast->children[ 1 ] );
                    intRight = boost::get< int >( &right );
                    if ( ! intRight )
                        doubleRight = boost::get< double >( &right );
                    if ( doubleRight )
                    {
                        if ( *doubleRight )
                            return 1;
                    }
                    else
                    {
                        if ( *intRight )
                            return 1;
                    }
                    return 0;
                default :
                    return 0;
                }
            }
            else
            {
                return GetFunScalarValue( *ast );
            }
        }
        else
        {
            const Leaf &      leaf( boost::get< Leaf >( node ) );
            const Constant *  constant( boost::get< Constant >( &leaf ) );

            if ( constant )
            {
                return *constant;
            }
            else
            {
                const Variable &  variable( boost::get< Variable >( leaf ) );
                return GetVarScalarValue( variable );
            }
        }

        return 0;
    }


    BasicEval::ScalarValueType  BasicEval::GetFunScalarValue(
                                                    const Subtree &  ast ) const
    {
        bool             evalResult( false );
        ScalarValueType  result( GetBasicFunScalarValue( ast, evalResult ) );

        if ( evalResult )
            return result;

        return 0;
    }


    BasicEval::ScalarValueType  BasicEval::GetVarScalarValue(
                                                const Variable &  var ) const
    {
        return 0;
    }


    BasicEval::ScalarValueType  BasicEval::GetBasicFunScalarValue(
                                    const Subtree &  ast, bool &  result ) const
    {
        const Function &  fun( boost::get< Function >( ast.type ) );

        result = true;

        ScalarValueType   arg( GetScalarValue( ast.children[ 0 ] ) );
        int *             intArg( NULL );
        double *          doubleArg( NULL );

        intArg = boost::get< int >( &arg );
        if ( ! intArg )
            doubleArg = boost::get< double >( &arg );

        if ( fun == "Sqr" )
        {
            if ( doubleArg )
                return *doubleArg * *doubleArg;
            else
                return *intArg * *intArg;
        }
        if ( fun == "Sqrt" )
        {
            if ( doubleArg )
                return std::sqrt( *doubleArg );
            else
                return std::sqrt( *intArg );
        }

        result = false;

        return 0;
    }
}


class  CexmcCustomFilterEval : public CexmcAST::BasicEval
{
    private:
        virtual ScalarValueType  GetFunScalarValue(
                                        const CexmcAST::Subtree &  ast ) const;

        virtual ScalarValueType  GetVarScalarValue(
                                        const CexmcAST::Variable &  var ) const;
};


CexmcAST::BasicEval::ScalarValueType  CexmcCustomFilterEval::GetFunScalarValue(
                                        const CexmcAST::Subtree &  ast ) const
{
    bool             evalResult( false );
    ScalarValueType  result( GetBasicFunScalarValue( ast, evalResult ) );

    if ( evalResult )
        return result;

    return 1;
}


CexmcAST::BasicEval::ScalarValueType  CexmcCustomFilterEval::GetVarScalarValue(
                                        const CexmcAST::Variable &  var ) const
{
    return 1;
}


namespace  CexmcCustomFilter
{
    using namespace boost::spirit;
    using namespace boost::spirit::qi;
    using namespace boost::spirit::ascii;
    using namespace boost::phoenix;
    using namespace CexmcAST;
    using boost::spirit::qi::rule;
    using boost::spirit::ascii::space;
    using boost::spirit::ascii::space_type;
    using boost::spirit::ascii::alpha;
    using boost::spirit::ascii::alnum;
    using boost::spirit::unused_type;


    enum  Action
    {
        KeepTPT,
        KeepEDT,
        DeleteTPT,
        DeleteEDT
    };


    struct  ParseResult
    {
        ParseResult() : action( KeepTPT )
        {}

        void  Initialize( void )
        {
            action = KeepTPT;
            expression.children.clear();
            expression.type = Operator( Uninitialized );
        }

        Action   action;

        Subtree  expression;
    };


    struct  Compiler
    {
        template < typename  A, typename  B = unused_type,
                   typename  C = unused_type, typename  D = unused_type >
        struct  result { typedef void  type; };

        void  operator()( ParseResult &  parseResult, Action  value ) const;

        void  operator()( ParseResult &  parseResult, Subtree &  value ) const;

        void  operator()( Subtree &  ast, Node &  node ) const;

        void  operator()( Node &  self, Node &  left, Node &  right,
                          Operator  value ) const;

        void  operator()( Node &  self, Node &  child, Operator  value ) const;

        void  operator()( Node &  self, Node &  primary ) const;

        void  operator()( Node &  self, Node &  child, std::string &  value )
                                                                        const;

        void  operator()( Leaf &  self, std::string &  name ) const;

        void  operator()( Leaf &  self, int  value, size_t  index ) const;
    };


    void  Compiler::operator()( ParseResult &  parseResult, Action  value )
                                                                        const
    {
        parseResult.action = value;
    }


    void  Compiler::operator()( ParseResult &  parseResult, Subtree &  value )
                                                                        const
    {
        parseResult.expression = value;
    }


    void  Compiler::operator()( Subtree &  ast, Node &  node ) const
    {
        try
        {
            ast = boost::get< Subtree >( node );
        }
        catch( const boost::bad_get & )
        {
            ast.type = Operator( Top );
            ast.children.push_back( node );
        }
    }


    void  Compiler::operator()( Node &  self, Node &  left, Node &  right,
                                Operator  value ) const
    {
        Subtree &  ast( boost::get< Subtree >( self ) );

        ast.children.push_back( left );
        ast.type = value;

        Subtree *  astRight( boost::get< Subtree >( &right ) );

        if ( ! astRight )
        {
            ast.children.push_back( right );
            return;
        }

        bool        haveSamePriorities( false );
        Operator *  rightOp( boost::get< Operator >( &astRight->type ) );

        if ( rightOp )
            haveSamePriorities = value.priority == rightOp->priority;

        if ( value.hasRLAssoc || ! haveSamePriorities )
        {
            ast.children.push_back( right );
            return;
        }

        Subtree *  astDeepestRight( astRight );

        /* propagate left binary operators with LR associativity (i.e. all in
         * our grammar) deep into the AST until any operator with a different
         * priority (which includes operators in parentheses that have priority
         * 0) or a unary operator or a function occured */
        while ( true )
        {
            Subtree * candidate = boost::get< Subtree >(
                                            &astDeepestRight->children[ 0 ] );
            if ( ! candidate )
                break;

            if ( candidate->children.size() < 2 )
                break;

            bool        haveSamePriorities( false );
            Operator *  candidateOp( boost::get< Operator >(
                                                        &candidate->type ) );

            if ( candidateOp )
                haveSamePriorities = value.priority == candidateOp->priority;

            /* FIXME: what to do if candidate has RL association? Our grammar is
             * not a subject of this issue; probably no grammar is a subject. */
            if ( ! haveSamePriorities )
                break;

            astDeepestRight = candidate;
        }

        Subtree    astResult;
        astResult.children.push_back( ast.children[ 0 ] );
        astResult.children.push_back( astDeepestRight->children[ 0 ] );
        astResult.type = value;
        astDeepestRight->children[ 0 ] = astResult;
        self = right;
    }


    void  Compiler::operator()( Node &  self, Node &  child, Operator  value )
                                                                        const
    {
        Subtree &  ast( boost::get< Subtree >( self ) );
        ast.children.push_back( child );
        ast.type = value;
    }


    void  Compiler::operator()( Node &  self, Node &  primary ) const
    {
        self = primary;

        Subtree *  ast( boost::get< Subtree >( &self ) );

        if ( ! ast )
            return;

        Operator *  op( boost::get< Operator >( &ast->type ) );

        if ( op )
            op->priority = 0;
    }


    void  Compiler::operator()( Node &  self, Node &  child,
                                std::string &  value ) const
    {
        Subtree &  ast( boost::get< Subtree >( self ) );

        ast.children.push_back( child );
        ast.type = value;
    }


    void  Compiler::operator()( Leaf &  self, std::string &  name ) const
    {
        Variable &  variable( boost::get< Variable >( self ) );
        variable.name = name;
    }


    void  Compiler::operator()( Leaf &  self, int  value, size_t  index ) const
    {
        Variable &  variable( boost::get< Variable >( self ) );
        switch ( index )
        {
        case 0 :
            variable.index1 = value;
            break;
        case 1 :
            variable.index2 = value;
            break;
        default :
            break;
        }
    }


    template  < typename  Iterator >
    struct  Grammar : grammar< Iterator, ParseResult(), space_type >
    {
        Grammar();

        rule< Iterator, ParseResult(), space_type >            statement;

        rule< Iterator, Action(), space_type >                 action;

        rule< Iterator, Subtree(), space_type >                condition;

        rule< Iterator, Node(), space_type >                   expression;

        rule< Iterator, Node(), space_type >                   primary_expr;

        rule< Iterator, Node(), space_type >                   function1;

        rule< Iterator, std::string(), space_type >            identifier;

        rule< Iterator, Leaf(), space_type >                   leaf_operand;

        rule< Iterator, Leaf(), space_type >                   constant;

        rule< Iterator, Leaf(), space_type >                   variable;

        rule< Iterator, Node(), space_type >                   or_expr;

        rule< Iterator, Node(), space_type >                   and_expr;

        rule< Iterator, Node(), space_type >                   relation;

        rule< Iterator, Node(), space_type >                   addition;

        rule< Iterator, Node(), space_type >                   multiplication;

        rule< Iterator, Node(), space_type >                   unary_expr;

        rule< Iterator, Operator(), space_type >               unary_op;

        rule< Iterator, Operator(), space_type >               mult_op;

        rule< Iterator, Operator(), space_type >               add_op;

        rule< Iterator, Operator(), space_type >               rel_op;

        real_parser< double, strict_real_policies< double > >  strict_double;

        function< Compiler >                                   op;
    };


    template  < typename  Iterator >
    Grammar< Iterator >::Grammar() : Grammar::base_type( statement )
    {
        statement = action[ op( _val, _1 ) ] >>
                                        *( condition[ op( _val, _1 ) ] );

        action = lit( "keep" ) >>
                ( lit( "tpt" )[ _val = KeepTPT ] |
                  lit( "edt" )[ _val = KeepEDT ] ) |
                lit( "delete" ) >>
                ( lit ( "tpt" )[ _val = DeleteTPT ] |
                  lit ( "edt" )[ _val = DeleteEDT ] );

        condition = lit( "if" ) >> expression[ op( _val, _1 ) ];

        expression %= or_expr;

        identifier %= lexeme[ alpha >> *( alnum | lit( '_' ) ) ];

        primary_expr = function1[ _val = _1 ] |
                lit( '(' ) >> expression[ op( _val, _1 ) ] >> lit( ')' ) |
                leaf_operand[ _val = _1 ];

        leaf_operand %= constant | variable;

        constant %= strict_double | int_;

        variable = identifier[ op( _val, _1 ) ] >>
                -( lit( '[' ) >> ( uint_[ op( _val, _1, 0 ) ] - lit( '0' ) ) >>
                   -( lit( ',' ) >> ( uint_[ op( _val, _1, 1 ) ] -
                      lit( '0' ) ) ) >> lit( ']' ) );

        function1 = ( identifier >> lit( '(' ) >> expression >> lit( ')' ) )
                    [ op( _val, _2, _1 ) ];

        or_expr = ( and_expr >> lit( '|' ) >> or_expr )
                  [ op( _val, _1, _2, Operator( Or, 1 ) ) ] |
                  and_expr[ _val = _1 ];

        and_expr = ( relation >> lit( '&' ) >> and_expr )
                   [ op( _val, _1, _2, Operator( And, 2 ) ) ] |
                   relation[ _val = _1 ];

        relation = ( addition >> rel_op >> addition )
                   [ op( _val, _1, _3, _2 ) ] |
                   addition[ _val = _1 ];

        addition = ( multiplication >> add_op >> addition )
                   [ op( _val, _1, _3, _2 ) ] |
                   multiplication[ _val = _1 ];

        multiplication = ( unary_expr >> mult_op >> multiplication )
                         [ op( _val, _1, _3, _2 ) ] |
                         unary_expr[ _val = _1 ];

        unary_expr = ( unary_op >> primary_expr )[ op( _val, _2, _1 ) ] |
                     primary_expr[ _val = _1 ];

        unary_op = lit( '-' )[ _val = Operator( UMinus, 6, true ) ] |
                   lit( '!' )[ _val = Operator( Not, 6, true ) ];

        mult_op = lit( '*' )[ _val = Operator( Mult, 5 ) ] |
                  lit( '/' )[ _val = Operator( Div, 5 ) ];

        add_op = lit( '+' )[ _val = Operator( Plus, 4 ) ] |
                 lit( '-' )[ _val = Operator( Minus, 4 ) ];

        rel_op = lit( "<=" )[ _val = Operator( LessEq, 3 ) ] |
                 lit( ">=" )[ _val = Operator( MoreEq, 3 ) ] |
                 lit( "!=" )[ _val = Operator( NotEq, 3 ) ] |
                 lit( '<' )[ _val = Operator( Less, 3 ) ] |
                 lit( '>' )[ _val = Operator( More, 3 ) ] |
                 lit( '=' )[ _val = Operator( Eq, 3 ) ];
    }
}


int  main( void )
{
    CexmcCustomFilter::ParseResult  parseResult;
    CexmcCustomFilterEval           eval;
    std::vector< std::string >      expressions;

    expressions.push_back( "keep" );
    expressions.push_back( "delete tpt if (5.1 * 6) / var1[ 1 ] * var2" );
    expressions.push_back( "delete tpt if 5.1 * 6 + var1 * var2[ 1, 10 ]" );
    expressions.push_back( "delete tpt if some_var*6*2.9" );
    expressions.push_back( "delete tpt if 5.1 + 6 * Sum(var1) + var2" );
    expressions.push_back( "delete tpt if 5.1 + 6 * Sum(var1 + var2)" );
    expressions.push_back( "delete tpt if 5.1 + 6 * (var1 + var2)" );
    expressions.push_back( "delete tpt if 5.1 + 6" );
    expressions.push_back( "delete tpt if 5.1 + -6 + var1" );
    expressions.push_back( "delete tpt if (1 + 2) * (3 / (4 + -5) * 5 + 6)" );
    expressions.push_back( "delete tpt if (3 / (4 + -5) * 5 + 6) * (1 + 2)" );
    expressions.push_back( "delete tpt if 1 + 2 * (3 / (4 + -5) * 5 + 6)" );
    expressions.push_back( "keep if a > b & a | -(7 + 8) >= -a & a[1,20]" );
    expressions.push_back( "keep if -Sum(2) != 8 + 9 * (5 + 6) + ! 1 * 7" );
    expressions.push_back( "keep if -Sum(2) != 8 + 9 * ((5 + 6) > ! (1 * 7))" );
    expressions.push_back( "delete edt" );
    expressions.push_back( "delete tpt" );
    expressions.push_back( "delete tpt if 20 * 2.3 > 5" );
    expressions.push_back( "delete tpt if 46. > 5" );
    expressions.push_back( "delete tpt if 2 < 10" );
    expressions.push_back( "delete tpt if 1 & 1" );
    expressions.push_back( "delete tpt if 20 * 2.3 < 5 & 2 < aa" );
    expressions.push_back( "delete tpt if 7." );
    expressions.push_back( "delete tpt if Sqrt(Sqr(7-1))" );
    expressions.push_back( "delete tpt if 8 * 4 / 2 * 4 * 6 + 8 - 4 / 3" );
    expressions.push_back( "delete tpt if 8 * f(4 / 2)" );
    expressions.push_back( "delete tpt if 8 * f(4) / 2" );
    expressions.push_back( "delete tpt if "
                    "2 * 8 * (4 - 8 / 4) / (2 - 4) * 6 / 3" );
    expressions.push_back( "delete tpt if "
                    "c + 1 & 2 * 8 * (4 - 8 / 4) / (2 - 4) * 6 / 3 & d" );
    expressions.push_back( "delete tpt if "
                    "6 / 8 * ((4 / 3) / 2) * 4 / f(3 * 10 - 4 / 5 ) / 5 - 1" );
    expressions.push_back( "delete tpt if (8/2) * (4/2) * 6 * (2/2) " );

    CexmcCustomFilter::Grammar< std::string::const_iterator >
                                                        customFilterGrammar;

    for ( std::vector< std::string >::const_iterator  k( expressions.begin() );
          k != expressions.end(); ++k )
    {
        const char *  message( "passed" );

        std::string::const_iterator   begin( k->begin() );
        std::string::const_iterator   end( k->end() );

        parseResult.Initialize();

        if ( ! CexmcCustomFilter::phrase_parse( begin, end, customFilterGrammar,
                    CexmcCustomFilter::space, parseResult ) || begin != end )
            message = "failed";

        std::cout << "Parsing of '" << *k << "' " << message << std::endl;
        std::cout << "Result: action = " << parseResult.action <<
                     ", parsed tree = " << std::endl << std::endl;
        parseResult.expression.Print();
        std::cout << std::endl;

        bool  evalRes( eval( parseResult.expression ) );

        if ( evalRes )
            message = "passed";
        else
            message = "failed";
        std::cout << "Evaluation " << message << std::endl;;
    }
}

