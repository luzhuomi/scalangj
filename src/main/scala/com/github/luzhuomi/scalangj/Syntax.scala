package com.github.luzhuomi.scalangj

import java.lang.reflect.Modifier
import com.github.luzhuomi.scalangj.Syntax.NormalAnnotation
import com.github.luzhuomi.scalangj.Syntax.SingleElementAnnotation
import com.github.luzhuomi.scalangj.Syntax.MarkerAnnotation


object Syntax {

    // Packages
    /**
      * A compilation unit is the top level syntactic goal symbol of a Java program.
      *
      * @param pkg_decl the optional package declaration
      * @param imp_decls the list of import declarations
      * @param type_decls the list of type declarations
      */ 
    case class CompilationUnit(pkg_decl:Option[PackageDecl], imp_decls:List[ImportDecl], type_decls:List[TypeDecl]) 


    /**
      * A package declaration appears within a compilation unit to indicate the package to which the compilation unit belongs.
      *
      * @param name the name of the package
      */
    case class PackgeDecl(name:Name) 

    /**
      * An import declaration allows a static member or a named type to be referred to by a single unqualified identifier.
      * @param is_static signals whether the declaration only imports static members.
      * @param name name of the imported module
      * @param all_in_scope signals whether the declaration brings all names in the named type or package, or only brings a single name into scope.
      */
    case class ImportDecl(is_static:Boolean, name:Name, all_in_scope:Boolean)   


    // Declarations
    /**
      * A type declaration declares a class type or an interface type.
      */
    sealed trait TypeDecl
    case class ClassTypeDecl(class_decl:ClassDecl) extends TypeDecl
    case class InterfaceTypeDecl(iface_decl:InterfaceDecl) extends TypeDecl

    /**
      * A class declaration specifies a new named reference type.
      */

    sealed trait ClassDecl
    case class ClassDecl_(modifiers:List[Modifier], id:Ident, type_params:List[TypeParam], ref_type:Option[RefType], ref_types:List[RefType], body:ClassBody) extends ClassDecl
    case class EnumDecl(modifiers:List[Modifier], id:Ident, ref_types:List[RefType], body:EnumBody) extends ClassDecl


    /**
      * A class body may contain declarations of members of the class, that is,
      * fields, classes, interfaces and methods.
      * A class body may also contain instance initializers, static
      * initializers, and declarations of constructors for the class.
      * @param decls declarations in the class
      */
    case class ClassBody(decls:List[Decl])

    /**
      * he body of an enum type may contain enum constants.
      *
      * @param constants constants in the enum
      * @param decls declarations in the enum
      */
    case class EnumBody(constants:List[EnumConstant], decls:List[Decl])
   
    /**
      * An enum constant defines an instance of the enum type
      *
      * @param id
      * @param args
      * @param body
      */
    case class EnumConstant(id:Ident, args:List[Argument], body:Option[ClassBody])

    /**
      * An interface declaration introduces a new reference type whose members
      * are classes, interfaces, constants and abstract methods. This type has
      * no implementation, but otherwise unrelated classes can implement it by
      * providing implementations for its abstract methods.
      * @param kind 
      * @param modifiers
      * @param id
      * @param type_params
      * @param ref_types
      * @param body
      */
    case class InterfaceDecl(kind:InterfaceKind, modifiers:List[Modifier], id:Ident, type_params:List[TypeParam], ref_types:List[RefType], body:InterfaceBody)

    /**
      * Interface can declare either a normal interface or an annotation
      */
    sealed trait InterfaceKind
    case object InterfaceNormal extends InterfaceKind
    case object InterfaceAnnotation extends InterfaceKind

    /**
      * The body of an interface may declare members of the interface.
      *
      * @param member_decls
      */
    case class InterfaceBody(member_decls:List[MemberDecl]) 

    /**
      * A declaration is either a member declaration, or a declaration of an
      * initializer, which may be static.
      */
    sealed trait Decl 
    case class MemberDecl_(member:MemberDecl) extends Decl
    case class InitDecl(is_static:Boolean, blk:Block) extends Decl


    /**
      * A class or interface member can be an inner class or interface, a field or
      * constant, or a method or constructor. An interface may only have as members
      * constants (not fields), abstract methods, and no constructors.
      */
    sealed trait MemberDecl
    /**
      * The variables of a class type are introduced by field declarations.
      * @param modifiers
      * @param ty
      * @param var_decls
      */
    case class FieldDecl(modifiers:List[Modifier], ty:Type, var_decls:List[VarDecl]) extends MemberDecl
    /**
      * A method declares executable code that can be invoked, passing a fixed number of values as arguments.
      *
      * @param modifiers
      * @param type_params
      * @param ty
      * @param id
      * @param formal_params
      * @param ex_types
      * @param exp
      * @param body
      */
    case class MethodDecl(modifiers:List[Modifier], type_params:List[TypeParams], ty:Option[Type], id:Ident, formal_params:List[FormalParams], ex_types:List[ExceptionType], exp:Option[Exp], body:MethodBody) extends MemberDecl
    
    /**
      * A constructor is used in the creation of an object that is an instance of a class.
      *
      * @param modifiers
      * @param type_params
      * @param id
      * @param formal_parms
      * @param ex_types
      * @param body
      */
    case class ConstructorDecl(modifiers:List[Modier], type_params:List[TypeParam], id:Ident, formal_parms:List[FormalParam], ex_types:List[ExceptionType], body:ConstructorBody) extends MemberDecl
    /**
      * A member class is a class whose declaration is directly enclosed in another class or interface declaration.
      *
      * @param class_decl
      */
    case class MemberClassDecl(class_decl:ClassDecl) extends MemberDecl
    /**
      * A member interface is an interface whose declaration is directly enclosed in another class or interface declaration.
      *
      * @param iface_decl
      */
    case class MemberInterfaceDecl(iface_decl:InterfaceDecl) extends MemberDecl

    /**
      * A declaration of a variable, which may be explicitly initialized.
      *
      * @param id
      * @param var_init
      */
    case class VarDecl(id:VarDeclId, var_init:Option[VarInit])

    /**
      * The name of a variable in a declaration, which may be an array.
      */
    sealed trait VarDeclId
    case class VarId(id:Ident) extends VarDeclId
    /**
      * Multi-dimensional arrays are represented by nested applications of 'VarDeclArray'.
      *
      * @param var_decl_id
      */
    case class VarDeclArray(var_decl_id:VarDeclId) extends VarDeclId


    /**
      * Explicit initializer for a variable declaration.
      */
    sealed trait VarInit
    case class InitExp(exp:Exp) extends VarInit
    case class InitArray(array_init:ArrayInit) extends VarInit

    /**
      * A formal parameter in method declaration. The last parameter
      * for a given declaration may be marked as variable arity,
      * indicated by the boolean argument.
      * @param modifiers
      * @param ty
      * @param has_arity
      * @param var_decl_id
      */
    case class FormalParam(modifiers:List[Modifier], ty:Type, has_arity:Boolean, var_decl_id:VarDeclId) 

    /**
      * A method body is either a block of code that implements the method or simply a
      *  semicolon, indicating the lack of an implementation (modelled by 'Nothing').
      * @param block
      */
    case class MethodBody(block:Option[Block])

    /**
      * The first statement of a constructor body may be an explicit invocation of
      * another constructor of the same class or of the direct superclass.
      * @param expl_Constr_inv
      * @param blk_stmts
      */
    case class ConstructorBody(expl_Constr_inv: Option[ExplConstrInv], blk_stmts:List[BlockStmt])


    /**
      * An explicit constructor invocation invokes another constructor of the
      * same class, or a constructor of the direct superclass, which may
      * be qualified to explicitly specify the newly created object's immediately
      */
    sealed trait ExplConstrInv 

    case class ThisInvoke(ref_types:List[RefType], args:List[Argument]) extends ExplConstrInv
    case class SuperInvoke(ref_type:List[RefType], args:List[Argument]) extends ExplConstrInv
    case class PrimarySuperInvoke(exp:Exp, ref_type:List[RefType], args:List[Argument]) extends ExplConstrInv

    /**
      * A modifier specifying properties of a given declaration. In general only
      * a few of these modifiers are allowed for each declaration type, for instance
      * a member type declaration may only specify one of public, private or protected.
      */
    sealed trait Modifier
    case object Public extends Modifier
    case object Protected extends Modifier
    case object Private extends Modifier
    case object Abstract extends Modifier
    case object Final extends Modifier
    case object Static extends Modifier
    case object StrictFP extends Modifier
    case object Transient extends Modifier
    case object Volatile extends Modifier
    case object Native extends Modifier
    case class Annotation_(ann:Annotation) extends Modifier
    case object Synchronized extends Modifier

    // show instance?

    def show(m:Modifier):String = m match {
      case Public => "public"
      case Protected => "protected"
      case Abstract => "abstract"
      case Final => "final"
      case Static => "static"
      case StrictFP => "strictfp"
      case Transient => "transient"
      case Volatile => "volatile"
      case Native => "native"
      case Annotation(a) => show(a)
      case Synchronized => "synchronized"
    }

    /**
      * Annotations have three different forms: no-parameter, single-parameter or key-value pairs
      */
    sealed trait Annotation
    // Not type because not type generics not allowed
    case class NormalAnnotation(annName:Name, annKV:List[(Ident, Element)]) extends Annotation
    case class SingleElementAnnotation( annName:Name, annValue:ElementValue) extends Annotation
    case class MarkerAnnotation(annName:Name) extends Annotation

    def desugarAnnotation(a:Annotation):(Name,List[(Ident,ElementValue)]) = a match {
      case NormalAnnotation(annName, annKV) => (annName, annKv)
      case SingleElementAnnotation(annName, annValue) => (annName, List((Ident("value"), annValue)))
      case MarkerAnnotation(annName) => (annName, List())
    } 

    /**
      * Annotations may contain  annotations or (loosely) expressions
      */
    sealed trait ElementValue
    case class EVVal(var_init:VarInit) extends ElementValue
    case class EVAnn(ann:Annotatoin) extends ElementValue


    // Statements

    /**
      * A block is a sequence of statements, local class declarations
      * and local variable declaration statements within braces.
      * @param stmts
      */
    case class Block(stmts:List[BlockStmt])

    /**
      * A block statement is either a normal statement, a local
      * class declaration or a local variable declaration.
      */
    sealed trait BlockStmt 
    case class BlockStmt_(stmt:Stmt) extends BlockStmt
    case class LocalClass(class_decl:ClassDecl) extends BlockStmt
    case class LocalVars(modifiers:List[Modifier], ty:Type, var_decls:List[VarDecl]) extends BlockStmt


    sealed trait Stmt
    case class StmtBlock(blk:Block) extends Stmt
    case class IfThen(exp:Exp, stmt:Stmt) extends Stmt
    case class IfThenElse(exp:Exp, then_stmt:Stmt, else_stmt:Stmt) extends Stmt
    case class While(exp:Exp, stmt:Stmt) extends Stmt
    case class BasicFor(init:Option[ForInit], loop_cond:Option[Exp], post_update:Option[Exp], stmt:Stmt) extends Stmt
}
