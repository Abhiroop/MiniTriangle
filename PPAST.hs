{-# LANGUAGE RecordWildCards #-}
{-
******************************************************************************
*                                  H M T C                                   *
*                                                                            *
*       Module:         PPAST                                                *
*       Purpose:        Simple pretty printer for AST                        *
*       Authors:        Henrik Nilsson                                       *
*                                                                            *
*                 Copyright (c) Henrik Nilsson, 2006 - 2012                  *
*                                                                            *
******************************************************************************
-}

-- | Simple pretty printer for AST.

module PPAST (
    ppAST       -- AST -> String
) where

-- HMTC module imports
import Name (Name)
import SrcPos (SrcPos)
import PPUtilities
import AST
import Data.Maybe

------------------------------------------------------------------------------
-- Pretty printing of AST
------------------------------------------------------------------------------

-- | Converts AST to a nicely laid-out textual representation for
-- display purposes.

ppAST :: AST -> String
ppAST ast = ppCommand 0 (astCmd ast) ""


------------------------------------------------------------------------------
-- Pretty printing of commands
------------------------------------------------------------------------------

ppCommand :: Int -> Command -> ShowS
ppCommand n (CmdAssign {..}) =
    indent n . showString "CmdAssign" . spc . ppSrcPos cmdSrcPos. nl
    . ppExpression (n+1) caVar
    . ppExpression (n+1) caVal
ppCommand n (CmdCall {..}) =
    indent n . showString "CmdCall" . spc . ppSrcPos cmdSrcPos. nl
    . ppExpression (n+1) ccProc
    . ppSeq (n+1) ppExpression ccArgs
ppCommand n (CmdSeq {..}) =
    indent n . showString "CmdSeq" . spc . ppSrcPos cmdSrcPos. nl
    . ppSeq (n+1) ppCommand csCmds
ppCommand n (CmdIf {..}) =
    indent n . showString "CmdIf" . spc . ppSrcPos cmdSrcPos. nl
    . ppExpression (n+1) ciCond
    . ppCommand (n+1) ciThen
    . ppCommand (n+1) (fromMaybe (CmdEmpty "") ciElse)
ppCommand n (CmdWhile {..}) =
    indent n . showString "CmdWhile" . spc . ppSrcPos cmdSrcPos. nl
    . ppExpression (n+1) cwCond
    . ppCommand (n+1) cwBody
ppCommand n (CmdLet {..}) =
    indent n . showString "CmdLet" . spc . ppSrcPos cmdSrcPos. nl
    . ppSeq (n+1) ppDeclaration clDecls
    . ppCommand (n+1) clBody
ppCommand n (CmdRepeat {..}) =
    indent n . showString "CmdRepeat" . spc . ppSrcPos cmdSrcPos . nl
    . ppCommand (n + 1) crBody
    . ppExpression (n + 1) crCond


------------------------------------------------------------------------------
-- Pretty printing of expressions
------------------------------------------------------------------------------

ppExpression :: Int -> Expression -> ShowS
ppExpression n (ExpLitInt {eliVal = v}) =
    indent n . showString "ExpLitInt". spc . shows v . nl
ppExpression n (ExpVar {evVar = v}) =
    indent n . showString "ExpVar" . spc . ppName v . nl
ppExpression n (ExpApp {eaFun = f, eaArgs = es, expSrcPos = sp}) =
    indent n . showString "ExpApp" . spc . ppSrcPos sp . nl
    . ppExpression (n+1) f
    . ppSeq (n+1) ppExpression es
ppExpression n (ExpTernary {..}) =
    indent n . showString "ExpTernary" . spc . ppSrcPos expSrcPos . nl
    . ppExpression (n+1) etBoolExp
    . ppExpression (n+1) etExp1
    . ppExpression (n+1) etExp2

------------------------------------------------------------------------------
-- Pretty printing of declarations
------------------------------------------------------------------------------

ppDeclaration :: Int -> Declaration -> ShowS
ppDeclaration n (DeclConst {dcConst = c, dcType = t, dcVal = e,
                            declSrcPos = sp}) = 
    indent n . showString "DeclConst" . spc . ppSrcPos sp . nl
    . indent (n+1) . ppName c . nl
    . ppTypeDenoter (n+1) t
    . ppExpression (n+1) e
ppDeclaration n (DeclVar {dvVar = v, dvType = t, dvMbVal = me,
                          declSrcPos = sp}) = 
    indent n . showString "DeclVar" . spc . ppSrcPos sp . nl
    . indent (n+1) . ppName v . nl
    . ppTypeDenoter (n+1) t
    . maybe id (ppExpression (n+1)) me


------------------------------------------------------------------------------
-- Pretty printing of type denoters
------------------------------------------------------------------------------

ppTypeDenoter :: Int -> TypeDenoter -> ShowS
ppTypeDenoter n (TDBaseType {tdbtName = tn}) = 
    indent n . showString "TDBaseType" . spc . ppName tn . nl
