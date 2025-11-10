-- This is a debug module, omitted by default from Cabal file.
-- Add it to 'other-modules' and 'import InputShowOrphans ()'
-- whenever you need to print 'HsDecl' and its components
-- for debug purposes.
-- (GHC provides 'instance Outputable', but it's not enough:
-- it pretty prints while hiding internal structure.)

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Input.ShowOrphans () where

import GHC.Core.Coercion.Axiom (Branches (..), BuiltInFamInjectivity (..), BuiltInFamRewrite (..), CoAxBranch (..), CoAxiom (..), CoAxiomRule (..))
import GHC.Core.TyCo.Rep (CoSel (..), Coercion (..), CoercionHole (..), FunSel (..), MCoercion (..), MCoercionN, TyLit (..), Type (..), UnivCoProvenance (..))
import GHC.Core.TyCon (TyCon (..))
import GHC.Core.Type (ForAllTyBinder, ForAllTyFlag (..), FunTyFlag (..), Specificity (..))
import GHC.Data.BooleanFormula (BooleanFormula (..))
import GHC.Hs (ActivationAnn (..), AnnArithSeq (..), AnnClassDecl (..), AnnClsInstDecl (..), AnnConDeclGADT (..), AnnConDeclH98 (..), AnnDataDefn (..), AnnDecl (..), AnnExplicitSum (..), AnnFamilyDecl (..), AnnFieldLabel (..), AnnFunRhs (..), AnnPSB (..), AnnProjection (..), AnnProvenance (..), AnnSig (..), AnnSpecSig (..), AnnSynDecl (..), AnnTransStmt (..), AnnTyVarBndr (..), Anno, AnnsIf (..), ArithSeqInfo (..), BracketAnn (..), CImportSpec (..), ClsInstDecl (..), ConDecl (..), ConDeclField (..), DataConCantHappen, DataDefnCons (..), DataFamInstDecl (..), DefaultDecl (..), DerivClauseTys (..), DerivDecl (..), DerivStrategy (..), DocDecl (..), DotFieldOcc (..), EpAnnHsCase (..), EpAnnLam (..), EpAnnSumPat (..), EpAnnUnboundVar (..), EpLinearArrow (..), FamEqn (..), FamilyDecl (..), FamilyInfo (..), FamilyResultSig (..), FieldLabelStrings (..), FieldOcc (..), FixitySig (..), ForeignDecl (..), ForeignExport (..), ForeignImport (..), FunDep (..), GRHS (..), GRHSs (..), GhcPs, GrhsAnn (..), HsArg (..), HsArrAppType (..), HsArrowMatchContext (..), HsArrowOf (..), HsBang (..), HsBind, HsBindLR (..), HsBndrKind (..), HsBndrVar (..), HsBndrVis (..), HsCmd (..), HsCmdTop (..), HsConDeclGADTDetails (..), HsConDetails (..), HsConPatTyArg (..), HsDataDefn (..), HsDecl (..), HsDerivingClause (..), HsDoFlavour (..), HsExpr (..), HsFieldBind (..), HsForAllTelescope (..), HsGroup (..), HsIPBinds (..), HsIPName (..), HsLamVariant (..), HsLit (..), HsLocalBindsLR (..), HsMatchContext (..), HsMultAnn (..), HsOuterTyVarBndrs (..), HsOverLit (..), HsPatSigType (..), HsPatSynDir (..), HsPragE (..), HsQuote (..), HsRecFields (..), HsRuleAnn (..), HsScaled (..), HsSigType (..), HsStmtContext (..), HsTupArg (..), HsTupleSort (..), HsTyLit (..), HsTyPat (..), HsTyVarBndr (..), HsType (..), HsUntypedSplice (..), HsValBindsLR (..), HsWildCardBndrs (..), IPBind (..), InjectivityAnn (..), InstDecl (..), LHsQTyVars (..), LHsRecUpdFields (..), Match (..), MatchGroup (..), NHsValBindsLR (..), NamespaceSpecifier (..), NoExtField (..), OverLitVal (..), ParStmtBlock (..), Pat (..), PatSynBind (..), RecFieldsDotDot (..), RecordPatSynField (..), RoleAnnotDecl (..), RuleBndr (..), RuleDecl (..), RuleDecls (..), Sig (..), SpliceDecl (..), SrcStrictness (..), SrcUnpackedness (..), StandaloneKindSig (..), StmtLR (..), TransForm (..), TyClDecl (..), TyClGroup (..), TyFamInstDecl (..), WarnDecl (..), WarnDecls (..), WithHsDocIdentifiers (..), XViaStrategyPs (..))
import GHC.Hs.Basic (FieldLabelString (..), FixityDirection (..), LexicalFixity (..), Role (..))
import GHC.Parser.Annotation (AnnContext (..), AnnList (..), AnnListBrackets (..), AnnListItem (..), AnnParen (..), AnnPragma (..), AnnSortKey (..), EpAnn (..), EpAnnComments (..), EpLayout (..), EpToken (..), EpUniToken (..), NameAdornment (..), NameAnn (..), NoEpAnns (..), SrcSpanAnnA, TrailingAnn (.. )
import GHC.Types.Basic (Activation (..), Boxity (..), DoPmc (..), GenReason (..), InlinePragma (..), LeftOrRight (..), Origin (..), OverlapMode (..), PromotionFlag (..), RecFlag (..), TopLevelFlag (..))
import GHC.Types.Fixity (Fixity (..))
import GHC.Types.ForeignCall (CCallConv (..), CCallTarget (..), CExportSpec (..), CType (..), Header (..))
import GHC.Types.Name (Name)
import GHC.Types.Name.Occurrence (OccName (..))
import GHC.Types.Name.Reader (RdrName (..))
import GHC.Types.SourceText (StringLiteral (..))
import GHC.Types.SrcLoc (GenLocated)
import GHC.Types.Var (Var (..), VarBndr (..))
import GHC.Unit (GenModule (..), Module)
import GHC.Unit.Module.Warnings (InWarningCategory (..), WarningTxt (..))
import GHC.Utils.Outputable (Outputable (..), defaultSDocContext, runSDoc )

instance Show OccName where
    show = show . (`runSDoc` defaultSDocContext) . ppr
instance Show GHC.Types.Name.Name where
    show = show . (`runSDoc` defaultSDocContext) . ppr
instance Show GHC.Types.Var.Var where
    show = show . (`runSDoc` defaultSDocContext) . ppr
instance Show GHC.Core.TyCon.TyCon where
    show = show . (`runSDoc` defaultSDocContext) . ppr

instance Show BuiltInFamRewrite where
    show (BIF_Rewrite a b c d _ _) = show ("BIF_Rewrite", a, b, c, d)
instance Show BuiltInFamInjectivity where
    show (BIF_Interact a b _) = show ("BIF_Interact", a, b)
instance Show CoercionHole where
    show (CoercionHole a _ b) = show ("CoercionHole", a, b)
instance Show (NHsValBindsLR GhcPs) where
    show (NValBinds a _) = show ("NValBinds", a)

deriving instance Show (HsValBindsLR GhcPs GhcPs)
deriving instance Show (GHC.Hs.Sig GhcPs)
deriving instance Show AnnArithSeq
deriving instance Show RecFlag
deriving instance (Show a, Show b, Show c) => Show (HsConDetails a b c)
deriving instance Show (HsExpr GhcPs)
deriving instance Show (IPBind GhcPs)
deriving instance Show (HsMultAnn GhcPs)
deriving instance Show (HsBind GhcPs)
deriving instance Show AnnTransStmt
deriving instance Show (FamilyInfo GhcPs)
deriving instance Show (FamilyResultSig GhcPs)
deriving instance Show DoPmc
deriving instance Show GenReason
deriving instance Show (RecordPatSynField GhcPs)
deriving instance Show (HsPatSynDir GhcPs)
deriving instance Show AnnPSB
deriving instance Show (PatSynBind GhcPs GhcPs)
deriving instance Show Origin
deriving instance Show (MatchGroup GhcPs (GenLocated SrcSpanAnnA (HsCmd GhcPs)))
deriving instance Show (MatchGroup GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs)))
deriving instance (Show a, Show (Anno (GRHS GhcPs a))) => Show (Match GhcPs a)
deriving instance Show (InjectivityAnn GhcPs)
deriving instance Show AnnFamilyDecl
deriving instance Show TopLevelFlag
deriving instance Show (FamilyDecl GhcPs)
deriving instance Show (DataFamInstDecl GhcPs)
deriving instance Show (InstDecl GhcPs)
deriving instance Show AnnClsInstDecl
deriving instance Show (ClsInstDecl GhcPs)
deriving instance (Show a) => Show (AnnSortKey a)
deriving instance Show TransForm
deriving instance Show (DerivClauseTys GhcPs)
deriving instance Show (HsBndrVis GhcPs)
deriving instance Show (LHsQTyVars GhcPs)
deriving instance Show (GHC.Hs.FunDep GhcPs)
deriving instance Show AnnSynDecl
deriving instance Show AnnClassDecl
deriving instance Show EpLayout
deriving instance Show (TyClDecl GhcPs)
deriving instance Show (HsDerivingClause GhcPs)
deriving instance Show AnnDataDefn
deriving instance Show CType
deriving instance Show (HsDataDefn GhcPs)
deriving instance Show EpAnnSumPat
deriving instance Show (HsTyPat GhcPs)
deriving instance Show (HsConPatTyArg GhcPs)
deriving instance Show (Pat GhcPs)
deriving instance (Show a) => Show (DataDefnCons a)
deriving instance (Show a) => Show (HsScaled GhcPs a)
deriving instance Show AnnConDeclGADT
deriving instance Show AnnConDeclH98
deriving instance Show (HsConDeclGADTDetails GhcPs)
deriving instance Show (ConDecl GhcPs)
deriving instance Show (ParStmtBlock GhcPs GhcPs)
deriving instance Show (StmtLR GhcPs GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs)))
deriving instance Show (StmtLR GhcPs GhcPs (GenLocated SrcSpanAnnA (HsCmd GhcPs)))
deriving instance Show (HsForAllTelescope GhcPs)
deriving instance Show (HsUntypedSplice GhcPs)
deriving instance (Show a) => Show (WithHsDocIdentifiers a GhcPs)
deriving instance (Show a) => Show (HsArrowOf a GhcPs)
deriving instance Show (HsIPBinds GhcPs)
deriving instance Show Boxity
deriving instance Show EpAnnUnboundVar
deriving instance Show AnnExplicitSum
deriving instance Show AnnProjection
deriving instance Show (HsPragE GhcPs)
deriving instance Show AnnFieldLabel
deriving instance Show HsArrAppType
deriving instance Show EpAnnLam
deriving instance Show AnnsIf
deriving instance Show EpAnnHsCase
deriving instance Show (HsCmd GhcPs)
deriving instance Show (HsCmdTop GhcPs)
deriving instance Show FieldLabelString
deriving instance Show (DotFieldOcc GhcPs)
deriving instance Show (FieldLabelStrings GhcPs)
deriving instance Show (ArithSeqInfo GhcPs)
deriving instance Show (TyClGroup GhcPs)
deriving instance Show (HsGroup GhcPs)
deriving instance Show (HsQuote GhcPs)
deriving instance Show (LHsRecUpdFields GhcPs)
deriving instance Show (HsLocalBindsLR GhcPs GhcPs)
deriving instance Show HsArrowMatchContext
deriving instance (Show a) => Show (HsStmtContext a)
deriving instance (Show a, Show b) => Show (HsFieldBind a b)
deriving instance (Show a, Show b) => Show (BracketAnn a b)
deriving instance Show OverLitVal
deriving instance Show (HsOverLit GhcPs)
deriving instance Show (HsLit GhcPs)
deriving instance Show (HsTupArg GhcPs)
deriving instance (Show a) => Show (HsRecFields GhcPs a)
deriving instance Show RecFieldsDotDot
deriving instance (Show a) => Show (AnnList a)
deriving instance (Show a) => Show (HsMatchContext a)
deriving instance Show GrhsAnn
deriving instance (Show a) => Show (GRHS GhcPs a)
deriving instance (Show a, Show (Anno (GRHS GhcPs a))) => Show (GRHSs GhcPs a)
deriving instance Show Role
deriving instance (Show a, Show b) => Show (HsArg GhcPs a b)
deriving instance (Show a) => Show (FamEqn GhcPs a)
deriving instance Show (TyFamInstDecl GhcPs)
deriving instance Show OverlapMode
deriving instance Show XViaStrategyPs
deriving instance Show (DerivStrategy GhcPs)
deriving instance Show (DerivDecl GhcPs)
deriving instance Show AnnSig
deriving instance Show AnnSpecSig
deriving instance Show InlinePragma
deriving instance Show HsTupleSort
deriving instance Show AnnContext
deriving instance Show HsDoFlavour
deriving instance Show LexicalFixity
deriving instance Show AnnFunRhs
deriving instance Show HsLamVariant
deriving instance (Show a) => Show (HsWildCardBndrs GhcPs a)
deriving instance (Show a) => Show (BooleanFormula a)
deriving instance Show FixityDirection
deriving instance Show GHC.Types.Fixity.Fixity
deriving instance Show (FixitySig GhcPs)
deriving instance (Show (Branches a)) => Show (CoAxiom a)
deriving instance Show CoAxiomRule
deriving instance Show UnivCoProvenance
deriving instance Show FunSel
deriving instance Show CoSel
deriving instance Show LeftOrRight
deriving instance Show Coercion
deriving instance Show MCoercionN
deriving instance Show GHC.Core.TyCo.Rep.Type
deriving instance Show (HsType GhcPs)
deriving instance Show PromotionFlag
deriving instance Show ForAllTyFlag
deriving instance Show ForAllTyBinder
deriving instance Show FunTyFlag
deriving instance Show TyLit
deriving instance Show CoAxBranch
deriving instance Show (Branches a)
deriving instance Show AnnParen
deriving instance Show HsIPName
deriving instance Show SrcUnpackedness
deriving instance Show SrcStrictness
deriving instance Show HsBang
deriving instance Show (HsTyLit GhcPs)
deriving instance Show AnnListBrackets
deriving instance Show (FieldOcc GhcPs)
deriving instance Show (ConDeclField GhcPs)
deriving instance Show (DocDecl GhcPs)
deriving instance Show (RoleAnnotDecl GhcPs)
deriving instance Show (HsDecl GhcPs)
deriving instance Show (DefaultDecl GhcPs)
deriving instance Show (RuleDecl GhcPs)
deriving instance Show (RuleDecls GhcPs)
deriving instance Show (SpliceDecl GhcPs)
deriving instance Show (EpUniToken a b)
deriving instance Show (EpToken a)
deriving instance Show EpAnnComments
deriving instance (Show a) => Show (EpAnn a)
deriving instance Show TrailingAnn
deriving instance Show NameAdornment
deriving instance Show NameAnn
deriving instance Show NoExtField
deriving instance Show EpLinearArrow
deriving instance Show Module
deriving instance Show Specificity
deriving instance Show RdrName
deriving instance Show AnnListItem
deriving instance Show NoEpAnns
deriving instance Show DataConCantHappen
deriving instance Show AnnTyVarBndr
deriving instance Show (HsBndrVar GhcPs)
deriving instance Show (HsBndrKind GhcPs)
deriving instance (Show a) => Show (HsTyVarBndr a GhcPs)
deriving instance (Show a) => Show (HsOuterTyVarBndrs a GhcPs)
deriving instance Show (HsSigType GhcPs)
deriving instance Show (StandaloneKindSig GhcPs)
deriving instance Show CCallConv
deriving instance Show Header
deriving instance Show CCallTarget
deriving instance Show CImportSpec
deriving instance Show AnnPragma
deriving instance Show StringLiteral
deriving instance Show CExportSpec
deriving instance Show InWarningCategory
deriving instance Show NamespaceSpecifier
deriving instance Show (ForeignExport GhcPs)
deriving instance Show (ForeignImport GhcPs)
deriving instance Show (ForeignDecl GhcPs)
deriving instance Show (WarningTxt GhcPs)
deriving instance Show (WarnDecl GhcPs)
deriving instance Show (WarnDecls GhcPs)
deriving instance Show GHC.Types.Basic.Activation
deriving instance Show ActivationAnn
deriving instance Show HsRuleAnn
deriving instance Show (AnnProvenance GhcPs)
deriving instance Show (AnnDecl GhcPs)
deriving instance Show (HsPatSigType GhcPs)
deriving instance Show (RuleBndr GhcPs)
