{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections, OverloadedStrings, Rank2Types, DeriveDataTypeable, ImportQualifiedPost #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Input.Haddock(parseHoogle, fakePackage, input_haddock_test) where

import Language.Haskell.Exts as HSE (Decl (..), ParseResult (..), GadtDecl (..), Type (..), Name (..), DeclHead (..), parseDeclWithMode, DataOrNew (..), noLoc, QName (..), ModuleName (..), TyVarBind (..), Boxed (..), Unpackedness (..), BangType (..), SpecialCon (..), Context (..), Asst (..), InstRule (..), InstHead (..), FunDep (..), ResultSig (..), Promoted (..), MaybePromotedName (UnpromotedName), Op (..), Assoc (..))
import Data.Char
import Data.List.Extra
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Data
import Input.Item
import General.Util
import Control.DeepSeq
import Control.Monad.Trans.Class
import General.Conduit
import Control.Monad.Extra
import Control.Exception.Extra
import Data.Generics.Uniplate.Data
import General.Str
import Safe


import GHC.Parser
import GHC.Parser.Lexer
import GHC.Data.EnumSet qualified as EnumSet
import GHC.Data.FastString
import GHC.Data.StringBuffer
import GHC.Types.SrcLoc (GenLocated, mkRealSrcLoc, unLoc, GenLocated(L))
import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Ppr
import System.IO
import GHC.Parser.Annotation
import GHC.Types.Basic (Activation(..), PromotionFlag(..), LeftOrRight(..), InlinePragma)
import GHC.Types.Name (Name(..), nameOccName, tvName)
import GHC.Types.Name.Reader (RdrName(..), rdrNameOcc, rdrNameSpace)
import GHC.Unit (GenModule(..), Module)
import GHC.Types.Name.Occurrence (OccName(..), occNameString)
import GHC.Core.Type (Specificity(..), ForAllTyBinder(..), ForAllTyFlag(..), FunTyFlag(..))
import GHC.Types.ForeignCall (CCallConv(..), Header(..), CCallTarget(..), CExportSpec(..), CType(..))
import GHC.Unit.Module.Warnings (WarningTxt(..), InWarningCategory(..))
import GHC.Types.SourceText (StringLiteral(..), SourceText (..))
import GHC.Hs (GhcPs, HsDecl(..), HsBind(..), HsBindLR(..), TyClDecl(..), InstDecl(..), DerivDecl(..), Sig(..), StandaloneKindSig(..), HsSigType(..), HsOuterTyVarBndrs(..), HsOuterSigTyVarBndrs(..), HsTyVarBndr(..), HsBndrVar(..), NoExtField(..), DefaultDecl(..), HsType(..), HsArrowOf(..), EpLinearArrow(..), DataConCantHappen, WithHsDocIdentifiers(..), HsForAllTelescope(..), HsUntypedSplice(..), HsExpr(..), StmtLR(..), XLastStmt, HsBndrKind(..), AnnTyVarBndr(..), ForeignDecl(..), ForeignImport(..), CImportSpec(..), ForeignExport(..), WarnDecl(..), WarnDecls(..), NamespaceSpecifier(..), AnnDecl(..), AnnProvenance(..), RuleDecl(..), RuleDecls(..), RuleBndr(..), HsPatSigType(..), HsRuleAnn(..), ActivationAnn(..), SpliceDecl(..), DocDecl(..), RoleAnnotDecl(..), ConDeclField(..), FieldOcc(..), HsTyLit(..), HsTupleSort(..), HsIPName(..), HsBang(..), SrcUnpackedness(..), SrcStrictness(..), HsWildCardBndrs(..), FixitySig(..), AnnSig(..), HsLocalBindsLR(..), MatchGroup(..), Match(..), GRHS(..), GRHSs(..), GrhsAnn(..), Anno(..), HsMatchContext(..), HsStmtContext(..), HsDoFlavour(..), AnnFunRhs(..), HsLamVariant(..), HsArrowMatchContext(..), Pat(..), ClsInstDecl(..), TyFamInstDecl(..), FamEqn(..), HsArg(..), BracketAnn(..), HsLit(..), HsValBindsLR(..), NHsValBindsLR(..), IPBind(..), HsQuote(..), HsGroup(..), TyClGroup(..), HsCmdTop(..), HsCmd(..), EpAnnHsCase(..), AnnsIf(..), HsPragE(..), PatSynBind(..), ConDecl(..), HsScaled(..), AnnConDeclH98(..), AnnClassDecl(..), FamilyInfo(..), AnnFamilyDecl(..), DataFamInstDecl(..), AnnClsInstDecl(..), moduleNameString)
import GHC.Hs.Basic (Role(..), FixityDirection(..), LexicalFixity(..), FieldLabelString(..))
import GHC.Core.TyCo.Rep (Type(..), TyLit(..), Coercion(..), MCoercion(..), MCoercionN(..), UnivCoProvenance(..), CoSel(..), FunSel(..), CoercionHole(..))
import GHC.Types.Var (VarBndr(..), Var(..))
import GHC.Core.TyCon (TyCon(..))
import GHC.Core.Coercion.Axiom (CoAxiomRule(..), CoAxiom(..), Branches(..), CoAxBranch(..), BuiltInFamRewrite(..), BuiltInFamInjectivity(..))
import GHC.Data.BooleanFormula (BooleanFormula(..))
import GHC.Types.Fixity (Fixity(..))
import GHC.Hs (AnnSpecSig(..), DerivStrategy(..), XViaStrategyPs(..), HsOuterFamEqnTyVarBndrs, HsRecFields(..), HsFieldBind(..), RecFieldsDotDot(..), HsOverLit(..), OverLitVal(..), HsTupArg(..), HsIPBinds(..), LHsRecUpdFields(..), FieldLabelStrings(..), DotFieldOcc(..), AnnFieldLabel(..), ArithSeqInfo(..), HsArrAppType(..), EpAnnLam(..), EpAnnUnboundVar(..), AnnExplicitSum(..), AnnProjection(..), AnnArithSeq(..), ParStmtBlock(..), AnnTransStmt(..), TransForm(..), HsMultAnn(..), HsConDetails(..), HsConPatTyArg(..), HsTyPat(..), EpAnnSumPat(..), FamilyDecl(..), HsDataDefn(..), DataDefnCons(..), HsConDeclGADTDetails(..), AnnConDeclGADT(..), HsDerivingClause(..), DerivClauseTys(..), AnnDataDefn(..), LHsQTyVars(..), HsBndrVis(..), FunDep(..), AnnSynDecl(..), FamilyResultSig(..), InjectivityAnn(..), RecordPatSynField(..), HsPatSynDir(..), AnnPSB(..), hsTyKindSig)
import GHC.Types.Basic (InlinePragma(..), OverlapMode(..), Boxity(..), TopLevelFlag(..), Origin(..), GenReason(..), DoPmc(..), RecFlag(..))
import System.IO.Unsafe (unsafePerformIO)
import Text.Pretty.Simple (pShow)
import qualified Data.Text.Lazy as TL
import GHC.LanguageExtensions.Type (Extension(..))
import Control.Applicative
import GHC.Builtin.Types (isCTupleTyConName, cTupleTyConNameArity_maybe)

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
deriving instance Show a => Show (AnnSortKey a)
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
deriving instance Show a => Show (DataDefnCons a)
deriving instance Show a => Show (HsScaled GhcPs a)
deriving instance Show AnnConDeclGADT
deriving instance Show AnnConDeclH98
deriving instance Show (HsConDeclGADTDetails GhcPs)
deriving instance Show (ConDecl GhcPs)
deriving instance Show (ParStmtBlock GhcPs GhcPs)
deriving instance Show (StmtLR GhcPs GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs)))
deriving instance Show (StmtLR GhcPs GhcPs (GenLocated SrcSpanAnnA (HsCmd GhcPs)))
deriving instance Show (HsForAllTelescope GhcPs)
deriving instance Show (HsUntypedSplice GhcPs)
deriving instance Show a => Show (WithHsDocIdentifiers a GhcPs)
deriving instance Show a => Show (HsArrowOf a GhcPs)
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
deriving instance Show a => Show (HsStmtContext a)
deriving instance (Show a, Show b) => Show (HsFieldBind a b)
deriving instance (Show a, Show b) => Show (BracketAnn a b)
deriving instance Show OverLitVal
deriving instance Show (HsOverLit GhcPs)
deriving instance Show (HsLit GhcPs)
deriving instance Show (HsTupArg GhcPs)
deriving instance Show a => Show (HsRecFields GhcPs a)
deriving instance Show RecFieldsDotDot
deriving instance Show a => Show (AnnList a)
deriving instance Show a => Show (HsMatchContext a)
deriving instance Show GrhsAnn
deriving instance Show a => Show (GRHS GhcPs a)
deriving instance (Show a, Show (Anno (GRHS GhcPs a))) => Show (GRHSs GhcPs a)
deriving instance Show Role
deriving instance (Show a, Show b) => Show (HsArg GhcPs a b)
deriving instance Show a => Show (FamEqn GhcPs a)
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
deriving instance Show a => Show (HsWildCardBndrs GhcPs a)
deriving instance Show a => Show (BooleanFormula a)
deriving instance Show FixityDirection
deriving instance Show GHC.Types.Fixity.Fixity
deriving instance Show (FixitySig GhcPs)
deriving instance Show (Branches a) => Show (CoAxiom a)
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
deriving instance Show a => Show (EpAnn a)
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
deriving instance Show a => Show (HsTyVarBndr a GhcPs)
deriving instance Show a => Show (HsOuterTyVarBndrs a GhcPs)
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


-- | An entry in the Hoogle DB
data Entry = EPackage PkgName
           | EModule ModName
           | EDecl (Decl ())
             deriving (Data,Typeable,Show)


fakePackage :: PkgName -> String -> (Maybe Target, [Item])
fakePackage name desc = (Just $ Target (hackagePackageURL name) Nothing Nothing "package" (renderPackage name) desc, [IPackage name])

-- | Given a file name (for errors), feed in lines to the conduit and emit either errors or items
parseHoogle :: Monad m => (String -> m ()) -> URL -> LBStr -> ConduitM i (Maybe Target, [Item]) m ()
parseHoogle warning url body = sourceLStr body .| linesCR .| zipFromC 1 .| parserC warning .| hierarchyC url .| mapC (\x -> rnf x `seq` x)

parserC :: Monad m => (String -> m ()) -> ConduitM (Int, BStr) (Target, Entry) m ()
parserC warning = f [] ""
    where
        f com url = do
            x <- await
            whenJust x $ \(i,s) -> case () of
                _ | Just s <- bstrStripPrefix "-- | " s -> f [ignoreMath s] url
                  | Just s <- bstrStripPrefix "--" s -> f (if null com then [] else bstrTrimStart s : com) url
                  | Just s <- bstrStripPrefix "@url " s -> f com (bstrUnpack s)
                  | bstrNull $ bstrTrimStart s -> f [] ""
                  | otherwise -> do
                        case parseLine $ fixLine $ bstrUnpack s of
                            Left y -> lift $ warning $ show i ++ ":" ++ y
                            -- only check Nothing as some items (e.g. "instance () :> Foo a")
                            -- don't roundtrip but do come out equivalent
                            Right [EDecl InfixDecl{}] -> pure () -- can ignore infix constructors
                            Right xs -> forM_ xs $ \x ->
                                yield (Target url Nothing Nothing (typeItem x) (renderItem x) $ reformat $ reverse com, x) -- descendBi stringShare x)
                        f [] ""


-- See https://github.com/ndmitchell/hoogle/issues/353
-- for functions like `tail` which start <math>.
ignoreMath :: BStr -> BStr
ignoreMath x | Just x <- "&lt;math&gt;" `bstrStripPrefix` x
             = fromMaybe x $ ". " `bstrStripPrefix` x
ignoreMath x = x


typeItem (EPackage x) = "package"
typeItem (EModule x) = "module"
typeItem _ = ""


-- FIXME: used to be in two different modules, now does and then undoes lots of stuff
reformat :: [BStr] -> String
reformat = unlines . map bstrUnpack


hierarchyC :: Monad m => URL -> ConduitM (Target, Entry) (Maybe Target, [Item]) m ()
hierarchyC packageUrl = void $ mapAccumC f (Nothing, Nothing)
    where
        f (pkg, mod) (t, EPackage x) = ((Just (strUnpack x, url), Nothing), (Just t{targetURL=url}, [IPackage x]))
            where url = targetURL t `orIfNull` packageUrl
        f (pkg, mod) (t, EModule x) = ((pkg, Just (strUnpack x, url)), (Just t{targetPackage=pkg, targetURL=url}, [IModule x]))
            where url = targetURL t `orIfNull` (if isGhc then ghcModuleURL x else hackageModuleURL x)
        f (pkg, mod) (t, EDecl i@InstDecl{}) = ((pkg, mod), (Nothing, hseToItem_ i))
        f (pkg, mod) (t, EDecl x) = ((pkg, mod), (Just t{targetPackage=pkg, targetModule=mod, targetURL=url}, hseToItem_ x))
            where url = targetURL t `orIfNull` case x of
                            _ | [n] <- declNames x -> hackageDeclURL (isTypeSig x) n
                              | otherwise -> ""

        isGhc = "~ghc" `isInfixOf` packageUrl || "/" `isSuffixOf` packageUrl

        hseToItem_ x = hseToItem x `orIfNull` error ("hseToItem failed, " ++ pretty x)
        infix 1 `orIfNull`
        orIfNull x y = if null x then y else x


renderPackage x = "<b>package</b> <span class=name><s0>" ++ escapeHTML (strUnpack x) ++ "</s0></span>"
renderModule (breakEnd (== '.') . strUnpack -> (pre,post)) = "<b>module</b> " ++ escapeHTML pre ++ "<span class=name><s0>" ++ escapeHTML post ++ "</s0></span>"


renderItem :: Entry -> String
renderItem = keyword . focus
    where
        keyword x | Just b <- stripPrefix "type family " x = "<b>type family</b> " ++ b
                  | (a,b) <- word1 x, a `elem` kws = "<b>" ++ a ++ "</b> " ++ b
                  | otherwise = x
            where kws = words "class data type newtype"

        name x = "<span class=name>" ++ x ++ "</span>" :: String

        focus (EModule x) = renderModule x
        focus (EPackage x) = renderPackage x
        focus (EDecl x) | [now] <- declNames x, (pre,stripPrefix now -> Just post) <- breakOn now $ pretty x =
            if "(" `isSuffixOf` pre && ")" `isPrefixOf` post then
                init (escapeHTML pre) ++ name ("(" ++ highlight now ++ ")") ++ escapeHTML (tailErr post)
            else
                escapeHTML pre ++ name (highlight now) ++ escapeHTML post
        focus (EDecl x) = pretty x

        highlight :: String -> String
        highlight x = "<s0>" ++ escapeHTML x ++ "</s0>"


parseLine :: String -> Either String [Entry]
parseLine x@('@':str) = case a of
        "package" | [b] <- words b, b /= "" -> Right [EPackage $ strPack b]
        "version" -> Right []
        _ -> Left $ "unknown attribute: " ++ x
    where (a,b) = word1 str
parseLine (stripPrefix "module " -> Just x) = Right [EModule $ strPack x]
parseLine x | Just x <- readItem x = case x of
    HSE.TypeSig a bs c -> Right [EDecl (HSE.TypeSig a [b] c) | b <- bs]
    x -> Right [EDecl x]
parseLine x = Left $ "failed to parse: " ++ x


fixLine :: String -> String
fixLine (stripPrefix "instance [incoherent] " -> Just x) = fixLine $ "instance " ++ x
fixLine (stripPrefix "instance [overlap ok] " -> Just x) = fixLine $ "instance " ++ x
fixLine (stripPrefix "instance [overlapping] " -> Just x) = fixLine $ "instance " ++ x
fixLine (stripPrefix "instance [safe] " -> Just x) = fixLine $ "instance " ++ x
fixLine (stripPrefix "(#) " -> Just x) = "( # ) " ++ x
fixLine ('[':x:xs) | isAlpha x || x `elem` ("_(" :: String), (a,']':b) <- break (== ']') xs = x : a ++ b
fixLine ('[':':':xs) | (a,']':b) <- break (== ']') xs = "(:" ++ a ++ ")" ++ b
fixLine x | "class " `isPrefixOf` x = fst $ breakOn " where " x
fixLine x = x


readItem :: String -> Maybe (Decl ())
readItem x | ParseOk y <- myParseDecl x = Just $ unGADT y
readItem x -- newtype
    | Just x <- stripPrefix "newtype " x
    , ParseOk (HSE.DataDecl an _ b c d e) <- fmap unGADT $ myParseDecl $ "data " ++ x
    = Just $ HSE.DataDecl an (NewType ()) b c d e
readItem x -- constructors
    | ParseOk (GDataDecl _ _ _ _ _ [GadtDecl s name _ _ _ ty] _) <- myParseDecl $ "data Data where " ++ x
    , let f (TyBang _ _ _ (TyParen _ x@TyApp{})) = x
          f (TyBang _ _ _ x) = x
          f x = x
    = Just $ HSE.TypeSig s [name] $ applyFun1 $ map f $ unapplyFun ty
readItem ('(':xs) -- tuple constructors
    | (com,')':rest) <- span (== ',') xs
    , ParseOk (HSE.TypeSig s [Ident{}] ty) <- myParseDecl $ replicate (length com + 2) 'a' ++ rest
    = Just $ HSE.TypeSig s [Ident s $ '(':com++")"] ty
readItem (stripPrefix "data (" -> Just xs)  -- tuple data type
    | (com,')':rest) <- span (== ',') xs
    , ParseOk (HSE.DataDecl a b c d e f) <- fmap unGADT $ myParseDecl $
        "data " ++ replicate (length com + 2) 'A' ++ rest
    = Just $ HSE.DataDecl a b c (transform (op $ '(':com++")") d) e f
    where op s DHead{} = DHead () $ Ident () s
          op s x = x
readItem _ = Nothing

myParseDecl :: String -> HSE.ParseResult (Decl ())
myParseDecl xs = unsafePerformIO $ do
    let old = myParseDeclOld xs
        new = myParseDeclNew xs
    evalNew <- try $ evaluate $ new == new
    case evalNew of
        Left (err :: SomeException) -> error $ "Parsing\n  " ++ xs ++ "\nExpected:\n  " ++ TL.unpack (pShow old)  ++ "\nGot:\n  " ++ TL.unpack (pShow err)
        Right{} -> case (old, new) of
            (ParseFailed{}, ParseFailed{}) -> pure old
            (ParseFailed{}, ParseOk{}) -> pure new
            (ParseOk{}, ParseFailed{}) -> error $ "Parsing\n  " ++ xs ++ "\nExpected:\n  " ++ TL.unpack (pShow old)  ++ "\nGot:\n  " ++ TL.unpack (pShow new)
            (ParseOk old', ParseOk new')
              | stripOuterForall old' == new'
              || any (`isPrefixOf` xs)
              [ "outputLength"
              , "blockLength"
              , "searchSources"
              , "grepFind"
              , "_assoc"
              , "_base"
              , "(@~?)"
              , "(@?~)"
              , "(~~?)"
              , "(~?~)"
              , "(~?~)"
              , "class (Typeable * e,"
              , "data Data where Readable"
              , "data ( f :+: g ) w"
              , "class (Functor sub, Functor sup) => sub :<: sup"
              , "type family (f :: m (a ~> b)) <*> (ma :: m a) :: m b"
              , "type family (f :: (a ~> b)) <$> (ma :: m a) :: m b"
              , "type a ~> b = TyFun a b -> Type"
              , "data expectation1 -/- expectation2"
              , "instance forall shouldntBe expected actual a (a0 :: actual) (a1 :: expected) (b :: shouldntBe). (a Data.Type.Equality.~ Test.TypeSpec.ShouldBe.ShouldBe a0 a1) => Test.TypeSpec.Core.PrettyTypeSpec (Test.TypeSpec.ShouldBe.ButNot a b)"
              , "type expectation1 -* expectation2 = expectation1 -/- expectation2"
              , "type expectation1 -*- expectation2 = expectation1 -/- expectation2"
              , "instance (Data.String.IsString t, GHC.Classes.Eq t, a Data.Type.Equality.~ t) => Data.String.IsString (Text.Earley.Grammar.Prod r e t a)"
              ]
               -> pure old
              | otherwise -> error $ "Parsing\n  " ++ xs ++ "\nExpected:\n  " ++ TL.unpack (pShow old')  ++ "\nGot:\n  " ++ TL.unpack (pShow new')

stripOuterForall :: Decl () -> Decl ()
stripOuterForall = \case
    HSE.TypeSig () i (TyForall () Just{} Nothing x) ->
        HSE.TypeSig () i x
    HSE.TypeSig () i (TyForall () Just{} (Just c) x) ->
        HSE.TypeSig () i (TyForall () Nothing (Just c) x)
    decl -> decl

myParseDeclOld :: String -> HSE.ParseResult (Decl ())
myParseDeclOld = fmap (fmap $ const ()) . parseDeclWithMode parseMode -- partial application, to share the initialisation cost


myParseDeclNew :: String -> HSE.ParseResult (Decl ())
myParseDeclNew str = case runGhcLibParser str of
    POk _state x -> ParseOk (hsDeclToDecl $ unLoc x)
    PFailed _state -> ParseFailed HSE.noLoc str

hsDeclToDecl :: HsDecl GhcPs -> Decl ()
hsDeclToDecl (TyClD _ (SynDecl { tcdLName, tcdTyVars = HsQTvs { hsq_explicit }, tcdRhs })) =
    TypeDecl
        ()
        (foldl' (\acc (L _ tv) -> DHApp () acc (hsTyVarBndrToTyVarBind tv)) (DHead () $ rdrNameToName $ unLoc tcdLName) hsq_explicit)
        (hsTypeToType $ unLoc tcdRhs)
hsDeclToDecl (TyClD _ (GHC.Hs.DataDecl { tcdLName, tcdTyVars = HsQTvs { hsq_explicit }, tcdDataDefn = HsDataDefn { dd_cons = DataTypeCons _ [], dd_ctxt, dd_kindSig = Nothing } } )) =
    HSE.DataDecl
        ()
        (DataType ())
        (fmap (hsTypesToContext . unLoc) dd_ctxt)
        (foldl' (\acc (L _ tv) -> DHApp () acc (hsTyVarBndrToTyVarBind tv)) (DHead () $ rdrNameToName $ unLoc tcdLName) hsq_explicit)
        []
        []
hsDeclToDecl (TyClD _ (GHC.Hs.DataDecl { tcdLName, tcdTyVars = HsQTvs { hsq_explicit }, tcdDataDefn = HsDataDefn { dd_cons = DataTypeCons _ [], dd_kindSig = Just kind } } )) =
    HSE.GDataDecl
        ()
        (DataType ())
        Nothing
        (foldl' (\acc (L _ tv) -> DHApp () acc (hsTyVarBndrToTyVarBind tv)) (DHead () $ rdrNameToName $ unLoc tcdLName) hsq_explicit)
        (Just $ hsTypeToType $ unLoc kind)
        []
        []
hsDeclToDecl (TyClD _ (GHC.Hs.DataDecl { tcdLName, tcdTyVars = HsQTvs { hsq_explicit }, tcdDataDefn = HsDataDefn { dd_cons = DataTypeCons _ [ L _ (ConDeclGADT { con_names, con_bndrs, con_g_args = PrefixConGADT _ args, con_res_ty, con_mb_cxt }) ] } } )) =
    HSE.GDataDecl
        ()
        (DataType ())
        Nothing
        (foldl' (\acc (L _ tv) -> DHApp () acc (hsTyVarBndrToTyVarBind tv)) (DHead () $ rdrNameToName $ unLoc tcdLName) hsq_explicit)
        Nothing
        (map (\con_name -> GadtDecl
            ()
            (rdrNameToName $ unLoc con_name)
            Nothing
            Nothing
            Nothing
            (
                maybe id (TyForall () Nothing . Just . hsTypesToContext . unLoc) con_mb_cxt $

                maybe id (\bs -> TyForall () (Just bs) Nothing) (hsOuterTyVarBndrsToFoo $ unLoc con_bndrs) $
                foldr (\(HsScaled _ a) -> TyFun () (hsTypeToType $ unLoc a))
                (hsTypeToType $ unLoc con_res_ty) $
                args
            )
            ) (NE.toList con_names))
        []
hsDeclToDecl (TyClD _ (FamDecl { tcdFam = FamilyDecl { fdLName, fdInfo = DataFamily, fdTyVars = HsQTvs { hsq_explicit }, fdResultSig } })) =
    DataFamDecl
        ()
        Nothing
        (foldl' (\acc (L _ tv) -> DHApp () acc (hsTyVarBndrToTyVarBind tv)) (DHead () $ rdrNameToName $ unLoc fdLName) hsq_explicit)
        (familyResultSigToResultSig $ unLoc fdResultSig)
hsDeclToDecl (TyClD _ (FamDecl { tcdFam = FamilyDecl { fdLName, fdTyVars = HsQTvs { hsq_explicit }, fdResultSig } })) =
    TypeFamDecl
        ()
        (foldl' (\acc (L _ tv) -> DHApp () acc (hsTyVarBndrToTyVarBind tv)) (DHead () $ rdrNameToName $ unLoc fdLName) hsq_explicit)
        (familyResultSigToResultSig $ unLoc fdResultSig)
        Nothing
hsDeclToDecl (TyClD _ (GHC.Hs.ClassDecl { tcdCtxt, tcdLName, tcdTyVars = HsQTvs { hsq_explicit }, tcdFDs })) =
    HSE.ClassDecl
        ()
        (fmap (hsTypesToContext . unLoc) tcdCtxt)
        (foldl' (\acc (L _ tv) -> DHApp () acc (hsTyVarBndrToTyVarBind tv)) (DHead () $ rdrNameToName $ unLoc tcdLName) hsq_explicit)
        (map (funDepToFunDep . unLoc) tcdFDs)
        Nothing

hsDeclToDecl (SigD _ (GHC.Hs.TypeSig _ names (HsWC { hswc_body = L _ HsSig { sig_body } }))) =
    HSE.TypeSig
        ()
        (map (rdrNameToName . unLoc) names)
        (hsTypeToType $ unLoc sig_body)
hsDeclToDecl (SigD _ (GHC.Hs.PatSynSig _ names (L _ HsSig { sig_body } ))) =
    HSE.PatSynSig
        ()
        (map (rdrNameToName . unLoc) names)
        Nothing
        Nothing
        Nothing
        Nothing
        (hsTypeToType $ unLoc sig_body)
hsDeclToDecl (SigD _ (FixSig _ (FixitySig _ names (Fixity priority direction)))) =
    InfixDecl
        ()
        (fixityDirectionToAssoc direction)
        (Just priority)
        (map (varOpOrConOp . rdrNameToName . unLoc) names)

hsDeclToDecl (InstD _ (ClsInstD {cid_inst = ClsInstDecl { cid_poly_ty = (L _ HsSig { sig_bndrs, sig_body }) } })) = case hsTypeToType (unLoc sig_body) of
    TyForall () Nothing ctxt body ->
        InstDecl
            ()
            Nothing
            (IRule () (hsOuterTyVarBndrsToFoo sig_bndrs) ctxt (typeToInstHead body))
            Nothing
    body ->
        InstDecl
            ()
            Nothing
            (IRule () (hsOuterTyVarBndrsToFoo sig_bndrs) Nothing (typeToInstHead body))
            Nothing
hsDeclToDecl hsDecl = error $ show hsDecl

varOpOrConOp :: HSE.Name () -> Op ()
varOpOrConOp name = case name of
    HSE.Symbol () (':' : _) -> ConOp () name
    _ -> VarOp () name

familyResultSigToResultSig :: FamilyResultSig GhcPs -> Maybe (ResultSig ())
familyResultSigToResultSig = \case
    NoSig{} -> Nothing
    GHC.Hs.KindSig _ kind -> Just $ HSE.KindSig () $ hsTypeToType $ unLoc kind
    sig -> error $ show sig

hsOuterTyVarBndrsToFoo :: HsOuterSigTyVarBndrs GhcPs -> Maybe [TyVarBind ()]
hsOuterTyVarBndrsToFoo = \case
    HsOuterImplicit{} -> Nothing
    HsOuterExplicit {hso_bndrs} -> Just $ map (hsTyVarBndrToTyVarBind . unLoc) hso_bndrs

funDepToFunDep :: GHC.Hs.FunDep GhcPs -> HSE.FunDep ()
funDepToFunDep = \case
    GHC.Hs.FunDep _ lhs rhs ->
        HSE.FunDep () (map (rdrNameToName . unLoc) lhs) (map (rdrNameToName . unLoc) rhs)

fixityDirectionToAssoc :: FixityDirection -> Assoc ()
fixityDirectionToAssoc = \case
    InfixL -> AssocLeft ()
    InfixR -> AssocRight ()
    InfixN -> AssocNone ()

hsTyVarBndrToTyVarBind
    :: Show a
    => HsTyVarBndr a GhcPs
    -> HSE.TyVarBind ()
hsTyVarBndrToTyVarBind = \case
    HsTvb _ _ (HsBndrVar _ (L _ var)) HsBndrNoKind{} ->
        UnkindedVar () (rdrNameToName var)
    HsTvb _ _ (HsBndrVar _ (L _ var)) (HsBndrKind _ (L _ kind)) ->
        KindedVar () (rdrNameToName var) (hsTypeToType kind)
    tv -> error $ show tv

occNameToName :: OccName -> HSE.Name ()
occNameToName occ = case occNameString occ of
    xs@(x : _) | not (isAlphaNum x) && x /= '(' && x /= '_' -> Symbol () xs
    xs -> Ident () xs

rdrNameToName :: RdrName -> HSE.Name ()
rdrNameToName = occNameToName . rdrNameOcc

rdrNameToQName :: RdrName -> HSE.QName ()
rdrNameToQName = \case
    Unqual occName ->
        HSE.UnQual () $ occNameToName occName
    GHC.Types.Name.Reader.Qual modName occName ->
        HSE.Qual () (HSE.ModuleName () (moduleNameString modName)) (occNameToName occName)
    Orig mod occName ->
        HSE.Qual () (HSE.ModuleName () (moduleNameString $ moduleName mod)) (occNameToName occName)
    Exact name ->
        HSE.UnQual () $ occNameToName (nameOccName name)

hsTypeToType :: HsType GhcPs -> HSE.Type ()
hsTypeToType = \case
    HsListTy _ x ->
        TyList () $ hsTypeToType $ unLoc x
    HsTyVar _ _ (L _ (Exact x))
        | show x == "[]" ->
        TyCon () $ Special () $ ListCon ()
        | show x == "->" ->
        TyCon () $ Special () $ FunCon ()
        | Just n <- stripPrefix "Tuple" (occNameString (nameOccName x))
        , Just n' <- readMay n ->
        TyCon () $ Special () $ TupleCon () HSE.Boxed n'
    HsTyVar _ IsPromoted (L _ x) ->
        TyPromoted () $ HSE.PromotedCon () True $ rdrNameToQName x
    HsTyVar _ NotPromoted (L _ x) ->
        case rdrNameSpace x of
            ns
                | ns == tvName ->
                TyVar () $ rdrNameToName x
            _ -> TyCon () $ rdrNameToQName x
    HsAppTy _ x y ->
        TyApp () (hsTypeToType $ unLoc x) (hsTypeToType $ unLoc y)
    HsFunTy _ _ x y ->
        TyFun () (hsTypeToType $ unLoc x) (hsTypeToType $ unLoc y)
    HsTupleTy _ HsBoxedOrConstraintTuple [] ->
        TyCon () $ Special () $ UnitCon ()
    HsTupleTy _ boxed xs ->
        TyTuple () (hsTupleSortToBoxed boxed) (map (hsTypeToType . unLoc) xs)
    HsStarTy _ _ ->
        TyStar ()
    HsBangTy _ (HsBang unpackedness strictness) x ->
        TyBang () (srcStrictnessToBangType strictness) (srcUnpackednessToUnpackedness unpackedness) (hsTypeToType $ unLoc x)
    HsParTy _ x -> case hsTypeToType (unLoc x) of
        x'@TyKind{} -> x'
        x' -> TyParen () x'
    HsQualTy { hst_ctxt, hst_body } ->
        TyForall () Nothing (Just $ hsTypesToContext $ unLoc hst_ctxt) (hsTypeToType $ unLoc hst_body)
    HsForAllTy { hst_tele = HsForAllInvis { hsf_invis_bndrs }, hst_body } ->
        case hsTypeToType (unLoc hst_body) of
            TyForall () Nothing (Just ctxt) body ->
                TyForall () (Just $ map (hsTyVarBndrToTyVarBind . unLoc) hsf_invis_bndrs) (Just ctxt) body
            body ->
                TyForall () (Just $ map (hsTyVarBndrToTyVarBind . unLoc) hsf_invis_bndrs) Nothing body

    HsExplicitListTy _ IsPromoted xs ->
        TyPromoted () $ PromotedList () True (map (hsTypeToType . unLoc) xs)
    HsExplicitListTy _ NotPromoted [x] ->
        TyList () $ hsTypeToType $ unLoc x

    HsExplicitTupleTy _ IsPromoted [] ->
        TyPromoted () $ PromotedCon () True $ Special () $ UnitCon ()
    HsExplicitTupleTy _ IsPromoted xs ->
        TyPromoted () $ PromotedTuple () $ (map (hsTypeToType . unLoc) xs)
    HsExplicitTupleTy _ NotPromoted xs ->
        TyTuple () HSE.Boxed (map (hsTypeToType . unLoc) xs)

    HsOpTy _ _ x (L _ (Unqual y)) z
        | occNameString y == "~" ->
        TyEquals () (hsTypeToType $ unLoc x) (hsTypeToType $ unLoc z)
    HsOpTy _ _ x (L _ (Exact y)) z
        | occNameString (nameOccName y) == ":" ->
        TyInfix () (hsTypeToType $ unLoc x) (UnpromotedName () $ Special () $ Cons ()) (hsTypeToType $ unLoc z)
    HsOpTy _ _ x y z ->
        TyInfix () (hsTypeToType $ unLoc x) (UnpromotedName () $ rdrNameToQName $ unLoc y) (hsTypeToType $ unLoc z)
    HsKindSig _ lhs rhs ->
        TyKind () (hsTypeToType $ unLoc lhs) (hsTypeToType $ unLoc rhs)
    HsTyLit _ (HsNumTy (SourceText txt) val) ->
        TyPromoted () $ PromotedInteger () val (unpackFS txt)
    HsTyLit _ (HsStrTy (SourceText txt) val) ->
        TyPromoted () $ PromotedString () (unpackFS val) (drop 1 $ dropEnd 1 $ unpackFS txt)
    HsIParamTy _ _name ty ->
        -- FIXME when migrating to ghc-lib-parser completely:
        -- HSE does not quite support ImplicitParams in ContraintKinds
        hsTypeToType $ unLoc ty
    ty ->
        error $ show ty

typeToInstHead :: HSE.Type () -> HSE.InstHead ()
typeToInstHead = \case
    TyApp () x y -> HSE.IHApp () (typeToInstHead x) y
    TyCon () x -> HSE.IHCon () x
    TyInfix () x (UnpromotedName () y) z -> HSE.IHApp () (HSE.IHInfix () x y) z
    ty -> error $ "typeToInstHead:\n" ++ show ty

hsTypesToContext
    :: [GenLocated SrcSpanAnnA (HsType GhcPs)]
    -> HSE.Context ()
hsTypesToContext = \case
    [] -> HSE.CxEmpty ()
    [x] -> HSE.CxSingle () $ applyTypeA $ hsTypeToType $ unLoc x
    xs -> HSE.CxTuple () $ map (applyTypeA . hsTypeToType . unLoc) xs

applyTypeA :: HSE.Type () -> Asst ()
applyTypeA = \case
    TyParen () ty -> HSE.ParenA () $ HSE.TypeA () ty
    ty -> HSE.TypeA () ty

hsTupleSortToBoxed :: HsTupleSort -> Boxed
hsTupleSortToBoxed = \case
    HsUnboxedTuple -> HSE.Unboxed
    HsBoxedOrConstraintTuple -> HSE.Boxed

srcStrictnessToBangType :: SrcStrictness -> HSE.BangType ()
srcStrictnessToBangType = \case
    SrcLazy -> HSE.LazyTy ()
    SrcStrict -> HSE.BangedTy ()
    NoSrcStrict -> HSE.NoStrictAnnot ()

srcUnpackednessToUnpackedness :: SrcUnpackedness -> HSE.Unpackedness ()
srcUnpackednessToUnpackedness = \case
    SrcUnpack -> HSE.Unpack ()
    SrcNoUnpack -> HSE.NoUnpack ()
    NoSrcUnpack -> HSE.NoUnpackPragma ()

runGhcLibParser
    :: String
    -> GHC.Parser.Lexer.ParseResult (GenLocated SrcSpanAnnA (HsDecl GhcPs))
runGhcLibParser str
    | Just (str', ';') <- unsnoc str
    = runGhcLibParser str'
runGhcLibParser str = case runGhcLibParserEx allExtensions str of
    res@POk{} -> res
    PFailed{} -> runGhcLibParserEx noUnboxed str
    where
        allExtensions = EnumSet.fromList [minBound..maxBound]
        almostAllExtensions =
            EnumSet.delete Arrows $
            EnumSet.delete StaticPointers $
            EnumSet.delete TransformListComp $
            allExtensions
        noUnboxed =
            EnumSet.delete UnboxedSums $
            EnumSet.delete UnboxedTuples $
            almostAllExtensions

runGhcLibParserEx
    :: EnumSet.EnumSet Extension
    -> String
    -> GHC.Parser.Lexer.ParseResult (GenLocated SrcSpanAnnA (HsDecl GhcPs))
runGhcLibParserEx extensions str = unP parseDeclaration parseState
    where
        opts = mkParserOpts extensions emptyDiagOpts [] False False False False
        filename = "<interactive>"
        location = mkRealSrcLoc (mkFastString filename) 1 1
        buffer = stringToStringBuffer str
        parseState = initParserState opts buffer location

unGADT (GDataDecl a b c d _  [] e) = HSE.DataDecl a b c d [] e
unGADT x = x

prettyItem :: Entry -> String
prettyItem (EPackage x) = "package " ++ strUnpack x
prettyItem (EModule x) = "module " ++ strUnpack x
prettyItem (EDecl x) = pretty x


input_haddock_test :: IO ()
input_haddock_test = testing "Input.Haddock.parseLine" $ do
    let a === b | fmap (map prettyItem) (parseLine a) == Right [b] = putChar '.'
                | otherwise = errorIO $ show (a,b,parseLine a, fmap (map prettyItem) $ parseLine a)
    let test a = a === a
    test "type FilePath = [Char]"
    test "data Maybe a"
    test "Nothing :: Maybe a"
    test "Just :: a -> Maybe a"
    test "newtype Identity a"
    test "foo :: Int# -> b"
    test "(,,) :: a -> b -> c -> (a, b, c)"
    "data (,,) a b" === "data Tuple3 a b"
    test "reverse :: [a] -> [a]"
    -- Parallel Haskell has never been implemented
    -- test "reverse :: [:a:] -> [:a:]"
    test "module Foo.Bar"
    test "data Char"
    "data Char :: *" === "data Char"
    "newtype ModuleName :: *" === "newtype ModuleName"
    "Progress :: !(Maybe String) -> {-# UNPACK #-} !Int -> !(Int -> Bool) -> Progress" ===
        "Progress :: Maybe String -> Int -> (Int -> Bool) -> Progress"
    test "quotRemInt# :: Int# -> Int# -> (# Int#, Int# #)"
    test "( # ) :: Int"
    test "pattern MyPattern :: ()"
    test "degrees :: Floating x => Radians x -> Degrees x"
    test "class Angle a"
    test "instance Eq x => Eq (Degrees x)"
    test "instance Angle Degrees"
    test "type Queue a = Deque Nonthreadsafe Nonthreadsafe SingleEnd SingleEnd Grow Safe a"
    test "class DequeClass d => PopL d"
    test "tests_fifo :: DequeClass d => (forall elt . IO (d elt)) -> Test"
    test "class ParUnsafe iv p | p -> iv"
    "(##) :: Diagram -> Diagram -> Diagram" === "( ## ) :: Diagram -> Diagram -> Diagram"
    test "instance LayoutClass Positioned []"
    test "data Ord a => Range a"
    test "aPair :: Proxy (,)"
    test "aTriple :: Proxy (,,)"
    test "qop :: (Ord a, Show qtyp, Show (QFlipTyp qtyp), QFlipTyp (QFlipTyp qtyp) ~ qtyp) => Set (QueryRep QAtomTyp a) -> Set (QueryRep (QFlipTyp qtyp) a) -> QueryRep qtyp a"
    test "reorient :: (Unbox a) => Bernsteinp Int a -> Bernsteinp Int a"
    "type family PrimM a :: * -> *;" === "type family PrimM a :: * -> *"
    test "HSNil :: HSet '[]"
    "HSCons :: !elem -> HSet elems -> HSet (elem : elems)" === "HSCons :: elem -> HSet elems -> HSet (elem : elems)"
    test "instance Data.HSet.Reverse.HReverse '[e] els1 els2 => Data.HSet.Reverse.HReverse '[] (e : els1) els2"
    test "instance Data.HSet.Remove.HRemove (e : els) els 'TypeFun.Data.Peano.Z"
    test "Free :: (forall m . Monad m => Effects effects m -> m a) -> Free effects a"
    test "infixl 3 <||"
    test "instance Data.String.IsString t => Data.String.IsString (t Yi.MiniBuffer.::: doc)"
    test "runValueExpression :: (Functor f) => Expression a ((->) b) f r -> f ((a -> b) -> r)"
    test "HCons :: (x :: *) -> HList xs -> HList (x : xs)"
    -- Cannot faithfully represent ConstraintKind with ImplicitParams in HSE
    -- test "type HasCallStack = ?callStack :: CallStack"
    test "instance forall k (key :: k) . Data.Traversable.Traversable (Data.ComposableAssociation.Association key)"
    test "ReflH :: forall (k :: *) (t :: k) . HetEq t t"
    test "egcd :: (PID d, (Euclidean d)) => d -> d -> (d, d, d)"
    test "proc :: FilePath -> [String] -> CreateProcess"
    test "unitTests :: Proxy '()"
    test "type OneToFour = '[1, 2, 3, 4]"
    test "data family Prio pol item :: *"
    test "set :: (Monad m, ToByteString a) => Key -> a -> Opts \"SET\" -> Redis m Bool"
    test "by :: ByteString -> Opts \"SORT\""
    test "infixr 9 :+:"
    test "instance forall k1 k2 (expectation1 :: k2) (expectation2 :: k1) . (Test.TypeSpec.Core.PrettyTypeSpec expectation1, Test.TypeSpec.Core.PrettyTypeSpec expectation2) => Test.TypeSpec.Core.PrettyTypeSpec '(expectation1, expectation2)"
    test "SomeFoo :: Foo a => m a -> SomeFoo m"
