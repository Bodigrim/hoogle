{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections, OverloadedStrings, Rank2Types, DeriveDataTypeable, ImportQualifiedPost #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-deriving-typeable #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Input.Haddock(parseHoogle, fakePackage, input_haddock_test) where

import Language.Haskell.Exts as HSE (Decl (..), ParseResult (..), GadtDecl (..), Type (..), Name (..), DeclHead (..), parseDeclWithMode, DataOrNew (..), noLoc, QName (..), ModuleName (..), TyVarBind (..), Boxed (..), Unpackedness (..), BangType (..), SpecialCon (..), Context (..), Asst (..), InstRule (..), InstHead (..), FunDep (..), ResultSig (..), Promoted (..), MaybePromotedName (..), Op (..), Assoc (..), IPName (..), FieldDecl (..), InjectivityInfo (InjectivityInfo))
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
import Input.ShowOrphans ()


import GHC.Parser
import GHC.Parser.Lexer
import GHC.Data.EnumSet qualified as EnumSet
import GHC.Data.FastString
import GHC.Data.StringBuffer
import GHC.Types.SrcLoc (GenLocated, mkRealSrcLoc, unLoc, GenLocated(L))
import GHC.Utils.Error
import GHC.Parser.Annotation
import GHC.Types.Basic (PromotionFlag(..))
import GHC.Types.Name (nameOccName, tvName)
import GHC.Types.Name.Reader (RdrName(..), rdrNameOcc, rdrNameSpace)
import GHC.Unit (GenModule(..))
import GHC.Types.Name.Occurrence (OccName(..), occNameString)
import GHC.Types.SourceText (SourceText (..))
import GHC.Hs (GhcPs, HsDecl(..), TyClDecl(..), InstDecl(..), Sig(..), HsSigType(..), HsOuterTyVarBndrs(..), HsOuterSigTyVarBndrs, HsTyVarBndr(..), HsBndrVar(..), HsType(..), HsForAllTelescope(..), HsBndrKind(..), ConDeclField(..), FieldOcc(..), HsTyLit(..), HsTupleSort(..), HsIPName(..), HsBang(..), SrcUnpackedness(..), SrcStrictness(..), HsWildCardBndrs(..), FixitySig(..), ClsInstDecl(..), ConDecl(..), HsScaled(..), FamilyInfo(..), moduleNameString, hsIPNameFS, InjectivityAnn (..), StandaloneKindSig (..))
import GHC.Hs.Basic (FixityDirection(..))
import GHC.Types.Fixity (Fixity(..))
import GHC.Hs (FamilyDecl(..), HsDataDefn(..), DataDefnCons(..), HsConDeclGADTDetails(..), LHsQTyVars(..), FunDep(..), FamilyResultSig(..))
import System.IO.Unsafe (unsafePerformIO)
import Text.Pretty.Simple (pShow)
import qualified Data.Text.Lazy as TL
import GHC.LanguageExtensions.Type (Extension(..))

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
        Left (err :: SomeException)
            | not (any (`isPrefixOf` xs)
                [
                ])
            -> error $ "Parsing\n  " ++ xs ++ "\nExpected:\n  " ++ TL.unpack (pShow old)  ++ "\nGot:\n  " ++ TL.unpack (pShow err)
            | otherwise
            -> pure old
        Right{} -> case (old, new) of
            (ParseFailed{}, ParseFailed{}) -> pure old
            (ParseFailed{}, ParseOk{}) -> pure new
            (ParseOk{}, ParseFailed{})
                | not (any (`isPrefixOf` xs)
                    [ "type FT_List"
                    , "type TransmitPacketFunction"
                    , "type ProcessPacketFunction"
                    , "type family (i :: TypeInt) * (i' :: TypeInt) :: TypeInt"
                    , "type family (a :: ExactPi') * (b :: ExactPi') :: ExactPi'"
                    , "type family (a :: Natural) * (b :: Natural) :: Natural"
                    , "instance Control.Monad.Reader.Has.GHas 'Data.Path.Here rec (GHC.Generics.K1 i rec)"
                    , "instance Control.Monad.Except.CoHas.GCoHas 'Data.Path.Here rec (GHC.Generics.K1 i rec)"
                    , "data ( (a :: Nat) * (b :: Nat) ) (c :: Nat)"
                    , "type family (a :: Dimension) * (b :: Dimension)"
                    , "type family (v1 :: Variant) * (v2 :: Variant) :: Variant"
                    ])
                -> error $ "Parsing\n  " ++ xs ++ "\nExpected:\n  " ++ TL.unpack (pShow old)  ++ "\nGot:\n  " ++ TL.unpack (pShow new)
                | otherwise
                -> pure old
            (ParseOk old', ParseOk new')
              | flipDHInfix (stripOuterForall old') == new'
              || "Data.Type.Equality.~" `isInfixOf` xs
              || "GHC.Types.~" `isInfixOf` xs
              || "GHC.Internal.Types.~" `isInfixOf` xs
              || "%1 ->" `isInfixOf` xs
              || " * " `isInfixOf` xs
              || "(*)" `isInfixOf` xs
              || "a69895866216" `isInfixOf` xs
              || any ((`isPrefixOf` xs) . dropWhile isSpace)
              [ "outputLength"
              , "blockLength"
              , "searchSources"
              , "grepFind"
              , "_assoc"
              , "_base"
              , "data ( f :+: g ) w"
              , "class (Functor sub, Functor sup) => sub :<: sup"
              , "type family (f :: m (a ~> b)) <*> (ma :: m a) :: m b"
              , "type family (f :: (a ~> b)) <$> (ma :: m a) :: m b"
              , "type a ~> b = TyFun a b -> Type"
              , "data expectation1 -/- expectation2"
              , "type expectation1 -* expectation2 = expectation1 -/- expectation2"
              , "type expectation1 -*- expectation2 = expectation1 -/- expectation2"
              , "data a :-> c"
              , "class (f x, g x) => ( f `And` g ) x"
              , "class (f (g x)) => ( f `Compose` g ) x"
              , "data ( f -.-> g ) a"
              , "data () => ( (f :: k -> Type) :+: (g :: k -> Type) ) (p :: k)"
              , "data () => ( (f :: k -> Type) :*: (g :: k -> Type) ) (p :: k)"

              , "class a ~R# b => Coercible (a :: k) (b :: k)"
              , "data ( (f :: k -> Type) -.-> (g :: k -> Type) ) (a :: k)"
              , "data ( (f :: l -> Type) :.: (g :: k -> l) ) (p :: k)"
              , "data ( f :.: g ) x"
              , "data ( (c :: a -> Exp b) =<< (d :: Exp a) ) (e :: b)"
              , "data ( (c :: Exp a) >>= (d :: a -> Exp b) ) (e :: b)"
              , "data ( (d :: b -> Exp c) <=< (e :: a -> Exp b) ) (f :: a) (g :: c)"
              , "data ( (c :: a -> b) <$> (d :: Exp a) ) (e :: b)"
              , "data ( (c :: Exp a -> b) <*> (d :: Exp a) ) (e :: b)"
              , "data ( (c :: a -> Exp b) $ (d :: a) ) (e :: b)"
              , "data ( (b :: a) .<> (c :: a) ) (d :: a)"
              , "data ( (a :: Bool) || (b :: Bool) ) (c :: Bool)"
              , "data ( (a :: Bool) && (b :: Bool) ) (c :: Bool)"
              , "data ( (a :: b -> Exp c) *** (d :: b' -> Exp c') ) (e :: (b, b')) (f :: (c, c'))"
              , "data ( (c :: a) & (d :: a -> Exp b) ) (e :: b)"
              , "data ( (a :: Nat) + (b :: Nat) ) (c :: Nat)"
              , "data ( (a :: Nat) - (b :: Nat) ) (c :: Nat)"
              , "data ( (a :: Nat) ^ (b :: Nat) ) (c :: Nat)"
              , "data ( (a :: Nat) <= (b :: Nat) ) (c :: Bool)"
              , "data ( (a :: Nat) < (b :: Nat) ) (c :: Bool)"
              , "data ( (a :: Nat) > (b :: Nat) ) (c :: Bool)"
              , "data ( (a :: Nat) >= (b :: Nat) ) (c :: Bool)"
              , "data ( (b :: [a]) ++ (c :: [a]) ) (d :: [a])"
              , "data ( (b :: a) <= (c :: a) ) (d :: Bool)"
              , "data ( (b :: a) < (c :: a) ) (d :: Bool)"
              , "data ( (b :: a) > (c :: a) ) (d :: Bool)"
              , "data ( (b :: a) >= (c :: a) ) (d :: Bool)"
              , "data ( (f :: a ~> b) <$> (p :: PParser a) ) (s :: FunKind PState PReply b)"
              , "data ( (l :: PParser a ~> b) <*> (r :: PParser a) ) (s :: FunKind PState PReply b)"
              , "data ( (l :: PParser a) <|> (r :: PParser a) ) (s :: FunKind PState PReply a)"
              , "data ( (l :: PParser a) >>= (r :: a ~> PParser b) ) (s :: FunKind PState PReply b)"
              , "data ( c :=>: (f :: k -> Type) ) (a :: k)"
              , "data (x0_49_0 ∷ k) :\\ (x1_49_1 ∷ k) ∷ Subst"
              , "pattern TestSeqWitness :: () => forall a. QuantifyConstraints a => a -> TestSequence s -> TestSequence s"
              , "class (c1 a, c2 a) => ( (c1 :: k -> Constraint) :&&: (c2 :: k -> Constraint) ) (a :: k)"
              , "data ( (f :: k -> Type) :*: (g :: k -> Type) ) (p :: k)"
              , "data ( (f :: k -> Type) :+: (g :: k -> Type) ) (p :: k)"
              , "data Data where Fin :: forall (m :: Nat) (a :: Nat). !Nat m -> !m < a -> Fin a"
              , "type HighIxN (n :: Natural) = (4 <= n, KnownNat n, KnownNat n - 1, Index IxN n - 1, IxN n - 1 ~ Ix n - 1)"
              , "data ( (f :: k2 -> Type) :.: (g :: k1 -> k2) ) (p :: k1)"
              , "data ( (f :: Type -> Type -> Type -> Type) :+: (g :: Type -> Type -> Type -> Type) ) (m :: Type -> Type) k"
              , "data Data where FastIdx :: !BindId |-> BindPred -> !KIndex |-> KVSub -> !KVar |-> Hyp -> !CMap IBindEnv -> !CMap [SubcId] -> !SEnv Sort -> Index"
              , "type InterpreterFor (e :: Effect) (r :: [Effect]) = forall a. () => Sem e ': r a -> Sem r a"
              , "pattern Succ :: forall n. () => forall n1. n ~ Succ n1 => SNat n1 -> SNat n"
              , "instance Data.Eq.Singletons.PEq (*)"
              , "instance Data.Singletons.Decide.SDecide (*)"
              , "data ( (a6989586621679154339 :: b ~> c) .@#@$$$ (a6989586621679154340 :: a ~> b) ) (c1 :: TyFun a c)"
              , "type family ( (a6989586621679154339 :: b ~> c) .@#@$$$$ (a6989586621679154340 :: a ~> b) ) (a6989586621679154341 :: a) :: c"
              , "type family ( (a1 :: a ~> m b) >=> (a2 :: b ~> m c) ) (a3 :: a) :: m c"
              , "type family ( (a1 :: b ~> m c) <=< (a2 :: a ~> m b) ) (a3 :: a) :: m c"
              , "data ( (a6989586621680354988 :: a ~> m b) >=>@#@$$$ (a6989586621680354989 :: b ~> m c) ) (c1 :: TyFun a m c)"
              , "data ( (a6989586621680354976 :: b ~> m c) <=<@#@$$$ (a6989586621680354977 :: a ~> m b) ) (c1 :: TyFun a m c)"
              , "pattern Fold1_ :: forall a b. forall x. (a -> x) -> (x -> a -> x) -> (x -> b) -> Fold1 a b"
              , "data ( f :+: g ) e"
              , "type family (a :: Dimension) * (b :: Dimension)"
              , "pattern (:<) :: forall (f :: Type -> Type) a (n :: Nat). (Dom f a, KnownNat n, CFreeMonoid f) => forall (n1 :: Nat). (n ~ (1 + n1), KnownNat n1) => a -> Sized f n1 a -> Sized f n a"
              , "pattern (:>) :: forall (f :: Type -> Type) a (n :: Nat). (Dom f a, KnownNat n, CFreeMonoid f) => forall (n1 :: Nat). (n ~ (n1 + 1), KnownNat n1) => Sized f n1 a -> a -> Sized f n a"
              , "type p --> q = forall a. Sing a -> p @@ a -> q @@ a"
              , "type ( p --># q ) h = forall a. Sing a -> p @@ a -> h (q @@ a)"
              , "type p -?> q = forall a. Sing a -> p @@ a -> Decision (q @@ a)"
              , "instance (c1"
              , "type ( p -?># q ) h = forall a. Sing a -> p @@ a -> h (Decision (q @@ a))"
              , "pattern SomeSized :: Vector v a => forall n. KnownNat n => Vector v n a -> v a"
              , "class (c a, d a) => ( c & d ) a"
              ]
               -> pure old
              | otherwise -> error $ "Parsing\n  " ++ xs ++ "\nExpected:\n  " ++ TL.unpack (pShow (flipDHInfix (stripOuterForall old')))  ++ "\nGot:\n  " ++ TL.unpack (pShow new')

flipDHInfix :: Decl () -> Decl ()
flipDHInfix = \case
    HSE.TypeDecl () x y -> HSE.TypeDecl () (go x) y
    HSE.DataDecl () x y z t u -> HSE.DataDecl () x y (go z) t u
    HSE.GDataDecl () x y z t u v -> HSE.GDataDecl () x y (go z) t u v
    HSE.ClassDecl () x y z t -> HSE.ClassDecl () x (go y) z t
    HSE.TypeFamDecl () x y z -> HSE.TypeFamDecl () (go x) y z
    decl -> decl
    where
        go :: DeclHead () -> DeclHead ()
        go = \case
            DHead () x -> DHead () x
            DHInfix () x y -> DHApp () (DHead () y) x
            DHParen () x -> DHParen () (go x)
            DHApp () x y -> DHApp () (go x) y

stripOuterForall :: Decl () -> Decl ()
stripOuterForall = \case
    HSE.TypeSig () i (TyForall () Just{} Nothing x) ->
        HSE.TypeSig () i x
    HSE.TypeSig () i (TyForall () Just{} (Just c) x) ->
        HSE.TypeSig () i (TyForall () Nothing (Just c) x)
    HSE.PatSynSig () name forall1 ctx1 forall2 ctx2 ty ->
        HSE.PatSynSig () name Nothing ctx1 Nothing ctx2 ty
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
                maybe id (\bs -> applyTyForall (Just bs) Nothing) (hsOuterTyVarBndrsToFoo $ unLoc con_bndrs) $
                maybe id (applyTyForall Nothing . Just . hsTypesToContext . unLoc) con_mb_cxt $
                foldr (\(HsScaled _ a) -> TyFun () (hsTypeToType $ unLoc a))
                (hsTypeToType $ unLoc con_res_ty)
                args
            )
            ) (NE.toList con_names))
        []
hsDeclToDecl (TyClD _ (GHC.Hs.DataDecl { tcdLName, tcdTyVars = HsQTvs { hsq_explicit }, tcdDataDefn = HsDataDefn { dd_cons = DataTypeCons _ [ L _ (ConDeclGADT { con_names, con_bndrs, con_g_args = RecConGADT _ (L _ args), con_res_ty, con_mb_cxt }) ] } } )) =
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
            (Just $ map (conDeclFieldToFieldDecl . unLoc) args)
            (
                maybe id (TyForall () Nothing . Just . hsTypesToContext . unLoc) con_mb_cxt $
                maybe id (\bs -> TyForall () (Just bs) Nothing) (hsOuterTyVarBndrsToFoo $ unLoc con_bndrs) $
                hsTypeToType $
                unLoc con_res_ty
            )
            ) (NE.toList con_names))
        []

hsDeclToDecl (TyClD _ (FamDecl { tcdFam = FamilyDecl { fdLName, fdInfo = DataFamily, fdTyVars = HsQTvs { hsq_explicit }, fdResultSig } })) =
    DataFamDecl
        ()
        Nothing
        (foldl' (\acc (L _ tv) -> DHApp () acc (hsTyVarBndrToTyVarBind tv)) (DHead () $ rdrNameToName $ unLoc fdLName) hsq_explicit)
        (familyResultSigToResultSig $ unLoc fdResultSig)
hsDeclToDecl (TyClD _ (FamDecl { tcdFam = FamilyDecl { fdLName, fdTyVars = HsQTvs { hsq_explicit }, fdResultSig, fdInjectivityAnn } })) =
    TypeFamDecl
        ()
        (foldl' (\acc (L _ tv) -> DHApp () acc (hsTyVarBndrToTyVarBind tv)) (DHead () $ rdrNameToName $ unLoc fdLName) hsq_explicit)
        (familyResultSigToResultSig $ unLoc fdResultSig)
        (fmap (injectivityAnnToInjectivityInfo . unLoc) fdInjectivityAnn)
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
    case hsTypeToType (unLoc sig_body) of
        TyForall () Nothing (Just ctx1) (TyForall () Nothing (Just ctx2) ty) ->
            HSE.PatSynSig
                ()
                (map (rdrNameToName . unLoc) names)
                Nothing
                (Just ctx1)
                Nothing
                (Just ctx2)
                ty
        TyForall () Nothing (Just ctx) ty ->
            HSE.PatSynSig
                ()
                (map (rdrNameToName . unLoc) names)
                Nothing
                (Just ctx)
                Nothing
                Nothing
                ty
        ty ->
            HSE.PatSynSig
                ()
                (map (rdrNameToName . unLoc) names)
                Nothing
                Nothing
                Nothing
                Nothing
                ty
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

-- TODO FIXME when migrating to ghc-lib-parser completely:
-- HSE does not support standalone kind signatures
hsDeclToDecl (KindSigD _ (StandaloneKindSig _ x y)) =
   HSE.GDataDecl
        ()
        (DataType ())
        Nothing
        (DHead () $ rdrNameToName $ unLoc x)
        (Just $ hsTypeToType $ unLoc $ sig_body $ unLoc y)
        []
        []

hsDeclToDecl hsDecl = error $ show hsDecl

injectivityAnnToInjectivityInfo :: InjectivityAnn GhcPs -> HSE.InjectivityInfo ()
injectivityAnnToInjectivityInfo = \case
    InjectivityAnn _ lhs rhs ->
        HSE.InjectivityInfo () (rdrNameToName $ unLoc lhs) (map (rdrNameToName . unLoc) rhs)

conDeclFieldToFieldDecl :: ConDeclField GhcPs -> HSE.FieldDecl ()
conDeclFieldToFieldDecl = \case
    ConDeclField {cd_fld_names, cd_fld_type} ->
        HSE.FieldDecl () (map (fieldOccToName . unLoc) cd_fld_names) (hsTypeToType $ unLoc cd_fld_type)

fieldOccToName :: FieldOcc GhcPs -> HSE.Name ()
fieldOccToName = \case
    FieldOcc {foLabel} -> rdrNameToName $ unLoc foLabel

varOpOrConOp :: HSE.Name () -> Op ()
varOpOrConOp name = case name of
    HSE.Symbol () (':' : _) -> ConOp () name
    HSE.Ident () (c : _)
        | isUpper c -> ConOp () name
        | otherwise -> VarOp () name
    _ -> VarOp () name

familyResultSigToResultSig :: FamilyResultSig GhcPs -> Maybe (ResultSig ())
familyResultSigToResultSig = \case
    NoSig{} -> Nothing
    GHC.Hs.KindSig _ kind ->
        Just $ HSE.KindSig () $ hsTypeToType $ unLoc kind
    GHC.Hs.TyVarSig _ tvb ->
        Just $ HSE.TyVarSig () $ hsTyVarBndrToTyVarBind $ unLoc tvb

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
    HsTupleTy _ HsUnboxedTuple [] ->
        TyCon () $ Special () $ UnboxedSingleCon ()
    HsTupleTy _ boxed xs ->
        TyTuple () (hsTupleSortToBoxed boxed) (map (hsTypeToType . unLoc) xs)

    HsStarTy _ _ ->
        TyStar ()
    HsBangTy _ (HsBang unpackedness strictness) x ->
        applyTyBang (srcStrictnessToBangType strictness) (srcUnpackednessToUnpackedness unpackedness) (hsTypeToType $ unLoc x)
    HsParTy _ x -> case hsTypeToType (unLoc x) of
        x'@TyKind{} -> x'
        x' -> TyParen () x'
    HsQualTy { hst_ctxt, hst_body } ->
        applyTyForall Nothing (Just $ hsTypesToContext $ unLoc hst_ctxt) $
            hsTypeToType $ unLoc hst_body

    HsForAllTy { hst_tele = HsForAllInvis { hsf_invis_bndrs }, hst_body } ->
        applyTyForall (Just $ map (hsTyVarBndrToTyVarBind . unLoc) hsf_invis_bndrs) Nothing $
            hsTypeToType $ unLoc hst_body
    -- TODO FIXME when migrating to ghc-lib-parser completely:
    -- HSE does not forall with visible binders
    HsForAllTy { hst_tele = HsForAllVis { hsf_vis_bndrs }, hst_body } ->
        applyTyForall (Just $ map (hsTyVarBndrToTyVarBind . unLoc) hsf_vis_bndrs) Nothing $
            hsTypeToType $ unLoc hst_body

    HsExplicitListTy _ IsPromoted xs ->
        TyPromoted () $ PromotedList () True (map (hsTypeToType . unLoc) xs)
    HsExplicitListTy _ NotPromoted [x] ->
        TyList () $ hsTypeToType $ unLoc x
    HsExplicitListTy _ NotPromoted xs ->
        TyPromoted () $ PromotedList () False (map (hsTypeToType . unLoc) xs)

    HsExplicitTupleTy _ IsPromoted [] ->
        TyPromoted () $ PromotedCon () True $ Special () $ UnitCon ()
    HsExplicitTupleTy _ IsPromoted xs ->
        TyPromoted () $ PromotedTuple () $ map (hsTypeToType . unLoc) xs
    HsExplicitTupleTy _ NotPromoted xs ->
        TyTuple () HSE.Boxed (map (hsTypeToType . unLoc) xs)

    HsOpTy _ _ x (L _ (Unqual y)) z
        | occNameString y == "~" ->
        TyEquals () (hsTypeToType $ unLoc x) (hsTypeToType $ unLoc z)
    HsOpTy _ promotion x (L _ (Exact y)) z
        | occNameString (nameOccName y) == ":" ->
        TyInfix () (hsTypeToType $ unLoc x)
            (promotionFlagToMaybePromotedName promotion $ Special () $ Cons ()) (hsTypeToType $ unLoc z)
    HsOpTy _ promotion x y z ->
        TyInfix () (hsTypeToType $ unLoc x)
            (promotionFlagToMaybePromotedName promotion $ rdrNameToQName $ unLoc y) (hsTypeToType $ unLoc z)
    HsKindSig _ lhs rhs ->
        TyKind () (hsTypeToType $ unLoc lhs) (hsTypeToType $ unLoc rhs)
    HsTyLit _ (HsNumTy (SourceText txt) val) ->
        TyPromoted () $ PromotedInteger () val (unpackFS txt)
    HsTyLit _ (HsStrTy (SourceText txt) val) ->
        TyPromoted () $ PromotedString () (unpackFS val) (drop 1 $ dropEnd 1 $ unpackFS txt)
    HsSumTy _ xs ->
        TyUnboxedSum () $ map (hsTypeToType . unLoc) xs
    HsWildCardTy _ ->
        TyWildCard () Nothing
    HsIParamTy _ _name ty ->
        -- FIXME when migrating to ghc-lib-parser completely:
        -- HSE does not quite support ImplicitParams in ConstraintKinds,
        hsTypeToType $ unLoc ty
    HsAppKindTy _ x y ->
        -- FIXME when migrating to ghc-lib-parser completely:
        -- HSE does not support syntax like
        -- forall (r :: RuntimeRep) (a :: TYPE r). Maybe# @r a
        TyApp () (hsTypeToType $ unLoc x) (hsTypeToType $ unLoc y)
    ty ->
        error $ show ty

promotionFlagToMaybePromotedName :: PromotionFlag -> QName () -> MaybePromotedName ()
promotionFlagToMaybePromotedName = \case
    NotPromoted -> UnpromotedName ()
    IsPromoted -> PromotedName ()

applyTyForall :: Maybe [TyVarBind ()] -> Maybe (Context ()) -> HSE.Type () -> HSE.Type ()
applyTyForall mArg1 mArg2 = \case
    TyForall () Nothing mArg2' ty
        | isNothing mArg2 -> TyForall () mArg1 mArg2' ty
    ty -> TyForall () mArg1 mArg2 ty

applyTyBang :: BangType () -> Unpackedness () -> HSE.Type () -> HSE.Type ()
applyTyBang bang unpack = \case
    TyBang () (NoStrictAnnot ()) unpack' ty
        | unpack == NoUnpackPragma () -> TyBang () bang unpack' ty
    TyBang () bang' (NoUnpackPragma ()) ty
        | bang == NoStrictAnnot () -> TyBang () bang' unpack ty
    TyApp () x y -> TyApp () (applyTyBang bang unpack x) y
    ty -> TyBang () bang unpack ty

typeToInstHead :: HSE.Type () -> HSE.InstHead ()
typeToInstHead = \case
    TyApp () x y -> HSE.IHApp () (typeToInstHead x) y
    TyCon () x -> HSE.IHCon () x
    TyInfix () x (UnpromotedName () y) z -> HSE.IHApp () (HSE.IHInfix () x y) z
    -- The rest happens only in ghc-prim, which are likely some magical forms.
    -- Let's skip them.
    ty -> HSE.IHCon () $ Special () $ UnitCon ()

hsTypesToContext
    :: [GenLocated SrcSpanAnnA (HsType GhcPs)]
    -> HSE.Context ()
hsTypesToContext = \case
    [] -> HSE.CxEmpty ()
    [x] -> HSE.CxSingle () $ hsTypeToAsst $ unLoc x
    xs -> HSE.CxTuple () $ map (hsTypeToAsst . unLoc) xs

hsTypeToAsst :: HsType GhcPs -> Asst ()
hsTypeToAsst = \case
    HsIParamTy _ name t ->
        HSE.IParam () (hsIPNameToIPName $ unLoc name) (hsTypeToType $ unLoc t)
    HsParTy _ t ->
        HSE.ParenA () $ hsTypeToAsst $ unLoc t
    t -> case hsTypeToType t of
        TyParen () ty -> HSE.ParenA () $ HSE.TypeA () ty
        ty -> HSE.TypeA () ty

hsIPNameToIPName :: HsIPName -> HSE.IPName ()
hsIPNameToIPName = IPDup () . unpackFS . hsIPNameFS

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
runGhcLibParser str = case runGhcLibParserEx almostAllExtensions str of
    PFailed{}
        | '#' `elem` str -> runGhcLibParserEx noUnboxed str
    res -> res
    where
        allExtensions = EnumSet.fromList [minBound..maxBound]
        almostAllExtensions =
            -- This extension makes "proc" a keyword
            EnumSet.delete Arrows $
            -- This extension makes "mdo" and "rec" keywords
            EnumSet.delete RecursiveDo $
            -- This extension makes "static" a keyword
            EnumSet.delete StaticPointers $
            -- This extension makes "by", "group" and "using" keywords
            EnumSet.delete TransformListComp
            allExtensions
        noUnboxed =
            EnumSet.delete UnboxedSums $
            EnumSet.delete UnboxedTuples
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
    test "(@~?) :: (HasCallStack, Ord a, Num a, Show a, ?epsilon :: a) => a -> a -> Assertion"
    test "data Data where { Idx :: {idxChildren :: Index key (Node height key val)} -> Node ('S height) key val}"
    test "UnexpectedResponse :: forall k a b . () => Host -> Response k a b -> ProtocolError"
    test "(.) :: Category k cat => forall (b :: k) (c :: k) (a :: k) . cat b c -> cat a b -> cat a c"
    test "infixl 3 `And`"
    test "infix 1 `shouldBe`"
    test "pattern The :: The d a => a -> d"
    test "Html :: Element \"html\" '[] (Elements [\"head\", \"body\"]) (ManifestA & '[])"
    test "instance forall k1 v1 (pk :: k1 -> GHC.Types.Constraint) (k2 :: k1) (pv :: v1 -> GHC.Types.Constraint) (v2 :: v1) . (pk k2, pv v2) => Type.Membership.KeyTargetAre pk pv (k2 'Type.Membership.Internal.:> v2)"
    test "crDoubleBuffer :: CompactorReturn s -> {-# UNPACK #-} !DoubleBuffer s"
    test "expectationFailure :: (?callStack :: CallStack) => String -> Expectation"
    test "type family MapTyCon t xs = r | r -> xs"
    test "pattern Id :: CRCategory k => (β ~ α, Object k α) => k α β"
    test "pattern Stream :: () => () => Repetition"
    test "In# :: (# #) -> In (a :: Effects) (b :: Effects)"
    test "stretchOuter :: forall (s :: Nat) (sh :: [Nat]) (v :: Type -> Type) a . Shape sh => Array (1 ': sh) v a -> Array (s ': sh) v a"
    test "anyAsciiDecimalWord# :: Addr# -> Addr# -> (# (# #) | (# Word#, Addr# #) #)"
    test "class SymbolToField (sym :: Symbol) rec typ | sym rec -> typ"
    test "closestPairDist_spec :: _ => ([r] -> r) -> (r -> t) -> [b] -> Property"
    -- Cannot faithfully represent ConstraintKind with ImplicitParams in HSE
    -- test "type HasCallStack = ?callStack :: CallStack"
    -- Cannot faithfully represent @r in HSE
    -- test "Maybe# :: forall (r :: RuntimeRep) (a :: TYPE r). (# (# #) | a #) -> Maybe# @r a"
    -- Cannot faithfully represent visible binders in HSE
    -- test "data NDFamily_ :: forall (name :: Name) -> forall (ks :: Params name). ParamsProxy name ks -> Res name ks Any :~: r -> Args name ks -> Exp r"
    -- Cannot faithfully represent standalone kind signatures in HSE
    -- test "type MinBound :: a;"
