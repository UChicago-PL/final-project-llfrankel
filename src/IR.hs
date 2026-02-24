{-# LANGUAGE OverloadedStrings #-}

module IR where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Map (Map)

data Access = Free | Bound
  deriving (Show, Eq)

data Dimension = Dimension
  { dimName :: String,
    dimAccess :: Access
  }
  deriving (Show, Eq)

data PrimitiveSpec = PrimitiveSpec
  { primName :: String,
    primOutputDomain :: [Dimension],
    primHardware :: [String],
    primAddsState :: Bool
  }
  deriving (Show, Eq)

data BackendSpec = BackendSpec
  { backendId :: String,
    backendHardware :: [String],
    backendSinks :: [String],
    backendCoords :: Map String Dimension,
    backendPrimitives :: Map String PrimitiveSpec
  }
  deriving (Show, Eq)

data Strand = Strand
  { strandName :: String,
    strandIndex :: Int,
    strandExpr :: Expr
  }
  deriving (Show, Eq)

data Bundle = Bundle
  { bundleName :: String,
    bundleStrands :: [Strand]
  }
  deriving (Show, Eq)

data Spindle = Spindle
  { spindleName :: String,
    spindleParams :: [String],
    spindleLocals :: [Bundle],
    spindleReturns :: [Expr]
  }
  deriving (Show, Eq)

data Program = Program
  { progBundles :: Map String Bundle,
    progSpindles :: Map String Spindle,
    progOrder :: [OrderEntry],
    progResources :: [String],
    progTextRes :: [String]
  }
  deriving (Show, Eq)

data OrderEntry = OrderEntry
  { orderBundle :: String,
    orderStrands :: Maybe [String]
  }
  deriving (Show, Eq)

data AnalysisInput = AnalysisInput
  { inputProgram :: Program,
    inputBackends :: [BackendSpec]
  }
  deriving (Show, Eq)

data Expr
  = Num Double
  | Param String
  | Index String Expr
  | Binary String Expr Expr
  | Unary String Expr
  | Call String [Expr]
  | Builtin String [Expr]
  | Extract Expr Int
  | Remap Expr (Map String Expr)
  | CacheRead String Int
  deriving (Show, Eq)

instance FromJSON Access where
  parseJSON = withText "Access" $ \t -> case t of
    "free" -> pure Free
    "bound" -> pure Bound
    _ -> fail ("unknownn access:" ++ show t)

instance ToJSON Access where
  toJSON Free = String "free"
  toJSON Bound = String "bound"

instance FromJSON Dimension where
  parseJSON = withObject "Dimension" $ \ob -> do
    n <- ob .: "name"
    a <- ob .: "access"
    pure (Dimension n a)

instance ToJSON Dimension where
  toJSON d =
    object
      [ "name" .= dimName d,
        "access" .= dimAccess d
      ]

instance FromJSON PrimitiveSpec where
  parseJSON = withObject "PrimitiveSpec" $ \ob -> do
    n <- ob .: "name"
    od <- ob .: "outputDomain"
    hw <- ob .: "hardwareRequired"
    st <- ob .: "addsState"
    pure (PrimitiveSpec n od hw st)

instance ToJSON PrimitiveSpec where
  toJSON p =
    object
      [ "name" .= primName p,
        "outputDomain" .= primOutputDomain p,
        "hardwareRequired" .= primHardware p,
        "addsState" .= primAddsState p
      ]

instance FromJSON BackendSpec where
  parseJSON = withObject "BackendSpec" $ \ob -> do
    i <- ob .: "identifier"
    hw <- ob .: "hardwareOwned"
    sk <- ob .: "outputSinks"
    cs <- ob .: "coordinateSpecs"
    ps <- ob .: "primitiveSpecs"
    pure (BackendSpec i hw sk cs ps)

instance ToJSON BackendSpec where
  toJSON b =
    object
      [ "identifier" .= backendId b,
        "hardwareOwned" .= backendHardware b,
        "outputSinks" .= backendSinks b,
        "coordinateSpecs" .= backendCoords b,
        "primitiveSpecs" .= backendPrimitives b
      ]

instance FromJSON Strand where
  parseJSON = withObject "Strand" $ \ob -> do
    n <- ob .: "name"
    i <- ob .: "index"
    e <- ob .: "expr"
    pure (Strand n i e)

instance ToJSON Strand where
  toJSON s =
    object
      [ "name" .= strandName s,
        "index" .= strandIndex s,
        "expr" .= strandExpr s
      ]

instance FromJSON Bundle where
  parseJSON = withObject "Bundle" $ \ob -> do
    n <- ob .: "name"
    s <- ob .: "strands"
    pure (Bundle n s)

instance ToJSON Bundle where
  toJSON b =
    object
      [ "name" .= bundleName b,
        "strands" .= bundleStrands b
      ]

instance FromJSON Spindle where
  parseJSON = withObject "Spindle" $ \ob -> do
    n <- ob .: "name"
    p <- ob .: "params"
    l <- ob .: "locals"
    r <- ob .: "returns"
    pure (Spindle n p l r)

instance ToJSON Spindle where
  toJSON s =
    object
      [ "name" .= spindleName s,
        "params" .= spindleParams s,
        "locals" .= spindleLocals s,
        "returns" .= spindleReturns s
      ]

instance FromJSON OrderEntry where
  parseJSON = withObject "OrderEntry" $ \ob -> do
    b <- ob .: "bundle"
    s <- ob .:? "strands"
    pure (OrderEntry b s)

instance ToJSON OrderEntry where
  toJSON e = object $ ("bundle" .= orderBundle e) : maybe [] (\s -> ["strands" .= s]) (orderStrands e)

instance FromJSON Program where
  parseJSON = withObject "Program" $ \ob -> do
    b <- ob .: "bundles"
    sp <- ob .: "spindles"
    or' <- ob .: "order"
    r <- ob .:? "resources" .!= []
    tr <- ob .:? "textResources" .!= []
    pure (Program b sp or' r tr)

instance ToJSON Program where
  toJSON p =
    object
      [ "bundles" .= progBundles p,
        "spindles" .= progSpindles p,
        "order" .= progOrder p,
        "resources" .= progResources p,
        "textResources" .= progTextRes p
      ]

instance FromJSON AnalysisInput where
  parseJSON = withObject "AnalysisInput" $ \ob -> do
    p <- ob .: "program"
    b <- ob .: "backends"
    pure (AnalysisInput p b)

instance ToJSON AnalysisInput where
  toJSON a =
    object
      [ "program" .= inputProgram a,
        "backends" .= inputBackends a
      ]

instance FromJSON Expr where
  parseJSON = withObject "Expr" $ \ob -> do
    tag <- ob .: "type" :: Parser String
    case tag of
      "num" -> Num <$> ob .: "value"
      "param" -> Param <$> ob .: "name"
      "index" -> Index <$> ob .: "bundle" <*> ob .: "indexExpr"
      "binary" -> Binary <$> ob .: "op" <*> ob .: "left" <*> ob .: "right"
      "unary" -> Unary <$> ob .: "op" <*> ob .: "operand"
      "call" -> Call <$> ob .: "spindle" <*> ob .: "args"
      "builtin" -> Builtin <$> ob .: "name" <*> ob .: "args"
      "extract" -> Extract <$> ob .: "call" <*> ob .: "index"
      "remap" -> Remap <$> ob .: "base" <*> ob .: "substitutions"
      "cacheRead" -> CacheRead <$> ob .: "cacheId" <*> ob .: "tapIndex"
      _ -> fail ("unknown expr type:" ++ tag)

instance ToJSON Expr where
  toJSON expr = case expr of
    Num v -> tagged "num" ["value" .= v]
    Param n -> tagged "param" ["name" .= n]
    Index b e -> tagged "index" ["bundle" .= b, "indexExpr" .= e]
    Binary op l r -> tagged "binary" ["op" .= op, "left" .= l, "right" .= r]
    Unary op e -> tagged "unary" ["op" .= op, "operand" .= e]
    Call s args -> tagged "call" ["spindle" .= s, "args" .= args]
    Builtin n args -> tagged "builtin" ["name" .= n, "args" .= args]
    Extract c i -> tagged "extract" ["call" .= c, "index" .= i]
    Remap b subs -> tagged "remap" ["base" .= b, "substitutions" .= subs]
    CacheRead c t -> tagged "cacheRead" ["cacheId" .= c, "tapIndex" .= t]
    where
      tagged t fields = object (("type" .= (t :: String)) : fields)
