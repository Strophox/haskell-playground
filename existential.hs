main = do
  let myObjs = [myA, myB]
  display (head (inc <$> myObjs))
  display (last (inc . inc . inc <$> myObjs))

data Counter a = forall self. NewCounter
  { _this    :: self
  , _inc     :: self -> self
  , _display :: self -> IO ()
  , tag :: a
  }

inc :: Counter a -> Counter a
inc (NewCounter x i d t) = NewCounter{_this=i x, _inc=i, _display=d, tag=t}

display :: Counter a -> IO ()
display NewCounter{_this=x, _display=d} = d x

myA, myB :: Counter String
myA = NewCounter{_this=0, _inc=(+1), _display=print, tag="A"}
myB = NewCounter{_this="", _inc=('*':), _display=putStrLn, tag="B"}
