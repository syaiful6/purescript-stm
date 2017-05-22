// module Control.Monad.STM.Internal

var tid = 0;

exports._newTVarId = function () {
  return tid++;
}
