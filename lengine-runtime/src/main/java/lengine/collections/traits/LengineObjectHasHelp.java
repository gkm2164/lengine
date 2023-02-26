package lengine.collections.traits;

import lengine.runtime.LengineString;
import lengine.collections.LengineMapKey;
import lengine.collections.LengineSequence;

public interface LengineObjectHasHelp {
    LengineSequence help();
    LengineString help(LengineMapKey key);
}
