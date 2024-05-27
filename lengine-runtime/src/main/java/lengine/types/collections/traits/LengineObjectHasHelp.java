package lengine.types.collections.traits;

import lengine.types.LengineString;
import lengine.types.collections.LengineMapKey;
import lengine.types.collections.LengineSequence;

public interface LengineObjectHasHelp {
    LengineSequence help();
    LengineString help(LengineMapKey key);
}
