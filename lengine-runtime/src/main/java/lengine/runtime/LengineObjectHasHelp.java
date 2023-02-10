package lengine.runtime;

import lengine.util.LengineMapKey;
import lengine.util.LengineSequence;

public interface LengineObjectHasHelp {
    LengineSequence help();
    String help(LengineMapKey key);
}
