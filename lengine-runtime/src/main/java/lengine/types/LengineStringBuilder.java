package lengine.types;

import lengine.types.collections.traits.CollectionBuilder;

public class LengineStringBuilder implements CollectionBuilder<LengineString> {
    private final StringBuilder stringBuilder = new StringBuilder();
    @Override
    public void ADD(Object elem) {
        stringBuilder.append(elem);
    }

    @Override
    public LengineString BUILD() {
        return LengineString.create(stringBuilder.toString());
    }
}
