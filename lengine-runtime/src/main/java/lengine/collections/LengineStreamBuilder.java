package lengine.collections;

import java.util.LinkedList;
import java.util.List;

import lengine.collections.traits.CollectionBuilder;

public class LengineStreamBuilder implements CollectionBuilder<LengineStream> {
    private final List<Object> thisObj = new LinkedList<>();
    @Override
    public void ADD(Object elem) {
        thisObj.add(elem);
    }

    @Override
    public LengineStream BUILD() {
        return LengineStream.createFromList(thisObj);
    }
}
