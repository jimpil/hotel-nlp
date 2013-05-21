package hotel_nlp.externals;
import org.jgrapht.graph.DefaultEdge;

public final class RelationEdge<V> extends DefaultEdge {
    private final V vertex1;
    private final V vertex2;
    private final String label;

    public RelationEdge(V v1, V v2, String label) {
        vertex1 = v1;
        vertex2 = v2;
        this.label = label;
     }
     
     @Override
     public V getSource() {
        return vertex1;
     }
     
    @Override
    public V getTarget() {
        return vertex2;
    }
    
    @Override
    public String toString() {
        return label;
     }
}
