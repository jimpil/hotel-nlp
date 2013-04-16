
package hotel_nlp.externals;

public class OMG extends org.apache.uima.analysis_component.JCasAnnotator_ImplBase {
	
   	 private final component, extractor;

   	 public OMG(Object o, Object extractor){
   	 	super();
   	 	component = o;
   	 	this.extractor = extractor;
   	 }


@override
public void process(JCas aJCas) throws AnalysisEngineProcessException{
  Object trueInput = extractor.invoke(aJCas);
   //Object result
  try{
      component.invoke(trueInput);
    }
    catch Exception {
    	component.applyTo(trueInput);
    }
}

}