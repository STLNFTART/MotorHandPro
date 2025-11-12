#!/bin/bash
cp PrimalLLM PrimalLLM.backup3

# Remove the broken lines and rebuild properly
sed -i '75,85d' PrimalLLM
sed -i '69a\
  useEffect(() => {\
    const initializePrimalLLM = async () => {\
      try {\
        const cfg = {\
          dt: 1,\
          half_life: 6,\
          lambda: 0.115525,\
          alpha: 0.109101\
        };\
        // Add your initialization code here\
      } catch (error) {\
        console.error("Error:", error);\
      }\
    };\
    initializePrimalLLM();\
  }, [llmConfig]);' PrimalLLM

echo "Cleaned up useEffect structure"
