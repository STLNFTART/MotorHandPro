#!/bin/bash
# Better syntax fix
cp PrimalLLM PrimalLLM.backup2

# Find and fix the specific useEffect block
sed -i '69,73c\
  useEffect(() => {\
    const initializePrimalLLM = async () => {\
      try {\
        // Your code here\
      } catch (error) {\
        console.error("Error:", error);\
      }\
    };\
    initializePrimalLLM();\
  }, []);' PrimalLLM

echo "Fixed useEffect syntax"
