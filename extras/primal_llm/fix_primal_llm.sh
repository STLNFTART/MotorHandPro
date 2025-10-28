#!/bin/bash
# fix_primal_llm.sh

# Create backup
cp PrimalLLM PrimalLLM.backup

# Fix the syntax error by adding missing closing brackets and dependencies
sed -i '69a\      } catch (error) {\
        console.error("Failed to initialize Primal LLM:", error);\
      }\
    };\
    \
    initializePrimalLLM();\
  }, [llmConfig]);' PrimalLLM

echo "Fixed syntax error in PrimalLLM"
