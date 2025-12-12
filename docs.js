const API_BASE = '/api';

// Bind to n and p keys
document.addEventListener('keydown', (e) => {
    if (!['INPUT', 'TEXTAREA'].includes(e.target.tagName)) {
        if (e.key === 'n') {
            e.preventDefault();
            nextSection();
        }
        if (e.key === 'p') {
            e.preventDefault();
            prevSection();
        }
    }
});

// Documentation structure
// const docStructure = [
//     {
//         title: 'Introduction',
//         id: 'introduction',
//         items: []
//     },
//     {
//         title: 'Getting Started',
//         id: 'getting-started',
//         items: [
//             { title: 'Installation', id: 'installation' },
//             { title: 'Your First Program', id: 'hello-world' }
//         ]
//     },
//     {
//         title: 'Syntax Basics',
//         id: 'syntax',
//         items: [
//             { title: 'Functions', id: 'functions' },
//             { title: 'Pattern Matching', id: 'pattern-matching' },
//             { title: 'Lists', id: 'lists' }
//         ]
//     },
//     {
//         title: 'Type System',
//         id: 'types',
//         items: [
//             { title: 'Basic Types', id: 'basic-types' },
//             { title: 'Type Annotations', id: 'type-annotations' }
//         ]
//     },
//     {
//         title: 'Advanced Features',
//         id: 'advanced',
//         items: [
//             { title: 'Recursion', id: 'recursion' },
//             { title: 'Higher-Order Functions', id: 'higher-order' }
//         ]
//     },
//     {
//         title: 'Examples',
//         id: 'examples',
//         items: [
//             { title: 'Fibonacci Sequence', id: 'fibonacci' },
//             { title: 'Quicksort', id: 'quicksort' }
//         ]
//     },
//     {
//         title: 'Language Reference',
//         id: 'reference',
//         items: [
//             { title: 'Keywords', id: 'keywords' },
//             { title: 'Operators', id: 'operators' }
//         ]
//     }
// ];

const docStructure = [
    {
        title: 'Introduction',
        id: 'introduction',
        items: []
    },
    {
        title: 'Getting Started',
        id: 'getting-started',
        items: [
            { title: 'Installation', id: 'installation' },
            { title: 'Your First Program', id: 'hello-world' }
        ]
    },
    {
        title: 'Syntax Basics',
        id: 'syntax',
        items: [
            { title: 'Functions', id: 'functions' },
            { title: 'Pattern Matching', id: 'pattern-matching' },
            { title: 'Lists and the Cons Operator', id: 'lists' },
            { title: 'Tail Recursion', id: 'tail-recursion' },
            { title: 'Lambda Functions', id: 'lambdas' }
        ]
    },
    {
        title: 'Type System',
        id: 'types',
        items: [
            { title: 'Basic Types', id: 'basic-types' },
            { title: 'Type Signatures', id: 'type-signatures' }
        ]
    },
    {
        title: 'Language Reference',
        id: 'reference',
        items: [
            { title: 'Keywords', id: 'keywords' },
            { title: 'Operators', id: 'operators' },
            { title: 'Built-in Functions', id: 'built-in-functions' },
            { title: 'Error Atoms', id: 'error-atoms' }
        ]
    }
];

// Initialize on load
document.addEventListener('DOMContentLoaded', () => {
    generateTOC();
    setupScrollSpy();
    setupMobileTOC();
    setupSearch();
    highlightActiveSection();
});

// Generate table of contents
function generateTOC() {
  const tocContainer = document.getElementById('tocContainer');
  
  docStructure.forEach(section => {
    const sectionDiv = document.createElement('div');
    sectionDiv.className = 'toc-section';
    sectionDiv.dataset.originalDisplay = 'block';
    
    const titleDiv = document.createElement('div');
    titleDiv.className = 'toc-section-title';
    titleDiv.onclick = () => {
      if (section.items.length > 0) {
        sectionDiv.classList.toggle('collapsed');
      } else {
        scrollToSection(section.id);
      }
    };
    
    if (section.items.length > 0) {
      const chevron = document.createElement('span');
      chevron.className = 'toc-chevron';
      chevron.textContent = '▼';
      titleDiv.appendChild(chevron);
    }
    
    const titleText = document.createElement('span');
    titleText.textContent = section.title;
    titleDiv.appendChild(titleText);
    
    sectionDiv.appendChild(titleDiv);
    
    if (section.items.length > 0) {
      const itemsDiv = document.createElement('div');
      itemsDiv.className = 'toc-items';
      
      section.items.forEach(item => {
        const itemDiv = document.createElement('div');
        itemDiv.className = 'toc-item';
        itemDiv.textContent = item.title;
        itemDiv.dataset.originalDisplay = 'block';
        itemDiv.onclick = () => scrollToSection(item.id);
        itemsDiv.appendChild(itemDiv);
      });
      
      sectionDiv.appendChild(itemsDiv);
    }
    
    tocContainer.appendChild(sectionDiv);
  });
}

// Scroll to section
function scrollToSection(id) {
  const element = document.getElementById(id);
  if (element) {
    element.scrollIntoView({ behavior: 'smooth', block: 'start' });
    
    // Close mobile menu if open
    closeMobileMenu();
  }
}

// Close mobile menu
function closeMobileMenu() {
  const sidebar = document.getElementById('docsSidebar');
  const overlay = document.getElementById('docsOverlay');
  sidebar.classList.remove('open');
  overlay.classList.remove('show');
}

// Setup scroll spy for active highlighting
function setupScrollSpy() {
  const observer = new IntersectionObserver(
    (entries) => {
      entries.forEach(entry => {
        if (entry.isIntersecting) {
          updateActiveSection(entry.target.id);
        }
      });
    },
    {
      rootMargin: '-100px 0px -66%'
    }
  );

  // Observe all sections
  document.querySelectorAll('.section, .subsection-title').forEach(section => {
    observer.observe(section);
  });
}

// Update active section in TOC
function updateActiveSection(activeId) {
  // Remove all active classes
  document.querySelectorAll('.toc-section-title, .toc-item').forEach(el => {
    el.classList.remove('active');
  });

  // Find and activate the matching TOC item
  const allItems = document.querySelectorAll('.toc-section-title, .toc-item');
  allItems.forEach(item => {
    const sectionDiv = item.closest('.toc-section');
    const sectionTitle = sectionDiv?.querySelector('.toc-section-title');
    
    // Check if this is a section title
    if (item.classList.contains('toc-section-title')) {
      const section = docStructure.find(s => s.title === item.textContent.trim());
      if (section && section.id === activeId) {
        item.classList.add('active');
      }
    }
    
    // Check if this is a subsection item
    if (item.classList.contains('toc-item')) {
      const itemText = item.textContent.trim();
      docStructure.forEach(section => {
        const subsection = section.items.find(i => i.title === itemText);
        if (subsection && subsection.id === activeId) {
          item.classList.add('active');
          // Also expand the parent section if collapsed
          sectionDiv?.classList.remove('collapsed');
        }
      });
    }
  });
}

// Highlight active section on initial load
function highlightActiveSection() {
  const hash = window.location.hash.slice(1);
  if (hash) {
    setTimeout(() => {
      updateActiveSection(hash);
      scrollToSection(hash);
    }, 100);
  }
}

// Setup mobile TOC toggle
function setupMobileTOC() {
  const toggle = document.getElementById('mobileTocToggle');
  const sidebar = document.getElementById('docsSidebar');
  const overlay = document.getElementById('docsOverlay');

  toggle.addEventListener('click', () => {
    const isOpen = sidebar.classList.contains('open');
    
    if (isOpen) {
      closeMobileMenu();
    } else {
      sidebar.classList.add('open');
      overlay.classList.add('show');
    }
  });

  overlay.addEventListener('click', () => {
    closeMobileMenu();
  });
}

// Setup search functionality
function setupSearch() {
  const searchInput = document.getElementById('searchInput');
  let searchTimeout;

  searchInput.addEventListener('input', (e) => {
    clearTimeout(searchTimeout);
    searchTimeout = setTimeout(() => {
      performSearch(e.target.value);
    }, 300);
  });
}

function performSearch(query) {
  const lowerQuery = query.toLowerCase().trim();
  
  if (!lowerQuery) {
    // Show all sections and items
    document.querySelectorAll('.toc-section').forEach(section => {
      section.style.display = section.dataset.originalDisplay || 'block';
      section.classList.remove('collapsed');
      
      // Show all items
      section.querySelectorAll('.toc-item').forEach(item => {
        item.style.display = item.dataset.originalDisplay || 'block';
      });
    });
    return;
  }

  // Filter sections and items
  document.querySelectorAll('.toc-section').forEach(section => {
    const sectionTitle = section.querySelector('.toc-section-title').textContent.toLowerCase();
    const items = section.querySelectorAll('.toc-item');
    
    let hasMatch = sectionTitle.includes(lowerQuery);
    let visibleItemCount = 0;
    
    items.forEach(item => {
      const itemText = item.textContent.toLowerCase();
      if (itemText.includes(lowerQuery)) {
        hasMatch = true;
        item.style.display = item.dataset.originalDisplay || 'block';
        visibleItemCount++;
      } else {
        item.style.display = 'none';
      }
    });

    if (hasMatch) {
      section.style.display = section.dataset.originalDisplay || 'block';
      // Only expand if there are visible items or section title matches
      if (visibleItemCount > 0 || sectionTitle.includes(lowerQuery)) {
        section.classList.remove('collapsed');
      }
    } else {
      section.style.display = 'none';
    }
  });
}

// Escape HTML for safe display
function escapeHtml(text) {
  const div = document.createElement('div');
  div.textContent = text;
  return div.innerHTML;
}

// Run code example
async function runCode(button) {
  const codeExample = button.closest('.code-example');
  const codeBlock = codeExample.querySelector('.code-example-code');
  const outputBlock = codeExample.querySelector('.code-example-output');
  const code = codeBlock.textContent.trim();

  // Update button state
  button.disabled = true;
  const originalHTML = button.innerHTML;
  button.innerHTML = '<span class="spinner"></span> Running...';
  
  // Show output area with loading state
  outputBlock.className = 'code-example-output show';
  outputBlock.textContent = 'Compiling and running...';

  try {
    const response = await fetch(`${API_BASE}/test`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        problemId: 'docs_example',
        code: code,
        sessionToken: null  // Docs don't need session
      })
    });

    const data = await response.json();
    
    if (!response.ok) {
      // Handle error response
      outputBlock.className = 'code-example-output show error';
      outputBlock.textContent = data.error || 'Error running code';
    } else if (data.results && data.results.length > 0) {
      // Handle the new response format
      const result = data.results[0];
      
      if (result.testPassed) {
        // Success - show output
        outputBlock.className = 'code-example-output show';
        if (result.testError) {
          // The output is in testError field for docs examples
          outputBlock.textContent = result.testError;
        } else {
          outputBlock.textContent = '✓ Code executed successfully';
        }
      } else {
        // Compilation or runtime error
        outputBlock.className = 'code-example-output show error';
        outputBlock.textContent = result.testError || 'Error running code';
      }
    } else {
      // Fallback for unexpected response format
      outputBlock.className = 'code-example-output show';
      outputBlock.textContent = '✓ Code executed successfully';
    }
  } catch (error) {
    outputBlock.className = 'code-example-output show error';
    outputBlock.textContent = `Error: ${error.message}\n\nNote: Make sure the Amortia server is running to execute code examples.`;
  } finally {
    button.disabled = false;
    button.innerHTML = originalHTML;
  }
}

// Copy code to clipboard
function copyCode(button) {
  const codeExample = button.closest('.code-example');
  const codeBlock = codeExample.querySelector('.code-example-code');
  const code = codeBlock.textContent.trim();

  navigator.clipboard.writeText(code).then(() => {
    const originalText = button.textContent;
    button.textContent = '✓ Copied!';
    button.style.color = '#4ec9b0';
    
    setTimeout(() => {
      button.textContent = originalText;
      button.style.color = '';
    }, 2000);
  }).catch(err => {
    console.error('Failed to copy:', err);
    button.textContent = '✗ Failed';
    setTimeout(() => {
      button.textContent = 'Copy';
    }, 2000);
  });
}

// Handle hash changes
window.addEventListener('hashchange', () => {
  const hash = window.location.hash.slice(1);
  if (hash) {
    updateActiveSection(hash);
  }
});






// NAVIGATION

function getAllSections() {
    // Get all elements with IDs that we care about
    const allElements = Array.from(document.querySelectorAll('[id]')).filter(el => {
        // Filter to elements that look like content sections
        return el.classList.contains('section') || 
               el.classList.contains('subsection-title') ||
               el.tagName === 'H2' || 
               el.tagName === 'H3' ||
               el.classList.contains('subsection');
    });
    
    // Sort by position on page (top to bottom)
    return allElements.sort((a, b) => {
        return a.getBoundingClientRect().top + window.scrollY - 
               b.getBoundingClientRect().top - window.scrollY;
    });
}

function nextSection() {
    const sections = getAllSections();
    const currentScroll = window.scrollY;
    
    // Find the first section whose top is below current scroll position
    for (let section of sections) {
        const sectionTop = section.getBoundingClientRect().top + window.scrollY;
        if (sectionTop > currentScroll + 100) { // +100 for some buffer
            section.scrollIntoView({ behavior: 'smooth', block: 'start' });
            return;
        }
    }
    
    // If at bottom, go to first section
    if (sections.length > 0) {
        sections[0].scrollIntoView({ behavior: 'smooth', block: 'start' });
    }
}

function prevSection() {
    const sections = getAllSections();
    const currentScroll = window.scrollY;
    
    // Find the last section whose top is above current scroll position
    for (let i = sections.length - 1; i >= 0; i--) {
        const section = sections[i];
        const sectionTop = section.getBoundingClientRect().top + window.scrollY;
        if (sectionTop < currentScroll - 100) { // -100 for some buffer
            section.scrollIntoView({ behavior: 'smooth', block: 'start' });
            return;
        }
    }
    
    // If at top, go to last section
    if (sections.length > 0) {
        sections[sections.length - 1].scrollIntoView({ behavior: 'smooth', block: 'start' });
    }
}



// INTERACTIVE DOCS

const docKeywords = {
  'true':    '#4EC9B0',
  'false':   '#F48771',
  'type':    '#35CDAF',
  'error':   '#F48771',
};

// Pattern matching is done in order, so more specific patterns should come first
const syntaxDefinitions = [
  // COMMENTS - must come before strings since they can contain quote-like characters
  {
    pattern: /--.*$/,
    color: '#6A9955',
    title: 'Line Comment',
    description: 'A comment that extends to the end of the line. Comments are ignored by the compiler.',
    isComment: true
  },

  // STRINGS - but we'll handle escape sequences separately inside them
  {
    pattern: /"(?:[^"\\]|\\.)*"/,
    color: '#CE9178',
    title: 'String Literal',
    description: 'A sequence of characters enclosed in double quotes. Can contain escape sequences like ~n for newline.',
    allowNested: true  // Allow nested patterns inside strings
  },
  
  // ESCAPE SEQUENCES (will be matched inside strings)
  {
    pattern: /~n/,
    color: '#DCDCAA',
    title: 'Newline',
    description: 'Escape sequence for a newline character inside strings. Erlang-style formatting.',
    nested: true
  },

  // NUMBERS (floats before integers)
  {
    pattern: /-?\d+\.\d+/,
    color: '#B5CEA8',
    title: 'Float Number',
    description: 'Floating point numeric literal (e.g., 3.14, -2.5).',
    nested: true
  },
  {
    pattern: /-?\d+/,
    color: '#B5CEA8',
    title: 'Integer Number',
    description: 'Integer numeric literal (e.g., 42, -10).',
    nested: true
  },
  
  {
    pattern: /\[\[[A-Z][a-zA-Z0-9_]*,\s*[a-z]\]\]/,
    color: '#DCDCDC',
    title: 'List of Two-Element Lists',
    description: 'A list of lists, where each inner list contains exactly two items.',
    allowNested: true
  },

  // SPECIFIC TYPE PATTERNS (before general type matching)
  {
    pattern: /\[[a-z]\]/,
    color: '#DCDCDC',
    title: 'Generic List',
    description: 'A list containing elements of type \'a\', a placeholder that can represent any data like Int, String, or custom types.',
  },
  
  // TYPE NAMES (capitalized identifiers)
  {
    pattern: /\bString\b/,
    color: '#35CDAF',
    title: 'String Type',
    description: 'Text data type for sequences of characters.',
    nested: true
  },
  {
    pattern: /\bInt\b/,
    color: '#35CDAF',
    title: 'Integer Type',
    description: 'Whole number data type without decimal points.',
    nested: true
  },
  {
    pattern: /\bBool\b/,
    color: '#35CDAF',
    title: 'Boolean Type',
    description: 'Logical data type with two values: true or false.',
    nested: true
  },
  
  // FUNCTION NAMES (must come before general keywords to catch the name after defn)
  {
    pattern: /(?<=\bdefn\s+)[a-z_][a-zA-Z0-9_]*/,
    color: '#DCDCAA',
    title: 'Function Name',
    description: 'The name of the function being defined.'
  },

  // KEYWORDS
  {
    pattern: /\bdefn\b/,
    color: '#569CD6',
    title: 'Function Definition',
    description: 'Keyword to define a new function. Every function in Amortia starts with defn.'
  },
  {
    pattern: /\berror\b/,
    color: '#569CD6',
    title: 'Error Signal',
    description: 'Raises an error with the given atom. Used for signaling exceptional conditions.'
  },
  {
    pattern: /\bwhen\b/,
    color: '#569CD6',
    title: 'Guard Clause',
    description: 'Adds a boolean condition that must be true for the pattern to match.'
  },
  {
    pattern: /\blet\b/,
    color: '#569CD6',
    title: 'Variable Binding',
    description: 'Binds a value to a name within the current scope. Similar to const in JavaScript.'
  },
  {
    pattern: /\bif\b/,
    color: '#569CD6',
    title: 'Conditional',
    description: 'Evaluates a condition and executes different code based on true/false.'
  },
  
  // BUILT-IN FUNCTIONS
  {
    pattern: /\bdisplay\b/,
    color: '#DCDCDC',
    title: 'Print Function',
    description: 'Main built-in function to print text to stdout.'
  },
  {
    pattern: /\bhalt\b/,
    color: '#DCDCDC',
    title: 'Exit Function',
    description: 'Terminates the program and returns an exit code to the operating system.'
  },
  {
    pattern: /\bmap\b/,
    color: '#569CD6',
    title: 'Map Function',
    description: 'Applies a function to each element of a list, returning a new list with results.'
  },
  {
    pattern: /\bcar\b/,
    color: '#DCDCDC',
    title: 'List Head',
    description: 'Returns the first element of a list. Derived from Lisp terminology.',
    nested: true
  },
  {
    pattern: /\bcdr\b/,
    color: '#DCDCDC',
    title: 'List Tail',
    description: 'Returns all elements except the first. Derived from Lisp terminology.'
  },
  {
    pattern: /\bhead\b/,
    color: '#DCDCDC',
    title: 'List Head',
    description: 'Returns the first element of a list.'
  },
  {
    pattern: /\btail\b/,
    color: '#DCDCDC',
    title: 'List Tail',
    description: 'Returns all elements of a list except the first.'
  },
  
  // SPECIAL IDENTIFIERS
  {
    pattern: /\bmain\b/,
    color: '#DCDCDC',
    title: 'Entry Point',
    description: 'The main function is the entry point of your program. Execution starts here.'
  },
  
  // BOOLEAN LITERALS
  {
    pattern: /\btrue\b/,
    color: '#569CD6',
    title: 'Boolean True',
    description: 'Boolean literal value representing true.'
  },
  {
    pattern: /\bfalse\b/,
    color: '#569CD6',
    title: 'Boolean False',
    description: 'Boolean literal value representing false.'
  },
  
  // ERROR ATOMS
  {
    pattern: /\bbadarg\b/,
    color: '#F48771',
    title: 'Error Atom',
    description: 'Common error atom indicating a bad argument was passed to a function.'
  },
  {
    pattern: /\bnotfound\b/,
    color: '#F48771',
    title: 'Error Atom',
    description: 'Common error atom indicating a bad argument was passed to a function.'
  },
  {
    pattern: /\bundefined\b/,
    color: '#F48771',
    title: 'Error Atom',
    description: 'Common error atom indicating a bad argument was passed to a function.'
  },
  
  {
    pattern: /,/,
    color: '#DCDCDC',
    title: 'Comma Separator',
    description: 'Separates elements in lists, signal the start of a guard, or the end of a lambda.',
    nested: true  // Can be highlighted inside list patterns
  },

  // LIST PATTERNS (specific before general) - now with nested support!


    {
      pattern: /\[(?:[a-z_][a-zA-Z0-9_ ]*|\[[^\]]*\])(?:,\s*(?:[a-z_][a-zA-Z0-9_ ]*|\[[^\]]*\]))*\|[a-z_][a-zA-Z0-9_ ]*\]/,
      color: '#DCDCDC',
      title: 'List Pattern',
      description: 'Destructures a list into head elements and tail. The | operator separates them, commas separate multiple head elements.',
      nested: true,
      allowNested: true
    },
    
    
    {
        pattern: /\[(?:[a-z_][a-zA-Z0-9_ ]*|\d+)(?:,\s*(?:[a-z_][a-zA-Z0-9_ ]*|\d+))*\]/,
        color: '#DCDCDC',
        title: 'List Literal',
        description: 'A list containing multiple elements separated by commas.',
        allowNested: true
    },
    
    {
        pattern: /\[\[[a-z]\]\]/,
        color: '#DCDCDC',
        title: 'List of Lists',
        description: 'A list of lists of type \'a\'.',
        allowNested: true
    },
    
    {
        pattern: /\[[a-z_][a-zA-Z0-9_]*\]/,
        color: '#DCDCDC',
        title: 'Single item List',
        description: 'Matches a single item list.',
        allowNested: true  // Allow nested _ highlighting
    },
    {
        pattern: /\[\]/,
        color: '#DCDCDC',
        title: 'Empty List',
        description: 'Matches an empty list. Base case for many recursive list operations.'
    },
    
    // UNIT TYPE
    {
        pattern: /\(\)/,
        color: '#DCDCDC',
        title: 'Unit Type',
        description: 'Represents a function with no parameters or a function that returns nothing (void).'
    },
    
    // OPERATORS (multi-character before single)
    {
        pattern: /::/,
        color: '#35CDAF',
        title: 'Type Signature',
        description: 'Separates the function name from its type signature. Declares parameter and return types.'
    },
    {
        pattern: /->/,
        color: '#569CD6',
        title: 'Pattern Arrow',
        description: 'Separates a pattern from its result expression in pattern matching.'
    },
    {
        pattern: /\+\+/,
        color: '#D7BA7D',
        title: 'Concatenation',
        description: 'Concatenates two lists or strings together into a single list/string.'
    },
    {
        pattern: /==/,
        color: '#DCDCDC',
        title: 'Equality comparison operator',
        description: 'Returns true if the left and right operand are equal, false otherwise.'
    },
    {
        pattern: /=/,
        color: '#DCDCDC',
        title: 'assignment operator',
        description: 'Used for variable binding. Stores a value from its right-hand side (RHS) into a variable on its left-hand side (LHS)'
    },
    {
        pattern: /!=/,
        color: '#DCDCDC',
        title: 'Inequality',
        description: 'Returns true if the left and right operands are not equal.'
    },
    {
        pattern: /<=/,
        color: '#DCDCDC',
        title: 'Less Than or Equal',
        description: 'Returns true if left operand is less than or equal to right operand.'
    },
    {
        pattern: />=/,
        color: '#DCDCDC',
        title: 'Greater Than or Equal',
        description: 'Returns true if left operand is greater than or equal to right operand.'
    },
    {
        pattern: /</,
        color: '#DCDCDC',
        title: 'Less Than',
        description: 'Returns true if the left operand is less than right operand, false otherwise.'
    },
    {
        pattern: />/,
        color: '#DCDCDC',
        title: 'Greater than operator',
        description: 'Returns true if the left operand is greater than right operand, false otherwise.'
    },
    {
        pattern: /\\/,
        color: '#569CD6',
        title: 'Lambda λ',
        description: 'A special form used to create anonymous, first-class functions (procedures) without a name, which take arguments and return a value or another procedure.'
    },
    {
        pattern: /\|/,
        color: '#DCDCDC',
        title: 'Cons operator',
        description: 'Destructures a list into it\'s HEAD|TAIL binding names to them. It can also be used to construct lists E.g X = [1|[2,3]]',
        nested: true  // Can be highlighted inside list patterns
    },
    
    // WILDCARD
    {
        pattern: /\b_\b/,
        color: '#777778',
        title: 'Wildcard',
        description: 'Matches any value but doesn\'t bind it to a name. Used when you don\'t need the value.',
        nested: true  // Can be highlighted inside list patterns
    }
];

// Initialize interactive docs on load
document.addEventListener('DOMContentLoaded', () => {
    setTimeout(initializeInteractiveDocs, 500);
});

function initializeInteractiveDocs() {
  const codeBlocks = document.querySelectorAll('.code-example-code');
  
  codeBlocks.forEach(block => {
    const example = block.closest('.code-example');
    if (!example) return;
    
    // Create documentation panel
    const docPanel = document.createElement('div');
    docPanel.className = 'code-doc-panel';
    example.appendChild(docPanel);
    
    // Make the code example position relative
    example.style.position = 'relative';
    
    // Process the code to make elements hoverable
    processCodeBlock(block, docPanel);
  });
}
function processCodeBlock(block, docPanel) {
  const code = block.textContent;
  
  // First pass: find all docstrings in the entire code
  const docstringRanges = findDocstrings(code);
  
  const lines = code.split('\n');
  
  // Clear and rebuild with interactive elements
  block.innerHTML = '';
  
  let charOffset = 0;
  lines.forEach((line) => {
    const lineDiv = document.createElement('div');
    lineDiv.className = 'code-line';
    
    const lineStart = charOffset;
    const lineEnd = charOffset + line.length;
    
    // Process each line for syntax elements
    const processedLine = processLineForSyntax(line, lineStart, docstringRanges);
    lineDiv.innerHTML = processedLine;
    
    block.appendChild(lineDiv);
    
    charOffset = lineEnd + 1; // +1 for the newline character
  });
}

function findDocstrings(code) {
  const ranges = [];
  // Match { followed by whitespace/newlines, then a string
  const regex = /\{\s*("(?:[^"\\]|\\.)*")/g;
  let match;
  
  while ((match = regex.exec(code)) !== null) {
    // The string starts at match.index + match[0].indexOf('"')
    const stringStart = match.index + match[0].indexOf('"');
    const stringEnd = stringStart + match[1].length;
    
    ranges.push({
      start: stringStart,
      end: stringEnd,
      text: match[1]
    });
  }
  
  return ranges;
}

function processLineForSyntax(line, lineStart, docstringRanges) {
  // Create an array of all matches with their positions
  const matches = [];
  
  syntaxDefinitions.forEach((def, defIndex) => {
    // Skip docstring pattern - we handle those separately
    if (def.title === 'Docstring') return;
    
    // Create a regex from the pattern
    const regex = new RegExp(def.pattern.source, 'g');
    let match;
    
    while ((match = regex.exec(line)) !== null) {
      const absoluteStart = lineStart + match.index;
      const absoluteEnd = absoluteStart + match[0].length;
      
      // Check if this match overlaps with a docstring
      const isInDocstring = docstringRanges.some(range => 
        absoluteStart >= range.start && absoluteEnd <= range.end
      );
      
      // If it's a regular string that's actually a docstring, skip it
      if (def.title === 'String Literal' && isInDocstring) {
        continue;
      }
      
      matches.push({
        start: match.index,
        end: match.index + match[0].length,
        text: match[0],
        def: def,
        defIndex: defIndex,
        allowNested: def.allowNested || false,
        isNested: def.nested || false
      });
    }
  });
  
  // Now add docstring matches for this line
  docstringRanges.forEach((range, idx) => {
    const lineEnd = lineStart + line.length;
    
    // Check if docstring overlaps with this line
    if (range.start <= lineEnd && range.end >= lineStart) {
      // Calculate the portion of the docstring on this line
      const matchStart = Math.max(0, range.start - lineStart);
      const matchEnd = Math.min(line.length, range.end - lineStart);
      
      if (matchStart < matchEnd) {
        matches.push({
          start: matchStart,
          end: matchEnd,
          text: line.substring(matchStart, matchEnd),
          def: {
            color: '#777778',
            title: 'Docstring',
            description: 'Function documentation string. the first element in a function body to document its purpose.'
          },
          defIndex: -1, // High priority
          allowNested: true,
          isNested: false
        });
      }
    }
  });
  
  // Sort matches by start position, then by definition order (earlier = higher priority)
  matches.sort((a, b) => {
    if (a.start !== b.start) return a.start - b.start;
    return a.defIndex - b.defIndex;
  });
  
  // Build hierarchy: allow nested patterns inside patterns marked with allowNested
  const processedMatches = [];
  
  for (const match of matches) {
    let shouldAdd = true;
    
    // Check if this match is inside another match
    for (const existing of processedMatches) {
      if (match.start >= existing.start && match.end <= existing.end) {
        // This match is inside an existing match
        if (existing.allowNested && match.isNested) {
          // Allow it as a nested match
          if (!existing.nested) {
            existing.nested = [];
          }
          existing.nested.push(match);
        }
        shouldAdd = false;
        break;
      }
    }
    
    if (shouldAdd) {
      processedMatches.push(match);
    }
  }
  
  // Build the HTML with highlighted spans
  let result = '';
  let currentPos = 0;
  
  for (const match of processedMatches) {
    // Add text before match
    if (match.start > currentPos) {
      result += escapeHtml(line.substring(currentPos, match.start));
    }
    
    // Add the match with potential nested elements
    result += buildMatchHtml(match, line);
    
    currentPos = match.end;
  }
  
  // Add remaining text
  if (currentPos < line.length) {
    result += escapeHtml(line.substring(currentPos));
  }
  
  return result;
}

function buildMatchHtml(match, line) {
  const escapedTitle = escapeHtml(match.def.title);
  const escapedDesc = escapeHtml(match.def.description);
  
  if (match.nested && match.nested.length > 0) {
    // This match contains nested elements
    let content = '';
    let pos = match.start;
    
    // Sort nested matches by position
    match.nested.sort((a, b) => a.start - b.start);
    
    for (const nested of match.nested) {
      // Add text before nested match
      if (nested.start > pos) {
        content += escapeHtml(line.substring(pos, nested.start));
      }
      
      // Add nested match
      const nestedTitle = escapeHtml(nested.def.title);
      const nestedDesc = escapeHtml(nested.def.description);
      const nestedText = escapeHtml(nested.text);
      
      content += `<span class="syntax-highlight" 
                       data-title="${nestedTitle}" 
                       data-description="${nestedDesc}"
                       data-color="${nested.def.color}"
                       style="color: ${nested.def.color}; cursor: help;">${nestedText}</span>`;
      
      pos = nested.end;
    }
    
    // Add remaining text
    if (pos < match.end) {
      content += escapeHtml(line.substring(pos, match.end));
    }
    
    // Wrap in parent span
    return `<span class="syntax-highlight" 
                 data-title="${escapedTitle}" 
                 data-description="${escapedDesc}"
                 data-color="${match.def.color}"
                 style="color: ${match.def.color}; cursor: help;">${content}</span>`;
  } else {
    // Simple match without nesting
    const escapedText = escapeHtml(match.text);
    return `<span class="syntax-highlight" 
                 data-title="${escapedTitle}" 
                 data-description="${escapedDesc}"
                 data-color="${match.def.color}"
                 style="color: ${match.def.color}; cursor: help;">${escapedText}</span>`;
  }
}

function escapeHtml(text) {
  const div = document.createElement('div');
  div.textContent = text;
  return div.innerHTML;
}

// Set up event delegation for hover effects
document.addEventListener('mouseover', (e) => {
  if (e.target.classList.contains('syntax-highlight')) {
    // Stop propagation so we only show the innermost element's docs
    e.stopPropagation();
    
    const title = e.target.dataset.title;
    const description = e.target.dataset.description;
    const color = e.target.dataset.color;
    
    const example = e.target.closest('.code-example');
    const panel = example.querySelector('.code-doc-panel');
    
    showDocumentation(panel, { title, description, color }, e.target);
  }
});

document.addEventListener('mouseout', (e) => {
  if (e.target.classList.contains('syntax-highlight')) {
    const example = e.target.closest('.code-example');
    const panel = example.querySelector('.code-doc-panel');
    
    hideDocumentation(panel);
  }
});

function showDocumentation(panel, def, element) {
  // Process description to highlight keywords
  let processedDescription = def.description;
  
  // Replace each keyword with colored span
  Object.entries(docKeywords).forEach(([keyword, color]) => {
    const regex = new RegExp(`\\b${keyword}\\b`, 'g');
    processedDescription = processedDescription.replace(
      regex, 
      `<span style="color: ${color}; font-weight: 500;">${keyword}</span>`
    );
  });
  
  panel.innerHTML = `
    <div class="doc-content">
      <div class="doc-title" style="color: ${def.color};">
        ${def.title}
      </div>
      <div class="doc-description">
        ${processedDescription}
      </div>
    </div>
  `;
  
  // Get exact position of the hovered element
  const rect = element.getBoundingClientRect();
  
  // Position panel to the right of element with 20px gap
  const panelLeft = rect.right + 20;
  const panelTop = rect.top;
  
  panel.style.left = `${panelLeft}px`;
  panel.style.top = `${panelTop}px`;
  
  // Arrow points exactly at the end (right edge) of the hovered element
  // Calculate arrow position: element's vertical center relative to panel top
  const arrowTop = rect.height / 2;
  
  panel.style.setProperty('--arrow-top', `${arrowTop}px`);
  panel.style.setProperty('--arrow-color', def.color);
  panel.style.borderLeftColor = def.color;
  
  panel.classList.add('show');
  
  // Add glow effect to the hovered element
  element.style.textShadow = `0 0 8px ${def.color}`;
}

function hideDocumentation(panel) {
  panel.classList.remove('show');
  
  // Remove glow from all elements
  document.querySelectorAll('.syntax-highlight').forEach(el => {
    el.style.textShadow = '';
  });
}
