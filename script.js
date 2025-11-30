const API_BASE = 'http://localhost:3000/api';

// Persistent storage using localStorage
const solvedProblems = new Set();
const testedProblems = new Set();
let username = 'Anonymous';
let currentlyOpenProblem = null;

// Initialize
document.addEventListener('DOMContentLoaded', async () => {
  // Load username from localStorage
  const savedUsername = localStorage.getItem('username');
  const usernameInput = document.getElementById('usernameInput');
  if (savedUsername) {
    username = savedUsername;
    usernameInput.value = savedUsername;
  }
  
  // Load tested problems from localStorage (these don't need server verification)
  const savedTested = localStorage.getItem('testedProblems');
  if (savedTested) {
    const tested = JSON.parse(savedTested);
    tested.forEach(id => testedProblems.add(id));
  }
  
  // Check for already submitted solutions on page load
  // This will verify against the server and update solvedProblems
  await checkAllSubmissions();
  
  // Enable submit buttons for tested problems that aren't solved
  testedProblems.forEach(problemId => {
    if (!solvedProblems.has(problemId)) {
      const problem = document.querySelector(`[data-problem-id="${problemId}"]`);
      if (problem) {
        const submitButton = problem.querySelector('.btn-secondary');
        submitButton.disabled = false;
      }
    }
  });
  
  updateProgress();
  
  // Load all solutions for solved problems
  document.querySelectorAll('.problem').forEach(problem => {
    const problemId = problem.dataset.problemId;
    loadSolutions(problemId);
  });
});

// Check if user has already submitted each problem
async function checkAllSubmissions() {
  const problems = document.querySelectorAll('.problem');
  const currentUsername = getUsername();
  
  // Clear solvedProblems - we'll rebuild it from server data
  solvedProblems.clear();
  
  for (const problem of problems) {
    const problemId = problem.dataset.problemId;
    try {
      const response = await fetch(`${API_BASE}/check-submission/${problemId}/${currentUsername}`);
      const data = await response.json();
      
      if (data.hasSubmitted) {
        solvedProblems.add(problemId);
        
        problem.classList.add('solved');
        const badge = problem.querySelector('.status-badge');
        badge.style.display = 'inline-block';
        
        // Disable the submit button and show it's already submitted
        const submitButton = problem.querySelector('.btn-secondary');
        submitButton.disabled = true;
        submitButton.innerHTML = '✓ Already Submitted';
        submitButton.style.background = '#4ec9b0';
        submitButton.style.color = '#1e1e1e';
      } else {
        // Make sure it's not marked as solved if server says it's not
        problem.classList.remove('solved');
        const badge = problem.querySelector('.status-badge');
        badge.style.display = 'none';
      }
    } catch (error) {
      console.error(`Error checking submission for ${problemId}:`, error);
    }
  }
  
  // Save the verified solved problems to localStorage
  saveSolvedProblems();
  updateProgress();
}

// Username handling
const usernameInput = document.getElementById('usernameInput');
const usernameSaved = document.getElementById('usernameSaved');

usernameInput.addEventListener('input', () => {
  const newUsername = usernameInput.value.trim() || 'Anonymous';
  username = newUsername;
  localStorage.setItem('username', newUsername);
  
  usernameSaved.style.display = 'inline';
  setTimeout(() => {
    usernameSaved.style.display = 'none';
  }, 2000);
  
  // Re-check submissions with new username
  checkAllSubmissions();
});

function getUsername() {
  return username;
}

// Save to localStorage
function saveSolvedProblems() {
  localStorage.setItem('solvedProblems', JSON.stringify([...solvedProblems]));
}

function saveTestedProblems() {
  localStorage.setItem('testedProblems', JSON.stringify([...testedProblems]));
}

function toggleProblem(problemId) {
  const problem = document.querySelector(`[data-problem-id="${problemId}"]`);
  const content = problem.querySelector('.problem-content');
  
  // Close previously open problem if it's different
  if (currentlyOpenProblem && currentlyOpenProblem !== problemId) {
    const prevProblem = document.querySelector(`[data-problem-id="${currentlyOpenProblem}"]`);
    const prevContent = prevProblem.querySelector('.problem-content');
    prevContent.classList.remove('expanded');
  }
  
  // Toggle current problem
  const isExpanding = !content.classList.contains('expanded');
  content.classList.toggle('expanded');
  
  // Update tracking
  currentlyOpenProblem = isExpanding ? problemId : null;
  
  // Smooth scroll to problem if expanding and not fully visible
  if (isExpanding) {
    setTimeout(() => {
      const rect = problem.getBoundingClientRect();
      const isVisible = rect.top >= 0 && rect.bottom <= window.innerHeight;
      
      if (!isVisible) {
        problem.scrollIntoView({ behavior: 'smooth', block: 'center' });
      }
    }, 250);
  }
}

// Run tests
async function runTests(problemId) {
  const problem = document.querySelector(`[data-problem-id="${problemId}"]`);
  const code = problem.querySelector('.editor').value.trim();
  const resultsDiv = problem.querySelector('.test-results');
  const runButton = problem.querySelector('.btn-primary');
  const submitButton = problem.querySelector('.btn-secondary');
  
  // Check for empty input
  if (!code) {
    resultsDiv.style.display = 'block';
    resultsDiv.innerHTML = `
      <div style="padding: 20px; text-align: center;">
        <div style="font-size: 32px; margin-bottom: 12px;">👀</div>
        <div style="color: #dcdcaa; font-size: 14px; font-weight: 600;">
          Are you trying to test empty code? What do you expect?
        </div>
        <div style="color: #858585; font-size: 12px; margin-top: 8px;">
          Write some code first, then run the tests!
        </div>
      </div>
    `;
    return;
  }
  
  // Disable button and show loading
  runButton.disabled = true;
  runButton.innerHTML = '<span class="spinner"></span> Running...';
  
  resultsDiv.style.display = 'block';
  resultsDiv.innerHTML = '<div class="test-case running"><span class="spinner"></span> Running tests...</div>';

  try {
    const response = await fetch(`${API_BASE}/test`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ problemId, code })
    });

    const data = await response.json();
    
    let html = '<div style="margin-bottom: 12px; font-weight: 600; color: #4ec9b0;">Test Results:</div>';
    data.results.forEach(test => {
      const errorDisplay = test.testError 
        ? `<div style="margin-left: 22px; margin-top: 4px; font-size: 11px; color: #f48771; white-space: pre-wrap;">${escapeHtml(test.testError)}</div>` 
        : '';
      
      html += `
        <div class="test-case ${test.testPassed ? 'passed' : 'failed'}">
          <span class="test-case-icon">${test.testPassed ? '✓' : '✗'}</span>
          <span>${escapeHtml(test.testName)}</span>
          ${errorDisplay}
        </div>
      `;
    });

    resultsDiv.innerHTML = html;

    if (data.allPassed) {
      testedProblems.add(problemId);
      saveTestedProblems();
      
      // Check if already submitted before enabling submit button
      const hasSubmitted = await checkIfAlreadySubmitted(problemId);
      
      if (!hasSubmitted) {
        submitButton.disabled = false;
        submitButton.innerHTML = 'Submit Solution';
        submitButton.classList.add('pulse');
        setTimeout(() => submitButton.classList.remove('pulse'), 2000);
        
        resultsDiv.innerHTML += '<div style="margin-top: 16px; padding: 12px; background: rgba(78, 201, 176, 0.1); border-left: 3px solid #4ec9b0; border-radius: 3px; color: #4ec9b0; font-weight: 600;">✓ All tests passed! You can now submit your solution.</div>';
      } else {
        resultsDiv.innerHTML += '<div style="margin-top: 16px; padding: 12px; background: rgba(78, 201, 176, 0.1); border-left: 3px solid #4ec9b0; border-radius: 3px; color: #4ec9b0; font-weight: 600;">✓ All tests passed! (You have already submitted a solution for this problem)</div>';
      }
    } else {
      testedProblems.delete(problemId);
      saveTestedProblems();
      submitButton.disabled = true;
      submitButton.classList.remove('pulse');
    }

  } catch (error) {
    resultsDiv.innerHTML = `<div style="color: #f48771;">Error running tests: ${escapeHtml(error.message)}<br><br>Make sure the server is running!</div>`;
  } finally {
    runButton.disabled = false;
    runButton.innerHTML = '<span class="btn-text">Run Tests</span>';
  }
}

// Check if user already submitted this problem
async function checkIfAlreadySubmitted(problemId) {
  try {
    const currentUsername = getUsername();
    const response = await fetch(`${API_BASE}/check-submission/${problemId}/${currentUsername}`);
    const data = await response.json();
    return data.hasSubmitted;
  } catch (error) {
    console.error('Error checking submission:', error);
    return false;
  }
}

// Submit solution
async function submitSolution(problemId) {
  const problem = document.querySelector(`[data-problem-id="${problemId}"]`);
  const code = problem.querySelector('.editor').value;
  const submitButton = problem.querySelector('.btn-secondary');

  // Check if tests were passed
  if (!testedProblems.has(problemId)) {
    showNotification('Please run and pass all tests first!', 'error');
    return;
  }

  // Check if already submitted
  const hasSubmitted = await checkIfAlreadySubmitted(problemId);
  if (hasSubmitted) {
    showNotification('You have already submitted a solution for this problem', 'error');
    return;
  }

  submitButton.disabled = true;
  submitButton.innerHTML = '<span class="spinner"></span> Submitting...';

  try {
    const response = await fetch(`${API_BASE}/submit`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ 
        problemId, 
        code,
        username: getUsername()
      })
    });

    const data = await response.json();

    if (response.ok && data.success) {
      problem.classList.add('solved');
      const badge = problem.querySelector('.status-badge');
      badge.style.display = 'inline-block';
      solvedProblems.add(problemId);
      saveSolvedProblems();
      updateProgress();
      
      // Update button to show it's submitted
      submitButton.innerHTML = '✓ Submitted';
      submitButton.style.background = '#4ec9b0';
      submitButton.style.color = '#1e1e1e';
      
      // Confetti celebration!
      confetti({
        particleCount: 100,
        spread: 70,
        origin: { y: 0.6 }
      });
      
      showNotification('Solution submitted successfully! 🎉', 'success');
      
      // Load solutions after a brief delay
      setTimeout(() => loadSolutions(problemId), 500);
    } else {
      showNotification(data.error || 'Error submitting solution.', 'error');
      submitButton.disabled = false;
      submitButton.innerHTML = 'Submit Solution';
    }
  } catch (error) {
    showNotification('Error submitting solution. Please try again.', 'error');
    submitButton.disabled = false;
    submitButton.innerHTML = 'Submit Solution';
  }
}

// Show notification
function showNotification(message, type) {
  const notification = document.getElementById('notification');
  notification.textContent = message;
  notification.className = `notification ${type} show`;
  
  setTimeout(() => {
    notification.classList.remove('show');
  }, 3000);
}

async function loadSolutions(problemId) {
  const problem = document.querySelector(`[data-problem-id="${problemId}"]`);
  const solutionsContent = problem.querySelector('.solutions-content');

  try {
    const response = await fetch(`${API_BASE}/solutions/${problemId}`);
    const solutions = await response.json();

    if (solutions.length === 0) {
      solutionsContent.innerHTML = '<div class="locked-message"><div class="locked-icon">🔒</div><div>No solutions yet. Be the first to submit!</div></div>';
      return;
    }

    // Build solutions HTML
    let html = '';
    solutions.forEach(sol => {
      const code = sol.code || '';
      const username = sol.username || 'Anonymous';
      const submittedAt = sol.submittedAt;
      
      const timestamp = submittedAt ? new Date(submittedAt).toLocaleString() : 'Unknown date';
      
      html += `
        <div class="solution-card">
          <div class="solution-meta">
            <span class="solution-author">by ${escapeHtml(username)}</span>
            <span class="solution-time">${timestamp}</span>
          </div>
          <div class="solution-code"><pre class="code">${escapeHtml(code)}</pre></div>
        </div>
      `;
    });

    solutionsContent.innerHTML = html;
  } catch (error) {
    console.error('Error loading solutions:', error);
  }
}

// Update progress bar
function updateProgress() {
  const total = document.querySelectorAll('.problem').length;
  const solved = solvedProblems.size;
  const percent = (solved / total * 100).toFixed(1);
  const progressBar = document.getElementById('progressBar');
  const progressText = document.getElementById('progressText');
  
  progressBar.style.width = percent + '%';
  progressText.textContent = `${solved} / ${total} completed`;
}

function escapeHtml(text) {
  const div = document.createElement('div');
  div.textContent = text;
  return div.innerHTML;
}
