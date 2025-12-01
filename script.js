const API_BASE = '/api';

// Session management (replaces localStorage manipulation)
let sessionToken = null;
let currentUsername = null;
const solvedProblems = new Set();
let currentlyOpenProblem = null;

// Initialize
document.addEventListener('DOMContentLoaded', async () => {
  // Try to restore session from sessionStorage (more secure than localStorage)
  const savedToken = sessionStorage.getItem('sessionToken');
  const savedUsername = sessionStorage.getItem('username');
  
  if (savedToken && savedUsername) {
    sessionToken = savedToken;
    currentUsername = savedUsername;
    document.getElementById('usernameInput').value = savedUsername;
    document.getElementById('usernameInput').disabled = true;
    
    // Verify session is still valid
    await checkAllSubmissions();
  }
  
  updateProgress();
  
  // Load solutions for all problems
  document.querySelectorAll('.problem').forEach(problem => {
    const problemId = problem.dataset.problemId;
    loadSolutions(problemId);
  });
});

// Username handling - now requires login
const usernameInput = document.getElementById('usernameInput');
const usernameSaved = document.getElementById('usernameSaved');

usernameInput.addEventListener('keypress', async (e) => {
  if (e.key === 'Enter') {
    await loginUser();
  }
});

usernameInput.addEventListener('blur', async () => {
  const username = usernameInput.value.trim();
  if (username && !sessionToken) {
    await loginUser();
  }
});

async function loginUser() {
  const username = usernameInput.value.trim();
  
  if (!username) {
    showNotification('Please enter a username', 'error');
    return;
  }
  
  if (username === 'Anonymous') {
    showNotification('Please choose a unique username', 'error');
    return;
  }
  
  try {
    const response = await fetch(`${API_BASE}/login`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ username })
    });
    
    const data = await response.json();
    
    if (response.ok) {
      sessionToken = data.token;
      currentUsername = data.username;
      
      // Save to sessionStorage (cleared on browser close)
      sessionStorage.setItem('sessionToken', sessionToken);
      sessionStorage.setItem('username', currentUsername);
      
      usernameInput.disabled = true;
      
      if (data.newUser) {
        showNotification(`Welcome, ${currentUsername}! Your username has been reserved.`, 'success');
      } else {
        showNotification(`Welcome back, ${currentUsername}!`, 'success');
      }
      
      // Check submissions with valid session
      await checkAllSubmissions();
    } else {
      showNotification(data.error || 'Username already taken. Please choose another.', 'error');
      sessionToken = null;
      currentUsername = null;
    }
  } catch (error) {
    showNotification('Error connecting to server', 'error');
    console.error('Login error:', error);
  }
}

// Check submissions from server
async function checkAllSubmissions() {
  if (!currentUsername) return;
  
  const problems = document.querySelectorAll('.problem');
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
        
        const submitButton = problem.querySelector('.btn-secondary');
        submitButton.disabled = true;
        submitButton.innerHTML = '✓ Already Submitted';
        submitButton.style.background = '#4ec9b0';
        submitButton.style.color = '#1e1e1e';
      } else {
        problem.classList.remove('solved');
        const badge = problem.querySelector('.status-badge');
        badge.style.display = 'none';
      }
    } catch (error) {
      console.error(`Error checking submission for ${problemId}:`, error);
    }
  }
  
  updateProgress();
}

function toggleProblem(problemId) {
  const problem = document.querySelector(`[data-problem-id="${problemId}"]`);
  const content = problem.querySelector('.problem-content');
  
  if (currentlyOpenProblem && currentlyOpenProblem !== problemId) {
    const prevProblem = document.querySelector(`[data-problem-id="${currentlyOpenProblem}"]`);
    const prevContent = prevProblem.querySelector('.problem-content');
    prevContent.classList.remove('expanded');
  }
  
  const isExpanding = !content.classList.contains('expanded');
  content.classList.toggle('expanded');
  
  currentlyOpenProblem = isExpanding ? problemId : null;
  
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

// Run tests - now requires session
async function runTests(problemId) {
  // Check if logged in
  if (!sessionToken) {
    showNotification('Please log in with a username first', 'error');
    document.getElementById('usernameInput').focus();
    return;
  }
  
  const problem = document.querySelector(`[data-problem-id="${problemId}"]`);
  const code = problem.querySelector('.editor').value.trim();
  const resultsDiv = problem.querySelector('.test-results');
  const runButton = problem.querySelector('.btn-primary');
  const submitButton = problem.querySelector('.btn-secondary');
  
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
  
  runButton.disabled = true;
  runButton.innerHTML = '<span class="spinner"></span> Running...';
  
  resultsDiv.style.display = 'block';
  resultsDiv.innerHTML = '<div class="test-case running"><span class="spinner"></span> Running tests...</div>';

  try {
    const response = await fetch(`${API_BASE}/test`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ 
        problemId, 
        code,
        sessionToken 
      })
    });

    if (response.status === 401) {
      showNotification('Session expired. Please log in again.', 'error');
      sessionToken = null;
      currentUsername = null;
      sessionStorage.clear();
      usernameInput.disabled = false;
      return;
    }

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
      // Check if already submitted
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

async function checkIfAlreadySubmitted(problemId) {
  if (!currentUsername) return false;
  
  try {
    const response = await fetch(`${API_BASE}/check-submission/${problemId}/${currentUsername}`);
    const data = await response.json();
    return data.hasSubmitted;
  } catch (error) {
    console.error('Error checking submission:', error);
    return false;
  }
}

// Submit solution - server validates tests again
async function submitSolution(problemId) {
  if (!sessionToken) {
    showNotification('Please log in first', 'error');
    return;
  }
  
  const problem = document.querySelector(`[data-problem-id="${problemId}"]`);
  const code = problem.querySelector('.editor').value;
  const submitButton = problem.querySelector('.btn-secondary');

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
        sessionToken
      })
    });

    const data = await response.json();

    if (response.status === 401) {
      showNotification('Session expired. Please log in again.', 'error');
      sessionToken = null;
      currentUsername = null;
      sessionStorage.clear();
      usernameInput.disabled = false;
      submitButton.disabled = false;
      submitButton.innerHTML = 'Submit Solution';
      return;
    }

    if (response.status === 403) {
      showNotification('Tests must pass on the server before submission. Please run tests again.', 'error');
      submitButton.disabled = false;
      submitButton.innerHTML = 'Submit Solution';
      return;
    }

    if (response.ok && data.success) {
      problem.classList.add('solved');
      const badge = problem.querySelector('.status-badge');
      badge.style.display = 'inline-block';
      solvedProblems.add(problemId);
      updateProgress();
      
      submitButton.innerHTML = '✓ Submitted';
      submitButton.style.background = '#4ec9b0';
      submitButton.style.color = '#1e1e1e';
      
      confetti({
        particleCount: 100,
        spread: 70,
        origin: { y: 0.6 }
      });
      
      showNotification('Solution submitted successfully! 🎉', 'success');
      
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
