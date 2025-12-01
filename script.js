const API_BASE = '/api';

// Session management
let sessionToken = null;
let currentUsername = null;
let isVerifiedUser = false;
const solvedProblems = new Set();
let currentlyOpenProblem = null;
let problems = [];

// Track empty test attempts per problem
const emptyTestAttempts = new Map();

// Initialize
document.addEventListener('DOMContentLoaded', async () => {
  // Always load problems first (no login required to view)
  await loadProblems();
  
  // Try to restore session from sessionStorage
  const savedToken = sessionStorage.getItem('sessionToken');
  const savedUsername = sessionStorage.getItem('username');
  
  if (savedToken && savedUsername) {
    sessionToken = savedToken;
    currentUsername = savedUsername;
    isVerifiedUser = sessionStorage.getItem('verified') === 'true';
    document.getElementById('usernameInput').value = savedUsername;
    document.getElementById('usernameInput').disabled = true;
    document.getElementById('passwordInput').disabled = true;
    document.getElementById('loginButton').style.display = 'none';
    
    // Verify session is still valid
    const isValid = await verifySession();
    if (!isValid) {
      showNotification('Session expired. Please log in again.', 'error');
      clearSession();
    } else {
      // Check submissions if logged in
      await checkAllSubmissions();
    }
  }
  
  updateProgress();
});

// Load problems from API
async function loadProblems() {
  try {
    const response = await fetch(`${API_BASE}/problems`);
    problems = await response.json();
    renderProblems();
  } catch (error) {
    console.error('Error loading problems:', error);
    showNotification('Error loading problems from server', 'error');
  }
}

// Render problems dynamically
function renderProblems() {
  const container = document.querySelector('.container');
  
  // Group problems by section
  const sections = {};
  problems.forEach(problem => {
    const section = problem.section || 'Working with Lists';
    if (!sections[section]) {
      sections[section] = [];
    }
    sections[section].push(problem);
  });
  
  // Find where to insert (after difficulty legend)
  const difficultyLegend = document.querySelector('.difficulty-legend');
  
  // Clear existing problems
  const existingProblems = document.querySelectorAll('.problem, .section-header');
  existingProblems.forEach(el => el.remove());
  
  // Keep track of where to insert next
  let lastElement = difficultyLegend;
  
  // Render each section
  Object.keys(sections).forEach(sectionName => {
    const sectionHeader = document.createElement('div');
    sectionHeader.className = 'section-header';
    sectionHeader.textContent = sectionName;
    
    // Insert section header after the last element
    lastElement.insertAdjacentElement('afterend', sectionHeader);
    lastElement = sectionHeader;
    
    // Insert problems in correct order (after section header)
    sections[sectionName].forEach(problem => {
      const problemEl = createProblemElement(problem);
      lastElement.insertAdjacentElement('afterend', problemEl);
      lastElement = problemEl; // Update last element to the one we just inserted
    });
  });
}

// Create problem HTML element
function createProblemElement(problem) {
  const problemDiv = document.createElement('div');
  problemDiv.className = 'problem';
  problemDiv.dataset.problemId = problem.problemId;
  
  const difficultyClass = `difficulty-${problem.problemDifficulty}`;
  const difficultySymbol = {
    'easy': '(*)',
    'medium': '(**)',
    'hard': '(***)'
  }[problem.problemDifficulty] || '(*)';
  
  problemDiv.innerHTML = `
    <div class="problem-header" onclick="toggleProblem('${problem.problemId}')">
      <span class="problem-number">${problem.problemNumber}</span>
      <span class="problem-difficulty ${difficultyClass}">${difficultySymbol}</span>
      <span class="problem-title">${escapeHtml(problem.problemTitle)}</span>
      <span class="status-badge solved" style="display: none;">✓ Solved</span>
    </div>
    <div class="problem-content">
      <div class="problem-description">
        ${escapeHtml(problem.problemDescription)}
      </div>
      <div class="problem-example">
        <div class="example-label">Example:</div>
        <div class="code"><span class="function">${escapeHtml(problem.problemExample.code)}</span>
<span class="comment">-- Result: ${escapeHtml(problem.problemExample.result)}</span></div>
      </div>

      <div class="editor-container">
        <div class="editor-header">
          <span>Your Solution</span>
          <span class="editor-language">
            <span class="logo-wrapper"><img src="/logo.png" alt="" class="editor-logo"></span>
            Amortia
          </span>
        </div>
        <textarea class="editor" data-problem="${problem.problemId}" placeholder="${escapeHtml(problem.problemFunctionSignature)}"></textarea>
      </div>

      <div class="button-group">
        <button class="btn btn-primary" onclick="runTests('${problem.problemId}')">
          <span class="btn-text">Run Tests</span>
        </button>
        <button class="btn btn-secondary" onclick="submitSolution('${problem.problemId}')" disabled>
          Submit Solution
        </button>
      </div>

      <div class="test-results" style="display: none;"></div>

      <div class="solutions-section">
        <div class="solutions-header">Community Solutions</div>
        <div class="solutions-content">
          <div class="locked-message">
            <div class="locked-icon">🔒</div>
            <div>Submit your solution to see other people's solutions!</div>
          </div>
        </div>
      </div>
    </div>
  `;
  
  return problemDiv;
}

// Verify if session is still valid
async function verifySession() {
  if (!sessionToken) return false;
  
  try {
    const response = await fetch(`${API_BASE}/verify-session`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ verifyToken: sessionToken })
    });
    
    return response.ok;
  } catch (error) {
    return false;
  }
}

// Clear session state
function clearSession() {
  sessionToken = null;
  currentUsername = null;
  isVerifiedUser = false;
  sessionStorage.clear();
  const usernameInput = document.getElementById('usernameInput');
  const passwordInput = document.getElementById('passwordInput');
  const loginButton = document.getElementById('loginButton');
  usernameInput.value = '';
  passwordInput.value = '';
  usernameInput.disabled = false;
  passwordInput.disabled = false;
  loginButton.style.display = 'inline-block';
  usernameInput.focus();
}

// Username handling
const usernameInput = document.getElementById('usernameInput');
const passwordInput = document.getElementById('passwordInput');
const loginButton = document.getElementById('loginButton');

loginButton.addEventListener('click', async () => {
  await loginUser();
});

usernameInput.addEventListener('keypress', async (e) => {
  if (e.key === 'Enter') {
    passwordInput.focus();
  }
});

passwordInput.addEventListener('keypress', async (e) => {
  if (e.key === 'Enter') {
    await loginUser();
  }
});

async function loginUser() {
  const username = usernameInput.value.trim();
  const password = passwordInput.value.trim();
  
  if (!username) {
    showNotification('Please enter a username', 'error');
    return;
  }
  
  if (username === 'Anonymous') {
    showNotification('Please choose a unique username', 'error');
    return;
  }
  
  loginButton.disabled = true;
    
  try {
    const response = await fetch(`${API_BASE}/login`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ 
        username,
        password: password || null
      })
    });
    
    const data = await response.json();
    
    if (response.ok) {
      sessionToken = data.token;
      currentUsername = data.username;
      isVerifiedUser = data.verified || false;
      
      sessionStorage.setItem('sessionToken', sessionToken);
      sessionStorage.setItem('username', currentUsername);
      sessionStorage.setItem('verified', isVerifiedUser);
      
      usernameInput.disabled = true;
      passwordInput.disabled = true;
      loginButton.style.display = 'none';
      
      if (data.newUser) {
        if (isVerifiedUser) {
          showNotification(`✓ Welcome, ${currentUsername}! Your account is verified.`, 'success');
        } else {
          showNotification(`Welcome, ${currentUsername}! - ⚠️ No password set!`, 'success');
        }
      } else {
        if (isVerifiedUser) {
          showNotification(`✓ Welcome back, ${currentUsername}!`, 'success');
        } else {
          showNotification(`Welcome back, ${currentUsername}! ⚠️ Unprotected account.`, 'success');
        }
      }
      
      await checkAllSubmissions();
    } else {
      showNotification(data.error || 'Login failed', 'error');
      sessionToken = null;
      currentUsername = null;
      isVerifiedUser = false;
    }
  } catch (error) {
    showNotification('Error connecting to server', 'error');
    console.error('Login error:', error);
  } finally {
    loginButton.disabled = false;
    loginButton.innerHTML = 'Login';
  }
}

// Check submissions from server
async function checkAllSubmissions() {
  if (!currentUsername) return;
  
  const problemElements = document.querySelectorAll('.problem');
  solvedProblems.clear();
  
  for (const problem of problemElements) {
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
        
        loadSolutions(problemId);
      } else {
        problem.classList.remove('solved');
        const badge = problem.querySelector('.status-badge');
        badge.style.display = 'none';
        
        loadSolutions(problemId);
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

// Run tests
async function runTests(problemId) {
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
    const attempts = (emptyTestAttempts.get(problemId) || 0) + 1;
    emptyTestAttempts.set(problemId, attempts);
    
    const messages = [
      { emoji: '👀', title: 'Are you trying to test empty code?', subtitle: 'Write some code first, then run the tests!' },
      { emoji: '🤨', title: 'Really? Empty code again?', subtitle: 'I\'m starting to think you\'re doing this on purpose...' },
      { emoji: '😐', title: 'Okay, this is getting ridiculous.', subtitle: 'Please. Just write. Some. Code.' },
      { emoji: '😤', title: 'ARE YOU KIDDING ME?!', subtitle: 'HOW MANY TIMES DO I NEED TO TELL YOU?!' },
      { emoji: '🤬', title: 'I AM GOING TO LOSE IT', subtitle: 'WRITE. CODE. IN. THE. EDITOR. THEN. CLICK. RUN.' },
      { emoji: '💀', title: 'You know what? Fine.', subtitle: 'You win. I give up. Do whatever you want.' },
      { emoji: '🪦', title: '...', subtitle: '' }
    ];
    
    let messageIndex = Math.min(attempts - 1, messages.length - 1);
    let message = messages[messageIndex];
    
    if (attempts >= 15) {
      const total = problems.length;
      const solved = solvedProblems.size;
      message = {
        emoji: '🔥',
        title: 'You like to joke around a lot...',
        subtitle: `...for someone who has only completed ${solved}/${total} problems. Go write some code.`
      };
    }
    
    if (attempts >= 16) {
      resultsDiv.style.display = 'block';
      resultsDiv.innerHTML = `
        <div style="padding: 20px; text-align: center;">
          <div style="font-size: 32px; margin-bottom: 12px;">🎵</div>
          <div style="color: #dcdcaa; font-size: 14px; font-weight: 600; margin-bottom: 16px;">
            You know the rules, and so do I...
          </div>
          <video id="rickrollVideo" width="100%" style="max-width: 560px; border-radius: 8px; pointer-events: none;" loop muted disablepictureinpicture>
            <source src="https://ia804503.us.archive.org/15/items/kikTXNL6MvX6ZpRXM/kikTXNL6MvX6ZpRXM.mp4" type="video/mp4">
          </video>
          <audio id="rickrollAudio" loop style="display: none;">
            <source src="https://ia804503.us.archive.org/15/items/kikTXNL6MvX6ZpRXM/kikTXNL6MvX6ZpRXM.mp4" type="audio/mp4">
          </audio>
          <div style="color: #858585; font-size: 12px; margin-top: 12px;">
            Maybe this will inspire you to actually write some code.
          </div>
        </div>
      `;
      
      setTimeout(() => {
        const video = document.getElementById('rickrollVideo');
        const audio = document.getElementById('rickrollAudio');
        
        if (video && audio) {
          Promise.all([
            new Promise(resolve => {
              if (video.readyState >= 3) resolve();
              else video.addEventListener('canplay', resolve, { once: true });
            }),
            new Promise(resolve => {
              if (audio.readyState >= 3) resolve();
              else audio.addEventListener('canplay', resolve, { once: true });
            })
          ]).then(() => {
            video.currentTime = 0;
            audio.currentTime = 0;
            video.play();
            audio.play();
          });
        }
      }, 100);
      
      return;
    }
    
    resultsDiv.style.display = 'block';
    resultsDiv.innerHTML = `
      <div style="padding: 20px; text-align: center;">
        <div style="font-size: 32px; margin-bottom: 12px;">${message.emoji}</div>
        <div style="color: #dcdcaa; font-size: 14px; font-weight: 600;">
          ${message.title}
        </div>
        ${message.subtitle ? `<div style="color: #858585; font-size: 12px; margin-top: 8px;">
          ${message.subtitle}
        </div>` : ''}
      </div>
    `;
    return;
  }
  
  emptyTestAttempts.set(problemId, 0);
  
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
      clearSession();
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

// Submit solution
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
      clearSession();
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

  const hasSubmitted = solvedProblems.has(problemId);

  if (!hasSubmitted) {
    solutionsContent.innerHTML = '<div class="locked-message"><div class="locked-icon">🔒</div><div>Submit your solution to see other people\'s solutions!</div></div>';
    return;
  }

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
      const verified = sol.verified || false;
      
      const timestamp = submittedAt ? new Date(submittedAt).toLocaleString() : 'Unknown date';
      const verifiedBadge = verified ? '<span class="verified-badge" title="Verified account with password">✓ Verified</span>' : '';
      
      html += `
        <div class="solution-card">
          <div class="solution-meta">
            <span class="solution-author">by ${escapeHtml(username)} ${verifiedBadge}</span>
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
  const total = problems.length;
  const solved = solvedProblems.size;
  const percent = total > 0 ? (solved / total * 100).toFixed(1) : 0;
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
