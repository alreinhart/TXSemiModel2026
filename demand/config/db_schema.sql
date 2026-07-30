-- Database schema for Semiconductor Jobs Database
-- SQLite 3.x compatible

-- Companies table: stores company metadata
CREATE TABLE IF NOT EXISTS companies (
    company_id INTEGER PRIMARY KEY AUTOINCREMENT,
    company_name TEXT NOT NULL UNIQUE,
    careers_url TEXT NOT NULL,
    platform TEXT NOT NULL,  -- workday, oracle, custom
    active BOOLEAN DEFAULT 1,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Jobs table: main table storing all job listings
CREATE TABLE IF NOT EXISTS jobs (
    job_id INTEGER PRIMARY KEY AUTOINCREMENT,
    company_id INTEGER NOT NULL,
    job_title TEXT NOT NULL,
    job_url TEXT UNIQUE NOT NULL,
    location TEXT,
    job_responsibilities TEXT,
    min_education TEXT,
    min_experience TEXT,
    preferred_qualifications TEXT,
    salary_range TEXT,
    job_identification TEXT,
    job_category TEXT,
    degree_level TEXT,
    ecl_gtc_required TEXT,
    essential_skills TEXT,
    posting_date DATE,
    scrape_run_id INTEGER NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (company_id) REFERENCES companies (company_id),
    FOREIGN KEY (scrape_run_id) REFERENCES scrape_runs (run_id)
);

-- Scrape runs table: tracks each scraping session
CREATE TABLE IF NOT EXISTS scrape_runs (
    run_id INTEGER PRIMARY KEY AUTOINCREMENT,
    run_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    company_id INTEGER,
    jobs_found INTEGER DEFAULT 0,
    jobs_new INTEGER DEFAULT 0,
    jobs_updated INTEGER DEFAULT 0,
    status TEXT DEFAULT 'running',  -- running, completed, failed
    error_message TEXT,
    duration_seconds INTEGER,
    FOREIGN KEY (company_id) REFERENCES companies (company_id)
);

-- Create indexes for faster queries
CREATE INDEX IF NOT EXISTS idx_jobs_company ON jobs(company_id);
CREATE INDEX IF NOT EXISTS idx_jobs_location ON jobs(location);
CREATE INDEX IF NOT EXISTS idx_jobs_posting_date ON jobs(posting_date);
CREATE INDEX IF NOT EXISTS idx_jobs_title ON jobs(job_title);
CREATE INDEX IF NOT EXISTS idx_scrape_runs_date ON scrape_runs(run_date);
CREATE INDEX IF NOT EXISTS idx_scrape_runs_company ON scrape_runs(company_id);

-- Create view for easy job querying with company name
CREATE VIEW IF NOT EXISTS vw_jobs_full AS
SELECT 
    j.job_id,
    c.company_name,
    j.job_title,
    j.location,
    j.job_responsibilities,
    j.min_education,
    j.min_experience,
    j.preferred_qualifications,
    j.salary_range,
    j.job_identification,
    j.job_category,
    j.degree_level,
    j.ecl_gtc_required,
    j.essential_skills,
    j.posting_date,
    j.job_url,
    j.created_at,
    j.updated_at
FROM jobs j
JOIN companies c ON j.company_id = c.company_id;

-- Create view for scrape run statistics
CREATE VIEW IF NOT EXISTS vw_scrape_stats AS
SELECT 
    sr.run_id,
    c.company_name,
    sr.run_date,
    sr.jobs_found,
    sr.jobs_new,
    sr.jobs_updated,
    sr.status,
    sr.duration_seconds,
    sr.error_message
FROM scrape_runs sr
LEFT JOIN companies c ON sr.company_id = c.company_id
ORDER BY sr.run_date DESC;
