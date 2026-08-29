import os

JOB_DATABASE_ENV = "VIS_JOB_DB"
DEFAULT_JOB_DATABASE = "jobs.sqlite3"


def job_database_path() -> str:
    return os.environ.get(JOB_DATABASE_ENV, DEFAULT_JOB_DATABASE)
