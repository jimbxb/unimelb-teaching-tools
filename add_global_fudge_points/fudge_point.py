#!/usr/bin/env python3
"""
This script is used to add fudge points to all student submissions, allowing for linear scaling of grades.
Note students who received a zero originally do not receive a grade bump, which this script presumes requires a good faith effort.

This script is supplemented by config.ini, for which a sample is provided in which the user can configure the number of fudge points to add or subtract.
"""
import sys
import os
from pathlib import Path

sys.path.insert(0, str(Path(os.path.realpath(__file__)).parent.parent))

from utils import (  # pylint:disable=wrong-import-position
    get_user_info,
    get_users_ids,
    get_quiz_info,
    get_truthy_config_option,
    get_quiz_submission_history,
    submit_quiz_payload,
    logger,
)

MODULE_CONFIG_SECTION = "FUDGEPOINTS"

initial_fudge_points = float(
    get_truthy_config_option("initial_fudge_points", MODULE_CONFIG_SECTION)
)
max_points = float(get_truthy_config_option("max_points", MODULE_CONFIG_SECTION))
respect_cap = bool(get_truthy_config_option("respect_cap", MODULE_CONFIG_SECTION))


def interactive_grader(submission):
    """Grades an individual user's submission"""
    user: str = (
        get_user_info(submission["user_id"])["name"].encode("utf-8").decode("ascii")
    )

    logger.info(f"Updating [bold cyan]{user}[/bold cyan]")

    submission_history = sorted(
        submission["submission_history"], key=lambda x: x["attempt"]
    )
    if "submission_data" not in submission_history[0]:
        return

    most_recent_answers = submission_history[0]["submission_data"]

    total = sum([i["points"] for i in most_recent_answers])
    fudge_points = initial_fudge_points
    if fudge_points > 0 and total in (0, max_points) or total == 0:
        logger.info("No fudging required.")
        return

    if total + fudge_points > max_points and respect_cap:
        fudge_points = max_points - total

    logger.info(f"{total} --> {total+fudge_points}")

    payload = {
        "quiz_submissions": [
            {
                "attempt": submission_history[0]["attempt"],
                "fudge_points": fudge_points,
            }
        ]
    }

    submit_quiz_payload(submission_history[0]["id"], payload)


def main():
    logger.info("Downloading User Data...")
    get_users_ids()
    logger.info("Fetching Quiz Answers...")
    quiz = get_quiz_info()
    quiz_assignment_id = quiz["assignment_id"]
    for submission in get_quiz_submission_history(quiz_assignment_id):
        try:
            interactive_grader(submission)
        except KeyboardInterrupt:
            continue
        except Exception as e:
            logger.error(e)
            continue


if __name__ == "__main__":
    main()
