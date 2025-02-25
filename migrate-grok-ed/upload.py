import shutil

"""
This script is used to interact with the EdStem API to create and manage lessons, slides, and challenges.
It uses various libraries including requests, marshmallow_dataclass, pydantic, and rich for logging and pretty printing.

Classes:
    edAPI: Main class to interact with the EdStem API.
        - EDAPI_OBJ: Base class for API objects that require server-side IDs.
        - Module: Represents a module in the EdStem system.
        - Slide: Represents a slide in the EdStem system.
        - Lesson: Represents a lesson in the EdStem system.
        - Challenge: Represents a challenge in the EdStem system.

Functions:
    rsync(src: str, dest: str, args): A retrying rsync function to synchronize files.
    create_challenge(folder: Path, session: edAPI, lesson: edAPI.Lesson): Creates a challenge from a given folder.
    get_new_or_old_slide(session: edAPI, lesson: edAPI.Lesson, slide_title: str, slide_type: str) -> edAPI.Slide: Retrieves or creates a new slide.
    create_slides_and_challenges(slide_folder: Path, session: edAPI, lesson: edAPI.Lesson): Creates slides and challenges from a given folder.
    create_lesson(lesson_folder: Path, session: edAPI, module: edAPI.Module, existing_lessons): Creates a lesson from a given folder.
    create_module(module_folder: Path, session: edAPI, existing_modules, existing_lessons): Creates a module from a given folder.
    check_and_resolve_collision(session: edAPI, lesson: edAPI.Lesson, slide_title: str): Prepends "Explainer:" to the name of any existing text
        slides in the lesson that match the target slide_title, as this interferes with the creation of lessons.
    create_all_modules(session: edAPI): Creates all modules by iterating through the output/grok_exercises directory.
    delete_all_modules(session: edAPI): Deletes all modules by iterating through the lessons in Ed. Used for cleaning in a testing environment.
    set_lesson_commands(session: edAPI, lesson: edAPI.Lesson, build_command: Optional[str], run_command: Optional[str], terminal_command: Optional[str]):
        Given the lesson - set its build, run and terminal commands.
    set_all_lesson_commands(session: edAPI, check_function: Callable[[edAPI.lesson], bool], build_mapping_function: Callable[[edAPI.lesson], Optional[str]],
        run_mapping_function: Callable[[edAPI.lesson], Optional[str]], terminal_mapping_function: Callable[[edAPI.lesson], Optional[str]]):
        Takes a session, runs each lesson through the check_function, if the check_function returns True, then the 
        mapping function is called on that lesson to set the run command for all slides to that callable.
    main(): Main function to initiate the session and create all modules.
"""
import requests
import json
import rich

# replace print with rich.print
from rich import print
from typing import Iterable, List, Any, Mapping, Optional, Dict, Callable
from typing import ClassVar, Type
from datetime import datetime
import challenge
import lesson
import slide
from pathlib import Path
import yaml
from marshmallow_dataclass import dataclass
from marshmallow import Schema
import marshmallow_dataclass
import yaml2pyclass
from grok_problem import GrokProblem
from grok_test import GrokTest
from pydantic import BaseModel
from pprint import pprint
import subprocess
import sys
from collections import defaultdict
from retrying import retry

## add logging with rich
from rich.logging import RichHandler
import logging
import ipdb
import re
from configparser import ConfigParser


log = logging.getLogger("rich")

config = ConfigParser()
config.read("config/config_comp90059.ini")

FORMAT = "%(message)s"
log_level = config.get("GLOBAL", "log_level", fallback="DEBUG")
logging.basicConfig(
    level=log_level, format=FORMAT, datefmt="[%X]", handlers=[RichHandler()]
)

ED_COURSE_ID = config.get("ED", "ed_course_id")

DRY_RUN = False
DRY_RUN_ALLOW_GETS = True
ALLOW_RSYNC = True


session = requests.Session()
grok_slug = config.get("GROK", "grok_course_slug")
log_dir =  Path("output")/ grok_slug / "logs"
log_dir.mkdir(parents=True, exist_ok=True)
file_handler = logging.FileHandler(filename=log_dir/"upload.log")
file_handler.formatter = logging.Formatter(
    "%(asctime)s %(name)-12s %(levelname)-8s %(message)s", datefmt="%d-%b-%y %H:%M:%S"
)
log.addHandler( file_handler)

def replace_inline_code(text):
    """
    Replace <code data-lang='py3'>...</code> with Markdown backticks.
    """
    pattern = r"<code data-lang='py3'>(.*?)</code>"
    return re.sub(pattern, r"`\1`", text, flags=re.DOTALL | re.IGNORECASE)

class edAPI:

    token: Optional[str]
    base_url: Optional[str]
    class_id: Optional[str]
    url_suffix: Optional[str]

    def new(self, base_url="", token="", class_id=None, url_suffix=""):
        if not base_url:
            raise Exception("No base_url set!")
        if not token:
            raise Exception("No token set!")

        self.token = token
        self.base_url = base_url
        self.class_id = class_id
        self.url_suffix = url_suffix
        return self

    # create a new class for api requests that requires the server side id of the object to be modified
    class EDAPI_OBJ:
        token: Optional[str]
        base_url: Optional[str]
        url_suffix: Optional[str]
        id: Optional[int]
        Schema: ClassVar[Type[Schema]] = Schema  # type: ignore

        def new(self, base_url="", url_suffix="", token="", sid=None, class_id=None):
            self.base_url = base_url
            self.url_suffix = url_suffix
            self.id = sid
            self.token = token
            self.class_id = class_id

        @retry(wait_fixed=2000)
        def api_request(
            self,
            method,
            data=None,
            json_data=None,
            files=None,
            url_suffix="",
            include_sid=True,
        ):

            if DRY_RUN:
                log.info(f"DRY RUN: {method} {self.base_url}/{url_suffix}")
                return {}

            if not hasattr(self, "base_url") or not self.base_url:
                log.error("No base_url set!")
                raise Exception("No base_url set!")

            if not hasattr(self, "token") or not self.token:
                log.error("No token set!")
                raise Exception("No token set!")
            if self.url_suffix and not url_suffix:
                url_suffix = self.url_suffix

            full_url = self.base_url + "/" + url_suffix

            if include_sid:
                if not hasattr(self, "id") or not self.id:
                    log.error("No server-side ID (sid) set for the API request!")
                    raise Exception("No sid set!")
                else:
                    full_url = full_url + "/" + str(self.id)

            auth = {"Authorization": "Bearer " + self.token}

            # if data: # todo add more checks here
            #     data.pop('sid')
            #     data.pop('url_suffix')
            #     data.pop('base_url')
            #     data.pop('token')

            request = None

            if data and json_data:
                raise Exception("Cannot have both data and json_data")

            if data:
                request = requests.Request(
                    method, full_url, data=data, headers=auth
                ).prepare()
            elif json_data:
                request = requests.Request(
                    method, full_url, json=json_data, headers=auth
                ).prepare()
            elif files:
                request = requests.Request(
                    method,
                    full_url,
                    files=files,
                    headers=auth,
                ).prepare()
            else:
                request = requests.Request(method, full_url, headers=auth).prepare()

            # log.debug(request.__dict__)
            if (DRY_RUN_ALLOW_GETS and method == "GET") or not DRY_RUN:
                if request.body:
                    log.debug(request.body)

                response = session.send(request, verify=True)
                try:
                    response.raise_for_status()
                except requests.exceptions.HTTPError as e:
                    if method == "DELETE" and response.status_code == 410:
                        log.info("Object deleted")
                        return response.json()
                    try:
                        # log the response in json and the status code combined with the status code first
                        log.error(f"HTTP {response.status_code}: {response.json()}")

                    except requests.exceptions.JSONDecodeError:
                        pass
                    raise e

                try:
                    return response.json()
                except requests.exceptions.JSONDecodeError as e:
                    if response.status_code == 200:
                        return {}
                    else:
                        raise e

            else:
                return {}

        def get_internal(self) -> Mapping[str, Any]:
            return self.api_request("GET")

        def get(self):
            class_name = self.__class__
            schema = marshmallow_dataclass.class_schema(class_name)()
            data = self.get_internal()
            class_name = self.__class__.__name__

            try:
                data = data[class_name.lower()]
            except KeyError:
                log.error("Warning - no data returned, object not updated")
                return self

            obj: edAPI.EDAPI_OBJ = schema.load(data)  # type: ignore
            obj.base_url = self.base_url
            obj.url_suffix = self.url_suffix
            obj.token = self.token
            obj.class_id = self.class_id
            obj.url_suffix = self.url_suffix

            return obj

        def wrap(self, data):
            class_name = self.__class__.__name__
            return {class_name.lower(): data}

        def unwrap(self, data):
            class_name = self.__class__.__name__
            return data[class_name.lower()]

        def put(self, data=None, json_data=None, files=None):
            if json_data:
                json_data = self.wrap(json_data)
            return self.api_request("PUT", data=data, json_data=json_data, files=files)

        def delete(self):
            return self.api_request("DELETE")

        def patch(self, data=None, json_data=None):
            if json_data:
                json_data = self.wrap(json_data)
            return self.api_request("PATCH", data=data, json_data=json_data)

        def post(self, data=None, json_data=None, include_sid=True, url_suffix=""):
            if json_data:
                json_data = self.wrap(json_data)
            response = self.api_request(
                "POST",
                data=data,
                json_data=json_data,
                include_sid=include_sid,
                url_suffix=url_suffix,
            )
            if response:
                self.id = self.unwrap(response)["id"]

            return response

        def dump(self):
            return marshmallow_dataclass.class_schema(self.__class__)().dump(self)

        def json_str(self):
            return marshmallow_dataclass.class_schema(self.__class__)().dumps(self)

        def json(self):
            return json.loads(self.json_str())

        def __str__(self):
            return str(self.json_str())

        def save(self):
            return self.put(json_data=self.json())

        def create(self, create_suffix=""):
            if not create_suffix:
                create_suffix = self.url_suffix
            response = self.post(
                json_data=self.json(),
                include_sid=False,
                url_suffix=create_suffix,
            )
            class_name = self.__class__
            schema = marshmallow_dataclass.class_schema(class_name)()

            class_name = self.__class__.__name__
            try:
                data = response[class_name.lower()]
                obj = schema.load(data)
                self.__dict__.update(obj.__dict__)
            except KeyError:
                log.error("Warning - no data returned, object not updated")

            return self

    @dataclass
    class Module(EDAPI_OBJ):
        name: Optional[str | None]
        user_id: Optional[int | None]
        created_at: Optional[datetime | None]
        updated_at: Optional[datetime | None]
        course_id: Optional[int | None]
        id: Optional[int | None]

        def new(self, base_url="", token="", sid=None, class_id=None):
            super().new(
                base_url=base_url,
                url_suffix=f"lessons/modules",
                token=token,
                sid=sid,
                class_id=class_id,
            )
            self.name: str | None = None
            self.user_id: int | None = None
            self.created_at: datetime | None = None
            self.updated_at: datetime | None = None
            self.course_id: int | None = None
            return self

        def create(self):
            return super().create(f"courses/{self.class_id}/{self.url_suffix}")

    @dataclass
    class Slide(EDAPI_OBJ):
        original_id: Optional[int | None]
        lesson_id: Optional[int | None]
        user_id: Optional[int | None]
        course_id: Optional[int | None]
        type: Optional[str | None]
        title: Optional[str | None]
        points: Optional[int | None]
        index: Optional[int | None]
        is_hidden: Optional[bool | None]
        status: Optional[str | None]
        correct: Optional[bool | None]
        response: Optional[str | None]
        created_at: Optional[datetime | None]
        updated_at: Optional[datetime | None]
        challenge_id: Optional[int | None]
        content: Optional[str | None]
        id: Optional[int | None]
        index: Optional[int | None]
        type: Optional[str | None]
        status: Optional[str | None]
        active_status: Optional[bool | None]
        is_survey: Optional[bool | None]
        passage: Optional[str | None]
        mode: Optional[str | None]
        auto_points: Optional[bool | None]
        rubric_points: Optional[int | None]
        lesson_markable_id: Optional[int | None]
        rubric_id: Optional[int | None]

        def save(self):
            ffiles = {"slide": (None, json.dumps(self.dump()))}
            return self.api_request(method="PUT", files=ffiles)

        def new(self, base_url="", token="", sid=None, class_id=None):
            super().new(
                base_url=base_url,
                url_suffix="lessons/slides",
                token=token,
                sid=sid,
                class_id=class_id,
            )
            self.original_id: int | None = None
            self.lesson_id: int | None = None
            self.user_id: int | None = None
            self.course_id: int | None = None
            self.type: str | None = None
            self.title: str | None = None
            self.points: int | None = None
            self.index: int | None = None
            self.is_hidden: bool | None = None
            self.status: str | None = None
            self.correct: bool | None = None
            self.response: str | None = None
            self.created_at: datetime | None = None
            self.updated_at: datetime | None = None
            self.challenge_id: int | None = None
            self.content: str | None = None
            self.private = object()
            self.index: int | None = None
            self.type: str | None = None
            self.status: str | None = None
            self.active_status: Optional[bool | None]
            self.is_survey: Optional[bool | None]
            self.passage: Optional[str | None]
            self.mode: Optional[str | None]
            self.auto_points: Optional[bool | None]
            self.rubric_points: Optional[int | None]
            self.lesson_markable_id: Optional[int | None]
            return self

        def create(self):

            ffiles = {"slide": (None, json.dumps({"type": self.type}))}
            response = self.api_request(
                method="POST",
                url_suffix=f"lessons/{self.lesson_id}/slides",
                files=ffiles,
                include_sid=False,
            )

            class_name = self.__class__
            schema = marshmallow_dataclass.class_schema(class_name)()

            class_name = self.__class__.__name__

            try:
                data = response[class_name.lower()]

                obj = schema.load(data)
                self.__dict__.update(obj.__dict__)
            except KeyError:
                log.error("Warning - no data returned, object not updated")

            return self

        def put(self, data=None, json_data=None, files=None):
            return self.api_request("PUT", data=data, json_data=json_data, files=files)

        def get_challenge(self):

            schema = marshmallow_dataclass.class_schema(edAPI.Challenge)()
            challenge = edAPI.Challenge(None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None).new(self.base_url, self.token, self.challenge_id)  # type: ignore
            data = challenge.get_internal()
            class_name = edAPI.Challenge.__name__
            try:
                data = data[class_name.lower()]

                if "tickets" in data and "connect" in data["tickets"]:
                    for ticket in data["tickets"]["connect"]:
                        if "from" in ticket:
                            bak = data["tickets"]["connect"]["from"]
                            del data["tickets"]["connect"]["from"]
                            data["tickets"]["connect"]["from_"] = bak
                            break

                obj: edAPI.Challenge = schema.load(data)  # type: ignore
            except KeyError:
                log.error("Warning - no data returned, object not updated")
                return None
            obj.base_url = self.base_url
            obj.token = self.token
            obj.id = self.challenge_id
            obj.url_suffix = "challenges"

            return obj

    class Lesson(EDAPI_OBJ):
        available_at: Optional[datetime | None] = None
        due_at: Optional[datetime | None] = None
        index: Optional[int | None] = None
        is_hidden: Optional[bool | None] = None
        is_timed: Optional[bool | None] = None
        is_unlisted: Optional[bool | None] = None
        late_submissions: Optional[bool | None] = None
        locked_at: Optional[datetime | None] = None
        module_id: Optional[int | None] = None
        openable: Optional[bool | None] = None
        outline: Optional[str | None] = None
        password: Optional[str | None] = None
        prerequisites: Optional[List[Any] | None] = None
        release_challenge_feedback: Optional[bool | None] = None
        release_challenge_feedback_while_active: Optional[bool | None] = None
        release_challenge_solutions: Optional[bool | None] = None
        release_challenge_solutions_while_active: Optional[bool | None] = None
        release_quiz_correctness_only: Optional[bool | None] = None
        release_quiz_solutions: Optional[bool | None] = None
        reopen_submissions: Optional[bool | None] = None
        settings: Optional[Any | None] = (
            None  # Adjusted for `lesson.Settings` if type is defined elsewhere
        )
        solutions_at: Optional[datetime | None] = None
        state: Optional[str | None] = None
        timer_duration: Optional[int | None] = None
        timer_expiration_access: Optional[bool | None] = None
        title: Optional[str | None] = None
        tutorial_regex: Optional[str | None] = None
        type: Optional[str | None] = None
        updated_at: Optional[datetime | None] = None
        last_viewed_slide_id: Optional[int | None] = None
        original_id: Optional[int | None] = None
        timer_effective_duration: Optional[int | None] = None
        slide_count: Optional[int | None] = None
        created_at: Optional[datetime | None] = None
        status: Optional[str | None] = None
        first_viewed_at: Optional[datetime | None] = None
        user_id: Optional[int | None] = None
        course_id: Optional[int | None] = None
        number: Optional[int | None] = None
        attempted_at: Optional[datetime | None] = None
        id: Optional[int | None] = None
        slides: Optional[List[slide.SlideDataClass] | None] = (
            None  # Adjusted for `slide.SlideDataClass` if type is defined elsewhere
        )

        # Newly added fields
        release_feedback_while_active: Optional[bool | None] = None
        kind: Optional[str | None] = None
        grade_passback_auto_send: Optional[bool | None] = None
        clean_attempts: Optional[bool | None] = None
        require_user_override: Optional[bool | None] = None
        openable_without_attempt: Optional[bool | None] = None
        attempt_id: Optional[int | None] = None
        effective_locked_at: Optional[datetime | None] = None
        submitted_at: Optional[datetime | None] = None
        effective_available_at: Optional[datetime | None] = None
        attempts: Optional[int | None] = None
        effective_due_at: Optional[datetime | None] = None
        release_feedback: Optional[bool | None] = None
        grade_passback_mode: Optional[str | None] = None
        attempts_remaining: Optional[int | None] = None
        grade_passback_scale_to: Optional[int | None] = None
        password_one_time: Optional[bool | None] = None
        slide_marks_summary: Optional[List[slide.SlideSummaryDataClass] | None] = None
        is_grade_passback_available: Optional[bool | None] = None

        def create(self):
            return super().create(f"courses/{self.class_id}/{self.url_suffix}")

        def new(self, base_url="", token="", sid=None, url_suffix=None, class_id=None):
            super().new(
                base_url=base_url,
                url_suffix="lessons",
                token=token,
                sid=sid,
                class_id=class_id,
            )
            if url_suffix:
                self.url_suffix = url_suffix
            self.available_at: datetime | None = None
            self.due_at: datetime | None = None
            self.index: int | None = None
            self.is_hidden: bool | None = None
            self.is_timed: bool | None = None
            self.is_unlisted: bool | None = None
            self.late_submissions: bool | None = None
            self.locked_at: datetime | None = None
            self.module_id: int | None = None
            self.openable: bool | None = None
            self.outline: str | None = None
            self.password: str | None = None
            self.prerequisites: List[Any] | None = None
            self.release_challenge_feedback: bool | None = None
            self.release_challenge_feedback_while_active: bool | None = None
            self.release_challenge_solutions: bool | None = None
            self.release_challenge_solutions_while_active: bool | None = None
            self.release_quiz_correctness_only: bool | None = None
            self.release_quiz_solutions: bool | None = None
            self.reopen_submissions: bool | None = None
            self.settings: Any | None = None
            self.solutions_at: datetime | None = None
            self.state: str | None = None
            self.timer_duration: int | None = None
            self.timer_expiration_access: bool | None = None
            self.title: str | None = None
            self.tutorial_regex: str | None = None
            self.type: str | None = None
            self.updated_at: Optional[datetime | None] = None
            self.last_viewed_slide_id: Optional[int | None] = None
            self.original_id: Optional[int | None] = None
            self.timer_effective_duration: Optional[int | None] = None
            self.slide_count: Optional[int | None] = None
            self.created_at: Optional[datetime | None] = None
            self.status: Optional[str | None] = None
            self.first_viewed_at: Optional[datetime | None] = None
            self.user_id: Optional[int | None] = None
            self.course_id: Optional[int | None] = None
            self.number: Optional[int | None] = None
            self.attempted_at: Optional[datetime | None] = None
            self.id: Optional[int | None] = sid
            self.slides: Optional[List[slide.SlideDataClass] | None] = None
            # Newly added fields
            self.release_feedback_while_active: Optional[bool | None] = None
            self.kind: Optional[str | None] = None
            self.grade_passback_auto_send: Optional[bool | None] = None
            self.clean_attempts: Optional[bool | None] = None
            self.require_user_override: Optional[bool | None] = None
            self.openable_without_attempt: Optional[bool | None] = None
            self.attempt_id: Optional[int | None] = None
            self.effective_locked_at: Optional[datetime | None] = None
            self.submitted_at: Optional[datetime | None] = None
            self.effective_available_at: Optional[datetime | None] = None
            self.attempts: Optional[int | None] = None
            self.effective_due_at: Optional[datetime | None] = None
            self.release_feedback: Optional[bool | None] = None
            self.grade_passback_mode: Optional[str | None] = None
            self.attempts_remaining: Optional[int | None] = None
            self.grade_passback_scale_to: Optional[int | None] = None
            self.password_one_time: Optional[bool | None] = None
            self.slide_marks_summary: Optional[str | None] = None

            return self

        def upload_challenge_files(self, req_body):
            """
            Upload given files to respective places:
            Structure:
            {
                "title": String,
                "content": String,
                "scaffold": Dictionary,
                "solution": Dictionary,
                "test_files": Dictionary,
                "scaffold_binary": Dictionary,
                "solution_binary": Dictionary,
                "test_files_binary": Dictionary,
            }
            Where the title and content are as normal in slides,
            and each Dictionary is of the form:
            {
                String: String
            }
            Where the first string is the target filename 
            and the second string is the data, either text data
            or, for keys in the main structure that include the "_binary"
            suffix, base64 encoded data.
            """
            # https://edstem.org/api/lessons/{lesson_id}/slides/challenge
            lesson_id = self.id
            target_url = f"lessons/{lesson_id}/slides/challenge"
            keyview = {
                "test_files": list(req_body["test_files"].keys()),
                "solution": list(req_body["solution"].keys()),
                "scaffold": list(req_body["scaffold"].keys()),
            }
            log.info(f"Sending files {json.dumps(keyview, indent=4)} to {target_url}")
            self.api_request("POST", url_suffix=target_url, json_data=req_body, include_sid=False)

        def get_all(self):
            response = self.api_request(
                "GET", url_suffix=f"courses/{self.class_id}/lessons", include_sid=False
            )
            new_lessons = []
            new_modules = []
            lesson_schema = marshmallow_dataclass.class_schema(edAPI.Lesson)()
            if not response and DRY_RUN:
                return new_lessons, new_modules

            for lesson in response["lessons"]:

                new_lessons.append(lesson_schema.load(lesson))

            module_schema = marshmallow_dataclass.class_schema(edAPI.Module)()
            for module in response["modules"]:
                new_modules.append(module_schema.load(module))

            return new_lessons, new_modules

    class Challenge(EDAPI_OBJ):
        check_hash: Optional[str | None]
        content: Optional[str | None]
        course_id: Optional[int | None]
        created_at: Optional[datetime | None]
        category: Optional[str | None]
        difficulty: Optional[str | None]
        due_at: Optional[datetime | None]
        exam_visibility: Optional[str | None]
        exam_visibility_alternate_hour: Optional[bool | None]
        explanation: Optional[str | None]
        features: Optional[Dict[str, bool] | None]
        id: Optional[int | None]
        is_active: Optional[bool | None]
        is_exam: Optional[bool | None]
        is_feedback_visible: Optional[bool | None]
        is_hidden: Optional[bool | None]
        kind: Optional[str | None]
        lab_category: Optional[str | None]
        lab_name_regex: Optional[str | None]
        lab_reset: Optional[bool | None]
        language: Optional[str | None]
        manual_solution_control: Optional[bool | None]
        outline: Optional[str | None]
        password: Optional[str | None]
        points: Optional[int | None]
        requires_lti_link: Optional[bool | None]
        scaffold_hash: Optional[str | None]
        score: Optional[int | None]
        sequential_completion: Optional[bool | None]
        session: Optional[str | None]
        settings: Optional[Any | None]
        solution_hash: Optional[str | None]
        status: Optional[str | None]
        testbase_hash: Optional[str | None]
        tickets: Optional[Any | None]
        tutorial_regex: Optional[str | None]
        type: Optional[str | None]
        unavailable_after_completion: Optional[bool | None]
        updated_at: Optional[datetime | None]
        title: Optional[str | None]
        number: Optional[int | None]
        original_id: Optional[int | None]

        attempts_remaining: Optional[int | None]
        rubric_points: Optional[int | None]
        auto_points: Optional[bool | None]
        earliest_submission_time: Optional[datetime | None]
        lesson_markable_id: Optional[int | None]
        extra_attempts: Optional[int | None]
        attempts_within_last_interval: Optional[int | None]
        rubric_id: Optional[int | None]

        def json_str(self):
            tmp = super().dump()
            bak = None
            if "tickets" in tmp and tmp["tickets"]:
                # check if tickets has attr connect
                if hasattr(tmp["tickets"], "connect"):
                    if tmp["tickets"].connect and "from" in tmp["tickets"].connect:
                        bak = tmp["tickets"]["connect"]["from"]
                        tmp["tickets"]["connect"].pop("from")
                        tmp["tickets"]["connect"]["from_"] = bak
            tmp_dict = {
                key: value.to_dict() if hasattr(value, "to_dict") else value
                for key, value in tmp.items()
            }
            return json.dumps(tmp_dict)

        def get(self):
            class_name = self.__class__
            schema = marshmallow_dataclass.class_schema(class_name)()
            data = self.get_internal()
            class_name = self.__class__.__name__
            data = data[class_name.lower()]

            if "tickets" in data and "connect" in data["tickets"]:
                for ticket in data["tickets"]["connect"]:
                    if "from" in ticket:
                        bak = data["tickets"]["connect"]["from"]
                        del data["tickets"]["connect"]["from"]
                        data["tickets"]["connect"]["from_"] = bak
                        break

            obj = schema.load(data)
            self.__dict__.update(obj.__dict__)
            return self

        def new(self, base_url="", token="", sid=None, class_id=None):
            super().new(
                base_url=base_url,
                url_suffix="challenges",
                token=token,
                sid=sid,
                class_id=class_id,
            )
            self.check_hash: str | None = None
            self.content: str | None = None
            self.course_id: int | None = None
            self.created_at: datetime | None = None
            self.difficulty: str | None = None
            self.due_at: datetime | None = None
            self.exam_visibility: str | None = None
            self.exam_visibility_alternate_hour: bool | None = None
            self.explanation: str | None = None
            self.features: Dict[str, bool] | None = None
            self.id: int | None = sid
            self.is_active: bool | None = None
            self.is_exam: bool | None = None
            self.is_feedback_visible: bool | None = None
            self.is_hidden: bool | None = None
            self.kind: str | None = None
            self.lab_category: str | None = None
            self.lab_name_regex: str | None = None
            self.lab_reset: bool | None = None
            self.language: str | None = None
            self.manual_solution_control: bool | None = None
            self.original_id: int | None = None
            self.outline: str | None = None
            self.password: str | None = None
            self.requires_lti_link: bool | None = None
            self.scaffold_hash: str | None = None
            self.score: int | None = None
            self.sequential_completion: bool | None = None
            self.session: str | None = None
            self.settings: challenge.Settings | None = None
            self.solution_hash: str | None = None
            self.status: str | None = None
            self.testbase_hash: str | None = None
            self.tickets: challenge.Tickets | None = None
            self.tutorial_regex: str | None = None
            self.type: str | None = None
            self.unavailable_after_completion: bool | None = None
            self.updated_at: datetime | None = None
            self.title: str | None = None

            self.attempts_remaining: Optional[int | None] = None
            self.rubric_points: Optional[int | None] = None
            self.auto_points: Optional[bool | None] = None
            self.earliest_submission_time: Optional[datetime | None] = None
            self.lesson_markable_id: Optional[int | None] = None
            self.extra_attempts: Optional[int | None] = None
            self.attempts_within_last_interval: Optional[int | None] = None
            self.rubric_id: Optional[int | None] = None

            return self

        def save(self):
            base_suffix = self.url_suffix + "/" + str(self.id) + "/connect/"
            urls = [
                base_suffix + a for a in ["scaffold", "solution", "testbase", "check"]
            ]
            for url in urls:
                self.api_request("POST", url_suffix=url, include_sid=False)

            base_suffix = self.url_suffix + "/" + str(self.id) + "/update/"
            urls = [
                base_suffix + a for a in ["scaffold", "solution", "testbase", "check"]
            ]
            for url in urls:
                self.api_request("POST", url_suffix=url, include_sid=False)

            self.api_request(
                "POST",
                url_suffix=self.url_suffix + f"/{self.id}/connect/testbase",
                json_data=json.dumps({"user_id": None, "password": None, "i": None}),
                include_sid=False,
            )
            self.patch(json_data=self.json_str())

        def patch(self, json_data):
            data = json.loads(self.json_str())
            data.pop("check_hash")
            data.pop("scaffold_hash")
            data.pop("solution_hash")
            data.pop("testbase_hash")
            data.pop("created_at")
            data.pop("updated_at")
            data.pop("score")
            data.pop("status")

            return super().patch(json_data=data)

    def slide(self, sid=None):
        return self.Slide(
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
        ).new(base_url=self.base_url, token=self.token, sid=sid, class_id=self.class_id)

    def lesson(self, sid=None):
        return self.Lesson().new(
            base_url=self.base_url, token=self.token, sid=sid, class_id=self.class_id
        )

    def module(self, sid=None):
        return self.Module(None, None, None, None, None, None).new(
            base_url=self.base_url, token=self.token, sid=sid, class_id=self.class_id
        )

    def challenge(self, sid=None):
        return self.Challenge().new(
            base_url=self.base_url, token=self.token, sid=sid, class_id=self.class_id
        )


# a retrying rsync function
@retry(wait_fixed=2000)
def rsync(src: str, dest: str, args):
    subprocess.run(
        [
            "rsync",
            *args,
            str(src),
            dest,
        ],
        timeout=20,
        check=True,
    )


def create_challenge(folder: Path, session: edAPI, lesson: edAPI.Lesson, problemPath: Path):
    # set content to the contents of content.amber
    content = (problemPath.with_suffix('.xml')).read_text()
    # set solution_text to the contents of solution_notes.amber
    if (folder / "solution_notes.xml").exists():
        solution_text = (folder / "solution_notes.xml").read_text()
    else:
        solution_text = None

    # read in problem.yaml
    # problem = None
    # problem_yaml_path = folder / "problem.yaml"
    with open(problemPath) as f:
        grok_problem = json.load(f)

    grok_ed_map = (
        ("workspace", "scaffold"),
        ("solutions", "solution"),
        #("tests", "check"),
        #("tests", "testbase"),
        ("tests", "test_files"),
    )

    req_body = {
        "title": grok_problem['title'],
        "description_md": content,
        "scaffold": {},
        "solution": {},
        "test_files": {},
    }

    makefile = folder / "workspace" / "Makefile"
    for grok_folder, ed_folder in grok_ed_map:
        # log.info(f"Uploading {grok_folder} to {ed_folder}")
        for wfile in (folder / grok_folder).glob("**/*"):
            if not wfile.name.endswith(".yaml"):
                if wfile.is_dir():
                    log.info(
                        #f"Skipping {wfile} as part of module {mychallenge.id} and lesson {lesson.id}"
                        f"Skipping {wfile} as part of module"
                    )
                    continue
                log.info(
                    #f"Uploading {wfile} as part of module {mychallenge.id} and lesson {lesson.id}"
                    f"Uploading {wfile} as part of module"
                )
                upload_file_path = str(wfile.relative_to(folder / grok_folder))
                keyname = ed_folder
                try:
                    file_content = wfile.read_text()
                except:
                    keyname = ed_folder + "_binary"
                    if keyname not in req_body.keys():
                        req_body[keyname] = {}
                    bfile = wfile.read_bytes()
                    import base64
                    file_content = base64.b64encode(wfile.read_bytes()).decode()
                req_body[keyname][upload_file_path] = file_content
        if makefile.exists():
            req_body[ed_folder]["Makefile"] = makefile.read_text()
        elif lesson.type == "c":
            log.warning(f"Makefile does not exist at {makefile}")

    # Ensure upload POST goes smoothly
    check_and_resolve_collision(session, lesson, req_body["title"])

    # Upload files.
    lesson.upload_challenge_files(req_body)

    slide = get_new_or_old_slide(session, lesson, grok_problem["title"], "code")  # type: ignore
    slide.content = content
    slide.save()

    log.info(f"Challenge (Path: {folder}, Lesson {lesson})")

    if grok_problem["language"] == 20:
        log.warning(
            f"Skipping slide {grok_problem["title"]} because it is a multiple choice quiz"
        )
        return

    mychallenge = slide.get_challenge()
    mychallenge.settings = challenge.Settings(
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
    )
    mychallenge.tickets = challenge.Tickets(
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
    )

    # set the content to the contents of content.amber
    mychallenge.content = content
    if solution_text:
        mychallenge.explanation = solution_text
    if lesson.type == "c":
        mychallenge.settings.build_command = "make all"  # type: ignore
        mychallenge.settings.run_command = "make run"  # type: ignore
        mychallenge.settings.check_command = "make run"  # type: ignore
    elif lesson.type == "python":
        mychallenge.settings.run_command = "python3 program.py"
        mychallenge.settings.check_command = "python3 program.py"

    if not mychallenge.tickets:
        mychallenge.tickets = challenge.Tickets(
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
        )

    if not mychallenge.tickets.mark_standard:
        mychallenge.tickets.mark_standard = challenge.Mark(
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
        )

    if not mychallenge.tickets.mark_standard.run_limit:
        mychallenge.tickets.mark_standard.run_limit = challenge.MarkCustomRunLimit(
            None, None
        )
    if not mychallenge.tickets.run_standard:
        mychallenge.tickets.run_standard = challenge.Run(
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
        )

    # mychallenge.tickets.run_standard.build_command = "make all"  # type: ignore
    mychallenge.tickets.run_standard.run_command = "python program.py"  # type: ignore

    mychallenge.tickets.mark_standard.easy = False  # penalize whitespace infractions
    mychallenge.tickets.mark_standard.run_limit.pty = False  # make the output match up with the terminal output without having to interleave
    mychallenge.type = "code"

    testcases: List[challenge.Testcase] = []

    for f in (folder / "tests").iterdir():
        log.info(f)
        grok_test = json.loads(grok_problem['tests'])
        grok_test = grok_test['tests'][int(f.stem)]
        relative_dir = f.relative_to(folder / "tests")
        testcase = challenge.Testcase(
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
        )
        
        testcase.name = grok_test["label"]

        # if it's c
        if lesson.type == "c":
            testcase.run_command = "./program"
        elif lesson.type == "python":
            if (folder / "tests" / relative_dir / "test.py").exists():
                testcase.run_command = f"python3 {str(folder / "tests" / relative_dir / "test.py")}"
            else:
                testcase.run_command = "python3 program.py"

        if (folder / "tests" / relative_dir / "stdin.txt").exists():
            testcase.stdin_path = str(relative_dir / "stdin.txt")
        else:
            log.warning(f"stdin does not exist for {grok_test["label"]}")
            testcase.stdin_path = str("")

        check = challenge.Check(
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
        )
        # if stdout exists use it, if stdio exists use that instead
        if (folder / "tests" / relative_dir / "stdout.txt").exists():
            check.expect_path = str(relative_dir / "stdout.txt")
        elif (folder / "tests" / relative_dir / "stdio").exists():
            # Not handled in markdown step
            check.expect_path = str(relative_dir / "stdio")
        else:
            log.warn(f"Neither stdout or stdio exist for {grok_test["label"]}")
            check.expect_path = str("")
            # raise Exception(f"Neither stdout or stdio exist for {grok_test.label}")

        check.type = "check_diff"
        check.source = challenge.Source("source_mixed", "")
        testcase.checks = [check]
        testcases.append(testcase)

    mychallenge.tickets.mark_standard.testcases = testcases
    mychallenge.save()
    slide.save()


def check_and_resolve_collision(
    session: edAPI, lesson: edAPI.Lesson, slide_title: str
):
    slide: edAPI.Slide = session.slide()
    new_slide = True
    lesson = session.lesson(lesson.id).get()  # type: ignore
    if lesson.slides:
        for t in lesson.slides:
            if t.title == slide_title:
                log.warning(f"Slide {slide_title} already exists, renaming")
                slide = session.slide(t.id).get()  # type: ignore
                new_slide = False
                if slide.type == "document":
                    slide.title = "Explainer:" + slide.title
                slide.save()
                

def get_new_or_old_slide(
    session: edAPI, lesson: edAPI.Lesson, slide_title: str, slide_type: str
) -> edAPI.Slide:
    slide: edAPI.Slide = session.slide()
    new_slide = True
    lesson = session.lesson(lesson.id).get()  # type: ignore
    if lesson.slides:
        for t in lesson.slides:
            if t.title == slide_title:
                log.warning(f"Slide {slide_title} already exists")
                slide = session.slide(t.id).get()  # type: ignore
                new_slide = False
                break
    slide.type = slide_type
    slide.lesson_id = lesson.id
    if new_slide:
        log.info(f"Creating new slide {slide_title}")
        slide.create()
    slide.title = slide_title
    slide.save()
    return slide


def create_slides_and_challenges(
    slide_folder: Path, session: edAPI, lesson: edAPI.Lesson
):

    # TODO: check if the slide already exists
    log.info(f"Creating slides and challenges for {slide_folder}")

    i = 0
    while True:
        ref_path = Path(slide_folder / f"{i}.ref")
        xml_path = Path(slide_folder / f"{i}.xml")
        json_path = Path(slide_folder / f"{i}.json")
        if ref_path.exists():
            # create challenge
            ref = ref_path.read_text()
            challenge_path = Path(f"output/{grok_slug}/grok_exercises") / ref
            if not challenge_path.exists():
                log.error(f"Challenge Path {challenge_path} does not exist")
                sys.exit(1)

            create_challenge(challenge_path, session, lesson)
        elif xml_path.exists():
            # create slide
            metadata = json.loads(xml_path.with_suffix(".json").read_text())
            slide = get_new_or_old_slide(session, lesson, replace_inline_code(metadata["title"]), "document")
            slide.content = xml_path.read_text()
            slide.save()
        elif json_path.exists():
            log.warning(f"JSON exists but not xml or ref in {i}")
            metadata = json.loads(json_path.read_text())

            # Get reference
            if "problem_id" in metadata.keys():
                ref_id = metadata["problem_id"]
                ref_folder = Path(f"output/{grok_slug}/grok_exercises")
                ref_file_options = list(ref_folder.glob(f"./*/{ref_id}.json"))
                if not len(ref_file_options) == 1:
                    log.error("SKIP: Expected one file matching excercise {} but found {}. Files are {}. Slide folder is {}".format(ref_id, len(ref_file_options), ref_file_options, slide_folder))
                    # print("glob is", f"./*/{ref_id}.json", "ref folder is", ref_folder, "ref_id is", ref_id)
                    i += 1
                    continue
                selected_ref_item: Path = ref_file_options[0]
                # Create challenge for reference.
                single_challenge_path = selected_ref_item.parent / str(ref_id)
                create_challenge(single_challenge_path, session, lesson, selected_ref_item)

        else:
            break
        i += 1


def create_lesson(
    lesson_folder: Path,
    session: edAPI,
    module: edAPI.Module,
    existing_lessons,
):
    # check if a lesson of that name already exists under that module
    lesson: edAPI.Lesson = session.lesson()
    still_not_seen = True
    if lesson_folder.name in existing_lessons:
        # check all the lessons with that name to see if they are under this module
        for l in existing_lessons[lesson_folder.name]:
            l = session.lesson(l).get()

            if l.module_id == module.id:
                lesson = l
                log.warning(
                    f"Lesson {lesson_folder.name} already exists under {module.name}"
                )
                still_not_seen = False

    if still_not_seen == True:
        lesson_name_details = lesson_folder.name.split("_")
        lesson.create()
        lesson.index = lesson_name_details[0]
        lesson.title = replace_inline_code("_".join(lesson_name_details[1:]))
        lesson.type = config.get("ED", "lesson_type", fallback="python")
        lesson.icon = config.get("ED", "lesson_type", fallback="General")
        lesson.module_id = module.id
        lesson.save()

    # create slides and challenges
    success = create_slides_and_challenges(lesson_folder, session, lesson)
    return success


def create_module(
    module_folder: Path, session: edAPI, existing_modules, existing_lessons
):
    # create a new lesson
    module = session.module()

    module_json_file = list(module_folder.glob("*.json"))

    if len(module_json_file) == 0:
        log.info(f"Found no json files in {module_folder}")
        return False

    if len(module_json_file) != 1:
        log.error(f"Found {len(module_json_file)} json files in {module_folder}")
        sys.exit(1)
    module_json_file = module_json_file[0]
    module_json = json.loads(module_json_file.read_text())

    module.name = replace_inline_code(module_json["title"])

    if (
        "exam" in module.name.lower()
        or "project" in module.name.lower()
        or "test" in module.name.lower()
    ):
        log.info(f"Skipping {module.name} entirely")
        return False
    if module.name in existing_modules:
        log.info(f"Module {module.name} already exists, skipping")
        module = existing_modules[module.name][0]
    else:
        log.info(f"Creating module {module.name}")
        module.create()

    # for each lesson folder in the module folder
    for lesson_folder in module_folder.iterdir():
        if not lesson_folder.is_dir():
            continue

        log.info(f"Creating lesson {lesson_folder.name}")

        # if "Chapter 10: Dynamic Structures (FOA only)" in module.name:
        #     log.info(f"Skipping {module.name} entirely")
        #     continue

        success = create_lesson(
            lesson_folder, session, module, existing_lessons=existing_lessons
        )
        if success:
            log.warning(
                "Short circuiting for testing. Remove the break statement to run all lessons"
            )
            continue

    return True


def create_all_modules(session: edAPI):
    lessons, modules = session.lesson().get_all()
    existing_modules = defaultdict(list)
    existing_lessons = defaultdict(list)
    for module in modules:
        existing_modules[module.name].append(module)
    for lesson in lessons:
        existing_lessons[lesson.title].append(lesson)

    import ipdb

    for lesson in lessons:
        if lesson.title == "Untitled Lesson":
            log.info(f"Deleting {lesson.title}")
            session.lesson(lesson.id).delete()

    # for every folder in output/grok_exercises
    for module_folder in Path(f"output/{grok_slug}/modules").iterdir():
        if not module_folder.is_dir:
            continue

        success = create_module(
            module_folder, session, existing_modules, existing_lessons
        )
        if success:
            log.info(
                "Short circuiting for testing. Remove the break statement to run all modules"
            )
            continue  # short circuit it for testing


def delete_all_modules(session: edAPI):
    lessons, modules = session.lesson().get_all()
    existing_modules = defaultdict(list)
    for lesson in lessons:
        log.info(f"Deleting {lesson.title}")
        session.lesson(lesson.id).delete()


def set_lesson_commands(session: edAPI, lesson: edAPI.Lesson, build_command: Optional[str], run_command: Optional[str], terminal_command: Optional[str]):
    """
    Given the lesson - set its build, run and terminal commands.
    """
    lesson = session.lesson(lesson.id).get()
    if not lesson.slides:
        # No slides, nothing to do.
        log.warning(f"No slides in lesson {lesson.title}")
        return
    for slide in lesson.slides:
        if slide.type == "code":
            current_slide = session.slide(slide.id).get()
            challenge = current_slide.get_challenge()

            if build_command:
                challenge.settings["build_command"] = build_command
                challenge.tickets["run_standard"]["build_command"] = build_command
                log.info(f"Updating {slide.title}'s build command to {build_command}")
                challenge.save()
            if run_command:
                challenge.settings["run_command"] = run_command
                challenge.settings["check_command"] = run_command
                challenge.tickets["run_standard"]["run_command"] = run_command
                log.info(f"Updating {slide.title}'s run and check command to {run_command}")
            if terminal_command:
                challenge.settings["terminal_command"] = terminal_command
                log.info(f"Updating {slide.title}'s terminal command to {terminal_command}")
            challenge.save()
            current_slide.save()
    lesson.save()


def set_all_lesson_commands(session: edAPI, check_function: Callable[[edAPI.lesson], bool], 
    build_mapping_function: Callable[[edAPI.lesson], Optional[str]],
    run_mapping_function: Callable[[edAPI.lesson], Optional[str]],
    terminal_mapping_function: Callable[[edAPI.lesson], Optional[str]]
    ):
    """
    Takes a session, runs each lesson through the check_function, if the check_function returns True, then the 
    mapping function is called on that lesson to set the run command for all slides to that callable.
    """
    lessons, modules = session.lesson().get_all()
    existing_modules = defaultdict(list)
    for lesson in lessons:
        if check_function(lesson):
            log.info(f"Applying update to run commands for {lesson.title}")
            set_lesson_commands(session, lesson, build_mapping_function(lesson), run_mapping_function(lesson), terminal_mapping_function(lesson))


def main():

    # Example checks & update steps.
    def is_haskell(lesson: edAPI.Lesson) -> bool:
        HASKELL_WEEKS = [5, 6, 7, 8, 9, 10, 11]
        HASKELL_WEEK_SET = ["Week {}".format(i) for i in HASKELL_WEEKS]
        HASKELL_WORKSHOP_SET = ["Workshop {}".format(i) for i in HASKELL_WEEKS]
        HASKELL_PHRASES = HASKELL_WEEK_SET + HASKELL_WORKSHOP_SET
        for phrase in HASKELL_PHRASES:
            if phrase in lesson.title:
                return True
        return False

    def haskell_build(lesson: edAPI.Lesson) -> Optional[str]:
        return "ghc Main.hs"

    def haskell_run(lesson: edAPI.Lesson) -> Optional[str]:
        return "./Main"

    def haskell_terminal(lesson: edAPI.Lesson) -> Optional[str]:
        return "ghci"

    def is_prolog(lesson: edAPI.Lesson) -> bool:
        PROLOG_WEEKS = [1, 2, 3, 4]
        PROLOG_WEEK_SET = ["Week {}".format(i) for i in PROLOG_WEEKS]
        PROLOG_WORKSHOP_SET = ["Workshop {}".format(i) for i in PROLOG_WEEKS]
        PROLOG_PHRASES = PROLOG_WEEK_SET + PROLOG_WORKSHOP_SET
        for phrase in PROLOG_PHRASES:
            if phrase in lesson.title:
                return True
        return False

    def prolog_build(lesson: edAPI.Lesson) -> Optional[str]:
        """
        Keep using any existing build command.
        """
        return None

    def prolog_run(lesson: edAPI.Lesson) -> Optional[str]:
        return "swipl -l main.pl"

    def prolog_terminal(lesson: edAPI.Lesson) -> Optional[str]:
        return "swipl -l main.pl"

    session = edAPI().new(
        f"https://edstem.org/api", config.get("ED","ed_token"), class_id=config.get("ED", "ed_course_id")
    )

    # For cleanup after testing
    # delete_all_modules(session)
    # Create all lessons.
    create_all_modules(session)
    # lesson: edAPI.lesson = session.lesson(31193).get()
    # create_challenge(Path("output/grok_exercises/Ex10.x1"), session, lesson)

    # After new modules are complete, run haskell & prolog checks.
    # set_all_lesson_commands(session, is_haskell, haskell_build, haskell_run, haskell_terminal)
    # set_all_lesson_commands(session, is_prolog, prolog_build, prolog_run, prolog_terminal)


main()
