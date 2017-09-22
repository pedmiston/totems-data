from os import environ
import sys
import json

from invoke import task
import pandas
import gspread
from unipath import Path
from oauth2client.service_account import ServiceAccountCredentials
from sqlalchemy import update

import graph
import db
from db import Group, Player
from tasks import paths

TOTEMS_DIR = Path(paths.R_PKG, 'data-raw/totems')
if not TOTEMS_DIR.isdir():
    TOTEMS_DIR.mkdir()

try:
    landscape = graph.Landscape()
except graph.main.LandscapeNotInitialized:
    print('WARNING! Landscape not initialized. Run "inv graph.load"')


@task
def download(ctx, name=None, clear_before=False, analyze_after=False):
    """Download the experiment data from the totems database."""
    available = ['tables', 'subj_info', 'survey']

    if name is None:
        names = available
    else:
        assert name in available, 'name "%s" not in %s' % (name, available)
        names = [name]

    if clear_before:
        TOTEMS_DIR.rmtree()
        TOTEMS_DIR.mkdir()

    if 'tables' in names:
        tables()
    if 'subj_info' in names:
        subj_info()
    if 'survey' in names:
        survey()

    if analyze_after:
        analyze(ctx)


def tables():
    con = db.connect_to_db()
    for table in con.table_names():
        frame = pandas.read_sql('SELECT * FROM %s' % table, con)
        out_csv = Path(TOTEMS_DIR, '{}.csv'.format(table.split('_')[1]))
        frame.to_csv(out_csv, index=False)


def subj_info(sanitize=True, save_as=True):
    """Download the subject info sheet from Google Drive."""
    df = get_worksheet('totems-subj-info')
    df.rename(columns=dict(SubjID='ID_Player',
                           Initials='Experimenter'),
              inplace=True)
    cols = 'ID_Player Strategy Date Room Experimenter Compliance'.split()
    # Sanitize!
    if sanitize:
        for col in cols:
            try:
                df[col] = df[col].str.replace('\n', '')
            except AttributeError:
                pass

    if save_as:
        df[cols].to_csv(Path(TOTEMS_DIR, 'SubjInfo.csv'), index=False)

    return df[cols]


def survey():
    """Download the survey responses from Google Drive."""
    df = get_worksheet('totems-survey-responses')
    df.to_csv(Path(TOTEMS_DIR, 'Survey.csv'), index=False)


def get_worksheet(title):
    creds_dict = db.get_from_vault(
        vault_file='secrets/lupyanlab-service-account.json'
    )
    credentials = ServiceAccountCredentials.from_json_keyfile_dict(
        creds_dict, scopes='https://spreadsheets.google.com/feeds')

    gc = gspread.authorize(credentials)

    try:
        ws = gc.open(title).sheet1
    except gspread.SpreadsheetNotFound:
        print('spreadsheet %s not found, is it shared with the creds email?' % title)

    return pandas.DataFrame(ws.get_all_records())


@task
def analyze(ctx):
    """Analyze the totems experiment data."""
    workshop = pandas.read_csv(Path(TOTEMS_DIR, 'Workshop.csv'))
    workshop = label_teams_and_strategies(workshop)
    workshop = filter_valid_teams(workshop)
    workshop = calculate_team_time(workshop)

    workshop = workshop.sort_values(['ID_Group', 'TeamTime']).reset_index(drop=True)

    workshop = (workshop.groupby('ID_Player')
                        .apply(rolling_history)
                        .reset_index(drop=True))
    workshop = (workshop.groupby('ID_Group')
                        .apply(rolling_history, prefix='Team')
                        .reset_index(drop=True))

    # Get scores for all WorkShopResults, then only get
    # scores for the first time a player makes the item,
    # and then only for the first time a team makes the item.
    scores = workshop.WorkShopResult.apply(landscape.get_score)
    workshop['Score']     = scores.where(workshop.UniqueItem, 0)
    workshop['TeamScore'] = scores.where(workshop.TeamUniqueItem, 0)

    # Calculate number of adjacent items
    workshop = determine_adjacent_possible(workshop)

    workshop_cols = [
        'ID_Player',
        'PlayerTime', 'TeamTime',
        'WorkShopString', 'UniqueGuess', 'TeamUniqueGuess',
        'WorkShopResult', 'UniqueItem', 'TeamUniqueItem',
        'Score', 'TeamScore',
        'Inventory', 'TeamInventory',
        'NumAdjacent'
    ]
    workshop[workshop_cols].to_csv(Path(TOTEMS_DIR, 'WorkshopAnalyzed.csv'),
                                   index=False)


def rolling_history(trials, prefix=''):
    """Keep track of rolling variables, like total known inventory."""
    trials = trials.sort_values('TeamTime').reset_index(drop=True)
    rolling_guesses = set()
    rolling_inventory = landscape.starting_inventory()  # also a set

    inventories = []
    unique_item = []
    unique_guess = []

    for trial in trials.itertuples():
        # Record the guess
        is_unique_guess = trial.WorkShopString not in rolling_guesses
        if is_unique_guess:
            rolling_guesses.add(trial.WorkShopString)
        unique_guess.append(int(is_unique_guess))

        # Record the result of the guess
        is_guess_successful = trial.WorkShopResult != 0
        is_unique_item = 0  # default
        if is_guess_successful:
            item = int(trial.WorkShopResult)
            is_unique_item = item not in rolling_inventory
            if is_unique_item:
                rolling_inventory.add(item)
        unique_item.append(int(is_unique_item))

        # Store the current rolling inventory
        inventories.append(freeze_inventory(rolling_inventory))

    trials[prefix+'Inventory'] = inventories
    trials[prefix+'UniqueItem'] = unique_item
    trials[prefix+'UniqueGuess'] = unique_guess
    return trials


def determine_adjacent_possible(trials):
    """Calculate the number of adjacent possibilities for each player."""
    trials = trials.copy()
    inventories = trials.TeamInventory.apply(
        lambda x: list(map(int, x.split('-')))
    )
    trials['NumAdjacent'] = (inventories.apply(landscape.adjacent_possible)
                                        .apply(len))
    return trials


def label_teams_and_strategies(frame):
    # TODO: Make this work even when db is offline
    con = db.connect_to_db()
    labels = pandas.read_sql("""
    SELECT ID_Player, Treatment, Table_Player.ID_Group as ID_Group, Ancestor
    FROM Table_Player
    LEFT JOIN Table_Group
    ON Table_Player.ID_Group = Table_Group.ID_Group
    """, con)

    labels['Generation'] = 1
    labels['Generation'] = (labels.Generation
                                  .where(labels.Treatment != 'Diachronic',
                                         labels.Ancestor))
    labels.drop('Ancestor', axis=1, inplace=True)

    labels['ID_Player'] = labels.ID_Player.astype(int)
    return frame.merge(labels)


def calculate_team_time(trials):
    trials = trials.copy()
    session_duration_sec = 25 * 60
    # Convert milliseconds to seconds
    trials['PlayerTime'] = trials.TrialTime/1000
    trials['TeamTime'] = trials.PlayerTime.where(
        trials.Treatment != 'Diachronic',
        trials.PlayerTime + ((trials.Generation - 1) * session_duration_sec),
    )
    return trials


def freeze_inventory(inventory):
    return '-'.join(map(str, sorted(list(inventory))))


# def extract_trajectories(workshop):
#     workshop = workshop.copy()
#     player_trajectories = (workshop.groupby('ID_Player')
#                                    .apply(convert_to_trajectory)
#                                    .reset_index(level=0))
#     unique_trajectories = player_trajectories.Trajectory.unique()
#     trajectory_labels = {trajectory: ix
#                          for ix, trajectory in enumerate(unique_trajectories)}
#     player_trajectories['TrajectoryID'] = \
#         player_trajectories.Trajectory.map(trajectory_labels)
#     return label_teams_and_strategies(player_trajectories)
#
#
# def convert_to_trajectory(player_workshop):
#     ordered_inventions = (player_workshop.ix[player_workshop.UniqueItem == 1]
#                                          .sort_values('TrialTime')
#                                          .WorkShopResult
#                                          .tolist())
#     trajectory_str = '-'.join(map(str, ordered_inventions))
#     return pandas.DataFrame({'Trajectory': trajectory_str}, index=[0])


def filter_valid_teams(workshop):
    con = db.connect_to_db()
    groups = pandas.read_sql("""
    SELECT ID_Group, Status
    FROM Table_Group
    """, con)
    valid_groups = groups.ix[groups.Status == 'E', 'ID_Group'].tolist()
    return workshop.ix[workshop.ID_Group.isin(valid_groups), ]
from invoke import task

import tasks
from tasks.paths import R_PKG, Path


@task
def use_data(ctx, use_data_script=None, clear_data_dir=False):
    """Compile data to .rda in totems R pkg."""
    if use_data_script is None:
        use_data_scripts = Path(R_PKG, 'data-raw/').listdir('use-data*.R')
    else:
        use_data_scripts = [Path(R_PKG, 'data-raw/', use_data_script + '.R')]

    if clear_data_dir:
        ctx.run('cd {R_pkg} && rm -rf data/*.rda'.format(R_pkg=R_PKG), echo=True)

    cmd = 'cd {R_pkg} && Rscript {use_data_script}'
    for use_data_script in use_data_scripts:
        ctx.run(cmd.format(R_pkg=R_PKG, use_data_script=use_data_script),
                echo=True)


@task
def install(ctx, use_data_too=False, make_graph=False,
            use_data_script=None, document_only=False,
            clear_data_dir=False, ignore_prereqs=False):
    """Install the totems R pkg."""
    cmd = 'cd {R_pkg} && Rscript -e "{R_cmds}"'
    R_cmds = """
    library(devtools)
    document()
    install()
    """.split()

    if not ignore_prereqs:
        crotchet_prereq = "devtools::install_github('pedmiston/crotchet')"
        ctx.run(cmd.format(R_pkg=R_PKG, R_cmds=crotchet_prereq))

    if use_data_too or use_data_script or clear_data_dir:
        use_data(ctx, use_data_script=use_data_script,
                 clear_data_dir=clear_data_dir)

    if make_graph:
        tasks.graph.tree(ctx, view_off=True)
        tasks.graph.tree(ctx, max_generation=6, name='landscape-sample',
                         view_off=True)
        tasks.graph.tree(ctx, max_number=100, name='landscape-tools',
                         view_off=True)

    if document_only:
        R_cmds = ["devtools::document()"]

    ctx.run(cmd.format(R_pkg=R_PKG, R_cmds=';'.join(R_cmds)))
