#!/usr/bin/env python

"""
This script syncs notes in ~/notes with the Simplenote server. It plays nicely with nvPY.
"""

from simplenote import Simplenote
from ConfigParser import ConfigParser
from os import path, environ, listdir
import re

# The following two functions are copied from the nvPY source code
# Regex to identify the note title - the first line with non-whitespace
note_title_re = re.compile('\s*(.*)\n?')

def get_note_title_file(note):
    """Return the filename corresponding to the note"""
    mo = note_title_re.match(note.get('content', ''))
    if mo:
        fn = mo.groups()[0]
        fn = fn.replace(' ', '_')
        fn = fn.replace('/', '_')
        if not fn:
            return ''
        
        if isinstance(fn, str):
            fn = unicode(fn, 'utf-8')
        else:
            fn = unicode(fn)

        fn += '.txt'
        return fn
    else:
        return ''
    
notesConfig = ConfigParser()
notesConfig.read(path.join(environ['HOME'], '.notes.cfg'))

sn_username = notesConfig.get('notes', 'sn_username')
sn_password = notesConfig.get('notes', 'sn_password')
notes_dir = notesConfig.get('notes', 'notes_dir')

simplenote = Simplenote(sn_username, sn_password)

# Get notes list
notes_without_content, _ = simplenote.get_note_list()

# Populate the notes that are not deleted
notes_with_content = [simplenote.get_note(note['key'])[0] for note in notes_without_content if not note['deleted']]
remote_note_files = { get_note_title_file(note): note for note in notes_with_content }

local_note_files = listdir(unicode(notes_dir, 'utf-8'))

# Upload missing local files to the remote
for local_note_file in local_note_files:
    if local_note_file in remote_note_files.keys():
        # Update the remote note with the contents of the local note,
        # if the local note has been updated after the remote note
        if path.getmtime(path.join(notes_dir, local_note_file)) > float(remote_note_files[local_note_file]['modifydate']):
            with open(path.join(notes_dir, local_note_file), 'r') as f:
                content = f.read()
                remote_note = remote_note_files[local_note_file]
                remote_note['content'] = content
                remote_note['modifydate'] = path.getmtime(path.join(notes_dir, local_note_file))
                simplenote.update_note(remote_note)
    else:
        # Or create a new remote note from the the local note
        with open(path.join(notes_dir, local_note_file), 'r') as f:
            content = f.read()
            local_note = {'content': content}
            simplenote.add_note(local_note)

# Download missing remote files to the local store
for remote_file_name, remote_note in remote_note_files.iteritems():
    if remote_file_name not in local_note_files:
        # Save the contents of the remote note to a local files
        with open(path.join(notes_dir, remote_file_name), 'w') as f:
            f.write(remote_note['content'])
    else:
        # Update the local file with the contents of the remote note,
        # if the remote note has been updated after the local note
        # and the remote note is not marked as deleted
        if float(remote_note['modifydate']) > path.getmtime(path.join(notes_dir, remote_file_name)):
            with open(path.join(notes_dir, remote_file_name), 'w') as f:
                f.write(remote_note['content'])
