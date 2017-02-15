#!/usr/bin/env python

"""
This script syncs notes in ~/notes with the Simplenote server. It plays nicely with nvPY.
"""

from simplenote import Simplenote
from ConfigParser import ConfigParser
from os import path, environ, listdir
import re

# The following function is copied from the nvPY source code
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
            
            if note_markdown(note):
                fn += '.mkdn'
            else:
                fn += '.txt'
                
                return fn
    else:
        return ''
    
nvpyConfig = ConfigParser()
nvpyConfig.read(path.join(environ['HOME'], '.nvpy.cfg'))

sn_username = nvpyConfig.get('nvpy', 'sn_username')
sn_password = nvpyConfig.get('nvpy', 'sn_password')
notes_dir = nvpyConfig.get('nvpy', 'txt_path')

simplenote = Simplenote(sn_username, sn_password)

# Get notes list
notes, _ = simplenote.get_note_list()

# Which notes do I have locally that are not in the notes list?
remote_note_files = {get_note_title_file(note) : note for note in notes if get_note_title_file(note) != ''}
local_note_files = listdir(notes_dir)

for local_note_file in local_note_files:
    if local_note_file in remote_note_files.keys():
        # Update the remote note with the contents of the local note
        with open(path.join(notes_dir, local_note_file), 'r') as f:
            content = f.read()
            remote_note = remote_note_files[local_note_file]
            remote_note['content'] = content
            simplenote.update_note(remote_note)
    else:
        # Or create a new remote note from the the local note
        with open(path.join(notes_dir, local_note_file), 'r') as f:
            content = f.read()
            local_note = {'content': content}
            simplenote.add_note(local_note)
