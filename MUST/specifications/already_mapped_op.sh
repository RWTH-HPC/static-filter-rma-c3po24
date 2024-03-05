#!/bin/bash
#Helper script because using " in the AWK script was not possible
grep \"$1\" must_base_specification.xml >> /dev/null
