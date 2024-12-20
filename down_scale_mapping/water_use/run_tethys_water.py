"""
@Date: 02/20/2022
@author: Yifei Duan
@Project: Tethys V1.0

License:  BSD 2-Clause, see LICENSE and DISCLAIMER files
Copyright (c) 2017, Battelle Memorial Institute

This is an example program showing how to run the model.

"""

import os
from tethys.model import Tethys


if __name__ == "__main__":

    # get path to config.ini in the example dir
    cfg = os.path.join(os.path.dirname(os.path.realpath(__file__)), 'config_ec.ini')
    # run the Tethys model and save outputs
    dmw = Tethys(config=cfg)
    # if needed, reuse the gridded data or gis data output by the model
    grid_outputs = dmw.gridded_data
    gis_outputs = dmw.gis_data