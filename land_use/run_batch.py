import demeter

config_file = 'D:\demeter\demeter\example\demeter_v1.2.0_data_supplement\config_Current-Con.ini'
# run all time steps
demeter.run_model(config_file=config_file, write_outputs=True)

config_file = 'D:\demeter\demeter\example\demeter_v1.2.0_data_supplement\config_Current-Con_CI.ini'
# run all time steps
demeter.run_model(config_file=config_file, write_outputs=True)

config_file = 'D:\demeter\demeter\example\demeter_v1.2.0_data_supplement\config_Updated_NoDAC.ini'
# run all time steps
demeter.run_model(config_file=config_file, write_outputs=True)

config_file = 'D:\demeter\demeter\example\demeter_v1.2.0_data_supplement\config_Updated_NoDAC_CI.ini'
# run all time steps
demeter.run_model(config_file=config_file, write_outputs=True)
