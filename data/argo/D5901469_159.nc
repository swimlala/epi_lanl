CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  %   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-20T21:21:43Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        F   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    =    FORMAT_VERSION                 	long_name         File format version    
_FillValue                    =0   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    =4   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    =8   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    =H   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    =X   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    =h   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  =�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  >@   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  �  ?    CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        ?�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    ?�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    ?�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  `  ?�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    @8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    @D   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  `  @H   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  `  @�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  `  A   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    Ah   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           At   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    A�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            A�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           A�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           A�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    A�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    A�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    A�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        D�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    E    PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    E   PROFILE_CNDC_QC                	long_name         #Global quality flag of CNDC profile    conventions       Argo reference table 2a    
_FillValue                    E   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    E   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        I�  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p  ��   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     I�  �<   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     I�  �h   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     I� G$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     I� �P   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     I� �|   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     I� I8   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p ��   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     I� �d   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p �    CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     I� �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     I� KL   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     I� �x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     I� �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � M`   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   N    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   Z    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   f    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � r    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   r�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   r�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   r�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   r�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � r�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , s�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   s�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 s�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        t    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        t,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       t8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 tDArgo profile    3.1 1.2 19500101000000  20181120212143  20200901153721  5901469 5901469 5901469 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               �   �   �AAA AOAOAO  2688                            2688                            2688                            2C  2B  2C  DAD APEX                            APEX                            APEX                            2730                            2730                            2730                            112607                          112607                          112607                          846 846 846 @���c]��@���c]��@���c]��111 @��״%��@��״%��@��״%��@5T9XbN@5T9XbN@5T9XbN�con��P�con��P�con��P111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                          ACA ACA  CA BCA @9��@�  @�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B'��B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�fC  C�C L�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>���=���=���=���>���>L��=���=���>L��>���=���=���>���>L��=���=���=���=���=���=���=���>L��=���=���>L��=���=���>L��=���=���>L��=���=���=���=���=���>���>L��=���=���>L��=���=���=���=���=���=���=���>L��=���>L��=���=���=���=���=���=���>L��=���=���>L��=���=���=���=���>L��=���=���=���=���=���=���=���=���=���=���=���=���=���=���=���=���=���=���=���=���=���=���>���>L��=���=���=���>L��=���=���=���=���=���>L��=���=���=���>���>L��=���=���=���=���=���=���>L��=���=���=���=���=���=���=���=���=���=���=���    >L��=���=���>L��=���=���=���>L��>L��=���=���>���=���=���=���=���=���=���>���>���>���=���>L��>L��=���=���=���=���>L��>���>L��=���=���>L��>���>���=���=���>L��>���>���=���=���=���>���?   >���=���=���=���=���=���=���>���>���>���=���=���=���=���=���=���=���=���=���>���>L��>L��=���>L��=���=���=���=���>L��>L��=���=���>���>���>���>L��>L��>L��=���>���>���=���>���>���>L��>���>���>L��>���>���>���>L��>���>���>���>���>���>���>���>���?   >���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>L��>���>L��>���>���>���>���>���>���>L��>���>���>L��>���>���>���>���>L��?   >���>L��>���>���>���>L��>���>���>���>���>���>���>���>���>���>���>���>���>L��>���>���>���>���>L��>���>���>���>���>L��>���>���>L��>���>���>���>���>���>���>���>���>���>���>���>L��>���>���>L��>���>���>���>���>���>���>���>���>���>���>���>���>���>L��>���>���>���>���>���>L��>���>���>���>���>���>���>���>���>L��>���>���>���>���>���>���>���>L��>���>���>���>���>���>���>L��>���>���>���>���>���>���>���>���>���>���>���>���>L��>L��>���>���>���>���>���=���>���>���>���>���>���>���>���>���>���>���>���>���>���=���>���>���>���>���>���>L��>���>L��>���>���>���>���>���>���>���>���>���?   >���>���>L��>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>L��>���>���>���>���>���>���>���>L��>���>���>���>���>���>���>���>���>L��>���>���>���>���>L��>���>���>���>���>���>L��>���>���>L��>L��=���>���>L��>���>���>���?   ?��?333?333?L��?L��?L��?fff?�  ?���?���?���?���?���?�ff?�ff?�  ?�  ?�  ?�  ?���?���?�ff?�ff?�ff?�ff?�33@   @   @   @ff@��@��@��@33@33@��@   @   @   @&ff@,��@,��@333@9��@9��@@  @Fff@L��@S33@Y��@`  @`  @l��@s33@y��@�  @�33@�ff@���@���@�  @�33@�ff@���@���@�33@�ff@���@���@�  @�33@�ff@���@���@�  @�33@�ff@ə�@���@�  @�33@�ff@ٙ�@���@�  @�33@�ff@陚@���@�  @�ff@���@���A   A33A��AffA  A	��A��AffA  A��A33A��AffA��A33A��AffA   A!��A#33A$��A&ffA(  A+33A,��A.ffA0  A1��A333A4��A6ffA8  A8  A9��A;33A<��A>ffA@  AA��AC33AD��AFffAH  AI��AK33AL��ANffAP  AQ��AT��AVffAX  AY��A[33A\��A`  Aa��Ac33Ad��Ah  Ai��Ak33Al��Ap  Aq��As33At��Ax  Ay��A{33A|��A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�  A���A���A�ffA�  A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A���A�ffA�  A���A���A���A�ffA�  A���A���A�ffA�33A�  A���Ař�A�ffA�33A�  A���Aə�A�ffA�33A�  A���A͙�A�ffA�33A�  A���Aљ�A�ffA�33A�  A���Aՙ�A�ffA�33A�  A���Aٙ�A�ffA�33A�  A���Aݙ�A�ffA�33A�  A���AᙚA�ffA�33A���A噚A�ffA�33A�  A���A�ffA�33A�  A���A홚A�33A�  A���A�A�ffA�  A���A���A�ffA�33A���A���A�ffA�33A���A���A�ffA�33B   B ��B33B��B  BffB33B��B  BffB��B33B  BffB��B33B��B  BffB��B	33B
  B
ffB
��B33B��B  BffB��B33B��B��BffB��B��B33B��B  BffB��B33B��B  BffB��B33B��B  BffB��B33B��B  BffB��B33B��B  B  BffB��B33B��B  BffB��B33B��B  BffB��B33B��B  BffB��B33B33B��B   B ffB ��B!33B!��B"  B"ffB"��B#33B#��B$  B$ffB$��B%33B%��B&  B&ffB&��B'33B'��B(  B(ffB(��B)33B)��B*  B*ffB*��B+33B+��B,  B,ffB,��B-33B-��B.  B.ffB.��B/33B/��B0  B0ffB0��B133B1��B2  B2��B333B3��B4  B4ffB4��B533B5��B6  B6ffB6��B733B7��B8  B8ffB8��B933B9��B:  B:ffB:��B;33B;��B<  B<��B=33B=33B>  B>ffB>��B?33B?��B@  B@ffB@��BA33BA��BB  BBffBB��BC33BC��BDffBD��BE33BE��BF  BFffBF��BG33BG��BH  BHffBI33BI��BJ  BJffBJ��BK33BK��BL  BLffBL��BM��BN  BNffBN��BO33BO��BP  BPffBP��BQ33BR  BRffBR��BS33BS��BT  BTffBT��BU33BU��BV  BVffBV��BW33BW��BX  BXffBY33BY��BZ  BZffBZ��B[33B[��B\  B\ffB\��B]33B]��B^  B^ffB^��B_33B_��B`  B`ffB`��Ba33Ba��Ba��Bb  BbffBb��Bc33Bc��Bc��Bd  BdffBd��Be33Be33Be��Bf  BfffBfffBf��Bg33Bg33Bg��Bh  Bh  BhffBh��Bi33Bi��Bi��Bj  BjffBjffBj��Bk33Bk33Bk��Bl  Bl  BlffBl��Bl��Bm33Bm��Bn  Bn  BnffBnffBn��Bo33Bo��Bo��Bp  BpffBpffBp��Bq33Bq33Bq��Br  Br  BrffBr��Bs33Bs33Bs��Bt  BtffBtffBt��Bu33Bu��Bu��Bv  BvffBv��Bw33Bw33Bw��Bx  BxffBx��By33By33By��Bz  BzffBz��B{33B{33B{��B|  B|ffB|��B}33B}��B}��B~  B~ffB~��B33B��B�  B�  B�33B�ffB���B���B�  B�33B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�  B�33B�ffB���B���B�  B�33B�ffB���B���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB�ffB���B���B�  B�33B���B���B���B�  B�33B���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�ffB���B���B�  B�33B�ffB���B���B�33B�ffB���B���B�  B�33B���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�ffB���B���B�  B�33B���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�ffB���B���B�  B�33B�ffB���B���B�  B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�33B�ffB���B���B�  B�33B���B���B�  B�33B���B���B�  B�33B���B���B�  B�33B���B���B�  B�ffB���B���B�  B�ffB���B���B�  B�ffB���B���B�33B�ffB���B�  B�33B�ffB���B�  B�33B�ffB���B�  B�ffB���B���B�  B�ffB���B���B�33B�ffB���B�  B�33B�ffB���B�  B�33B���B���B�33B�ffB���B�  B�33B�ffB���B�  B�ffB���B���B�33B�ffB���B�  B�33B���B���B�  B�ffB���B�  B�33B�ffB���B�  B�ffB���B���B�33B�ffB���B�  B�33B�B���B�33B�ffBÙ�B�  B�33Bę�B���B�33B�ffBř�B�  B�33Bƙ�B���B�  B�ffBǙ�B�  B�33Bș�B���B�33B�ffBə�B�  B�33Bʙ�B���B�33B�ffB˙�B�  B�33B̙�B���B�  B�ffB͙�C��C�3C��C  C�CL�CffC��C�3C�fC  C33CL�C� C��C�3C�fC  C�CL�CffC��C�3C��C  C�C33CL�C� C��C�3C��C  C�C33CL�CffC� C�3C��C�fC  C�C33CL�CffC� C� C��C�3C��C�fC   C   C �C 33C 33C L�C ffC ffC � C ��C ��C �3C �3C ��C ��C �fC �fC!  C!  C!  C!�C!�C!33C!33C!33C!33C!33C!L�C!L�C!L�C!L�C!L�C!L�C!L�C!L�C!L�C!L�C!L�C!L�C!L�C!L�C!L�C!L�C!L�C!33C!33C!33C!33C!�C!�C!�C!�C!  C!  C!  C �fC �fC ��C ��C ��C �3C �3C ��C ��C � C � C � C ff@9��@9��@@  @Fff@L��@S33@Y��@`  @`  @l��@s33@y��@�  @�33@�ff@���@���@�  @�33@�ff@���@���@�33@�ff@���@���@�  @�33@�ff@���@���@�  @�33@�ff@ə�@���@�  @�33@�ff@ٙ�@���@�  @�33@�ff@陚@���@�  @�ff@���@���A   A33A��AffA  A	��A��AffA  A��A33A��AffA��A33A��AffA   A!��A#33A$��A&ffA(  A+33A,��A.ffA0  A1��A333A4��A6ffA8  A8  A9��A;33A<��A>ffA@  AA��AC33AD��AFffAH  AI��AK33AL��ANffAP  AQ��AT��AVffAX  AY��A[33A\��A`  Aa��Ac33Ad��Ah  Ai��Ak33Al��Ap  Aq��As33At��Ax  Ay��A{33A|��A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�  A���A���A�ffA�  A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A���A�ffA�  A���A���A���A�ffA�  A���A���A�ffA�33A�  A���Ař�A�ffA�33A�  A���Aə�A�ffA�33A�  A���A͙�A�ffA�33A�  A���Aљ�A�ffA�33A�  A���Aՙ�A�ffA�33A�  A���Aٙ�A�ffA�33A�  A���Aݙ�A�ffA�33A�  A���AᙚA�ffA�33A���A噚A�ffA�33A�  A���A�ffA�33A�  A���A홚A�33A�  A���A�A�ffA�  A���A���A�ffA�33A���A���A�ffA�33A���A���A�ffA�33B   B ��B33B��B  BffB33B��B  BffB��B33B  BffB��B33B��B  BffB��B	33B
  B
ffB
��B33B��B  BffB��B33B��B��BffB��B��B33B��B  BffB��B33B��B  BffB��B33B��B  BffB��B33B��B  BffB��B33B��B  B  BffB��B33B��B  BffB��B33B��B  BffB��B33B��B  BffB��B33B33B��B   B ffB ��B!33B!��B"  B"ffB"��B#33B#��B$  B$ffB$��B%33B%��B&  B&ffB&��B'33B'��B(  B(ffB(��B)33B)��B*  B*ffB*��B+33B+��B,  B,ffB,��B-33B-��B.  B.ffB.��B/33B/��B0  B0ffB0��B133B1��B2  B2��B333B3��B4  B4ffB4��B533B5��B6  B6ffB6��B733B7��B8  B8ffB8��B933B9��B:  B:ffB:��B;33B;��B<  B<��B=33B=33B>  B>ffB>��B?33B?��B@  B@ffB@��BA33BA��BB  BBffBB��BC33BC��BDffBD��BE33BE��BF  BFffBF��BG33BG��BH  BHffBI33BI��BJ  BJffBJ��BK33BK��BL  BLffBL��BM��BN  BNffBN��BO33BO��BP  BPffBP��BQ33BR  BRffBR��BS33BS��BT  BTffBT��BU33BU��BV  BVffBV��BW33BW��BX  BXffBY33BY��BZ  BZffBZ��B[33B[��B\  B\ffB\��B]33B]��B^  B^ffB^��B_33B_��B`  B`ffB`��Ba33Ba��Ba��Bb  BbffBb��Bc33Bc��Bc��Bd  BdffBd��Be33Be33Be��Bf  BfffBfffBf��Bg33Bg33Bg��Bh  Bh  BhffBh��Bi33Bi��Bi��Bj  BjffBjffBj��Bk33Bk33Bk��Bl  Bl  BlffBl��Bl��Bm33Bm��Bn  Bn  BnffBnffBn��Bo33Bo��Bo��Bp  BpffBpffBp��Bq33Bq33Bq��Br  Br  BrffBr��Bs33Bs33Bs��Bt  BtffBtffBt��Bu33Bu��Bu��Bv  BvffBv��Bw33Bw33Bw��Bx  BxffBx��By33By33By��Bz  BzffBz��B{33B{33B{��B|  B|ffB|��B}33B}��B}��B~  B~ffB~��B33B��B�  B�  B�33B�ffB���B���B�  B�33B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�  B�33B�ffB���B���B�  B�33B�ffB���B���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB�ffB���B���B�  B�33B���B���B���B�  B�33B���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�ffB���B���B�  B�33B�ffB���B���B�33B�ffB���B���B�  B�33B���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�ffB���B���B�  B�33B���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�  B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�33B�ffB���B���B�  B�ffB���B���B�  B�33B�ffB���B���B�  B�ffB���B���B�  B�33B�ffB���B�  B�33B�ffB���B���B�33B�ffB���B���B�  B�33B���B���B�  B�33B���B���B�  B�33B���B���B�  B�33B���B���B�  B�ffB���B���B�  B�ffB���B���B�  B�ffB���B���B�33B�ffB���B�  B�33B�ffB���B�  B�33B�ffB���B�  B�ffB���B���B�  B�ffB���B���B�33B�ffB���B�  B�33B�ffB���B�  B�33B���B���B�33B�ffB���B�  B�33B�ffB���B�  B�ffB���B���B�33B�ffB���B�  B�33B���B���B�  B�ffB���B�  B�33B�ffB���B�  B�ffB���B���B�33B�ffB���B�  B�33B�B���B�33B�ffBÙ�B�  B�33Bę�B���B�33B�ffBř�B�  B�33Bƙ�B���B�  B�ffBǙ�B�  B�33Bș�B���B�33B�ffBə�B�  B�33Bʙ�B���B�33B�ffB˙�B�  B�33B̙�B���B�  B�ffB͙�C��C�3C��C  C�CL�CffC��C�3C�fC  C33CL�C� C��C�3C�fC  C�CL�CffC��C�3C��C  C�C33CL�C� C��C�3C��C  C�C33CL�CffC� C�3C��C�fC  C�C33CL�CffC� C� C��C�3C��C�fC   C   C �C 33C 33C L�C ffC ffC � C ��C ��C �3C �3C ��C ��C �fC �fC!  C!  C!  C!�C!�C!33C!33C!33C!33C!33C!L�C!L�C!L�C!L�C!L�C!L�C!L�C!L�C!L�C!L�C!L�C!L�C!L�C!L�C!L�C!L�C!L�C!33C!33C!33C!33C!�C!�C!�C!�C!  C!  C!  C �fC �fC ��C ��C ��C �3C �3C ��C ��C � C � C � C ffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @5�@{�@�@�A z�A>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�B�RB�RB�RB'Q�B/�RB7�RB?�RBG�RBO�RBW�RB`�Bg�RBo�RBw�RB�RB��)B��)B��)B��)B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�zC�C�C :�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>k� <�<�<�>k� >�<�<�>�>k� <�<�>k� >�<�<�<�<�<�<�<�>�<�<�>�<�<�>�<�<�>�<�<�<�<�<�>k� >�<�<�>�<�<�<�<�<�<�<�>�<�>�<�<�<�<�<�<�>�<�<�>�<�<�<�<�>�<�<�<�<�<�<�<�<�<�<�<�<�<�<�<�<�<�<�<�<�<�<�>k� >�<�<�<�>�<�<�<�<�<�>�<�<�<�>k� >�<�<�<�<�<�<�>�<�<�<�<�<�<�<�<�<�<�<���\)>�<�<�>�<�<�<�>�>�<�<�>���<�<�<�<�<�<�>k� >���>k� <�>�>�<�<�<�<�>�>k� >�<�<�>�>k� >k� <�<�>�>���>���<�<�<�>k� >�(�>���<�<�<�<�<�<�>k� >k� >k� <�<�<�<�<�<�<�<�<�>k� >�>�<�>�<�<�<�<�>�>�<�<�>k� >k� >k� >�>�>�<�>���>k� <�>k� >k� >�>k� >k� >�>k� >k� >k� >�>k� >k� >k� >k� >k� >���>k� >���>�(�>k� >���>k� >���>k� >k� >k� >k� >k� >k� >k� >k� >k� >k� >k� >���>k� >k� >k� >���>k� >�>k� >�>���>k� >k� >k� >k� >k� >�>k� >k� >�>k� >k� >���>k� >�>�(�>���>�>k� >k� >k� >�>���>k� >k� >k� >k� >���>k� >���>k� >k� >k� >���>�>k� >k� >k� >���>�>k� >k� >k� >���>�>���>���>�>k� >k� >���>���>k� >k� >���>���>k� >���>k� >�>k� >k� >�>���>���>k� >���>���>k� >k� >k� >k� >���>k� >���>���>�>���>���>k� >k� >k� >�>k� >k� >k� >���>���>���>���>k� >�>k� >k� >k� >k� >k� >k� >k� >�>k� >k� >k� >k� >k� >k� >�>k� >k� >���>k� >���>���>���>k� >���>���>k� >k� >�>�>k� >k� >k� >���>k� <�>���>k� >k� >���>���>k� >k� >k� >���>k� >k� >k� >k� <�>���>k� >���>���>���>�>k� >�>k� >k� >���>k� >k� >���>k� >k� >k� >�(�>���>k� >�>k� >k� >k� >k� >k� >k� >k� >k� >k� >���>k� >k� >���>���>k� >���>k� >k� >k� >k� >k� >k� >k� >�>���>���>k� >k� >���>k� >k� >�>���>���>���>���>���>���>���>k� >�>k� >k� >k� >k� >�>k� >���>k� >k� >k� >�>k� >k� >�>�<�>���>�>k� >���>���>�(�?�?!G�?!G�?:�H?:�H?:�H?Tz�?n{?��
?��
?���?���?���?�p�?�p�?�
=?�
=?�
=?�
=?��
?��
?�p�?�p�?�p�?�p�?�=p?�
=?�
=?�
=@�@Q�@Q�@Q�@�R@�R@�@�@�@�@!�@(Q�@(Q�@.�R@5�@5�@;�@A�@HQ�@N�R@U�@[�@[�@hQ�@n�R@u�@{�@���@�(�@�\)@��\@�@���@�(�@�\)@��\@���@�(�@�\)@��\@�@���@�(�@�\)@��\@�@���@�(�@�\)@ʏ\@�@���@�(�@�\)@ڏ\@�@���@�(�@�\)@�\@�@�(�@�\)@��\@�A{A�AG�A�HAz�A�AG�A�HAz�A{A�AG�Az�A{A�AG�A�HA z�A"{A#�A%G�A&�HA*{A+�A-G�A.�HA0z�A2{A3�A5G�A6�HA6�HA8z�A:{A;�A=G�A>�HA@z�AB{AC�AEG�AF�HAHz�AJ{AK�AMG�AN�HAPz�AS�AUG�AV�HAXz�AZ{A[�A^�HA`z�Ab{Ac�Af�HAhz�Aj{Ak�An�HApz�Ar{As�Av�HAxz�Az{A{�A~�HA�=qA�
>A���A�p�A�=qA��
A���A�p�A�
>A��
A���A�p�A�
>A��
A�p�A�=qA�
>A��
A�p�A�=qA�
>A���A�p�A�=qA�
>A���A�p�A�=qA��
A���A�p�A�=qA��
A���A�p�A�=qA��
A���A�p�A�=qA��
A���A�p�A�=qA��
A���A�p�A�=qA�
>A���A�p�A�=qA�
>A��
A���A�p�A�=qA�
>A��
A�p�A�=qA�
>A�
>A��
A�p�A�=qA�
>A��
A£�A�p�A�=qA�
>A��
Aƣ�A�p�A�=qA�
>A��
Aʣ�A�p�A�=qA�
>A��
AΣ�A�p�A�=qA�
>A��
Aң�A�p�A�=qA�
>A��
A֣�A�p�A�=qA�
>A��
Aڣ�A�p�A�=qA�
>A��
Aޣ�A�p�A�=qA�
>A��
A��A�=qA�
>A��
A��A�p�A�=qA��
A��A�p�A�=qA�
>A��A�p�A�=qA�
>A��
A�p�A�=qA�
>A��
A���A�=qA�
>A��
A���A�=qA�
>A��
A���A�p�B �B �BQ�B�RB�B�BQ�B�RB�B�B�B�RB�B�B�BQ�B�RB�B�B�B	�RB
�B
�B
�BQ�B�RB�B�B�BQ�BQ�B�B�B�B�BQ�B�RB�B�B�BQ�B�RB�B�B�BQ�B�RB�B�B�BQ�B�RB�B�B�BQ�B�RB�RB�B�B�BQ�B�RB�B�B�BQ�B�RB�B�B�BQ�B�RB�B�B�B�BQ�B�RB �B �B �B!Q�B!�RB"�B"�B"�B#Q�B#�RB$�B$�B$�B%Q�B%�RB&�B&�B&�B'Q�B'�RB(�B(�B(�B)Q�B)�RB*�B*�B*�B+Q�B+�RB,�B,�B,�B-Q�B-�RB.�B.�B.�B/Q�B/�RB0�B0�B0�B1Q�B1�RB2�B2�B3Q�B3�RB4�B4�B4�B5Q�B5�RB6�B6�B6�B7Q�B7�RB8�B8�B8�B9Q�B9�RB:�B:�B:�B;Q�B;�RB<�B<�B<�B=�RB>�B>�B>�B?Q�B?�RB@�B@�B@�BAQ�BA�RBB�BB�BB�BCQ�BD�BD�BD�BEQ�BE�RBF�BF�BF�BGQ�BG�RBH�BH�BIQ�BI�RBJ�BJ�BJ�BKQ�BK�RBL�BL�BMQ�BM�RBN�BN�BN�BOQ�BO�RBP�BP�BP�BQ�RBR�BR�BR�BSQ�BS�RBT�BT�BT�BUQ�BU�RBV�BV�BV�BWQ�BW�RBX�BX�BYQ�BY�RBZ�BZ�BZ�B[Q�B[�RB\�B\�B\�B]Q�B]�RB^�B^�B^�B_Q�B_�RB`�B`�B`�BaQ�BaQ�Ba�RBb�Bb�Bb�BcQ�BcQ�Bc�RBd�Bd�Bd�Bd�BeQ�Be�RBf�Bf�Bf�Bf�Bf�BgQ�Bg�RBg�RBh�Bh�Bh�BiQ�BiQ�Bi�RBj�Bj�Bj�Bj�Bj�BkQ�Bk�RBk�RBl�Bl�Bl�Bl�BmQ�Bm�RBm�RBn�Bn�Bn�Bn�BoQ�BoQ�Bo�RBp�Bp�Bp�Bp�Bp�BqQ�Bq�RBq�RBr�Br�Br�Br�BsQ�Bs�RBt�Bt�Bt�Bt�BuQ�BuQ�Bu�RBv�Bv�Bv�Bv�BwQ�Bw�RBx�Bx�Bx�Bx�ByQ�By�RBz�Bz�Bz�Bz�B{Q�B{�RB|�B|�B|�B}Q�B}Q�B}�RB~�B~�B~�BQ�B�RB�RB�\B�B�B�u�B���B��)B�\B�\B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B��)B�\B�B�B�u�B���B��)B�\B�B�B�u�B�u�B���B��)B�\B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B�\B�B�B�u�B�u�B��)B�\B�B�B�B�B�u�B���B��)B�\B�u�B�u�B���B��)B�\B�u�B�u�B��)B�\B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B�B�B�u�B���B��)B�\B�B�B�u�B���B�\B�B�B�u�B���B��)B�\B�u�B���B��)B�\B�B�B���B��)B�\B�B�B�u�B���B��)B�B�B�u�B���B��)B�\B�u�B���B��)B�\B�B�B�u�B��)B�\B�B�B�u�B���B��)B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B�B�B�u�B���B��)B�\B�B�B�u�B��)B�\B�B�B�u�B���B�\B�B�B�u�B���B��)B�\B�u�B���B��)B�\B�u�B���B��)B�\B�u�B���B��)B�\B�u�B���B��)B�B�B�u�B���B��)B�B�B�u�B���B��)B�B�B�u�B���B�\B�B�B�u�B��)B�\B�B�B�u�B��)B�\B�B�B���B��)B�B�B�u�B���B��)B�B�B�u�B���B�\B�B�B�u�B��)B�\B�B�B���B��)B�\B�u�B���B�\B�B�B�u�B��)B�\B�B�B���B��)B�B�B�u�B���B�\B�B�B���B��)B�\B�u�B���B��)B�B�B�u�B��)B�\B�B�B���B��)B�B�B�u�B���B�\B�B�B���B��)B�\B�u�B¨�B�\B�B�B�u�B��)B�\B�u�BĨ�B�\B�B�B�u�B��)B�\B�u�Bƨ�B��)B�B�B�u�B��)B�\B�u�BȨ�B�\B�B�B�u�B��)B�\B�u�Bʨ�B�\B�B�B�u�B��)B�\B�u�B̨�B��)B�B�B�u�C��C�GC��C�C�C:�CTzC��C�GC�zC�C!GC:�CnC��C�GC�zC�C�C:�CTzC��C�GC��C�C�C!GC:�CnC��C�GC��C�C�C!GC:�CTzCnC�GC��C�zC�C�C!GC:�CTzCnCnC��C�GC��C�zC�C�C �C !GC !GC :�C TzC TzC nC ��C ��C �GC �GC ��C ��C �zC �zC �C �C �C!�C!�C!!GC!!GC!!GC!!GC!!GC!:�C!:�C!:�C!:�C!:�C!:�C!:�C!:�C!:�C!:�C!:�C!:�C!:�C!:�C!:�C!:�C!:�C!!GC!!GC!!GC!!GC!�C!�C!�C!�C �C �C �C �zC �zC ��C ��C ��C �GC �GC ��C ��C nC nC nC Tz@5�@5�@;�@A�@HQ�@N�R@U�@[�@[�@hQ�@n�R@u�@{�@���@�(�@�\)@��\@�@���@�(�@�\)@��\@���@�(�@�\)@��\@�@���@�(�@�\)@��\@�@���@�(�@�\)@ʏ\@�@���@�(�@�\)@ڏ\@�@���@�(�@�\)@�\@�@�(�@�\)@��\@�A{A�AG�A�HAz�A�AG�A�HAz�A{A�AG�Az�A{A�AG�A�HA z�A"{A#�A%G�A&�HA*{A+�A-G�A.�HA0z�A2{A3�A5G�A6�HA6�HA8z�A:{A;�A=G�A>�HA@z�AB{AC�AEG�AF�HAHz�AJ{AK�AMG�AN�HAPz�AS�AUG�AV�HAXz�AZ{A[�A^�HA`z�Ab{Ac�Af�HAhz�Aj{Ak�An�HApz�Ar{As�Av�HAxz�Az{A{�A~�HA�=qA�
>A���A�p�A�=qA��
A���A�p�A�
>A��
A���A�p�A�
>A��
A�p�A�=qA�
>A��
A�p�A�=qA�
>A���A�p�A�=qA�
>A���A�p�A�=qA��
A���A�p�A�=qA��
A���A�p�A�=qA��
A���A�p�A�=qA��
A���A�p�A�=qA��
A���A�p�A�=qA�
>A���A�p�A�=qA�
>A��
A���A�p�A�=qA�
>A��
A�p�A�=qA�
>A�
>A��
A�p�A�=qA�
>A��
A£�A�p�A�=qA�
>A��
Aƣ�A�p�A�=qA�
>A��
Aʣ�A�p�A�=qA�
>A��
AΣ�A�p�A�=qA�
>A��
Aң�A�p�A�=qA�
>A��
A֣�A�p�A�=qA�
>A��
Aڣ�A�p�A�=qA�
>A��
Aޣ�A�p�A�=qA�
>A��
A��A�=qA�
>A��
A��A�p�A�=qA��
A��A�p�A�=qA�
>A��A�p�A�=qA�
>A��
A�p�A�=qA�
>A��
A���A�=qA�
>A��
A���A�=qA�
>A��
A���A�p�B �B �BQ�B�RB�B�BQ�B�RB�B�B�B�RB�B�B�BQ�B�RB�B�B�B	�RB
�B
�B
�BQ�B�RB�B�B�BQ�BQ�B�B�B�B�BQ�B�RB�B�B�BQ�B�RB�B�B�BQ�B�RB�B�B�BQ�B�RB�B�B�BQ�B�RB�RB�B�B�BQ�B�RB�B�B�BQ�B�RB�B�B�BQ�B�RB�B�B�B�BQ�B�RB �B �B �B!Q�B!�RB"�B"�B"�B#Q�B#�RB$�B$�B$�B%Q�B%�RB&�B&�B&�B'Q�B'�RB(�B(�B(�B)Q�B)�RB*�B*�B*�B+Q�B+�RB,�B,�B,�B-Q�B-�RB.�B.�B.�B/Q�B/�RB0�B0�B0�B1Q�B1�RB2�B2�B3Q�B3�RB4�B4�B4�B5Q�B5�RB6�B6�B6�B7Q�B7�RB8�B8�B8�B9Q�B9�RB:�B:�B:�B;Q�B;�RB<�B<�B<�B=�RB>�B>�B>�B?Q�B?�RB@�B@�B@�BAQ�BA�RBB�BB�BB�BCQ�BD�BD�BD�BEQ�BE�RBF�BF�BF�BGQ�BG�RBH�BH�BIQ�BI�RBJ�BJ�BJ�BKQ�BK�RBL�BL�BMQ�BM�RBN�BN�BN�BOQ�BO�RBP�BP�BP�BQ�RBR�BR�BR�BSQ�BS�RBT�BT�BT�BUQ�BU�RBV�BV�BV�BWQ�BW�RBX�BX�BYQ�BY�RBZ�BZ�BZ�B[Q�B[�RB\�B\�B\�B]Q�B]�RB^�B^�B^�B_Q�B_�RB`�B`�B`�BaQ�BaQ�Ba�RBb�Bb�Bb�BcQ�BcQ�Bc�RBd�Bd�Bd�Bd�BeQ�Be�RBf�Bf�Bf�Bf�Bf�BgQ�Bg�RBg�RBh�Bh�Bh�BiQ�BiQ�Bi�RBj�Bj�Bj�Bj�Bj�BkQ�Bk�RBk�RBl�Bl�Bl�Bl�BmQ�Bm�RBm�RBn�Bn�Bn�Bn�BoQ�BoQ�Bo�RBp�Bp�Bp�Bp�Bp�BqQ�Bq�RBq�RBr�Br�Br�Br�BsQ�Bs�RBt�Bt�Bt�Bt�BuQ�BuQ�Bu�RBv�Bv�Bv�Bv�BwQ�Bw�RBx�Bx�Bx�Bx�ByQ�By�RBz�Bz�Bz�Bz�B{Q�B{�RB|�B|�B|�B}Q�B}Q�B}�RB~�B~�B~�BQ�B�RB�RB�\B�B�B�u�B���B��)B�\B�\B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B��)B�\B�B�B�u�B���B��)B�\B�B�B�u�B�u�B���B��)B�\B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B�\B�B�B�u�B�u�B��)B�\B�B�B�B�B�u�B���B��)B�\B�u�B�u�B���B��)B�\B�u�B�u�B��)B�\B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B�B�B�u�B���B��)B�\B�B�B�u�B���B�\B�B�B�u�B���B��)B�\B�u�B���B��)B�\B�B�B���B��)B�\B�B�B�u�B���B��)B�B�B�u�B���B��)B�\B�u�B���B��)B�\B�B�B�u�B��)B�\B�B�B�u�B���B��)B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B�B�B�u�B���B��)B�\B�B�B�u�B���B��)B�B�B�u�B���B��)B�\B�B�B�u�B��)B�\B�B�B�u�B���B�\B�B�B�u�B���B��)B�\B�u�B���B��)B�\B�u�B���B��)B�\B�u�B���B��)B�\B�u�B���B��)B�B�B�u�B���B��)B�B�B�u�B���B��)B�B�B�u�B���B�\B�B�B�u�B��)B�\B�B�B�u�B��)B�\B�B�B���B��)B�B�B�u�B���B��)B�B�B�u�B���B�\B�B�B�u�B��)B�\B�B�B���B��)B�\B�u�B���B�\B�B�B�u�B��)B�\B�B�B���B��)B�B�B�u�B���B�\B�B�B���B��)B�\B�u�B���B��)B�B�B�u�B��)B�\B�B�B���B��)B�B�B�u�B���B�\B�B�B���B��)B�\B�u�B¨�B�\B�B�B�u�B��)B�\B�u�BĨ�B�\B�B�B�u�B��)B�\B�u�Bƨ�B��)B�B�B�u�B��)B�\B�u�BȨ�B�\B�B�B�u�B��)B�\B�u�Bʨ�B�\B�B�B�u�B��)B�\B�u�B̨�B��)B�B�B�u�C��C�GC��C�C�C:�CTzC��C�GC�zC�C!GC:�CnC��C�GC�zC�C�C:�CTzC��C�GC��C�C�C!GC:�CnC��C�GC��C�C�C!GC:�CTzCnC�GC��C�zC�C�C!GC:�CTzCnCnC��C�GC��C�zC�C�C �C !GC !GC :�C TzC TzC nC ��C ��C �GC �GC ��C ��C �zC �zC �C �C �C!�C!�C!!GC!!GC!!GC!!GC!!GC!:�C!:�C!:�C!:�C!:�C!:�C!:�C!:�C!:�C!:�C!:�C!:�C!:�C!:�C!:�C!:�C!:�C!!GC!!GC!!GC!!GC!�C!�C!�C!�C �C �C �C �zC �zC ��C ��C ��C �GC �GC ��C ��C nC nC nC TzG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AɬAɮAɺ^AɼjAɾwA���A���A���AɼjAɧ�Aɝ�Aɩ�A�"�Aȩ�A�33A���A���A�{A��
Ař�A�v�A�v�A�9XA�Aę�A�-A�&�A�%A���A�ZA��!A��-A��wA���A��/A���A�oA��!A�dZA��;A���A���A���A�dZA�JA���A�x�A��!A��A�ȴA��DA�/A��`A���A��hA�VA��9A�v�A�+A��jA�G�A�JA���A��A���A�r�A��#A���A�/A���A��A��DA��RA��A�ȴA�ffA�E�A��A�A��`G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��HA�hsA�Q�A�C�AɅA�S�A��#A�1'Aɡ�Aȗ�A�ĜA�dZAɃA��A��PA�x�A���A�  A�%A�G�A�=qA���A�hsA�r�A�
=A��HA�$�AƏ\A���AɍPA��A��#A���A�VA��;A���Aɇ+A���A��^A�33A�&�A��A�/A�Aá�AĶFA�r�AȃA�C�AǃAǕ�A�\)A���A��;A��`A��9A�
=A��TA���A�=qA��DA���A�A�A��RA�%A��A�~�A�ffA���A���A��DA��A��A���A�ZA�z�A��A��A�1'A���A��wAģ�A�-A�hsA�VA���A�A�A�ffA�^5A�A���A���A�C�A�7LA��A�$�A���A�A�A�ZA��A�I�A���A�~�A�5?A�`BA��RA�bA��A�K�A�C�A�dZA���A���A�`BA��`A�r�A��-A��RA��A�t�A��uA�p�A���A���A+A�t�A�Aȩ�A���A��`A���A�ffA�O�A�A�AɍPA�`BA��A��+A�G�A���A���A�t�AɑhAɅA��A�`BA�v�A�&�A�`BA�r�A��/Aƕ�A�|�A�p�A�=qAĸRA�|�A�v�A�z�A���AÑhA�`BA�|�A�~�A��/A��FA���A�ĜAɋDAɅA�p�A��A�hsA�9XA�jA�&�A�"�AɋDA�~�A��;A�hsA���Aȥ�A���A�A�A�;dA�^5AȓuA��AɓuAɇ+A�
=AżjA�z�A�{A���A�"�A�A�AɋDA�1'A��;A�|�AɑhAɍPAȣ�A�ZAɅAɅAǓuAɇ+A���AɅAɁAɃA��AɋDA�|�Aɇ+A�~�AɃAɅAɃAɃA�z�AɃA�p�Aɇ+AɅAɋDAɕ�Aɕ�AɓuAɕ�AɓuAɕ�AɋDAɑhAɏ\AɑhAɍPAɍPAɏ\AɍPAɋDAɋDAɏ\Aɏ\AɍPAɏ\AɅAɉ7AɋDAɇ+Aɉ7Aɉ7AɑhAɇ+AɋDAɉ7Aɉ7Aɇ+Aɉ7AɃAɍPAɉ7AɅAɛ�Aɝ�Aɛ�Aɏ\Aɇ+AɍPAɉ7Aɏ\AɓuAɗ�AɓuAɏ\Aɏ\AɑhAɏ\Aɏ\Aɏ\AɍPAɍPAɑhAɏ\AɍPAɋDAɑhAɍPAɏ\AɍPAɍPAɍPAɍPAɍPAɍPAɍPAɋDAɍPAɏ\AɋDAɏ\AɋDAɍPAɉ7AɍPAɋDAɋDAɋDAɏ\AɋDAɉ7A�1AɍPAɇ+AɋDAɋDAɍPAɓuAɏ\AɁAɑhAɍPAɉ7AɋDAɋDAɋDAɉ7Aɇ+AɁAɇ+Aɇ+AɃA�~�AɁA�z�A�~�AɁAɁAɁAɇ+AɋDAɇ+A�|�Aɇ+Aɉ7Aɇ+AɅAɇ+A�"�A�|�A�~�Aɉ7AɁAɇ+AɅAɅA�~�A�hsA�t�AɋDAɁAɇ+Aɏ\Aɇ+AɃAɁA�z�Aɗ�AɑhAɕ�Aɇ+Aɇ+AɓuAɍPAɇ+AɓuAə�A�n�Aɉ7AɋDAɋDAɋDAɏ\AɋDAɏ\A�ĜAɏ\AɑhAɑhAɏ\AɓuA�M�Aɇ+Aɏ\AɍPAɍPAɏ\A�`BAɏ\AɃAɍPAɍPAɑhAɉ7Aɉ7AɍPAɍPAɍPAɉ7Aɉ7AɓuAɑhAɁAɍPAɏ\Aɏ\AɍPAɍPAɏ\AɑhAɏ\AɃAɕ�Aɏ\AɓuAɓuAɓuAɏ\AɍPAɓuAɏ\Aɉ7Aɉ7AɍPAɑhAɏ\AɋDAɏ\Aɛ�Aɉ7Aɕ�Aɝ�Aə�AɍPAɑhAɓuAɝ�Aɝ�Aɟ�AɓuAɟ�Aɟ�Aɡ�Aɟ�Aɝ�Aɡ�Aɝ�Aɝ�AɓuAɟ�Aɣ�Aɟ�Aɝ�Aɟ�Aɡ�Aɡ�AɓuAɛ�A�VAɇ+Aȡ�Aɛ�A���Aɝ�Aɛ�Aɡ�Aɣ�Aɣ�Aɡ�Aɣ�Aɣ�Aɣ�Aɣ�Aɣ�Aɣ�Aɣ�Aɣ�Aɡ�Aɡ�Aɟ�Aɥ�Aɟ�Aɣ�Aɟ�Aɝ�Aɟ�Aɣ�Aɥ�Aɧ�Aɥ�Aɥ�Aɥ�Aɣ�Aɥ�Aɥ�Aɣ�Aɣ�Aɣ�Aɥ�Aɣ�Aɣ�Aɥ�Aɥ�Aɥ�Aɣ�Aɥ�Aɩ�Aɧ�Aɩ�Aɩ�Aɩ�Aɩ�Aɩ�Aɩ�Aɥ�Aɧ�Aɧ�Aɧ�AɬAɩ�AɬAɬAɩ�Aɩ�Aɰ!Aɲ-Aɲ-Aɰ!AɮAɮAɬAɰ!AɮAɴ9AɸRAɶFAɶFAɺ^AɸRAɶFAɸRAɸRAɸRAɺ^Aɺ^Aɺ^AɸRAɸRAɺ^AɼjAɼjAɾwAɼjA���Aɺ^AɶFAɺ^AɶFAɶFAɸRAɶFAɺ^Aɺ^Aɺ^AɾwAɾwA���A���A�A���A���AɾwAɾwA���A���AɾwAɾwAɼjAɼjAɺ^AɼjAɼjAɺ^Aɺ^AɼjAɼjAɼjAɺ^AɼjAɺ^AɼjAɼjAɼjA���AɾwAɾwAɾwAɾwAɾwAɾwAɾwAɾwAɾwAɾwA���A���A�A�A�A���AɾwAɼjAɾwAɾwAɼjAɼjAɸRAɸRAɸRAɼjAɼjAɾwAɼjAɶFAɺ^Aɺ^Aɺ^AɾwAɾwA���AɾwA���A�AɾwA���A���AɾwA���A���A�AɾwA���A���A���AɾwAɾwA���AɾwAɾwAɰ!Aɺ^AɬAɩ�Aɩ�Aɩ�Aɧ�Aɧ�Aɩ�Aɧ�Aɧ�Aɧ�Aɥ�Aɥ�Aɣ�Aɣ�Aɡ�Aɥ�Aɥ�Aɟ�Aɡ�Aɡ�Aɟ�Aɝ�Aɛ�Aɟ�Aɝ�Aɗ�Aɗ�Aɝ�Aɛ�Aɗ�Aɕ�AɑhAə�Aə�Aə�Aɗ�Aɛ�Aə�Aɗ�Aə�Aə�AɑhAɑhAɝ�Aɣ�AɸRAɲ-Aɴ9Aɛ�AɾwA�A�A�ĜA���A�ZA�dZA�E�A�E�A�A�A�7LA�-A�+A�+A�&�A�&�A�(�A�&�A� �A��A��A�bA��A���A��A��A���A���A��A��
A��
A���A���A���A���A�ȴA�ĜA�ƨA�ƨA�ĜA�ĜA���Aȴ9Aȝ�A�dZA�G�A�  A��/A��#A���A�bNA�C�A��A���A��A���A�Aƴ9AƗ�A�n�A�$�A�VA�JA�1A�1A�1A�1A�%A�%A�A�  A���A��A��`A��`A��yA��mA��mA��mA��mA��TA��HA��/A��#A���A���A�ƨAżjAŶFAŴ9AŶFAŸRAżjAżjAžwA���A�ĜA�A���A��HA��HA��mA��A��A�A�%A�%A�%A�%A�%A�A���A���A���A���A���A���A���A��A��A���A��TA��;A��A���A���A���A���A���A���A���A���A�ȴA�ȴA�ƨA�ƨA�ĜA�ĜAžwAżjAź^AŸRAŰ!Aţ�Aŗ�AōPAŋDAŉ7AōPAŋDAŉ7Aŉ7AŇ+AŇ+AŇ+Aŉ7Aŉ7Aŉ7AŋDAŉ7AŅAŃA�x�A�r�A�p�A�p�A�n�A�jA�hsA�ffA�ffA�hsA�jA�n�A�r�A�z�A�|�A�~�AŇ+AōPAŉ7AŇ+AŅAŃAŁAŁA�~�A�z�A�t�A�p�A�p�A�l�A�hsA�jA�ffA�dZA�`BA�ZA�VA�M�A�E�A�C�A�A�A�A�A�?}A�;dA�9XA�9XA�7LA�33A�/A�-A�-A�(�A�"�A� �A��A��A��A�oA�oA�JA�
=A�%A�A���A���A���A�  A�A�  A���A��A��A��`A��HA��/A��#A��#A���A���A�ƨA�AļjAĸRAĴ9AĬAđhA�t�A�bNA�O�A�E�A�A�A�;dA�9XA�5?A�5?A�33A�1'A�/A�+A�&�A�&�A�$�A�&�A�$�A�&�A�&�A�$�A�$�A�$�A�&�A�$�A�$�A�$�A�$�A�"�A�$�A�"�A�$�A�"�A�"�A�&�A�&�A�&�A�(�A�&�A�(�A�(�A�&�A�"�A��A��A��A��A��A�{A�oA�VA�
=A�
=A�1A�1A�1A�A�A���A��yA��#A�ȴA���Aã�AÍPA�`BA�K�A�E�A�?}A�=qA�1'A�&�A�+A��A�
=A�
=A�A�A���A��A��yA��/A��A���A¶FA®A®A°!A©�A�A�A\A\AA�~�A�|�A�z�A�t�A�t�A�dZA�ffA�l�A�p�A�^5A�^5A�\)A�bNA�^5A�ZA�\)A�XA�VA�\)A�ZA�Q�A�O�A�S�A�G�A�33A�(�A�  A�%A���A���A���A���A��+A�l�A�ffA�\)A�A�A��A��A���A�r�A�S�A�I�A�"�A���A��`A��;A�ƨA��9A���A��uA�5?A�A�bA��/A��A���A��/A��RA��A��^A��-A���A���A��A�~�A�|�A�|�A�|�A�v�A�t�A�bNA�O�A�;dA�$�A�VA���A��A��#A���A���A�ƨA�A�A���A���A��jA��^A��FA��9A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA��hA��hA��\A��PA��PA��DA��DA��DA��PA��DA��A�XA�1'A��A�
=A��A��TA��#A���A��FA���A��DA�t�A�n�A�ffA�^5A�^5A�?}A�;dA�+A��A�  A���A���A���A��A��yA��TA���A���A�x�A�n�A�S�A�;dA�/A�&�A�(�A�&�A�&�A�"�A��A�{A�oA�VA�
=A�A���A�A�
=A�1A���A��A��A��mA��HA��/A��A���A���A���A�ƨA�A��9A���A�v�A�hsA�ffA�`BA�`BA�`BA�bNA�dZA�dZA�bNA�bNA�dZA�dZA�bNA�dZA�bNA�bNA�dZA�dZA�dZA�bNA�dZA�`BA�`BA�\)A�VA�K�A�G�A�;dA�1'A�+A��A�JA���A��;A���A��-A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��PA�n�A�\)A�/A�  A���A���A��uA��A�~�A�|�A�x�A�z�A�z�A�z�A�z�A�x�A�x�A�z�A�|�A�~�A�|�A�z�A�|�A�~�A�~�A�~�A��A��+A��hA��FA��
A��HA��
A��RA���A���A��\A��PA��PA�t�A�hsA�^5A�O�A�E�A�=qA�;dA�9XA�33A�33A�-A�-A�+A�&�A� �A��A��A�VA�A���A��A��mA��yA��A�  A�JA�VA���A��A��yA��`A��TA��;A��/A��/A��HA���A���A���A��7A�jA�VA�-A� �A�33A�E�A�^5A�n�A��A���A���A���A���A��A��!A��-A��-A��-A��!A��!A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A��A���A��A��;A��#A���A���A���A���A���A�ƨA�ƨA���A��-A���A���A���A���A���A���A���A��uA��PA��A��A�~�A�~�A�~�A�v�A�l�A�;dA�5?A�/A�1'A�+A�+A�(�A�$�A�&�A�"�A�"�A� �A��A��A��A��A��A��A��A��A��A�{A�oA�{A�oA�bA�bA�JA�JA�VA�
=A�%A�A���A���A���A���A���A���A���A��A��A��mA��`A��TA��TA��HA��TA��TA��TA��TA��TA��TA��HA��HA��#A��A��
A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A���A��
A��
A��
A��
A��
A��
A��
A��
A��
A��
A��
A��A��A��A��A��#A���A���A��
A��
A��A��A��A���A��/A��`A��HA��HA��HA��HA��yA��yA��A��A��A��A��A��A���A���A���A���A���Aɩ�AɬAɩ�AɬAɮAɬAɧ�Aɩ�AɮAɬAɮAɮAɬAɮAɴ9Aɲ-Aɴ9Aɴ9Aɲ-AɮAɰ!Aɴ9AɶFAɴ9AɸRAɸRAɸRAɸRAɸRAɸRAɺ^AɸRAɺ^AɾwAɼjAɺ^Aɺ^Aɺ^AɼjA���A�AɾwAɾwAɺ^A���AɾwAɺ^AɸRAɸRAɶFAɺ^AɼjAɼjAɾwAɾwA���A���A���A���A�A�ĜA���A���AɾwA���A���A���AɼjAɾwAɼjA���AɾwAɺ^Aɺ^AɾwAɾwA���A���AɾwAɾwAɼjAɾwA���A���A���A���A�A���A���AɾwA���A���A���A���A�A�A�A�A�ĜA�ĜAɾwA�A���AɾwAɾwA���AɼjAɺ^AɼjA���A�AɾwAɾwA���AɼjAɼjAɼjA���A���A���A�A���A�A�ĜA�ĜA���A���A�A�A�A�A�A�A�A���A�A���A���A���AɾwAɼjAɴ9AɮAɬAɮAɩ�Aɩ�AɬAɬAɬAɩ�Aɩ�Aɧ�Aɧ�Aɧ�Aɣ�Aɥ�Aɣ�Aɧ�Aɡ�Aɧ�Aɥ�Aɥ�Aɣ�Aɡ�Aɟ�Aɛ�Aɛ�Aɡ�Aɝ�Aɝ�Aɗ�Aɗ�Aɛ�Aɛ�Aɛ�Aə�Aə�Aɟ�Aɛ�Aɛ�Aɛ�Aɗ�Aɕ�Aə�Aə�Aɟ�Aɲ-AɾwAɸRAɾwA�A�ƨA�A�ƨAɟ�Aɗ�A�hsA�?}A�9XA�;dA�7LA�+A�-A�+A�(�A�(�A�(�A�+A�&�A��A��A�{A��A���A��A��A���A���A��A��#A��A��A��
A���A���A�ȴA�ȴA�ȴA�ƨA�ȴA�ƨA���Aȩ�Aȉ7A�S�A�+A��`A��;A���AǙ�A�bNA�1'A�bA���A��#A���A�ƨAƬAƑhA�^5A��A�bA�
=A�
=A�1A�
=A�1A�1A�1A�%A�A���A��A��mA��yA��yA��yA��yA��yA��mA��`A��HA��/A��A���A���A�AżjAŶFAŶFAŸRAź^AžwAžwAžwA�ĜA�A�ĜA�ȴA���A��HA��mA��A���A�%A�%A�1A�1A�1A�%A�%A�A�  A���A���A���A���A���A��A��A��A��mA��HA��#A��A���A���A���A���A���A���A���A�ȴA�ȴA�ȴA�ȴA�ȴA�ĜA���A���Aź^AŸRAŰ!Aţ�Aŗ�Aŏ\Aŏ\AōPAōPAōPAŉ7AŋDAŋDAŉ7AŋDAŋDAŋDAŋDAŋDAŋDAŇ+AŅA�x�A�t�A�r�A�p�A�p�A�jA�jA�hsA�jA�jA�n�A�r�A�x�A�|�A�~�AŁAŋDAōPAŉ7AŇ+AŅAŅAŃAŃA�~�A�x�A�t�A�p�A�r�A�l�A�jA�jA�jA�dZA�`BA�ZA�XA�M�A�E�A�C�A�C�A�C�A�?}A�=qA�=qA�;dA�7LA�5?A�1'A�/A�-A�(�A�$�A� �A�"�A��A��A��A�bA�JA�
=A�1A�A�  A�  A�A�A�A�  A���A��A��yA��TA��;A��;A��#A��
A���A���A�ƨA�AļjAĸRAĲ-Aģ�AąA�jA�\)A�K�A�E�A�?}A�;dA�;dA�7LA�7LA�33A�1'A�/A�-A�+A�&�A�(�A�(�A�&�A�&�A�(�A�&�A�&�A�&�A�(�A�&�A�&�A�&�A�$�A�$�A�&�A�$�A�$�A�$�A�&�A�&�A�(�A�(�A�+A�(�A�+A�+A�&�A�"�A��A��A��A��A��A��A�oA�bA�JA�
=A�
=A�1A�1A�%A�%A�A�  A���A���A���A��mAò-AËDA�|�A�n�A�dZA�S�A�E�A�9XA�5?A�-A�"�A��A�VA�A�A���A��yA��HA��;A���A¸RA´9A²-A°!A°!A�A�AhA\A7A�~�A�~�A�~�A�v�A�t�A�dZA�p�A�r�A�dZA�^5A�`BA�`BA�bNA�bNA�`BA�^5A�XA�ZA�\)A�ZA�S�A�S�A�VA�Q�A�5?A�{A�%A�A��A���A���A���A��\A�n�A�jA�\)A�=qA��A��mA���A�r�A�\)A�I�A�"�A�
=A��A��A��A��jA���A���A��A�ffA�S�A�;dA�$�A�%A��A��#A���A��jA��FA��A���A��DA��A�~�A�~�A�z�A�x�A�x�A�bNA�M�A�;dA�"�A�VA���A��A��/A���A���A�ȴA�ĜA�ĜA�A���A��wA��jA��RA��-A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA��hA��hA��\A��PA��\A��\A��PA��+A�x�A�C�A�-A�oA�  A��`A��HA��A�ȴA��A���A��A�r�A�n�A�l�A�l�A�bNA�VA�I�A�1'A�VA�  A���A���A��A��yA��`A��#A�ĜA���A��A�`BA�E�A�7LA�-A�(�A�+A�+A�(�A� �A��A��A�oA�bA�1A�A�  A�
=A�JA�A���A��A��A��`A��HA��/A��#A��
A���A���A�ƨA��jA��A��PA�v�A�hsA�hsA�bNA�bNA�dZA�dZA�dZA�dZA�ffA�dZA�dZA�dZA�ffA�ffA�ffA�dZA�dZA�dZA�dZA�dZA�dZA�bNA�bNA�`BA�S�A�O�A�G�A�=qA�5?A�+A��A�
=A��A��
A��^A��!A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��PA�p�A�\)A�/A�A���A���A��hA��A�|�A�|�A�z�A�|�A�z�A�|�A�z�A�z�A�z�A�|�A�|�A�~�A�~�A�~�A��A�~�A��A��A��A��PA���A���A��;A��;A���A��^A��A���A���A���A��\A��A�p�A�bNA�O�A�C�A�=qA�=qA�9XA�5?A�/A�1'A�/A�+A�$�A�"�A��A��A�
=A�  A���A��A��mA��A���A�JA�VA�A��A��A��yA��mA��TA��;A��/A��TA��HA���A���A���A�t�A�bNA�7LA�$�A�$�A�;dA�XA�ffA�z�A���A���A���A���A��A��-A��-A��9A��9A��9A��-A��!A��A��A��A��A��A��!A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A��
A��HA��HA��A���A���A���A���A���A���A�ĜA��^A��!A���A���A���A���A���A���A���A��\A��\A��A��A��A��A�~�A�v�A�?}A�7LA�33A�1'A�-A�+A�+A�&�A�&�A�&�A�$�A�"�A� �A��A� �A��A��A��A��A��A��A��A��A��A�{A�oA�oA�bA�bA�VA�JA�
=A�%A�A���A���A���A���A���A���A���A���A��A��yA��`A��TA��TA��`A��TA��`A��TA��TA��`A��`A��`A��HA��;A��/A��#A��#A��A��
A��
A��
A���A���A���A���A���A���A���A���A���A���A��
A��A��A��A��
A��#A��
A��A��
A��/A��#A��
A��
A��
A���A���A��
A��A��#A��/A��A��A��/A��A��#A��HA��`A��`A��;A��HA��HA��#A��mA��A��A��A��A��A��A��A��A��A���A���A���A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 AɬAɮAɺ^AɼjAɾwA���A���A���AɼjAɧ�Aɝ�Aɩ�A�"�Aȩ�A�33A���A���A�{A��
Ař�A�v�A�v�A�9XA�Aę�A�-A�&�A�%A���A�ZA��!A��-A��wA���A��/A���A�oA��!A�dZA��;A���A���A���A�dZA�JA���A�x�A��!A��A�ȴA��DA�/A��`A���A��hA�VA��9A�v�A�+A��jA�G�A�JA���A��A���A�r�A��#A���A�/A���A��A��DA��RA��A�ȴA�ffA�E�A��A�A��`G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��HA�hsA�Q�A�C�AɅA�S�A��#A�1'Aɡ�Aȗ�A�ĜA�dZAɃA��A��PA�x�A���A�  A�%A�G�A�=qA���A�hsA�r�A�
=A��HA�$�AƏ\A���AɍPA��A��#A���A�VA��;A���Aɇ+A���A��^A�33A�&�A��A�/A�Aá�AĶFA�r�AȃA�C�AǃAǕ�A�\)A���A��;A��`A��9A�
=A��TA���A�=qA��DA���A�A�A��RA�%A��A�~�A�ffA���A���A��DA��A��A���A�ZA�z�A��A��A�1'A���A��wAģ�A�-A�hsA�VA���A�A�A�ffA�^5A�A���A���A�C�A�7LA��A�$�A���A�A�A�ZA��A�I�A���A�~�A�5?A�`BA��RA�bA��A�K�A�C�A�dZA���A���A�`BA��`A�r�A��-A��RA��A�t�A��uA�p�A���A���A+A�t�A�Aȩ�A���A��`A���A�ffA�O�A�A�AɍPA�`BA��A��+A�G�A���A���A�t�AɑhAɅA��A�`BA�v�A�&�A�`BA�r�A��/Aƕ�A�|�A�p�A�=qAĸRA�|�A�v�A�z�A���AÑhA�`BA�|�A�~�A��/A��FA���A�ĜAɋDAɅA�p�A��A�hsA�9XA�jA�&�A�"�AɋDA�~�A��;A�hsA���Aȥ�A���A�A�A�;dA�^5AȓuA��AɓuAɇ+A�
=AżjA�z�A�{A���A�"�A�A�AɋDA�1'A��;A�|�AɑhAɍPAȣ�A�ZAɅAɅAǓuAɇ+A���AɅAɁAɃA��AɋDA�|�Aɇ+A�~�AɃAɅAɃAɃA�z�AɃA�p�Aɇ+AɅAɋDAɕ�Aɕ�AɓuAɕ�AɓuAɕ�AɋDAɑhAɏ\AɑhAɍPAɍPAɏ\AɍPAɋDAɋDAɏ\Aɏ\AɍPAɏ\AɅAɉ7AɋDAɇ+Aɉ7Aɉ7AɑhAɇ+AɋDAɉ7Aɉ7Aɇ+Aɉ7AɃAɍPAɉ7AɅAɛ�Aɝ�Aɛ�Aɏ\Aɇ+AɍPAɉ7Aɏ\AɓuAɗ�AɓuAɏ\Aɏ\AɑhAɏ\Aɏ\Aɏ\AɍPAɍPAɑhAɏ\AɍPAɋDAɑhAɍPAɏ\AɍPAɍPAɍPAɍPAɍPAɍPAɍPAɋDAɍPAɏ\AɋDAɏ\AɋDAɍPAɉ7AɍPAɋDAɋDAɋDAɏ\AɋDAɉ7A�1AɍPAɇ+AɋDAɋDAɍPAɓuAɏ\AɁAɑhAɍPAɉ7AɋDAɋDAɋDAɉ7Aɇ+AɁAɇ+Aɇ+AɃA�~�AɁA�z�A�~�AɁAɁAɁAɇ+AɋDAɇ+A�|�Aɇ+Aɉ7Aɇ+AɅAɇ+A�"�A�|�A�~�Aɉ7AɁAɇ+AɅAɅA�~�A�hsA�t�AɋDAɁAɇ+Aɏ\Aɇ+AɃAɁA�z�Aɗ�AɑhAɕ�Aɇ+Aɇ+AɓuAɍPAɇ+AɓuAə�A�n�Aɉ7AɋDAɋDAɋDAɏ\AɋDAɏ\A�ĜAɏ\AɑhAɑhAɏ\AɓuA�M�Aɇ+Aɏ\AɍPAɍPAɏ\A�`BAɏ\AɃAɍPAɍPAɑhAɉ7Aɉ7AɍPAɍPAɍPAɉ7Aɉ7AɓuAɑhAɁAɍPAɏ\Aɏ\AɍPAɍPAɏ\AɑhAɏ\AɃAɕ�Aɏ\AɓuAɓuAɓuAɏ\AɍPAɓuAɏ\Aɉ7Aɉ7AɍPAɑhAɏ\AɋDAɏ\Aɛ�Aɉ7Aɕ�Aɝ�Aə�AɍPAɑhAɓuAɝ�Aɝ�Aɟ�AɓuAɟ�Aɟ�Aɡ�Aɟ�Aɝ�Aɡ�Aɝ�Aɝ�AɓuAɟ�Aɣ�Aɟ�Aɝ�Aɟ�Aɡ�Aɡ�AɓuAɛ�A�VAɇ+Aȡ�Aɛ�A���Aɝ�Aɛ�Aɡ�Aɣ�Aɣ�Aɡ�Aɣ�Aɣ�Aɣ�Aɣ�Aɣ�Aɣ�Aɣ�Aɣ�Aɡ�Aɡ�Aɟ�Aɥ�Aɟ�Aɣ�Aɟ�Aɝ�Aɟ�Aɣ�Aɥ�Aɧ�Aɥ�Aɥ�Aɥ�Aɣ�Aɥ�Aɥ�Aɣ�Aɣ�Aɣ�Aɥ�Aɣ�Aɣ�Aɥ�Aɥ�Aɥ�Aɣ�Aɥ�Aɩ�Aɧ�Aɩ�Aɩ�Aɩ�AɬAɩ�AɬAɮAɬAɧ�Aɩ�AɮAɬAɮAɮAɬAɮAɴ9Aɲ-Aɴ9Aɴ9Aɲ-AɮAɰ!Aɴ9AɶFAɴ9AɸRAɸRAɸRAɸRAɸRAɸRAɺ^AɸRAɺ^AɾwAɼjAɺ^Aɺ^Aɺ^AɼjA���A�AɾwAɾwAɺ^A���AɾwAɺ^AɸRAɸRAɶFAɺ^AɼjAɼjAɾwAɾwA���A���A���A���A�A�ĜA���A���AɾwA���A���A���AɼjAɾwAɼjA���AɾwAɺ^Aɺ^AɾwAɾwA���A���AɾwAɾwAɼjAɾwA���A���A���A���A�A���A���AɾwA���A���A���A���A�A�A�A�A�ĜA�ĜAɾwA�A���AɾwAɾwA���AɼjAɺ^AɼjA���A�AɾwAɾwA���AɼjAɼjAɼjA���A���A���A�A���A�A�ĜA�ĜA���A���A�A�A�A�A�A�A�A���A�A���A���A���AɾwAɼjAɴ9AɮAɬAɮAɩ�Aɩ�AɬAɬAɬAɩ�Aɩ�Aɧ�Aɧ�Aɧ�Aɣ�Aɥ�Aɣ�Aɧ�Aɡ�Aɧ�Aɥ�Aɥ�Aɣ�Aɡ�Aɟ�Aɛ�Aɛ�Aɡ�Aɝ�Aɝ�Aɗ�Aɗ�Aɛ�Aɛ�Aɛ�Aə�Aə�Aɟ�Aɛ�Aɛ�Aɛ�Aɗ�Aɕ�Aə�Aə�Aɟ�Aɲ-AɾwAɸRAɾwA�A�ƨA�A�ƨAɟ�Aɗ�A�hsA�?}A�9XA�;dA�7LA�+A�-A�+A�(�A�(�A�(�A�+A�&�A��A��A�{A��A���A��A��A���A���A��A��#A��A��A��
A���A���A�ȴA�ȴA�ȴA�ƨA�ȴA�ƨA���Aȩ�Aȉ7A�S�A�+A��`A��;A���AǙ�A�bNA�1'A�bA���A��#A���A�ƨAƬAƑhA�^5A��A�bA�
=A�
=A�1A�
=A�1A�1A�1A�%A�A���A��A��mA��yA��yA��yA��yA��yA��mA��`A��HA��/A��A���A���A�AżjAŶFAŶFAŸRAź^AžwAžwAžwA�ĜA�A�ĜA�ȴA���A��HA��mA��A���A�%A�%A�1A�1A�1A�%A�%A�A�  A���A���A���A���A���A��A��A��A��mA��HA��#A��A���A���A���A���A���A���A���A�ȴA�ȴA�ȴA�ȴA�ȴA�ĜA���A���Aź^AŸRAŰ!Aţ�Aŗ�Aŏ\Aŏ\AōPAōPAōPAŉ7AŋDAŋDAŉ7AŋDAŋDAŋDAŋDAŋDAŋDAŇ+AŅA�x�A�t�A�r�A�p�A�p�A�jA�jA�hsA�jA�jA�n�A�r�A�x�A�|�A�~�AŁAŋDAōPAŉ7AŇ+AŅAŅAŃAŃA�~�A�x�A�t�A�p�A�r�A�l�A�jA�jA�jA�dZA�`BA�ZA�XA�M�A�E�A�C�A�C�A�C�A�?}A�=qA�=qA�;dA�7LA�5?A�1'A�/A�-A�(�A�$�A� �A�"�A��A��A��A�bA�JA�
=A�1A�A�  A�  A�A�A�A�  A���A��A��yA��TA��;A��;A��#A��
A���A���A�ƨA�AļjAĸRAĲ-Aģ�AąA�jA�\)A�K�A�E�A�?}A�;dA�;dA�7LA�7LA�33A�1'A�/A�-A�+A�&�A�(�A�(�A�&�A�&�A�(�A�&�A�&�A�&�A�(�A�&�A�&�A�&�A�$�A�$�A�&�A�$�A�$�A�$�A�&�A�&�A�(�A�(�A�+A�(�A�+A�+A�&�A�"�A��A��A��A��A��A��A�oA�bA�JA�
=A�
=A�1A�1A�%A�%A�A�  A���A���A���A��mAò-AËDA�|�A�n�A�dZA�S�A�E�A�9XA�5?A�-A�"�A��A�VA�A�A���A��yA��HA��;A���A¸RA´9A²-A°!A°!A�A�AhA\A7A�~�A�~�A�~�A�v�A�t�A�dZA�p�A�r�A�dZA�^5A�`BA�`BA�bNA�bNA�`BA�^5A�XA�ZA�\)A�ZA�S�A�S�A�VA�Q�A�5?A�{A�%A�A��A���A���A���A��\A�n�A�jA�\)A�=qA��A��mA���A�r�A�\)A�I�A�"�A�
=A��A��A��A��jA���A���A��A�ffA�S�A�;dA�$�A�%A��A��#A���A��jA��FA��A���A��DA��A�~�A�~�A�z�A�x�A�x�A�bNA�M�A�;dA�"�A�VA���A��A��/A���A���A�ȴA�ĜA�ĜA�A���A��wA��jA��RA��-A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA��hA��hA��\A��PA��\A��\A��PA��+A�x�A�C�A�-A�oA�  A��`A��HA��A�ȴA��A���A��A�r�A�n�A�l�A�l�A�bNA�VA�I�A�1'A�VA�  A���A���A��A��yA��`A��#A�ĜA���A��A�`BA�E�A�7LA�-A�(�A�+A�+A�(�A� �A��A��A�oA�bA�1A�A�  A�
=A�JA�A���A��A��A��`A��HA��/A��#A��
A���A���A�ƨA��jA��A��PA�v�A�hsA�hsA�bNA�bNA�dZA�dZA�dZA�dZA�ffA�dZA�dZA�dZA�ffA�ffA�ffA�dZA�dZA�dZA�dZA�dZA�dZA�bNA�bNA�`BA�S�A�O�A�G�A�=qA�5?A�+A��A�
=A��A��
A��^A��!A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��PA�p�A�\)A�/A�A���A���A��hA��A�|�A�|�A�z�A�|�A�z�A�|�A�z�A�z�A�z�A�|�A�|�A�~�A�~�A�~�A��A�~�A��A��A��A��PA���A���A��;A��;A���A��^A��A���A���A���A��\A��A�p�A�bNA�O�A�C�A�=qA�=qA�9XA�5?A�/A�1'A�/A�+A�$�A�"�A��A��A�
=A�  A���A��A��mA��A���A�JA�VA�A��A��A��yA��mA��TA��;A��/A��TA��HA���A���A���A�t�A�bNA�7LA�$�A�$�A�;dA�XA�ffA�z�A���A���A���A���A��A��-A��-A��9A��9A��9A��-A��!A��A��A��A��A��A��!A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A��
A��HA��HA��A���A���A���A���A���A���A�ĜA��^A��!A���A���A���A���A���A���A���A��\A��\A��A��A��A��A�~�A�v�A�?}A�7LA�33A�1'A�-A�+A�+A�&�A�&�A�&�A�$�A�"�A� �A��A� �A��A��A��A��A��A��A��A��A��A�{A�oA�oA�bA�bA�VA�JA�
=A�%A�A���A���A���A���A���A���A���A���A��A��yA��`A��TA��TA��`A��TA��`A��TA��TA��`A��`A��`A��HA��;A��/A��#A��#A��A��
A��
A��
A���A���A���A���A���A���A���A���A���A���A��
A��A��A��A��
A��#A��
A��A��
A��/A��#A��
A��
A��
A���A���A��
A��A��#A��/A��A��A��/A��A��#A��HA��`A��`A��;A��HA��HA��#A��mA��A��A��A��A��A��A��A��A��A���A���A���A��Aɩ�AɬAɩ�AɬAɮAɬAɧ�Aɩ�AɮAɬAɮAɮAɬAɮAɴ9Aɲ-Aɴ9Aɴ9Aɲ-AɮAɰ!Aɴ9AɶFAɴ9AɸRAɸRAɸRAɸRAɸRAɸRAɺ^AɸRAɺ^AɾwAɼjAɺ^Aɺ^Aɺ^AɼjA���A�AɾwAɾwAɺ^A���AɾwAɺ^AɸRAɸRAɶFAɺ^AɼjAɼjAɾwAɾwA���A���A���A���A�A�ĜA���A���AɾwA���A���A���AɼjAɾwAɼjA���AɾwAɺ^Aɺ^AɾwAɾwA���A���AɾwAɾwAɼjAɾwA���A���A���A���A�A���A���AɾwA���A���A���A���A�A�A�A�A�ĜA�ĜAɾwA�A���AɾwAɾwA���AɼjAɺ^AɼjA���A�AɾwAɾwA���AɼjAɼjAɼjA���A���A���A�A���A�A�ĜA�ĜA���A���A�A�A�A�A�A�A�A���A�A���A���A���AɾwAɼjAɴ9AɮAɬAɮAɩ�Aɩ�AɬAɬAɬAɩ�Aɩ�Aɧ�Aɧ�Aɧ�Aɣ�Aɥ�Aɣ�Aɧ�Aɡ�Aɧ�Aɥ�Aɥ�Aɣ�Aɡ�Aɟ�Aɛ�Aɛ�Aɡ�Aɝ�Aɝ�Aɗ�Aɗ�Aɛ�Aɛ�Aɛ�Aə�Aə�Aɟ�Aɛ�Aɛ�Aɛ�Aɗ�Aɕ�Aə�Aə�Aɟ�Aɲ-AɾwAɸRAɾwA�A�ƨA�A�ƨAɟ�Aɗ�A�hsA�?}A�9XA�;dA�7LA�+A�-A�+A�(�A�(�A�(�A�+A�&�A��A��A�{A��A���A��A��A���A���A��A��#A��A��A��
A���A���A�ȴA�ȴA�ȴA�ƨA�ȴA�ƨA���Aȩ�Aȉ7A�S�A�+A��`A��;A���AǙ�A�bNA�1'A�bA���A��#A���A�ƨAƬAƑhA�^5A��A�bA�
=A�
=A�1A�
=A�1A�1A�1A�%A�A���A��A��mA��yA��yA��yA��yA��yA��mA��`A��HA��/A��A���A���A�AżjAŶFAŶFAŸRAź^AžwAžwAžwA�ĜA�A�ĜA�ȴA���A��HA��mA��A���A�%A�%A�1A�1A�1A�%A�%A�A�  A���A���A���A���A���A��A��A��A��mA��HA��#A��A���A���A���A���A���A���A���A�ȴA�ȴA�ȴA�ȴA�ȴA�ĜA���A���Aź^AŸRAŰ!Aţ�Aŗ�Aŏ\Aŏ\AōPAōPAōPAŉ7AŋDAŋDAŉ7AŋDAŋDAŋDAŋDAŋDAŋDAŇ+AŅA�x�A�t�A�r�A�p�A�p�A�jA�jA�hsA�jA�jA�n�A�r�A�x�A�|�A�~�AŁAŋDAōPAŉ7AŇ+AŅAŅAŃAŃA�~�A�x�A�t�A�p�A�r�A�l�A�jA�jA�jA�dZA�`BA�ZA�XA�M�A�E�A�C�A�C�A�C�A�?}A�=qA�=qA�;dA�7LA�5?A�1'A�/A�-A�(�A�$�A� �A�"�A��A��A��A�bA�JA�
=A�1A�A�  A�  A�A�A�A�  A���A��A��yA��TA��;A��;A��#A��
A���A���A�ƨA�AļjAĸRAĲ-Aģ�AąA�jA�\)A�K�A�E�A�?}A�;dA�;dA�7LA�7LA�33A�1'A�/A�-A�+A�&�A�(�A�(�A�&�A�&�A�(�A�&�A�&�A�&�A�(�A�&�A�&�A�&�A�$�A�$�A�&�A�$�A�$�A�$�A�&�A�&�A�(�A�(�A�+A�(�A�+A�+A�&�A�"�A��A��A��A��A��A��A�oA�bA�JA�
=A�
=A�1A�1A�%A�%A�A�  A���A���A���A��mAò-AËDA�|�A�n�A�dZA�S�A�E�A�9XA�5?A�-A�"�A��A�VA�A�A���A��yA��HA��;A���A¸RA´9A²-A°!A°!A�A�AhA\A7A�~�A�~�A�~�A�v�A�t�A�dZA�p�A�r�A�dZA�^5A�`BA�`BA�bNA�bNA�`BA�^5A�XA�ZA�\)A�ZA�S�A�S�A�VA�Q�A�5?A�{A�%A�A��A���A���A���A��\A�n�A�jA�\)A�=qA��A��mA���A�r�A�\)A�I�A�"�A�
=A��A��A��A��jA���A���A��A�ffA�S�A�;dA�$�A�%A��A��#A���A��jA��FA��A���A��DA��A�~�A�~�A�z�A�x�A�x�A�bNA�M�A�;dA�"�A�VA���A��A��/A���A���A�ȴA�ĜA�ĜA�A���A��wA��jA��RA��-A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA��hA��hA��\A��PA��\A��\A��PA��+A�x�A�C�A�-A�oA�  A��`A��HA��A�ȴA��A���A��A�r�A�n�A�l�A�l�A�bNA�VA�I�A�1'A�VA�  A���A���A��A��yA��`A��#A�ĜA���A��A�`BA�E�A�7LA�-A�(�A�+A�+A�(�A� �A��A��A�oA�bA�1A�A�  A�
=A�JA�A���A��A��A��`A��HA��/A��#A��
A���A���A�ƨA��jA��A��PA�v�A�hsA�hsA�bNA�bNA�dZA�dZA�dZA�dZA�ffA�dZA�dZA�dZA�ffA�ffA�ffA�dZA�dZA�dZA�dZA�dZA�dZA�bNA�bNA�`BA�S�A�O�A�G�A�=qA�5?A�+A��A�
=A��A��
A��^A��!A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��PA�p�A�\)A�/A�A���A���A��hA��A�|�A�|�A�z�A�|�A�z�A�|�A�z�A�z�A�z�A�|�A�|�A�~�A�~�A�~�A��A�~�A��A��A��A��PA���A���A��;A��;A���A��^A��A���A���A���A��\A��A�p�A�bNA�O�A�C�A�=qA�=qA�9XA�5?A�/A�1'A�/A�+A�$�A�"�A��A��A�
=A�  A���A��A��mA��A���A�JA�VA�A��A��A��yA��mA��TA��;A��/A��TA��HA���A���A���A�t�A�bNA�7LA�$�A�$�A�;dA�XA�ffA�z�A���A���A���A���A��A��-A��-A��9A��9A��9A��-A��!A��A��A��A��A��A��!A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A��
A��HA��HA��A���A���A���A���A���A���A�ĜA��^A��!A���A���A���A���A���A���A���A��\A��\A��A��A��A��A�~�A�v�A�?}A�7LA�33A�1'A�-A�+A�+A�&�A�&�A�&�A�$�A�"�A� �A��A� �A��A��A��A��A��A��A��A��A��A�{A�oA�oA�bA�bA�VA�JA�
=A�%A�A���A���A���A���A���A���A���A���A��A��yA��`A��TA��TA��`A��TA��`A��TA��TA��`A��`A��`A��HA��;A��/A��#A��#A��A��
A��
A��
A���A���A���A���A���A���A���A���A���A���A��
A��A��A��A��
A��#A��
A��A��
A��/A��#A��
A��
A��
A���A���A��
A��A��#A��/A��A��A��/A��A��#A��HA��`A��`A��;A��HA��HA��#A��mA��A��A��A��A��A��A��A��A��A���A���A���A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?-/�=�\h>��@�@�@�?lD>6�@1�@�Hk@�I�>	� @E~@�K4>�l�=ƭ.>��@YE�?���=�4�>=�>���?T�=��?�B�@N�\=��&>�-�@�B�> ,�@�׈@�?h=���?Q%?��3>	I�@ d�@�4�@0>�=�_[?Là@� q=[�0=�nD=�5+>�b@��c?"|[@��?�/>��@�/E>�=w��=��=�|[>ϫ@pG�@	'�> �>fԀ>�M@>*�@�2�>��@f�>�?���=X�=_eV=��=�b9=�=�>��Q=�A�=�6�=���?Mw�=��m>q��=��=��W@h]d=�QY=�2�>\��=���>��@�6e@�A5>�z%>�9>oǤ>�'=@�;%>�0@)�>��@�eA>A@@�9�>3>2{�@�5?@�?S?�V>�`@ ��@�;=�J#>Sz@�9@��5>�@�~=��=�/=���>�Q?fn�=�?�=�\�=��?��S??�@�$> �H?�z@�E�=�kf=��[@w�1@�A�?<�i>G��@�E�@�HV=�)5>��@�+@�: =�Z�?]�@�CW@�E$?�K�>`��@�;d?w%>#�P?�5=���>��@�D�@�E$=ג�>K�@�) @�C�@�F�@��Z>�@I��@�Ex@�Ex@X]�=�0+?�J�?Z�v@�FJ@�C�@�I{=��@Jw=�
R?�<�>�@���@�F�@�C�@�G�=��@>.m�@�4�=��g?E<@�>LI�@2��?�#O@�P�@�O�@E:i?�[@�8?>��@m@y@���?0<K@�M@NEN?]L�>3]@�N{@�Ln@�O�@P;@�H@�D�?<9X@�I@�$�@�L�@�K4@�K4@c�@�J�@�K�@�K�@�K�@�J8@�J8@�J8@�J�@�I@�I�@�LY@�N<@�O�@�PH@�PH@�O�@�O�@�PH@�O�@�O�@�N�@�N�@�N�@�N'@�N'@�N'@�N�@�O�@�O7@�N'@�M�@�N�@�N�@�N'@�LY@�L�@�K�@�M@�L�@�M@�N<@�L�@�M@�L�@�L�@�L�@�K�@�M@�N'@�N�@�O�@�R�@�PH@�QY@�M+@�N�@�P�@�LY@�Mj@�O�@�Q�@�Q�@�O7@�N{@�N�@�N{@�N'@�N{@�N{@�Oa@�N�@�N'@�M�@�M�@�Oa@�Mj@�Mj@�M@�M@�L�@�M@�Mj@�M@�M+@�N'@�M�@�M�@�M@�M+@�L�@�L�@�LY@�K�@�L�@�L�@�L�@�N'@�Mj@�Ln@��@�M@�L�@�K�@�J�@�K4@�L�@�M@�N�@�Pr@�L�@�LY@�L�@�K�@�M+@�LY@�K4@�A�@�K^@�I�@�H�@�H@�J8@�H�@�HV@�J8@�If@�L@�LY@�K�@�K4@�LY@�K�@�K4@�J�@�K�@�J8@Hz@�I@�K^@�K�@�J8@�K�@�Mj@�N<@�HV@�H�@�J8@�N{@�P	@�O�@�N{@�L�@�J�@�Ln@�F�@�Ri@�P�@�N�@�L�@�Ln@�N{@�N{@�O7@�O�@�QY@�LY@�K�@�L�@�N<@�N�@�O7@�N{@�N�@97�@�P	@�PH@�O�@�N�@�N{@�M�@��A@�O�@�N�@�N�@�O�@�N�@�N�@�O�@�O�@�Q/@�Q�@�N'@�O7@�N{@�M�@�M@�L�@�N�@�O�@�N�@�N�@�N'@�N�@�N�@�O�@�Oa@�O�@�P�@�Q@�QY@�Q/@�QY@�Ri@�R@�QY@�N<@�Mj@�PH@�O7@�N{@�M@�O�@�QY@�O�@�Oa@�Q@�Se@�R�@�S�@�T�@�Q�@�N�@�T�@�T�@�U�@�U2@�U\@�T�@�T�@�T�@�U�@�U\@�U\@�Vm@�Vm@�U�@�T�@�U\@�V@�U2@�V@�Vm@�Vm@�V@�U2@�T�@�N�@�N�@\C�@�U�@�J8@�Q@�R�@�V�@�V�@�W~@�V�@�W*@�W~@�Wi@�W*@�W*@�W~@�W @�Vm@�V@�V�@�X:@�W�@�V�@�W*@�W*@�W�@�W�@�X�@�X:@�X�@�X�@�X�@�X�@�W�@�X:@�W�@�X:@�X:@�X:@�X�@�Y6@�X�@�Y`@�Y�@�Y�@�Y�@�Y�@�Z@�Zq@�Z�@�Zq@�Zq@�Z�@�Z�@�Z@�Z@�Y�@�Zq@�[-@�Z�@�\>@�\�@�\>@�[�@�\�@�^@�^@�^@�]�@�]�@�^_@�_@�]�@�^�@�`-@�aR@�`�@�`�@�a�@�aR@�aR@�a�@�a�@�b@�b�@�b�@�b�@�b@�bc@�c @�d0@�c�@�d�@�cs@�cs@�cs@�c @�b�@�a�@�bc@�b�@�b�@�c�@�d0@�d0@�eA@�d�@�e�@�e�@�e�@�e�@�e�@�eA@�e�@�e�@�e�@�eA@�e�@�eV@�e@�e@�eV@�d�@�dE@�dE@�d�@�ek@�eV@�eV@�eV@�eV@�e�@�e�@�f@�ff@�f�@�f�@�f�@�f�@�f@�ff@�ff@�ff@�f�@�f{@�g�@�g�@�g�@�g�@�g#@�f�@�f�@�f�@�ff@�ff@�f{@�e�@�e@�eV@�ek@�g#@�f{@�f{@�d�@�e@�e�@�ek@�f'@�g#@�g8@�g8@�g8@�g�@�g�@�g�@�g�@�g�@�g�@�hI@�g�@�h^@�hI@�g�@�g8@�g8@�g8@�f{@�f'@�ek@�cI@�a|@�`�@�_F@�^�@�^�@�_�@�^5@�^5@�^5@�]�@�^5@�]�@�]�@�]:@�\�@�\h@�\@�\�@�[W@�[W@�[B@�ZG@�[@�Y�@�ZG@�[@�X�@�X�@�Y�@�Y�@�ZG@�Wi@�WT@�W�@�W�@�Xy@�Xd@�X�@�X%@�X@�W�@�Wi@�V�@�UG@�V@�Y!@�^J@�_�@�^J@�]�@�]:@�a(@�a(@�_@�[�@�N@�:T@�1Q@�/�@�-�@�*@�*@�$�@�"�@�")@�"�@�!�@� �@�?@��@�+@��@�_@�
�@�	l@�[@�@�:@�&@���@��@��D@��H@���@���@���@���@��@��9@��W@��p@��h@��i@��@�̎@��@���@��@��h@��G@���@�i/@�]�@�Ta@�L@�H@�@y@�<6@�6@�/o@�$@��@�D@�@�w@�w@��@��@�f@�U@��@�
@��@��@�
@�
�@��@�
�@�
�@�
�@�
=@��@�[@��@�}@�\@��@�;@��@�_@�_@�b@��@�@�3@�@��@�T@��@�&�@�-w@�1�@�4�@�8q@�>�@�C�@�D(@�C�@�C�@�B�@�A�@�@�@�?�@�?)@�>l@�=\@�<�@�<K@�;:@�9�@�9@�6�@�3r@�1Q@�0�@�/�@�.�@�.@�-�@�-�@�-8@�,�@�+�@�*�@�*E@�)�@�(�@�'�@�&B@�$�@�#@� �@��@��@�H@�j@�Z@�E@�E@�E@�^@��@�^@�^@�^@��@��@��@�@�o@��@�M@��@��@�
�@�N@�@�o@�,@��@�+@�n@�L@�6@�#�@�+�@�0U@�3@�6@�=q@�?�@�@�@�At@�A�@�At@�A @�@d@�?S@�=�@�;@�:T@�:�@�: @�9C@�9�@�9.@�7�@�7"@�5?@�3�@�1�@�0j@�0@�/�@�.�@�.
@�-8@�-8@�,|@�+�@�*Z@�)J@�(�@�'|@�&l@�$�@�#�@�"�@�!�@� @��@��@�*@�m@��@��@�;@�;@��@��@��@��@��@��@�b@�(@��@�J@�@��@��@�@�G@�&@��@���@���@��o@��>@��@��@���@��@�ݭ@��D@���@���@�܇@�܇@���@���@���@�ݭ@��Y@�ݭ@�ݭ@�ݭ@��@��@��@��j@��U@��U@��U@��j@��j@��j@��j@��j@��@��@��@��j@��&@��z@��7@���@���@���@��&@���@�ܱ@�ۡ@�ڐ@���@��@��o@�ײ@��M@���@��,@��@��@���@��-@���@���@���@��!@�Ŭ@��@��@���@���@���@���@���@�~(@�w�@�r�@�nD@�j�@�f{@�c@�\�@�W�@�T�@�P	@�Jb@�F_@�A�@�:�@�5T@�1@�/o@�.
@�)�@�&W@�#%@��@�@��@��@��@��@��@�o@�	l@��@�@��@�P@�  @���@��3@���@���@��f@��4@��g@��@���@���@��@��*@���@��@���@���@���@��`@��@��p@��@�r2@�d@�]�@�VC@�I{@�7�@�(�@��@��@��@��@��@���@���@���@��?@��#@���@��d@���@�|F@�t@�gw@�bc@�`�@�Z�@�U�@�N�@�Jw@�E�@�@d@�;�@�:T@�9@�9@�7�@�6�@�5�@�3�@�/Z@�)_@�"h@��@�@��@�	@�)@�P@���@���@���@��"@���@���@���@���@��3@��8@���@��@@��@��g@��@��@���@��@��@���@���@��@��@���@��V@��V@��V@��@��@��9@��@��>@��@��p@��@��!@���@���@��@��N@��@��@@��@���@���@���@��U@���@���@��@��*@���@�{t@�v�@�t�@�s.@�q�@�m�@�h�@�cI@�Y�@�N'@�HV@�E�@�D�@�At@�>-@�:~@�5T@�+V@�O@�]@��@��@��@���@��C@� ?@���@���@���@��j@���@���@���@���@��3@� T@�}@��@�`@���@���@��@��f@��Z@��I@��,@��t@��@��@���@���@��w@��1@���@��?@���@�Ɠ@���@���@���@���@���@��?@��?@���@��.@��.@�ć@��3@��7@�¹@���@���@��@���@���@���@���@���@��K@��"@��	@���@���@��-@���@���@�}A@�w@�t�@�s�@�r�@�r�@�s@�r�@�rq@�rq@�r@�q�@�q�@�p�@�p�@�o�@�o�@�n/@�m]@�l@�k'@�j@�h^@�fQ@�c�@�a�@�^�@�Y�@�Q�@�H�@�<K@�*�@�X@�A@��@�P@��X@���@���@���@���@� @� �@��@�P@�`@��@�h@�@��@��@�	-@�
�@�E@�4@��@�`@�/o@�I�@�U�@�X%@�QY@�J�@�D�@�?}@�;�@�9�@�6;@�0j@�)J@�#�@�!@� @�\@�\@� @��@�y@��@��@�@��@�	@�	@�P@��@��@�s@��@��@�8@�a@�#�@�%�@�#�@�@�j@��@��@�s@��@��@�@��@���@���@���@��n@�֡@�ȴ@�à@��e@��0@���@��@���@�
�@��@�@�@��@�!�@�">@�#O@�$�@�%F@�&@�&�@�(@�(�@�(�@�)t@�)�@�*@�*@�)�@�*o@�*Z@�*�@�+,@�*�@�+,@�+k@�+k@�+�@�,(@�,�@�.@�1'@�6@�Ft@�\�@�d�@�f�@�d�@�b�@�a|@�`�@�_�@�_@�^t@�]@�Y�@�U�@�Sz@�R~@�R*@�Q/@�PH@�O�@�M�@�K�@�JM@�G�@�F�@�F @�E�@�C�@�A�@�>W@�U@�#@��@�,@��@��@�5@�9@��@�
=@�	�@��@��@��@��@��@��@��@�@�S@��@�l@�@��@��@��@�!@��@�;@� ~@��.@��3@���@��@@��@��@���@���@��]@���@���@��b@��@���@��@��@���@��	@���@��@��@��@���@���@��	@���@��	@��	@��]@���@���@��Y@���@���@���@���@���@� �@�6@�l@�>@��@�@�[@�	@�	�@�N@�J@��@��@��@�4@��@�0@�+@��@�'@�{@�w@�@�]@�@�n@�Y@�Y@�@�@��@��@�P@�@�U@�Y@��@��@��@��@��@��@�@�@�U@��@�X@��@�6@� �@�")@�!�@�!�@��>@��}@���@���@���@���@���@���@���@��@��!@��`@���@��:@��|@��W@��W@��W@��l@��`@��W@��@���@���@���@���@���@��5@��t@��5@��_@���@��J@��@���@���@��o@���@��@��#@��@��@��A@���@��#@���@���@��@��@��@��E@��V@���@��@��(@���@��w@��M@���@���@���@��@��@��@���@��@��@��g@��@���@��@��@��@��@��g@���@��b@���@��M@��@��#@��#@���@���@���@��s@��@��@��@��^@��@���@���@��@���@��+@��Z@��0@��+@���@��I@��o@���@��@��I@��
@��@��|@��M@���@��+@���@���@��@��@��@��@���@��@��@���@���@���@��j@��+@���@���@��@���@���@���@��'@���@��@��U@��@@���@��+@��+@���@��@��|@���@��>@��$@��)@��-@��@��g@��R@��@��W@���@��W@���@��@��@��@��[@���@��W@��@��@��t@��`@��@��q@��@��@���@��S@���@���@��@��C@���@��C@��@��}@���@��C@��@��\@��@��@���@��>@��,@���@��w@��
@��<@��M@��{@���@��@��i@�߹@���@���@��<@���@���@���@���@��p@��@���@���@���@���@���@��1@���@���@��<@��{@��4@��I@���@��B@���@��[@���@��@��q@���@���@���@���@���@���@��@�~|@�sX@�[l@�N{@�1�@�0@@�/o@�#%@�
�@���@�� @��@��@��j@��U@���@�˧@��P@���@���@���@��D@��D@��@���@���@��r@���@���@��@��Q@���@���@��4@���@���@���@��@��k@��p@���@���@���@��m@���@���@��@��@���@���@��@��k@���@���@���@��'@���@���@���@���@���@���@�Ԁ@���@�ײ@��^@�׈@��M@��g@��k@���@�ҳ@��@��c@�л@��)@�͟@���@��F@��!@��T@��]@���@���@��@���@��@���@��+@���@���@��b@���@��f@���@���@���@��
@��|@���@���@���@���@���@��D@���@���@��D@��@���@���@��
@��@��@��@��@���@��+@��D@���@��c@���@�� @���@���@��@��,@���@���@���@��m@��G@���@���@���@��n@��@��W@���@��1@�ӄ@�Ӯ@��1@��1@���@���@��u@��%@��@��d@�ʗ@��O@��@��>@���@���@��@��?@��"@���@��e@��e@���@���@���@��D@���@���@���@��A@���@���@��=@���@��[@���@��$@���@���@���@��6@���@��@��\@���@��m@�� @���@��}@���@���@���@��Q@��@��@���@��#@��Z@��N@���@��F@���@���@��O@��@���@�|p@�w�@�s�@�q"@�oT@�nD@�m�@�m	@�l�@�l�@�m�@�m�@�l�@�n@�n/@�nn@�nD@�nn@�n�@�n�@�o*@�o?@�oT@�o?@�oT@�o~@�o~@�o~@�o~@�oT@�o~@�o�@�o~@�o�@�p�@�q�@�r\@�s�@�s�@�sm@�s�@�sX@�p�@�o�@�n�@�oT@�o@�n�@�m�@�mr@�l�@�k�@�k<@�j�@�j�@�j+@�i�@�i/@�h^@�g�@�f�@�f{@�c�@�]d@�H�@�5+@�/@�'�@�"�@�\@�Q@��@�_@�	-@�@��@���@��E@���@��o@���@���@���@��9@�ρ@�͟@��[@���@���@�õ@��Y@��s@��E@��@��O@���@���@��"@��`@���@��H@���@��@���@���@��E@���@��@���@��9@��(@���@��@��@���@���@��@��:@���@�xB@�i�@�f�@�\�@�b�@�P�@�2#@�,�@��@��@�
@�W@��=@���@��'@��\@��@���@��v@�{�@�o@�k�@�d�@�X�@�M�@�GZ@�>�@�3�@�*�@�!�@�e@�Z@�P@���@��=@��1@��@��@�߹@��j@�ٔ@�؄@��E@��Z@�؄@�׈@���@��q@�@���@���@��T@��@��,@��(@���@���@���@��c@��S@���@��l@���@��`@��:@���@���@���@��3@���@��@���@���@���@��@���@���@��L@��T@���@��;@���@���@���@���@���@���@���@���@��*@���@��e@���@���@��@��}@�y)@�k�@�`�@�Z�@�J�@�I@�Ex@�>�@�3�@�+A@�!@��@��@��@��@�@��@�	-@���@��@���@��X@��C@��@��@�ߤ@��/@�Ԁ@�ŗ@��,@���@��@���@���@���@��@���@��R@���@��[@���@���@���@��F@��F@��t@��j@���@��Q@���@��9@���@��x@���@��@��6@��%@��*@���@���@���@���@�t @�fQ@�`�@�b@�`�@�a(@�b�@�c^@�c�@�c^@�c�@�cs@�cs@�c�@�c�@�c�@�c�@�c�@�c�@�c�@�c @�c @�cI@�b�@�a�@�a=@�\�@�Z2@�V�@�Q/@�MU@�I{@�C�@�:@�1{@�%p@��@��@��@��@�#@��@��@�8@�8@�I@��@�
@��@��@��@��@�@��@�I@�
@��@��@�w@��@�(@�@�A@�	�@���@���@���@���@���@���@��b@���@���@��5@���@���@��1@���@���@��@��V@���@���@��@��I@��
@��^@���@��@���@���@���@���@�ݭ@��g@���@��@��@��.@��@���@�ܜ@���@��x@��\@��"@��V@��
@��@���@���@���@���@��0@��@��@���@��(@���@���@���@���@��@��P@���@��C@��>@��"@�Ɠ@��@��j@���@���@���@���@���@���@���@���@��@���@���@�xB@�n�@�Y�@�R@�P	@�c�@�z�@��#@���@���@���@��@��@��V@���@���@���@���@���@���@���@���@�@�¹@���@���@���@���@�õ@���@���@��@�õ@��a@��7@��L@��a@��;@��U@���@���@���@���@��@��@��@��@�&@��X@���@���@���@��&@��@��+@��'@��I@��J@��@���@��9@���@���@���@���@���@��6@��`@��z@��;@���@���@���@��$@��@���@��9@���@���@���@���@���@���@��!@���@���@���@�� @��)@��@���@��@���@���@��z@��P@��@��*@��*@���@��m@���@��@���@��Q@��@��I@��@���@��A@��@���@���@��,@��@��|@���@���@���@��N@��@��
@��@��@��k@��@���@��A@���@���@��N@���@��|@���@���@���@���@���@���@���@��|@���@��$@���@���@��I@���@���@���@��I@��s@���@���@���@���@��@��@���@��E@���@���@��s@���@���@���@���@���@���@���@���@��E@���@��@���@��@��@��@��@���@��8@���@���@��I@��^@���@���@��^@��^@���@��Y@��/@��nG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     44443444334434443444444434434334444434443444434444344444344444343444444444444444434444433444434443434433444344334344444444444443443344334443443344344444334433334433344433344444333344344444433443433433443333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333343333333333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�G�O�G�O�G�O�@�Hg@�I�G�O�G�O�@�K5G�O�G�O�G�O�@YE�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@N�[G�O�G�O�@�B�G�O�@�׋@�?jG�O�G�O�G�O�G�O�G�O�@�4�G�O�G�O�G�O�@� oG�O�G�O�G�O�G�O�@��dG�O�G�O�G�O�G�O�@�/FG�O�G�O�G�O�G�O�G�O�@pG�G�O�G�O�G�O�G�O�G�O�@�2�G�O�@f�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@h]mG�O�G�O�G�O�G�O�G�O�@�6i@�A8G�O�G�O�G�O�G�O�@�;&G�O�G�O�G�O�@�e@G�O�@�9�G�O�G�O�@�5A@�?PG�O�G�O�G�O�@�=G�O�G�O�@�>@��3G�O�@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�E�G�O�G�O�@w�=@�A�G�O�G�O�@�E�@�HZG�O�G�O�G�O�@�:G�O�G�O�@�CV@�E'G�O�G�O�@�;bG�O�G�O�G�O�G�O�G�O�@�D�@�E$G�O�G�O�@�)"@�C�@�F�@��YG�O�G�O�@�E|@�Ey@X]�G�O�G�O�G�O�@�FH@�C�@�I~G�O�G�O�G�O�G�O�G�O�@���@�F�@�C�@�G�G�O�G�O�@�4�G�O�G�O�G�O�G�O�G�O�G�O�@�P�@�O�G�O�G�O�@�8G�O�@m@~@���G�O�@�M~@NEMG�O�G�O�@�N}@�Lo@�O�@P>@�H @�D�G�O�@�I@�$�@�L�@�K0@�K5@c�z@�J�@�K�@�K�@�K�@�J:@�J7@�J7@�J�@�I@�I�@�LY@�N>@�O�@�PI@�PL@�O�@�O�@�PL@�O�@�O�@�N�@�N�@�N�@�N,@�N%@�N%@�N�@�O�@�O6@�N)@�M�@�N�@�N�@�N*@�LW@�L�@�K�@�L�@�L�@�M@�NA@�L�@�L�@�L�@�L�@�L�@�K�@�M@�N%@�N�@�O�@�R�@�PK@�QV@�M)@�N�@�P�@�LZ@�Mj@�O�@�Q�@�Q�@�O<@�Nw@�N�@�Nw@�N*@�N|@�Nz@�Oc@�N�@�N*@�M�@�M�@�Ob@�Mg@�Mj@�M@�M~@�L�@�M@�Mg@�M@�M.@�N&@�M�@�M�@�M@�M,@�L�@�L�@�LZ@�K�@�L�@�L�@�L�@�N*@�Ml@�Lm@��@�M@�L�@�K�@�J�@�K8@�L�@�M@�N�@�Pw@�L�@�LY@�L�@�K�@�M.@�LZ@�K5@�A�@�K^@�I�@�H�@�H@�J9@�H�@�HV@�J9@�Ie@�L@�L\@�K�@�K5@�L\@�K�@�K2@�J�@�K�@�J>G�O�@�I@�K^@�K�@�J9@�K�@�Ml@�N>@�HV@�H�@�J9@�Nz@�P@�O�@�Nw@�L�@�J�@�Ls@�F�@�Rh@�P�@�N�@�L�@�Lo@�Nz@�Nz@�O7@�O�@�Q_@�L]@�K�@�L�@�N<@�N�@�O<@�N}@�N�G�O�@�P	@�PL@�O�@�N�@�Nv@�M�@��D@�O�@�N�@�N�@�O�@�N�@�N�@�O�@�O�@�Q5@�Q�@�N'@�O9@�Nz@�M�@�M@�L�@�N�@�O�@�N�@�N�@�N&@�N�@�N�@�O�@�Ob@�O�@�P�@�Q@�QV@�Q*@�Q^@�Ri@�R@�QZ@�N>@�Mg@�PI@�O7@�N}@�M~@�O�@�QW@�O�@�O`@�Q@�Sg@�R�@�S�@�T�@�Q�@�N�@�T�@�T�@�U�@�U0@�U^@�T�@�T�@�T�@�U�@�U[@�U\@�Vo@�Vj@�U�@�T�@�U\@�V@�U3@�V@�Vn@�Vo@�V@�U6@�T�@�N�@�N�@\C�@�U�@�J7@�Q@�R�@�V�@�V�@�W@�V�@�W-@�W@�Wj@�W,@�W*@�W@�W@�Vo@�V@�V�@�X;@�W�@�V�@�W,@�W'@�W�@�W�@�X�@�XA@�X�@�X�@�X�@�X�@�W�@�X>@�W�@�X;@�X:@�X;@�X�@�Y7@�X�@�Yf@�Y�@�Y�@�Y�@�Y�@�Z@�Zt@�Z�@�Zq@��?@��@���@���@���@���@���@��@���@��@��#@��_@���@��:@��z@��V@��V@��Y@��j@��b@��V@��@���@���@���@���@���@��3@��v@��2@��]@���@��J@��@���@���@��r@���@��@��"@��@��@��@@���@��&@���@���@��@��@��@��I@��T@���@��@��*@���@��w@��N@���@���@���@��@��@��@���@��@��@��f@��@���@��@��@��@��@��e@���@��a@���@��P@��@��"@��%@���@���@���@��v@��@��@��@��^@��@���@���@��@���@��-@��W@��2@��.@���@��J@��n@���@��@��M@��
@��@��~@��Q@���@��.@���@���@��@��@��@��@���@��@��@���@���@���@��n@��-@���@���@��@���@���@���@��$@���@��@��U@��B@���@��+@��-@���@��@��z@���@��B@��(@��*@��-@��@��j@��U@��@��Z@���@��W@���@��@��@��@��^@���@��W@��@��
@��v@��b@��@��s@��@��@��@��T@���@���@��@��D@���@��D@��@��~@���@��D@��@��]@��
@��
@���@��>@��.@���@��t@��
@��<@��K@��|@���@��@��m@�߻@���@���@��@@���@���@���@���@��o@��@���@���@���@���@���@��/@���@���@��=@��|@��2@��J@���@��B@���@��\@���@��@��n@���@���@���@���@���@���@��@�~z@�s[@�[l@�Nx@�1�@�0>@�/q@�#"@�
�@���@��"@��@��@��m@��Z@���@�˧@��N@���@���@���@��E@��G@��@���@���@��s@���@���@��"@��R@���@���@��5@���@���@���@��@��j@��o@���@���@���@��n@���@���@��
@��@���@���@��@��j@���@���@���@��(@���@���@���@���@���@���@�Ԃ@���@�״@��a@�׈@��R@��f@��n@���@�ҵ@��
@��`@�о@��)@�͞@���@��F@��"@��Y@��]@���@���@��~@���@��@���@��+@���@���@��`@���@��f@���@���@���@��@��}@���@���@���@���@���@��C@���@���@��D@��@���@���@��
@�� @��@��@��@���@��0@��C@���@��b@���@��"@���@���@��@��/@���@���@���@��o@��F@���@���@���@��m@��@��U@���@��-@�ӆ@�Ӯ@��2@��.@���@���@��x@��"@��@��d@�ʖ@��N@��@��?@���@���@��
@��B@��'@���@��c@��f@���@���@���@��F@���@���@���@��A@���@���@��@@���@��[@���@��%@���@���@���@��6@���@��@��[@���@��m@���@���@��~@���@���@���@��K@��@��@���@�� @��V@��M@���@��F@���@���@��Q@��@���@�|r@�w�@�s�@�q"@�o\@�nB@�m�@�m
@�l�@�l�@�m�@�m�@�l�@�n @�n/@�nq@�nB@�nm@�n�@�n�@�o+@�o>@�oT@�o>@�oT@�o�@�o@�o�@�o@�oT@�o}@�o�@�o�@�o�@�p�@�q�@�rb@�s�@�s�@�sk@�s�@�s[@�p�@�o�@�n�@�oT@�o@�n�@�m�@�mr@�l�@�k�@�k9@�j�@�j�@�j-@�i�@�i2@�h\@�g�@�f�@�f�@�c�@�]b@�H�@�5+@�/@�'�@�#@�]@�S@��@�]@�	-@�@��@���@��F@���@��q@���@���@���@��8@�ς@�͢@��Z@��@���@�ö@��]@��t@��F@��@��O@���@���@��$@��`@���@��F@���@��@���@���@��B@���@��@���@��:@��(@���@��@��@���@���@��@��8@���@�xB@�i�@�f�@�\�@�b�@�P�@�2"@�,�@��@��@�@�Z@��=@���@��(@��\@��
@���@��y@�{�@�o@�k�@�d�@�X�@�M�@�GZ@�>�@�3�@�*�@�!�@�g@�[@�R@���@��;@��1@��@��@�߾@��k@�ٔ@�؃@��C@��]@�؉@�׋@���@��o@�@���@���@��V@��
@��+@��,@���@���@���@��f@��Q@���@��n@���@��b@��<@���@���@���@��6@���@��@���@���@���@��@���@���@��J@��U@���@��<@���@���@���@���@���@���@���@���@��)@���@��c@���@���@��@��{@�y,@�k�@�`�@�Z�@�J�@�I@�Ex@�>�@�3�@�+?@�!@��@��@��@��@�@��@�	)@���@��~@���@��X@��E@��
@��@�ߤ@��1@��@�Ŗ@��,@���@��@���@���@���@��@���@��N@���@��^@���@���@���@��I@��G@��w@��j@���@��M@���@��6@���@��@���@��@��6@��$@��&@���@���@���@���@�t@�fU@�`�@�b@�`�@�a,@�b�@�c^@�c�@�c^@�c�@�cr@�cv@�c�@�c�@�c�@�c�@�c�@�c�@�c�@�c @�c#@�cJ@�b�@�a�@�aA@�\�@�Z/@�V�@�Q.@�MW@�I~@�C@�:@�1~@�%m@��@��@��@��@�&@��@��@�9@�:@�J@��@�@��@��@��@��@�@��@�N@�@��@��@�{@��@�(@�@�B@�	�@���@���@���@���@���@���@��c@���@���@��4@���@���@��4@���@���@��@��Y@���@���@��@��D@��
@��[@���@��@���@���@���@���@�ݮ@��d@���@��@��@��*@��@���@�ܞ@���@��y@��Z@��"@��W@��
@��@���@���@���@���@��5@��
@��@���@��)@���@���@���@���@��@��P@���@��B@��?@��$@�Ɩ@��@��o@���@���@���@���@���@���@���@���@��@���@���@�xF@�n�@�Y�@�R@�P@�c�@�z�@��(@���@���@���@��@��@��T@���@���@���@���@���@���@���@���@�@�º@���@���@���@���@�õ@���@���@��@�ö@��a@��6@��M@��a@��:@��R@���@���@���@���@�� @�� @��@��@�*@��W@���@���@���@��)@��}@��/@��&@��I@��L@��@���@��=@���@���@���@���@���@��:@��f@��w@��>@���@���@���@��&@��@���@��8@���@���@���@���@���@���@��!@���@���@���@��@��*@��@���@��@���@���@��w@��Q@��@��*@��,@���@��s@���@��@���@��P@��@��I@��@���@��?@���@���@���@��*@��@��}@��@���@���@��L@��@��@��@��@��h@��@���@��E@���@���@��L@���@��}@���@���@���@���@���@���@���@���@���@��$@���@���@��N@���@���@���@��J@��t@���@���@���@���@��@��@���@��F@���@���@��v@���@���@���@���@���@���@���@���@��G@���@��@���@��@��@��@��@���@��7@���@���@��L@��`@���@���@��[@��[@���@��\@��3@��o@��?@��@���@���@���@���@���@��@���@��@��#@��_@���@��:@��z@��V@��V@��Y@��j@��b@��V@��@���@���@���@���@���@��3@��v@��2@��]@���@��J@��@���@���@��r@���@��@��"@��@��@��@@���@��&@���@���@��@��@��@��I@��T@���@��@��*@���@��w@��N@���@���@���@��@��@��@���@��@��@��f@��@���@��@��@��@��@��e@���@��a@���@��P@��@��"@��%@���@���@���@��v@��@��@��@��^@��@���@���@��@���@��-@��W@��2@��.@���@��J@��n@���@��@��M@��
@��@��~@��Q@���@��.@���@���@��@��@��@��@���@��@��@���@���@���@��n@��-@���@���@��@���@���@���@��$@���@��@��U@��B@���@��+@��-@���@��@��z@���@��B@��(@��*@��-@��@��j@��U@��@��Z@���@��W@���@��@��@��@��^@���@��W@��@��
@��v@��b@��@��s@��@��@��@��T@���@���@��@��D@���@��D@��@��~@���@��D@��@��]@��
@��
@���@��>@��.@���@��t@��
@��<@��K@��|@���@��@��m@�߻@���@���@��@@���@���@���@���@��o@��@���@���@���@���@���@��/@���@���@��=@��|@��2@��J@���@��B@���@��\@���@��@��n@���@���@���@���@���@���@��@�~z@�s[@�[l@�Nx@�1�@�0>@�/q@�#"@�
�@���@��"@��@��@��m@��Z@���@�˧@��N@���@���@���@��E@��G@��@���@���@��s@���@���@��"@��R@���@���@��5@���@���@���@��@��j@��o@���@���@���@��n@���@���@��
@��@���@���@��@��j@���@���@���@��(@���@���@���@���@���@���@�Ԃ@���@�״@��a@�׈@��R@��f@��n@���@�ҵ@��
@��`@�о@��)@�͞@���@��F@��"@��Y@��]@���@���@��~@���@��@���@��+@���@���@��`@���@��f@���@���@���@��@��}@���@���@���@���@���@��C@���@���@��D@��@���@���@��
@�� @��@��@��@���@��0@��C@���@��b@���@��"@���@���@��@��/@���@���@���@��o@��F@���@���@���@��m@��@��U@���@��-@�ӆ@�Ӯ@��2@��.@���@���@��x@��"@��@��d@�ʖ@��N@��@��?@���@���@��
@��B@��'@���@��c@��f@���@���@���@��F@���@���@���@��A@���@���@��@@���@��[@���@��%@���@���@���@��6@���@��@��[@���@��m@���@���@��~@���@���@���@��K@��@��@���@�� @��V@��M@���@��F@���@���@��Q@��@���@�|r@�w�@�s�@�q"@�o\@�nB@�m�@�m
@�l�@�l�@�m�@�m�@�l�@�n @�n/@�nq@�nB@�nm@�n�@�n�@�o+@�o>@�oT@�o>@�oT@�o�@�o@�o�@�o@�oT@�o}@�o�@�o�@�o�@�p�@�q�@�rb@�s�@�s�@�sk@�s�@�s[@�p�@�o�@�n�@�oT@�o@�n�@�m�@�mr@�l�@�k�@�k9@�j�@�j�@�j-@�i�@�i2@�h\@�g�@�f�@�f�@�c�@�]b@�H�@�5+@�/@�'�@�#@�]@�S@��@�]@�	-@�@��@���@��F@���@��q@���@���@���@��8@�ς@�͢@��Z@��@���@�ö@��]@��t@��F@��@��O@���@���@��$@��`@���@��F@���@��@���@���@��B@���@��@���@��:@��(@���@��@��@���@���@��@��8@���@�xB@�i�@�f�@�\�@�b�@�P�@�2"@�,�@��@��@�@�Z@��=@���@��(@��\@��
@���@��y@�{�@�o@�k�@�d�@�X�@�M�@�GZ@�>�@�3�@�*�@�!�@�g@�[@�R@���@��;@��1@��@��@�߾@��k@�ٔ@�؃@��C@��]@�؉@�׋@���@��o@�@���@���@��V@��
@��+@��,@���@���@���@��f@��Q@���@��n@���@��b@��<@���@���@���@��6@���@��@���@���@���@��@���@���@��J@��U@���@��<@���@���@���@���@���@���@���@���@��)@���@��c@���@���@��@��{@�y,@�k�@�`�@�Z�@�J�@�I@�Ex@�>�@�3�@�+?@�!@��@��@��@��@�@��@�	)@���@��~@���@��X@��E@��
@��@�ߤ@��1@��@�Ŗ@��,@���@��@���@���@���@��@���@��N@���@��^@���@���@���@��I@��G@��w@��j@���@��M@���@��6@���@��@���@��@��6@��$@��&@���@���@���@���@�t@�fU@�`�@�b@�`�@�a,@�b�@�c^@�c�@�c^@�c�@�cr@�cv@�c�@�c�@�c�@�c�@�c�@�c�@�c�@�c @�c#@�cJ@�b�@�a�@�aA@�\�@�Z/@�V�@�Q.@�MW@�I~@�C@�:@�1~@�%m@��@��@��@��@�&@��@��@�9@�:@�J@��@�@��@��@��@��@�@��@�N@�@��@��@�{@��@�(@�@�B@�	�@���@���@���@���@���@���@��c@���@���@��4@���@���@��4@���@���@��@��Y@���@���@��@��D@��
@��[@���@��@���@���@���@���@�ݮ@��d@���@��@��@��*@��@���@�ܞ@���@��y@��Z@��"@��W@��
@��@���@���@���@���@��5@��
@��@���@��)@���@���@���@���@��@��P@���@��B@��?@��$@�Ɩ@��@��o@���@���@���@���@���@���@���@���@��@���@���@�xF@�n�@�Y�@�R@�P@�c�@�z�@��(@���@���@���@��@��@��T@���@���@���@���@���@���@���@���@�@�º@���@���@���@���@�õ@���@���@��@�ö@��a@��6@��M@��a@��:@��R@���@���@���@���@�� @�� @��@��@�*@��W@���@���@���@��)@��}@��/@��&@��I@��L@��@���@��=@���@���@���@���@���@��:@��f@��w@��>@���@���@���@��&@��@���@��8@���@���@���@���@���@���@��!@���@���@���@��@��*@��@���@��@���@���@��w@��Q@��@��*@��,@���@��s@���@��@���@��P@��@��I@��@���@��?@���@���@���@��*@��@��}@��@���@���@��L@��@��@��@��@��h@��@���@��E@���@���@��L@���@��}@���@���@���@���@���@���@���@���@���@��$@���@���@��N@���@���@���@��J@��t@���@���@���@���@��@��@���@��F@���@���@��v@���@���@���@���@���@���@���@���@��G@���@��@���@��@��@��@��@���@��7@���@���@��L@��`@���@���@��[@��[@���@��\@��3@��oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     44443444334434443444444434434334444434443444434444344444344444343444444444444444434444433444434443434433444344334344444444444443443344334443443344344444334433334433344433344444333344344444433443433433443333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333343333333333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9)��9)�99)��9)��9)��9)��9)��9)��9)��9)�b9)��9)�9)��9)��9)�19)�9)�9)�9)�"9)�9)�9)�89)��9)��9)��9)��9)��9)��9)�,9)��9)�9)��9)� 9)��9)��9)�x9)�(9)��9)�h9)��9)��9)�G9)��9)�~9)��9)��9)��9)�h9)��9)��9)��9)�	9)��9)��9)��9)��9)�+9)�9)��9)�U9)�b9)��9)�A9)��9)��9)��9)�V9)�9)��9)��9)��9)��9)�G9)��9)�9)��9)�9)�~9)�9)�C9)��9)��9)��9)��9)��9)�*9)�<9)�>9)��9)�9)�>9)��9)�|9)��9)�J9)��9)�
9)��9)��9)��9)��9)�!9)�P9)�<9)�9)��9)��9)�39)�9)��9)��9)�S9)�w9)�:9)�V9)�W9)��9)�P9)��9)��9)�t9)�u9)�R9)�!9)��9)��9)�w9)��9)��9)��9)��9)��9)�^9)��9)�9)��9)�K9)��9)��9)�|9)�<9)�/9)��9)��9)��9)��9)��9)��9)�!9)�9)��9)�9)��9)�9)��9)�R9)��9)��9)�9)��9)�9)��9)��9)�.9)�9)�o9)�-9)�@9)�G9)��9)�9)��9)��9)�?9)��9)�9)��9)�99)�79)��9)��9)�K9)�9)��9)��9)��9)��9)��9)�89)�(9)��9)��9)��9)�.9)�h9)�T9)�)9)�y9)o}9)o�9)m9)n�9)g�9)g�9)g�9)f@9)f�9)f�9)fk9)e�9)a�9)`�9)a9)N�9)Q�9)P9)PW9)R9)R%9)P�9)E#9)D{9)D=9)C�9)A�9)?Q9);�9);~9);�9):�9):�9)9�9)6�9).e9)#L9)h9(�z9(��9(�N9(߂9(�99(��9(��9(�Q9(��9(�G9(��9(��9(�19({�9(r�9(]�9(Z"9(Y9(X�9(X�9(Xl9(XA9(X9(W�9(W9(V9(Rv9(P�9(K�9(L�9(M�9(MK9(M$9(L�9(Lc9(J�9(I�9(H9(FX9(D(9(@�9(=E9(=H9(?g9(Dv9(KV9(ML9(OX9(O�9(O�9(Q�9(R19(Uz9(V�9(]9(g�9(k=9(n�9(z9(��9(�9(��9(��9(��9(��9(��9(��9(� 9(��9(�G9(��9(��9(�g9(}�9(|9(~�9(xd9(v�9(t�9(s:9(rD9(q�9(p�9(qY9(q39(qp9(q9(n�9(n�9(n�9(m�9(m�9(m19(j�9(jV9(h�9(g�9(b�9(]9(W/9(S�9(S�9(S/9(T9(S�9(Rs9(R89(R9(R^9(Ss9(Sn9(SZ9(Sm9(S�9(T�9(S�9(Q�9(L�9(I�9(Hz9(I69(J09(Jm9(J�9(L?9(Q�9(S�9(X�9(\�9(d/9(k�9(p79(p�9({S9(�9(�9(�i9(��9(��9(�n9(�j9(�9(�<9(}�9(|b9(}J9(|�9(z�9(|�9(|P9({9({9(y9(xL9(v�9(sk9(s9(r�9(r�9(r9(q9(p�9(p�9(o�9(n�9(m9(l�9(k�9(j�9(h�9(g89(f�9(f&9(dp9(c9(b9(a?9(`�9(^�9(]P9(\�9(] 9(]�9(^L9(^�9(^�9(]9(Z�9(X9(U�9(Tl9(Sn9(S9(Qt9(N�9(L�9(K9(I�9(G9(EH9(B�9(?d9(5�9(,�9((U9($X9(!�9(�9(�9(�9(w9(;9(79(09(K9(c9(l9(�9(�9(�9(�9(�9(9(�9(�9(�9(�9(�9(�9(�9(�9(�9(�9(�9( *9(�9(�9(!9("49("�9($9($9(#�9($T9(#�9(!;9( e9(.9(�9(�9(�9(9(�9(#9(P9(�9(X9( 9(�9(B9(�9(�9(�9(V9(�9( 9(�9'�9'�9'ߐ9'�I9'ӑ9'��9'��9'��9'��9'��9'��9'�A9'�u9'��9'��9'�9'�z9'�w9'��9'��9'�99'~Z9'9'{�9'|�9'ts9'q9'k59'l9'g�9'`9'`h9'_�9'[�9'\(9'Q�9'X9'V�9'S�9'Mm9'Mm9'O9'OT9'O�9'O�9'M	9'K�9'J�9'J�9'J�9'Gx9'G�9'F�9'C9'6�9')"9'�9'�9'�9'�9'�9&�#9&ݨ9&ʟ9&ȸ9&�9&�q9&�]9&�#9&s^9&]�9&RP9&K�9&:�9&-K9& v9&9&P9&
L9%�*9%��9%�L9%�-9%�l9%�l9%��9%��9%��9%��9%��9%��9%��9%�Q9%�_9%�9%�89%�(9%��9%�9%�.9%�09%��9%{9%t<9%j�9%`�9%[9%V�9%P�9%L�9%K�9%JI9%I�9%I*9%H9%G�9%G29%F�9%E'9%D9%B�9%A�9%?�9%< 9%;�9%<�9%<�9%;�9%=�9%<�9%@R9%>�9%@9%>9%?�9%?9%@�9%?l9%>�9%=�9%=M9%=K9%<�9%=�9%=�9%=�9%?+9%C�9%LC9%K�9%H?9%*�9%�9%�9%�9$��9$��9$�b9$��9$��9$�69$�9$ʔ9$ȯ9$Ǣ9$��9$�9$��9$�09$��9$��9$�9$�o9$�\9$�"9$��9$��9$�M9$��9$w�9$nW9$e�9$[I9$S9$O9$K�9$NH9$M�9$M�9$J39$F�9$K�9$L�9$L�9$K�9$K�9$J�9$V�9$Y9$W�9$R9$Np9$L�9$I�9$G�9$GU9$Et9$Dc9$Ce9$??9$=�9$:B9$4�9$&N9$�9$�9$b9$<9$�9$59$�9$�9$�9$�9$�9$�9$H9$9$F9$F9$9$�9$�9$u9$x9$�9$�9$(9$�9$P9$�9$	39$�9#��9#��9#��9#�9#��9#��9#�
9#�&9#�09#�99#¢9#�'9#�&9#µ9#¶9#��9#�9#Ê9#�9#�9#�r9#�k9#Ė9#�9#��9#É9#�\9#�r9#��9#�c9#��9#��9#��9#�9#�9#�U9#�[9#�w9#p49#Yw9#T9#Np9#KD9#J�9#K=9#KA9#K�9#L�9#Lz9#L�9#M9#ML9#N�9#M�9#O�9#O�9#P9#P�9#Q�9#Q�9#SX9#Yb9#ke9#�B9#��9#��9#��9#�29#��9#�9#��9#�39#�Y9#�9#{�9#u�9#n�9#l�9#i�9#i�9#jx9#p\9#nC9#m�9#m�9#l�9#jd9#j�9#kM9#k�9#f�9#a99#_�9#\�9#Zx9#_�9#e�9#u�9#y59#x�9#o9#nf9#hI9#h�9#fg9#c]9#cs9#e�9#d,9#^�9#@x9#?�9#+
9#!b9#�9#�9#�9#�9#-9#6�9#B�9#Td9#a<9#c�9#g�9#i�9#mY9#nz9#o99#p�9#q09#q�9#s�9#t�9#u39#u[9#up9#vf9#v�9#vf9#vU9#v9#v�9#v�9#vV9#v9#u�9#u�9#v9#t�9#s�9#s99#s�9#t�9#v|9#��9#��9#�-9#�99#��9#��9#�{9#�9#�@9#��9#�9#��9#��9#��9#��9#��9#��9#��9#�K9#�w9#�T9#��9#�^9#��9#��9#�9#��9#��9#�V9qt9m�9j�9j9h�9h9g#9f9fO9f9d]9d�9c49c 9c#9b�9b�9a�9`^9`�9_�9_#9_9^�9^�9]�9]�9]@9]9\�9\�9[E9Y�9X�9V�9T�9T*9S�9S�9Sp9Sp9S�9S�9P9O�9OY9Op9P�9Q�9Q�9Q�9R�9S�9T�9Tg9S�9S9Qy9P�9P}9P9P�9Pk9Pi9P)9P)9P=9PO9P9P�9P�9Q%9Q%9Q�9RL9RH9RH9Q�9R
9R 9R 9RK9RL9R�9R�9R�9R�9R�9R[9R9RL9Rq9R�9R�9R/9R�9RH9RH9R�9S�9S�9SX9S�9S�9S�9S�9T�9U�9VK9V"9V�9V�9W39W39V�9V�9W�9W�9W�9XG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bm�Bm�Bm�Bl�Bm�Bm�Bl�Bl�Bl�Bl�Bk�Bk�BjB/B~�B��B��B�B��B��B��B��BB1BhB�B"�B'�B�BJB�B�TB�B��B  B��B\B�B�B�B�B�B,BO�B`BBjBt�B�VB�{B�^B�}BÖBÖBǮB��B��B��B�B�B�#B�BB�BB�NB�NB�NB�ZB�`B�fB�ZB�/B��B��B��B��B��B��BŢBBÖBȴG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@t�>��?@�AHW�B��@���?>�HA�nB��B�?/�HA��B�g?��? �*?,�A��@��	?��?tk8?�A�@[D�?!�"@�#�A�T�?�A@Q]B�O?NͯBV�BH�>��g@��I@�7�?0�VA>�<B�4A��k?}�@� B7F>�_[>��I>�)�?6ТB;�@_S�A^�E@J�?�I�Bl'?�;�>��%>��?X�?A�1A���AV�(?FO?���@ck?[DB�\?+�A��?+?@ݨ�>�Z�>��4>��D>�D?��?�H>��T>ǇH>�U @���?#
�?�{[>���? �
A��>�y�?#� ?�Ɔ?�2?I6PB�]B�}?�-�?(q�?���?�d(B7�?��}A`D�?7�/B3�?y�*B��?+0�?e�ZB��Bح@J?0�VAAEB��>�?9�?A��eB��?@��Bk�>�*�>�(>��@ŗ@��`>ʥa>۟4?��A$.�@��(AUF?%�@2�B>��"?!w�A�K&Bǣ@��%?�I�B��B�A>��?J��AKR�BMD?��@��B��B��A4�?��QB��@���?Wn�@�f`??�)�B��B��?��?;CtB��BËBĦBQF?<��A��BB��A�H�?��A$��@�ҦB��B��B��?A1AZ��>��@�_]?/j�B��B��B�TB/H?8.?`h#B�?!�@�ZyAX�?�̖A�@��bB�	B��A��#A4t�B��@��zA�ʃA���@qB�_A�[3@�k�?c��B��BB rA�-iB��B��@���B��BG�B�B�3B�gA��B��B�?BĆB�`B�~BóBćB��BƞB�B͡BƝBȗB��B��BB�4B��B�B�9BōB�QB��BB�&B�&B�B�pB��B��B�!B��B��B�[BūB�lB��BŃB�aBķBB�-B��B�aB�aB�@BûB�B�&B�bB��B��B��B��B�zB�B�dB�B¬B�4BÁB�!B�OBèB�BèB�[BàB�tB�AB�*B�[B��BĉBúB�xB¬B�BÃB��B�B�xB�B�5B��BýB��B��B�rBÕB��B�B�BéBÍBÍB�[B�;B�'A�s'B�B�-B��B��B�dB�fB�CBɀBĮB��B�BÕB��B�
B�B��B �B��B�~B�B�
B�JB�EB�XB�JBĈB� B��B��B��B��B�LB�B�4B��B��A��tB��B�&B�mB�JB�0BƧB�iB�XB�{B�B�@BʫB��BèB�$B�B�NBĦB�B��B��B�@B��B�B�tBǆB�4B�LB�mB�vBÍB�BŴB�OB�8B��A��0B�B�B�:B��B�BܝA�y<B��B��BĹBĜB�B��B��B��B��B��BŶBƺB�tB��B�B�aB�YB��B�IBɈB�/B��B��B��B�IB�B��B�B�B��B�GBŴB�fBįB�fB�xBóB�WB�B�#B��BŃBĥB�B�
B�eB�B��B��B�sB��BȋB��BĸB�DBßB�B�>B�>B�[BèB�|B��B�oB��B�BðB«B�xB�BĤB��B�B�GB��B�EB�-A��B�]B�TB�oB��B�B�6B��B�BÙB��B��BÑBÉB��B�QB��B�,B��B�B�#BĞB�WB��B�hB�vBĘB�B��B�	BüBüB��B�]B��B�)B�!B�BíB�BĿB�TBęB�}B�\BđB�>B�cB��BÏBÆB��B��B�BĤB�oB�(B��B��B��B�gB�B�tB�{B�B�8B�0BÒB�iB��B�mBþB�;B�BÉBÊB��B��B�gB�+BäBÜB��B��B��BþB��B�B��B�B��BÌB�SB��B�B�PB�cB��BĀB�B��B��B�jB�aBþB�hB�:B�1B«B�nB�B�{B�!B��B��B�QBêB� B��BčB�B�PB��BõB�.B��B��BĐBüB�B�B�B�QB��B�#B�B�B�
B�SBØBÐBÈB��BÊBóBêB��B��B�eB��BÖB�YB�$B�B��B�3B�B�YB�dB�SBïB��B��BĔBãB�MB��B�4B�?B�kB�.B��B��B�nB��B�vB�9B�B¢B�CBöBB��B��BB��B��B��B��B£B��B�BB�{B�BB�B��B�B�^B��B�uB�B�nB�BB��B�/BB��B��B�B��B��B��B��B��B�B��B±B��B�AB�DB�B��B��B¼B�nB�B��B�rB��B��B�5B�PBªB�B� B�}BĳB��B�(B�(B�MB�7B��B�YB��B��B�B�B�B��B�aB�QB��B��B�KB�9B�MB��B�LB�;B��B�mB��B� B�B��B�@B��B�cB�B�B�qB�bB��B��B��B��B��B��B��B�B�9B� B��B��B��B�B�ZB��B�EB��B�BҢBҢB�sB�pB��B� B�}B�'B�B�B�\B�KB��B��B��B��B�B��B��B�bB�oB��B��B�0B��B�B�oB��B�B�sB��B�B^B	B�BsB�BbBVB/B�B�B�BQBgBlB�B!�B#�B"~B!�B!�B!�B �B�B B! B!B \B #BkB�B�B�B"BGB}BB �B!WB �B!�B"�B"B"HB!�B!B!�B!B!)B pB B�B�B�B�BwBBxB�B!�B"CB#B!mB![B!�B"B"�B"�B"�B"B";B"�B"B""B"hB!~B"�B$�B%�B&LB(_B*�B,�B0�B4�B6�B:dB=BB�BC�BE�BG�BK,BJ�BM]BN�BO�BP^BP�BPBO�BO�BO�BP�BQBQ�BR�BR�BS�BSBS�BT�BT�BVCBW�BXbBX�BX BXBX�BY�BX�BY BYFBY�BY�BX�BY~BZ0BY�BZ BZ�BZ�B[�BZB[�B[�B[�B\�B]�B]�B]�B]B\HB[^BZ�B['B\YB]�B]�B^B]�B[QB]�B^fB^JB]�B^DB]�B\�B[�B^lBdYBg�Bl�Bn�BoBqBq~BrBr�Bs�Bt�BuTBv�ByCBx�Bz By)By�ByzByrBz0BzvBzYBy�BzIBzTBzKBzCB{	Bz2Bz�By�Bz�Bz�By�Bz0Bz�BzGBzpBy�Bx�BxXBx�BzQBzBz-BysBx�By�By)By]BzTByLByBxBvQBv�Bu�Bw�B{�BB��B�iB~=BtUB~�B�B}�ByBs�Bs�Bs�Bn�BpyBt�Bn�Bl{Bj]BghBf�Bf_Bf�BbBfmBfnBh Bf�BbBa Bb�Ba6Ba�B]�Ba�B`�B`.B^B^�BZ�B`�B_B[XBT�BZ�BY:BY�BT�BS�BT
BP0BPBPBKBH8BG�BD�B;�B7B6^B*}B6�B-RB)vBOBaB�BB �BB�B�BB	uBCBIB�B3BmB�B�B�B�BMB�B�B+�B7CB&bB5�B-�B34B)�B1�B2�B(�B&uB&#B,B1AB3�B3B2/B1#B1�B.iB0B0�B1pB2/B6B9�B;B?{BB�BCHBDABE/BD�BD�BD�BE<BD�BE2BD�BF�BEmBEDBIlBL:BKLBKBJ~BJbBK*BJ�BJ\BInBI�BJ_BI�BH�BG�BGFBH?BH�BHBG�BH@BGHBG�BH�BKFBK BE�B@�B=�BCLBB�BA�BD�BE�BC�BBqBB�BBKBDBH�BH�BJ�BL�BH�BP6BL�BJBF,BJeBJwBIjBF�BG�BE�BCqBB�BE�BPEBM�BP�BU-BW�BZ�BZ�BZ�BY�BY�B\�BavBb?Bc�Bd�BhBk�Bn$Bk�BisBk�BkEBlBk�BlXBl�Bk�BlLBj�BiBh�Bf�Be<BbQBkBo$Bp�Br�BsBs Br%BqJBqABqUBqMBp!BocBp-Bn�Bo*Bn/Bl�Bk�Bj�Bj�Bh�Bh�Bf�BesBc=Bd�Ba^BbSBa!B^�B\�B]�B\�B^�BeUBiBk Bk�Bl_Bl�BlNBlBl�Bk�Bk�Bj^Bj�BjBioBiBg�Bg�BfjBfVBduBb�Ba�B^�B^B[�BW�BUBX�BTBT�BZ+B]�BjBk�BpNBq�Bq�Bs�Bs�BtBt�Bu�Bu�Bv�Bw`Bx-Bw�BysB{'B{�B|pB~�B�kB�B��B�B�gB��B��B��B�oB�@B�DB�WB�<B��B�B�B��B�B�#B��B��B��B�B��B�{B�bB��B��B��B�=B�^B��B��B�B�nB��B��BBŞBB��B��B�yB��B��B�eB�LB�B��B��B��B� B�B�B��B��B�?B��BΏB�_B׊B��B��B�B�tB�B�B�wB�3B�^B�B�*B�B�B�4B��B�B�iB�B�B�B�B�B�%B�jB�B�#B�lB�B�B�mB��B�B�rB�+B�B�B�B�B�B�BBuB�B-ByBwBBABB�B�B1B	B�B�B�BB�B6B%B1B�B�B,B�B�BOB{B�B�BB:B�BPB�BEBB�B�BrBpB�B�BpB{B�B,B�BsB�BB_B�BBsB�BzB�B�BBBBxBoBBTB8B�B�BB <B!,B"�B"B"�B#B#�B#�B#�B#nB"�B$�B&B&�B'1B(\B(SB(�B*B,9B,B,�B.EB0�B1}B4B4vB4B67B6�B7JB7kB9�B9�B:�B;�B==B>�B?B?�B@~BA%BAxBA�BA�BB�BB�BC�BCBE}BE}BEKBEiBE"BE"BE�BGDBC'B>�B@�B@�B@�B@�B=�B<�B:LB:YB<~B=qB=�B@B@BA�BB"B@�B?NBmBl�Bn$BmRBmUBn*Bm�BmBmPBl�Bm^Bm�Bm�BlpBm�Bm�Bl�Bl�Bm�BmNBnLBl�BmZBnBmABm9Bm0Bm�Bm�BmxBl�Bm.Bl�Bl�Bm?Bm�Bm�BmBl�Bm�Bl�Bm�Bl�Bl�BmwBmBl�BmOBm�BmnBl�BmBl�Bl�Bl�Bl�BmHBmBl�BmuBl�Bm�Bm*BmoBm_Bm�BmBm}BmFBm�BlkBm-BnDBm�BlqBl�BlBl�Bm%BmVBm�Bl�Bl�Bm�Bl�Bm9BlzBm;Bm�Bm�Bm#BmhBmLBm~Bm3Bm�Bl�Bl�Bl�BlrBmWBl�Bl�BmyBm6Bl"Bm�Bl�Bl�Bk�BmBm9BmXBlCBl�Bl�BlbBl-Bl�Bl�BlZBmBlBl
Bk�BmBl�BlABm!BmBl�Bm'Bl�BmBmBl)BlGBl�Bl�Bl7Bl�Bm�BlBl'Bl*Bl�Bk�Bk�BlBlBl�Bk�BlBl�Bk�BmOBl	Bl�Bk`BllBl8Bl�Bj�Bk�Bl�Bl�Bk>BkIBk�Bk�BlBkDBkBk�Bk�Bk9BloBl�Bk$Bl'Bk�Bl�Bl*Bl�Bj"Bk�Bj�Bk�Bj�BknBi�Bj�BjHBkBj�BmoBh�Bw�Bi�BlbBiBk�Bj<Bi�BjSBi�BjJBi�Bh�Bi�BjBh>Bl�Bh�Bi2Bi`Bi�Bh�Bh�Bj�Bh�Bh�BhkBh�Bg�Bf�Bf�BfvBfnBf�Be�BeEBe.Be�Bh:Bf�BjYBj�BkyBolBz"Bx�B|�B~�B}�B�CB�gB��B�-B��B��B��B�
B�eB��B��B��B�CB� B��B��B�lB�B��B�0B�FB��B��B�`B�B�jB��B�BB�6B�*B��B�xB�UB��B�B��B�FB�GB��B��B�	B�sB��B��B�kB�7B��B�hB�8B�B�:B�UB�XB�B� B̾B��B̃B̪B�oB̖B��B�B�SB˄B�0B�B�B˿B�DB˼B�+B�zB�nB��B��B��B�tB;B͏BͮB̔B̲BͥB��B�B�fB�RB��B�"B�_B�dB�)BʋB�FB��B�YB�JB�.B�,B�VB�NB�2B�=B˗B�+B��B�B��B��B�RB��B̨B�=B�IBѧB�,B��B��B��B�B�B�JB��B��B��B��B��B��B�B�dB�\B��B�LB�}B��B��B��B��B�=B��B��B��B *B =B�B�B8B�B�B�B�BbB�B�B�B�B�B�BxB�B;B�B�B|B;B�BdBxB�B�B�B	B�B�B�B	�B
DB
�B�BByB}B�B�B�B�BBGBDB%BB�B�BmB�B`B"B�BaB�B�B�BB�B�B|B@B �B ^B .B!B!%B nB!�B!�B!�B �B!�B!�B!�B"�B"�B!�B"oB"�B"_B!�B"�B"�B#JB#�B$sB#cB#�B$�B$	B$�B$sB%�B%~B%�B%�B'B'"B'�B(B'�B(5B'�B(@B'�B(pB(pB(�B(�B(�B(dB)�B&�B&~B%;B$�B$B"�B"�B!�B"B!EB �B!{B B�B WB�BJB6BnBTB B�B<B�B�B�B�B&B�BJB�B�B�B�B�B:BBB]B�B	BwB�BoB�B7B&B
�BEB
|B
�B	BB�B�B��B�zB��B��B��B�7B�nB�JB�B�kB�uB��B��B��B��B��B�'B�#B�B�xB�B�sB�IB��B�XB�B��B߳B��B�B�/B�?B߼B��B޵B�#B��B�B�B�B�B�B�eB�RB�[B��B��B�B� B�B�B�|B��B��B�-B�B�B�B��B�NB��B�B��B�>B�B�B��B�B��B��B��B��B��B�B��B��B�.B��B��B�B��B�2B�EB��B��B�MB��B�[B�qB��B��B��B�B	�B�BBB/B�B�B��B��B��B��B��B��B�(B��B��B��B�0B��B�nB��B�CB��B��B��B��B�nB�/B��B�rB��B�B�BB�BB�BB~B	B	B�B
�BB�B�B;B9B�BB�BpB BjB�BHBsBlB�B�B�BB�B�B&B�BB~B�B B�B>BqB,B�B/B'B�B�B�B�BLBB�B�B�B�B�B�B8B�B�B�B�BBdB�B�B<B
BB`BB�B�BEB=B�B�B�BB�BB�B[B#BrB�BhB"B�B�BsB�B�B�BrB�BfB	B�BwBTB�BmB�B�B�B�B�BFBB�B�B5B�B�B/B?B�BqB�B�B�B�BUB'�B;�BGeBQ�BS"BRUBP�BP<BN<BO�BN:BL�BK�BK=BLBN�BN7BNBPuBW�BXBV�BWtBX0BXQBYvB\[B_B_	B]�B^�BaBaBc�BfPBl�BoBr�Bo�Bo�Bm>BnCBm�BluBmRBl�BlKBm�BcBd�B`CB^;B[MB[3BY'Bc.Bm�Bq Bt5Bz�B�0B��B�B��B�!B�4B�B�BB��B�B��B�RB��B�B�B�	B�BB��B��B��B��B�B��B�B��B��B��B�iB�TB�^B��B��B��B�vB�ZB�CB�8B��B��B�B��B�jB��B��B�B�8B��B��B�vB�YB�B��B� B��B�PB�B��B��B��B�IB��B��B��B�B�BüBêB��B��B�RBÈB�7B�PB�gB¾B�nBB��B��B�wB��B�cB��B�iB�KB�BBB�~B��B�~B��BñB�1B�jB��BðB�"BB�BÜB�iB�aBêB�NB��B�BƬBƷB�WB��B� B��B��B�@B��BʟB˱B��B��B�5B��B�3B�}B�SB�JB��B��B��B��B̻B�/B�/B��BΎB�sB��B�%B�%B̽BͽB�B��B�B��B��B̮B�1B΄B�1B��BΊB��B�FB̄BˮB�B�[B�yB�%B��B�BɊB�7B�B�7B�+B��BɢB�`B�BɒBȡB��B�QB�'B�B�B��BŜB�HB��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111141114111111111111111111111111111111111111111111111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999944443444334434443444444434434334444434443444434444344444344444343444444444444444434444433444434443434433444344334344444444444443443344334443443344344444334433334433344433344444333344344444433443433433443333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333343333333333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 Bm�Bm�Bm�Bl�Bm�Bm�Bl�Bl�Bl�Bl�Bk�Bk�Bj�G�O�BB��B�G�O�B��B��B�B��BB5BmB�B"�B'�B�BMB�B�[B�B��B B��BaB�B�B�B�B�B,BO�B`JBj�Bt�B�`B�B�dB��BÝBÜBǶB��B��B��B�B�B�)B�NB�HB�VB�XB�VB�`B�iB�jB�_B�3B��B��B��B�B��B��BŨBBÜBȿG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B� G�O�G�O�G�O�B��B�G�O�G�O�B�nG�O�G�O�G�O�A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�T�G�O�G�O�B�SG�O�BV�BH�G�O�G�O�G�O�G�O�G�O�B�8G�O�G�O�G�O�B7LG�O�G�O�G�O�G�O�B;�G�O�G�O�G�O�G�O�Bl,G�O�G�O�G�O�G�O�G�O�A���G�O�G�O�G�O�G�O�G�O�B�eG�O�A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�G�O�G�O�G�O�G�O�G�O�B�fBʄG�O�G�O�G�O�G�O�B7�G�O�G�O�G�O�B3�G�O�B��G�O�G�O�B��BذG�O�G�O�G�O�B��G�O�G�O�A��pB��G�O�Bk�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BG�O�G�O�A�K1BǨG�O�G�O�B��B�JG�O�G�O�G�O�BMKG�O�G�O�B��B��G�O�G�O�B��G�O�G�O�G�O�G�O�G�O�B��B��G�O�G�O�B��BÐBĪBQKG�O�G�O�BB��A�H�G�O�G�O�G�O�B��B��B��G�O�G�O�G�O�G�O�G�O�B��B�B�XB/NG�O�G�O�B�G�O�G�O�G�O�G�O�G�O�G�O�B�B��G�O�G�O�B��G�O�A�ʋA���G�O�B�cA�[:G�O�G�O�B��BB yA�-nB��B��G�O�B��BHB�#B�5B�nA��B��B�EBČB�eBĄBøBČB��BƤB�!BͥBƤBȟB��B��BB�8B��B�B�>BŏB�SB��BB�)B�)B�!B�rB��B��B�&B��B��B�aBůB�tB��BņB�hBĽB¡B�2B��B�hB�hB�CBÿB�B�)B�gB��B��B��B��B�~B�B�lB�!B²B�8BÇB�(B�YBìB�	BìB�aBæB�yB�EB�0B�aB��BĒBÿB�zB²B�BÉB��B�B�zB�B�=B� B��B��B��B�wB×B��B�B� BîBÓBÓB�aB�BB�,A�s1B�B�2B��B��B�lB�lB�KBɈBĶB��B�B×B��B�B�B��B �B��BB�B�B�OB�NB�^B�OBČB�B��B��B��B��B�PB�	B�;B�B��G�O�B��B�-B�rB�OB�5BƬB�oB�^B̀B� B�EBʴB��BìB�+B�B�VBĪB�B��B��B�CB��B�B�yBǉB�8B�WB�tB�zBÓB�BŻB�YB�?B��G�O�B�BĈB�?B��B�BܣA�yHB�B��B��BĨB�B� B��B��B��B��BŻB��B�yB��B�B�hB�`B�B�QBɎB�3B��B��B��B�OB�B��B�B�B��B�PBŻB�jBĶB�nB�zBøB�[B�B�(B��BņBĪB�B�B�kB�B��B��B�yB��BȐB��B��B�HBæB�B�CB�CB�dBìBĀB��B�rB� B�BöB¯BÀB�BĪB��BÅB�OB��B�KB�4A��B�cB�YB�sB��B�&B�;B��B�BàB��B��BØBÎB��B�WB��B�3B��B�	B�)BĥB�\B��B�lB�{BġBÇB��B�B��B��B��B�dB��B�0B�&B�BöB�B��B�[BġBĂB�^BĔB�CB�jB��BÔBm�Bl�Bn(BmUBm\Bn.Bm�BmBmVBl�BmdBm�Bm�BlvBm�Bm�Bl�Bl�Bm�BmUBnQBl�BmaBnBmIBm>Bm7Bm�Bm�Bm{Bl�Bm6Bl�Bl�BmGBm�Bm�Bm!Bl�Bm�Bl�Bm�Bl�Bl�Bm|BmBl�BmSBm�BmtBl�BmBl�Bl�Bl�Bl�BmLBmBl�Bm{Bl�Bm�Bm0BmtBmeBm�BmBm�BmJBm�BlqBm3BnIBm�BluBl�Bl�Bl�Bm,Bm^Bm�Bl�Bl�Bm�Bl�Bm>BlBmABm�Bm�Bm*BmnBmRBm�Bm8Bm�Bl�Bl�Bl�BlxBm^Bl�Bl�Bm|Bm=Bl&Bm�BmBl�Bk�Bm%BmABmZBlFBmBl�BlgBl2Bl�Bl�Bl_BmBl%BlBk�BmBl�BlFBm%Bm
Bl�Bm+Bl�BmBmBl0BlLBl�Bl�Bl<Bl�Bm�Bl#Bl0Bl2Bl�Bk�Bk�Bl#BlBl�Bk�BlBl�BlBmSBlBl�BkgBlqBl?Bl�Bj�Bk�Bl�Bl�BkEBkNBk�Bk�BlBkIBkBk�Bk�Bk<BlvBl�Bk+Bl,Bk�Bl�Bl2Bl�Bj&Bk�Bj�Bk�BkBkrBi�Bj�BjNBkBj�BmtBiBw�Bi�BliBiBlBj@Bi�BjZBi�BjOBi�Bi Bi�BjBhCBl�Bh�Bi9BidBi�Bh�Bh�Bj�Bh�Bh�BhqBh�Bg�Bf�Bf�Bf|BfwBf�Be�BeLBe5Be�BhABf�Bj]Bj�Bk{BosBz'Bx�B|�B~�B}�B�JB�lB��B�4B��B��B��B�B�kB��B��B��B�KB�B��B��B�tB�B��B�7B�KB��B��B�hB�$B�oB��B�GB�>B�0B��B�|B�[B��B�
B��B�LB�MB��B��B�B�wB��B��B�rB�=B��B�pB�<B�#B�@B�^B�_B�B�%B��B��B̋B̱B�wB̛B��B� B�XBˈB�9B�B�B��B�JB˿B�4B́B�tB��B��B��B�zB��B͕B͵B̙B̶BͬB�B̇B�lB�XB��B�'B�dB�fB�-BʒB�NB��B�^B�SB�4B�1B�`B�SB�8B�CB˜B�4B��B�'B��B��B�YB��B̮B�AB�OBѯB�0B��B��B��B�B�B�QB��B��B��B��B��B��B�B�jB�_B��B�OB��B��B��B��B��B�CB��B��B��B 1B CB�B�B>B�B�B�B�BgB�B�B�B�B�B�B|B�B?B�B�B�B?B�BhBB�B�B�BB�B�B�B	�B
KB
�BBBB�B�B�B�BBBNBIB*BB�B�BuB�BiB(B�BfB�B�B�BB�B�B�BAB �B fB 2B!B!+B sB!�B!�B!�B �B!�B!�B!�B"�B"�B!�B"rB"�B"iB!�B"�B"�B#UB#�B$zB#fB#�B$�B$B$�B$xB%�B%�B%�B%�B'B'*B'�B(B'�B(8B'�B(CB'�B(uB(rB(�B)B(�B(iB)�B&�B&�B%BB$�B$B"�B"�B!�B"B!NB �B!�B !B�B ]B�BOB=BtBZB)B�BAB�B�BB�B-B�BOB�B�B�B�B�B?B	BB_B�BBB�BtB�B<B-B
�BKB
�B
�B	#BB�B�B��B�}B��B��B��B�@B�rB�RB�B�qB�{B��B��B�B��B��B�+B�*B�B�B�B�yB�PB��B�]B�!B��B߹B��B�B�6B�EB��B��B޻B�+B��B�B�B�B�B��B�lB�ZB�cB�B��B�B�'B�B��B�B��B��B�2B�B�B�B��B�RB��B�B��B�DB�B�B��B�
B��B�B��B��B��B�B��B��B�2B��B��B�B��B�8B�LB��B��B�QB��B�cB�uB��B��B��B�B	�B�BBB6B�B�B��B��B��B��B��B��B�0B��B��B�B�6B��B�oB��B�HB��B�B��B��B�qB�6B��B�xB��B�B�BB�BB�BB�BBB�B
�BBB�BABAB�BB�BuB%BqB�BNBxBsBB�B�B$B�B�B.B�B'B�B�B	B�BABuB0B�B3B.B�B�B�B�BRBBB�B�B�B�BB>B�B�B�B�BBjB�B�BDBBBhBB�B BLBDB�B�B�BB�B$BBdB'ByB�BqB)B�B�BzB�B�B�BuB�BmBB�BBZB�BsB�B
B�B�B�BMBB�B�B;B�B�B7BAB�BvB�B�B�B�BYB'�B;�BGkBQ�BS)BR]BP�BPBBNABO�BN>BL�BK�BKBBLBN�BN>BNBPyBW�BX#BV�BW}BX5BXTBY|B\bB_	B_B]�B^�Ba"BaBc�BfWBl�BoBr�Bo�Bo�BmGBnKBm�Bl{BmZBl�BlNBm�BcBd�B`LB^CB[TB[:BY.Bc5Bm�Bq)Bt;Bz�B�6B��B�B��B�*B�;B�B�IB��B�	B��B�YB�B�$B�&B�B�JB��B��B��B��B�B��B�B��B��B��B�mB�WB�fB��B��B��B�}B�`B�KB�@B��B��B�B��B�pB��B��B�B�=B��B��B�|B�_B�B��B�&B��B�WB�B��B��B��B�RB��B��B��BĆB�B��BïB��B��B�XBÍB�<B�VB�mB��B�uBB��B��B�~B� B�jB��B�rB�OB�B¥BBB��BB��BúB�3B�rB��B÷B�(B£B�BáB�qB�jBïB�VB��B�&BƲB��B�\B��B�	B��B��B�EB��BʥB˹B��B�B�:B��B�:B̅B�\B�RB��B��B��B��B��B�7B�6B��BΖB�}B�B�)B�)B��B��B�#B��B�$B��B��B̴B�8BΈB�8B��BΓB��B�KB̍B˷B�B�cB�~B�)B��B�BɑB�=B�B�<B�1B��BɦB�fB�BɛBȩB��B�WB�0B�!B�!B��BŤB�PB��Bm�Bl�Bn(BmUBm\Bn.Bm�BmBmVBl�BmdBm�Bm�BlvBm�Bm�Bl�Bl�Bm�BmUBnQBl�BmaBnBmIBm>Bm7Bm�Bm�Bm{Bl�Bm6Bl�Bl�BmGBm�Bm�Bm!Bl�Bm�Bl�Bm�Bl�Bl�Bm|BmBl�BmSBm�BmtBl�BmBl�Bl�Bl�Bl�BmLBmBl�Bm{Bl�Bm�Bm0BmtBmeBm�BmBm�BmJBm�BlqBm3BnIBm�BluBl�Bl�Bl�Bm,Bm^Bm�Bl�Bl�Bm�Bl�Bm>BlBmABm�Bm�Bm*BmnBmRBm�Bm8Bm�Bl�Bl�Bl�BlxBm^Bl�Bl�Bm|Bm=Bl&Bm�BmBl�Bk�Bm%BmABmZBlFBmBl�BlgBl2Bl�Bl�Bl_BmBl%BlBk�BmBl�BlFBm%Bm
Bl�Bm+Bl�BmBmBl0BlLBl�Bl�Bl<Bl�Bm�Bl#Bl0Bl2Bl�Bk�Bk�Bl#BlBl�Bk�BlBl�BlBmSBlBl�BkgBlqBl?Bl�Bj�Bk�Bl�Bl�BkEBkNBk�Bk�BlBkIBkBk�Bk�Bk<BlvBl�Bk+Bl,Bk�Bl�Bl2Bl�Bj&Bk�Bj�Bk�BkBkrBi�Bj�BjNBkBj�BmtBiBw�Bi�BliBiBlBj@Bi�BjZBi�BjOBi�Bi Bi�BjBhCBl�Bh�Bi9BidBi�Bh�Bh�Bj�Bh�Bh�BhqBh�Bg�Bf�Bf�Bf|BfwBf�Be�BeLBe5Be�BhABf�Bj]Bj�Bk{BosBz'Bx�B|�B~�B}�B�JB�lB��B�4B��B��B��B�B�kB��B��B��B�KB�B��B��B�tB�B��B�7B�KB��B��B�hB�$B�oB��B�GB�>B�0B��B�|B�[B��B�
B��B�LB�MB��B��B�B�wB��B��B�rB�=B��B�pB�<B�#B�@B�^B�_B�B�%B��B��B̋B̱B�wB̛B��B� B�XBˈB�9B�B�B��B�JB˿B�4B́B�tB��B��B��B�zB��B͕B͵B̙B̶BͬB�B̇B�lB�XB��B�'B�dB�fB�-BʒB�NB��B�^B�SB�4B�1B�`B�SB�8B�CB˜B�4B��B�'B��B��B�YB��B̮B�AB�OBѯB�0B��B��B��B�B�B�QB��B��B��B��B��B��B�B�jB�_B��B�OB��B��B��B��B��B�CB��B��B��B 1B CB�B�B>B�B�B�B�BgB�B�B�B�B�B�B|B�B?B�B�B�B?B�BhBB�B�B�BB�B�B�B	�B
KB
�BBBB�B�B�B�BBBNBIB*BB�B�BuB�BiB(B�BfB�B�B�BB�B�B�BAB �B fB 2B!B!+B sB!�B!�B!�B �B!�B!�B!�B"�B"�B!�B"rB"�B"iB!�B"�B"�B#UB#�B$zB#fB#�B$�B$B$�B$xB%�B%�B%�B%�B'B'*B'�B(B'�B(8B'�B(CB'�B(uB(rB(�B)B(�B(iB)�B&�B&�B%BB$�B$B"�B"�B!�B"B!NB �B!�B !B�B ]B�BOB=BtBZB)B�BAB�B�BB�B-B�BOB�B�B�B�B�B?B	BB_B�BBB�BtB�B<B-B
�BKB
�B
�B	#BB�B�B��B�}B��B��B��B�@B�rB�RB�B�qB�{B��B��B�B��B��B�+B�*B�B�B�B�yB�PB��B�]B�!B��B߹B��B�B�6B�EB��B��B޻B�+B��B�B�B�B�B��B�lB�ZB�cB�B��B�B�'B�B��B�B��B��B�2B�B�B�B��B�RB��B�B��B�DB�B�B��B�
B��B�B��B��B��B�B��B��B�2B��B��B�B��B�8B�LB��B��B�QB��B�cB�uB��B��B��B�B	�B�BBB6B�B�B��B��B��B��B��B��B�0B��B��B�B�6B��B�oB��B�HB��B�B��B��B�qB�6B��B�xB��B�B�BB�BB�BB�BBB�B
�BBB�BABAB�BB�BuB%BqB�BNBxBsBB�B�B$B�B�B.B�B'B�B�B	B�BABuB0B�B3B.B�B�B�B�BRBBB�B�B�B�BB>B�B�B�B�BBjB�B�BDBBBhBB�B BLBDB�B�B�BB�B$BBdB'ByB�BqB)B�B�BzB�B�B�BuB�BmBB�BBZB�BsB�B
B�B�B�BMBB�B�B;B�B�B7BAB�BvB�B�B�B�BYB'�B;�BGkBQ�BS)BR]BP�BPBBNABO�BN>BL�BK�BKBBLBN�BN>BNBPyBW�BX#BV�BW}BX5BXTBY|B\bB_	B_B]�B^�Ba"BaBc�BfWBl�BoBr�Bo�Bo�BmGBnKBm�Bl{BmZBl�BlNBm�BcBd�B`LB^CB[TB[:BY.Bc5Bm�Bq)Bt;Bz�B�6B��B�B��B�*B�;B�B�IB��B�	B��B�YB�B�$B�&B�B�JB��B��B��B��B�B��B�B��B��B��B�mB�WB�fB��B��B��B�}B�`B�KB�@B��B��B�B��B�pB��B��B�B�=B��B��B�|B�_B�B��B�&B��B�WB�B��B��B��B�RB��B��B��BĆB�B��BïB��B��B�XBÍB�<B�VB�mB��B�uBB��B��B�~B� B�jB��B�rB�OB�B¥BBB��BB��BúB�3B�rB��B÷B�(B£B�BáB�qB�jBïB�VB��B�&BƲB��B�\B��B�	B��B��B�EB��BʥB˹B��B�B�:B��B�:B̅B�\B�RB��B��B��B��B��B�7B�6B��BΖB�}B�B�)B�)B��B��B�#B��B�$B��B��B̴B�8BΈB�8B��BΓB��B�KB̍B˷B�B�cB�~B�)B��B�BɑB�=B�B�<B�1B��BɦB�fB�BɛBȩB��B�WB�0B�!B�!B��BŤB�PB��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111141114111111111111111111111111111111111111111111111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999944443444334434443444444434434334444434443444434444344444344444343444444444444444434444433444434443434433444344334344444444444443443344334443443344344444334433334433344433344444333344344444433443433433443333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333343333333333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202009011537212020090115372120200901153721202009011537212020090115372120200901153721202009011537212020090115372120200901153721202009011537212020090115372120200901153721AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201811202121432018112021214320181120212143    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201811202121432018112021214320181120212143  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201811202121432018112021214320181120212143  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202009011537212020090115372120200901153721  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                