CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  d   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:57Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  7d   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8D   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  8�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  9    WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    9`   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           9h   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9x   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            9|   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           9�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           9�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ;�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z           ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  F�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        I�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  T�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        W�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        b�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  m�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        p�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  {�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ~�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  �  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ֬   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                     ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                     �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �L   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �LArgo profile    3.1 1.2 19500101000000  20180724220257  20210722161417  5905740 5905740 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7220                            7220                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12003                           12003                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�j�na@�j�na11  @�j�}'ְ@�j�}'ְ@*n�CV��@*n�CV���cJX� ���cJX� ��11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  ?L��@   @@  @�33@�33@�  @�33A��A��A#33A>ffA`  A���A�  A�  A�33A�  A���A���A���B   BffB��B  B ��B(ffB0  B8  B@��BH  BPffBX  B`ffBhffBpffBxffB�33B�ffB�ffB�  B�  B�  B���B���B���B�  B�33B�ffB�33B�33B�ffB�  B���B���B�ffB���B�33B�33Bי�B�  B�  B���B�33B�  B�  B�33B�33B�33C �C  C�C  C�C
33C�C  C�C�fC  C33C�C  C�C�C�fC!�fC$  C&  C(  C*  C,�C.�C0�C2�C4�C6  C8  C:  C;�fC=�fC@  CB  CC�fCE�fCG�fCJ  CL�CN�CP�CR�CT�CV  CX  CZ�C\  C^  C`  Cb�Cd33Cf�Cg�fCj  Cl�Cm�fCp  Cq�fCt  Cv  Cx33Cz  C|  C~�C��C��C��C�  C�  C�  C�  C�  C��3C�  C��C��C��C��C�  C�  C��C�  C�  C��C�  C��C�  C�  C�  C��3C�  C�  C��3C�  C�  C��C��C�  C��C��C�  C��C�  C�  C��C��C�  C�  C�  C��C��C�  C�  C�  C�  C��C��C��C��C��C��C��C�  C��C�  C��3C�  C��3C��3C��C��C��C��C�  C�  C�  C��3C��3C��3C��3C�  C��3C�  C��C�  C�  C�  C��C��C�  C��3C�  C��C�  C��3C�  C��C��3C�  C��C��C��C��C��C��C��C��C��C��C��C��C�  C�  C�  C��3C�  C��C��C�  C��C��C�  C��C��C��C��C�  C��3C�&fD ��D��D&fDS3D��D�fD  D
y�D�fD��D&fDY�D��D�fD��D3D@ D` Dl�Dy�D� D� D�fD� D�fD��D!��D"� D#�3D%ffD&L�D'33D)  D)ٚD+�fD,�fD-ffD/@ D0�D1��D2��D3��D5��D6l�D7FfD9�D9�fD;��D<` D>fD>��D@S3DA� DB��DD,�DE��DFs3DG�3DIy�DJ&fDK��DM&fDMٚDOffDQ  DQ�fDS� DTS3DV33DW  DX3DZ  D[�D\&fD]&fD^,�D`33Da&fDb,�Dc  Dd��De� DgffDh�Di� Dj�fDk�3Dm�Dn9�Do` Dq@ Dr��Dsl�Du&fDvfDw��Dx��DzfDz�f?333?333?333?333?��?333?333?333?��?333?L��?333?L��?333?L��?333?L��?L��?L��?L��?333?333?L��?fff?L��?fff?�  ?�  ?���?���?�33?���?�ff@ff@��@&ff@333@L��@`  @y��@�33@���@���@�ff@�ff@���@ə�@�33@�33@�  A   A  A��A��A33A#33A+33A1��A;33A@  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444414441141414144444114114111111111111111111111111111111111                                                                                                                                                                                                                                                                                                        ?�ff@   @`  @�33@�33@�  @�33A��A��A+33AFffAh  A���A�  A�  A�33A�  A���A���A���B  B
ffB��B  B"��B*ffB2  B:  BB��BJ  BRffBZ  BbffBjffBrffBzffB�33B�ffB�ffB�  B�  B�  B���B���B���B�  B�33B�ffB�33B�33B�ffB�  B���B���B�ffB���B�33B�33Bؙ�B�  B�  B���B�33B�  B�  B�33B�33B�33C ��C� C��C� C��C
�3C��C� C��CffC� C�3C��C� C��C��C ffC"ffC$� C&� C(� C*� C,��C.��C0��C2��C4��C6� C8� C:� C<ffC>ffC@� CB� CDffCFffCHffCJ� CL��CN��CP��CR��CT��CV� CX� CZ��C\� C^� C`� Cb��Cd�3Cf��ChffCj� Cl��CnffCp� CrffCt� Cv� Cx�3Cz� C|� C~��C�L�C�L�C�L�C�@ C�@ C�@ C�@ C�@ C�33C�@ C�L�C�Y�C�L�C�Y�C�@ C�@ C�L�C�@ C�@ C�L�C�@ C�L�C�@ C�@ C�@ C�33C�@ C�@ C�33C�@ C�@ C�L�C�L�C�@ C�L�C�Y�C�@ C�L�C�@ C�@ C�Y�C�L�C�@ C�@ C�@ C�L�C�L�C�@ C�@ C�@ C�@ C�L�C�L�C�L�C�L�C�Y�C�Y�C�L�C�@ C�L�C�@ C�33C�@ C�33C�33C�L�C�L�C�L�C�L�C�@ C�@ C�@ C�33C�33C�33C�33C�@ C�33C�@ C�L�C�@ C�@ C�@ C�Y�C�L�C�@ C�33C�@ C�L�C�@ C�33C�@ C�Y�C�33C�@ C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�Y�C�L�C�L�C�L�C�@ C�@ C�@ C�33C�@ C�L�C�L�C�@ C�L�C�L�C�@ C�L�C�Y�C�L�C�L�C�@ C�33C�ffD ٚD�DFfDs3D��D�fD  D
��D�fD�DFfDy�D��D�fD�D33D` D� D��D��D� D� D�fD� D�fD��D!��D"� D#�3D%�fD&l�D'S3D)  D)��D+�fD,�fD-�fD/` D09�D2�D2��D3ٚD5��D6��D7ffD99�D:fD;��D<� D>&fD>��D@s3DB  DB��DDL�DE��DF�3DH3DI��DJFfDK��DMFfDM��DO�fDQ  DQ�fDS� DTs3DVS3DW@ DX33DZ@ D[9�D\FfD]FfD^L�D`S3DaFfDbL�Dc@ De�De� Dg�fDh9�Di� Dj�fDl3Dm9�DnY�Do� Dq` Dr��Ds��DuFfDv&fDx�Dy�Dz&fD{fG�O�G�O�G�O�G�O�?���G�O�G�O�G�O�?���?���G�O�?���G�O�?���G�O�?���G�O�G�O�G�O�G�O�G�O�?���?�ffG�O�?�ff?�33G�O�?�  ?���?ٙ�?�33@ff@33@&ff@9��@Fff@S33@l��@�  @���@�33@���@���@�ff@�ff@���@ٙ�@�33@�33A   A  A  A��A��A#33A+33A333A9��AC33AH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444414441141414144444114114111111111111111111111111111111111                                                                                                                                                                                                                                                                                                        @ v@ �@ {@ �@ "�@ (�@ 0x@ 8�@ >@ E�@ Q=@ _�@ m�@ z�@ ��@ ��@ ��@ �-@ ��@ �|@ �t@ ��@ �@j@o@g@,`@:@I@UU@c�@p�@~�@��@�H@��@��@��@є@��@�@�,@%@�@!s@/�@>@Lu@Yn@g@uk@��@��@�U@��@��@ƨ@�O@��@��@��@	�@�@%�@33@A�@O0@\�@j@ww@��@��@�@�r@�k@�c@׹@�@�Y@^@V@O@)�@7L@C�@Q=@_�@m:@z�@��@��@��@�-@��@�|@�t@�@��@�@b@�@,`@9X@F�@T�@b�@qS@~�@��@�H@��@��@@��@��@�@�,@�@�@"�@/@=q@K�@X@ff@s_@��@�\@�a@��@�R@ƨ@�O@��@�@��@
=@�@%�@33@@,@N�@\�@k.@x&@�|@��@�m@��@��@�c@׹@�@�@  @�@O@(G@6�@DD@Q=@_�@m:@{�@�7@�0@��@��@�&@�|@�t@�@� @	@	@	�@	,`@	:�@	H]@	UU@	b�@	p�@	~K@	��@	�H@	��@	��@	��@	є@	ލ@	�@	��@
�@
�@
""@
/@
<�@
K�@
Yn@
g@
t�@
��@
�\@
�@
��@
��@
�J@
��@
�H@
�@@
��@
�@�@%�@33@B8@O0@\)@i!@ww@��@��@��@�@�@ȴ@�
@�`@�@ �@V@�@)�@7L@D�@SI@`B@m�@{�@��@�0@��@��@�&@�|@�#@�@�q@@@g@-�@:�@H]@UU@bN@��@�@��@�E@[@>�@`A@��@�J@�`@1@)�@Ji@k.@��@�@�o@�@
=@&�@C�@_�@��@��@�7@�4@1@>�@X�@r�@��@�@խ@�@
@O0@g@~�@�~@ȴ@��@o@+�@]�@uk@��@�w@�O@�@�@D�@Z@��@�@��@��@�@+@S�@}�@�@��@��@�@g@K@`A@�\@��@�@�Y@J@DD@^�@{�@��@��@�(@@ @:@l�@��@��@��@�@
�@+@Ji@i!@��@��@�@� @&;@>@s_@��@�Y@�>G�O�G�O�G�O�G�O�@ G�O�G�O�G�O�@ @ �G�O�@ �G�O�@ �G�O�@ �G�O�G�O�G�O�G�O�G�O�@ �@ vG�O�@ v@ %G�O�@ �@ �@ 1@ 	�@ 
�@ J@ V@ b@ �@ @ �@ �@ �@ �@ 
@ !s@ #�@ &�@ (G@ +@ -@ 0x@ 33@ 6�@ :@ <@ ?}@ B8@ E�@ I@ K�@ O�@ Q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ȴA���A���A���A���A���A��
A��A��A��A��#A��
A���A���A���A�AڸRAڡ�Aڗ�Aڛ�AڑhAڃA�|�A�x�A�v�A�z�AڃAڃA�~�AځAڃAڅA�|�A�x�A�|�AځAڏ\Aڕ�AڅA�hsA�hsA��A�I�A�O�A�v�A�"�A�"�A�ZA�bNA���A�{A�VA��#A�z�A���A���A���A�VA���A���A�"�A���A�O�A�jA�p�A��FA���A���A�ĜA�O�A��mA�G�A�XA�K�A�  A�n�A�z�A��A{?}At��Ar�RAq�An=qAj�AhbNAd��AbjA^��A\Q�A[x�AXZAT��AR~�AN-AI�wAE�AB$�A@�A>~�A<n�A:=qA8�jA7�^A7\)A7VA6��A61A4r�A49XA3�wA3G�A2�HA2��A2�A1ƨA1;dA0��A0n�A01A/
=A.�`A.��A-�mA,ĜA,1'A+p�A)�A'��A&��A&��A&�A&E�A&{A%�^A$��A#�
A#�A"bNA!��A!G�A!%A �A ��A ��A E�A��A�A�`A�jA�TA�7At�AK�A�AM�A�mAdZA��AbAp�A\)A7LA�A��A1'A��A|�A�Av�A��AƨA��AC�A�yA��A��A�A��A��AVA��A��AXA%A��An�AM�A1A�^AXA
=A�/A��An�A��A�PAS�A��AȴA�DA��AG�A�A
�A
�uA
(�A	��A	�hA	7LA��A�!Av�A�Ap�A�A��A�A�\AE�A  A��A�PAl�A��A�9An�AJA�#AA��A�7A�A\)A&�A��A��Az�A$�A��A/A ��A ��A   @��@���@�J@�X@��@��w@��P@�5?@�hs@���@�(�@��P@�;d@�A�@�dZ@���@�j@�33@��@�X@��@���@�@��@�Z@�t�@⟾@���@�%@߾w@�M�@�x�@�ƨ@�
=@�?}@ְ!@��@�"�@ҏ\@�M�@�@�p�@�r�@�p�@�Z@Ɨ�@�%@�  @°!@��#@���@��!@���@�Q�@��P@�;d@�n�@��@��@��w@��@��@�C�@���@�V@�(�@�K�@��@��+@�O�@��@�dZ@�-@�J@���@�ƨ@���@���@�  @�l�@�5?@���@���@�  @��F@�C�@�@�n�@��@��@��D@�Q�@��@��y@�E�@��T@��@��F@�ȴ@�5?@�%@�Q�@�;d@�ff@�hs@�b@���@�5?@���@�V@�Ĝ@���@���A���A���A���A���A�ȴA�ȴA���A���A�ĜA�A���A�ĜA�A�ĜA�ĜA�ĜA�ĜA�ƨA�ƨA�ȴA�ƨA�ƨA�ȴA�ƨA���A�ȴA�ȴA���A�ƨA�ȴA���A�ƨA�ƨA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��
A��A��A��A��#A��A��A��A��A��#A��/G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                        A�ȴA���A���A���A���A���A��
A��A��A��A��#A��
A���A���A���A�AڸRAڡ�Aڗ�Aڛ�AڑhAڃA�|�A�x�A�v�A�z�AڃAڃA�~�AځAڃAڅA�|�A�x�A�|�AځAڏ\Aڕ�AڅA�hsA�hsA��A�I�A�O�A�v�A�"�A�"�A�ZA�bNA���A�{A�VA��#A�z�A���A���A���A�VA���A���A�"�A���A�O�A�jA�p�A��FA���A���A�ĜA�O�A��mA�G�A�XA�K�A�  A�n�A�z�A��A{?}At��Ar�RAq�An=qAj�AhbNAd��AbjA^��A\Q�A[x�AXZAT��AR~�AN-AI�wAE�AB$�A@�A>~�A<n�A:=qA8�jA7�^A7\)A7VA6��A61A4r�A49XA3�wA3G�A2�HA2��A2�A1ƨA1;dA0��A0n�A01A/
=A.�`A.��A-�mA,ĜA,1'A+p�A)�A'��A&��A&��A&�A&E�A&{A%�^A$��A#�
A#�A"bNA!��A!G�A!%A �A ��A ��A E�A��A�A�`A�jA�TA�7At�AK�A�AM�A�mAdZA��AbAp�A\)A7LA�A��A1'A��A|�A�Av�A��AƨA��AC�A�yA��A��A�A��A��AVA��A��AXA%A��An�AM�A1A�^AXA
=A�/A��An�A��A�PAS�A��AȴA�DA��AG�A�A
�A
�uA
(�A	��A	�hA	7LA��A�!Av�A�Ap�A�A��A�A�\AE�A  A��A�PAl�A��A�9An�AJA�#AA��A�7A�A\)A&�A��A��Az�A$�A��A/A ��A ��A   @��@���@�J@�X@��@��w@��P@�5?@�hs@���@�(�@��P@�;d@�A�@�dZ@���@�j@�33@��@�X@��@���@�@��@�Z@�t�@⟾@���@�%@߾w@�M�@�x�@�ƨ@�
=@�?}@ְ!@��@�"�@ҏ\@�M�@�@�p�@�r�@�p�@�Z@Ɨ�@�%@�  @°!@��#@���@��!@���@�Q�@��P@�;d@�n�@��@��@��w@��@��@�C�@���@�V@�(�@�K�@��@��+@�O�@��@�dZ@�-@�J@���@�ƨ@���@���@�  @�l�@�5?@���@���@�  @��F@�C�@�@�n�@��@��@��D@�Q�@��@��y@�E�@��T@��@��F@�ȴ@�5?@�%@�Q�@�;d@�ff@�hs@�b@���@�5?@���@�V@�Ĝ@���@���A���A���A���A���A�ȴA�ȴA���A���A�ĜA�A���A�ĜA�A�ĜA�ĜA�ĜA�ĜA�ƨA�ƨA�ȴA�ƨA�ƨA�ȴA�ƨA���A�ȴA�ȴA���A�ƨA�ȴA���A�ƨA�ƨA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��
A��A��A��A��#A��A��A��A��A��#A��/G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                        ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	w�B	w�B	w�B	w�B	w�B	w�B	w�B	w�B	w�B	w�B	w�B	w�B	w�B	w�B	w�B	x�B	x�B	{�B	}�B	}�B	~�B	�B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�=B	�JB	�7B	�%B	r�B	YB	,B	#�B	XB	�-B	��B	��B	�B
�B
N�B
W
B
YB
I�B
hsB
x�B
dZB
I�B
E�B
�DB
�B1'BaHBp�Bl�BhsBZB?}B1B
�fB
��B
�VB
6FB
  B	�/B	�3B	�%B	k�B	2-B	�B	{B	
=B��B��B�B�B�B�B	bB	VB	B��B	%B	1B	  B	B	uB	"�B	=qB	o�B	��B	�-B	�}B	ƨB	��B	��B	�B	�NB	�B	��B	��B
oB
�B
,B
0!B
6FB
>wB
@�B
G�B
M�B
P�B
Q�B
S�B
T�B
T�B
Q�B
M�B
K�B
L�B
L�B
M�B
M�B
M�B
N�B
O�B
P�B
P�B
P�B
R�B
R�B
Q�B
R�B
R�B
R�B
Q�B
O�B
N�B
O�B
N�B
O�B
O�B
T�B
XB
YB
VB
R�B
O�B
L�B
K�B
K�B
Q�B
Q�B
S�B
T�B
S�B
T�B
VB
VB
T�B
R�B
R�B
R�B
O�B
N�B
O�B
N�B
M�B
L�B
M�B
M�B
L�B
L�B
K�B
L�B
K�B
J�B
K�B
K�B
J�B
J�B
K�B
L�B
L�B
L�B
K�B
J�B
J�B
I�B
H�B
G�B
E�B
D�B
D�B
C�B
C�B
B�B
A�B
A�B
A�B
B�B
A�B
@�B
?}B
=qB
;dB
:^B
:^B
9XB
9XB
9XB
8RB
9XB
8RB
7LB
9XB
8RB
8RB
7LB
7LB
7LB
9XB
;dB
;dB
;dB
;dB
:^B
:^B
9XB
8RB
7LB
6FB
6FB
5?B
33B
2-B
0!B
0!B
0!B
0!B
/B
0!B
/B
.B
-B
.B
.B
(�B
%�B
%�B
#�B
#�B
"�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
bB
bB
�B
VB
hB
PB
VB
VB
PB
PB
PB
JB
JB
JB
JB
JB
PB
VB
VB
\B
bB
\B
bB
\B
hB
hB
oB
oB
uB
hB
hB
{B
{B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
"�B
#�B
%�B
%�B
%�B
&�B
&�B
(�B
(�B
(�B
)�B
+B
+B
,B
-B	x�B	w�B	w�B	x�B	w�B	x�B	v�B	w�B	v�B	x�B	x�B	x�B	x�B	w�B	x�B	x�B	x�B	w�B	w�B	w�B	w�B	x�B	x�B	x�B	w�B	w�B	x�B	w�B	x�B	w�B	w�B	w�B	w�B	x�B	w�B	w�B	w�B	w�B	w�B	x�B	w�B	w�B	v�B	w�B	w�B	w�B	w�B	w�B	v�B	w�B	w�B	w�B	w�B	v�B	w�B	w�B	w�B	w�B	w�B	w�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                        B	w�B	w�B	w�B	w�B	w�B	w�B	w�B	w�B	w�B	w�B	w�B	w�B	w�B	w�B	w�B	x�B	x�B	{�B	}�B	}�B	~�B	��B	�B	�B	��B	��B	� B	�B	�B	�B	�B	�	B	�B	�B	�B	�B	�*B	�8B	�%B	�B	r�B	YB	+�B	#�B	W�B	�B	�yB	��B	�B
�B
N�B
V�B
Y
B
I�B
hfB
x�B
dMB
I�B
E�B
�8B
�B1Ba>Bp�Bl�BhjBZB?uB)B
�^B
��B
�OB
6?B	��B	�(B	�,B	�B	k~B	2&B	�B	uB	
7B��B��B�B�B�B�B	_B	SB	B��B	#B	/B��B	B	tB	"�B	=qB	o�B	��B	�.B	�~B	ƪB	��B	�B	�B	�RB	�B	��B	� B
uB
�B
,B
0)B
6OB
>�B
@�B
G�B
M�B
P�B
Q�B
TB
UB
UB
Q�B
M�B
K�B
L�B
L�B
M�B
M�B
M�B
N�B
O�B
P�B
P�B
P�B
SB
SB
RB
S	B
S	B
S
B
RB
O�B
N�B
O�B
N�B
O�B
O�B
UB
X-B
Y5B
V"B
SB
O�B
L�B
K�B
K�B
RB
RB
TB
U!B
TB
U"B
V)B
V)B
U$B
SB
SB
SB
PB
OB
PB
OB
M�B
L�B
M�B
M�B
L�B
L�B
K�B
L�B
K�B
J�B
K�B
K�B
J�B
J�B
K�B
M B
M B
MB
K�B
J�B
J�B
I�B
H�B
G�B
E�B
D�B
D�B
C�B
C�B
B�B
A�B
A�B
A�B
B�B
A�B
@�B
?�B
=�B
;�B
:�B
:�B
9�B
9�B
9�B
8�B
9�B
8�B
7�B
9�B
8�B
8�B
7�B
7�B
7�B
9�B
;�B
;�B
;�B
;�B
:�B
:�B
9�B
8�B
7�B
6�B
6�B
5�B
3�B
2~B
0sB
0sB
0tB
0tB
/oB
0uB
/pB
.iB
-dB
.kB
.kB
)OB
&>B
&?B
$4B
$6B
#1B
!'B
 !B
B
B
B

B
B
B
B
	B
B
B
B
B
B
B
�B
�B
�B
B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
�B
	B
B
B
B
B
B
B
'B
)B
/B
+B
9B
:B
BB
IB
QB
YB
ZB
bB
cB
kB
mB
tB
oB
qB
~B
�B
{B
 �B
 �B
 �B
!�B
"�B
"�B
#�B
$�B
&�B
&�B
&�B
'�B
'�B
)�B
)�B
)�B
*�B
+�B
+�B
,�B
-�B	x�B	w�B	w�B	x�B	w�B	x�B	v�B	w�B	v�B	x�B	x�B	x�B	x�B	w�B	x�B	x�B	x�B	w�B	w�B	w�B	w�B	x�B	x�B	x�B	w�B	w�B	x�B	w�B	x�B	w�B	w�B	w�B	w�B	x�B	w�B	w�B	w�B	w�B	w�B	x�B	w�B	w�B	v�B	w�B	w�B	w�B	w�B	w�B	v�B	w�B	w�B	w�B	w�B	v�B	w�B	w�B	w�B	w�B	w�B	w�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                        <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202572021061413571920210614135719202107221611192021072216111920210722161119201807242202572021061413571920210614135719202107221611192021072216111920210722161119PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422025720180724220257  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422025720180724220257QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422025720180724220257QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216141720210722161417IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                