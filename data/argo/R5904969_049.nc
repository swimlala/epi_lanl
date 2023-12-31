CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:14:09Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Jl   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  LP   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  S�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ]L   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  f�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  nH   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  u�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  w�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �(   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �`   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �p   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �t   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024141409  20181024141409  5904969 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               1A   AO  6784                            2B  A   APEX                            7725                            111215                          846 @��d�\;1   @��e��@2e`A�7L�cυ�Q�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      1B   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D��D� DfD� D  D� D  D� D  D� D  D� D  D� D fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DFy�DF��DG� DH  DHy�DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Dy��D�EqD���111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@���A ��A ��A@��A`��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB 33B��B33B33B 33B(33B033B833B@33BH33BP33BX��B`33Bh33Bp33Bx33B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C �C�C�C�C�C
�C�C�C�C�C�C�B��B��B��B��B��B��B��B��B��B��C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�3C�fC���C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�3C�fC�fC�fC�fC�fC�fC�fC�fC�fC�3C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fD 3D �3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D|�D��D�3D	�D�3D3D�3D3D�3D3D�3D3D�3D3D�3D 	�D �3D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=3D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB�3DC3DC�3DD3DD�3DE3DE�3DF3DF|�DF��DG�3DH3DH|�DI3DI�3DJ3DJ�3DK3DK�3DL3DL�3DM3DM�3DN3DN�3DO3DO�3DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX3DX�3DY3DY�3DZ3DZ�3D[3D[�3D\3D\�3D]3D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da�3Db3Db�3Dc3Dc�3Dd3Dd�3De3De�3Dy��D�GD��{111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�r�A�p�A�v�A�v�A�n�A�hsA�ffA�^5A�M�A�5?A� �A���A���A��yA���Aݴ9AݑhA݉7A݋DA݋DA݋DA݋DA݉7A�|�A�`BA�O�A��A�
=A�  A�A�l�A��A�t�Aڗ�A��A�7LA��/A� �A�I�A�A�A�O�Aϲ-A�5?A��mA�x�A�n�A���A��A�ȴA�dZA��mAʇ+A�(�A�{A��AƬA���A��TA�Q�A� �A�33A�  A�K�A���A��HA���A�$�A���A���A��A���A��FA���A� �A�(�A�{A��AƬA���A��TA�Q�A� �A�33A�  A�K�A���A��HA���A�$�A���A���A��A���A��FA���A� �A���A���A���A�bNA�M�A�/A��A��A�\)A�bA�oA��/A�l�A��A���A�p�A�ZA�?}A�&�A���A�A�n�A�
=A�ȴA�;dA��9A��A���A�bA�`BA��RA�ffA��A�9XA���A��^A�(�A��/A�hsA���A���A��9A��A�;dA�M�A�/A�bA�(�AyVAvJAs�TAp�Ah�AfbAc"�A`=qA]ƨAZ^5AY�AY�AW�wAV�+AU�7AR��AQdZAP��APffAP=qAP$�AOhsAMO�AH�!AE33AD �AB�9AAA@9XA?�A<�uA9\)A7A5p�A3�A1A0  A/VA.{A,��A*��A(�uA'x�A&�A%�FA"ȴA!;dA ȴA ~�A 1A?}Av�AS�AK�Ar�AĜA��AbA��A{A7LA��A(�A�hA�A�DA��A�A��A�AA
��A
�\A	�wA��A(�A�FA��AZA�FA&�A7LAZA�A�wA1'A �A VA   @���@���@��@�`B@�@�O�@���@�G�@��@��m@�$�@�M�@�@�1'@���@�@�t�@�G�@�{@�G�@�/@ە�@�(�@�j@��@�J@�ff@�Q�@���@�p�@֧�@ج@թ�@�o@�x�@�@�I�@�K�@̓u@Ǿw@�S�@��@�^5@�$�@Ł@�7L@��@�Ĝ@�9X@��;@å�@�|�@�dZ@�"�@�@�5?@���@��h@�?}@�X@���@�ƨ@�|�@�\)@��@�-@�^5@�C�@ǍP@�9X@���@Ǖ�@�;d@��H@�ȴ@�{@���@Ł@��@ģ�@\@���@��@�?}@�Z@�1'@�b@�b@�Q�@��F@���@�X@��@��9@��@�Z@� �@��w@�dZ@�
=@��H@��\@�-@�@��7@��@��@�z�@�I�@���@�+@��;@�r�@�I�@�l�@���@�ff@��^@�7L@�&�@��-@�(�@��j@��@��@��R@��7@��u@���@���@��D@�$�@�V@�^5@��R@�=q@�^5@�M�@�V@�V@�@��h@���@���@�ƨ@���@��+@��7@��D@��@�`B@�X@���@�t�@�
=@�@�
=@�+@�;d@�C�@�\)@��w@�;d@���@�=q@�J@���@��7@�?}@�%@���@���@�V@���@�7L@�-@��@��h@�V@��j@�9X@��P@��@��@���@��@�dZ@�K�@�
=@��y@��R@��+@�~�@�$�@�{@�{@�J@�J@�{@�{@�{@�J@�@�hs@���@�(�@��w@���@�n�@��@��-@���@��@�x�@�7L@���@��@�Z@�bN@�r�@�z�@��@�z�@�Z@��@���@�l�@��@�@�V@���@�`B@�hs@�X@�%@���@��D@�Z@��@�K�@��@��@��y@���@��R@���@�n�@���@���@m%111111111111111111111111111111111111111111111111111111111111111111111111414111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�r�A�p�A�v�A�v�A�n�A�hsA�ffA�^5A�M�A�5?A� �A���A���A��yA���Aݴ9AݑhA݉7A݋DA݋DA݋DA݋DA݉7A�|�A�`BA�O�A��A�
=A�  A�A�l�A��A�t�Aڗ�A��A�7LA��/A� �A�I�A�A�A�O�Aϲ-A�5?A��mA�x�A�n�A���A��A�ȴA�dZA��mAʇ+A�(�A�{A��AƬA���A��TA�Q�A� �A�33A�  A�K�A���A��HA���A�$�A���A���A��A���A��FA���A� �A�(�A�{A��AƬA���A��TA�Q�A� �A�33A�  A�K�A���A��HA���A�$�A���A���A��A���A��FA���A� �A���A���A���A�bNA�M�A�/A��A��A�\)A�bA�oA��/A�l�A��A���A�p�A�ZA�?}A�&�A���A�A�n�A�
=A�ȴA�;dA��9A��A���A�bA�`BA��RA�ffA��A�9XA���A��^A�(�A��/A�hsA���A���A��9A��A�;dA�M�A�/A�bA�(�AyVAvJAs�TAp�Ah�AfbAc"�A`=qA]ƨAZ^5AY�AY�AW�wAV�+AU�7AR��AQdZAP��APffAP=qAP$�AOhsAMO�AH�!AE33AD �AB�9AAA@9XA?�A<�uA9\)A7A5p�A3�A1A0  A/VA.{A,��A*��A(�uA'x�A&�A%�FA"ȴA!;dA ȴA ~�A 1A?}Av�AS�AK�Ar�AĜA��AbA��A{A7LA��A(�A�hA�A�DA��A�A��A�AA
��A
�\A	�wA��A(�A�FA��AZA�FA&�A7LAZA�A�wA1'A �A VA   @���@���@��@�`B@�@�O�@���@�G�@��@��m@�$�@�M�@�@�1'@���@�@�t�@�G�@�{@�G�@�/@ە�@�(�@�j@��@�J@�ff@�Q�@���@�p�@֧�@ج@թ�@�o@�x�@�@�I�@�K�@̓u@Ǿw@�S�@��@�^5@�$�@Ł@�7L@��@�Ĝ@�9X@��;@å�@�|�@�dZ@�"�@�@�5?@���@��h@�?}@�X@���@�ƨ@�|�@�\)@��@�-@�^5@�C�@ǍP@�9X@���@Ǖ�@�;d@��H@�ȴ@�{@���@Ł@��@ģ�@\@���@��@�?}@�Z@�1'@�b@�b@�Q�@��F@���@�X@��@��9@��@�Z@� �@��w@�dZ@�
=@��H@��\@�-@�@��7@��@��@�z�@�I�@���@�+@��;@�r�@�I�@�l�@���@�ff@��^@�7L@�&�@��-@�(�@��j@��@��@��R@��7@��u@���@���@��D@�$�@�V@�^5@��R@�=q@�^5@�M�@�V@�V@�@��h@���@���@�ƨ@���@��+@��7@��D@��@�`B@�X@���@�t�@�
=@�@�
=@�+@�;d@�C�@�\)@��w@�;d@���@�=q@�J@���@��7@�?}@�%@���@���@�V@���@�7L@�-@��@��h@�V@��j@�9X@��P@��@��@���@��@�dZ@�K�@�
=@��y@��R@��+@�~�@�$�@�{@�{@�J@�J@�{@�{@�{@�J@�@�hs@���@�(�@��w@���@�n�@��@��-@���@��@�x�@�7L@���@��@�Z@�bN@�r�@�z�@��@�z�@�Z@��@���@�l�@��@�@�V@���@�`B@�hs@�X@�%@���@��D@�Z@��@�K�@��@��@��y@���@��R@���@�n�@���@���@m%111111111111111111111111111111111111111111111111111111111111111111111111414111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
r�B
q�B
q�B
p�B
p�B
q�B
r�B
s�B
t�B
u�B
v�B
x�B
x�B
w�B
s�B
u�B
x�B
� B
�%B
��B
�'B
�^B
��B
�B
�/B
�`B
��BB�B)�B%�B9XB!�B�B�B�B6FB;dB>wBA�BZBo�B�B��B��B��B�ZB
=B�B0!BD�Bn�Br�Bo�Bt�BjBo�Bq�Bt�B�DB�BƨBƨBĜB�B��B��B��B�ZB
=B�B0!BD�Bn�Br�Bo�Bt�BjBo�Bq�Bt�B�DB�BƨBƨBĜBB�)B�)B��B�jB��B�=B�Bm�BS�B5?B�B\B��B�B�fB�`B�ZB�ZB�`B�`B�B��B��BŢB�?B��B��B�BiyBL�B6FB%�B�BJBB
�B
�)B
��B
��B
�qB
�B
�B
dZB
XB
G�B
.B
uB	�mB	��B	��B	��B	�%B	p�B	bNB	N�B	C�B	49B	.B	,B	&�B	 �B	�B	uB	PB	1B	%B	B	B	  B��B�B�;B�B�B��B��B��B��BȴBĜBÖB��B�}B�dBÖBBÖB�qB�9B�!B��B��B�7B~�B}�B}�B|�B}�B{�Bw�Bn�BjBffBt�B|�Br�Bk�BgmB`BB[#B[#B\)B]/B`BBbNBdZBdZBdZBk�Bl�Bm�Bn�Bo�Bm�Bn�B}�B�uB��B��B�3B�FB�XB�LB�B�9B�3B�B��B�hB�DB�{B��B�JB�B�B� B�+B�hB�\B�JB�B{�Bw�Bs�Bk�Bo�Bs�Bl�B_;BgmB�B�bB�Bt�Bn�Bn�B�1B��B�\B��B�RBƨBŢB�B��B�=B�DB�DB�JB�JB�\B�VB�VB�VB�bB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�LB�B��B	%B	{B	�B	�B	�B	�B	(�B	+B	+B	)�B	'�B	!�B	�B	�B		7B	%B	%B	B	B	
=B	
=B	DB	DB	JB	\B	bB	hB	oB	uB	{B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	!�B	&�B	,B	2-B	9XB	9XB	7LB	6FB	7LB	>wB	A�B	D�B	K�B	\)B	k�B	dZB	bNB	iyB	o�B	l�B	q�B	t�B	{�B	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�FB	�LB	�FB	�9B	�3B	�-B	�-B	�'B	�'B	�'B	�-B	�3B	�XB	�jB	ĜB	ƨB	ɺB	ɺB	ɺB	ȴB	ɺB	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�)B	�)B	�)B	�/B	�;B	�BB	�BB	�HB	�NB	�ZB	�fB	�`B	�`B	�fB	�fB	�`B	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
?B
�B
+Q111111111111111111111111111111111111111111111111111111111111111111111111414111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
r�B
q�B
q�B
p�B
p�B
q�B
r�B
s�B
t�B
u�B
v�B
x�B
x�B
w�B
s�B
u�B
x�B
� B
�%B
��B
�'B
�^B
��B
�B
�/B
�`B
��BB�B)�B%�B9XB!�B�B�B�B6FB;dB>wBA�BZBo�B�B��B��B��B�ZB
=B�B0!BD�Bn�Br�Bo�Bt�BjBo�Bq�Bt�B�DB�BƨBƨBĜB�B��B��B��B�ZB
=B�B0!BD�Bn�Br�Bo�Bt�BjBo�Bq�Bt�B�DB�BƨBƨBĜBB�)B�)B��B�jB��B�=B�Bm�BS�B5?B�B\B��B�B�fB�`B�ZB�ZB�`B�`B�B��B��BŢB�?B��B��B�BiyBL�B6FB%�B�BJBB
�B
�)B
��B
��B
�qB
�B
�B
dZB
XB
G�B
.B
uB	�mB	��B	��B	��B	�%B	p�B	bNB	N�B	C�B	49B	.B	,B	&�B	 �B	�B	uB	PB	1B	%B	B	B	  B��B�B�;B�B�B��B��B��B��BȴBĜBÖB��B�}B�dBÖBBÖB�qB�9B�!B��B��B�7B~�B}�B}�B|�B}�B{�Bw�Bn�BjBffBt�B|�Br�Bk�BgmB`BB[#B[#B\)B]/B`BBbNBdZBdZBdZBk�Bl�Bm�Bn�Bo�Bm�Bn�B}�B�uB��B��B�3B�FB�XB�LB�B�9B�3B�B��B�hB�DB�{B��B�JB�B�B� B�+B�hB�\B�JB�B{�Bw�Bs�Bk�Bo�Bs�Bl�B_;BgmB�B�bB�Bt�Bn�Bn�B�1B��B�\B��B�RBƨBŢB�B��B�=B�DB�DB�JB�JB�\B�VB�VB�VB�bB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�LB�B��B	%B	{B	�B	�B	�B	�B	(�B	+B	+B	)�B	'�B	!�B	�B	�B		7B	%B	%B	B	B	
=B	
=B	DB	DB	JB	\B	bB	hB	oB	uB	{B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	!�B	&�B	,B	2-B	9XB	9XB	7LB	6FB	7LB	>wB	A�B	D�B	K�B	\)B	k�B	dZB	bNB	iyB	o�B	l�B	q�B	t�B	{�B	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�FB	�LB	�FB	�9B	�3B	�-B	�-B	�'B	�'B	�'B	�-B	�3B	�XB	�jB	ĜB	ƨB	ɺB	ɺB	ɺB	ȴB	ɺB	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�)B	�)B	�)B	�/B	�;B	�BB	�BB	�HB	�NB	�ZB	�fB	�`B	�`B	�fB	�fB	�`B	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
?B
�B
+Q111111111111111111111111111111111111111111111111111111111111111111111111414111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.05 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024141409                              AO  ARCAADJP                                                                    20181024141409    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024141409  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024141409  QCF$                G�O�G�O�G�O�4000            