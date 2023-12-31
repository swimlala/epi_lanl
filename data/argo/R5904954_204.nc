CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:35Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @P   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  B   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  J�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Q�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  X`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Z   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  `�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  b�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pp   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  r(   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �L   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �\   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �`   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �t   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �|Argo profile    3.1 1.2 19500101000000  20181005191735  20181005191735  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��d��1   @��ey\��@6r� Ĝ�d���$�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  BffB  B ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�C�C  C  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cw�fCz  C|  C~  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��3C��3C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��3C��3C��3C��3C��3C��3C�  C��C��C��C�  C�  C��3C��3C��3C��3C��3C��fC��fC��3C��3C��3C�  C��C��C�  C�  C��C�  C��3C��3C��3C�  C�  C�  C�  C��C��C�  C��3C�  C��C��3C�  C�  C�  C��C�  C�  C��C��C�  C�  C��C��C��C��C��C��C��C��C�  C��C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C��fC��3C��3C��fC��3C�  C��3C�  C��C��C��C��3C��C��C�  C��3C�  C�  C��C�  C��3C�  C��D   D � D  D� D��D�fD  D�fD��D� D  D�fD��D� D  D�fD  D� D	  D	�fD
fD
� D  D� DfD�fDfD� D  D� D  Dy�D  Dy�D��Dy�D  Dy�D  D� D  D� D  D� D��D� D��D� D��Dy�D��D� D  D�fD��D� D��D� D��D�fD��D� D  D� D   D y�D ��D!�fD"fD"y�D#  D#�fD#��D$� D%  D%y�D&  D&y�D&��D'� D(fD(� D)  D)� D*fD*� D*��D+� D,  D,� D,��D-� D.fD.y�D.��D/y�D0fD0��D1  D1� D2  D2�fD3  D3� D4  D4y�D4��D5y�D6  D6�fD7fD7�fD8fD8�fD9fD9� D:  D:� D;fD;�fD<fD<�fD=fD=�fD>  D>� D?  D?�fD?��D@� DA  DA�fDB  DB� DB��DCy�DDfDD� DD��DE�fDE��DFy�DG  DG� DG�3DHy�DI  DI� DJfDJ� DJ��DK� DL  DLy�DM  DM� DN  DN�fDOfDO�fDPfDP�fDQ  DQs3DQ��DR� DSfDS� DT  DT�fDU  DU�fDVfDV� DW  DW�fDXfDXs3DX�3DYy�DY��DZ� D[fD[� D[��D\� D]�D]��D^�D^� D^��D_y�D`fD`� Da  Da� Db  Db�fDcfDc� Dc��Ddy�De  De�fDffDf�fDg  Dy��D�!�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�=qA�A!�AA�Aa�A��\A��\A��\A��\A��\AЏ\A��\A�\)B G�BG�B�BG�B �B(G�B0G�B8G�B@G�BHG�BPG�BXG�B`G�BhG�BpG�BxG�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�W
B�W
B�W
B�#�B�#�B�#�C �C�C�C�C�C
�C�C�C�C�C�C+�C+�C�C�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cw�RCz�C|�C~�C��C��C��C��C��C��C��)C��)C��C��)C��)C��C��C��C��C��)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��)C��)C��)C��)C��)C��)C��C��C��C��C��C��C��)C��)C��)C��)C��)C��\C��\C��)C��)C��)C��C��C��C��C��C��C��C��)C��)C��)C��C��C��C��C��C��C��C��)C��C��C��)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��)C��C��C��C��)C��C��C��C��C��)C��)C��\C��)C��)C��\C��)C��C��)C��C��C��C��C��)C��C��C��C��)C��C��C��C��C��)C��C��D {D �{D{D�{D�D��D{D��D�D�{D{D��D�D�{D{D��D{D�{D	{D	��D

�D
�{D{D�{D
�D��D
�D�{D{D�{D{D~D{D~D�D~D{D~D{D�{D{D�{D{D�{D�D�{D�D�{D�D~D�D�{D{D��D�D�{D�D�{D�D��D�D�{D{D�{D {D ~D �D!��D"
�D"~D#{D#��D#�D$�{D%{D%~D&{D&~D&�D'�{D(
�D(�{D){D)�{D*
�D*�{D*�D+�{D,{D,�{D,�D-�{D.
�D.~D.�D/~D0
�D0�HD1{D1�{D2{D2��D3{D3�{D4{D4~D4�D5~D6{D6��D7
�D7��D8
�D8��D9
�D9�{D:{D:�{D;
�D;��D<
�D<��D=
�D=��D>{D>�{D?{D?��D?�D@�{DA{DA��DB{DB�{DB�DC~DD
�DD�{DD�DE��DE�DF~DG{DG�{DG��DH~DI{DI�{DJ
�DJ�{DJ�DK�{DL{DL~DM{DM�{DN{DN��DO
�DO��DP
�DP��DQ{DQw�DQ�DR�{DS
�DS�{DT{DT��DU{DU��DV
�DV�{DW{DW��DX
�DXw�DX��DY~DY�DZ�{D[
�D[�{D[�D\�{D]HD]�HD^HD^�{D^�D_~D`
�D`�{Da{Da�{Db{Db��Dc
�Dc�{Dc�Dd~De{De��Df
�Df��Dg{Dy�>D�$)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�/A�/A�/A�1'A�33A�1'A�33A�7LA�7LA�5?A�33A�33A�7LA�5?A�5?A�5?A�5?A�;dA�=qA�?}A�7LA�7LA�7LA�5?A�;dA�E�A�5?A�%A��A��A��yA��`A��TA��HA��/A��;A��;A��/A��#A��HA��;A��
A���A��#A��HA��`A��A��A���AͮA͏\A�hsA�$�A̸RA�n�A� �A�ĜA�O�AʓuA��TA�z�A��A�XA�A�VA��A�=qA�I�A�/A�M�A��DA�x�A�(�A�^5A��mA�7LA��RA�1A�`BA��HA��A�M�A�VA��wA��A���A��A�^5A�A�A�{A��A�hsA���A�I�A���A�ȴA�&�A�ĜA���A�|�A��RA�9XA���A�hA{l�Ax1'AwoAv �Aup�Au�As|�Aq�An�RAk��AjI�Ah�9Ag\)Ae%Ac��Ab��A`�A_O�A]�A[�TAZM�AY"�AW�AT��AQ\)AN�AM�PAL1AK&�AJZAI��AH�yAH$�AF~�AEG�ADr�AC�AA"�A?�mA?�A>9XA=O�A<(�A:��A:{A9p�A9oA8��A8 �A7|�A6r�A4�jA3��A2M�A0�+A/?}A.�A-ƨA,�A+"�A*�A)�
A)�-A(9XA'G�A$ĜA#x�A#K�A"�/A"$�A!�A!/A �9A��A$�A�-Ap�A&�An�A��A%A-A�
A��A��A5?A^5A�AbA&�AĜA��AS�A�A5?AC�A�!AjA�HA%A
�A	7LA��A�A �AoA�uA�A|�A�At�A�AAx�A (�@�7L@��m@��H@��@�j@��@�^5@��-@���@�\@�w@�x�@�I�@�S�@�R@��@�7L@�r�@�C�@��@��
@�n�@�hs@��
@�ȴ@�J@��@ݑh@�X@ܛ�@�J@�&�@��@�l�@���@Ο�@�-@�p�@���@̃@�Q�@���@��
@���@ˍP@���@��#@���@���@��H@�^5@�v�@���@�p�@�hs@�?}@�1@þw@��
@��
@�K�@��@§�@�&�@�I�@���@��y@��7@���@�&�@�ȴ@�?}@�{@���@��@�X@���@���@���@�7L@���@�{@��!@��@��@���@�t�@�33@��H@�^5@��@��9@��@�Z@�1@��w@���@�S�@���@�M�@�=q@���@�`B@�Ĝ@� �@���@���@��@���@�?}@� �@�9X@���@��h@��^@���@���@��@�z�@��y@���@�E�@�V@�E�@���@�V@��u@�9X@�1@���@�+@�ȴ@���@�E�@��#@���@���@��@�X@��@���@��u@�9X@��@�ƨ@���@�K�@�ȴ@�=q@�-@��T@�7L@��j@��@�j@�  @�ƨ@�K�@���@���@�$�@��#@���@��@�x�@�hs@��@��j@�9X@���@�33@���@�ȴ@�^5@�M�@���@���@�hs@�&�@�%@��`@�z�@��@�z�@�r�@��@���@��w@��P@�;d@��+@�^5@�M�@�E�@�$�@�@��@��^@��7@�`B@�&�@�V@�&�@�7L@�?}@�O�@�&�@��j@�9X@��m@�t�@�dZ@�S�@�S�@�S�@�K�@�"�@��\@�V@��@��"11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�/A�/A�/A�1'A�33A�1'A�33A�7LA�7LA�5?A�33A�33A�7LA�5?A�5?A�5?A�5?A�;dA�=qA�?}A�7LA�7LA�7LA�5?A�;dA�E�A�5?A�%A��A��A��yA��`A��TA��HA��/A��;A��;A��/A��#A��HA��;A��
A���A��#A��HA��`A��A��A���AͮA͏\A�hsA�$�A̸RA�n�A� �A�ĜA�O�AʓuA��TA�z�A��A�XA�A�VA��A�=qA�I�A�/A�M�A��DA�x�A�(�A�^5A��mA�7LA��RA�1A�`BA��HA��A�M�A�VA��wA��A���A��A�^5A�A�A�{A��A�hsA���A�I�A���A�ȴA�&�A�ĜA���A�|�A��RA�9XA���A�hA{l�Ax1'AwoAv �Aup�Au�As|�Aq�An�RAk��AjI�Ah�9Ag\)Ae%Ac��Ab��A`�A_O�A]�A[�TAZM�AY"�AW�AT��AQ\)AN�AM�PAL1AK&�AJZAI��AH�yAH$�AF~�AEG�ADr�AC�AA"�A?�mA?�A>9XA=O�A<(�A:��A:{A9p�A9oA8��A8 �A7|�A6r�A4�jA3��A2M�A0�+A/?}A.�A-ƨA,�A+"�A*�A)�
A)�-A(9XA'G�A$ĜA#x�A#K�A"�/A"$�A!�A!/A �9A��A$�A�-Ap�A&�An�A��A%A-A�
A��A��A5?A^5A�AbA&�AĜA��AS�A�A5?AC�A�!AjA�HA%A
�A	7LA��A�A �AoA�uA�A|�A�At�A�AAx�A (�@�7L@��m@��H@��@�j@��@�^5@��-@���@�\@�w@�x�@�I�@�S�@�R@��@�7L@�r�@�C�@��@��
@�n�@�hs@��
@�ȴ@�J@��@ݑh@�X@ܛ�@�J@�&�@��@�l�@���@Ο�@�-@�p�@���@̃@�Q�@���@��
@���@ˍP@���@��#@���@���@��H@�^5@�v�@���@�p�@�hs@�?}@�1@þw@��
@��
@�K�@��@§�@�&�@�I�@���@��y@��7@���@�&�@�ȴ@�?}@�{@���@��@�X@���@���@���@�7L@���@�{@��!@��@��@���@�t�@�33@��H@�^5@��@��9@��@�Z@�1@��w@���@�S�@���@�M�@�=q@���@�`B@�Ĝ@� �@���@���@��@���@�?}@� �@�9X@���@��h@��^@���@���@��@�z�@��y@���@�E�@�V@�E�@���@�V@��u@�9X@�1@���@�+@�ȴ@���@�E�@��#@���@���@��@�X@��@���@��u@�9X@��@�ƨ@���@�K�@�ȴ@�=q@�-@��T@�7L@��j@��@�j@�  @�ƨ@�K�@���@���@�$�@��#@���@��@�x�@�hs@��@��j@�9X@���@�33@���@�ȴ@�^5@�M�@���@���@�hs@�&�@�%@��`@�z�@��@�z�@�r�@��@���@��w@��P@�;d@��+@�^5@�M�@�E�@�$�@�@��@��^@��7@�`B@�&�@�V@�&�@�7L@�?}@�O�@�&�@��j@�9X@��m@�t�@�dZ@�S�@�S�@�S�@�K�@�"�@��\@�V@��@��"11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B}�B}�B}�B}�B}�B}�B}�B}�B~�B~�B~�B}�B~�B}�B}�B}�B}�B� B� B�B~�B}�B}�B}�B�B�VB�?B�`B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BBBB
=B�B�B�B!�B"�B'�B-B5?BA�BI�BQ�BW
B\)BW
BT�B^5Bt�Bu�B�+B�=B�7B�VB�uB��B��B��B�3B�dB�LB�?B%�B�B�B�#B�)B�/B�#B��B��BÖB�?B��B��B�BjBK�BD�B9XB+BbB
�B
�/B
��B
��B
��B
�hB
�B
cTB
J�B
B�B
?}B
=qB
9XB
-B
!�B
DB	��B	�B	�TB	�B	ȴB	��B	�RB	�B	��B	��B	�DB	�B	w�B	l�B	\)B	I�B	=qB	5?B	,B	&�B	#�B	�B	�B	�B	\B	
=B	+B	B��B��B�B�B�sB�`B�NB�BB�;B�5B�)B�#B�B�B��B��B��BŢBB�}B�qB�XB�?B�-B�!B�B��B��B��B��B��B��B��B��B�{B�uB�hB�VB�JB�JB�DB�7B�%B�B�B�B�B}�Bz�Bv�Bt�Bq�Bp�Bo�Bl�Bk�BjBhsBffBe`BgmBjBe`BbNB`BB_;BdZBm�BiyBgmBffBdZBaHB_;B[#BZBXBVBS�BP�BP�BP�BO�BP�BO�BO�BN�BL�BK�BJ�BI�BI�BI�BJ�BJ�BJ�BL�BO�BQ�BVBVBVBVBW
BW
BW
BVBVBVBR�BW
BZB]/B`BBcTBffBdZBdZBcTBbNBcTBbNBcTBe`BjBk�Bl�Bm�Bn�Bq�Bs�Bv�B|�B�7B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�?B�XB�XB�XB�wB��B��BƨB��B��B�;B�NB�fB�yB�B�B�B�B�B��B��B��B��B��B��B	%B		7B	
=B	JB	\B	hB	{B	�B	�B	�B	�B	!�B	"�B	'�B	2-B	9XB	;dB	=qB	>wB	A�B	B�B	?}B	=qB	E�B	I�B	L�B	N�B	O�B	Q�B	T�B	W
B	YB	[#B	\)B	]/B	_;B	aHB	bNB	cTB	cTB	dZB	ffB	ffB	gmB	iyB	l�B	m�B	n�B	p�B	r�B	v�B	x�B	x�B	{�B	~�B	�B	�B	�B	�B	�B	�B	�B	�+B	�=B	�JB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�?B	�LB	�LB	�^B	�jB	�qB	�wB	�}B	�}B	��B	��B	B	B	ĜB	ƨB	ǮB	ȴB	ȴB	ȴB	ɺB	ȴB	ǮB	ƨB	ǮB	ȴB	ȴB	ȴB	ȴB	ȴB	ȴB	��B	��B	��B
%22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B}�B}�B}�B}�B}�B}�B}�B}�B~�B~�B~�B}�B~�B}�B}�B}�B}�B� B� B�B~�B}�B}�B}�B�B�VB�?B�`B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BBBB
=B�B�B�B!�B"�B'�B-B5?BA�BI�BQ�BW
B\)BW
BT�B^5Bt�Bu�B�+B�=B�7B�VB�uB��B��B��B�3B�dB�LB�?B%�B�B�B�#B�)B�/B�#B��B��BÖB�?B��B��B�BjBK�BD�B9XB+BbB
�B
�/B
��B
��B
��B
�hB
�B
cTB
J�B
B�B
?}B
=qB
9XB
-B
!�B
DB	��B	�B	�TB	�B	ȴB	��B	�RB	�B	��B	��B	�DB	�B	w�B	l�B	\)B	I�B	=qB	5?B	,B	&�B	#�B	�B	�B	�B	\B	
=B	+B	B��B��B�B�B�sB�`B�NB�BB�;B�5B�)B�#B�B�B��B��B��BŢBB�}B�qB�XB�?B�-B�!B�B��B��B��B��B��B��B��B��B�{B�uB�hB�VB�JB�JB�DB�7B�%B�B�B�B�B}�Bz�Bv�Bt�Bq�Bp�Bo�Bl�Bk�BjBhsBffBe`BgmBjBe`BbNB`BB_;BdZBm�BiyBgmBffBdZBaHB_;B[#BZBXBVBS�BP�BP�BP�BO�BP�BO�BO�BN�BL�BK�BJ�BI�BI�BI�BJ�BJ�BJ�BL�BO�BQ�BVBVBVBVBW
BW
BW
BVBVBVBR�BW
BZB]/B`BBcTBffBdZBdZBcTBbNBcTBbNBcTBe`BjBk�Bl�Bm�Bn�Bq�Bs�Bv�B|�B�7B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�?B�XB�XB�XB�wB��B��BƨB��B��B�;B�NB�fB�yB�B�B�B�B�B��B��B��B��B��B��B	%B		7B	
=B	JB	\B	hB	{B	�B	�B	�B	�B	!�B	"�B	'�B	2-B	9XB	;dB	=qB	>wB	A�B	B�B	?}B	=qB	E�B	I�B	L�B	N�B	O�B	Q�B	T�B	W
B	YB	[#B	\)B	]/B	_;B	aHB	bNB	cTB	cTB	dZB	ffB	ffB	gmB	iyB	l�B	m�B	n�B	p�B	r�B	v�B	x�B	x�B	{�B	~�B	�B	�B	�B	�B	�B	�B	�B	�+B	�=B	�JB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�?B	�LB	�LB	�^B	�jB	�qB	�wB	�}B	�}B	��B	��B	B	B	ĜB	ƨB	ǮB	ȴB	ȴB	ȴB	ɺB	ȴB	ǮB	ƨB	ǮB	ȴB	ȴB	ȴB	ȴB	ȴB	ȴB	��B	��B	��B
%22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.07 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191735                              AO  ARCAADJP                                                                    20181005191735    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191735  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191735  QCF$                G�O�G�O�G�O�8000            