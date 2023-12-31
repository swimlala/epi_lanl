CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       $Woods Hole Oceanographic Institution   source        
Argo float     history       92021-02-17T00:27:27Z creation; 2022-05-04T12:55:30Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         iPRIMARY | https://orcid.org/0000-0001-5113-1068 | Deborah West-Mack, Woods Hole Oceanographic Institution         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7d   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7t   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7x   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7|   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  84   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8d   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8h   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8l   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8p   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            9   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           9   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           9   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9    POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9$   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9,   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :,   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :0   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :4   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :8   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :<   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  a�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qD   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �L   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ˬ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Ϝ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ߄   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20210217002727  20220504085530  1901663 US ARGO PROJECT                                                 BRECK OWENS, STEVE JAYNE, P.E. ROBBINS                          PRES            TEMP            PSAL               �A   AO  5137                            2C  D   S2A                             7179                            SBE602 V1.3                     854 @��R~1   @���M��C��<�@D�a���1   GPS     Primary sampling: averaged [nominal 2 dbar binned data sampled at 0.5 Hz from a SBE41CP]                                                                                                                                                                           A   A   B   ?�=q@�@@  @}p�@�G�@\@�G�A   A  A\)A?\)A`��A\)A��A�  A�  A��AϮA�  A�  B   B(�B  B  B�
B'�
B0  B8  B?�
BH  BP  BX  B_�
Bh(�Bp  Bw�
B�{B�  B��B�  B�{B�  B�  B��B��
B��B�  B��
B��B�  B��B��B�{B�{B�  B�  B��B�  B�{B�{B�{B�  B�{B�{B�  B��B�  B�  C   C
=C
=C
=C
=C

=C  C  C  C  C
=C
=C  C
=C  C��C   C"  C$  C&  C(  C*
=C,
=C.  C0  C2  C4  C6  C8
=C:  C;��C>  C@
=CB{CD
=CF
=CH  CJ  CL  CN
=CP
=CR  CT  CV
=CX  CZ  C\
=C^
=C_��Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp
=Cr  Cs��Cv  Cx  Cz  C|
=C~  C�C�C�C�C�  C�C�  C���C���C���C���C�  C�C�  C�  C�  C���C�  C�C�  C���C�  C�C�C�C�C�C�C�  C���C�  C�  C�  C�C�C�  C�  C�C�C�C�  C�  C�  C���C�  C���C���C�  C�  C�C�  C�C�C�  C���C�  C�C�  C�  C�  C���C���C�C�  C���C�  C�C�  C�  C�  C�  C�C�  C�C�
=C�
=C�C�  C���C���C���C���C�  C�  C�  C�C�  C���C�  C�C�C�C�C���C���C�  C�  C���C�  C�C�  C�C�C�  C���C�  C���C���C�  C���C���C�  C�  C�C�  C���C���C�  C�  C�  C���C�  C�C�C���C�  C�C�  C���D � D  D��D  D}qD  D��D�qD� D�D��D�D��D  D� D�qD}qD	  D	��D	�qD
� D  D� D�D� D  D� D  D� D�qD� DD�D�D��D�D}qD  D��D�D��D  D��D�D��D  D� D  D}qD  D��D  D� D�D��D�qD� D�D� D  D��D�D� D   D }qD!  D!� D!�qD"}qD#  D#� D$  D$� D$�qD%}qD%��D&� D'�D'� D'�qD(� D)�D)� D*�D*��D+D+��D,  D,� D-  D-� D.  D.� D/  D/� D/�qD0}qD1  D1��D2�D2��D3  D3� D4�D4��D5  D5}qD6  D6� D7  D7}qD8  D8}qD9  D9� D:  D:��D;�D;��D<  D<� D=  D=}qD=�qD>� D?�D?� D?�qD@� DA�DA� DB  DB� DB�qDC��DDDD� DD�qDE}qDF  DF� DG  DG}qDG�qDH}qDI  DI� DJ  DJ��DK�DK��DL  DL}qDL�qDM� DN  DN}qDN�qDO� DP  DP� DQ  DQ��DRDR� DR��DSz�DS��DT}qDU  DU��DV  DV��DW  DW}qDX�DX��DY  DY��DY�qDZ� D[  D[� D\  D\��D]�D]��D^  D^� D_  D_}qD_�qD`� Da  Daz�Db  Db}qDb�qDc� Dd  Dd��De  De� Df�Df�Dg�Dg� Dh  Dh� Di  Di}qDj  Dj� Dj�qDk� Dl  Dl� Dm�Dm��Dn  Dn� Do  Do� Dp  Dp}qDq  Dq� Dr  Dr� Dr�qDs� Dt�Dt��Du�Du� Dv  Dv��Dw  Dw��Dx�Dx��Dx�qDy� Dz�Dz� D{  D{� D|�D|��D}�D}��D}�qD~}qD~�qD}qD�  D�@ D�~�D�� D�  D�@ D�� D�� D���D�>�D�� D�� D���D�=qD�� D��HD�  D�>�D�~�D���D���D�>�D��HD��HD�  D�>�D�� D�� D���D�@ D�� D�� D�  D�>�D�~�D���D���D�@ D�� D�� D���D�>�D�~�D�� D���D�@ D�� D���D�  D�AHD�� D�� D�HD�AHD�� D��HD�HD�>�D�� D�� D�  D�@ D�� D���D���D�>�D�~�D���D�  D�@ D�~�D���D�  D�>�D�� D�� D�  D�>�D�� D�� D���D�>�D�� D�� D���D�@ D�� D��qD���D�>�D�~�D�� D�HD�AHD��HD��HD�  D�>�D�� D���D�  D�AHD�� D�� D�HD�@ D�� D���D���D�@ D�� D�� D�  D�AHD��HD���D���D�>�D�~�D���D���D�@ D�~�D�� D�  D�>�D�� D�D�  D�@ D��HD��HD�  D�@ D�� D�� D�  D�@ D�~�D���D�  D�AHD��HD�� D�  D�AHD��HD��HD�  D�@ D�� D�� D�  D�@ D�� D��HD�  D�@ D��HD��HD�  D�>�D�~�D�� D�  D�@ D�� D��HD�  D�>�D�~�D�� D���D�>�D�� D��qD���D�@ D�� D�� D���D�@ D��HD�� D�  D�AHD��HD��HD�HD�@ D�~�D�� D���D�@ D��HD��HD�HD�@ D�� D��HD�  D�>�D�� D�D�  D�@ D�� D�� D�  D�>�D�~�D���D�  D�AHD�� D�� D�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�~�D���D�  D�@ D�� D��HD�HD�@ D�� D�� D�  D�@ D�� D�� D�HD�AHD�� D�� D�  D�@ D�� D�� D�  D�AHD��HD�� D���D�@ D�� D��HD�  D�>�D�� D��HD�  D�@ D�~�D�� D�  D�>�DÀ D�� D�HD�AHDāHD�� D�  D�@ Dŀ D�� D�  D�@ D�~�D�� D�  D�@ D�~�D�� D�  D�AHDȁHD��HD�  D�>�Dɀ D�� D�  D�@ Dʀ D�� D�HD�@ Dˀ D�� D���D�AHD̀ D�� D�  D�>�D̀ D;�D�  D�@ D΀ D��HD�  D�>�Dπ D�� D���D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ DҾ�D�  D�@ D�~�D�� D�  D�@ DԀ D��HD�HD�AHDՀ Dվ�D�  D�>�Dր D�� D�  D�@ D׀ D��HD�HD�@ D�~�Dؾ�D���D�@ DفHD��HD�HD�AHDڀ Dھ�D�  D�AHDۀ D�� D�HD�AHD܀ D�� D�  D�>�D�~�D��HD�HD�@ Dހ D�� D���D�>�D߁HD��HD���D�>�D�� DྸD���D�@ D� D�� D�HD�AHD� D�� D���D�@ D� D㾸D�  D�AHD�HD�� D���D�>�D�HD��HD�HD�@ D� D�� D�  D�>�D�~�D�� D�  D�@ D� D��HD�HD�@ D� D��HD�HD�@ D�HD�� D���D�@ D� D�� D�  D�@ D�HD��HD�HD�@ D� D��HD�  D�@ D�~�DD�  D�@ D�~�DﾸD���D�@ D�� D�� D�  D�B�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�HD�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�>�D�~�D�� D�  D�@ D�� D�� D��D�1�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?�=q@�@@  @}p�@�G�@\@�G�A   A  A\)A?\)A`��A\)A��A�  A�  A��AϮA�  A�  B   B(�B  B  B�
B'�
B0  B8  B?�
BH  BP  BX  B_�
Bh(�Bp  Bw�
B�{B�  B��B�  B�{B�  B�  B��B��
B��B�  B��
B��B�  B��B��B�{B�{B�  B�  B��B�  B�{B�{B�{B�  B�{B�{B�  B��B�  B�  C   C
=C
=C
=C
=C

=C  C  C  C  C
=C
=C  C
=C  C��C   C"  C$  C&  C(  C*
=C,
=C.  C0  C2  C4  C6  C8
=C:  C;��C>  C@
=CB{CD
=CF
=CH  CJ  CL  CN
=CP
=CR  CT  CV
=CX  CZ  C\
=C^
=C_��Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp
=Cr  Cs��Cv  Cx  Cz  C|
=C~  C�C�C�C�C�  C�C�  C���C���C���C���C�  C�C�  C�  C�  C���C�  C�C�  C���C�  C�C�C�C�C�C�C�  C���C�  C�  C�  C�C�C�  C�  C�C�C�C�  C�  C�  C���C�  C���C���C�  C�  C�C�  C�C�C�  C���C�  C�C�  C�  C�  C���C���C�C�  C���C�  C�C�  C�  C�  C�  C�C�  C�C�
=C�
=C�C�  C���C���C���C���C�  C�  C�  C�C�  C���C�  C�C�C�C�C���C���C�  C�  C���C�  C�C�  C�C�C�  C���C�  C���C���C�  C���C���C�  C�  C�C�  C���C���C�  C�  C�  C���C�  C�C�C���C�  C�C�  C���D � D  D��D  D}qD  D��D�qD� D�D��D�D��D  D� D�qD}qD	  D	��D	�qD
� D  D� D�D� D  D� D  D� D�qD� DD�D�D��D�D}qD  D��D�D��D  D��D�D��D  D� D  D}qD  D��D  D� D�D��D�qD� D�D� D  D��D�D� D   D }qD!  D!� D!�qD"}qD#  D#� D$  D$� D$�qD%}qD%��D&� D'�D'� D'�qD(� D)�D)� D*�D*��D+D+��D,  D,� D-  D-� D.  D.� D/  D/� D/�qD0}qD1  D1��D2�D2��D3  D3� D4�D4��D5  D5}qD6  D6� D7  D7}qD8  D8}qD9  D9� D:  D:��D;�D;��D<  D<� D=  D=}qD=�qD>� D?�D?� D?�qD@� DA�DA� DB  DB� DB�qDC��DDDD� DD�qDE}qDF  DF� DG  DG}qDG�qDH}qDI  DI� DJ  DJ��DK�DK��DL  DL}qDL�qDM� DN  DN}qDN�qDO� DP  DP� DQ  DQ��DRDR� DR��DSz�DS��DT}qDU  DU��DV  DV��DW  DW}qDX�DX��DY  DY��DY�qDZ� D[  D[� D\  D\��D]�D]��D^  D^� D_  D_}qD_�qD`� Da  Daz�Db  Db}qDb�qDc� Dd  Dd��De  De� Df�Df�Dg�Dg� Dh  Dh� Di  Di}qDj  Dj� Dj�qDk� Dl  Dl� Dm�Dm��Dn  Dn� Do  Do� Dp  Dp}qDq  Dq� Dr  Dr� Dr�qDs� Dt�Dt��Du�Du� Dv  Dv��Dw  Dw��Dx�Dx��Dx�qDy� Dz�Dz� D{  D{� D|�D|��D}�D}��D}�qD~}qD~�qD}qD�  D�@ D�~�D�� D�  D�@ D�� D�� D���D�>�D�� D�� D���D�=qD�� D��HD�  D�>�D�~�D���D���D�>�D��HD��HD�  D�>�D�� D�� D���D�@ D�� D�� D�  D�>�D�~�D���D���D�@ D�� D�� D���D�>�D�~�D�� D���D�@ D�� D���D�  D�AHD�� D�� D�HD�AHD�� D��HD�HD�>�D�� D�� D�  D�@ D�� D���D���D�>�D�~�D���D�  D�@ D�~�D���D�  D�>�D�� D�� D�  D�>�D�� D�� D���D�>�D�� D�� D���D�@ D�� D��qD���D�>�D�~�D�� D�HD�AHD��HD��HD�  D�>�D�� D���D�  D�AHD�� D�� D�HD�@ D�� D���D���D�@ D�� D�� D�  D�AHD��HD���D���D�>�D�~�D���D���D�@ D�~�D�� D�  D�>�D�� D�D�  D�@ D��HD��HD�  D�@ D�� D�� D�  D�@ D�~�D���D�  D�AHD��HD�� D�  D�AHD��HD��HD�  D�@ D�� D�� D�  D�@ D�� D��HD�  D�@ D��HD��HD�  D�>�D�~�D�� D�  D�@ D�� D��HD�  D�>�D�~�D�� D���D�>�D�� D��qD���D�@ D�� D�� D���D�@ D��HD�� D�  D�AHD��HD��HD�HD�@ D�~�D�� D���D�@ D��HD��HD�HD�@ D�� D��HD�  D�>�D�� D�D�  D�@ D�� D�� D�  D�>�D�~�D���D�  D�AHD�� D�� D�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�~�D���D�  D�@ D�� D��HD�HD�@ D�� D�� D�  D�@ D�� D�� D�HD�AHD�� D�� D�  D�@ D�� D�� D�  D�AHD��HD�� D���D�@ D�� D��HD�  D�>�D�� D��HD�  D�@ D�~�D�� D�  D�>�DÀ D�� D�HD�AHDāHD�� D�  D�@ Dŀ D�� D�  D�@ D�~�D�� D�  D�@ D�~�D�� D�  D�AHDȁHD��HD�  D�>�Dɀ D�� D�  D�@ Dʀ D�� D�HD�@ Dˀ D�� D���D�AHD̀ D�� D�  D�>�D̀ D;�D�  D�@ D΀ D��HD�  D�>�Dπ D�� D���D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ DҾ�D�  D�@ D�~�D�� D�  D�@ DԀ D��HD�HD�AHDՀ Dվ�D�  D�>�Dր D�� D�  D�@ D׀ D��HD�HD�@ D�~�Dؾ�D���D�@ DفHD��HD�HD�AHDڀ Dھ�D�  D�AHDۀ D�� D�HD�AHD܀ D�� D�  D�>�D�~�D��HD�HD�@ Dހ D�� D���D�>�D߁HD��HD���D�>�D�� DྸD���D�@ D� D�� D�HD�AHD� D�� D���D�@ D� D㾸D�  D�AHD�HD�� D���D�>�D�HD��HD�HD�@ D� D�� D�  D�>�D�~�D�� D�  D�@ D� D��HD�HD�@ D� D��HD�HD�@ D�HD�� D���D�@ D� D�� D�  D�@ D�HD��HD�HD�@ D� D��HD�  D�@ D�~�DD�  D�@ D�~�DﾸD���D�@ D�� D�� D�  D�B�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�HD�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�>�D�~�D�� D�  D�@ D�� D�� D��D�1�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AmO�AmO�AmO�AmO�AmS�AmXAm\)AmO�AmO�Am\)Am`BAmhsAml�Amp�Amp�Aml�Aml�Amp�Amt�Amx�Amx�Amx�Amx�Am|�Am�Am�Amx�Amp�Amx�Amx�Am�Amx�Am�Amp�AmO�AmXAmO�AmG�Am7LAm%Ak��Ai��Ag
=Af�!Af��Af�\AfVAf5?Ae�Aex�AdjAc�-Ab��Aa`BA`�A`=qA`{A_ƨA_O�A_;dA_+A_+A^�A^��A]�A\jA[oAY�AY��AY��AZE�A[�A]�FA]��A]�A\ȴA\�!A\A[�A[?}A[�AZ�/AZI�AZ1AY�mAYS�AX��AX�\AX~�AX�AWp�AV�`AU��AT�AS&�APVAN��ALM�AKO�AKVAJ�HAJv�AJ�\AJ��AJJAI�-AI�TAI��AIAI?}AH��AH�AH  AG�wAG�AFVADbAC�AB{A@�`A@bNA?�
A?��A>�RA=��A=`BA<��A<=qA;�#A;��A;�A:��A;�PA;��A;A;`BA;oA:��A:�/A:�jA:�A:M�A:9XA:(�A9�
A9��A9�A9dZA8��A8JA7�mA7�#A7�
A7�PA6jA5�mA5��A5XA5
=A4��A4jA3�wA2�A2��A2��A2{A1l�A0�HA0�9A0�+A/��A/33A.�yA.bNA.(�A.  A-�A,VA+K�A*bNA)�;A)?}A(��A(�A(�HA(v�A(�A'G�A&��A&jA&1'A%�A$�A$9XA$A#��A"Q�A"�A"��A"A�A!�A!p�A!7LA �Al�A"�A+A��A�-A|�AE�A��At�A~�AffAJA�-A&�A�AG�A?}A�A�AĜA�uAQ�A �AA��A�mA��A�wA��AS�A7LA�AȴA~�AbA�A�AjA��At�A;dAA~�A  A��A�A�wA��A��Ax�AK�A/A
��A
n�A
A�A	�A	K�Av�A  A�A�#A��AdZA7LAoA��A�9A(�A`BAG�A�jA�7A;dA�A�AS�A�A M�@��y@��F@���@�@�p�@��;@�O�@���@�D@��@�@�\)@�\@���@���@�bN@��@�K�@�5?@���@�h@��@��@�t�@�33@�!@��#@�A�@�\)@�
=@��@�ȴ@�ȴ@�+@�@�-@�(�@�S�@ݺ^@١�@�Z@׶F@�o@֗�@���@ա�@�`B@�&�@���@�b@���@��#@��`@�b@�v�@��@�"�@���@ʟ�@ɉ7@��@���@�I�@�C�@���@Ĵ9@��H@�-@�X@��u@��@���@�\)@�dZ@��@��@�K�@���@��R@�M�@�J@���@��@�%@��9@�Z@���@�"�@�M�@�7L@���@�b@�l�@��@�@�Ĝ@�1@��@���@���@�x�@�`B@�X@��`@�+@�-@��^@���@�A�@� �@���@��@�=q@�p�@�/@��@��@�Q�@�l�@���@��@���@��w@��\@��@���@���@��h@��/@��@���@�~�@�J@�X@�A�@���@�t�@�"�@��R@�=q@�@��@�`B@�/@��`@��@��u@��@�r�@�Q�@�ƨ@�dZ@�33@��@���@��@���@�hs@�G�@�/@��@��@�V@��`@���@�Q�@�1@��@�C�@�@���@��R@�M�@�{@���@��-@�r�@� �@� �@�b@�b@���@��@�
=@���@�ff@�=q@�@��-@��7@�V@���@��D@�r�@�(�@�1@��@�C�@��y@���@���@��\@��+@�V@��@��T@���@�x�@�7L@�7L@�/@�/@�%@��@�V@���@���@���@���@�l�@�dZ@�33@��y@���@�^5@�5?@���@��#@���@��^@�hs@�?}@���@��
@�S�@��y@���@�E�@��@��7@�hs@�/@��/@���@�I�@�1'@�b@��@K�@;d@+@~��@~E�@}@}?}@|I�@{�F@{��@{"�@z^5@yhs@x��@xĜ@x�u@xA�@w�;@wl�@v��@v{@uO�@tj@sS�@s@r~�@q��@q7L@pbN@o�@o�@o�@o�w@p  @pA�@o�@o|�@o;d@o+@o\)@o\)@o;d@o
=@n��@m@l�D@l9X@l1@k��@k�
@k��@kdZ@kS�@kS�@kC�@j��@j=q@i��@i��@i��@iX@h�9@hbN@g�@e�T@dz�@c��@c�F@d9X@d�j@d1@c��@dj@d�@d��@d�/@e/@ep�@eV@c��@b�@b�!@a��@`r�@_�@^��@]O�@]�h@]�h@]?}@]�h@]�T@]��@\��@^�+@^��@_�;@`��@`bN@`1'@`b@_��@_|�@_\)@^ȴ@]�T@]�h@]p�@]`B@]O�@]O�@]/@\�j@\z�@\I�@[�m@Z��@ZM�@Z-@Y�#@YG�@YG�@Y�@X�`@X��@XA�@W�@W|�@V��@Vȴ@Vȴ@Vȴ@V�@V�y@V��@Vȴ@V{@U`B@U`B@U?}@T��@T�@S��@S�F@SdZ@SS�@SdZ@S��@S�
@RJ@Q��@Q�^@Q&�@P�u@O�;@O��@O��@O+@M@M��@Mp�@M�@Mp�@L�@L1@Kt�@K"�@Ko@K@J��@JJ@IX@I7L@I7L@I�@H�9@HbN@HQ�@HQ�@HA�@H1'@G�;@G+@FE�@F�y@Gl�@G�w@G�@H��@H��@H�@G�;@Gl�@GK�@F��@F��@F�y@Fȴ@FE�@FE�@FV@FE�@F$�@FE�@Fff@FV@F{@E��@E�-@E�h@E�@D�@D��@D9X@C��@C�m@C�F@C�F@C��@C��@CdZ@C"�@C@B�@B�H@B��@B��@B�\@Bn�@B�@A��@A��@AX@@�9@@�@@�@@�@@�@@r�@@Q�@@Q�@@ �@?�@?��@?|�@?l�@?\)@?\)@?K�@?�@>�y@>��@>v�@>ff@>@=�h@=`B@=`B@=`B@=O�@=�@<��@<�D@<�D@<Z@<�@<1@;ƨ@;ƨ@;�
@;�
@;ƨ@<9X@;�
@;��@;t�@;o@:�H@:�H@:�@;@:�@:�H@:�H@:�H@:�H@:��@:��@:n�@:=q@9�@9x�@9X@9G�@9&�@9&�@9�@9%@8��@8�9@8�u@8r�@8bN@81'@8 �@8b@7�@7|�@7l�@7K�@7;d@7�@6��@6�@6ȴ@6��@6v�@6E�@6{@5�T@5@5��@5��@5�h@5�@5`B@5`B@5`B@5/@4�@4��@4�j@4��@4�D@4z�@4j@4I�@49X@4(�@41@3�m@3�F@3��@3�@3dZ@3"�@3o@2�@2�H@2�H@2�!@2�!@2��@2M�@2�@1��@1��@1�^@1�7@1x�@1hs@1X@1G�@17L@1%@1%@1%@0��@0�`@1%@0��@0��@0�u@0r�@0Q�@0Q�@0Q�@0Q�@0A�@01'@01'@0b@/�;@/��@/�w@/�@/��@/��@/��@/|�@/K�@/;d@/+@/�@/�@/
=@/
=@.��@.�@.ȴ@.ȴ@.��@.ff@.V@.5?@.{@-��@-�-@-��@-�@-�@-p�@-p�@-p�@-p�@-?}@-�@,��@,�@,�@,�@,�@,�@,��@,��@,��@,�/@,�j@,�@,�D@,z�@,Z@,Z@,I�@,Z@,I�@,(�@+�m@+ƨ@+ƨ@+ƨ@+�F@+�F@+�F@+ƨ@+ƨ@+ƨ@+ƨ@+�F@+�F@+��@+dZ@+t�@+dZ@+dZ@+S�@+C�@+C�@+C�@+C�@+33@+33@+33@+"�@+"�@+"�@+o@+@*�@*�H@*��@*��@*�!@*��@*��@*��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AmO�AmO�AmO�AmO�AmS�AmXAm\)AmO�AmO�Am\)Am`BAmhsAml�Amp�Amp�Aml�Aml�Amp�Amt�Amx�Amx�Amx�Amx�Am|�Am�Am�Amx�Amp�Amx�Amx�Am�Amx�Am�Amp�AmO�AmXAmO�AmG�Am7LAm%Ak��Ai��Ag
=Af�!Af��Af�\AfVAf5?Ae�Aex�AdjAc�-Ab��Aa`BA`�A`=qA`{A_ƨA_O�A_;dA_+A_+A^�A^��A]�A\jA[oAY�AY��AY��AZE�A[�A]�FA]��A]�A\ȴA\�!A\A[�A[?}A[�AZ�/AZI�AZ1AY�mAYS�AX��AX�\AX~�AX�AWp�AV�`AU��AT�AS&�APVAN��ALM�AKO�AKVAJ�HAJv�AJ�\AJ��AJJAI�-AI�TAI��AIAI?}AH��AH�AH  AG�wAG�AFVADbAC�AB{A@�`A@bNA?�
A?��A>�RA=��A=`BA<��A<=qA;�#A;��A;�A:��A;�PA;��A;A;`BA;oA:��A:�/A:�jA:�A:M�A:9XA:(�A9�
A9��A9�A9dZA8��A8JA7�mA7�#A7�
A7�PA6jA5�mA5��A5XA5
=A4��A4jA3�wA2�A2��A2��A2{A1l�A0�HA0�9A0�+A/��A/33A.�yA.bNA.(�A.  A-�A,VA+K�A*bNA)�;A)?}A(��A(�A(�HA(v�A(�A'G�A&��A&jA&1'A%�A$�A$9XA$A#��A"Q�A"�A"��A"A�A!�A!p�A!7LA �Al�A"�A+A��A�-A|�AE�A��At�A~�AffAJA�-A&�A�AG�A?}A�A�AĜA�uAQ�A �AA��A�mA��A�wA��AS�A7LA�AȴA~�AbA�A�AjA��At�A;dAA~�A  A��A�A�wA��A��Ax�AK�A/A
��A
n�A
A�A	�A	K�Av�A  A�A�#A��AdZA7LAoA��A�9A(�A`BAG�A�jA�7A;dA�A�AS�A�A M�@��y@��F@���@�@�p�@��;@�O�@���@�D@��@�@�\)@�\@���@���@�bN@��@�K�@�5?@���@�h@��@��@�t�@�33@�!@��#@�A�@�\)@�
=@��@�ȴ@�ȴ@�+@�@�-@�(�@�S�@ݺ^@١�@�Z@׶F@�o@֗�@���@ա�@�`B@�&�@���@�b@���@��#@��`@�b@�v�@��@�"�@���@ʟ�@ɉ7@��@���@�I�@�C�@���@Ĵ9@��H@�-@�X@��u@��@���@�\)@�dZ@��@��@�K�@���@��R@�M�@�J@���@��@�%@��9@�Z@���@�"�@�M�@�7L@���@�b@�l�@��@�@�Ĝ@�1@��@���@���@�x�@�`B@�X@��`@�+@�-@��^@���@�A�@� �@���@��@�=q@�p�@�/@��@��@�Q�@�l�@���@��@���@��w@��\@��@���@���@��h@��/@��@���@�~�@�J@�X@�A�@���@�t�@�"�@��R@�=q@�@��@�`B@�/@��`@��@��u@��@�r�@�Q�@�ƨ@�dZ@�33@��@���@��@���@�hs@�G�@�/@��@��@�V@��`@���@�Q�@�1@��@�C�@�@���@��R@�M�@�{@���@��-@�r�@� �@� �@�b@�b@���@��@�
=@���@�ff@�=q@�@��-@��7@�V@���@��D@�r�@�(�@�1@��@�C�@��y@���@���@��\@��+@�V@��@��T@���@�x�@�7L@�7L@�/@�/@�%@��@�V@���@���@���@���@�l�@�dZ@�33@��y@���@�^5@�5?@���@��#@���@��^@�hs@�?}@���@��
@�S�@��y@���@�E�@��@��7@�hs@�/@��/@���@�I�@�1'@�b@��@K�@;d@+@~��@~E�@}@}?}@|I�@{�F@{��@{"�@z^5@yhs@x��@xĜ@x�u@xA�@w�;@wl�@v��@v{@uO�@tj@sS�@s@r~�@q��@q7L@pbN@o�@o�@o�@o�w@p  @pA�@o�@o|�@o;d@o+@o\)@o\)@o;d@o
=@n��@m@l�D@l9X@l1@k��@k�
@k��@kdZ@kS�@kS�@kC�@j��@j=q@i��@i��@i��@iX@h�9@hbN@g�@e�T@dz�@c��@c�F@d9X@d�j@d1@c��@dj@d�@d��@d�/@e/@ep�@eV@c��@b�@b�!@a��@`r�@_�@^��@]O�@]�h@]�h@]?}@]�h@]�T@]��@\��@^�+@^��@_�;@`��@`bN@`1'@`b@_��@_|�@_\)@^ȴ@]�T@]�h@]p�@]`B@]O�@]O�@]/@\�j@\z�@\I�@[�m@Z��@ZM�@Z-@Y�#@YG�@YG�@Y�@X�`@X��@XA�@W�@W|�@V��@Vȴ@Vȴ@Vȴ@V�@V�y@V��@Vȴ@V{@U`B@U`B@U?}@T��@T�@S��@S�F@SdZ@SS�@SdZ@S��@S�
@RJ@Q��@Q�^@Q&�@P�u@O�;@O��@O��@O+@M@M��@Mp�@M�@Mp�@L�@L1@Kt�@K"�@Ko@K@J��@JJ@IX@I7L@I7L@I�@H�9@HbN@HQ�@HQ�@HA�@H1'@G�;@G+@FE�@F�y@Gl�@G�w@G�@H��@H��@H�@G�;@Gl�@GK�@F��@F��@F�y@Fȴ@FE�@FE�@FV@FE�@F$�@FE�@Fff@FV@F{@E��@E�-@E�h@E�@D�@D��@D9X@C��@C�m@C�F@C�F@C��@C��@CdZ@C"�@C@B�@B�H@B��@B��@B�\@Bn�@B�@A��@A��@AX@@�9@@�@@�@@�@@�@@r�@@Q�@@Q�@@ �@?�@?��@?|�@?l�@?\)@?\)@?K�@?�@>�y@>��@>v�@>ff@>@=�h@=`B@=`B@=`B@=O�@=�@<��@<�D@<�D@<Z@<�@<1@;ƨ@;ƨ@;�
@;�
@;ƨ@<9X@;�
@;��@;t�@;o@:�H@:�H@:�@;@:�@:�H@:�H@:�H@:�H@:��@:��@:n�@:=q@9�@9x�@9X@9G�@9&�@9&�@9�@9%@8��@8�9@8�u@8r�@8bN@81'@8 �@8b@7�@7|�@7l�@7K�@7;d@7�@6��@6�@6ȴ@6��@6v�@6E�@6{@5�T@5@5��@5��@5�h@5�@5`B@5`B@5`B@5/@4�@4��@4�j@4��@4�D@4z�@4j@4I�@49X@4(�@41@3�m@3�F@3��@3�@3dZ@3"�@3o@2�@2�H@2�H@2�!@2�!@2��@2M�@2�@1��@1��@1�^@1�7@1x�@1hs@1X@1G�@17L@1%@1%@1%@0��@0�`@1%@0��@0��@0�u@0r�@0Q�@0Q�@0Q�@0Q�@0A�@01'@01'@0b@/�;@/��@/�w@/�@/��@/��@/��@/|�@/K�@/;d@/+@/�@/�@/
=@/
=@.��@.�@.ȴ@.ȴ@.��@.ff@.V@.5?@.{@-��@-�-@-��@-�@-�@-p�@-p�@-p�@-p�@-?}@-�@,��@,�@,�@,�@,�@,�@,��@,��@,��@,�/@,�j@,�@,�D@,z�@,Z@,Z@,I�@,Z@,I�@,(�@+�m@+ƨ@+ƨ@+ƨ@+�F@+�F@+�F@+ƨ@+ƨ@+ƨ@+ƨ@+�F@+�F@+��@+dZ@+t�@+dZ@+dZ@+S�@+C�@+C�@+C�@+C�@+33@+33@+33@+"�@+"�@+"�@+o@+@*�@*�H@*��@*��@*�!@*��@*��@*��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B[#B[#B[#B[#B[#B[#B[#B[#B[#BZB[#BZBZBZBZBZBYBXBXBW
BT�BP�BB�B-BuBVBPBJB	7B+BB��B�B�mB�/B��BȴBŢBĜBǮBÖBB��B��B��BÖB�-B��B��B�7B�DB�\B��B��B�ZB�B�B�B�B�B�B�B�B�B�B��B��B�B��B��B��B��B�B�B�TB�BĜB��B�oB~�Bw�Bv�Bu�Bv�Bx�By�By�B|�B� B�B�B~�B{�By�Bs�Bp�Bl�B^5BF�B?}B33B+B%�B �B�B�BVB	7B%BB��B��B��B��BB
=B	7B+BBBBBB��B��B��B��B��B��B��B�B�mB�fB�`B�ZB�;B��B��B��BȴBŢBB�wB�RB�-B�3B�'B�B��B��B��B��B��B�\B�PB�=B�+B�B~�Bs�BjBaHB\)BW
BVBT�BT�BP�BL�BG�BC�BA�B?}B;dB5?B0!B.B)�B"�B)�B,B'�B�B�B�BuBPBDB
=BB
��B
�TB
��B
ŢB
�?B
�B
�B
�B
��B
y�B
��B
� B
o�B
n�B
t�B
y�B
�B
�\B
��B
��B
�B
�3B
�FB
�FB
�FB
�FB
�FB
�FB
�?B
�3B
�'B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
�oB
�hB
�bB
�bB
�bB
�VB
�PB
�JB
�=B
�1B
�%B
�B
}�B
|�B
|�B
}�B
|�B
|�B
{�B
z�B
y�B
w�B
t�B
p�B
n�B
jB
e`B
bNB
^5B
YB
W
B
S�B
N�B
G�B
?}B
>wB
=qB
;dB
5?B
0!B
.B
-B
+B
(�B
&�B
"�B
�B
�B
�B
�B
�B
uB
oB
hB
\B
PB
JB
DB
	7B
+B
B
B
B
B
B
  B	��B	��B	��B	��B	�B	�B	�ZB	�NB	�HB	�;B	�/B	�)B	�#B	�B	�B	�
B	��B	��B	��B	��B	��B	ǮB	ÖB	��B	��B	�}B	�qB	�qB	�qB	�dB	�RB	�9B	�'B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�!B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�'B	�'B	�'B	�-B	�-B	�9B	�FB	�LB	�RB	�RB	�RB	�RB	�XB	�XB	�XB	�dB	�jB	�qB	�qB	�wB	��B	B	ĜB	ƨB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	��B	�B	�B	�
B	�B	�/B	�;B	�HB	�HB	�NB	�`B	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
%B
	7B

=B

=B
	7B
	7B
	7B
1B
1B
	7B
	7B
1B
	7B

=B
DB
JB
VB
oB
{B
�B
�B
�B
!�B
"�B
"�B
"�B
"�B
!�B
!�B
 �B
!�B
!�B
#�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
)�B
+B
+B
,B
-B
/B
2-B
49B
5?B
6FB
6FB
7LB
9XB
:^B
:^B
;dB
<jB
=qB
=qB
=qB
=qB
>wB
?}B
?}B
@�B
@�B
@�B
A�B
B�B
B�B
B�B
D�B
F�B
H�B
I�B
J�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
L�B
L�B
L�B
M�B
N�B
S�B
VB
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
VB
VB
YB
]/B
`BB
bNB
cTB
ffB
hsB
jB
l�B
n�B
n�B
p�B
r�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
w�B
x�B
x�B
x�B
y�B
z�B
{�B
|�B
|�B
}�B
� B
� B
�B
�B
�B
�B
�B
�B
�%B
�+B
�1B
�1B
�1B
�7B
�=B
�=B
�JB
�JB
�PB
�\B
�hB
�hB
�oB
�oB
�oB
�oB
�uB
�uB
�uB
�{B
�{B
�{B
�{B
�{B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�-B
�3B
�9B
�9B
�9B
�9B
�?B
�FB
�LB
�RB
�RB
�RB
�XB
�XB
�^B
�dB
�qB
�qB
�qB
�wB
�}B
��B
��B
B
B
ÖB
ĜB
ŢB
ƨB
ǮB
ȴB
ȴB
ȴB
ɺB
ɺB
ɺB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�
B
�
B
�
B
�B
�B
�B
�B
�B
�B
�#B
�#B
�)B
�)B
�/B
�/B
�5B
�5B
�;B
�;B
�BB
�HB
�NB
�TB
�TB
�ZB
�ZB
�`B
�fB
�mB
�sB
�sB
�sB
�sB
�sB
�yB
�yB
�yB
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B  B  B  B  B  BBBBBBBBBBBBB%B%B+B+B+B1B1B1B	7B	7B	7B
=BDBDBJBJBPBPBPBPBPBVBVBVBVB\B\BbBbBhBhBhBoBoBo1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144444444444444444444444111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B\/B\/B\'B\B\B\&B\^B\!B[�B\B\B\B\ B\-B\6B\-B\B\B[B[$B["B[$B[B[B[&B[<B[7BZB[!BZBZ1BZ	BZLBZwBY
BX*BX(BWCBU�BT*BH�B3�B�B�B�B�B	�B�B[B��B�B��B��B��B��B�"BŀB��B��B¸B��B�0BBǍB�`B��B�AB��B�4B��B�,B��B�}B��B�B�B�wB�B�cB�B�tB�IB�sB�+B�HB�TB�UB�B��B��B�LB�)B�B��B�+B�B��B��Bx�BwNBv�Bv�Bx�B{RBz�B|qB�B��B�rB�B|�B{�Bt�BqfBo�BdhBH�BCjB6�B,�B'dB!oB 9B�BB
�BBB��B�>B��B��B=B
]B
JBBwBfB{B�B�B�5B�*B��B�tB�4B�2B��B�B��B�B�xB�BB�6BՊBϣB̤BɔB�[BøB�tB�zB��B��B��B��B�nB�\B�DB�2B��B�VB��B��B��B��B�kBv�BmBb�B]�BW�BV6BU2BV4BRBOBIhBDXBB;B@�B=�B7>B0�B/yB-dB"nB)�B-B)�B BwB�BxB.BDB�B�B
�:G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�$B
��B
��B
�-B
�B
��B
��B
��B
��B
��B
��B
��B
��B
�UB
�^B
��B
��B
��B
��B
�|B
��B
��B
��B
�B
��B
��B
�7B
�B
�jB
]B
}$B
}CB
~�B
}�B
}pB
|[B
{5B
z�B
yvB
v�B
qB
pJB
m�B
fB
d~B
a B
Y�B
W�B
V@B
QzB
LBB
AB
?qB
>fB
=�B
8�B
0�B
.�B
-�B
+�B
)aB
(FB
&�B
 :B
bB
B
�B
B
B
�B
aB
�B
HB
�B
B

�B
	�B
tB
�B
[B
&B
B
 nB	��B	��B	�!B	�/B	�BB	�XB	�B	�\B	�CB	�B	�B	ܸB	ېB	�B	٫B	�9B	֮B	ծB	�nB	�6B	�=B	��B	�4B	��B	�&B	�B	�!B	��B	�xB	��B	��B	�B	��B	�CB	�VB	�8B	�B	��B	�fB	��B	��B	�dB	��B	��B	�SB	��B	��B	�tB	��B	��B	��B	��B	�B	��B	�HB	��B	��B	��B	��B	�B	�DB	�QB	�B	�vB	�vB	�8B	�'B	�B	��B	��B	�[B	�HB	�tB	��B	�~B	��B	�kB	��B	��B	��B	�B	�B	�B	�BB	�B	��B	��B	�DB	��B	��B	��B	�B	�B	�B	��B	�>B	�LB	��B	��B	��B	�xB	��B	�B	�UB	�~B	��B	�=B	��B	�B	�,B	�SB	�/B	�B	��B	��B	�B	��B	�uB	�0B	�FB	�]B	��B	�cB	�/B	�B	�B	�B	��B	�B	�3B	�bB	�fB	�gB	��B	�LB	�CB	��B	�AB	�~B	�9B	�CB	�,B	��B	�`B	��B	��B	��B	�B	��B	��B	�xB	�CB	�B	�,B	�LB	� B	��B	��B	�B	�B	�bB	�,B	��B	��B	��B	�hB	�B	�(B	�B	�UB	�eB	�jB	�xB	�WB	�tB	�B	�B	�B	�QB	��B	� B	�:B	��B	�CB	�lB	�fB	�.B	�gB	��B	��B	��B	�kB	��B	�aB	�JB	�OB	��B	�~B	�8B	�tB	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	¦B	ĹB	�!B	��B	�!B	�(B	�}B	�7B	��B	�+B	�pB	ΔB	�6B	�
B	�B	�$B	�4B	�BB	�OB	ҩB	ԘB	ԺB	��B	�NB	�qB	�yB	קB	ֱB	�fB	�:B	�B	�B	��B	�B	߭B	�vB	�B	�dB	�BB	�rB	�B	�B	��B	�*B	�|B	��B	��B	��B	��B	��B	��B	�B	�B	�B	� B	�"B	��B	��B	��B	�B	�EB	�B	��B	��B	��B	�:B	��B	�vB	��B	�jB	�2B	�]B	��B
$B
B
�B
	B

�B
B

B
	zB
	�B
	lB
�B

IB

*B
B
	@B

~B
B
B
�B
�B
EB
2B
 B

B
" B
"�B
"�B
#B
#B
!�B
"FB
!�B
"B
!�B
#�B
$�B
$�B
%B
%@B
&B
'B
'GB
(�B
)bB
)B
)?B
)kB
*B
+,B
+1B
,DB
-dB
/�B
2ZB
4�B
5kB
6JB
6KB
7FB
9PB
:UB
:�B
;�B
<�B
={B
=�B
=�B
>B
>�B
?�B
?�B
@�B
@yB
@YB
AzB
C�B
B�B
B�B
EB
G$B
IFB
I�B
J�B
J#B
J�B
J�B
J�B
J�B
J�B
K^B
KKB
K>B
L
B
L�B
L�B
M#B
NNB
OqB
TB
VB
W*B
W`B
WOB
WB
WB
WB
W!B
WPB
V�B
V�B
X�B
\�B
`B
b"B
b�B
f-B
h�B
kB
l�B
n�B
n�B
p�B
r�B
s�B
uB
t�B
t�B
t�B
t�B
u�B
w�B
x�B
yB
yB
y�B
{B
|BB
}B
}6B
~JB
�2B
�B
�/B
�B
�B
� B
�BB
�RB
�DB
�<B
�@B
�CB
�GB
�^B
�YB
��B
�bB
�yB
��B
��B
��B
�lB
�pB
�rB
�B
��B
�zB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�xB
�B
�B
�B
�4B
�B
��B
��B
��B
�B
�B
�B
�B
�B
�&B
�+B
�>B
�BB
�cB
�{B
�IB
�BB
�SB
�>B
�IB
�PB
�iB
�cB
�jB
�lB
�eB
�~B
�cB
�kB
��B
��B
�B
��B
��B
��B
��B
��B
��B
©B
¹B
��B
��B
��B
��B
��B
ȴB
��B
��B
��B
ɻB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�6B
�B
�&B
�B
�B
�6B
�B
�#B
�UB
�EB
�>B
�MB
�9B
�VB
�>B
�@B
�AB
�FB
�KB
�fB
�GB
�JB
�\B
�_B
�?B
�hB
�|B
�B
�B
�B
�sB
�vB
�wB
�B
�B
�{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��B
�B
�B
�B
�B
�B
�B
�B
��B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
�B
��B
��B
��B
��B
��B
��B
��B B B B B !BB'BBBB!B1BHB5BBB/B$B)BB.B.B1B=B8B	RB	_B	4B
LBFBVBXBNBMBQB^BRBRBbBZBWBjBfBlBnBsBvBwBxBqBoBs1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144444444444444444444444111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�&<#�&<#�<#׺<#�{<#�<#ߜ<#�<<#��<#�X<#�<#�X<#�I<#�<#׎<#�<#�X<#�{<#�<#�<#�<#�<#�i<#�I<#�<#��<#�C<#�{<#�<#ף<#�C<#�C<#��<#��<#׎<#�<#��<#��<$'<+�!<?�<C��<%.+<#�<#�M<$
<#��<$R'<% <*1#<'Dv<(��<.O:<&��<%�<$	<$ub<$�.<#�&<#�+<#�$<$2G<$��</�I<'�s<-��<.9l<#��<#��<&|V<3��<4#h<$k�<'n�<$r�<#�m<&|V<%@�<$MO<#��<$E<%�`<$C�<#��<%�V<%��<$�<#�<$��<&�%<%��<)�<+�^<5�<K��<7(<A~<*B�<$k�<$�<$��<#��<#�I<%�<$�t<$�<#�<$)
<%\\<$_�<$j|<&s�<$XX<$I�<,$;<?�Q<'�</w�<,sq<%�L<%��<$/%<(��<'1;<&Gi<% <&�k<$��<$6�<%I<$Z�<&��<$aD<#�*<$�2<$i&<#�<#�!<#�W<$!><$�<#�<#��<$k�<$�<#�<#��<$��<(��<#��<#�8<#��<$z�<*��<%�Z<$Sa<$k�<$o�<$?[<$�`<&�a<'n�<$G<$�<%�l<&�%<%�R<$�<$�<%��<'|<$�X<%{@<$2G<$<%e<,�X<*�><(��<%ȧ<&!�<$\"<#ޫ<#�J<$�L<$��<'�c<&'<$I�<$7�<%�l<'�e<&�}<$<<%Z2<,��<#��<#�&<$��<&��<#�(<$F9<+n<&��<$m,<#�
<%�V<)�e<%Z2G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<$m,<#�(<#�H<$y�<$j|<%*<%��<&�<%�6<&A�<$��<$8�<$<<%�#<%�<+ы<'J�<$�2<#��<#�<#�<$	<#��<$9�<%�<$�<$�<&�3<()+<%b�<#��<#�<$P�<$(<$
�<$ <#�<$b�<%��<'�T<$ �<&�<,�?<$ѩ<'�e<)�1<$(<$c�<'�B<)'7<3x<%�b<$�X<$��<(�<.u<$A�<$v<$1:<$I�<#��<%I</9<3x<$aD<$�<%�<%��<$$<#�a<$��<%F<$�Q<#��<$XX<%:{<(X~<%D�<$G<#�<#�*<#�X<#��<$W<$Z<'��<%\\<(�)<<MZ<'k�<$��<$��<$R'<$�<$f<#�H<#�W<$�<$�J<&�<&y<%�M<%I<(�H<'�E<)N<#�<$"2<%�<$5w<#�<$��<%�j<'��<&ke<)�e<$�J<%*<$�L<$�j<$<#�Q<#�&<#�<#�M<#�W<$B�<#��<$.<#��<#��<$�<$I�<$�<$&<$��<$f�<%2?<&�<$��<$y�<$�J<'x�<$�<%��<$�<%��<$*<%^�<#�<#�^<#��<$W<)X<%��<$XX<%K:<$ub<#��<$e.<$��<$��<$��<#�H<$ <#�a<$!><%`�<%04<$��<%�Z<&�a<&n4<$��<#��<#�<#�N<$��<%��<%�<$Z�<$I�<$��<&�<$��<#�<$<$2G<$J�<#��<$O�<#�<#�N<$
�<#�<#ߜ<#ڑ<#�8<#�&<$q@<$#(<#�5<$ �<$�<$�(<$�<#�<#��<#ޫ<#��<#�X<#؄<#�<$�<$�<$.<$Y�<$ �<#��<#�D<#�N<$2G<#�	<#��<#�<&��<$f<#ף<#�o<#�i<#ߜ<$?[<$L<$,<#��<#�!<#�g<$�<#�<$I�<$6�<#�+<#�<$�<#�<$ K<$-<$!><#��<#�{<#��<#��<#�5<#��<#�	<#�N<#�<#��<#�{<#�c<#׎<#�M<#�C<#��<#ޫ<$'<%�<#�<#�<#�<#�<$�<$G<#�g<#�<#�(<#�<#��<#��<$�<#�<$�	<%�<$Z�<$5w<$	<$
<$�<$4e<#�&<#��<$�<#��<$
�<#�8<#�<#��<#�<#ا<#ٛ<$�<#��<#�H<$ <$J�<$�<#ۮ<#�Q<$.<$H�<#��<#�^<#��<#�<#�<#��<#�N<$C�<$%<$I�<$g�<#�<#�H<$ �<$"2<$2G<#�	<#��<#�X<#�&<#��<#�8<#��<#�<#�N<#؄<#��<#�<#��<#�^<#��<$6�<$��<#�l<#�^<#ا<#��<#�<#��<#�C<#�X<#�$<#�W<$v<#�<#��<#�*<#�<$
�<#�<$�J<$��<$��<$v<#�<#�5<#�<$a<#�<$'<#�(<#�<#�<#ߜ<#��<#�<$k�<$Z�<#�<$Z<%�<$	<$��<$��<#�r<#�I<#��<#��<#�J<#�U<$�<$�L<#�	<$1:<$9�<#�<#�8<#�8<#��<#�<#��<$/<$A�<#�&<#�*<#�<#׺<#�<<#�8<#��<#�E<#�8<#��<$j|<#��<#�8<#�M<$ �<#�{<#�l<#��<#�<#�<#�a<#�8<#�)<#��<#�<#�<#�&<#�<<#�I<#�J<$
<$�<#�X<#�8<#��<$F<#��<#��<#�&<#׺<#�X<#�l<#׺<%&<#�]<#�<$�<$�<$
<#�<#�<#��<$Ş<#�r<#�l<#�<#��<$"2<$<$�<#�<#��<#�$<#�<$/<$}<#ا<#�<#�*<#�<#�<#�<#�&<#��<#ا<#��<$k<$><<$p<#�)<#�<#��<$�<#��<#�	<$�<#�(<#�o<#�l<#��<#��<#��<#�(<#�<<#�&<#��<#��<#�c<#؄<#��<#�E<#�E<#��<#��<#�N<#�<#��<#�<#ޫ<#�$<#�+<#�<#�$<#ף<#�<#��<#��<#��<#׺<#�<#؄<#ۮ<#�o<#�<#��<#��<#�)<$
�<#��<#�<#�<#�<#��<#��<#�<#ܯ<#��<#�&<#�*<#�<#�<#�<#ا<#�+<#�8<#��<#�l<#�$<#�<#�(<#�8<#�&<#�<#�$<#��<#��<#�J<#�X<#�8<#�E<#��<#�<#�<#�0<#�0<#�0<#�<#�<#ۮ<#ܯ<#�<#ܯ<#�<#�{<#�I<#��<#׎<#�<#�<#�<#��<#ٛ<#�l<#�<#��<#��<#�o<#׺<#�<#�<#��<#ا<#�l<#ٛ<#��<#�<#�$<#��<#�i<#�$<#�<#�l<#ף<#ٛ<#�C<#��<#�o<#��<#��<#�<#�l<#��<#�l<#�+<#�o<#��<#�
<#��<#��<#؄<#�<#�<<#�l<#�E<#�D<#ף<#�o<#׺<#��<#�i<#��<#׺<#׺<#��<#�o<#�l<#��<#�o<#�<#��<#ף<#�o<#�$<#�<#�r<#�<#�$<#�<#��<#�D<#�l<#��<#�8<#׺<#��<#�{<#��<#��<#ܯ<#�<#�<#ף<#�i<#�c<#ף<#ڑ<#ߜ<#�*<#�D<#�
<#�<#�<#��<#��<#�<#�D<#ܯ<#��<#�X<#ף<#��<#�<#�<#ٛ<#��<#׺<#ף<#�$<#�<#ף<#�<#�C<#��<#ף<#�<#��<#ߜ<#�C<#��<#ٛ<#ߜ<#��<#��<#��<#�&<#�0<#�<#�<#�<#�8<#�c<#��<#�X<#�<#�<#�<#�<#�I<#�<#�<#�o<#�o<#��<#�]<#��<#�D<#�<#׎<#�i<#׺<#��<#��<#�o<#�<#�
<#��<#�<#�<#ף<#�<#�<#�
<#�{<#�0<#�D<#��<#�<#׺<#�<#�<#ף<#�<#�<#�<#ף<#�<#�<#�{<#�<#�<#�C<#�X<#��<#�{<#��<#ף<#׺<#��<#�<#�
<#�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = CTM_ADJ_PSAL, multiplicative adjustment term r = 1, no additional adjustment necessary.                                                                                                                                                              None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            CTM: alpha=0.141C, tau=6.89s, rise rate = 10 cm/s with error equal to the adjustment;OW: r =1(+/-0.0001), vertically averaged dS =-0.005(+/-0.002),                                                                                                             SOLO-W floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface.  Additional correction was unnecessary in DMQC;      PRES_ADJ_ERR: SBE sensor accuracy + resolution error                                                   No significant temperature drift detected;  TEMP_ADJ_ERR: SBE sensor accuracy + resolution error                                                                                                                                                                PSAL_ADJ corrects Conductivity Thermal Mass (CTM), Johnson et al., 2007, JAOT.; No significant drift detected in conductivity                                                                                                                                   202205040000002022050400000020220504000000  AO  ARGQQCPL                                                                    20210217002727  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210217002727  QCF$                G�O�G�O�G�O�4000            WHOIARSQWHQCV0.5                                                                20211004000000  QC                  G�O�G�O�G�O�                WHOIARSQWHQCV0.5                                                                20211004000000  CF                  G�O�G�O�G�O�                WHOIARSQCTM V1.0                                                                20220503000000  IP                  G�O�G�O�G�O�                WHOIARCAOWC V2.0ARGO_for_DMQC_2021V03; CTD_for_DMQC_2021V02                     20220504000000  IP                  G�O�G�O�G�O�                