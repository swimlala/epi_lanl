CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:16:55Z creation      
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
resolution        =���   axis      Z        h  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  S`   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  mP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  }�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191655  20181005191655  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @״��e�~1   @״����@4�XbM��c��\)1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   B   @���@���A   A   A>ffA`  A�  A�  A���A���A�  A�  A�  A�33B   B��B��B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C�C
�C  C  C�C�C  C�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C5�fC8  C:  Cj�Cl�Cn  Co�fCr  Ct�Cv  Cx�Cz�C|  C~  C��C�  C�  C��C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C��C�  C��3C�  C�  C��3C��3C��3C�  C��C�  C�  C�  C��3C��3C��3C��3C��C�  C�  C�  C��3C��3C�  C��C�  C��3C�  C�  C��fC��3C��C�  C�  C��C��C��3C��3C��C�  C��3C��C�  C��3C��3C�  C��C�  C��3C��C��C��3C��3C��3C��3C��C��C�  C�  C�  C��3C��3C�  C��C��C�  C��3C��3C��C��C�  C��3C�  C��C��C�  C��3C��C�  C�  C��3C��C��3C�  C�  C�  C�  C��C��C�  C�  C��3C��C�  C��C��C��C�  C��3C��C�  C�  C�  C�  C��C��3C�  C��3C�  C��C�  D   D � D  D� DfD� D  D�fD��D� D  D� DfD� D��Dy�D  D� D	  D	� D
fD
� D  D� D  D� DfD�fDfDy�D��D�fDfD� DfDy�D  D�fD  D� D  D� D  Dy�D  D�fD  D� DfD� D  D� D  D� D  D� D��D� D  D� DfD�fDfD�fD�3D � D!  D!�fD"  D"� D#  D#� D$fD$y�D%  D%� D&  D&�fD'fD'�fD'��D(� D)fD)�fD*  D*y�D*�3D+y�D+��D,� D-  D-y�D-�3D.y�D/  D/y�D/��D0y�D1  D1� D1��D2��D3  D3y�D4fD4�fD5  D5� D6fD6�fD7  D7� D8  D8y�D9  D9� D:fD:�fD;fD;�fD<  D<y�D<��D=� D>fD>�fD>��D?� D@fD@�fD@�3DAy�DB  DB� DCfDC�fDD  DD�fDEfDEy�DFfDFy�DG  DG�fDH  DH�fDIfDI� DJ  DJ� DK  DK� DK��DLy�DMfDM��DM��DN�fDO  DO�fDPfDP�fDQfDQ�fDR  DR� DSfDS� DS��DT� DU  DUy�DU��DVy�DW  DW� DX  DX� DY  DYy�DZ  DZ� DZ�3D[�fD\fD\�fD]  D]y�D]��D^� D_  D_� D_��D`y�Da  Da�fDbfDb�fDcfDc� Dd  Dd�fDe  Dey�De��Df�fDg  Dgy�Dh  Dh� Dh��Diy�Dj  Dj� Dk  Dk� Dl  Dl�fDmfDm� Dn  Dn�fDo  Doy�Do��Dp� DqfDq�fDr  Dr� DsfDs�fDtfDt�fDt��Duy�Du��Dv�fDwfDw�fDw�3Dy�\D�2�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��A(�A$(�AB�\Ad(�A�{A�{A��HA��HA�{A�{A�{A�G�B
=B��B��B
=B!
=B)
=B1
=B9
=BA
=BI
=BQ
=BYp�Ba
=Bi
=Bq
=By
=B��B��B��B��B��B��B��B��B��B��B��B��B��B��RB��B��B��BąBȅB̅BЅBԅB؅B܅B��B�B�B�B��B�B��B��C B�CB�CB�C\)C\)C
\)CB�CB�C\)C\)CB�C(�CB�CB�CB�CB�C B�C"B�C$B�C&B�C(B�C*B�C,B�C.B�C0B�C2B�C4B�C6(�C8B�C:B�Cj\)Cl\)CnB�Cp(�CrB�Ct\)CvB�Cx\)Cz\)C|B�C~B�C�.C�!HC�!HC�.C�!HC�!HC�.C�!HC�!HC�{C�!HC�!HC�!HC�!HC�{C�{C�!HC�.C�!HC�{C�!HC�!HC�{C�{C�{C�!HC�.C�!HC�!HC�!HC�{C�{C�{C�{C�.C�!HC�!HC�!HC�{C�{C�!HC�.C�!HC�{C�!HC�!HC��C�{C�.C�!HC�!HC�.C�.C�{C�{C�.C�!HC�{C�.C�!HC�{C�{C�!HC�.C�!HC�{C�.C�.C�{C�{C�{C�{C�.C�.C�!HC�!HC�!HC�{C�{C�!HC�:�C�:�C�!HC�{C�{C�.C�.C�!HC�{C�!HC�.C�.C�!HC�{C�.C�!HC�!HC�{C�.C�{C�!HC�!HC�!HC�!HC�.C�.C�!HC�!HC�{C�.C�!HC�.C�.C�:�C�!HC�{C�.C�!HC�!HC�!HC�!HC�.C�{C�!HC�{C�!HC�.C�!HD �D ��D�D��D
D��D�D�
D
>D��D�D��D
D��D
>D�>D�D��D	�D	��D

D
��D�D��D�D��D
D�
D
D�>D
>D�
D
D��D
D�>D�D�
D�D��D�D��D�D�>D�D�
D�D��D
D��D�D��D�D��D�D��D
>D��D�D��D
D�
D
D�
D �D ��D!�D!�
D"�D"��D#�D#��D$
D$�>D%�D%��D&�D&�
D'
D'�
D(
>D(��D)
D)�
D*�D*�>D+�D+�>D,
>D,��D-�D-�>D.�D.�>D/�D/�>D0
>D0�>D1�D1��D2
>D2�qD3�D3�>D4
D4�
D5�D5��D6
D6�
D7�D7��D8�D8�>D9�D9��D:
D:�
D;
D;�
D<�D<�>D=
>D=��D>
D>�
D?
>D?��D@
D@�
DA�DA�>DB�DB��DC
DC�
DD�DD�
DE
DE�>DF
DF�>DG�DG�
DH�DH�
DI
DI��DJ�DJ��DK�DK��DL
>DL�>DM
DM�qDN
>DN�
DO�DO�
DP
DP�
DQ
DQ�
DR�DR��DS
DS��DT
>DT��DU�DU�>DV
>DV�>DW�DW��DX�DX��DY�DY�>DZ�DZ��D[�D[�
D\
D\�
D]�D]�>D^
>D^��D_�D_��D`
>D`�>Da�Da�
Db
Db�
Dc
Dc��Dd�Dd�
De�De�>Df
>Df�
Dg�Dg�>Dh�Dh��Di
>Di�>Dj�Dj��Dk�Dk��Dl�Dl�
Dm
Dm��Dn�Dn�
Do�Do�>Dp
>Dp��Dq
Dq�
Dr�Dr��Ds
Ds�
Dt
Dt�
Du
>Du�>Dv
>Dv�
Dw
Dw�
Dx�Dy� D�:�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�VA�VA�bNA�bNA�bNA�bNA�bNA�dZA�dZA�XA�"�A�%A�ĜA�l�A͛�A��AɁA��HAǛ�A�-A���A�r�A�A�A��yA�l�A�O�A�\)Aħ�A�5?A���A��TA�p�A�?}A���A��mA��`A�JA�9XA�=qA���A�%A��7A�VA��wA�^5A���A���A�K�A��mA���A�K�A�
=A���A�&�A�S�A�G�A��A�%A�t�A�JA��#A�dZA���A�+A��
A�ƨA�;dA�VA��!A�VA�bA�C�A���A�oA���A���A�%A�hsA�E�A�G�A�Q�A�r�A��A�\)A��A�E�A�|�A���A�O�A���A���A�=qAY�AU`BAT��ATVAS��ASdZASC�ARE�AP��AO�AO%AN  AMAK�mAIO�AGt�AF�AEK�ACp�A@�/A=�A:M�A8^5A7
=A5��A1��A/�
A.�uA-��A-G�A++A*�A*v�A)�#A(��A'��A'
=A&~�A%oA#��A#��A#XA"ZA �HA�;A�HAVA5?A��A�A��AVA��A+A~�AJA�HA�9A�wA�yAbAp�A�/A�mA~�AS�A�+A�AC�A
�A
1'A	��A	O�A�`AffA��AO�A/A�A�!A=qA�Ap�Ar�A�AK�AoA�!AA�A��Ap�AVA ĜA ��A ��A �A33A33A;dAV@��@�l�@�x�@�Q�@���@��@���@���@�I�@�@�G�@�o@���@��
@�l�@���@�ff@���@�dZ@ٺ^@ى7@���@�1'@ץ�@��@Չ7@�t�@�V@с@���@�(�@��;@�"�@�-@�hs@̴9@�1'@˝�@ʟ�@�=q@��T@�x�@�?}@�%@��@�ȴ@Ə\@ŉ7@Ĭ@���@�@���@�O�@�V@���@��F@�t�@�K�@�@���@��@�`B@��`@��9@�z�@�Q�@�A�@�1'@�b@��m@�ƨ@��P@�C�@��y@���@��@��@��#@���@��@��@���@���@�l�@�@���@�5?@���@�@�&�@��9@�1'@��@�1@���@��@��
@��F@�ƨ@�ƨ@�l�@�;d@��y@���@���@��!@���@��+@�V@�ff@��+@�ȴ@�@�@���@���@�@�`B@�r�@�A�@�ƨ@�S�@�M�@��T@���@��7@�p�@�&�@�z�@�K�@��y@��+@���@�hs@�%@�Z@��@���@�
=@���@���@��\@�5?@��h@�%@���@�r�@�9X@�1'@�1'@�9X@���@���@���@�%@�V@��@���@� �@�C�@��@���@��\@� �@���@��;@��m@�S�@��m@��
@�K�@�@���@�X@�?}@�G�@��@���@���@�bN@�Q�@��w@�dZ@�\)@�33@�+@�"�@���@���@�v�@�M�@�{@��@��#@��-@���@��h@��7@�X@���@�Ĝ@���@�r�@�A�@�  @��m@��F@�l�@�K�@��@���@�M�@�@��@��-@�7L@���@�r�@�A�@��m@��@��P@�dZ@�C�@�33@�
=@�^5@���@��#@���@�?}@��@��j@�bN@�9X@���@���@�|�@��@���@�V@�$�@���@�@��h@�p�@�hs@�X@�O�@�G�@�G�@��@���@��u@�r�@�1@��@���@�+@�ȴ@�~�@�@���@��@�G�@��@���@���@��@�bN@�I�@�b@��
@�|�@�"�@�ȴ@��+@�V@���@�p�@��|@t[�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�VA�VA�bNA�bNA�bNA�bNA�bNA�dZA�dZA�XA�"�A�%A�ĜA�l�A͛�A��AɁA��HAǛ�A�-A���A�r�A�A�A��yA�l�A�O�A�\)Aħ�A�5?A���A��TA�p�A�?}A���A��mA��`A�JA�9XA�=qA���A�%A��7A�VA��wA�^5A���A���A�K�A��mA���A�K�A�
=A���A�&�A�S�A�G�A��A�%A�t�A�JA��#A�dZA���A�+A��
A�ƨA�;dA�VA��!A�VA�bA�C�A���A�oA���A���A�%A�hsA�E�A�G�A�Q�A�r�A��A�\)A��A�E�A�|�A���A�O�A���A���A�=qAY�AU`BAT��ATVAS��ASdZASC�ARE�AP��AO�AO%AN  AMAK�mAIO�AGt�AF�AEK�ACp�A@�/A=�A:M�A8^5A7
=A5��A1��A/�
A.�uA-��A-G�A++A*�A*v�A)�#A(��A'��A'
=A&~�A%oA#��A#��A#XA"ZA �HA�;A�HAVA5?A��A�A��AVA��A+A~�AJA�HA�9A�wA�yAbAp�A�/A�mA~�AS�A�+A�AC�A
�A
1'A	��A	O�A�`AffA��AO�A/A�A�!A=qA�Ap�Ar�A�AK�AoA�!AA�A��Ap�AVA ĜA ��A ��A �A33A33A;dAV@��@�l�@�x�@�Q�@���@��@���@���@�I�@�@�G�@�o@���@��
@�l�@���@�ff@���@�dZ@ٺ^@ى7@���@�1'@ץ�@��@Չ7@�t�@�V@с@���@�(�@��;@�"�@�-@�hs@̴9@�1'@˝�@ʟ�@�=q@��T@�x�@�?}@�%@��@�ȴ@Ə\@ŉ7@Ĭ@���@�@���@�O�@�V@���@��F@�t�@�K�@�@���@��@�`B@��`@��9@�z�@�Q�@�A�@�1'@�b@��m@�ƨ@��P@�C�@��y@���@��@��@��#@���@��@��@���@���@�l�@�@���@�5?@���@�@�&�@��9@�1'@��@�1@���@��@��
@��F@�ƨ@�ƨ@�l�@�;d@��y@���@���@��!@���@��+@�V@�ff@��+@�ȴ@�@�@���@���@�@�`B@�r�@�A�@�ƨ@�S�@�M�@��T@���@��7@�p�@�&�@�z�@�K�@��y@��+@���@�hs@�%@�Z@��@���@�
=@���@���@��\@�5?@��h@�%@���@�r�@�9X@�1'@�1'@�9X@���@���@���@�%@�V@��@���@� �@�C�@��@���@��\@� �@���@��;@��m@�S�@��m@��
@�K�@�@���@�X@�?}@�G�@��@���@���@�bN@�Q�@��w@�dZ@�\)@�33@�+@�"�@���@���@�v�@�M�@�{@��@��#@��-@���@��h@��7@�X@���@�Ĝ@���@�r�@�A�@�  @��m@��F@�l�@�K�@��@���@�M�@�@��@��-@�7L@���@�r�@�A�@��m@��@��P@�dZ@�C�@�33@�
=@�^5@���@��#@���@�?}@��@��j@�bN@�9X@���@���@�|�@��@���@�V@�$�@���@�@��h@�p�@�hs@�X@�O�@�G�@�G�@��@���@��u@�r�@�1@��@���@�+@�ȴ@�~�@�@���@��@�G�@��@���@���@��@�bN@�I�@�b@��
@�|�@�"�@�ȴ@��+@�V@���@�p�@��|@t[�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�By�By�By�By�By�By�By�By�By�By�By�Bx�Bw�Bz�B�=B�oB�{B�hB��B�B�B�9B�LB�qB��B�
B�B�BI�B|�B�B�=B�VB�oB��B�FBǮB��B�B�B��B��BȴB��B��B��BȴB��B��B��B��B�^B�FB��B��B��B�+BiyBR�BF�BF�B<jB)�B�BDB%BB��B�B�/B��B�^B��B�uBjB]/BS�BH�B,B�BVBB
��B
�B
�#B
��B
ŢB
�dB
�3B
�B
��@���B	C�B	7LB	49B	33B	0!B	/B	-B	)�B	"�B	�B	�B	�B	�B	bB	B��B��B�B�mB�#BɺB�}B�dB�FB�-B�B�B�B�B�B�'B�'B�-B�'B�B�B�B��B��B��B��B��B��B��B��B��B�{B��B��B�uB�oB�oB�uB�hB�VB�VB�JB�PB�PB�PB�hB�hB�\B�\B�\B�\B�bB�\B�bB�uB�{B�hB�oB�uB�uB�{B��B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�XB��B��B�
B�B��B��B��B��B��B�HB�yB�TB�B��B�}B�'B�-B�B�B��B��B��B�B�!B�!B�3B�?B�RB�^B�dB�jB�jB�qB�wB��B��BÖBƨBȴBɺB��B��B��B��B��B�
B�B�B�)B�;B�BB�TB�fB�yB�B��B��B��B��B��B	B	B	B	B	+B	DB	\B	bB	oB	oB	oB	uB	uB	uB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	$�B	$�B	%�B	'�B	'�B	+B	,B	-B	.B	/B	1'B	1'B	2-B	2-B	2-B	49B	7LB	:^B	;dB	=qB	@�B	E�B	F�B	H�B	I�B	J�B	J�B	L�B	N�B	O�B	Q�B	ZB	]/B	aHB	cTB	cTB	bNB	aHB	`BB	`BB	`BB	hsB	k�B	l�B	m�B	n�B	p�B	t�B	{�B	|�B	~�B	�B	�B	�B	�%B	�7B	�=B	�DB	�JB	�JB	�PB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�?B	�RB	�RB	�^B	�dB	�XB	�XB	�wB	ÖB	ÖB	�qB	�jB	�wB	��B	��B	ƨB	ƨB	ŢB	B	��B	��B	��B	B	B	B	B	ĜB	ŢB	ŢB	ǮB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�)B	�5B	�5B	�BB	�BB	�HB	�TB	�TB	�TB	�fB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
%B
+B
�B
�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  By�By�By�By�By�By�By�By�By�By�By�Bx�Bw�Bz�B�=B�oB�{B�hB��B�B�B�9B�LB�qB��B�
B�B�BI�B|�B�B�=B�VB�oB��B�FBǮB��B�B�B��B��BȴB��B��B��BȴB��B��B��B��B�^B�FB��B��B��B�+BiyBR�BF�BF�B<jB)�B�BDB%BB��B�B�/B��B�^B��B�uBjB]/BS�BH�B,B�BVBB
��B
�B
�#B
��B
ŢB
�dB
�3B
�B
��@���B	C�B	7LB	49B	33B	0!B	/B	-B	)�B	"�B	�B	�B	�B	�B	bB	B��B��B�B�mB�#BɺB�}B�dB�FB�-B�B�B�B�B�B�'B�'B�-B�'B�B�B�B��B��B��B��B��B��B��B��B��B�{B��B��B�uB�oB�oB�uB�hB�VB�VB�JB�PB�PB�PB�hB�hB�\B�\B�\B�\B�bB�\B�bB�uB�{B�hB�oB�uB�uB�{B��B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�XB��B��B�
B�B��B��B��B��B��B�HB�yB�TB�B��B�}B�'B�-B�B�B��B��B��B�B�!B�!B�3B�?B�RB�^B�dB�jB�jB�qB�wB��B��BÖBƨBȴBɺB��B��B��B��B��B�
B�B�B�)B�;B�BB�TB�fB�yB�B��B��B��B��B��B	B	B	B	B	+B	DB	\B	bB	oB	oB	oB	uB	uB	uB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	$�B	$�B	%�B	'�B	'�B	+B	,B	-B	.B	/B	1'B	1'B	2-B	2-B	2-B	49B	7LB	:^B	;dB	=qB	@�B	E�B	F�B	H�B	I�B	J�B	J�B	L�B	N�B	O�B	Q�B	ZB	]/B	aHB	cTB	cTB	bNB	aHB	`BB	`BB	`BB	hsB	k�B	l�B	m�B	n�B	p�B	t�B	{�B	|�B	~�B	�B	�B	�B	�%B	�7B	�=B	�DB	�JB	�JB	�PB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�?B	�RB	�RB	�^B	�dB	�XB	�XB	�wB	ÖB	ÖB	�qB	�jB	�wB	��B	��B	ƨB	ƨB	ŢB	B	��B	��B	��B	B	B	B	B	ĜB	ŢB	ŢB	ǮB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�)B	�5B	�5B	�BB	�BB	�HB	�TB	�TB	�TB	�fB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
%B
+B
�B
�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191655                              AO  ARCAADJP                                                                    20181005191655    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191655  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191655  QCF$                G�O�G�O�G�O�8000            