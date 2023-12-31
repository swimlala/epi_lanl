CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:17Z creation      
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
resolution        =���   axis      Z        `  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  S@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  \x   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  c�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  m   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  tp   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  vH   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  }�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190617  20181005190617  5904953 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6432                            2B  A   APEX                            7467                            062512                          846 @ע[�`1   @ע�FP@3�+I��c�;dZ�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @�33@�  A   A   A>ffA`  A�  A���A���A���A�  A�  A�  A�  B   BffB  B  B   B(  B/��B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B�33B�33B�  B���B�  B�  B�  B�  C   C  C�C  C  C
  C  C  C  C  C�C  C  C  C�C  C�fC!�fC$  C&  C(  C*  C,�C.�C0�C1�fC3�fC5�fC8  C:  C;�fC=�fC@  CB�CD�CF�CH�CJ�CL  CN�CP�CR  CT�CV  CW�fCY��C[�fC^�C`  Ca�fCc�fCe��Cg��Ci��Ck��Cm�fCp�Cr33Ct  Cv  Cx  Cy�fC|�C}��C��C�  C��3C��C��3C��C��3C��C��3C��C��3C��C��3C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C��C��3C��C��C�  C�  C�  C�  C��C�  C��3C��3C��C��3C��3C�  C�  C�  C��C�  C��3C��C��3C��C�  C�  C��C��fC��C��C��fC�  C��C��3C��3C��3C��C�  C��3C��C��C�  C��3C�  C��C��C��3C�  C��C�  C��C��3C�  C��C��3C��3C��C��C��C��C�  C��3C�  C��fC��3C�  C�  C��C�  C��fC��3C�  C�  C��C��C�  C��C��C��3C�  C��C��C�  C�  C��C�  C��C�  C��fC�  C�  C�  C�  C��3C��C�  C��fC��3C�  C�  C��3D   D y�D ��Dy�D  D� DfD� D�3D� D  D� DfDy�D��D� D  D� D	  D	� D	��D
y�D  D�fDfD� DfD�fD  Dy�D��D�fDfDy�D��Dy�D  D�fD��Dy�D  D� D�D�fD��D� DfDy�D  D�fD  D� DfD�fDfD� D��D� D��Dy�D��Dy�D  Dy�D�3D s3D ��D!� D"fD"�fD"��D#� D$  D$� D%  D%� D&  D&� D'  D'�fD(  D(s3D(��D)� D*fD*�fD+  D+y�D+��D,y�D,��D-y�D-��D.�fD/fD/�fD/��D0� D1fD1� D2  D2y�D2��D3� D4  D4�fD4��D5y�D6  D6� D7  D7y�D7�3D8s3D8��D9� D:�D:�fD:��D;y�D;��D<s3D=  D=�fD=��D>y�D?fD?�fD@fD@�fDA  DA�fDBfDB� DB��DC� DD  DD� DE  DE� DF  DFy�DF�3DGy�DH  DHy�DH��DI� DJ  DJ� DK  DK� DL  DL� DM  DM�fDN  DNy�DO  DO� DO��DP��DQfDQy�DQ��DR� DSfDS�fDTfDT� DT�3DUy�DU��DV� DW  DWy�DX  DX� DYfDY�fDZ  DZ� D[fD[� D\  D\� D\��D]� D^  D^y�D^��D_� D`fD`�fDafDa�fDbfDby�Db��Dcy�Dc��Ddy�Dd�3Dey�Df  Df� Dg  Dg� DhfDh��Di  Diy�Dj  Dj�fDk  DyqHD�.fD�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�Q�@��A�\A"�\A@��Ab�\A�G�A�{A�{A�{A�G�A�G�A�G�A�G�B ��B	
=B��B��B ��B(��B0=qB8=qB@��BH��BP��BX��B`��Bh��Bp��Bx��B�Q�B��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B��B��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�BЅB�Q�B�Q�B܅B��B�B�Q�B��B�Q�B�Q�B�Q�B�Q�C (�C(�CB�C(�C(�C
(�C(�C(�C(�C(�CB�C(�C(�C(�CB�C(�C \C"\C$(�C&(�C((�C*(�C,B�C.B�C0B�C2\C4\C6\C8(�C:(�C<\C>\C@(�CBB�CDB�CFB�CHB�CJB�CL(�CNB�CPB�CR(�CTB�CV(�CX\CY��C\\C^B�C`(�Cb\Cd\Ce��Cg��Ci��Ck��Cn\CpB�Cr\)Ct(�Cv(�Cx(�Cz\C|B�C}��C�!HC�{C��C�.C��C�!HC��C�.C��C�!HC��C�!HC��C�{C�{C��C�{C�{C�{C��C�{C�{C�{C�!HC�{C�{C�{C�!HC��C�!HC�!HC�{C�{C�{C�{C�!HC�{C��C��C�!HC��C��C�{C�{C�{C�!HC�{C��C�!HC��C�!HC�{C�{C�!HC���C�!HC�!HC���C�{C�.C��C��C��C�!HC�{C��C�!HC�!HC�{C��C�{C�.C�!HC��C�{C�!HC�{C�!HC��C�{C�!HC��C��C�!HC�.C�.C�!HC�{C��C�{C���C��C�{C�{C�!HC�{C���C��C�{C�{C�!HC�!HC�{C�!HC�!HC��C�{C�!HC�!HC�{C�{C�.C�{C�!HC�{C���C�{C�{C�{C�{C��C�.C�{C���C��C�{C�{C��D 
=D ��D�D��D
=D�=D�D�=D�pD�=D
=D�=D�D��D�D�=D
=D�=D	
=D	�=D
�D
��D
=D��D�D�=D�D��D
=D��D�D��D�D��D�D��D
=D��D�D��D
=D�=D
D��D�D�=D�D��D
=D��D
=D�=D�D��D�D�=D�D�=D�D��D�D��D
=D��D�pD }pD!�D!�=D"�D"��D#�D#�=D$
=D$�=D%
=D%�=D&
=D&�=D'
=D'��D(
=D(}pD)�D)�=D*�D*��D+
=D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0�=D1�D1�=D2
=D2��D3�D3�=D4
=D4��D5�D5��D6
=D6�=D7
=D7��D7�pD8}pD9�D9�=D:
D:��D;�D;��D<�D<}pD=
=D=��D>�D>��D?�D?��D@�D@��DA
=DA��DB�DB�=DC�DC�=DD
=DD�=DE
=DE�=DF
=DF��DF�pDG��DH
=DH��DI�DI�=DJ
=DJ�=DK
=DK�=DL
=DL�=DM
=DM��DN
=DN��DO
=DO�=DP�DP�
DQ�DQ��DR�DR�=DS�DS��DT�DT�=DT�pDU��DV�DV�=DW
=DW��DX
=DX�=DY�DY��DZ
=DZ�=D[�D[�=D\
=D\�=D]�D]�=D^
=D^��D_�D_�=D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��Dd�pDe��Df
=Df�=Dg
=Dg�=Dh�Dh�
Di
=Di��Dj
=Dj��Dk
=Dy{�D�3�D�Ǯ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��A���A�ȴAͲ-A�;dAˋDA��
A�ZA��;AɸRAɗ�A�?}A��A�%A�A���A��`A��#A���Aȏ\A�/A��A��A��A�ƨAǕ�A�  AƶFAƼjA�~�A���AľwA�%A�bA�$�A�`BA�C�A�VA���A��A�z�A�(�A��`A���A�1A���A���A��^A�bA���A���A�9XA�S�A���A�ĜA�I�A�9XA�+A��A��/A�VA��RA���A���A���A��#A�VA���A��+A�jA��
A�(�A�hsA�ȴA��A�  A�A�jA�ƨA�A�A��^A�ȴA��yA�ZA��A�z�A���A��#A�jA�E�A�
=A�&�A�M�A�G�A�7A| �Az�Ay�wAw��Ao��Al�AkhsAgK�Ae`BAaS�A^��A]�AY�wAU��AUoASp�AO��AM`BAK�AI��AH�/AGdZAF�AF(�AFJAEl�AC�A@E�A>�/A>n�A<�RA:VA8jA6�RA5S�A4��A3��A2A�A1��A1t�A1VA0bA/hsA.�A-�^A-XA,��A,��A,VA,~�A,-A+��A+oA)�A'��A't�A'+A&E�A%�mA%�7A%K�A#�mA#%A"{A �yA�A|�A�`AM�A�A|�Ap�A~�A��AI�A�hA��A�AO�AbAoAJA|�A
�/A
�A
{A	�PA��A\)A��A��A��A�mA^5A��A �+@��@��@�I�@�x�@�l�@��@�!@�&�@��D@��;@�dZ@��/@��T@��`@�\)@�j@�v�@���@߶F@���@�n�@��/@�ƨ@�;d@�@ى7@��@�b@��
@��@�b@Гu@�o@��@���@Η�@��T@�hs@��`@�A�@��m@˥�@�+@�=q@���@���@�\)@��H@�^5@�/@���@ě�@ă@�r�@�S�@°!@��T@���@�bN@�1@�ȴ@�J@��@�&�@���@�r�@�r�@�Z@�9X@� �@�b@�1@���@��;@��@��P@�;d@���@��^@��`@��@��@�~�@�5?@�{@�@���@���@�o@���@��+@�-@�@��`@��@�j@�9X@�b@���@�@��@�@���@�O�@��`@�Ĝ@��@�Q�@��
@���@�C�@�n�@�^5@��+@���@��H@�~�@�$�@���@���@�x�@�G�@��/@���@�dZ@�K�@���@�S�@�o@���@��+@�ff@�-@�{@�J@��T@���@�hs@�%@��j@�z�@�(�@���@���@��P@�l�@�C�@��@��@���@���@��\@�n�@�ff@�E�@��@��^@��@��u@�9X@��;@��@�"�@��@�o@���@��H@�ȴ@��\@�5?@��@�@��#@�hs@��@��@���@�z�@�b@�ƨ@���@�l�@�K�@�33@�"�@���@�E�@�`B@��`@���@�I�@��@�;d@�ȴ@��\@��@��@��@��/@���@�9X@��
@�ƨ@���@�dZ@�33@��@��!@�5?@�@���@��`@�r�@�1@��@���@��P@�C�@���@��H@��H@���@��@���@���@�z�@�z�@�bN@�1'@�b@�1@�  @�  @�1@�1@�  @��@�|�@�o@��!@�v�@�V@�=q@�$�@��@�@�X@��`@�Ĝ@��@��u@�z�@�9X@��@��
@��@���@���@��@�dZ@���@���@�$�@��@�p�@�?}@��j@�Q�@� �@�  @��@��@�dZ@�dZ@�
=@��+@}�@t�u@a(�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A��A���A�ȴAͲ-A�;dAˋDA��
A�ZA��;AɸRAɗ�A�?}A��A�%A�A���A��`A��#A���Aȏ\A�/A��A��A��A�ƨAǕ�A�  AƶFAƼjA�~�A���AľwA�%A�bA�$�A�`BA�C�A�VA���A��A�z�A�(�A��`A���A�1A���A���A��^A�bA���A���A�9XA�S�A���A�ĜA�I�A�9XA�+A��A��/A�VA��RA���A���A���A��#A�VA���A��+A�jA��
A�(�A�hsA�ȴA��A�  A�A�jA�ƨA�A�A��^A�ȴA��yA�ZA��A�z�A���A��#A�jA�E�A�
=A�&�A�M�A�G�A�7A| �Az�Ay�wAw��Ao��Al�AkhsAgK�Ae`BAaS�A^��A]�AY�wAU��AUoASp�AO��AM`BAK�AI��AH�/AGdZAF�AF(�AFJAEl�AC�A@E�A>�/A>n�A<�RA:VA8jA6�RA5S�A4��A3��A2A�A1��A1t�A1VA0bA/hsA.�A-�^A-XA,��A,��A,VA,~�A,-A+��A+oA)�A'��A't�A'+A&E�A%�mA%�7A%K�A#�mA#%A"{A �yA�A|�A�`AM�A�A|�Ap�A~�A��AI�A�hA��A�AO�AbAoAJA|�A
�/A
�A
{A	�PA��A\)A��A��A��A�mA^5A��A �+@��@��@�I�@�x�@�l�@��@�!@�&�@��D@��;@�dZ@��/@��T@��`@�\)@�j@�v�@���@߶F@���@�n�@��/@�ƨ@�;d@�@ى7@��@�b@��
@��@�b@Гu@�o@��@���@Η�@��T@�hs@��`@�A�@��m@˥�@�+@�=q@���@���@�\)@��H@�^5@�/@���@ě�@ă@�r�@�S�@°!@��T@���@�bN@�1@�ȴ@�J@��@�&�@���@�r�@�r�@�Z@�9X@� �@�b@�1@���@��;@��@��P@�;d@���@��^@��`@��@��@�~�@�5?@�{@�@���@���@�o@���@��+@�-@�@��`@��@�j@�9X@�b@���@�@��@�@���@�O�@��`@�Ĝ@��@�Q�@��
@���@�C�@�n�@�^5@��+@���@��H@�~�@�$�@���@���@�x�@�G�@��/@���@�dZ@�K�@���@�S�@�o@���@��+@�ff@�-@�{@�J@��T@���@�hs@�%@��j@�z�@�(�@���@���@��P@�l�@�C�@��@��@���@���@��\@�n�@�ff@�E�@��@��^@��@��u@�9X@��;@��@�"�@��@�o@���@��H@�ȴ@��\@�5?@��@�@��#@�hs@��@��@���@�z�@�b@�ƨ@���@�l�@�K�@�33@�"�@���@�E�@�`B@��`@���@�I�@��@�;d@�ȴ@��\@��@��@��@��/@���@�9X@��
@�ƨ@���@�dZ@�33@��@��!@�5?@�@���@��`@�r�@�1@��@���@��P@�C�@���@��H@��H@���@��@���@���@�z�@�z�@�bN@�1'@�b@�1@�  @�  @�1@�1@�  @��@�|�@�o@��!@�v�@�V@�=q@�$�@��@�@�X@��`@�Ĝ@��@��u@�z�@�9X@��@��
@��@���@���@��@�dZ@���@���@�$�@��@�p�@�?}@��j@�Q�@� �@�  @��@��@�dZ@�dZ@�
=@��+@}�@t�u@a(�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�mB
�mB
�mB
�yB
�yB
�B
�B  BPB�B/B49B;dBD�BG�BL�BP�BR�BVBYB]/Bs�B�B�B�B�VB��B��B�?B��B�B\B-BD�BF�BF�BJ�BN�BL�BR�B[#BbNBdZBe`Be`BjBq�Bo�Bt�By�B�B�B� B�B� B�B�B{�Bp�Bv�B��B�!B��B��B�7Bt�Bl�BbNB^5BYBJ�BB�B=qB6FB,B�B��B��B�fB��BɺB�FB��B�oB{�Be`B<jB�B
�#B
�LB
�B
��B
��B
�%B
XB
'�B
�B
B	�B	�B	�B	��B	�PB	�B	jB	gmB	YB	F�B	8RB	-B	�B	�B		7B��B�B�yB�fB�ZB�HB�/B�)B�#B�
B�B��BɺBƨB�wB�B��B�bB�1B�+B�B~�B}�B}�B� B�B�%B�B�%B�1B�+B�VB�uB��B�B�3B�jBɺB��B��B��B�B�#B�5B�HB�ZB�ZB�TB�;B�
B��B��B��B��B��B�B�)B��BĜB�dB��B�}B�qB��B�wB�jB�3B�!B�!B�'B�3B�3B�3B�3B�9B�FB�RB��B�wB�XB�jB��B�wB�dB�XB�XB�qBÖBÖBĜBĜBǮB��B��B��B��B��B��B��B��B��B��B��B��B�B�
B�B�#B�;B�5B�5B�B�B�B�B�B�B�B�#B�/B�/B�/B�/B�;B�TB�fB�sB�yB�B�B�B�B�B�B��B��B��B��B	  B	B	%B		7B	DB	PB	\B	hB	hB	oB	oB	uB	uB	uB	uB	uB	{B	�B	�B	�B	�B	 �B	"�B	%�B	)�B	+B	,B	.B	1'B	7LB	=qB	@�B	A�B	C�B	F�B	J�B	K�B	M�B	N�B	P�B	R�B	XB	^5B	`BB	aHB	cTB	hsB	jB	m�B	o�B	p�B	p�B	p�B	q�B	s�B	v�B	y�B	� B	�B	�B	�%B	�+B	�+B	�1B	�7B	�1B	�1B	�=B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�'B	�-B	�3B	�9B	�?B	�RB	�XB	�^B	�dB	�jB	�qB	�}B	��B	��B	ÖB	ŢB	ŢB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�
B	�B	�B	�B	�
B	�B	��B	��B	��B	�
B	�B	�B	�)B	�/B	�/B	�;B	�BB	�BB	�NB	�`B	�fB	�fB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
B
�B
�B
,q2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B
�mB
�mB
�mB
�yB
�yB
�B
�B  BPB�B/B49B;dBD�BG�BL�BP�BR�BVBYB]/Bs�B�B�B�B�VB��B��B�?B��B�B\B-BD�BF�BF�BJ�BN�BL�BR�B[#BbNBdZBe`Be`BjBq�Bo�Bt�By�B�B�B� B�B� B�B�B{�Bp�Bv�B��B�!B��B��B�7Bt�Bl�BbNB^5BYBJ�BB�B=qB6FB,B�B��B��B�fB��BɺB�FB��B�oB{�Be`B<jB�B
�#B
�LB
�B
��B
��B
�%B
XB
'�B
�B
B	�B	�B	�B	��B	�PB	�B	jB	gmB	YB	F�B	8RB	-B	�B	�B		7B��B�B�yB�fB�ZB�HB�/B�)B�#B�
B�B��BɺBƨB�wB�B��B�bB�1B�+B�B~�B}�B}�B� B�B�%B�B�%B�1B�+B�VB�uB��B�B�3B�jBɺB��B��B��B�B�#B�5B�HB�ZB�ZB�TB�;B�
B��B��B��B��B��B�B�)B��BĜB�dB��B�}B�qB��B�wB�jB�3B�!B�!B�'B�3B�3B�3B�3B�9B�FB�RB��B�wB�XB�jB��B�wB�dB�XB�XB�qBÖBÖBĜBĜBǮB��B��B��B��B��B��B��B��B��B��B��B��B�B�
B�B�#B�;B�5B�5B�B�B�B�B�B�B�B�#B�/B�/B�/B�/B�;B�TB�fB�sB�yB�B�B�B�B�B�B��B��B��B��B	  B	B	%B		7B	DB	PB	\B	hB	hB	oB	oB	uB	uB	uB	uB	uB	{B	�B	�B	�B	�B	 �B	"�B	%�B	)�B	+B	,B	.B	1'B	7LB	=qB	@�B	A�B	C�B	F�B	J�B	K�B	M�B	N�B	P�B	R�B	XB	^5B	`BB	aHB	cTB	hsB	jB	m�B	o�B	p�B	p�B	p�B	q�B	s�B	v�B	y�B	� B	�B	�B	�%B	�+B	�+B	�1B	�7B	�1B	�1B	�=B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�'B	�-B	�3B	�9B	�?B	�RB	�XB	�^B	�dB	�jB	�qB	�}B	��B	��B	ÖB	ŢB	ŢB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�
B	�B	�B	�B	�
B	�B	��B	��B	��B	�
B	�B	�B	�)B	�/B	�/B	�;B	�BB	�BB	�NB	�`B	�fB	�fB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
B
�B
�B
,q2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.16 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190617                              AO  ARCAADJP                                                                    20181005190617    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190617  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190617  QCF$                G�O�G�O�G�O�8000            