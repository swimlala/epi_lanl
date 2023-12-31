CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-10-18T09:15:52Z AOML 3.0 creation; 2016-06-01T00:08:27Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151018091552  20160531170827  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055_7112_130                   2C  D   APEX                            5374                            041511                          846 @�w�<�$1   @�w��y@:��hr�!�c�n��P1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBPffBW��B_��Bh  Bp  Bx  B���B���B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DWfDW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dly�Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtY�DyS3D��D�I�D�C3D�ɚD�  D�FfD��3D��3D�3D�S3D�vfD�� D���D�I�DږfD�� D���D�,�D�s3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��R@˅AA%AEAeA��HA��HA��HA��HA��HA��HA��HA��HBp�B	p�Bp�Bp�B!p�B)p�B1p�B9p�BAp�BI�
BQ�
BY
>Ba
>Bip�Bqp�Byp�B�Q�B��B��RB��RB��RB��B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBĸRBȸRB̸RBиRBԸRBظRBܸRB�RB�RB�RB�RB�RB��RB��RB��RC \)C\)C\)C\)C\)C
\)C\)C\)C\)C\)C\)C\)C\)C\)C\)C\)C \)C"\)C$\)C&\)C(\)C*\)C,\)C.\)C0\)C2\)C4\)C6\)C8\)C:\)C<\)C>\)C@\)CB\)CD\)CF\)CH\)CJ\)CL\)CN\)CP\)CR\)CT\)CV\)CX\)CZ\)C\\)C^\)C`\)Cb\)Cd\)Cf\)Ch\)Cj\)Cl\)Cn\)Cp\)Cr\)Ct\)Cv\)Cx\)Cz\)C|\)C~\)C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�:�C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.D 
D �
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D	
D	�
D

D
�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D 
D �
D!
D!�
D"
D"�
D#
D#�
D$
D$�
D%
D%�
D&
D&�
D'
D'�
D(
D(�
D)
D)�
D*
D*�
D+
D+�
D,
D,�
D-
D-�
D.
D.�
D/
D/�
D0
D0�
D1
D1�
D2
D2�
D3
D3�
D4
D4�
D5
D5�
D6
D6�
D7
D7�
D8
D8�
D9
D9�
D:
D:�
D;
D;�
D<
D<�
D=
D=�
D>
D>�
D?
D?�
D@
D@�
DA
DA�
DB
DB�
DC
DC�
DD
DD�
DE
DE�
DF
DF�
DG
DG�
DH
DH�
DI
DI�
DJ
DJ�
DK
DK�
DL
DL�
DM
DM�
DN
DN�
DO
DO�
DP
DP�
DQ
DQ�
DR
DR�
DS
DS�
DT
DT�
DU
DU�
DV
DV�
DWpDW�
DX
DX�
DY
DY�
DZ
DZ�
D[
D[�
D\
D\�
D]
D]�
D^
D^�
D_
D_�
D`
D`�
Da
Da�
Db
Db�
Dc
Dc�
Dd
Dd�
De
De�
Df
Df�
Dg
Dg�
Dh
Dh�
Di
Di�
Dj
Dj�
Dk
Dk�
Dl
Dl��Dm
Dm�
Dn
Dn�
Do
Do�
Dp
Dp�
Dq
Dq�
Dr
Dr�
Ds
Ds�
Dt
Dtp�Dyj=D�%D�UD�N�D��D��D�Q�D���D�θD��D�^�D���D��D�D�UDڡ�D�ۅD��RD�8RD�~�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aϡ�A�~�A�hsA�Q�A�A�A�9XA�33A�-A�+A�+A�&�A�"�A��A��A�{A�bA�JA�%A�A�  A���A��A��AΕ�A�5?A���A���A�M�Aƛ�A��/A���A���A�{A��9A�9XA��A���A��DA�~�A�bNA��A�S�A�bA�{A�C�A��A�(�A�S�A�/A��FA�t�A�dZA�p�A���A��hA��;A�E�A��mA��^A��+A��9A���A�oA���A�1'A�XA�x�A��hA�VA��A��mA��+A�-A��A�A�ĜA��uA��Ap�A~z�A}��A}dZA{�mAz�Az{Ax�+At�As
=Aq�;Aq\)Ap�Ao��AoVAn  Al �Ag��Aet�Ac+A`�jA_hsA]�A[�hAZ��AZbAY�AW��AWC�AW�AV��AVffAU�mAU;dAU%AT�`AT��AT��AT9XAS&�AR-AQ�-AQl�AQK�AQ
=AP��AO�AO��AOC�ANbNAM�AM��AM7LAL(�AJ�RAH��AH��AH�AG��AFv�AFAEt�AD�DAC|�AB �A@��A@�jA?��A> �A<��A<�DA<1A;�-A;\)A;/A:�/A9��A7�PA6v�A6  A5��A5��A4�!A4{A2�A17LA0�DA/�7A.��A. �A-��A,JA+�A*n�A)��A(A�A'�A'
=A&ĜA&jA"��A"(�A"bA!��A �A bNA�AĜA$�A��A33A5?AdZAĜAdZAI�A/A��AXA�A��A�DA�A"�A��A�wA�DA�mAx�AdZA�A�HA�\A��A��A�A
1A	hsA	A�A��AXAz�A�TA&�A�A��A�A��AA bN@��@�ȴ@�dZ@�-@�?}@�b@���@�b@��y@�!@���@�=q@땁@��@陚@�7@�p�@�z�@�!@�G�@�w@��@��@�v�@�@���@��@�^5@ޏ\@�7L@�\)@�(�@ӝ�@���@��#@�Ĝ@ϕ�@�~�@̴9@���@�;d@ʟ�@�&�@��y@�/@�|�@�^5@��^@���@�1@��u@�dZ@�S�@� �@�"�@�v�@�5?@��@���@�E�@�@���@�G�@��j@��@��P@���@���@���@��9@��@��@��@�+@��^@�  @�E�@�7L@�z�@��
@���@�\)@�o@��+@��@��h@�p�@�7L@�V@��/@�Ĝ@���@���@���@�bN@�  @�C�@��H@��@�&�@�Z@�ƨ@�+@���@�Z@�(�@���@��w@��P@�K�@���@�n�@�=q@�$�@�@��h@��@��/@���@� �@�33@�n�@���@�x�@�X@�7L@���@��@��H@���@�@���@�hs@�O�@�?}@�?}@�&�@�Ĝ@� �@��;@�dZ@�E�@�{@��#@��@�?}@��@��@��@�/@�?}@�hs@�`B@�?}@��@��u@�z�@�bN@�I�@�b@�1@��@�ƨ@��w@���@�t�@�S�@�C�@�+@�@��!@�-@�@�X@��@���@���@���@��/@��/@��/@���@��j@���@��u@��u@�z�@�9X@�  @��w@���@�|�@�;d@�
=@�
=@��@��!@���@��+@�~�@�n�@�^5@�V@�M�@�E�@�=q@�$�@���@��-@��^@���@��^@��7@�hs@�G�@�?}@�?}@�V@��@��@��@��
@���@��@�l�@�S�@�o@���@�v�@�=q@��@�{@��@�@��7@���@+@~�y@~�+@}��@|j@{ƨ@{�F@{��@{"�@z��@z=q@z-@z�@y��@y�#@y��@yhs@y7L@y%@x�`@x�u@xQ�@xA�@v��@nV@h �@aG�@[��@R��@NV@E�T@;�m@5V@.{@(A�@#@
=@=q@bN@/@��@��@�;@�!11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aϡ�A�~�A�hsA�Q�A�A�A�9XA�33A�-A�+A�+A�&�A�"�A��A��A�{A�bA�JA�%A�A�  A���A��A��AΕ�A�5?A���A���A�M�Aƛ�A��/A���A���A�{A��9A�9XA��A���A��DA�~�A�bNA��A�S�A�bA�{A�C�A��A�(�A�S�A�/A��FA�t�A�dZA�p�A���A��hA��;A�E�A��mA��^A��+A��9A���A�oA���A�1'A�XA�x�A��hA�VA��A��mA��+A�-A��A�A�ĜA��uA��Ap�A~z�A}��A}dZA{�mAz�Az{Ax�+At�As
=Aq�;Aq\)Ap�Ao��AoVAn  Al �Ag��Aet�Ac+A`�jA_hsA]�A[�hAZ��AZbAY�AW��AWC�AW�AV��AVffAU�mAU;dAU%AT�`AT��AT��AT9XAS&�AR-AQ�-AQl�AQK�AQ
=AP��AO�AO��AOC�ANbNAM�AM��AM7LAL(�AJ�RAH��AH��AH�AG��AFv�AFAEt�AD�DAC|�AB �A@��A@�jA?��A> �A<��A<�DA<1A;�-A;\)A;/A:�/A9��A7�PA6v�A6  A5��A5��A4�!A4{A2�A17LA0�DA/�7A.��A. �A-��A,JA+�A*n�A)��A(A�A'�A'
=A&ĜA&jA"��A"(�A"bA!��A �A bNA�AĜA$�A��A33A5?AdZAĜAdZAI�A/A��AXA�A��A�DA�A"�A��A�wA�DA�mAx�AdZA�A�HA�\A��A��A�A
1A	hsA	A�A��AXAz�A�TA&�A�A��A�A��AA bN@��@�ȴ@�dZ@�-@�?}@�b@���@�b@��y@�!@���@�=q@땁@��@陚@�7@�p�@�z�@�!@�G�@�w@��@��@�v�@�@���@��@�^5@ޏ\@�7L@�\)@�(�@ӝ�@���@��#@�Ĝ@ϕ�@�~�@̴9@���@�;d@ʟ�@�&�@��y@�/@�|�@�^5@��^@���@�1@��u@�dZ@�S�@� �@�"�@�v�@�5?@��@���@�E�@�@���@�G�@��j@��@��P@���@���@���@��9@��@��@��@�+@��^@�  @�E�@�7L@�z�@��
@���@�\)@�o@��+@��@��h@�p�@�7L@�V@��/@�Ĝ@���@���@���@�bN@�  @�C�@��H@��@�&�@�Z@�ƨ@�+@���@�Z@�(�@���@��w@��P@�K�@���@�n�@�=q@�$�@�@��h@��@��/@���@� �@�33@�n�@���@�x�@�X@�7L@���@��@��H@���@�@���@�hs@�O�@�?}@�?}@�&�@�Ĝ@� �@��;@�dZ@�E�@�{@��#@��@�?}@��@��@��@�/@�?}@�hs@�`B@�?}@��@��u@�z�@�bN@�I�@�b@�1@��@�ƨ@��w@���@�t�@�S�@�C�@�+@�@��!@�-@�@�X@��@���@���@���@��/@��/@��/@���@��j@���@��u@��u@�z�@�9X@�  @��w@���@�|�@�;d@�
=@�
=@��@��!@���@��+@�~�@�n�@�^5@�V@�M�@�E�@�=q@�$�@���@��-@��^@���@��^@��7@�hs@�G�@�?}@�?}@�V@��@��@��@��
@���@��@�l�@�S�@�o@���@�v�@�=q@��@�{@��@�@��7@���@+@~�y@~�+@}��@|j@{ƨ@{�F@{��@{"�@z��@z=q@z-@z�@y��@y�#@y��@yhs@y7L@y%@x�`@x�u@xQ�@xA�@v��@nV@h �@aG�@[��@R��@NV@E�T@;�m@5V@.{@(A�@#@
=@=q@bN@/@��@��@�;@�!11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBo�Bo�Bn�Bo�Bn�Bn�Bn�Bn�Bn�Bn�Bn�Bn�Bo�Bo�Bo�Bn�Bo�Bo�Bo�Bo�Bn�Bm�BjBdZB_;B\)BO�B:^B,B�B  B�'Br�B`BBp�Bk�Bq�B�B�Bx�Be`BW
BG�B:^B/B�BhBhB�B{BB�B�TB��B�FB��B��B�=Bw�Br�BffB[#BN�BI�BA�B49B$�B�B#�B�BB
��B
�B
�fB
�B
ÖB
�B
��B
��B
��B
�uB
�PB
� B
u�B
m�B
]/B
?}B
/B
$�B
�B
�B
\B
	7B	��B	�B	��B	�RB	��B	��B	��B	�hB	�1B	�B	~�B	w�B	u�B	u�B	s�B	p�B	t�B	z�B	y�B	z�B	z�B	{�B	z�B	y�B	u�B	o�B	jB	jB	iyB	hsB	gmB	e`B	cTB	`BB	]/B	\)B	ZB	W
B	Q�B	L�B	G�B	E�B	D�B	@�B	:^B	8RB	49B	0!B	)�B	#�B	!�B	 �B	�B	�B	{B	oB	bB	\B	PB	DB		7B	B��B��B��B��B��B�B�B�mB�NB�;B�#B�B��B��BȴBĜB�}B�XB�-B�B�B��B��B��B��B��B��B��B�{B�oB�VB�PB�DB�7B�+B�B�B� B}�By�Bv�Bu�Bu�Bt�Br�Bp�Bm�Bk�BhsBe`BdZBcTBcTBbNBaHB_;B\)BYBT�BQ�BO�BL�BI�BF�BD�BB�BA�B?}B?}B>wB=qB;dB9XB7LB6FB33B1'B0!B.B,B+B)�B(�B'�B$�B#�B#�B#�B#�B"�B"�B#�B(�B%�B$�B%�B&�B)�B/B1'B/B.B2-B5?B49B49B7LB5?B33B2-B1'B0!B0!B0!B0!B/B.B.B,B+B-B-B/B/B49B;dB=qBD�BB�BA�B?}B;dB<jB@�BC�BG�BF�BG�BH�BG�BG�BK�BL�BL�BL�BK�BL�BM�BO�BR�BXB[#B]/B`BB`BB`BB`BBbNBe`BffBgmBhsBhsBiyBiyBiyBiyBiyBiyBiyBm�Bm�Bp�Bu�Bx�Bz�B{�B�B�+B�7B�=B�DB�DB�DB�PB�\B�bB�bB�bB�oB��B��B��B��B��B��B��B��B��B��B�B�3B�RB�XB�}BBĜBĜBŢBŢBŢBɺB��B��B��B�BB�NB�ZB�sB�B�B�B�B�B��B	B	B	%B	PB	hB	oB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	#�B	&�B	&�B	(�B	+B	,B	-B	.B	/B	2-B	5?B	6FB	7LB	8RB	9XB	?}B	C�B	G�B	J�B	L�B	N�B	O�B	P�B	P�B	R�B	R�B	T�B	W
B	XB	YB	YB	ZB	ZB	\)B	]/B	_;B	cTB	cTB	dZB	ffB	iyB	l�B	n�B	p�B	p�B	u�B	{�B	{�B	� B	�B	�B	�%B	�%B	�%B	�+B	�7B	�DB	�DB	�JB	�DB	�DB	�DB	�DB	�PB	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�9B	��B	�ZB	��B
B
oB
�B
'�B
49B
9XB
A�B
H�B
O�B
T�B
[#B
]/B
aHB
dZB
jB
p�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Bo�Bo�Bn{Bo�Bn�Bn�Bn�Bn�Bn|Bn�Bn�Bn�Bo�Bo�Bo�BnBo�Bo�Bo�Bo�BnBmzBjfBdAB_B\BO�B:EB+�BxB��B�Br�B`#Bp�BkeBq�B��B��Bx�Be;BV�BG�B:=B.�BBFBHBdBXB�B�B�-B��B�#B��B��B�Bw�Br�BfBB[BN�BI�BAhB4B$�B�B#�B`B�B
��B
�B
�AB
��B
�tB
��B
��B
��B
�sB
�TB
�.B
�B
u�B
mtB
]B
?\B
.�B
$�B
�B
oB
=B
	B	��B	�hB	ʦB	�5B	��B	��B	�B	�OB	�B	��B	~�B	w�B	u�B	u�B	s�B	p�B	t�B	z�B	y�B	z�B	z�B	{�B	z�B	y�B	u�B	o�B	jdB	jdB	i`B	hXB	gSB	eFB	c9B	`'B	]B	\B	ZB	V�B	Q�B	L�B	G�B	E�B	D�B	@iB	:GB	89B	4!B	0B	)�B	#�B	!�B	 �B	�B	uB	cB	XB	IB	EB	9B	.B		B	B��B��B��B��B��B�B�B�VB�6B�#B�
B��B��B��BȟBĆB�gB�BB�B��B��B��B��B��B��B��B�xB�oB�gB�ZB�>B�<B�.B�#B�B�
B��B�B}�By�Bv�Bu�Bu�Bt�Br�Bp�BmBkpBhaBeMBdDBc?Bc@Bb8Ba5B_&B\BYBT�BQ�BO�BL�BI�BF�BD�BBzBAvB?kB?jB>eB=^B;RB9EB7:B64B3!B1B0B.B+�B*�B)�B(�B'�B$�B#�B#�B#�B#�B"�B"�B#�B(�B%�B$�B%�B&�B)�B/B1B/	B-�B2B5*B4%B4%B76B5+B3B2B1B0	B0B0B0B/B-�B-�B+�B*�B,�B,�B/B/B4"B;MB=ZBD�BBxBArB?gB;MB<RB@nBC~BG�BF�BG�BH�BG�BG�BK�BL�BL�BL�BK�BL�BM�BO�BR�BW�B[B]B`+B`(B`(B`'Bb4BeEBfPBgTBhYBh]Bi^Bi`Bi_Bi_BiaBi_Bi`BmxBmxBp�Bu�Bx�Bz�B{�B��B�B�B� B�+B�(B�)B�3B�CB�GB�HB�FB�SB�cB�cB�lB�xB��B��B��B��B��B��B��B�B�3B�8B�]B�oBĀB�BńBńBŁBɝB��B��B��B�"B�0B�8B�SB�eB�qB�~B��B�B��B	 �B	�B	B	/B	FB	MB	SB	fB	yB	yB	yB	xB	xB	xB	zB	}B	}B	~B	}B	�B	 �B	#�B	&�B	&�B	(�B	*�B	+�B	,�B	-�B	.�B	2	B	5B	6#B	7)B	80B	96B	?\B	CtB	G�B	J�B	L�B	N�B	O�B	P�B	P�B	R�B	R�B	T�B	V�B	W�B	X�B	X�B	Y�B	Y�B	\B	]B	_B	c0B	c0B	d8B	f@B	iUB	ldB	nsB	p}B	pB	u�B	{�B	{�B	�B	��B	��B	��B	��B	��B	�B	�B	�!B	�B	�$B	�B	� B	�B	�B	�*B	�EB	�NB	�OB	�\B	�nB	�uB	�uB	�vB	�sB	�zB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�1B	��B
 �B
DB
pB
'�B
4B
9,B
A^B
H�B
O�B
T�B
Z�B
]B
aB
d.B
jUB
pyB
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.36 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708272016053117082720160531170827  AO  ARCAADJP                                                                    20151018091552    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151018091552  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151018091552  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170827  IP                  G�O�G�O�G�O�                