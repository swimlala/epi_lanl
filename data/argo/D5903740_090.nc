CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-09-05T16:11:36Z AOML 3.0 creation; 2016-06-01T00:08:20Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140905161136  20160531170820  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ZA   AO  4055_7112_090                   2C  D   APEX                            5374                            041511                          846 @�aN��1   @�a��?�@9�9Xb�d��O�;1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ZA   A   A   @�ff@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0ffB7��B@  BH  BP  BX  B`  Bh  BpffBw33B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX�fDY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dy� D�fD�L�D��fD��fD�3D�<�D�l�D���D�fD�P D�<�D���D���D�9�Dڙ�D��3D�  D�FfD�y�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@˅A(�A%AEAeA��HA��HA��HA��HA��HA��HA��HA��HBp�B	p�Bp�Bp�B!p�B)�
B1�
B9
>BAp�BIp�BQp�BYp�Bap�Bip�Bq�
Bx��B��B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBĸRBȸRB̸RBиRBԸRBظRBܸRB�RB�RB�RB�RB�RB��RB��RB��RC \)C\)C\)C\)C\)C
\)C\)C\)C\)C\)C\)C\)C\)C\)C\)C\)C \)C"\)C$\)C&\)C(\)C*\)C,\)C.\)C0\)C2\)C4\)C6\)C8\)C:\)C<\)C>\)C@\)CB\)CD\)CF\)CH\)CJ\)CL\)CN\)CP\)CR\)CT\)CV\)CX\)CZ\)C\\)C^\)C`\)Cb\)Cd\)Cf\)Ch\)Cj\)Cl\)Cn\)Cp\)Cr\)Ct\)Cv\)Cx\)Cz\)C|\)C~\)C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�:�C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�!GC�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.D 
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
DW
DW�
DX
DX�pDY
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
Dl�
Dm
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
Dt��Dy�
D�!�D�XRD���D���D��D�HRD�xRD��RD�!�D�[�D�HRD��RD�RD�EDڥD�޸D��D�Q�D�D�˅11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��/A���Aۛ�AۮA���A�^5A���A���Aں^A�~�A�/A�$�A�&�A�7LA�33A��A�oA��A���A׺^A�G�A�  A��#A�{A�XA�5?A�XAʸRAɑhAȼjA�`BA\A�ZA���A��-A��
A��A��HA��RA��+A��#A�`BA�ȴA�jA�A�bNA���A���A�M�A���A���A�n�A�bA�;dA�K�A���A�bNA���A��+A��
A�hsA�%A�bA���A���A�C�A���A�dZA�(�A�{A���A��\A�v�A�bNA�`BA�S�A�1A��^A���A�|�A�VA�oA�33A�1'A��A��A�  A�dZA��uA���A�VA�~�A�M�A��A�  A���A���A��A�?}A��A�(�A�bA��A���A���A���A��7A��7A��+A�Q�A�&�A�dZA�33A��A�t�A�S�A��#A~�9AzI�Az�Ay+Aw�Aw��Av�DAt��Ao��An�DAn5?Am��Ak��AjQ�Ai?}Ah�+Ah=qAh1Af�9Ad��Ad(�Ab�DA^-A\^5A[�FAZ�HAX�AWx�AW�AVz�AV�AU��AU;dAT��ATv�ASK�AQ�AP�AO�wAKhsAJ��AJr�AJ9XAJ1AI��AF�DAE�AC�;AA�wA?A>1'A=dZA<�HA<A:�A:�A9�A9p�A8�A8$�A77LA6�A6v�A5�A4ĜA2�A0ffA/VA.��A-��A,�\A+�wA*��A*  A)A)l�A)O�A)VA(��A(�\A(�A(r�A(E�A(5?A(1'A(�A'�FA&bNA$��A#XA"�/A"bNA"JA!��A!��A!�TA!�-A!�PA 9XA%An�A�9A�#A33A��Al�A�jA�A�An�A�AAAffA`BA��AjA�A�hA�uAt�A
��A	A�mAz�AO�A�A��A��AffA|�AJAhsA+A �RA jA (�@���@�;d@�v�@���@���@���@�/@�dZ@��@�v�@�K�@�7@�I�@� �@�P@�
=@��@�hs@�j@��u@�I�@���@ߝ�@�\)@��H@�^5@�J@݁@ܴ9@ۅ@ؼj@��@�E�@���@���@�^5@Ѻ^@���@�@�S�@�p�@ț�@ț�@ȋD@��@�%@��@��@�M�@�J@��@��#@��@�?}@��@� �@�\)@��`@�"�@���@��@�x�@��`@��@���@�j@��F@��P@�l�@�+@��+@��@���@���@��P@�
=@�-@�V@��@��u@�  @��!@��@�@�x�@���@�z�@��@��R@��@�b@�l�@�n�@��@�j@���@�M�@���@�?}@�(�@�K�@��R@�^5@���@�p�@��9@�K�@��!@�~�@�~�@�E�@�@��T@��h@���@�j@�(�@��@��m@��@���@���@��P@��@��@�33@���@��@���@�M�@��#@��-@��h@�X@���@�Ĝ@���@�bN@�I�@�1'@�1@��m@���@��@���@��@���@�?}@��`@���@��D@�j@�I�@���@��
@��w@��F@���@�t�@�S�@�o@�o@�o@�
=@��H@�^5@��@���@�7L@�%@���@�Ĝ@��9@���@��@�Z@� �@��@��F@���@�dZ@�o@���@��!@���@�v�@�=q@�{@��@��#@���@�/@�V@���@��`@��D@� �@�  @��
@��@�"�@�V@�{@��@���@���@��^@��@�&�@���@�(�@��@K�@~��@~�+@}��@|�/@|z�@|9X@{ƨ@{dZ@{@z�@z^5@z-@y��@yhs@y7L@y%@x��@x�`@x��@xĜ@xr�@rJ@kt�@c�F@Z~�@U�@L�/@E`B@<��@7;d@2=q@-p�@(A�@"M�@��@r�@33@+@��@�@�@�711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��/A���Aۛ�AۮA���A�^5A���A���Aں^A�~�A�/A�$�A�&�A�7LA�33A��A�oA��A���A׺^A�G�A�  A��#A�{A�XA�5?A�XAʸRAɑhAȼjA�`BA\A�ZA���A��-A��
A��A��HA��RA��+A��#A�`BA�ȴA�jA�A�bNA���A���A�M�A���A���A�n�A�bA�;dA�K�A���A�bNA���A��+A��
A�hsA�%A�bA���A���A�C�A���A�dZA�(�A�{A���A��\A�v�A�bNA�`BA�S�A�1A��^A���A�|�A�VA�oA�33A�1'A��A��A�  A�dZA��uA���A�VA�~�A�M�A��A�  A���A���A��A�?}A��A�(�A�bA��A���A���A���A��7A��7A��+A�Q�A�&�A�dZA�33A��A�t�A�S�A��#A~�9AzI�Az�Ay+Aw�Aw��Av�DAt��Ao��An�DAn5?Am��Ak��AjQ�Ai?}Ah�+Ah=qAh1Af�9Ad��Ad(�Ab�DA^-A\^5A[�FAZ�HAX�AWx�AW�AVz�AV�AU��AU;dAT��ATv�ASK�AQ�AP�AO�wAKhsAJ��AJr�AJ9XAJ1AI��AF�DAE�AC�;AA�wA?A>1'A=dZA<�HA<A:�A:�A9�A9p�A8�A8$�A77LA6�A6v�A5�A4ĜA2�A0ffA/VA.��A-��A,�\A+�wA*��A*  A)A)l�A)O�A)VA(��A(�\A(�A(r�A(E�A(5?A(1'A(�A'�FA&bNA$��A#XA"�/A"bNA"JA!��A!��A!�TA!�-A!�PA 9XA%An�A�9A�#A33A��Al�A�jA�A�An�A�AAAffA`BA��AjA�A�hA�uAt�A
��A	A�mAz�AO�A�A��A��AffA|�AJAhsA+A �RA jA (�@���@�;d@�v�@���@���@���@�/@�dZ@��@�v�@�K�@�7@�I�@� �@�P@�
=@��@�hs@�j@��u@�I�@���@ߝ�@�\)@��H@�^5@�J@݁@ܴ9@ۅ@ؼj@��@�E�@���@���@�^5@Ѻ^@���@�@�S�@�p�@ț�@ț�@ȋD@��@�%@��@��@�M�@�J@��@��#@��@�?}@��@� �@�\)@��`@�"�@���@��@�x�@��`@��@���@�j@��F@��P@�l�@�+@��+@��@���@���@��P@�
=@�-@�V@��@��u@�  @��!@��@�@�x�@���@�z�@��@��R@��@�b@�l�@�n�@��@�j@���@�M�@���@�?}@�(�@�K�@��R@�^5@���@�p�@��9@�K�@��!@�~�@�~�@�E�@�@��T@��h@���@�j@�(�@��@��m@��@���@���@��P@��@��@�33@���@��@���@�M�@��#@��-@��h@�X@���@�Ĝ@���@�bN@�I�@�1'@�1@��m@���@��@���@��@���@�?}@��`@���@��D@�j@�I�@���@��
@��w@��F@���@�t�@�S�@�o@�o@�o@�
=@��H@�^5@��@���@�7L@�%@���@�Ĝ@��9@���@��@�Z@� �@��@��F@���@�dZ@�o@���@��!@���@�v�@�=q@�{@��@��#@���@�/@�V@���@��`@��D@� �@�  @��
@��@�"�@�V@�{@��@���@���@��^@��@�&�@���@�(�@��@K�@~��@~�+@}��@|�/@|z�@|9X@{ƨ@{dZ@{@z�@z^5@z-@y��@yhs@y7L@y%@x��@x�`@x��@xĜ@xr�@rJ@kt�@c�F@Z~�@U�@L�/@E`B@<��@7;d@2=q@-p�@(A�@"M�@��@r�@33@+@��@�@�@�711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�`B�`B�sBB�BuB{B �B�B�B�B�B�B"�B%�B%�B)�B'�BoBB�B�BDB
=B+BBBB��B��B�B�B��B��BǮB�}B�LB�B��B�hB�+B~�B{�By�Bt�Bn�B]/BI�BB�B?}B49B�B��B�yB�;B�
B��B��BɺBǮBǮBɺB��BǮB�dB�'B��B�VB�B�B�B�B� B~�B~�B}�Bz�Bv�Bt�Bq�Bn�BhsBZBI�BG�BB�B2-B$�B�BDB��B�B�B�TB��B�^B��B�DBp�B`BBF�B/B�BuB
=BBBBB
��B
�TB
B
�B
��B
�bB
�PB
�B
iyB
F�B
C�B
<jB
33B
/B
$�B
oB	�B	�B	�mB	�HB	��B	��B	ƨB	��B	�wB	�jB	�3B	��B	��B	��B	�B	w�B	r�B	m�B	cTB	]/B	\)B	ZB	YB	W
B	T�B	R�B	P�B	J�B	C�B	>wB	5?B	!�B	�B	�B	�B	�B	�B	1B	B��B�B�yB�ZB�TB�TB�HB�;B�5B�)B�B�B�B��B��B��B��BǮB��B�dB�LB�9B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�JB�=B�7B�1B�1B�1B�1B�%B�B�B}�By�Bt�Bq�Bo�Bl�BiyBffBdZBbNB]/BXBVBQ�BN�BM�BL�BJ�BI�BG�BD�BA�B>wB;dB7LB5?B5?B49B33B2-B1'B/B0!B0!B/B.B.B,B+B)�B&�B"�B"�B �B�B�B�B�B�B�B�B�B�B�B!�B!�B"�B#�B"�B#�B#�B#�B$�B$�B$�B%�B$�B$�B'�B,B-B.B1'B1'B2-B1'B1'B2-B33B49B33B2-B0!B0!B0!B1'B2-B2-B2-B2-B2-B2-B2-B2-B1'B33B6FB6FB7LB8RB9XB:^B:^B:^B<jB<jB<jB<jB=qB>wB?}B@�BD�BE�BH�BK�BK�BL�BM�BR�BVBVBW
BXBZB]/B`BBiyBm�Bp�Bu�B|�B� B�B�JB�VB�hB��B��B��B��B��B��B�B�FB�dB�jB�jB�wB��B��BBȴB��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�)B�BB�HB�NB�TB�mB�sB�yB�B�B�B�B�B�B�B��B	  B	B	%B		7B	JB	PB	VB	\B	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	%�B	'�B	,B	.B	0!B	0!B	1'B	1'B	33B	49B	6FB	7LB	9XB	:^B	;dB	>wB	?}B	B�B	B�B	C�B	F�B	G�B	H�B	I�B	J�B	N�B	O�B	P�B	P�B	S�B	W
B	XB	YB	[#B	^5B	e`B	gmB	gmB	hsB	hsB	iyB	jB	m�B	o�B	t�B	w�B	x�B	y�B	{�B	}�B	�B	�B	�B	�B	�%B	�+B	�1B	�=B	�DB	�JB	�VB	�VB	�\B	�\B	�bB	�bB	�bB	�oB	��B	�?B	��B	�B	��B
JB
�B
&�B
/B
7LB
>wB
G�B
O�B
W
B
\)B
cTB
gmB
k�B
o�B
s�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�MB�LB�aB�BnBaBfB �B�B�B�B�B�B"�B%�B%�B)�B'�B]BB�B�B/B
'BB	B�B�B��B��B�xB� B��B˲BǖB�dB�0B��B��B�OB�B~�B{�By�Bt�Bn}B]BI�BBqB?`B4BcB��B�YB�B��BκBʟBɝBǍBǐBɚBʹBǋB�BB�B�|B�5B�B� B��B��B�B~�B~�B}�Bz�Bv�Bt�Bq�BnwBhRBY�BI�BG�BBjB2B$�BjB B��B�~B�aB�0BϼB�<B��B�"Bp�B`!BF�B.�ByBTB
B�B�B�B �B
��B
�3B
�pB
��B
�_B
�AB
�/B
��B
iXB
F�B
CxB
<LB
3B
.�B
$�B
PB	�B	�_B	�QB	�.B	��B	̰B	ƎB	�lB	�\B	�NB	�B	��B	��B	�nB	��B	w�B	r�B	mvB	c9B	]B	\B	ZB	X�B	V�B	T�B	R�B	P�B	J�B	C{B	>^B	5%B	!�B	�B	�B	�B	�B	{B	B	 �B��B�B�cB�EB�>B�AB�4B�&B�!B�B�B��B��B��B��B��BʬBǘB�lB�PB�8B�%B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�zB�TB�5B�)B�$B�B�B�B�B�B�	B��B}�By�Bt�Bq�Bo�BlwBihBfTBdHBb;B]BW�BU�BQ�BN�BM�BL�BJ�BI�BG�BD�BAvB>fB;SB7;B5+B5-B4)B3!B2B1B/B0B0B/B-�B. B+�B*�B)�B&�B"�B"�B �B�B�B�B|BlB�B�B�B�B�B!�B!�B"�B#�B"�B#�B#�B#�B$�B$�B$�B%�B$�B$�B'�B+�B,�B-�B1B1B2B1B1B2B3 B4"B3B2B0B0B0B1B2B2B2B2B2B2B2B2B1B3B6.B6/B76B8>B9BB:IB:IB:HB<UB<RB<QB<TB=]B>_B?hB@jBD�BE�BH�BK�BK�BL�BM�BR�BU�BU�BV�BW�BZB]B`,Bi^BmxBp�Bu�B|�B�B��B�.B�:B�MB�qB��B��B��B��B��B��B�(B�HB�LB�KB�YB�dB�bB�rBȖB˨BͶBκB��B��B��B��B��B��B��B��B��B��B��B�B�%B�'B�0B�6B�NB�QB�\B�dB�nB�pB�xB�~B��B�B��B��B	�B	B		B	&B	.B	5B	<B	MB	\B	`B	`B	eB	oB	yB	�B	�B	�B	�B	�B	!�B	%�B	'�B	+�B	-�B	/�B	/�B	1B	1B	3B	4B	6"B	7*B	94B	:<B	;AB	>RB	?ZB	BlB	BlB	CsB	F�B	G�B	H�B	I�B	J�B	N�B	O�B	P�B	P�B	S�B	V�B	W�B	X�B	Z�B	^B	e;B	gHB	gIB	hPB	hPB	iUB	jZB	mnB	ozB	t�B	w�B	x�B	y�B	{�B	}�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�&B	�0B	�1B	�6B	�6B	�=B	�>B	�<B	�FB	��B	�B	ͫB	�]B	��B
!B
rB
&�B
.�B
7 B
>LB
G�B
O�B
V�B
[�B
c+B
gCB
kZB
orB
s�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.36 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708202016053117082020160531170820  AO  ARCAADJP                                                                    20140905161136    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140905161136  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140905161136  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170820  IP                  G�O�G�O�G�O�                