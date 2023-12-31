CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-04-30T02:15:33Z AOML 3.0 creation; 2016-06-01T00:08:30Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20160430021533  20160531170830  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055_7112_149                   2C  D   APEX                            5374                            041511                          846 @רr��$1   @רsU��@;^vȴ9X�c���-V1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @9��@�  @�  A   A   A@  A`  A�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0ffB8  B?��BH  BP  BX  B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1�fD2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy` D��D�6fD�vfD�� D�#3D�S3D�vfD��fD�fD�33D���D��fD�3D�C3Dڌ�D��3D�fD�I�D�c3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @P��@��@˅AA%AEAeA��HA��A��HA��HA��HA��HA��HA��HBp�B	p�Bp�Bp�B!p�B)�
B1�
B9p�BA
>BIp�BQp�BYp�Ba
>Bip�Bqp�Byp�B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBĸRBȸRB̸RBиRBԸRBظRBܸRB�RB�RB�RB�RB�RB��RB��RB��RC \)C\)C\)C\)C\)C
\)C\)C\)C\)C\)C\)C\)C\)C\)C\)C\)C \)C"\)C$\)C&\)C(\)C*\)C,\)C.\)C0\)C2\)C4\)C6\)C8\)C:\)C<\)C>\)C@\)CB\)CD\)CF\)CH\)CJ\)CL\)CN\)CP\)CR\)CT\)CV\)CX\)CZ\)C\\)C^\)C`\)Cb\)Cd\)Cf\)Ch\)Cj\)Cl\)Cn\)Cp\)Cr\)Ct\)Cv\)Cx\)Cz\)C|\)C~\)C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.D 
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
D1�pD2
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
Dt�=Dyw
D�(RD�A�D���D���D�.�D�^�D���D���D��D�>�D��RD���D��D�N�DژRD�θD��D�UD�n�D��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��jA���A��jA��jA��^A��^A��^A��^A��jA��jA��jA��jA��jA��jA��wA��!A���A�dZA��mA�t�A��A��#A�ƨA��A���A�ĜA��A��wA��!A��`A���A��A��-A�VA���A��+A�=qA���A���A��9A���A�v�A�O�A��A�ƨA���A�1'A���A��;A�p�A�ȴA�p�A�/A�{A� �A�n�A���A�9XA�oA�ƨA���A�n�A�&�A���A�=qA���A��A� �A�ffA�p�A�A�A�ZA��/A�XA�z�A� �A���A��wA���A�A�A�?}A�Q�A��+A���A���A�I�A��PA�A��A�ffA� �A��A��#A���A�oA�~�A�A�\)A�C�A���A�~�A�33A�JA��yA��hA�ZA���A�+A��A���A�p�A��TA��A��DA~��A~v�A}�mA{/AwC�Au�At=qAr�Ap$�AnM�Al�jAkp�AjffAiO�Ah�/Af��Ae%Ac�Ac�Ab=qAa+A^��A]��A[�-AZ��AYK�AW��AU`BAUVATQ�ATJASoARE�AQ�hAQl�AQ�AP��API�ANM�AL�jAK|�AJ1'AHbAFbNAD^5AD1AC�-AA�A?\)A>1'A=hsA<�\A;|�A:�A:  A8ZA81A7ƨA7�hA7?}A6�RA5�A4�DA1�TA1��A1�A/�mA/p�A/VA.�A.�jA-\)A,�yA,VA+�FA+C�A*��A)�A)/A(1'A%�wA#�#A#|�A#oA"�A"��A!��A��A�wA��A��Az�A1'AS�A1'A"�A�AJAG�A�Ax�A�yA��A�A�\A1'A�;A\)AA�A��An�A`BA
-A	`BAr�AA�AAA��A|�AXA7LA
=A�/A��A~�A�A\)A�A5?A�;A&�A%A �yA Z@���@�C�@��-@�I�@�ȴ@���@��u@�\)@���@�S�@��@�I�@�+@�ff@���@�+@�-@�I�@�F@�\)@���@���@�  @��y@� �@�\)@�{@�G�@�;d@�hs@�(�@�S�@���@�;d@�v�@с@�z�@���@�K�@�^5@�Ĝ@�+@�5?@���@���@Ǯ@��#@�/@��m@�+@�{@�%@���@�|�@�
=@�v�@�x�@�A�@��@���@���@�ƨ@��@���@�v�@�J@�p�@�&�@�C�@�&�@�|�@��!@�n�@�O�@��j@��D@��@�Z@���@�t�@��@���@�E�@�x�@���@��;@�dZ@���@�@��@�&�@��@�G�@�z�@�ƨ@�33@�"�@��@�o@��@��!@��+@�v�@�E�@��@���@��#@��h@���@���@��@�ff@��^@�7L@�%@���@��/@��u@� �@���@��@��m@�ƨ@���@��P@�K�@���@��#@���@��/@���@��D@�z�@�Z@�9X@� �@���@�;d@�+@��@���@���@�ff@���@�x�@�&�@��9@��@�Z@���@���@�\)@�"�@��@��R@���@��@��h@�`B@�/@��@���@��@��`@���@��@�t�@���@�V@�5?@��@���@���@��@���@��j@���@��u@�Q�@�  @���@���@�"�@��@�n�@�J@��@��#@��7@�?}@�%@��`@��j@��9@��9@��@��u@�I�@���@�|�@�K�@�o@�ȴ@���@�v�@�V@�@���@��@�p�@�X@�G�@��@��@��u@�A�@�(�@��@�b@�1@��@���@���@�dZ@�33@��@��H@��+@�=q@�5?@��@�@���@�hs@�X@�?}@��@�Ĝ@��9@~ff@xQ�@o��@g��@a��@Z-@Sƨ@L�/@Dj@?�;@8��@1��@+S�@&v�@ A�@��@\)@�9@C�@��@V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��jA���A��jA��jA��^A��^A��^A��^A��jA��jA��jA��jA��jA��jA��wA��!A���A�dZA��mA�t�A��A��#A�ƨA��A���A�ĜA��A��wA��!A��`A���A��A��-A�VA���A��+A�=qA���A���A��9A���A�v�A�O�A��A�ƨA���A�1'A���A��;A�p�A�ȴA�p�A�/A�{A� �A�n�A���A�9XA�oA�ƨA���A�n�A�&�A���A�=qA���A��A� �A�ffA�p�A�A�A�ZA��/A�XA�z�A� �A���A��wA���A�A�A�?}A�Q�A��+A���A���A�I�A��PA�A��A�ffA� �A��A��#A���A�oA�~�A�A�\)A�C�A���A�~�A�33A�JA��yA��hA�ZA���A�+A��A���A�p�A��TA��A��DA~��A~v�A}�mA{/AwC�Au�At=qAr�Ap$�AnM�Al�jAkp�AjffAiO�Ah�/Af��Ae%Ac�Ac�Ab=qAa+A^��A]��A[�-AZ��AYK�AW��AU`BAUVATQ�ATJASoARE�AQ�hAQl�AQ�AP��API�ANM�AL�jAK|�AJ1'AHbAFbNAD^5AD1AC�-AA�A?\)A>1'A=hsA<�\A;|�A:�A:  A8ZA81A7ƨA7�hA7?}A6�RA5�A4�DA1�TA1��A1�A/�mA/p�A/VA.�A.�jA-\)A,�yA,VA+�FA+C�A*��A)�A)/A(1'A%�wA#�#A#|�A#oA"�A"��A!��A��A�wA��A��Az�A1'AS�A1'A"�A�AJAG�A�Ax�A�yA��A�A�\A1'A�;A\)AA�A��An�A`BA
-A	`BAr�AA�AAA��A|�AXA7LA
=A�/A��A~�A�A\)A�A5?A�;A&�A%A �yA Z@���@�C�@��-@�I�@�ȴ@���@��u@�\)@���@�S�@��@�I�@�+@�ff@���@�+@�-@�I�@�F@�\)@���@���@�  @��y@� �@�\)@�{@�G�@�;d@�hs@�(�@�S�@���@�;d@�v�@с@�z�@���@�K�@�^5@�Ĝ@�+@�5?@���@���@Ǯ@��#@�/@��m@�+@�{@�%@���@�|�@�
=@�v�@�x�@�A�@��@���@���@�ƨ@��@���@�v�@�J@�p�@�&�@�C�@�&�@�|�@��!@�n�@�O�@��j@��D@��@�Z@���@�t�@��@���@�E�@�x�@���@��;@�dZ@���@�@��@�&�@��@�G�@�z�@�ƨ@�33@�"�@��@�o@��@��!@��+@�v�@�E�@��@���@��#@��h@���@���@��@�ff@��^@�7L@�%@���@��/@��u@� �@���@��@��m@�ƨ@���@��P@�K�@���@��#@���@��/@���@��D@�z�@�Z@�9X@� �@���@�;d@�+@��@���@���@�ff@���@�x�@�&�@��9@��@�Z@���@���@�\)@�"�@��@��R@���@��@��h@�`B@�/@��@���@��@��`@���@��@�t�@���@�V@�5?@��@���@���@��@���@��j@���@��u@�Q�@�  @���@���@�"�@��@�n�@�J@��@��#@��7@�?}@�%@��`@��j@��9@��9@��@��u@�I�@���@�|�@�K�@�o@�ȴ@���@�v�@�V@�@���@��@�p�@�X@�G�@��@��@��u@�A�@�(�@��@�b@�1@��@���@���@�dZ@�33@��@��H@��+@�=q@�5?@��@�@���@�hs@�X@�?}@��@�Ĝ@��9@~ff@xQ�@o��@g��@a��@Z-@Sƨ@L�/@Dj@?�;@8��@1��@+S�@&v�@ A�@��@\)@�9@C�@��@V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBjBk�Bl�Bx�B�B�%B�+B��B�FB��B�B�B��B%BDBVB\BVB{B�B�BoB\BuBuBuBuBuBuB�B�B{BuBoBbB1B��B�B�`B�B�sB�;B�B�B��B�#B�/B�;B�fB�mB�TB�5B�B��B��B�'B��B� BQ�B&�B1'Bp�B�B�BjBdZBVBI�B@�B:^B1'B!�BoB%B��B�B�mB�BÖB�RB�!B��B��B��B��B�JBq�BH�B,B{BB
��B
�B
�BB
�B
��B
�9B
��B
��B
�%B
�B
x�B
aHB
@�B
33B
&�B
�B
  B	�B	�HB	�B	��B	��B	ŢB	�RB	�B	��B	��B	��B	�hB	�B	y�B	iyB	`BB	T�B	G�B	:^B	9XB	8RB	8RB	1'B	,B	&�B	)�B	)�B	'�B	#�B	�B	JB	B��B�B�mB�/B�B��B��B��B�jB�XB�9B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�bB�VB�JB�7B�Bz�Bv�Bu�Bt�Br�Bp�BjBe`BcTBbNBaHB`BB^5B[#BXBW
BT�BS�BQ�BN�BL�BJ�BH�BF�BE�BD�BB�B@�B<jB9XB7LB6FB49B2-B1'B1'B0!B0!B/B/B/B/B.B.B.B-B,B+B'�B%�B$�B#�B#�B"�B!�B �B �B�B�B�B�B�B�B�B�B�B�B�B�B{B{B�B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B$�B%�B#�B!�B�B�B�B�B�B�B�B�B�B�B �B"�B#�B%�B&�B(�B(�B)�B,B1'B33B49B49B8RB:^B:^B<jB=qB=qB=qB=qB>wB?}B@�BA�BB�BD�BF�BI�BM�BS�BS�BW
BYB]/BaHBe`BgmBiyBiyBiyBiyBjBk�Bl�Bl�Bl�Bm�Bn�Bo�Bp�Bu�B{�B{�B}�B�%B�1B�1B�1B�1B�7B�PB�VB�\B�\B�bB�uB��B��B��B��B��B��B��B�B�B�B�B�B�!B�3B�3B�9B�9B�9B�LB�^B�qB�}BÖBĜBŢB��B��B��B��B��B��B��B�B�)B�/B�5B�;B�;B�BB�BB�HB�`B�yB�B�B�B�B��B��B��B	  B	B	B	B	%B	DB	DB	PB	uB	{B	�B	�B	�B	�B	�B	"�B	$�B	%�B	&�B	&�B	&�B	&�B	&�B	(�B	,B	1'B	33B	5?B	9XB	:^B	<jB	=qB	@�B	C�B	D�B	E�B	F�B	F�B	H�B	J�B	M�B	P�B	Q�B	R�B	R�B	R�B	S�B	T�B	W
B	YB	\)B	\)B	_;B	cTB	ffB	ffB	gmB	hsB	m�B	n�B	o�B	p�B	s�B	u�B	v�B	�B	��B	�jB	�B	�B	��B
JB
uB
�B
&�B
0!B
9XB
A�B
G�B
N�B
T�B
]/B
e`B
k�B
p�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bi[Bi[BiWBiWBiWBiWBi[BiWBiWBi[BiWBiUBiWBi[BiWBjbBkgBllBx�B��B�B�B��B�+B��B�fB�B��BB)B8BAB8B\BtBaBSBABVBVBYBSB[BVBaBfBZBXBQBABB��B�rB�CB�wB�TB�B��B��B��B�B�B�B�HB�OB�0B�B��B̬B�cB�	B��B�BQ�B&�B1Bp�B��B��BjaBd6BU�BI�B@^B:<B1B!�BKBB��B�B�HB��B�qB�.B��B��B��B��B�cB�(Bq�BH�B+�BZB�B
��B
�B
� B
��B
ϻB
�B
��B
�qB
�B
��B
x�B
a&B
@cB
3B
&�B
}B	��B	�B	�+B	��B	��B	ʦB	ŇB	�6B	��B	��B	��B	�~B	�NB	�B	y�B	i^B	`(B	T�B	G�B	:GB	9>B	8:B	8:B	1B	+�B	&�B	)�B	)�B	'�B	#�B	oB	3B	�B��B�B�VB�B�B��B˰B�uB�VB�BB�(B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�~B��B�rB�hB�aB�[B�PB�@B�7B�#B�Bz�Bv�Bu�Bt�Br�Bp�BjlBeNBc>Bb<Ba7B`0B^#B[BW�BV�BT�BS�BQ�BN�BL�BJ�BH�BF�BE�BD�BBzB@qB<VB9+B7;B64B4'B2B1B1B0B0B/B.�B/
B/
B.B.B.B,�B+�B*�B'�B%�B$�B#�B#�B"�B!�B �B �B�B�B~BzB�BoB_BzB[BUBqBTBfBPBRBNB[BZBZBTBYBuBnB~B�B�B�BqB�B�BB�B�B�BfB�BfB�B�BwBwB"�B$�B%�B#�B!�B�B�B�B�B�B�B�B�B�B�B �B"�B#�B%�B&�B(�B(�B)�B+�B1B3B4#B4%B8<B:GB:EB<SB=WB=^B=[B=ZB>`B?eB@lBApBBwBD�BF�BI�BM�BS�BS�BV�BX�B]Ba0BeFBgUBiaBiaBi^Bi_BjhBkjBlpBlsBlrBmvBn~Bo�Bp�Bu�B{�B{�B}�B�	B�B�B�B�B�B�6B�9B�@B�@B�GB�ZB�hB�yB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�/B�AB�UB�^B�xBāBłBͳB��B��B��B��B��B��B��B�	B�B�B�B�B�"B�"B�(B�>B�[B�vB�B�B�B��B��B��B��B	 �B	�B	�B	B	!B	#B	/B	TB	YB	eB	}B	�B	�B	�B	"�B	$�B	%�B	&�B	&�B	&�B	&�B	&�B	(�B	+�B	1B	3B	5B	94B	:;B	<IB	=MB	@`B	CsB	DzB	E�B	F�B	F�B	H�B	J�B	M�B	P�B	Q�B	R�B	R�B	R�B	S�B	T�B	V�B	X�B	\B	\B	_B	c.B	fAB	fAB	gHB	hPB	mlB	nrB	oyB	p~B	s�B	u�B	v�B	��B	��B	�EB	��B	�pB	��B
!B
MB
�B
&�B
/�B
9.B
A^B
G�B
N�B
T�B
]B
e7B
kZB
pwB
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.36 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708302016053117083020160531170830  AO  ARCAADJP                                                                    20160430021533    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160430021533  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160430021533  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170830  IP                  G�O�G�O�G�O�                