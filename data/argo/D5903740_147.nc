CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-04-09T19:16:11Z AOML 3.0 creation; 2016-06-01T00:08:30Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20160409191611  20160531170830  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055_7112_147                   2C  D   APEX                            5374                            041511                          846 @ףS6��1   @ףS��6�@:����F�c�z�G�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDy` D�3D�S3D���D���D�	�D�<�D�p D���D���D�C3D��3D�� D� D�C3Dړ3D��3D���D�&fD�y�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @P��@��@˅AA%AEAeA��HA�{A��HA��HA��HA��HA��HA��HBp�B	p�Bp�Bp�B!p�B)p�B1p�B9p�BAp�BIp�BQp�BYp�Bap�Bip�Bqp�Byp�B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBĸRBȸRB̸RBиRBԸRBظRBܸRB�RB�RB�RB�RB�RB��RB��RB��RC \)C\)C\)C\)C\)C
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
Dt}pDyw
D��D�^�D��D��D�D�HRD�{�D��RD�RD�N�D���D�˅D��D�N�Dڞ�D�޸D�D�1�D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�%A�JA�A�A���A�$�A���A�r�A�p�A�33A��A�+A�=qA�?}A�9XA�;dA�;dA�;dA�9XA�1'A�33A�1'A�33A�1'A�-A�-A�$�A�oA�{A��#A��+A�G�A��A��A�?}A��mA�`BA�bA���A��A�x�A�dZA�Q�A�G�A���A���A�\)A�&�A���A���A�dZA�A�O�A��jA��A�33A���A��A���A�\)A�1'A�E�A�ȴA�~�A�ffA�+A�A�^5A��#A��jA�/A�G�A�p�A��PA�A�A�r�A�=qA��RA��A���A�x�A�M�A�r�A���A�x�A�7LA��yA�Q�A��A�(�A��`A��\A���A���A�1A���A�M�A��A�VA��yA�Q�A�ȴA���A��\A��wA��A�-A�`BA�A�A��A�
=A�JA�ƨA�33A��A�&�A�XA�"�A��`A���A�I�A��uA�5?A}33A|ĜA{t�Av  As��As/Ar1'ApE�Ak�#Aj�Ag�Ac�Aa�A`��A`A^�`A\A[33AZȴAZbNAZ(�AY�;AY�AYVAX��AW��AWp�AVQ�AS��AR��AQ
=AO��AN5?AK�PAF��AF9XAE�PAD��AD��AC��AB�HAB-AA�7A@��A@A>��A=�A<�`A<�uA;�^A:Q�A7�
A6�+A5�-A5
=A4�A49XA4{A3��A3/A2��A2A1S�A0�RA/�
A//A.VA-`BA+oA)��A'��A&�9A&�A%�A%�A$�HA$��A$~�A$r�A$I�A#l�A"z�A!�wA!�A!%A;dAJA��AhsA��A�A�A�A��AAQ�A�hA��A=qA�wA|�A�A��A��AffA�-A�hA%A�7A
�jA
VA	�A	�-A	/A��A �A�\AO�AA n�@�o@��y@���@��@�r�@�l�@��\@��7@�S�@�O�@�dZ@��T@�z�@��/@���@��@�\)@�@��y@�+@�@��/@�1@�S�@�^5@��@��T@�7@��`@�I�@ߥ�@�V@�r�@�1@��/@�+@�ȴ@�E�@��@�l�@ӝ�@��y@�@�Z@Ο�@ʏ\@���@ȣ�@ȃ@�;d@���@�o@���@��y@�Q�@��!@�@�`B@�%@��u@�|�@�
=@��y@���@���@��@�p�@�1@�o@�n�@��@�1'@�~�@��@�z�@��@��+@��7@�7L@���@�r�@�Z@�I�@� �@��w@�v�@���@��7@�j@��;@���@�ƨ@�ƨ@�S�@��@��h@��7@��@��@��7@��@�{@��T@���@�@���@�O�@���@�(�@�S�@�E�@���@�x�@�%@�I�@�S�@��!@��+@�-@���@�X@���@�9X@��
@�"�@�$�@��^@���@��m@���@�S�@�+@���@�E�@��#@��@�9X@� �@���@�+@��@��!@�ff@�-@�@��#@���@�@�hs@�`B@�?}@��/@��D@�Q�@�b@��F@�l�@�33@���@�E�@�-@�@��@�X@��`@���@�Q�@��@�b@��@�t�@��@��@��H@��R@��!@���@���@�~�@�M�@��@���@��#@��h@�hs@�X@�?}@�/@�/@��@��`@���@�bN@�I�@�9X@�1'@� �@�  @��
@��P@��@�l�@�;d@�33@�"�@�@���@��!@��+@�^5@�M�@�=q@���@��#@��^@��7@��@�x�@�hs@�`B@�O�@�/@�%@��@���@���@�j@�bN@�Q�@�I�@�(�@�@;d@~�R@~V@~ff@~{@}��@}�h@}�@|Z@{��@{�m@{t�@z�@z-@v��@p1'@i&�@`b@X�9@S��@J�!@B-@<��@7�@1�@-V@(��@"�\@/@&�@V@�H@�w@33@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�%A�JA�A�A���A�$�A���A�r�A�p�A�33A��A�+A�=qA�?}A�9XA�;dA�;dA�;dA�9XA�1'A�33A�1'A�33A�1'A�-A�-A�$�A�oA�{A��#A��+A�G�A��A��A�?}A��mA�`BA�bA���A��A�x�A�dZA�Q�A�G�A���A���A�\)A�&�A���A���A�dZA�A�O�A��jA��A�33A���A��A���A�\)A�1'A�E�A�ȴA�~�A�ffA�+A�A�^5A��#A��jA�/A�G�A�p�A��PA�A�A�r�A�=qA��RA��A���A�x�A�M�A�r�A���A�x�A�7LA��yA�Q�A��A�(�A��`A��\A���A���A�1A���A�M�A��A�VA��yA�Q�A�ȴA���A��\A��wA��A�-A�`BA�A�A��A�
=A�JA�ƨA�33A��A�&�A�XA�"�A��`A���A�I�A��uA�5?A}33A|ĜA{t�Av  As��As/Ar1'ApE�Ak�#Aj�Ag�Ac�Aa�A`��A`A^�`A\A[33AZȴAZbNAZ(�AY�;AY�AYVAX��AW��AWp�AVQ�AS��AR��AQ
=AO��AN5?AK�PAF��AF9XAE�PAD��AD��AC��AB�HAB-AA�7A@��A@A>��A=�A<�`A<�uA;�^A:Q�A7�
A6�+A5�-A5
=A4�A49XA4{A3��A3/A2��A2A1S�A0�RA/�
A//A.VA-`BA+oA)��A'��A&�9A&�A%�A%�A$�HA$��A$~�A$r�A$I�A#l�A"z�A!�wA!�A!%A;dAJA��AhsA��A�A�A�A��AAQ�A�hA��A=qA�wA|�A�A��A��AffA�-A�hA%A�7A
�jA
VA	�A	�-A	/A��A �A�\AO�AA n�@�o@��y@���@��@�r�@�l�@��\@��7@�S�@�O�@�dZ@��T@�z�@��/@���@��@�\)@�@��y@�+@�@��/@�1@�S�@�^5@��@��T@�7@��`@�I�@ߥ�@�V@�r�@�1@��/@�+@�ȴ@�E�@��@�l�@ӝ�@��y@�@�Z@Ο�@ʏ\@���@ȣ�@ȃ@�;d@���@�o@���@��y@�Q�@��!@�@�`B@�%@��u@�|�@�
=@��y@���@���@��@�p�@�1@�o@�n�@��@�1'@�~�@��@�z�@��@��+@��7@�7L@���@�r�@�Z@�I�@� �@��w@�v�@���@��7@�j@��;@���@�ƨ@�ƨ@�S�@��@��h@��7@��@��@��7@��@�{@��T@���@�@���@�O�@���@�(�@�S�@�E�@���@�x�@�%@�I�@�S�@��!@��+@�-@���@�X@���@�9X@��
@�"�@�$�@��^@���@��m@���@�S�@�+@���@�E�@��#@��@�9X@� �@���@�+@��@��!@�ff@�-@�@��#@���@�@�hs@�`B@�?}@��/@��D@�Q�@�b@��F@�l�@�33@���@�E�@�-@�@��@�X@��`@���@�Q�@��@�b@��@�t�@��@��@��H@��R@��!@���@���@�~�@�M�@��@���@��#@��h@�hs@�X@�?}@�/@�/@��@��`@���@�bN@�I�@�9X@�1'@� �@�  @��
@��P@��@�l�@�;d@�33@�"�@�@���@��!@��+@�^5@�M�@�=q@���@��#@��^@��7@��@�x�@�hs@�`B@�O�@�/@�%@��@���@���@�j@�bN@�Q�@�I�@�(�@�@;d@~�R@~V@~ff@~{@}��@}�h@}�@|Z@{��@{�m@{t�@z�@z-@v��@p1'@i&�@`b@X�9@S��@J�!@B-@<��@7�@1�@-V@(��@"�\@/@&�@V@�H@�w@33@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B�
B�B�B�;B�`B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B+BVBoB �B&�B.B8RB>wBC�BD�BE�BD�BD�BC�BA�B=qB?}B?}B?}BC�BC�B?}B>wB8RB(�B!�B�BoB\BDB��BoB�BoBhB\BVBBB+B��B�TB��B�XB�9B�B��B�uB�oB�BdZBP�BC�B@�B:^B5?B0!B'�B!�B�B#�B)�B�B!�B�BuBVBVB	7B+B��B��B�dB��B�JBx�BiyB`BB]/BS�BC�B-B
��B
�#B
�XB
��B
��B
�uB
�{B
�{B
�hB
�1B
s�B
\)B
VB
C�B
{B
B	��B	�yB	��B	�B	��B	�B	cTB	Q�B	I�B	B�B	9XB	2-B	/B	/B	0!B	2-B	33B	49B	6FB	>wB	D�B	>wB	2-B	�B		7B��B�B�TB��BɺBǮBƨBŢBĜBÖBBBBB��BĜBBB��B�}B�jB�dB�XB�RB�LB�FB�?B�?B�9B�3B�-B�!B�B�B��B��B��B��B�PB�7B�B� B}�B{�By�By�Bx�Bw�Bw�Bv�Bs�Bq�Bp�Bn�Bl�BiyBffBcTB_;B\)BYBT�BO�BK�BH�BE�BC�BB�BA�B@�B?}B>wB=qB=qB=qB<jB:^B8RB6FB5?B49B33B2-B0!B.B+B$�B �B�B�B�B�B�B�B�B�B�B�BuBoBoBuBhB{BoB{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B{B�B�BoB�B�B�B�B�BhBVBhBhBbBbBhBuB�B�B�B�B �B!�B!�B!�B$�B%�B%�B%�B%�B%�B%�B(�B+B+B/B1'B6FB;dB;dB?}B?}BB�BC�BD�BE�BE�BE�BE�BE�BI�BK�BK�BO�BR�BR�BR�BQ�BS�BZB_;B`BBaHBbNBdZBhsBm�Br�Bu�Bw�Bw�By�B{�B� B�B�+B�=B�DB�PB�bB�{B��B��B��B��B��B��B�B�B�'B�LB�RB�qBBBÖBĜBǮB��B��B�#B�BB�`B�B�B��B��B��B��B��B	  B	B	B	B	B	+B	1B		7B	
=B	DB	DB	JB	hB	uB	�B	�B	�B	�B	�B	$�B	&�B	)�B	,B	,B	-B	0!B	2-B	33B	49B	6FB	6FB	6FB	6FB	8RB	:^B	<jB	=qB	>wB	B�B	D�B	D�B	F�B	F�B	F�B	G�B	H�B	K�B	N�B	O�B	P�B	P�B	Q�B	R�B	T�B	XB	XB	YB	\)B	\)B	\)B	^5B	`BB	aHB	bNB	dZB	e`B	e`B	hsB	iyB	jB	l�B	m�B	m�B	m�B	n�B	n�B	p�B	q�B	r�B	s�B	u�B	x�B	x�B	x�B	y�B	z�B	|�B	}�B	�B	�B	�B	�+B	�+B	�7B	�=B	�JB	�\B	�hB	�uB	��B	��B	��B	�LB	��B	�B	��B
JB
�B
%�B
.B
6FB
=qB
C�B
H�B
M�B
VB
[#B
bNB
e`B
hsB
m�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BʹB̰BʹB̭BλB��B��B��B��B��B�B�>B�`B�fB�hB�hB�fB�fB�hB�hB�hB�hB�hB�hB�fB�hB�nB�xB��B��BB6BRB �B&�B-�B86B>\BCyBD�BE�BD�BD�BCzBAnB=SB?`B?aB?_BCxBCyB?\B>[B84B(�B!�B�BVB=B&B��BTBiBPBHB>B3B�BB
B��B�3B̬B�:B�B��B��B�TB�NB��Bd8BP�BCtB@_B:=B5B0 B'�B!�BuB#�B)�B�B!�BkBRB4B2B	BB��B��B�CB��B�)Bx�BiWB`!B]BS�BCuB,�B
��B
�B
�6B
��B
�aB
�VB
�\B
�[B
�GB
�B
s�B
\
B
U�B
CxB
]B
 �B	��B	�^B	��B	��B	��B	��B	c9B	Q�B	I�B	BzB	9@B	2B	/B	/B	0	B	2B	3B	4#B	6.B	>aB	D�B	>_B	2B	B		#B��B�B�BB��BɨBǙBƓBŎBĈBÂB�|B�|B�zB�|B�sBĉB�zB�|B�sB�hB�TB�PB�CB�>B�7B�0B�,B�)B�%B� B�B�B� B��B��B��B��B�zB�>B�#B� B�B}�B{�By�By�Bx�Bw�Bw�Bv�Bs�Bq�Bp�Bn�BlvBifBfTBcBB_*B\BYBT�BO�BK�BH�BE�BC�BB|BAvB@qB?lB>fB=cB=`B=FB<WB:KB8CB64B5B4'B3#B2B/�B.B*�B$�B �B�B{B{B|B{B�BoBfB{BnBFB^B\BFB<BiBDBNBZBZBnBnBRBZByB`B�BlBgB�BdB�BBB�B{BMBLB{BkB\B|BxByB]BmB<B(B9B<B3B4B<BcBnBxBwB�B �B!�B!�B!�B$�B%�B%�B%�B%�B%�B%�B(�B*�B*�B/B1B6.B;MB;MB?gB?fBBwBCBD�BE�BE�BE�BE�BE�BI�BK�BK�BO�BR�BR�BR�BQ�BS�BZB_"B`)Ba.Bb4Bd@BhZBmwBr�Bu�Bw�Bw�By�B{�B�B��B�B�"B�+B�5B�GB�_B��B��B��B��B��B��B��B��B�
B�1B�3B�RB�sB�oB�xB�BǎBʤBιB�B�!B�@B�xB��B��B��B��B��B��B��B	 �B	�B	�B	�B	
B	B		B	
B	#B	!B	*B	FB	VB	�B	�B	�B	�B	�B	$�B	&�B	)�B	+�B	+�B	,�B	/�B	2B	3B	4B	6#B	6#B	6!B	6#B	80B	:=B	<EB	=MB	>RB	BkB	DxB	DyB	F�B	F�B	F�B	G�B	H�B	K�B	N�B	O�B	P�B	P�B	Q�B	R�B	T�B	W�B	W�B	X�B	\B	\B	\B	^B	`B	a%B	b)B	d7B	e;B	e9B	hPB	iTB	j[B	lfB	mnB	mnB	mmB	nuB	ntB	p�B	q�B	r�B	s�B	u�B	x�B	x�B	x�B	y�B	z�B	|�B	}�B	��B	��B	��B	�B	�B	�B	�B	�$B	�6B	�CB	�OB	�ZB	��B	��B	�$B	��B	�sB	��B
!B
sB
%�B
-�B
6B
=FB
ClB
H�B
M�B
U�B
Z�B
b"B
e5B
hIB
mdB
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.36 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708302016053117083020160531170830  AO  ARCAADJP                                                                    20160409191611    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160409191611  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160409191611  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170830  IP                  G�O�G�O�G�O�                