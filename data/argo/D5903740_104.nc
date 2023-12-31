CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-01-25T03:16:13Z AOML 3.0 creation; 2016-06-01T00:08:22Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150125031613  20160531170823  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               hA   AO  4055_7112_104                   2C  D   APEX                            5374                            041511                          846 @�5>�k�1   @�5?es��@:%�����dV$�/�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    hA   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%y�D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy��D�fD�9�D���D�� D��3D�I�D��fD���D��D�C3D��fD��fD�	�D�<�DچfD�ɚD��fD�&fD�y�D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@˅AA%AEAeA��HA��HA��HA��HA��HA��HA��HA��HBp�B	p�Bp�Bp�B!p�B)p�B1p�B9p�BAp�BIp�BQp�BYp�Bap�Bip�Bqp�Byp�B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBĸRBȸRB̸RBиRBԸRBظRBܸRB�RB�RB�RB�RB�RB��RB��RB��RC \)C\)C\)C\)C\)C
\)C\)C\)C\)C\)C\)C\)C\)C\)C\)C\)C \)C"\)C$\)C&\)C(\)C*\)C,\)C.\)C0\)C2\)C4B�C6\)C8\)C:\)C<\)C>\)C@\)CB\)CD\)CF\)CH\)CJ\)CL\)CN\)CP\)CR\)CT\)CV\)CX\)CZ\)C\\)C^\)C`\)Cb\)Cd\)Cf\)Ch\)Cj\)Cl\)Cn\)Cp\)Cr\)Ct\)Cv\)Cx\)Cz\)C|\)C~\)C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.D 
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
D%��D&
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
Dt�=Dy��D��D�ED��D�˅D���D�UD���D��RD�%D�N�D���D���D�D�HRDڑ�D��D��D�1�D�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�VA�S�A�S�A�S�A�S�A�S�A�S�A�S�A�O�A�Q�A�VA�I�A�A�A�A�A�%A�XA��`A��A��\A�%A���A�?}A��A�p�A�M�A�5?A��A�%A��A��yA��`A���A�ƨA���A��-A���A��A��-A��RA���A��!A��!A��!A��A��A���A���A��\A�x�A�dZA�7LA��;A���A��\A�XA�A�ȴA��wA��A���A���A��uA��A�t�A�ffA�ZA�Q�A�I�A�?}A�&�A�A��yA���A�S�A�oA��!A�S�A���A��
A��A�A�A�$�A��A��A�I�A�VA��mA��jA��A�-A�JA��wA��!A��\A�9XA���A�^5A�JA���A�XA��mA�t�A��A���A�bNA��HA�p�A�A�v�A�;dA���A�JA���A�n�A���A��yA��HA���A��jA�^5A��wA�{A��TA���A��A�(�A�jA���A�K�A�A��A���A��\A�AoA|��A{�
Ay�TAw7LAuO�Ar��Ap  Am�FAm�AiK�Ag;dAe�hAchsAb�`Ab��AaAa�A`��A`1'A_dZA\$�AZZAYƨAYO�AX�AXZAW�PAV��AVQ�AUhsAS�AR�9AR{APĜAN��AL�DAJ�AHĜAF��AD�+AC&�AB  AA"�A=;dA;�FA;�7A;�A9;dA6�+A5�TA5A5|�A5G�A5�A4�A3|�A2-A0��A/oA-�mA,��A*�yA)��A)p�A);dA(��A(M�A'�7A&�RA&I�A$�`A$9XA#�A#�A"�RA"�+A!��A!%A �A ��A ��AƨA��AJA�A��A5?A��A��AE�AAx�A%A��AZA��A33A��AE�A�^A�AO�A�A��A��A-A��A�`A�AS�A
ZA	�wA	"�A�HA��A5?A�TA&�A$�A�DA��Ax�AAAG�A �DA E�@��@��F@��H@�=q@��T@�7L@���@��w@���@�M�@���@�r�@�ƨ@�@��@�1'@���@�|�@���@���@�O�@� �@�ƨ@�33@�p�@�o@��@��@�~�@�G�@���@���@��m@�C�@�&�@�v�@�%@���@��y@Ο�@��#@�A�@ʟ�@���@�?}@���@ȋD@�1'@�l�@�X@���@��@�1'@���@���@�$�@���@�7L@��`@�Q�@�+@�v�@���@��@�@�=q@�@��@�  @��@�V@�5?@�{@��@���@�A�@��@�\)@�o@�~�@��@�Ĝ@�(�@�dZ@��@�?}@��`@��j@�  @�|�@�^5@�bN@���@��@���@�1@�\)@�"�@�n�@���@�%@���@���@��H@��R@��\@�V@��@���@��@�j@�9X@��@��@�+@�v�@��@��7@��`@�A�@�b@��@��@�l�@�K�@�+@�v�@��#@��^@��@�O�@�G�@�V@��
@�l�@�\)@�K�@�K�@�C�@�@��!@�n�@�^5@�=q@�$�@��@���@��@��#@��7@�?}@���@���@��@�z�@���@���@��@�"�@���@�^5@�-@���@�G�@�%@�Ĝ@��u@�Z@�1'@��@��
@��F@���@�C�@��!@�^5@��-@�%@��/@���@�z�@���@��F@��P@�t�@�t�@��@��@��@��
@�\)@�33@��H@���@�n�@�E�@�$�@�$�@��^@�`B@�X@�7L@��j@��u@�bN@�Q�@�I�@�1'@� �@�b@��@��@;d@~��@}@}p�@}?}@}/@}V@|��@|�j@|1@{�@{�@{C�@z�H@xbN@o�@gl�@^�y@VE�@Ol�@G|�@A%@9��@2^5@-?}@&��@ Q�@��@?}@\)@
��@
=@z�@~�@ �u11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�VA�S�A�S�A�S�A�S�A�S�A�S�A�S�A�O�A�Q�A�VA�I�A�A�A�A�A�%A�XA��`A��A��\A�%A���A�?}A��A�p�A�M�A�5?A��A�%A��A��yA��`A���A�ƨA���A��-A���A��A��-A��RA���A��!A��!A��!A��A��A���A���A��\A�x�A�dZA�7LA��;A���A��\A�XA�A�ȴA��wA��A���A���A��uA��A�t�A�ffA�ZA�Q�A�I�A�?}A�&�A�A��yA���A�S�A�oA��!A�S�A���A��
A��A�A�A�$�A��A��A�I�A�VA��mA��jA��A�-A�JA��wA��!A��\A�9XA���A�^5A�JA���A�XA��mA�t�A��A���A�bNA��HA�p�A�A�v�A�;dA���A�JA���A�n�A���A��yA��HA���A��jA�^5A��wA�{A��TA���A��A�(�A�jA���A�K�A�A��A���A��\A�AoA|��A{�
Ay�TAw7LAuO�Ar��Ap  Am�FAm�AiK�Ag;dAe�hAchsAb�`Ab��AaAa�A`��A`1'A_dZA\$�AZZAYƨAYO�AX�AXZAW�PAV��AVQ�AUhsAS�AR�9AR{APĜAN��AL�DAJ�AHĜAF��AD�+AC&�AB  AA"�A=;dA;�FA;�7A;�A9;dA6�+A5�TA5A5|�A5G�A5�A4�A3|�A2-A0��A/oA-�mA,��A*�yA)��A)p�A);dA(��A(M�A'�7A&�RA&I�A$�`A$9XA#�A#�A"�RA"�+A!��A!%A �A ��A ��AƨA��AJA�A��A5?A��A��AE�AAx�A%A��AZA��A33A��AE�A�^A�AO�A�A��A��A-A��A�`A�AS�A
ZA	�wA	"�A�HA��A5?A�TA&�A$�A�DA��Ax�AAAG�A �DA E�@��@��F@��H@�=q@��T@�7L@���@��w@���@�M�@���@�r�@�ƨ@�@��@�1'@���@�|�@���@���@�O�@� �@�ƨ@�33@�p�@�o@��@��@�~�@�G�@���@���@��m@�C�@�&�@�v�@�%@���@��y@Ο�@��#@�A�@ʟ�@���@�?}@���@ȋD@�1'@�l�@�X@���@��@�1'@���@���@�$�@���@�7L@��`@�Q�@�+@�v�@���@��@�@�=q@�@��@�  @��@�V@�5?@�{@��@���@�A�@��@�\)@�o@�~�@��@�Ĝ@�(�@�dZ@��@�?}@��`@��j@�  @�|�@�^5@�bN@���@��@���@�1@�\)@�"�@�n�@���@�%@���@���@��H@��R@��\@�V@��@���@��@�j@�9X@��@��@�+@�v�@��@��7@��`@�A�@�b@��@��@�l�@�K�@�+@�v�@��#@��^@��@�O�@�G�@�V@��
@�l�@�\)@�K�@�K�@�C�@�@��!@�n�@�^5@�=q@�$�@��@���@��@��#@��7@�?}@���@���@��@�z�@���@���@��@�"�@���@�^5@�-@���@�G�@�%@�Ĝ@��u@�Z@�1'@��@��
@��F@���@�C�@��!@�^5@��-@�%@��/@���@�z�@���@��F@��P@�t�@�t�@��@��@��@��
@�\)@�33@��H@���@�n�@�E�@�$�@�$�@��^@�`B@�X@�7L@��j@��u@�bN@�Q�@�I�@�1'@� �@�b@��@��@;d@~��@}@}p�@}?}@}/@}V@|��@|�j@|1@{�@{�@{C�@z�H@xbN@o�@gl�@^�y@VE�@Ol�@G|�@A%@9��@2^5@-?}@&��@ Q�@��@?}@\)@
��@
=@z�@~�@ �u11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�sB�ZB�fB�B�B�B��BBBB%B%BBBBBBBBBB  B  BBB��BBBBBBB  B  B��B��B��B��B�B��B��B�B�B�B�B�B�B�B�yB�yB�sB�sB�mB�mB�fB�`B�TB�HB�)B�B��BǮB�B��B�1B� Bz�Bw�Bv�Bp�B`BBZBW
BR�BM�BI�BF�B@�BB�B@�B7LB'�B �B�B\BB��B�B�5B�B��B�}B�-B��Bu�B%�B��B��Bt�BXBP�BM�BK�BH�BF�B;dBuB
��B
��B
�B
�B
B
��B
��B
��B
�JB
�B
z�B
u�B
l�B
dZB
R�B
J�B
9XB
&�B
�B
B	�B	�#B	��B	�9B	��B	��B	�DB	�1B	�B	� B	{�B	x�B	t�B	l�B	[#B	R�B	O�B	L�B	H�B	E�B	C�B	?}B	>wB	:^B	1'B	+B	%�B	�B	�B	PB	%B	  B��B�B�B�B�`B�/B�B�B��B��BɺBǮBǮBƨBŢBÖB��B�jB�LB�-B�B��B��B��B��B��B��B��B��B��B��B�oB�\B�PB�DB�=B�1B�+B�B�B�B�B� B{�Bx�Bu�Br�Bm�BiyBgmBe`BcTBaHB`BB_;B^5B\)BZBXBW
BT�BR�BO�BJ�BG�BE�BD�BB�B@�B=qB;dB9XB7LB5?B49B49B33B2-B0!B-B)�B&�B%�B$�B"�B!�B�B�B�B�B�B�B�B�B�B�B�B{BuBuBoBbBPBPBPBPBJBJBDB
=B
=B
=B1B+B+B%B%BBBBBBBBBBB%BBB%B1B	7B	7B
=B
=B	7B	7B
=BPBbBhBhB{B{B{B�B�B�B�B�B�B�B�B �B �B"�B$�B(�B)�B(�B(�B+B.B/B1'B1'B2-B33B49B9XB:^B<jBA�BC�BD�BD�BF�BF�BI�BQ�BS�BW
BbNBffBhsBiyBl�Bn�Bq�Bx�B|�B}�B}�B~�B�B�B�7B�7B�DB�DB�DB�DB�\B�{B��B��B��B��B��B��B��B��B��B��B�B�!B�'B�-B�9B�9B�FB��BĜBĜBŢBŢBŢBǮBɺB��B��B��B��B��B��B��B��B��B��B�B�
B�B�B�5B�BB�HB�`B�yB�B�B�B��B��B��B��B	B	B	+B	1B	
=B	DB	hB	�B	!�B	'�B	+B	,B	.B	/B	6FB	9XB	;dB	=qB	>wB	@�B	B�B	F�B	I�B	N�B	P�B	T�B	W
B	YB	[#B	\)B	[#B	_;B	cTB	cTB	e`B	l�B	n�B	p�B	q�B	r�B	r�B	s�B	s�B	u�B	v�B	w�B	z�B	~�B	�B	�B	�B	�B	�B	�B	�7B	�PB	�PB	�VB	�hB	��B	�}B	��B	�NB	��B
	7B
�B
!�B
-B
8RB
?}B
G�B
P�B
YB
_;B
e`B
k�B
p�B
t�B
w�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�zB�zB�[B�BB�JB�pB��B�B��BBBBBBB�B�B�B�B�B�B�B �B��B��B �B �B��B �B �B�B�B�B �B��B��B��B��B��B��B�B��B��B�B�~B�zB�jB�jB�iB�eB�`B�_B�ZB�[B�TB�TB�LB�KB�;B�4B�B��B��BǖB��B��B�B�Bz�Bw�Bv�Bp�B`&BY�BV�BR�BM�BI�BF�B@dBBpB@aB7/B'�B �BpB;B�B��B�fB�B��BͲB�YB�
B��Bu�B%�BηB��Bt�BW�BP�BM�BK�BH�BF�B;ABRB
��B
��B
�B
�dB
�kB
��B
��B
�`B
�*B
��B
z�B
u�B
lkB
d;B
R�B
J�B
9:B
&�B
pB
�B	�oB	�B	��B	�B	��B	�qB	�*B	�B	�B	�B	{�B	x�B	t�B	lqB	[	B	R�B	O�B	L�B	H�B	E�B	C�B	?gB	>^B	:HB	1B	*�B	%�B	�B	pB	:B	B��B��B�B�{B�hB�JB�B�B��B��B��BɦBǘBǗBƔBŎBÂB�oB�TB�8B�B��B��B��B��B��B��B��B��B��B�B�kB�\B�GB�=B�0B�(B�B�B�B� B��B��B�B{�Bx�Bu�Br�BmBifBg[BeMBcABa5B`1B_*B^"B\BZBW�BV�BT�BR�BO�BJ�BG�BE�BD�BB|B@pB=^B;UB9FB7;B5,B4&B4'B3"B2B0B,�B)�B&�B%�B$�B"�B!�B�B�B�B�BmB�BeBzBvB[BUBOBeBcB]B7B$B#B#B@BB BB
B
B
BBB�B�BB�B�B�B�B�B�B�B�BB�BB�B�B�B B	
B	$B
B
B	 B	$B
B!B4B;B8BLBMBeBnBmBmBxB|B�B�B�B �B �B"�B$�B(�B)�B(�B(�B*�B-�B/B1B1B2B3B4#B9DB:EB<UBAqBCBD�BD�BF�BF�BI�BQ�BS�BV�Bb5BfLBh[Bi_BlqBn|Bq�Bx�B|�B}�B}�B~�B��B��B�B�B�*B�)B�(B�)B�@B�bB�pB�{B��B��B��B��B��B��B��B��B��B�B�
B�B�B�B�)B�dB�|B�}BŃBŃBŃBǎBɞB˩B˧B̮B͵B͵BκBλB��B��B��B��B��B��B��B�B�!B�'B�?B�\B�kB�oB�B��B��B��B��B	�B	�B		B	B	
B	!B	HB	|B	!�B	'�B	*�B	+�B	-�B	.�B	6#B	95B	;CB	=MB	>TB	@aB	BkB	F�B	I�B	N�B	P�B	T�B	V�B	X�B	[ B	\B	Z�B	_B	c0B	c.B	e<B	lhB	npB	p�B	q�B	r�B	r�B	s�B	s�B	u�B	v�B	w�B	z�B	~�B	��B	��B	��B	��B	��B	��B	�B	�+B	�)B	�0B	�@B	��B	�VB	˞B	�%B	��B
	B
aB
!�B
,�B
8(B
?QB
G�B
P�B
X�B
_B
e5B
kWB
p{B
t�B
w�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.36 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708232016053117082320160531170823  AO  ARCAADJP                                                                    20150125031613    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150125031613  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20150125031613  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170823  IP                  G�O�G�O�G�O�                