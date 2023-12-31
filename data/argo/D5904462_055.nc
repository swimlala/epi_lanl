CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-05-30T19:17:18Z AOML 3.0 creation; 2016-08-07T21:51:18Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150530191718  20160807145118  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               7A   AO  5287_9017_055                   2C  D   APEX                            6529                            072314                          846 @�T�j1`1   @�T��5@
@/��t��d��E���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    7A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(ffB/��B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B뙚B���B���B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C �C!��C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR�CS�fCV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDy��D��D�@ D�#3D�� D�  D�<�D��3D���D��D�9�D���D���D��D�6fDڌ�D��fD�  D�P D�s3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@˅AA%AEAeA��HA��HA��HA��HA��HA��HA��HA��HBp�B	p�Bp�Bp�B!�
B)�
B1
>B9p�BAp�BIp�BQp�BYp�Bap�Bip�Bqp�By�
B��B��B��B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBĸRBȸRB̸RBиRBԸRBظRBܸRB�RB��B��B�Q�B��B�B��RB��RC \)C\)C\)C\)C\)C
\)C\)C\)C\)C\)C\)C\)C\)C\)C\)Cu�C u�C"(�C$B�C&\)C(\)C*\)C,\)C.\)C0\)C2\)C4\)C6\)C8\)C:\)C<\)C>\)C@\)CB\)CD\)CF\)CH\)CJ\)CL\)CN\)CPu�CRu�CTB�CV\)CX\)CZ\)C\\)C^\)C`\)Cb\)Cd\)Cf\)Ch\)Cj\)Cl\)Cn\)Cp\)Cr\)Ct\)Cv\)Cx\)Cz\)C|\)C~\)C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.D 
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
Dt}pDy��D�RD�K�D�.�D�˅D��D�HRD���D��RD�%D�ED��D��RD�RD�A�DژRD���D��D�[�D�~�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�|�A�z�A�ZA�E�A�VAׁA�l�A�ffA�^5A�XA�O�A�I�A�E�A�7LA�+A��A�{A�VA���Aֲ-A�7LAҮAщ7A�
=A�?}Aϴ9AυA�dZA���A�bNA���A̼jA˙�A��AʑhA���AɮAɍPA�=qA�  A��
AȾwA�M�A�+A�ȴAžwA�7LA���AÛ�A�
=A¬A��jA�ȴA���A�=qA�9XA�I�A���A�VA��;A���A�ƨA�r�A�r�A�ZA�JA���A�(�A��A�=qA�bNA���A�t�A�l�A��^A�bNA��A�=qA��HA�A�A�G�A��
A�p�A��9A���A��!A�l�A�/A�ȴA�n�A�VA�(�A�hsA��A|��At�Ao�Al-Ah�!Ae�hAb�+A[O�AU�PAPz�AMƨAK�#AJ��AJ��AJ�+AJr�AIl�AGdZAFbAE|�AD��AC7LA?�A?A>��A;��A9C�A7�A5�wA3�A0�A,�A)A(��A({A&ĜA%�TA%t�A$(�A#33A"��A"A�A!��A!
=A �+A(�AbNA��A�yA  A�RA�A�A�DA5?AA�jA;dAA�AhsAȴAZA9XA5?A��A��AhsA
=A\)A	�hA	�A��A��A=qA��A�7A"�A�RA�A��AO�A�A�/A�+A�^AhsA��A��AI�A�A�A�FAXA ��A ȴA ��A �+@���@��-@�V@�r�@��H@�n�@���@��D@�O�@�\@�-@�7L@�!@�9@��
@���@�!@��y@���@�Z@�P@�S�@�  @��m@�+@��@��T@��@ߝ�@�hs@�o@�
=@�I�@և+@�@���@�/@ԛ�@�b@��
@�dZ@�5?@�G�@Ϯ@��y@́@̓u@���@��;@��
@��
@��
@˾w@�@�~�@�-@�J@ɡ�@ə�@�p�@ȴ9@��;@�dZ@�o@�"�@��@�^5@�v�@��y@��@Ƈ+@�M�@��/@öF@�|�@�n�@�J@��h@���@��
@��@��!@�X@���@��@�&�@�1'@��@�{@�-@�$�@�ff@�~�@�$�@��-@�X@���@�X@�x�@��7@�?}@��u@�Z@�9X@�ƨ@�t�@��H@���@�E�@���@�?}@���@��F@���@�@�S�@��+@�^5@��@��@�@���@��7@�p�@�V@�bN@�I�@���@�S�@���@�@���@�x�@�V@���@��@��D@�Z@��m@�\)@�;d@��@��y@�5?@��@���@�p�@�7L@�Ĝ@���@�z�@� �@���@���@�l�@�o@���@�E�@�{@��@���@���@��7@�%@�z�@�9X@�ƨ@�|�@�S�@�;d@�
=@��@��\@��T@�`B@�/@�V@��D@�r�@�9X@��@��F@��@�\)@�C�@�33@�
=@��@�ff@�-@���@��^@�x�@��@��D@�r�@�j@�bN@�bN@�Q�@�1'@�|�@��@�@��@�v�@�E�@�J@��@��#@��-@��@�`B@�G�@�&�@�V@��`@�z�@���@�t�@��H@�ff@�^5@�V@�$�@���@�@�&�@��@��D@�z�@�Z@� �@���@���@�|�@�dZ@�S�@�o@�ȴ@���@�ff@�J@��T@��7@�&�@�V@���@��@�1@�l�@�S�@�;d@��R@��!@���@��!@�v�@�E�@�-@���@�@�hs@���@���@���@��@�bN@��m@���@�\)@���@�=q@��@��T@��-@���@���@��h@�p�@�O�@�&�@��@��u@�Q�@�b@�1@���@��m@��;@��F@�l�@�+@�@��H@��R@�~�@���@~E�@t�j@lZ@d�/@]�@T�@K��@AG�@9G�@4j@.�y@)�7@$z�@ r�@"�@V@G�@�@	�^@�R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�|�A�z�A�ZA�E�A�VAׁA�l�A�ffA�^5A�XA�O�A�I�A�E�A�7LA�+A��A�{A�VA���Aֲ-A�7LAҮAщ7A�
=A�?}Aϴ9AυA�dZA���A�bNA���A̼jA˙�A��AʑhA���AɮAɍPA�=qA�  A��
AȾwA�M�A�+A�ȴAžwA�7LA���AÛ�A�
=A¬A��jA�ȴA���A�=qA�9XA�I�A���A�VA��;A���A�ƨA�r�A�r�A�ZA�JA���A�(�A��A�=qA�bNA���A�t�A�l�A��^A�bNA��A�=qA��HA�A�A�G�A��
A�p�A��9A���A��!A�l�A�/A�ȴA�n�A�VA�(�A�hsA��A|��At�Ao�Al-Ah�!Ae�hAb�+A[O�AU�PAPz�AMƨAK�#AJ��AJ��AJ�+AJr�AIl�AGdZAFbAE|�AD��AC7LA?�A?A>��A;��A9C�A7�A5�wA3�A0�A,�A)A(��A({A&ĜA%�TA%t�A$(�A#33A"��A"A�A!��A!
=A �+A(�AbNA��A�yA  A�RA�A�A�DA5?AA�jA;dAA�AhsAȴAZA9XA5?A��A��AhsA
=A\)A	�hA	�A��A��A=qA��A�7A"�A�RA�A��AO�A�A�/A�+A�^AhsA��A��AI�A�A�A�FAXA ��A ȴA ��A �+@���@��-@�V@�r�@��H@�n�@���@��D@�O�@�\@�-@�7L@�!@�9@��
@���@�!@��y@���@�Z@�P@�S�@�  @��m@�+@��@��T@��@ߝ�@�hs@�o@�
=@�I�@և+@�@���@�/@ԛ�@�b@��
@�dZ@�5?@�G�@Ϯ@��y@́@̓u@���@��;@��
@��
@��
@˾w@�@�~�@�-@�J@ɡ�@ə�@�p�@ȴ9@��;@�dZ@�o@�"�@��@�^5@�v�@��y@��@Ƈ+@�M�@��/@öF@�|�@�n�@�J@��h@���@��
@��@��!@�X@���@��@�&�@�1'@��@�{@�-@�$�@�ff@�~�@�$�@��-@�X@���@�X@�x�@��7@�?}@��u@�Z@�9X@�ƨ@�t�@��H@���@�E�@���@�?}@���@��F@���@�@�S�@��+@�^5@��@��@�@���@��7@�p�@�V@�bN@�I�@���@�S�@���@�@���@�x�@�V@���@��@��D@�Z@��m@�\)@�;d@��@��y@�5?@��@���@�p�@�7L@�Ĝ@���@�z�@� �@���@���@�l�@�o@���@�E�@�{@��@���@���@��7@�%@�z�@�9X@�ƨ@�|�@�S�@�;d@�
=@��@��\@��T@�`B@�/@�V@��D@�r�@�9X@��@��F@��@�\)@�C�@�33@�
=@��@�ff@�-@���@��^@�x�@��@��D@�r�@�j@�bN@�bN@�Q�@�1'@�|�@��@�@��@�v�@�E�@�J@��@��#@��-@��@�`B@�G�@�&�@�V@��`@�z�@���@�t�@��H@�ff@�^5@�V@�$�@���@�@�&�@��@��D@�z�@�Z@� �@���@���@�|�@�dZ@�S�@�o@�ȴ@���@�ff@�J@��T@��7@�&�@�V@���@��@�1@�l�@�S�@�;d@��R@��!@���@��!@�v�@�E�@�-@���@�@�hs@���@���@���@��@�bN@��m@���@�\)@���@�=q@��@��T@��-@���@���@��h@�p�@�O�@�&�@��@��u@�Q�@�b@�1@���@��m@��;@��F@�l�@�+@�@��H@��RG�O�@���@~E�@t�j@lZ@d�/@]�@T�@K��@AG�@9G�@4j@.�y@)�7@$z�@ r�@"�@V@G�@�@	�^@�R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�B
�B
�B
uB
VB
VB
VB
VB
VB
VB
\B
bB
bB
bB
\B
\B
\B
VB
DB
B
 �B
&�B
$�B
%�B
A�B
VB
l�B
�jB
�B@�Bm�B�B��B�TB�B�B��B1B�B$�B/B=qBQ�BW
BR�B[#BhsBcTBaHBiyBx�B��B�{B\)B&�BuB��B:^B#�B%�B�B��B�B�`B�B�B�sB�NB�B��BĜBŢBĜB�}B�-B��B�BW
BJB
��B
��B
q�B
_;B
C�B
6FB
2-B
1'B
/B
+B
&�B
�B
uB
	7B	��B	��B	�LB	��B	�7B	u�B	aHB	:^B	�B	+B��B��B��B�B�B�B�B�B�B�B�sB�`B�;B�)B�B��B��BɺBŢB��B�jB��BŢBƨBŢBǮBȴBǮBɺB��B��B��B��BɺBǮB��B��B��B��B��B��B��B��B�B��B��B��BǮBĜBB��BBB��B��B��B�}B�qB�dB�qB�}B��B�}B��B��BÖB��B��B�
B�B�#B�;B�`B�B�B�B�B�B��B��B��B��B��B	B	B	B	B	DB	VB	VB	VB	\B	\B	VB	VB	DB	%B	B	B��B��B	%B	+B		7B	�B	�B	�B	�B	!�B	&�B	)�B	7LB	8RB	9XB	:^B	=qB	<jB	=qB	@�B	;dB	9XB	9XB	:^B	=qB	C�B	F�B	G�B	H�B	F�B	D�B	A�B	C�B	D�B	L�B	R�B	S�B	S�B	T�B	T�B	VB	ZB	\)B	_;B	_;B	`BB	`BB	e`B	n�B	q�B	r�B	u�B	v�B	x�B	y�B	{�B	�B	�%B	�7B	�DB	�PB	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	�oB	�oB	�{B	��B	��B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�9B	�FB	�LB	�LB	�?B	�9B	�3B	�3B	�?B	�FB	�LB	�RB	�LB	�FB	�?B	�3B	�-B	�LB	�qB	��B	��B	B	��B	��B	��B	��B	B	ÖB	ŢB	ĜB	ÖB	B	B	ŢB	ŢB	ŢB	ŢB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�#B	�)B	�/B	�;B	�BB	�HB	�TB	�TB	�TB	�ZB	�fB	�fB	�mB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
%B
%B
+B
+B
1B
1B
1B
	7B
	7B
	7B

=B
DB
JB
PB
\B
oB
uB
uB
{B
�B
{B
{B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
&�B
.B
33B
8RB
>wB
B�B
J�B
Q�B
W
B
[#B
aHB
dZB
hsB
k�B
p�B
u�B
x�B
|�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
�B
�B
xB
kB
aB
CB
CB
CB
CB
@B
?B
GB
OB
QB
QB
GB
HB
GB
CB
1B
B
 �B
&�B
$�B
%�B
AuB
U�B
lvB
�NB
�B@eBmuB��B��B�9B�B�B��BBcB$�B/B=UBQ�BV�BR�B[BhWBc4Ba*Bi\Bx�B�}B�\B\B&�BXB��B:>B#�B%�BcB��B�cB�>B�oB��B�RB�/B��BˤB�|BŁB�|B�\B�B��B��BV�B,B
ʡB
��B
q�B
_B
CvB
6*B
2B
1B
.�B
*�B
&�B
�B
ZB
	B	��B	��B	�1B	��B	�B	u�B	a0B	:FB	�B	B��B��B��B�B�B�B��B�B�uB�iB�[B�KB�&B�B��B��B��BɤBŌB�lB�WB�lBŊBƐBŊBǗBȝBǕBɡBʫBʩB˯BʫBɣBǔBʨBʪBʬB˯BͺB��B��B��B��B��B��BͺBǕBĄB�wB�oB�wB�uB�qB�qB�jB�cB�WB�MB�WB�bB�jB�cB�jB�oB�{BʨB͹B��B��B�	B�!B�DB�dB�oB�B��B�B��B��B��B��B��B	 �B	�B	�B	�B	'B	9B	8B	8B	?B	@B	7B	9B	'B	
B	�B	 �B��B��B	B	B		B	bB	hB	�B	�B	!�B	&�B	)�B	7-B	82B	96B	:>B	=PB	<JB	=PB	@aB	;DB	95B	96B	:?B	=OB	CtB	F�B	G�B	H�B	F�B	D{B	AhB	CsB	D}B	L�B	R�B	S�B	S�B	T�B	T�B	U�B	Y�B	\	B	_B	_B	`!B	`"B	e=B	nsB	q�B	r�B	u�B	v�B	x�B	y�B	{�B	��B	� B	�B	� B	�-B	�1B	�OB	�]B	�iB	�hB	�oB	�pB	�dB	�\B	�JB	�IB	�WB	�kB	�dB	�VB	�VB	�iB	�uB	��B	�B	��B	��B	��B	��B	�B	� B	�'B	�%B	�B	�B	�B	�B	�B	� B	�&B	�.B	�$B	� B	�B	�B	�B	�'B	�KB	�fB	�`B	�gB	�aB	�cB	�cB	�cB	�iB	�nB	�{B	�vB	�pB	�hB	�gB	�{B	�|B	�|B	�{B	ƁB	ƃB	ǇB	ȌB	ɔB	̧B	̥B	̥B	ͭB	оB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�+B	�,B	�-B	�2B	�>B	�=B	�DB	�KB	�KB	�LB	�SB	�RB	�UB	�[B	�dB	�iB	�jB	�qB	�oB	�uB	�vB	�}B	�}B	�}B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
B
B
	B
B
B
	B
	B
	B

B
B
"B
&B
0B
EB
LB
JB
OB
UB
PB
SB
VB
RB
RB
]B
bB
dB
jB
jB
kB
kB
iB
qB
xB
uB
uB
vB
vB
|B
}B
|B
B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
�B
&�B
-�B
3B
8*B
>KB
BeB
J�B
Q�B
V�B
Z�B
aB
d0B
hHB
kZB
pzB
u�B
x�B
|�B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.36 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451182016080714511820160807145118  AO  ARCAADJP                                                                    20150530191718    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150530191718  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150530191718  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145118  IP                  G�O�G�O�G�O�                