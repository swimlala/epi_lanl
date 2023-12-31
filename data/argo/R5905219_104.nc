CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2021-02-16T09:44:19Z creation;2021-02-16T09:44:20Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `x   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ͐   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �$   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �T   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20210216094419  20210216095235  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               hA   JA                                  2B  A   APEX                            7906                            051216                          846 @�^ˏ\(�1   @�^��H,@4\(�\�d��t�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�33A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C�fC  C  C  C  C  C  C   C!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	fD	� D
  D
� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D�fDfD� D  D� D  D� D  Dy�D  D� D  D� D   D � D!fD!� D"  D"�fD#  D#� D$  D$� D%  D%� D&fD&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6y�D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN�fDO  DO� DP  DP� DQ  DQ� DR  DR�fDSfDS� DT  DT�fDUfDU�fDV  DV� DW  DW�fDX  DX� DX��DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Duy�Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�C3D3D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D���D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ Dԃ3D��3D�  D�@ DՀ D�� D�  D�@ Dփ3D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݃3D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�<�D� D�� D�  D�@ D�|�D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�C3D� D��D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@˅AA%AEAeA��HA��HA��HA��HA�{A�{A��HA��HBp�B	p�Bp�Bp�B!p�B)p�B1p�B9p�BAp�BIp�BQp�BYp�Bap�Bip�Bqp�Byp�B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��B��RB��RB��B��B��RB��RBĸRBȸRB̸RBиRBԸRBظRBܸRB�RB��B�RB�RB�RB��RB��RB��RC \)C\)C\)Cu�C\)C
\)C\)C\)C\)CB�C\)C\)C\)C\)C\)C\)C \)C"B�C$\)C&\)C(\)C*\)C,\)C.\)C0\)C2\)C4\)C6\)C8\)C:\)C<\)C>\)C@\)CB\)CD\)CF\)CH\)CJ\)CL\)CN\)CP\)CRu�CTu�CV\)CX\)CZ\)C\\)C^\)C`\)Cb\)Cd\)Cf\)Ch\)Cj\)Cl\)Cn\)Cp\)Cr\)Ct\)Cv\)Cx\)Cz\)C|\)C~\)C�.C�:�C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�:�C�.C�.C�.C�.C�.C�!GC�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�:�C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�:�C�.C�.C�.C�.C�.C�.C�.C�:�C�.C�.C�.C�!GC�.C�.C�.C�.C�.C�.D 
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
D	pD	�
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
D�D�
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
D�D�
D
D�
D
D�
D
D�pDpD�
D
D�
D
D�
D
D��D
D�
D
D�
D 
D �
D!pD!�
D"
D"�pD#
D#�
D$
D$�
D%
D%�
D&pD&�
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
D6��D7
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
DN�pDO
DO�
DP
DP�
DQ
DQ�
DR
DR�pDSpDS�
DT
DT�pDUpDU�pDV
DV�
DW
DW�pDX
DX�
DY�DY�
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
Dt�
Du
Du��Dv
Dv�
Dw
Dw�
Dx
Dx�
Dy
Dy�
Dz
Dz�
D{
D{�
D|
D|�
D}
D}�
D~
D~�
D
D�
D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�N�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D��RD�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D��RD��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D��RD�˅D��D�K�D���D�˅D��D�K�D���D�θD��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�θD��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�N�D���D�˅D��D�K�D���D�˅D��D�K�D���D��RD�RD�K�D���D�˅D��D�K�D���D�˅D��D�K�D��RD��RD��D�N�D�D�˅D��D�K�DË�D�˅D��D�K�Dċ�D�˅D�RD�K�Dŋ�D�˅D��D�K�DƋ�D�˅D��D�K�Dǋ�D�˅D��D�K�Dȋ�D�˅D��D�K�Dɋ�D�˅D��D�K�Dʋ�D�˅D��D�K�Dˋ�D�˅D��D�K�D̋�D�˅D��D�K�D͋�D�˅D��D�K�D΋�D�˅D��D�K�Dϋ�D�˅D��D�K�DЋ�D�˅D��D�K�Dы�D�˅D��D�K�Dҋ�D�˅D��D�K�DӋ�D�˅D��D�K�DԎ�D�θD��D�K�DՋ�D�˅D��D�K�D֎�D�˅D��D�K�D׋�D�˅D��D�K�D؋�D�˅D��D�K�Dً�D�˅D��D�K�Dڋ�D�˅D��D�K�Dۋ�D�˅D��D�K�D܋�D�˅D��D�K�Dݎ�D�˅D��D�K�Dދ�D�˅D��D�K�Dߋ�D�˅D��D�K�D���D�˅D��D�K�DዅD�˅D��D�K�D⋅D�˅D��D�K�D㋅D�˅D��D�HRD䋅D�˅D��D�N�D勅D�˅D��D�K�D担D�˅D��D�K�D狅D�˅D�RD�HRD苅D�˅D��D�K�D�RD��RD��D�K�DꋅD�˅D��D�K�D닅D�˅D��D�K�D싅D�˅D��D�K�D틅D�˅D��D�K�DD�˅D��D�K�DD�˅D��D�K�D���D�˅D��D�N�D�D��RD�RD�K�D�D�˅D��D�K�D�D�˅D��D�K�D�RD�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�l�A�7LA��A�VA�  A���A���A��A��A��yA��yA��mA��HA��#A��#A��#A��/A��HA��HA��TA��mA��mA��yA��A��A��A��A���A�%A�33A�/A�/A�?}A�p�A̝�Ạ�A̲-A�A̾wA̰!A�bNA�K�A���A��A�A�1A��A�/A�S�ÁȀ\A̧�A���A�(�A���AˬA�E�A�C�AŶFAá�A��/A�/A�-A��`A���A�JA�l�A�1'A��wA���A�x�A�E�A�-A�jA�l�A�ƨA�ȴA�ƨA���A�A���A�;dA���A��A��A�33A���A��!A�9XA��A��DA�Q�A���A���A�(�A�jA�1A��A�~�A��DA��FA�A�A��A���A�n�A�5?A��PA��A���A�  A�1A�9XA��DA�-A�1'A�+A|z�Ay\)Aw��At��ArZAp�DAn�Am
=AkXAjM�Ag+Ad�Ad-Aa33A_�A]p�A[�AXQ�AVĜAU�TAR�AQoAO/AK/AIp�AH�AF�HADȴAC��AB �AA7LAA+A?hsA=%A:ffA8�A8��A7�TA7�A6�RA6-A5�-A5+A4�DA45?A2��A1G�A0�A0�A/��A-��A,Q�A,�A+ƨA+`BA*�jA)x�A(��A(  A&�A%��A$�A#p�A!�A ��At�A�jAbNA�
A7LA=qA��AC�AjA��AA�A�yAbNA��AZA��AVAA�Ap�A
��A
Q�A	�FA	"�Az�A�FAG�A��A�A��A/A�\AJA\)A
=A�A�-A�A @�C�@�+@��H@��@�o@�~�@�J@���@�Ĝ@��m@��R@��T@�bN@�@�-@�Z@�9X@�b@��m@�t�@�~�@�?}@���@띲@�@�-@�ƨ@�{@�%@�j@�A�@��
@�+@�O�@���@��`@���@�ƨ@�~�@ݑh@�hs@�/@�r�@�l�@ى7@�S�@պ^@���@�t�@�ȴ@Ұ!@ҟ�@�^5@��@�x�@�%@��@���@�X@�(�@���@ɲ-@���@ȼj@�A�@���@�\)@�M�@�O�@���@�  @�;d@¸R@���@���@�dZ@�o@�o@�
=@���@���@�{@���@�@�/@���@��D@���@�o@��R@�n�@�^5@�5?@���@���@��@�`B@��@�Z@�Q�@�Q�@�I�@�1'@���@�+@��R@�M�@���@�`B@�V@�%@��j@�r�@�  @��;@��w@�K�@���@�~�@�O�@���@��/@��`@��@��@�Q�@�1'@��@�  @���@��;@��
@���@�dZ@�;d@�"�@��\@�-@��^@��@�&�@�b@�t�@�S�@�;d@��y@�M�@�@�@���@�hs@�/@���@��u@�j@�Z@��@�K�@���@���@�ff@�-@�J@���@���@�p�@�&�@�V@��`@���@��/@��/@���@�9X@��w@�C�@�@���@���@���@�v�@�5?@�X@���@��`@���@�Ĝ@�r�@�  @��
@�ƨ@��@�
=@���@��R@���@�E�@�@���@�7L@�%@���@��@��@��@�ƨ@�C�@�+@��@��y@��!@�~�@�~�@�n�@�^5@��@�@��h@�/@���@��u@�1@��F@���@�\)@�
=@��H@��!@���@�~�@�-@��@���@��@�O�@�V@��`@��`@��/@���@��9@��u@�Q�@��;@��F@��@�;d@��@�ȴ@�ȴ@��R@��\@�V@���@��^@��h@�O�@��@��9@��u@�Q�@��@���@�dZ@�C�@���@��R@�~�@�E�@�J@��@��-@�x�@�?}@��@��`@��u@�I�@��@�1@��F@�l�@�"�@���@���@���@��\@��\@�v�@�5?@�J@���@��#@�x�@�O�@�?}@�7L@��@��j@�r�@�r�@�Z@�9X@�(�@� �@���@��F@�dZ@��@��H@���@���@��\@�V@��@�@��-@�&�@��`@��@�j@� �@�l�@�33@��@�@��H@��+@�E�@��@��T@��-@�p�@��@��9@���@�A�@�  @~�y@~v�@~ff@~@}�h@}O�@}?}@|�@|(�@|1@{�m@{��@{��@{��@{S�@{o@z��@z�!@z�\@z=q@z-@zJ@yx�@x��@xr�@xQ�@xb@w�;@w|�@w\)@w
=@v5?@v@u��@u@u�h@up�@u?}@u�@t�/@t�@t9X@sƨ@r�@r^5@r�@q�@q�^@q��@q7L@p�`@p��@pbN@p1'@o�w@oK�@o�@o
=@nȴ@nv�@nV@m��@mV@l�j@lz�@l(�@k�m@k�F@k��@kt�@kdZ@kS�@k33@k"�@j�@j��@jn�@jJ@i�^@i�7@i&�@h1'@g�@g+@f�@fv�@fV@e�@e�-@eO�@d�j@dZ@c�
@cƨ@c33@b�!@b^5@b-@b�@a��@a�^@ax�@aG�@a�@`��@`Ĝ@`��@`�@_�@_��@_�@^�R@^��@^v�@^5?@]��@\��@\��@\I�@[ƨ@[dZ@["�@Z�@Z�!@ZM�@Y�#@Yx�@Y%@X�@W;d@W
=@V�y@Vff@U�h@UV@T�@T�@TI�@S��@S��@S@R�H@R�!@R~�@Q��@QG�@QG�@Q�@P�9@P �@O��@O|�@Nȴ@NE�@M�@MV@L��@L�D@Lj@L9X@K��@K�F@J�@J��@J�\@JM�@JJ@I�#@Ihs@I&�@I�@H��@H�`@H��@H�@H �@G�@G��@G�@G�P@Gl�@G;d@F�R@F5?@E�T@E��@E�@Dz�@DZ@D�@C��@Cƨ@C��@C�@CS�@B�@B�!@B~�@B^5@BM�@B-@A�@A�^@A7L@@�9@@1'@?�;@?�P@?K�@?
=@>��@>$�@=�@=p�@=?}@=�@=V@<��@<�D@<j@<I�@<(�@;��@;�F@;S�@;o@:�H@:��@:�\@:n�@:^5@:M�@:�@9��@9�^@9hs@97L@8��@8�@8b@7��@7�P@7K�@7;d@7+@6ȴ@6E�@5�-@5O�@5�@5V@4�/@49X@3�m@3�
@3�@3"�@2��@2�\@2M�@1�#@1�@1�#@1��@1�^@1��@1��@1x�@1X@17L@1%@0��@0��@0��@0�u@0�@0A�@01'@0 �@0  @/�@/�P@/|�@/\)@/;d@/
=@.�@.��@.v�@.V@.$�@-�T@-@-�-@-O�@,�@,�j@,j@,(�@+�F@+t�@+33@+@*�H@*��@*�!@*-@*�@)�@)hs@)G�@)G�@)�@(��@(�@(A�@(b@'�;@'|�@'K�@'
=@&�R@&�+@&ff@&{@%�@%�h@%p�@%/@$��@$�j@$�D@$Z@$�@#�m@#��@#S�@#o@"�@"��@"~�@"^5@"=q@"J@!�7@!7L@ �`@ r�@ bN@ Q�@  �@�;@�@�P@l�@\)@+@
=@�@ȴ@�R@�R@��@v�@E�@@�@��@��@?}@V@��@�/@�@j@I�@�@�F@��@t�@33@"�@o@�@��@�!@~�@M�@-@��@�#@hs@��@�9@r�@Q�@A�@�w@�P@�@l�@�y@�@�R@v�@V@E�@5?@@��@(�@�F@dZ@C�@o@��@~�@n�@M�@��@�^@��@�7@x�@hs@G�@7L@%@�`@bN@A�@1'@ �@1'@1'@ �@  @��@
=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�l�A�7LA��A�VA�  A���A���A��A��A��yA��yA��mA��HA��#A��#A��#A��/A��HA��HA��TA��mA��mA��yA��A��A��A��A���A�%A�33A�/A�/A�?}A�p�A̝�Ạ�A̲-A�A̾wA̰!A�bNA�K�A���A��A�A�1A��A�/A�S�ÁȀ\A̧�A���A�(�A���AˬA�E�A�C�AŶFAá�A��/A�/A�-A��`A���A�JA�l�A�1'A��wA���A�x�A�E�A�-A�jA�l�A�ƨA�ȴA�ƨA���A�A���A�;dA���A��A��A�33A���A��!A�9XA��A��DA�Q�A���A���A�(�A�jA�1A��A�~�A��DA��FA�A�A��A���A�n�A�5?A��PA��A���A�  A�1A�9XA��DA�-A�1'A�+A|z�Ay\)Aw��At��ArZAp�DAn�Am
=AkXAjM�Ag+Ad�Ad-Aa33A_�A]p�A[�AXQ�AVĜAU�TAR�AQoAO/AK/AIp�AH�AF�HADȴAC��AB �AA7LAA+A?hsA=%A:ffA8�A8��A7�TA7�A6�RA6-A5�-A5+A4�DA45?A2��A1G�A0�A0�A/��A-��A,Q�A,�A+ƨA+`BA*�jA)x�A(��A(  A&�A%��A$�A#p�A!�A ��At�A�jAbNA�
A7LA=qA��AC�AjA��AA�A�yAbNA��AZA��AVAA�Ap�A
��A
Q�A	�FA	"�Az�A�FAG�A��A�A��A/A�\AJA\)A
=A�A�-A�A @�C�@�+@��H@��@�o@�~�@�J@���@�Ĝ@��m@��R@��T@�bN@�@�-@�Z@�9X@�b@��m@�t�@�~�@�?}@���@띲@�@�-@�ƨ@�{@�%@�j@�A�@��
@�+@�O�@���@��`@���@�ƨ@�~�@ݑh@�hs@�/@�r�@�l�@ى7@�S�@պ^@���@�t�@�ȴ@Ұ!@ҟ�@�^5@��@�x�@�%@��@���@�X@�(�@���@ɲ-@���@ȼj@�A�@���@�\)@�M�@�O�@���@�  @�;d@¸R@���@���@�dZ@�o@�o@�
=@���@���@�{@���@�@�/@���@��D@���@�o@��R@�n�@�^5@�5?@���@���@��@�`B@��@�Z@�Q�@�Q�@�I�@�1'@���@�+@��R@�M�@���@�`B@�V@�%@��j@�r�@�  @��;@��w@�K�@���@�~�@�O�@���@��/@��`@��@��@�Q�@�1'@��@�  @���@��;@��
@���@�dZ@�;d@�"�@��\@�-@��^@��@�&�@�b@�t�@�S�@�;d@��y@�M�@�@�@���@�hs@�/@���@��u@�j@�Z@��@�K�@���@���@�ff@�-@�J@���@���@�p�@�&�@�V@��`@���@��/@��/@���@�9X@��w@�C�@�@���@���@���@�v�@�5?@�X@���@��`@���@�Ĝ@�r�@�  @��
@�ƨ@��@�
=@���@��R@���@�E�@�@���@�7L@�%@���@��@��@��@�ƨ@�C�@�+@��@��y@��!@�~�@�~�@�n�@�^5@��@�@��h@�/@���@��u@�1@��F@���@�\)@�
=@��H@��!@���@�~�@�-@��@���@��@�O�@�V@��`@��`@��/@���@��9@��u@�Q�@��;@��F@��@�;d@��@�ȴ@�ȴ@��R@��\@�V@���@��^@��h@�O�@��@��9@��u@�Q�@��@���@�dZ@�C�@���@��R@�~�@�E�@�J@��@��-@�x�@�?}@��@��`@��u@�I�@��@�1@��F@�l�@�"�@���@���@���@��\@��\@�v�@�5?@�J@���@��#@�x�@�O�@�?}@�7L@��@��j@�r�@�r�@�Z@�9X@�(�@� �@���@��F@�dZ@��@��H@���@���@��\@�V@��@�@��-@�&�@��`@��@�j@� �@�l�@�33@��@�@��H@��+@�E�@��@��T@��-@�p�@��@��9@���@�A�@�  @~�y@~v�@~ff@~@}�h@}O�@}?}@|�@|(�@|1@{�m@{��@{��@{��@{S�@{o@z��@z�!@z�\@z=q@z-@zJ@yx�@x��@xr�@xQ�@xb@w�;@w|�@w\)@w
=@v5?@v@u��@u@u�h@up�@u?}@u�@t�/@t�@t9X@sƨ@r�@r^5@r�@q�@q�^@q��@q7L@p�`@p��@pbN@p1'@o�w@oK�@o�@o
=@nȴ@nv�@nV@m��@mV@l�j@lz�@l(�@k�m@k�F@k��@kt�@kdZ@kS�@k33@k"�@j�@j��@jn�@jJ@i�^@i�7@i&�@h1'@g�@g+@f�@fv�@fV@e�@e�-@eO�@d�j@dZ@c�
@cƨ@c33@b�!@b^5@b-@b�@a��@a�^@ax�@aG�@a�@`��@`Ĝ@`��@`�@_�@_��@_�@^�R@^��@^v�@^5?@]��@\��@\��@\I�@[ƨ@[dZ@["�@Z�@Z�!@ZM�@Y�#@Yx�@Y%@X�@W;d@W
=@V�y@Vff@U�h@UV@T�@T�@TI�@S��@S��@S@R�H@R�!@R~�@Q��@QG�@QG�@Q�@P�9@P �@O��@O|�@Nȴ@NE�@M�@MV@L��@L�D@Lj@L9X@K��@K�F@J�@J��@J�\@JM�@JJ@I�#@Ihs@I&�@I�@H��@H�`@H��@H�@H �@G�@G��@G�@G�P@Gl�@G;d@F�R@F5?@E�T@E��@E�@Dz�@DZ@D�@C��@Cƨ@C��@C�@CS�@B�@B�!@B~�@B^5@BM�@B-@A�@A�^@A7L@@�9@@1'@?�;@?�P@?K�@?
=@>��@>$�@=�@=p�@=?}@=�@=V@<��@<�D@<j@<I�@<(�@;��@;�F@;S�@;o@:�H@:��@:�\@:n�@:^5@:M�@:�@9��@9�^@9hs@97L@8��@8�@8b@7��@7�P@7K�@7;d@7+@6ȴ@6E�@5�-@5O�@5�@5V@4�/@49X@3�m@3�
@3�@3"�@2��@2�\@2M�@1�#@1�@1�#@1��@1�^@1��@1��@1x�@1X@17L@1%@0��@0��@0��@0�u@0�@0A�@01'@0 �@0  @/�@/�P@/|�@/\)@/;d@/
=@.�@.��@.v�@.V@.$�@-�T@-@-�-@-O�@,�@,�j@,j@,(�@+�F@+t�@+33@+@*�H@*��@*�!@*-@*�@)�@)hs@)G�@)G�@)�@(��@(�@(A�@(b@'�;@'|�@'K�@'
=@&�R@&�+@&ff@&{@%�@%�h@%p�@%/@$��@$�j@$�D@$Z@$�@#�m@#��@#S�@#o@"�@"��@"~�@"^5@"=q@"J@!�7@!7L@ �`@ r�@ bN@ Q�@  �@�;@�@�P@l�@\)@+@
=@�@ȴ@�R@�R@��@v�@E�@@�@��@��@?}@V@��@�/@�@j@I�@�@�F@��@t�@33@"�@o@�@��@�!@~�@M�@-@��@�#@hs@��@�9@r�@Q�@A�@�w@�P@�@l�@�y@�@�R@v�@V@E�@5?@@��@(�@�F@dZ@C�@o@��@~�@n�@M�@��@�^@��@�7@x�@hs@G�@7L@%@�`@bN@A�@1'@ �@1'@1'@ �@  @��@
=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�5B	�;B	�BB	�NB	�`B	�B	�B	�B	��B
B
�B
�B
�B
$�B
,B
0!B
 �B
�B
�B
�B
&�B
)�B
33B
<jB
I�B
`BB
hsB
t�B
�%B
�)BbNB�B�B��B�3B�XB��B�B�BB�5B�fB�sB�TBhB�B�B�B%�B?}B>wB=qB49B'�B�B  B��B��B�fB�`B�B��BBB��B��B�sB�ZB�5B�B�B��B�dB�B��B�DBjBYBQ�BF�B33B#�BhB
��B
�HB
��B
��B
�}B
�-B
��B
�=B
z�B
k�B
S�B
B�B
6FB
%�B
�B
1B	��B	�B	�mB	�#B	��B	�RB	�-B	��B	��B	�=B	|�B	m�B	e`B	^5B	Q�B	H�B	<jB	.B	#�B	�B	�B	{B	oB	PB	1B	%B	  B��B�B�yB�mB�`B�NB�HB�;B�/B�#B�B�
B��B��B��B��BȴBÖB�wB�qB�jB�^B�RB�?B�-B�!B�B��B��B��B��B��B��B��B��B��B��B�{B�uB�hB�bB�VB�7B�1B�+B�%B�B�B�B�%B�%B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B� B~�B� B� B�B�B�B�B�%B�+B�7B�1B�1B�=B�7B�=B�=B�DB�PB�VB�hB�hB�hB�hB�hB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�LB�dB�dB�dB�qB�qB��BƨBɺB��B��B��B��B��B��B��B��B��B��B��B�B�)B�HB�`B�B�B�B�B��B��B��B��B	B	B	B	+B	VB	bB	hB	hB	oB	{B	�B	�B	�B	�B	�B	"�B	)�B	0!B	49B	6FB	;dB	C�B	H�B	N�B	T�B	VB	XB	]/B	_;B	`BB	aHB	bNB	dZB	e`B	iyB	jB	n�B	r�B	v�B	w�B	w�B	x�B	|�B	~�B	�B	�B	�%B	�7B	�DB	�DB	�PB	�bB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�-B	�FB	�XB	�^B	�^B	�dB	�qB	�wB	�}B	��B	ÖB	ĜB	ŢB	ƨB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�/B	�/B	�5B	�BB	�BB	�`B	�fB	�fB	�mB	�mB	�sB	�sB	�yB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
	7B

=B
DB
DB
DB
JB
JB
PB
PB
PB
VB
\B
\B
\B
VB
\B
\B
\B
\B
bB
bB
hB
hB
hB
oB
uB
uB
{B
{B
{B
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
%�B
%�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
(�B
(�B
(�B
(�B
(�B
)�B
+B
+B
+B
+B
,B
,B
,B
,B
-B
.B
/B
/B
0!B
1'B
1'B
1'B
1'B
2-B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
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
XB
XB
YB
YB
YB
YB
YB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
o�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
z�B
{�B
|�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
{�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�g1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�5B	�;B	�BB	�NB	�`B	�B	�B	�B	��B
B
�B
�B
�B
$�B
,B
0!B
 �B
�B
�B
�B
&�B
)�B
33B
<jB
I�B
`BB
hsB
t�B
�%B
�)BbNB�B�B��B�3B�XB��B�B�BB�5B�fB�sB�TBhB�B�B�B%�B?}B>wB=qB49B'�B�B  B��B��B�fB�`B�B��BBB��B��B�sB�ZB�5B�B�B��B�dB�B��B�DBjBYBQ�BF�B33B#�BhB
��B
�HB
��B
��B
�}B
�-B
��B
�=B
z�B
k�B
S�B
B�B
6FB
%�B
�B
1B	��B	�B	�mB	�#B	��B	�RB	�-B	��B	��B	�=B	|�B	m�B	e`B	^5B	Q�B	H�B	<jB	.B	#�B	�B	�B	{B	oB	PB	1B	%B	  B��B�B�yB�mB�`B�NB�HB�;B�/B�#B�B�
B��B��B��B��BȴBÖB�wB�qB�jB�^B�RB�?B�-B�!B�B��B��B��B��B��B��B��B��B��B��B�{B�uB�hB�bB�VB�7B�1B�+B�%B�B�B�B�%B�%B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B� B~�B� B� B�B�B�B�B�%B�+B�7B�1B�1B�=B�7B�=B�=B�DB�PB�VB�hB�hB�hB�hB�hB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�LB�dB�dB�dB�qB�qB��BƨBɺB��B��B��B��B��B��B��B��B��B��B��B�B�)B�HB�`B�B�B�B�B��B��B��B��B	B	B	B	+B	VB	bB	hB	hB	oB	{B	�B	�B	�B	�B	�B	"�B	)�B	0!B	49B	6FB	;dB	C�B	H�B	N�B	T�B	VB	XB	]/B	_;B	`BB	aHB	bNB	dZB	e`B	iyB	jB	n�B	r�B	v�B	w�B	w�B	x�B	|�B	~�B	�B	�B	�%B	�7B	�DB	�DB	�PB	�bB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�-B	�FB	�XB	�^B	�^B	�dB	�qB	�wB	�}B	��B	ÖB	ĜB	ŢB	ƨB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�/B	�/B	�5B	�BB	�BB	�`B	�fB	�fB	�mB	�mB	�sB	�sB	�yB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
	7B

=B
DB
DB
DB
JB
JB
PB
PB
PB
VB
\B
\B
\B
VB
\B
\B
\B
\B
bB
bB
hB
hB
hB
oB
uB
uB
{B
{B
{B
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
%�B
%�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
(�B
(�B
(�B
(�B
(�B
)�B
+B
+B
+B
+B
,B
,B
,B
,B
-B
.B
/B
/B
0!B
1'B
1'B
1'B
1'B
2-B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
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
XB
XB
YB
YB
YB
YB
YB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
o�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
z�B
{�B
|�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
{�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�g1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20210216184412  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20210216094419  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20210216094419  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20210216094419  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20210216094420  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20210216094420  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20210216094420  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20210216094420  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20210216094420  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20210216094420                      G�O�G�O�G�O�                JA  ARUP                                                                        20210216095235                      G�O�G�O�G�O�                