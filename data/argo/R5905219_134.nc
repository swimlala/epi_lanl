CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2021-12-10T12:42:33Z creation;2021-12-10T12:42:35Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \|   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ``   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �$   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �(   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �,   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �0   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �p   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20211210124233  20211210125229  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            7906                            051216                          846 @٨g�\(�1   @٨h�H,@3-V�dWn��P1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�33@�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ�CL  CN  CP  CR  CT  CV  CX  CZ�C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C��3D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D��D � D!  D!� D"  D"� D#  D#y�D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DCfDC� DD  DD� DE  DE� DF  DF� DGfDG� DHfDH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DOy�DP  DP� DQ  DQ� DR  DR� DS  DS� DTfDT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De�fDf  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dky�Dl  Dl� Dm  Dm� Dn  Dn�fDo  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�|�D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D��3D�3D�C3DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�<�Dɀ D�� D�  D�<�Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΃3D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ DҼ�D���D�@ DӀ D�� D�  D�@ Dԃ3D��3D�  D�@ DՀ D�� D�  D�C3Dփ3D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D���D�@ D� D�� D�  D�@ D� D��3D�3D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��R@˅AA%AEAeA��HA�{A��HA��HA��HA��HA��HA��HBp�B	p�Bp�B
>B!p�B)p�B1p�B9p�BAp�BIp�BQp�BYp�Bap�Bip�Bqp�Byp�B��RB��RB��B��B��RB��RB��RB��RB��RB��RB��RB��RB��RB��B��B��RB��RBĸRBȸRB̸RBиRBԅBظRBܸRB�RB�RB�RB�RB�RB��RB��RB��RC \)C\)C\)C\)Cu�C
\)C\)C\)C\)C\)C\)C\)C\)C\)C\)C\)C \)C"\)C$\)C&\)C(\)C*\)C,\)C.\)C0\)C2\)C4\)C6\)C8B�C:\)C<\)C>\)C@\)CB\)CD\)CF\)CH\)CJu�CL\)CN\)CP\)CR\)CT\)CV\)CX\)CZu�C\\)C^\)C`\)Cb\)Cd\)Cf\)Ch\)Cj\)Cl\)Cn\)Cp\)Cr\)Ct\)Cv\)Cx\)Cz\)C|\)C~\)C�.C�.C�.C�.C�.C�.C�.C�.C�:�C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�!GC�!GC�.C�.C�!GC�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�!GC�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�!GC�!GC�.C�.C�:�C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�:�C�.C�.C�.C�.C�.C�!GD �D �
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
D�D�
D
D�
D �D �
D!
D!�
D"
D"�
D#
D#��D$
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
DCpDC�
DD
DD�
DE
DE�
DF
DF�
DGpDG�
DHpDH�
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
DO��DP
DP�
DQ
DQ�
DR
DR�
DS
DS�
DTpDT�
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
De�pDf
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
Dk��Dl
Dl�
Dm
Dm�
Dn
Dn�pDo
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
Du�
Dv
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
D��D�K�D���D�˅D��D�N�D���D�˅D��D�K�D��RD�˅D��D�K�D��RD�˅D��D�K�D���D�˅D��D�K�D��RD�˅D��D�K�D���D�˅D��D�K�D���D��RD��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D��RD��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�N�D���D�˅D��D�K�D���D�˅D��D�HRD���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D�RD�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D��RD��D�K�D���D�˅D��D�K�D���D�˅D�RD�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D�D�θD��D�N�DË�D�˅D��D�K�Dċ�D�˅D��D�K�Dŋ�D�˅D��D�K�DƋ�D�˅D��D�K�Dǋ�D�˅D��D�K�Dȋ�D�˅D��D�HRDɋ�D�˅D��D�HRDʋ�D�˅D��D�K�Dˋ�D�˅D��D�K�D̋�D�˅D��D�K�D͋�D�˅D��D�K�DΎ�D�˅D��D�K�Dϋ�D�˅D��D�K�DЋ�D�˅D��D�K�Dы�D�˅D��D�K�Dҋ�D��RD�RD�K�DӋ�D�˅D��D�K�DԎ�D�θD��D�K�DՋ�D�˅D��D�N�D֎�D�˅D��D�K�D׋�D�˅D��D�K�D؋�D�˅D��D�K�Dً�D�˅D��D�K�Dڋ�D�˅D��D�K�Dۋ�D�˅D��D�K�D܋�D�˅D��D�K�D݋�D�˅D��D�K�Dދ�D�˅D��D�K�Dߋ�D�˅D��D�K�D���D�˅D��D�K�DዅD�˅D��D�K�D⋅D�˅D��D�K�D㋅D�˅D��D�K�D䋅D�˅D��D�K�D勅D�˅D��D�K�D担D�˅D��D�K�D狅D�˅D��D�K�D苅D�˅D��D�K�D鋅D�˅D��D�K�DꋅD�˅D��D�K�D닅D�˅D��D�K�D싅D�˅D��D�K�D틅D�˅D��D�K�DD�˅D��D�K�DD�˅D��D�K�D���D�˅D�RD�K�D�D�˅D��D�K�D�D�θD��D�K�D�RD�˅D��D�K�D�D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��D�K�D���D�˅D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aӗ�Aӟ�Aӡ�Aӡ�Aӡ�Aӣ�Aӣ�Aӣ�Aӣ�Aӥ�Aӥ�Aӧ�Aӧ�Aө�AӬAӬAӮAӮAӮAӮAӰ!AӮAӮAӰ!AӰ!AӲ-AӲ-AӴ9AӲ-AӬAӥ�AӑhA�l�A�JAҟ�A�JAЬA��mAʕ�A�A��A�
=AŃA�~�A�l�A��RA�7LA���A��A�G�A�C�A�ƨA���A�+A��A���A��#A���A�x�A��A�ffA�x�A�A��yA�VA��#A�hsA�+A�`BA��A��FA�&�A��DA��hA���A��A��wA�$�A�ȴA��A�7LA�I�A�oA�ĜA��+A�ffA��A�A�I�A��A�ZA��A�r�A��wA�"�A���A���A��-A�"�A���A��/A~ZAv��As�Aq��Anr�Ah��Ag&�Ae+Ab��A_�A_��A^I�A]"�A\{AYO�AV��AT��ARbAO|�AN�RAM�hAL1AJ�yAHM�AD-AC�^ABȴAA��A@M�A?hsA>�9A=�PA<��A;�A:�HA9�A6�9A5��A5�#A5�FA5x�A5�A3�wA,�!A)"�A(9XA'`BA&�RA&M�A&-A%�TA%�FA%x�A%VA$�RA#��A"ȴA"ZA"A!|�A I�A�A�AA�AVAA�A�FA�HA �A\)AO�A�A�FAr�A/A��AA�A`BA��AjA5?A�;A�A
�/A
$�A�A�A��AM�A�A��Ax�A?}A�;Av�A�A%A z�A A�@���@���@�
=@�E�@��j@�E�@�$�@���@�o@�Ĝ@��@�C�@�\@웦@��#@旍@�1'@�p�@�K�@�ff@�&�@܃@۾w@�;d@ڰ!@�-@�hs@���@���@���@��@��@�x�@Ь@�Q�@�  @��;@�$�@�9X@��@�bN@�+@�n�@�~�@őh@�$�@��9@�A�@���@�J@���@��7@�V@��;@��@�~�@�M�@�@��-@���@�X@��`@��@�"�@��+@��@�?}@�V@�1@���@��y@�ȴ@��@��^@�%@�r�@��@��@��@��@� �@�bN@�bN@��;@���@��P@�l�@�+@�V@��#@��T@��7@�%@��`@�Ĝ@��9@���@���@�z�@�z�@�r�@�j@��h@�5?@��7@��@�hs@�X@��j@��@�  @��P@�K�@�;d@��@��@��\@�n�@�V@�M�@�E�@�5?@�@�@��h@���@�r�@���@��F@���@��P@�dZ@��y@��R@��R@��R@��R@�ȴ@��@�l�@��w@���@��@�S�@�;d@���@�ȴ@�n�@�?}@�bN@�  @��F@��F@�|�@��P@�C�@���@�V@�E�@�-@�-@�$�@��@�{@��-@�O�@���@��m@�t�@�
=@�$�@�@�x�@���@���@��@��^@�/@���@���@�I�@� �@��@�  @��;@�ƨ@��@��F@�dZ@�33@�"�@��\@�M�@��+@�;d@��w@��@�r�@���@�b@�;d@�"�@�t�@�  @�(�@�1'@�b@��@��;@�|�@�;d@�+@�"�@�
=@��R@���@��\@�~�@�n�@�5?@��@���@�@���@�7L@���@���@�Q�@� �@��F@�dZ@��@���@�^5@��-@�O�@�?}@�7L@���@�Q�@�  @��@�ƨ@��@���@�S�@�;d@��@�o@��@��H@���@��R@�n�@�@���@��-@���@��7@�G�@�?}@�V@��`@���@��9@��D@�r�@�I�@�b@��;@��@�;d@��H@��\@�ff@�E�@�J@��T@��h@�p�@�X@�G�@�/@�&�@���@��u@��m@���@�+@�@��y@���@�~�@�v�@�^5@�E�@�=q@�{@��T@��@�/@�V@��`@���@�Ĝ@��j@��9@���@��@�Z@�(�@��@��F@��@�l�@�\)@�+@��H@��R@���@�ff@�M�@�$�@�{@�J@�J@�@���@��@�@�X@���@���@��u@�Z@�@\)@
=@~��@~��@~V@}�T@}@}��@}�h@}`B@}V@|�@{��@{�@{�@{�@{dZ@{S�@{33@z�@zn�@y�@yx�@y%@x��@x�@xb@wl�@w\)@w�@v��@vv�@u�@u`B@t��@tZ@t9X@t1@s�m@s�@s@r��@r�!@r�\@r~�@rJ@q&�@p�@pbN@pQ�@p1'@p �@p  @o��@o�@o\)@o�@n�R@nv�@n5?@mO�@lI�@l1@k�@kC�@j��@i�^@iX@i�@h�`@h�9@h�u@h�@hr�@hbN@h1'@h  @g�w@g|�@g�@fv�@f5?@ep�@d�@dI�@cC�@c"�@co@b��@b��@b-@aX@`��@`��@`bN@`A�@`b@_�w@_|�@_l�@_K�@_+@^��@^��@^V@^@]��@]/@\�j@[�m@[dZ@Z��@Z-@Y�#@Y��@Y&�@Y%@X��@X��@XĜ@X�9@XbN@X1'@X  @W�;@W��@W|�@WK�@W;d@W�@V�@V��@V{@U`B@T��@T�j@T��@Tz�@T1@S�F@SS�@R��@RM�@Q�@Q��@Q�7@Qhs@Q7L@P��@P�9@PA�@O��@Ol�@Nȴ@N��@Nv�@NV@N$�@M`B@MV@L�/@Lj@K��@J�@J�!@Jn�@J=q@JJ@I��@I7L@HĜ@H��@H�@G��@GK�@F�y@F��@F5?@F{@E�T@E@E�-@E�@EO�@E/@D�/@D�D@D9X@Cƨ@C�@CdZ@CS�@C@B~�@BJ@A�@A�#@A�#@A��@A��@@�`@@bN@@1'@@ �@?�;@?l�@>��@>5?@=��@=p�@<��@<�@<(�@;�F@;�@:�@:��@:^5@:M�@9��@9��@9��@97L@8��@8r�@7�@7�@7��@7;d@7
=@6ȴ@6{@5�-@5�@5/@4��@4�D@4I�@41@3��@3S�@2�H@2��@2n�@2-@1�@1��@1��@1hs@17L@1�@0��@0Ĝ@0�9@0��@0r�@01'@0  @/�;@/�w@/\)@/+@/
=@.��@.��@-�@-�-@-��@-��@-�h@-?}@,��@,��@,j@,I�@,�@+S�@*��@*M�@*M�@*=q@*-@*J@)�#@)��@)G�@)&�@)�@(��@(�`@(��@(��@(bN@(1'@(  @'�@'�w@'\)@&�@&�R@&��@&��@&�+@&5?@&@%�@%�T@%��@%��@%V@$��@$z�@$j@$�@#��@#t�@#S�@#S�@#"�@"�@"��@"�!@"~�@"M�@"-@!��@!��@!x�@!G�@!%@ ��@ Ĝ@ �9@ �u@ r�@ Q�@  �@   @�@��@�w@��@l�@;d@��@�+@ff@V@E�@E�@5?@{@��@��@`B@�@�@�/@�j@�@�@��@z�@9X@(�@�@��@ƨ@��@��@S�@o@�H@~�@M�@��@hs@7L@Ĝ@��@�@r�@bN@1'@�@�P@|�@
=@�y@�R@��@v�@{@p�@O�@?}@�@�@�@��@�D@(�@1@��@�m@��@S�@"�@@�@�H@��@�\@^5@M�@=q@�@�#@��@hs@7L@%@�`@�9@�9@�u@bN@ �@b@�w@|�@\)@�@�y@�R@�+@V@$�@�@��@�@p�@/@V@V@�@�@z�@I�@I�@(�@�@�@1@�m@�
@�F@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aӗ�Aӟ�Aӡ�Aӡ�Aӡ�Aӣ�Aӣ�Aӣ�Aӣ�Aӥ�Aӥ�Aӧ�Aӧ�Aө�AӬAӬAӮAӮAӮAӮAӰ!AӮAӮAӰ!AӰ!AӲ-AӲ-AӴ9AӲ-AӬAӥ�AӑhA�l�A�JAҟ�A�JAЬA��mAʕ�A�A��A�
=AŃA�~�A�l�A��RA�7LA���A��A�G�A�C�A�ƨA���A�+A��A���A��#A���A�x�A��A�ffA�x�A�A��yA�VA��#A�hsA�+A�`BA��A��FA�&�A��DA��hA���A��A��wA�$�A�ȴA��A�7LA�I�A�oA�ĜA��+A�ffA��A�A�I�A��A�ZA��A�r�A��wA�"�A���A���A��-A�"�A���A��/A~ZAv��As�Aq��Anr�Ah��Ag&�Ae+Ab��A_�A_��A^I�A]"�A\{AYO�AV��AT��ARbAO|�AN�RAM�hAL1AJ�yAHM�AD-AC�^ABȴAA��A@M�A?hsA>�9A=�PA<��A;�A:�HA9�A6�9A5��A5�#A5�FA5x�A5�A3�wA,�!A)"�A(9XA'`BA&�RA&M�A&-A%�TA%�FA%x�A%VA$�RA#��A"ȴA"ZA"A!|�A I�A�A�AA�AVAA�A�FA�HA �A\)AO�A�A�FAr�A/A��AA�A`BA��AjA5?A�;A�A
�/A
$�A�A�A��AM�A�A��Ax�A?}A�;Av�A�A%A z�A A�@���@���@�
=@�E�@��j@�E�@�$�@���@�o@�Ĝ@��@�C�@�\@웦@��#@旍@�1'@�p�@�K�@�ff@�&�@܃@۾w@�;d@ڰ!@�-@�hs@���@���@���@��@��@�x�@Ь@�Q�@�  @��;@�$�@�9X@��@�bN@�+@�n�@�~�@őh@�$�@��9@�A�@���@�J@���@��7@�V@��;@��@�~�@�M�@�@��-@���@�X@��`@��@�"�@��+@��@�?}@�V@�1@���@��y@�ȴ@��@��^@�%@�r�@��@��@��@��@� �@�bN@�bN@��;@���@��P@�l�@�+@�V@��#@��T@��7@�%@��`@�Ĝ@��9@���@���@�z�@�z�@�r�@�j@��h@�5?@��7@��@�hs@�X@��j@��@�  @��P@�K�@�;d@��@��@��\@�n�@�V@�M�@�E�@�5?@�@�@��h@���@�r�@���@��F@���@��P@�dZ@��y@��R@��R@��R@��R@�ȴ@��@�l�@��w@���@��@�S�@�;d@���@�ȴ@�n�@�?}@�bN@�  @��F@��F@�|�@��P@�C�@���@�V@�E�@�-@�-@�$�@��@�{@��-@�O�@���@��m@�t�@�
=@�$�@�@�x�@���@���@��@��^@�/@���@���@�I�@� �@��@�  @��;@�ƨ@��@��F@�dZ@�33@�"�@��\@�M�@��+@�;d@��w@��@�r�@���@�b@�;d@�"�@�t�@�  @�(�@�1'@�b@��@��;@�|�@�;d@�+@�"�@�
=@��R@���@��\@�~�@�n�@�5?@��@���@�@���@�7L@���@���@�Q�@� �@��F@�dZ@��@���@�^5@��-@�O�@�?}@�7L@���@�Q�@�  @��@�ƨ@��@���@�S�@�;d@��@�o@��@��H@���@��R@�n�@�@���@��-@���@��7@�G�@�?}@�V@��`@���@��9@��D@�r�@�I�@�b@��;@��@�;d@��H@��\@�ff@�E�@�J@��T@��h@�p�@�X@�G�@�/@�&�@���@��u@��m@���@�+@�@��y@���@�~�@�v�@�^5@�E�@�=q@�{@��T@��@�/@�V@��`@���@�Ĝ@��j@��9@���@��@�Z@�(�@��@��F@��@�l�@�\)@�+@��H@��R@���@�ff@�M�@�$�@�{@�J@�J@�@���@��@�@�X@���@���@��u@�Z@�@\)@
=@~��@~��@~V@}�T@}@}��@}�h@}`B@}V@|�@{��@{�@{�@{�@{dZ@{S�@{33@z�@zn�@y�@yx�@y%@x��@x�@xb@wl�@w\)@w�@v��@vv�@u�@u`B@t��@tZ@t9X@t1@s�m@s�@s@r��@r�!@r�\@r~�@rJ@q&�@p�@pbN@pQ�@p1'@p �@p  @o��@o�@o\)@o�@n�R@nv�@n5?@mO�@lI�@l1@k�@kC�@j��@i�^@iX@i�@h�`@h�9@h�u@h�@hr�@hbN@h1'@h  @g�w@g|�@g�@fv�@f5?@ep�@d�@dI�@cC�@c"�@co@b��@b��@b-@aX@`��@`��@`bN@`A�@`b@_�w@_|�@_l�@_K�@_+@^��@^��@^V@^@]��@]/@\�j@[�m@[dZ@Z��@Z-@Y�#@Y��@Y&�@Y%@X��@X��@XĜ@X�9@XbN@X1'@X  @W�;@W��@W|�@WK�@W;d@W�@V�@V��@V{@U`B@T��@T�j@T��@Tz�@T1@S�F@SS�@R��@RM�@Q�@Q��@Q�7@Qhs@Q7L@P��@P�9@PA�@O��@Ol�@Nȴ@N��@Nv�@NV@N$�@M`B@MV@L�/@Lj@K��@J�@J�!@Jn�@J=q@JJ@I��@I7L@HĜ@H��@H�@G��@GK�@F�y@F��@F5?@F{@E�T@E@E�-@E�@EO�@E/@D�/@D�D@D9X@Cƨ@C�@CdZ@CS�@C@B~�@BJ@A�@A�#@A�#@A��@A��@@�`@@bN@@1'@@ �@?�;@?l�@>��@>5?@=��@=p�@<��@<�@<(�@;�F@;�@:�@:��@:^5@:M�@9��@9��@9��@97L@8��@8r�@7�@7�@7��@7;d@7
=@6ȴ@6{@5�-@5�@5/@4��@4�D@4I�@41@3��@3S�@2�H@2��@2n�@2-@1�@1��@1��@1hs@17L@1�@0��@0Ĝ@0�9@0��@0r�@01'@0  @/�;@/�w@/\)@/+@/
=@.��@.��@-�@-�-@-��@-��@-�h@-?}@,��@,��@,j@,I�@,�@+S�@*��@*M�@*M�@*=q@*-@*J@)�#@)��@)G�@)&�@)�@(��@(�`@(��@(��@(bN@(1'@(  @'�@'�w@'\)@&�@&�R@&��@&��@&�+@&5?@&@%�@%�T@%��@%��@%V@$��@$z�@$j@$�@#��@#t�@#S�@#S�@#"�@"�@"��@"�!@"~�@"M�@"-@!��@!��@!x�@!G�@!%@ ��@ Ĝ@ �9@ �u@ r�@ Q�@  �@   @�@��@�w@��@l�@;d@��@�+@ff@V@E�@E�@5?@{@��@��@`B@�@�@�/@�j@�@�@��@z�@9X@(�@�@��@ƨ@��@��@S�@o@�H@~�@M�@��@hs@7L@Ĝ@��@�@r�@bN@1'@�@�P@|�@
=@�y@�R@��@v�@{@p�@O�@?}@�@�@�@��@�D@(�@1@��@�m@��@S�@"�@@�@�H@��@�\@^5@M�@=q@�@�#@��@hs@7L@%@�`@�9@�9@�u@bN@ �@b@�w@|�@\)@�@�y@�R@�+@V@$�@�@��@�@p�@/@V@V@�@�@z�@I�@I�@(�@�@�@1@�m@�
@�F@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BD�BD�BE�BE�BE�BF�BF�BF�BF�BF�BF�BF�BF�BF�BG�BG�BG�BF�BG�BH�BI�BJ�BJ�BJ�BI�BH�BI�BK�BR�B\)Be`Bq�B�=B��B��B��B��BB�B�B��B%BuB'�B-B2-B49B8RBA�BE�BJ�BM�BW
BQ�BT�Be`Bs�Bs�Bp�B`BBO�BG�B?}B)�B�B1B��B�B�HB�HB�#B�
B�BŢB�qB�XB�9B�3B�FB��B�Bw�Bt�Bn�B`BBcTB`BB\)BR�B?}B �B�B
��B
�B
�`B
��B
�B
��B
�B
l�B
R�B
9XB
B	�mB	�)B	��B	��B	��B	��B	�PB	{�B	y�B	w�B	o�B	k�B	cTB	XB	P�B	J�B	C�B	B�B	?}B	;dB	5?B	2-B	�B	�B	�B	�B	{B	\B	VB	JB	
=B	
=B	+B	%B	DB	B	B	B	B��B��B�TB��B��B��B��B��B��B��B��B��B��B��BȴBB��B�wB�jB�XB�B��B��B��B��B��B�oB�hB��B�{B�uB�oB�oB�oB�hB�\B�VB�bB�\B�\B�bB�hB�bB�bB�bB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B��B��B�B��B��B��B�hB�bB�{B��B��B��B��B��B��B��B��B��B��B��B��B�-B�FB�LB�RB�^B�dB�?B�3B�-B�RBĜBǮBÖBǮB��B��B��B��B��B��B�B�)B�5B�;B�BB�NB�ZB�sB�yB�B�B�B�B��B	  B	B	B	+B	PB	VB	VB	{B	�B	�B	�B	�B	"�B	$�B	(�B	+B	,B	,B	,B	-B	.B	,B	+B	+B	.B	33B	49B	5?B	6FB	7LB	8RB	:^B	:^B	;dB	=qB	F�B	^5B	iyB	jB	m�B	p�B	q�B	r�B	z�B	~�B	�B	�B	�%B	�+B	�7B	�7B	�7B	�1B	�1B	�1B	�1B	�DB	�bB	�hB	�oB	�uB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�?B	�FB	�RB	�XB	�XB	�RB	�RB	�^B	�}B	��B	ŢB	ȴB	ȴB	ȴB	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ƨB	ŢB	ĜB	ÖB	ĜB	ĜB	ĜB	ŢB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�5B	�HB	�TB	�HB	�HB	�TB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
	7B
	7B

=B

=B
DB
DB
JB
PB
VB
VB
VB
VB
VB
VB
VB
\B
bB
hB
hB
hB
hB
oB
oB
oB
oB
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
+B
,B
+B
,B
-B
-B
.B
.B
.B
.B
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
33B
33B
33B
33B
33B
33B
33B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
5?B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
:^B
:^B
:^B
:^B
;dB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
@�B
@�B
A�B
@�B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
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
XB
XB
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
ZB
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
]/B
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
`BB
`BB
aHB
aHB
aHB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
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
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
k�B
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
m�B
n�B
n�B
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
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
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
u�B
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
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
~�B
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
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BD�BD�BE�BE�BE�BF�BF�BF�BF�BF�BF�BF�BF�BF�BG�BG�BG�BF�BG�BH�BI�BJ�BJ�BJ�BI�BH�BI�BK�BR�B\)Be`Bq�B�=B��B��B��B��BB�B�B��B%BuB'�B-B2-B49B8RBA�BE�BJ�BM�BW
BQ�BT�Be`Bs�Bs�Bp�B`BBO�BG�B?}B)�B�B1B��B�B�HB�HB�#B�
B�BŢB�qB�XB�9B�3B�FB��B�Bw�Bt�Bn�B`BBcTB`BB\)BR�B?}B �B�B
��B
�B
�`B
��B
�B
��B
�B
l�B
R�B
9XB
B	�mB	�)B	��B	��B	��B	��B	�PB	{�B	y�B	w�B	o�B	k�B	cTB	XB	P�B	J�B	C�B	B�B	?}B	;dB	5?B	2-B	�B	�B	�B	�B	{B	\B	VB	JB	
=B	
=B	+B	%B	DB	B	B	B	B��B��B�TB��B��B��B��B��B��B��B��B��B��B��BȴBB��B�wB�jB�XB�B��B��B��B��B��B�oB�hB��B�{B�uB�oB�oB�oB�hB�\B�VB�bB�\B�\B�bB�hB�bB�bB�bB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B��B��B�B��B��B��B�hB�bB�{B��B��B��B��B��B��B��B��B��B��B��B��B�-B�FB�LB�RB�^B�dB�?B�3B�-B�RBĜBǮBÖBǮB��B��B��B��B��B��B�B�)B�5B�;B�BB�NB�ZB�sB�yB�B�B�B�B��B	  B	B	B	+B	PB	VB	VB	{B	�B	�B	�B	�B	"�B	$�B	(�B	+B	,B	,B	,B	-B	.B	,B	+B	+B	.B	33B	49B	5?B	6FB	7LB	8RB	:^B	:^B	;dB	=qB	F�B	^5B	iyB	jB	m�B	p�B	q�B	r�B	z�B	~�B	�B	�B	�%B	�+B	�7B	�7B	�7B	�1B	�1B	�1B	�1B	�DB	�bB	�hB	�oB	�uB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�?B	�FB	�RB	�XB	�XB	�RB	�RB	�^B	�}B	��B	ŢB	ȴB	ȴB	ȴB	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ƨB	ŢB	ĜB	ÖB	ĜB	ĜB	ĜB	ŢB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�5B	�HB	�TB	�HB	�HB	�TB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
	7B
	7B

=B

=B
DB
DB
JB
PB
VB
VB
VB
VB
VB
VB
VB
\B
bB
hB
hB
hB
hB
oB
oB
oB
oB
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
+B
,B
+B
,B
-B
-B
.B
.B
.B
.B
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
33B
33B
33B
33B
33B
33B
33B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
5?B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
:^B
:^B
:^B
:^B
;dB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
@�B
@�B
A�B
@�B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
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
XB
XB
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
ZB
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
]/B
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
`BB
`BB
aHB
aHB
aHB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
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
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
k�B
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
m�B
n�B
n�B
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
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
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
u�B
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
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
~�B
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
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20211210214110  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20211210124233  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20211210124234  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20211210124234  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20211210124234  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20211210124234  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20211210124234  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20211210124234  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20211210124235  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20211210124235                      G�O�G�O�G�O�                JA  ARUP                                                                        20211210125229                      G�O�G�O�G�O�                