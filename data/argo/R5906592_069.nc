CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-03-31T12:01:49Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  pL   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �(   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �4   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �4   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �4   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �4   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �`   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �h   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �p   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230331120149  20230331120149  5906592 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               EA   AO  8756                            2B  A   NAVIS_A                         1284                            170425                          863 @� �z��1   @� 5��@0�t��dZ�]c�f1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         EA   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D7��D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� DrfDr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D���D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�ɚD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��R@ÅAA!AAAaA��HA��HA��HA��HA��HA��HA��HA��HB p�Bp�Bp�Bp�B p�B(p�B0p�B8p�B@p�BHp�BPp�BXp�B`p�Bhp�Bpp�Bxp�B�8RB�8RB�8RB�8RB�8RB�8RB�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�k�B�8RB�C )C)C)C)C)C
)C)C)C)C)C)C)C)C)C)C)C )C")C$)C&)C()C*)C,)C.)C0)C2)C4)C6)C8)C:)C<)C>)C@)CB5�CD)CF)CH)CJ)CL)CN)CP)CR)CT)CV)CX)CZ)C\)C^)C`)Cb)Cd)Cf)Ch)Cj)Cl)Cn)Cp)Cr)Ct)Cv)Cx)Cz)C|)C~)C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D 
D �
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�pD	
D	�
D

D
�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D 
D �
D!
D!�
D"
D"�
D#
D#�
D$
D$�
D%
D%�
D&
D&�
D'
D'�
D(
D(�
D)
D)�
D*
D*�
D+
D+�
D,
D,�
D-
D-�
D.
D.�
D/
D/�
D0
D0�
D1
D1�
D2
D2�
D3
D3�
D4
D4�
D5
D5�
D6
D6�
D7
D7�
D8 �D8�
D9
D9�
D:
D:�
D;
D;�
D<
D<�
D=
D=�
D>
D>�
D?
D?�
D@
D@�
DA
DA�
DB
DB�
DC
DC�
DD
DD�
DE
DE�
DF
DF�
DG
DG�
DH
DH�
DI
DI�
DJ
DJ�
DK
DK�
DL
DL�
DM
DM�
DN
DN�
DO
DO�
DP
DP�
DQ
DQ�
DR
DR�
DS
DS�
DT
DT�
DU
DU�
DV
DV�
DW
DW�
DX
DX�
DY
DY�
DZ
DZ�
D[
D[�
D\
D\�
D]
D]�
D^
D^�
D_
D_�
D`
D`�
Da
Da�
Db
Db�
Dc
Dc�
Dd
Dd�
De
De�
Df
Df�
Dg
Dg�
Dh
Dh�
Di
Di�
Dj
Dj�
Dk
Dk�
Dl
Dl�
Dm
Dm�
Dn
Dn�
Do
Do�
Dp
Dp�
Dq
Dq�
DrpDr�
Ds
Ds�
Dt
Dt�
Du
Du�
Dv
Dv�
Dw
Dw�
Dx
Dx�
Dy
Dy�
Dz
Dz�
D{
D{�
D|
D|�
D}
D}�
D~
D~�
D
D�
D��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�F�D���D�ÅD� RD�C�D���D�ÅD��D�C�D�D�ÅD��D�C�DÃ�D�ÅD��D�C�Dă�D�ÅD��D�C�DŃ�D�ÅD��D�C�Dƃ�D�ÅD��D�C�Dǃ�D�ÅD��D�C�Dȃ�D�ÅD��D�C�DɃ�D�ÅD��D�C�Dʃ�D�ÅD��D�C�D˃�D�ÅD��D�C�D̃�D�ÅD��D�C�D̓�D�ÅD��D�C�D΃�D�ÅD��D�C�Dσ�D�ÅD��D�C�DЃ�D�ÅD��D�C�Dу�D�ÅD��D�C�D҃�D�ÅD��D�C�DӃ�D�ÅD��D�C�Dԃ�D�ÅD��D�C�DՃ�D�ÅD��D�C�Dփ�D�ÅD��D�C�D׃�D�ÅD��D�C�D؃�D�ÅD��D�C�Dك�D�ÅD��D�C�Dڃ�D�ÅD��D�C�Dۃ�D�ÅD��D�C�D܃�D�ÅD��D�C�D݃�D�ÅD��D�C�Dރ�D�ÅD��D�C�D߃�D�ÅD��D�C�D���D�ÅD��D�C�DჅD�ÅD��D�C�D⃅D�ÅD��D�C�DヅD�ÅD��D�C�D䃅D�ÅD��D�C�D僅D�ÅD��D�C�D情D�ÅD��D�C�D烅D�ÅD��D�C�D胅D�ÅD��D�C�D郅D�ÅD��D�C�DꃅD�ÅD��D�C�D냅D�ÅD��D�C�D샅D�ÅD��D�C�D탅D�ÅD��D�C�DD�ÅD��D�C�DD�ÅD��D�C�D���D�ÅD��D�C�D�D�ÅD��D�C�D�D�ÅD��D�C�D�D�ÅD��D�C�D�D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D��D��R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AѲ-AѴ9AѸRAѸRAѼjA�ƨA���A���A���A���A���A���A���A���A���A���A���A���A�ȴAѣ�A�t�A�ZA�G�A��A�ƨA�ZAϰ!A�K�A��TA�v�A�S�A�9XA��A��#A͡�Aʹ9A��HA��A��A��`A�ȴAͬAͺ^A��A��
A��A�ĜAͩ�A͏\A̓A�=qA�M�A��A�?}A� �A���A�~�A��A˥�A��
A��A�K�A��hA��A�E�A��A���A�\)A�|�A�x�A���A�`BA��jA��PA���A���A�~�A�/A� �A���A���A�(�A��#A��;A�9XA��A�(�A�z�A�=qA��A���A��^A�hsA���A�hsA�(�A�n�A�I�A|z�Av��AuhsAr��Ap�jAnĜAj�Ad��Ab�+AahsA`A�A^-A[�FAY��AV�ARAPz�ANJAH{AE�-AD�AD9XACoAB$�AA
=A?O�A=��A;p�A9�
A9oA8��A8(�A7��A7S�A6�/A6I�A5�-A5l�A4�RA09XA/�A.��A.=qA-�A.A-hsA+G�A*M�A'A"ĜA!A!�A!�hA"�HA"5?A^5AK�A�FA{A�mA�A��A�\AG�A��A�#A��A�#A�AG�A �A�A
�!A	��A	
=A�^A�A��AZA�A��AVA9XA�;AVA�A �yA b@��@��H@���@�J@�?}@��@��@���@�/@��
@���@�$�@���@�G�@��/@�b@��y@�-@�`B@�?}@���@@�ȴ@�p�@�u@��m@���@�F@�w@땁@�l�@�33@�R@�X@��`@蛦@�j@��
@���@�h@��/@�@��@��@�A�@�ȴ@��#@���@�ƨ@�S�@��@���@�p�@���@�z�@�(�@ۍP@ٲ-@�bN@��m@�t�@֏\@�@ղ-@Չ7@�X@�7L@��`@ԓu@�r�@�  @ӶF@�dZ@�33@���@�ff@�p�@�%@�z�@�1@϶F@�S�@�
=@���@�n�@�ff@�~�@��T@���@�%@��@���@�j@�j@� �@˥�@�C�@��@�o@�33@�K�@�@ʏ\@ʧ�@ʇ+@�-@�$�@��@�@�7L@ȋD@ȃ@�j@�Z@ț�@�9X@�\)@��y@�=q@��T@ũ�@�G�@���@Ĭ@�b@��@�;d@���@�@�~�@���@�@��@�G�@��j@���@�C�@�~�@�ff@�^5@�E�@�J@�@�7L@�Ĝ@�I�@�b@���@��@�t�@�"�@���@�5?@�-@�$�@��@�hs@�%@��@��@�  @���@��;@��P@�dZ@�S�@�@�@���@�p�@�/@���@��j@���@��@��@��@�;d@�C�@���@�@��#@�x�@�7L@���@�Q�@�(�@�ƨ@�t�@�l�@�33@���@��!@�E�@�$�@���@��/@��9@��D@�I�@��@��P@�"�@�33@�~�@�@��^@���@�O�@��D@��@�o@�=q@��@�hs@�7L@�V@�%@���@�1'@�1@��@��;@��F@�|�@�"�@�
=@���@�ff@�=q@�@�x�@�V@���@��j@�r�@� �@��@���@�C�@�o@�ȴ@��+@�ff@�E�@�-@��@��^@��7@��@���@��F@���@�-@��@���@�/@���@��j@��@�Z@�I�@�A�@��
@�33@���@��+@�ff@�5?@��T@��7@�x�@�hs@�X@�7L@�V@��`@��j@���@�1'@�b@��@���@���@�K�@��@�
=@�@���@��y@��y@��H@��@���@���@�7L@���@��@�j@�1'@�1@��@���@��P@�|�@�"�@���@���@�-@�J@�@���@���@���@��@��@��T@���@���@��@�hs@�O�@��@��`@���@���@�r�@�A�@�(�@�  @�K�@�"�@��H@�M�@�5?@�J@��#@���@�hs@�/@��@���@��j@��@���@�j@� �@��
@���@�
=@��+@�ff@�=q@�{@�@���@��h@�G�@�&�@���@�9X@���@��@�dZ@��@���@�^5@�V@�M�@��#@�/@�%@��@���@�Q�@�A�@�I�@�(�@�b@��@\)@
=@~��@}�h@|��@|�@|�@|1@{�@{C�@{@zn�@y�#@y7L@x��@x �@w+@vv�@u@t��@t�@t9X@tI�@t�@t1@s�m@s��@sC�@s@q��@q��@pbN@o�;@o�@o;d@nȴ@nff@m�@m��@m@m�-@m�h@m?}@l��@lj@kt�@k33@j�@j�!@jn�@j�@i�7@h�`@hA�@g�@g��@gK�@f�@f��@fE�@f@e�@e�-@e��@e��@d��@c�@b�\@a�7@a%@`��@`�`@`Ĝ@`r�@`b@_�@_��@_�w@_��@_l�@_+@^�y@^ȴ@^��@^v�@^E�@^@]�-@]�-@]p�@\(�@[�F@["�@Z��@Z~�@ZM�@Z-@Y��@Yhs@X��@Xr�@Xb@W��@W;d@V�R@V5?@U�@U��@U�h@UV@TZ@SdZ@R��@R-@Q�#@Q�^@P��@PQ�@P �@P  @O��@OK�@N��@N��@Nv�@NV@M�h@M?}@M/@MV@L�j@Lz�@L1@K33@K@J��@J=q@J�@I�^@IG�@H�u@Hb@G��@G;d@FV@E�@E�h@EO�@EV@D�j@D�D@Dz�@Dz�@DI�@C�
@C"�@B�@B�!@B^5@B=q@A��@@Q�@@b@?�;@?|�@?�@>��@>v�@>V@>V@>5?@=�@=?}@=�@<�j@;ƨ@;33@:��@:��@:�\@:M�@:J@9X@9&�@9�@9%@8��@8��@8�9@8�u@8Q�@8 �@8  @7�;@7��@7�P@7|�@7�@6��@6@5�-@5O�@5?}@5/@5�@5V@4��@4�@4��@4Z@3ƨ@3��@3t�@3C�@3"�@3@2�H@2�!@2=q@1�#@1hs@1�@0�u@0r�@0 �@/�@/�P@/|�@/K�@/
=@.��@.�y@.�R@.��@.v�@.V@.5?@.$�@.@-�@-�T@-@-p�@,�@,�@,z�@,j@,I�@,�@+�@*��@*n�@*^5@*=q@)��@)�#@)��@)x�@)7L@(��@(�9@(��@(r�@(A�@'�@'��@'\)@'K�@';d@'�@&ȴ@&��@&E�@%O�@$�/@$�j@$�@$�D@$Z@$9X@#��@#t�@#C�@#"�@#o@"�H@"��@"M�@"-@!�@!�^@!hs@ �`@ �9@ 1'@�w@�P@\)@;d@+@+@�@�@v�@{@@�-@�h@?}@��@��@�@��@�@��@��@�D@9X@1@ƨ@S�@33@"�@o@�@�H@=q@�@J@��@��@�@�#@�^@��@G�@&�@��@��@�9@��@��@r�@ �@  @�@�;@��@��@��@ȴ@�+@ff@E�@{@�@��@@�-@��@�@`B@?}@�@V@�/@�@Z@1@�
@�F@�@S�@"�@"�@"�@"�@o@�@~�@^5@=q@J@��@��@��@X@G�@&�@��@Ĝ@��@bN@A�@ �@  @��@|�@\)@l�@l�@+@�@
=@��@�R@E�@$�@�@�T@��@/@V@�@�@�j@z�@j@�m@ƨ@��@dZ@33@
�@
��@
��@
�!@
n�@
=q@	�#@	��@	x�@	G�@	&�@	&�@	�@	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AѲ-AѴ9AѸRAѸRAѼjA�ƨA���A���A���A���A���A���A���A���A���A���A���A���A�ȴAѣ�A�t�A�ZA�G�A��A�ƨA�ZAϰ!A�K�A��TA�v�A�S�A�9XA��A��#A͡�Aʹ9A��HA��A��A��`A�ȴAͬAͺ^A��A��
A��A�ĜAͩ�A͏\A̓A�=qA�M�A��A�?}A� �A���A�~�A��A˥�A��
A��A�K�A��hA��A�E�A��A���A�\)A�|�A�x�A���A�`BA��jA��PA���A���A�~�A�/A� �A���A���A�(�A��#A��;A�9XA��A�(�A�z�A�=qA��A���A��^A�hsA���A�hsA�(�A�n�A�I�A|z�Av��AuhsAr��Ap�jAnĜAj�Ad��Ab�+AahsA`A�A^-A[�FAY��AV�ARAPz�ANJAH{AE�-AD�AD9XACoAB$�AA
=A?O�A=��A;p�A9�
A9oA8��A8(�A7��A7S�A6�/A6I�A5�-A5l�A4�RA09XA/�A.��A.=qA-�A.A-hsA+G�A*M�A'A"ĜA!A!�A!�hA"�HA"5?A^5AK�A�FA{A�mA�A��A�\AG�A��A�#A��A�#A�AG�A �A�A
�!A	��A	
=A�^A�A��AZA�A��AVA9XA�;AVA�A �yA b@��@��H@���@�J@�?}@��@��@���@�/@��
@���@�$�@���@�G�@��/@�b@��y@�-@�`B@�?}@���@@�ȴ@�p�@�u@��m@���@�F@�w@땁@�l�@�33@�R@�X@��`@蛦@�j@��
@���@�h@��/@�@��@��@�A�@�ȴ@��#@���@�ƨ@�S�@��@���@�p�@���@�z�@�(�@ۍP@ٲ-@�bN@��m@�t�@֏\@�@ղ-@Չ7@�X@�7L@��`@ԓu@�r�@�  @ӶF@�dZ@�33@���@�ff@�p�@�%@�z�@�1@϶F@�S�@�
=@���@�n�@�ff@�~�@��T@���@�%@��@���@�j@�j@� �@˥�@�C�@��@�o@�33@�K�@�@ʏ\@ʧ�@ʇ+@�-@�$�@��@�@�7L@ȋD@ȃ@�j@�Z@ț�@�9X@�\)@��y@�=q@��T@ũ�@�G�@���@Ĭ@�b@��@�;d@���@�@�~�@���@�@��@�G�@��j@���@�C�@�~�@�ff@�^5@�E�@�J@�@�7L@�Ĝ@�I�@�b@���@��@�t�@�"�@���@�5?@�-@�$�@��@�hs@�%@��@��@�  @���@��;@��P@�dZ@�S�@�@�@���@�p�@�/@���@��j@���@��@��@��@�;d@�C�@���@�@��#@�x�@�7L@���@�Q�@�(�@�ƨ@�t�@�l�@�33@���@��!@�E�@�$�@���@��/@��9@��D@�I�@��@��P@�"�@�33@�~�@�@��^@���@�O�@��D@��@�o@�=q@��@�hs@�7L@�V@�%@���@�1'@�1@��@��;@��F@�|�@�"�@�
=@���@�ff@�=q@�@�x�@�V@���@��j@�r�@� �@��@���@�C�@�o@�ȴ@��+@�ff@�E�@�-@��@��^@��7@��@���@��F@���@�-@��@���@�/@���@��j@��@�Z@�I�@�A�@��
@�33@���@��+@�ff@�5?@��T@��7@�x�@�hs@�X@�7L@�V@��`@��j@���@�1'@�b@��@���@���@�K�@��@�
=@�@���@��y@��y@��H@��@���@���@�7L@���@��@�j@�1'@�1@��@���@��P@�|�@�"�@���@���@�-@�J@�@���@���@���@��@��@��T@���@���@��@�hs@�O�@��@��`@���@���@�r�@�A�@�(�@�  @�K�@�"�@��H@�M�@�5?@�J@��#@���@�hs@�/@��@���@��j@��@���@�j@� �@��
@���@�
=@��+@�ff@�=q@�{@�@���@��h@�G�@�&�@���@�9X@���@��@�dZ@��@���@�^5@�V@�M�@��#@�/@�%@��@���@�Q�@�A�@�I�@�(�@�b@��@\)@
=@~��@}�h@|��@|�@|�@|1@{�@{C�@{@zn�@y�#@y7L@x��@x �@w+@vv�@u@t��@t�@t9X@tI�@t�@t1@s�m@s��@sC�@s@q��@q��@pbN@o�;@o�@o;d@nȴ@nff@m�@m��@m@m�-@m�h@m?}@l��@lj@kt�@k33@j�@j�!@jn�@j�@i�7@h�`@hA�@g�@g��@gK�@f�@f��@fE�@f@e�@e�-@e��@e��@d��@c�@b�\@a�7@a%@`��@`�`@`Ĝ@`r�@`b@_�@_��@_�w@_��@_l�@_+@^�y@^ȴ@^��@^v�@^E�@^@]�-@]�-@]p�@\(�@[�F@["�@Z��@Z~�@ZM�@Z-@Y��@Yhs@X��@Xr�@Xb@W��@W;d@V�R@V5?@U�@U��@U�h@UV@TZ@SdZ@R��@R-@Q�#@Q�^@P��@PQ�@P �@P  @O��@OK�@N��@N��@Nv�@NV@M�h@M?}@M/@MV@L�j@Lz�@L1@K33@K@J��@J=q@J�@I�^@IG�@H�u@Hb@G��@G;d@FV@E�@E�h@EO�@EV@D�j@D�D@Dz�@Dz�@DI�@C�
@C"�@B�@B�!@B^5@B=q@A��@@Q�@@b@?�;@?|�@?�@>��@>v�@>V@>V@>5?@=�@=?}@=�@<�j@;ƨ@;33@:��@:��@:�\@:M�@:J@9X@9&�@9�@9%@8��@8��@8�9@8�u@8Q�@8 �@8  @7�;@7��@7�P@7|�@7�@6��@6@5�-@5O�@5?}@5/@5�@5V@4��@4�@4��@4Z@3ƨ@3��@3t�@3C�@3"�@3@2�H@2�!@2=q@1�#@1hs@1�@0�u@0r�@0 �@/�@/�P@/|�@/K�@/
=@.��@.�y@.�R@.��@.v�@.V@.5?@.$�@.@-�@-�T@-@-p�@,�@,�@,z�@,j@,I�@,�@+�@*��@*n�@*^5@*=q@)��@)�#@)��@)x�@)7L@(��@(�9@(��@(r�@(A�@'�@'��@'\)@'K�@';d@'�@&ȴ@&��@&E�@%O�@$�/@$�j@$�@$�D@$Z@$9X@#��@#t�@#C�@#"�@#o@"�H@"��@"M�@"-@!�@!�^@!hs@ �`@ �9@ 1'@�w@�P@\)@;d@+@+@�@�@v�@{@@�-@�h@?}@��@��@�@��@�@��@��@�D@9X@1@ƨ@S�@33@"�@o@�@�H@=q@�@J@��@��@�@�#@�^@��@G�@&�@��@��@�9@��@��@r�@ �@  @�@�;@��@��@��@ȴ@�+@ff@E�@{@�@��@@�-@��@�@`B@?}@�@V@�/@�@Z@1@�
@�F@�@S�@"�@"�@"�@"�@o@�@~�@^5@=q@J@��@��@��@X@G�@&�@��@Ĝ@��@bN@A�@ �@  @��@|�@\)@l�@l�@+@�@
=@��@�R@E�@$�@�@�T@��@/@V@�@�@�j@z�@j@�m@ƨ@��@dZ@33@
�@
��@
��@
�!@
n�@
=q@	�#@	��@	x�@	G�@	&�@	&�@	�@	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
t�B
r�B
r�B
r�B
t�B
w�B
w�B
�B
�bB
�JB
�JB
�JB
�hB
�PB
�DB
��B
��B
��B
�B
�B
�B
��B
�B
�LB
�qB
B
ĜB
B
ÖB
ƨB
ǮB
��B\)BiyBo�Bk�B�%B��B��B�uB�uB�JB��B�/B��B+BbB,B/B.B,B&�B�B��B�)B��B�^B��Bp�BI�B8RB$�BJB
�B
�B
�`B
�5B
�
B
��B
��B
�VB
}�B
w�B
r�B
jB
dZB
P�B
49B
1B	�ZB	�B	ȴB	�jB	�B	��B	}�B	p�B	l�B	e`B	^5B	R�B	H�B	;dB	)�B	�B	�B	B��B��B��B��B�B�B�B�mB�TB�/B�)B�#B�5B��B	DB	uB	 �B	�B	�B	�B	 �B	�B	�B	 �B	(�B	.B	)�B	�B	/B	 �B	PB		7B	PB	\B	,B	<jB	�B	bB	B��B��B��B�B�B�yB�mB�fB�ZB�BB�BB�;B�5B�/B�5B�5B�5B�;B�BB�BB�NB�TB�TB�ZB�`B�`B�mB�mB�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B	  B	B	1B		7B		7B		7B	DB	PB	oB	�B	�B	�B	�B	�B	�B	�B	!�B	$�B	)�B	)�B	+B	,B	.B	2-B	7LB	9XB	:^B	:^B	9XB	;dB	@�B	C�B	F�B	I�B	J�B	K�B	N�B	O�B	Q�B	Q�B	R�B	T�B	\)B	_;B	`BB	aHB	e`B	gmB	gmB	hsB	iyB	jB	l�B	m�B	p�B	q�B	u�B	w�B	|�B	� B	� B	�B	�+B	�PB	�hB	�oB	�oB	�uB	��B	�{B	��B	��B	��B	��B	��B	��B	�B	�!B	�3B	�3B	�3B	�?B	�FB	�FB	�LB	�RB	�dB	�qB	��B	B	ĜB	ŢB	ƨB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�/B	�/B	�/B	�5B	�5B	�;B	�BB	�HB	�BB	�BB	�BB	�BB	�BB	�HB	�NB	�TB	�ZB	�TB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�mB	�yB	�yB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
+B
1B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
DB

=B
	7B
+B
%B
%B
%B
%B
+B
+B
+B
+B
+B
+B
1B
	7B
	7B

=B

=B

=B
DB
DB
DB
DB
DB
DB
JB
JB
JB
JB
PB
PB
PB
PB
PB
VB
VB
VB
\B
\B
\B
VB
VB
\B
\B
bB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
(�B
(�B
(�B
)�B
+B
+B
)�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
-B
.B
.B
/B
0!B
0!B
0!B
0!B
0!B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
33B
33B
49B
49B
49B
49B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
=qB
=qB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
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
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
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
L�B
L�B
M�B
M�B
M�B
N�B
N�B
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
O�B
P�B
O�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
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
XB
XB
XB
XB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
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
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
dZB
dZB
e`B
dZB
dZB
e`B
e`B
e`B
e`B
e`B
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
gmB
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
k�B
l�B
l�B
l�B
l�B
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
o�B
o�B
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
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
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
z�B
z�B
z�B
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
}�B
}�B
}�B
}�B
}�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
t�B
r�B
r�B
r�B
t�B
w�B
w�B
�B
�bB
�JB
�JB
�JB
�hB
�PB
�DB
��B
��B
��B
�B
�B
�B
��B
�B
�LB
�qB
B
ĜB
B
ÖB
ƨB
ǮB
��B\)BiyBo�Bk�B�%B��B��B�uB�uB�JB��B�/B��B+BbB,B/B.B,B&�B�B��B�)B��B�^B��Bp�BI�B8RB$�BJB
�B
�B
�`B
�5B
�
B
��B
��B
�VB
}�B
w�B
r�B
jB
dZB
P�B
49B
1B	�ZB	�B	ȴB	�jB	�B	��B	}�B	p�B	l�B	e`B	^5B	R�B	H�B	;dB	)�B	�B	�B	B��B��B��B��B�B�B�B�mB�TB�/B�)B�#B�5B��B	DB	uB	 �B	�B	�B	�B	 �B	�B	�B	 �B	(�B	.B	)�B	�B	/B	 �B	PB		7B	PB	\B	,B	<jB	�B	bB	B��B��B��B�B�B�yB�mB�fB�ZB�BB�BB�;B�5B�/B�5B�5B�5B�;B�BB�BB�NB�TB�TB�ZB�`B�`B�mB�mB�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B	  B	B	1B		7B		7B		7B	DB	PB	oB	�B	�B	�B	�B	�B	�B	�B	!�B	$�B	)�B	)�B	+B	,B	.B	2-B	7LB	9XB	:^B	:^B	9XB	;dB	@�B	C�B	F�B	I�B	J�B	K�B	N�B	O�B	Q�B	Q�B	R�B	T�B	\)B	_;B	`BB	aHB	e`B	gmB	gmB	hsB	iyB	jB	l�B	m�B	p�B	q�B	u�B	w�B	|�B	� B	� B	�B	�+B	�PB	�hB	�oB	�oB	�uB	��B	�{B	��B	��B	��B	��B	��B	��B	�B	�!B	�3B	�3B	�3B	�?B	�FB	�FB	�LB	�RB	�dB	�qB	��B	B	ĜB	ŢB	ƨB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�/B	�/B	�/B	�5B	�5B	�;B	�BB	�HB	�BB	�BB	�BB	�BB	�BB	�HB	�NB	�TB	�ZB	�TB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�mB	�yB	�yB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
+B
1B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
DB

=B
	7B
+B
%B
%B
%B
%B
+B
+B
+B
+B
+B
+B
1B
	7B
	7B

=B

=B

=B
DB
DB
DB
DB
DB
DB
JB
JB
JB
JB
PB
PB
PB
PB
PB
VB
VB
VB
\B
\B
\B
VB
VB
\B
\B
bB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
(�B
(�B
(�B
)�B
+B
+B
)�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
-B
.B
.B
/B
0!B
0!B
0!B
0!B
0!B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
33B
33B
49B
49B
49B
49B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
=qB
=qB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
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
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
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
L�B
L�B
M�B
M�B
M�B
N�B
N�B
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
O�B
P�B
O�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
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
XB
XB
XB
XB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
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
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
dZB
dZB
e`B
dZB
dZB
e`B
e`B
e`B
e`B
e`B
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
gmB
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
k�B
l�B
l�B
l�B
l�B
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
o�B
o�B
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
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
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
z�B
z�B
z�B
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
}�B
}�B
}�B
}�B
}�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.11 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230331120149                              AO  ARCAADJP                                                                    20230331120149    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230331120149  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20230331120149  QCF$                G�O�G�O�G�O�0               