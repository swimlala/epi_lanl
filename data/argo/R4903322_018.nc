CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-10-06T10:00:41Z creation      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20201006100041  20201006100041  4903322 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  8286                            2B  A   NAVIS_A                         1165                            170425                          863 @�=�(d,T1   @�=��mf@9|j~��#�dQ�^5?}1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@��BG33BO��BX  B`ffBg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D1��D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DDfDD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@ÅAA!AAAaA��HA��HA��HA��HA��HA��HA��HA��HB p�Bp�Bp�Bp�B p�B(p�B0p�B8�
BA=qBG��BP
>BXp�B`�
Bh
>Bpp�Bxp�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RC )C)C)C)C)C
)C)C)C)C)C)C)C)C)C)C)C )C")C$)C&)C()C*)C,)C.)C0)C2)C4)C6)C8)C:)C<)C>)C@)CB)CD)CF)CH)CJ)CL)CN)CP)CR)CT)CV)CX)CZ)C\)C^)C`)Cb)Cd)Cf)Ch)Cj)Cl)Cn)Cp)Cr)Ct)Cv)Cx)Cz)C|)C~)C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�GC�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D 
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
D�
D	
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
D�pD
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
D�pD 
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
D2 �D2�
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
D8
D8�
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
DDpDD�
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
Dr
Dr�
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
D��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ƸD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�@RD��RD�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D�D�ÅD��D�C�DÃ�D�ÅD��D�C�Dă�D�ÅD��D�C�DŃ�D�ÅD��D�C�Dƃ�D�ÅD��D�C�Dǃ�D�ÅD��D�C�Dȃ�D�ÅD��D�C�DɃ�D�ÅD��D�C�Dʃ�D�ÅD��D�C�D˃�D�ÅD��D�C�D̃�D�ÅD��D�C�D̓�D�ÅD��D�C�D΃�D�ÅD��D�C�Dσ�D�ÅD��D�C�DЃ�D�ÅD��D�C�Dу�D�ÅD��D�C�D҃�D�ÅD��D�C�DӃ�D�ÅD��D�C�Dԃ�D�ÅD��D�C�DՃ�D�ÅD��D�C�Dփ�D�ÅD��D�C�D׃�D�ÅD��D�C�D؃�D�ÅD��D�C�Dك�D�ÅD��D�C�Dڃ�D�ÅD��D�C�Dۃ�D�ÅD��D�C�D܃�D�ÅD��D�C�D݃�D�ÅD��D�C�Dރ�D�ÅD��D�C�D߃�D�ÅD��D�C�D���D�ÅD��D�C�DჅD�ÅD��D�C�D⃅D�ÅD��D�C�DヅD�ÅD��D�C�D䃅D�ÅD��D�C�D僅D�ÅD��D�C�D情D�ÅD��D�C�D烅D�ÅD��D�C�D胅D�ÅD��D�C�D郅D�ÅD��D�C�DꃅD�ÅD��D�C�D냅D�ÅD��D�C�D샅D�ÅD��D�C�D탅D�ÅD��D�C�DD�ÅD��D�C�DD�ÅD��D�C�D���D�ÅD��D�C�D�D�ÅD��D�C�D�D�ÅD��D�C�D�D�ÅD��D�C�D�D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�9XA�33A�bA���A�I�A��A�A��;A߰!AߋDA�t�A�ffA�ZA�XA�O�A�C�A�33A�+A�  A޾wAޡ�A�Q�Aݗ�A�Q�A���A���A�G�A´9A���A�C�A��^A��/A���A���A��mA��`A�G�A���A��7A���A���A��A�ƨA��A��yA�dZA��HA��FA��;A��TA�M�A���A�\)A��/A���A��A���A��A�r�A�ȴA�Q�A��A��`A��9A�=qA��A��A�bNA�bA���A�dZA��A���A�"�A��9A�Q�A��PA�v�A�33A�{A��A���A�r�A�oA��^A�|�A�C�A��`A�ZA��!A�A�I�A�dZA��A~��A}�^A|jA{XAz(�Ay�TAx�jAw�hAv��AvJAup�At�DAs�PAs?}Aq�Apz�Ap9XAo�;AoVAm"�Ak
=Ai7LAh�Ah(�AhbAg�FAg33Af�/AfVAel�Ac/A`��A`A_"�A]S�A\v�A[�AZ��AZ�AY��AY�AY�mAY�PAXz�AW��AV�AV$�AU?}AT�AT�AT�RAT�DAT �AS��AR��AQ33APĜAOp�AN�RAM�TAM33AKG�AJAIO�AH�9AGVAFI�AF5?AE��AE�#AEACAB^5AAoA@��A@I�A?�hA=��A<ĜA<n�A<9XA;�A;%A9�PA8�jA8(�A6ZA5`BA4{A3t�A3�A2��A2�uA2A133A0VA.A�A,�RA+�hA+K�A*��A*bNA(��A'G�A&�A&z�A&=qA%hsA$�A$��A#��A#p�A"�A!��A!;dA!"�A �A �A =qA7LA��AQ�A?}A��A�AS�AĜAJA|�At�A"�AM�AƨA��AO�A��A��A&�A�A��AAZA�A�HAVA
�A
JA	��A	ƨA	�^A	��A��A1Ap�AoA��A1'AA%A�mA;dA�HA��A7LA ��@�hs@�@��#@�/@�ƨ@��#@�|�@�M�@�ƨ@�l�@�+@���@���@� �@���@�Z@��
@��y@�-@�b@�~�@�h@�`B@��@��@ާ�@�Z@�$�@ؓu@��@�V@Ձ@�I�@Ӆ@�t�@�t�@�l�@�K�@���@�K�@�{@�;d@�J@��@Ǯ@Ɵ�@�^5@�=q@�p�@�9X@�C�@�E�@��h@���@�Z@�dZ@��@�7L@���@�r�@�9X@���@��y@�@�7L@�33@�^5@���@�  @�;d@��+@��@��j@�j@�dZ@�V@���@��/@� �@��F@�;d@��7@�bN@���@�C�@�
=@��y@��\@�-@�@�O�@�?}@��j@��D@�(�@�A�@�I�@� �@��P@�C�@�
=@���@�^5@�$�@���@�@�p�@���@��9@�z�@�9X@���@���@���@��@���@�$�@��-@��/@��D@�Q�@��@��@�
=@�ff@��@���@�`B@��@��j@��@��@��@��u@�z�@�bN@�1@��P@�\)@�"�@��R@�v�@��@�`B@�V@��`@��@�Z@�b@���@��F@���@���@�+@��@��!@���@�^5@�@�V@��@�A�@��m@�
=@�n�@�$�@�@���@���@��@���@��u@�r�@�9X@��m@�l�@�+@��@��!@��@�@�33@��H@���@�-@��-@���@��h@�x�@�`B@���@�j@�  @�ƨ@���@�K�@�33@�
=@��H@��H@��R@�ff@�J@���@���@���@���@��@�V@��j@�z�@�bN@�  @��F@�t�@�33@��y@���@�n�@�J@��@���@��-@��@�`B@��@��@��9@�I�@�b@�;@��@|�@~�+@~v�@~{@}�@}p�@}`B@}O�@}V@|��@|9X@|(�@{��@{ƨ@{S�@z�@z��@z^5@zJ@y��@y��@yhs@y�@xA�@w�@w+@v��@u��@t��@t(�@s��@st�@s33@r��@q�^@q�@p�`@p�9@pQ�@p �@o�w@ol�@o�@n�y@nv�@n$�@m�@l��@l�j@l�D@lj@l1@k�m@k��@k33@j~�@i��@iX@iG�@i&�@hĜ@hbN@g��@g|�@g�@f�y@f�R@fv�@fff@fV@e@d��@d��@d�j@d�D@d(�@c�
@ct�@c33@b�@b��@b�!@b-@a��@a��@ahs@aX@`�`@`�u@`r�@`  @_�;@_�w@_�w@_��@_\)@^E�@\��@\�@\�@[S�@Z�!@Z�\@Z^5@Z�@ZJ@Y��@Y�^@X��@X�u@X�@XQ�@XA�@XA�@XA�@Xb@W��@W�P@V��@V��@V$�@U@U�-@U�h@U�@Up�@U?}@U/@U�@T��@T��@T��@T�D@T�D@T�D@TZ@T�@S�
@S��@SS�@S"�@R��@R~�@Q7L@Pr�@O�@O\)@O
=@N5?@MV@L1@K�@KC�@K@J��@J��@J~�@Jn�@J^5@JM�@J-@JJ@I��@H��@H�@H1'@H �@H �@G�@G�@G�w@G�P@G\)@G�@F��@F�y@F�@Fȴ@F�R@Fv�@Fff@FE�@F5?@F{@E�T@E��@Ep�@E�@D��@D(�@D1@C��@Cƨ@C��@C��@C�@CS�@C"�@C@C@C@B�H@B��@B�!@B��@B�\@B^5@A�^@A��@Ax�@@�`@@�u@@�@@r�@@Q�@@  @?�;@?�@?K�@>��@>�@>�R@>�+@>v�@>@=@=�@=�@<�j@<Z@;��@;@:-@9�#@9�7@9%@8��@8A�@7�;@7��@7l�@7;d@6��@6�R@6v�@65?@6{@5��@5�h@5/@4�j@4��@4z�@4I�@4�@3ƨ@3��@3�@3"�@2�!@2n�@2n�@2-@1��@1�7@1G�@1%@0�9@0Q�@/�@/\)@/+@.�@.5?@-��@-�@,��@,�@,�D@,Z@,�@+�
@+��@+dZ@+S�@+o@*�!@*n�@*^5@*=q@*�@*�@*J@)��@)�@)�7@)X@)�@(��@(�@(bN@( �@'��@'\)@'
=@&�@&ff@%�@%�T@%��@%�-@%�h@%�@$�j@$z�@$Z@$Z@$I�@$(�@$�@#��@#C�@#"�@#@"��@"~�@"n�@"^5@"J@!��@!hs@!&�@ Ĝ@ ��@ �u@ �@ bN@ Q�@ 1'@ 1'@  �@ b@�;@K�@
=@�y@ȴ@�+@E�@E�@5?@$�@$�@{@�T@�T@@�-@�@?}@��@�@�@�D@(�@1@�
@dZ@33@��@�\@M�@�@��@��@��@��@��@��@��@�7@hs@G�@&�@��@��@��@ �@��@�@\)@+@�@��@ȴ@�+@v�@ff@E�@@��@@��@p�@p�@p�@p�@`B@O�@O�@?}@/@V@��@��@j@�@�
@��@dZ@"�@o@�@�H@��@~�@M�@-@�@��@7L@&�@�`@Ĝ@��@A�@�;@l�@+@�@ff@V@E�@�-@�@O�@��@�/@�/@�j@��@Z@1@ƨ@��@dZ@o@
�@
^5@	�#@	��@	�7@	X@��@��@r�@�@�w@l�@�y@�@v�@�@��@�-@�@/@V@V@�/@�@j@I�@9X@(�@�@1@1@��@��@�m@�m@�m@�m@�m@�F@��@C�@�H@��@��@M�@�@��@�7@hs@G�@7L@&�@ �`@ ��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�9XA�33A�bA���A�I�A��A�A��;A߰!AߋDA�t�A�ffA�ZA�XA�O�A�C�A�33A�+A�  A޾wAޡ�A�Q�Aݗ�A�Q�A���A���A�G�A´9A���A�C�A��^A��/A���A���A��mA��`A�G�A���A��7A���A���A��A�ƨA��A��yA�dZA��HA��FA��;A��TA�M�A���A�\)A��/A���A��A���A��A�r�A�ȴA�Q�A��A��`A��9A�=qA��A��A�bNA�bA���A�dZA��A���A�"�A��9A�Q�A��PA�v�A�33A�{A��A���A�r�A�oA��^A�|�A�C�A��`A�ZA��!A�A�I�A�dZA��A~��A}�^A|jA{XAz(�Ay�TAx�jAw�hAv��AvJAup�At�DAs�PAs?}Aq�Apz�Ap9XAo�;AoVAm"�Ak
=Ai7LAh�Ah(�AhbAg�FAg33Af�/AfVAel�Ac/A`��A`A_"�A]S�A\v�A[�AZ��AZ�AY��AY�AY�mAY�PAXz�AW��AV�AV$�AU?}AT�AT�AT�RAT�DAT �AS��AR��AQ33APĜAOp�AN�RAM�TAM33AKG�AJAIO�AH�9AGVAFI�AF5?AE��AE�#AEACAB^5AAoA@��A@I�A?�hA=��A<ĜA<n�A<9XA;�A;%A9�PA8�jA8(�A6ZA5`BA4{A3t�A3�A2��A2�uA2A133A0VA.A�A,�RA+�hA+K�A*��A*bNA(��A'G�A&�A&z�A&=qA%hsA$�A$��A#��A#p�A"�A!��A!;dA!"�A �A �A =qA7LA��AQ�A?}A��A�AS�AĜAJA|�At�A"�AM�AƨA��AO�A��A��A&�A�A��AAZA�A�HAVA
�A
JA	��A	ƨA	�^A	��A��A1Ap�AoA��A1'AA%A�mA;dA�HA��A7LA ��@�hs@�@��#@�/@�ƨ@��#@�|�@�M�@�ƨ@�l�@�+@���@���@� �@���@�Z@��
@��y@�-@�b@�~�@�h@�`B@��@��@ާ�@�Z@�$�@ؓu@��@�V@Ձ@�I�@Ӆ@�t�@�t�@�l�@�K�@���@�K�@�{@�;d@�J@��@Ǯ@Ɵ�@�^5@�=q@�p�@�9X@�C�@�E�@��h@���@�Z@�dZ@��@�7L@���@�r�@�9X@���@��y@�@�7L@�33@�^5@���@�  @�;d@��+@��@��j@�j@�dZ@�V@���@��/@� �@��F@�;d@��7@�bN@���@�C�@�
=@��y@��\@�-@�@�O�@�?}@��j@��D@�(�@�A�@�I�@� �@��P@�C�@�
=@���@�^5@�$�@���@�@�p�@���@��9@�z�@�9X@���@���@���@��@���@�$�@��-@��/@��D@�Q�@��@��@�
=@�ff@��@���@�`B@��@��j@��@��@��@��u@�z�@�bN@�1@��P@�\)@�"�@��R@�v�@��@�`B@�V@��`@��@�Z@�b@���@��F@���@���@�+@��@��!@���@�^5@�@�V@��@�A�@��m@�
=@�n�@�$�@�@���@���@��@���@��u@�r�@�9X@��m@�l�@�+@��@��!@��@�@�33@��H@���@�-@��-@���@��h@�x�@�`B@���@�j@�  @�ƨ@���@�K�@�33@�
=@��H@��H@��R@�ff@�J@���@���@���@���@��@�V@��j@�z�@�bN@�  @��F@�t�@�33@��y@���@�n�@�J@��@���@��-@��@�`B@��@��@��9@�I�@�b@�;@��@|�@~�+@~v�@~{@}�@}p�@}`B@}O�@}V@|��@|9X@|(�@{��@{ƨ@{S�@z�@z��@z^5@zJ@y��@y��@yhs@y�@xA�@w�@w+@v��@u��@t��@t(�@s��@st�@s33@r��@q�^@q�@p�`@p�9@pQ�@p �@o�w@ol�@o�@n�y@nv�@n$�@m�@l��@l�j@l�D@lj@l1@k�m@k��@k33@j~�@i��@iX@iG�@i&�@hĜ@hbN@g��@g|�@g�@f�y@f�R@fv�@fff@fV@e@d��@d��@d�j@d�D@d(�@c�
@ct�@c33@b�@b��@b�!@b-@a��@a��@ahs@aX@`�`@`�u@`r�@`  @_�;@_�w@_�w@_��@_\)@^E�@\��@\�@\�@[S�@Z�!@Z�\@Z^5@Z�@ZJ@Y��@Y�^@X��@X�u@X�@XQ�@XA�@XA�@XA�@Xb@W��@W�P@V��@V��@V$�@U@U�-@U�h@U�@Up�@U?}@U/@U�@T��@T��@T��@T�D@T�D@T�D@TZ@T�@S�
@S��@SS�@S"�@R��@R~�@Q7L@Pr�@O�@O\)@O
=@N5?@MV@L1@K�@KC�@K@J��@J��@J~�@Jn�@J^5@JM�@J-@JJ@I��@H��@H�@H1'@H �@H �@G�@G�@G�w@G�P@G\)@G�@F��@F�y@F�@Fȴ@F�R@Fv�@Fff@FE�@F5?@F{@E�T@E��@Ep�@E�@D��@D(�@D1@C��@Cƨ@C��@C��@C�@CS�@C"�@C@C@C@B�H@B��@B�!@B��@B�\@B^5@A�^@A��@Ax�@@�`@@�u@@�@@r�@@Q�@@  @?�;@?�@?K�@>��@>�@>�R@>�+@>v�@>@=@=�@=�@<�j@<Z@;��@;@:-@9�#@9�7@9%@8��@8A�@7�;@7��@7l�@7;d@6��@6�R@6v�@65?@6{@5��@5�h@5/@4�j@4��@4z�@4I�@4�@3ƨ@3��@3�@3"�@2�!@2n�@2n�@2-@1��@1�7@1G�@1%@0�9@0Q�@/�@/\)@/+@.�@.5?@-��@-�@,��@,�@,�D@,Z@,�@+�
@+��@+dZ@+S�@+o@*�!@*n�@*^5@*=q@*�@*�@*J@)��@)�@)�7@)X@)�@(��@(�@(bN@( �@'��@'\)@'
=@&�@&ff@%�@%�T@%��@%�-@%�h@%�@$�j@$z�@$Z@$Z@$I�@$(�@$�@#��@#C�@#"�@#@"��@"~�@"n�@"^5@"J@!��@!hs@!&�@ Ĝ@ ��@ �u@ �@ bN@ Q�@ 1'@ 1'@  �@ b@�;@K�@
=@�y@ȴ@�+@E�@E�@5?@$�@$�@{@�T@�T@@�-@�@?}@��@�@�@�D@(�@1@�
@dZ@33@��@�\@M�@�@��@��@��@��@��@��@��@�7@hs@G�@&�@��@��@��@ �@��@�@\)@+@�@��@ȴ@�+@v�@ff@E�@@��@@��@p�@p�@p�@p�@`B@O�@O�@?}@/@V@��@��@j@�@�
@��@dZ@"�@o@�@�H@��@~�@M�@-@�@��@7L@&�@�`@Ĝ@��@A�@�;@l�@+@�@ff@V@E�@�-@�@O�@��@�/@�/@�j@��@Z@1@ƨ@��@dZ@o@
�@
^5@	�#@	��@	�7@	X@��@��@r�@�@�w@l�@�y@�@v�@�@��@�-@�@/@V@V@�/@�@j@I�@9X@(�@�@1@1@��@��@�m@�m@�m@�m@�m@�F@��@C�@�H@��@��@M�@�@��@�7@hs@G�@7L@&�@ �`@ ��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B{BoBbB\BPB
=B	7B1B1B	7B
=B
=B	7B1B1B+BBB��B��B�B�5B��B�qB��B�B~�B~�B}�B}�B�B|�Bu�Bk�BYBQ�BN�BE�B@�B7LB+B#�B�B�B	7B�B�HB�)B�
B��B��BŢB�jB�9B�B��B��B�hB�JB}�Bl�BcTB[#BQ�BJ�B<jB-B$�B �B�B�B  B
��B
�B
�NB
�5B
�)B
�B
�
B
��B
��B
ȴB
ĜB
��B
�jB
�9B
�B
��B
��B
�PB
�B
}�B
x�B
m�B
hsB
aHB
]/B
YB
P�B
K�B
F�B
B�B
=qB
7LB
33B
/B
#�B
 �B
�B
�B
\B
B	��B	��B	�B	�B	�B	�B	�yB	�`B	�BB	�B	ƨB	�}B	�^B	�9B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�VB	�=B	�%B	�B	�B	�B	�B	}�B	z�B	u�B	m�B	jB	dZB	_;B	[#B	W
B	N�B	G�B	B�B	@�B	9XB	49B	33B	2-B	1'B	-B	'�B	!�B	�B	�B	{B	uB	DB	%B	B	B	B��B��B�B�B�fB�TB�/B�B�
B��B��B��B��BǮB��B�^B�9B�-B�!B�B��B��B��B��B��B��B��B��B�hB�bB�\B�JB�7B�1B�+B�B�B� B}�Bw�Bs�Bq�Bo�Bm�Bk�BiyBgmBffBffBdZBbNBaHB`BB^5B\)BZBXBT�BS�BQ�BO�BM�BK�BG�BF�BE�BE�BD�BD�BD�BC�BA�BA�B@�B@�B>wB>wB<jB;dB:^B9XB7LB6FB6FB49B49B2-B2-B1'B/B.B.B.B-B-B,B+B+B'�B'�B'�B'�B%�B%�B#�B"�B!�B"�B�B!�B!�B!�B"�B"�B#�B$�B$�B$�B$�B#�B"�B%�B'�B(�B+B-B.B1'B2-B2-B2-B33B6FB7LB8RB9XB:^B;dB=qB@�BA�BC�BB�BC�BD�BE�BF�BI�BM�BN�BR�BVBYBZB_;BbNBcTBhsBk�Bm�Bs�Bx�By�B~�B�JB�bB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�'B�3B�?B�?B�FB�RB�dB�qBBÖBĜBƨBȴB��B��B��B��B��B�B�B�)B�/B�BB�NB�ZB�`B�ZB�ZB�`B�`B�fB�sB�B�B�B�B��B��B��B��B	  B	B	B	B	+B	+B	1B	1B	JB	VB	bB	oB	�B	�B	�B	�B	"�B	$�B	#�B	%�B	&�B	&�B	'�B	(�B	.B	/B	0!B	1'B	2-B	49B	5?B	6FB	8RB	:^B	;dB	=qB	A�B	D�B	E�B	F�B	F�B	H�B	H�B	I�B	K�B	L�B	M�B	N�B	O�B	P�B	R�B	S�B	T�B	VB	W
B	YB	[#B	^5B	^5B	`BB	cTB	e`B	ffB	jB	l�B	o�B	o�B	s�B	t�B	v�B	w�B	y�B	{�B	|�B	~�B	~�B	� B	�B	�B	�B	�B	�B	�%B	�7B	�DB	�DB	�JB	�VB	�oB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�'B	�3B	�FB	�RB	�RB	�XB	�^B	�dB	�jB	�qB	�wB	�wB	��B	��B	ÖB	ŢB	ŢB	ŢB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�/B	�;B	�BB	�BB	�BB	�HB	�TB	�ZB	�`B	�fB	�fB	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
%B
%B
%B
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

=B

=B

=B
DB
DB
DB
VB
\B
bB
hB
hB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+B
,B
,B
,B
,B
,B
-B
-B
-B
-B
.B
.B
.B
.B
/B
/B
/B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
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
6FB
6FB
6FB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
<jB
=qB
=qB
=qB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
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
I�B
I�B
J�B
K�B
K�B
K�B
K�B
L�B
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
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
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
\)B
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
]/B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
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
bNB
bNB
bNB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
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
hsB
hsB
hsB
hsB
hsB
iyB
jB
jB
jB
jB
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
o�B
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
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
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
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B{BoBbB\BPB
=B	7B1B1B	7B
=B
=B	7B1B1B+BBB��B��B�B�5B��B�qB��B�B~�B~�B}�B}�B�B|�Bu�Bk�BYBQ�BN�BE�B@�B7LB+B#�B�B�B	7B�B�HB�)B�
B��B��BŢB�jB�9B�B��B��B�hB�JB}�Bl�BcTB[#BQ�BJ�B<jB-B$�B �B�B�B  B
��B
�B
�NB
�5B
�)B
�B
�
B
��B
��B
ȴB
ĜB
��B
�jB
�9B
�B
��B
��B
�PB
�B
}�B
x�B
m�B
hsB
aHB
]/B
YB
P�B
K�B
F�B
B�B
=qB
7LB
33B
/B
#�B
 �B
�B
�B
\B
B	��B	��B	�B	�B	�B	�B	�yB	�`B	�BB	�B	ƨB	�}B	�^B	�9B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�VB	�=B	�%B	�B	�B	�B	�B	}�B	z�B	u�B	m�B	jB	dZB	_;B	[#B	W
B	N�B	G�B	B�B	@�B	9XB	49B	33B	2-B	1'B	-B	'�B	!�B	�B	�B	{B	uB	DB	%B	B	B	B��B��B�B�B�fB�TB�/B�B�
B��B��B��B��BǮB��B�^B�9B�-B�!B�B��B��B��B��B��B��B��B��B�hB�bB�\B�JB�7B�1B�+B�B�B� B}�Bw�Bs�Bq�Bo�Bm�Bk�BiyBgmBffBffBdZBbNBaHB`BB^5B\)BZBXBT�BS�BQ�BO�BM�BK�BG�BF�BE�BE�BD�BD�BD�BC�BA�BA�B@�B@�B>wB>wB<jB;dB:^B9XB7LB6FB6FB49B49B2-B2-B1'B/B.B.B.B-B-B,B+B+B'�B'�B'�B'�B%�B%�B#�B"�B!�B"�B�B!�B!�B!�B"�B"�B#�B$�B$�B$�B$�B#�B"�B%�B'�B(�B+B-B.B1'B2-B2-B2-B33B6FB7LB8RB9XB:^B;dB=qB@�BA�BC�BB�BC�BD�BE�BF�BI�BM�BN�BR�BVBYBZB_;BbNBcTBhsBk�Bm�Bs�Bx�By�B~�B�JB�bB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�'B�3B�?B�?B�FB�RB�dB�qBBÖBĜBƨBȴB��B��B��B��B��B�B�B�)B�/B�BB�NB�ZB�`B�ZB�ZB�`B�`B�fB�sB�B�B�B�B��B��B��B��B	  B	B	B	B	+B	+B	1B	1B	JB	VB	bB	oB	�B	�B	�B	�B	"�B	$�B	#�B	%�B	&�B	&�B	'�B	(�B	.B	/B	0!B	1'B	2-B	49B	5?B	6FB	8RB	:^B	;dB	=qB	A�B	D�B	E�B	F�B	F�B	H�B	H�B	I�B	K�B	L�B	M�B	N�B	O�B	P�B	R�B	S�B	T�B	VB	W
B	YB	[#B	^5B	^5B	`BB	cTB	e`B	ffB	jB	l�B	o�B	o�B	s�B	t�B	v�B	w�B	y�B	{�B	|�B	~�B	~�B	� B	�B	�B	�B	�B	�B	�%B	�7B	�DB	�DB	�JB	�VB	�oB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�'B	�3B	�FB	�RB	�RB	�XB	�^B	�dB	�jB	�qB	�wB	�wB	��B	��B	ÖB	ŢB	ŢB	ŢB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�/B	�;B	�BB	�BB	�BB	�HB	�TB	�ZB	�`B	�fB	�fB	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
%B
%B
%B
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

=B

=B

=B
DB
DB
DB
VB
\B
bB
hB
hB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+B
,B
,B
,B
,B
,B
-B
-B
-B
-B
.B
.B
.B
.B
/B
/B
/B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
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
6FB
6FB
6FB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
<jB
=qB
=qB
=qB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
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
I�B
I�B
J�B
K�B
K�B
K�B
K�B
L�B
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
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
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
\)B
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
]/B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
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
bNB
bNB
bNB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
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
hsB
hsB
hsB
hsB
hsB
iyB
jB
jB
jB
jB
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
o�B
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
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
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
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.11 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20201006100041                              AO  ARCAADJP                                                                    20201006100041    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20201006100041  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20201006100041  QCF$                G�O�G�O�G�O�0               