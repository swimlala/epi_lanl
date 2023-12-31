CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-03-26T00:37:07Z creation;2019-03-26T00:37:10Z conversion to V3.1;2019-12-23T06:04:50Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        \  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \,   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  s8   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  �l   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  �0   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  ˌ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �H   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �H   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �H   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �H   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20190326003707  20200120021523  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_134                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @رw*I��1   @رw�9 @8�<�쿱�c6%��1�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� DqfDq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ Dؼ�D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@ÅAA!AAAaA��HA��HA��HA��HA��HA��HA��HA��HB p�Bp�Bp�Bp�B p�B(p�B0p�B8p�B@p�BHp�BPp�BXp�B`p�Bhp�Bpp�Bxp�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�k�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RC )C)C)C)C)C
)C)C)C)C)C)C)C)C)C)C)C )C")C$)C&)C()C*)C,)C.)C0)C2)C4)C6)C8)C:)C<)C>)C@)CB)CD)CF)CH)CJ)CL)CN)CP)CR)CT)CV)CX)CZ)C\)C^)C`)Cb)Cd)Cf)Ch)Cj)Cl)Cn)Cp)Cr)Ct)Cv)Cx)Cz)C|)C~)C�C�HC�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�HC�C�C�HC�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D 
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
DqqDq�
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
D��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D�D�ÅD��D�C�DÃ�D�ÅD��D�C�Dă�D�ÅD��D�C�DŃ�D�ÅD��D�C�Dƃ�D�ÅD��D�C�Dǃ�D�ÅD��D�C�Dȃ�D�ÅD��D�C�DɃ�D�ÅD��D�C�Dʃ�D�ÅD��D�C�D˃�D�ÅD��D�C�D̃�D�ÅD��D�C�D̓�D�ÅD��D�C�D΃�D�ÅD��D�C�Dσ�D�ÅD��D�C�DЃ�D�ÅD��D�C�Dу�D�ÅD��D�C�D҃�D�ÅD��D�C�DӃ�D�ÅD��D�C�Dԃ�D�ÅD��D�C�DՃ�D�ÅD��D�C�Dփ�D�ÅD��D�C�D׃�D�ÅD��D�C�D؃�D��RD��D�C�Dك�D�ÅD��D�C�Dڃ�D�ÅD��D�C�Dۃ�D�ÅD��D�C�D܃�D�ÅD��D�C�D݃�D�ÅD��D�C�Dރ�D�ÅD��D�C�D߃�D�ÅD��D�C�D���D�ÅD��D�C�DჅD�ÅD��D�C�D⃅D�ÅD��D�C�DヅD�ÅD��D�C�D䃅D�ÅD��D�C�D僅D�ÅD��D�C�D情D�ÅD��D�C�D烅D�ÅD��D�C�D胅D�ÅD��D�C�D郅D�ÅD��D�C�DꃅD�ÅD��D�C�D냅D�ÅD��D�C�D샅D�ÅD��D�C�D탅D�ÅD��D�C�DD�ÅD��D�C�DD�ÅD��D�C�D���D�ÅD��D�C�D�D�ÅD��D�C�D�D�ÅD��D�C�D�D�ÅD��D�C�D�D�ÅD��D�C�D���D�ƸD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A�{A�oA�JA�VA�bA�bA�JA�VA�JA�VA�VA�VA�VA�oA�oA�bA�VA�A��
A��\A���A��A��A��PA�t�A�K�A��TA��yA��9A�|�A���A��A�\)A��uA�I�A���A�JA��wA�dZA�K�A�O�A��FA���A���A�G�A�O�A���A�ZA�dZA� �A��#A� �A���A��/A�(�A��yA��PA�XA���A�/A���A��;A�hsA�bNA��A��A���A�{A�bA���A��hA��uA�~�A��A�M�A�(�A��A�dZA��A�=qA���A�
=A��A�dZA���A�;dA��A���A�C�A���A��7A��
A%A|Az��Ay�AwO�Av�/Av �Ar�RAo+AjM�AgAeC�AdAbȴA`jAa;dAb��Ac"�Ab-A_�TA]��A]"�A\�yA\�9AZ�HAXZAV�AU?}AT�ATJAR�9AQ��APz�AO33AMS�AL{AJI�AHI�AG��AG�mAG�PAE��AEAD�uAD�AC�;AB��AAl�A@��A>bA:jA6ffA4ZA1�FA/�mA.�/A.1A-x�A,��A,�A++A(�A&ffA%K�A$�RA$bNA#?}A#A"bNA"bA!��A ĜA ^5AO�A��A�9AbA��A�7A|�AhsA�`A$�A�A^5A��A��A�yA�AƨA�AS�AbA/AZAA?}A$�A�7A
��A
~�A	+A�mA��A�A�\A{AhsA��A(�A|�AȴAr�AS�@��m@��@�t�@�~�@��#@���@���@���@�hs@�X@�/@�o@���@�j@�`B@��@��@�r�@���@�\@㝲@�h@�I�@�33@ް!@�M�@���@ݩ�@�X@�I�@�^5@ׅ@��@�?}@��@�ff@�@мj@�A�@Ͼw@ϝ�@ϝ�@ϝ�@ϝ�@Ο�@�hs@̴9@�
=@��@ɡ�@�X@�%@�Ĝ@�b@���@ēu@�bN@���@� �@��@��!@�^5@�J@��/@��@�"�@��-@��u@���@��\@�M�@���@��7@�X@�`B@�X@��@���@���@�9X@��@�=q@�5?@���@�Ĝ@��m@�K�@��H@�ff@��7@�r�@�  @�dZ@��@���@�5?@�{@��@�hs@��@�1'@�l�@��y@��-@�?}@���@��@��@��@���@�E�@��-@��7@�G�@��@���@��@��w@�S�@�@��y@�{@��#@�@�x�@�X@�V@�Ĝ@��D@�Q�@�b@��
@�"�@�~�@�-@��@���@�hs@�O�@�7L@���@��@�(�@��m@���@���@��@�33@�33@�;d@��@��@���@�=q@�{@�J@�@���@�/@�%@���@���@�%@�%@�%@�%@���@��/@���@��@��u@�z�@�9X@��P@��@��@��!@�M�@�@��#@���@��7@��7@��h@��7@�O�@�&�@�V@�%@���@��/@���@�Ĝ@���@�r�@�1'@�b@��;@�|�@�o@���@���@��+@�n�@�V@�V@�M�@��@���@��^@���@��7@�X@�V@���@��j@��@���@�bN@�9X@�1@��;@��;@�|�@�\)@�\)@�S�@�;d@��@���@�ȴ@���@�~�@�n�@�M�@�5?@���@���@���@�x�@�hs@�G�@��@�V@�V@���@���@���@���@���@�r�@� �@�1@��m@��m@���@�\)@�C�@�+@�
=@���@���@�5?@�@��@��T@��^@�p�@�X@��@��/@��9@�j@�1'@�b@���@��m@���@��F@���@�l�@�C�@�@�ȴ@�~�@�ff@�E�@�J@��T@���@�V@���@��@��u@��D@�bN@�1@�@�@�@~�R@~V@}�@}��@}O�@}?}@}V@|�/@|�@|��@|Z@{��@{��@{S�@z��@z-@z�@y��@yhs@y7L@y&�@x�9@x�@xA�@w�w@w;d@v�@u�-@t��@t�D@t�D@t9X@s��@st�@r�!@rn�@rJ@q�7@q7L@q&�@q�@q%@p��@p1'@o�w@ol�@n�y@n�R@nv�@nE�@n{@m�-@m�h@mp�@l�@lZ@k�m@k�F@k�@k33@k"�@j�\@jn�@jM�@jJ@i�7@i7L@h��@h�u@h �@g�@g�@g|�@g
=@fȴ@f��@fv�@fV@f5?@ep�@d�@d��@d�D@dZ@c��@c�F@c33@b�H@b~�@a��@a�^@a��@a�7@ahs@a%@`A�@` �@`b@_�@_�;@_�P@^�@^{@]��@]p�@]/@\��@\�@\�j@\1@[��@[�@[S�@Z��@Z�\@Z�@Y�^@Yhs@YG�@Y&�@Y%@Xr�@Xb@W�;@W��@W�@W|�@W
=@Vȴ@V��@V�+@V{@U�h@U�h@U�@U�@U`B@T��@TI�@S��@Sƨ@So@R��@R=q@RJ@Q�#@Q��@QG�@Q�@P�9@P�@PQ�@PA�@P  @O��@O��@O�P@O\)@N�y@NV@M�T@Mp�@M�@L��@LI�@L1@L1@K��@Kƨ@KC�@J�!@J=q@I�@Ix�@I&�@H��@Hr�@HA�@Hb@G�@G��@G
=@F��@Fv�@Fff@FE�@E�@E��@E`B@E/@E�@D��@D��@D��@D(�@C��@CdZ@CC�@B��@B=q@A�#@A�7@Ax�@A7L@@��@@�`@@r�@@Q�@@ �@@  @?��@?|�@?+@>�y@>ȴ@>��@>V@=�@=�@=�@<�/@<�j@<��@<z�@<j@<Z@<I�@<1@;��@;��@;S�@;"�@:��@:��@:�@9��@9�@9�#@9��@9x�@8Ĝ@8�@8Q�@8  @7K�@6�R@6�+@6{@5`B@5/@4�@41@3dZ@333@2�@2��@2n�@2J@1��@1�7@1�7@1x�@1hs@17L@1%@0��@0�@0bN@0 �@/�w@/l�@/�@.��@.E�@.5?@-�@-@-��@-�@-O�@-/@-�@-V@,��@,��@,�/@,�j@,�j@,�j@,��@,j@+�m@+dZ@+C�@+33@+"�@*��@*��@*��@*-@)�@)�@)�7@)X@)G�@)&�@(��@(��@(r�@(b@'\)@'
=@&ȴ@&v�@&ff@&E�@%�-@%��@%�h@%p�@%O�@%/@%V@$�/@$��@$�@$Z@$(�@$�@#��@#��@#�m@#�m@#��@#C�@"��@"��@"��@"�!@"��@"�\@"-@!�@!�^@!x�@!X@!G�@!G�@!&�@ ��@ �u@ r�@ A�@   @��@��@�P@|�@l�@\)@\)@K�@�@�@��@ff@$�@�@��@��@p�@/@V@�/@�@z�@�@�
@��@t�@C�@@�@��@n�@n�@^5@=q@�@��@��@�@��@�^@��@��@G�@%@�`@�9@�@bN@Q�@1'@  @�w@��@�P@K�@+@
=@�R@��@�+@�+@�+@v�@$�@@p�@O�@/@/@V@��@�j@��@�D@z�@�@��@�@S�@C�@"�@@��@�!@n�@�@��@�#@�^@�7@��@1'@�w@��@\)@��@�R@��@��@v�@v�@E�@5?@$�@@@@�T@�-@�@`B@/@/@�@�@j@1@��@dZ@
�@
��@
�!11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A�{A�oA�JA�VA�bA�bA�JA�VA�JA�VA�VA�VA�VA�oA�oA�bA�VA�A��
A��\A���A��A��A��PA�t�A�K�A��TA��yA��9A�|�A���A��A�\)A��uA�I�A���A�JA��wA�dZA�K�A�O�A��FA���A���A�G�A�O�A���A�ZA�dZA� �A��#A� �A���A��/A�(�A��yA��PA�XA���A�/A���A��;A�hsA�bNA��A��A���A�{A�bA���A��hA��uA�~�A��A�M�A�(�A��A�dZA��A�=qA���A�
=A��A�dZA���A�;dA��A���A�C�A���A��7A��
A%A|Az��Ay�AwO�Av�/Av �Ar�RAo+AjM�AgAeC�AdAbȴA`jAa;dAb��Ac"�Ab-A_�TA]��A]"�A\�yA\�9AZ�HAXZAV�AU?}AT�ATJAR�9AQ��APz�AO33AMS�AL{AJI�AHI�AG��AG�mAG�PAE��AEAD�uAD�AC�;AB��AAl�A@��A>bA:jA6ffA4ZA1�FA/�mA.�/A.1A-x�A,��A,�A++A(�A&ffA%K�A$�RA$bNA#?}A#A"bNA"bA!��A ĜA ^5AO�A��A�9AbA��A�7A|�AhsA�`A$�A�A^5A��A��A�yA�AƨA�AS�AbA/AZAA?}A$�A�7A
��A
~�A	+A�mA��A�A�\A{AhsA��A(�A|�AȴAr�AS�@��m@��@�t�@�~�@��#@���@���@���@�hs@�X@�/@�o@���@�j@�`B@��@��@�r�@���@�\@㝲@�h@�I�@�33@ް!@�M�@���@ݩ�@�X@�I�@�^5@ׅ@��@�?}@��@�ff@�@мj@�A�@Ͼw@ϝ�@ϝ�@ϝ�@ϝ�@Ο�@�hs@̴9@�
=@��@ɡ�@�X@�%@�Ĝ@�b@���@ēu@�bN@���@� �@��@��!@�^5@�J@��/@��@�"�@��-@��u@���@��\@�M�@���@��7@�X@�`B@�X@��@���@���@�9X@��@�=q@�5?@���@�Ĝ@��m@�K�@��H@�ff@��7@�r�@�  @�dZ@��@���@�5?@�{@��@�hs@��@�1'@�l�@��y@��-@�?}@���@��@��@��@���@�E�@��-@��7@�G�@��@���@��@��w@�S�@�@��y@�{@��#@�@�x�@�X@�V@�Ĝ@��D@�Q�@�b@��
@�"�@�~�@�-@��@���@�hs@�O�@�7L@���@��@�(�@��m@���@���@��@�33@�33@�;d@��@��@���@�=q@�{@�J@�@���@�/@�%@���@���@�%@�%@�%@�%@���@��/@���@��@��u@�z�@�9X@��P@��@��@��!@�M�@�@��#@���@��7@��7@��h@��7@�O�@�&�@�V@�%@���@��/@���@�Ĝ@���@�r�@�1'@�b@��;@�|�@�o@���@���@��+@�n�@�V@�V@�M�@��@���@��^@���@��7@�X@�V@���@��j@��@���@�bN@�9X@�1@��;@��;@�|�@�\)@�\)@�S�@�;d@��@���@�ȴ@���@�~�@�n�@�M�@�5?@���@���@���@�x�@�hs@�G�@��@�V@�V@���@���@���@���@���@�r�@� �@�1@��m@��m@���@�\)@�C�@�+@�
=@���@���@�5?@�@��@��T@��^@�p�@�X@��@��/@��9@�j@�1'@�b@���@��m@���@��F@���@�l�@�C�@�@�ȴ@�~�@�ff@�E�@�J@��T@���@�V@���@��@��u@��D@�bN@�1@�@�@�@~�R@~V@}�@}��@}O�@}?}@}V@|�/@|�@|��@|Z@{��@{��@{S�@z��@z-@z�@y��@yhs@y7L@y&�@x�9@x�@xA�@w�w@w;d@v�@u�-@t��@t�D@t�D@t9X@s��@st�@r�!@rn�@rJ@q�7@q7L@q&�@q�@q%@p��@p1'@o�w@ol�@n�y@n�R@nv�@nE�@n{@m�-@m�h@mp�@l�@lZ@k�m@k�F@k�@k33@k"�@j�\@jn�@jM�@jJ@i�7@i7L@h��@h�u@h �@g�@g�@g|�@g
=@fȴ@f��@fv�@fV@f5?@ep�@d�@d��@d�D@dZ@c��@c�F@c33@b�H@b~�@a��@a�^@a��@a�7@ahs@a%@`A�@` �@`b@_�@_�;@_�P@^�@^{@]��@]p�@]/@\��@\�@\�j@\1@[��@[�@[S�@Z��@Z�\@Z�@Y�^@Yhs@YG�@Y&�@Y%@Xr�@Xb@W�;@W��@W�@W|�@W
=@Vȴ@V��@V�+@V{@U�h@U�h@U�@U�@U`B@T��@TI�@S��@Sƨ@So@R��@R=q@RJ@Q�#@Q��@QG�@Q�@P�9@P�@PQ�@PA�@P  @O��@O��@O�P@O\)@N�y@NV@M�T@Mp�@M�@L��@LI�@L1@L1@K��@Kƨ@KC�@J�!@J=q@I�@Ix�@I&�@H��@Hr�@HA�@Hb@G�@G��@G
=@F��@Fv�@Fff@FE�@E�@E��@E`B@E/@E�@D��@D��@D��@D(�@C��@CdZ@CC�@B��@B=q@A�#@A�7@Ax�@A7L@@��@@�`@@r�@@Q�@@ �@@  @?��@?|�@?+@>�y@>ȴ@>��@>V@=�@=�@=�@<�/@<�j@<��@<z�@<j@<Z@<I�@<1@;��@;��@;S�@;"�@:��@:��@:�@9��@9�@9�#@9��@9x�@8Ĝ@8�@8Q�@8  @7K�@6�R@6�+@6{@5`B@5/@4�@41@3dZ@333@2�@2��@2n�@2J@1��@1�7@1�7@1x�@1hs@17L@1%@0��@0�@0bN@0 �@/�w@/l�@/�@.��@.E�@.5?@-�@-@-��@-�@-O�@-/@-�@-V@,��@,��@,�/@,�j@,�j@,�j@,��@,j@+�m@+dZ@+C�@+33@+"�@*��@*��@*��@*-@)�@)�@)�7@)X@)G�@)&�@(��@(��@(r�@(b@'\)@'
=@&ȴ@&v�@&ff@&E�@%�-@%��@%�h@%p�@%O�@%/@%V@$�/@$��@$�@$Z@$(�@$�@#��@#��@#�m@#�m@#��@#C�@"��@"��@"��@"�!@"��@"�\@"-@!�@!�^@!x�@!X@!G�@!G�@!&�@ ��@ �u@ r�@ A�@   @��@��@�P@|�@l�@\)@\)@K�@�@�@��@ff@$�@�@��@��@p�@/@V@�/@�@z�@�@�
@��@t�@C�@@�@��@n�@n�@^5@=q@�@��@��@�@��@�^@��@��@G�@%@�`@�9@�@bN@Q�@1'@  @�w@��@�P@K�@+@
=@�R@��@�+@�+@�+@v�@$�@@p�@O�@/@/@V@��@�j@��@�D@z�@�@��@�@S�@C�@"�@@��@�!@n�@�@��@�#@�^@�7@��@1'@�w@��@\)@��@�R@��@��@v�@v�@E�@5?@$�@@@@�T@�-@�@`B@/@/@�@�@j@1@��@dZ@
�@
��@
�!11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	�/B	�/B	�/B	�/B	�/B	�/B	�/B	�/B	�/B	�/B	�/B	�/B	�5B	�5B	�5B	�5B	�BB	�NB	�B

=B
-B
?}B
�^B
�BB%B\B�B+B0!B5?B>wB?}BF�B=qB=qBI�B\)B`BBffB�dB�BJBoBoB��B�LB��B�PB�JB�JB�VB��B��B��B�B�!B�B��B�^BǮB�XB��B�PBz�B_;BZBK�BA�BJ�BK�BgmBZBG�B5?B{BB
��B
�B
�sB
�)B
��B
�FB
��B
��B
}�B
K�B
=qB
�B
DB
B	�B	�B	�RB	��B	��B	��B	��B	��B	��B	�bB	jB	1'B	!�B	�B	uB	�B	VB	6FB	z�B	�B	� B	{�B	p�B	l�B	jB	iyB	bNB	N�B	A�B	33B	+B	(�B	!�B	�B	uB	JB��B��B�yB�)B�B�
B�
B��B��B��BɺBǮB��B�LB�!B��B�1Bk�B]/BS�BM�BL�BK�BI�BH�BI�BF�BL�BM�BI�BG�BJ�BP�BR�BR�BQ�BQ�BP�BO�BN�BK�BG�BE�BE�BD�BD�BC�BB�B@�B?}B<jB;dB=qB=qB=qB<jB:^B:^B7LB6FB49B49B33B1'B1'B/B.B/B-B-B-B0!B49B5?B33B1'B1'B:^BB�BI�BE�BA�BA�BA�BA�BA�BA�BA�BA�B@�B?}B@�B?}B?}B>wB;dB9XB7LB49B1'B.B,B,B,B+B)�B)�B(�B'�B)�B-B/B0!B2-B7LB=qB@�BD�BE�BH�BK�BM�BQ�BR�BXB\)B_;BcTBe`BffBffBgmBgmBhsBl�Bk�Bk�Bk�BffBl�Bq�Bq�Bp�Bp�Bl�Bp�Bn�Bm�Bn�Bs�Bt�Bu�Bv�Bx�By�By�Bx�Bw�Bw�Bx�B|�B~�B� B�%B�7B�JB�\B�oB�{B��B��B��B��B��B��B��B��B��B�B�'B�9B�^B��BȴB��B��B�B�B�)B�HB�`B�B�B�B�B��B��B��B	B	%B	%B	bB	oB	uB	�B	�B	�B	�B	�B	�B	 �B	"�B	(�B	/B	2-B	49B	8RB	;dB	=qB	?}B	D�B	I�B	O�B	Q�B	T�B	T�B	VB	ZB	ZB	\)B	^5B	`BB	bNB	e`B	gmB	iyB	k�B	n�B	s�B	t�B	t�B	u�B	u�B	v�B	v�B	v�B	v�B	w�B	w�B	y�B	y�B	z�B	}�B	~�B	~�B	� B	�B	�B	�%B	�+B	�1B	�7B	�7B	�7B	�7B	�=B	�DB	�DB	�DB	�JB	�JB	�JB	�JB	�VB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�3B	�3B	�9B	�FB	�LB	�RB	�XB	�XB	�dB	�dB	�jB	�jB	�wB	�}B	�}B	��B	B	ĜB	ĜB	ŢB	ŢB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�B	�)B	�/B	�/B	�/B	�5B	�;B	�;B	�BB	�HB	�HB	�TB	�ZB	�`B	�`B	�fB	�fB	�fB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
%B
+B
+B
1B
1B
1B
1B
	7B
	7B

=B

=B

=B

=B

=B
DB
DB
DB
DB
JB
JB
PB
PB
PB
VB
VB
VB
\B
\B
\B
bB
bB
bB
hB
hB
hB
oB
oB
oB
uB
uB
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
#�B
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
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
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
/B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
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
33B
33B
33B
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
7LB
8RB
7LB
7LB
8RB
8RB
8RB
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
<jB
<jB
<jB
<jB
<jB
<jB
<jB
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
>wB
>wB
?}B
?}B
?}B
?}B
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
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
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
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
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
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
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
Q�B
Q�B
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
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
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
ZB
[#B
[#B
[#B
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
]/B
]/B
]/B
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
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
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
cTB
cTB
cTB
cTB
dZB
dZB
dZB
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
iyB
iyB
jB
jB
jB
k�B
k�B
k�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	�/B	�B	�/B	�B	�/B	�/B	�B	�/B	�B	�/B	�B	�/B	�B	�5B	�5B	�B	�BB	�NB	�B

=B
-B
?}B
�^B
�}BBB\B�B*�B0!B5%B>wB?}BF�B=qB=qBI�B\)B`BBffB�JB�wBJBoBTB��B�LB��B�6B�JB�JB�VB�sB��B��B�B�!B�B��B�DBǔB�>B��B�PBz�B_;BZBK�BA�BJ�BK�BgmBZBG�B5?BaBB
��B
�B
�sB
�B
͹B
�+B
��B
��B
}�B
K�B
=VB
�B
)B
B	�B	�B	�RB	��B	��B	��B	��B	��B	��B	�bB	jB	1B	!�B	�B	uB	yB	VB	6FB	z�B	�B	�B	{�B	p�B	l�B	jB	i_B	b4B	N�B	AoB	33B	+B	(�B	!�B	�B	[B	JB��B��B�yB�)B��B��B�
B��B͹B��BɠBǮB��B�2B�!B��B�1BkkB]BS�BM�BL�BK�BI�BH�BI�BF�BL�BM�BI�BG�BJ�BP�BR�BR�BQ�BQ�BP�BO�BN�BK�BG�BE�BE�BD�BD�BC�BB�B@�B?}B<jB;dB=VB=VB=qB<jB:DB:DB72B6+B49B49B3B1'B1'B/ B.B/ B-B-B,�B0B4B5?B33B1B1'B:^BBuBI�BE�BA�BA�BAoBA�BA�BA�BAoBAoB@�B?}B@iB?}B?cB>wB;JB9XB72B49B1B.B,B+�B,B*�B)�B)�B(�B'�B)�B-B/B0!B2B72B=VB@�BD�BE�BH�BK�BM�BQ�BR�BW�B\B_;Bc:Be`BffBffBgmBgmBhsBlqBkkBkkBk�BfLBl�Bq�Bq�Bp�Bp�Bl�Bp�Bn�BmwBn}Bs�Bt�Bu�Bv�Bx�By�By�Bx�Bw�Bw�Bx�B|�B~�B�B�%B�B�0B�BB�oB�{B�sB��B��B��B��B��B��B��B��B��B�B�9B�^B��BȴBˬB��B��B��B�B�-B�FB�eB�qB�B�B��B��B��B	B	%B	%B	HB	TB	[B	�B	�B	�B	�B	�B	�B	 �B	"�B	(�B	/ B	2B	4B	8RB	;JB	=qB	?}B	D�B	I�B	O�B	Q�B	T�B	T�B	U�B	ZB	ZB	\B	^5B	`'B	b4B	eFB	gRB	i_B	kkB	n}B	s�B	t�B	t�B	u�B	u�B	v�B	v�B	v�B	v�B	w�B	w�B	y�B	y�B	z�B	}�B	~�B	~�B	�B	��B	��B	�B	�B	�B	�B	�7B	�B	�B	�#B	�)B	�)B	�DB	�0B	�0B	�0B	�0B	�VB	�HB	��B	�mB	�sB	�yB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�-B	�B	�B	�9B	�+B	�2B	�8B	�>B	�XB	�JB	�JB	�PB	�PB	�]B	�}B	�cB	�iB	�uB	ĜB	ĜB	ňB	ňB	ǮB	ǮB	ɺB	��B	��B	ˬB	̳B	��B	��B	��B	οB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�)B	�/B	�B	�B	�B	�!B	�!B	�BB	�HB	�-B	�:B	�ZB	�`B	�`B	�LB	�LB	�LB	�mB	�sB	�XB	�B	�B	�qB	�qB	�qB	�B	�B	�}B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B
B
B
�B
�B
�B
B
B
�B
�B
B
B
B
B
B
%B
B
+B
1B
B
B
1B
	B
	B

#B

#B

#B

#B

#B
DB
)B
)B
DB
0B
0B
6B
6B
PB
<B
<B
<B
BB
BB
BB
bB
HB
HB
NB
NB
NB
oB
oB
TB
[B
[B
uB
uB
[B
gB
�B
�B
mB
mB
�B
�B
sB
yB
�B
B
B
B
�B
�B
�B
�B
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
#�B
#�B
#�B
#�B
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
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
*�B
+�B
,B
+�B
+�B
+�B
-B
-B
,�B
-B
.B
.B
/ B
/B
/ B
/ B
/ B
0B
0!B
0B
1'B
1B
1B
1B
2B
2B
2B
2B
2B
3B
3B
33B
3B
33B
3B
4B
4B
49B
5?B
5?B
5?B
6FB
6+B
6+B
6+B
6+B
7LB
8RB
7LB
72B
88B
88B
8RB
8RB
88B
88B
9XB
9XB
9>B
9XB
9XB
:^B
:^B
:DB
;JB
;JB
;dB
<PB
<PB
<jB
<jB
<PB
<jB
<PB
=qB
=VB
=qB
=VB
=VB
>]B
>]B
>]B
>]B
>wB
>]B
>wB
?}B
?cB
?}B
?}B
@iB
A�B
AoB
AoB
AoB
AoB
AoB
AoB
BuB
B�B
C{B
C{B
C{B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
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
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
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
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
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
Q�B
Q�B
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
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
U�B
U�B
U�B
VB
U�B
U�B
U�B
U�B
VB
W
B
V�B
V�B
V�B
W
B
XB
XB
W�B
W�B
XB
W�B
YB
YB
YB
X�B
ZB
ZB
ZB
ZB
ZB
ZB
[	B
[	B
[	B
[#B
[	B
[#B
[#B
[#B
[	B
\B
\B
\B
\B
\)B
\B
]/B
]/B
]/B
]B
]/B
]/B
]/B
^5B
^5B
^B
^B
^5B
^5B
_!B
_!B
_!B
_!B
_;B
_!B
`BB
`'B
`'B
aHB
a-B
aHB
a-B
a-B
aHB
aHB
aHB
a-B
a-B
b4B
b4B
bNB
b4B
bNB
bNB
bNB
b4B
cTB
c:B
cTB
c:B
d@B
d@B
dZB
e`B
ffB
fLB
ffB
fLB
gmB
gRB
gmB
gRB
gRB
gRB
gmB
gmB
gmB
gmB
gmB
hsB
hXB
hXB
hXB
hsB
hXB
hXB
i_B
iyB
jB
jB
jB
kkB
kkB
kk11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.11(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201904030046102019040300461020190403004610201904040042302019040400423020190404004230JA  ARFMdecpA19c                                                                20190326093650  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190326003707  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190326003709  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190326003709  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190326003710  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190326003710  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190326003710  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190326003710  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190326003710  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190326003710                      G�O�G�O�G�O�                JA  ARUP                                                                        20190326005743                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190326153556  CV  JULD            G�O�G�O�Fŋ�                JM  ARCAJMQC2.0                                                                 20190402154610  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190402154610  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190403154230  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021523                      G�O�G�O�G�O�                