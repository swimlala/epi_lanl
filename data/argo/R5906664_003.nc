CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  h   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-06-08T12:00:46Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 h  G   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Jx   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 h  X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  [�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 h  v�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 h  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 h  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 h  �x   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  Ȁ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    Ȱ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ˰   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ΰ   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  Ѱ   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �T   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �\Argo profile    3.1 1.2 19500101000000  20210608120046  20210608120046  5906664 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  8759                            2B  A   NAVIS_A                         1287                            170425                          863 @�z�[�yF1   @�z��	��@1�+I��d�vȴ91   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         A   A   A   @���@�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffBߙ�B�  B�  B�  B�33B���B�  B�  C   C�fC�fC  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ�CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�3D�)�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�Q�@ÅAA!A@(�AaA��HA��HA��HA��HA��HA��HA��HA��HB p�Bp�Bp�Bp�B p�B(p�B0p�B8p�B@p�BHp�BPp�BXp�B`p�Bhp�Bpp�Bxp�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�k�Bܞ�B���B�8RB�8RB�8RB�k�B�B�8RB�8RC )C�C�C)C)C
)C)C)C)C)C)C)C)C)C)C)C )C")C$)C&)C()C*)C,)C.)C0�C2)C4)C6)C8)C:)C<)C>)C@)CB)CD)CF)CH)CJ5�CL)CN)CP)CR)CT)CV)CX)CZ)C\)C^)C`)Cb)Cd)Cf)Ch)Cj)Cl)Cn)Cp)Cr)Ct)Cv)Cx)Cz)C|)C~)C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D 
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
DpD�
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
D��D�C�D���D�ƸD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D��RD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�@RD���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D���D�ÅD��D�C�D�D�ÅD��D�C�DÃ�D�ÅD��D�C�Dă�D�ÅD��D�C�DŃ�D�ÅD��D�C�Dƃ�D�ÅD��D�C�Dǃ�D�ÅD��D�C�Dȃ�D�ÅD��D�C�DɃ�D�ÅD��D�C�Dʃ�D�ÅD��D�C�D˃�D�ÅD��D�C�D̃�D�ÅD��D�C�D̓�D�ÅD��D�C�D΃�D�ÅD��D�C�Dσ�D�ÅD��D�C�DЃ�D�ÅD��D�C�Dу�D�ÅD��D�C�D҃�D�ÅD��D�C�DӃ�D�ÅD��D�C�Dԃ�D�ÅD��D�C�DՃ�D�ÅD��D�C�Dփ�D�ÅD��D�C�D׃�D�ÅD��D�C�D؃�D�ÅD��D�C�Dك�D�ÅD��D�-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�oA�&�A�-A�-A�+A��A��A���A���A�ĜA�AռjAոRAծAա�A՛�AՑhA�r�A�jA�hsA�hsA�C�AԾwA�^5A��/AӮAӓuA�l�A�\)A�9XA�  A���A�ffA�{A��Aя\Aї�AуA�VAд9A�/A� �A�JA���A��;AϺ^A�`BA�^5A͑hA��A�jA�ffA���A��A���A��A��`A�ZA�S�A�$�A�{A�A��FA�t�A���A��hA�%A�JA���A�33A��A���A��A��TA��FA��AVAv�`AqƨAnI�Al��Ah�/Ad��Aa��A`(�A_�#A_��A_|�A\��AZE�AX�\AV�ARz�AQoAP1AL�!AK�AK|�AK7LAK&�AJ�`AJ��AJE�AI��AHn�AB^5A>�RA<��A:��A9��A8�A89XA7�A5��A5A3��A1�FA/�mA-��A*�/A&��A#��A �`AA9XA��A|�A33A�A�!AZA��A��AO�A\)AbNA�
A?}A�\A33AZAl�Ar�A�TAdZA�!A�mA\)A�A��AE�AƨAQ�Al�A
bNA	��A^5AC�A�A�A~�AbAoAZA1AA�AĜA��A��A��A�\Az�AQ�A�TA�A ��A z�A {@�{@�M�@��y@�-@�7L@��j@�@���@�!@���@�@�M�@�V@�ff@�
=@�t�@���@���@��`@��`@��@��/@�Q�@�\)@�;d@���@��h@��/@�w@�;d@��@��
@�@�/@�ƨ@�w@@�t�@�;d@�~�@�7@�7L@��/@�Z@��;@�S�@���@�v�@�=q@��@��@��@�9@�j@睲@��@�-@��T@�O�@���@䛦@㕁@◍@�/@���@���@��@�b@�@ާ�@�v�@�V@��@���@�x�@۶F@�=q@�-@��@��#@���@�x�@�7L@��`@���@؃@�z�@�r�@�I�@�b@��@�t�@�@և+@�M�@�{@��@�  @�K�@�
=@��H@ҧ�@�=q@��#@Ѻ^@�hs@���@�1@ϝ�@���@�J@�@�`B@�Ĝ@��@ˮ@�o@��@��@ʰ!@�^5@��T@���@ɲ-@Ɂ@�z�@�@�=q@��@�hs@���@�~�@��@��T@���@��^@���@�hs@��@��@�9X@�l�@�S�@�K�@�;d@�+@�@�$�@�@�{@�J@�hs@�V@�Q�@�Z@�z�@� �@���@�K�@�M�@�$�@��-@��@�X@�/@��@�Ĝ@�j@�A�@� �@��@��F@��@���@��\@�~�@��@���@�G�@�r�@�1@�ƨ@��@���@��P@�t�@�K�@�"�@��R@��^@���@�p�@�7L@�Ĝ@�z�@� �@��F@���@�|�@��@�ȴ@��!@�~�@��@��^@�x�@��@���@�Ĝ@�r�@�A�@�1'@��;@��P@�"�@�~�@�J@��^@�7L@�Ĝ@�j@��
@��@��@�n�@��@���@��@�p�@�?}@�%@�Ĝ@���@��u@��D@�z�@��@�dZ@�+@�@���@�^5@��@���@�O�@��@��@��@��/@���@���@�/@�Q�@�  @��;@��w@�33@��@��R@�~�@��#@�p�@��@���@�Ĝ@�1'@��w@�|�@�@�=q@���@���@�`B@�X@�?}@���@��@��D@�j@�b@��m@��@�C�@�o@�@��@���@���@��\@�v�@�5?@��T@��h@�X@�/@��@��u@�j@�Q�@�1'@�1@��;@���@�t�@�C�@�;d@�;d@�;d@�33@��H@�n�@�5?@��@���@�?}@���@���@�Ĝ@��@���@��u@��u@��@�r�@�Z@���@��@��@���@�n�@�E�@���@���@���@��@���@�j@�A�@�(�@�ƨ@���@�t�@�"�@��y@��@���@���@�n�@��@��@���@�?}@��@�%@���@��@��/@���@��j@���@��@�z�@�bN@�bN@�Q�@� �@���@��;@���@��P@���@��\@�v�@�^5@�M�@�5?@��@�@��@���@�@���@���@��7@�p�@�X@�&�@�%@���@��`@��j@�z�@�I�@��@�@��+@�v�@�n�@�^5@�5?@��T@��7@�G�@��@�V@���@��`@��/@���@���@���@�bN@�(�@�@+@~{@}�h@}/@}�@|�/@|�@|z�@|Z@{�
@{dZ@z�!@y�#@y&�@x�u@w�@w��@w�P@wl�@v�R@u�@uV@t��@t�D@t9X@s�@r�\@r-@q�@q�7@q&�@p��@p��@pbN@pA�@o�@ol�@oK�@oK�@o\)@o+@n��@n�@nff@n$�@n{@m�@l�@l�/@l�D@k�m@k"�@j�\@j^5@i�#@i&�@h�9@h��@h�u@h�u@h �@g�@g�P@gK�@f��@fv�@f5?@e�T@eO�@d�@d�D@c�F@ct�@ct�@cdZ@c33@c"�@b��@b-@a�@`  @_�w@_|�@_;d@_+@^�@^��@^$�@]��@]O�@]V@\��@\��@\��@\��@\j@[�m@[S�@["�@Z��@Z=q@Yhs@Y&�@X��@XbN@W�;@W|�@W+@V5?@U�@U��@U�-@Up�@T��@T�@T�j@T��@Tz�@TZ@T9X@Sƨ@SC�@S@R��@R~�@Rn�@R=q@Q��@Q�^@Qx�@QX@QG�@Q7L@Q�@P�9@O�@OK�@N�@Nv�@M�h@MO�@L�@L�@Lj@K��@K@J~�@JJ@JJ@I��@I��@I��@I�@HĜ@H�u@HbN@H �@G�w@GK�@F�@Fff@F@E�@E�h@EO�@D�@D�D@D1@Cƨ@C�@CC�@B�@B��@B��@Bn�@BM�@A��@A��@AX@@Ĝ@@bN@?;d@>�@>�R@>��@>5?@=�@=?}@=�@<��@<��@<�D@;�F@;33@:��@:=q@8��@8�@8bN@8r�@8 �@7;d@6��@6ff@6$�@6{@6{@5�T@5��@5`B@5/@4��@4�D@4z�@4j@49X@41@3�F@3��@3�@3dZ@3S�@3"�@2�H@2��@2�\@2~�@2=q@2�@2J@1�7@1%@0��@0�9@0�u@0bN@01'@/��@/+@/
=@.�y@.ȴ@.��@.�+@.V@-�@-?}@,(�@+�@+S�@+"�@*�H@)��@)hs@)X@)G�@)7L@)&�@)�@)%@(��@(�@'��@'l�@';d@'+@'�@&�@&�R@&��@&�+@&V@&5?@&{@&{@&{@&{@&@&{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�oA�&�A�-A�-A�+A��A��A���A���A�ĜA�AռjAոRAծAա�A՛�AՑhA�r�A�jA�hsA�hsA�C�AԾwA�^5A��/AӮAӓuA�l�A�\)A�9XA�  A���A�ffA�{A��Aя\Aї�AуA�VAд9A�/A� �A�JA���A��;AϺ^A�`BA�^5A͑hA��A�jA�ffA���A��A���A��A��`A�ZA�S�A�$�A�{A�A��FA�t�A���A��hA�%A�JA���A�33A��A���A��A��TA��FA��AVAv�`AqƨAnI�Al��Ah�/Ad��Aa��A`(�A_�#A_��A_|�A\��AZE�AX�\AV�ARz�AQoAP1AL�!AK�AK|�AK7LAK&�AJ�`AJ��AJE�AI��AHn�AB^5A>�RA<��A:��A9��A8�A89XA7�A5��A5A3��A1�FA/�mA-��A*�/A&��A#��A �`AA9XA��A|�A33A�A�!AZA��A��AO�A\)AbNA�
A?}A�\A33AZAl�Ar�A�TAdZA�!A�mA\)A�A��AE�AƨAQ�Al�A
bNA	��A^5AC�A�A�A~�AbAoAZA1AA�AĜA��A��A��A�\Az�AQ�A�TA�A ��A z�A {@�{@�M�@��y@�-@�7L@��j@�@���@�!@���@�@�M�@�V@�ff@�
=@�t�@���@���@��`@��`@��@��/@�Q�@�\)@�;d@���@��h@��/@�w@�;d@��@��
@�@�/@�ƨ@�w@@�t�@�;d@�~�@�7@�7L@��/@�Z@��;@�S�@���@�v�@�=q@��@��@��@�9@�j@睲@��@�-@��T@�O�@���@䛦@㕁@◍@�/@���@���@��@�b@�@ާ�@�v�@�V@��@���@�x�@۶F@�=q@�-@��@��#@���@�x�@�7L@��`@���@؃@�z�@�r�@�I�@�b@��@�t�@�@և+@�M�@�{@��@�  @�K�@�
=@��H@ҧ�@�=q@��#@Ѻ^@�hs@���@�1@ϝ�@���@�J@�@�`B@�Ĝ@��@ˮ@�o@��@��@ʰ!@�^5@��T@���@ɲ-@Ɂ@�z�@�@�=q@��@�hs@���@�~�@��@��T@���@��^@���@�hs@��@��@�9X@�l�@�S�@�K�@�;d@�+@�@�$�@�@�{@�J@�hs@�V@�Q�@�Z@�z�@� �@���@�K�@�M�@�$�@��-@��@�X@�/@��@�Ĝ@�j@�A�@� �@��@��F@��@���@��\@�~�@��@���@�G�@�r�@�1@�ƨ@��@���@��P@�t�@�K�@�"�@��R@��^@���@�p�@�7L@�Ĝ@�z�@� �@��F@���@�|�@��@�ȴ@��!@�~�@��@��^@�x�@��@���@�Ĝ@�r�@�A�@�1'@��;@��P@�"�@�~�@�J@��^@�7L@�Ĝ@�j@��
@��@��@�n�@��@���@��@�p�@�?}@�%@�Ĝ@���@��u@��D@�z�@��@�dZ@�+@�@���@�^5@��@���@�O�@��@��@��@��/@���@���@�/@�Q�@�  @��;@��w@�33@��@��R@�~�@��#@�p�@��@���@�Ĝ@�1'@��w@�|�@�@�=q@���@���@�`B@�X@�?}@���@��@��D@�j@�b@��m@��@�C�@�o@�@��@���@���@��\@�v�@�5?@��T@��h@�X@�/@��@��u@�j@�Q�@�1'@�1@��;@���@�t�@�C�@�;d@�;d@�;d@�33@��H@�n�@�5?@��@���@�?}@���@���@�Ĝ@��@���@��u@��u@��@�r�@�Z@���@��@��@���@�n�@�E�@���@���@���@��@���@�j@�A�@�(�@�ƨ@���@�t�@�"�@��y@��@���@���@�n�@��@��@���@�?}@��@�%@���@��@��/@���@��j@���@��@�z�@�bN@�bN@�Q�@� �@���@��;@���@��P@���@��\@�v�@�^5@�M�@�5?@��@�@��@���@�@���@���@��7@�p�@�X@�&�@�%@���@��`@��j@�z�@�I�@��@�@��+@�v�@�n�@�^5@�5?@��T@��7@�G�@��@�V@���@��`@��/@���@���@���@�bN@�(�@�@+@~{@}�h@}/@}�@|�/@|�@|z�@|Z@{�
@{dZ@z�!@y�#@y&�@x�u@w�@w��@w�P@wl�@v�R@u�@uV@t��@t�D@t9X@s�@r�\@r-@q�@q�7@q&�@p��@p��@pbN@pA�@o�@ol�@oK�@oK�@o\)@o+@n��@n�@nff@n$�@n{@m�@l�@l�/@l�D@k�m@k"�@j�\@j^5@i�#@i&�@h�9@h��@h�u@h�u@h �@g�@g�P@gK�@f��@fv�@f5?@e�T@eO�@d�@d�D@c�F@ct�@ct�@cdZ@c33@c"�@b��@b-@a�@`  @_�w@_|�@_;d@_+@^�@^��@^$�@]��@]O�@]V@\��@\��@\��@\��@\j@[�m@[S�@["�@Z��@Z=q@Yhs@Y&�@X��@XbN@W�;@W|�@W+@V5?@U�@U��@U�-@Up�@T��@T�@T�j@T��@Tz�@TZ@T9X@Sƨ@SC�@S@R��@R~�@Rn�@R=q@Q��@Q�^@Qx�@QX@QG�@Q7L@Q�@P�9@O�@OK�@N�@Nv�@M�h@MO�@L�@L�@Lj@K��@K@J~�@JJ@JJ@I��@I��@I��@I�@HĜ@H�u@HbN@H �@G�w@GK�@F�@Fff@F@E�@E�h@EO�@D�@D�D@D1@Cƨ@C�@CC�@B�@B��@B��@Bn�@BM�@A��@A��@AX@@Ĝ@@bN@?;d@>�@>�R@>��@>5?@=�@=?}@=�@<��@<��@<�D@;�F@;33@:��@:=q@8��@8�@8bN@8r�@8 �@7;d@6��@6ff@6$�@6{@6{@5�T@5��@5`B@5/@4��@4�D@4z�@4j@49X@41@3�F@3��@3�@3dZ@3S�@3"�@2�H@2��@2�\@2~�@2=q@2�@2J@1�7@1%@0��@0�9@0�u@0bN@01'@/��@/+@/
=@.�y@.ȴ@.��@.�+@.V@-�@-?}@,(�@+�@+S�@+"�@*�H@)��@)hs@)X@)G�@)7L@)&�@)�@)%@(��@(�@'��@'l�@';d@'+@'�@&�@&�R@&��@&�+@&V@&5?@&{@&{@&{@&{@&@&{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	%�B	$�B	$�B	$�B	%�B	&�B	&�B	(�B	(�B	(�B	)�B	)�B	)�B	)�B	)�B	+B	,B	/B	0!B	0!B	0!B	.B	'�B	#�B	�B	�B	�B	�B	�B	�B	uB	oB	oB	�B	�B	+B	7LB	B�B	N�B	B�B	6FB	8RB	6FB	33B	0!B	2-B	1'B	�B	VB��B��B	B	DB	 �B	'�B	n�B	�JB	��B	��B	�-B	��B	��B	�
B	�
B	��B
�B
�B
�B
�B
�B
�B
{B
uB
oB	�B	��B	�dB	��B	��B	�%B	� B	t�B	cTB	]/B	VB	S�B	Q�B	O�B	N�B	F�B	C�B	A�B	A�B	=qB	?}B	B�B	?}B	@�B	?}B	?}B	?}B	@�B	@�B	@�B	@�B	N�B	R�B	W
B	[#B	\)B	^5B	^5B	^5B	gmB	l�B	q�B	hsB	W
B	G�B	(�B��B�
BĜB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�'B�-B�-B�?B�RB�qBÖBŢBƨBǮB��B��B��B��BɺB��B��B��B��B��B��B��B��B��B��BɺBȴBǮBǮBȴBǮBǮBȴB��B��B��B�B�B�B�B��B�B�/B�;B�`B�B�B��B��B	�B	8RB	F�B	K�B	O�B	S�B	O�B	O�B	XB	_;B	dZB	gmB	iyB	iyB	l�B	p�B	p�B	s�B	v�B	~�B	�7B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�'B	�-B	�-B	�-B	�-B	�-B	�-B	�-B	�3B	�-B	�-B	�3B	�3B	�9B	�9B	�?B	�FB	�FB	�RB	�^B	�dB	�^B	�dB	�^B	�dB	�jB	�jB	�jB	�jB	�jB	�qB	�jB	��B	��B	��B	B	B	��B	B	ÖB	ÖB	B	ÖB	ÖB	ÖB	ÖB	ÖB	ÖB	ĜB	ŢB	ƨB	ŢB	ƨB	ǮB	ȴB	ɺB	ɺB	ɺB	ȴB	ɺB	ɺB	ɺB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ȴB	ŢB	ÖB	B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	B	ÖB	ÖB	B	B	B	ÖB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	�B	�B	�B	�
B	��B	��B	�B	�B	�
B	�
B	�B	�B	�/B	�/B	�/B	�/B	�5B	�/B	�#B	�#B	�#B	�)B	�5B	�BB	�NB	�TB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�fB	�mB	�mB	�mB	�sB	�sB	�sB	�sB	�sB	�yB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
%B
B
%B
%B
+B
1B
	7B

=B

=B
DB

=B
DB
DB
DB
DB
JB
JB
JB
JB
PB
JB
PB
PB
VB
PB
PB
PB
VB
VB
VB
\B
bB
bB
bB
bB
hB
hB
hB
hB
hB
oB
oB
oB
oB
uB
oB
uB
oB
uB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
#�B
$�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
%�B
%�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
,B
,B
,B
,B
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
/B
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
1'B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
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
49B
5?B
6FB
6FB
6FB
5?B
5?B
7LB
7LB
7LB
7LB
7LB
7LB
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
:^B
:^B
:^B
:^B
:^B
:^B
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
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
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
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
D�B
D�B
E�B
E�B
E�B
E�B
E�B
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
G�B
G�B
H�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
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
R�B
R�B
R�B
R�B
R�B
R�B
R�B
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
S�B
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
XB
XB
XB
XB
XB
YB
YB
YB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
`BB
aHB
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
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	%�B	$�B	$�B	$�B	%�B	&�B	&�B	(�B	(�B	(�B	)�B	)�B	)�B	)�B	)�B	+B	,B	/B	0!B	0!B	0!B	.B	'�B	#�B	�B	�B	�B	�B	�B	�B	uB	oB	oB	�B	�B	+B	7LB	B�B	N�B	B�B	6FB	8RB	6FB	33B	0!B	2-B	1'B	�B	VB��B��B	B	DB	 �B	'�B	n�B	�JB	��B	��B	�-B	��B	��B	�
B	�
B	��B
�B
�B
�B
�B
�B
�B
{B
uB
oB	�B	��B	�dB	��B	��B	�%B	� B	t�B	cTB	]/B	VB	S�B	Q�B	O�B	N�B	F�B	C�B	A�B	A�B	=qB	?}B	B�B	?}B	@�B	?}B	?}B	?}B	@�B	@�B	@�B	@�B	N�B	R�B	W
B	[#B	\)B	^5B	^5B	^5B	gmB	l�B	q�B	hsB	W
B	G�B	(�B��B�
BĜB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�'B�-B�-B�?B�RB�qBÖBŢBƨBǮB��B��B��B��BɺB��B��B��B��B��B��B��B��B��B��BɺBȴBǮBǮBȴBǮBǮBȴB��B��B��B�B�B�B�B��B�B�/B�;B�`B�B�B��B��B	�B	8RB	F�B	K�B	O�B	S�B	O�B	O�B	XB	_;B	dZB	gmB	iyB	iyB	l�B	p�B	p�B	s�B	v�B	~�B	�7B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�'B	�-B	�-B	�-B	�-B	�-B	�-B	�-B	�3B	�-B	�-B	�3B	�3B	�9B	�9B	�?B	�FB	�FB	�RB	�^B	�dB	�^B	�dB	�^B	�dB	�jB	�jB	�jB	�jB	�jB	�qB	�jB	��B	��B	��B	B	B	��B	B	ÖB	ÖB	B	ÖB	ÖB	ÖB	ÖB	ÖB	ÖB	ĜB	ŢB	ƨB	ŢB	ƨB	ǮB	ȴB	ɺB	ɺB	ɺB	ȴB	ɺB	ɺB	ɺB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ȴB	ŢB	ÖB	B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	B	ÖB	ÖB	B	B	B	ÖB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	�B	�B	�B	�
B	��B	��B	�B	�B	�
B	�
B	�B	�B	�/B	�/B	�/B	�/B	�5B	�/B	�#B	�#B	�#B	�)B	�5B	�BB	�NB	�TB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�fB	�mB	�mB	�mB	�sB	�sB	�sB	�sB	�sB	�yB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
%B
B
%B
%B
+B
1B
	7B

=B

=B
DB

=B
DB
DB
DB
DB
JB
JB
JB
JB
PB
JB
PB
PB
VB
PB
PB
PB
VB
VB
VB
\B
bB
bB
bB
bB
hB
hB
hB
hB
hB
oB
oB
oB
oB
uB
oB
uB
oB
uB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
#�B
$�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
%�B
%�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
,B
,B
,B
,B
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
/B
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
1'B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
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
49B
5?B
6FB
6FB
6FB
5?B
5?B
7LB
7LB
7LB
7LB
7LB
7LB
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
:^B
:^B
:^B
:^B
:^B
:^B
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
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
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
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
D�B
D�B
E�B
E�B
E�B
E�B
E�B
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
G�B
G�B
H�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
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
R�B
R�B
R�B
R�B
R�B
R�B
R�B
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
S�B
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
XB
XB
XB
XB
XB
YB
YB
YB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
`BB
aHB
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
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.11 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20210608120046                              AO  ARCAADJP                                                                    20210608120046    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210608120046  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210608120046  QCF$                G�O�G�O�G�O�0               