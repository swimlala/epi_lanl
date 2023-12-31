CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-10-10T09:47:41Z creation;2022-10-10T09:47:42Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20221010094741  20221010100112  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @���韫1   @����A@.��t��cU$�/1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B���B���B���B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C�C  C  C
  C  C  C  C  C  C  C  C  C�C  C   C"  C$  C&  C(  C*  C,  C.  C0  C233C4  C5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C]�fC`  Cb  Cd  Cf  Ch  Cj�Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!�fD"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DLfDL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�9�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @G�@�=p@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�B�HBG�B�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B��B��B��B��B��B��B�#�B�#�B��qB��B��>B��>B��qB��B�#�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C�C�C�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC�RC�C�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC2+�C3�RC5޸C7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]޸C_�RCa�RCc�RCe�RCg�RCj�Cl�Cm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!�zD!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DLzDL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�B=D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D��=D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D¿
D��
D�?
D�
Dÿ
D��
D�?
D�
DĿ
D��
D�?
D�
Dſ
D��
D�?
D�
Dƿ
D��
D�?
D�
Dǿ
D��
D�?
D�
Dȿ
D��
D�?
D�
Dɿ
D��
D�?
D�
Dʿ
D��
D�?
D�
D˿
D��
D�?
D�
D̿
D��
D�?
D�
DͿ
D��
D�?
D�
Dο
D��
D�?
D�
DϿ
D��
D�?
D�
Dп
D��
D�?
D�
Dѿ
D��
D�?
D�
Dҿ
D��
D�?
D�
Dӿ
D��
D�?
D�
DԿ
D��
D�?
D�
Dտ
D��
D�?
D�
Dֿ
D��
D�?
D�
D׿
D��
D�?
D�
Dؿ
D��
D�?
D�
Dٿ
D��
D�?
D�
Dڿ
D��
D�?
D�
Dۿ
D��
D�?
D�
Dܿ
D��
D�?
D�
Dݿ
D��
D�?
D�
D޿
D��
D�?
D�
D߿
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D��=D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D��
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D���D��
D�8�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�c A�d�A�g�A�h�A�kQA�n�A�rA�e�A�8�A�xA�A��A��,A��A��;A��QA��&A��?Aڸ�Aڭ�AڦLAڛ�Aڔ�Aڑ4Aڏ(AڒoAڟVAڴnA�A��-Aڽ�AڽqAڻ0Aڻ0Aڮ�AڅA�OA؞OA؉7A��A��mA���AĲ�A�?HA��oA�2�A��bA��mA�MA�ݘA���A���A�H�A�<�A��A���A�%zA�خA��DA���A�(A��A�IA���A��A�s�A���A�N�A�{�A�{A�&�A���A��A�zA�w�A���A��dA��_A�D�A�YA���A��!A}��Az�Awg�Ar� Am��Ak�!Ai�Ah#�Ae*�A`��A^/A[�AX��AU��AS:�AO��ALI�AJ�PAG��AD`�AB%�AA�A?��A>A�A=P�A:�>A8��A5�RA5c A4/�A1�A.��A-�?A-�LA-�A,)�A+�hA)MjA&��A%{JA"خA!S�A!?�A!1'A ZAu%A�AK^A��A��A�A�HA�$AA��A	�A9XA��Ac A��A1A�YA��A�vAA�)A�1Ap;A{�A]dAe�AjA=A=qA�A~A�UASA��AzA}VA�A iA� A�Ap;A$�A�}A}�A�,AI�A��A��Ar�A��A��A��AZ�A��A�A�DA� A	�)A�jA>�A�9A��A�zA@�A��A�AqA��A ��@�s�@��	A 2�@�g8@�v`@���@��>@���@���@�l�@��@�dZ@���@��Q@�i�@�#:@��@�x�@��@��Z@��A@���@�!@�@��@�v`@���@�<@��3@��@��y@쉠@� �@��@�/�@��f@�L0@�˒@�x�@�=@�r�@�dZ@橓@�c@�=@��)@��U@�R@��@��@��@�e�@�5�@�Ĝ@�@�l�@�I�@��@�
=@�\�@��@ߎ�@ޑ @���@�H�@� \@��@܆Y@۬q@���@�n�@��z@�=@��E@�S�@��@ץ�@��@�:�@շ@�Q�@��8@���@�:*@��@Ҷ�@�M@���@��@Ѐ�@��@�U�@΋D@��@�Y�@�Ĝ@�J�@���@˰�@�iD@�ی@�Z�@�'R@ɬq@�o @Ɏ�@�RT@���@�ȴ@���@�d�@��@�E9@���@�C-@��]@Ř�@��@�e@���@�Q�@��@�z@��@���@���@�f�@���@�~(@��&@��P@��@�j@�H@��@���@�]�@���@�e,@��@��5@�ی@��@��6@�~(@�E�@�($@��@��@���@��$@�~�@�!�@�w�@�Q@��@��@���@�Z@��@��.@�%�@��@���@�A @���@�'R@���@���@�F@��@��]@���@��o@���@��c@���@�B[@�"�@���@�g8@�{@��-@�iD@��@�,=@�z@�$�@��@��C@�J#@��@�a@���@���@���@��:@��@��@�tT@�x�@���@���@�[W@��@�-w@�P�@�=�@��@���@��@��@��@��E@���@�+k@��
@�|@��@�Q@��@�H�@��@���@�,=@�@��@�x�@�q@��@�ߤ@���@�d�@�GE@��@���@�t�@��@���@�Ɇ@���@���@�e�@�\�@�H�@�_@���@��@�iD@�F@��@�D�@���@�>�@�q@���@���@���@�~�@�J�@�(�@��@��,@���@��@�R�@�-@��@�  @��@��@�Q�@��@��@���@�kQ@�C-@�e@�~�@�>�@�)_@� i@��,@���@�ff@�1@��>@��@��@���@�%F@��@���@��R@�Ft@��@��m@��@��@��?@���@���@�� @��+@�z�@�PH@�خ@���@�c�@�8�@��`@���@�@�@��@��Q@���@���@�@O@��y@���@�p;@��@��[@�x�@�33@���@���@�:�@�ݘ@���@�)_@�ѷ@���@��@�a|@�8�@�,=@���@���@�l�@��@�Ɇ@��L@�kQ@�%�@��@��@��0@���@�v`@�8�@��@�҉@��z@�Q@��]@��C@�o�@�4@��@���@�L0@�!�@�k@~��@}�@}�n@}�@|�?@|�@{�@z�@zC�@z�@y��@y��@x��@x�@w�+@wdZ@v�8@v�@vq�@v�@u��@uS&@t�@t��@t�@sx@sS@r��@r
�@q[W@p�v@p�@p4n@p�@o��@n�@m�D@m�=@m0�@l�`@l�9@lN�@kt�@j�6@j?@j	@i�@ia�@i+�@h�|@h��@h$@g�g@giD@fߤ@f��@f0U@e`B@eF@e#�@d��@d|�@d]d@dXy@d<�@c�g@cy�@cS�@b��@b�b@ba|@bE�@a�@aw2@a�@`��@`:�@`x@_�*@^�@^�@^$�@]�@]�@\K^@[�k@[e�@[!-@Z�s@Z��@ZTa@Z6�@Y�j@X�$@X$@W�W@W~�@V��@VOv@VJ@U\�@U�@Tb@S��@Sg�@S)_@S�@SS@Rں@Rs�@Q��@Q�@Q�"@Q@P��@P�@P>B@O�[@O@Nu%@N+k@M��@MN<@L�|@L��@L�Y@L4n@K�A@K�F@KP�@J�@J��@JGE@I�#@I�@I�'@I�h@Ij@H��@H:�@H@G� @GdZ@F�y@F��@FE�@E�Z@E%F@DH@Cݘ@Ca@C4�@C�@B�r@A�@A��@A!�@@��@@M@?�]@?�@?�@?E9@>�H@>�\@>{�@>ff@=��@=ϫ@=�'@=|@=Vm@=�@<Ɇ@<r�@<�@;��@;�@:�s@:�m@:��@:!�@9�t@9c@9X@97L@9�@8�j@8@7�q@7]�@6�@6}V@63�@5��@5�@5�'@5O�@50�@5�@4�@4�@4m�@4Xy@44n@47@3��@3��@3@2��@2�}@2
�@1�3@1��@1c�@1O�@1-w@0��@0�v@0��@0��@0��@0]d@0�@/�K@/�k@/qv@/K�@/�@.�m@.��@.�A@.J�@.1�@-�j@-��@-�@-G�@,��@,|�@,1'@+�W@+��@+��@+C@*�@*��@*��@*��@*V@*	@)��@)��@)o @)A @)=�@(�K@(|�@(PH@(b@'�r@'��@'O@'Y@&ں@&��@&�r@&s�@&H�@&�@%�@%�C@%T�@%#�@$��@$��@$�@$�@$�z@$�@$9X@#�]@#�	@#U�@"�M@"��@"�F@"Z�@"=q@"@"J@!�@!rG@!N<@!@@ �@ �e@ :�@�@ƨ@�4@j�@Mj@&@�@�h@��@;�@��@��@S&@��@��@�.@N�@6@�W@��@��@�V@��@K�@�8@�m@�<@�b@u%@?@��@��@�@�H@x�@[W@8�@;@��@�v@��@��@y>@A�@-�@�]@˒@_p@�@�}@v�@Z�@E�@�)@�9@��@}�@5�@��@�@�9@�@c�@b@�Q@�q@O@ߤ@�!@�\@v�@YK@Ov@@�@�@�>@�j@��@��@F@!�@	l@�z@Xy@x@�A@�6@��@t�@F�@S@�H@��@�r@q�@V@Q@$�@�>@��@u�@X@A @=�@&�@ی@��@�@U2@%�@��@�}@�}@��@�	@j�@O@9�@.I@
�8@
�B@
�h@
��@
�x@
h
@
E�@
E�@
5?@
O@	�D@	�n@	w2@	F@	-w@	+@��@ѷ@��@��@u�@g8@M@$@�@b@�A@�
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�c A�d�A�g�A�h�A�kQA�n�A�rA�e�A�8�A�xA�A��A��,A��A��;A��QA��&A��?Aڸ�Aڭ�AڦLAڛ�Aڔ�Aڑ4Aڏ(AڒoAڟVAڴnA�A��-Aڽ�AڽqAڻ0Aڻ0Aڮ�AڅA�OA؞OA؉7A��A��mA���AĲ�A�?HA��oA�2�A��bA��mA�MA�ݘA���A���A�H�A�<�A��A���A�%zA�خA��DA���A�(A��A�IA���A��A�s�A���A�N�A�{�A�{A�&�A���A��A�zA�w�A���A��dA��_A�D�A�YA���A��!A}��Az�Awg�Ar� Am��Ak�!Ai�Ah#�Ae*�A`��A^/A[�AX��AU��AS:�AO��ALI�AJ�PAG��AD`�AB%�AA�A?��A>A�A=P�A:�>A8��A5�RA5c A4/�A1�A.��A-�?A-�LA-�A,)�A+�hA)MjA&��A%{JA"خA!S�A!?�A!1'A ZAu%A�AK^A��A��A�A�HA�$AA��A	�A9XA��Ac A��A1A�YA��A�vAA�)A�1Ap;A{�A]dAe�AjA=A=qA�A~A�UASA��AzA}VA�A iA� A�Ap;A$�A�}A}�A�,AI�A��A��Ar�A��A��A��AZ�A��A�A�DA� A	�)A�jA>�A�9A��A�zA@�A��A�AqA��A ��@�s�@��	A 2�@�g8@�v`@���@��>@���@���@�l�@��@�dZ@���@��Q@�i�@�#:@��@�x�@��@��Z@��A@���@�!@�@��@�v`@���@�<@��3@��@��y@쉠@� �@��@�/�@��f@�L0@�˒@�x�@�=@�r�@�dZ@橓@�c@�=@��)@��U@�R@��@��@��@�e�@�5�@�Ĝ@�@�l�@�I�@��@�
=@�\�@��@ߎ�@ޑ @���@�H�@� \@��@܆Y@۬q@���@�n�@��z@�=@��E@�S�@��@ץ�@��@�:�@շ@�Q�@��8@���@�:*@��@Ҷ�@�M@���@��@Ѐ�@��@�U�@΋D@��@�Y�@�Ĝ@�J�@���@˰�@�iD@�ی@�Z�@�'R@ɬq@�o @Ɏ�@�RT@���@�ȴ@���@�d�@��@�E9@���@�C-@��]@Ř�@��@�e@���@�Q�@��@�z@��@���@���@�f�@���@�~(@��&@��P@��@�j@�H@��@���@�]�@���@�e,@��@��5@�ی@��@��6@�~(@�E�@�($@��@��@���@��$@�~�@�!�@�w�@�Q@��@��@���@�Z@��@��.@�%�@��@���@�A @���@�'R@���@���@�F@��@��]@���@��o@���@��c@���@�B[@�"�@���@�g8@�{@��-@�iD@��@�,=@�z@�$�@��@��C@�J#@��@�a@���@���@���@��:@��@��@�tT@�x�@���@���@�[W@��@�-w@�P�@�=�@��@���@��@��@��@��E@���@�+k@��
@�|@��@�Q@��@�H�@��@���@�,=@�@��@�x�@�q@��@�ߤ@���@�d�@�GE@��@���@�t�@��@���@�Ɇ@���@���@�e�@�\�@�H�@�_@���@��@�iD@�F@��@�D�@���@�>�@�q@���@���@���@�~�@�J�@�(�@��@��,@���@��@�R�@�-@��@�  @��@��@�Q�@��@��@���@�kQ@�C-@�e@�~�@�>�@�)_@� i@��,@���@�ff@�1@��>@��@��@���@�%F@��@���@��R@�Ft@��@��m@��@��@��?@���@���@�� @��+@�z�@�PH@�خ@���@�c�@�8�@��`@���@�@�@��@��Q@���@���@�@O@��y@���@�p;@��@��[@�x�@�33@���@���@�:�@�ݘ@���@�)_@�ѷ@���@��@�a|@�8�@�,=@���@���@�l�@��@�Ɇ@��L@�kQ@�%�@��@��@��0@���@�v`@�8�@��@�҉@��z@�Q@��]@��C@�o�@�4@��@���@�L0@�!�@�k@~��@}�@}�n@}�@|�?@|�@{�@z�@zC�@z�@y��@y��@x��@x�@w�+@wdZ@v�8@v�@vq�@v�@u��@uS&@t�@t��@t�@sx@sS@r��@r
�@q[W@p�v@p�@p4n@p�@o��@n�@m�D@m�=@m0�@l�`@l�9@lN�@kt�@j�6@j?@j	@i�@ia�@i+�@h�|@h��@h$@g�g@giD@fߤ@f��@f0U@e`B@eF@e#�@d��@d|�@d]d@dXy@d<�@c�g@cy�@cS�@b��@b�b@ba|@bE�@a�@aw2@a�@`��@`:�@`x@_�*@^�@^�@^$�@]�@]�@\K^@[�k@[e�@[!-@Z�s@Z��@ZTa@Z6�@Y�j@X�$@X$@W�W@W~�@V��@VOv@VJ@U\�@U�@Tb@S��@Sg�@S)_@S�@SS@Rں@Rs�@Q��@Q�@Q�"@Q@P��@P�@P>B@O�[@O@Nu%@N+k@M��@MN<@L�|@L��@L�Y@L4n@K�A@K�F@KP�@J�@J��@JGE@I�#@I�@I�'@I�h@Ij@H��@H:�@H@G� @GdZ@F�y@F��@FE�@E�Z@E%F@DH@Cݘ@Ca@C4�@C�@B�r@A�@A��@A!�@@��@@M@?�]@?�@?�@?E9@>�H@>�\@>{�@>ff@=��@=ϫ@=�'@=|@=Vm@=�@<Ɇ@<r�@<�@;��@;�@:�s@:�m@:��@:!�@9�t@9c@9X@97L@9�@8�j@8@7�q@7]�@6�@6}V@63�@5��@5�@5�'@5O�@50�@5�@4�@4�@4m�@4Xy@44n@47@3��@3��@3@2��@2�}@2
�@1�3@1��@1c�@1O�@1-w@0��@0�v@0��@0��@0��@0]d@0�@/�K@/�k@/qv@/K�@/�@.�m@.��@.�A@.J�@.1�@-�j@-��@-�@-G�@,��@,|�@,1'@+�W@+��@+��@+C@*�@*��@*��@*��@*V@*	@)��@)��@)o @)A @)=�@(�K@(|�@(PH@(b@'�r@'��@'O@'Y@&ں@&��@&�r@&s�@&H�@&�@%�@%�C@%T�@%#�@$��@$��@$�@$�@$�z@$�@$9X@#�]@#�	@#U�@"�M@"��@"�F@"Z�@"=q@"@"J@!�@!rG@!N<@!@@ �@ �e@ :�@�@ƨ@�4@j�@Mj@&@�@�h@��@;�@��@��@S&@��@��@�.@N�@6@�W@��@��@�V@��@K�@�8@�m@�<@�b@u%@?@��@��@�@�H@x�@[W@8�@;@��@�v@��@��@y>@A�@-�@�]@˒@_p@�@�}@v�@Z�@E�@�)@�9@��@}�@5�@��@�@�9@�@c�@b@�Q@�q@O@ߤ@�!@�\@v�@YK@Ov@@�@�@�>@�j@��@��@F@!�@	l@�z@Xy@x@�A@�6@��@t�@F�@S@�H@��@�r@q�@V@Q@$�@�>@��@u�@X@A @=�@&�@ی@��@�@U2@%�@��@�}@�}@��@�	@j�@O@9�@.I@
�8@
�B@
�h@
��@
�x@
h
@
E�@
E�@
5?@
O@	�D@	�n@	w2@	F@	-w@	+@��@ѷ@��@��@u�@g8@M@$@�@b@�A@�
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�B
�B
�B
aB
SB
5B
B
 B
&�B
*B
+�B
./B
/�B
/�B
0�B
2B
49B
<�B
FB
HKB
J#B
MB
R�B
T{B
U2B
W�B
^�B
h�B
tnB
w�B
�B
��B
�XB
��B
��B
�B
�?B
ϑB
�$B
�"B
�B
�hB
��B
�uB
��BP�B��B�tB�wBgB�B<BzB$B�BBB�B�$B�8BؓB�B��B��B��B��B|Bh�BKDB7�B vB �B
�\B
�B
��B
HB
iB
W�B
PbB
I�B
'�B
(B	��B	�,B	�2B	��B	��B	��B	�;B	u�B	dZB	KDB	<jB	/�B	 BB	�B	9B�zB��B�B�)B�MBΊB��BʦB��B�mB�B��B� B�)B�IB�XB�TB�jB�5B��B��B�TBªBϑB�[B�rB�OB��B�0B�^B��B�LB�;B��BðB��B�\B��B�B	
�B	FB	�B	)B	$�B	9�B	O�B	`�B	nB	}�B	��B	��B	�:B	��B	�B	��B	��B	��B	�GB	��B	��B	��B	�fB	�YB	��B	ɠB	ѷB	��B	�bB	�(B	�dB	ҽB	ңB	�B	��B	��B	�OB	�jB	�aB	��B	��B	�EB	ňB	�BB	ѝB	ѝB	�4B	�hB	�B	B	�(B	�B	��B	��B	�'B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�FB	�\B	�-B	��B	��B	��B	�yB	�B	��B	��B	�`B	�qB	��B	��B	��B	��B	��B	�wB	�BB	��B	��B	�HB	�B	�<B	��B	��B	�B	��B	�B	�-B	āB	ňB	ňB	�?B	�zB	��B	ȴB	�RB	ʦB	�B	�B	�JB	�B	��B	ΊB	�VB	��B	ϑB	�4B	� B	҉B	�{B	��B	�gB	��B	��B	�gB	�B	ՁB	�2B	��B	�aB	ӏB	�oB	��B	ңB	��B	�@B	�uB	��B	�uB	��B	өB	ҽB	�oB	�@B	՛B	՛B	�,B	�B	ԕB	յB	֡B	�$B	��B	�B	�eB	ٚB	�1B	�yB	��B	��B	�KB	ٴB	�WB	��B	��B	��B	��B	�;B	��B	��B	��B	�4B	�B	��B	��B	�
B	�B	�B	�B	��B	�_B	��B	�*B	�DB	�eB	��B	��B	�B	�B	�WB	�)B	�B	�B	��B	�B	�B	�B	��B	�`B	�RB	��B	�rB	��B	��B	�B	��B	�B	�B	��B	��B	�6B	�VB	��B	�VB	��B	�(B	�qB	�B	��B	�6B	�0B	��B	��B	�B	��B	��B	�2B	��B	�`B	��B	��B	��B	�B	��B	��B	��B	�0B	�0B	�^B	��B	�B	�dB	�B	�B	�B	��B	��B
 �B
SB
SB
MB
3B
�B
oB	��B
�B
B
�B
�B
B
�B
B
 �B
�B
YB
?B
fB
	�B
�B
<B
�B
BB
 B
B
�B
�B
:B
oB
oB
oB
[B
�B
�B
2B
B
gB
MB
MB
B
B
SB

B
�B
�B
SB
B
�B
�B
�B
MB
�B
�B
9B
SB
�B
�B
�B
_B
�B
�B
�B
�B
�B
�B
�B
�B
eB
�B
QB
7B
�B
�B
	B
�B
�B
=B
qB
�B
�B
�B
�B
�B
�B
�B
IB
~B
�B
�B
B
B
VB
VB
�B
VB
pB
�B
 B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 vB
!-B
!B
!B
!HB
"hB
"�B
"�B
"�B
"�B
"�B
"�B
# B
#�B
#�B
$@B
$ZB
$�B
$�B
%`B
%,B
%`B
%�B
&2B
&LB
&�B
'B
'B
'mB
'�B
'�B
($B
(sB
(XB
)*B
)*B
)�B
*KB
*�B
*B
*�B
*�B
*�B
+6B
+�B
,qB
,�B
-)B
-�B
-�B
.�B
.�B
.�B
/5B
/�B
/�B
0UB
0UB
0;B
0�B
0�B
1�B
2�B
3B
3�B
3MB
3hB
3�B
4B
4B
4�B
5%B
5�B
5�B
6+B
5�B
6`B
6�B
6�B
7�B
7�B
8B
8RB
9	B
9	B
9rB
9�B
9�B
:B
:*B
:xB
:xB
:�B
:xB
:*B
:�B
;0B
;dB
;�B
;�B
<�B
<PB
<�B
<�B
<�B
<�B
=�B
=�B
>(B
>�B
>�B
>�B
?HB
?�B
@ B
@B
@B
@iB
@iB
@�B
@�B
@�B
A;B
A B
A�B
A�B
A�B
BAB
B�B
B�B
CB
C{B
C�B
C�B
C�B
C�B
DB
D�B
D�B
EmB
E�B
E�B
EmB
E�B
E�B
FYB
FtB
F�B
F�B
F�B
G+B
GEB
G_B
G�B
H1B
H�B
IB
IlB
I�B
I�B
I�B
I�B
I�B
J=B
K^B
KxB
KxB
K�B
L0B
L�B
L�B
MjB
MPB
NVB
NpB
N�B
O\B
O\B
O\B
OvB
O\B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q B
Q�B
Q�B
RB
RoB
R�B
R�B
S&B
S�B
S�B
TB
T{B
T{B
T{B
T�B
U2B
T�B
U2B
UB
U2B
UgB
U�B
U�B
VB
VSB
VSB
VmB
V�B
V�B
W�B
XEB
X+B
XyB
X�B
X�B
YB
Y�B
Y�B
ZB
ZB
Z�B
Z�B
Z�B
Z�B
[	B
[	B
Z�B
Z�B
Z�B
[#B
[	B
[=B
[�B
\)B
\�B
\�B
\�B
\�B
\�B
\�B
]B
]�B
]�B
^B
^�B
^�B
^�B
^�B
^�B
^�B
_!B
^�B
^�B
_pB
_�B
`'B
`\B
`�B
`�B
a-B
aHB
abB
a|B
a�B
a�B
b4B
b4B
bNB
bNB
b�B
cTB
c:B
c�B
dB
dB
dtB
dtB
d�B
d�B
d�B
d�B
eB
d�B
e`B
e�B
e�B
e�B
f�B
f�B
gB
gRB
g�B
g�B
g�B
h
B
h>B
h�B
h�B
h�B
h�B
iyB
i�B
i�B
jB
jB
jB
j�B
kB
kB
kQB
kQB
kkB
k�B
lB
lB
l"B
lqB
l=B
l�B
mB
m)B
mCB
mCB
m�B
m�B
m�B
nIB
ncB
n}B
n}B
n�B
n�B
n�B
o5B
oiB
o�B
o�B
o�B
pB
o�B
pB
p;B
pUB
pUB
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r-B
r-B
r|B
r�B
r�B
r�B
r�B
sB
s�B
s�B
s�B
tB
t9B
tTB
tTB
t�B
t�B
t�B
u%B
u?B
u�B
u�B
vB
vB
vFB
v�B
v�B
v�B
wB
wB
v�B
v�B
wfB
w�B
w�B
w�B
w�B
xB
xRB
x�B
x�B
x�B
x�B
y	B
y$B
yXB
yrB
yrB
y�B
y�B
y�B
y�B
z*B
z*B
zDB
z^B
z�B
{0B
{dB
{B
{�B
{�B
|B
|B
|PB
|jB
|�B
|�B
}B
}B
}<B
}<B
}�B
}�B
}�B
~]B
~�B
~�B
B
B
HB
HB
HB
}B
�B
�B
�B
� B
�OB
�OB
�OB
��B
� B
�oB
��B
��B
��B
��B
�B
�[B
�[B
��B
��B
��B
��B
��B
�-B
�aB
��B
��B
��B
��B
��B
��B
�MB
��B
��B
��B
��B
�B
�SB
�9B
�SB
��B
��B
��B
��B
��B
�?B
�tB
��B
�tB
��B
��B
��B
��B
�B
��B
�B
��B
��B
��B
��B
��B
�1B
�KB
��B
��B
��B
��B
��B
�B
�7B
�B
�RB
�R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�B
�B
�B
aB
SB
5B
B
 B
&�B
*B
+�B
./B
/�B
/�B
0�B
2B
49B
<�B
FB
HKB
J#B
MB
R�B
T{B
U2B
W�B
^�B
h�B
tnB
w�B
�B
��B
�XB
��B
��B
�B
�?B
ϑB
�$B
�"B
�B
�hB
��B
�uB
��BP�B��B�tB�wBgB�B<BzB$B�BBB�B�$B�8BؓB�B��B��B��B��B|Bh�BKDB7�B vB �B
�\B
�B
��B
HB
iB
W�B
PbB
I�B
'�B
(B	��B	�,B	�2B	��B	��B	��B	�;B	u�B	dZB	KDB	<jB	/�B	 BB	�B	9B�zB��B�B�)B�MBΊB��BʦB��B�mB�B��B� B�)B�IB�XB�TB�jB�5B��B��B�TBªBϑB�[B�rB�OB��B�0B�^B��B�LB�;B��BðB��B�\B��B�B	
�B	FB	�B	)B	$�B	9�B	O�B	`�B	nB	}�B	��B	��B	�:B	��B	�B	��B	��B	��B	�GB	��B	��B	��B	�fB	�YB	��B	ɠB	ѷB	��B	�bB	�(B	�dB	ҽB	ңB	�B	��B	��B	�OB	�jB	�aB	��B	��B	�EB	ňB	�BB	ѝB	ѝB	�4B	�hB	�B	B	�(B	�B	��B	��B	�'B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�FB	�\B	�-B	��B	��B	��B	�yB	�B	��B	��B	�`B	�qB	��B	��B	��B	��B	��B	�wB	�BB	��B	��B	�HB	�B	�<B	��B	��B	�B	��B	�B	�-B	āB	ňB	ňB	�?B	�zB	��B	ȴB	�RB	ʦB	�B	�B	�JB	�B	��B	ΊB	�VB	��B	ϑB	�4B	� B	҉B	�{B	��B	�gB	��B	��B	�gB	�B	ՁB	�2B	��B	�aB	ӏB	�oB	��B	ңB	��B	�@B	�uB	��B	�uB	��B	өB	ҽB	�oB	�@B	՛B	՛B	�,B	�B	ԕB	յB	֡B	�$B	��B	�B	�eB	ٚB	�1B	�yB	��B	��B	�KB	ٴB	�WB	��B	��B	��B	��B	�;B	��B	��B	��B	�4B	�B	��B	��B	�
B	�B	�B	�B	��B	�_B	��B	�*B	�DB	�eB	��B	��B	�B	�B	�WB	�)B	�B	�B	��B	�B	�B	�B	��B	�`B	�RB	��B	�rB	��B	��B	�B	��B	�B	�B	��B	��B	�6B	�VB	��B	�VB	��B	�(B	�qB	�B	��B	�6B	�0B	��B	��B	�B	��B	��B	�2B	��B	�`B	��B	��B	��B	�B	��B	��B	��B	�0B	�0B	�^B	��B	�B	�dB	�B	�B	�B	��B	��B
 �B
SB
SB
MB
3B
�B
oB	��B
�B
B
�B
�B
B
�B
B
 �B
�B
YB
?B
fB
	�B
�B
<B
�B
BB
 B
B
�B
�B
:B
oB
oB
oB
[B
�B
�B
2B
B
gB
MB
MB
B
B
SB

B
�B
�B
SB
B
�B
�B
�B
MB
�B
�B
9B
SB
�B
�B
�B
_B
�B
�B
�B
�B
�B
�B
�B
�B
eB
�B
QB
7B
�B
�B
	B
�B
�B
=B
qB
�B
�B
�B
�B
�B
�B
�B
IB
~B
�B
�B
B
B
VB
VB
�B
VB
pB
�B
 B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 vB
!-B
!B
!B
!HB
"hB
"�B
"�B
"�B
"�B
"�B
"�B
# B
#�B
#�B
$@B
$ZB
$�B
$�B
%`B
%,B
%`B
%�B
&2B
&LB
&�B
'B
'B
'mB
'�B
'�B
($B
(sB
(XB
)*B
)*B
)�B
*KB
*�B
*B
*�B
*�B
*�B
+6B
+�B
,qB
,�B
-)B
-�B
-�B
.�B
.�B
.�B
/5B
/�B
/�B
0UB
0UB
0;B
0�B
0�B
1�B
2�B
3B
3�B
3MB
3hB
3�B
4B
4B
4�B
5%B
5�B
5�B
6+B
5�B
6`B
6�B
6�B
7�B
7�B
8B
8RB
9	B
9	B
9rB
9�B
9�B
:B
:*B
:xB
:xB
:�B
:xB
:*B
:�B
;0B
;dB
;�B
;�B
<�B
<PB
<�B
<�B
<�B
<�B
=�B
=�B
>(B
>�B
>�B
>�B
?HB
?�B
@ B
@B
@B
@iB
@iB
@�B
@�B
@�B
A;B
A B
A�B
A�B
A�B
BAB
B�B
B�B
CB
C{B
C�B
C�B
C�B
C�B
DB
D�B
D�B
EmB
E�B
E�B
EmB
E�B
E�B
FYB
FtB
F�B
F�B
F�B
G+B
GEB
G_B
G�B
H1B
H�B
IB
IlB
I�B
I�B
I�B
I�B
I�B
J=B
K^B
KxB
KxB
K�B
L0B
L�B
L�B
MjB
MPB
NVB
NpB
N�B
O\B
O\B
O\B
OvB
O\B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q B
Q�B
Q�B
RB
RoB
R�B
R�B
S&B
S�B
S�B
TB
T{B
T{B
T{B
T�B
U2B
T�B
U2B
UB
U2B
UgB
U�B
U�B
VB
VSB
VSB
VmB
V�B
V�B
W�B
XEB
X+B
XyB
X�B
X�B
YB
Y�B
Y�B
ZB
ZB
Z�B
Z�B
Z�B
Z�B
[	B
[	B
Z�B
Z�B
Z�B
[#B
[	B
[=B
[�B
\)B
\�B
\�B
\�B
\�B
\�B
\�B
]B
]�B
]�B
^B
^�B
^�B
^�B
^�B
^�B
^�B
_!B
^�B
^�B
_pB
_�B
`'B
`\B
`�B
`�B
a-B
aHB
abB
a|B
a�B
a�B
b4B
b4B
bNB
bNB
b�B
cTB
c:B
c�B
dB
dB
dtB
dtB
d�B
d�B
d�B
d�B
eB
d�B
e`B
e�B
e�B
e�B
f�B
f�B
gB
gRB
g�B
g�B
g�B
h
B
h>B
h�B
h�B
h�B
h�B
iyB
i�B
i�B
jB
jB
jB
j�B
kB
kB
kQB
kQB
kkB
k�B
lB
lB
l"B
lqB
l=B
l�B
mB
m)B
mCB
mCB
m�B
m�B
m�B
nIB
ncB
n}B
n}B
n�B
n�B
n�B
o5B
oiB
o�B
o�B
o�B
pB
o�B
pB
p;B
pUB
pUB
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r-B
r-B
r|B
r�B
r�B
r�B
r�B
sB
s�B
s�B
s�B
tB
t9B
tTB
tTB
t�B
t�B
t�B
u%B
u?B
u�B
u�B
vB
vB
vFB
v�B
v�B
v�B
wB
wB
v�B
v�B
wfB
w�B
w�B
w�B
w�B
xB
xRB
x�B
x�B
x�B
x�B
y	B
y$B
yXB
yrB
yrB
y�B
y�B
y�B
y�B
z*B
z*B
zDB
z^B
z�B
{0B
{dB
{B
{�B
{�B
|B
|B
|PB
|jB
|�B
|�B
}B
}B
}<B
}<B
}�B
}�B
}�B
~]B
~�B
~�B
B
B
HB
HB
HB
}B
�B
�B
�B
� B
�OB
�OB
�OB
��B
� B
�oB
��B
��B
��B
��B
�B
�[B
�[B
��B
��B
��B
��B
��B
�-B
�aB
��B
��B
��B
��B
��B
��B
�MB
��B
��B
��B
��B
�B
�SB
�9B
�SB
��B
��B
��B
��B
��B
�?B
�tB
��B
�tB
��B
��B
��B
��B
�B
��B
�B
��B
��B
��B
��B
��B
�1B
�KB
��B
��B
��B
��B
��B
�B
�7B
�B
�RB
�R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20221010094740  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20221010094741  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221010094742  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221010094742                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221010184747  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221010184747  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20221010100112                      G�O�G�O�G�O�                