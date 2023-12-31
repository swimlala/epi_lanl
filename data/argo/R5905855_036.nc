CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:16:52Z creation;2022-06-04T19:16:53Z conversion to V3.1      
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191652  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               $A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @���Z��91   @��赪I�@0�$�/��c�"��`B1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  Bؙ�Bۙ�B���B�  B�  B�  B�B�ffB���B���C   C�fC  C  C  C
  C  C  C  C  C  C�C�C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL33CN�CO�fCQ�fCT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cy�fC|  C}�fC�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D�fD�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@~{@�
=A�A�A?�A_�A�A�A�A�A�A�A�A��\A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B�#�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B؊>Bۊ>B߽qB��B��B��B��>B�W
B��qB��qB��C޸C�RC�RC�RC	�RC�RC�RC�RC�RC�RC�C�C�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCL+�CN�CO޸CQ޸CS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy޸C{�RC}޸C�RC��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D�zD�=D�?
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
D��
D��
D�?
D�
D��
D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�S�A�d&A�c A�s�A�m�A�kA�rGA�}VA�~�AʀiA�~(A�{�A�|�A�}VA�~�A�.Aʀ AʂAAʄ�Aʇ�AʊrAʉAʍPAʎ"Aʌ�Aʉ�A�t�A�MA�(�A���A���A���A��A�:A�FA��A�OA�-�A�33A��A��>A��]A��gAɯAɭ�AɮAɯOAɯOAɧAɗ�A�r�A��A�J�AƶFAŰUA��A�6A�!�A�ZQA��cA�-wA���A��zA��A��jA�}"A��A�x8A��qA�<jA��A��A��A��A��}A�S�A�<�A�%A��jA�y�A���A���A���A���A�̘A�:�A���A�R A�a�A��+A��A�,�A�}�A��A�.A�y�A���A�hsA�A�]�A�&A�ΥA��yA�~A~�\Ay}�Av�Apz�Am��Ai�[Ah͟AgxAf�{Ae�OAe�Ad~(AcYKAb%A^�FAZl"AWu�AV�*AU��ATSAS�fAQ?ANqAK��AI�7AC~�A>xA<6�A:[WA9�CA9\�A8�'A7($A5�A2QA1�uA1��A1A0�A-�oA+ĜA+&�A*�xA*A)�:A(c�A&sA%��A$��A"��A!~�A�A�mAj�A��A"hADgA�A�>AB[A�]A8�A-wAL0A�XA҉A�A�{AP�AGA��A��A$A�rAیA�zA�]A \A	AiDACAxlA[WA��A�}A
�A
]�A	MjAZ�A^�A0UA�7A��AoAw�A��A�]A �
A �UA IR@�N�@�w�@��@���@�r�@��0@��@��@��@�kQ@�ƨ@��@��@�+�@���@�]d@�E�@���@���@�?@��@��X@�o @�J�@�E9@�_@��@��>@���@��r@�خ@���@�ԕ@���@�j@�g8@���@�o�@��@�C�@���@�8�@�x@���@�,�@�c�@�@���@�bN@䩓@�ߤ@��8@�!-@���@�V@��@߰�@�y�@�f�@�ی@ᦵ@�Z�@��"@��@䍹@��@�u@Ṍ@��@߷�@���@��@ۊ	@�j�@�>B@�b@��@��N@��@ۊ�@�u�@��a@��@��A@�|@�S&@�/�@���@�H@�S�@�C�@���@�R�@���@��@�@�zx@�8@��@�h
@�($@���@�zx@�,�@��8@�GE@��W@��@�4n@�o @��@�kQ@�4@͖S@̴9@�j�@���@�^5@�y�@��,@Ȁ�@�(�@��)@ǥ�@ǈf@�F�@ƋD@ōP@�(@ī6@�bN@�4@���@å�@�'�@��@��8@��@º�@�bN@���@��M@�v�@�?�@�b@���@�@���@�1@���@�5�@��@���@�خ@��@��U@��@�d�@�_@���@���@��@�T�@��@���@�J@�x@�2a@���@��@�:�@��@���@��1@�j@��D@�u�@�+�@� i@���@�A�@��0@���@�Y�@��I@�7�@���@�qv@�?}@���@�:*@�4@��C@�Y�@���@�ϫ@���@�_p@�=�@��@��@���@�@�u�@�&@��f@���@��[@��@�ݘ@�=�@��@��]@��_@���@��q@��@�!-@���@�_@���@��@���@���@���@�Q@���@�T�@��c@���@�B[@��@��@���@���@�6@�_@��3@�j�@�P�@�^�@�Dg@��@��@�1@��-@�}�@�:�@��@�	l@���@���@�u@�ƨ@��*@�rG@�(@���@��@�#:@���@���@��@�Y�@�-w@��@���@��@�q�@�-�@��@���@�ԕ@���@�A @���@�	@���@��~@��{@�k�@�5�@���@�͟@��I@���@�[�@��N@���@�F@�$t@���@�V@��Z@���@�F�@��v@�YK@�,=@��@�x@�1@���@���@�'�@���@���@��@��F@�d�@��@���@��@�a�@�=�@�(�@�@@�;@���@�~�@�$�@�خ@��V@�rG@�J#@� \@�@�ߤ@���@�_�@��3@�a@���@��Y@�0U@�x@��Q@���@�j�@�N<@��B@���@��\@�ff@��@��D@���@���@�l�@�Z�@�J#@�!�@��]@�z�@�R�@�6@��@�l�@�J#@��@���@�[�@�!�@��@�P@Mj@�@~�L@~n�@~Ov@~=q@~$�@}�z@}4@|֡@|��@|(�@{=@z�'@zxl@zM�@y��@y�C@y|@ya�@x�`@x,=@w�K@w��@w�@w�@vh
@vJ�@vB[@u��@u�'@u��@u�S@u-w@tb@s�@s{J@sO@r�2@rn�@q�.@qX@q<6@q�@p�D@p/�@p�@o�a@o|�@o6z@n�@n��@nH�@m��@mϫ@m��@m�S@mX@m�@l�5@l�4@l��@lA�@k�&@k��@kS�@k/�@j��@j��@j^5@j	@i�7@iV@g��@g
=@f��@fh
@e�^@e�=@e[W@d��@d��@d��@d|�@dXy@d�@c�@c� @c��@cRT@b�B@b�F@b@�@a�D@a��@a[W@a8�@a�@`�@``�@`b@_�a@_�F@_~�@_.I@^�c@^p;@]�@]u�@]k�@]Y�@]N<@]5�@\��@\�p@\��@\�@[��@[=@[(@Z҉@Z��@Z� @Z?@Yϫ@Yq@X�@Xw�@W�@We�@W�@V�@V��@Vi�@Vc @VM�@U�@U�H@U2a@T�|@T��@Tr�@T?�@Tx@Sݘ@SP�@R�@RGE@Q�^@QT�@P��@P:�@O��@O|�@Os@Oa@Nں@N��@N\�@N�@M�d@M��@M�7@Ms�@ML�@Mq@M;@L��@Lu�@LV�@L�@K��@K\)@J�M@J�m@J�\@J�+@Jl�@J!�@I�H@I2a@G�@G,�@F�8@F��@FTa@F�@E@E��@E�7@Ee,@EJ�@E�@D��@D��@C��@B�'@B��@Bp;@B_@A�@A�t@Aj@A�@@��@@��@@�@?�@>�6@>Q@>E�@=�N@=O�@=@<�@<�I@<C-@<�@;�+@;ƨ@;��@:�2@:_�@:)�@9�@9�@9�h@9a�@9q@8�$@8j@82�@7�*@7{J@7X�@7K�@7�@6��@5�.@5c�@5�@4��@4�@4��@4]d@3�@3��@3iD@3,�@2��@2p;@2\�@2;�@1��@1�t@1�@1k�@1J�@1	l@0�?@0w�@0*�@/��@/�@/o@.�'@.?@-�#@-��@-[W@-�@,ѷ@,9X@+˒@+�a@+��@+�w@+�f@+O@+6z@+C@+Y@*�,@*��@*�@*�A@)�.@)��@)�9@)��@)�n@)B�@(�v@(��@(�.@(9X@(7@(�@'��@'��@'n/@',�@'Y@&�@&s�@&u@%8�@%�@$�9@$�4@$�o@$j@$'R@#|�@#o�@#l�@#1�@"�@"��@"^5@"C�@"E�@"GE@"{@!�M@ �	@ ~(@ %�@��@�4@ߤ@�x@�+@GE@ϫ@s�@S&@0�@��@�)@��@z�@ �@	�@1@�@  @�
@o�@&@�M@��@� @C�@5?@��@�>@ԕ@�-@��@F@�@��@�E@�@�o@D�@,=@$@�@��@�$@qv@_p@O@+@�M@ߤ@��@�@��@��@_�@GE@@�@��@f�@*0@%@�`@�$@�@Xy@<�@x@� @�F@�k@s@@O@"�@
=@�@��@��@c @L0@�@��@�3@�S@�M@e,@Y�@?}@�|@�4@_@4n@~@��@�K@�	@g�@�@��@�L@�\@Ta@O@�^@a�@+@�@	l@;@�P@�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�S�A�d&A�c A�s�A�m�A�kA�rGA�}VA�~�AʀiA�~(A�{�A�|�A�}VA�~�A�.Aʀ AʂAAʄ�Aʇ�AʊrAʉAʍPAʎ"Aʌ�Aʉ�A�t�A�MA�(�A���A���A���A��A�:A�FA��A�OA�-�A�33A��A��>A��]A��gAɯAɭ�AɮAɯOAɯOAɧAɗ�A�r�A��A�J�AƶFAŰUA��A�6A�!�A�ZQA��cA�-wA���A��zA��A��jA�}"A��A�x8A��qA�<jA��A��A��A��A��}A�S�A�<�A�%A��jA�y�A���A���A���A���A�̘A�:�A���A�R A�a�A��+A��A�,�A�}�A��A�.A�y�A���A�hsA�A�]�A�&A�ΥA��yA�~A~�\Ay}�Av�Apz�Am��Ai�[Ah͟AgxAf�{Ae�OAe�Ad~(AcYKAb%A^�FAZl"AWu�AV�*AU��ATSAS�fAQ?ANqAK��AI�7AC~�A>xA<6�A:[WA9�CA9\�A8�'A7($A5�A2QA1�uA1��A1A0�A-�oA+ĜA+&�A*�xA*A)�:A(c�A&sA%��A$��A"��A!~�A�A�mAj�A��A"hADgA�A�>AB[A�]A8�A-wAL0A�XA҉A�A�{AP�AGA��A��A$A�rAیA�zA�]A \A	AiDACAxlA[WA��A�}A
�A
]�A	MjAZ�A^�A0UA�7A��AoAw�A��A�]A �
A �UA IR@�N�@�w�@��@���@�r�@��0@��@��@��@�kQ@�ƨ@��@��@�+�@���@�]d@�E�@���@���@�?@��@��X@�o @�J�@�E9@�_@��@��>@���@��r@�خ@���@�ԕ@���@�j@�g8@���@�o�@��@�C�@���@�8�@�x@���@�,�@�c�@�@���@�bN@䩓@�ߤ@��8@�!-@���@�V@��@߰�@�y�@�f�@�ی@ᦵ@�Z�@��"@��@䍹@��@�u@Ṍ@��@߷�@���@��@ۊ	@�j�@�>B@�b@��@��N@��@ۊ�@�u�@��a@��@��A@�|@�S&@�/�@���@�H@�S�@�C�@���@�R�@���@��@�@�zx@�8@��@�h
@�($@���@�zx@�,�@��8@�GE@��W@��@�4n@�o @��@�kQ@�4@͖S@̴9@�j�@���@�^5@�y�@��,@Ȁ�@�(�@��)@ǥ�@ǈf@�F�@ƋD@ōP@�(@ī6@�bN@�4@���@å�@�'�@��@��8@��@º�@�bN@���@��M@�v�@�?�@�b@���@�@���@�1@���@�5�@��@���@�خ@��@��U@��@�d�@�_@���@���@��@�T�@��@���@�J@�x@�2a@���@��@�:�@��@���@��1@�j@��D@�u�@�+�@� i@���@�A�@��0@���@�Y�@��I@�7�@���@�qv@�?}@���@�:*@�4@��C@�Y�@���@�ϫ@���@�_p@�=�@��@��@���@�@�u�@�&@��f@���@��[@��@�ݘ@�=�@��@��]@��_@���@��q@��@�!-@���@�_@���@��@���@���@���@�Q@���@�T�@��c@���@�B[@��@��@���@���@�6@�_@��3@�j�@�P�@�^�@�Dg@��@��@�1@��-@�}�@�:�@��@�	l@���@���@�u@�ƨ@��*@�rG@�(@���@��@�#:@���@���@��@�Y�@�-w@��@���@��@�q�@�-�@��@���@�ԕ@���@�A @���@�	@���@��~@��{@�k�@�5�@���@�͟@��I@���@�[�@��N@���@�F@�$t@���@�V@��Z@���@�F�@��v@�YK@�,=@��@�x@�1@���@���@�'�@���@���@��@��F@�d�@��@���@��@�a�@�=�@�(�@�@@�;@���@�~�@�$�@�خ@��V@�rG@�J#@� \@�@�ߤ@���@�_�@��3@�a@���@��Y@�0U@�x@��Q@���@�j�@�N<@��B@���@��\@�ff@��@��D@���@���@�l�@�Z�@�J#@�!�@��]@�z�@�R�@�6@��@�l�@�J#@��@���@�[�@�!�@��@�P@Mj@�@~�L@~n�@~Ov@~=q@~$�@}�z@}4@|֡@|��@|(�@{=@z�'@zxl@zM�@y��@y�C@y|@ya�@x�`@x,=@w�K@w��@w�@w�@vh
@vJ�@vB[@u��@u�'@u��@u�S@u-w@tb@s�@s{J@sO@r�2@rn�@q�.@qX@q<6@q�@p�D@p/�@p�@o�a@o|�@o6z@n�@n��@nH�@m��@mϫ@m��@m�S@mX@m�@l�5@l�4@l��@lA�@k�&@k��@kS�@k/�@j��@j��@j^5@j	@i�7@iV@g��@g
=@f��@fh
@e�^@e�=@e[W@d��@d��@d��@d|�@dXy@d�@c�@c� @c��@cRT@b�B@b�F@b@�@a�D@a��@a[W@a8�@a�@`�@``�@`b@_�a@_�F@_~�@_.I@^�c@^p;@]�@]u�@]k�@]Y�@]N<@]5�@\��@\�p@\��@\�@[��@[=@[(@Z҉@Z��@Z� @Z?@Yϫ@Yq@X�@Xw�@W�@We�@W�@V�@V��@Vi�@Vc @VM�@U�@U�H@U2a@T�|@T��@Tr�@T?�@Tx@Sݘ@SP�@R�@RGE@Q�^@QT�@P��@P:�@O��@O|�@Os@Oa@Nں@N��@N\�@N�@M�d@M��@M�7@Ms�@ML�@Mq@M;@L��@Lu�@LV�@L�@K��@K\)@J�M@J�m@J�\@J�+@Jl�@J!�@I�H@I2a@G�@G,�@F�8@F��@FTa@F�@E@E��@E�7@Ee,@EJ�@E�@D��@D��@C��@B�'@B��@Bp;@B_@A�@A�t@Aj@A�@@��@@��@@�@?�@>�6@>Q@>E�@=�N@=O�@=@<�@<�I@<C-@<�@;�+@;ƨ@;��@:�2@:_�@:)�@9�@9�@9�h@9a�@9q@8�$@8j@82�@7�*@7{J@7X�@7K�@7�@6��@5�.@5c�@5�@4��@4�@4��@4]d@3�@3��@3iD@3,�@2��@2p;@2\�@2;�@1��@1�t@1�@1k�@1J�@1	l@0�?@0w�@0*�@/��@/�@/o@.�'@.?@-�#@-��@-[W@-�@,ѷ@,9X@+˒@+�a@+��@+�w@+�f@+O@+6z@+C@+Y@*�,@*��@*�@*�A@)�.@)��@)�9@)��@)�n@)B�@(�v@(��@(�.@(9X@(7@(�@'��@'��@'n/@',�@'Y@&�@&s�@&u@%8�@%�@$�9@$�4@$�o@$j@$'R@#|�@#o�@#l�@#1�@"�@"��@"^5@"C�@"E�@"GE@"{@!�M@ �	@ ~(@ %�@��@�4@ߤ@�x@�+@GE@ϫ@s�@S&@0�@��@�)@��@z�@ �@	�@1@�@  @�
@o�@&@�M@��@� @C�@5?@��@�>@ԕ@�-@��@F@�@��@�E@�@�o@D�@,=@$@�@��@�$@qv@_p@O@+@�M@ߤ@��@�@��@��@_�@GE@@�@��@f�@*0@%@�`@�$@�@Xy@<�@x@� @�F@�k@s@@O@"�@
=@�@��@��@c @L0@�@��@�3@�S@�M@e,@Y�@?}@�|@�4@_@4n@~@��@�K@�	@g�@�@��@�L@�\@Ta@O@�^@a�@+@�@	l@;@�P@�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	sMB	sB	sB	r�B	r�B	r�B	r�B	s�B	shB	s3B	r�B	r�B	r|B	r�B	r�B	r�B	r�B	s�B	s�B	t9B	u�B	u�B	x�B	y�B	zxB	��B	�B	�B	�kB	�OB	��B	��B	��B	�FB	��B	�B	��B	�rB	��B
4B
$B
&B
4nB
:�B
;0B
;�B
;�B
<B
=VB
>(B
=<B
C�B
S�B
tTB
�B
��BWB9�B?�BO�Bm�B� B��B�#B� B�B�B��B�eB�.B̳B�4B��B��B�?B�=B�"B�}B�wB�B�B�B�B�
B�'B��B��B�lB_pBL~B4�BEB
rB
�B
��B
��B
�|B
�(B
�B
�B
h$B
J�B
>]B
pB	��B	�B	�=B	y�B	h$B	R�B	J�B	DB	J#B	F�B	@�B	=B	8lB	1�B	!�B	�B	uB��B��B�RB��B	,B	)_B	%�B	B	KB��B�:BǔB�[B��B�"B��B�?B��B��B�^B��B�]B�{BƨB��B��B�+B��B�KB�dB��B��B�NBѝBևB�/B��B��B�B�RB��B�OB�B� B��B�|B�[B��B�hB��B�B��B	�B	 �B	'�B	3�B	7�B	8�B	:xB	=�B	D3B	KB	^�B	a�B	[WB	W�B	K)B	E9B	AoB	>]B	8�B	3�B	./B	'8B	"4B	dB	yB	�B	$B	!bB	�B	"B	�B	�B	!�B	!�B	#TB	:�B	@�B	@ B	B'B	C�B	K�B	UB	T,B	U�B	V�B	YKB	Z�B	^�B	b�B	ezB	kB	n}B	p�B	s�B	v`B	{dB	|�B	~BB	��B	�mB	�+B	�1B	�~B	�HB	��B	�YB	�_B	��B	�HB	��B	��B	�B	��B	��B	��B	��B	�$B	��B	�nB	�#B	�7B	��B	��B	�0B	��B	�#B	�DB	��B	�JB	��B	�_B	��B	�"B	�^B	ĶB	�B	�gB	�MB	�KB	�B	āB	�B	�B	��B	�wB	��B	��B	��B	� B	�B	��B	�}B	�)B	�<B	�.B	�BB	уB	�TB	��B	ևB	�1B	�#B	��B	�-B	�|B	�HB	��B	��B	��B	�FB	�B	�B	�
B	�B	�B	��B	�B	��B	�B	�"B	�"B	�B	�B	�yB	��B	�>B	�LB	�tB	�B	�fB	�RB	��B	�B	�B	�$B	�B	�B	�B	�QB	�B	�QB	��B	�B	�qB	�"B	��B	�B	�=B	��B	�B	�)B	��B	�wB	�B	�B	�B	�B	�oB	�B	��B	�!B	��B	�B	�B	��B	�MB	�B	�B	�B	�B	��B	��B	��B	��B	�FB	��B	��B	��B	�LB	��B	�lB	��B	�rB	��B	��B	��B	�dB	�B	��B	�6B	��B	��B	��B	��B	�6B	�dB	��B	�DB	�DB	�B	�B	�dB	��B	��B	��B
 �B
 �B
  B
  B
  B
  B	��B	��B	�HB	��B	�}B
 4B
 iB
 iB
 OB
 iB
 iB
 �B
�B
�B
AB
�B
�B
�B
�B
{B
�B
�B
MB
3B
�B
B
gB
3B
9B
B
�B
�B
�B
�B
�B
�B
+B
_B
zB
�B
B
�B

=B

�B

�B

�B

�B
�B
�B
B
�B
<B
�B
�B
�B
pB
�B
�B
�B
VB
BB
�B
�B
BB
�B
bB
�B
}B
�B
 B
TB
�B
�B
@B
gB
MB
�B
�B
mB
mB
�B
?B
?B
sB
�B
�B
$B
SB
�B
SB
B
1B
�B
�B
�B
�B
_B
�B
KB
�B
�B
�B
�B
7B
B
�B
	B
=B
#B
	B
�B
WB
�B
)B
CB
]B
xB
xB
�B
B
�B
B
jB
�B
B
pB
�B
�B
�B
 B
 �B
 �B
!B
!�B
"hB
"�B
"�B
#�B
#�B
#�B
$�B
%B
%`B
%zB
%�B
%�B
&LB
&�B
&�B
'B
'�B
(�B
)yB
)�B
)�B
)yB
)�B
)�B
)�B
*B
+B
+QB
+�B
,B
,"B
,WB
,�B
-B
-)B
-CB
-)B
-B
-wB
.B
.cB
.}B
.�B
/iB
/�B
/�B
/�B
0UB
0oB
0�B
0�B
1AB
1�B
2B
2aB
2�B
2�B
3B
2�B
2�B
33B
3MB
3B
2�B
2GB
1'B
1AB
1AB
1[B
1�B
2B
2�B
2�B
2�B
2�B
3B
3MB
3hB
3�B
3�B
4B
4nB
5B
5�B
5�B
5�B
6B
6+B
6zB
6�B
7�B
7�B
7�B
7�B
8B
8�B
8lB
88B
8�B
:B
:�B
:�B
:^B
:B
9�B
:B
9�B
:DB
:xB
:xB
:�B
;JB
;�B
;�B
;�B
;�B
;�B
;�B
;�B
;�B
<PB
<�B
<�B
=B
=B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
@4B
@ B
?�B
@�B
AUB
A�B
A�B
B�B
B�B
B�B
C-B
CGB
C�B
DB
D�B
D�B
D�B
EB
EB
EB
E9B
ESB
E�B
FB
F%B
F�B
GB
GEB
G_B
G�B
G�B
G�B
G�B
HB
HB
H�B
H�B
I7B
I7B
IRB
I�B
I�B
I�B
JXB
J�B
KDB
KxB
K�B
LdB
MB
L�B
MB
L�B
M�B
M�B
M�B
M�B
N"B
N"B
N"B
N<B
N"B
NVB
NVB
NVB
N�B
N�B
N�B
N�B
OBB
O�B
O�B
O�B
O�B
O�B
O�B
PB
PbB
QB
P�B
QB
QhB
Q�B
Q�B
R B
R:B
RTB
RTB
RTB
R�B
R�B
RoB
R�B
S�B
S[B
SuB
S�B
S�B
S�B
TB
TaB
T{B
T�B
T�B
U�B
VB
VSB
V9B
V�B
V�B
W$B
W$B
WYB
W�B
W�B
W�B
W�B
W�B
X_B
X�B
X�B
X�B
X�B
Y1B
YKB
YB
Y�B
Y�B
Z7B
Z�B
Z�B
Z�B
Z�B
Z�B
[	B
[�B
\B
\]B
\]B
\]B
\]B
\�B
\�B
]/B
]IB
]dB
]�B
^B
^B
^B
^OB
^jB
^�B
^�B
^�B
^�B
^�B
^�B
_!B
_!B
_�B
_�B
_�B
`BB
`�B
`�B
aB
abB
abB
a�B
bhB
bhB
bNB
bNB
b�B
b�B
b�B
b�B
b�B
c B
c B
c B
c B
c�B
c�B
c�B
c�B
c�B
d&B
dZB
d�B
d�B
d�B
d�B
d�B
eB
eB
ezB
e�B
e�B
e�B
e�B
ffB
g8B
g8B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
i_B
iyB
i�B
i�B
iyB
i�B
i�B
jeB
j�B
kB
kB
k�B
lB
lWB
l=B
lqB
l�B
m)B
m]B
mwB
m�B
m�B
m�B
nB
n}B
n}B
n}B
n}B
n}B
n}B
n�B
o5B
o5B
oOB
o�B
o�B
o�B
p!B
pB
p!B
p!B
p;B
poB
p�B
p�B
p�B
p�B
qB
qAB
qAB
q[B
qAB
q[B
q�B
q�B
q�B
q�B
rB
raB
raB
r�B
r�B
r�B
r�B
r�B
sB
s3B
shB
s�B
s�B
tB
t9B
tTB
t�B
t�B
t�B
t�B
t�B
u%B
u?B
u?B
utB
u�B
u�B
u�B
u�B
vB
v+B
vFB
v`B
v�B
v�B
wB
w2B
wLB
wLB
wfB
wLB
w�B
w�B
xB
xRB
xRB
x�B
x�B
x�B
x�B
y$B
y�B
y�B
y�B
y�B
y�B
zDB
z�B
z�B
z�B
z�B
z�B
z�B
{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	sMB	sB	sB	r�B	r�B	r�B	r�B	s�B	shB	s3B	r�B	r�B	r|B	r�B	r�B	r�B	r�B	s�B	s�B	t9B	u�B	u�B	x�B	y�B	zxB	��B	�B	�B	�kB	�OB	��B	��B	��B	�FB	��B	�B	��B	�rB	��B
4B
$B
&B
4nB
:�B
;0B
;�B
;�B
<B
=VB
>(B
=<B
C�B
S�B
tTB
�B
��BWB9�B?�BO�Bm�B� B��B�#B� B�B�B��B�eB�.B̳B�4B��B��B�?B�=B�"B�}B�wB�B�B�B�B�
B�'B��B��B�lB_pBL~B4�BEB
rB
�B
��B
��B
�|B
�(B
�B
�B
h$B
J�B
>]B
pB	��B	�B	�=B	y�B	h$B	R�B	J�B	DB	J#B	F�B	@�B	=B	8lB	1�B	!�B	�B	uB��B��B�RB��B	,B	)_B	%�B	B	KB��B�:BǔB�[B��B�"B��B�?B��B��B�^B��B�]B�{BƨB��B��B�+B��B�KB�dB��B��B�NBѝBևB�/B��B��B�B�RB��B�OB�B� B��B�|B�[B��B�hB��B�B��B	�B	 �B	'�B	3�B	7�B	8�B	:xB	=�B	D3B	KB	^�B	a�B	[WB	W�B	K)B	E9B	AoB	>]B	8�B	3�B	./B	'8B	"4B	dB	yB	�B	$B	!bB	�B	"B	�B	�B	!�B	!�B	#TB	:�B	@�B	@ B	B'B	C�B	K�B	UB	T,B	U�B	V�B	YKB	Z�B	^�B	b�B	ezB	kB	n}B	p�B	s�B	v`B	{dB	|�B	~BB	��B	�mB	�+B	�1B	�~B	�HB	��B	�YB	�_B	��B	�HB	��B	��B	�B	��B	��B	��B	��B	�$B	��B	�nB	�#B	�7B	��B	��B	�0B	��B	�#B	�DB	��B	�JB	��B	�_B	��B	�"B	�^B	ĶB	�B	�gB	�MB	�KB	�B	āB	�B	�B	��B	�wB	��B	��B	��B	� B	�B	��B	�}B	�)B	�<B	�.B	�BB	уB	�TB	��B	ևB	�1B	�#B	��B	�-B	�|B	�HB	��B	��B	��B	�FB	�B	�B	�
B	�B	�B	��B	�B	��B	�B	�"B	�"B	�B	�B	�yB	��B	�>B	�LB	�tB	�B	�fB	�RB	��B	�B	�B	�$B	�B	�B	�B	�QB	�B	�QB	��B	�B	�qB	�"B	��B	�B	�=B	��B	�B	�)B	��B	�wB	�B	�B	�B	�B	�oB	�B	��B	�!B	��B	�B	�B	��B	�MB	�B	�B	�B	�B	��B	��B	��B	��B	�FB	��B	��B	��B	�LB	��B	�lB	��B	�rB	��B	��B	��B	�dB	�B	��B	�6B	��B	��B	��B	��B	�6B	�dB	��B	�DB	�DB	�B	�B	�dB	��B	��B	��B
 �B
 �B
  B
  B
  B
  B	��B	��B	�HB	��B	�}B
 4B
 iB
 iB
 OB
 iB
 iB
 �B
�B
�B
AB
�B
�B
�B
�B
{B
�B
�B
MB
3B
�B
B
gB
3B
9B
B
�B
�B
�B
�B
�B
�B
+B
_B
zB
�B
B
�B

=B

�B

�B

�B

�B
�B
�B
B
�B
<B
�B
�B
�B
pB
�B
�B
�B
VB
BB
�B
�B
BB
�B
bB
�B
}B
�B
 B
TB
�B
�B
@B
gB
MB
�B
�B
mB
mB
�B
?B
?B
sB
�B
�B
$B
SB
�B
SB
B
1B
�B
�B
�B
�B
_B
�B
KB
�B
�B
�B
�B
7B
B
�B
	B
=B
#B
	B
�B
WB
�B
)B
CB
]B
xB
xB
�B
B
�B
B
jB
�B
B
pB
�B
�B
�B
 B
 �B
 �B
!B
!�B
"hB
"�B
"�B
#�B
#�B
#�B
$�B
%B
%`B
%zB
%�B
%�B
&LB
&�B
&�B
'B
'�B
(�B
)yB
)�B
)�B
)yB
)�B
)�B
)�B
*B
+B
+QB
+�B
,B
,"B
,WB
,�B
-B
-)B
-CB
-)B
-B
-wB
.B
.cB
.}B
.�B
/iB
/�B
/�B
/�B
0UB
0oB
0�B
0�B
1AB
1�B
2B
2aB
2�B
2�B
3B
2�B
2�B
33B
3MB
3B
2�B
2GB
1'B
1AB
1AB
1[B
1�B
2B
2�B
2�B
2�B
2�B
3B
3MB
3hB
3�B
3�B
4B
4nB
5B
5�B
5�B
5�B
6B
6+B
6zB
6�B
7�B
7�B
7�B
7�B
8B
8�B
8lB
88B
8�B
:B
:�B
:�B
:^B
:B
9�B
:B
9�B
:DB
:xB
:xB
:�B
;JB
;�B
;�B
;�B
;�B
;�B
;�B
;�B
;�B
<PB
<�B
<�B
=B
=B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
@4B
@ B
?�B
@�B
AUB
A�B
A�B
B�B
B�B
B�B
C-B
CGB
C�B
DB
D�B
D�B
D�B
EB
EB
EB
E9B
ESB
E�B
FB
F%B
F�B
GB
GEB
G_B
G�B
G�B
G�B
G�B
HB
HB
H�B
H�B
I7B
I7B
IRB
I�B
I�B
I�B
JXB
J�B
KDB
KxB
K�B
LdB
MB
L�B
MB
L�B
M�B
M�B
M�B
M�B
N"B
N"B
N"B
N<B
N"B
NVB
NVB
NVB
N�B
N�B
N�B
N�B
OBB
O�B
O�B
O�B
O�B
O�B
O�B
PB
PbB
QB
P�B
QB
QhB
Q�B
Q�B
R B
R:B
RTB
RTB
RTB
R�B
R�B
RoB
R�B
S�B
S[B
SuB
S�B
S�B
S�B
TB
TaB
T{B
T�B
T�B
U�B
VB
VSB
V9B
V�B
V�B
W$B
W$B
WYB
W�B
W�B
W�B
W�B
W�B
X_B
X�B
X�B
X�B
X�B
Y1B
YKB
YB
Y�B
Y�B
Z7B
Z�B
Z�B
Z�B
Z�B
Z�B
[	B
[�B
\B
\]B
\]B
\]B
\]B
\�B
\�B
]/B
]IB
]dB
]�B
^B
^B
^B
^OB
^jB
^�B
^�B
^�B
^�B
^�B
^�B
_!B
_!B
_�B
_�B
_�B
`BB
`�B
`�B
aB
abB
abB
a�B
bhB
bhB
bNB
bNB
b�B
b�B
b�B
b�B
b�B
c B
c B
c B
c B
c�B
c�B
c�B
c�B
c�B
d&B
dZB
d�B
d�B
d�B
d�B
d�B
eB
eB
ezB
e�B
e�B
e�B
e�B
ffB
g8B
g8B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
i_B
iyB
i�B
i�B
iyB
i�B
i�B
jeB
j�B
kB
kB
k�B
lB
lWB
l=B
lqB
l�B
m)B
m]B
mwB
m�B
m�B
m�B
nB
n}B
n}B
n}B
n}B
n}B
n}B
n�B
o5B
o5B
oOB
o�B
o�B
o�B
p!B
pB
p!B
p!B
p;B
poB
p�B
p�B
p�B
p�B
qB
qAB
qAB
q[B
qAB
q[B
q�B
q�B
q�B
q�B
rB
raB
raB
r�B
r�B
r�B
r�B
r�B
sB
s3B
shB
s�B
s�B
tB
t9B
tTB
t�B
t�B
t�B
t�B
t�B
u%B
u?B
u?B
utB
u�B
u�B
u�B
u�B
vB
v+B
vFB
v`B
v�B
v�B
wB
w2B
wLB
wLB
wfB
wLB
w�B
w�B
xB
xRB
xRB
x�B
x�B
x�B
x�B
y$B
y�B
y�B
y�B
y�B
y�B
zDB
z�B
z�B
z�B
z�B
z�B
z�B
{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105234  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191652  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191653  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191653                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041700  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041700  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                