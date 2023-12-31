CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-09-14T21:43:52Z creation;2022-09-14T21:43:53Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220914214352  20230309121502  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @��|e�81   @��,�5z@/@     �c��z�H1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�ffB�ffB�  B�  B�  B�  B���B�33B�  B�  B�  B�33B�ffB�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf33Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*fD*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJfDJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@fg@|��@�ff@�33A33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B�L�B�L�B��fB��fB��fB��fB�� B��B��fB��fB��fB��B�L�B��fB��fB��fB��fBǳ3B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB�L�B��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C4�C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cd�Cf&fCg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D*3D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DJ3DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�3D��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A؊=A؈�A؊	A؈�A؉7A،~A،~A،A؋xA؉7A؋DA،JA،�Aؑ Aؒ�Aؑ4Aؑ4Aؐ�A؏\A؏(A؎"A؎"A؍�A؍�A؍�A؍PA،�A؋�A؉�A؊=A؇�A�rAִ�A�9�AИ+A��?A�:�A�DgA�v�A�FA�h�A���A���A���A��A�D�A�!�A�L0A�J#A���A��dA� iA�R�A��XA�QA��A�qA���A�ZQA��A�|PA�8�A�PA��A�o�A�F?A�MA��6A���A�רA�8�A�;dA�qAA�ҽA�qA�+A��A�qAA��A���A��|A���A�P�A���A��KA�e,A�B�A��A�"hA��A�<�A��JA�M�AC-A}GEA{�FAy�UAw�`Asv`Am�Al_Ak<6Ag�Adn�A^��AZ�PAV�HAR;dAO�VAL��AIn�AG�^AFu�AETaAC��AA>�A@��A>ƨA<��A:4A6�]A5�A3IRA2�A2�9A2��A1�IA/��A.ɆA-�A,�<A,(�A+͟A*ffA)l�A(��A(��A(^5A(A'6A&�A%��A%�NA%�HA%33A%L�A%  A$�A$��A#�$A"��A!�A �	A A A ��A w�A L�A FtA PHA�'AD�A!A]dAB�A0UA�AcA�A�,A�XAm]A��A;dA9�A��A�A!AjA��Ay>A��A�CA� AjA�:A6zA�rA��A=qA�MA/�Ao�A�A��AffA/A@A~(A
�A
��A	��A	��A��A�AC�A�fA�3A�A�yA��A1'AqA�[Ac�A	�A��AN<A� A ��A �}A y�A ^�A .�@���@�2a@�oi@�:�@�?@�U�@��4@�q�@�.�@��@��}@�L0@�c@�z@�$�@��j@�33@�@�|�@��p@�C-@��@�خ@�@�F@��@�8�@�F@��@��@���@�p;@�B[@��@�+@�h
@�S�@�[�@���@�X�@�;@�@���@�M@��@��/@�D�@�@���@���@�҉@�@�z�@�*�@�t�@�#�@�@�;@��@��@��r@ߪ�@�s@ޣ�@�B[@��@ݧ�@�|�@�X@ڦL@��@�c@���@�Q�@��@׹�@�+@���@ֈ�@Ձ@ԅ�@�.�@Ӛk@�@@�V�@��j@�dZ@�L�@�p;@�e@�@ϓ@�(�@���@�O@��@ͷ@͊	@�IR@�	l@�`�@��g@�F�@ʼj@�"h@��@�S&@�n�@��)@Ǽ�@�c�@�Ĝ@�1�@��@Ŏ�@�!�@��,@Ā�@��3@�S�@�8�@�ߤ@�9X@���@�E9@�<6@�S�@�N<@�4�@�Ta@��S@�Dg@��H@���@��1@��9@���@�_p@���@���@���@��@�dZ@��"@�;�@�ݘ@�V@��@�bN@�	@��a@�/�@��4@�A�@�  @���@��@��@���@��@��I@�[�@��H@���@�Ĝ@�r�@��j@�{J@�J�@��Y@�@�c@�S@��]@�=@��<@�_�@�@���@��~@�Dg@���@�ݘ@���@�!-@���@���@��e@���@�a|@�,=@��+@���@�hs@��b@�@��z@��P@��=@��T@��g@�ԕ@�Dg@���@��.@�8�@��@���@��"@�=�@��|@��[@���@�p;@�1�@��j@��t@�"�@��@���@��m@�
�@��@�6z@��@�1�@���@���@��7@�]�@��@��@��`@���@���@���@�K^@�~@�1@��[@�X�@�q@��1@�?�@���@�J�@�8@�+@�@�R�@�-�@��@�s�@�>�@�!�@���@���@��@�\�@�$@��@��D@��N@��k@���@�~�@�e,@�Dg@��@���@�V@��@�g�@�<6@��@��@���@�7�@���@�rG@�X@�B�@�?}@��@���@��@�Z@�"h@��@��=@���@�o @�IR@��9@�h
@�PH@��)@���@�8@�;@�Z�@��@�;d@�	l@�.�@�|@�4�@��@��@��@��@�Q@���@��4@�rG@�x@�a�@�L�@�RT@�Y@��|@��s@���@��A@�?�@�G�@��@��y@�{�@�@��D@�˒@�|@�]�@�=@��@���@��R@��\@��Y@�E�@�4@���@�0�@��@���@��s@���@&@~E�@}��@}��@}�@}��@~�@}�@}#�@|�)@|%�@{�F@{j�@z�s@z�L@z{@y�7@x�`@w��@w]�@v�b@u�@u��@u��@uo @uO�@t�@t�O@s��@s\)@s�@r�A@rV@r0U@q�@q<6@p�@p��@p~(@ph�@o��@o��@oX�@o1�@o/�@n��@m��@m(�@l��@lPH@k�6@k�[@k��@kdZ@j��@jd�@j:*@i�)@i+�@h�Y@hN�@g��@f��@f��@fYK@f?@e��@ea�@eJ�@e=�@d��@d�@d]d@d1@c�+@c�@cJ#@cC@b�L@b#:@a�-@aw2@a@`��@`�I@`-�@_��@_O@_)_@^��@^{@]�@]G�@\ی@\[�@\ �@[�@[��@[(@Z�@ZE�@Y��@Y��@Y}�@Y;@X�j@X��@Xm�@XD�@X~@X  @W�Q@W��@W��@W�:@Wa@W
=@Vff@T�$@S��@S��@SRT@S6z@R�@R�@R��@Rp;@RE�@R�@Q��@Q�"@P�@P��@PA�@O=@N�c@Nu%@N�@M�D@M��@M��@M��@M:�@Lg8@L$@K�@Kn/@KY@J�H@J��@I��@Iw2@IX@IL�@I2a@H��@H��@H[�@HQ�@HQ�@H$@GS�@F��@FQ@E��@E�@E[W@EN<@E?}@E-w@D�@D�j@D�@D�o@D�@Dl"@Dh�@DM@C��@C9�@C$t@C i@B��@BGE@B!�@A�@AIR@A2a@A�@@�@@"h@?"�@>��@>;�@> �@=�C@=u�@=N<@<��@<��@<��@<'R@;t�@;33@:�"@;
=@;o@:��@:��@:��@:GE@9�.@9��@9��@9#�@8��@8�p@8[�@7�@7��@7iD@7"�@6�]@6ff@6&�@5�#@5S&@5�@4��@4>B@3��@3�@3��@3RT@2�@2�]@2��@2E�@2�@1�@1:�@0�P@0��@0�@/��@/v`@.ߤ@.#:@-c�@,��@+ݘ@+�w@+j�@+1�@*͟@*&�@)@)��@):�@(Ɇ@(tT@(Xy@(-�@'�@'6z@&�8@&��@&a|@&&�@&�@%��@%�=@%T�@$�@$�z@$|�@$m�@$_@$N�@$�@#�@#�r@#�@#�W@#�@#��@#s@#E9@#33@#&@"�@"�b@"+k@!u�@!S&@!<6@!0�@!!�@!q@!�@ �	@ �@ ��@ ��@ Q�@�r@�@e�@A�@�"@�x@��@v�@n�@a|@�@�C@A @!�@��@��@��@/�@�@�r@خ@��@�@��@�2@�X@��@��@��@� @R�@�o@�`@�@�z@�@~(@tT@u�@w�@y>@y>@bN@D�@(�@��@��@l�@Y@�@��@�m@�@�!@�@a|@?@�@�@��@ی@��@�D@��@�.@�.@��@��@��@h�@bN@Z@K^@Ft@I�@�@�@��@RT@4�@�,@� @{�@C�@�T@�@�@s�@�@��@��@oi@7�@�@�
@�f@a@J#@�@��@�m@l�@+k@
�@�@�@��@�h@N<@�@�f@�f@�@��@l"@S�@H@4n@��@�&@�Q@��@��@iD@C�@,�@Y@
=@
�"@
�@
� @
d�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A؊=A؈�A؊	A؈�A؉7A،~A،~A،A؋xA؉7A؋DA،JA،�Aؑ Aؒ�Aؑ4Aؑ4Aؐ�A؏\A؏(A؎"A؎"A؍�A؍�A؍�A؍PA،�A؋�A؉�A؊=A؇�A�rAִ�A�9�AИ+A��?A�:�A�DgA�v�A�FA�h�A���A���A���A��A�D�A�!�A�L0A�J#A���A��dA� iA�R�A��XA�QA��A�qA���A�ZQA��A�|PA�8�A�PA��A�o�A�F?A�MA��6A���A�רA�8�A�;dA�qAA�ҽA�qA�+A��A�qAA��A���A��|A���A�P�A���A��KA�e,A�B�A��A�"hA��A�<�A��JA�M�AC-A}GEA{�FAy�UAw�`Asv`Am�Al_Ak<6Ag�Adn�A^��AZ�PAV�HAR;dAO�VAL��AIn�AG�^AFu�AETaAC��AA>�A@��A>ƨA<��A:4A6�]A5�A3IRA2�A2�9A2��A1�IA/��A.ɆA-�A,�<A,(�A+͟A*ffA)l�A(��A(��A(^5A(A'6A&�A%��A%�NA%�HA%33A%L�A%  A$�A$��A#�$A"��A!�A �	A A A ��A w�A L�A FtA PHA�'AD�A!A]dAB�A0UA�AcA�A�,A�XAm]A��A;dA9�A��A�A!AjA��Ay>A��A�CA� AjA�:A6zA�rA��A=qA�MA/�Ao�A�A��AffA/A@A~(A
�A
��A	��A	��A��A�AC�A�fA�3A�A�yA��A1'AqA�[Ac�A	�A��AN<A� A ��A �}A y�A ^�A .�@���@�2a@�oi@�:�@�?@�U�@��4@�q�@�.�@��@��}@�L0@�c@�z@�$�@��j@�33@�@�|�@��p@�C-@��@�خ@�@�F@��@�8�@�F@��@��@���@�p;@�B[@��@�+@�h
@�S�@�[�@���@�X�@�;@�@���@�M@��@��/@�D�@�@���@���@�҉@�@�z�@�*�@�t�@�#�@�@�;@��@��@��r@ߪ�@�s@ޣ�@�B[@��@ݧ�@�|�@�X@ڦL@��@�c@���@�Q�@��@׹�@�+@���@ֈ�@Ձ@ԅ�@�.�@Ӛk@�@@�V�@��j@�dZ@�L�@�p;@�e@�@ϓ@�(�@���@�O@��@ͷ@͊	@�IR@�	l@�`�@��g@�F�@ʼj@�"h@��@�S&@�n�@��)@Ǽ�@�c�@�Ĝ@�1�@��@Ŏ�@�!�@��,@Ā�@��3@�S�@�8�@�ߤ@�9X@���@�E9@�<6@�S�@�N<@�4�@�Ta@��S@�Dg@��H@���@��1@��9@���@�_p@���@���@���@��@�dZ@��"@�;�@�ݘ@�V@��@�bN@�	@��a@�/�@��4@�A�@�  @���@��@��@���@��@��I@�[�@��H@���@�Ĝ@�r�@��j@�{J@�J�@��Y@�@�c@�S@��]@�=@��<@�_�@�@���@��~@�Dg@���@�ݘ@���@�!-@���@���@��e@���@�a|@�,=@��+@���@�hs@��b@�@��z@��P@��=@��T@��g@�ԕ@�Dg@���@��.@�8�@��@���@��"@�=�@��|@��[@���@�p;@�1�@��j@��t@�"�@��@���@��m@�
�@��@�6z@��@�1�@���@���@��7@�]�@��@��@��`@���@���@���@�K^@�~@�1@��[@�X�@�q@��1@�?�@���@�J�@�8@�+@�@�R�@�-�@��@�s�@�>�@�!�@���@���@��@�\�@�$@��@��D@��N@��k@���@�~�@�e,@�Dg@��@���@�V@��@�g�@�<6@��@��@���@�7�@���@�rG@�X@�B�@�?}@��@���@��@�Z@�"h@��@��=@���@�o @�IR@��9@�h
@�PH@��)@���@�8@�;@�Z�@��@�;d@�	l@�.�@�|@�4�@��@��@��@��@�Q@���@��4@�rG@�x@�a�@�L�@�RT@�Y@��|@��s@���@��A@�?�@�G�@��@��y@�{�@�@��D@�˒@�|@�]�@�=@��@���@��R@��\@��Y@�E�@�4@���@�0�@��@���@��s@���@&@~E�@}��@}��@}�@}��@~�@}�@}#�@|�)@|%�@{�F@{j�@z�s@z�L@z{@y�7@x�`@w��@w]�@v�b@u�@u��@u��@uo @uO�@t�@t�O@s��@s\)@s�@r�A@rV@r0U@q�@q<6@p�@p��@p~(@ph�@o��@o��@oX�@o1�@o/�@n��@m��@m(�@l��@lPH@k�6@k�[@k��@kdZ@j��@jd�@j:*@i�)@i+�@h�Y@hN�@g��@f��@f��@fYK@f?@e��@ea�@eJ�@e=�@d��@d�@d]d@d1@c�+@c�@cJ#@cC@b�L@b#:@a�-@aw2@a@`��@`�I@`-�@_��@_O@_)_@^��@^{@]�@]G�@\ی@\[�@\ �@[�@[��@[(@Z�@ZE�@Y��@Y��@Y}�@Y;@X�j@X��@Xm�@XD�@X~@X  @W�Q@W��@W��@W�:@Wa@W
=@Vff@T�$@S��@S��@SRT@S6z@R�@R�@R��@Rp;@RE�@R�@Q��@Q�"@P�@P��@PA�@O=@N�c@Nu%@N�@M�D@M��@M��@M��@M:�@Lg8@L$@K�@Kn/@KY@J�H@J��@I��@Iw2@IX@IL�@I2a@H��@H��@H[�@HQ�@HQ�@H$@GS�@F��@FQ@E��@E�@E[W@EN<@E?}@E-w@D�@D�j@D�@D�o@D�@Dl"@Dh�@DM@C��@C9�@C$t@C i@B��@BGE@B!�@A�@AIR@A2a@A�@@�@@"h@?"�@>��@>;�@> �@=�C@=u�@=N<@<��@<��@<��@<'R@;t�@;33@:�"@;
=@;o@:��@:��@:��@:GE@9�.@9��@9��@9#�@8��@8�p@8[�@7�@7��@7iD@7"�@6�]@6ff@6&�@5�#@5S&@5�@4��@4>B@3��@3�@3��@3RT@2�@2�]@2��@2E�@2�@1�@1:�@0�P@0��@0�@/��@/v`@.ߤ@.#:@-c�@,��@+ݘ@+�w@+j�@+1�@*͟@*&�@)@)��@):�@(Ɇ@(tT@(Xy@(-�@'�@'6z@&�8@&��@&a|@&&�@&�@%��@%�=@%T�@$�@$�z@$|�@$m�@$_@$N�@$�@#�@#�r@#�@#�W@#�@#��@#s@#E9@#33@#&@"�@"�b@"+k@!u�@!S&@!<6@!0�@!!�@!q@!�@ �	@ �@ ��@ ��@ Q�@�r@�@e�@A�@�"@�x@��@v�@n�@a|@�@�C@A @!�@��@��@��@/�@�@�r@خ@��@�@��@�2@�X@��@��@��@� @R�@�o@�`@�@�z@�@~(@tT@u�@w�@y>@y>@bN@D�@(�@��@��@l�@Y@�@��@�m@�@�!@�@a|@?@�@�@��@ی@��@�D@��@�.@�.@��@��@��@h�@bN@Z@K^@Ft@I�@�@�@��@RT@4�@�,@� @{�@C�@�T@�@�@s�@�@��@��@oi@7�@�@�
@�f@a@J#@�@��@�m@l�@+k@
�@�@�@��@�h@N<@�@�f@�f@�@��@l"@S�@H@4n@��@�&@�Q@��@��@iD@C�@,�@Y@
=@
�"@
�@
� @
d�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�B
��B
бB
��B
бB
��B
� B
��B
��B
� B
��B
бB
��B
��B
� B
��B
��B
бB
��B
��B
��B
��B
��B
бB
��B
��B
� B
��B
ЗB
�}B
��B
��B
ϫB
�FB
�]B2B/�BK^B\xBqB+B!BV�BvFB�B�QB�XB��B�B��B\]BL�BDBI7BgmBe�BY�BI7B7�B&LB�BdB�B��B�]B�B�pB�dB��B��B��B|Bp�Bf�B\BQNB@OB8�B,WB;BhB iB
�B
��B
�B
��B
s�B
e,B
J�B
+�B
"�B
QB
$B
.B
�B	��B	��B	�B	��B	��B	��B	�qB	�^B	xRB	YeB	EB	/5B	B	�B	�B��B��B�B�GB��B�B�2B��BݲB�{B�0B��B�]B�jBߊB��B��B�KB��B�wB	<B	B	!�B	4�B	GEB	P}B	X�B	w2B	z�B	v`B	��B	�bB	��B	�B	��B	��B	�XB	̈́B	ϑB	ҽB	�6B	�tB	�AB	�VB	�VB	�;B	޸B	�$B	�`B	��B	�+B	��B	��B	�qB	�B	��B
 �B
 iB	��B	�}B	��B	��B	��B	��B	��B	�B	�nB	��B	��B	�`B	��B	�B	�B	�B	�B	�UB	��B	�B	��B	��B	�B	�)B	�B	�kB	�KB	�B	�iB	�IB	�6B	�*B	�sB	�eB	� B	�UB	�aB	�aB	�B	�B	�B	�B	�B	�B	�B	��B	�wB	�WB	�B	��B	��B	��B	�CB	��B	�AB	�`B	�%B	��B	��B	�B	�B	��B	�B	��B	�?B	�FB	��B	�%B	�B	��B	�GB	�B	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�$B	�B	�+B	�B	�B	��B	�GB	��B	�AB	�AB	��B	�B	�B	�GB	��B	�MB	�B	�B	�B	�9B	��B	�B	��B	�?B	�B	��B	�B	�%B	��B	�B	�B	��B	��B	�%B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�?B	��B	�B	�|B	�|B	�B	��B	�AB	�B	�B	��B	�TB	��B	��B	��B	�hB	�TB	��B	�B	��B	�?B	��B	��B	��B	�+B	��B	��B	�LB	�lB	��B	��B	��B	�$B	��B	��B	�RB	�B	�lB	��B	��B	��B	�^B	��B	�B	�6B	��B	��B	�"B	��B
  B
 �B	��B	��B	�B	�]B
 iB
;B
B
�B
MB
�B
�B
�B
tB
�B
�B
tB
�B
?B
?B
�B
_B
B
�B
B
KB
fB
�B
	7B
	7B
	7B
	RB
	B
	B
�B
	�B
	B
	RB
	�B
	�B
	lB
	�B
	RB
�B
	B
	�B
	�B
	�B

	B
	�B

rB

XB

=B

=B

rB

=B

�B

rB

rB

�B

�B
)B
)B
B

�B
B
�B

�B

rB
)B
�B
�B
[B
uB
�B
(B
�B
jB
jB
6B
6B
�B
�B
�B
BB
B
�B
�B
[B
�B
@B
@B
uB
@B
:B
�B
�B
:B
 B
TB
oB
�B
B
[B
@B
@B
&B
�B
�B
�B
{B
B
MB
gB
B
�B
�B
$B
YB
�B
�B
�B
?B
sB
+B
EB
yB
1B
�B
QB
�B
�B
�B
�B
�B
qB
qB
WB
qB
�B
B
)B
�B
OB
jB
�B
�B
!B
VB
 B
 vB
!HB
!�B
!�B
!�B
"�B
# B
"�B
"hB
"NB
"�B
#B
#B
#B
#B
$ZB
$�B
$�B
%zB
&B
&�B
'8B
*B
,�B
-B
-B
*�B
)_B
)yB
)�B
)�B
)�B
)�B
*�B
*�B
+kB
,"B
-CB
-�B
-�B
/5B
1B
1�B
1�B
1�B
1�B
1�B
2�B
2�B
2�B
2�B
2aB
2�B
2�B
3B
3B
3�B
4B
4TB
4TB
49B
4�B
4�B
4�B
4�B
5?B
4�B
49B
3�B
3MB
2�B
1�B
2B
2aB
3�B
5ZB
6�B
7�B
88B
8lB
9	B
9>B
9>B
9rB
9XB
9�B
9�B
9�B
:xB
:^B
:�B
;B
;0B
;0B
;B
:�B
:�B
:�B
;JB
;0B
;JB
;B
;dB
;JB
;JB
;B
:�B
;0B
;�B
<PB
=B
=VB
=�B
=�B
?HB
?�B
?HB
?.B
?HB
@4B
@iB
@OB
@OB
@�B
AoB
AoB
AB
@�B
@4B
@ B
@4B
@B
?�B
?�B
?�B
?�B
?�B
@OB
@OB
@OB
A B
A;B
AoB
A�B
A�B
BAB
BuB
BuB
B�B
CB
C{B
CaB
C�B
C�B
C�B
D3B
D�B
D�B
D�B
EB
EmB
E�B
E�B
FB
F?B
F?B
FYB
FtB
F�B
F�B
G�B
G�B
G�B
HfB
H�B
H�B
H�B
IB
IB
IB
IB
I7B
I7B
I7B
IB
I7B
IRB
I�B
J�B
KDB
K)B
KxB
K^B
K�B
K�B
K�B
K�B
LB
L0B
LJB
LJB
L�B
L�B
L�B
M�B
M�B
M�B
NB
NB
N"B
N"B
N<B
N�B
OBB
O(B
OBB
O�B
OvB
OvB
O�B
PB
P.B
P.B
P.B
PB
PHB
P�B
P�B
P�B
P�B
P�B
QB
QB
Q�B
RB
R�B
RoB
R�B
R�B
R�B
R�B
R�B
SB
SB
UgB
V�B
V�B
VmB
U�B
UgB
UgB
U�B
T�B
T�B
U2B
UMB
UB
VB
W
B
WYB
W$B
V9B
U�B
U�B
U�B
VB
VB
VB
V�B
V�B
V�B
V�B
W�B
X+B
Y1B
Y�B
Z�B
Z�B
[WB
[�B
\B
[�B
[qB
[=B
[=B
\CB
\�B
]~B
]�B
^B
^B
^B
^OB
^OB
^�B
^�B
^�B
^�B
_;B
_�B
_�B
_�B
_�B
_�B
`B
`B
`'B
abB
a�B
bB
bNB
b4B
bB
b4B
bB
bB
b�B
c B
c�B
dB
d�B
d�B
d�B
d�B
eFB
e�B
fB
e�B
ffB
f�B
gB
gB
gB
g�B
g�B
h
B
h$B
hsB
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
jB
j0B
jeB
j�B
j�B
kB
j�B
kB
k6B
k�B
k�B
kkB
kkB
k�B
k�B
k�B
lWB
m)B
mCB
mCB
mwB
mwB
m�B
m�B
m�B
m�B
m�B
m�B
nIB
ncB
ncB
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o5B
o�B
o�B
o�B
o�B
p!B
p�B
p�B
p�B
qB
qAB
q�B
q�B
q�B
rB
rB
rB
q�B
rB
rGB
r�B
s�B
tB
tB
tB
t9B
t9B
t9B
t9B
t9B
tB
t9B
tnB
t�B
t�B
t�B
uZB
u�B
u�B
u�B
vB
v+B
v+B
vzB
v�B
v�B
v�B
v�B
v�B
xRB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
y	B
y$B
y	B
yXB
yXB
y�B
y�B
zB
z^B
z�B
z�B
z�B
{dB
{dB
{B
{�B
|B
|B
{�B
|�B
|�B
|�B
|�B
}<B
}<B
}"B
}VB
}VB
}�B
}�B
~BB
~]B
~]B
~wB
~wB
~�B
~�B
.B
cB
�B
�B
�OB
��B
��B
��B
�B
�oB
��B
��B
��B
��B
�AB
�[B
��B
�uB
��B
��B
��B
�-B
�G111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�B
��B
бB
��B
бB
��B
� B
��B
��B
� B
��B
бB
��B
��B
� B
��B
��B
бB
��B
��B
��B
��B
��B
бB
��B
��B
� B
��B
ЗB
�}B
��B
��B
ϫB
�FB
�]B2B/�BK^B\xBqB+B!BV�BvFB�B�QB�XB��B�B��B\]BL�BDBI7BgmBe�BY�BI7B7�B&LB�BdB�B��B�]B�B�pB�dB��B��B��B|Bp�Bf�B\BQNB@OB8�B,WB;BhB iB
�B
��B
�B
��B
s�B
e,B
J�B
+�B
"�B
QB
$B
.B
�B	��B	��B	�B	��B	��B	��B	�qB	�^B	xRB	YeB	EB	/5B	B	�B	�B��B��B�B�GB��B�B�2B��BݲB�{B�0B��B�]B�jBߊB��B��B�KB��B�wB	<B	B	!�B	4�B	GEB	P}B	X�B	w2B	z�B	v`B	��B	�bB	��B	�B	��B	��B	�XB	̈́B	ϑB	ҽB	�6B	�tB	�AB	�VB	�VB	�;B	޸B	�$B	�`B	��B	�+B	��B	��B	�qB	�B	��B
 �B
 iB	��B	�}B	��B	��B	��B	��B	��B	�B	�nB	��B	��B	�`B	��B	�B	�B	�B	�B	�UB	��B	�B	��B	��B	�B	�)B	�B	�kB	�KB	�B	�iB	�IB	�6B	�*B	�sB	�eB	� B	�UB	�aB	�aB	�B	�B	�B	�B	�B	�B	�B	��B	�wB	�WB	�B	��B	��B	��B	�CB	��B	�AB	�`B	�%B	��B	��B	�B	�B	��B	�B	��B	�?B	�FB	��B	�%B	�B	��B	�GB	�B	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�$B	�B	�+B	�B	�B	��B	�GB	��B	�AB	�AB	��B	�B	�B	�GB	��B	�MB	�B	�B	�B	�9B	��B	�B	��B	�?B	�B	��B	�B	�%B	��B	�B	�B	��B	��B	�%B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�?B	��B	�B	�|B	�|B	�B	��B	�AB	�B	�B	��B	�TB	��B	��B	��B	�hB	�TB	��B	�B	��B	�?B	��B	��B	��B	�+B	��B	��B	�LB	�lB	��B	��B	��B	�$B	��B	��B	�RB	�B	�lB	��B	��B	��B	�^B	��B	�B	�6B	��B	��B	�"B	��B
  B
 �B	��B	��B	�B	�]B
 iB
;B
B
�B
MB
�B
�B
�B
tB
�B
�B
tB
�B
?B
?B
�B
_B
B
�B
B
KB
fB
�B
	7B
	7B
	7B
	RB
	B
	B
�B
	�B
	B
	RB
	�B
	�B
	lB
	�B
	RB
�B
	B
	�B
	�B
	�B

	B
	�B

rB

XB

=B

=B

rB

=B

�B

rB

rB

�B

�B
)B
)B
B

�B
B
�B

�B

rB
)B
�B
�B
[B
uB
�B
(B
�B
jB
jB
6B
6B
�B
�B
�B
BB
B
�B
�B
[B
�B
@B
@B
uB
@B
:B
�B
�B
:B
 B
TB
oB
�B
B
[B
@B
@B
&B
�B
�B
�B
{B
B
MB
gB
B
�B
�B
$B
YB
�B
�B
�B
?B
sB
+B
EB
yB
1B
�B
QB
�B
�B
�B
�B
�B
qB
qB
WB
qB
�B
B
)B
�B
OB
jB
�B
�B
!B
VB
 B
 vB
!HB
!�B
!�B
!�B
"�B
# B
"�B
"hB
"NB
"�B
#B
#B
#B
#B
$ZB
$�B
$�B
%zB
&B
&�B
'8B
*B
,�B
-B
-B
*�B
)_B
)yB
)�B
)�B
)�B
)�B
*�B
*�B
+kB
,"B
-CB
-�B
-�B
/5B
1B
1�B
1�B
1�B
1�B
1�B
2�B
2�B
2�B
2�B
2aB
2�B
2�B
3B
3B
3�B
4B
4TB
4TB
49B
4�B
4�B
4�B
4�B
5?B
4�B
49B
3�B
3MB
2�B
1�B
2B
2aB
3�B
5ZB
6�B
7�B
88B
8lB
9	B
9>B
9>B
9rB
9XB
9�B
9�B
9�B
:xB
:^B
:�B
;B
;0B
;0B
;B
:�B
:�B
:�B
;JB
;0B
;JB
;B
;dB
;JB
;JB
;B
:�B
;0B
;�B
<PB
=B
=VB
=�B
=�B
?HB
?�B
?HB
?.B
?HB
@4B
@iB
@OB
@OB
@�B
AoB
AoB
AB
@�B
@4B
@ B
@4B
@B
?�B
?�B
?�B
?�B
?�B
@OB
@OB
@OB
A B
A;B
AoB
A�B
A�B
BAB
BuB
BuB
B�B
CB
C{B
CaB
C�B
C�B
C�B
D3B
D�B
D�B
D�B
EB
EmB
E�B
E�B
FB
F?B
F?B
FYB
FtB
F�B
F�B
G�B
G�B
G�B
HfB
H�B
H�B
H�B
IB
IB
IB
IB
I7B
I7B
I7B
IB
I7B
IRB
I�B
J�B
KDB
K)B
KxB
K^B
K�B
K�B
K�B
K�B
LB
L0B
LJB
LJB
L�B
L�B
L�B
M�B
M�B
M�B
NB
NB
N"B
N"B
N<B
N�B
OBB
O(B
OBB
O�B
OvB
OvB
O�B
PB
P.B
P.B
P.B
PB
PHB
P�B
P�B
P�B
P�B
P�B
QB
QB
Q�B
RB
R�B
RoB
R�B
R�B
R�B
R�B
R�B
SB
SB
UgB
V�B
V�B
VmB
U�B
UgB
UgB
U�B
T�B
T�B
U2B
UMB
UB
VB
W
B
WYB
W$B
V9B
U�B
U�B
U�B
VB
VB
VB
V�B
V�B
V�B
V�B
W�B
X+B
Y1B
Y�B
Z�B
Z�B
[WB
[�B
\B
[�B
[qB
[=B
[=B
\CB
\�B
]~B
]�B
^B
^B
^B
^OB
^OB
^�B
^�B
^�B
^�B
_;B
_�B
_�B
_�B
_�B
_�B
`B
`B
`'B
abB
a�B
bB
bNB
b4B
bB
b4B
bB
bB
b�B
c B
c�B
dB
d�B
d�B
d�B
d�B
eFB
e�B
fB
e�B
ffB
f�B
gB
gB
gB
g�B
g�B
h
B
h$B
hsB
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
jB
j0B
jeB
j�B
j�B
kB
j�B
kB
k6B
k�B
k�B
kkB
kkB
k�B
k�B
k�B
lWB
m)B
mCB
mCB
mwB
mwB
m�B
m�B
m�B
m�B
m�B
m�B
nIB
ncB
ncB
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o5B
o�B
o�B
o�B
o�B
p!B
p�B
p�B
p�B
qB
qAB
q�B
q�B
q�B
rB
rB
rB
q�B
rB
rGB
r�B
s�B
tB
tB
tB
t9B
t9B
t9B
t9B
t9B
tB
t9B
tnB
t�B
t�B
t�B
uZB
u�B
u�B
u�B
vB
v+B
v+B
vzB
v�B
v�B
v�B
v�B
v�B
xRB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
y	B
y$B
y	B
yXB
yXB
y�B
y�B
zB
z^B
z�B
z�B
z�B
{dB
{dB
{B
{�B
|B
|B
{�B
|�B
|�B
|�B
|�B
}<B
}<B
}"B
}VB
}VB
}�B
}�B
~BB
~]B
~]B
~wB
~wB
~�B
~�B
.B
cB
�B
�B
�OB
��B
��B
��B
�B
�oB
��B
��B
��B
��B
�AB
�[B
��B
�uB
��B
��B
��B
�-B
�G111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220914214351  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220914214352  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220914214353  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220914214353                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220915064359  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220915064359  QCF$                G�O�G�O�G�O�             100JA  ARUP                                                                        20220914215958                      G�O�G�O�G�O�                JA  ARFMdecpA30a                                                                20230307235412  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230308001544  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230308001545  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230308001545                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230308001545  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230308001545  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230309121502                      G�O�G�O�G�O�                