CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-11-29T15:42:17Z creation;2022-11-29T15:42:18Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20221129154217  20221129155713  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�����1   @���R@//��w�cK�E���1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BI33BO��BVffB_��Bi��Bp  Bv  B�  B�  B�  B�33B�ffB�  B�  B�33B�  B�  B�  B�ffB���B�ffB���B���B���B�  B�  B�  B�  B�  B�33B�ffB�  B㙚B���B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&fD&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR�fDS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�3D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�p 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @fg@|��@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BI  BOfgBV33B_fgBifgBo��Bu��B��B��fB��fB��B�L�B��fB��fB��B��fB��fB��fB�L�B�� B�L�B�� B��3B��3B��fB��fB��fB��fB��fB��B�L�B��fB� B�3B��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CP�CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D&3D&�3D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR�3DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��3D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��D�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�nf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��6A���A��;A��A���A���A��2A��cA���A��lA��A��NA���Aӵ?AӯOAӫ�Aӟ�Aӓ�Aӌ�Aӊ�AӉ�AӃ�A�l�A�aA��]A��#A҄MA��A�e�A�\A�T�A�;dA�ĜA���A�F?A���A�ҽA��A���A�I�A��A�5tA�FtA�-�A�бA�A���A��A�(�A��A�|�A���A��aA��:A�D3A��GA���A���A��dA�N�A�A�cA��A�
	A}]�Az��At	lAl7�AbjAY҉AS<�AQ2�AM�AI��AE�AC�AAߤAAA?��A>ɆA<�A;1�A:��A8�tA7��A4�aA3�A2�oA/�A-��A+��A)��A(G�A&�cA%��A#��A#@A!V�A�qAAH�A@AQ�AȴAoiA)_A�AƨA�0AiDA�MAJ�A�AA~A4nAYA�A�A)�A��A 7�As�A'�A�AA��A��A�6A�A�hA�+A!jA ��A�$AD�A7�Al"A+A!�A@AD�AeA�AdZA!-A�AGA��A#:A�bA�1AsA�QA�4A�A�4A($A��A��AO�A��A+�Av`A�A�_AMA�A$�A?�A(�AAu�A��A�A��A��A� A+�A
�XA	��A	AںA�:A�A�]A��A�Ab�A0UAɆA��Aa�AG�A!�A��A`�A�AkQA[�AW?AK^A�ARTA�AM�A��A��A33A �9A O@��nA bNA bNA 3�@�E9@�I�@���@�m]@�֡@���@�YK@�"�@�@�z@�M@�=q@��@�Z�@��q@�x�@�\�@���@�Y�@��X@�m]@�Z�@��6@�"@��@�L@�33@�-�@��X@�A @�֡@���@�%@�F@��@�=@��,@�D�@�ݘ@���@�j@��@�g8@�Ta@�R�@�GE@墜@�+@㯸@�l�@⒣@���@�p�@�U�@�S&@�2a@���@ޚ�@�7�@�'R@�8�@�h
@�M�@���@���@��@�)_@��A@�%@��)@��@�O@���@�IR@Գh@�-w@�l"@�f�@�f�@�S@�s@�|@ң�@�1'@��j@�X@��/@Ј�@�Z�@���@�J�@�N�@���@�C-@˷@��@ʈ�@�Z@�'R@��@ɷ�@�H�@���@�r�@�	@���@�=q@��9@š�@�c@�a�@�)_@ĸR@�!�@æ�@�9�@�%@���@»�@�C�@��t@�V@���@��T@��V@�hs@�8@��`@�3�@�dZ@��@�e�@���@���@�$t@�y>@�@��}@��@��v@��}@�w�@�&�@���@���@�Y�@�5�@��@�?@�خ@�v`@�Vm@��8@�PH@��@�_@�@���@�p�@�33@��y@��4@�ݘ@���@�F@���@�PH@�خ@�J�@��@��p@���@��@���@��@���@�B[@��@��@@�]�@�+�@��@��@��f@��@�y>@�3�@��T@�|@�,�@�$t@�&@�!�@��,@�~(@��
@�Vm@��@��o@�خ@�o @�@��@���@���@�^5@�{@���@�9�@��c@��@��@�^5@��&@���@�g�@�&@���@�z@�I�@� �@�1@��@�u�@���@���@�c @�-�@���@��@�w�@�GE@�G@���@�w2@�,�@��!@�q�@�-@�G@���@�@@��O@���@�xl@�Z@�4n@�e@� �@��@��m@���@��:@�w2@�Z�@�@@��[@��R@���@�YK@��o@��^@��"@�F�@�'�@��@��E@���@���@��_@���@�^5@�5?@��@��+@���@��F@�e�@�	l@��@��A@�^5@�?�@��6@�f�@�Vm@�F@�F@�+�@���@��x@���@��x@���@�Ft@���@�|@�O@�=�@�
=@���@�u%@�4@���@�E9@��@��Y@�m�@�i�@�_�@�8�@�($@�u@��@���@��u@�^5@���@���@���@���@��k@�5�@���@�xl@�N�@�$@���@���@�`B@��@���@���@�$�@�@�{@��@�ݘ@�S�@��@���@�|�@�1�@�	�@�ݘ@��f@�G�@��/@���@���@�i�@�I�@�!�@��+@���@��@�"�@���@��]@��p@��@��}@���@�C-@��@a@�@~��@~��@~q�@~L0@~_@}��@}0�@|��@{ƨ@{@z��@z�h@y��@yA @x��@w�m@v\�@u�@u��@u�C@u�@u�@u��@u}�@u-w@t�)@tXy@s��@rں@q�)@q/@p��@pc�@o�&@oW?@n�+@n@m�@m�t@m�@l��@l_@k��@k�@k�@j�B@jQ@j-@j@iϫ@h�[@h�_@hy>@h�@g�[@gO@f�@f�@fd�@f!�@f$�@f�@ep�@e+�@d�@d%�@c;d@b�M@b��@b��@bE�@a��@azx@ax�@a�@`��@`b@_�4@_Y@^�\@]��@]��@]#�@\�$@\bN@[�	@[@[(@[@Z�A@YVm@Y+@X�/@X>B@W��@W��@Wv`@W&@V��@Vp;@V	@U�@U%@Tw�@S�+@St�@R�m@RR�@R	@Q�"@QS&@P�/@PC-@Oƨ@OJ#@N�8@Nq�@M��@M}�@MF@M@Lr�@K��@J��@Jh
@J�@IT�@H�9@H��@Hoi@H�@G�F@G8@F��@F?@F �@E��@E�@D��@C��@C��@C��@CdZ@C�@B�,@B�@Bs�@A��@Ac@A�@@�9@@K^@?��@?خ@?��@?�@?9�@>��@>�R@>�h@>�@>#:@>�@>_@=�D@=�)@=�>@=�X@=hs@=+@<��@;�*@;j�@;6z@:��@:��@:��@:d�@:�@9�'@9p�@9\�@9Dg@9 \@8�@8�D@8Q�@8b@7�K@7�$@7W?@7C@6�@6�'@6��@6�}@6�@6c @6�@5�n@5}�@5w2@5+�@4��@4H@3�
@3_p@2��@2�@2�F@2c @2�@1�@1c@1 \@0�[@0�O@0~(@/�&@/v`@.�y@.p;@.6�@-�D@-�@-��@-��@-F@,��@,�4@,�@,��@,�u@,��@,z�@,Q�@,7@+�@+ݘ@+�@+�f@+_p@+/�@*��@*ȴ@*kQ@*E�@*$�@)��@)�@)5�@(��@(h�@( �@(�@'��@'�@&�@&�s@&�B@&�@&�b@&��@%�T@%��@%2a@$��@$�.@$r�@$�@#�:@#>�@"�@"�h@"u%@"Q@"J@!�@!�t@!u�@!`B@!#�@ �`@ �e@ j@ �@�@��@�f@S�@�@�@a|@L0@@�^@��@p�@B�@@@��@N�@خ@��@a@]�@=@@��@�m@z@	@�#@�@�'@��@rG@N<@=�@&�@�P@Ĝ@��@��@"h@�&@��@��@x@U�@/�@�@�B@��@H�@&�@�D@@e,@�@��@��@K^@�@��@g�@"�@�2@��@Ov@E�@;�@:*@.�@#:@�@O@@@�N@�@f�@7L@q@��@ی@��@�Y@"h@�A@� @�@@�	@_p@K�@/�@�@��@^5@@�@�"@\�@�	@��@��@Xy@�@�[@��@g�@dZ@4�@�@
��@
��@
�@
�A@
^5@
&�@
 �@	�D@	�>@	ԕ@	�@	��@	�"@	o @	j@	A @	%@��@��@�@_@/�@1@�Q@��@dZ@�@҉@�}@��@� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��6A���A��;A��A���A���A��2A��cA���A��lA��A��NA���Aӵ?AӯOAӫ�Aӟ�Aӓ�Aӌ�Aӊ�AӉ�AӃ�A�l�A�aA��]A��#A҄MA��A�e�A�\A�T�A�;dA�ĜA���A�F?A���A�ҽA��A���A�I�A��A�5tA�FtA�-�A�бA�A���A��A�(�A��A�|�A���A��aA��:A�D3A��GA���A���A��dA�N�A�A�cA��A�
	A}]�Az��At	lAl7�AbjAY҉AS<�AQ2�AM�AI��AE�AC�AAߤAAA?��A>ɆA<�A;1�A:��A8�tA7��A4�aA3�A2�oA/�A-��A+��A)��A(G�A&�cA%��A#��A#@A!V�A�qAAH�A@AQ�AȴAoiA)_A�AƨA�0AiDA�MAJ�A�AA~A4nAYA�A�A)�A��A 7�As�A'�A�AA��A��A�6A�A�hA�+A!jA ��A�$AD�A7�Al"A+A!�A@AD�AeA�AdZA!-A�AGA��A#:A�bA�1AsA�QA�4A�A�4A($A��A��AO�A��A+�Av`A�A�_AMA�A$�A?�A(�AAu�A��A�A��A��A� A+�A
�XA	��A	AںA�:A�A�]A��A�Ab�A0UAɆA��Aa�AG�A!�A��A`�A�AkQA[�AW?AK^A�ARTA�AM�A��A��A33A �9A O@��nA bNA bNA 3�@�E9@�I�@���@�m]@�֡@���@�YK@�"�@�@�z@�M@�=q@��@�Z�@��q@�x�@�\�@���@�Y�@��X@�m]@�Z�@��6@�"@��@�L@�33@�-�@��X@�A @�֡@���@�%@�F@��@�=@��,@�D�@�ݘ@���@�j@��@�g8@�Ta@�R�@�GE@墜@�+@㯸@�l�@⒣@���@�p�@�U�@�S&@�2a@���@ޚ�@�7�@�'R@�8�@�h
@�M�@���@���@��@�)_@��A@�%@��)@��@�O@���@�IR@Գh@�-w@�l"@�f�@�f�@�S@�s@�|@ң�@�1'@��j@�X@��/@Ј�@�Z�@���@�J�@�N�@���@�C-@˷@��@ʈ�@�Z@�'R@��@ɷ�@�H�@���@�r�@�	@���@�=q@��9@š�@�c@�a�@�)_@ĸR@�!�@æ�@�9�@�%@���@»�@�C�@��t@�V@���@��T@��V@�hs@�8@��`@�3�@�dZ@��@�e�@���@���@�$t@�y>@�@��}@��@��v@��}@�w�@�&�@���@���@�Y�@�5�@��@�?@�خ@�v`@�Vm@��8@�PH@��@�_@�@���@�p�@�33@��y@��4@�ݘ@���@�F@���@�PH@�خ@�J�@��@��p@���@��@���@��@���@�B[@��@��@@�]�@�+�@��@��@��f@��@�y>@�3�@��T@�|@�,�@�$t@�&@�!�@��,@�~(@��
@�Vm@��@��o@�خ@�o @�@��@���@���@�^5@�{@���@�9�@��c@��@��@�^5@��&@���@�g�@�&@���@�z@�I�@� �@�1@��@�u�@���@���@�c @�-�@���@��@�w�@�GE@�G@���@�w2@�,�@��!@�q�@�-@�G@���@�@@��O@���@�xl@�Z@�4n@�e@� �@��@��m@���@��:@�w2@�Z�@�@@��[@��R@���@�YK@��o@��^@��"@�F�@�'�@��@��E@���@���@��_@���@�^5@�5?@��@��+@���@��F@�e�@�	l@��@��A@�^5@�?�@��6@�f�@�Vm@�F@�F@�+�@���@��x@���@��x@���@�Ft@���@�|@�O@�=�@�
=@���@�u%@�4@���@�E9@��@��Y@�m�@�i�@�_�@�8�@�($@�u@��@���@��u@�^5@���@���@���@���@��k@�5�@���@�xl@�N�@�$@���@���@�`B@��@���@���@�$�@�@�{@��@�ݘ@�S�@��@���@�|�@�1�@�	�@�ݘ@��f@�G�@��/@���@���@�i�@�I�@�!�@��+@���@��@�"�@���@��]@��p@��@��}@���@�C-@��@a@�@~��@~��@~q�@~L0@~_@}��@}0�@|��@{ƨ@{@z��@z�h@y��@yA @x��@w�m@v\�@u�@u��@u�C@u�@u�@u��@u}�@u-w@t�)@tXy@s��@rں@q�)@q/@p��@pc�@o�&@oW?@n�+@n@m�@m�t@m�@l��@l_@k��@k�@k�@j�B@jQ@j-@j@iϫ@h�[@h�_@hy>@h�@g�[@gO@f�@f�@fd�@f!�@f$�@f�@ep�@e+�@d�@d%�@c;d@b�M@b��@b��@bE�@a��@azx@ax�@a�@`��@`b@_�4@_Y@^�\@]��@]��@]#�@\�$@\bN@[�	@[@[(@[@Z�A@YVm@Y+@X�/@X>B@W��@W��@Wv`@W&@V��@Vp;@V	@U�@U%@Tw�@S�+@St�@R�m@RR�@R	@Q�"@QS&@P�/@PC-@Oƨ@OJ#@N�8@Nq�@M��@M}�@MF@M@Lr�@K��@J��@Jh
@J�@IT�@H�9@H��@Hoi@H�@G�F@G8@F��@F?@F �@E��@E�@D��@C��@C��@C��@CdZ@C�@B�,@B�@Bs�@A��@Ac@A�@@�9@@K^@?��@?خ@?��@?�@?9�@>��@>�R@>�h@>�@>#:@>�@>_@=�D@=�)@=�>@=�X@=hs@=+@<��@;�*@;j�@;6z@:��@:��@:��@:d�@:�@9�'@9p�@9\�@9Dg@9 \@8�@8�D@8Q�@8b@7�K@7�$@7W?@7C@6�@6�'@6��@6�}@6�@6c @6�@5�n@5}�@5w2@5+�@4��@4H@3�
@3_p@2��@2�@2�F@2c @2�@1�@1c@1 \@0�[@0�O@0~(@/�&@/v`@.�y@.p;@.6�@-�D@-�@-��@-��@-F@,��@,�4@,�@,��@,�u@,��@,z�@,Q�@,7@+�@+ݘ@+�@+�f@+_p@+/�@*��@*ȴ@*kQ@*E�@*$�@)��@)�@)5�@(��@(h�@( �@(�@'��@'�@&�@&�s@&�B@&�@&�b@&��@%�T@%��@%2a@$��@$�.@$r�@$�@#�:@#>�@"�@"�h@"u%@"Q@"J@!�@!�t@!u�@!`B@!#�@ �`@ �e@ j@ �@�@��@�f@S�@�@�@a|@L0@@�^@��@p�@B�@@@��@N�@خ@��@a@]�@=@@��@�m@z@	@�#@�@�'@��@rG@N<@=�@&�@�P@Ĝ@��@��@"h@�&@��@��@x@U�@/�@�@�B@��@H�@&�@�D@@e,@�@��@��@K^@�@��@g�@"�@�2@��@Ov@E�@;�@:*@.�@#:@�@O@@@�N@�@f�@7L@q@��@ی@��@�Y@"h@�A@� @�@@�	@_p@K�@/�@�@��@^5@@�@�"@\�@�	@��@��@Xy@�@�[@��@g�@dZ@4�@�@
��@
��@
�@
�A@
^5@
&�@
 �@	�D@	�>@	ԕ@	�@	��@	�"@	o @	j@	A @	%@��@��@�@_@/�@1@�Q@��@dZ@�@҉@�}@��@� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�JB�pB��B�JB�B�B�yB��B�B�B� BخB�B��B��B��B�NB��B�B��B�,B��B�*B��B�B
	BB�QB	�PB	��B	�\B	�6B	�@B	��B
�B
%zB
2�B
AoB
K�B
q'B
[=B
9rB
1AB
5�B
3B
*�B
qB
�B
"NB
%�B
 �B
"B
0B	�`B	��B	�B	�	B	уB	�}B	�<B	�B	��B	�FB	��B	��B	�mB	��B	utB	?B	�B�B��B�B�B��B�\B��B�sB��B�eB��B��B�2B��B�B�KB�YB�{By�Bw�BsMBm�Bj�Bi�BlWBt9B{�By�BtBu�B��B��B��B��B�KB��B��B��B�=B�B�=B��B�B	�B	�B	�B	�B	GB	l�B	��B	��B	��B	�B	��B	�dB	�|B	��B	�B	��B
�B
"4B
D�B
=<B
-]B
'B
)�B
>BB
OB
TaB
X�B
a�B
cB
g8B
d�B
bB
gmB
l�B
k�B
j�B
hsB
d@B
^B
[�B
Z7B
[�B
R�B
OB
NpB
VmB
V�B
UB
QhB
?}B
:�B
&B
mB
�B
�B
B
/B
�B
}B

	B
B
�B
B
�B
�B

�B
�B
�B
gB
�B
�B
�B
	B
�B
<B
�B
�B
<B
�B
:B
�B
B
oB
B
�B
4B
&B
�B
�B
hB
xB
4B
�B
�B
"B
.B
�B
�B
�B
VB
!|B
;B
B
xB
�B
�B
_B
sB
�B
�B
�B
&B
{B
�B
qB
�B
�B
�B
KB
�B
$B
uB
}B
(B
�B
�B
�B
�B
B
�B
KB

�B
~B

rB
	RB
�B
_B
�B
�B
�B
�B
�B
�B
1B
B
�B
B
YB
MB
�B
uB
 �B	�0B	�LB	��B	�B	��B	��B	�B	�lB	��B	��B
�B
�B
B
�B
UB
 OB	�B	��B	�fB	��B	�%B	�?B	�+B	��B	��B	�VB	��B	�VB	��B
oB
�B
B
'B
'B
B
 �B	��B	�HB	��B	��B	�xB	��B	�B	��B	�B	�fB	�LB	�fB	�fB	�B	�fB	�fB	�B	��B	��B	�ZB	�?B	��B	�fB	�B	��B	�B	�lB	�	B	�>B	�	B	��B	��B	�DB	��B	�B	��B	�xB	�*B	��B	��B	�rB	��B	�XB	�>B	��B	��B	��B	�B	�B	�VB	�VB	��B	�wB	��B	��B	�HB	��B	��B	��B	��B	�}B
 4B
 OB
 B
 4B
 OB
UB
B
B
 �B
 B
B
 B
uB
�B
-B
�B
GB
GB
B
�B
�B
�B
�B
�B
�B
�B
B
B
9B
mB
�B
B
B
%B
%B
%B
YB
�B
�B
�B
+B
zB
EB
B
�B
_B
zB
1B
fB
	B
	�B

�B
^B
�B
JB
0B
�B
B
PB
�B
�B
"B
�B
(B
�B
�B
�B
B
�B
�B
2B
gB
�B
�B
�B
9B
YB
�B
�B
�B
sB
�B

B
?B
B
�B
+B
�B
�B
�B
�B
�B
�B
�B
eB
KB
1B
1B
KB
KB
eB
eB
eB
B
�B
�B
�B
�B
�B
�B
�B
#B
�B
�B
B
�B
�B
B
�B
IB
IB
/B
~B
~B
�B
B
B
B
5B
�B
VB
�B
 B
 'B
 \B
!�B
!B
!-B
!-B
!-B
!�B
"NB
#�B
#�B
#:B
# B
"�B
# B
#B
"�B
"�B
"�B
#nB
#nB
$B
$�B
$�B
%FB
%�B
%�B
%�B
%�B
&2B
%�B
&B
'RB
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)_B
)�B
*B
*KB
*KB
*eB
*�B
*�B
+�B
,B
,B
,�B
,�B
,qB
,WB
,�B
-�B
.cB
.�B
/ B
/iB
/�B
0B
1B
1�B
1�B
2GB
2aB
2|B
2�B
0!B
2�B
2�B
33B
4B
4B
49B
49B
4TB
4TB
4�B
5B
5?B
5tB
5�B
5�B
6FB
6`B
6FB
6�B
6�B
6�B
7B
7�B
7�B
7�B
7�B
8�B
8�B
8�B
9>B
:xB
:�B
:�B
:�B
:�B
:�B
:�B
:�B
;0B
;dB
;�B
;�B
<�B
<�B
=B
=VB
=<B
=�B
>B
>�B
>�B
>�B
>�B
?cB
?cB
?}B
?�B
?�B
@ B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
AUB
B'B
B�B
B�B
B�B
B�B
B�B
BAB
CB
B�B
CB
C�B
D�B
D�B
EB
D�B
D�B
E�B
E�B
E�B
FB
E�B
E�B
FB
FYB
G+B
H�B
HKB
H�B
H�B
IB
I�B
J	B
I�B
I�B
J�B
J�B
K)B
KB
K�B
LB
L0B
LJB
L�B
L�B
MB
MPB
M�B
M�B
MjB
MB
L~B
MjB
M�B
M�B
N<B
NVB
N�B
N�B
N�B
N�B
O�B
PB
P�B
P�B
P�B
Q B
Q4B
Q4B
Q�B
Q�B
Q�B
S@B
S�B
TB
TB
T{B
T�B
UB
U�B
U�B
U�B
U�B
U�B
V9B
V�B
WYB
W�B
W�B
X+B
X_B
X�B
X�B
YKB
YB
Z7B
Z�B
Z�B
[=B
[#B
[=B
[qB
[�B
\B
\B
\)B
\CB
\�B
]B
]/B
]B
]/B
]/B
]�B
]�B
^B
^OB
_VB
_VB
_VB
_�B
_�B
_�B
`B
`�B
`�B
`�B
`�B
`�B
`�B
abB
aHB
a|B
a�B
a�B
a�B
bNB
b�B
b�B
b�B
b�B
b�B
b�B
b�B
c B
cTB
cnB
c�B
c�B
d@B
d�B
d�B
e,B
ezB
e�B
e�B
e�B
f2B
f�B
f�B
f�B
f�B
f�B
gB
gmB
gRB
g�B
h�B
h�B
iDB
i�B
iyB
i_B
i�B
jB
i�B
j0B
i�B
jB
jB
j0B
jKB
jeB
jeB
jB
j�B
j�B
j�B
j�B
j�B
j�B
k6B
k6B
kkB
k�B
k�B
l"B
l=B
lqB
lqB
l�B
l�B
m]B
mCB
m]B
m�B
nB
nB
n/B
n�B
n�B
n�B
o5B
o�B
o�B
pB
p�B
p�B
q'B
qAB
q�B
q�B
rB
r-B
rGB
r|B
raB
r�B
r�B
s3B
sMB
s�B
tB
tB
tB
tTB
t�B
uB
uB
u%B
uZB
u�B
u�B
u�B
vB
vFB
vzB
v�B
wfB
wfB
w�B
w�B
w�B
w�B
xB
w�B
xlB
x�B
y	B
y	B
y$B
y	B
y>B
yrB
yrB
y�B
y�B
y�B
y�B
y�B
zxB
z�B
z�B
z�B
z�B
z�B
{0B
{B
{B
{�B
{�B
|B
|6B
|PB
|�B
}B
}B
}VB
}�B
}�B
~B
~]B
~�B
~�B
.B
HB
HB
cB
cB
}B
}B
�B
�B
�B
�B
�B
�B
�OB
�iB
��B
��B
��B
��B
�B
�oB
��B
��B
��B
��B
��B
�B
�'B
�[B
��B
�-B
�aB
�{B
��B
��B
�gB
�gB
��B
��B
��B
��B
��B
�B
�B
�%B
�?B
�tB
��B
��B
��B
��B
�_B
�_B
�_B
��B
�zB
�zB
��B
��B
��B
��B
�B
�KB
��B
��B
��B
��B
�B
�B
�7B
�RB
��B
��B
�	B
�	B
�XB
�X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�JB�pB��B�JB�B�B�yB��B�B�B� BخB�B��B��B��B�NB��B�B��B�,B��B�*B��B�B
	BB�QB	�PB	��B	�\B	�6B	�@B	��B
�B
%zB
2�B
AoB
K�B
q'B
[=B
9rB
1AB
5�B
3B
*�B
qB
�B
"NB
%�B
 �B
"B
0B	�`B	��B	�B	�	B	уB	�}B	�<B	�B	��B	�FB	��B	��B	�mB	��B	utB	?B	�B�B��B�B�B��B�\B��B�sB��B�eB��B��B�2B��B�B�KB�YB�{By�Bw�BsMBm�Bj�Bi�BlWBt9B{�By�BtBu�B��B��B��B��B�KB��B��B��B�=B�B�=B��B�B	�B	�B	�B	�B	GB	l�B	��B	��B	��B	�B	��B	�dB	�|B	��B	�B	��B
�B
"4B
D�B
=<B
-]B
'B
)�B
>BB
OB
TaB
X�B
a�B
cB
g8B
d�B
bB
gmB
l�B
k�B
j�B
hsB
d@B
^B
[�B
Z7B
[�B
R�B
OB
NpB
VmB
V�B
UB
QhB
?}B
:�B
&B
mB
�B
�B
B
/B
�B
}B

	B
B
�B
B
�B
�B

�B
�B
�B
gB
�B
�B
�B
	B
�B
<B
�B
�B
<B
�B
:B
�B
B
oB
B
�B
4B
&B
�B
�B
hB
xB
4B
�B
�B
"B
.B
�B
�B
�B
VB
!|B
;B
B
xB
�B
�B
_B
sB
�B
�B
�B
&B
{B
�B
qB
�B
�B
�B
KB
�B
$B
uB
}B
(B
�B
�B
�B
�B
B
�B
KB

�B
~B

rB
	RB
�B
_B
�B
�B
�B
�B
�B
�B
1B
B
�B
B
YB
MB
�B
uB
 �B	�0B	�LB	��B	�B	��B	��B	�B	�lB	��B	��B
�B
�B
B
�B
UB
 OB	�B	��B	�fB	��B	�%B	�?B	�+B	��B	��B	�VB	��B	�VB	��B
oB
�B
B
'B
'B
B
 �B	��B	�HB	��B	��B	�xB	��B	�B	��B	�B	�fB	�LB	�fB	�fB	�B	�fB	�fB	�B	��B	��B	�ZB	�?B	��B	�fB	�B	��B	�B	�lB	�	B	�>B	�	B	��B	��B	�DB	��B	�B	��B	�xB	�*B	��B	��B	�rB	��B	�XB	�>B	��B	��B	��B	�B	�B	�VB	�VB	��B	�wB	��B	��B	�HB	��B	��B	��B	��B	�}B
 4B
 OB
 B
 4B
 OB
UB
B
B
 �B
 B
B
 B
uB
�B
-B
�B
GB
GB
B
�B
�B
�B
�B
�B
�B
�B
B
B
9B
mB
�B
B
B
%B
%B
%B
YB
�B
�B
�B
+B
zB
EB
B
�B
_B
zB
1B
fB
	B
	�B

�B
^B
�B
JB
0B
�B
B
PB
�B
�B
"B
�B
(B
�B
�B
�B
B
�B
�B
2B
gB
�B
�B
�B
9B
YB
�B
�B
�B
sB
�B

B
?B
B
�B
+B
�B
�B
�B
�B
�B
�B
�B
eB
KB
1B
1B
KB
KB
eB
eB
eB
B
�B
�B
�B
�B
�B
�B
�B
#B
�B
�B
B
�B
�B
B
�B
IB
IB
/B
~B
~B
�B
B
B
B
5B
�B
VB
�B
 B
 'B
 \B
!�B
!B
!-B
!-B
!-B
!�B
"NB
#�B
#�B
#:B
# B
"�B
# B
#B
"�B
"�B
"�B
#nB
#nB
$B
$�B
$�B
%FB
%�B
%�B
%�B
%�B
&2B
%�B
&B
'RB
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)_B
)�B
*B
*KB
*KB
*eB
*�B
*�B
+�B
,B
,B
,�B
,�B
,qB
,WB
,�B
-�B
.cB
.�B
/ B
/iB
/�B
0B
1B
1�B
1�B
2GB
2aB
2|B
2�B
0!B
2�B
2�B
33B
4B
4B
49B
49B
4TB
4TB
4�B
5B
5?B
5tB
5�B
5�B
6FB
6`B
6FB
6�B
6�B
6�B
7B
7�B
7�B
7�B
7�B
8�B
8�B
8�B
9>B
:xB
:�B
:�B
:�B
:�B
:�B
:�B
:�B
;0B
;dB
;�B
;�B
<�B
<�B
=B
=VB
=<B
=�B
>B
>�B
>�B
>�B
>�B
?cB
?cB
?}B
?�B
?�B
@ B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
AUB
B'B
B�B
B�B
B�B
B�B
B�B
BAB
CB
B�B
CB
C�B
D�B
D�B
EB
D�B
D�B
E�B
E�B
E�B
FB
E�B
E�B
FB
FYB
G+B
H�B
HKB
H�B
H�B
IB
I�B
J	B
I�B
I�B
J�B
J�B
K)B
KB
K�B
LB
L0B
LJB
L�B
L�B
MB
MPB
M�B
M�B
MjB
MB
L~B
MjB
M�B
M�B
N<B
NVB
N�B
N�B
N�B
N�B
O�B
PB
P�B
P�B
P�B
Q B
Q4B
Q4B
Q�B
Q�B
Q�B
S@B
S�B
TB
TB
T{B
T�B
UB
U�B
U�B
U�B
U�B
U�B
V9B
V�B
WYB
W�B
W�B
X+B
X_B
X�B
X�B
YKB
YB
Z7B
Z�B
Z�B
[=B
[#B
[=B
[qB
[�B
\B
\B
\)B
\CB
\�B
]B
]/B
]B
]/B
]/B
]�B
]�B
^B
^OB
_VB
_VB
_VB
_�B
_�B
_�B
`B
`�B
`�B
`�B
`�B
`�B
`�B
abB
aHB
a|B
a�B
a�B
a�B
bNB
b�B
b�B
b�B
b�B
b�B
b�B
b�B
c B
cTB
cnB
c�B
c�B
d@B
d�B
d�B
e,B
ezB
e�B
e�B
e�B
f2B
f�B
f�B
f�B
f�B
f�B
gB
gmB
gRB
g�B
h�B
h�B
iDB
i�B
iyB
i_B
i�B
jB
i�B
j0B
i�B
jB
jB
j0B
jKB
jeB
jeB
jB
j�B
j�B
j�B
j�B
j�B
j�B
k6B
k6B
kkB
k�B
k�B
l"B
l=B
lqB
lqB
l�B
l�B
m]B
mCB
m]B
m�B
nB
nB
n/B
n�B
n�B
n�B
o5B
o�B
o�B
pB
p�B
p�B
q'B
qAB
q�B
q�B
rB
r-B
rGB
r|B
raB
r�B
r�B
s3B
sMB
s�B
tB
tB
tB
tTB
t�B
uB
uB
u%B
uZB
u�B
u�B
u�B
vB
vFB
vzB
v�B
wfB
wfB
w�B
w�B
w�B
w�B
xB
w�B
xlB
x�B
y	B
y	B
y$B
y	B
y>B
yrB
yrB
y�B
y�B
y�B
y�B
y�B
zxB
z�B
z�B
z�B
z�B
z�B
{0B
{B
{B
{�B
{�B
|B
|6B
|PB
|�B
}B
}B
}VB
}�B
}�B
~B
~]B
~�B
~�B
.B
HB
HB
cB
cB
}B
}B
�B
�B
�B
�B
�B
�B
�OB
�iB
��B
��B
��B
��B
�B
�oB
��B
��B
��B
��B
��B
�B
�'B
�[B
��B
�-B
�aB
�{B
��B
��B
�gB
�gB
��B
��B
��B
��B
��B
�B
�B
�%B
�?B
�tB
��B
��B
��B
��B
�_B
�_B
�_B
��B
�zB
�zB
��B
��B
��B
��B
�B
�KB
��B
��B
��B
��B
�B
�B
�7B
�RB
��B
��B
�	B
�	B
�XB
�X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20221129154214  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20221129154217  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221129154218  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221129154218                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221129154218  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221129154218  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20221129155713                      G�O�G�O�G�O�                