CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-07-08T18:46:59Z creation;2023-07-08T18:47:02Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20230708184659  20230708185707  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�8��ò�1   @�8�g(��@1d���S��cY�E��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A��A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B��B��B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B�  B�  B�  B�ffB�B���B�  C   C�fC  C  C  C
  C  C  C  C  C�C�C�C  C  C  C �C"  C#�fC%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>��C?��CA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�<�DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր Dּ�D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�3D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @fg@|��@�ffA ��A33A?33A_33A33A���A���A���A�fgAϙ�Aߙ�ABfgBfgBfgB��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��B�L�B��fB�� B��3B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB�L�B��fB��fB��fB��fB�L�B� B��3B��fB��fCٙC�3C�3C�3C	�3C�3C�3C�3C�3C�C�C�C�3C�3C�3C �C!�3C#ٙC%ٙC'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C>��C?��CAٙCC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Ct�Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC�gC�gC���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC�gC�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�;3D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD���D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�;3D�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fDֻ3D��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��D�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�3D��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��3D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�>fD�k31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A��A���A��A�چA��dA��A��`A���A��A���A��A��fA��"A��WA�ܒA��pA�\�A�T,A��mAʑ4A�^A�+�A�(�A�2aA�33A�SA��A��.A��|Aɽ<A���AɈ1A��A�9$A��KA�9�A�#nA�$�A�OA���A��A�͟A��7A���A���A���A�(�A�<6A�%FA�.A��A��YA�^jA�V9A�ÖA�~(A�A�L�A�rGA���A���A�ӏA���A�D3A�1�A�X�A��#A��TA�V9A���A�N<A��A��:A�:�A��oA���A�
=A�<6A��
A�֡A��%A��nA��bA�ӏA�_;A��UA���A�>�A�$@A��A�`A|�@AzE�AsA AqخAl0�Ag�AdQAa=A_\)A\��AZ�AX$tAU"�AO*�AK˒AHqAE6�AC�#A@��A=�uA;�.A:ZA6�LA5O�A3ѷA2��A0CA/{�A.�EA,�dA+�A*=�A)��A)!A'��A&�A%��A$�fA#�]A"3�A C�AuA��A�}A��A�KA��A
�Ad�A�AU�A=qA�eA�A��A�A:*Aw2A�,AA�ARTA��A��A@�ACA
c�A	�ACA�EA-wAo A
=AH�A��A�fA�A�hA ��A �A$tA�AB�A m]@���@�@���@���@���@���@�Z@��@��@�(�@�@�;d@�`�@��7@�$@��@���@�4@�X�@�+@��o@�U2@�c @���@���@�:�@��@�2�@��@�I�@��@�h@�a�@�u�@��@��@�Ft@�l"@�U2@��@��y@���@�r�@��"@�z�@�%�@���@���@��@�9X@��m@�`B@�!@�p;@�
=@�Y�@�A@�;�@���@�t@��@�o @�\�@���@�}�@�K^@�9�@ង@�6@��@���@��m@�ԕ@ۏ�@�F@��@�3�@��@ي�@�0�@��[@�_�@��@�@�<�@�V@��@Ԭ@�x�@�Z�@�@���@��
@�Vm@� \@��@��)@ЋD@�l"@�:�@��@���@Ϣ�@�}�@�a@��@��@�+�@��B@̗�@�R�@��@ˬq@�&@ʚ�@�4@��@�� @��#@ɂ�@�G�@�RT@Ɋ�@ɤ@@ɚk@�|@�\)@�'�@�$�@ǝ�@�E9@��@���@���@�@�|�@���@ĭ�@�4n@���@�m]@�@�e@��*@�l�@���@�|�@��@��q@��k@�:�@��@�7�@�U2@��D@�خ@�Z�@��	@�	�@���@�T�@�!�@��@�e�@�x�@�N<@��@���@��	@�~(@��m@��=@��a@��o@��w@�`B@���@���@��'@��@��	@��@�X�@���@�-@��@�y�@�2a@��6@�($@��X@�/�@�q@�e�@�C�@�G@��:@�H�@��@���@��o@�e�@�PH@�4n@��@���@���@�)_@��@��B@�͟@��x@�\�@�E�@��@���@���@�q@���@��#@���@���@�Mj@�+@��@���@�1�@�  @���@��0@�o�@��@���@��I@�<�@�~�@��@�H�@���@��@���@�4n@��o@��g@�/@��F@�;�@���@��8@���@�0U@��9@�@��@�<6@��f@���@���@���@��@�3�@��+@��@��"@�T�@��@���@�q@�\�@��@��@���@�j�@�J#@�K�@��@��u@�|�@�]d@�O@���@�~�@�2a@���@��}@��@�$@���@��{@��4@���@���@�O@��@�<�@���@��;@�O@�/�@���@��@�V�@�]d@��@��9@���@�<�@��Z@���@���@���@��@�ϫ@���@���@���@��^@��#@�|�@�^�@�Z�@�=�@�O@�N<@�/@���@���@�'�@��c@�K^@���@��k@�Dg@� \@��@��@��<@��z@�6�@���@���@��d@��	@�Mj@�$t@���@�~(@�%�@��Q@��k@��~@�%@���@���@�Z@�"h@��Q@��{@�_p@�;d@���@��}@�PH@��-@���@�a�@�;d@�.I@�q@��@�K�@�O�@��@�Y@��2@�8@���@���@�{@�b@�?�@�1�@�-�@�$�@��@��@�Z�@�ѷ@� �@�
@ݘ@��@��@��@ݘ@~�@~Ta@~�+@~�1@~�L@~��@}ϫ@}+�@|�$@{�m@{�*@{{J@{l�@z�H@z&�@y�C@y:�@y�@x�/@x�@x�o@w�:@vz@vC�@u�)@u��@t�@s�k@r�B@r��@rZ�@q�@q��@q2a@q�@p�K@p�K@p��@pĜ@p��@p�D@p�Y@poi@p_@p4n@o��@o�@n��@m�z@m�C@m��@l~@kP�@k/�@k)_@k!-@k�@k�@k@k�@kY@k@j��@j&�@j�@i�Z@iT�@i�@h��@h�e@h��@hK^@h!@gy�@f��@f��@fl�@fZ�@e�.@e�=@e�D@f_@e�@e��@e��@ee,@e*0@d�P@d�?@d�@du�@d*�@cK�@b��@bR�@a5�@`֡@`��@`�@`h�@`!@_+@^҉@^��@^�,@^�m@^�1@^C�@^O@]��@]�@]Vm@]�@\�@\'R@[�k@[1�@Z��@Z:*@ZJ@Y�@Y�-@Y+@X�K@X�p@X�@Xz�@X'R@WU�@V��@V�}@V�@Vu%@V&�@VJ@U��@U�H@Uhs@U7L@U%@T�K@T��@T�@T�/@T��@T(�@S�m@S��@Sa@R�@R�!@R��@Rv�@RYK@R3�@Q�@Q@P[�@P�@O�@Na|@Ne@M�)@M�t@Mo @M+@L��@LH@K��@Kj�@J�H@J}V@I��@I��@Ihs@H�)@He�@G�;@F�1@F@�@E�T@Ea�@D�@D�D@Doi@D'R@C��@CH�@C8@B��@Bff@B�@A�Z@A�d@A�-@AX@A&�@@��@@/�@?��@?�$@?o�@>�@>��@>E�@>�@=��@=�@=�>@=��@<�@<x@;�+@;��@;��@;�@;C@:�c@:�@:v�@9��@97L@8�j@8w�@8N�@8b@7�K@7�4@78@6��@6��@6�@5ԕ@5��@5	l@4�@3�
@3��@3ƨ@3�@3�V@3a@3)_@2ȴ@2�A@2u@1�"@1x�@1j@1`B@1S&@1:�@1�@0��@0�5@0��@0��@0��@0�e@0��@0Q�@/�+@/�f@.�@.~�@.Ov@.0U@-�@-�X@-�=@-��@-a�@-?}@-@-�@,�p@,�9@,�O@,�Y@+��@+S�@+'�@*��@*��@*��@*�@*��@*��@*}V@*\�@*3�@*e@*	@*O@*	@)�o@)�d@)��@)��@)�'@)��@)p�@)f�@)Y�@(�f@'�*@'�@&�@&�1@&n�@&\�@%�@%w2@%\�@%Dg@%�@$��@$�Y@$,=@#v`@#+@#(@"��@"1�@!��@!��@!Dg@ ��@ I�@ !@ b@� @@O@�@~�@h
@@Vm@<6@8�@2a@&�@��@y>@2�@ݘ@��@�:@�@W?@�@�c@�"@�@�y@�\@V@��@�7@�@��@_@Ft@4n@�@A�@�@��@��@s�@YK@B[@	@��@ϫ@�M@�@�@��@��@��@u�@]d@U2@PH@H@"h@�}@��@�P@�s@s�@#:@�>@��@s�@@@�5@�v@֡@�E@��@��@D�@"h@�m@�@�q@�k@��@n/@Z�@�@ߤ@�F@n�@E�@1�@)�@&�@#:@O@�@�@u@�@�>@�T@�@�d@��@k�@��@�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A��A���A��A�چA��dA��A��`A���A��A���A��A��fA��"A��WA�ܒA��pA�\�A�T,A��mAʑ4A�^A�+�A�(�A�2aA�33A�SA��A��.A��|Aɽ<A���AɈ1A��A�9$A��KA�9�A�#nA�$�A�OA���A��A�͟A��7A���A���A���A�(�A�<6A�%FA�.A��A��YA�^jA�V9A�ÖA�~(A�A�L�A�rGA���A���A�ӏA���A�D3A�1�A�X�A��#A��TA�V9A���A�N<A��A��:A�:�A��oA���A�
=A�<6A��
A�֡A��%A��nA��bA�ӏA�_;A��UA���A�>�A�$@A��A�`A|�@AzE�AsA AqخAl0�Ag�AdQAa=A_\)A\��AZ�AX$tAU"�AO*�AK˒AHqAE6�AC�#A@��A=�uA;�.A:ZA6�LA5O�A3ѷA2��A0CA/{�A.�EA,�dA+�A*=�A)��A)!A'��A&�A%��A$�fA#�]A"3�A C�AuA��A�}A��A�KA��A
�Ad�A�AU�A=qA�eA�A��A�A:*Aw2A�,AA�ARTA��A��A@�ACA
c�A	�ACA�EA-wAo A
=AH�A��A�fA�A�hA ��A �A$tA�AB�A m]@���@�@���@���@���@���@�Z@��@��@�(�@�@�;d@�`�@��7@�$@��@���@�4@�X�@�+@��o@�U2@�c @���@���@�:�@��@�2�@��@�I�@��@�h@�a�@�u�@��@��@�Ft@�l"@�U2@��@��y@���@�r�@��"@�z�@�%�@���@���@��@�9X@��m@�`B@�!@�p;@�
=@�Y�@�A@�;�@���@�t@��@�o @�\�@���@�}�@�K^@�9�@ង@�6@��@���@��m@�ԕ@ۏ�@�F@��@�3�@��@ي�@�0�@��[@�_�@��@�@�<�@�V@��@Ԭ@�x�@�Z�@�@���@��
@�Vm@� \@��@��)@ЋD@�l"@�:�@��@���@Ϣ�@�}�@�a@��@��@�+�@��B@̗�@�R�@��@ˬq@�&@ʚ�@�4@��@�� @��#@ɂ�@�G�@�RT@Ɋ�@ɤ@@ɚk@�|@�\)@�'�@�$�@ǝ�@�E9@��@���@���@�@�|�@���@ĭ�@�4n@���@�m]@�@�e@��*@�l�@���@�|�@��@��q@��k@�:�@��@�7�@�U2@��D@�خ@�Z�@��	@�	�@���@�T�@�!�@��@�e�@�x�@�N<@��@���@��	@�~(@��m@��=@��a@��o@��w@�`B@���@���@��'@��@��	@��@�X�@���@�-@��@�y�@�2a@��6@�($@��X@�/�@�q@�e�@�C�@�G@��:@�H�@��@���@��o@�e�@�PH@�4n@��@���@���@�)_@��@��B@�͟@��x@�\�@�E�@��@���@���@�q@���@��#@���@���@�Mj@�+@��@���@�1�@�  @���@��0@�o�@��@���@��I@�<�@�~�@��@�H�@���@��@���@�4n@��o@��g@�/@��F@�;�@���@��8@���@�0U@��9@�@��@�<6@��f@���@���@���@��@�3�@��+@��@��"@�T�@��@���@�q@�\�@��@��@���@�j�@�J#@�K�@��@��u@�|�@�]d@�O@���@�~�@�2a@���@��}@��@�$@���@��{@��4@���@���@�O@��@�<�@���@��;@�O@�/�@���@��@�V�@�]d@��@��9@���@�<�@��Z@���@���@���@��@�ϫ@���@���@���@��^@��#@�|�@�^�@�Z�@�=�@�O@�N<@�/@���@���@�'�@��c@�K^@���@��k@�Dg@� \@��@��@��<@��z@�6�@���@���@��d@��	@�Mj@�$t@���@�~(@�%�@��Q@��k@��~@�%@���@���@�Z@�"h@��Q@��{@�_p@�;d@���@��}@�PH@��-@���@�a�@�;d@�.I@�q@��@�K�@�O�@��@�Y@��2@�8@���@���@�{@�b@�?�@�1�@�-�@�$�@��@��@�Z�@�ѷ@� �@�
@ݘ@��@��@��@ݘ@~�@~Ta@~�+@~�1@~�L@~��@}ϫ@}+�@|�$@{�m@{�*@{{J@{l�@z�H@z&�@y�C@y:�@y�@x�/@x�@x�o@w�:@vz@vC�@u�)@u��@t�@s�k@r�B@r��@rZ�@q�@q��@q2a@q�@p�K@p�K@p��@pĜ@p��@p�D@p�Y@poi@p_@p4n@o��@o�@n��@m�z@m�C@m��@l~@kP�@k/�@k)_@k!-@k�@k�@k@k�@kY@k@j��@j&�@j�@i�Z@iT�@i�@h��@h�e@h��@hK^@h!@gy�@f��@f��@fl�@fZ�@e�.@e�=@e�D@f_@e�@e��@e��@ee,@e*0@d�P@d�?@d�@du�@d*�@cK�@b��@bR�@a5�@`֡@`��@`�@`h�@`!@_+@^҉@^��@^�,@^�m@^�1@^C�@^O@]��@]�@]Vm@]�@\�@\'R@[�k@[1�@Z��@Z:*@ZJ@Y�@Y�-@Y+@X�K@X�p@X�@Xz�@X'R@WU�@V��@V�}@V�@Vu%@V&�@VJ@U��@U�H@Uhs@U7L@U%@T�K@T��@T�@T�/@T��@T(�@S�m@S��@Sa@R�@R�!@R��@Rv�@RYK@R3�@Q�@Q@P[�@P�@O�@Na|@Ne@M�)@M�t@Mo @M+@L��@LH@K��@Kj�@J�H@J}V@I��@I��@Ihs@H�)@He�@G�;@F�1@F@�@E�T@Ea�@D�@D�D@Doi@D'R@C��@CH�@C8@B��@Bff@B�@A�Z@A�d@A�-@AX@A&�@@��@@/�@?��@?�$@?o�@>�@>��@>E�@>�@=��@=�@=�>@=��@<�@<x@;�+@;��@;��@;�@;C@:�c@:�@:v�@9��@97L@8�j@8w�@8N�@8b@7�K@7�4@78@6��@6��@6�@5ԕ@5��@5	l@4�@3�
@3��@3ƨ@3�@3�V@3a@3)_@2ȴ@2�A@2u@1�"@1x�@1j@1`B@1S&@1:�@1�@0��@0�5@0��@0��@0��@0�e@0��@0Q�@/�+@/�f@.�@.~�@.Ov@.0U@-�@-�X@-�=@-��@-a�@-?}@-@-�@,�p@,�9@,�O@,�Y@+��@+S�@+'�@*��@*��@*��@*�@*��@*��@*}V@*\�@*3�@*e@*	@*O@*	@)�o@)�d@)��@)��@)�'@)��@)p�@)f�@)Y�@(�f@'�*@'�@&�@&�1@&n�@&\�@%�@%w2@%\�@%Dg@%�@$��@$�Y@$,=@#v`@#+@#(@"��@"1�@!��@!��@!Dg@ ��@ I�@ !@ b@� @@O@�@~�@h
@@Vm@<6@8�@2a@&�@��@y>@2�@ݘ@��@�:@�@W?@�@�c@�"@�@�y@�\@V@��@�7@�@��@_@Ft@4n@�@A�@�@��@��@s�@YK@B[@	@��@ϫ@�M@�@�@��@��@��@u�@]d@U2@PH@H@"h@�}@��@�P@�s@s�@#:@�>@��@s�@@@�5@�v@֡@�E@��@��@D�@"h@�m@�@�q@�k@��@n/@Z�@�@ߤ@�F@n�@E�@1�@)�@&�@#:@O@�@�@u@�@�>@�T@�@�d@��@k�@��@�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
A�B
A�B
A�B
A�B
BB
A�B
BB
A�B
A�B
A�B
A�B
B'B
BB
A�B
BB
C{B
D3B
M�B
cTB
~(B
�xB
��B
��B
�<B
� B
�uB
��B
��B
��B
��B
��B
�VB
�B
�QB
�9B
��B
z�B
w�B
Z7B
=B
8B
L~B
]�B
c:B
j0B
xRB
��B
��B
�B
�%B}B�B4B0UB$�B�B(BdBKB*B
B'B
�6B
��B�B"�B�B�B1�B6zB7B.�B'�B �B�B�BB�B
�IB
�B
y�B
N�B
:�B
+�B
yB
}B
�B	��B	��B	��B	�B	��B	z*B	p�B	R:B	H�B	.B	"hB	 �B	?B	B	PB		�B	�B�[B�vBϑB��B��B��B��B�?B�uB�B�5B�yB�pB��B�dB�nB��B��B�qB��B�FB��B��B�SB��B��B�YB��B��B��B�+B��B�,B�zB�B��B� B��B��B�tB��B�B�qB�cB��B�B��B��B��BуB��B��B�B�B�B�B�cB��B�B�B�B��B�}B��B	}B	�B	+kB	4nB	M6B	_�B	c�B	b�B	c�B	d�B	e�B	d@B	]�B	\�B	^�B	d�B	t�B	vFB	|B	}<B	y�B	x�B	� B	��B	}�B	}qB	��B	�zB	�SB	�jB	��B	�NB	��B	��B	��B	�,B	�vB	�jB	�B	�}B	��B	��B	�HB	�4B	�8B	��B	��B	�B	� B	�aB	�B	��B	��B	ĶB	ÖB	��B	�:B	�{B	ӏB	��B	�B	��B	ϫB	ٚB	�eB	ںB	ٴB	��B	��B	�TB	��B	��B	�-B	یB	�mB	�BB	ɺB	�+B	��B	ĜB	�fB	�B	��B	ʦB	͟B	͟B	�B	̳B	�~B	˒B	ʦB	ɠB	��B	�?B	�B	�fB	�tB	�[B	�;B	��B	��B	ĜB	ƎB	�lB	��B	�#B	�xB	��B	�xB	�xB	�^B	˒B	�xB	�^B	��B	��B	�XB	��B	�PB	��B	�B	ϫB	ΥB	̳B	�~B	�~B	�B	�BB	�B	�.B	�B	��B	��B	�{B	ԯB	�B	��B	�:B	�B	�}B	�bB	��B	�B	�}B	�}B	ЗB	��B	� B	� B	��B	ԕB	�B	�{B	�{B	ԕB	�B	�B	��B	רB	�?B	�B	֡B	�sB	�B	ڠB	޸B	�@B	�LB	�B	��B	�
B	�2B	�TB	�B	�B	�B	�B	��B	�B	��B	��B	�B	�B	�B	�XB	��B	�B	�B	��B	�B	�wB	�B	�B	�AB	��B	��B	�B	�B	�B	��B	�TB	�B	�%B	��B	�`B	��B	��B	�2B	��B	��B	��B	�B	�B	�fB	��B	��B	�rB	�rB	�>B	��B	�B	�DB	�0B	�dB	��B	��B	��B	��B	��B	�BB	��B	��B	�cB
 OB
oB
�B
�B
GB
�B
B
�B
B
�B
�B
B
tB
?B
�B
B
MB
�B
�B
B
�B
�B
�B
�B
�B
�B
B
�B
?B
�B
+B
zB
B
�B

rB
�B
0B
~B
�B
�B
�B
�B
�B
<B
B
�B
�B
�B
}B
�B
&B
�B
�B
�B
B
�B
�B
�B
�B
�B
$B
�B
�B
KB
�B
)B
�B
�B
)B
jB
jB
�B
�B
B
]B
�B
�B
	B
�B
OB
�B
�B
;B
�B
 �B
!-B
$B
%�B
%�B
$�B
�B
 �B
$&B
$�B
%,B
%�B
'mB
)*B
)*B
)*B
(�B
(XB
%�B
$�B
#TB
!�B
!bB
!-B
#nB
$�B
$�B
$tB
$�B
%,B
$�B
$ZB
$B
#�B
#�B
$B
$tB
#�B
$ZB
$@B
$B
#�B
%zB
%zB
&B
&B
&2B
&�B
&�B
&�B
&�B
'8B
'8B
'�B
)B
)B
)_B
)DB
)DB
)yB
,�B
/ B
0�B
1AB
4�B
4TB
5�B
2|B
/�B
-�B
/�B
1�B
1�B
2�B
3�B
3�B
4nB
3hB
2�B
2GB
1�B
2�B
2�B
3hB
3�B
5tB
5�B
5�B
72B
;B
<�B
>wB
?�B
?�B
@B
@�B
@�B
@�B
@�B
AB
@�B
@�B
@�B
@iB
@�B
AoB
BB
A�B
A B
@�B
@�B
@�B
?cB
>�B
?�B
@iB
@�B
@OB
@B
@�B
B'B
CB
CGB
CB
B�B
C-B
C�B
DB
DMB
D3B
E9B
EB
D�B
C�B
B�B
CGB
ESB
E�B
D�B
EB
EB
EB
E9B
EmB
E�B
E�B
E�B
FB
F�B
F�B
F�B
GB
G+B
F�B
F�B
F�B
F�B
FtB
E�B
E�B
EmB
F?B
F%B
FB
F�B
G�B
J=B
LB
LJB
L�B
LJB
L0B
K�B
K�B
K�B
KDB
KDB
J�B
J=B
H�B
H1B
GzB
F�B
F�B
GB
GB
FYB
F%B
E�B
E�B
F�B
GB
GB
F�B
F�B
F�B
GzB
G_B
GzB
G_B
F�B
F�B
F�B
F�B
FtB
FtB
F?B
FtB
F�B
F�B
GEB
G�B
HB
H1B
IB
I�B
J	B
JrB
J�B
K^B
KDB
KxB
K�B
L~B
L�B
L�B
M�B
OB
N�B
O�B
O�B
K�B
O�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
RoB
R�B
R�B
RTB
R B
R�B
S@B
S�B
T,B
TaB
TaB
T�B
T�B
U2B
VB
V�B
V�B
W
B
WsB
WYB
W?B
X�B
X�B
Y1B
Y�B
Z�B
[#B
[#B
[�B
\B
\xB
\]B
]/B
]dB
]�B
]�B
]�B
]�B
^5B
^B
^�B
^�B
_B
_VB
_;B
_�B
_�B
`'B
`vB
`vB
`vB
`\B
`\B
a-B
a�B
a�B
a�B
a�B
a|B
bhB
bNB
b4B
b�B
b�B
c:B
c�B
c�B
dB
d@B
d�B
d�B
d�B
eB
eFB
e�B
e�B
e�B
fLB
gB
g8B
gRB
g8B
gRB
gmB
g�B
g�B
g�B
h
B
h�B
iB
h�B
iB
i*B
iB
i*B
i_B
i�B
i_B
i�B
i�B
i�B
i�B
iDB
i�B
i�B
jeB
kB
kB
kQB
kQB
k�B
k�B
k�B
k�B
lB
k�B
l"B
l"B
l�B
lqB
l=B
l�B
m)B
mwB
m�B
m�B
m�B
m�B
m�B
nIB
n/B
n/B
ncB
n�B
n�B
n�B
n}B
n}B
n�B
n�B
n�B
n�B
n�B
n�B
o B
n�B
n�B
oOB
p�B
qB
qAB
q[B
qvB
q[B
q�B
r-B
r-B
r-B
rGB
r|B
r|B
r�B
sMB
sMB
shB
shB
s3B
s3B
r�B
r�B
shB
s�B
s�B
s�B
tB
t�B
u%B
uZB
u%B
u�B
vzB
vzB
vzB
v�B
vzB
v�B
w2B
w�B
w�B
xRB
xRB
x�B
x�B
y>B
y�B
z*B
z^B
z�B
{JB
{dB
{�B
{�B
|B
|�B
}<B
}<B
}<B
}�B
~B
~�B
~wB
~�B
~�B
~�B
~�B
B
cB
.B
�B
� B
�4B
�iB
��B
��B
��B
��B
��B
��B
��B
��B
�;B
� B
� B
�'B
�AB
��B
��B
��B
��B
�[B
�uB
�uB
�[B
�[B
��B
�uB
��B
�-B
�GB
��B
��B
��B
��B
��B
��B
��B
�3B
�gB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�mB
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
A�B
A�B
A�B
A�B
BB
A�B
BB
A�B
A�B
A�B
A�B
B'B
BB
A�B
BB
C{B
D3B
M�B
cTB
~(B
�xB
��B
��B
�<B
� B
�uB
��B
��B
��B
��B
��B
�VB
�B
�QB
�9B
��B
z�B
w�B
Z7B
=B
8B
L~B
]�B
c:B
j0B
xRB
��B
��B
�B
�%B}B�B4B0UB$�B�B(BdBKB*B
B'B
�6B
��B�B"�B�B�B1�B6zB7B.�B'�B �B�B�BB�B
�IB
�B
y�B
N�B
:�B
+�B
yB
}B
�B	��B	��B	��B	�B	��B	z*B	p�B	R:B	H�B	.B	"hB	 �B	?B	B	PB		�B	�B�[B�vBϑB��B��B��B��B�?B�uB�B�5B�yB�pB��B�dB�nB��B��B�qB��B�FB��B��B�SB��B��B�YB��B��B��B�+B��B�,B�zB�B��B� B��B��B�tB��B�B�qB�cB��B�B��B��B��BуB��B��B�B�B�B�B�cB��B�B�B�B��B�}B��B	}B	�B	+kB	4nB	M6B	_�B	c�B	b�B	c�B	d�B	e�B	d@B	]�B	\�B	^�B	d�B	t�B	vFB	|B	}<B	y�B	x�B	� B	��B	}�B	}qB	��B	�zB	�SB	�jB	��B	�NB	��B	��B	��B	�,B	�vB	�jB	�B	�}B	��B	��B	�HB	�4B	�8B	��B	��B	�B	� B	�aB	�B	��B	��B	ĶB	ÖB	��B	�:B	�{B	ӏB	��B	�B	��B	ϫB	ٚB	�eB	ںB	ٴB	��B	��B	�TB	��B	��B	�-B	یB	�mB	�BB	ɺB	�+B	��B	ĜB	�fB	�B	��B	ʦB	͟B	͟B	�B	̳B	�~B	˒B	ʦB	ɠB	��B	�?B	�B	�fB	�tB	�[B	�;B	��B	��B	ĜB	ƎB	�lB	��B	�#B	�xB	��B	�xB	�xB	�^B	˒B	�xB	�^B	��B	��B	�XB	��B	�PB	��B	�B	ϫB	ΥB	̳B	�~B	�~B	�B	�BB	�B	�.B	�B	��B	��B	�{B	ԯB	�B	��B	�:B	�B	�}B	�bB	��B	�B	�}B	�}B	ЗB	��B	� B	� B	��B	ԕB	�B	�{B	�{B	ԕB	�B	�B	��B	רB	�?B	�B	֡B	�sB	�B	ڠB	޸B	�@B	�LB	�B	��B	�
B	�2B	�TB	�B	�B	�B	�B	��B	�B	��B	��B	�B	�B	�B	�XB	��B	�B	�B	��B	�B	�wB	�B	�B	�AB	��B	��B	�B	�B	�B	��B	�TB	�B	�%B	��B	�`B	��B	��B	�2B	��B	��B	��B	�B	�B	�fB	��B	��B	�rB	�rB	�>B	��B	�B	�DB	�0B	�dB	��B	��B	��B	��B	��B	�BB	��B	��B	�cB
 OB
oB
�B
�B
GB
�B
B
�B
B
�B
�B
B
tB
?B
�B
B
MB
�B
�B
B
�B
�B
�B
�B
�B
�B
B
�B
?B
�B
+B
zB
B
�B

rB
�B
0B
~B
�B
�B
�B
�B
�B
<B
B
�B
�B
�B
}B
�B
&B
�B
�B
�B
B
�B
�B
�B
�B
�B
$B
�B
�B
KB
�B
)B
�B
�B
)B
jB
jB
�B
�B
B
]B
�B
�B
	B
�B
OB
�B
�B
;B
�B
 �B
!-B
$B
%�B
%�B
$�B
�B
 �B
$&B
$�B
%,B
%�B
'mB
)*B
)*B
)*B
(�B
(XB
%�B
$�B
#TB
!�B
!bB
!-B
#nB
$�B
$�B
$tB
$�B
%,B
$�B
$ZB
$B
#�B
#�B
$B
$tB
#�B
$ZB
$@B
$B
#�B
%zB
%zB
&B
&B
&2B
&�B
&�B
&�B
&�B
'8B
'8B
'�B
)B
)B
)_B
)DB
)DB
)yB
,�B
/ B
0�B
1AB
4�B
4TB
5�B
2|B
/�B
-�B
/�B
1�B
1�B
2�B
3�B
3�B
4nB
3hB
2�B
2GB
1�B
2�B
2�B
3hB
3�B
5tB
5�B
5�B
72B
;B
<�B
>wB
?�B
?�B
@B
@�B
@�B
@�B
@�B
AB
@�B
@�B
@�B
@iB
@�B
AoB
BB
A�B
A B
@�B
@�B
@�B
?cB
>�B
?�B
@iB
@�B
@OB
@B
@�B
B'B
CB
CGB
CB
B�B
C-B
C�B
DB
DMB
D3B
E9B
EB
D�B
C�B
B�B
CGB
ESB
E�B
D�B
EB
EB
EB
E9B
EmB
E�B
E�B
E�B
FB
F�B
F�B
F�B
GB
G+B
F�B
F�B
F�B
F�B
FtB
E�B
E�B
EmB
F?B
F%B
FB
F�B
G�B
J=B
LB
LJB
L�B
LJB
L0B
K�B
K�B
K�B
KDB
KDB
J�B
J=B
H�B
H1B
GzB
F�B
F�B
GB
GB
FYB
F%B
E�B
E�B
F�B
GB
GB
F�B
F�B
F�B
GzB
G_B
GzB
G_B
F�B
F�B
F�B
F�B
FtB
FtB
F?B
FtB
F�B
F�B
GEB
G�B
HB
H1B
IB
I�B
J	B
JrB
J�B
K^B
KDB
KxB
K�B
L~B
L�B
L�B
M�B
OB
N�B
O�B
O�B
K�B
O�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
RoB
R�B
R�B
RTB
R B
R�B
S@B
S�B
T,B
TaB
TaB
T�B
T�B
U2B
VB
V�B
V�B
W
B
WsB
WYB
W?B
X�B
X�B
Y1B
Y�B
Z�B
[#B
[#B
[�B
\B
\xB
\]B
]/B
]dB
]�B
]�B
]�B
]�B
^5B
^B
^�B
^�B
_B
_VB
_;B
_�B
_�B
`'B
`vB
`vB
`vB
`\B
`\B
a-B
a�B
a�B
a�B
a�B
a|B
bhB
bNB
b4B
b�B
b�B
c:B
c�B
c�B
dB
d@B
d�B
d�B
d�B
eB
eFB
e�B
e�B
e�B
fLB
gB
g8B
gRB
g8B
gRB
gmB
g�B
g�B
g�B
h
B
h�B
iB
h�B
iB
i*B
iB
i*B
i_B
i�B
i_B
i�B
i�B
i�B
i�B
iDB
i�B
i�B
jeB
kB
kB
kQB
kQB
k�B
k�B
k�B
k�B
lB
k�B
l"B
l"B
l�B
lqB
l=B
l�B
m)B
mwB
m�B
m�B
m�B
m�B
m�B
nIB
n/B
n/B
ncB
n�B
n�B
n�B
n}B
n}B
n�B
n�B
n�B
n�B
n�B
n�B
o B
n�B
n�B
oOB
p�B
qB
qAB
q[B
qvB
q[B
q�B
r-B
r-B
r-B
rGB
r|B
r|B
r�B
sMB
sMB
shB
shB
s3B
s3B
r�B
r�B
shB
s�B
s�B
s�B
tB
t�B
u%B
uZB
u%B
u�B
vzB
vzB
vzB
v�B
vzB
v�B
w2B
w�B
w�B
xRB
xRB
x�B
x�B
y>B
y�B
z*B
z^B
z�B
{JB
{dB
{�B
{�B
|B
|�B
}<B
}<B
}<B
}�B
~B
~�B
~wB
~�B
~�B
~�B
~�B
B
cB
.B
�B
� B
�4B
�iB
��B
��B
��B
��B
��B
��B
��B
��B
�;B
� B
� B
�'B
�AB
��B
��B
��B
��B
�[B
�uB
�uB
�[B
�[B
��B
�uB
��B
�-B
�GB
��B
��B
��B
��B
��B
��B
��B
�3B
�gB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�mB
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230708184657  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230708184659  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230708184702  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230708184702                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230708184703  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230708184703  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230708185707                      G�O�G�O�G�O�                