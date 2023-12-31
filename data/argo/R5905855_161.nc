CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-07-13T09:47:26Z creation;2023-07-13T09:47:27Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230713094726  20230803081500  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�:�4��1   @�:�>2�@0;��S���c���E�1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @,��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C33C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB�CD  CF  CH  CJ  CK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz�C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"fD"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY�fDZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@)��@|��@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BP33BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��B��fB��fB��3B��fB��fB��fB��fB��fB��fB��fB��fB��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C&fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C@�CB�CC�3CE�3CG�3CI�3CKٙCM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cz�C|�C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D"3D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY�3DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aϟ�Aϟ�AϠ�AϢ4AϢ�Aϣ�AϥFAϥ�AϦAϩ*AϪeAϤ�Aϛ�Aϛ�AϚ7Aϖ�AϚ�AϜAϟ�AϞ�Aϟ!AϠ�AϘ�AϠ�Aϟ�Aϖ�Aσ{A�s�A�S[A�F�A�DgA�@�A�A�A�A�A�A�A�B�A�A A�=�A�?�A�=<A�/OA�w�Aƫ6A�W
A�jKA��A�hsA�_A��{A���A��A���A�	�A�:^A���A� A�RTA��TA��A��TA�+6A�#�A�� A�XyA�'A��A���A�e�A�P�A��A��A��iA��2A�cA�S�A�m�A�5A��A�G�A�y	A�eA�fA���A�yrA��>A�9$A�A�A�r�A���A���A��eA�b�A���A�5�A��A�o�A�*�A�-�AQA}�6A{��Ay֡At*�Ao�Al]dAg  Ae��A`��A].�AZu�AX�AW[�AT2aAQ�XAP[�AO9�AM�AKrGAI��AH�AG;dADVAB��AAcA@��A@��A@jA@,�A>c A;ɆA:�PA6҉A5v`A2R�A/�FA.�'A-v�A+E�A)��A)8�A(zxA(R�A'�gA'�A%��A$��A"s�A �}A!ѷA"}VA"ѷA"�A"�A�FA_Am�A��AiDA�A�AݘA�+A��A-wA�-A��AMA#:A(�AOvA��A�[A��A�zA�A��AA�A(�A��A�?A>�A�A��Ap�A�A�kAv`A7�A��A�A�#A��A!�A�hA0UA
A	��A	�A	I�A��A�A�A��A��AXyA��A�4AخA��A�MA$tA�A�A�6A5�A2�A �"@���@��o@���@�?@�u%@���@���@�~�@�o@�$�@���@�bN@�:�@��'@�o@��
@��n@�_@�g8@�4@��2@�M@��d@�@�x@坲@��@�kQ@��o@��@�V@�G�@ߠ'@އ+@ݗ$@�+k@�n/@�J�@�Ĝ@׫�@ؓu@�O@�!�@��@�|�@���@�\�@؜x@،�@�\�@ד@��y@��@��H@ӶF@�S&@�\�@�$t@р4@З�@Ї+@Б�@�_@ϟV@Ό@Ό@Τ�@β�@ή}@�ff@��@͇�@�W?@�J�@�#�@̡b@�,=@��;@˷@�A @ʋD@�[�@� �@�t�@�	l@�ی@ȆY@ȅ�@ǆ�@�@@ƽ<@��@��m@�=@�5�@�!-@�#�@İ�@�s�@��H@Ĩ�@�r�@��]@�IR@º�@�/�@��@�`B@�`�@�9�@�S@��@��p@��\@�d�@�~@�خ@�0�@�\�@�ϫ@�c@�4�@��/@���@�e,@�9�@��@�	�@��F@��E@��_@� �@�ƨ@��c@�Z@���@��3@�\�@��@��`@�}V@�\)@�(�@�+�@�2a@�J�@�[W@�!�@�ߤ@�Ov@��[@��P@�a@�@��}@�C�@�@�*�@�e,@���@��d@�Y�@��@�ff@�7@��K@��[@��@��@��@��z@�a@���@�7@�7@��@�c@�8�@�;@��]@���@��@���@�H�@�@��&@��@�4�@�E�@��N@�K�@��@���@��X@�z�@�H�@�/�@��@���@�4@���@��U@��@�_@�	�@�a�@�͟@���@�{@��@�\�@��@��@�4n@�@�q@�d�@���@���@�N<@��f@���@��b@�oi@�_@�	@��T@�0�@���@��D@�E�@��@�˒@��@���@�1'@���@��"@�f�@�4�@��,@��.@��m@���@�N<@�
=@��"@��j@���@��e@��@��@���@���@��w@��*@���@��C@���@��Z@�  @���@�k�@�!�@��E@�� @�Ft@���@�Y�@�S@��@�!@��m@���@���@���@�rG@��"@�[�@��o@�Ft@�
�@���@�%F@���@�xl@��@��;@��6@��@��A@��@��#@���@��$@���@���@�|@�qv@�b�@�^�@�Vm@�#�@���@��@��`@��s@���@�H�@�a|@�u@���@��	@���@�c @�/�@�u@��.@���@�4@���@���@��N@�j�@�X�@�O�@�/�@�r�@��m@��@�a@�4@���@�M@�x@��F@��S@�=�@��@��@��/@���@�}V@�($@)_@~��@~5?@}�^@|��@|	�@{;d@zc @y:�@x��@xr�@w��@v�c@u�z@um]@uN<@u�M@uzx@u@t�u@tg8@s�@t�@s��@sn/@s>�@r��@rv�@r-@q�@q�@qDg@p��@p7�@o�K@o��@o�	@oa@oK�@o33@o�@n�@nO@l�p@l~@kJ#@k@j��@k�@j��@j�s@ji�@j#:@i�@hH@g��@g.I@fGE@e|@e \@e \@e;@d~@c{J@cU�@cMj@c/�@c�@b��@b��@bC�@a�'@a-w@a�@`�@`�o@`M@_�a@_S@^͟@^�@^z@^C�@^	@]�.@]�@]��@]�@\��@\1'@[�{@Z�@Z)�@Z��@Z?@Y�9@Yԕ@Y��@Ys�@X��@W"�@U�N@U|@T��@UB�@U�N@U@@T��@S�W@S�K@S��@S��@SU�@R��@R�L@R�@R�@Q��@P��@P`�@Pb@OH�@N�s@N5?@M�=@L��@LA�@K��@Kg�@KC�@K@J�R@Jn�@J1�@I�T@I��@I�@I�@I�@H�f@H�5@H�U@H�@HH@H!@G��@G˒@G�a@G�a@G�w@Gj�@G i@F�2@F��@FkQ@FW�@E��@E��@E�"@E\�@E-w@D�5@D��@D��@D�@C�k@CY@B6�@B�@B4@B�@B�@B�@A�^@A@@��@@r�@@7�@@�@@�@?�&@?�K@?�@?�$@?iD@?4�@>�"@>��@>҉@>u%@>�@=�@=��@=�@<�@<�.@<D�@;y�@;C�@;33@;C@;�@:�'@:	@9w2@8��@8�e@8��@8�z@8��@8N�@7��@7a@7;d@7
=@6��@6͟@6�@6_�@5�T@5��@5�X@5�@5�@4ی@4�[@4�@4�@3�@3��@2�<@2@1��@1�@0�|@0�@0�@01@/�r@/�g@/�@.��@.��@.�@.��@.
�@-�@-�#@-�#@-��@-�@-�H@-�"@,��@,@+��@+H�@*��@*B[@)�d@)p�@)+�@(�P@(�E@(�U@(z�@(`�@(Z@(K^@'� @'qv@'8@&��@&d�@&GE@%�t@$��@$�j@$��@$�@#��@#S�@#�@#S@"�"@"�,@"�L@"d�@"e@!�)@!��@!J�@!�@ �E@ �o@ Z@ 9X@ݘ@�q@�*@��@@O@�"@��@��@YK@�@��@��@��@��@�7@hs@J�@<6@0�@&�@�@�@C-@� @��@\)@�M@��@H�@$�@�d@�H@�C@�@��@hs@e,@[W@?}@�5@�[@�p@�Y@4n@~@@�@t�@J#@;d@1�@C@(@�y@ȴ@��@�R@��@�6@l�@R�@@�@$�@�@
�@�~@m]@S&@�@�v@��@>B@��@��@�@�K@�k@e�@a@.I@(@�H@��@u@��@ԕ@��@`B@S&@G�@�@�@;@�5@�p@��@�4@j@_@V�@D�@(�@@�;@��@t�@,�@ i@�@҉@��@�h@�\@l�@c @E�@�@�j@�C@zx@<6@�@�)@|�@Z@G@��@�@��@��@j�@X�@
�M@
��@
�L@
�\@
a|@
6�@
@	��@	�"@	�@	��@	�S@	�@	p�@	X@	L�@	@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aϟ�Aϟ�AϠ�AϢ4AϢ�Aϣ�AϥFAϥ�AϦAϩ*AϪeAϤ�Aϛ�Aϛ�AϚ7Aϖ�AϚ�AϜAϟ�AϞ�Aϟ!AϠ�AϘ�AϠ�Aϟ�Aϖ�Aσ{A�s�A�S[A�F�A�DgA�@�A�A�A�A�A�A�A�B�A�A A�=�A�?�A�=<A�/OA�w�Aƫ6A�W
A�jKA��A�hsA�_A��{A���A��A���A�	�A�:^A���A� A�RTA��TA��A��TA�+6A�#�A�� A�XyA�'A��A���A�e�A�P�A��A��A��iA��2A�cA�S�A�m�A�5A��A�G�A�y	A�eA�fA���A�yrA��>A�9$A�A�A�r�A���A���A��eA�b�A���A�5�A��A�o�A�*�A�-�AQA}�6A{��Ay֡At*�Ao�Al]dAg  Ae��A`��A].�AZu�AX�AW[�AT2aAQ�XAP[�AO9�AM�AKrGAI��AH�AG;dADVAB��AAcA@��A@��A@jA@,�A>c A;ɆA:�PA6҉A5v`A2R�A/�FA.�'A-v�A+E�A)��A)8�A(zxA(R�A'�gA'�A%��A$��A"s�A �}A!ѷA"}VA"ѷA"�A"�A�FA_Am�A��AiDA�A�AݘA�+A��A-wA�-A��AMA#:A(�AOvA��A�[A��A�zA�A��AA�A(�A��A�?A>�A�A��Ap�A�A�kAv`A7�A��A�A�#A��A!�A�hA0UA
A	��A	�A	I�A��A�A�A��A��AXyA��A�4AخA��A�MA$tA�A�A�6A5�A2�A �"@���@��o@���@�?@�u%@���@���@�~�@�o@�$�@���@�bN@�:�@��'@�o@��
@��n@�_@�g8@�4@��2@�M@��d@�@�x@坲@��@�kQ@��o@��@�V@�G�@ߠ'@އ+@ݗ$@�+k@�n/@�J�@�Ĝ@׫�@ؓu@�O@�!�@��@�|�@���@�\�@؜x@،�@�\�@ד@��y@��@��H@ӶF@�S&@�\�@�$t@р4@З�@Ї+@Б�@�_@ϟV@Ό@Ό@Τ�@β�@ή}@�ff@��@͇�@�W?@�J�@�#�@̡b@�,=@��;@˷@�A @ʋD@�[�@� �@�t�@�	l@�ی@ȆY@ȅ�@ǆ�@�@@ƽ<@��@��m@�=@�5�@�!-@�#�@İ�@�s�@��H@Ĩ�@�r�@��]@�IR@º�@�/�@��@�`B@�`�@�9�@�S@��@��p@��\@�d�@�~@�خ@�0�@�\�@�ϫ@�c@�4�@��/@���@�e,@�9�@��@�	�@��F@��E@��_@� �@�ƨ@��c@�Z@���@��3@�\�@��@��`@�}V@�\)@�(�@�+�@�2a@�J�@�[W@�!�@�ߤ@�Ov@��[@��P@�a@�@��}@�C�@�@�*�@�e,@���@��d@�Y�@��@�ff@�7@��K@��[@��@��@��@��z@�a@���@�7@�7@��@�c@�8�@�;@��]@���@��@���@�H�@�@��&@��@�4�@�E�@��N@�K�@��@���@��X@�z�@�H�@�/�@��@���@�4@���@��U@��@�_@�	�@�a�@�͟@���@�{@��@�\�@��@��@�4n@�@�q@�d�@���@���@�N<@��f@���@��b@�oi@�_@�	@��T@�0�@���@��D@�E�@��@�˒@��@���@�1'@���@��"@�f�@�4�@��,@��.@��m@���@�N<@�
=@��"@��j@���@��e@��@��@���@���@��w@��*@���@��C@���@��Z@�  @���@�k�@�!�@��E@�� @�Ft@���@�Y�@�S@��@�!@��m@���@���@���@�rG@��"@�[�@��o@�Ft@�
�@���@�%F@���@�xl@��@��;@��6@��@��A@��@��#@���@��$@���@���@�|@�qv@�b�@�^�@�Vm@�#�@���@��@��`@��s@���@�H�@�a|@�u@���@��	@���@�c @�/�@�u@��.@���@�4@���@���@��N@�j�@�X�@�O�@�/�@�r�@��m@��@�a@�4@���@�M@�x@��F@��S@�=�@��@��@��/@���@�}V@�($@)_@~��@~5?@}�^@|��@|	�@{;d@zc @y:�@x��@xr�@w��@v�c@u�z@um]@uN<@u�M@uzx@u@t�u@tg8@s�@t�@s��@sn/@s>�@r��@rv�@r-@q�@q�@qDg@p��@p7�@o�K@o��@o�	@oa@oK�@o33@o�@n�@nO@l�p@l~@kJ#@k@j��@k�@j��@j�s@ji�@j#:@i�@hH@g��@g.I@fGE@e|@e \@e \@e;@d~@c{J@cU�@cMj@c/�@c�@b��@b��@bC�@a�'@a-w@a�@`�@`�o@`M@_�a@_S@^͟@^�@^z@^C�@^	@]�.@]�@]��@]�@\��@\1'@[�{@Z�@Z)�@Z��@Z?@Y�9@Yԕ@Y��@Ys�@X��@W"�@U�N@U|@T��@UB�@U�N@U@@T��@S�W@S�K@S��@S��@SU�@R��@R�L@R�@R�@Q��@P��@P`�@Pb@OH�@N�s@N5?@M�=@L��@LA�@K��@Kg�@KC�@K@J�R@Jn�@J1�@I�T@I��@I�@I�@I�@H�f@H�5@H�U@H�@HH@H!@G��@G˒@G�a@G�a@G�w@Gj�@G i@F�2@F��@FkQ@FW�@E��@E��@E�"@E\�@E-w@D�5@D��@D��@D�@C�k@CY@B6�@B�@B4@B�@B�@B�@A�^@A@@��@@r�@@7�@@�@@�@?�&@?�K@?�@?�$@?iD@?4�@>�"@>��@>҉@>u%@>�@=�@=��@=�@<�@<�.@<D�@;y�@;C�@;33@;C@;�@:�'@:	@9w2@8��@8�e@8��@8�z@8��@8N�@7��@7a@7;d@7
=@6��@6͟@6�@6_�@5�T@5��@5�X@5�@5�@4ی@4�[@4�@4�@3�@3��@2�<@2@1��@1�@0�|@0�@0�@01@/�r@/�g@/�@.��@.��@.�@.��@.
�@-�@-�#@-�#@-��@-�@-�H@-�"@,��@,@+��@+H�@*��@*B[@)�d@)p�@)+�@(�P@(�E@(�U@(z�@(`�@(Z@(K^@'� @'qv@'8@&��@&d�@&GE@%�t@$��@$�j@$��@$�@#��@#S�@#�@#S@"�"@"�,@"�L@"d�@"e@!�)@!��@!J�@!�@ �E@ �o@ Z@ 9X@ݘ@�q@�*@��@@O@�"@��@��@YK@�@��@��@��@��@�7@hs@J�@<6@0�@&�@�@�@C-@� @��@\)@�M@��@H�@$�@�d@�H@�C@�@��@hs@e,@[W@?}@�5@�[@�p@�Y@4n@~@@�@t�@J#@;d@1�@C@(@�y@ȴ@��@�R@��@�6@l�@R�@@�@$�@�@
�@�~@m]@S&@�@�v@��@>B@��@��@�@�K@�k@e�@a@.I@(@�H@��@u@��@ԕ@��@`B@S&@G�@�@�@;@�5@�p@��@�4@j@_@V�@D�@(�@@�;@��@t�@,�@ i@�@҉@��@�h@�\@l�@c @E�@�@�j@�C@zx@<6@�@�)@|�@Z@G@��@�@��@��@j�@X�@
�M@
��@
�L@
�\@
a|@
6�@
@	��@	�"@	�@	��@	�S@	�@	p�@	X@	L�@	@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�*B
�_B
�DB
�_B
�_B
�_B
�DB
�DB
�DB
�*B
��B
�B
�B
��B
��B
�B
��B
�B
�B
��B
��B
��B
��B
��B
��B
��B
�B
�yB
�*B
�B
�XB
�
B
��B
��B
�
B
�
B
��B
�B
�$B
�$B
�>B1�BP�BZ�Bh�B��B��B�\B�/B�=B��B�KB�`B�LB�xB�B��B��B�QB�)B��B�B��B��B�BB��B��B�B��B~�B|Bu�B}�B|jBz^B}VB�;Bh�B]IBN�B>�B~B%B
�nB
޸B
�B
��B
��B
�hB
��B
��B
{JB
vzB
o�B
dtB
Y�B
I�B
@B
:^B
1�B
&B
yB	��B	��B	��B	��B	�`B	��B	~BB	r�B	lqB	dtB	\xB	PbB	K�B	F?B	CB	AUB	C�B	CB	ESB	G�B	FtB	FYB	GzB	GzB	GEB	GzB	Q�B	]�B	^�B	RB	G�B	8�B	(�B	 B	WB	uB	.B	�B	�B	�B	�B	fB	'B��B	uB	�B		B	,�B	5�B	8lB	Q�B	BB	(�B	B	1B	�B�(B�B	�B�jB�B	�B	�B	�B	B	�B	�B	�B	 B	B	#�B	+kB	(�B	8�B	?B	K�B	�AB	��B	��B	��B	��B	��B	��B	��B	��B	�}B	��B	��B	��B	�nB	�B	��B	��B	�TB	�TB	�B	��B	��B	�B	��B	�?B	��B	�B	�tB	��B	�B	��B	��B	�B	��B	�OB	�aB	��B	�zB	ǮB	ʦB	ǔB	�KB	�%B	��B	��B	�B	��B	��B	��B	�aB	�@B	�4B	�B	��B	�RB	�KB	�?B	�9B	�%B	�fB	̳B	��B	οB	�pB	�bB	� B	��B	ևB	�
B	׍B	��B	ևB	՛B	�uB	��B	�"B	��B	ȀB	�B	��B	�6B	ѷB	�HB	��B	��B	�B	�B	��B	�>B	��B	�B	�*B	�yB	�`B	�&B	�B	�2B	�$B	�B	�nB	�mB	�$B	�sB	�mB	�ZB	�B	�]B	�B	� B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	�tB	�?B	�TB	�9B	��B	�tB	�RB	��B	�LB	��B	��B	�lB	�	B	��B	�xB	�B	�B	�DB	��B
 �B
�B
'B
 OB
AB
[B
uB
�B
�B
{B
�B
GB
�B
�B
�B
�B
�B
	�B

XB
	�B
	�B

XB
B
)B
xB
dB
B
�B
JB
�B
�B
�B
dB
�B
xB
DB

�B

�B
xB
�B
jB
6B
VB
B
BB
�B
bB
4B
 B
HB
�B
�B
�B
�B
�B
�B
�B
�B
pB
xB

�B
	�B

�B
xB
B
jB
�B
�B
�B
�B
�B
SB
~B
 'B
 \B
!-B
"hB
"B
"4B
"B
"�B
# B
#�B
# B
#�B
#�B
#�B
#�B
$@B
#�B
#�B
#TB
#nB
$B
$�B
$tB
$�B
%`B
&B
&�B
'mB
'mB
(
B
'�B
'�B
(XB
(XB
(>B
'�B
($B
(�B
*0B
+kB
+6B
*�B
*�B
*KB
*�B
+B
+QB
+�B
+�B
+�B
,B
+�B
,=B
,�B
-CB
-]B
-�B
-wB
-]B
-�B
.�B
.B
.IB
/B
/�B
0;B
0�B
1vB
1�B
2|B
2�B
3�B
49B
4�B
4�B
4�B
5?B
5�B
6�B
6FB
6�B
8�B
:�B
<�B
?cB
@�B
A�B
C{B
C�B
CGB
C�B
C�B
DgB
DB
C{B
B�B
BB
A�B
A;B
B�B
C�B
C�B
B�B
B�B
C�B
C�B
GB
F?B
EB
E�B
ESB
E9B
E�B
E�B
F%B
F�B
G�B
H�B
IRB
I�B
IB
IlB
JrB
KxB
K�B
K�B
K�B
L0B
L�B
L�B
NB
N�B
NVB
N�B
N�B
NVB
OBB
O�B
OBB
M�B
M�B
MPB
MB
M�B
NB
NpB
PB
Q�B
R�B
S�B
TB
S�B
S�B
S[B
S&B
S�B
T�B
U�B
U�B
V�B
V�B
W$B
WYB
WYB
W�B
W�B
W�B
W�B
XB
W�B
X+B
X�B
X�B
X�B
X�B
Y1B
YB
Y�B
ZB
[	B
[#B
[	B
Z�B
[qB
[�B
[�B
\)B
]B
]�B
]dB
]/B
]dB
]�B
^�B
`B
aB
aB
`�B
`�B
`�B
aB
bNB
cB
c B
b�B
b�B
b�B
c�B
d�B
d�B
dtB
dtB
d�B
e,B
fB
e�B
e,B
d�B
ezB
f�B
f�B
g�B
g8B
gB
f�B
f�B
g�B
gmB
g�B
g�B
h>B
hsB
i�B
jKB
j�B
kQB
k�B
lB
l�B
l�B
l�B
l�B
mwB
m�B
m�B
nIB
nB
nIB
ncB
n�B
n�B
o B
oOB
oiB
o�B
p;B
poB
p�B
qAB
rB
raB
r|B
q�B
q�B
s�B
s�B
s�B
tB
s�B
t9B
tnB
r�B
p�B
qAB
qB
q�B
utB
u�B
u?B
t�B
uB
uB
u?B
u?B
u�B
v`B
vzB
v�B
v�B
wfB
w�B
xB
x�B
x�B
y>B
x�B
yrB
yrB
zB
z�B
{�B
|B
|PB
|�B
|�B
}B
}"B
}"B
}<B
}�B
~B
~B
~BB
~�B
~�B
~�B
.B
cB
cB
cB
HB
}B
�B
�B
�B
�B
�B
��B
��B
�B
��B
�B
�;B
�UB
�UB
��B
��B
��B
�'B
�'B
�'B
�'B
��B
��B
��B
�AB
�'B
�AB
�[B
�uB
�uB
�uB
�uB
�uB
��B
��B
��B
��B
��B
��B
��B
�-B
�-B
�-B
��B
��B
��B
��B
�gB
�MB
�3B
�3B
�B
�B
��B
�B
��B
�mB
�mB
�SB
�9B
�SB
��B
��B
��B
��B
��B
��B
��B
�B
��B
�YB
�tB
��B
��B
�B
��B
��B
��B
�zB
�EB
�1B
�B
�1B
��B
��B
��B
��B
�7B
�B
�7B
�B
��B
��B
��B
��B
�rB
��B
��B
��B
��B
��B
��B
��B
�)B
��B
��B
�JB
��B
�B
�PB
��B
��B
�B
�B
�"B
�pB
��B
��B
�pB
��B
�B
�B
�vB
��B
�vB
�B
��B
��B
��B
�NB
��B
� B
�oB
�oB
�TB
��B
��B
�B
�@B
�uB
�B
�B
��B
��B
�,B
�FB
�aB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�SB
��B
��B
��B
��B
��B
��B
�
B
�
B
�
B
�
B
��B
�?B
��B
�_B
��B
��B
�KB
��B
�B
�QB
��B
��B
��B
��B
��B
��B
�B
�B
�]B
��B
��B
��B
�B
�~B
��B
��B
�B
��B
��B
��B
��B
��B
��B
�B
�;B
�VB
�VB
�VB
�VB
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
�B
�HB
��B
��B
��B
��B
��B
�B
�NB
�NB
��B
�hB
��B
��B
�TB
�nB
�nB
��B
��B
�B
�&B
�&B
�@B
�&B
�&B
�@B
��B
�tB
��B
��B
��B
��B
�B
�B
�`B
��B
��B
�B
�2B
�2B
�fB
�fB
��B
��B
��B
��B
��B
�B
�RB
�mB
��B
��B
��B
�$B
�sB
��B
�B
��B
��B
��B
��B
�yB
�DB
��B
��B
��B
��B
��B
��B
�KB
�B
�B
�B
�B
�B
��B
�QB
�6B
�6B
��333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333B
�*B
�_B
�DB
�_B
�_B
�_B
�DB
�DB
�DB
�*B
��B
�B
�B
��B
��B
�B
��B
�B
�B
��B
��B
��B
��B
��B
��B
��B
�B
�yB
�*B
�B
�XB
�
B
��B
��B
�
B
�
B
��B
�B
�$B
�$B
�>B1�BP�BZ�Bh�B��B��B�\B�/B�=B��B�KB�`B�LB�xB�B��B��B�QB�)B��B�B��B��B�BB��B��B�B��B~�B|Bu�B}�B|jBz^B}VB�;Bh�B]IBN�B>�B~B%B
�nB
޸B
�B
��B
��B
�hB
��B
��B
{JB
vzB
o�B
dtB
Y�B
I�B
@B
:^B
1�B
&B
yB	��B	��B	��B	��B	�`B	��B	~BB	r�B	lqB	dtB	\xB	PbB	K�B	F?B	CB	AUB	C�B	CB	ESB	G�B	FtB	FYB	GzB	GzB	GEB	GzB	Q�B	]�B	^�B	RB	G�B	8�B	(�B	 B	WB	uB	.B	�B	�B	�B	�B	fB	'B��B	uB	�B		B	,�B	5�B	8lB	Q�B	BB	(�B	B	1B	�B�(B�B	�B�jB�B	�B	�B	�B	B	�B	�B	�B	 B	B	#�B	+kB	(�B	8�B	?B	K�B	�AB	��B	��B	��B	��B	��B	��B	��B	��B	�}B	��B	��B	��B	�nB	�B	��B	��B	�TB	�TB	�B	��B	��B	�B	��B	�?B	��B	�B	�tB	��B	�B	��B	��B	�B	��B	�OB	�aB	��B	�zB	ǮB	ʦB	ǔB	�KB	�%B	��B	��B	�B	��B	��B	��B	�aB	�@B	�4B	�B	��B	�RB	�KB	�?B	�9B	�%B	�fB	̳B	��B	οB	�pB	�bB	� B	��B	ևB	�
B	׍B	��B	ևB	՛B	�uB	��B	�"B	��B	ȀB	�B	��B	�6B	ѷB	�HB	��B	��B	�B	�B	��B	�>B	��B	�B	�*B	�yB	�`B	�&B	�B	�2B	�$B	�B	�nB	�mB	�$B	�sB	�mB	�ZB	�B	�]B	�B	� B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	�tB	�?B	�TB	�9B	��B	�tB	�RB	��B	�LB	��B	��B	�lB	�	B	��B	�xB	�B	�B	�DB	��B
 �B
�B
'B
 OB
AB
[B
uB
�B
�B
{B
�B
GB
�B
�B
�B
�B
�B
	�B

XB
	�B
	�B

XB
B
)B
xB
dB
B
�B
JB
�B
�B
�B
dB
�B
xB
DB

�B

�B
xB
�B
jB
6B
VB
B
BB
�B
bB
4B
 B
HB
�B
�B
�B
�B
�B
�B
�B
�B
pB
xB

�B
	�B

�B
xB
B
jB
�B
�B
�B
�B
�B
SB
~B
 'B
 \B
!-B
"hB
"B
"4B
"B
"�B
# B
#�B
# B
#�B
#�B
#�B
#�B
$@B
#�B
#�B
#TB
#nB
$B
$�B
$tB
$�B
%`B
&B
&�B
'mB
'mB
(
B
'�B
'�B
(XB
(XB
(>B
'�B
($B
(�B
*0B
+kB
+6B
*�B
*�B
*KB
*�B
+B
+QB
+�B
+�B
+�B
,B
+�B
,=B
,�B
-CB
-]B
-�B
-wB
-]B
-�B
.�B
.B
.IB
/B
/�B
0;B
0�B
1vB
1�B
2|B
2�B
3�B
49B
4�B
4�B
4�B
5?B
5�B
6�B
6FB
6�B
8�B
:�B
<�B
?cB
@�B
A�B
C{B
C�B
CGB
C�B
C�B
DgB
DB
C{B
B�B
BB
A�B
A;B
B�B
C�B
C�B
B�B
B�B
C�B
C�B
GB
F?B
EB
E�B
ESB
E9B
E�B
E�B
F%B
F�B
G�B
H�B
IRB
I�B
IB
IlB
JrB
KxB
K�B
K�B
K�B
L0B
L�B
L�B
NB
N�B
NVB
N�B
N�B
NVB
OBB
O�B
OBB
M�B
M�B
MPB
MB
M�B
NB
NpB
PB
Q�B
R�B
S�B
TB
S�B
S�B
S[B
S&B
S�B
T�B
U�B
U�B
V�B
V�B
W$B
WYB
WYB
W�B
W�B
W�B
W�B
XB
W�B
X+B
X�B
X�B
X�B
X�B
Y1B
YB
Y�B
ZB
[	B
[#B
[	B
Z�B
[qB
[�B
[�B
\)B
]B
]�B
]dB
]/B
]dB
]�B
^�B
`B
aB
aB
`�B
`�B
`�B
aB
bNB
cB
c B
b�B
b�B
b�B
c�B
d�B
d�B
dtB
dtB
d�B
e,B
fB
e�B
e,B
d�B
ezB
f�B
f�B
g�B
g8B
gB
f�B
f�B
g�B
gmB
g�B
g�B
h>B
hsB
i�B
jKB
j�B
kQB
k�B
lB
l�B
l�B
l�B
l�B
mwB
m�B
m�B
nIB
nB
nIB
ncB
n�B
n�B
o B
oOB
oiB
o�B
p;B
poB
p�B
qAB
rB
raB
r|B
q�B
q�B
s�B
s�B
s�B
tB
s�B
t9B
tnB
r�B
p�B
qAB
qB
q�B
utB
u�B
u?B
t�B
uB
uB
u?B
u?B
u�B
v`B
vzB
v�B
v�B
wfB
w�B
xB
x�B
x�B
y>B
x�B
yrB
yrB
zB
z�B
{�B
|B
|PB
|�B
|�B
}B
}"B
}"B
}<B
}�B
~B
~B
~BB
~�B
~�B
~�B
.B
cB
cB
cB
HB
}B
�B
�B
�B
�B
�B
��B
��B
�B
��B
�B
�;B
�UB
�UB
��B
��B
��B
�'B
�'B
�'B
�'B
��B
��B
��B
�AB
�'B
�AB
�[B
�uB
�uB
�uB
�uB
�uB
��B
��B
��B
��B
��B
��B
��B
�-B
�-B
�-B
��B
��B
��B
��B
�gB
�MB
�3B
�3B
�B
�B
��B
�B
��B
�mB
�mB
�SB
�9B
�SB
��B
��B
��B
��B
��B
��B
��B
�B
��B
�YB
�tB
��B
��B
�B
��B
��B
��B
�zB
�EB
�1B
�B
�1B
��B
��B
��B
��B
�7B
�B
�7B
�B
��B
��B
��B
��B
�rB
��B
��B
��B
��B
��B
��B
��B
�)B
��B
��B
�JB
��B
�B
�PB
��B
��B
�B
�B
�"B
�pB
��B
��B
�pB
��B
�B
�B
�vB
��B
�vB
�B
��B
��B
��B
�NB
��B
� B
�oB
�oB
�TB
��B
��B
�B
�@B
�uB
�B
�B
��B
��B
�,B
�FB
�aB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�SB
��B
��B
��B
��B
��B
��B
�
B
�
B
�
B
�
B
��B
�?B
��B
�_B
��B
��B
�KB
��B
�B
�QB
��B
��B
��B
��B
��B
��B
�B
�B
�]B
��B
��B
��B
�B
�~B
��B
��B
�B
��B
��B
��B
��B
��B
��B
�B
�;B
�VB
�VB
�VB
�VB
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
�B
�HB
��B
��B
��B
��B
��B
�B
�NB
�NB
��B
�hB
��B
��B
�TB
�nB
�nB
��B
��B
�B
�&B
�&B
�@B
�&B
�&B
�@B
��B
�tB
��B
��B
��B
��B
�B
�B
�`B
��B
��B
�B
�2B
�2B
�fB
�fB
��B
��B
��B
��B
��B
�B
�RB
�mB
��B
��B
��B
�$B
�sB
��B
�B
��B
��B
��B
��B
�yB
�DB
��B
��B
��B
��B
��B
��B
�KB
�B
�B
�B
�B
�B
��B
�QB
�6B
�6B
��333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230713094536  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230713094726  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230713094727  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230713094727                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230713094727  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230713094727  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230713095953                      G�O�G�O�G�O�                JA  COOAcooa1.0                                                                 20230803071839  CF  PSAL            D�� D���?�                  JA  COOAcooa1.0                                                                 20230803071839  CF  PSAL            @,��D�@ ?�                  JA  COOAcooa1.0                                                                 20230803071839  CF  PSAL_ADJUSTED   D�@ D���?�                  JA  COOAcooa1.0                                                                 20230803071839  CF  PSAL_ADJUSTED   @,��D�  ?�                  JA  ARUP                                                                        20230803081500                      G�O�G�O�G�O�                