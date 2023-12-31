CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-07-05T09:42:31Z creation;2022-07-05T09:42:32Z conversion to V3.1      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220705094231  20220705095820  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               |A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @���'O1   @�����a�@-s33333�c���`A�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH��BNffBW��B`  Bh  Bq��Bv  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C  C  C
  C  C  C  C  C  C  C  C  C  C  C�fC"  C$  C&  C(�C*�C,  C.  C/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz�fD{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�3D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @fg@|��@�ff@�ffA33A?33A_33A33A���A���A���A�fgA�fgAߙ�AA���B��B��B��B��B'��B/��B7��B?��BH��BN33BWfgB_��Bg��BqfgBu��B��B��fB��fB��fB��fB��fB��fB��fB��fB�L�B�� B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3CٙC!�3C#�3C%�3C(�C*�C+�3C-�3C/ٙC1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CV�CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz�3Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD���D���D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�A�D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��D�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD���D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�1�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AѥFAљ1AѤ�AѡbAѧAѩ_AѪ0AѪ0AѫAѫkAѫAѫ�Aѫ�AѫkAѪeAѬqAѧ�Aѩ_Aѩ�AѪ�Aѩ�Aѩ�AѭAњ�A�}"A�m�A�C-Aˎ"A��A���A��AAð�A��A�6A���A��[A�6FA�ѷA�|�A�	A���A�{A�A�A�O�A��A���A��A�9�A�uZA���A�n�A���A�N�A�"hA�YA��OA��A�MjA�;�A�b�A�K)A��LA�=<A�{A��4A��A�*�A�,�A��$A���A��A�|A�<�A�g8A���A���A���A���A�ɆA���A�VAzs�Ax�Aw*0Av%�At��Ap�rAf7�AcQ�Aa�A]�pAY(�AU�AS�AP�ALzxAI~�AH��AHC�AG��AD_AB�AA�:AA�A?�A<L0A9�A7GA5��A4O�A3�A1��A1�)A1�A1�A/i�A.�A.C�A-��A-(A,ںA,a�A*qvA)�+A)H�A)/�A(�aA('RA'*0A'\�A%��A&#:A&��A'A�A'e�A'�FA&�WA'�{A'��A&7A%��A%͟A%�*A%�A$�A#�[A#(�A"��A"�LA!^�A �3A�PA�A��A"hA�?A�A�AL0A�QAMjA�cA�QA]�A֡A��ArGA�CA�BAQ�AߤAN<A6A�&A!-A�dA!-A�aA'�A�jA4A�}Ao A)_A�hAa�A{�A{A
��A
�rA	��A	�+A	�AZA��AIRA��A�6A1�A��A�UAqA�A~A�FA�A�eA��A8�Ar�A �yA �YA !@��5@�Ta@�F@��@���@���@��+@��V@��\@�ƨ@�@�y>@�Q�@���@�K�@�]d@��Q@��`@�m�@�:@���@@���@��M@��@�Q�@�@�B�@��@�bN@��@�G�@��'@�*�@���@��@�|@�L�@��@�tT@�1�@�	@�!@�1@���@�P@��@�@��@��@��@���@�t@�G�@���@�@��@���@�Z@�7@ݠ�@݈f@�G�@�Y@ܨ�@��r@ۈf@�j�@�`B@�IR@��M@ڛ�@��]@�\)@�m�@��3@�K�@��p@�:�@գn@��@��Q@ӗ$@�E9@��@��@ҥz@�\�@��@���@�($@�j@�҉@���@�N�@�S�@�ȴ@�_�@�O�@��@�_@ɠ�@�s�@��@���@ȵ@�H�@��j@�o@Ʋ�@��@��K@�zx@�:�@ı�@đ�@�z�@Ó�@�>�@�
=@�3�@��@�[W@�͟@��_@�N�@���@�1�@��@�_�@��@���@�{J@�F�@��@��+@�_@��g@��@�Mj@���@�s�@�>B@�ԕ@�zx@�S&@��@���@���@�q@�Ta@�@��z@�x@�A @�V@��$@�g8@��@�s@�/@�ں@���@�C�@��Q@���@�dZ@��`@�b@��@��@��@�5?@�(�@� �@��*@���@�U�@���@�Z@�($@�1@��@�J#@��@��y@���@��F@�_@�]�@�$t@��M@���@���@���@�h
@�{@���@��@���@�6@�y�@�,�@�&@�&�@�Y@��B@�y>@�6@���@�_p@� i@���@���@�PH@��@��@�ƨ@��4@��`@��O@�kQ@�PH@���@�4@���@��@�y>@��@�� @�s�@�7L@�@�+k@��z@��V@��4@�qv@�O@�8@�/@��`@���@��.@���@�	@��-@�K�@��M@��e@�Xy@�6@�$�@�  @�x@��@�ߤ@��z@�_�@��@�خ@��7@�S&@�A @�@���@�PH@��@��6@���@��"@��@��u@�c @�8�@��@��A@�@�|@�S�@��5@��.@��@��N@���@��4@�8�@��@��@�d�@�M@�H@�@�خ@�Z�@��P@��,@��@�{�@���@��@�j�@�'�@��@�͟@���@���@�}V@�V�@�!@��@���@�y�@�Dg@��@��]@�~(@�N�@�4n@�$�@��@�4@���@��3@��n@�=@��@��@���@�W�@�M@�w2@�%@��_@��A@�v�@�e�@�PH@��@�	@���@��t@���@�RT@�F�@��@���@�u�@�M@�/�@�-�@��@�s@��@�S@���@��@�\�@��@���@�zx@�Q�@��@�ȴ@�i�@�Z�@�@�@�5?@�
@X�@S@~�m@~c @}��@|�j@|Ft@{� @{g�@z�F@y�@y^�@x��@xM@w��@w8@v�6@vJ�@vJ@u��@u�@tq@tD�@t(�@s�w@s!-@rp;@r&�@q��@q�X@p�K@p*�@o8@n��@n�@m�H@mhs@l�f@lD�@kS�@j�x@j.�@j4@i�C@i!�@h'R@gj�@f�@f� @fR�@e��@e�@e��@e4@d��@dy>@d?�@d�@c�@c��@cK�@c�@b�]@b�+@a�#@a�@`��@`tT@`7�@`  @_�{@_+@^��@^�L@^��@^h
@]�@]|@]T�@\��@[��@[O@[�@Z��@Y�@Y�3@Yp�@Y&�@Y5�@Y�@Y;@X�I@W�@W��@W,�@V�@V+k@U�j@U��@UJ�@U	l@T��@S�@S��@S��@S>�@S@R��@R�@Q��@QS&@Q*0@Q%@P֡@O��@OC�@N��@NV@M�@M7L@L�/@LĜ@L��@L>B@K�@K=@Ko@J�'@J	@I��@I��@IA @I!�@H�5@Hh�@H~@G�
@G��@G9�@G@F��@F_@E�@E�^@E��@EL�@E!�@Dی@D��@DPH@Dx@C�&@C�{@C@C@C�@B�@BV@A��@Ax�@AG�@@��@@��@@��@@m�@?��@?�{@?Y@>�@>��@>�A@>\�@>�@=�h@=c�@=7L@=%@<�z@<h�@<:�@;�m@;��@;t�@;=@:�2@:�R@:n�@:	@9��@9��@9[W@8��@8N�@8,=@7��@7�$@7iD@6��@6�@6{�@6�@5@5�-@5�S@5p�@5=�@5-w@5�@5�@5@5;@4�`@4��@4��@4�I@4w�@4Q�@4�@3�;@3�$@3l�@36z@2��@2�,@2�@1��@1�@1!�@0��@0@/�$@/@O@.�@.�6@.��@._�@.#:@-�9@-��@,�P@,��@,Z@,!@+�4@*�@)�Z@)��@)c�@)4@(�@(��@(D�@'�@'|�@'�@&��@&c @&1�@%��@%��@%�@%N<@%/@%q@$��@$��@$j@$6@$@#�
@#��@#��@#A�@"��@"ߤ@"��@"�r@"^5@!�j@!�=@!�"@!}�@!+�@ �)@ q@ 2�@ $@   @�g@j�@+@
=@��@��@^5@)�@@�@�-@x�@@�@�9@|�@`�@:�@�@�6@�f@��@�4@X�@��@kQ@@�@�@��@O�@-w@;@�4@g8@:�@�W@e�@�@�2@�H@�@��@��@�A@Z�@�@�n@|@w2@u�@T�@�|@��@S�@'R@�@�;@�@qv@
=@�L@W�@8�@&�@J@��@��@�@e,@!�@�@��@�U@��@�o@oi@PH@	�@��@�K@��@�	@y�@j�@b�@b�@C�@/�@�@�@�@��@p;@!�@��@ϫ@�H@�-@[W@&�@@��@��@��@�z@Z@	�@�@��@��@F�@�@
��@
�A@
@�@	�Z@	�-@	�7@	m]@	S&@	8�@	%F@	�@	@@	�@�f@�5@�/@��@��@�O@�u@�D@w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AѥFAљ1AѤ�AѡbAѧAѩ_AѪ0AѪ0AѫAѫkAѫAѫ�Aѫ�AѫkAѪeAѬqAѧ�Aѩ_Aѩ�AѪ�Aѩ�Aѩ�AѭAњ�A�}"A�m�A�C-Aˎ"A��A���A��AAð�A��A�6A���A��[A�6FA�ѷA�|�A�	A���A�{A�A�A�O�A��A���A��A�9�A�uZA���A�n�A���A�N�A�"hA�YA��OA��A�MjA�;�A�b�A�K)A��LA�=<A�{A��4A��A�*�A�,�A��$A���A��A�|A�<�A�g8A���A���A���A���A�ɆA���A�VAzs�Ax�Aw*0Av%�At��Ap�rAf7�AcQ�Aa�A]�pAY(�AU�AS�AP�ALzxAI~�AH��AHC�AG��AD_AB�AA�:AA�A?�A<L0A9�A7GA5��A4O�A3�A1��A1�)A1�A1�A/i�A.�A.C�A-��A-(A,ںA,a�A*qvA)�+A)H�A)/�A(�aA('RA'*0A'\�A%��A&#:A&��A'A�A'e�A'�FA&�WA'�{A'��A&7A%��A%͟A%�*A%�A$�A#�[A#(�A"��A"�LA!^�A �3A�PA�A��A"hA�?A�A�AL0A�QAMjA�cA�QA]�A֡A��ArGA�CA�BAQ�AߤAN<A6A�&A!-A�dA!-A�aA'�A�jA4A�}Ao A)_A�hAa�A{�A{A
��A
�rA	��A	�+A	�AZA��AIRA��A�6A1�A��A�UAqA�A~A�FA�A�eA��A8�Ar�A �yA �YA !@��5@�Ta@�F@��@���@���@��+@��V@��\@�ƨ@�@�y>@�Q�@���@�K�@�]d@��Q@��`@�m�@�:@���@@���@��M@��@�Q�@�@�B�@��@�bN@��@�G�@��'@�*�@���@��@�|@�L�@��@�tT@�1�@�	@�!@�1@���@�P@��@�@��@��@��@���@�t@�G�@���@�@��@���@�Z@�7@ݠ�@݈f@�G�@�Y@ܨ�@��r@ۈf@�j�@�`B@�IR@��M@ڛ�@��]@�\)@�m�@��3@�K�@��p@�:�@գn@��@��Q@ӗ$@�E9@��@��@ҥz@�\�@��@���@�($@�j@�҉@���@�N�@�S�@�ȴ@�_�@�O�@��@�_@ɠ�@�s�@��@���@ȵ@�H�@��j@�o@Ʋ�@��@��K@�zx@�:�@ı�@đ�@�z�@Ó�@�>�@�
=@�3�@��@�[W@�͟@��_@�N�@���@�1�@��@�_�@��@���@�{J@�F�@��@��+@�_@��g@��@�Mj@���@�s�@�>B@�ԕ@�zx@�S&@��@���@���@�q@�Ta@�@��z@�x@�A @�V@��$@�g8@��@�s@�/@�ں@���@�C�@��Q@���@�dZ@��`@�b@��@��@��@�5?@�(�@� �@��*@���@�U�@���@�Z@�($@�1@��@�J#@��@��y@���@��F@�_@�]�@�$t@��M@���@���@���@�h
@�{@���@��@���@�6@�y�@�,�@�&@�&�@�Y@��B@�y>@�6@���@�_p@� i@���@���@�PH@��@��@�ƨ@��4@��`@��O@�kQ@�PH@���@�4@���@��@�y>@��@�� @�s�@�7L@�@�+k@��z@��V@��4@�qv@�O@�8@�/@��`@���@��.@���@�	@��-@�K�@��M@��e@�Xy@�6@�$�@�  @�x@��@�ߤ@��z@�_�@��@�خ@��7@�S&@�A @�@���@�PH@��@��6@���@��"@��@��u@�c @�8�@��@��A@�@�|@�S�@��5@��.@��@��N@���@��4@�8�@��@��@�d�@�M@�H@�@�خ@�Z�@��P@��,@��@�{�@���@��@�j�@�'�@��@�͟@���@���@�}V@�V�@�!@��@���@�y�@�Dg@��@��]@�~(@�N�@�4n@�$�@��@�4@���@��3@��n@�=@��@��@���@�W�@�M@�w2@�%@��_@��A@�v�@�e�@�PH@��@�	@���@��t@���@�RT@�F�@��@���@�u�@�M@�/�@�-�@��@�s@��@�S@���@��@�\�@��@���@�zx@�Q�@��@�ȴ@�i�@�Z�@�@�@�5?@�
@X�@S@~�m@~c @}��@|�j@|Ft@{� @{g�@z�F@y�@y^�@x��@xM@w��@w8@v�6@vJ�@vJ@u��@u�@tq@tD�@t(�@s�w@s!-@rp;@r&�@q��@q�X@p�K@p*�@o8@n��@n�@m�H@mhs@l�f@lD�@kS�@j�x@j.�@j4@i�C@i!�@h'R@gj�@f�@f� @fR�@e��@e�@e��@e4@d��@dy>@d?�@d�@c�@c��@cK�@c�@b�]@b�+@a�#@a�@`��@`tT@`7�@`  @_�{@_+@^��@^�L@^��@^h
@]�@]|@]T�@\��@[��@[O@[�@Z��@Y�@Y�3@Yp�@Y&�@Y5�@Y�@Y;@X�I@W�@W��@W,�@V�@V+k@U�j@U��@UJ�@U	l@T��@S�@S��@S��@S>�@S@R��@R�@Q��@QS&@Q*0@Q%@P֡@O��@OC�@N��@NV@M�@M7L@L�/@LĜ@L��@L>B@K�@K=@Ko@J�'@J	@I��@I��@IA @I!�@H�5@Hh�@H~@G�
@G��@G9�@G@F��@F_@E�@E�^@E��@EL�@E!�@Dی@D��@DPH@Dx@C�&@C�{@C@C@C�@B�@BV@A��@Ax�@AG�@@��@@��@@��@@m�@?��@?�{@?Y@>�@>��@>�A@>\�@>�@=�h@=c�@=7L@=%@<�z@<h�@<:�@;�m@;��@;t�@;=@:�2@:�R@:n�@:	@9��@9��@9[W@8��@8N�@8,=@7��@7�$@7iD@6��@6�@6{�@6�@5@5�-@5�S@5p�@5=�@5-w@5�@5�@5@5;@4�`@4��@4��@4�I@4w�@4Q�@4�@3�;@3�$@3l�@36z@2��@2�,@2�@1��@1�@1!�@0��@0@/�$@/@O@.�@.�6@.��@._�@.#:@-�9@-��@,�P@,��@,Z@,!@+�4@*�@)�Z@)��@)c�@)4@(�@(��@(D�@'�@'|�@'�@&��@&c @&1�@%��@%��@%�@%N<@%/@%q@$��@$��@$j@$6@$@#�
@#��@#��@#A�@"��@"ߤ@"��@"�r@"^5@!�j@!�=@!�"@!}�@!+�@ �)@ q@ 2�@ $@   @�g@j�@+@
=@��@��@^5@)�@@�@�-@x�@@�@�9@|�@`�@:�@�@�6@�f@��@�4@X�@��@kQ@@�@�@��@O�@-w@;@�4@g8@:�@�W@e�@�@�2@�H@�@��@��@�A@Z�@�@�n@|@w2@u�@T�@�|@��@S�@'R@�@�;@�@qv@
=@�L@W�@8�@&�@J@��@��@�@e,@!�@�@��@�U@��@�o@oi@PH@	�@��@�K@��@�	@y�@j�@b�@b�@C�@/�@�@�@�@��@p;@!�@��@ϫ@�H@�-@[W@&�@@��@��@��@�z@Z@	�@�@��@��@F�@�@
��@
�A@
@�@	�Z@	�-@	�7@	m]@	S&@	8�@	%F@	�@	@@	�@�f@�5@�/@��@��@�O@�u@�D@w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N"B
N"B
NB
NVB
N"B
NpB
N"B
M�B
M�B
MjB
M�B
M�B
OBB
Q�B
T{B
VB
Z�B �BG_Bt�B�kB6�BZ�Bg�By>B�?B�B�#B�rB��B�eB�$B��B��B��B��B�ZB��B��B�yB��By	Bt�Bu�BzBp�Bk6Bd�BX�B:�B!bB�B�BB{B��B��B�BٚB��B��ByrB\�BC�B!�B
�B
��B
k6B
SB
J#B
7�B
�B	�B	��B	�KB	�HB	��B	��B	bhB	N�B	E�B	1�B	]B	1B��B��B�$B��B��B��B�)B�B޸BڠBרB��B��B� B��B�BԕBܒB	B	%B	#TB	�B	�B	�B	yB	�B	bB	bB	B	�B	"�B	,"B	0�B	5�B	7�B	8�B	\�B	`�B	j�B	z�B	�~B	��B	��B	��B	��B	�B	��B	�VB	�lB	ңB	�hB	��B	�0B	ˬB	�B	�[B	��B	�
B	�qB	��B	��B	�B	�B	��B	�nB	��B	�xB	��B	�PB
 4B
�B
�B
-B
AB	�B
 OB
B
-B
 �B	�B	�<B	��B	��B	�B	�AB	�B	�0B	�B	�wB	��B	�"B	�B	�B	�B	�B	��B	�B	�fB	��B	�B	�B	��B	��B	�B	�B	�B	�FB	�B	�"B	��B	�CB	��B	�fB	�B	�,B	�B	߾B	�)B	�qB	�IB	ڠB	��B	ևB	�aB	�hB	ңB	�,B	��B	�sB	�B	�B	�QB	�kB	چB	��B	�)B	��B	�~B	�~B	�5B	��B	�!B	�B	��B	��B	�BB	��B	�B	�B	��B	��B	��B	�B	�RB	�B	�fB	�B	��B	�yB	�B	�LB	��B	��B	�B	��B	�
B	�sB	��B	��B	�KB	�B	�B	�B	��B	�B	��B	��B	�;B	�B	�B	�B	��B	��B	�3B	��B	��B	��B	��B	��B	��B	��B	�tB	�tB	��B	�B	�3B	��B	�`B	�%B	�9B	�B	�vB	�[B	��B	��B	��B	�AB	�B	�UB	��B	�5B	��B	��B	��B	�B	�B	�3B	�nB	��B	��B	�+B	��B	��B	��B	�B	��B	�9B	�9B	�TB	�B	��B	�B	��B	�B	��B	�`B	�FB	��B	��B	��B	�fB	��B	��B	�B	��B	�B	��B	��B	�	B	��B	�*B	�DB	�DB	�B	�*B	�^B	�xB	�xB	��B	�xB	�^B	�xB	��B	�JB	�B	�B	�jB	��B	��B	��B	�jB	��B	��B	��B	��B	��B	��B	��B	��B	�"B	�"B	�qB	�VB	��B	�B	��B	��B	��B	�cB
 �B
 OB
 �B
 �B
 �B
 �B
 B
B
 B
�B
'B
�B
GB
{B
�B
�B
�B
gB
B
B
�B
YB
?B
�B
�B
?B
YB
tB
�B
�B
EB
_B
�B
�B
�B
�B
�B
�B
KB
fB
�B
�B
�B
	B
	�B
	�B
	�B
	�B
	�B

�B
xB
xB
�B
�B
PB
VB
VB
�B
�B
(B
(B
�B
"B
<B
B
�B
\B
�B
}B
4B
�B
�B
�B
hB
hB
hB
 B
 B
&B
@B
�B
,B
�B
�B
�B
�B
2B
gB
�B
B
�B
�B
?B
�B
sB
�B
yB
�B
�B
B
B
�B
�B
qB
�B
B
]B
xB
]B
�B
�B
�B
�B
�B
�B
�B
;B
�B
 'B
 �B
 �B
 �B
 �B
 �B
!-B
!�B
"hB
"hB
"�B
"�B
#TB
#TB
#nB
#�B
#�B
$&B
$&B
$tB
$�B
$�B
%B
%FB
%`B
%zB
%�B
%�B
&B
&�B
'B
'B
'8B
'B
'8B
'B
'�B
'mB
(�B
(�B
(�B
)DB
)DB
)_B
)_B
)DB
)yB
)�B
)�B
*B
*eB
*�B
+6B
+�B
+�B
,WB
,�B
,�B
-)B
-wB
-�B
.B
-�B
-�B
-�B
.�B
.�B
/ B
.�B
/B
/�B
/�B
0B
0B
/�B
/�B
/�B
1B
1B
0�B
0�B
1[B
1�B
1�B
1�B
2|B
3�B
4�B
4�B
4�B
5�B
5�B
6zB
6�B
6�B
7LB
7�B
8B
8�B
9>B
9�B
9�B
:DB
:�B
:�B
:xB
:�B
:�B
;B
;�B
<B
<B
<�B
=B
=�B
=�B
=�B
>(B
>]B
>�B
?.B
?�B
?�B
@ B
@4B
@OB
@4B
?.B
>]B
>BB
>BB
>]B
>wB
>]B
>]B
>]B
>wB
>]B
>wB
>]B
>BB
>BB
>�B
>�B
>�B
>wB
>�B
?cB
?}B
?cB
?}B
?}B
@B
@OB
@�B
AB
AB
A;B
A�B
A�B
A�B
BAB
B�B
B�B
B�B
EB
E�B
FB
FYB
FB
FtB
F�B
F�B
F�B
G_B
G�B
G�B
HfB
H�B
IB
I7B
IlB
I�B
I�B
J�B
J�B
J�B
KDB
K^B
KxB
K�B
L0B
L�B
L�B
L�B
L�B
M�B
N"B
N�B
N�B
OB
O�B
O�B
O�B
O�B
P}B
Q�B
RB
R:B
S@B
S�B
S�B
TFB
T�B
U�B
U�B
VSB
VmB
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W$B
W?B
WsB
WsB
WsB
W�B
W�B
X+B
X�B
YB
Y�B
Y�B
Y�B
Z7B
Z7B
Z7B
ZQB
Z�B
[	B
[=B
[WB
[WB
\B
\)B
\�B
\�B
\�B
\�B
]B
]IB
]�B
]�B
]�B
]�B
^5B
^OB
^OB
^�B
^�B
^�B
^�B
_VB
_;B
_�B
_�B
_�B
`'B
`BB
`vB
`�B
`�B
aB
a-B
aHB
a�B
a�B
b4B
b�B
b�B
b�B
b�B
b�B
c B
c:B
c:B
c:B
c:B
c:B
cTB
cnB
cnB
c�B
c�B
c�B
c�B
d@B
dZB
d�B
d�B
d�B
d�B
e,B
eFB
e`B
e�B
e�B
fLB
f�B
g8B
g�B
g�B
hXB
hXB
h$B
hXB
iB
j0B
jeB
jKB
j0B
jKB
i�B
iDB
h�B
iDB
i�B
i�B
i�B
i�B
i�B
j0B
j0B
j�B
j�B
kQB
k�B
k�B
k�B
l�B
l�B
l�B
l�B
mCB
mB
mCB
mwB
mCB
m]B
n�B
oB
oiB
oiB
o�B
o�B
o�B
pUB
pUB
pUB
p;B
p�B
p�B
q'B
q'B
q'B
qAB
q'B
q�B
qvB
q�B
q�B
q�B
q�B
rB
rB
r-B
rGB
raB
r�B
r�B
r�B
shB
sMB
s�B
s�B
s�B
tB
s�B
tB
s�B
uB
t�B
t�B
utB
u�B
u�B
u�B
vB
v�B
v�B
v�B
v�B
w�B
xB
xB
xB
x8B
xlB
xlB
xlB
x�B
x�B
y	B
yXB
y>B
yXB
y>B
y�B
y�B
z*B
z�B
z�B
z�B
{B
{B
{�B
|6B
|�B
|�B
}B
}"B
}<B
}"B
}"B
}�B
}VB
}VB
}qB
}�B
}�B
}�B
}�B
~B
~�B
~�B
~�B
~�B
B
B
B
.B
.B
HB
cB
�B
�B
�B
�B
�B
�iB
��B
��B
��B
��B
�UB
�oB
��B
��B
��B
��B
��B
�'B
�AB
�[B
�uB
��B
�-B
�GB
��B
��B
��B
�3B
��B
��B
��B
�B
�B
�9B
�SB
�SB
�SB
�mB
�9B
��B
��B
��B
��B
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N"B
N"B
NB
NVB
N"B
NpB
N"B
M�B
M�B
MjB
M�B
M�B
OBB
Q�B
T{B
VB
Z�B �BG_Bt�B�kB6�BZ�Bg�By>B�?B�B�#B�rB��B�eB�$B��B��B��B��B�ZB��B��B�yB��By	Bt�Bu�BzBp�Bk6Bd�BX�B:�B!bB�B�BB{B��B��B�BٚB��B��ByrB\�BC�B!�B
�B
��B
k6B
SB
J#B
7�B
�B	�B	��B	�KB	�HB	��B	��B	bhB	N�B	E�B	1�B	]B	1B��B��B�$B��B��B��B�)B�B޸BڠBרB��B��B� B��B�BԕBܒB	B	%B	#TB	�B	�B	�B	yB	�B	bB	bB	B	�B	"�B	,"B	0�B	5�B	7�B	8�B	\�B	`�B	j�B	z�B	�~B	��B	��B	��B	��B	�B	��B	�VB	�lB	ңB	�hB	��B	�0B	ˬB	�B	�[B	��B	�
B	�qB	��B	��B	�B	�B	��B	�nB	��B	�xB	��B	�PB
 4B
�B
�B
-B
AB	�B
 OB
B
-B
 �B	�B	�<B	��B	��B	�B	�AB	�B	�0B	�B	�wB	��B	�"B	�B	�B	�B	�B	��B	�B	�fB	��B	�B	�B	��B	��B	�B	�B	�B	�FB	�B	�"B	��B	�CB	��B	�fB	�B	�,B	�B	߾B	�)B	�qB	�IB	ڠB	��B	ևB	�aB	�hB	ңB	�,B	��B	�sB	�B	�B	�QB	�kB	چB	��B	�)B	��B	�~B	�~B	�5B	��B	�!B	�B	��B	��B	�BB	��B	�B	�B	��B	��B	��B	�B	�RB	�B	�fB	�B	��B	�yB	�B	�LB	��B	��B	�B	��B	�
B	�sB	��B	��B	�KB	�B	�B	�B	��B	�B	��B	��B	�;B	�B	�B	�B	��B	��B	�3B	��B	��B	��B	��B	��B	��B	��B	�tB	�tB	��B	�B	�3B	��B	�`B	�%B	�9B	�B	�vB	�[B	��B	��B	��B	�AB	�B	�UB	��B	�5B	��B	��B	��B	�B	�B	�3B	�nB	��B	��B	�+B	��B	��B	��B	�B	��B	�9B	�9B	�TB	�B	��B	�B	��B	�B	��B	�`B	�FB	��B	��B	��B	�fB	��B	��B	�B	��B	�B	��B	��B	�	B	��B	�*B	�DB	�DB	�B	�*B	�^B	�xB	�xB	��B	�xB	�^B	�xB	��B	�JB	�B	�B	�jB	��B	��B	��B	�jB	��B	��B	��B	��B	��B	��B	��B	��B	�"B	�"B	�qB	�VB	��B	�B	��B	��B	��B	�cB
 �B
 OB
 �B
 �B
 �B
 �B
 B
B
 B
�B
'B
�B
GB
{B
�B
�B
�B
gB
B
B
�B
YB
?B
�B
�B
?B
YB
tB
�B
�B
EB
_B
�B
�B
�B
�B
�B
�B
KB
fB
�B
�B
�B
	B
	�B
	�B
	�B
	�B
	�B

�B
xB
xB
�B
�B
PB
VB
VB
�B
�B
(B
(B
�B
"B
<B
B
�B
\B
�B
}B
4B
�B
�B
�B
hB
hB
hB
 B
 B
&B
@B
�B
,B
�B
�B
�B
�B
2B
gB
�B
B
�B
�B
?B
�B
sB
�B
yB
�B
�B
B
B
�B
�B
qB
�B
B
]B
xB
]B
�B
�B
�B
�B
�B
�B
�B
;B
�B
 'B
 �B
 �B
 �B
 �B
 �B
!-B
!�B
"hB
"hB
"�B
"�B
#TB
#TB
#nB
#�B
#�B
$&B
$&B
$tB
$�B
$�B
%B
%FB
%`B
%zB
%�B
%�B
&B
&�B
'B
'B
'8B
'B
'8B
'B
'�B
'mB
(�B
(�B
(�B
)DB
)DB
)_B
)_B
)DB
)yB
)�B
)�B
*B
*eB
*�B
+6B
+�B
+�B
,WB
,�B
,�B
-)B
-wB
-�B
.B
-�B
-�B
-�B
.�B
.�B
/ B
.�B
/B
/�B
/�B
0B
0B
/�B
/�B
/�B
1B
1B
0�B
0�B
1[B
1�B
1�B
1�B
2|B
3�B
4�B
4�B
4�B
5�B
5�B
6zB
6�B
6�B
7LB
7�B
8B
8�B
9>B
9�B
9�B
:DB
:�B
:�B
:xB
:�B
:�B
;B
;�B
<B
<B
<�B
=B
=�B
=�B
=�B
>(B
>]B
>�B
?.B
?�B
?�B
@ B
@4B
@OB
@4B
?.B
>]B
>BB
>BB
>]B
>wB
>]B
>]B
>]B
>wB
>]B
>wB
>]B
>BB
>BB
>�B
>�B
>�B
>wB
>�B
?cB
?}B
?cB
?}B
?}B
@B
@OB
@�B
AB
AB
A;B
A�B
A�B
A�B
BAB
B�B
B�B
B�B
EB
E�B
FB
FYB
FB
FtB
F�B
F�B
F�B
G_B
G�B
G�B
HfB
H�B
IB
I7B
IlB
I�B
I�B
J�B
J�B
J�B
KDB
K^B
KxB
K�B
L0B
L�B
L�B
L�B
L�B
M�B
N"B
N�B
N�B
OB
O�B
O�B
O�B
O�B
P}B
Q�B
RB
R:B
S@B
S�B
S�B
TFB
T�B
U�B
U�B
VSB
VmB
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W$B
W?B
WsB
WsB
WsB
W�B
W�B
X+B
X�B
YB
Y�B
Y�B
Y�B
Z7B
Z7B
Z7B
ZQB
Z�B
[	B
[=B
[WB
[WB
\B
\)B
\�B
\�B
\�B
\�B
]B
]IB
]�B
]�B
]�B
]�B
^5B
^OB
^OB
^�B
^�B
^�B
^�B
_VB
_;B
_�B
_�B
_�B
`'B
`BB
`vB
`�B
`�B
aB
a-B
aHB
a�B
a�B
b4B
b�B
b�B
b�B
b�B
b�B
c B
c:B
c:B
c:B
c:B
c:B
cTB
cnB
cnB
c�B
c�B
c�B
c�B
d@B
dZB
d�B
d�B
d�B
d�B
e,B
eFB
e`B
e�B
e�B
fLB
f�B
g8B
g�B
g�B
hXB
hXB
h$B
hXB
iB
j0B
jeB
jKB
j0B
jKB
i�B
iDB
h�B
iDB
i�B
i�B
i�B
i�B
i�B
j0B
j0B
j�B
j�B
kQB
k�B
k�B
k�B
l�B
l�B
l�B
l�B
mCB
mB
mCB
mwB
mCB
m]B
n�B
oB
oiB
oiB
o�B
o�B
o�B
pUB
pUB
pUB
p;B
p�B
p�B
q'B
q'B
q'B
qAB
q'B
q�B
qvB
q�B
q�B
q�B
q�B
rB
rB
r-B
rGB
raB
r�B
r�B
r�B
shB
sMB
s�B
s�B
s�B
tB
s�B
tB
s�B
uB
t�B
t�B
utB
u�B
u�B
u�B
vB
v�B
v�B
v�B
v�B
w�B
xB
xB
xB
x8B
xlB
xlB
xlB
x�B
x�B
y	B
yXB
y>B
yXB
y>B
y�B
y�B
z*B
z�B
z�B
z�B
{B
{B
{�B
|6B
|�B
|�B
}B
}"B
}<B
}"B
}"B
}�B
}VB
}VB
}qB
}�B
}�B
}�B
}�B
~B
~�B
~�B
~�B
~�B
B
B
B
.B
.B
HB
cB
�B
�B
�B
�B
�B
�iB
��B
��B
��B
��B
�UB
�oB
��B
��B
��B
��B
��B
�'B
�AB
�[B
�uB
��B
�-B
�GB
��B
��B
��B
�3B
��B
��B
��B
�B
�B
�9B
�SB
�SB
�SB
�mB
�9B
��B
��B
��B
��B
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220705094206  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220705094231  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220705094232  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220705094232                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220705184237  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220705184237  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220705095820                      G�O�G�O�G�O�                