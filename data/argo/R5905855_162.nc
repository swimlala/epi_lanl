CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-07-23T12:44:15Z creation;2023-07-23T12:44:16Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20230723124415  20230803081500  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�<��d��1   @�<��qf@/ۥ�S���c��+J1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B���B���B�  B�  B�  B�ffB�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	fD	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D��3D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�3D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B(33B033B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��B��fB�� B��fB��fB��fB��3B��3B��fB��fB��fB�L�B��fB��B׳3B۳3B��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C@�CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C|�C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D	3D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�A�D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fD���D��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��D�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�@�A�C�A�B�A�F�A�O�A�P�A�IA�H�A�K�A�8�A�$�A�
	A���A��xA��&AΡ�AΊ�A΅A΃GA�~�A�zxA�sMA�l�A�d&A�U�A�3hA�4A���A��A��AͿA���A�YAʒ�AɯA��fA�D�A�B�A�k�A��A��A�!A�@OA�!A�xA���A��*A���A�o�A���A�� A��EA�~�A��A�WsA���A���A�a�A���A��A���A�уA��
A�K^A���A�lWA��A�"�A�
rA��jA��A�ҽA��GA��A��A���A��A���A�A�F?A�T�A�>A�G�A���A��A��<A�DA�s�A�rA��A�lWA���A��mA�"�A��AA��-A~��Ax�	AwH�Au�AuFtAt^5ArQ�Aox�AkbNAf�Ads�AaA^d�A\o�A[9XAZJ#AXJ�AW�AUϫAP�bAL6zAK��AI�HAGbNAF��AF��AFE9AEU2AC#:AA��AA
=A?�A?�A=��A<�A:�A8�rA7�A5�]A4��A2,�A0H�A.M�A,FtA*��A)��A'_A&_A%�A$U2A"��A!C�A��Ap;A\�A�Ao A�bAE9AjA�
A8�A�A��A��A!�Ah�A�AB�AM�A'RA��A�A&�A��A�CA��A��A�"A��A`�A	�9A�AbNA\�A!-A�~A�.A4nA�A�AQ�AuAA�A
=A�,A�A"hA��A��A��AP�A�A �A a�A  i@�n/@�h�@��a@��@��B@�ѷ@��@���@�@��M@�5�@��@��h@���@�}V@�_@��M@���@���@�E9@�}V@���@�E9@��@��?@�bN@�(�@�Q�@�L0@��6@�2a@�x@���@�@���@���@�Dg@�U�@�e@�oi@�c @��D@�>�@��a@�!�@�z@��D@߃{@�@O@�͟@�V@ܕ@��a@ۂ�@�n/@�\�@�RT@�B�@�;d@�8�@�o@ړu@ٙ�@إz@�zx@�7L@Ե@�g8@�/�@��]@�
�@ъ�@�L�@�P�@�Vm@�T�@�H�@�:�@� i@��@�J�@�C�@�%F@̞@�Q�@�@���@�T�@ʺ�@�^5@Ɉf@�u%@�6�@��@�e,@�V@ƂA@ŶF@ć�@�@�&@�V�@��;@�:�@�@�@���@�ԕ@�C�@�~@���@��@��s@�w�@�)�@���@�0�@��@��@��P@��r@�J@��A@�ƨ@��t@�l�@��/@�{@���@�ϫ@��"@�@�S�@���@�S@�>B@��j@���@���@�W?@�'�@��@���@�Q�@�d�@�U2@��>@��@�O�@�]d@�B[@�{@��]@���@��@���@��b@�Ta@�!�@�˒@���@��@�bN@��m@���@���@���@�qv@��$@��@�m]@���@�x�@���@���@�&�@��M@�/@���@��,@�4@�v`@�ѷ@��@���@�`�@�:�@�&�@�&�@�w2@�x@�F�@�U2@��Q@��z@���@���@���@��"@��~@���@�A�@��!@�&�@��z@��7@�S�@�>�@�%F@��`@�PH@��@���@��"@�l�@�E9@��8@��@���@�K^@�@��a@���@�o�@�^�@�2a@��6@�oi@�(�@���@��z@���@��@�Vm@� \@��@���@��]@���@��@��A@��o@�Z�@��@�O�@��@�#:@�M@��@��D@���@�O�@��P@���@��7@�k�@�0�@��L@�Ov@�@�@�8�@�6@�@���@���@��}@��z@��@�~�@�Y�@�(�@��E@��m@���@���@�U2@���@��$@�S&@�=�@�K�@�@@�� @�bN@��@��z@���@�a�@���@�kQ@�d�@��)@��@�W?@�9�@��@�d�@�?�@�3�@��@�J@��@�ݘ@��M@�F@�	l@���@��@�C-@��@�{J@�S�@�y�@��/@���@���@���@�y>@�	@��6@�c@�RT@�-w@�@��2@���@�YK@��@�{J@�x@�w2@�s�@�e�@��@���@��4@��+@�w�@���@���@���@��A@�>B@�x@��N@�`B@�q@��E@�i�@��@�g@E9@�@~\�@~4@}�@|:�@{�W@{�$@{iD@{/�@{�@z��@z�1@zq�@z4@y�@yVm@x�P@x(�@wy�@w8@w@w�@v�]@v��@u��@t�p@t4n@sMj@s@r�@r��@r��@r)�@qQ�@p�@o�r@o��@og�@oU�@o&@n�@n��@o$t@n��@n�@m�t@l��@l�@k�A@k��@j�2@j�@i��@i��@i`B@iN<@i(�@h֡@h�Y@hC-@g�
@g8@fں@f��@f\�@f8�@e��@e=�@e�@e�@e�@e�@d�I@d]d@d[�@c��@c)_@b��@bv�@b@a�@a��@b@a��@a+@`@_=@^v�@^{@]�9@]-w@] \@]^�@]=�@]?}@\��@\1'@[�
@[�@[�@[�
@[��@[@O@Z��@Z�!@Z:*@Z�@Y�o@Y�C@YIR@Y%F@X�O@W�+@Wqv@W]�@W;d@V�@V� @VO@T�5@T��@TM@Sg�@R�6@ROv@R_@Q��@Q0�@P�f@P��@P��@P�@PC-@P/�@O��@O�6@O�a@O�*@OiD@O=@O.I@N�B@N:*@M��@M�3@M�C@M�n@M��@M�'@M4@LM@L~@K� @Kx@K6z@K�@J�@I|@IB�@Hی@HS�@G�@Gخ@G��@G>�@F҉@Fu%@E�N@E(�@D�$@Dc�@DG@Cg�@B�@B�B@Bxl@B@�@B+k@A�@As�@A�@@�U@@��@@,=@?�@?�F@?b�@?�@>��@>�@>^5@>GE@>e@=�^@=��@=m]@<�P@<�@<M@<,=@;��@:��@:��@:~�@:��@:z@:V@9�9@9X@9@9+@8��@8��@8Ĝ@8��@8w�@8oi@8c�@8A�@8�@7�m@7��@7�@6�@6��@60U@5�^@5G�@4�E@4�p@4�?@4��@4��@4�@4~(@4,=@3�V@3�@2�h@2�@1��@1�@1�'@1V@0�@0֡@0�O@0(�@/��@/,�@.�@.ں@.�x@.^5@.?@-��@-��@-J�@,�[@,M@,<�@,2�@,!@,1@+�
@+�w@+�f@+@*�M@*�@*�m@*�\@*V@*&�@)��@)Q�@)&�@)�@(��@(e�@'S�@&�L@&@�@&
�@%�.@%��@%�@%��@%hs@%O�@$�@$Ft@#�@#��@#��@#��@#g�@#O@#/�@"�H@"��@"l�@"e@!�)@!�X@!c�@!&�@ �K@ �@ ��@ `�@ I�@ �@ƨ@��@l�@'�@�@�@ȴ@u%@W�@�9@`B@Ĝ@�_@j@U2@D�@�@�0@O@(@ߤ@�X@�6@}V@Z�@�@�.@rG@�@�U@��@tT@N�@7�@~@�;@�*@��@��@�P@iD@/�@�H@�'@�r@\�@1�@-@��@��@rG@o @`B@G�@*0@@@	l@�@�e@U2@M@�@�@�@�@b@  @�@�A@ݘ@�}@��@b�@�@�@��@�A@H�@u@�^@hs@:�@#�@�@�f@��@��@�|@��@�9@g8@C-@2�@�@�]@�6@��@��@�P@)_@��@��@��@�'@�}@��@q�@E�@)�@�o@�@��@�9@�^@��@x�@2a@V@�[@�9@��@�o@u�@_@4n@�@@@@x@�@�Q@��@Mj@+@�@�@
��@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�@�A�C�A�B�A�F�A�O�A�P�A�IA�H�A�K�A�8�A�$�A�
	A���A��xA��&AΡ�AΊ�A΅A΃GA�~�A�zxA�sMA�l�A�d&A�U�A�3hA�4A���A��A��AͿA���A�YAʒ�AɯA��fA�D�A�B�A�k�A��A��A�!A�@OA�!A�xA���A��*A���A�o�A���A�� A��EA�~�A��A�WsA���A���A�a�A���A��A���A�уA��
A�K^A���A�lWA��A�"�A�
rA��jA��A�ҽA��GA��A��A���A��A���A�A�F?A�T�A�>A�G�A���A��A��<A�DA�s�A�rA��A�lWA���A��mA�"�A��AA��-A~��Ax�	AwH�Au�AuFtAt^5ArQ�Aox�AkbNAf�Ads�AaA^d�A\o�A[9XAZJ#AXJ�AW�AUϫAP�bAL6zAK��AI�HAGbNAF��AF��AFE9AEU2AC#:AA��AA
=A?�A?�A=��A<�A:�A8�rA7�A5�]A4��A2,�A0H�A.M�A,FtA*��A)��A'_A&_A%�A$U2A"��A!C�A��Ap;A\�A�Ao A�bAE9AjA�
A8�A�A��A��A!�Ah�A�AB�AM�A'RA��A�A&�A��A�CA��A��A�"A��A`�A	�9A�AbNA\�A!-A�~A�.A4nA�A�AQ�AuAA�A
=A�,A�A"hA��A��A��AP�A�A �A a�A  i@�n/@�h�@��a@��@��B@�ѷ@��@���@�@��M@�5�@��@��h@���@�}V@�_@��M@���@���@�E9@�}V@���@�E9@��@��?@�bN@�(�@�Q�@�L0@��6@�2a@�x@���@�@���@���@�Dg@�U�@�e@�oi@�c @��D@�>�@��a@�!�@�z@��D@߃{@�@O@�͟@�V@ܕ@��a@ۂ�@�n/@�\�@�RT@�B�@�;d@�8�@�o@ړu@ٙ�@إz@�zx@�7L@Ե@�g8@�/�@��]@�
�@ъ�@�L�@�P�@�Vm@�T�@�H�@�:�@� i@��@�J�@�C�@�%F@̞@�Q�@�@���@�T�@ʺ�@�^5@Ɉf@�u%@�6�@��@�e,@�V@ƂA@ŶF@ć�@�@�&@�V�@��;@�:�@�@�@���@�ԕ@�C�@�~@���@��@��s@�w�@�)�@���@�0�@��@��@��P@��r@�J@��A@�ƨ@��t@�l�@��/@�{@���@�ϫ@��"@�@�S�@���@�S@�>B@��j@���@���@�W?@�'�@��@���@�Q�@�d�@�U2@��>@��@�O�@�]d@�B[@�{@��]@���@��@���@��b@�Ta@�!�@�˒@���@��@�bN@��m@���@���@���@�qv@��$@��@�m]@���@�x�@���@���@�&�@��M@�/@���@��,@�4@�v`@�ѷ@��@���@�`�@�:�@�&�@�&�@�w2@�x@�F�@�U2@��Q@��z@���@���@���@��"@��~@���@�A�@��!@�&�@��z@��7@�S�@�>�@�%F@��`@�PH@��@���@��"@�l�@�E9@��8@��@���@�K^@�@��a@���@�o�@�^�@�2a@��6@�oi@�(�@���@��z@���@��@�Vm@� \@��@���@��]@���@��@��A@��o@�Z�@��@�O�@��@�#:@�M@��@��D@���@�O�@��P@���@��7@�k�@�0�@��L@�Ov@�@�@�8�@�6@�@���@���@��}@��z@��@�~�@�Y�@�(�@��E@��m@���@���@�U2@���@��$@�S&@�=�@�K�@�@@�� @�bN@��@��z@���@�a�@���@�kQ@�d�@��)@��@�W?@�9�@��@�d�@�?�@�3�@��@�J@��@�ݘ@��M@�F@�	l@���@��@�C-@��@�{J@�S�@�y�@��/@���@���@���@�y>@�	@��6@�c@�RT@�-w@�@��2@���@�YK@��@�{J@�x@�w2@�s�@�e�@��@���@��4@��+@�w�@���@���@���@��A@�>B@�x@��N@�`B@�q@��E@�i�@��@�g@E9@�@~\�@~4@}�@|:�@{�W@{�$@{iD@{/�@{�@z��@z�1@zq�@z4@y�@yVm@x�P@x(�@wy�@w8@w@w�@v�]@v��@u��@t�p@t4n@sMj@s@r�@r��@r��@r)�@qQ�@p�@o�r@o��@og�@oU�@o&@n�@n��@o$t@n��@n�@m�t@l��@l�@k�A@k��@j�2@j�@i��@i��@i`B@iN<@i(�@h֡@h�Y@hC-@g�
@g8@fں@f��@f\�@f8�@e��@e=�@e�@e�@e�@e�@d�I@d]d@d[�@c��@c)_@b��@bv�@b@a�@a��@b@a��@a+@`@_=@^v�@^{@]�9@]-w@] \@]^�@]=�@]?}@\��@\1'@[�
@[�@[�@[�
@[��@[@O@Z��@Z�!@Z:*@Z�@Y�o@Y�C@YIR@Y%F@X�O@W�+@Wqv@W]�@W;d@V�@V� @VO@T�5@T��@TM@Sg�@R�6@ROv@R_@Q��@Q0�@P�f@P��@P��@P�@PC-@P/�@O��@O�6@O�a@O�*@OiD@O=@O.I@N�B@N:*@M��@M�3@M�C@M�n@M��@M�'@M4@LM@L~@K� @Kx@K6z@K�@J�@I|@IB�@Hی@HS�@G�@Gخ@G��@G>�@F҉@Fu%@E�N@E(�@D�$@Dc�@DG@Cg�@B�@B�B@Bxl@B@�@B+k@A�@As�@A�@@�U@@��@@,=@?�@?�F@?b�@?�@>��@>�@>^5@>GE@>e@=�^@=��@=m]@<�P@<�@<M@<,=@;��@:��@:��@:~�@:��@:z@:V@9�9@9X@9@9+@8��@8��@8Ĝ@8��@8w�@8oi@8c�@8A�@8�@7�m@7��@7�@6�@6��@60U@5�^@5G�@4�E@4�p@4�?@4��@4��@4�@4~(@4,=@3�V@3�@2�h@2�@1��@1�@1�'@1V@0�@0֡@0�O@0(�@/��@/,�@.�@.ں@.�x@.^5@.?@-��@-��@-J�@,�[@,M@,<�@,2�@,!@,1@+�
@+�w@+�f@+@*�M@*�@*�m@*�\@*V@*&�@)��@)Q�@)&�@)�@(��@(e�@'S�@&�L@&@�@&
�@%�.@%��@%�@%��@%hs@%O�@$�@$Ft@#�@#��@#��@#��@#g�@#O@#/�@"�H@"��@"l�@"e@!�)@!�X@!c�@!&�@ �K@ �@ ��@ `�@ I�@ �@ƨ@��@l�@'�@�@�@ȴ@u%@W�@�9@`B@Ĝ@�_@j@U2@D�@�@�0@O@(@ߤ@�X@�6@}V@Z�@�@�.@rG@�@�U@��@tT@N�@7�@~@�;@�*@��@��@�P@iD@/�@�H@�'@�r@\�@1�@-@��@��@rG@o @`B@G�@*0@@@	l@�@�e@U2@M@�@�@�@�@b@  @�@�A@ݘ@�}@��@b�@�@�@��@�A@H�@u@�^@hs@:�@#�@�@�f@��@��@�|@��@�9@g8@C-@2�@�@�]@�6@��@��@�P@)_@��@��@��@�'@�}@��@q�@E�@)�@�o@�@��@�9@�^@��@x�@2a@V@�[@�9@��@�o@u�@_@4n@�@@@@x@�@�Q@��@Mj@+@�@�@
��@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}VB
}VB
}"B
}<B
|�B
|�B
|B
{�B
{B
{dB
{dB
{dB
{0B
z�B
zDB
y�B
y$B
x�B
w�B
v�B
v`B
u�B
s�B
l�B
gB
`vB
[�B
T{B
MjB
Q�B
l"B
zB
~]B
y�B
�B
�#B
�cB�B6zBQ B�oB�aB��B��B��B�B�B'RB�BeBB?B�B_B�=BݘB��B��B��B��B�B��B��B��B�ButBoOB_�BWsBR�BDgB72B)*BB
�B
��B
��B
�dB
�]B
�xB
t�B
abB
^�B
W�B
M6B
B[B
7LB
)�B
�B	�VB	�B	�_B	�B	�jB	��B	�zB	�B	��B	��B	�OB	|PB	q'B	j�B	e�B	[	B	S�B	J�B	9�B	;B	;�B	B�B	C-B	?�B	>�B	=B	=�B	A�B	IB	K�B	L0B	O�B	V�B	U�B	TB	S[B	V�B	VmB	S�B	PbB	L�B	O�B	UMB	\�B	aB	Z�B	T�B	PbB	V�B	SuB	W?B	^B	\�B	[�B	Z�B	[�B	\CB	W?B	O�B	LJB	PbB	WYB	c�B	f�B	jeB	l�B	i�B	d�B	_�B	i�B	dB	_;B	RTB	C-B	7LB	:DB	R�B	dZB	w�B	��B	y	B	e�B	bNB	eFB	f�B	cB	e,B	vB	yXB	zB	z�B	HB	��B	�pB	��B	��B	�kB	��B	��B	�;B	�3B	�2B	�2B	��B	�rB	��B	��B	��B	��B	�"B	��B	��B	��B	�B	ðB	�gB	��B	�mB	�mB	�SB	�mB	��B	ƎB	ǮB	�fB	�B	ȴB	�RB	��B	��B	�=B	��B	�6B	�\B	�NB	уB	�@B	�TB	�gB	�+B	��B	�dB	�/B	��B	��B	�kB	�:B	�4B	��B	�@B	��B	��B	�?B	�
B	��B	֡B	�2B	�B	��B	��B	��B	��B	��B	ؓB	�_B	�+B	�sB	�$B	ؓB	ڠB	�WB	ܬB	�]B	ܒB	�CB	�B	�!B	��B	��B	�NB	�B	�B	�B	�nB	�nB	�B	�B	�B	�8B	�8B	�8B	�8B	�B	��B	��B	�B	�B	�_B	�_B	��B	�KB	��B	�KB	��B	�B	��B	�AB	�-B	�aB	�B	��B	�9B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�HB
 �B
 �B
�B
�B
�B
�B
-B
B
�B
�B
zB
�B
	�B

rB
	�B
	B
�B
EB
EB
_B
�B
�B
B
fB
�B

	B
jB
�B
6B
�B
B
�B
�B
�B
dB
�B
�B
B
<B
�B
4B
NB
�B
�B
.B
 B
NB
B
�B
�B
,B
B
B
eB
B
�B
kB
eB
�B
�B
B
7B
�B
WB
B
�B
�B
�B
kB
B
qB
�B
�B
B
kB
kB
�B
�B
 BB
!bB
!�B
"hB
$B
$&B
#�B
#TB
%zB
'8B
&�B
&�B
&fB
&B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
*0B
*eB
*KB
*0B
*B
*eB
+B
,�B
,�B
,"B
,=B
,�B
,�B
-)B
-CB
-]B
-�B
.�B
/B
/B
/5B
/iB
/�B
/�B
0oB
1'B
2aB
2B
1B
1vB
1�B
2-B
3�B
5%B
3�B
6�B
9$B
9�B
9�B
9rB
9XB
:�B
:xB
:^B
:DB
:xB
:�B
<6B
<jB
<jB
=qB
?}B
@ B
@B
@�B
A;B
A;B
A;B
@�B
@OB
?�B
?�B
?�B
AUB
A�B
A�B
BB
A�B
AUB
AB
A�B
B�B
B�B
C�B
D�B
CGB
D�B
E�B
E�B
E�B
E�B
E�B
FB
F�B
G�B
H�B
I�B
J�B
KxB
K�B
L�B
LB
KDB
J�B
KB
M6B
L�B
L�B
L�B
L~B
M6B
LB
KDB
J�B
KB
K�B
LdB
M6B
M6B
M6B
MB
L�B
MPB
MPB
M�B
N�B
O(B
N�B
NpB
N�B
P.B
Q4B
S@B
S�B
TFB
T�B
T�B
T�B
T�B
UMB
U�B
V9B
U�B
U�B
VB
U�B
U�B
U�B
VSB
VmB
V�B
W$B
W$B
W?B
W$B
W
B
V�B
V�B
W?B
W�B
W�B
X+B
X_B
YB
Y�B
Z7B
ZkB
Z�B
Z�B
Z�B
Z�B
[	B
[�B
[�B
\�B
]dB
^B
]�B
]B
\�B
]�B
^B
^OB
^5B
^jB
^�B
_B
`�B
a�B
aHB
a�B
a�B
cB
c�B
c�B
c�B
c�B
d�B
d�B
d�B
d�B
d�B
eFB
ezB
ezB
e�B
f�B
f�B
gRB
g�B
g�B
g�B
h$B
h
B
h
B
h$B
h
B
g�B
g�B
g�B
h�B
h$B
g�B
gmB
g�B
h
B
h
B
h>B
h�B
g�B
h$B
g�B
gRB
gB
gRB
h
B
h�B
kB
kQB
kQB
k�B
l�B
l�B
n�B
o B
o B
o5B
o�B
pUB
poB
o�B
o�B
qB
qAB
p�B
p�B
p�B
p;B
o�B
o�B
o�B
o�B
pB
o�B
oOB
o B
n�B
nIB
n�B
n�B
n�B
n�B
o B
o�B
o�B
o�B
o�B
pB
o�B
p;B
p;B
p!B
pUB
p�B
p�B
p�B
p�B
q'B
qvB
qvB
qvB
qvB
q[B
qAB
q�B
r�B
s3B
s�B
s�B
s�B
sMB
tnB
t�B
t�B
t�B
utB
vB
vFB
vFB
v`B
vzB
vzB
v�B
wfB
w�B
xB
x8B
x�B
y	B
y$B
yXB
y�B
yrB
y�B
z*B
z�B
z�B
z�B
{B
{JB
{JB
{dB
{�B
|B
|6B
|PB
|PB
|jB
|�B
|�B
}B
}qB
}�B
}�B
}�B
~(B
~�B
~�B
~�B
B
~�B
B
�B
�B
�B
�B
�OB
�iB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�UB
��B
��B
�B
�AB
��B
��B
��B
��B
�B
�B
�B
�B
�aB
��B
�B
�3B
��B
��B
�B
�B
��B
��B
��B
��B
�B
�tB
��B
��B
��B
�+B
�_B
�_B
��B
��B
�B
��B
�7B
�RB
�lB
�lB
��B
��B
��B
��B
�=B
�#B
�	B
�=B
�=B
�XB
�rB
��B
�DB
�DB
�DB
�^B
��B
�B
�PB
��B
��B
��B
��B
��B
��B
�"B
�B
��B
��B
�BB
�\B
�\B
��B
��B
��B
��B
�B
�B
�HB
��B
��B
� B
�B
��B
��B
� B
�:B
�TB
�TB
��B
�B
�@B
��B
�B
�B
�B
��B
�B
��B
��B
�B
��B
��B
��B
��B
��B
��B
�
B
��B
��B
�EB
��B
�B
�B
��B
�KB
�eB
�B
��B
��B
��B
�#B
�=B
�=B
�WB
��B
��B
�B
�B
�B
�)B
�]B
��B
��B
�B
�/B
�dB
�IB
��B
�B
�B
�B
�B
�B
�5B
�5B
�5B
�OB
��B
��B
�B
�B
�B
��B
��B
�B
�B
�!B
�B
�!B
�!B
�VB
��B
��B
��B
��B
�BB
�BB
��B
��B
��B
�-B
�bB
�bB
��B
�|B
�|B
�|B
�|B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
��B
��B
��B
��B
�nB
��B
��B
��B
�B
�@B
�ZB
�tB
�ZB
�tB
��B
�ZB
��B
�B
�`B
��B
��B
��B
��B
��B
�B
�2B
�LB
�LB
�LB
�fB
�LB
�LB
��B
��B
��B
�B
�B
�B
�B
�m3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333  B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}VB
}VB
}"B
}<B
|�B
|�B
|B
{�B
{B
{dB
{dB
{dB
{0B
z�B
zDB
y�B
y$B
x�B
w�B
v�B
v`B
u�B
s�B
l�B
gB
`vB
[�B
T{B
MjB
Q�B
l"B
zB
~]B
y�B
�B
�#B
�cB�B6zBQ B�oB�aB��B��B��B�B�B'RB�BeBB?B�B_B�=BݘB��B��B��B��B�B��B��B��B�ButBoOB_�BWsBR�BDgB72B)*BB
�B
��B
��B
�dB
�]B
�xB
t�B
abB
^�B
W�B
M6B
B[B
7LB
)�B
�B	�VB	�B	�_B	�B	�jB	��B	�zB	�B	��B	��B	�OB	|PB	q'B	j�B	e�B	[	B	S�B	J�B	9�B	;B	;�B	B�B	C-B	?�B	>�B	=B	=�B	A�B	IB	K�B	L0B	O�B	V�B	U�B	TB	S[B	V�B	VmB	S�B	PbB	L�B	O�B	UMB	\�B	aB	Z�B	T�B	PbB	V�B	SuB	W?B	^B	\�B	[�B	Z�B	[�B	\CB	W?B	O�B	LJB	PbB	WYB	c�B	f�B	jeB	l�B	i�B	d�B	_�B	i�B	dB	_;B	RTB	C-B	7LB	:DB	R�B	dZB	w�B	��B	y	B	e�B	bNB	eFB	f�B	cB	e,B	vB	yXB	zB	z�B	HB	��B	�pB	��B	��B	�kB	��B	��B	�;B	�3B	�2B	�2B	��B	�rB	��B	��B	��B	��B	�"B	��B	��B	��B	�B	ðB	�gB	��B	�mB	�mB	�SB	�mB	��B	ƎB	ǮB	�fB	�B	ȴB	�RB	��B	��B	�=B	��B	�6B	�\B	�NB	уB	�@B	�TB	�gB	�+B	��B	�dB	�/B	��B	��B	�kB	�:B	�4B	��B	�@B	��B	��B	�?B	�
B	��B	֡B	�2B	�B	��B	��B	��B	��B	��B	ؓB	�_B	�+B	�sB	�$B	ؓB	ڠB	�WB	ܬB	�]B	ܒB	�CB	�B	�!B	��B	��B	�NB	�B	�B	�B	�nB	�nB	�B	�B	�B	�8B	�8B	�8B	�8B	�B	��B	��B	�B	�B	�_B	�_B	��B	�KB	��B	�KB	��B	�B	��B	�AB	�-B	�aB	�B	��B	�9B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�HB
 �B
 �B
�B
�B
�B
�B
-B
B
�B
�B
zB
�B
	�B

rB
	�B
	B
�B
EB
EB
_B
�B
�B
B
fB
�B

	B
jB
�B
6B
�B
B
�B
�B
�B
dB
�B
�B
B
<B
�B
4B
NB
�B
�B
.B
 B
NB
B
�B
�B
,B
B
B
eB
B
�B
kB
eB
�B
�B
B
7B
�B
WB
B
�B
�B
�B
kB
B
qB
�B
�B
B
kB
kB
�B
�B
 BB
!bB
!�B
"hB
$B
$&B
#�B
#TB
%zB
'8B
&�B
&�B
&fB
&B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
*0B
*eB
*KB
*0B
*B
*eB
+B
,�B
,�B
,"B
,=B
,�B
,�B
-)B
-CB
-]B
-�B
.�B
/B
/B
/5B
/iB
/�B
/�B
0oB
1'B
2aB
2B
1B
1vB
1�B
2-B
3�B
5%B
3�B
6�B
9$B
9�B
9�B
9rB
9XB
:�B
:xB
:^B
:DB
:xB
:�B
<6B
<jB
<jB
=qB
?}B
@ B
@B
@�B
A;B
A;B
A;B
@�B
@OB
?�B
?�B
?�B
AUB
A�B
A�B
BB
A�B
AUB
AB
A�B
B�B
B�B
C�B
D�B
CGB
D�B
E�B
E�B
E�B
E�B
E�B
FB
F�B
G�B
H�B
I�B
J�B
KxB
K�B
L�B
LB
KDB
J�B
KB
M6B
L�B
L�B
L�B
L~B
M6B
LB
KDB
J�B
KB
K�B
LdB
M6B
M6B
M6B
MB
L�B
MPB
MPB
M�B
N�B
O(B
N�B
NpB
N�B
P.B
Q4B
S@B
S�B
TFB
T�B
T�B
T�B
T�B
UMB
U�B
V9B
U�B
U�B
VB
U�B
U�B
U�B
VSB
VmB
V�B
W$B
W$B
W?B
W$B
W
B
V�B
V�B
W?B
W�B
W�B
X+B
X_B
YB
Y�B
Z7B
ZkB
Z�B
Z�B
Z�B
Z�B
[	B
[�B
[�B
\�B
]dB
^B
]�B
]B
\�B
]�B
^B
^OB
^5B
^jB
^�B
_B
`�B
a�B
aHB
a�B
a�B
cB
c�B
c�B
c�B
c�B
d�B
d�B
d�B
d�B
d�B
eFB
ezB
ezB
e�B
f�B
f�B
gRB
g�B
g�B
g�B
h$B
h
B
h
B
h$B
h
B
g�B
g�B
g�B
h�B
h$B
g�B
gmB
g�B
h
B
h
B
h>B
h�B
g�B
h$B
g�B
gRB
gB
gRB
h
B
h�B
kB
kQB
kQB
k�B
l�B
l�B
n�B
o B
o B
o5B
o�B
pUB
poB
o�B
o�B
qB
qAB
p�B
p�B
p�B
p;B
o�B
o�B
o�B
o�B
pB
o�B
oOB
o B
n�B
nIB
n�B
n�B
n�B
n�B
o B
o�B
o�B
o�B
o�B
pB
o�B
p;B
p;B
p!B
pUB
p�B
p�B
p�B
p�B
q'B
qvB
qvB
qvB
qvB
q[B
qAB
q�B
r�B
s3B
s�B
s�B
s�B
sMB
tnB
t�B
t�B
t�B
utB
vB
vFB
vFB
v`B
vzB
vzB
v�B
wfB
w�B
xB
x8B
x�B
y	B
y$B
yXB
y�B
yrB
y�B
z*B
z�B
z�B
z�B
{B
{JB
{JB
{dB
{�B
|B
|6B
|PB
|PB
|jB
|�B
|�B
}B
}qB
}�B
}�B
}�B
~(B
~�B
~�B
~�B
B
~�B
B
�B
�B
�B
�B
�OB
�iB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�UB
��B
��B
�B
�AB
��B
��B
��B
��B
�B
�B
�B
�B
�aB
��B
�B
�3B
��B
��B
�B
�B
��B
��B
��B
��B
�B
�tB
��B
��B
��B
�+B
�_B
�_B
��B
��B
�B
��B
�7B
�RB
�lB
�lB
��B
��B
��B
��B
�=B
�#B
�	B
�=B
�=B
�XB
�rB
��B
�DB
�DB
�DB
�^B
��B
�B
�PB
��B
��B
��B
��B
��B
��B
�"B
�B
��B
��B
�BB
�\B
�\B
��B
��B
��B
��B
�B
�B
�HB
��B
��B
� B
�B
��B
��B
� B
�:B
�TB
�TB
��B
�B
�@B
��B
�B
�B
�B
��B
�B
��B
��B
�B
��B
��B
��B
��B
��B
��B
�
B
��B
��B
�EB
��B
�B
�B
��B
�KB
�eB
�B
��B
��B
��B
�#B
�=B
�=B
�WB
��B
��B
�B
�B
�B
�)B
�]B
��B
��B
�B
�/B
�dB
�IB
��B
�B
�B
�B
�B
�B
�5B
�5B
�5B
�OB
��B
��B
�B
�B
�B
��B
��B
�B
�B
�!B
�B
�!B
�!B
�VB
��B
��B
��B
��B
�BB
�BB
��B
��B
��B
�-B
�bB
�bB
��B
�|B
�|B
�|B
�|B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
��B
��B
��B
��B
�nB
��B
��B
��B
�B
�@B
�ZB
�tB
�ZB
�tB
��B
�ZB
��B
�B
�`B
��B
��B
��B
��B
��B
�B
�2B
�LB
�LB
�LB
�fB
�LB
�LB
��B
��B
��B
�B
�B
�B
�B
�m3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230723124414  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230723124415  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230723124416  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230723124416                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230723124416  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230723124416  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230723125718                      G�O�G�O�G�O�                JA  COOAcooa1.0                                                                 20230803071839  CF  PSAL            D�� D��3?�                  JA  COOAcooa1.0                                                                 20230803071839  CF  PSAL            @���D�� ?�                  JA  COOAcooa1.0                                                                 20230803071839  CF  PSAL_ADJUSTED   D�  D��3?�                  JA  COOAcooa1.0                                                                 20230803071839  CF  PSAL_ADJUSTED   @���D�� ?�                  JA  ARUP                                                                        20230803081500                      G�O�G�O�G�O�                