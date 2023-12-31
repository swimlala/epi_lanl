CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:39:08Z creation;2022-06-04T17:39:08Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604173908  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               \A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @ًz����1   @ً{ ���@-�I�^5�cR-V1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B���B�  B�  C   C  C  C  C  C
  C  C  C�C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Ci�fCl  Cn  Cp  Cr  Ct  Cv�Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~�fDfD� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @<��@|��@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bp33Bw��B��B��fB��fB��fB��fB��B��fB�� B��fB��fB��fB��fB��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB�3B�3B��fB�3B��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�C�C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C<�C=ٙC?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3CiٙCk�3Cm�3Co�3Cq�3Cs�3Cv�Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~�3D3D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD���D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�{3D��3D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�h 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A�AA��A��jAֱ'Aְ!A֋�A�tTA�k�A�OA�7A�1A�M�A��AҊ	A�g�A�*0A��9A�3hAФtAЅ�A�Z�A�@�A�4�A�)_A��A���A��A���A��A��tAϼ�Aϸ�AϷLAϲ-Aϫ6AϘ�Aτ�A��Aʤ@A���AŞOA�n�AýqADA�49A��A�QA��nA��5A�8�A��3A�F?A�A�ӏA���A��A�L0A���A�r�A��mA���A��A��A�1A�4nA�IRA���A�,A���A��A��\A��?A�+�A�>wA�s�A�W?A�уA��jA���A���A���A���A� �A�j�A��A���A�m)A���A�n�A�Z�A�&�A���A}_pAw�At��Ar	�Al�wAh_Ag
�Af�Ad��Aa�A`��A_jA[\)AW��AU|�AS$tAQ��APOvAM�gAJ�dAIu�AG'�AAY�A>  A<~(A:r�A9��A8j�A6��A4�>A3,=A2Z�A1m�A18�A12�A0�)A/��A.+kA+
�A(�A("hA(A&a|A%p�A#rGA#�bA!��A��A�A�A�As�Az�A�AFA�A�A�vA�KA�A \A�A�A�vAVATaA��A�mA�A��A��AZ�A�A:�A�*A2�A� A�RA��A|�A_A@A}VA�XAVA�AɆAy�A�yA[�A8A(�A�AYA��A%A��AU�A�$A7A
�CA
�=A
�A	��A�"Aw2Aw�AA�A�yA�A��A`BA�LAo�A�AT�A�/AɆA�pA��A�A:�A � A ��A %F@�Q�@�@�1�@���@��H@��@�b@��}@���@�@�Ɇ@���@���@�_p@���@��@��j@��A@���@�5?@�X@�^�@���@�@�(�@�  @��@�"@�A�@�%@���@�5?@�خ@�p�@��@���@�Mj@���@�tT@�C-@�GE@��@낪@귀@�#:@�h@��5@�@�E�@��j@��E@��@�&�@�M�@�$�@㹌@��p@�kQ@�[@��@ߨX@�X�@�-w@��m@�1@ݗ�@���@�O@ۗ�@�v�@�S�@�~(@׃{@�!-@��'@��@�{J@��|@�tT@��@�N<@ҵ@�e�@�-�@�u�@�ѷ@�e�@��g@�*0@Α @�{@���@̃@��6@��@ʕ@��@���@ɛ=@ɨX@���@�t�@Ȱ!@ȋD@Ƿ�@�o@��@��@Ƶ�@�H@���@ż@�`B@ĵ�@�S�@�/�@���@�5�@��@º�@ªe@_@�s�@�3�@��H@�,�@��]@�{�@���@���@�`B@�Y@��`@�1'@��@��-@��{@��@��O@���@�l�@��@��K@�X@��@�4@���@���@�@O@��@���@�z�@���@�hs@��@�o @���@�D�@�_@�R�@�H@��-@�$t@��!@�=q@���@�ں@��F@�U2@��@�X@��@��?@��@���@� �@�ԕ@���@�|�@�/@��!@�Xy@�!�@���@�{J@���@���@�6@��*@�'�@��@���@��@��K@�%F@�bN@�x@��^@���@�1�@��@�J@���@�1�@��M@��@�Q@���@��#@��0@��f@�B�@��@�S�@��@��@�� @��:@�J#@��@���@��A@�1'@�@���@��h@�O@�o@��+@�PH@���@�*0@��@��s@��'@��1@�y>@�r�@�.�@�x@��<@�V@�+k@��r@���@��0@�f�@��m@���@�YK@�1�@���@��M@�F@�E9@�O�@�S�@�B�@���@��O@��@��	@�1'@���@��D@�x@�W�@�C�@�&�@�$@��#@��w@���@��:@�{J@��@�xl@�@�@���@���@�_p@��@���@�K^@��@�@���@��x@�[�@� �@��m@��@��S@�O�@��/@�r�@�V�@�j@�V@��@��M@��'@�>B@�1@��a@�a�@�+@��f@�ߤ@��9@�[�@�
�@��H@��=@�c@�8�@���@���@���@�?@�1@���@�ԕ@���@���@�E9@�
=@��v@���@���@�s�@�`�@�@�ݘ@�� @��6@���@��S@�Y�@�IR@�IR@�0�@�(@��@�%@��|@��h@�J�@�G@���@��X@��{@�#�@���@���@�\�@�5?@�x@Mj@~��@~@�@}�-@}4@|�o@{��@z�@z	@yA @x�K@x��@x/�@w�@w��@wW?@v� @v@u��@us�@uO�@u@@t�E@t��@ty>@s�m@sRT@s�@r�@r��@rR�@q��@q��@qS&@p�E@p:�@o�w@oMj@n�"@n��@nV@m��@l��@l>B@ke�@k@j�@j�'@j��@j�@i�-@i|@iS&@i@h��@hc�@h@g��@gC�@f��@f��@fi�@f@e�-@eDg@d�@dbN@c�r@c��@c�4@c_p@c=@b�R@bff@b3�@a�3@a+@`>B@_��@_�@^��@]�D@]*0@\��@[�@[�6@[��@[=@Z��@Z��@ZYK@Z@YrG@Y#�@Y�@X��@X�@X7@W��@W/�@V��@Vp;@V0U@U�@U��@U[W@T��@Tr�@T4n@S�Q@S\)@SC@R{�@R5?@R_@Qԕ@Q�~@Q�@P  @OA�@O�@O�@N�"@N��@N�H@N�m@NkQ@M�@Mc�@M�@L��@L�)@L��@Lm�@K�@Kn/@J��@J�L@JJ�@J)�@Ju@I�'@IG�@H��@H�j@H�_@H?�@G�@G��@GH�@F��@Fu%@F=q@E�t@Ea�@E4@D�K@DM@D$@C�@B�@A�@ArG@A�@@��@@?�@?��@?)_@>ߤ@>�@>��@>Ta@=Y�@<�5@<�p@<��@<Z@<7@;��@;A�@;�@:�@:�r@:��@:ff@9�@9�@94@8�K@8��@8�9@8�@7��@7�*@7v`@6��@6�h@6^5@5�@5IR@4�E@4��@4U2@4$@3��@3��@3v`@3'�@2�L@2�@1�j@1�@1�=@1`B@17L@1q@0�f@0�@02�@/� @/4�@.��@.��@.�b@._�@.6�@.($@.{@-�)@-�n@-`B@-=�@-+@,�O@,N�@,1@+��@+�@+S�@+S@*�"@+@*�c@*��@*L0@*�@)��@)��@):�@(��@(r�@(>B@'�@'ƨ@'x@'33@&�@&��@&�1@&u%@&Ov@&e@%��@%��@%��@%Vm@$ѷ@$�o@$�.@$6@#�@#��@#j�@#$t@"�]@"Ta@"�@"
�@"	@!�3@!%F@ �K@ �@ r�@ V�@ Xy@ >B@ *�@ �@ 1@ݘ@��@P�@ߤ@GE@�@�"@<6@�@��@��@]d@7�@!@�@� @��@dZ@A�@+@�@�M@��@��@u%@Ov@�@��@��@|@<6@�@��@c�@!@��@�[@v`@F�@>�@Y@�@��@��@u%@�@@w2@Dg@ \@�v@y>@1'@1@�W@��@.I@�@��@�!@��@$�@�@��@��@|@f�@?}@@�/@�U@�o@u�@"h@  @��@��@��@�f@>�@�X@u%@V@E�@1�@�@�'@c@p�@%F@�@�@�	@�$@I�@�@�@�f@Mj@+@
�c@
�X@
��@
�b@
��@
1�@
)�@
�@
_@	��@	��@	�M@	X@	G�@	=�@	*0@	�@	�@��@�f@�/@��@~(@I�@~@�A@�;@�}@��@��@�F@�k@o�@U�@H�@�@�,@�<@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A�AA��A��jAֱ'Aְ!A֋�A�tTA�k�A�OA�7A�1A�M�A��AҊ	A�g�A�*0A��9A�3hAФtAЅ�A�Z�A�@�A�4�A�)_A��A���A��A���A��A��tAϼ�Aϸ�AϷLAϲ-Aϫ6AϘ�Aτ�A��Aʤ@A���AŞOA�n�AýqADA�49A��A�QA��nA��5A�8�A��3A�F?A�A�ӏA���A��A�L0A���A�r�A��mA���A��A��A�1A�4nA�IRA���A�,A���A��A��\A��?A�+�A�>wA�s�A�W?A�уA��jA���A���A���A���A� �A�j�A��A���A�m)A���A�n�A�Z�A�&�A���A}_pAw�At��Ar	�Al�wAh_Ag
�Af�Ad��Aa�A`��A_jA[\)AW��AU|�AS$tAQ��APOvAM�gAJ�dAIu�AG'�AAY�A>  A<~(A:r�A9��A8j�A6��A4�>A3,=A2Z�A1m�A18�A12�A0�)A/��A.+kA+
�A(�A("hA(A&a|A%p�A#rGA#�bA!��A��A�A�A�As�Az�A�AFA�A�A�vA�KA�A \A�A�A�vAVATaA��A�mA�A��A��AZ�A�A:�A�*A2�A� A�RA��A|�A_A@A}VA�XAVA�AɆAy�A�yA[�A8A(�A�AYA��A%A��AU�A�$A7A
�CA
�=A
�A	��A�"Aw2Aw�AA�A�yA�A��A`BA�LAo�A�AT�A�/AɆA�pA��A�A:�A � A ��A %F@�Q�@�@�1�@���@��H@��@�b@��}@���@�@�Ɇ@���@���@�_p@���@��@��j@��A@���@�5?@�X@�^�@���@�@�(�@�  @��@�"@�A�@�%@���@�5?@�خ@�p�@��@���@�Mj@���@�tT@�C-@�GE@��@낪@귀@�#:@�h@��5@�@�E�@��j@��E@��@�&�@�M�@�$�@㹌@��p@�kQ@�[@��@ߨX@�X�@�-w@��m@�1@ݗ�@���@�O@ۗ�@�v�@�S�@�~(@׃{@�!-@��'@��@�{J@��|@�tT@��@�N<@ҵ@�e�@�-�@�u�@�ѷ@�e�@��g@�*0@Α @�{@���@̃@��6@��@ʕ@��@���@ɛ=@ɨX@���@�t�@Ȱ!@ȋD@Ƿ�@�o@��@��@Ƶ�@�H@���@ż@�`B@ĵ�@�S�@�/�@���@�5�@��@º�@ªe@_@�s�@�3�@��H@�,�@��]@�{�@���@���@�`B@�Y@��`@�1'@��@��-@��{@��@��O@���@�l�@��@��K@�X@��@�4@���@���@�@O@��@���@�z�@���@�hs@��@�o @���@�D�@�_@�R�@�H@��-@�$t@��!@�=q@���@�ں@��F@�U2@��@�X@��@��?@��@���@� �@�ԕ@���@�|�@�/@��!@�Xy@�!�@���@�{J@���@���@�6@��*@�'�@��@���@��@��K@�%F@�bN@�x@��^@���@�1�@��@�J@���@�1�@��M@��@�Q@���@��#@��0@��f@�B�@��@�S�@��@��@�� @��:@�J#@��@���@��A@�1'@�@���@��h@�O@�o@��+@�PH@���@�*0@��@��s@��'@��1@�y>@�r�@�.�@�x@��<@�V@�+k@��r@���@��0@�f�@��m@���@�YK@�1�@���@��M@�F@�E9@�O�@�S�@�B�@���@��O@��@��	@�1'@���@��D@�x@�W�@�C�@�&�@�$@��#@��w@���@��:@�{J@��@�xl@�@�@���@���@�_p@��@���@�K^@��@�@���@��x@�[�@� �@��m@��@��S@�O�@��/@�r�@�V�@�j@�V@��@��M@��'@�>B@�1@��a@�a�@�+@��f@�ߤ@��9@�[�@�
�@��H@��=@�c@�8�@���@���@���@�?@�1@���@�ԕ@���@���@�E9@�
=@��v@���@���@�s�@�`�@�@�ݘ@�� @��6@���@��S@�Y�@�IR@�IR@�0�@�(@��@�%@��|@��h@�J�@�G@���@��X@��{@�#�@���@���@�\�@�5?@�x@Mj@~��@~@�@}�-@}4@|�o@{��@z�@z	@yA @x�K@x��@x/�@w�@w��@wW?@v� @v@u��@us�@uO�@u@@t�E@t��@ty>@s�m@sRT@s�@r�@r��@rR�@q��@q��@qS&@p�E@p:�@o�w@oMj@n�"@n��@nV@m��@l��@l>B@ke�@k@j�@j�'@j��@j�@i�-@i|@iS&@i@h��@hc�@h@g��@gC�@f��@f��@fi�@f@e�-@eDg@d�@dbN@c�r@c��@c�4@c_p@c=@b�R@bff@b3�@a�3@a+@`>B@_��@_�@^��@]�D@]*0@\��@[�@[�6@[��@[=@Z��@Z��@ZYK@Z@YrG@Y#�@Y�@X��@X�@X7@W��@W/�@V��@Vp;@V0U@U�@U��@U[W@T��@Tr�@T4n@S�Q@S\)@SC@R{�@R5?@R_@Qԕ@Q�~@Q�@P  @OA�@O�@O�@N�"@N��@N�H@N�m@NkQ@M�@Mc�@M�@L��@L�)@L��@Lm�@K�@Kn/@J��@J�L@JJ�@J)�@Ju@I�'@IG�@H��@H�j@H�_@H?�@G�@G��@GH�@F��@Fu%@F=q@E�t@Ea�@E4@D�K@DM@D$@C�@B�@A�@ArG@A�@@��@@?�@?��@?)_@>ߤ@>�@>��@>Ta@=Y�@<�5@<�p@<��@<Z@<7@;��@;A�@;�@:�@:�r@:��@:ff@9�@9�@94@8�K@8��@8�9@8�@7��@7�*@7v`@6��@6�h@6^5@5�@5IR@4�E@4��@4U2@4$@3��@3��@3v`@3'�@2�L@2�@1�j@1�@1�=@1`B@17L@1q@0�f@0�@02�@/� @/4�@.��@.��@.�b@._�@.6�@.($@.{@-�)@-�n@-`B@-=�@-+@,�O@,N�@,1@+��@+�@+S�@+S@*�"@+@*�c@*��@*L0@*�@)��@)��@):�@(��@(r�@(>B@'�@'ƨ@'x@'33@&�@&��@&�1@&u%@&Ov@&e@%��@%��@%��@%Vm@$ѷ@$�o@$�.@$6@#�@#��@#j�@#$t@"�]@"Ta@"�@"
�@"	@!�3@!%F@ �K@ �@ r�@ V�@ Xy@ >B@ *�@ �@ 1@ݘ@��@P�@ߤ@GE@�@�"@<6@�@��@��@]d@7�@!@�@� @��@dZ@A�@+@�@�M@��@��@u%@Ov@�@��@��@|@<6@�@��@c�@!@��@�[@v`@F�@>�@Y@�@��@��@u%@�@@w2@Dg@ \@�v@y>@1'@1@�W@��@.I@�@��@�!@��@$�@�@��@��@|@f�@?}@@�/@�U@�o@u�@"h@  @��@��@��@�f@>�@�X@u%@V@E�@1�@�@�'@c@p�@%F@�@�@�	@�$@I�@�@�@�f@Mj@+@
�c@
�X@
��@
�b@
��@
1�@
)�@
�@
_@	��@	��@	�M@	X@	G�@	=�@	*0@	�@	�@��@�f@�/@��@~(@I�@~@�A@�;@�}@��@��@�F@�k@o�@U�@H�@�@�,@�<@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
}VB
zB
z*B
y�B
y$B
y$B
x�B
xlB
x�B
{0B
�B
�+B
�jB
�B
�RB
˒B
οB
��B
�B
�#B
��B
�B
��B
��B
�_B
ևB
�oB
ϑB
�B
��B
�(B
�(B
��B
��B
οB
οB
ΥB
͹B
��B
��B
��B
��B
��B
��B
�cB
��B
�\B
�KB
�B?B]B,WB;�BB�Bc�B~]B�B��B�?B�hB�KB��B�gBּB�9B�@B�BԕB�
B�#B��B�@B�B�/B�1BðB�B��Bq�B]�BF�B8�B'�B�BMB�B
�:B
�6B
�B
��B
n�B
W?B
8�B
YB	��B	��B	�hB	��B	B	u�B	rB	i�B	Z7B	RoB	H�B	9�B	&�B	�B	2B	�B	
rB	[B�B�B�>B��B�B�B�B� B��B�|B�B�B��B�6B��B	 �B	�B	
�B	7B	�B��B�dB	�B	B		B	�B	�B	FB�jB��B�B�%B	�B	AoB	R�B	\�B	^�B	e,B	oB	�B	�MB	��B	��B	�B	��B	�eB	��B	��B	��B	��B	�B	��B	�B	�HB	�jB	��B	�VB	�VB	�VB	��B	��B	��B	�$B	��B	��B	�RB	�	B	�B	��B	�ZB	��B	��B	��B	��B	�B	�AB	��B	��B	��B	�B	��B	��B	�UB	�hB	�9B	��B	�)B	�DB	��B	��B	�>B	�$B	��B	��B	��B	�XB	��B	��B	��B	��B	�vB	�8B	�zB	�3B	�TB	� B	�JB	��B	�LB	��B	��B	�}B	ªB	��B	żB	�tB	�zB	ǮB	�1B	��B	ȀB	��B	�7B	ȀB	��B	ɺB	��B	�rB	�#B	�rB	�	B	ɆB	�RB	�lB	�lB	�RB	�lB	ɠB	�B	��B	��B	�B	�lB	�	B	�	B	�=B	��B	�RB	�	B	��B	�dB	�jB	ΊB	ΊB	ΥB	ΊB	ϑB	�B	��B	�}B	�.B	ϫB	ϫB	�\B	��B	�vB	͹B	�6B	��B	��B	�MB	ԕB	՛B	�$B	ּB	�sB	�$B	�B	յB	�gB	��B	өB	�oB	�@B	��B	ּB	�
B	��B	ؓB	��B	�1B	�kB	�B	�B	�_B	�+B	ؓB	�B	�qB	یB	ٚB	��B	�sB	�
B	֡B	ؓB	ۦB	�bB	��B	޸B	޸B	��B	�|B	��B	�B	�B	�8B	�B	��B	�B	��B	�
B	��B	�B	��B	�KB	��B	�yB	�B	�_B	�B	�KB	��B	�B	��B	�IB	��B	�B	�/B	��B	�UB	�iB	�B	�B	� B	��B	�B	�B	�AB	�B	�`B	��B	�+B	�aB	�B	� B	��B	��B	�B	�CB	�B	��B	�B	��B	��B	��B	�+B	�B	�B	�tB	�B	�B	�aB	�B	��B	�GB	��B	��B	�B	�AB	�-B	�B	�aB	�B	��B	�B	�B	�ZB	��B	��B	�fB	��B	��B	��B	�PB	�<B	�"B	��B	��B	��B	��B
B
�B
�B
{B
gB
?B
?B
�B
�B
SB
SB
B
9B
SB
B
SB
�B
%B
%B
SB
�B
gB
�B
{B
�B
B
�B
�B
gB
3B
MB
9B
B
gB
3B
�B
�B
B
�B
�B
SB
9B
B
B
mB
�B
�B
�B
�B
mB
�B
?B
�B
+B
�B
_B
�B
�B
	�B

#B

	B
)B
�B
�B
<B
pB
}B
�B
B
.B
 B
�B
�B
KB
=B
xB
�B
�B
�B
dB
�B
�B
B
�B
�B
5B
�B
5B
;B
 BB
 B
!HB
 �B
!|B
!�B
"NB
"�B
"�B
"�B
#:B
# B
#TB
#:B
#�B
#�B
%�B
%�B
%`B
$�B
#�B
$�B
$�B
%�B
&2B
&2B
&LB
&2B
&�B
&�B
&�B
&LB
&�B
&�B
&�B
&�B
'�B
'�B
($B
'�B
(�B
(sB
(sB
)�B
)_B
*eB
)yB
*0B
*�B
*B
+B
*�B
*B
*B
*KB
+�B
+�B
+QB
,WB
,�B
,WB
,=B
,qB
,qB
,WB
,�B
-CB
,�B
-�B
.cB
/�B
/�B
0�B
1B
1vB
2�B
1�B
2�B
2�B
3�B
3MB
3�B
3�B
4TB
5�B
5�B
4�B
5�B
5ZB
5?B
6FB
6+B
6FB
6�B
7LB
6�B
7�B
7B
7LB
6�B
7fB
8B
8B
8lB
8�B
9�B
9XB
8�B
8RB
9XB
:B
9>B
:�B
;dB
<B
;�B
<�B
=B
=VB
>B
=�B
=�B
>B
>�B
>]B
>�B
>�B
>�B
>�B
?HB
?�B
?}B
?HB
@4B
@iB
?�B
@�B
@iB
@iB
@�B
@�B
AB
A;B
AUB
AoB
AoB
A B
BB
BB
A�B
B'B
B�B
B�B
C-B
C�B
C�B
D�B
E�B
F?B
F�B
F�B
F?B
GB
GEB
F�B
GB
F�B
G�B
G�B
G�B
GEB
G�B
H�B
IB
I�B
I�B
J=B
I�B
J�B
J#B
J=B
J�B
J�B
KDB
L0B
K�B
K�B
L�B
L�B
MjB
M6B
MB
M�B
NpB
OB
N�B
N�B
OvB
O(B
OvB
N�B
N�B
O(B
O�B
PbB
P}B
P�B
P.B
P}B
P�B
Q�B
QNB
Q4B
RoB
RB
RB
RTB
R�B
R�B
R�B
R�B
S@B
S@B
S[B
S�B
S�B
T{B
TaB
T�B
UB
UB
UgB
V9B
U�B
VB
V�B
W?B
W?B
W
B
WsB
V�B
WsB
WYB
WYB
W$B
W?B
W$B
XB
X�B
X_B
X_B
X_B
X�B
X�B
YKB
YB
XyB
YeB
XyB
YB
YB
Y�B
Y�B
ZB
Y�B
Y�B
Y�B
ZkB
ZQB
ZB
Y�B
ZB
[#B
[qB
[qB
[WB
\B
\)B
[�B
\B
\�B
\�B
\�B
]/B
]~B
^B
^5B
^�B
_pB
_VB
^�B
^�B
_�B
`'B
`vB
aHB
abB
`�B
aB
a�B
a�B
a�B
b4B
a�B
bhB
b�B
b�B
c B
c:B
cTB
c�B
c:B
c:B
c�B
dB
d@B
c�B
cnB
e�B
e`B
fB
f2B
f�B
ffB
f�B
gB
gB
g�B
g8B
g�B
h
B
hXB
h�B
h$B
hsB
i*B
i�B
h�B
h�B
iB
i�B
jKB
j0B
jB
j�B
jeB
jB
j�B
kkB
l�B
mCB
mCB
mCB
l�B
m�B
l=B
lWB
lWB
m�B
m�B
m)B
m�B
m]B
m]B
m)B
m)B
nIB
nIB
m�B
n�B
o5B
n�B
n�B
oB
oOB
oiB
o B
o�B
poB
p�B
q'B
rGB
rGB
r�B
r�B
s3B
t9B
uB
t�B
tB
tnB
t�B
u�B
u�B
u?B
u�B
utB
vzB
wB
wB
v`B
w2B
v�B
w�B
wfB
w�B
xB
w�B
w�B
x8B
x�B
x�B
y	B
y$B
y>B
yrB
z*B
y�B
zB
z^B
z�B
{B
{B
{0B
{B
{0B
|B
{�B
|PB
|B
|�B
|�B
|�B
}B
}B
}VB
}�B
}qB
}�B
}�B
}qB
}�B
}�B
}�B
~wB
}B
~�B
~�B
~�B
~wB
~�B
�B
.B
~�B
cB
~�B
�B
.B
�B
.B
�B
�4B
� B
�4B
�B
��B
�B
�B
�;B
�oB
��B
��B
�[B
��B
��B
�GB
�GB
�{B
��B
��B
��B
��B
��B
��B
��B
�gB
�MB
�B
��B
�9B
�9B
�9B
�mB
�SB
�9B
�mB
��B
��B
��B
��B
�YB
�YB
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
}VB
zB
z*B
y�B
y$B
y$B
x�B
xlB
x�B
{0B
�B
�+B
�jB
�B
�RB
˒B
οB
��B
�B
�#B
��B
�B
��B
��B
�_B
ևB
�oB
ϑB
�B
��B
�(B
�(B
��B
��B
οB
οB
ΥB
͹B
��B
��B
��B
��B
��B
��B
�cB
��B
�\B
�KB
�B?B]B,WB;�BB�Bc�B~]B�B��B�?B�hB�KB��B�gBּB�9B�@B�BԕB�
B�#B��B�@B�B�/B�1BðB�B��Bq�B]�BF�B8�B'�B�BMB�B
�:B
�6B
�B
��B
n�B
W?B
8�B
YB	��B	��B	�hB	��B	B	u�B	rB	i�B	Z7B	RoB	H�B	9�B	&�B	�B	2B	�B	
rB	[B�B�B�>B��B�B�B�B� B��B�|B�B�B��B�6B��B	 �B	�B	
�B	7B	�B��B�dB	�B	B		B	�B	�B	FB�jB��B�B�%B	�B	AoB	R�B	\�B	^�B	e,B	oB	�B	�MB	��B	��B	�B	��B	�eB	��B	��B	��B	��B	�B	��B	�B	�HB	�jB	��B	�VB	�VB	�VB	��B	��B	��B	�$B	��B	��B	�RB	�	B	�B	��B	�ZB	��B	��B	��B	��B	�B	�AB	��B	��B	��B	�B	��B	��B	�UB	�hB	�9B	��B	�)B	�DB	��B	��B	�>B	�$B	��B	��B	��B	�XB	��B	��B	��B	��B	�vB	�8B	�zB	�3B	�TB	� B	�JB	��B	�LB	��B	��B	�}B	ªB	��B	żB	�tB	�zB	ǮB	�1B	��B	ȀB	��B	�7B	ȀB	��B	ɺB	��B	�rB	�#B	�rB	�	B	ɆB	�RB	�lB	�lB	�RB	�lB	ɠB	�B	��B	��B	�B	�lB	�	B	�	B	�=B	��B	�RB	�	B	��B	�dB	�jB	ΊB	ΊB	ΥB	ΊB	ϑB	�B	��B	�}B	�.B	ϫB	ϫB	�\B	��B	�vB	͹B	�6B	��B	��B	�MB	ԕB	՛B	�$B	ּB	�sB	�$B	�B	յB	�gB	��B	өB	�oB	�@B	��B	ּB	�
B	��B	ؓB	��B	�1B	�kB	�B	�B	�_B	�+B	ؓB	�B	�qB	یB	ٚB	��B	�sB	�
B	֡B	ؓB	ۦB	�bB	��B	޸B	޸B	��B	�|B	��B	�B	�B	�8B	�B	��B	�B	��B	�
B	��B	�B	��B	�KB	��B	�yB	�B	�_B	�B	�KB	��B	�B	��B	�IB	��B	�B	�/B	��B	�UB	�iB	�B	�B	� B	��B	�B	�B	�AB	�B	�`B	��B	�+B	�aB	�B	� B	��B	��B	�B	�CB	�B	��B	�B	��B	��B	��B	�+B	�B	�B	�tB	�B	�B	�aB	�B	��B	�GB	��B	��B	�B	�AB	�-B	�B	�aB	�B	��B	�B	�B	�ZB	��B	��B	�fB	��B	��B	��B	�PB	�<B	�"B	��B	��B	��B	��B
B
�B
�B
{B
gB
?B
?B
�B
�B
SB
SB
B
9B
SB
B
SB
�B
%B
%B
SB
�B
gB
�B
{B
�B
B
�B
�B
gB
3B
MB
9B
B
gB
3B
�B
�B
B
�B
�B
SB
9B
B
B
mB
�B
�B
�B
�B
mB
�B
?B
�B
+B
�B
_B
�B
�B
	�B

#B

	B
)B
�B
�B
<B
pB
}B
�B
B
.B
 B
�B
�B
KB
=B
xB
�B
�B
�B
dB
�B
�B
B
�B
�B
5B
�B
5B
;B
 BB
 B
!HB
 �B
!|B
!�B
"NB
"�B
"�B
"�B
#:B
# B
#TB
#:B
#�B
#�B
%�B
%�B
%`B
$�B
#�B
$�B
$�B
%�B
&2B
&2B
&LB
&2B
&�B
&�B
&�B
&LB
&�B
&�B
&�B
&�B
'�B
'�B
($B
'�B
(�B
(sB
(sB
)�B
)_B
*eB
)yB
*0B
*�B
*B
+B
*�B
*B
*B
*KB
+�B
+�B
+QB
,WB
,�B
,WB
,=B
,qB
,qB
,WB
,�B
-CB
,�B
-�B
.cB
/�B
/�B
0�B
1B
1vB
2�B
1�B
2�B
2�B
3�B
3MB
3�B
3�B
4TB
5�B
5�B
4�B
5�B
5ZB
5?B
6FB
6+B
6FB
6�B
7LB
6�B
7�B
7B
7LB
6�B
7fB
8B
8B
8lB
8�B
9�B
9XB
8�B
8RB
9XB
:B
9>B
:�B
;dB
<B
;�B
<�B
=B
=VB
>B
=�B
=�B
>B
>�B
>]B
>�B
>�B
>�B
>�B
?HB
?�B
?}B
?HB
@4B
@iB
?�B
@�B
@iB
@iB
@�B
@�B
AB
A;B
AUB
AoB
AoB
A B
BB
BB
A�B
B'B
B�B
B�B
C-B
C�B
C�B
D�B
E�B
F?B
F�B
F�B
F?B
GB
GEB
F�B
GB
F�B
G�B
G�B
G�B
GEB
G�B
H�B
IB
I�B
I�B
J=B
I�B
J�B
J#B
J=B
J�B
J�B
KDB
L0B
K�B
K�B
L�B
L�B
MjB
M6B
MB
M�B
NpB
OB
N�B
N�B
OvB
O(B
OvB
N�B
N�B
O(B
O�B
PbB
P}B
P�B
P.B
P}B
P�B
Q�B
QNB
Q4B
RoB
RB
RB
RTB
R�B
R�B
R�B
R�B
S@B
S@B
S[B
S�B
S�B
T{B
TaB
T�B
UB
UB
UgB
V9B
U�B
VB
V�B
W?B
W?B
W
B
WsB
V�B
WsB
WYB
WYB
W$B
W?B
W$B
XB
X�B
X_B
X_B
X_B
X�B
X�B
YKB
YB
XyB
YeB
XyB
YB
YB
Y�B
Y�B
ZB
Y�B
Y�B
Y�B
ZkB
ZQB
ZB
Y�B
ZB
[#B
[qB
[qB
[WB
\B
\)B
[�B
\B
\�B
\�B
\�B
]/B
]~B
^B
^5B
^�B
_pB
_VB
^�B
^�B
_�B
`'B
`vB
aHB
abB
`�B
aB
a�B
a�B
a�B
b4B
a�B
bhB
b�B
b�B
c B
c:B
cTB
c�B
c:B
c:B
c�B
dB
d@B
c�B
cnB
e�B
e`B
fB
f2B
f�B
ffB
f�B
gB
gB
g�B
g8B
g�B
h
B
hXB
h�B
h$B
hsB
i*B
i�B
h�B
h�B
iB
i�B
jKB
j0B
jB
j�B
jeB
jB
j�B
kkB
l�B
mCB
mCB
mCB
l�B
m�B
l=B
lWB
lWB
m�B
m�B
m)B
m�B
m]B
m]B
m)B
m)B
nIB
nIB
m�B
n�B
o5B
n�B
n�B
oB
oOB
oiB
o B
o�B
poB
p�B
q'B
rGB
rGB
r�B
r�B
s3B
t9B
uB
t�B
tB
tnB
t�B
u�B
u�B
u?B
u�B
utB
vzB
wB
wB
v`B
w2B
v�B
w�B
wfB
w�B
xB
w�B
w�B
x8B
x�B
x�B
y	B
y$B
y>B
yrB
z*B
y�B
zB
z^B
z�B
{B
{B
{0B
{B
{0B
|B
{�B
|PB
|B
|�B
|�B
|�B
}B
}B
}VB
}�B
}qB
}�B
}�B
}qB
}�B
}�B
}�B
~wB
}B
~�B
~�B
~�B
~wB
~�B
�B
.B
~�B
cB
~�B
�B
.B
�B
.B
�B
�4B
� B
�4B
�B
��B
�B
�B
�;B
�oB
��B
��B
�[B
��B
��B
�GB
�GB
�{B
��B
��B
��B
��B
��B
��B
��B
�gB
�MB
�B
��B
�9B
�9B
�9B
�mB
�SB
�9B
�mB
��B
��B
��B
��B
�YB
�YB
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104921  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173908  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173908  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173908                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023915  QCP$                G�O�G�O�G�O�         208F35EJA  ARGQrqcpc3.6                                                                20220605023915  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                