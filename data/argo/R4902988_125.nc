CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-03-14T09:44:36Z creation;2023-03-14T09:44:37Z conversion to V3.1      
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
_FillValue                 �  I0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ph   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tT   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Έ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �h   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �h   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �h   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230314094436  20230314095739  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               }A   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @��L�X^1   @���\(�@;�Q���c�z�G�1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  A   A   A@  A`  A�  A�  A�  A�  A�33A�33A�33A�33B   B  B  B  B   B(  B0  B8  B@  BH  BO��BX  B`  Bh  Bp  Bx  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C�fC  C
  C  C  C�fC  C  C  C  C  C  C  C   C"  C#�fC%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CQ�fCT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Co�fCr  Cs�fCv  Cx  Cz�C|  C~  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C��3C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D��D� D	  D	� D
  D
� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D   D � D!  D!y�D"  D"� D#  D#� D$  D$� D$��D%� D&  D&� D'fD'� D(  D(� D)  D)� D*  D*�fD+fD+� D,  D,� D-  D-y�D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5fD5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<fD<� D=  D=� D>fD>�fD?  D?� D?��D@� DAfDA� DA��DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DT��DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\y�D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De�fDf  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl�fDm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dx��Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~fD~�fD  D� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D���D���D�@ D�� D�� D�  D�C3D��3D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D���D���D�@ D�� D���D�  D�@ D��3D��3D�3D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�|�D���D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�C3Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ Dռ�D�  D�@ Dր D�� D�  D�@ D׀ D�� D�3D�@ D؀ D�� D�3D�C3Dـ Dټ�D�  D�@ Dڀ D�� D�  D�@ D�|�D�� D�  D�@ D܀ D�� D�  D�@ D݃3D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�|�D�� D�  D�C3D� D�� D�  D�@ D� D��D���D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�9�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�33@�ff@�ffA33A?33A_33A33A���A���A���A���A���A���A���A���B��B��B��B��B'��B/��B7��B?��BG��BOfgBW��B_��Bg��Bo��Bw��B��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��3B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��B��fC�3C�3CٙC�3C	�3C�3C�3CٙC�3C�3C�3C�3C�3C�3C�3C�3C!�3C#ٙC%ٙC'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQٙCS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Ch�Ci�3Ck�3Cm�3CoٙCq�3CsٙCu�3Cw�3Cz�C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C�gC���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC�gC���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D3D�3D��D|�D��D|�D�gD|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D3D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��DvgD��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!vgD!��D"|�D"��D#|�D#��D$|�D$�gD%|�D%��D&|�D'3D'|�D'��D(|�D(��D)|�D)��D*�3D+3D+|�D+��D,|�D,��D-vgD-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D53D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D<3D<|�D<��D=|�D>3D>�3D>��D?|�D?�gD@|�DA3DA|�DA�gDB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT�gDU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\vgD\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De�3De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl�3Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx�gDy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D~3D~�3D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�;3D�~fD��3D��3D�>fD�~fD��fD��fD�A�D���D��fD��3D�>fD�~fD��fD��fD�>fD�~fD��fD��D�>fD�~fD��fD��fD�A�D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�A�D�~fD��fD��fD�>fD�~fD��fD��D�>fD�~fD���D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��3D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��3D�>fD�~fD��fD��fD�>fD�~fD��3D��3D�>fD�~fD��3D��3D�>fD�~fD��3D��fD�>fD���D���D��D�A�D���D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�A�D�~fD��fD��fD�>fD�~fD��fD��fD�A�D�~fD��fD��fD�>fD�~fD���D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��fD�>fD�~fD��fD��fD�>fD�~fD���D��D�>fD�{3D��3D��fD�>fD�~fD��fD��fD�;3D�{3D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�>fD�~fD��fD��fD�>fD�~fD���D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD���D���D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD���D��fD��fD�>fD���D��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�A�D�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDջ3D��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��D�>fD�~fDؾfD��D�A�D�~fDٻ3D��fD�>fD�~fDھfD��fD�>fD�{3D۾fD��fD�>fD�~fDܾfD��fD�>fD݁�DݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�{3D�fD��fD�A�D�~fD�fD��fD�>fD�~fD�3D��3D�;3D�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��3D�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�A�D�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�3D��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD���D���D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�8 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�l�A�p�A�q�A�r�A�k�A�m)A�o�A�tA�}�A��1A��A���A��bA���A��A��-A���A���A��$A��RA���A���A���A��6A��6A���A��dA���A��3A��CA���A���A��\A�x�A�g8A�W�A�L�A�?}A���A��CA��#A�HKA���A�&�A�v�A�ȀA�zxA�DA���A�{A��(A�s�A�\�A��A�dZA��2A�0!A��
A��A��0A��A��8A� �A��A�H�A�+A��A��ZA��A�d�A�ΥA�E�A�FA���A�d�A��'A���A��zA�K^A���A�+6A��A��&A�xA�bA��UA�+�A���A��TA�:A�Q�A�MjA��(A��2A��'A��A���A�#:A��A�kA}VAxAo�A5?A{��AztTAy�Ay��Ax��Aw$�AvrGAtY�Ar�Aq��Aq&�Ap�Ap��Ao�MAna|Alw2AkϫAkVAj�zAj�oAhqAgAf�Ad_pAc�Ab}�AbG�Ab?}A`��A]��A[XAZeAY`BAX��AXAVGEAT~�AS�AQ�tAO�QAN=�AM&�AK��AK�AJ��AH�AG'RAG�AF�hAEW�ADMACAA�AA�~A@��A@ \A?��A?rGA>J�A=��A<��A;��A:�NA:g8A9�]A8�A7�6A6	�A3�[A34A2��A2�4A1�tA0OA0�A/p�A.�hA-�A,��A+��A*�7A)F�A'��A'H�A&X�A$�A$-wA#�cA#Q�A#�A"��A"C�A!��A ��A >BA��A9XA�A��A��A��A�VAOvA�aAXA�A��AxAA�7AS&A�A/�As�AH�A�A�WA��AخA��A\�AtTA��Ao A	�A�~A�A5�A��A>BA-�AخA��AdZA(�A��A��AA�A ��A sA Y�A 7@��'@�S@��@�PH@���@��@�iD@�@��@��@��
@��6@��a@���@�<6@��x@��	@�'R@�@�dZ@��@��)@�l�@���@�+@�}@���@�s�@���@��@� @�a@�@�>B@�4@��@�6z@�Z�@��}@�?�@�֡@�[�@�҉@ԃ@���@ґ�@�Q@���@�,�@��@̤�@�	�@ȧ@��]@�~�@�[W@�L�@��@���@ƆY@���@�g8@�Mj@�w�@��@�H�@��?@��@�1@��/@�#:@���@�8@�PH@��:@�7L@��y@��}@���@�l�@���@��)@�e@��X@�e�@�8@�/�@�*0@�#�@���@���@��`@��@��}@��@���@��'@��@�\)@���@���@�]d@�C-@��w@���@�(�@��z@�l�@�g�@���@�c @�5?@��W@�]�@���@���@���@�u�@�oi@�M�@�A�@�C�@�(�@��@�:�@�(�@�o @�`�@�E9@��f@���@�|�@�L0@��@��@��3@���@��P@�A�@�2a@��@��@��@��p@���@�0U@�"h@�
�@��T@��F@�^�@��@��@���@���@��h@�Dg@���@��@���@�D�@���@��$@�dZ@�d�@���@��@��I@���@���@�n/@�4�@� i@��@��$@���@�I�@�G@���@�e�@�J�@�2a@��]@���@�  @��f@�+@��8@��@��f@��"@��8@��8@���@���@���@���@�M�@���@�rG@�C@���@�#�@�U2@�{J@��{@��@�u�@�f�@�F�@��@��@���@��@��u@���@�m�@�D�@��@�1@���@��
@�t�@��b@�c�@�?@�~@��+@���@��n@�a�@��@�@�@�}@~��@}�Z@}^�@|�f@|�@|<�@|G@{�K@{=@z�B@z_@x�@x�@x��@x��@x	�@w�
@w�a@w\)@w�@v��@v� @v@�@u�9@u?}@t�9@t/�@s�@s�}@s��@sX�@r�"@r��@r�m@r��@r	@q�@qDg@q!�@pw�@o��@o��@o�W@o��@o�g@o�@o�*@o�$@o�f@os@oMj@o&@o@n�@n�H@n�@n($@m�z@mL�@l�K@k�*@j��@j�@i�@i�S@i}�@i|@if�@i-w@h��@hl"@h4n@g��@fn�@e�@e�-@e�7@eo @eN<@d��@d`�@d2�@c��@b$�@aԕ@aN<@`�Y@`l"@`N�@`Ft@`2�@_�w@_x@_RT@_�@^�@^��@^c @^1�@]��@]�@\��@\�E@\��@\��@\��@\�$@\7@[��@[@Z�F@Z_�@Zu@Y�z@Y�@Ym]@Y�@Xoi@W�0@W�0@W��@V��@Vs�@Vl�@U�~@T �@R�!@R\�@Q�z@Qc@QVm@QA @Q*0@P�@P��@P:�@P@PG@O�P@N�@NB[@M�9@M�^@M��@M�X@Me,@M	l@L��@L�@L�@L��@LV�@K�a@K�@Kt�@K]�@KF�@KC@Jv�@I�D@I��@I��@I��@Ij@IN<@I<6@I�@H�5@H�O@H��@Hy>@HN�@G��@G�K@G�$@F�@F^5@FJ@E��@EN<@D�5@DĜ@DFt@C�@C��@C�	@C��@CH�@B�@B��@B:*@B@A�@A��@A��@A0�@@�@@�@@�_@@bN@@G@?��@?��@?qv@?o�@?\)@?.I@>�8@>��@>#:@=�t@=��@=��@=Vm@=q@=�@=@=;@<�P@<�z@<V�@;�&@;�	@;P�@;1�@:��@:d�@:!�@9�j@9�~@9��@9x�@9o @9IR@8�[@8Ft@7�;@7��@7)_@6�c@6͟@6Z�@5|@4��@4��@4l"@4M@3�F@3U�@34�@3�@3�@3�@2v�@2+k@2�@1��@1��@1�@1�X@1�S@1�~@1`B@1A @1@0�P@0��@0y>@/��@/�:@.xl@-rG@-O�@-A @,��@,��@,>B@+�
@+n/@+$t@*��@*ߤ@*��@*�!@*�@*� @*xl@*YK@*;�@*O@*�@)}�@)j@)k�@)L�@)A @):�@):�@)/@)-w@)q@(�@(�?@(��@(�@'��@']�@'@O@'�@&��@&��@&��@&V@&;�@&#:@%�@%�7@%�@$�?@$j@#�@#�@"�@"�!@"�6@"�@"YK@!�@!�9@!�t@!rG@!S&@!G�@!4@!%F@!#�@!�@!�@!+@!�@ ��@ �@ �@ ��@ ��@ ��@ c�@ K^@ 4n@�@�0@��@W?@�"@3�@�@��@��@��@�~@zx@?}@��@[�@~@��@��@{J@$t@��@4@��@��@�@�h@}�@p�@Y�@N<@:�@-w@��@�D@V�@��@��@��@��@�@@�@l�@4�@!-@�@��@�A@R�@�@�@��@	l@�)@�@��@oi@�@� @��@y�@.I@��@v�@_�@3�@@�T@�j@�N@�@��@u�@A @�@@�@��@��@_@��@�0@��@��@�f@{J@s@C�@(@�@�c@��@�s@�,@҉@͟@��@z@Z�@0U@@�j@�t@��@�@��@}�@m]@`B@X@Q�@O�@Dg@-w@�@�@�@�)@��@��@bN@M@��@a@�@
u%@

�@	�^@	s�@	hs@	j@	k�@	f�@	a�@	`B@	X@	L�@	B�@	Dg@	F@	:�@	+@		l@�/@��@�.@c�@M@C-@9X@�@�@�&@��@خ@ݘ@��@�;@�;@��@ƨ@�0@��@��@�@�@��@��@��@��@��@�@@�@��@��@{J11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�l�A�p�A�q�A�r�A�k�A�m)A�o�A�tA�}�A��1A��A���A��bA���A��A��-A���A���A��$A��RA���A���A���A��6A��6A���A��dA���A��3A��CA���A���A��\A�x�A�g8A�W�A�L�A�?}A���A��CA��#A�HKA���A�&�A�v�A�ȀA�zxA�DA���A�{A��(A�s�A�\�A��A�dZA��2A�0!A��
A��A��0A��A��8A� �A��A�H�A�+A��A��ZA��A�d�A�ΥA�E�A�FA���A�d�A��'A���A��zA�K^A���A�+6A��A��&A�xA�bA��UA�+�A���A��TA�:A�Q�A�MjA��(A��2A��'A��A���A�#:A��A�kA}VAxAo�A5?A{��AztTAy�Ay��Ax��Aw$�AvrGAtY�Ar�Aq��Aq&�Ap�Ap��Ao�MAna|Alw2AkϫAkVAj�zAj�oAhqAgAf�Ad_pAc�Ab}�AbG�Ab?}A`��A]��A[XAZeAY`BAX��AXAVGEAT~�AS�AQ�tAO�QAN=�AM&�AK��AK�AJ��AH�AG'RAG�AF�hAEW�ADMACAA�AA�~A@��A@ \A?��A?rGA>J�A=��A<��A;��A:�NA:g8A9�]A8�A7�6A6	�A3�[A34A2��A2�4A1�tA0OA0�A/p�A.�hA-�A,��A+��A*�7A)F�A'��A'H�A&X�A$�A$-wA#�cA#Q�A#�A"��A"C�A!��A ��A >BA��A9XA�A��A��A��A�VAOvA�aAXA�A��AxAA�7AS&A�A/�As�AH�A�A�WA��AخA��A\�AtTA��Ao A	�A�~A�A5�A��A>BA-�AخA��AdZA(�A��A��AA�A ��A sA Y�A 7@��'@�S@��@�PH@���@��@�iD@�@��@��@��
@��6@��a@���@�<6@��x@��	@�'R@�@�dZ@��@��)@�l�@���@�+@�}@���@�s�@���@��@� @�a@�@�>B@�4@��@�6z@�Z�@��}@�?�@�֡@�[�@�҉@ԃ@���@ґ�@�Q@���@�,�@��@̤�@�	�@ȧ@��]@�~�@�[W@�L�@��@���@ƆY@���@�g8@�Mj@�w�@��@�H�@��?@��@�1@��/@�#:@���@�8@�PH@��:@�7L@��y@��}@���@�l�@���@��)@�e@��X@�e�@�8@�/�@�*0@�#�@���@���@��`@��@��}@��@���@��'@��@�\)@���@���@�]d@�C-@��w@���@�(�@��z@�l�@�g�@���@�c @�5?@��W@�]�@���@���@���@�u�@�oi@�M�@�A�@�C�@�(�@��@�:�@�(�@�o @�`�@�E9@��f@���@�|�@�L0@��@��@��3@���@��P@�A�@�2a@��@��@��@��p@���@�0U@�"h@�
�@��T@��F@�^�@��@��@���@���@��h@�Dg@���@��@���@�D�@���@��$@�dZ@�d�@���@��@��I@���@���@�n/@�4�@� i@��@��$@���@�I�@�G@���@�e�@�J�@�2a@��]@���@�  @��f@�+@��8@��@��f@��"@��8@��8@���@���@���@���@�M�@���@�rG@�C@���@�#�@�U2@�{J@��{@��@�u�@�f�@�F�@��@��@���@��@��u@���@�m�@�D�@��@�1@���@��
@�t�@��b@�c�@�?@�~@��+@���@��n@�a�@��@�@�@�}@~��@}�Z@}^�@|�f@|�@|<�@|G@{�K@{=@z�B@z_@x�@x�@x��@x��@x	�@w�
@w�a@w\)@w�@v��@v� @v@�@u�9@u?}@t�9@t/�@s�@s�}@s��@sX�@r�"@r��@r�m@r��@r	@q�@qDg@q!�@pw�@o��@o��@o�W@o��@o�g@o�@o�*@o�$@o�f@os@oMj@o&@o@n�@n�H@n�@n($@m�z@mL�@l�K@k�*@j��@j�@i�@i�S@i}�@i|@if�@i-w@h��@hl"@h4n@g��@fn�@e�@e�-@e�7@eo @eN<@d��@d`�@d2�@c��@b$�@aԕ@aN<@`�Y@`l"@`N�@`Ft@`2�@_�w@_x@_RT@_�@^�@^��@^c @^1�@]��@]�@\��@\�E@\��@\��@\��@\�$@\7@[��@[@Z�F@Z_�@Zu@Y�z@Y�@Ym]@Y�@Xoi@W�0@W�0@W��@V��@Vs�@Vl�@U�~@T �@R�!@R\�@Q�z@Qc@QVm@QA @Q*0@P�@P��@P:�@P@PG@O�P@N�@NB[@M�9@M�^@M��@M�X@Me,@M	l@L��@L�@L�@L��@LV�@K�a@K�@Kt�@K]�@KF�@KC@Jv�@I�D@I��@I��@I��@Ij@IN<@I<6@I�@H�5@H�O@H��@Hy>@HN�@G��@G�K@G�$@F�@F^5@FJ@E��@EN<@D�5@DĜ@DFt@C�@C��@C�	@C��@CH�@B�@B��@B:*@B@A�@A��@A��@A0�@@�@@�@@�_@@bN@@G@?��@?��@?qv@?o�@?\)@?.I@>�8@>��@>#:@=�t@=��@=��@=Vm@=q@=�@=@=;@<�P@<�z@<V�@;�&@;�	@;P�@;1�@:��@:d�@:!�@9�j@9�~@9��@9x�@9o @9IR@8�[@8Ft@7�;@7��@7)_@6�c@6͟@6Z�@5|@4��@4��@4l"@4M@3�F@3U�@34�@3�@3�@3�@2v�@2+k@2�@1��@1��@1�@1�X@1�S@1�~@1`B@1A @1@0�P@0��@0y>@/��@/�:@.xl@-rG@-O�@-A @,��@,��@,>B@+�
@+n/@+$t@*��@*ߤ@*��@*�!@*�@*� @*xl@*YK@*;�@*O@*�@)}�@)j@)k�@)L�@)A @):�@):�@)/@)-w@)q@(�@(�?@(��@(�@'��@']�@'@O@'�@&��@&��@&��@&V@&;�@&#:@%�@%�7@%�@$�?@$j@#�@#�@"�@"�!@"�6@"�@"YK@!�@!�9@!�t@!rG@!S&@!G�@!4@!%F@!#�@!�@!�@!+@!�@ ��@ �@ �@ ��@ ��@ ��@ c�@ K^@ 4n@�@�0@��@W?@�"@3�@�@��@��@��@�~@zx@?}@��@[�@~@��@��@{J@$t@��@4@��@��@�@�h@}�@p�@Y�@N<@:�@-w@��@�D@V�@��@��@��@��@�@@�@l�@4�@!-@�@��@�A@R�@�@�@��@	l@�)@�@��@oi@�@� @��@y�@.I@��@v�@_�@3�@@�T@�j@�N@�@��@u�@A @�@@�@��@��@_@��@�0@��@��@�f@{J@s@C�@(@�@�c@��@�s@�,@҉@͟@��@z@Z�@0U@@�j@�t@��@�@��@}�@m]@`B@X@Q�@O�@Dg@-w@�@�@�@�)@��@��@bN@M@��@a@�@
u%@

�@	�^@	s�@	hs@	j@	k�@	f�@	a�@	`B@	X@	L�@	B�@	Dg@	F@	:�@	+@		l@�/@��@�.@c�@M@C-@9X@�@�@�&@��@خ@ݘ@��@�;@�;@��@ƨ@�0@��@��@�@�@��@��@��@��@��@�@@�@��@��@{J11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�jB��B�;B�VB�OB޸B޸BߊB��B�B�:B��B�,B�B�8B�B�>B�>B�B�>B�XB�>B�$B�>B�>B�$B�B�B�RB��B�fB�B�LB�tB�hB�bB�'B�jB��B��B��B��B��B�OBpBc�B[WBS�BCGB/ B&B#:B �B �B�B�B	BBUB�cB�dB��B�vB�tB�$B�NB�'B�IB��B�Bp!Be�B_�BZ�BQ�BEB0�BB�B�_BޞB�yB�XB��B��B��B�<B��B|6BnBa�BRoBH�B4�B,WB*BWB�B6BBxB^B
#B�B
�dB
�B
��B
�KB
�B
��B
��B
ѝB
��B
�cB
��B
�$B
��B
��B
��B
��B
�B
��B
��B
�FB
��B
�B
B
x�B
pB
lWB
jB
i*B
d�B
U�B
G�B
?�B
<jB
9�B
4�B
./B
%�B
/B
gB

�B
 �B	�XB	�aB	�kB	��B	�|B	�+B	��B	յB	�B	�B	�RB	�B	ªB	�cB	�B	�xB	�>B	��B	�GB	��B	��B	��B	��B	�:B	�VB	��B	�B	�xB	��B	��B	�9B	�{B	}�B	{dB	zB	vzB	r�B	o�B	iB	c�B	^OB	X�B	U�B	R�B	M�B	K^B	I7B	G+B	E9B	DB	BuB	@OB	<B	:�B	9	B	7�B	7LB	6zB	6B	5�B	5B	3hB	0B	,B	�B	�B	gB	�B	oB	\B	dB	�B	�B	fB	�B	�B	tB	�B	�B	 iB	�B�$B��B��B��B��B�QB��B��B�
B��B�RB�LB�B�B�B�tB�;B�jB޸B�B�BܒB��B�#B�B�IB��B�xB�CBܒB�WB�qBچB�B��B�+B�B�YB�9B��B��B�B��B�mB�BՁBյB�SB��B��BՁB�9B��BԯBյB��B՛B��BԯBյB��BٴBٴB��BخBٴB�CBߤB�!B�OB�OB�B�vB�BB�B��B߾B߾B߾B�pB߾B�\B��B�B��B�B��B�8B�]B�OB�B�[B�B�B�hB�9B�B�B��B�ZB��B�"B��B��B	 OB	 �B	 �B	 �B	 4B��B	 �B	 �B	gB		7B	�B	�B	�B	�B	mB	jB	($B	)�B	+QB	0!B	3�B	7LB	7�B	88B	A B	K�B	OB	O�B	P�B	SB	T�B	VSB	V�B	W�B	W�B	XyB	X_B	X+B	X�B	YB	]�B	c�B	g�B	nIB	u�B	w�B	zxB	{�B	}qB	� B	�oB	�B	�-B	�mB	��B	��B	�0B	��B	��B	��B	��B	��B	�?B	��B	��B	��B	��B	�pB	��B	�tB	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�tB	��B	�_B	�7B	�lB	ɺB	�=B	��B	�B	ˬB	�0B	̘B	�B	��B	�VB	�<B	�VB	��B	ЗB	ңB	�[B	�B	��B	ؓB	�=B	ݘB	��B	�pB	�B	�B	��B	�B	��B	�B	��B	�B	��B	��B	��B	�B	�BB	��B
UB
[B
�B
+B
�B
	�B

�B
�B
�B
�B
<B
B
B
bB
 B
�B
�B
�B
�B
�B
�B
�B
CB
IB
 'B
 �B
!�B
%FB
&�B
(XB
)DB
)�B
,�B
/iB
1AB
4�B
7�B
<�B
A�B
BuB
B�B
B�B
C�B
DgB
EB
F�B
G�B
IB
I�B
J�B
L0B
NB
OvB
QNB
RB
R�B
S@B
S�B
T�B
UgB
UgB
VSB
X�B
Z�B
[�B
\xB
_;B
`�B
a-B
aHB
a�B
a�B
a�B
b�B
b�B
cTB
c�B
dB
d�B
e�B
e�B
e�B
gmB
jB
mB
qAB
s�B
w2B
y	B
zDB
z�B
z�B
z�B
z�B
{JB
{�B
|�B
~(B
~�B
�B
��B
�?B
��B
��B
��B
��B
�fB
�RB
��B
�xB
��B
�.B
��B
��B
��B
��B
��B
��B
�uB
��B
�FB
��B
��B
�SB
��B
�$B
��B
�B
��B
��B
�#B
�=B
�=B
�	B
��B
��B
��B
��B
�'B
�bB
��B
�hB
��B
��B
��B
��B
��B
�XB
��B
��B
�kB
��B
��B
��B
�tB
��B
�fB
��B
�B
�B
��B
�rB
�^B
��B
��B
�B
�qB
��B
��B
��B
��B
��B
��B
��B
��B
�AB
�[B
�uB
��B
�MB
āB
��B
�B
�SB
ňB
�B
�1B
ȚB
��B
�B
ɆB
ɺB
��B
�#B
ʦB
�)B
�xB
ˬB
��B
̳B
�B
�jB
�B
�.B
��B
ѝB
�TB
�&B
�[B
ԕB
��B
�B
�B
�9B
֡B
�$B
רB
خB
��B
ٚB
��B
�B
��B
�qB
ۦB
��B
�]B
�/B
��B
�B
�OB
�5B
�jB
޸B
�!B
��B
�B
�bB
�B
�B
�B
�NB
�NB
�hB
�B
�B
� B
�B
��B
�zB
��B
��B
�B
�RB
�B
��B
�
B
�
B
�$B
�
B
�$B
��B
�B
�0B
��B
�B
�"B
�WB
�B
�B
�UB
�B
�AB
��B
�aB
��B
�3B
�MB
�3B
�MB
��B
�nB
�nB
�nB
�B
��B
�%B
�ZB
�ZB
��B
��B
�zB
��B
��B
��B
��B
��B
�rB
�xB
��B
��B
�B
��B
�jB
��B
��B
��B
�"B
�VB
��B
��B
��B
��B
�BB
��B
�B
�.B
�HB OB 4B 4B iB �B �B �B �B �B �BB;B�BoB�BaB{B�BgBgB�BmB�B�B�BYB�B_BB	�B
rB
�B
�B
�B
�B^BBBJB�B�BBBBB6B6BB6BjBjB�B�B�BB"BVBpB�B(B\B�BBB�B�B�B�B�B�B B�B&B@B�B�B,B�B2BmB�B
B
BYBsBsB�B�B�B�B_B�B�B�BBB7BBB�B�B�B	B�B�B�BCB�B/B�BBOBOB�B�BVB�B�B 'B �B!B!B!bB!�B!�B!�B!�B!�B"4B"hB"�B"�B"�B#B#B#:B#:B#�B#�B$@B$�B$�B$�B$�B%FB%�B%�B%�B%�B%�B%�B%�B%�B%�B&B&B&fB&�B&�B'B'RB'mB'mB'�B'�B'�B'�B'�B'�B'�B'�B($B(>B(XB(�B(�B(�B)B)yB*B*eB*�B+�B,"B,�B-B-B-B-B-)B-)B-)B-)B-CB-]B-]B-CB-]B-�B-wB-�B-�B./B.cB.}B.}B.�B.�B/ B/ B/OB/OB/5B/5B/5B/5B/OB/�B/�B/�B/�B/�B/�B/�B/�B/�B/�B/�B/�B/�B/�B0B044444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444B�jB��B�;B�VB�OB޸B޸BߊB��B�B�:B��B�,B�B�8B�B�>B�>B�B�>B�XB�>B�$B�>B�>B�$B�B�B�RB��B�fB�B�LB�tB�hB�bB�'B�jB��B��B��B��B��B�OBpBc�B[WBS�BCGB/ B&B#:B �B �B�B�B	BBUB�cB�dB��B�vB�tB�$B�NB�'B�IB��B�Bp!Be�B_�BZ�BQ�BEB0�BB�B�_BޞB�yB�XB��B��B��B�<B��B|6BnBa�BRoBH�B4�B,WB*BWB�B6BBxB^B
#B�B
�dB
�B
��B
�KB
�B
��B
��B
ѝB
��B
�cB
��B
�$B
��B
��B
��B
��B
�B
��B
��B
�FB
��B
�B
B
x�B
pB
lWB
jB
i*B
d�B
U�B
G�B
?�B
<jB
9�B
4�B
./B
%�B
/B
gB

�B
 �B	�XB	�aB	�kB	��B	�|B	�+B	��B	յB	�B	�B	�RB	�B	ªB	�cB	�B	�xB	�>B	��B	�GB	��B	��B	��B	��B	�:B	�VB	��B	�B	�xB	��B	��B	�9B	�{B	}�B	{dB	zB	vzB	r�B	o�B	iB	c�B	^OB	X�B	U�B	R�B	M�B	K^B	I7B	G+B	E9B	DB	BuB	@OB	<B	:�B	9	B	7�B	7LB	6zB	6B	5�B	5B	3hB	0B	,B	�B	�B	gB	�B	oB	\B	dB	�B	�B	fB	�B	�B	tB	�B	�B	 iB	�B�$B��B��B��B��B�QB��B��B�
B��B�RB�LB�B�B�B�tB�;B�jB޸B�B�BܒB��B�#B�B�IB��B�xB�CBܒB�WB�qBچB�B��B�+B�B�YB�9B��B��B�B��B�mB�BՁBյB�SB��B��BՁB�9B��BԯBյB��B՛B��BԯBյB��BٴBٴB��BخBٴB�CBߤB�!B�OB�OB�B�vB�BB�B��B߾B߾B߾B�pB߾B�\B��B�B��B�B��B�8B�]B�OB�B�[B�B�B�hB�9B�B�B��B�ZB��B�"B��B��B	 OB	 �B	 �B	 �B	 4B��B	 �B	 �B	gB		7B	�B	�B	�B	�B	mB	jB	($B	)�B	+QB	0!B	3�B	7LB	7�B	88B	A B	K�B	OB	O�B	P�B	SB	T�B	VSB	V�B	W�B	W�B	XyB	X_B	X+B	X�B	YB	]�B	c�B	g�B	nIB	u�B	w�B	zxB	{�B	}qB	� B	�oB	�B	�-B	�mB	��B	��B	�0B	��B	��B	��B	��B	��B	�?B	��B	��B	��B	��B	�pB	��B	�tB	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�tB	��B	�_B	�7B	�lB	ɺB	�=B	��B	�B	ˬB	�0B	̘B	�B	��B	�VB	�<B	�VB	��B	ЗB	ңB	�[B	�B	��B	ؓB	�=B	ݘB	��B	�pB	�B	�B	��B	�B	��B	�B	��B	�B	��B	��B	��B	�B	�BB	��B
UB
[B
�B
+B
�B
	�B

�B
�B
�B
�B
<B
B
B
bB
 B
�B
�B
�B
�B
�B
�B
�B
CB
IB
 'B
 �B
!�B
%FB
&�B
(XB
)DB
)�B
,�B
/iB
1AB
4�B
7�B
<�B
A�B
BuB
B�B
B�B
C�B
DgB
EB
F�B
G�B
IB
I�B
J�B
L0B
NB
OvB
QNB
RB
R�B
S@B
S�B
T�B
UgB
UgB
VSB
X�B
Z�B
[�B
\xB
_;B
`�B
a-B
aHB
a�B
a�B
a�B
b�B
b�B
cTB
c�B
dB
d�B
e�B
e�B
e�B
gmB
jB
mB
qAB
s�B
w2B
y	B
zDB
z�B
z�B
z�B
z�B
{JB
{�B
|�B
~(B
~�B
�B
��B
�?B
��B
��B
��B
��B
�fB
�RB
��B
�xB
��B
�.B
��B
��B
��B
��B
��B
��B
�uB
��B
�FB
��B
��B
�SB
��B
�$B
��B
�B
��B
��B
�#B
�=B
�=B
�	B
��B
��B
��B
��B
�'B
�bB
��B
�hB
��B
��B
��B
��B
��B
�XB
��B
��B
�kB
��B
��B
��B
�tB
��B
�fB
��B
�B
�B
��B
�rB
�^B
��B
��B
�B
�qB
��B
��B
��B
��B
��B
��B
��B
��B
�AB
�[B
�uB
��B
�MB
āB
��B
�B
�SB
ňB
�B
�1B
ȚB
��B
�B
ɆB
ɺB
��B
�#B
ʦB
�)B
�xB
ˬB
��B
̳B
�B
�jB
�B
�.B
��B
ѝB
�TB
�&B
�[B
ԕB
��B
�B
�B
�9B
֡B
�$B
רB
خB
��B
ٚB
��B
�B
��B
�qB
ۦB
��B
�]B
�/B
��B
�B
�OB
�5B
�jB
޸B
�!B
��B
�B
�bB
�B
�B
�B
�NB
�NB
�hB
�B
�B
� B
�B
��B
�zB
��B
��B
�B
�RB
�B
��B
�
B
�
B
�$B
�
B
�$B
��B
�B
�0B
��B
�B
�"B
�WB
�B
�B
�UB
�B
�AB
��B
�aB
��B
�3B
�MB
�3B
�MB
��B
�nB
�nB
�nB
�B
��B
�%B
�ZB
�ZB
��B
��B
�zB
��B
��B
��B
��B
��B
�rB
�xB
��B
��B
�B
��B
�jB
��B
��B
��B
�"B
�VB
��B
��B
��B
��B
�BB
��B
�B
�.B
�HB OB 4B 4B iB �B �B �B �B �B �BB;B�BoB�BaB{B�BgBgB�BmB�B�B�BYB�B_BB	�B
rB
�B
�B
�B
�B^BBBJB�B�BBBBB6B6BB6BjBjB�B�B�BB"BVBpB�B(B\B�BBB�B�B�B�B�B�B B�B&B@B�B�B,B�B2BmB�B
B
BYBsBsB�B�B�B�B_B�B�B�BBB7BBB�B�B�B	B�B�B�BCB�B/B�BBOBOB�B�BVB�B�B 'B �B!B!B!bB!�B!�B!�B!�B!�B"4B"hB"�B"�B"�B#B#B#:B#:B#�B#�B$@B$�B$�B$�B$�B%FB%�B%�B%�B%�B%�B%�B%�B%�B%�B&B&B&fB&�B&�B'B'RB'mB'mB'�B'�B'�B'�B'�B'�B'�B'�B($B(>B(XB(�B(�B(�B)B)yB*B*eB*�B+�B,"B,�B-B-B-B-B-)B-)B-)B-)B-CB-]B-]B-CB-]B-�B-wB-�B-�B./B.cB.}B.}B.�B.�B/ B/ B/OB/OB/5B/5B/5B/5B/OB/�B/�B/�B/�B/�B/�B/�B/�B/�B/�B/�B/�B/�B/�B0B044444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230314094328  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230314094436  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230314094437  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230314094437                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230314094438  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230314094438  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20230314095739                      G�O�G�O�G�O�                