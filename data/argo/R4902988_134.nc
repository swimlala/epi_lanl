CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-06-11T06:43:59Z creation;2023-06-11T06:44:00Z conversion to V3.1      
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
_FillValue                 �  I,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p\   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tH   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �p   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �x   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �|   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230611064359  20230611072621  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @�24Vx�1   @�2q�r@;h�\)�c�����1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@�  A   A   A@  A`  A�  A�  A�33A�33A�  A���A�  A�  B   BffB��B��B   B(  B0  B8  B@ffBH  BP  BW��B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�fC�fC�fC
  C  C  C  C  C  C�fC  C�C  C�fC   C"  C$  C&  C(  C)�fC,  C.  C0  C2�C4  C6  C8  C:  C<  C=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C�  C��C�  C�  C��C�  C�  C��C�  C��3C�  C��C�  C�  C��C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3D y�D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D�fD	fD	�fD
  D
� D  D� D  D� D  D� D  D� D  D� D��Dy�D��D� D  D� D  Dy�D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#� D$  D$� D%fD%�fD&  D&� D'  D'� D(  D(y�D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D/��D0y�D1  D1� D2  D2� D3  D3� D4  D4� D5fD5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?fD?� D?��D@y�DA  DA� DB  DB� DC  DC� DDfDD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DIy�DJ  DJ� DK  DK� DL  DL� DM  DM� DM��DNy�DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DYy�DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^y�D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De�fDf  Dfy�Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� DkfDk�fDl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dq��Dr� Ds  Ds� Dt  Dty�Du  Du� Dv  Dv� Dv��Dwy�Dx  Dx� Dy  Dy� Dz  Dz� D{  D{�fD|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D��3D�3D�@ D�� D���D�  D�C3D��3D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�|�D���D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�<�D�|�D���D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D D¼�D�  D�@ DÀ D�� D���D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D��3D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�3D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D���D�<�D΀ D��3D�  D�<�Dπ D��3D�  D�@ DЃ3D��3D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D��3D�3D�@ D�|�D�� D�  D�@ D׀ D�� D�  D�C3D؀ D�� D�  D�@ Dـ D�� D�  D�<�Dڀ D�� D�  D�@ Dۀ D��3D�  D�@ D܀ D�� D�  D�C3D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D߼�D�  D�@ D�� D�� D���D�@ D� D�� D�  D�@ D� D��3D�3D�C3D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D���D�  D�@ D�|�D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D� D��3D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�ff@�ffA33A?33A_33A33A���A���A���A���A�fgAߙ�AA���B33B��BfgB��B'��B/��B7��B@33BG��BO��BWfgB_fgBg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB۳3B��fB��fB��fB��fB��fB��fB��fB��fB��fC�3CٙCٙCٙC	�3C�3C�3C�3C�3C�3CٙC�3C�C�3CٙC�3C!�3C#�3C%�3C'�3C)ٙC+�3C-�3C/�3C2�C3�3C5�3C7�3C9�3C;�3C=ٙC?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3CcٙCe�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C�gC���C���C���C���C�gC���C���C�gC���C���C�gC���C���C���C�gC���C���C�gC���C���C�gC�gC�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC�gC�gC���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���D vgD ��D|�D��D|�D��D|�D��D|�D��D�3D��D|�D��D|�D��D�3D	3D	�3D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D�gDvgD�gD|�D��D|�D��DvgD�gD|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D#3D#|�D#��D$|�D%3D%�3D%��D&|�D&��D'|�D'��D(vgD(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/�gD0vgD0��D1|�D1��D2|�D2��D3|�D3��D4|�D53D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D?3D?|�D?�gD@vgD@��DA|�DA��DB|�DB��DC|�DD3DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DIvgDI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM�gDNvgDN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DYvgDY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^vgD^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De�3De��DfvgDf��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dk3Dk�3Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq�gDr|�Dr��Ds|�Ds��DtvgDt��Du|�Du��Dv|�Dv�gDwvgDw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{�3D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��D�>fD�~fD��fD��fD�>fD�{3D��fD��fD�>fD�~fD���D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��3D�>fD�~fD���D��D�>fD�~fD��3D��fD�A�D���D��fD��fD�>fD�~fD���D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��3D��fD�>fD�~fD��3D��fD�>fD�~fD��fD��fD�A�D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��3D��fD�A�D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��3D��fD�>fD�~fD��fD��fD�>fD�{3D��3D��3D�;3D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��fD�>fD���D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�;3D�{3D��fD��fD�;3D�{3D��3D��fD�>fD���D��fD��fD�>fD�~fD��fD��fD�A�D���D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�A�D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD»3D��fD�>fD�~fDþfD��3D�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fD���D��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��D�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��3D�;3D�~fD���D��fD�;3D�~fD���D��fD�>fDЁ�D���D��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fD���D��D�>fD�{3D־fD��fD�>fD�~fD׾fD��fD�A�D�~fDؾfD��fD�>fD�~fDپfD��fD�;3D�~fDھfD��fD�>fD�~fD���D��fD�>fD�~fDܾfD��fD�A�D�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߻3D��fD�>fD�~fD�fD��3D�>fD�~fD�fD��fD�>fD�~fD���D��D�A�D�~fD�3D��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�{3D��3D��fD�>fD�{3D�fD��fD�>fD�{3D�fD��fD�>fD�~fD�fD��fD�>fD�~fD���D��fD�>fD�~fD�fD��3D�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�{3D��fD��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�p�A���Aȡ�A�GEA��)A��A�gA�uZA��wA�רA��A��eA��A��A��A�N�A��A�ޞA�JA�GzA�	lA�a�A��SA�5tA��KA�8A��A���A��A��]A�P}A��=A���A�+A�ϫA��,A�<jA��!A�ߤA�eA�8�A�k�A��EA��A���A��WA�J�A���A���A���A��A�K�A�ɺA��"A�~A�3�A�ncA��/A�W�A���A�˒A�IA�&�A�A���A�<A�y�A���A���A�H�A��&A���A�.A���A�u�A��lA���A�ںA�}VA��YA�e�A��$A�'RA��#A�&A���A��A�n/A�+kA��UA~?�A|�fAz��Ay�XAx��AvrGAu#:As�AsH�Ap�Ao!�An�Anp;Ak��Aj�Ai��Ah��Ag�FAe�FAdߤAc��Ac.IAb�]Ab4A`�A_�A^��A\��A[2�AZ�	AZL0AY�AAYMAX�AX�AWTaAVɆAVPHAU�"AU��AU�ATv`ASt�AR��AR��AQ�DAO�|AO-wAN7AL8�AJ��AJe�AJ+AI<6AH��AGu%AFJ�AEAD+AC��AC�AB7AA�A@�jA@_pA?��A?q�A>��A=ȴA;;�A8�A7�RA7FA6��A6�A5c�A4��A4�!A4��A4�IA4��A4�DA4�A2͟A1��A0T�A/��A/�A.	A-@A,��A,V�A,=�A,{A+�mA+�A+J#A*�yA)_pA'��A&��A%�cA$��A$MA#��A#t�A"� A!�A 4A�jA��A�]AeA#�A��A�A�XA�A��A�aA�PA�A�[AQA!A�A�AC�A�A�A��A�A��A֡A\�A��AA
�*A	��A��A=A��A��ACA]dA��A\�A�?A�IA�A|�Ae,AAA��A m]@�=�@�� @� \@���@���@���@�
=@���@�/�@��w@�>�@���@�S�@�/�@��y@�6@��6@�l�@�y>@���@���@�r�@��@��@��@�V�@ߊ�@�m]@�tT@���@շ@�Ov@ӏ�@��@�}V@��@�X@̽<@�Ov@��8@�=�@�B[@�iD@�_@õt@º�@��g@��	@��H@��Q@���@���@�(�@��j@���@�Mj@���@�:*@��z@��@@�x�@�@���@�c�@�9X@��r@���@��&@���@���@�B�@�d�@��q@��@��8@�u�@���@�'�@�ی@��@���@���@���@�Y�@��@��L@�kQ@�!�@��#@�;d@��@�,=@��b@���@��'@�)�@�}�@��@��@��b@�xl@�n�@��]@�r�@�-w@���@���@�Z�@�7@��W@���@�K�@���@�H@�Vm@���@��r@��	@�e,@�F�@�$t@��@�o@���@��B@���@�[�@���@�Mj@��1@��@��U@��@��@���@��@�҉@���@�h
@�4n@�  @�ԕ@��X@��{@�\�@�J#@�*0@��@���@���@���@��Y@�M@�7@�خ@��{@��@��@���@�H@��>@���@�<6@��@��O@��A@�u%@�q�@��@�@�rG@�@@���@�r�@�[�@�?�@�4n@��@��D@���@��j@���@��4@�=@��p@���@�q@�[�@�R�@��@�	�@��]@��V@��@���@��@�c @�M@�A�@�(�@��@��@��@�k@�4@=@�@~��@~B[@}��@}Vm@|�@{�@{$t@y��@x�[@x��@x��@xm�@x!@wiD@v�H@v��@vc @v($@u��@u�~@u��@up�@u(�@t�v@t�z@t�@sS�@r�M@rE�@q�X@q-w@p~(@pQ�@p(�@o�@o|�@oZ�@n��@nQ@n+k@m�d@m�~@m&�@lM@k�+@kdZ@k!-@k�@kS@k i@j�s@j��@j{@i�T@i�-@iL�@h�5@h�Y@hQ�@hA�@h�@g�@g��@g�@g=@f�@f�'@f��@f�F@fq�@fd�@f\�@fW�@fH�@f-@e�9@e�=@ehs@e	l@d��@c�r@cx@c@O@b�8@b�h@b� @b�@bn�@b@a�@a�"@au�@`�@`  @_�r@_�+@_�A@_�@_�@_�A@_�a@_��@_C�@^��@^��@^
�@]��@]��@]@@\�@\~@[�0@Z�@ZR�@Yc�@Y�@Y@X�f@X�)@X�z@Xc�@W��@W��@W\)@W4�@W@Vc @V!�@U�#@U�@U�=@UB�@T`�@S��@S!-@S i@R��@Rn�@R&�@R �@Q�@P�I@P  @O�*@Ox@OE9@O1�@O�@N�@N��@N��@Nq�@Nc @Nc @N�@M�S@M-w@L�K@L�@LM@Lb@K�m@K��@K{J@K�@J}V@JZ�@J1�@Ju@I�)@I�T@I��@I|@H��@G�F@G>�@G)_@G!-@F��@F�@E�@E�@E�@E�Z@E�@E��@E|@D��@De�@C�;@C4�@C(@B�8@B҉@B=q@A=�@@�_@@?�@@	�@?�@?��@?�:@?J#@>�@>��@>�b@>@=��@=�C@=e,@<��@<��@<��@<1'@;�6@;��@;a@;C@:�'@:)�@:
�@9�N@9�@9��@9��@9��@9\�@9B�@8��@8�K@8�@8PH@7�m@7�	@6�8@6h
@60U@6	@5��@5�@5��@5�-@5�X@5��@5}�@5m]@5L�@5!�@5 \@5�@5�@4�@4~(@4PH@4x@3��@3)_@2�,@2��@23�@1�Z@1�h@1}�@1zx@1:�@0ی@0��@0��@0�o@0c�@0<�@0$@/��@/'�@.xl@.i�@.a|@.a|@.1�@-��@-N<@,��@,��@,l"@,"h@+�@+�@+��@+qv@+F�@*��@*��@*�<@*� @*h
@*\�@*L0@*�@)�@)�)@)�z@)��@)��@)a�@)/@)+@)�@(��@(�K@(�@(��@(��@(oi@(N�@(1'@(�@'�[@'!-@'S@&��@&��@&i�@&&�@%�Z@%�C@%�"@%N<@$�@$�p@$��@$Ĝ@$�j@$�_@$��@$�o@$y>@$?�@$%�@#�]@#y�@#P�@#H�@#�@"�H@"��@"�L@"��@"c @"B[@"$�@"�@!�@!�@!�@!�#@!�@!��@!��@!�S@!��@!��@!Q�@ �@ H@|�@;d@&@��@��@h
@Ov@+k@e@ �@�)@�@��@��@}�@�@�Y@x@��@�$@��@�:@�4@W?@�@�A@E�@�@�@	@u@�Z@�z@p�@�@�@�@�A@�}@o@�@��@d�@d�@_�@YK@Ta@Ta@M�@@�@5?@-@#:@	@ �@�N@u�@Dg@Ĝ@�.@/�@�@�@��@{J@y�@x@v`@�<@c @:*@	@��@��@�3@��@��@�@�-@��@�X@�'@�/@�D@H@b@��@,�@�@�<@i�@�Z@�S@��@N<@q@�5@�E@�@��@�?@��@�9@��@u�@S�@~@��@�@�4@dZ@P�@>�@8@'�@(@
��@
�@
O@	��@	�t@	�@	��@	a�@�@�$@��@��@�4@�@��@�.@|�@y>@Ft@ �@@�g@�@�w@�F@�F@��@�F@�F@�F@��@�@�H@�x@��@^5@8�@4@�@��@��@��@�n@��@m]@<6@�@��@��@�/@�e@��@��@��@K^@�g@�}@��@b�@6z@�@��@�c@�H@͟@�X@��@kQ@:*@1�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�p�A���Aȡ�A�GEA��)A��A�gA�uZA��wA�רA��A��eA��A��A��A�N�A��A�ޞA�JA�GzA�	lA�a�A��SA�5tA��KA�8A��A���A��A��]A�P}A��=A���A�+A�ϫA��,A�<jA��!A�ߤA�eA�8�A�k�A��EA��A���A��WA�J�A���A���A���A��A�K�A�ɺA��"A�~A�3�A�ncA��/A�W�A���A�˒A�IA�&�A�A���A�<A�y�A���A���A�H�A��&A���A�.A���A�u�A��lA���A�ںA�}VA��YA�e�A��$A�'RA��#A�&A���A��A�n/A�+kA��UA~?�A|�fAz��Ay�XAx��AvrGAu#:As�AsH�Ap�Ao!�An�Anp;Ak��Aj�Ai��Ah��Ag�FAe�FAdߤAc��Ac.IAb�]Ab4A`�A_�A^��A\��A[2�AZ�	AZL0AY�AAYMAX�AX�AWTaAVɆAVPHAU�"AU��AU�ATv`ASt�AR��AR��AQ�DAO�|AO-wAN7AL8�AJ��AJe�AJ+AI<6AH��AGu%AFJ�AEAD+AC��AC�AB7AA�A@�jA@_pA?��A?q�A>��A=ȴA;;�A8�A7�RA7FA6��A6�A5c�A4��A4�!A4��A4�IA4��A4�DA4�A2͟A1��A0T�A/��A/�A.	A-@A,��A,V�A,=�A,{A+�mA+�A+J#A*�yA)_pA'��A&��A%�cA$��A$MA#��A#t�A"� A!�A 4A�jA��A�]AeA#�A��A�A�XA�A��A�aA�PA�A�[AQA!A�A�AC�A�A�A��A�A��A֡A\�A��AA
�*A	��A��A=A��A��ACA]dA��A\�A�?A�IA�A|�Ae,AAA��A m]@�=�@�� @� \@���@���@���@�
=@���@�/�@��w@�>�@���@�S�@�/�@��y@�6@��6@�l�@�y>@���@���@�r�@��@��@��@�V�@ߊ�@�m]@�tT@���@շ@�Ov@ӏ�@��@�}V@��@�X@̽<@�Ov@��8@�=�@�B[@�iD@�_@õt@º�@��g@��	@��H@��Q@���@���@�(�@��j@���@�Mj@���@�:*@��z@��@@�x�@�@���@�c�@�9X@��r@���@��&@���@���@�B�@�d�@��q@��@��8@�u�@���@�'�@�ی@��@���@���@���@�Y�@��@��L@�kQ@�!�@��#@�;d@��@�,=@��b@���@��'@�)�@�}�@��@��@��b@�xl@�n�@��]@�r�@�-w@���@���@�Z�@�7@��W@���@�K�@���@�H@�Vm@���@��r@��	@�e,@�F�@�$t@��@�o@���@��B@���@�[�@���@�Mj@��1@��@��U@��@��@���@��@�҉@���@�h
@�4n@�  @�ԕ@��X@��{@�\�@�J#@�*0@��@���@���@���@��Y@�M@�7@�خ@��{@��@��@���@�H@��>@���@�<6@��@��O@��A@�u%@�q�@��@�@�rG@�@@���@�r�@�[�@�?�@�4n@��@��D@���@��j@���@��4@�=@��p@���@�q@�[�@�R�@��@�	�@��]@��V@��@���@��@�c @�M@�A�@�(�@��@��@��@�k@�4@=@�@~��@~B[@}��@}Vm@|�@{�@{$t@y��@x�[@x��@x��@xm�@x!@wiD@v�H@v��@vc @v($@u��@u�~@u��@up�@u(�@t�v@t�z@t�@sS�@r�M@rE�@q�X@q-w@p~(@pQ�@p(�@o�@o|�@oZ�@n��@nQ@n+k@m�d@m�~@m&�@lM@k�+@kdZ@k!-@k�@kS@k i@j�s@j��@j{@i�T@i�-@iL�@h�5@h�Y@hQ�@hA�@h�@g�@g��@g�@g=@f�@f�'@f��@f�F@fq�@fd�@f\�@fW�@fH�@f-@e�9@e�=@ehs@e	l@d��@c�r@cx@c@O@b�8@b�h@b� @b�@bn�@b@a�@a�"@au�@`�@`  @_�r@_�+@_�A@_�@_�@_�A@_�a@_��@_C�@^��@^��@^
�@]��@]��@]@@\�@\~@[�0@Z�@ZR�@Yc�@Y�@Y@X�f@X�)@X�z@Xc�@W��@W��@W\)@W4�@W@Vc @V!�@U�#@U�@U�=@UB�@T`�@S��@S!-@S i@R��@Rn�@R&�@R �@Q�@P�I@P  @O�*@Ox@OE9@O1�@O�@N�@N��@N��@Nq�@Nc @Nc @N�@M�S@M-w@L�K@L�@LM@Lb@K�m@K��@K{J@K�@J}V@JZ�@J1�@Ju@I�)@I�T@I��@I|@H��@G�F@G>�@G)_@G!-@F��@F�@E�@E�@E�@E�Z@E�@E��@E|@D��@De�@C�;@C4�@C(@B�8@B҉@B=q@A=�@@�_@@?�@@	�@?�@?��@?�:@?J#@>�@>��@>�b@>@=��@=�C@=e,@<��@<��@<��@<1'@;�6@;��@;a@;C@:�'@:)�@:
�@9�N@9�@9��@9��@9��@9\�@9B�@8��@8�K@8�@8PH@7�m@7�	@6�8@6h
@60U@6	@5��@5�@5��@5�-@5�X@5��@5}�@5m]@5L�@5!�@5 \@5�@5�@4�@4~(@4PH@4x@3��@3)_@2�,@2��@23�@1�Z@1�h@1}�@1zx@1:�@0ی@0��@0��@0�o@0c�@0<�@0$@/��@/'�@.xl@.i�@.a|@.a|@.1�@-��@-N<@,��@,��@,l"@,"h@+�@+�@+��@+qv@+F�@*��@*��@*�<@*� @*h
@*\�@*L0@*�@)�@)�)@)�z@)��@)��@)a�@)/@)+@)�@(��@(�K@(�@(��@(��@(oi@(N�@(1'@(�@'�[@'!-@'S@&��@&��@&i�@&&�@%�Z@%�C@%�"@%N<@$�@$�p@$��@$Ĝ@$�j@$�_@$��@$�o@$y>@$?�@$%�@#�]@#y�@#P�@#H�@#�@"�H@"��@"�L@"��@"c @"B[@"$�@"�@!�@!�@!�@!�#@!�@!��@!��@!�S@!��@!��@!Q�@ �@ H@|�@;d@&@��@��@h
@Ov@+k@e@ �@�)@�@��@��@}�@�@�Y@x@��@�$@��@�:@�4@W?@�@�A@E�@�@�@	@u@�Z@�z@p�@�@�@�@�A@�}@o@�@��@d�@d�@_�@YK@Ta@Ta@M�@@�@5?@-@#:@	@ �@�N@u�@Dg@Ĝ@�.@/�@�@�@��@{J@y�@x@v`@�<@c @:*@	@��@��@�3@��@��@�@�-@��@�X@�'@�/@�D@H@b@��@,�@�@�<@i�@�Z@�S@��@N<@q@�5@�E@�@��@�?@��@�9@��@u�@S�@~@��@�@�4@dZ@P�@>�@8@'�@(@
��@
�@
O@	��@	�t@	�@	��@	a�@�@�$@��@��@�4@�@��@�.@|�@y>@Ft@ �@@�g@�@�w@�F@�F@��@�F@�F@�F@��@�@�H@�x@��@^5@8�@4@�@��@��@��@�n@��@m]@<6@�@��@��@�/@�e@��@��@��@K^@�g@�}@��@b�@6z@�@��@�c@�H@͟@�X@��@kQ@:*@1�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�mB��B�:BϫB�rB��B�B�'B�sB��By�BuZBo�BlqBo Bn�Bl�BmBtBl�Bk�Bo�Bo5Br�Br|Br�Bs�Bs�Bu%Bv�Bv�Bx�Bw�B�B~�B}�B{�Bx�Br�BmCBe�B]IBP}BA;B2-B �B�B�BB�BKB;B�rB�|B��B�VB�B�VB�B��B��B��By>BaHBX+BL�B=�B.IBB�0B�AB�yB��B�MB�XB��B�fB��B|�B_�BI�B>�B6B/iB(sB B�B&B"B	B
�hB
�,B
�sB
��B
ʦB
��B
�B
�!B
��B
�FB
�$B
��B
�<B
��B
t�B
x8B
x�B
p!B
d@B
^5B
X�B
W
B
X+B
YeB
W
B
T�B
Q�B
K�B
@OB
9>B
72B
8B
6�B
5�B
5�B
4�B
.�B
-�B
,WB
+�B
*KB
%�B
 B
xB
kB
�B
4B
�B
?B	�DB	��B	�5B	�B	�sB	�B	�'B	��B	��B	��B	�dB	�=B	�B	B	��B	�HB	��B	��B	�$B	�MB	��B	�YB	��B	��B	��B	��B	��B	��B	�B	�PB	�dB	��B	��B	��B	��B	}�B	y�B	u�B	q�B	m)B	f�B	d&B	b�B	a�B	`�B	_�B	^jB	\�B	\xB	YB	Q�B	P�B	N<B	I�B	E�B	C�B	B[B	?.B	;0B	5B	-wB	&�B	�B	1B	B	TB	B	PB		B	zB	�B	 �B��B��B��B�6B��B�dB�DB�XB�fB��B�B�MB�B�iB��B�iB�B�B�B��B��B��B�B�NB��B��B�B�B��B�~B�xB�CB�KB��B�mB�gBөBҽBңB��B�B��BϑB�B�B��B��B��B�#B�_B�B�YB�9BĜB�{B��B��B�HB��B�BB�6B��B�0B�B��B��B�B�B��B�`B�2B�B��B�B��B�B��B�^B��B��B�B�B�B�wB��B�OBÖB�{B�aB�{B�MBĶBňBƎBƎB��BǔB�BȚBȀBȀBˬB�JB�~B��B��BּB�B�KB�KBڠB�B޸B�VB߾B��B�`B�fB�XB��B�B�KB�B��B�/B� B�B��B��B�VB	 �B	GB	�B	B	B	B	YB	1B	4B	?B	�B	$tB	&�B	($B	(�B	*eB	,qB	.B	1AB	4�B	7fB	<jB	@�B	B'B	C{B	EB	EmB	ESB	F?B	G�B	I7B	L�B	O�B	S�B	XEB	]~B	e�B	l�B	mB	n�B	t�B	v�B	w�B	z�B	}<B	}B	� B	��B	��B	��B	��B	��B	�DB	�~B	�~B	��B	�NB	�[B	��B	�?B	�QB	�VB	��B	��B	��B	��B	�!B	�3B	��B	�lB	��B	��B	�DB	�]B	��B	�gB	��B	�0B	��B	�.B	� B	��B	�NB	�:B	��B	�[B	ԯB	�sB	ٚB	�B	�VB	��B	�hB	�nB	�fB	�fB	�B	��B	��B	�%B	��B	��B	�B	�JB	��B	�qB	��B	�]B	��B	��B	�cB	�HB
 B
�B
�B
�B
	lB
�B
[B
�B
�B
 �B
!|B
#�B
$�B
)B
+�B
-CB
-�B
.cB
0�B
1vB
1�B
2|B
4B
5B
5�B
8B
;B
=qB
@OB
CB
D�B
G+B
HKB
H�B
I�B
J�B
K)B
L�B
MPB
M�B
O�B
P�B
R:B
T�B
V�B
X+B
X�B
Y�B
YB
Y�B
Z�B
[�B
]dB
]~B
]�B
^�B
^�B
_�B
`�B
a|B
b�B
c�B
dZB
f2B
iB
j�B
k�B
k�B
l�B
l�B
mCB
mwB
mwB
m�B
nB
o�B
p!B
p�B
q�B
s�B
utB
wfB
xB
x�B
yrB
y�B
y�B
zDB
{�B
|B
}"B
}B
�iB
�[B
�[B
�B
�'B
�AB
�uB
�[B
��B
��B
��B
�B
��B
��B
��B
�#B
��B
�6B
��B
��B
� B
��B
��B
�sB
��B
��B
��B
��B
��B
��B
��B
�]B
��B
�dB
�pB
�vB
�HB
��B
�NB
� B
��B
��B
��B
��B
�yB
��B
��B
�6B
�=B
�5B
��B
�vB
�B
��B
�B
��B
��B
�9B
�nB
��B
��B
��B
�B
��B
��B
��B
�>B
��B
��B
��B
�JB
�B
�qB
��B
�B
�}B
��B
�4B
�OB
��B
� B
��B
ĶB
�B
��B
�B
ǮB
ȀB
��B
��B
��B
ȚB
�B
ȚB
��B
��B
�xB
̳B
��B
�<B
�pB
�pB
ϫB
�hB
�&B
��B
��B
�{B
ԕB
�B
��B
�SB
ևB
ևB
��B
�B
�yB
��B
�B
�B
�QB
�qB
��B
��B
��B
�B
ݘB
ބB
�B
�;B
�pB
ߤB
�pB
ߤB
�'B
��B
��B
�B
�B
�B
��B
� B
�ZB
�FB
�zB
�FB
�B
�LB
�fB
�B
�B
�B
��B
��B
�B
�mB
�B
�mB
��B
�XB
��B
�*B
�B
�eB
�kB
�B
��B
�B
��B
��B
��B
��B
�IB
��B
��B
�OB
�5B
�iB
��B
��B
��B
��B
�3B
�MB
��B
�3B
�hB
�nB
��B
��B
��B
��B
��B
��B
�fB
��B
��B
�lB
�	B
��B
�XB
�>B
��B
��B
��B
��B
��B
�xB
��B
��B
��B
�dB
��B
��B
��B
��B
�PB
��B
��B
��B
�VB
�"B
�qB
��B
�]B
�}B
�}B
��B
��B iB �BBoB�B�B�B�BB�B�BGBGBBGB�B�B�B�B�B�BB9B�B�B%BYB�B�B�B_BEBEB+BEBzB�B�B�B�BB�B	B
�B
�B
�B^B�B�B�B0B~B�B�B�B�BBPB"BpBB�B�B�B�B�B�B}B B�BhB�B�B�B�B B�B�B�BaB�B{B�B�BSB�B�B�B�B�B�B�B�B
B$B�B
BYB�BBBBeB�B�B�B�B	B�B�B�BCB]BxB�B�B/BBB~B~B�B~B�B�B�B�B�B;B�B �B �B!HB!�B"�B"�B"�B#nB#�B#�B#�B$B#�B$B$@B$B$ZB$�B$�B%FB%�B%�B&fB&2B&fB&�B&�B&�B&�B'RB'�B(sB(�B(�B(�B(�B)DB)�B)�B*B*0B*B*KB*B*B*B*�B*�B+B+6B+�B+�B+�B+�B+�B+�B+�B+�B+�B+�B,"B-)B-wB-�B./B.cB.�B.cB.}B.�B/B/B/OB/ B/�B/�B/�B/�B/�B0oB0�B0B0;B0�B1AB1AB1vB1�B2aB2�B2�B2�B2�B2�B2�B2�B3�B3�B3�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 B�mB��B�:BϫB�rB��B�B�'B�sB��By�BuZBo�BlqBo Bn�Bl�BmBtBl�Bk�Bo�Bo5Br�Br|Br�Bs�Bs�Bu%Bv�Bv�Bx�Bw�B�B~�B}�B{�Bx�Br�BmCBe�B]IBP}BA;B2-B �B�B�BB�BKB;B�rB�|B��B�VB�B�VB�B��B��B��By>BaHBX+BL�B=�B.IBB�0B�AB�yB��B�MB�XB��B�fB��B|�B_�BI�B>�B6B/iB(sB B�B&B"B	B
�hB
�,B
�sB
��B
ʦB
��B
�B
�!B
��B
�FB
�$B
��B
�<B
��B
t�B
x8B
x�B
p!B
d@B
^5B
X�B
W
B
X+B
YeB
W
B
T�B
Q�B
K�B
@OB
9>B
72B
8B
6�B
5�B
5�B
4�B
.�B
-�B
,WB
+�B
*KB
%�B
 B
xB
kB
�B
4B
�B
?B	�DB	��B	�5B	�B	�sB	�B	�'B	��B	��B	��B	�dB	�=B	�B	B	��B	�HB	��B	��B	�$B	�MB	��B	�YB	��B	��B	��B	��B	��B	��B	�B	�PB	�dB	��B	��B	��B	��B	}�B	y�B	u�B	q�B	m)B	f�B	d&B	b�B	a�B	`�B	_�B	^jB	\�B	\xB	YB	Q�B	P�B	N<B	I�B	E�B	C�B	B[B	?.B	;0B	5B	-wB	&�B	�B	1B	B	TB	B	PB		B	zB	�B	 �B��B��B��B�6B��B�dB�DB�XB�fB��B�B�MB�B�iB��B�iB�B�B�B��B��B��B�B�NB��B��B�B�B��B�~B�xB�CB�KB��B�mB�gBөBҽBңB��B�B��BϑB�B�B��B��B��B�#B�_B�B�YB�9BĜB�{B��B��B�HB��B�BB�6B��B�0B�B��B��B�B�B��B�`B�2B�B��B�B��B�B��B�^B��B��B�B�B�B�wB��B�OBÖB�{B�aB�{B�MBĶBňBƎBƎB��BǔB�BȚBȀBȀBˬB�JB�~B��B��BּB�B�KB�KBڠB�B޸B�VB߾B��B�`B�fB�XB��B�B�KB�B��B�/B� B�B��B��B�VB	 �B	GB	�B	B	B	B	YB	1B	4B	?B	�B	$tB	&�B	($B	(�B	*eB	,qB	.B	1AB	4�B	7fB	<jB	@�B	B'B	C{B	EB	EmB	ESB	F?B	G�B	I7B	L�B	O�B	S�B	XEB	]~B	e�B	l�B	mB	n�B	t�B	v�B	w�B	z�B	}<B	}B	� B	��B	��B	��B	��B	��B	�DB	�~B	�~B	��B	�NB	�[B	��B	�?B	�QB	�VB	��B	��B	��B	��B	�!B	�3B	��B	�lB	��B	��B	�DB	�]B	��B	�gB	��B	�0B	��B	�.B	� B	��B	�NB	�:B	��B	�[B	ԯB	�sB	ٚB	�B	�VB	��B	�hB	�nB	�fB	�fB	�B	��B	��B	�%B	��B	��B	�B	�JB	��B	�qB	��B	�]B	��B	��B	�cB	�HB
 B
�B
�B
�B
	lB
�B
[B
�B
�B
 �B
!|B
#�B
$�B
)B
+�B
-CB
-�B
.cB
0�B
1vB
1�B
2|B
4B
5B
5�B
8B
;B
=qB
@OB
CB
D�B
G+B
HKB
H�B
I�B
J�B
K)B
L�B
MPB
M�B
O�B
P�B
R:B
T�B
V�B
X+B
X�B
Y�B
YB
Y�B
Z�B
[�B
]dB
]~B
]�B
^�B
^�B
_�B
`�B
a|B
b�B
c�B
dZB
f2B
iB
j�B
k�B
k�B
l�B
l�B
mCB
mwB
mwB
m�B
nB
o�B
p!B
p�B
q�B
s�B
utB
wfB
xB
x�B
yrB
y�B
y�B
zDB
{�B
|B
}"B
}B
�iB
�[B
�[B
�B
�'B
�AB
�uB
�[B
��B
��B
��B
�B
��B
��B
��B
�#B
��B
�6B
��B
��B
� B
��B
��B
�sB
��B
��B
��B
��B
��B
��B
��B
�]B
��B
�dB
�pB
�vB
�HB
��B
�NB
� B
��B
��B
��B
��B
�yB
��B
��B
�6B
�=B
�5B
��B
�vB
�B
��B
�B
��B
��B
�9B
�nB
��B
��B
��B
�B
��B
��B
��B
�>B
��B
��B
��B
�JB
�B
�qB
��B
�B
�}B
��B
�4B
�OB
��B
� B
��B
ĶB
�B
��B
�B
ǮB
ȀB
��B
��B
��B
ȚB
�B
ȚB
��B
��B
�xB
̳B
��B
�<B
�pB
�pB
ϫB
�hB
�&B
��B
��B
�{B
ԕB
�B
��B
�SB
ևB
ևB
��B
�B
�yB
��B
�B
�B
�QB
�qB
��B
��B
��B
�B
ݘB
ބB
�B
�;B
�pB
ߤB
�pB
ߤB
�'B
��B
��B
�B
�B
�B
��B
� B
�ZB
�FB
�zB
�FB
�B
�LB
�fB
�B
�B
�B
��B
��B
�B
�mB
�B
�mB
��B
�XB
��B
�*B
�B
�eB
�kB
�B
��B
�B
��B
��B
��B
��B
�IB
��B
��B
�OB
�5B
�iB
��B
��B
��B
��B
�3B
�MB
��B
�3B
�hB
�nB
��B
��B
��B
��B
��B
��B
�fB
��B
��B
�lB
�	B
��B
�XB
�>B
��B
��B
��B
��B
��B
�xB
��B
��B
��B
�dB
��B
��B
��B
��B
�PB
��B
��B
��B
�VB
�"B
�qB
��B
�]B
�}B
�}B
��B
��B iB �BBoB�B�B�B�BB�B�BGBGBBGB�B�B�B�B�B�BB9B�B�B%BYB�B�B�B_BEBEB+BEBzB�B�B�B�BB�B	B
�B
�B
�B^B�B�B�B0B~B�B�B�B�BBPB"BpBB�B�B�B�B�B�B}B B�BhB�B�B�B�B B�B�B�BaB�B{B�B�BSB�B�B�B�B�B�B�B�B
B$B�B
BYB�BBBBeB�B�B�B�B	B�B�B�BCB]BxB�B�B/BBB~B~B�B~B�B�B�B�B�B;B�B �B �B!HB!�B"�B"�B"�B#nB#�B#�B#�B$B#�B$B$@B$B$ZB$�B$�B%FB%�B%�B&fB&2B&fB&�B&�B&�B&�B'RB'�B(sB(�B(�B(�B(�B)DB)�B)�B*B*0B*B*KB*B*B*B*�B*�B+B+6B+�B+�B+�B+�B+�B+�B+�B+�B+�B+�B,"B-)B-wB-�B./B.cB.�B.cB.}B.�B/B/B/OB/ B/�B/�B/�B/�B/�B0oB0�B0B0;B0�B1AB1AB1vB1�B2aB2�B2�B2�B2�B2�B2�B2�B3�B3�B3�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230611064359  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230611064359  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230611064400  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230611064400                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230611064401  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230611064401  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20230611072621                      G�O�G�O�G�O�                