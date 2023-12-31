CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-03-23T12:42:35Z creation;2023-03-23T12:42:36Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230323124235  20230323125752  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               ~A   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @���s1   @�OW�@;�XbM��c�hr�!1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@�  A   A   A@  A`  A�  A�33A�33A�  A�  A�  A�  A�  A�33B��B  B  B   B'��B0  B8  B@  BH  BP  BXffB`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�C  C  C  C�fC�fC   C"�C$  C%�fC'�fC*�C,  C.  C0�C2  C4  C6  C8�C:  C;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~�C�  C��C�  C�  C��3C��C�  C��3C�  C�  C�  C�  C��3C��3C�  C��C��C�  C��C��C�  C�  C�  C�  C��C��C��C��C�  C�  C��3C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  D   D � D  D� DfD� DfD� D  D� D  D� DfD� D��D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D�fDfD�fD  D� DfD� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D   D y�D!  D!� D"  D"� D"��D#� D$  D$� D%  D%� D&  D&�fD'  D'� D'��D(y�D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D4��D5y�D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>�fD?  D?� D@  D@� DA  DA�fDB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DRy�DS  DS� DTfDT� DU  DU� DU��DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� D`��Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df�fDg  Dg� Dh  Dh� Di  Di�fDj  Dj� Dk  Dk� Dl  Dl� Dm  Dm�fDnfDn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Dr��Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�C3D��3D�� D���D�<�D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�C3D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D���D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D¼�D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�<�D�|�Dɼ�D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�C3Dـ D�� D���D�@ Dڀ D�� D�3D�@ Dۃ3D��3D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�C3D�3D�� D�  D�@ D� D�� D���D�<�D� D��3D�3D�@ D� D�� D���D�@ D�� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�,�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���BfgB��B��B��B'fgB/��B7��B?��BG��BO��BX33B_��Bg��Bp33Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�C�3C�3C�3CٙCٙC�3C"�C#�3C%ٙC'ٙC*�C+�3C-�3C0�C1�3C3�3C5�3C8�C9�3C;ٙC=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C~�C�3C�gC���C���C���C�gC���C���C���C���C���C���C���C���C���C�gC�gC���C�gC�gC���C���C���C���C�gC�gC�gC�gC���C���C���C���C���C�gC�gC���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C�gC�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D3D|�D3D|�D��D|�D��D|�D3D|�D�gD|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D�3D3D�3D��D|�D3D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��DvgD��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D vgD ��D!|�D!��D"|�D"�gD#|�D#��D$|�D$��D%|�D%��D&�3D&��D'|�D'�gD(vgD(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4�gD5vgD5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>�3D>��D?|�D?��D@|�D@��DA�3DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DRvgDR��DS|�DT3DT|�DT��DU|�DU�gDV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`�gDa|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df�3Df��Dg|�Dg��Dh|�Dh��Di�3Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm�3Dn3Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr�gDs|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�{3D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�A�D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�;3D�{3D��3D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�A�D�~fD��fD��fD�A�D���D��fD��3D�;3D�{3D��3D��fD�>fD�~fD��fD��fD�>fD�~fD���D��D�A�D�~fD��3D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��3D�>fD�~fD��fD��fD�A�D�~fD��fD��fD�>fD���D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�;3D�{3D��3D��fD�>fD���D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�A�D���D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�;3D�{3D»3D��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�;3D�{3Dɻ3D��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�A�D�~fDپfD��3D�>fD�~fDھfD��D�>fDہ�D���D��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�A�D�~fD�fD��fD�>fD�~fD�3D��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��D�A�D쁙D�fD��fD�>fD�~fD��fD��3D�;3D�~fD���D��D�>fD�~fD�fD��3D�>fD�~fD�fD��fD�>fD�~fD�3D��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��3D�>fD�~fD��fD��fD�+311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A���A���A��SA��"A��A�t�A�>�A�4nA�6�A�*0A�xA�A���A��A��A��jA���A�ǮA���A���A��nA��	A��A�.A�qvA�c�A�K�A�?�A�0UA�IA�bA��A��A��2A��%A���A��fA��8A���A��8A��>A��A��PA��8A���A��|A��]A���A�[�A��JA���A�7�A���A�<6A��A�TaA���A��~A��A���A�`�A��A���A��QA���A�бA�s�A� A��KA�5�A�ҽA�tTA��AA�GEA�5?A��>A�zDA��OA��mA�C�A��FA�ݘA�VA�:�A��A���A�H�A���A��MA��yA��A�A�ΥA���A��A}�MA|�MAz��Axt�Aw�AvL0At�As7LArVmAq_Ap	Ao�Anp;Ak�>Aj�Aj�	AjzxAj<6Ah��AfƨAeAd�-AcVmA`J�A_xA^�A]��A\ \AYqAX�ZAX�AW�AVQ�AT��AT%FAT  ASiDAQ�]AQR�AQ&�AP�]AP��AP^�AO��AOA�AMYKALN�AK�AKU�AJ�rAJ�AIm]AH��AGF�AF��AFHAEK^AC��ACB[AB�AB-AAaA@�A@Y�A?��A>��A=	A<�A;e�A:�>A:`BA97A7��A6  A5u�A4�#A4�A3��A2��A2\�A1�;A1�_A1jA/�A/ �A.�CA.b�A-l�A,�A*�A*L�A)�jA(�0A(jA(L0A'�zA&��A&]�A%��A$�fA$Q�A#P�A!�A |�A �A�A��A5?A�!A�AJ�AOvA�rAVA��A;�A͟A_A�bA?}A�6AMjA��A�/AQA��AJA�AO�A�+A�A��AL0A
>BAs�AȴAc AƨA��A�A	�A�A�fA�uA!�A =�@�C�@���@���@��@�I�@��M@��$@��@�`B@�@�.�@�7L@���@�1'@븻@�@�h
@��@�@��@�]d@��r@���@�@��E@�o @�K^@��P@���@��@�Z@�Vm@ЦL@�c @��
@Ο�@�5�@�?�@�0�@�b@��@���@�o�@��D@��X@��D@�Y�@�1�@�V@�e,@���@�c @�J�@�(�@�@���@��@�H�@���@�O@���@�H�@�c�@��@�?@�O@�C�@�ԕ@��:@�S�@���@�  @��A@���@�iD@��7@��@���@�`�@�Q�@��@���@��S@��@���@���@���@���@���@�a@�4@���@�Z@�6�@���@��r@��
@�rG@��,@���@��9@��m@��k@�A @���@�~(@�	�@�K�@��@��@���@��@�\�@��9@�v`@�	�@���@�m�@�خ@��X@�|@�5�@�:�@���@�B[@�M@��Q@���@��'@���@��{@�o @�O�@�@�ی@���@���@���@���@�Z�@�?�@�4@��X@���@�hs@�?}@��@�S@��5@��@�*�@�Z�@��@��@�o @�!-@���@��4@���@��+@�d�@���@���@�c�@�N<@�ں@�c�@�6�@�J@��+@���@��
@���@�@O@��@���@���@�֡@���@�y>@�Z@���@�s@�&�@��@��@��p@��'@��F@�E�@� �@�U�@��|@��@��@��@�O@�c@���@�g8@�  @��@�ϫ@��7@�C@��@�Ĝ@��;@��c@���@�ѷ@��<@���@��<@��)@��@��@�Dg@�W?@�,�@��@��]@��b@�%�@�K@iD@F�@~�h@}�9@}@@|�E@|�z@|tT@|g8@|	�@{��@{��@{��@{t�@{8@{�@z�h@z+k@y�3@y�'@x�P@xy>@x`�@x6@w�@w�0@w$t@v�@u��@u��@uA @u@u@@t�P@ty>@s�r@s��@r�H@r�@r
�@q�o@q�@q��@qV@pj@o�r@oخ@o��@oj�@oMj@oK�@o$t@n��@n�y@n��@n��@nkQ@mϫ@m@m��@mrG@m�@k�r@k�@k�*@k1�@j��@j҉@j��@j}V@j�@j	@i��@i^�@g�W@g)_@fYK@e�@e@e��@e��@e<6@d�$@d��@dh�@cƨ@bff@`�o@_J#@_,�@_�@^�'@^Ta@^\�@^kQ@]�n@]a�@]?}@];@\�I@[�@[j�@[�@[�@[�@[ i@Z�@Z��@Z�@Zߤ@Z��@Z��@Z��@Z�x@Zp;@Z�@Y��@Yc�@X�v@Xe�@X-�@W��@WMj@W�@V�@VTa@T�_@S�w@SiD@So@R�c@R��@R�r@RL0@R5?@R$�@R+k@R6�@R1�@Q��@Q�C@Q��@Q�n@Q��@Q��@Qw2@Qq@PC-@OS�@N��@Na|@N;�@N@M�@M�N@M��@Mc�@MT�@MT�@MT�@MT�@MA @L��@L�@L�U@L�O@L�.@LN�@K�&@K�@K��@K\)@J�y@JQ@J�@I�@I�#@I�S@H�K@H�D@G�r@GK�@G�@F�@F��@F�A@F��@F�+@F}V@FTa@E�3@Eo @E:�@D�@D	�@C�f@Cg�@C4�@C@B�6@BV@A��@As�@AA @A4@A \@A�@@�e@@|�@@�@?�F@?��@?��@?iD@?�@>��@>J@=�h@=F@<�@<��@<r�@<Xy@<M@<%�@;��@;�}@;��@;t�@;\)@;Y@:҉@:W�@:.�@:	@:	@9��@9Y�@9Q�@9Dg@9%F@8��@8��@8��@8|�@8?�@7�@7�{@7�@6ں@6=q@6�@6
�@5��@4��@4Ft@3˒@3�@3X�@333@3�@3 i@2��@2_�@2e@1�'@1[W@0�@0�u@0r�@0~@/x@/$t@/(@/
=@/�@.��@/S@.�8@.�@.��@.� @.u%@.:*@.0U@.#:@.u@-�#@-��@-�z@-�3@-��@-��@-�n@-�M@-a�@-�@,�U@,�_@,�@+��@*�@*�s@*��@*҉@*��@*kQ@*�@)rG@)<6@)!�@(��@(�j@(�O@(y>@($@'��@'��@'�{@'b�@'RT@'!-@&�y@&��@&s�@&\�@&E�@&�@%�@%�@%�^@%��@%Y�@%8�@%0�@%+�@%@@$�P@$��@$7@#�A@#�@#��@#l�@#$t@#�@"��@"� @"-@"
�@!��@!��@!��@!�H@!��@!f�@!N<@!%F@ �5@ �4@ oi@��@��@��@��@{J@O@��@�X@��@l�@L0@��@��@�X@��@L�@5�@%F@�@�@�@ѷ@�@�9@��@�4@��@��@bN@�K@�@��@��@��@��@��@l�@h
@a|@\�@L0@6�@($@u@�N@@�@p�@^�@T�@B�@%@��@Q�@$@�@��@� @��@A�@�@�2@��@��@R�@Ov@@�@)�@J@��@e,@q@	l@ѷ@��@��@��@�u@�_@~(@r�@u�@bN@�@�w@�@=@��@��@�,@��@ȴ@�<@�x@kQ@@�"@�@�?@�4@�Y@h�@C-@1'@1'@~@�@��@�w@�*@��@�$@��@X�@�@�@S@ں@��@i�@R�@@�@u@�@��@�3@�@�M@S&@Vm@<6@��@�j@��@��@��@�I@�.@�@y>@g8@M@<�@'R@@1@��@x@iD@Y@(@�@@
��@
�@
��@
�@
��@
�r@
l�@
M�@
+k@
@	��@	��@	^�@	!�@ѷ@��@u�@m�@:�@(�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A���A���A��SA��"A��A�t�A�>�A�4nA�6�A�*0A�xA�A���A��A��A��jA���A�ǮA���A���A��nA��	A��A�.A�qvA�c�A�K�A�?�A�0UA�IA�bA��A��A��2A��%A���A��fA��8A���A��8A��>A��A��PA��8A���A��|A��]A���A�[�A��JA���A�7�A���A�<6A��A�TaA���A��~A��A���A�`�A��A���A��QA���A�бA�s�A� A��KA�5�A�ҽA�tTA��AA�GEA�5?A��>A�zDA��OA��mA�C�A��FA�ݘA�VA�:�A��A���A�H�A���A��MA��yA��A�A�ΥA���A��A}�MA|�MAz��Axt�Aw�AvL0At�As7LArVmAq_Ap	Ao�Anp;Ak�>Aj�Aj�	AjzxAj<6Ah��AfƨAeAd�-AcVmA`J�A_xA^�A]��A\ \AYqAX�ZAX�AW�AVQ�AT��AT%FAT  ASiDAQ�]AQR�AQ&�AP�]AP��AP^�AO��AOA�AMYKALN�AK�AKU�AJ�rAJ�AIm]AH��AGF�AF��AFHAEK^AC��ACB[AB�AB-AAaA@�A@Y�A?��A>��A=	A<�A;e�A:�>A:`BA97A7��A6  A5u�A4�#A4�A3��A2��A2\�A1�;A1�_A1jA/�A/ �A.�CA.b�A-l�A,�A*�A*L�A)�jA(�0A(jA(L0A'�zA&��A&]�A%��A$�fA$Q�A#P�A!�A |�A �A�A��A5?A�!A�AJ�AOvA�rAVA��A;�A͟A_A�bA?}A�6AMjA��A�/AQA��AJA�AO�A�+A�A��AL0A
>BAs�AȴAc AƨA��A�A	�A�A�fA�uA!�A =�@�C�@���@���@��@�I�@��M@��$@��@�`B@�@�.�@�7L@���@�1'@븻@�@�h
@��@�@��@�]d@��r@���@�@��E@�o @�K^@��P@���@��@�Z@�Vm@ЦL@�c @��
@Ο�@�5�@�?�@�0�@�b@��@���@�o�@��D@��X@��D@�Y�@�1�@�V@�e,@���@�c @�J�@�(�@�@���@��@�H�@���@�O@���@�H�@�c�@��@�?@�O@�C�@�ԕ@��:@�S�@���@�  @��A@���@�iD@��7@��@���@�`�@�Q�@��@���@��S@��@���@���@���@���@���@�a@�4@���@�Z@�6�@���@��r@��
@�rG@��,@���@��9@��m@��k@�A @���@�~(@�	�@�K�@��@��@���@��@�\�@��9@�v`@�	�@���@�m�@�خ@��X@�|@�5�@�:�@���@�B[@�M@��Q@���@��'@���@��{@�o @�O�@�@�ی@���@���@���@���@�Z�@�?�@�4@��X@���@�hs@�?}@��@�S@��5@��@�*�@�Z�@��@��@�o @�!-@���@��4@���@��+@�d�@���@���@�c�@�N<@�ں@�c�@�6�@�J@��+@���@��
@���@�@O@��@���@���@�֡@���@�y>@�Z@���@�s@�&�@��@��@��p@��'@��F@�E�@� �@�U�@��|@��@��@��@�O@�c@���@�g8@�  @��@�ϫ@��7@�C@��@�Ĝ@��;@��c@���@�ѷ@��<@���@��<@��)@��@��@�Dg@�W?@�,�@��@��]@��b@�%�@�K@iD@F�@~�h@}�9@}@@|�E@|�z@|tT@|g8@|	�@{��@{��@{��@{t�@{8@{�@z�h@z+k@y�3@y�'@x�P@xy>@x`�@x6@w�@w�0@w$t@v�@u��@u��@uA @u@u@@t�P@ty>@s�r@s��@r�H@r�@r
�@q�o@q�@q��@qV@pj@o�r@oخ@o��@oj�@oMj@oK�@o$t@n��@n�y@n��@n��@nkQ@mϫ@m@m��@mrG@m�@k�r@k�@k�*@k1�@j��@j҉@j��@j}V@j�@j	@i��@i^�@g�W@g)_@fYK@e�@e@e��@e��@e<6@d�$@d��@dh�@cƨ@bff@`�o@_J#@_,�@_�@^�'@^Ta@^\�@^kQ@]�n@]a�@]?}@];@\�I@[�@[j�@[�@[�@[�@[ i@Z�@Z��@Z�@Zߤ@Z��@Z��@Z��@Z�x@Zp;@Z�@Y��@Yc�@X�v@Xe�@X-�@W��@WMj@W�@V�@VTa@T�_@S�w@SiD@So@R�c@R��@R�r@RL0@R5?@R$�@R+k@R6�@R1�@Q��@Q�C@Q��@Q�n@Q��@Q��@Qw2@Qq@PC-@OS�@N��@Na|@N;�@N@M�@M�N@M��@Mc�@MT�@MT�@MT�@MT�@MA @L��@L�@L�U@L�O@L�.@LN�@K�&@K�@K��@K\)@J�y@JQ@J�@I�@I�#@I�S@H�K@H�D@G�r@GK�@G�@F�@F��@F�A@F��@F�+@F}V@FTa@E�3@Eo @E:�@D�@D	�@C�f@Cg�@C4�@C@B�6@BV@A��@As�@AA @A4@A \@A�@@�e@@|�@@�@?�F@?��@?��@?iD@?�@>��@>J@=�h@=F@<�@<��@<r�@<Xy@<M@<%�@;��@;�}@;��@;t�@;\)@;Y@:҉@:W�@:.�@:	@:	@9��@9Y�@9Q�@9Dg@9%F@8��@8��@8��@8|�@8?�@7�@7�{@7�@6ں@6=q@6�@6
�@5��@4��@4Ft@3˒@3�@3X�@333@3�@3 i@2��@2_�@2e@1�'@1[W@0�@0�u@0r�@0~@/x@/$t@/(@/
=@/�@.��@/S@.�8@.�@.��@.� @.u%@.:*@.0U@.#:@.u@-�#@-��@-�z@-�3@-��@-��@-�n@-�M@-a�@-�@,�U@,�_@,�@+��@*�@*�s@*��@*҉@*��@*kQ@*�@)rG@)<6@)!�@(��@(�j@(�O@(y>@($@'��@'��@'�{@'b�@'RT@'!-@&�y@&��@&s�@&\�@&E�@&�@%�@%�@%�^@%��@%Y�@%8�@%0�@%+�@%@@$�P@$��@$7@#�A@#�@#��@#l�@#$t@#�@"��@"� @"-@"
�@!��@!��@!��@!�H@!��@!f�@!N<@!%F@ �5@ �4@ oi@��@��@��@��@{J@O@��@�X@��@l�@L0@��@��@�X@��@L�@5�@%F@�@�@�@ѷ@�@�9@��@�4@��@��@bN@�K@�@��@��@��@��@��@l�@h
@a|@\�@L0@6�@($@u@�N@@�@p�@^�@T�@B�@%@��@Q�@$@�@��@� @��@A�@�@�2@��@��@R�@Ov@@�@)�@J@��@e,@q@	l@ѷ@��@��@��@�u@�_@~(@r�@u�@bN@�@�w@�@=@��@��@�,@��@ȴ@�<@�x@kQ@@�"@�@�?@�4@�Y@h�@C-@1'@1'@~@�@��@�w@�*@��@�$@��@X�@�@�@S@ں@��@i�@R�@@�@u@�@��@�3@�@�M@S&@Vm@<6@��@�j@��@��@��@�I@�.@�@y>@g8@M@<�@'R@@1@��@x@iD@Y@(@�@@
��@
�@
��@
�@
��@
�r@
l�@
M�@
+k@
@	��@	��@	^�@	!�@ѷ@��@u�@m�@:�@(�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B�jB��B��B��B�oB�uB�tBбBҽB�TB�B�EB�kB�]B�BܬB��B�~B��B��B�~B�~B��BݘB�dBܒB�xB��B�=B�#B��B��B�	B�	B�=BیB��B��B��B�)B�)B�]BܒBܒB��B�]B�B�=B�kB�BΥBȚB��B�nB��B�B�aBt�B^�BIlB49B+�BpB�B�$B�B�BɺB�B�PB��B��B�)B�Bt�B0�BUB��B��B�yB�MB��B��B��B��B�uBk�BaBW?BNVBE9B-�B#�B�BIB�B	RBB
�LB
�qB
�B
�/B
�$B
��B
��B
��B
��B
�MB
��B
�B
��B
��B
�2B
�{B
�BB
�AB
w�B
p�B
k�B
YKB
QNB
K�B
F�B
A;B
/iB
*�B
)DB
$�B
VB
�B
�B
:B
�B
)B
EB
�B
B
�B
uB
 B	��B	��B	�B	��B	�B	�B	�B	��B	�B	�B	�B	רB	ּB	��B	��B	��B	�KB	��B	��B	�4B	��B	��B	��B	�GB	��B	��B	�B	�B	��B	�kB	��B	��B	��B	��B	��B	�DB	��B	�EB	��B	�aB	}�B	{0B	y>B	u�B	rB	iDB	g�B	fB	b�B	`vB	_;B	^5B	ZB	WYB	UMB	QB	N"B	K^B	D�B	@�B	>BB	<�B	;�B	:�B	8B	5�B	33B	.IB	*�B	$�B	#B	 �B	;B	jB	�B	kB	yB	�B	2B	 B	.B	�B	
�B	
�B	 4B��B��B�DB��B�?B�B�B�B�B�B�B�B��B�TB�B��B�;BݘB�]B�B�BרB�sBևB�B�B�B��BԯB��BԕB�,BҽBөB�@B�FB�{B��B��BѝBЗBбB�VB̈́B̈́B��B�6B�\B��BѷBѝB��B�
BյB�QB�pB�'B�bB�NB��B�B�nB��B�B��B�B�B�_B�_B�DB�*B��B�6B�qB��B�)B�"B�B�B�B��B��B�`B��B��B��B�^B�jB��B	-B	�B	9B	�B	pB	�B	�B	�B	B	�B	YB	�B	VB	"�B	# B	#nB	$�B	%�B	&�B	'�B	+�B	,WB	0B	5?B	8B	9XB	<B	BAB	G�B	L~B	N"B	PB	Q�B	TB	VB	ZkB	^B	`\B	b�B	f�B	l=B	oOB	p�B	t�B	utB	u?B	vFB	vFB	v�B	zDB	{B	B	�B	�AB	��B	�mB	�B	�tB	��B	�_B	��B	�#B	�B	�xB	��B	�0B	��B	�B	��B	�B	�,B	��B	��B	��B	�B	��B	�&B	��B	��B	�B	�tB	�B	��B	�LB	�	B	�"B	�BB	�.B	��B	ȴB	ʌB	��B	��B	ѷB	�aB	�SB	�1B	�7B	ڠB	��B	��B	�)B	�/B	��B	��B	�B	��B	�B	�RB	�sB	�0B	�6B	�B	�!B	�B	�3B	��B	�B	�B	��B	��B	�rB	�rB	�rB	��B	�B	��B	��B	�B	��B	�jB	��B	��B	�B
 4B
mB
B
�B
�B
B
:B
�B
FB
MB
�B
�B
�B
VB
!-B
#TB
%�B
)�B
,�B
0B
2B
5�B
7�B
8�B
8�B
8�B
8�B
8�B
9rB
9XB
9rB
9�B
9>B
9�B
9�B
9�B
:*B
;B
<6B
@�B
C�B
DMB
D�B
E9B
F�B
HfB
K�B
L~B
L�B
MjB
M�B
M�B
N<B
O�B
QB
R B
UB
XEB
X�B
YKB
Z�B
[�B
]�B
`BB
a�B
a�B
a�B
cTB
dtB
d�B
e�B
fLB
gB
h>B
h�B
i�B
l�B
lWB
l"B
l�B
m�B
qvB
q�B
shB
tTB
uZB
v`B
wfB
xRB
yrB
y�B
y�B
zDB
|jB
}�B
��B
�AB
��B
�{B
�9B
�EB
��B
��B
�B
�rB
��B
��B
�{B
��B
��B
�9B
��B
��B
�sB
�B
��B
��B
��B
��B
��B
�B
��B
��B
��B
�B
�-B
�HB
�bB
�|B
��B
��B
�B
�4B
��B
��B
��B
�FB
��B
�
B
�sB
��B
��B
�B
��B
�qB
�[B
�-B
��B
�3B
��B
�B
�hB
��B
��B
��B
��B
��B
��B
�9B
�TB
�TB
�nB
�TB
�TB
��B
�?B
��B
�DB
��B
�(B
��B
��B
�HB
��B
�4B
��B
��B
��B
��B
��B
� B
��B
��B
�[B
�uB
��B
�aB
�gB
ĶB
�B
ňB
ƨB
�1B
ȴB
�B
�B
ɺB
�)B
��B
�6B
οB
�(B
�BB
��B
�HB
�bB
�.B
�HB
ЗB
� B
ңB
�B
өB
ՁB
ּB
ּB
�$B
׍B
�+B
��B
��B
چB
�	B
�	B
�=B
�WB
�)B
�]B
�dB
�B
�B
�B
�jB
�!B
ߤB
�\B
��B
�HB
�B
��B
�:B
�TB
�TB
�B
�@B
�ZB
��B
��B
�B
�FB
��B
�B
�B
�B
�RB
�sB
��B
��B
��B
�B
�_B
�_B
��B
�KB
�B
�QB
�B
�qB
�)B
�IB
�IB
�cB
�5B
�;B
�vB
�B
�aB
�B
�B
�|B
�B
��B
�3B
�B
��B
��B
��B
��B
��B
�B
�B
��B
��B
�B
�%B
�ZB
�%B
�tB
��B
�FB
�`B
��B
�2B
�2B
�LB
��B
��B
�B
�B
�B
�8B
�RB
�lB
�lB
��B
��B
�	B
�>B
�*B
��B
��B
��B
��B
��B
�B
��B
��B
��B
�B
�BB
��B
��B
��B
�B
��B B 4B �B �B �BBoBB'B[BuB�BGBaB{B�B3BgB�B�B�B�B9BBtB?BtB+B_B�B�BfB	B	7B	lB	�B	�B	�B	�B
	B
=B
XB
�B
�BDB~BJB~B~B�B�BjB�B�BB<B�B�B�BB�B�B�B�BB�BbBbB}B�B�B�B�B B B�B[B@B[BuB�B�B�B�B�B�BB,B{B�B�B�BgBgBgB�B�B9B�B�B?B?BYBsBEBEB�B�BBeBeBB�B�B7B�B	B#BqB�B�B�B�B�B�B�B�B�B]B�B�B~B�BBB5BOB5B�B�B!B�B �B �B!-B!HB!�B!�B!�B!�B!�B"B"hB"�B"�B"�B"�B"�B#:B#�B#nB#�B#�B#�B$�B$�B$�B%,B%FB%,B%`B%`B%�B%�B%�B&B&�B&�B&�B'B'B'B'8B'8B'RB'mB'�B'�B'�B'�B'�B(�B(�B(�B)_B)_B)_B)DB)yB)yB)�B)�B)�B*B*KB*eB*�B*�B+QB+�B+�B,qB,�B-wB-�B-�B.�B.�B/�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444B��B��B�jB��B��B��B�oB�uB�tBбBҽB�TB�B�EB�kB�]B�BܬB��B�~B��B��B�~B�~B��BݘB�dBܒB�xB��B�=B�#B��B��B�	B�	B�=BیB��B��B��B�)B�)B�]BܒBܒB��B�]B�B�=B�kB�BΥBȚB��B�nB��B�B�aBt�B^�BIlB49B+�BpB�B�$B�B�BɺB�B�PB��B��B�)B�Bt�B0�BUB��B��B�yB�MB��B��B��B��B�uBk�BaBW?BNVBE9B-�B#�B�BIB�B	RBB
�LB
�qB
�B
�/B
�$B
��B
��B
��B
��B
�MB
��B
�B
��B
��B
�2B
�{B
�BB
�AB
w�B
p�B
k�B
YKB
QNB
K�B
F�B
A;B
/iB
*�B
)DB
$�B
VB
�B
�B
:B
�B
)B
EB
�B
B
�B
uB
 B	��B	��B	�B	��B	�B	�B	�B	��B	�B	�B	�B	רB	ּB	��B	��B	��B	�KB	��B	��B	�4B	��B	��B	��B	�GB	��B	��B	�B	�B	��B	�kB	��B	��B	��B	��B	��B	�DB	��B	�EB	��B	�aB	}�B	{0B	y>B	u�B	rB	iDB	g�B	fB	b�B	`vB	_;B	^5B	ZB	WYB	UMB	QB	N"B	K^B	D�B	@�B	>BB	<�B	;�B	:�B	8B	5�B	33B	.IB	*�B	$�B	#B	 �B	;B	jB	�B	kB	yB	�B	2B	 B	.B	�B	
�B	
�B	 4B��B��B�DB��B�?B�B�B�B�B�B�B�B��B�TB�B��B�;BݘB�]B�B�BרB�sBևB�B�B�B��BԯB��BԕB�,BҽBөB�@B�FB�{B��B��BѝBЗBбB�VB̈́B̈́B��B�6B�\B��BѷBѝB��B�
BյB�QB�pB�'B�bB�NB��B�B�nB��B�B��B�B�B�_B�_B�DB�*B��B�6B�qB��B�)B�"B�B�B�B��B��B�`B��B��B��B�^B�jB��B	-B	�B	9B	�B	pB	�B	�B	�B	B	�B	YB	�B	VB	"�B	# B	#nB	$�B	%�B	&�B	'�B	+�B	,WB	0B	5?B	8B	9XB	<B	BAB	G�B	L~B	N"B	PB	Q�B	TB	VB	ZkB	^B	`\B	b�B	f�B	l=B	oOB	p�B	t�B	utB	u?B	vFB	vFB	v�B	zDB	{B	B	�B	�AB	��B	�mB	�B	�tB	��B	�_B	��B	�#B	�B	�xB	��B	�0B	��B	�B	��B	�B	�,B	��B	��B	��B	�B	��B	�&B	��B	��B	�B	�tB	�B	��B	�LB	�	B	�"B	�BB	�.B	��B	ȴB	ʌB	��B	��B	ѷB	�aB	�SB	�1B	�7B	ڠB	��B	��B	�)B	�/B	��B	��B	�B	��B	�B	�RB	�sB	�0B	�6B	�B	�!B	�B	�3B	��B	�B	�B	��B	��B	�rB	�rB	�rB	��B	�B	��B	��B	�B	��B	�jB	��B	��B	�B
 4B
mB
B
�B
�B
B
:B
�B
FB
MB
�B
�B
�B
VB
!-B
#TB
%�B
)�B
,�B
0B
2B
5�B
7�B
8�B
8�B
8�B
8�B
8�B
9rB
9XB
9rB
9�B
9>B
9�B
9�B
9�B
:*B
;B
<6B
@�B
C�B
DMB
D�B
E9B
F�B
HfB
K�B
L~B
L�B
MjB
M�B
M�B
N<B
O�B
QB
R B
UB
XEB
X�B
YKB
Z�B
[�B
]�B
`BB
a�B
a�B
a�B
cTB
dtB
d�B
e�B
fLB
gB
h>B
h�B
i�B
l�B
lWB
l"B
l�B
m�B
qvB
q�B
shB
tTB
uZB
v`B
wfB
xRB
yrB
y�B
y�B
zDB
|jB
}�B
��B
�AB
��B
�{B
�9B
�EB
��B
��B
�B
�rB
��B
��B
�{B
��B
��B
�9B
��B
��B
�sB
�B
��B
��B
��B
��B
��B
�B
��B
��B
��B
�B
�-B
�HB
�bB
�|B
��B
��B
�B
�4B
��B
��B
��B
�FB
��B
�
B
�sB
��B
��B
�B
��B
�qB
�[B
�-B
��B
�3B
��B
�B
�hB
��B
��B
��B
��B
��B
��B
�9B
�TB
�TB
�nB
�TB
�TB
��B
�?B
��B
�DB
��B
�(B
��B
��B
�HB
��B
�4B
��B
��B
��B
��B
��B
� B
��B
��B
�[B
�uB
��B
�aB
�gB
ĶB
�B
ňB
ƨB
�1B
ȴB
�B
�B
ɺB
�)B
��B
�6B
οB
�(B
�BB
��B
�HB
�bB
�.B
�HB
ЗB
� B
ңB
�B
өB
ՁB
ּB
ּB
�$B
׍B
�+B
��B
��B
چB
�	B
�	B
�=B
�WB
�)B
�]B
�dB
�B
�B
�B
�jB
�!B
ߤB
�\B
��B
�HB
�B
��B
�:B
�TB
�TB
�B
�@B
�ZB
��B
��B
�B
�FB
��B
�B
�B
�B
�RB
�sB
��B
��B
��B
�B
�_B
�_B
��B
�KB
�B
�QB
�B
�qB
�)B
�IB
�IB
�cB
�5B
�;B
�vB
�B
�aB
�B
�B
�|B
�B
��B
�3B
�B
��B
��B
��B
��B
��B
�B
�B
��B
��B
�B
�%B
�ZB
�%B
�tB
��B
�FB
�`B
��B
�2B
�2B
�LB
��B
��B
�B
�B
�B
�8B
�RB
�lB
�lB
��B
��B
�	B
�>B
�*B
��B
��B
��B
��B
��B
�B
��B
��B
��B
�B
�BB
��B
��B
��B
�B
��B B 4B �B �B �BBoBB'B[BuB�BGBaB{B�B3BgB�B�B�B�B9BBtB?BtB+B_B�B�BfB	B	7B	lB	�B	�B	�B	�B
	B
=B
XB
�B
�BDB~BJB~B~B�B�BjB�B�BB<B�B�B�BB�B�B�B�BB�BbBbB}B�B�B�B�B B B�B[B@B[BuB�B�B�B�B�B�BB,B{B�B�B�BgBgBgB�B�B9B�B�B?B?BYBsBEBEB�B�BBeBeBB�B�B7B�B	B#BqB�B�B�B�B�B�B�B�B�B]B�B�B~B�BBB5BOB5B�B�B!B�B �B �B!-B!HB!�B!�B!�B!�B!�B"B"hB"�B"�B"�B"�B"�B#:B#�B#nB#�B#�B#�B$�B$�B$�B%,B%FB%,B%`B%`B%�B%�B%�B&B&�B&�B&�B'B'B'B'8B'8B'RB'mB'�B'�B'�B'�B'�B(�B(�B(�B)_B)_B)_B)DB)yB)yB)�B)�B)�B*B*KB*eB*�B*�B+QB+�B+�B,qB,�B-wB-�B-�B.�B.�B/�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230323124234  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230323124235  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230323124236  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230323124236                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230323124237  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230323124237  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20230323125752                      G�O�G�O�G�O�                