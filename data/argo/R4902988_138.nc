CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-07-21T03:47:59Z creation;2023-07-21T03:48:00Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230721034759  20230721040046  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @�;�G�{1   @�;��'O@<�O�;d�c۝�-V1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @���A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�33A�33B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B���B�  B�33B�33B�33B�  B�  B�33B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33C   C�fC�fC  C  C
  C  C  C  C  C  C�C�C  C  C  C   C"  C$�C%�fC(  C*  C,  C-�fC0  C1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn  Cp  Cr�Ct  Cv�Cx  Cz  C|  C}�fC�  C�  C�  C��C�  C��C�  C�  C��C��3C�  C�  C�  C��C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C��3C��3C�  C�  C��C��C�  C��3C��3C��3C��3C�  C�  C��C��C��C�  C�  C��C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C��3C��3D y�D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D	  D	� D	��D
� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#y�D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0fD0� D0��D1y�D2  D2� D3  D3� D4  D4� D5  D5� D6fD6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG�fDH  DH� DH��DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN�fDO  DO� DP  DPy�DP��DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[fD[�fD\  D\y�D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Db��Dcy�Dc��Dd� De  De� Df  Df� Dg  Dg�fDh  Dhy�Di  Di� Dj  Dj�fDkfDk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr�fDsfDs� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{y�D|  D|� D}  D}�fD~  D~� D  D�fD�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D���D�<�D�|�D���D�  D�<�D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D��3D�3D�C3D�� D��3D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�C3D��3D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�<�D�� D��3D�3D�@ D�� D�� D���D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�3D�C3Dǀ D�� D���D�<�DȀ D�� D�3D�@ Dɀ D�� D�  D�@ Dʀ D�� D���D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D��3D�  D�@ DӀ D��3D�  D�@ DԀ DԼ�D���D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ D�|�D�� D�  D�<�D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D޼�D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D��D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�3D��3D�  D�@ D�3D��3D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@fg@|��@�33@�ffA33A?33A_33A33A���A���A���A�fgAϙ�Aߙ�A���A���B��B��B��B��B'��B/��B7��B@33BG��BO��BW��B_��Bh33Bo��Bw��B��B��fB��fB��fB��B��fB��fB��fB��fB��3B��fB��fB��fB��fB��3B��fB��B��B��B��fB��fB��B��fB��fB��fB��fB��fB��fB��B��fB��fB��B��fCٙCٙC�3C�3C	�3C�3C�3C�3C�3C�3C�C�C�3C�3C�3C�3C!�3C$�C%ٙC'�3C)�3C+�3C-ٙC/�3C1ٙC3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CEٙCG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Cl�Cm�3Co�3Cr�Cs�3Cv�Cw�3Cy�3C{�3C}ٙC�3C���C���C�gC���C�gC���C���C�gC���C���C���C���C�gC���C���C���C�gC���C���C���C���C���C���C�gC���C���C���C�gC���C���C���C���C���C���C���C�gC�gC���C���C���C���C���C���C���C�gC�gC�gC���C���C�gC���C���C���C���C�gC�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C�gC�gC���C���C���D vgD ��D�3D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��DvgD��D	|�D	�gD
|�D3D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#vgD#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D03D0|�D0�gD1vgD1��D2|�D2��D3|�D3��D4|�D4��D5|�D63D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG�3DG��DH|�DH�gDI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN�3DN��DO|�DO��DPvgDP�gDQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�D[3D[�3D[��D\vgD\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db�gDcvgDc�gDd|�Dd��De|�De��Df|�Df��Dg�3Dg��DhvgDh��Di|�Di��Dj�3Dk3Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr�3Ds3Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{vgD{��D||�D|��D}�3D}��D~|�D~��D�3D��D�;3D�{3D��fD��fD�>fD�~fD��fD��fD�A�D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�A�D�~fD��fD��fD�>fD�~fD��fD��3D�;3D�{3D��3D��fD�;3D�~fD��fD��fD�>fD�~fD��3D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��D�>fD�~fD��fD��fD�>fD�~fD���D��D�A�D�~fD���D��fD�;3D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��fD�>fD�~fD��fD��fD�>fD�~fD���D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�;3D�~fD��fD��fD�A�D���D��fD��fD�;3D�{3D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��3D��fD�>fD�~fD��fD��fD�>fD�{3D��3D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��3D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�;3D�{3D��fD��fD�>fD�~fD��fD��fD�;3D�~fD���D��D�>fD�~fD��fD��3D�>fD�~fD��fD��3D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��3D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��3D�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��D�A�D�~fDǾfD��3D�;3D�~fDȾfD��D�>fD�~fDɾfD��fD�>fD�~fDʾfD��3D�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fD���D��fD�>fD�~fD���D��fD�>fD�~fDԻ3D��3D�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�{3D۾fD��fD�;3D�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޻3D��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��D�>fD�~fD�3D��3D�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�A�D큙D���D��fD�>fDD���D��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��BAϿAϮ�Aϊ�A�MA��A��fA��mA��/A��EA�֡Aο�A�S�A��OA��A�CA��A�l�A�L�A�e�A��A���A�(�A��A��-A�� A���A��A�}�A���A��A�y�A�OA�)*A�یA�d�A�A��?A��<A�t�A���A���A���A��A��CA���A�W?A���A� \A��MA��8A�PHA��A��A�m)A���A�3�A��A�|�A���A�FA�y>A�~]A��MA�+kA�b�A�)_A�P}A�.IA��A�+A��}A�,A���A��A�d&A��5A�UgA��A�1A�&LA�}�A���A�oiA�}"A�PA��VA��A��A~��A{m]Az�KAy�Aw��At;dAs+kAra�Aq��Ao��An��An!�Am�Ak��Aj�AjSAi��Af�2AeE�Ad��Ab��AaS&A`n/A_Y�A^A�A]u�A\�qAZ�<AY��AXq�AX	lAW�AW�MAW��AW�DAXuAW��AW�AV,=AU��AUZ�AU	AT�AR1�AQ�tAP�\AO:�AN�KAN\�AL�&AK҉AJ}�AIqvAH��AG�BAG1'AE8�AC�8AC�AB^�AAl�AA1�A@��A@��A@��A@bNA?@OA>��A=�A=YA<�'A<U2A;�&A:C�A9"hA8rGA7�zA6�A55�A4�kA3�A2֡A1��A1	A0j�A/��A.�>A.��A.��A.��A.U�A-҉A-�-A,�mA+�bA*��A*VmA)��A)_pA(��A'ߤA'IRA& �A$}�A#XA"T�A!�eA!�A ��A U2A��A�`A�A��A{�A4AaA[�A�FA6�A��A�A�*Ap;A�A�/A~�A+A�\Ag8A?AT�A��A\)Ah�A�.Ar�A�A�A�HA��A
�A
�HA
�FA
y�A	��A	%A�fAA|�A�A�XA�QA��A��A�A[�AAE9A >B@���@�{@���@���@���@�$@���@���@��h@�zx@��@�q�@��@�o@�|�@�	@�҉@�@��a@ޞ@��@ݫ�@�Z�@ܿ�@�2�@�|�@�˒@�H�@�{�@ѷ@���@�@�X�@λ�@�@��@�PH@��@�4@��@��s@�˒@�c�@�($@�IR@��,@�6@�J#@���@�l"@�#:@��@�\)@��0@�L0@�+@�h�@�J�@�(�@�U�@�1�@���@��@��f@��}@�$�@�o�@�"�@� i@�ѷ@��L@���@���@��@��~@���@�=q@�m]@��X@��u@�V�@�@���@��@�8�@��@���@��B@�2a@�T�@��@��+@�1�@��M@�h�@��M@�j�@��@��@��O@�m�@�!�@��@���@���@��@���@���@��$@���@�(@��@�)_@��	@�e,@��@���@�xl@�"h@��Q@���@�_�@�o @�S�@��@��[@�tT@�L0@��@�e�@��!@�A�@���@�	l@���@��@�\�@� �@���@���@��Q@��K@���@���@��~@��{@�=@��@�q�@�-@�9X@�/�@���@��~@�W?@��"@��/@��o@���@��@�`�@���@�{J@�=�@�H�@�(@���@�=q@���@���@��4@�J�@�?}@��v@�Ov@��o@���@��Z@���@���@�ԕ@���@��@�+@�2�@���@��@��#@� �@�)�@�0U@���@�+@�S�@�$@�@~�@iD@1�@E9@4�@Y@~��@}�3@|�P@|��@|@{��@{>�@{4�@z�x@zOv@zO@yԕ@yϫ@y�C@y��@y��@y�h@yo @yF@y&�@x�@x�@xQ�@x/�@w��@w+@w�@v�r@uDg@tXy@tx@s�
@s�@s�@s��@s��@r�@q�j@qe,@q�@q;@p�p@pbN@pH@o��@oqv@o=@o'�@oC@n��@nff@m�3@m��@m[W@m=�@m&�@l��@lH@l1@kt�@k�@j~�@i��@i&�@hr�@hu�@h?�@g!-@fz@f#:@f�@e�D@e��@eԕ@e�H@eS&@dĜ@d1'@c�@c��@c��@c��@cA�@b�@b�m@b��@b-@a�z@a/@`w�@`H@`<�@`/�@`�@`�@_��@_��@_�:@_\)@_1�@^�8@^�s@^�'@^�L@^{�@^^5@^)�@]�@]�n@]�M@]f�@]8�@\�v@\H@[��@[�k@[iD@[$t@Z�y@Zȴ@Z��@Z8�@Z{@Y�o@Y�@X�$@W��@W�	@Wqv@Wb�@W1�@W(@V��@Vn�@Uԕ@Uc�@U*0@UV@U%@Toi@T(�@S��@S��@S]�@S@O@S&@R�c@R��@R&�@Q�#@Q��@Q`B@Q^�@Q<6@P�p@Ph�@P<�@P  @O��@O1�@N��@M�Z@M�M@M\�@L�|@L�j@L�@LS�@L1'@Lx@K� @K|�@K;d@J��@J��@Jd�@J�@I��@Ix�@I�@H��@Hy>@G�A@G��@G��@Gv`@GY@Fn�@Fa|@F\�@F^5@FYK@FGE@F_@Em]@E@@D�f@D>B@C�m@C��@Cs@C\)@CH�@Bں@B�+@B^5@B0U@A��@A�@A�N@A�'@A��@A�M@A�@Aw2@A`B@A5�@@�5@@��@@�u@@6@?�&@?��@?A�@?�@? i@>��@>�y@>��@>҉@>�R@>M�@=�)@=��@=��@=w2@=`B@=J�@=0�@=%F@=�@<ی@<9X@;�@;�:@;Y@:�]@:�R@:h
@:3�@9��@9`B@8��@7�6@7��@7iD@7K�@76z@7'�@7@7
=@7 i@6��@6@5��@5�@5A @5;@4�@4�@4y>@3�}@3]�@2� @2Q@2_@1�3@1��@1�S@1|@1`B@1=�@1�@0�p@0Ĝ@0��@0S�@0�@/y�@/8@/�@.�@.��@.��@.O@-�j@-��@-O�@,ی@,�O@,��@,��@,Q�@+�A@+��@+t�@+g�@+C�@+"�@*�8@*��@*�\@*��@*{�@*q�@*d�@*Ov@*?@*5?@)�@)c@)8�@)	l@(ѷ@(H@'�k@'t�@'&@&�8@&�c@&�@&͟@&�}@&�+@&B[@%��@%S&@$�@$�4@$j@$1'@$,=@$�@#��@#�@#a@"�@"� @"Ov@!�j@!�7@!Vm@!B�@!0�@!�@ �K@ ֡@ ��@ ~(@ r�@ l"@ �@�@n/@��@��@5?@J@�@e,@��@Ft@'R@7@7@b@�@��@��@�@x@RT@Mj@C�@@ں@��@�<@�!@��@i�@@�@�3@��@�~@�M@-w@��@�|@Ɇ@Ĝ@��@��@�@_@Z@b@�a@��@�@@\)@�@�c@��@�A@:*@3�@e@��@��@k�@V@�@��@�@�o@w�@m�@<�@��@�@�@��@��@@O@͟@��@v�@.�@�@�-@�X@�X@��@��@rG@\�@[W@^�@5�@@	l@��@�K@�/@�p@��@�@��@�.@��@��@�o@V�@9X@�g@x@iD@Y@�@�@�r@xl@:*@�>@��@��@��@x�@s�@o @k�@c�@hs@a�@X@S&@+@�4@z�@]d@,=@�@�@b@�@�@��@�q@��@��@��@{J@H�@
��@
�s@
��@
�A@
;�@
 �@	�@	��@	�C@	�~@	u�@	rG@	^�@	L�@	F@	7L@	@�@�v@֡@��@7�@G@��@��@�K@�q@��@�4@a@)_@�h@�@}V@Ta@�@e@@�@�@��@e,@8�@@��@�e@j@�@��@��@��@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��BAϿAϮ�Aϊ�A�MA��A��fA��mA��/A��EA�֡Aο�A�S�A��OA��A�CA��A�l�A�L�A�e�A��A���A�(�A��A��-A�� A���A��A�}�A���A��A�y�A�OA�)*A�یA�d�A�A��?A��<A�t�A���A���A���A��A��CA���A�W?A���A� \A��MA��8A�PHA��A��A�m)A���A�3�A��A�|�A���A�FA�y>A�~]A��MA�+kA�b�A�)_A�P}A�.IA��A�+A��}A�,A���A��A�d&A��5A�UgA��A�1A�&LA�}�A���A�oiA�}"A�PA��VA��A��A~��A{m]Az�KAy�Aw��At;dAs+kAra�Aq��Ao��An��An!�Am�Ak��Aj�AjSAi��Af�2AeE�Ad��Ab��AaS&A`n/A_Y�A^A�A]u�A\�qAZ�<AY��AXq�AX	lAW�AW�MAW��AW�DAXuAW��AW�AV,=AU��AUZ�AU	AT�AR1�AQ�tAP�\AO:�AN�KAN\�AL�&AK҉AJ}�AIqvAH��AG�BAG1'AE8�AC�8AC�AB^�AAl�AA1�A@��A@��A@��A@bNA?@OA>��A=�A=YA<�'A<U2A;�&A:C�A9"hA8rGA7�zA6�A55�A4�kA3�A2֡A1��A1	A0j�A/��A.�>A.��A.��A.��A.U�A-҉A-�-A,�mA+�bA*��A*VmA)��A)_pA(��A'ߤA'IRA& �A$}�A#XA"T�A!�eA!�A ��A U2A��A�`A�A��A{�A4AaA[�A�FA6�A��A�A�*Ap;A�A�/A~�A+A�\Ag8A?AT�A��A\)Ah�A�.Ar�A�A�A�HA��A
�A
�HA
�FA
y�A	��A	%A�fAA|�A�A�XA�QA��A��A�A[�AAE9A >B@���@�{@���@���@���@�$@���@���@��h@�zx@��@�q�@��@�o@�|�@�	@�҉@�@��a@ޞ@��@ݫ�@�Z�@ܿ�@�2�@�|�@�˒@�H�@�{�@ѷ@���@�@�X�@λ�@�@��@�PH@��@�4@��@��s@�˒@�c�@�($@�IR@��,@�6@�J#@���@�l"@�#:@��@�\)@��0@�L0@�+@�h�@�J�@�(�@�U�@�1�@���@��@��f@��}@�$�@�o�@�"�@� i@�ѷ@��L@���@���@��@��~@���@�=q@�m]@��X@��u@�V�@�@���@��@�8�@��@���@��B@�2a@�T�@��@��+@�1�@��M@�h�@��M@�j�@��@��@��O@�m�@�!�@��@���@���@��@���@���@��$@���@�(@��@�)_@��	@�e,@��@���@�xl@�"h@��Q@���@�_�@�o @�S�@��@��[@�tT@�L0@��@�e�@��!@�A�@���@�	l@���@��@�\�@� �@���@���@��Q@��K@���@���@��~@��{@�=@��@�q�@�-@�9X@�/�@���@��~@�W?@��"@��/@��o@���@��@�`�@���@�{J@�=�@�H�@�(@���@�=q@���@���@��4@�J�@�?}@��v@�Ov@��o@���@��Z@���@���@�ԕ@���@��@�+@�2�@���@��@��#@� �@�)�@�0U@���@�+@�S�@�$@�@~�@iD@1�@E9@4�@Y@~��@}�3@|�P@|��@|@{��@{>�@{4�@z�x@zOv@zO@yԕ@yϫ@y�C@y��@y��@y�h@yo @yF@y&�@x�@x�@xQ�@x/�@w��@w+@w�@v�r@uDg@tXy@tx@s�
@s�@s�@s��@s��@r�@q�j@qe,@q�@q;@p�p@pbN@pH@o��@oqv@o=@o'�@oC@n��@nff@m�3@m��@m[W@m=�@m&�@l��@lH@l1@kt�@k�@j~�@i��@i&�@hr�@hu�@h?�@g!-@fz@f#:@f�@e�D@e��@eԕ@e�H@eS&@dĜ@d1'@c�@c��@c��@c��@cA�@b�@b�m@b��@b-@a�z@a/@`w�@`H@`<�@`/�@`�@`�@_��@_��@_�:@_\)@_1�@^�8@^�s@^�'@^�L@^{�@^^5@^)�@]�@]�n@]�M@]f�@]8�@\�v@\H@[��@[�k@[iD@[$t@Z�y@Zȴ@Z��@Z8�@Z{@Y�o@Y�@X�$@W��@W�	@Wqv@Wb�@W1�@W(@V��@Vn�@Uԕ@Uc�@U*0@UV@U%@Toi@T(�@S��@S��@S]�@S@O@S&@R�c@R��@R&�@Q�#@Q��@Q`B@Q^�@Q<6@P�p@Ph�@P<�@P  @O��@O1�@N��@M�Z@M�M@M\�@L�|@L�j@L�@LS�@L1'@Lx@K� @K|�@K;d@J��@J��@Jd�@J�@I��@Ix�@I�@H��@Hy>@G�A@G��@G��@Gv`@GY@Fn�@Fa|@F\�@F^5@FYK@FGE@F_@Em]@E@@D�f@D>B@C�m@C��@Cs@C\)@CH�@Bں@B�+@B^5@B0U@A��@A�@A�N@A�'@A��@A�M@A�@Aw2@A`B@A5�@@�5@@��@@�u@@6@?�&@?��@?A�@?�@? i@>��@>�y@>��@>҉@>�R@>M�@=�)@=��@=��@=w2@=`B@=J�@=0�@=%F@=�@<ی@<9X@;�@;�:@;Y@:�]@:�R@:h
@:3�@9��@9`B@8��@7�6@7��@7iD@7K�@76z@7'�@7@7
=@7 i@6��@6@5��@5�@5A @5;@4�@4�@4y>@3�}@3]�@2� @2Q@2_@1�3@1��@1�S@1|@1`B@1=�@1�@0�p@0Ĝ@0��@0S�@0�@/y�@/8@/�@.�@.��@.��@.O@-�j@-��@-O�@,ی@,�O@,��@,��@,Q�@+�A@+��@+t�@+g�@+C�@+"�@*�8@*��@*�\@*��@*{�@*q�@*d�@*Ov@*?@*5?@)�@)c@)8�@)	l@(ѷ@(H@'�k@'t�@'&@&�8@&�c@&�@&͟@&�}@&�+@&B[@%��@%S&@$�@$�4@$j@$1'@$,=@$�@#��@#�@#a@"�@"� @"Ov@!�j@!�7@!Vm@!B�@!0�@!�@ �K@ ֡@ ��@ ~(@ r�@ l"@ �@�@n/@��@��@5?@J@�@e,@��@Ft@'R@7@7@b@�@��@��@�@x@RT@Mj@C�@@ں@��@�<@�!@��@i�@@�@�3@��@�~@�M@-w@��@�|@Ɇ@Ĝ@��@��@�@_@Z@b@�a@��@�@@\)@�@�c@��@�A@:*@3�@e@��@��@k�@V@�@��@�@�o@w�@m�@<�@��@�@�@��@��@@O@͟@��@v�@.�@�@�-@�X@�X@��@��@rG@\�@[W@^�@5�@@	l@��@�K@�/@�p@��@�@��@�.@��@��@�o@V�@9X@�g@x@iD@Y@�@�@�r@xl@:*@�>@��@��@��@x�@s�@o @k�@c�@hs@a�@X@S&@+@�4@z�@]d@,=@�@�@b@�@�@��@�q@��@��@��@{J@H�@
��@
�s@
��@
�A@
;�@
 �@	�@	��@	�C@	�~@	u�@	rG@	^�@	L�@	F@	7L@	@�@�v@֡@��@7�@G@��@��@�K@�q@��@�4@a@)_@�h@�@}V@Ta@�@e@@�@�@��@e,@8�@@��@�e@j@�@��@��@��@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BZB[	B[=BZ�BZkBYKBX�BX�BX�BX�BX_BVBE�BBԯB�gB�qB��B��B�EB�vB�LB�B��B��B��B��B��BqvB[qB1�B�B�BB(>B#TB 'B�B�B�hB�QB�aB�uB�B��B��B�kB�B�eB��B�_B��B�B��B�ZB�WB�uB�	B|6Bi�B`�BT�BJ#B5�B,qBB�B��B�;B�B�)B�{BǮB��B�GB�B��B��B�UBy>Bh>B_�BXBR�BNpB=<B1�B)_BQB	lB
�B
�XB
�B
�KB
�B
��B
��B
�oB
�B
�\B
��B
��B
��B
��B
��B
��B
xB
k�B
gRB
b4B
V�B
R�B
P�B
M�B
I�B
G�B
?�B
:�B
8�B
4�B
3hB
4B
33B
4B
2�B
1�B
/�B
+B
'mB
%�B
$&B
�B
�B
�B
�B
	7B
�B
�B	�B	��B	��B	��B	��B	��B	�B	�
B	�oB	�DB	��B	��B	�aB	ªB	��B	��B	��B	�B	�2B	��B	�AB	��B	�cB	�qB	�sB	�hB	��B	��B	��B	�B	��B	��B	�zB	�B	�OB	~�B	}B	{B	zxB	y�B	y	B	x�B	wfB	uB	s�B	o�B	j�B	g�B	f2B	dZB	a�B	^B	\�B	WYB	R�B	MB	H�B	G�B	CGB	AoB	?�B	=VB	72B	3MB	1�B	0�B	/�B	-�B	)_B	&�B	$�B	"�B	 �B	!B	B	�B	�B	kB	�B	�B	�B	�B	
XB	�B	B	B	{B	�B	 iB�]B��B�PB�	B��B��B�B�tB�GB�AB�B�=B�kB�0B�*B�B�2B�FB��B��B�B��B��B�dB��B�WB�7B��B��B֡B�aBԯB�B�B��B�B��BʌB�RB�1B�	B�B�fB��B��BǔB��B��B�mBɺBƨB��B�B��B�+B�B�+BƎB��B�_B��B�_B�7BȴBɆB��B˒B�^B�0B�jB̈́B͹B͹B͟B�jB͟B�PB��B��BΊBΊB��BϫB��B�}B҉B�FB��BյB��B�MB��B�9B�+B�7BڠB��B�-B��B�B�B�B�B�B��B�tB�B�`B��B��B�'B�B��B�FB�`B�dB	MB	+B	fB		B	�B	TB	�B	MB	
B	�B	qB	qB	�B	 �B	$ZB	(�B	/B	1�B	2aB	5�B	:DB	=B	>�B	@iB	B'B	C{B	DgB	LdB	N�B	O�B	O�B	PB	O�B	O�B	OBB	Q4B	T�B	Y1B	]�B	d�B	g�B	k�B	m�B	r-B	v�B	y>B	y�B	}�B	��B	��B	��B	��B	��B	�lB	�0B	�B	��B	�aB	��B	��B	��B	��B	�\B	� B	��B	��B	�DB	��B	�5B	��B	��B	�>B	��B	�VB	��B	��B	B	ÖB	��B	�tB	��B	��B	�,B	�mB	׍B	�EB	چB	��B	�:B	��B	�HB	�B	�B	�wB	� B	�B	�nB	�zB	�B	��B	�ZB	�B	�qB	�HB
�B
�B
�B
SB
�B
zB

rB
xB
�B
�B
B
�B
B
�B
�B
9B

B
�B
�B
1B
�B
�B
WB
B
�B
;B
 �B
!bB
"�B
%`B
&fB
'�B
+�B
.IB
.�B
/�B
0!B
0�B
0�B
2B
6+B
;B
=qB
?}B
?�B
A�B
FB
F�B
G�B
J�B
L�B
L�B
L�B
M�B
O�B
RB
R�B
S[B
S�B
S�B
VSB
WYB
X+B
Z7B
[qB
]~B
_B
aHB
b�B
c B
d@B
g8B
h�B
iDB
i�B
i�B
i�B
j�B
j�B
l=B
m�B
o�B
qB
q[B
r�B
s�B
t�B
v+B
vzB
wB
xlB
y�B
|B
~�B
.B
.B
cB
�B
�B
��B
��B
�'B
�-B
��B
��B
��B
��B
�B
��B
�B
��B
��B
��B
��B
�XB
�)B
�B
��B
�B
��B
��B
��B
�:B
�oB
�uB
�aB
��B
�B
��B
��B
�qB
�B
�]B
��B
�/B
��B
�OB
�!B
��B
��B
��B
�B
�:B
�`B
��B
��B
�mB
��B
�*B
�yB
��B
��B
��B
��B
��B
��B
��B
�cB
��B
��B
��B
��B
�aB
��B
�B
��B
��B
��B
��B
��B
�DB
��B
��B
��B
��B
��B
�qB
�wB
��B
��B
�4B
��B
��B
��B
�{B
��B
�9B
ŢB
�YB
�YB
�EB
��B
ȴB
��B
��B
��B
��B
�lB
��B
�xB
�xB
�jB
�B
�pB
�B
�B
�B
�B
бB
��B
�hB
� B
� B
�:B
ңB
ҽB
��B
��B
�B
�@B
ӏB
�B
ԯB
ԯB
�gB
�B
ּB
׍B
��B
��B
��B
�+B
�+B
�_B
�_B
�KB
�B
چB
ںB
��B
�#B
�=B
یB
یB
ۦB
��B
�IB
�B
�jB
�pB
߾B
�B
�B
��B
�B
�hB
�B
�`B
��B
��B
�2B
�fB
�B
�B
�B
�B
�RB
��B
�DB
�B
�B
�B
�B
�6B
�B
�B
�)B
�B
��B
�B
�B
�UB
�UB
��B
�B
�B
�vB
�B
��B
��B
�|B
��B
�B
�nB
�B
�B
�?B
�tB
�+B
�zB
��B
�fB
�B
�lB
��B
��B
��B
��B
�*B
�^B
�xB
��B
��B
�0B
��B
�B
�6B
�6B
�PB
�jB
��B
��B
��B
�"B
��B
�BB
�wB
��B
��B �BB�B�B�BB�BAB[B�BGBMBBB�B�B�B�B�B�B�B�B�BKB	7B	�B	�B
	B
#B
XB
�B
�B
�BDBDB^B�B0B~B�BB�B�B�BpB�BbBbB}B}B�B�B�B�B�B�B�B�B�B B:BTBoB�B[B�B�B,BaB,BBaB{B�B�BMBgB�BB9BSB�B$BYBYBsB�B�BB_B�B�B�B�BKBBB�BQB�B=BWBWB�BCB�BB/BIB�BdB~BIBdB�BBBBB5BOBjBjBjB�B�BBB;BVBVB�B�B�B�B�B�B B BB \B!HB!�B!�B"NB"�B"�B#B#:B#�B$&B$ZB$�B$�B$�B$�B$�B$�B%,B%B%FB%`B%FB%�B%�B&2B&LB&�B&�B&�B&�B&�B'B'�B'�B'�B'�B'�B'�B($B(sB(�B(�B)*B)�B*KB*�B*�B*�B*�B*�B*�B*�B*�B+B+B+kB+�B+�B+�B,qB,�B-CB-wB-]B-�B-�B-�B-�B-�B.cB/B/OB/OB/�B0B0B0B0B0;B0�B0�B1B1'B1[B1AB1�B2�B2�B2�B2�B2�B2�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444BZB[	B[=BZ�BZkBYKBX�BX�BX�BX�BX_BVBE�BBԯB�gB�qB��B��B�EB�vB�LB�B��B��B��B��B��BqvB[qB1�B�B�BB(>B#TB 'B�B�B�hB�QB�aB�uB�B��B��B�kB�B�eB��B�_B��B�B��B�ZB�WB�uB�	B|6Bi�B`�BT�BJ#B5�B,qBB�B��B�;B�B�)B�{BǮB��B�GB�B��B��B�UBy>Bh>B_�BXBR�BNpB=<B1�B)_BQB	lB
�B
�XB
�B
�KB
�B
��B
��B
�oB
�B
�\B
��B
��B
��B
��B
��B
��B
xB
k�B
gRB
b4B
V�B
R�B
P�B
M�B
I�B
G�B
?�B
:�B
8�B
4�B
3hB
4B
33B
4B
2�B
1�B
/�B
+B
'mB
%�B
$&B
�B
�B
�B
�B
	7B
�B
�B	�B	��B	��B	��B	��B	��B	�B	�
B	�oB	�DB	��B	��B	�aB	ªB	��B	��B	��B	�B	�2B	��B	�AB	��B	�cB	�qB	�sB	�hB	��B	��B	��B	�B	��B	��B	�zB	�B	�OB	~�B	}B	{B	zxB	y�B	y	B	x�B	wfB	uB	s�B	o�B	j�B	g�B	f2B	dZB	a�B	^B	\�B	WYB	R�B	MB	H�B	G�B	CGB	AoB	?�B	=VB	72B	3MB	1�B	0�B	/�B	-�B	)_B	&�B	$�B	"�B	 �B	!B	B	�B	�B	kB	�B	�B	�B	�B	
XB	�B	B	B	{B	�B	 iB�]B��B�PB�	B��B��B�B�tB�GB�AB�B�=B�kB�0B�*B�B�2B�FB��B��B�B��B��B�dB��B�WB�7B��B��B֡B�aBԯB�B�B��B�B��BʌB�RB�1B�	B�B�fB��B��BǔB��B��B�mBɺBƨB��B�B��B�+B�B�+BƎB��B�_B��B�_B�7BȴBɆB��B˒B�^B�0B�jB̈́B͹B͹B͟B�jB͟B�PB��B��BΊBΊB��BϫB��B�}B҉B�FB��BյB��B�MB��B�9B�+B�7BڠB��B�-B��B�B�B�B�B�B��B�tB�B�`B��B��B�'B�B��B�FB�`B�dB	MB	+B	fB		B	�B	TB	�B	MB	
B	�B	qB	qB	�B	 �B	$ZB	(�B	/B	1�B	2aB	5�B	:DB	=B	>�B	@iB	B'B	C{B	DgB	LdB	N�B	O�B	O�B	PB	O�B	O�B	OBB	Q4B	T�B	Y1B	]�B	d�B	g�B	k�B	m�B	r-B	v�B	y>B	y�B	}�B	��B	��B	��B	��B	��B	�lB	�0B	�B	��B	�aB	��B	��B	��B	��B	�\B	� B	��B	��B	�DB	��B	�5B	��B	��B	�>B	��B	�VB	��B	��B	B	ÖB	��B	�tB	��B	��B	�,B	�mB	׍B	�EB	چB	��B	�:B	��B	�HB	�B	�B	�wB	� B	�B	�nB	�zB	�B	��B	�ZB	�B	�qB	�HB
�B
�B
�B
SB
�B
zB

rB
xB
�B
�B
B
�B
B
�B
�B
9B

B
�B
�B
1B
�B
�B
WB
B
�B
;B
 �B
!bB
"�B
%`B
&fB
'�B
+�B
.IB
.�B
/�B
0!B
0�B
0�B
2B
6+B
;B
=qB
?}B
?�B
A�B
FB
F�B
G�B
J�B
L�B
L�B
L�B
M�B
O�B
RB
R�B
S[B
S�B
S�B
VSB
WYB
X+B
Z7B
[qB
]~B
_B
aHB
b�B
c B
d@B
g8B
h�B
iDB
i�B
i�B
i�B
j�B
j�B
l=B
m�B
o�B
qB
q[B
r�B
s�B
t�B
v+B
vzB
wB
xlB
y�B
|B
~�B
.B
.B
cB
�B
�B
��B
��B
�'B
�-B
��B
��B
��B
��B
�B
��B
�B
��B
��B
��B
��B
�XB
�)B
�B
��B
�B
��B
��B
��B
�:B
�oB
�uB
�aB
��B
�B
��B
��B
�qB
�B
�]B
��B
�/B
��B
�OB
�!B
��B
��B
��B
�B
�:B
�`B
��B
��B
�mB
��B
�*B
�yB
��B
��B
��B
��B
��B
��B
��B
�cB
��B
��B
��B
��B
�aB
��B
�B
��B
��B
��B
��B
��B
�DB
��B
��B
��B
��B
��B
�qB
�wB
��B
��B
�4B
��B
��B
��B
�{B
��B
�9B
ŢB
�YB
�YB
�EB
��B
ȴB
��B
��B
��B
��B
�lB
��B
�xB
�xB
�jB
�B
�pB
�B
�B
�B
�B
бB
��B
�hB
� B
� B
�:B
ңB
ҽB
��B
��B
�B
�@B
ӏB
�B
ԯB
ԯB
�gB
�B
ּB
׍B
��B
��B
��B
�+B
�+B
�_B
�_B
�KB
�B
چB
ںB
��B
�#B
�=B
یB
یB
ۦB
��B
�IB
�B
�jB
�pB
߾B
�B
�B
��B
�B
�hB
�B
�`B
��B
��B
�2B
�fB
�B
�B
�B
�B
�RB
��B
�DB
�B
�B
�B
�B
�6B
�B
�B
�)B
�B
��B
�B
�B
�UB
�UB
��B
�B
�B
�vB
�B
��B
��B
�|B
��B
�B
�nB
�B
�B
�?B
�tB
�+B
�zB
��B
�fB
�B
�lB
��B
��B
��B
��B
�*B
�^B
�xB
��B
��B
�0B
��B
�B
�6B
�6B
�PB
�jB
��B
��B
��B
�"B
��B
�BB
�wB
��B
��B �BB�B�B�BB�BAB[B�BGBMBBB�B�B�B�B�B�B�B�B�BKB	7B	�B	�B
	B
#B
XB
�B
�B
�BDBDB^B�B0B~B�BB�B�B�BpB�BbBbB}B}B�B�B�B�B�B�B�B�B�B B:BTBoB�B[B�B�B,BaB,BBaB{B�B�BMBgB�BB9BSB�B$BYBYBsB�B�BB_B�B�B�B�BKBBB�BQB�B=BWBWB�BCB�BB/BIB�BdB~BIBdB�BBBBB5BOBjBjBjB�B�BBB;BVBVB�B�B�B�B�B�B B BB \B!HB!�B!�B"NB"�B"�B#B#:B#�B$&B$ZB$�B$�B$�B$�B$�B$�B%,B%B%FB%`B%FB%�B%�B&2B&LB&�B&�B&�B&�B&�B'B'�B'�B'�B'�B'�B'�B($B(sB(�B(�B)*B)�B*KB*�B*�B*�B*�B*�B*�B*�B*�B+B+B+kB+�B+�B+�B,qB,�B-CB-wB-]B-�B-�B-�B-�B-�B.cB/B/OB/OB/�B0B0B0B0B0;B0�B0�B1B1'B1[B1AB1�B2�B2�B2�B2�B2�B2�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230721034754  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230721034759  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230721034800  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230721034800                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230721034801  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230721034801  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20230721040046                      G�O�G�O�G�O�                