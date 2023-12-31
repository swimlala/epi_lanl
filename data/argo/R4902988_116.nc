CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-12-14T00:55:37Z creation;2022-12-14T00:55:38Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20221214005537  20221214011054  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               tA   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @�3�hK�1   @�3�>2�@;���+�d���l�1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A!��AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BO��BW��B`  Bh  Bp  BxffB�  B���B�  B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�33B�33B�33B�  B�  B�  B�33B���B�  B���B�  B�  B�  B�  B�  B���B�  B�  C   C  C�C  C  C
  C  C  C  C  C  C  C  C�C�C�C �C"  C$  C&  C(  C*  C+�fC.  C0  C1�fC3�fC6  C8  C:  C<  C>  C?�fCB�CD  CF�CH  CJ  CL  CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  D   D �fD  D� D  D� D  D� D  D� D  Dy�D  Dy�D  D� D  D� D��D	y�D	��D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D��Dy�D��Dy�D  Dy�D  D� D  D� D  Dy�D  D�fD  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'y�D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D7��D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DC��DD� DE  DE� DF  DF� DG  DG� DH  DH� DIfDI� DJ  DJy�DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DVfDV�fDWfDW� DX  DX� DY  DY� DZ  DZ� D[  D[� D[��D\� D]fD]� D^  D^� D_  D_� D_��D`� Da  Day�Da��Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� DsfDs�fDt  Dt� Du  Du� Du��Dv� Dw  Dw� DxfDx�fDy  Dy� Dz  Dz� D{  D{� D|  D|y�D|��D}� D~fD~� D  D� D�  D�@ D�� D�� D�  D�@ D��3D�� D���D�<�D�|�D�� D�  D�@ D�� D�� D���D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D��3D��3D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�|�D���D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�|�D�� D�  D�@ D D�� D���D�<�DÀ Dü�D���D�<�DĀ D�� D�  D�@ Dŀ D�� D�  D�C3Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D��3D�  D�<�DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D׼�D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۃ3D�� D���D�@ D܃3D�� D���D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�C3D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�3D�@ D� D�� D�  D�@ D�3D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D��3D�  D�@ D��3D��3D�  D�@ D� D��D�  D�@ D� D��D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@  @|��@�ff@�ffA ��A@��A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B@33BG��BOfgBWfgB_��Bg��Bo��Bx33B��B��3B��fB��B��fB��fB��fB��fB��3B��fB��fB��fB��fB��fB��B��B��B��fB��fB��fB��Bӳ3B��fB۳3B��fB��fB��fB��fB��fB�3B��fB��fB��fC�3C�C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�C�C�C �C!�3C#�3C%�3C'�3C)�3C+ٙC-�3C/�3C1ٙC3ٙC5�3C7�3C9�3C;�3C=�3C?ٙCB�CC�3CF�CG�3CI�3CK�3CN�CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C�gC���C���C���C�gC���C���C���C���C���C���C���C�gC���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���D �3D ��D|�D��D|�D��D|�D��D|�D��DvgD��DvgD��D|�D��D|�D�gD	vgD	�gD
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D�3D3D|�D��D|�D�gDvgD�gDvgD��DvgD��D|�D��D|�D��DvgD��D�3D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'vgD'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7�gD8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC�gDD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DI3DI|�DI��DJvgDJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DV3DV�3DW3DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[�gD\|�D]3D]|�D]��D^|�D^��D_|�D_�gD`|�D`��DavgDa�gDb|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Ds3Ds�3Ds��Dt|�Dt��Du|�Du�gDv|�Dv��Dw|�Dx3Dx�3Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D|vgD|�gD}|�D~3D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD���D��fD��3D�;3D�{3D��fD��fD�>fD�~fD��fD��3D�;3D�{3D��fD��fD�>fD�~fD��fD��fD�>fD�{3D��fD��fD�>fD�~fD��fD��fD�A�D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��3D��fD�>fD���D��fD��fD�>fD�~fD��fD��fD�A�D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��D�>fD�~fD��fD��fD�>fD�~fD��3D��fD�>fD�~fD��fD��fD�>fD���D���D��D�A�D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�A�D���D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�{3D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�>fD�{3D��3D��3D�;3D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�{3D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�A�D�~fD��fD��fD�>fD�~fD���D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�{3D��fD��fD�>fD�~fD��fD��3D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�{3D��fD��fD�>fD�{3D��fD��fD�>fD�~fD¾fD��3D�;3D�~fDû3D��3D�;3D�~fDľfD��fD�>fD�~fDžfD��fD�A�D�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fD���D��fD�;3D�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׻3D��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fDہ�D۾fD��3D�>fD܁�DܾfD��3D�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��3D�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��D�A�D�~fD�fD��fD�>fD�~fD�fD��fD�>fD�{3D�fD��D�>fD�~fD�fD��fD�>fD遙D���D��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�3D��fD�>fD�~fD���D��fD�>fD���D���D��fD�>fD�~fD�3D��fD�>fD�~fD�3D��fD�>fD�~fD�fD��fD�A�D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD���D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��mA��vA��A��A���A��;A��vA��A���A��AA��A��MA��A���A��ZA���A��	A���A���A��fA��rA�A��A�A��A�YA��A�%A�+A��A�1A�
rA�
rA�DA�DA��A��A�VA�\A��A�.A�4A��A�@A�A�{A��A��A�$A��A��A�`�A�m)A���A�خA�|�A�
rA�7LA���A��A�;0A���A�"�A��CA��;A�6zA���A��`A���A��\A�`�A�{A�~A�ǮA��A��{A�9�A���A�@�A���A���A�C�A��A�A���A��xA��A��uA��A�\]A��A���A�A�*�A��/A��IA���A���A�jKA�0�A��A���A�9�A��A�
=A�s�A�.A��A�6�A�A~U2A}�A{=qAxیAv�9At�DAs�+Ar0�Aq�AnxlAl	AkxAh��AhjAg��Ae�cAe��AdQ�Ac��Ac#�Aa�A`˒A`8�A_�A_(�A^0UA[��AZ��AZ1�AYe�AXrGAW��AV�AUjAUAT�KAT�RAT��AR9�AO��AOi�AO/�AN��AN�AN��AMĜAL�jAK�8AK��AK�AKqAI��AH��AHFAG��AGxlAF�mAD��ACX�AA�A@v�A?�A>F�A=��A=CA<��A<~�A;�fA;\)A:�mA:OA9h
A8�A6OA4��A2�A1Q�A/�vA/��A.3�A-�A,��A+n�A){A'��A&�9A$�XA#��A"�#A"�A �A��A�A�MA��A �A��AiDA��A'RA��A�A�:AJ�A6�A�AoiA/�A��A�5AeA�$A�A��A�AdZA?�AdZAv`A�A�A�A��A��A
�.A
��A
j�A
!A	IRAI�A��AjA�AZ�A�A�hAHA��AZ�A8�A~�@�L�@�Ɇ@��z@�N�@��h@��m@�@��@�X@�4@�֡@��@�@�{@�K�@�_@�C@尊@�5�@�ѷ@���@�C-@�/�@߇�@�h�@��@�@ڷ�@���@�33@��K@�Ɇ@�:�@��P@�W�@ӿH@�-w@�r�@�@�ݘ@ѯ�@Ш�@�@�k�@�tT@��@��@��@�&�@ʝI@��@ɖS@��s@ȃ@�S�@�"h@ǀ4@�U�@��@�V@���@ű[@��
@�K�@�H@��@���@���@���@�_�@� �@�>�@�v�@�� @���@�J�@�1@�� @���@��	@�2a@��)@�/�@�u@��g@��@��a@��@�33@��@��9@�s�@�D�@�ݘ@���@�o @��f@�[W@�_�@��c@�W�@��@���@�\�@�@���@���@�S�@��@��)@�A @���@�ѷ@�[�@��~@� i@���@��)@�=@���@���@�YK@�:�@��@�b�@��@�$�@�-w@�z�@��@���@�A @�(�@�
=@�ѷ@���@�s�@� �@��@�O�@�!�@��2@�a|@���@��7@���@���@�}�@�n/@�x�@�^�@�)�@��z@�g�@��@��}@��>@��N@���@���@��@��f@�|�@�&�@��	@�#:@��@��@��H@�A @�4�@�Ɇ@�W�@�~@��@���@���@��@���@��C@��S@���@��"@��f@���@�w2@�O@�33@�'�@�@��]@��z@�?@�4@��R@�m�@�7@��z@�A�@��1@�c @�=q@�6@�1�@�,=@�*�@�$�@�!@�~@�	@��@�{@��A@���@��f@�b@�˒@��@��@�J�@�1@~�@~6�@}��@}�@}@|�O@|��@|��@|�@{�@@{4�@z�X@z�b@zR�@y��@ym]@y;@x�@x%�@w� @w�@wt�@wX�@wJ#@wS@vl�@u��@uhs@u<6@u�@t��@tw�@t1'@s�r@s�w@s�F@s�K@s��@s"�@r�s@r@qN<@p�U@poi@pK^@p7�@p �@o�f@o�@n�]@n?@m�j@m�"@m-w@m+@m�@l��@l<�@j�8@j}V@j=q@j$�@j �@i�>@i�j@i�@i�z@i�d@i�3@i��@i��@i��@iO�@h4n@f�b@eԕ@eDg@e+�@d��@d�@dg8@c�a@b�@ba|@b!�@a�@`-�@_
=@^u%@^=q@^4@]�T@]�N@]��@]O�@\��@\��@\]d@\2�@[��@Z�r@Y�M@Y?}@Y:�@Y7L@Y2a@Y%F@Xی@XĜ@X��@X��@X��@XXy@X'R@W�]@W�@W�&@W�;@W�}@W��@Wl�@W>�@W�@VYK@Up�@T�P@T��@T`�@T	�@S�*@S�@R��@RC�@Qs�@QV@Q;@P��@P��@P�U@P��@P�$@P�$@P��@P�@O�K@OdZ@O'�@O�@N�c@N�2@N҉@N~�@Nl�@N+k@M�)@M�'@M��@Mzx@M8�@L�[@L7�@K��@K��@K'�@J��@J�+@Ji�@JM�@J@�@J1�@J&�@J �@I�@Im]@Ia�@I2a@H��@Hl"@H�@G��@G��@G��@GiD@Gg�@GO@F��@F.�@E��@Eԕ@E�M@E2a@E+@E�@D�@D��@Dw�@C��@C��@C��@Cj�@CF�@CC@Bߤ@B��@B��@B;�@A��@A��@A�@Ahs@AIR@A \@@�j@@w�@@$@?�@?ƨ@?��@?��@?|�@?_p@?/�@>��@>�@=��@=rG@=q@<�P@<�/@<I�@;��@;W?@;8@;+@;�@:�"@:��@:�\@:c @:@8��@8S�@8N�@8H@8*�@8*�@7�@7��@6��@6�h@6��@6� @6�A@6YK@6-@5�D@5�'@5O�@4��@4(�@3��@3��@3a@3�@3 i@2�B@2�\@2z@2V@2Ov@2($@1��@1��@1�9@1��@1�@1�@1��@1�d@1�z@1��@1�@1��@1zx@1N<@1-w@0��@0�E@0��@0  @/�P@/s@/J#@.��@.��@.�F@.kQ@.V@.?@.3�@-�.@-��@-&�@,��@,֡@,�p@,Ɇ@,�O@,�_@,]d@,Q�@,M@,S�@,A�@,:�@,7�@,�@,�@+ƨ@+b�@+�@*�]@*��@*}V@*c @*@)��@)`B@)?}@)�@(�4@(q@((�@(x@'�+@'��@'�;@'��@'�[@'��@'x@'C�@'+@'o@&��@&�@&1�@&�@&
�@&
�@&_@&	@&J@&�@&
�@%��@%�@%�@%@%rG@%f�@%(�@$2�@$�@$�@$1@#�r@#��@#�;@#��@#�@#�@@#��@#K�@#�@"��@"��@"Q@"=q@"{@!�@!u�@!B�@!*0@!;@ �@ Ĝ@ �@ �@��@|�@�@�B@�b@p;@zx@^�@f�@f�@hs@hs@O�@�M@x@�g@��@�{@>�@�@��@xl@;�@!�@J@�@�3@��@�M@o @5�@�P@Ĝ@�I@*�@��@�@s@@�@��@��@��@xl@�@��@��@zx@B�@5�@!�@��@�I@M@��@�@�Q@��@˒@��@�@�P@X�@ȴ@v�@R�@�@�@�^@�H@��@��@�h@�@e,@IR@:�@!�@�@��@��@��@Ĝ@�e@�e@�@��@y>@PH@ �@��@�4@S�@)_@�@��@q�@_�@Ta@H�@0U@�@�d@|@m]@T�@:�@2a@/@%F@V@�@�@��@�m@��@�P@|�@v`@X�@1�@
��@
�@
�B@
&�@
&�@
+k@	�D@	ϫ@	u�@	m]@	Vm@	N<@	&�@	%F@	�@�@�@��@tT@K^@?�@-�@M@�@�A111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��mA��vA��A��A���A��;A��vA��A���A��AA��A��MA��A���A��ZA���A��	A���A���A��fA��rA�A��A�A��A�YA��A�%A�+A��A�1A�
rA�
rA�DA�DA��A��A�VA�\A��A�.A�4A��A�@A�A�{A��A��A�$A��A��A�`�A�m)A���A�خA�|�A�
rA�7LA���A��A�;0A���A�"�A��CA��;A�6zA���A��`A���A��\A�`�A�{A�~A�ǮA��A��{A�9�A���A�@�A���A���A�C�A��A�A���A��xA��A��uA��A�\]A��A���A�A�*�A��/A��IA���A���A�jKA�0�A��A���A�9�A��A�
=A�s�A�.A��A�6�A�A~U2A}�A{=qAxیAv�9At�DAs�+Ar0�Aq�AnxlAl	AkxAh��AhjAg��Ae�cAe��AdQ�Ac��Ac#�Aa�A`˒A`8�A_�A_(�A^0UA[��AZ��AZ1�AYe�AXrGAW��AV�AUjAUAT�KAT�RAT��AR9�AO��AOi�AO/�AN��AN�AN��AMĜAL�jAK�8AK��AK�AKqAI��AH��AHFAG��AGxlAF�mAD��ACX�AA�A@v�A?�A>F�A=��A=CA<��A<~�A;�fA;\)A:�mA:OA9h
A8�A6OA4��A2�A1Q�A/�vA/��A.3�A-�A,��A+n�A){A'��A&�9A$�XA#��A"�#A"�A �A��A�A�MA��A �A��AiDA��A'RA��A�A�:AJ�A6�A�AoiA/�A��A�5AeA�$A�A��A�AdZA?�AdZAv`A�A�A�A��A��A
�.A
��A
j�A
!A	IRAI�A��AjA�AZ�A�A�hAHA��AZ�A8�A~�@�L�@�Ɇ@��z@�N�@��h@��m@�@��@�X@�4@�֡@��@�@�{@�K�@�_@�C@尊@�5�@�ѷ@���@�C-@�/�@߇�@�h�@��@�@ڷ�@���@�33@��K@�Ɇ@�:�@��P@�W�@ӿH@�-w@�r�@�@�ݘ@ѯ�@Ш�@�@�k�@�tT@��@��@��@�&�@ʝI@��@ɖS@��s@ȃ@�S�@�"h@ǀ4@�U�@��@�V@���@ű[@��
@�K�@�H@��@���@���@���@�_�@� �@�>�@�v�@�� @���@�J�@�1@�� @���@��	@�2a@��)@�/�@�u@��g@��@��a@��@�33@��@��9@�s�@�D�@�ݘ@���@�o @��f@�[W@�_�@��c@�W�@��@���@�\�@�@���@���@�S�@��@��)@�A @���@�ѷ@�[�@��~@� i@���@��)@�=@���@���@�YK@�:�@��@�b�@��@�$�@�-w@�z�@��@���@�A @�(�@�
=@�ѷ@���@�s�@� �@��@�O�@�!�@��2@�a|@���@��7@���@���@�}�@�n/@�x�@�^�@�)�@��z@�g�@��@��}@��>@��N@���@���@��@��f@�|�@�&�@��	@�#:@��@��@��H@�A @�4�@�Ɇ@�W�@�~@��@���@���@��@���@��C@��S@���@��"@��f@���@�w2@�O@�33@�'�@�@��]@��z@�?@�4@��R@�m�@�7@��z@�A�@��1@�c @�=q@�6@�1�@�,=@�*�@�$�@�!@�~@�	@��@�{@��A@���@��f@�b@�˒@��@��@�J�@�1@~�@~6�@}��@}�@}@|�O@|��@|��@|�@{�@@{4�@z�X@z�b@zR�@y��@ym]@y;@x�@x%�@w� @w�@wt�@wX�@wJ#@wS@vl�@u��@uhs@u<6@u�@t��@tw�@t1'@s�r@s�w@s�F@s�K@s��@s"�@r�s@r@qN<@p�U@poi@pK^@p7�@p �@o�f@o�@n�]@n?@m�j@m�"@m-w@m+@m�@l��@l<�@j�8@j}V@j=q@j$�@j �@i�>@i�j@i�@i�z@i�d@i�3@i��@i��@i��@iO�@h4n@f�b@eԕ@eDg@e+�@d��@d�@dg8@c�a@b�@ba|@b!�@a�@`-�@_
=@^u%@^=q@^4@]�T@]�N@]��@]O�@\��@\��@\]d@\2�@[��@Z�r@Y�M@Y?}@Y:�@Y7L@Y2a@Y%F@Xی@XĜ@X��@X��@X��@XXy@X'R@W�]@W�@W�&@W�;@W�}@W��@Wl�@W>�@W�@VYK@Up�@T�P@T��@T`�@T	�@S�*@S�@R��@RC�@Qs�@QV@Q;@P��@P��@P�U@P��@P�$@P�$@P��@P�@O�K@OdZ@O'�@O�@N�c@N�2@N҉@N~�@Nl�@N+k@M�)@M�'@M��@Mzx@M8�@L�[@L7�@K��@K��@K'�@J��@J�+@Ji�@JM�@J@�@J1�@J&�@J �@I�@Im]@Ia�@I2a@H��@Hl"@H�@G��@G��@G��@GiD@Gg�@GO@F��@F.�@E��@Eԕ@E�M@E2a@E+@E�@D�@D��@Dw�@C��@C��@C��@Cj�@CF�@CC@Bߤ@B��@B��@B;�@A��@A��@A�@Ahs@AIR@A \@@�j@@w�@@$@?�@?ƨ@?��@?��@?|�@?_p@?/�@>��@>�@=��@=rG@=q@<�P@<�/@<I�@;��@;W?@;8@;+@;�@:�"@:��@:�\@:c @:@8��@8S�@8N�@8H@8*�@8*�@7�@7��@6��@6�h@6��@6� @6�A@6YK@6-@5�D@5�'@5O�@4��@4(�@3��@3��@3a@3�@3 i@2�B@2�\@2z@2V@2Ov@2($@1��@1��@1�9@1��@1�@1�@1��@1�d@1�z@1��@1�@1��@1zx@1N<@1-w@0��@0�E@0��@0  @/�P@/s@/J#@.��@.��@.�F@.kQ@.V@.?@.3�@-�.@-��@-&�@,��@,֡@,�p@,Ɇ@,�O@,�_@,]d@,Q�@,M@,S�@,A�@,:�@,7�@,�@,�@+ƨ@+b�@+�@*�]@*��@*}V@*c @*@)��@)`B@)?}@)�@(�4@(q@((�@(x@'�+@'��@'�;@'��@'�[@'��@'x@'C�@'+@'o@&��@&�@&1�@&�@&
�@&
�@&_@&	@&J@&�@&
�@%��@%�@%�@%@%rG@%f�@%(�@$2�@$�@$�@$1@#�r@#��@#�;@#��@#�@#�@@#��@#K�@#�@"��@"��@"Q@"=q@"{@!�@!u�@!B�@!*0@!;@ �@ Ĝ@ �@ �@��@|�@�@�B@�b@p;@zx@^�@f�@f�@hs@hs@O�@�M@x@�g@��@�{@>�@�@��@xl@;�@!�@J@�@�3@��@�M@o @5�@�P@Ĝ@�I@*�@��@�@s@@�@��@��@��@xl@�@��@��@zx@B�@5�@!�@��@�I@M@��@�@�Q@��@˒@��@�@�P@X�@ȴ@v�@R�@�@�@�^@�H@��@��@�h@�@e,@IR@:�@!�@�@��@��@��@Ĝ@�e@�e@�@��@y>@PH@ �@��@�4@S�@)_@�@��@q�@_�@Ta@H�@0U@�@�d@|@m]@T�@:�@2a@/@%F@V@�@�@��@�m@��@�P@|�@v`@X�@1�@
��@
�@
�B@
&�@
&�@
+k@	�D@	ϫ@	u�@	m]@	Vm@	N<@	&�@	%F@	�@�@�@��@tT@K^@?�@-�@M@�@�A111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B�pB��B�VB�pB�pB��B��B�VB��B��B�pB��B�VB�"B�<B�"B��B�B�<B�"B�"B��B�<B�"B�"B�<B�VB�<B�pB�pB�VB�<B�VB�pB��B�pB�pB�pB�pB�pB��B��B�0BdBGB��B˒BŢB�B��Bp;BgBd@BU2BL�BG�BE�BD�BC�BC�BD3BCaBB[B=�B4�B�BAB�	B�B�B�B�7BбB��B��B�=B�_BpUBNpB"4B�B��B�B�$B�	B�B��B��B�4B��B�B�SB�vB��B}BtnBk�Bd�B\�BS�B=<B2B+�B$�B_B	�B
��B
�OB
�zB
ܬB
ՁB
��B
�*B
�TB
��B
��B
��B
�?B
�B
�BB
�lB
�+B
��B
zB
u�B
r�B
o�B
lqB
_;B
XEB
T�B
PHB
LJB
G�B
EmB
:xB
:�B
6�B
5�B
5tB
*eB
~B
�B
KB
EB
�B
B
�B
B
	B
�B
�B
-B	��B	�zB	�9B	�'B	�IB	�eB	�B	ٚB	��B	�DB	��B	�UB	�B	�xB	��B	�fB	�TB	�'B	�)B	��B	�B	��B	�B	� B	��B	�9B	}B	y�B	v�B	pB	m�B	j0B	a-B	]�B	Y�B	U�B	O�B	L�B	J#B	F�B	C{B	?�B	=�B	<�B	8lB	5�B	4�B	4�B	2|B	1B	/�B	.�B	-wB	,�B	,�B	+QB	*0B	)�B	&�B	$@B	"4B	!�B	;B	)B	�B	�B	9B	{B	@B	�B	�B	:B	�B	 B	B	�B	�B	�B	)B		�B	�B		�B	�B	mB	�B	�B	�B	'B	 iB��B��B�^B��B�	B�rB��B��B��B�zB�tB��B�B�MB�|B�|B��B��B�3B�B�B�B�B�B��B�B��B�B��B��B�2B�B��B��B�>B��B��B�xB��B��B��B�B�BB�B�cB	 4B	 �B	[B	�B	�B	�B	B	�B	�B	B	fB	�B		�B		�B		�B		�B		�B	�B	�B	vB	B	9B	=B	�B	�B	�B	B	�B	 �B	'�B	+�B	,"B	,�B	-wB	-�B	-�B	.�B	/�B	1[B	1�B	2|B	2�B	2�B	3�B	4nB	5B	5�B	6+B	6�B	7�B	88B	88B	9$B	>�B	GzB	I7B	JXB	J�B	K)B	K)B	K�B	LdB	L�B	MB	M�B	MjB	O�B	U�B	X+B	Y�B	]IB	_�B	bhB	h
B	k�B	l�B	m]B	m�B	m�B	n�B	o�B	p!B	s�B	w�B	|jB	~�B	�OB	�B	�;B	��B	�uB	��B	��B	��B	�mB	��B	�1B	��B	�xB	�B	�"B	��B	�B	�]B	�B	��B	�-B	��B	��B	�0B	��B	�B	��B	�B	��B	��B	��B	�VB	��B	�]B	�B	�4B	ѝB	ѷB	�FB	�yB	چB	߾B	�nB	�B	��B	�B	�*B	�_B	��B	�QB	�B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�;B	�|B	��B	��B	��B	�]B
 4B
'B
%B
0B
�B
�B
�B
�B
(B
(B
\B
�B
�B
�B
�B
HB
hB
B
�B
	B
�B
~B
%,B
+B
+�B
/�B
3�B
5?B
6`B
7�B
88B
8B
8lB
9rB
:�B
<PB
@�B
B[B
D�B
G�B
HKB
I�B
KB
L�B
M�B
M�B
NB
N"B
N"B
N�B
O�B
QNB
Q�B
Q�B
R:B
R�B
S�B
TB
UgB
V9B
V�B
XB
YB
]~B
^B
`BB
a�B
cTB
d&B
d�B
e,B
e`B
g�B
jB
j�B
l�B
m�B
n�B
o�B
o�B
o�B
poB
q�B
u�B
w2B
w�B
x8B
x�B
y	B
y	B
y�B
y�B
y�B
yrB
y�B
zB
y�B
zDB
}"B
��B
�B
�+B
�_B
��B
�7B
��B
�B
�.B
�NB
��B
�uB
�B
�=B
�B
��B
�5B
��B
��B
��B
�\B
��B
��B
�TB
��B
��B
�B
��B
�B
�B
�)B
�)B
�]B
�IB
�}B
��B
��B
�5B
��B
�UB
��B
��B
�B
�'B
�[B
��B
��B
�hB
�B
��B
�2B
�B
�lB
��B
��B
�jB
��B
�4B
��B
�gB
ŢB
��B
�B
�YB
�tB
�YB
�tB
�?B
��B
�_B
��B
�B
��B
�XB
ʦB
ʦB
��B
�xB
�xB
�0B
̘B
�PB
�jB
̈́B
�"B
��B
�HB
� B
�B
�:B
�&B
�@B
өB
��B
��B
�,B
�B
�{B
՛B
��B
��B
�mB
׍B
�_B
��B
�1B
ٴB
ٚB
��B
��B
�B
ۦB
�xB
��B
�B
ݲB
�5B
ބB
ބB
ބB
ޞB
ߤB
��B
��B
�HB
�bB
�B
��B
�B
�B
�:B
��B
�ZB
��B
�,B
�FB
�zB
�B
�B
�B
�B
��B
�sB
��B
��B
��B
�B
�DB
�B
�B
��B
�B
�B
�CB
�]B
�}B
�B
�UB
�B
�B
��B
��B
�B
�B
��B
�aB
�B
�tB
��B
�tB
��B
��B
��B
�zB
��B
��B
��B
�B
�B
�lB
��B
��B
�rB
��B
��B
��B
��B
��B
��B
�VB
�qB
��B
�(B
�]B
�wB
��B
��B
��B
�B
�HB
�.B
�cB
�cB
�cB
�cB
�cB
��B
��B
��B
��B B 4B �B �B �B'B�B�B�B�B�B�B3BMBgB�B�BSBBYBtBtBtB�B�B+BB+B+BEB+B_BzB_B�BfB�B	B	lB	�B	�B
#B
�B)BDB�B0BdB�BBBB6BPBjB�B�BBB<BVB�BvBvBvB�B�B�B�B�B�B�B�B�B�BbB.B�BoB:B:B:BTBTBoB�B�B�B�BB�B�B,B{B{B�BB�B�B�BBmBmB�B�B�B+B�B1B1B�B#B=B#B#B#B	B�B�B/B�B�BIB�B�BOB�B�B�B!B;B�B�B�B�B 'B \B �B �B!|B!�B!�B"NB"�B# B#:B#nB#nB#nB$@B$�B$�B$�B%,B%FB%FB%zB&B&�B'B'B'B'B'B'mB'RB'mB'�B(�B(�B(�B)_B)�B)�B)�B)�B)�B)�B*0B*eB*B*�B*�B*�B+B+B+6B+6B+kB+kB+kB+�B+�B+�B+�B,qB,�B-B-B-�B.B./B./B.cB.cB.cB.�B/ B/�B/�B/�B/�B/�B/�B/�B/�B/�B0;B0�B1vB1�B1�B1�B1�B2GB2aB2�B2�B3B3�B3�B3�B4B49B4�B4�B4�B4�B5?B5B5ZB5tB5�B5�B6`B6`B6zB6�B6�B6�B6�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444B��B��B��B��B��B��B��B��B��B�pB��B�VB�pB�pB��B��B�VB��B��B�pB��B�VB�"B�<B�"B��B�B�<B�"B�"B��B�<B�"B�"B�<B�VB�<B�pB�pB�VB�<B�VB�pB��B�pB�pB�pB�pB�pB��B��B�0BdBGB��B˒BŢB�B��Bp;BgBd@BU2BL�BG�BE�BD�BC�BC�BD3BCaBB[B=�B4�B�BAB�	B�B�B�B�7BбB��B��B�=B�_BpUBNpB"4B�B��B�B�$B�	B�B��B��B�4B��B�B�SB�vB��B}BtnBk�Bd�B\�BS�B=<B2B+�B$�B_B	�B
��B
�OB
�zB
ܬB
ՁB
��B
�*B
�TB
��B
��B
��B
�?B
�B
�BB
�lB
�+B
��B
zB
u�B
r�B
o�B
lqB
_;B
XEB
T�B
PHB
LJB
G�B
EmB
:xB
:�B
6�B
5�B
5tB
*eB
~B
�B
KB
EB
�B
B
�B
B
	B
�B
�B
-B	��B	�zB	�9B	�'B	�IB	�eB	�B	ٚB	��B	�DB	��B	�UB	�B	�xB	��B	�fB	�TB	�'B	�)B	��B	�B	��B	�B	� B	��B	�9B	}B	y�B	v�B	pB	m�B	j0B	a-B	]�B	Y�B	U�B	O�B	L�B	J#B	F�B	C{B	?�B	=�B	<�B	8lB	5�B	4�B	4�B	2|B	1B	/�B	.�B	-wB	,�B	,�B	+QB	*0B	)�B	&�B	$@B	"4B	!�B	;B	)B	�B	�B	9B	{B	@B	�B	�B	:B	�B	 B	B	�B	�B	�B	)B		�B	�B		�B	�B	mB	�B	�B	�B	'B	 iB��B��B�^B��B�	B�rB��B��B��B�zB�tB��B�B�MB�|B�|B��B��B�3B�B�B�B�B�B��B�B��B�B��B��B�2B�B��B��B�>B��B��B�xB��B��B��B�B�BB�B�cB	 4B	 �B	[B	�B	�B	�B	B	�B	�B	B	fB	�B		�B		�B		�B		�B		�B	�B	�B	vB	B	9B	=B	�B	�B	�B	B	�B	 �B	'�B	+�B	,"B	,�B	-wB	-�B	-�B	.�B	/�B	1[B	1�B	2|B	2�B	2�B	3�B	4nB	5B	5�B	6+B	6�B	7�B	88B	88B	9$B	>�B	GzB	I7B	JXB	J�B	K)B	K)B	K�B	LdB	L�B	MB	M�B	MjB	O�B	U�B	X+B	Y�B	]IB	_�B	bhB	h
B	k�B	l�B	m]B	m�B	m�B	n�B	o�B	p!B	s�B	w�B	|jB	~�B	�OB	�B	�;B	��B	�uB	��B	��B	��B	�mB	��B	�1B	��B	�xB	�B	�"B	��B	�B	�]B	�B	��B	�-B	��B	��B	�0B	��B	�B	��B	�B	��B	��B	��B	�VB	��B	�]B	�B	�4B	ѝB	ѷB	�FB	�yB	چB	߾B	�nB	�B	��B	�B	�*B	�_B	��B	�QB	�B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�;B	�|B	��B	��B	��B	�]B
 4B
'B
%B
0B
�B
�B
�B
�B
(B
(B
\B
�B
�B
�B
�B
HB
hB
B
�B
	B
�B
~B
%,B
+B
+�B
/�B
3�B
5?B
6`B
7�B
88B
8B
8lB
9rB
:�B
<PB
@�B
B[B
D�B
G�B
HKB
I�B
KB
L�B
M�B
M�B
NB
N"B
N"B
N�B
O�B
QNB
Q�B
Q�B
R:B
R�B
S�B
TB
UgB
V9B
V�B
XB
YB
]~B
^B
`BB
a�B
cTB
d&B
d�B
e,B
e`B
g�B
jB
j�B
l�B
m�B
n�B
o�B
o�B
o�B
poB
q�B
u�B
w2B
w�B
x8B
x�B
y	B
y	B
y�B
y�B
y�B
yrB
y�B
zB
y�B
zDB
}"B
��B
�B
�+B
�_B
��B
�7B
��B
�B
�.B
�NB
��B
�uB
�B
�=B
�B
��B
�5B
��B
��B
��B
�\B
��B
��B
�TB
��B
��B
�B
��B
�B
�B
�)B
�)B
�]B
�IB
�}B
��B
��B
�5B
��B
�UB
��B
��B
�B
�'B
�[B
��B
��B
�hB
�B
��B
�2B
�B
�lB
��B
��B
�jB
��B
�4B
��B
�gB
ŢB
��B
�B
�YB
�tB
�YB
�tB
�?B
��B
�_B
��B
�B
��B
�XB
ʦB
ʦB
��B
�xB
�xB
�0B
̘B
�PB
�jB
̈́B
�"B
��B
�HB
� B
�B
�:B
�&B
�@B
өB
��B
��B
�,B
�B
�{B
՛B
��B
��B
�mB
׍B
�_B
��B
�1B
ٴB
ٚB
��B
��B
�B
ۦB
�xB
��B
�B
ݲB
�5B
ބB
ބB
ބB
ޞB
ߤB
��B
��B
�HB
�bB
�B
��B
�B
�B
�:B
��B
�ZB
��B
�,B
�FB
�zB
�B
�B
�B
�B
��B
�sB
��B
��B
��B
�B
�DB
�B
�B
��B
�B
�B
�CB
�]B
�}B
�B
�UB
�B
�B
��B
��B
�B
�B
��B
�aB
�B
�tB
��B
�tB
��B
��B
��B
�zB
��B
��B
��B
�B
�B
�lB
��B
��B
�rB
��B
��B
��B
��B
��B
��B
�VB
�qB
��B
�(B
�]B
�wB
��B
��B
��B
�B
�HB
�.B
�cB
�cB
�cB
�cB
�cB
��B
��B
��B
��B B 4B �B �B �B'B�B�B�B�B�B�B3BMBgB�B�BSBBYBtBtBtB�B�B+BB+B+BEB+B_BzB_B�BfB�B	B	lB	�B	�B
#B
�B)BDB�B0BdB�BBBB6BPBjB�B�BBB<BVB�BvBvBvB�B�B�B�B�B�B�B�B�B�BbB.B�BoB:B:B:BTBTBoB�B�B�B�BB�B�B,B{B{B�BB�B�B�BBmBmB�B�B�B+B�B1B1B�B#B=B#B#B#B	B�B�B/B�B�BIB�B�BOB�B�B�B!B;B�B�B�B�B 'B \B �B �B!|B!�B!�B"NB"�B# B#:B#nB#nB#nB$@B$�B$�B$�B%,B%FB%FB%zB&B&�B'B'B'B'B'B'mB'RB'mB'�B(�B(�B(�B)_B)�B)�B)�B)�B)�B)�B*0B*eB*B*�B*�B*�B+B+B+6B+6B+kB+kB+kB+�B+�B+�B+�B,qB,�B-B-B-�B.B./B./B.cB.cB.cB.�B/ B/�B/�B/�B/�B/�B/�B/�B/�B/�B0;B0�B1vB1�B1�B1�B1�B2GB2aB2�B2�B3B3�B3�B3�B4B49B4�B4�B4�B4�B5?B5B5ZB5tB5�B5�B6`B6`B6zB6�B6�B6�B6�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20221214005530  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20221214005537  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221214005537  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221214005538                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221214005538  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221214005538  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20221214011054                      G�O�G�O�G�O�                