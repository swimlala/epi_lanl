CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-07-11T03:49:45Z creation;2023-07-11T03:49:46Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20230711034945  20230711040251  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @�9�m�51   @�9���H@;�$�/�c�+I�1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@���A   A   A@  A`  A�  A�  A�  A�  A�33A�33A�  A���B   B  B  B  B ffB(  B0  B8  B@  BH  BO��BW��B`  Bh  Bp  Bx  B�  B�  B���B���B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�fC  C�fC  C  C  C  C  C  C  C �C"  C$  C&  C(�C*  C+�fC.  C/�fC1�fC4  C6  C8  C:  C;�fC=�fC@  CB  CD  CE�fCH  CI�fCL  CN�CP  CR  CT  CV  CX  CZ�C\�C^  C`�Cb  Cd  Cf  Ch  Cj  Ck�fCn  Cp  Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C��C��C��C�  C�  C�  C�  C�  C��3C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  D fD � D  Dy�D��D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D��D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(fD(� D)  D)� D*  D*� D+  D+�fD,fD,� D-  D-� D.  D.� D/fD/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D;��D<� D=  D=� D>  D>� D?  D?� D@  D@� D@��DA� DB  DB� DC  DC� DD  DD�fDE  DE� DF  DF�fDG  DG� DH  DH� DI  DI� DJfDJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DRfDR� DS  DS� DT  DT� DU  DU� DU��DV� DW  DW� DX  DX� DY  DY� DY��DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`�fDa  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dky�Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq�fDr  Dr� Ds  Dsy�Dt  Dt�fDu  Du� Du��Dv� Dw  Dw� Dx  Dx�fDy  Dy� Dz  Dzy�Dz��D{� D|  D|� D}  D}y�D~  D~� D~��D� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�3D�@ D�|�D���D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D��3D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D���D�<�D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D��3D�  D�@ D��3D�� D�  D�@ D D�� D�  D�@ DÃ3D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�3D�@ Dǀ D�� D�  D�@ DȀ D�� D�3D�@ Dɀ D�� D�  D�@ Dʀ Dʼ�D�  D�@ Dˀ D˼�D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�3D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D���D�<�Dـ D�� D�3D�C3Dڀ D�� D�  D�@ Dۀ D��3D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D޼�D���D�<�D߀ D�� D�  D�<�D�� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D��3D�� D�  D�@ D� D��D�  D�C3D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@�33@�ffA33A?33A_33A33A���A���A���A���A���Aߙ�A�fgA���B��B��B��B 33B'��B/��B7��B?��BG��BOfgBWfgB_��Bg��Bo��Bw��B��B��fB��3B��3B��fB��fB��fB��fB��3B��fB��fB��fB��fB��fB��fB��fB��fBó3B��fB��fB��fBӳ3B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3CٙC�3CٙC�3C�3C�3C�3C�3C�3C�3C �C!�3C#�3C%�3C(�C)�3C+ٙC-�3C/ٙC1ٙC3�3C5�3C7�3C9�3C;ٙC=ٙC?�3CA�3CC�3CEٙCG�3CIٙCK�3CN�CO�3CQ�3CS�3CU�3CW�3CZ�C\�C]�3C`�Ca�3Cc�3Ce�3Cg�3Ci�3CkٙCm�3Co�3Cq�3CsٙCu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C�gC�gC�gC���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C�gC���C���C���C���C���C���C���C�gC���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���D 3D |�D ��DvgD�gD|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��DvgD��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D�3D��D|�D��D|�D��D|�D��D|�D�gD|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D(3D(|�D(��D)|�D)��D*|�D*��D+�3D,3D,|�D,��D-|�D-��D.|�D/3D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;�gD<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@�gDA|�DA��DB|�DB��DC|�DC��DD�3DD��DE|�DE��DF�3DF��DG|�DG��DH|�DH��DI|�DJ3DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DR3DR|�DR��DS|�DS��DT|�DT��DU|�DU�gDV|�DV��DW|�DW��DX|�DX��DY|�DY�gDZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`�3D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��DkvgDk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq�3Dq��Dr|�Dr��DsvgDs��Dt�3Dt��Du|�Du�gDv|�Dv��Dw|�Dw��Dx�3Dx��Dy|�Dy��DzvgDz�gD{|�D{��D||�D|��D}vgD}��D~|�D~�gD|�D��D�>fD�~fD���D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��3D��fD�>fD�~fD��fD��D�>fD�{3D��3D��fD�>fD�{3D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�A�D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD���D��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��fD�>fD���D��fD��fD�>fD�~fD��fD��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��3D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�;3D�~fD��fD��3D�;3D�~fD��3D��3D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD���D��fD��fD�>fD�~fD��fD��fD�>fD���D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD���D��fD��fD�>fD�~fD��fD��3D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�A�D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�{3D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��D�>fD�~fD���D��fD�>fD���D��fD��fD�>fD�~fD¾fD��fD�>fDÁ�DþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��D�>fD�~fDǾfD��fD�>fD�~fDȾfD��D�>fD�~fDɾfD��fD�>fD�~fDʻ3D��fD�>fD�~fD˻3D��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��D�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��3D�;3D�~fDپfD��D�A�D�~fDھfD��fD�>fD�~fD���D��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޻3D��3D�;3D�~fD߾fD��fD�;3D�~fD�fD��D�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�{3D�fD��fD�>fD�~fD�fD��fD�A�D�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�;3D�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD���D�fD��fD�>fD�~fD�3D��fD�A�D�~fD���D��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AʴAʮ�Aʢ�Aʞ�AʔAʋxA�|�A�kA�:�A�(�A�"4A���AƖ�A���A���A�	A�IRA�YKA��zA�C-A�
�A��]A�e�A�>BA�~]A�zxA��bA��A�A���A�*0A��A��2A�tA��A�8RA�"hA�D�A���A�l�A�U2A��aA���A���A�DA��A�z�A�4nA�ҽA�	�A��oA�C�A�<jA�y	A�F�A�;A�xlA��gA� �A�.}A�5�A�cTA��A�ƨA���A�K)A�ӏA�.A���A��A���A�+A�DgA���A� \A�d&A���A�/A��SA��BA�C-A�A	A~��A|�7AzѷAy�Ax �AvJ�Au��AuB�Aq�Aoe,An�OAn#�AmL�Al�Al�rAlW�Ak�Aj�Aiu%Ai/�Ah��Ah^�Ag��Ag�HAg�AAg6zAe~Ab�8Aby�A_�;A\��A[�A[�AZ��AY�4AW��AV($AU6�ATAT�.AT&AS��AS�AQ�EAQxAP��AP��AP8AO1�AL��AL;dAJ�WAI�|AH0�AG��AG�	AF�AE!�AC�tACrGACH�AB��AA_�A@�pA@N<A?�A>�wA=��A=��A=K^A=�A<5?A;+�A:�rA:A A:-wA:SA9ߤA8;dA6zxA6�A5]dA4�DA2�]A2	lA0�~A/�bA.�|A.��A.��A.A,e,A+2aA*��A*��A*J�A)ݘA)y>A)j�A)XA)�A(?A'�A'�hA'��A'��A&�A%��A$|A#TaA!%AیAK^A��AO�A�gA3�A�AA@A4Ac�A�|A��As�AZ�A�A�IA�A�
AY�A�A�[AN<A�AZAS�A��A�.A@�A��A�AȴA�=Aa|AVA�A

=A	��A	�A�AW?A&A�A�	A�mAJ�A��A�A��A�VA[�A��Ao�AV�A$ASA�ZA�@���@���@��@��+@��'@���@�d�@���@��@�o @�?}@�-@�S@�C@�z@���@�]d@�8@�t�@���@���@��Z@�y>@ܧ@�s�@�G@�}�@��@֔F@�6�@��@���@�zx@��@�Xy@Ӳ�@�ff@�'R@�O@��@�-w@�tT@��@�]d@��@�A�@Û=@�tT@��@�hs@�Ov@���@�=@�7L@�o@�c @��@��S@�@���@��~@�?�@�1�@�GE@�Vm@�[�@��@�  @��@��?@���@�_p@��L@��j@��9@���@�z�@��D@��f@�!-@���@�Z@��>@�5�@���@�8�@��@��~@�9X@���@��e@�|@�V�@��@���@�.I@��)@�h
@�� @��S@�u�@��@���@�l"@��a@�-w@���@�*0@�+@���@�@��{@��@�:*@��X@���@���@�|�@�4�@��@��H@���@�\�@�S�@�R�@�]d@�bN@�N�@�#�@�/�@��@��@�7�@�=q@�1�@���@��q@��'@��f@���@�)�@�/�@�	�@���@���@���@���@���@��q@���@���@�9�@��@��U@���@�R�@�{@���@��9@��M@�YK@���@��@��o@�:�@���@��$@�4@��w@��:@�Z�@��2@�}V@�h
@�7@�ݘ@���@���@��z@���@��@�x@���@���@��;@���@��W@��g@�hs@�33@��@�
=@��K@���@��}@���@��.@���@�S&@��@��2@���@�w�@�R�@�&�@��@�{@~�@~?@~($@}��@|֡@|/�@{�@{.I@z�"@{'�@z�@z��@y�@x�f@x�p@xr�@x  @w"�@v�@vz@vc @v)�@vJ@u�@u�@u@u��@uB�@t�e@t�@s;d@r��@rOv@q�@q��@qf�@qq@p��@p��@p��@pA�@o�@o�	@ov`@oRT@n��@nu%@n)�@m�@m��@m�'@mrG@l��@l��@lQ�@kخ@kE9@j�@j��@j��@j�b@j��@jff@i�@i5�@h�)@h�@g��@gF�@f�8@f�@f{�@fe@f&�@e��@e8�@d�`@dw�@de�@dFt@d4n@c�0@b�,@bB[@a�z@a&�@a�@`��@`��@`c�@_e�@_6z@_1�@_"�@^�h@^ �@]�-@]�@]L�@\�`@\oi@\V�@\M@\(�@[˒@[��@[S�@[=@[&@Z�@Z�R@Z��@Zu@Y��@Z@Z$�@YIR@X��@X �@Wqv@V��@V1�@U�@U��@S�@S1�@R��@R�@Q��@Q��@Q��@QT�@Q/@Q�@Q%@P�@P��@P֡@P��@P�U@P�e@Py>@Pj@PU2@P�@Pb@O��@Oƨ@O�f@O=@O�@N�@NZ�@N@�@NO@M��@M�9@M��@M@@L��@Lb@K�w@K��@K��@K\)@J�]@J�<@J�x@J�r@Jp;@J_�@J�@I�3@I�@Is�@I\�@IO�@I0�@I \@I@H�|@H��@H��@H�@HZ@G��@G��@G\)@G+@G�@F�X@Fp;@F?@F�@E�@E�@DtT@D!@C�@C��@C��@C��@Cƨ@C�:@C\)@CW?@CW?@C1�@B�B@B�m@B� @B�@B@B
�@B4@A�T@A�n@AY�@@�@@��@@	�@?�@?�
@?�f@?(@>��@>-@=��@=�'@=�@=Vm@=�@<��@<�5@<�`@<r�@;�@;��@;��@;dZ@;�@:ں@:W�@:R�@:V@:C�@9�@9��@9o @9<6@9�@8��@8��@8��@8V�@7�@7��@7C�@6�"@6h
@5�M@5F@57L@5@@5�@4��@5;@4��@4��@4�@4Z@3�@3�@3��@3Z�@3
=@2��@2��@2��@1��@1��@1��@1��@1��@1}�@17L@0��@0��@0e�@/��@/�@.��@.4@-ϫ@-s�@-N<@-?}@-%F@,��@,�e@,z�@,'R@+�@+��@+l�@+;d@*�@*d�@*�@)�-@)�@)f�@)&�@)�@(�E@(�_@(Xy@(	�@(G@'�A@'خ@'ƨ@'��@'��@'~�@'b�@'A�@&��@&v�@&6�@& �@%��@%&�@%!�@$�E@$|�@$bN@$Ft@$/�@$  @#��@#s@#_p@#U�@#S�@#O@#9�@#"�@#�@"��@"�R@"�@"\�@"4@!j@ ی@ ��@ �9@ ��@ �@ �o@ U2@ *�@ (�@ !@�+@�[@�f@iD@U�@�@�"@�8@�2@��@�L@�@S&@�@��@[�@-�@�@�W@��@v`@t�@l�@RT@6z@�@�@�2@ں@�@�@�x@��@��@�+@xl@i�@W�@:*@��@�H@c@X@#�@@��@ی@m�@g8@`�@U2@�@�@��@)_@�@��@��@��@��@)�@{@@��@hs@/@�9@PH@�@�@�;@��@\)@�@�c@�!@~�@-@��@��@o @%F@�P@ѷ@g8@I�@/�@"h@�@�K@��@�	@U�@+@��@�@��@��@��@��@�3@��@��@��@5�@�|@�z@U2@<�@~@  @��@�K@��@��@y�@>�@�@
��@
�y@
��@
�6@
q�@
#:@	��@	��@	��@	s�@	a�@	2a@�@��@�z@tT@9X@�r@ݘ@�@�@�4@S�@8@4�@.I@Y@��@��@c @M�@.�@	@@�)@�@��@��@e,@-w@��@�$@�I@r�@<�@!@�@G@��@��@�V@~�@j�@b�@P�@A�@'�@�@�@�@C�@C�@8�@+k@($1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AʴAʮ�Aʢ�Aʞ�AʔAʋxA�|�A�kA�:�A�(�A�"4A���AƖ�A���A���A�	A�IRA�YKA��zA�C-A�
�A��]A�e�A�>BA�~]A�zxA��bA��A�A���A�*0A��A��2A�tA��A�8RA�"hA�D�A���A�l�A�U2A��aA���A���A�DA��A�z�A�4nA�ҽA�	�A��oA�C�A�<jA�y	A�F�A�;A�xlA��gA� �A�.}A�5�A�cTA��A�ƨA���A�K)A�ӏA�.A���A��A���A�+A�DgA���A� \A�d&A���A�/A��SA��BA�C-A�A	A~��A|�7AzѷAy�Ax �AvJ�Au��AuB�Aq�Aoe,An�OAn#�AmL�Al�Al�rAlW�Ak�Aj�Aiu%Ai/�Ah��Ah^�Ag��Ag�HAg�AAg6zAe~Ab�8Aby�A_�;A\��A[�A[�AZ��AY�4AW��AV($AU6�ATAT�.AT&AS��AS�AQ�EAQxAP��AP��AP8AO1�AL��AL;dAJ�WAI�|AH0�AG��AG�	AF�AE!�AC�tACrGACH�AB��AA_�A@�pA@N<A?�A>�wA=��A=��A=K^A=�A<5?A;+�A:�rA:A A:-wA:SA9ߤA8;dA6zxA6�A5]dA4�DA2�]A2	lA0�~A/�bA.�|A.��A.��A.A,e,A+2aA*��A*��A*J�A)ݘA)y>A)j�A)XA)�A(?A'�A'�hA'��A'��A&�A%��A$|A#TaA!%AیAK^A��AO�A�gA3�A�AA@A4Ac�A�|A��As�AZ�A�A�IA�A�
AY�A�A�[AN<A�AZAS�A��A�.A@�A��A�AȴA�=Aa|AVA�A

=A	��A	�A�AW?A&A�A�	A�mAJ�A��A�A��A�VA[�A��Ao�AV�A$ASA�ZA�@���@���@��@��+@��'@���@�d�@���@��@�o @�?}@�-@�S@�C@�z@���@�]d@�8@�t�@���@���@��Z@�y>@ܧ@�s�@�G@�}�@��@֔F@�6�@��@���@�zx@��@�Xy@Ӳ�@�ff@�'R@�O@��@�-w@�tT@��@�]d@��@�A�@Û=@�tT@��@�hs@�Ov@���@�=@�7L@�o@�c @��@��S@�@���@��~@�?�@�1�@�GE@�Vm@�[�@��@�  @��@��?@���@�_p@��L@��j@��9@���@�z�@��D@��f@�!-@���@�Z@��>@�5�@���@�8�@��@��~@�9X@���@��e@�|@�V�@��@���@�.I@��)@�h
@�� @��S@�u�@��@���@�l"@��a@�-w@���@�*0@�+@���@�@��{@��@�:*@��X@���@���@�|�@�4�@��@��H@���@�\�@�S�@�R�@�]d@�bN@�N�@�#�@�/�@��@��@�7�@�=q@�1�@���@��q@��'@��f@���@�)�@�/�@�	�@���@���@���@���@���@��q@���@���@�9�@��@��U@���@�R�@�{@���@��9@��M@�YK@���@��@��o@�:�@���@��$@�4@��w@��:@�Z�@��2@�}V@�h
@�7@�ݘ@���@���@��z@���@��@�x@���@���@��;@���@��W@��g@�hs@�33@��@�
=@��K@���@��}@���@��.@���@�S&@��@��2@���@�w�@�R�@�&�@��@�{@~�@~?@~($@}��@|֡@|/�@{�@{.I@z�"@{'�@z�@z��@y�@x�f@x�p@xr�@x  @w"�@v�@vz@vc @v)�@vJ@u�@u�@u@u��@uB�@t�e@t�@s;d@r��@rOv@q�@q��@qf�@qq@p��@p��@p��@pA�@o�@o�	@ov`@oRT@n��@nu%@n)�@m�@m��@m�'@mrG@l��@l��@lQ�@kخ@kE9@j�@j��@j��@j�b@j��@jff@i�@i5�@h�)@h�@g��@gF�@f�8@f�@f{�@fe@f&�@e��@e8�@d�`@dw�@de�@dFt@d4n@c�0@b�,@bB[@a�z@a&�@a�@`��@`��@`c�@_e�@_6z@_1�@_"�@^�h@^ �@]�-@]�@]L�@\�`@\oi@\V�@\M@\(�@[˒@[��@[S�@[=@[&@Z�@Z�R@Z��@Zu@Y��@Z@Z$�@YIR@X��@X �@Wqv@V��@V1�@U�@U��@S�@S1�@R��@R�@Q��@Q��@Q��@QT�@Q/@Q�@Q%@P�@P��@P֡@P��@P�U@P�e@Py>@Pj@PU2@P�@Pb@O��@Oƨ@O�f@O=@O�@N�@NZ�@N@�@NO@M��@M�9@M��@M@@L��@Lb@K�w@K��@K��@K\)@J�]@J�<@J�x@J�r@Jp;@J_�@J�@I�3@I�@Is�@I\�@IO�@I0�@I \@I@H�|@H��@H��@H�@HZ@G��@G��@G\)@G+@G�@F�X@Fp;@F?@F�@E�@E�@DtT@D!@C�@C��@C��@C��@Cƨ@C�:@C\)@CW?@CW?@C1�@B�B@B�m@B� @B�@B@B
�@B4@A�T@A�n@AY�@@�@@��@@	�@?�@?�
@?�f@?(@>��@>-@=��@=�'@=�@=Vm@=�@<��@<�5@<�`@<r�@;�@;��@;��@;dZ@;�@:ں@:W�@:R�@:V@:C�@9�@9��@9o @9<6@9�@8��@8��@8��@8V�@7�@7��@7C�@6�"@6h
@5�M@5F@57L@5@@5�@4��@5;@4��@4��@4�@4Z@3�@3�@3��@3Z�@3
=@2��@2��@2��@1��@1��@1��@1��@1��@1}�@17L@0��@0��@0e�@/��@/�@.��@.4@-ϫ@-s�@-N<@-?}@-%F@,��@,�e@,z�@,'R@+�@+��@+l�@+;d@*�@*d�@*�@)�-@)�@)f�@)&�@)�@(�E@(�_@(Xy@(	�@(G@'�A@'خ@'ƨ@'��@'��@'~�@'b�@'A�@&��@&v�@&6�@& �@%��@%&�@%!�@$�E@$|�@$bN@$Ft@$/�@$  @#��@#s@#_p@#U�@#S�@#O@#9�@#"�@#�@"��@"�R@"�@"\�@"4@!j@ ی@ ��@ �9@ ��@ �@ �o@ U2@ *�@ (�@ !@�+@�[@�f@iD@U�@�@�"@�8@�2@��@�L@�@S&@�@��@[�@-�@�@�W@��@v`@t�@l�@RT@6z@�@�@�2@ں@�@�@�x@��@��@�+@xl@i�@W�@:*@��@�H@c@X@#�@@��@ی@m�@g8@`�@U2@�@�@��@)_@�@��@��@��@��@)�@{@@��@hs@/@�9@PH@�@�@�;@��@\)@�@�c@�!@~�@-@��@��@o @%F@�P@ѷ@g8@I�@/�@"h@�@�K@��@�	@U�@+@��@�@��@��@��@��@�3@��@��@��@5�@�|@�z@U2@<�@~@  @��@�K@��@��@y�@>�@�@
��@
�y@
��@
�6@
q�@
#:@	��@	��@	��@	s�@	a�@	2a@�@��@�z@tT@9X@�r@ݘ@�@�@�4@S�@8@4�@.I@Y@��@��@c @M�@.�@	@@�)@�@��@��@e,@-w@��@�$@�I@r�@<�@!@�@G@��@��@�V@~�@j�@b�@P�@A�@'�@�@�@�@C�@C�@8�@+k@($1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B6�B6�B7B7B7LB7�B7�B8RB9>B9>B8B1�B�B�B�hB�}B��B��Bu�BiDB[�BO(B@ B,�B&2B�B-B��B��B��B��B�dB��B�B�DB��B��B�9B��B��B��B�B�/B��BezBTFBY1Bh�Bh�B]BR:BNBF�B9�B3�B-�B$�B�B�B��B�@BЗB�lB��B�<B�B�B�sB�$B��B�pB�%ByrBm)BWYBK�B4�B*�B!-B]B[B�BKB�B
�^B
�iB
�nB
�;B
��B
�B
�1B
�HB
�]B
�RB
�B
�B
�IB
��B
��B
��B
��B
�7B
��B
�%B
��B
�3B
��B
��B
� B
y�B
jB
gRB
\CB
J�B
D�B
B�B
@ B
<�B
1�B
)�B
"�B
 �B
B
xB
B
gB
}B
DB

rB
	�B
�B
�B	��B	�lB	�B	��B	�B	��B	�B	�HB	�]B	רB	�2B	��B	�TB	�"B	�XB	�_B	�B	��B	�wB	��B	�B	�B	��B	�hB	�;B	��B	��B	��B	�B	��B	�B	��B	�WB	��B	��B	�B	��B	��B	�aB	�UB	�4B	~�B	y�B	s�B	rGB	q�B	o�B	n}B	l�B	lWB	k�B	k6B	i�B	g�B	f�B	e�B	dtB	aHB	\�B	W?B	RoB	J�B	C�B	<B	8�B	6�B	5�B	3�B	0UB	-�B	*�B	&�B	%�B	%zB	$�B	$B	#�B	!|B	�B	�B	�B	WB	7B	�B	�B	FB	�B	
�B		�B	_B	9B	�B	�B	�B	B��B�qB�XB�LB�B�%B�9B��B�hB��B�B�[B�oB��B��B�]B��B��B�kB�B�B�LB�B�LB��B�BݘB��B��B��B�mB��B��B�&B�TB�oBϫB��B��B̘BʌB�	BɆB��B��B��B�SB�BðBªB��BªB��B�AB�AB�'BB��B��B��B��B�B��B��B�iB��B�UB��B�B��B��BB��B�9B��BǮB��BǮB��B��BʦB��B˒B��B�B�\B�HB�hB�oBңB�uBּB��B��B��B�B�]B�dB�'B��B��B�B�mB�B��B�B�B��B�3B��B�B��B�HB	[B	�B	�B	jB	B	FB	�B	�B	
B	�B	eB	�B	�B	/B	�B	!�B	&LB	*�B	-]B	5%B	8�B	=�B	A�B	J	B	MB	PHB	UB	Z�B	\�B	_;B	aHB	cTB	hsB	i�B	j�B	l�B	n}B	n�B	ncB	n�B	o�B	p�B	q�B	s�B	x8B	y>B	y�B	{dB	{B	{�B	{dB	}B	�-B	�B	��B	�EB	�B	��B	��B	�(B	��B	��B	��B	�1B	�kB	��B	��B	�pB	�B	��B	��B	�,B	�
B	�6B	��B	��B	�`B	��B	��B	��B	�}B	�oB	�SB	�zB	�B	ˬB	͹B	� B	�&B	�2B	׍B	ٴB	�]B	�|B	��B	�B	��B	�LB	�sB	�B	�=B	��B	�[B	��B	�|B	�9B	�RB	��B	��B	�(B	�}B
uB
�B
�B
�B
	�B
	�B
	�B
	�B
^B
~B
B
�B
�B
�B
B
B
7B
�B
�B
"�B
(>B
*KB
+B
+�B
,�B
.�B
/5B
/OB
/OB
/OB
/OB
/�B
/�B
/�B
2|B
4�B
6`B
9XB
<�B
=�B
?�B
B�B
D3B
EmB
FB
F?B
FYB
FtB
G_B
HfB
H�B
IRB
JrB
LdB
N"B
O�B
O�B
P�B
QhB
S�B
T{B
T�B
V�B
X_B
YeB
ZQB
Z�B
[=B
[�B
\�B
_;B
b�B
d�B
d�B
g�B
iyB
j0B
jB
j�B
l�B
m�B
o�B
qAB
rGB
tB
tB
tnB
t�B
vFB
yXB
{JB
|�B
HB
�B
�iB
��B
��B
��B
�SB
�mB
�mB
�B
�RB
�#B
��B
�DB
��B
�"B
�pB
�pB
��B
�B
�.B
��B
��B
�:B
��B
�uB
�B
��B
�mB
��B
��B
��B
��B
�#B
�/B
��B
��B
�-B
��B
�B
��B
�KB
��B
��B
��B
�)B
��B
�/B
��B
��B
��B
��B
�5B
�iB
�iB
��B
�!B
�UB
��B
�'B
�'B
��B
��B
��B
�MB
��B
�B
�tB
��B
��B
�`B
��B
��B
��B
�XB
�0B
��B
��B
�B
�jB
��B
�(B
��B
�HB
��B
��B
��B
�UB
��B
�'B
�[B
�uB
��B
��B
�-B
�aB
�GB
��B
�B
ĜB
ŢB
�YB
��B
�_B
�zB
�B
��B
�RB
�RB
��B
ˬB
��B
͹B
�<B
�"B
�VB
�pB
�VB
��B
�\B
�\B
�BB
ϫB
�}B
�bB
��B
��B
�B
��B
��B
�:B
ңB
�@B
�B
��B
��B
�B
�B
ּB
׍B
�EB
�eB
�B
�QB
ڠB
��B
�qB
�qB
یB
�qB
ܒB
�~B
��B
�B
�jB
�B
�B
�BB
�'B
�B
�'B
��B
�bB
�B
��B
�NB
�B
�B
� B
�B
�tB
��B
�zB
��B
�8B
��B
�B
�*B
�B
�B
�B
�B
�B
�B
�B
�B
�kB
�B
��B
�WB
��B
�B
�CB
�B
��B
��B
� B
�B
�OB
�B
��B
�;B
��B
�'B
��B
�MB
�B
�%B
��B
�+B
�`B
��B
��B
�B
��B
�B
��B
��B
�rB
��B
�B
�*B
�dB
��B
��B
��B
��B
�VB
�VB
��B
�B
�]B
��B
��B
��B
�B
�.B
�}B
��B
��B
��B
��B �BUB�B�BuBGB-B�BMBgB�B�BB�BB?BYBtB�B�B�BBEB�B�B�B1B	7B	�B	�B	�B	�B
#B
rB
�B
�B
�B
�BDBxB�B�B�BJB~B~B~B�B�B�B�B�B�B�B.BbB}B B4BNB4B�B�B�B:B B:B�B�B�B�B�B�B�BBB&B�B�BaB{B�B�B�BMB�B�B�B�BmB�B
B�B�B�B�B_B�BKBeBKBB7B�BqB�B]B]BxB�B/B~B�BBjB�B�B;B�B B \B �B!-B!HB!|B!|B!�B"B"4B"hB"�B"�B#�B$tB$�B$�B$�B%B%,B%,B%,B%FB&B&fB&�B'mB'mB'�B'�B'�B($B($B($B(sB(�B)B)*B)*B)DB)yB)�B*KB*�B*�B+B+B+6B+�B+�B+�B,WB,�B-B-]B-�B-�B-�B.B.cB.}B.}B.}B.�B/OB/iB/�B/�B/�B0B0;B0UB0�B0�B0�B1'B1AB1�B2B2-B2|B2�B2�B2�B2�B2�B3B3hB3�B3MB3MB3�B3�B3�B3�B3�B4nB4�B4�B4�B5%B4�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  B6�B6�B7B7B7LB7�B7�B8RB9>B9>B8B1�B�B�B�hB�}B��B��Bu�BiDB[�BO(B@ B,�B&2B�B-B��B��B��B��B�dB��B�B�DB��B��B�9B��B��B��B�B�/B��BezBTFBY1Bh�Bh�B]BR:BNBF�B9�B3�B-�B$�B�B�B��B�@BЗB�lB��B�<B�B�B�sB�$B��B�pB�%ByrBm)BWYBK�B4�B*�B!-B]B[B�BKB�B
�^B
�iB
�nB
�;B
��B
�B
�1B
�HB
�]B
�RB
�B
�B
�IB
��B
��B
��B
��B
�7B
��B
�%B
��B
�3B
��B
��B
� B
y�B
jB
gRB
\CB
J�B
D�B
B�B
@ B
<�B
1�B
)�B
"�B
 �B
B
xB
B
gB
}B
DB

rB
	�B
�B
�B	��B	�lB	�B	��B	�B	��B	�B	�HB	�]B	רB	�2B	��B	�TB	�"B	�XB	�_B	�B	��B	�wB	��B	�B	�B	��B	�hB	�;B	��B	��B	��B	�B	��B	�B	��B	�WB	��B	��B	�B	��B	��B	�aB	�UB	�4B	~�B	y�B	s�B	rGB	q�B	o�B	n}B	l�B	lWB	k�B	k6B	i�B	g�B	f�B	e�B	dtB	aHB	\�B	W?B	RoB	J�B	C�B	<B	8�B	6�B	5�B	3�B	0UB	-�B	*�B	&�B	%�B	%zB	$�B	$B	#�B	!|B	�B	�B	�B	WB	7B	�B	�B	FB	�B	
�B		�B	_B	9B	�B	�B	�B	B��B�qB�XB�LB�B�%B�9B��B�hB��B�B�[B�oB��B��B�]B��B��B�kB�B�B�LB�B�LB��B�BݘB��B��B��B�mB��B��B�&B�TB�oBϫB��B��B̘BʌB�	BɆB��B��B��B�SB�BðBªB��BªB��B�AB�AB�'BB��B��B��B��B�B��B��B�iB��B�UB��B�B��B��BB��B�9B��BǮB��BǮB��B��BʦB��B˒B��B�B�\B�HB�hB�oBңB�uBּB��B��B��B�B�]B�dB�'B��B��B�B�mB�B��B�B�B��B�3B��B�B��B�HB	[B	�B	�B	jB	B	FB	�B	�B	
B	�B	eB	�B	�B	/B	�B	!�B	&LB	*�B	-]B	5%B	8�B	=�B	A�B	J	B	MB	PHB	UB	Z�B	\�B	_;B	aHB	cTB	hsB	i�B	j�B	l�B	n}B	n�B	ncB	n�B	o�B	p�B	q�B	s�B	x8B	y>B	y�B	{dB	{B	{�B	{dB	}B	�-B	�B	��B	�EB	�B	��B	��B	�(B	��B	��B	��B	�1B	�kB	��B	��B	�pB	�B	��B	��B	�,B	�
B	�6B	��B	��B	�`B	��B	��B	��B	�}B	�oB	�SB	�zB	�B	ˬB	͹B	� B	�&B	�2B	׍B	ٴB	�]B	�|B	��B	�B	��B	�LB	�sB	�B	�=B	��B	�[B	��B	�|B	�9B	�RB	��B	��B	�(B	�}B
uB
�B
�B
�B
	�B
	�B
	�B
	�B
^B
~B
B
�B
�B
�B
B
B
7B
�B
�B
"�B
(>B
*KB
+B
+�B
,�B
.�B
/5B
/OB
/OB
/OB
/OB
/�B
/�B
/�B
2|B
4�B
6`B
9XB
<�B
=�B
?�B
B�B
D3B
EmB
FB
F?B
FYB
FtB
G_B
HfB
H�B
IRB
JrB
LdB
N"B
O�B
O�B
P�B
QhB
S�B
T{B
T�B
V�B
X_B
YeB
ZQB
Z�B
[=B
[�B
\�B
_;B
b�B
d�B
d�B
g�B
iyB
j0B
jB
j�B
l�B
m�B
o�B
qAB
rGB
tB
tB
tnB
t�B
vFB
yXB
{JB
|�B
HB
�B
�iB
��B
��B
��B
�SB
�mB
�mB
�B
�RB
�#B
��B
�DB
��B
�"B
�pB
�pB
��B
�B
�.B
��B
��B
�:B
��B
�uB
�B
��B
�mB
��B
��B
��B
��B
�#B
�/B
��B
��B
�-B
��B
�B
��B
�KB
��B
��B
��B
�)B
��B
�/B
��B
��B
��B
��B
�5B
�iB
�iB
��B
�!B
�UB
��B
�'B
�'B
��B
��B
��B
�MB
��B
�B
�tB
��B
��B
�`B
��B
��B
��B
�XB
�0B
��B
��B
�B
�jB
��B
�(B
��B
�HB
��B
��B
��B
�UB
��B
�'B
�[B
�uB
��B
��B
�-B
�aB
�GB
��B
�B
ĜB
ŢB
�YB
��B
�_B
�zB
�B
��B
�RB
�RB
��B
ˬB
��B
͹B
�<B
�"B
�VB
�pB
�VB
��B
�\B
�\B
�BB
ϫB
�}B
�bB
��B
��B
�B
��B
��B
�:B
ңB
�@B
�B
��B
��B
�B
�B
ּB
׍B
�EB
�eB
�B
�QB
ڠB
��B
�qB
�qB
یB
�qB
ܒB
�~B
��B
�B
�jB
�B
�B
�BB
�'B
�B
�'B
��B
�bB
�B
��B
�NB
�B
�B
� B
�B
�tB
��B
�zB
��B
�8B
��B
�B
�*B
�B
�B
�B
�B
�B
�B
�B
�B
�kB
�B
��B
�WB
��B
�B
�CB
�B
��B
��B
� B
�B
�OB
�B
��B
�;B
��B
�'B
��B
�MB
�B
�%B
��B
�+B
�`B
��B
��B
�B
��B
�B
��B
��B
�rB
��B
�B
�*B
�dB
��B
��B
��B
��B
�VB
�VB
��B
�B
�]B
��B
��B
��B
�B
�.B
�}B
��B
��B
��B
��B �BUB�B�BuBGB-B�BMBgB�B�BB�BB?BYBtB�B�B�BBEB�B�B�B1B	7B	�B	�B	�B	�B
#B
rB
�B
�B
�B
�BDBxB�B�B�BJB~B~B~B�B�B�B�B�B�B�B.BbB}B B4BNB4B�B�B�B:B B:B�B�B�B�B�B�B�BBB&B�B�BaB{B�B�B�BMB�B�B�B�BmB�B
B�B�B�B�B_B�BKBeBKBB7B�BqB�B]B]BxB�B/B~B�BBjB�B�B;B�B B \B �B!-B!HB!|B!|B!�B"B"4B"hB"�B"�B#�B$tB$�B$�B$�B%B%,B%,B%,B%FB&B&fB&�B'mB'mB'�B'�B'�B($B($B($B(sB(�B)B)*B)*B)DB)yB)�B*KB*�B*�B+B+B+6B+�B+�B+�B,WB,�B-B-]B-�B-�B-�B.B.cB.}B.}B.}B.�B/OB/iB/�B/�B/�B0B0;B0UB0�B0�B0�B1'B1AB1�B2B2-B2|B2�B2�B2�B2�B2�B3B3hB3�B3MB3MB3�B3�B3�B3�B3�B4nB4�B4�B4�B5%B4�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230711034944  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230711034945  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230711034946  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230711034946                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230711034947  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230711034947  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20230711040251                      G�O�G�O�G�O�                