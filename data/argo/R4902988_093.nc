CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T22:55:47Z creation;2022-06-04T22:55:48Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604225547  20220609221504  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               ]A   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @���l��1   @��밋�B@<��
=q�c��l�C�1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@�  A   A   A@  A`  A���A���A���A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�33B�33B�33B�33B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�33B�  B�  B�  B�  B�  B�33B�  C   C�fC  C  C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CY�fC\  C^  C`  Cb  Cc�fCf  Ch  Cj  Cl  Cn  Cp�Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C��C��C��C��C�  C�  C��3C��3C�  C�  C��3C��3C��3C��3C��3C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C��C��C��C�  C��3C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  D   D y�D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(y�D(��D)� D*  D*� D+  D+� D,  D,� D-  D-�fD.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4fD4� D5  D5� D6  D6� D7  D7� D8  D8� D8��D9y�D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG�fDH  DH� DI  DI� DJ  DJy�DK  DK� DL  DLy�DM  DM� DN  DN� DO  DO� DP  DP�fDQfDQ� DR  DR� DS  DS� DT  DT� DU  DU� DU��DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dc��Dd� DefDe� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Dn��Do� Dp  Dp� Dq  Dq�fDr  Dr� Dr��Ds� DtfDt� Dt��Du� DvfDv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz�fD{  D{� D|  D|� D}  D}� D~  D~� DfD� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D��3D�3D�C3D�� D���D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D��3D�3D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�C3D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D��3D�  D�<�D�|�D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�3D�@ D�� D�� D�  D�@ D��3D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�<�D�|�D�� D���D�@ DĀ D��3D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�3D�C3Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ D˃3D�� D�  D�@ D�|�D̼�D���D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�<�DЀ D�� D���D�@ Dр D�� D�  D�C3DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր Dּ�D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@���@���AffA>ffA^ffA�  A�  A�  A�  A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B�  B�  B�  B�  B�  B�  B�  B���B���B���B���B���B���B���Bϙ�B���B���B���B�  B���B���B���B���B���B�  B���B���C��C�fC�fC  C	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC.  C/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY��C[�fC]�fC_�fCa�fCc��Ce�fCg�fCi�fCk�fCm�fCp  Cq�fCs��Cu�fCw�fCy�fC{�fC}�fC�fC�  C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��fC��fC��3C��3C��fC��fC��3C��3C��3C��3C��3C��3C�  C�  C�  C�  C�  C��3C��3C��fC��fC��3C��3C��fC��fC��fC��fC��fC��3C��3C�  C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��fC��3C��3C��3C��3C�  C�  C�  C�  C��3C��fC��fC��3C��3C��3C��3C��3C��fC��3C��3C��3C��fC��3C��3C��3C��3D s4D ��D� D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D�4D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(s4D(�4D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-� D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D4  D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8�4D9s4D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DG� DG��DHy�DH��DIy�DI��DJs4DJ��DKy�DK��DLs4DL��DMy�DM��DNy�DN��DOy�DO��DP� DQ  DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU�4DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc�4Ddy�De  Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn�4Doy�Do��Dpy�Dp��Dq� Dq��Dry�Dr�4Dsy�Dt  Dty�Dt�4Duy�Dv  Dvy�Dv��Dwy�Dw��Dxy�Dx��Dyy�Dy��Dz� Dz��D{y�D{��D|y�D|��D}y�D}��D~y�D  Dy�D��D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D�� D�  D�@ D�|�D���D���D�9�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�9�D�|�D�� D�  D�<�D�|�D���D���D�<�D�� D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�@ D�|�D���D���D�@ D�|�D���D���D�<�D�|�D�� D���D�9�D�y�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�@ D�� D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�9�D�|�D���D�  D�<�D�|�D���D���D�<�D�� D�� D�  D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�� D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D�  D�<�D�|�D���D���D�9�D�|�D���D���D�<�D�|�D�� D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D�� D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�y�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D¼�D���D�9�D�y�Dü�D���D�<�D�|�D�� D���D�<�D�|�Dż�D���D�<�D�|�DƼ�D���D�<�D�|�DǼ�D���D�<�D�|�Dȼ�D�  D�@ D�|�Dɼ�D���D�<�D�|�Dʼ�D���D�<�Dˀ D˼�D���D�<�D�y�D̹�D���D�<�D�|�Dͼ�D���D�<�D�|�Dμ�D���D�<�D�|�Dϼ�D���D�9�D�|�Dм�D���D�<�D�|�DѼ�D���D�@ D�|�DҼ�D���D�<�D�|�DӼ�D���D�<�D�|�DԼ�D���D�<�D�|�Dռ�D���D�<�D�|�Dֹ�D���D�<�D�|�D׼�D���D�<�D�|�Dؼ�D���D�<�D�|�Dټ�D���D�<�D�|�Dڼ�D���D�<�D�|�Dۼ�D���D�<�D�|�Dܼ�D���D�<�D�|�Dݼ�D���D�<�D�|�D޼�D���D�<�D�|�D߼�D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�9�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D�� D���D�<�D� D��D���D�<�D�|�D��D���D�<�D�|�D��D�  D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�@ D�|�D���D���D�<�D�|�D���D�  D�@ D�|�D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A��JA��DA�|A�MjA�*�A���A��LA���A�+kA��A�a�A��(A���A�\]A��GA��'A��bA�jA�L0A�-�A�
�A��5A��[A��^A���A���A�.}A�A�T�A��A�2�A��A��A�{A��|A�Z�A�	A��A���A��`A�>�A�y�A���A�@�A��A���A�uA�poA�qA�[�A��>A�"4A��A��aA�_�A�eA��A�D�A��"A���A�8�A���A��A���A�u�A�A�l�A�)�A��A�{JA��A���A�HKA�oiA�o A��6A���A�ĜA��dA��A�̘A�?A�)�A�
�A��A�|�A~�+A}XA{*0Ay�Auk�At�AsArF�Aq?}Ap_An�hAm�jAm($Ak��Aj��Ai)_Ag�AgL�Af�#AeS�Acy�AbH�Aa>�A`[WA^��A]�\A\e�A[�1AX�AV�AV͟AV��AV�.AV.�AU�RATIRAS�AR��AR��AQ��AP
�AN��AM��AMZ�AM�AL=�AK��AK�fAJ�	AI�AI��AI#�AHz�AG��AFu�AE��AE-�AD�PAD�AD�AC�ABQ�AA��A@��A?�A?�_A?��A>�mA>�A=��A<�A<��A<8�A;�`A;�{A:یA:��A:�$A:]�A9y�A8n/A7�RA7+�A6W�A5��A4�oA3}�A2�:A1?}A0��A/ƨA/`BA.��A-�mA-z�A,�'A,!A+<�A*ںA*ffA*qA(�EA'�jA'VmA&�A%��A%ԕA%��A%aA$�A"��A!�A!@�A �.A �A �9A e,A��A#:AJ#Ao A(�A
=A�$AT�A4AݘAL�A�mAhsACA�A�{A�AaAX�A�>Az�A�ZA�}AYAE�A#:AOA�VAu�A�A��A1A�]A�A	�A�eA�A(�Au�AMA��A�A<6A_A��AخA Q@���@���@��t@��q@��@�l"@�b@�bN@�;@�҉@�9�@�^5@��@�0U@�h@�&@�_@�`�@�� @�Q@�;d@�Q�@��+@�O@�@�	�@�@�s�@Ա�@�w2@�q�@�rG@�J�@��@�e,@���@�~�@�}V@ǉ7@ū�@�u�@�($@�|�@�+@��@�$@��@���@���@�H�@���@���@�S&@�~�@�O�@��@��:@��6@�Ta@��@��;@�4@���@���@���@���@��@���@�e�@�@��Q@��w@�b�@�H@��	@��@���@��@���@��$@�ی@�&�@�e�@��!@��H@��6@�_�@�U2@�N�@�I�@�8�@��@��k@�9�@��H@��@�L�@���@��j@���@�$�@��@��@�_@�T�@��2@���@�u%@�W�@�:�@�&�@���@�=�@�$@��)@��A@�ƨ@��@���@�W?@�;@��u@�d�@�($@��"@�L�@��@�&�@�a@�/@��@�z@�h�@��@��*@�5�@�ȴ@��e@��_@���@�U2@�@��>@��t@��@��	@��y@�͟@��9@��z@���@�;�@��@��o@�$�@��g@�c@��@�V@�	�@��@���@�IR@��6@�~�@�bN@�L0@��.@�F�@��U@��6@�d�@�5?@�	@��W@��@��@���@�F@�%F@��f@���@�Y@�a�@�N<@�1�@�0�@�2a@��@���@���@�O@���@���@�?}@�,�@�C@��@���@��]@��$@���@�ff@�6@�O@�m@�0@�@��@��@�:@y�@}�t@}=�@|�@{�@{��@{s@{U�@{/�@z&�@y��@yhs@y&�@y�@y@@xA�@wخ@w�
@wƨ@w�a@w�	@w]�@w�@v�m@va|@vTa@vZ�@v��@v8�@uԕ@u�-@u	l@t��@tQ�@s�@s
=@rxl@r1�@r_@q|@qDg@q2a@pm�@o��@n��@n� @n��@nC�@mIR@l��@l��@lc�@k��@kA�@k�@j�x@js�@ja|@jJ@i�@i@@h�I@hq@hw�@h*�@g�f@g]�@g�@f��@f��@f�x@fn�@e�H@e;@dM@d@c�	@c�@b�@b��@b.�@a��@a�@a�@a��@aa�@a?}@a@@a�@`�@`Ɇ@`��@`!@_\)@_S@^�@^�!@^6�@]��@]o @]k�@]^�@]-w@]�@]�@\��@\��@\y>@\ �@[�]@[��@[�{@[J#@[C@Z�H@Z� @Z)�@Y�T@Y�@Y��@YrG@YDg@Y�@X�P@X�@Xw�@W�A@W��@W�*@Wqv@WE9@W'�@V�@V��@V{�@VM�@V8�@V�@U��@Ux�@T�	@T��@T��@TK^@T1@S�F@S~�@S@O@R��@R�r@R\�@RGE@Q��@P��@Pq@PC-@O�m@O�
@O��@O�@Ot�@O_p@O;d@N��@N�'@N�b@M�N@Mu�@MIR@M&�@M#�@M�@L�`@L�Y@LPH@L/�@K�m@K��@K�P@KO@K i@J��@J�'@J�@J�}@J�L@J�b@Jxl@JOv@J1�@I�)@Ic@HtT@H*�@G�@GE9@Fv�@E�Z@E#�@D�D@D>B@DM@C�W@C�Q@C�{@C"�@C i@B��@BYK@B�@A��@A�>@A�H@A��@A��@A}�@AN<@A-w@@�@@�I@@m�@@1'@?��@?��@?�	@?iD@?�@?@>��@>��@>V@=�>@<�@<��@<1'@;�@@;|�@;S�@;1�@;o@:�M@:��@:Ta@:3�@9w2@8�f@8�U@8l"@8U2@8PH@8/�@8�@8�@7��@7��@7U�@7Mj@7�@7�@7�@6��@6H�@5�@5hs@5N<@5F@5&�@4��@49X@3�}@3��@3�	@3s@3�@2��@2��@2��@2��@2��@2E�@1��@1F@0��@/�]@/C�@/&@/�@.�@.�H@.�@.�@.�]@.�,@.��@.ȴ@.�@.�!@.��@.��@.{�@.J�@.{@-�3@-��@-o @-2a@,Ɇ@+��@*�M@*�M@*ߤ@*��@*��@*��@*h
@*O@*�@*@*	@)�@)�d@)��@(��@(�K@(�?@(h�@(2�@(@'��@'�	@'/�@'�@&��@&�\@&M�@&6�@&($@&�@&�@&�@%��@%�@%�d@%@%��@%Dg@%	l@$�@$��@$w�@$M@$>B@$:�@$6@$:�@$-�@$�@#�f@#
=@"�B@"\�@!��@!�>@!�@!�j@!�z@!�S@!�@ �@ �.@ �D@ tT@ 1'@� @�q@�4@S�@K�@"�@�6@.�@_@�Z@�9@��@�@�@�@q@Q�@�@��@�W@� @�a@�@�@@{J@_p@33@
=@�X@}V@:*@�d@�X@�7@?}@�U@I�@��@,�@��@��@xl@v�@n�@B[@��@�7@F@�[@~(@9X@7@�@�&@�g@ƨ@��@��@�@@��@\)@�h@M�@&�@�9@�@�=@��@c@u�@^�@J�@4@ \@�@w�@[�@�@�@��@�P@�4@n/@U�@>�@$t@��@��@�6@�F@�A@p;@a|@�@��@�@j@X@Q�@L�@O�@Dg@ی@��@�9@|�@Z@�@��@��@�k@v`@@
�2@
��@
s�@
H�@
+k@
&�@
�@	ϫ@	��@	�S@	Q�@��@�v@�$@�@Z@D�@7�@1@�;@��@j�@S�@F�@C�@=@!-@�@�@�L@@�@�@ �@�Z@��@�@��@�@a�@0�@5�@�@��@��@��@_@7�@�@��@�{@O@@O@=@9�@C@��@�@�s1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A��JA��DA�|A�MjA�*�A���A��LA���A�+kA��A�a�A��(A���A�\]A��GA��'A��bA�jA�L0A�-�A�
�A��5A��[A��^A���A���A�.}A�A�T�A��A�2�A��A��A�{A��|A�Z�A�	A��A���A��`A�>�A�y�A���A�@�A��A���A�uA�poA�qA�[�A��>A�"4A��A��aA�_�A�eA��A�D�A��"A���A�8�A���A��A���A�u�A�A�l�A�)�A��A�{JA��A���A�HKA�oiA�o A��6A���A�ĜA��dA��A�̘A�?A�)�A�
�A��A�|�A~�+A}XA{*0Ay�Auk�At�AsArF�Aq?}Ap_An�hAm�jAm($Ak��Aj��Ai)_Ag�AgL�Af�#AeS�Acy�AbH�Aa>�A`[WA^��A]�\A\e�A[�1AX�AV�AV͟AV��AV�.AV.�AU�RATIRAS�AR��AR��AQ��AP
�AN��AM��AMZ�AM�AL=�AK��AK�fAJ�	AI�AI��AI#�AHz�AG��AFu�AE��AE-�AD�PAD�AD�AC�ABQ�AA��A@��A?�A?�_A?��A>�mA>�A=��A<�A<��A<8�A;�`A;�{A:یA:��A:�$A:]�A9y�A8n/A7�RA7+�A6W�A5��A4�oA3}�A2�:A1?}A0��A/ƨA/`BA.��A-�mA-z�A,�'A,!A+<�A*ںA*ffA*qA(�EA'�jA'VmA&�A%��A%ԕA%��A%aA$�A"��A!�A!@�A �.A �A �9A e,A��A#:AJ#Ao A(�A
=A�$AT�A4AݘAL�A�mAhsACA�A�{A�AaAX�A�>Az�A�ZA�}AYAE�A#:AOA�VAu�A�A��A1A�]A�A	�A�eA�A(�Au�AMA��A�A<6A_A��AخA Q@���@���@��t@��q@��@�l"@�b@�bN@�;@�҉@�9�@�^5@��@�0U@�h@�&@�_@�`�@�� @�Q@�;d@�Q�@��+@�O@�@�	�@�@�s�@Ա�@�w2@�q�@�rG@�J�@��@�e,@���@�~�@�}V@ǉ7@ū�@�u�@�($@�|�@�+@��@�$@��@���@���@�H�@���@���@�S&@�~�@�O�@��@��:@��6@�Ta@��@��;@�4@���@���@���@���@��@���@�e�@�@��Q@��w@�b�@�H@��	@��@���@��@���@��$@�ی@�&�@�e�@��!@��H@��6@�_�@�U2@�N�@�I�@�8�@��@��k@�9�@��H@��@�L�@���@��j@���@�$�@��@��@�_@�T�@��2@���@�u%@�W�@�:�@�&�@���@�=�@�$@��)@��A@�ƨ@��@���@�W?@�;@��u@�d�@�($@��"@�L�@��@�&�@�a@�/@��@�z@�h�@��@��*@�5�@�ȴ@��e@��_@���@�U2@�@��>@��t@��@��	@��y@�͟@��9@��z@���@�;�@��@��o@�$�@��g@�c@��@�V@�	�@��@���@�IR@��6@�~�@�bN@�L0@��.@�F�@��U@��6@�d�@�5?@�	@��W@��@��@���@�F@�%F@��f@���@�Y@�a�@�N<@�1�@�0�@�2a@��@���@���@�O@���@���@�?}@�,�@�C@��@���@��]@��$@���@�ff@�6@�O@�m@�0@�@��@��@�:@y�@}�t@}=�@|�@{�@{��@{s@{U�@{/�@z&�@y��@yhs@y&�@y�@y@@xA�@wخ@w�
@wƨ@w�a@w�	@w]�@w�@v�m@va|@vTa@vZ�@v��@v8�@uԕ@u�-@u	l@t��@tQ�@s�@s
=@rxl@r1�@r_@q|@qDg@q2a@pm�@o��@n��@n� @n��@nC�@mIR@l��@l��@lc�@k��@kA�@k�@j�x@js�@ja|@jJ@i�@i@@h�I@hq@hw�@h*�@g�f@g]�@g�@f��@f��@f�x@fn�@e�H@e;@dM@d@c�	@c�@b�@b��@b.�@a��@a�@a�@a��@aa�@a?}@a@@a�@`�@`Ɇ@`��@`!@_\)@_S@^�@^�!@^6�@]��@]o @]k�@]^�@]-w@]�@]�@\��@\��@\y>@\ �@[�]@[��@[�{@[J#@[C@Z�H@Z� @Z)�@Y�T@Y�@Y��@YrG@YDg@Y�@X�P@X�@Xw�@W�A@W��@W�*@Wqv@WE9@W'�@V�@V��@V{�@VM�@V8�@V�@U��@Ux�@T�	@T��@T��@TK^@T1@S�F@S~�@S@O@R��@R�r@R\�@RGE@Q��@P��@Pq@PC-@O�m@O�
@O��@O�@Ot�@O_p@O;d@N��@N�'@N�b@M�N@Mu�@MIR@M&�@M#�@M�@L�`@L�Y@LPH@L/�@K�m@K��@K�P@KO@K i@J��@J�'@J�@J�}@J�L@J�b@Jxl@JOv@J1�@I�)@Ic@HtT@H*�@G�@GE9@Fv�@E�Z@E#�@D�D@D>B@DM@C�W@C�Q@C�{@C"�@C i@B��@BYK@B�@A��@A�>@A�H@A��@A��@A}�@AN<@A-w@@�@@�I@@m�@@1'@?��@?��@?�	@?iD@?�@?@>��@>��@>V@=�>@<�@<��@<1'@;�@@;|�@;S�@;1�@;o@:�M@:��@:Ta@:3�@9w2@8�f@8�U@8l"@8U2@8PH@8/�@8�@8�@7��@7��@7U�@7Mj@7�@7�@7�@6��@6H�@5�@5hs@5N<@5F@5&�@4��@49X@3�}@3��@3�	@3s@3�@2��@2��@2��@2��@2��@2E�@1��@1F@0��@/�]@/C�@/&@/�@.�@.�H@.�@.�@.�]@.�,@.��@.ȴ@.�@.�!@.��@.��@.{�@.J�@.{@-�3@-��@-o @-2a@,Ɇ@+��@*�M@*�M@*ߤ@*��@*��@*��@*h
@*O@*�@*@*	@)�@)�d@)��@(��@(�K@(�?@(h�@(2�@(@'��@'�	@'/�@'�@&��@&�\@&M�@&6�@&($@&�@&�@&�@%��@%�@%�d@%@%��@%Dg@%	l@$�@$��@$w�@$M@$>B@$:�@$6@$:�@$-�@$�@#�f@#
=@"�B@"\�@!��@!�>@!�@!�j@!�z@!�S@!�@ �@ �.@ �D@ tT@ 1'@� @�q@�4@S�@K�@"�@�6@.�@_@�Z@�9@��@�@�@�@q@Q�@�@��@�W@� @�a@�@�@@{J@_p@33@
=@�X@}V@:*@�d@�X@�7@?}@�U@I�@��@,�@��@��@xl@v�@n�@B[@��@�7@F@�[@~(@9X@7@�@�&@�g@ƨ@��@��@�@@��@\)@�h@M�@&�@�9@�@�=@��@c@u�@^�@J�@4@ \@�@w�@[�@�@�@��@�P@�4@n/@U�@>�@$t@��@��@�6@�F@�A@p;@a|@�@��@�@j@X@Q�@L�@O�@Dg@ی@��@�9@|�@Z@�@��@��@�k@v`@@
�2@
��@
s�@
H�@
+k@
&�@
�@	ϫ@	��@	�S@	Q�@��@�v@�$@�@Z@D�@7�@1@�;@��@j�@S�@F�@C�@=@!-@�@�@�L@@�@�@ �@�Z@��@�@��@�@a�@0�@5�@�@��@��@��@_@7�@�@��@�{@O@@O@=@9�@C@��@�@�s1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�hB� B��B�jB�xB��BƨBňBƎB��B��B��B��B��B��B��B��B��B�)B�CB�)B��B��B�NB��B��B�KB�B�PB��Bw�B^�BBuB6�B2�B�B�B(B�*B�8B��B��B�YB�iB�*B�B�B��B�nB�
B�NB��B� B{�Bv�Bc�B]�BXEBA B:B3�B*0B�B+B-B�PB��B�CB�B��B�BԕB�B�;B��B��B�B�=B��B��B.By	BiBY�BGzB1vB!-B�B�B
��B
�B
�IB
՛B
�B
��B
�B
��B
�iB
�B
�|B
�dB
��B
��B
��B
�_B
�oB
x�B
rB
k�B
h
B
cB
[�B
T�B
O�B
F�B
<�B
>�B
?�B
@�B
?B
>]B
7fB
1B
,�B
'�B
!�B
?B
,B
B
�B
�B
�B
EB
tB
 OB	��B	�>B	��B	�FB	��B	��B	��B	�B	��B	��B	�6B	�sB	��B	��B	ݲB	�+B	��B	�9B	�{B	�\B	͟B	�#B	�rB	�RB	�_B	�YB	�GB	�'B	�B	��B	��B	�lB	�B	��B	�IB	��B	�zB	��B	�B	�yB	��B	�NB	��B	�B	��B	�YB	�uB	�iB	{JB	yXB	v�B	t�B	p�B	k6B	g�B	fB	a�B	`�B	_�B	]�B	Z�B	T,B	M�B	K�B	J�B	IlB	H�B	GzB	D�B	A�B	>]B	:*B	7�B	7�B	6+B	5B	49B	3�B	2�B	1�B	/B	./B	*�B	*0B	(�B	(�B	!�B	!B	!-B	"NB	VB	xB	�B	B	FB	TB	B	B	B	
XB	_B	gB	 �B��B�^B�lB�zB�9B�aB��B�5B�/B�CB�B�eB�B��B�tB�B�4B�hBݲB�BٚB�KB׍B��BԕB��B�[B��B��B�B��B�HB�\B͟B�6B�B��B˒B�	B�7B�rB�7B�RB��BʦB�#B�B��BȀB�KB̳B�^B�XB�XB�rB�=B�DB��BʦB�)B�)B�xB̘B��B�6BϑB��BуB�NB��B�PB��B̘B�}BּB�!B�BB�B��B�B��B�B�NB�NB�B�B��B��B�B�B�3B�B��B�RB�*B��B�}B	�B	�B	�B	�B	3B	MB	B	?B	�B	B	
�B	6B	(B	�B	�B	,B	�B	hB	�B	SB	�B	�B	#B	B	�B	�B	!B	%�B	'�B	($B	,�B	.�B	/OB	0UB	2|B	4nB	7�B	:^B	<�B	>�B	?�B	@B	B�B	D�B	E9B	FtB	I�B	K�B	O�B	PHB	SuB	V�B	W?B	W�B	XEB	ZB	\�B	^�B	_�B	c�B	d&B	d�B	e�B	f�B	f�B	gB	h�B	q'B	rB	tB	v�B	|B	~�B	�;B	��B	�B	��B	��B	��B	�"B	��B	��B	��B	�EB	��B	��B	��B	�'B	��B	�bB	��B	�B	�kB	�0B	�kB	�5B	�9B	��B	��B	�B	ǮB	��B	��B	��B	�=B	�)B	��B	��B	�MB	�sB	��B	�yB	ٚB	�B	چB	��B	��B	�B	�&B	�B	�QB	��B	�CB	�B	�IB	�B	�OB	��B	��B	�JB
[B
�B
gB
�B
B
�B
	RB
xB
~B
6B
�B
�B
�B
B
aB
�B
9B
SB
�B
EB
B
=B
�B
xB
B
B
!HB
$ZB
&LB
'�B
)�B
-CB
/�B
0�B
1�B
4B
5tB
5�B
72B
9	B
;B
;�B
;dB
;�B
=�B
>�B
@ B
A�B
DMB
FB
GB
JXB
K�B
LJB
MPB
NB
O�B
S&B
V�B
W
B
YeB
]IB
^�B
`B
bB
bNB
bNB
b�B
c�B
d�B
h�B
j0B
l"B
n}B
p�B
q�B
s�B
t9B
tTB
t�B
u%B
uZB
utB
u�B
u�B
vB
vzB
v�B
xRB
z�B
{B
{�B
|jB
}�B
� B
�B
��B
�oB
��B
��B
�B
�uB
�-B
�B
��B
��B
��B
�B
�7B
�lB
�#B
�)B
�B
�"B
��B
�vB
��B
��B
�4B
�hB
��B
�&B
��B
�2B
��B
�mB
�
B
�YB
�B
�B
��B
�7B
��B
�#B
�B
��B
�5B
��B
�VB
�'B
�B
�B
��B
��B
��B
�2B
��B
��B
�>B
�6B
��B
�"B
�CB
�wB
�B
��B
�B
�iB
��B
�oB
��B
�AB
��B
�B
��B
�ZB
��B
�zB
�B
�8B
�B
�RB
��B
�>B
�rB
�xB
��B
�dB
�dB
��B
��B
��B
��B
�B
�jB
��B
�B
��B
�iB
��B
��B
B
�gB
�9B
ǮB
�lB
ɺB
��B
�	B
�#B
ʦB
�)B
�)B
��B
̳B
�jB
��B
��B
�VB
�pB
ΊB
ΥB
�(B
ϑB
�.B
ЗB
��B
уB
�:B
҉B
��B
�&B
��B
��B
�aB
��B
�B
��B
�B
�yB
�eB
چB
��B
�=B
یB
��B
��B
ܒB
�B
�/B
޸B
ߊB
�B
�B
��B
��B
�B
�-B
�bB
�B
�NB
�B
�B
��B
��B
��B
�B
�&B
�FB
��B
��B
��B
�B
��B
��B
��B
�B
�_B
�DB
��B
�B
�B
�B
�B
�B
�QB
�B
��B
�CB
��B
�B
�B
��B
��B
�!B
�;B
�!B
�;B
�UB
�oB
�oB
�B
�B
�B
��B
�'B
�AB
�B
�-B
�GB
�B
��B
�hB
��B
�`B
�+B
�zB
��B
�B
��B
�LB
��B
��B
��B
��B
�B
�8B
�lB
�rB
�rB
��B
�DB
��B
��B
�B
��B
�6B
�B
�PB
�B
�<B
�VB
��B
��B
��B
��B
��B
�(B
�(B
�(B
�wB
��B
�B
�HB
��B  B OB iB iB �B BoB�B[B�B�B�B�B�B�B�B�B9B%B�B�B�B�BzB�B�B	B	B�B�B	�B	lB	lB	�B	�B	�B	�B
�B
�BBDB�B�BBdB�B�BBPB�B�BB"B<B�BVB<B�B(BvB�B�BhB B B:B B BTB�BB[B,B{B�BBB�B�B�B�B�B�BB�B�BsB�B+ByB�B�B�BB1BeB�B�B�BkB�B�B�B	B	B=B=BqB�B�B�BCB]B]BxBxB�B�B�BIB�B�B�B�B�B�BjBjB�B�B�BpBpB�B�B�B BB \B �B!-B!HB!�B!�B!�B!�B!�B!�B"B"�B"�B"�B#B"�B"�B"�B#:B#�B#�B#�B#�B#�B#�B#�B#�B$&B$@B$&B$�B$�B%B$�B$�B$�B%B%FB%`B%FB$�B$�B%B$�B$@B$&B$@B$ZB$�B$�B%FB%,B%,B$�B%,B%FB%,B%z4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  B�B�hB� B��B�jB�xB��BƨBňBƎB��B��B��B��B��B��B��B��B��B�)B�CB�)B��B��B�NB��B��B�KB�B�PB��Bw�B^�BBuB6�B2�B�B�B(B�*B�8B��B��B�YB�iB�*B�B�B��B�nB�
B�NB��B� B{�Bv�Bc�B]�BXEBA B:B3�B*0B�B+B-B�PB��B�CB�B��B�BԕB�B�;B��B��B�B�=B��B��B.By	BiBY�BGzB1vB!-B�B�B
��B
�B
�IB
՛B
�B
��B
�B
��B
�iB
�B
�|B
�dB
��B
��B
��B
�_B
�oB
x�B
rB
k�B
h
B
cB
[�B
T�B
O�B
F�B
<�B
>�B
?�B
@�B
?B
>]B
7fB
1B
,�B
'�B
!�B
?B
,B
B
�B
�B
�B
EB
tB
 OB	��B	�>B	��B	�FB	��B	��B	��B	�B	��B	��B	�6B	�sB	��B	��B	ݲB	�+B	��B	�9B	�{B	�\B	͟B	�#B	�rB	�RB	�_B	�YB	�GB	�'B	�B	��B	��B	�lB	�B	��B	�IB	��B	�zB	��B	�B	�yB	��B	�NB	��B	�B	��B	�YB	�uB	�iB	{JB	yXB	v�B	t�B	p�B	k6B	g�B	fB	a�B	`�B	_�B	]�B	Z�B	T,B	M�B	K�B	J�B	IlB	H�B	GzB	D�B	A�B	>]B	:*B	7�B	7�B	6+B	5B	49B	3�B	2�B	1�B	/B	./B	*�B	*0B	(�B	(�B	!�B	!B	!-B	"NB	VB	xB	�B	B	FB	TB	B	B	B	
XB	_B	gB	 �B��B�^B�lB�zB�9B�aB��B�5B�/B�CB�B�eB�B��B�tB�B�4B�hBݲB�BٚB�KB׍B��BԕB��B�[B��B��B�B��B�HB�\B͟B�6B�B��B˒B�	B�7B�rB�7B�RB��BʦB�#B�B��BȀB�KB̳B�^B�XB�XB�rB�=B�DB��BʦB�)B�)B�xB̘B��B�6BϑB��BуB�NB��B�PB��B̘B�}BּB�!B�BB�B��B�B��B�B�NB�NB�B�B��B��B�B�B�3B�B��B�RB�*B��B�}B	�B	�B	�B	�B	3B	MB	B	?B	�B	B	
�B	6B	(B	�B	�B	,B	�B	hB	�B	SB	�B	�B	#B	B	�B	�B	!B	%�B	'�B	($B	,�B	.�B	/OB	0UB	2|B	4nB	7�B	:^B	<�B	>�B	?�B	@B	B�B	D�B	E9B	FtB	I�B	K�B	O�B	PHB	SuB	V�B	W?B	W�B	XEB	ZB	\�B	^�B	_�B	c�B	d&B	d�B	e�B	f�B	f�B	gB	h�B	q'B	rB	tB	v�B	|B	~�B	�;B	��B	�B	��B	��B	��B	�"B	��B	��B	��B	�EB	��B	��B	��B	�'B	��B	�bB	��B	�B	�kB	�0B	�kB	�5B	�9B	��B	��B	�B	ǮB	��B	��B	��B	�=B	�)B	��B	��B	�MB	�sB	��B	�yB	ٚB	�B	چB	��B	��B	�B	�&B	�B	�QB	��B	�CB	�B	�IB	�B	�OB	��B	��B	�JB
[B
�B
gB
�B
B
�B
	RB
xB
~B
6B
�B
�B
�B
B
aB
�B
9B
SB
�B
EB
B
=B
�B
xB
B
B
!HB
$ZB
&LB
'�B
)�B
-CB
/�B
0�B
1�B
4B
5tB
5�B
72B
9	B
;B
;�B
;dB
;�B
=�B
>�B
@ B
A�B
DMB
FB
GB
JXB
K�B
LJB
MPB
NB
O�B
S&B
V�B
W
B
YeB
]IB
^�B
`B
bB
bNB
bNB
b�B
c�B
d�B
h�B
j0B
l"B
n}B
p�B
q�B
s�B
t9B
tTB
t�B
u%B
uZB
utB
u�B
u�B
vB
vzB
v�B
xRB
z�B
{B
{�B
|jB
}�B
� B
�B
��B
�oB
��B
��B
�B
�uB
�-B
�B
��B
��B
��B
�B
�7B
�lB
�#B
�)B
�B
�"B
��B
�vB
��B
��B
�4B
�hB
��B
�&B
��B
�2B
��B
�mB
�
B
�YB
�B
�B
��B
�7B
��B
�#B
�B
��B
�5B
��B
�VB
�'B
�B
�B
��B
��B
��B
�2B
��B
��B
�>B
�6B
��B
�"B
�CB
�wB
�B
��B
�B
�iB
��B
�oB
��B
�AB
��B
�B
��B
�ZB
��B
�zB
�B
�8B
�B
�RB
��B
�>B
�rB
�xB
��B
�dB
�dB
��B
��B
��B
��B
�B
�jB
��B
�B
��B
�iB
��B
��B
B
�gB
�9B
ǮB
�lB
ɺB
��B
�	B
�#B
ʦB
�)B
�)B
��B
̳B
�jB
��B
��B
�VB
�pB
ΊB
ΥB
�(B
ϑB
�.B
ЗB
��B
уB
�:B
҉B
��B
�&B
��B
��B
�aB
��B
�B
��B
�B
�yB
�eB
چB
��B
�=B
یB
��B
��B
ܒB
�B
�/B
޸B
ߊB
�B
�B
��B
��B
�B
�-B
�bB
�B
�NB
�B
�B
��B
��B
��B
�B
�&B
�FB
��B
��B
��B
�B
��B
��B
��B
�B
�_B
�DB
��B
�B
�B
�B
�B
�B
�QB
�B
��B
�CB
��B
�B
�B
��B
��B
�!B
�;B
�!B
�;B
�UB
�oB
�oB
�B
�B
�B
��B
�'B
�AB
�B
�-B
�GB
�B
��B
�hB
��B
�`B
�+B
�zB
��B
�B
��B
�LB
��B
��B
��B
��B
�B
�8B
�lB
�rB
�rB
��B
�DB
��B
��B
�B
��B
�6B
�B
�PB
�B
�<B
�VB
��B
��B
��B
��B
��B
�(B
�(B
�(B
�wB
��B
�B
�HB
��B  B OB iB iB �B BoB�B[B�B�B�B�B�B�B�B�B9B%B�B�B�B�BzB�B�B	B	B�B�B	�B	lB	lB	�B	�B	�B	�B
�B
�BBDB�B�BBdB�B�BBPB�B�BB"B<B�BVB<B�B(BvB�B�BhB B B:B B BTB�BB[B,B{B�BBB�B�B�B�B�B�BB�B�BsB�B+ByB�B�B�BB1BeB�B�B�BkB�B�B�B	B	B=B=BqB�B�B�BCB]B]BxBxB�B�B�BIB�B�B�B�B�B�BjBjB�B�B�BpBpB�B�B�B BB \B �B!-B!HB!�B!�B!�B!�B!�B!�B"B"�B"�B"�B#B"�B"�B"�B#:B#�B#�B#�B#�B#�B#�B#�B#�B$&B$@B$&B$�B$�B%B$�B$�B$�B%B%FB%`B%FB$�B$�B%B$�B$@B$&B$@B$ZB$�B$�B%FB%,B%,B$�B%,B%FB%,B%z4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604213054  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604225547  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604225547  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604225548                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605075556  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605075556  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20220609221504                      G�O�G�O�G�O�                