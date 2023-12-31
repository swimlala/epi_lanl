CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-12-26T09:36:50Z creation;2018-12-26T09:36:53Z conversion to V3.1;2019-12-23T06:09:42Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181226093650  20200120021520  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               qA   JA  I2_0675_113                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @؛0*�1   @؛��Y @7�#9����c8����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�fC  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DG��DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DNy�DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� DqfDq�fDr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D���D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @`  @�33@�33A	��A)��AI��Ai��A���A���A���A���A���A���A���A���BffB
ffBffBffB"ffB*ffB2ffB:ffBBffBJffBRffBZffBbffBjffBrffBzffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�  B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C ��C��C��C��C��C
��C��C��C��C��C��C��C��C� C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�Y�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�D &fD �fD&fD�fD&fD� D&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD	&fD	�fD
&fD
�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD &fD �fD!&fD!�fD"&fD"�fD#&fD#�fD$&fD$�fD%&fD%�fD&&fD&�fD'&fD'�fD(&fD(�fD)&fD)�fD*&fD*�fD+&fD+�fD,&fD,�fD-&fD-�fD.&fD.�fD/&fD/�fD0&fD0�fD1&fD1�fD2&fD2�fD3&fD3�fD4&fD4�fD5&fD5�fD6&fD6�fD7&fD7�fD8&fD8�fD9&fD9�fD:&fD:�fD;&fD;�fD<&fD<�fD=&fD=�fD>&fD>�fD?&fD?�fD@&fD@�fDA&fDA�fDB&fDB�fDC&fDC�fDD&fDD�fDE&fDE�fDF&fDF�fDG&fDG�fDH  DH�fDI&fDI�fDJ&fDJ�fDK&fDK�fDL&fDL�fDM&fDM�fDN&fDN� DO&fDO�fDP&fDP�fDQ&fDQ�fDR&fDR�fDS&fDS�fDT&fDT�fDU&fDU�fDV&fDV�fDW&fDW�fDX&fDX�fDY&fDY�fDZ&fDZ�fD[&fD[�fD\&fD\�fD]&fD]�fD^&fD^�fD_&fD_�fD`&fD`�fDa&fDa�fDb&fDb�fDc&fDc�fDd&fDd�fDe&fDe�fDf&fDf�fDg&fDg�fDh&fDh�fDi&fDi�fDj&fDj�fDk&fDk�fDl&fDl�fDm&fDm�fDn&fDn�fDo&fDo�fDp&fDp�fDq,�Dq��Dr&fDr�fDs&fDs�fDt&fDt�fDu&fDu�fDv&fDv�fDw&fDw�fDx&fDx�fDy&fDy�fDz&fDz�fD{&fD{�fD|&fD|�fD}&fD}�fD~&fD~�fD&fD�fD�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D3D��3D�3D�S3DÓ3D��3D�3D�S3Dē3D��3D�3D�S3Dœ3D��3D�3D�S3DƓ3D��3D�3D�S3DǓ3D��3D�3D�S3Dȓ3D��3D�3D�S3Dɓ3D��3D�3D�S3Dʓ3D��3D�3D�S3D˓3D��3D�3D�S3D̓3D��3D�3D�S3D͓3D��3D�3D�S3DΓ3D��3D�3D�S3Dϓ3D��3D�3D�S3DГ3D��3D�3D�S3Dѓ3D��3D�3D�S3Dғ3D��3D�3D�S3Dӓ3D��3D� D�S3Dԓ3D��3D�3D�S3DՓ3D��3D�3D�S3D֓3D��3D�3D�S3Dד3D��3D�3D�S3Dؓ3D��3D�3D�S3Dٓ3D��3D�3D�S3Dړ3D��3D�3D�S3Dۓ3D��3D�3D�S3Dܓ3D��3D�3D�S3Dݓ3D��3D�3D�S3Dޓ3D��3D�3D�S3Dߓ3D��3D�3D�S3D��3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�fD��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�fD�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�fD�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��fD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�VA�XA�XA�ZA�XA�XA�\)A�bNA�bNA�dZA�ffA�hsA�hsA�ffA�hsA�hsA�bNA�bNA�hsA�hsA�n�A�r�A�n�A�jA�bNA�`BA�bNA�XA�S�A�S�A�VA�XA�bNA�bNA�hsA�t�A�v�A�v�A�bNA�A�A�  Aĉ7A�+A�\)A�n�A�C�A��
A�M�A�p�A�ffA��A�r�A���A�;dA�7LA��A�"�A���A�VA��DA�M�A���A��A���A�(�A���A�G�A�l�A��A�?}A���A�\)A�A��7A��A�x�A�r�A���A�1'A��`A�VA��A��wA��PA��A��A�1A��A��
A��A�(�A�^5A�{A�"�A��\A�"�A�x�A�n�A��A���A�G�A��A�bA��A���A�$�A�%A�ȴA�I�A�VA�A��DA��9A�|�A�S�A���A�l�A��A|�Ay��AxQ�Av�As��As/As/As33ArI�Ap�An5?AkS�Aj��AjZAix�Ah �Af��Ad5?A`��A_x�A_�A^�yA^v�A]C�A[AYp�AW`BAV9XAUG�AR �AP��AO�#ANĜAM33AGS�AE��AD�+AD(�AC�ACA@bNA<��A<1'A;�mA: �A;�A:�HA:{A9�hA9+A8�jA8=qA6�jA6 �A6  A5\)A2ĜA1l�A0��A0�/A0jA0  A/`BA.��A-A-S�A,�A,JA+�^A+O�A*(�A(5?A'O�A&��A%�A%O�A$�DA#\)A!�wA VA+A9XAp�A�/Az�At�A��A��A�^A��A��A�PA�\AXA��A��A�`A�wA�HA1A=qA�A
-A	�PA�`A9XA��A%A�
Ax�A��A��AȴA7LA b@�$�@���@��@��h@��@�X@�@�^5@�@��
@���@�p�@�Z@��@�+@�I�@�@��@�M�@�p�@�b@ߝ�@�@�^5@��`@���@ڇ+@�p�@ش9@��@֧�@�V@Ұ!@ѩ�@�/@Гu@Ͼw@�K�@��@ΰ!@�~�@�-@�V@�bN@��;@�$�@Ɂ@�(�@�S�@Ƈ+@�-@�@ŉ7@�?}@��/@�b@�@�@�z�@�
=@�O�@��@�j@�(�@�o@���@�O�@�bN@�1@�t�@�o@�M�@��-@�/@��/@�Z@��w@��R@�E�@��@��@���@���@��!@���@�E�@�&�@�r�@�9X@�|�@��@�ff@��#@��@���@�K�@�33@�o@�
=@���@�5?@�`B@���@�Ĝ@���@��D@�z�@�I�@�(�@��@��@���@��@�|�@�dZ@�K�@�;d@�
=@��@�v�@�V@��\@�n�@�E�@���@�p�@�G�@�7L@��`@��9@�I�@���@���@��w@��@���@�S�@��H@�M�@�=q@���@��+@�M�@��@��@��h@�7L@��@���@��@��u@�z�@�1@�t�@���@�~�@�@���@���@��@�hs@�O�@�/@�V@��`@��D@��@��P@��@��y@��!@��+@�=q@��@��-@�&�@��j@�9X@���@��;@���@�^5@��T@��-@��/@�z�@�9X@�  @��
@��@�l�@��P@���@�|�@�l�@�K�@�o@��@�ȴ@���@��\@�n�@�E�@�$�@��^@�`B@�V@�Ĝ@��9@��@�bN@�Q�@�Q�@�A�@�Q�@�r�@�r�@�Z@�9X@��@�C�@�@��y@��y@�ȴ@���@�n�@�^5@�=q@��#@�G�@���@��/@�Ĝ@��@���@��D@�r�@�Q�@�(�@�  @��;@�ƨ@��w@��F@�S�@�o@�n�@�M�@�{@��@��@�-@�@���@��-@��7@�x�@�G�@�G�@�7L@�7L@�&�@��/@���@���@��j@���@�z�@�Q�@�A�@�(�@� �@�  @��w@��@�\)@��H@���@�M�@��@��@���@���@�Ĝ@��D@�9X@��@�@~�y@~��@~��@~E�@~@~5?@}�h@|��@{�m@{dZ@z�H@z�!@z��@z�\@zn�@z^5@z�@yx�@x��@x�9@x��@x�u@x�@xr�@xA�@wl�@v�+@v5?@u`B@t�j@tz�@t9X@s��@sC�@r��@q��@qx�@q&�@p�9@pQ�@p �@o�@o�w@oK�@n��@nE�@n@m�h@m/@l��@l��@l9X@k��@kC�@k33@j~�@jJ@i�@i��@i�7@ihs@i7L@i�@h��@h��@hr�@h1'@g�@g�;@g�P@g+@f��@fȴ@f��@fv�@fE�@f$�@e��@e�h@ep�@d�j@dZ@d�@c"�@b�\@b-@a�7@`��@`�u@`Q�@_��@_l�@_�@^�y@^ȴ@^��@^�+@]�@]�-@]?}@\��@\�@\I�@[��@[C�@[@Z�H@Z��@Z�!@Z~�@Z=q@Y�@Yx�@Y�@Y%@X��@X�`@X��@X1'@W�@W|�@W;d@Vȴ@VE�@U��@Up�@U`B@UO�@U/@T�@T�D@Tz�@TZ@T1@S��@S��@St�@S33@R�@R��@R�@Q��@P��@PĜ@P�u@PbN@P �@O�;@O�w@O��@O�@N��@N�@N��@N5?@M�T@M��@M�h@MO�@MV@L��@L�@L�D@L(�@L1@K�m@K�m@K�F@K��@KdZ@KS�@K33@K"�@K@J�!@J~�@J^5@JM�@J�@I��@I�^@I��@I��@I�@H��@H�9@H�u@Hr�@G�@G+@G
=@F�R@Fv�@F5?@E��@Ep�@EO�@E�@D�j@D��@DZ@C��@CdZ@CC�@Co@B��@B=q@A��@AX@A�@@��@@��@@ �@?�P@>ȴ@>��@>V@=�-@=p�@=O�@=?}@=V@<��@<��@<Z@<�@;�F@;33@:�H@:�!@:M�@:�@9��@9x�@9&�@8��@8�@8  @7�;@7�;@7�w@7l�@6�@6$�@6$�@6@5�@5V@4�/@4��@4�j@4�@4�D@4j@41@3S�@3o@2�H@2�\@2=q@2J@1��@1��@1�7@1hs@1�@0��@0�u@0bN@0A�@0 �@/��@/K�@/;d@/+@/
=@.�y@.ȴ@.��@.��@.�+@.v�@.ff@.E�@.{@-��@-`B@-?}@-V@,�/@,��@,j@,I�@,1@+�
@+C�@+@*�H@*��@*�!@*^5@*-@)�@)��@)��@)hs@)G�@)7L@)�@(��@(��@(�9@(�u@(A�@( �@(  @'�@'�;@'�w@'�P@'K�@';d@'+@'
=@&�y@&ȴ@&��@&ff@&@%�-@%`B@%�@$��@$��@$z�@$I�@$(�@#�m@#C�@#"�@#o@"�H@"�\@"~�@"M�@"�@"J@!��@!�7@!X@!�@ ��@ bN@  �@�;@�@��@l�@+@�@�+@V@{@��@�@p�@p�@?}@�@V@�/@�@�D@j@9X@�m@�F@�@C�@o@@�H@~�@M�@-@�@�7@x�@X@��@�`@Ĝ@��@bN@ �@�@��@�w@�@�P@;d@ȴ@ff@$�@{@@�@�T@�T@��@@�-@�@p�@O�@/@�@�@��@�D@z�@j@Z@(�@�
@ƨ@��@��@��@C�@"�@�!@�\@M�@-@��@��@hs@X@&�@�`@Ĝ@�9@�@1'@b@�@��@�P@+@��@@�T@��@@@�-@�@p�@`B@?}@V@�@��@�j@�@�D@z�@I�@9X@�@��@�m@�m@�
@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�VA�XA�XA�ZA�XA�XA�\)A�bNA�bNA�dZA�ffA�hsA�hsA�ffA�hsA�hsA�bNA�bNA�hsA�hsA�n�A�r�A�n�A�jA�bNA�`BA�bNA�XA�S�A�S�A�VA�XA�bNA�bNA�hsA�t�A�v�A�v�A�bNA�A�A�  Aĉ7A�+A�\)A�n�A�C�A��
A�M�A�p�A�ffA��A�r�A���A�;dA�7LA��A�"�A���A�VA��DA�M�A���A��A���A�(�A���A�G�A�l�A��A�?}A���A�\)A�A��7A��A�x�A�r�A���A�1'A��`A�VA��A��wA��PA��A��A�1A��A��
A��A�(�A�^5A�{A�"�A��\A�"�A�x�A�n�A��A���A�G�A��A�bA��A���A�$�A�%A�ȴA�I�A�VA�A��DA��9A�|�A�S�A���A�l�A��A|�Ay��AxQ�Av�As��As/As/As33ArI�Ap�An5?AkS�Aj��AjZAix�Ah �Af��Ad5?A`��A_x�A_�A^�yA^v�A]C�A[AYp�AW`BAV9XAUG�AR �AP��AO�#ANĜAM33AGS�AE��AD�+AD(�AC�ACA@bNA<��A<1'A;�mA: �A;�A:�HA:{A9�hA9+A8�jA8=qA6�jA6 �A6  A5\)A2ĜA1l�A0��A0�/A0jA0  A/`BA.��A-A-S�A,�A,JA+�^A+O�A*(�A(5?A'O�A&��A%�A%O�A$�DA#\)A!�wA VA+A9XAp�A�/Az�At�A��A��A�^A��A��A�PA�\AXA��A��A�`A�wA�HA1A=qA�A
-A	�PA�`A9XA��A%A�
Ax�A��A��AȴA7LA b@�$�@���@��@��h@��@�X@�@�^5@�@��
@���@�p�@�Z@��@�+@�I�@�@��@�M�@�p�@�b@ߝ�@�@�^5@��`@���@ڇ+@�p�@ش9@��@֧�@�V@Ұ!@ѩ�@�/@Гu@Ͼw@�K�@��@ΰ!@�~�@�-@�V@�bN@��;@�$�@Ɂ@�(�@�S�@Ƈ+@�-@�@ŉ7@�?}@��/@�b@�@�@�z�@�
=@�O�@��@�j@�(�@�o@���@�O�@�bN@�1@�t�@�o@�M�@��-@�/@��/@�Z@��w@��R@�E�@��@��@���@���@��!@���@�E�@�&�@�r�@�9X@�|�@��@�ff@��#@��@���@�K�@�33@�o@�
=@���@�5?@�`B@���@�Ĝ@���@��D@�z�@�I�@�(�@��@��@���@��@�|�@�dZ@�K�@�;d@�
=@��@�v�@�V@��\@�n�@�E�@���@�p�@�G�@�7L@��`@��9@�I�@���@���@��w@��@���@�S�@��H@�M�@�=q@���@��+@�M�@��@��@��h@�7L@��@���@��@��u@�z�@�1@�t�@���@�~�@�@���@���@��@�hs@�O�@�/@�V@��`@��D@��@��P@��@��y@��!@��+@�=q@��@��-@�&�@��j@�9X@���@��;@���@�^5@��T@��-@��/@�z�@�9X@�  @��
@��@�l�@��P@���@�|�@�l�@�K�@�o@��@�ȴ@���@��\@�n�@�E�@�$�@��^@�`B@�V@�Ĝ@��9@��@�bN@�Q�@�Q�@�A�@�Q�@�r�@�r�@�Z@�9X@��@�C�@�@��y@��y@�ȴ@���@�n�@�^5@�=q@��#@�G�@���@��/@�Ĝ@��@���@��D@�r�@�Q�@�(�@�  @��;@�ƨ@��w@��F@�S�@�o@�n�@�M�@�{@��@��@�-@�@���@��-@��7@�x�@�G�@�G�@�7L@�7L@�&�@��/@���@���@��j@���@�z�@�Q�@�A�@�(�@� �@�  @��w@��@�\)@��H@���@�M�@��@��@���@���@�Ĝ@��D@�9X@��@�@~�y@~��@~��@~E�@~@~5?@}�h@|��@{�m@{dZ@z�H@z�!@z��@z�\@zn�@z^5@z�@yx�@x��@x�9@x��@x�u@x�@xr�@xA�@wl�@v�+@v5?@u`B@t�j@tz�@t9X@s��@sC�@r��@q��@qx�@q&�@p�9@pQ�@p �@o�@o�w@oK�@n��@nE�@n@m�h@m/@l��@l��@l9X@k��@kC�@k33@j~�@jJ@i�@i��@i�7@ihs@i7L@i�@h��@h��@hr�@h1'@g�@g�;@g�P@g+@f��@fȴ@f��@fv�@fE�@f$�@e��@e�h@ep�@d�j@dZ@d�@c"�@b�\@b-@a�7@`��@`�u@`Q�@_��@_l�@_�@^�y@^ȴ@^��@^�+@]�@]�-@]?}@\��@\�@\I�@[��@[C�@[@Z�H@Z��@Z�!@Z~�@Z=q@Y�@Yx�@Y�@Y%@X��@X�`@X��@X1'@W�@W|�@W;d@Vȴ@VE�@U��@Up�@U`B@UO�@U/@T�@T�D@Tz�@TZ@T1@S��@S��@St�@S33@R�@R��@R�@Q��@P��@PĜ@P�u@PbN@P �@O�;@O�w@O��@O�@N��@N�@N��@N5?@M�T@M��@M�h@MO�@MV@L��@L�@L�D@L(�@L1@K�m@K�m@K�F@K��@KdZ@KS�@K33@K"�@K@J�!@J~�@J^5@JM�@J�@I��@I�^@I��@I��@I�@H��@H�9@H�u@Hr�@G�@G+@G
=@F�R@Fv�@F5?@E��@Ep�@EO�@E�@D�j@D��@DZ@C��@CdZ@CC�@Co@B��@B=q@A��@AX@A�@@��@@��@@ �@?�P@>ȴ@>��@>V@=�-@=p�@=O�@=?}@=V@<��@<��@<Z@<�@;�F@;33@:�H@:�!@:M�@:�@9��@9x�@9&�@8��@8�@8  @7�;@7�;@7�w@7l�@6�@6$�@6$�@6@5�@5V@4�/@4��@4�j@4�@4�D@4j@41@3S�@3o@2�H@2�\@2=q@2J@1��@1��@1�7@1hs@1�@0��@0�u@0bN@0A�@0 �@/��@/K�@/;d@/+@/
=@.�y@.ȴ@.��@.��@.�+@.v�@.ff@.E�@.{@-��@-`B@-?}@-V@,�/@,��@,j@,I�@,1@+�
@+C�@+@*�H@*��@*�!@*^5@*-@)�@)��@)��@)hs@)G�@)7L@)�@(��@(��@(�9@(�u@(A�@( �@(  @'�@'�;@'�w@'�P@'K�@';d@'+@'
=@&�y@&ȴ@&��@&ff@&@%�-@%`B@%�@$��@$��@$z�@$I�@$(�@#�m@#C�@#"�@#o@"�H@"�\@"~�@"M�@"�@"J@!��@!�7@!X@!�@ ��@ bN@  �@�;@�@��@l�@+@�@�+@V@{@��@�@p�@p�@?}@�@V@�/@�@�D@j@9X@�m@�F@�@C�@o@@�H@~�@M�@-@�@�7@x�@X@��@�`@Ĝ@��@bN@ �@�@��@�w@�@�P@;d@ȴ@ff@$�@{@@�@�T@�T@��@@�-@�@p�@O�@/@�@�@��@�D@z�@j@Z@(�@�
@ƨ@��@��@��@C�@"�@�!@�\@M�@-@��@��@hs@X@&�@�`@Ĝ@�9@�@1'@b@�@��@�P@+@��@@�T@��@@@�-@�@p�@`B@?}@V@�@��@�j@�@�D@z�@I�@9X@�@��@�m@�m@�
@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B|�B{�B{�B{�B|�B|�B|�B|�B|�B|�B|�B|�B}�B|�B|�B|�B|�B|�B|�B|�B� B�B�B�%B�1B�1B�1B�1B�1B�1B�1B�=B�VB�VB�bB��B��B�B�-B�^BƨB�#B�yB��BPB�B�B �B�B-Bm�Bt�B�B�7B�PB�bB�bB�bB�hB�bB�\B�bB�\B�DB�DB�DB�DB�%B�B�B�B�B�B�B{�Bw�Br�BhsB_;B\)BVBR�BN�BB�B5?B"�B�B	7B%B��B��B�sB�B��BÖB�jB�B��B�oB�VB�B�B� B|�Bq�BE�B,B&�B�BJB
�HB
�dB
�{B
y�B
e`B
W
B
P�B
B�B
"�B	��B	�B	�BB	�B	�B	�B	�#B	�#B	��B	�dB	�B	�B	�!B	�B	��B	��B	~�B	ffB	\)B	^5B	_;B	bNB	[#B	L�B	@�B	6FB	)�B	&�B	�B	VB	DB	+B	%B��BĜBȴB��B�B�
B��B�'B��B�B��B�XBÖBÖB��B�}B�jB�^B�?B�-B�'B�B��B��B��B��B��B��B�uB�hB�JB�DB�+B�B�B�B�B{�Bu�Bu�Bq�Bp�Bn�Bl�BffBcTB_;B]/B\)BYBXBVBS�BM�BM�BJ�BG�BF�BE�BC�BB�B@�B=qB:^B8RB5?B6FB2-B1'B/B/B.B,B-B)�B(�B(�B%�B%�B%�B#�B"�B"�B �B�B�B �B�B�B�B�B�B�B�B�B!�B!�B �B!�B!�B!�B �B �B!�B"�B%�B%�B%�B&�B%�B%�B%�B&�B'�B'�B'�B)�B+B+B-B-B.B/B1'B33B49B6FB6FB:^B@�BB�BD�BG�BH�BI�BJ�BK�BL�BM�BP�BS�BVBYBZB[#B^5B_;B`BBbNBbNBdZBe`BgmBl�Bm�Bo�Bt�Bu�By�B{�B~�B� B~�B�B�%B�1B�VB�oB��B��B��B��B��B��B��B�B�!B�!B�'B�'B�3B�LB�}BBĜBƨBǮBǮBɺB��B��B��B��B��B�B�
B�B�#B�/B�/B�/B�BB�NB�`B�sB�B�B�B��B��B��B	B	B	%B	%B	+B	+B	DB	bB	uB	�B	�B	�B	�B	!�B	"�B	$�B	'�B	)�B	+B	,B	-B	.B	1'B	49B	7LB	;dB	>wB	@�B	A�B	D�B	E�B	F�B	F�B	G�B	I�B	K�B	O�B	R�B	T�B	VB	^5B	aHB	cTB	cTB	e`B	hsB	hsB	gmB	p�B	r�B	x�B	w�B	v�B	v�B	y�B	|�B	}�B	~�B	� B	�B	�B	�B	�+B	�1B	�1B	�7B	�=B	�DB	�VB	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�9B	�?B	�FB	�FB	�?B	�?B	�LB	�RB	�XB	�^B	�jB	�wB	�}B	��B	��B	��B	ÖB	ĜB	ŢB	ƨB	ƨB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�)B	�5B	�BB	�HB	�TB	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
	7B
	7B
	7B

=B

=B

=B
DB
DB
JB
JB
JB
PB
VB
VB
VB
\B
\B
bB
hB
hB
hB
hB
hB
oB
oB
oB
oB
oB
uB
uB
uB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
+B
+B
+B
+B
,B
,B
,B
,B
-B
-B
-B
-B
.B
.B
.B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
XB
XB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
e`B
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B|�B{�B{�B{�B|�B|�B|�B|�B|�B|�B|�B|�B}�B|�B|�B|�B|�B|�B|�B|�B�B��B��B��B��B��B��B��B��B��B��B�	B�"B�"B�.B�qB��B��B��B�*B�tB��B�DB��BB_B~B �BkB,�Bm]Bt�B��B�B�B�.B�.B�.B�4B�.B�(B�.B�(B�B�B�B�B��B��B��B��B��B��B��B{�Bw�Br|Bh>B_B[�BU�BR�BN�BB[B5B"�BMB	B�B��B�B�>B��B˒B�aB�B��B�_B�:B�"B��B��B�B|�BqvBEmB+�B&�B�BB
�B
�0B
�FB
y�B
e,B
V�B
P�B
B[B
"�B	��B	�UB	��B	��B	��B	��B	��B	��B	ЗB	�B	��B	��B	��B	��B	��B	�MB	~�B	f2B	[�B	]�B	^�B	a�B	Z�B	L~B	@OB	6B	)�B	&�B	eB	"B	
�B	�B	�B��B�MB�fBЗB��B��B˒B��B��B��B��B�	B�GB�GB�UB�.B�6B�B��B��B��B��B��B�pB�dB�]B�QB�EB�&B�B��B��B��B��B��B��B��B{�ButButBq[BpUBncBlWBfBcB^�B\�B[�BX�BW�BU�BS�BM�BM�BJ�BGzBFtBEmBCGBBAB@4B="B:B8B4�B5�B1�B0�B.�B.�B-�B+�B,�B)�B(�B(�B%�B%�B%�B#�B"�B"�B �BpBjB vBdBjB~BjB~BdBjBdB!�B!|B �B!|B!|B!|B vB �B!|B"�B%�B%�B%�B&�B%�B%�B%�B&�B'�B'�B'�B)�B*�B*�B,�B,�B-�B.�B0�B2�B3�B5�B5�B:B@4BBABDMBGzBH�BIlBJrBKxBL~BM�BP�BS�BU�BX�BY�BZ�B]�B_B_�Ba�BbBd&BeBgBl=BmCBoOBtnBu�By�B{�B~�B�B~�B��B��B��B�B� B�9B�?B�WB�dB�pB�|B��B��B��B��B��B��B��B��B�.B�AB�MB�YB�zB�EB�lB�rB�~BЗBөB��BյBּB��B��B��B��B��B��B��B�B�$B�0B�IB�hB�tB��B��B	 �B	�B	�B	�B	�B	�B	B	B	&B	9B	QB	dB	pB	!|B	"�B	$�B	'�B	)�B	*�B	+�B	,�B	-�B	0�B	3�B	6�B	;B	>(B	@4B	A;B	DMB	ESB	FYB	FtB	G_B	IlB	KxB	O�B	R�B	T�B	U�B	]�B	`�B	cB	cB	eB	h
B	h$B	gB	p;B	raB	x�B	w�B	v`B	vzB	y�B	|�B	}�B	~�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�&B	�&B	�2B	�9B	�WB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�(B	�HB	�4B	�B	�;B	�GB	�MB	�SB	�YB	�?B	�_B	�fB	�lB	ɆB	�xB	̈́B	ΊB	ΊB	ϑB	ϑB	ѝB	ϑB	ϫB	ϑB	ϑB	ϫB	ѝB	ңB	��B	ԕB	ԯB	յB	յB	ּB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�0B	�CB	�CB	�0B	�0B	�0B	�0B	�0B	�6B	�CB	�IB	�iB	�hB	�nB	�tB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B

�B

�B
�B
�B
�B
�B
B
�B
B
�B
B
B
B
B
B
B
B
 B
 B
 B
 B
 B
&B
&B
&B
&B
,B
,B
,B
,B
,B
2B
2B
B
2B
B
9B
9B
$B
EB
EB
KB
QB
QB
QB
WB
WB
]B
xB
xB
]B
]B
]B
~B
dB
jB
jB
OB
jB
pB
VB
 vB
 vB
 \B
 vB
 vB
 \B
!|B
!|B
!|B
!|B
!|B
!bB
"�B
"�B
#nB
#�B
#nB
$�B
$�B
%�B
%zB
%�B
%�B
%zB
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
*�B
*�B
*�B
*�B
+�B
+�B
+�B
+�B
,�B
,�B
,�B
,�B
-�B
-�B
-�B
.�B
.�B
.�B
.�B
.�B
/�B
/�B
/�B
/�B
/�B
0�B
0�B
0�B
0�B
0�B
0�B
0�B
0�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
2�B
2�B
2�B
2�B
2�B
3�B
3�B
3�B
5B
4�B
5�B
5�B
6B
5�B
5�B
6�B
6�B
6�B
8B
8B
8B
8B
9	B
9	B
8�B
:B
:*B
:B
;B
;B
<B
<B
<B
=B
="B
>(B
>(B
>(B
>(B
>(B
>(B
>(B
?.B
?.B
?.B
@OB
@OB
@4B
AUB
A B
A B
A;B
A;B
BAB
BAB
BAB
BAB
CGB
CGB
DMB
DgB
DMB
DgB
ESB
ESB
ESB
E9B
ESB
EmB
ESB
ESB
FYB
FYB
FYB
GEB
G_B
GEB
GEB
HfB
HfB
HKB
HfB
HfB
IlB
IRB
IRB
IlB
IlB
JrB
JrB
JrB
JrB
JrB
JrB
KxB
KxB
KxB
KxB
K�B
KxB
KxB
KxB
L~B
L~B
L~B
L~B
L~B
MjB
MjB
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]�B
]�B
]�B
^B
]�B
^�B
^�B
^�B
^�B
_�B
_�B
_�B
_�B
_�B
_�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
bB
a�B
a�B
cB
cB
cB
cB
cB
cB
cB
cB
cB
cB
dB
d&B
dB
dB
dB
d�B
eB
eB
e,B
eB
eB
eB
eB
eB
fB
eB
fB
fB
e�B
f2B
gB
gB
gB
gB
h$B
h$B
h$B
h$B
i*B
i*B
i*B
iDB
i*B
i*B
i*B
j0B
j0B
j0B
k6B
k6B
l"B
l=B
l=B
l=B
lWB
l"B
l=B
l=B
l=B
l=B
m)B
mCB
mCB
mCB
mCB
mCB
m]B
mCB
m]B
nIB
nIB
nIB
nIB
nIB
nI111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.6(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201812310036402018123100364020181231003640201901010029002019010100290020190101002900JA  ARFMdecpA19c                                                                20181226183641  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181226093650  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181226093651  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181226093652  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181226093652  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181226093652  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181226093652  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181226093652  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181226093653  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181226093653                      G�O�G�O�G�O�                JA  ARUP                                                                        20181226095848                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181226154111  CV  JULD            G�O�G�O�F��y                JM  ARCAJMQC2.0                                                                 20181230153640  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181230153640  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181231152900  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021520                      G�O�G�O�G�O�                