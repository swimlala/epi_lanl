CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-04-25T00:36:40Z creation;2019-04-25T00:36:44Z conversion to V3.1;2019-12-23T06:03:19Z update;     
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
resolution        =���   axis      Z        d  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  s\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ۨ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �L   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �\   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �`   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �t   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �|Argo profile    3.1 1.2 19500101000000  20190425003640  20200120031517  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_141                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @ظ�~ 1   @ظ�`��@8D?���cn��P1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D3��D4� D5  D5� D6fD6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di�fDj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dx��Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D��3D��3D�3D�,�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @Y��@�33@�33A	��A)��AI��Ai��A���A���A���A���A���A���A���A���BffB
ffBffBffB"ffB*ffB2ffB:ffBBffBJffBRffBZffBbffBjffBrffBzffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct� Cv��Cx��Cz��C|��C~��C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�D &fD �fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD	&fD	�fD
&fD
�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD &fD �fD!&fD!�fD"&fD"�fD#&fD#�fD$&fD$�fD%&fD%�fD&&fD&�fD'&fD'�fD(&fD(�fD)&fD)�fD*&fD*�fD+&fD+�fD,&fD,�fD-&fD-�fD.&fD.�fD/&fD/�fD0&fD0�fD1&fD1�fD2&fD2�fD3&fD3�fD4  D4�fD5&fD5�fD6,�D6�fD7&fD7�fD8&fD8�fD9&fD9�fD:&fD:�fD;&fD;�fD<&fD<�fD=&fD=�fD>&fD>�fD?&fD?�fD@&fD@�fDA&fDA�fDB&fDB�fDC&fDC�fDD&fDD�fDE&fDE�fDF&fDF�fDG&fDG�fDH&fDH�fDI&fDI�fDJ&fDJ�fDK&fDK�fDL&fDL�fDM&fDM�fDN&fDN�fDO&fDO�fDP&fDP�fDQ&fDQ�fDR&fDR�fDS&fDS�fDT&fDT�fDU&fDU�fDV&fDV�fDW&fDW�fDX&fDX�fDY&fDY�fDZ&fDZ�fD[&fD[�fD\&fD\�fD]&fD]�fD^&fD^�fD_&fD_�fD`&fD`�fDa&fDa�fDb&fDb�fDc&fDc�fDd&fDd�fDe&fDe�fDf&fDf�fDg&fDg�fDh&fDh�fDi&fDi��Dj&fDj�fDk&fDk�fDl&fDl�fDm&fDm�fDn&fDn�fDo&fDo�fDp&fDp�fDq&fDq�fDr&fDr�fDs&fDs�fDt&fDt�fDu&fDu�fDv&fDv�fDw&fDw�fDx&fDx�fDy  Dy�fDz&fDz�fD{&fD{�fD|&fD|�fD}&fD}�fD~&fD~�fD&fD�fD�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D� D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�P D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D�� D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D3D��3D�3D�S3DÓ3D��3D�3D�S3Dē3D��3D�3D�S3Dœ3D��3D�3D�S3DƓ3D��3D�3D�S3DǓ3D��3D�3D�S3Dȓ3D��3D�3D�S3Dɓ3D��3D�3D�S3Dʓ3D��3D�3D�S3D˓3D��3D�3D�S3D̓3D��3D�3D�S3D͓3D��3D�3D�S3DΓ3D��3D�3D�S3Dϓ3D��3D�3D�S3DГ3D��3D�3D�S3Dѓ3D��3D�3D�S3Dғ3D��3D�3D�S3Dӓ3D��3D�3D�S3Dԓ3D��3D�3D�S3DՓ3D��3D�3D�S3D֓3D��3D�3D�S3Dד3D��3D�3D�S3Dؓ3D��3D�3D�S3Dٓ3D��3D�3D�S3Dړ3D��3D�3D�S3Dۓ3D��3D�3D�S3Dܓ3D��3D�3D�S3Dݓ3D��3D�3D�S3Dޓ3D��3D�3D�S3Dߓ3D��3D�3D�S3D��3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��fD�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D��3D��3D�3D�S3D��fD��fD�fD�@ 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�Q�A�K�A�O�A�ZA�K�A�=qA�;dA�9XA�5?A�33A�1'A�/A�-A�+A�+A�(�A��A�
=A�%A�  A���A���A��A��A��A��yA��mA��TA��HA��#A���A��RA���A���A��uA�Q�A���A�v�A�M�A��A�
=A��A��yA���A�^5A�K�A�(�A��`A�v�A�JA��#A���A��A�%A���A���A�M�A��A��A�A�|�A�XA�+A��A��TA���A��9A�^5A�"�A�t�A��7A�G�A��FA�t�A�7LA�~�A���A��+A��A�5?A���A�33A��HA��RA��A��A��!A���A���A�"�A���A���A�z�A�A��A���A���A�;dA��7A�=qA�{A�ZA��
A�r�A���A�A�r�A���A��!A�n�A��A���A��!A�ĜA�jA�5?A��A��A�(�A�z�A���A�(�A~��A}�A{�Az��Ay��Ax��AvI�Au
=Ar�Am�
Ai�;Ah$�Af=qAd�Aa�;A`ZA^�!A]\)A\JAZȴAZA�AY�AY�AYoAX=qAW`BAVE�AT��AS�AR5?AO+AN(�AKXAJ9XAG�^AE�TAC��AB^5A@�`A?�A?�A>�!A> �A=/A;�#A:A�A8�jA85?A8  A7p�A6~�A5x�A4~�A3��A2�yA21'A0ffA.n�A.bNA.1'A-��A,��A*��A)%A(I�A'�A'��A&��A#�;A#��A#oA"JA �RA jA�mA�`A�TA��A~�A��Az�AZAA�hAp�A%AQ�A�yA��A�mAA��A�At�AO�A�A�
A�^A��A	��A��AZA(�A;dA�+A�A��A�A�AA�A��@��@�/@�S�@�ȴ@�E�@�J@�X@�I�@��H@��h@��j@�@��@�^5@���@�p�@��@�O�@�K�@�^5@�O�@�b@�"�@�7L@���@�V@�@�t�@���@�Ĝ@�o@�Q�@�"�@��@ָR@��#@�1'@� �@�ƨ@���@��T@с@�X@��@�S�@��#@���@̴9@��@�dZ@ʸR@�J@��`@ǶF@��m@+@�ff@�ff@���@��@�;d@�"�@���@���@���@�n�@�ff@�ff@�E�@��@��
@��@��@�I�@��@���@�ff@�@��@�X@�7L@���@��@�j@��@��P@�l�@�C�@�
=@��R@��+@�M�@�-@���@��9@�C�@�M�@��@��@��-@�?}@���@�;d@�E�@���@���@�X@���@��9@� �@�ƨ@��P@�|�@� �@�b@��m@�V@�Z@�+@��#@��@�Z@���@�"�@���@���@�~�@�$�@���@�&�@�j@�1@��m@��F@���@��P@��@�l�@�K�@��@��!@�v�@��@���@�X@�&�@�7L@�/@�7L@�G�@�X@�?}@��@��`@��@�/@���@�r�@�A�@�A�@�I�@�I�@�Z@�r�@�j@���@�dZ@�C�@��@��!@�n�@�ff@�=q@�$�@���@��^@�`B@�?}@��@��u@�bN@�I�@� �@��m@�dZ@�ff@���@�x�@�hs@�O�@�7L@��@��`@��j@�r�@�(�@��m@��P@�33@�
=@��R@�=q@��@��7@�p�@�X@�?}@�7L@��@��9@��@�bN@�A�@�b@��@���@��P@�S�@�+@�"�@��@�
=@���@���@���@�ȴ@��R@���@�~�@�ff@�^5@�M�@�E�@�5?@���@���@���@��h@�`B@��h@���@��@�X@�?}@�/@���@��u@�j@�(�@��m@��;@���@���@�S�@�33@�
=@���@�-@��@��^@���@��7@�O�@�/@�/@�&�@��@��@���@��9@��u@�1@��
@��
@��F@���@���@��@�|�@�C�@�
=@��R@�~�@�M�@�-@��@��T@���@��@�X@���@���@��/@��9@��@�1'@�(�@l�@;d@~ȴ@~E�@~5?@~5?@}@}`B@}�@}`B@|�@|��@|9X@{33@z^5@y��@y%@xQ�@x1'@x1'@xr�@w��@w;d@w\)@wK�@vv�@v{@u@u�@t�@t�j@t�D@s�F@rM�@q�7@o�@o\)@nv�@n��@nE�@nv�@nE�@m@m��@mp�@m/@l�@l�@l�@k��@k"�@k@j��@j^5@j-@i��@i7L@iG�@iX@iX@iX@i&�@h�@h  @g�P@f��@f5?@e��@e�-@e�-@e@e�h@e�h@ep�@eO�@eV@d�/@d�@dI�@d�@c��@cƨ@c"�@b��@a��@ahs@a%@`�@`bN@`A�@_�@^��@^{@]p�@\��@\�D@[�m@[ƨ@[t�@ZM�@Y��@Yx�@Y%@XbN@XbN@XĜ@X�`@X�`@X��@X�u@Xb@W�P@W�@V��@Vȴ@V��@V�+@VV@V5?@V{@V@Up�@U?}@U/@UV@T�/@T��@T1@S��@SdZ@R�@R��@R=q@Q�^@Q��@Q��@Q�7@Q�@PĜ@P�u@PQ�@O�w@O|�@O+@N��@NE�@M��@M�@Mp�@MO�@M?}@L��@L�/@L��@L9X@L(�@L1@K��@K�m@Kƨ@K�F@K�F@K��@K33@JJ@I�^@I�7@I&�@H�u@H �@Hb@H  @G��@G�@Fȴ@F��@F5?@E`B@D�/@D��@D9X@CdZ@BM�@BJ@A�^@AX@A�@@��@@��@@�9@@�@@r�@@bN@@ �@@  @?�;@?�;@?��@?�@?\)@>E�@=p�@=�@<�@<�j@<�@<�D@<�D@<z�@<Z@;�
@;dZ@:�H@:�!@:^5@:=q@:J@9��@9�7@97L@8��@8��@81'@7�;@7�w@7��@7|�@7l�@7\)@7;d@7�@6�@6��@6�+@6V@6@5��@5�@5`B@5?}@5?}@4��@4�j@4z�@4(�@3��@3"�@2�@2�@2�H@2��@2^5@2M�@2J@1��@1&�@0��@0�9@0��@0r�@0Q�@0b@/�w@/|�@.��@.�R@.��@.��@.�+@.v�@.ff@.{@-@-p�@,��@,�D@,Z@,I�@,I�@,(�@+��@+�
@+��@+dZ@+33@+o@*��@*n�@*M�@*-@*J@)��@)hs@)%@(�`@(�9@(1'@(  @'�@'�w@'�@'�P@'
=@&v�@&$�@%��@%�h@%?}@%?}@$��@$��@$�@$�j@$��@$Z@$1@#�m@#ƨ@#t�@#33@#o@#"�@"�@"�!@"~�@"=q@"-@!�#@!x�@!X@!&�@ �`@ �9@ �9@ ��@ ��@ �@  �@   @�@�@�@�P@;d@
=@ȴ@5?@@�h@p�@`B@/@��@z�@(�@�m@��@"�@�!@��@~�@=q@J@�@�@��@X@&�@��@�@r�@1'@��@�w@�P@;d@�@��@ȴ@��@v�@V@5?@{@�T@@�h@O�@�@��@�@��@�@�D@j@(�@�m@��@t�@dZ@dZ@o@�!@��@^5@-@�@��@�@�#@�^@hs@G�@�@��@Ĝ@��@�u@�@Q�@b@�@�;@��@�@|�@�@��@�R@��@v�@V@5?@��@��@`B@/@�@�@��@��@�@�@z�@Z@(�@1@ƨ@��@t�@S�@"�@
�@
��@
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�Q�A�K�A�O�A�ZA�K�A�=qA�;dA�9XA�5?A�33A�1'A�/A�-A�+A�+A�(�A��A�
=A�%A�  A���A���A��A��A��A��yA��mA��TA��HA��#A���A��RA���A���A��uA�Q�A���A�v�A�M�A��A�
=A��A��yA���A�^5A�K�A�(�A��`A�v�A�JA��#A���A��A�%A���A���A�M�A��A��A�A�|�A�XA�+A��A��TA���A��9A�^5A�"�A�t�A��7A�G�A��FA�t�A�7LA�~�A���A��+A��A�5?A���A�33A��HA��RA��A��A��!A���A���A�"�A���A���A�z�A�A��A���A���A�;dA��7A�=qA�{A�ZA��
A�r�A���A�A�r�A���A��!A�n�A��A���A��!A�ĜA�jA�5?A��A��A�(�A�z�A���A�(�A~��A}�A{�Az��Ay��Ax��AvI�Au
=Ar�Am�
Ai�;Ah$�Af=qAd�Aa�;A`ZA^�!A]\)A\JAZȴAZA�AY�AY�AYoAX=qAW`BAVE�AT��AS�AR5?AO+AN(�AKXAJ9XAG�^AE�TAC��AB^5A@�`A?�A?�A>�!A> �A=/A;�#A:A�A8�jA85?A8  A7p�A6~�A5x�A4~�A3��A2�yA21'A0ffA.n�A.bNA.1'A-��A,��A*��A)%A(I�A'�A'��A&��A#�;A#��A#oA"JA �RA jA�mA�`A�TA��A~�A��Az�AZAA�hAp�A%AQ�A�yA��A�mAA��A�At�AO�A�A�
A�^A��A	��A��AZA(�A;dA�+A�A��A�A�AA�A��@��@�/@�S�@�ȴ@�E�@�J@�X@�I�@��H@��h@��j@�@��@�^5@���@�p�@��@�O�@�K�@�^5@�O�@�b@�"�@�7L@���@�V@�@�t�@���@�Ĝ@�o@�Q�@�"�@��@ָR@��#@�1'@� �@�ƨ@���@��T@с@�X@��@�S�@��#@���@̴9@��@�dZ@ʸR@�J@��`@ǶF@��m@+@�ff@�ff@���@��@�;d@�"�@���@���@���@�n�@�ff@�ff@�E�@��@��
@��@��@�I�@��@���@�ff@�@��@�X@�7L@���@��@�j@��@��P@�l�@�C�@�
=@��R@��+@�M�@�-@���@��9@�C�@�M�@��@��@��-@�?}@���@�;d@�E�@���@���@�X@���@��9@� �@�ƨ@��P@�|�@� �@�b@��m@�V@�Z@�+@��#@��@�Z@���@�"�@���@���@�~�@�$�@���@�&�@�j@�1@��m@��F@���@��P@��@�l�@�K�@��@��!@�v�@��@���@�X@�&�@�7L@�/@�7L@�G�@�X@�?}@��@��`@��@�/@���@�r�@�A�@�A�@�I�@�I�@�Z@�r�@�j@���@�dZ@�C�@��@��!@�n�@�ff@�=q@�$�@���@��^@�`B@�?}@��@��u@�bN@�I�@� �@��m@�dZ@�ff@���@�x�@�hs@�O�@�7L@��@��`@��j@�r�@�(�@��m@��P@�33@�
=@��R@�=q@��@��7@�p�@�X@�?}@�7L@��@��9@��@�bN@�A�@�b@��@���@��P@�S�@�+@�"�@��@�
=@���@���@���@�ȴ@��R@���@�~�@�ff@�^5@�M�@�E�@�5?@���@���@���@��h@�`B@��h@���@��@�X@�?}@�/@���@��u@�j@�(�@��m@��;@���@���@�S�@�33@�
=@���@�-@��@��^@���@��7@�O�@�/@�/@�&�@��@��@���@��9@��u@�1@��
@��
@��F@���@���@��@�|�@�C�@�
=@��R@�~�@�M�@�-@��@��T@���@��@�X@���@���@��/@��9@��@�1'@�(�@l�@;d@~ȴ@~E�@~5?@~5?@}@}`B@}�@}`B@|�@|��@|9X@{33@z^5@y��@y%@xQ�@x1'@x1'@xr�@w��@w;d@w\)@wK�@vv�@v{@u@u�@t�@t�j@t�D@s�F@rM�@q�7@o�@o\)@nv�@n��@nE�@nv�@nE�@m@m��@mp�@m/@l�@l�@l�@k��@k"�@k@j��@j^5@j-@i��@i7L@iG�@iX@iX@iX@i&�@h�@h  @g�P@f��@f5?@e��@e�-@e�-@e@e�h@e�h@ep�@eO�@eV@d�/@d�@dI�@d�@c��@cƨ@c"�@b��@a��@ahs@a%@`�@`bN@`A�@_�@^��@^{@]p�@\��@\�D@[�m@[ƨ@[t�@ZM�@Y��@Yx�@Y%@XbN@XbN@XĜ@X�`@X�`@X��@X�u@Xb@W�P@W�@V��@Vȴ@V��@V�+@VV@V5?@V{@V@Up�@U?}@U/@UV@T�/@T��@T1@S��@SdZ@R�@R��@R=q@Q�^@Q��@Q��@Q�7@Q�@PĜ@P�u@PQ�@O�w@O|�@O+@N��@NE�@M��@M�@Mp�@MO�@M?}@L��@L�/@L��@L9X@L(�@L1@K��@K�m@Kƨ@K�F@K�F@K��@K33@JJ@I�^@I�7@I&�@H�u@H �@Hb@H  @G��@G�@Fȴ@F��@F5?@E`B@D�/@D��@D9X@CdZ@BM�@BJ@A�^@AX@A�@@��@@��@@�9@@�@@r�@@bN@@ �@@  @?�;@?�;@?��@?�@?\)@>E�@=p�@=�@<�@<�j@<�@<�D@<�D@<z�@<Z@;�
@;dZ@:�H@:�!@:^5@:=q@:J@9��@9�7@97L@8��@8��@81'@7�;@7�w@7��@7|�@7l�@7\)@7;d@7�@6�@6��@6�+@6V@6@5��@5�@5`B@5?}@5?}@4��@4�j@4z�@4(�@3��@3"�@2�@2�@2�H@2��@2^5@2M�@2J@1��@1&�@0��@0�9@0��@0r�@0Q�@0b@/�w@/|�@.��@.�R@.��@.��@.�+@.v�@.ff@.{@-@-p�@,��@,�D@,Z@,I�@,I�@,(�@+��@+�
@+��@+dZ@+33@+o@*��@*n�@*M�@*-@*J@)��@)hs@)%@(�`@(�9@(1'@(  @'�@'�w@'�@'�P@'
=@&v�@&$�@%��@%�h@%?}@%?}@$��@$��@$�@$�j@$��@$Z@$1@#�m@#ƨ@#t�@#33@#o@#"�@"�@"�!@"~�@"=q@"-@!�#@!x�@!X@!&�@ �`@ �9@ �9@ ��@ ��@ �@  �@   @�@�@�@�P@;d@
=@ȴ@5?@@�h@p�@`B@/@��@z�@(�@�m@��@"�@�!@��@~�@=q@J@�@�@��@X@&�@��@�@r�@1'@��@�w@�P@;d@�@��@ȴ@��@v�@V@5?@{@�T@@�h@O�@�@��@�@��@�@�D@j@(�@�m@��@t�@dZ@dZ@o@�!@��@^5@-@�@��@�@�#@�^@hs@G�@�@��@Ĝ@��@�u@�@Q�@b@�@�;@��@�@|�@�@��@�R@��@v�@V@5?@��@��@`B@/@�@�@��@��@�@�@z�@Z@(�@1@ƨ@��@t�@S�@"�@
�@
��@
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
� B
� B
}�B
|�B
|�B
�B
�B
�DB
�oB
��B
��B
��B
�XB
��B
�B
�mB
�BbB,B;dBW
Bw�B�oB��B�3B��B�B�`B�mB�B�B�B�B�B�B�B��B��B��BDBB��B�mB�
B��B��BǮB�XB�FB�9B�!B�B�B��B��B��B��B�B�wB�wB�jB�^B��B�yB��B��B�B�/B��B�uBk�B\)BB�B�B
��B
�qB
�-B
�B
��B
��B
�uB
�%B
� B
�B
}�B
s�B
]/B
XB
O�B
F�B
=qB
5?B
+B
$�B
 �B
�B
!�B
uB
+B	��B	�;B	ÖB	�^B	�'B	��B	��B	�\B	�1B	�B	~�B	u�B	r�B	p�B	m�B	k�B	ffB	aHB	]/B	VB	O�B	E�B	1'B	+B	�B	{B	
=B��B��B�B�TB�;B�#B�B��B��B��BƨB��B�}B�}B�qB�^B�LB�3B�!B�B��B��B��B��B��B��B��B�hB�+B�B�B�B~�Bu�Bs�Bq�Bo�Bk�BiyBgmBdZBbNB_;B]/B[#BYBR�BQ�BP�BO�BN�BK�BI�BH�BB�BA�BA�BA�B@�B@�B>wB<jB8RB5?B2-B.B-B-B-B+B+B)�B(�B&�B$�B%�B �B!�B"�B"�B"�B!�B"�B#�B$�B%�B&�B'�B'�B)�B(�B)�B.B2-B2-B2-B2-B2-B2-B49B5?B49B5?B7LB7LB<jB?}BH�BJ�BK�BK�BM�BP�BP�BQ�BS�BT�BS�BS�BS�BXBZB[#B[#B\)B^5B^5B^5B_;B_;B`BB`BBaHBbNBe`BiyBiyBiyBiyBiyBiyBk�Bk�Bk�Bk�Bl�Bo�Bv�Bu�Bw�By�By�B~�B�%B�+B�1B�7B�DB�JB�PB�hB�oB�uB�uB�{B��B��B��B�B�B�!B�9B�jB�qB�qB�qB�qB�}B�qB��BĜBĜBǮB��B��B�
B�B�5B�`B�B�B�B�B�B�B�B�B��B	B	B	B	B	B	%B	1B		7B	\B	hB	oB	uB	{B	{B	�B	�B	�B	�B	�B	!�B	"�B	%�B	(�B	-B	0!B	0!B	1'B	1'B	6FB	8RB	8RB	8RB	@�B	D�B	D�B	E�B	K�B	P�B	R�B	R�B	S�B	VB	XB	]/B	`BB	`BB	bNB	cTB	e`B	ffB	gmB	gmB	hsB	hsB	iyB	jB	n�B	r�B	q�B	r�B	s�B	s�B	s�B	s�B	t�B	y�B	z�B	|�B	~�B	�B	�B	�B	�+B	�=B	�VB	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�9B	�?B	�?B	�FB	�FB	�FB	�RB	�XB	�XB	�^B	�dB	�jB	�qB	�qB	�wB	�}B	�}B	��B	ÖB	ĜB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�/B	�/B	�5B	�BB	�HB	�HB	�NB	�NB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�`B	�fB	�fB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
%B
%B
+B
+B
+B
1B
1B
	7B
	7B

=B
DB
DB
DB
DB
DB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
PB
VB
VB
\B
bB
hB
hB
hB
oB
uB
oB
oB
oB
uB
uB
{B
{B
{B
{B
{B
{B
uB
oB
hB
hB
bB
oB
oB
oB
hB
hB
hB
hB
hB
hB
hB
uB
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
#�B
$�B
$�B
%�B
$�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
,B
,B
,B
,B
,B
,B
,B
,B
-B
-B
-B
.B
.B
.B
/B
/B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
33B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
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
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
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
J�B
J�B
J�B
J�B
J�B
K�B
K�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
I�B
J�B
K�B
K�B
K�B
L�B
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
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
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
R�B
R�B
R�B
R�B
R�B
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
VB
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
YB
YB
YB
YB
YB
ZB
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
\)B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
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
bNB
bNB
bNB
bNB
bNB
bNB
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
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
iy1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
}�B
|�B
|�B
��B
��B
�B
�:B
�MB
�~B
��B
�$B
ѷB
��B
�8B
�oB.B+�B;0BV�Bw�B�:B��B��B̘B��B�,B�8B�KB�QB�KB�QB�B�WB�oB��B��B��BB�B��B�8B��BѷBҽB�zB�$B�B�B��B��B��B��B��B��B��B��B�BB�BB�6B�*B̘B�DB��B�B�=B��B�OB�@BkQB[�BB[BeB
��B
�<B
��B
��B
��B
��B
�&B
��B
�B
��B
}�B
s�B
\�B
W�B
O�B
FtB
="B
5B
*�B
$�B
 �B
�B
!�B
@B
�B	��B	�B	�GB	�B	��B	��B	�KB	�B	��B	��B	~�B	utB	r|B	poB	m]B	k6B	f2B	aB	\�B	U�B	O�B	EmB	0�B	*�B	xB	,B	
	B��B�B�KB�B��B��B��BԯBЗBʌB�YB�UB�.B�.B�"B�*B��B��B��B��B��B��B�]B�]B�qB�KB�SB�B��B��B��B��B~�Bu�Bs�Bq[BoOBkQBi*Bg8BdBa�B_B\�BZ�BX�BR�BQ�BP�BO�BN�BK�BIlBH�BBABAUBAUBA;B@4B@4B>BB<6B8B4�B1�B-�B,�B,�B,�B*�B*�B)�B(�B&�B$�B%�B vB!|B"�B"�B"�B!|B"�B#�B$�B%�B&�B'�B'�B)�B(�B)�B-�B1�B1�B1�B1�B1�B1�B4B4�B3�B4�B6�B7B<B?.BH�BJrBKxBK�BM�BP�BP�BQ�BS�BT�BS�BS�BS�BW�BY�BZ�BZ�B[�B]�B]�B]�B_B^�B_�B_�B`�Ba�BeBiDBi*BiDBi*Bi*Bi*Bk6Bk6Bk6Bk6Bl=BoOBvzBu�Bw�By�By�B~�B��B��B��B��B��B�B�B�B� B�@B�&B�,B�?B�WB�jB��B��B��B��B�B�"B�"B�"B�"B�HB�"B�;B�MB�MB�_B�~BңBּB��B�B�B�hB�hB�aB�UB�IB�[B�AB�hB��B	 �B	�B	�B	�B	�B	�B	�B	�B	B	B	 B	&B	,B	,B	2B	2B	9B	QB	pB	!|B	"�B	%�B	(�B	,�B	/�B	/�B	0�B	0�B	5�B	8B	8B	8B	@4B	DMB	DMB	E9B	KxB	P�B	R�B	R�B	S�B	U�B	W�B	\�B	_�B	_�B	a�B	cB	d�B	fB	gB	gB	h$B	h$B	i*B	j0B	nIB	rGB	q[B	raB	shB	shB	shB	sMB	tnB	y�B	z�B	|�B	~�B	��B	��B	��B	��B	��B	�B	�B	�B	� B	�MB	�EB	�QB	�dB	�jB	�pB	�vB	�\B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�	B	�B	�B	�B	�B	�"B	�(B	�.B	�.B	�;B	�GB	�MB	�SB	�_B	�xB	̈́B	̈́B	ΊB	ϫB	ϑB	ѝB	өB	ԯB	ּB	��B	��B	��B	��B	ںB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�$B	�*B	�*B	�0B	�0B	�KB	�6B	�6B	�=B	�=B	�CB	�)B	�IB	�cB	�IB	�IB	�/B	�IB	�iB	�IB	�AB	�[B	�[B	�[B	�UB	�[B	�aB	�nB	�zB	�tB	�zB	��B	�zB	�zB	�zB	��B	��B	��B	��B	��B	�zB	��B	�zB	�tB	�zB	�zB	��B	��B	��B	��B	��B	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
B
�B
�B
�B
�B
�B
�B
B
�B
B
B
B
B
 B
B
B
 B
&B
 B
B
B
&B
@B
,B
,B
B
B
,B
,B
&B
 B
B
B
�B
 B
 B
 B
B
B
B
B
 B
B
B
&B
2B
B
2B
?B
+B
WB
=B
]B
]B
CB
dB
dB
jB
jB
jB
pB
 vB
 vB
 vB
 vB
!|B
!bB
!|B
"�B
"hB
#�B
$�B
$�B
%�B
$�B
%zB
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
,�B
,�B
,�B
-�B
-�B
-�B
.�B
.�B
/�B
/�B
0�B
0�B
0�B
1�B
1�B
1�B
2�B
3�B
3�B
3�B
4�B
4�B
4�B
5�B
5�B
5�B
5�B
5�B
5�B
5�B
6�B
6�B
6�B
6�B
6�B
6�B
8B
9	B
9	B
9	B
9	B
:B
9�B
:B
:B
:B
:B
:�B
;B
<B
<B
<B
<B
<B
="B
=B
="B
=B
>B
>(B
?.B
?.B
?HB
?.B
?HB
?.B
?.B
?.B
@4B
@4B
@4B
@4B
A;B
A;B
A;B
A;B
A B
A;B
A;B
B'B
BAB
BAB
CGB
CGB
CGB
CGB
DMB
DMB
DMB
DMB
DMB
ESB
ESB
ESB
ESB
ESB
ESB
ESB
FYB
FYB
G_B
G_B
G_B
GzB
G_B
GzB
G_B
GzB
HfB
HfB
IlB
IlB
IlB
I�B
IlB
I�B
I�B
IlB
IlB
JrB
JrB
JXB
JrB
JXB
JrB
JrB
JrB
JrB
JrB
JrB
KxB
KxB
JrB
JrB
JrB
JrB
JrB
JrB
JrB
IlB
JrB
KxB
KxB
KxB
L~B
L�B
L~B
L~B
L~B
L~B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
NpB
N�B
O�B
O�B
O�B
O�B
O�B
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
R�B
R�B
R�B
R�B
R�B
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
U�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
W�B
X�B
X�B
X�B
X�B
X�B
Y�B
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
[�B
[�B
[�B
\�B
\�B
\�B
\�B
]�B
]�B
]�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_�B
_�B
_�B
_�B
`�B
`�B
aB
`�B
aB
`�B
a�B
a�B
a�B
a�B
a�B
a�B
b�B
cB
cB
dB
dB
dB
dB
dB
eB
eB
eB
f2B
f2B
f2B
fB
fB
fB
fB
fB
g8B
g8B
gB
gB
gB
gB
g8B
h$B
h$B
h$B
i*1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.6(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201904300034282019043000342820190430003428201905010024092019050100240920190501002409JA  ARFMdecpA19c                                                                20190425093638  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190425003640  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190425003641  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190425003642  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190425003643  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190425003643  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190425003643  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190425003643  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190425003643  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190425003644                      G�O�G�O�G�O�                JA  ARUP                                                                        20190425005816                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190425154112  CV  JULD            G�O�G�O�F�Ǟ                JM  ARCAJMQC2.0                                                                 20190429153428  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190429153428  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190430152409  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120031517                      G�O�G�O�G�O�                