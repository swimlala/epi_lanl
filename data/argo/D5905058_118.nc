CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-01-16T15:36:26Z creation;2019-01-16T15:36:29Z conversion to V3.1;2019-12-23T06:08:32Z update;     
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pL   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �`   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �h   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �p   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190116153626  20200120021523  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               vA   JA  I2_0675_118                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @ؠ^���1   @ؠ^��� @7���c2j~��#1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D5��D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ Dͼ�D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ DҼ�D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D��3D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�ff@�33A	��A)��AI��Ai��A���A���A���A���A���A���A���A���BffB
ffBffBffB"��B*ffB2ffB:ffBBffBJffBRffBZffBbffBjffBrffBzffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�  B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�@ C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�Y�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�D &fD �fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD	&fD	�fD
&fD
�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD &fD �fD!&fD!�fD"&fD"�fD#&fD#�fD$&fD$�fD%&fD%�fD&&fD&�fD'&fD'�fD(&fD(�fD)&fD)�fD*&fD*�fD+&fD+�fD,&fD,�fD-&fD-�fD.&fD.�fD/&fD/�fD0&fD0�fD1&fD1�fD2&fD2�fD3&fD3�fD4&fD4�fD5&fD5�fD6  D6�fD7&fD7�fD8&fD8�fD9&fD9�fD:&fD:�fD;&fD;�fD<&fD<�fD=&fD=�fD>&fD>�fD?&fD?�fD@&fD@�fDA&fDA�fDB&fDB�fDC&fDC�fDD&fDD�fDE&fDE�fDF&fDF�fDG&fDG�fDH&fDH�fDI&fDI�fDJ&fDJ�fDK&fDK�fDL&fDL�fDM&fDM�fDN&fDN�fDO&fDO�fDP&fDP�fDQ&fDQ�fDR&fDR�fDS&fDS�fDT&fDT�fDU&fDU�fDV&fDV�fDW&fDW�fDX&fDX�fDY&fDY�fDZ&fDZ�fD[&fD[�fD\&fD\�fD]&fD]�fD^&fD^�fD_&fD_�fD`&fD`�fDa&fDa�fDb&fDb�fDc&fDc�fDd&fDd�fDe&fDe�fDf&fDf�fDg&fDg�fDh&fDh�fDi&fDi�fDj&fDj�fDk&fDk�fDl&fDl�fDm&fDm�fDn&fDn�fDo&fDo�fDp&fDp�fDq&fDq�fDr&fDr�fDs&fDs�fDt&fDt�fDu&fDu�fDv&fDv�fDw&fDw�fDx&fDx�fDy&fDy�fDz&fDz�fD{&fD{�fD|&fD|�fD}&fD}�fD~&fD~�fD&fD�fD�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D� D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D3D��3D�3D�S3DÓ3D��3D�3D�S3Dē3D��3D�3D�S3Dœ3D��3D�3D�S3DƓ3D��3D�3D�S3DǓ3D��3D�3D�S3Dȓ3D��3D�3D�S3Dɓ3D��3D�3D�S3Dʓ3D��3D�3D�S3D˓3D��3D�3D�S3D̓3D��3D�3D�S3D͓3D�� D�3D�S3DΓ3D��3D�3D�S3Dϓ3D��3D�3D�S3DГ3D��3D�3D�S3Dѓ3D��3D�3D�S3Dғ3D�� D�3D�S3Dӓ3D��3D�3D�S3Dԓ3D��3D�3D�S3DՓ3D��3D�3D�S3D֓3D��fD�3D�S3Dד3D��3D�3D�S3Dؓ3D��3D�3D�S3Dٓ3D��3D�3D�S3Dړ3D��3D�3D�S3Dۓ3D��3D�3D�S3Dܓ3D��3D�3D�S3Dݓ3D��3D�3D�S3Dޓ3D��3D�3D�S3Dߓ3D��3D�3D�S3D��3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�P D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�fD�Ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�bNA�p�A��+A��PA��PA��PA��PA��PA��PA��PA��PA��PA��PA��hA��uA��uA��hA��uA���A���A���A���A���A���A���A���A���A��hA��\A��uA��uA��uA��\A��A��A��A��A��A�|�A�z�A�~�A���A���A��A�bNA��A��hA��A��yA��hA��A�A���A��RA��!A��+A�  A�ĜA��\A��A�M�A��TA�9XA��A��#A��A���A�ȴA���A���A�v�A��A��A�K�A��;A���A�ƨA� �A�p�A���A�v�A��A�`BA�$�A�
=A��#A�M�A� �A�Q�A�ĜA�r�A���A�p�A��9A��wA�XA��A��#A��A�7LA���A��hA�M�A��A�n�A�bA��A��A��A���A�z�A��A��RA�&�A�hsA�\)A���A��-A�M�A�=qA�=qAhsA|�!AzbAxĜAw"�Ar�9AoVAl�9Ak��Ai�Ah{Ae��Abr�A_XA^=qA]O�A[33AY��AV  ATn�AR5?AQXAO�AN  AL�AI�FAG33AFffAEC�AD��AEdZAD��ADAB��A@�!A?l�A>�RA=�A=7LA<ĜA<Q�A:��A9��A8��A8Q�A7��A6�!A5�FA5O�A5?}A57LA5�A4 �A1�A133A0v�A0$�A.��A-��A,bNA*n�A'�TA%�;A$^5A#p�A"1A!33A bNA��A�HAbNAJA|�A�A�\A��A��A�\AJA�A�`Az�A�TAK�A
=A�A7LA��A�mA�Ar�A�A7LAr�AĜAO�A�mA
�9A	��A	dZA�yA��A�A�RA�FA7LA�DA��A�A5?A��A ȴ@��P@��@��@�33@�G�@�z�@��@�5?@���@�o@��@�bN@���@�  @ꗍ@�$�@�O�@�j@�1'@��
@�C�@�ff@�j@��@�M�@�@�|�@��#@ە�@�K�@�n�@�1'@�t�@֗�@�/@Լj@Ұ!@Ѳ-@��@�1'@��y@ͩ�@�G�@̛�@��@˥�@��@�x�@��;@�=q@�G�@þw@�ȴ@�=q@�@��h@�&�@�(�@��@��w@�dZ@��H@�M�@��@�Z@��m@�K�@�5?@��@�/@��m@�=q@�hs@�Ĝ@��@���@���@�ff@��7@�%@���@���@�@�V@��/@�(�@�dZ@��@�-@��-@�p�@�G�@�&�@��@��@��@���@�t�@�\)@�K�@�+@�-@�?}@�r�@��@�bN@�\)@�33@�"�@�@��R@���@�v�@�V@�=q@�{@��@��#@�hs@��@���@�Ĝ@��@���@��u@���@��9@��D@�9X@��;@���@�S�@���@�5?@���@��T@�G�@��/@�r�@��;@�;d@��y@�ff@��@���@�7L@��`@��@��@�(�@��m@�ƨ@���@���@�S�@�33@�@���@�$�@�{@�@���@���@��#@�E�@�=q@��`@�j@�1'@�b@��P@�"�@�@�@��y@���@��!@���@�n�@�E�@�{@�@�J@�@���@��#@��@�G�@��@�I�@�1'@��@�  @�Ĝ@�bN@�1@�ƨ@�|�@�S�@�33@�K�@�l�@�t�@���@��F@�bN@�S�@��y@��R@���@���@��!@��@�K�@�|�@�t�@�\)@�;d@���@���@���@��!@�ff@�J@���@��^@��-@���@�`B@�/@�&�@���@�z�@�bN@�A�@���@�dZ@�S�@�;d@�o@��y@�~�@���@��h@�G�@���@�Ĝ@�z�@�I�@�A�@�9X@�(�@��m@�S�@��@���@���@���@�V@��@���@��h@�hs@���@���@�Q�@�1'@�b@�  @��m@��w@�|�@�C�@�"�@�o@�o@�o@���@�ȴ@���@�E�@��T@���@�`B@��@���@��/@��j@�I�@�b@�1@�  @�P@K�@
=@~�R@~V@~E�@~E�@~5?@}@}�@|�D@{��@{��@{t�@z�@z��@z�!@zM�@y�#@y&�@x��@xQ�@x  @w�@w��@w�P@w
=@v��@v5?@v@u��@u@u@u�-@u�h@u�@u�@uV@t�@t�D@t9X@t�@s�F@r�H@r~�@rM�@rJ@q��@qhs@q&�@q�@q%@p��@p��@p�`@p�`@p�`@p�9@pr�@o�@n��@n{@mp�@mV@l��@l(�@k�
@kS�@ko@j��@j�!@j-@i��@i�^@i%@h �@g��@g;d@f�@f@e��@e�-@ep�@e/@dz�@dI�@d9X@c�m@ct�@c@b�\@b-@a�^@ax�@`Ĝ@`A�@`b@_�;@_�w@_��@_|�@_K�@^v�@^5?@]@]p�@]?}@\Z@[�
@[t�@[C�@Z�@Z��@Z��@Z~�@Z-@Y��@Y�#@Y��@YX@Y&�@X�`@X�@Xr�@XA�@W�@W��@W|�@W\)@W
=@V�R@V��@Vff@VV@VE�@U��@U�h@U?}@T��@T(�@S��@S��@S"�@R�H@R�!@R~�@R-@RJ@Q�@Q�#@Qx�@Q7L@Q�@P�9@P1'@O�w@O�P@O�@N�+@N5?@N$�@M��@M`B@M�@L��@L�D@L9X@L1@K�F@K33@J�!@J-@JJ@I�#@I��@I�^@I��@IG�@H��@H�`@H�u@Hr�@HA�@Hb@G�;@G��@G�w@Gl�@G
=@F��@F$�@E�T@E�-@Ep�@E�@D�@Dz�@DI�@DI�@D1@Cƨ@C��@CdZ@C33@B��@Bn�@B^5@B�@A�#@A��@A�7@AX@@��@@�@@A�@@  @?��@?�P@?K�@>�y@>v�@>$�@=�@=�-@=`B@=�@<��@<��@<I�@<9X@<�@;��@;C�@;o@:~�@:=q@:�@9�^@9�7@9X@9�@8Ĝ@8bN@8 �@7�@7+@6�@6��@6E�@5�-@5O�@4��@4�@4�/@4�@49X@3��@3C�@2�H@2�!@2~�@2M�@2-@1��@1��@1�7@1X@0��@0Ĝ@0�@01'@/��@/+@.�@.��@.V@.5?@.@-��@-�-@-�h@-`B@-�@,�j@,�D@,I�@+�
@+��@+C�@+@*�H@*��@*n�@*-@*J@)�@)��@)��@)�7@)&�@(��@(�@(1'@(b@(  @'�;@'�@'l�@'+@&��@&��@&V@%�@%�T@%��@%p�@%`B@%`B@%�@$�j@$�D@$I�@$�@#��@#�F@#��@#�@#33@#@"�!@"~�@"n�@"^5@"=q@"-@"J@!�^@!�7@!G�@!�@ ��@ �9@ r�@ Q�@ A�@ 1'@ b@�;@��@�P@+@��@�@ȴ@ȴ@ȴ@�R@ff@{@@�T@@�-@�h@?}@V@V@��@�@�@�/@�D@I�@�@ƨ@�@C�@�@�!@�\@^5@=q@J@��@�@�#@�^@��@�7@x�@&�@��@Ĝ@�9@�u@�@r�@bN@bN@A�@1'@ �@�;@|�@�@��@�@ȴ@�R@�+@V@$�@�T@��@�-@�@`B@/@/@��@�j@z�@j@I�@9X@(�@�@��@�F@��@�@C�@"�@�@�H@��@�!@��@�\@=q@��@�#@��@hs@G�@�@��@�`@�`@��@Ĝ@��@b@�w@��@\)@;d@�@��@�R@��@��@�+@v�@ff@ff@5?@�T@@��@�@`B@/@V@V1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�bNA�p�A��+A��PA��PA��PA��PA��PA��PA��PA��PA��PA��PA��hA��uA��uA��hA��uA���A���A���A���A���A���A���A���A���A��hA��\A��uA��uA��uA��\A��A��A��A��A��A�|�A�z�A�~�A���A���A��A�bNA��A��hA��A��yA��hA��A�A���A��RA��!A��+A�  A�ĜA��\A��A�M�A��TA�9XA��A��#A��A���A�ȴA���A���A�v�A��A��A�K�A��;A���A�ƨA� �A�p�A���A�v�A��A�`BA�$�A�
=A��#A�M�A� �A�Q�A�ĜA�r�A���A�p�A��9A��wA�XA��A��#A��A�7LA���A��hA�M�A��A�n�A�bA��A��A��A���A�z�A��A��RA�&�A�hsA�\)A���A��-A�M�A�=qA�=qAhsA|�!AzbAxĜAw"�Ar�9AoVAl�9Ak��Ai�Ah{Ae��Abr�A_XA^=qA]O�A[33AY��AV  ATn�AR5?AQXAO�AN  AL�AI�FAG33AFffAEC�AD��AEdZAD��ADAB��A@�!A?l�A>�RA=�A=7LA<ĜA<Q�A:��A9��A8��A8Q�A7��A6�!A5�FA5O�A5?}A57LA5�A4 �A1�A133A0v�A0$�A.��A-��A,bNA*n�A'�TA%�;A$^5A#p�A"1A!33A bNA��A�HAbNAJA|�A�A�\A��A��A�\AJA�A�`Az�A�TAK�A
=A�A7LA��A�mA�Ar�A�A7LAr�AĜAO�A�mA
�9A	��A	dZA�yA��A�A�RA�FA7LA�DA��A�A5?A��A ȴ@��P@��@��@�33@�G�@�z�@��@�5?@���@�o@��@�bN@���@�  @ꗍ@�$�@�O�@�j@�1'@��
@�C�@�ff@�j@��@�M�@�@�|�@��#@ە�@�K�@�n�@�1'@�t�@֗�@�/@Լj@Ұ!@Ѳ-@��@�1'@��y@ͩ�@�G�@̛�@��@˥�@��@�x�@��;@�=q@�G�@þw@�ȴ@�=q@�@��h@�&�@�(�@��@��w@�dZ@��H@�M�@��@�Z@��m@�K�@�5?@��@�/@��m@�=q@�hs@�Ĝ@��@���@���@�ff@��7@�%@���@���@�@�V@��/@�(�@�dZ@��@�-@��-@�p�@�G�@�&�@��@��@��@���@�t�@�\)@�K�@�+@�-@�?}@�r�@��@�bN@�\)@�33@�"�@�@��R@���@�v�@�V@�=q@�{@��@��#@�hs@��@���@�Ĝ@��@���@��u@���@��9@��D@�9X@��;@���@�S�@���@�5?@���@��T@�G�@��/@�r�@��;@�;d@��y@�ff@��@���@�7L@��`@��@��@�(�@��m@�ƨ@���@���@�S�@�33@�@���@�$�@�{@�@���@���@��#@�E�@�=q@��`@�j@�1'@�b@��P@�"�@�@�@��y@���@��!@���@�n�@�E�@�{@�@�J@�@���@��#@��@�G�@��@�I�@�1'@��@�  @�Ĝ@�bN@�1@�ƨ@�|�@�S�@�33@�K�@�l�@�t�@���@��F@�bN@�S�@��y@��R@���@���@��!@��@�K�@�|�@�t�@�\)@�;d@���@���@���@��!@�ff@�J@���@��^@��-@���@�`B@�/@�&�@���@�z�@�bN@�A�@���@�dZ@�S�@�;d@�o@��y@�~�@���@��h@�G�@���@�Ĝ@�z�@�I�@�A�@�9X@�(�@��m@�S�@��@���@���@���@�V@��@���@��h@�hs@���@���@�Q�@�1'@�b@�  @��m@��w@�|�@�C�@�"�@�o@�o@�o@���@�ȴ@���@�E�@��T@���@�`B@��@���@��/@��j@�I�@�b@�1@�  @�P@K�@
=@~�R@~V@~E�@~E�@~5?@}@}�@|�D@{��@{��@{t�@z�@z��@z�!@zM�@y�#@y&�@x��@xQ�@x  @w�@w��@w�P@w
=@v��@v5?@v@u��@u@u@u�-@u�h@u�@u�@uV@t�@t�D@t9X@t�@s�F@r�H@r~�@rM�@rJ@q��@qhs@q&�@q�@q%@p��@p��@p�`@p�`@p�`@p�9@pr�@o�@n��@n{@mp�@mV@l��@l(�@k�
@kS�@ko@j��@j�!@j-@i��@i�^@i%@h �@g��@g;d@f�@f@e��@e�-@ep�@e/@dz�@dI�@d9X@c�m@ct�@c@b�\@b-@a�^@ax�@`Ĝ@`A�@`b@_�;@_�w@_��@_|�@_K�@^v�@^5?@]@]p�@]?}@\Z@[�
@[t�@[C�@Z�@Z��@Z��@Z~�@Z-@Y��@Y�#@Y��@YX@Y&�@X�`@X�@Xr�@XA�@W�@W��@W|�@W\)@W
=@V�R@V��@Vff@VV@VE�@U��@U�h@U?}@T��@T(�@S��@S��@S"�@R�H@R�!@R~�@R-@RJ@Q�@Q�#@Qx�@Q7L@Q�@P�9@P1'@O�w@O�P@O�@N�+@N5?@N$�@M��@M`B@M�@L��@L�D@L9X@L1@K�F@K33@J�!@J-@JJ@I�#@I��@I�^@I��@IG�@H��@H�`@H�u@Hr�@HA�@Hb@G�;@G��@G�w@Gl�@G
=@F��@F$�@E�T@E�-@Ep�@E�@D�@Dz�@DI�@DI�@D1@Cƨ@C��@CdZ@C33@B��@Bn�@B^5@B�@A�#@A��@A�7@AX@@��@@�@@A�@@  @?��@?�P@?K�@>�y@>v�@>$�@=�@=�-@=`B@=�@<��@<��@<I�@<9X@<�@;��@;C�@;o@:~�@:=q@:�@9�^@9�7@9X@9�@8Ĝ@8bN@8 �@7�@7+@6�@6��@6E�@5�-@5O�@4��@4�@4�/@4�@49X@3��@3C�@2�H@2�!@2~�@2M�@2-@1��@1��@1�7@1X@0��@0Ĝ@0�@01'@/��@/+@.�@.��@.V@.5?@.@-��@-�-@-�h@-`B@-�@,�j@,�D@,I�@+�
@+��@+C�@+@*�H@*��@*n�@*-@*J@)�@)��@)��@)�7@)&�@(��@(�@(1'@(b@(  @'�;@'�@'l�@'+@&��@&��@&V@%�@%�T@%��@%p�@%`B@%`B@%�@$�j@$�D@$I�@$�@#��@#�F@#��@#�@#33@#@"�!@"~�@"n�@"^5@"=q@"-@"J@!�^@!�7@!G�@!�@ ��@ �9@ r�@ Q�@ A�@ 1'@ b@�;@��@�P@+@��@�@ȴ@ȴ@ȴ@�R@ff@{@@�T@@�-@�h@?}@V@V@��@�@�@�/@�D@I�@�@ƨ@�@C�@�@�!@�\@^5@=q@J@��@�@�#@�^@��@�7@x�@&�@��@Ĝ@�9@�u@�@r�@bN@bN@A�@1'@ �@�;@|�@�@��@�@ȴ@�R@�+@V@$�@�T@��@�-@�@`B@/@/@��@�j@z�@j@I�@9X@(�@�@��@�F@��@�@C�@"�@�@�H@��@�!@��@�\@=q@��@�#@��@hs@G�@�@��@�`@�`@��@Ĝ@��@b@�w@��@\)@;d@�@��@�R@��@��@�+@v�@ff@ff@5?@�T@@��@�@`B@/@V@V1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B�/B�`B�B�B�B�B�B�B�B�B�B�B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  BB%B�B5?BL�BdZBu�Bz�B}�B{�B�B�B�PB��B��B��B��B�hB�=B�PB}�Bl�Bl�Bk�BhsB\)BVBN�BI�BI�BI�BL�BQ�BO�BK�BC�B;dB33B(�B�B�B�B�B�B�B�B�BhB	7B��B�ZB�BB��B��BB�B��B��B��B�1Bo�B^5BYBR�BE�B7LB+B�BB
�B
��B
ÖB
�3B
��B
�\B
�B
t�B
W
B
P�B
J�B
9XB
+B
�B
VB	�B	��B	�FB	�B	��B	�=B	x�B	aHB	H�B	@�B	;dB	.B	%�B	bB		7B	B��B��B�B�B�B��BB�wB�jBȴB��B��B��B�}B�^B�RB�RB�FB�9B�3B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�uB�VB�7B�B� Bx�Bq�Bl�BhsBe`BbNB`BB]/B\)BYBXBW
BVBW
BT�BR�BQ�BR�BP�BO�BM�BL�BI�BG�BI�BE�BD�BC�BA�B>wB=qB:^B7LB5?B33B1'B/B/B-B-B,B-B,B)�B'�B(�B&�B%�B%�B#�B"�B#�B!�B �B!�B �B �B!�B �B�B!�B!�B!�B$�B#�B"�B"�B"�B"�B"�B"�B"�B"�B$�B$�B$�B%�B(�B,B,B+B,B-B,B,B,B/B5?B5?B5?B6FB8RB7LB6FB7LB7LB8RB8RB;dB>wB?}BB�BE�BG�BG�BI�BI�BM�BP�BP�BQ�BR�BR�BT�BXBZB[#B]/BaHBaHBcTBgmBl�Bo�Bq�Br�Bt�Bw�By�B|�B~�B�B�7B�DB�bB�bB�uB��B��B��B��B��B��B��B��B��B�B�!B�'B�-B�-B�-B�XB��BĜBƨB��B��B��B��B��B��B��B��B��B�B�
B�#B�TB�yB�B�B�B�B�B��B��B��B��B	B	B	+B	
=B	JB	\B	hB	oB	�B	�B	�B	�B	!�B	$�B	&�B	'�B	+B	.B	/B	1'B	49B	6FB	7LB	9XB	:^B	=qB	?}B	?}B	A�B	C�B	B�B	C�B	G�B	J�B	N�B	Q�B	VB	YB	YB	YB	YB	YB	[#B	]/B	^5B	`BB	bNB	dZB	dZB	ffB	hsB	k�B	o�B	s�B	u�B	z�B	}�B	� B	� B	�B	�B	�B	�%B	�+B	�1B	�PB	�\B	�\B	�bB	�bB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�9B	�RB	�RB	�XB	�^B	�qB	�}B	�}B	�}B	��B	ĜB	ŢB	ƨB	ƨB	ƨB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�#B	�)B	�/B	�;B	�;B	�BB	�BB	�HB	�NB	�ZB	�`B	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
1B
	7B
	7B

=B

=B

=B

=B
DB
DB
DB
DB
DB
DB
DB
DB
JB
JB
PB
VB
VB
\B
\B
bB
bB
bB
hB
hB
hB
hB
oB
oB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
+B
+B
+B
,B
,B
,B
-B
-B
.B
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
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
33B
33B
33B
33B
49B
49B
49B
49B
49B
5?B
5?B
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
7LB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
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
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
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
N�B
N�B
N�B
O�B
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
P�B
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
S�B
S�B
T�B
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
XB
YB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
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
_;B
_;B
_;B
`BB
`BB
`BB
`BB
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
aHB
aHB
aHB
bNB
aHB
bNB
bNB
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
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BʌB͟B��B�,B�WB�cB�iB�oB�oB�vB�vB�|B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�BeB5BL�Bd&Bu�Bz�B}�B{�B��B��B�B�YB��B��B�~B�4B�	B�B}�BlWBlWBkQBh>B[�BU�BN�BI�BI�BI�BL�BQ�BO�BK�BCaB;0B2�B(�BkBeB_BYB_BkBkBkB4B	B��B�&B�BөB͟B�[B��B��B��B�?B��BoiB]�BX�BR�BEmB7B*�BkB�B
�]B
ҽB
�aB
��B
��B
�B
��B
t�B
V�B
P�B
J�B
9	B
*�B
WB
B	�OB	��B	��B	��B	�]B	��B	x�B	aB	H�B	@OB	;0B	-�B	%�B	B	�B	�B��B��B�|B�KBյB�rB�AB�BB�B�fB��BЗB�~B�HB�*B�B�B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�WB�EB�2B�&B�B�B��B�Bx�BqvBlWBh$BeBbB`B\�B[�BX�BW�BV�BU�BV�BT�BR�BQ�BR�BP�BO�BM�BL�BI�BGzBI�BESBDgBCaBA;B>(B=<B:B6�B4�B2�B0�B.�B.�B,�B,�B+�B,�B+�B)�B'�B(�B&�B%�B%�B#�B"�B#�B!|B vB!|B vB �B!|B vBpB!|B!|B!|B$�B#�B"�B"�B"�B"�B"�B"�B"�B"�B$�B$�B$�B%�B(�B+�B+�B*�B+�B,�B+�B+�B+�B.�B4�B5B4�B6B8B6�B5�B6�B6�B8B8B;B>BB?.BB[BESBG_BGzBIlBIlBM�BP�BP�BQ�BR�BR�BT�BW�BY�BZ�B\�B`�B`�BcBgBl=BoiBq[BraBtnBw�By�B|�B~�B��B��B��B�B�B�&B�YB�QB�OB�|B��B��B��B��B��B��B��B��B��B��B��B�	B�OB�MB�YB�rB�~B�~B�~B̈́BϑBѝBңBԯBյBּB��B�B�*B�IB�OB�[B�aB�hB�zB��B��B��B	 �B	�B	�B		�B	�B	B	B	 B	2B	2B	YB	WB	!|B	$�B	&�B	'�B	*�B	-�B	.�B	0�B	3�B	5�B	6�B	9	B	:B	="B	?.B	?.B	A B	CGB	BAB	CGB	G_B	JrB	N�B	Q�B	U�B	X�B	X�B	X�B	X�B	X�B	Z�B	\�B	^B	_�B	a�B	dB	dB	fB	h$B	k6B	oOB	shB	utB	z�B	}�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	��B	�&B	�EB	�=B	�dB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�	B	�B	�"B	�B	�.B	�.B	�;B	�MB	�SB	�YB	�YB	�YB	�fB	�lB	�lB	�rB	�~B	�~B	�~B	ϑB	ЗB	ЗB	ѝB	ѝB	ѝB	өB	ԯB	��B	өB	��B	ּB	��B	��B	��B	ںB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�,B	�B	�B	�B	�$B	�*B	�0B	�0B	�0B	�B	�6B	�=B	�WB	�CB	�CB	�CB	�CB	�CB	�IB	�/B	�OB	�[B	�[B	�aB	�MB	�hB	�hB	�hB	��B	�tB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	�lB	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B
	�B
	�B
	�B
	�B

�B
B

�B

�B

�B

�B

�B

�B
�B
�B
B
B
B
B
B
B
B
B
B
B
B
B
 B
 B
 B
&B
,B
,B
2B
2B
9B
9B
9B
9B
?B
?B
+B
EB
+B
EB
KB
KB
QB
WB
WB
]B
]B
]B
~B
dB
dB
dB
dB
jB
OB
pB
pB
pB
 \B
 \B
!bB
!bB
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%zB
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
+�B
+�B
+�B
,�B
,�B
-�B
-�B
-�B
.�B
.�B
.�B
.�B
/�B
/�B
/�B
/�B
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
2�B
2�B
2�B
2�B
3�B
3�B
3�B
3�B
3�B
4�B
4�B
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
6�B
8B
8B
9$B
9$B
9	B
9	B
8�B
:B
:*B
:*B
:B
:�B
;B
;B
;B
<B
<6B
<B
=B
="B
="B
="B
>(B
>(B
>(B
?.B
?.B
?.B
?HB
?B
?.B
?.B
@4B
@4B
A;B
A;B
A;B
A;B
BAB
BAB
B'B
BAB
CGB
CGB
DMB
DMB
D3B
ESB
ESB
ESB
ESB
ESB
FYB
FYB
FYB
G_B
G_B
G_B
HfB
HfB
HfB
HfB
IlB
IlB
IlB
IlB
JrB
JXB
JrB
KxB
KxB
KxB
L~B
L~B
L~B
L�B
L�B
L~B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
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
P�B
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
S�B
S�B
T�B
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
W�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
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
^�B
^�B
^�B
_�B
_�B
_�B
_�B
`B
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
aB
`�B
`�B
`�B
a�B
`�B
a�B
bB
c B
cB
b�B
cB
cB
c B
c B
dB
dB
dB
dB
dB
dB
dB
c�B
e,B
eB
eB
eB
eB
eB
eB
eB
fB
fB
fB
fB
fB
fB
gB
gB
gB
gB
gB
gB
g8B
h$B
h>B
h$B
h$B
h$B
i*B
i*B
i*B
i*B
i*B
i*B
i*B
jKB
j0B
jKB
j0B
j0B
k6B
k6B
k6B
k6B
k6B
k6B
k6B
k6B
k6B
l=B
l=B
l=B
l=B
l=B
l=B
mCB
mCB
mC1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.6(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201901220037062019012200370620190122003706201901230026472019012300264720190123002647JA  ARFMdecpA19c                                                                20190117003625  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190116153626  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190116153627  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190116153628  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190116153628  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190116153628  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190116153629  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190116153629  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190116153629  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190116153629                      G�O�G�O�G�O�                JA  ARUP                                                                        20190116155606                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190116153213  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20190121153706  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190121153706  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190122152647  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021523                      G�O�G�O�G�O�                