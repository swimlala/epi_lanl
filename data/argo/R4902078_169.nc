CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-12-12T13:00:31Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `x   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �<   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �\   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݌   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �,   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �0   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �4   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �8Argo profile    3.1 1.2 19500101000000  20191212130031  20191212130031  4902078 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5439                            2B  A   NAVIS_A                         0460                            011514                          863 @���b��1   @��ڲ@��@,Z��vȴ�d��\(��1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      �A   A   A   @���@���A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB��B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�  A	��A)��AI��Ai��A���A���A���A���A���A���A���A���BffB
ffBffBffB"ffB*��B2ffB:ffBBffBJffBRffBZffBbffBjffBrffBz��B�  B�  B�  B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�Y�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�D &fD �fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD	&fD	�fD
&fD
�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD &fD �fD!&fD!�fD"&fD"�fD#&fD#�fD$&fD$�fD%&fD%�fD&&fD&�fD'&fD'�fD(&fD(�fD)&fD)�fD*&fD*�fD+&fD+�fD,&fD,�fD-&fD-�fD.&fD.�fD/&fD/�fD0&fD0�fD1&fD1�fD2&fD2�fD3&fD3�fD4&fD4�fD5&fD5�fD6&fD6�fD7&fD7�fD8&fD8�fD9&fD9�fD:&fD:�fD;&fD;�fD<&fD<�fD=&fD=�fD>&fD>�fD?&fD?�fD@&fD@�fDA&fDA�fDB&fDB�fDC&fDC�fDD&fDD�fDE&fDE�fDF&fDF�fDG&fDG�fDH&fDH�fDI&fDI�fDJ&fDJ�fDK&fDK�fDL&fDL�fDM&fDM�fDN&fDN�fDO&fDO�fDP&fDP�fDQ&fDQ�fDR&fDR�fDS&fDS�fDT&fDT�fDU&fDU�fDV&fDV�fDW&fDW�fDX&fDX�fDY&fDY�fDZ&fDZ�fD[&fD[�fD\&fD\�fD]&fD]�fD^&fD^�fD_&fD_�fD`&fD`�fDa&fDa�fDb&fDb�fDc&fDc�fDd&fDd�fDe&fDe�fDf&fDf�fDg&fDg�fDh&fDh�fDi&fDi�fDj&fDj�fDk&fDk�fDl&fDl�fDm&fDm�fDn&fDn�fDo&fDo�fDp&fDp�fDq&fDq�fDr&fDr�fDs&fDs�fDt&fDt�fDu&fDu�fDv&fDv�fDw&fDw�fDx&fDx�fDy&fDy�fDz&fDz�fD{&fD{�fD|&fD|�fD}&fD}�fD~&fD~�fD&fD�fD�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D3D��3D�3D�S3DÓ3D��3D�3D�S3Dē3D��3D�3D�S3Dœ3D��3D�3D�S3DƓ3D��3D�3D�S3DǓ3D��3D�3D�S3Dȓ3D��3D�3D�S3Dɓ3D��3D�3D�S3Dʓ3D��3D�3D�S3D˓3D��3D�3D�S3D̓3D��3D�3D�S3D͓3D��3D�3D�S3DΓ3D��3D�3D�S3Dϓ3D��3D�3D�S3DГ3D��3D�3D�S3Dѓ3D��3D�3D�S3Dғ3D��3D�3D�S3Dӓ3D��3D�3D�S3Dԓ3D��3D�3D�S3DՓ3D��3D�3D�S3D֓3D��3D�3D�S3Dד3D��3D�3D�S3Dؓ3D��3D�3D�S3Dٓ3D��3D�3D�S3Dړ3D��3D�3D�S3Dۓ3D��3D�3D�S3Dܓ3D��3D�3D�S3Dݓ3D��3D�3D�S3Dޓ3D��3D�3D�S3Dߓ3D��3D�3D�S3D��3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��fD�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�XA�\)A�bNA�dZA�ffA�ffA�jA�n�A�z�A�t�A�p�A�l�A�S�A�;dA�9XA�9XA��A� �A�XA�t�A�p�A�\AᕁAᙚA�9XA��hA�=qAݕ�A�bNAخA�K�Aԝ�AΝ�A�VA���AƅA��TA��
AÅA���A�5?A���A���A�JA��;A�/A�;dA���A��#A���A���A�A��A��;A��A�bNA�M�A�A�(�A�-A���A�=qA��9A��A��TA��mA���A�t�A�9XA�O�A��7A�S�A�1'A�|�A���A��A�VA�K�A���A�ƨA��A��yA�|�A{x�Ax��Av9XAr�Ap��Ao�AnbNAf��AcXAb1A`�jA_+A[�wAY�TAW��AV��AU"�AQ�7APAL��AL{AH=qAD�!ABI�AAt�AA%A?dZA;�PA9��A3l�A2��A0ffA1O�A3�;A4��A3�-A2E�A1�FA1hsA0��A0�A/�FA/?}A.�!A+�#A'��A%S�A$��A$(�A#��A#��A#VA"jA!�hA!�A v�A�A�AQ�A�9AĜA��A-A�A�;A�;A�HAC�A�+AVAVA�A�hAr�A{A?}A�#A��A��Al�A
ffA	�wA	��A	�A	S�A	7LA	/A	/A	&�A	�A	A	%A	�A	C�A	l�A	|�A	�hA	x�A	K�A	&�A�A1AA|�AC�A�A�HAv�A  A�PAS�A
=AȴA1AO�A&�A�/AbNA��A�FA ��@�33@���@���@��@�1@�ȴ@�hs@��m@�M�@�D@��@�w@�I�@��@���@�ȴ@�\@�ff@�n�@��@�!@�@�@�F@�+@�C�@��y@�R@�v�@�E�@��-@�Ĝ@�I�@�A�@�1@���@���@���@�ƨ@�|�@��@�"�@�^5@�h@�%@�@�b@��@�@�J@�7L@�bN@�1@�ƨ@�dZ@�
=@އ+@��@ݙ�@ݑh@݉7@�hs@�G�@ܴ9@��m@���@�l�@��H@�^5@��#@ى7@��`@�9X@ץ�@�;d@֏\@��@ՙ�@���@�j@�Z@�9X@� �@�S�@��T@�/@ЋD@� �@�ƨ@υ@�o@���@·+@�5?@���@���@��m@�S�@�C�@�;d@�33@��@���@��@�7L@��@�Ĝ@Ǿw@��@�E�@ř�@�7L@��`@ċD@� �@�ƨ@�l�@�@§�@�$�@�/@�A�@��@���@�;d@��@���@��\@�v�@�M�@��-@��`@�9X@�9X@�  @��F@�dZ@�;d@�o@��@���@�v�@�5?@���@�@�?}@���@�Q�@���@�l�@���@��+@�=q@��T@��@�%@�Ĝ@�z�@�1'@��@�ƨ@�"�@�~�@�@��T@��^@���@�hs@�7L@���@��/@���@�1@��@�t�@�K�@�@��+@�$�@�{@���@���@�`B@�V@��@�j@�1@��
@���@�S�@�+@���@��@��R@���@�v�@�V@�5?@�$�@��@���@���@�@��h@��`@�z�@�j@�I�@�  @�l�@�;d@�"�@��@��R@�v�@�$�@�@��T@�G�@�bN@���@�l�@�C�@�o@��+@�n�@�M�@�@���@�`B@�&�@��@���@��j@��u@�z�@�j@�A�@��@���@�ƨ@�S�@��y@���@��R@���@�n�@�E�@�5?@�J@���@�&�@�r�@��m@���@�33@��y@��@���@��R@�M�@�J@���@��^@�O�@���@�r�@�I�@��@��@��;@���@���@�l�@�S�@�C�@��@��@���@���@��@�Z@��m@�dZ@��@��R@�n�@�E�@�E�@�=q@�=q@�V@�^5@��@�`B@�V@�Ĝ@��9@�r�@� �@��;@�\)@�+@��@�{@�x�@��@��j@��@�bN@�(�@�1@�  @��@��m@��
@�ƨ@��@���@��@�t�@��y@��\@�-@�J@�@��#@��7@�?}@���@��j@�bN@���@���@��P@�t�@�33@�@��H@��R@��\@�~�@�^5@�V@�M�@�5?@��@��T@���@��@�hs@�G�@��D@���@��P@�33@���@�~�@�^5@�$�@�@�p�@�7L@�%@�Ĝ@�Q�@�(�@��@�w@K�@~ff@}@}O�@}/@|��@{ƨ@{"�@z�H@y�@yX@x��@x  @wK�@v�R@v��@vv�@vff@v5?@u�T@uO�@t(�@s��@sdZ@r~�@q��@qX@q�@pr�@o�;@oK�@n�@m@m?}@m/@l��@l��@l�@l��@lj@l9X@k�m@k��@j�@i�@iX@h�9@g�@fȴ@fȴ@f�R@f��@f�+@f5?@f$�@e�@e��@e��@ep�@e/@d�/@d�@d��@d�D@dZ@d�@c��@c��@c��@cƨ@cƨ@c�F@c�F@c�F@c�F@ct�@cC�@c"�@b��@a�@a�@`Ĝ@`A�@_|�@^�R@^E�@]�-@]p�@]?}@]�@\�/@[ƨ@[S�@[33@[o@Z�@Z�!@Z~�@Y��@YX@X��@XbN@X �@W�w@W�P@V�y@VV@V{@U�@Tz�@TZ@S��@S33@R��@R��@RM�@R�@RJ@Q�@Q��@Q7L@Q�@P�u@O�P@Nȴ@Nff@N$�@M��@MO�@L��@LZ@L1@K�m@Kƨ@Kt�@KC�@K33@K@J�@J-@I�7@Ihs@I7L@HĜ@H�u@HbN@Hb@G�;@G�@G�P@G\)@G+@F�y@F��@F�+@Fv�@Fff@Fff@E@E�@D�@DI�@C�
@CdZ@C33@B��@B~�@BM�@B�@A�#@A&�@@��@@Q�@@b@?��@?+@>ȴ@>��@>�+@>ff@>E�@>{@=�@=��@=�@<�@<�j@<��@<�D@<j@<I�@<(�@;��@;�
@;ƨ@;��@;�@;33@;"�@;@:�\@9��@9x�@9�@8��@8bN@7�;@7|�@7K�@7;d@7+@6ȴ@6ff@6@5@5�h@5p�@5�@5O�@5�@5V@4�@4��@4�@4(�@3dZ@3"�@2�H@2��@2M�@2�@2J@1��@1��@17L@0��@0�9@0��@0r�@0Q�@/�@/l�@.ȴ@.v�@.@-�T@-��@-��@-/@,�@,�@,j@+��@+�
@+�F@+��@+��@*�\@)�@)��@)��@)G�@)%@(�`@(�u@(�u@(�u@(�u@(�@(1'@'��@'�P@'\)@'+@&�y@&��@&E�@&$�@%�T@%p�@%/@%�@%V@$��@$�D@$(�@#ƨ@#��@#S�@"�@"��@"�!@"�\@"M�@"�@!��@!X@!7L@!7L@!%@!%@ ��@ ��@ ��@ �@ bN@ 1'@�@+@
=@�@ff@5?@@��@�h@`B@/@��@�/@z�@��@�
@��@dZ@C�@33@"�@��@M�@��@��@��@�@��@��@hs@X@G�@�@��@A�@ �@  @�@�;@�w@|�@l�@��@ȴ@��@�+@ff@E�@�T@�T@�@�-@`B@O�@?}@/@V@��@�j@�@��@�D@z�@9X@��@�
@�
@ƨ@ƨ@S�@"�@�\@�@�^@��@x�@&�@�@��@��@��@��@��@��@Ĝ@�9@��@�@Q�@ �@  @�;@�w@�@|�@l�@\)@;d@��@ȴ@5?@$�@$�@@O�@�@�j@�@z�@Z@9X@(�@�@1@1@��@�m@�F@t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�XA�\)A�bNA�dZA�ffA�ffA�jA�n�A�z�A�t�A�p�A�l�A�S�A�;dA�9XA�9XA��A� �A�XA�t�A�p�A�\AᕁAᙚA�9XA��hA�=qAݕ�A�bNAخA�K�Aԝ�AΝ�A�VA���AƅA��TA��
AÅA���A�5?A���A���A�JA��;A�/A�;dA���A��#A���A���A�A��A��;A��A�bNA�M�A�A�(�A�-A���A�=qA��9A��A��TA��mA���A�t�A�9XA�O�A��7A�S�A�1'A�|�A���A��A�VA�K�A���A�ƨA��A��yA�|�A{x�Ax��Av9XAr�Ap��Ao�AnbNAf��AcXAb1A`�jA_+A[�wAY�TAW��AV��AU"�AQ�7APAL��AL{AH=qAD�!ABI�AAt�AA%A?dZA;�PA9��A3l�A2��A0ffA1O�A3�;A4��A3�-A2E�A1�FA1hsA0��A0�A/�FA/?}A.�!A+�#A'��A%S�A$��A$(�A#��A#��A#VA"jA!�hA!�A v�A�A�AQ�A�9AĜA��A-A�A�;A�;A�HAC�A�+AVAVA�A�hAr�A{A?}A�#A��A��Al�A
ffA	�wA	��A	�A	S�A	7LA	/A	/A	&�A	�A	A	%A	�A	C�A	l�A	|�A	�hA	x�A	K�A	&�A�A1AA|�AC�A�A�HAv�A  A�PAS�A
=AȴA1AO�A&�A�/AbNA��A�FA ��@�33@���@���@��@�1@�ȴ@�hs@��m@�M�@�D@��@�w@�I�@��@���@�ȴ@�\@�ff@�n�@��@�!@�@�@�F@�+@�C�@��y@�R@�v�@�E�@��-@�Ĝ@�I�@�A�@�1@���@���@���@�ƨ@�|�@��@�"�@�^5@�h@�%@�@�b@��@�@�J@�7L@�bN@�1@�ƨ@�dZ@�
=@އ+@��@ݙ�@ݑh@݉7@�hs@�G�@ܴ9@��m@���@�l�@��H@�^5@��#@ى7@��`@�9X@ץ�@�;d@֏\@��@ՙ�@���@�j@�Z@�9X@� �@�S�@��T@�/@ЋD@� �@�ƨ@υ@�o@���@·+@�5?@���@���@��m@�S�@�C�@�;d@�33@��@���@��@�7L@��@�Ĝ@Ǿw@��@�E�@ř�@�7L@��`@ċD@� �@�ƨ@�l�@�@§�@�$�@�/@�A�@��@���@�;d@��@���@��\@�v�@�M�@��-@��`@�9X@�9X@�  @��F@�dZ@�;d@�o@��@���@�v�@�5?@���@�@�?}@���@�Q�@���@�l�@���@��+@�=q@��T@��@�%@�Ĝ@�z�@�1'@��@�ƨ@�"�@�~�@�@��T@��^@���@�hs@�7L@���@��/@���@�1@��@�t�@�K�@�@��+@�$�@�{@���@���@�`B@�V@��@�j@�1@��
@���@�S�@�+@���@��@��R@���@�v�@�V@�5?@�$�@��@���@���@�@��h@��`@�z�@�j@�I�@�  @�l�@�;d@�"�@��@��R@�v�@�$�@�@��T@�G�@�bN@���@�l�@�C�@�o@��+@�n�@�M�@�@���@�`B@�&�@��@���@��j@��u@�z�@�j@�A�@��@���@�ƨ@�S�@��y@���@��R@���@�n�@�E�@�5?@�J@���@�&�@�r�@��m@���@�33@��y@��@���@��R@�M�@�J@���@��^@�O�@���@�r�@�I�@��@��@��;@���@���@�l�@�S�@�C�@��@��@���@���@��@�Z@��m@�dZ@��@��R@�n�@�E�@�E�@�=q@�=q@�V@�^5@��@�`B@�V@�Ĝ@��9@�r�@� �@��;@�\)@�+@��@�{@�x�@��@��j@��@�bN@�(�@�1@�  @��@��m@��
@�ƨ@��@���@��@�t�@��y@��\@�-@�J@�@��#@��7@�?}@���@��j@�bN@���@���@��P@�t�@�33@�@��H@��R@��\@�~�@�^5@�V@�M�@�5?@��@��T@���@��@�hs@�G�@��D@���@��P@�33@���@�~�@�^5@�$�@�@�p�@�7L@�%@�Ĝ@�Q�@�(�@��@�w@K�@~ff@}@}O�@}/@|��@{ƨ@{"�@z�H@y�@yX@x��@x  @wK�@v�R@v��@vv�@vff@v5?@u�T@uO�@t(�@s��@sdZ@r~�@q��@qX@q�@pr�@o�;@oK�@n�@m@m?}@m/@l��@l��@l�@l��@lj@l9X@k�m@k��@j�@i�@iX@h�9@g�@fȴ@fȴ@f�R@f��@f�+@f5?@f$�@e�@e��@e��@ep�@e/@d�/@d�@d��@d�D@dZ@d�@c��@c��@c��@cƨ@cƨ@c�F@c�F@c�F@c�F@ct�@cC�@c"�@b��@a�@a�@`Ĝ@`A�@_|�@^�R@^E�@]�-@]p�@]?}@]�@\�/@[ƨ@[S�@[33@[o@Z�@Z�!@Z~�@Y��@YX@X��@XbN@X �@W�w@W�P@V�y@VV@V{@U�@Tz�@TZ@S��@S33@R��@R��@RM�@R�@RJ@Q�@Q��@Q7L@Q�@P�u@O�P@Nȴ@Nff@N$�@M��@MO�@L��@LZ@L1@K�m@Kƨ@Kt�@KC�@K33@K@J�@J-@I�7@Ihs@I7L@HĜ@H�u@HbN@Hb@G�;@G�@G�P@G\)@G+@F�y@F��@F�+@Fv�@Fff@Fff@E@E�@D�@DI�@C�
@CdZ@C33@B��@B~�@BM�@B�@A�#@A&�@@��@@Q�@@b@?��@?+@>ȴ@>��@>�+@>ff@>E�@>{@=�@=��@=�@<�@<�j@<��@<�D@<j@<I�@<(�@;��@;�
@;ƨ@;��@;�@;33@;"�@;@:�\@9��@9x�@9�@8��@8bN@7�;@7|�@7K�@7;d@7+@6ȴ@6ff@6@5@5�h@5p�@5�@5O�@5�@5V@4�@4��@4�@4(�@3dZ@3"�@2�H@2��@2M�@2�@2J@1��@1��@17L@0��@0�9@0��@0r�@0Q�@/�@/l�@.ȴ@.v�@.@-�T@-��@-��@-/@,�@,�@,j@+��@+�
@+�F@+��@+��@*�\@)�@)��@)��@)G�@)%@(�`@(�u@(�u@(�u@(�u@(�@(1'@'��@'�P@'\)@'+@&�y@&��@&E�@&$�@%�T@%p�@%/@%�@%V@$��@$�D@$(�@#ƨ@#��@#S�@"�@"��@"�!@"�\@"M�@"�@!��@!X@!7L@!7L@!%@!%@ ��@ ��@ ��@ �@ bN@ 1'@�@+@
=@�@ff@5?@@��@�h@`B@/@��@�/@z�@��@�
@��@dZ@C�@33@"�@��@M�@��@��@��@�@��@��@hs@X@G�@�@��@A�@ �@  @�@�;@�w@|�@l�@��@ȴ@��@�+@ff@E�@�T@�T@�@�-@`B@O�@?}@/@V@��@�j@�@��@�D@z�@9X@��@�
@�
@ƨ@ƨ@S�@"�@�\@�@�^@��@x�@&�@�@��@��@��@��@��@��@Ĝ@�9@��@�@Q�@ �@  @�;@�w@�@|�@l�@\)@;d@��@ȴ@5?@$�@$�@@O�@�@�j@�@z�@Z@9X@(�@�@1@1@��@�m@�F@t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B��B	�B	0!B	8RB	@�B	O�B	m�B	�JB	�B	��B	�B	��B
B
B	��B	�B	�B	�BB
uB
@�B
ffB
�VB
�wBG�B33B�LBƨB�RB�{B�!B��BBBPB�B �B0!B:^B49B)�B�B�BuB�B	7B�B�sB�fB�;B�#B�)B�5B�)B��B��B�PB}�BJ�B!�B\BB
�B
�yB
�#B
ŢB
��B
��B
��B
l�B
L�B
/B
�B

=B	��B	�yB	�ZB	�yB	�yB	�/B	�?B	��B	�oB	�1B	z�B	dZB	T�B	K�B	I�B	:^B	 �B	�B	VB	
=B��B�B�B��B��B�B�B�B��B	B	B	.B	gmB	��B	��B	��B	��B	�B	�B	�9B	�}B	��B	ŢB	ȴB	�}B	�^B	�XB	�XB	�RB	�LB	�LB	�LB	�LB	�FB	�FB	�?B	�'B	�B	�B	�^B	�}B	ÖB	ŢB	ŢB	ƨB	��B	��B	��B	��B	�
B	�B	�B	�/B	�5B	�HB	�#B	��B	��B	��B	ɺB	ȴB	ȴB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	�B	�)B	�5B	�NB	�ZB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
B
%B
bB
hB
\B
JB
JB
\B
oB
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
bB
hB
uB
�B
�B
{B
uB
�B
{B
uB
uB
uB
uB
oB
oB
oB
oB
oB
oB
hB
oB
oB
oB
oB
hB
hB
bB
hB
hB
{B
uB
{B
{B
uB
oB
oB
hB
hB
\B
VB
VB
VB
VB
VB
\B
VB
\B
\B
bB
hB
hB
hB
hB
hB
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
oB
oB
oB
oB
hB
bB
bB
bB
bB
bB
hB
hB
hB
oB
oB
oB
uB
{B
{B
{B
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
%�B
&�B
&�B
&�B
&�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
,B
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
/B
/B
0!B
0!B
1'B
33B
49B
33B
33B
49B
49B
33B
1'B
1'B
2-B
33B
33B
33B
49B
49B
49B
49B
5?B
6FB
7LB
6FB
49B
49B
49B
49B
6FB
7LB
6FB
5?B
5?B
5?B
5?B
6FB
8RB
9XB
9XB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
:^B
:^B
;dB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
=qB
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
@�B
@�B
?}B
?}B
?}B
?}B
?}B
?}B
A�B
B�B
C�B
C�B
D�B
D�B
D�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
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
L�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
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
XB
XB
XB
W
B
XB
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
]/B
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
_;B
_;B
_;B
_;B
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
bNB
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
ffB
ffB
ffB
ffB
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
jB
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
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�h111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�SB�YB�SB�SB�SB�SB�SB�SB�eB�YB�SB�B	[B	�B	(B	08B	?�B	]FB	{�B	��B	��B	�kB	�B	��B	��B	�B	�:B	��B	��B
*B
08B
VB
~B
�,B7cB"�B�B�]B�B�0B��B�B��B��B�B6BzB�B*B#�B�B[B[B*B<B��B�kB�(B�B��B��B��B��B��B��B��B}Bm�B:vB�B
�B
�B
�eB
�.B
��B
�WB
��B
�[B
�BB
\@B
<�B
�B
	OB	��B	�B	�.B	�B	�.B	�.B	��B	��B	�aB	�$B	w�B	j�B	TB	D�B	;|B	9oB	*B	zB	6B�B��B�B�_B�SB�qB�qB�SB�FB�YB�B��B��B	�B	W"B	�<B	�UB	�zB	��B	��B	��B	��B	�2B	�>B	�WB	�iB	�2B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�2B	�KB	�WB	�WB	�]B	��B	�vB	��B	��B	ƿB	ŹB	��B	��B	��B	��B	��B	íB	��B	��B	�oB	�iB	�iB	�iB	�iB	�oB	�oB	�vB	�|B	�|B	��B	§B	ĳB	��B	��B	��B	�B	�B	�B	�:B	�FB	�FB	�@B	�@B	�@B	�FB	�FB	�FB	�@B	�:B	�:B	�@B	�@B	�SB	�SB	�SB	�YB	�kB	�~B	�B	�~B	�kB	�eB	�kB	�qB	�~B	�B	�xB	�kB	�YB	�FB	�@B	�MB	�kB	�B	�B	�B	�B	�B	��B	��B
 B
B	�B	��B	��B	�B
$B
*B
0B
0B
0B
0B
0B
0B
*B
$B
B
 B
B
*B
<B
<B
0B
*B
6B
0B
*B
*B
*B
*B
$B
$B
$B
$B
$B
$B
B
$B
$B
$B
$B
B
B
 B
B
B
0B
*B
0B
0B
*B
$B
$B
B
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B
 B
B
B
B
B
B
B
B
B
B
B
$B
$B
$B
$B
$B
$B
$B
$B
$B
B
 B
 B
 B
 B
 B
B
B
B
$B
$B
$B
*B
0B
0B
0B
*B
*B
0B
0B
0B
6B
6B
6B
6B
6B
6B
<B
<B
BB
BB
HB
HB
HB
HB
HB
HB
	OB
	OB
	OB
	OB
	OB
	OB
	OB
	OB

UB

UB

UB

UB

UB

UB

UB

UB

UB

UB

UB

UB

UB

UB

UB

UB

UB

UB

UB

UB

UB
	OB

UB

UB
[B
[B
[B
aB
aB
aB
aB
gB
gB
gB
gB
mB
mB
mB
sB
sB
sB
sB
sB
sB
sB
zB
zB
zB
zB
zB
zB
zB
zB
zB
zB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
"�B
"�B
#�B
#�B
"�B
 �B
 �B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
%�B
'B
%�B
#�B
#�B
#�B
#�B
%�B
'B
%�B
$�B
$�B
$�B
$�B
%�B
(B
)B
)B
(B
(B
(B
(B
(B
(B
)B
*B
*B
+B
,B
,B
-&B
-&B
-&B
-&B
-&B
-&B
-&B
-&B
-&B
-&B
.,B
.,B
-&B
.,B
.,B
.,B
.,B
/2B
/2B
/2B
08B
08B
08B
08B
08B
/2B
/2B
/2B
/2B
/2B
/2B
1>B
2DB
3KB
3KB
4QB
4QB
4QB
6]B
6]B
6]B
6]B
7cB
7cB
7cB
7cB
7cB
7cB
7cB
8iB
8iB
8iB
8iB
9oB
9oB
9oB
:vB
:vB
:vB
:vB
:vB
:vB
:vB
:vB
:vB
:vB
:vB
;|B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
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
F�B
G�B
H�B
H�B
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
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
N�B
N�B
N�B
N�B
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
RB
RB
RB
RB
S	B
S	B
S	B
S	B
S	B
S	B
S	B
TB
TB
TB
TB
TB
TB
TB
TB
UB
UB
UB
VB
VB
VB
VB
W"B
W"B
W"B
W"B
W"B
X(B
X(B
X(B
X(B
X(B
Y.B
Y.B
Y.B
Y.B
Y.B
Y.B
Z4B
Z4B
Z4B
Z4B
Z4B
Z4B
[:B
[:B
[:B
[:B
[:B
[:B
[:B
[:B
[:B
[:B
[:B
[:B
\@B
\@B
\@B
]FB
]FB
]FB
]FB
^MB
^MB
^MB
^MB
^MB
_SB
_SB
`YB
`YB
`YB
`YB
`YB
`YB
`YB
`YB
`YB
`YB
`YB
a_B
a_B
a_B
a_B
a_B
beB
beB
beB
beB
beB
beB
ckB
ckB
ckB
ckB
ckB
dqB
dqB
dqB
exB
exB
exB
exB
exB
exB
exB
f~B
f~B
f~B
f~B
f~B
f~B
f~B
g�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
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
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
}B
}B
}B
}B
}B
}B
}B
}B
}B
~B
~B
~B
~B
~B
~B
~B
B
B
B
B
B
B
B
B
B
B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      surface_pressure=-0.60 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     salinity_offset = -0.0159092                                                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                      PSAL ADJUST [dd mm yyyy N S_off stddev] 15 06 2019 151 -0.0159092 0.0003 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                        20191212130031              20191212130031  AO  ARCAADJP                                                                    20191212130031    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20191212130031    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20191212130031  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20191212130031  QCF$                G�O�G�O�G�O�0               