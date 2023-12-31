CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2021-09-02T15:41:26Z creation;2021-09-02T15:41:28Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `x   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ͐   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �$   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �T   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20210902154126  20210902155234  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               |A   JA                                  2B  A   APEX                            7906                            051216                          846 @ُ�JU�1   @ُ�\�$@4G-�db�x���1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C�fC
  C�C�C  C  C  C�fC  C  C  C�C   C"  C$�C&�C(  C*  C,  C.  C0  C2  C4  C6  C8  C9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Ca�fCd  Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D$��D%y�D%��D&� D'  D'� D(  D(� D)  D)� D*  D*�fD+  D+� D,  D,� D-  D-y�D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5fD5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D>��D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DK��DL� DMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dky�Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds�fDt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�3D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�C3Dك3D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�<�D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��A�\A&�\AF�\Af�\A�z�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B���B���B���B���B���B���B���B���B���B���B���B���B���B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�B���B���C h�Ch�Ch�Ch�CO\C
h�C��C��Ch�Ch�Ch�CO\Ch�Ch�Ch�C��C h�C"h�C$��C&��C(h�C*h�C,h�C.h�C0h�C2h�C4h�C6h�C8h�C:O\C<h�C>h�C@h�CBh�CDh�CFh�CHh�CJh�CLh�CNh�CPh�CRh�CTh�CVh�CXh�CZh�C\h�C^h�C`h�CbO\Cdh�CfO\Chh�Cjh�Clh�Cnh�Cph�Crh�Cth�Cvh�Cxh�Czh�C|h�C~h�C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�'�C�4{C�4{C�4{C�4{C�'�C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�'�C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�AHC�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{D =D �=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D	=D	�=D
=D
�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D =D �=D!=D!�=D"=D"�=D#=D#�=D$=D$�=D%�D%��D&�D&�=D'=D'�=D(=D(�=D)=D)�=D*=D*��D+=D+�=D,=D,�=D-=D-��D.=D.�=D/=D/�=D0=D0�=D1=D1�=D2=D2�=D3=D3�=D4=D4�=D5 �D5�=D6=D6�=D7=D7�=D8=D8�=D9=D9�=D:=D:�=D;=D;�=D<=D<�=D==D=�=D>=D>�=D?�D?�=D@=D@�=DA=DA�=DB=DB�=DC=DC�=DD=DD�=DE=DE�=DF=DF�=DG=DG�=DH=DH�=DI=DI�=DJ=DJ�=DK=DK�=DL�DL�=DM �DM�=DN=DN�=DO=DO�=DP=DP�=DQ=DQ�=DR=DR�=DS=DS�=DT=DT�=DU=DU�=DV=DV�=DW=DW�=DX=DX�=DY=DY�=DZ=DZ�=D[=D[�=D\=D\�=D]=D]�=D^=D^�=D_=D_�=D`=D`�=Da=Da�=Db=Db�=Dc=Dc�=Dd=Dd�=De=De�=Df=Df�=Dg=Dg�=Dh=Dh�=Di=Di�=Dj=Dj�=Dk=Dk��Dl=Dl�=Dm=Dm�=Dn=Dn�=Do=Do�=Dp=Dp�=Dq=Dq�=Dr=Dr�=Ds=Ds��Dt=Dt�=Du=Du�=Dv=Dv�=Dw=Dw�=Dx=Dx�=Dy=Dy�=Dz=Dz�=D{=D{�=D|=D|�=D}=D}�=D~=D~�=D=D�=D�D�MD��D��D�D�MD��D��D�D�MD��D���D�D�MD��D��RD�D�MD��D��D�D�MD��D��D�D�I�D��D��D�D�MD��D��D�D�MD��D��D�D�MD��RD��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�I�D��D��D�D�I�D��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��RD��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD���D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D���D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�	�D�I�D��D���D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�PRD��D��D�D�MD��D��D�D�MD��D��D�D�MDD��D�RD�MDÍD��D�D�MDčD��D�D�MDōD��D�D�MDƍD��D�D�MDǍD��D�D�MDȍD��D�D�MDɍD��D�D�MDʍD��D�D�MDˍD��D�D�MD̍D��D�D�MD͍D��D�D�MD΍D��D�D�MDύD��D�D�MDЍD��D�D�MDэD��D�D�MDҍD��D�D�MDӍD��D�D�MDԍD��D�D�MDՍD��D�D�MD֍D��D�D�MD׍D��D�D�MD؍D��D�D�PRDِRD��D�D�MDڍD��D�D�MDۍD��D�D�I�D܍D��D�D�MDݍD��D�D�MDލD��D�D�MDߍD��D�D�MD��D���D�D�MD�D��D�D�MD�D��D�D�MD�D��D�D�MD�D��D�D�MD�D��D�	�D�MD�D��D�D�MD�D��D�D�MD�D��D�D�MD�D��D�D�MD�D��D�D�MD�D��D�D�MD�D��D�D�MD�D��D�D�MD�D��D�D�MD�D��D�D�MD��D��D�D�MD�D��D�D�MD�D��D�D�MD�D��D�D�MD�D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�PRD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�|�A�|�Aۇ+Aۇ+Aۉ7Aۉ7AۅAۇ+AۅAہAہA�n�A�hsA�dZA�K�A�-A�bA��A��TA���A�M�A��
A�`BA�/A�"�A��/A֙�A�l�A��A�(�A���A�v�A��yA�hsA��mA�/A�33A��TA�~�AΟ�A�~�AΕ�A�XAͲ-A���A�t�A��`A˩�A�\)A��yA�^5A��Aɴ9Aɟ�A��AȰ!A�ĜA��A�~�A�(�Aũ�Aď\A��
A�VA���A�=qA��-A���A���A��yA��A���A�x�A�oA���A��jA���A���A�A�A�dZA�;dA��!A�ffA�ffA���A���A�7LA�/A�ZA���A�z�A�K�A��A���A��RA�I�A���A�C�A�A�A�n�A���A� �A��!A�t�A�XA��A���A�`BA�ȴA��PA�ZA��/A�E�A��A�1A�~�A�ZA�33A�1'A�XA�M�A�  A�`BA��A�JA}��A| �Av�Ar �Am�-AiO�Ae�TAc�TAa�A]G�AX�yAV=qAT��ASO�ARM�AQ
=AO��AL=qAKoAJ��AIG�AGAFbNAD{ABZA@��A@v�A?�#A?VA>��A=ƨA<��A;�PA:�+A8�`A7��A6��A4ZA1�;A0ȴA0^5A/�A.ȴA-�A-�A+A*�A%�A �jA�AƨA�At�A�jA��A�!A�A�/A�DAM�AhsA�AA"�A�+A��A�A�-AhsAG�A�yAjA��A�`A��A^5A�uA�!A�A��AbAO�AVA{A
v�A
�A	33A��A�/Az�A�A�;A�hA?}A�A��A��A��A  A~�AbNAv�Az�A�AZA��A 1'@�K�@�?}@�b@�@�E�@�?}@��;@�1'@�C�@�@��@�1'@�7L@�;d@��@��H@޸R@�5?@��@��`@�dZ@���@�
=@��@��m@��#@϶F@�V@͉7@·+@��T@�/@���@�V@�%@�9X@��#@�
=@��H@���@���@�o@��#@��@�7L@�Ĝ@��@�O�@�K�@��;@�  @Ɵ�@�|�@\@��`@��w@��@��@��+@�M�@���@�7L@��@�b@��@��@�o@�^5@��@��`@���@��u@�bN@�\)@�E�@��-@��7@��@� �@�b@��;@���@���@���@��H@���@���@��+@�v�@���@��^@���@���@�@��\@�5?@���@�?}@���@��m@�33@�^5@�E�@�-@��@��#@���@��-@��-@��-@��h@���@� �@� �@���@���@���@�~�@�n�@�~�@��!@���@�~�@���@��R@���@�ȴ@���@��@�`B@���@�z�@�A�@�1@�K�@�"�@��H@��!@�n�@���@��h@�X@��9@��@� �@��P@��P@��@�S�@�"�@���@���@�n�@�-@�`B@��D@�Z@� �@��@��;@�(�@���@��w@��@���@�|�@�\)@�@��R@�~�@�E�@���@��-@�p�@�7L@��`@��9@��@���@�bN@�1'@��w@�dZ@���@��@���@���@���@�&�@��@��u@�Q�@�1@���@���@�ƨ@���@��@�;d@�o@�@���@���@���@�n�@��@���@���@��@�O�@��@��/@��9@�j@���@��@��y@��@���@��+@�ff@�$�@��^@���@��h@�7L@��`@��9@�z�@���@��F@�K�@��@��y@�ȴ@�V@�-@��T@��^@�x�@�%@��@�Z@�A�@�A�@�(�@���@���@��P@�t�@�S�@�33@���@���@�V@���@�hs@�`B@�&�@��@�Ĝ@���@��D@�r�@�Q�@���@��@�|�@�dZ@�@��H@��R@�^5@�E�@�{@��#@��h@�x�@�`B@�hs@�O�@�7L@���@��`@�Ĝ@�j@�1'@�(�@�  @��;@��@�l�@�C�@���@��R@���@�V@�-@��@��T@���@�X@�%@��j@��@��@��@��@�9X@� �@��@�b@�;@;d@~ȴ@~V@~@}�-@}O�@|�j@|j@|(�@{�F@{dZ@{o@z~�@y��@yX@y�@x�9@x1'@w��@wl�@w
=@v�R@vV@v5?@u�-@u`B@t�@tj@t�@s�
@s�F@sC�@r-@qG�@p�`@pĜ@p�@o�@o�w@o;d@o
=@o
=@nȴ@nv�@l�@kdZ@k"�@k@j�H@j��@j�!@jn�@jn�@jM�@jJ@i�@i��@iX@h��@h��@h��@g�@gK�@f��@e��@e�h@e��@e��@eO�@e?}@d�/@dZ@c�
@c�@cC�@b�@b�!@bJ@a�^@a�7@ahs@a%@`bN@` �@_�@_;d@^��@^��@^��@^�y@^��@^{@]�@]�-@]?}@]V@\Z@[��@["�@Z��@Z~�@Z=q@Y�@YX@X�@XQ�@X �@W�;@W|�@Vȴ@V��@V�+@V$�@V@U�T@U`B@UV@T�/@T9X@S��@SS�@R�@R��@R~�@Q�#@Q��@Q�^@Qhs@QG�@P��@P�@P  @O��@O�@N��@N{@M��@M�h@MV@L�@L�/@L�@L��@L�D@Lj@L1@K�
@K��@K@J�H@J��@J�!@JJ@I��@Ix�@I7L@H��@H�`@H��@Hr�@G�w@G+@F�R@Fv�@FE�@E�@E�-@E�@E?}@D�/@Dz�@D(�@C�m@C��@C@B�!@B^5@B�@A�#@A�^@AX@@��@@Ĝ@@bN@@ �@?�@?�w@?\)@?
=@>�y@>ȴ@>V@>{@=�@=?}@=V@<�j@<j@<9X@;��@;��@;"�@:�!@:M�@:-@9�@9x�@8�`@8�9@8��@8�u@8r�@8A�@7�@7�@7|�@7+@6��@6�@6��@6v�@65?@5��@5�-@5��@5�@5O�@5?}@4��@4�@4z�@4j@4Z@4(�@3�m@3��@3t�@3o@2��@2��@2~�@2n�@2M�@1�@1x�@1&�@1%@0�`@0��@0�9@0�@0 �@/�@/;d@/�@.�y@.�@.ȴ@.�R@.��@.v�@.$�@-@-�h@-p�@-�@,��@,j@,�@+�
@+ƨ@+�F@+"�@*��@*n�@*�@*J@)�@)�#@)�^@)�7@)G�@)�@(�`@(�9@(r�@(  @'��@'�P@'|�@'K�@'
=@&��@&ff@&V@&5?@&{@%�@%@%`B@%/@$�@$z�@$�@#��@#�@#dZ@#dZ@#S�@#33@"�H@"��@"�\@"-@!��@!��@!��@!X@!&�@!%@ ��@ ��@ �@ bN@ 1'@ b@�w@\)@K�@
=@�@�+@ff@5?@�@�T@@�h@?}@V@��@�@j@I�@9X@��@�@dZ@S�@S�@S�@C�@�H@^5@-@�@�^@��@�7@x�@hs@X@G�@�`@Ĝ@��@A�@ �@b@  @�@�w@�P@l�@;d@�@�@��@�+@v�@V@E�@@��@�-@�h@�h@�@O�@�@��@�j@z�@Z@Z@I�@I�@9X@(�@1@�
@��@dZ@33@o@��@�!@�\@^5@-@�@J@�#@�^@��@x�@G�@�@%@Ĝ@��@bN@A�@1'@ �@�@��@��@��@|�@\)@�@�y@ȴ@��@��@V@5?@$�@$�@{@�@�-@`B@/@�@�@�j@��@�D@j@j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�|�A�|�Aۇ+Aۇ+Aۉ7Aۉ7AۅAۇ+AۅAہAہA�n�A�hsA�dZA�K�A�-A�bA��A��TA���A�M�A��
A�`BA�/A�"�A��/A֙�A�l�A��A�(�A���A�v�A��yA�hsA��mA�/A�33A��TA�~�AΟ�A�~�AΕ�A�XAͲ-A���A�t�A��`A˩�A�\)A��yA�^5A��Aɴ9Aɟ�A��AȰ!A�ĜA��A�~�A�(�Aũ�Aď\A��
A�VA���A�=qA��-A���A���A��yA��A���A�x�A�oA���A��jA���A���A�A�A�dZA�;dA��!A�ffA�ffA���A���A�7LA�/A�ZA���A�z�A�K�A��A���A��RA�I�A���A�C�A�A�A�n�A���A� �A��!A�t�A�XA��A���A�`BA�ȴA��PA�ZA��/A�E�A��A�1A�~�A�ZA�33A�1'A�XA�M�A�  A�`BA��A�JA}��A| �Av�Ar �Am�-AiO�Ae�TAc�TAa�A]G�AX�yAV=qAT��ASO�ARM�AQ
=AO��AL=qAKoAJ��AIG�AGAFbNAD{ABZA@��A@v�A?�#A?VA>��A=ƨA<��A;�PA:�+A8�`A7��A6��A4ZA1�;A0ȴA0^5A/�A.ȴA-�A-�A+A*�A%�A �jA�AƨA�At�A�jA��A�!A�A�/A�DAM�AhsA�AA"�A�+A��A�A�-AhsAG�A�yAjA��A�`A��A^5A�uA�!A�A��AbAO�AVA{A
v�A
�A	33A��A�/Az�A�A�;A�hA?}A�A��A��A��A  A~�AbNAv�Az�A�AZA��A 1'@�K�@�?}@�b@�@�E�@�?}@��;@�1'@�C�@�@��@�1'@�7L@�;d@��@��H@޸R@�5?@��@��`@�dZ@���@�
=@��@��m@��#@϶F@�V@͉7@·+@��T@�/@���@�V@�%@�9X@��#@�
=@��H@���@���@�o@��#@��@�7L@�Ĝ@��@�O�@�K�@��;@�  @Ɵ�@�|�@\@��`@��w@��@��@��+@�M�@���@�7L@��@�b@��@��@�o@�^5@��@��`@���@��u@�bN@�\)@�E�@��-@��7@��@� �@�b@��;@���@���@���@��H@���@���@��+@�v�@���@��^@���@���@�@��\@�5?@���@�?}@���@��m@�33@�^5@�E�@�-@��@��#@���@��-@��-@��-@��h@���@� �@� �@���@���@���@�~�@�n�@�~�@��!@���@�~�@���@��R@���@�ȴ@���@��@�`B@���@�z�@�A�@�1@�K�@�"�@��H@��!@�n�@���@��h@�X@��9@��@� �@��P@��P@��@�S�@�"�@���@���@�n�@�-@�`B@��D@�Z@� �@��@��;@�(�@���@��w@��@���@�|�@�\)@�@��R@�~�@�E�@���@��-@�p�@�7L@��`@��9@��@���@�bN@�1'@��w@�dZ@���@��@���@���@���@�&�@��@��u@�Q�@�1@���@���@�ƨ@���@��@�;d@�o@�@���@���@���@�n�@��@���@���@��@�O�@��@��/@��9@�j@���@��@��y@��@���@��+@�ff@�$�@��^@���@��h@�7L@��`@��9@�z�@���@��F@�K�@��@��y@�ȴ@�V@�-@��T@��^@�x�@�%@��@�Z@�A�@�A�@�(�@���@���@��P@�t�@�S�@�33@���@���@�V@���@�hs@�`B@�&�@��@�Ĝ@���@��D@�r�@�Q�@���@��@�|�@�dZ@�@��H@��R@�^5@�E�@�{@��#@��h@�x�@�`B@�hs@�O�@�7L@���@��`@�Ĝ@�j@�1'@�(�@�  @��;@��@�l�@�C�@���@��R@���@�V@�-@��@��T@���@�X@�%@��j@��@��@��@��@�9X@� �@��@�b@�;@;d@~ȴ@~V@~@}�-@}O�@|�j@|j@|(�@{�F@{dZ@{o@z~�@y��@yX@y�@x�9@x1'@w��@wl�@w
=@v�R@vV@v5?@u�-@u`B@t�@tj@t�@s�
@s�F@sC�@r-@qG�@p�`@pĜ@p�@o�@o�w@o;d@o
=@o
=@nȴ@nv�@l�@kdZ@k"�@k@j�H@j��@j�!@jn�@jn�@jM�@jJ@i�@i��@iX@h��@h��@h��@g�@gK�@f��@e��@e�h@e��@e��@eO�@e?}@d�/@dZ@c�
@c�@cC�@b�@b�!@bJ@a�^@a�7@ahs@a%@`bN@` �@_�@_;d@^��@^��@^��@^�y@^��@^{@]�@]�-@]?}@]V@\Z@[��@["�@Z��@Z~�@Z=q@Y�@YX@X�@XQ�@X �@W�;@W|�@Vȴ@V��@V�+@V$�@V@U�T@U`B@UV@T�/@T9X@S��@SS�@R�@R��@R~�@Q�#@Q��@Q�^@Qhs@QG�@P��@P�@P  @O��@O�@N��@N{@M��@M�h@MV@L�@L�/@L�@L��@L�D@Lj@L1@K�
@K��@K@J�H@J��@J�!@JJ@I��@Ix�@I7L@H��@H�`@H��@Hr�@G�w@G+@F�R@Fv�@FE�@E�@E�-@E�@E?}@D�/@Dz�@D(�@C�m@C��@C@B�!@B^5@B�@A�#@A�^@AX@@��@@Ĝ@@bN@@ �@?�@?�w@?\)@?
=@>�y@>ȴ@>V@>{@=�@=?}@=V@<�j@<j@<9X@;��@;��@;"�@:�!@:M�@:-@9�@9x�@8�`@8�9@8��@8�u@8r�@8A�@7�@7�@7|�@7+@6��@6�@6��@6v�@65?@5��@5�-@5��@5�@5O�@5?}@4��@4�@4z�@4j@4Z@4(�@3�m@3��@3t�@3o@2��@2��@2~�@2n�@2M�@1�@1x�@1&�@1%@0�`@0��@0�9@0�@0 �@/�@/;d@/�@.�y@.�@.ȴ@.�R@.��@.v�@.$�@-@-�h@-p�@-�@,��@,j@,�@+�
@+ƨ@+�F@+"�@*��@*n�@*�@*J@)�@)�#@)�^@)�7@)G�@)�@(�`@(�9@(r�@(  @'��@'�P@'|�@'K�@'
=@&��@&ff@&V@&5?@&{@%�@%@%`B@%/@$�@$z�@$�@#��@#�@#dZ@#dZ@#S�@#33@"�H@"��@"�\@"-@!��@!��@!��@!X@!&�@!%@ ��@ ��@ �@ bN@ 1'@ b@�w@\)@K�@
=@�@�+@ff@5?@�@�T@@�h@?}@V@��@�@j@I�@9X@��@�@dZ@S�@S�@S�@C�@�H@^5@-@�@�^@��@�7@x�@hs@X@G�@�`@Ĝ@��@A�@ �@b@  @�@�w@�P@l�@;d@�@�@��@�+@v�@V@E�@@��@�-@�h@�h@�@O�@�@��@�j@z�@Z@Z@I�@I�@9X@(�@1@�
@��@dZ@33@o@��@�!@�\@^5@-@�@J@�#@�^@��@x�@G�@�@%@Ĝ@��@bN@A�@1'@ �@�@��@��@��@|�@\)@�@�y@ȴ@��@��@V@5?@$�@$�@{@�@�-@`B@/@�@�@�j@��@�D@j@j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�PB
�VB
�VB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�bB
�B
y�B
bNB
R�B
N�B
M�B
[#B
VB
E�B
5?B
1'B
1'B
49B
49B
2-B
+B
(�B
&�B
#�B
(�B
A�B
VB
W
B
YB
\)B
cTB
hsB
~�B
�\B
��B
�!B
�RB
�/B
��BPB49BM�B`BBn�Bz�B�VB��B��B��B�mB��B�ZB�B��B��B�B�B�B��B+B#�B�B�B{B	7B
=B��B�B�B�B�mB�/B�B��B��B��B��BǮBÖB�wB�B��B�By�Br�BjBe`BcTBbNB`BB]/BO�BD�B9XB0!B'�B �BVB
��B
�`B
�HB
��B
��B
��B
�DB
|�B
v�B
n�B
[#B
B�B
/B
	7B	�fB	ÖB	��B	��B	�1B	~�B	hsB	ZB	O�B	K�B	G�B	E�B	?}B	<jB	=qB	<jB	?}B	@�B	D�B	M�B	Q�B	ZB	_;B	_;B	cTB	e`B	dZB	ffB	k�B	o�B	o�B	k�B	dZB	bNB	VB	L�B	H�B	O�B	T�B	`BB	aHB	e`B	YB	T�B	?}B	%B�B�ZB�B��B�B�ZB�`B�ZB�BB�/B�/B�)B�#B�#B�NB�B�B��B	B	+B	VB	�B	�B	�B	�B	bB	PB	�B	!�B	#�B	�B	!�B	#�B	!�B	�B	hB	bB	DB	+B	hB	oB	\B	uB	�B	�B	�B	{B	oB	�B	$�B	8RB	:^B	<jB	B�B	;dB	33B	)�B	�B	+B�B��BƨB��B�jB�RB�qB�wB�jB�dB�XB�^B�dB�dB�jB�wBÖB��B�qB��BŢBƨBƨB��B�^B�FB�-B�?BÖB��B��B��B�
B�/B�BB	PB	�B	�B	 �B	#�B	$�B	"�B	#�B	)�B	)�B	'�B	(�B	$�B	,B	-B	)�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	$�B	&�B	(�B	+B	/B	2-B	2-B	2-B	33B	9XB	;dB	<jB	>wB	?}B	=qB	=qB	>wB	?}B	B�B	F�B	F�B	H�B	I�B	K�B	L�B	M�B	N�B	N�B	R�B	VB	[#B	\)B	`BB	ffB	ffB	hsB	hsB	gmB	hsB	iyB	jB	m�B	p�B	q�B	r�B	s�B	u�B	v�B	w�B	y�B	{�B	|�B	|�B	}�B	}�B	�B	�B	�+B	�+B	�7B	�VB	�bB	�oB	�uB	�{B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�3B	�3B	�LB	�RB	�^B	�dB	�jB	�}B	B	B	ÖB	ŢB	ĜB	ĜB	ĜB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�)B	�5B	�5B	�BB	�BB	�HB	�HB	�TB	�ZB	�`B	�`B	�`B	�mB	�yB	�B	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
%B
+B
1B
1B
	7B

=B
DB
DB
JB
PB
VB
\B
\B
\B
\B
bB
bB
hB
hB
hB
hB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
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
+B
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
.B
/B
/B
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
33B
49B
49B
49B
49B
49B
5?B
6FB
6FB
7LB
7LB
7LB
8RB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
8RB
8RB
9XB
9XB
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
=qB
=qB
=qB
<jB
<jB
=qB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
B�B
B�B
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
F�B
F�B
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
G�B
G�B
G�B
G�B
G�B
F�B
F�B
G�B
G�B
G�B
H�B
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
M�B
M�B
M�B
N�B
N�B
N�B
N�B
M�B
N�B
N�B
N�B
N�B
M�B
N�B
O�B
O�B
P�B
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
\)B
\)B
\)B
\)B
\)B
\)B
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
`BB
`BB
`BB
`BB
`BB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
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
jB
jB
jB
jB
jB
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
m�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
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
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
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
x�B
x�B
x�B
x�B
x�B
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
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�D1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�PB
�VB
�VB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�bB
�B
y�B
bNB
R�B
N�B
M�B
[#B
VB
E�B
5?B
1'B
1'B
49B
49B
2-B
+B
(�B
&�B
#�B
(�B
A�B
VB
W
B
YB
\)B
cTB
hsB
~�B
�\B
��B
�!B
�RB
�/B
��BPB49BM�B`BBn�Bz�B�VB��B��B��B�mB��B�ZB�B��B��B�B�B�B��B+B#�B�B�B{B	7B
=B��B�B�B�B�mB�/B�B��B��B��B��BǮBÖB�wB�B��B�By�Br�BjBe`BcTBbNB`BB]/BO�BD�B9XB0!B'�B �BVB
��B
�`B
�HB
��B
��B
��B
�DB
|�B
v�B
n�B
[#B
B�B
/B
	7B	�fB	ÖB	��B	��B	�1B	~�B	hsB	ZB	O�B	K�B	G�B	E�B	?}B	<jB	=qB	<jB	?}B	@�B	D�B	M�B	Q�B	ZB	_;B	_;B	cTB	e`B	dZB	ffB	k�B	o�B	o�B	k�B	dZB	bNB	VB	L�B	H�B	O�B	T�B	`BB	aHB	e`B	YB	T�B	?}B	%B�B�ZB�B��B�B�ZB�`B�ZB�BB�/B�/B�)B�#B�#B�NB�B�B��B	B	+B	VB	�B	�B	�B	�B	bB	PB	�B	!�B	#�B	�B	!�B	#�B	!�B	�B	hB	bB	DB	+B	hB	oB	\B	uB	�B	�B	�B	{B	oB	�B	$�B	8RB	:^B	<jB	B�B	;dB	33B	)�B	�B	+B�B��BƨB��B�jB�RB�qB�wB�jB�dB�XB�^B�dB�dB�jB�wBÖB��B�qB��BŢBƨBƨB��B�^B�FB�-B�?BÖB��B��B��B�
B�/B�BB	PB	�B	�B	 �B	#�B	$�B	"�B	#�B	)�B	)�B	'�B	(�B	$�B	,B	-B	)�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	$�B	&�B	(�B	+B	/B	2-B	2-B	2-B	33B	9XB	;dB	<jB	>wB	?}B	=qB	=qB	>wB	?}B	B�B	F�B	F�B	H�B	I�B	K�B	L�B	M�B	N�B	N�B	R�B	VB	[#B	\)B	`BB	ffB	ffB	hsB	hsB	gmB	hsB	iyB	jB	m�B	p�B	q�B	r�B	s�B	u�B	v�B	w�B	y�B	{�B	|�B	|�B	}�B	}�B	�B	�B	�+B	�+B	�7B	�VB	�bB	�oB	�uB	�{B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�3B	�3B	�LB	�RB	�^B	�dB	�jB	�}B	B	B	ÖB	ŢB	ĜB	ĜB	ĜB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�)B	�5B	�5B	�BB	�BB	�HB	�HB	�TB	�ZB	�`B	�`B	�`B	�mB	�yB	�B	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
%B
+B
1B
1B
	7B

=B
DB
DB
JB
PB
VB
\B
\B
\B
\B
bB
bB
hB
hB
hB
hB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
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
+B
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
.B
/B
/B
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
33B
49B
49B
49B
49B
49B
5?B
6FB
6FB
7LB
7LB
7LB
8RB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
8RB
8RB
9XB
9XB
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
=qB
=qB
=qB
<jB
<jB
=qB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
B�B
B�B
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
F�B
F�B
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
G�B
G�B
G�B
G�B
G�B
F�B
F�B
G�B
G�B
G�B
H�B
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
M�B
M�B
M�B
N�B
N�B
N�B
N�B
M�B
N�B
N�B
N�B
N�B
M�B
N�B
O�B
O�B
P�B
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
\)B
\)B
\)B
\)B
\)B
\)B
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
`BB
`BB
`BB
`BB
`BB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
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
jB
jB
jB
jB
jB
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
m�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
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
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
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
x�B
x�B
x�B
x�B
x�B
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
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�D1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20210902214024  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20210902154126  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20210902154127  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20210902154127  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20210902154127  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20210902154127  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20210902154127  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20210902154127  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20210902154128  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20210902154128                      G�O�G�O�G�O�                JA  ARUP                                                                        20210902155234                      G�O�G�O�G�O�                