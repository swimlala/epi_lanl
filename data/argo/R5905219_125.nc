CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2021-09-13T12:42:13Z creation;2021-09-13T12:42:15Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �8   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �D   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �H   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �P   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210913124213  20210913125243  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               }A   JA                                  2B  A   APEX                            7906                            051216                          846 @ْQq�-1   @ْSH+�@4DZ�1�dN5?|�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�  B���B�  B�  B�  B�ffB���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C#�fC%�fC'�fC*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx�Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C��C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#� D$  D$� D%  D%� D&fD&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4�fD5  D5� D6  D6� D7  D7� D8  D8� D9fD9� D:  D:� D;  D;�fD<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG�fDH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D]��D^� D_  D_� D`  D`� Da  Da�fDbfDb� Dc  Dc� Dd  Dd� De  De� Df  Dfy�Dg  Dg� DhfDh�fDi  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�3D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ D�|�D�� D�  D�<�Dˀ D�� D�  D�@ D�|�D�� D�  D�@ D̀ D�� D�  D�<�D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�3D�C3DՀ Dռ�D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܃3D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�3D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D��3D�  D�@ D�� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�)�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�Q�@��A�\A&�\AF�\Af�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B

=B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�8RBԞ�B���Bܞ�B���B���B���B�8RB�B���B���B���C h�Ch�Ch�Ch�Ch�C
h�Ch�Ch�Ch�Ch�Ch�Ch�Ch�Ch�Ch�Ch�C h�C"h�C$O\C&O\C(O\C*h�C,h�C.h�C0h�C2h�C4h�C6h�C8h�C:h�C<��C>h�C@h�CBh�CDh�CFh�CHh�CJh�CLh�CNh�CPh�CRh�CTh�CVh�CXh�CZh�C\h�C^h�C`O\Cbh�Cdh�Cfh�Chh�Cjh�Clh�Cnh�Cph�Crh�Cth�Cv��Cx��Czh�C|h�C~h�C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�'�C�'�C�4{C�AHC�AHC�4{C�AHC�AHC�AHC�4{C�4{C�4{C�4{C�4{C�4{C�4{C�AHC�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�AHC�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�'�C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�AHC�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{D =D �=D=D�=D=D�=D=D�=D=D�=D�D�=D=D�=D=D�=D=D�=D	=D	�=D
=D
�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D��D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D =D �=D!=D!�=D"=D"�=D# �D#�=D$=D$�=D%=D%�=D& �D&��D'=D'�=D(=D(�=D)=D)�=D*=D*�=D+=D+�=D,=D,�=D-=D-�=D.=D.�=D/=D/�=D0=D0�=D1=D1�=D2=D2�=D3=D3�=D4=D4��D5=D5�=D6=D6�=D7=D7�=D8=D8�=D9 �D9�=D:=D:�=D;=D;��D<=D<�=D==D=�=D>=D>�=D?=D?�=D@=D@�=DA=DA�=DB=DB�=DC=DC�=DD=DD�=DE=DE�=DF=DF�=DG=DG��DH=DH�=DI=DI�=DJ=DJ�=DK=DK�=DL=DL�=DM=DM�=DN=DN�=DO=DO�=DP=DP�=DQ=DQ�=DR=DR�=DS=DS�=DT=DT�=DU=DU�=DV=DV�=DW=DW�=DX=DX�=DY=DY�=DZ=DZ�=D[=D[�=D\=D\�=D]=D]�=D^�D^�=D_=D_�=D`=D`�=Da=Da��Db �Db�=Dc=Dc�=Dd=Dd�=De=De�=Df=Df��Dg=Dg�=Dh �Dh��Di=Di�=Dj=Dj�=Dk=Dk�=Dl=Dl�=Dm=Dm�=Dn=Dn�=Do=Do�=Dp=Dp�=Dq=Dq�=Dr=Dr�=Ds=Ds�=Dt=Dt�=Du=Du�=Dv=Dv�=Dw=Dw�=Dx=Dx�=Dy=Dy�=Dz=Dz�=D{=D{�=D|=D|�=D}=D}�=D~=D~�=D=D�=D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D���D�D�MD��RD��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�	�D�MD��D���D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D���D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��RD��RD�D�MD���D��D�D�MD��D��D�D�I�D��D��D�D�PRD��D��D�D�MD��D��D�D�MD��D��D�D�PRD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�RD�MD��D��D�D�MD��D��D�D�MD��D��D�D�I�D���D���D�D�MD��D��D�D�MD��D��D�D�MD��RD��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�I�D��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��RD�D�I�D��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MDD��D�RD�MDÍD��D�D�MDčD��D�D�MDōD��D�D�MDƍD��D�D�MDǍD��D�D�MDȍD��D�D�MDɍD��D�D�MDʉ�D��D�D�I�DˍD��D�D�MD̉�D��D�D�MD͍D��D�D�I�D΍D��D�D�MDύD��D�D�MDЍD��D�D�MDэD��D�D�MDҍD��D�D�MDӍD��D�D�MDԍD��D�RD�PRDՍD���D�D�MD֍D��D�D�MD׍D��D�D�MD؍D��D�D�MDٍD��D�D�MDڍD��D�D�MDۍD��D�D�MDܐRD��D�D�MDݍD��D�D�MDލD��D�RD�MDߍD��D�D�MD��D��D�D�MD�D��D�D�MD�D��D�D�MD�D��D�D�MD�D��D�RD�MD�D��D�D�MD�D��D�D�MD�D��D�D�MD�D��D�D�MD�D��D�D�MD�D��D�D�MD�D��D�D�I�D�D��D�D�MD�D��D�D�MD�D��D�D�MD�RD��RD�D�MD��D��D�D�I�D�D��D�D�MD�D��D�D�MD�D��D�D�MD�D��RD�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�6�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�x�A�x�A�|�A�|�A�~�A�|�A�x�AۃA�~�A�~�A�z�AۅAہAۅA�~�A�x�A�v�A�r�A�hsA�VA�^5A�ZA�XA�VA�S�A�S�A�Q�A�O�A�O�A�O�A�O�A�O�A�O�A�M�A�M�A�M�A�M�A�K�A�M�A�M�A�M�A�K�A�G�A�;dA�33A�/A�"�AڑhA�I�A�K�A�\)AѾwA��A�ƨAΑhA���A�jAʲ-A�
=A�;dA�G�A��;A�dZAŧ�A��Aú^A�-A���A��`A��A�ƨA��A��A��A���A�G�A��A��\A�VA�=qA��7A��RA�A�ĜA��#A�;dA�$�A�"�A��A��/A���A���A� �A��A� �A��mA��!A�-A���A���A�p�A�`BA�(�A���A��RA��A��-A��PA��TA�?}A�dZA��mA�hsA��A�K�A�ZA�ƨA��A��HA�9XA�v�A�~�A���Ax�A{7LAw�PAr�AoG�Ak��Ag�-Ad�uA`z�A_S�A^bA[�AW�PAU�ATn�AS+ARI�AQ�FAQC�APE�AN�HAL�RAK�AI�#AI�AH�jAG�AFM�AD~�AB��A@��A?ƨA?�A=XA<n�A;�#A:ȴA9A8��A8 �A6=qA3��A2�yA2��A1�;A0r�A.z�A-;dA,A+��A)p�A(��A&bNA$�DA#�7A"�/A"=qA"n�A"��A"A�A!�wA"I�A!x�A I�AK�A�A5?A�Az�AE�Ax�A�A�AJA�hA�jA1A�yA;dAJA�A��AffA�Ax�A
�9A	��A�9A5?A�#AĜA�-AXAoA��A�yA��AO�A�A��A�\A~�A$�A�A/A ��A ��A �9A ��A �uA ��A r�@�ff@�/@���@��@��^@��m@��\@�hs@�9@��T@���@�7L@�`B@��@���@���@��`@�b@�p�@���@�K�@��H@�?}@�b@ۮ@ڰ!@ف@�dZ@�O�@�dZ@��@�{@�=q@�X@�&�@�r�@�(�@�(�@�(�@�(�@�A�@��;@��#@�x�@��@�~�@��@�@Ǯ@�9X@��
@���@ř�@ě�@�  @�dZ@��H@�=q@��m@�@�^5@���@�`B@�@���@�V@�=q@��F@�;d@���@��@�r�@�{@�C�@�t�@�@�E�@��h@�  @��H@��!@�ȴ@��R@���@��@��@��@��!@�ff@�5?@�-@���@���@�7L@�G�@�^5@�=q@��@�Ĝ@��@���@���@��@�r�@��
@���@�l�@��-@�/@��@��u@�j@�Q�@�  @��R@�V@��+@�V@��@��@���@�X@�%@�b@���@���@�|�@�dZ@�o@��H@�@�@��^@��^@�7L@�&�@���@���@��@� �@�  @�|�@�C�@��H@���@�n�@�-@�{@��#@��7@���@��@��D@�1'@�I�@���@���@��j@���@��@�j@�1'@��
@��F@��@�K�@�o@��H@���@�M�@�J@���@���@���@�%@��`@���@��/@��j@� �@��@�C�@�+@��!@�ff@�$�@���@��@�%@��@���@�|�@�\)@�dZ@�C�@�;d@���@�J@��^@�&�@��`@���@��m@��w@�|�@��y@���@�J@��7@�&�@���@�bN@�b@��;@��F@��@�C�@��!@�^5@�{@��T@���@�p�@�7L@���@���@��@�bN@�Q�@�I�@�1'@� �@� �@�b@�l�@�+@���@���@�n�@�E�@�J@���@���@�X@��`@��@�Z@�1'@��@��m@���@��@�C�@��@��@���@���@���@�M�@��@�@���@�X@�/@�%@��`@��/@���@��@�bN@�9X@�1@��@�33@���@��y@���@�v�@�$�@���@��@���@�hs@�?}@�&�@���@��@�Q�@�9X@��@��@��
@�ƨ@���@���@�t�@�;d@�ȴ@��R@��R@���@�-@���@��-@��7@��@�?}@���@�r�@� �@�b@��@~�@~$�@}�T@}�-@}�@|�/@|(�@{�F@{��@{��@{dZ@z�@z��@zM�@y��@y��@yX@x��@x�@w��@w
=@v�@vV@vv�@vE�@v{@u�T@u��@v5?@v{@u�T@u�-@uO�@t��@t�@t�D@tj@t(�@s�F@sC�@r��@rM�@q��@q��@q&�@p �@o�P@o
=@m�T@m`B@mV@l�@lI�@k��@k@j�\@j=q@i�@iG�@h��@h�9@h��@hr�@hbN@h  @g\)@f��@f��@fV@e`B@d��@d�j@d��@d��@d�D@d(�@c�
@cS�@c"�@c@b��@b�@b�H@b��@b�!@b=q@a��@a��@a�^@ax�@`�`@`Ĝ@`�u@`bN@`b@_+@^��@^�+@]�@]�-@]`B@]?}@]�@\��@\�D@\(�@[��@[�m@[��@[t�@[S�@["�@Z��@Zn�@ZM�@Z=q@Z-@Y�@Y��@Y�7@Y&�@Y�@Y�@XĜ@XQ�@W�;@W��@W�@V��@Vv�@VV@V$�@V@U��@U/@T��@T�D@Tz�@TZ@T9X@S�m@St�@SC�@S33@S@Q�@Q�7@QX@Q7L@P��@P�u@Pr�@P1'@P  @O�;@O;d@N�R@N�+@Nv�@NE�@N$�@M�T@M�@M�@L�@L��@L�@K�
@Kƨ@Kt�@K33@J�@J�H@J^5@I��@I%@H�9@HbN@Hb@G��@G|�@G+@F�@FE�@F{@E��@E�@EO�@D��@D��@DI�@C�m@CC�@B�!@B~�@Bn�@BM�@B-@A��@A7L@A&�@@�`@@1'@?�@?��@?;d@?+@?
=@>ȴ@>��@=�@=@=/@<�D@<Z@<(�@;�m@;ƨ@;�@;o@;@;o@:��@9�^@97L@8�`@8�9@7�@7�@7��@7�P@7+@6�@6V@65?@6$�@6{@5@5�h@5O�@5O�@5�@4j@3�m@3�
@3�@3C�@2�H@2��@2-@1&�@0��@0r�@0b@/��@/;d@/
=@.�y@.�y@.��@.E�@.$�@.{@.{@.{@-O�@-?}@-O�@-?}@-V@,��@,��@,I�@+ƨ@+t�@+dZ@+C�@+"�@+o@+@*��@*n�@*J@)��@)x�@)hs@)G�@)&�@)%@(��@(��@(Ĝ@(�@(b@'�;@'�w@'�@'�P@'\)@'+@&��@&�@&ȴ@&�R@&��@&v�@&V@&$�@&@%@%��@%�h@%p�@%O�@$�@$��@$I�@#�
@#�@#dZ@#"�@"�H@"�\@"n�@"M�@"-@!�@!��@!�7@!hs@!&�@!%@ ��@ ��@ r�@ bN@ b@�w@�P@l�@;d@�@��@�y@�y@ȴ@��@E�@�T@�@`B@O�@/@�@�/@�@z�@�@�
@��@t�@33@@��@��@��@n�@M�@J@��@�^@hs@&�@��@�9@��@��@bN@A�@ �@b@�@�w@;d@�y@��@ff@5?@�@�T@@�@�@Z@1@�
@��@��@�@t�@C�@�H@~�@M�@=q@-@��@�#@��@�^@��@hs@X@7L@%@�`@�@Q�@1'@ �@  @�w@��@|�@K�@;d@+@�@��@�@��@v�@E�@�@@�@`B@O�@�@�/@�j@�@��@��@z�@(�@�
@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�x�A�x�A�|�A�|�A�~�A�|�A�x�AۃA�~�A�~�A�z�AۅAہAۅA�~�A�x�A�v�A�r�A�hsA�VA�^5A�ZA�XA�VA�S�A�S�A�Q�A�O�A�O�A�O�A�O�A�O�A�O�A�M�A�M�A�M�A�M�A�K�A�M�A�M�A�M�A�K�A�G�A�;dA�33A�/A�"�AڑhA�I�A�K�A�\)AѾwA��A�ƨAΑhA���A�jAʲ-A�
=A�;dA�G�A��;A�dZAŧ�A��Aú^A�-A���A��`A��A�ƨA��A��A��A���A�G�A��A��\A�VA�=qA��7A��RA�A�ĜA��#A�;dA�$�A�"�A��A��/A���A���A� �A��A� �A��mA��!A�-A���A���A�p�A�`BA�(�A���A��RA��A��-A��PA��TA�?}A�dZA��mA�hsA��A�K�A�ZA�ƨA��A��HA�9XA�v�A�~�A���Ax�A{7LAw�PAr�AoG�Ak��Ag�-Ad�uA`z�A_S�A^bA[�AW�PAU�ATn�AS+ARI�AQ�FAQC�APE�AN�HAL�RAK�AI�#AI�AH�jAG�AFM�AD~�AB��A@��A?ƨA?�A=XA<n�A;�#A:ȴA9A8��A8 �A6=qA3��A2�yA2��A1�;A0r�A.z�A-;dA,A+��A)p�A(��A&bNA$�DA#�7A"�/A"=qA"n�A"��A"A�A!�wA"I�A!x�A I�AK�A�A5?A�Az�AE�Ax�A�A�AJA�hA�jA1A�yA;dAJA�A��AffA�Ax�A
�9A	��A�9A5?A�#AĜA�-AXAoA��A�yA��AO�A�A��A�\A~�A$�A�A/A ��A ��A �9A ��A �uA ��A r�@�ff@�/@���@��@��^@��m@��\@�hs@�9@��T@���@�7L@�`B@��@���@���@��`@�b@�p�@���@�K�@��H@�?}@�b@ۮ@ڰ!@ف@�dZ@�O�@�dZ@��@�{@�=q@�X@�&�@�r�@�(�@�(�@�(�@�(�@�A�@��;@��#@�x�@��@�~�@��@�@Ǯ@�9X@��
@���@ř�@ě�@�  @�dZ@��H@�=q@��m@�@�^5@���@�`B@�@���@�V@�=q@��F@�;d@���@��@�r�@�{@�C�@�t�@�@�E�@��h@�  @��H@��!@�ȴ@��R@���@��@��@��@��!@�ff@�5?@�-@���@���@�7L@�G�@�^5@�=q@��@�Ĝ@��@���@���@��@�r�@��
@���@�l�@��-@�/@��@��u@�j@�Q�@�  @��R@�V@��+@�V@��@��@���@�X@�%@�b@���@���@�|�@�dZ@�o@��H@�@�@��^@��^@�7L@�&�@���@���@��@� �@�  @�|�@�C�@��H@���@�n�@�-@�{@��#@��7@���@��@��D@�1'@�I�@���@���@��j@���@��@�j@�1'@��
@��F@��@�K�@�o@��H@���@�M�@�J@���@���@���@�%@��`@���@��/@��j@� �@��@�C�@�+@��!@�ff@�$�@���@��@�%@��@���@�|�@�\)@�dZ@�C�@�;d@���@�J@��^@�&�@��`@���@��m@��w@�|�@��y@���@�J@��7@�&�@���@�bN@�b@��;@��F@��@�C�@��!@�^5@�{@��T@���@�p�@�7L@���@���@��@�bN@�Q�@�I�@�1'@� �@� �@�b@�l�@�+@���@���@�n�@�E�@�J@���@���@�X@��`@��@�Z@�1'@��@��m@���@��@�C�@��@��@���@���@���@�M�@��@�@���@�X@�/@�%@��`@��/@���@��@�bN@�9X@�1@��@�33@���@��y@���@�v�@�$�@���@��@���@�hs@�?}@�&�@���@��@�Q�@�9X@��@��@��
@�ƨ@���@���@�t�@�;d@�ȴ@��R@��R@���@�-@���@��-@��7@��@�?}@���@�r�@� �@�b@��@~�@~$�@}�T@}�-@}�@|�/@|(�@{�F@{��@{��@{dZ@z�@z��@zM�@y��@y��@yX@x��@x�@w��@w
=@v�@vV@vv�@vE�@v{@u�T@u��@v5?@v{@u�T@u�-@uO�@t��@t�@t�D@tj@t(�@s�F@sC�@r��@rM�@q��@q��@q&�@p �@o�P@o
=@m�T@m`B@mV@l�@lI�@k��@k@j�\@j=q@i�@iG�@h��@h�9@h��@hr�@hbN@h  @g\)@f��@f��@fV@e`B@d��@d�j@d��@d��@d�D@d(�@c�
@cS�@c"�@c@b��@b�@b�H@b��@b�!@b=q@a��@a��@a�^@ax�@`�`@`Ĝ@`�u@`bN@`b@_+@^��@^�+@]�@]�-@]`B@]?}@]�@\��@\�D@\(�@[��@[�m@[��@[t�@[S�@["�@Z��@Zn�@ZM�@Z=q@Z-@Y�@Y��@Y�7@Y&�@Y�@Y�@XĜ@XQ�@W�;@W��@W�@V��@Vv�@VV@V$�@V@U��@U/@T��@T�D@Tz�@TZ@T9X@S�m@St�@SC�@S33@S@Q�@Q�7@QX@Q7L@P��@P�u@Pr�@P1'@P  @O�;@O;d@N�R@N�+@Nv�@NE�@N$�@M�T@M�@M�@L�@L��@L�@K�
@Kƨ@Kt�@K33@J�@J�H@J^5@I��@I%@H�9@HbN@Hb@G��@G|�@G+@F�@FE�@F{@E��@E�@EO�@D��@D��@DI�@C�m@CC�@B�!@B~�@Bn�@BM�@B-@A��@A7L@A&�@@�`@@1'@?�@?��@?;d@?+@?
=@>ȴ@>��@=�@=@=/@<�D@<Z@<(�@;�m@;ƨ@;�@;o@;@;o@:��@9�^@97L@8�`@8�9@7�@7�@7��@7�P@7+@6�@6V@65?@6$�@6{@5@5�h@5O�@5O�@5�@4j@3�m@3�
@3�@3C�@2�H@2��@2-@1&�@0��@0r�@0b@/��@/;d@/
=@.�y@.�y@.��@.E�@.$�@.{@.{@.{@-O�@-?}@-O�@-?}@-V@,��@,��@,I�@+ƨ@+t�@+dZ@+C�@+"�@+o@+@*��@*n�@*J@)��@)x�@)hs@)G�@)&�@)%@(��@(��@(Ĝ@(�@(b@'�;@'�w@'�@'�P@'\)@'+@&��@&�@&ȴ@&�R@&��@&v�@&V@&$�@&@%@%��@%�h@%p�@%O�@$�@$��@$I�@#�
@#�@#dZ@#"�@"�H@"�\@"n�@"M�@"-@!�@!��@!�7@!hs@!&�@!%@ ��@ ��@ r�@ bN@ b@�w@�P@l�@;d@�@��@�y@�y@ȴ@��@E�@�T@�@`B@O�@/@�@�/@�@z�@�@�
@��@t�@33@@��@��@��@n�@M�@J@��@�^@hs@&�@��@�9@��@��@bN@A�@ �@b@�@�w@;d@�y@��@ff@5?@�@�T@@�@�@Z@1@�
@��@��@�@t�@C�@�H@~�@M�@=q@-@��@�#@��@�^@��@hs@X@7L@%@�`@�@Q�@1'@ �@  @�w@��@|�@K�@;d@+@�@��@�@��@v�@E�@�@@�@`B@O�@�@�/@�j@�@��@��@z�@(�@�
@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�FB
�LB
�FB
�?B
�FB
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�FB
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�9B
�3B
�-B
�'B
��B
�DB
aHB
k�B
��B
��B  B
��B
��B  B;dBW
B�B�B�B��BB+B'�BH�BK�B~�B|�B�B�oB�B�'B�B�B�B��BN�BhB+BB  B��B�B�BB��B�XB��B�B�9BĜB��BǮB�XB�dB�^B�B�B��B�oB�+Bu�BaHBR�BM�BE�BB�B=qB6FB+B�BPB
�B
��B
��B
ĜB
�RB
�B
��B
�JB
y�B
n�B
Q�B
6FB
�B	��B	�)B	ȴB	��B	��B	}�B	v�B	o�B	e`B	VB	O�B	L�B	L�B	N�B	L�B	J�B	H�B	H�B	E�B	B�B	B�B	F�B	H�B	G�B	F�B	;dB	:^B	C�B	H�B	I�B	J�B	K�B	O�B	XB	ffB	ffB	e`B	cTB	XB	Q�B	P�B	R�B	N�B	D�B	@�B	9XB	49B	)�B	�B	�B	+B	B	B��B	1B	 �B	!�B	�B	8RB	33B	2-B	�B	PB	%B��B��B��B	  B��B�B�B�yB�yB�fB�HB�#B�B��B��B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�/B�HB�B��B�B�B�B�B�B�B�B��B�B�5B�5B��B�B�mB�sB�B�fB�TB�#B�B��B�
B��B��B��B��B��BɺBŢBB��BŢB��B��B�B�B�B�B�B�
B�5B�NB�HB�HB�fB�B�mB�;B�B�HB�yB�mB�ZB�`B�sB�B�B�B�B�B�B�B�B��B	+B	%B	%B	B	B	B	VB	�B	%�B	/B	33B	49B	33B	0!B	.B	,B	.B	.B	33B	33B	7LB	6FB	5?B	6FB	7LB	7LB	:^B	=qB	?}B	F�B	J�B	T�B	XB	YB	YB	YB	_;B	hsB	jB	k�B	jB	k�B	l�B	gmB	e`B	gmB	k�B	n�B	o�B	r�B	u�B	w�B	z�B	|�B	� B	�B	�B	�+B	�+B	�1B	�=B	�JB	�JB	�PB	�VB	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�!B	�!B	�'B	�3B	�LB	�^B	�XB	�XB	�RB	�dB	�wB	�wB	�}B	��B	ÖB	B	B	ÖB	ÖB	B	B	B	��B	ÖB	ÖB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�5B	�;B	�BB	�HB	�HB	�HB	�NB	�`B	�fB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
1B
	7B

=B

=B
DB
DB
JB
JB
VB
VB
\B
\B
bB
bB
hB
hB
oB
oB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
$�B
$�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
$�B
#�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
)�B
+B
+B
+B
+B
)�B
)�B
)�B
+B
+B
,B
,B
,B
+B
,B
.B
.B
/B
0!B
33B
49B
49B
5?B
6FB
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
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
;dB
;dB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
<jB
=qB
=qB
>wB
?}B
@�B
A�B
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
B�B
B�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
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
J�B
J�B
K�B
K�B
K�B
K�B
K�B
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
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
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
XB
XB
XB
YB
YB
YB
YB
ZB
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
\)B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
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
bNB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
dZB
e`B
ffB
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
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
hsB
hsB
hsB
iyB
jB
jB
jB
k�B
k�B
k�B
jB
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
w�B
w�B
w�B
x�B
x�B
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
{�B
{�B
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
�B
�B
�B
�B
�B
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
�B
�B
�B
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
�1B
�1B
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
�DB
�D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�FB
�LB
�FB
�?B
�FB
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�FB
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�9B
�3B
�-B
�'B
��B
�DB
aHB
k�B
��B
��B  B
��B
��B  B;dBW
B�B�B�B��BB+B'�BH�BK�B~�B|�B�B�oB�B�'B�B�B�B��BN�BhB+BB  B��B�B�BB��B�XB��B�B�9BĜB��BǮB�XB�dB�^B�B�B��B�oB�+Bu�BaHBR�BM�BE�BB�B=qB6FB+B�BPB
�B
��B
��B
ĜB
�RB
�B
��B
�JB
y�B
n�B
Q�B
6FB
�B	��B	�)B	ȴB	��B	��B	}�B	v�B	o�B	e`B	VB	O�B	L�B	L�B	N�B	L�B	J�B	H�B	H�B	E�B	B�B	B�B	F�B	H�B	G�B	F�B	;dB	:^B	C�B	H�B	I�B	J�B	K�B	O�B	XB	ffB	ffB	e`B	cTB	XB	Q�B	P�B	R�B	N�B	D�B	@�B	9XB	49B	)�B	�B	�B	+B	B	B��B	1B	 �B	!�B	�B	8RB	33B	2-B	�B	PB	%B��B��B��B	  B��B�B�B�yB�yB�fB�HB�#B�B��B��B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�/B�HB�B��B�B�B�B�B�B�B�B��B�B�5B�5B��B�B�mB�sB�B�fB�TB�#B�B��B�
B��B��B��B��B��BɺBŢBB��BŢB��B��B�B�B�B�B�B�
B�5B�NB�HB�HB�fB�B�mB�;B�B�HB�yB�mB�ZB�`B�sB�B�B�B�B�B�B�B�B��B	+B	%B	%B	B	B	B	VB	�B	%�B	/B	33B	49B	33B	0!B	.B	,B	.B	.B	33B	33B	7LB	6FB	5?B	6FB	7LB	7LB	:^B	=qB	?}B	F�B	J�B	T�B	XB	YB	YB	YB	_;B	hsB	jB	k�B	jB	k�B	l�B	gmB	e`B	gmB	k�B	n�B	o�B	r�B	u�B	w�B	z�B	|�B	� B	�B	�B	�+B	�+B	�1B	�=B	�JB	�JB	�PB	�VB	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�!B	�!B	�'B	�3B	�LB	�^B	�XB	�XB	�RB	�dB	�wB	�wB	�}B	��B	ÖB	B	B	ÖB	ÖB	B	B	B	��B	ÖB	ÖB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�5B	�;B	�BB	�HB	�HB	�HB	�NB	�`B	�fB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
1B
	7B

=B

=B
DB
DB
JB
JB
VB
VB
\B
\B
bB
bB
hB
hB
oB
oB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
$�B
$�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
$�B
#�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
)�B
+B
+B
+B
+B
)�B
)�B
)�B
+B
+B
,B
,B
,B
+B
,B
.B
.B
/B
0!B
33B
49B
49B
5?B
6FB
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
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
;dB
;dB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
<jB
=qB
=qB
>wB
?}B
@�B
A�B
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
B�B
B�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
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
J�B
J�B
K�B
K�B
K�B
K�B
K�B
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
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
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
XB
XB
XB
YB
YB
YB
YB
ZB
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
\)B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
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
bNB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
dZB
e`B
ffB
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
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
hsB
hsB
hsB
iyB
jB
jB
jB
k�B
k�B
k�B
jB
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
w�B
w�B
w�B
x�B
x�B
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
{�B
{�B
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
�B
�B
�B
�B
�B
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
�B
�B
�B
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
�1B
�1B
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
�DB
�D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20210913214033  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20210913124213  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20210913124213  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20210913124213  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20210913124214  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20210913124214  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20210913124214  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20210913124214  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20210913124215  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20210913124215                      G�O�G�O�G�O�                JA  ARUP                                                                        20210913125243                      G�O�G�O�G�O�                