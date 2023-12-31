CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:19:28Z creation;2022-06-04T19:19:29Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191928  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               3A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�%��c^1   @�%�l�d @.V�t��cW��$�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BQ��BW��Ba33BfffBp  Bx  B��B���B�  B�  B�  B�  B�33B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���Bә�B�33B���B�  B�  B�  B�  B�  B�  B�  B���B�ffC�fC  C  C  C
  C  C  C  C  C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,33C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� DyfDy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@  @p  @�33@�33A��A=��A]��A}��A���A���A���A���A���A���A���A���BffBffBffBffB'ffB/ffB7ffB?ffBGffBQ  BW  B`��Be��BoffBwffB  B�� B��3B��3B��3B��3B��fB�L�B�L�B�� B��3B��3B��3B��3B��3B��3B��3Bó3Bǳ3B˳3BЀ B�L�B��fBۀ B߳3B�3B�3B�3B�3B�3B��3B�� B��C� CٚCٚCٚC	ٚCٚCٚCٚCٚC�4CٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC,�C-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_�4CaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt�fDuvfDu�fDvvfDv�fDwvfDw�fDxvfDx��DyvfDy�fDzvfDz�fD{vfD{�fD|vfD|�fD}vfD}�fD~vfD~�fDvfD�fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�8 D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�>fD�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�x D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D»3D��3D�;3D�{3Dû3D��3D�;3D�{3DĻ3D��3D�;3D�{3DŻ3D��3D�;3D�{3Dƻ3D��3D�;3D�{3Dǻ3D��3D�;3D�{3DȻ3D��3D�;3D�{3Dɻ3D��3D�;3D�{3Dʻ3D��3D�;3D�{3D˻3D��3D�;3D�{3D̻3D��3D�;3D�{3Dͻ3D��3D�;3D�{3Dλ3D��3D�;3D�{3Dϻ3D��3D�;3D�{3Dл3D��3D�;3D�{3Dѻ3D��3D�;3D�{3Dһ3D��3D�;3D�{3Dӻ3D��3D�;3D�{3DԻ3D��3D�;3D�{3Dջ3D��3D�;3D�{3Dֻ3D��3D�;3D�{3D׻3D��3D�;3D�{3Dػ3D��3D�;3D�{3Dٻ3D��3D�;3D�{3Dڻ3D��3D�;3D�{3Dۻ3D��3D�;3D�{3Dܻ3D��3D�;3D�{3Dݻ3D��3D�;3D�{3D޻3D��3D�;3D�{3D߻3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�8 D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A��.A��A� A�bA��A��A�VA�"A��A� A��A��A�bA��VA��	A���A�	�A�YA��"A�̘A�ʌA���A��AϯOA�$�A͒oA��A�hA�v�A��.A���A�ĜA���A�J�A�ޞA�w�A��[A�I�A�ȴA��=A��A���A�b�A��A�J�A�T�A���A�<6A���A�ںA��A��YA���A�(A�U�A��-A��1A�F?A���A�kQA�7A��DA��)A��
A�aHA�&�A�A�VA���A��SA�4�A���A��A���A�uZA�gA�}�A���A��A��oA�oA�L�A��A�G�A��}A�0UA�Z�A}	Ay�Av1As��Ap2aAlm]Ah*0Ac�MA^�AZ�AWFtAR�9APc�AO}VAM�3AL6AI}�AE�BAC�AAN<A@ffA>z�A;��A:��A9��A8�hA7 \A5��A4�)A4,�A2f�A1FA0
=A-�A(�A%,=A"�oA"!�A!_pAԕA1�A-AZ�A�>A|�A9�A#˒A%�CA%rGA#�5A!y>A ˒A�Aa|A�A"�A~�AC-A��A��A��A�NA	�Ac�A�_A�ZA�A1'A'RA�A��A��A�>AxlAG�A�9A�5A�9A�<A��A=qA}�A�	A�	A�A�AtTA>BA:�AHA
A��AJ#A	6�A	~�A	��A	y>A	g�A	$tA��A��A!-AAm�A��A�pAߤA�&AMjAL�A��A�CAE9A -�@��$@�S�@���@��"@��@��L@��\@�|@�Q�@���@���@��.@���@���@�J@�u@�Z�@��,@���@�a|@�*@�R�@�X�@� �@�f�@�#�@��@��@�4@�b�@��@��@�@�@��`@�o@�e@���@�f@�0�@��y@��@詓@�Ft@�#�@���@�a�@�(@�@��@䉠@�x@�@オ@�IR@��@�)�@�|�@�X@�Ta@��5@޺�@�e�@��@�J#@ܟ�@�^5@�&�@���@��@�h�@�[W@ش9@��U@�ی@�8�@�u@Ր�@�g8@�H�@���@�%F@��X@��@�[W@��@ԧ@�C-@�|�@ҿ�@�Ta@�&�@ѓ@�5?@�n/@�C@ζ�@�#:@̀4@���@�@�c�@�Q�@�8@��P@ʏ\@�h�@�@ɤ@@�g�@�/@�$@ǩ*@�a@� \@���@ƃ@�H@�'R@��>@ū�@œ@�+�@���@�1�@��@���@Ñh@A@�-@���@���@�-w@���@��]@���@���@��o@� �@��y@�`�@�Ov@�
�@�|@���@���@�;�@��d@�\�@�!�@�Ɇ@�w�@�9X@���@��0@��S@�v`@���@���@�2a@� \@��@���@���@�}�@�)_@��@�ߤ@��@�}V@�C-@���@�W?@�+@���@�M@��o@��"@��y@�,=@��0@��7@�e�@��@���@��@�ی@��'@���@�2�@���@��-@��@��@�8@��`@���@�l�@��@���@�$@��C@�e�@�A�@�0�@�
=@���@�oi@�.�@��@��*@��@�e�@��@���@���@�� @�u�@�[�@�Q�@�
�@��j@��	@�N<@�7L@�@@�͟@��o@�Q@�=q@�!�@��@�v`@�7L@��@�͟@���@�W�@�($@���@��X@�c�@�F@�(@��_@�Xy@��@��:@�`B@�Z�@�	l@��I@���@���@�0�@�֡@�;�@���@���@�S�@��5@�ں@���@���@�&�@��^@�t�@�*0@���@��@��@�(�@��Z@���@���@�m]@�(�@��+@�{@��@���@���@�\�@���@�v�@��@���@��}@�e,@���@��@��@��H@�Dg@��@�Ĝ@�_�@��>@���@�j�@��h@�a|@�	@���@��K@��@�j@�A�@��M@�{�@��q@�a�@�)_@� i@��@��@���@�O@��T@���@�P�@�+@��	@��@���@��@��z@���@�.�@�@���@���@���@�Q�@�'�@���@��9@��@��o@�(�@��@�� @��@��6@��@��}@���@��{@�`B@�X@�S&@�8@��@��j@���@��A@�l�@�2�@�{@��#@��	@�a�@�F@��@��|@��`@��@�V�@�u@��@��&@���@�X@��@���@��<@�c�@�C�@�($@�f@~��@~GE@}�^@}V@|Ĝ@|y>@{��@{o�@{
=@z�,@za|@y��@ya�@y�@x�9@xXy@x/�@w�@w.I@v�6@v�@u�@u��@u��@u�3@u2a@t�E@t��@s�]@so�@s�@rOv@qB�@p��@p��@p�@p2�@o]�@n��@nC�@m�H@mDg@mq@l�@lZ@l�@k��@kg�@j�@j��@i�D@ix�@i5�@h�@h��@hbN@g�W@g��@gy�@g@f�@f{@e��@e�@e��@e-w@d��@dbN@dN�@cخ@c�	@co�@cO@b��@b�A@b:*@b
�@a��@af�@a8�@`��@`�@`S�@_�@_&@^��@^s�@^h
@^-@^�@]�N@]L�@\�.@\oi@\_@\"h@[� @[��@[S�@[�@Z�@Z�X@ZE�@Y�d@Y \@X��@X��@XC-@XG@X�@W�]@W�@W��@WP�@W�@Vں@V��@V($@U�@U��@U��@U|@Uk�@U\�@U@T`�@Sخ@S��@Sj�@S/�@R�]@R�6@R��@RZ�@R	@Q�@Q[W@P�j@PXy@P@O��@OU�@O;d@N��@NB[@N�@M��@M��@M(�@L��@Lh�@L4n@K��@KO@J�"@J�@J��@J��@J$�@I��@I%F@H�@H>B@G��@G��@G4�@F��@F�@FR�@E��@EQ�@D�j@D'R@Cn/@C i@B�!@B�b@B{@A��@Aa�@A5�@A�@@*�@?�@?�@?E9@?"�@?�@?�@>�@>�'@>�L@>!�@=��@=B�@=-w@=�@<�P@<�U@<l"@;�r@;�k@;"�@:��@:��@:&�@9zx@8�|@8�p@8�@8]d@8I�@8-�@7�m@7�0@733@7S@6҉@6�6@6��@63�@5j@4��@4  @3�
@3��@3"�@2�y@2��@2�\@2?@1�d@1�~@1��@1m]@1+�@1�@0�@0�j@0m�@0-�@/�@/��@/v`@/S�@/A�@/6z@/33@.��@.��@.��@.��@.c @.J@-�N@-e,@-IR@-@,�?@,��@,_@+�6@+��@+_p@+X�@+8@+&@*�y@*�s@*�m@*l�@*J@)��@)�3@)rG@)�@(��@(��@(`�@'ݘ@'��@'�@@'x@'Z�@'4�@&��@&�M@&ں@&��@&}V@&L0@&.�@&4@%��@%�'@%�~@%^�@%Dg@%2a@%@%@$��@$��@$~(@$	�@#��@#�:@#|�@#iD@#>�@"�c@"��@"L0@!�@!�^@!�@!\�@ ��@ �O@ oi@ ?�@�W@��@�:@RT@,�@��@�<@��@i�@V@Ov@@��@��@o @�@�[@�e@��@�@PH@b@��@��@��@s@s@dZ@4�@ i@�2@�@��@��@�h@��@q�@GE@@�@�@�M@�@��@��@q@4n@��@��@�@l�@S@�s@��@�\@GE@�@{@u@�T@�M@[W@<6@�@�5@�@�@l"@<�@x@�@��@�@�@Z�@.I@@�@�R@��@kQ@^5@W�@;�@{@@�j@��@zx@S&@7L@	l@�v@ѷ@�U@�D@oi@D�@@�r@خ@�0@��@s@n/@n/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A��.A��A� A�bA��A��A�VA�"A��A� A��A��A�bA��VA��	A���A�	�A�YA��"A�̘A�ʌA���A��AϯOA�$�A͒oA��A�hA�v�A��.A���A�ĜA���A�J�A�ޞA�w�A��[A�I�A�ȴA��=A��A���A�b�A��A�J�A�T�A���A�<6A���A�ںA��A��YA���A�(A�U�A��-A��1A�F?A���A�kQA�7A��DA��)A��
A�aHA�&�A�A�VA���A��SA�4�A���A��A���A�uZA�gA�}�A���A��A��oA�oA�L�A��A�G�A��}A�0UA�Z�A}	Ay�Av1As��Ap2aAlm]Ah*0Ac�MA^�AZ�AWFtAR�9APc�AO}VAM�3AL6AI}�AE�BAC�AAN<A@ffA>z�A;��A:��A9��A8�hA7 \A5��A4�)A4,�A2f�A1FA0
=A-�A(�A%,=A"�oA"!�A!_pAԕA1�A-AZ�A�>A|�A9�A#˒A%�CA%rGA#�5A!y>A ˒A�Aa|A�A"�A~�AC-A��A��A��A�NA	�Ac�A�_A�ZA�A1'A'RA�A��A��A�>AxlAG�A�9A�5A�9A�<A��A=qA}�A�	A�	A�A�AtTA>BA:�AHA
A��AJ#A	6�A	~�A	��A	y>A	g�A	$tA��A��A!-AAm�A��A�pAߤA�&AMjAL�A��A�CAE9A -�@��$@�S�@���@��"@��@��L@��\@�|@�Q�@���@���@��.@���@���@�J@�u@�Z�@��,@���@�a|@�*@�R�@�X�@� �@�f�@�#�@��@��@�4@�b�@��@��@�@�@��`@�o@�e@���@�f@�0�@��y@��@詓@�Ft@�#�@���@�a�@�(@�@��@䉠@�x@�@オ@�IR@��@�)�@�|�@�X@�Ta@��5@޺�@�e�@��@�J#@ܟ�@�^5@�&�@���@��@�h�@�[W@ش9@��U@�ی@�8�@�u@Ր�@�g8@�H�@���@�%F@��X@��@�[W@��@ԧ@�C-@�|�@ҿ�@�Ta@�&�@ѓ@�5?@�n/@�C@ζ�@�#:@̀4@���@�@�c�@�Q�@�8@��P@ʏ\@�h�@�@ɤ@@�g�@�/@�$@ǩ*@�a@� \@���@ƃ@�H@�'R@��>@ū�@œ@�+�@���@�1�@��@���@Ñh@A@�-@���@���@�-w@���@��]@���@���@��o@� �@��y@�`�@�Ov@�
�@�|@���@���@�;�@��d@�\�@�!�@�Ɇ@�w�@�9X@���@��0@��S@�v`@���@���@�2a@� \@��@���@���@�}�@�)_@��@�ߤ@��@�}V@�C-@���@�W?@�+@���@�M@��o@��"@��y@�,=@��0@��7@�e�@��@���@��@�ی@��'@���@�2�@���@��-@��@��@�8@��`@���@�l�@��@���@�$@��C@�e�@�A�@�0�@�
=@���@�oi@�.�@��@��*@��@�e�@��@���@���@�� @�u�@�[�@�Q�@�
�@��j@��	@�N<@�7L@�@@�͟@��o@�Q@�=q@�!�@��@�v`@�7L@��@�͟@���@�W�@�($@���@��X@�c�@�F@�(@��_@�Xy@��@��:@�`B@�Z�@�	l@��I@���@���@�0�@�֡@�;�@���@���@�S�@��5@�ں@���@���@�&�@��^@�t�@�*0@���@��@��@�(�@��Z@���@���@�m]@�(�@��+@�{@��@���@���@�\�@���@�v�@��@���@��}@�e,@���@��@��@��H@�Dg@��@�Ĝ@�_�@��>@���@�j�@��h@�a|@�	@���@��K@��@�j@�A�@��M@�{�@��q@�a�@�)_@� i@��@��@���@�O@��T@���@�P�@�+@��	@��@���@��@��z@���@�.�@�@���@���@���@�Q�@�'�@���@��9@��@��o@�(�@��@�� @��@��6@��@��}@���@��{@�`B@�X@�S&@�8@��@��j@���@��A@�l�@�2�@�{@��#@��	@�a�@�F@��@��|@��`@��@�V�@�u@��@��&@���@�X@��@���@��<@�c�@�C�@�($@�f@~��@~GE@}�^@}V@|Ĝ@|y>@{��@{o�@{
=@z�,@za|@y��@ya�@y�@x�9@xXy@x/�@w�@w.I@v�6@v�@u�@u��@u��@u�3@u2a@t�E@t��@s�]@so�@s�@rOv@qB�@p��@p��@p�@p2�@o]�@n��@nC�@m�H@mDg@mq@l�@lZ@l�@k��@kg�@j�@j��@i�D@ix�@i5�@h�@h��@hbN@g�W@g��@gy�@g@f�@f{@e��@e�@e��@e-w@d��@dbN@dN�@cخ@c�	@co�@cO@b��@b�A@b:*@b
�@a��@af�@a8�@`��@`�@`S�@_�@_&@^��@^s�@^h
@^-@^�@]�N@]L�@\�.@\oi@\_@\"h@[� @[��@[S�@[�@Z�@Z�X@ZE�@Y�d@Y \@X��@X��@XC-@XG@X�@W�]@W�@W��@WP�@W�@Vں@V��@V($@U�@U��@U��@U|@Uk�@U\�@U@T`�@Sخ@S��@Sj�@S/�@R�]@R�6@R��@RZ�@R	@Q�@Q[W@P�j@PXy@P@O��@OU�@O;d@N��@NB[@N�@M��@M��@M(�@L��@Lh�@L4n@K��@KO@J�"@J�@J��@J��@J$�@I��@I%F@H�@H>B@G��@G��@G4�@F��@F�@FR�@E��@EQ�@D�j@D'R@Cn/@C i@B�!@B�b@B{@A��@Aa�@A5�@A�@@*�@?�@?�@?E9@?"�@?�@?�@>�@>�'@>�L@>!�@=��@=B�@=-w@=�@<�P@<�U@<l"@;�r@;�k@;"�@:��@:��@:&�@9zx@8�|@8�p@8�@8]d@8I�@8-�@7�m@7�0@733@7S@6҉@6�6@6��@63�@5j@4��@4  @3�
@3��@3"�@2�y@2��@2�\@2?@1�d@1�~@1��@1m]@1+�@1�@0�@0�j@0m�@0-�@/�@/��@/v`@/S�@/A�@/6z@/33@.��@.��@.��@.��@.c @.J@-�N@-e,@-IR@-@,�?@,��@,_@+�6@+��@+_p@+X�@+8@+&@*�y@*�s@*�m@*l�@*J@)��@)�3@)rG@)�@(��@(��@(`�@'ݘ@'��@'�@@'x@'Z�@'4�@&��@&�M@&ں@&��@&}V@&L0@&.�@&4@%��@%�'@%�~@%^�@%Dg@%2a@%@%@$��@$��@$~(@$	�@#��@#�:@#|�@#iD@#>�@"�c@"��@"L0@!�@!�^@!�@!\�@ ��@ �O@ oi@ ?�@�W@��@�:@RT@,�@��@�<@��@i�@V@Ov@@��@��@o @�@�[@�e@��@�@PH@b@��@��@��@s@s@dZ@4�@ i@�2@�@��@��@�h@��@q�@GE@@�@�@�M@�@��@��@q@4n@��@��@�@l�@S@�s@��@�\@GE@�@{@u@�T@�M@[W@<6@�@�5@�@�@l"@<�@x@�@��@�@�@Z�@.I@@�@�R@��@kQ@^5@W�@;�@{@@�j@��@zx@S&@7L@	l@�v@ѷ@�U@�D@oi@D�@@�r@خ@�0@��@s@n/@n/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	1B	�B	�B	�B	�B	�B	�B	1B	KB	1B	KB	KB	1B	KB	B	B	QB	�B	�B	�B	�B	�B	�B	�B	WB	'8B	*KB	&�B	1[B	o�B	rGB	�:B	�+B	��B	�TB
5�B
5%B
2�B
7�B
EmB
OBB
_�B
c�B
a�B
^B
a�B
��B
�	B
��B
�B�B-CB<jB[�Bo5BhsBVmBZB]/B^�B]�BF�B/B'B�B,�BUgBdtB��B��B��B��B�RBr�Be�B]�BK�B9>B!�B?B
��B
ĶB
�(B
`�B
2aB
B
zB	��B	�B	��B	�B	�aB	�=B	��B	hXB	KxB	/�B	=B	DB�jB�B	B	gB	�B	(B	�B	�B	�B	-B	oB��B�JB��B�BB	�B	�B	
rB	1B	 �B��B�B�nB��B�IB��B�%B�[B�B�*B� B�.B��B�cB�(B	d@B	�B	�B	�qB	��B	��B	��B	�lB	��B	�\B	��B	�_B	�aB	shB	Y�B	I�B	AB	?�B	<jB	J�B	T{B	M�B	KDB	MB	b�B	[�B	a�B	b�B	abB	r�B	�B	�/B	��B	��B	�UB	ȚB	ɆB	͟B	ϫB	�NB	��B	�+B	��B	�)B	�QB	��B	��B	�LB	�B	�B	�uB	�KB	�DB	�XB	ǮB	��B	��B	�jB	�RB	ɠB	��B	�B	�]B	�B	��B	��B	�B	��B	��B	�B	ǔB	��B	�MB	��B	�B	یB	چB	�#B	ڠB	�xB	�#B	��B	�eB	ٴB	�QB	��B	�	B	�=B	��B	��B	�=B	��B	�B	��B	ٚB	ٚB	�B	�7B	�B	�B	�B	��B	�B	��B	�dB	ܬB	�B	�=B	�#B	��B	�kB	�B	�eB	یB	ڠB	�7B	�QB	ںB	�	B	��B	چB	�B	ٚB	��B	�+B	�sB	ڠB	چB	یB	�xB	ܒB	ܬB	�CB	�CB	�dB	�5B	ߤB	��B	�pB	޸B	��B	ݘB	�_B	��B	ԕB	�2B	ݘB	��B	�B	�B	��B	��B	�[B	�B	�;B	��B	�GB	��B	�;B	�B	�B	�qB	�B	�eB	�0B	�DB	�B	�B	�_B	�B	�B	��B	��B	�B	�B	�B	�B	��B	��B	�B	�B	�B	��B	�CB	�]B	�wB	�]B	��B	��B	��B	��B	�OB	�}B	�}B	�/B	�OB	�B	�B	�GB	�GB	�GB	�aB	�GB	��B	��B	�B	�B	�5B	�IB	��B	�}B	��B	�wB	�B	�B	� B	�B	�B	�!B	�B	��B	�AB	�AB	�AB	�B	�B	��B	�B	�MB	�MB	�hB	��B	�B	�TB	�nB	�nB	��B	��B	��B	�LB	�fB	�fB	��B	�B	�	B	��B	�xB	�PB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�"B	��B	��B	��B	��B	��B	�}B
 �B
�B
B
�B
	7B
	�B
	�B
	�B
	�B
	�B

=B

	B

	B

#B

#B

#B

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
DB
DB
B
0B
�B
�B
�B
�B
�B
�B
\B
�B
�B
.B
bB
�B
�B
bB
.B
�B
�B
 B
B
�B
}B
HB
4B
HB
\B
�B
�B
6B
6B
"B
<B
�B
�B
�B
4B
�B
bB
�B
�B
�B
 B
B
�B
�B
�B
B
:B
oB
�B
uB
�B
�B
�B
�B
gB
gB
�B
gB
B
SB
mB
�B
�B
�B
�B
+B
�B
�B
�B
1B
�B
B
B
	B
	B
�B
�B
)B
�B
�B
]B
CB
�B
B
OB
�B
�B
�B
�B
�B
�B
 vB
 �B
!HB
!-B
!HB
!�B
"4B
"4B
"B
"4B
"4B
"hB
"�B
"�B
"�B
#nB
#�B
$&B
$�B
$tB
$tB
%FB
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&2B
&fB
&fB
&�B
&�B
&�B
(
B
(�B
(�B
(�B
)�B
)�B
)�B
*B
*�B
+B
+kB
+�B
+�B
,�B
,�B
-�B
.B
./B
.�B
.�B
.�B
/B
/�B
0!B
0B
0B
0�B
0�B
1AB
1�B
2aB
2aB
2-B
2�B
3�B
3�B
3�B
4B
4�B
4�B
5ZB
6B
6`B
6�B
6�B
7LB
6�B
6�B
7fB
8�B
9�B
9�B
:*B
:DB
9�B
:B
:*B
:^B
:�B
;B
;B
;B
;0B
;�B
<B
<6B
<jB
<�B
=B
="B
=VB
=�B
=�B
=�B
>(B
>]B
>]B
>�B
>�B
>�B
?B
?.B
?.B
?cB
?}B
?�B
?�B
?�B
@iB
@�B
@�B
@�B
AoB
A�B
BB
B'B
BAB
BuB
BuB
B�B
B�B
B�B
B�B
B�B
CGB
C�B
C{B
C�B
D�B
D�B
EB
E�B
FYB
FtB
F�B
F�B
F�B
F�B
G_B
G�B
G�B
G�B
H1B
H�B
H�B
H�B
H�B
IB
H�B
IRB
IlB
J	B
J=B
J=B
J�B
J�B
J�B
J�B
J�B
K^B
KxB
K�B
K�B
K�B
L0B
LJB
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
NB
NB
NVB
NVB
N�B
N�B
N�B
N�B
O(B
OvB
O�B
O�B
PHB
PHB
P.B
PbB
P�B
P�B
P�B
P�B
Q4B
Q�B
Q�B
Q�B
RB
R�B
R�B
R�B
R�B
R�B
R�B
SB
SuB
S�B
TB
TFB
T�B
T�B
UB
UgB
UgB
U�B
VB
VSB
V�B
W
B
W�B
W�B
W�B
W�B
XyB
X�B
XyB
X�B
Y1B
YKB
YKB
Y�B
Y�B
Y�B
Y�B
ZB
Y�B
Y�B
ZkB
Z�B
[	B
[	B
[#B
[#B
[=B
[�B
[�B
[�B
\B
\)B
\)B
\]B
\�B
]B
]/B
]/B
]~B
]dB
]dB
]�B
]�B
]�B
]�B
^B
^5B
^B
^OB
^�B
_!B
_�B
`B
`BB
`vB
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
aHB
aHB
abB
a|B
a�B
bNB
bNB
b�B
cB
c B
c B
c:B
c B
cTB
cTB
c:B
c:B
cTB
cnB
c�B
dB
c�B
d&B
dZB
dZB
dtB
e,B
eFB
e�B
ezB
e�B
e�B
e�B
e�B
e�B
e�B
fLB
f2B
fLB
ffB
f�B
f�B
gB
gRB
g�B
g�B
g�B
h$B
h$B
hXB
hsB
h�B
h�B
h�B
h�B
iB
i*B
i*B
i�B
i�B
i�B
i�B
i�B
i�B
jB
jB
i�B
j0B
j0B
j�B
kB
kQB
kkB
kkB
k�B
k�B
lB
lWB
l�B
lWB
lqB
l�B
mB
m�B
nB
n/B
ncB
ncB
n�B
n�B
n�B
n�B
oB
oOB
o�B
o�B
o�B
o�B
pB
pB
p!B
p!B
p;B
pUB
p;B
poB
p�B
p�B
p�B
p�B
qB
qAB
q'B
q[B
q[B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
raB
r�B
sB
sMB
sMB
s�B
s�B
tB
tB
tTB
t�B
t�B
uB
uB
u�B
u�B
u�B
u�B
u�B
v+B
vFB
v`B
v�B
vzB
v�B
v�B
v�B
wB
wfB
w�B
w�B
w�B
w�B
w�B
w�B
xB
x8B
xlB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
y>B
yXB
yrB
y�B
y�B
y�B
y�B
zDB
zDB
zxB
z�B
z�B
z�B
z�B
{B
{JB
{JB
{J111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	1B	�B	�B	�B	�B	�B	�B	1B	KB	1B	KB	KB	1B	KB	B	B	QB	�B	�B	�B	�B	�B	�B	�B	WB	'8B	*KB	&�B	1[B	o�B	rGB	�:B	�+B	��B	�TB
5�B
5%B
2�B
7�B
EmB
OBB
_�B
c�B
a�B
^B
a�B
��B
�	B
��B
�B�B-CB<jB[�Bo5BhsBVmBZB]/B^�B]�BF�B/B'B�B,�BUgBdtB��B��B��B��B�RBr�Be�B]�BK�B9>B!�B?B
��B
ĶB
�(B
`�B
2aB
B
zB	��B	�B	��B	�B	�aB	�=B	��B	hXB	KxB	/�B	=B	DB�jB�B	B	gB	�B	(B	�B	�B	�B	-B	oB��B�JB��B�BB	�B	�B	
rB	1B	 �B��B�B�nB��B�IB��B�%B�[B�B�*B� B�.B��B�cB�(B	d@B	�B	�B	�qB	��B	��B	��B	�lB	��B	�\B	��B	�_B	�aB	shB	Y�B	I�B	AB	?�B	<jB	J�B	T{B	M�B	KDB	MB	b�B	[�B	a�B	b�B	abB	r�B	�B	�/B	��B	��B	�UB	ȚB	ɆB	͟B	ϫB	�NB	��B	�+B	��B	�)B	�QB	��B	��B	�LB	�B	�B	�uB	�KB	�DB	�XB	ǮB	��B	��B	�jB	�RB	ɠB	��B	�B	�]B	�B	��B	��B	�B	��B	��B	�B	ǔB	��B	�MB	��B	�B	یB	چB	�#B	ڠB	�xB	�#B	��B	�eB	ٴB	�QB	��B	�	B	�=B	��B	��B	�=B	��B	�B	��B	ٚB	ٚB	�B	�7B	�B	�B	�B	��B	�B	��B	�dB	ܬB	�B	�=B	�#B	��B	�kB	�B	�eB	یB	ڠB	�7B	�QB	ںB	�	B	��B	چB	�B	ٚB	��B	�+B	�sB	ڠB	چB	یB	�xB	ܒB	ܬB	�CB	�CB	�dB	�5B	ߤB	��B	�pB	޸B	��B	ݘB	�_B	��B	ԕB	�2B	ݘB	��B	�B	�B	��B	��B	�[B	�B	�;B	��B	�GB	��B	�;B	�B	�B	�qB	�B	�eB	�0B	�DB	�B	�B	�_B	�B	�B	��B	��B	�B	�B	�B	�B	��B	��B	�B	�B	�B	��B	�CB	�]B	�wB	�]B	��B	��B	��B	��B	�OB	�}B	�}B	�/B	�OB	�B	�B	�GB	�GB	�GB	�aB	�GB	��B	��B	�B	�B	�5B	�IB	��B	�}B	��B	�wB	�B	�B	� B	�B	�B	�!B	�B	��B	�AB	�AB	�AB	�B	�B	��B	�B	�MB	�MB	�hB	��B	�B	�TB	�nB	�nB	��B	��B	��B	�LB	�fB	�fB	��B	�B	�	B	��B	�xB	�PB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�"B	��B	��B	��B	��B	��B	�}B
 �B
�B
B
�B
	7B
	�B
	�B
	�B
	�B
	�B

=B

	B

	B

#B

#B

#B

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
DB
DB
B
0B
�B
�B
�B
�B
�B
�B
\B
�B
�B
.B
bB
�B
�B
bB
.B
�B
�B
 B
B
�B
}B
HB
4B
HB
\B
�B
�B
6B
6B
"B
<B
�B
�B
�B
4B
�B
bB
�B
�B
�B
 B
B
�B
�B
�B
B
:B
oB
�B
uB
�B
�B
�B
�B
gB
gB
�B
gB
B
SB
mB
�B
�B
�B
�B
+B
�B
�B
�B
1B
�B
B
B
	B
	B
�B
�B
)B
�B
�B
]B
CB
�B
B
OB
�B
�B
�B
�B
�B
�B
 vB
 �B
!HB
!-B
!HB
!�B
"4B
"4B
"B
"4B
"4B
"hB
"�B
"�B
"�B
#nB
#�B
$&B
$�B
$tB
$tB
%FB
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&2B
&fB
&fB
&�B
&�B
&�B
(
B
(�B
(�B
(�B
)�B
)�B
)�B
*B
*�B
+B
+kB
+�B
+�B
,�B
,�B
-�B
.B
./B
.�B
.�B
.�B
/B
/�B
0!B
0B
0B
0�B
0�B
1AB
1�B
2aB
2aB
2-B
2�B
3�B
3�B
3�B
4B
4�B
4�B
5ZB
6B
6`B
6�B
6�B
7LB
6�B
6�B
7fB
8�B
9�B
9�B
:*B
:DB
9�B
:B
:*B
:^B
:�B
;B
;B
;B
;0B
;�B
<B
<6B
<jB
<�B
=B
="B
=VB
=�B
=�B
=�B
>(B
>]B
>]B
>�B
>�B
>�B
?B
?.B
?.B
?cB
?}B
?�B
?�B
?�B
@iB
@�B
@�B
@�B
AoB
A�B
BB
B'B
BAB
BuB
BuB
B�B
B�B
B�B
B�B
B�B
CGB
C�B
C{B
C�B
D�B
D�B
EB
E�B
FYB
FtB
F�B
F�B
F�B
F�B
G_B
G�B
G�B
G�B
H1B
H�B
H�B
H�B
H�B
IB
H�B
IRB
IlB
J	B
J=B
J=B
J�B
J�B
J�B
J�B
J�B
K^B
KxB
K�B
K�B
K�B
L0B
LJB
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
NB
NB
NVB
NVB
N�B
N�B
N�B
N�B
O(B
OvB
O�B
O�B
PHB
PHB
P.B
PbB
P�B
P�B
P�B
P�B
Q4B
Q�B
Q�B
Q�B
RB
R�B
R�B
R�B
R�B
R�B
R�B
SB
SuB
S�B
TB
TFB
T�B
T�B
UB
UgB
UgB
U�B
VB
VSB
V�B
W
B
W�B
W�B
W�B
W�B
XyB
X�B
XyB
X�B
Y1B
YKB
YKB
Y�B
Y�B
Y�B
Y�B
ZB
Y�B
Y�B
ZkB
Z�B
[	B
[	B
[#B
[#B
[=B
[�B
[�B
[�B
\B
\)B
\)B
\]B
\�B
]B
]/B
]/B
]~B
]dB
]dB
]�B
]�B
]�B
]�B
^B
^5B
^B
^OB
^�B
_!B
_�B
`B
`BB
`vB
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
aHB
aHB
abB
a|B
a�B
bNB
bNB
b�B
cB
c B
c B
c:B
c B
cTB
cTB
c:B
c:B
cTB
cnB
c�B
dB
c�B
d&B
dZB
dZB
dtB
e,B
eFB
e�B
ezB
e�B
e�B
e�B
e�B
e�B
e�B
fLB
f2B
fLB
ffB
f�B
f�B
gB
gRB
g�B
g�B
g�B
h$B
h$B
hXB
hsB
h�B
h�B
h�B
h�B
iB
i*B
i*B
i�B
i�B
i�B
i�B
i�B
i�B
jB
jB
i�B
j0B
j0B
j�B
kB
kQB
kkB
kkB
k�B
k�B
lB
lWB
l�B
lWB
lqB
l�B
mB
m�B
nB
n/B
ncB
ncB
n�B
n�B
n�B
n�B
oB
oOB
o�B
o�B
o�B
o�B
pB
pB
p!B
p!B
p;B
pUB
p;B
poB
p�B
p�B
p�B
p�B
qB
qAB
q'B
q[B
q[B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
raB
r�B
sB
sMB
sMB
s�B
s�B
tB
tB
tTB
t�B
t�B
uB
uB
u�B
u�B
u�B
u�B
u�B
v+B
vFB
v`B
v�B
vzB
v�B
v�B
v�B
wB
wfB
w�B
w�B
w�B
w�B
w�B
w�B
xB
x8B
xlB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
y>B
yXB
yrB
y�B
y�B
y�B
y�B
zDB
zDB
zxB
z�B
z�B
z�B
z�B
{B
{JB
{JB
{J111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105237  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191928  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191929  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191929                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041936  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041936  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                