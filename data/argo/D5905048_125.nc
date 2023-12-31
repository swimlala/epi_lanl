CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-06-03T00:35:25Z creation;2017-06-03T00:35:28Z conversion to V3.1;2019-12-19T08:05:54Z update;     
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
resolution        =���   axis      Z        p  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \X   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  `4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ۴   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  20170603003525  20200116211517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               }A   JA  I2_0577_125                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�6(�1   @�6�$�@2ֵ'��d]�z�H1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D9��D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DN��DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�3D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�6f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�33@�33A��A=��A]��A}��A���A���A���A���A���A���A���A���BffBffBffBffB'ffB/ffB7ffB?ffBGffBOffBWffB_ffBgffBoffBwffBffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B�3B�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCO�3CQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9� D:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN� DOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt�fDuvfDu�fDvvfDv�fDwvfDw�fDxvfDx�fDyvfDy�fDzvfDz�fD{vfD{�fD|vfD|�fD}vfD}�fD~vfD~�fDvfD�fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�>fD�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D»3D��3D�;3D�{3Dû3D��3D�;3D�{3DĻ3D��3D�;3D�{3DŻ3D��fD�;3D�{3Dƻ3D��3D�;3D�{3Dǻ3D��3D�;3D�{3DȻ3D��3D�;3D�{3Dɻ3D��3D�;3D�{3Dʻ3D��3D�;3D�{3D˻3D��3D�;3D�{3D̻3D��3D�;3D�{3Dͻ3D��3D�;3D�{3Dλ3D��3D�;3D�{3Dϻ3D��3D�;3D�{3Dл3D��3D�;3D�{3Dѻ3D��3D�;3D�{3Dһ3D��3D�;3D�{3Dӻ3D��3D�;3D�{3DԻ3D��3D�;3D�{3Dջ3D��3D�;3D�{3Dֻ3D��3D�;3D�{3D׻3D��3D�;3D�{3Dػ3D��3D�;3D�{3Dٻ3D��3D�;3D�{3Dڻ3D��3D�;3D�{3Dۻ3D��3D�;3D�{3Dܻ3D��3D�;3D�{3Dݻ3D��3D�;3D�{3D޻3D��3D�;3D�{3D߻3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�~fD��3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��fD�1�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���AּjAֶFAָRAִ9AֶFAָRAָRAּjA��`A��A���A֬A֟�A֟�A�^5A�%A��A��TAթ�A�A�Aԩ�A�x�A�G�A�  Aҩ�A��A�Q�A��A�ĜAН�A�7LA�=qA��A�ĜA�l�A�1A̲-A�G�A���A˧�A�O�A�oA��TAʛ�A�K�A�1'A��`AɁA�^5A�C�A��A��TA�9XA�7LAƲ-A��A�;dA�(�A���A�|�A�v�A�ƨA��9A�1A�ƨA���A�A�l�A��A��PA�oA���A��;A�&�A��wA�^5A��+A�;dA��7A��DA���A�ƨA�ffA���A���A���A��A�%A�9XA��7A�\)A��FA�`BA��`A�`BA���A��A��A�;dA�5?A� �A��#A��+A��+A�x�A�K�A� �A�bA��mA��yA���A�r�A���A��PA�ƨA��uA���A�"�A�z�A�l�A��`A�$�A�A�A���A�`BA~z�Ay��AvA�As��Ap9XAm��AmAlM�Aj�Ah1Ae��Aa�hA];dA\(�AZ��AY��AW�AS��AR�jAQ��AM�;AK��AJ{AG��AF9XAE\)AD�AD �AC�#AB�jAA�
AA�hA?�TA<�!A:��A9G�A8�HA7G�A6^5A5�A3��A2A�A1�A.�A-��A,��A*�A(�yA( �A%��A#O�A#�A#G�A#hsA#;dA!�A ��A��A�!A��AXA�A�^A�DAn�AA�A�AdZA�AVA�wA;dA
=A"�A`BA�A��A�AA�HA��A�^A&�A%AM�A�TA�RA?}A��A��A��A��A;dA9XA?}A	��An�A�RA�FAK�A��A�;A�;A;dA $�@���@��P@��@�bN@�dZ@���@��-@�bN@�S�@�+@�G�@�9X@�K�@�R@�5?@�p�@��/@�@��m@�  @�ff@�?}@���@� �@���@��@�7L@۝�@�~�@��#@�&�@�1@ՙ�@�E�@���@Η�@́@�&�@���@ʰ!@�{@��@�n�@�ȴ@�p�@���@�z�@�  @��@�  @ǝ�@�V@��@�o@�$�@��T@���@��@��@�O�@��F@��!@�E�@�E�@�X@�V@�%@���@�  @��F@�S�@��H@�{@�7L@�(�@��@�C�@���@��T@�O�@���@�S�@��@�G�@�`B@�&�@�r�@��F@�S�@�"�@�33@��@��y@���@�v�@�ff@��#@�`B@��h@��u@�r�@�j@�(�@���@�
=@�ȴ@�^5@��@���@�hs@�G�@��@���@��9@� �@��m@��
@�;d@��@���@���@�~�@�V@�$�@�{@�@�p�@�/@���@���@��D@��@�z�@�j@�I�@� �@�  @���@�  @��;@�C�@�
=@��@��R@�J@���@��@�&�@��/@��u@�j@�I�@�  @���@���@�33@�@��y@���@���@�ff@�$�@���@��#@���@��-@�`B@�&�@�Ĝ@���@�z�@�r�@�(�@��
@���@�ƨ@��F@���@�dZ@�"�@�ȴ@��\@�ff@�V@�E�@�5?@�{@��@���@�x�@�O�@�%@��`@���@��j@��@�Z@�I�@�9X@�A�@�9X@��m@�ƨ@���@�t�@�\)@�;d@�"�@���@�~�@��@���@���@��@�G�@��@�Ĝ@��u@�bN@�9X@��m@��
@��w@�t�@�S�@���@��@��!@�n�@�-@���@���@��h@�X@���@��@��@�I�@���@��m@��F@�K�@�C�@�C�@�33@�+@�
=@��@��R@���@���@�~�@�^5@�-@�@��#@���@��7@��@�x�@�p�@�hs@�X@��@��`@�Ĝ@���@��@�Z@� �@�1@��w@�\)@�"�@�@��H@�v�@�$�@��T@��-@�`B@�V@��D@�Q�@�9X@��m@��@�^5@�M�@�=q@��@��#@���@�@�@��^@��-@�p�@�%@��/@�Ĝ@���@�Z@�(�@� �@�1@|�@~�y@~�+@~V@~5?@}�@}`B@}?}@}V@|��@|�@|�@|z�@{t�@z��@yhs@x�@x�u@x�9@xQ�@w�@w��@w|�@v�y@v��@v�+@vE�@v$�@u�T@u�@uO�@uV@t�@tz�@t�@s��@sC�@rn�@q�@qX@pĜ@pbN@o��@ol�@o+@n��@n5?@m�@m�h@l��@l�j@l�@kƨ@k��@k@i��@h��@h�@hbN@hQ�@h1'@g�@gK�@g;d@g+@g
=@f��@fff@f$�@f$�@e�T@e�-@e��@e�@e�@dz�@c�
@b��@a��@`��@`Q�@_��@_;d@^�@^��@^�+@^5?@^@]@]�@]O�@\�/@\�@\z�@\z�@\j@\Z@[�
@[��@["�@Z��@Z~�@Z=q@Y��@Y��@X�`@X�@W�@W�@W\)@W;d@W;d@W+@W�@V��@V�y@V�y@V��@V{@U�@U?}@T��@TI�@T9X@T�@St�@R�!@R^5@Q��@Q��@Q�#@Q7L@PĜ@Pr�@O��@Ol�@O\)@O+@N��@N�@N�R@N��@N�+@Nv�@N{@M�@M/@L�/@LZ@L1@K��@K��@K��@K��@K�m@K�
@K�
@K�F@K�@K"�@J��@J=q@I�^@IG�@H��@HbN@HA�@Hb@G�P@F��@F��@F��@F�+@FE�@F$�@E�h@EO�@E/@E�@E�@D�j@D9X@Ct�@B�H@B�\@Bn�@B^5@BM�@B-@A��@A�#@A��@A��@A�#@AX@@��@@Ĝ@@�9@@��@@��@@��@@��@@��@@�@@Q�@?�@?�w@?\)@?+@?�@>�y@>ȴ@>E�@=p�@=?}@=?}@=O�@=O�@=?}@=?}@=?}@=/@=V@<�@<z�@<I�@<I�@<(�@;��@;"�@;@;@:�@:=q@9�^@9hs@8�u@81'@7�@7�w@7��@7\)@6�y@6v�@6E�@6$�@6@5@5�h@5`B@5?}@5V@4��@4��@4Z@41@3�@333@2�H@2�\@2M�@1��@1%@0��@0r�@0  @/�w@/l�@/+@.�R@-�@-�h@-�h@-�@-p�@-V@,�@,�D@,j@,I�@,�@+�
@+�@+33@*�@)�#@)�7@)hs@)&�@)�@(��@(�`@(��@(��@(  @'\)@';d@&��@&�+@&v�@&V@&E�@&5?@&{@&@%�T@%��@%�@%V@$j@$I�@#�m@#�F@#��@#��@#t�@#o@"M�@"-@"J@!��@!x�@!G�@!&�@!�@!%@!%@ ��@ ��@ ��@ ��@ �`@ Ĝ@ bN@�@|�@;d@�@�y@��@��@V@��@��@�h@p�@O�@?}@�/@�j@z�@Z@Z@Z@I�@I�@(�@1@�
@�F@S�@"�@�@��@��@M�@J@�^@X@%@�`@�9@�@�@Q�@1'@�@��@\)@;d@�@�y@��@�+@V@�T@�-@?}@�@��@z�@�D@�D@j@�
@t�@dZ@C�@"�@@�@��@�!@��@�\@~�@^5@=q@��@�^@�7@x�@x�@X@G�@7L@7L@7L@�@%@Ĝ@�u@�@bN@bN@1'@��@�@l�@+@�@�@�y@��@��@�+@ff@V@E�@5?@5?@$�@{@{@{@@�@@�h1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���AּjAֶFAָRAִ9AֶFAָRAָRAּjA��`A��A���A֬A֟�A֟�A�^5A�%A��A��TAթ�A�A�Aԩ�A�x�A�G�A�  Aҩ�A��A�Q�A��A�ĜAН�A�7LA�=qA��A�ĜA�l�A�1A̲-A�G�A���A˧�A�O�A�oA��TAʛ�A�K�A�1'A��`AɁA�^5A�C�A��A��TA�9XA�7LAƲ-A��A�;dA�(�A���A�|�A�v�A�ƨA��9A�1A�ƨA���A�A�l�A��A��PA�oA���A��;A�&�A��wA�^5A��+A�;dA��7A��DA���A�ƨA�ffA���A���A���A��A�%A�9XA��7A�\)A��FA�`BA��`A�`BA���A��A��A�;dA�5?A� �A��#A��+A��+A�x�A�K�A� �A�bA��mA��yA���A�r�A���A��PA�ƨA��uA���A�"�A�z�A�l�A��`A�$�A�A�A���A�`BA~z�Ay��AvA�As��Ap9XAm��AmAlM�Aj�Ah1Ae��Aa�hA];dA\(�AZ��AY��AW�AS��AR�jAQ��AM�;AK��AJ{AG��AF9XAE\)AD�AD �AC�#AB�jAA�
AA�hA?�TA<�!A:��A9G�A8�HA7G�A6^5A5�A3��A2A�A1�A.�A-��A,��A*�A(�yA( �A%��A#O�A#�A#G�A#hsA#;dA!�A ��A��A�!A��AXA�A�^A�DAn�AA�A�AdZA�AVA�wA;dA
=A"�A`BA�A��A�AA�HA��A�^A&�A%AM�A�TA�RA?}A��A��A��A��A;dA9XA?}A	��An�A�RA�FAK�A��A�;A�;A;dA $�@���@��P@��@�bN@�dZ@���@��-@�bN@�S�@�+@�G�@�9X@�K�@�R@�5?@�p�@��/@�@��m@�  @�ff@�?}@���@� �@���@��@�7L@۝�@�~�@��#@�&�@�1@ՙ�@�E�@���@Η�@́@�&�@���@ʰ!@�{@��@�n�@�ȴ@�p�@���@�z�@�  @��@�  @ǝ�@�V@��@�o@�$�@��T@���@��@��@�O�@��F@��!@�E�@�E�@�X@�V@�%@���@�  @��F@�S�@��H@�{@�7L@�(�@��@�C�@���@��T@�O�@���@�S�@��@�G�@�`B@�&�@�r�@��F@�S�@�"�@�33@��@��y@���@�v�@�ff@��#@�`B@��h@��u@�r�@�j@�(�@���@�
=@�ȴ@�^5@��@���@�hs@�G�@��@���@��9@� �@��m@��
@�;d@��@���@���@�~�@�V@�$�@�{@�@�p�@�/@���@���@��D@��@�z�@�j@�I�@� �@�  @���@�  @��;@�C�@�
=@��@��R@�J@���@��@�&�@��/@��u@�j@�I�@�  @���@���@�33@�@��y@���@���@�ff@�$�@���@��#@���@��-@�`B@�&�@�Ĝ@���@�z�@�r�@�(�@��
@���@�ƨ@��F@���@�dZ@�"�@�ȴ@��\@�ff@�V@�E�@�5?@�{@��@���@�x�@�O�@�%@��`@���@��j@��@�Z@�I�@�9X@�A�@�9X@��m@�ƨ@���@�t�@�\)@�;d@�"�@���@�~�@��@���@���@��@�G�@��@�Ĝ@��u@�bN@�9X@��m@��
@��w@�t�@�S�@���@��@��!@�n�@�-@���@���@��h@�X@���@��@��@�I�@���@��m@��F@�K�@�C�@�C�@�33@�+@�
=@��@��R@���@���@�~�@�^5@�-@�@��#@���@��7@��@�x�@�p�@�hs@�X@��@��`@�Ĝ@���@��@�Z@� �@�1@��w@�\)@�"�@�@��H@�v�@�$�@��T@��-@�`B@�V@��D@�Q�@�9X@��m@��@�^5@�M�@�=q@��@��#@���@�@�@��^@��-@�p�@�%@��/@�Ĝ@���@�Z@�(�@� �@�1@|�@~�y@~�+@~V@~5?@}�@}`B@}?}@}V@|��@|�@|�@|z�@{t�@z��@yhs@x�@x�u@x�9@xQ�@w�@w��@w|�@v�y@v��@v�+@vE�@v$�@u�T@u�@uO�@uV@t�@tz�@t�@s��@sC�@rn�@q�@qX@pĜ@pbN@o��@ol�@o+@n��@n5?@m�@m�h@l��@l�j@l�@kƨ@k��@k@i��@h��@h�@hbN@hQ�@h1'@g�@gK�@g;d@g+@g
=@f��@fff@f$�@f$�@e�T@e�-@e��@e�@e�@dz�@c�
@b��@a��@`��@`Q�@_��@_;d@^�@^��@^�+@^5?@^@]@]�@]O�@\�/@\�@\z�@\z�@\j@\Z@[�
@[��@["�@Z��@Z~�@Z=q@Y��@Y��@X�`@X�@W�@W�@W\)@W;d@W;d@W+@W�@V��@V�y@V�y@V��@V{@U�@U?}@T��@TI�@T9X@T�@St�@R�!@R^5@Q��@Q��@Q�#@Q7L@PĜ@Pr�@O��@Ol�@O\)@O+@N��@N�@N�R@N��@N�+@Nv�@N{@M�@M/@L�/@LZ@L1@K��@K��@K��@K��@K�m@K�
@K�
@K�F@K�@K"�@J��@J=q@I�^@IG�@H��@HbN@HA�@Hb@G�P@F��@F��@F��@F�+@FE�@F$�@E�h@EO�@E/@E�@E�@D�j@D9X@Ct�@B�H@B�\@Bn�@B^5@BM�@B-@A��@A�#@A��@A��@A�#@AX@@��@@Ĝ@@�9@@��@@��@@��@@��@@��@@�@@Q�@?�@?�w@?\)@?+@?�@>�y@>ȴ@>E�@=p�@=?}@=?}@=O�@=O�@=?}@=?}@=?}@=/@=V@<�@<z�@<I�@<I�@<(�@;��@;"�@;@;@:�@:=q@9�^@9hs@8�u@81'@7�@7�w@7��@7\)@6�y@6v�@6E�@6$�@6@5@5�h@5`B@5?}@5V@4��@4��@4Z@41@3�@333@2�H@2�\@2M�@1��@1%@0��@0r�@0  @/�w@/l�@/+@.�R@-�@-�h@-�h@-�@-p�@-V@,�@,�D@,j@,I�@,�@+�
@+�@+33@*�@)�#@)�7@)hs@)&�@)�@(��@(�`@(��@(��@(  @'\)@';d@&��@&�+@&v�@&V@&E�@&5?@&{@&@%�T@%��@%�@%V@$j@$I�@#�m@#�F@#��@#��@#t�@#o@"M�@"-@"J@!��@!x�@!G�@!&�@!�@!%@!%@ ��@ ��@ ��@ ��@ �`@ Ĝ@ bN@�@|�@;d@�@�y@��@��@V@��@��@�h@p�@O�@?}@�/@�j@z�@Z@Z@Z@I�@I�@(�@1@�
@�F@S�@"�@�@��@��@M�@J@�^@X@%@�`@�9@�@�@Q�@1'@�@��@\)@;d@�@�y@��@�+@V@�T@�-@?}@�@��@z�@�D@�D@j@�
@t�@dZ@C�@"�@@�@��@�!@��@�\@~�@^5@=q@��@�^@�7@x�@x�@X@G�@7L@7L@7L@�@%@Ĝ@�u@�@bN@bN@1'@��@�@l�@+@�@�@�y@��@��@�+@ff@V@E�@5?@5?@$�@{@{@{@@�@@�h1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
B
B
B
B
B
B
B
B
%B
+B
	7B
�B
(�B
'�B
%�B
'�B
+B
49B
E�B
_;B
gmB
��B
��B�BT�Bm�Bl�B�=B�+Bw�By�Bz�B|�B|�Bx�B{�B~�B�B�DB��B��B�B�FB�qB�wBBȴB��B��B�
B�B�B�B��B��BB�B)�B.B0!B8RB@�BN�BdZBjBr�Bt�Bu�Bu�By�B~�B�B�+B�1B�JB�oB��B��B��B��B�7B�By�Bo�Be`BW
BM�BG�BB�B<jB/B%�B �B�B��B�B�5B�B�B�BbBBB  B��B��B��B��B�B�B�B�B�)B��B�9B��B��Br�B �B
�B
��B
�{B
�B
�B
�B
�bB
��B
�DB
|�B
W
B
!�B
B	��B	�B	�ZB	�5B	�B	ɺB	�XB	��B	�%B	� B	v�B	o�B	ffB	Q�B	H�B	B�B	/B	"�B	�B	\B		7B	B	B��B��B��B��B�B�B�5B�B��B��B��B��BŢB�jB�-B�B��B��B��B��B�bB�DB�7B�B�B�=B�hB�uB�uB�\B�\B�bB�JB�DB�=B�VB��B��B��B��B��B�oB�\B�=B�+B�%B�1B�JB�hB��B�B�'B�3B�3B�?B�XB�^B��B��B��B��B��B��B��B��B�B�B��B��BĜBǮBƨBŢBĜB�dB�}B��B�}B�dB�jB�FB��B��B��B��B��B��B��B�!B�3B�FB�RB�XB�}BĜBǮB��B�5B�B�B��B��B	  B	+B	JB	VB	hB	�B	�B	�B	hB		7B	B	B	B	B	+B	+B	
=B	VB	�B	!�B	#�B	,B	0!B	49B	6FB	7LB	;dB	<jB	<jB	<jB	8RB	7LB	9XB	C�B	>wB	;dB	9XB	@�B	C�B	G�B	I�B	I�B	I�B	J�B	K�B	M�B	M�B	L�B	M�B	N�B	T�B	W
B	XB	XB	XB	XB	YB	XB	VB	W
B	\)B	^5B	\)B	]/B	[#B	[#B	\)B	_;B	^5B	^5B	^5B	_;B	bNB	cTB	ffB	hsB	hsB	hsB	iyB	iyB	iyB	jB	l�B	o�B	r�B	s�B	t�B	v�B	w�B	x�B	~�B	�B	�B	�%B	�1B	�DB	�PB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�FB	�RB	�XB	�^B	�wB	��B	ĜB	ƨB	ƨB	ǮB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�/B	�/B	�5B	�;B	�;B	�BB	�BB	�BB	�BB	�HB	�HB	�HB	�NB	�TB	�ZB	�`B	�`B	�`B	�fB	�`B	�`B	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
%B
+B

=B

=B

=B

=B
DB
DB
JB
JB
JB
JB
PB
PB
PB
VB
VB
\B
\B
hB
hB
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
�B
�B
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
"�B
"�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
$�B
$�B
%�B
%�B
&�B
'�B
'�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
,B
,B
,B
-B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
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
2-B
33B
49B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
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
<jB
<jB
<jB
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
>wB
>wB
>wB
>wB
>wB
>wB
?}B
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
E�B
E�B
E�B
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
H�B
H�B
H�B
H�B
H�B
H�B
H�B
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
K�B
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
L�B
M�B
N�B
M�B
M�B
M�B
M�B
N�B
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
^5B
^5B
^5B
_;B
_;B
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
cTB
cTB
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
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
hsB
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
jB
jB
jB
jB
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
n�B
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
s�B
s�B
t�B
t�B
t�B
t�B
t�B
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
u�B
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
w�B
w�B
w�B
w�B
w�B
w�B
x�B
w�B
w�B
x�B
x�B
x�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
B
-B
3B
3B
3B
3B
B
9B
?B
B
	B
�B
)*B
(>B
&B
($B
+�B
4�B
E�B
_VB
h�B
��B
�uB~BUMBm�Bm�B��B�fBx�BzxB{�B~(B~�Bz�B|�B�B�B�B�SB��B��B��B��B�B�-B�RB�B̳B��B�B��B�-B��B�BB�B+�B1[B2-B9>BA BOvBf2Bl�Bs�ButBv`Bw2B{0B� B�gB�fB�lB�B��B��B��B�CB��B�)B�mB|BrBh�BZkBOvBI7BE�BA�B2�B(>B$tB#�B�B�B�nB�B�'BB�BuBUB �B�qB��B��B�FB�3B�;B��B�B�HB��B�B�B�/B{�B'�B
��B
�B
�sB
��B
�EB
��B
�uB
�sB
��B
��B
[WB
%zB
	B	�rB	�B	��B	��B	ݘB	̈́B	�BB	�,B	��B	��B	x�B	s3B	i�B	S�B	KB	F�B	1�B	%�B	dB	B	
rB	%B	�B��B�}B�0B��B�hB�!B��BٴB�B��B�<B̳BǮB�]B�TB��B�@B��B�]B��B�:B�VB��B��B�B��B�:B�B��B��B��B�hB�jB��B��B��B��B�B�WB�kB�YB�[B�.B��B�zB�?B�1B�JB��B��B��B�|B�9B��B�B��B�BB�vBևB�vB�BBϑB�B�B׍B��B�2B͟B��B�B��B��BƎB��B��B�AB��B��B�OB�	B��B��B��B��B��B��B�B��B��B��B��B�B� B��B�1B�VB�VB�5B�!B��B��B	 �B	�B	jB	BB	B	YB	�B	B	�B	
�B	B	�B	�B	�B	�B	�B	
rB	<B	�B	"�B	$tB	,qB	0oB	4nB	6�B	8B	<�B	<�B	=�B	=B	8�B	7�B	:DB	E9B	?�B	<�B	:*B	@�B	C�B	HKB	I�B	I�B	J#B	K^B	LB	N"B	N<B	M�B	N�B	O�B	UgB	WsB	X�B	X�B	X�B	X�B	Z7B	X�B	V�B	W?B	\xB	^�B	\�B	]�B	[qB	[WB	\xB	_�B	^�B	^�B	^�B	_�B	b�B	c�B	gB	h�B	h�B	h�B	i�B	i�B	i�B	j�B	l�B	o�B	r�B	s�B	t�B	wB	x8B	yXB	.B	�[B	��B	�tB	�fB	�xB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�2B	�
B	�$B	�
B	�*B	�0B	�0B	�B	�CB	�cB	��B	�vB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�	B	�	B	�B	��B	��B	��B	�B	�"B	�B	�B	�B	�B	�:B	�&B	�aB	�B	�B	�B	�YB	�_B	�+B	�KB	�KB	�1B	�kB	�qB	�qB	�qB	�]B	�dB	�IB	�jB	�VB	�pB	��B	�vB	��B	��B	�|B	�|B	�|B	�B	�nB	�B	�zB	�`B	�B	�B	�zB	�zB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�2B	�	B	�B	�B	�0B	�JB	�"B	�"B	�<B	�]B	�HB
 OB
 OB
[B
'B
[B
aB
3B
9B
9B
9B
YB
zB

rB

rB

rB

rB
^B
xB
~B
dB
~B
~B
PB
jB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
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
�B
�B
�B
�B
B
�B
�B
�B
B
B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
B
 BB
!B
!HB
"B
"�B
"�B
$&B
%B
%B
$�B
%B
&B
&B
%�B
%B
%B
&B
&B
&B
&B
%�B
&B
&2B
%,B
%,B
&B
&2B
'8B
(>B
($B
*B
*0B
*KB
*KB
*B
*0B
+6B
+B
+6B
,=B
,=B
,WB
-�B
/iB
/5B
0UB
0UB
0UB
0UB
0UB
0;B
0;B
0UB
0UB
0UB
0;B
0;B
0UB
1[B
1[B
1[B
1vB
1vB
1�B
2�B
3�B
4�B
5tB
5tB
6�B
6zB
7�B
7�B
7fB
7�B
7�B
8lB
8lB
8�B
8�B
8�B
8lB
8�B
8�B
8�B
9�B
9�B
9�B
:xB
9�B
:xB
:�B
:�B
:�B
;�B
;�B
;�B
;�B
<jB
<�B
<jB
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
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
E�B
E�B
E�B
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
H�B
H�B
H�B
H�B
IB
IB
IB
J	B
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
KB
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
MB
L�B
MB
MB
MB
MB
N"B
N�B
M�B
M�B
M�B
M�B
N�B
M�B
NB
M�B
N�B
OB
OB
N�B
N�B
O(B
PB
O�B
O�B
O�B
PHB
Q4B
QB
QNB
RB
R B
SB
SB
S&B
S&B
T,B
TB
TB
T,B
T,B
T,B
UB
UB
U2B
UB
U2B
U2B
V9B
VSB
VB
V9B
W$B
W?B
WYB
X_B
XEB
X_B
YeB
YKB
YKB
Y1B
ZkB
Z�B
[WB
[=B
[=B
[WB
[qB
\]B
\]B
\CB
\]B
\]B
\]B
]IB
]dB
]dB
]~B
^OB
^jB
^OB
_VB
_VB
_VB
_pB
_pB
_�B
`�B
`vB
`\B
a|B
abB
abB
abB
abB
a|B
a|B
bhB
b�B
bhB
b�B
b�B
c�B
c�B
cnB
cTB
c�B
cnB
c�B
d�B
e�B
ezB
e�B
f�B
f�B
f�B
f�B
f�B
ffB
f�B
f�B
ffB
ffB
f�B
f�B
f�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
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
j�B
j�B
j�B
j�B
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
n�B
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
s�B
s�B
t�B
t�B
t�B
t�B
t�B
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
u�B
v�B
v�B
v�B
v�B
v�B
v�B
xB
w�B
w�B
xB
w�B
w�B
w�B
w�B
w�B
w�B
x�B
w�B
xB
x�B
y	B
y	B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.15(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201706070037232017060700372320170607003723201806221314202018062213142020180622131420201804050715592018040507155920180405071559  JA  ARFMdecpA19c                                                                20170603093510  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170603003525  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170603003526  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170603003526  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170603003527  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170603003527  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170603003527  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170603003527  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170603003527  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170603003528                      G�O�G�O�G�O�                JA  ARUP                                                                        20170603010828                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170603153503  CV  JULD            G�O�G�O�F�a�                JM  ARCAJMQC2.0                                                                 20170606153723  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170606153723  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404221559  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041420  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211517                      G�O�G�O�G�O�                